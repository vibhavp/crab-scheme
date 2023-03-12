use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace1},
    combinator::{cut, map, map_res, opt},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

use super::{
    body, data, datum, variable, whitespace_delimited, Body, Boolean, Character, Datum, Number,
    OneOrMore, SchemeString, SyntaxBinding, Variable,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Constant(Constant<'a>),
    Variable(Variable),
    Quote(Datum<'a>),
    Lambda {
        formals: Formals,
        body: Body<'a>,
    },
    If {
        cond: Box<Expression<'a>>,
        then: Box<Expression<'a>>,
        else_expr: Option<Box<Expression<'a>>>,
    },
    Set {
        var: Variable,
        expr: Box<Expression<'a>>,
    },
    LetSyntax(Vec<SyntaxBinding>, OneOrMore<Expression<'a>>),
    LetRecSyntax(Vec<SyntaxBinding>, OneOrMore<Expression<'a>>),
    Application(Application<'a>),
}

pub(super) fn expression(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let quote = context(
        "quote",
        preceded(
            terminated(tag("quote"), multispace1),
            map(cut(datum), Expression::Quote),
        ),
    );
    let lambda = context(
        "lambda",
        preceded(
            terminated(tag("lambda"), multispace1),
            map(cut(tuple((formals, body))), |(formals, body)| {
                Expression::Lambda { formals, body }
            }),
        ),
    );
    let if_expr = context(
        "if",
        preceded(
            terminated(tag("if"), multispace1),
            map(
                tuple((
                    context("condition", cut(whitespace_delimited(expression))),
                    context("then", cut(whitespace_delimited(expression))),
                    context("else", opt(whitespace_delimited(expression))),
                )),
                |(cond, then, else_expr)| Expression::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_expr: else_expr.map(Box::new),
                },
            ),
        ),
    );
    let set = context(
        "set!",
        preceded(
            terminated(tag("set!"), multispace1),
            map(
                separated_pair(variable, multispace1, expression),
                |(var, expr)| Expression::Set {
                    var,
                    expr: Box::new(expr),
                },
            ),
        ),
    );
    let application = context(
        "application",
        map_res(many1(whitespace_delimited(expression)), |exprs| {
            Ok::<_, ()>(Expression::Application(exprs.try_into()?))
        }),
    );
    let sexp = alt((quote, lambda, if_expr, set, application));
    context(
        "expression",
        alt((
            map(variable, Expression::Variable),
            map(constant, Expression::Constant),
            map(preceded(char('\''), cut(datum)), Expression::Quote),
            delimited(
                context("expression begin", char('(')),
                whitespace_delimited(sexp),
                context("expression end", cut(char(')'))),
            ),
        )),
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant<'a> {
    Boolean(Boolean),
    Number(Number<'a>),
    Character(Character),
    String(SchemeString<'a>),
}

fn constant(input: &str) -> IResult<&str, Constant, VerboseError<&str>> {
    context(
        "constant",
        alt((
            map(data::boolean, Constant::Boolean),
            map(data::number, Constant::Number),
            map(data::character, Constant::Character),
            map(data::string, Constant::String),
        )),
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Formals {
    Variable(Variable),
    VecVariable(Vec<Variable>),
    OneOrMoreRest(OneOrMore<Variable>, Variable),
}

fn formals(input: &str) -> IResult<&str, Formals, VerboseError<&str>> {
    context(
        "formals",
        alt((
            map(preceded(multispace1, variable), Formals::Variable),
            delimited(
                context("formals begin", char('(')),
                alt((
                    map(many0(whitespace_delimited(variable)), Formals::VecVariable),
                    map_res(
                        separated_pair(
                            many1(whitespace_delimited(variable)),
                            char('.'),
                            context("rest", whitespace_delimited(variable)),
                        ),
                        |(vars, rest)| Ok::<_, ()>(Formals::OneOrMoreRest(vars.try_into()?, rest)),
                    ),
                )),
                context("formals end", cut(char(')'))),
            ),
        )),
    )(input)
}

pub type Application<'a> = OneOrMore<Expression<'a>>;

#[cfg(test)]
mod test {
    use super::*;
    use data::{List, Symbol};

    #[test]
    fn test_expression() {
        assert_eq!(
            expression("#t"),
            Ok(("", Expression::Constant(Constant::Boolean(true))))
        );
        assert_eq!(
            expression("'(#t)"),
            Ok((
                "",
                Expression::Quote(Datum::List(data::List::NList(vec![Datum::Boolean(true)])))
            ))
        );
        assert_eq!(
            expression("(+ a b)"),
            Ok((
                "",
                Expression::Application(OneOrMore::More(vec![
                    Expression::Variable(Variable::Plus),
                    Expression::Variable(Variable::InitialSubsequent("a".into())),
                    Expression::Variable(Variable::InitialSubsequent("b".into()))
                ]))
            ))
        );
        assert_eq!(
            expression("'(+ a b)"),
            Ok((
                "",
                Expression::Quote(Datum::List(List::NList(vec![
                    Datum::Symbol(Symbol::Plus),
                    Datum::Symbol(Symbol::InitialSubsequent("a".into())),
                    Datum::Symbol(Symbol::InitialSubsequent("b".into()))
                ])))
            ))
        );
        assert_eq!(
            expression("(set! foo bar)"),
            Ok((
                "",
                Expression::Set {
                    var: Variable::InitialSubsequent("foo".into()),
                    expr: Box::new(Expression::Variable(Variable::InitialSubsequent(
                        "bar".into()
                    )))
                }
            ))
        );
        assert_eq!(
            expression("(set! foo (quote (+ a b)))"),
            Ok((
                "",
                Expression::Set {
                    var: Variable::InitialSubsequent("foo".into()),
                    expr: Box::new(Expression::Quote(Datum::List(List::NList(vec![
                        Datum::Symbol(Symbol::Plus),
                        Datum::Symbol(Symbol::InitialSubsequent("a".into())),
                        Datum::Symbol(Symbol::InitialSubsequent("b".into()))
                    ]))))
                }
            ))
        );
        assert_eq!(
            expression("(quote (+ a b))"),
            Ok((
                "",
                Expression::Quote(Datum::List(List::NList(vec![
                    Datum::Symbol(Symbol::Plus),
                    Datum::Symbol(Symbol::InitialSubsequent("a".into())),
                    Datum::Symbol(Symbol::InitialSubsequent("b".into()))
                ])))
            ))
        );
        assert_eq!(
            expression("(if (> a b) foo bar)"),
            Ok((
                "",
                Expression::If {
                    cond: Box::new(Expression::Application(OneOrMore::More(vec![
                        Expression::Variable(Variable::InitialSubsequent(">".into())),
                        Expression::Variable(Variable::InitialSubsequent("a".into())),
                        Expression::Variable(Variable::InitialSubsequent("b".into()))
                    ]))),
                    then: Box::new(Expression::Variable(Variable::InitialSubsequent(
                        "foo".into()
                    ))),
                    else_expr: Some(Box::new(Expression::Variable(Variable::InitialSubsequent(
                        "bar".into()
                    )))),
                }
            ))
        )
    }
}
