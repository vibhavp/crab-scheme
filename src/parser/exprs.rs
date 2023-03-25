use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1},
    combinator::{cut, map, opt, verify},
    error::{context, ContextError, ParseError},
    multi::{many0, many1},
    sequence::{preceded, separated_pair, terminated, tuple},
    AsChar, IResult, InputIter, InputTakeAtPosition,
};

use super::{
    body, data, datum, s_expression, s_expression_context, variable, whitespace_delimited, Body,
    Boolean, Character, Datum, DatumParseError, Number, OneOrMore, ParseToDatum, SchemeString,
    SyntaxBinding, Variable,
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
    Let(Let<'a>),
    LetSyntax(Vec<SyntaxBinding<'a>>, OneOrMore<Expression<'a>>),
    LetRecSyntax(Vec<SyntaxBinding<'a>>, OneOrMore<Expression<'a>>),
    Application(Application<'a>),
}

pub(super) fn expression<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Expression<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
    let quote = context(
        "quote",
        preceded(
            terminated(tag("quote"), multispace0),
            map(cut(datum), Expression::Quote),
        ),
    );
    let lambda = context(
        "lambda",
        preceded(
            terminated(tag("lambda"), multispace0),
            map(cut(tuple((formals, body))), |(formals, body)| {
                Expression::Lambda { formals, body }
            }),
        ),
    );
    let if_expr = context(
        "if",
        preceded(
            terminated(tag("if"), multispace0),
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
                separated_pair(variable, multispace0, expression),
                |(var, expr)| Expression::Set {
                    var,
                    expr: Box::new(expr),
                },
            ),
        ),
    );
    let let_expr = context(
        "let",
        map(
            preceded(
                terminated(tag("let"), multispace0),
                tuple((
                    opt(whitespace_delimited(context("proc-id", variable))),
                    cut(s_expression_context(
                        "let-bindings",
                        many0(whitespace_delimited(binding)),
                    )),
                    whitespace_delimited(body),
                )),
            ),
            |(variable, bindings, body)| {
                Expression::Let(if let Some(var) = variable {
                    Let::NamedLet(var, bindings, body)
                } else {
                    Let::SimpleLet(bindings, body)
                })
            },
        ),
    );

    let application = context(
        "application",
        map(many1(whitespace_delimited(expression)), |exprs| {
            Expression::Application(exprs.into())
        }),
    );
    let sexp = alt((quote, lambda, if_expr, set, let_expr, application));

    context(
        "expression",
        alt((
            map(constant, Expression::Constant),
            map(variable, Expression::Variable),
            map(preceded(char('\''), cut(datum)), Expression::Quote),
            s_expression(sexp),
        )),
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant<'a> {
    Boolean(Boolean),
    Number(Number),
    Character(Character),
    String(SchemeString<'a>),
}

fn constant<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Constant<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
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
    Rest(Variable),
    Variables(Vec<Variable>),
    VariablesRest(OneOrMore<Variable, Variable>, Variable),
}

fn formals<'a, I, E: ParseError<I> + ContextError<I>>(input: I) -> IResult<I, Formals, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Copy,
{
    context(
        "lambda formals",
        alt((
            map(
                terminated(context("rest-id", variable), multispace0),
                Formals::Rest,
            ),
            s_expression(alt((map(
                verify(
                    tuple((
                        many0(whitespace_delimited(variable)),
                        opt(preceded(
                            char('.'),
                            whitespace_delimited(context("rest-id", cut(variable))),
                        )),
                    )),
                    |(variables, rest_id)| {
                        rest_id.is_none() || (rest_id.is_some() && !variables.is_empty())
                    },
                ),
                |(vars, rest_id)| match rest_id {
                    Some(rest) => Formals::VariablesRest(vars.into(), rest),
                    None => Formals::Variables(vars),
                },
            ),))),
        )),
    )(input)
}

pub type Application<'a> = OneOrMore<Expression<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Let<'a> {
    SimpleLet(Vec<Binding<'a>>, Body<'a>),
    NamedLet(Variable, Vec<Binding<'a>>, Body<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding<'a> {
    pub var: Variable,
    pub expr: Expression<'a>,
}

// (<variable> <expression>)
fn binding<'a, I, E: DatumParseError<I>>(input: I) -> IResult<I, Binding<'a>, E>
where
    I: ParseToDatum<'a>,
    <I as InputIter>::Item: AsChar + Clone + Copy,
    <I as InputTakeAtPosition>::Item: AsChar + Clone + Copy,
{
    context(
        "binding",
        map(
            s_expression(separated_pair(variable, multispace0, cut(expression))),
            |(var, expr)| Binding { var, expr },
        ),
    )(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use data::{List, Symbol};
    use nom::error::VerboseError;

    #[test]
    fn test_expression() {
        assert_eq!(
            expression::<_, VerboseError<&str>>("#t"),
            Ok(("", Expression::Constant(Constant::Boolean(Boolean(true)))))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("'(#t)"),
            Ok((
                "",
                Expression::Quote(Datum::List(data::List::NList(vec![Datum::Boolean(
                    Boolean(true)
                )])))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(+ a b)"),
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
            expression::<_, VerboseError<&str>>("'(+ a b)"),
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
            expression::<_, VerboseError<&str>>("(set! foo bar)"),
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
            expression::<_, VerboseError<&str>>("(set! foo (quote (+ a b)))"),
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
            expression::<_, VerboseError<&str>>("(quote (+ a b))"),
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
            expression::<_, VerboseError<&str>>("(if (> a b) foo bar)"),
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
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(lambda foo bar)"),
            Ok((
                "",
                Expression::Lambda {
                    formals: Formals::Rest(Variable::InitialSubsequent("foo".into())),
                    body: Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(
                            Variable::InitialSubsequent("bar".into())
                        )))
                    )
                }
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(lambda (foo a1 a2) bar)"),
            Ok((
                "",
                Expression::Lambda {
                    formals: Formals::Variables(vec![
                        Variable::InitialSubsequent("foo".into()),
                        Variable::InitialSubsequent("a1".into()),
                        Variable::InitialSubsequent("a2".into())
                    ]),
                    body: Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(
                            Variable::InitialSubsequent("bar".into())
                        )))
                    )
                }
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(lambda (foo a1 . rest) bar)"),
            Ok((
                "",
                Expression::Lambda {
                    formals: Formals::VariablesRest(
                        OneOrMore::More(vec![
                            Variable::InitialSubsequent("foo".into()),
                            Variable::InitialSubsequent("a1".into()),
                        ]),
                        Variable::InitialSubsequent("rest".into())
                    ),
                    body: Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(
                            Variable::InitialSubsequent("bar".into())
                        )))
                    )
                }
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(let ((a b) (c d)) c)",),
            Ok((
                "",
                Expression::Let(Let::SimpleLet(
                    vec![
                        Binding {
                            var: Variable::InitialSubsequent("a".into()),
                            expr: Expression::Variable(Variable::InitialSubsequent("b".into()))
                        },
                        Binding {
                            var: Variable::InitialSubsequent("c".into()),
                            expr: Expression::Variable(Variable::InitialSubsequent("d".into()))
                        },
                    ],
                    Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(
                            Variable::InitialSubsequent("c".into())
                        )))
                    )
                ))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(let f ((a b)) c)",),
            Ok((
                "",
                Expression::Let(Let::NamedLet(
                    Variable::InitialSubsequent("f".into()),
                    vec![Binding {
                        var: Variable::InitialSubsequent("a".into()),
                        expr: Expression::Variable(Variable::InitialSubsequent("b".into()))
                    }],
                    Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(
                            Variable::InitialSubsequent("c".into())
                        )))
                    )
                ))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(let f ((a b) (c d)) c)",),
            Ok((
                "",
                Expression::Let(Let::NamedLet(
                    Variable::InitialSubsequent("f".into()),
                    vec![
                        Binding {
                            var: Variable::InitialSubsequent("a".into()),
                            expr: Expression::Variable(Variable::InitialSubsequent("b".into()))
                        },
                        Binding {
                            var: Variable::InitialSubsequent("c".into()),
                            expr: Expression::Variable(Variable::InitialSubsequent("d".into()))
                        },
                    ],
                    Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(
                            Variable::InitialSubsequent("c".into())
                        )))
                    )
                ))
            ))
        );
    }
}
