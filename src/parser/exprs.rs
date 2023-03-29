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
        map(
            verify(
                many1(whitespace_delimited(expression)),
                |exprs: &Vec<Expression>| !matches!(exprs[0], Expression::Constant(_)),
            ),
            |exprs| Expression::Application(exprs.into()),
        ),
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
    use crate::parser::{atoms::*, Identifier};
    use data::List;
    use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
    use std::assert_matches::assert_matches;

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
                    Expression::Variable(Identifier::Known(ident_atom!("+"))),
                    Expression::Variable(Identifier::from("a")),
                    Expression::Variable(Identifier::from("b"))
                ]))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("'(+ a b)"),
            Ok((
                "",
                Expression::Quote(Datum::List(List::NList(vec![
                    Datum::Symbol(Identifier::Known(ident_atom!("+"))),
                    Datum::Symbol(Identifier::from("a")),
                    Datum::Symbol(Identifier::from("b"))
                ])))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(set! foo bar)"),
            Ok((
                "",
                Expression::Set {
                    var: Identifier::from("foo"),
                    expr: Box::new(Expression::Variable(Identifier::from("bar")))
                }
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(set! foo (quote (+ a b)))"),
            Ok((
                "",
                Expression::Set {
                    var: Identifier::from("foo"),
                    expr: Box::new(Expression::Quote(Datum::List(List::NList(vec![
                        Datum::Symbol(Identifier::Known(ident_atom!("+"))),
                        Datum::Symbol(Identifier::from("a")),
                        Datum::Symbol(Identifier::from("b"))
                    ]))))
                }
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(quote (+ a b))"),
            Ok((
                "",
                Expression::Quote(Datum::List(List::NList(vec![
                    Datum::Symbol(Identifier::Known(ident_atom!("+"))),
                    Datum::Symbol(Identifier::from("a")),
                    Datum::Symbol(Identifier::from("b"))
                ])))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(if (> a b) foo bar)"),
            Ok((
                "",
                Expression::If {
                    cond: Box::new(Expression::Application(OneOrMore::More(vec![
                        Expression::Variable(Identifier::Known(ident_atom!(">"))),
                        Expression::Variable(Identifier::from("a")),
                        Expression::Variable(Identifier::from("b"))
                    ]))),
                    then: Box::new(Expression::Variable(Identifier::from("foo"))),
                    else_expr: Some(Box::new(Expression::Variable(Identifier::from("bar")))),
                }
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(lambda foo bar)"),
            Ok((
                "",
                Expression::Lambda {
                    formals: Formals::Rest(Identifier::from("foo")),
                    body: Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(Identifier::from("bar"))))
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
                        Identifier::from("foo"),
                        Identifier::from("a1"),
                        Identifier::from("a2")
                    ]),
                    body: Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(Identifier::from("bar"))))
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
                        OneOrMore::More(vec![Identifier::from("foo"), Identifier::from("a1"),]),
                        Identifier::from("rest")
                    ),
                    body: Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(Identifier::from("bar"))))
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
                            var: Identifier::from("a"),
                            expr: Expression::Variable(Identifier::from("b"))
                        },
                        Binding {
                            var: Identifier::from("c"),
                            expr: Expression::Variable(Identifier::from("d"))
                        },
                    ],
                    Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(Identifier::from("c"))))
                    )
                ))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(let f ((a b)) c)",),
            Ok((
                "",
                Expression::Let(Let::NamedLet(
                    Identifier::from("f"),
                    vec![Binding {
                        var: Identifier::from("a"),
                        expr: Expression::Variable(Identifier::from("b"))
                    }],
                    Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(Identifier::from("c"))))
                    )
                ))
            ))
        );
        assert_eq!(
            expression::<_, VerboseError<&str>>("(let f ((a b) (c d)) c)",),
            Ok((
                "",
                Expression::Let(Let::NamedLet(
                    Identifier::from("f"),
                    vec![
                        Binding {
                            var: Identifier::from("a"),
                            expr: Expression::Variable(Identifier::from("b"))
                        },
                        Binding {
                            var: Identifier::from("c"),
                            expr: Expression::Variable(Identifier::from("d"))
                        },
                    ],
                    Body(
                        vec![],
                        OneOrMore::One(Box::new(Expression::Variable(Identifier::from("c"))))
                    )
                ))
            ))
        );
    }

    #[test]
    fn test_invalid_application() {
        assert_matches!(
            expression::<_, VerboseError<&str>>("(#t foo bar)"),
            Err(nom::Err::Error(VerboseError { errors }))
            if matches!(errors.as_slice(), [(_, VerboseErrorKind::Nom(ErrorKind::Verify)), ..])
        );

        assert_matches!(
            expression::<_, VerboseError<&str>>("(123 foo bar)"),
            Err(nom::Err::Error(VerboseError { errors }))
            if matches!(errors.as_slice(), [(_, VerboseErrorKind::Nom(ErrorKind::Verify)), ..])
        );

        assert_matches!(
            expression::<_, VerboseError<&str>>("(\"foo\" foo bar)"),
            Err(nom::Err::Error(VerboseError { errors }))
            if matches!(errors.as_slice(), [(_, VerboseErrorKind::Nom(ErrorKind::Verify)), ..])
        );

        assert_matches!(
            expression::<_, VerboseError<&str>>(r"(#\c foo bar)"),
            Err(nom::Err::Error(VerboseError { errors }))
            if matches!(errors.as_slice(), [(_, VerboseErrorKind::Nom(ErrorKind::Verify)), ..])
        )
    }
}
