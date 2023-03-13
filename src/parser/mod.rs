mod data;
mod defs;
mod exprs;
mod idents;
mod numbers;
mod programs;

pub use data::*;
pub use defs::*;
pub use exprs::*;
pub use idents::*;
pub use programs::*;

use nom::{
    character::complete::{char, multispace0},
    combinator::cut,
    error::{context, ContextError, ParseError, VerboseError},
    sequence::delimited,
    IResult, Parser,
};
pub use numbers::*;

#[derive(Debug, Clone, PartialEq)]
pub enum OneOrMore<T> {
    One(Box<T>),
    More(Vec<T>),
}

impl<T: Clone> TryFrom<Vec<T>> for OneOrMore<T> {
    type Error = ();
    fn try_from(value: Vec<T>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(())
        } else if value.len() == 1 {
            Ok(OneOrMore::One(Box::new(value[0].clone())))
        } else {
            Ok(OneOrMore::More(value))
        }
    }
}

impl<T> From<OneOrMore<T>> for Vec<T> {
    fn from(value: OneOrMore<T>) -> Self {
        match value {
            OneOrMore::One(b) => vec![*b],
            OneOrMore::More(b) => b,
        }
    }
}

pub(self) fn whitespace_delimited<'a, O, F, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

pub(self) fn s_expression<'a, O, F, E: ParseError<&'a str> + ContextError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    s_expression_context("sexp", inner)
}

pub(self) fn s_expression_context<'a, O, F, E: ParseError<&'a str> + ContextError<&'a str>>(
    ctx: &'static str,
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    context(
        ctx,
        delimited(
            context("sexp begin", char('(')),
            whitespace_delimited(inner),
            context("sexp end", cut(char(')'))),
        ),
    )
}
