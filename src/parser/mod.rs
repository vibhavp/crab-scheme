mod data;
mod defs;
mod exprs;
mod idents;
mod numbers;
mod programs;

use std::ops::Deref;

pub use data::*;
pub use defs::*;
pub use exprs::*;
pub use idents::*;
pub use programs::*;

use nom::{
    character::complete::{char, multispace0},
    combinator::cut,
    error::{context, ContextError, ParseError},
    sequence::delimited,
    IResult, Parser,
};
pub use numbers::*;

#[derive(Debug, Clone, PartialEq)]
pub enum OneOrMore<T: Clone, C = Box<T>> {
    One(C),
    More(Vec<T>),
}

impl<T: Clone> From<Vec<T>> for OneOrMore<T> {
    fn from(vec: Vec<T>) -> Self {
        assert!(!vec.is_empty());
        if vec.len() == 1 {
            OneOrMore::One(Box::new(vec[0].clone()))
        } else {
            OneOrMore::More(vec)
        }
    }
}

impl<T: Clone> From<Vec<T>> for OneOrMore<T, T> {
    fn from(vec: Vec<T>) -> Self {
        assert!(!vec.is_empty());
        if vec.len() == 1 {
            OneOrMore::One(vec[0].clone())
        } else {
            OneOrMore::More(vec)
        }
    }
}

impl<T: Clone, C: Deref<Target = T>> From<OneOrMore<T, C>> for Vec<T> {
    fn from(value: OneOrMore<T, C>) -> Self {
        match value {
            OneOrMore::One(b) => vec![(*b).clone()],
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
