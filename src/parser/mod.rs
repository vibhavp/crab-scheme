mod data;
mod defs;
mod exprs;
mod idents;
mod numbers;
mod programs;

use nom_locate::LocatedSpan;
use std::ops::{Deref, RangeFrom};

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
    AsChar, IResult, InputIter, InputTakeAtPosition, Parser, Slice,
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

impl From<OneOrMore<Identifier, Identifier>> for Vec<Identifier> {
    fn from(value: OneOrMore<Identifier, Identifier>) -> Self {
        match value {
            OneOrMore::One(v) => vec![v],
            OneOrMore::More(v) => v,
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

pub(self) fn whitespace_delimited<I, O, F, E: ParseError<I>>(
    inner: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(multispace0, inner, multispace0)
}

pub(self) fn s_expression<I, O, F, E: ParseError<I> + ContextError<I>>(
    inner: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: InputIter + InputTakeAtPosition + Slice<RangeFrom<usize>> + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as InputIter>::Item: AsChar,
{
    s_expression_context("sexp", inner)
}

pub(self) fn s_expression_context<I, O, F, E: ParseError<I> + ContextError<I>>(
    ctx: &'static str,
    inner: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: InputIter + InputTakeAtPosition + Slice<RangeFrom<usize>> + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as InputIter>::Item: AsChar,
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

#[derive(Debug, Clone)]
pub struct WithPosition<T, I, X = ()> {
    pub position: LocatedSpan<I, X>,
    pub token: T,
}
