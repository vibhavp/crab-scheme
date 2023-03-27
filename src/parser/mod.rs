#[macro_use]
mod idents;
mod data;
mod defs;
mod exprs;
mod numbers;
mod programs;

use nom_locate::{position, LocatedSpan};
use std::{
    borrow::Borrow,
    ops::{Deref, RangeFrom, RangeTo},
};

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
    AsBytes, AsChar, IResult, InputIter, InputTake, InputTakeAtPosition, Offset, Parser, Slice,
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

impl<T: Clone> From<OneOrMore<T, Box<T>>> for Vec<T> {
    fn from(value: OneOrMore<T, Box<T>>) -> Self {
        match value {
            OneOrMore::One(b) => vec![(*b).clone()],
            OneOrMore::More(b) => b,
        }
    }
}

impl<T: Clone, C> OneOrMore<T, C>
where
    C: Borrow<T>,
{
    fn first(&self) -> &T {
        match self {
            Self::One(c) => c.borrow(),
            Self::More(v) => &v[0],
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

trait Position:
    InputIter + InputTake + Slice<RangeTo<usize>> + Slice<RangeFrom<usize>> + AsBytes + Offset
{
}

impl<T> Position for T where
    T: InputIter + InputTake + Slice<RangeTo<usize>> + Slice<RangeFrom<usize>> + AsBytes + Offset
{
}

pub(self) fn with_position<I, O, F, X, E: ParseError<Span<I, X>>>(
    mut inner: F,
) -> impl FnMut(Span<I, X>) -> IResult<Span<I, X>, WithPosition<O, I, X>, E>
where
    F: Parser<Span<I, X>, O, E>,
    I: InputIter + InputTake + Slice<RangeTo<usize>> + Slice<RangeFrom<usize>> + AsBytes + Offset,
    X: Clone,
{
    move |input| {
        let (input, pos) = position(input)?;
        let (input, output) = inner.parse(input)?;

        Ok((
            input,
            WithPosition {
                position: pos,
                token: output,
            },
        ))
    }
}

type Span<I, X> = LocatedSpan<I, X>;

#[derive(Debug, Clone)]
pub struct WithPosition<T, I, X = ()> {
    pub position: Span<I, X>,
    pub token: T,
}

impl<T, I, X> Deref for WithPosition<T, I, X> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl<T, I, X> AsRef<T> for WithPosition<T, I, X>
where
    <WithPosition<T, I, X> as Deref>::Target: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        &self.token
    }
}

impl<T, I, X> Borrow<T> for WithPosition<T, I, X> {
    fn borrow(&self) -> &T {
        &self.token
    }
}
