mod data;
mod defs;
mod exprs;
mod idents;
mod numbers;

pub use data::*;
pub use defs::*;
pub use exprs::*;
pub use idents::*;
use nom::{
    character::complete::multispace0, error::VerboseError, sequence::delimited, IResult, Parser,
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

pub(self) fn whitespace_delimited<'a, O, F>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
where
    F: Parser<&'a str, O, VerboseError<&'a str>>,
{
    delimited(multispace0, inner, multispace0)
}
