use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, satisfy},
    combinator::{map, recognize},
    error::ParseError,
    multi::many0,
    sequence::tuple,
    AsChar, Compare, IResult, InputIter, InputLength, InputTake, Offset, Slice,
};
use std::{
    borrow::Cow,
    fmt,
    ops::{Deref, RangeFrom, RangeTo},
};
use string_cache::DefaultAtom;

#[macro_use]
pub mod atoms {
    include!(concat!(env!("OUT_DIR"), "/identifier_atom.rs"));
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Identifier {
    Known(atoms::KnownIdentifierAtom),
    Other(DefaultAtom),
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <str as fmt::Display>::fmt(self, f)
    }
}

impl<'a> From<Cow<'a, str>> for Identifier {
    fn from(value: Cow<'a, str>) -> Self {
        atoms::KnownIdentifierAtom::try_static(&value)
            .map(Identifier::Known)
            .unwrap_or_else(|| Identifier::Other(DefaultAtom::from(value)))
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier::from(Cow::from(value))
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Identifier::from(Cow::from(value))
    }
}

impl Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &str {
        match self {
            Self::Known(k) => k,
            Self::Other(d) => d,
        }
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        match self {
            Self::Known(k) => k,
            Self::Other(d) => d,
        }
    }
}

pub trait ToIdentifier<'a>:
    Slice<RangeFrom<usize>>
    + InputIter
    + Offset
    + InputLength
    + Clone
    + Compare<&'a str>
    + InputTake
    + Slice<RangeTo<usize>>
    + Into<&'a str>
{
}

impl<'a, T> ToIdentifier<'a> for T where
    T: Slice<RangeFrom<usize>>
        + InputIter
        + Offset
        + InputLength
        + Clone
        + Compare<&'a str>
        + InputTake
        + Slice<RangeTo<usize>>
        + Into<&'a str>
{
}

pub(super) fn identifier<'a, I, E: ParseError<I>>(input: I) -> IResult<I, Identifier, E>
where
    I: ToIdentifier<'a>,
    <I as InputIter>::Item: AsChar + Copy,
{
    map(
        alt((
            recognize(tuple((initial::<I, _>, many0(subsequent)))),
            recognize(char('+')),
            recognize(char('-')),
            recognize(tag("...")),
        )),
        |s| {
            let s: &str = s.into();
            Identifier::from(s.to_ascii_lowercase())
        },
    )(input)
}

#[derive(Debug, Clone)]
enum Initial {
    Letter(Letter),
    Other(char), // | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
}

impl From<Initial> for char {
    fn from(value: Initial) -> char {
        match value {
            Initial::Letter(l) | Initial::Other(l) => l,
        }
    }
}

fn initial<I, E: ParseError<I>>(input: I) -> IResult<I, Initial, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
    alt((
        map(letter, Initial::Letter),
        map(
            alt((
                char('!'),
                char('$'),
                char('%'),
                char('&'),
                char('*'),
                char('/'),
                char(':'),
                char('<'),
                char('='),
                char('>'),
                char('?'),
                char('~'),
                char('_'),
                char('^'),
                char('"'),
            )),
            Initial::Other,
        ),
    ))(input)
}

#[derive(Debug, Clone)]
enum Subsequent {
    Initial(Initial),
    Digit(DigitIdent),
    Other(char), // . | + | -
}

fn subsequent<I, E: ParseError<I>>(input: I) -> IResult<I, Subsequent, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar + Copy,
{
    alt((
        map(initial, Subsequent::Initial),
        map(digit_ident, Subsequent::Digit),
        map(alt((char('.'), char('+'), char('-'))), Subsequent::Other),
    ))(input)
}

type Letter = char; // a | b | ... | z

fn letter<I, E: ParseError<I>>(input: I) -> IResult<I, Letter, E>
where
    I: Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: AsChar,
{
    satisfy(|c| c.is_alphabetic())(input)
}

type DigitIdent = char; // 0 | 1 | ... | 9

pub(super) fn digit_ident<I, E: ParseError<I>>(input: I) -> IResult<I, char, E>
where
    I: Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: AsChar,
{
    satisfy(|c| c.is_ascii_digit())(input)
}

#[cfg(test)]
mod test {
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn test_identifier() {
        assert_eq!(
            identifier::<_, VerboseError<&str>>("hello"),
            Ok(("", Identifier::from("hello")))
        )
    }
}
