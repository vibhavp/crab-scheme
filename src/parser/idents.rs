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
use std::ops::{RangeFrom, RangeTo};

#[macro_use]
pub mod atoms {
    include!(concat!(env!("OUT_DIR"), "/identifier_atom.rs"));
}

pub type Identifier = atoms::IdentifierAtom;

pub trait ToIdentifier<'a>:
    Slice<RangeFrom<usize>>
    + InputIter
    + Offset
    + InputLength
    + Clone
    + Compare<&'a str>
    + InputTake
    + Slice<RangeTo<usize>>
    + Into<Identifier>
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
        + Into<Identifier>
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
        |s| s.into(),
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
