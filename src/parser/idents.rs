use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, satisfy},
    combinator::{map, recognize, value},
    error::ParseError,
    multi::many0,
    sequence::tuple,
    AsChar, Compare, IResult, InputIter, InputLength, InputTake, Offset, Slice,
};
use std::{
    fmt::{self, Display},
    ops::{RangeFrom, RangeTo},
};
use string_cache::DefaultAtom;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
    InitialSubsequent(DefaultAtom),
    Plus,
    Minus,
    Ellipsis,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Identifier::InitialSubsequent(s) => write!(f, "{}", s),
            Identifier::Plus => write!(f, "+"),
            Identifier::Minus => write!(f, "-"),
            Identifier::Ellipsis => write!(f, "..."),
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
    + Into<DefaultAtom>
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
        + Into<DefaultAtom>
{
}

pub(super) fn identifier<'a, I, E: ParseError<I>>(input: I) -> IResult<I, Identifier, E>
where
    I: ToIdentifier<'a>,
    <I as InputIter>::Item: AsChar + Copy,
{
    alt((
        map(
            recognize(tuple((initial::<I, _>, many0(subsequent)))),
            |s| Identifier::InitialSubsequent(s.into()),
        ),
        value(Identifier::Plus, char('+')),
        value(Identifier::Minus, char('-')),
        value(Identifier::Ellipsis, tag("...")),
    ))(input)
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
            Ok(("", Identifier::InitialSubsequent("hello".into())))
        )
    }
}
