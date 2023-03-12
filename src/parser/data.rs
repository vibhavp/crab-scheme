use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char},
    combinator::{cut, map, map_res, not, opt, recognize, value},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair},
    IResult, Parser,
};
use std::borrow::Cow;

use super::{identifier, num, whitespace_delimited, Identifier, Num, OneOrMore, Radix};

#[derive(Debug, Clone, PartialEq)]
pub enum Datum<'a> {
    Boolean(Boolean),
    Symbol(Symbol),
    Number(Number<'a>),
    Character(Character),
    String(SchemeString<'a>),
    List(List<'a>),
    Vector(Vector<'a>),
}

pub(super) fn datum(input: &str) -> IResult<&str, Datum, VerboseError<&str>> {
    alt((
        map(boolean, Datum::Boolean),
        map(symbol, Datum::Symbol),
        map(number, Datum::Number),
        map(character, Datum::Character),
        map(string, Datum::String),
        map(list, Datum::List),
        map(vector, Datum::Vector),
    ))(input)
}

pub type Boolean = bool;

pub(super) fn boolean(input: &str) -> IResult<&str, Boolean, VerboseError<&str>> {
    map(preceded(char('#'), alt((char('t'), char('f')))), |c| {
        c == 't'
    })(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number<'a> {
    Num2(Num<'a>),
    Num8(Num<'a>),
    Num10(Num<'a>),
    Num16(Num<'a>),
}

pub(super) fn number(input: &str) -> IResult<&str, Number, VerboseError<&str>> {
    map(num, |n| match n.prefix.radix {
        Radix::Two => Number::Num2(n),
        Radix::Eight => Number::Num8(n),
        Radix::Ten => Number::Num10(n),
        Radix::Sixteen => Number::Num16(n),
    })(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Character {
    Any(char),
    Newline,
    Space,
}

pub(super) fn character(input: &str) -> IResult<&str, Character, VerboseError<&str>> {
    preceded(
        tag(r#"#\"#),
        alt((
            value(Character::Newline, tag("newline")),
            value(Character::Space, tag("space")),
            map(anychar, Character::Any),
        )),
    )(input)
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemeString<'a>(pub Cow<'a, str>);

pub(super) fn string(input: &str) -> IResult<&str, SchemeString, VerboseError<&str>> {
    delimited(
        context("opening string quote", char('"')),
        map(recognize(many0(string_character)), |c| {
            SchemeString(c.into())
        }),
        context("closing string quote", cut(char('"'))),
    )(input)
}

pub type StringCharacter = char;

fn string_character(input: &str) -> IResult<&str, StringCharacter, VerboseError<&str>> {
    alt((
        value('"', tag(r#"\""#)),
        value('\\', tag(r#"\\"#)),
        not(char('"'))
            .and(not(char('\\')))
            .and(anychar)
            .map(|c| c.1),
    ))(input)
}

pub type Symbol = Identifier;

fn symbol(input: &str) -> IResult<&str, Symbol, VerboseError<&str>> {
    identifier(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum List<'a> {
    NList(Vec<Datum<'a>>),
    Dot(OneOrMore<Datum<'a>>, Box<Datum<'a>>),
    Abbreviation(Abbreviation),
}

fn delimited_datum(input: &str) -> IResult<&str, Datum, VerboseError<&str>> {
    whitespace_delimited(datum)(input)
}

fn list(input: &str) -> IResult<&str, List, VerboseError<&str>> {
    alt((
        delimited(
            context("opening list parenthesis", char('(')),
            alt((
                map_res(
                    separated_pair(many1(delimited_datum), char('.'), delimited_datum),
                    |(one_or_more_datum, cdr_datum)| {
                        Ok::<_, ()>(List::Dot(
                            one_or_more_datum.try_into()?,
                            Box::new(cdr_datum),
                        ))
                    },
                ),
                map(many0(delimited_datum), List::NList),
            )),
            context("closing list parenthesis", cut(char(')'))),
        ),
        map(abbreviation, List::Abbreviation),
    ))(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Abbreviation {
    SingleQuote,   // '
    Backtick,      // `
    Unquote,       // ,
    UnquoteSplice, // ,@
}

fn abbreviation(input: &str) -> IResult<&str, Abbreviation, VerboseError<&str>> {
    alt((
        value(Abbreviation::SingleQuote, char('\'')),
        value(Abbreviation::Backtick, char('`')),
        map(preceded(char(','), opt(char('@'))), |splice| {
            if splice.is_some() {
                Abbreviation::UnquoteSplice
            } else {
                Abbreviation::Unquote
            }
        }),
    ))(input)
}

pub type Vector<'a> = Vec<Datum<'a>>;

fn vector(input: &str) -> IResult<&str, Vector, VerboseError<&str>> {
    preceded(
        char('#'),
        delimited(
            char('('),
            many0(delimited_datum),
            context("closing vector parenthesis", cut(char(')'))),
        ),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_boolean() {
        assert_eq!(boolean("#t"), Ok(("", true)));
        assert_eq!(boolean("#f"), Ok(("", false)));
    }

    #[test]
    fn test_character() {
        assert_eq!(character(r#"#\a"#), Ok(("", Character::Any('a'))));
        assert_eq!(character(r#"#\newline"#), Ok(("", Character::Newline)));
        assert_eq!(character(r#"#\space"#), Ok(("", Character::Space)))
    }

    #[test]
    fn test_string_character() {
        assert_eq!(string_character(r#"\""#), Ok(("", '"')));
        assert_eq!(string_character(r#"\\"#), Ok(("", '\\')));
        assert_eq!(string_character("f"), Ok(("", 'f')));
    }

    #[test]
    fn test_string() {
        assert_eq!(
            string(r#""hello, world!""#),
            Ok(("", SchemeString("hello, world!".into())))
        );
        assert!(string(r#""hello, world!"#).is_err());
    }

    #[test]
    fn test_abbreviation() {
        assert_eq!(abbreviation("'"), Ok(("", Abbreviation::SingleQuote)));
        assert_eq!(abbreviation("`"), Ok(("", Abbreviation::Backtick)));
        assert_eq!(abbreviation(","), Ok(("", Abbreviation::Unquote)));
        assert_eq!(abbreviation(",@"), Ok(("", Abbreviation::UnquoteSplice)));
    }

    #[test]
    fn test_list() {
        assert_eq!(list("()"), Ok(("", List::NList(vec![]))));
        assert_eq!(
            list("(a b c)"),
            Ok((
                "",
                List::NList(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                ])
            ))
        );
        assert_eq!(
            list("(a . b)"),
            Ok((
                "",
                List::Dot(
                    OneOrMore::One(Box::new(Datum::Symbol(Identifier::InitialSubsequent(
                        "a".into()
                    )))),
                    Box::new(Datum::Symbol(Identifier::InitialSubsequent("b".into())))
                )
            ))
        );
        assert_eq!(
            list("(a (a b))"),
            Ok((
                "",
                List::NList(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::List(List::NList(vec![
                        Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                        Datum::Symbol(Identifier::InitialSubsequent("b".into()))
                    ]))
                ])
            ))
        );
        assert_eq!(
            list("(a b c . b)"),
            Ok((
                "",
                List::Dot(
                    OneOrMore::More(vec![
                        Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                        Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                        Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                    ]),
                    Box::new(Datum::Symbol(Identifier::InitialSubsequent("b".into())))
                )
            ))
        )
    }

    #[test]
    fn test_datum() {
        assert_eq!(datum("#t"), Ok(("", Datum::Boolean(true))));
        assert_eq!(datum("#f"), Ok(("", Datum::Boolean(false))));

        assert_eq!(
            datum("foo"),
            Ok((
                "",
                Datum::Symbol(Identifier::InitialSubsequent("foo".into()))
            ))
        );

        assert_eq!(
            datum("(a b c)"),
            Ok((
                "",
                Datum::List(List::NList(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                ]))
            ))
        );

        assert_eq!(
            datum("#(a b c)"),
            Ok((
                "",
                Datum::Vector(vec![
                    Datum::Symbol(Identifier::InitialSubsequent("a".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("b".into())),
                    Datum::Symbol(Identifier::InitialSubsequent("c".into()))
                ])
            ))
        )
    }
}
