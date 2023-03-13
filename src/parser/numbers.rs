use std::num::{ParseFloatError, ParseIntError};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, one_of},
    combinator::{map, map_res, opt, recognize, value},
    error::{FromExternalError, ParseError},
    multi::{many0, many0_count, many1},
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Prefix {
    pub radix: Radix,
    pub exactness: Exactness,
}

fn prefix<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Prefix, E> {
    let (input, (radix, exactness)) =
        (pair(radix, exactness).or(pair(radix, exactness))).parse(input)?;
    Ok((input, Prefix { radix, exactness }))
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Num<'a> {
    pub prefix: Prefix,
    pub complex: Complex<'a>,
}

pub(super) fn num<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseFloatError>
        + FromExternalError<&'a str, ParseIntError>,
>(
    input: &'a str,
) -> IResult<&str, Num, E> {
    map(tuple((prefix, complex)), |(prefix, complex)| Num {
        prefix,
        complex,
    })(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Complex<'a> {
    pub real: Option<Real<'a>>,
    pub imag: Option<Real<'a>>,
}

fn complex<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseFloatError>
        + FromExternalError<&'a str, ParseIntError>,
>(
    input: &'a str,
) -> IResult<&'a str, Complex, E> {
    alt((
        map(real, |r| Complex {
            real: Some(r),
            imag: None,
        }),
        (map(separated_pair(real, char('+'), imag), |(real, u)| Complex {
            real: Some(real),
            imag: Some(Real::Positive(u)),
        })),
        (map(separated_pair(real, char('-'), imag), |(real, u)| Complex {
            real: Some(real),
            imag: Some(Real::Negative(u)),
        })),
        (map(preceded(char('+'), imag), |u| Complex {
            real: None,
            imag: Some(Real::Positive(u)),
        })),
        (map(preceded(char('-'), imag), |u| Complex {
            real: None,
            imag: Some(Real::Negative(u)),
        })),
    ))(input)
}

fn imag<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    input: &'a str,
) -> IResult<&str, Ureal, E> {
    alt((
        map(char('i'), |_| Ureal::Decimal(1.0)),
        terminated(ureal, char('i')),
    ))(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Real<'a> {
    Positive(Ureal<'a>),
    Negative(Ureal<'a>),
}

fn real<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    input: &'a str,
) -> IResult<&'a str, Real, E> {
    map(
        tuple((opt(alt((char('+'), char('-')))), ureal)),
        |(sign, ur)| match sign {
            Some('-') => Real::Positive(ur),
            Some('+') | None => Real::Positive(ur),
            _ => Real::Positive(ur),
        },
    )(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Ureal<'a> {
    Uinteger(Uinteger<'a>),
    Frac(Uinteger<'a>, Uinteger<'a>),
    Decimal(Decimal10),
}

fn ureal<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    input: &'a str,
) -> IResult<&'a str, Ureal, E> {
    alt((
        map(uinteger, Ureal::Uinteger),
        map(separated_pair(uinteger, char('/'), uinteger), |(n, d)| {
            Ureal::Frac(n, d)
        }),
        map(decimal10, Ureal::Decimal),
    ))(input)
}

fn uinteger10<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, u64, E> {
    map_res(
        pair(recognize(many1(digit10)), many0_count(char('#'))),
        |(num, zeros)| {
            num.parse::<u64>()
                .map(|u| u * (10_u64).pow(zeros.try_into().unwrap()))
        },
    )(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Uinteger<'a> {
    pub digits: &'a str,
    pub zeros: usize,
}

fn uinteger<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Uinteger, E> {
    map(
        pair(
            recognize(take_while1(|chr: char| {
                chr.is_digit(2)
                    || chr.is_digit(8)
                    || chr.is_ascii_digit()
                    || chr.is_ascii_hexdigit()
            })),
            many0_count(char('#')),
        ),
        |(digits, zeros)| Uinteger { digits, zeros },
    )
    .parse(input)
}

pub type Decimal10 = f64;

fn decimal10<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseFloatError>,
>(
    input: &'a str,
) -> IResult<&'a str, Decimal10, E> {
    alt((
        map_res(tuple((recognize(uinteger10), exponent)), |(u, e)| {
            let mut f = String::from(u);
            f.push('e');
            f.push_str(&e.to_string());
            f.parse::<f64>()
        }),
        map_res(
            tuple((
                alt((
                    recognize(tuple((char('.'), many1(digit10)))),
                    recognize(tuple((many1(digit10), char('.'), many0(digit10)))),
                )),
                opt(recognize(tuple((
                    preceded(
                        one_of("esfdl"),
                        map(opt(one_of("+-")), |s| match s {
                            Some('+') | None => 1,
                            Some('-') => -1,
                            _ => 1,
                        }),
                    ),
                    many1(one_of("0123456789")),
                )))),
            )),
            |(base, exp)| {
                let mut f = String::from(base);
                if let Some(e) = exp {
                    f.push_str(&e.replace(['e', 's', 'f', 'd', 'l'], "e"));
                }
                f.parse::<f64>()
            },
        ),
    ))(input)
}

fn exponent<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, i32, E> {
    map_res(
        tuple((
            preceded(
                one_of("esfdl"),
                map(opt(one_of("+-")), |s| match s {
                    Some('+') | None => 1,
                    Some('-') => -1,
                    _ => 1,
                }),
            ),
            many1(digit10),
        )),
        |(sign, digits)| {
            digits
                .into_iter()
                .collect::<String>()
                .parse::<u32>()
                .map(|u| (u as i32 * sign))
        },
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Exactness {
    Empty,
    Inexact,
    Exact,
}

fn exactness<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Exactness, E> {
    alt((
        value(Exactness::Inexact, char('i')),
        value(Exactness::Exact, char('e')),
        value(Exactness::Empty, tag("")),
    ))
    .parse(input)
}

fn digit10<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, char, E> {
    one_of("012345679")(input)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Radix {
    Two,
    Eight,
    Ten,
    Sixteen,
}

fn radix<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, Radix, E> {
    map(
        opt(alt((
            value(Radix::Two, tag("#b")),
            value(Radix::Eight, tag("#o")),
            value(Radix::Sixteen, tag("#x")),
            value(Radix::Ten, tag("#d")),
        ))),
        |r| r.unwrap_or(Radix::Ten),
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use nom::error::VerboseError;

    use super::*;

    #[test]
    fn test_prefix() {
        use Exactness::*;
        use Radix::*;

        fn case(input: &'static str, r: Radix, e: Exactness) -> Case {
            Case {
                input,
                prefix: Prefix {
                    radix: r,
                    exactness: e,
                },
            }
        }

        struct Case {
            input: &'static str,
            prefix: Prefix,
        }

        let cases = [
            case("#be", Two, Exact),
            case("#bi", Two, Inexact),
            case("#b", Two, Empty),
            case("#oe", Eight, Exact),
            case("#oi", Eight, Inexact),
            case("#o", Eight, Empty),
            case("e", Ten, Exact),
            case("#de", Ten, Exact),
            case("i", Ten, Inexact),
            case("", Ten, Empty),
            case("#xe", Sixteen, Exact),
            case("#xi", Sixteen, Inexact),
            case("#x", Sixteen, Empty),
        ];

        for case in cases {
            assert_eq!(
                prefix::<VerboseError<&str>>(case.input),
                Ok(("", case.prefix)),
                "input: {}",
                case.input
            );
        }
    }

    #[test]
    fn test_decimal10() {
        struct Case {
            input: &'static str,
            decimal: Decimal10,
        }

        fn case(input: &'static str, decimal: Decimal10) -> Case {
            Case { input, decimal }
        }

        let cases = [
            case("10e1", "10e1".parse::<f64>().unwrap()),
            case("10e-1", "10e-1".parse::<f64>().unwrap()),
            case("10e0", "10e0".parse::<f64>().unwrap()),
            case("10.0", 10.0),
            case("10.2e0", "10.2e0".parse::<f64>().unwrap()),
            case("10.2e3", "10.2e3".parse::<f64>().unwrap()),
            case("10.2e-3", "10.2e-3".parse::<f64>().unwrap()),
        ];

        for case in cases {
            assert_eq!(
                decimal10::<VerboseError<&str>>(case.input),
                Ok(("", case.decimal)),
                "parsing {}",
                case.input
            )
        }
    }
}
