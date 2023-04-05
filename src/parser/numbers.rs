use super::whitespace_delimited;
use nom::{
    branch::{alt, permutation},
    character::complete::{char, digit0, digit1},
    combinator::{cut, map, map_res, opt, recognize, success, value},
    error::{context, ContextError, Error, FromExternalError, ParseError},
    multi::many0_count,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    AsChar, IResult, InputIter, InputLength, InputTakeAtPosition, Offset, Slice,
};
use num::{
    rational::Ratio, Complex as NumComplex, FromPrimitive, Num as NumTrait, One, ToPrimitive, Zero,
};
use std::{
    num::{ParseFloatError, ParseIntError},
    ops::{Add, Div, Mul, RangeFrom, RangeTo, Rem, Sub},
    str::FromStr,
};

pub trait ParseToRadix<T, E> {
    fn parse_to_radix(&self, radix: Radix) -> Result<T, E>;
}

impl ParseToRadix<usize, ParseIntError> for &str {
    fn parse_to_radix(&self, radix: Radix) -> Result<usize, ParseIntError> {
        usize::from_str_radix(self, radix as u32)
    }
}

pub trait ParseToResult<T, E> {
    fn parse_to_res(&self) -> Result<T, E>;
}

impl<T: FromStr> ParseToResult<T, T::Err> for &str {
    fn parse_to_res(&self) -> Result<T, T::Err> {
        T::from_str(self)
    }
}

pub trait ParseToNumber:
    Slice<RangeFrom<usize>>
    + Slice<RangeTo<usize>>
    + Offset
    + InputIter
    + InputLength
    + InputTakeAtPosition
    + ParseToResult<f64, ParseFloatError>
    + ParseToRadix<usize, ParseIntError>
    + ParseToResult<u32, ParseIntError>
    + Clone
{
}

impl<T> ParseToNumber for T where
    T: Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Offset
        + InputIter
        + InputLength
        + InputTakeAtPosition
        + ParseToResult<f64, ParseFloatError>
        + ParseToRadix<usize, ParseIntError>
        + ParseToResult<u32, ParseIntError>
        + Clone
{
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Prefix {
    pub radix: Radix,
    pub exactness: Exactness,
}

fn prefix<I, E: ParseError<I> + ContextError<I>>(input: I) -> IResult<I, Prefix, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
    context(
        "prefix",
        map(permutation((radix, exactness)), |(radix, exactness)| {
            Prefix { radix, exactness }
        }),
    )(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Num {
    pub exactness: Exactness,
    pub complex: NumComplex<Real>,
}

macro_rules! num_op_impl {
    ($t:ty, $f:ident) => {
        impl $t for Num {
            type Output = Self;
            fn $f(self, rhs: Self) -> Self {
                let exactness = if self.exactness != rhs.exactness {
                    Exactness::Inexact
                } else {
                    self.exactness
                };

                Num {
                    exactness,
                    complex: self.complex.$f(rhs.complex),
                }
            }
        }
    };
}
num_op_impl!(Add, add);
num_op_impl!(Mul, mul);
num_op_impl!(Div, div);
num_op_impl!(Sub, sub);
num_op_impl!(Rem, rem);

impl Zero for Num {
    fn zero() -> Self {
        Self {
            exactness: Exactness::Exact,
            complex: Zero::zero(),
        }
    }

    fn is_zero(&self) -> bool {
        self.complex.is_zero()
    }
}

impl One for Num {
    fn one() -> Self {
        Self {
            exactness: Exactness::Exact,
            complex: One::one(),
        }
    }

    fn is_one(&self) -> bool {
        self.complex.is_one()
    }
}

impl FromPrimitive for Num {
    fn from_i64(n: i64) -> Option<Self> {
        Some(Num {
            exactness: Exactness::Exact,
            complex: NumComplex::new(Real::from_i64(n)?, Real::zero()),
        })
    }

    fn from_u64(n: u64) -> Option<Self> {
        Some(Num {
            exactness: Exactness::Exact,
            complex: NumComplex::new(Real::from_u64(n)?, Real::zero()),
        })
    }

    fn from_f64(n: f64) -> Option<Self> {
        Some(Num {
            exactness: Exactness::Exact,
            complex: NumComplex::new(Real::from_f64(n)?, Real::zero()),
        })
    }
}

pub(super) fn num<
    I,
    E: ParseError<I>
        + FromExternalError<I, ParseIntError>
        + FromExternalError<I, ParseFloatError>
        + ContextError<I>,
>(
    input: I,
) -> IResult<I, Num, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    let (input, prefix) = prefix(input)?;
    map(complex(prefix.radix), move |complex| Num {
        exactness: complex.exactness(),
        complex: complex.into(),
    })(input)
}

#[derive(Debug, Default, PartialEq, Copy, Clone)]
struct Complex {
    pub real: Option<Real>,
    pub imag: Option<Real>,
}

impl From<Complex> for NumComplex<Real> {
    fn from(value: Complex) -> NumComplex<Real> {
        NumComplex::new(
            value.real.unwrap_or(Real::zero()),
            value.imag.unwrap_or(Real::zero()),
        )
    }
}

impl Complex {
    pub fn exactness(&self) -> Exactness {
        let real = self.real.map(|r| r.exactness()).unwrap_or(Exactness::Exact);
        let complex = self.imag.map(|r| r.exactness()).unwrap_or(Exactness::Exact);

        if real == complex {
            real
        } else {
            Exactness::Inexact
        }
    }
}

fn complex<
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Complex, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    alt((
        (map(preceded(char('+'), imag(radix)), |u| Complex {
            real: None,
            imag: Some(u.with_sign(1)),
        })),
        (map(preceded(char('-'), imag(radix)), |u| Complex {
            real: None,
            imag: Some(u.with_sign(-1)),
        })),
        (map(
            separated_pair(real(radix), whitespace_delimited(char('+')), imag(radix)),
            |(real, u)| Complex {
                real: Some(real),
                imag: Some(u.with_sign(1)),
            },
        )),
        (map(
            separated_pair(real(radix), whitespace_delimited(char('-')), imag(radix)),
            |(real, u)| Complex {
                real: Some(real),
                imag: Some(u.with_sign(-1)),
            },
        )),
        map(real(radix), |r| Complex {
            real: Some(r),
            imag: None,
        }),
    ))
}

fn imag<
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Ureal, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    alt((
        map(char('i'), |_| Ureal::Decimal(1.0)),
        terminated(ureal(radix), char('i')),
    ))
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Real {
    Integer(isize),
    Frac(Ratio<isize>),
    Decimal(Decimal10),
}

pub enum NumFromStrRadixErr {
    Nom(()),
    UnsupportedRadix,
}

impl NumTrait for Real {
    type FromStrRadixErr = NumFromStrRadixErr;

    fn from_str_radix(str: &str, r: u32) -> Result<Real, Self::FromStrRadixErr> {
        let radix = match r {
            2 => Radix::Two,
            8 => Radix::Eight,
            10 => Radix::Ten,
            16 => Radix::Sixteen,
            _ => return Err(NumFromStrRadixErr::UnsupportedRadix),
        };

        real::<_, Error<&str>>(radix)(str)
            .map(|(_, real)| real)
            .map_err(|_| NumFromStrRadixErr::Nom(()))
    }
}

impl ToPrimitive for Real {
    fn to_i64(&self) -> Option<i64> {
        match self {
            Self::Integer(i) => i.to_i64(),
            Self::Frac(r) => r.to_i64(),
            Self::Decimal(f) => f.to_i64(),
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match self {
            Self::Integer(i) => i.to_u64(),
            Self::Frac(r) => r.to_u64(),
            Self::Decimal(f) => f.to_u64(),
        }
    }

    fn to_f64(&self) -> Option<f64> {
        match self {
            Self::Integer(i) => i.to_f64(),
            Self::Frac(r) => r.to_f64(),
            Self::Decimal(f) => f.to_f64(),
        }
    }
}

impl FromPrimitive for Real {
    fn from_i64(n: i64) -> Option<Real> {
        Some(Real::Integer(n as isize))
    }

    fn from_u64(n: u64) -> Option<Real> {
        Some(Real::Integer(n as isize))
    }

    fn from_f64(n: f64) -> Option<Real> {
        Some(Real::Decimal(n))
    }
}

impl Zero for Real {
    fn zero() -> Self {
        Self::Integer(0)
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Decimal(f) => f.is_zero(),
            Self::Frac(r) => r.is_zero(),
            Self::Integer(i) => i.is_zero(),
        }
    }
}

impl One for Real {
    fn one() -> Self {
        Self::Integer(1)
    }

    fn is_one(&self) -> bool {
        match self {
            Self::Decimal(f) => f.is_one(),
            Self::Frac(r) => r.is_one(),
            Self::Integer(i) => i.is_one(),
        }
    }
}

macro_rules! op_impl {
    ($t:ty, $f:ident) => {
        impl $t for Real {
            type Output = Self;
            fn $f(self, rhs: Real) -> Self {
                if matches!(self, Real::Decimal(_)) || matches!(rhs, Real::Decimal(_)) {
                    Self::Decimal(self.to_f64().unwrap().$f(rhs.to_f64().unwrap()))
                } else {
                    let lhs = match self {
                        Real::Frac(r) => r,
                        Real::Integer(i) => Ratio::new_raw(i, 1),
                        _ => unreachable!(),
                    };
                    let rhs = match rhs {
                        Real::Frac(r) => r,
                        Real::Integer(i) => Ratio::new_raw(i, 1),
                        _ => unreachable!(),
                    };

                    let result = lhs.$f(rhs);
                    if result.is_integer() {
                        Self::Integer(*result.numer())
                    } else {
                        Self::Frac(result)
                    }
                }
            }
        }
    };
}

op_impl!(Add, add);
op_impl!(Mul, mul);
op_impl!(Sub, sub);
op_impl!(Div, div);
op_impl!(Rem, rem);

impl Real {
    pub fn exactness(&self) -> Exactness {
        if matches!(self, Self::Decimal(_)) {
            Exactness::Inexact
        } else {
            Exactness::Exact
        }
    }
}

fn real<
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Real, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    map(
        tuple((opt(alt((char('+'), char('-')))), ureal(radix))),
        |(sign, ur)| {
            ur.with_sign(match sign {
                Some('-') => -1,
                Some('+') | _ => 1,
            })
        },
    )
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Ureal {
    Uinteger(Uinteger),
    Frac(Ratio<usize>),
    Decimal(Decimal10),
}

impl Ureal {
    pub fn with_sign(&self, signum: i8) -> Real {
        match self {
            Ureal::Uinteger(u) => Real::Integer(signum as isize * *u as isize),
            Ureal::Frac(r) => {
                Real::Frac(Ratio::new(*r.numer() as isize, *r.denom() as isize) * signum as isize)
            }
            Ureal::Decimal(d) => Real::Decimal(d * signum as f64),
        }
    }
}

fn ureal<
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Ureal, E>
where
    I: ParseToNumber,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    alt((
        map(decimal10, Ureal::Decimal),
        map(
            separated_pair(
                uinteger(radix),
                whitespace_delimited(char('/')),
                uinteger(radix),
            ),
            |(n, d)| Ureal::Frac(Ratio::new(n, d)),
        ),
        map(uinteger(radix), Ureal::Uinteger),
    ))
}

fn uinteger10<I, E: ParseError<I> + FromExternalError<I, ParseIntError>>(
    input: I,
) -> IResult<I, usize, E>
where
    I: Slice<RangeFrom<usize>>
        + Offset
        + Slice<RangeTo<usize>>
        + InputIter
        + InputLength
        + ParseToRadix<usize, ParseIntError>
        + InputTakeAtPosition
        + Clone,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    uinteger(Radix::Ten)(input)
}

pub type Uinteger = usize;

fn uinteger<I, E: ParseError<I> + FromExternalError<I, ParseIntError>>(
    radix: Radix,
) -> impl FnMut(I) -> IResult<I, Uinteger, E>
where
    I: Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Offset
        + InputIter
        + InputLength
        + ParseToRadix<usize, ParseIntError>
        + InputTakeAtPosition
        + Clone,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    map_res(
        pair(
            recognize(match radix {
                Radix::Eight => digit1::<I, _>,
                Radix::Sixteen => digit1,
                Radix::Ten => digit1,
                Radix::Two => digit1,
            }),
            many0_count(char('#')),
        ),
        move |(digits, zeros)| {
            digits
                .parse_to_radix(radix)
                .map(|u| u * (10_usize).pow(zeros.try_into().unwrap()))
        },
    )
}

pub type Decimal10 = f64;

fn sign<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
    alt((value(1, char('+')), value(-1, char('-')), success(1)))(input)
}

fn decimal10<
    I,
    E: ParseError<I> + FromExternalError<I, ParseIntError> + FromExternalError<I, ParseFloatError>,
>(
    input: I,
) -> IResult<I, Decimal10, E>
where
    I: Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + Offset
        + InputIter
        + InputLength
        + InputTakeAtPosition
        + ParseToResult<f64, ParseFloatError>
        + ParseToRadix<usize, ParseIntError>
        + ParseToResult<u32, ParseIntError>
        + Clone,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    alt((
        map_res(
            tuple((recognize(uinteger10::<I, _>), exponent)),
            |(u, e)| {
                let mut f = String::from_iter(u.iter_elements().map(AsChar::as_char));
                f.push('e');
                f.push_str(&e.to_string());
                f.parse::<f64>()
            },
        ),
        map_res(
            tuple((
                alt((
                    recognize(tuple((char::<I, _>('.'), digit1))),
                    recognize(tuple((digit1, char('.'), digit0))),
                )),
                opt(recognize(tuple((
                    preceded(
                        alt((char('e'), char('s'), char('f'), char('d'), char('l'))),
                        sign,
                    ),
                    digit1,
                )))),
            )),
            |(base, exp)| {
                let mut f = String::from_iter(base.iter_elements().map(AsChar::as_char));
                if let Some(e) = exp {
                    f.extend(e.iter_elements().map(|c| match c.as_char() {
                        'e' | 's' | 'f' | 'd' | 'l' => 'e',
                        c => c,
                    }));
                }
                f.parse::<f64>()
            },
        ),
    ))(input)
}

fn exponent<I, E: ParseError<I> + FromExternalError<I, ParseIntError>>(
    input: I,
) -> IResult<I, i32, E>
where
    I: Slice<RangeFrom<usize>>
        + InputIter
        + Clone
        + InputTakeAtPosition
        + ParseToResult<u32, ParseIntError>,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    map_res(
        tuple((
            preceded(
                alt((
                    char::<I, _>('e'),
                    char('s'),
                    char('f'),
                    char('d'),
                    char('l'),
                )),
                sign,
            ),
            cut(digit1),
        )),
        |(sign, digits)| digits.parse_to_res().map(|u| (u as i32 * sign)),
    )(input)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Exactness {
    Inexact,
    Exact,
}

impl Default for Exactness {
    fn default() -> Self {
        Self::Exact
    }
}

fn exactness<I, E: ParseError<I>>(input: I) -> IResult<I, Exactness, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
    alt((
        value(Exactness::Inexact, char('i')),
        value(Exactness::Exact, char('e')),
        success(Exactness::Exact),
    ))(input)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Radix {
    Two = 2,
    Eight = 8,
    Ten = 10,
    Sixteen = 16,
}

fn radix<I, E: ParseError<I>>(input: I) -> IResult<I, Radix, E>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone,
    <I as InputIter>::Item: AsChar,
{
    map(
        opt(preceded(
            char('#'),
            alt((
                value(Radix::Two, char('b')),
                value(Radix::Eight, char('o')),
                value(Radix::Sixteen, char('x')),
                value(Radix::Ten, char('d')),
            )),
        )),
        |r| r.unwrap_or(Radix::Ten),
    )(input)
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
            case("#b", Two, Exact),
            case("#oe", Eight, Exact),
            case("#oi", Eight, Inexact),
            case("#o", Eight, Exact),
            case("e", Ten, Exact),
            case("#de", Ten, Exact),
            case("i", Ten, Inexact),
            case("", Ten, Exact),
            case("#xe", Sixteen, Exact),
            case("#xi", Sixteen, Inexact),
            case("#x", Sixteen, Exact),
        ];

        for case in cases {
            assert_eq!(
                prefix::<_, VerboseError<&str>>(case.input),
                Ok(("", case.prefix)),
                "input: {}",
                case.input
            );
        }
    }

    #[test]
    fn test_imag() {
        struct Case {
            input: &'static str,
            num: Num,
        }
        fn case(input: &'static str, exactness: Exactness, real: Real, imag: Real) -> Case {
            Case {
                input,
                num: Num {
                    exactness,
                    complex: NumComplex::new(real, imag),
                },
            }
        }
        let cases = [
            case(
                "-2.0i",
                Exactness::Inexact,
                Real::zero(),
                Real::from_f64(-2.0).unwrap(),
            ),
            case(
                "+2.0i",
                Exactness::Inexact,
                Real::zero(),
                Real::from_f64(2.0).unwrap(),
            ),
            case(
                "1 +2i",
                Exactness::Exact,
                Real::from_i64(1).unwrap(),
                Real::from_i64(2).unwrap(),
            ),
            case(
                "1.0 + 2i",
                Exactness::Inexact,
                Real::from_f64(1.0).unwrap(),
                Real::from_i64(2).unwrap(),
            ),
            case(
                "1+ 2.0i",
                Exactness::Inexact,
                Real::from_i64(1).unwrap(),
                Real::from_f64(2.0).unwrap(),
            ),
            case(
                "1.0  + 2.0i",
                Exactness::Inexact,
                Real::from_f64(1.0).unwrap(),
                Real::from_f64(2.0).unwrap(),
            ),
            case(
                "-1.0  + 2.0i",
                Exactness::Inexact,
                Real::from_f64(-1.0).unwrap(),
                Real::from_f64(2.0).unwrap(),
            ),
        ];

        for case in cases {
            assert_eq!(
                num::<_, VerboseError<&str>>(case.input),
                Ok(("", case.num)),
                "parsing {}",
                case.input
            )
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
                decimal10::<_, VerboseError<&str>>(case.input),
                Ok(("", case.decimal)),
                "parsing {}",
                case.input
            )
        }
    }
}
