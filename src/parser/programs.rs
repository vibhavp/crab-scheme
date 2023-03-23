use nom::{branch::alt, combinator::map, error::context, multi::many0, IResult};

use super::{
    definition, expression, whitespace_delimited, DatumParseError, Definition, Expression,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a>(pub Vec<Form<'a>>);

pub fn program<'a, E: DatumParseError<'a>>(input: &'a str) -> IResult<&'a str, Program, E> {
    context("program", map(many0(whitespace_delimited(form)), Program))(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Form<'a> {
    Definition(Definition<'a>),
    Expression(Expression<'a>),
}

fn form<'a, E: DatumParseError<'a>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "form",
        alt((
            map(definition, Form::Definition),
            map(expression, Form::Expression),
        )),
    )(input)
}
