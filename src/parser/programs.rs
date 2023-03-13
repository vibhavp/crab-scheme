use nom::{
    branch::alt,
    combinator::map,
    error::{context, VerboseError},
    multi::many0,
    IResult,
};

use super::{definition, expression, whitespace_delimited, Definition, Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a>(pub Vec<Form<'a>>);

pub fn program(input: &str) -> IResult<&str, Program, VerboseError<&str>> {
    context("program", map(many0(whitespace_delimited(form)), Program))(input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Form<'a> {
    Definition(Definition<'a>),
    Expression(Expression<'a>),
}

fn form(input: &str) -> IResult<&str, Form, VerboseError<&str>> {
    context(
        "form",
        alt((
            map(definition, Form::Definition),
            map(expression, Form::Expression),
        )),
    )(input)
}
