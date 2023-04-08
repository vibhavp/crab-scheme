use std::assert_matches::debug_assert_matches;

use num::{One, Zero};

use crate::{
    ir::{IRExpression, MathOp, NumberType, Operation, PrimitiveApplication, Type},
    ir_gen::build_tree::BuildTree,
    parser::{
        atoms::KnownIdentifierAtom, Datum, Expression, Num,
    },
};

use super::{build_exprs::ExprBuildError, translate::TranslationContext};

pub fn build_primitive<'a>(
    context: &mut TranslationContext<'a>,
    name: KnownIdentifierAtom,
    args: Vec<Expression<'a>>,
) -> Result<IRExpression<'a>, ExprBuildError> {
    match name {
        math_ident
        @ (ident_atom!("+") | ident_atom!("-") | ident_atom!("*") | ident_atom!("/")) => {
            if args.is_empty() {
                match math_ident {
                    ident_atom!("-") | ident_atom!("/") => {
                        return Err(ExprBuildError::InvalidPrimitiveArgs {
                            primitive: math_ident,
                            needed: 1,
                            have: 0,
                        })
                    }
                    ident_atom!("+") => {
                        return Ok(IRExpression::Constant(Datum::Number(Num::zero())))
                    }
                    ident_atom!("*") => {
                        return Ok(IRExpression::Constant(Datum::Number(Num::one())))
                    }
                    _ => unreachable!(),
                }
            }
            let init = match math_ident {
                ident_atom!("-") | ident_atom!("+") => {
                    IRExpression::Constant(Datum::Number(Num::zero()))
                }
                ident_atom!("*") | ident_atom!("/") => {
                    IRExpression::Constant(Datum::Number(Num::one()))
                }
                _ => unreachable!(),
            };

            debug_assert_matches!(init, IRExpression::Constant(Datum::Number(_)));

            let op = get_math_op(&math_ident);

            args.into_iter().try_fold(init, |acc, expr| {
                if let IRExpression::Constant(ref c) = acc {
                    debug_assert_matches!(c, Datum::Number(_));
                }

                let val = expr.build(context)?;

                let result = match acc {
                    // If we're adding two numerical constants, just fold them 
                    IRExpression::Constant(Datum::Number(num)) if let IRExpression::Constant(Datum::Number(num2)) = val => {
                        IRExpression::Constant(Datum::Number(num + num2))
                    }
                    _ => {
                        let (lhs_var, _) = context.push_define(None, true);
                        context.push_op(Operation::Set(lhs_var.clone(), acc), true);
                        
                        let rhs_var = if let IRExpression::Variable(v) = val {
                            v
                        } else {
                            let (arg_var, _) = context.push_define(None, true);
                            context.push_op(Operation::Set(arg_var.clone(), val), true);
                            arg_var
                        };
                        context.push_op(Operation::Primitive(
                            PrimitiveApplication::TypeAssert(
                                Type::Number(NumberType::Complex),
                                rhs_var.clone())),
                            true);
                        Ok(IRExpression::Primitive(PrimitiveApplication::Math(op, lhs_var, rhs_var)))
                    }?
                };

                Ok::<_, ExprBuildError>(result)
            })
        }
        type_predicate_ident @ (ident_atom!("pair?") | ident_atom!("list?") | ident_atom!("number?") | ident_atom!("complex?") | ident_atom!("rational?") | ident_atom!("integer?")) => {
            let typ = match type_predicate_ident {
                ident_atom!("pair?") | ident_atom!("list?") => Type::Cons,
                ident_atom!("number?") => Type::Number(NumberType::Number),
                ident_atom!("complex?") => Type::Number(NumberType::Complex),
                ident_atom!("rational?") => Type::Number(NumberType::Rational),
                ident_atom!("integer?") => Type::Number(NumberType::Integer),
                _ => unreachable!()
            };

            let mut args = args.into_iter();
            let val = args.next().ok_or_else(|| ExprBuildError::InvalidPrimitiveArgs{
                primitive: type_predicate_ident.clone(), needed: 1, have: 0})?;
            
            if args.len() != 0 {
                Err(ExprBuildError::InvalidPrimitiveArgs{
                    primitive: type_predicate_ident, needed: 1, have: args.len() + 1})
            } else {
                Ok(IRExpression::Primitive(PrimitiveApplication::TypePredicate(typ,
                    val.build_to_variable(context, true)?)))
            }
        }
        _ => todo!(),
    }
}

fn get_math_op(name: &KnownIdentifierAtom) -> MathOp {
    match *name {
        ident_atom!("+") => MathOp::Add,
        ident_atom!("*") => MathOp::Multiply,
        ident_atom!("-") => MathOp::Subtract,
        ident_atom!("/") => MathOp::Divide,
        _ => unreachable!(),
    }
}
