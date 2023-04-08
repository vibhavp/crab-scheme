mod runtime;
mod tags;

use inkwell::{builder::Builder, context::Context, module::Module};

use crate::ir::{Operation, Procedure, IR};

use self::runtime::RuntimeTypes;

pub struct CodegenContext<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    types: RuntimeTypes<'a>,
}

pub trait BuildLLVMIR<'a> {
    fn build_llvm_ir(self, ir: &IR<'a>, ctx: &CodegenContext<'a>);
}
