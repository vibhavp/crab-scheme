use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    intrinsics::Intrinsic,
    module::Module,
    types::PointerType,
    values::{IntValue, PointerMathValue, PointerValue},
};

use super::runtime::RuntimeTypes;

const TAG_MASK: u64 = 0xff << 56;

const TAG_CONS: u64 = 0x01 << 56;
const TAG_VECTOR: u64 = 0x02 << 56;
const TAG_STRING: u64 = 0x03 << 56;
const TAG_INTEGER: u64 = 0x04 << 56;
const TAG_RATIONAL: u64 = 0x05 << 56;
const TAG_REAL: u64 = 0x06 << 56;
const TAG_COMPLEX: u64 = 0x07 << 56;

const PTR_MASK: u64 = !TAG_MASK;

impl<'context> RuntimeTypes<'context> {
    fn ptr_mask(&self) -> IntValue<'context> {
        self.addr_tag.const_int(PTR_MASK, false)
    }

    pub fn emit_untag(
        &self,
        module: &Module<'context>,
        builder: &Builder<'context>,
        ptr: PointerValue<'context>,
        target_type: PointerType<'context>,
    ) -> PointerValue<'context> {
        let mask_intrinsic =
            Intrinsic::find("llvm.ptrmask").expect("should return the ptrmark intrinsic");
        let mask_func = mask_intrinsic
            .get_declaration(module, &[self.object.into(), self.addr_tag.into()])
            .expect("should return llvm.ptrmask.p1.i64");

        let untagged_ptr_1 = builder
            .build_call(mask_func, &[ptr.into(), self.ptr_mask().into()], "")
            .try_as_basic_value()
            .left()
            .expect("should return untagged ptr")
            .into_pointer_value();

        let untagged = builder.build_address_space_cast(untagged_ptr_1, target_type, "");
        debug_assert_eq!(untagged.get_type(), target_type);

        untagged
    }

    pub fn emit_untag_as_int(
        &self,
        module: &Module<'context>,
        builder: &Builder<'context>,
        ptr: PointerValue<'context>,
        name: &str,
    ) -> IntValue<'context> {
        let mask_intrinsic =
            Intrinsic::find("llvm.ptrmask").expect("should return the ptrmark intrinsic");
        let mask_func = mask_intrinsic
            .get_declaration(module, &[self.object.into(), self.addr_tag.into()])
            .expect("should return llvm.ptrmask.p1.i64");

        let untagged = builder
            .build_call(mask_func, &[ptr.into(), self.ptr_mask().into()], "")
            .try_as_basic_value()
            .left()
            .expect("should return untagged ptr")
            .into_pointer_value();

        builder.build_ptr_to_int(untagged, self.numerical.integer, name)
    }
}
