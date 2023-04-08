use std::ops::Deref;

use inkwell::{
    builder::Builder,
    context::Context,
    intrinsics::Intrinsic,
    module::{Linkage, Module},
    types::{BasicType, IntType, PointerType, StructType},
    values::{FunctionValue, IntValue, PointerValue},
    AddressSpace,
};
use paste::paste;

pub enum ObjectType<'context> {
    Struct(StructType<'context>),
    Pointer(PointerType<'context>),
}

pub trait GCStructType<'context>: Deref<Target = StructType<'context>> {
    const GCDATA_FIELD_INDEX: u32;
    fn gcdata_field_type(&self) -> StructType<'context> {
        self.get_field_type_at_index(Self::GCDATA_FIELD_INDEX)
            .expect("should return gcdata field")
            .into_struct_type()
    }
}

pub struct PairType<'context>(StructType<'context>);
impl<'context> GCStructType<'context> for PairType<'context> {
    const GCDATA_FIELD_INDEX: u32 = 0;
}

impl<'context> Deref for PairType<'context> {
    type Target = StructType<'context>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

macro_rules! struct_type_methods {
    ($field:ident, $idx:literal) => {
        paste! {
            fn [<$field _index>](&self) -> u32 {
                $idx
            }

            fn [<$field _field_type>](&self) -> PointerType<'context> {
                self.0.get_field_type_at_index($idx).expect("should return field").into_pointer_type()
            }
            fn [<emit_ $field _gep>](&self,
                builder: &Builder<'context>,
                struct_ptr: PointerValue<'context>,
                name: &str) -> PointerValue<'context> {
                    builder.build_struct_gep(self.0, struct_ptr, self.[<$field _index>](), name).expect(concat!("should build ", stringify!($field), " GEP"))
                }
        }
    }
}

impl<'context> PairType<'context> {
    struct_type_methods!(car, 1);
    struct_type_methods!(cdr, 2);
}

pub struct VectorType<'context>(StructType<'context>);

impl<'context> Deref for VectorType<'context> {
    type Target = StructType<'context>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'context> GCStructType<'context> for VectorType<'context> {
    const GCDATA_FIELD_INDEX: u32 = 0;
}

impl<'context> VectorType<'context> {
    struct_type_methods!(length, 1);
    struct_type_methods!(elems, 2);
}

pub struct RuntimeTypes<'context> {
    pub object: PointerType<'context>,

    pub gcdata: StructType<'context>,
    pub addr_tag: IntType<'context>,

    pub pair: PairType<'context>,

    pub length: IntType<'context>,
    pub vector: VectorType<'context>,
    pub string: StructType<'context>,

    pub numerical: NumericTypes<'context>,
}

pub struct NumericTypes<'context> {
    pub complex: StructType<'context>,
    pub rational: StructType<'context>,
    pub float: StructType<'context>,
    pub integer: IntType<'context>,
}

pub fn make_runtime_types(ctx: &Context) -> RuntimeTypes {
    let object = ctx.i8_type().ptr_type(AddressSpace::from(1));
    let gcdata = {
        let marked = ctx.bool_type();
        ctx.struct_type(&[marked.into()], true)
    };

    let addr_tag = ctx.i64_type();

    let pair = PairType(ctx.struct_type(&[gcdata.into(), object.into(), object.into()], false));
    let length = ctx.i64_type();

    let vector = {
        let elems = object.ptr_type(Default::default());
        VectorType(ctx.struct_type(&[gcdata.into(), length.into(), elems.into()], false))
    };
    let string = {
        let s = ctx.i8_type().ptr_type(Default::default());
        ctx.struct_type(&[gcdata.into(), length.into(), s.into()], false)
    };

    let integer = ctx.i64_type();
    let float = {
        let f = ctx.f64_type();
        ctx.struct_type(&[gcdata.into(), f.into()], false)
    };
    let rational = ctx.struct_type(&[gcdata.into(), integer.into(), integer.into()], false);
    let complex = ctx.struct_type(&[gcdata.into(), object.into(), object.into()], false);

    RuntimeTypes {
        gcdata,
        object,
        addr_tag,
        pair,
        length,
        vector,
        string,
        numerical: NumericTypes {
            complex,
            rational,
            float,
            integer,
        },
    }
}

pub struct PairFunctions<'ctx> {
    car: FunctionValue<'ctx>,
    cdr: FunctionValue<'ctx>,
}

pub fn make_pair_fns<'context>(
    context: &'context Context,
    module: &Module<'context>,
    types: RuntimeTypes<'context>,
) -> PairFunctions<'context> {
    macro_rules! make_pair_fn {
        ($name:tt) => {{
            let fn_type = types.object.fn_type(&[types.object.into()], false);
            let func = module.add_function(stringify!($name), fn_type, Some(Linkage::Internal));
            let entry = context.append_basic_block(func, "entry");
            let builder = context.create_builder();
            builder.position_at_end(entry);

            let pair_ptr = types.emit_untag(
                module,
                &builder,
                func.get_nth_param(0)
                    .expect("should return object param")
                    .into_pointer_value(),
                types.pair.ptr_type(AddressSpace::default()),
            );

            paste! {
                let elem_ptr = types.pair.[<emit_ $name _gep>](&builder, pair_ptr, stringify!(name));
            }

            let elem = builder
                .build_load(types.object, elem_ptr, stringify!($name))
                .into_pointer_value();
            debug_assert_eq!(elem.get_type(), types.object);
            builder.build_return(Some(&elem));
            func
        }};
    }

    PairFunctions {
        car: make_pair_fn!(car),
        cdr: make_pair_fn!(cdr),
    }
}

struct VectorFunctions<'context> {
    r#ref: FunctionValue<'context>,
    set: FunctionValue<'context>,
}

fn emit_elems<'context>(
    types: &RuntimeTypes<'context>,
    ctx: &'context Context,
    module: &Module<'context>,
    builder: &Builder<'context>,
    param_ptr: PointerValue<'context>,
) -> PointerValue<'context> {
    let vector_ptr = types.emit_untag(
        module,
        builder,
        param_ptr,
        types.vector.ptr_type(AddressSpace::default()),
    );

    let elems_ptr = types.vector.emit_elems_gep(&builder, vector_ptr, "");
    let elems = builder
        .build_load(types.vector.elems_field_type(), elems_ptr, "elems")
        .into_pointer_value();
    return elems;
}

fn make_vector_functions<'context>(
    ctx: &'context Context,
    module: &Module<'context>,
    types: RuntimeTypes<'context>,
) -> VectorFunctions<'context> {
    let r#ref = {
        let fn_type = types
            .object
            .fn_type(&[types.object.into(), types.object.into()], false);
        let func = module.add_function("vector-ref", fn_type, Some(Linkage::Internal));
        let entry = ctx.append_basic_block(func, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(entry);

        let idx = types.emit_untag_as_int(
            module,
            &builder,
            func.get_nth_param(1)
                .expect("should return length param")
                .into_pointer_value(),
            "idx",
        );

        let elems = emit_elems(
            &types,
            ctx,
            module,
            &builder,
            func.get_nth_param(0)
                .expect("should return vector param")
                .into_pointer_value(),
        );

        let elem_ptr =
            unsafe { builder.build_in_bounds_gep(types.object, elems, &[idx.into()], "") };

        let elem = builder.build_load(types.object, elem_ptr, "elem");
        builder.build_return(Some(&elem));

        func
    };

    let set = {
        let fn_type = types.object.fn_type(
            &[
                types.object.into(),
                types.object.into(),
                types.object.into(),
            ],
            false,
        );
        let func = module.add_function("vector-set!", fn_type, Some(Linkage::Internal));
        let entry = ctx.append_basic_block(func, "entry");
        let builder = ctx.create_builder();
        builder.position_at_end(entry);

        let vec_param = func
            .get_nth_param(0)
            .expect("should return vector param")
            .into_pointer_value();

        let idx = types.emit_untag_as_int(
            module,
            &builder,
            func.get_nth_param(1)
                .expect("should return length param")
                .into_pointer_value(),
            "idx",
        );

        let elems = emit_elems(&types, ctx, module, &builder, vec_param);
        let elem_ptr =
            unsafe { builder.build_in_bounds_gep(types.object, elems, &[idx.into()], "") };

        builder.build_store(
            elem_ptr,
            func.get_nth_param(2).expect("should return obj param"),
        );
        builder.build_return(Some(&vec_param));

        func
    };

    VectorFunctions { r#ref, set }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pair_fns() {
        let context = Context::create();
        let module = context.create_module("test_pair_fns");
        let rt = make_runtime_types(&context);
        make_pair_fns(&context, &module, rt);

        assert_eq!(module.verify(), Ok(()));
    }

    #[test]
    fn vector_fns() {
        let context = Context::create();
        let module = context.create_module("test_vector_fns");
        let rt = make_runtime_types(&context);

        make_vector_functions(&context, &module, rt);
        module.print_to_stderr();

        assert_eq!(module.verify(), Ok(()))
    }
}
