#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
mod scheme_capi {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

use melior::{dialect::Handle, Context};
use std::mem;

fn make_context() -> Context {
    let handle =
        unsafe { Handle::from_raw(mem::transmute(scheme_capi::mlirGetDialectHandle__scheme__())) };
    let context = Context::new();
    handle.register_dialect(&context);

    context
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn context_sanity() {
        let context = make_context();
        println!("{:?}", context);
    }
}
