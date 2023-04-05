extern crate bindgen;
extern crate cmake;
extern crate string_cache_codegen;

use std::env;
use std::path::{Path, PathBuf};

fn main() {
    string_cache_codegen::AtomType::new("parser::atoms::KnownIdentifierAtom", "ident_atom!")
        .atoms(&["+", "-", "*", "/", "<", "=", ">"])
        .atoms(&["...", "!", "$", "%", "&", ":", "?", "~", "_", "^"])
        .atoms(&[
            "pair?", "list?", "cons", "car", "cdr", "set-car!", "set-cdr!", "null?",
        ])
        .atoms(&["number?", "complex?", "real?", "rational?", "integer?"])
        .atoms(&["boolean?"])
        .write_to_file(&Path::new(&env::var("OUT_DIR").unwrap()).join("identifier_atom.rs"))
        .unwrap();

    let dst = cmake::build("libMLIRScheme");
    println!("cargo:rustc-link-search=native={}/lib", dst.display());
    println!("cargo:rustc-link-lib=static=MLIRScheme");
    println!("cargo:rustc-link-lib=static=SchemeCAPI");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings");
}
