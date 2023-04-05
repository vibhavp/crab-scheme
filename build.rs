extern crate string_cache_codegen;

use std::env;
use std::path::Path;

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
        .unwrap()
}
