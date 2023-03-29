extern crate string_cache_codegen;

use std::env;
use std::path::Path;

fn main() {
    string_cache_codegen::AtomType::new("parser::atoms::KnownIdentifierAtom", "ident_atom!")
        .atoms(&[
            "+", "-", "...", "!", "$", "%", "&", "*", "/", ":", "<", "=", ">", "?", "~", "_", "^",
        ])
        .atoms(&["cons", "car", "cdr", "set-car!", "set-cdr!", "null?"])
        .write_to_file(&Path::new(&env::var("OUT_DIR").unwrap()).join("identifier_atom.rs"))
        .unwrap()
}
