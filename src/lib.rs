#![feature(strict_provenance)]
#![feature(assert_matches)]
#![feature(pointer_is_aligned)]
#![feature(if_let_guard)]

#[macro_use]
pub mod parser;

pub mod ir;
pub mod ir_gen;
