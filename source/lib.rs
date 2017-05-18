#![crate_type = "lib"]

// Tagua VM
//
//
// New BSD License
//
// Copyright © 2016-2017, Ivan Enderlin.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the Hoa nor the names of its contributors may be
//       used to endorse or promote products derived from this software without
//       specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

//! Tagua VM
//!
//! Tagua VM is an experimental [PHP](http://php.net/) Virtual Machine written
//! with [the Rust language](https://www.rust-lang.org/) and [the LLVM Compiler
//! Infrastructure](http://llvm.org/).
//!
//! This library contains lexical and syntactic analysers, aka the parser,
//! for the PHP language.
//!
//! This is a parser combinator. The immediate consequence is that the lexical
//! and syntax analyzers form a monolithic algorithm. The organization is the
//! following:
//!
//!   * The `tokens` module declares all the lexemes,
//!   * The `rules` module declares the grammar as a set of rules,
//!   * The `ast` module contains the structure that will constitute the AST.
//!
//! The parser is based on [nom](https://github.com/Geal/nom). nom is a parser
//! combinator library with a focus on safe parsing, streaming patterns, and as
//! much as possible zero copy. We try to enforce the zero copy property to
//! hold.

// Increase the macro recursion limit.
#![recursion_limit="128"]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
extern crate regex;
extern crate memchr;
extern crate bytecount;
#[cfg(test)]
#[macro_use]
extern crate quickcheck;

pub mod internal;
#[macro_use]
pub mod macros;
pub mod ast;
pub mod rules;
pub mod tokens;

pub use self::internal::*;

/// Complete parsing of a datum starting by the sentence symbol of the grammar.
///
/// The grammar is a set of rules. By definition, it has a sentence symbol,
/// also called the root rule. The `parse` function will lex, parse and produce
/// the associated AST of the `input` datum.
///
/// # Examples
///
/// ```
/// use tagua_parser::parse;
/// use tagua_parser::tokens::Span;
///
/// let expression = Span::new(b"1+2");
/// tagua_parser::parse(expression);
/// ```
pub fn parse(input: tokens::Span) -> ast::Expression {
    rules::root(input)
}
