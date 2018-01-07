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

//! The grammar as a set of rules.
//!
//! The grammar is splitted into group of rules for the sake of clarity.

pub mod comments;
pub mod expressions;
pub mod literals;
pub mod skip;
pub mod statements;
pub mod tokens;
pub mod whitespaces;

use super::ast::Expression;
use super::tokens::Span;

/// The `root` parser is the axiom of the grammar, i.e. the entry
/// point of all the parsers.
///
/// # Examples
///
/// ```
/// use std::borrow::Cow;
/// use tagua_parser::ast::{
///     Expression,
///     Literal
/// };
/// use tagua_parser::tokens::{
///     Span,
///     Token
/// };
/// use tagua_parser::rules::root;
///
/// # fn main() {
/// let input  = Span::new(b"'Hello, World!'");
/// let output = Expression::Literal(Literal::String(Token::new(Cow::from(&b"Hello, World!"[..]), input)));
///
/// assert_eq!(root(input), output);
/// # }
/// ```
pub fn root(input: Span) -> Expression {
    match expressions::expression(input) {
        Ok((_, ast)) => ast,
        _ => panic!("Youhouuu")
    }
}


#[cfg(test)]
mod tests {
    use std::borrow::Cow;
    use super::root;
    use super::super::ast::{
        Expression,
        Literal
    };
    use super::super::tokens::{
        Span,
        Token
    };

    #[test]
    fn case_root() {
        let input  = Span::new(b"'Hello, World!'");
        let output = Expression::Literal(Literal::String(Token::new(Cow::from(&b"Hello, World!"[..]), input)));

        assert_eq!(root(input), output);
    }

    #[test]
    #[should_panic(expected = "Youhouuu")]
    fn case_root_panic() {
        root(Span::new(b"!"));
    }
}
