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

//! Group of expression rules.
//!
//! The list of all expressions is provided by the PHP Language Specification
//! in the [Grammar chapter, Expressions
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#expressions).

pub mod constant;
pub mod primaries;

use super::super::ast::Expression;
use super::super::tokens::Span;

named_attr!(
    #[doc="
        Recognize all kind of expressions.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::{Expression, Literal};
        use tagua_parser::rules::expressions::expression;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            expression(Span::new(b\"echo 'Hello, World!'\")),
            Result::Done(
                Span::new_at(b\"\", 20, 1, 21),
                Expression::Echo(vec![
                    Expression::Literal(
                        Literal::String(
                            Token::new(
                                b\"Hello, World!\".to_vec(),
                                Span::new_at(b\"'Hello, World!'\", 5, 1, 6)
                            )
                        )
                    )
                ])
            )
        );
        # }
        ```
    "],
    pub expression<Span, Expression>,
    call!(primaries::primary)
);
