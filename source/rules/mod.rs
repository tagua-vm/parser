// Tagua VM
//
//
// New BSD License
//
// Copyright © 2016-2016, Ivan Enderlin.
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

use super::ast;
use nom::IResult::Done;

pub fn root(input: &[u8]) -> ast::Expression {
    match expressions::expr(input) {
        Done(_, ast) => ast,
        _ => panic!("Youhouuu")
    }
}


#[cfg(test)]
mod tests {
    use nom::IResult::Done;
    use super::expressions::expr;
    use super::super::ast;

    #[test]
    fn case_expr_multiplication() {
        let input = b"1*2";
        let output = Done(
            &b""[..], ast::Expression::Multiplication { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
    }

    #[test]
    fn case_expr_division() {
        let input = b"1/2";
        let output = Done(
            &b""[..], ast::Expression::Division { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
    }

    #[test]
    fn case_expr_modulo() {
        let input = b"3%2";
        let output = Done(
            &b""[..], ast::Expression::Modulo { a: ast::Term { t: 3 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
    }

    #[test]
    fn case_expr_addition() {
        let input = b"1+2";
        let output = Done(
            &b""[..], ast::Expression::Addition { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
    }

    #[test]
    fn case_expr_subtraction() {
        let input = b"1-2";
        let output = Done(
            &b""[..], ast::Expression::Subtraction { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
    }
}
