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

//! Group of expression rules.

use nom::multispace;
use super::super::ast;
use super::super::tokens as token;
use super::literals::decimal;

named!(
    pub expr<&[u8], ast::Expression>,
    alt!(
        call!(mul)
      | call!(div)
      | call!(modulo)
      | call!(add)
      | call!(sub)
    )
);

named!(
    mul<&[u8], ast::Expression>,
    chain!(
        left: decimal ~
        opt!(multispace) ~
        tag!(token::MULTIPLY) ~
        opt!(multispace) ~
        right: decimal,
        || ast::Expression::Multiplication { a: ast::Term { t: left }, b: ast::Term { t: right } }
    )
);

named!(
    div<&[u8], ast::Expression>,
    chain!(
        left: decimal ~
        opt!(multispace) ~
        tag!(token::DIVIDE) ~
        opt!(multispace) ~
        right: decimal,
        || ast::Expression::Division { a: ast::Term { t: left }, b: ast::Term { t: right } }
    )
);

named!(
    modulo<&[u8], ast::Expression>,
    chain!(
        left: decimal ~
        opt!(multispace) ~
        tag!(token::MODULO) ~
        opt!(multispace) ~
        right: decimal,
        || ast::Expression::Modulo { a: ast::Term { t: left }, b: ast::Term { t: right } }
    )
);

named!(
    add<&[u8], ast::Expression>,
    chain!(
        left: decimal ~
        opt!(multispace) ~
        tag!(token::PLUS) ~
        opt!(multispace) ~
        right: decimal,
        || ast::Expression::Addition { a: ast::Term { t: left }, b: ast::Term { t: right } }
    )
);

named!(
    sub<&[u8], ast::Expression>,
    chain!(
        left: decimal ~
        opt!(multispace) ~
        tag!(token::SUBTRACT) ~
        opt!(multispace) ~
        right: decimal,
        || ast::Expression::Subtraction { a: ast::Term { t: left }, b: ast::Term { t: right } }
    )
);


#[cfg(test)]
mod tests {
    use nom::IResult::Done;
    use super::{
        expr,
        add,
        mul,
        div,
        sub,
        modulo
    };
    use super::super::super::ast;

    #[test]
    fn case_multiplication() {
        let input = b"1*2";
        let output = Done(
            &b""[..], ast::Expression::Multiplication { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), mul(input));
    }

    #[test]
    fn case_multiplication_with_spaces() {
        let input = b"1 *  \t2";
        let output = Done(
            &b""[..], ast::Expression::Multiplication { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), mul(input));
    }

    #[test]
    fn case_multiplication_with_new_lines() {
        let input = b"1\n*\r\n2";
        let output = Done(
            &b""[..], ast::Expression::Multiplication { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), mul(input));
    }

    #[test]
    fn case_division() {
        let input = b"1/2";
        let output = Done(
            &b""[..], ast::Expression::Division { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), div(input));
    }

    #[test]
    #[ignore]
    fn case_division_with_spaces() {
        let input = b"1 /  \t2";
        let output = Done(
            &b""[..], ast::Expression::Division { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), mul(input));
    }

    #[test]
    #[ignore]
    fn case_division_with_new_lines() {
        let input = b"1\n/\r\n2";
        let output = Done(
            &b""[..], ast::Expression::Division { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), mul(input));
    }

    #[test]
    fn cas_modulo() {
        let input = b"1%2";
        let output = Done(
            &b""[..], ast::Expression::Modulo { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), modulo(input));
    }

    #[test]
    fn cas_modulo_with_spaces() {
        let input = b"1 %  \t2";
        let output = Done(
            &b""[..], ast::Expression::Modulo { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), modulo(input));
    }

    #[test]
    fn cas_modulo_with_new_lines() {
        let input = b"1\n%\r\n2";
        let output = Done(
            &b""[..], ast::Expression::Modulo { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), modulo(input));
    }

    #[test]
    fn case_addition() {
        let input = b"1+2";
        let output = Done(
            &b""[..], ast::Expression::Addition { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), add(input));
    }

    #[test]
    fn case_addition_with_spaces() {
        let input = b"1 +  \t2";
        let output = Done(
            &b""[..], ast::Expression::Addition { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), add(input));
    }

    #[test]
    fn case_addition_with_new_lines() {
        let input = b"1\n+\r\n2";
        let output = Done(
            &b""[..], ast::Expression::Addition { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), add(input));
    }

    #[test]
    fn case_subtraction() {
        let input = b"1-2";
        let output = Done(
            &b""[..], ast::Expression::Subtraction { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), sub(input));
    }

    #[test]
    fn case_subtraction_with_spaces() {
        let input = b"1 -  \t2";
        let output = Done(
            &b""[..], ast::Expression::Subtraction { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), sub(input));
    }

    #[test]
    fn case_subtraction_with_new_lines() {
        let input = b"1\n-\r\n2";
        let output = Done(
            &b""[..], ast::Expression::Subtraction { a: ast::Term { t: 1 }, b: ast::Term { t: 2 } }
        );

        assert_eq!(expr(input), output);
        assert_eq!(expr(input), sub(input));
    }
}
