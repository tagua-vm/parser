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

//! The skip rule.
//!
//! The skip rule is a special rule representing all the tokens that are not
//! required. For instance, whitespaces and comments can most of the time be
//! skipped.

use super::comments::comment;
use super::super::tokens::Span;
use super::whitespaces::whitespace;

named_attr!(
    #[doc="
        Recognize all tokens to skip.

        A skip token is a token that is not relevant for the understanding of
        the language. It is present for comestic reasons only.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::rules::skip::skip;
        use tagua_parser::tokens::Span;

        # fn main () {
        assert_eq!(
            skip(Span::new(b\"/* foo */ \\n\\thello\")),
            Result::Done(
                Span::new_at(b\"hello\", 12, 2, 2),
                vec![
                    Span::new_at(b\" foo \", 2, 1, 3),
                    Span::new_at(b\" \\n\\t\", 9, 1, 10)
                ]
            )
        );
        # }
        ```
    "],
    pub skip<Span, Vec<Span>>,
    many0!(
        alt!(
            comment
          | whitespace
        )
    )
);


#[cfg(test)]
mod tests {
    use super::skip;
    use super::super::super::internal::Result;
    use super::super::super::tokens::Span;

    #[test]
    fn case_skip_comment() {
        let input  = Span::new(b"/* foo */hello");
        let output = Result::Done(Span::new_at(b"hello", 9, 1, 10), vec![Span::new_at(b" foo ", 2, 1, 3)]);

        assert_eq!(skip(input), output);
    }

    #[test]
    fn case_skip_whitespace() {
        let input  = Span::new(b"  \nhello");
        let output = Result::Done(Span::new_at(b"hello", 3, 2, 1), vec![Span::new_at(b"  \n", 0, 1, 1)]);

        assert_eq!(skip(input), output);
    }

    #[test]
    fn case_skip_comment_whitespace() {
        let input  = Span::new(b"/* foo */  \nhello");
        let output = Result::Done(
            Span::new_at(b"hello", 12, 2, 1),
            vec![
                Span::new_at(b" foo ", 2, 1, 3),
                Span::new_at(b"  \n", 9, 1, 10)
            ]
        );

        assert_eq!(skip(input), output);
    }
}
