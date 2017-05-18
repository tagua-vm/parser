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

//! Group of white space rules.
//!
//! The list of all white spaces is provided by the PHP Language Specification
//! in the [Grammar chapter, White Space
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#white-space).

use super::super::tokens::Span;

named_attr!(
    #[doc="
        Recognize all whitespaces.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::rules::whitespaces::whitespace;
        use tagua_parser::tokens::Span;

        # fn main () {
        assert_eq!(
            whitespace(Span::new(b\"\\n \\r\\tabc\")),
            Result::Done(
                Span::new_at(b\"abc\", 4, 2, 4),
                Span::new(b\"\\n \\r\\t\")
            )
        );
        # }
        ```
    "],
    pub whitespace<Span, Span>,
    is_a!(" \t\n\r")
);

#[cfg(test)]
mod tests {
    use super::super::super::internal::{
        Error,
        ErrorKind,
        Result
    };
    use super::super::super::tokens::Span;
    use super::whitespace;

    #[test]
    fn case_whitespace_space() {
        let input  = Span::new(b"   ");
        let output = Result::Done(Span::new_at(b"", 3, 1, 4), input);

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_whitespace_horizontal_tabulation() {
        let input  = Span::new(b"\t\t\t");
        let output = Result::Done(Span::new_at(b"", 3, 1, 4), input);

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_whitespace_carriage_return_line_feed() {
        let input  = Span::new(b"\r\n\r\n\r\n");
        let output = Result::Done(Span::new_at(b"", 6, 4, 1), input);

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_whitespace_carriage_return() {
        let input  = Span::new(b"\r\r\r");
        let output = Result::Done(Span::new_at(b"", 3, 1, 4), input);

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_whitespace_line_feed() {
        let input  = Span::new(b"\n\n\n");
        let output = Result::Done(Span::new_at(b"", 3, 4, 1), input);

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_whitespace_mixed() {
        let input  = Span::new(b"\n \n \r\t  \t\r\n\t \t\t");
        let output = Result::Done(Span::new_at(b"", 15, 4, 5), input);

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_whitespace_with_a_tail() {
        let input  = Span::new(b"\n \n \r\t  \t\r\n\t \t\tabc ");
        let output = Result::Done(Span::new_at(b"abc ", 15, 4, 5), Span::new(b"\n \n \r\t  \t\r\n\t \t\t"));

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_whitespace_too_short() {
        let input  = Span::new(b"");
        let output = Result::Done(Span::new_at(b"", 0, 1, 1), input);

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_invalid_whitespace_not_a_valid_whitespace() {
        let input  = Span::new(b"\xa0 ");
        let output = Result::Error(Error::Position(ErrorKind::IsA, input));

        assert_eq!(whitespace(input), output);
    }

    #[test]
    fn case_invalid_whitespace_not_a_valid_character() {
        let input  = Span::new(b"abc\n \t");
        let output = Result::Error(Error::Position(ErrorKind::IsA, input));

        assert_eq!(whitespace(input), output);
    }
}
