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

//! Group of white space rules.
//!
//! The list of all white spaces is provided by the PHP Language Specification
//! in the [Grammar chapter, White Space
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#white-space).

named_attr!(
    #[doc="
        Recognize all whitespaces.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::rules::whitespaces::whitespace;

        # fn main () {
        assert_eq!(whitespace(b\"\\n \\r\\tabc\"), Result::Done(&b\"abc\"[..], &b\"\\n \\r\\t\"[..]));
        # }
        ```
    "],
    pub whitespace,
    is_a!(" \t\n\r")
);


#[cfg(test)]
mod tests {
    use super::super::super::internal::{
        Error,
        ErrorKind,
        Result
    };
    use super::whitespace;

    #[test]
    fn case_whitespace_space() {
        assert_eq!(whitespace(b"   "), Result::Done(&b""[..], &b"   "[..]));
    }

    #[test]
    fn case_whitespace_horizontal_tabulation() {
        assert_eq!(whitespace(b"\t\t\t"), Result::Done(&b""[..], &b"\t\t\t"[..]));
    }

    #[test]
    fn case_whitespace_carriage_return_line_feed() {
        assert_eq!(whitespace(b"\r\n\r\n\r\n"), Result::Done(&b""[..], &b"\r\n\r\n\r\n"[..]));
    }

    #[test]
    fn case_whitespace_carriage_return() {
        assert_eq!(whitespace(b"\r\r\r"), Result::Done(&b""[..], &b"\r\r\r"[..]));
    }

    #[test]
    fn case_whitespace_line_feed() {
        assert_eq!(whitespace(b"\n\n\n"), Result::Done(&b""[..], &b"\n\n\n"[..]));
    }

    #[test]
    fn case_whitespace_mixed() {
        assert_eq!(whitespace(b"\n \n \r\t  \t\r\n\t \t\t"), Result::Done(&b""[..], &b"\n \n \r\t  \t\r\n\t \t\t"[..]));
    }

    #[test]
    fn case_whitespace_with_a_tail() {
        assert_eq!(whitespace(b"\n \n \r\t  \t\r\n\t \t\tabc "), Result::Done(&b"abc "[..], &b"\n \n \r\t  \t\r\n\t \t\t"[..]));
    }

    #[test]
    fn case_whitespace_too_short() {
        assert_eq!(whitespace(b""), Result::Done(&b""[..], &b""[..]));
    }

    #[test]
    fn case_invalid_whitespace_not_a_valid_whitespace() {
        assert_eq!(whitespace(b"\xa0 "), Result::Error(Error::Position(ErrorKind::IsA, &b"\xa0 "[..])));
    }

    #[test]
    fn case_invalid_whitespace_not_a_valid_character() {
        assert_eq!(whitespace(b"abc\n \t"), Result::Error(Error::Position(ErrorKind::IsA, &b"abc\n \t"[..])));
    }
}
