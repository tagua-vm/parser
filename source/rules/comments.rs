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

//! Group of comment rules.
//!
//! The list of all comments is provided by the PHP Language Specification in
//! the [Grammar chapter, Comments
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#comments).

use super::super::tokens::Span;
use super::super::tokens;

named_attr!(
    #[doc="
        Recognize all kind of comments.

        A comment can be a single line (`//` or `#`) or a delimited block (`/* … */`).

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::rules::comments::comment;
        use tagua_parser::tokens::Span;

        # fn main () {
        assert_eq!(
            comment(Span::new(b\"/* foo */ bar\")),
            Result::Done(
                Span::new_at(b\" bar\", 9, 1, 10),
                Span::new_at(b\" foo \", 2, 1, 3)
            )
        );
        # }
        ```
    "],
    pub comment<Span, Span>,
    alt!(
        comment_delimited
      | comment_single_line
    )
);

named!(
    comment_delimited<Span, Span>,
    preceded!(
        tag!(tokens::BLOCK_COMMENT_OPEN),
        take_until_and_consume!(tokens::BLOCK_COMMENT_CLOSE)
    )
);

named!(
    comment_single_line<Span, Span>,
    preceded!(
        alt!(tag!(tokens::INLINE_COMMENT) | tag!(tokens::INLINE_COMMENT_HASH)),
        regex!(r"^(?-u).*?(\r\n|\n|$)")
    )
);


#[cfg(test)]
mod tests {
    use super::{
        comment,
        comment_delimited,
        comment_single_line
    };
    use super::super::super::internal::{
        Error,
        ErrorKind,
        Result
    };
    use super::super::super::tokens::Span;

    #[test]
    fn case_comment_single_line_double_slash_empty() {
        let input  = Span::new(b"//");
        let output = Result::Done(Span::new_at(b"", 2, 1, 3), Span::new_at(b"", 2, 1, 3));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_double_slash_with_feed() {
        let input  = Span::new(b"// foobar\nbazqux");
        let output = Result::Done(Span::new_at(b"bazqux", 10, 2, 1), Span::new_at(b" foobar\n", 2, 1, 3));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_double_slash_with_carriage_return_feed() {
        let input  = Span::new(b"// foobar\r\nbazqux");
        let output = Result::Done(Span::new_at(b"bazqux", 11, 2, 1), Span::new_at(b" foobar\r\n", 2, 1, 3));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_double_slash_without_ending() {
        let input  = Span::new(b"// foobar");
        let output = Result::Done(Span::new_at(b"", 9, 1, 10), Span::new_at(b" foobar", 2, 1, 3));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_double_slash_embedded() {
        let input  = Span::new(b"//foo//bar");
        let output = Result::Done(Span::new_at(b"", 10, 1, 11), Span::new_at(b"foo//bar", 2, 1, 3));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_hash_empty() {
        let input  = Span::new(b"#");
        let output = Result::Done(Span::new_at(b"", 1, 1, 2), Span::new_at(b"", 1, 1, 2));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_hash_with_line_feed() {
        let input  = Span::new(b"# foobar\nbazqux");
        let output = Result::Done(Span::new_at(b"bazqux", 9, 2, 1), Span::new_at(b" foobar\n", 1, 1, 2));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_hash_with_carriage_return_line_feed() {
        let input  = Span::new(b"# foobar\r\nbazqux");
        let output = Result::Done(Span::new_at(b"bazqux", 10, 2, 1), Span::new_at(b" foobar\r\n", 1, 1, 2));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_hash_without_line_ending() {
        let input  = Span::new(b"# foobar");
        let output = Result::Done(Span::new_at(b"", 8, 1, 9), Span::new_at(b" foobar", 1, 1, 2));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_single_line_hash_embedded() {
        let input  = Span::new(b"#foo#bar");
        let output = Result::Done(Span::new_at(b"", 8, 1, 9), Span::new_at(b"foo#bar", 1, 1, 2));

        assert_eq!(comment_single_line(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_delimited_empty() {
        let input  = Span::new(b"/**/xyz");
        let output = Result::Done(Span::new_at(b"xyz", 4, 1, 5), Span::new_at(b"", 2, 1, 3));

        assert_eq!(comment_delimited(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_delimited_almost_nested() {
        let input  = Span::new(b"/****/xyz");
        let output = Result::Done(Span::new_at(b"xyz", 6, 1, 7), Span::new_at(b"**", 2, 1, 3));

        assert_eq!(comment_delimited(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_comment_delimited() {
        let input  = Span::new(b"/* foo bar\nbaz\r\nqux // hello,\n /*world!*/xyz */");
        let output = Result::Done(Span::new_at(b"xyz */", 41, 4, 12), Span::new_at(b" foo bar\nbaz\r\nqux // hello,\n /*world!", 2, 1, 3));

        assert_eq!(comment_delimited(input), output);
        assert_eq!(comment(input), output);
    }

    #[test]
    fn case_invalid_comment_delimited_not_closed() {
        let input = Span::new(b"/*foobar");

        assert_eq!(comment_delimited(input), Result::Error(Error::Position(ErrorKind::TakeUntilAndConsume, Span::new_at(b"foobar", 2, 1, 3))));
        assert_eq!(comment(input), Result::Error(Error::Position(ErrorKind::Alt, input)));
    }
}
