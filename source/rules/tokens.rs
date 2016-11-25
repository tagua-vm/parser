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

//! Group of token rules.
//!
//! The list of all tokens is provided by the PHP Language Specification in
//! the [Grammar chapter, Tokens
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#tokens).

use super::super::ast::{
    Name,
    Variable
};
use super::super::tokens;

named!(
    pub variable<Variable>,
    map_res!(
        preceded!(
            tag!(tokens::VARIABLE),
            name
        ),
        variable_mapper
    )
);

#[inline(always)]
fn variable_mapper(string: &[u8]) -> Result<Variable, ()> {
    Ok(Variable(string))
}

named!(
    pub qualified_name<Name>,
    do_parse!(
        head: opt!(
            alt!(
                tag!(tokens::NAMESPACE_SEPARATOR)
              | terminated!(
                    keyword!(tokens::NAMESPACE),
                    first!(tag!(tokens::NAMESPACE_SEPARATOR))
                )
            )
        ) >>
        accumulator: map_res!(
            exclude!(first!(name), tokens::keywords),
            wrap_into_vector_mapper
        ) >>
        result: fold_into_vector_many0!(
            preceded!(
                first!(tag!(tokens::NAMESPACE_SEPARATOR)),
                exclude!(first!(name), tokens::keywords)
            ),
            accumulator
        ) >>
        (
            match head {
                Some(handle) => {
                    if handle == tokens::NAMESPACE_SEPARATOR {
                        Name::FullyQualified(result)
                    } else {
                        Name::RelativeQualified(result)
                    }
                },

                None => {
                    if result.len() > 1 {
                        Name::Qualified(result)
                    } else {
                        Name::Unqualified(result[0])
                    }
                }
            }
        )
    )
);

#[inline(always)]
fn wrap_into_vector_mapper(string: &[u8]) -> Result<Vec<&[u8]>, ()> {
    Ok(vec![string])
}

named!(
    pub name,
    re_bytes_find_static!(r"^[a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*")
);


#[cfg(test)]
mod tests {
    use super::{
        name,
        qualified_name,
        variable
    };
    use super::super::super::ast::{
        Name,
        Variable
    };
    use super::super::super::internal::{
        Error,
        ErrorKind,
        Result
    };
    use super::super::super::macros::ErrorKindCustom;

    #[test]
    fn case_variable() {
        assert_eq!(variable(b"$foo"), Result::Done(&b""[..], Variable(&b"foo"[..])));
    }

    #[test]
    fn case_variable_shortest() {
        assert_eq!(variable(b"$x"), Result::Done(&b""[..], Variable(&b"x"[..])));
    }

    #[test]
    fn case_invalid_variable_prefix() {
        assert_eq!(variable(b"x"), Result::Error(Error::Position(ErrorKind::Tag, &b"x"[..])));
    }

    #[test]
    fn case_invalid_variable_name() {
        assert_eq!(variable(b"$0"), Result::Error(Error::Code(ErrorKind::RegexpFind)));
    }

    #[test]
    fn case_unqualified_name() {
        assert_eq!(qualified_name(b"Foo"), Result::Done(&b""[..], Name::Unqualified(&b"Foo"[..])));
    }

    #[test]
    fn case_invalid_unqualified_name() {
        assert_eq!(qualified_name(b"class"), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"class"[..])));
        assert_eq!(qualified_name(b"ClAsS"), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"ClAsS"[..])));
    }

    #[test]
    fn case_qualified_name() {
        assert_eq!(qualified_name(b"Foo\\Bar\\Baz"), Result::Done(&b""[..], Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..], &b"Baz"[..]])));
    }

    #[test]
    fn case_qualified_name_vector_capacity() {
        if let Result::Done(_, Name::Qualified(vector)) = qualified_name(b"Foo\\Bar\\Baz") {
            assert_eq!(vector.capacity(), vector.len());
            assert_eq!(vector.len(), 3);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn case_qualified_name_with_skip_tokens() {
        assert_eq!(qualified_name(b"Foo\n/* baz */ \\ Bar /* qux */\\"), Result::Done(&b" /* qux */\\"[..], Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..]])));
    }

    #[test]
    fn case_invalid_qualified_name() {
        assert_eq!(qualified_name(b"Foo\\class\\Baz"), Result::Done(&b"\\class\\Baz"[..], Name::Unqualified(&b"Foo"[..])));
        assert_eq!(qualified_name(b"Foo\\ClAsS\\Baz"), Result::Done(&b"\\ClAsS\\Baz"[..], Name::Unqualified(&b"Foo"[..])));
    }

    #[test]
    fn case_relative_qualified_name() {
        assert_eq!(qualified_name(b"namespace\\Foo\\Bar\\Baz"), Result::Done(&b""[..], Name::RelativeQualified(vec![&b"Foo"[..], &b"Bar"[..], &b"Baz"[..]])));
    }

    #[test]
    fn case_relative_qualified_name_vector_capacity() {
        if let Result::Done(_, Name::RelativeQualified(vector)) = qualified_name(b"namespace\\Foo\\Bar\\Baz") {
            assert_eq!(vector.capacity(), vector.len());
            assert_eq!(vector.len(), 3);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn case_relative_qualified_name_case_insensitive() {
        assert_eq!(qualified_name(b"NaMeSpAcE\\Foo\\Bar\\Baz"), Result::Done(&b""[..], Name::RelativeQualified(vec![&b"Foo"[..], &b"Bar"[..], &b"Baz"[..]])));
    }

    #[test]
    fn case_relative_qualified_name_with_skip_tokens() {
        assert_eq!(qualified_name(b"namespace/* baz */ \\ Foo\n/* qux */ \\ Bar /* hello */\\"), Result::Done(&b" /* hello */\\"[..], Name::RelativeQualified(vec![&b"Foo"[..], &b"Bar"[..]])));
    }

    #[test]
    fn case_invalid_relative_qualified_name_with_namespace() {
        assert_eq!(qualified_name(b"namespace\\Foo\\namespace\\Baz"), Result::Done(&b"\\namespace\\Baz"[..], Name::RelativeQualified(vec![&b"Foo"[..]])));
        assert_eq!(qualified_name(b"namespace\\Foo\\NaMeSpAcE\\Baz"), Result::Done(&b"\\NaMeSpAcE\\Baz"[..], Name::RelativeQualified(vec![&b"Foo"[..]])));
    }

    #[test]
    fn case_invalid_relative_qualified_name_with_any_keyword() {
        assert_eq!(qualified_name(b"namespace\\class"), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"class"[..])));
        assert_eq!(qualified_name(b"namespace\\ClAsS"), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"ClAsS"[..])));
    }

    #[test]
    fn case_fully_qualified_name() {
        assert_eq!(qualified_name(b"\\Foo\\Bar\\Baz"), Result::Done(&b""[..], Name::FullyQualified(vec![&b"Foo"[..], &b"Bar"[..], &b"Baz"[..]])));
    }

    #[test]
    fn case_fully_qualified_name_vector_capacity() {
        if let Result::Done(_, Name::FullyQualified(vector)) = qualified_name(b"\\Foo\\Bar\\Baz") {
            assert_eq!(vector.capacity(), vector.len());
            assert_eq!(vector.len(), 3);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn case_fully_qualified_shortest_name() {
        assert_eq!(qualified_name(b"\\Foo"), Result::Done(&b""[..], Name::FullyQualified(vec![&b"Foo"[..]])));
    }

    #[test]
    fn case_invalid_fully_and_relative_qualified_name_with_namespace() {
        assert_eq!(qualified_name(b"\\namespace\\Foo\\Bar"), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"namespace\\Foo\\Bar"[..])));
    }

    #[test]
    fn case_invalid_fully_and_relative_qualified_name_with_any_keyword() {
        assert_eq!(qualified_name(b"\\class\\Foo\\Bar"), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"class\\Foo\\Bar"[..])));
        assert_eq!(qualified_name(b"\\ClAsS\\Foo\\Bar"), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"ClAsS\\Foo\\Bar"[..])));
    }

    #[test]
    fn case_invalid_qualified_name_ending_with_a_separator() {
        assert_eq!(qualified_name(b"Foo\\Bar\\"), Result::Done(&b"\\"[..], Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..]])));
    }

    #[test]
    fn case_name() {
        assert_eq!(name(b"_fooBar42"), Result::Done(&b""[..], &b"_fooBar42"[..]));
    }

    #[test]
    fn case_name_shortest() {
        assert_eq!(name(b"x"), Result::Done(&b""[..], &b"x"[..]));
    }

    #[test]
    fn case_name_only_head() {
        assert_eq!(name(b"aB_\x80"), Result::Done(&b""[..], &b"aB_\x80"[..]));
    }

    #[test]
    fn case_name_head_and_tail() {
        assert_eq!(name(b"aB_\x80aB7\xff"), Result::Done(&b""[..], &b"aB_\x80aB7\xff"[..]));
    }

    #[test]
    fn case_name_copyright() {
        // © = 0xa9
        assert_eq!(name(b"\xa9"), Result::Done(&b""[..], &b"\xa9"[..]));
    }

    #[test]
    fn case_name_non_breaking_space() {
        //   = 0xa0
        assert_eq!(name(b"\xa0"), Result::Done(&b""[..], &b"\xa0"[..]));
    }

    #[test]
    fn case_invalid_name() {
        assert_eq!(name(b"0x"), Result::Error(Error::Code(ErrorKind::RegexpFind)));
    }
}
