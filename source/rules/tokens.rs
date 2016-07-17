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

use super::super::tokens;
use super::super::ast::{
    Name,
    Variable
};

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

fn variable_mapper(string: &[u8]) -> Result<Variable, ()> {
    Ok(Variable(string))
}

named!(
    pub qualified_name<Name>,
    chain!(
        head: alt!(
            tag!(tokens::NAMESPACE_SEPARATOR)
          | terminated!(
                tag!(tokens::NAMESPACE),
                tag!(tokens::NAMESPACE_SEPARATOR)
            )
        )? ~
        mut accumulator: map_res!(
            exclude!(name, tag!(tokens::NAMESPACE)),
            wrap_into_vector_mapper
        ) ~
        many0!(
            tap!(
                tail: preceded!(
                    tag!(tokens::NAMESPACE_SEPARATOR),
                    exclude!(name, tag!(tokens::NAMESPACE))
                ) =>
                    accumulator.push(tail)
            )
        ),
        || {
            match head {
                Some(handle) =>
                    if handle == tokens::NAMESPACE_SEPARATOR {
                        Name::FullyQualified(accumulator)
                    } else {
                        Name::RelativeQualified(accumulator)
                    },

                None =>
                    if accumulator.len() > 1 {
                        Name::Qualified(accumulator)
                    } else {
                        Name::Unqualified(accumulator[0])
                    }
            }
        }
    )
);

fn wrap_into_vector_mapper(string: &[u8]) -> Result<Vec<&[u8]>, ()> {
    Ok(vec![string])
}

named!(
    pub name,
    re_bytes_find_static!(r"^[a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*")
);


#[cfg(test)]
mod tests {
    use nom::IResult::{Done, Error};
    use nom::{Err, ErrorKind};
    use super::{
        name,
        qualified_name,
        variable
    };
    use super::super::super::ast::{
        Name,
        Variable
    };
    use super::super::super::macros::ErrorKindCustom;

    #[test]
    fn case_variable() {
        assert_eq!(variable(b"$foo"), Done(&b""[..], Variable(&b"foo"[..])));
    }

    #[test]
    fn case_variable_shortest() {
        assert_eq!(variable(b"$x"), Done(&b""[..], Variable(&b"x"[..])));
    }

    #[test]
    fn case_invalid_variable_prefix() {
        assert_eq!(variable(b"x"), Error(Err::Position(ErrorKind::Tag, &b"x"[..])));
    }

    #[test]
    fn case_invalid_variable_name() {
        assert_eq!(variable(b"$0"), Error(Err::Code(ErrorKind::RegexpFind)));
    }

    #[test]
    fn case_unqualified_name() {
        assert_eq!(qualified_name(b"Foo"), Done(&b""[..], Name::Unqualified(&b"Foo"[..])));
    }

    #[test]
    fn case_qualified_name() {
        assert_eq!(qualified_name(b"Foo\\Bar\\Baz"), Done(&b""[..], Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..], &b"Baz"[..]])));
    }

    #[test]
    fn case_invalid_qualified_name() {
        assert_eq!(qualified_name(b"Foo\\namespace\\Baz"), Done(&b"\\namespace\\Baz"[..], Name::Unqualified(&b"Foo"[..])));
    }

    #[test]
    fn case_relative_qualified_name() {
        assert_eq!(qualified_name(b"namespace\\Foo\\Bar\\Baz"), Done(&b""[..], Name::RelativeQualified(vec![&b"Foo"[..], &b"Bar"[..], &b"Baz"[..]])));
    }

    #[test]
    fn case_invalid_relative_qualified_name() {
        assert_eq!(qualified_name(b"namespace\\Foo\\namespace\\Baz"), Done(&b"\\namespace\\Baz"[..], Name::RelativeQualified(vec![&b"Foo"[..]])));
    }

    #[test]
    fn case_fully_qualified_name() {
        assert_eq!(qualified_name(b"\\Foo\\Bar\\Baz"), Done(&b""[..], Name::FullyQualified(vec![&b"Foo"[..], &b"Bar"[..], &b"Baz"[..]])));
    }

    #[test]
    fn case_fully_qualified_shortest_name() {
        assert_eq!(qualified_name(b"\\Foo"), Done(&b""[..], Name::FullyQualified(vec![&b"Foo"[..]])));
    }

    #[test]
    fn case_invalid_fully_and_relative_qualified_name() {
        assert_eq!(qualified_name(b"\\namespace\\Foo\\Bar"), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"namespace\\Foo\\Bar"[..])));
    }

    #[test]
    fn case_invalid_qualified_name_ending_with_a_separator() {
        assert_eq!(qualified_name(b"Foo\\Bar\\"), Done(&b"\\"[..], Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..]])));
    }

    #[test]
    fn case_name() {
        assert_eq!(name(b"_fooBar42"), Done(&b""[..], &b"_fooBar42"[..]));
    }

    #[test]
    fn case_name_shortest() {
        assert_eq!(name(b"x"), Done(&b""[..], &b"x"[..]));
    }

    #[test]
    fn case_name_only_head() {
        assert_eq!(name(b"aB_\x80"), Done(&b""[..], &b"aB_\x80"[..]));
    }

    #[test]
    fn case_name_head_and_tail() {
        assert_eq!(name(b"aB_\x80aB7\xff"), Done(&b""[..], &b"aB_\x80aB7\xff"[..]));
    }

    #[test]
    fn case_name_copyright() {
        // © = 0xa9
        assert_eq!(name(b"\xa9"), Done(&b""[..], &b"\xa9"[..]));
    }

    #[test]
    fn case_name_non_breaking_space() {
        //   = 0xa0
        assert_eq!(name(b"\xa0"), Done(&b""[..], &b"\xa0"[..]));
    }

    #[test]
    fn case_invalid_name() {
        assert_eq!(name(b"0x"), Error(Err::Code(ErrorKind::RegexpFind)));
    }
}
