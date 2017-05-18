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
use super::super::tokens::Span;

named_attr!(
    #[doc="
        Recognize a variable.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Variable;
        use tagua_parser::rules::tokens::variable;
        use tagua_parser::tokens::Span;

        # fn main () {
        assert_eq!(
            variable(Span::new(b\"$foo\")),
            Result::Done(
                Span::new_at(b\"\", 4, 1, 5),
                Variable(Span::new_at(b\"foo\", 1, 1, 2))
            )
        );
        # }
        ```

        Note that the variable prefix `$` is not present in the output
        of the parser.
    "],
    pub variable<Span, Variable>,
    map_res!(
        preceded!(
            tag!(tokens::VARIABLE),
            name
        ),
        variable_mapper
    )
);

#[inline]
fn variable_mapper(span: Span) -> Result<Variable, ()> {
    Ok(Variable(span))
}

named_attr!(
    #[doc="
        Recognize a qualified name.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Name;
        use tagua_parser::rules::tokens::qualified_name;
        use tagua_parser::tokens::Span;

        # fn main () {
        assert_eq!(
            qualified_name(Span::new(b\"Foo\\\\Bar\\\\Baz\")),
            Result::Done(
                Span::new_at(b\"\", 11, 1, 12),
                Name::Qualified(vec![
                    Span::new_at(b\"Foo\", 0, 1, 1),
                    Span::new_at(b\"Bar\", 4, 1, 5),
                    Span::new_at(b\"Baz\", 8, 1, 9)
                ])
            )
        );
        # }
        ```
    "],
    pub qualified_name<Span, Name>,
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
                    if handle.as_slice() == tokens::NAMESPACE_SEPARATOR {
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

#[inline]
fn wrap_into_vector_mapper(span: Span) -> Result<Vec<Span>, ()> {
    Ok(vec![span])
}

named_attr!(
    #[doc="
        Recognize a name.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::rules::tokens::name;
        use tagua_parser::tokens::Span;

        # fn main () {
        assert_eq!(
            name(Span::new(b\"foo\")),
            Result::Done(
                Span::new_at(b\"\", 3, 1, 4),
                Span::new(b\"foo\")
            )
        );
        # }
        ```
    "],
    pub name<Span, Span>,
    regex!(r"(?-u)^[a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*")
);


#[cfg(test)]
mod tests {
    use nom::Slice;
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
    use super::super::super::tokens::Span;

    #[test]
    fn case_variable() {
        let input  = Span::new(b"$foo");
        let output = Result::Done(Span::new_at(b"", 4, 1, 5), Variable(input.slice(1..)));

        assert_eq!(variable(input), output);
    }

    #[test]
    fn case_variable_shortest() {
        let input  = Span::new(b"$x");
        let output = Result::Done(Span::new_at(b"", 2, 1, 3), Variable(input.slice(1..)));

        assert_eq!(variable(input), output);
    }

    #[test]
    fn case_invalid_variable_prefix() {
        let input = Span::new(b"x");

        assert_eq!(variable(input), Result::Error(Error::Position(ErrorKind::Tag, input)));
    }

    #[test]
    fn case_invalid_variable_name() {
        let input = Span::new(b"$0");

        assert_eq!(variable(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
    }

    #[test]
    fn case_unqualified_name() {
        let input  = Span::new(b"Foo");
        let output = Result::Done(Span::new_at(b"", 3, 1, 4), Name::Unqualified(input));

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_invalid_unqualified_name() {
        let input1 = Span::new(b"class");
        let input2 = Span::new(b"ClAsS");

        assert_eq!(qualified_name(input1), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), input1)));
        assert_eq!(qualified_name(input2), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), input2)));
    }

    #[test]
    fn case_qualified_name() {
        let input  = Span::new(b"Foo\\Bar\\Baz");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Name::Qualified(vec![
                Span::new(b"Foo"),
                Span::new_at(b"Bar", 4, 1, 5),
                Span::new_at(b"Baz", 8, 1, 9)
            ])
        );

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_qualified_name_vector_capacity() {
        if let Result::Done(_, Name::Qualified(vector)) = qualified_name(Span::new(b"Foo\\Bar\\Baz")) {
            assert_eq!(vector.capacity(), vector.len());
            assert_eq!(vector.len(), 3);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn case_qualified_name_with_skip_tokens() {
        let input  = Span::new(b"Foo\n/* baz */ \\ Bar /* qux */\\");
        let output = Result::Done(
            Span::new_at(b" /* qux */\\", 19, 2, 16),
            Name::Qualified(vec![
                Span::new(b"Foo"),
                Span::new_at(b"Bar", 16, 2, 13)
            ])
        );
        
        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_invalid_qualified_name() {
        assert_eq!(
            qualified_name(Span::new(b"Foo\\class\\Baz")),
            Result::Done(
                Span::new_at(b"\\class\\Baz", 3, 1, 4),
                Name::Unqualified(Span::new(b"Foo"))
            )
        );
        assert_eq!(
            qualified_name(Span::new(b"Foo\\ClAsS\\Baz")),
            Result::Done(
                Span::new_at(b"\\ClAsS\\Baz", 3, 1, 4),
                Name::Unqualified(Span::new(b"Foo"))
            )
        );
    }

    #[test]
    fn case_relative_qualified_name() {
        let input  = Span::new(b"namespace\\Foo\\Bar\\Baz");
        let output = Result::Done(
            Span::new_at(b"", 21, 1, 22),
            Name::RelativeQualified(vec![
                Span::new_at(b"Foo", 10, 1, 11),
                Span::new_at(b"Bar", 14, 1, 15),
                Span::new_at(b"Baz", 18, 1, 19)
            ])
        );

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_relative_qualified_name_vector_capacity() {
        if let Result::Done(_, Name::RelativeQualified(vector)) = qualified_name(Span::new(b"namespace\\Foo\\Bar\\Baz")) {
            assert_eq!(vector.capacity(), vector.len());
            assert_eq!(vector.len(), 3);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn case_relative_qualified_name_case_insensitive() {
        let input  = Span::new(b"NaMeSpAcE\\Foo\\Bar\\Baz");
        let output = Result::Done(
            Span::new_at(b"", 21, 1, 22),
            Name::RelativeQualified(vec![
                Span::new_at(b"Foo", 10, 1, 11),
                Span::new_at(b"Bar", 14, 1, 15),
                Span::new_at(b"Baz", 18, 1, 19)
            ])
        );

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_relative_qualified_name_with_skip_tokens() {
        let input  = Span::new(b"namespace/* baz */ \\ Foo\n/* qux */ \\ Bar /* hello */\\");
        let output = Result::Done(
            Span::new_at(b" /* hello */\\", 40, 2, 16),
            Name::RelativeQualified(vec![
                Span::new_at(b"Foo", 21, 1, 22),
                Span::new_at(b"Bar", 37, 2, 13)
            ])
        );

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_invalid_relative_qualified_name_with_namespace() {
        let input1 = Span::new(b"namespace\\Foo\\namespace\\Baz");
        let input2 = Span::new(b"namespace\\Foo\\NaMeSpAcE\\Baz");

        assert_eq!(
            qualified_name(input1),
            Result::Done(
                Span::new_at(b"\\namespace\\Baz", 13, 1, 14),
                Name::RelativeQualified(vec![Span::new_at(b"Foo", 10, 1, 11)])
            )
        );
        assert_eq!(
            qualified_name(input2),
            Result::Done(
                Span::new_at(b"\\NaMeSpAcE\\Baz", 13, 1, 14),
                Name::RelativeQualified(vec![Span::new_at(b"Foo", 10, 1, 11)])
            )
        );
    }

    #[test]
    fn case_invalid_relative_qualified_name_with_any_keyword() {
        let input1 = Span::new(b"namespace\\class");
        let input2 = Span::new(b"namespace\\ClAsS");

        assert_eq!(qualified_name(input1), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), Span::new_at(b"class", 10, 1, 11))));
        assert_eq!(qualified_name(input2), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), Span::new_at(b"ClAsS", 10, 1, 11))));
    }

    #[test]
    fn case_fully_qualified_name() {
        let input  = Span::new(b"\\Foo\\Bar\\Baz");
        let output = Result::Done(
            Span::new_at(b"", 12, 1, 13),
            Name::FullyQualified(vec![
                Span::new_at(b"Foo", 1, 1, 2),
                Span::new_at(b"Bar", 5, 1, 6),
                Span::new_at(b"Baz", 9, 1, 10)
            ])
        );

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_fully_qualified_name_vector_capacity() {
        if let Result::Done(_, Name::FullyQualified(vector)) = qualified_name(Span::new(b"\\Foo\\Bar\\Baz")) {
            assert_eq!(vector.capacity(), vector.len());
            assert_eq!(vector.len(), 3);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn case_fully_qualified_shortest_name() {
        let input  = Span::new(b"\\Foo");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Name::FullyQualified(vec![Span::new_at(b"Foo", 1, 1, 2)])
        );

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_invalid_fully_and_relative_qualified_name_with_namespace() {
        let input = Span::new(b"\\namespace\\Foo\\Bar");

        assert_eq!(qualified_name(input), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), input.slice(1..))));
    }

    #[test]
    fn case_invalid_fully_and_relative_qualified_name_with_any_keyword() {
        let input1 = Span::new(b"\\class\\Foo\\Bar");
        let input2 = Span::new(b"\\ClAsS\\Foo\\Bar");

        assert_eq!(qualified_name(input1), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), input1.slice(1..))));
        assert_eq!(qualified_name(input2), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), input2.slice(1..))));
    }

    #[test]
    fn case_invalid_qualified_name_ending_with_a_separator() {
        let input  = Span::new(b"Foo\\Bar\\");
        let output = Result::Done(
            Span::new_at(b"\\", 7, 1, 8),
            Name::Qualified(vec![
                Span::new(b"Foo"),
                Span::new_at(b"Bar", 4, 1, 5)
            ])
        );

        assert_eq!(qualified_name(input), output);
    }

    #[test]
    fn case_name() {
        let input  = Span::new(b"_fooBar42");
        let output = Result::Done(Span::new_at(b"", 9, 1, 10), input);

        assert_eq!(name(input), output);
    }

    #[test]
    fn case_name_shortest() {
        let input  = Span::new(b"x");
        let output = Result::Done(Span::new_at(b"", 1, 1, 2), input);

        assert_eq!(name(input), output);
    }

    #[test]
    fn case_name_only_head() {
        let input  = Span::new(b"aB_\x80");
        let output = Result::Done(Span::new_at(b"", 4, 1, 5), input);

        assert_eq!(name(input), output);
    }

    #[test]
    fn case_name_head_and_tail() {
        let input  = Span::new(b"aB_\x80aB7\xff");
        let output = Result::Done(Span::new_at(b"", 8, 1, 9), input);

        assert_eq!(name(input), output);
    }

    #[test]
    fn case_name_copyright() {
        // © = 0xa9
        let input  = Span::new(b"\xa9");
        let output = Result::Done(Span::new_at(b"", 1, 1, 2), input);

        assert_eq!(name(input), output);
    }

    #[test]
    fn case_name_non_breaking_space() {
        //   = 0xa0
        let input  = Span::new(b"\xa0");
        let output = Result::Done(Span::new_at(b"", 1, 1, 2), input);

        assert_eq!(name(input), output);
    }

    #[test]
    fn case_invalid_name() {
        assert_eq!(name(Span::new(b"0x")), Result::Error(Error::Code(ErrorKind::RegexpFind)));
    }
}
