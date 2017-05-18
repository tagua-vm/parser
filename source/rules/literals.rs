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

//! Group of literal rules.
//!
//! The list of all literals is provided by the PHP Language Specification in
//! the [Grammar chapter, Literals
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#literals).

use nom::{
    InputLength,
    Slice
};
use std::num::{
    ParseFloatError,
    ParseIntError
};
use std::result::Result as StdResult;
use std::str::FromStr;
use std::str;
use super::super::ast::Literal;
use super::super::tokens::{
    Span,
    Token
};
use super::super::internal::{
    Error,
    ErrorKind,
    Result
};
use super::tokens;

named_attr!(
    #[doc="
        Recognize all kind of literals.

        A literal is either a null, a boolean, an expotential, an integer or an integer.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::literal;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            literal(Span::new(b\"0x2a\")),
            Result::Done(
                Span::new_at(b\"\", 4, 1, 5),
                Literal::Integer(Token::new(42i64, Span::new(b\"0x2a\")))
            )
        );
        # }
        ```
    "],
    pub literal<Span, Literal>,
    alt!(
        null
      | boolean
      | exponential
      | integer
      | string
    )
);

named_attr!(
    #[doc="
        Recognize a null value.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::null;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            null(Span::new(b\"null\")),
            Result::Done(
                Span::new_at(b\"\", 4, 1, 5),
                Literal::Null(Token::new((), Span::new(b\"null\")))
            )
        );
        # }
        ```
    "],
    pub null<Span, Literal>,
    map_res!(
        itag!(b"null"),
        null_mapper
    )
);

#[inline]
fn null_mapper(span: Span) -> StdResult<Literal, ()> {
    Ok(Literal::Null(Token::new((), span)))
}

named_attr!(
    #[doc="
        Recognize a boolean.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::boolean;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            boolean(Span::new(b\"true\")),
            Result::Done(
                Span::new_at(b\"\", 4, 1, 5),
                Literal::Boolean(Token::new(true, Span::new(b\"true\")))
            )
        );
        # }
        ```
    "],
    pub boolean<Span, Literal>,
    map_res!(
        alt!(itag!(b"true") | itag!(b"false")),
        boolean_mapper
    )
);

#[inline]
fn boolean_mapper(span: Span) -> StdResult<Literal, ()> {
    Ok(Literal::Boolean(Token::new(span.as_slice()[0] == b't', span)))
}

named_attr!(
    #[doc="
        Recognize an integer.

        An integer is either a binary, a decimal, an hexadecimal or an octal representation.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::integer;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            integer(Span::new(b\"0b101010\")),
            Result::Done(
                Span::new_at(b\"\", 8, 1, 9),
                Literal::Integer(Token::new(42i64, Span::new(b\"0b101010\")))
            )
        );
        # }
        ```
    "],
    pub integer<Span, Literal>,
    alt_complete!(
        binary
      | decimal
      | hexadecimal
      | octal
    )
);

named_attr!(
    #[doc="
        Recognize an integer with the binary notation.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::binary;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            binary(Span::new(b\"0b101010\")),
            Result::Done(
                Span::new_at(b\"\", 8, 1, 9),
                Literal::Integer(Token::new(42i64, Span::new(b\"0b101010\")))
            )
        );
        # }
        ```
    "],
    pub binary<Span, Literal>,
    map_res!(
        regex!(r"(?-u)^0[bB][01]+"),
        binary_mapper
    )
);

#[inline]
fn binary_mapper(span: Span) -> StdResult<Literal, ParseIntError> {
    i64
        ::from_str_radix(
            unsafe { str::from_utf8_unchecked(&(span.as_slice())[2..]) },
            2
        )
        .and_then(
            |binary| {
                Ok(Literal::Integer(Token::new(binary, span)))
            }
        )
}

named_attr!(
    #[doc="
        Recognize an integer with the octal notation.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::octal;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            octal(Span::new(b\"052\")),
            Result::Done(
                Span::new_at(b\"\", 3, 1, 4),
                Literal::Integer(Token::new(42i64, Span::new(b\"052\")))
            )
        );
        # }
        ```
    "],
    pub octal<Span, Literal>,
    map_res!(
        regex!(r"(?-u)^0[0-7]*"),
        octal_mapper
    )
);

#[inline]
fn octal_mapper(span: Span) -> StdResult<Literal, ParseIntError> {
    if span.input_len() > 1 {
        i64
            ::from_str_radix(
                unsafe { str::from_utf8_unchecked(span.as_slice()) },
                8
            )
            .and_then(
                |octal| {
                    Ok(Literal::Integer(Token::new(octal, span)))
                }
            )
    } else {
        Ok(Literal::Integer(Token::new(0i64, span)))
    }
}

named_attr!(
    #[doc="
        Recognize an integer with the decimal notation.

        If the integer is too large to fit in an `i64`, then it will be a `f64`.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::decimal;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            decimal(Span::new(b\"42\")),
            Result::Done(
                Span::new_at(b\"\", 2, 1, 3),
                Literal::Integer(Token::new(42i64, Span::new(b\"42\")))
            )
        );
        # }
        ```
    "],
    pub decimal<Span, Literal>,
    map_res!(
        regex!(r"(?-u)^[1-9][0-9]*"),
        decimal_mapper
    )
);

#[inline]
fn decimal_mapper(span: Span) -> StdResult<Literal, ParseFloatError> {
    let string = unsafe { str::from_utf8_unchecked(span.as_slice()) };

    i64
        ::from_str(string)
        .and_then(
            |decimal| {
                Ok(Literal::Integer(Token::new(decimal, span)))
            }
        )
        .or_else(
            |_: ParseIntError| {
                f64
                    ::from_str(string)
                    .and_then(
                        |decimal| {
                            Ok(Literal::Real(Token::new(decimal, span)))
                        }
                    )
            }
        )
}

named_attr!(
    #[doc="
        Recognize an integer with the hexadecimal notation.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::decimal;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            decimal(Span::new(b\"42\")),
            Result::Done(
                Span::new_at(b\"\", 2, 1, 3),
                Literal::Integer(Token::new(42i64, Span::new(b\"42\")))
            )
        );
        # }
        ```
    "],
    pub hexadecimal<Span, Literal>,
    map_res!(
        regex!(r"(?-u)^0[xX][0-9a-fA-F]+"),
        hexadecimal_mapper
    )
);

#[inline]
fn hexadecimal_mapper(span: Span) -> StdResult<Literal, ParseIntError> {
    i64
        ::from_str_radix(
            unsafe { str::from_utf8_unchecked(&(span.as_slice())[2..]) },
            16
        )
        .and_then(
            |hexadecimal| {
                Ok(Literal::Integer(Token::new(hexadecimal, span)))
            }
        )
}

named_attr!(
    #[doc="
        Recognize a real with the exponential notation.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::exponential;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            exponential(Span::new(b\"123.456e+78\")),
            Result::Done(
                Span::new_at(b\"\", 11, 1, 12),
                Literal::Real(Token::new(123.456e78f64, Span::new(b\"123.456e+78\")))
            )
        );
        # }
        ```
    "],
    pub exponential<Span, Literal>,
    map_res!(
        regex!(r"(?-u)^(([0-9]*\.[0-9]+|[0-9]+\.)([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)"),
        exponential_mapper
    )
);

#[inline]
fn exponential_mapper(span: Span) -> StdResult<Literal, ParseFloatError> {
    f64
        ::from_str(unsafe { str::from_utf8_unchecked(span.as_slice()) })
        .and_then(
            |exponential| {
                Ok(Literal::Real(Token::new(exponential, span)))
            }
        )
}

/// String errors.
pub enum StringError {
    /// The datum starts as a string but is too short to be a string.
    TooShort,

    /// The string open character is not correct.
    InvalidOpeningCharacter,

    /// The string close character is not correct.
    InvalidClosingCharacter,

    /// The string is not correctly encoded (expect UTF-8).
    InvalidEncoding,

    /// The string delimiter identifier is syntactically invalid.
    InvalidDelimiterIdentifier
}

named_attr!(
    #[doc="
        Recognize all kind of strings.

        # Examples

        ```
        # extern crate tagua_parser;
        use tagua_parser::Result;
        use tagua_parser::ast::Literal;
        use tagua_parser::rules::literals::string;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main () {
        assert_eq!(
            string(Span::new(b\"'foobar'\")),
            Result::Done(
                Span::new_at(b\"\", 8, 1, 9),
                Literal::String(Token::new(b\"foobar\".to_vec(), Span::new(b\"'foobar'\")))
            )
        );
        # }
        ```
    "],
    pub string<Span, Literal>,
    alt!(
        string_single_quoted
      | string_nowdoc
    )
);

fn string_single_quoted(span: Span) -> Result<Span, Literal> {
    let input        = span.as_slice();
    let input_length = span.input_len();

    if input_length < 2 {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
    }

    if input[0] == b'b' || input[0] == b'B' {
        if input_length < 3 {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
        } else if input[1] != b'\'' {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
        } else {
            return string_single_quoted(span.slice(1..));
        }
    } else if input[0] != b'\'' {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    let mut output   = Vec::new();
    let mut offset   = 1;
    let mut iterator = input[offset..].iter().enumerate();

    while let Some((index, item)) = iterator.next() {
        if *item == b'\\' {
            if let Some((next_index, next_item)) = iterator.next() {
                if *next_item == b'\'' ||
                   *next_item == b'\\' {
                    output.extend(&input[offset..index + 1]);
                    offset = next_index + 1;
                }
            } else {
                return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
            }
        } else if *item == b'\'' {
            output.extend(&input[offset..index + 1]);

            return Result::Done(
                span.slice(index + 2..),
                Literal::String(Token::new(output, span.slice(..index + 2)))
            );
        }
    }

    Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)))
}

const STRING_NOWDOC_OPENING: &'static [u8] = &[b'<', b'<', b'<'];

fn string_nowdoc(span: Span) -> Result<Span, Literal> {
    let input        = span.as_slice();
    let input_length = span.input_len();

    // `<<<'A'\nA\n` is the shortest datum.
    if input_length < 9 {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
    }

    if input[0] == b'b' || input[0] == b'B' {
        if input_length < 10 {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
        } else if !input[1..].starts_with(STRING_NOWDOC_OPENING) {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
        } else {
            return string_nowdoc(span.slice(1..));
        }
    } else if !input.starts_with(STRING_NOWDOC_OPENING) {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    let mut offset = 3;

    for item in input[offset..].iter() {
        if *item != b' ' && *item != b'\t' {
            break;
        }

        offset += 1;
    }

    if input[offset] != b'\'' {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    offset += 1;

    let name_span;
    let next_span;

    if let Result::Done(i, n) = tokens::name(span.slice(offset..)) {
        name_span = n;
        next_span = i;
    } else {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidDelimiterIdentifier as u32)))
    }

    let name              = name_span.as_slice();
    let name_length       = name_span.input_len();
    let next_input        = next_span.as_slice();
    let next_input_length = next_span.input_len();

    if next_input_length < 3 + name_length || next_input[0] != b'\'' || next_input[1] != b'\n' {
        if next_input[1] != b'\r' || next_input[2] != b'\n' {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
        }
    }

    let mut span_offset = offset + name_length;

    if next_input[1] == b'\r' {
        offset = 2;
    } else {
        offset = 1;
    }

    span_offset += offset;

    for (index, item) in next_input[offset..].iter().enumerate() {
        if *item == b'\n' {
            if !next_input[offset + index + 1..].starts_with(name) {
                continue;
            }

            let mut lookahead_offset = offset + index + name_length + 1;

            if lookahead_offset >= next_input_length {
                return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
            }

            if next_input[lookahead_offset] == b';' {
                lookahead_offset += 1;
            }

            if lookahead_offset >= next_input_length {
                return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
            }

            let mut ending_offset = 0;

            if next_input[lookahead_offset] == b'\r' {
                if lookahead_offset + 1 >= next_input_length {
                    return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
                }

                ending_offset     = 1;
                lookahead_offset += 1;
            }

            if next_input[lookahead_offset] == b'\n' {
                if index == 0 {
                    return Result::Done(
                        next_span.slice(lookahead_offset + 1..),
                        Literal::String(Token::new(Vec::new(), span.slice(..span_offset + lookahead_offset - ending_offset)))
                    );
                }

                return Result::Done(
                    next_span.slice(lookahead_offset + 1..),
                    Literal::String(
                        Token::new(
                            next_input[offset + 1..offset - ending_offset + index].to_vec(),
                            span.slice(..span_offset + lookahead_offset - ending_offset)
                        )
                    )
                );
            }
        }
    }

    Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)))
}


#[cfg(test)]
mod tests {
    use nom::Slice;
    use super::{
        StringError,
        binary,
        boolean,
        decimal,
        exponential,
        hexadecimal,
        integer,
        literal,
        null,
        octal,
        string,
        string_nowdoc,
        string_single_quoted
    };
    use super::super::super::ast::Literal;
    use super::super::super::internal::{
        Error,
        ErrorKind,
        Result
    };
    use super::super::super::tokens::{
        Span,
        Token
    };

    #[test]
    fn case_null() {
        let input  = Span::new(b"null");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Null(Token::new((), input))
        );

        assert_eq!(null(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_null_case_insensitive() {
        let input  = Span::new(b"NuLl");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Null(Token::new((), Span::new(b"null")))
        );

        assert_eq!(null(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_true() {
        let input  = Span::new(b"true");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Boolean(Token::new(true, input))
        );

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_true_case_insensitive() {
        let input  = Span::new(b"TrUe");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Boolean(Token::new(true, Span::new(b"true")))
        );

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_false() {
        let input  = Span::new(b"false");
        let output = Result::Done(
            Span::new_at(b"", 5, 1, 6),
            Literal::Boolean(Token::new(false, input))
        );

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_false_case_insensitive() {
        let input  = Span::new(b"FaLsE");
        let output = Result::Done(
            Span::new_at(b"", 5, 1, 6),
            Literal::Boolean(Token::new(false, Span::new(b"false")))
        );

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_lowercase_b() {
        let input  = Span::new(b"0b101010");
        let output = Result::Done(
            Span::new_at(b"", 8, 1, 9),
            Literal::Integer(Token::new(42i64, input))
        );

        assert_eq!(binary(input),  output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_uppercase_b() {
        let input  = Span::new(b"0B101010");
        let output = Result::Done(
            Span::new_at(b"", 8, 1, 9),
            Literal::Integer(Token::new(42i64, input))
        );

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_maximum_integer_value() {
        let input  = Span::new(b"0b111111111111111111111111111111111111111111111111111111111111111");
        let output = Result::Done(
            Span::new_at(b"", 65, 1, 66),
            Literal::Integer(Token::new(::std::i64::MAX, input))
        );

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_overflow() {
        let input  = Span::new(b"0b1000000000000000000000000000000000000000000000000000000000000000");
        let output = Result::Done(
            Span::new_at(b"b1000000000000000000000000000000000000000000000000000000000000000", 1, 1, 2),
            Literal::Integer(Token::new(0i64, Span::new(b"0")))
        );

        assert_eq!(binary(input), Result::Error(Error::Position(ErrorKind::MapRes, input)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_no_number() {
        let input  = Span::new(b"0b");
        let output = Result::Done(
            Span::new_at(b"b", 1, 1, 2),
            Literal::Integer(Token::new(0i64, Span::new(b"0")))
        );

        assert_eq!(binary(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_not_starting_by_zero_b() {
        let input  = Span::new(b"1");
        let output = Result::Done(
            Span::new_at(b"", 1, 1, 2),
            Literal::Integer(Token::new(1i64, input))
        );

        assert_eq!(binary(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_not_in_base() {
        let input  = Span::new(b"0b120");
        let output = Result::Done(
            Span::new_at(b"20", 3, 1, 4),
            Literal::Integer(Token::new(1i64, Span::new(b"0b1")))
        );

        assert_eq!(binary(input),  output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal() {
        let input  = Span::new(b"052");
        let output = Result::Done(
            Span::new_at(b"", 3, 1, 4),
            Literal::Integer(Token::new(42i64, input))
        );

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal_zero() {
        let input  = Span::new(b"0");
        let output = Result::Done(
            Span::new_at(b"", 1, 1, 2),
            Literal::Integer(Token::new(0i64, input))
        );

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal_maximum_integer_value() {
        let input  = Span::new(b"0777777777777777777777");
        let output = Result::Done(
            Span::new_at(b"", 22, 1, 23),
            Literal::Integer(Token::new(::std::i64::MAX, input))
        );

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_overflow() {
        let input  = Span::new(b"01000000000000000000000");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(octal(input), Result::Error(Error::Position(ErrorKind::MapRes, input)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_not_starting_by_zero() {
        let input  = Span::new(b"7");
        let output = Result::Done(
            Span::new_at(b"", 1, 1, 2),
            Literal::Integer(Token::new(7i64, input))
        );

        assert_eq!(octal(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_not_in_base() {
        let input  = Span::new(b"8");
        let output = Result::Done(
            Span::new_at(b"", 1, 1, 2),
            Literal::Integer(Token::new(8i64, input))
        );

        assert_eq!(octal(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_one_digit() {
        let input  = Span::new(b"7");
        let output = Result::Done(
            Span::new_at(b"", 1, 1, 2),
            Literal::Integer(Token::new(7i64, input))
        );

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_many_digits() {
        let input  = Span::new(b"42");
        let output = Result::Done(
            Span::new_at(b"", 2, 1, 3),
            Literal::Integer(Token::new(42i64, input))
        );

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    quickcheck! {
        fn case_decimal_random(input: u32) -> bool {
            let input  = input * 2 + 1;
            let string = input.to_string();

            match decimal(Span::new(string.as_bytes())) {
                Result::Done(_, Literal::Integer(Token { value: output, .. })) => {
                    input == output as u32
                },

                _ => {
                    false
                }
            }
        }
    }

    #[test]
    fn case_decimal_plus() {
        let input  = Span::new(b"42+");
        let output = Result::Done(
            Span::new_at(b"+", 2, 1, 3),
            Literal::Integer(Token::new(42i64, Span::new(b"42")))
        );

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_maximum_integer_value() {
        let input  = Span::new(b"9223372036854775807");
        let output = Result::Done(
            Span::new_at(b"", 19, 1, 20),
            Literal::Integer(Token::new(::std::i64::MAX, input))
        );

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_overflow_to_real() {
        let input  = Span::new(b"9223372036854775808");
        let output = Result::Done(
            Span::new_at(b"", 19, 1, 20),
            Literal::Real(Token::new(9223372036854775808f64, input))
        );

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_maximum_real_value() {
        let input  = Span::new(b"179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        let output = Result::Done(
            Span::new_at(b"", 309, 1, 310),
            Literal::Real(Token::new(::std::f64::MAX, input))
        );

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_decimal_overflow_to_infinity() {
        let input  = Span::new(b"1797693134862315700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        let output = Result::Done(
            Span::new_at(b"", 310, 1, 311),
            Literal::Real(Token::new(::std::f64::INFINITY, input))
        );

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_lowercase_x() {
        let input  = Span::new(b"0x2a");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Integer(Token::new(42i64, input))
        );

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_uppercase_x() {
        let input  = Span::new(b"0X2a");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Integer(Token::new(42i64, input))
        );

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_uppercase_alpha() {
        let input  = Span::new(b"0x2A");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Integer(Token::new(42i64, input))
        );

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_no_number() {
        let input  = Span::new(b"0x");
        let output = Result::Done(
            Span::new_at(b"x", 1, 1, 2),
            Literal::Integer(Token::new(0i64, Span::new(b"0")))
        );

        assert_eq!(hexadecimal(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_not_in_base() {
        let input  = Span::new(b"0xg");
        let output = Result::Done(
            Span::new_at(b"xg", 1, 1, 2),
            Literal::Integer(Token::new(0i64, Span::new(b"0")))
        );

        assert_eq!(hexadecimal(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_maximum_integer_value() {
        let input  = Span::new(b"0x7fffffffffffffff");
        let output = Result::Done(
            Span::new_at(b"", 18, 1, 19),
            Literal::Integer(Token::new(::std::i64::MAX, input))
        );

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_overflow() {
        let input  = Span::new(b"0x8000000000000000");
        let output = Result::Done(
            Span::new_at(b"x8000000000000000", 1, 1, 2),
            Literal::Integer(Token::new(0i64, Span::new(b"0")))
        );

        assert_eq!(hexadecimal(input), Result::Error(Error::Position(ErrorKind::MapRes, Span::new(b"0x8000000000000000"))));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential() {
        let input  = Span::new(b"123.456e+78");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Literal::Real(Token::new(123.456e78f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_fractional_part() {
        let input  = Span::new(b"123.456");
        let output = Result::Done(
            Span::new_at(b"", 7, 1, 8),
            Literal::Real(Token::new(123.456f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_part() {
        let input  = Span::new(b"123.");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Real(Token::new(123.0f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_fractional_part() {
        let input  = Span::new(b".456");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Literal::Real(Token::new(0.456f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_exponent_part_with_lowercase_e() {
        let input  = Span::new(b"123.e78");
        let output = Result::Done(
            Span::new_at(b"", 7, 1, 8),
            Literal::Real(Token::new(123e78f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_integer_rational_and_exponent_part() {
        let input  = Span::new(b"123e78");
        let output = Result::Done(
            Span::new_at(b"", 6, 1, 7),
            Literal::Real(Token::new(123e78f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_exponent_part_with_uppercase_e() {
        let input  = Span::new(b"123.E78");
        let output = Result::Done(
            Span::new_at(b"", 7, 1, 8),
            Literal::Real(Token::new(123e78f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_unsigned_exponent_part() {
        let input  = Span::new(b"123.e78");
        let output = Result::Done(
            Span::new_at(b"", 7, 1, 8),
            Literal::Real(Token::new(123e78f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_positive_exponent_part() {
        let input  = Span::new(b"123.e+78");
        let output = Result::Done(
            Span::new_at(b"", 8, 1, 9),
            Literal::Real(Token::new(123e78f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_negative_exponent_part() {
        let input  = Span::new(b"123.e-78");
        let output = Result::Done(
            Span::new_at(b"", 8, 1, 9),
            Literal::Real(Token::new(123e-78f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_negative_zero_exponent_part() {
        let input  = Span::new(b"123.e-0");
        let output = Result::Done(
            Span::new_at(b"", 7, 1, 8),
            Literal::Real(Token::new(123f64, input))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_missing_exponent_part() {
        let input  = Span::new(b".7e");
        let output = Result::Done(
            Span::new_at(b"e", 2, 1, 3),
            Literal::Real(Token::new(0.7f64, Span::new(b".7")))
        );

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_exponential_only_the_dot() {
        let input = Span::new(b".");

        assert_eq!(exponential(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(literal(input), Result::Error(Error::Position(ErrorKind::Alt, Span::new(b"."))));
    }

    #[test]
    fn case_string_single_quoted() {
        let input  = Span::new(b"'foobar'tail");
        let output = Result::Done(
            Span::new_at(b"tail", 8, 1, 9),
            Literal::String(
                Token::new(
                    b"foobar".to_vec(),
                    Span::new(b"'foobar'")
                )
            )
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_quote() {
        let input  = Span::new(b"'foo\\'bar'tail");
        let output = Result::Done(
            Span::new_at(b"tail", 10, 1, 11),
            Literal::String(
                Token::new(
                    b"foo'bar".to_vec(),
                    Span::new(b"'foo\\'bar'")
                )
            )
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_backslash() {
        let input  = Span::new(b"'foo\\\\bar'tail");
        let output = Result::Done(
            Span::new_at(b"tail", 10, 1, 11),
            Literal::String(
                Token::new(
                    b"foo\\bar".to_vec(),
                    Span::new(b"'foo\\\\bar'")
                )
            )
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_any() {
        let input  = Span::new(b"'foo\\nbar'tail");
        let output = Result::Done(
            Span::new_at(b"tail", 10, 1, 11),
            Literal::String(
                Token::new(
                    b"foo\\nbar".to_vec(),
                    Span::new(b"'foo\\nbar'")
                )
            )
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_many() {
        let input  = Span::new(b"'\\'f\\oo\\\\bar\\\\'tail");
        let output = Result::Done(
            Span::new_at(b"tail", 15, 1, 16),
            Literal::String(
                Token::new(
                    b"'f\\oo\\bar\\".to_vec(),
                    Span::new(b"'\\'f\\oo\\\\bar\\\\'")
                )
            )
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_empty() {
        let input  = Span::new(b"''tail");
        let output = Result::Done(
            Span::new_at(b"tail", 2, 1, 3),
            Literal::String(
                Token::new(
                    Vec::new(),
                    Span::new(b"''")
                )
            )
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_single_quoted() {
        let input  = Span::new(b"b'foobar'");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Literal::String(Token::new(b"foobar".to_vec(), input.slice(1..)))
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_uppercase_single_quoted() {
        let input  = Span::new(b"B'foobar'");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Literal::String(Token::new(b"foobar".to_vec(), input.slice(1..)))
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_single_quoted_escaped_many() {
        let input  = Span::new(b"b'\\'f\\oo\\\\bar'");
        let output = Result::Done(
            Span::new_at(b"", 14, 1, 15),
            Literal::String(Token::new(b"'f\\oo\\bar".to_vec(), input.slice(1..)))
        );

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_too_short() {
        let input  = Span::new(b"'");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_opening_character() {
        let input  = Span::new(b"foobar'");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_closing_character() {
        let input  = Span::new(b"'foobar");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_closing_character_is_a_backslash() {
        let input  = Span::new(b"'foobar\\");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_single_quoted_too_short() {
        let input  = Span::new(b"b'");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_single_quoted_too_short() {
        let input  = Span::new(b"B'");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_single_quoted_opening_character() {
        let input  = Span::new(b"bb'");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc() {
        let input  = Span::new(b"<<<'FOO'\nhello \n  world \nFOO;\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 30, 5, 1),
            Literal::String(
                Token::new(
                    b"hello \n  world ".to_vec(),
                    Span::new(b"<<<'FOO'\nhello \n  world \nFOO;\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_crlf() {
        let input  = Span::new(b"<<<'FOO'\r\nhello \r\n  world \r\nFOO;\r\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 34, 5, 1),
            Literal::String(
                Token::new(
                    b"hello \r\n  world ".to_vec(),
                    Span::new(b"<<<'FOO'\r\nhello \r\n  world \r\nFOO;\r\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_without_semi_colon() {
        let input  = Span::new(b"<<<'FOO'\nhello \n  world \nFOO\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 29, 5, 1),
            Literal::String(
                Token::new(
                    b"hello \n  world ".to_vec(),
                    Span::new(b"<<<'FOO'\nhello \n  world \nFOO\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_without_semi_colon_crlf() {
        let input  = Span::new(b"<<<'FOO'\r\nhello \r\n  world \r\nFOO\r\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 33, 5, 1),
            Literal::String(
                Token::new(
                    b"hello \r\n  world ".to_vec(),
                    Span::new(b"<<<'FOO'\r\nhello \r\n  world \r\nFOO\r\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_empty() {
        let input  = Span::new(b"<<<'FOO'\nFOO\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 13, 3, 1),
            Literal::String(
                Token::new(
                    Vec::new(),
                    Span::new(b"<<<'FOO'\nFOO\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_empty_crlf() {
        let input  = Span::new(b"<<<'FOO'\r\nFOO\r\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 15, 3, 1),
            Literal::String(
                Token::new(
                    Vec::new(),
                    Span::new(b"<<<'FOO'\r\nFOO\r\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_with_whitespaces_before_identifier() {
        let input  = Span::new(b"<<<   \t  'FOO'\nhello \n  world \nFOO\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 35, 5, 1),
            Literal::String(
                Token::new(
                    b"hello \n  world ".to_vec(),
                    Span::new(b"<<<   \t  'FOO'\nhello \n  world \nFOO\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_with_whitespaces_before_identifier_crlf() {
        let input  = Span::new(b"<<<   \t  'FOO'\r\nhello \r\n  world \r\nFOO\r\ntail");
        let output = Result::Done(
            Span::new_at(b"tail", 39, 5, 1),
            Literal::String(
                Token::new(
                    b"hello \r\n  world ".to_vec(),
                    Span::new(b"<<<   \t  'FOO'\r\nhello \r\n  world \r\nFOO\r\n")
                )
            )
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_nowdoc() {
        let input  = Span::new(b"b<<<'FOO'\nhello \n  world \nFOO\n");
        let output = Result::Done(
            Span::new_at(b"", 30, 5, 1),
            Literal::String(Token::new(b"hello \n  world ".to_vec(), input.slice(1..)))
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_nowdoc_crlf() {
        let input  = Span::new(b"b<<<'FOO'\r\nhello \r\n  world \r\nFOO\r\n");
        let output = Result::Done(
            Span::new_at(b"", 34, 5, 1),
            Literal::String(Token::new(b"hello \r\n  world ".to_vec(), input.slice(1..)))
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_uppercase_nowdoc() {
        let input  = Span::new(b"B<<<'FOO'\nhello \n  world \nFOO\n");
        let output = Result::Done(
            Span::new_at(b"", 30, 5, 1),
            Literal::String(Token::new(b"hello \n  world ".to_vec(), input.slice(1..)))
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_uppercase_nowdoc_crlf() {
        let input  = Span::new(b"B<<<'FOO'\r\nhello \r\n  world \r\nFOO\r\n");
        let output = Result::Done(
            Span::new_at(b"", 34, 5, 1),
            Literal::String(Token::new(b"hello \r\n  world ".to_vec(), input.slice(1..)))
        );

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_too_short() {
        let input  = Span::new(b"<<<'A'\nA");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character() {
        let input  = Span::new(b"<<FOO'\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_first_quote() {
        let input  = Span::new(b"<<<FOO'\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_second_quote() {
        let input  = Span::new(b"<<<'FOO\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_invalid_identifier() {
        let input  = Span::new(b"<<<'42'\nhello \n  world \n42\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidDelimiterIdentifier as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_partially_invalid_identifier() {
        let input  = Span::new(b"<<<'F O O'\nhello \n  world \nF O O\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_newline() {
        let input  = Span::new(b"<<<'FOO'hello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character() {
        let input  = Span::new(b"<<<'FOO'\nhello \n  world \nFO;\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_semi_colon_no_newline() {
        let input  = Span::new(b"<<<'FOO'\nhello \n  world \nFOO");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_semi_colon_no_newline_crlf() {
        let input  = Span::new(b"<<<'FOO'\r\nhello \r\n  world \r\nFOO");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_newline() {
        let input  = Span::new(b"<<<'FOO'\nhello \n  world \nFOO;");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_newline_crlf() {
        let input  = Span::new(b"<<<'FOO'\r\nhello \r\n  world \r\nFOO;");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_missing_lf_in_crlf() {
        let input  = Span::new(b"<<<'FOO'\r\nhello \r\n  world \r\nFOO\r");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_too_short() {
        let input  = Span::new(b"b<<<'A'\nA");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_opening_character() {
        let input  = Span::new(b"b<<FOO'\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_opening_character_missing_first_quote() {
        let input  = Span::new(b"b<<<FOO'\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_opening_character_missing_second_quote() {
        let input  = Span::new(b"b<<<'FOO\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_too_short() {
        let input  = Span::new(b"B<<<'A'\nA");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_opening_character() {
        let input  = Span::new(b"B<<FOO'\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_opening_character_missing_first_quote() {
        let input  = Span::new(b"B<<<FOO'\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_opening_character_missing_second_quote() {
        let input  = Span::new(b"B<<<'FOO\nhello \n  world \nFOO\n");
        let output = Result::Error(Error::Position(ErrorKind::Alt, input));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }
}
