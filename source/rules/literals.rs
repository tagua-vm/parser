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

//! Group of literal rules.
//!
//! The list of all literals is provided by the PHP Language Specification in
//! the [Grammar chapter, Literals
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#literals).

use nom::{
    hex_digit,
    oct_digit
};
use std::num::ParseIntError;
use std::result::Result as StdResult;
use std::str::FromStr;
use std::str;
use super::super::ast::Literal;
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

        # fn main () {
        assert_eq!(literal(b\"0x2a\"), Result::Done(&b\"\"[..], Literal::Integer(42i64)));
        # }
        ```
    "],
    pub literal<Literal>,
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

        # fn main () {
        assert_eq!(null(b\"null\"), Result::Done(&b\"\"[..], Literal::Null));
        # }
        ```
    "],
    pub null<Literal>,
    map_res!(
        itag!("null"),
        |_| -> StdResult<Literal, ()> {
            Ok(Literal::Null)
        }
    )
);

named!(
    pub boolean<Literal>,
    map_res!(
        alt!(itag!(&b"true"[..]) | itag!(&b"false"[..])),
        |bytes: &[u8]| -> StdResult<Literal, ()> {
            Ok(Literal::Boolean(bytes[0] == 't' as u8))
        }
    )
);

named!(
    pub integer<Literal>,
    alt_complete!(
        binary
      | decimal
      | hexadecimal
      | octal
    )
);

named!(
    pub binary<Literal>,
    map_res!(
        preceded!(
            tag!("0"),
            preceded!(
                is_a!("bB"),
                is_a!("01")
            )
        ),
        |bytes: &[u8]| {
            i64
                ::from_str_radix(
                    unsafe { str::from_utf8_unchecked(bytes) },
                    2
                )
                .and_then(
                    |binary| {
                        Ok(Literal::Integer(binary))
                   }
                )
        }
    )
);

named!(
    pub octal<Literal>,
    map_res!(
        preceded!(tag!("0"), opt!(complete!(oct_digit))),
        |value: Option<&[u8]>| {
            match value {
                Some(bytes) =>
                    i64
                        ::from_str_radix(
                            unsafe { str::from_utf8_unchecked(bytes) },
                            8
                        )
                        .and_then(
                            |octal| {
                                Ok(Literal::Integer(octal))
                            }
                        ),

                None =>
                    Ok(Literal::Integer(0i64))
            }
        }
    )
);

named!(
    pub decimal<Literal>,
    map_res!(
        re_bytes_find_static!(r"^[1-9][0-9]*"),
        |bytes: &[u8]| {
            let string = unsafe { str::from_utf8_unchecked(bytes) };

            i64
                ::from_str(string)
                .and_then(
                    |decimal| {
                        Ok(Literal::Integer(decimal))
                   }
                )
                .or_else(
                    |_: ParseIntError| {
                        f64
                            ::from_str(string)
                            .and_then(
                                |decimal| {
                                    Ok(Literal::Real(decimal))
                                }
                            )
                    }
                )
        }
    )
);

named!(
    pub hexadecimal<Literal>,
    map_res!(
        preceded!(
            tag!("0"),
            preceded!(
                is_a!("xX"),
                complete!(hex_digit)
            )
        ),
        |bytes: &[u8]| {
            i64
                ::from_str_radix(
                    unsafe { str::from_utf8_unchecked(bytes) },
                    16
                )
                .and_then(
                    |hexadecimal| {
                        Ok(Literal::Integer(hexadecimal))
                    }
                )
        }
    )
);

named!(
    pub exponential<Literal>,
    map_res!(
        re_bytes_find_static!(r"^(([0-9]*\.[0-9]+|[0-9]+\.)([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)"),
        |bytes: &[u8]| {
            f64
                ::from_str(unsafe { str::from_utf8_unchecked(bytes) })
                .and_then(
                    |exponential| {
                        Ok(Literal::Real(exponential))
                    }
                )
        }
    )
);

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

named!(
    pub string<Literal>,
    alt!(
        string_single_quoted
      | string_nowdoc
    )
);

fn string_single_quoted(input: &[u8]) -> Result<&[u8], Literal> {
    let input_length = input.len();

    if input_length < 2 {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
    }

    if input[0] == 'b' as u8 || input[0] == 'B' as u8 {
        if input_length < 3 {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
        } else if input[1] != '\'' as u8 {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
        } else {
            return string_single_quoted(&input[1..]);
        }
    } else if input[0] != '\'' as u8 {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    let mut output   = Vec::new();
    let mut offset   = 1;
    let mut iterator = input[offset..].iter().enumerate();

    while let Some((index, item)) = iterator.next() {
        if *item == '\\' as u8 {
            if let Some((next_index, next_item)) = iterator.next() {
                if *next_item == '\'' as u8 ||
                   *next_item == '\\' as u8 {
                    output.extend(&input[offset..index + 1]);
                    offset = next_index + 1;
                }
            } else {
                return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
            }
        } else if *item == '\'' as u8 {
            output.extend(&input[offset..index + 1]);

            return Result::Done(&input[index + 2..], Literal::String(output));
        }
    }

    Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)))
}

const STRING_NOWDOC_OPENING: &'static [u8] = &['<' as u8, '<' as u8, '<' as u8];

fn string_nowdoc(input: &[u8]) -> Result<&[u8], Literal> {
    let input_length = input.len();

    // `<<<'A'\nA\n` is the shortest datum.
    if input_length < 9 {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
    }

    if input[0] == 'b' as u8 || input[0] == 'B' as u8 {
        if input_length < 10 {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32)));
        } else if false == input[1..].starts_with(STRING_NOWDOC_OPENING) {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
        } else {
            return string_nowdoc(&input[1..]);
        }
    } else if false == input.starts_with(STRING_NOWDOC_OPENING) {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    let mut offset = 3;

    for item in input[offset..].iter() {
        if *item != ' ' as u8 && *item != '\t' as u8 {
            break;
        }

        offset += 1;
    }

    if input[offset] != '\'' as u8 {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    offset += 1;

    let name;
    let next_input;

    if let Result::Done(i, n) = tokens::name(&input[offset..]) {
        name       = n;
        next_input = i;
    } else {
        return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidDelimiterIdentifier as u32)))
    }

    let next_input_length = next_input.len();
    let name_length       = name.len();

    if next_input_length < 3 + name_length || next_input[0] != '\'' as u8 || next_input[1] != '\n' as u8 {
        if next_input[1] != '\r' as u8 || next_input[2] != '\n' as u8 {
            return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
        }
    }

    if next_input[1] == '\r' as u8 {
        offset = 2;
    } else {
        offset = 1;
    }

    for (index, item) in next_input[offset..].iter().enumerate() {
        if *item == '\n' as u8 {
            if !next_input[offset + index + 1..].starts_with(name) {
                continue;
            }

            let mut lookahead_offset = offset + index + name_length + 1;

            if lookahead_offset >= next_input_length {
                return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
            }

            if next_input[lookahead_offset] == ';' as u8 {
                lookahead_offset += 1;
            }

            if lookahead_offset >= next_input_length {
                return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
            }

            let mut ending_offset = 0;

            if next_input[lookahead_offset] == '\r' as u8 {
                if lookahead_offset + 1 >= next_input_length {
                    return Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
                }

                ending_offset     = 1;
                lookahead_offset += 1;
            }

            if next_input[lookahead_offset] == '\n' as u8 {
                if index == 0 {
                    return Result::Done(
                        &next_input[lookahead_offset + 1..],
                        Literal::String(Vec::new())
                    );
                }

                return Result::Done(
                    &next_input[lookahead_offset + 1..],
                    Literal::String(next_input[offset + 1..offset - ending_offset + index].to_vec())
                );
            }
        }
    }

    Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)))
}


#[cfg(test)]
mod tests {
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

    #[test]
    fn case_null() {
        let input  = b"null";
        let output = Result::Done(&b""[..], Literal::Null);

        assert_eq!(null(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_null_case_insensitive() {
        let input  = b"NuLl";
        let output = Result::Done(&b""[..], Literal::Null);

        assert_eq!(null(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_true() {
        let input  = b"true";
        let output = Result::Done(&b""[..], Literal::Boolean(true));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_true_case_insensitive() {
        let input  = b"TrUe";
        let output = Result::Done(&b""[..], Literal::Boolean(true));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_false() {
        let input  = b"false";
        let output = Result::Done(&b""[..], Literal::Boolean(false));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_false_case_insensitive() {
        let input  = b"FaLsE";
        let output = Result::Done(&b""[..], Literal::Boolean(false));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_lowercase_b() {
        let input  = b"0b101010";
        let output = Result::Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_uppercase_b() {
        let input  = b"0B101010";
        let output = Result::Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_maximum_integer_value() {
        let input  = b"0b111111111111111111111111111111111111111111111111111111111111111";
        let output = Result::Done(&b""[..], Literal::Integer(::std::i64::MAX));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_overflow() {
        let input  = b"0b1000000000000000000000000000000000000000000000000000000000000000";
        let output = Result::Done(&b"b1000000000000000000000000000000000000000000000000000000000000000"[..], Literal::Integer(0i64));

        assert_eq!(binary(input), Result::Error(Error::Position(ErrorKind::MapRes, &input[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_no_number() {
        let input  = b"0b";
        let output = Result::Done(&b"b"[..], Literal::Integer(0i64));

        assert_eq!(binary(input), Result::Error(Error::Position(ErrorKind::MapRes, &b"0b"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_not_starting_by_zero_b() {
        let input  = b"1";
        let output = Result::Done(&b""[..], Literal::Integer(1i64));

        assert_eq!(binary(input), Result::Error(Error::Position(ErrorKind::Tag, &b"1"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_not_in_base() {
        let input  = b"0b120";
        let output = Result::Done(&b"20"[..], Literal::Integer(1i64));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal() {
        let input  = b"052";
        let output = Result::Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal_zero() {
        let input  = b"0";
        let output = Result::Done(&b""[..], Literal::Integer(0i64));

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal_maximum_integer_value() {
        let input  = b"0777777777777777777777";
        let output = Result::Done(&b""[..], Literal::Integer(::std::i64::MAX));

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_overflow() {
        let input  = b"01000000000000000000000";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"01000000000000000000000"[..]));

        assert_eq!(octal(input), Result::Error(Error::Position(ErrorKind::MapRes, &b"01000000000000000000000"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_not_starting_by_zero() {
        let input  = b"7";
        let output = Result::Done(&b""[..], Literal::Integer(7i64));

        assert_eq!(octal(input), Result::Error(Error::Position(ErrorKind::Tag, &b"7"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_not_in_base() {
        let input  = b"8";
        let output = Result::Done(&b""[..], Literal::Integer(8));

        assert_eq!(octal(input), Result::Error(Error::Position(ErrorKind::Tag, &b"8"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_one_digit() {
        let input  = b"7";
        let output = Result::Done(&b""[..], Literal::Integer(7i64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_many_digits() {
        let input  = b"42";
        let output = Result::Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    quickcheck! {
        fn case_decimal_random(input: u32) -> bool {
            let input  = input * 2 + 1;
            let string = input.to_string();
            let bytes  = string.as_bytes();

            match decimal(bytes) {
                Result::Done(_, Literal::Integer(output)) => {
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
        let input  = b"42+";
        let output = Result::Done(&b"+"[..], Literal::Integer(42i64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_maximum_integer_value() {
        let input  = b"9223372036854775807";
        let output = Result::Done(&b""[..], Literal::Integer(::std::i64::MAX));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_overflow_to_real() {
        let input  = b"9223372036854775808";
        let output = Result::Done(&b""[..], Literal::Real(9223372036854775808f64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_maximum_real_value() {
        let input  = b"179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        let output = Result::Done(&b""[..], Literal::Real(::std::f64::MAX));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_decimal_overflow_to_infinity() {
        let input  = b"1797693134862315700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        let output = Result::Done(&b""[..], Literal::Real(::std::f64::INFINITY));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_lowercase_x() {
        let input  = b"0x2a";
        let output = Result::Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_uppercase_x() {
        let input  = b"0X2a";
        let output = Result::Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_uppercase_alpha() {
        let input  = b"0x2A";
        let output = Result::Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_no_number() {
        let input  = b"0x";
        let output = Result::Done(&b"x"[..], Literal::Integer(0i64));

        assert_eq!(hexadecimal(input), Result::Error(Error::Position(ErrorKind::Complete, &b""[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_not_in_base() {
        let input  = b"0xg";
        let output = Result::Done(&b"xg"[..], Literal::Integer(0i64));

        assert_eq!(hexadecimal(input), Result::Error(Error::Position(ErrorKind::HexDigit, &b"g"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_maximum_integer_value() {
        let input  = b"0x7fffffffffffffff";
        let output = Result::Done(&b""[..], Literal::Integer(::std::i64::MAX));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_overflow() {
        let input  = b"0x8000000000000000";
        let output = Result::Done(&b"x8000000000000000"[..], Literal::Integer(0i64));

        assert_eq!(hexadecimal(input), Result::Error(Error::Position(ErrorKind::MapRes, &b"0x8000000000000000"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential() {
        let input  = b"123.456e+78";
        let output = Result::Done(&b""[..], Literal::Real(123.456e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_fractional_part() {
        let input  = b"123.456";
        let output = Result::Done(&b""[..], Literal::Real(123.456f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_part() {
        let input  = b"123.";
        let output = Result::Done(&b""[..], Literal::Real(123.0f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_fractional_part() {
        let input  = b".456";
        let output = Result::Done(&b""[..], Literal::Real(0.456f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_exponent_part_with_lowercase_e() {
        let input  = b"123.e78";
        let output = Result::Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_integer_rational_and_exponent_part() {
        let input  = b"123e78";
        let output = Result::Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_exponent_part_with_uppercase_e() {
        let input  = b"123.E78";
        let output = Result::Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_unsigned_exponent_part() {
        let input  = b"123.e78";
        let output = Result::Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_positive_exponent_part() {
        let input  = b"123.e+78";
        let output = Result::Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_negative_exponent_part() {
        let input  = b"123.e-78";
        let output = Result::Done(&b""[..], Literal::Real(123e-78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_negative_zero_exponent_part() {
        let input  = b"123.e-0";
        let output = Result::Done(&b""[..], Literal::Real(123f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_missing_exponent_part() {
        let input  = b".7e";
        let output = Result::Done(&b"e"[..], Literal::Real(0.7f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_exponential_only_the_dot() {
        let input = b".";

        assert_eq!(exponential(input), Result::Error(Error::Code(ErrorKind::RegexpFind)));
        assert_eq!(literal(input), Result::Error(Error::Position(ErrorKind::Alt, &b"."[..])));
    }

    #[test]
    fn case_string_single_quoted() {
        let input  = b"'foobar'";
        let output = Result::Done(&b""[..], Literal::String(b"foobar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_quote() {
        let input  = b"'foo\\'bar'";
        let output = Result::Done(&b""[..], Literal::String(b"foo'bar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_backslash() {
        let input  = b"'foo\\\\bar'";
        let output = Result::Done(&b""[..], Literal::String(b"foo\\bar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_any() {
        let input  = b"'foo\\nbar'";
        let output = Result::Done(&b""[..], Literal::String(b"foo\\nbar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_many() {
        let input  = b"'\\'f\\oo\\\\bar\\\\'";
        let output = Result::Done(&b""[..], Literal::String(b"'f\\oo\\bar\\".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_empty() {
        let input  = b"''";
        let output = Result::Done(&b""[..], Literal::String(Vec::new()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_single_quoted() {
        let input  = b"b'foobar'";
        let output = Result::Done(&b""[..], Literal::String(b"foobar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_uppercase_single_quoted() {
        let input  = b"B'foobar'";
        let output = Result::Done(&b""[..], Literal::String(b"foobar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_single_quoted_escaped_many() {
        let input  = b"b'\\'f\\oo\\\\bar'";
        let output = Result::Done(&b""[..], Literal::String(b"'f\\oo\\bar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_too_short() {
        let input  = b"'";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_opening_character() {
        let input  = b"foobar'";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_closing_character() {
        let input  = b"'foobar";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_closing_character_is_a_backslash() {
        let input  = b"'foobar\\";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_single_quoted_too_short() {
        let input  = b"b'";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_single_quoted_too_short() {
        let input  = b"B'";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_single_quoted_opening_character() {
        let input  = b"bb'";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc() {
        let input  = b"<<<'FOO'\nhello \n  world \nFOO;\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_crlf() {
        let input  = b"<<<'FOO'\r\nhello \r\n  world \r\nFOO;\r\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \r\n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_without_semi_colon() {
        let input  = b"<<<'FOO'\nhello \n  world \nFOO\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_without_semi_colon_crlf() {
        let input  = b"<<<'FOO'\r\nhello \r\n  world \r\nFOO\r\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \r\n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_empty() {
        let input  = b"<<<'FOO'\nFOO\n";
        let output = Result::Done(&b""[..], Literal::String(Vec::new()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_empty_crlf() {
        let input  = b"<<<'FOO'\r\nFOO\r\n";
        let output = Result::Done(&b""[..], Literal::String(Vec::new()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_with_whitespaces_before_identifier() {
        let input  = b"<<<   \t  'FOO'\nhello \n  world \nFOO\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_with_whitespaces_before_identifier_crlf() {
        let input  = b"<<<   \t  'FOO'\r\nhello \r\n  world \r\nFOO\r\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \r\n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_nowdoc() {
        let input  = b"b<<<'FOO'\nhello \n  world \nFOO\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_nowdoc_crlf() {
        let input  = b"b<<<'FOO'\r\nhello \r\n  world \r\nFOO\r\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \r\n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_uppercase_nowdoc() {
        let input  = b"B<<<'FOO'\nhello \n  world \nFOO\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_uppercase_nowdoc_crlf() {
        let input  = b"B<<<'FOO'\r\nhello \r\n  world \r\nFOO\r\n";
        let output = Result::Done(&b""[..], Literal::String(b"hello \r\n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_too_short() {
        let input  = b"<<<'A'\nA";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character() {
        let input  = b"<<FOO'\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_first_quote() {
        let input  = b"<<<FOO'\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_second_quote() {
        let input  = b"<<<'FOO\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_invalid_identifier() {
        let input  = b"<<<'42'\nhello \n  world \n42\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidDelimiterIdentifier as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_partially_invalid_identifier() {
        let input  = b"<<<'F O O'\nhello \n  world \nF O O\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_newline() {
        let input  = b"<<<'FOO'hello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character() {
        let input  = b"<<<'FOO'\nhello \n  world \nFO;\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_semi_colon_no_newline() {
        let input  = b"<<<'FOO'\nhello \n  world \nFOO";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_semi_colon_no_newline_crlf() {
        let input  = b"<<<'FOO'\r\nhello \r\n  world \r\nFOO";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_newline() {
        let input  = b"<<<'FOO'\nhello \n  world \nFOO;";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_no_newline_crlf() {
        let input  = b"<<<'FOO'\r\nhello \r\n  world \r\nFOO;";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character_missing_lf_in_crlf() {
        let input  = b"<<<'FOO'\r\nhello \r\n  world \r\nFOO\r";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_too_short() {
        let input  = b"b<<<'A'\nA";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_opening_character() {
        let input  = b"b<<FOO'\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_opening_character_missing_first_quote() {
        let input  = b"b<<<FOO'\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_nowdoc_opening_character_missing_second_quote() {
        let input  = b"b<<<'FOO\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_too_short() {
        let input  = b"B<<<'A'\nA";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_opening_character() {
        let input  = b"B<<FOO'\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_opening_character_missing_first_quote() {
        let input  = b"B<<<FOO'\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_uppercase_nowdoc_opening_character_missing_second_quote() {
        let input  = b"B<<<'FOO\nhello \n  world \nFOO\n";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Result::Error(Error::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }
}
