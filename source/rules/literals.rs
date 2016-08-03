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
    Err,
    ErrorKind,
    IResult,
    hex_digit,
    oct_digit
};
use std::num::ParseIntError;
use std::str::FromStr;
use std::str;
use super::super::ast::Literal;

named!(
    pub literal<Literal>,
    alt!(
        null
      | boolean
      | exponential
      | integer
      | string
    )
);

named!(
    pub null<Literal>,
    map_res!(
        itag!("null"),
        |_| -> Result<Literal, ()> {
            Ok(Literal::Null)
        }
    )
);

named!(
    pub boolean<Literal>,
    map_res!(
        alt!(itag!("true".as_bytes()) | itag!("false".as_bytes())),
        |bytes: &[u8]| -> Result<Literal, ()> {
            Ok(Literal::Boolean(bytes[0] == 't' as u8))
        }
    )
);

named!(
    pub integer<Literal>,
    alt!(
        binary
      | octal
      | decimal
      | hexadecimal
    )
);

named!(
    pub binary<Literal>,
    map_res!(
        preceded!(
            tag!("0"),
            preceded!(
                alt!(tag!("b") | tag!("B")),
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
        preceded!(tag!("0"), oct_digit),
        |bytes: &[u8]| {
            i64
                ::from_str_radix(
                    unsafe { str::from_utf8_unchecked(bytes) },
                    8
                )
                .and_then(
                    |octal| {
                        Ok(Literal::Integer(octal))
                    }
                )
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
                alt!(tag!("x") | tag!("X")),
                hex_digit
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
#[derive(Debug)]
pub enum StringError {
    /// The datum starts as a string but is too short to be a string.
    TooShort,
    /// The string open character is not correct.
    InvalidOpeningCharacter,
    /// The string close character is not correct.
    InvalidClosingCharacter,
    /// The string is not correctly encoded (expect UTF-8).
    InvalidEncoding
}

named!(
    pub string<Literal>,
    alt!(
        string_single_quoted
      | string_nowdoc
    )
);

fn string_single_quoted(input: &[u8]) -> IResult<&[u8], Literal> {
    let input_length = input.len();

    if input_length < 2 {
        return IResult::Error(Err::Code(ErrorKind::Custom(StringError::TooShort as u32)));
    }

    if input[0] == 'b' as u8 {
        if input_length < 3 {
            return IResult::Error(Err::Code(ErrorKind::Custom(StringError::TooShort as u32)));
        } else if input[1] != '\'' as u8 {
            return IResult::Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
        } else {
            return string_single_quoted(&input[1..]);
        }
    } else if input[0] != '\'' as u8 {
        return IResult::Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
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
                return IResult::Error(Err::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)));
            }
        } else if *item == '\'' as u8 {
            output.extend(&input[offset..index + 1]);

            return IResult::Done(&input[index + 2..], Literal::String(output));
        }
    }

    IResult::Error(Err::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)))
}

fn string_nowdoc(input: &[u8]) -> IResult<&[u8], Literal> {
    // `<<<'A'\nA\n` is the shortest datum.
    if input.len() < 9 {
        return IResult::Error(Err::Code(ErrorKind::Custom(StringError::TooShort as u32)));
    }

    if false == input.starts_with(&['<' as u8, '<' as u8, '<' as u8, '\'' as u8]) {
        return IResult::Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    let padding      = 4;
    let mut offset   = padding;
    let mut iterator = input[offset..].iter().enumerate();

    while let Some((index, item)) = iterator.next() {
        if *item == '\'' as u8 {
            offset += index;

            break;
        }
    }

    if input[offset] != '\'' as u8 || input[offset + 1] != '\n' as u8 {
        return IResult::Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32)));
    }

    let name       = &input[padding..offset];
    let mut output = Vec::new();

    iterator.next();

    while let Some((index, item)) = iterator.next() {
        if *item == '\n' as u8 {
            if !input[padding + index + 1..].starts_with(name) {
                continue;
            }

            offset                   = padding + index;
            let mut lookahead_offset = offset + name.len() + 1;

            if input[lookahead_offset] == ';' as u8 {
                lookahead_offset += 1;
            }

            if input[lookahead_offset] == '\n' as u8 {
                output.extend(&input[padding + name.len() + 2..offset]);

                return IResult::Done(&input[lookahead_offset + 1..], Literal::String(output));
            }
        }
    }

    IResult::Error(Err::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32)))
}


#[cfg(test)]
mod tests {
    use nom::IResult::{Done, Error};
    use nom::{Err, ErrorKind};
    use super::super::super::ast::Literal;
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

    #[test]
    fn case_null() {
        let input  = b"null";
        let output = Done(&b""[..], Literal::Null);

        assert_eq!(null(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_null_case_insensitive() {
        let input  = b"NuLl";
        let output = Done(&b""[..], Literal::Null);

        assert_eq!(null(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_true() {
        let input  = b"true";
        let output = Done(&b""[..], Literal::Boolean(true));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_true_case_insensitive() {
        let input  = b"TrUe";
        let output = Done(&b""[..], Literal::Boolean(true));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_false() {
        let input  = b"false";
        let output = Done(&b""[..], Literal::Boolean(false));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_boolean_false_case_insensitive() {
        let input  = b"FaLsE";
        let output = Done(&b""[..], Literal::Boolean(false));

        assert_eq!(boolean(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_lowercase_b() {
        let input  = b"0b101010";
        let output = Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_uppercase_b() {
        let input  = b"0B101010";
        let output = Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_binary_maximum_integer_value() {
        let input  = b"0b111111111111111111111111111111111111111111111111111111111111111";
        let output = Done(&b""[..], Literal::Integer(::std::i64::MAX));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_overflow() {
        let input  = b"0b1000000000000000000000000000000000000000000000000000000000000000";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(binary(input), Error(Err::Position(ErrorKind::MapRes, &input[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_no_number() {
        let input  = b"0b";
        let output = Error(Err::Position(ErrorKind::Alt, &b"0b"[..]));

        assert_eq!(binary(input), Error(Err::Position(ErrorKind::MapRes, &b"0b"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_not_starting_by_zero_b() {
        let input  = b"1";
        let output = Done(&b""[..], Literal::Integer(1i64));

        assert_eq!(binary(input), Error(Err::Position(ErrorKind::Tag, &b"1"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_binary_not_in_base() {
        let input  = b"0b120";
        let output = Done(&b"20"[..], Literal::Integer(1i64));

        assert_eq!(binary(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal() {
        let input  = b"052";
        let output = Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_octal_maximum_integer_value() {
        let input  = b"0777777777777777777777";
        let output = Done(&b""[..], Literal::Integer(!(1i64 << 63)));

        assert_eq!(octal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_overflow() {
        let input  = b"01000000000000000000000";
        let output = Error(Err::Position(ErrorKind::Alt, &b"01000000000000000000000"[..]));

        assert_eq!(octal(input), Error(Err::Position(ErrorKind::MapRes, &b"01000000000000000000000"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_not_starting_by_zero() {
        let input  = b"7";
        let output = Done(&b""[..], Literal::Integer(7i64));

        assert_eq!(octal(input), Error(Err::Position(ErrorKind::Tag, &b"7"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_octal_not_in_base() {
        let input  = b"8";
        let output = Done(&b""[..], Literal::Integer(8));

        assert_eq!(octal(input), Error(Err::Position(ErrorKind::Tag, &b"8"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_one_digit() {
        let input  = b"7";
        let output = Done(&b""[..], Literal::Integer(7i64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_many_digits() {
        let input  = b"42";
        let output = Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_plus() {
        let input  = b"42+";
        let output = Done(&b"+"[..], Literal::Integer(42i64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_maximum_integer_value() {
        let input  = b"9223372036854775807";
        let output = Done(&b""[..], Literal::Integer(::std::i64::MAX));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_overflow_to_real() {
        let input  = b"9223372036854775808";
        let output = Done(&b""[..], Literal::Real(9223372036854775808f64));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_decimal_maximum_real_value() {
        let input  = b"179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        let output = Done(&b""[..], Literal::Real(::std::f64::MAX));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_decimal_overflow_to_infinity() {
        let input  = b"1797693134862315700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        let output = Done(&b""[..], Literal::Real(::std::f64::INFINITY));

        assert_eq!(decimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_lowercase_x() {
        let input  = b"0x2a";
        let output = Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_uppercase_x() {
        let input  = b"0X2a";
        let output = Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_uppercase_alpha() {
        let input  = b"0x2A";
        let output = Done(&b""[..], Literal::Integer(42i64));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_no_number() {
        let input  = b"0x";
        let output = Error(Err::Position(ErrorKind::Alt, &b"0x"[..]));

        assert_eq!(hexadecimal(input), Error(Err::Position(ErrorKind::HexDigit, &b""[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_not_in_base() {
        let input  = b"0xg";
        let output = Error(Err::Position(ErrorKind::Alt, &b"0xg"[..]));

        assert_eq!(hexadecimal(input), Error(Err::Position(ErrorKind::HexDigit, &b"g"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_hexadecimal_maximum_integer_value() {
        let input  = b"0x7fffffffffffffff";
        let output = Done(&b""[..], Literal::Integer(::std::i64::MAX));

        assert_eq!(hexadecimal(input), output);
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_hexadecimal_overflow() {
        let input  = b"0x8000000000000000";
        let output = Error(Err::Position(ErrorKind::Alt, &b"0x8000000000000000"[..]));

        assert_eq!(hexadecimal(input), Error(Err::Position(ErrorKind::MapRes, &b"0x8000000000000000"[..])));
        assert_eq!(integer(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential() {
        let input  = b"123.456e+78";
        let output = Done(&b""[..], Literal::Real(123.456e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_fractional_part() {
        let input  = b"123.456";
        let output = Done(&b""[..], Literal::Real(123.456f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_part() {
        let input  = b"123.";
        let output = Done(&b""[..], Literal::Real(123.0f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_fractional_part() {
        let input  = b".456";
        let output = Done(&b""[..], Literal::Real(0.456f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_exponent_part_with_lowercase_e() {
        let input  = b"123.e78";
        let output = Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_integer_rational_and_exponent_part() {
        let input  = b"123e78";
        let output = Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_exponent_part_with_uppercase_e() {
        let input  = b"123.E78";
        let output = Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_unsigned_exponent_part() {
        let input  = b"123.e78";
        let output = Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_positive_exponent_part() {
        let input  = b"123.e+78";
        let output = Done(&b""[..], Literal::Real(123e78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_negative_exponent_part() {
        let input  = b"123.e-78";
        let output = Done(&b""[..], Literal::Real(123e-78f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_only_with_rational_and_negative_zero_exponent_part() {
        let input  = b"123.e-0";
        let output = Done(&b""[..], Literal::Real(123f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_exponential_missing_exponent_part() {
        let input  = b".7e";
        let output = Done(&b"e"[..], Literal::Real(0.7f64));

        assert_eq!(exponential(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_exponential_only_the_dot() {
        let input = b".";

        assert_eq!(exponential(input), Error(Err::Code(ErrorKind::RegexpFind)));
        assert_eq!(literal(input), Error(Err::Position(ErrorKind::Alt, &b"."[..])));
    }

    #[test]
    fn case_string_single_quoted() {
        let input  = b"'foobar'";
        let output = Done(&b""[..], Literal::String(b"foobar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_quote() {
        let input  = b"'foo\\'bar'";
        let output = Done(&b""[..], Literal::String(b"foo'bar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_backslash() {
        let input  = b"'foo\\\\bar'";
        let output = Done(&b""[..], Literal::String(b"foo\\bar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_any() {
        let input  = b"'foo\\nbar'";
        let output = Done(&b""[..], Literal::String(b"foo\\nbar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_escaped_many() {
        let input  = b"'\\'f\\oo\\\\bar\\\\'";
        let output = Done(&b""[..], Literal::String(b"'f\\oo\\bar\\".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_single_quoted_empty() {
        let input  = b"''";
        let output = Done(&b""[..], Literal::String(Vec::new()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_single_quoted() {
        let input  = b"b'foobar'";
        let output = Done(&b""[..], Literal::String(b"foobar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_binary_single_quoted_escaped_many() {
        let input  = b"b'\\'f\\oo\\\\bar'";
        let output = Done(&b""[..], Literal::String(b"'f\\oo\\bar".to_vec()));

        assert_eq!(string_single_quoted(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_too_short() {
        let input  = b"'";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Error(Err::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_opening_character() {
        let input  = b"foobar'";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_closing_character() {
        let input  = b"'foobar";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_single_quoted_closing_character_is_a_backslash() {
        let input  = b"'foobar\\";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_single_quoted_too_short() {
        let input  = b"b'";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Error(Err::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_binary_single_quoted_opening_character() {
        let input  = b"bb'";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_single_quoted(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc() {
        let input  = b"<<<'FOO'\nhello \n  world \nFOO;\n";
        let output = Done(&b""[..], Literal::String(b"hello \n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_without_semi_colon() {
        let input  = b"<<<'FOO'\nhello \n  world \nFOO\n";
        let output = Done(&b""[..], Literal::String(b"hello \n  world ".to_vec()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_string_nowdoc_empty() {
        let input  = b"<<<'FOO'\n\nFOO\n";
        let output = Done(&b""[..], Literal::String(Vec::new()));

        assert_eq!(string_nowdoc(input), output);
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_too_short() {
        let input  = b"<<<'A'\nA";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Error(Err::Code(ErrorKind::Custom(StringError::TooShort as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_first_quote() {
        let input  = b"<<<FOO'\nhello \n  world \nFOO\n";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_second_quote() {
        let input  = b"<<<'FOO\nhello \n  world \nFOO\n";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_opening_character_missing_newline() {
        let input  = b"<<<'FOO'hello \n  world \nFOO\n";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidOpeningCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }

    #[test]
    fn case_invalid_string_nowdoc_closing_character() {
        let input  = b"<<<'FOO'\nhello \n  world \nFO;\n";
        let output = Error(Err::Position(ErrorKind::Alt, &input[..]));

        assert_eq!(string_nowdoc(input), Error(Err::Code(ErrorKind::Custom(StringError::InvalidClosingCharacter as u32))));
        assert_eq!(string(input), output);
        assert_eq!(literal(input), output);
    }
}
