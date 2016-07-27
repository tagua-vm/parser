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

//! Extra macros helping to write parsers.

/// Custom values for `nom::ErrorKind::Custom`.
#[derive(Debug)]
pub enum ErrorKindCustom {
    /// Represent errors from the `exclude` macro.
    Exclude,
    /// Represent errors from the `itag` macro.
    ITag
}

/// `exclude!(I -> IResult<I, O>, I -> IResult<I, P>) => I -> IResult<I, 0>`
/// returns the result of the first parser if the second fails. Both parsers
/// run on the same input.
///
/// This is handy when the first parser accepts general values and the second
/// parser denies a particular subset of values.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// use nom::IResult::{Done, Error};
/// use nom::{Err, ErrorKind};
/// # #[macro_use]
/// # extern crate taguavm_parser;
/// use taguavm_parser::macros::ErrorKindCustom;
///
/// # fn main() {
/// named!(
///     test,
///     exclude!(
///         is_a!("abcdef"),
///         alt!(
///             tag!("abc")
///           | tag!("ace")
///         )
///     )
/// );
///
/// assert_eq!(test(&b"fedabc"[..]), Done(&b""[..], &b"fedabc"[..]));
/// assert_eq!(test(&b"abcabc"[..]), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"abcabc"[..])));
/// # }
/// ```
#[macro_export]
macro_rules! exclude(
    ($input:expr, $submacro1:ident!($($arguments1:tt)*), $submacro2:ident!($($arguments2:tt)*)) => (
        {
            match $submacro1!($input, $($arguments1)*) {
                ::nom::IResult::Done(i, o) =>
                    match $submacro2!(o, $($arguments2)*) {
                        ::nom::IResult::Done(_, _) =>
                            ::nom::IResult::Error(::nom::Err::Position(::nom::ErrorKind::Custom($crate::macros::ErrorKindCustom::Exclude as u32), $input)),

                        ::nom::IResult::Incomplete(_) =>
                            ::nom::IResult::Done(i, o),

                        ::nom::IResult::Error(_) =>
                            ::nom::IResult::Done(i, o),
                    },

                ::nom::IResult::Incomplete(e) =>
                    ::nom::IResult::Incomplete(e),

                ::nom::IResult::Error(e) =>
                    ::nom::IResult::Error(e)
            }
        }
    );

    ($input:expr, $submacro1:ident!($($arguments1:tt)*), $g:expr) => (
        exclude!($input, $submacro1!($($arguments1)*), call!($g));
    );

    ($input:expr, $f:expr, $submacro2:ident!($($arguments2:tt)*)) => (
        exclude!($input, call!($f), $submacro2!($($arguments2)*));
    );

    ($input:expr, $f:expr, $g:expr) => (
        exclude!($input, call!($f), call!($g));
    );
);

/// `first!(I -> IResult<I, O>) => I -> IResult<I, O>`
/// is applying the `skip` rule before the first argument; it allows to skip
/// tokens.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// use nom::IResult::Done;
/// # #[macro_use]
/// # extern crate taguavm_parser;
///
/// # fn main() {
/// named!(
///     test,
///     first!(tag!("bar"))
/// );
///
/// assert_eq!(test(&b"/* foo */bar"[..]), Done(&b""[..], &b"bar"[..]));
/// # }
/// ```
#[macro_export]
macro_rules! first(
    ($input:expr, $submacro:ident!($($arguments:tt)*)) => (
        {
            preceded!(
                $input,
                call!($crate::rules::skip::skip),
                $submacro!($($arguments)*)
            )
        }
    );

    ($input:expr, $f:expr) => (
        first!($input, call!($f));
    );
);

/// `itag!(&[T]: nom::AsBytes) => &[T] -> IResult<&[T], &[T]>`
/// declares a case-insensitive ASCII array as a suite to recognize.
///
/// It is pretty similar to the nom `tag!` macro except it is case-insensitive
/// and only accepts ASCII characters so far.
///
/// It does not return the consumed data but the expected data (the first
/// argument).
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// use nom::IResult::Done;
/// # #[macro_use]
/// # extern crate taguavm_parser;
///
/// # fn main() {
/// named!(
///     test<&str>,
///     itag!("foobar")
/// );
///
/// let output = Done(&b""[..], "foobar");
///
/// assert_eq!(test(&b"foobar"[..]), output);
/// assert_eq!(test(&b"FoObAr"[..]), output);
/// # }
/// ```
#[macro_export]
macro_rules! itag(
    ($input:expr, $string:expr) => (
        {
            use std::ascii::AsciiExt;

            #[inline(always)]
            fn as_bytes<T: ::nom::AsBytes>(datum: &T) -> &[u8] {
                datum.as_bytes()
            }

            let expected      = $string;
            let bytes         = as_bytes(&expected);
            let input_length  = $input.len();
            let bytes_length  = bytes.len();
            let length        = ::std::cmp::min(input_length, bytes_length);
            let reduced_input = &$input[..length];
            let reduced_bytes = &bytes[..length];

            let output: ::nom::IResult<_, _> =
                if !reduced_input.eq_ignore_ascii_case(reduced_bytes) {
                    ::nom::IResult::Error(::nom::Err::Position(::nom::ErrorKind::Custom($crate::macros::ErrorKindCustom::ITag as u32), $input))
                } else if length < bytes_length {
                    ::nom::IResult::Incomplete(::nom::Needed::Size(bytes_length))
                } else {
                    ::nom::IResult::Done(&$input[bytes_length..], $string)
                };

            output
        }
    );
);


#[cfg(test)]
mod tests {
    use nom::IResult::{Done, Error, Incomplete};
    use nom::{Err, ErrorKind, Needed};
    use super::ErrorKindCustom;

    #[test]
    fn case_exclude_empty_set() {
        named!(
            test,
            exclude!(
                is_a!("abcdef"),
                alt!(
                    tag!("abc")
                  | tag!("ace")
                )
            )
        );

        assert_eq!(test(&b"fedabc"[..]), Done(&b""[..], &b"fedabc"[..]));
    }

    #[test]
    fn case_exclude_one_branch() {
        named!(
            test,
            exclude!(
                is_a!("abcdef"),
                alt!(
                    tag!("abc")
                  | tag!("ace")
                )
            )
        );

        assert_eq!(test(&b"abcabc"[..]), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"abcabc"[..])));
    }

    #[test]
    fn case_exclude_another_branch() {
        named!(
            test,
            exclude!(
                is_a!("abcdef"),
                alt!(
                    tag!("abc")
                  | tag!("ace")
                )
            )
        );

        assert_eq!(test(&b"acebdf"[..]), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"acebdf"[..])));
    }

    #[test]
    fn case_first_with_whitespace() {
        named!(hello, tag!("hello"));
        named!(test1, first!(tag!("hello")));
        named!(test2, first!(hello));

        let input  = &b"  \nhello\t\r"[..];
        let output = Done(&b"\t\r"[..], &b"hello"[..]);

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_first_with_comment() {
        named!(hello, tag!("hello"));
        named!(test1, first!(tag!("hello")));
        named!(test2, first!(hello));

        let input  = &b"/* foo */hello/* bar */"[..];
        let output = Done(&b"/* bar */"[..], &b"hello"[..]);

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_first_with_whitespace_and_comment() {
        named!(hello, tag!("hello"));
        named!(test1, first!(tag!("hello")));
        named!(test2, first!(hello));

        let input  = &b"/* foo */  \nhello/* bar */\t"[..];
        let output = Done(&b"/* bar */\t"[..], &b"hello"[..]);

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_itag() {
        named!(test1<&str>, itag!("foobar"));
        named!(test2<&str>, itag!("fOoBaR"));

        let input = &b"FoObArBaZQuX"[..];

        assert_eq!(test1(input), Done(&b"BaZQuX"[..], "foobar"));
        assert_eq!(test2(input), Done(&b"BaZQuX"[..], "fOoBaR"));
    }

    #[test]
    fn case_itag_incomplete() {
        named!(test1<&str>, itag!("foobar"));
        named!(test2<&str>, itag!("FoObAR"));

        let input  = &b"FoOb"[..];
        let output = Incomplete(Needed::Size(6));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_itag_error() {
        named!(test<&str>, itag!("foobar"));

        assert_eq!(test(&b"BaZQuX"[..]), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::ITag as u32), &b"BaZQuX"[..])));
    }
}
