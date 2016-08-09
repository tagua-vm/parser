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

/// Custom values for `ErrorKind::Custom`.
#[derive(Debug)]
pub enum ErrorKindCustom {
    /// Represent errors from the `exclude` macro.
    Exclude,
    /// Represent errors from the `itag` macro.
    ITag
}

/// `exclude!(I -> Result<I, O>, I -> Result<I, P>) => I -> Result<I, 0>`
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
/// # #[macro_use]
/// # extern crate tagua_parser;
/// use tagua_parser::{
///     Error,
///     ErrorKind,
///     Result
/// };
/// use tagua_parser::macros::ErrorKindCustom;
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
/// assert_eq!(test(&b"fedabc"[..]), Result::Done(&b""[..], &b"fedabc"[..]));
/// assert_eq!(test(&b"abcabc"[..]), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"abcabc"[..])));
/// # }
/// ```
#[macro_export]
macro_rules! exclude(
    ($input:expr, $submacro1:ident!($($arguments1:tt)*), $submacro2:ident!($($arguments2:tt)*)) => (
        {
            match $submacro1!($input, $($arguments1)*) {
                $crate::Result::Done(i, o) =>
                    match $submacro2!(o, $($arguments2)*) {
                        $crate::Result::Done(_, _) =>
                            $crate::Result::Error($crate::Error::Position($crate::ErrorKind::Custom($crate::macros::ErrorKindCustom::Exclude as u32), $input)),

                        $crate::Result::Incomplete(_) =>
                            $crate::Result::Done(i, o),

                        $crate::Result::Error(_) =>
                            $crate::Result::Done(i, o),
                    },

                $crate::Result::Incomplete(e) =>
                    $crate::Result::Incomplete(e),

                $crate::Result::Error(e) =>
                    $crate::Result::Error(e)
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

/// `first!(I -> Result<I, O>) => I -> Result<I, O>`
/// is applying the `skip` rule before the first argument; it allows to skip
/// tokens.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// # #[macro_use]
/// # extern crate tagua_parser;
/// use tagua_parser::Result;
///
/// # fn main() {
/// named!(
///     test,
///     first!(tag!("bar"))
/// );
///
/// assert_eq!(test(&b"/* foo */bar"[..]), Result::Done(&b""[..], &b"bar"[..]));
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

/// `itag!(&[T]: nom::AsBytes) => &[T] -> Result<&[T], &[T]>`
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
/// # #[macro_use]
/// # extern crate tagua_parser;
/// use tagua_parser::Result;
///
/// # fn main() {
/// named!(
///     test<&str>,
///     itag!("foobar")
/// );
///
/// let output = Result::Done(&b""[..], "foobar");
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

            let output: $crate::Result<_, _> =
                if !reduced_input.eq_ignore_ascii_case(reduced_bytes) {
                    $crate::Result::Error($crate::Error::Position($crate::ErrorKind::Custom($crate::macros::ErrorKindCustom::ITag as u32), $input))
                } else if length < bytes_length {
                    $crate::Result::Incomplete($crate::Needed::Size(bytes_length))
                } else {
                    $crate::Result::Done(&$input[bytes_length..], $string)
                };

            output
        }
    );
);

/// `keyword!(&[T]: nom::AsBytes) => &[T] -> Result<&[T], &[T]>`
/// is an alias to the `itag` macro.
///
/// The goal of this alias is twofold:
///
///   1. It avoids confusion and errors (a PHP keyword is always
///      case-insensitive),
///   2. It ensures a better readability of parsers.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// # #[macro_use]
/// # extern crate tagua_parser;
/// use tagua_parser::{
///     Result,
///     tokens
/// };
///
/// # fn main() {
/// named!(
///     test<&[u8]>,
///     keyword!(tokens::CLASS)
/// );
///
/// let output = Result::Done(&b""[..], tokens::CLASS);
///
/// assert_eq!(test(&b"class"[..]), output);
/// assert_eq!(test(&b"ClAsS"[..]), output);
/// # }
/// ```
#[macro_export]
macro_rules! keyword(
    ($input:expr, $keyword:expr) => (
        {
            itag!($input, $keyword)
        }
    );
);


#[cfg(test)]
mod tests {
    use super::ErrorKindCustom;
    use super::super::internal::{
        Error,
        ErrorKind,
        Needed,
        Result
    };

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

        assert_eq!(test(&b"fedabc"[..]), Result::Done(&b""[..], &b"fedabc"[..]));
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

        assert_eq!(test(&b"abcabc"[..]), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"abcabc"[..])));
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

        assert_eq!(test(&b"acebdf"[..]), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::Exclude as u32), &b"acebdf"[..])));
    }

    #[test]
    fn case_first_with_whitespace() {
        named!(hello, tag!("hello"));
        named!(test1, first!(tag!("hello")));
        named!(test2, first!(hello));

        let input  = &b"  \nhello\t\r"[..];
        let output = Result::Done(&b"\t\r"[..], &b"hello"[..]);

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_first_with_comment() {
        named!(hello, tag!("hello"));
        named!(test1, first!(tag!("hello")));
        named!(test2, first!(hello));

        let input  = &b"/* foo */hello/* bar */"[..];
        let output = Result::Done(&b"/* bar */"[..], &b"hello"[..]);

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_first_with_whitespace_and_comment() {
        named!(hello, tag!("hello"));
        named!(test1, first!(tag!("hello")));
        named!(test2, first!(hello));

        let input  = &b"/* foo */  \nhello/* bar */\t"[..];
        let output = Result::Done(&b"/* bar */\t"[..], &b"hello"[..]);

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_itag() {
        named!(test1<&str>, itag!("foobar"));
        named!(test2<&str>, itag!("fOoBaR"));

        let input = &b"FoObArBaZQuX"[..];

        assert_eq!(test1(input), Result::Done(&b"BaZQuX"[..], "foobar"));
        assert_eq!(test2(input), Result::Done(&b"BaZQuX"[..], "fOoBaR"));
    }

    #[test]
    fn case_itag_incomplete() {
        named!(test1<&str>, itag!("foobar"));
        named!(test2<&str>, itag!("FoObAR"));

        let input  = &b"FoOb"[..];
        let output = Result::Incomplete(Needed::Size(6));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_itag_error() {
        named!(test<&str>, itag!("foobar"));

        assert_eq!(test(&b"BaZQuX"[..]), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::ITag as u32), &b"BaZQuX"[..])));
    }

    #[test]
    fn case_keyword() {
        named!(test1<&str>, keyword!("foobar"));
        named!(test2<&str>, keyword!("fOoBaR"));

        let input = &b"FoObArBaZQuX"[..];

        assert_eq!(test1(input), Result::Done(&b"BaZQuX"[..], "foobar"));
        assert_eq!(test2(input), Result::Done(&b"BaZQuX"[..], "fOoBaR"));
    }

    #[test]
    fn case_keyword_incomplete() {
        named!(test1<&str>, keyword!("foobar"));
        named!(test2<&str>, keyword!("FoObAR"));

        let input  = &b"FoOb"[..];
        let output = Result::Incomplete(Needed::Size(6));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_keyword_error() {
        named!(test<&str>, keyword!("foobar"));

        assert_eq!(test(&b"BaZQuX"[..]), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::ITag as u32), &b"BaZQuX"[..])));
    }
}
