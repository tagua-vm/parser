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
    /// Represent errors from the `skip` macro.
    Skip
}

/// `exclude!(I -> IResult<I, O>, I -> IResult<I, P>) => I -> IResult<I, 0>`
/// returns the result of the first parser if the second fails. Both parsers
/// run on the same input.
///
/// This is handy when the first parser accepts general values and the second
/// parser denies a particular subset of values.
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

/// `skip!(I -> IResult<I, O>) => I -> IResult<I, Option<()>>`
/// applies the parser 0 or more times and skips the consumed data,
/// nothing is returned.
///
/// The embedded parser may return `nom::IResult::Incomplete`.
///
/// This is heavily inspired by the original nom `many0` macro.
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
///     test< Option<()> >,
///     skip!(tag!("abc"))
/// );
///
/// let output = Done(&b"def"[..], None);
/// assert_eq!(test(&b"def"[..]), output);
/// assert_eq!(test(&b"abcdef"[..]), output);
/// assert_eq!(test(&b"abcabcdef"[..]), output);
/// # }
/// ```
#[macro_export]
macro_rules! skip(
    ($input:expr, $submac:ident!($($arguments:tt)*)) => (
        {
            use ::nom::InputLength;

            let out;
            let mut input = $input;

            loop {
                if input.input_len() == 0 {
                    out = ::nom::IResult::Done(input, None);

                    break;
                }

                match $submac!(input, $($arguments)*) {
                    ::nom::IResult::Done(i, _) => {
                        if i == input {
                            out = ::nom::IResult::Error(::nom::Err::Position(::nom::ErrorKind::Custom($crate::macros::ErrorKindCustom::Skip as u32), input));

                            break;
                        }

                        input = i;
                    },

                    ::nom::IResult::Incomplete(::nom::Needed::Unknown) => {
                        out = ::nom::IResult::Incomplete(::nom::Needed::Unknown);

                        break;
                    },

                    ::nom::IResult::Incomplete(::nom::Needed::Size(i)) => {
                        let size = i + ($input).input_len() - input.input_len();
                        out      = ::nom::IResult::Incomplete(::nom::Needed::Size(size));

                        break;
                    },

                    ::nom::IResult::Error(_) => {
                        out = ::nom::IResult::Done(input, None);

                        break;
                    }
                }
            }

            out
        }
    );

    ($input:expr, $f:expr) => (
        skip!($input, call!($f));
    );
);


#[cfg(test)]
mod tests {
    use nom::IResult::{Done, Error};
    use nom::{Err, ErrorKind};
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
    fn case_skip_iterate_0_time() {
        named!(
            test< Option<()> >,
            skip!(tag!("abc"))
        );

        assert_eq!(test(&b"def"[..]), Done(&b"def"[..], None));
    }

    #[test]
    fn case_skip_iterate_1_time() {
        named!(
            test< Option<()> >,
            skip!(tag!("abc"))
        );

        assert_eq!(test(&b"abcdef"[..]), Done(&b"def"[..], None));
    }

    #[test]
    fn case_skip_iterate_2_time() {
        named!(
            test< Option<()> >,
            skip!(tag!("abc"))
        );

        assert_eq!(test(&b"abcabcdef"[..]), Done(&b"def"[..], None));
    }
}
