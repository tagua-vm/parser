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
    /// Represent errors from the `and_not` macro.
    AndNot
}

/// `and_not!(I -> IResult<I, O>, I -> IResult<I, P>) => I -> IResult<I, 0>`
/// returns the result of the first parser if the second fails. Both parsers
/// run on the same input.
///
/// This is handy when the first parser accepts general values and the second
/// parser denies a particular subset of values.
///
/// ```ignore
/// use nom::IResult::{Done, Error};
/// use nom::{Err, ErrorKind};
///
/// named!(
///     test,
///     and_not!(
///         is_a!("abcdef"),
///         alt!(
///             tag!("abc")
///           | tag!("ace")
///         )
///     )
/// );
///
/// assert_eq!(test(&b"fedabc"[..]), Done(&b""[..], &b"fedabc"[..]));
/// assert_eq!(test(&b"abcabc"[..]), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::AndNot as u32), &b"abcabc"[..])));
/// ```
#[macro_export]
macro_rules! and_not(
    ($input:expr, $submacro1:ident!( $($arguments1:tt)* ), $submacro2:ident!( $($arguments2:tt)* )) => (
        {
            match $submacro1!($input, $($arguments1)*) {
                ::nom::IResult::Done(i, o) =>
                    match $submacro2!($input, $($arguments2)*) {
                        ::nom::IResult::Done(_, _) =>
                            ::nom::IResult::Error(::nom::Err::Position(::nom::ErrorKind::Custom($crate::macros::ErrorKindCustom::AndNot as u32), $input)),

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

    ($input:expr, $submacro1:ident!( $($arguments1:tt)* ), $g:expr) => (
        and_not!($input, $submacro1!($($arguments1)*), call!($g));
    );

    ($input:expr, $f:expr, $submacro1:ident!( $($arguments1:tt)* )) => (
        and_not!($input, call!($f), $submacro1!($($arguments1)*));
    );

    ($input:expr, $f:expr, $g:expr) => (
        and_not!($input, call!($f), call!($g));
    );
);


#[cfg(test)]
mod tests {
    use nom::IResult::{Done, Error};
    use nom::{Err, ErrorKind};
    use super::ErrorKindCustom;

    #[test]
    fn case_and_not() {
        named!(
            test,
            and_not!(
                is_a!("abcdef"),
                alt!(
                    tag!("abc")
                  | tag!("ace")
                )
            )
        );

        assert_eq!(test(&b"fedabc"[..]), Done(&b""[..], &b"fedabc"[..]));
        assert_eq!(test(&b"abcabc"[..]), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::AndNot as u32), &b"abcabc"[..])));
        assert_eq!(test(&b"acebdf"[..]), Error(Err::Position(ErrorKind::Custom(ErrorKindCustom::AndNot as u32), &b"acebdf"[..])));
    }
}
