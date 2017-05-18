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

//! Extra macros helping to write parsers.

/// Custom values for `ErrorKind::Custom`.
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
/// use tagua_parser::tokens::Span;
///
/// # fn main() {
/// named!(
///     test<Span, Span>,
///     first!(tag!(b"bar"))
/// );
///
/// assert_eq!(
///     test(Span::new(b"/* foo */bar")),
///     Result::Done(
///         Span::new_at(b"", 12, 1, 13),
///         Span::new_at(b"bar", 9, 1, 10)
///     )
/// );
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

/// `itag!(I -> Result<I, O>) => I -> Result<I, O>`
/// declares a case-insensitive tag to recognize.
///
/// It is pretty similar to the nom `tag!` macro except it is
/// case-insensitive, and return the expected tag, not the consumed
/// tag.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// # #[macro_use]
/// # extern crate tagua_parser;
/// use tagua_parser::Result;
/// use tagua_parser::tokens::Span;
///
/// # fn main() {
/// named!(
///     test<Span, Span>,
///     itag!(b"foobar")
/// );
///
/// let output = Result::Done(Span::new_at(b"", 6, 1, 7), Span::new(b"foobar"));
///
/// assert_eq!(test(Span::new(b"foobar")), output);
/// assert_eq!(test(Span::new(b"FoObAr")), output);
/// # }
/// ```
#[macro_export]
macro_rules! itag(
    ($input:expr, $tag:expr) => (
        {
            use $crate::tokens::Span;
            use nom::{
                Compare,
                CompareResult,
                InputLength,
                Slice
            };

            let tag_length = $tag.input_len();

            let output: $crate::Result<_, _> = match $input.compare_no_case($tag) {
                CompareResult::Ok => {
                    let Span { offset, line, column, .. } = $input.slice(..tag_length);
                    let consumed = Span::new_at($tag, offset, line, column);

                    $crate::Result::Done($input.slice(tag_length..), consumed)
                },

                CompareResult::Incomplete => {
                    $crate::Result::Incomplete($crate::Needed::Size($tag.input_len()))
                },

                CompareResult::Error => {
                    $crate::Result::Error($crate::Error::Position($crate::ErrorKind::Custom($crate::macros::ErrorKindCustom::ITag as u32), $input))
                }
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
/// use tagua_parser::tokens::Span;
///
/// # fn main() {
/// named!(
///     test<Span, Span>,
///     keyword!(tokens::CLASS)
/// );
///
/// let output = Result::Done(Span::new_at(b"", 5, 1, 6), Span::new(tokens::CLASS));
///
/// assert_eq!(test(Span::new(b"class")), output);
/// assert_eq!(test(Span::new(b"ClAsS")), output);
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

/// `fold_into_vector_many0!(I -> IResult<I,O>, R) => I -> IResult<I, R>`
/// is a wrapper around `fold_many0!` specifically designed for vectors.
///
/// This is strictly equivalent to `fold_many0!(submacro!(…),
/// Vec::new(), fold_into_vector)` but it shrinks the capacity of the
/// vector to fit the current length.
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
///     test<Vec<&[u8]>>,
///     fold_into_vector_many0!(
///         tag!("abc"),
///         Vec::new()
///     )
/// );
///
/// if let Result::Done(_, vector) = test(b"abcabcabc") {
///     assert_eq!(vector.capacity(), vector.len());
/// }
/// # }
/// ```
#[macro_export]
macro_rules! fold_into_vector_many0(
    ($input:expr, $submacro:ident!($($arguments:tt)*), $init:expr) => (
        {
            let result = fold_many0!(
                $input,
                $submacro!($($arguments)*),
                $init,
                $crate::internal::fold_into_vector
            );

            if let ::nom::IResult::Done(input, mut output) = result {
                output.shrink_to_fit();

                ::nom::IResult::Done(input, output)
            } else {
                result
            }
        }
    );

    ($input:expr, $function:expr, $init:expr) => (
        fold_many0!($input, call!($function), $init);
    );
);

/// `regex!(regexp) => &[T] -> IResult<&[T], &[T]>`
/// Return the first match.
///
/// This is exactly like `re_bytes_find_static!` from nom, except that
/// it works on `Span`.
#[macro_export]
macro_rules! regex (
    ($input:expr, $regex:expr) => (
        {
            use nom::Slice;

            regex_bytes!(RE, $regex);

            if let Some(first_match) = RE.find($input.as_slice()) {
                $crate::Result::Done($input.slice(first_match.end()..), $input.slice(first_match.start()..first_match.end()))
            } else {
                let output: $crate::Result<_, _> = $crate::Result::Error(error_code!($crate::ErrorKind::RegexpFind));

                output
            }
        }
    )
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
    use super::super::tokens::Span;

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
    fn case_exclude_incomplete() {
        named!(
            test,
            exclude!(
                take!(3),
                alt!(
                    tag!("abc")
                  | tag!("ace")
                )
            )
        );

        assert_eq!(test(&b"a"[..]), Result::Incomplete(Needed::Size(3)));
    }

    #[test]
    fn case_exclude_incomplete_submacro() {
        named!(
            test,
            exclude!(
                take!(3),
                take!(5)
            )
        );

        assert_eq!(test(&b"abcdef"[..]), Result::Done(&b"def"[..], &b"abc"[..]));
    }

    #[test]
    fn case_first_with_whitespace() {
        named!(hello<Span, Span>, tag!(b"hello"));
        named!(test1<Span, Span>, first!(tag!(b"hello")));
        named!(test2<Span, Span>, first!(hello));

        let input  = Span::new(b"  \nhello\t\r");
        let output = Result::Done(Span::new_at(b"\t\r", 8, 2, 6), Span::new_at(b"hello", 3, 2, 1));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_first_with_comment() {
        named!(hello<Span, Span>, tag!(b"hello"));
        named!(test1<Span, Span>, first!(tag!(b"hello")));
        named!(test2<Span, Span>, first!(hello));

        let input  = Span::new(b"/* foo */hello/* bar */");
        let output = Result::Done(Span::new_at(b"/* bar */", 14, 1, 15), Span::new_at(b"hello", 9, 1, 10));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_first_with_whitespace_and_comment() {
        named!(hello<Span, Span>, tag!(b"hello"));
        named!(test1<Span, Span>, first!(tag!(b"hello")));
        named!(test2<Span, Span>, first!(hello));

        let input  = Span::new(b"/* foo */  \nhello/* bar */\t");
        let output = Result::Done(Span::new_at(b"/* bar */\t", 17, 2, 6), Span::new_at(b"hello", 12, 2, 1));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_itag() {
        named!(test1<Span, Span>, itag!(b"foobar"));
        named!(test2<Span, Span>, itag!(b"fOoBaR"));

        let input = Span::new(&b"FoObArBaZQuX"[..]);

        assert_eq!(test1(input), Result::Done(Span::new_at(&b"BaZQuX"[..], 6, 1, 7), Span::new_at(&b"foobar"[..], 0, 1, 1)));
        assert_eq!(test2(input), Result::Done(Span::new_at(&b"BaZQuX"[..], 6, 1, 7), Span::new_at(&b"fOoBaR"[..], 0, 1, 1)));
    }

    #[test]
    fn case_itag_incomplete() {
        named!(test1<Span, Span>, itag!(b"foobar"));
        named!(test2<Span, Span>, itag!(b"FoObAR"));

        let input  = Span::new(&b"FoOb"[..]);
        let output = Result::Incomplete(Needed::Size(6));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_itag_error() {
        named!(test<Span, Span>, itag!(b"foobar"));

        assert_eq!(test(Span::new(&b"BaZQuX"[..])), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::ITag as u32), Span::new(&b"BaZQuX"[..]))));
    }

    #[test]
    fn case_keyword() {
        named!(test1<Span, Span>, keyword!(b"foobar"));
        named!(test2<Span, Span>, keyword!(b"fOoBaR"));

        let input  = Span::new(b"FoObArBaZQuX");
        let output = Span::new_at(b"BaZQuX", 6, 1, 7);

        assert_eq!(test1(input), Result::Done(output, Span::new(b"foobar")));
        assert_eq!(test2(input), Result::Done(output, Span::new(b"fOoBaR")));
    }

    #[test]
    fn case_keyword_incomplete() {
        named!(test1<Span, Span>, keyword!(b"foobar"));
        named!(test2<Span, Span>, keyword!(b"FoObAR"));

        let input  = Span::new(b"FoOb");
        let output = Result::Incomplete(Needed::Size(6));

        assert_eq!(test1(input), output);
        assert_eq!(test2(input), output);
    }

    #[test]
    fn case_keyword_error() {
        named!(test<Span, Span>, keyword!(b"foobar"));

        let input = Span::new(b"BaZQuX");

        assert_eq!(test(input), Result::Error(Error::Position(ErrorKind::Custom(ErrorKindCustom::ITag as u32), input)));
    }

    #[test]
    fn case_fold_into_vector_many0() {
        named!(
            test<Vec<&[u8]>>,
            fold_into_vector_many0!(
                tag!("abc"),
                Vec::new()
            )
        );

        if let Result::Done(_, vector) = test(&b"abcabcabc"[..]) {
            assert_eq!(vector.capacity(), vector.len());
            assert_eq!(vector.len(), 3);
        } else {
            assert!(false);
        }
    }
}
