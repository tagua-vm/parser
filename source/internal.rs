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

//! Internal utilities for the parser.

use smallvec::VecLike;

/// Hold the result of a parser.
pub use nom::IResult as Result;

/// Contain the error that a parser can return.
pub use nom::Err as Error;

/// Indicate which parser has returned an error.
pub use nom::ErrorKind;

/// Indicate the context of an error.
pub use nom::Context;

/// Contain information on needed data if a parser returned `Incomplete`.
pub use nom::Needed;

/// Represent the type of the input elements.
pub type InputElement = u8;

/// Represent the type of the input.
pub type Input<'a> = &'a [InputElement];

/// Fold an item into a vector.
/// This is useful when combined with the `fold_many0!` macro for instance.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// # #[macro_use]
/// # extern crate tagua_parser;
/// use tagua_parser::Result;
/// use tagua_parser::internal::fold_into_vector;
///
/// # fn main() {
/// named!(
///     test<&[u8], Vec<&[u8]>>,
///     fold_many0!(
///         tag!("abc"),
///         Vec::new(),
///         fold_into_vector
///     )
/// );
///
/// assert_eq!(test(&b"abcabc"[..]), Ok((&b""[..], vec![&b"abc"[..], &b"abc"[..]])));
/// # }
/// ```
pub fn fold_into_vector<I, V: VecLike<I>>(mut accumulator: V, item: I) -> V {
    accumulator.push(item);

    accumulator
}
