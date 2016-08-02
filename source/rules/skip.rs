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

//! The skip rule.
//!
//! The skip rule is a special rule representing all the tokens that are not
//! required. For instance, whitespaces and comments can most of the time be
//! skipped.

use super::comments::comment;
use super::whitespaces::whitespace;

named!(
    pub skip< Vec<&[u8]> >,
    many0!(
        alt!(
            comment
          | whitespace
        )
    )
);


#[cfg(test)]
mod tests {
    use nom::IResult::Done;
    use super::skip;

    #[test]
    fn case_skip_comment() {
        assert_eq!(skip(b"/* foo */hello"), Done(&b"hello"[..], vec![&b" foo "[..]]));
    }

    #[test]
    fn case_skip_whitespace() {
        assert_eq!(skip(b"  \nhello"), Done(&b"hello"[..], vec![&b"  \n"[..]]));
    }

    #[test]
    fn case_skip_comment_whitespace() {
        assert_eq!(skip(b"/* foo */  \nhello"), Done(&b"hello"[..], vec![&b" foo "[..], &b"  \n"[..]]));
    }
}
