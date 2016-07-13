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

//! The identifier rule.
//!
//! The identifier rule as described by the PHP Language Specification in The
//! [Lexical Structure chapter, Names
//! section](https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#names).

use super::super::ast::Identifier;

named!(
    pub identifier<Identifier>,
    map_res!(
        re_bytes_find_static!(r"^[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*"),
        identifier_mapper
    )
);

fn identifier_mapper(string: &[u8]) -> Result<Identifier, ()> {
    Ok(Identifier(string))
}


#[cfg(test)]
mod tests {
    use nom::IResult::{Done, Error};
    use nom::{Err, ErrorKind};
    use super::identifier;
    use super::super::super::ast::Identifier;

    #[test]
    fn case_identifier() {
        assert_eq!(identifier(b"_fooBar42"), Done(&b""[..], Identifier(&b"_fooBar42"[..])));
    }

    #[test]
    fn case_identifier_shortest() {
        assert_eq!(identifier(b"x"), Done(&b""[..], Identifier(&b"x"[..])));
    }

    #[test]
    fn case_identifier_only_head() {
        assert_eq!(identifier(b"aB_\x80"), Done(&b""[..], Identifier(&b"aB_\x80"[..])));
    }

    #[test]
    fn case_identifier_head_and_tail() {
        assert_eq!(identifier(b"aB_\x80aB7\xff"), Done(&b""[..], Identifier(&b"aB_\x80aB7\xff"[..])));
    }

    #[test]
    fn case_identifier_copyright() {
        // © = 0xa9
        assert_eq!(identifier(b"\xa9"), Done(&b""[..], Identifier(&b"\xa9"[..])));
    }

    #[test]
    fn case_identifier_non_breaking_space() {
        //   = 0xa0
        assert_eq!(identifier(b"\xa0"), Done(&b""[..], Identifier(&b"\xa0"[..])));
    }

    #[test]
    fn case_identifier_invalid() {
        assert_eq!(identifier(b"0x"), Error(Err::Code(ErrorKind::RegexpFind)));
    }
}
