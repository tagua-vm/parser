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

//! Structures that will constitute the Abstract Syntax Tree.

/// A term.
#[derive(Debug, PartialEq)]
pub struct Term {
    /// The term value.
    pub t: Literal
}

/// A binding of a value to a variable.
#[derive(Debug, PartialEq)]
pub struct Binding {
    ///
    pub variable: String,
    /// 
    pub expression: Term
}

/// An addition of two terms.
#[derive(Debug, PartialEq)]
pub struct Addition {
    /// The left-hand side of the addition.
    pub a: Term,
    /// The right-hand side of the addition.
    pub b: Term
}

/// A literal represents a fixed value, aka an atom.
#[derive(Debug, PartialEq)]
pub enum Literal {
    /// A null value.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::literals::literal;
    /// use tagua_parser::ast::Literal;
    ///
    /// # fn main () {
    /// assert_eq!(literal(b"null"), Done(&b""[..], Literal::Null));
    /// # }
    /// ```
    Null,

    /// A boolean, either `true` or `false`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::literals::literal;
    /// use tagua_parser::ast::Literal;
    ///
    /// # fn main () {
    /// assert_eq!(literal(b"true"),  Done(&b""[..], Literal::Boolean(true)));
    /// assert_eq!(literal(b"false"), Done(&b""[..], Literal::Boolean(false)));
    /// # }
    /// ```
    Boolean(bool),

    /// An integer, for instance a binary, octal, decimal or hexadecimal number.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::literals::literal;
    /// use tagua_parser::ast::Literal;
    ///
    /// # fn main () {
    /// let output = Done(&b""[..], Literal::Integer(42u64));
    ///
    /// assert_eq!(literal(b"0b101010"), output);
    /// assert_eq!(literal(b"052"), output);
    /// assert_eq!(literal(b"42"), output);
    /// assert_eq!(literal(b"0x2a"), output);
    /// # }
    /// ```
    Integer(u64),

    /// A real, for instance an exponential number.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::literals::literal;
    /// use tagua_parser::ast::Literal;
    ///
    /// # fn main () {
    /// let output = Done(&b""[..], Literal::Real(4.2f64));
    ///
    /// assert_eq!(literal(b"4.2"), output);
    /// assert_eq!(literal(b".42e1"), output);
    /// assert_eq!(literal(b"420.0e-2"), output);
    /// # }
    /// ```
    Real(f64),

    /// A string.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::literals::literal;
    /// use tagua_parser::ast::Literal;
    ///
    /// # fn main () {
    /// assert_eq!(literal(b"'foo\\'bar'"), Done(&b""[..], Literal::String(b"foo'bar".to_vec())));
    /// # }
    /// ```
    String(Vec<u8>)
}


/// A variable.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate nom;
/// use nom::IResult::Done;
/// # extern crate tagua_parser;
/// use tagua_parser::rules::tokens::variable;
/// use tagua_parser::ast::Variable;
///
/// # fn main () {
/// assert_eq!(
///     variable(b"$foo"),
///     Done(&b""[..], Variable(&b"foo"[..]))
/// );
/// # }
/// ```
/// Note that the `$` is not present.
#[derive(Debug, PartialEq)]
pub struct Variable<'a>(pub &'a [u8]);

/// A name represents an entity name.
#[derive(Debug, PartialEq)]
pub enum Name<'a> {
    /// An unqualified name, i.e. a name without a namespace, like `Bar`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::tokens::qualified_name;
    /// use tagua_parser::ast::Name;
    ///
    /// # fn main () {
    /// assert_eq!(
    ///     qualified_name(b"Bar"),
    ///     Done(&b""[..], Name::Unqualified(&b"Bar"[..]))
    /// );
    /// # }
    /// ```
    Unqualified(&'a [u8]),

    /// A qualified name, i.e. a name in a relative namespace (aliased or not),
    /// like `Foo\Bar`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::tokens::qualified_name;
    /// use tagua_parser::ast::Name;
    ///
    /// # fn main () {
    /// assert_eq!(
    ///     qualified_name(b"Foo\\Bar"),
    ///     Done(&b""[..], Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..]]))
    /// );
    /// # }
    /// ```
    Qualified(Vec<&'a [u8]>),

    /// A relative qualified name, i.e. a name in a relative namespace
    /// restricted to the current namespace, like `namespace\Foo\Bar`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::tokens::qualified_name;
    /// use tagua_parser::ast::Name;
    ///
    /// # fn main () {
    /// assert_eq!(
    ///     qualified_name(b"namespace\\Foo\\Bar"),
    ///     Done(&b""[..], Name::RelativeQualified(vec![&b"Foo"[..], &b"Bar"[..]]))
    /// );
    /// # }
    /// ```
    /// Note that the `namespace` part is not present.
    RelativeQualified(Vec<&'a [u8]>),

    /// A fully qualified name, i.e. a name in an absolute namespace, like
    /// `\Foo\Bar`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate nom;
    /// use nom::IResult::Done;
    /// # extern crate tagua_parser;
    /// use tagua_parser::rules::tokens::qualified_name;
    /// use tagua_parser::ast::Name;
    ///
    /// # fn main () {
    /// assert_eq!(
    ///     qualified_name(b"\\Foo\\Bar"),
    ///     Done(&b""[..], Name::FullyQualified(vec![&b"Foo"[..], &b"Bar"[..]]))
    /// );
    /// # }
    /// ```
    /// Note that the leading `\` part is not present.
    FullyQualified(Vec<&'a [u8]>)
}
