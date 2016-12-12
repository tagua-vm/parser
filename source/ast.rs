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
    /// A boolean, either `true` or `false`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Literal;
    /// use tagua_parser::rules::literals::literal;
    ///
    /// # fn main() {
    /// assert_eq!(literal(b"true"), Result::Done(&b""[..], Literal::Boolean(true)));
    /// assert_eq!(literal(b"false"), Result::Done(&b""[..], Literal::Boolean(false)));
    /// # }
    /// ```
    Boolean(bool),

    /// An integer, for instance a binary, octal, decimal or hexadecimal number.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Literal;
    /// use tagua_parser::rules::literals::literal;
    ///
    /// # fn main() {
    /// let output = Result::Done(&b""[..], Literal::Integer(42i64));
    ///
    /// assert_eq!(literal(b"0b101010"), output);
    /// assert_eq!(literal(b"052"), output);
    /// assert_eq!(literal(b"42"), output);
    /// assert_eq!(literal(b"0x2a"), output);
    /// # }
    /// ```
    Integer(i64),

    /// A null value.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Literal;
    /// use tagua_parser::rules::literals::literal;
    ///
    /// # fn main() {
    /// assert_eq!(literal(b"null"), Result::Done(&b""[..], Literal::Null));
    /// # }
    /// ```
    Null,

    /// A real, for instance an exponential number.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Literal;
    /// use tagua_parser::rules::literals::literal;
    ///
    /// # fn main() {
    /// let output = Result::Done(&b""[..], Literal::Real(4.2f64));
    ///
    /// assert_eq!(literal(b"4.2"), output);
    /// assert_eq!(literal(b".42e1"), output);
    /// assert_eq!(literal(b"420e-2"), output);
    /// # }
    /// ```
    Real(f64),

    /// A string.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Literal;
    /// use tagua_parser::rules::literals::literal;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     literal(b"'foo\\'bar'"),
    ///     Result::Done(&b""[..], Literal::String(b"foo'bar".to_vec()))
    /// );
    /// # }
    /// ```
    String(Vec<u8>)
}

/// A variable.
///
/// # Examples
///
/// ```
/// # extern crate tagua_parser;
/// use tagua_parser::Result;
/// use tagua_parser::ast::Variable;
/// use tagua_parser::rules::tokens::variable;
///
/// # fn main() {
/// assert_eq!(
///     variable(b"$foo"),
///     Result::Done(&b""[..], Variable(&b"foo"[..]))
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
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Name;
    /// use tagua_parser::rules::tokens::qualified_name;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     qualified_name(b"Bar"),
    ///     Result::Done(&b""[..], Name::Unqualified(&b"Bar"[..]))
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
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Name;
    /// use tagua_parser::rules::tokens::qualified_name;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     qualified_name(b"Foo\\Bar"),
    ///     Result::Done(&b""[..], Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..]]))
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
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Name;
    /// use tagua_parser::rules::tokens::qualified_name;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     qualified_name(b"namespace\\Foo\\Bar"),
    ///     Result::Done(&b""[..], Name::RelativeQualified(vec![&b"Foo"[..], &b"Bar"[..]]))
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
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Name;
    /// use tagua_parser::rules::tokens::qualified_name;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     qualified_name(b"\\Foo\\Bar"),
    ///     Result::Done(&b""[..], Name::FullyQualified(vec![&b"Foo"[..], &b"Bar"[..]]))
    /// );
    /// # }
    /// ```
    /// Note that the leading `\` part is not present.
    FullyQualified(Vec<&'a [u8]>)
}

/// An expression.
#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    /// Anonymous function.
    /// An anonymous function is defined like, and behaves like, a
    /// named function except that the former has no name, and an
    /// enclosed scope.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{
    ///     AnonymousFunction,
    ///     Arity,
    ///     Expression,
    ///     Name,
    ///     Parameter,
    ///     Scope,
    ///     Statement,
    ///     Ty,
    ///     Variable
    /// };
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"function (I $x, J &$y): O use ($z) { return; }"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::AnonymousFunction(
    ///             AnonymousFunction {
    ///                 declaration_scope: Scope::Dynamic,
    ///                 inputs           : Arity::Finite(vec![
    ///                     Parameter {
    ///                         ty   : Ty::Copy(Some(Name::Unqualified(&b"I"[..]))),
    ///                         name : Variable(&b"x"[..]),
    ///                         value: None
    ///                     },
    ///                     Parameter {
    ///                         ty   : Ty::Reference(Some(Name::Unqualified(&b"J"[..]))),
    ///                         name : Variable(&b"y"[..]),
    ///                         value: None
    ///                     }
    ///                 ]),
    ///                 output         : Ty::Copy(Some(Name::Unqualified(&b"O"[..]))),
    ///                 enclosing_scope: Some(vec![Expression::Variable(Variable(&b"z"[..]))]),
    ///                 body           : vec![Statement::Return]
    ///             }
    ///         )
    ///     )
    /// );
    /// # }
    /// ```
    AnonymousFunction(AnonymousFunction<'a>),

    /// Array.
    /// A collection of heterogeneous pairs (key, value). The key is
    /// optional.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"['foo', 42 => 'bar', 'baz' => $qux]"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Array(vec![
    ///             (
    ///                 None,
    ///                 Expression::Literal(Literal::String(b"foo".to_vec()))
    ///             ),
    ///             (
    ///                 Some(Expression::Literal(Literal::Integer(42i64))),
    ///                 Expression::Literal(Literal::String(b"bar".to_vec()))
    ///             ),
    ///             (
    ///                 Some(Expression::Literal(Literal::String(b"baz".to_vec()))),
    ///                 Expression::Variable(Variable(&b"qux"[..]))
    ///             )
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Array(Vec<(Option<Expression<'a>>, Expression<'a>)>),

    /// An echo.
    /// Echo converts each of its expression's values into strings,
    /// concatenates them in order given, and writes the result to the
    /// output stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"echo 'foobar', $bazqux, 42"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Echo(vec![
    ///             Expression::Literal(Literal::String(b"foobar".to_vec())),
    ///             Expression::Variable(Variable(&b"bazqux"[..])),
    ///             Expression::Literal(Literal::Integer(42i64))
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Echo(Vec<Expression<'a>>),

    /// Empty.
    /// Returns `TRUE` if the variable or value designated by the
    /// expression is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"empty('')"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Empty(
    ///             Box::new(
    ///                 Expression::Literal(
    ///                     Literal::String(b"".to_vec())
    ///                 )
    ///             )
    ///         )
    ///     )
    /// );
    /// # }
    /// ```
    Empty(Box<Expression<'a>>),

    /// Eval.
    /// Late evaluation of a PHP program represented as a string.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"eval('1 + 2;')"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Eval(
    ///             Box::new(
    ///                 Expression::Literal(
    ///                     Literal::String(b"1 + 2;".to_vec())
    ///                 )
    ///             )
    ///         )
    ///     )
    /// );
    /// # }
    /// ```
    Eval(Box<Expression<'a>>),

    /// Exit.
    /// Terminate the current script.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"exit(42)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Exit(
    ///             Some(
    ///                 Box::new(
    ///                     Expression::Literal(
    ///                         Literal::Integer(42i64)
    ///                     )
    ///                 )
    ///             )
    ///         )
    ///     )
    /// );
    /// # }
    /// ```
    Exit(Option<Box<Expression<'a>>>),

    /// Isset.
    /// Return `TRUE` if all expressions set and their values are not
    /// `NULL`. Otherwise, it returns `FALSE`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"isset($foo, $bar)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Isset(vec![
    ///             Expression::Variable(Variable(&b"foo"[..])),
    ///             Expression::Variable(Variable(&b"bar"[..]))
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Isset(Vec<Expression<'a>>),

    /// List.
    /// Match and assign one or more elements of the source array to
    /// the target variables.
    ///
    /// # Examples
    ///
    /// A keyed list:
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"list('foo' => $foo, 'bar' => $bar, 'baz' => $baz)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::List(vec![
    ///             Some((
    ///                 Some(Expression::Literal(Literal::String(b"foo".to_vec()))),
    ///                 Expression::Variable(Variable(&b"foo"[..]))
    ///             )),
    ///             Some((
    ///                 Some(Expression::Literal(Literal::String(b"bar".to_vec()))),
    ///                 Expression::Variable(Variable(&b"bar"[..]))
    ///             )),
    ///             Some((
    ///                 Some(Expression::Literal(Literal::String(b"baz".to_vec()))),
    ///                 Expression::Variable(Variable(&b"baz"[..]))
    ///             ))
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    /// An unkeyed list:
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"list($foo, , , $bar, $baz)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::List(vec![
    ///             Some((
    ///                 None,
    ///                 Expression::Variable(Variable(&b"foo"[..]))
    ///             )),
    ///             None,
    ///             None,
    ///             Some((
    ///                 None,
    ///                 Expression::Variable(Variable(&b"bar"[..]))
    ///             )),
    ///             Some((
    ///                 None,
    ///                 Expression::Variable(Variable(&b"baz"[..]))
    ///             ))
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    List(Vec<Option<(Option<Expression<'a>>, Expression<'a>)>>),

    /// A literal.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"'Hello, World!'"),
    ///     Result::Done(&b""[..], Expression::Literal(Literal::String(b"Hello, World!".to_vec())))
    /// );
    /// # }
    /// ```
    Literal(Literal),

    /// A name.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Name};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"Foo\\Bar"),
    ///     Result::Done(&b""[..], Expression::Name(Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..]])))
    /// );
    /// # }
    /// ```
    Name(Name<'a>),

    /// Print.
    /// Unlike `echo`, `print` can be used in any context allowing an
    /// expression. It always returns the value `1`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"print $foo"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Print(
    ///             Box::new(
    ///                 Expression::Variable(Variable(&b"foo"[..])),
    ///             )
    ///         )
    ///     )
    /// );
    /// # }
    /// ```
    Print(Box<Expression<'a>>),

    /// Reference.
    /// Describe an expression assignment by reference.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Literal, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"[7 => &$foo]"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Array(vec![
    ///             (
    ///                 Some(Expression::Literal(Literal::Integer(7i64))),
    ///                 Expression::Reference(
    ///                     Box::new(Expression::Variable(Variable(&b"foo"[..])))
    ///                 )
    ///             )
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Reference(Box<Expression<'a>>),

    /// Unset.
    /// Unset the variables designated by each expression.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"unset($foo, $bar)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Expression::Unset(vec![
    ///             Expression::Variable(Variable(&b"foo"[..])),
    ///             Expression::Variable(Variable(&b"bar"[..]))
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Unset(Vec<Expression<'a>>),

    /// A variable.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{Expression, Variable};
    /// use tagua_parser::rules::expressions::expression;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     expression(b"$foo"),
    ///     Result::Done(&b""[..], Expression::Variable(Variable(&b"foo"[..])))
    /// );
    /// # }
    /// ```
    Variable(Variable<'a>)
}

/// A type declaration.
#[derive(Debug, PartialEq)]
pub enum Ty<'a> {
    /// A type representing a datum passed by copy.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{
    ///     Arity,
    ///     Name,
    ///     Parameter,
    ///     Ty,
    ///     Variable
    /// };
    /// use tagua_parser::rules::statements::function::parameters;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     parameters(b"(I $x)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Arity::Finite(vec![
    ///             Parameter {
    ///                 ty   : Ty::Copy(Some(Name::Unqualified(&b"I"[..]))),
    ///                 name : Variable(&b"x"[..]),
    ///                 value: None
    ///             }
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Copy(Option<Name<'a>>),

    /// A type representing a datum passed by reference.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{
    ///     Arity,
    ///     Name,
    ///     Parameter,
    ///     Ty,
    ///     Variable
    /// };
    /// use tagua_parser::rules::statements::function::parameters;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     parameters(b"(I &$x)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Arity::Finite(vec![
    ///             Parameter {
    ///                 ty   : Ty::Reference(Some(Name::Unqualified(&b"I"[..]))),
    ///                 name : Variable(&b"x"[..]),
    ///                 value: None
    ///             }
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Reference(Option<Name<'a>>)
}

/// A parameter (of a function, or a method).
#[derive(Debug, PartialEq)]
pub struct Parameter<'a> {
    pub ty   : Ty<'a>,
    pub name : Variable<'a>,
    pub value: Option<Expression<'a>>
}

/// Arity of a function.
#[derive(Debug, PartialEq)]
pub enum Arity<'a> {
    /// A function with no parameter.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::Arity;
    /// use tagua_parser::rules::statements::function::parameters;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     parameters(b"()"),
    ///     Result::Done(&b""[..], Arity::Constant)
    /// );
    /// # }
    /// ```
    Constant,

    /// A function with a finite number of parameters.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{
    ///     Arity,
    ///     Parameter,
    ///     Ty,
    ///     Variable
    /// };
    /// use tagua_parser::rules::statements::function::parameters;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     parameters(b"($x, $y)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Arity::Finite(vec![
    ///             Parameter {
    ///                 ty   : Ty::Copy(None),
    ///                 name : Variable(&b"x"[..]),
    ///                 value: None
    ///             },
    ///             Parameter {
    ///                 ty   : Ty::Copy(None),
    ///                 name : Variable(&b"y"[..]),
    ///                 value: None
    ///             }
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Finite(Vec<Parameter<'a>>),

    /// A variadic function.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate tagua_parser;
    /// use tagua_parser::Result;
    /// use tagua_parser::ast::{
    ///     Arity,
    ///     Parameter,
    ///     Ty,
    ///     Variable
    /// };
    /// use tagua_parser::rules::statements::function::parameters;
    ///
    /// # fn main() {
    /// assert_eq!(
    ///     parameters(b"($x, ...$y)"),
    ///     Result::Done(
    ///         &b""[..],
    ///         Arity::Infinite(vec![
    ///             Parameter {
    ///                 ty   : Ty::Copy(None),
    ///                 name : Variable(&b"x"[..]),
    ///                 value: None
    ///             },
    ///             Parameter {
    ///                 ty   : Ty::Copy(None),
    ///                 name : Variable(&b"y"[..]),
    ///                 value: None
    ///             }
    ///         ])
    ///     )
    /// );
    /// # }
    /// ```
    Infinite(Vec<Parameter<'a>>)
}

/// A function.
#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub name  : &'a [u8],
    pub inputs: Arity<'a>,
    pub output: Ty<'a>,
    pub body  : Vec<Statement<'a>>
}

/// An anonymous function.
#[derive(Debug, PartialEq)]
pub struct AnonymousFunction<'a> {
    pub declaration_scope: Scope,
    pub inputs           : Arity<'a>,
    pub output           : Ty<'a>,
    pub enclosing_scope  : Option<Vec<Expression<'a>>>,
    pub body             : Vec<Statement<'a>>
}

/// A statement.
#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    /// A function declaration.
    Function(Function<'a>),

    /// A return.
    Return
}

/// A scope.
#[derive(Debug, PartialEq)]
pub enum Scope {
    /// A dynamic scope (default one).
    Dynamic,

    /// A static scope (when declared with the `static` keyword).
    Static
}

/// A visibility.
#[derive(Debug, PartialEq)]
pub enum Visibility {
    Public,
    Protected,
    Private
}
