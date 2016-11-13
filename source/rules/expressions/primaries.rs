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

//! Group of primary expression rules.
//!
//! The list of all primary expressions is provided by the PHP Language
//! Specification in the [Grammar chapter, Expressions
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#primary-expressions).

use std::result::Result as StdResult;
use super::expression;
use super::super::literals::literal;
use super::super::super::internal::fold_into_vector;
use super::super::tokens::{
    qualified_name,
    variable
};
use super::super::super::ast::{
    Expression,
    Literal,
    Name,
    Variable
};
use super::super::super::internal::{
    Error,
    ErrorKind
};
use super::super::super::tokens;

/// Intrinsic errors.
pub enum IntrinsicError {
    /// The exit code is reserved (only 255 is reserved to PHP).
    ReservedExitCode,
    /// The exit code is out of range if greater than 255.
    OutOfRangeExitCode
}

named!(
    pub primary<Expression>,
    alt!(
        variable       => { variable_mapper }
      | qualified_name => { qualified_name_mapper }
      | literal        => { literal_mapper }
      | intrinsic
    )
);

#[inline(always)]
fn variable_mapper<'a>(variable: Variable<'a>) -> Expression<'a> {
    Expression::Variable(variable)
}

#[inline(always)]
fn qualified_name_mapper<'a>(name: Name<'a>) -> Expression<'a> {
    Expression::Name(name)
}

#[inline(always)]
fn literal_mapper<'a>(literal: Literal) -> Expression<'a> {
    Expression::Literal(literal)
}

named!(
    pub intrinsic<Expression>,
    alt!(
        intrinsic_construct
      | intrinsic_operator
    )
);

named!(
    intrinsic_construct<Expression>,
    alt!(
        intrinsic_echo
      | intrinsic_unset
    )
);

named!(
    intrinsic_operator<Expression>,
    alt!(
        intrinsic_array
      | intrinsic_empty
      | intrinsic_eval
      | intrinsic_exit
      | intrinsic_isset
      | intrinsic_print
    )
);

named!(
    intrinsic_echo<Expression>,
    chain!(
        accumulator: map_res!(
            preceded!(
                keyword!(tokens::ECHO),
                first!(expression)
            ),
            into_vector_mapper
        ) ~
        result: fold_many0!(
            preceded!(
                first!(tag!(tokens::COMMA)),
                first!(expression)
            ),
            accumulator,
            fold_into_vector
        ),
        || { echo_mapper(result) }
    )
);

#[inline(always)]
fn into_vector_mapper<T>(item: T) -> StdResult<Vec<T>, ()> {
    Ok(vec![item])
}

#[inline(always)]
fn echo_mapper<'a>(expressions: Vec<Expression<'a>>) -> Expression<'a> {
    Expression::Echo(expressions)
}

named!(
    intrinsic_unset<Expression>,
    chain!(
        accumulator: map_res!(
            preceded!(
                keyword!(tokens::UNSET),
                preceded!(
                    first!(tag!(tokens::LEFT_PARENTHESIS)),
                    first!(expression)
                )
            ),
            into_vector_mapper
        ) ~
        result: terminated!(
            fold_many0!(
                preceded!(
                    first!(tag!(tokens::COMMA)),
                    first!(expression)
                ),
                accumulator,
                fold_into_vector
            ),
            first!(tag!(tokens::RIGHT_PARENTHESIS))
        ),
        || { unset_mapper(result) }
    )
);

#[inline(always)]
fn unset_mapper<'a>(expressions: Vec<Expression<'a>>) -> Expression<'a> {
    Expression::Unset(expressions)
}

named!(
    intrinsic_array<Expression>,
    preceded!(
        tag!(tokens::LEFT_SQUARE_BRACKET),
        alt!(
            map_res!(
                first!(tag!(tokens::RIGHT_SQUARE_BRACKET)),
                empty_array_mapper
            )
            | chain!(
                accumulator: map_res!(
                    first!(intrinsic_array_pair),
                    into_vector_mapper
                ) ~
                result: fold_many0!(
                    preceded!(
                        first!(tag!(tokens::COMMA)),
                        first!(intrinsic_array_pair)
                    ),
                    accumulator,
                    fold_into_vector
                ) ~
                opt!(first!(tag!(tokens::COMMA))) ~
                first!(tag!(tokens::RIGHT_SQUARE_BRACKET)),
                || { array_mapper(result) }
            )
        )
    )
);

named!(
    intrinsic_array_pair<(Option<Expression>, Expression)>,
    chain!(
        key: opt!(
            terminated!(
                expression,
                first!(tag!(tokens::MAP))
            )
        ) ~
        value: first!(expression),
        || { (key, value) }
    )
);

#[inline(always)]
fn empty_array_mapper<'a>(_: &[u8]) -> StdResult<Expression<'a>, ()> {
    Ok(Expression::Array(vec![]))
}

#[inline(always)]
fn array_mapper<'a>(expressions: Vec<(Option<Expression<'a>>, Expression<'a>)>) -> Expression<'a> {
    Expression::Array(expressions)
}

named!(
    intrinsic_empty<Expression>,
    map_res!(
        preceded!(
            keyword!(tokens::EMPTY),
            preceded!(
                first!(tag!(tokens::LEFT_PARENTHESIS)),
                terminated!(
                    first!(expression),
                    first!(tag!(tokens::RIGHT_PARENTHESIS))
                )
            )
        ),
        empty_mapper
    )
);

#[inline(always)]
fn empty_mapper<'a>(expression: Expression<'a>) -> StdResult<Expression<'a>, ()> {
    Ok(Expression::Empty(Box::new(expression)))
}

named!(
    intrinsic_eval<Expression>,
    map_res!(
        preceded!(
            keyword!(tokens::EVAL),
            preceded!(
                first!(tag!(tokens::LEFT_PARENTHESIS)),
                terminated!(
                    first!(expression),
                    first!(tag!(tokens::RIGHT_PARENTHESIS))
                )
            )
        ),
        eval_mapper
    )
);

#[inline(always)]
fn eval_mapper<'a>(expression: Expression<'a>) -> StdResult<Expression<'a>, ()> {
    Ok(Expression::Eval(Box::new(expression)))
}

named!(
    intrinsic_exit<Expression>,
    map_res!(
        preceded!(
            alt!(
                keyword!(tokens::EXIT)
              | keyword!(tokens::DIE)
            ),
            opt!(
                preceded!(
                    first!(tag!(tokens::LEFT_PARENTHESIS)),
                    terminated!(
                        first!(expression),
                        first!(tag!(tokens::RIGHT_PARENTHESIS))
                    )
                )
            )
        ),
        exit_mapper
    )
);

#[inline(always)]
fn exit_mapper<'a>(expression: Option<Expression<'a>>) -> StdResult<Expression<'a>, Error<ErrorKind>> {
    match expression {
        Some(expression) => {
            if let Expression::Literal(Literal::Integer(code)) = expression {
                if code == 255 {
                    return Err(Error::Code(ErrorKind::Custom(IntrinsicError::ReservedExitCode as u32)));
                } else if code > 255 {
                    return Err(Error::Code(ErrorKind::Custom(IntrinsicError::OutOfRangeExitCode as u32)));
                }
            }

            Ok(Expression::Exit(Some(Box::new(expression))))
        },

        None => {
            Ok(Expression::Exit(None))
        }
    }
}

named!(
    intrinsic_isset<Expression>,
    chain!(
        accumulator: map_res!(
            preceded!(
                keyword!(tokens::ISSET),
                preceded!(
                    first!(tag!(tokens::LEFT_PARENTHESIS)),
                    first!(expression)
                )
            ),
            into_vector_mapper
        ) ~
        result: terminated!(
            fold_many0!(
                preceded!(
                    first!(tag!(tokens::COMMA)),
                    first!(expression)
                ),
                accumulator,
                fold_into_vector
            ),
            first!(tag!(tokens::RIGHT_PARENTHESIS))
        ),
        || { isset_mapper(result) }
    )
);

#[inline(always)]
fn isset_mapper<'a>(expressions: Vec<Expression<'a>>) -> Expression<'a> {
    Expression::Isset(expressions)
}

named!(
    intrinsic_print<Expression>,
    map_res!(
        preceded!(
            keyword!(tokens::PRINT),
            first!(expression)
        ),
        print_mapper
    )
);

#[inline(always)]
fn print_mapper<'a>(expression: Expression<'a>) -> StdResult<Expression<'a>, ()> {
    Ok(Expression::Print(Box::new(expression)))
}


#[cfg(test)]
mod tests {
    use super::{
        intrinsic,
        intrinsic_array,
        intrinsic_construct,
        intrinsic_echo,
        intrinsic_empty,
        intrinsic_eval,
        intrinsic_exit,
        intrinsic_isset,
        intrinsic_operator,
        intrinsic_print,
        intrinsic_unset,
        primary
    };
    use super::super::expression;
    use super::super::super::super::ast::{
        Expression,
        Literal,
        Name,
        Variable
    };
    use super::super::super::super::internal::{
        Error,
        ErrorKind,
        Result
    };

    #[test]
    fn case_primary_variable() {
        let input  = b"$foo";
        let output = Result::Done(&b""[..], Expression::Variable(Variable(&b"foo"[..])));

        assert_eq!(primary(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_primary_qualified_name() {
        let input  = b"Foo\\Bar";
        let output = Result::Done(&b""[..], Expression::Name(Name::Qualified(vec![&b"Foo"[..], &b"Bar"[..]])));

        assert_eq!(primary(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_primary_literal() {
        let input  = b"'Hello, World!'";
        let output = Result::Done(&b""[..], Expression::Literal(Literal::String(b"Hello, World!".to_vec())));

        assert_eq!(primary(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_echo_one_expression() {
        let input  = b"echo /* baz */ 'foobar'";
        let output = Result::Done(
            &b""[..],
            Expression::Echo(
                vec![
                    Expression::Literal(Literal::String(b"foobar".to_vec()))
                ]
            )
        );

        assert_eq!(intrinsic_echo(input), output);
        assert_eq!(intrinsic_construct(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_echo_many_expressions() {
        let input  = b"echo /* baz */ 'foobar',\t $bazqux, \n  42";
        let output = Result::Done(
            &b""[..],
            Expression::Echo(
                vec![
                    Expression::Literal(Literal::String(b"foobar".to_vec())),
                    Expression::Variable(Variable(&b"bazqux"[..])),
                    Expression::Literal(Literal::Integer(42i64))
                ]
            )
        );

        assert_eq!(intrinsic_echo(input), output);
        assert_eq!(intrinsic_construct(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_intrinsic_echo_expression_missing() {
        let input  = b"echo;";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"echo;"[..]));

        assert_eq!(intrinsic_echo(input), Result::Error(Error::Position(ErrorKind::Alt, &b";"[..])));
        assert_eq!(intrinsic_construct(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_unset_one_variable() {
        let input  = b"unset($foo)";
        let output = Result::Done(
            &b""[..],
            Expression::Unset(
                vec![
                    Expression::Variable(Variable(&b"foo"[..]))
                ]
            )
        );

        assert_eq!(intrinsic_unset(input), output);
        assert_eq!(intrinsic_construct(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_unset_many_variables() {
        let input  = b"unset($foo, $bar)";
        let output = Result::Done(
            &b""[..],
            Expression::Unset(
                vec![
                    Expression::Variable(Variable(&b"foo"[..])),
                    Expression::Variable(Variable(&b"bar"[..]))
                ]
            )
        );

        assert_eq!(intrinsic_unset(input), output);
        assert_eq!(intrinsic_construct(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_intrinsic_unset_zero_variable() {
        let input  = b"unset()";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"unset()"[..]));

        assert_eq!(intrinsic_unset(input), Result::Error(Error::Position(ErrorKind::Alt, &b")"[..])));
        assert_eq!(intrinsic_construct(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_empty_string() {
        let input  = b"empty('foo')";
        let output = Result::Done(
            &b""[..],
            Expression::Empty(
                Box::new(
                    Expression::Literal(
                        Literal::String(b"foo".to_vec())
                    )
                )
            )
        );

        assert_eq!(intrinsic_empty(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_empty_integer() {
        let input  = b"empty(42)";
        let output = Result::Done(
            &b""[..],
            Expression::Empty(
                Box::new(
                    Expression::Literal(
                        Literal::Integer(42i64)
                    )
                )
            )
        );

        assert_eq!(intrinsic_empty(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_intrinsic_empty_expression_missing() {
        let input  = b"empty()";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"empty()"[..]));

        assert_eq!(intrinsic_empty(input), Result::Error(Error::Position(ErrorKind::Alt, &b")"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_eval() {
        let input  = b"eval('1 + 2;')";
        let output = Result::Done(
            &b""[..],
            Expression::Eval(
                Box::new(
                    Expression::Literal(
                        Literal::String(b"1 + 2;".to_vec())
                    )
                )
            )
        );

        assert_eq!(intrinsic_eval(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_intrinsic_eval_expression_missing() {
        let input  = b"eval()";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"eval()"[..]));

        assert_eq!(intrinsic_eval(input), Result::Error(Error::Position(ErrorKind::Alt, &b")"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_exit() {
        let input  = b"exit(42)";
        let output = Result::Done(
            &b""[..],
            Expression::Exit(
                Some(
                    Box::new(
                        Expression::Literal(
                            Literal::Integer(42i64)
                        )
                    )
                )
            )
        );

        assert_eq!(intrinsic_exit(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_exit_with_no_argument() {
        let input  = b"exit 42";
        let output = Result::Done(&b" 42"[..], Expression::Exit(None));

        assert_eq!(intrinsic_exit(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_exit_with_a_variable() {
        let input  = b"exit($foo)";
        let output = Result::Done(
            &b""[..],
            Expression::Exit(
                Some(
                    Box::new(
                        Expression::Variable(
                            Variable(&b"foo"[..])
                        )
                    )
                )
            )
        );

        assert_eq!(intrinsic_exit(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_exit_with_reserved_code_255() {
        let input  = b"exit(255)";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"exit(255)"[..]));

        assert_eq!(intrinsic_exit(input), Result::Error(Error::Position(ErrorKind::MapRes, &b"exit(255)"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_exit_with_out_of_range_code() {
        let input  = b"exit(256)";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"exit(256)"[..]));

        assert_eq!(intrinsic_exit(input), Result::Error(Error::Position(ErrorKind::MapRes, &b"exit(256)"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_die() {
        let input  = b"die(42)";
        let output = Result::Done(
            &b""[..],
            Expression::Exit(
                Some(
                    Box::new(
                        Expression::Literal(
                            Literal::Integer(42i64)
                        )
                    )
                )
            )
        );

        assert_eq!(intrinsic_exit(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_die_with_no_parenthesis() {
        let input  = b"die 42";
        let output = Result::Done(&b" 42"[..], Expression::Exit(None));

        assert_eq!(intrinsic_exit(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_die_with_a_variable() {
        let input  = b"die($foo)";
        let output = Result::Done(
            &b""[..],
            Expression::Exit(
                Some(
                    Box::new(
                        Expression::Variable(
                            Variable(&b"foo"[..])
                        )
                    )
                )
            )
        );

        assert_eq!(intrinsic_exit(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_die_with_reserved_code_255() {
        let input  = b"die(255)";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"die(255)"[..]));

        assert_eq!(intrinsic_exit(input), Result::Error(Error::Position(ErrorKind::MapRes, &b"die(255)"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_die_with_out_of_range_code() {
        let input  = b"die(256)";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"die(256)"[..]));

        assert_eq!(intrinsic_exit(input), Result::Error(Error::Position(ErrorKind::MapRes, &b"die(256)"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_isset_one_variable() {
        let input  = b"isset($foo)";
        let output = Result::Done(
            &b""[..],
            Expression::Isset(
                vec![
                    Expression::Variable(Variable(&b"foo"[..]))
                ]
            )
        );

        assert_eq!(intrinsic_isset(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_isset_many_variables() {
        let input  = b"isset($foo, $bar)";
        let output = Result::Done(
            &b""[..],
            Expression::Isset(
                vec![
                    Expression::Variable(Variable(&b"foo"[..])),
                    Expression::Variable(Variable(&b"bar"[..]))
                ]
            )
        );

        assert_eq!(intrinsic_isset(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_intrinsic_isset_zero_variable() {
        let input  = b"isset()";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"isset()"[..]));

        assert_eq!(intrinsic_isset(input), Result::Error(Error::Position(ErrorKind::Alt, &b")"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_print() {
        let input  = b"print /* baz */ 'foobar'";
        let output = Result::Done(
            &b""[..],
            Expression::Print(
                Box::new(
                    Expression::Literal(Literal::String(b"foobar".to_vec()))
                )
            )
        );

        assert_eq!(intrinsic_print(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_invalid_intrinsic_print_expression_missing() {
        let input  = b"print;";
        let output = Result::Error(Error::Position(ErrorKind::Alt, &b"print;"[..]));

        assert_eq!(intrinsic_print(input), Result::Error(Error::Position(ErrorKind::Alt, &b";"[..])));
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_array_empty() {
        let input  = b"[]";
        let output = Result::Done(&b""[..], Expression::Array(vec![]));

        assert_eq!(intrinsic_array(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_array_one_value() {
        let input  = b"['foo']";
        let output = Result::Done(
            &b""[..],
            Expression::Array(
                vec![(
                    None,
                    Expression::Literal(Literal::String(b"foo".to_vec()))
                )]
            )
        );

        assert_eq!(intrinsic_array(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_array_one_pair() {
        let input  = b"[42 => 'foo']";
        let output = Result::Done(
            &b""[..],
            Expression::Array(
                vec![(
                    Some(Expression::Literal(Literal::Integer(42i64))),
                    Expression::Literal(Literal::String(b"foo".to_vec()))
                )]
            )
        );

        assert_eq!(intrinsic_array(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_array_many_pairs() {
        let input  = b"['foo', 42 => 'bar', 'baz' => $qux]";
        let output = Result::Done(
            &b""[..],
            Expression::Array(
                vec![
                    (
                        None,
                        Expression::Literal(Literal::String(b"foo".to_vec()))
                    ),
                    (
                        Some(Expression::Literal(Literal::Integer(42i64))),
                        Expression::Literal(Literal::String(b"bar".to_vec()))
                    ),
                    (
                        Some(Expression::Literal(Literal::String(b"baz".to_vec()))),
                        Expression::Variable(Variable(&b"qux"[..]))
                    )
                ]
            )
        );

        assert_eq!(intrinsic_array(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }

    #[test]
    fn case_intrinsic_array_recursive() {
        let input  = b"['foo', 42 => [3 => 5, 7 => [11 => '13']], 'baz' => $qux]";
        let output = Result::Done(
            &b""[..],
            Expression::Array(
                vec![
                    (
                        None,
                        Expression::Literal(Literal::String(b"foo".to_vec()))
                    ),
                    (
                        Some(Expression::Literal(Literal::Integer(42i64))),
                        Expression::Array(
                            vec![
                                (
                                    Some(Expression::Literal(Literal::Integer(3i64))),
                                    Expression::Literal(Literal::Integer(5i64))
                                ),
                                (
                                    Some(Expression::Literal(Literal::Integer(7i64))),
                                    Expression::Array(
                                        vec![(
                                            Some(Expression::Literal(Literal::Integer(11i64))),
                                            Expression::Literal(Literal::String(b"13".to_vec()))
                                        )]
                                    )
                                )
                            ]
                        )
                    ),
                    (
                        Some(Expression::Literal(Literal::String(b"baz".to_vec()))),
                        Expression::Variable(Variable(&b"qux"[..]))
                    )
                ]
            )
        );

        assert_eq!(intrinsic_array(input), output);
        assert_eq!(intrinsic_operator(input), output);
        assert_eq!(intrinsic(input), output);
        assert_eq!(expression(input), output);
    }
}
