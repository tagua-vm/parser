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

//! Group of assignment operator rules.
//!
//! The list of all assignment operators is provided by the PHP
//! Language Specification in the [Expressions chapter, Assignment
//! operators
//! section](https://github.com/php/php-langspec/blob/master/spec/10-expressions.md#assignment-operators).

use super::super::expressions::expression;
use super::super::super::ast::{
    NAryOperation,
    BinaryOperator,
    Expression,
    TernaryOperator,
};
use super::super::super::tokens::Span;
use super::super::super::tokens;
use super::super::tokens::qualified_name;

named_attr!(
    #[doc="
        Recognize all assignment expressions.
    "],
    pub assignment<Span, Expression>,
    map_res!(
        conditional,
        nary_expression_mapper
    )
);

#[inline]
fn nary_expression_mapper<'a>(nary_operation: NAryOperation<'a>) -> Result<Expression<'a>, ()> {
    Ok(Expression::NAryOperation(nary_operation))
}

named_attr!(
    #[doc="
        Recognize a conditional expression.
    "],
    pub conditional<Span, NAryOperation>,
    do_parse!(
        left_operand: logical_or >>
        result: fold_many0!(
            do_parse!(
                first!(tag!(tokens::TERNARY_THEN)) >>
                middle_operand: opt!(first!(expression)) >>
                first!(tag!(tokens::TERNARY_ELSE)) >>
                right_operand: first!(logical_or) >>
                (middle_operand, right_operand)
            ),
            left_operand,
            |accumulator, (middle_operand, right_operand)| {
                match middle_operand {
                    Some(middle_operand) => {
                        NAryOperation::Ternary {
                            operator      : TernaryOperator::Conditional,
                            left_operand  : Box::new(accumulator),
                            middle_operand: Box::new(middle_operand),
                            right_operand : Box::new(right_operand)
                        }
                    },

                    None => {
                        NAryOperation::Binary {
                            operator     : BinaryOperator::Conditional,
                            left_operand : Box::new(accumulator),
                            right_operand: Box::new(right_operand)
                        }
                    }

                }
            }
        ) >>
        (result)
    )
);

macro_rules! binary_operation {
    (
        $parser_name:ident:
        $left_parser:ident
        ($operator_token:ident as $operator_representation:ident)
        $right_parser:ident
    ) => (
        named!(
            $parser_name<Span, NAryOperation>,
            do_parse!(
                left_operand: $left_parser >>
                result: fold_many0!(
                    preceded!(
                        first!(tag!(tokens::$operator_token)),
                        first!($right_parser)
                    ),
                    left_operand,
                    |accumulator, right_operand| {
                        NAryOperation::Binary {
                            operator     : BinaryOperator::$operator_representation,
                            left_operand : Box::new(accumulator),
                            right_operand: Box::new(right_operand)
                        }
                    }
                ) >>
                (result)
            )
        );
    );

    (
        $parser_name:ident:
        $left_parser:ident
        ($($operator_token:ident as $operator_representation:ident),*)
        $right_parser:ident
    ) => (
        named!(
            $parser_name<Span, NAryOperation>,
            do_parse!(
                left_operand: $left_parser >>
                result: fold_many0!(
                    do_parse!(
                        operator: first!(
                            alt_complete!(
                                $(
                                    tag!(tokens::$operator_token) => {
                                        |_| { BinaryOperator::$operator_representation }
                                    }
                                )|*
                            )
                        ) >>
                        right_operand: first!($right_parser) >>
                        (operator, right_operand)
                    ),
                    left_operand,
                    |accumulator, (operator, right_operand)| {
                        NAryOperation::Binary {
                            operator     : operator,
                            left_operand : Box::new(accumulator),
                            right_operand: Box::new(right_operand)
                        }
                    }
                ) >>
                (result)
            )
        );
    )
}

binary_operation!(logical_or : logical_and (BOOLEAN_OR  as LogicalOr)  logical_and);
binary_operation!(logical_and: bitwise_or  (BOOLEAN_AND as LogicalAnd) bitwise_or);
binary_operation!(bitwise_or : bitwise_xor (BITWISE_OR  as BitwiseOr)  bitwise_xor);
binary_operation!(bitwise_xor: bitwise_and (BITWISE_XOR as BitwiseXor) bitwise_and);
binary_operation!(bitwise_and: equality    (BITWISE_AND as BitwiseAnd) equality);
binary_operation!(
    equality:
    relational
    (
        IDENTICAL     as Identical,
        NOT_IDENTICAL as NotIdentical,
        EQUAL         as Equal,
        NOT_EQUAL     as NotEqual,
        NOT_EQUAL_BIS as NotEqual
    )
    relational
);
binary_operation!(
    relational:
    shift
    (
        COMPARE                  as Comparison,
        LESS_THAN_OR_EQUAL_TO    as LessThanOrEqualTo,
        GREATER_THAN_OR_EQUAL_TO as GreaterThanOrEqualTo,
        LESS_THAN                as LessThan,
        GREATER_THAN             as GreaterThan
    )
    shift
);
binary_operation!(
    shift:
    additive
    (
        BITWISE_LEFT_SHIFT  as BitwiseShiftLeft,
        BITWISE_RIGHT_SHIFT as BitwiseShiftRight
    )
    additive
);
binary_operation!(
    additive:
    multiplicative
    (
        ADD         as Plus,
        SUBSTRACT   as Minus,
        CONCATENATE as Dot
    )
    multiplicative
);
binary_operation!(
    multiplicative:
    instanceof
    (
        MULTIPLY as Multiplication,
        DIVIDE   as Division,
        MODULO   as Modulo
    )
    instanceof
);

named!(
    instanceof<Span, NAryOperation>,
    alt_complete!(
        do_parse!(
            subject: expression >>
            type_designator: preceded!(
                first!(tag!(tokens::INSTANCEOF)),
                first!(
                    alt!(
                        qualified_name => {
                            |qualified_name| {
                                Expression::Name(qualified_name)
                            }
                        }
                      | expression
                    )
                )
            ) >>
            (
                NAryOperation::Binary {
                    operator     : BinaryOperator::InstanceOf,
                    left_operand : Box::new(NAryOperation::Nullary(Box::new(subject))),
                    right_operand: Box::new(NAryOperation::Nullary(Box::new(type_designator)))
                }
            )
        )
      | unary_operation
    )
);

named!(
    unary_operation<Span, NAryOperation>,
    call!(leaf)
);

named!(
    leaf<Span, NAryOperation>,
    map_res!(
        expression,
        |expression| -> Result<NAryOperation, ()> {
            Ok(NAryOperation::Nullary(Box::new(expression)))
        }
    )
);


#[cfg(test)]
mod tests {
    use super::assignment;
    use super::super::super::super::ast::{
        BinaryOperator,
        Expression,
        Literal,
        NAryOperation,
        Name,
        TernaryOperator,
        Variable
    };
    use super::super::super::super::internal::Result;
    use super::super::super::super::tokens::{
        Span,
        Token
    };

    /// Build a nullary operation.
    macro_rules! nullary_operation {
        ($expression:expr) => (
            NAryOperation::Nullary(Box::new($expression))
        )
    }

    /// Build a binary operation.
    macro_rules! binary_operation {
        ($operator:ident, $left_operand:expr, $right_operand:expr) => (
            NAryOperation::Binary {
                operator     : BinaryOperator::$operator,
                left_operand : Box::new($left_operand),
                right_operand: Box::new($right_operand)
            }
        )
    }

    /// Build a ternary operation.
    macro_rules! ternary_operation {
        ($operator:ident, $left_operand:expr, $middle_operand:expr, $right_operand:expr) => (
            NAryOperation::Ternary {
                operator      : TernaryOperator::$operator,
                left_operand  : Box::new($left_operand),
                middle_operand: Box::new($middle_operand),
                right_operand : Box::new($right_operand)
            }
        )
    }

    /// Build a literal expression with an integer inside.
    macro_rules! integer {
        ($value:expr, $span:expr) => (
            Expression::Literal(Literal::Integer(Token::new($value, $span)))
        )
    }

    #[test]
    fn case_conditional() {
        let input  = Span::new(b"1 ? 2 : 3 ? 4 : 5");
        let output = Result::Done(
            Span::new_at(b"", 17, 1, 18),
            Expression::NAryOperation(
                ternary_operation!(
                    Conditional,
                    ternary_operation!(
                        Conditional,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        integer!(2, Span::new_at(b"2", 4, 1, 5)),
                        nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                    ),
                    integer!(4, Span::new_at(b"4", 12, 1, 13)),
                    nullary_operation!(integer!(5, Span::new_at(b"5", 16, 1, 17)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_conditional_with_none_middle_operator() {
        let input  = Span::new(b"1 ?: 2 ? : 3");
        let output = Result::Done(
            Span::new_at(b"", 12, 1, 13),
            Expression::NAryOperation(
                binary_operation!(
                    Conditional,
                    binary_operation!(
                        Conditional,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 11, 1, 12)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_logical_or() {
        let input  = Span::new(b"1 || 2 || 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    LogicalOr,
                    binary_operation!(
                        LogicalOr,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_logical_and() {
        let input  = Span::new(b"1 && 2 && 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    LogicalAnd,
                    binary_operation!(
                        LogicalAnd,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_bitwise_or() {
        let input  = Span::new(b"1 | 2 | 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    BitwiseOr,
                    binary_operation!(
                        BitwiseOr,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_bitwise_xor() {
        let input  = Span::new(b"1 ^ 2 ^ 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    BitwiseXor,
                    binary_operation!(
                        BitwiseXor,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_bitwise_and() {
        let input  = Span::new(b"1 & 2 & 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    BitwiseAnd,
                    binary_operation!(
                        BitwiseAnd,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_equality_identical() {
        let input  = Span::new(b"1 === 2 === 3");
        let output = Result::Done(
            Span::new_at(b"", 13, 1, 14),
            Expression::NAryOperation(
                binary_operation!(
                    Identical,
                    binary_operation!(
                        Identical,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 6, 1, 7)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 12, 1, 13)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_equality_not_identical() {
        let input  = Span::new(b"1 !== 2 !== 3");
        let output = Result::Done(
            Span::new_at(b"", 13, 1, 14),
            Expression::NAryOperation(
                binary_operation!(
                    NotIdentical,
                    binary_operation!(
                        NotIdentical,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 6, 1, 7)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 12, 1, 13)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_equality_equal() {
        let input  = Span::new(b"1 == 2 == 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    Equal,
                    binary_operation!(
                        Equal,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_equality_not_equal() {
        let input  = Span::new(b"1 != 2 != 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    NotEqual,
                    binary_operation!(
                        NotEqual,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_equality_not_equal_bis() {
        let input  = Span::new(b"1 <> 2 <> 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    NotEqual,
                    binary_operation!(
                        NotEqual,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_relational_compare() {
        let input  = Span::new(b"1 <=> 2 <=> 3");
        let output = Result::Done(
            Span::new_at(b"", 13, 1, 14),
            Expression::NAryOperation(
                binary_operation!(
                    Comparison,
                    binary_operation!(
                        Comparison,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 6, 1, 7)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 12, 1, 13)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_relational_less_than_or_equal_to() {
        let input  = Span::new(b"1 <= 2 <= 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    LessThanOrEqualTo,
                    binary_operation!(
                        LessThanOrEqualTo,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_relational_greater_than_or_equal_to() {
        let input  = Span::new(b"1 >= 2 >= 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    GreaterThanOrEqualTo,
                    binary_operation!(
                        GreaterThanOrEqualTo,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_relational_less_than() {
        let input  = Span::new(b"1 < 2 < 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    LessThan,
                    binary_operation!(
                        LessThan,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_relational_greater_than() {
        let input  = Span::new(b"1 > 2 > 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    GreaterThan,
                    binary_operation!(
                        GreaterThan,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_shift_left() {
        let input  = Span::new(b"1 << 2 << 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    BitwiseShiftLeft,
                    binary_operation!(
                        BitwiseShiftLeft,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_shift_right() {
        let input  = Span::new(b"1 >> 2 >> 3");
        let output = Result::Done(
            Span::new_at(b"", 11, 1, 12),
            Expression::NAryOperation(
                binary_operation!(
                    BitwiseShiftRight,
                    binary_operation!(
                        BitwiseShiftRight,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 5, 1, 6)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 10, 1, 11)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_additive_plus() {
        let input  = Span::new(b"1 + 2 + 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    Plus,
                    binary_operation!(
                        Plus,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_additive_minus() {
        let input  = Span::new(b"1 - 2 - 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    Minus,
                    binary_operation!(
                        Minus,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_additive_dot() {
        let input  = Span::new(b"1 . 2 . 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    Dot,
                    binary_operation!(
                        Dot,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_multiplicative_multiply() {
        let input  = Span::new(b"1 * 2 * 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    Multiplication,
                    binary_operation!(
                        Multiplication,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_multiplicative_division() {
        let input  = Span::new(b"1 / 2 / 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    Division,
                    binary_operation!(
                        Division,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_multiplicative_modulo() {
        let input  = Span::new(b"1 % 2 % 3");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Expression::NAryOperation(
                binary_operation!(
                    Modulo,
                    binary_operation!(
                        Modulo,
                        nullary_operation!(integer!(1, Span::new(b"1"))),
                        nullary_operation!(integer!(2, Span::new_at(b"2", 4, 1, 5)))
                    ),
                    nullary_operation!(integer!(3, Span::new_at(b"3", 8, 1, 9)))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_instanceof_with_qualified_name_type_designator() {
        let input  = Span::new(b"1 instanceof C");
        let output = Result::Done(
            Span::new_at(b"", 14, 1, 15),
            Expression::NAryOperation(
                binary_operation!(
                    InstanceOf,
                    nullary_operation!(integer!(1, Span::new(b"1"))),
                    nullary_operation!(Expression::Name(Name::Unqualified(Span::new_at(b"C", 13, 1, 14))))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }

    #[test]
    fn case_instanceof_with_expression_type_designator() {
        let input  = Span::new(b"1 instanceof $c");
        let output = Result::Done(
            Span::new_at(b"", 15, 1, 16),
            Expression::NAryOperation(
                binary_operation!(
                    InstanceOf,
                    nullary_operation!(integer!(1, Span::new(b"1"))),
                    nullary_operation!(Expression::Variable(Variable(Span::new_at(b"c", 14, 1, 15))))
                )
            )
        );

        assert_eq!(assignment(input), output);
    }
}
