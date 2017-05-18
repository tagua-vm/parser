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

//! Group of function rules.
//!
//! The list of all function rules is provided by the PHP Language
//! Specification in the [Chapter chapter, Function Definition
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#function-definition).

use std::result::Result as StdResult;
use super::compound_statement;
use super::super::expressions::constant::constant_expression;
use super::super::tokens::{
    name,
    qualified_name,
    variable
};
use super::super::super::ast::{
    Arity,
    Expression,
    Function,
    Name,
    Parameter,
    Statement,
    Ty,
    Variable
};
use super::super::super::internal::{
    Error,
    ErrorKind
};
use super::super::super::tokens;
use super::super::super::tokens::Span;

/// Function errors.
pub enum FunctionError {
    /// A variadic function has a `...parameter` at an invalid
    /// position. It must be the latest one.
    InvalidVariadicParameterPosition,

    /// A function has multiple parameters declared with the same name.
    MultipleParametersWithSameName
}

named_attr!(
    #[doc="
        Recognize a function.

        # Examples

        A function with 3 inputs, aka parameters:

        1. `$x`, untyped and passed by copy,
        2. `$y`, typed with a fully-qualified name, and passed by
           an implicit reference (this is a copy type, but the type
           is an object, so this is always a reference),
        3. `$z`, typed with a primite type, and passed by reference.

        The output is also typed with a unqualified name, and
        explicitly passed by reference.

        The arity of this function is finite.

        ```
        use tagua_parser::Result;
        use tagua_parser::ast::{
            Arity,
            Function,
            Name,
            Parameter,
            Statement,
            Ty,
            Variable
        };
        use tagua_parser::rules::statements::function::function;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main() {
        assert_eq!(
            function(Span::new(b\"function &f($x, \\\\I\\\\J $y, int &$z): O { return; }\")),
            Result::Done(
                Span::new_at(b\"\", 48, 1, 49),
                Statement::Function(
                    Function {
                        name  : Span::new_at(b\"f\", 10, 1, 11),
                        inputs: Arity::Finite(vec![
                            Parameter {
                                ty   : Ty::Copy(None),
                                name : Variable(Span::new_at(b\"x\", 13, 1, 14)),
                                value: None
                            },
                            Parameter {
                                ty   : Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b\"I\", 17, 1, 18), Span::new_at(b\"J\", 19, 1, 20)]))),
                                name : Variable(Span::new_at(b\"y\", 22, 1, 23)),
                                value: None
                            },
                            Parameter {
                                ty   : Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b\"int\", 25, 1, 26)]))),
                                name : Variable(Span::new_at(b\"z\", 31, 1, 32)),
                                value: None
                            }
                        ]),
                        output: Ty::Reference(Some(Name::Unqualified(Span::new_at(b\"O\", 35, 1, 36)))),
                        body  : vec![Statement::Return]
                    }
                )
            )
        );
        # }
        ```

        This function has an infinite arity. This is also called a
        variadic function. The last parameter receives all extra
        arguments.

        ```
        use tagua_parser::Result;
        use tagua_parser::ast::{
            Arity,
            Function,
            Name,
            Parameter,
            Statement,
            Ty,
            Variable
        };
        use tagua_parser::rules::statements::function::function;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main() {
        assert_eq!(
            function(Span::new(b\"function f($x, int ...$y) { return; }\")),
            Result::Done(
                Span::new_at(b\"\", 37, 1, 38),
                Statement::Function(
                    Function {
                        name  : Span::new_at(b\"f\", 9, 1, 10),
                        inputs: Arity::Infinite(vec![
                            Parameter {
                                ty   : Ty::Copy(None),
                                name : Variable(Span::new_at(b\"x\", 12, 1, 13)),
                                value: None
                            },
                            Parameter {
                                ty   : Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b\"int\", 15, 1, 16)]))),
                                name : Variable(Span::new_at(b\"y\", 23, 1, 24)),
                                value: None
                            }
                        ]),
                        output: Ty::Copy(None),
                        body  : vec![Statement::Return]
                    }
                )
            )
        );
        # }
        ```
    "],
    pub function<Span, Statement>,
    do_parse!(
        first!(keyword!(tokens::FUNCTION)) >>
        output_is_a_reference: opt!(first!(tag!(tokens::REFERENCE))) >>
        name: first!(name) >>
        inputs: first!(parameters) >>
        output_type: opt!(
            preceded!(
                first!(tag!(tokens::FUNCTION_OUTPUT)),
                alt!(
                    first!(native_type)
                  | first!(qualified_name)
                )
            )
        ) >>
        body: first!(compound_statement) >>
        (
            into_function(
                name,
                inputs,
                output_is_a_reference.is_some(),
                output_type,
                body
            )
        )
    )
);

named_attr!(
    #[doc="
        Recognize a list of function parameters.

        # Examples

        ```
        use tagua_parser::Result;
        use tagua_parser::ast::{
            Arity,
            Name,
            Parameter,
            Ty,
            Variable
        };
        use tagua_parser::rules::statements::function::parameters;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main() {
        assert_eq!(
            parameters(Span::new(b\"($x, \\\\I\\\\J $y, int &$z)\")),
            Result::Done(
                Span::new_at(b\"\", 22, 1, 23),
                Arity::Finite(vec![
                    Parameter {
                        ty   : Ty::Copy(None),
                        name : Variable(Span::new_at(b\"x\", 2, 1, 3)),
                        value: None
                    },
                    Parameter {
                        ty   : Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b\"I\", 6, 1, 7), Span::new_at(b\"J\", 8, 1, 9)]))),
                        name : Variable(Span::new_at(b\"y\", 11, 1, 12)),
                        value: None
                    },
                    Parameter {
                        ty   : Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b\"int\", 14, 1, 15)]))),
                        name : Variable(Span::new_at(b\"z\", 20, 1, 21)),
                        value: None
                    }
                ])
            )
        );
        # }
        ```
    "],
    pub parameters<Span, Arity>,
    map_res!(
        terminated!(
            preceded!(
                tag!(tokens::LEFT_PARENTHESIS),
                opt!(
                    do_parse!(
                        accumulator: map_res!(
                            first!(parameter),
                            into_vector_mapper
                        ) >>
                        result: fold_into_vector_many0!(
                            preceded!(
                                first!(tag!(tokens::COMMA)),
                                first!(parameter)
                            ),
                            accumulator
                        ) >>
                        (result)
                    )
                )
            ),
            first!(tag!(tokens::RIGHT_PARENTHESIS))
        ),
        parameters_mapper
    )
);

#[inline]
fn parameters_mapper<'a>(pairs: Option<Vec<(Parameter<'a>, bool)>>) -> StdResult<Arity, Error<ErrorKind>> {
    let mut pairs = match pairs {
        Some(pairs) => {
            pairs
        },

        None => {
            return Ok(Arity::Constant);
        }
    };

    let last_pair      = pairs.pop();
    let mut parameters = Vec::new();

    for (parameter, is_variadic) in pairs {
        if is_variadic {
            return Err(Error::Code(ErrorKind::Custom(FunctionError::InvalidVariadicParameterPosition as u32)));
        }

        if parameters.iter().any(|p: &Parameter<'a>| p.name.0.as_slice() == parameter.name.0.as_slice()) {
            return Err(Error::Code(ErrorKind::Custom(FunctionError::MultipleParametersWithSameName as u32)));
        }

        parameters.push(parameter);
    }

    match last_pair {
        Some((last_parameter, is_variadic)) => {
            if parameters.iter().any(|p: &Parameter<'a>| p.name.0.as_slice() == last_parameter.name.0.as_slice()) {
                return Err(Error::Code(ErrorKind::Custom(FunctionError::MultipleParametersWithSameName as u32)));
            }

            parameters.push(last_parameter);

            if is_variadic {
                Ok(Arity::Infinite(parameters))
            } else {
                Ok(Arity::Finite(parameters))
            }
        },

        None => {
            Ok(Arity::Constant)
        }
    }
}

named!(
    parameter<Span, (Parameter, bool)>,
    do_parse!(
        ty: opt!(
            alt!(
                native_type
              | qualified_name
            )
        ) >>
        is_a_reference: opt!(first!(tag!(tokens::REFERENCE))) >>
        is_variadic: opt!(first!(tag!(tokens::ELLIPSIS))) >>
        name: first!(variable) >>
        default_value: opt!(
            preceded!(
                first!(tag!(tokens::ASSIGN)),
                first!(constant_expression)
            )
        ) >>
        (
            into_parameter(
                ty,
                is_a_reference.is_some(),
                is_variadic.is_some(),
                name,
                default_value
            )
        )
    )
);

#[inline]
fn into_vector_mapper<T>(item: T) -> StdResult<Vec<T>, ()> {
    Ok(vec![item])
}

#[inline]
fn into_parameter<'a>(
    ty            : Option<Name<'a>>,
    is_a_reference: bool,
    is_variadic   : bool,
    name          : Variable<'a>,
    default_value : Option<Expression<'a>>
) -> (Parameter<'a>, bool) {
    (
        Parameter {
            ty   : if is_a_reference { Ty::Reference(ty) } else { Ty::Copy(ty) },
            name : name,
            value: default_value
        },
        is_variadic
    )
}

named!(
    #[doc="
        Recognize all native types.

        # Examples

        ```
        use tagua_parser::Result;
        use tagua_parser::ast::Name;
        use tagua_parser::rules::statements::function::native_type;
        use tagua_parser::tokens::{
            Span,
            Token
        };

        # fn main() {
        assert_eq!(
            native_type(Span::new(b\"int\")),
            Result::Done(
                Span::new_at(b\"\", 3, 1, 4),
                Name::FullyQualified(vec![Span::new(b\"int\")])
            )
        );
        # }
        ```
    "],
    pub native_type<Span, Name>,
    map_res!(
        alt_complete!(
            tag!(tokens::ARRAY)
          | tag!(tokens::BOOL)
          | tag!(tokens::CALLABLE)
          | tag!(tokens::FLOAT)
          | tag!(tokens::INT)
          | tag!(tokens::ITERABLE)
          | tag!(tokens::STRING)
        ),
        native_type_mapper
    )
);

#[inline]
fn native_type_mapper(native_type_name: Span) -> Result<Name, ()> {
    Ok(Name::FullyQualified(vec![native_type_name]))
}

#[inline]
fn into_function<'a>(
    name                 : Span<'a>,
    inputs               : Arity<'a>,
    output_is_a_reference: bool,
    output_type          : Option<Name<'a>>,
    body                 : Vec<Statement<'a>>
) -> Statement<'a> {
    let output = if output_is_a_reference {
        Ty::Reference(output_type)
    } else {
        Ty::Copy(output_type)
    };

    Statement::Function(
        Function {
            name  : name,
            inputs: inputs,
            output: output,
            body  : body
        }
    )
}


#[cfg(test)]
mod tests {
    use super::{
        function,
        native_type,
        parameters
    };
    use super::super::statement;
    use super::super::super::super::ast::{
        Arity,
        Expression,
        Function,
        Literal,
        Name,
        Parameter,
        Statement,
        Ty,
        Variable
    };
    use super::super::super::super::internal::{
        Error,
        ErrorKind,
        Result
    };
    use super::super::super::super::tokens::{
        Span,
        Token
    };

    #[test]
    fn case_function() {
        let input  = Span::new(b"function f(I $x, J &$y): O { return; }");
        let output = Result::Done(
            Span::new_at(b"", 38, 1, 39),
            Statement::Function(
                Function {
                    name  : Span::new_at(b"f", 9, 1, 10),
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Copy(Some(Name::Unqualified(Span::new_at(b"I", 11, 1, 12)))),
                            name : Variable(Span::new_at(b"x", 14, 1, 15)),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Reference(Some(Name::Unqualified(Span::new_at(b"J", 17, 1, 18)))),
                            name : Variable(Span::new_at(b"y", 21, 1, 22)),
                            value: None
                        }
                    ]),
                    output: Ty::Copy(Some(Name::Unqualified(Span::new_at(b"O", 25, 1, 26)))),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_function_arity_zero() {
        let input  = Span::new(b"function f() {}");
        let output = Result::Done(
            Span::new_at(b"", 15, 1, 16),
            Statement::Function(
                Function {
                    name  : Span::new_at(b"f", 9, 1, 10),
                    inputs: Arity::Constant,
                    output: Ty::Copy(None),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_function_arity_many() {
        let input  = Span::new(b"function f($a, I\\J $b, int &$c, \\K $d) {}");
        let output = Result::Done(
            Span::new_at(b"", 41, 1, 42),
            Statement::Function(
                Function {
                    name  : Span::new_at(b"f", 9, 1, 10),
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Copy(None),
                            name : Variable(Span::new_at(b"a", 12, 1, 13)),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Copy(Some(Name::Qualified(vec![Span::new_at(b"I", 15, 1, 16), Span::new_at(b"J", 17, 1, 18)]))),
                            name : Variable(Span::new_at(b"b", 20, 1, 21)),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b"int", 23, 1, 24)]))),
                            name : Variable(Span::new_at(b"c", 29, 1, 30)),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b"K", 33, 1, 34)]))),
                            name : Variable(Span::new_at(b"d", 36, 1, 37)),
                            value: None
                        }
                    ]),
                    output: Ty::Copy(None),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_variadic_function_arity_many() {
        let input  = Span::new(b"function f($a, I\\J $b, int &...$c) {}");
        let output = Result::Done(
            Span::new_at(b"", 37, 1, 38),
            Statement::Function(
                Function {
                    name  : Span::new_at(b"f", 9, 1, 10),
                    inputs: Arity::Infinite(vec![
                        Parameter {
                            ty   : Ty::Copy(None),
                            name : Variable(Span::new_at(b"a", 12, 1, 13)),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Copy(Some(Name::Qualified(vec![Span::new_at(b"I", 15, 1, 16), Span::new_at(b"J", 17, 1, 18)]))),
                            name : Variable(Span::new_at(b"b", 20, 1, 21)),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b"int", 23, 1, 24)]))),
                            name : Variable(Span::new_at(b"c", 32, 1, 33)),
                            value: None
                        }
                    ]),
                    output: Ty::Copy(None),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_function_output_by_copy() {
        let input  = Span::new(b"function f(): \\O {}");
        let output = Result::Done(
            Span::new_at(b"", 19, 1, 20),
            Statement::Function(
                Function {
                    name  : Span::new_at(b"f", 9, 1, 10),
                    inputs: Arity::Constant,
                    output: Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b"O", 15, 1, 16)]))),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_function_output_by_reference() {
        let input  = Span::new(b"function &f(): int {}");
        let output = Result::Done(
            Span::new_at(b"", 21, 1, 22),
            Statement::Function(
                Function {
                    name  : Span::new_at(b"f", 10, 1, 11),
                    inputs: Arity::Constant,
                    output: Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b"int", 15, 1, 16)]))),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_invalid_variadic_function_parameter_position() {
        let input = Span::new(b"function f(...$x, $y) {}");

        assert_eq!(function(input),  Result::Error(Error::Position(ErrorKind::MapRes, Span::new_at(b"(...$x, $y) {}", 10, 1, 11))));
        assert_eq!(statement(input), Result::Error(Error::Position(ErrorKind::Alt, input)));
    }

    #[test]
    fn case_parameters_one_by_copy() {
        let input  = Span::new(b"($x)");
        let output = Result::Done(
            Span::new_at(b"", 4, 1, 5),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Copy(None),
                    name : Variable(Span::new_at(b"x", 2, 1, 3)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_by_reference() {
        let input  = Span::new(b"(&$x)");
        let output = Result::Done(
            Span::new_at(b"", 5, 1, 6),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Reference(None),
                    name : Variable(Span::new_at(b"x", 3, 1, 4)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_with_a_copy_type() {
        let input  = Span::new(b"(A\\B\\C $x)");
        let output = Result::Done(
            Span::new_at(b"", 10, 1, 11),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Copy(Some(Name::Qualified(vec![Span::new_at(b"A", 1, 1, 2), Span::new_at(b"B", 3, 1, 4), Span::new_at(b"C", 5, 1, 6)]))),
                    name : Variable(Span::new_at(b"x", 8, 1, 9)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_with_a_reference_type() {
        let input  = Span::new(b"(int &$x)");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b"int", 1, 1, 2)]))),
                    name : Variable(Span::new_at(b"x", 7, 1, 8)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_variadic_by_copy() {
        let input  = Span::new(b"(...$x)");
        let output = Result::Done(
            Span::new_at(b"", 7, 1, 8),
            Arity::Infinite(vec![
                Parameter {
                    ty   : Ty::Copy(None),
                    name : Variable(Span::new_at(b"x", 5, 1, 6)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_variadic_with_a_reference_type() {
        let input  = Span::new(b"(I &...$x)");
        let output = Result::Done(
            Span::new_at(b"", 10, 1, 11),
            Arity::Infinite(vec![
                Parameter {
                    ty   : Ty::Reference(Some(Name::Unqualified(Span::new_at(b"I", 1, 1, 2)))),
                    name : Variable(Span::new_at(b"x", 8, 1, 9)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_by_copy_with_a_default_value() {
        let input  = Span::new(b"($x = 42)");
        let output = Result::Done(
            Span::new_at(b"", 9, 1, 10),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Copy(None),
                    name : Variable(Span::new_at(b"x", 2, 1, 3)),
                    value: Some(Expression::Literal(Literal::Integer(Token::new(42i64, Span::new_at(b"42", 6, 1, 7)))))
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_by_reference_with_a_default_value() {
        let input  = Span::new(b"(&$x = 'foo')");
        let output = Result::Done(
            Span::new_at(b"", 13, 1, 14),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Reference(None),
                    name : Variable(Span::new_at(b"x", 3, 1, 4)),
                    value: Some(Expression::Literal(Literal::String(Token::new(b"foo".to_vec(), Span::new_at(b"'foo'", 7, 1, 8)))))
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_with_a_copy_type_and_a_default_value() {
        let input  = Span::new(b"(float $x = 4.2)");
        let output = Result::Done(
            Span::new_at(b"", 16, 1, 17),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b"float", 1, 1, 2)]))),
                    name : Variable(Span::new_at(b"x", 8, 1, 9)),
                    value: Some(Expression::Literal(Literal::Real(Token::new(4.2f64, Span::new_at(b"4.2", 12, 1, 13)))))
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_one_with_a_reference_type_and_a_default_value() {
        let input  = Span::new(b"(array &$x = ['foo' => true])");
        let output = Result::Done(
            Span::new_at(b"", 29, 1, 30),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b"array", 1, 1, 2)]))),
                    name : Variable(Span::new_at(b"x", 9, 1, 10)),
                    value: Some(
                        Expression::Array(vec![
                            (
                                Some(Expression::Literal(Literal::String(Token::new(b"foo".to_vec(), Span::new_at(b"'foo'", 14, 1, 15))))),
                                Expression::Name(Name::Unqualified(Span::new_at(b"true", 23, 1, 24)))
                            )
                        ])
                    )
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_variadic_arity_one_by_copy() {
        let input  = Span::new(b"(...$x)");
        let output = Result::Done(
            Span::new_at(b"", 7, 1, 8),
            Arity::Infinite(vec![
                Parameter {
                    ty   : Ty::Copy(None),
                    name : Variable(Span::new_at(b"x", 5, 1, 6)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_variadic_arity_one_by_reference() {
        let input  = Span::new(b"(&...$x)");
        let output = Result::Done(
            Span::new_at(b"", 8, 1, 9),
            Arity::Infinite(vec![
                Parameter {
                    ty   : Ty::Reference(None),
                    name : Variable(Span::new_at(b"x", 6, 1, 7)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_variadic_arity_one_with_a_copy_type() {
        let input  = Span::new(b"(A\\B\\C ...$x)");
        let output = Result::Done(
            Span::new_at(b"", 13, 1, 14),
            Arity::Infinite(vec![
                Parameter {
                    ty   : Ty::Copy(Some(Name::Qualified(vec![Span::new_at(b"A", 1, 1, 2), Span::new_at(b"B", 3, 1, 4), Span::new_at(b"C", 5, 1, 6)]))),
                    name : Variable(Span::new_at(b"x", 11, 1, 12)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_variadic_arity_one_with_a_reference_type() {
        let input  = Span::new(b"(int &...$x)");
        let output = Result::Done(
            Span::new_at(b"", 12, 1, 13),
            Arity::Infinite(vec![
                Parameter {
                    ty   : Ty::Reference(Some(Name::FullyQualified(vec![Span::new_at(b"int", 1, 1, 2)]))),
                    name : Variable(Span::new_at(b"x", 10, 1, 11)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_many() {
        let input  = Span::new(b"(&$x, int $y, I\\J $z)");
        let output = Result::Done(
            Span::new_at(b"", 21, 1, 22),
            Arity::Finite(vec![
                Parameter {
                    ty   : Ty::Reference(None),
                    name : Variable(Span::new_at(b"x", 3, 1, 4)),
                    value: None
                },
                Parameter {
                    ty   : Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b"int", 6, 1, 7)]))),
                    name : Variable(Span::new_at(b"y", 11, 1, 12)),
                    value: None
                },
                Parameter {
                    ty   : Ty::Copy(Some(Name::Qualified(vec![Span::new_at(b"I", 14, 1, 15), Span::new_at(b"J", 16, 1, 17)]))),
                    name : Variable(Span::new_at(b"z", 19, 1, 20)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_parameters_many_variadic() {
        let input  = Span::new(b"(&$x, int $y, I\\J ...$z)");
        let output = Result::Done(
            Span::new_at(b"", 24, 1, 25),
            Arity::Infinite(vec![
                Parameter {
                    ty   : Ty::Reference(None),
                    name : Variable(Span::new_at(b"x", 3, 1, 4)),
                    value: None
                },
                Parameter {
                    ty   : Ty::Copy(Some(Name::FullyQualified(vec![Span::new_at(b"int", 6, 1, 7)]))),
                    name : Variable(Span::new_at(b"y", 11, 1, 12)),
                    value: None
                },
                Parameter {
                    ty   : Ty::Copy(Some(Name::Qualified(vec![Span::new_at(b"I", 14, 1, 15), Span::new_at(b"J", 16, 1, 17)]))),
                    name : Variable(Span::new_at(b"z", 22, 1, 23)),
                    value: None
                }
            ])
        );

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_invalid_parameters_variadic_position() {
        let input  = Span::new(b"(...$x, $y)");
        let output = Result::Error(Error::Position(ErrorKind::MapRes, input));

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_invalid_parameters_two_not_unique() {
        let input  = Span::new(b"($x, $x)");
        let output = Result::Error(Error::Position(ErrorKind::MapRes, input));

        assert_eq!(parameters(input), output);
    }

    #[test]
    fn case_invalid_parameters_many_not_unique() {
        let input  = Span::new(b"($x, $y, $x, $z)");
        let output = Result::Error(Error::Position(ErrorKind::MapRes, input));

        assert_eq!(parameters(input), output);
    }

    macro_rules! test_native_type {
        ($test:ident: $name:expr) => {
            #[test]
            fn $test() {
                let input  = Span::new($name);
                let output = Result::Done(
                    Span::new_at(b"", $name.len(), 1, $name.len() as u32 + 1),
                    Name::FullyQualified(vec![input])
                );

                assert_eq!(native_type(input), output);
            }
        }
    }

    test_native_type!(case_native_type_array:    b"array");
    test_native_type!(case_native_type_bool:     b"bool");
    test_native_type!(case_native_type_callable: b"callable");
    test_native_type!(case_native_type_float:    b"float");
    test_native_type!(case_native_type_int:      b"int");
    test_native_type!(case_native_type_iterable: b"iterable");
    test_native_type!(case_native_type_string:   b"string");
}
