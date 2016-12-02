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

//! Group of function rules.
//!
//! The list of all function rules is provided by the PHP Language
//! Specification in the [Chapter chapter, Function Definition
//! section](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#function-definition).

use std::result::Result as StdResult;
use super::compound_statement;
use super::super::tokens::{
    name,
    qualified_name,
    variable
};
use super::super::super::ast::{
    Arity,
    Function,
    Name,
    Parameter,
    Statement,
    Ty,
    Variable
};
use super::super::super::tokens;

named_attr!(
    #[doc="
        Recognize a function.

        # Examples

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

        # fn main() {
        assert_eq!(
            function(b\"function &f($x, \\\\I\\\\J $y, int &$z): O { return; }\"),
            Result::Done(
                &b\"\"[..],
                Statement::Function(
                    Function {
                        name  : &b\"f\"[..],
                        inputs: Arity::Finite(vec![
                            Parameter {
                                ty   : Ty::Copy(None),
                                name : Variable(&b\"x\"[..]),
                                value: None
                            },
                            Parameter {
                                ty   : Ty::Copy(Some(Name::FullyQualified(vec![&b\"I\"[..], &b\"J\"[..]]))),
                                name : Variable(&b\"y\"[..]),
                                value: None
                            },
                            Parameter {
                                ty   : Ty::Reference(Some(Name::Unqualified(&b\"int\"[..]))),
                                name : Variable(&b\"z\"[..]),
                                value: None
                            }
                        ]),
                        output: Ty::Reference(Some(Name::Unqualified(&b\"O\"[..]))),
                        body  : vec![Statement::Return]
                    }
                )
            )
        );
        # }
        ```
    "],
    pub function<Statement>,
    do_parse!(
        first!(keyword!(tokens::FUNCTION)) >>
        output_is_a_reference: opt!(first!(tag!(tokens::REFERENCE))) >>
        name: first!(name) >>
        first!(tag!(tokens::LEFT_PARENTHESIS)) >>
        inputs: opt!(first!(parameters)) >>
        first!(tag!(tokens::RIGHT_PARENTHESIS)) >>
        output_type: opt!(
            preceded!(
                first!(tag!(tokens::FUNCTION_OUTPUT)),
                first!(qualified_name)
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
            Name,
            Parameter,
            Ty,
            Variable
        };
        use tagua_parser::rules::statements::function::parameters;

        # fn main() {
        assert_eq!(
            parameters(b\"$x, \\\\I\\\\J $y, int &$z\"),
            Result::Done(
                &b\"\"[..],
                vec![
                    Parameter {
                        ty   : Ty::Copy(None),
                        name : Variable(&b\"x\"[..]),
                        value: None
                    },
                    Parameter {
                        ty   : Ty::Copy(Some(Name::FullyQualified(vec![&b\"I\"[..], &b\"J\"[..]]))),
                        name : Variable(&b\"y\"[..]),
                        value: None
                    },
                    Parameter {
                        ty   : Ty::Reference(Some(Name::Unqualified(&b\"int\"[..]))),
                        name : Variable(&b\"z\"[..]),
                        value: None
                    }
                ]
            )
        );
        # }
        ```
    "],
    pub parameters< Vec<Parameter> >,
    do_parse!(
        accumulator: map_res!(
            parameter,
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
);

named!(
    parameter<Parameter>,
    do_parse!(
        ty: opt!(qualified_name) >>
        is_a_reference: opt!(first!(tag!(tokens::REFERENCE))) >>
        name: first!(variable) >>
        (into_parameter(ty, is_a_reference.is_some(), name))
    )
);

#[inline(always)]
fn into_vector_mapper<T>(item: T) -> StdResult<Vec<T>, ()> {
    Ok(vec![item])
}

#[inline(always)]
fn into_parameter<'a>(ty: Option<Name<'a>>, is_a_reference: bool, name: Variable<'a>) -> Parameter<'a> {
    Parameter {
        ty   : if is_a_reference { Ty::Reference(ty) } else { Ty::Copy(ty) },
        name : name,
        value: None
    }
}

#[inline(always)]
fn into_function<'a>(
    name                 : &'a [u8],
    inputs               : Option<Vec<Parameter<'a>>>,
    output_is_a_reference: bool,
    output_type          : Option<Name<'a>>,
    body                 : Vec<Statement<'a>>
) -> Statement<'a> {
    let inputs = match inputs {
        Some(inputs) => {
            Arity::Finite(inputs)
        },

        None => {
            Arity::Constant
        }
    };

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
    use super::function;
    use super::super::statement;
    use super::super::super::super::ast::{
        Arity,
        Function,
        Name,
        Parameter,
        Statement,
        Ty,
        Variable
    };
    use super::super::super::super::internal::Result;

    #[test]
    fn case_function() {
        let input  = b"function f(I $x, J &$y): O { return; }";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Copy(Some(Name::Unqualified(&b"I"[..]))),
                            name : Variable(&b"x"[..]),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Reference(Some(Name::Unqualified(&b"J"[..]))),
                            name : Variable(&b"y"[..]),
                            value: None
                        }
                    ]),
                    output: Ty::Copy(Some(Name::Unqualified(&b"O"[..]))),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_function_arity_zero() {
        let input  = b"function f() {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
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
    fn case_function_arity_one_by_copy() {
        let input  = b"function f($x) {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Copy(None),
                            name : Variable(&b"x"[..]),
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
    fn case_function_arity_one_by_reference() {
        let input  = b"function f(&$x) {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Reference(None),
                            name : Variable(&b"x"[..]),
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
    fn case_function_arity_one_with_a_copy_type() {
        let input  = b"function f(A\\B\\C $x) {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Copy(Some(Name::Qualified(vec![&b"A"[..], &b"B"[..], &b"C"[..]]))),
                            name : Variable(&b"x"[..]),
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
    fn case_function_arity_one_with_a_reference_type() {
        let input  = b"function f(int &$x) {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Reference(Some(Name::Unqualified(&b"int"[..]))),
                            name : Variable(&b"x"[..]),
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
    fn case_function_arity_many() {
        let input  = b"function f($a, I\\J $b, int &$c, \\K $d) {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Finite(vec![
                        Parameter {
                            ty   : Ty::Copy(None),
                            name : Variable(&b"a"[..]),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Copy(Some(Name::Qualified(vec![&b"I"[..], &b"J"[..]]))),
                            name : Variable(&b"b"[..]),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Reference(Some(Name::Unqualified(&b"int"[..]))),
                            name : Variable(&b"c"[..]),
                            value: None
                        },
                        Parameter {
                            ty   : Ty::Copy(Some(Name::FullyQualified(vec![&b"K"[..]]))),
                            name : Variable(&b"d"[..]),
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
        let input  = b"function f(): \\O {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Constant,
                    output: Ty::Copy(Some(Name::FullyQualified(vec![&b"O"[..]]))),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }

    #[test]
    fn case_function_output_by_reference() {
        let input  = b"function &f(): int {}";
        let output = Result::Done(
            &b""[..],
            Statement::Function(
                Function {
                    name  : &b"f"[..],
                    inputs: Arity::Constant,
                    output: Ty::Reference(Some(Name::Unqualified(&b"int"[..]))),
                    body  : vec![Statement::Return]
                }
            )
        );

        assert_eq!(function(input), output);
        assert_eq!(statement(input), output);
    }
}
