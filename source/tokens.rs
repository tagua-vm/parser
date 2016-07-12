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

//! List of lexemes.
//!
//! The list of all lexemes, aka tokens, is provided by the PHP Language
//! Specification in the [Grammar
//! chapter](https://github.com/php/php-langspec/blob/master/spec/19-grammar.md#lexical-grammar).
//!
//! All lexemes are declared as static string constants.

/// Helper to declare a token.
///
/// ### Examples
///
/// The following example declares the `FOO_BAR` token:
///
/// ```
/// token!(FOO_BAR: "foobar"; "The `FOO_BAR` token, mostly used in example.");
/// ```
macro_rules! token {
    ($name:ident: $value:expr; $documentation:expr) => (
        #[doc=$documentation]
        const $name: &'static str = $value;
    );

    (pub $name:ident: $value:expr; $documentation:expr) => (
        #[doc=$documentation]
        pub const $name: &'static str = $value;
    )
}

token!(
    pub ABSTRACT: "abstract";
    "The `ABSTRACT` token.\n\nRepresent an abstract entity, e.g. `abstract class C { … }`."
);
token!(
    pub ADD: "+";
    "The `ADD` token.\n\nRepresent the addition operator, e.g. `$x + $y`."
);
token!(
    pub ADD_AND_ASSIGN: "+=";
    "The `ADD_AND_ASSIGN` token.\n\nRepresent the addition assignment operator, e.g. `$x += $y;`."
);
token!(
    pub AND: "and";
    "The `AND` token.\n\nRepresent the conjunction operator, used in a logical expression, e.g. `$x and $y`."
);
token!(
    pub ARRAY: "array";
    "The `ARRAY` token.\n\nRepresent the array constructor, e.g. `array($x, $y)`."
);
token!(
    pub AS: "as";
    "The `AS` token.\n\nRepresent the alias operator, e.g. `use Foo\\Bar as Baz`."
);
token!(
    pub ASSIGN: "=";
    "The `ASSIGN` token.\n\nRepresent a binding of a value to a variable, e.g. `$x = 42`."
);
token!(
    pub BITWISE_AND: "&";
    "The `BITWISE_AND` token.\n\nRepresent the bitwise conjunction operator, e.g. `$x & $y`."
);
token!(
    pub BITWISE_AND_AND_ASSIGN: "&=";
    "The `BITWISE_AND_AND_ASSIGN` token.\n\nRepresent the bitwise conjunction assignment operator, e.g. `$x &= $y;`."
);
token!(
    pub BITWISE_LEFT_SHIFT: "<<";
    "The `BITWISE_LEFT_SHIFT` token.\n\nRepresent the bitwise left shift operator, e.g. `$x << $y`."
);
token!(
    pub BITWISE_LEFT_SHIFT_AND_ASSIGN: "<<=";
    "The `BITWISE_LEFT_SHIFT_AND_ASSIGN` token.\n\nRepresent the bitwise left shift assignment operator, e.g. `$x <<= $y;`."
);
token!(
    pub BITWISE_NOT: "~";
    "The `BITWISE_NOT` token.\n\nRepresent the bitwise negation operator, e.g. `~$x`."
);
token!(
    pub BITWISE_OR: "|";
    "The `BITWISE_OR` token.\n\nRepresent the inclusive bitwise disjunction operator, e.g. `$x | $y`."
);
token!(
    pub BITWISE_OR_AND_ASSIGN: "|=";
    "The `BITWISE_OR_AND_ASSIGN` token.\n\nRepresent the inclusive bitwise disjunction assignment operator, e.g. `$x |= $y;`."
);
token!(
    pub BITWISE_RIGHT_SHIFT: ">>";
    "The `BITWISE_RIGHT_SHIFT` token.\n\nRepresent the bitwise right shift operator, e.g. `$x >> $y`."
);
token!(
    pub BITWISE_RIGHT_SHIFT_AND_ASSIGN: "<<=";
    "The `BITWISE_RIGHT_SHIFT_AND_ASSIGN` token.\n\nRepresent the bitwise right shift assignment operator, e.g. `$x >>= $y;`."
);
token!(
    pub BITWISE_XOR: "^";
    "The `BITWISE_XOR` token.\n\nRepresent the exclusive bitwise disjunction operator, e.g. `$x ^ $y`."
);
token!(
    pub BITWISE_XOR_AND_ASSIGN: "^=";
    "The `BITWISE_XOR_AND_ASSIGN` token.\n\nRepresent the exclusive bitwise disjunction assignment operator, e.g. `$x ^= $y;`."
);
token!(
    pub BOOLEAN_AND: "&&";
    "The `BOOLEAN_AND` token.\n\nRepresent the boolean conjunction operator, e.g. `$x && $y`."
);
token!(
    pub BOOLEAN_NOT: "!";
    "The `NOT` token.\n\nRepresent the boolean negation operator, e.g. `!$x`."
);
token!(
    pub BOOLEAN_OR: "||";
    "The `BOOLEAN_OR` token.\n\nRepresent the boolean disjunction operator, e.g. `$x || $y`."
);
token!(
    pub BREAK: "break";
    "The `BREAK` token.\n\nRepresent the control flow breaker operator, e.g. `break 2`."
);
token!(
    pub CALLABLE: "callable";
    "The `CALLABLE` token.\n\nRepresent the callable type, e.g. `function f(callable $x) { … }`."
);
token!(
    pub CASE: "case";
    "The `CASE` token.\n\nRepresent a case in a `switch` control structure, e.g. `switch (…) { case …: …; }`."
);
token!(
    pub CATCH: "catch";
    "The `CATCH` token.\n\nRepresent the `catch` block of a `try`/`catch` control structure, e.g. `try { … } catch (Exception $e) { … }`."
);
token!(
    pub CLASS: "class";
    "The `CLASS` token.\n\nRepresent the class declaration operator, e.g. `class C { … }`."
);
token!(
    pub CLONE: "clone";
    "The `CLONE` token.\n\nRepresent the clone operator, e.g. `clone $x`."
);
token!(
    pub COALESCE: "??";
    "The `COALESCE` token.\n\nRepresent the null coalescing operator, e.g. `$x ?? $y`."
);
token!(
    pub COMMA: ",";
    "The `COMMA` token.\n\nRepresent the list item separator, e.g. `($x, $y, $z)`."
);
token!(
    pub COMPARE: "<=>";
    "The `COMPARE` token.\n\nRepresent the comparison operator, e.g. `$x <=> $y`."
);
token!(
    pub CONCATENATE: ".";
    "The `CONCATENATE` token.\n\nRepresent the concatenation operator, e.g. `'foo' . 'bar'`."
);
token!(
    pub CONCATENATE_AND_ASSIGN: ".=";
    "The `CONCATENATE_AND_ASSIGN` token.\n\nRepresent the concatenation assignment operator, e.g. `$x .= $y;`."
);
token!(
    pub CONST: "const";
    "The `CONST` token.\n\nRepresent the constant declaration operator, e.g. `const ANSWER = 42;`."
);
token!(
    pub CONTINUE: "continue";
    "The `CONTINUE` token.\n\nRepresent the control flow continuer operator, e.g. `continue 2;`."
);
token!(
    pub DECLARE: "declare";
    "The `DECLARE` token.\n\nRepresent the declaration operator, e.g. `declare(foo='bar');`."
);
token!(
    pub DECREMENT: "--";
    "The `DECREMENT` token.\n\nRepresent the decrement operator, e.g. `$number--`."
);
token!(
    pub DEFAULT: "default";
    "The `DEFAULT` token.\n\nRepresent the default case in a `switch` control structure, e.g. `switch (…) { … default: …; }`."
);
token!(
    pub DIE: "die";
    "The `DIE` token.\n\nRepresent the termination operator, e.g. `die(42);`."
);
token!(
    pub DIVIDE: "/";
    "The `DIVIDE` token.\n\nRepresent the division operator, e.g. `$x / $y`."
);
token!(
    pub DIVIDE_AND_ASSIGN: "/=";
    "The `DIVIDE_AND_ASSIGN` token.\n\nRepresent the division assignment operator, e.g. `$x /= $y`."
);
token!(
    pub DO: "do";
    "The `DO` token.\n\nRepresent the body of a `do`/`while` loop, e.g. `do { … } while (…);`."
);
token!(
    pub DYNAMIC_CALL: "->";
    "The `DYNAMIC_CALL` token.\n\nRepresent the dynamic method call operator, e.g. `$object->method()`."
);
token!(
    pub ECHO: "echo";
    "The `ECHO` token.\n\nRepresent the output writer operator, e.g. `echo 'foobar';`."
);
token!(
    pub ELLIPSIS: "...";
    "The `ELLIPSIS` token.\n\nRepresent the ellipsis operator, e.g. `$x...`."
);
token!(
    pub ELSE: "else";
    "The `ELSE` token.\n\nRepresent the falsy block of a condition control structure, e.g. `if (…) { … } else { … }`."
);
token!(
    pub ELSEIF: "elseif";
    "The `ELSEIF` token.\n\nRepresent a `else if` block, e.g. `if (…) { … } elseif { … } else { … }`."
);
token!(
    pub EMPTY: "empty";
    "The `EMPTY` token.\n\nRepresent the emptiness operator, e.g. `empty($x)`."
);
token!(
    pub ENDDECLARE: "enddeclare";
    "The `ENDDECLARE` token.\n\nRepresent the end of a `declare` block, e.g. `declare: … enddeclare`."
);
token!(
    pub ENDFOR: "endfor";
    "The `ENDFOR` token.\n\nRepresent the end of a `for` block, e.g. `for (…; …; …): … endfor`."
);
token!(
    pub ENDFOREACH: "endforeach";
    "The `ENDFOREACH` token.\n\nRepresent the end of a `foreach` block, e.g. `foreach ($i as $k => $v): … endforeach`."
);
token!(
    pub ENDIF: "endif";
    "The `ENDIF` token.\n\nRepresent the end of an `if` block, e.g. `if (…): … endif`."
);
token!(
    pub ENDSWITCH: "endswitch";
    "The `ENDSWITCH` token.\n\nRepresent the end of a `switch` block, e.g. `switch(…): … endswitch`."
);
token!(
    pub ENDWHILE: "endwhile";
    "The `ENDWHILE` token.\n\nRepresent the end of a `while` block, e.g. `while(…): … endwhile`."
);
token!(
    pub EQUAL: "==";
    "The `EQUAL` token.\n\nRepresent the equality comparison operator, e.g. `$x == $y`."
);
token!(
    pub EVAL: "eval";
    "The `EVAL` token.\n\nRepresent the late-evaluation operator, e.g. `eval($x)`."
);
token!(
    pub EXIT: "exit";
    "The `EXIT` token.\n\nRepresent the termination operator, e.g. `exit(42);`."
);
token!(
    pub EXTENDS: "extends";
    "The `EXTENDS` token.\n\nRepresent the inheritance operator, e.g. `class C extends D { … }`."
);
token!(
    pub FINAL: "final";
    "The `FINAL` token.\n\nRepresent a final entity, e.g. `final class C { … }`."
);
token!(
    pub FINALLY: "finally";
    "The `FINALLY` token.\n\nRepresent the finally block of a `try`/`catch` control structure, e.g. `try { … } catch (…) { … } finally { … }`."
);
token!(
    pub FOR: "for";
    "The `FOR` token.\n\nRepresent a `for` loop, e.g. `for (…; …; …) { … }`."
);
token!(
    pub FOREACH: "foreach";
    "The `FOREACH` token.\n\nRepresent a `foreach` loop, e.g. `foreach ($i as $k => $v) { … }`."
);
token!(
    pub FUNCTION: "function";
    "The `FUNCTION` token.\n\nRepresent the function declaration operator, e.g. `function f(…) { … }`."
);
token!(
    pub GLOBAL: "global";
    "The `GLOBAL` token.\n\nRepresent the global visibility modifier, e.g. `global $x`."
);
token!(
    pub GOTO: "goto";
    "The `GOTO` token.\n\nRepresent the jump operator, e.g. `goto x;`."
);
token!(
    pub GREATER_THAN: ">";
    "The `GREATER_THAN` token.\n\nRepresent the greater than comparison operator, e.g. `$x > $y`."
);
token!(
    pub GREATER_THAN_OR_EQUAL_TO: ">=";
    "The `GREATER_THAN_OR_EQUAL_TO` token.\n\nRepresent the greater than or equal to comparison operator, e.g. `$x >= $y`."
);
token!(
    pub IDENTICAL: "===";
    "The `IDENTICAL` token.\n\nRepresent the strict equality comparison operator, e.g. `$x === $y`."
);
token!(
    pub IF: "if";
    "The `IF` token.\n\nRepresent the truly block of a condition control structure, e.g. `if (…) { … }`."
);
token!(
    pub IMPLEMENTS: "implements";
    "The `IMPLEMENTS` token.\n\nRepresent the implementation operator, e.g. `class C implements I { … }`."
);
token!(
    pub INCLUDE: "include";
    "The `INCLUDE` token.\n\nRepresent the import operator, e.g. `include $x;`."
);
token!(
    pub INCLUDE_ONCE: "include_once";
    "The `INCLUDE_ONCE` token.\n\nRepresent the import once operator, e.g. `include_once $x;`."
);
token!(
    pub INCREMENT: "++";
    "The `INCREMENT` token.\n\nRepresent the increment operator, e.g. `$number++`."
);
token!(
    pub INSTANCEOF: "instanceof";
    "The `INSTANCEOF` token.\n\nRepresent the subset operator, e.g. `$o instanceof C`."
);
token!(
    pub INSTEADOF: "insteadof";
    "The `INSTEADOF` token.\n\nRepresent the conflict resolution operator, `use C, D { C::f insteadof D }`."
);
token!(
    pub INTERFACE: "interface";
    "The `INTERFACE` token.\n\nRepresent the interface declaration operator, e.g. `interface I { … }`."
);
token!(
    pub ISSET: "isset";
    "The `ISSET` token.\n\nRepresent the existence operator, e.g. `isset($x)`."
);
token!(
    pub LEFT_CURLY_BRACKET: "{";
    "The `LEFT_CURLY_BRACKET` token.\n\nUsed to open a block, e.g. `if (…) { … }`."
);
token!(
    pub LEFT_PARENTHESIS: "]";
    "The `LEFT_PARENTHESIS` token.\n\nUsed to open a group of something, e.g. `if (…)`."
);
token!(
    pub LEFT_SQUARE_BRACKET: "[";
    "The `LEFT_SQUARE_BRACKET` token.\n\nUsed to open an array construction or an array access for instance, e.g. `[2, 4, 6, 9][0]`."
);
token!(
    pub LESS_THAN: "<";
    "The `LESS_THAN` token.\n\nRepresent the less than comparison operator, e.g. `$x < $y`."
);
token!(
    pub LESS_THAN_OR_EQUAL_TO: "<=";
    "The `LESS_THAN_OR_EQUAL_TO` token.\n\nRepresent the less than or equal to comparison operator, e.g. `$x <= $y`."
);
token!(
    pub LIST: "list";
    "The `LIST` token.\n\nRepresent the destructuring operator, e.g. `list($x, $y) = $a`."
);
token!(
    pub MODULO: "%";
    "The `MODULO` token.\n\nRepresent the modulus operator, e.g. `$x % $y`."
);
token!(
    pub MODULO_AND_ASSIGN: "%=";
    "The `MODULO_AND_ASSIGN` token.\n\nRepresent the modulus assignment operator, e.g. `$x %= $y;`."
);
token!(
    pub MULTIPLY: "*";
    "The `MULTIPLY` token.\n\nRepresent the multiplication operator, e.g. `$x * $y`."
);
token!(
    pub MULTIPLY_AND_ASSIGN: "*=";
    "The `MULTIPLY_AND_ASSIGN` token.\n\nRepresent the multiplication assignment operator, e.g. `$x *= $y;`."
);
token!(
    pub NAMESPACE: "namespace";
    "The `NAMESPACE` token.\n\nRepresent the namespace declaration operator, e.g. `namespace N;`."
);
token!(
    pub NEW: "new";
    "The `NEW` token.\n\nRepresent the instanciation operator, e.g. `new C()`."
);
token!(
    pub NOT_EQUAL: "!=";
    "The `NOT_EQUAL` token.\n\nRepresent the not equal comparison operator, e.g. `$x != $y`."
);
token!(
    pub NOT_IDENTICAL: "!==";
    "The `NOT_IDENTICAL` token.\n\nRepresent the strict not equal comparison operator, e.g. `$x !== $y`."
);
token!(
    pub OR: "or";
    "The `OR` token.\n\nRepresent the inclusive disjunction operator, used in a logical expression, e.g. `$x or $y`."
);
token!(
    pub POW: "**";
    "The `POW` token.\n\nRepresent the power operator, e.g. `$x ** $y`."
);
token!(
    pub POW_AND_ASSIGN: "**=";
    "The `POW_AND_ASSIGN` token.\n\nRepresent the power assignment operator, e.g. `$x **= $y;`."
);
token!(
    pub PRINT: "print";
    "The `PRINT` token.\n\nRepresent another output writer operator, e.g. `print 'foobar';`, see `echo`."
);
token!(
    pub PRIVATE: "private";
    "The `PRIVATE` token.\n\nRepresent the private visibility operator, e.g. `private $x`."
);
token!(
    pub PROTECTED: "protected";
    "The `PROTECTED` token.\n\nRepresent the protected visibility operator, e.g. `protected $x`."
);
token!(
    pub PUBLIC: "public";
    "The `PUBLIC` token.\n\nRepresent the public visibility operator, e.g. `public $x`."
);
token!(
    pub REFERENCE: "&";
    "The `REFERENCE` token.\n\nRepresent the reference operator, e.g. `&$x`."
);
token!(
    pub REQUIRE: "require";
    "The `REQUIRE` token.\n\nRepresent the import operator, e.g. `require $x;`."
);
token!(
    pub REQUIRE_ONCE: "require_once";
    "The `REQUIRE_ONCE` token.\n\nRepresent the import once operator, e.g. `require_once $x;`."
);
token!(
    pub RETURN: "return";
    "The `RETURN` token.\n\nRepresent the return operator, e.g. `return $x;`."
);
token!(
    pub RIGHT_CURLY_BRACKET: "{";
    "The `RIGHT_CURLY_BRACKET` token.\n\nUsed to close a block, e.g. `if (…) { … }`."
);
token!(
    pub RIGHT_PARENTHESIS: ")";
    "The `RIGHT_PARENTHESIS` token.\n\nUsed to close a group of something, e.g. `if (…)`."
);
token!(
    pub RIGHT_SQUARE_BRACKET: "]";
    "The `RIGHT_SQUARE_BRACKET` token.\n\nUsed to close an array construction or an array access for instance, e.g. `[2, 4, 6, 9][0]`."
);
token!(
    pub SEMICOLON: ";";
    "The `SEMICOLON` token.\n\nRepresent the end of an instruction, e.g. `$x = …;`."
);
token!(
    pub STATIC: "static";
    "The `STATIC` token.\n\nRepresent the stack declaration operator, e.g. `static $x`."
);
token!(
    pub STATIC_CALL: "::";
    "The `STATIC_CALL` token.\n\nRepresent the static method call operator, e.g. `class::method()`."
);
token!(
    pub SUBTRACT: "-";
    "The `SUBTRACT` token.\n\nRepresent the subtraction operator, e.g. `$x - $y`."
);
token!(
    pub SUBTRACT_AND_ASSIGN: "-=";
    "The `SUBTRACT_AND_ASSIGN` token.\n\nRepresent the subtraction assignment operator, e.g. `$x -= $y;`."
);
token!(
    pub SWITCH: "switch";
    "The `SWITCH` token.\n\nRepresent the switch control structure, e.g. `switch ($x) { … }`."
);
token!(
    pub TERNARY_ELSE: ":";
    "The `TERNARY_ELSE` token.\n\nRepresent the falsy block of a ternary condition, e.g. `$x ? … : …`."
);
token!(
    pub TERNARY_THEN: "?";
    "The `TERNARY_THEN` token.\n\nRepresent the truly block of a ternary condition, e.g. `$x ? … : …`."
);
token!(
    pub THROW: "throw";
    "The `THROW` token.\n\nRepresent the throw exception operator, e.g. `throw $e;`."
);
token!(
    pub TRAIT: "trait";
    "The `TRAIT` token.\n\nRepresent the trait declaration operator, e.g. `trait T { … }`."
);
token!(
    pub TRY: "try";
    "The `TRY` token.\n\nRepresent the `try` block of a `try`/`catch` control structure, e.g. `try { … } catch (Exception $e) { … }`."
);
token!(
    pub UNSET: "unset";
    "The `UNSET` token.\n\nRepresent the destruction operator, e.g. `unset($x);`."
);
token!(
    pub USE: "use";
    "The `USE` token.\n\nRepresent the importing operator (for namespaces or traits for instance), e.g. `use C\\D;`."
);
token!(
    pub VAR: "var";
    "The `VAR` token.\n\nRepresent the variable declaration operator (for old PHP versions), e.g. `var $x = …;`."
);
token!(
    pub VARIABLE: "$";
    "The `VARIABLE` token.\n\nRepresent the variable declaration operator, e.g. `$foo`."
);
token!(
    pub WHILE: "while";
    "The `WHILE` token.\n\nRepresent a `while` loop, e.g. `while (…) { … }`."
);
token!(
    pub XOR: "xor";
    "The `XOR` token.\n\nRepresent the exclusive disjunction operator, used in a logical expression, e.g. `$x xor $y`."
);
token!(
    pub YIELD: "yield";
    "The `YIELD` token.\n\nRepresent the generator operator, e.g. `yield …;`."
);
token!(
    pub YIELD_FROM: "yield from";
    "The `YIELD_FROM` token.\n\nRepresent the delegated generator operator, e.g. `yield from …;`."
);
