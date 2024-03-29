# SAPL Language Information

<img src="https://image.freepik.com/free-vector/cute-business-llama-icon-illustration-alpaca-mascot-cartoon-character-animal-icon-concept-isolated_138676-989.jpg"
     alt="sapl icon"
     style="width: 100px; border-radius: 5px" />


## Purpose

Mainly designed to learn Rust. The idea was to create a modern preprocessor language. So essentially something that can be used to provide backend processing to HTML similar to PHP. Could also be used as a preprocessor of sorts to other languages? Right now it's just as interesting language I suppose.

## Basics

SAPL is a dynamically typed interpreted language with roots from OCaml, Rust, C++, and Python. It is closely tied to functional languages, and as such immutability is the default.

## Expressions and Definitions

SAPL code is divided into two classes, expressions and definitions. Really a better name for definitions would be non-expressions. Roughly speaking an expression is something that produces a value while a definition does not. All expressions can be part of a definition, but definitions cannot be part of an expression. Definitions include `let` and function definitions, and loops. Expressions include everything else. In neither category are sequences, which are a series of expressions or definitions. The hierarchy is as follows:

```
code
|   sequences
|
|__ definitions
|   |   let definitions
|   |   functions
|   |   loops
|   |   imports
|   |   struct and type definitions
|  
|__ expressions
|   |   operators, literals, if, try ...
|   |   basically everything else
    

```

Definitions actually do produce a value. That value is `unit` which is essentially the "no value" value. `unit` is also produced by loops and if blocks that do not have an else branch.

## Names

A valid name contains alphabetic ascii characters with underscores and optionally followed by numbers. It must not be a keyword or name of a type. Names may also contain `::` which denote namespaces

Ex. 

* `my_var`
* `HELLO_WORLD`
* `MyClass`
* `variable13`
* `game::entity`

There is a special name: `_`. The underscore can be used anywhere a name can be used (as it is a valid name), however it has the special property that it cannot be bound into the environment. Thus it can be used to ignore a certain value where syntactically, a name is required.

I've adopted the convention of snake_case for variable/constant names and function names.

```Javascript
let _ = 10;
// 10 is not saved anywhere, instead the value is ignored
```


## Types

* *some*
    * *number*
        * int
        * float
    * string
    * bool
    * array
    * *tuple*
        * tuple_n
            * where n indicates the tuple size
            * ex. `tuple_3` is a 3-tuple
    * map
    * function
    * range
    * *object*
        * *`TypeName`*
            * `StructName`
            * Structs are their own type name
            * Types (interfaces, denoted with the `type` keyword in sapl) are their own meta type name
    * ref
* *none*
    * unit

Types are divided into two classes, `some` types and `none` types. Every type, with the exception of `unit`, is a `some` type. `int` and `float` are also part of a smaller subclass of types called `number`. Essentially, every value in sapl is a variant that can take on one of the enumerated types or be empty. Type names in italics are *meta-types*, or type categories and not actual types themselves. Meta-types can be used to type checking as a type that is part of a meta-type *is* that meta-type. For example, a `float` *is* a `number`.

A string literal can be defined with single or double quotes. String literals can contain new lines.

Literal productions are as follows:

`<string> ::= '...' | "..."`

`<tuple> ::= (<expr1>, <expr2>, <expr3>, ...)`

`<array> ::= [<expr1>, <expr2>, ...] | []`

`<map> ::= {<string1>: <expr1>, <string2>: <expr2>, ...} | {}`

`<range> ::= <expr1> .. <expr2>`

`<bool> ::= true | false `

`<unit> ::= None`

`<ref> ::= &<expr> | &&<expr>`



## Basic Operators

The following are pretty standard operators that work as you'd expect:

* Arithmetic operators: `+`, `-`, `*`, `/`, `%`
* Short-circuit operators: `&&`, `||`
* Comparison operators: `==`, `>`, `<`, `<=`, `>=`, `!=`

* `**` is the exponentiation operators
    * If passes two integers, it will return an integer
    * If passed a single float, or the exponent is negative it will return a float

* `!` is the pre-expression unary negation operator
    * It comes before the expression it's being applied to
    * If applied to a bool it returns the opposite
    * If applied to a number (int or float) it returns the negation of that number

* `?` is the post-expression unary valid check operator
    * It comes after the expression it's being applied to
    * If applied to an expression that returns a value, it returns `true`
    * If applied to an expression that raises an exception, or a evaluation error, it returns `false`

* `@` is the array and map concatenation operator
    * To avoid confusion, with arrays `@` adds an element onto an array while `+` merges two arrays into one
    ```BNF
    <array> ::= <array> @ <expr1> @ <expr2> @ ... |
                <array> + <array> + ...
    ```

    ```BNF
    <map> ::= <map> @ <tuple_2> @ <tuple_2> @ ...       |
              <map> @ [<tuple_2>, <tuple_2>, ...] @ ... |
              <map> @ <map> @ ...
    ```

* `is` is the type check operator
    * It evaluates its left branch and requires a name on it's right branch which is interpreted as a type or *meta-type*
    * *meta-types* are not types themselves, but categories of types
    * `<expr> is <name>`
    ```Javascript
    5 is some //true, some meta-type
    (10, 23) is tuple_2 // true
    {'key': 'value'} is array //false
    (10, 'Test', [50, 10]) is tuple //true, tuple meta-type
    ```

* `as` is the type conversion operator
    * It evaluates its left branch and converts it to the type denoted by the name in its right branch
    * `<expr> as <name>`
    * Anything, except functions, can be converted to a `string` by pretty-printing it
    * Numbers to bool
        * 0 is false, everything else is true
    * Bool to numbers
        * false is 0, true is 1
    * Numbers to string, parses the string as the respective numeric type
    * Tuples and arrays are convertible
    * A map can be converted to an array by appending each key value pair as a tuple in an array
        * `{'key1': 1, 'key2': 'val'} as array == [('key1', 1), ('key2', 'val')]`
    * An array can be converted to a map by making each string representation of the element's index its key
        * `[1, 2] as map == {'0': 1, '1': 2}`

* `==` and `!=` compare by *structural equality*
    * No types are converted, expect for references which check wether the data they point to is equal to the data of the other comparator
    * Therefore `0` stored as an `int` is not equal to `0` stored as a float since no types are converted
    * Built in functions cannot be compared. This also includes member functions of an object, which are wrapped in a function implemented in Rust

* `..` is the binary range operator
    * It constructs a range from its two arguments



### Implicit Conversions

* Operator `+`:
    * If either operand is a string, both operands are converted to strings and the result is the concatenation of those strings
    * If both operators are arrays, create a new array that is the first followed by the second
    * Otherwise follow arithmetic operator conversion rules

* Arithmetic Operators:
    * If operands are a mix of floats and ints, convert both to floats
    * `**` -> if exponent operand is negative int, return a float
    * Otherwise, do not convert and apply operator to operands
        * Thus division of two integers truncates the result if it is not an integer

* Short circuit boolean operators:
    * Convert both to booleans and apply the operator, short circuiting when possible

    * Boolean conversions:
        * booleans stay the same
        * non-zero integers are `true`, `0` is `false`
        * non-zero floats are `true`, `0` is `false`
            * What's considered `0` is any float that is less than `epsilon` from `0`
        * non-empty strings, arrays and maps are `true`, empty strings, arrays, and maps are `false`
        * valid ranges (composed of two unequal integers) are `true`, invalid ranges are `false`
        * functions and tuples are `true`
        * unit is `false`

### Operator Precedence

We haven't talked about all of the operators yet, but here is the list. Lower numbers have higher precedence

1. `.` 
2. `&`, `&&`, `*` (unary ref and deref), `include`
3. `[]`, `!`, `?`, `-` (unary minus)
4. `as`, `is`
5. `**`,
6. `*`, `%`, `/`, `&` (bitwise and)
7. `+`, `-`, `|` (bitwise or)
8. `==`, `!=`, `>`, `<`, `<=`, `>=`
9. `&&`
10. `||`,
11. `..`, `@`
12. `|>`
13. `return`, `throw`
14. `=`, `<-`

Operators on the same precedence level have left association. This means that pre-expression unary operations take precedence over post-expression unary operators

### Examples

```Javascript
'Hello ' + 'World' // "Hello World"
42 // 42
42 - -20 // 62
10+20-3*4 // 18
true || 10 / 0 > 0 // true
false && 5 / 0 == 0 // false
10 ** 3 * 4 == 100 // false
4.001 > 2 ** 2 // true
'Windows' > "Doors" // true
!2 // -2
!false // true
undeclared_variable? // false
10 ** -2 // 0.01
10 % 4 + 20 * 3 // 62
20 - 3 + 10 - 23 + 5 - 4 * 3 // -3
10 ** 3 / 3 // 333
```

## Constants and Variables

```BNF
<constant> ::= let <name> = <expr>  |
               let <name_1>, <name_2>, ... <name_n> = <tuple_n>
<variable> ::= let var <name> = <expr>  |
               let var <name_1>, var <name_2>, ... var <name_n> = <tuple_n>
```

In this case `<name>` is the name of the variable or constant. For a variable, I was debating the syntax `var <name> = <expr>` but I decided against it because I felt it makes creating variables too easy.

Structured bindings can be used to unpack a tuple. There must be the same number of names as elements in the tuple.


Examples:
```Javascript
let my_num = 10;
let person, age = ('John', 53);
let p2, var age2 = ('Kathy', 40);

let var variable = 20
```

### Shadowing and Name Lookup

Works as you'd expect in most languages in most cases

```OCaml
let name = 20;
let name = "Jill";
name
```
The result is "Jill"


Order of name lookup:
1. Check the environment for an exact match
2. Check environment for variables in the `export` *namespace*
    * Variables are put in the `export` namespace when they are declared `pub`

I'll refer to this as *free name* lookup to differentiate it with *dot name* lookup which we'll talk about later.


## Control Flow

In SAPL, blocks can either be denoted with braces or a colon
```BNF
<if> ::=  
    if <expr1>: <code> else <expr>    |

    if <expr1>: <code> else: <expr>   |

    if <expr1>:
        <code>
      else {
        <code>
    }                                 |

    if <expr1> {
        <code>
    } else {
        <code>
    }
```

An if block will convert any non-bool value to a boolean in the guard expression during evaluation. No other conversion is done implicitly. An if block without an else where the guard is not satisfied produces the `unit` value. If blocks can also be nested within an else branch to create an `else if`. However, the parser reads this as a whole new `if` block within the else branch of an outer if.

Also take note that without braces, the else branch **may only** contain an *expression*, while with braces present it can contain any *code*.

Examples:

```Javascript
if 10 == 20: 'Hello' else 'Bye' // "Bye"

if 10 < 5: 0
else if 10 == 9.0: 1
else if 3 == 3: 5
else 10
// 5

if 10 == 20:
    let x = 20;
    let y = 30;
    x + y
else {
    let x = 40;
    let y = 50;
    x + y
} // 90


if 10 * -10 > -10:
    0
else if 40 > 300:
    0
else if 10 + '' == '10':
    // concatenation results in  a string
    if 'cat' != true {
        // equality checking does not convert types
        if 'apple' < 'banana':
            (10 + 3) ** 4 % 19
        // Braces are needed here otherwise the else would be part of the above if
    } else 
        (10 + 4) ** 3 % 17

// 4

if '': 0 else 10 // 10
// empty string is converted to false

if []: 10 // unit -> since the empty array converts to false

let max = if x < y: y else x
// max is, well the max of x and y
```

#### *Design Decision*:
I wanted if statements to be able to be neatly inlined (like the last example). Thus I created the option to use `:` or braces. This can lead to some confusion of where to use braces or a colon. My advice is use a colon if the block **only** contains a *single* expression or definition and there is no nesting. Use braces otherwise. More formally, braces are required when there is no other terminal token, and the block contains more than a single expression. For example the `else` branch of an `if` may only contain an expression unless braces are used. Consider the following:

```Javascript
let favorite_color = if age <= 12: 'red' else 'sky blue';
let favorite_food = if age < 10: 'spaghetti' else 'Risotto alla Milanese';
//...
```

If the `else` branch could contain more than just a single expression without braces, then there would be no way to tell if you mean what was written above or the following:

```Javascript
let favorite_color = 
    if age <= 12:
        'red'
    else
        'sky blue';
        let favorite_food =
            if age < 10:
                'spaghetti'
            else
                'Risotto alla Milanese'
//...
```
Take `age = 18`, then the first code snippet would bind `favorite_color = 'sky blue'` and `favorite_food = 'Risotto alla Milanese'` while the second one would bind `favorite_color = unit`. `favorite_food` would only be bound in the scope of the `else` branch and thus be popped off the stack by the "..."

Moreover, an `if` branch can only contain a single expression if `:` is used without an `else`. This is because the `else` serves as a sentinal symbol for the `if` block, and without it there would be problems similar to the one demonstrated above. Once again, my advice is to use braces unless you are inlining the expression.

### Try




```BNF
<try> ::=
    try
        <code1>
    catch <name>:
        <expr> 
    
    | 

    try {
        <code1>
    } catch <name> {
        <code>
    }

    |

    try:
        <code1>
    catch <name>:
        <expr>
```
Once again, try/catch blocks can use braces or a colon (or neither for the try). A `try` block **must** always be followed by a `catch` block, therefore, there is never a time when you need braces for the try block since `catch` will serve as a sentinel token. However, like the `else` block, a `catch` block must either have braces or contain only a single expression.

`<name>` is the name of the variable that is bound to the exception returned in the catch block if an exception is caught. If no exception is throw, the value of `<code1>` is returned. Otherwise, the thrown exception is bound to `<name>` and `<code>` is evaluated and the result (which can be another exception) is returned

An exception can be thrown with the `throw` unary operator. Any value can be thrown. A thrown value is wrapped in an exception and is either thrown out of the program (in which case execution stops), or is caught and bound in the catch block. There are two types of runtime errors in SAPL programs. Exceptions, and evaluation errors. Generally, the distinction is that exceptions can be created by broken invariants (such as divide by zero exceptions, an invalid operation applied on a variable etc) while evaluation errors are errors caused by the programmer (such as an undefined variable). Evaluation errors cannot be raised manually, and cannot be caught. Most of the runtime errors are exceptions.

However, as mentioned earlier, evaluation errors can be "caught" by the `?` operator which in that case would return false. The use of this operator will be revealed shortly

Examples:
```Javascript
let res = try 10 / 0 catch x: x
// res is a string "Divide by zero exception"

try {
    throw 20 + 10
} catch result {
    result + 10
}
// 40

try {
    'Hello' * 3
} catch x:
    'Exn caught'

// "Exn caught"
```

As with many languages, the if branches and try/catch blocks each have their own scope

```Javascript
let name = 'K';

try
    let name = 'P';
    throw name
catch x {
    name + x
}
// KP
```

As you are probably noticing, the colon may be omitted for things that contain a single keyword such as `else` or `try`. I wouldn't suggest it because it seems kind of inconsistent.

## Sequences

```BNF
<sequence> ::= (<expr1> | <defn1>); (<expr2> | <defn2>); ... (<expr> | <defn>)
```
Sequences are denoted with the `;` operator. Any expression or definition followed by a `;` indicates that there is a continuation. During evaluation, only the value produced by the final expression is returned. All earlier values are discarded unless an exception or evaluation error is produced or the `return` operator is used.

It's important to note that the last line in a block **cannot** have a semicolon. This is because the semicolon tells the interpreter that more code is coming in the block. What is a block? Well, same as most coding languages. Basically any area where `<code>` can occur that has it's own scope. This includes the branches of a conditional, loops, functions, etc.

```Rust
100;
50 + " Hello";
let school = 'HHS';
school
// Returns "HHS", the prevents expressions in the sequence are evaluated, but the result is ignored


10 / 0;
100
// Divide by zero exception

let var count = 0; //semi colon because there is a continuation
for i in 0 .. 10 {
    let num = i * 100 - i;
    count = count + 1 //No semicolon since this is the last line in the block
}
// no semicolon since the for loop ends the block

```

### Sequence Elision

I didn't like the look of a semicolon following a brace so I decided to create a few exceptions where the sequence operator can be elided. However, it's important to know that these exceptions are part of a sequence even though they don't require a semicolon. Putting a semicolon would not be wrong however.

Sequence elision occurs following any definition (`try`, `while`, `for`, `struct`, `type`, `fun`, etc.) with the exception of `let` and `import` definitions.

An if or try block within a `let` definition would still require a semicolon because let definitions are not allowed to elide the sequence operator.

Examples:
```Javascript
if 10 + '' == '10':
    0 
    //One again note how this 0 does not have a ; since its the last line in the if branch's block
else
    'No semicolon'

if true {
    false
} else {
    'No semicolon'
}

try
    10 / 0
catch _:
    'Error'

let person = if 10: 'Jackie' else 'Joey';

if person == 'Jackie' {
    let details = person + ' from Wisconsin';
    details
} else 0
```
All of these expression are part of one single sequence. Notice that *sequence elision* works even when no braces are used. Just for clarification, none of the above blocks require the use of braces. I just showed them for syntactic variation to show the different varieties.

## Comments

Single line comments with `//` or multiline comments with `/* */`

# Functions

```BNF
<function> ::= fun <name> <arg1> <arg2> ... [-> <expr>] { <code> } |
               fun (<arg1> <arg2> ...) <expr>                     |
               fun (<arg1> <arg2> ...) { <code> }

<arg> ::= [var] <name> | ([var] <name> : <expr>)
```

A function can be defined by either a function definition or function expression (lambda). A function definition **must** have braces but a lambda optionally may not. If a lambda does not use braces, it **may only** contain an expression. Lambdas are unnamed.

Since `<function>` is within `<code>`, functions can be nested.

Examples:
```Kotlin
fun add x y {
    x + y
}

add(10, 20); // 30
add('Hello', ' world'); // "Hello World"

fun double f x {
    f(x) + f(x)
}

double(add, 10) // 40
```

As you can see, functions are themselves values, and can be passed to other functions and stored in variables.

```OCaml
let lambda1 = fun (a b) a + b;
let lambda2 = fun () {
    if lambda1? {
        100 - 10 * 3 ** 2
    } else {
        20
    }
}

```

## Function Capture

Both lambdas and functions capture all variables by copying any used variable/constant into their own environment.

```OCaml
let num = 20;

fun func {
    num + 20
}

let num = 30;
func() 
```
The original variable num is captured and the result is 40

```OCaml
let z = 10;
let lambda = fun (x y) x ** (y + z);
lambda(2, 2)
```
Results in 4096

Variable capture occurs at parse time for free names. This is called *static capture*. However *dynamic capture* occurs at runtime for object member functions. For member functions, the name `self` is dynamically captured and is an object that stores the current state of the object.

```Rust
struct Machine {
    def id = 0

    fun Machine id {
        self.id = id
    }

    pub fun get_id {
        self.id
    }
}

let mach = Machine(10);
mach.get_id()
//will be 10 because id is bound at runtime and not 0, which
//is the inital value
```

## Partial Application

Partial application uses placeholders like C++'s `std::bind`. The placeholder is denoted with `?`. During a partial application, a new function is created with a new environment. The expressions passed to the function are evaluated and the result is copied into this new environment. You can partially apply any parameters in any location, the resulting function will take these unknown parameters in the same relative order to each other.

```OCaml
let func = fun (x y) x + y;
let f2 = func(?, 10);
f2(5)
```

```OCaml
fun quad a b c d {
    a - b + c - d
}

let q1 = quad(?, 10, ?, 0);
let q2 = q1(?, 5);
let q3 = q2;
q3(3)
// 3 - 10 + 5 - 0


fun log level msg code {
    "[LOG (" + level + ")]: " + msg + " {" + code + "}"
}
let debug = log("DEBUG", ?, ?);
debug("Got Here!", 1) //"[LOG (DEBUG)]: Got Here! {1}"


fun do_stuff x y z {
    x - y / z
}

let add_dbl = do_stuff(10, ?, 30);
add_dbl(60) //10 - 60 / 30 = 8
```

## Pipelining

Functions can be pipelined like OCaml and other functional languages with the `|>` operator. During pipelining, a function can be *implicitly partially applied* if not all arguments are specified. During implicit partial application the first available parameters are bound first.

```OCaml
fun triple a b c {
    a + b * c
}

let double = 10 |> triple;
double(5, 5) //35
```

```OCaml
fun sub x y {
    x - y
}

fun inc x {
     x + 1
}

fun mul x y {
    x * y
}

fun dbl_call f x {
    f(x) + f(x)
}

10 |> sub(?, 4) |> inc
   |> mul |> dbl_call(?, 10)

// ((10 - 4) + 1) * 10 + ((10 - 4) + 1) * 10
// 140
```

## Recursion

For functions, recursion works simply as you'd expect.

```OCaml
fun summation start end {
    fun sum_helper i {
        if i < end:
            i + sum_helper(i + 1)
        else end
    }
    sum_helper(start)
}
summation(0, 100) //5050
```

However, since lambdas are unnamed, recursive lambdas would refer to themselves with the `this` keyword. Technically speaking, `this` is not a keyword but rather a name that is captured in the lambda's closure which refers to itself.

```OCaml
let countdown = fun (x) {
    if x <= 0: 0
    else {
        x + this(x - 1)
    }
};
countdown(10) //55
```

## Postcondition Expressions

Function definitions (not lambdas) can have a *postcondition expression* following an `->` after the function arguments (if any). A postcondition expression is evaluated after the function produces a value. If the postcondition returns true, or if it only contains a name and that name is valid, the result is returned from the function. If a postcondition produces any other value, exception, or error, the functions returns a postcondition violated exception. Note this means that **any runtime error** in the postcondition expression is treated as a failed postcondition and produces a new postcondition violated exception.

The postcondtion expression is evaluated in the function's captured environment. However a few new names are bound in the postcondition's environment.

* `result` is always bound in the postcondition expression's environment to the result of the function
* the name of the result's type is bound, and all meta-types that type satisfies
    * ex. `int` will be a constant with the value of the function's result if that result is an integer. In this examples `number`, and `some` will also be bound to the same value.

This means that means that a postcondtion containing the name `string` will cause an undeclared variable exception (and thus the postcondition will be violated) if the function actually produces a range.

This is why you cannot create names that are types because the types are used as variable names to indicate the type of the result.

```OCaml
fun get_name x -> string {
    return 'Joe'
}
```
When `get_name` is executed, the value it produces is a string, and thus 'Joe' will be bound to `string` in the postcondition environment. Since this postcondition contains only a name, and that name is valid, the postcondition is satisfied.

```OCaml
fun func -> bool {
    false
}
```
In this example, `false` is bound to the name `bool` in the postcondition expression (henceforth referrred to as PCE) environment. But since the PCE is just a name, the PCE will become true since `bool` is a valid name.

```OCaml
fun func -> bool {
    'Hello'
}
```
Here, the PCE evaluates to false because the name `bool` is undeclared since the result is a string and thus bound to the name `string`




```OCaml
fun func -> bool || string {
    false
}
```
Slightly confusingly, this PCE is false because the value `false` is bound to the name `bool`. Since the PCE contains more than just a single name, it evaluates the expression instead of just checking to see if the expression is valid. This is where the `?` operator can come in:

```OCaml
fun func -> bool? || string? {
    false
}
```
Now this postcondition is satisfied because the PCE uses the `?` operator to check if `bool` and `string` are valid.


As I previously mentioned, invalid PCE's will result in a failed postcondition. This allows you to ensure that the result implements certain *concepts*.

```OCaml
fun get_add x -> result + result {
    if x == 0: "Hello "
    else if x == 1: 10
    else if x == 2: 3.14
    else 10
}
get_add(0) + get_add(1) + get_add(2)
// "Hello 103.14"
```

This postcondition ensures that whatever is returned from `get_add` can be added to itself. Notice that the PCE `result + result` is equivalent to `(result + result)?` since any error in the PCE is treated as the PCE evaluating to `false` (failed postcondition). Mind you, this is only true in postcondition expressions, this is not true in say, an if guard, as the exception would cause execution to stop.


```OCaml
fun tie x y -> tuple_2 {
    return (x, y)
}
let x, y = tie(10, 20);
x + y //30
```

Postcondition expressions also take part in the name capture of the function:

```OCaml
fun valid_name str -> bool {
    let names = [
        "Billy",
        "Jackson",
        "Persephone"
    ];
    names.contains(str)
}

fun meet person -> valid_name(string) {
    person.name
}

let p = {name: "Jackson", age: 10};
meet(p)
```
`valid_name` is only used in `meet`s PCE, but it is captured.

### Precondition Annotations

Let's take one look at postcondition expression again:

Without PCE:
```Kotlin
fun func x {
    if x == 0 {
        let result = /*computation*/;
        assert(result > 0);
        result
    } else if x == 'h' {
        let result = /*computation 2*/;
        assert(result > 0);
        result
    } else if x == [] {
        let result = /*computation 3*/;
        assert(result > 0);
        result
    }
    //...
}
```

With PCE
```Kotlin
fun func x -> number > 0 {
    if x == 0:
        /*computation 1*/
    else if x == 'h':
        /*computation 2*/
    else if x == []:
        /* comp 3 */
    //...
}
```

Now you might say: "but wait, I'm a good programmer and I can factor out that repeated assertation code into a separate function!" And you'd be right. But I think this is cleaner, and quite frankly, it's my language and I can do what I want! Plus, you're almost certainly me and at this point I'm just talking to myself in writing. So be quiet and let me enjoy my postcondition expressions!

```OCaml
 let max = fun (x y) if x > y: x else y;
let min = fun (x y) if x < y: x else y;

fun gcd x y -> int >= 1 {
    assert(x >= 0 && y >= 0);
    // Check precondition

    let max = max(x, y);
    let min = min(x, y);
    if x <= 1 || y <= 1:
        max
    else
        gcd(max - min, min)
}
```

Let's look at precondition annotations. If a variable is enclosed in parenthesis, a precondition annotation can be placed in these parenthesis following a colon. The rules work exactly like a post condition expression, except, instead of always binding `result` along with any type variables, it also binds `arg`. The preconditions are checked one a value is bound to that parameter. Therefore, preconditions are checked during partial application (for the arguments specified)

```Rust
fun add (x: number) (y: number) -> number {
    x + y
}

add(20, 30.0) +
    try: add('Hello', " there") catch _: 0
// 20 + 30.0 + 0
```

```Rust
fun is_valid_param x {
    x is int
}

fun add (x: (arg + arg)?) (y: is_valid_param(arg)) {
    x + y
}

add("Hello ", 30) +
    try: add("Hello", 54.0) catch _: 0
// Hello 300
```

## Return keyword

`return` is not needed, but it can be used to cause code execution to stop early. Specifically what happens is it causes the value to bubble up from the AST during evaluation until it hits the first function node. Then the return value is converted into a normal value and is the result of the function. Basically, it works like `return` in C++ and Java and other imperative languages.

```Kotlin
fun test_fun x {
    if x == 10 { return x };
    x * x
}

test_fun(10) + test_fun(8)
//74
```

---

# References

References are kind of poorly named. Behind the scenes all references are **reference-counted shared smart pointers**. Thus, dead references aren't possible (ideally). There are two types of references, though they share the same type, mutable and immutable. An immutable reference is defined with `&` while mutable uses `&&`. All values can be references, including fully evaluated ones.

Only variables can have mutable references taken. Basically, anything under a constant value cannot be mutated. When applying the reference operator to a literal, such as `10`, the operations creates a new reference that refers to its own copy of that value. This becomes akin to using the `new` operator in C++ or Java. If you take the reference of a name or another reference, the data will be shared.

To derefence, use the `*` operator and to assign to the value of a reference, use the `<-` operator.

```Rust
let var num = 10;
let num_alias = &&num;

num_alias <- 20; //mutate the reference, but `num_alias` itself is still constant
num = num + 20;

*num_alias //dereference and get the value 40
```

```Rust
let var lst = ['Hello'];
let var lst_view = &lst;

lst.push_back('World');
lst_view.contains('World') // true

*lst_view //['Hello', 'World']

lst_view.push_back('Bye') //error, lst_view is immutable ref, even though it itself is a variable

lst_view = &&lst; //ok since lst_view is a variable and lst is mutable itself
```

```Rust
let var lst = &&10; 
//create a reference that owns a copy of the value 10
lst <- 20;
*lst + 10 //30
//The reference of lst is now 20, obviously the value 10 is unaffected

```

## Weak References

Weak references are not something you as a user must know about. They exist temporarily and ~~can~~ should [^1] never cause a dead reference error. They are automatically derefenced and their value's copied when used outside the limited area they can exist. For example, the result of the dot operator is a weak reference. This allows mutable values to be mutated with the `=` operator. However the second they are used for something else besides updating or lookup, (such as assignment), their values are copied.

To keep the weak reference around as a reference (instead of copying), you can use the `&` or `&&` to promote it to a full reference.

All non-reference specific operators which apply to references apply to weak references as well with the same semantics (ex. `.`, `()`, `[]`, etc.)

```Rust
let var obj = MyObj();

obj.my_num = 3; //use weak reference to update the data
5 + obj.my_num //8,  weak reference is copied

let my_ref = &&obj.my_num; //weak reference promoted
my_ref <- 20;
5 + obj.my_num // 25

obj.my_func(20) //function application on weak reference works like normal reference
obj.child.child.next.get() // chain and function app of weak references

```

[^1]: I wouldn't be so naive and say the code is bug free


## Reference Operators and Mutability

* `<-` Is the reference update operator
    * It changes the value behind a mutable reference
    * It continually applies itself until a non-reference is found, and updates the value of that non-reference

* `*` Is the dereference operator
    * Like `<-`, it will continue to apply itself until a non-reference is found

* `.`, `()`, `[]`, and `==` can be applied to references.
    * When applied to references, they continue to apply themselves until a non reference is found
    * then they apply themselves to the non-reference

When dealing with chains of references/variables. Any single immutable value renders the entire chain immutable. For example, a mutable reference in an immutable object means that the reference is also immutable when attempted to be updated through the immutable object. In general, mutability rules enforce that an immutable value cannot have its data changing out from under it.

## Auto Dereferencing

As you saw, there are a few situations where references automatically dereference themselves. These cases do not invoke a copy of the data. This occurs in indexing with `[]`, getting members/applying functions with dot syntax and invoking a function with `()`.

With the dot operator, what specifically happens for dot-syntax-applied (*bound form*) functions is that the value on the LHS of the dot is passed as a reference to the first argument of the function on the right. If the LHS is mutable, a mutable reference is passed otherwise an immutable one is passed.

The other case of auto-dereferencing is *reference-collapsing*. You cannot have a reference to a reference externally. Internally, there are cases where this occurs but from the user's point of view this can never happen. If this occurs, the outer reference just becomes a direct reference to the data. Therefore, you will never need a chain of dereference operators.

```Rust
let num = &(&&5);
*num // 5
// notice we don't do **num

num <- 10 // error
// num is an immutable reference to 5, 
//the inner mutable reference has been collapsed
```

It is safe to think of such reference collapsing situations as references to references internally and that all ways to manipulate the reference (`<-`, `*`, `.`, `[]`, etc.) simply keep applying themselves like C++'s `->` operator until a non-reference is reached. In fact, that's probably the best way to think about it as we'll see situations where there does indeed seem to be references to references.

## Pass by reference

Like C++, the default is *pass-by-value*. Thus, to avoid excess copies, data must be explicitly passed by reference


# Standard Library
## Built-in functions and Dot operator

Built-in or hardcoded functions are hardcoded in the interpreter. I'll talk about the global namespace functions here but every function can be called in *free-form* syntax or *bound* syntax. That is the dot operator can be used to pass the first parameter as a reference to the function.

Ex. `val.func()` is the same as calling `func(&val)` or `func(&&val)`. The value is passed by mutable reference if it is mutable otherwise it's passed as a constant reference (single `&`)

The dot lookup evaluation order of `context.name` is as follows:
1. If `context` is a reference, lookup `name` in the context of `*context`
    * This does not incur a copy
2. If `context` is an object, lookup `name` in the dynamic `self` context
    * If name is private and the current *calling context* does not have permission to access it (not being accessed from a *friend* object or the object itself) then name lookup fails
3. Following *free-name* lookup rules, look for name `typeof(context)::name` and pass `context` as a reference to the first parameter of the function `name` if `name` is indeed a function
    * Ex. `'test'.contains('es')` will search for `string::contains` and pass `&&'test'` as the first parameter
4. Following free-name lookup rules, look for `name` and pass `context` by reference to the first parameter of the function `name` if `name` is a function

To clarify, for steps 3 and 4, `name` should be a function because non-objects cannot have member values

Here are the current standard free functions. Do note that all of them can be applied via dot syntax

* `assert(<expr>, [<string>])`
    * If `<expr>` results in `false`, throws an evaluation error (cannot be caught) with the string "Assertation error" or the second parameter if one is provided
* `typeof(<expr>)`
    * Returns the type of the evaluated expression stored in a string
    * The type returned is the most specific type not a *meta-type* such as `none`, `some`, or `number`.
* `len(<expr>)`
    * If `expr` evaluates to a string, get the length in bytes
    * If `expr` evaluates to a tuple or string, gets the length of that tuple or string
    * If `expr` evaluates to a map, gets the amount of bindings in the map
    * For types gets the amount of members for that type
    * For objects:
        * If the object or parent defines the `__len__` function, call that function
        * Otherwise gets the amount of non-constructor members of the object
    * `unit` is 0
    * If `expr` evaluates to a range of integers, gets the difference between the range start and end
        * Note this means that the length can be 0
    * If `expr` evaluates to a range of anything else, returns 2
    * Everything else will return 1
* `print(<expr1>, <expr2>, ...)`
    * Converts all expressions to strings, and prints them to standard out
    * standard out may not be the console
* `println(<expr1>, <expr2>, ...)`
* `cout(<expr1>, <expr2>, ...)`
    * Same as `print` but to console out
* `coutln(<expr>, ...)`
* `cin_line()`
    * Reads a line from console input
    * Result is right-trimmed of whitespace
* `clone(<expr>)`
    * Evaluates `<expr>` to a value and performs a deep copy
    * If `<expr>` evaluates to an object, calls the `__clone__` function if it has been defined
    * Note: if you access `clone` via dot syntax then the context object is passed by reference and thus clone will return a reference (still a deep copy)

```Javascript
typeof((5, 3, 4)) //"tuple_3"
typeof(5?) //"bool"
typeof([] @ 'Helen') //"array"

len((10, 20, 30)) // 3
len('Hello') //5
[20, 'Hello', 'World'].len() //3

print('Hi', '. How are', ' you') //prints "Hi. How are you" without the quotes

coutln("Please enter your name:");
let response = cin_line()
```

## Templates

This is technically a standard library function but it warrants its own section.

`template(path: <string>, bindings: <map>, [delim: <string>])`
* Reads the file located at `path`, processing any sapl code which is marked by `delim` and returns a string of the file read
* If `delim` is omitted, the default delimiter is "$$"
    * `delim.len()` must be >= 2
    * To escape the delimiter, put a backslash in front of each character in the delimiter
* sapl code between the delimiters will be evaluated, and the code is replaced by the pretty printed result of the execution
* sapl code is executed in its own environment which can be augmented using `bindings`
    * For each k/v pair in `bindings`, add the pair to the environment where the key is the name and value in the map is the value in the environment
* the standard out for the environment of this template is the string that the function returns
    * thus, for code executed by the `template` function, `cout` and `print` do different things
* Anything not enclosed by `delim` is simply returned as-is

The usage for this is to allow keeping different languages separate. For example, data processing can occur in a sapl file and the results can be passed into an html file where the values are simply streamed in in the right locations. See template_test1.txt.

## Arrays

Arrays are stored in contiguous memory. They can be used with `+` and `@` operators, which both create new resultant arrays from their operands. Arrays also provide indexed access with `[]`

Provided interface:
* `<array>.len()`
    * Gets the amount of elements in `<array>`
    * See `len()` above
* `<array>.contains(<expr1>, <expr2>, ...)`
    * Returns true if the values of *all* passed expressions are within `<array>`
    * False otherwise
* `<array>.set(<int>, <expr>)` - mutating
    * Sets the value at the specified index to the evaluated expression
    * Throws if index out of bounds
    * Requires the context array is mutable
* `<array>.push_back(<expr>, <expr2>, ...)` - mut
    * Appends the value of each expression to the array
* `<array>.remove(<int>)` - mut
    * Removes the index
    * Throws if index out of bounds
* `<array>.insert(<int>, <expr>)`
    * Inserts the value of `<expr>` at the index specified
* `<array> @ <expr>`
    * Creates a new array by appending the value of `<expr>` to the back of `<array>`
* `<array1> + <array2>`
    * Creates a new array that is the concatenation of the elements in `<array2>` following the elements of `<array1>`
* `<array>[<expr>]`
    * If `<expr>` evaluates to an integer
        * Gets the element at that index or raises an index out of bounds exception
    * If `<expr>` evaluates to a range
        * Gets a new array that contains all the elements of `<array>` within the specified range

Examples:
```Javascript
let lst = ["hello", 100, false, 'goodbye', 8.70];
(lst @ 100 @ 50 @ 80 @ 90).size() //9

[10, 30, 'hello'] == [10, 30, "hello"] //true

[10, 30, 'hello'] == [10, 30, "hgllo"] //false

let wd = [10, 30, 'hello'][2];
let lst = [3.14, 6.28, 2.73];
wd + lst[1 - 1]
// "hello3.14"

let names = ['Diana', 'Lexi', 'Brady', 'Andrew', 'Martin'];
let names = names + ['Angelina', 'Garcia'];
names[2..6] //['Brady', 'Andrew', 'Martin', 'Angelina']

//remember, these functions can be accessed as free functions too
//but mutating functions require a reference passed
let has_5 = array::contains(?, 5);
let lst = [10, 7];
let lst2 = [5, 500, 300];
!has_5(lst) && has_5(lst2) //true

let var lst = [];
lst.push_back(10, 20, 'Hello');
let arr = &&lst;
arr.push_back(3.14);
lst // [10, 20, 'Hello', 3.14]

let var a = [];
let a_push_front = a.insert(0, ?);
// same as array::insert(&&a, 0, ?)
a_push_front(10);
a_push_front(20);
a_push_front(30);
a // [30, 20, 10]
```

## Range

A range is essentially a 2-tuple with added semantics. A range is denoted as `start..end` where the larger of the two is exclusive and the smaller is inclusive. A valid range contains two integers where `start != end`. `start` can be greater than or less than `end`. If `start > end` then the range counts down (and effectively goes in reverse).

While only *valid* ranges can have the above elements, you can use the `..` operator to create a range from anything just like a tuple. Ranges can also be broken apart by *structured bindings* just like tuples.

As mentioned earlier, valid ranges convert to `true` while invalid ones convert to `false`.

```Rust
let rng = 3.14..false;
if rng: 'Bad' else 'Yes'
// Results in "Yes"

let min, max = -1..100; min - max //-101

let rng = 0 .. 0 - 100 //0..-100

let names = ['Diana', 'Lexi', 'Brady', 'Andrew', 'Martin'];
let names = names + ['Angelina', 'Garcia'];
names[2..6] == ['Brady', 'Andrew', 'Martin', 'Angelina'] &&
//2 inclusive, 6 exclusive
(names @ 'Brandy')[8..4] == ['Brandy', 'Garcia', 'Angelina', 'Martin']
//8 is exclusive, 4 inclusive
```

Start and end elements of a range can also be accessed with `fst()` and `snd()`.

```Rust
let r = 1..5;
r.fst() + r.snd() //6
```

## Tuples

```BNF
<tuple_n> ::= (<expr_1>, ..., <expr_n>)
```

Tuples differ from arrays in the following ways:
* Length of tuples are encoded in their type
* Can be broken apart with structured bindings
* Cannot be indexed
* Are completely immutable and cannot be concatenated

```Rust
let x, y = (10, 20); x + y //30

let tup = (10, 'a', 'c'); 
let a, b, c = tup; 
a + b + c // "10ac"

let _, _, name = (42, 'Corsair', 'Jim'); 
name //"Jim"
```

## Options

Technically, every value is an option. I suppose that in a dynamically typed language, I avoid null pointers by simply re-branding the same idea as optionals and variants. As you've seen, you can get a `none` value (technically `unit`) with the `None` keyword. You can also force a value to be some by using the `Some(<expr>)` function.

`Some()` evaluates the passed expression and wraps it in a single element tuple. This allows these values to be automatically unpacked in for loops and let defintions as if they were the value themselves. Why is this here? Well there are some functions, as we'll see shortly, that use `unit` as a special no data value. There may very well be `unit`s in your data and returning `unit` directly would make the library think that the end of data has been reached. To circumvent this you can wrap all data values in `Some()` which provides indirection so `None` can be returned as data.

```Rust
let x = Some(5);
x // 5

Some(None) is some //true
None is some // false
Some(5) is tuple_1 //true

let var x = None;
x = Some(None); //not a let definition, so not unpacked

x == None // false
x == Some(None) // true

struct ExIter {
    def var counter = 0

    pub fun __next__ {
        self.counter = self.counter + 1;
        if self.counter < 11:
            Some(None)
            // return None as data
        else:
            None
            // return None directly to indicate end of iteration
    }
}

let var count = 0;
for v in ExIter():
    // v is automatically unpacked
    if v is none:
        count = count + 1

count //10

```

## Maps

Maps are key value pairs. The key must be a string, and the value can be anything

```BNF
<map> ::= {<name>: <expr>, <string>: <expr2>, ...}
```

When defining a map literal, if the key is a valid name, quotes can be omitted. Keys with valid names can be indexed via the dot operator

Provided interface:
* `<map>[<expr>]`
    * gets the value with the key that is the value of `<expr>` when evaluated. If `<expr>` does not result in a string, or the key is not found, returns an error
* `<map>.contains(<expr1>, <expr2>, ...)`
    * True if `<map>` contains all the keys passed to it
    * False otherwise
* `<map>.insert(<expr>, [<expr2>])` - mut
    * If `<expr>` evaluates to a tuple
        * Insert that key value tuple
    * Otherwise insert the value of the first as the key and second as the value
* `<map>.remove(<string>)` - mut
    * Removes the key from the map
* `<map1> @ <map2>`
    * Creates a new map this is the result of adding all pairs in `<map2>` to `<map1>`
    * This means any duplicate keys will be taken from `<map2>`
* `<map> @ <tuple_2>`
    * Creates a new map with the pair `<tuple_2>` added to map
    * Overwrites the previous value if the key already exists
* `<map> @ [<tuple_2>, <tuple_2>, ...]`
    * Adds all tuples in the list to `<map>`
    * Duplicate keys are overwritten

Examples:
```Rust
let mp = {'address': '333 East Valley Road'};
let mp = mp @ { 'house_color': 'red', 'car': 'volvo' };
mp['house_color'] //"red"

let mp = {'address': '333 East Valley Road'};
let mp = mp @ [('name', 'Alex'), ('age', 19)];
if mp.contains('address', 'name'):
    mp.contains('age', 'ssn')
else 0
// false

let mp = {};
let mp = mp @ ('name', 'Alex') @ ('age', 19);
mp['name'] + " " + mp['age']
//"Alex 19"


let map = {
    'name': 'Jill',
    'aliases': ('J', 'Jillian'),
    'age': 20,
    'speak': fun (name nicks age) {
        let n1, n2 = nicks;
        "Hello, my name is " + name
        + ", but you can call me " + n1 
        + " or " + n2 + ". I am " + age
        + " years old."
    }
};
map['speak'](map['name'], map['aliases'], map['age'])
//"Hello, my name is Jill, but you can call me J or Jillian. I am 20 years old."

let var mp = {"id card": 5670811, 'name': 'Jim'};
mp.insert('married', true);
mp.remove("id card");
mp //{'name': 'Jim', 'married': true}
```

## Strings

Strings are utf8, however all operations operate on bytes

* `<string>.contains(<string>, <string>, ...)`
* `<string>[<expr>]`
    * Like arrays, `<expr>` can be an index or range
    * The index is the byte index
* `<string>.split(<string>)`
    * Gets an array of strings split on the specified delimiter

The following have yet to be implemented:
* `<string>.find(<string>, [<int>])`
    * Gets the starting byte index of the passed needle
    * Will start looking after the passed integer if supplied
* `<string>.find_all(<string>, [<int>])`
    * Gets an array of integers where each integer is the starting locating of an occurrence of the needle in the haystack
    * The list is ordered so that the first occurrence is index 0
    * Will start looking after the passed integer if supplied
* `<string>.match(<string>)`
    * Returns a tuple of (starting index, matches string) for the first occurrence
* `<string>.match_all(<string>)`
    * Gets an array of (index, match) tuples
    * The array is ordered so index 0 is the first occurrence
* `<string>.replace(regex: <string>, replace: <string>)` - mut
    * Regex replace of all occurrences of `regex` with `replace`
* `<string>.push_back(<string>, <string2>, ...)` - mut
    * Self-explanatory

Examples:
```Rust
"My name is Joe ".split(' ') //['My', 'name', 'is', 'Joe', '']

```

# Loops

## For Loops

```BNF
<for> ::= 
    for <name1>[, <name2>, ...] in <expr> [if <expr2>]:
        <code>
    
    |

    for <name>[, <name2>, ...] in <expr> [if <expr2>] {
        <code>
    }
```

For loops can iterate over iterables such as ranges, maps, and arrays. For maps, the for loop iterates over key/value tuples and as such a structured binding can be in the declared names. A structured binding can also be used when iterating over a list of **only** tuples of the same length. For loops can use a colon or braces. If `<expr>` evaluates to a non-iterable, an exception is thrown.

For loops can have an optional *filter expression* following an `if`. This works like the common iterator function `filter()` in many languages. The filter expression is evaluated on each iteration after the names have been bound to the current element in the iterable and before the body is evaluated. If the filter expression (FE) evaluates to `true`, the body is evaluates, if `false`, the current element is skipped. Anything else produces (or passes if it was already generated) an exception.

I have thought about adding an optional *map expression* with the `where` keyword. So far I felt it's not too hard to perform a mapping manually as the first few lines in the body of the loop.

```Javascript
let var i = 0;
for idx in 0..100:
    i = i + idx
i //4950

let var count = 0;
let rng = 0 .. -100;
for i in rng if i % 2 == 0 {
    count = count - i
}
count //2550

let var result = '';
let lst = [
    ('Cat', 10),
    ('Apple', 2),
    ('Pear', 3)
];
for nm, num in lst if num < 10 {
    result = result + nm + ':' + num + ', '
}
result // "Apple:2, Pear3, "


```

## While Loops

Pretty standard. Can either use a colon or braces

```Javascript
let var count = 0;
while count < 100:
    count = count + 1

count //100
```

# Modules

The most primitive form of modularity is the include operator.


`include <expr>`

`<expr>` is evaluated to a string, which is the path to the file to parse. Think of this of taking the entire file parsing it, and evaluating it right at that line. Included names can shadow existing ones and definitions are directly added to the current scope. The final result of evaluating the file is returned from the include operator.

Imports are a little more refined. Instead of a file, it takes a name and converts it to a file path by converting `::` to "/" and by appending a ".sapl". So the module name `game::characters::dragon` will look for a file "game/characters/dragon.sapl". Imports are evaluated in a separate scope, and the only thing added to the current scope are public definitions (which are part of the `export::` namespace).

The `as` keyword can be used to specify the namespace name to add all the public definitions to. If not specified, the fully qualified module name is used as the namespace. Finally, you can opt to not put the public definitions in a namespace by appending `::*` to the import module name.

im_in_test.sapl:

```Rust
let const = 7;

fun helper v {
    v - const
}

pub fun extern_func a b {
    helper(a) + b
}

extern_func(?, 10)
```
some other file:

```C++
let f = include "examples/im_in_test.sapl";
extern_func(20, 10) + f(5) + const //38



import examples::im_in_test as test;
test::extern_func(10, 20) //23

import examples::im_in_test as test;
test::const //error, not defined

import examples::im_in_test::*;
extern_func(10, 20) //23

import examples::im_in_test;
examples::im_in_test::extern_func(10, 20) //23
```

# Objects

Objects and types are **the only** reference type. This means that the **only** way to create a new instance of an object is by the constructor or clone function. Otherwise, the same object is simply passed around by reference. Since types are completely immutable, knowing that they're a reference type is merely an implementation detail.

```Rust
let var obj1 = Obj();
let var obj2 = obj1;
obj2.name = 'SEV';
obj1.name //'SEV'
```

The base object type is the `struct`. Structs may subtype `type`s. The difference between the two is that a `type` lacks a constructor and thus cannot be used on its own. Thus, it is an interface only. Type interfaces may provide default implementation of functions and default values of members but inheritors are free to override them.

All member variables must be defined. Unlike Python or JS, they cannot just come into existence ad hoc. Definitions have the following syntax:

`[pub] def [var1] name1 [= expr1], [var2] name2 [= expr2], ...`

The access modifier (`pub`) applies to all definitions in a series while the mutability modifier (`var`) applies locally to the next name. Immutable values can be set in the constructor, but cannot be set anywhere else.

No top-level line needs to end with a semicolon in an object or type (code in a function does), although you are more then welcome to use it.

The constructor does not need `pub` before it, it is always public and always named the same as the object. If no constructor is defined, a default no-parameter one is created which sets all values to their specified initial values or `unit`. The initial values are set by copying the initial value to the new instance object. Therefore, if an initial value is a reference, the data behind that reference is **shared** for all instances of an object.


Each function gets a `self` object which refers to itself. `self` is an implicit first parameter to **all** member functions. You need not declare this parameter in the argument list, but it is always there. This is because member functions are accessed via dot-syntax, and this passes the context object (left side of `.`) as a reference to the function.

Thus, there is **no** way to access member variables except through the `self` object (or any other instance object)


Examples
```Rust
struct Person {
    def ssn; // private constant
    pub def name, var age = 0
    // public constant name, and public var age

    // constructor, which takes a value
    fun Person name {
        0
    }

    pub fun hello { //public function
        0
    }
}

struct Person {
    def ssn
    pub def name, var age = 0

    fun Person name {
        self.name = name;
        self.ssn = 156
    }

    pub fun greet {
        "Hello! My name is " + self.name
        + " and I am " + self.age
        + " years old."
    }

    pub fun verify test_ssn {
        self.ssn == test_ssn
        //ssn is added to function scope as self::ssn
    }
}

let jane = Person('Jane'); //invoke constructor
[jane.greet(), jane.verify(156), 10 |> jane.verify, jane.name]
//['Hello! My name is Jane and I am 0 years old.', true, false, 'Jane']
//notice the get the value of name, it had to be dereferenced

struct Animal {
    def var species

    fun Animal species {
        self.species = species
    }

    pub fun mutate species {
        self.species = species
    }
}

let anim = Animal('Canine');
let fail =
try
    anim.mutate('Feline'); 
    //cannot mutate anim bc its a constant
    true
catch _: false;
let fail = fail || try anim.species; true catch _: false;
// cannot access species bc its a private value
fail // false

struct Machine {
    pub def var id = &&0
}

let m = Machine();
m.id <- 10; //error, m is a constant
*m.id




struct Machine {
    pub def id = &&0
}

let var m = Machine();
m.id <- 10; //error, id is constant
*m.id



struct Machine {
    pub def var id = 0
}

let var m = Machine();
m.id <- 10;
*m.id //`0



struct Person {
    def secret = 0

    fun get_password {
        self.secret
    }
}

let adam = Person();
adam.secret
adam.get_password()
//ERROR, both secret and get_password are private
```

## Calling Contexts and Friends

A *calling context* is the context of where an object is being used. It's either in a public, constructor, or self context and is intrinsic to the **copy** of the object reference being used. A constructor context is given to the `self` object passed to the constructor, this allows all variables, public or private, mutable or immutable to be available form read/write access. The self context makes all members readable, but only mutable ones writable. A public context allows only public members to be readable and respects the mutability of each member.

An object or type may declare friends with the `friend` keyword. 

`<friends> ::= friend <name1>, friend <name2>, ...`

Invocation of the object in a context of a friend object may access private data. Works exactly like C++ friends


## Subtyping

Subtyping works by copying all the members **and friends** of the type to the object. Thus, if the super type has a member reference, all instances of all subtypes share that one data value. Multiple subtyping is allowed by using a comma seperated list of types. If name collisions occur, the last specified super type takes priority.

Type methods are also passed a `self` object. However this `self` will not refer to the type but rather an instance of an object that subtypes the type.

```Rust
type Person {
    pub def name, age

    pub fun speak {
        "Hello. I am " + self.name 
    }
}

let my_type = Person; //type alias

struct Baby : Person {
    //notice age and name not defined in struct
    fun Baby name {
        self.age = 0;
        self.name = name
    }

    pub fun speak {
        // function override
        "Goo-goo-ga-ga"
    }
}

struct Child : my_type {
    fun Child name {
        self.age = 10;
        self.name = name
    }
}

let little_jimmy = Baby('Jimmy');
let bobby = Child('Bobby');
[little_jimmy.speak(), little_jimmy.age, bobby.speak(), bobby.age]
//['Goo-goo-ga-ga', 0, 'Hello. I am Bobby', 10]

type Counter {
    def var count = &&0

    fun inc {
        self.count <- *self.count + 1
        // although count can be thought of as a reference to a reference
        // only one dereference is necessary
    }

    pub fun get_count {
        *self.count
    }
}

struct Obj : Counter {

    pub def var a_num

    fun Obj {
        self.inc();
        self.do_it()
    }

    fun do_it {
        fun call_it {
            self.a_num = 10
            // self is statically captured from the outer function `do_it`
        }

        call_it()
    }
    
}

Obj();
Obj();
let a = Obj();
(a.get_count(), a.a_num)
//(3, 10)
```

## Special Functions

Special functions are denoted by preceding and trailing double underscores. As of now, they are like the constructor in the respect that they are always public. However, calling them directly requires the correct calling context.

* `__len__ : self -> int`
    * Overrides the behavior of the built-in `len()` function
    * Expected to return an integer
    * Thus `__len__` can be used as a free or bound function

* `__call__ : var self, ... -> a`
    * Overrides the behavior when a object is called like a function (ie. use the `()` operators on the object)
    * The arguments passed between the `()` are passed directly to `__call__`

* `__index__ : self, int -> a`
    * Overrides the behavior when an object is indexed with the `[]` operator
    * Takes a single argument which is passed between the brackets

* `__next__ : var self -> a`
    * Allows an object to be iterated over in a for loop
    * The data returned by `__next__` is considered to be the data yielded by the iterator
    * Returning `None` (or any `unit` value) from `__next__` stops the iteration

* `__iter__ : self -> iter`
    * Returns an iterator to the object. An iterator must override the `__next__` method or be iterable such as an array, map, or range
    * Can be used anywhere `__next__` is expected

* `__clone__ : self -> self`
    * Similar to `__len__`
    * Overrides the behavior of `clone()`
    * The purpose of overriding clone is to allow deep copying of reference members

```Rust
struct Ring {
    def var ring = [], max_size, var idx = 0

    pub fun Ring size {
        self.max_size = size
    }

    pub fun __len__ {
        self.ring.len()
    }

    pub fun __index__ i {
        (self.ring)[i % len(self)]
    }

    pub fun push_back x {
        if self.ring.len() > self.idx:
            self.ring.set(self.idx, x)
        else:
            self.ring.push_back(x)
        self.idx = self.idx + 1;
        if self.idx >= self.max_size:
            self.idx = 0

    }

    pub fun __call__ {
        self.ring
    }

}

let var ring = Ring(4);
ring.push_back(10);
ring.push_back(9);
ring.push_back(8);
ring.push_back(7);
ring.push_back(6);
(ring[0], ring(), ring.len())
//(6, [6, 9, 8, 7], 4)

```


```Rust
struct Rope {
    def l1, l2, var idx = 0

    fun Rope l1 l2 {
        self.l1 = l1;
        self.l2 = l2
    }

    fun __next__ {
        let res = 
        if self.idx < self.l1.len() + self.l2.len():
            if self.idx >= self.l1.len():
                (self.l2)[self.idx - self.l1.len()]
            else:
                (self.l1)[self.idx]
        else:
            None;
        self.idx = self.idx + 1;
        res
    }

}
let r = Rope([5, 4], [3, 2, 1]);
let var buf = [];
for i in r {
    buf.push_back(i)
}
buf //[5, 4, 3, 2, 1]
```

```Rust
struct ListIter {
    def var nx, var val

    fun ListIter start {
        if start is List:
            self.val = Some(start.val);
            // assignment: doesn't unpack
            self.nx = start.next
        else:
            throw "Invalid argument passed to ListIter"
    }

    pub fun __next__ {
        let val = self.val;
        if self.nx is some:
            self.val = Some(self.nx.val);
            self.nx = self.nx.next
        else:
            self.val = None
        val
    }
}
struct List {
    pub def var val
    def var next
    friend ListIter
    //ListIter is a friend, can access private internals

    fun List val {
        self.next = None;
        if val is array && val.len() >= 1:
            self.val = val[0];
            for idx in 1 .. val.len():
                self.push_back(val[idx])
        else:
            self.val = val
        
    }

    pub fun push_back x {
        let var n = &&self.next;
        while *n is some {
            n = &&n.next
        }
        n <- List(x)
    }

    pub fun __len__ {
        let var n = &self.next;
        let var count = 1;
        while *n is some {
            n = &n.next;
            count = count + 1
        }
        count
    }

    pub fun contains x {
        if x == self.val: true
        else if self.next is some:
            self.next.contains(x)
        else:
            false
    }

    pub fun __iter__ {
        ListIter(self)
    }
}
let lst = List([10, 20, 30]);
let tup = (lst.contains(10), lst.len(), lst.contains(50));

let var sum = 0;
for e in lst {
    // e is the unpacked Some()
    sum = sum + e
}
(tup, sum) //((true, 3, false), 60)

```


```Rust
struct Obj1 {
    pub def var a, var b

    fun Obj1 a b { 
        let var a = a;
        self.a = &&a;
        self.b = b
    }

    fun __clone__ {
        Obj1(*self.a, self.b)
    }
}

struct Obj2 {
    pub def var a, var b

    fun Obj2 a b { 
        let var a = a;
        self.a = &&a;
        self.b = b
    }
}

let var a1 = Obj1(10, 20);
let var b1 = a1.clone();
a1.a <- 20;
let test1 = *b1.a; //10

let var a2 = Obj2(10, 20);
let var b2 = a2.clone();
b2.a <- 20;
*a2.a + test1
//30
```

# TODO
- [ ] Cleanup (Can this ever really be totally complete?)
- [x] Modules, Imports and Includes
- [x] References and a solution to all the copies
- [x] Classes and Interfaces (similar to Rust's structs and traits)
- [x] Variants and Options (Sort of)
- [x] Templates? (not in the sense of C++, but rather similar to BLAZOR in HTML)
- [x] Standard library functions (more functions for types, more provided functions) WIP
- [ ] Streams?
- [x] Iterators? (Partial)
- [ ] ~~Try to avoid feature creep~~ **FAILED**

