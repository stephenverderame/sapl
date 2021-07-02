#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser;
    use crate::evaluator;
    use crate::lexer::Tokens;
    use crate::parser::Ast;
    use crate::parser::Op;
    use crate::evaluator::Values;
    use crate::evaluator::Res;
    use std::collections::VecDeque;
    use std::collections::HashMap;

    fn assert_toks_eq(left: &VecDeque<Tokens>, right: Vec<Tokens>) {
        assert_eq!(*left, VecDeque::from(right));
    }

    fn assert_parse_eq(left: Vec<Tokens>, right: Ast) {
        let mut vd = VecDeque::from(left);
        assert_eq!(parser::parse(&mut vd).unwrap(), right);
    }

    fn assert_parse_str_eq(code: &str, ast: Ast) {
        let mut ts = lexer::tokenize(code.as_bytes());
        assert_eq!(parser::parse(&mut ts).unwrap(), ast);
    }

    fn assert_val_eq(code: &str, val: Values) {
        assert_eval_eq(code, Res::Vl(val))
    }

    fn assert_eval_eq(code: &str, res: Res) {
        let mut ts = lexer::tokenize(code.as_bytes());
        let ast = parser::parse(&mut ts).unwrap();
        assert_eq!(evaluator::evaluate(&ast), res);
    }

    fn assert_sapl_eq(left: &str, right: &str) {
        assert_eq!(crate::parse_sapl(left.as_bytes()), 
            crate::parse_sapl(right.as_bytes()))
    }

    #[test]
    fn literal_test() {
        let mut tokens = lexer::tokenize("10".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(10)]);

        tokens = lexer::tokenize("10.45".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Float(10.45)]);

        tokens = lexer::tokenize("-97.54".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Float(-97.54)]);

        tokens = lexer::tokenize("-1001".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(-1001)]);

        tokens = lexer::tokenize("0.45".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Float(0.45)]);

        tokens = lexer::tokenize("-0.77".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Float(-0.77)]);

        tokens = lexer::tokenize("0".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(0)]);

        tokens = lexer::tokenize("   0".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(0)]);

        tokens = lexer::tokenize("   134    ".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(134)]);

        tokens = lexer::tokenize("'Hello World'".as_bytes());
        assert_toks_eq(&tokens, 
            vec![Tokens::TString("Hello World".to_owned())]);
    }

    #[test]
    fn op_test() {
        let mut tokens = lexer::tokenize("+ - * / % **".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::OpPlus, Tokens::OpMinus,
            Tokens::OpMult, Tokens::OpDiv, Tokens::OpMod,
            Tokens::OpExp]);

        tokens = lexer::tokenize("&& || | & +".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::OpLand, Tokens::OpLor,
            Tokens::OpOr, Tokens::OpAnd, Tokens::OpPlus]);

        tokens = lexer::tokenize("(10 + 5) * (3 - 2)".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::LParen, Tokens::Integer(10),
                Tokens::OpPlus, Tokens::Integer(5), Tokens::RParen,
                Tokens::OpMult, Tokens::LParen, Tokens::Integer(3),
                Tokens::OpMinus, Tokens::Integer(2), Tokens::RParen]);

        tokens = lexer::tokenize("(((30)))".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::LParen, Tokens::LParen,
            Tokens::LParen, Tokens::Integer(30), Tokens::RParen,
            Tokens::RParen, Tokens::RParen]);
    }

    #[test]
    fn lex_str_test() {
        let toks = lexer::tokenize("if '': 0 else 5".as_bytes());
        assert_toks_eq(&toks, vec![Tokens::If, Tokens::TString("".to_owned()),
            Tokens::Colon, Tokens::Integer(0), Tokens::Else,
            Tokens::Integer(5)]);
    }

    #[test]
    fn lex_name_test() {
        let toks = lexer::tokenize("true || false&&true".as_bytes());
        assert_toks_eq(&toks, vec![Tokens::Bool(true),
            Tokens::OpLor, Tokens::Bool(false), Tokens::OpLand,
            Tokens::Bool(true)]);
    }

    #[test]
    fn multiline_test() {
        let t = lexer::tokenize("13\n\t40".as_bytes());
        assert_toks_eq(&t, vec![Tokens::Integer(13), Tokens::Integer(40)]);
    }

    #[test]
    fn literal_stream_test() {
        let mut tokens = lexer::tokenize("134     'Cat'  -10.5".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(134),
            Tokens::TString("Cat".to_owned()), Tokens::Float(-10.5)]);

        tokens = lexer::tokenize("\r\n\t".as_bytes());
        assert_toks_eq(&tokens, vec![]);

        tokens = lexer::tokenize("13\n\t40".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(13), Tokens::Integer(40)]);

        tokens = lexer::tokenize(r#"
            13
                40 
                'C'
            "#.as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(13), Tokens::Integer(40), Tokens::TString("C".to_owned())]);
    }

    #[test]
    #[should_panic]
    fn multiple_decimal_pts() {
        lexer::tokenize("10.0.0".as_bytes());
    }

    #[test]
    #[should_panic]
    fn multiple_minus_sign() {
        lexer::tokenize("--10".as_bytes());
    }

    #[test]
    fn arithmetic_parse() {
        assert_parse_eq(
            vec![Tokens::Integer(10), Tokens::OpPlus, Tokens::Integer(200)], 
            Ast::Bop(Box::new(Ast::VInt(10)), Op::Plus, Box::new(Ast::VInt(200)))
        );

        assert_parse_str_eq("10 + 20 * 30", Ast::Bop(Box::new(Ast::VInt(10)),
             Op::Plus, Box::new(Ast::Bop(Box::new(Ast::VInt(20)), Op::Mult, Box::new(Ast::VInt(30))))));

        assert_parse_str_eq("43 <= 10 * 10", 
            Ast::Bop(Box::new(Ast::VInt(43)), Op::Leq, Box::new(
                Ast::Bop(Box::new(Ast::VInt(10)), Op::Mult, Box::new(Ast::VInt(10)))
        )));

        assert_parse_str_eq("43 <= 10 * 10 && true", Ast::Bop(
            Box::new(Ast::Bop(Box::new(Ast::VInt(43)), Op::Leq, Box::new(
                Ast::Bop(Box::new(Ast::VInt(10)), Op::Mult, Box::new(Ast::VInt(10)))
            ))), Op::Land, Box::new(Ast::VBool(true))
        ));

        assert_parse_str_eq("20 + 3 - 10", Ast::Bop(
            Box::new(Ast::Bop(Box::new(Ast::VInt(20)), Op::Plus, Box::new(Ast::VInt(3)))),
            Op::Sub,
            Box::new(Ast::VInt(10))
        ));

    }

    #[test]
    fn arithmetic_eval() {
        assert_val_eq("10 + 0.45 * 2", Values::Float(10.9));
        assert_val_eq("10 ** -2", Values::Float(0.01));
        assert_val_eq("10 % 4 + 20 * 3", Values::Int(62));
        assert_val_eq("20 - 3 + 10 - 23 + 5 - 4 * 3", Values::Int(-3));
        assert_val_eq("10 ** 3 / 3", Values::Int(333));
        assert_val_eq("'Hello ' + 'World'", Values::Str("Hello World".to_owned()));
        assert_val_eq("42", Values::Int(42));
        assert_val_eq("42 - -20", Values::Int(62));
        assert_val_eq("10+20-3*4", Values::Int(18));
        assert_val_eq("true || 10 / 0 > 0", Values::Bool(true));
        assert_val_eq("false && 5 / 0 == 0", Values::Bool(false));
    }

    #[test]
    fn parenthesis_eval() {
        assert_val_eq("(20 + 20) * 10", Values::Int(400));
        assert_val_eq("2 ** (4 + 4)", Values::Int(256));
        assert_val_eq("(((30)))", Values::Int(30));
        assert_val_eq("(30 - 5) / (10 + 5.0)", Values::Float(25.0 / 15.0));
        assert_val_eq("'Hello ' + (4000 + 410 * 2)", Values::Str("Hello 4820".to_owned()));
    }

    #[test]
    fn bool_eval() {
        assert_val_eq("43 <= 10 * 10 && 23 == 23", Values::Bool(true));
        assert_val_eq("10 + 10 - 20 > -5 || false", Values::Bool(true));
        assert_val_eq("10 ** 3 * 4 == 100", Values::Bool(false));
        assert_val_eq("4.001 > 2 ** 2", Values::Bool(true));
        assert_val_eq("'Windows' > 'Doors'", Values::Bool(true));
        assert_val_eq("'apple' == 'apple' && 'dog' != 'cat'", Values::Bool(true));
        assert_val_eq("true && false || true", Values::Bool(true));
    }

    #[test]
    fn conditional_test() {
        assert_val_eq("if 100 != 10: 50 + 50", Values::Int(100));
        assert_val_eq("if '': 0 else 10", Values::Int(10));

        assert_parse_str_eq("if true: false else if true: true", Ast::If(Box::new(Ast::VBool(true)),
            Box::new(Ast::VBool(false)), Some(Box::new(
                Ast::If(Box::new(Ast::VBool(true)), Box::new(Ast::VBool(true)), None)
            ))
        ));

        assert_val_eq("if false: false else if true: true", Values::Bool(true));

        assert_val_eq(r#"
            if 300 == 30 * 100: 
                0
            else if 10 > 0:
                1
        "#, Values::Int(1));

        assert_val_eq(r#"
            if 10 * -10 > -10:
                0
            else if 40 > 300:
                0
            else if 10 + '' == '10':
                if 'cat' != true {
                    if 'apple' < 'banana':
                        (10 + 3) ** 4 % 19
                } else 
                    (10 + 4) ** 3 % 17
        "#, Values::Int(4));

        assert_val_eq(r#"
        let x = 'app';
        if x == 'apple' {
            let y = x
        };
        if x == 'app' {
            "yes"
        }
        "#, Values::Str("yes".to_owned()));
    } 

    #[test]
    fn sequence_test() {
        assert_val_eq(r#"
        300;
        50 + 50 < 80;
        30
        "#
        , Values::Int(30));

        assert_val_eq(r#"
        if 10 * 10:
            'Hello World';
            30 * (40 - 20 - 10 + 5 - 3)
        "#
        , Values::Int(360));
    }

    #[test]
    fn let_test() {
        assert_parse_str_eq("let x = 5; x", Ast::Seq(vec![
            Box::new(Ast::Let(vec![("x".to_owned(), false)], Box::new(Ast::VInt(5)))),
            Box::new(Ast::Name("x".to_owned()))
        ]));
        assert_val_eq("let x = 5; x", Values::Int(5));
        assert_val_eq(r#"
        let name = 'Joe';
        let other = 
        if name {
            let name = 'y';
            name + ' yea!'
        };
        name + other
        "#, Values::Str("Joey yea!".to_owned()));
    }

    #[test]
    fn func_test() {
        assert_val_eq(r#"
        fun do_stuff x y z {
            x + y + z
        };
        let x = 500 * (20 - 3 ** 2);
        do_stuff(x, 10, 20)
        "#, Values::Int(5530));

        assert_val_eq(r#"
        let x = 10;
        fun no_params {
            if x > 5:
                x ** 3
            else
                x
        };
        let y = no_params() - x;
        'Answer: ' + y
        "#, Values::Str("Answer: 990".to_owned())); 

        assert_val_eq(r#"
        fun max a b {
            if a > b:
                a
            else
                b
        }

        fun min a b {
            if a < b: a else b
        }

        min(max(5, 10), 11)
        "#, Values::Int(10));

        assert_parse_str_eq("3 * func(3)", Ast::Bop(Box::new(Ast::VInt(3)), Op::Mult, 
            Box::new(Ast::FnApply(Box::new(Ast::Name("func".to_owned())), vec![Box::new(Ast::VInt(3))]))));

        assert_val_eq(r#"
        fun fact x {
            if x <= 1: 1
            else
                x * fact(x - 1)
        }

        fact(5)
        "#, Values::Int(120));
    }

    #[test]
    fn recursion_test() {
        // CHECK
        assert_val_eq(r#"
        fun summation start end {
            fun sum_helper i {
                if i < end:
                    i + sum_helper(i + 1)
                else end
            }

            sum_helper(start)
        }

        summation(0, 50)
        "#, Values::Int(1275));
    }

    #[test]
    fn comment_test() {
        let tokens = lexer::tokenize(r#"
        // comment
        3
        /*

        56

        */
        5//hello

        11/* a number */
        12
        "#.as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(3), Tokens::Integer(5), Tokens::Integer(11),
            Tokens::Integer(12)]);
    }

    #[test]
    fn higher_order_function_test() {
        assert_val_eq(r#"
        fun add x y {
            x + y
        }

        fun double f x {
            f(x, x) + f(x, x)
        }

        double(add, 10)
        "#, Values::Int(40)); 
        let tokens = lexer::tokenize("let add10 = add(10, ?)".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Let, Tokens::Name("add10".to_owned()),
            Tokens::OpAssign, Tokens::Name("add".to_owned()), Tokens::LParen, Tokens::Integer(10),
            Tokens::Comma, Tokens::OpQ, Tokens::RParen]);

        assert_parse_str_eq("let add10 = add(10, ?)", 
            Ast::Let(vec![("add10".to_owned(), false)], Box::new(
                Ast::FnApply(Box::new(Ast::Name("add".to_owned())), 
                vec![Box::new(Ast::VInt(10)), Box::new(Ast::Placeholder)]))
        ));

        assert_val_eq(r#"
        fun add x y {
            x + y
        }

        let add10 = add(10, ?);
        add10(5)
        "#, Values::Int(15));

        assert_val_eq(r#"
        fun do_stuff x y z {
            x - y / z
        }

        let add_dbl = do_stuff(10, ?, 30);
        add_dbl(60)
        "#, Values::Int(8));

        assert_val_eq(r#"
        fun log level msg code {
            "[LOG (" + level + ")]: " + msg + " {" + code + "}"
        }

        let debug = log("DEBUG", ?, ?);
        debug("Got Here!", 1)
        "#, Values::Str("[LOG (DEBUG)]: Got Here! {1}".to_owned()));

        assert_val_eq(r#"
        fun inc x {
            x + 1
        }

        5 |> inc
        "#, Values::Int(6));

        assert_val_eq(r#"
        fun sub x y {
            x - y
        }

        5 |> sub(?, 3)
        "#, Values::Int(2));
    }

    #[test]
    fn pipeline_test() {
        assert_parse_str_eq("5 |> sub(?, 3) |> inc", Ast::Bop(
            Box::new(Ast::Bop(
                Box::new(Ast::VInt(5)),
                Op::Pipeline,
                Box::new(Ast::FnApply(Box::new(Ast::Name("sub".to_owned())), vec![Box::new(Ast::Placeholder),
                    Box::new(Ast::VInt(3))]))
            )), 
            Op::Pipeline, 
            Box::new(Ast::Name("inc".to_owned()))
        ));


        assert_val_eq(r#"
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
        "#, Values::Int(140));

        assert_val_eq(r#"
        fun to_string x {
            "" + x
        }

        fun concat x y {
            x + " " + y
        }

        100 + 10 * 6 |> to_string 
        |> concat(?, "hello")
        "#, Values::Str("160 hello".to_owned()));
    }

    #[test]
    fn list_test() {
        assert_val_eq("[10, true, 10 + 10, 3.14]", Values::Array(Box::new(vec![Values::Int(10),
        Values::Bool(true), Values::Int(20), 
        Values::Float(3.14)])));

        assert_val_eq("len([100, false, 'Hello'])", Values::Int(3));

        assert_val_eq(r#"
        let lst = ["hello", 100, false, 'goodbye', 8.70];
        len(lst @ 100 @ 50 @ 80 @ 90)
        "#, Values::Int(9));

        assert_val_eq(r#"
        [10, 30, 'hello'] == [10, 30, "hello"]
        "#, Values::Bool(true));

        assert_val_eq(r#"
        [10, 30, 'hello'] == [10, 30, "hgllo"]
        "#, Values::Bool(false));

        assert_val_eq(r#"
        let wd = [10, 30, 'hello'][2];
        let lst = [3.14, 6.28, 2.73];
        wd + lst[1 - 1]
        "#, Values::Str("hello3.14".to_owned()));

        assert_val_eq(r#"
        let names = ['Diana', 'Lexi', 'Brady', 'Andrew', 'Martin'];
        let names = names + ['Angelina', 'Garcia'];
        names[2..6] == ['Brady', 'Andrew', 'Martin', 'Angelina'] &&
        (names @ 'Brandy')[8..4] == ['Brandy', 'Garcia', 'Angelina', 'Martin']
        "#, Values::Bool(true));
    }

    #[test]
    fn uop_test() {
        assert_val_eq("!43", Values::Int(-43));
        assert_val_eq("let x = true; !x", Values::Bool(false));
        assert_val_eq("let x = 3.14; !x", Values::Float(-3.14));

        assert_val_eq(r#"
        let idx = 10;
        if lst? && idx?:
            10
        else if idx? && !(lst?):
            11
        else
            12
        "#, Values::Int(11));
    }

    #[test]
    fn lambda_test() {
        assert_val_eq(r#"
        let lam = fun (x y) x + y;
        lam(10, 20)
        "#, Values::Int(30));

        assert_val_eq(r#"
        let min_max = fun (x y z) {
            let min = if x < y && x < z: x
                else if y < x && y < z: y
                else z;
            let max = if x > y && x > z: x
                else if y > x && y > z: y
                else z;
            (min, max)
        };
        let min, max = min_max(10, 50, -100);
        min + max
        "#, Values::Int(-50));

        assert_val_eq(r#"
        let countdown = fun (x) {
            if x <= 0: 0
            else {
                x + this(x - 1)
            }
        };
        countdown(10)
        "#
        , Values::Int(55));

        assert_sapl_eq(r#"
        let y = (fun (x y) x * y)(20, 5);
        let fx = (fun (x y) x + y)(?, 2);
        fx(y)
        "#, "102");
    }

    #[test]
    fn tuple_test() {
        assert_val_eq("let x, y = (10, 20); x + y", Values::Int(30));
        assert_val_eq("let tup = (10, 'a', 'c'); let a, b, c = tup; a + b + c", 
            Values::Str("10ac".to_owned()));
        assert_val_eq("let min, max = -1..100; min - max", Values::Int(-101));
        assert_val_eq("let _, _, name = (42, 'Corsair', 'Jim'); name", Values::Str("Jim".to_owned()));
    }

    #[should_panic]
    #[test]
    fn bad_binding() {
        assert_val_eq("let x, y = (1, 2, 3)", Values::Unit);
    }

    #[test]
    fn map_test() {
        let mut m = HashMap::<String, Values>::new();
        m.insert("name".to_owned(), Values::Str("Billy".to_owned()));
        m.insert("age".to_owned(), Values::Int(53));
        assert_val_eq("{'name': 'Billy', 'age': 53 }", Values::Map(Box::new(m)));

        assert_val_eq(r#"
        let mp = {
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
        let speak = mp['speak'];
        speak(mp['name'], mp['aliases'], mp['age'])
        "#, Values::Str("Hello, my name is Jill, but you can call me J or Jillian. I am 20 years old.".to_owned()));

        assert_val_eq(r#"
        let mp = {
            'str key': 20
        };
        mp['str key']
        "#, Values::Int(20));

        assert_val_eq(r#"
        let mp = {};
        let mp = mp @ ('name', 'Alex') @ ('age', 19);
        mp['name'] + " " + mp['age']
        "#, Values::Str("Alex 19".to_owned()));

        assert_val_eq(r#"
        let mp = {'address': '333 East Valley Road'};
        let mp = mp @ [('name', 'Alex'), ('age', 19)];
        if mp.contains('address', 'name'):
            mp.contains('age', 'ssn')
        else:
            mp
        "#, Values::Bool(false));

        assert_val_eq(r#"
        let mp = {'address': '333 East Valley Road'};
        let mp = mp @ { 'house' + '_color': 'red', 'car': 'volvo' };
        mp['house_color']
        "#, Values::Str("red".to_owned()));
    }

    #[test]
    fn return_test() {
        assert_parse_str_eq("if x == 10: return x", 
            Ast::If(Box::new(Ast::Bop(Box::new(Ast::Name("x".to_owned())), Op::Eq, Box::new(Ast::VInt(10)))),
                Box::new(Ast::Uop(Box::new(Ast::Name("x".to_owned())), Op::Return)), None));

        assert_val_eq(r#"
        fun test_fun x {
            if x == 10 { return x };
            x * x
        }

        test_fun(10) + test_fun(8)
        "#
        , Values::Int(74))
    }

    #[test]

    fn try_test() {
        assert_val_eq(r#"
        fun idiv x y {
            try
                x / y
            catch _:
                0
        }

        idiv(100, 0) + idiv(100, 10)
        "#, Values::Int(10));

        assert_val_eq(r#"
        try
            let name = 'Freya' + 10 ** 3;
            let town = 'Smithtown';
            throw name + ' from ' + town
        catch str:
            str
        "#, Values::Str("Freya1000 from Smithtown".to_owned()));
    }

    #[test]
    fn typeof_test() {
        assert_val_eq(r#"
        typeof((4, 5, 3))
        "#, Values::Str("tuple_3".to_owned()));

        assert_val_eq(r#"
        typeof(5?)
        "#, Values::Str("bool".to_owned()));

        assert_val_eq(r#"
        typeof([] @ 'Helen')
        "#, Values::Str("array".to_owned()));

        assert_val_eq(r#"
        let name = 'Hailey';
        assert(typeof(name) == 'string')
        "#, Values::Unit);
    }

    #[test]
    #[should_panic]
    fn assert_fail() {
        assert_val_eq("assert(4 < 3, 'Should panic')", Values::Unit);
    }

    #[test]
    fn postcondition_test() {
        assert_val_eq(r#"
        let max = fun (x y) if x > y: x else y;
        let min = fun (x y) if x < y: x else y;

        fun gcd x y -> int >= 1 {
            assert(x >= 1 || y >= 1);

            let max = max(x, y);
            let min = min(x, y);
            if x <= 1 || y <= 1:
                max
            else
                gcd(max - min, min)

        }

        gcd(10, 5)
        "#, Values::Int(5));

        assert_val_eq(r#"
        fun get_name x -> string {
            return 'Joe'
        }

        10 |> get_name
        "#, Values::Str("Joe".to_owned()));

        assert_val_eq(r#"
        fun tie x y -> tuple_2 {
            return (x, y)
        }

        let x, y = tie(10, 20);
        x + y
        "#, Values::Int(30));

        assert_val_eq(r#"
        fun valid_name str -> bool {
            let names = [
                "Billy",
                "Jackson",
                "Persephone"
            ];
            names.contains(str)
        }

        fun meet person -> valid_name(string) {
            person['name']
        }

        let p = {'name': "Jackson", 'a' + 'ge': 10};
        meet(p)
        "#, Values::Str("Jackson".to_owned()));

        assert_val_eq(r#"
        fun get_add x -> (result + result)? {
            if x == 0: "Hello "
            else if x == 1: 10
            else if x == 2: 3.14
            else 10
        }

        get_add(0) + get_add(1) + get_add(2)
        "#, Values::Str("Hello 103.14".to_owned()));

        assert_sapl_eq(r#"
        fun get_some -> some {
            0
        }
        get_some()
        "#, "0");
    }

    #[test]
    #[should_panic]
    fn bad_postcondition() {
        assert_val_eq(r#"
        fun error -> unit {
            4.3
        }
        
        error()
        "#, Values::Unit)
    }

    #[test]
    #[should_panic]
    fn bad_postcondition2() {
        assert_val_eq(r#"
        fun error -> result * result {
            []
        }
        
        error()
        "#, Values::Unit)
    }

    #[test]
    fn for_test() {
        assert_val_eq(r#"
        let var count = 0;
        let rng = 0 .. -100;
        for i in rng if i % 2 == 0 {
            count = count - i
        }
        count
        "#, Values::Int(2550));

        assert_val_eq(r#"
        let var result = '';
        let lst = [
            ('Cat', 10),
            ('Apple', 2),
            ('Pear', 3)
        ];
        for nm, num in lst if num < 10 {
            result = result + nm + ':' + num + ', '
        }
        result
        "#, Values::Str("Apple:2, Pear:3, ".to_owned()));

        assert_val_eq(r#"
        let var i = 0;
        for idx in 0..100:
            i = i + idx
        i
        "#, Values::Int(4950));
    }

    #[test]
    fn while_test() {
        assert_val_eq(r#"
        let var count = 0;
        while count < 100:
            count = count + 1
        
        count
        "#, Values::Int(100));

    }

    #[test]
    fn ref_tests() {
        assert_parse_str_eq("*x + 20", Ast::Bop(
            Box::new(Ast::Uop(Box::new(Ast::Name("x".to_owned())), Op::Deref)),
            Op::Plus,
            Box::new(Ast::VInt(20))
        ));

        assert_val_eq(r#"
        let x = &100;
        *x + 20
        "#, Values::Int(120));

       assert_val_eq(r#"
        let things = ['Keyboard', 'Mouse', 'Cable', 'Calculator', 'USB'];
        let t2 = &things;
        len(t2)
        "#, Values::Int(5)); 

        assert_val_eq(r#"
        let var name = 'SEV';
        let name2 = &&name;
        name2 <- 'SC';
        name
        "#, Values::Str("SC".to_owned()));

        assert_val_eq(r#"
        fun mut_count x {
            for i in 1..11:
                x <- *x * i
        }
        let var counter = 1;
        mut_count(&&counter);
        counter
        "#, Values::Int(3628800));

        assert_eval_eq(r#"let var n = 1; let p = &&n; p = 3"#, 
            Res::Exn(Values::Str("Cannot mutate an immutable value".to_owned())));

        assert_eval_eq(r#"let var n = 1; let p = &n; p <- 3"#, 
            Res::Exn(Values::Str("Cannot mutate an immutable value".to_owned())));

        assert_val_eq(r#"
        let var lst = [];
        lst.push_back(10, 20, 'Hello');
        let arr = &&lst;
        arr.push_back(3.14);
        lst
        "#, 
        Values::Array(Box::new(vec![Values::Int(10), 
            Values::Int(20), 
            Values::Str("Hello".to_owned()),
            Values::Float(3.14)])));

        assert_sapl_eq(r#"
        let lst = ['Hello', 'There', 'I', 'am', 'good'];
        let lst2 = &lst;
        lst2[2]
        "#, "'I'");

        assert_sapl_eq(r#"
        let lst1 = [4, 3, 5];
        let var lst2 = lst1 @ 45;
        lst2.set(0, 100);
        lst2[0] + lst1[0]
        "#, "104");

        assert_sapl_eq(r#"
        let lst = [&&5, &&6, &&8];
        lst[0] <- 3;
        *(lst[0])
        "#, "3");

        assert_sapl_eq(r#"
        let var lst = [0, 1, 3, 5];
        lst.insert(2, 'H');
        lst.remove(0);
        lst
        "#, "[1, 'H', 3, 5]");

        assert_sapl_eq(r#"
        let var mp = {'id card': 5670811, 'name': 'Jim'};
        mp.insert('married', true);
        mp.remove('id card');
        mp
        "#, "{'name': 'Jim', 'married': true}");
    }

    #[test]
    fn hardcoded_tests() {
        assert_sapl_eq(r#"
        len({'a': 'b', 'c': 'd'}) + len([3, 4, 5])
        "#, 
        "len('bad') + len(0..10) - 4 * len(true..3.14)");

        assert_sapl_eq(r#"
        let lst = [0, 1, 'Hello', 'Bye'];
        let lst_len = len(lst);
        for _ in lst {
            assert(lst.contains(lst[random(lst_len)]))
        }
        true
        "#, "true");

        assert_val_eq("None", Values::Unit);

        assert_sapl_eq(r#"
        fun zdiv x y {
            try x / y
            catch _: 0
        }
        let r = &20;
        (40).zdiv(20) + (*r).zdiv(10)
        "#, "4");

        assert_sapl_eq(r#"
        ([10, 20, 30] |> len) + [0, 1, 2].len()
        "#, "6");
    }

    #[test]
    fn dot_test() {
        assert_sapl_eq(r#"
        let var lst = [4, 3, 2];
        lst.push_back(10);
        lst[lst.len() - 1]
        "#, "10");

        assert_sapl_eq(r#"
        let var lst = [4, 3, 2];
        lst.push_back(10, 'Hello');
        lst
        "#, "[4, 3, 2, 10, 'Hello']");

        assert_sapl_eq(r#"
        let var lst = [4, 3, 2];
        lst.insert(0, 'G');
        lst.set(1, 5);
        lst[0..3]
        "#, "['G', 5, 3]");

        assert_sapl_eq(r#"
        let var mp = {};
        mp.insert(('key', 3));
        mp.insert('key2', 4);
        mp['key'] + mp['key2']
        "#, "7");

        assert_sapl_eq(r#"
        let lst = [23, 100, 'G'];
        'G' |> lst.contains
        "#, "true");

        assert_sapl_eq(r#"
        let push5 = fun (x) x.push_back(5);
        let var ar = [];
        push5(&&ar);
        push5(&&ar);
        ar
        "#, "[5, 5]");

        assert_sapl_eq(r#"
        let var a = [];
        let a_push_front = a.insert(0, ?);
        a_push_front(10);
        a_push_front(20);
        a_push_front(30);
        a
        "#, "[30, 20, 10]");

        assert_sapl_eq(r#"
        let lst = [&fun (x) x + x, fun (x) x * x, &fun (x) x ** x];
        lst[0](5) + lst[1](2)
        "#, "14");

        assert_sapl_eq(r#"
        let lst = [[3, 2], [4, 3], [5, 4]];
        lst[0][1]
        "#, "2");

        
    }

    #[test]
    fn more_ref_tests() {
        assert_sapl_eq(r#"
        let func = &fun (x) x + 200;
        func(200)
        "#, "400");

        assert_sapl_eq(r#"
        let func = &fun (x) x + 200;
        func(200)
        "#, "400");

        assert_sapl_eq(r#"
        let var num = 0;
        fun do_it -> int == 10 {
            num = num + 10;
            num
        }
        num = do_it();
        num = num + do_it();
        num
        "#, "20");
    }
    #[test]
    fn struct_tests() {
        let st = 
        crate::parser::SaplStruct {
            name: "Person".to_owned(),
            ctor: Some(Box::new(
                Ast::Func("Person".to_owned(), vec!["name".to_owned()],
                    Box::new(Ast::VInt(0)), None)
            )),
            dtor: Some(Box::new(
                Ast::Func("`Person".to_owned(), Vec::<String>::new(),
                    Box::new(Ast::VInt(0)), None)
            )),
            publics: vec![("name".to_owned(), false, None),
                ("age".to_owned(), true, Some(Box::new(Ast::VInt(0)))),
                ("hello".to_owned(), false, Some(Box::new(Ast::Func(
                    "hello".to_owned(), Vec::<String>::new(), Box::new(Ast::VInt(0)), None))))],
            privates: vec![("ssn".to_owned(), false, None)],
            parents: Vec::<String>::new(),
        };
        assert_parse_str_eq(r#"
        struct Person {
            def ssn;
            pub def name, var age = 0

            fun Person name {
                0
            }

            fun `Person {
                0
            }

            pub fun hello {
                0
            }
        }
        "#, Ast::Struct(st));

        assert_sapl_eq(r#"
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
            }
        }

        let jane = Person('Jane');
        [jane.greet(), jane.verify(156), 10 |> jane.verify, jane.name]
        "#, 
        "['Hello! My name is Jane and I am 0 years old.', true, false, 'Jane']");

        assert_sapl_eq(r#"
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
            true
        catch _: false;
        let fail = fail || try anim.species; true catch _: false;
        fail
        "#, "false");

        assert_sapl_eq(r#"
        struct Machine {
            pub def var id = &&0
        }

        let m = Machine();
        try
            m.id = 10;
            m.id
        catch _:
            'pass'
        "#, "'pass'");

        assert_sapl_eq(r#"
        struct Machine {
            pub def id = &&0
        }

        let var m = Machine();
        try
            m.id = 10;
            m.id
        catch _:
            'pass'
        "#, "'pass'");

        assert_sapl_eq(r#"
        struct Machine {
            pub def var id = 0
        }

        let var m = Machine();
        m.id = 10;
        m.id
        "#, "10");

        assert_sapl_eq(r#"
        struct Machine {
            pub def id = 0
        }

        let var m = Machine();
        try
            m.id = 10;
            m.id
        catch _:
            'pass'
        "#, "'pass'");

        assert_sapl_eq(r#"
        struct Machine {
            pub def var id = 0
        }

        let m = Machine();
        try
            m.id = 10;
            m.id
        catch _:
            'pass'
        "#, "'pass'");

        assert_sapl_eq(r#"
        struct Person {
            def secret = 0

            fun get_password {
                self.secret
            }
        }

        let adam = Person();
        try:
            adam.secret
        catch _:
            true
        &&
        try
            adam.get_password()
        catch _:
            true
        "#, "true")   
    }

    #[test]
    fn type_tests() {
        assert_sapl_eq(r#"
        type Person {
            pub def name, age

            pub fun speak {
                "Hello. I am " + self.name 
            }
        }

        let my_type = Person;

        struct Baby : Person {
            fun Baby name {
                self.age = 0;
                self.name = name
            }

            pub fun speak {
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
        "#, 
        "['Goo-goo-ga-ga', 0, 'Hello. I am Bobby', 10]");

        assert_sapl_eq(r#"
        type Counter {
            def var count = &&0

            fun inc {
                self.count <- *self.count + 1
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
                }

                call_it()
            }
            
        }

        Obj();
        Obj();
        let a = Obj();
        (a.get_count(), a.a_num)
        "#, "(3, 10)")
    }

    #[test]
    fn cast_test() {
        assert_sapl_eq("true as int", "1");
        assert_sapl_eq("[43, 10, 'Hello'] as string", r#""[43, 10, Hello]""#);
        assert_sapl_eq("'3400' as int", "3400");
        assert_sapl_eq(r#"
        let act = {'name': 'Key', 'age': 53} as array;
        let exp = [('name', 'Key'), ('age', 53)];
        act == exp || act == exp[exp.len() .. 0]"#, "true");

        assert_sapl_eq("5 is some", "true");
        assert_sapl_eq("(2, 5) is tuple_2", "true");
        assert_sapl_eq("[100, 20] is map", "false");
    }

    #[test]
    fn scope_test() {
        assert_parse_str_eq("self::name", Ast::Name("self::name".to_owned()));

        assert_sapl_eq(r#"
        let has_5 = array::contains(?, 5);
        let lst = [10, 7];
        let lst2 = [5, 500, 300];
        !has_5(lst) && has_5(lst2)
        "#, "true");

        assert_sapl_eq(r#"
        [50, 10].array::contains(10)
        "#, "true");
    }

    #[test]
    fn template_test() {
        assert_sapl_eq(r#"
        template("examples/template_test1.txt", {'name': 'Joe', 'count': 10})
        "#, r#"'Hi! My name is Joe\nJoe\nJoe\nJoe\nJoe\nJoe\nJoe\nJoe\nJoe\nJoe\n'"#);
    }

    #[test]
    fn include_test() {
       assert_sapl_eq(r#"
        let f = include "examples/im_in_test.sapl";
        extern_func(20, 10) + f(5) + const
        "#, "38");

        assert_sapl_eq(r#"
        pub let name = 'str';

        pub fun do x {
            x + x + x
        }

        do(name)
        "#, "'strstrstr'");

        assert_parse_str_eq("import examples::im_in_test as test", 
            Ast::Import("examples/im_in_test.sapl".to_owned(), Some("test".to_owned())));

        assert_sapl_eq(r#"
        import examples::im_in_test as test;
        test::extern_func(10, 20)
        "#, "23");

        assert_sapl_eq(r#"
        import examples::im_in_test as test;

        try:
            test::const
        catch _:
            true
        "#, "true");

        assert_sapl_eq(r#"
        import examples::im_in_test::*;

        extern_func(10, 20)
        "#, "23");

        assert_sapl_eq(r#"
        import examples::im_in_test;

        examples::im_in_test::extern_func(10, 20)
        "#, "23");
    }

    #[test]
    fn more_class_tests() {
        assert_sapl_eq(r#"
        struct Class {
            pub def a_num

            fun Class x {
                self.a_num = self.setup(x + 20)               
            }

            fun setup y {
                self.do_it(y * 2)
            }

            fun do_it z {
                z - 3
            }

            pub fun do_it_again x {
                self.setup(x + 20)
            }
        }

        let c = Class(20);
        c.a_num + c.do_it_again(20)
        "#, "154");

        assert_sapl_eq(r#"
        struct Test {
            pub def list = []

            fun Test x {
                if x is array:
                    for e in x:
                        self.list.push_back(e)
                else:
                    self.list.push_back(x)
            }
        }

        let t = Test(10);
        t.list.contains(10)
        "#, "true");

        assert_sapl_eq(r#"
        struct Test {
            pub def var val
        }

        let var t = Test();
        t.val = 20;
        t.val
        "#, "20");

        assert_sapl_eq(r#"
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
        "#, "(6, [6, 9, 8, 7], 4)");

        assert_sapl_eq(r#"
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
        buf
        "#, "[5, 4, 3, 2, 1]");
    }

    #[test]
    fn str_tests() {
        assert_sapl_eq("'😀'", "'😀'");
        assert_sapl_eq("'hello'.contains('el')", "true");
        assert_sapl_eq("'My name is Joe '.split(' ')", "['My', 'name', 'is', 'Joe', '']");
    }

    #[test]
    fn string_test() {
        use Res::Vl;
        assert_eval_eq(r#"'\n'"#, Vl(Values::Str("\n".to_owned())));
        assert_eval_eq(r#"'Hello\n'"#, Vl(Values::Str("Hello\n".to_owned())));
        assert_eval_eq(r#"'\tHello\nGoodbye\n'"#, Vl(Values::Str("\tHello\nGoodbye\n".to_owned())));
        assert_eval_eq(r#"'\\n'"#, Vl(Values::Str("\\n".to_owned())));
    }

    #[test]
    #[should_panic]
    fn mut_ref_from_immu(){
        assert_val_eq(r#"
        let name = 'DC';
        let x = &&name
        "#, Values::Unit);
    }
}