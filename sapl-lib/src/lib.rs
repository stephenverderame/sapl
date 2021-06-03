mod lexer;
mod parser;
mod evaluator;

pub use evaluator::Values;
use std::io::Read;

pub fn parse_sapl(input: impl Read) -> Result<Values, String> {
    let mut tokens = lexer::tokenize(input);
    let ast = parser::parse(&mut tokens);
    if let Ok(ast) = ast {
        evaluator::evaluate(&ast)
    } else {
        Err(ast.unwrap_err())
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser;
    use crate::evaluator;
    use crate::lexer::Tokens;
    use crate::parser::Ast;
    use crate::parser::Op;
    use crate::evaluator::Values;
    use std::collections::VecDeque;

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
        let mut ts = lexer::tokenize(code.as_bytes());
        let ast = parser::parse(&mut ts).unwrap();
        assert_eq!(evaluator::evaluate(&ast).unwrap(), val);

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
                if 'cat' != true:
                    if 'apple' > 'banana' {
                        (10 + 3) ** 4 % 19
                    }
                else 
                    (10 + 4) ** 3 % 17
        "#, Values::Int(7));

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
            Box::new(Ast::Let("x".to_owned(), Box::new(Ast::VInt(5)))),
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
            Box::new(Ast::FnApply("func".to_owned(), vec![Box::new(Ast::VInt(3))]))));

        assert_val_eq(r#"
        fun fact x {
            if x <= 1: 1
            else
                x * fact(x - 1)
        }

        fact(2)
        "#, Values::Int(2));
    }
}
