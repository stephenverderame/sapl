mod lexer;
mod parser;
mod evaluator;

pub use evaluator::Values;
use std::io::Read;

pub fn parse_sapl(input: impl Read) -> Result<Values, String> {
    let mut tokens = lexer::tokenize(input);
    let ast = parser::parse(&mut tokens);
    if let Ok(ast) = ast {
        evaluator::eval(ast)
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
        assert_eq!(evaluator::eval(ast).unwrap(), val);

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

        tokens = lexer::tokenize(".45".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Float(0.45)]);

        tokens = lexer::tokenize("-.77".as_bytes());
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
    fn lex_name_test() {
        let toks = lexer::tokenize("true || false&&true".as_bytes());
        assert_toks_eq(&toks, vec![Tokens::Bool(true),
            Tokens::OpLor, Tokens::Bool(false), Tokens::OpLand,
            Tokens::Bool(true)]);
    }

    #[test]
    fn literal_stream_test() {
        let tokens = lexer::tokenize("134     'Cat'  -10.5".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(134),
            Tokens::TString("Cat".to_owned()), Tokens::Float(-10.5)])
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
    }
}
