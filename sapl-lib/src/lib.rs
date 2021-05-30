mod lexer;
mod parser;

#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser;
    use crate::lexer::Tokens;
    use crate::parser::Ast;
    use crate::parser::Op;
    use std::collections::VecDeque;

    fn assert_toks_eq(left: &VecDeque<Tokens>, right: Vec<Tokens>) {
        assert_eq!(*left, VecDeque::from(right));
    }

    fn assert_parse_eq(left: Vec<Tokens>, right: Ast) {
        let mut vd = VecDeque::from(left);
        assert_eq!(parser::parse(&mut vd).unwrap(), right);
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
    }

    #[test]
    fn literal_stream_test() {
        let mut tokens = lexer::tokenize("134     'Cat'  -10.5".as_bytes());
        assert_toks_eq(&tokens, vec![Tokens::Integer(134),
            Tokens::TString("Cat".to_owned()), Tokens::Float(-10.5)])
    }

    #[test]
    #[should_panic]
    fn multiple_decimal_pts() {
        let mut tokens = lexer::tokenize("10.0.0".as_bytes());
    }

    #[test]
    #[should_panic]
    fn multiple_minus_sign() {
        let mut tokens = lexer::tokenize("--10".as_bytes());
    }

    #[test]
    fn arithmetic_parse() {
        assert_parse_eq(
            vec![Tokens::Integer(10), Tokens::OpPlus, Tokens::Integer(200)], 
            Ast::Bop(Box::new(Ast::VInt(10)), Op::Plus, Box::new(Ast::VInt(200)))
        );
    }
}
