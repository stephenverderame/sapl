use std::io::*;
use std::collections::VecDeque;

#[derive(PartialEq, Debug)]
pub enum Tokens {
    Integer(i32),
    Float(f64),
    TString(String),
    Bool(bool),
    OpPlus, OpMinus, OpMult, OpDiv, OpMod, OpExp,
    OpLor, OpLand, OpOr, OpAnd, OpEq, OpLt, OpGt, 
    OpLeq, OpGeq, OpNeq,
    LParen, RParen,
}

/// Converts an input stream into a deque of tokens
pub fn tokenize(reader: impl Read) -> VecDeque<Tokens> {
    let mut r = BufReader::new(reader);
    let mut buf = Vec::<u8>::new();
    let mut deque = VecDeque::<Tokens>::new();
    let mut fsm = TokenizerFSM::new();
    while r.read_until(b'\n', &mut buf)
        .expect("Failed to read from tokenizer input") != 0 
    {
        let mut t_line = fsm.tokenize_line(&buf).expect("Could not tokenize line");
        deque.append(&mut t_line);
    }
    deque
    
}
#[derive(PartialEq, Debug, Clone, Copy)]
enum TokenizerStates {
    Init,
    ReadInt,
    ReadFloat,
    ReadError,
    ReadMinus,
    ReadString(u8),
    ReadOperator,
    ReadLang,
    ReadId,
}

struct TokenizerFSM {
    state: TokenizerStates,
    input: String,
}

impl TokenizerFSM {

    pub fn new() -> TokenizerFSM {
        TokenizerFSM {
            state: TokenizerStates::Init,
            input: String::new(),
        }
    }

    /// Converts a line in the source file into a stream of tokens
    pub fn tokenize_line(&mut self, line: &[u8]) -> Result<VecDeque<Tokens>> {
        let mut d = VecDeque::<Tokens>::new();
        let buf_line = TokenizerFSM::add_eof_to_buf(line);
        for c in buf_line {
            let (next_state, tok) = self.state_transition(c);
            self.append_to_token_stream(&mut d, tok);                    
            self.append_char_to_input(c, next_state);
            self.state = next_state;
            if self.state == TokenizerStates::ReadError { 
                return Err(Error::from(ErrorKind::NotFound)); 
            }
        }
        Ok(d)
    }

    /// Adds a whitespace character to `line` to indicate the end of the line
    fn add_eof_to_buf(line: &[u8]) -> Vec<u8> {
        let mut b = Vec::from(line);
        b.push(b' ');
        b
    }
    
    /// Gets a `(next_state, token)` tuple that occurs from
    /// reading `input` from the current FSM state
    /// `next_state` is the next FSM state
    /// `token` is the token to append to the token stream or `None`
    fn state_transition(&self, input: u8) 
        -> (TokenizerStates, Option<Tokens>) 
    {
        match &self.state {
            //lex strings
            //must be before everything else
            TokenizerStates::ReadString(delim) if input == *delim => 
                (TokenizerStates::Init, self.done_parse_token()),
            TokenizerStates::ReadString(_) => (self.state, None), 
            _ if input == b'\'' || input == b'"' => 
                (TokenizerStates::ReadString(input), self.done_parse_token()),

            // lex numbers
            TokenizerStates::Init | TokenizerStates::ReadMinus 
            | TokenizerStates::ReadInt if input == b'.' => 
                (TokenizerStates::ReadFloat, None),
            TokenizerStates::Init if input == b'-' => 
                (TokenizerStates::ReadMinus, None),
            TokenizerStates::ReadFloat if input == b'.' =>
                (TokenizerStates::ReadError, None),
            TokenizerStates::ReadInt | TokenizerStates::ReadFloat
                if input.is_ascii_digit() => (self.state, None),
            TokenizerStates::ReadMinus if input.is_ascii_digit() =>
                (TokenizerStates::ReadInt, None),
            _ if input.is_ascii_digit() && self.state != TokenizerStates::ReadId 
                => (TokenizerStates::ReadInt, self.done_parse_token()),

            // lex ops
            TokenizerStates::ReadMinus | TokenizerStates::ReadOperator
                if TokenizerFSM::is_op_symbol(input) => 
                    (TokenizerStates::ReadOperator, None),
             _ if TokenizerFSM::is_op_symbol(input) =>
                (TokenizerStates::ReadOperator, self.done_parse_token()),

            //lex lang tokens
            TokenizerStates::ReadLang => 
                (TokenizerFSM::state_of_input(input), self.done_parse_token()),
            _ if TokenizerFSM::is_lang_symbol(input) =>
                (TokenizerStates::ReadLang, self.done_parse_token()),

            //lex names + keys
            //must be after numbers
            TokenizerStates::ReadId if TokenizerFSM::is_name_character(input) =>
                (TokenizerStates::ReadId, None),
            _ if TokenizerFSM::is_name_character(input) =>
                (TokenizerStates::ReadId, self.done_parse_token()),
            
            _ if input.is_ascii_whitespace() => 
                (TokenizerStates::Init, self.done_parse_token()),        

            _ => panic!("Unknown state transition: {:?} to {}", &self.state, input as char),
        }
    }

    /// Gets the next state based on `c`
    /// Used when a state has multiple possible transitions
    fn state_of_input(c: u8) -> TokenizerStates {
        match c {
            s if s.is_ascii_digit() => TokenizerStates::ReadInt,
            b'.' => TokenizerStates::ReadFloat,
            b'-' => TokenizerStates::ReadMinus,
            b'\'' => TokenizerStates::ReadString(b'\''),
            b'"' => TokenizerStates::ReadString(b'"'),
            s if s.is_ascii_whitespace() => TokenizerStates::Init,
            s if TokenizerFSM::is_lang_symbol(s) => TokenizerStates::ReadLang,
            s if TokenizerFSM::is_op_symbol(s) => TokenizerStates::ReadOperator,
            s if TokenizerFSM::is_name_character(s) => TokenizerStates::ReadId,
            s => panic!("Unknown symbol {}", s as char),
        }
    }

    /// Appends `tok` to `stream` if `tok` is not empty or the Ignore token
    /// If a token is appended, the internal token buffer is cleared
    fn append_to_token_stream(&mut self, stream: &mut VecDeque<Tokens>, 
        tok: Option<Tokens>)
    {
        if let Some(x) = tok {
            self.input.clear();
            stream.push_back(x)    
        } 

    }

    /// Appends `c` to the input buffer it it is not a whitespace
    /// or string delimiter
    fn append_char_to_input(&mut self, c: u8, next_state: TokenizerStates) {
        match (self.state, next_state) {
            (TokenizerStates::ReadString(delim), _) |
            (_, TokenizerStates::ReadString(delim)) if delim == c => (),
            (TokenizerStates::ReadString(_), _) |
            (_, TokenizerStates::ReadString(_)) =>
                self.input.push(c as char),
            _ if !c.is_ascii_whitespace() =>
                self.input.push(c as char),
            _ => (),
        }
    }

    /// Signals that a whitespace character has occurred and 
    /// an entire token has been read
    fn done_parse_token(&self) -> Option<Tokens> {
        //self.input = self.input.trim_left().to_owned();
        match &self.state {
            TokenizerStates::ReadInt => self.parse_cur_as_int(),
            TokenizerStates::ReadFloat => self.parse_cur_as_float(),
            TokenizerStates::ReadString(_) => self.parse_cur_as_str(),
            TokenizerStates::ReadOperator 
            | TokenizerStates::ReadMinus => self.parse_cur_as_op(),
            TokenizerStates::ReadLang => self.parse_cur_as_lang(),
            TokenizerStates::Init => None,
            TokenizerStates::ReadId => self.parse_cur_as_name(),
            _ => panic!("Error converting '{}' to token in state {:?}", self.input, self.state),
        }
    }

    /// Parses everything that is in the current token buffer
    /// as an integer
    fn parse_cur_as_int(&self) -> Option<Tokens> {
        match &self.input.parse::<i32>() {
            Err(_) => panic!("Cannot parse int: \"{}\"", &self.input),
            Ok(x) => Some(Tokens::Integer(*x)),
        }
        
    }

    /// Parses everything in the current token buffer
    /// as a float
    fn parse_cur_as_float(&self) -> Option<Tokens> {
        match &self.input.parse::<f64>() {
            Err(_) => panic!("Cannot parse float"),
            Ok(x) => Some(Tokens::Float(*x)),
        }
    }

    /// Parses the current token buffer as a string
    /// Requires that the string delimiters (' or ")
    /// Are not included in the input buffer
    fn parse_cur_as_str(&self) -> Option<Tokens> {
        if !self.input.is_ascii() {
            None
        } else {
            Some(Tokens::TString(self.input.clone()))
        }
    }

    /// True if `c` is a character used in operators
    fn is_op_symbol(c: u8) -> bool {
        match c {
            b'+' | b'-' | b'&' | b'|'
            | b'/' | b'*' | b'.' | b'%'
            | b'^' | b'@' | b'!' | b'#'
            | b'=' | b'~' | b'<' | b'>'
            | b'?' | b':' => true,
            _ => false,
        }
    }

    /// True if `c` is a language symbol such as `()` or `{}`
    fn is_lang_symbol(c: u8) -> bool {
        match c {
            b'(' | b')' | b'{' | b'}'
            | b';' => true,
            _ => false,
        }
    }

    /// Parses the current token buffer as an operator
    fn parse_cur_as_op(&self) -> Option<Tokens> {
        match &self.input[..] {
            "+" => Some(Tokens::OpPlus),
            "-" => Some(Tokens::OpMinus),
            "*" => Some(Tokens::OpMult),
            "/" => Some(Tokens::OpDiv),
            "%" => Some(Tokens::OpMod),
            "**" => Some(Tokens::OpExp),
            "&&" => Some(Tokens::OpLand),
            "||" => Some(Tokens::OpLor),
            "|" => Some(Tokens::OpOr),
            "&" => Some(Tokens::OpAnd),
            "<" => Some(Tokens::OpLt),
            ">" => Some(Tokens::OpGt),
            "<=" => Some(Tokens::OpLeq),
            ">=" => Some(Tokens::OpGeq),
            "==" => Some(Tokens::OpEq),
            "!=" => Some(Tokens::OpNeq),
            _ => panic!("'{}' is not a recognized operator", self.input),
        }
    }

    fn parse_cur_as_lang(&self) -> Option<Tokens> {
        match &self.input[..] {
            "(" => Some(Tokens::LParen),
            ")" => Some(Tokens::RParen),
            _ => panic!("\"{}\" is not a recognized language symbol", self.input),
        }
    }

    fn is_name_character(c: u8) -> bool {
        c.is_ascii_alphanumeric() || c == b'_'
    }

    fn parse_cur_as_name(&self) -> Option<Tokens> {
        match &self.input[..] {
            "true" => Some(Tokens::Bool(true)),
            "false" => Some(Tokens::Bool(false)),
            _ => panic!("Unknown name {}", self.input),
        }
    }
    
}
