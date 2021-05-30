use std::io::*;
use std::collections::VecDeque;

#[derive(PartialEq, Debug)]
pub enum Tokens {
    Integer(i32),
    Float(f64),
    TString(String),
    Ignore,
    OpPlus, OpMinus, OpMult, OpDiv, OpMod, OpExp,
    OpLor, OpLand, OpOr, OpAnd,
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
            self.append_to_token_stream(&mut d, tok, next_state, c as char);
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
    
    /// Gets a `(next_state, token)` pair that occurs from
    /// reading `input` from the current FSM state
    fn state_transition(&self, input: u8) 
        -> (TokenizerStates, Option<Tokens>) 
    {
        match &self.state {
            TokenizerStates::Init | TokenizerStates::ReadMinus 
                if input.is_ascii_digit() => (TokenizerStates::ReadInt, None),

            TokenizerStates::Init if input == b'\'' || input == b'"'
                => (TokenizerStates::ReadString(input), Some(Tokens::Ignore)),

            TokenizerStates::Init | TokenizerStates::ReadMinus 
            | TokenizerStates::ReadInt if input == b'.' 
                => (TokenizerStates::ReadFloat, None),

            TokenizerStates::Init if input == b'-' 
                => (TokenizerStates::ReadMinus, None),

            TokenizerStates::ReadInt | TokenizerStates::ReadFloat
                if input.is_ascii_digit() =>  (self.state, None),

            TokenizerStates::ReadString(delim) if input == *delim
                => (TokenizerStates::Init, self.done_parse_token()),

            TokenizerStates::ReadString(_) => (self.state, None), 

            TokenizerStates::Init | TokenizerStates::ReadMinus 
            | TokenizerStates::ReadOperator
                if TokenizerFSM::is_op_symbol(input) => 
                    (TokenizerStates::ReadOperator, None),

            TokenizerStates::ReadOperator | TokenizerStates::ReadMinus 
                if !TokenizerFSM::is_op_symbol(input) => 
                    (TokenizerStates::Init, self.done_parse_token()),
            
            _ if input.is_ascii_whitespace() => 
                (TokenizerStates::Init, self.done_parse_token()),

            TokenizerStates::ReadInt | TokenizerStates::ReadFloat 
                if !input.is_ascii_digit() 
                => (TokenizerStates::ReadError, None),             

            _ => panic!("Unknown state transition: {:?} to {}", &self.state, input as char),
        }
    }

    /// Appends `tok` to `stream` if `tok` is not empty or the Ignore token
    /// If a token is appended, the internal token buffer is cleared
    fn append_to_token_stream(&mut self, stream: &mut VecDeque<Tokens>, 
        tok: Option<Tokens>, next_state: TokenizerStates, input: char)
    {
        if let Some(x) = tok {
            if x != Tokens::Ignore { stream.push_back(x); }
            self.input.clear();
            self.state = TokenizerStates::Init;
        } else {
            self.input.push(input);
        }
    }

    /// Signals that a whitespace character has occurred and 
    /// an entire token has been read
    fn done_parse_token(&self) -> Option<Tokens> {
        match &self.state {
            TokenizerStates::ReadInt => self.parse_cur_as_int(),
            TokenizerStates::ReadFloat => self.parse_cur_as_float(),
            TokenizerStates::ReadString(_) => self.parse_cur_as_str(),
            TokenizerStates::ReadOperator 
            | TokenizerStates::ReadMinus => self.parse_cur_as_op(),
            TokenizerStates::Init => Some(Tokens::Ignore),
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
            _ => panic!("'{}' is not a recognized operator", self.input),
        }
    }
    
}
