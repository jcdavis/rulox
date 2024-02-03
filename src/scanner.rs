
use std::{str::Chars, collections::HashMap};

pub struct Scanner<'a> {
    source: Chars<'a>,
    current: Option<char>,
    next: Option<char>,
    buffer: String,
    line: u32,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum TokenType {
  // Single-character tokens.
  LeftParen,RightParen,
  LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus,
  Semicolon, Slash, Star,
  // One or two character tokens.
  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,
  // Literals.
  Identifier, String, Number,
  // Keywords.
  And, Class, Else, False,
  For, Fun, If, Nil, Or,
  Print, Return, Super, This,
  True, Var, While,

  Error, Eof
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub contents: String,
    pub line: u32,
}

impl Scanner<'_> {
    pub fn new(source_str: &str) -> Scanner {
        let mut chars = source_str.chars();
        let current = chars.next();
        let next = chars.next();
        Scanner {
            source: chars,
            current,
            next,
            buffer: String::new(),
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.buffer.clear();
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();
        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                if self.matches('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.matches('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.matches('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.matches('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            '"' => self.string(),
            _ if Self::is_digit(c) => self.digit(),
            _ if Self::is_alpha(c) => self.identifier(),
            _ => self.error_token("unfinished"),
        }
    }


    fn matches(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(c) => {
                if expected == c {
                    self.advance();
                    true
                } else {
                    false
                }
            },
            None => false,
        }
    }

    fn advance(&mut self) -> char {
        let unwrapped = self.current.expect("didn't gaurd?");
        self.buffer.push(unwrapped);
        self.current = self.next;
        self.next = self.source.next();
        unwrapped
    }

    fn make_token(&mut self, token_type: TokenType) -> Token {
        let contents = std::mem::take(&mut self.buffer);
        Token {
            token_type,
            contents,
            line: self.line,
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            token_type: TokenType::Error,
            contents: message.to_string(),
            line: self.line,
        }
    }

    fn is_at_end(&self) -> bool {
        self.next.is_none()
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\r') | Some('\t') => {self.advance();} ,
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                },
                Some('/') if self.peek_next() == Some('/') => {
                    while self.peek() != Some('\n') && !self.is_at_end() {
                        self.advance();
                    }
                },
                _ => break,
            };
        }
    }

    fn string(&mut self) -> Token {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            self.error_token("Unterminated string")
        } else {
            // Closing
            self.advance();
            self.make_token(TokenType::String)
        }
    }

    fn digit(&mut self) -> Token {
        while self.peek().is_some_and(Self::is_digit) {
            self.advance();
        }
        if self.peek() == Some('.') && self.peek_next().is_some_and(Self::is_digit) {
            // Consume .
            self.advance();
            while self.peek() >= Some('0') && self.peek() <= Some('9') {
                self.advance();
            }
        }
        self.make_token(TokenType::Number)
    }

    fn identifier(& mut self) -> Token {
        while self.peek().is_some_and(|c| Self::is_digit(c) || Self::is_alpha(c)) {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        let keywords = HashMap::from([
            ("and", TokenType::And),
            ("class", TokenType::Class),
            ("else", TokenType::Else),
            ("false", TokenType::False),
            ("for", TokenType::For),
            ("fun", TokenType::Fun),
            ("if", TokenType::If),
            ("nil", TokenType::Nil),
            ("or", TokenType::Or),
            ("print", TokenType::Print),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("var", TokenType::Var),
            ("while", TokenType::While)
        ]);
        *keywords.get(self.buffer.as_str()).unwrap_or(&TokenType::Identifier)
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_alpha(c: char) -> bool {
        c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
    }

    fn peek(&self) -> Option<char> {
        self.current
    }

    fn peek_next(&self) -> Option<char> {
        self.next
    }
}