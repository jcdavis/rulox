
use std::{str::Chars, collections::HashMap};

pub struct Scanner<'a> {
    source: Chars<'a>,
    current: Option<char>,
    next: Option<char>,
    buffer: String,
    line: u32,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
  // Single-character tokens.
  TOKEN_LEFT_PAREN,TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
  // One or two character tokens.
  TOKEN_BANG, TOKEN_BANG_EQUAL,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,
  // Literals.
  TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
  // Keywords.
  TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
  TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
  TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
  TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

  TOKEN_ERROR, TOKEN_EOF
}

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
        if self.is_at_end() {
            return self.make_token(TokenType::TOKEN_EOF);
        }

        let c = self.advance();
        match c {
            '(' => return self.make_token(TokenType::TOKEN_LEFT_PAREN),
            ')' => return self.make_token(TokenType::TOKEN_RIGHT_PAREN),
            '{' => return self.make_token(TokenType::TOKEN_LEFT_BRACE),
            '}' => return self.make_token(TokenType::TOKEN_RIGHT_BRACE),
            ';' => return self.make_token(TokenType::TOKEN_SEMICOLON),
            ',' => return self.make_token(TokenType::TOKEN_COMMA),
            '.' => return self.make_token(TokenType::TOKEN_DOT),
            '-' => return self.make_token(TokenType::TOKEN_MINUS),
            '+' => return self.make_token(TokenType::TOKEN_PLUS),
            '/' => return self.make_token(TokenType::TOKEN_SLASH),
            '*' => return self.make_token(TokenType::TOKEN_STAR),
            '!' => {
                if self.matches('=') {
                    return self.make_token(TokenType::TOKEN_BANG_EQUAL);
                } else {
                    return self.make_token(TokenType::TOKEN_BANG);
                }
            }
            '=' => {
                if self.matches('=') {
                    return self.make_token(TokenType::TOKEN_EQUAL_EQUAL);
                } else {
                    return self.make_token(TokenType::TOKEN_EQUAL);
                }
            }
            '<' => {
                if self.matches('=') {
                    return self.make_token(TokenType::TOKEN_LESS_EQUAL);
                } else {
                    return self.make_token(TokenType::TOKEN_LESS);
                }
            }
            '>' => {
                if self.matches('=') {
                    return self.make_token(TokenType::TOKEN_GREATER_EQUAL);
                } else {
                    return self.make_token(TokenType::TOKEN_GREATER);
                }
            }
            '"' => return self.string(),
            _ if Self::is_digit(c) => return self.digit(),
            _ if Self::is_alpha(c) => return self.identifier(),
            _ => self.error_token("unfinished"),
        }
    }


    fn matches(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(c) => {
                if expected == c {
                    self.source.next();
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
        let contents = std::mem::replace(&mut self.buffer, String::new());
        Token {
            token_type,
            contents,
            line: self.line,
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            token_type: TokenType::TOKEN_ERROR,
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
            self.make_token(TokenType::TOKEN_STRING)
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
        self.make_token(TokenType::TOKEN_NUMBER)
    }

    fn identifier(& mut self) -> Token {
        while self.peek().is_some_and(|c| Self::is_digit(c) || Self::is_alpha(c)) {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        let keywords = HashMap::from([
            ("and", TokenType::TOKEN_AND),
            ("class", TokenType::TOKEN_CLASS),
            ("else", TokenType::TOKEN_ELSE),
            ("false", TokenType::TOKEN_FALSE),
            ("for", TokenType::TOKEN_FOR),
            ("fun", TokenType::TOKEN_FUN),
            ("if", TokenType::TOKEN_IF),
            ("nil", TokenType::TOKEN_NIL),
            ("or", TokenType::TOKEN_OR),
            ("print", TokenType::TOKEN_PRINT),
            ("return", TokenType::TOKEN_RETURN),
            ("super", TokenType::TOKEN_SUPER),
            ("this", TokenType::TOKEN_THIS),
            ("true", TokenType::TOKEN_TRUE),
            ("var", TokenType::TOKEN_VAR),
            ("while", TokenType::TOKEN_WHILE)
        ]);
        *keywords.get(self.buffer.as_str()).unwrap_or(&TokenType::TOKEN_IDENTIFIER)
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii()
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