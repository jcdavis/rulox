
use std::iter::Peekable;
use std::str::Chars;

pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    buffer: String,
    line: u32,
}

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
    token_type: TokenType,
    contents: String,
    line: u32,
}

impl Scanner<'_> {
    pub fn new(source_str: &str) -> Scanner {
        Scanner {
            source: source_str.chars().peekable(),
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
            _ => self.error_token("unfinished"),
        }
    }


    fn matches(&mut self, expected: char) -> bool {
        match self.source.peek() {
            Some(c) => {
                if expected == *c {
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
        self.source.next().expect("didn't gaurd?")
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

    fn is_at_end(&mut self) -> bool {
        self.source.peek().is_none()
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.source.peek() {
                Some(' ') | Some('\r') | Some('\t') => self.advance(),
                Some('\n') => {
                    self.line += 1;
                    self.advance()
                },
                _ => break,
            };
        }
    }
}