use std::collections::HashMap;

fn main() {
    let json = parse_json("{\"hello\":1}").unwrap();
    println!("{:?}", json);
}

#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

#[derive(Debug)]
pub struct JsonError {
    pub message: String,
    pub position: usize,
    pub kind: JsonErrorKind,
}

#[derive(Debug)]
pub enum JsonErrorKind {
    UnexpectedCharacter,
    InvalidLiteral,
    InvalidNumber,
    InvalidString,
}

pub fn parse_json(input: &str) -> Result<JsonValue, JsonError> {
    parse_json_impl(input.as_bytes())
}

pub fn parse_json_bytes(input: &[u8]) -> Result<JsonValue, JsonError> {
    parse_json_impl(input)
}

fn parse_json_impl(input: &[u8]) -> Result<JsonValue, JsonError> {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;

    let mut parser = Parser::new(&tokens);
    parser.parse()
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    BeginObject,
    EndObject,
    BeginArray,
    EndArray,
    Colon,
    Comma,
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

struct Tokenizer<'a> {
    input: &'a [u8],
    position: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self { input, position: 0 }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, JsonError> {
        let mut tokens = Vec::new();

        while self.position < self.input.len() {
            self.skip_whitespace();

            let token = match self.current_byte() {
                b'{' => {
                    self.increment_position(1);
                    Token::BeginObject
                }
                b'}' => {
                    self.increment_position(1);
                    Token::EndObject
                }
                b'[' => {
                    self.increment_position(1);
                    Token::BeginArray
                }
                b']' => {
                    self.increment_position(1);
                    Token::EndArray
                }
                b':' => {
                    self.increment_position(1);
                    Token::Colon
                }
                b',' => {
                    self.increment_position(1);
                    Token::Comma
                }
                b'"' => self.tokenize_string()?,
                b'0'..=b'9' | b'-' => self.tokenize_number()?,
                b'n' | b't' | b'f' => self.tokenize_literal()?,
                _ => {
                    return Err(JsonError {
                        message: "Unexpected character".to_string(),
                        position: self.position,
                        kind: JsonErrorKind::UnexpectedCharacter,
                    });
                }
            };

            tokens.push(token);
        }

        Ok(tokens)
    }

    fn tokenize_literal(&mut self) -> Result<Token, JsonError> {
        if self.match_literal(b"null") {
            return Ok(Token::Null);
        }

        if self.match_literal(b"true") {
            return Ok(Token::Boolean(true));
        }

        if self.match_literal(b"false") {
            return Ok(Token::Boolean(false));
        }

        Err(JsonError {
            message: "Invalid Literal".to_string(),
            position: self.position,
            kind: JsonErrorKind::InvalidLiteral,
        })
    }

    fn tokenize_string(&mut self) -> Result<Token, JsonError> {
        let mut result = String::new();
        self.increment_position(1);

        while self.has_more() {
            let next: char = match self.current_byte() {
                b'"' => {
                    self.increment_position(1);
                    break;
                }
                b'\\' => {
                    self.increment_position(1);
                    if !self.has_more() {
                        return Err(JsonError {
                            message: "Invalid string".to_string(),
                            position: self.position,
                            kind: JsonErrorKind::InvalidString,
                        });
                    }
                    match self.current_byte() {
                        b'"' => {
                            self.increment_position(1);
                            '"'
                        }
                        b'\\' => {
                            self.increment_position(1);
                            '\\'
                        }
                        b'/' => {
                            self.increment_position(1);
                            '/'
                        }
                        b'b' => {
                            self.increment_position(1);
                            '\u{0008}'
                        }
                        b'f' => {
                            self.increment_position(1);
                            '\u{000c}'
                        }
                        b'n' => {
                            self.increment_position(1);
                            '\n'
                        }
                        b'r' => {
                            self.increment_position(1);
                            '\r'
                        }
                        b't' => {
                            self.increment_position(1);
                            '\t'
                        }
                        b'u' => {
                            self.increment_position(1);

                            if !self.has_more_than(3) {
                                return Err(JsonError {
                                    message: "Invalid string".to_string(),
                                    position: self.position,
                                    kind: JsonErrorKind::InvalidString,
                                });
                            }

                            // Take the next 4 bytes and check they are hex
                            let escaped_bytes = &self.input[self.position..self.position + 4];
                            self.increment_position(4);

                            let hex_str =
                                std::str::from_utf8(escaped_bytes).map_err(|_| JsonError {
                                    message: "Invalid string".to_string(),
                                    position: self.position,
                                    kind: JsonErrorKind::InvalidString,
                                })?;

                            let code_point =
                                u16::from_str_radix(hex_str, 16).map_err(|_| JsonError {
                                    message: "Invalid string".to_string(),
                                    position: self.position,
                                    kind: JsonErrorKind::InvalidString,
                                })?;

                            std::char::from_u32(code_point as u32).ok_or(JsonError {
                                message: "Invalid string".to_string(),
                                position: self.position,
                                kind: JsonErrorKind::InvalidString,
                            })?
                        }
                        _ => {
                            return Err(JsonError {
                                message: "Invalid string".to_string(),
                                position: self.position,
                                kind: JsonErrorKind::InvalidString,
                            });
                        }
                    }
                }
                _ => {
                    let next = self.current_byte();
                    self.increment_position(1);
                    next as char
                }
            };
            result.push(next);
        }

        Ok(Token::String(result))
    }

    fn tokenize_number(&mut self) -> Result<Token, JsonError> {
        let start_pos = self.position;

        if self.has_more() && self.current_byte() == b'-' {
            self.increment_position(1);
        }

        if self.has_more() {
            match self.current_byte() {
                b'0' => {
                    self.increment_position(1);
                }
                b'1'..=b'9' => {
                    self.increment_position(1);
                    self.consume_digits();
                }
                _ => {
                    return Err(JsonError {
                        message: "Invalid number".to_string(),
                        position: self.position,
                        kind: JsonErrorKind::InvalidNumber,
                    });
                }
            }
        }

        if self.has_more() && self.current_byte() == b'.' {
            self.increment_position(1);
            if !self.has_more() || !self.is_digit(self.current_byte()) {
                return Err(JsonError {
                    message: "Invalid number".to_string(),
                    position: self.position,
                    kind: JsonErrorKind::InvalidNumber,
                });
            }
            self.consume_digits();
        }

        if self.has_more() && (self.current_byte() == b'E' || self.current_byte() == b'e') {
            self.increment_position(1);

            if self.has_more() && (self.current_byte() == b'+' || self.current_byte() == b'-') {
                self.increment_position(1);
            }

            if !self.has_more() || !self.is_digit(self.current_byte()) {
                return Err(JsonError {
                    message: "Invalid number".to_string(),
                    position: self.position,
                    kind: JsonErrorKind::InvalidNumber,
                });
            }
            self.consume_digits();
        }

        let number_str =
            std::str::from_utf8(&self.input[start_pos..self.position]).map_err(|_| JsonError {
                message: "Invalid number".to_string(),
                position: start_pos,
                kind: JsonErrorKind::InvalidNumber,
            })?;

        let number = number_str.parse().map_err(|_| JsonError {
            message: "Invalid number".to_string(),
            position: start_pos,
            kind: JsonErrorKind::InvalidNumber,
        })?;

        Ok(Token::Number(number))
    }

    fn has_more(&self) -> bool {
        self.has_more_than(0)
    }

    fn has_more_than(&self, count: usize) -> bool {
        self.position + count < self.input.len()
    }

    fn is_digit(&self, byte: u8) -> bool {
        byte.is_ascii_digit()
    }

    fn increment_position(&mut self, count: usize) {
        self.position += count;
    }

    fn consume_digits(&mut self) {
        while self.position < self.input.len() && self.is_digit(self.current_byte()) {
            self.increment_position(1);
        }
    }

    fn current_byte(&self) -> u8 {
        self.input[self.position]
    }

    fn match_literal(&mut self, literal: &[u8]) -> bool {
        for (i, &ch) in literal.iter().enumerate() {
            if self.position + i >= self.input.len() || self.input[self.position + i] != ch {
                return false;
            }
        }

        self.increment_position(literal.len());
        true
    }

    fn skip_whitespace(&mut self) {
        while self.position < self.input.len() {
            match self.input[self.position] {
                b' ' | b'\n' | b'\r' | b'\t' => self.increment_position(1),
                _ => break,
            }
        }
    }
}

struct Parser<'a> {
    input: &'a [Token],
    position: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a [Token]) -> Self {
        Self { input, position: 0 }
    }

    fn parse(&mut self) -> Result<JsonValue, JsonError> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tokenize_null() {
        let input = "null";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Null]);
    }

    #[test]
    fn tokenize_true() {
        let input = "true";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Boolean(true)]);
    }

    #[test]
    fn tokenize_false() {
        let input = "false";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Boolean(false)]);
    }

    #[test]
    fn tokenize_brackets_and_braces() {
        let input = "{}[]";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::BeginObject,
                Token::EndObject,
                Token::BeginArray,
                Token::EndArray,
            ]
        );
    }

    #[test]
    fn tokenize_colon() {
        let input = ":";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Colon]);
    }

    #[test]
    fn tokenize_comma() {
        let input = ",";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Comma]);
    }

    #[test]
    fn tokenize_number() {
        let input = "123";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Number(123.0)]);
    }

    #[test]
    fn tokenize_negative_number() {
        let input = "-123.45";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Number(-123.45)]);
    }

    #[test]
    fn tokenizes_number_with_exponent() {
        let input = "1.23e4";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Number(1.23e4)]);
    }

    #[test]
    fn tokenize_number_with_negative_exponent() {
        let input = "1.23e-4";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Number(1.23e-4)]);
    }

    #[test]
    fn tokenize_string() {
        let input = "\"hello\"";
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::String("hello".to_string())]);
    }

    #[test]
    fn tokenize_string_with_escape() {
        let input = "\"h\\u00e9llo\\nworld\\\"";

        println!("{}", input);
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::String("héllo\nworld\"".to_string())]);
    }
}
