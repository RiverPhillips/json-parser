use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct JsonError {
    pub message: String,
    pub position: usize,
    pub kind: JsonErrorKind,
}
#[derive(Debug, Eq, PartialEq)]
pub enum JsonErrorKind {
    UnexpectedCharacter,
    MalformedObject,
    InvalidLiteral,
    InvalidNumber,
    InvalidString,
    UnexpectedEndOfInput,
    UnterminatedString,
    InvalidEscapeSequence,
}

impl JsonError {
    fn unexpected_character(position: usize) -> Self {
        Self {
            message: "Unexpected character".to_string(),
            position,
            kind: JsonErrorKind::UnexpectedCharacter,
        }
    }

    fn invalid_literal(position: usize) -> Self {
        Self {
            message: "Invalid literal".to_string(),
            position,
            kind: JsonErrorKind::InvalidLiteral,
        }
    }

    fn unterminated_string(position: usize) -> Self {
        Self {
            message: "Unterminated string".to_string(),
            position,
            kind: JsonErrorKind::UnterminatedString,
        }
    }

    fn invalid_escape_sequence(position: usize) -> Self {
        Self {
            message: "Invalid escape sequence".to_string(),
            position,
            kind: JsonErrorKind::InvalidEscapeSequence,
        }
    }
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

        while self.has_more() {
            self.skip_whitespace();
            if !self.has_more() {
                break;
            }

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
                    return Err(JsonError::unexpected_character(self.position));
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

        Err(JsonError::invalid_literal(self.position))
    }

    fn tokenize_string(&mut self) -> Result<Token, JsonError> {
        let mut result = String::new();
        let mut string_terminated = false;
        self.increment_position(1);

        while self.has_more() {
            let next: char = match self.current_byte() {
                b'"' => {
                    self.increment_position(1);
                    string_terminated = true;
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
                                return Err(JsonError::invalid_escape_sequence(self.position));
                            }

                            // Take the next 4 bytes and check they are hex
                            let escaped_bytes = &self.input[self.position..self.position + 4];

                            let hex_str = std::str::from_utf8(escaped_bytes)
                                .map_err(|_| JsonError::invalid_escape_sequence(self.position))?;

                            let code_point = u16::from_str_radix(hex_str, 16)
                                .map_err(|_| JsonError::invalid_escape_sequence(self.position))?;

                            let val = std::char::from_u32(code_point as u32)
                                .ok_or(JsonError::invalid_escape_sequence(self.position))?;

                            self.increment_position(4);
                            val
                        }
                        _ => return Err(JsonError::invalid_escape_sequence(self.position)),
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

        if !string_terminated {
            Err(JsonError::unterminated_string(self.position))
        } else {
            Ok(Token::String(result))
        }
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
        if self.position >= self.input.len() {
            return Err(JsonError {
                message: "Unexpected end of input".to_string(),
                position: self.position,
                kind: JsonErrorKind::UnexpectedEndOfInput,
            });
        }

        match &self.input[self.position] {
            Token::BeginObject => self.parse_object(),
            Token::BeginArray => self.parse_array(),
            Token::String(s) => {
                self.position += 1;
                Ok(JsonValue::String(s.clone()))
            }
            Token::Number(n) => {
                self.position += 1;
                Ok(JsonValue::Number(*n))
            }
            Token::Boolean(b) => {
                self.position += 1;
                Ok(JsonValue::Bool(*b))
            }
            Token::Null => {
                self.position += 1;
                Ok(JsonValue::Null)
            }
            _ => Err(JsonError {
                message: "Unexpected token".to_string(),
                position: self.position,
                kind: JsonErrorKind::UnexpectedCharacter,
            }),
        }
    }

    fn parse_object(&mut self) -> Result<JsonValue, JsonError> {
        // Consume '{'
        self.position += 1;

        let mut map = HashMap::new();

        if self.position < self.input.len() && self.input[self.position] == Token::EndObject {
            self.position += 1;
            return Ok(JsonValue::Object(map));
        }

        loop {
            let key = match &self.input[self.position] {
                Token::String(s) => {
                    self.position += 1;
                    s.clone()
                }
                _ => {
                    return Err(JsonError {
                        message: "Expected string".to_string(),
                        position: self.position,
                        kind: JsonErrorKind::UnexpectedCharacter,
                    });
                }
            };

            if self.position >= self.input.len() {
                return Err(JsonError {
                    message: "Unexpected end of input".to_string(),
                    position: self.position,
                    kind: JsonErrorKind::UnexpectedEndOfInput,
                });
            }

            if self.input[self.position] != Token::Colon {
                return Err(JsonError {
                    message: "Expected ':'".to_string(),
                    position: self.position,
                    kind: JsonErrorKind::UnexpectedCharacter,
                });
            }

            self.position += 1;

            let value = self.parse()?;
            map.insert(key, value);

            if self.position >= self.input.len() {
                return Err(JsonError {
                    message: "Unexpected end of input".to_string(),
                    position: self.position,
                    kind: JsonErrorKind::UnexpectedEndOfInput,
                });
            }

            match self.input[self.position] {
                Token::Comma => {
                    self.position += 1;
                }
                Token::EndObject => {
                    self.position += 1;
                    return Ok(JsonValue::Object(map));
                }
                _ => {
                    return Err(JsonError {
                        message: "Expected ',' or '}'".to_string(),
                        position: self.position,
                        kind: JsonErrorKind::MalformedObject,
                    });
                }
            }
        }
    }

    fn parse_array(&mut self) -> Result<JsonValue, JsonError> {
        // Consume the [
        self.position += 1;

        let mut arr = Vec::new();
        if self.position < self.input.len() && self.input[self.position] == Token::EndArray {
            self.position += 1;
            return Ok(JsonValue::Array(arr));
        }

        loop {
            let value = self.parse()?;
            arr.push(value);

            // Next token should be a comma or the end of the array
            match self.input[self.position] {
                Token::Comma => {
                    self.position += 1;
                }
                Token::EndArray => {
                    self.position += 1;
                    return Ok(JsonValue::Array(arr));
                }
                _ => {
                    return Err(JsonError {
                        message: "Expected ',' or ']'".to_string(),
                        position: self.position,
                        kind: JsonErrorKind::UnexpectedCharacter,
                    });
                }
            }
        }
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
    fn returns_error_on_unexpected_character() {
        let input = "!";
        let error = Tokenizer::new(input.as_bytes()).tokenize().unwrap_err();
        assert_eq!(error, JsonError::unexpected_character(0),);
    }

    #[test]
    fn returns_error_on_invalid_literal() {
        let input = "nil";
        let error = Tokenizer::new(input.as_bytes()).tokenize().unwrap_err();
        assert_eq!(error, JsonError::invalid_literal(0));
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
        let input = "\"h\\u00e9llo\\nworld\\\"\"";

        dbg!(input);

        println!("{}", input);
        let tokens = Tokenizer::new(input.as_bytes()).tokenize().unwrap();
        assert_eq!(tokens, vec![Token::String("héllo\nworld\"".to_string())]);
    }

    #[test]
    fn parse_test_object() {
        let input = r#"{"hello":1}"#;

        let mut expected_map = HashMap::new();
        expected_map.insert("hello".to_string(), JsonValue::Number(1.0));

        assert_eq!(parse_json(input).unwrap(), JsonValue::Object(expected_map));
    }

    #[test]
    fn returns_error_on_unterminated_string() {
        let input = r#""hello"#;
        let error = Tokenizer::new(input.as_bytes()).tokenize().unwrap_err();

        assert_eq!(error, JsonError::unterminated_string(6));
    }
}
