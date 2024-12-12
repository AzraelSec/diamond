use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Illegal(String),
    Eof,
    //
    Ident(String),
    Int(String),
    String(String),
    //
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    //
    EQ,
    NotEq,
    LT,
    LTET,
    GT,
    GTET,

    //
    Comma,
    Semicolon,
    Colon,
    //
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    //
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    While,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal(s) => write!(f, "Illegal({})", s),
            Token::Eof => write!(f, "EOF"),
            Token::Ident(s) => write!(f, "{}", s.to_string()),
            Token::Int(n) => write!(f, "{}", n.to_string()),
            Token::String(s) => write!(f, "{}", s.to_string()),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::EQ => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LT => write!(f, "<"),
            Token::LTET => write!(f, "<="),
            Token::GT => write!(f, ">"),
            Token::GTET => write!(f, ">="),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Lbracket => write!(f, "["),
            Token::Rbracket => write!(f, "]"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::While => write!(f, "while"),
        }
    }
}

impl Token {
    pub fn from_char(c: Option<char>, peek: Option<char>) -> (Option<Token>, bool) {
        match c {
            // NOTE: we're not handling Token::Bang and Token::Assign since they could be EQ/NotEq
            Some('=') => match peek {
                Some('=') => (Some(Token::EQ), true),
                _ => (Some(Token::Assign), false),
            },
            Some('+') => (Some(Token::Plus), false),
            Some('-') => (Some(Token::Minus), false),
            Some('!') => match peek {
                Some('=') => (Some(Token::NotEq), true),
                _ => (Some(Token::Bang), false),
            },
            Some('*') => (Some(Token::Asterisk), false),
            Some('/') => (Some(Token::Slash), false),
            //
            Some('<') => match peek {
                Some('=') => (Some(Token::LTET), true),
                _ => (Some(Token::LT), false),
            },
            Some('>') => match peek {
                Some('=') => (Some(Token::GTET), true),
                _ => (Some(Token::GT), false),
            },
            //
            Some(',') => (Some(Token::Comma), false),
            Some(';') => (Some(Token::Semicolon), false),
            Some(':') => (Some(Token::Colon), false),
            //
            Some('(') => (Some(Token::Lparen), false),
            Some(')') => (Some(Token::Rparen), false),
            Some('{') => (Some(Token::Lbrace), false),
            Some('}') => (Some(Token::Rbrace), false),
            Some('[') => (Some(Token::Lbracket), false),
            Some(']') => (Some(Token::Rbracket), false),
            //
            None => (Some(Token::Eof), false),
            _ => (None, false),
        }
    }

    pub fn from_word(w: String) -> Option<Token> {
        match w.as_str() {
            "fn" => Some(Token::Function),
            "let" => Some(Token::Let),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "return" => Some(Token::Return),
            "while" => Some(Token::While),
            _ => None,
        }
    }

    pub fn supports_infix(&self) -> bool {
        matches!(
            &self,
            Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::EQ
                | Token::NotEq
                | Token::LT
                | Token::LTET
                | Token::GT
                | Token::GTET
                | Token::Lparen
                | Token::Lbracket
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_word() {
        let tests = vec![
            ("fn", Some(Token::Function)),
            ("let", Some(Token::Let)),
            ("true", Some(Token::True)),
            ("false", Some(Token::False)),
            ("if", Some(Token::If)),
            ("else", Some(Token::Else)),
            ("return", Some(Token::Return)),
            ("unknown", None), // Test case for unrecognized input
            ("Fn", None),      // Case sensitivity test
            ("", None),        // Empty string case
        ];

        for (i, (input, expected_token)) in tests.iter().enumerate() {
            let actual_token = Token::from_word(input.to_string());

            assert_eq!(
                actual_token, *expected_token,
                "Test case {}: Word mismatch. Input: '{}', Expected: {:?}, Got: {:?}",
                i, input, expected_token, actual_token
            );
        }
    }

    #[test]
    fn test_from_char() {
        let tests = vec![
            (Some('='), None, Some(Token::Assign), false),
            (Some('='), Some('='), Some(Token::EQ), true),
            (Some('+'), None, Some(Token::Plus), false),
            (Some('-'), None, Some(Token::Minus), false),
            (Some('!'), None, Some(Token::Bang), false),
            (Some('!'), Some('='), Some(Token::NotEq), true),
            (Some('<'), None, Some(Token::LT), false),
            (Some('<'), Some('='), Some(Token::LTET), true),
            (Some('>'), None, Some(Token::GT), false),
            (Some('>'), Some('='), Some(Token::GTET), true),
            (Some(','), None, Some(Token::Comma), false),
            (Some(';'), None, Some(Token::Semicolon), false),
            (Some('('), None, Some(Token::Lparen), false),
            (Some(')'), None, Some(Token::Rparen), false),
            (Some('{'), None, Some(Token::Lbrace), false),
            (Some('}'), None, Some(Token::Rbrace), false),
            (None, None, Some(Token::Eof), false),
            (Some('#'), None, None, false), // An unhandled character case
        ];

        for (i, (c, peek, expected_token, expect_to_read_char)) in tests.iter().enumerate() {
            let (actual_token, actual_to_read_char) = Token::from_char(*c, *peek);

            assert_eq!(
                actual_token, *expected_token,
                "Test case {}: Token mismatch. Expected {:?}, got {:?}",
                i, expected_token, actual_token
            );

            assert_eq!(
                actual_to_read_char, *expect_to_read_char,
                "Test case {}: Read character flag mismatch. Expected {}, got {}",
                i, expect_to_read_char, actual_to_read_char
            );
        }
    }
}
