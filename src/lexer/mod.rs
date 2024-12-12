use token::Token;

pub mod token;

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            None
        } else {
            self.input[self.read_position..].chars().next()
        };

        self.position = self.read_position;
        self.read_position += self.ch.map_or(1, |c| c.len_utf8());
    }

    fn peek_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input[self.read_position..].chars().next()
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match Token::from_char(self.ch, self.peek_char()) {
            (Some(tok), should_read_next) => {
                if should_read_next {
                    self.read_char();
                };
                tok
            }
            (None, _) => {
                if Self::is_string_starter(self.ch) {
                    self.read_string()
                } else if Self::is_letter(self.ch) {
                    return self.read_identifier();
                } else if Self::is_digit(self.ch) {
                    return self.read_integer();
                } else {
                    // note: there must be a better way
                    Token::Illegal((if let Some(ch) = self.ch { ch } else { '?' }).to_string())
                }
            }
        };

        self.read_char();
        token
    }

    pub fn token_iter(&mut self) -> LexerIterator {
        LexerIterator { lexer: self }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.ch, Some(' ') | Some('\t') | Some('\n') | Some('\r')) {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> Token {
        let pos = self.position;
        while Self::is_letter(self.ch) {
            self.read_char();
        }
        let word = self.input[pos..self.position].to_string();

        match Token::from_word(word.clone()) {
            Some(token) => token,
            None => Token::Ident(word),
        }
    }

    fn read_integer(&mut self) -> Token {
        let pos = self.position;
        while Self::is_digit(self.ch) {
            self.read_char();
        }
        Token::Int(self.input[pos..self.position].to_string())
    }

    fn read_string(&mut self) -> Token {
        let start_pos = self.position + 1;
        self.read_char();

        // note: this is not perfect, but it's good enough for now
        while self.ch.is_some() && self.ch != Some('"') {
            self.read_char();
        }

        Token::String(self.input[start_pos..self.position].to_string())
    }

    fn is_string_starter(ch: Option<char>) -> bool {
        matches!(ch, Some('"'))
    }

    fn is_letter(ch: Option<char>) -> bool {
        match ch {
            None => false,
            Some(ch) => ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || ch == '_',
        }
    }

    fn is_digit(ch: Option<char>) -> bool {
        match ch {
            None => false,
            Some(ch) => ch.is_ascii_digit(),
        }
    }
}

pub struct LexerIterator<'a> {
    lexer: &'a mut Lexer,
}

impl<'a> Iterator for LexerIterator<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next_token() {
            Token::Eof => None,
            token => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_whitespace() {
        let input = "   hello";
        let mut lexer = Lexer::new(input.to_string());
        let tok = lexer.next_token();
        assert_eq!(
            tok,
            Token::Ident("hello".to_string()),
            "token wrong. expected={:?}, got={:?}",
            Token::Ident("hello".to_string()),
            tok,
        );
    }

    #[test]
    fn test_lexer() {
        let input = r#"
        let five = 555;
        let ten = 10;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);

        !-/*5;

        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;

        10 != 9;

        "foobar";
        "foo bar";

        [1, 2];

        {"foo": "bar"}
        "#;

        let tests = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("555".to_string()),
            Token::Semicolon,
            //
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            //
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            //
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            //
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            //
            Token::Int("5".to_string()),
            Token::LT,
            Token::Int("10".to_string()),
            Token::GT,
            Token::Int("5".to_string()),
            Token::Semicolon,
            //
            Token::If,
            Token::Lparen,
            Token::Int("5".to_string()),
            Token::LT,
            Token::Int("10".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            //
            Token::Int("10".to_string()),
            Token::EQ,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::Semicolon,
            //
            Token::String("foobar".to_string()),
            Token::Semicolon,
            Token::String("foo bar".to_string()),
            Token::Semicolon,
            //
            Token::Lbracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::Rbracket,
            Token::Semicolon,
            //
            Token::Lbrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::Rbrace,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for (i, expected_token) in tests.iter().enumerate() {
            let token = lexer.next_token();

            assert_eq!(
                token, *expected_token,
                "tests[{}] - token wrong. expected={:?}, got={:?}",
                i, expected_token, token
            )
        }
    }
}
