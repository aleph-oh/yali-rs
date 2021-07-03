#[cfg(test)]
use serde::{Deserialize, Serialize};

#[cfg_attr(test, derive(Deserialize, Serialize, Debug, PartialEq))]
pub(crate) enum Token {
    Integer(u64),
    Number(f64),
    Symbol(String),
    LeftParen,
    RightParen,
}

impl From<String> for Token {
    fn from(s: String) -> Self {
        if let Ok(as_float) = s.parse() {
            if let Ok(as_int) = s.parse() {
                Self::Integer(as_int)
            } else {
                Self::Number(as_float)
            }
        } else {
            match s.as_str() {
                "(" => Self::LeftParen,
                ")" => Self::RightParen,
                _ => Self::Symbol(s),
            }
        }
    }
}

#[cfg_attr(test, derive(Deserialize, Serialize, Debug, PartialEq))]
pub(crate) struct Tokens(Vec<Token>);

impl From<Vec<String>> for Tokens {
    fn from(tokens: Vec<String>) -> Self {
        Self(tokens.into_iter().map(Token::from).collect())
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::Tokens;
    use serde::{Deserialize, Serialize};
    use std::fmt::Display;
    use std::fs::File;
    use std::path::Path;

    #[derive(Deserialize, Serialize)]
    struct TokenizerTestData {
        input: Vec<String>,
        expected: Tokens,
    }

    fn check<P>(path: P)
    where
        P: AsRef<Path> + Display,
    {
        let test_file =
            File::open(path.as_ref()).unwrap_or_else(|_| panic!("Failed to open {}", path));
        let test_data: Vec<TokenizerTestData> = serde_json::from_reader(test_file)
            .unwrap_or_else(|_| panic!("Failed to parse {}", path));
        for example in test_data {
            assert_eq!(Tokens::from(example.input), example.expected)
        }
    }

    #[test]
    fn no_mixed_tokens() {
        check("./tokenize_examples/no_mixed_tokens.json")
    }

    #[test]
    fn only_atoms() {
        check("./tokenize_examples/only_atoms.json")
    }

    #[test]
    fn no_nesting() {
        check("./tokenize_examples/no_nesting.json")
    }

    #[test]
    fn nested() {
        check("./tokenize_examples/nested.json")
    }
}
