//! This module contains a parser which, given a stream of tokens, validates the tokens
//! and transforms them into a syntax tree.
use crate::tokenizer::{Token, Tokens};
#[cfg(test)]
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

enum ValidationError {
    UnbalancedParens,
    EmptyExpr,
    NeedsParens,
}

struct ValidatedTokens(Vec<Token>);

impl TryFrom<Tokens> for ValidatedTokens {
    type Error = ValidationError;

    fn try_from(tokens: Tokens) -> Result<Self, Self::Error> {
        let inner = tokens.0;
        let total_parens = inner
            .iter()
            .filter(|&tok| tok == &Token::LeftParen || tok == &Token::RightParen)
            .count();
        let unclosed_parens = inner.iter().try_fold(0, |unclosed_parens, tok| match tok {
            Token::LeftParen => Ok(unclosed_parens + 1),
            Token::RightParen => {
                if unclosed_parens == 0 {
                    Err(ValidationError::UnbalancedParens)?;
                }
                Ok(unclosed_parens - 1)
            }
            _ => Ok(unclosed_parens),
        })?;
        if total_parens == 0 && inner.len() > 1 {
            return Err(ValidationError::EmptyExpr);
        }
        if unclosed_parens > 0 {
            return Err(ValidationError::UnbalancedParens);
        }
        Ok(ValidatedTokens(inner))
    }
}

#[cfg_attr(test, derive(Deserialize, Serialize, PartialEq, Debug))]
enum Expression {
    Atom(Atom),
    SExpr(Vec<Expression>),
}

#[cfg_attr(test, derive(Deserialize, Serialize, PartialEq, Debug))]
enum Atom {
    Number(f64),
    Integer(u64),
    Symbol(String),
}

impl TryFrom<Token> for Atom {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Integer(n) => Ok(Self::Integer(n)),
            Token::Number(x) => Ok(Self::Number(x)),
            Token::Symbol(s) => Ok(Self::Symbol(s)),
            Token::LeftParen | Token::RightParen => Err(()), // can't parse left / right parens into atoms: they're delimiters
        }
    }
}

#[cfg_attr(test, derive(Deserialize, Serialize, PartialEq, Debug))]
enum ParseError {
    UnbalancedParens,
    EmptyExpr,
    NeedsParens,
}

impl From<ValidationError> for ParseError {
    fn from(error: ValidationError) -> Self {
        match error {
            ValidationError::UnbalancedParens => ParseError::UnbalancedParens,
            ValidationError::EmptyExpr => ParseError::EmptyExpr,
            ValidationError::NeedsParens => ParseError::NeedsParens,
        }
    }
}

impl TryFrom<&ValidatedTokens> for Expression {
    type Error = ParseError;

    fn try_from(tokens: &ValidatedTokens) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl TryFrom<Tokens> for Expression {
    type Error = ParseError;

    fn try_from(tokens: Tokens) -> Result<Self, Self::Error> {
        let tokens = ValidatedTokens::try_from(tokens)?;
        Self::try_from(&tokens)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Expression, ParseError};
    use crate::tokenizer::Tokens;
    use serde::{Deserialize, Serialize};
    use serde_json;
    use std::convert::TryFrom;
    use std::fs::File;
    use std::path::Path;

    #[derive(Serialize, Deserialize)]
    struct HappyParseTestData {
        input: Tokens,
        expected: Expression,
    }

    #[derive(Serialize, Deserialize)]
    struct SadParseTestData {
        input: Tokens,
        expected: ParseError,
    }

    macro_rules! load_test_data {
        ($path: expr, $ty: ty) => {{
            let test_file = File::open($path).expect("Failed to open test data file");
            let test_data: Vec<$ty> =
                serde_json::from_reader(test_file).expect("Could not parse test data file");
            test_data
        }};
    }

    fn check_happy<P>(file: P)
    where
        P: AsRef<Path>,
    {
        let test_data: Vec<HappyParseTestData> = load_test_data!(file, HappyParseTestData);
        for example in test_data {
            assert_eq!(
                Expression::try_from(example.input).expect("Failed to parse"),
                example.expected
            );
        }
    }

    fn check_sad<P>(file: P)
    where
        P: AsRef<Path>,
    {
        let test_data: Vec<SadParseTestData> = load_test_data!(file, SadParseTestData);
        for example in test_data {
            assert_eq!(
                Expression::try_from(example.input).expect_err("Should have failed to parse"),
                example.expected
            );
        }
    }

    #[test]
    fn no_nesting() {
        check_happy("./parse_examples/no_nesting.json")
    }

    #[test]
    fn unmatched_parens() {
        check_sad("./parse_examples/unmatched_parens.json")
    }

    #[test]
    fn empty_expr() {
        check_sad("./parse_examples/empty_expr.json")
    }
}
