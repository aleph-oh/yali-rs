//! This module contains a parser which, given a stream of tokens, validates the tokens
//! and transforms them into a syntax tree.

#[cfg(test)]
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::str::FromStr;

#[cfg_attr(test, derive(Deserialize, Serialize, Debug, PartialEq))]
pub(crate) enum Expression {
    Atom(Atom),
    SExpr(Vec<Expression>),
}

/// Atomic values which form the basis of expressions.
#[cfg_attr(test, derive(Deserialize, Serialize, Debug, PartialEq))]
pub(crate) enum Atom {
    Integer(i64),
    Number(f64),
    Symbol(String),
}

#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq, Deserialize, Serialize))]
pub(crate) enum ParseError {
    UnbalancedParen,
    EmptyExpr,
}

struct TokenStream(Vec<String>);

impl TryFrom<Vec<String>> for TokenStream {
    type Error = ParseError;

    fn try_from(tokens: Vec<String>) -> Result<Self, Self::Error> {
        let mut unclosed = 0;
        for token in &tokens {
            match token.as_str() {
                "(" => unclosed += 1,
                ")" => {
                    unclosed -= 1;
                    if unclosed < 0 {
                        return Err(ParseError::UnbalancedParen);
                    }
                }
                _ => {}
            }
        }
        if unclosed == 0 {
            Ok(TokenStream(tokens))
        } else {
            return Err(ParseError::UnbalancedParen);
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnbalancedParen => {
                write!(f, "Unbalanced parentheses")
            }
            ParseError::EmptyExpr => {
                write!(f, "Empty expression")
            }
        }
    }
}

impl Error for ParseError {}

impl FromStr for Atom {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "(" || s == ")" {
            return Err(());
        }
        if let Ok(as_float) = s.parse() {
            if let Ok(as_int) = s.parse() {
                Ok(Atom::Integer(as_int))
            } else {
                Ok(Atom::Number(as_float))
            }
        } else {
            Ok(Atom::Symbol(s.into()))
        }
    }
}

impl Expression {
    pub(crate) fn parse_tokens(tokens: Vec<String>) -> Result<Expression, ParseError> {
        let stream = TokenStream::try_from(tokens)?;
        Ok(Expression::parse_tokens_recursive(&*stream.0)?.0)
    }

    fn parse_tokens_recursive(tokens: &[String]) -> Result<(Expression, &[String]), ParseError> {
        let (first, rest) = tokens.split_first().ok_or(ParseError::EmptyExpr)?;
        match first.as_str() {
            "(" => Ok((Expression::read_tokens(rest)?.0, rest)),
            ")" => Err(ParseError::UnbalancedParen),
            _ => Ok((
                Expression::Atom(first.parse().expect("Shouldn't occur; covered error cases")),
                rest,
            )),
        }
    }

    fn read_tokens(tokens: &[String]) -> Result<(Expression, &[String]), ParseError> {
        let mut result = Vec::new();
        let mut rest = tokens;
        loop {
            let (next, _) = rest.split_first().ok_or(ParseError::UnbalancedParen)?;
            if next == ")" {
                return Ok((Expression::SExpr(result), rest));
            }
            let (expr, new_rest) = Expression::parse_tokens_recursive(rest)?;
            result.push(expr);
            rest = new_rest;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Expression, ParseError};
    use serde::{Deserialize, Serialize};
    use serde_json;
    use std::fs::File;
    use std::path::Path;

    #[derive(Serialize, Deserialize)]
    struct HappyParseTestData {
        input: Vec<String>,
        expected: Expression,
    }

    #[derive(Serialize, Deserialize)]
    struct SadParseTestData {
        input: Vec<String>,
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
                Expression::parse_tokens(example.input.clone())
                    .expect(&*format!("Failed to parse {:?}", &*example.input)),
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
                Expression::parse_tokens(example.input.clone()).expect_err(&*format!(
                    "Should have failed to parse {:?}",
                    &*example.input
                )),
                example.expected
            );
        }
    }

    #[test]
    fn no_nesting() {
        check_happy("./parse_examples/few_atoms.json")
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
