//! This module contains a parser which, given a stream of tokens, validates the tokens
//! and transforms them into a syntax tree.

use serde::{Deserialize, Serialize};
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

macro_rules! safely_convertible {
    ($val: expr, $src_ty: ty, $dest_ty: ty) => {
        $val as $src_ty == ($val as $dest_ty) as $src_ty
    };
}

impl FromStr for Atom {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "(" || s == ")" {
            return Err(());
        }
        if let Ok(as_float) = s.parse() {
            if safely_convertible!(as_float, f64, i64) {
                Ok(Atom::Integer(as_float as i64))
            } else {
                Ok(Atom::Number(as_float))
            }
        } else {
            Ok(Atom::Symbol(s.into()))
        }
    }
}

impl Expression {
    pub(crate) fn parse_tokens(tokens: &[String]) -> Result<Expression, ParseError> {
        Ok(Expression::parse_tokens_recursive(tokens)?.0)
    }

    fn parse_tokens_recursive(tokens: &[String]) -> Result<(Expression, &[String]), ParseError> {
        let (first, rest) = tokens.split_first().ok_or(ParseError::EmptyExpr)?;
        match first.as_str() {
            "(" => Ok((Expression::parse_tokens_recursive(rest)?.0, rest)),
            ")" => Err(ParseError::UnbalancedParen),
            _ => Ok((
                Expression::Atom(first.parse().expect("Shouldn't occur; covered error cases")),
                rest,
            )),
        }
    }

    fn read_tokens(tokens: &[String]) -> Result<(Expression, &[String]), ParseError> {
        let mut result = Vec::new();
        let rest = tokens;
        loop {
            let (next, mut rest) = rest.split_first().ok_or(ParseError::UnbalancedParen)?;
            println!("{}", next);
            if next == ")" {
                return Ok((Expression::SExpr(result), rest));
            }
            let (expr, new_rest) = Expression::read_tokens(rest)?;
            result.push(expr);
            rest = new_rest;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Expression::SExpr;
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
                Expression::parse_tokens(&*example.input)
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
                Expression::parse_tokens(&*example.input).expect_err(&*format!(
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
}
