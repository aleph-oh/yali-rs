//! This module contains a lexer / tokenizer which supports single-line comments.

/// Lexes, or tokenizes, a provided string into individual separated tokens.
pub(crate) fn lex<S: AsRef<str>>(src: S) -> Vec<String> {
    let src = src.as_ref();
    src.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(&str::to_string)
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::lexer::lex;
    use serde::{Deserialize, Serialize};
    use serde_json;
    use std::fs::File;
    use std::path::Path;

    #[derive(Serialize, Deserialize)]
    struct TestData {
        input: String,
        expected: Vec<String>
    }

    fn check<P: AsRef<Path>>(file: P) {
        let test_file = File::open(file).expect("Failed to open test data file");
        let test_data: Vec<TestData> = serde_json::from_reader(test_file).expect("Could not parse test data file");
        for example in test_data {
            assert_eq!(lex(example.input), example.expected);
        }
    }

    #[test]
    fn atom() {
        assert_eq!(lex("1"), vec!["1"])
    }

    #[test]
    fn s_expression() {
        assert_eq!(vec!["(", "+", "1", "1", ")"], lex("(+ 1 1)"))
    }

    #[test]
    fn multiline() {
        check("./lex_examples/multiline.json")
    }

    #[test]
    fn comments() {
        check("./lex_examples/comments.json")
    }
}
