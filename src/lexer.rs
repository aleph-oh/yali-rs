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

    #[test]
    fn lex_atom() {
        assert_eq!(lex("1"), vec!["1"])
    }

    #[test]
    fn lex_s_expression() {
        assert_eq!(lex("(+ 1 1)"), vec!["(", "+", "1", "1", ")"])
    }
}
