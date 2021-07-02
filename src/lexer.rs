//! This module contains a lexer / tokenizer which supports single-line comments.

fn lex(src: &str) -> Vec<&str> {
    unimplemented!()
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

