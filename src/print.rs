use std::fmt::{Error as FmtError, Result as FmtResult, Write};
use std::iter::repeat;
use std::mem::replace;

pub struct TokenBuffer {
    tokens: Vec<(usize, Vec<String>)>,
    line: Vec<String>,
    indent: usize,
}

pub struct TokenBlock {
    tokens: Vec<(usize, Vec<String>)>,
}

impl TokenBuffer {
    pub fn new(indent: usize) -> Self {
        TokenBuffer {
            tokens: vec![],
            line: vec![],
            indent,
        }
    }
    pub fn commit_line(&mut self, with_semicolon: bool) -> FmtResult {
        if with_semicolon {
            self.write_char(';')?;
        }
        let new_line = replace(&mut self.line, vec![]);
        self.tokens.push((self.indent, new_line));
        Ok(())
    }

    pub fn sub_buffer(&mut self) -> Self {
        Self::new(self.indent + 1)
    }

    pub fn commit_block(&mut self, block: TokenBlock) -> FmtResult {
        self.write_char('{')?;
        self.commit_line(false)?;
        self.tokens.extend(block.tokens.into_iter());
        self.write_char('}')
    }

    pub fn commit_inline_block(&mut self, block: TokenBlock) -> FmtResult {
        self.line
            .extend(block.tokens.into_iter().map(|(_, t)| t).flatten());
        Ok(())
    }

    pub fn freeze(mut self) -> Result<TokenBlock, FmtError> {
        if !self.line.is_empty() {
            self.commit_line(false)?;
        }

        Ok(TokenBlock {
            tokens: self.tokens,
        })
    }

    pub fn format(&self, spaces: usize) -> String {
        self.tokens
            .iter()
            .map(|(indent, tokens)| {
                format!(
                    "{}{}",
                    repeat(' ').take(spaces * indent).collect::<String>(),
                    tokens.join(" ")
                )
            })
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn format_inline(&self) -> String {
        self.tokens
            .iter()
            .map(|(_, tokens)| tokens.join(" "))
            .collect::<Vec<String>>()
            .join("")
    }
}

impl Write for TokenBuffer {
    fn write_str(&mut self, s: &str) -> FmtResult {
        self.line.push(s.to_string());

        Ok(())
    }
}

impl Default for TokenBuffer {
    fn default() -> Self {
        TokenBuffer::new(0)
    }
}
