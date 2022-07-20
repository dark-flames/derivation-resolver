use std::fmt::{Error as FmtError, Result as FmtResult, Write};
use std::iter::repeat;
use std::mem::replace;

pub trait ToToken {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult;
}

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

    pub fn parenthesized_by(
        &mut self,
        callback: impl Fn(&mut TokenBuffer) -> FmtResult,
    ) -> FmtResult {
        self.write_char('(')?;
        callback(self)?;
        self.write_char(')')
    }

    pub fn parenthesized(&mut self, obj: &impl ToToken) -> FmtResult {
        self.parenthesized_by(|b| obj.to_token(b))
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

impl<T: ToToken> ToToken for Box<T> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        self.as_ref().to_token(buffer)
    }
}
