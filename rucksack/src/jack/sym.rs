use crate::parse::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sym {
    LCurly,
    RCurly,
    LRound,
    RRound,
    LSquare,
    RSquare,
    LAngle,
    RAngle,
    Dot,
    Comma,
    Semi,
    Plus,
    Minus,
    Splat,
    Slash,
    Amp,
    Pipe,
    Equals,
    Tilde,
}

impl Sym {
    pub fn as_str(&self) -> &'static str {
        use Sym::*;
        match self {
            LCurly => "{",
            RCurly => "}",
            LRound => "(",
            RRound => ")",
            LSquare => "[",
            RSquare => "]",
            LAngle => "<",
            RAngle => ">",
            Dot => ".",
            Comma => ",",
            Semi => ";",
            Plus => "+",
            Minus => "-",
            Splat => "*",
            Slash => "/",
            Amp => "&",
            Pipe => "|",
            Equals => "=",
            Tilde => "~",
        }
    }
}

impl<'a> Parser<'a, Sym> for Sym {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Sym> {
        map(match_literal(self.as_str()), |()| *self).parse(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::*;

    #[test]
    fn parse_within_brackets() {
        // let ctx = Ctx{};
        // let parser = right(left(match_literal("needle")))
    }
}