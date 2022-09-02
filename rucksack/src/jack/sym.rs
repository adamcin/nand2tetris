use crate::parse::*;

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

impl Matchable for Sym {
    fn matcher<'a>(&self, _ctx: &'a Ctx) -> Box<dyn Parser<'a, ()> + 'a> {
        Box::new(match_literal(self.as_str()))
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