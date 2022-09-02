use std::ops::RangeBounds;

pub type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;
pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

pub trait Parses<Output> {
    fn parse<'a>(input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

pub fn map<'a, P, F, A, B>(p: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        p.parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

pub fn and_then<'a, P, F, A, B, E>(p: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> Result<B, E>,
{
    move |input| {
        p.parse(input)
            .and_then(|(next_input, result)| match map_fn(result) {
                Ok(result) => Ok((next_input, result)),
                Err(_) => Err(input),
            })
    }
}

pub fn pair<'a, P1, P2, R1, R2>(left: P1, right: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        left.parse(input).and_then(|(next_input, result1)| {
            right
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

pub fn ok<'a, P, R>(p: P) -> impl Parser<'a, Option<R>>
where
    P: Parser<'a, R>,
{
    move |input| match p.parse(input).ok() {
        Some((remaining, output)) => Ok((remaining, Some(output))),
        None => Ok((input, None)),
    }
}

pub fn or_else<'a, P1, P2, R>(p: P1, elze: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input| p.parse(input).or_else(|next_input| elze.parse(next_input))
}

pub fn none<'a, R>() -> impl Parser<'a, R> {
    move |input| Err(input)
}

pub fn left<'a, P1, P2, R1, R2>(left: P1, r: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(left, r), |(left, _right)| left)
}

pub fn right<'a, P1, P2, R1, R2>(l: P1, right: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(l, right), |(_left, right)| right)
}

pub fn pred<'a, P, A, F>(p: P, pred: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = p.parse(input) {
            if pred(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

pub fn range<'a, P, A, B>(p: P, bounds: B) -> impl Parser<'a, Vec<A>>
where
    B: RangeBounds<usize>,
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = p.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        if bounds.contains(&result.len()) {
            Ok((input, result))
        } else {
            Err(input)
        }
    }
}

pub fn not<'a, P, A>(p: P) -> impl Parser<'a, ()>
where
    P: Parser<'a, A>,
{
    move |input: &'a str| match p.parse(input) {
        Ok(_) => Err(input),
        Err(_) => Ok((input, ())),
    }
}

pub fn until<'a, 'u, P, A, B>(p: P, bounds: B, until: &'u str) -> impl Parser<'a, Vec<A>>
where
    B: RangeBounds<usize>,
    P: Parser<'a, A>,
    'u: 'a,
{
    let parser = right(not(match_literal(until)), p);
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        if bounds.contains(&result.len()) {
            Ok((input, result))
        } else {
            Err(input)
        }
    }
}

pub fn peek<'a, A, F, P>(count: usize, pred: F, opt_p: P) -> impl Parser<'a, Option<A>>
where
    F: Parser<'a, bool>,
    P: Parser<'a, A>,
{
    move |input: &'a str| match pred.parse(&input[0..count.min(input.len())]).ok() {
        Some((_, true)) => opt_p
            .parse(input)
            .map(|(remaining, output)| (remaining, Some(output))),
        _ => Ok((input, None)),
    }
}

pub fn zero_or_more<'a, P, A>(p: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    range(p, 0..)
}

pub fn one_or_more<'a, P, A>(p: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    range(p, 1..)
}

pub fn match_literal<'a, 'b>(expected: &'b str) -> impl Parser<'a, ()>
where
    'b: 'a,
{
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

pub fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

pub fn eof(input: &str) -> ParseResult<()> {
    if input.is_empty() {
        Ok((input, ()))
    } else {
        Err(input)
    }
}

pub fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

pub fn inlinespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace() && c != &'\n' && c != &'\r')
}

pub fn non_nl_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c != &'\n' && c != &'\r')
}

pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

pub fn pad1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(inlinespace_char())
}

pub fn pad0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(inlinespace_char())
}

pub fn digit_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_digit(10))
}

pub fn i16_literal<'a>() -> impl Parser<'a, i16> {
    and_then(
        pair(ok(pred(any_char, |c| c == &'-')), range(digit_char(), 0..)),
        |(neg, digits)| {
            let value: String = vec![neg.map(|c| vec![c]).unwrap_or(Vec::new()), digits]
                .concat()
                .into_iter()
                .collect();
            i16::from_str_radix(value.as_str(), 10)
        },
    )
}

pub fn newline<'a>() -> impl Parser<'a, ()> {
    or_else(eof, or_else(match_literal("\r\n"), match_literal("\n")))
}

pub fn non_nl0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(non_nl_char())
}

pub fn inline_comment<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("//"),
        left(
            map(until(any_char, 0.., "\n"), |chars| {
                chars.into_iter().collect()
            }),
            newline(),
        ),
    )
}

pub fn block_comment<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("/*"),
        left(
            map(until(any_char, 0.., "*/"), |chars| {
                chars.into_iter().collect()
            }),
            match_literal("*/"),
        ),
    )
}

pub fn drop<'a, A, P>(p: P) -> impl Parser<'a, ()>
where
    P: Parser<'a, A>,
{
    map(p, |_| ())
}

pub fn comspace<'a>() -> impl Parser<'a, ()> {
    drop(range(
        or_else(
            drop(space1()),
            or_else(drop(block_comment()), drop(inline_comment())),
        ),
        1..,
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
        assert_eq!(
            Ok((" Hello Robert!", ())),
            parse_joe.parse("Hello Joe! Hello Robert!")
        );
        assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
    }

    #[test]
    fn digit_parser() {
        let parse_digit = digit_char();
        assert_eq!(Ok(("abc", '1')), parse_digit.parse("1abc"));
        assert_eq!(Err("abcd"), parse_digit.parse("abcd"));
    }

    #[test]
    fn whitespace_parser() {
        let parse_wsp = whitespace_char();
        assert_eq!(Ok(("abc", ' ')), parse_wsp.parse(" abc"));
        assert_eq!(Err("abcd"), parse_wsp.parse("abcd"));
    }

    #[test]
    fn newline_parser() {
        let parse_nl = newline();
        assert_eq!(Ok(("Hey", ())), parse_nl.parse("\nHey"));
        assert_eq!(Ok(("abc", ())), parse_nl.parse("\r\nabc"));
        assert_eq!(Err("abcd"), parse_nl.parse("abcd"));
    }

    #[test]
    fn peek_parser() {
        let parse_peek = peek(
            4,
            |p: &'static str| Ok((p, p.contains("="))),
            map(
                left(
                    range(
                        pred(any_char, |c| c == &'A' || c == &'D' || c == &'M'),
                        1..=3,
                    ),
                    match_literal("="),
                ),
                |chars| -> String { chars.into_iter().collect() },
            ),
        );
        assert_eq!(Ok(("f", Some("AMD".to_owned()))), parse_peek.parse("AMD=f"));
        assert_eq!(Ok(("f", Some("AD".to_owned()))), parse_peek.parse("AD=f"));
        assert_eq!(Ok(("ABCD", None)), parse_peek.parse("ABCD"));
        assert_eq!(Ok(("ABC", None)), parse_peek.parse("ABC"));
        assert_eq!(Err("BC="), parse_peek.parse("ABC="));
    }

    #[test]
    fn simple_test() {}
}
