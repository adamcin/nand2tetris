//

#[cfg(test)]
pub(crate) mod testutil {
    use std::fmt::Debug;

    use crate::{jack::token::*, parse::*};

    pub trait TokenResultFn<R> = Fn(&[Token]) -> Result<R, Option<Token>>;

    pub fn transform_result<'a, R>(
        result: ParseResult<'a, &'a [Token], R>,
    ) -> Result<R, Option<Token>> {
        result
            .map_err(|value| {
                println!("result: {:?}", value);
                value.first().cloned()
            })
            .map(|(rem, value)| value)
    }

    // pub fn assert_tokens_result<'a, F, R>(
    //     stream: &'a TokenStream,
    //     pf: &F,
    //     expected: Result<R, Option<Token>>,
    // ) where
    //     F: TokenResultFn<R> + Copy, // Fn(&[Token]) -> Result<R, Option<Token>> + Copy,
    //     R: Debug + std::cmp::PartialEq,
    // {
    // }

    pub fn assert_tokens<F, R>(pairs: Vec<(&str, Result<R, Option<Token>>)>, pf: F)
    where
        F: TokenResultFn<R> + Copy, // Box<dyn Parser<'a, &'a [Token], R> + 'a> + Copy + 'a,
        R: Debug + std::cmp::PartialEq,
    {
        pairs.into_iter().for_each(|(source, expected)| {
            let src_str = source.to_owned();
            let (_, stream) = TokenStream::parse_into(src_str.as_str())
                .expect(format!("failed to tokenize {}", source).as_str());
            //assert_tokens_result(&stream, &pf, expected);
            let result = pf(stream.tokens());
            assert_eq!(expected, result);
        });
    }
}
