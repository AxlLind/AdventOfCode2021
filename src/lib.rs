use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemFn, NestedMeta, Lit, Ident};

#[proc_macro_attribute]
pub fn main(args: TokenStream, input: TokenStream) -> TokenStream {
  let input_path = match &parse_macro_input!(args as AttributeArgs)[..] {
    [NestedMeta::Lit(Lit::Int(day))] => format!("../../inputs/{}.in", day.token().to_string()),
    _ => panic!("Expected one integer argument"),
  };

  let mut aoc_solution = parse_macro_input!(input as ItemFn);
  aoc_solution.sig.ident = Ident::new("aoc_solution", aoc_solution.sig.ident.span());

  let tokens = quote! {
    const INPUT: &str = include_str!(#input_path);
    #aoc_solution
    fn main() {
      let now = ::std::time::Instant::now();
      let (p1, p2) = aoc_solution(INPUT.trim_end());
      let time = now.elapsed().as_millis();
      println!("Part one: {}", p1);
      println!("Part two: {}", p2);
      println!("Time: {}ms", time);
    }
  };
  TokenStream::from(tokens)
}
