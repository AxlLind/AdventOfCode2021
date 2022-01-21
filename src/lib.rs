use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemFn, NestedMeta, Lit, Ident};

#[proc_macro_attribute]
pub fn main(args: TokenStream, input: TokenStream) -> TokenStream {
  let day = match &parse_macro_input!(args as AttributeArgs)[..] {
    [NestedMeta::Lit(Lit::Str(s))] => s.value(),
    _ => panic!("Expected one string argument")
  };

  let mut aoc_solution = parse_macro_input!(input as ItemFn);
  aoc_solution.sig.ident = Ident::new("aoc_solution", aoc_solution.sig.ident.span());

  let input_path = format!("../../inputs/{}.in", day);
  let tokens = quote! {
    fn main() {
      #aoc_solution
      let input = include_str!(#input_path);
      let now = ::std::time::Instant::now();
      let (p1,p2) = aoc_solution(input.trim_end());
      let time = now.elapsed().as_millis();
      println!("Part one: {}", p1);
      println!("Part two: {}", p2);
      println!("Time: {}ms", time);
    }
  };
  TokenStream::from(tokens)
}
