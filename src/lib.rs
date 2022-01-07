use proc_macro::*;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemFn, NestedMeta, Lit, Ident};

#[proc_macro_attribute]
pub fn main(args: TokenStream, input: TokenStream) -> TokenStream {
  let day = match &parse_macro_input!(args as AttributeArgs)[..] {
    [NestedMeta::Lit(Lit::Str(s))] => format!("../../inputs/{}.in", s.value()),
    _ => panic!("Expected one string argument")
  };

  let mut aocsolver = parse_macro_input!(input as ItemFn);
  aocsolver.sig.ident = Ident::new("aocsolver", aocsolver.sig.ident.span());

  let tokens = quote! {
    fn main() {
      #aocsolver
      let input = include_str!(#day);
      let now = ::std::time::Instant::now();
      let (p1,p2) = aocsolver(input.trim_end());
      let time = now.elapsed().as_millis();
      println!("Part one: {}", p1);
      println!("Part two: {}", p2);
      println!("Time: {}ms", time);
    }
  };
  TokenStream::from(tokens)
}
