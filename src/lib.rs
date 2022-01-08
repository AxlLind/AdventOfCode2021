use std::{fs, env, path::Path};
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemFn, NestedMeta, Lit, Ident};
use reqwest::blocking::Client;

fn ensure_inputfile(day: &str) {
  let path = format!("./inputs/{}.in", day);
  if Path::new(&path).is_file() {
    return;
  }
  let input = Client::new()
    .get(format!("https://adventofcode.com/2021/day/{}/input", day.trim_start_matches('0')))
    .header("cookie", format!("session={}", env::var("AOC_SESSION").expect("AOC_SESSION not set")))
    .send()
    .expect("http request failed")
    .text()
    .expect("malformed input response");
  fs::write(path,input).expect("writing file failed");
}

#[proc_macro_attribute]
pub fn main(args: TokenStream, input: TokenStream) -> TokenStream {
  let day = match &parse_macro_input!(args as AttributeArgs)[..] {
    [NestedMeta::Lit(Lit::Str(s))] => s.value(),
    _ => panic!("Expected one string argument")
  };

  let mut aocsolver = parse_macro_input!(input as ItemFn);
  aocsolver.sig.ident = Ident::new("aocsolver", aocsolver.sig.ident.span());

  ensure_inputfile(&day);

  let input_path = format!("../../inputs/{}.in", day);
  let tokens = quote! {
    fn main() {
      #aocsolver
      let input = include_str!(#input_path);
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
