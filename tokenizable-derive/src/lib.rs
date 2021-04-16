extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

fn impl_tokenizable(ast: &syn::DeriveInput) -> quote::Tokens {
  let name = &ast.ident;
  let data = &ast.data;

  match data {
    Enum(data) => {
      let variants = data.variants;
      let error_typename = format_ident!("{}Error", name);
      let error_type = quote!{
        enum #error_typename {

        }

      };


      quote! {
        impl Tokenizable for #name {
          type TokenizationError = #error_typename;

          fn tokenize(stream: &mut ReversableStream<char>) -> Result<Self, Self::TokenizationError> {
            match #variant::tokenize(stream) {
              Ok(res) => { return Ok(res); },
              Err(err) => #variant::TokenizationError,
            }
          }
        }
      }
    },
    _ => panic!("can only derive on enums")
  }
}

#[proc_macro_derive(Tokenizable)]
pub fn tokenizable_derive(input: TokenStream) -> TokenStream {
  // Construct a string representation of the type definition
  let s = input.to_string();

  // Parse the string representation
  let ast = syn::parse_derive_input(&s).unwrap();

  // Build the impl
  let gen = impl_tokenizable(&ast);

  // Return the generated impl
  gen.parse().unwrap()
}
