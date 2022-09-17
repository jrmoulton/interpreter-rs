use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Object)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // println!("{ast:#?}");
    let name = &ast.ident;
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };
    let types = fields.iter().map(|val| val.ty.clone());
    let idents = fields.iter().map(|val| val.ident.as_ref());
    let idents_clone = fields.iter().map(|val| val.ident.as_ref());
    // let idents = quote! {#(#idents,)*};
    let expanded = quote! {
        impl #name {
            pub fn new(#(#idents: #types,)*) -> #name {
                #name {#(#idents_clone,)*}
            }
        }
        impl std::fmt::Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(format_args!("{}", self.value))
            }

        }
        // impl ObjectTrait for #name {
        //     fn inner(&self) -> &dyn std::any::Any {
        //         &self.value
        //     }
        // }
    };
    expanded.into()
}

// impl Integer {
//     pub fn new(value: i64) -> Self {
//         Self { value }
//     }
// }
// impl Display for Integer {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         f.write_fmt(format_args!("{}", self.value))
//     }
// }
// impl ObjectTrait for Integer {
//     fn object_type(&self) -> ObjectType {
//         ObjectType::Integer
//     }
// }
