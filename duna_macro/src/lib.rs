use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(ConvertInt64)]
/// Automatically provides conversion methods to and from 64-bit int and wrapping int types.
/// Assumes the target type has a new(u64) method, and has a backing field named value.
pub fn convert_int64_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_convert_int64_derive(&ast)
}

#[proc_macro_derive(ConvertInt32)]
/// Automatically provides conversion methods to and from 32-bit int and wrapping int types.
/// Assumes the target type has a new(u32) method, and has a backing field named value.
pub fn convert_int32_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_convert_int32_derive(&ast)
}
#[proc_macro_derive(ITypeArith)]
/// Automatically derives IType for an instance of ITypeArith.
pub fn itype_arith_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_arith_derive(&ast)
}

#[proc_macro_derive(ITypeLoad)]
/// Automatically derives IType for an instance of ITypeLoad.
pub fn itype_load_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_load_derive(&ast)
}

fn impl_convert_int64_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl From<u64> for #name {
            fn from(value: u64) -> #name {
                #name::new(value)
            }
        }

        impl From<i64> for #name {
            fn from(value: i64) -> #name {
                #name::new(value as u64)
            }
        }

        impl From<Wrapping<u64>> for #name {
            fn from(value: Wrapping<u64>) -> #name {
                #name::new(value.0)
            }
        }

        impl From<Wrapping<i64>> for #name {
            fn from(value: Wrapping<i64>) -> #name {
                #name::new(value.0 as u64)
            }
        }

        impl From<#name> for u64 {
            fn from(value: #name) -> u64 {
                value.value
            }
        }

        impl From<#name> for i64 {
            fn from(value: #name) -> i64 {
                value.value as i64
            }
        }


        impl From<#name> for Wrapping<u64> {
            fn from(value: #name) -> Wrapping<u64> {
                Wrapping(value.value)
            }
        }

        impl From<#name> for Wrapping<i64> {
            fn from(value: #name) -> Wrapping<i64> {
                Wrapping(value.value as i64)
            }
        }
    };
    gen.into()
}

fn impl_convert_int32_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl From<u32> for #name {
            fn from(value: u32) -> #name {
                #name::new(value)
            }
        }

        impl From<i32> for #name {
            fn from(value: i32) -> #name {
                #name::new(value as u32)
            }
        }

        impl From<Wrapping<u32>> for #name {
            fn from(value: Wrapping<u32>) -> #name {
                #name::new(value.0)
            }
        }

        impl From<Wrapping<i32>> for #name {
            fn from(value: Wrapping<i32>) -> #name {
                #name::new(value.0 as u32)
            }
        }

        impl From<#name> for u32 {
            fn from(value: #name) -> u32 {
                value.value
            }
        }

        impl From<#name> for i32 {
            fn from(value: #name) -> i32 {
                value.value as i32
            }
        }


        impl From<#name> for Wrapping<u32> {
            fn from(value: #name) -> Wrapping<u32> {
                Wrapping(value.value)
            }
        }

        impl From<#name> for Wrapping<i32> {
            fn from(value: #name) -> Wrapping<i32> {
                Wrapping(value.value as i32)
            }
        }
    };
    gen.into()
}

fn impl_itype_arith_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl<T: MachineDataWidth> IType<T> for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeArith<T>>::inst_fields()
            }

            fn eval(state: &UserProgState<T>, rd: IRegister, rs1: IRegister, imm: T::RegData) -> UserDiff<T> {
                let new_rd_val = <#name as ITypeArith<T>>::eval(state.regfile.read(rs1), imm);
                UserDiff::reg_write_pc_p4(state, rd, new_rd_val)
            }
        }
    };
    gen.into()
}

fn impl_itype_load_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl<T: MachineDataWidth> IType<T> for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeLoad<T>>::inst_fields()
            }

            fn eval(state: &UserProgState<T>, rd: IRegister, rs1: IRegister, imm: T::RegData) -> UserDiff<T> {
                let rs1_val = state.regfile.read(rs1);
                let addr = T::RegData::from(T::signed_from_data(rs1_val).wrapping_add(T::signed_from_data(imm)));
                let new_rd_val = <#name as ITypeLoad<T>>::eval(&state.memory, T::ByteAddr::from(addr));
                UserDiff::reg_write_pc_p4(state, rd, new_rd_val)
            }
        }
    };
    gen.into()
}
