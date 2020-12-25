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

#[proc_macro_derive(ITypeArith64)]
/// Automatically derives IType for an instance of ITypeArith that is implemented only for 64-bit
/// instructions.
pub fn itype_arith_64_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_arith_64_derive(&ast)
}

#[proc_macro_derive(ITypeLoad)]
/// Automatically derives IType for an instance of ITypeLoad.
pub fn itype_load_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_load_derive(&ast)
}

#[proc_macro_derive(ITypeLoad64)]
/// Automatically derives IType for an instance of ITypeLoad that is implemented only for 64-bit
/// instructions.
pub fn itype_load_64_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_load_64_derive(&ast)
}

fn impl_convert_int64_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        use std::num::Wrapping;

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
        impl<S: AtLeast32b> IType<S> for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeArith<S>>::inst_fields()
            }

            fn eval(
                state: &ProgramState<RiscV<S>, S>,
                rd: RiscVRegister,
                rs1: RiscVRegister,
                imm: BitStr32
            ) -> InstResult<RiscV<S>, S> {
                let new_rd_val = <#name as ITypeArith<S>>::eval(state.user_state.regfile.read(rs1), imm);
                Ok(UserDiff::reg_write_pc_p4(&state.user_state, rd, new_rd_val))
            }
        }
    };
    gen.into()
}

fn impl_itype_arith_64_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl IType<RS64b> for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeArith<RS64b>>::inst_fields()
            }

            fn eval(
                state: &ProgramState<RiscV<RS64b>, RS64b>,
                rd: RiscVRegister,
                rs1: RiscVRegister,
                imm: BitStr32
            ) -> InstResult<RiscV<RS64b>, RS64b> {
                let new_rd_val = <#name as ITypeArith<RS64b>>::eval(state.user_state.regfile.read(rs1), imm);
                Ok(UserDiff::reg_write_pc_p4(&state.user_state, rd, new_rd_val))
            }
        }
    };
    gen.into()
}

fn impl_itype_load_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl<S: AtLeast32b> IType<S> for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeLoad<S>>::inst_fields()
            }

            fn eval(
                state: &ProgramState<RiscV<S>, S>,
                rd: RiscVRegister,
                rs1: RiscVRegister,
                imm: BitStr32
            ) -> InstResult<RiscV<S>, S> {
                let rs1_val: SignedValue<S> = state.user_state.regfile.read(rs1).into();
                let addr: RegValue<S> = (rs1_val + imm.into()).into();
                let result = <#name as ITypeLoad<S>>::eval(state, addr.into());
                match result {
                    Ok((new_rd_val, mut diffs)) => {
                        diffs.extend(
                            UserDiff::reg_write_pc_p4(&state.user_state, rd, new_rd_val)
                        );
                        Ok(diffs)
                    },
                    Err(fault) => state.handle_trap(&fault.into()),
                }
            }
        }
    };
    gen.into()
}

fn impl_itype_load_64_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl IType<RS64b> for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeLoad<RS64b>>::inst_fields()
            }

            fn eval(
                state: &ProgramState<RiscV<RS64b>, RS64b>,
                rd: RiscVRegister,
                rs1: RiscVRegister,
                imm: BitStr32
            ) -> InstResult<RiscV<RS64b>, RS64b> {
                let rs1_val: i64 = state.user_state.regfile.read(rs1).into();
                let imm_val: i64 = SignedValue::<RS64b>::from(imm).into();
                let addr: DataDword = (rs1_val + imm_val).into();
                let result = <#name as ITypeLoad<RS64b>>::eval(state, addr.into());
                match result {
                    Ok((new_rd_val, mut diffs)) => {
                        diffs.extend(
                            UserDiff::reg_write_pc_p4(&state.user_state, rd, new_rd_val)
                        );
                        Ok(diffs)
                    },
                    Err(fault) => state.handle_trap(&fault.into()),
                }
            }
        }
    };
    gen.into()
}
