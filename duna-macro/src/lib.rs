use proc_macro::TokenStream;
use quote::quote;

/// Automatically derives IType for an instance of ITypeArith.
#[proc_macro_derive(ITypeArith)]
pub fn itype_arith_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_arith_derive(&ast)
}

/// Automatically derives IType for an instance of ITypeArith that is implemented only for 64-bit
/// instructions.
#[proc_macro_derive(ITypeArith64)]
pub fn itype_arith_64_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_arith_64_derive(&ast)
}

/// Automatically derives IType for an instance of ITypeLoad.
#[proc_macro_derive(ITypeLoad)]
pub fn itype_load_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_load_derive(&ast)
}

/// Automatically derives IType for an instance of ITypeLoad that is implemented only for 64-bit
/// instructions.
#[proc_macro_derive(ITypeLoad64)]
pub fn itype_load_64_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_itype_load_64_derive(&ast)
}

fn impl_itype_arith_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let name_lower = {
        let s: TokenStream = format!("{}{}{}", "\"", &ast.ident.to_string().to_lowercase(), "\"")
            .parse()
            .unwrap();
        syn::parse_macro_input!(s as syn::LitStr)
    };
    let gen = quote! {
        impl<S: AtLeast32b> IType<S> for #name {
            fn name() -> &'static str {
                #name_lower
            }

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
    let name_lower = {
        let s: TokenStream = format!("{}{}{}", "\"", &ast.ident.to_string().to_lowercase(), "\"")
            .parse()
            .unwrap();
        syn::parse_macro_input!(s as syn::LitStr)
    };
    let gen = quote! {
        impl IType<W64b> for #name {
            fn name() -> &'static str {
                #name_lower
            }

            fn inst_fields() -> IInstFields {
                <#name as ITypeArith<W64b>>::inst_fields()
            }

            fn eval(
                state: &ProgramState<RiscV<W64b>, W64b>,
                rd: RiscVRegister,
                rs1: RiscVRegister,
                imm: BitStr32
            ) -> InstResult<RiscV<W64b>, W64b> {
                let new_rd_val = <#name as ITypeArith<W64b>>::eval(state.user_state.regfile.read(rs1), imm);
                Ok(UserDiff::reg_write_pc_p4(&state.user_state, rd, new_rd_val))
            }
        }
    };
    gen.into()
}

fn impl_itype_load_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let name_lower = {
        let s: TokenStream = format!("{}{}{}", "\"", &ast.ident.to_string().to_lowercase(), "\"")
            .parse()
            .unwrap();
        syn::parse_macro_input!(s as syn::LitStr)
    };
    let gen = quote! {
        impl<S: AtLeast32b> IType<S> for #name {
            fn name() -> &'static str {
                #name_lower
            }

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
    let name_lower = {
        let s: TokenStream = format!("{}{}{}", "\"", &ast.ident.to_string().to_lowercase(), "\"")
            .parse()
            .unwrap();
        syn::parse_macro_input!(s as syn::LitStr)
    };
    let gen = quote! {
        impl IType<W64b> for #name {
            fn name() -> &'static str {
                #name_lower
            }

            fn inst_fields() -> IInstFields {
                <#name as ITypeLoad<W64b>>::inst_fields()
            }

            fn eval(
                state: &ProgramState<RiscV<W64b>, W64b>,
                rd: RiscVRegister,
                rs1: RiscVRegister,
                imm: BitStr32
            ) -> InstResult<RiscV<W64b>, W64b> {
                let rs1_val: i64 = state.user_state.regfile.read(rs1).into();
                let imm_val: i64 = SignedValue::<W64b>::from(imm).into();
                let addr: DataDword = (rs1_val + imm_val).into();
                let result = <#name as ITypeLoad<W64b>>::eval(state, addr.into());
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
