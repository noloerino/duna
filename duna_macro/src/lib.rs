use proc_macro::TokenStream;
use quote::quote;

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

fn impl_itype_arith_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl IType for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeArith>::inst_fields()
            }

            fn eval(state: &UserProgState, rd: IRegister, rs1: IRegister, imm: BitStr32) -> UserStateChange {
                let new_rd_val = <#name as ITypeArith>::eval(state.regfile.read(rs1), imm);
                UserStateChange::reg_write_pc_p4(state, rd, new_rd_val)
            }
        }
    };
    gen.into()
}

fn impl_itype_load_derive(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl IType for #name {
            fn inst_fields() -> IInstFields {
                <#name as ITypeLoad>::inst_fields()
            }

            fn eval(state: &UserProgState, rd: IRegister, rs1: IRegister, imm: BitStr32) -> UserStateChange {
                let offs = imm.to_sgn_data_word();
                let rs1_val = state.regfile.read(rs1);
                let addr = DataWord::from(i32::from(rs1_val).wrapping_add(i32::from(offs)));
                let new_rd_val = <#name as ITypeLoad>::eval(&state.memory, ByteAddress::from(addr));
                UserStateChange::reg_write_pc_p4(state, rd, new_rd_val)
            }
        }
    };
    gen.into()
}
