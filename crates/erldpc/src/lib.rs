extern crate labrador_ldpc;

extern crate rustler;

use rustler::{Encoder, Env, Error, Term};
use labrador_ldpc::LDPCCode;

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
    }
}

rustler::rustler_export_nifs!(
    "erldpc",
    [
        ("encode", 1, encode)
    ],
    None
);

fn encode<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    // Pick the TC128 code, n=128 k=64
    // (that's 8 bytes of user data encoded into 16 bytes)
    let code = LDPCCode::TC128;
    let txdata: Vec<u8> = args[0].decode()?;
    // Allocate memory for the encoded data
    let mut txcode = vec![0u8; code.n()/8];
    code.copy_encode(&txdata, &mut txcode);
    Ok((atoms::ok()).encode(env))
}
