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
    Ok((atoms::ok(), txcode).encode(env))
}

#[cfg(test)]
mod tests {
    #[test]
    fn example_test() {
        use labrador_ldpc::LDPCCode;

        // Pick the TC128 code, n=128 k=64
        // (that's 8 bytes of user data encoded into 16 bytes)
        let code = LDPCCode::TC128;

        // Generate some data to encode
        let txdata: Vec<u8> = (0..8).collect();

        // Allocate memory for the encoded data
        let mut txcode = vec![0u8; code.n()/8];

        // Encode, copying `txdata` into the start of `txcode` then computing the parity bits
        code.copy_encode(&txdata, &mut txcode);

        // Copy the transmitted data and corrupt a few bits
        let mut rxcode = txcode.clone();
        rxcode[0] ^= 0x55;

        // Allocate some memory for the decoder's working area and output
        let mut working = vec![0u8; code.decode_bf_working_len()];
        let mut rxdata = vec![0u8; code.output_len()];

        // Decode for at most 20 iterations
        code.decode_bf(&rxcode, &mut rxdata, &mut working, 20);

        println!("{:?}", txdata);
        println!("{:?}", txcode);

        // Check the errors got corrected
        assert_eq!(&rxdata[..8], &txdata[..8]);
    }
}
