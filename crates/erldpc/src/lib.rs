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
        ("encode_tc128", 1, encode_tc128),
        ("decode_tc128", 2, decode_tc128),
        ("encode_tc256", 1, encode_tc256),
        ("decode_tc256", 2, decode_tc256),
        ("encode_tc512", 1, encode_tc512),
        ("decode_tc512", 2, decode_tc512),
        ("encode_tm1280", 1, encode_tm1280),
        ("decode_tm1280", 2, decode_tm1280),
        ("encode_tm1536", 1, encode_tm1536),
        ("decode_tm1536", 2, decode_tm1536),
        ("encode_tm2048", 1, encode_tm2048),
        ("decode_tm2048", 2, decode_tm2048),
        ("encode_tm5120", 1, encode_tm5120),
        ("decode_tm5120", 2, decode_tm5120),
        ("encode_tm6144", 1, encode_tm6144),
        ("decode_tm6144", 2, decode_tm6144),
        ("encode_tm8192", 1, encode_tm8192),
        ("decode_tm8192", 2, decode_tm8192)
    ],
    None
);

// TC128 scheme encode/decode
fn encode_tc128<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TC128;
    let txdata: Vec<u8> = args[0].decode()?;
    let mut txcode = vec![0u8; code.n()/8];
    code.copy_encode(&txdata, &mut txcode);
    Ok((atoms::ok(), txcode).encode(env))
}

fn decode_tc128<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TC128;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}

// TC256 scheme encode/decode
fn encode_tc256<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TC256;
    let txdata: Vec<u8> = args[0].decode()?;
    let mut txcode = vec![0u8; code.n()/8];
    code.copy_encode(&txdata, &mut txcode);
    Ok((atoms::ok(), txcode).encode(env))
}

fn decode_tc256<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TC256;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}

// TC512 scheme encode/decode
fn encode_tc512<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TC512;
    let data: Vec<u8> = args[0].decode()?;
    // Allocate memory for the encoded data
    let mut codeword = vec![0u8; code.n()/8];
    code.copy_encode(&data, &mut codeword);
    Ok((atoms::ok(), codeword).encode(env))
}

fn decode_tc512<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TC512;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}

// TM1280 scheme encode/decode
fn encode_tm1280<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM1280;
    let data: Vec<u8> = args[0].decode()?;
    let mut codeword = vec![0u8; code.n()/8];
    code.copy_encode(&data, &mut codeword);
    Ok((atoms::ok(), codeword).encode(env))
}

fn decode_tm1280<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM1280;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}

// TM1536 scheme encode/decode
fn encode_tm1536<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM1536;
    let data: Vec<u8> = args[0].decode()?;
    let mut codeword = vec![0u8; code.n()/8];
    code.copy_encode(&data, &mut codeword);
    Ok((atoms::ok(), codeword).encode(env))
}

fn decode_tm1536<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM1536;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}

// TM2048 scheme encode/decode
fn encode_tm2048<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM2048;
    let data: Vec<u8> = args[0].decode()?;
    let mut codeword = vec![0u8; code.n()/8];
    code.copy_encode(&data, &mut codeword);
    Ok((atoms::ok(), codeword).encode(env))
}

fn decode_tm2048<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM2048;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}


// TM5120 scheme encode/decode
fn encode_tm5120<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM5120;
    let data: Vec<u8> = args[0].decode()?;
    let mut codeword = vec![0u8; code.n()/8];
    code.copy_encode(&data, &mut codeword);
    Ok((atoms::ok(), codeword).encode(env))
}

fn decode_tm5120<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM5120;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}


// TM6144 scheme encode/decode
fn encode_tm6144<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM6144;
    let data: Vec<u8> = args[0].decode()?;
    let mut codeword = vec![0u8; code.n()/8];
    code.copy_encode(&data, &mut codeword);
    Ok((atoms::ok(), codeword).encode(env))
}

fn decode_tm6144<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM6144;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
}

// TM8192 scheme encode/decode
fn encode_tm8192<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM8192;
    let data: Vec<u8> = args[0].decode()?;
    let mut codeword = vec![0u8; code.n()/8];
    code.copy_encode(&data, &mut codeword);
    Ok((atoms::ok(), codeword).encode(env))
}

fn decode_tm8192<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let code = LDPCCode::TM8192;
    let rxcode: Vec<u8> = args[0].decode()?;
    let iterations: u8 = args[1].decode()?;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((atoms::ok(), &rxdata).encode(env))
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
        let mut codeword = vec![0u8; code.n()/8];

        // Encode, copying `txdata` into the start of `codeword` then computing the parity bits
        code.copy_encode(&txdata, &mut codeword);

        // Copy the transmitted data and corrupt a few bits
        let mut rxcode = codeword.clone();
        rxcode[0] ^= 0x55;

        // Allocate some memory for the decoder's working area and output
        let mut working = vec![0u8; code.decode_bf_working_len()];
        let mut rxdata = vec![0u8; code.output_len()];

        // Decode for at most 20 iterations
        code.decode_bf(&rxcode, &mut rxdata, &mut working, 20);

        println!("{:?}", txdata);
        println!("{:?}", codeword);

        // Check the errors got corrected
        assert_eq!(&rxdata[..8], &txdata[..8]);
    }
}
