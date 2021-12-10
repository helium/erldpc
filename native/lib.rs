extern crate labrador_ldpc;

extern crate rustler;

use labrador_ldpc::LDPCCode;
use rustler::{Encoder, Env, NifResult, Term};

rustler::atoms! {
    ok,
}

// TC128 scheme encode/decode
#[rustler::nif(name = "encode_tc128")]
fn encode_tc128<'a>(env: Env<'a>, txdata: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC128;
    let mut txcode = vec![0u8; code.n() / 8];
    code.copy_encode(&txdata, &mut txcode);
    Ok((ok(), txcode).encode(env))
}

#[rustler::nif(name = "decode_tc128")]
fn decode_tc128<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC128;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TC256 scheme encode/decode
#[rustler::nif(name = "encode_tc256")]
fn encode_tc256<'a>(env: Env<'a>, txdata: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC256;
    let mut txcode = vec![0u8; code.n() / 8];
    code.copy_encode(&txdata, &mut txcode);
    Ok((ok(), txcode).encode(env))
}

#[rustler::nif(name = "decode_tc256")]
fn decode_tc256<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC256;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TC512 scheme encode/decode
#[rustler::nif(name = "encode_tc512")]
fn encode_tc512<'a>(env: Env<'a>, data: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC512;
    // Allocate memory for the encoded data
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data, &mut codeword);
    Ok((ok(), codeword).encode(env))
}

#[rustler::nif(name = "decode_tc512")]
fn decode_tc512<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC512;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TM1280 scheme encode/decode
#[rustler::nif(name = "encode_tm1280")]
fn encode_tm1280<'a>(env: Env<'a>, data: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1280;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data, &mut codeword);
    Ok((ok(), codeword).encode(env))
}

#[rustler::nif(name = "decode_tm1280")]
fn decode_tm1280<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1280;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TM1536 scheme encode/decode
#[rustler::nif(name = "encode_tm1536")]
fn encode_tm1536<'a>(env: Env<'a>, data: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1536;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data, &mut codeword);
    Ok((ok(), codeword).encode(env))
}

#[rustler::nif(name = "decode_tm1536")]
fn decode_tm1536<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1536;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TM2048 scheme encode/decode
#[rustler::nif(name = "encode_tm2048")]
fn encode_tm2048<'a>(env: Env<'a>, data: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM2048;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data, &mut codeword);
    Ok((ok(), codeword).encode(env))
}

#[rustler::nif(name = "decode_tm2048")]
fn decode_tm2048<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM2048;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TM5120 scheme encode/decode
#[rustler::nif(name = "encode_tm5120")]
fn encode_tm5120<'a>(env: Env<'a>, data: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM5120;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data, &mut codeword);
    Ok((ok(), codeword).encode(env))
}

#[rustler::nif(name = "decode_tm5120")]
fn decode_tm5120<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM5120;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TM6144 scheme encode/decode
#[rustler::nif(name = "encode_tm6144")]
fn encode_tm6144<'a>(env: Env<'a>, data: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM6144;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data, &mut codeword);
    Ok((ok(), codeword).encode(env))
}

#[rustler::nif(name = "decode_tm6144")]
fn decode_tm6144<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM6144;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

// TM8192 scheme encode/decode
#[rustler::nif(name = "encode_tm8192")]
fn encode_tm8192<'a>(env: Env<'a>, data: Vec<u8>) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM8192;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data, &mut codeword);
    Ok((ok(), codeword).encode(env))
}

#[rustler::nif(name = "decode_tm8192")]
fn decode_tm8192<'a>(env: Env<'a>, rxcode: Vec<u8>, iterations: u8) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM8192;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode, &mut rxdata, &mut working, iterations as usize);
    Ok((ok(), rxdata).encode(env))
}

rustler::init!(
    "erldpc",
    [
        encode_tc128,
        decode_tc128,
        encode_tc256,
        decode_tc256,
        encode_tc512,
        decode_tc512,
        encode_tm1280,
        decode_tm1280,
        encode_tm1536,
        decode_tm1536,
        encode_tm2048,
        decode_tm2048,
        encode_tm5120,
        decode_tm5120,
        encode_tm6144,
        decode_tm6144,
        encode_tm8192,
        decode_tm8192,
    ],
    load = load
);

fn load(_env: Env, _: Term) -> bool {
    true
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
        let mut codeword = vec![0u8; code.n() / 8];

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
