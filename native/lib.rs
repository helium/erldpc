use crate::bin::Bin;
use labrador_ldpc::LDPCCode;
use rustler::{Encoder, Env, NifResult, Term};

mod bin;

rustler::atoms! {
    ok,
}

// TC128 scheme encode/decode
#[rustler::nif(name = "encode_tc128")]
fn encode_tc128<'a>(env: Env<'a>, txdata: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC128;
    let mut txcode = vec![0u8; code.n() / 8];
    code.copy_encode(&txdata.0, &mut txcode);
    Ok((ok(), Bin(txcode)).encode(env))
}

#[rustler::nif(name = "decode_bf_tc128")]
fn decode_bf_tc128<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC128;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..8];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tc128")]
fn decode_ms_tc128<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC128;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 128];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..8];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tc128")]
fn decode_ms_soft_tc128<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC128;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..8];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TC256 scheme encode/decode
#[rustler::nif(name = "encode_tc256")]
fn encode_tc256<'a>(env: Env<'a>, txdata: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC256;
    let mut txcode = vec![0u8; code.n() / 8];
    code.copy_encode(&txdata.0, &mut txcode);
    Ok((ok(), Bin(txcode)).encode(env))
}

#[rustler::nif(name = "decode_ms_tc256")]
fn decode_ms_tc256<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC256;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 256];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..16];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tc256")]
fn decode_ms_soft_tc256<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC256;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..16];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_bf_tc256")]
fn decode_bf_tc256<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC256;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..16];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TC512 scheme encode/decode
#[rustler::nif(name = "encode_tc512")]
fn encode_tc512<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC512;
    // Allocate memory for the encoded data
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data.0, &mut codeword);
    Ok((ok(), Bin(codeword)).encode(env))
}

#[rustler::nif(name = "decode_bf_tc512")]
fn decode_bf_tc512<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC512;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..32];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tc512")]
fn decode_ms_tc512<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC512;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 512];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..32];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tc512")]
fn decode_ms_soft_tc512<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TC512;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..32];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TM1280 scheme encode/decode
#[rustler::nif(name = "encode_tm1280")]
fn encode_tm1280<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1280;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data.0, &mut codeword);
    Ok((ok(), Bin(codeword)).encode(env))
}

#[rustler::nif(name = "decode_bf_tm1280")]
fn decode_bf_tm1280<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1280;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tm1280")]
fn decode_ms_tm1280<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1280;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 1280];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tm1280")]
fn decode_ms_soft_tm1280<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1280;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TM1536 scheme encode/decode
#[rustler::nif(name = "encode_tm1536")]
fn encode_tm1536<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1536;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data.0, &mut codeword);
    Ok((ok(), Bin(codeword)).encode(env))
}

#[rustler::nif(name = "decode_bf_tm1536")]
fn decode_bf_tm1536<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1536;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tm1536")]
fn decode_ms_tm1536<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1536;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 1536];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tm1536")]
fn decode_ms_soft_tm1536<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM1536;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TM2048 scheme encode/decode
#[rustler::nif(name = "encode_tm2048")]
fn encode_tm2048<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM2048;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data.0, &mut codeword);
    Ok((ok(), Bin(codeword)).encode(env))
}

#[rustler::nif(name = "decode_bf_tm2048")]
fn decode_bf_tm2048<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM2048;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tm2048")]
fn decode_ms_tm2048<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM2048;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 2048];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tm2048")]
fn decode_ms_soft_tm2048<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM2048;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..128];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TM5120 scheme encode/decode
#[rustler::nif(name = "encode_tm5120")]
fn encode_tm5120<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM5120;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data.0, &mut codeword);
    Ok((ok(), Bin(codeword)).encode(env))
}

#[rustler::nif(name = "decode_bf_tm5120")]
fn decode_bf_tm5120<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM5120;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tm5120")]
fn decode_ms_tm5120<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM5120;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 5120];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tm5120")]
fn decode_ms_soft_tm5120<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM5120;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TM6144 scheme encode/decode
#[rustler::nif(name = "encode_tm6144")]
fn encode_tm6144<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM6144;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data.0, &mut codeword);
    Ok((ok(), Bin(codeword)).encode(env))
}

#[rustler::nif(name = "decode_bf_tm6144")]
fn decode_bf_tm6144<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM6144;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tm6144")]
fn decode_ms_tm6144<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM6144;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 6144];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tm6144")]
fn decode_ms_soft_tm6144<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM6144;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

// TM8192 scheme encode/decode
#[rustler::nif(name = "encode_tm8192")]
fn encode_tm8192<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM8192;
    let mut codeword = vec![0u8; code.n() / 8];
    code.copy_encode(&data.0, &mut codeword);
    Ok((ok(), Bin(codeword)).encode(env))
}

#[rustler::nif(name = "decode_bf_tm8192")]
fn decode_bf_tm8192<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM8192;
    let mut working = vec![0u8; code.decode_bf_working_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_bf(&rxcode.0, &mut rxdata, &mut working, iterations);
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_tm8192")]
fn decode_ms_tm8192<'a>(env: Env<'a>, rxcode: Bin, iterations: usize) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM8192;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    let mut llrs = vec![0i8; 8192];
    code.hard_to_llrs(&rxcode.0, &mut llrs);
    code.decode_ms(
        &llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

#[rustler::nif(name = "decode_ms_soft_tm8192")]
fn decode_ms_soft_tm8192<'a>(
    env: Env<'a>,
    in_llrs: Vec<i8>,
    iterations: usize,
) -> NifResult<Term<'a>> {
    let code = LDPCCode::TM8192;
    let mut working = vec![0i8; code.decode_ms_working_len()];
    let mut working_u8 = vec![0u8; code.decode_ms_working_u8_len()];
    let mut rxdata = vec![0u8; code.output_len()];
    code.decode_ms(
        &in_llrs,
        &mut rxdata,
        &mut working,
        &mut working_u8,
        iterations,
    );
    let out = &rxdata[..512];
    Ok((ok(), Bin(out.to_vec())).encode(env))
}

rustler::init!(
    "erldpc",
    [
        encode_tc128,
        decode_bf_tc128,
        decode_ms_tc128,
        decode_ms_soft_tc128,
        encode_tc256,
        decode_bf_tc256,
        decode_ms_tc256,
        decode_ms_soft_tc256,
        encode_tc512,
        decode_bf_tc512,
        decode_ms_tc512,
        decode_ms_soft_tc512,
        encode_tm1280,
        decode_bf_tm1280,
        decode_ms_tm1280,
        decode_ms_soft_tm1280,
        encode_tm1536,
        decode_bf_tm1536,
        decode_ms_tm1536,
        decode_ms_soft_tm1536,
        encode_tm2048,
        decode_bf_tm2048,
        decode_ms_tm2048,
        decode_ms_soft_tm2048,
        encode_tm5120,
        decode_bf_tm5120,
        decode_ms_tm5120,
        decode_ms_soft_tm5120,
        encode_tm6144,
        decode_bf_tm6144,
        decode_ms_tm6144,
        decode_ms_soft_tm6144,
        encode_tm8192,
        decode_bf_tm8192,
        decode_ms_tm8192,
        decode_ms_soft_tm8192,
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
