fn main() {
    let message = "msg";
    let msg = encode_message(message);

    let msg_by_4bytes = create_4bytes_chunks(msg);

    let mut hash = H;

    for i in 0..msg_by_4bytes.len() {
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut h] = hash;
        let w = calc_w(msg_by_4bytes[i]);

        for j in 0..64 {
            let t1 = h
                .wrapping_add(upper_sigma_1(e))
                .wrapping_add(ch(e, f, g))
                .wrapping_add(K[j])
                .wrapping_add(w[j]);
            let t2 = upper_sigma_0(a).wrapping_add(maj(a, b, c));

            h = g;
            g = f;
            f = e;
            e = d.wrapping_add(t1);
            d = c;
            c = b;
            b = a;
            a = t1.wrapping_add(t2);
        }

        hash[0] = a.wrapping_add(hash[0]);
        hash[1] = b.wrapping_add(hash[1]);
        hash[2] = c.wrapping_add(hash[2]);
        hash[3] = d.wrapping_add(hash[3]);
        hash[4] = e.wrapping_add(hash[4]);
        hash[5] = f.wrapping_add(hash[5]);
        hash[6] = g.wrapping_add(hash[6]);
        hash[7] = h.wrapping_add(hash[7]);
    }

    let result = hash.iter().map(|x| format!("{:x}", x)).collect::<String>();
    print!("{}", result);
}

const K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

const H: [u32; 8] = [
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
];

fn encode_message(message: &str) -> Vec<u8> {
    let mut padded_msg = message.as_bytes().to_vec();

    let bit_length = padded_msg.len() as u64 * 8;

    let bit_length_byte_array = bit_length.to_be_bytes();

    padded_msg.push(0x80);

    // 64: block size
    // (padded_msg.len() % 64): message size
    // 8: bit length size
    let padding_length = 64 - (padded_msg.len() % 64) - 8;

    // 0x00: padding
    padded_msg.extend(vec![0; padding_length]);

    // bit length
    padded_msg.extend(bit_length_byte_array.iter());

    assert!(padded_msg.len() % 64 == 0);

    padded_msg
}

fn create_4bytes_chunks(msg: Vec<u8>) -> Vec<[u32; 16]> {
    let chunks_count = msg.len() / 64;
    let mut chunks: Vec<[u32; 16]> = Vec::with_capacity(chunks_count);

    for c in 0..chunks_count {
        let mut chunk = [0u32; 16];
        for i in 0..16 {
            let base = c * 64 + i * 4;
            chunk[i] = ((msg[base] as u32) << 24)
                | ((msg[base + 1] as u32) << 16)
                | ((msg[base + 2] as u32) << 8)
                | (msg[base + 3] as u32);
        }
        chunks.push(chunk);
    }

    chunks
}

fn calc_w(msg: [u32; 16]) -> [u32; 64] {
    let mut w: [u32; 64] = [0; 64];

    for i in 0..16 {
        w[i] = msg[i];
    }

    for i in 16..64 {
        w[i] = lower_sigma_1(w[i - 2])
            .wrapping_add(w[i - 7])
            .wrapping_add(lower_sigma_0(w[i - 15]))
            .wrapping_add(w[i - 16]);
    }

    w
}
fn rotr(x: u32, n: u32) -> u32 {
    (x >> n) | (x << (32 - n))
}

fn shr(x: u32, n: u32) -> u32 {
    x >> n
}

fn ch(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ (!x & z)
}

fn maj(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ (x & z) ^ (y & z)
}

fn upper_sigma_0(x: u32) -> u32 {
    rotr(x, 2) ^ rotr(x, 13) ^ rotr(x, 22)
}

fn upper_sigma_1(x: u32) -> u32 {
    rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25)
}

fn lower_sigma_0(x: u32) -> u32 {
    rotr(x, 7) ^ rotr(x, 18) ^ shr(x, 3)
}

fn lower_sigma_1(x: u32) -> u32 {
    rotr(x, 17) ^ rotr(x, 19) ^ shr(x, 10)
}
