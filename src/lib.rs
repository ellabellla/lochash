use std::ops;

use num_traits::{identities, sign, PrimInt};

fn iterate_encode<T>(hash: &mut T, value: f64, interval: &mut (f64, f64))
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    let mid = (interval.0 + interval.1) / 2.0;
    if value > mid {
        *hash |= T::one();
        *interval = (mid, interval.1)
    } else {
        *interval = (interval.0, mid)
    }
}

fn iterate_decode<T>(hash: T, mask: T, interval: &mut (f64, f64))
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    if hash & mask != T::zero() {
        *interval = ((interval.0 + interval.1) / 2.0, interval.1);
    } else {
        *interval = (interval.0, (interval.0 + interval.1) / 2.0);
    }
}

pub fn axis_error<T>(interval: (f64, f64)) -> f64
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    let size = T::zero().leading_zeros() / 2;
    let mut error = (interval.0.abs() + interval.1.abs()) / 2.0;
    for _ in 0..size {
        error /= 2.0
    }
    error
}

pub fn axis_error_3d<T>(interval: (f64, f64)) -> f64
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    let size = T::zero().leading_zeros() / 3;
    let mut error = (interval.0.abs() + interval.1.abs()) / 2.0;
    for _ in 0..size {
        error /= 2.0
    }
    error
}

pub fn encode<T>(x: f64, y: f64, mut x_interval: (f64, f64), mut y_interval: (f64, f64)) -> T
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    let mut hash: T = T::zero();
    let size = hash.leading_zeros();

    let mut even = true;
    for i in 0..size {
        if even {
            iterate_encode(&mut hash, y, &mut y_interval)
        } else {
            iterate_encode(&mut hash, x, &mut x_interval)
        }
        even = !even;
        if i != size - 1 {
            hash <<= T::one();
        }
    }
    hash
}

pub fn decode<T>(hash: T, mut x_interval: (f64, f64), mut y_interval: (f64, f64)) -> (f64, f64)
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    let size = T::zero().leading_zeros();

    let mut even = true;
    let mut mask = !(T::max_value() >> 1);

    for _ in 0..size {
        if even {
            iterate_decode(hash, mask, &mut y_interval)
        } else {
            iterate_decode(hash, mask, &mut x_interval)
        }
        even = !even;
        mask >>= T::one();
    }

    let x = (x_interval.0 + x_interval.1) / 2.0;
    let y = (y_interval.0 + y_interval.1) / 2.0;

    return (x, y);
}

pub fn encode_3d<T>(
    x: f64,
    y: f64,
    z: f64,
    mut x_interval: (f64, f64),
    mut y_interval: (f64, f64),
    mut z_interval: (f64, f64),
) -> T
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    let mut hash: T = T::zero();
    let size = hash.leading_zeros() / 3;
    let padding = hash.leading_zeros() - (size * 3) - 1;

    for _ in 0..size {
        iterate_encode(&mut hash, y, &mut y_interval);
        hash <<= T::one();

        iterate_encode(&mut hash, x, &mut x_interval);
        hash <<= T::one();

        iterate_encode(&mut hash, z, &mut z_interval);
        hash <<= T::one();
    }

    for _ in 0..padding {
        hash <<= T::one();
    }

    hash
}

pub fn decode_3d<T>(
    hash: T,
    mut x_interval: (f64, f64),
    mut y_interval: (f64, f64),
    mut z_interval: (f64, f64),
) -> (f64, f64, f64)
where
    T: sign::Unsigned
        + PrimInt
        + identities::Zero
        + ops::BitOrAssign<T>
        + ops::ShlAssign
        + ops::ShrAssign
        + identities::One,
{
    let size = T::zero().leading_zeros() / 3;

    let mut mask = !(T::max_value() >> 1);

    for _ in 0..size {
        iterate_decode(hash, mask, &mut y_interval);
        mask >>= T::one();

        iterate_decode(hash, mask, &mut x_interval);
        mask >>= T::one();

        iterate_decode(hash, mask, &mut z_interval);
        mask >>= T::one();
    }

    let x = (x_interval.0 + x_interval.1) / 2.0;
    let y = (y_interval.0 + y_interval.1) / 2.0;
    let z = (z_interval.0 + z_interval.1) / 2.0;

    return (x, y, z);
}

#[cfg(test)]
mod tests {
    use crate::{axis_error, axis_error_3d, decode, decode_3d, encode, encode_3d};
    use lazy_static::lazy_static;

    const X_INTERVAL: (f64, f64) = (-90.0, 90.0);
    const Y_INTERVAL: (f64, f64) = (-180.0, 180.0);
    const Z_INTERVAL: (f64, f64) = (-90.0, 90.0);

    lazy_static! {
        static ref X_128_ERR: f64 = axis_error::<u128>(X_INTERVAL);
        static ref Y_128_ERR: f64 = axis_error::<u128>(Y_INTERVAL);
        static ref LOC_128_ERR: (f64, f64) = (*X_128_ERR, *Y_128_ERR);
        
        static ref X_3D_128_ERR: f64 = axis_error_3d::<u128>(X_INTERVAL);
        static ref Y_3D_128_ERR: f64 = axis_error_3d::<u128>(Y_INTERVAL);
        static ref Z_3D_128_ERR: f64 = axis_error_3d::<u128>(Z_INTERVAL);
        static ref LOC_3D_128_ERR: (f64, f64, f64) = (*X_3D_128_ERR, *Y_3D_128_ERR, *Z_3D_128_ERR);

        static ref X_64_ERR: f64 = axis_error::<u64>(X_INTERVAL);
        static ref Y_64_ERR: f64 = axis_error::<u64>(Y_INTERVAL);
        static ref LOC_64_ERR: (f64, f64) = (*X_64_ERR, *Y_64_ERR);

        static ref X_3D_64_ERR: f64 = axis_error_3d::<u64>(X_INTERVAL);
        static ref Y_3D_64_ERR: f64 = axis_error_3d::<u64>(Y_INTERVAL);
        static ref Z_3D_64_ERR: f64 = axis_error_3d::<u64>(Z_INTERVAL);
        static ref LOC_3D_64_ERR: (f64, f64, f64) = (*X_3D_64_ERR, *Y_3D_64_ERR, *Z_3D_64_ERR);

        static ref X_32_ERR: f64 = axis_error::<u32>(X_INTERVAL);
        static ref Y_32_ERR: f64 = axis_error::<u32>(Y_INTERVAL);
        static ref LOC_32_ERR: (f64, f64) = (*X_32_ERR, *Y_32_ERR);

        static ref X_3D_32_ERR: f64 = axis_error_3d::<u32>(X_INTERVAL);
        static ref Y_3D_32_ERR: f64 = axis_error_3d::<u32>(Y_INTERVAL);
        static ref Z_3D_32_ERR: f64 = axis_error_3d::<u32>(Z_INTERVAL);
        static ref LOC_3D_32_ERR: (f64, f64, f64) = (*X_3D_32_ERR, *Y_3D_32_ERR, *Z_3D_32_ERR);

        static ref X_16_ERR: f64 = axis_error::<u16>(X_INTERVAL);
        static ref Y_16_ERR: f64 = axis_error::<u16>(Y_INTERVAL);
        static ref LOC_16_ERR: (f64, f64) = (*X_16_ERR, *Y_16_ERR);

        static ref X_3D_16_ERR: f64 = axis_error_3d::<u16>(X_INTERVAL);
        static ref Y_3D_16_ERR: f64 = axis_error_3d::<u16>(Y_INTERVAL);
        static ref Z_3D_16_ERR: f64 = axis_error_3d::<u16>(Z_INTERVAL);
        static ref LOC_3D_16_ERR: (f64, f64, f64) = (*X_3D_16_ERR, *Y_3D_16_ERR, *Z_3D_16_ERR);

        static ref X_8_ERR: f64 = axis_error::<u8>(X_INTERVAL);
        static ref Y_8_ERR: f64 = axis_error::<u8>(Y_INTERVAL);
        static ref LOC_8_ERR: (f64, f64) = (*X_8_ERR, *Y_8_ERR);

        static ref X_3D_8_ERR: f64 = axis_error_3d::<u8>(X_INTERVAL);
        static ref Y_3D_8_ERR: f64 = axis_error_3d::<u8>(Y_INTERVAL);
        static ref Z_3D_8_ERR: f64 = axis_error_3d::<u8>(Z_INTERVAL);
        static ref LOC_3D_8_ERR: (f64, f64, f64) = (*X_3D_8_ERR, *Y_3D_8_ERR, *Z_3D_8_ERR);
    }

    fn within_error(value: f64, error: f64, result: f64) -> bool {
        result >= value - error && result <= value + error
    }

    fn loc_within_error(loc: (f64, f64), error: (f64, f64), result: (f64, f64)) -> bool {
        let (x, y) = loc;
        let (x_err, y_err) = error;
        let (r_x, r_y) = result;
        within_error(x, x_err, r_x) && within_error(y, y_err, r_y)
    }

    fn loc_3d_within_error(
        loc: (f64, f64, f64),
        error: (f64, f64, f64),
        result: (f64, f64, f64),
    ) -> bool {
        let (x, y, z) = loc;
        let (x_err, y_err, z_err) = error;
        let (r_x, r_y, r_z) = result;
        within_error(x, x_err, r_x) && within_error(y, y_err, r_y) && within_error(z, z_err, r_z)
    }

    fn test_all_dem_128(loc: (f64, f64, f64)) {
        let hash: u128 = encode(loc.0, loc.1, X_INTERVAL, Y_INTERVAL);
        assert!(loc_within_error(
            (loc.0, loc.1),
            *LOC_128_ERR,
            decode(hash, X_INTERVAL, Y_INTERVAL)
        ));

        let hash: u128 = encode_3d(loc.0, loc.1, loc.2, X_INTERVAL, Y_INTERVAL, Z_INTERVAL);
        assert!(loc_3d_within_error(
            loc,
            *LOC_3D_128_ERR,
            decode_3d(hash, X_INTERVAL, Y_INTERVAL, Z_INTERVAL)
        ));
    }

    fn test_all_dem_64(loc: (f64, f64, f64)) {
        let hash: u64 = encode(loc.0, loc.1, X_INTERVAL, Y_INTERVAL);
        assert!(loc_within_error(
            (loc.0, loc.1),
            *LOC_64_ERR,
            decode(hash, X_INTERVAL, Y_INTERVAL)
        ));

        let hash: u64 = encode_3d(loc.0, loc.1, loc.2, X_INTERVAL, Y_INTERVAL, Z_INTERVAL);
        assert!(loc_3d_within_error(
            loc,
            *LOC_3D_64_ERR,
            decode_3d(hash, X_INTERVAL, Y_INTERVAL, Z_INTERVAL)
        ));
    }

    fn test_all_dem_32(loc: (f64, f64, f64)) {
        let hash: u32 = encode(loc.0, loc.1, X_INTERVAL, Y_INTERVAL);
        assert!(loc_within_error(
            (loc.0, loc.1),
            *LOC_32_ERR,
            decode(hash, X_INTERVAL, Y_INTERVAL)
        ));

        let hash: u32 = encode_3d(loc.0, loc.1, loc.2, X_INTERVAL, Y_INTERVAL, Z_INTERVAL);
        assert!(loc_3d_within_error(
            loc,
            *LOC_3D_32_ERR,
            decode_3d(hash, X_INTERVAL, Y_INTERVAL, Z_INTERVAL)
        ));
    }

    fn test_all_dem_16(loc: (f64, f64, f64)) {
        let hash: u16 = encode(loc.0, loc.1, X_INTERVAL, Y_INTERVAL);
        assert!(loc_within_error(
            (loc.0, loc.1),
            *LOC_16_ERR,
            decode(hash, X_INTERVAL, Y_INTERVAL)
        ));

        let hash: u16 = encode_3d(loc.0, loc.1, loc.2, X_INTERVAL, Y_INTERVAL, Z_INTERVAL);
        assert!(loc_3d_within_error(
            loc,
            *LOC_3D_16_ERR,
            decode_3d(hash, X_INTERVAL, Y_INTERVAL, Z_INTERVAL)
        ));
    }

    fn test_all_dem_8(loc: (f64, f64, f64)) {
        let hash: u8 = encode(loc.0, loc.1, X_INTERVAL, Y_INTERVAL);
        assert!(loc_within_error(
            (loc.0, loc.1),
            *LOC_8_ERR,
            decode(hash, X_INTERVAL, Y_INTERVAL)
        ));

        let hash: u8 = encode_3d(loc.0, loc.1, loc.2, X_INTERVAL, Y_INTERVAL, Z_INTERVAL);
        assert!(loc_3d_within_error(
            loc,
            *LOC_3D_8_ERR,
            decode_3d(hash, X_INTERVAL, Y_INTERVAL, Z_INTERVAL)
        ));
    }

    #[test]
    pub fn test_128() {
        test_all_dem_128((90.0, 100.0, 90.0));
        test_all_dem_128((12.0, 46.0, 69.0));
        test_all_dem_128((37.0, -146.0, 0.0));
        test_all_dem_128((0.0, 0.0, 0.0));
        test_all_dem_128((90.0, 180.0, 90.0));
        test_all_dem_128((-90.0, -180.0, -90.0));
    }

    #[test]
    pub fn test_64() {
        test_all_dem_64((90.0, 100.0, 90.0));
        test_all_dem_64((12.0, 46.0, 69.0));
        test_all_dem_64((37.0, 146.0, 0.0));
        test_all_dem_64((0.0, 0.0, 0.0));
        test_all_dem_64((90.0, 180.0, 90.0));
        test_all_dem_64((-90.0, -180.0, -90.0));
    }

    #[test]
    pub fn test_32() {
        test_all_dem_32((90.0, 100.0, 90.0));
        test_all_dem_32((12.0, 46.0, 69.0));
        test_all_dem_32((37.0, 146.0, 0.0));
        test_all_dem_32((0.0, 0.0, 0.0));
        test_all_dem_32((90.0, 180.0, 90.0));
        test_all_dem_32((-90.0, -180.0, -90.0));
    }

    #[test]
    pub fn test_16() {
        test_all_dem_16((90.0, 100.0, 90.0));
        test_all_dem_16((12.0, 46.0, 69.0));
        test_all_dem_16((37.0, 146.0, 0.0));
        test_all_dem_16((0.0, 0.0, 0.0));
        test_all_dem_16((90.0, 180.0, 90.0));
        test_all_dem_16((-90.0, -180.0, -90.0));
    }

    #[test]
    pub fn test_8() {
        test_all_dem_8((90.0, 100.0, 90.0));
        test_all_dem_8((12.0, 46.0, 69.0));
        test_all_dem_8((37.0, 146.0, 0.0));
        test_all_dem_8((0.0, 0.0, 0.0));
        test_all_dem_8((90.0, 180.0, 90.0));
        test_all_dem_8((-90.0, -180.0, -90.0));
    }
}
