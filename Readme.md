# LocHash

A generic library for calculating GeoHash like hashes for arbitrary 2d and 3d coordinates. The hash is encoded into an unsigned integer. The precision of the hash is determined by the size of the unsigned integer.

## Usage
```rust
// The default GeoHash intervals
let x_interval = (-90.0, 90.0);
let y_interval = (-180.0, 180.0);

let hash: u64 = encode(60.2, 120.5, x_interval, y_interval);
println!("Location Hash: {}", hash);

let (x, y) = decode(hash, x_interval, y_interval);
println!("Decoded Location: {}, {}", x, y);

let x_error = axis_error::<u64>(x_interval);
let y_error = axis_error::<u64>(y_interval);
println!("X and Y hash error: {}, {}", x_error, y_error);
```


## License
This software is provided under the MIT license. [Click](LICENSE) here to view.