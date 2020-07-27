const toF32 = new Float32Array(1);
const toF64 = new Float64Array(1);

export function convertToF32AndBack(f64) {
    toF32[0] = f64;
    toF64[0] = toF32[0];
    return toF64[0];
}