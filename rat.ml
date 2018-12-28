include Bignum

let (~-) = ( * ) ((of_float_decimal 0.) - of_float_decimal 1.)

let zero = of_float_decimal 0.

let sexp_of_t t = Core.Float.sexp_of_t (to_float t)