
#include "./../HATS/project.hats"

(** TODO: extra 3 chars in buf; necessary for constraints if n < 2 **)
fun {} base64_encode{n,m:nat | m >= (4*n/3 + 3)}{l:addr}( 
    array_v(char?,l,m) | &bytes(n), ptr l, size_t n 
  ) : #[m0:nat | m0 <= m] ( 
      array_v(char,l,m0)
    , array_v(char?,l + m0*sizeof(char),m - m0) 
    | size_t m0 ) 
