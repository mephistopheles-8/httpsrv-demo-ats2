
#include "./../HATS/project.hats"

typedef sha1_values = @{
  h0 = uint32 
, h1 = uint32 
, h2 = uint32 
, h3 = uint32 
, h4 = uint32 
}

fun {} sha1{n:nat}(msg: &bytes(n), len: size_t n) : sha1_values
