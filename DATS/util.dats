
#include "share/atspre_staload.hats"

extern
castfn byte2ui ( byte ) : 
    [n: nat | n <= 255] uint n

fun bin2hex{n:nat}( buf: &bytes(n), sz: size_t n, out: &array(char,2*n + 1) )
  : void = loop( buf, out, i2sz(0), sz ) 
     where {
      fun loop{i,n:nat | i <= n}( buf: &bytes(n), out: &array(char,2*n + 1), i: size_t i, sz: size_t n )
      : void =
        if i < sz
        then 
         let
           val hextbl = "0123456789abcdef"
           val b0 = byte2ui( buf[i] )
          in
            out[2*i] := hextbl[ $UNSAFE.cast{uintLte(15)}( b0 >> 4 ) ];
            out[2*i + 1] := hextbl[ b0 mod 16U ];
            loop( buf, out, i + 1, sz )
         end
        else out[2*i] := '\0'
    }
