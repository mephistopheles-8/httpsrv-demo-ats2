
#include "share/atspre_staload.hats"
staload "./../SATS/sha1.sats"
staload _ = "./../DATS/sha1.dats"

%{
static void
_sha1_print( uint32_t a, uint32_t b, uint32_t c, uint32_t d, uint32_t e) 
{
  printf("%x %x %x %x %x", a, b, c, d, e);
}
%}

extern
fun _sha1_print( uint32, uint32, uint32, uint32, uint32 ) : void = "mac#"


implement main0 () = {

    var msg = @[byte][3](
        $UNSAFE.cast{byte}('M')
      , $UNSAFE.cast{byte}('a')
      , $UNSAFE.cast{byte}('n')
      )

    var output = @[byte][20]()

    val vals0 = sha1( msg, i2sz(3) )
    val () = println!(
        "h0 ", vals0.h0
     , " h1 ", vals0.h1
     , " h2 ", vals0.h2
     , " h3 ", vals0.h3
     , " h4 ", vals0.h4
    )

    val () = _sha1_print( vals0.h0, vals0.h1, vals0.h2, vals0.h3, vals0.h4 );
  }
