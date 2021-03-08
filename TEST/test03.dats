
#include "share/atspre_staload.hats"
staload "./../SATS/sha1.sats"
staload _ = "./../DATS/sha1.dats"

implement main0 () = {

    var msg = @[byte][3](
        $UNSAFE.cast{byte}('M')
      , $UNSAFE.cast{byte}('a')
      , $UNSAFE.cast{byte}('n')
      )

    var output = @[byte][20]()
    var output_hex = @[char][41]('\0')

    val vals0 = sha1( msg, i2sz(3) )
    val () = println!(
        "h0 ", vals0.h0
     , " h1 ", vals0.h1
     , " h2 ", vals0.h2
     , " h3 ", vals0.h3
     , " h4 ", vals0.h4
    )

    val () = sha1hex( output_hex, vals0 )
    val () = println!($UNSAFE.cast{string}(addr@output_hex))

  }
