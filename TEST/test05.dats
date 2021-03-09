
#include "share/atspre_staload.hats"
staload "./../SATS/websocket.sats"
staload _ = "./../DATS/websocket.dats"
staload _ = "./../DATS/base64.dats"
staload _ = "./../DATS/sha1.dats"

implement main0 () = {
    (** len : 24 **)
    val sec_ws_key = "dGhlIHNhbXBsZSBub25jZQ=="
    
    var output_b64 = @[char][29]()
    
    val p = ptrcast(sec_ws_key) 
    val (pf,pff | p) = $UNSAFE.ptr0_vtake{array(char,24)}( p )

    val () = ws_handshake_accept( !p, output_b64 )

    val () = println!( $UNSAFE.cast{string}( addr@output_b64 ) )

    prval () = pff( pf )

  }
