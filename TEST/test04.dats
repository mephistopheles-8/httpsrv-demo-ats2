
#include "share/atspre_staload.hats"
staload "./../SATS/sha1.sats"
staload _ = "./../DATS/sha1.dats"

implement main0 () = {
    (** len : 60 **)
    val digest = "dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    (*
     The server would then take the SHA-1 hash of this,
     giving the value 0xb3 0x7a 0x4f 0x2c 0xc0 0x62 0x4f 0x16 0x90 0xf6
     0x46 0x06 0xcf 0x38 0x59 0x45 0xb2 0xbe 0xc4 0xea.
    *)

    val p = ptrcast(digest) 
    val (pf,pff | p) = $UNSAFE.ptr0_vtake{bytes(60)}( p )

    val vals0 = sha1( !p, i2sz(60) )

    var output_hex = @[char][41]('\0')
    val () = sha1hex( output_hex, vals0 )

    val () = println!( $UNSAFE.cast{string}(addr@output_hex))

    prval () = pff(pf)

  }
