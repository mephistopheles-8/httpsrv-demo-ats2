
#include "share/atspre_staload.hats"
staload "./../SATS/sha1.sats"
staload _ = "./../DATS/sha1.dats"
staload "./../SATS/base64.sats"
staload _ = "./../DATS/base64.dats"

implement main0 () = {
    (** len : 60 **)
    val digest = "dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    (*
     The server would then take the SHA-1 hash of this,
     giving the value 0xb3 0x7a 0x4f 0x2c 0xc0 0x62 0x4f 0x16 0x90 0xf6
     0x46 0x06 0xcf 0x38 0x59 0x45 0xb2 0xbe 0xc4 0xea.
     s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
    *)

    val p = ptrcast(digest) 
    val (pf,pff | p) = $UNSAFE.ptr0_vtake{bytes(60)}( p )

    val vals0 = sha1( !p, i2sz(60) )

    var output_bin = @[byte][20]()
    var output_hex = @[char][41]('\0')
    var output_b64 = @[char][29]('\0')

    val () = sha1hex( output_hex, vals0 )
    val () = println!( $UNSAFE.cast{string}(addr@output_hex) )

    val () = sha1bin( output_bin, vals0 )
    val (pb640,pb641 | m0) = base64_encode( view@output_b64 | output_bin, addr@output_b64, i2sz(20) )

    val () = println!( $UNSAFE.cast{string}(addr@output_b64) )

    prval () = __assert(pb640) where {
      extern prfn __assert{n:nat}{l:addr}( !array(char,n) @ l >> array(char?,n) @ l ) : void
    }

    prval pb64 = array_v_unsplit( pb640, pb641 )
    prval () = view@output_b64 := pb64

    prval () = pff(pf)

  }
