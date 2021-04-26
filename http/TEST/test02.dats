
#include "share/atspre_staload.hats"
staload "./../SATS/base64.sats"
staload _ = "./../DATS/base64.dats"

implement main0 () = {
    
    var buf = @[byte][3](
        $UNSAFE.cast{byte}('M')
      , $UNSAFE.cast{byte}('a')
      , $UNSAFE.cast{byte}('n')
      )

    var outbuf = @[char][7]()

    val outp = addr@outbuf 
    val (pf1,pf2 | m0) = base64_encode( view@outbuf | buf, outp, i2sz(3) )

    val _ = array_foreach<char>( !outp, m0 ) where {
      implement (env)
      array_foreach$fwork<char><env>( c, env ) = println!(c)
    }

    val () = array_uninitize<char>( !outp, m0 ) where {
        implement (e:t@ype)
        array_uninitize$clear<e>( x, x ) = () 
      }
    prval parr0 = array_v_unsplit( pf1, pf2 )
    prval () = view@outbuf := parr0

  }
