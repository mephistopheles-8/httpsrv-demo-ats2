#include "./../HATS/project.hats"

staload "./../SATS/base64.sats"
staload "./../SATS/sha1.sats"
staload "./../SATS/websocket.sats"

#define WS_UUID "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

implement {}
ws_handshake_accept( sec_websocket_key, sec_websocket_accept ) =
  let
      var buf = @[char][60]()
      var output_bin = @[byte][20]()

      val buf0p = addr@buf
      val buf1p = ptr_add<char>( buf0p, i2sz(24) )

      prval (pbuf0, pbuf1) = array_v_split_at( view@buf | i2sz(24) )      
     
      val () = array_copy<char>( !buf0p, sec_websocket_key, i2sz(24) ) 

      var loop0 
        = fix@loop0( buf: &array(char?,36) >> array(char,36), i : sizeLte(36) )
        : void =>
            if i < 36
            then
             let
                val ws_uuid = WS_UUID
                val c = ws_uuid[i] 
                val () = buf[i,c] 
              in loop0( buf, i + 1 )
             end
            else {
              extern prfn buf_is_set( buf: &array(char?,36) >> array(char,36) ) : void
              prval () = buf_is_set( buf )
            }

      val () = loop0( !buf1p, i2sz(0) )

      prval pbuf = array_v_unsplit( pbuf0, pbuf1 )
      prval () = view@buf := pbuf

      extern prfn chars2bytes{n:nat}( &array(char,n) >> array(byte,n) ) : void
      extern prfn bytes2chars{n:nat}( &array(byte,n) >> array(char,n) ) : void

      prval () = chars2bytes( buf )

      val vals0 = sha1( buf, i2sz(60) )
      
      prval () = bytes2chars( buf )

      val () = sha1bin( output_bin, vals0 )

      val (pswa0,pswa1 | m0) 
        = base64_encode( 
            view@sec_websocket_accept 
          | output_bin
          , addr@sec_websocket_accept
          , i2sz(20) 
          )

      val () = assertloc( m0 = 28 )

      prval pf = array_v_unsing( pswa1 )
      val px = ptr_add<char>( addr@sec_websocket_accept, m0 )
      val () = ptr_set<char>( pf | px, '\0' )
      prval pswa1 = array_v_sing( pf )

      prval pswa = array_v_unsplit( pswa0, pswa1 )
      
      prval () = view@sec_websocket_accept := pswa
   in
  end


