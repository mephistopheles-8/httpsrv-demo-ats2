
#include "./../HATS/project.hats"
#include "share/atspre_staload.hats"

staload "./../SATS/base64.sats"
staload "./../SATS/sha1.sats"
staload "./../SATS/websocket.sats"

#define WS_UUID "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

implement {}
ws_frame_parser_init (ap,sz) = @{
      is_fin = false
    , opcode = 0U
    , is_masked = false
    , payload_begun = false
    , masking_key = 0U
    , payload_length = 0LLU
    , payload_length_size = 0
    , buf = ap
    , bufsz = sz
    , j = i2sz(0)
    , i = 0LLU
    , k = 0LLU
  }

implement {}
ws_frame_parser_destroy( wfp ) = wfp.buf

implement {}
ws_frame_parser_reset_buf (wfp) = wfp.j := i2sz(0)

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

implement {}
ws_frame_parse{n,bsz}{l}( buf, n, env ) = (
    ifcase
     | env.payload_begun && env.k = env.payload_length => ws_success()
     | env.j = env.bufsz => ws_buffer_full()
     | _ => ws_continue() 
  ) where {
    val _ = array_foreach_env<byte><ws_frame_parser(l,bsz)>( buf, n, env ) where {
      implement 
      array_foreach$cont<byte><ws_frame_parser(l,bsz)>( b, env ) = (
        env.j < env.bufsz && ( ~env.payload_begun || env.k < env.payload_length )
      )
      implement 
      array_foreach$fwork<byte><ws_frame_parser(l,bsz)>( b, env ) = (
            ifcase
             | i = 0 =>
                let
                    val is_fin = (b0 >> 7) = 1U
                    val opcode = g1ofg0(b0) mod 16U
                 in
                   env.is_fin := is_fin;
                   env.opcode := opcode;
                end
             | i = 1 =>
                let
                    val is_masked = (b0 >> 7) = 1U
                    val payload_length = b0 land 0x7FU
                    val () = env.is_masked := is_masked
                 in ifcase
                     | payload_length = 0x7F => (
                          env.payload_length_size := 8;
                        )
                     | payload_length = 0x7D => (
                          env.payload_length_size := 2;
                        )
                     | _ => (
                        env.payload_length := g0uint2uint( payload_length );
                        env.payload_length_size := 0;
                        if ~is_masked then env.payload_begun := true;
                      )
                end
             | i >= 2 && i < 10 && env.payload_length_size = 8 => (
                  env.payload_length := (env.payload_length << 8) lor g0uint2uint(b0);
                  if i = 9 && ~env.is_masked then env.payload_begun := true;
                )
             | i >= 2 && i < 4 && env.payload_length_size = 2 => (
                  env.payload_length := (env.payload_length << 8) lor g0uint2uint(b0);
                  if i = 3 && ~env.is_masked then env.payload_begun := true;
              )
             | env.is_masked &&
               i >= (2 + env.payload_length_size) && 
               i < (2 + env.payload_length_size + 4) => (
                  env.masking_key := (env.masking_key << 8) lor b0;
                  if i = (2 + env.payload_length_size + 3) then env.payload_begun := true;
              ) 
             | env.is_masked => 
               let
                  val j = env.j
                  val bsz = env.bufsz
                  val i0 = $UNSAFE.cast{size_t}(
                      $UNSAFE.cast{int}(i)- 2 - env.payload_length_size - 4
                    ) 
                  val b1 = 
                    b0 lxor (
                      (env.masking_key >> 8*(
                         3 - ($UNSAFE.cast{intBtwe(0,3)}(i0 mod i2sz(4))) 
                       )
                      ) land 0xFFU
                    )

                in if j < bsz
                   then begin 
                    arrayptr_set_at<byte>( env.buf, j, i2byte(b1) );
                    env.j := j + 1;
                    env.k := env.k + 1LLU;
                   end
               end  
             | _ => 
               let
                  val j = env.j
                  val bsz = env.bufsz
                in if j < bsz
                   then begin 
                    arrayptr_set_at<byte>( env.buf, j, b );
                    env.j := j + 1;
                    env.k := env.k + 1LLU;
                   end
               end 
          ) where {
            val b0 = byte2uint0( b )
            val i = env.i
            val () = env.i := i + 1LLU
          } 
    }

  }
