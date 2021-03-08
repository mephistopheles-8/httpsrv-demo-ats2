
#include "./../HATS/project.hats"
staload "./../SATS/sha1.sats"

fun {} rotl{n:nat | n <= 32}( x: uint32, shift: int n ) : uint32 
  = (x << shift) lor (x >> (32 - shift))

implement {}
sha1{n}( msg, output, len )
  = let

      (** Integers here should be in "machine" endianness **)

			macdef h0i = 0x67452301U
			macdef h1i = 0xEFCDAB89U
			macdef h2i = 0x98BADCFEU
			macdef h3i = 0x10325476U
			macdef h4i = 0xC3D2E1F0U

			val ml = 8*len

      var buf = @[byte][64]()
      var ubuf = @[uint32][80](g0uint2uint(0U))

      typedef values = @{
        h0 = uint32 
      , h1 = uint32 
      , h2 = uint32 
      , h3 = uint32 
      , h4 = uint32 
      }

      var values0 : values = @{
          h0 = g0uint2uint(h0i)
        , h1 = g0uint2uint(h1i)
        , h2 = g0uint2uint(h2i)
        , h3 = g0uint2uint(h3i)
        , h4 = g0uint2uint(h4i)
      }

      fun loop{i,n:nat | i <= n + 64}{msg:addr}( 
          pfm: !array_v(byte,msg,n) 
        | buf: &b0ytes(64)
        , ubuf: &array(uint32,80)
        , msg: ptr msg
        , i: size_t i
        , n: size_t n
        , values0 : &values 
      ) : void 
      = let
          vtypedef env = @{
            msg = arrayptr(byte,msg,n)  
          , i = sizeLte(n)
          , n = size_t n
          , padding = sizeLte(64)
          }
          prval () = b0ytes2bytes( buf )
          val (pf | ap) = arrayptr_objectify( pfm | msg )
          var env0 : env = @{
              msg = ap
            , i = (if i < n then i else n) : sizeLte(n)
            , n = n
            , padding = (if i > n then i - n else i2sz(0)) : sizeLte(64)
          }

          (** Move the message into the 512-bit block; 
              add the one bit at the end **)

          val _ = array_foreach_env<byte><env>( buf, i2sz(64), env0 ) where {
            implement
            array_foreach$fwork<byte><env>( b, env ) 
              = let
                  val i = env.i
                  val n = env.n  
                 in if i < n
                    then (
                      b := arrayptr_get_at<byte>(env.msg, i); 
                      env.i := i + i2sz(1)
                    )
                    else (
                      b := (if env.padding = 0 then i2byte(0x80) else i2byte(0)); 
                      env.padding := ((env.padding + 1) mod i2sz(64))
                    )
                end
          }
          
          val (pfm0 | _) = arrayptr_unobjectify( pf | env0.msg )
          prval () = pfm := pfm0

          (** If we have enough padding, add the big-endian msg bit-len
              to the end of the block **)

          val i1 = env0.i          
          val padding = env0.padding

          val () = (
            if padding >= 5
            then 
             let
                val ml = n*8
                var loop 
                  = fix@loop( buf: &bytes(64), i : sizeLte(4), ml: size_t ) 
                  : void => ( 
                    if i > 0
                    then 
                     let
                        val () = buf[i2sz(64) - i] := (
                          i2byte( sz2i(ml land i2sz(0xFF)) )
                        )
                      in loop(buf, i - 1, ml >> 8)
                     end
                    else () 
                  )         
              in loop( buf, i2sz(4), ml )
             end
            else ()
          ) 
     
          (** Write msg buf to 80-elem word array **)
    
          fun loop0 ( msg: &bytes(64), buf: &array(uint32,80), i : sizeLte(80) ) 
            : void 
            = if i < 80
              then 
                if i < 16
                then 
                 let
                     val b0 = byte2uint0(msg[i*4])
                     val b1 = byte2uint0(msg[i*4 + 1])
                     val b2 = byte2uint0(msg[i*4 + 2])
                     val b3 = byte2uint0(msg[i*4 + 3])

                     (* Need to verify; I think this is correct *) 
                     val u : uint32 =  g0uint2uint( (b0 << 24) lor (b1 << 16) lor (b2 << 8) lor b3 )

                     val () = buf[i,u]
                  in loop0( msg, buf, i + i2sz(1))
                 end
                else
                 let
                     val () = buf[i,rotl( buf[i - 3] lxor buf[i - 8] lxor buf[i - 14] lxor buf[i - 16],  1)]
                  in loop0( msg, buf, i + i2sz(1))
                 end
              else ()

          prval () = b0ytes2bytes( buf )
          val () = loop0( buf, ubuf, i2sz(0) )

          (** Calculate values and integrate **)
          var state : values = values0

          val _ = array_iforeach_env<uint32><values>( ubuf, i2sz(80), state ) where {
            implement
            array_iforeach$fwork<uint32><values>( i, w, env ) =
              let
                  val a = env.h0
                  val b = env.h1
                  val c = env.h2
                  val d = env.h3
                  val e = env.h4

                  val @(f,k) 
                    = (
                      ifcase
                      | i < 20 => @(f,k) where {
                            val f = (b land c) lor ((lnot b) land d)
                            val k = g0uint2uint(0x5A827999U)
                          } 
                      | i < 40 => @(f,k) where {
                            val f = (b lxor c lxor d)
                            val k = g0uint2uint(0x6ED9EBA1U)
                         }
                      | i < 60 => @(f,k) where {
                            val f = (b land c) lor (b land d) lor (c land d)
                            val k = g0uint2uint(0x8F1BBCDCU)
                        }
                      | _ => @(f,k) where {
                            val f = (b lxor c lxor d)
                            val k = g0uint2uint(0xCA62C1D6U)
                       }
                    ) : @(uint32,uint32)
                  val tmp = rotl(a,5) + f + e + k + w

               in env := @{
                  h0 = tmp
                , h1 = a
                , h2 = rotl(b,30)
                , h3 = c
                , h4 = d
                }
              end 
          }

          val () = values0 := @{
            h0 = state.h0 + values0.h0
          , h1 = state.h1 + values0.h1
          , h2 = state.h2 + values0.h2
          , h3 = state.h3 + values0.h3
          , h4 = state.h4 + values0.h4
          }

        in if padding < 5
           then loop( pfm | buf, ubuf, msg, i1 + padding, n, values0)
           else ()
       end 
      

        val () = loop( view@msg | buf, ubuf, addr@msg, i2sz(0), len, values0) 

        prval () = b0ytes2bytes( output )
        (** TODO: write result to buf **)
     in 
    end
