
#include "./../HATS/project.hats"
staload "./../SATS/base64.sats"

macdef b64_table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
macdef b64_padding = '='

implement {}
base64_encode{n,m}{l}( pf0 | buf, pout, sz ) 
  = let
      fun loop{i,m,m1,n:nat | i <= n; m >= (4*n/3 + 3); m1 == i*4/3}{l:addr}(
          pf1: array_v(char,l,m1)
        , pf2: array_v(char?,l + m1*sizeof(char),m - m1) 
        | buf: &bytes(n), i : size_t i, pout: ptr l, sz: size_t n 
       ) : #[m0:nat | m0 <= m] (
          array_v(char,l,m0)
        , array_v(char?,l + m0*sizeof(char),m - m0) 
        | size_t m0 
        ) = if i < sz
            then
             let
               val b64_table = b64_table
               val b0 = byte2uint0( array_get_at<byte>( buf, i ) )
               val b1 = if i + i2sz(1) < sz then byte2uint0( array_get_at<byte>( buf, i + i2sz(1) ) ) else 0U
               val b2 = if i + i2sz(2) < sz then byte2uint0( array_get_at<byte>( buf, i + i2sz(2) ) ) else 0U
               
               val hv = (b0 << 16) lor (b1 << 8) lor b2

               (* using mod instead of land because land is
                  only defined for uint0, not uint1 *)

               val h0 = g1ofg0(hv >> 18) mod 0x40U
               val h1 = g1ofg0(hv >> 12) mod 0x40U
               val h2 = g1ofg0(hv >> 6) mod 0x40U
               val h3 = g1ofg0(hv) mod 0x40U

               var c = @[char][4](
                  b64_table[h0]
                , b64_table[h1]
                , if i + i2sz(1) < sz then b64_table[h2] else '='
                , if i + i2sz(2) < sz then b64_table[h3] else '='
                )
            
               val m1 = (i*i2sz(4)/i2sz(3) : size_t m1)
               prval (pf3,pf2a) = array_v_split_at( pf2 | i2sz(4) )

               val pout1 = ptr_add<char>( pout, m1 )
               val pout2 = ptr_add<char>( pout, m1 + i2sz(4))
              
               val () = array_copy<char>( !pout1, c, i2sz(4))
 
               prval pf1a = array_v_unsplit( pf1, pf3 )

              in if i + i2sz(3) < sz
                 then loop{i + 3,m,m1+4,n}{l}(pf1a, pf2a | buf,i + i2sz(3), pout, sz)
                 else (pf1a, pf2a | (i + i2sz(3))*i2sz(4)/i2sz(3))
             end 
            else (pf1, pf2 | i*i2sz(4)/i2sz(3))

     in loop{0,m,0,n}( array_v_nil(), pf0 | buf, i2sz(0), pout, sz )
    end
