
#include "share/atspre_staload.hats"
#include "./../mylibies.hats"

val headers=
"GET / HTTP/1.1\r
Host: mbellaire.info\r
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:83.0) Gecko/20100101 Firefox/83.0\r
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8\r
Accept-Language: en-US,en;q=0.5\r
Accept-Encoding: gzip, deflate, br\r
DNT: 1\r
Connection: keep-alive\r
Upgrade-Insecure-Requests: 1\r
\r
"

fn method_print( fp: FILEref, m : Method ) : void =
  case m of
  | Get() => fprint!(fp,"Get")
  | Post() => fprint!(fp,"Post")
  | Put() => fprint!(fp,"Put")
  | Patch() => fprint!(fp,"Patch")
  | Delete() => fprint!(fp,"Delete")
  | Connect() => fprint!(fp,"Connect")
  | Options() => fprint!(fp,"Options")
  | Head() => fprint!(fp,"Head")
  | Trace() => fprint!(fp,"Trace")

implement main0 () = {
  
  #define BUFLEN 8192
  var buf = @[char][BUFLEN]()

  val ( pf | ap )
    = arrayptr_objectify( view@buf | addr@buf )

  var env0 : http_parse_env( BUFLEN, int, buf )
    = http_parse_env_init<int>( ap, i2sz(BUFLEN), 0 ) 

  stadef hlen = 337
  val hlen : size_t hlen = string1_length( headers ) 
  val hp = ptrcast(headers)
  
  val (pf0,pff0 | p) = $UNSAFE.ptr0_vtake{bytes(hlen)}( hp )

  val sz = http_req_parse<int>( !p, strlen(headers), env0 )
    where {
      implement {e0}
      http_req$route( method, uri, version, env ) = ( 
        method_print(stdout_ref,method);
        print_newline();
        println!("URI: ", uri );
        println!("HTTP/",version.0,".",version.1);
      )
      implement {e0}
      http_req$header( k, v, env ) = ( 
        println!("k: ", k, " | v: ", v );
      )
    }

  prval () = pff0(pf0)

  val () = println!("finished? ", env0.finished)

  val @(b0,x) = http_parse_env_destroy<int>( env0 )

  val ( pf | p )
    = arrayptr_unobjectify( pf | b0 )

  prval () = view@buf := pf
}

