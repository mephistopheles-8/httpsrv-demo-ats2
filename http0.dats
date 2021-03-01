
#include "share/atspre_staload.hats"

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

datatype Method =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch

fn {} is_uri_char( c : char ) : bool =
    isalpha(c) || isdigit(c) || 
    strchr("-._~:/?#[]@!$&'()*+,;=", c) > ~1

fn {} is_ows( c : char ) : bool = c = ' ' || c = '\t'

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


extern
fun {env:vt@ype+}  
http_req$route( 
    method: Method
  , uri: !Strptr1 
  , version: @(int,int)
  , env: &env >> _ 
) : void  

extern
fun {env:vt@ype+} 
http_req$header( key: !Strptr1 , value: !Strptr1, env: &env >> _ ) : void  

vtypedef http_parse_env(BUFLEN:int,uenv:vt@ype+,buf:addr) = @{
    buf = arrayptr(char?,buf,BUFLEN)
  , i = sizeLte(BUFLEN)
  , line = size_t
  , colon = intBtwe(~1,BUFLEN)
  , first_non_whitespace = intBtwe(~1,BUFLEN-2)
  , last_non_whitespace = intBtwe(~1,BUFLEN-2)
  , method = Method
  , version_major = int
  , version_minor = int
  , finished = bool
  , err = bool
  , uenv = uenv
  , buflen = size_t BUFLEN
  }

extern
fun {uenv:vt@ype+}
http_parse_env_init{len:nat | len > 2}{buf:addr}( 
    buf: arrayptr(char?,buf,len)
  , buflen: size_t len
  , env: uenv 
) : http_parse_env(len,uenv,buf)

extern
fun {uenv:vt@ype+}
http_parse_env_destroy{len:nat}{buf:addr}( env: http_parse_env(len,uenv,buf) ) 
  : @( arrayptr(char?,buf,len), uenv )

implement {uenv}
http_parse_env_init( buf, buflen, uenv ) = @{
    buf = buf
  , i = i2sz(0)
  , line = i2sz(0)
  , colon = ~1 
  , first_non_whitespace = ~1 
  , last_non_whitespace = ~1
  , method = Get()
  , version_major = 0
  , version_minor = 0
  , finished = false
  , err = false
  , uenv = uenv 
  , buflen = buflen
  }

implement {uenv}
http_parse_env_destroy( env ) = @( env.buf, env.uenv )

extern
fun {env:vt@ype+} 
http_req_parse{n:nat}{buflen:nat}{buf: addr}( 
    buf: &bytes(n)
  , sz: size_t n
  , env: &http_parse_env(buflen,env,buf) 
) : sizeLte(n) 


implement {uenv}
http_req_parse{n}{buflen}{buf}( req, sz, env0 ) = sz0 where {
 
  val sz0 = array_foreach_env<byte><http_parse_env(buflen,uenv,buf)>( req, sz, env0 ) where {
        implement
        array_foreach$cont<byte><http_parse_env(buflen,uenv,buf)>( b, env0 ) =
          ( env0.i != env0.buflen && ~env0.finished && ~env0.err )

        implement
        array_foreach$fwork<byte><http_parse_env(buflen,uenv,buf)>( b, env0 ) = (
          if i < buflen
          then (
            ifcase
            | c = '\n' => ( 
                 let
                     val len = i
                     prval [len:int] EQINT() = g1uint_get_index(i)
                     val line = env0.line
                     val colon = env0.colon
                     val first_non_whitespace = env0.first_non_whitespace
                     val last_non_whitespace = env0.last_non_whitespace
                     val () = ( 
                        arrayptr_set_at<char?>( env0.buf, i, '\0' );
                        env0.i := i2sz(0);
                        env0.line := env0.line + 1;
                        env0.colon := ~1;
                        env0.first_non_whitespace := ~1;
                        env0.last_non_whitespace := ~1;
                      )
                  in if line = 0
                     then {
                        datatype first_line_mode =
                           | failure 
                           | method
                           | method_p
                           | method_commit
                           | uri
                           | http
                           | version_minor
                           | version_major

                        typedef first_line_env = @{
                          state = first_line_mode
                        , expect = string
                        , success = bool
                        , method = Method
                        , version_major = int
                        , version_minor = int
                        , uri_begin = size_t
                        , uri_end = size_t
                        , i = size_t
                        }
                        var env : first_line_env = @{
                            state = method()
                          , expect = ""
                          , success = false
                          , method = Get()
                          , version_major = 0
                          , version_minor = 0
                          , uri_begin = i2sz(0) 
                          , uri_end = i2sz(0)
                          , i = i2sz(0)
                          }

                        implement
                        string_foreach$cont<first_line_env>(c,x) = (
                          case+ x.state of
                           | failure() => false
                           | _ => true
                        ) 
                        implement
                        string_foreach$fwork<first_line_env>(c,x) = {
                          val c0 = tolower(c) 
                          val () = (
                            case+ x.state of
                             | method() => ( 
                                  case+ c0 of 
                                   | 'p' => x.state := method_p()
                                   | 'g' => (
                                      x.state := method_commit(); 
                                      x.expect := "et"; 
                                      x.method := Get()
                                    )
                                   | 'h' => (
                                      x.state := method_commit(); 
                                      x.expect := "ead"; 
                                      x.method := Head()
                                    ) 
                                   | 'd' => (
                                      x.state := method_commit(); 
                                      x.expect := "elete"; 
                                      x.method := Delete()
                                    )
                                   | 'c' => (
                                      x.state := method_commit(); 
                                      x.expect := "onnect"; 
                                      x.method := Connect()
                                    )
                                   | 'o' => (
                                      x.state := method_commit(); 
                                      x.expect := "ptions"; 
                                      x.method := Options()
                                    ) 
                                   | 't' => (
                                      x.state := method_commit(); 
                                      x.expect := "race"; 
                                      x.method := Trace()
                                    )
                                   | _ => x.state := failure()
                                  )
                             | method_p() => (
                                  case+ c0 of
                                  | 'o' => (
                                      x.state := method_commit(); 
                                      x.expect := "st"; 
                                      x.method := Post()
                                    ) 
                                  | 'u' => (
                                      x.state := method_commit(); 
                                      x.expect := "t"; 
                                      x.method := Put()
                                    ) 
                                  | 'a' => (
                                      x.state := method_commit(); 
                                      x.expect := "atch"; 
                                      x.method := Patch()
                                  ) 
                                 | _ => x.state := failure()
                              )
                             | method_commit() =>
                                  let
                                     val s1 = g1ofg0(x.expect)
                                   in if string_isnot_empty(s1)
                                      then 
                                        if s1.head() = c0
                                        then x.expect := s1.tail()
                                        else x.state := failure()
                                      else 
                                        if c0 = ' '
                                        then (
                                          x.state := uri();
                                          x.uri_begin := x.i + i2sz(1);
                                          x.uri_end := x.i + i2sz(1);
                                        )
                                        else x.state := failure()
                                  end
                             | uri() => (
                                ifcase
                                 | is_uri_char(c) => () 
                                 | c = ' ' => (
                                      x.uri_end := x.i;
                                      x.state := http();
                                      x.expect := "http"
                                  )
                                 | _ => x.state := failure()  
                               )
                             | http() =>  
                                let
                                   val s1 = g1ofg0(x.expect)
                                 in if string_isnot_empty(s1)
                                    then 
                                      if s1.head() = c0
                                      then x.expect := s1.tail()
                                      else x.state := failure()
                                    else 
                                      if c0 = '/'
                                      then x.state := version_major()
                                      else x.state := failure()
                                end
                             | version_major() => (
                                  ifcase
                                   | c = '1' || c = '2' => (
                                        x.version_major := (if c = '1' then 1 else 2)
                                     ) 
                                   | c = '.' => x.state := version_minor()
                                   | _ => x.state := failure()
                                )
                             | version_minor() => (
                                  ifcase
                                   | c = '0' || c = '1' => (
                                        x.version_minor := (if c = '0' then 0 else 1);
                                        x.success := true;
                                     ) 
                                   | _ => x.state := failure()
                               )
                             | failure() => ()
                           )
                          val () = x.i := x.i + i2sz(1)     
                        }
                        val xs = string_foreach_env<first_line_env>( $UNSAFE.castvwtp1{string len}(env0.buf), env )
                        val e1 = g1ofg0( env.uri_end )
                        val () = assertloc( e1 < buflen )
                        val () = arrayptr_set_at<char?>( env0.buf, e1, '\0' );
                        val uriptr = $UNSAFE.castvwtp1{Strptr1}(ptr_add<char>(ptrcast(env0.buf), env.uri_begin));
                        val () =
                          if env.success && xs = len 
                          then http_req$route<uenv>(env.method, uriptr, @(env.version_major, env.version_minor), env0.uenv)
                          else env0.err := true
                        prval () = $UNSAFE.cast2void(uriptr)
                      }
                     else ( 
                       if colon >= 0 
                       then if colon < sz2i(i)
                            then (
                              arrayptr_set_at<char?>( env0.buf, colon, '\0' );
                              if last_non_whitespace >= 0 
                              then arrayptr_set_at<char?>( env0.buf, last_non_whitespace + 1, '\0' );
                              
                              let
                                 val p = (
                                  if first_non_whitespace > ~1 
                                  then ptr_add<char>(ptrcast(env0.buf), first_non_whitespace)
                                  else ptrcast("")
                                 )
                                 val kptr = $UNSAFE.castvwtp1{Strptr1}(env0.buf)
                                 val vptr = $UNSAFE.castvwtp1{Strptr1}(p)

                                 val () = 
                                  http_req$header<uenv>( kptr, vptr, env0.uenv )
                                 prval () = $UNSAFE.cast2void(kptr)
                                 prval () = $UNSAFE.cast2void(vptr)
                               in
                              end
                            ) where {
                                // This was necessary because the `&&` macro obliterates
                                // constraint solving. Refactored to not need the assertion,
                                // but left the note here
                                // prval () = $UNSAFE.prop_assert{colon >= 0 && colon < BUFLEN}()
                             }
                            else ()
                       else if first_non_whitespace = ~1
                            then (
                              env0.finished := true;
                            )
                            else (
                              env0.err := true;
                            ) 
                     )
                 end
            )
            | c = ':' && env0.colon = ~1 => (
               let
                   val () = ( 
                      arrayptr_set_at<char?>( env0.buf, i, c );
                      env0.colon := sz2i(i);
                      env0.i := i + 1;
                    )
                in ()
               end
            ) 
            | _ => ( 
               let
                   val () = (
                      if env0.colon != ~1 && ~is_ows(c)
                      then (
                        if env0.first_non_whitespace = ~1 then env0.first_non_whitespace := sz2i(i);
                        env0.last_non_whitespace := sz2i(i); 
                      );
                      arrayptr_set_at<char?>( env0.buf, i, if env0.colon = ~1 && env0.line > 0 then tolower(c) else c );
                      env0.i := i + 1;
                    )
                in ()
               end
            )
          )
        ) where {
          val i = env0.i
          val c = $UNSAFE.cast{char}(b)
          val buflen = env0.buflen
        }
    }
}

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

