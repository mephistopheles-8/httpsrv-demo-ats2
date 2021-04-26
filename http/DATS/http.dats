
#include "./../HATS/project.hats"
#include "share/atspre_staload.hats"

staload "./../SATS/http.sats"

fn {} is_uri_char( c : char ) : bool =
    isalpha(c) || isdigit(c) || 
    strchr("-._~:/?#[]@!$&'()*+,;=", c) > ~1

fn {} is_ows( c : char ) : bool = c = ' ' || c = '\t'

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
