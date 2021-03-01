
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

fn is_uri_char( c : char ) : bool =
    isalpha(c) || isdigit(c) || 
    strchr("-._~:/?#[]@!$&'()*+,;=", c) > ~1

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

  vtypedef env = @{
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
    }

  var env0 : env = @{
    buf = ap
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
  }
 
  val _ = string_foreach_env<env>( headers, env0 ) where {
        implement
        string_foreach$cont<env>( c, env0 ) =
          ( env0.i != i2sz(BUFLEN) && ~env0.finished && ~env0.err )

        implement
        string_foreach$fwork<env>( c, env0 ) = (
          if i < i2sz(BUFLEN)
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
                        val () = assertloc( e1 < i2sz(BUFLEN) )
                        val () = arrayptr_set_at<char?>( env0.buf, e1, '\0' );
                        val () =
                          if env.success && xs = len 
                          then (
                            method_print(stdout_ref,env.method);
                            print_newline();
                            println!("URI: ", $UNSAFE.cast{string}(ptr_add<char>(ptrcast(env0.buf), env.uri_begin)) );
                            println!("HTTP/",env.version_major,".",env.version_minor);
                          )
                          else env0.err := true
                      }
                     else ( 
                       if colon >= 0 
                       then if colon < sz2i(i)
                            then (
                              arrayptr_set_at<char?>( env0.buf, colon, '\0' );
                              if last_non_whitespace >= 0 
                              then arrayptr_set_at<char?>( env0.buf, last_non_whitespace + 1, '\0' );
                              println!(line, ": "
                                  , $UNSAFE.castvwtp1{string}(env0.buf), " ||| "
                                  , (if first_non_whitespace > ~1 
                                    then $UNSAFE.cast{string}(ptr_add<char>(ptrcast(env0.buf), first_non_whitespace))
                                    else "") : string 
                                );
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
                              println!("OK!");
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
                in ()//println!(c)
               end
            ) 
            | _ => ( 
               let
                   val () = (
                      if env0.colon != ~1 && ~isspace(c)
                      then (
                        if env0.first_non_whitespace = ~1 then env0.first_non_whitespace := sz2i(i);
                        env0.last_non_whitespace := sz2i(i); 
                      );
                      arrayptr_set_at<char?>( env0.buf, i, if env0.colon = ~1 && env0.line > 0 then tolower(c) else c );
                      env0.i := i + 1;
                    )
                in ()//println!(c)
               end
            )
          )
        ) where {
          val i = env0.i
        }
    }
  
  val ( pf | p )
    = arrayptr_unobjectify( pf | env0.buf )

  (** This could be a bug **)
  prval () = $UNSAFE.cast2void( env0 ) 
  prval () = view@buf := pf
 
}
