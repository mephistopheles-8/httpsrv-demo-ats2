
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
   // , method = Method
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
  //, method = Get()
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
                        var env : @(int,string,bool,Method) = @(0,"",false,Get())
                        implement
                        string_foreach$cont<(@(int,string,bool,Method))>(c,x) = x.0 != ~1
                        implement
                        string_foreach$fwork<(@(int,string,bool,Method))>(c,x) = (
                          if x.1 = "" && x.0 != 2 
                          then (  
                            ifcase
                             | c0 = 'g' => (x.0 := 2; x.1 := "et"; x.3 := Get())
                             | c0 = 'h' => (x.0 := 2; x.1 := "ead"; x.3 := Head())
                             | c0 = 'p' => x.0 := 1
                             | c0 = 'd' => (x.0 := 2; x.1 := "elete"; x.3 := Delete())
                             | c0 = 'c' => (x.0 := 2; x.1 := "onnect"; x.3 := Connect())
                             | c0 = 'o' && x.0 = 0 => (x.0 := 2; x.1 := "ptions"; x.3 := Options())
                             | c0 = 'o' && x.0 = 1 => (x.0 := 2; x.1 := "st"; x.3 := Post())
                             | c0 = 't' => (x.0 := 2; x.1 := "race"; x.3 := Trace())
                             | c0 = 'u' && x.0 = 1 => (x.0 := 2; x.1 := "t"; x.3 := Put())
                             | c0 = 'a' && x.0 = 1 => (x.0 := 2; x.1 := "tch"; x.3 := Patch())
                             | _ => x.0 := ~1
                            )
                          else 
                            let
                               val s1 = g1ofg0(x.1)
                             in if string_isnot_empty(s1)
                                then if s1.head() = c0
                                     then x.1 := s1.tail()
                                     else x.0 := ~1
                                else 
                                  if isspace(c0)
                                  then x.2 := true
                                  else x.0 := ~1
                            end
                          ) where {
                            val c0 = tolower(c)
                          }
 
                        val _ = string_foreach_env<(@(int,string,bool,Method))>( $UNSAFE.castvwtp1{string len}(env0.buf), env )
                        val () =
                          if env.2 then 
                            case env.3 of
                            | Get() => println!("Get")
                            | Post() => println!("Post")
                            | Put() => println!("Put")
                            | Patch() => println!("Patch")
                            | Delete() => println!("Delete")
                            | Connect() => println!("Connect")
                            | Options() => println!("Options")
                            | Head() => println!("Head")
                            | Trace() => println!("Trace")
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

  prval () = view@buf := pf
 
}
