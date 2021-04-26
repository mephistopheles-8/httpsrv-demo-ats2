%{#
#include <pthread.h>
%}
#define ASYNCNET_EPOLL
#include "share/atspre_staload.hats"

#define M88_targetloc "./../"
#include "{$M88}/evloop/mylibies.hats"
#include "{$M88}/http/mylibies.hats"
#include "{$M88}/atshtml/mylibies.hats"

staload "libats/libc/SATS/sys/socket.sats"
staload "libats/libc/SATS/sys/socket_in.sats"
staload "libats/libc/SATS/netinet/in.sats"
staload "libats/libc/SATS/errno.sats"
staload "libats/libc/SATS/fcntl.sats"
staload "libats/libc/SATS/unistd.sats"
staload "libats/SATS/athread.sats"
staload _ = "libats/DATS/athread.dats"

#define BUFSZ 1024

absreimpl evloop_params

datatype conn_status = 
  | Listen
  | Read
  | Write
  | Ws

datavtype Route =
  | Err404
  | Err400
  | GetIndex
  | GetAbout
  | GetWs of ( uint, Strptr0 )

fn route_free( r: Route ) : void =
  case+ r of
  | ~Err404 () => ()
  | ~Err400 () => ()
  | ~GetIndex () => ()
  | ~GetAbout () => ()
  | ~GetWs (_, sec_ws_acc_opt) => (
      free( sec_ws_acc_opt )
  )

vtypedef client_state = @{
    status = conn_status
  , bytes_read = size_t
  , reqs_served = int
  , route = Route
  }

implement 
sockenv$free<client_state>( x ) = route_free( x.route )

macdef SOMAXCONN = $extval(intGt(0), "SOMAXCONN")


stacst page_title : int
stacst utf8 : int
stacst en : int
stacst hello_world : int
stacst ws_test : int
stacst viewport : int
stacst default_viewport : int

stadef document 
  = doctype'
  :*: html'(lang$(en) :@: anil
        , head'(anil,
              meta'(charset$(utf8) :@: anil)
          :*: meta'(name$(viewport) :@: content$(default_viewport) :@: anil) 
          :*: title'(page_title)
          :*: enil
        ) 
      :*: body'(anil,
             p'(anil, text'(hello_world) :*: enil)
         :*: script'(anil,ws_test)
         :*: enil
        )
      :*: enil
  ) :*: enil

#define s2m string2mixed

implement (env)
html5$attr<en><env>( x ) 
  = s2m("en")
implement (env)
html5$attr<utf8><env>( x )
  = s2m("utf-8")
implement (env)
html5$attr<viewport><env>( x )
  = s2m("viewport")
implement (env)
html5$attr<default_viewport><env>( x )  
  = s2m("width=device-width, initial-scale=1.0")
implement (env)
html5$script<ws_test><env>( x )      = s2m("
  (function () {
    var ws = new WebSocket(\"ws://localhost:8888/ws\");
    ws.addEventListener( \"open\", () => {
      setInterval( () => {
        ws.send(\"Hello!\");
        console.log(\"fire\");
      }, 1000);
    });
   ws.addEventListener(\"message\", (e) => {
      console.log(e.data);
   });
  })()
")

vtypedef writer(l:addr) = @{
  buf = arrayptr(byte,l,BUFSZ)
, i = sizeLte(BUFSZ)
}

implement (l:addr)
html5$out<char><writer(l)>( x, c ) = (
  if i < i2sz(BUFSZ)
  then ( 
    arrayptr_set_at<byte>( x.buf, i, $UNSAFE.cast{byte}(c) );
    x.i := i + i2sz(1)
  )
  else ()
) where {
  val i = x.i
}

implement (l:addr)
html5$out<string><writer(l)>( x, s ) = {
  val _ = string_foreach_env<writer(l)>( g1ofg0(s), x ) where {
    implement
    string_foreach$cont<writer(l)>(c,x) = x.i < i2sz(BUFSZ)
    implement
    string_foreach$fwork<writer(l)>(c,x) = html5$out<char><writer(l)>(x,c)
  } 
}

implement (l:addr)
html5$out<strmixed1><writer(l)>( x, sm ) = {
  val _ = strmixed_foreach<writer(l)>( sm, x ) where {
    implement
    strmixed_foreach$cont<writer(l)>(c,x) = x.i < i2sz(BUFSZ)
    implement
    strmixed_foreach$fwork<writer(l)>(c,x) = html5$out<char><writer(l)>(x,c)
  } 
}


extern praxi socket_is_conn{fd:int}{st:status}( !sockfd(fd,st) >> sockfd(fd,conn) ) : void
extern praxi socket_is_listening{fd:int}{st:status}( !sockfd(fd,st) >> sockfd(fd,listen) ) : void

implement
evloop$process<client_state>( pool, evts, env ) = (
  case+ info.status of
  | Listen() =>
      let
          implement
          sockfd_accept_all$withfd<evloop(client_state)>(cfd,pool) = (
            if evloop_events_add{client_state}( pool, EvtR(), senv )
            then true where {
                prval () = opt_unnone( senv )
              }
            else false where {
              prval () = opt_unsome( senv )
              val @(cfd,_) = sockenv_decompose<client_state>( senv )
              val () = sockfd_close_ign( cfd )
            }
          ) where {
            var cfd = cfd
            var cinfo : client_state = @{
                  status = Read()
                , bytes_read = i2sz(0)
                , reqs_served = 0
                , route = Err404()
              }
            var senv = sockenv_create<client_state>( cfd, cinfo )
          }
          prval () = socket_is_listening( sock )

          val ()   = sockfd_accept_all<evloop(client_state)>(sock, pool)
          prval () = fold@env
       in
      end
  | Read() =>
      let
        var linebuf = @[char][BUFSZ]()
        var buf = @[byte][BUFSZ]()

        prval () = b0ytes2bytes( buf )

        val ( pf | ap )
          = arrayptr_objectify( view@linebuf | addr@linebuf )

        var env0 : http_parse_env( BUFSZ, Route, linebuf )
          = http_parse_env_init<Route>( ap, i2sz(BUFSZ), Err404() )
        
        datatype l_action =
          | keep_open
          | close_sock
          | arm_write

        fun loop{fd:int}{n:nat}(
               sock : &sockfd(fd,conn)
             , buf: &array(byte,n)
             , sz : size_t n
             , env0 : &http_parse_env(BUFSZ,Route,linebuf)
        ) : l_action 
         = let
              val ssz = sockfd_read(sock,buf,sz) 
            in if  ssz > 0 then 
                let
                  prval (pf1,pf2) = array_v_split_at( view@buf | g1int2uint(ssz) )

                  val _ = http_req_parse<Route>( buf, g1int2uint(ssz), env0 )
                    where {
                      implement
                      http_req$route<Route>( method, uri, version, env ) = (
                        route_free( env );
                        ifcase
                         | uri = "/" => (env := GetIndex()) 
                         | uri = "/index" => (env := GetIndex()) 
                         | uri = "/about" => (env := GetAbout())
                         | uri = "/ws" => (env := GetWs(0U, strptr_null()))
                         | _ => (env := Err404())
                      ) 
                      implement
                      http_req$header<Route>( k, v, env ) = (
                        case+ env of
                        | @GetWs(s,sec_ws_key_opt) => (
                            ifcase
                            | k = "upgrade" => {
                                val () = s := (s lor 1U)
                                prval () = fold@env
                              } 
                            | k = "connection" => {
                                val () = s := (s lor 2U)
                                prval () = fold@env
                              } 
                            | k = "sec-websocket-key" => {
                                val () = s := (s lor 4U)
                                (** FIXME: could probably do verification here **)
                                val () = free( sec_ws_key_opt )
                                val () = sec_ws_key_opt := copy(v)
                                prval () = fold@env
                              } 
                            | k = "sec-websocket-version" => {
                                val () = s := (s lor 8U)
                                prval () = fold@env
                              }
                            | _ => {
                                prval () = fold@env
                            }
                          )
                       | _ => ()  
                      )
                    }

                  prval () = view@buf := array_v_unsplit( pf1, pf2 )
                in ifcase
                   | env0.finished && ~env0.err => arm_write()
                   | env0.err  => close_sock()
                   | _         => loop(sock,buf,sz,env0)
                end
              else  ifcase
                    | ssz = 0 => close_sock() 
                      (* I think we need to block here until 
                        we store the HTTP parser state somewhere *)
                    | the_errno_test(EAGAIN) => loop(sock,buf,sz,env0)
                      (*  keep_open() *) 
                    | _ => close_sock() 
           end

         prval () = socket_is_conn( sock )
         
         val b = loop(sock, buf, i2sz(BUFSZ), env0 ) 

        val @(b0,route) = http_parse_env_destroy<Route>( env0 )

        val () = (route_free(info.route); info.route := route) 

        val ( pf | p )
          = arrayptr_unobjectify( pf | b0 )

        prval () = view@linebuf := pf

      in case+ b of
         | arm_write() => {
              val () = info.status := Write()
              prval () = fold@env 
              val () = assertloc( evloop_events_mod{client_state}( pool, EvtW(), env) )
           }
         | close_sock() => {
              prval () = fold@env 
              val () = assertloc( evloop_events_dispose{client_state}( pool, env ) )
          }
         | keep_open() => {
              prval () = fold@env 
          } 
      end
  | Write() =>
      let
        fun html_headers_out{fd:int}( sock: !sockfd(fd,conn), status: string, len: int  )
          : void
          = let
              #define BSZ 256
              var buf = @[char][BSZ]()
              typedef
              cstring = $extype"atstype_string"
              val bufp = $UNSAFE.cast{cstring}(addr@buf)
              val n(*int*) =
                $extfcall(ssize_t, "snprintf", bufp, BSZ
                  , "HTTP/1.1 %s\r\nContent-Type:text/html\r\nContent-Length: %d\r\n\r\n"
                  , status, len
                )
              val n = g1ofg0(n)
              val () = assertloc( n > ~1 )

              prval [sz:int] EQINT() = eqint_make_gint( n ) 

              val _ = sockfd_write_string( sock, $UNSAFE.cast{string sz}(bufp) , g1int2uint(n) )
            in () 
           end
        
        fun ws_headers_out{fd:int}( sock: !sockfd(fd,conn), resp: &array(char,29)  )
          : void
          = let
              #define BSZ 256
              var buf = @[char][BSZ]()
              typedef
              cstring = $extype"atstype_string"
              val bufp = $UNSAFE.cast{cstring}(addr@buf)
              val n(*int*) =
                $extfcall(ssize_t, "snprintf", bufp, BSZ
                  , "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-Websocket-Accept: %s\r\n\r\n"
                  , $UNSAFE.cast{cstring}( addr@resp )
                )
              val n = g1ofg0(n)
              val () = assertloc( n > ~1 )

              prval [sz:int] EQINT() = eqint_make_gint( n ) 

              val _ = sockfd_write_string( sock, $UNSAFE.cast{string sz}(bufp) , g1int2uint(n) )
            in () 
           end


        var outbuf = @[byte][BUFSZ]()
        prval () = b0ytes2bytes( outbuf )
        
        val ( pf | ap )
          = arrayptr_objectify( view@outbuf | addr@outbuf )

        var wenv : writer(outbuf) = @{
          buf = ap
        , i = i2sz(0)
        }

        prval () = socket_is_conn( sock )

        fun response{fd:int}{outbuf:addr}( r0: !Route, sock: !sockfd(fd,conn), wenv: &writer(outbuf)  )
          : void = (
            case+ r0 of
             | GetIndex() =>  
                let
                   val () = html5_elm_list_out<document><writer(outbuf)>( wenv ) where {
                      implement (env)
                      html5$text<page_title><env>( x )  = s2m("Hello world")
                      implement (env)
                      html5$text<hello_world><env>( x ) = s2m("Hello world!")
                    }
                   val _ = html_headers_out( sock, "200 OK", sz2i( wenv.i ) )

                   val p0 = arrayptr2ptr( wenv.buf )
                   prval pfa = arrayptr_takeout( wenv.buf )
                   val ssz = sockfd_write( sock, !p0, wenv.i )
                   prval () = arrayptr_addback( pfa | wenv.buf )
                 in
                end 
             | GetAbout() =>   
                let
                   val () = html5_elm_list_out<document><writer(outbuf)>( wenv ) where {
                      implement (env)
                      html5$text<page_title><env>( x )  = s2m("About")
                      implement (env)
                      html5$text<hello_world><env>( x ) = s2m("This is the story about some guy")
                    }

                   val _ = html_headers_out( sock, "200 OK", sz2i( wenv.i ) )

                   val p0 = arrayptr2ptr( wenv.buf )
                   prval pfa = arrayptr_takeout( wenv.buf )
                   val ssz = sockfd_write( sock, !p0, wenv.i )
                   prval () = arrayptr_addback( pfa | wenv.buf )
                 in
                end 
             | GetWs(b,sec_ws_key_opt) => {
                  (** FIXME: do real validation **)
                  val () = assertloc( strptr_length( sec_ws_key_opt ) >= 24 )
                  val () = assertloc( b = 15U )

                  var output_b64 = @[char][29]()
                  val p = ptrcast(sec_ws_key_opt) 
                  val (pf,pff | p) = $UNSAFE.ptr0_vtake{array(char,24)}( p )

                  val () = ws_handshake_accept( !p, output_b64 )

                  val () = ws_headers_out( sock, output_b64 )

                  prval () = pff( pf )
                }
             | Err400() =>   
                let
                   val () = html5_elm_list_out<document><writer(outbuf)>( wenv ) where {
                      implement (env)
                      html5$text<page_title><env>( x )  = s2m("Bad Request")
                      implement (env)
                      html5$text<hello_world><env>( x ) = s2m("Bad Request")
                    }

                   val _ = html_headers_out( sock, "400 Bad Request", sz2i( wenv.i ) )

                   val p0 = arrayptr2ptr( wenv.buf )
                   prval pfa = arrayptr_takeout( wenv.buf )
                   val ssz = sockfd_write( sock, !p0, wenv.i )
                   prval () = arrayptr_addback( pfa | wenv.buf )
                 in
                end 
             | Err404() =>   
                let
                   val () = html5_elm_list_out<document><writer(outbuf)>( wenv ) where {
                      implement (env)
                      html5$text<page_title><env>( x )  = s2m("Not Found")
                      implement (env)
                      html5$text<hello_world><env>( x ) = s2m("Not Found")
                    }

                   val _ = html_headers_out( sock, "404 Not Found", sz2i( wenv.i ) )
                   
                   val p0 = arrayptr2ptr( wenv.buf )
                   prval pfa = arrayptr_takeout( wenv.buf )
                   val ssz = sockfd_write( sock, !p0, wenv.i )
                   prval () = arrayptr_addback( pfa | wenv.buf )
                 in
                end 
          )  

        val () = response(info.route, sock, wenv)

        val ( pf | p )
          = arrayptr_unobjectify( pf | wenv.buf )
        
        prval () = view@outbuf := pf

        val () = info.status := (
          case+ info.route of
            | GetWs(_,_) => Ws()
            | _ => Read()
        )

        prval () = fold@env

        val () = assertloc( evloop_events_mod( pool, EvtR(), env) )
       in  
      end
  | Ws () => {
        val () = println!("WS activity")
        var buf = @[byte][BUFSZ]()
        var msgbuf = @[byte][BUFSZ]()
        prval () = b0ytes2bytes( msgbuf )

        val (pfap | ap) = arrayptr_objectify( view@msgbuf | addr@msgbuf )

        var penv : ws_frame_parser(msgbuf,BUFSZ) 
          = ws_frame_parser_init( ap, i2sz(BUFSZ) )
        
        fun loop{fd:int}{n:nat}(
               sock : &sockfd(fd,conn)
             , buf: &array(byte,n)
             , sz : size_t n
             , penv: &ws_frame_parser(msgbuf,BUFSZ)
        ) : bool 
         = let
              val ssz = sockfd_read(sock,buf,sz) 
            in if  ssz > 0 then 
                let
                  prval (pf1,pf2) = array_v_split_at( view@buf | g1int2uint(ssz) )

                  val status = ws_frame_parse( buf, g1int2uint( ssz ), penv )

                  prval () = view@buf := array_v_unsplit( pf1, pf2 )

                in case+ status of
                  | ws_success() => true where {
                      val j = penv.j
                      val bsz = penv.bufsz
                      val () = (
                        if j < bsz
                        then begin
                           println!("Success");
                           (case+ penv.opcode of
                            | 0x0U => print!("CONTINUATION: ")
                            | 0x1U => print!("TEXT: ")
                            | 0x2U => print!("BIN: ")
                            | 0x8U => print!("CLOSE: ")
                            | 0x9U => print!("PING: ")
                            | 0xAU => print!("PONG: ")
                            | _ => print!("UNKNOWN: ")
                           );
                           arrayptr_set_at<byte>(penv.buf, j, i2byte(0));
                           println!( $UNSAFE.cast{string}(ptrcast(penv.buf) ) ); 
                          end 
                        else ()
                      )
                    }
                  | ws_continue() => loop(sock,buf,sz,penv)
                  | ws_buffer_full() => (ws_frame_parser_reset_buf(penv); loop(sock,buf,sz,penv))
                end
              else  ifcase
                    | ssz = 0 => false 
                      (* I think we need to block here until 
                        we store the HTTP parser state somewhere *)
                    | the_errno_test(EAGAIN) => true // loop(sock,buf,sz)
                      (*  keep_open() *) 
                    | _ => false
           end

        prval () = b0ytes2bytes( buf )
        prval () = socket_is_conn( sock )
        val b0 = loop( sock, buf, i2sz(BUFSZ), penv )

        val ap = ws_frame_parser_destroy( penv )

        val (pfmb | p) = arrayptr_unobjectify( pfap | ap )
        prval () = view@msgbuf := pfmb

        val () = 
          if b0
          then {
            var resp = @[byte][8](
                i2byte(0x81)
              , i2byte(0x06)
              , $UNSAFE.cast{byte}('H') 
              , $UNSAFE.cast{byte}('e') 
              , $UNSAFE.cast{byte}('l') 
              , $UNSAFE.cast{byte}('l') 
              , $UNSAFE.cast{byte}('o') 
              , $UNSAFE.cast{byte}('!') 
              )
            val ssz = sockfd_write( sock, resp, i2sz(8) )

            prval () = fold@env
          
            val () = assertloc( evloop_events_mod( pool, EvtR(), env) )
          }
          else {
            prval () = fold@env
            val () = assertloc( evloop_events_dispose{client_state}( pool, env ) )
          }
        
    }
) where {
  val @CLIENT(sock,data,info) = env
}

implement main0 () = println!("Hello [test01]")
  where {
    var lfd : sockfd0?

    var lsock_params : sockfd_setup_params = (@{
        af = AF_INET
      , st = SOCK_STREAM 
      , nonblocking = true // handled by evloop
      , reuseaddr = true
      , nodelay = true
      , cloexec = false
      , port = 8888
      , address = in_addr_hbo2nbo (INADDR_ANY)
      , backlog = SOMAXCONN
    })

    val () =
      if sockfd_setup( lfd, lsock_params )
      then 
       let
          prval () = sockopt_unsome( lfd )
          val () = println!("Listening at port ", lsock_params.port )
          fun spawn_threads{fd:int}( lfd0: sockfd(fd,listen), i: intGte(0) )
            : void =
             if i > 0 
             then 
              let
                  extern
                  fn dup{fd:int}{st:status}( sfd: !sockfd(fd,st) )
                      : [fd0:int | fd >= ~1] (option_v(socket_v(fd0,st),fd0 > ~1) | int fd0) = "mac#dup"

                  val (pf | fd0) = dup( lfd0 )
                  val () = assertloc( fd0 > ~1 )
                  prval Some_v(pf) = pf
                  val lfd = sockfd_encode( pf | fd0)
 
                  val _ = athread_create_cloptr_exn(llam() =>
                     let
                         var p : evloop(client_state)?
                         var evloop_params : evloop_params = (@{
                             maxevents = i2sz(256)
                           } : evloop_params)
                      in if evloop_create<client_state>( p, evloop_params ) 
                          then
                           let
                             prval () = opt_unsome( p )
                             var linfo : client_state = @{
                                   status = Listen()
                                 , bytes_read = i2sz(0)
                                 , reqs_served = 0
                                 , route = Err404()
                               }

                             var senv = sockenv_create<client_state>( lfd, linfo )
                             
                             val () = assertloc( evloop_events_add{client_state}( p,  EvtR(), senv) )
                             
                             prval () = opt_unnone( senv )
                             
                             var x : int = 0
                             val () = evloop_run<int><client_state>(p, x)

                           in 
                             evloop_close_exn<client_state>( p ); 
                           end
                         else 
                           let
                             prval () = opt_unnone( p ) 
                           in
                             println!("Failed to create TCP pool");
                             sockfd_close_exn( lfd )  
                           end
                     end
                  )
               in spawn_threads(lfd0,i-1)
              end
             else sockfd_close_exn( lfd0 ) // is this valid?

          (** Fixme: just lock here **) 
        in spawn_threads( lfd, 4 );
           while ( true ) ( ignoret(sleep(100)) )
       end
      else println!("Failed to creatte listening socket") where {
          prval () = sockopt_unnone( lfd ) 
        } 

  }
