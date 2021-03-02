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

datatype Route =
  | Err404
  | Err400
  | GetIndex
  | GetAbout
 
vtypedef client_state = @{
    status = conn_status
  , bytes_read = size_t
  , reqs_served = int
  , route = Route
  }

implement 
sockenv$free<client_state>( x ) = ()

macdef SOMAXCONN = $extval(intGt(0), "SOMAXCONN")

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
          extern praxi socket_is_listening{fd:int}{st:status}( !sockfd(fd,st) >> sockfd(fd,listen) ) : void
          prval () = socket_is_listening( sock )

          val ()   = sockfd_accept_all<evloop(client_state)>(sock, pool)
          prval () = fold@env
       in
      end
  | Read() =>
      let
        var linebuf = @[char][BUFSZ]()
        var buf = @[byte][BUFSZ](i2byte(0))

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
                        ifcase
                         | uri = "/" => (env := GetIndex()) 
                         | uri = "/index" => (env := GetIndex()) 
                         | uri = "/about" => (env := GetAbout())
                         | _ => (env := Err404())
                      ) 
                      implement {e0}
                      http_req$header( k, v, env ) = ( 
                       (* println!("k: ", k, " | v: ", v ); *)
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

         prval () = socket_is_conn( sock ) where {
            extern praxi socket_is_conn{fd:int}{st:status}( !sockfd(fd,st) >> sockfd(fd,conn) ) : void
         }
         val b = loop(sock, buf, i2sz(BUFSZ), env0 ) 

        val @(b0,route) = http_parse_env_destroy<Route>( env0 )

        val () = info.route := route 

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
        extern praxi socket_is_conn{fd:int}{st:status}( !sockfd(fd,st) >> sockfd(fd,conn) ) : void
        prval () = socket_is_conn( sock )

        val @(str,n) : [n:nat] @(string n,size_t n) = (
            case+ info.route of
            | GetIndex() => @("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 6\r\n\r\nHello!", i2sz(70) )
            | GetAbout() => @("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 32\r\n\r\nThis is the story about some guy", i2sz(97) )
            | Err400() => @("HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\nContent-Length: 11\r\n\r\nBad Request", i2sz(85) )
            | _ => @("HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found", i2sz(80) )
          ) : [n:nat] @(string n,size_t n)

        val ssz = sockfd_write_string( sock, str, n )
        val () = info.status := Read()
        prval () = fold@env

        val () = assertloc( evloop_events_mod( pool, EvtR(), env) )
       in  
      end

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
