#include "./../HATS/project.hats"

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

fun {uenv:vt@ype+}
http_parse_env_init{len:nat | len > 2}{buf:addr}( 
    buf: arrayptr(char?,buf,len)
  , buflen: size_t len
  , env: uenv 
) : http_parse_env(len,uenv,buf)

fun {uenv:vt@ype+}
http_parse_env_destroy{len:nat}{buf:addr}( env: http_parse_env(len,uenv,buf) ) 
  : @( arrayptr(char?,buf,len), uenv )


fun {env:vt@ype+}  
http_req$route( 
    method: Method
  , uri: !Strptr1 
  , version: @(int,int)
  , env: &env >> _ 
) : void  

fun {env:vt@ype+} 
http_req$header( key: !Strptr1 , value: !Strptr1, env: &env >> _ ) : void  

fun {env:vt@ype+} 
http_req_parse{n:nat}{buflen:nat}{buf: addr}( 
    buf: &bytes(n)
  , sz: size_t n
  , env: &http_parse_env(buflen,env,buf) 
) : sizeLte(n) 

