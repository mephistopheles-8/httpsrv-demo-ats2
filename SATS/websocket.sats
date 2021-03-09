
#include "./../HATS/project.hats"

fun {} ws_handshake_accept( 
    sec_websocket_key: &array(char,24)
  , sec_websocket_accept: &array(char?,29) >> array(char,29) 
  ): void
