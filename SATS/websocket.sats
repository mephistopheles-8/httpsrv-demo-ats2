
#include "./../HATS/project.hats"


typedef ws_op = uintBtwe(0,15)
macdef ws_op_continuation = 0x0U
macdef ws_op_text = 0x1U
macdef ws_op_bin = 0x2U
macdef ws_op_close = 0x8U
macdef ws_op_ping = 0x9U
macdef ws_op_pong = 0xAU

vtypedef ws_frame_parser(l:addr,n:int) = @{
  is_fin = bool
, opcode = ws_op
, is_masked = bool
, payload_begun = bool
, masking_key = uint
, payload_length = ullint
, payload_length_size = intLte(16)
, buf = arrayptr(byte,l,n)
, bufsz = size_t n
, j = sizeLte(n) // position within buf
, i = ullint // position in msg
, k = ullint // position in payload
} 

fun {} ws_handshake_accept( 
    sec_websocket_key: &array(char,24)
  , sec_websocket_accept: &array(char?,29) >> array(char,29) 
  ): void

datatype ws_parse_status =
  | ws_success
  | ws_continue
  | ws_buffer_full

fun {} ws_frame_parse{n,bsz:nat}{l:addr}( &bytes(n), size_t n, &ws_frame_parser(l,bsz) )
  : ws_parse_status 
