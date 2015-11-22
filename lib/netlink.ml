open Ctypes
open Foreign

(* --- to be upstreamed to ctypes --- *)
let castp typ p = from_voidp typ (to_voidp p)

let read_nullable t p =
  if p = null then None
  else Some !@(castp t (allocate (ptr void) p))
;;

let write_nullable t = function
  | None -> null
  | Some f -> !@(castp (ptr void) (allocate t f))
;;

let nullable_view t =
  let read = read_nullable t
  and write = write_nullable t in
  view ~read ~write (ptr void)
;;

let string_opt = nullable_view string
(* --- *)

(* The library names vary by distribution, so use a search list *)

let libnl_names = [
  "libnl-3.so";
  "libnl-3.so.200"; (* Debian/Ubuntu *)
]

let libnl_route_names = [
  "libnl-route-3.so";
  "libnl-route-3.so.200"; (* Debian/Ubuntu *)
]

let dlopen ~filenames ~flags =
  let rec loop = function
    | [] ->
      failwith
        (Printf.sprintf "Failed to open any of these libraries: [ %s ] (is the package missing?)"
           (String.concat ", " filenames))
    | n :: ns ->
      try
	Dl.dlopen ~filename:n ~flags
      with _ ->
	loop ns
  in
  loop filenames
;;

let libnl = dlopen ~filenames:libnl_names ~flags:[Dl.RTLD_LAZY]
let libnl_route = dlopen ~filenames:libnl_route_names ~flags:[Dl.RTLD_LAZY]

module Socket = struct
  type t
  let t : t structure typ = structure "nl_sock"
      
  type protocol = NETLINK_ROUTE
    
  let int_of_protocol = function
    | NETLINK_ROUTE -> 0
  ;;
      
  let protocol_of_int = function
    | 0 -> NETLINK_ROUTE
    | _ -> invalid_arg "protocol"
  ;;
             
  let protocol = view ~read:protocol_of_int ~write:int_of_protocol int
      
  let alloc = foreign ~from:libnl "nl_socket_alloc" (void @-> returning (ptr t))
  let free = foreign ~from:libnl "nl_socket_free" (ptr t @-> returning void)
      
  exception Connect_failed
    
  let connect' = foreign ~from:libnl "nl_connect" (ptr t @-> protocol @-> returning int)
  let connect s p =
    let ret = connect' s p in
    if ret = 0
    then ()
    else raise Connect_failed
  ;;
        
  let close = foreign ~from:libnl "nl_close" (ptr t @-> returning void)
end

module type Cache_object = sig
  type elt
  val elt : elt structure typ
end

module Cache (M : Cache_object) : sig
  type t
  val t       : t structure typ
  val free    : t structure ptr -> unit
  val iter    : (M.elt structure ptr -> unit) -> t structure ptr -> unit
  val to_list : t structure ptr -> M.elt structure ptr list
  val alloc   : ((unit ptr) ptr -> int) -> t structure ptr
end = struct
  type t
  let t : t structure typ = structure "nl_cache"

  let free cache =
    foreign ~from:libnl "nl_cache_free" (ptr t @-> returning void) cache
  ;;
  
  let iter f cache =
    let callback_t = ptr M.elt @-> ptr void @-> returning void in
    let foreach = foreign ~from:libnl "nl_cache_foreach"
	(ptr t @-> funptr callback_t @-> ptr void @-> returning void) in
    let f' x _ = f x in
    foreach cache f' null
  ;;

  let to_list cache =
    let get_first = foreign ~from:libnl "nl_cache_get_first" (ptr t @-> returning (ptr M.elt)) in
    let get_prev = foreign ~from:libnl "nl_cache_get_prev" (ptr M.elt @-> returning (ptr M.elt)) in
    let get_last = foreign ~from:libnl "nl_cache_get_last" (ptr t @-> returning (ptr M.elt)) in
    
    let first = get_first cache in
    let rec loop obj ac =
      if obj = first
      then obj :: ac
      else loop (get_prev obj) (obj :: ac)
    in
    loop (get_last cache) []
  ;;

  let alloc f =
    let cache = allocate (ptr void) null in
    match f cache with
    | 0 -> coerce (ptr void) (ptr t) (!@ cache)
    | x -> failwith (Printf.sprintf "alloc_cache failed with %d" x)
  ;;
end

module Address = struct
  type t
  let t : t structure typ = structure "nl_addr"
      
  let to_string' = foreign ~from:libnl "nl_addr2str"
      (ptr t @-> string @-> returning string)
      
  let to_string addr =
    let buf = String.make 128 ' ' in
    to_string' addr buf
  ;;
end
      
module Route = struct
  module Link = struct
    module Stat_id = struct
      type stat_id = RX_PACKETS
                   | TX_PACKETS
                   | RX_BYTES
                   | TX_BYTES
                   | RX_ERRORS
                   | TX_ERRORS
                   | RX_DROPPED
                   | TX_DROPPED           
                   | RX_COMPRESSED        
                   | TX_COMPRESSED        
                   | RX_FIFO_ERR          
                   | TX_FIFO_ERR          
                   | RX_LEN_ERR           
                   | RX_OVER_ERR
                   | RX_CRC_ERR
                   | RX_FRAME_ERR
                   | RX_MISSED_ERR
                   | TX_ABORT_ERR
                   | TX_CARRIER_ERR
                   | TX_HBEAT_ERR
                   | TX_WIN_ERR
                   | COLLISIONS
                   | MULTICAST
                   | IP6_INPKTS
                   | IP6_INHDRERRORS
                   | IP6_INTOOBIGERRORS
                   | IP6_INNOROUTES

      let to_int = function
        | RX_PACKETS -> 0
        | TX_PACKETS -> 1
        | RX_BYTES -> 2
        | TX_BYTES -> 3
        | RX_ERRORS -> 4
        | TX_ERRORS -> 5
        | RX_DROPPED -> 6
        | TX_DROPPED -> 7      
        | RX_COMPRESSED -> 8      
        | TX_COMPRESSED -> 9   
        | RX_FIFO_ERR -> 10
        | TX_FIFO_ERR -> 11
        | RX_LEN_ERR -> 12
        | RX_OVER_ERR -> 13
        | RX_CRC_ERR -> 14
        | RX_FRAME_ERR -> 15
        | RX_MISSED_ERR -> 16
        | TX_ABORT_ERR -> 17
        | TX_CARRIER_ERR -> 18
        | TX_HBEAT_ERR -> 19
        | TX_WIN_ERR -> 20
        | COLLISIONS -> 21
        | MULTICAST -> 22
        | IP6_INPKTS -> 23
        | IP6_INHDRERRORS -> 24
        | IP6_INTOOBIGERRORS -> 25
        | IP6_INNOROUTES -> 26

      let of_int = function
        | 0 -> RX_PACKETS
        | 1 -> TX_PACKETS
        | 2 -> RX_BYTES
        | 3 -> TX_BYTES
        | 4 -> RX_ERRORS
        | 5 -> TX_ERRORS
        | 6 -> RX_DROPPED
        | 7 -> TX_DROPPED           
        | 8 -> RX_COMPRESSED        
        | 9 -> TX_COMPRESSED        
        | 10 -> RX_FIFO_ERR          
        | 11 -> TX_FIFO_ERR          
        | 12 -> RX_LEN_ERR           
        | 13 -> RX_OVER_ERR
        | 14 -> RX_CRC_ERR
        | 15 -> RX_FRAME_ERR
        | 16 -> RX_MISSED_ERR
        | 17 -> TX_ABORT_ERR
        | 18 -> TX_CARRIER_ERR
        | 19 -> TX_HBEAT_ERR
        | 20 -> TX_WIN_ERR
        | 21 -> COLLISIONS
        | 22 -> MULTICAST
        | 23 -> IP6_INPKTS
        | 24 -> IP6_INHDRERRORS
        | 25 -> IP6_INTOOBIGERRORS
        | 26 -> IP6_INNOROUTES
        | _ -> invalid_arg "stat_id"

      let t = view ~read:of_int ~write:to_int int
    end

    type t
    let t : t structure typ = structure "rtnl_link"
    let foreign fname = foreign ~from:libnl_route ("rtnl_link_"^fname)
        
    module Cache = Cache(struct type elt = t let elt = t end)
    
    let alloc_cache = foreign "alloc_cache"
        (ptr Socket.t @-> int @-> ptr (ptr void) @-> returning int)
        
    let alloc = foreign "alloc"
        (void @-> returning (ptr t))

    let add = foreign "add"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)
        
    let delete = foreign "delete"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)
        
    let change = foreign "change"
        (ptr Socket.t @-> ptr t @-> ptr t @-> int @-> returning int)
        
    let get_by_name = foreign "get_by_name"
        (ptr Cache.t @-> string @-> returning (ptr t))

    let string_maker ~f i =
      let slen = 128 in
      let s = String.make slen (Char.chr 0) in
      f i s (Unsigned.Size_t.of_int slen)
    ;;

    let x2str s = foreign s (int @-> string @-> size_t @-> returning string)
    let str2x s = foreign s (string @-> returning int)

    let stat2str' = x2str "stat2str"
    let stat2str = string_maker ~f:stat2str'
    let str2stat = str2x "str2stat"
        
    let flags2str' = x2str "flags2str"
    let flags2str = string_maker ~f:flags2str'
    let str2flags = str2x "str2flags"

    let operstate2str' = x2str "operstate2str"
    let operstate2str = string_maker ~f:operstate2str'
    let str2operstate = str2x "str2operstate"

    let mode2str' = x2str "mode2str"
    let mode2str = string_maker ~f:mode2str'
    let str2mode = str2x "str2mode"

    let carrier2str' = x2str "carrier2str"
    let carrier2str = string_maker ~f:carrier2str'
    let str2carrier = str2x "str2carrier"
    
    let put = foreign "put"
        (ptr t @-> returning void)

    let set_ifindex = foreign "set_ifindex"
        (ptr t @-> int @-> returning void)
        
    let get_ifindex = foreign "get_ifindex"
        (ptr t @-> returning int)

    let set_name = foreign "set_name"
        (ptr t @-> string @-> returning void)

    let get_name = foreign "get_name"
        (ptr t @-> returning string)

    let set_mtu = foreign "set_mtu"
        (ptr t @-> uint32_t @-> returning void)
        
    let get_mtu = foreign "get_mtu"
        (ptr t @-> returning uint32_t)

    let get_stat = foreign "get_stat"
        (ptr t @-> Stat_id.t @-> returning uint64_t)

    let set_addr = foreign "set_addr"
        (ptr t @-> ptr Address.t @-> returning void)
        
    let get_addr = foreign "get_addr"
        (ptr t @-> returning (ptr Address.t))

    let get_qdisc = foreign "get_qdisc"
        (ptr t @-> returning string)

    let set_qdisc = foreign "set_qdisc"
        (ptr t @-> string @-> returning void)
        
    let set_flags = foreign "set_flags"
        (ptr t @-> uint32_t @-> returning void)

    let unset_flags = foreign "unset_flags"
        (ptr t @-> uint32_t @-> returning void)

    let get_flags = foreign "get_flags"
        (ptr t @-> returning uint32_t)

    let set_txqlen = foreign "set_txqlen"
        (ptr t @-> uint32_t @-> returning void)

    let get_txqlen = foreign "get_txqlen"
        (ptr t @-> returning uint32_t)

    let set_weight = foreign "set_weight"
        (ptr t @-> uint32_t @-> returning void)

    let get_weight = foreign "get_weight"
        (ptr t @-> returning uint32_t)

    let set_family = foreign "set_family"
        (ptr t @-> int @-> returning void)

    let get_family = foreign "get_family"
        (ptr t @-> returning int)

    let set_arptype = foreign "set_arptype"
        (ptr t @-> uint32_t @-> returning void)

    let get_arptype = foreign "get_arptype"
        (ptr t @-> returning uint32_t)

    let set_broadcast = foreign "set_broadcast"
        (ptr t @-> ptr Address.t @-> returning void)

    let get_broadcast = foreign "get_broadcast"
        (ptr t @-> returning (ptr Address.t))

    let set_link = foreign "set_link"
        (ptr t @-> int @-> returning void)

    let get_link = foreign "get_link"
        (ptr t @-> returning int)

    let set_master = foreign "set_master"
        (ptr t @-> int @-> returning void)

    let get_master = foreign "get_master"
        (ptr t @-> returning int)

    let set_operstate = foreign "set_operstate"
        (ptr t @-> uint8_t @-> returning void)

    let get_operstate = foreign "get_operstate"
        (ptr t @-> returning uint8_t)

    let set_linkmode = foreign "set_linkmode"
        (ptr t @-> uint8_t @-> returning void)

    let get_linkmode = foreign "get_linkmode"
        (ptr t @-> returning uint8_t)

    let set_info_type = foreign "set_info_type"
        (ptr t @-> string @-> returning int)

    let get_info_type = foreign "get_info_type"
        (ptr t @-> returning string)

    let set_group = foreign "set_group"
        (ptr t @-> uint32_t @-> returning void)
        
    let get_group = foreign "get_group"
        (ptr t @-> returning uint32_t)
        
    let set_carrier = foreign "set_carrier"
        (ptr t @-> uint8_t @-> returning void)
        
    let get_carrier = foreign "get_carrier"
        (ptr t @-> returning uint8_t)
        
    let get_ifalias = foreign "get_ifalias"
        (ptr t @-> returning string_opt)
        
    let set_ifalias = foreign "set_ifalias"
        (ptr t @-> string @-> returning void)
        
    let get_num_vf = foreign "get_num_vf"
        (ptr t @-> ptr uint32_t @-> returning int)
        
    let set_promiscuity = foreign "set_promiscuity"
        (ptr t @-> uint32_t @-> returning void)
        
    let get_promiscuity = foreign "get_promiscuity"
        (ptr t @-> returning uint32_t)
        
    let set_num_tx_queues = foreign "set_num_tx_queues"
        (ptr t @-> uint32_t @-> returning void)
        
    let get_num_tx_queues = foreign "get_num_tx_queues"
        (ptr t @-> returning uint32_t)
        
    let set_num_rx_queues = foreign "set_num_rx_queues"
        (ptr t @-> uint32_t @-> returning void)
        
    let get_num_rx_queues = foreign "get_num_rx_queues"
        (ptr t @-> returning uint32_t)
        
    let enslave_ifindex = foreign "enslave_ifindex"
        (ptr Socket.t @-> int @-> int @-> returning int)
        
    let enslave = foreign "enslave"
        (ptr Socket.t @-> ptr t @-> ptr t @-> returning int)
        
    let release_ifindex = foreign "release_ifindex"
        (ptr Socket.t @-> int @-> returning int)
        
    let release = foreign "release"
        (ptr Socket.t @-> ptr t @-> returning int)
  end

  module Route = struct
    type t
    let t : t structure typ = structure "rtnl_route"
    let foreign fname = foreign ~from:libnl_route ("rtnl_route_"^fname)
        
    module Cache = Cache(struct type elt = t let elt = t end)
        
    let alloc_cache =
      foreign "alloc_cache" (ptr Socket.t @-> int @-> int @-> ptr (ptr void) @-> returning int)

    let alloc = foreign "alloc"
        (void @-> returning (ptr t))
        
    let put = foreign "put"
        (ptr t @-> returning void)
        
    let get = foreign "get"
        (ptr t @-> returning void)
        
    let add = foreign "add"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)
        
    let delete = foreign "delete"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)
        
    let set_table = foreign "set_table"
        (ptr t @-> uint32_t @-> returning void)
        
    let get_table = foreign "get_table"
        (ptr t @-> returning uint32_t)
        
    let set_scope = foreign "set_scope"
        (ptr t @-> uint8_t @-> returning void)

    let get_scope = foreign "get_scope"
        (ptr t @-> returning uint8_t)

    let set_tos = foreign "set_tos"
        (ptr t @-> uint8_t @-> returning void)

    let get_tos = foreign "get_tos"
        (ptr t @-> returning uint8_t)

    let set_protocol = foreign "set_protocol"
        (ptr t @-> uint8_t @-> returning void)

    let get_protocol = foreign "get_protocol"
        (ptr t @-> returning uint8_t)

    let set_priority = foreign "set_priority"
        (ptr t @-> uint32_t @-> returning void)

    let get_priority = foreign "get_priority"
        (ptr t @-> returning uint32_t)

    let set_family = foreign "set_family"
        (ptr t @-> uint8_t @-> returning int)

    let get_family = foreign "get_family"
        (ptr t @-> returning uint8_t)

    let set_type = foreign "set_type"
        (ptr t @-> uint8_t @-> returning int)

    let get_type = foreign "get_type"
        (ptr t @-> returning uint8_t)

    let set_flags = foreign "set_flags"
        (ptr t @-> uint32_t @-> returning void)

    let unset_flags = foreign "unset_flags"
        (ptr t @-> uint32_t @-> returning void)

    let get_flags = foreign "get_flags"
        (ptr t @-> returning uint32_t)

    let set_metric = foreign "set_metric"
        (ptr t @-> int @-> int @-> returning int)

    let unset_metric = foreign "unset_metric"
        (ptr t @-> int @-> returning int)

    (*
    let get_metric = foreign "get_metric"
        (ptr t @-> int @-> uint32_t * @-> returning int) *)

    let set_dst = foreign "set_dst"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_dst = foreign "get_dst"
        (ptr t @-> returning (ptr Address.t))

    let set_src = foreign "set_src"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_src = foreign "get_src"
        (ptr t @-> returning (ptr Address.t))

    let set_pref_src = foreign "set_pref_src"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_pref_src = foreign "get_pref_src"
        (ptr t @-> returning (ptr Address.t))

    let set_iif = foreign "set_iif"
        (ptr t @-> int @-> returning void)

    let get_iif = foreign "get_iif"
        (ptr t @-> returning int)

    (* This is in route.h but not in libnl3-route?
    let get_src_len = foreign "get_src_len"
        (ptr t @-> returning int) *)

    let get_nnexthops = foreign "get_nnexthops"
        (ptr t @-> returning int)

    let guess_scope = foreign "guess_scope"
        (ptr t @-> returning int)

    let table2str = foreign "table2str"
        (int @-> string @-> size_t @-> returning string)

    let str2table = foreign "str2table"
        (string @-> returning int)

    let read_table_names = foreign "read_table_names"
        (string @-> returning int)

    let proto2str = foreign "proto2str"
        (int @-> string @-> size_t @-> returning string)

    let str2proto = foreign "str2proto"
        (string @-> returning int)

    let read_protocol_names = foreign "read_protocol_names"
        (string @-> returning int)

    let metric2str = foreign "metric2str"
        (int @-> string @-> size_t @-> returning string)

    let str2metric = foreign "str2metric"
        (string @-> returning int)
  end
  
  module RTAddress = struct
    type t
    let t : t structure typ = structure "rtnl_addr"
    let foreign fname = foreign ~from:libnl_route ("rtnl_addr_"^fname)
        
    module Cache = Cache(struct type elt = t let elt = t end)
    
    let alloc_cache =
      foreign "alloc_cache" (ptr Socket.t @-> ptr (ptr void) @-> returning int)

    let alloc = foreign "alloc"
        (void @-> returning (ptr t))

    let put = foreign "put"
        (ptr t @-> returning void)

    let add = foreign "add"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)

    let delete = foreign "delete"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)

    let flags2str' = foreign "flags2str"
        (int @-> string @-> size_t @-> returning string)
 
    let flags2str i =
      let slen = 128 in
      let s = String.make slen (Char.chr 0) in
      flags2str' i s (Unsigned.Size_t.of_int slen)
    ;;
    
    let str2flags = foreign "str2flags"
        (string @-> returning int)

    let set_ifindex = foreign "set_ifindex"
        (ptr t @-> int @-> returning void)
        
    let get_ifindex = foreign "get_ifindex"
        (ptr t @-> returning int)

    let set_label = foreign "set_label"
        (ptr t @-> string @-> returning void)
        
    let get_label = foreign "get_label"
        (ptr t @-> returning string_opt)

    let set_local = foreign "set_local"
        (ptr t @-> ptr Address.t @-> returning int)
         
    let get_local = foreign "get_local"
        (ptr t @-> returning (ptr Address.t))

    let set_family = foreign "set_family"
        (ptr t @-> int @-> returning void)

    let get_family = foreign "get_family"
        (ptr t @-> returning int)

    let set_prefixlen = foreign "set_family"
        (ptr t @-> int @-> returning void)

    let get_prefixlen = foreign "get_family"
        (ptr t @-> returning int)

    let set_scope = foreign "set_scope"
        (ptr t @-> int @-> returning void)

    let get_scope = foreign "get_scope"
        (ptr t @-> returning int)

    let set_flags = foreign "set_flags"
        (ptr t @-> int @-> returning void)
        
    let unset_flags = foreign "unset_flags"
        (ptr t @-> int @-> returning void)

    let get_flags = foreign "get_flags"
        (ptr t @-> returning int)

    let set_peer = foreign "set_peer"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_peer = foreign "get_peer"
        (ptr t @-> returning (ptr Address.t))

    let set_broadcast = foreign "set_broadcast"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_broadcast = foreign "get_broadcast"
        (ptr t @-> returning (ptr Address.t))

    let set_multicast = foreign "set_multicast"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_multicast = foreign "get_multicast"
        (ptr t @-> returning (ptr Address.t))

    let set_anycast = foreign "set_anycast"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_anycast = foreign "get_anycast"
        (ptr t @-> returning (ptr Address.t))

    let set_valid_lifetme = foreign "set_valid_lifetime"
        (ptr t @-> uint32_t @-> returning void)

    let get_valid_lifetime = foreign "get_valid_lifetime"
        (ptr t @-> returning uint32_t)

    let set_preferred_lifetime = foreign "set_preferred_lifetime"
        (ptr t @-> uint32_t @-> returning void)

    let get_preferred_lifetime = foreign "get_preferred_lifetime"
        (ptr t @-> returning uint32_t)

    let get_create_time = foreign "get_create_time"
        (ptr t @-> returning uint32_t)

    let get_last_update_time = foreign "get_last_update_time"
        (ptr t @-> returning uint32_t)

    let set_link = foreign "set_link"
        (ptr t @-> ptr Link.t @-> returning void)

    let get_link = foreign "get_link"
        (ptr t @-> returning (ptr Link.t))
  end

  module Rule = struct
    type t
    let t : t structure typ = structure "rtnl_rule"
    let foreign fname = foreign ~from:libnl_route ("rtnl_rule_"^fname)

    module Cache = Cache(struct type elt = t let elt = t end)
        
    let alloc_cache =
      foreign "alloc_cache" (ptr Socket.t @-> int @-> ptr (ptr void) @-> returning int)
            
    let alloc = foreign "alloc"
        (void @-> returning (ptr t))

    let put = foreign "put"
        (ptr t @-> returning void)

    let add = foreign "add"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)

    let delete = foreign "delete"
        (ptr Socket.t @-> ptr t @-> int @-> returning int)

    let set_family = foreign "set_family"
        (ptr t @-> int @-> returning void)

    let get_family = foreign "get_family"
        (ptr t @-> returning int)

    let set_prio = foreign "set_prio"
        (ptr t @-> uint32_t @-> returning void)

    let get_prio = foreign "get_prio"
        (ptr t @-> returning uint32_t)

    let set_mark = foreign "set_mark"
        (ptr t @-> uint32_t @-> returning void)

    let get_mark = foreign "get_mark"
        (ptr t @-> returning uint32_t)

    let set_mask = foreign "set_mask"
        (ptr t @-> uint32_t @-> returning void)

    let get_mask = foreign "get_mask"
        (ptr t @-> returning uint32_t)

    let set_table = foreign "set_table"
        (ptr t @-> uint32_t @-> returning void)

    let get_table = foreign "get_table"
        (ptr t @-> returning uint32_t)

    let set_dsfield = foreign "set_dsfield"
        (ptr t @-> uint8_t @-> returning void)

    let get_dsfield = foreign "get_dsfield"
        (ptr t @-> returning uint8_t)

    let set_src = foreign "set_src"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_src = foreign "get_src"
        (ptr t @-> returning (ptr Address.t))

    let set_dst = foreign "set_dst"
        (ptr t @-> ptr Address.t @-> returning int)

    let get_dst = foreign "get_dst"
        (ptr t @-> returning (ptr Address.t))

    let set_action = foreign "set_action"
        (ptr t @-> uint8_t @-> returning void)

    let get_action = foreign "get_action"
        (ptr t @-> returning uint8_t)

    let set_iif = foreign "set_iif"
        (ptr t @-> string @-> returning int)

    let get_iif = foreign "get_iif"
        (ptr t @-> returning string_opt)

    let set_oif = foreign "set_oif"
        (ptr t @-> string @-> returning int)

    let get_oif = foreign "get_oif"
        (ptr t @-> returning string_opt)

    let set_realms = foreign "set_realms"
        (ptr t @-> uint32_t @-> returning void)

    let get_realms = foreign "get_realms"
        (ptr t @-> returning uint32_t)

    let set_goto = foreign "set_goto"
        (ptr t @-> uint32_t @-> returning void)

    let get_goto = foreign "get_goto"
        (ptr t @-> returning uint32_t)
  end
end
