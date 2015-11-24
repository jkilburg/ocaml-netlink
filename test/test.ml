open Netlink.Route

let printf = Printf.printf

let id x = x
let opt = function | None -> "<NONE>" | Some x -> x
  
let rtnl_address s =
  let cache = RTAddress.Cache.alloc (RTAddress.alloc_cache s) in

  let print_address_info addr =
    printf "\tget_label : %s\n%!" (opt (RTAddress.get_label addr));
    printf "\tget_ifindex : %d\n%!" (id (RTAddress.get_ifindex addr));
    printf "\tget_family : %d\n%!" (id (RTAddress.get_family addr));
    printf "\tget_prefixlen : %d\n%!" (id (RTAddress.get_prefixlen addr));
    printf "\tget_scope : %d\n%!" (id (RTAddress.get_scope addr));
    printf "\tget_flags : %d\n%!" (Unsigned.UInt32.to_int (RTAddress.get_flags addr));
    printf "\tget_local : %s\n%!" (Netlink.Address.to_string (RTAddress.get_local addr));
    printf "\tget_peer : %s\n%!" (Netlink.Address.to_string (RTAddress.get_peer addr));
    printf "\tget_broadcast : %s\n%!" (Netlink.Address.to_string (RTAddress.get_broadcast addr));
    printf "\tget_multicast : %s\n%!" (Netlink.Address.to_string (RTAddress.get_multicast addr));
    printf "\tget_anycast : %s\n%!" (Netlink.Address.to_string (RTAddress.get_anycast addr));
    printf "\tget_valid_lifetime : %d\n%!" (Unsigned.UInt32.to_int (RTAddress.get_valid_lifetime addr));
    printf "\tget_preferred_lifetime : %d\n%!" (Unsigned.UInt32.to_int (RTAddress.get_preferred_lifetime addr));
    printf "\tget_create_time : %d\n%!" (Unsigned.UInt32.to_int (RTAddress.get_create_time addr));
    printf "\tget_last_update_time : %d\n%!" (Unsigned.UInt32.to_int (RTAddress.get_last_update_time addr));
    
    printf "\n"
  in
  printf "== Print addresses using Cache.iter ==\n";
  RTAddress.Cache.iter print_address_info cache;
  
  RTAddress.Cache.free cache;
;;

let rtnl_link s =
  let cache = Link.Cache.alloc (Link.alloc_cache s 2) in
  
  let print_link_info link =
    let tx_bytes = Link.get_stat link Link.Stat_id.TX_BYTES in
    printf "\tTX bytes: %d\n" (Unsigned.UInt64.to_int tx_bytes);
    let rx_bytes = Link.get_stat link Link.Stat_id.RX_BYTES in
    printf "\tRX bytes: %d\n" (Unsigned.UInt64.to_int rx_bytes);
    let rx_errors = Link.get_stat link Link.Stat_id.RX_ERRORS in
    printf "\tRX errors: %d\n" (Unsigned.UInt64.to_int rx_errors);

    printf "\tget_qdisc : %s\n%!" (id (Link.get_qdisc link));
    printf "\tget_name : %s\n%!" (id (Link.get_name link));
    printf "\tget_group : %d\n%!" (Unsigned.UInt32.to_int (Link.get_group link));
    printf "\tget_flags : %d\n%!" (Unsigned.UInt32.to_int (Link.get_flags link));
    printf "\tget_mtu : %d\n%!" (Unsigned.UInt32.to_int (Link.get_mtu link));
    printf "\tget_txqlen : %d\n%!" (Unsigned.UInt32.to_int (Link.get_txqlen link));
    printf "\tget_ifindex : %d\n%!" (id (Link.get_ifindex link));
    printf "\tget_family : %d\n%!" (id (Link.get_family link));
    printf "\tget_arptype : %d\n%!" (Unsigned.UInt32.to_int (Link.get_arptype link));
    printf "\tget_addr : %s\n%!" (Netlink.Address.to_string (Link.get_addr link));
    printf "\tget_broadcast : %s\n%!" (Netlink.Address.to_string (Link.get_broadcast link));
    printf "\tget_link : %d\n%!" (id (Link.get_link link));
    printf "\tget_master : %d\n%!" (id (Link.get_master link));
    printf "\tget_carrier : %d\n%!" (Unsigned.UInt8.to_int (Link.get_carrier link));
    printf "\tget_operstate : %d\n%!" (Unsigned.UInt8.to_int (Link.get_operstate link));
    printf "\tget_linkmode : %d\n%!" (Unsigned.UInt8.to_int (Link.get_linkmode link));
    printf "\tget_ifalias : %s\n%!" (opt (Link.get_ifalias link));
    (* printf "get_type : %s\n%!" (id (Link.get_type link)); *)
    printf "\tget_promiscuity : %d\n%!" (Unsigned.UInt32.to_int (Link.get_promiscuity link));
    printf "\tget_num_tx_queues : %d\n%!" (Unsigned.UInt32.to_int (Link.get_num_tx_queues link));
    printf "\tget_num_rx_queues : %d\n%!" (Unsigned.UInt32.to_int (Link.get_num_rx_queues link));
    printf "\tget_info_type : %s\n%!" (opt (Link.get_info_type link));
    printf "\tget_weight : %d\n%!" (Unsigned.UInt32.to_int (Link.get_weight link));

    printf "\n"
  in
  printf "\n== Print links using Cache.iter ==\n";
  Link.Cache.iter print_link_info cache;
  
  Link.Cache.free cache;
;;

let rtnl_rule s =
  let cache = Rule.Cache.alloc (Rule.alloc_cache s 2) in

  let print_rule rule =
    printf "\tget_family : %d\n%!" (id (Rule.get_family rule));
    printf "\tget_prio : %d\n%!" (Unsigned.UInt32.to_int (Rule.get_prio rule));
    printf "\tget_mark : %d\n%!" (Unsigned.UInt32.to_int (Rule.get_mark rule));
    printf "\tget_mask : %d\n%!" (Unsigned.UInt32.to_int (Rule.get_mask rule));
    printf "\tget_table : %d\n%!" (Unsigned.UInt32.to_int (Rule.get_table rule));
    printf "\tget_dsfield : %d\n%!" (Unsigned.UInt8.to_int (Rule.get_dsfield rule));
    printf "\tget_src : %s\n%!" (Netlink.Address.to_string (Rule.get_src rule));
    printf "\tget_dst : %s\n%!" (Netlink.Address.to_string (Rule.get_dst rule));
    printf "\tget_action : %d\n%!" (Unsigned.UInt8.to_int (Rule.get_action rule));
    printf "\tget_iif : %s\n%!" (opt (Rule.get_iif rule));
    printf "\tget_oif : %s\n%!" (opt (Rule.get_oif rule));
    printf "\tget_realms : %d\n%!" (Unsigned.UInt32.to_int (Rule.get_realms rule));
    printf "\tget_goto : %d\n%!" (Unsigned.UInt32.to_int (Rule.get_goto rule));

    printf "\n";
  in
  printf "== Print rules using Cache.iter ==\n";
  Rule.Cache.iter print_rule cache;

  Rule.Cache.free cache
;;

let rtnl_route s =
  let cache = Route.Cache.alloc (Route.alloc_cache s 2 0) in

  let print_route route =
    printf "\tget_table : %d\n%!" (Unsigned.UInt32.to_int (Route.get_table route));
    printf "\tget_scope : %d\n%!" (Unsigned.UInt8.to_int (Route.get_scope route));
    printf "\tget_tos : %d\n%!" (Unsigned.UInt8.to_int (Route.get_tos route));
    printf "\tget_protocol : %d\n%!" (Unsigned.UInt8.to_int (Route.get_protocol route));
    printf "\tget_priority : %d\n%!" (Unsigned.UInt32.to_int (Route.get_priority route));
    printf "\tget_family : %d\n%!" (Unsigned.UInt8.to_int (Route.get_family route));
    printf "\tget_type : %d\n%!" (Unsigned.UInt8.to_int (Route.get_type route));
    printf "\tget_flags : %d\n%!" (Unsigned.UInt32.to_int (Route.get_flags route));
    printf "\tget_dst : %s\n%!" (Netlink.Address.to_string (Route.get_dst route));
    printf "\tget_src : %s\n%!" (Netlink.Address.to_string (Route.get_src route));
    printf "\tget_pref_src : %s\n%!" (Netlink.Address.to_string (Route.get_pref_src route));
    printf "\tget_iif : %d\n%!" (id (Route.get_iif route));
    (* printf "\tget_src_len : %d\n%!" (id (Route.get_src_len route)); *)
    printf "\tget_nnexthops : %d\n%!" (id (Route.get_nnexthops route));
    
    printf "\n";
  in
  printf "== Print routes using Cache.iter ==\n";
  Route.Cache.iter print_route cache;

  Route.Cache.free cache
;;

let _ =
  (* Create and connect socket *)
  let s = Netlink.Socket.alloc () in
  Netlink.Socket.connect s Netlink.Socket.NETLINK_ROUTE;

  (* Tests *)
  rtnl_link s;  
  rtnl_address s;
  rtnl_rule s;
  rtnl_route s;
  
  (* Clean up socket *)
  Netlink.Socket.close s;
  Netlink.Socket.free s
;;
