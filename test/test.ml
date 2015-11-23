open Netlink.Route

let printf = Printf.printf

let id x = x
let opt = function | None -> "<NONE>" | Some x -> x
  
let rtnl_address s =
  let cache = RTAddress.Cache.alloc (RTAddress.alloc_cache s) in

  let print_address_info addr =
    let ifindex = RTAddress.get_ifindex addr in
    let label = RTAddress.get_label addr in
    begin match label with
      | None ->
	printf "%d:\n" ifindex
      | Some label ->
	printf "%d: %s:\n" ifindex label
    end;
    
    let local = RTAddress.get_local addr in
    printf "\t%s\n" (Netlink.Address.to_string local);
    
    print_endline ""
  in
  print_endline "== Print addresses using Cache.iter ==\n";
  RTAddress.Cache.iter print_address_info cache;
  
  RTAddress.Cache.free cache;
;;

let rtnl_link s =
  let cache = Link.Cache.alloc (Link.alloc_cache s 2) in
  
  let print_link_info link =
    let ifindex = Link.get_ifindex link in
    let name = Link.get_name link in
    printf "=== %d: %s ===\n" ifindex name;

    let addr = Link.get_addr link in
    printf "Address: %s\n" (Netlink.Address.to_string addr);
    let mtu = Link.get_mtu link in
    printf "MTU: %d\n" (Unsigned.UInt32.to_int mtu);
    let tx_bytes = Link.get_stat link Link.Stat_id.TX_BYTES in
    printf "TX bytes: %d\n" (Unsigned.UInt64.to_int tx_bytes);
    let rx_bytes = Link.get_stat link Link.Stat_id.RX_BYTES in
    printf "RX bytes: %d\n" (Unsigned.UInt64.to_int rx_bytes);
    let rx_errors = Link.get_stat link Link.Stat_id.RX_ERRORS in
    printf "RX errors: %d\n" (Unsigned.UInt64.to_int rx_errors);

    printf "get_qdisc : %s\n%!" (id (Link.get_qdisc link));
    printf "get_name : %s\n%!" (id (Link.get_name link));
    printf "get_group : %d\n%!" (Unsigned.UInt32.to_int (Link.get_group link));
    printf "get_flags : %d\n%!" (Unsigned.UInt32.to_int (Link.get_flags link));
    printf "get_mtu : %d\n%!" (Unsigned.UInt32.to_int (Link.get_mtu link));
    printf "get_txqlen : %d\n%!" (Unsigned.UInt32.to_int (Link.get_txqlen link));
    printf "get_ifindex : %d\n%!" (id (Link.get_ifindex link));
    printf "get_family : %d\n%!" (id (Link.get_family link));
    printf "get_arptype : %d\n%!" (Unsigned.UInt32.to_int (Link.get_arptype link));
    printf "get_addr : %s\n%!" (Netlink.Address.to_string (Link.get_addr link));
    printf "get_broadcast : %s\n%!" (Netlink.Address.to_string (Link.get_broadcast link));
    printf "get_link : %d\n%!" (id (Link.get_link link));
    printf "get_master : %d\n%!" (id (Link.get_master link));
    printf "get_carrier : %d\n%!" (Unsigned.UInt8.to_int (Link.get_carrier link));
    printf "get_operstate : %d\n%!" (Unsigned.UInt8.to_int (Link.get_operstate link));
    printf "get_linkmode : %d\n%!" (Unsigned.UInt8.to_int (Link.get_linkmode link));
    printf "get_ifalias : %s\n%!" (opt (Link.get_ifalias link));
    (* printf "get_type : %s\n%!" (id (Link.get_type link)); *)
    printf "get_promiscuity : %d\n%!" (Unsigned.UInt32.to_int (Link.get_promiscuity link));
    printf "get_num_tx_queues : %d\n%!" (Unsigned.UInt32.to_int (Link.get_num_tx_queues link));
    printf "get_num_rx_queues : %d\n%!" (Unsigned.UInt32.to_int (Link.get_num_rx_queues link));
    printf "get_info_type : %s\n%!" (opt (Link.get_info_type link));
    printf "get_weight : %d\n%!" (Unsigned.UInt32.to_int (Link.get_weight link));

    printf "\n"
  in
  print_endline "== Print links using Cache.iter ==\n";
  Link.Cache.iter print_link_info cache;
  
  print_endline "== Print links using Cache.to_list and List.iter ==\n";
  let l = Link.Cache.to_list cache in
  List.iter print_link_info l;

  Link.Cache.free cache;
;;

let rtnl_rule s =
  let cache = Rule.Cache.alloc (Rule.alloc_cache s 2) in

  let print_rule rule =
    let src = Rule.get_src rule in
    printf "\tSource     : %s\n" (Netlink.Address.to_string src);
    let dst = Rule.get_dst rule in
    printf "\tDestination: %s\n" (Netlink.Address.to_string dst);
    printf "\tMark       : %d\n" (Rule.get_mark rule |> Unsigned.UInt32.to_int);
    printf "\tFamily     : %d\n" (Rule.get_family rule);
    let fix_str = function | None -> "<NONE>" | Some s -> s in
    printf "\tIIF        : %s\n" (Rule.get_iif rule |> fix_str);
    printf "\tOIF        : %s\n" (Rule.get_oif rule |> fix_str);
  in
  print_endline "== Print rules using Cache.iter ==\n";
  Rule.Cache.iter print_rule cache;

  Rule.Cache.free cache
;;

let rtnl_route s =
  let cache = Route.Cache.alloc (Route.alloc_cache s 2 0) in

  let print_route route =
    let src = Route.get_src route in
    printf "\tSource     : %s\n" (Netlink.Address.to_string src);
    let dst = Route.get_dst route in
    printf "\tDestination: %s\n" (Netlink.Address.to_string dst);
    printf "\tFamily     : %d\n" (Route.get_family route |> Unsigned.UInt8.to_int);
    printf "\tIIF        : %d\n" (Route.get_iif route);
  in
  print_endline "== Print routes using Cache.iter ==\n";
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
