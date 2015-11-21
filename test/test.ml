open Netlink.Route

let printf = Printf.printf
               
let rtnl_address s =
  let cache = RTAddress.Cache.alloc s in

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
  let cache = Link.Cache.alloc s in
  
  let print_link_info link =
    let ifindex = Link.get_ifindex link in
    let name = Link.get_name link in
    printf "%d: %s:\n" ifindex name;
    
    let addr = Link.get_addr link in
    printf "\tAddress: %s\n" (Netlink.Address.to_string addr);
    let mtu = Link.get_mtu link in
    printf "\tMTU: %d\n" (Unsigned.UInt32.to_int mtu);
    let tx_bytes = Link.get_stat link Link.Stat_id.TX_BYTES in
    printf "\tTX bytes: %d\n" (Unsigned.UInt64.to_int tx_bytes);
    let rx_bytes = Link.get_stat link Link.Stat_id.RX_BYTES in
    printf "\tRX bytes: %d\n" (Unsigned.UInt64.to_int rx_bytes);
    let rx_errors = Link.get_stat link Link.Stat_id.RX_ERRORS in
    printf "\tRX errors: %d\n" (Unsigned.UInt64.to_int rx_errors);
    print_endline ""
  in
  print_endline "== Print links using Cache.iter ==\n";
  Link.Cache.iter print_link_info cache;
  
  print_endline "== Print links using Cache.to_list and List.iter ==\n";
  let l = Link.Cache.to_list cache in
  List.iter print_link_info l;

  Link.Cache.free cache;
;;

let rtnl_rule s =
  let cache = Rule.Cache.alloc s in

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

let _ =
  (* Create and connect socket *)
  let s = Netlink.Socket.alloc () in
  Netlink.Socket.connect s Netlink.Socket.NETLINK_ROUTE;

  (* Tests *)
  rtnl_link s;  
  rtnl_address s;
  rtnl_rule s;
  
  (* Clean up socket *)
  Netlink.Socket.close s;
  Netlink.Socket.free s
;;
