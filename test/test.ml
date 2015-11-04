open Netlink.Route
       
let _ =
  (* Create and connect socket *)
  
  let s = Netlink.Socket.alloc () in
  Netlink.Socket.connect s Netlink.Socket.NETLINK_ROUTE;
  
  (* Links *)
  
  let cache = Link.cache_alloc s in
  
  let print_link_info link =
    let ifindex = Link.get_ifindex link in
    let name = Link.get_name link in
    Printf.printf "%d: %s:\n" ifindex name;
    
    let addr = Link.get_addr link in
    Printf.printf "\tAddress: %s\n" (Netlink.Address.to_string addr);
    let mtu = Link.get_mtu link in
    Printf.printf "\tMTU: %d\n" (Unsigned.UInt32.to_int mtu);
    let tx_bytes = Link.get_stat link Link.Stat_id.TX_BYTES in
    Printf.printf "\tTX bytes: %d\n" (Unsigned.UInt64.to_int tx_bytes);
    let rx_bytes = Link.get_stat link Link.Stat_id.RX_BYTES in
    Printf.printf "\tRX bytes: %d\n" (Unsigned.UInt64.to_int rx_bytes);
    let rx_errors = Link.get_stat link Link.Stat_id.RX_ERRORS in
    Printf.printf "\tRX errors: %d\n" (Unsigned.UInt64.to_int rx_errors);
    print_endline ""
  in
  print_endline "== Print links using Link.cache_iter ==\n";
  Link.cache_iter print_link_info cache;
  
  print_endline "== Print links using Link.cache_to_list and List.iter ==\n";
  let l = Link.cache_to_list cache in
  List.iter print_link_info l;
  
  Netlink.Cache.free cache;
  
  (* Addresses *)
  
  let cache = Address.cache_alloc s in
  
  let print_address_info addr =
    let ifindex = Address.get_ifindex addr in
    let label = Address.get_label addr in
    begin match label with
      | None ->
	Printf.printf "%d:\n" ifindex
      | Some label ->
	Printf.printf "%d: %s:\n" ifindex label
    end;
    
    let local = Address.get_local addr in
    Printf.printf "\t%s\n" (Netlink.Address.to_string local);
    
    print_endline ""
  in
  print_endline "== Print addresses using Addr.cache_iter ==\n";
  Address.cache_iter print_address_info cache;
  
  Netlink.Cache.free cache;
  
  (* Clean up socket *)
  
  Netlink.Socket.close s;
  Netlink.Socket.free s
;;
