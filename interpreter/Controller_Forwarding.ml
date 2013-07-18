open Flowlog_Types;;
open Type_Helpers;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

let debug = true;;

module Controller_Forwarding = struct

	let pkt_buffer = ref None;;

	(* type of pkt_info is switchId * packetIn option but I can't declare it that for some reason. *)
	let update_buffer pkt_info : unit = pkt_buffer := pkt_info;;

	let begins_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1 then false else
		(String.sub str1 0 (String.length str2)) = str2;;

	let get_field (notif : Types.notif_val) (str : string) : Types.term =
		if debug then print_endline ("starting get_field with field " ^ str);
		match notif with Types.Notif_val(Types.Type(_, fields), terms) ->
		let combined = List.combine fields terms in
		if debug then print_endline (Type_Helpers.list_to_string (function (str, t) -> str ^ ":" ^ Type_Helpers.term_to_string t) combined);
		if debug then print_endline (Type_Helpers.term_to_string (List.assoc str combined));
		List.assoc str combined;;

    let handle_arp_nwproto (parsedpk : Packet.packet) : int = 
      match parsedpk.nw with (* fail if ever anything else *)
        | Arp(Arp.Query (_, _, _)) -> 1
        | Arp(Arp.Reply (_, _, _, _)) -> 2
        | _ -> failwith "handle_arp_nwproto: expected arp query or reply.";;

	let pkt_to_notif (sw : switchId) (pk : packetIn) : Types.notif_val = 
	   try
		if debug then print_endline "starting pkt_to_notif";
		let pkt_payload = parse_payload pk.input_payload in
		let isIp = ((dlTyp pkt_payload) = 0x0800) in
		let terms = List.map (function x -> Types.Constant(x)) [Int64.to_string sw;
		string_of_int pk.port;
		Int64.to_string pkt_payload.Packet.dlSrc;
		Int64.to_string pkt_payload.Packet.dlDst;
		string_of_int (dlTyp pkt_payload);
		Int32.to_string (nwSrc pkt_payload);
		Int32.to_string (nwDst pkt_payload);
		(* It is vital to not lose the nwProto field, even for ARP packets. 
		   OpenFlow smuggles whether the packet is query (1) or reply (2) in that field! 
		   Yet, new version of ocaml-packet throws an exception if we try to access that field
		   for an ARP packet... *)
		if isIp then (string_of_int (nwProto pkt_payload)) else (string_of_int (handle_arp_nwproto pkt_payload))] in
		(*let _ = if debug then print_endline ("pkt to term list: " ^ (Type_Helpers.list_to_string Type_Helpers.term_to_string ans)) in
		let _ = if debug then print_endline ("dlTyp: " ^ (string_of_int (dlTyp pkt_payload))) in*)
		if debug then print_endline "finishing pkt_to_notif";
		Types.Notif_val(Types.packet_type, terms)

	with 	 
	 Invalid_argument(x) -> Printf.printf "Invalid_argument in pkt_to_notif: %s\n%!" x; exit(1)
	| _ -> Printf.printf "unknown exception in pkt_to_notif!\n%!"; exit(1);;


     let of_pport_to_string (pp: pseudoPort) : string =
     match pp with      
      | PhysicalPort(id) -> "PhysicalPort: "^(string_of_int id);
      | AllPorts -> "AllPorts";
      | InPort -> "InPort";
      | Flood -> "Flood";
      | Controller(x) -> "Controller:"^(string_of_int x);;

      let sod = function
        Some v -> v
        | None -> -1;;

    let of_action_to_string (act : action) : string =
    match act with
     | Output(pseudoPort) -> "Output to "^(of_pport_to_string pseudoPort);
     | SetDlVlan(dlVlan) -> "Set dlVlan="^(string_of_int (sod dlVlan));
     | SetDlVlanPcp(dlVlanPcp) -> "Set dlVlanPcp="^(string_of_int dlVlanPcp);
     | SetDlSrc(dlAddr) -> "Set dlSrc="^(Int64.to_string dlAddr);
     | SetDlDst(dlAddr) -> "Set dlDst="^(Int64.to_string dlAddr);
     | SetNwSrc(nwAddr) -> "Set nwsrc="^(Int32.to_string nwAddr);
     | SetNwDst(nwAddr) -> "Set nwdst="^(Int32.to_string nwAddr);
     | SetNwTos(nwTos) -> "Set nwTos= "^(string_of_int nwTos);
     | SetTpSrc(tpPort) -> "Set tpSrc = "^(string_of_int tpPort);
     | SetTpDst(tpPort) -> "Set tpDst = "^(string_of_int tpPort);;

	(* notice that the current implementation is not efficient--if its just a repeater its doing way too much work. *)
	let forward_packets (notifs : Types.notif_val list) : unit =
		match !pkt_buffer with
		| None -> raise (Failure "forward packets called before packet arrived.");
		| Some(sw, pk) ->
		let _ = pkt_buffer := None in
		let actions_list = ref [] in
		let pk_notif = pkt_to_notif sw pk in
		let dlSrc_old = Type_Helpers.term_to_string (get_field pk_notif "DLSRC") in
		if debug then print_endline ("dlSrc_old: " ^ dlSrc_old);
		let dlDst_old = Type_Helpers.term_to_string (get_field pk_notif "DLDST") in
		let nwSrc_old = Type_Helpers.term_to_string (get_field pk_notif "NWSRC") in
		let nwDst_old = Type_Helpers.term_to_string (get_field pk_notif "NWDST") in
		if debug then print_endline ("dlSrc_old: " ^ dlSrc_old ^ " dlDst_old: " ^ dlDst_old ^ " nwSrc_old: " ^ nwSrc_old ^ " nwDst_old: " ^ nwDst_old);
		let _ = List.iter (fun notif -> 
			let locPt_string = Type_Helpers.term_to_string (get_field notif "LOCPT") in
			(* if (begins_with locPt_string "_h") then actions_list := Output(AllPorts) :: !actions_list else *)
			let _ = actions_list := Output(PhysicalPort(int_of_string locPt_string)) :: !actions_list in
	
			let dlSrc_new = Type_Helpers.term_to_string (get_field notif "DLSRC") in
			let _ = actions_list := SetDlSrc(Int64.of_string (if (begins_with dlSrc_new "_h") then dlSrc_old else dlSrc_new)) :: !actions_list in
	
			let dlDst_new = Type_Helpers.term_to_string (get_field notif "DLDST") in
			let _ = actions_list := SetDlDst(Int64.of_string (if (begins_with dlDst_new "_h") then dlDst_old else dlDst_new)) :: !actions_list in
	
			let nwSrc_new = Type_Helpers.term_to_string (get_field notif "NWSRC") in
			let _ = actions_list := SetNwSrc(Int32.of_string (if (begins_with nwSrc_new "_h") then nwSrc_old else nwSrc_new)) :: !actions_list in
	
			let nwDst_new = Type_Helpers.term_to_string (get_field notif "NWDST") in
			let _ = actions_list := SetNwDst(Int32.of_string (if (begins_with nwDst_new "_h") then nwDst_old else nwDst_new)) :: !actions_list in
			()) notifs in
		let _ = if debug then Printf.printf "FORWARDING PACKET. Switch=%s, Fields= %s\n%!" (Int64.to_string sw) (Packet.to_string (parse_payload pk.input_payload)) in
        let _ = if debug then (List.iter (fun act -> (Printf.printf "---ACTION: %s\n%!" (of_action_to_string act))) !actions_list) in
        let _ = if debug && (List.length !actions_list) = 0 then Printf.printf "---NO ACTIONS! Packet will be dropped.\n%!" in
		send_packet_out sw 0l {output_payload = pk.input_payload; port_id = None; apply_actions = !actions_list};;

end

