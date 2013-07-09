open Xsb;;
open Syntax;;
open Flowlog;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

let debug = true;;

module type PROGRAM = sig
	val program : Flowlog.program;;
end

module Make_OxModule (Program : PROGRAM) = struct
	include OxStart.DefaultTutorialHandlers;;
	include Syntax;;
	
	let ref_out_ch = ref None;;
	let ref_in_ch = ref None;;
	let get_ch = (fun () -> match !ref_out_ch with
		| None -> let out_ch, in_ch = Xsb.start_xsb () in 
			let _ = ref_out_ch := Some(out_ch) in
			let _ = ref_in_ch := Some(in_ch) in
			let _ = Flowlog.start_program Program.program out_ch in_ch in
			let _ = if debug then print_endline "started program" in
			(out_ch, in_ch);
		| Some(out_ch) -> match !ref_in_ch with
			|Some(in_ch) -> (out_ch, in_ch);
			| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"));;
	
	let _ = get_ch ();;
	
	let switch_connected (sw : switchId) (feats : OpenFlow0x01.SwitchFeatures.t) : unit =
	    Printf.printf "Switch %Ld connected.\n%!" sw;
	    (* the next line does nothing but tests the calling of feats.ports which right now fails *)
	    let port_nums = List.map (fun (x : PortDescription.t)-> x.PortDescription.port_no) feats.SwitchFeatures.ports in
	    let out_ch, in_ch = get_ch () in
	    let sw_string = Int64.to_string sw in
	    let notifs = List.map (fun portid -> Notif_val(switch_port_type, [Constant(sw_string); Constant(string_of_int portid)])) port_nums in
	    List.iter (fun notif -> let _ = Evaluation.respond_to_notification notif Program.program out_ch in_ch in ()) notifs;;

	let pkt_to_notif (sw : switchId) (pk : packetIn) : notif_val = 
		let _ = if debug then print_endline "starting pkt_to_notif" in
		let pkt_payload = parse_payload pk.input_payload in
		let isIp = ((dlTyp pkt_payload) = 0x0800) in
		let terms = List.map (function x -> Constant(x)) [Int64.to_string sw;
		string_of_int pk.port;
		Int64.to_string pkt_payload.Packet.dlSrc;
		Int64.to_string pkt_payload.Packet.dlDst;
		string_of_int (dlTyp pkt_payload);
		Int32.to_string (nwSrc pkt_payload);
		Int32.to_string (nwDst pkt_payload);
		if isIp then (string_of_int (nwProto pkt_payload)) else "arp"] in
		let _ = if debug then print_endline ("pkt to term list: " ^ (list_to_string term_to_string ans)) in
		let _ = if debug then print_endline ("dlTyp: " ^ (string_of_int (dlTyp pkt_payload))) in
		Notif_val(packet_type, terms);;

	let begins_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1 then false else
		(String.sub str1 0 (String.length str2)) = str2;;

	let get_field (notif : notif_val) (str : string) : term = 
		match notif with Notif_val(Type(_, fields), terms) ->
		List.assoc str (List.combine fields terms);;

	(* notice that the current implementation is not efficient--if its just a repeater its doing way too much work. *)
	let forward_packets (notifs : notif_val list) (sw : switchId) (pk : packetIn) : unit = 
		let actions_list = ref [] in
		let pk_notif = pkt_to_notif sw pk in
		let dlSrc_old = term_to_string (get_field pk_notif "DlSrc") in
		let dlDst_old = term_to_string (get_field pk_notif "DlDst") in
		let nwSrc_old = term_to_string (get_field pk_notif "NwSrc") in
		let nwDst_old = term_to_string (get_field pk_notif "NwDst") in
		let _ = List.iter (fun notif -> 
			let locPt_string = term_to_string (get_field notif "LocPt") in
			(* if (begins_with locPt_string "_h") then actions_list := Output(AllPorts) :: !actions_list else *)
			let _ = actions_list := Output(PhysicalPort(int_of_string locPt_string)) :: !actions_list in
	
			let dlSrc_new = term_to_string (get_field notif "DlSrc") in
			let _ = actions_list := SetDlSrc(Int64.of_string (if (begins_with dlSrc_new "_h") then dlSrc_old else dlSrc_new)) :: !actions_list in
	
			let dlDst_new = term_to_string (get_field notif "DlDst") in
			let _ = actions_list := SetDlDst(Int64.of_string (if (begins_with dlDst_new "_h") then dlDst_old else dlDst_new)) :: !actions_list in
	
			let nwSrc_new = term_to_string (get_field notif "NwSrc") in
			let _ = actions_list := SetNwSrc(Int32.of_string (if (begins_with nwSrc_new "_h") then nwSrc_old else nwSrc_new)) :: !actions_list in
	
			let nwDst_new = term_to_string (get_field notif NwDst) in
			let _ = actions_list := SetNwDst(Int32.of_string (if (begins_with nwDst_new "_h") then nwDst_old else nwDst_new)) :: !actions_list in
			()) notifs in
		let _ = if debug then print_endline ("print packet payload: " ^ (Packet.to_string (parse_payload pk.input_payload))) in
		send_packet_out sw 0l {output_payload = pk.input_payload; port_id = None; apply_actions = !actions_list};;

	let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
		if debug then Printf.printf "%s\n%!" (packetIn_to_string pk);
		let out_ch, in_ch = get_ch () in
		let outpackets = Evaluation.respond_to_notification (pkt_to_notif sw pk) Program.program out_ch in_ch in
		forward_packets outpackets sw pk;;

		
	let cleanup () : unit = 
		let _ = if debug then print_endline "running cleanup" in
		let out_ch, in_ch = get_ch () in
		Xsb.halt_xsb out_ch;;

end

(* Eventually change to MyOxStart.Make so cleanup is called on exceptions. *)
module Make_Controller (Program : PROGRAM) = OxStart.Make (Make_OxModule (Program));;

(*module Union (Pg1 : PROGRAM) (Pg2 : PROGRAM) = struct
let program = match Pg1.program with 
	| Flowlog.Program(name_1, rel_list_1, forward_rel_1) -> 
	match Pg2.program with 
	| Flowlog.Program(name_2, rel_list_2, forward_rel_2) ->
	let process_1 = process_relation_name (append_string ("/" ^ name_1)) in
	let process_2 = process_relation_name (append_string ("/" ^ name_2)) in
	Flowlog.Program(name_1 ^ "+" ^ name_2, 
		(List.map process_1 rel_list_1) @ (List.map process_2 rel_list_2), 
		Flowlog.Relation("forward", Flowlog.packet_vars @ Flowlog.packet_vars_2, 
		(match process_1 forward_rel_1 with Flowlog.Relation(_, _, clauses) -> clauses)
		@ (match process_2 forward_rel_2 with Flowlog.Relation(_, _, clauses) -> clauses)));;
end*)