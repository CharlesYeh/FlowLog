open Flowlog_Types
open Flowlog_Helpers
open NetCore_Types
open ExtList.List
open Printf
open Xsb_Communication



(* 
  ALLOWED atomic fmlas:

  // assume in clause body. assume NNF. assume vars have been substituted out as much as possible
  // means that such vars that DO occur can never appear as x=y or x=const.
  
  
  (5) atom: 
*)

(* FORBIDDEN (pass to controller always):
% (1) joins over existentials [can do better here, no hard constraint]
% (2) assigns to newpkt fields not supported for modif. in OF, like nwProto [up to openflow to allow]
% (3) assigns to allowed newpkt field in pkt-dependent way, not state-dep way 
%    e.g. newpkt.dlSrc = pkt.dlDst
%    but R(newpkt.dlSrc, pkt.dlDst) is ok
%    ^^^ this may result in multiple packets being sent. but that's fine.
%    SPECIAL CASE: newpkt.locPt != pkt.locPt is allowed. Means to use AllPorts.
% (4) pkt.x = pkt.y --- can't do equality in netcore preds
*)

(* Any assignment to a newpkt field must either be 
  (1) the trivial old-field assignment WITHOUT a surrounding not
  (2) a positive relational binding (no surrounding NOT)      
  (3) special case NOT newpkt.locPt = oldpkt.locPt *)

exception IllegalFieldModification of formula;;
exception IllegalAssignmentViaEquals of formula;;
exception IllegalAtomMustBePositive of formula;;
exception IllegalExistentialUse of formula;;
exception IllegalEquality of (term * term);;

let packet_fields = ["locSw";"locPt";"dlSrc";"dlDst";"dlTyp";"nwSrc";"nwDst";"nwProto"];;
let legal_to_modify_packet_fields = ["locPt";"dlSrc";"dlDst";"dlTyp";"nwSrc";"nwDst"];;

let legal_field_to_modify (fname: string): bool =
	mem fname legal_to_modify_packet_fields;;

(* 2 & 3 *) 
let rec forbidden_assignment_check (newpkt: string) (f: formula) (innot: bool): unit = 
 	  let check_legal_newpkt_fields = function  
							| TField(varname, fld)
                when varname = newpkt -> 
      				   			if not (legal_field_to_modify fld) then
      	 					   		raise (IllegalFieldModification f)
      	 					| _ -> () 
      	 				in
    (* use of negation on equality: ok if [pkt.x = 5], [new.pt = old.pt], [newpkt.x = 5] *)
    let check_legal_negation (t1: term) (t2: term): unit =
      let dangerous = (match (t1, t2) with 
          | (TField(v1, f1), TField(v2, f2)) ->
            f1 <> "locPt" || f2 <> "locPt"
          | (TField(v1, f1), TConst(cstr)) -> v1 = newpkt
          | _ -> false) in 
      (*(printf "check_legal_negation: %s %s %b %b\n%!" (string_of_term t1) (string_of_term t2) innot dangerous);*)
      if (innot && dangerous) then raise (IllegalEquality(t1,t2))
    in

    let check_not_same_pkt (t1: term) (t2: term): unit =
      match (t1, t2) with 
        | (TField(v, f), TField(v2, f2)) when v = v2 ->
              raise (IllegalEquality(t1,t2))
        | _ -> ()
    in


    let check_same_field_if_newpkt (t1:term) (t2:term) : unit = 
    	match (t1, t2) with
			   | (TField(var1, fld1), TField(var2, fld2))
			     	when var1 = newpkt || var2 = newpkt ->
			       	if fld1 <> fld2 then raise (IllegalAssignmentViaEquals f)
      	 | (TField(var1, fld1), TVar(_)) 
            when var1 = newpkt -> raise (IllegalAssignmentViaEquals f)
      	 | (TVar(_), TField(var1, fld2)) 
            when var1 = newpkt -> raise (IllegalAssignmentViaEquals f)
      	 | (TField(var1, fld1), TConst(_)) -> ()	
      	 | (TConst(_), TField(var2, fld2)) -> ()
      	 | _ -> ()	      	 	
      	 	in

	match f with
		| FTrue -> ()
    	| FFalse -> ()
    	| FAnd(f1, f2) ->
      		 forbidden_assignment_check newpkt f1 innot;
      		 forbidden_assignment_check newpkt f2 innot;
      	| FOr(f1, f2) -> 
      		 forbidden_assignment_check newpkt f1 innot;
      		 forbidden_assignment_check newpkt f2 innot;
      	| FNot(f) -> forbidden_assignment_check newpkt f (not innot);
    	| FEquals(t1, t2) -> 
        (* ALLOWED: 
        (1) equality: between new and old, same field. NOT negated
        (2) equality: special case NOT(newpkt.locPt = pkt.locPt)
        (3) equality: new = const
        (4) equality: old = const *)
        check_legal_negation t1 t2; (* if negated, must be special case *)
    		check_legal_newpkt_fields t1; (* not trying to set an unsettable field *)
    		check_legal_newpkt_fields t2;
    		check_same_field_if_newpkt t1 t2; (* can't swap fields, etc. w/o controller *)
        check_not_same_pkt t1 t2;

      	| FAtom(modname, relname, tlargs) ->       		
      		(* new field must be legal for modification by openflow *)
      		iter check_legal_newpkt_fields tlargs;
      		(* if involves a newpkt, must be positive *)    
      		if (innot && (ExtList.List.exists (function | TField(newpkt, _) -> true | _ -> false) tlargs)) then  	
      			raise (IllegalAtomMustBePositive f);;      		

(* returns list of existentials used by f. Will throw exception if re-used in new *atomic* fmla.
	assume this is a clause fmla (no disjunction) *)
let rec common_existential_check (sofar: string list) (f: formula): string list =
	let ext_helper (t: term): (string list) =
		match t with 
			| TVar(v) -> 
				if mem v sofar then raise (IllegalExistentialUse f)
				else [v]
			| TConst(_) -> []
			| TField(_,_) -> [] 
		in

	match f with
		| FTrue -> []
   	| FFalse -> []
   	| FAnd(f1, f2) ->
     		 let lhs_uses = common_existential_check sofar f1 in
     		 	(* unique is in ExtLst.List --- removes duplicates *)
     		 	unique (lhs_uses @ common_existential_check (lhs_uses @ sofar) f2)
  	| FOr(f1, f2) -> failwith "common_existential_check"      		 
  	| FNot(f) -> common_existential_check sofar f 
   	| FEquals(t1, t2) -> 
   		(* equals formulas don't represent a join. do nothing *)
   		sofar
   	| FAtom(modname, relname, tlargs) ->  
    		unique (flatten (map ext_helper tlargs));;	

let validate_clause (cl: clause): unit =
	ignore (common_existential_check [] cl.body);	
	match cl.head with 
		| FAtom("", "forward", [TVar(newpktname)]) ->
			forbidden_assignment_check newpktname cl.body false
		| _ -> failwith "validate_clause";;

(***************************************************************************************)

(* Replace state references with constant matrices *)
let rec partial_evaluation (f: formula): formula = 
  (* assume valid clause body for PE *)
  match f with 
    | FTrue -> f
    | FFalse -> f
    | FEquals(t1, t2) -> f
    | FAnd(f1, f2) -> FAnd(partial_evaluation f1, partial_evaluation f2)
    | FNot(f) -> FNot(partial_evaluation f)
    | FOr(f1, f2) -> failwith "partial_evaluation"              
    | FAtom(modname, relname, tlargs) ->  
      printf "partial_evaluation on atomic %s\n%!" (string_of_formula f);
      let xsbresults: (string list) list = Communication.get_state f in
        (*iter (fun sl -> printf "result: %s\n%!" (String.concat "," sl)) results;*)
        let disjuncts = map 
          (fun sl -> build_and (reassemble_xsb_equality tlargs sl)) 
          xsbresults in
        let fresult = build_or disjuncts in
        printf "... result was: %s\n%!" (string_of_formula fresult);
        fresult;;

(***************************************************************************************)

(*type output = {
  outDlSrc : dlAddr match_modify;
  outDlDst : dlAddr match_modify;
  outDlVlan : dlVlan match_modify;
  outDlVlanPcp : dlVlanPcp match_modify;
  outNwSrc : nwAddr match_modify;
  outNwDst : nwAddr match_modify;
  outNwTos : nwTos match_modify;
  outTpSrc : tpPort match_modify;
  outTpDst : tpPort match_modify;
  outPort : port 
}*)

let rec build_switch_actions (oldpkt: string) (body: formula): action =
  (* list of SwitchAction(output)*)
  let atoms = conj_to_list body in
  printf ">>> bsa: %s\n%!" (String.concat " ; " (map string_of_formula atoms));

  (* TODO: This includes dealing with special case pkt.locPt != newpkt.locPt *)

  [];;

open NetCore_Pattern
open NetCore_Wildcard


(* worst ocaml error ever: used "val" for varname. *)

let build_switch_pred (oldpkt: string) (body: formula): pred =  
let field_to_pattern (fld: string) (aval:string): NetCore_Pattern.t =         
  match fld with (* switch handled via different pred type *)
    | "locPt" -> {all with ptrnInPort = WildcardExact (Physical(Int32.of_string aval)) }
    | "dlSrc" -> {all with ptrnDlSrc = WildcardExact (Int64.of_string aval) }
    | "dlDst" -> {all with ptrnDlDst = WildcardExact (Int64.of_string aval) }
    | "dlTyp" -> {all with ptrnDlTyp = WildcardExact (int_of_string aval) }
    | "nwSrc" -> {all with ptrnNwSrc = WildcardExact (Int32.of_string aval) }
    | "nwDst" ->  {all with ptrnNwDst = WildcardExact (Int32.of_string aval) }
    | "nwProto" -> {all with ptrnNwProto = WildcardExact (int_of_string aval) }
    | _ -> failwith "field_to_pattern" in
    (* TODO: dlVLan, dlVLanPCP *)

  let eq_to_pred (eqf: formula): pred option =
    match eqf with
      (* only match oldpkt.<field> here*)        
       FNot(FEquals((TField(varname, fld)), (TConst(aval)))) when varname = oldpkt ->
        if fld = "locSw" then Some(OnSwitch(Int64.of_string aval))
        else Some(Hdr(field_to_pattern fld aval))
      | FEquals(TConst(aval), TField(varname, fld)) when varname = oldpkt ->
        if fld = "locSw" then Some(Not(OnSwitch(Int64.of_string aval)))  
        else Some(Not(Hdr(field_to_pattern fld aval)))
      | FTrue -> Some(Everything)
      | FFalse -> Some(Nothing)
      | _ -> failwith ("build_switch_pred: "^(string_of_formula eqf)) in

  (* After PE, should be only equalities and negated equalities. Should be just a conjunction *)
  let eqlist = conj_to_list body in 
    (* only extract involving oldpkt *)
     (*| Hdr of ptrn
  | OnSwitch of switchId*)
    let predlist = filter_map eq_to_pred eqlist in
      fold_left (fun acc pred -> match pred with | Nothing -> acc | _ -> And(acc, pred)) Everything predlist;; 

(* todo: lots of code overlap in these functions. should unify *)
(* removes the packet-in atom (since that's meaningless here). 
   returns the var the old packet was bound to, and the trimmed fmla *)
let rec trim_packet_from_body (body: formula): (string * formula) =
  match body with
    | FTrue -> ("", body)
    | FFalse -> ("", body)
    | FEquals(t1, t2) -> ("", body)
    | FAnd(f1, f2) -> 
      let (var1, trimmed1) = trim_packet_from_body f1 in
      let (var2, trimmed2) = trim_packet_from_body f2 in
      let trimmed = if trimmed1 = FTrue then 
                      trimmed2 
                    else if trimmed2 = FTrue then
                      trimmed1 
                    else
                      FAnd(trimmed1, trimmed2) in
      if (var1 = var2) || var1 = "" then
        (var2, trimmed)
      else if var2 = "" then
        (var1, trimmed)
      else failwith "trim_packet_from_clause: multiple variables used in packet-in"    
    | FNot(f) ->
      let (v, t) = trim_packet_from_body f in
        (v, FNot(t))
    | FOr(f1, f2) -> failwith "trim_packet_from_clause"              
    | FAtom("", "packet-in", [TVar(varstr)]) ->  
      (varstr, FTrue)
    | _ -> ("", body);;    


let policy_of_conjunction (oldpkt: string) (callback: get_packet_handler option) (body: formula): pol = 
  (* can't just say "if action is fwd, it's a fwd clause" because 
     this may be an approximation to an un-compilable clause, which needs
     interpretation at the controller! Instead trust callback to carry that info. *)  
      
  let my_action_list = match callback with
            | Some(f) -> [ControllerAction(f)]            
            | _ -> build_switch_actions oldpkt body in 

  let mypred = build_switch_pred oldpkt body in 

    ITE(mypred,
        Action(my_action_list), 
        Action([]));;
  	
(***************************************************************************************)

(* Side effect: reads current state in XSB *)
(* Throws exception rather than using option type: more granular error result *)
let clause_to_netcore (callback: get_packet_handler option) (cl: clause): pol =   
    match cl.head with
      | FAtom(_, _, _) ->
        let pebody = partial_evaluation cl.body in
        let (oldpkt, trimmedbody) = trim_packet_from_body pebody in 
        (* partial eval may insert disjunctions because of multiple tuples to match 
           so we need to pull those disjunctions up and create multiple policies *)
        (* todo: this is pretty inefficient for large numbers of tuples. do better? *)
        let bodies = disj_to_list (disj_to_top trimmedbody) in 
        (* anything not the old packet is a RESULT variable.
           Remember that we know this clause is packet-triggered, but
           we have no constraints on what gets produced. Maybe a bunch of 
           non-packet variables e.g. +R(x, y, z) ... *)
        fold_left (fun acc body -> 
                    (Union (acc, policy_of_conjunction oldpkt callback body)))
                  (policy_of_conjunction oldpkt callback (hd bodies))
                  (tl bodies)
      | _ -> failwith "clause_to_netcore";;

(* Used to pre-filter controller notifications as much as possible *)
let strip_to_valid (cl: clause): clause =
  cl;; (* TODO: remove atoms that make clause body uncompilable *)

(* return the union of policies for each clause *)
(* Side effect: reads current state in XSB *)
let clauses_to_netcore (clauses: clause list) (callback: get_packet_handler option): pol =
  let clause_pols = match callback with
      | Some(f) -> map (clause_to_netcore callback) (map strip_to_valid clauses)
      | None ->    map (clause_to_netcore None) clauses in
    if length clause_pols = 0 then 
      Filter(Nothing)
    else 
      fold_left (fun acc pol -> Union(acc, pol)) 
        (hd clause_pols) (tl clause_pols);;  

let can_compile_clause (cl: clause): bool =
  try 
    validate_clause cl;
    true
  with (* catch only "expected" exceptions *)
    | IllegalFieldModification(_) -> false
    | IllegalAssignmentViaEquals(_) -> false
    | IllegalAtomMustBePositive(_) -> false
    | IllegalExistentialUse(_) -> false
    | IllegalEquality(_,_) -> false;;

let subtract (biglst: 'a list) (toremove: 'a list): 'a list =
  (filter (fun ele -> not (mem ele toremove)) biglst);;

(* Side effect: reads current state in XSB *)
(* Set up policies for all packet-triggered clauses *)
let program_to_netcore (p: flowlog_program) (callback: get_packet_handler): (pol * pol) =
  let fwd_clauses = (filter is_forward_clause p.clauses) in
  let non_fwd_clauses_by_packets = (filter is_packet_triggered_clause (subtract p.clauses fwd_clauses)) in
  let can_compile_fwd_clauses = (filter can_compile_clause fwd_clauses) in
  let cant_compile_fwd_clauses = subtract fwd_clauses can_compile_fwd_clauses in
    (clauses_to_netcore can_compile_fwd_clauses None,
     clauses_to_netcore (cant_compile_fwd_clauses @ non_fwd_clauses_by_packets) (Some callback));;

let make_policy_stream (p: flowlog_program) =
  printf "Making policy and stream...\n%!";

  (* stream of policies, with function to push new policies on *)
  let (policies, push) = Lwt_stream.create () in

  (* The callback to be invoked when the policy says to send pkt to controller *)
  let rec updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =    
    (* Update the policy via the push function *)
    printf "Packet in on switch %Ld.\n%s\n%!" sw (Packet.to_string pkt);
    let (newfwdpol, newnotifpol) = program_to_netcore p updateFromPacket in
    let newpol = Union(newfwdpol, newnotifpol) in
      push (Some newpol);        
      printf "NEW policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol newpol);
      (* Do nothing more *) 
      [] in
  
    let (initfwdpol, initnotifpol) = program_to_netcore p updateFromPacket in
    let initpol = Union(initfwdpol, initnotifpol) in
      printf "INITIAL policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initpol);
      (* cargo-cult hacking invocation. why call this? *)
      NetCore_Stream.from_stream initpol policies;;


