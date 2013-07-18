open Unix;;
open Printf;;
open Flowlog_Types;;
open Type_Helpers;;
open Flowlog_Thrift_Out;;

let debug = true;;

module Xsb = struct
	
	(* creates a pair channels for talking to xsb, starts xsb, and returns the channels *)
	let start_xsb () : out_channel * in_channel * in_channel =
		let xin_channel, xout_channel, error_channel = Unix.open_process_full "xsb" (Unix.environment ()) in
		(xout_channel, xin_channel, error_channel);;

	let ref_out_ch = ref None;;
	let ref_in_ch = ref None;;
	let ref_err_ch = ref None;;

	let get_ch () : out_channel * in_channel = 
		match !ref_out_ch with
		| None -> let out_ch, in_ch, err_ch = start_xsb () in 
			let _ = ref_out_ch := Some(out_ch) in
			let _ = ref_in_ch := Some(in_ch) in
			let _ = ref_err_ch := Some(err_ch) in
			(out_ch, in_ch);
		| Some(out_ch) -> (match !ref_in_ch with
			|Some(in_ch) -> (out_ch, in_ch);
			| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"););;


	let halt_xsb () : unit = 
		let out_ch, _ = get_ch () in
		output_string out_ch "halt.\n";
		flush out_ch;;

	
	(* because Tim can't find a non-blocking read similar to read-bytes-avail in Racket,
	    this halts XSB, then terminates  *)
	let debug_print_errors_and_exit () : unit =
	  halt_xsb();	    	    
	  let errstr = ref "" in
	  try
	    while true do
            errstr := !errstr ^ (String.make 1 (input_char (match !ref_err_ch with 
                                               | Some(ch) -> ch;
                                               | _ -> raise (End_of_file))));                      
	      done
	  with End_of_file -> Printf.printf "%s\n%!" !errstr; exit(1);;


	(* Prints the XSB listings currently asserted to stdout.
	   This function is useful for confirming that XSB knows what we think it knows. *)
	let debug_print_listings () : unit =
	    Printf.printf "---------------- PRINTING LISTINGS ----------------\n%!";
		let out_ch, in_ch = get_ch () in
		output_string out_ch ("listing.\n"); flush out_ch;

		let next_str = ref (input_line in_ch) in
		  Printf.printf "%s\n%!" !next_str;
		  while not (Type_Helpers.ends_with (String.trim !next_str) "yes") do
			next_str := input_line in_ch;
			Printf.printf "%s\n%!" !next_str;
		  done;
		  Printf.printf "-------------------------------------------------\n%!";;


	(* This takes in a string command (not query, this doesn't deal with the semicolons).
	It writes the command to xsb and returns the resulting text. *)
	let send_assert (str : string) : string =
	    if debug then Printf.printf "send_assert: %s\n%!" str;
		let out_ch, in_ch = get_ch () in
		output_string out_ch (str ^ "\n");
		flush out_ch;		
		let answer = ref "" in
		let next_str = ref "" in		
		while (not (Type_Helpers.ends_with (String.trim !next_str) "yes") && not (Type_Helpers.ends_with (String.trim !next_str) "no")) do		
			next_str := input_line in_ch;

			(* Do not use this: it won't work. But it is useful for debugging situations with weird XSB output. *)
			(*next_str := (!next_str) ^ (String.make 1 (input_char in_ch));*)

            if debug then Printf.printf "DEBUG: send_assert %s getting response. Line was: %s\n%!" str (!next_str);
			answer := (!answer ^ "\n" ^ String.trim !next_str);
		done;
		(* if debug then Printf.printf "send_assert answer: %s\n%!" (String.trim !answer); *)
		String.trim !answer;;


	(* Removes str2 from the end of str1 if its there, otherwise returns str1 *)
	let remove_from_end (str1 : string) (str2 : string) : string = 
		if Type_Helpers.ends_with str1 str2
		then String.sub str1 0 ((String.length str1) - (String.length str2))
		else str1;; 

	(* groups elements of alist into lists of size num except possibly the first one *)
	let rec group (alist : 'a list) (num : int) : ('a list) list = 
		match alist with
		| [] -> [];
		| f :: r -> match group r num with
			| [] -> [[f]];
			| f1 :: r1 -> if List.length f1 < num
						then (f :: f1) :: r1
						else [f] :: (f1 :: r1);;

	let after_equals (str : string) : string =
		let equals_index = try String.index str '=' with Not_found -> -1 in
		String.trim (String.sub str (equals_index + 1) (String.length str - equals_index - 1));;

	(* Takes a string query (thing with semicolon answers), the number of variables involved.
	 It writes the query to xsb and returns a list of lists with all of the results. *)
	let send_query (str : string) (num_vars : int) : (string list) list =
	    if debug then Printf.printf "send_query: %s (%d vars)\n%!" str num_vars;
		let out_ch, in_ch = get_ch () in
		output_string out_ch (str ^ "\n");
		flush out_ch;
		if debug then Printf.printf "query sent...\n%!";
		(* if get | ?- | ?-[hang w/o newline] expect error in error buffer *)

		(*let first_line = input_line in_ch in
		if ((ends_with (String.trim first_line) "no") || (ends_with (String.trim first_line) "yes")) then [] else*)
		let answer = ref [] in		
		let next_str = ref (input_line in_ch) in
        
        (* Do not use this: it won't work. But it is useful for debugging situations with weird XSB output. 
           Note the debug_print_errors_and_exit() call---catches error case (which has no endline at end of input) *)
        (*let next_str = ref "" in*)        
		(*while not (Type_Helpers.ends_with !next_str "\n") do
		  next_str := (!next_str) ^ (String.make 1 (input_char in_ch));
		  Printf.printf "next_str=%s\n%!" !next_str;
		  if (Type_Helpers.ends_with !next_str "| ?- | ?-") then debug_print_errors_and_exit();
		done;*)
		
		let counter = ref 0 in
		while not (Type_Helpers.ends_with (String.trim !next_str) "no") do
			if debug then Printf.printf "DEBUG: send_query %s getting response. Line was: %s\n%!" str (!next_str);
			if (!counter mod num_vars = 0) then
			(output_string out_ch ";\n";
			flush out_ch);
			counter := !counter + 1;
			next_str := input_line in_ch;
			answer := (remove_from_end (String.trim !next_str) "no") :: !answer;
			(* TODO Beware: if num_vars is wrong, this will freeze. Can we improve? *)
		done;
		if debug then Printf.printf "send_query done.\n%!";
		List.map (fun (l : string list) -> List.map after_equals l) (group (List.rev !answer) num_vars);;

end


(* Provides functions for high level communication with XSB. *)
(* Right now ignoring queries. *)
module Communication = struct

	(* assertion, number of answers to expect (number of variables in clause) *)
	(* if this is a query with 0 variables, will call send_assert and thus need to provide [] vs [[]] *)
	let send_message (message : string) (num_ans : int) : (Types.term list) list =
		let answer = (if num_ans > 0 then
		                Xsb.send_query message num_ans 
		              else let yn = Xsb.send_assert message in
		                if (Type_Helpers.ends_with yn "yes") then [[]]
		                else []) in
		List.map (fun (l : string list) -> List.map (fun str -> Types.Constant(str)) l) answer;;

    (* extract the variables from a list of terms *)
    let get_variables_among_terms (terms : Types.term list): Types.term list =
      List.filter (function 
	    | Types.Variable(_) -> true
		| Types.Field_ref(_, _) -> true
		| Types.Constant(_) -> false) terms

	let send_relation (rel : Types.relation) (args : Types.term list) (process : string -> string -> string) : (Types.term list) list =
		let vars = get_variables_among_terms args in
		let args_string = (Type_Helpers.list_to_string Type_Helpers.term_to_string args)  in
		let str = (match rel with
		| Types.PlusRelation(name, _, _) -> process ("+" ^ name) args_string;
		| Types.MinusRelation(name, _, _) -> process ("-" ^ name) args_string;
		| Types.HelperRelation(name, _, _) -> process name args_string;
		| Types.NotifRelation(bb, _, _) -> process (Type_Helpers.blackbox_name bb) args_string;) in
		if debug then print_endline ("sending: " ^ str);
		send_message str (List.length vars);;

    (* what are the query atoms in this clause? *)
	let get_bbqueries (cls : Types.clause) : Types.atom list =
		List.fold_right (fun lit acc -> 
			let a = Type_Helpers.get_atom lit in
			match a with
			| Types.Query(_, _, _) -> a :: acc;
			| _ -> acc;) (Type_Helpers.clause_body cls) [];;

    (* populate helper relation for FL evaluation. we've got the answer from thrift already 
       therefore we have a list of tuples (as string lists) for each atom. *)
	let assert_or_retract_queries (qs : (Types.atom * ((string list) list)) list) (straction: string): unit =	
		List.iter (function (queryatom, ans) -> match queryatom with
			| Types.Query(bb, str, tl) -> List.iter (fun atuple -> 				                             
				                               ignore (send_message (straction^"((" ^ str ^ "/" ^ 
				                               (Type_Helpers.blackbox_name bb) ^ "(" ^ 
				                               (Type_Helpers.list_to_string (fun x -> x) atuple) ^ ")))." ) 0); ()) ans;
			| _ -> raise (Failure "only queries allowed here");) qs;;

	(* don't duplicate the enormous string construction code *)
	let assert_queries (qs : (Types.atom * string list list) list) : unit =
		assert_or_retract_queries qs "assert";;

	let retract_queries (qs : (Types.atom * string list list) list) : unit =
		assert_or_retract_queries qs "retract";;

	let query_relation (rel : Types.relation) (args : Types.argument list) : (Types.term list) list = 
	    (* BB query atoms referenced in any clause of the relation. relation body is a clause list *)
		let queries = List.fold_right (fun cls acc -> (get_bbqueries cls) @ acc) (Type_Helpers.relation_body rel) [] in
		if debug then Printf.printf "  query_relation called. %d query atoms to handle.\n%!" (List.length queries);		          
		(* call thrift and obtain results as (query-term list-of-list-of-string) pair *)
		let query_answers = List.map (fun q -> match q with 
			| Types.Query(bb, str, tl) -> (q, Flowlog_Thrift_Out.doBBquery bb q); 
			| _ -> raise (Failure "this is only for queries")) 
		                      queries in

		assert_queries query_answers;
		let ans = send_relation rel (Type_Helpers.arguments_to_terms args) (fun name args_string -> name ^ "(" ^ args_string ^ ").") in
		retract_queries query_answers;
		if debug then Printf.printf "  query_relation done handling BB query.\n%!";		          
		ans;;

(*
	let query_relation (rel : Types.relation) (args : Types.argument list) : (Types.term list) list =
		if debug then print_endline ("query relation: " ^ (Type_Helpers.relation_name rel) ^ "(" ^ 
			(Type_Helpers.list_to_string Type_Helpers.argument_to_string args) ^ ")");
		let ans = send_relation rel (Type_Helpers.arguments_to_terms args) (fun name args_string -> name ^ "(" ^ args_string ^ ").") in
		if debug then List.iter (fun tl -> Printf.printf "query answer: %s\n%!" (Type_Helpers.list_to_string Type_Helpers.term_to_string tl)) ans;
		ans;;
*)

	let retract_relation (rel : Types.relation) (args : Types.term list) : unit =
		let _ = send_relation rel args (fun name args_string -> 
			"retract((" ^ name ^ "(" ^ args_string ^ "))).") in ();;

	let assert_relation (rel : Types.relation) (args : Types.term list) : unit =
		retract_relation rel args;
		let _ = send_relation rel args (fun name args_string -> 
			"assert((" ^ name ^ "(" ^ args_string ^ "))).") in ();;

	(* Returns x :: l if x not already in l *)
	let add_unique (x : 'a) (l : 'a list) : 'a list = if List.mem x l then l else x :: l;;
	
	(* Same as add_unique but only if x is a Variable *)
	let add_unique_var (t : Types.term) (acc : Types.term list) : Types.term list = 
		match t with
		| Types.Constant(_) -> acc;
		| Types.Variable(_) -> add_unique t acc;
		| Types.Field_ref(_, _) -> add_unique t acc;;
	
	(* Takes a desugared clause (i.e. one whose arguments are all terms and body contains no Field_refs) and
		returns the number of variables in the clause *)
	let get_vars (cls : Types.clause) : Types.term list =
		let args, body = (Type_Helpers.clause_arguments cls, Type_Helpers.clause_body cls) in
		List.fold_right (fun (lit : Types.literal) (acc : Types.term list) -> 
				match Type_Helpers.get_atom(lit) with
				| Types.Equals(t1, t2) -> add_unique_var t1 (add_unique_var t2 acc);
				| Types.Apply(_, tl) -> List.fold_right add_unique_var tl acc;
				| Types.Query(_, _, tl) -> List.fold_right add_unique_var tl acc; 
				| Types.Bool(_) -> acc;) body (List.fold_right add_unique_var (Type_Helpers.arguments_to_terms args) []);;

	let start_clause (cls : Types.clause) : unit =
		if debug then print_endline ("assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).");
		let _ = send_message ("assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).") (List.length (get_vars cls)) in ();;
		

	let start_relation (rel : Types.relation) : unit =
		match Type_Helpers.relation_body rel with
		| [] -> start_clause (Types.HelperClause(Type_Helpers.relation_name rel, Type_Helpers.relation_arguments rel, []));
		| body -> List.iter start_clause body;;

	let start_program (prgm : Types.program) : unit =
		print_endline "starting program.";
		match prgm with Types.Program(_, _, relations) ->
		List.iter start_relation relations;;

end