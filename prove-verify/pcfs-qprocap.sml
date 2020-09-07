(*
Copyright (c) <2008,2009> <Deepak Garg dg@cs.cmu.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

(* A tool for checking qprocap signatures, and converting them to
   parameterless procaps by substitution *)

structure InjectProcap = struct

fun print_err s = TextIO.output (TextIO.stdErr, s)

(* Return the parent directory of a given path. Assume that path is
   valid and does not end in "/".  Return NONE if path is already
   root.
*)

fun parent_dir (fname: string): string Option.option =
    case fname of 
	"/" => NONE
      | _ =>
	let fun locate_last_slash 0 = 
		if (String.sub (fname, 0) = #"/") then 0 else ~1
	      | locate_last_slash n =
		if (String.sub (fname, n) = #"/") 
		then n else locate_last_slash (n-1)
			    
	    val last_loc = locate_last_slash ((String.size fname) - 1)
	in
	    case last_loc of
		~1 => NONE (* There isn't a slash! This should not happen, unless the pathname is badly formed *)
	      | 0 => SOME "/" (* Last slash is at the beginning *) 
	      | n => SOME (String.substring (fname, 0, n))
	end

(* Recursively get a list of ancestors of a given path. The path must begin with a "/" and not end in one *)
fun ancestors (path) = 
    case parent_dir path of
	NONE => [path] (* path = "/" *)
      | SOME p => (ancestors p) @ [path]

(* Take a list of paths, and create a directory for each of
   them. Ignore paths that already exist as directories. If any path
   exists as a file, signal an error. If any directory cannot be
   created, signal an error *)

fun create_paths [] = true
  | create_paths (p :: pl) =
    let val b = (OS.FileSys.isDir p) handle OS.SysErr _ => false
    in
	case b of
	    true => create_paths pl
	  | false => 
	    (let val _ = print_err ("Trying to create directory: " ^ p ^ "\n")
		 val _ = OS.FileSys.mkDir p
	     in
		 create_paths pl
	     end) handle OS.SysErr _ => false
    end


exception PathIsRoot

fun inject_procap (path_to_pcfs, uid_str, file_str, perm_str) (procap_str) = 
    let val rel_dir_path = case (parent_dir ("/" ^ uid_str ^ file_str)) of
			       NONE => (print_err ("Cannot inject procaps for directory /");
					raise PathIsRoot
				       )
			     | SOME pl => pl
	val paths = ancestors rel_dir_path
	val full_paths = List.map (fn p => path_to_pcfs ^ "/#config/procaps" ^ p) paths
	val b = create_paths full_paths
	val file_path = path_to_pcfs ^ "/#config/procaps/" ^ uid_str ^ file_str ^ ".perm." ^ perm_str
    in
	case b of
	    false => false
	  | true => 
	    (let val ostream = TextIO.openOut file_path
	     in 
		 TextIO.output (ostream, procap_str);
		 TextIO.closeOut (ostream);
		 true
	     end)
	    handle IO.Io _ => false
    end

end (* struct *)
   




structure PCFSQprocaps = struct

fun print_err s = TextIO.output (TextIO.stdErr, s)

fun error () = 
    (print_err ("Usage: \n   pcfs-qprocap <path-to-pcfs> <qprocap-file> [-key <symmetric-key>] [-subst <substitution-file>] [-i]\n");
     print_err ("Parse a qprocap, check its MAC, and print it back to standard output.\n");
     print_err ("Optionally, the qprocap's parameters can be substituted to get a procap\n");
     print_err ("and the resulting procap can be injected into PCFS' procap store.\n");
     print_err ("   <path-to-pcfs>: Path where PCFS is mounted. This may be ignored in some cases (see below).\n");
     print_err ("   <qprocap-file>: File containing the qprocap to process.\n");
     print_err ("   -key <symmetric-key>: Symmetric key to verify the qprocap, and possibly create a new one.\n");
     print_err ("                         Must be exactly 40 hex digits long.\n");
     print_err ("                         If omitted, key is read from <path-to-pcfs>/#config/shared-key.\n");
     print_err ("   -subst <substitution-file>: A substitution from the qprocap's parameters to ground terms.\n");
     print_err ("                               If omitted, no substitution is done.\n");
     print_err ("   -i: Inject the resulting procap into PCFS' procap store. This will be done only if all\n");
     print_err ("       parameters have been substituted.\n");
     print_err ("\n");
     print_err ("If -key is used, and -i is not used, then <path-to-pcfs> is ignored.\n");
     
     OS.Process.exit(OS.Process.failure))
;

val path_to_pcfs = ref "";
val qprocap_file = ref "";
val has_key = ref false;
val key_hex = ref "";
val do_subst = ref false;
val subst_file = ref "";
val inject = ref false;

local 
    fun parse_inject L = 
	case L of
	    "-i" :: [] => inject := true
	  | [] => inject := false
	  | _ => error ()
		 
    fun parse_subst L =
	case L of
	    "-subst" :: sfile :: L' => (do_subst := true; subst_file := sfile ; parse_inject L')
	  | _ => (do_subst:= false; parse_inject L)
		 
    fun parse_key L =
	case L of
	    "-key" :: key :: L' => (has_key := true; key_hex := key; parse_subst L')
	  | _ => (has_key := false ; parse_subst L)
		 
in
val _ = case (CommandLine.arguments()) of
	    pcfs :: qfile :: L => (path_to_pcfs := pcfs; qprocap_file := qfile; parse_key L)
	  | _ => error ()
end	 
;


(* check that path_to_pcfs does not end in a "/" *)
val _ = if ((!path_to_pcfs) = "") then (path_to_pcfs := ".")
	else 
	    let val lastchar = List.last (explode (!path_to_pcfs))
	    in 
		if (lastchar = #"/") 
		then
		    (print_err ("path_to_pcfs cannot end in / \n"); 
		     error() )
		else  () 
	    end ;


(* Parse the symmetric key *)
val symkey = if (not (!has_key)) then 
		 let val key_path = (!path_to_pcfs) ^ "/#config/shared-key"
		 in
		     case ((PCFSConfig.parse_shared_key_file key_path) 
			   handle IO.Io _ => NONE) of 
			 NONE => (print_err ("Unable to read symmetric key from: " ^ key_path ^ "\n");
				  error())
		       | SOME k => k
		 end
	     else (* Key was given directly in hex *)
		 case (Crypto.hexToSymkey (!key_hex)) of
		     NONE => (print_err ("Given key: " ^ (!key_hex) ^ " is not a valid symmetric key.\n");
				  error())
		   | SOME k => k

;

(* Read the qprocap from the file *)

val qprocap_str = (Parser.readFile (!qprocap_file)) 
    handle IO.Io _ => 
	   (print_err ("Unable to read qprocap file: " ^ (!qprocap_file) ^ "\n"); error ())
;

(* Load the qprocap. This will parse it and check its mac *)

val qprocap_parsed = (ProcapsProcess.load_qprocap (qprocap_str, symkey))
    handle ProcapsProcess.BadQprocap s => (print_err s; error ())
;

(*****************************************
Substitute parameters if -subst is given
******************************************)

val qprocap_substituted =
    if (!do_subst) then (* Apply the substitution *)
	let
	    (* parse the substitution *)
	    val subst_opt = (Parser.parse_substitution_file (!subst_file))
		handle IO.Io _ => (print_err ("Unable to read substitution file: " ^ (!subst_file) ^ "\n");
				   error())
	in 
	    case subst_opt of
		NONE => (print_err ("Unable to parse substitution from file: " ^ (!subst_file) ^ "\n");
			 error ())
	      | SOME subst => Procaps.apply_subst subst qprocap_parsed
	end
    else (* Nothing to do here *)
	qprocap_parsed 
	

(* Convert the new qprocap to ordinary form *)

val qprocap_final = Procaps.qprocap_parsed_to_qprocap qprocap_substituted;

(* Get a string representation of the procap, and print it to standard output *)

val final_str = Procaps.makestring_qprocap (qprocap_final, symkey);

val _ = print final_str;

(*****************************************
Inject procap if -i is given
******************************************)

val _ =
    if (!inject) then
	let val (params, (k,f,p,_,_,_)) = qprocap_final 
	in
	    case params of
		(_ :: _) => (print_err ("Cannot inject procap: all parameters have not been substituted.\n");
			     error())
	      | [] => 
		(case k of
		     (Terms.Prim_int2principal (Terms.Prim_int u)) =>
		     let val uid_str = Int.toString u
		     in
			 case f of 
			     (Terms.Prim_str2file (Terms.Prim_str file_str)) =>
			     let val perm_str = case p of
						    Terms.Read => "read"
						  | Terms.Execute => "execute"
						  | Terms.Write => "write"
						  | Terms.Govern => "govern"
						  | Terms.Identity => "identity"
						  |  _ => (print_err ("Permission authorized by procap is unknown.\n");
							   error ())
			     in
				 (if (InjectProcap.inject_procap (!path_to_pcfs, uid_str, file_str, perm_str) (final_str))
				  then
				      print_err ("Successfully injected procap.\n")
				  else
				      print_err ("Failed to inject procap.\n")
				 ) handle InjectProcap.PathIsRoot => print_err ("Failed to inject procap.\n")
			     end
			     
			   | _ => (print_err ("File authorized by procap does not have form prim_str2file \"...\"\n");
				   error ())
		     end
		   | _ => (print_err ("Principal authorized by procap does not have form prim_int2principal <uid>\n");
			   error ())
		)
	end
    else
	()

end (* struct *)

