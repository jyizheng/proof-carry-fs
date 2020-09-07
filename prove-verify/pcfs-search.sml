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

(* The top level proof search program for PCFS *)

structure PCFSSearch = struct

fun print_err s = TextIO.output (TextIO.stdErr, s)

fun error () = 
    (print_err ("Usage: \n  pcfs-search [-i] <path-to-pcfs>  [-decl <decl_file>]" ^ 
		" {-prop <prop> | <uid> <file> <perm> <from> <to>}" ^
		" [{-raw|-cert} <hyp_file1> <hyp_file2> ...]\n");

     print_err ("Find a proof of a proposition from given hypotheses (in BL) and print it to the standard output.\n");
     print_err ("   -i (optional) asks user for help about state conditions that may be essential to the proof.\n");
     print_err ("   <path-to-pcfs> is the path (without trailing /) at which PCFS is mounted. This path may be ignored in some cases (see below).\n");
     print_err ("   -decl <decl_file>: <decl_file> should contain the signature being used. If omitted, declarations are read from <path-to-pcfs>/#config/declarations.\n");
     print_err ("   -prop <prop>: <prop> is the proposition to prove.\n");
     print_err ("   <uid> <file> <perm> <from> <to>: If these parameters are given instead of <prop>, then the proposition proved is:\n");
     print_err ("      (at (says (prim_int2principal <admin>) (atom (may (prim_int2principal <uid>) (prim_str2file <file>) <perm>))) <from> <to>)\n");
     print_err ("      where <admin> is ADMIN's uid, read from <path-to-pcfs>/#config/config-file.\n");
     print_err ("   -raw: <hyp_file1> <hyp_file2> ... contain BL assertions, not certificates.\n");
     print_err ("   -cert: <hyp_file1> <hyp_file2> ... contain PCFS certificates from which assertions must be extracted.\n");
     print_err ("          The certificates are verified with a CA public key read from <path-to-pcfs>/#config/ca-pubkey.pem.\n");
     print_err ("   <hyp_file1> <hyp_file2> ...: Files from which the hypotheses are read.\n");
     
     print_err ("   If -raw, -prop, and -decl are all used, then <path-to-pcfs> is ignored.\n");
     OS.Process.exit(OS.Process.failure))
;

(* Reset the cache of user responses *)
val _ = StatesCheck.reset_cache();

val path_to_pcfs = ref "" ;

val givenDecl = ref false ; (* false means read the declarations from <path-to-pcfs>/#config/declarations *)
val decl_file = ref "" ;

val isProp = ref false ; (* false means K, F, P will be given. True means prop is given *)
val propStr = ref ("") ;
val K_str = ref ("") ;
val F_str = ref ("") ;
val P_str = ref ("") ;
val From_str = ref "" ;
val To_str = ref "" ;

val isRaw = ref false ; (* false means certificates are used, otherwise hypothesis are raw programs *)

val hyp_files = ref ([]: string list) ;


local 
    fun parse_cmdline_hyps L =
	case L of
	    "-raw" :: L' => (isRaw := true; hyp_files := L')
	  | "-cert" :: L' => (isRaw := false; hyp_files := L')
	  | _ => error ()

    fun parse_cmdline_prop L =
	case L of
	    "-prop" :: prop_str :: L' => (isProp := true; propStr := prop_str; parse_cmdline_hyps L')
	  | k :: f :: p :: from :: to :: L' => (isProp := false; K_str := k; 
						F_str:= f; P_str := p; 
						From_str := from; To_str := to;
						parse_cmdline_hyps L')
	  | _ => error ()
		 
    fun parse_cmdline_decl L =
	case L of
	    "-decl" :: df :: L' => (givenDecl := true; decl_file := df; parse_cmdline_prop L')
	  | _ => parse_cmdline_prop L
in

fun parse_cmdline L =
    case L of 
	"-i" :: path :: L' => (StatesCheck.ask_user := true; path_to_pcfs := path; parse_cmdline_decl L')
      |	path :: L' => (StatesCheck.ask_user := false; path_to_pcfs := path; parse_cmdline_decl L')
      | _ => error ()

end (* local *)

val _ = parse_cmdline (CommandLine.arguments()) ;

(* check that path_to_pcfs does not end in a "/" *)
val _ = if ((!path_to_pcfs) = "") then (path_to_pcfs := ".")
	else 
	    let val lastchar = List.last (explode (!path_to_pcfs))
	    in 
		if (lastchar = #"/") 
		then
		    (print_err ("path_to_pcfs cannot end in / \n"); 
		     error() )
		else 
		    (StatesCheck.path_prefix := (!path_to_pcfs);
		     () )
	    end ;

(* Parse declarations from <path_to_pcfs>/#config/declarations if
   givenDecl is false, else parse declarations from !decl_file *)

val decls = 
    let val d_file = case (!givenDecl) of 
			 true => !decl_file
		       | false => (!path_to_pcfs) ^ "/#config/declarations"
    in
	case ((Parser.parse_decls_file d_file) handle IO.Io _ => NONE) of
	    NONE => (print_err ("Warning: Unable to parse declarations from file: " ^ d_file ^ "\n"); [])
	  | SOME decls => decls
    end
;
    

(* parse the prop or the term arguments k, f, p, from, to *)
val prop = ref (Props.p_top) ;
val K = ref (Terms.Loca) ;
val F = ref (Terms.Loca) ;
val P = ref (Terms.Loca) ;
val From = ref (Terms.Loca) ;
val To = ref (Terms.Loca) ;


val _ = case (!isProp) of
	    true => 
	    (case (Parser.parse_prop (!propStr)) of
		 NONE => (print_err ("Unable to parse proposition to check\n"); error())
	       | SOME A => prop := A
	    )
	    
	  | false =>
	    (case (Int.fromString (!K_str)) of
		 NONE => (print_err ("Unable to parse uid of principal to be authorized\n"); error ())
	       | SOME k => (K := Terms.Prim_int2principal (Terms.Prim_int k);
			    F := Terms.Prim_str2file (Terms.Prim_str (!F_str));
			    (case (!P_str) of
				 "read" => P := Terms.Read
			       | "write" => P := Terms.Write
			       | "execute" => P := Terms.Execute
			       | "govern" => P := Terms.Govern
			       | "identity" => P := Terms.Identity
			       | _ => (print_err ("Unknown permission: " ^ (!P_str) ^ "\n"); error ()));
			    case (XmlParser.parse_date (!From_str)) of
				NONE => (print_err ("Unable to parse date from: " ^ (!From_str) ^ "\n"); error())
			      | SOME d_fr => (From := Terms.Prim_date2time (Terms.Prim_date d_fr);
					      case (XmlParser.parse_date (!To_str)) of
						  NONE => (print_err ("Unable to parse date from: " ^ (!From_str) ^ "\n");
							   error())
						| SOME d_to => To := Terms.Prim_date2time (Terms.Prim_date d_to)
					     )
			   )
	    )
						   
;

(* If -prop is not given, we will also need the id of the admin *)

val adminid = ref (Terms.Loca) ;

val _ = if (!isProp) then () (* id of admin is not needed *)
	else 
	    let val config_file = (!path_to_pcfs) ^ "/#config/config-file"
	    in
		case ((PCFSConfig.parse_config_file config_file) handle IO.Io _ => NONE) of
		    NONE => (print_err ("Unable to read ADMIN_UID from " ^ config_file ^ "\n");
			     error ())
		  | SOME i => adminid := Terms.Prim_int2principal (Terms.Prim_int i)
		    
	    end


(* If "-certs" is given as an option, we need the CA's public key *)

val CA = case (!isRaw) of
	     true => NONE (* CA's key is not needed *)
		     
	   | false => let val ca_path = (!path_to_pcfs) ^ "/#config/ca-pubkey.pem"
		      in 
			  case (Crypto.readPubkeyFromFile ca_path) of
			      NONE => (print_err ("Unable to read CA's public key from " ^ ca_path ^ "\n");
				       error ())
			    | SOME k => SOME k
		      end
;


(* A function to print a proofterm, if there is one *)

fun print_proofterm (p: Proofs.pfn Option.option) =
    case p of
	NONE => (print_err "Failed to find a proof\n";
		 error ())
      | SOME pfn => print ((Proofs.makestring_pfn pfn) ^ "\n")


(* Now do the search based on the options *)

val _ = (case (!isRaw, !isProp) of
	     (false, false) => (* The certificates are signed, and the prop is not explicit *)
	     print_proofterm (ProverTop.prove_file_cert_pcfs decls (!hyp_files, Option.valOf CA) 
							      (!adminid, !K,  !F, !P, !From, !To))
	     
	   | (true, true) => (* Here the certificates are raw (unsigned), and the prop is explicit *) 
	     print_proofterm (ProverTop.prove_file_raw decls (!hyp_files) (!prop))

	   | (true, false) => (* Here the certificates are raw (unsigned), but prop is not explicit *)
	     print_proofterm (ProverTop.prove_file_raw_pcfs decls (!hyp_files) (!adminid, !K,  !F, !P, !From, !To))

	   | (false, true) => (* Here the certificates are signed, and the prop is explicit *)
	     print_proofterm (ProverTop.prove_file_cert decls (!hyp_files, Option.valOf CA) (!prop))

	) handle ProverTop.ProverError s => (print_err (s ^ "\n"); error())

	    


end (* struct *)
