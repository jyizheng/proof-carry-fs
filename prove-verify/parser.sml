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

structure BLLrVals = BLLrValsFun (structure Token = LrParser.Token)

structure BLLex = BLLexFun (structure Tokens = BLLrVals.Tokens)

structure BLParser =
	Join (structure ParserData = BLLrVals.ParserData
			structure Lex = BLLex
			structure LrParser = LrParser)

structure Parser = struct
local
    open ParserCommon
in
fun invoke lexstream = 
    let fun print_error (s, (linenum, linepos), _ ) =
	    print ("Error in line " ^ (Int.toString linenum) ^ " at position " ^ (Int.toString linepos) ^ ", " ^ s ^ "\n")
    in
	BLParser.parse (0, lexstream, print_error, ())
    end

fun parse(s:string) = 
    let val lexer = (BLLex.UserDeclarations.initlex(); BLParser.makeLexer (fn _ => s ^ "\255"));
	    fun f (lexer ) = 
		let val (result, lexer) = invoke lexer
		in
		    result
		end
    in
	Option.SOME (f(lexer)) handle ParseError => Option.NONE
    end
	
fun parse_prop(s: string) = 
    case parse("\254" ^ s) of
	Option.SOME (Prop l) => Option.SOME l
      | _ => Option.NONE


fun parse_decls(s: string) = 
    case parse ("\253" ^ s) of
	Option.SOME (Declarations ds) => Option.SOME ds
      | _ => Option.NONE

fun parse_proofterm(s: string) = 
    case parse("\252" ^ s) of
	Option.SOME (Proofterm l) => Option.SOME l
      | _ => Option.NONE

fun parse_program (s: string) = 
    case parse("\250" ^ s) of
	Option.SOME (Program l) => Option.SOME l
      | _ => Option.NONE

    
fun parse_constraint (s: string) = 
    case parse("\249" ^ s) of
	Option.SOME (Constraint l) => Option.SOME l
      | _ => Option.NONE

fun parse_term (s: string) = 
    case parse("\248" ^ s) of
	Option.SOME (Term l) => Option.SOME l
      | _ => Option.NONE


fun parse_qprocap (s: string) = 
    case parse("\247" ^ s) of
	Option.SOME (Qprocap l) => Option.SOME l
      | _ => Option.NONE

fun parse_substitution (s: string) = 
    case parse("\246" ^ s) of
	Option.SOME (Substitution l) => Option.SOME l
      | _ => Option.NONE

(* Read entire file as a string -- be careful, this may cause memory exhaustion *)
fun readFile (f: string): string =
    if (f <> "-") then
	let val inp = BinIO.openIn f
	    val s = BinIO.inputAll inp
	    val s' = Byte.bytesToString s
	in 
	    (BinIO.closeIn(inp); s')
	end
    else (* read stdIn *)
	let fun recread s = 
		let val snew = TextIO.inputLine (TextIO.stdIn)
		in
		    case snew of 
			NONE => s
		      | SOME s' => recread (s ^ s')
		end
	in
	    recread ""
	end


val parse_prop_file = parse_prop o readFile 
val parse_proofterm_file = parse_proofterm o readFile
val parse_program_file = parse_program o readFile
val parse_decls_file = parse_decls o readFile
val parse_qprocap_file = parse_qprocap o readFile
val parse_substitution_file = parse_substitution o readFile

end end

