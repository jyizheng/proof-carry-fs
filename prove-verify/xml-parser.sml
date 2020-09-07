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

(* A parser for generic signed certificate files. See certs.sml for
details of format etc. *)

structure XMLLrVals = XMLLrValsFun (structure Token = LrParser.Token)

structure XMLLex = XMLLexFun (structure Tokens = XMLLrVals.Tokens)

structure XMLParser =
	Join (structure ParserData = XMLLrVals.ParserData
			structure Lex = XMLLex
			structure LrParser = LrParser)

structure XmlParser = struct
fun invoke lexstream = 
    let fun print_error (s, (linenum, linepos), _ ) =
	    print ("Error in line " ^ (Int.toString linenum) ^ " at position " ^ (Int.toString linepos) ^ ", " ^ s ^ "\n")
    in
	XMLParser.parse (0, lexstream, print_error, ())
    end

fun parse(s:string) = 
    let val lexer = (XMLLex.UserDeclarations.initlex(); XMLParser.makeLexer (fn _ => s ^ "\255"));
	    fun f (lexer ) = 
		let val (result, lexer) = invoke lexer
		in
		    result
		end
    in
	Option.SOME (f(lexer)) handle ParseError => Option.NONE
    end
	
fun parse_cert(s: string) = 
    case parse("\254" ^ s) of
	Option.SOME (XMLCommon.Cert l) => Option.SOME l
      | _ => Option.NONE


fun parse_keymap(s: string) = 
    case parse("\253" ^ s) of
	Option.SOME (XMLCommon.KeyMap l) => Option.SOME l
      | _ => Option.NONE

fun parse_date(s: string) = 
    case parse("\252" ^ s) of
	Option.SOME (XMLCommon.Date l) => Option.SOME l
      | _ => Option.NONE

(* Read entire file as a string -- be careful, this may cause memory exhaustion *)
fun readFile (f: string): string =
    let val inp = BinIO.openIn f
	val s = BinIO.inputAll inp
	val s' = Byte.bytesToString s
    in 
	(BinIO.closeIn(inp); s')
    end


val parse_cert_file = parse_cert o readFile 
val parse_keymap_file = parse_keymap o readFile 
val parse_date_file = parse_date o readFile

end

