(*
 *
 * MATH 221 FALL 2016: the Cobra SML project
 *
 * a compiler for a Python-like language (Cobra) 
 * written in Standard ML
 * 
 * See the accompanying .md document.
 *
 * There are two uses for this SML code: 
 * 
 * 1) interactively with the command line
 *
 *     sml cobra.sml
 *
 *
 * 2) or "batch mode" with a command line like
 *
 *     sml cobra.sml example.cob
 *
 * 
 * The first allows you to interact with the various
 * parser and compiler functions (see MAJOR section
 * below). The second instead produces a compiled
 * pseudo-Ha75 program example.pha that can be run
 * with the command
 *
 *     python3 pseudoHa75.py example.pha
 *
 *)

exception SyntaxError
exception Unimplemented

val verbosity = ref true
fun verbose b = (verbosity := b)

(*
 *
 * SYNTAX TREE datatype definitions
 *
 *)

datatype prgm = SCRIPT of (defn list) * stmt

and defn = DEF of string * (string list) * stmt 

and stmt = 
  RETURN of expn
| BLOCK of (stmt list)
| IF of cond * stmt * stmt
| WHILE of cond * stmt
| PRINT of expn
| ASSIGN of string * expn
| PASS
| PCALL of string * (expn list)

and cond = 
  LESS of expn * expn
| EQUAL of expn * expn

and expn = 
  PLUS of expn * expn
| MINUS of expn * expn
| TIMES of expn * expn
| MOD of expn * expn
| DIV of expn * expn
| NUM of int
| LOOKUP of string
| INPUT
| FCALL of string * (expn list)


(*
 *
 * TOKENIZER code
 *
 *)

type tokens = (string list) ref

local
  fun isOp #"*" = true
    | isOp #"/" = true
    | isOp #"%" = true
    | isOp #"+" = true
    | isOp #"-" = true
    | isOp #"<" = true
    | isOp #"=" = true
    | isOp #":" = true
    | isOp #"," = true
    | isOp _    = false
  fun tkzSTART [] = []
    | tkzSTART (c::cs) = 
        if (Char.isDigit c) 
        then tkzNUMBER [c] cs
        else if (Char.isAlpha c)
        then tkzNAME [c] cs
        else if (isOp c)
        then tkzOP (c::cs)
        else if (c = #"(")
        then ("("::tkzSTART cs)
        else if (c = #")")
        then (")"::tkzSTART cs)
		else if (Char.isSpace c)
        then tkzSTART cs
        else (print "Unexpected token.\n";
              print (implode (c::cs));
			  raise SyntaxError)
  and tkzNUMBER ds [] = ((implode (rev ds))::(tkzSTART []))
    | tkzNUMBER ds (c::cs) = if (Char.isDigit c) 
                              then tkzNUMBER (c::ds) cs
                              else ((implode (rev ds))::(tkzSTART (c::cs)))
  and tkzNAME xr [] = ((implode (rev xr))::(tkzSTART []))
    | tkzNAME xr (c::cs) = if (Char.isAlphaNum c)
                            then tkzNAME (c::xr) cs
                            else ((implode (rev xr))::(tkzSTART (c::cs)))
  and tkzOP (#"*"::cs) = ("*"::(tkzSTART cs))
    | tkzOP (#"+"::cs) = ("+"::(tkzSTART cs))
    | tkzOP (#"-"::cs) = ("-"::(tkzSTART cs))
    | tkzOP (#"%"::cs) = ("%"::(tkzSTART cs))
    | tkzOP (#"/"::(#"/"::cs)) = ("//"::(tkzSTART cs))
    | tkzOP (#"<"::cs) = ("<"::(tkzSTART cs))
    | tkzOP (#"="::(#"="::cs)) = ("=="::(tkzSTART cs))
    | tkzOP (#"="::cs) = ("="::(tkzSTART cs))
    | tkzOP (#":"::cs) = (":"::(tkzSTART cs))
    | tkzOP (#","::cs) = (","::(tkzSTART cs))
    | tkzOP _ = raise SyntaxError
in
fun tokenize s = ref (tkzSTART (explode s))
end

fun next tkns = 
  case (!tkns) of
    [] => ""
  | (tok::_) => tok
    	
fun advance tkns = 
  let val tok = next tkns
      val _   = print ("# Processing token "^tok^"\n")
   in 
      if tok = "" 
      then tok
      else (tkns := tl (!tkns); tok)
  end

fun next_is_name tkns = 
  case (explode (next tkns)) of
    (c::cs) => (Char.isAlpha c) andalso 
               (foldl (fn (a,b) => a andalso b) true (map Char.isAlphaNum cs))
  | _       => false
   
fun next_is_number tkns = 
  case (Int.fromString (next tkns)) of
    NONE => false
  | SOME _ => true

fun process_number tkns = 
  if (next_is_number tkns) 
  then
    let val nxt = next tkns
        val (SOME num) = Int.fromString (advance tkns)
     in
        num
    end
  else (print "Expected a number token.\n";
        print ("Instead saw "^(next tkns)^".\n");
        raise SyntaxError)

fun process_name tkns =
  if (next_is_name tkns) 
  then (advance tkns)
  else (print "Expected name token.";
        print ("Instead saw "^(next tkns)^".\n");
        raise SyntaxError)

fun process tok tkns =
  if ((next tkns) = tok) 
  then (advance tkns)
  else (print ("Expected token "^tok^".\n");
        print ("Instead saw "^(next tkns)^".\n");
        raise SyntaxError)

(*
 *
 * PARSER functions
 *
 *
 *)

fun parse_prgm tkns = 
  let val ds = (parse_defns tkns)
      val  s = (parse_stmt tkns)
   in
      (SCRIPT (ds,s))
  end

and parse_defns tkns = 
  case (next tkns) of
    "def" => let val _  = (process "def" tkns)
                 val f  = (process_name tkns)
                 val _  = (process "(" tkns)
                 val xs = (parse_params tkns)
                 val _  = (process ")" tkns)
                 val _  = (process ":" tkns)
                 val s  = (parse_stmt tkns)
				 val ds  = (parse_defns tkns)
              in
                 ((DEF (f,xs,s))::ds)
             end
  | _     => []

and parse_params tkns =
  if (next tkns) = ")" then []
  else let fun parse_names () = 
             if (next tkns) = "," then
                let val _  = process "," tkns
                    val x  = process_name tkns
                    val xs = parse_names () 
                 in
                    (x::xs)
                end
             else
                []
        in
           (process_name tkns)::(parse_names ())
       end

(**************)
(*            *)
(* EXERCISE 1 *)
(*            *)
(**************)
and parse_stmt tkns = 
  case (next tkns) of
    "return" => let val _ = (process "return" tkns)
                    val e = (parse_expn tkns)
                 in 
                    (RETURN e)
                end

  | "begin"  => let val _ = (process "begin" tkns)
                    val ss = (parse_block tkns)
                    val _ = (process "end" tkns)
                 in 
                    (BLOCK ss)
                end

  | "if"     => let val _ =(process "if" tkns)
                    val ss1= (parse_cond tkns)
                    val s1=(process ":" tkns)
                    val s2 = (parse_stmt tkns)
                    val s3 = (process "else" tkns)
                    val s4 = (process ":" tkns)
                    val s5= (parse_stmt tkns)
                in
                   IF(ss1,s2,s5)

              end

  | "while"  => let val _ =(process "while" tkns)
                    val s1=(parse_cond tkns)
                    val s2=(process ":" tkns)
                    val s3=(parse_stmt tkns)
                in
                  WHILE(s1,s3)
              end

  | "print"  => let val _ = (process "print" tkns)
                    val _ = (process "(" tkns)
                    val e = (parse_expn tkns) 
                    val _ = (process ")" tkns)
                in 
                    (PRINT e)
                end

  | "pass"   => (process "pass" tkns; PASS)

  | nme      => let val x = (process_name tkns)
                in if (next tkns) = "=" then
                     let val _ = (process "=" tkns)
                         val e = (parse_expn tkns)
                      in
                         (ASSIGN (x,e))
                     end
                   else
                     let val _  =  (process "(" tkns)
                         val es = (parse_args tkns)
                         val _  =  (process ")" tkns)
                      in
                         (PCALL (x,es))
                     end
                end

and parse_cond tkns = 
  let val e1 = parse_expn tkns
      val co = case (next tkns) of
                 "<"  => process "<" tkns
               | "==" => process "==" tkns
               | t    => (print "Unexpected token for comparison.\n";
                          print ("Saw "^t^".\n");
                          raise SyntaxError)
      val e2 = parse_expn tkns
  in
      if co = "<" then (LESS (e1,e2))
                  else (EQUAL (e1,e2))
  end

and parse_args tkns =
  if (next tkns) = ")" then []
  else let fun parse_expns () = 
             if (next tkns) = "," then
                let val _  = process "," tkns
                    val e  = parse_expn tkns
                    val es = parse_expns () 
                 in
                    (e::es)
                end
             else
                []
        in
           (parse_expn tkns)::(parse_expns ())
       end

and parse_block tkns = 
  case (next tkns) of
    "end" => []
  | _     => let val s  = parse_stmt tkns
                 val ss = parse_block tkns
              in (s::ss)
             end

and parse_expn tkns = 
  parse_sum tkns

and parse_sum tkns = 
  let val prd = parse_product tkns
   in
      parse_rest_of_sum prd tkns
  end

and parse_rest_of_sum sum tkns = 
  case (next tkns) of
    "+" => let val _    = process "+" tkns
               val prd = parse_product tkns
            in
               parse_rest_of_sum (PLUS (sum,prd)) tkns
           end
  | "-" => let val _    = process "-" tkns
               val prd = parse_product tkns
            in
               parse_rest_of_sum (MINUS (sum,prd)) tkns
           end
  |  _  => sum

and parse_product tkns = 
  let val trm = parse_term tkns
   in
      parse_rest_of_product trm tkns
  end

and parse_rest_of_product prd tkns = 
  case (next tkns) of
    "*" => let val _    = process "*" tkns
               val trm = parse_term tkns
            in
               parse_rest_of_product (TIMES (prd,trm)) tkns
           end
  | "%" => let val _    = process "%" tkns
               val trm = parse_term tkns
            in
               parse_rest_of_product (MOD (prd,trm)) tkns
           end
  | "//" => let val _    = process "//" tkns
               val trm = parse_term tkns
            in
               parse_rest_of_product (DIV (prd,trm)) tkns
           end
  |  _  => prd

and parse_term tkns =
  if (next_is_number tkns)
  then (NUM (process_number tkns))
  else if (next_is_name tkns)
  then let val nme = (process_name tkns)
        in 
           if nme = "input"
           then let val _ = (process "(" tkns)
                    val _ = (process ")" tkns)
                 in
                    INPUT
                end
           else if (next tkns) = "(" 
           then let val _ =   (process "(" tkns)
                    val args = (parse_args tkns)
                    val _ =   (process ")" tkns)
                 in
                    (FCALL (nme,args))
                end
           else (LOOKUP nme)
       end
  else case (next tkns) of
         "(" => let val _   = (process "(" tkns)
                    val exp = (parse_expn tkns)
                    val _   = (process ")" tkns)
                 in
                    exp
                end

       |  _  => (print ("Error at "^next tkns);
                 raise SyntaxError)



datatype comp = LT0 | EQ0 | GT0 | GTE0 | LTE0 | NEQ0

datatype ir = ADD of string * string * string
		    | SUB of string * string * string
			| MOV of string * string
			| MUL2 of string * string
			| MOD2 of string * string
			| DIV2 of string * string
            | SET of string * int
			| GET_ARG of (int * string)
			| SET_ARGS of (string list)
            | UNSET_ARGS of int
			| CALL of string
			| GET_RET of string
			| SET_RET of string
			| ENTER
			| LEAVE
			| NOOP
			| STOP
            | GOTO of string
            | GOTOIF of string * comp * string
            | LABEL of string
            | GET of string
            | PUT of string

val tmp_i = ref 0
fun gen_tmp _ = (tmp_i:=(!tmp_i)+1; "tmp"^(Int.toString (!tmp_i)))

val lbl_i = ref 0
fun gen_lbl l = (lbl_i:=(!lbl_i)+1; l^(Int.toString (!lbl_i)))

(**************)
(*            *)
(* EXERCISE 2 *)
(*            *)
(**************)
fun gen_expn x (NUM i)     = [SET (x,i)]
  | gen_expn x (LOOKUP y)  = [MOV (x,y)]
  | gen_expn x (INPUT)     = [GET x]

  | gen_expn x (PLUS (LOOKUP y,LOOKUP z))  = [ADD (x,y,z)]  
  | gen_expn x (MINUS (LOOKUP y,LOOKUP z)) = [SUB (x,y,z)] 

  | gen_expn x (PLUS (e1,e2))  = 
      let val y = gen_tmp ()
          val z = gen_tmp ()
          val a = gen_expn y e1
          val b = gen_expn z e2
          val irm = [ADD (x,y,z)]
      in
          a @ b @ irm
      end
  | gen_expn x (MINUS (e1,e2)) =
      let val y = gen_tmp ()
          val z = gen_tmp ()
          val a = gen_expn y e1
          val b = gen_expn z e2
          val irm = [SUB (x,y,z)]
      in
          a @ b @ irm
      end

  | gen_expn x (TIMES (e,NUM 2)) = (gen_expn x e) @ [MUL2 (x,x)] 
  | gen_expn x (DIV (e,NUM 2))   = (gen_expn x e) @ [DIV2 (x,x)] 
  | gen_expn x (MOD (e,NUM 2))   = 
      let val y   = gen_tmp ()
          val z   = gen_tmp ()
          val ire = gen_expn y e 
          val irm = [DIV2(z,y), MUL2(z,z), SUB(x,y,z)]
      in 
          ire @ irm
      end
  | gen_expn x (TIMES (e1,e2)) = gen_expn x (FCALL ("mul",[e1,e2]))
  | gen_expn x (DIV (e1,e2))   = gen_expn x (FCALL ("div",[e1,e2]))
  | gen_expn x (MOD (e1,e2))   = gen_expn x (FCALL ("mod",[e1,e2]))
  | gen_expn x (FCALL (f,es))  = (gen_args es) @ [CALL f, GET_RET x] @ [UNSET_ARGS (length es)]

and gen_args es = 
      let val tmps = map (fn e => (gen_tmp (), e)) es
          val xs   = map (fn (x,_) => x) tmps
          val ts   = map (fn (t,e) => gen_expn t e) tmps
          val ss   = foldr (op@) [] ts
		  val sas  = [SET_ARGS xs]
      in 
          ss @ sas 
      end

(**************)
(*            *)
(* EXERCISE 3 *)
(*            *)
(**************)
fun gen_stmt (ASSIGN (x,e)) = gen_expn x e
  | gen_stmt PASS = [NOOP]
  | gen_stmt (RETURN e) = 
      let val x  = gen_tmp ()
          val ce = gen_expn x e 
	  in 
          ce @ [SET_RET x, LEAVE] 
      end

  | gen_stmt (WHILE (LESS  (e1,e2), b)) = 
      let val x1   = gen_tmp ()
          val x2   = gen_tmp ()
          val x    = gen_tmp ()
          val hdr  = (gen_expn x1 e1) @ (gen_expn x2 e2) @ [SUB(x,x1,x2)]
          val lt   = gen_lbl "then"
          val la   = gen_lbl "after"
          val cjmp = [GOTOIF(x,LT0,lt)]
          val thn  = [LABEL lt] @ gen_stmt b @ hdr @ cjmp
          val els  = [GOTO la]
      in
          hdr @ cjmp @ els @ thn @ [LABEL la]
      end


  | gen_stmt (WHILE (EQUAL (e1,e2), b)) =
      let val x1   = gen_tmp ()
          val x2   = gen_tmp ()
          val x    = gen_tmp ()
          val hdr  = (gen_expn x1 e1) @ (gen_expn x2 e2) @ [SUB(x,x1,x2)]
          val lt   = gen_lbl "then"
          val la   = gen_lbl "after"
          val cjmp = [GOTOIF(x,EQ0,lt)]
          val thn  = [LABEL lt] @ (gen_stmt b) @ hdr @ cjmp
          val els  = [GOTO la]
      in
          hdr @ cjmp @ els @ thn @ [LABEL la]
      end

  | gen_stmt (IF (LESS  (e1,e2), t, e)) = 
      let val lt   = gen_lbl "then"
          val la   = gen_lbl "after"
          val x1   = gen_tmp ()
          val x2   = gen_tmp ()
          val x    = gen_tmp ()
          val hdr  = (gen_expn x1 e1) @ (gen_expn x2 e2) @ [SUB(x,x1,x2)]
          val cjmp = [GOTOIF(x,LT0,lt)]
          val els  = (gen_stmt e) @ [GOTO la]
          val thn  = [LABEL lt] @ (gen_stmt t) 
	  in 
          hdr @ cjmp @ els @ thn @ [LABEL la]
      end

  | gen_stmt (IF (EQUAL  (e1,e2), t, e)) = 
      let val lt   = gen_lbl "then"
          val la   = gen_lbl "after"
          val x1   = gen_tmp ()
          val x2   = gen_tmp ()
          val x    = gen_tmp ()
          val hdr  = (gen_expn x1 e1) @ (gen_expn x2 e2) @ [SUB(x,x1,x2)]
          val cjmp = [GOTOIF(x,EQ0,lt)]
          val els  = (gen_stmt e) @ [GOTO la]
          val thn  = [LABEL lt] @ (gen_stmt t) 
	  in 
          hdr @ cjmp @ els @ thn @ [LABEL la]
      end

  | gen_stmt (PRINT (LOOKUP x)) = [PUT x]

  | gen_stmt (PRINT e) = 
      let val x  = gen_tmp ()
          val ce = gen_expn x e 
	  in 
          ce @ [PUT x]
      end

  | gen_stmt (PCALL(f,es)) = 
      let val args = gen_args es
		  val call = [CALL f, UNSET_ARGS (length es)]
      in 
          args @ call
      end

  | gen_stmt (BLOCK ss) = gen_blck ss
             
and gen_blck ss = foldr (op@) [] (map gen_stmt ss)

fun has_return [] = false
  | has_return [LEAVE] = true
  | has_return (_::irs) = has_return irs

fun gen_defn (DEF (f,params,s)) =
  let val lbl = [LABEL f]
	  fun get_arg i (x::xs) = (GET_ARG (i,x))::(get_arg (i+1) xs)
        | get_arg _ [] = []
	  val hdr = get_arg 1 params
      val bdy = gen_stmt s
      val rtn = if has_return bdy then [] 
                else [LEAVE]
  in 
      lbl @ [ENTER] @ hdr @ bdy @ rtn
  end

fun gen_defns ds = foldr (op@) [] (map gen_defn ds)

fun gen_prgm (SCRIPT (ds,s)) = 
  ([LABEL "main"] @ (gen_stmt s) @ [STOP] @ (gen_defns ds))

fun ir_to_ha75 (NOOP)          = "\tr0 = 0 # NO-OP\n"
  | ir_to_ha75 (STOP)          = "\tstop\n"
  | ir_to_ha75 (SET (d,c))     = "\t"^d^" = "^(Int.toString(c))^"\n"
  | ir_to_ha75 (MOV (d,s))     = "\t"^d^" = "^s^"\n"
  | ir_to_ha75 (MUL2 (d,s))     = "\t"^d^" = "^s^" * 2\n"
  | ir_to_ha75 (MOD2 (d,s))     = "\t"^d^" = "^s^" % 2\n"
  | ir_to_ha75 (DIV2 (d,s))     = "\t"^d^" = "^s^" // 2\n"
  | ir_to_ha75 (ADD (d,s1,s2)) = "\t"^d^" = "^s1^" + "^s2^"\n"
  | ir_to_ha75 (SUB (d,s1,s2)) = "\t"^d^" = "^s1^" - "^s2^"\n"

  (* I/O instructions*)
  | ir_to_ha75 (GET d)   = "\t"^d^" = get\n"
  | ir_to_ha75 (PUT s)   = "\tput "^s^"\n"

  (* go to instructions *)
  | ir_to_ha75 (LABEL l)           = l^":\n"
  | ir_to_ha75 (GOTO l)            = "\tgo to "^l^"\n"
  | ir_to_ha75 (GOTOIF (s,EQ0,l))  = "\tif "^s^" == 0 go to "^l^"\n"
  | ir_to_ha75 (GOTOIF (s,NEQ0,l)) = "\tif "^s^" != 0 go to "^l^"\n"
  | ir_to_ha75 (GOTOIF (s,LT0,l))  = "\tif "^s^" < 0 go to "^l^"\n"
  | ir_to_ha75 (GOTOIF (s,LTE0,l)) = "\tif "^s^" <= 0 go to "^l^"\n"
  | ir_to_ha75 (GOTOIF (s,GT0,l))  = "\tif "^s^" > 0 go to "^l^"\n"
  | ir_to_ha75 (GOTOIF (s,GTE0,l)) = "\tif "^s^" >= 0 go to "^l^"\n"

  (* callee instructions *) 
  | ir_to_ha75 (ENTER)         = ""
  | ir_to_ha75 (GET_ARG (i,x)) = "\t"^x^" = mem[sp+"^(Int.toString i)^"]\n"
  | ir_to_ha75 (SET_RET x)     = "\tr1 = "^x^"\n"
  | ir_to_ha75 (LEAVE)         = "\treturn\n"

  (* caller instructions *)
  | ir_to_ha75 (SET_ARGS []) = ""
  | ir_to_ha75 (SET_ARGS (x::xs)) = (ir_to_ha75 (SET_ARGS (xs)))^"\tpush "^x^"\n"
  | ir_to_ha75 (GET_RET x) = "\t"^x^" = r1\n"
  | ir_to_ha75 (CALL f) = "\tcall "^f^"\n"
  | ir_to_ha75 (UNSET_ARGS 0) = ""
  | ir_to_ha75 (UNSET_ARGS i) = "\tr0 = pop\n"^(ir_to_ha75 (UNSET_ARGS (i-1)))

(*
 *
 * MAJOR compiler functions
 *
 * parse : string -> prgm
 * parse_source : string -> prgm
 * compile_library : string -> ir list
 * compile_source : string -> ir list
 * make_ha75 : string -> unit
 *
 *)

fun parse s = parse_prgm (tokenize s)

fun parse_source filename = 
  let val f = TextIO.openIn filename
      val stream = (tokenize (TextIO.inputAll f))
      val _ = TextIO.closeIn f
   in 
      parse_prgm stream
  end

fun compile_library filename = 
  let val _   = print ("# Parsing and compiling library "^filename^"...\n")
      val f   = TextIO.openIn filename
      val stream = (tokenize (TextIO.inputAll f))
      val _   = TextIO.closeIn f
      val ds  = (parse_defns stream)
      val irl = (gen_defns ds)
      val _   = print ("# Done.\n")
   in 
      irl
  end

fun compile_source filename =
	let val _ = print ("# Parsing "^filename^"...\n")
        val script = parse_source filename
        val _ = print ("# Done. Generating intermediate code...\n")
    in 
        gen_prgm script 
    end

fun has_lib_function irs = 
  let fun is_lib_call (CALL "mul") = true
        | is_lib_call (CALL "div") = true
        | is_lib_call (CALL "mod") = true
        | is_lib_call _  = false
  in
      foldr (fn (a,b) => a orelse b) false (map is_lib_call irs)
  end

fun make_ha75 inname ir = 
  let fun extract (#"."::(#"c"::(#"o"::(#"b"::_)))) = []
        | extract [] = []
        | extract (c::cs) = c::(extract cs)
      val outname = (implode (extract (explode inname)))^".pha"
      val _ = print ("# Writing Ha75 source "^outname^"...\n")
      val f = TextIO.openOut outname
      val s = foldr (op^) "" (map ir_to_ha75 ir)
      val _ = TextIO.outputSubstr(f,Substring.full s)
      val _ = TextIO.closeOut f
      val _ = print "# Done.\n"
  in
      ()    
  end

(*
 *
 * MAIN script
 *
 * This checks whether you want to work with the compiler
 * interactively, or instead want to compile a .cob source
 * file into psudo-Ha75 .pha source.
 *
 *)

val _ = (Control.Print.printLength := 100)
val _ = (Control.Print.printDepth := 100)

val commandline = CommandLine.arguments()
val _ = if (null commandline) then ()
        else let val filename = hd commandline
                 val _   = verbose true
                 val irs = compile_source filename
                 val _   = verbose false
                 val irl = if (has_lib_function irs) then (compile_library "lib.cobl") else []
                 val _   = verbose true
                 val _   = make_ha75 filename (irs @ irl)
             in
                 OS.Process.exit(OS.Process.success)   
             end

