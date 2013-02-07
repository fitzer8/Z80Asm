MODULE Asm:Xref;
(*###############################################################
# Title:	Xref.Mod
# Func:		This is the code for the Xref function.
# Author:	Fitz.
# Ver:		0.0
# Todo:
#					Add in code to Error if line Count is creater then LineNoLength.
#					Review o_END in DoOpcode and see if anything needs to be done.
#					Finish DoLabelOp - OPT ELSE clause Eval()
#                   Add XREF to assembler.
#                   Add Macro capability.
#                   Add includes
#                   Add ability to assemble to screen.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110503 # Fitz       # Original 0.0.
# 20110428 # Fitz       # 0.2 Object code now output.
# 20110503 # Fitz       # 0.3 Symbol table is now sorted.
# 20110822 # Fitz       # 0.3 Fixed per latest Debug.
###############################################################*)
IMPORT
	Out,
	Ascii,
	Files,
	TextRider,
	BinaryRider,
	Strings,
	CharClass,
	Object,

	B		:= Asm:Buffer,
	Glbl	:= Asm:Globals,

	Utils,
	This, Debug
	;

CONST
	maxStringLen = 256;
	maxSymLen    = 256;
	maxOpcdLen   = 6;
	maxLineNoLen = 8;

	addrStart			= 1;
	B0Start				= 6;
	B1Start				= 8;
	B2Start				= 10;
	B3Start				= 12;
	lineNoStart			= 15;
	lineStart			= 21;

	o_Illegal  =   0;  (* Opcode not found in FindOpcode *)
	o_None     =   1;  (* No operands *)
	o_One      =   2;  (* One byte immediate operand *)
	o_Two      =   3;  (* Two byte immediate operand *)
	o_InrDcr   =   4;  (* INR or DCR instruction *)
	o_Arith    =   5;  (* Register to accumulator arithmetic *)
	o_MOV      =   6;  (* MOV instruction *)
	o_MVI      =   7;  (* MVI instruction *)
	o_LXI      =   8;  (* LXI instruction *)
	o_InxDcx   =   9;  (* INX, DCX, and DAD instructions *)
	o_PushPop  =  10;  (* PUSH and POP instructions *)
	o_StaxLdax =  11;  (* STAX and LDAX instructions *)
	o_RST      =  12;  (* RST instruction *)
	o_DB       =  13;  (* DB pseudo-op *)
	o_DW       =  14;  (* DW pseudo-op *)
	o_DS       =  15;  (* DS pseudo-op *)
	o_EQU      = -16;  (* EQU and SET pseudo-ops *)
	o_ORG      = -17;  (* ORG pseudo-op *)
	o_END      =  18;  (* END pseudo-op *)
	o_LIST     = -19;  (* LIST pseudo-op *)
	o_OPT      = -20;  (* OPT pseudo-op *)
	o_ASCII    =  21;  (* ASCII String instead of messing with DB. *)

(*
TYPE
	Integer	= INTEGER;
	Boolean	= BOOLEAN;

	OpcdStr = ARRAY maxOpcdLen OF CHAR;

	OpcdPtr = POINTER TO OpcdRec;
	OpcdRec = RECORD
		name:   OpcdStr;    (* Opcode name *)
		typ:    Integer;    (* Opcode type *)
		parm:   Integer;    (* Opcode parameter *)
		next:   OpcdPtr;    (* Pointer to next opcode entry *)
	END;

	ListPtr = POINTER TO ListRec;
	ListRec = RECORD
		addr:		Glbl.String;		(* Address String *)
		code:		Glbl.String;		(* Code bytes String *)
		line:		Glbl.String;		(* Line number String *)
		labl:		Glbl.String;		(* Label String *)
		opcd:		Glbl.String;		(* Opcode String *)
		parms:		Glbl.String;		(* Parameter String *)
		cmnt:		Glbl.String;		(* Comment String if any *)
	END;

	XrefEntry*		= POINTER TO XrefEntryDesc;
	XrefEntryDesc	= RECORD (Object.ObjectDesc)
		name*		: SymStr;   (* Symbol name *)
		line*		: INTEGER;  (* Line number *)
		next		: XrefEntry;	(* Pointer to next symtab entry *)
		defined*	: Boolean;  (* TRUE if defined on this line *)
	END;

	XrefList*		= LL.LinkedList( XrefEntry );
	XrefIter*		= LL.Iterator( XrefEntry );
*)

VAR
	symTab:				Glbl.XrefEntry;	(* Pointer to first entry in symtab *)

	errFlag:			Glbl.Boolean;	(* TRUE if error occurred this line *)
	errCount:			Glbl.Integer;	(* Total number of errors *)

	listLine:			Glbl.String;	(* Current listing line *)

	listFlag:			Glbl.Boolean;	(* FALSE to suppress listing source *)
	listThisLine:		Glbl.Boolean;	(* TRUE to force listing this line *)
	sourceEnd:			Glbl.Boolean;	(* TRUE when END pseudo encountered *)

	bytStr:				Glbl.String;	(* Buffer for long DB statements *)
	xferAddr:			Glbl.Integer;	(* Transfer address from END pseudo *)
	xferFound:			Glbl.Boolean;	(* TRUE if xfer addr defined w/ END *)

	DBG					: BOOLEAN;
	cmd					: STRING;
	str					: STRING;
	ok					: BOOLEAN;

	TST					: BOOLEAN;

	XrefNtry			: Glbl.XrefEntry;
	XrefList			: Glbl.XrefList;
	SymIter				: Glbl.SymIter;
	Ntry				: Glbl.XrefEntry;

(*
PROCEDURE Error( message: Glbl.String );
BEGIN
	errFlag := TRUE;
	INC( errCount );

	IF pass # 1 THEN
		listThisLine := TRUE;
		wLST.WriteString( "*** Error:  " );
		wLST.WriteString( message );
		wLST.WriteString( " *** " );
		wLST.WriteLn;
		IF cl_Err THEN
			Out.String( "*** Error:  " );
			Out.String( message );
			Out.String( " *** " );
			Out.Ln;
		END;
	END;
END Error;

PROCEDURE ErrorStart( message: Glbl.String );
BEGIN
	errFlag := TRUE;
	INC( errCount );

	IF pass # 1 THEN
		listThisLine := TRUE;
		wLST.WriteString( "*** Error:  " );
		wLST.WriteString( message );
		IF cl_Err THEN
			Out.String( "*** Error:  " );
			Out.String( message );
		END;
	END;
END ErrorStart;

PROCEDURE ErrorEnd;
BEGIN
	errFlag := TRUE;

	IF pass # 1 THEN
		listThisLine := TRUE;
		wLST.WriteString( " *** " );
		wLST.WriteLn;
		IF cl_Err THEN
			Out.String( " *** " );
			Out.Ln;
		END;
	END;
END ErrorEnd;
*)

(*
(*###############################################################
# Title:	Assign
# Func:		Assign object and list files to their file handles.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 201012203 # Fitz       # Original.
###############################################################*)
PROCEDURE FindSym(symName: Glbl.SymStr) : Glbl.XrefEntry;
VAR
	DBG			: BOOLEAN;

	Ntry		: Glbl.XrefEntry;
	found		: Glbl.Boolean;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.Label( "symName" );
		Debug.QString( symName );
		Debug.Nl;
	END;
	found := FALSE;
	
	IF XrefList.Size() > 0 THEN
	Ntry := XrefList.Get( 0 );
	SymIter := XrefList.GetIterator(NIL);
	WHILE SymIter.HasNext() & ~found DO
		Ntry := SymIter.Next();
		IF DBG THEN
			Out.String( "Addr: 0x" );
			Out.Hex( Ntry.value, 4 );
			Out.String( "    name: " ); Out.String( Ntry.name );
			Out.Ln; Out.Ln;
		END;
		IF Strings.Equal( Ntry.name, symName ) THEN
			found := TRUE;
		END;
	END;
	IF DBG & found THEN
		Debug.Debug;
		Debug.String( "Found the symbol." );
		Debug.Nl;
	END;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN Ntry;
END FindSym;

(*###############################################################
# Title:	NextSym
# Func:		Find the symbol that has a lexical value greater
#			than SymStr.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110502 # Fitz       # Original.
###############################################################*)
PROCEDURE NextSym( SymName: Glbl.SymStr ) : LONGINT;
VAR
	DBG			: BOOLEAN;

	Ntry		: Glbl.XrefEntry;
	found		: Glbl.Boolean;
	equal		: Glbl.Boolean;
	idx			: LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.Label( "SymName" );
		Debug.QString( SymName );
		Debug.Nl;
	END;

	found := FALSE;
	equal := FALSE;

	NEW( Ntry );
	Ntry := XrefList.Get( 0 );
	SymIter := XrefList.GetIterator(NIL);
	LOOP
		IF SymIter.HasNext() THEN
			Ntry := SymIter.Next();
		ELSE
			EXIT;
		END;
		IF DBG THEN
			Out.String( "idx: " );
			Out.LongInt( XrefList.IndexOf( Ntry ), 3 );
			Out.String( "    Addr: 0x" );
			Out.Hex( Ntry.value, 4 );
			Out.String( "    Ntry.name: " ); Out.String( Ntry.name );
			Out.String( "    SymName: " ); Out.String( SymName );
			Out.Ln; Out.Ln;
		END;
		IF Strings.Compare( Ntry.name, SymName ) = Strings.greater THEN
			IF DBG THEN
				Out.String( "greater" ); Out.Ln;
			END;
			found := TRUE;
			EXIT;
		END;
		IF Strings.Equal( Ntry.name, SymName ) THEN
			IF DBG THEN
				Out.String( "equal" ); Out.Ln;
			END;
			equal := TRUE;
			EXIT;
		END;
	END;
	idx := XrefList.IndexOf( Ntry );
	IF equal THEN
		idx := -1;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN idx;
END NextSym;
*)
	
(*###############################################################
# Title:	AddSym
# Func:		Assign object and list files to their file handles.
# Author:	Fitz
# Ver:		0.0
#================================================================
	XrefEntry*		= POINTER TO XrefEntryDesc;
	XrefEntryDesc	= RECORD (Object.ObjectDesc)
		name*		: SymStr;   (* Symbol name *)
		line*		: INTEGER;  (* Line number *)
		next		: XrefEntry;	(* Pointer to next symtab entry *)
		defined*	: Boolean;  (* TRUE if defined on this line *)
	END;
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 201012203 # Fitz       # Original.
###############################################################*)
PROCEDURE AddSym( symName: Glbl.SymStr; lineNo: INTEGER; def: BOOLEAN ): BOOLEAN;
VAR
	DBG			: BOOLEAN;

	Ntry		: Glbl.XrefEntry;
	p			: Glbl.XrefEntry;
	idx			: LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.QString( "symName", symName );
		Debug.Nl;
	END;

	NEW( p );
	p.line		:= lineNo;
	p.name		:= symName;
	p.defined	:= def;
(*
	idx := NextSym( symName );
*)

(*
	IF idx >= 0 THEN
		Ntry := XrefList.Get( idx );
*)
		XrefList.Insert( idx, p );
(*
	END;
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN TRUE;
END AddSym;
	
(*
PROCEDURE RefSym( symName: Glbl.SymStr ): Glbl.XrefEntry;
VAR
	DBG			: BOOLEAN;

	p			: Glbl.XrefEntry;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	p := FindSym( symName );
	IF p = NIL THEN
		p := AddSym( symName );
	END;

	IF ~p.defined THEN
		Out.String( "Symbol '" );
		Out.String( symName );
		Out.String( "' undefined" );
		Out.Ln;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN p;
END RefSym;

(*###############################################################
# Title:	DefSym
# Func:		To update or Add a symbol.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110425 # Fitz       # Original.
###############################################################*)
PROCEDURE DefSym( symName: Glbl.SymStr; val: LONGINT; setSym, equSym: BOOLEAN );
VAR
	DBG     : BOOLEAN;

	p		: Glbl.XrefEntry;
	msg		: Glbl.String;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.Label( "symName" );
		Debug.QString( symName );
		Debug.Nl;
	END;
	IF Strings.Length( symName ) # 0 THEN
		p := FindSym( symName );
		IF ~(Strings.Equal( p.name, symName )) THEN
			p := AddSym( symName );
		END;

		IF DBG THEN
			Debug.Debug;
			Debug.Label( "symName" );
			Debug.QString( symName );
			Debug.Nl;
			Debug.Debug;
			Debug.Label( "val" );
			Debug.Hex( val, 4 );
			Debug.Nl;
			Debug.Debug;
			Debug.Label( "setSym" );
			IF setSym THEN
				Debug.String( "TRUE" );
			ELSE
				Debug.String( "FALSE" );
			END;
			Debug.Nl;
			Debug.Debug;
			Debug.Label( "equSym" );
			IF equSym THEN
				Debug.String( "TRUE" );
			ELSE
				Debug.String( "FALSE" );
			END;
			Debug.Nl;
		END;
		IF ( ~p.defined ) OR ( p.isSet & setSym ) THEN
			p.value   := val;
			p.defined := TRUE;
			p.isSet   := setSym;
			p.equ     := equSym;
		ELSIF p.value # val THEN
			p.multiDef := TRUE;
			Strings.Assign( 'Symbol: "', msg );
			Strings.Append( symName, msg );
			Strings.Append( '" multiply defined', msg );
			Error( msg  );
		END; (* IF ( ~p.defined ) OR ( p.isSet & setSym ) THEN *)
	END; (* IF Strings.Length( symName ) # 0 THEN *)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DefSym;

(*###############################################################
# Title:	PrintLineNumber
# Func:		Process the program Arguments.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110206 # Fitz       # Original.
###############################################################*)
PROCEDURE PrintLineNumber( Num: LONGINT );
VAR
	DBG     : BOOLEAN;

	word:    Glbl.String;
	lineStart:	LONGINT;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	(*################################################
	# Because the Line Number length can vary, 
	# Determine where to start the line Number.
	################################################*)
	libc.sprintf( word, "%5d", Num );
	lineStart := lineNoStart + maxLineNoLen - Strings.Length( word );
	Strings.Append( word, listLine );
	Strings.Append( origLine, listLine );
	IF DBG THEN
		Debug.Debug;
		Debug.Label( "Print line no" );
		Debug.QString( listLine );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END PrintLineNumber;

(*###############################################################
# Title:	PrintListLine
# Func:		To print what ever is in listLine.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110426 # Fitz       # Original.
###############################################################*)
PROCEDURE PrintListLine;
VAR
	DBG     : BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Out.String( listLine ); Out.Ln;
	END;

	IF ( listFlag ) OR ( listThisLine ) THEN
		(*################################################
		# Print out the list Line
		################################################*)
		Utils.TrimWhite( listLine );
		wLST.WriteString( listLine );
		wLST.WriteLn;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END PrintListLine;	

PROCEDURE SortSymTab;
VAR
	DBG			: BOOLEAN;

	i,j,t		: Glbl.XrefEntry;
	sorted		: Glbl.Boolean;
	swapped		: Glbl.Boolean;
	temp		: Glbl.XrefEntry;
	Iter1		: Glbl.SymIter;
	Iter2		: Glbl.SymIter;
	e1, e2		: LONGINT;
	idx1, idx2	: LONGINT;
	size		: LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	sorted := FALSE;
	swapped := FALSE;
	i := XrefList.Get( idx1 );
	IF Strings.Equal( i.name, "ORG" ) THEN
		i := XrefList.Remove( 0 );
	END;
	size := XrefList.Size();
	idx1 := 0;
	idx2 := 1;
	LOOP
	IF ~sorted THEN
		i := XrefList.Get( idx1 );
		j := XrefList.Get( idx2 );
		IF DBG THEN
			Debug.Debug;
			Debug.Label( "size" );
			Debug.LInt( size, 3 );
			Debug.Nl;
			Debug.Debug;
			Debug.Label( "i.name" );
			Debug.QString( i.name );
			Debug.Nl;
			Debug.Debug;
			Debug.Label( "j.name" );
			Debug.QString( j.name );
			Debug.Nl;
		END;
		IF Strings.Compare( i.name, j.name ) = Strings.greater THEN
			Out.String( "greater" ); Out.Ln;
			XrefList.Insert( idx1, j );
			j := XrefList.Remove( idx2+1 );
			swapped := TRUE;
		END;
		INC( idx1 );
		INC( idx2 );
		IF ( idx2 > size ) & ~swapped THEN
			EXIT;
		END;
		Out.String( "Something here" ); Out.Ln;
		Out.String( "Something here" ); Out.Ln;

		END;
	END;
	sorted := TRUE;
*)

(*
	i := symTab;
	i := XrefList.Get( 0 );
	i := XrefList.elements.element;
	j := i.next;
      WHILE (j<>NIL) DO BEGIN
         sorted := TRUE;

         WHILE (j<>NIL) DO BEGIN
            IF j^.name < i^.name THEN BEGIN
               temp := i^;
               i^   := j^;
               j^   := temp;

               t       := i^.next;
               i^.next := j^.next;
               j^.next := t;

               sorted := FALSE;
            END;
            j := j^.next;
         END;
         i := i^.next;
         j := i^.next;
      END;
	END;
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END SortSymTab;

PROCEDURE DumpSym( p: Glbl.XrefEntry );
BEGIN
	IF ~p.defined  THEN
   		wLST.WriteString( " U" );
   		Out.String( " U" );
	END;
	IF  p.multiDef THEN
   		wLST.WriteString( " M" );
   		Out.String( " M" );
	END;
	IF  p.isSet    THEN
   		wLST.WriteString( " S" );
   		Out.String( " S" );
	END;
	IF  p.equ      THEN
   		wLST.WriteString( " E" );
   		Out.String( " E" );
	END;
END DumpSym;

PROCEDURE DumpSymTab;
VAR
	DBG     : BOOLEAN;

	p		: Glbl.XrefEntry;
	found	: Glbl.Boolean;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	SortSymTab;

	wLST.WriteLn;
	wLST.WriteLn;
	wLST.WriteLn;
	wLST.WriteLn;
	wLST.WriteString( "Symbol Table" );
	wLST.WriteLn;
	wLST.WriteLn;
	Ntry := XrefList.Get( 0 );
	SymIter := XrefList.GetIterator(NIL);
	WHILE SymIter.HasNext() & ~found DO
		Ntry := SymIter.Next();
		IF Strings.Equal( Ntry.name, "ORG" ) THEN
			Ntry := SymIter.Next();
		END;
		Out.String( "Addr: 0x" );
		Out.Hex( Ntry.value, 4 );
		wLST.WriteString( "Addr: 0x" );
		wLST.WriteHex( Ntry.value, 4 );
		DumpSym( Ntry );
		Out.String( "  " );
		wLST.WriteString( "   " );
		Out.String( "name: " ); Out.String( Ntry.name );
		Out.Ln; Out.Ln;
		wLST.WriteString( Ntry.name );
		wLST.WriteLn;
	END;
	IF DBG THEN
		Debug.Debug;
		Debug.String( "Found the symbol." );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DumpSymTab;
*)

BEGIN
	(*################################################
	# This symbol put in there to prevent conditional
	# processing later to determine if we need an
	# Insert or an Append.
	################################################*)
	XrefList := NEW( Glbl.XrefList );
	NEW( XrefNtry );
	XrefNtry.line := 0H;
	XrefNtry.name := "ORG";
	XrefList.Insert( 0, XrefNtry );

	Out.LongInt( errCount, 5 );
	Out.String( " :  Total Error(s)" );
	Out.Ln;

END Asm:Xref.

(*###############################################################
FFFFFFFFFFFFFFFFFFFFFFFFFF
###############################################################*)

