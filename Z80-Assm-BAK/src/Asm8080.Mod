MODULE Asm8080;
(*###############################################################
# Title:	Asm8080
# Func:		This is a basic i8080 assembler taken from asm8080.pas.
# Author:	Fitz.  Original by Bruce Tomlin.
# Ver:		0.2
# Todo:
#					Add in code to Error if line Count is creater then LineNoLength.
#					Review o_END in DoOpcode and see if anything needs to be done.
#					Finish DoLabelOp - OPT ELSE clause Eval()
#                   Add XREF to assembler.
#                   Add Macro capability.
#                   Add includes
#                   Add ability to assemble to screen.
#================================================================
# Original comments from PL/1 version. I reformatted them.
#================================================================
/*
	8080 Assembler - Feb 28, 1987 - by Bruce Tomlin

 	3/10/87:Added options for Kern's object format.
			Added listing on/off control.
			Added command line options.

	3/17/87:Fixed expression evaluator and made it have
			operator precedence.
			Changed xfer address output to hold xfer address
			until CodeEnd is called.
			Made a full command line parser.  The following DCL
			command is needed to set up the assembler:
			$ ASM :== $your_disk:[your_directory]ASM8080.EXE

	3/31/87:Fixed bug that would prevent lines after an END
			statement from going into the listing file.
			NOTE:  [this bug NOT fixed]  The command line parser
			will probably choke and gag on a source file spec
			with a '.' typed in as part of a directory name.
			The -l= and -o= may not like directory name periods
			either.
*/

#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110427 # Fitz       # Original 0.1 release.
# 20110428 # Fitz       # 0.2 Object code now output.
# 20110503 # Fitz       # 0.3 Symbol table is now sorted.
###############################################################*)
IMPORT
	libc,
	Out,
	Files,
	TextRider,
	BinaryRider,
	Strings,
	CharClass,

	Asm		:= Asm:Asm,
	B		:= Asm:Buffer,
	Glbl	:= Asm:Globals,
	Xref	:= Asm:Xref,

	OS := OS:ProcessManagement,

	Utils,
	This, Debug
	;

CONST
	maxStringLen = 256;
	maxOpcdLen   = 6;

	addrStart			= 1;
	B0Start				= 6;
	B1Start				= 8;
	B2Start				= 10;
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
		labl:		Glbl.String;		(* Label String *)
	END;

VAR
	symTab:				Glbl.SymEntry;	(* Pointer to first entry in symtab *)
	opcdTab:			OpcdPtr;		(* Opcode table *)
	listRec:			ListPtr;		(* Strings for the listing *)

	locPtr:				LONGINT;     (* Current program address *)
	pass:				Integer;     (* Current assembler pass *)
	errFlag:			Boolean;     (* TRUE if error occurred this line *)
	errCount:			Integer;     (* Total number of errors *)

	line:				Glbl.String;      (* Current line from input file *)
	origLine:			Glbl.String;      (* Original line for the listing *)
	listLine:			Glbl.String;      (* Current listing line *)

	listFlag:			Boolean;     (* FALSE to suppress listing source *)
	listThisLine:		Boolean;     (* TRUE to force listing this line *)
	sourceEnd:			Boolean;     (* TRUE when END pseudo encountered *)

	instr:				ARRAY 4  OF Integer;	(* Current instruction word *)
	instrLen:			Integer;			(* Current instruction length *)

	bytStr:				Glbl.String;      (* Buffer for long DB statements *)
	showAddr:			Boolean;     (* TRUE to show LocPtr on listing *)
	xferAddr:			Integer;     (* Transfer address from END pseudo *)
	xferFound:			Boolean;     (* TRUE if xfer addr defined w/ END *)

	(* Command line parameters *)
	cl_SrcName:			Glbl.String;      (* Source file name *)
	cl_ListName:		Glbl.String;      (* Listing file name *)
	cl_ObjName:			Glbl.String;      (* Object file name *)
	cl_Err:				Boolean;     (* TRUE for errors to screen *)


	res					: Files.Result;

	fASM				: Files.File;
	rASM				: TextRider.Reader;
    
	fLST				: Files.File;
	wLST				: TextRider.Writer;

	fCOM				: Files.File;
	wCOM				: BinaryRider.Writer;

	DBG					: BOOLEAN;
	cmd					: STRING;
	ok					: BOOLEAN;

	TST					: BOOLEAN;

	SymNtry				: Glbl.SymEntry;
	SymList				: Glbl.SymList;
	SymIter				: Glbl.SymIter;
	Ntry				: Glbl.SymEntry;

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
PROCEDURE Assign;
VAR
	DBG     : BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF Files.Exists( cl_ListName ) THEN
		IF DBG THEN
			Debug.Debug;
			Debug.String( "Found file: " );
			Debug.String( cl_ListName );
			Debug.Nl;
		END;
		ok := (OS.system( cmd ) = 0 );
	END;

	fLST := Files.New( cl_ListName, {Files.write}, res );
	IF fLST = NIL THEN
		Out.String( "ERROR:  Can't open file: " );
		Out.String( cl_ListName );
		Out.Ln;
		HALT( 2 );
	END;
	wLST := TextRider.ConnectWriter( fLST );
	
	IF Files.Exists( cl_ObjName ) THEN
		IF DBG THEN
			Debug.Debug;
			Debug.String( "Found file: " );
			Debug.String( cl_ObjName );
			Debug.Nl;
		END;
		ok := (OS.system( cmd ) = 0 );
	END;

	fCOM := Files.New( cl_ObjName, {Files.write}, res );
	IF fCOM = NIL THEN
		Out.String( "ERROR:  Can't open file: " );
		Out.String( cl_ObjName );
		Out.Ln;
		HALT( 2 );
	END;
	wCOM := BinaryRider.ConnectWriter( fCOM );

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END Assign;
	
(*###############################################################
# Title:	AssignSrc
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
PROCEDURE AssignSrc;
VAR
	DBG     : BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.Label( "cl_SrcName" );
		Debug.String( cl_SrcName );
		Debug.Nl;
	END;
	fASM := Files.Old( cl_SrcName, {Files.read}, res );
	IF fASM = NIL THEN
		Out.String( "ERROR:  Can't open file: " );
		Out.String( cl_SrcName );
		Out.Ln;
		HALT( 2 );
	END;
	rASM := TextRider.ConnectReader( fASM );

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END AssignSrc;
	
PROCEDURE Deblank( VAR s: ARRAY OF CHAR );
VAR
	DBG:	BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF Strings.Length( s ) > 0 THEN
		IF DBG THEN
			Debug.Debug;
			Debug.QString( "s", s );
			Debug.Nl;
		END;

		Utils.SkipWhite( s );
		Utils.TrimWhite( s );
		IF DBG THEN
			Debug.Debug;
			Debug.QString( "s", s );
			Debug.Nl;
		END;
	END; (* IF Strings.Length( s ) > 0 THEN *)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END Deblank;

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

PROCEDURE AddOpcode( name: OpcdStr; typ, parm: INTEGER );
VAR
	DBG			: BOOLEAN;

	p			: OpcdPtr;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	NEW( p );

	p^.name := name;
	p^.typ  := typ;
	p^.parm := parm;
	p^.next := opcdTab;

	opcdTab := p;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END AddOpcode;

PROCEDURE FindOpcode( name: OpcdStr; VAR typ, parm: INTEGER );
VAR
	DBG			: BOOLEAN;

	p			: OpcdPtr;
	found		: BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	found := FALSE;
	p := opcdTab;

	WHILE( p # NIL ) & ~found DO
		IF DBG THEN
			Debug.Debug;
			Debug.QString( "name", name );
			Debug.Nl;
			Debug.Debug;
			Debug.QString( "p.name", p^.name );
			Debug.Nl;
		END;
		found := ( p^.name = name );
		IF ~found THEN
			p := p^.next;
		END; (* IF ~found THEN *)
	END;

	IF ~found THEN
		typ  := o_Illegal;
		parm := 0;
	ELSE
		typ  := p^.typ;
		parm := p^.parm;
	END;
	IF DBG THEN
		IF typ = o_Illegal THEN
			Debug.Debug;
			Debug.String( "typ is o_Illegal" );
			Debug.Nl;
		END;
		Debug.Debug;
		Debug.Hex( "parm", parm, 2 );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END FindOpcode;

PROCEDURE InitOpcodes;
VAR
	DBG:			BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	opcdTab := NIL;

	AddOpcode('NOP' ,o_None,0);
	AddOpcode('RLC' ,o_None,7);
	AddOpcode('RRC' ,o_None,15);
	AddOpcode('RAL' ,o_None,23);
	AddOpcode('RAR' ,o_None,31);
	AddOpcode('DAA' ,o_None,39);
	AddOpcode('CMA' ,o_None,47);
	AddOpcode('RIM' ,o_None,48);
	AddOpcode('STC' ,o_None,55);
	AddOpcode('SIM' ,o_None,56);
	AddOpcode('CMC' ,o_None,63);
	AddOpcode('HLT' ,o_None,118);
	AddOpcode('RNZ' ,o_None,192);
	AddOpcode('RZ'  ,o_None,200);
	AddOpcode('RET' ,o_None,201);
	AddOpcode('RNC' ,o_None,208);
	AddOpcode('RC'  ,o_None,216);
	AddOpcode('RPO' ,o_None,224);
	AddOpcode('XTHL',o_None,227);
	AddOpcode('RPE' ,o_None,232);
	AddOpcode('PCHL',o_None,233);
	AddOpcode('XCHG',o_None,235);
	AddOpcode('RP'  ,o_None,240);
	AddOpcode('DI'  ,o_None,243);
	AddOpcode('RM'  ,o_None,248);
	AddOpcode('SPHL',o_None,249);
	AddOpcode('EI'  ,o_None,251);

	AddOpcode('ADI' ,o_One,198);
	AddOpcode('ACI' ,o_One,206);
	AddOpcode('OUT' ,o_One,211);
	AddOpcode('SUI' ,o_One,214);
	AddOpcode('IN'  ,o_One,219);
	AddOpcode('SBI' ,o_One,222);
	AddOpcode('ANI' ,o_One,230);
	AddOpcode('XRI' ,o_One,238);
	AddOpcode('ORI' ,o_One,246);
	AddOpcode('CPI' ,o_One,254);

	AddOpcode('SHLD',o_Two,34);
	AddOpcode('LHLD',o_Two,42);
	AddOpcode('STA' ,o_Two,50);
	AddOpcode('LDA' ,o_Two,58);
	AddOpcode('JNZ' ,o_Two,194);
	AddOpcode('JMP' ,o_Two,195);
	AddOpcode('CNZ' ,o_Two,196);
	AddOpcode('JZ'  ,o_Two,202);
	AddOpcode('CZ'  ,o_Two,204);
	AddOpcode('CALL',o_Two,205);
	AddOpcode('JNC' ,o_Two,210);
	AddOpcode('CNC' ,o_Two,212);
	AddOpcode('JC'  ,o_Two,218);
	AddOpcode('CC'  ,o_Two,220);
	AddOpcode('JPO' ,o_Two,226);
	AddOpcode('CPO' ,o_Two,228);
	AddOpcode('JPE' ,o_Two,234);
	AddOpcode('CPE' ,o_Two,236);
	AddOpcode('JP'  ,o_Two,242);
	AddOpcode('CP'  ,o_Two,244);
	AddOpcode('JM'  ,o_Two,250);
	AddOpcode('CM'  ,o_Two,252);

	AddOpcode('INR' ,o_InrDcr,4);
	AddOpcode('DCR' ,o_InrDcr,5);

	AddOpcode('ADD' ,o_Arith,128);
	AddOpcode('ADC' ,o_Arith,136);
	AddOpcode('SUB' ,o_Arith,144);
	AddOpcode('SBB' ,o_Arith,152);
	AddOpcode('ANA' ,o_Arith,160);
	AddOpcode('XRA' ,o_Arith,168);
	AddOpcode('ORA' ,o_Arith,176);
	AddOpcode('CMP' ,o_Arith,184);

	AddOpcode('MOV' ,o_MOV,64);

	AddOpcode('MVI' ,o_MVI,6);

	AddOpcode('LXI' ,o_LXI,1);

	AddOpcode('INX' ,o_InxDcx,3);
	AddOpcode('DAD' ,o_InxDcx,9);
	AddOpcode('DCX' ,o_InxDcx,11);

	AddOpcode('POP' ,o_PushPop,193);
	AddOpcode('PUSH',o_PushPop,197);

	AddOpcode('STAX',o_StaxLdax,2);
	AddOpcode('LDAX',o_StaxLdax,10);

	AddOpcode('RST' ,o_RST,0);

	AddOpcode('DB'  ,o_DB,0);
	AddOpcode('ASCII' ,o_ASCII,0);
	AddOpcode('DW'  ,o_DW,0);
	AddOpcode('DS'  ,o_DS,0);

	AddOpcode('='   ,o_EQU,0);
	AddOpcode('EQU' ,o_EQU,0);
	AddOpcode('SET' ,o_EQU,1);

	AddOpcode('ORG' ,o_ORG,0);
	AddOpcode('END' ,o_END,0);
	AddOpcode('LIST',o_LIST,0);
	AddOpcode('OPT' ,o_OPT,0);

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END InitOpcodes;

PROCEDURE FindSym(symName: Glbl.SymStr) : Glbl.SymEntry;
VAR
	DBG			: BOOLEAN;

	Ntry		: Glbl.SymEntry;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.QString( "symName", symName );
		Debug.Nl;
	END;
	Ntry := NIL;
	
	IF SymList.Size() > 0 THEN
		Ntry := SymList.Get( 0 );
		SymIter := SymList.GetIterator(NIL);
		WHILE SymIter.HasNext() DO
			Ntry := SymIter.Next();
			IF DBG THEN
				Out.String( "Addr: 0x" );
				Out.Hex( Ntry.value, 4 );
				Out.String( "    name: " ); Out.String( Ntry.name );
				Out.Ln; Out.Ln;
			END;
			IF Strings.Equal( Ntry.name, symName ) THEN
				IF DBG THEN
					Debug.Debug;
					Debug.String( "Found the symbol." );
					Debug.Nl;
				END;
			END;
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

	Ntry		: Glbl.SymEntry;
	found		: Boolean;
	equal		: Boolean;
	idx			: LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.QString( "SymName", SymName );
		Debug.Nl;
	END;

	found := FALSE;
	equal := FALSE;

	NEW( Ntry );
	Ntry := SymList.Get( 0 );
	SymIter := SymList.GetIterator(NIL);
	LOOP
		IF SymIter.HasNext() THEN
			Ntry := SymIter.Next();
		ELSE
			EXIT;
		END;
		IF DBG THEN
			Out.String( "idx: " );
			Out.LongInt( SymList.IndexOf( Ntry ), 3 );
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
	idx := SymList.IndexOf( Ntry );
	IF equal THEN
		idx := -1;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN idx;
END NextSym;
	
(*###############################################################
# Title:	AddSym
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
PROCEDURE AddSym( symName: Glbl.SymStr ): Glbl.SymEntry;
VAR
	DBG			: BOOLEAN;

	Ntry		: Glbl.SymEntry;
	p			: Glbl.SymEntry;
	found		: Boolean;
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

	found := FALSE;
	NEW( p );
	p.value		:= 0;
	p.name		:= symName;
	p.defined	:= FALSE;
	p.multiDef	:= FALSE;
	p.isSet		:= FALSE;
	p.equ		:= FALSE;
	idx := NextSym( symName );

	IF idx >= 0 THEN
		Ntry := SymList.Get( idx );
		SymList.Insert( idx, p );
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN p;
END AddSym;
	
PROCEDURE RefSym( symName: Glbl.SymStr ): Glbl.SymEntry;
VAR
	DBG			: BOOLEAN;

	p			: Glbl.SymEntry;

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

	p		: Glbl.SymEntry;
	msg		: Glbl.String;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.QString( "symName", symName );
		Debug.Nl;
	END;
	IF Strings.Length( symName ) # 0 THEN
		p := FindSym( symName );
		IF ~(Strings.Equal( p.name, symName )) THEN
			p := AddSym( symName );
		END;

		IF DBG THEN
			Debug.Debug;
			Debug.QString( "symName", symName );
			Debug.Nl;
			Debug.Debug;
			Debug.Hex( "val", val, 4 );
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

PROCEDURE GetWord( VAR word: ARRAY OF CHAR );
VAR
	DBG     : BOOLEAN;

	done		: BOOLEAN;
	str			: ARRAY 2 OF CHAR;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	Deblank( line );
	word[0] := 0X;

	(*################################################
	# If char is a comment char, make it 0X in line.
	################################################*)
	IF Strings.Length( line ) > 0 THEN
		IF line[0] = ';' THEN
			line[0] := 0X;
		END; (* IF line[0] = ';' THEN *)
	END; (* IF Strings.Length( line ) > 0 THEN *)

	(*################################################
	# If length of line is greater than 0.
	################################################*)
	IF Strings.Length( line ) > 0 THEN
		(*################################################
		# If line[0] is not a character, it must be a
		# number or numeric operator ( +|-|*|/ ); Move
		# char into word and delete it from line.
		################################################*)
		IF ~Utils.IsAlphaNum( line[0] ) THEN
			word[0] := line[0];
			word[1] := 0X;
			Strings.Delete( line, 0, 1 );
		(*################################################
		# If line[0] is a character, while line[0] is an
		# alphanum, save it to word.
		################################################*)
		ELSE
			done := FALSE;
			str[1] := 0X;
			WHILE ( Strings.Length( line ) > 0 ) & ~done DO
				str[0] := line[0];
				Strings.Append( str, word );
				Strings.Delete( line, 0, 1 );
				IF Strings.Length( line ) > 0 THEN
					done := ~Utils.IsAlphaNum( line[0] );
				END; (* IF Strings.Length( line ) > 0 THEN *)
			END; (* WHILE ( Strings.Length( line ) > 0 ) & ~done DO *)
		END; (* IF Pos( Upcase( line[0] ), alphaNumeric ) = 0 THEN *)
	END; (* IF Strings.Length( line ) > 0 THEN *)

	IF DBG THEN
		Debug.Debug;
		Debug.QString( "word", word );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END GetWord;

PROCEDURE Expect( expected: Glbl.String );
VAR
	DBG     : BOOLEAN;

	word	: Glbl.String;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	GetWord( word);
	IF DBG THEN
		Debug.Debug;
		Debug.QString( "expected", expected );
		Debug.Nl;
		Debug.Debug;
		Debug.QString( "word", word );
		Debug.Nl;
	END;
	IF ~Strings.Equal( word, expected ) THEN
		Out.String( '*** ERROR: "' );
		Out.String( expected );
		Out.String( '" expected *** ' );
		Out.Ln;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END Expect;


PROCEDURE Comma;
BEGIN
   Expect( ',' );
END Comma;

(* Assumes the number is already in UpperCase.  *)
PROCEDURE EvalNum(Str: Glbl.String): LONGINT;
VAR
	DBG     : BOOLEAN;

	val		: LONGINT;
	evalErr	: Boolean;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	evalErr := FALSE;
	val  := 0;

	val := Utils.AsciiStr2Number( Str );
	IF DBG THEN
		Debug.LInt( "val", val, 3 );
		Debug.Nl;
	END;

	IF evalErr THEN
		val := 0;
		Error('Invalid hexadecimal number');
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN val;
END EvalNum;

PROCEDURE Eval(): LONGINT;
VAR
	DBG     : BOOLEAN;

	word	: Glbl.String;
	val		: LONGINT;
	oldLine	: Glbl.String;

PROCEDURE Factor(): LONGINT;
VAR
	DBG		: BOOLEAN;

	word	: Glbl.String;
	symStr	: Glbl.SymStr;
	sym		: Glbl.SymEntry;
	val		: LONGINT;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	val  := 0;
	GetWord( word );

	IF Strings.Length( word ) = 0           THEN Error( 'Missing operand' )
	ELSIF(( Strings.Equal( word, '.' )) OR ( Strings.Equal( word, '*' ))) THEN val := locPtr;
	ELSIF  Strings.Equal( word, '-' ) THEN val := val - Factor();
	ELSIF  word='+'                THEN val := val + Factor();
	ELSIF  word='~'                THEN val := val - Factor() - 1;
	ELSIF  word='('                THEN
		val := Eval();
		Expect( ')' );
	ELSIF  word = "'" THEN
		IF Strings.Length( line ) = 0 THEN
			Error( 'Missing operand' );
		ELSE
			val := ORD( line[0] );
			Strings.Delete( line, 0, 1 );
			Expect( "'" );
		END;
	ELSIF CharClass.IsNumeric( word[0] ) THEN
		val := EvalNum( word );
	ELSE
		Strings.Assign( word, symStr );
		sym := RefSym( symStr );
		val := sym.value;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN( val );
END Factor;

PROCEDURE Term(): LONGINT;
VAR
	DBG     : BOOLEAN;

	word:    Glbl.String;
	val:     LONGINT;
	oldLine: Glbl.String;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	val := Factor();

	oldLine := line;
	GetWord( word );
	WHILE ( word = '*' ) OR ( word = '/' ) OR ( word = '%' ) DO
		CASE word[0] OF
			'*': val := val  *  Factor();
			| '/': val := val DIV Factor();
			| '%': val := val MOD Factor();
		END;
		oldLine := line;
		GetWord( word );
	END;
	line := oldLine;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN( val );
END Term;

BEGIN (* Beginning of Eval *)
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	val := Term();

	oldLine := line;
	GetWord( word );
	WHILE (word='+') OR (word='-') OR (word='*') OR (word='/') DO
		CASE word[0] OF
			'+': val := val + Term();
			| '-': val := val - Term();
		END;
		oldLine := line;
		GetWord( word );
	END;
	line := oldLine;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	val := val MOD 10000H;
	RETURN val;
END Eval;

PROCEDURE EvalByte(): LONGINT;
VAR
	DBG     : BOOLEAN;

	val: LONGINT;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	val := Eval();

	IF ( val < -128 ) OR ( val > 255 ) THEN
		Error( 'Byte out of range' );
	END;

	val := val MOD 256;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN val;
END EvalByte;

PROCEDURE FindReg(): Integer;
VAR
	DBG		: BOOLEAN;

	reg		: Integer;
	regName	: Glbl.String;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	GetWord( regName );
	Strings.Capitalize( regName );

	reg := 0;
	IF Strings.Equal( regName, "B" ) THEN
		reg := 0;
	ELSIF Strings.Equal( regName, "C" ) THEN
		reg := 1;
	ELSIF Strings.Equal( regName, "D" ) THEN
		reg := 2;
	ELSIF Strings.Equal( regName, "E" ) THEN
		reg := 3;
	ELSIF Strings.Equal( regName, "H" ) THEN
		reg := 4;
	ELSIF Strings.Equal( regName, "L" ) THEN
		reg := 5;
	ELSIF Strings.Equal( regName, "M" ) THEN
		reg := 6;
	ELSIF Strings.Equal( regName, "A" ) THEN
		reg := 7;
	ELSIF Strings.Equal( regName, "BC" ) THEN
		reg := 0;
	ELSIF Strings.Equal( regName, "DE" ) THEN
		reg := 1;
	ELSIF Strings.Equal( regName, "HL" ) THEN
		reg := 2;
	ELSIF Strings.Equal( regName, "SP" ) THEN
		reg := 3;
	ELSIF Strings.Equal( regName, "PSW" ) THEN
		reg := 3;
	ELSIF Strings.Equal( regName, "AF" ) THEN
		reg := 3;
	ELSE
		Out.String( "OOPS: Not a valid register name" );
		Out.Ln;
	END;
	IF DBG THEN
		Debug.Debug;
		Debug.QString( "regName", regName );
		Debug.Nl;
		Debug.Debug;
		Debug.Hex( "reg value", reg, 2 );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN reg;
END FindReg;


PROCEDURE CodeOrg( addr: LONGINT );
BEGIN
	locPtr := addr;
END CodeOrg;

PROCEDURE DoOpcode( typ, parm: Integer );
VAR
   val		: LONGINT;
   hi		: LONGINT;
   lo		: LONGINT;
   reg1		: Integer;
   reg2		: Integer;
   word		: Glbl.String;
   oldLine	: Glbl.String;
   found	: Boolean;
   idx		: LONGINT;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;
	IF DBG THEN
		Debug.Debug;
		Debug.Int( "typ", typ, 3 );
		Debug.Nl;
		Debug.Debug;
		Debug.Hex( "parm", parm, 3 );
		Debug.Nl;
	END;

	CASE typ OF
		o_None:
			instr[0] := parm;
			instrLen := 1;

		| o_One:
			instr[0] := parm;
			instr[1] := SHORT( EvalByte() );
			instrLen := 2;

		| o_Two:
			val := Eval();
			instr[0] := parm;
			instr[1] := SHORT( val MOD 0100H ); (* Low byte first for 8080 family *)
			instr[2] := SHORT( val DIV 0100H );
			instrLen := 3;

		| o_InrDcr:
			reg1     := FindReg();
			instr[0] := SHORT( parm + reg1*8 );
			instrLen := 1;

		| o_Arith:
			reg1     := FindReg();
			instr[0] := SHORT( parm + reg1 );
			instrLen := 1;

		| o_MOV:
			reg1     := FindReg();
			Comma;
			reg2     := FindReg();
			instr[0] := SHORT( parm + reg1*8 + reg2 );
			instrLen := 1;

		| o_MVI:
			reg1     := FindReg();
			Comma;
			instr[0] := SHORT( parm + reg1*8 );
			instr[1] := SHORT( EvalByte() );
			instrLen := 2;

		| o_LXI:
			reg1     := FindReg();
			Comma;
			val := Eval();
			instr[0] := parm + reg1*16;
			instr[1] := SHORT( val MOD 0100H ); (* Low byte first for 8080 family *)
			instr[2] := SHORT( val DIV 0100H );
			instrLen := 3;

		| o_InxDcx:
			reg1     := FindReg();
			instr[0] := parm + reg1*16;
			instrLen := 1;

		| o_PushPop:
			reg1     := FindReg();
			instr[0] := parm + reg1*16;
			instrLen := 1;

		| o_StaxLdax:
			reg1     := FindReg();
			instr[0] := parm + reg1*16;
			instrLen := 1;

		| o_RST:
			val := Eval();
			IF DBG THEN
				Debug.Debug;
				Debug.Hex( "val", val, 2 );
				Debug.Nl;
			END;
			CASE val OF
				0,1,2,3,4,5,6,7:
					val := val * 8;
				| 8,16,24,32,40,48,56:
			ELSE
				Error('Illegal restart number');
				val := 0;
			END;
			instr[0] := SHORT( parm + val );
			instrLen := 1;

		| o_DB:
			oldLine := line;
			GetWord( word );
			IF( word[0] = "'" ) OR ( word[0] = '"' ) THEN
				GetWord( word );
				IF DBG THEN
					Out.String( "I'm here" ); Out.Ln;
					Out.String( "word: " ); Out.Char( word[0] ); Out.Ln;
					Out.String( "word: " ); Out.Hex( SHORT( ORD( word[0] )), 2 ); Out.Ln;
					Out.String( "length of word: " ); Out.Hex( Strings.Length( word ), 2 ); Out.Ln;
				END;
				IF Strings.Length( word ) > 1 THEN
					Error( "It is not a byte." );
				ELSE
					instr[0] := SHORT( ORD( word[0] ));
				END;
				line[0] := 0X;
			ELSE
				line := oldLine;
				instr[0] := SHORT( EvalByte() );
			END;
			instrLen := 1;

		| o_ASCII:
			oldLine := line;
			GetWord( word );
			IF word = "'" THEN
				Strings.FindNext( "'", line, 0, found, reg1 ); (* Need an Int, using reg1. *)
				IF reg1 = 0 THEN
					bytStr := line;
					line   := '';
				ELSE
					Strings.Extract( line, 0, reg1, bytStr );
					Strings.Delete( line, 0, reg1 );
				END;
				instrLen := Strings.Length( bytStr );
				IF DBG THEN
					Debug.Debug;
					Debug.QString( "bytStr", bytStr );
					Debug.Nl;
					Debug.Debug;
					Debug.Hex( "instrLen", instrLen, 2 );
					Debug.Nl;
				END;
			END;
			line[0] := 0X;

		| o_DW:
			val := Eval();
			Utils.Split( val, hi, lo );
			instr[0] := SHORT( lo );
			instr[1] := SHORT( hi );
			instrLen := 2;

		| o_DS:
			val := Eval();

			IF pass = 2 THEN
				Utils.Word2HexStr( locPtr, word );
				B.Replace( word, addrStart, listLine, maxStringLen );
			END;
			idx := 0;
(* FitzFitz *)
			WHILE idx < val DO
				wCOM.WriteChar( CHR( 0 ));
				INC( idx );
			END;

			val := val + locPtr;
			CodeOrg( val );

(*
      o_END:     BEGIN
(* FitzFitz *)
                    oldLine := line;

                    IF Length(GetWord)<>0 THEN BEGIN
                       line := oldLine;
                       val  := Eval;
                       CodeXfer(val);
                       line := Copy(line,1,7) + '(' + Hex4(val) + ')' +
                               Copy(line,14,255);
                    END;

                    sourceEnd := TRUE;
                 END;
*)
		| o_END:
			IF DBG THEN
				Out.String( "END found" );
				Out.Ln;
			END;

	ELSE
		Error( 'Unknown opcode ');
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DoOpcode;

PROCEDURE DoLabelOp( typ, parm: Integer; labl: Glbl.SymStr; InputNo: LONGINT );
VAR
	DBG     : BOOLEAN;

	val:  LONGINT;
	word: Glbl.String;
	len:  LONGINT;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	CASE typ OF
		o_EQU:
			IF DBG THEN
				Out.String( "EQU" ); Out.Ln;
			END;

			IF DBG THEN
				Debug.Debug;
				Debug.QString( "labl", labl );
				Debug.Nl;
			END;

			len := Strings.Length( labl );
			IF( len = 0 ) THEN
				cl_Err := TRUE;
				Error( 'Missing label' )
			ELSE
				val := Eval();
				IF DBG THEN
					Debug.Debug;
					Debug.Hex( "val", val, 4 );
					Debug.Nl;
				END;

				Utils.Word2HexStr( val, listRec.addr );
				IF DBG THEN
					Debug.Debug;
					Debug.QString( "EQU listLine", listLine );
					Debug.Nl;
				END;

				DefSym( labl, val, parm=1, parm=0 );
			END;

		| o_ORG:
			IF DBG THEN
				Out.String( "ORG" ); Out.Ln;
			END;

			CodeOrg( Eval() );
			DefSym(labl,locPtr,FALSE,FALSE);
			showAddr := TRUE;

		| o_LIST:
			IF DBG THEN
				Out.String( "LIST" ); Out.Ln;
			END;

			listThisLine := TRUE;

			IF Strings.Length( labl ) # 0 THEN
				Error( 'Label not allowed' );
			END;

			GetWord( word );
			Strings.Capitalize( word );
			IF DBG THEN
				Debug.Debug;
				Debug.QString( "word", word );
				Debug.Nl;
			END;
			IF word = 'ON'  THEN
				listFlag := TRUE
			ELSIF word = 'OFF' THEN
				listFlag := FALSE
			ELSE
				Error( 'Illegal operand' );
			END;

		| o_OPT:
			Out.String( "OPT" ); Out.Ln;

			listThisLine := TRUE;

			IF Strings.Length( labl ) # 0 THEN
				Error( 'Label not allowed' );
			END;

			GetWord( word );
			IF word = 'LIST'   THEN
				listFlag := TRUE
			ELSIF word = 'NOLIST' THEN
				listFlag := FALSE
			ELSE
				Error( 'Illegal option' );
			END;

		ELSE
			Error( 'Unknown opcode' );
		END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DoLabelOp;

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

(*###############################################################
# Title:	ProcessString
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
PROCEDURE ProcessString;
VAR
	DBG     : BOOLEAN;

	len		: LONGINT;
	idx		: INTEGER;
	word	: Glbl.String;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.QString( "bytStr ", bytStr );
		Debug.Nl;
	END;
	len := Strings.Length( bytStr );
	idx := 0;

	LOOP
		IF DBG THEN
			Debug.Debug;
			Debug.Hex( "len", len, 3 );
			Debug.Nl;
			Debug.Debug;
			Debug.Hex( "idx", idx, 3 );
			Debug.Nl;
		END;
		DEC( len );
		Utils.Byte2HexStr( ORD( bytStr[0] ), word );
		B.Replace( word, B0Start+idx*2, listLine, maxStringLen );
		wCOM.WriteChar( CHR( ORD( bytStr[0] )));
		Strings.Delete( bytStr, 0, 1 );
		INC( idx );
		IF len = 0 THEN
			EXIT;
		END;
		IF idx = 4 THEN
			IF DBG THEN
				Out.String( "listLine: " );
				Out.String( listLine ); Out.Ln;
				Out.String( "Print line here" ); Out.Ln;
			END;
			PrintListLine;
			B.Clear( listLine, maxStringLen );
			idx := 0;
		END;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END ProcessString;	

PROCEDURE DoPass;
VAR
	DBG     : BOOLEAN;

	labl	: Glbl.SymStr;
	opcode	: OpcdStr;
	opcd	: OpcdStr;
	typ		: Integer;
	parm	: Integer;
	word	: Glbl.String;

	InputNo	: LONGINT;		(* line number for listing *)

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	AssignSrc;
	sourceEnd := FALSE;

	IF DBG THEN
		Out.String( 'Pass ' ); Out.Int( pass, 2 ); Out.Ln;
	END;

	CodeOrg( 0 );
	errCount := 0;
	listFlag := TRUE;
	InputNo := 0;

	REPEAT (* Repeat loop inside DoPass *)
		IF Utils.GetLine( rASM, line, InputNo ) THEN
			IF DBG THEN
				Debug.Nl;
				Debug.Debug;
				Debug.LInt( "InputNo", InputNo, 4 );
				Debug.Spaces( 4 );
				Debug.QString( "InputLine", line );
				Debug.Nl;
			END;
			Strings.Assign( line, origLine );

			errFlag      := FALSE;
			instrLen     := 0;
			showAddr     := FALSE;
			listThisLine := listFlag;
(*
			Strings.Assign( '', listLine ); (* 16 blanks *)
*)
			labl[0] := 0X;
			opcode[0] := 0X;

			IF pass = 2 THEN
				B.Clear( listLine, maxStringLen );
				B.Replace( line, lineStart, listLine, maxStringLen );
				libc.sprintf( word, "%5d", InputNo );
				B.Replace( word, lineNoStart, listLine, maxStringLen );
				IF DBG THEN
					Debug.QString( "listLine", listLine );
					Debug.Nl;
				END;
			END;

			IF Strings.Length(line) > 0 THEN
				IF Utils.IsAlphaNum( line[0] ) THEN
					GetWord( labl );
					showAddr := ( Strings.Length( labl ) > 0 );

					IF Strings.Length( line ) > 0 THEN
						IF line[0] = ':' THEN
							Strings.Delete( line, 0, 1 );
						END;
					END;
					IF DBG THEN
						Debug.QString( "labl", labl );
						Debug.Nl;
					END;
					Strings.Assign( labl, listRec.labl );
				END; (* IF Utils.IsAlphaNum( line[0] ) THEN *)

				IF DBG THEN
					Debug.QString( "line", line );
					Debug.Nl;
				END;
			END; (* IF Strings.Length(line) > 0 THEN *)

			IF Strings.Length( line ) > 0 THEN
				GetWord( opcode );
				Strings.Assign( opcode, opcd );
				Strings.Capitalize( opcode );

				IF DBG THEN
					Debug.QString( "line", line );
					Debug.Nl;
					Debug.QString( "opcode", opcode );
					Debug.Nl;
				END;
			END; (* IF Strings.Length(line) > 0 THEN *)

			IF Strings.Length( opcode ) = 0 THEN
				typ := 0;
				DefSym( labl, locPtr, FALSE, FALSE );
			ELSE
				FindOpcode( opcode, typ, parm );
				IF DBG THEN
					Debug.Debug;
					Debug.QString( "opcode", opcode );
					Debug.Nl;
					Debug.Debug;
					Debug.Hex( "typ", typ, 2 );
					Debug.Nl;
					Debug.Debug;
					Debug.Hex( "parm", parm, 2 );
					Debug.Nl;
				END;

				IF typ = o_Illegal THEN
					ErrorStart( 'Illegal opcode "' );
					Out.String( opcode );
					Out.Char( '"' );
					ErrorEnd;
				ELSIF typ <= 0 THEN
					showAddr := FALSE;
					DoLabelOp( typ, parm, labl, InputNo );
				ELSE
					showAddr := TRUE;
					DefSym( labl, locPtr, FALSE, FALSE );
					DoOpcode( typ, parm );
				END;
				IF typ # o_Illegal THEN
					GetWord( word );
					IF Strings.Length( word ) > 0 THEN
						Error( "Too many operands. I'm here" );
					END;
				END; (* IF typ # o_Illegal THEN *)
			END; (* IF Strings.Length( opcode ) = 0 THEN *)

			IF pass = 2 THEN
				(*################################################
				# Skip updating the address for DS as we have
				# already added the amount of storage to the
				# address.
				################################################*)
				IF typ # o_DS THEN
					IF showAddr THEN
						Utils.Word2HexStr( locPtr, word );
						B.Replace( word, addrStart, listLine, maxStringLen );
					END;
				END;

				IF instrLen >= 1 THEN
					Utils.Byte2HexStr( instr[0], word );
					B.Replace( word, B0Start, listLine, maxStringLen );
(* FitzWork *)
					IF typ # o_ASCII THEN
						wCOM.WriteChar( CHR( instr[0] ));
					END;
				END;
				IF instrLen = 2 THEN
					Utils.Byte2HexStr( instr[1], word );
					B.Replace( word, B1Start, listLine, maxStringLen );
					wCOM.WriteChar( CHR( instr[1] ));
				END;
				IF instrLen = 3 THEN
					Utils.Byte2HexStr( instr[2], word );
					B.Replace( word, B2Start, listLine, maxStringLen );
					Utils.Byte2HexStr( instr[1], word );
					B.Replace( word, B1Start, listLine, maxStringLen );
					wCOM.WriteChar( CHR( instr[1] ));
					wCOM.WriteChar( CHR( instr[2] ));
				END;
(*
				IF instrLen > 0 THEN
					FOR i := 1 TO -instrLen DO BEGIN
						IF i<=3 THEN BEGIN
							word := Hex2(ORD(bytStr[i]));
							listLine[i*3+4] := word[1];
							listLine[i*3+5] := word[2];
						END;
					CodeOut(ORD(bytStr[i]));
				END;
*)

				IF typ = o_ASCII THEN
					ProcessString;
				END;

				(*################################################
				# Print out the list Line
				################################################*)
				PrintListLine;

				(* IF listThisLine THEN ListOut; END; *)
			END; (* IF pass = 2 THEN *)

			locPtr := locPtr + ABS(instrLen);
(*
*)			
			Debug.Dec;
		ELSE
			sourceEnd := TRUE;
		END; (* IF Utils.GetLine( rASM, line, InputNo ) THEN *)
	UNTIL(( sourceEnd ) OR ( Strings.Equal( opcode, "END" )));
	IF DBG THEN
		Debug.Debug;
		Debug.String( "END or EOF found" );
		Debug.Nl;
	END;

(*
	IF pass = 2 THEN CodeEnd; END;

	(* Put the lines after the END statement into the listing file   *)
	(* while still checking for listing control statements.  Ignore  *)
	(* any lines which have invalid syntax, etc., because whatever   *)
	(* is found after an END statement should esentially be ignored. *)
	IF pass = 2 THEN
		sourceEnd := FALSE;
		REPEAT (* Repeat loop inside DoPass *)
			IF Utils.GetLine( rASM, line, InputNo ) THEN
				IF DBG THEN
					Debug.Nl;
					Debug.Debug;
					Debug.Label( "InputNo" );
					Debug.LInt( InputNo, 4 );
					Debug.Spaces( 4 );
					Debug.Label( "InputLine" );
					Debug.QString( line );
					Debug.Nl;
				END;

				errFlag      := FALSE;
				listThisLine := listFlag;
				Strings.Assign( '                ', listLine ); (* 16 blanks *)

				IF Strings.Length( line ) > 0 THEN
					IF Utils.IsAlphaNum( line[0] ) THEN
GetWord( word );
(*
               IF Strings.Length( word ) > 0 THEN
                   IF word = 'LIST' THEN
                         listThisLine := TRUE;
                         word := GetWord;

                         IF word = 'ON'  THEN listFlag := TRUE
                         ELSIF word = 'OFF' THEN listFlag := FALSE
                         ELSE                    listThisLine := listFlag;
                         END;
                   ELSIF word = 'OPT' THEN
                         listThisLine := TRUE;
                         word := GetWord;

                         IF word = 'LIST'   THEN listFlag := TRUE
                         ELSIF word = 'NOLIST' THEN listFlag := FALSE
                         ELSE                       listThisLine := listFlag;
                         END;
                   END; (* IF word = 'LIST' THEN *)
               END; (* IF Strings.Length( word ) > 0 THEN *)
*)
					END; (* IF Utils.IsAlphaNum( line[0] ) THEN *)
				END; (* IF Strings.Length( line ) > 0 THEN *)

				IF listThisLine THEN ListOut; END;
			ELSE
				sourceEnd := TRUE;
			END;
		UNTIL( sourceEnd );
	END; (* IF Pass=2 THEN *)
*)

	fASM.Close;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DoPass;

PROCEDURE DumpSym( p: Glbl.SymEntry );
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

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	(* SortSymTab; *)

	wLST.WriteLn;
	wLST.WriteLn;
	wLST.WriteLn;
	wLST.WriteLn;
	wLST.WriteString( "Symbol Table" );
	wLST.WriteLn;
	wLST.WriteLn;
	Ntry := SymList.Get( 0 );
	SymIter := SymList.GetIterator(NIL);
	WHILE SymIter.HasNext() DO
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

PROCEDURE Test;
VAR
	DBG :			BOOLEAN;

	val :			LONGINT;
	word :		Glbl.String;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	Out.String( "I'm testing EvalNum!" ); Out.Ln;
	Strings.Assign( "0FFF5H", word );
	val := EvalNum( word );
	val := val MOD 10000H;
	Out.String( "I entered this with 0FFF5H." );
	Out.Ln;
	Out.String( "The output should be 0FFF5H.  val: " );
	Out.Hex( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 0FFF5H THEN
		Out.String( "We have a problem here! The result should have been 0FFF5H." );
		Out.Ln;
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing EvalNum!" ); Out.Ln;
	Strings.Assign( "10O", word );
	val := EvalNum( word );
	Out.String( "I entered this with 10 Octal." );
	Out.Ln;
	Out.String( "The output should be 0008.  val: " );
	Out.Hex( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 8 THEN
		Out.String( "We have a problem here! The result should have been 8." );
		Out.Ln;
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing EvalNum!" ); Out.Ln;
	Strings.Assign( "10B", word );
	val := EvalNum( word );
	Out.String( "I entered this with 10 Binary." );
	Out.Ln;
	Out.String( "The output should be 0002.  val: " );
	Out.Hex( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 2 THEN
		Out.String( "We have a problem here! The result should have been 2." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "0005H", line );
	val := Eval();
	Out.String( "I entered this with 0005 Hex." );
	Out.Ln;
	Out.String( "The output should be 0005.  val: " );
	Out.Hex( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 5 THEN
		Out.String( "We have a problem here! The result should have been 5." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "0101B", line );
	val := Eval();
	Out.String( "I entered this with 0101 Binary." );
	Out.Ln;
	Out.String( "The output should be 0005.  val: " );
	Out.Hex( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 5 THEN
		Out.String( "We have a problem here! The result should have been 5." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "0101B + 05H", line );
	val := Eval();
	Out.String( "I entered this with 0101B + 05H." );
	Out.Ln;
	Out.String( "The output should be 000A.  val: " );
	Out.Hex( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 10 THEN
		Out.String( "We have a problem here! The result should have been 10." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "5 * 5H", line );
	val := Eval();
	Out.String( "I entered this with 5 * 5H." );
	Out.Ln;
	Out.String( "The output should be 25.  val: " );
	Out.Int( val, 2 );
	Out.Ln;
	Out.Ln;
	IF val # 25 THEN
		Out.String( "We have a problem here! The result should have been 25." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "25 / 05H", line );
	val := Eval();
	Out.String( "I entered this with 25 / 05H." );
	Out.Ln;
	Out.String( "The output should be 5.  val: " );
	Out.Int( val, 2 );
	Out.Ln;
	Out.Ln;
	IF val # 5 THEN
		Out.String( "We have a problem here! The result should have been 5." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "13 % 0AH", line );
	val := Eval();
	Out.String( "I entered this with 13 % 0AH." );
	Out.Ln;
	Out.String( "The output should be 3.  val: " );
	Out.Int( val, 2 );
	Out.Ln;
	Out.Ln;
	IF val # 3 THEN
		Out.String( "We have a problem here! The result should have been 3." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "10H + '1'", line );
	val := Eval();
	Out.String( "I entered this with 10H + '1'." );
	Out.Ln;
	Out.String( "The output should be 41H.  val: " );
	Out.Hex( val, 2 );
	Out.Ln;
	Out.Ln;
	IF val # 41H THEN
		Out.String( "We have a problem here! The result should have been 41H." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "10 * ( 0AH + 2 )", line );
	val := Eval();
	Out.String( "I entered this with 10 * ( 0AH + 2 )." );
	Out.Ln;
	Out.String( "The output should be 120.  val: " );
	Out.Int( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 120 THEN
		Out.String( "We have a problem here! The result should have been 120." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	Out.String( "I'm testing Eval, Term and Factor!" ); Out.Ln;
	Strings.Assign( "~0FH", line );
	val := Eval();
	Out.String( "I entered this with ~0FH." );
	Out.Ln;
	Out.String( "The output should be FFF0H.  val: " );
	Out.Hex( val, 4 );
	Out.Ln;
	Out.Ln;
	IF val # 0FFF0H THEN
		Out.String( "We have a problem here! The result should have been FFF0H." );
		Out.String( "Instead it is: " );
		Out.Hex( val, 4 );
		Out.Ln;
		Out.Ln;
		HALT( 1 );
	END;

	pass := 2;
	Strings.Assign( " .end", line );
	DoPass;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END Test;

BEGIN
	TST := TRUE;
	TST := FALSE;
	IF TST THEN
		Test();
		HALT( 1 );
	END;

	IF Asm.GetOptions( cl_SrcName, cl_ListName, cl_ObjName, cl_Err) THEN
		Asm.ShowOptions;
		HALT( 1 );
	END;

	Assign;

	NEW( listRec );
	symTab    := NIL;
	xferAddr  := 0;
	xferFound := FALSE;
	InitOpcodes;

	(*################################################
	# This symbol put in there to prevent conditional
	# processing later to determine if we need an
	# Insert or an Append.
	################################################*)
	SymList := NEW( Glbl.SymList );
	NEW( SymNtry );
	SymNtry.value := 0H;
	SymNtry.name := "ORG";
	SymList.Insert( 0, SymNtry );

	pass := 1;
	DoPass;

	pass := 2;
	DoPass;

	wLST.WriteLInt( errCount, 5 );
	wLST.WriteString( " :  Total Error(s)" );
	wLST.WriteLn;

	Out.LongInt( errCount, 5 );
	Out.String( " :  Total Error(s)" );
	Out.Ln;

	DumpSymTab;

	fCOM.Close;
	fLST.Close;
END Asm8080.

(*###############################################################
FFFFFFFFFFFFFFFFFFFFFFFFFF
###############################################################*)

