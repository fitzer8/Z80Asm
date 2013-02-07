MODULE Asm8080;
(*###############################################################
# Title:	Asm8080
# Func:		This is a basic i8080 assembler taken from asm8080.pas.
# Author:	Fitz
# Ver:		0.0
# Todo:
#	Section 1 = DoPass - read and list lines after .end is found.
#					Add in code to Error if line Count is creater then LineNoLength.
#					Finish DoPass
#					Finish EvalByte
#					Finish DoOpcode
#					Finish DoLabelOp - OPT ELSE clause Eval()
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100317 # Fitz       # Original.
###############################################################*)
IMPORT
	ProgramArgs,
	libc,
	Out,
	Ascii,
	Files,
	TextRider,
	BinaryRider,
	Strings,
	CharClass,
	Object,

	B := Lib:Buffer,

	OS := OS:ProcessManagement,

	LL := ADT:LinkedList,

	Utils,
	This, Debug
	;

(*
{R-}
{$M 16384,0,655360}
*)
CONST
   maxStringLen = 256;
   (* maxSymLen    = 16; *)
   maxSymLen    = 256;
   maxOpcdLen   = 5;
   maxLineNoLen = 8;

	addrStart			= 1;
	B1Start				= 6;
	B2Start				= 8;
	B3Start				= 10;
	B4Start				= 12;
	something			= 15;
	lineNoStart			= 21;
	lineStart			= 31;

	alphaNumeric = '1234567890$ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
	numeric      = '1234567890';
	hex          = '0123456789ABCDEF';
	(* white        = 9X' ';  (* A tab plus a space *) *)

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

	regs         = ' B C D E H L M A ';
	regVals      = ' 0 1 2 3 4 5 6 7 ';

	regPairs     = ' B D H SP BC DE HL ';
	regPairVals  = ' 0 1 2 3  0  1  2  ';

	pushRegs     = ' B D H PSW BC DE HL AF ';
	pushRegVals  = ' 0 1 2 3   0  1  2  3  ';

	staxRegs     = ' B D BC DE ';
	staxRegVals  = ' 0 1 0  1  ';

TYPE
	Integer	= INTEGER;
	Boolean	= BOOLEAN;

	String	= ARRAY maxStringLen OF CHAR;
	SymStr	= ARRAY maxSymLen OF CHAR;

	SymEntry  = POINTER TO SymEntryDesc;
	SymEntryDesc  = RECORD (Object.ObjectDesc)
		name:     SymStr;   (* Symbol name *)
		value:    LONGINT;  (* Symbol value *)
		next:     SymEntry;   (* Pointer to next symtab entry *)
		defined:  Boolean;  (* TRUE if defined *)
		multiDef: Boolean;  (* TRUE if multiply defined *)
		isSet:    Boolean;  (* TRUE if defined with SET pseudo *)
		equ:      Boolean;  (* TRUE if defined with EQU pseudo *)
	END;

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
		addr:		String;		(* Address String *)
		code:		String;		(* Code bytes String *)
		line:		String;		(* Line number String *)
		labl:		String;		(* Label String *)
		opcd:		String;		(* Opcode String *)
		parms:		String;		(* Parameter String *)
		cmnt:		String;		(* Comment String if any *)
	END;

	CmdEntry* = POINTER TO CmdEntryDesc;
	CmdEntryDesc = RECORD (Object.ObjectDesc)
		Cmd*:	ARRAY 10 OF CHAR;
		Addr*:	LONGINT;
	END; 
	
	SymbolList* = LL.LinkedList( SymEntry );
	SymbolIter* = LL.Iterator( SymEntry );

	LlEntry* = POINTER TO LlEntryDesc;
	LlEntryDesc = RECORD (Object.ObjectDesc)
		Val*:	LONGINT;
		Instr*:	ARRAY 10 OF CHAR;
	END; 
	
	LlList* = LL.LinkedList( LlEntry );
	LlIter* = LL.Iterator( LlEntry );

VAR
	symTab:				SymEntry;			(* Pointer to first entry in symtab *)
	opcdTab:			OpcdPtr;		(* Opcode table *)
	listRec:			ListPtr;		(* Strings for the listing *)

	locPtr:				LONGINT;     (* Current program address *)
	pass:					Integer;     (* Current assembler pass *)
	errFlag:			Boolean;     (* TRUE if error occurred this line *)
	errCount:			Integer;     (* Total number of errors *)

	line:					String;      (* Current line from input file *)
	origLine:			String;      (* Original line for the listing *)
	listLine:			String;      (* Current listing line *)

	listFlag:			Boolean;     (* FALSE to suppress listing source *)
	listThisLine:	Boolean;     (* TRUE to force listing this line *)
	sourceEnd:		Boolean;     (* TRUE when END pseudo encountered *)

	instr:				ARRAY 4  OF Integer;	(* Current instruction word *)
	instrLen:			Integer;			(* Current instruction length *)

	bytStr:				String;      (* Buffer for long DB statements *)
	showAddr:			Boolean;     (* TRUE to show LocPtr on listing *)
	xferAddr:			Integer;     (* Transfer address from END pseudo *)
	xferFound:		Boolean;     (* TRUE if xfer addr defined w/ END *)

	(* Command line parameters *)
	cl_SrcName:		String;      (* Source file name *)
	cl_ListName:	String;      (* Listing file name *)
	cl_ObjName:		String;      (* Object file name *)
	cl_Err:				Boolean;     (* TRUE for errors to screen *)


	rArg			: TextRider.Reader;

	res				: Files.Result;

	fASM			: Files.File;
	rASM			: TextRider.Reader;
	sASM			: TextRider.Scanner;
	ASM_Name	: ARRAY 256 OF CHAR;
    
	fLST			: Files.File;
	wLST			: TextRider.Writer;
	LST_Name	: ARRAY 256 OF CHAR;

	fCOM			: Files.File;
	wCOM			: BinaryRider.Writer;
	COM_Name	: ARRAY 256 OF CHAR;

	DBG				: BOOLEAN;
	cmd				: STRING;
	str				: STRING;
	ok				: BOOLEAN;

	TST				: BOOLEAN;
(*
   source:       Text;
   object:       Text;
   listing:      Text;
	int: INTEGER;
*)

(*
	e:			CmdEntry;
	Ntry:		CmdEntry;
*)

	ll:			LlList;
	e:	LlEntry;
	Ntry:		LlEntry;
	Iter:		LlIter;

	SymTab:		SymbolList;
	SymIter:	SymbolIter;

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

	argc    : LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
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

	argc    : LONGINT;

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

	i:		INTEGER;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF Strings.Length( s ) > 0 THEN
		IF DBG THEN
			Debug.Debug;
			Debug.Label( "s" );
			Debug.QString( s );
			Debug.Nl;
		END;

		Utils.SkipWhite( s );
		Utils.TrimWhite( s );
		IF DBG THEN
			Debug.Debug;
			Debug.Label( "s" );
			Debug.QString( s );
			Debug.Nl;
		END;
	END; (* IF Strings.Length( s ) > 0 THEN *)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END Deblank;

(*
	regs         = ' B C D E H L M A ';
	regVals      = ' 0 1 2 3 4 5 6 7 ';

	regPairs     = ' B D H SP BC DE HL ';
	regPairVals  = ' 0 1 2 3  0  1  2  ';
*)

PROCEDURE GetRegVal*( VAR word: ARRAY OF CHAR ): INTEGER;
VAR
	DBG     : BOOLEAN;

	done		: BOOLEAN;
	str			: ARRAY 2 OF CHAR;
	val			: INTEGER;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	Strings.Capitalize( word );
	IF Strings.Length( word ) = 1 THEN
		IF word[ 0 ] = "B" THEN val := 0;
		ELSIF word[ 0 ] = "C" THEN val := 1;
		ELSIF word[ 0 ] = "D" THEN val := 2;
		ELSIF word[ 0 ] = "E" THEN val := 3;
		ELSIF word[ 0 ] = "H" THEN val := 4;
		ELSIF word[ 0 ] = "L" THEN val := 5;
		ELSIF word[ 0 ] = "M" THEN val := 6;
		ELSIF word[ 0 ] = "A" THEN val := 7;
		ELSE
			Out.String( "This is not a valid register name!" );
			Out.Ln;
		END;
	ELSIF Strings.Length( word ) >= 2 THEN
		IF word[ 0 ] = "B" THEN val := 0;		(* BC *)
		ELSIF word[ 0 ] = "D" THEN val := 1;	(* DE *)
		ELSIF word[ 0 ] = "H" THEN val := 2;	(* HL *)
		ELSIF word[ 0 ] = "S" THEN val := 3;	(* SP *)
		ELSIF word[ 0 ] = "P" THEN val := 3;	(* PSW *)
		ELSIF word[ 0 ] = "A" THEN val := 3;	(* AF *)
		ELSE
			Out.String( "This is not a valid register pair name!" );
			Out.Ln;
		END;
	END; (* IF Strings.Length( word ) = 1 THEN *)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN val;
END GetRegVal;

(*
FUNCTION UprCase(s: String): String;
VAR
   i: Integer;

BEGIN
   FOR i := 1 TO Length(s) DO
      IF s[i] IN ['a'..'z'] THEN
         s[i] := UpCase(s[i]);

   UprCase := s;
END;


FUNCTION Hex2(i: Integer): String;
BEGIN
   i := i AND 255;
   Hex2 := Copy(hex,(i SHR  4)+1,1) + Copy(hex,(i AND 15)+1,1);
END;


FUNCTION Hex4(i: Integer): String;
BEGIN
   Hex4 := Hex2(i SHR 8) + Hex2(i AND 255);
END;
*)


PROCEDURE Error( message: String );
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

PROCEDURE ErrorStart( message: String );
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

	p				: OpcdPtr;

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

	p				: OpcdPtr;
	found		: BOOLEAN;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

   found := FALSE;
   p := opcdTab;

   WHILE( p # NIL ) & ~found DO
	    IF DBG THEN
			   Debug.Debug;
				 Debug.Label( "name" );
				 Debug.QString( name );
				 Debug.Nl;
			   Debug.Debug;
				 Debug.Label( "p.name" );
				 Debug.QString( p^.name );
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
   END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END FindOpcode;

(*
PROCEDURE FindOpcode(name: OpcdStr; VAR typ,parm: Integer);
VAR
   p:     OpcdPtr;
   found: Boolean;

BEGIN
   found := FALSE;
   p := opcdTab;

   WHILE (p<>NIL) AND NOT found DO BEGIN
      found := (p^.name = name);
      IF NOT found THEN
         p := p^.next;
   END;

   IF NOT found THEN BEGIN
      typ  := o_Illegal;
      parm := 0;
   END
   ELSE BEGIN
      typ  := p^.typ;
      parm := p^.parm;
   END;
END;
*)

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

	AddOpcode('RST' ,o_RST,199);

	AddOpcode('DB'  ,o_DB,0);
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

PROCEDURE FindSym(symName: SymStr): SymEntry;
VAR
	DBG:			BOOLEAN;

	p:     SymEntry;
	found: Boolean;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	found := FALSE;
	p     := symTab;
	WHILE ( p # NIL ) & ~found DO
		found := ( p^.name = symName );
		IF ~found THEN
			p := p^.next;
		END;
	END;
	IF DBG & found THEN
		Debug.Debug;
		Debug.String( "Found the symbol." );
		Debug.Nl;
	END;
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN p;
END FindSym;

(*###############################################################
# Title:	DumpSymbols
# Func:		To print the CmdList.
# Author:	Fitz
# Ver:		0.0
# Todo:		Nothing known.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100714 # Fitz       # Original.
PROCEDURE DumpSymTab( iter: Glbl.Iterator );
###############################################################*)
PROCEDURE DumpSymbols*;
VAR
	DBG:		BOOLEAN;

(*
	SymIter:	SymbolIter;
*)
	SymNtry:	SymEntry;
	
BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	IF DBG THEN
		SymIter := SymTab.GetIterator( NIL );
		WHILE SymIter.HasNext() DO
			SymNtry := SymIter.Next();
			Out.String( "Addr: 0x" );
			Out.Hex( SymNtry.value, 4 );
			Out.String( "    Name: " ); Out.String( SymNtry.name );
			Out.Ln;
		END;
		Out.Ln;
	END;
*)
	
	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DumpSymbols;

PROCEDURE AddSym( symName: SymStr ): SymEntry;
VAR
	DBG     : BOOLEAN;

	p		: SymEntry;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	p := NIL;

	NEW( p );
	WITH p: SymEntry DO
		p.name     := symName;
		p.value    := 0;
		p.next     := symTab;
		p.defined  := FALSE;
		p.multiDef := FALSE;
		p.isSet    := FALSE;
		p.equ      := FALSE;
	END;
	SymTab.Append( p );
*)

(*
		NEW( LblNtry );
		LblNtry.Addr := Addr;
		Strings.Assign( LblStr, LblNtry.Name );
		DisLL.LblLst.Append( LblNtry );
		DisLL.DumpLblList;
*)

(*
	NEW( p );

	WITH p: SymEntry DO
		p.name     := symName;
		p.value    := 0;
		p.next     := symTab;
		p.defined  := FALSE;
		p.multiDef := FALSE;
		p.isSet    := FALSE;
		p.equ      := FALSE;
	END;

	symTab := p;
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN p;
END AddSym;

PROCEDURE RefSym( symName: SymStr ): SymEntry;
VAR
	DBG:			BOOLEAN;

	p: SymEntry;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	p := NIL;
(*
   p := FindSym( symName );
   IF p = NIL THEN
	    p := AddSym( symName );
	 END;

   IF ~ p^.defined THEN
	    Out.String( "Symbol '" );
			Out.String( symName );
			Out.String( "' undefined" );
			Out.Ln;
      (* Error( 'Symbol "' + symName + '" undefined' ); *)
   END;
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN p;
END RefSym;

(*###############################################################
# Title:	DefSym
# Func:		Create an entry in the Symbol Table.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 201012203 # Fitz       # Original.
###############################################################*)
PROCEDURE DefSym( symName: SymStr; val: LONGINT; setSym, equSym: BOOLEAN );
VAR
	DBG     : BOOLEAN;

	p		: SymEntry;
	msg		: String;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	IF Strings.Length( symName ) # 0 THEN
		p := FindSym( symName );
		IF p = NIL THEN
			p := AddSym( symName );
		END;

		IF ( ~p^.defined ) OR ( p^.isSet & setSym ) THEN
			p^.value   := val;
			p^.defined := TRUE;
			p^.isSet   := setSym;
			p^.equ     := equSym;
		ELSIF p^.value # val THEN
			p^.multiDef := TRUE;
			Strings.Assign( 'Symbol: "', msg );
			Strings.Append( symName, msg );
			Strings.Append( '" multiply defined', msg );
			Error( msg  );
		END; (* IF ( ~p^.defined ) OR ( p^.isSet & setSym ) THEN *)
	END; (* IF Strings.Length( symName ) # 0 THEN *)
*)

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
	DBG := FALSE;
	DBG := TRUE;
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
		Debug.Label( "word" );
		Debug.QString( word );
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "line" );
		Debug.QString( line );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END GetWord;

(*
FUNCTION GetWord: String;
VAR
   word: String;
   done: Boolean;

BEGIN
   line := Deblank(line);
   word := '';

   IF Length(line)>0 THEN
      IF (line[1]=#12) OR (line[1]=';') THEN
         line := '';

   IF Length(line)>0 THEN BEGIN
      IF Pos(Upcase(line[1]),alphaNumeric)=0 THEN BEGIN
         word := Copy(Line,1,1);
         Delete(line,1,1);
      END
      ELSE BEGIN
         done := FALSE;
         WHILE (Length(line)>0) AND NOT done DO BEGIN
	    word := word + Upcase(line[1]);
            Delete(line,1,1);
	    IF Length(line)>0 THEN
               done := Pos(Upcase(line[1]),AlphaNumeric)=0;
         END;
      END;
   END;

   GetWord := word;
END;
*)

PROCEDURE Expect( expected: String );
VAR
	word		: String;

BEGIN
	GetWord( word);
	IF Strings.Equal( word, expected ) THEN
		Out.String( '*** ERROR: "' );
		Out.String( expected );
		Out.String( '" expected *** ' );
		Out.Ln;
	END;
END Expect;


PROCEDURE Comma;
BEGIN
   Expect( ',' );
END Comma;

(*
PROCEDURE EvalOct(octStr: String): Integer;
VAR
   octVal:  Integer;
   evalErr: Boolean;
   i,n:     Integer;

BEGIN
   evalErr := FALSE;
   octVal  := 0;

   FOR i := 1 TO Length(octStr) DO BEGIN
      n := Pos(octStr[i],'01234567');
      IF n=0 THEN
			   evalErr := TRUE
      ELSE
			   octVal  := octVal*8 + n-1;
			END;			 
   END;

   IF evalErr THEN BEGIN
      octVal := 0;
      Error('Invalid octal number');
   END;
END EvalOct;

PROCEDURE Conv2Word( val: Integer ): Integer;
VAR
	DBG     : BOOLEAN;

	hi, lo :  LONGINT;
	evalErr : Boolean;
	i,n :     Integer;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	Utils.Split( val, hi, lo );
	hi := hi MOD 100H;

	val := hi * 100H + lo;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN val;
END Conv2Word;
*)

(* Assumes the number is already in UpperCase.  *)
PROCEDURE EvalNum(Str: String): LONGINT;
VAR
	DBG     : BOOLEAN;

	val :		  LONGINT;
	evalErr :	Boolean;
	i,n :			Integer;

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
		Debug.Label( "val" );
		Debug.LInt( val, 3 );
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

	word		: String;
	val			:	LONGINT;
	oldLine	: String;

PROCEDURE Factor(): LONGINT;
VAR
	DBG :			BOOLEAN;

	word :		String;
	symStr :	SymStr;
	sym :			SymEntry;
	val :			LONGINT;

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
(*
		Strings.Assign( word, symStr );
		sym := RefSym( symStr );
		val := sym.value;
*)
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN( val );
END Factor;

PROCEDURE Term(): LONGINT;
VAR
	DBG     : BOOLEAN;

	word:    String;
	val:     LONGINT;
	oldLine: String;

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
	byte:	INTEGER;

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

(*
FUNCTION FindReg(regName,regList,valList: String): Integer;
VAR
   p:    Integer;
   reg:  Integer;
   code: Integer;

BEGIN
   p := Pos(' ' + Deblank(regName) + ' ',regList);

   IF p=0 THEN BEGIN
      reg := 0;
      Error('Illegal register "' + Deblank(RegName) + '"');
   END
   ELSE
      Val(Copy(valList,p,2),reg,code);

   FindReg := reg;
END;


PROCEDURE CodeOut(byte: Integer);
BEGIN
   IF pass=2 THEN
      WriteLn(object,Hex2(byte));
END;
*)

PROCEDURE CodeOrg( addr: LONGINT );
BEGIN
	locPtr := addr;
END CodeOrg;

PROCEDURE CodeFlush;
BEGIN
    (* Object file format does not use buffering; no flush needed *)
END CodeFlush;


PROCEDURE CodeEnd;
BEGIN
	CodeFlush;

(*
   IF (pass=2) AND xferFound THEN BEGIN
      WriteLn(object,'$',Hex4(xferAddr));
   END;
*)
END CodeEnd;


(*
PROCEDURE CodeXfer(addr: Integer);
BEGIN
   xferAddr  := addr;
   xferFound := TRUE;
END;
*)

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

	word:    String;
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

PROCEDURE DoOpcode( typ, parm: Integer );
VAR
	val:     LONGINT;
	reg1:    Integer;
	reg2:    Integer;
	word:    String;
	oldLine: String;
	hi, lo :  LONGINT;
	found		: BOOLEAN;
	here		: INTEGER;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;
	IF DBG THEN
		Debug.Debug;
		Debug.Label( "typ" );
		Debug.Int( typ, 3 );
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "parm" );
		Debug.Hex( parm, 3 );
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
			instr[1] := SHORT( val DIV 0100H );
			instr[2] := SHORT( val MOD 0100H );
			instrLen := 3;

		| o_InrDcr:
			GetWord( word );
			reg1 := GetRegVal( word );
			instr[0] := parm + reg1*8;
			instrLen := 1;

		| o_Arith:
			GetWord( word );
			reg1 := GetRegVal( word );
			instr[0] := parm + reg1;
			instrLen := 1;

		| o_MOV:
			GetWord( word );
			reg1 := GetRegVal( word );
			Comma;
			GetWord( word );
			reg2 := GetRegVal( word );
			instr[0] := parm + reg1*8 + reg2;
			instrLen := 1;

		| o_MVI:
			GetWord( word );
			reg1 := GetRegVal( word );
			Comma;
			instr[0] := parm + reg1*8;
			instr[1] := SHORT( EvalByte() );
			instrLen := 2;

		| o_LXI:
			GetWord( word );
			reg1 := GetRegVal( word );
			Comma;
			val      := Eval();
			instr[0] := parm + reg1*16;
			Utils.Split( val, hi, lo );
			instr[1] := SHORT( lo );
			instr[2] := SHORT( hi );
			instrLen := 3;

		| o_InxDcx:
			GetWord( word );
			reg1 := GetRegVal( word );
			instr[0] := SHORT( parm + reg1*16 );
			instrLen := 1;

		| o_PushPop:
			GetWord( word );
			reg1 := GetRegVal( word );
			instr[0] := SHORT( parm + reg1*16 );
			instrLen := 1;

		| o_StaxLdax:
			GetWord( word );
			reg1 := GetRegVal( word );
			instr[0] := SHORT( parm + reg1*16 );
			instrLen := 1;

		| o_RST:
			val := Eval();
			CASE val OF
				0,1,2,3,4,5,6,7:
					val := val * 8;
				(* | 8,16,24,32,40,48,56: ; *)
			ELSE
				Error('Illegal restart number');
				val := 0;
			END;
			instr[0] := SHORT( parm + val );
			instrLen := 1;

		| o_DB:
			oldLine := line;
			GetWord( word ); (* Get the "'" if it is a quoted character *)
			IF word = "'" THEN
				IF DBG THEN
					Debug.Debug;
					Debug.Label( "line" );
					Debug.QString( line );
					Debug.Nl;
				END;
				Strings.FindNext( "'", line, 0, found, here );
				IF here # 0 THEN
					line[ here ] := 0X;
				END;
				IF DBG THEN
					Debug.Debug;
					Debug.Label( "here" );
					Debug.Int( here, 3 );
					Debug.Nl;
					Debug.Debug;
					Debug.Label( "line" );
					Debug.QString( line );
					Debug.Nl;
				END;
				Strings.Assign( line, bytStr );
				line[ 0 ] := 0X;
				instrLen := Strings.Length( bytStr );
			ELSE
				line     := oldLine;
				instr[0] := SHORT( EvalByte() );
				instrLen := 1;
			END;

		| o_DW:
			val      := Eval();
			Utils.Split( val, hi, lo );
			instr[0] := SHORT( lo );
			instr[1] := SHORT( hi );
			instrLen := 2;

		| o_DS:
(* FitzWork *)
			val := Eval();

			IF pass = 2 THEN
(*
				showAddr := FALSE;
				Delete(listLine,1,13);
				listLine := Hex4(locPtr) + ':  (' + Hex4(val) + ')' + listLine;
*)
			END;

			val := val + locPtr;
			CodeOrg(val);

		| o_END:
(*
			oldLine := line;

			IF Length(GetWord)<>0 THEN BEGIN
				line := oldLine;
				val  := Eval;
				CodeXfer(val);
				line := Copy(line,1,7) + '(' + Hex4(val) + ')' +
					Copy(line,14,255);
			END;

			sourceEnd := TRUE;
*)
			Out.String( "END found" );
			Out.Ln;
	ELSE
		Error( 'Unknown opcode ');
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DoOpcode;

PROCEDURE DoLabelOp( typ, parm: Integer; labl: SymStr; InputNo: LONGINT );
VAR
	DBG     : BOOLEAN;

	val:  LONGINT;
	word: String;
	len:  LONGINT;
	hi, lo :  LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	CASE typ OF
		o_EQU:
			Out.String( "DoLabelOp: EQU" ); Out.Ln;

			IF DBG THEN
				Debug.Debug;
				Debug.Label( "labl" );
				Debug.QString( labl );
				Debug.Nl;
			END;

			len := Strings.Length( labl );
			IF( len = 0 ) THEN
				cl_Err := TRUE;
				Error( 'Missing label' )
			ELSE
				Deblank( line );
				IF line[ 0 ] = '$' THEN
					IF Strings.Length( line ) = 1 THEN
						Out.String( "I got here" ); Out.Ln;
						val := locPtr;
					END;
				ELSE
					val := Eval();
					IF DBG THEN
						Out.String( "Does this error out if $ in there?" );
						Out.Ln;
						Out.String( "word: " );
						Out.String( word );
						Out.Ln;
						Debug.Debug;
						Debug.Label( "val" );
						Debug.Hex( val, 4 );
						Debug.Nl;
					END;

					Utils.Word2HexStr( val, listRec.addr );
					IF DBG THEN
						Debug.Debug;
						Debug.Label( "EQU listLine" );
						Debug.QString( listLine );
						Debug.Nl;
					END;
					Utils.Split( val, hi, lo );
					instr[0] := SHORT( lo );
					instr[1] := SHORT( hi );
				END;

(*
				DefSym( labl, val, parm=1, parm=0 );
*)
			END;

		| o_ORG:
			Out.String( "ORG" ); Out.Ln;

			CodeOrg( Eval() );
(*
			DefSym( labl, locPtr, FALSE, FALSE );
*)
			showAddr := TRUE;

		| o_LIST:
			Out.String( "LIST" ); Out.Ln;

(*
			listThisLine := TRUE;

			IF Length(labl)<>0 THEN
				Error('Label not allowed');
			END;

			word := GetWord;
			IF word='ON'  THEN
				listFlag := TRUE
			ELSIF word='OFF' THEN
				listFlag := FALSE
			ELSE
				Error('Illegal operand');
			END;
*)

		| o_OPT:
			Out.String( "OPT" ); Out.Ln;

(*
			listThisLine := TRUE;

			IF Length(labl)<>0 THEN
				Error('Label not allowed');
			END;

			word := GetWord;
			IF word='LIST'   THEN
				listFlag := TRUE
			ELSIF word='NOLIST' THEN
				listFlag := FALSE
			ELSE
				Error('Illegal option');
			END;
*)

		ELSE
			Error('Unknown opcode');
		END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DoLabelOp;

PROCEDURE ListOut;
VAR
	DBG     : BOOLEAN;

	i: Integer;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	IF Deblank( listLine ) = #12 THEN
		WriteLn( listing, #12 )

	ELSE IF Deblank( listLine ) = '' THEN
		WriteLn( listing )

	ELSE
		i := Length( listLine );
		WHILE ( i > 0 ) & ( listLine[i] = ' ' ) DO
			Dec( i );
		END;
		listLine[0] := CHR( i );

		WriteLn( listing, listLine );
		IF errFlag AND cl_Err THEN
			WriteLn( listLine );
		END;
	END;
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END ListOut;

PROCEDURE GetLabel( labl: ARRAY OF CHAR );
VAR
	DBG     : BOOLEAN;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	GetWord( labl );
	showAddr := ( Strings.Length( labl ) > 0 );

	IF Strings.Length( line ) > 0 THEN
		IF line[0] = ':' THEN
			Strings.Delete( line, 0, 1 );
		END;
	END;
	IF DBG THEN
		Debug.Debug;
		Debug.Label( "labl" );
		Debug.QString( labl );
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "line" );
		Debug.QString( line );
		Debug.Nl;
	END;
	Strings.Assign( labl, listRec.labl );

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END GetLabel;

PROCEDURE GetOpcode( str: ARRAY OF CHAR );
VAR
	DBG     : BOOLEAN;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	GetWord( str );
	(* Strings.Assign( str, opcd ); *)
	Strings.Capitalize( str );
	Out.String( "str: " );
	Out.String( str );
	Out.Ln;

	IF DBG THEN
		Debug.Debug;
		Debug.Label( "str" );
		Debug.QString( str );
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "line" );
		Debug.QString( line );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END GetOpcode;

PROCEDURE DoPass;
VAR
	DBG     : BOOLEAN;

	labl	: SymStr;
	opcode	: OpcdStr;
	opcd	: OpcdStr;
	typ		: Integer;
	parm	: Integer;
	i		: Integer;
	idx		: Integer;
	word	: String;
	hi, lo	: LONGINT;
	val		: LONGINT;

	(* line number for listing *)
	InputNo:	LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	AssignSrc;
	sourceEnd := FALSE;

	Out.String( 'Pass ' ); Out.Int( pass, 2 ); Out.Ln;

	CodeOrg(0);
	errCount := 0;
	listFlag := TRUE;
	InputNo := 0;

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
			Strings.Assign( line, origLine );

			errFlag      := FALSE;
			instrLen     := 0;
			showAddr     := FALSE;
			listThisLine := listFlag;

			labl[0] := 0X;
			opcode[0] := 0X;

			IF pass = 2 THEN
				B.Clear( listLine, maxStringLen );
				B.Replace( line, lineStart, listLine, maxStringLen );
				libc.sprintf( word, "%5d", InputNo );
				B.Replace( word, lineNoStart, listLine, maxStringLen );
				IF DBG THEN
(*
Commented this out so all of these line line up.
					Debug.Debug;
*)
					Debug.Label( "listLine" );
					Debug.QString( listLine );
					Debug.Nl;
				END;
			END;
			Debug.Inc;

			IF Strings.Length( line ) > 0 THEN
				(*################################################
				# If the 1st char line[0] is AlphaNum
				# then we have a label, maybe. Must end w/ a ":".
				# If so, get the label into labl.
				################################################*)
				IF Utils.IsAlphaNum( line[0] ) THEN
					GetLabel( labl );
				END; (* IF Utils.IsAlphaNum( line[0] ) THEN *)
			END; (* IF Strings.Length( line ) > 0 THEN *)

			IF Strings.Length( line ) > 0 THEN
				GetOpcode( opcode );
			END; (* IF Strings.Length( line ) > 0 THEN *)

			IF Strings.Length( opcode ) = 0 THEN
				(*################################################
				# If there is an opcode w/ nothing more,
				# then call DefSym to create the Symbol table
				# entry even if no labl.
				################################################*)
				typ := 0;
(*
				DefSym( labl, locPtr, FALSE, FALSE );
*)

			ELSE
				FindOpcode( opcode, typ, parm );
(* Fitz1 *)

				IF typ = o_Illegal THEN
					ErrorStart( 'Illegal opcode "' );
					Out.String( opcode );
					Out.Char( '"' );
					ErrorEnd;
				ELSIF typ <= 0 THEN
					showAddr := FALSE;
HALT( 1 );
					DoLabelOp( typ, parm, labl, InputNo );
				ELSE
					showAddr := TRUE;
(*
					DefSym( labl, locPtr, FALSE, FALSE );
*)
					DoOpcode( typ, parm );
				END;
				IF typ # o_Illegal THEN
					GetWord( word );
					IF Strings.Length( word ) > 0 THEN
						Error( 'Too many operands' );
					END;
				END;
			END;

			IF pass = 2 THEN
				Out.String( "Inside IF" ); Out.Ln;

				IF showAddr THEN
					Utils.Word2HexStr( locPtr, word );
					B.Replace( word, addrStart, listLine, maxStringLen );
				END;

				(*################################################
				# This is to put the value of the EQU
				# in the listLine where the address goes.
				################################################*)
				IF typ = o_EQU THEN
					IF DBG THEN
						Out.String( "Inside DoPass and IF typ = o_EQU" );
						Out.Ln;
					END;
					lo := instr[0];
					hi := instr[1];
					Utils.Unsplit( val, hi, lo );
					IF DBG THEN
						Debug.Debug;
						Debug.Label( "val" );
						Debug.Hex( val, 4 );
						Debug.Nl;
					END;
					Utils.Word2HexStr( val, word );
					B.Replace( word, addrStart, listLine, maxStringLen );
				END;

				(*################################################
				# This is to put the value of the DB
				# in the listLine.
				################################################*)
				IF typ = o_DB THEN
(* FitzWork *)
					IF DBG THEN
						Out.String( "Inside DoPass and IF typ = o_DB" );
						Out.Ln;
					END;
					IF DBG THEN
						Debug.Debug;
						Debug.Label( "word" );
						Debug.QString( word );
						Debug.Nl;
					END;
(*
					lo := instr[0];
					hi := instr[1];
					Utils.Unsplit( val, hi, lo );
					IF DBG THEN
						Debug.Debug;
						Debug.Label( "val" );
						Debug.Hex( val, 4 );
						Debug.Nl;
					END;
					Utils.Word2HexStr( val, word );
					B.Replace( word, addrStart, listLine, maxStringLen );
*)
				END;

				IF instrLen > 0 THEN
					Utils.Byte2HexStr( instr[0], word );
					B.Replace( word, B1Start, listLine, maxStringLen );
					IF instrLen = 2 THEN
						Utils.Byte2HexStr( instr[1], word );
						B.Replace( word, B3Start, listLine, maxStringLen );
					ELSIF instrLen = 3 THEN
						Utils.Byte2HexStr( instr[2], word );
						B.Replace( word, B4Start, listLine, maxStringLen );
						Utils.Byte2HexStr( instr[1], word );
						B.Replace( word, B3Start, listLine, maxStringLen );
					END;
(*
				ELSIF instrLen < 0 THEN
(* FitzWork *)
					Strings.Append( "       ", listLine );
				ELSE
					FOR i := 1 TO -instrLen DO BEGIN
						IF i<=3 THEN BEGIN
							word := Hex2(ORD(bytStr[i]));
							listLine[i*3+4] := word[1];
							listLine[i*3+5] := word[2];
						END;
					CodeOut(ORD(bytStr[i]));
*)
				END;

(*
				(*################################################
				# Because the Line Number length can vary, 
				# Determine where to start the line Number.
				################################################*)
				libc.sprintf( word, "%5d", InputNo );
				lineStart := lineNoStart + maxLineNoLen - Strings.Length( word );
				Strings.Append( word, listLine );
				Strings.Append( origLine, listLine );
				IF DBG THEN
					Debug.Debug;
					Debug.Label( "Opcode listLine" );
					Debug.QString( listLine );
					Debug.Nl;
				END;
*)			

				(*################################################
				# Print out the list Line
				################################################*)
				wLST.WriteString( listLine );
				wLST.WriteLn;

				IF listThisLine THEN ListOut; END;
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
Section 1
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

PROCEDURE SortSymTab;
VAR
	DBG     : BOOLEAN;

	i,j,t	: SymEntry;
	sorted	: Boolean;
	temp	: SymEntry;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	IF symTab # NIL THEN

		i := symTab;
		j := i^.next;
		WHILE ( j # NIL ) DO
			sorted := TRUE;

			WHILE ( j # NIL ) DO
				IF j^.name < i^.name THEN
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
	END; (* IF symTab # NIL THEN *)
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END SortSymTab;

PROCEDURE DumpSym( p: SymEntry );
VAR
	DBG     : BOOLEAN;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

(*
	wLST.WriteString( p^.name );
	wLST.WriteChar( " " );
	wLST.WriteHex( p^.value, 4 );
	wLST.WriteLn;

	Write( listing, p^.name:maxSymLen, ' ', Hex4( p^.value ));

	IF NOT p^.defined  THEN Write( listing, ' U' );
	IF     p^.multiDef THEN Write( listing, ' M' );
	IF     p^.isSet    THEN Write( listing, ' S' );
	IF     p^.equ      THEN Write( listing, ' E' );

	WriteLn( listing );
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DumpSym;

(*###############################################################
# Title:	DumpSymTab
# Func:		Dump the symbol table.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100206 # Fitz       # Original.
###############################################################*)
PROCEDURE DumpSymTab;
VAR
	DBG     : BOOLEAN;

	p		: SymEntry;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;
	Out.String( "Fitz" ); Out.Ln;

(*
	SortSymTab;
	p := symTab;
	IF p = NIL THEN
		Out.String( "OOPS: p is NIL" );
		Out.Ln;
	END;

	DumpSym( p );

	p := symTab;
	WHILE ( p # NIL ) DO BEGIN
		DumpSym( p );
		p := p^.next;
	END;
*)

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DumpSymTab;

PROCEDURE ShowOptions;
BEGIN
   Out.Ln;
   Out.String('  Command line syntax:');
   Out.Ln;
   Out.Ln;
   Out.String('  ASM8080 [options] src [options]');
   Out.Ln;
   Out.Ln;
   Out.String('  Valid options:');
   Out.Ln;
   Out.Ln;
   Out.String('    -E  Show errors to screen');
   Out.Ln;
   Out.String('    -L  Make a listing file to src.LIS');
   Out.Ln;
   Out.String('    -L=name');
   Out.Ln;
   Out.String('    -O  Make an object file to src.OBJ');
   Out.Ln;
   Out.String('    -O=name');
   Out.Ln;
END ShowOptions;

(*###############################################################
# Title:	ProcessArgs
# Func:		Process the program Arguments.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100206 # Fitz       # Original.
###############################################################*)
PROCEDURE ProcessArgs;
VAR
	DBG     : BOOLEAN;

	argc    : LONGINT;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	argc := ProgramArgs.args.ArgNumber();
	IF DBG THEN
		Debug.Debug;
		Debug.Label( "Arg count" );
		Debug.LInt( argc, 2 );
		Debug.Nl;
	END;

	rArg := TextRider.ConnectReader( ProgramArgs.args );
	rArg.ReadLine( ASM_Name ); (* skip program invocation *)
	rArg.ReadLine( ASM_Name );

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END ProcessArgs;

PROCEDURE GetOptions(VAR cl_SrcName, cl_ListName, cl_ObjName: String; VAR cl_Err: BOOLEAN): BOOLEAN;
VAR
	DBG:			BOOLEAN;

	argc:			LONGINT;
	optStr:		String;
	option:		String;
	optParm:	String;
	prefix:		String;
	pos:			INTEGER;
	optErr:		BOOLEAN;
	idx:			INTEGER;
	found:		BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	cl_SrcName  := '';
	cl_ListName := 'NUL';
	cl_ObjName  := 'NUL';
	cl_Err      := FALSE;


	optErr := FALSE;

	argc := ProgramArgs.args.ArgNumber();
	IF DBG THEN
		Debug.Debug;
		Debug.Label( "Arg count" );
		Debug.LInt( argc, 2 );
		Debug.Nl;
	END;

	rArg := TextRider.ConnectReader( ProgramArgs.args );
	rArg.ReadLine( optStr ); (* skip program invocation *)

	idx := 0;
	REPEAT
		optParm[0] := 0X;
		rArg.ReadLine( optStr );
		IF DBG THEN
			Debug.Debug;
			Debug.Label( "optStr" );
			Debug.QString( optStr );
			Debug.Nl;
		END;

		IF DBG THEN
			Debug.Debug;
			Debug.Label( "len" );
			Debug.LInt( Strings.Length( optStr ), 3 );
			Debug.Nl;
		END;
		Strings.FindNext( "=", optStr, 0, found, pos );
		IF found THEN
			option[ 0 ] := optStr[ 0 ];
			option[ 1 ] := optStr[ 1 ];
			Strings.Delete( optStr, 0, 3 );
			Strings.Assign( optStr, optParm );
		ELSE
			Strings.Assign( optStr, option );
		END;
		IF DBG THEN
			Debug.Debug;
			Debug.Label( "optStr" );
			Debug.QString( optStr );
			Debug.Nl;
			Debug.Debug;
			Debug.Label( "option" );
			Debug.QString( option );
			Debug.Nl;
		END;

		IF Strings.Equal( option, "-L" ) THEN
			cl_ListName := optParm;
		ELSIF Strings.Equal( option, "-O" ) THEN
			cl_ObjName := optParm;
		ELSIF Strings.Equal( option, "-E" ) THEN
			cl_Err := TRUE;
		ELSIF option[0] = "?" THEN
			optErr := TRUE;
		ELSE
			IF(( option[0] = "-" ) OR ( Strings.Length( cl_SrcName ) # 0 ) OR ( Strings.Length( optParm ) # 0 )) THEN
				optErr := TRUE;
				Out.String( "Illegal command line option: " );
				Out.String( option );
				Out.Ln;
			ELSE
				Strings.Assign( option, cl_SrcName );
				Strings.FindNext( ".", cl_SrcName, 0, found, pos );
				IF ~found THEN
					Strings.Append( ".asm", cl_SrcName );
				END;
				Strings.FindNext( ".", option, 0, found, pos );
				IF ~found THEN
					Strings.Assign( option, prefix );
				ELSE
					Strings.Extract( option, 0, pos, prefix );
				END;
			END;
		END;

		INC( idx );
	UNTIL idx = argc;
	IF DBG THEN
		Debug.Nl;
		IF Strings.Length( cl_ListName ) > 0 THEN
			Debug.Debug;
			Debug.Label( "cl_ListName" );
			Debug.QString( cl_ListName );
			Debug.Nl;
		END;

		IF Strings.Length( cl_ObjName ) > 0 THEN
			Debug.Debug;
			Debug.Label( "cl_ObjName" );
			Debug.QString( cl_ObjName );
			Debug.Nl;
		END;

		Debug.Debug;
		Debug.Label( "cl_Err" );
		IF cl_Err THEN
			Debug.QString( "TRUE" );
		ELSE
			Debug.QString( "FALSE" );
		END;
		Debug.Nl;
		Debug.Nl;
	END;

	IF Strings.Length( cl_SrcName ) = 0 THEN
		optErr := TRUE;
		Out.String( "Source file not specified!" );
		Out.Ln;
	END;

	IF Strings.Length( cl_ListName ) = 0 THEN
		Strings.Assign( prefix, cl_ListName );
		Strings.Append( ".lst", cl_ListName );
	END;

	IF Strings.Length( cl_ObjName ) = 0 THEN
		Strings.Assign( prefix, cl_ObjName );
		Strings.Append( ".exe", cl_ObjName );
	END;

(*	
	Strings.FindNext( ".", cl_ListName, 0, found, pos );
	IF found THEN
		Strings.Assign( prefix, cl_ObjName );
		Strings.Append( ".lst", cl_ObjName );
	END;
*)	

	IF DBG THEN
		Debug.Debug;
		Debug.Label( "cl_SrcName" );
		Debug.String( cl_SrcName );
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "cl_ListName" );
		Debug.String( cl_ListName );
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "cl_ObjName" );
		Debug.String( cl_ObjName );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
	RETURN optErr;
END GetOptions;

PROCEDURE Test;
VAR
	DBG :			BOOLEAN;

	wASM :		TextRider.Reader;
	Str :			ARRAY 256 OF CHAR;
	val :			LONGINT;
	word :		String;
	long :		LONGINT;

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
(*
	int := SHORT( 0FFH );
	Out.Hex( int, 8 );
	Out.Ln;
	HALT( 1 );
*)

(*
(*###############################################################
# Title:	Test code for LinkedList
# Func:		Dump the symbol table.
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100206 # Fitz       # Original.
###############################################################*)
	ll := NEW( LlList );
	NEW( e );
	e.Val := 100H;
	e.Instr := "CODE";
	ll.Insert( 0, e );

	NEW( e );
	e.Val := 103H;
	e.Instr := "DUMP";
	ll.Append( e );
	
	Ntry := ll.Get( 0 );
	Iter := ll.GetIterator(NIL);
	WHILE Iter.HasNext() DO
		Ntry := Iter.Next();
		Out.String( "Addr: 0x" );
		Out.Hex( Ntry.Val, 4 );
		Out.String( "    Instruction: " ); Out.String( Ntry.Instr );
		Out.Ln; Out.Ln;
	END;
HALT( 1 );
*)

	IF GetOptions( cl_SrcName, cl_ListName, cl_ObjName, cl_Err) THEN
		ShowOptions;
		HALT( 1 );
	END;

	Assign;

	NEW( SymTab );
(*
	NEW( SymTab );
	NEW( SymIter );
	NEW( listRec );
	SymIter := SymTab.GetIterator( NIL );
*)
	symTab    := NIL;
	xferAddr  := 0;
	xferFound := FALSE;
	InitOpcodes;

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

(*
	DumpSymTab();
*)

	fCOM.Close;
	fLST.Close;
END Asm8080.

(*###############################################################
FFFFFFFFFFFFFFFFFFFFFFFFFF
###############################################################*)

(*###############################################################
###############################################################*)
