MODULE Dis;
(*###############################################################
# Title:	Dis
# Func:		This is a basic i8080 disassembler.
# Author:	Fitz
# Ver:		0.1
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100714 # Fitz       # Part1
# 20100506 # Fitz       # Part2
###############################################################*)
IMPORT
	SYSTEM,
	Out,
	Object,
	Strings,
	
	OS := OS:ProcessManagement,
	ProgramArgs,
	Files,
	TextRider,
	BinaryRider,
	CharClass,
	
	LL := ADT:LinkedList,
	STB := ADT:StringBuffer,
	Glbl := Dis:Globals,
	
	Inst8080,
	DisProc := Dis:Dis,
	CmdProc := Dis:CmdProcessor,
	ComProc := Dis:ComProcessor,	
	
	Buff := Buffer,
	Utils,	This, Debug
;

TYPE
	StringList = LL.LinkedList(STRING); 
	Entry = POINTER TO EntryDesc;
		EntryDesc = RECORD (Object.ObjectDesc)
		Instr:	ARRAY 10 OF CHAR;
		Parm:	ARRAY 10 OF CHAR;
		Val:	INTEGER;
		Cnt:	INTEGER;
	END; 
	
VAR
	e:			Glbl.CmdEntry;
	Ntry:		Glbl.CmdEntry;
	c:			Object.CharsLatin1;

	list_on:	BOOLEAN;
	lineNum:	INTEGER;
	
	InputNo:	INTEGER;
	InputLine:	ARRAY 256 OF CHAR;
	
	sb:			STB.StringBuffer;
	
	ok:			BOOLEAN;
	rc:			INTEGER;
	cmd:		STRING;
	
	rArg:		TextRider.Reader;

	res:		Files.Result;
	eof:		BOOLEAN;

	COM_Name:	ARRAY 256 OF CHAR;
	LST_Name:	ARRAY 256 OF CHAR;
	CMD_Name:	ARRAY 256 OF CHAR;


PROCEDURE Pad( sb: STB.StringBuffer );
BEGIN
	sb.Append( " " );
END Pad;

PROCEDURE PadTo( sb: STB.StringBuffer; pos: LONGINT );
VAR
	rest:	LONGINT;
	
BEGIN
	ASSERT( pos > sb.length );
	rest := pos-sb.length-1;
	WHILE rest > 0 DO
		Pad( sb );
		DEC( rest );
	END;
END PadTo;
  
(*###############################################################
# Title:	Init
# Func:		To initialize some of the assemblers global variables.
# Author:	Fitz
# Ver:		0.0
# Todo:		Nothing known.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100506 # Fitz       # Original.
###############################################################*)
PROCEDURE Init;
VAR
	str:		STRING;
	
BEGIN
	list_on		:= TRUE;

	lineNum		:= 0;
	
	sb := NEW(STB.StringBuffer, 10);
	sb.Clear;
	sb.Append( "Label:" );
	PadTo( sb, 10 );
	sb.Append( "JMP" );
	sb.AppendLn;
	str := sb.ToString();
	Out.Object( str );
	Out.Ln;
(*	
	Out.String( "123456789012345" );
	Out.Ln;
*)
	
	(*
	sb.Append( "JMP" );
	Out.String( "Buff contents: '" );
	Out.String( sb.data8 );
	Out.Char( "'" );
	Out.Ln;
	*)
	
END Init;

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
	DBG := TRUE;
	DBG := FALSE;
	(* DBG := TRUE; *)
	
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
	rArg.ReadLine( COM_Name ); (* skip program invocation *)
	rArg.ReadLine( COM_Name );

	IF DBG THEN
		Debug.Debug;
		Debug.Label( "COM file name" );
		Debug.String( COM_Name );
		Debug.Nl;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END ProcessArgs;

BEGIN
(*
	ll := NEW( Glbl.LinkedList );
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
*)
	
	Init;
	
	ProcessArgs;
	DisProc.OpenFiles( COM_Name );
	CmdProc.ReadCmds;
	ComProc.ReadCom( Glbl.PC );
(*
*)
	
	(* ReadInst; *)
	
	Glbl.fCOM.Close;
	Glbl.fCMD.Close;
	Glbl.fLST.Close;
	
END Dis.
