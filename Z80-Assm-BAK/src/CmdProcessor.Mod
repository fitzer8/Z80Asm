MODULE Dis:CmdProcessor;
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
	
	CharClass,
	
	LL := ADT:LinkedList,
	ADT:StringBuffer,
	
	DisLL := Dis:LinkedList,
	Glbl := Dis:Globals,
	
	Buffer,
	Utils,
	This, Debug
;

VAR
	list_on:	BOOLEAN;
	lineNum:	INTEGER;
	
	InputNo:	INTEGER;
	InputLine:	ARRAY 256 OF CHAR;
	
	ok:			BOOLEAN;
	rc:			INTEGER;
	cmd:		STRING;

(*###############################################################
# Title:	CmdLine
# Func:		To split the CMD line into usable pieces.
# Author:	Fitz
# Ver:		0.0
# Todo:		Nothing known.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100714 # Fitz       # Original.
###############################################################*)
PROCEDURE CmdLine*( InputLine: ARRAY OF CHAR );
VAR
	DBG:		BOOLEAN;
	
	CMD:		INTEGER;
	idx:		INTEGER;
	Addr:		INTEGER;
	
	Instr:		ARRAY 10 OF CHAR;
	AddrStr:	ARRAY 10 OF CHAR;
	CmdStr:		ARRAY 10 OF CHAR;
	LblStr:		ARRAY 10 OF CHAR;
	Cnt:		LONGINT;
	
	CmdNtry:		Glbl.CmdEntry;
	LblNtry:		Glbl.LblEntry;
	
BEGIN
	DBG := TRUE;
	DBG := FALSE;
	(* DBG := TRUE; *)
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "InputLine" );
		Debug.QString( InputLine );
		Debug.Nl;
	END;
	
	CMD := 0;
	idx := 0;
	WHILE ~CharClass.IsWhiteSpace( InputLine[ CMD ]) DO
		AddrStr[idx] := InputLine[CMD];
		INC( CMD );
		INC( idx );
	END;
	AddrStr[idx] := 0X;
	Addr := Utils.AsciiStr2Number( AddrStr );

	WHILE CharClass.IsWhiteSpace( InputLine[ CMD ]) DO
		INC( CMD );
	END;

	idx := 0;
	WHILE(( ~CharClass.IsWhiteSpace( InputLine[ CMD ])) & ( InputLine[ CMD ] # 0X )) DO
		CmdStr[idx] := InputLine[CMD];
		INC( CMD );
		INC( idx );
	END;
	CmdStr[idx] := 0X;
		
	WHILE(( CharClass.IsWhiteSpace( InputLine[ CMD ])) & ( InputLine[ CMD ] # 0X )) DO
		INC( CMD );
	END;

	IF Strings.Equal( CmdStr, "ORG" ) THEN
		IF DBG THEN
			Debug.Debug; Debug.String( "Found an ORG statement." ); Debug.Nl;
		END;
		Glbl.PC := Addr;
	ELSIF Strings.Equal( CmdStr, "DUMP" ) THEN
		IF DBG THEN
			Debug.Debug; Debug.String( "Found a DUMP statement." ); Debug.Nl;
		END;
		NEW( CmdNtry );
		CmdNtry.Addr := Addr;
		Strings.Assign( CmdStr, CmdNtry.Cmd );
		DisLL.CmdLst.Append( CmdNtry );
		DisLL.DumpCmdList;
	ELSIF Strings.Equal( CmdStr, "CODE" ) THEN
		IF DBG THEN
			Debug.Debug; Debug.String( "Found a CODE statement." ); Debug.Nl;
		END;
		NEW( CmdNtry );
		CmdNtry.Addr := Addr;
		Strings.Assign( CmdStr, CmdNtry.Cmd );
		DisLL.CmdLst.Append( CmdNtry );
		DisLL.DumpCmdList;
	ELSIF Strings.Equal( CmdStr, "BYTE" ) THEN
		IF DBG THEN
			Debug.Debug; Debug.String( "Found a BYTE statement." ); Debug.Nl;
		END;
	ELSIF Strings.Equal( CmdStr, "WORD" ) THEN
		IF DBG THEN
			Debug.Debug; Debug.String( "Found a WORD statement." ); Debug.Nl;
		END;
	ELSIF Strings.Equal( CmdStr, "ASCII" ) THEN
		IF DBG THEN
			Debug.Debug; Debug.String( "Found a ASCII statement." ); Debug.Nl;
		END;
	ELSIF Strings.Equal( CmdStr, "LABEL" ) THEN
		IF DBG THEN
			Debug.Debug; Debug.String( "Found a LABEL statement." ); Debug.Nl;
		END;
		idx := 0;
		WHILE(( ~CharClass.IsWhiteSpace( InputLine[ CMD ])) & ( InputLine[ CMD ] # 0X )) DO
			LblStr[idx] := InputLine[CMD];
			INC( CMD );
			INC( idx );
		END;
		LblStr[idx] := 0X;
		NEW( LblNtry );
		LblNtry.Addr := Addr;
		Strings.Assign( LblStr, LblNtry.Name );
		DisLL.LblLst.Append( LblNtry );
		DisLL.DumpLblList;
	END;
	
	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END CmdLine;

(*###############################################################
# Title:	ReadCmds
# Func:		To read the commands and load them into the list.
# Author:	Fitz
# Ver:		0.0
# Todo:		Nothing known.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100714 # Fitz       # Original.
###############################################################*)
PROCEDURE ReadCmds*;
VAR
	DBG:		BOOLEAN;
	
	Instr:		ARRAY 10 OF CHAR;
	AddrStr:	ARRAY 10 OF CHAR;
	CmdStr:		ARRAY 10 OF CHAR;
	Cnt:		LONGINT;
	
BEGIN
	DBG := TRUE;
	DBG := FALSE;
	(* DBG := TRUE; *)
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	WHILE( Utils.GetLine( Glbl.rCMD, InputLine, Cnt )) DO
		IF DBG THEN
			Debug.Nl;
			Debug.Debug;
			Debug.Label( "InputLine" );
			Debug.QString( InputLine );
			Debug.Nl;
		END;
		IF InputLine[0] # "#" THEN
			CmdLine( InputLine );
		END;
	END;
	IF DBG THEN
		Debug.Nl;
		Debug.Debug;
		Debug.Label( "InputLine" );
		Debug.QString( InputLine );
		Debug.Nl;
	END;
	IF InputLine[0] # "#" THEN
		CmdLine( InputLine );
	END;
	
	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END ReadCmds;

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
	DBG:	BOOLEAN;
	
BEGIN
	DBG := TRUE;
	DBG := FALSE;
	(* DBG := TRUE; *)
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	list_on		:= TRUE;

	lineNum		:= 0;
	
	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END Init;

BEGIN
	Init;
	
END Dis:CmdProcessor.

