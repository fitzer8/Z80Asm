MODULE Dis:LinkedList;
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
	
	Glbl := Dis:Globals,
	
	Buffer,
	This, Debug
;

VAR
	CmdLst*:	Glbl.CmdList; 
	
	LblLst*:	Glbl.LblList; 
	
(*###############################################################
# Title:	FindAddr
# Func:		Find addr in CmdList and return Ntry if found else NIL.
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
PROCEDURE FindAddr*( addr: LONGINT; VAR ntry: Glbl.CmdEntry );
VAR
	DBG:		BOOLEAN;

	CmdIter:	Glbl.CmdIter;
	CmdNtry:	Glbl.CmdEntry;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	(* DBG := TRUE; *)
	
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	CmdNtry := NIL;
	CmdIter := CmdLst.GetIterator( NIL );
	WHILE CmdIter.HasNext() DO
		CmdNtry := CmdIter.Next();
		IF CmdNtry.Addr = addr THEN
			IF DBG THEN
				Out.String( "Found it." ); Out.Ln;
			END;
			ntry := CmdNtry;
		END;
		
		IF DBG THEN
			Out.String( "Addr: 0x" );
			Out.Hex( CmdNtry.Addr, 4 );
			Out.String( "    Instruction: " ); Out.String( CmdNtry.Cmd );
			Out.Ln;
		END;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END FindAddr;

(*###############################################################
# Title:	DumpCmdList
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
PROCEDURE DumpCmdList( iter: Glbl.Iterator );
###############################################################*)
PROCEDURE DumpCmdList*;
VAR
	DBG:		BOOLEAN;

	CmdIter:	Glbl.CmdIter;
	CmdNtry:	Glbl.CmdEntry;
	
BEGIN
	DBG := TRUE;
	DBG := FALSE;
	(* DBG := TRUE; *)
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		CmdIter := CmdLst.GetIterator( NIL );
		WHILE CmdIter.HasNext() DO
			CmdNtry := CmdIter.Next();
			Out.String( "Addr: 0x" );
			Out.Hex( CmdNtry.Addr, 4 );
			Out.String( "    Instruction: " ); Out.String( CmdNtry.Cmd );
			Out.Ln;
		END;
		Out.Ln;
	END;
	
	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DumpCmdList;

(*###############################################################
# Title:	DumpLblList
# Func:		To print the LblList.
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
PROCEDURE DumpLblList*;
VAR
	DBG:	BOOLEAN;
	
	Name:		ARRAY 10 OF CHAR;
	AddrStr:	ARRAY 10 OF CHAR;
	CmdStr:		ARRAY 10 OF CHAR;
	Cnt:		LONGINT;
	
	LblIter:	Glbl.LblIter;
	LblNtry:	Glbl.LblEntry;
	
BEGIN
	DBG := TRUE;
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	IF DBG THEN
		LblIter := LblLst.GetIterator( NIL );
		WHILE LblIter.HasNext() DO
			LblNtry := LblIter.Next();
			Out.String( "Addr: 0x" );
			Out.Hex( LblNtry.Addr, 4 );
			Out.String( "    Name: " ); Out.String( LblNtry.Name );
			Out.Ln;
		END;
		Out.Ln;
	END;
	
	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END DumpLblList;

(*###############################################################
# Title:	FindLabel
# Func:		Find addr in CmdList and return Ntry if found else NIL.
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
PROCEDURE FindLabel*( addr: LONGINT; VAR ntry: Glbl.LblEntry );
VAR
	DBG:		BOOLEAN;

	LblIter:	Glbl.LblIter;
	LblNtry:	Glbl.LblEntry;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	(* DBG := TRUE; *)
	
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

	LblNtry := NIL;
	ntry := NIL;
	LblIter := LblLst.GetIterator( NIL );
	WHILE LblIter.HasNext() DO
		LblNtry := LblIter.Next();
		IF LblNtry.Addr = addr THEN
			IF DBG THEN
				Out.String( "Found it." ); Out.Ln;
			END;
			ntry := LblNtry;
		END;
		
		IF DBG THEN
			Out.String( "Addr: 0x" );
			Out.Hex( LblNtry.Addr, 4 );
			Out.String( "    Label: " ); Out.String( LblNtry.Name );
			Out.Ln;
		END;
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END FindLabel;

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

	CmdLst := NEW( Glbl.CmdList );
	LblLst := NEW( Glbl.LblList );
	
	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END Init;

BEGIN
	Init;
	
END Dis:LinkedList.
