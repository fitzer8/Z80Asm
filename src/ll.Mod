MODULE ll;
(*###############################################################
# Title:	ll
# Func:		This is a test of the ArrayList lib in oo2c.
# Author:	Fitz.
# Ver:		0.0
# Todo:
#					Add something here.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110428 # Fitz       # Original.
# 20110428 # Fitz       # Original 0.1 release.
# 20110428 # Fitz       # 0.2 Object code now output.
###############################################################*)
IMPORT
	libc,
	Out,
	Ascii,
	Strings,
	CharClass,
	Object,

	LL		:= ADT:LinkedList,

	Utils,
	This, Debug
	;

CONST
	maxStringLen = 256;
	maxSymLen    = 256;
	maxOpcdLen   = 6;
	maxLineNoLen = 8;

TYPE
	Integer	= INTEGER;
	Boolean	= BOOLEAN;

	String*	= ARRAY maxStringLen OF CHAR;
	SymStr*	= ARRAY maxSymLen OF CHAR;

	SymEntry*		= POINTER TO SymEntryDesc;
	SymEntryDesc	= RECORD (Object.ObjectDesc)
		name*		: SymStr;   (* Symbol name *)
		value*		: LONGINT;  (* Symbol value *)
		next		: SymEntry;	(* Pointer to next symtab entry *)
		defined*	: Boolean;  (* TRUE if defined *)
		multiDef*	: Boolean;  (* TRUE if multiply defined *)
		isSet*		: Boolean;  (* TRUE if defined with SET pseudo *)
		equ*		: Boolean;  (* TRUE if defined with EQU pseudo *)
	END;

	symList*		= LL.LinkedList( SymEntry );
	symIter*		= LL.Iterator( SymEntry );
	
VAR
	DBG					: BOOLEAN;

	symTab:				SymEntry;	(* Pointer to first entry in symtab *)

	SymNtry				: SymEntry;
	SymList				: symList;
	SymIter				: symIter;
	Ntry				: SymEntry;

	name				: SymStr;

(*###############################################################
# Title:	FindSym
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
PROCEDURE FindSym( );
VAR
	DBG			: BOOLEAN;

	Ntry		: SymEntry;
	found		: Boolean;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	IF DBG THEN
		Debug.Entry( This.File(), This.Func() );
	END;

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
	END;

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
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
PROCEDURE NextSym( SymName: SymStr ) : LONGINT;
VAR
	DBG			: BOOLEAN;

	Ntry		: SymEntry;
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
		Debug.Label( "SymName" );
		Debug.QString( SymName );
		Debug.Nl;
	END;

	found := FALSE;

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
			Out.String( "greater" ); Out.Ln;
			found := TRUE;
			EXIT;
		END;
	END;
	idx := SymList.IndexOf( Ntry );

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
PROCEDURE AddSym( SymName: SymStr );
VAR
	DBG			: BOOLEAN;

	Ntry		: SymEntry;
	p			: SymEntry;
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
		Debug.Label( "SymName" );
		Debug.QString( SymName );
		Debug.Nl;
	END;

	found := FALSE;
	NEW( p );
	p.value		:= 0;
	p.name		:= SymName;
	p.defined	:= FALSE;
	p.multiDef	:= FALSE;
	p.isSet		:= FALSE;
	p.equ		:= FALSE;
			Out.String( "    p.name: " ); Out.String( p.name );
			Out.Ln; Out.Ln;
idx := NextSym( SymName );

	Ntry := SymList.Get( idx );
	SymList.Insert( idx, p );

	IF DBG THEN
		Debug.Exit( This.File(), This.Func() );
	END;
END AddSym;
	
BEGIN
	(*################################################
	# This symbol put in there to prevent conditional
	# processing later to determine if we need an
	# Insert or an Append.
	################################################*)
	SymList := NEW( symList );
	NEW( SymNtry );
	SymNtry.value := 0;
	SymNtry.name := "ooo";
	SymList.Append( SymNtry );
(*
	SymList.Insert( 0, SymNtry );
*)

	NEW( SymNtry );
	SymNtry.value := 0100H;
	SymNtry.name := "stack";
	SymList.Append( SymNtry );

	Out.LongInt( SymNtry.value, 5 );
	Out.String( " :  Total Error(s)" );
	Out.Ln;

	name := "aaa";
	AddSym( name );
	
	name := "rrr";
	AddSym( name );
	
	name := "zzz";
	AddSym( name );
	
	FindSym;

END ll.

(*###############################################################
FFFFFFFFFFFFFFFFFFFFFFFFFF
###############################################################*)

