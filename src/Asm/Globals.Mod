MODULE Asm:Globals;
(*###############################################################
# Title:	Asm:Globals
# Func:		This file contains the Globals for Asm.
# Author:	Fitz
# Ver:		0.1
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100714 # Fitz       # Part1
# 20100506 # Fitz       # Part2
# 20110503 # Fitz       # Cleanup.
###############################################################*)
IMPORT
	Object,

	LL := ADT:LinkedList
;

CONST
	dmp* = 0;
	cod* = 1;
	asc* = 2;
	byt* = 3;
	wrd* = 4;
	
	maxStringLen = 256;
	maxSymLen    = 256;

TYPE
	Boolean* = BOOLEAN;
	Integer* = INTEGER;

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

	SymList*		= LL.LinkedList( SymEntry );
	SymIter*		= LL.Iterator( SymEntry );
	
	XrefEntry*		= POINTER TO XrefEntryDesc;
	XrefEntryDesc	= RECORD (Object.ObjectDesc)
		name*		: SymStr;   (* Symbol name *)
		line*		: LONGINT;  (* Line number *)
		next		: XrefEntry;	(* Pointer to next symtab entry *)
		defined*	: Boolean;  (* TRUE if defined on this line *)
	END;

	XrefList*		= LL.LinkedList( XrefEntry );
	XrefIter*		= LL.Iterator( XrefEntry );
	
END Asm:Globals.

