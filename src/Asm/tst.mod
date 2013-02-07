(*###############################################################
# Title:	title
# Func:		Proc
# Author:	Fitz
# Ver:		0.0
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20110206 # Fitz       # Original.
###############################################################*)
PROCEDURE Proc*(  );
VAR
DBG: BOOLEAN;

BEGIN
DBG := TRUE;
DBG := FALSE;
IF DBG THEN
Debug.Entry( This.File(), This.Func() );
END;


IF DBG THEN
Debug.Exit( This.File(), This.Func() );
END;	
END Proc;

