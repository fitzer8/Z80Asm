PROGRAM Asm8080;

{R-}
{$M 16384,0,655360}

CONST
   maxSymLen    = 16;
   maxOpcdLen   = 4;

   alphaNumeric = '1234567890$ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
   numeric      = '1234567890';
   hex          = '0123456789ABCDEF';
   white        = #9' ';  { A tab plus a space }

   o_Illegal  =   0;  { Opcode not found in FindOpcode }
   o_None     =   1;  { No operands }
   o_One      =   2;  { One byte immediate operand }
   o_Two      =   3;  { Two byte immediate operand }
   o_InrDcr   =   4;  { INR or DCR instruction }
   o_Arith    =   5;  { Register to accumulator arithmetic }
   o_MOV      =   6;  { MOV instruction }
   o_MVI      =   7;  { MVI instruction }
   o_LXI      =   8;  { LXI instruction }
   o_InxDcx   =   9;  { INX, DCX, and DAD instructions }
   o_PushPop  =  10;  { PUSH and POP instructions }
   o_StaxLdax =  11;  { STAX and LDAX instructions }
   o_RST      =  12;  { RST instruction }
   o_DB       =  13;  { DB pseudo-op }
   o_DW       =  14;  { DW pseudo-op }
   o_DS       =  15;  { DS pseudo-op }
   o_EQU      = -16;  { EQU and SET pseudo-ops }
   o_ORG      = -17;  { ORG pseudo-op }
   o_END      =  18;  { END pseudo-op }
   o_LIST     = -19;  { LIST pseudo-op }
   o_OPT      = -20;  { OPT pseudo-op }

   regs         = ' B C D E H L M A ';
   regVals      = ' 0 1 2 3 4 5 6 7 ';

   regPairs     = ' B D H SP BC DE HL ';
   regPairVals  = ' 0 1 2 3  0  1  2  ';

   pushRegs     = ' B D H PSW BC DE HL AF ';
   pushRegVals  = ' 0 1 2 3   0  1  2  3  ';

   staxRegs     = ' B D BC DE ';
   staxRegVals  = ' 0 1 0  1  ';

TYPE
   SymStr  = String[maxSymLen];

   SymPtr  = ^SymRec;
   SymRec  = RECORD
                name:     SymStr;   { Symbol name }
                value:    Integer;  { Symbol value }
                next:     SymPtr;   { Pointer to next symtab entry }
                defined:  Boolean;  { TRUE if defined }
                multiDef: Boolean;  { TRUE if multiply defined }
                isSet:    Boolean;  { TRUE if defined with SET pseudo }
                equ:      Boolean;  { TRUE if defined with EQU pseudo }
             END;

   OpcdStr = String[maxOpcdLen];

   OpcdPtr = ^OpcdRec;
   OpcdRec = RECORD
                name:   OpcdStr;    { Opcode name }
                typ:    Integer;    { Opcode type }
                parm:   Integer;    { Opcode parameter }
                next:   OpcdPtr;    { Pointer to next opcode entry }
             END;

VAR
   symTab:       SymPtr;      { Pointer to first entry in symtab }
   opcdTab:      OpcdPtr;     { Opcode table }

   locPtr:       Integer;     { Current program address }
   pass:         Integer;     { Current assembler pass }
   errFlag:      Boolean;     { TRUE if error occurred this line }
   errCount:     Integer;     { Total number of errors }

   line:         String;      { Current line from input file }
   listLine:     String;      { Current listing line }
   listFlag:     Boolean;     { FALSE to suppress listing source }
   listThisLine: Boolean;     { TRUE to force listing this line }
   sourceEnd:    Boolean;     { TRUE when END pseudo encountered }

   instr:        ARRAY[1..3] OF Integer; { Current instruction word }
   instrLen:     Integer;                { Current instruction length }

   bytStr:       String;      { Buffer for long DB statements }
   showAddr:     Boolean;     { TRUE to show LocPtr on listing }
   xferAddr:     Integer;     { Transfer address from END pseudo }
   xferFound:    Boolean;     { TRUE if xfer addr defined w/ END }

   { Command line parameters }
   cl_SrcName:   String;      { Source file name }
   cl_ListName:  String;      { Listing file name }
   cl_ObjName:   String;      { Object file name }
   cl_Err:       Boolean;     { TRUE for errors to screen }


   source:       Text;
   object:       Text;
   listing:      Text;


FUNCTION Deblank(s: String): String;
VAR
   i: Integer;

BEGIN
   i := Length(s);
   WHILE (i>0) AND (s[i] IN [#9,' ']) DO
      Dec(i);

   s[0] := CHR(i);

   i := 1;
   WHILE (i<=Length(s)) AND (s[i] IN [#9,' ']) DO
      Inc(i);
   Delete(s,1,i-1);

   Deblank := s;
END;


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


PROCEDURE Error(message: String);
BEGIN
   errFlag := TRUE;
   Inc(errCount);

   IF pass<>1 THEN BEGIN
      listThisLine := TRUE;
      WriteLn(listing,'*** Error:  ',Message,' ***');
      IF cl_Err THEN WriteLn('*** Error:  ',Message,' ***');
   END;
END;



PROCEDURE AddOpcode(name: OpcdStr; typ,parm: Integer);
VAR
   p: OpcdPtr;

BEGIN
   New(p);

   p^.name := name;
   p^.typ  := typ;
   p^.parm := parm;
   p^.next := opcdTab;

   opcdTab := p;
END;


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


PROCEDURE InitOpcodes;
BEGIN
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
END;


FUNCTION FindSym(symName: SymStr): SymPtr;
VAR
   p:     SymPtr;
   found: Boolean;

BEGIN
   found := FALSE;
   p     := SymTab;
   WHILE (p<>NIL) AND NOT Found DO BEGIN
      found := (p^.name = symName);
      IF NOT found THEN
         p := p^.next;
   END;

   FindSym := p;
END;


FUNCTION AddSym(symName: SymStr): SymPtr;
VAR
   p: SymPtr;

BEGIN
   New(p);

   WITH p^ DO BEGIN
      name     := SymName;
      value    := 0;
      next     := SymTab;
      defined  := FALSE;
      multiDef := FALSE;
      isSet    := FALSE;
      equ      := FALSE;
   END;

   symTab := p;

   AddSym := p;
END;


FUNCTION RefSym(symName: SymStr): Integer;
VAR
   p: SymPtr;

BEGIN
   p := FindSym(symName);
   IF p=NIL THEN p := AddSym(symName);

   IF NOT p^.defined THEN
      Error('Symbol "' + symName + '" undefined');

   RefSym := p^.value;
END;


PROCEDURE DefSym(symName: SymStr; val: Integer; setSym,equSym: Boolean);
VAR
   p: SymPtr;

BEGIN
   IF Length(symName)<>0 THEN BEGIN

      p := FindSym(symName);
      IF p=NIL THEN p := AddSym(symName);

      IF (NOT p^.defined) OR (p^.isSet AND setSym) THEN BEGIN
         p^.value   := val;
         p^.defined := TRUE;
         p^.isSet   := setSym;
         p^.equ     := equSym;
      END
      ELSE IF p^.value <> val THEN BEGIN
         p^.multiDef := TRUE;
         Error('Symbol "' + symName + '" multiply defined');
      END;
   END;
END;


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


PROCEDURE Expect(expected: String);
BEGIN
   IF GetWord<>expected THEN
      Error('"' + expected + '" expected');
END;


PROCEDURE Comma;
BEGIN
   Expect(',');
END;


FUNCTION EvalOct(octStr: String): Integer;
VAR
   octVal:  Integer;
   evalErr: Boolean;
   i,n:     Integer;

BEGIN
   evalErr := FALSE;
   octVal  := 0;

   FOR i := 1 TO Length(octStr) DO BEGIN
      n := Pos(octStr[i],'01234567');
      IF n=0 THEN evalErr := TRUE
             ELSE octVal  := octVal*8 + n-1;
   END;

   IF evalErr THEN BEGIN
      octVal := 0;
      Error('Invalid octal number');
   END;

   EvalOct := octVal;
END;


FUNCTION EvalDec(decStr: String): Integer;
VAR
   decVal:  Integer;
   evalErr: Boolean;
   i,n:     Integer;

BEGIN
   evalErr := FALSE;
   decVal  := 0;

   FOR i := 1 TO Length(decStr) DO BEGIN
      n := Pos(decStr[i],'0123456789');
      IF n=0 THEN evalErr := TRUE
             ELSE decVal  := decVal*10 + n-1;
   END;

   IF evalErr THEN BEGIN
      decVal := 0;
      Error('Invalid decimal number');
   END;

   EvalDec := decVal;
END;


FUNCTION EvalHex(hexStr: String): Integer;
VAR
   hexVal:  Integer;
   evalErr: Boolean;
   i,n:     Integer;

BEGIN
   evalErr := FALSE;
   hexVal  := 0;

   FOR i := 1 TO Length(hexStr) DO BEGIN
      n := Pos(Upcase(hexStr[i]),'0123456789ABCDEF');
      IF n=0 THEN evalErr := TRUE
             ELSE hexVal  := hexVal*16 + n-1;
   END;

   IF evalErr THEN BEGIN
      hexVal := 0;
      Error('Invalid hexadecimal number');
   END;

   EvalHex := hexVal;
END;


FUNCTION Eval: Integer; FORWARD;


FUNCTION Factor: Integer;
VAR
   word: String;
   val:  Integer;

BEGIN
   word := GetWord;
   val  := 0;
        IF Length(word)=0           THEN Error('Missing operand')
   ELSE IF (word='.') OR (word='*') THEN val := locPtr
   ELSE IF  word='-'                THEN val := -Factor
   ELSE IF  word='+'                THEN val := Factor
   ELSE IF  word='~'                THEN val := -Factor-1
   ELSE IF  word='('                THEN BEGIN
                                            val := Eval;
                                            Expect(')');
                                         END
   ELSE IF  word=''''               THEN BEGIN
                                            IF Length(line)=0 THEN
                                               Error('Missing operand')
                                            ELSE BEGIN
                                               val := Ord(line[1]);
                                               Delete(line,1,1);
                                               Expect('''');
                                            END;
                                         END
   ELSE IF Pos(word[1],numeric)>0   THEN BEGIN
                  CASE word[Length(word)] OF
                     'O': val := EvalOct(Copy(word,1,Length(word)-1));
                     'D': val := EvalDec(Copy(word,1,Length(word)-1));
                     'H': val := EvalHex(Copy(word,1,Length(word)-1));
                     ELSE val := EvalDec(word);
                  END;
                                         END
   ELSE                                  val := RefSym(word);

   Factor := val;
END;


FUNCTION Term: Integer;
VAR
   word:    String;
   val:     Integer;
   oldLine: String;

BEGIN
   val := Factor;

   oldLine := line;
   word := GetWord;
   WHILE (word='*') OR (word='/') OR (word='%') DO BEGIN
      CASE word[1] OF
         '*': val := val  *  Factor;
         '/': val := val DIV Factor;
         '%': val := val MOD Factor;
      END;
      oldLine := line;
      word := GetWord;
   END;
   line := oldLine;

   Term := val;
END;


FUNCTION Eval: Integer;
VAR
   word:    String;
   val:     Integer;
   oldLine: String;

BEGIN
   val := Term;

   oldLine := line;
   word := GetWord;
   WHILE (word='+') OR (word='-') {OR (word='*') OR (word='/')} DO BEGIN
      CASE word[1] OF
         '+': val := val + Term;
         '-': val := val - Term;
      END;
      oldLine := line;
      word := GetWord;
   END;
   line := oldLine;

   Eval := val;
END;


FUNCTION EvalByte: Integer;
VAR
   val: Integer;

BEGIN
   val := Eval;

   IF (val<-128) OR (val>255) THEN
      Error('Byte out of range');

   EvalByte := val AND 255;
END;


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


PROCEDURE CodeOrg(addr: Integer);
BEGIN
   locPtr := addr;

   IF pass=2 THEN
      WriteLn(object,':',Hex4(addr));
END;


PROCEDURE CodeFlush;
BEGIN
    { Object file format does not use buffering; no flush needed }
END;


PROCEDURE CodeEnd;
BEGIN
   CodeFlush;

   IF (pass=2) AND xferFound THEN BEGIN
      WriteLn(object,'$',Hex4(xferAddr));
   END;
END;


PROCEDURE CodeXfer(addr: Integer);
BEGIN
   xferAddr  := addr;
   xferFound := TRUE;
END;


PROCEDURE DoOpcode(typ,parm: Integer);
VAR
   val:     Integer;
   reg1:    Integer;
   reg2:    Integer;
   word:    String;
   oldLine: String;

BEGIN
   CASE typ OF
      o_None:     BEGIN
                     instr[1] := parm;
                     instrLen := 1;
                  END;

      o_One:      BEGIN
                     instr[1] := parm;
                     instr[2] := EvalByte;
                     instrLen := 2;
                  END;

      o_Two:      BEGIN
                     val := Eval;
                     instr[1] := parm;
                     instr[2] := val AND 255;
                     instr[3] := val SHR 8;
                     instrLen := 3;
                  END;

      o_InrDcr:   BEGIN
                     reg1     := FindReg(GetWord,regs,regVals);
                     instr[1] := parm + reg1*8;
                     instrLen := 1;
                  END;

      o_Arith:    BEGIN
                     reg1     := FindReg(GetWord,regs,regVals);
                     instr[1] := parm + reg1;
                     instrLen := 1;
                  END;

      o_MOV:      BEGIN
                     reg1     := FindReg(GetWord,regs,regVals);
                     Comma;
                     reg2     := FindReg(GetWord,regs,regVals);
                     instr[1] := parm + reg1*8 + reg2;
                     instrLen := 1;
                  END;

      o_MVI:      BEGIN
                     reg1     := FindReg(GetWord,regs,regVals);
                     Comma;
                     instr[1] := parm + reg1*8;
                     instr[2] := EvalByte;
                     instrLen := 2;
                  END;

      o_LXI:      BEGIN
                     reg1     := FindReg(GetWord,regPairs,regPairVals);
                     Comma;
                     val      := Eval;
                     instr[1] := parm + reg1*16;
                     instr[2] := val AND 255;
                     instr[3] := val SHR 8;
                     instrLen := 3;
                  END;

      o_InxDcx:   BEGIN
                     reg1     := FindReg(GetWord,regPairs,regPairVals);
                     instr[1] := parm + reg1*16;
                     instrLen := 1;
                  END;

      o_PushPop:  BEGIN
                     reg1     := FindReg(GetWord,pushRegs,pushRegVals);
                     instr[1] := parm + reg1*16;
                     instrLen := 1;
                  END;

      o_StaxLdax: BEGIN
                     reg1     := FindReg(GetWord,staxRegs,staxRegVals);
                     instr[1] := parm + reg1*16;
                     instrLen := 1;
                  END;

      o_RST:      BEGIN
                     val := Eval;
                     CASE val OF
                        0,1,2,3,4,5,6,7:     val := val * 8;
                        8,16,24,32,40,48,56: ;
                        ELSE BEGIN
                           Error('Illegal restart number');
                           val := 0;
                        END;
                     END;
                     instr[1] := parm + val;
                     instrLen := 1;
                  END;

      o_DB:       BEGIN
                     oldLine := line;
                     word := GetWord;
                     IF word='''' THEN BEGIN
                        val := Pos('''',line);
                        IF val=0 THEN BEGIN
                           bytStr := line;
                           line   := '';
                        END
                        ELSE BEGIN
                           bytStr := Copy(line,1,val-1);
                           Delete(line,1,val);
                        END;
                        instrLen := -Length(bytStr);
                     END
                     ELSE BEGIN
                        line     := oldLine;
                        instr[1] := EvalByte;
                        instrLen := 1;
                     END;
                  END;

      o_DW:       BEGIN
                     val      := Eval;
                     instr[1] := val AND 255;
                     instr[2] := val SHR 8;
                     instrLen := 2;
                  END;

      o_DS:       BEGIN
                     val := Eval;

                     IF pass=2 THEN BEGIN
                        showAddr := FALSE;
                        Delete(listLine,1,13);
                        listLine := Hex4(locPtr) + ':  (' + Hex4(val) + ')'
                                                 + listLine;
                     END;

                     val := val + locPtr;
                     CodeOrg(val);
                  END;

      o_END:     BEGIN
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

      ELSE Error('Unknown opcode');
   END;
END;


PROCEDURE DoLabelOp(typ,parm: Integer; labl: SymStr);
VAR
   val:  Integer;
   word: String;

BEGIN
   CASE typ OF
      o_EQU:   BEGIN
                  IF Length(labl)=0 THEN
                     Error('Missing label')
                  ELSE BEGIN
                     val := Eval;

                     listLine := Copy(listLine,1,6) + '= ' + Hex4(val) +
                                 Copy(listLine,13,255);

                     DefSym(labl,val,parm=1,parm=0);
                  END;
               END;


      o_ORG:   BEGIN
                  CodeOrg(Eval);
                  DefSym(labl,locPtr,FALSE,FALSE);
                  showAddr := TRUE;
               END;

      o_LIST:  BEGIN
                  listThisLine := TRUE;

                  IF Length(labl)<>0 THEN
                     Error('Label not allowed');

                  word := GetWord;
                       IF word='ON'  THEN listFlag := TRUE
                  ELSE IF word='OFF' THEN listFlag := FALSE
                  ELSE                    Error('Illegal operand');
               END;

      o_OPT:   BEGIN
                  listThisLine := TRUE;

                  IF Length(labl)<>0 THEN
                     Error('Label not allowed');

                  word := GetWord;
                       IF word='LIST'   THEN listFlag := TRUE
                  ELSE IF word='NOLIST' THEN listFlag := FALSE
                  ELSE                       Error('Illegal option');
               END;

      ELSE Error('Unknown opcode');
   END;
END;


PROCEDURE ListOut;
VAR
   i: Integer;

BEGIN
   IF Deblank(listLine) = #12 THEN
      WriteLn(listing,#12)

   ELSE IF Deblank(listLine)='' THEN
      WriteLn(listing)

   ELSE BEGIN
      i := Length(listLine);
      WHILE (i>0) AND (listLine[i]=' ') DO
         Dec(i);
      listLine[0] := CHR(i);

      WriteLn(listing,listLine);
      IF errFlag AND cl_Err THEN
         WriteLn(listLine);
   END;
END;


PROCEDURE DoPass;
VAR
   labl:    SymStr;
   opcode:  OpcdStr;
   typ:     Integer;
   parm:    Integer;
   i:       Integer;
   word:    String;

BEGIN
   Assign(source,cl_SrcName);
   Reset(source);
   sourceEnd := FALSE;

   WriteLn('Pass ',pass);

   CodeOrg(0);
   errCount := 0;
   listFlag := TRUE;

   WHILE (NOT Eof(source)) AND (NOT SourceEnd) DO BEGIN
      ReadLn(source,line);

      errFlag      := FALSE;
      instrLen     := 0;
      showAddr     := FALSE;
      listThisLine := ListFlag;
      listLine     := '                '; { 16 blanks }

      IF Pass=2 THEN listLine := Copy(listLine,1,16) + line;

      labl := '';

      IF Length(line)>0 THEN
         IF Pos(line[1],white)=0 THEN BEGIN
            labl := GetWord;
            showAddr := (Length(labl)<>0);

            IF Length(line)>0 THEN
               IF line[1]=':' THEN
                  Delete(line,1,1);

         END;

      opcode := GetWord;
      IF Length(opcode)=0 THEN BEGIN
         typ := 0;
         DefSym(labl,locPtr,FALSE,FALSE);
      END
      ELSE BEGIN
         FindOpcode(opcode,typ,parm);

         IF typ=o_Illegal THEN Error('Illegal opcode "' +
                                       Deblank(opcode) + '"')
         ELSE IF typ<0         THEN BEGIN
                                       showAddr := FALSE;
                                       DoLabelOp(typ,parm,labl);
                                    END
         ELSE                       BEGIN
                                       showAddr := TRUE;
                                       DefSym(labl,locPtr,FALSE,FALSE);
                                       DoOpcode(typ,parm);
         END;

         IF typ<>o_Illegal THEN
            IF Length(GetWord)>0 THEN
               Error('Too many operands');
      END;

      IF Pass=2 THEN BEGIN
         IF ShowAddr THEN
            listLine := Hex4(locPtr) + ':' + Copy(listLine,6,255);

         IF instrLen>0 THEN
            FOR i := 1 TO instrLen DO BEGIN
               word := Hex2(instr[i]);
               listLine[i*3+4] := word[1];
               listLine[i*3+5] := word[2];
               CodeOut(instr[I]);
            END
         ELSE FOR i := 1 TO -instrLen DO BEGIN
            IF I<=3 THEN BEGIN
               word := Hex2(ORD(bytStr[i]));
               listLine[i*3+4] := word[1];
               listLine[i*3+5] := word[2];
            END;
            CodeOut(ORD(bytStr[i]));
         END;

         IF listThisLine THEN ListOut;
      END;

      locPtr := locPtr + ABS(instrLen);
   END;

   IF Pass=2 THEN CodeEnd;

   { Put the lines after the END statement into the listing file   }
   { while still checking for listing control statements.  Ignore  }
   { any lines which have invalid syntax, etc., because whatever   }
   { is found after an END statement should esentially be ignored. }

   IF Pass=2 THEN
      WHILE NOT Eof(source) DO BEGIN
         listThisLine := listFlag;
         listLine := '                ' + line; { 16 blanks }

         IF Length(line)>0 THEN
            IF Pos(line[1],white)<>0 THEN BEGIN
               word := GetWord;
               IF Length(word)<>0 THEN BEGIN
                   IF word='LIST' THEN
                      BEGIN
                         listThisLine := TRUE;
                         word := GetWord;

                              IF word='ON'  THEN listFlag := TRUE
                         ELSE IF word='OFF' THEN listFlag := FALSE
                         ELSE                    listThisLine := listFlag;
                      END

                   ELSE IF word='OPT' THEN
                      BEGIN
                         listThisLine := TRUE;
                         word := GetWord;

                              IF word='LIST'   THEN listFlag := TRUE
                         ELSE IF word='NOLIST' THEN listFlag := FALSE
                         ELSE                       listThisLine := listFlag;
                      END;
               END;
            END;

         IF listThisLine THEN ListOut;
      END;

   Close(source);
END;


PROCEDURE SortSymTab;
VAR
   i,j,t:  SymPtr;
   sorted: Boolean;
   temp:   SymRec;

BEGIN
   IF symTab<>NIL THEN BEGIN

      i := symTab;
      j := i^.next;
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
END;


PROCEDURE DumpSym(p: SymPtr);
BEGIN
   Write(listing,p^.name:maxSymLen,' ',Hex4(p^.value));

   IF NOT p^.defined  THEN Write(listing,' U');
   IF     p^.multiDef THEN Write(listing,' M');
   IF     p^.isSet    THEN Write(listing,' S');
   IF     p^.equ      THEN Write(listing,' E');

   WriteLn(listing);
END;


PROCEDURE DumpSymTab;
VAR
   p: SymPtr;

BEGIN
   SortSymTab;

   p := symTab;
   WHILE (p<>NIL) DO BEGIN
      DumpSym(p);
      p := p^.next;
   END;
END;


PROCEDURE ShowOptions;
BEGIN
   WriteLn;
   WriteLn('  Command line syntax:');
   WriteLn;
   WriteLn('  ASM8080 [options] src [options]');
   WriteLn;
   WriteLn('  Valid options:');
   WriteLn;
   WriteLn('    -E  Show errors to screen');
   WriteLn('    -L  Make a listing file to src.LIS');
   WriteLn('    -L=name');
   WriteLn('    -O  Make an object file to src.OBJ');
   WriteLn('    -O=name');
   WriteLn;
END;


FUNCTION GetOption(VAR optStr: String): String;
VAR
   option: String[80];
   p:      Integer;

BEGIN
   optStr := Deblank(optStr);

   p := Pos(' ',optStr);

   IF p=0 THEN BEGIN
      option := optStr;
      optStr := '';
   END
   ELSE BEGIN
      option := Copy(optStr,1,p-1);
      optStr := Copy(optStr,p+1,255);
   END;

   optStr := UprCase(Deblank(optStr));

   GetOption := option;
END;


FUNCTION GetOptions(VAR cl_SrcName, cl_ListName,cl_ObjName: String; VAR cl_Err: Boolean): Boolean;
VAR
   s:       String;
   len:     Integer;
   optStr:  String;
   option:  String;
   optParm: String;
   prefix:  String;
   p:       Integer;
   err:     Integer;
   optErr:  Boolean;
   i:       Integer;

BEGIN
   cl_SrcName  := '';
   cl_ListName := 'NUL';
   cl_ObjName  := 'NUL';
   cl_Err      := FALSE;

   optErr := FALSE;
   optStr := ParamStr(1);
   FOR i := 2 TO ParamCount DO
      optStr := optStr + ' ' + ParamStr(i);

   option := GetOption(optStr);
   WHILE Length(option)<>0 DO BEGIN
      optParm := '';

      p := Pos('=',option);
      IF p>0 THEN BEGIN
         optParm := Copy(option,p+1,255);
         option  := Copy(option,1,p-1);
      END;

           IF option = '-L' THEN cl_ListName := optParm
      ELSE IF option = '-O' THEN cl_ObjName  := optParm
      ELSE IF option = '-E' THEN cl_Err      := TRUE
      ELSE IF option = '?'  THEN optErr      := TRUE
      ELSE BEGIN
         IF (Copy(option,1,1)='-') OR (Length(cl_SrcName)<>0) OR
            (Length(optParm)<>0) THEN BEGIN
            optErr := TRUE;
            WriteLn('Illegal command line option:  ',option);
         END
         ELSE BEGIN
            cl_SrcName := option;
            IF Pos('.',cl_SrcName)=0 THEN
               IF p=0 THEN cl_SrcName := cl_SrcName + '.ASM';

            p := Pos('.',option);
            IF p=0 THEN prefix := option
                   ELSE prefix := Copy(option,1,p-1);
         END;
      END;

      option := GetOption(optStr);
   END;

   IF cl_SrcName = '' THEN BEGIN
      optErr := TRUE;
      WriteLn('Source file not specified')
   END;

   IF cl_ListName = '' THEN cl_ListName := prefix + '.LIS';
   IF cl_ObjName  = '' THEN cl_ObjName  := prefix + '.DAT';
   IF Copy(cl_ListName,1,1)='.' THEN cl_ListName := prefix + cl_ListName;
   IF Copy(cl_ObjName ,1,1)='.' THEN cl_ObjName  := prefix + cl_ObjName;

   GetOptions := optErr;
END;


BEGIN
   IF GetOptions(cl_SrcName,cl_ListName,cl_ObjName,cl_Err) THEN BEGIN
      ShowOptions;
      Halt;
   END;

   Assign(listing,cl_ListName);
   Rewrite(listing);
   Assign(object,cl_ObjName);
   Rewrite(object);

   symTab    := NIL;
   xferAddr  := 0;
   xferFound := FALSE;
   InitOpcodes;

   pass := 1;
   DoPass;

   pass := 2;
   DoPass;

   WriteLn(listing);
   WriteLn(listing,errCount:5,' Total Error(s)');
   WriteLn(listing);

   IF cl_Err THEN BEGIN
      WriteLn;
      WriteLn(errCount:5,' Total Error(s)');
   END;

   DumpSymTab;

   Close(listing);
   Close(object);
END.
