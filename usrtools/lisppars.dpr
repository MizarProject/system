(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)
// Emacs Lisp output of formula skeletons

// Sentences are printed to the .lsp file as Lisp lists.
// Atomic formulas and quantification segments are just strings.

program LispPars;

uses pcmizver,mizenv,mconsole,errhan,monitor,syntax,parser,mscanner,lispscan,
    inout
    {$IFDEF MDEBUG} ,info {$ENDIF};

// Standard parsing stuff
type
LispItemPtr = ^LispItemObj;
 LispItemObj =
  object(ItemObj)
    nCanceledNbr:integer;
   constructor Init(fKind:ItemKind);
   procedure CreateExpression(fExpKind:ExpKind); virtual;
   procedure StartSentence; virtual;
   procedure FinishSentence; virtual;
  end;

 LispBlockPtr = ^LispBlockObj;
 LispBlockObj =
  object(BlockObj)
   constructor Init(fBlockKind:BlockKind);
   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
  end;

LispExpressionPtr = ^LispExpressionObj;
LispExpressionObj =
  object(ExpressionObj)
  constructor Init(fExpKind:ExpKind);
  procedure CreateSubexpression; virtual;
  end;

LispSubexpPtr = ^LispSubexpObj;
LispSubexpObj =
  object(SubexpObj)
   constructor Init;
   destructor Done; virtual;
   procedure StartQualifiedSegment; virtual;
   procedure FinishQualifiedSegment; virtual;   
   procedure FinishQuantified; virtual;
   procedure StartAtomicFormula; virtual;
   procedure FinishPredicativeFormula; virtual;
   procedure FinishQualifyingFormula; virtual;
   procedure FinishAttributiveFormula; virtual;
   procedure StartPrivateFormula; virtual;
   procedure FinishPrivateFormula; virtual;
   procedure StartExistential; virtual;
   procedure StartUniversal; virtual;
   procedure ProcessLeftParenthesis; virtual;
   procedure ProcessRightParenthesis; virtual;
  end;

// Standard stuff
constructor LispItemObj.Init(fKind : ItemKind);
begin inherited Init(fKind); end;
constructor LispBlockObj.Init(fBlockKind:BlockKind);
begin inherited Init(fBlockKind); end;
procedure LispItemObj.CreateExpression(fExpKind:ExpKind);
begin gExpPtr:=new(LispExpressionPtr,Init(fExpKind)); end;
procedure LispBlockObj.CreateItem(fItemKind:ItemKind);
begin gItemPtr:=new(LispItemPtr,Init(fItemKind)); end;
procedure LispBlockObj.CreateBlock(fBlockKind:BlockKind);
begin gBlockPtr:=new(LispBlockPtr,Init(fBlockKind)) end;
constructor LispExpressionObj.Init(fExpKind:ExpKind);
begin inherited Init(fExpKind); end;
procedure LispExpressionObj.CreateSubexpression;
begin gSubexpPtr:=new(LispSubexpPtr,Init) end;
constructor LispSubexpObj.Init;
begin inherited Init end;
destructor LispSubexpObj.Done;
begin gSubexpPtr:=LispSubexpPtr(Previous); end;
procedure InitArticle;	   
begin gBlockPtr:=new(LispBlockPtr, Init(blMain));end; 

// If > 0, everything printable is printed as it goes,
// otherwise the logical connectives are treated specially.
var Ignored:integer = 0; 


procedure Paren;
begin
 if (PrintLevel > 0) and (Ignored = 0) then
  gPrintedStr:= gPrintedStr + '(';
end;


procedure Unparen;
begin
 if (PrintLevel > 0) and (Ignored = 0) then
  gPrintedStr:= gPrintedStr + ') ';
end;

// Quoting if printing
procedure Quote;
begin if (PrintLevel > 0) then
 begin
  if (Ignored = 0) then gPrintedStr:= gPrintedStr + '"';
  inc(Ignored);
 end;
end;

// Unquoting if printing
procedure Unquote;
begin if (PrintLevel > 0) then
 begin
  if (Ignored = 1) then gPrintedStr:= gPrintedStr + '" ';
  dec(Ignored);
 end;
end; { Unquote }
 

var PBal: integer = 0; 	// paranthesis balance
var AtBal: integer = 0; // balance for current atomic fla
var AtPos: integer = 0; // '"' position for curent atomic fla 

// Quoting of atomic if printing
// we save the postion of the '"' and the balance
procedure QuoteAtom;
begin if (PrintLevel > 0) then
 begin
  if (Ignored = 0) then
  begin
   gPrintedStr	:= gPrintedStr + '"';
   AtBal	:= PBal;
   AtPos	:= Length(gPrintedStr);
  end;
  inc(Ignored);
 end;
end;

// Unquoting of atomic if printing
procedure UnquoteAtom;
var i:integer;
begin if (PrintLevel > 0) then
 begin
  if (Ignored = 1) then
  begin      
   gPrintedStr:= gPrintedStr + '" ';
   if AtBal > PBal then
   begin
    Mizassert(9000, gPrintedStr[AtPos] = '"');
    gPrintedStr[AtPos] := '(';
    i:= 1;
    while AtBal > PBal do
    begin
     Mizassert(9000, AtPos > i);
     Mizassert(9000, gPrintedStr[AtPos - i] in ['(',' ']);
     if gPrintedStr[AtPos - i] = '(' then dec(AtBal);
     inc(i);
    end;
    gPrintedStr[1 + AtPos - i] := '"';
   end;
  end;
  dec(Ignored);
 end;
end;

procedure LispSubexpObj.ProcessLeftParenthesis;
begin inc(PBal); end;
procedure LispSubexpObj.ProcessRightParenthesis; begin dec(PBal); end;

// Atomic flas start and end quoting
procedure LispSubexpObj.StartAtomicFormula;begin QuoteAtom; end;
procedure LispSubexpObj.StartPrivateFormula;begin Quote; end;
procedure LispSubexpObj.FinishPredicativeFormula;begin UnquoteAtom; end;
procedure LispSubexpObj.FinishQualifyingFormula;begin UnquoteAtom; end;
procedure LispSubexpObj.FinishAttributiveFormula;begin UnquoteAtom; end;
procedure LispSubexpObj.FinishPrivateFormula;begin Unquote; end;

// End of quantification ends quoting
procedure LispSubexpObj.FinishQuantified;begin Unparen; end;

procedure LispSubexpObj.StartQualifiedSegment;begin Paren; Quote; end;
procedure LispSubexpObj.FinishQualifiedSegment;begin Unquote; Unparen; end;

// Quantified flas have to handle their quentification
procedure LispSubexpObj.StartUniversal;
begin if (PrintLevel > 0) and (Ignored = 0) then
begin gPrintedStr:= gPrintedStr + 'for (Q ';  SkipFirst:=true;end;
end;

procedure LispSubexpObj.StartExistential;
begin if (PrintLevel > 0) and (Ignored = 0) then
begin gPrintedStr:= gPrintedStr + 'ex (Q ';  SkipFirst:=true;end;
end;


// All is done for sentences only
procedure LispItemObj.StartSentence;
begin
 gPrintedStr:= '((pos ' + IntToStr(CurPos.Line) + ' '
  + IntToStr(CurPos.Col) + ') ';
 PBal:= 0; AtBal:= 0;
 PrintLevel:=1;  Ignored:=0;
end;

procedure LispItemObj.FinishSentence;
begin
 gPrintedStr:= gPrintedStr + ')' + #10;
 OutFile.OutString(gPrintedStr);
 gPrintedStr:= '';
 PrintLevel:=0; Ignored:=1;
end;

begin
 DrawMizarScreen('Lisp Parser');
 InitExitProc;
 GetArticleName; GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 InitArticle;
 InitDisplayLine('Parsing '+MizFileName);
 FileExam(EnvFileName+'.dct');
// FileExam(EnvFileName+'.prf');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 ReadTokenProc:=LispReadToken;
 OutFile.OpenFile(MizFileName+'.lsp');
 Parse;
 FinishScanning;
 OutFile.Done;
 DrawErrorsMsg(ErrorNbr);
 FinishDrawing;
end.
