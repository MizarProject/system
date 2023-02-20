(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit formats;

interface

uses mobjects,inout,dicthan,_formats;

type
 FormatPtr = MFormatPtr;

 PrefixFormatPtr =  MPrefixFormatPtr;

 InfixFormatPtr = MInfixFormatPtr;

 BracketFormatPtr = MBracketFormatPtr;

 FormatsListPtr = MFormatsListPtr;

 FormatsList= MFormatsList;

function In_Format( var fInFile: MizXInStream): FormatPtr;
procedure Out_Format(var fOutFile: MizXOutStream; fForm: FormatPtr; aFormNr: integer);

// tells if SymbolTrans should be used for tranlsating symbol
// numbers, used in transferer
var
  DoStrans:boolean = false;
  SymbolTrans: ^SymbolIntSeqArr;

// global symbol renumbering
function STrans(fSymbolKind:char; fSymbolNr:integer): integer;

implementation

uses errhan,xmldict,xmlpars
{$IFDEF MDEBUG} ,info {$ENDIF};


{global symbol renumbering}
function STrans(fSymbolKind:char; fSymbolNr:integer): integer;
begin
  if DoSTrans then
  begin
    STrans:=SymbolTrans^[fSymbolKind].Value(fSymbolNr)
  end
  else
    STrans:= fSymbolNr;
end; { STrans }


function In_Format( var fInFile: MizXInStream): FormatPtr;
var
 lLex: Lexem;
 lArgsNbr,lLeftArgsNbr,lRightSymNr:integer;
begin
 with fInFile do
 begin
  GetLexemAttrs( atKind, atSymbolNr, lLex);
  lArgsNbr:= GetIntAttr( atArgNr);
  case lLex.Kind of
   'O','R':
    begin
     lLeftArgsNbr:= GetIntAttr( atLeftArgNr);
     In_Format:= new(InfixFormatPtr, Init(lLex.Kind,lLex.Nr,lLeftArgsNbr,
                                          lArgsNbr - lLeftArgsNbr));
    end;
   'J','U','V','G','L','M':
     In_Format:= new(PrefixFormatPtr,Init(lLex.Kind,lLex.Nr,lArgsNbr));
   'K':
    begin
     lRightSymNr:= GetIntAttr( atRightSymbolNr);
     In_Format:= new(BracketFormatPtr, Init(lLex.Nr, lRightSymNr,
                                            lArgsNbr, 0, 0));
    end;
  else RunTimeError(2019);
  end;
  AcceptEndState;
  NextElementState;
 end;
end;

// ##TODO: since STrans is needed, OutMMLFileObj must be used;
//         so either include iocorrel or move this there
// ##TODO: Is 'J' still used
// if aFormNr = 0, the atNr is not printed
(* ##RNC:
## Format keeps the kind of a given symbol and arities.
## For bracket formats (K) this keeps both symbols.   
## Optionally a nr (of the format) is kept, to which patterns may refer,
## This implementation might change in some time.
elFormat = 
 element elFormat {
   attribute atKind {'G'|'K'|'J'|'L'|'M'|'O'|'R'|'U'|'V'},
   attribute atNr { xsd:integer }?,
   attribute atSymbolNr { xsd:integer },
   attribute atArgNr { xsd:integer },
   attribute atLeftArgNr { xsd:integer }?,
   attribute atRightSymbolNr { xsd:integer }?
 }
*)

procedure Out_Format( var fOutFile: MizXOutStream; fForm: FormatPtr; aFormNr: integer);
begin
 with fOutFile,fForm^ do
 begin
  Out_XElStart( elFormat);
  Out_XAttr( atKind, fSymbol.Kind);
  if aFormNr > 0 then Out_XIntAttr( atNr, aFormNr);
  if fSymbol.Kind in ['L','J'] then
    Out_XIntAttr( atSymbolNr, STrans( 'G', fSymbol.Nr))
  else Out_XIntAttr( atSymbolNr, STrans( fSymbol.Kind, fSymbol.Nr));
  case fSymbol.Kind of
   'J','U','V','G','L','M':
    Out_XIntAttr( atArgNr, PrefixFormatPtr(fForm)^.fRightArgsNbr);
   'O','R':
    with InfixFormatPtr(fForm)^ do
     begin
      Out_XIntAttr( atArgNr, fLeftArgsNbr+fRightArgsNbr);
      Out_XIntAttr( atLeftArgNr, fLeftArgsNbr);
     end;
   'K':
    with BracketFormatPtr(fForm)^ do
     begin
      Out_XIntAttr( atArgNr, fArgsNbr);
      Out_XIntAttr( atRightSymbolNr, STrans( 'L', fRightSymbolNr));
     end;
  else RuntimeError(3300);
  end;
  Out_XElEnd0;
 end;
end;
  

end.

