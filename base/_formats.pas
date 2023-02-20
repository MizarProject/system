(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit _formats;

interface

uses mobjects,scanner,dicthan,xml_inout;

type
 MFormatPtr = ^MFormatObj;
 MFormatObj = object(MObject)
    fSymbol: LexemRec;
  constructor Init(aKind:Char; aSymNr:integer);
  procedure Out_Format( var fOutFile: XMLOutStreamObj; aFormNr: integer);
 end;

 // ##TODO: add assertions that nr. of all format arguments is equal
 //         to the number of visible args (Visible) of a pattern
 MPrefixFormatPtr =  ^MPrefixFormatObj;
 MPrefixFormatObj =  object(MFormatObj)
    fRightArgsNbr: byte;
   constructor Init(aKind:Char; aSymNr,aRArgsNbr:integer);
 end;

 MInfixFormatPtr = ^MInfixFormatObj;
 MInfixFormatObj = object(MPrefixFormatObj)
    fLeftArgsNbr: byte;
   constructor Init(aKind:Char; aSymNr,aLArgsNbr,aRArgsNbr:integer);
 end;

 MBracketFormatPtr = ^MBracketFormatObj;
 MBracketFormatObj = object(MInfixFormatObj)
    fRightSymbolNr: integer;
    fArgsNbr: byte;
   constructor Init(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr:integer);
  end;

 MFormatsListPtr = ^MFormatsList;
 MFormatsList = object(MSortedList)

    constructor Init(ALimit: Integer);

    constructor LoadFormats(fName:string);
    procedure StoreFormats(fName:string);

    function LookUp_PrefixFormat(aKind:char;
                                 aSymNr,aArgsNbr:integer):integer;
    function LookUp_FuncFormat(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;
    function LookUp_BracketFormat(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr:integer): integer;
    function LookUp_PredFormat(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;

    function CollectFormat(aFormat: MFormatPtr): integer;
    function CollectPrefixForm(aKind:char;
                               aSymNr,aArgsNbr:integer): integer;
    function CollectFuncForm(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;
    function CollectBracketForm(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr:integer): integer;
    function CollectPredForm(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;

  end;

function CompareFormats(aItem1, aItem2: Pointer): Integer;

function In_Format(fInFile: XMLInStreamPtr): MFormatPtr;

var gFormatsColl: MFormatsList;
    gPriority: BinIntFunc;
    gFormatsBase: integer;

implementation

uses errhan,xml_dict,xml_parser
{$IFDEF MDEBUG} ,info {$ENDIF};

constructor MFormatObj.Init(aKind:Char; aSymNr:integer);
begin
 fSymbol.Kind:=aKind;
 fSymbol.Nr:=aSymNr;
end;

constructor MPrefixFormatObj.Init(aKind:Char; aSymNr,aRArgsNbr:integer);
begin
 fSymbol.Kind:=aKind;
 fSymbol.Nr:=aSymNr;
 fRightArgsNbr:=aRArgsNbr;
end;

constructor MInfixFormatObj.Init(aKind:Char; aSymNr,aLArgsNbr,aRArgsNbr:integer);
begin
 fSymbol.Kind:=aKind;
 fSymbol.Nr:=aSymNr;
 fLeftArgsNbr:=aLArgsNbr;
 fRightArgsNbr:=aRArgsNbr;
end;

constructor MBracketFormatObj.Init(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr:integer);
begin
 fSymbol.Kind:='K';
 fSymbol.Nr:=aLSymNr;
 fRightSymbolNr:=aRSymNr;
 fArgsNbr:=aArgsNbr;
 fLeftArgsNbr:=aLArgsNbr;
 fRightArgsNbr:=aRArgsNbr;
end;

function CompareFormats(aItem1, aItem2: Pointer): Integer;
begin CompareFormats:=1;
 if MFormatPtr(aItem1)^.fSymbol.Kind < MFormatPtr(aItem2)^.fSymbol.Kind then
  CompareFormats := -1
 else if MFormatPtr(aItem1)^.fSymbol.Kind = MFormatPtr(aItem2)^.fSymbol.Kind then
  if MFormatPtr(aItem1)^.fSymbol.Nr < MFormatPtr(aItem2)^.fSymbol.Nr then
   CompareFormats := -1
  else if MFormatPtr(aItem1)^.fSymbol.Nr = MFormatPtr(aItem2)^.fSymbol.Nr then
   case MFormatPtr(aItem1)^.fSymbol.Kind of
   'J','U':
     CompareFormats := 0;
   'G','L','M','V':
     if MPrefixFormatPtr(aItem1)^.fRightArgsNbr < MPrefixFormatPtr(aItem2)^.fRightArgsNbr then
      CompareFormats := -1
     else if MPrefixFormatPtr(aItem1)^.fRightArgsNbr = MPrefixFormatPtr(aItem2)^.fRightArgsNbr then
      CompareFormats := 0;
   'O','R':
     if MInfixFormatPtr(aItem1)^.fLeftArgsNbr < MInfixFormatPtr(aItem2)^.fLeftArgsNbr then
      CompareFormats := -1
     else if MInfixFormatPtr(aItem1)^.fLeftArgsNbr = MInfixFormatPtr(aItem2)^.fLeftArgsNbr then
       if MInfixFormatPtr(aItem1)^.fRightArgsNbr < MInfixFormatPtr(aItem2)^.fRightArgsNbr then
        CompareFormats := -1
       else if MInfixFormatPtr(aItem1)^.fRightArgsNbr = MInfixFormatPtr(aItem2)^.fRightArgsNbr then
        CompareFormats := 0;
   'K':
     if MBracketFormatPtr(aItem1)^.fRightSymbolNr < MBracketFormatPtr(aItem2)^.fRightSymbolNr then
      CompareFormats := -1
     else if MBracketFormatPtr(aItem1)^.fRightSymbolNr = MBracketFormatPtr(aItem2)^.fRightSymbolNr then
       if MBracketFormatPtr(aItem1)^.fArgsNbr < MBracketFormatPtr(aItem2)^.fArgsNbr then
        CompareFormats := -1
       else if MBracketFormatPtr(aItem1)^.fArgsNbr = MBracketFormatPtr(aItem2)^.fArgsNbr then
        if MInfixFormatPtr(aItem1)^.fLeftArgsNbr < MInfixFormatPtr(aItem2)^.fLeftArgsNbr then
         CompareFormats := -1
        else if MInfixFormatPtr(aItem1)^.fLeftArgsNbr = MInfixFormatPtr(aItem2)^.fLeftArgsNbr then
         if MInfixFormatPtr(aItem1)^.fRightArgsNbr < MInfixFormatPtr(aItem2)^.fRightArgsNbr then
          CompareFormats := -1
         else if MInfixFormatPtr(aItem1)^.fRightArgsNbr = MInfixFormatPtr(aItem2)^.fRightArgsNbr then
          CompareFormats := 0;
   end;
end;

const PrefixFormatChars = [ 'M', 'V', 'U', 'J', 'L', 'G'];
function MFormatsList.LookUp_PrefixFormat(aKind:char;
                                         aSymNr,aArgsNbr:integer):integer;
var lFormat:MPrefixFormatObj; i: integer;
begin
 Mizassert( 3300, aKind in PrefixFormatChars);
 lFormat.Init( aKind, aSymNr, aArgsNbr);
 if Find(@lFormat,i) then
  LookUp_PrefixFormat:=fIndex^[i]+1
 else LookUp_PrefixFormat:=0;
end;

function MFormatsList.LookUp_FuncFormat(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;
 var lFormat:MInfixFormatObj; i: integer;
begin
 lFormat.Init('O',aSymNr,aLArgsNbr,aRArgsNbr);
 if Find(@lFormat,i) then
  LookUp_FuncFormat:=fIndex^[i]+1
 else LookUp_FuncFormat:=0;
end;

function MFormatsList.LookUp_BracketFormat(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr:integer): integer;
 var lFormat:MBracketFormatObj; i: integer;
begin
 lFormat.Init(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr);
 if Find(@lFormat,i) then
  LookUp_BracketFormat:=fIndex^[i]+1
 else LookUp_BracketFormat:=0;
end;

function MFormatsList.LookUp_PredFormat(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;
 var lFormat:MInfixFormatObj; i: integer;
begin
 lFormat.Init('R',aSymNr,aLArgsNbr,aRArgsNbr);
 if Find(@lFormat,i) then
  LookUp_PredFormat:=fIndex^[i]+1
 else LookUp_PredFormat:=0;
end;

function MFormatsList.CollectFormat(aFormat: MFormatPtr): integer;
 var lFormatNr,i:integer;
begin
 lFormatNr:=0;
 if not Find(aFormat,i) then
  begin lFormatNr:=Count+1;
   Insert(aFormat);
  end;
 CollectFormat:=lFormatNr;
end;

function MFormatsList.CollectBracketForm(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr:integer): integer;
 var lFormatNr:integer;
begin
 lFormatNr:=LookUp_BracketFormat(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr);
 if lFormatNr = 0 then
  begin lFormatNr:=Count+1;
   Insert(new(MBracketFormatPtr,Init(aLSymNr,aRSymNr,aArgsNbr,aLArgsNbr,aRArgsNbr)));
  end;
 CollectBracketForm:=lFormatNr;
end;

function MFormatsList.CollectFuncForm(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;
 var lFormatNr:integer;
begin lFormatNr:=LookUp_FuncFormat(aSymNr,aLArgsNbr,aRArgsNbr);
 if lFormatNr = 0 then
  begin lFormatNr:=Count+1;
   Insert(new(MInfixFormatPtr,Init('O',aSymNr,aLArgsNbr,aRArgsNbr)));
  end;
 CollectFuncForm:=lFormatNr;
end;

function MFormatsList.CollectPrefixForm(aKind:char;
                                        aSymNr,aArgsNbr:integer): integer;
var lFormatNr:integer;
begin lFormatNr:=LookUp_PrefixFormat(aKind,aSymNr,aArgsNbr);
 if lFormatNr = 0 then
  begin lFormatNr:=Count+1;
   Insert(new(MPrefixFormatPtr,Init(aKind,aSymNr,aArgsNbr)));
  end;
 CollectPrefixForm:=lFormatNr;
end;

function MFormatsList.CollectPredForm(aSymNr,aLArgsNbr,aRArgsNbr:integer): integer;
 var lFormatNr:integer;
begin
 lFormatNr:=LookUp_PredFormat(aSymNr,aLArgsNbr,aRArgsNbr);
 if lFormatNr = 0 then
  begin lFormatNr:=Count+1;
   Insert(new(MInfixFormatPtr,Init('R',aSymNr,aLArgsNbr,aRArgsNbr)));
  end;
 CollectPredForm:=lFormatNr;
end;

constructor MFormatsList.Init(ALimit: Integer);
begin
 InitSorted(ALimit,CompareFormats);
end;

constructor MFormatsList.LoadFormats(fName:string);
var
 lEnvFile: XMLInStreamPtr;
 lValue: integer; lLex: LexemRec;
begin
 InitSorted(100,CompareFormats);
 lEnvFile:=new(XMLInStreamPtr,OpenFile(fName));
 with lEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElName = XMLElemName[elFormats]);
  NextElementState;
  while not (nState = eEnd) and (nElName = XMLElemName[elFormat]) do
   Insert( In_Format( lEnvFile));
  gPriority.Init(10);
  while not (nState = eEnd) do
  begin
   XMLASSERT(nElName = XMLElemName[elPriority]);
   lLex.Kind:=GetAttr(XMLAttrName[ atKind])[1];
   lLex.Nr:=GetIntAttr(XMLAttrName[ atSymbolNr]);
   MizAssert(3300, lLex.Kind in ['O','L','K']);
   lValue:= GetIntAttr( XMLAttrName[ atValue]);
   gPriority.Assign( ord( lLex.Kind), lLex.Nr, lValue);
   AcceptEndState;
   NextElementState;
  end;
 end;
 dispose(lEnvFile,Done);
end;

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

function In_Format(fInFile: XMLInStreamPtr): MFormatPtr;
var
 lLex: LexemRec;
 lArgsNbr,lLeftArgsNbr,lRightSymNr:integer;
begin
 with fInFile^ do
 begin
  lLex.Kind:=GetAttr(XMLAttrName[ atKind])[1];
  lLex.Nr:=GetIntAttr(XMLAttrName[ atSymbolNr]);
  lArgsNbr:= GetIntAttr( XMLAttrName[ atArgNr]);
  case lLex.Kind of
   'O','R':
    begin
     lLeftArgsNbr:= GetIntAttr( XMLAttrName[ atLeftArgNr]);
     In_Format:= new(MInfixFormatPtr, Init(lLex.Kind,lLex.Nr,lLeftArgsNbr,
                                          lArgsNbr - lLeftArgsNbr));
    end;
   'J','U','V','G','L','M':
     In_Format:= new(MPrefixFormatPtr,Init(lLex.Kind,lLex.Nr,lArgsNbr));
   'K':
    begin
     lRightSymNr:= GetIntAttr( XMLAttrName[ atRightSymbolNr]);
     In_Format:= new(MBracketFormatPtr, Init(lLex.Nr, lRightSymNr,
                                            lArgsNbr, 0, 0));
    end;
  else RunTimeError(2019);
  end;
  AcceptEndState;
  NextElementState;
 end;
end;

procedure MFormatObj.Out_Format( var fOutFile: XMLOutStreamObj; aFormNr: integer);
begin
 with fOutFile do
 begin
  Out_XElStart( XMLElemName[elFormat]);
  Out_XAttr( XMLAttrName[atKind], fSymbol.Kind);
  if aFormNr > 0 then Out_XIntAttr( XMLAttrName[atNr], aFormNr);
  Out_XIntAttr( XMLAttrName[atSymbolNr], fSymbol.Nr);
  case fSymbol.Kind of
   'J','U','V','G','L','M':
    Out_XIntAttr( XMLAttrName[atArgNr], MPrefixFormatPtr(@Self)^.fRightArgsNbr);
   'O','R':
    with MInfixFormatPtr(@Self)^ do
   begin
    Out_XIntAttr( XMLAttrName[atArgNr], fLeftArgsNbr+fRightArgsNbr);
    Out_XIntAttr( XMLAttrName[atLeftArgNr], fLeftArgsNbr);
   end;
   'K':
    with MBracketFormatPtr(@Self)^ do
   begin
    Out_XIntAttr( XMLAttrName[atArgNr], fArgsNbr);
    Out_XIntAttr( XMLAttrName[atRightSymbolNr], fRightSymbolNr);
   end;
  else RuntimeError(3300);
  end;
  Out_XElEnd0;
 end;
end;

(* ##RNC:
## Format info contains symbol formats and priorities.
## Priorities are used only for functor symbols.
## This implementation might change in some time.
elFormats =
 element elFormats {
   elFormat*,
   element elPriority { 
    attribute atKind { 'O' | 'K' | 'L' },
    attribute atSymbolNr { xsd:integer },
    attribute atValue { xsd:integer }
   }*
 }
*)
procedure MFormatsList.StoreFormats(fName:string);
 var lEnvFile: XMLOutStreamObj;
     z: integer;
begin
 lEnvFile.OpenFile(fName);
 with lEnvFile do
 begin
  Out_XElStart0( XMLElemName[elFormats]);
  for z:=0 to Count-1 do
   MFormatPtr(Items^[z])^.Out_Format( lEnvFile, z + 1);
  with gPriority do
   for z:=0 to fCount-1 do
   begin
    Out_XElStart( XMLElemName[elPriority]);
    Out_XAttr( XMLAttrName[atKind], chr(fList^[z].X1));
    Out_XIntAttr( XMLAttrName[atSymbolNr], fList^[z].X2);
    Out_XIntAttr( XMLAttrName[atValue], fList^[z].Y);
    Out_XElEnd0;
   end;
  Out_XElEnd( XMLElemName[elFormats]);
 end;
 lEnvFile.Done;
end;

procedure DisposeFormats;
begin
 gFormatsColl.Done;
 gPriority.Done;
end;

end.

