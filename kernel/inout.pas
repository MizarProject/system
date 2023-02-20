(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit inout; // ###TODO: replace inout with xinout containing only XML

interface

uses errhan,mobjects,xml_inout,xml_parser,xmlpars,xmldict,syntax;

const Capital: array[chr(0)..chr(255)] of byte =
    ( 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    );

type
 Lexem = record Kind: char; Nr: integer; end;

 MizInStream =
  object(StreamObj)
    Current: Lexem;
   constructor OpenFile(const AFileName:string);

   procedure GetBuff;
   procedure NextChar;
   procedure InNewLines;

   procedure InPos ( var APos: Position );
   procedure InWord;
   procedure InInt( var AInt: integer );
   procedure InBool(var ABool: boolean);
   procedure InString(var aString: string);
   procedure InNatFunc1(var aFunc:NatFunc);
   procedure InNatFunc(var aFunc:NatFunc);
   procedure InNatSet(var aSet:NatSet);
  end;

 MizXInStream = object(XMLParsObj)
  constructor OpenFile(const AFileName:string);
  function GetOptAttr( aAttrName: TXMLAttrKind; var aVal:string): boolean;
  function GetOptIntAttr( aAttrName: TXMLAttrKind; var aVal:integer): boolean;
  function GetAttr( aAttrName: TXMLAttrKind): string;
  function GetIntAttr( aAttrName: TXMLAttrKind): integer;
  procedure GetPosAttrs(var fPos: Position);
  procedure GetLexemAttrs( aKind,aNr: TXMLAttrKind; var aLex:Lexem);
 end;
 
 MizOutStream =
  object(StreamObj)
   constructor OpenFile(const AFileName:string);
//   constructor AppendFile(const AFileName:string);
   destructor EraseFile;
   procedure OutNewLine;
   procedure OutChar( AChar: char );
   procedure OutWord( AChar: char; AInt: integer );
   procedure OutInt ( AInt: integer );
   procedure OutPos ( APos: Position );
   procedure OutCharAndPos(C:char; APos:Position);
   procedure OutWordAndPos(C:char; I:integer; APos:Position);
   procedure OutBool ( ABool: boolean );
   procedure OutString( const AString: string);
   procedure OutNatSet(const aSet:NatSet);
   procedure OutNatFunc(const aFunc:NatFunc);
   destructor Done; virtual;
end;

  MizXOutStream = object(XMLOutStreamObj)
   constructor OpenFile(const AFileName:string);

   procedure OutIndent;

   procedure Out_XElStart( fEl: TXMLElemKind);
   procedure Out_XElStart0( fEl: TXMLElemKind);
   procedure Out_XEl1( fEl: TXMLElemKind);
   procedure Out_XElEnd( fEl: TXMLElemKind);
   procedure Out_XAttr( fAt: TXMLAttrKind; fVal: string);
   procedure Out_XIntAttr( fAt: TXMLAttrKind; fVal: integer);
   procedure Out_XMizQuotedAttr( fAt: TXMLAttrKind; fVal: string);
  end;

var
// ##TODO: try to get rid of these
   InFile: MizInStream;
   OutFile: MizOutStream;
//   OutFile: MizOutCXStream;

implementation

uses mizenv,pcmizver,librenv,lexicon
{$IFDEF MDEBUG} ,info {$ENDIF};

const
  CharKind: array[chr(0)..chr(255)] of byte =
   (1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,3,3,0,0,0,0,0,0,
    0,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);

constructor MizInStream.OpenFile(const AFileName:string);
begin
{$IFDEF MDEBUG}
write(InfoFile,MizFileName+'.'+copy(AFileName,Length(AFilename)-2,3));
{$ENDIF}
  InitFile(AFileName);
  FileMode := 0;
  reset(nFile,1);
  GetBuff;
{$IFDEF MDEBUG}
writeln(InfoFile,' reset');
{$ENDIF}
end;

procedure MizInStream.GetBuff;
begin
  BlockRead(nFile,fFileBuff^,InOutFileBuffSize,fBuffCount);
  if fBuffCount = 0 then
   begin inc(fBuffCount);
    fFileBuff^[0]:=chr(26);
   end;
  fBuffInd:=0;
end;

procedure MizInStream.nextchar;
begin
 inc(fBuffInd);
 if fBuffInd < fBuffCount then exit;
 GetBuff;
end;

procedure MizInStream.InNewLines;
begin
 while true do
  begin
   if fFilebuff^[fBuffInd] = chr(26) then RunTimeError(2200);
   if CharKind[fFilebuff^[fBuffInd]] = 2
    then nextchar
    else exit;
  end;
end;

procedure MizInStream.InInt ( var aInt: integer );
begin
 repeat
  while fFilebuff^[fBuffInd] = ' ' do nextchar;
  InNewLines;
 until fFilebuff^[fBuffInd] <> ' ';
 aInt:=0;
 if fFilebuff^[fBuffInd] = chr(26) then RunTimeError(2200);
 while CharKind[fFilebuff^[fBuffInd]] = 3 do
// while ('0' <= fFilebuff^[fBuffInd]) and (fFilebuff^[fBuffInd] <='9')  do
  begin aInt:=aInt*10+(ord(fFilebuff^[fBuffInd])-ord('0')); nextchar end;
 if fFilebuff^[fBuffInd] = ' '
  then nextchar;
end;

procedure MizInStream.InWord;
begin InNewLines;
 if fFilebuff^[fBuffInd] = chr(26) then RunTimeError(2200);
 Current.Kind:=Char(fFilebuff^[fBuffInd]);
 nextchar;
 if Capital[Current.Kind]<>0 then InInt(Current.Nr);
end;

procedure MizInStream.InString(var aString: string);
begin InNewLines;
 aString:='';
 while true do
  begin
   if fFilebuff^[fBuffInd] = chr(26) then RunTimeError(2200);
   if CharKind[fFilebuff^[fBuffInd]] = 2
     then break
    else begin aString:=aString+fFilebuff^[fBuffInd]; nextchar end;
  end;
end;

procedure MizInStream.InBool ( var ABool: boolean );
begin InWord; ABool:=Current.Kind='+'; end;

procedure MizInStream.InPos ( var APos: Position );
begin InInt(APos.Line); InInt(APos.Col);
 CurPos:=APos;
end;

procedure MizInStream.InNatSet(var aSet:NatSet);
begin
 aSet.Init(8,8);
 InWord;
 with aSet do
 begin
  while Current.Kind <> ';' do
  begin
   InsertElem(Current.Nr);
   InWord;
  end;
  SetLimit(0);
 end;
end;

// ##TODO: another collision in I/O here
procedure MizInStream.InNatFunc1(var aFunc:NatFunc);
 var X: integer;
begin
 aFunc.InitNatFunc(8,8);
 with aFunc do
 begin
   while Current.Kind <> ';' do
   begin
    X:=Current.Nr; InWord;
    Assign(X,Current.Nr);
    InWord;
   end;
   SetLimit(0);
 end;
 InWord;
end;

procedure MizInStream.InNatFunc(var aFunc:NatFunc);
begin InWord; InNatFunc1( aFunc); end;

{------------------------------------------------------------------}

constructor MizOutStream.OpenFile(const AFileName:string);
begin
{$IFDEF MDEBUG}
write(InfoFile,MizFileName+'.'+copy(AFileName,Length(AFilename)-2,3));
{$ENDIF}
  InitFile(AFileName);
  rewrite(nFile,1);
{$IFDEF MDEBUG}
writeln(InfoFile,' rewritten');
{$ENDIF}
end;

destructor MizOutStream.Done;
begin
// OutChar(#10);
 if (fBuffInd > 0) and (fBuffInd < InOutFileBuffSize) then
  BlockWrite(nFile,fFileBuff^,fBuffInd,fBuffCount);
 inherited Done;
end;

(*constructor MizOutStream.AppendFile(const AFileName:string);
begin
{$IFDEF MDEBUG}
write(InfoFile,MizFileName+'.'+copy(AFileName,Length(AFilename)-2,3));
{$ENDIF}
  InitFile(AFileName);
  append(fFile);
{$IFDEF MDEBUG}
writeln(InfoFile,' appended');
{$ENDIF}
end;*)

procedure MizOutStream.OutChar ( aChar: char );
begin
 fFileBuff^[fBuffInd]:=AnsiChar(aChar);
 inc(fBuffInd);
 if fBuffInd = InOutFileBuffSize then
  begin BlockWrite(nFile,fFileBuff^,InOutFileBuffSize,fBuffCount);
    fBuffInd:=0;
  end;
end;

procedure MizOutStream.OutNewLine;
begin OutChar(chr(10)) end;

procedure MizOutStream.OutBool ( ABool: boolean );
begin
 if ABool then OutChar('+') else OutChar('-');
end;

procedure MizOutStream.OutInt ( aInt: integer );
begin
 outstring(IntToStr(aInt));
 outchar(' ');
end;

procedure MizOutStream.OutWord ( AChar: char; AInt: integer );
begin 
 OutChar(AChar);
 if Capital[AChar]<>0 then OutInt(AInt);
end;

procedure MizOutStream.OutPos ( APos: Position );
begin OutInt(APos.Line); OutInt(APos.Col) end;

procedure MizOutStream.OutCharAndPos(C:char; APos:Position);
begin OutChar(C); OutPos(APos) end;


procedure MizOutStream.OutWordAndPos(C:char; I:integer; APos:Position);
begin OutWord(C,I); OutPos(APos) end;


procedure MizOutStream.OutString ( const aString: string );
 var i: integer;
begin
 for i:=1 to length(aString) do
  OutChar(aString[i]);
end;

destructor MizOutStream.EraseFile;
begin Done;
 Erase(nFile);
end;

procedure MizOutStream.OutNatSet(const aSet:NatSet);
 var i: integer;
begin
 with aSet do
  for i:=0 to Count-1 do
  begin
   OutWord('X',IntPair(Items^[i]).X);
  end;
 OutChar(';');
end;

procedure MizOutStream.OutNatFunc(const aFunc:NatFunc);
 var i: integer;
begin
 with aFunc do
 begin
   for i:=0 to Count-1 do
    begin
     OutWord('X',IntPair(Items^[i]).X);
     OutWord('Y',IntPair(Items^[i]).Y);
    end;
   OutChar(';');
//   OutBool(nConsistent)
 end;
end;

{------------------------------------------------------------------}

// ##TODO: rewrite XMLScannObj.InitScanning as MizInStream to
//         avoid settextbuf
constructor MizXInStream.OpenFile(const AFileName:string);
begin
{$IFDEF MDEBUG}
 write(InfoFile, AFileName);
{$ENDIF}
 InitParsing( AFileName);
{$IFDEF MDEBUG}
 writeln(InfoFile,' reset');
{$ENDIF}
end;

// get string denoted by optional xml attribute aAttrName
function MizXInStream.GetOptAttr( aAttrName: TXMLAttrKind;
                                  var aVal:string): boolean;
 var lAtt: XMLAttrPtr;
begin
//XMLElemName
 lAtt:=XMLAttrPtr(nAttrVals.ObjectOf(XMLAttrName[aAttrName]));
 if Latt<>nil then
  begin
   aVal:=lAtt^.nValue;
   GetOptAttr:= true;
   exit;
  end;
 GetOptAttr:= false;
end;

function MizXInStream.GetOptIntAttr( aAttrName: TXMLAttrKind;
                                  var aVal:integer): boolean;
 var ec:integer;
     lName: string;
begin
 GetOptIntAttr:=false;
 if GetOptAttr(aAttrName,lName) then
  begin
   Val( GetAttr( aAttrName), aVal, ec);
   GetOptIntAttr:=ec=0;
  end;
end;

// get string denoted by  required xml attribute aAttrName
function MizXInStream.GetAttr( aAttrName: TXMLAttrKind): string;
 var lAtt: XMLAttrPtr;
begin
 lAtt:=XMLAttrPtr(nAttrVals.ObjectOf(XMLAttrName[aAttrName]));
 if Latt<>nil then
  begin
   GetAttr:=lAtt^.nValue;
   exit;
  end;
 MizAssert( errMissingXMLAttribute, false);
end;

// get integer denoted by required xml attribute aAttrName
function MizXInStream.GetIntAttr( aAttrName: TXMLAttrKind): integer;
 var ec:integer;
begin
 Val( GetAttr( aAttrName), result, ec);
end;

procedure MizXInStream.GetPosAttrs(var fPos: Position);
begin
 fPos.Line:= GetIntAttr( atLine);
 fPos.Col:= GetIntAttr( atCol);
end;

// get lexem denoted by required xml attributes aKind and aNr
procedure MizXInStream.GetLexemAttrs( aKind,aNr:TXMLAttrKind; var aLex:Lexem);
 var lName: string;
     lVal: integer;
begin
 if GetOptAttr(aKind,lName) then
   aLex.Kind:= lName[1];
 GetOptIntAttr(aNr,aLex.Nr);
end;

{------------------------------------------------------------------}

constructor MizXOutStream.OpenFile(const AFileName:string);
begin
 inherited OpenFile( AFileName);
 nIndent := 0;
end;

// print nIndent spaces; this is quite a lot so it is now ifedef-ed
procedure MizXOutStream.OutIndent;
begin
{$IFDEF _XML_INDENTING}
  inherited OutIndent;
{$ENDIF}
end;

// print '<' and the representation of fEl with indenting
procedure MizXOutStream.Out_XElStart( fEl: TXMLElemKind);
begin
 OutIndent;
 inc(nIndent);
 OutChar('<');
 OutString( XMLElemName[ fEl]);
end;

// no attributes expected
procedure MizXOutStream.Out_XElStart0( fEl: TXMLElemKind);
begin Out_XElStart( fEl); Out_XAttrEnd; end;

// no attributes and elements expected
procedure MizXOutStream.Out_XEl1( fEl: TXMLElemKind);
begin Out_XElStart( fEl); Out_XElEnd0; end;

// close the fEl element using '</'
procedure MizXOutStream.Out_XElEnd( fEl: TXMLElemKind);
begin
 dec(nIndent);
 OutIndent;
 OutString('</');
 OutString( XMLElemName[ fEl]);
 OutChar('>');
 OutNewLine;
end;

// print one attribute - value pair
procedure MizXOutStream.Out_XAttr( fAt: TXMLAttrKind; fVal: string);
begin
 OutChar(' ');
 OutString( XMLAttrName[ fAt]);
 OutString('="');
 OutString(fVal);
 OutChar('"');
end;

// print one attribute - value pair, where value is integer
procedure MizXOutStream.Out_XIntAttr( fAt: TXMLAttrKind; fVal: integer);
begin Out_XAttr( fAt, IntToStr( fVal)); end;

// fVal can be a dangerous string here, requiring XML quoting for Mizar
procedure MizXOutStream.Out_XMizQuotedAttr( fAt: TXMLAttrKind; fVal: string);
begin Out_XAttr( fAt, QuoteXMLAttr( fVal) ); end;

end.
