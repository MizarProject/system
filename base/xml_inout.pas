(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit xml_inout;

interface

uses errhan,mobjects,xml_parser;

type

 BuffChar = array[0..InOutFileBuffSize-1] of AnsiChar;

 XMLInStreamPtr = ^XMLInStreamObj;
 XMLInStreamObj = object(XMLParserObj)
  constructor OpenFile(const AFileName:string);
  function GetOptAttr( const aAttrName: string; var aVal:string): boolean;
  function GetAttr( const aAttrName: string): string;
  function GetIntAttr( const aAttrName: string): integer;
 end;

 StreamObj =
  object(MObject)
    nFile: File;
    fFileBuff: ^BuffChar;
    fBuffCount,fBuffInd: longint;
   constructor InitFile(const AFileName:string);
   procedure Error(Code,Info:integer); virtual;
   destructor Done; virtual;
  end;

 TXTStreamObj =
  object(MObject)
    nFile: text;
    nFileBuff: pointer;
   constructor InitFile(const AFileName:string);
   procedure Error(Code,Info:integer); virtual;
   destructor Done; virtual;
  end;

 XMLOutStreamPtr = ^XMLOutStreamObj;
 XMLOutStreamObj = object(StreamObj)
   nIndent:	integer; 	// indenting
  constructor OpenFile(const AFileName:string);
  constructor OpenFileWithXSL(const AFileName:string);
  destructor EraseFile;

  procedure OutChar( AChar: char );
  procedure OutNewLine;
  procedure OutString( const AString: string);

  procedure OutIndent;
  procedure Out_XElStart( const fEl: string);
  procedure Out_XAttrEnd;
  procedure Out_XElStart0( const fEl: string);
  procedure Out_XElEnd0;
  procedure Out_XEl1( const fEl: string);
  procedure Out_XElEnd( const fEl: string);
  procedure Out_XAttr( const fAt, fVal: string);
  procedure Out_XIntAttr( const fAt: string; fVal: integer);
  procedure Out_PosAsAttrs(const fPos: Position);
  procedure Out_XElWithPos(const fEl: string; const fPos: Position);
  procedure Out_XQuotedAttr( const fAt,fVal: string);
  destructor Done; virtual;
 end;

function QuoteStrForXML(const aStr:string):string;
function XMLToStr(const aXMLStr:string): string;
function QuoteXMLAttr( aStr:string): string;

const gXMLHeader = '<?xml version="1.0"?>' + #10;

implementation

uses sysutils,mizenv,pcmizver,librenv,xml_dict
{$IFDEF MDEBUG} ,info {$ENDIF};

function QuoteStrForXML( const aStr:string): string;
 const
  ValidCharTable = (['a'..'z','A'..'Z','0'..'9','-',' ',',','.','\','/','_',
                     '[',']','!',';',':','=']);
 var c : char;
     i : integer;
begin
  result := aStr;
  for i := Length(result) downto 1 do
  begin
    c := result[i];
    if not (c in ValidCharTable)  then
    begin
      result[i] := '&';
      Insert('#x' + IntToHex(Ord(c),2) + ';', result,i+1);
    end;
  end;
end;

function XMLToStr(const aXMLStr:string): string;
 var i, h : integer;
     lHexNr: string;
begin
  Result := aXMLStr;
  for i := Length(Result)-5 downto 1 do begin
    if (Result[i] = '&') and (Length(Result) >= i+5) then
    begin
      if (Result[i+1] = '#') and (Result[i+2] = 'x') then
      begin
        lHexNr := Result[i+3]+Result[i+4];
        // Must add '0x' to force Hex type, otherwise StrToInt assumes base 10
        h := StrToInt('0x' + lHexNr);
        Delete(Result, i, 5);
        Result[i] := chr(h);
      end;
    end;
  end;
  Result := Trim(Result);
end;

function QuoteXMLAttr( aStr:string): string;
 var i:integer;
begin
 result:= '';
 for i:=1 to length(aStr) do
  case aStr[i] of
   '"':  result:= result + '&quot;';
   '&':  result:= result + '&amp;';
   '<':  result:= result + '&lt;';
   '>':  result:= result + '&gt;';
  else if integer(aStr[i]) > 127 then
   result:= result + '&#x' + IntToHex(Ord(aStr[i]),2) + ';'
  else result:= result + aStr[i];
  end;
end;

procedure StreamObj.Error(Code,Info:integer);
begin
 RunError(2000+Code);
end;

constructor StreamObj.InitFile(const AFileName:string);
begin
  Assign(nFile,AFileName);
  new(fFileBuff);
  fBuffCount:=0; fBuffInd:=0;
end;

destructor StreamObj.Done;
begin
 Close(nFile);
 dispose(fFileBuff);
end;

procedure TXTStreamObj.Error(Code,Info:integer);
begin
 RunError(2000+Code);
end;

constructor TXTStreamObj.InitFile(const AFileName:string);
begin
  assign(nFile,AFileName);
  getmem(nFileBuff,InOutFileBuffSize);
  settextbuf(nFile,nFileBuff^,InOutFileBuffSize);
end;

destructor TXTStreamObj.Done;
begin
 FreeMem(nFileBuff,InOutFileBuffSize);
end;

{------------------------------------------------------------------}

constructor XMLInStreamObj.OpenFile(const AFileName:string);
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
function XMLInStreamObj.GetOptAttr( const aAttrName: string;
                                    var aVal:string): boolean;
var lAtt: XMLAttrPtr;
begin
 lAtt:=XMLAttrPtr(nAttrVals.ObjectOf(aAttrName));
 if Latt<>nil then
  begin
   aVal:=lAtt^.nValue;
   GetOptAttr:= true;
   exit;
  end;
 GetOptAttr:= false;
end;

// get string denoted by  required xml attribute aAttrName
function XMLInStreamObj.GetAttr( const aAttrName: string): string;
var lAtt: XMLAttrPtr;
begin
 lAtt:=XMLAttrPtr(nAttrVals.ObjectOf(aAttrName));
 if Latt<>nil then
  begin
   GetAttr:=lAtt^.nValue;
   exit;
  end;
 MizAssert( errMissingXMLAttribute, false);
end;

// get integer denoted by required xml attribute aAttrName
function XMLInStreamObj.GetIntAttr( const aAttrName: string): integer;
var lInt,ec:integer;
begin
 Val( GetAttr( aAttrName), lInt, ec);
 GetIntAttr:= lInt;
end;

{------------------------------------------------------------------}

constructor XMLOutStreamObj.OpenFile(const AFileName:string);
begin
{$IFDEF MDEBUG}
write(InfoFile,MizFileName+'.'+copy(AFileName,Length(AFilename)-2,3));
{$ENDIF}
  InitFile(AFileName);
  rewrite(nFile,1);
{$IFDEF MDEBUG}
writeln(InfoFile,' rewritten');
{$ENDIF}
 nIndent := 0;
 OutString( gXMLHeader);
end;

// add the stylesheet procesing info
constructor XMLOutStreamObj.OpenFileWithXSL(const AFileName:string);
begin
 OpenFile( AFileName);
 OutString('<?xml-stylesheet type="text/xml" href="file://' +
            MizFiles + 'miz.xml"?>'+ #10);
end;

destructor XMLOutStreamObj.Done;
begin
 if (fBuffInd > 0) and (fBuffInd < InOutFileBuffSize) then
  BlockWrite(nFile,fFileBuff^,fBuffInd,fBuffCount);
 inherited Done;
end;

destructor XMLOutStreamObj.EraseFile;
begin Done;
 Erase(nFile);
end;

procedure XMLOutStreamObj.OutChar ( aChar: char );
begin
 fFileBuff^[fBuffInd]:=AnsiChar(aChar);
 inc(fBuffInd);
 if fBuffInd = InOutFileBuffSize then
  begin BlockWrite(nFile,fFileBuff^,InOutFileBuffSize,fBuffCount);
    fBuffInd:=0;
  end;
end;

procedure XMLOutStreamObj.OutNewLine;
begin
 OutChar(#10);
end;

procedure XMLOutStreamObj.OutString ( const aString: string );
 var i: integer;
begin
 for i:=1 to length(aString) do
  OutChar(aString[i]);
end;

// print nIndent spaces; 
procedure XMLOutStreamObj.OutIndent;
var i:integer;
begin
 for i:=1 to nIndent do OutChar(' ');
end;

// print '<' and the representation of fEl with indenting
procedure XMLOutStreamObj.Out_XElStart( const fEl: string);
begin
 OutIndent;
 inc(nIndent);
 OutChar('<');
 OutString( fEl);
end;

// close the attributes with '>'
procedure  XMLOutStreamObj.Out_XAttrEnd;
begin
 OutChar('>');
 OutNewLine;
end;

// no attributes expected
procedure XMLOutStreamObj.Out_XElStart0( const fEl: string);
begin
 Out_XElStart( fEl);
 Out_XAttrEnd;
end;

// print '/>' with indenting
procedure XMLOutStreamObj.Out_XElEnd0;
begin
 OutString('/>');
 OutNewLine;
 dec(nIndent);
end;

// no attributes and elements expected
procedure XMLOutStreamObj.Out_XEl1( const fEl: string);
begin
 Out_XElStart( fEl);
 Out_XElEnd0;
end;

// close the fEl element using '</'
procedure XMLOutStreamObj.Out_XElEnd( const fEl: string);
begin
 dec(nIndent);
 OutIndent;
 OutString('</');
 OutString( fEl);
 OutChar('>');
 OutNewLine;
end;

// print one attribute - value pair
procedure XMLOutStreamObj.Out_XAttr( const fAt,fVal: string);
begin
 OutChar(' ');
 OutString( fAt);
 OutString('="');
 OutString(fVal);
 OutChar('"');
end;

// print one attribute - value pair, where value is integer
procedure XMLOutStreamObj.Out_XIntAttr( const fAt: string; fVal: integer);
begin
 Out_XAttr( fAt, IntToStr( fVal));
end;

procedure XMLOutStreamObj.Out_XElWithPos(const fEl: string; const fPos: Position);
begin
  Out_XElStart(fEl);
  Out_PosAsAttrs(fPos);
  Out_XElEnd0;
end;

procedure XMLOutStreamObj.Out_PosAsAttrs(const fPos: Position);
begin
  Out_XIntAttr( XMLAttrName[atLine], fPos.Line);
  Out_XIntAttr( XMLAttrName[atCol], fPos.Col);
end;

procedure XMLOutStreamObj.Out_XQuotedAttr( const fAt, fVal: string);
begin
 Out_XAttr( fAt, QuoteStrForXML( fVal) );
end;

end.
