(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit xml_parser;

interface

uses mobjects,errhan;

const InOutFileBuffSize = $4000;

// for xml attribute tables
const errElRedundant = 7500;// End of element expected, but child element found
const errElMissing = 7501; //Child element expected, but end of element found
const errMissingXMLAttribute = 7502; // Required XML attribute not found
const errWrongXMLElement = 7503; // Different XML element expected
const errBadXMLToken = 7506; // Unexpected XML token

// for xml tokenizer
type
  XMLTokenKind = (Err,  //   an error symbol
                  BI,     //   <?
                  EI,     //   ?>
                  DT,     //   <!
                  LT,     //   <
                  GT,     //   >
                  ET,     //   </
                  EE,     //   />
                  QT,     //   "
                  EQ,     //   =
                  EN,     //   Entity
                  ID,     //   Identifier, Name
                  EOTX);   //   End of text
  TokensSet = set of XMLTokenKind;

  XMLScannObj = object(MObject)
    nSourceFile: text;
    nSourceFileBuff: pointer;
    nCurTokenKind: XMLTokenKind;
    nSpelling: string;
    nPos: Position;
    nCurCol: integer;
    nLine: string;
    constructor InitScanning(const aFileName:string);
    destructor Done; virtual;
    procedure GetToken;
//   private    
    procedure GetAttrValue;
  end;

  TElementState = ( eStart, eEnd); 	// high-level parser states,
                                        // see procedure NextElementState

  XMLAttrPtr = ^XMLAttrObj;
  XMLAttrObj = object(MStrObj)
    nValue: string;
    constructor Init(const aName,aValue: string);
  end;

  XMLParserObj = object(XMLScannObj)
    nElName	: string;   // name of the current element
    nState	: TElementState;
    nAttrVals	: MSortedStrList;

    constructor InitParsing(const aFileName:string);
    destructor Done; virtual;
    procedure ErrorRecovery(aErr: integer; aSym: TokensSet);

    procedure NextTag; virtual;
    procedure NextElementState; virtual;
    procedure AcceptEndState; virtual;
    procedure AcceptStartState; virtual;
    procedure OpenStartTag; virtual;
    procedure CloseStartTag; virtual;
    procedure CloseEmptyElementTag; virtual;
    procedure ProcessEndTag; virtual;
    procedure ProcessAttributeName; virtual;
    procedure ProcessAttributeValue; virtual;
    procedure SetAttributeValue(const aVal:string);
  end;

procedure XMLASSERT( aCond: boolean);

procedure UnexpectedXMLElem( const aElem:string; aErr:integer);

implementation
{$IFDEF MDEBUG} uses info; {$ENDIF}

constructor XMLAttrObj.Init(const aName,aValue: string);
begin
 inherited Init(aName);
 nValue:=aValue;
end;

procedure XMLASSERT( aCond: boolean);
begin
 MizAssert( errWrongXMLElement, aCond);
end;

procedure UnexpectedXMLElem( const aElem:string; aErr:integer);
{$IFDEF MDEBUG}
var lEl:string;
{$ENDIF}
begin
 {$IFDEF MDEBUG}
 InfoNewLine;
// InfoString('Unexpected elem: '+ XMLElemName[aElem]+'; expected:');
// for lEl := Low(string) to High(string) do
//  if lEl in aExpected then InfoString( XMLElemName[lEl]+',');
 {$ENDIF}
RunTimeError(aErr);
end;

constructor XMLScannObj.InitScanning(const aFileName:string);
begin
 inherited Init;
 assign(nSourceFile,aFileName);
 getmem(nSourceFileBuff,InOutFileBuffSize);
 settextbuf(nSourceFile,nSourceFileBuff^,InOutFileBuffSize);
 reset(nSourceFile);
 nSpelling:='';
 nLine:='';
 nCurCol:=0;
 nPos.Line:=0;
 nPos.Col:=0;
 GetToken;
end;

destructor XMLScannObj.Done;
begin
 close(nSourceFile);
 FreeMem(nSourceFileBuff,InOutFileBuffSize);
 nLine:='';
 nSpelling:='';
 inherited Done;
end;

procedure XMLScannObj.GetToken;
 const CharKind: array[chr(0)..chr(255)] of byte =
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
//        #     &             - . / 0 1 2 3 4 5 6 7 8 9 : ;
    0,0,0,3,0,0,3,0,0,0,0,0,0,3,3,0,2,2,2,2,2,2,2,2,2,2,3,3,0,0,0,0,
//    A B C D E F G H I J K L M N O P Q R S T U V W X Y Z         _
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,3,
//    a b c d e f g h i j k l m n o p q r s t u v w x y z
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
begin
  while nCurCol = length(nLine) do
   begin
    if eof(nSourceFile) then
     begin
      nCurTokenKind:=EOTX;
      nSpelling:='';
      exit
     end;
    readln(nSourceFile,nLine);
    inc(nPos.Line);
    nLine:=nLine+' ';
    nCurCol:=1;
    while (nCurCol<length(nLine)) and (nLine[nCurCol]=' ') do inc(nCurCol);
   end;
  nPos.Col := nCurCol;
  case nLine[nCurCol] of
   'a'..'z', 'A'..'Z', '0'..'9','_','-','&':
    begin
     nCurTokenKind:=ID;
     repeat inc(nCurCol) until CharKind[nLine[nCurCol]] = 0;
    end;
   '"':
    begin
     nCurTokenKind:=QT;
     inc(nCurCol)
    end;
   '=':
    begin
     nCurTokenKind:=EQ;
     inc(nCurCol)
    end;
   '<':
    begin
     inc(nCurCol);
     case nLine[nCurCol] of
     '/': begin nCurTokenKind:=ET; inc(nCurCol); end;
     '?': begin nCurTokenKind:=BI; inc(nCurCol); end ;
     '!': begin nCurTokenKind:=DT; inc(nCurCol); end ;
      else nCurTokenKind:=LT;
     end;
    end;
   '>':
    begin
     nCurTokenKind:=gt;
     inc(nCurCol)
    end;
   '/':
    begin
     inc(nCurCol);
     if nLine[nCurCol] = '>' then
       begin
        nCurTokenKind:=EE;
        inc(nCurCol);
       end
      else nCurTokenKind:=Err;
    end;
   '?':
    begin
     inc(nCurCol);
     if nLine[nCurCol] = '>' then
       begin
        nCurTokenKind:=EI;
        inc(nCurCol);
       end
      else nCurTokenKind:=Err;
    end;
// ##JU: made entities just a part of ID - simpler, we don't interpret them now
//    '&':
//    begin inc(nCurCol);
//     while CharKind[nLine[nCurCol]] = 1 do inc(nCurCol);
//     if nLine[nCurCol] = ';' then begin nCurTokenKind:=EN; inc(nCurCol); end
//      else nCurTokenKind:=Err;
//    end;
   else
    begin
     nCurTokenKind:=Err;
     inc(nCurCol)
    end;
  end;
  nSpelling:=Copy(nLine,nPos.Col,nCurCol-nPos.Col);
  while (nCurCol<length(nLine)) and (nLine[nCurCol] in [' ','	'])
   do inc(nCurCol);
end;

procedure XMLScannObj.GetAttrValue;
 var lCol: integer;
begin
  lCol:=nCurCol;
  while (nCurCol < length(nLine)) and (nLine[nCurCol] <> '"') do
   inc(nCurCol);
  nSpelling:=Copy(nLine,lCol,nCurCol-lCol);
  if nLine[nCurCol] = '"' then
   inc(nCurCol);
  while (nCurCol<length(nLine)) and (nLine[nCurCol] in [' ','	'])
   do inc(nCurCol);
end;

constructor XMLParserObj.InitParsing(const aFileName:string);
begin
  inherited InitScanning(aFileName);
  nElName:= '';
  nAttrVals.Init(0);
  if nCurTokenKind = BI then
   begin
    GetToken;
    if (nCurTokenKind = ID) and (nSpelling = 'xml') then
      GetToken
    else ErrorRecovery(10,[EI,LT]);
//  Prolog and Document Type Declaration of a XML document is skipped
    while (nCurTokenKind <> EOTX) and (nCurTokenKind <> EI) do GetToken;
    if nCurTokenKind = EI then GetToken;
// skip all other initial processing instructions
    while nCurTokenKind = BI do
    begin
     GetToken;
     while (nCurTokenKind <> EOTX) and (nCurTokenKind <> EI) do GetToken;
     if nCurTokenKind = EI then GetToken;
    end;
   end;
end;

destructor XMLParserObj.Done;
begin
 inherited Done;
 nAttrVals.Done;
 nElName:='';
end;

// ErrorRecovery is no longer allowed for XML, bad XML is just RTE
procedure XMLParserObj.ErrorRecovery(aErr: integer; aSym: TokensSet);
begin
 // ErrImm(aErr);
 Mizassert( errBadXMLToken, false);
// while not (nCurTokenKind in [EOTX]+aSym) do GetToken;
end;

// Parses next part of XML, used for skipping some part of XML
//
// setting the nState to eStart or eEnd.
// nElName is set properly
// nAttrVals are omitted (skiped).
procedure XMLParserObj.NextTag;
begin
 case nCurTokenKind of
 EOTX: nState:= eEnd;  // sometimes we need this
 LT:
  begin
   nState := eStart;
   GetToken;
   if nCurTokenKind = ID then
   begin
    OpenStartTag;
    GetToken;
    repeat
     case nCurTokenKind of
      GT:
       begin
        GetToken;
        break
       end;
      EE:
       begin
        break
       end;
      ID:
       begin
        GetToken;
        if nCurTokenKind = EQ then
        begin
         GetToken;
         if nCurTokenKind = QT then
         begin
          GetAttrValue;
          GetToken;
         end
         else ErrorRecovery(3,[ID,GT,LT,ET]);;
        end
        else ErrorRecovery(4,[ID,GT,LT,ET]);;
       end;
     else
      begin
       ErrorRecovery(5,[GT,LT,ET]);
       break
      end;
     end;
    until nCurTokenKind = EOTX;
   end
   else ErrorRecovery(6,[LT,ET]);
  end;
 EE:
  begin
   nState := eEnd;
   GetToken;
  end;
 ET:
  begin
    nState := eEnd;
    GetToken;
    if nCurTokenKind = ID then
     begin
       OpenStartTag;
       GetToken;
       if nCurTokenKind = GT then
         GetToken
        else ErrorRecovery(7,[LT,ET]);
     end
    else ErrorRecovery(8,[LT,ET]);
  end;
 else ErrorRecovery(9,[LT,ET]);
 end;
end;

// Parses next part of XML, setting the nState to eStart or eEnd.
// If nState=eStart, nElName, nAttrVals are set properly.
// It is possible to go from nState=eStart to nState=eStart
// (when the element is non empty), and similarily from eEnd to eEnd.
procedure XMLParserObj.NextElementState;
begin
 case nCurTokenKind of
 EOTX: nState:= eEnd;  // sometimes we need this
 LT:
  begin
   nState := eStart;
   GetToken;
   if nCurTokenKind = ID then
   begin
    OpenStartTag;
    //  Start-Tag or Empty-Element-Tag Name = nSpelling
    GetToken;
    repeat
     case nCurTokenKind of
      GT:
       begin
        GetToken;
        CloseStartTag;
        //  End of a Start-Tag
        break
       end;
      EE:
       begin
        //  End of a Empty-Element-Tag
        CloseEmptyElementTag;
        break
       end;
      ID:
       begin
        //  Attribute-Name = nSpelling
        ProcessAttributeName;
        GetToken;
        if nCurTokenKind = EQ then
        begin
         GetToken;
         if nCurTokenKind = QT then
         begin
          GetAttrValue;
          ProcessAttributeValue;
          GetToken;
         end
         else ErrorRecovery(3,[ID,GT,LT,ET]);;
        end
        else ErrorRecovery(4,[ID,GT,LT,ET]);;
       end;
     else
      begin
       ErrorRecovery(5,[GT,LT,ET]);
       break
      end;
     end;
    until nCurTokenKind = EOTX;
   end
   else ErrorRecovery(6,[LT,ET]);
  end;
 EE:
  begin
   // MizAssert(nState = eStart);
   nState := eEnd;
   GetToken;
  end;
 ET:
  begin
   nState := eEnd;
   GetToken;
    if nCurTokenKind = ID then
     begin
//  End-Tag Name = nSpelling
       ProcessEndTag;
       GetToken;
       if nCurTokenKind = GT then
         begin
//  End of a End-Tag
          GetToken
         end
        else ErrorRecovery(7,[LT,ET]);
     end
    else ErrorRecovery(8,[LT,ET]);
  end;
 else ErrorRecovery(9,[LT,ET]);
 end;
end;

procedure XMLParserObj.AcceptEndState;
begin
 NextElementState;
 MizAssert( errElRedundant, nState = eEnd);
end;

procedure XMLParserObj.AcceptStartState;
begin
 NextElementState;
 MizAssert( errElMissing, nState = eStart);
end;

procedure XMLParserObj.OpenStartTag;
begin
 nElName:= nSpelling;
 nAttrVals.FreeAll;
end;

procedure XMLParserObj.CloseStartTag;
begin
end;

procedure XMLParserObj.CloseEmptyElementTag;
begin
end;

procedure XMLParserObj.ProcessEndTag;
begin
end;

procedure XMLParserObj.ProcessAttributeName;
begin
 nAttrVals.Insert(new(XMLAttrPtr,Init(nSpelling,'')));
end;

procedure XMLParserObj.ProcessAttributeValue;
begin
 SetAttributeValue(nSpelling);
end;

procedure XMLParserObj.SetAttributeValue(const aVal:string);
begin
 with nAttrVals do
  XMLAttrPtr(Items^[Count-1])^.nValue := aVal;
end;

end.
