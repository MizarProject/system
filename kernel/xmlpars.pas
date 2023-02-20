(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit xmlpars;

interface

uses xml_parser,xmldict;

// for xml tokenizer
type

  XMLParsObj = object(XMLParserObj)
    nElKind	: TXMLElemKind; // kind of nElName
    constructor InitParsing(const aFileName:string);
    procedure OpenStartTag; virtual;
  end;

procedure UnexpectedElem( aElem:TXMLElemKind; aErr:integer; aExpected: TXMLElemSet);

implementation
 {$IFDEF MDEBUG} uses info; {$ENDIF}

procedure UnexpectedElem( aElem:TXMLElemKind; aErr:integer;
                          aExpected: TXMLElemSet);
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
 UnexpectedXMLElem(XMLElemName[aElem], aErr);
end;

constructor XMLParsObj.InitParsing(const aFileName:string);
begin
  inherited InitParsing(aFileName);
end;

procedure XMLParsObj.OpenStartTag;
begin
 inherited OpenStartTag;
 nElKind:= Str2XMLElemKind( nSpelling);
end;

// Very simple (Tiny) XML parser
procedure ReadXML(const aFileName:string);
 var lXMLParser: XmlParsObj; lIndent,i:integer;
begin
 lXMLParser.InitParsing(aFileName);
 lIndent:=0;
 with lXMLParser do
// Document Type Definition is missed and the preliminary part of Standard XML
  while (nCurTokenKind <> EOTX) do
   begin
    NextElementState;
    if nState= eStart then
     begin
      for i:=1 to lIndent do write(' ');
      writeln(nElName + ' ' + XMLElemName[ nElKind]);
      inc(lIndent);
     end
    else
    begin
     dec(lIndent);
     for i:=1 to lIndent do write(' ');
     writeln('</>');
    end;
   end;
  lXMLParser.Done;
end;
(*
begin
 DrawMizarScreen('Tiny XML Parser for Mizar');
 InitExitProc;
 GetMizFileName('.xml');
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 ReadXML(MizFileName+ArticleExt);
 DrawErrorsMsg(ErrorNbr);
 FinishDrawing;
*)
end.
