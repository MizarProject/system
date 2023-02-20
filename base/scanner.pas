(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit scanner;

interface

uses errhan,mobjects;

const
 MaxLineLength  = 80;
 MaxConstInt = 2147483647; {maximal signed 32-bit integer}

type
 ASCIIArr = array[chr(0)..chr(255)] of byte;

 LexemRec = record Kind: char; Nr: integer; end;

 TokenPtr = ^TokenObj;
 TokenObj = object(MStrObj)
     fLexem: LexemRec;
    constructor Init(aKind:char; aNr:integer; const aSpelling: string);
 end;

 TokensCollection = object(MSortedStrList)
     fFirstChar: array [chr(30)..chr(255)] of integer;
    constructor InitTokens;
    constructor LoadDct(const aDctFileName:string);
    procedure SaveDct(const aDctFileName:string);
    procedure SaveXDct(const aDctFileName:string);
    function CollectToken(const aLexem:LexemRec; const aSpelling:string): boolean;
 end;

 MTokenPtr = ^MTokenObj;
 MTokenObj = object(TokenObj)
     fPos: Position;
    constructor Init(aKind:char; aNr:integer; const aSpelling: string; const aPos: Position);
 end;

 MTokeniser = object(MTokenObj)

     fPhrase: string;
     fPhrasePos: Position;

     fTokensBuf: MCollection;

     fTokens,fIdents: TokensCollection;

    constructor Init;
    destructor Done; virtual;

    procedure SliceIt; virtual;
    procedure GetToken; virtual;

    procedure GetPhrase;  virtual;
    function EndOfText: boolean; virtual;

    function IsIdentifierLetter(ch: char): boolean; virtual; 
    function IsIdentifierFirstLetter(ch: char): boolean; virtual;

    function Spelling(const aToken: LexemRec): string; virtual;
 end;

 MScannPtr = ^MScannObj;
 MScannObj = object(MTokeniser)
    Allowed: ASCIIArr;
     fSourceBuff: pointer;
     fSourceBuffSize: word;
     fSourceFile: text;
     fCurrentLine: string;
//     fVirtualEOF: boolean;

    constructor InitScanning(const aFileName,aDctFileName:string);
    destructor Done; virtual;

    procedure GetPhrase; virtual;
    procedure ProcessComment(fLine, fStart: integer; cmt: string); virtual;
    function EndOfText: boolean; virtual;

    function IsIdentifierLetter(ch: char): boolean; virtual;
 end;

implementation

    
uses
 mizenv,librenv,mconsole,xml_dict,xml_inout;

var DefaultAllowed:AsciiArr =
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1, {'_' allowed in identifiers by default!}
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);


constructor TokenObj.Init(aKind:char; aNr:integer; const aSpelling: string);
begin
 fLexem.Kind:=aKind;
 fLexem.Nr:=aNr;
 fStr:=aSpelling;
end;

constructor TokensCollection.InitTokens;
begin
 Init(100);
end;

function TokensCollection.CollectToken(const aLexem:LexemRec; const aSpelling:string): boolean;
 var k:integer; lToken:TokenPtr;
begin lToken:=new(TokenPtr,Init(aLexem.Kind,aLexem.Nr,aSpelling));
 if Search(lToken,k)
  then
   begin CollectToken:=false;
    dispose(lToken,Done)
   end
  else
   begin CollectToken:=true;
    Insert(lToken)
   end
end;

constructor TokensCollection.LoadDct(const aDctFileName:string);
 var Dct: text;
     lKind,lDummy:AnsiChar; lNr: integer; lString: string;
     i: integer; c:char;
begin
 assign(Dct,aDctFileName+'.dct');
 reset(Dct);
 InitTokens;
 while not seekEof(Dct) do
  begin
    readln(Dct,lKind,lNr,lDummy,lString);
    Insert(new(TokenPtr,Init(char(lKind),lNr,lString)));
  end;
 close(Dct);
 for c:=chr(30) to chr(255) do
  fFirstChar[c]:=-1;
  for i:=0 to Count-1 do
  begin
   c:=TokenPtr(Items^[fIndex^[i]])^.fStr[1];
   if fFirstChar[c] = -1 then
    fFirstChar[c]:=i;
  end;
end;

procedure TokensCollection.SaveDct(const aDctFileName:string);
 var i: integer; DctFile: text;
begin
 assign(DctFile,aDctFileName+'.dct'); rewrite(DctFile);
 for i:=0 to Count-1 do
  with TokenPtr(Items^[i])^, fLexem do
   writeln(DctFile,AnsiChar(Kind),Nr,' ',fStr);
 close(DctFile);
end;

(* ##RNC:
## Local dictionary for an article.
## The symbol kinds still use very internal notation.
elSymbols =
 attribute atAid { xsd:string }?,
 element elSymbols {
   element elSymbol {
    attribute atKind { xsd:string },
    attribute atNr { xsd:integer },
    attribute atName { xsd:integer }
   }*
 }
*)
procedure TokensCollection.SaveXDct(const aDctFileName:string);
var
 lEnvFile: XMLOutStreamObj; i: integer;
begin
 lEnvFile.OpenFile(aDctFileName);
 with lEnvFile do
 begin
  Out_XElStart( XMLElemName[elSymbols]);
  Out_XAttr( XMLAttrName[atAid], ArticleID);
  Out_XQuotedAttr( XMLAttrName[atMizfiles], MizFiles);
  Out_XAttrEnd;
  for i:=0 to Count-1 do
   with TokenPtr(Items^[i])^, fLexem do
  begin
   Out_XElStart( XMLElemName[elSymbol]);
   Out_XQuotedAttr( XMLAttrName[atKind], Kind);
   Out_XIntAttr( XMLAttrName[atNr], Nr);
   Out_XQuotedAttr( XMLAttrName[atName], fStr);
   Out_XElEnd0;
   end;
  Out_XElEnd( XMLElemName[elSymbols]);
 end;
 lEnvFile.Done;
end;

constructor MTokenObj.Init(aKind:char; aNr:integer; const aSpelling: string; const aPos: Position);
begin
 fLexem.Kind:=aKind;
 fLexem.Nr:=aNr;
 fStr:=aSpelling;
 fPos:=aPos;
end;

const
 Numeral = 'N';
 Identifier = 'I';
 ErrorSymbol = '?';
 EOT = '!';

function MTokeniser.Spelling(const aToken: LexemRec): string;
 var i: integer; s: string;
begin Spelling:='';
 if aToken.Kind = Numeral then
  begin
   Str(aToken.Nr,s);
   Spelling:=s;
  end else
 if aToken.Kind = Identifier then
  begin
   for i:=0 to fIdents.Count-1 do
    with TokenPtr(fIdents.Items^[i])^ do
     if fLexem.Nr = aToken.Nr then
      begin Spelling:=fStr; exit end;
  end
 else
  begin
   for i:=0 to fTokens.Count-1 do
    with TokenPtr(fTokens.Items^[i])^ do
     if (fLexem.Kind = aToken.Kind) and (fLexem.Nr = aToken.Nr) then
      begin Spelling:=fStr; exit end;
  end;
end;

constructor MTokeniser.Init;
begin
 fPos.Line:=0;
 fLexem.Kind:=' ';
 fPhrase:='  ';
 fPhrasePos.Line:=0;
 fPhrasePos.Col:=0;
 fTokensBuf.Init(80,8);
 fTokens.Init(0);
 fIdents.Init(100);
end;

destructor MTokeniser.Done;
begin
 fPhrase:='';
 fTokensBuf.Done;
 fTokens.Done;
 fIdents.Done;
end;

procedure MTokeniser.SliceIt;
var
  lCurrChar,EndOfPhrase, lIndex, EndOfSymbol, EndOfIdent, IdentLength,
  lToken, lFailed, I,J: integer;
  lNumber: longint;
  FoundToken, lIdent: TokenPtr;
  lSpelling: string;
  lPos:Position;
begin
 mizassert(2333,fTokensBuf.Count=0);
 lCurrChar:=1;
 lPos:=fPhrasePos;
 if (lPos.Col = 1) and (Pos('::$',fPhrase) = 1) then
  begin
   fTokensBuf.Insert(new(MTokenPtr,Init(' ',0,
                      copy(fPhrase,3,length(fPhrase)-3),lPos)));
   if copy(fPhrase,1,6)='::$EOF' then
    fTokensBuf.Insert(new(MTokenPtr,Init(EOT,0,fPhrase,lPos)));
   exit
  end;
 while fPhrase[lCurrChar]<>' ' do
 begin
   EndOfIdent:=lCurrChar;
   { 1. proba wyznaczenia identyfikatora }
   if IsIdentifierFirstLetter(fPhrase[EndOfIdent]) then
    while (EndOfIdent< length(fPhrase)) and
     IsIdentifierLetter(fPhrase[EndOfIdent]) do inc(EndOfIdent);
   IdentLength:=EndOfIdent-lCurrChar;
   if fPhrase[EndOfIdent] <= chr(31) then
    begin lPos.Col:=fPhrasePos.Col+EndOfIdent-1;
     if fPhrase[EndOfIdent] = chr(30) then
      fTokensBuf.Insert(new(MTokenPtr,Init(ErrorSymbol,200,'',lPos)))
     else
      fTokensBuf.Insert(new(MTokenPtr,Init(ErrorSymbol,201,'',lPos)));
     lCurrChar:=EndOfIdent+1;
     continue;
    end;
   dec(EndOfIdent);
   { 2. proba znalezienia symbolu slownikowego }
   EndOfPhrase:=lCurrChar; FoundToken:=nil;
   EndOfSymbol:=EndOfPhrase-1; { inicjalizowany dla porownania }
   { indeks poczateku listy symboli zaczynajacych sie na dana litere
     Symbole sa posortowane leksykograficznie
   }
   lToken:=fTokens.fFirstChar[fPhrase[EndOfPhrase]];
   inc(EndOfPhrase);
   if (lToken >= 0) then
   with fTokens do
    begin
     lIndex:=2;
     repeat
      if lIndex>length(TokenPtr(Items^[fIndex^[lToken]])^.fStr) then
       begin
        FoundToken:=Items^[fIndex^[lToken]];
        EndOfSymbol:=EndOfPhrase-1;
       end;
      if fPhrase[EndOfPhrase] = ' ' then break;
      if (lIndex<=length(TokenPtr(Items^[fIndex^[lToken]])^.fStr)) and
         (TokenPtr(Items^[fIndex^[lToken]])^.fStr[lIndex] = fPhrase[EndOfPhrase])
       then
       begin
        inc(lIndex);
        inc(EndOfPhrase)
       end
      else if (lToken < Count-1) then
        begin
         if (copy(TokenPtr(Items^[fIndex^[lToken]])^.fStr,1,lIndex-1)=
             copy(TokenPtr(Items^[fIndex^[lToken+1]])^.fStr,1,lIndex-1))
         then inc(lToken)
         else break;
        end
      else break;
     until false;
    end;
   if EndOfSymbol < EndOfIdent then
    begin
     { sprawdzenie czy idntyfikator nie jest liczba }
     lSpelling:=copy(fPhrase,lCurrChar,IdentLength);
     lPos.Col:=fPhrasePos.Col+EndOfIdent-1;
     if (ord(fPhrase[lCurrChar])>ord('0')) and
        (ord(fPhrase[lCurrChar])<=ord('9')) then
      begin
       lFailed:=0;
       for I:=1 to IdentLength-1 do
        if (ord(fPhrase[lCurrChar+I])<ord('0')) or
           (ord(fPhrase[lCurrChar+I])>ord('9')) then
         begin
          lFailed:=I+1;
          break;
         end;
       if lFailed=0 then
        begin
         if IdentLength > length(IntToStr(MaxConstInt)) then
          begin
           fTokensBuf.Insert(new(MTokenPtr,Init(ErrorSymbol,202,lSpelling,lPos)));
           lCurrChar:=EndOfIdent+1;
           continue;
          end;
         lNumber:=0;
         J:=1;
         for I:=IdentLength-1 downto 0 do
          begin
           lNumber:=lNumber+(ord(fPhrase[lCurrChar+I])-ord('0'))*J;
           J:=J*10;
          end;
         if lNumber > MaxConstInt then
          begin
           fTokensBuf.Insert(new(MTokenPtr,Init(ErrorSymbol,202,lSpelling,lPos)));
           lCurrChar:=EndOfIdent+1;
           continue;
          end;
         fTokensBuf.Insert(new(MTokenPtr,Init(Numeral,lNumber,lSpelling,lPos)));
         lCurrChar:=EndOfIdent+1;
         continue;
        end;
      end;
     lIdent:=new(TokenPtr,Init(Identifier,fIdents.Count+1,lSpelling));
     if fIdents.Search(lIdent,I)
      then dispose(lIdent,Done)
      else fIdents.Insert(lIdent);
     fTokensBuf.Insert(new(MTokenPtr,
         Init(Identifier,TokenPtr(fIdents.Items^[I])^.fLexem.Nr,lSpelling,lPos)));
     lCurrChar:=EndOfIdent+1;
     continue;
    end;
   if FoundToken <> nil then
   with FoundToken^ do
    begin
     lPos.Col:=fPhrasePos.Col+EndOfSymbol-1;
     fTokensBuf.Insert(new(MTokenPtr,Init(fLexem.Kind,fLexem.Nr,fStr,lPos)));
     lCurrChar:=EndOfSymbol+1;
     continue;
    end;
   lPos.Col:=fPhrasePos.Col+lCurrChar-1;
   fTokensBuf.Insert(new(MTokenPtr,Init(ErrorSymbol,203,fPhrase[lCurrChar],lPos)));
   inc(lCurrChar);
 end;
end;

procedure MTokeniser.GetPhrase;
begin Abstract1; end;

function MTokeniser.EndOfText: boolean;
begin Abstract1; EndOfText:= false; end;

function MTokeniser.IsIdentifierLetter(ch: char): boolean;
begin Abstract1; IsIdentifierLetter:= false; end;

procedure MTokeniser.GetToken;
begin
 if fLexem.Kind = EOT then exit;
 if fTokensBuf.Count > 0 then
  begin
   fLexem:=MTokenPtr(fTokensBuf.Items^[0])^.fLexem;
   fStr:=MTokenPtr(fTokensBuf.Items^[0])^.fStr;
   fPos:=MTokenPtr(fTokensBuf.Items^[0])^.fPos;
   fTokensBuf.AtFree(0);
   exit;
  end;
 GetPhrase;
 if EndOfText then
  begin fLexem.Kind:=EOT;
   fStr:='';
   fPos:=fPhrasePos;
   inc(fPos.Col);
   exit;
  end;
 SliceIt;
 fLexem:=MTokenPtr(fTokensBuf.Items^[0])^.fLexem;
 fStr:=MTokenPtr(fTokensBuf.Items^[0])^.fStr;
 fPos:=MTokenPtr(fTokensBuf.Items^[0])^.fPos;
 fTokensBuf.AtFree(0);
end;

function MTokeniser.IsIdentifierFirstLetter(ch: char): boolean;
begin
 IsIdentifierFirstLetter:=IsIdentifierLetter(ch);
end;

procedure MScannObj.GetPhrase;
 const
  Prohibited: ASCIIArr =
   (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);
 var i,k: integer;
begin
 fPhrasePos.Col:=fPhrasePos.Col+length(fPhrase)-1;
 { uzyskanie pierwszego znaczacego znaku }
 while fCurrentLine[fPhrasePos.Col] = ' ' do
  begin
   if fPhrasePos.Col >= length(fCurrentLine) then
    begin
     if EndOfText then exit;
     inc(fPos.Line);
     inc(fPhrasePos.Line);
     readln(fSourceFile,fCurrentLine);
     k:=Pos('::$',fCurrentLine);
     if (k = 1) then
      begin
       ProcessComment(fPhrasePos.Line, 1, copy(fCurrentLine, 1, length(fCurrentLine)));
       k:=length(fCurrentLine);
       while (k > 0) and (fCurrentLine[k] = ' ') do dec(k);
       delete(fCurrentLine,k+1,length(fCurrentLine));
       fCurrentLine:=fCurrentLine+' ';
       fPhrase:=Copy(fCurrentLine,1,length(fCurrentLine));
       fPhrasePos.Col:=1;
       fPos.Col:=0;
       exit
      end;
     k:=Pos('::',fCurrentLine);
     if (k <> 0) then
      begin
       ProcessComment(fPhrasePos.Line, k, copy(fCurrentLine, k, length(fCurrentLine)));
       delete(fCurrentLine,k+1,length(fCurrentLine));
       fCurrentLine[k]:=' ';
      end;
     k:=length(fCurrentLine);
     while (k > 0) and (fCurrentLine[k] = ' ') do dec(k);
     delete(fCurrentLine,k+1,length(fCurrentLine));
     for k:=1 to length(fCurrentLine)-1 do
      if Prohibited[fCurrentLine[k]]>0 then fCurrentLine[k]:=chr(31);
     fCurrentLine:=fCurrentLine+' ';
     if not LongLines then
     if length(fCurrentLine) > MaxLineLength then
      begin delete(fCurrentLine,MaxLineLength+1,length(fCurrentLine));
       fCurrentLine[MaxLineLength-1]:=chr(30); fCurrentLine[MaxLineLength]:=' ';
      end;
     fPhrasePos.Col:=0;
     fPos.Col:=0;
    end;
   inc(fPhrasePos.Col);
  end;
 for i:=fPhrasePos.Col to length(fCurrentLine) do
  if fCurrentLine[i] =  ' ' then break;
 fPhrase:=Copy(fCurrentLine,fPhrasePos.Col,i-fPhrasePos.Col+1);
end;

procedure MScannObj.ProcessComment(fLine, fStart: integer; cmt: string);
begin end;

function MScannObj.EndOfText: boolean;
begin
 EndOfText := (fPhrasePos.Col >= length(fCurrentLine)) and eof(fSourceFile);
end;

function MScannObj.IsIdentifierLetter(ch: char): boolean;
begin
 IsIdentifierLetter:=Allowed[ch]<>0;
end;

constructor MScannObj.InitScanning(const aFileName,aDctFileName:string);
begin
 inherited Init;
 Allowed:=DefaultAllowed;
 fTokens.LoadDct(aDctFileName);
 assign(fSourceFile,aFileName);
 fSourceBuffSize:=$4000;
 getmem(fSourceBuff,fSourceBuffSize);
 settextbuf(fSourceFile,fSourceBuff^,$4000);
 reset(fSourceFile);
 fCurrentLine:=' ';
 GetToken;
end;

destructor MScannObj.Done;
begin 
 close(fSourceFile);
 FreeMem(fSourceBuff,fSourceBuffSize);
 fCurrentLine:='';
 inherited Done;
end;

end.
