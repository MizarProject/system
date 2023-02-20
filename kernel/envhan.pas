(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit envhan;
interface

uses mobjects,errhan,scanner,mscanner;

type
 NameStr = string[8];

 PImpArticleId = ^ImpArticleId;
 ImpArticleId = object(MStrObj)
     fPos: Position;
    constructor Init(const aIdent:string; const aPos:Position);
  end;

 PEnvironmentDescr = ^EnvironmentDescr;
 EnvironmentDescr = object(MObject)
     Erroneous: boolean;
     Directive: array [DirectiveKind] of MSortedStrList;
    constructor InitEvl;
    constructor LoadEvl(const aFileName: string);
    procedure StoreEvl(const aFileName: string);
    procedure ReadEnvironment;

    constructor Init;
    destructor Done; virtual;
   end;

var Env: EnvironmentDescr;

type

 AccScannPtr = ^AccScannObj;
 AccScannObj = object(MScannObj)
   constructor InitScanning(const aFileName,aDctFileName:string);
   destructor Done; virtual;
   function IsIdentifierLetter(ch: char): boolean; virtual;
 end;

implementation

uses pcmizver,mizenv,librenv,mconsole,xml_dict,xml_inout;

constructor AccScannObj.InitScanning(const aFileName,aDctFileName:string);
begin
 inherited InitScanning(aFileName,aDctFileName);
end;

destructor AccScannObj.Done;
begin
 inherited Done;
end;

function AccScannObj.IsIdentifierLetter(ch: char): boolean;
begin
 IsIdentifierLetter:=Allowed[ch]<>0;
end;

constructor ImpArticleId.Init(const aIdent:string; const aPos:Position);
begin
 fStr:=aIdent; fPos:=aPos
end;

constructor EnvironmentDescr.Init;
 var lDir: DirectiveKind;
begin
 Erroneous:=false;
 for lDir:=low(DirectiveKind) to High(DirectiveKind) do
  Directive[lDir].Init(20);
end;

constructor EnvironmentDescr.InitEvl;
 var lPos:Position; lDir: DirectiveKind;
begin
 Erroneous:=false;
 with lPos do begin Line:=1; Col:=1 end;
 for lDir:=low(DirectiveKind) to High(DirectiveKind) do
  Directive[lDir].Init(20);
 Directive[syVocabularies].Insert(new(PImpArticleId,Init('HIDDEN',lPos)));
 if InsertHiddenFiles then
 begin
 Directive[syNotations].Insert(new(PImpArticleId,Init('HIDDEN',lPos)));
 Directive[syConstructors].Insert(new(PImpArticleId,Init('HIDDEN',lPos)));
 Directive[syRequirements].Insert(new(PImpArticleId,Init('HIDDEN',lPos)));
 end;
end;

destructor EnvironmentDescr.Done;
 var lDir: DirectiveKind;
begin
 Erroneous:=true;
 for lDir:=low(DirectiveKind) to High(DirectiveKind) do
  Directive[lDir].Done;
end;

procedure EnvironmentDescr.ReadEnvironment;
 const
  MainSet: set of TokenKind
   = [sy_Environ,sy_Begin,sy_Semicolon,sy_Proof,sy_Now,EOT,sy_Definition,sy_Theorem,
      sy_End,sy_LibraryDirective,sy_Reserve,sy_Struct,sy_Scheme];

 var Syntactically_Correct: boolean;

 procedure SynErr(fPos:Position; fErrNr:integer);
 begin
  if Syntactically_Correct then
   begin Syntactically_Correct:=false;
    if CurWord.Kind = sy_Error then Error(CurPos,CurWord.Nr)
     else Error(fPos,fErrNr);
    while not (CurWord.Kind in MainSet) do ReadToken;
   end;
 end;

 procedure WrongWord(fErrNr:integer);
 begin SynErr(CurPos,fErrNr) end;

 procedure SemErr(fErrNr:integer);
 begin if Syntactically_Correct then Error(CurPos,fErrNr) end;

 function Occurs(fW:TokenKind):boolean;
 begin Occurs:=false;
  if CurWord.Kind=FW then begin ReadToken; Occurs:=true end
 end;

 procedure MissingWord(fErrNr:integer);
  var lPos: Position;
 begin lPos:=CurPos; inc(lPos.Col); SynErr(lPos,fErrNr) end;

 procedure Accept(fCh:TokenKind; fErrNr:integer);
 begin if not Occurs(fCh) then MissingWord(fErrNr) end;

 procedure ReadDirectives(var fImport:MSortedStrList);
  var lName: string;
 begin
  ReadToken;
  repeat
   if CurWord.Kind <> Identifier then WrongWord(300) else
    begin
{     lName:=Copy(CurWord.Spelling,1,8);}
{     lName:=UpperCase(TrimString(lName));}
     lName:=CurWord.Spelling;
     if Length(lName) > 8 then SemErr(892) else
      if lName <> UpperCase(lName) then SemErr(891) else
     begin
       if fImport.IndexOfStr(lName)>=0 then SemErr(810)
         else fImport.Insert(new(PImpArticleId,Init(lName,CurPos)));
       ReadToken;
     end;
    end;
  until not Occurs(sy_Comma);
  Accept(sy_Semicolon,217);
 end;

 procedure UnexpectedWord(fErrNr:integer);
  var lPos: Position;
 begin lPos:=CurPos; ReadToken; SynErr(lPos,fErrNr) end;

 label 1;
begin
 FileExam(MizFiles+'mizar.dct');
 FileExam(MizFileName+ArticleExt);
 gScanner:=new(AccScannPtr, InitScanning(MizFileName+ArticleExt,MizFiles+'mizar'));
 StartScaner;
 ReadToken;
 Syntactically_Correct:=true;
 if CurWord.Kind= sy_Environ then ReadToken else SemErr(212);
 EnvironmentDescr.InitEvl;
 while CurWord.Kind <> sy_Begin do
  begin Syntactically_Correct:=true; DisplayLine(CurPos.Line,ErrorNbr);
   case CurWord.Kind of
    sy_LibraryDirective:
      ReadDirectives(Directive[DirectiveKind(CurWord.Nr-1)]);
    EOT:
      goto 1;
    sy_Environ:
      UnexpectedWord(211);
    sy_Reserve,sy_Struct,sy_Scheme,sy_Theorem,sy_Now,sy_Hereby,sy_Proof,sy_Definition,
    sy_Notation,sy_Registration:
     begin
      WrongWord(213);
      goto 1
     end;
    sy_End:
      UnexpectedWord(216);
    else
     begin
      WrongWord(210);
      Syntactically_Correct:=true;
      Accept(sy_Semicolon,217);
     end
   end;
  end;
1:
 CloseSourceFile;
end;

constructor EnvironmentDescr.LoadEvl(const aFileName: string);
 var d: DirectiveKind;
     i, lCnt: Integer;
     lPos: Position;
     lSrc: XMLInStreamPtr;
begin
  FileExam(aFileName);
  Erroneous:=false;
  lSrc:=new(XMLInStreamPtr,OpenFile(aFileName));
  lSrc^.NextElementState; { Environ }
  for d:=low(DirectiveKind) to High(DirectiveKind) do
  begin
    lSrc^.NextElementState; { Directive }
    lCnt:=lSrc^.GetIntAttr(XMLAttrName[atArgNr]);
    Directive[d].Init(lCnt);
    for i:=1 to lCnt do
    begin
      lSrc^.NextElementState; { Ident }
      lPos.Line:=lSrc^.GetIntAttr(XMLAttrName[atLine]);
      lPos.Col:=lSrc^.GetIntAttr(XMLAttrName[atCol]);
      Directive[d].Insert(new(PImpArticleId, Init(lSrc^.GetAttr(XMLAttrName[atName]), lPos)));
      lSrc^.AcceptEndState; { /Ident }
    end;
    lSrc^.NextElementState; { /Directive }
  end;
  lSrc^.NextElementState; { /Environ }
  dispose(lSrc,Done);
end;

procedure EnvironmentDescr.StoreEvl(const aFileName: string);
 var d: DirectiveKind;
     i: Integer;
     lEvl: XMLOutStreamPtr;
begin
  for d:=low(DirectiveKind) to High(DirectiveKind) do Directive[d].Pack;
  lEvl:=new(XMLOutStreamPtr,OpenFile(aFileName));
  lEvl^.Out_XElStart( XMLElemName[elEnviron]);
  lEvl^.Out_XAttr( XMLAttrName[atAid], ArticleID);
  lEvl^.Out_XAttrEnd;
  for d:=Low(DirectiveKind) to High(DirectiveKind) do
  with Directive[d] do
  begin
   lEvl^.Out_XElStart( XMLElemName[elDirective]);
   lEvl^.Out_XAttr( XMLAttrName[atName], DirectiveName[d]);
   lEvl^.Out_XIntAttr( XMLAttrName[atArgNr], Count);
   lEvl^.Out_XAttrEnd;
   for i:=0 to Count-1 do
    with PImpArticleId(Items^[i])^ do
    begin
     lEvl^.Out_XElStart( XMLElemName[elIdent]);
     lEvl^.Out_XAttr( XMLAttrName[atName], fStr);
     lEvl^.Out_XIntAttr( XMLAttrName[atLine], fPos.Line);
     lEvl^.Out_XIntAttr( XMLAttrName[atCol], fPos.Col);
     lEvl^.Out_XElEnd0;
    end;
   lEvl^.Out_XElEnd( XMLElemName[elDirective]);
  end;
  lEvl^.Out_XElEnd( XMLElemName[elEnviron]);
  dispose(lEvl,Done);
end;

end.
