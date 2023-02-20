(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program IrrVoc;

uses mobjects,pcmizver,mconsole,errhan,mizenv,monitor,
     mscanner,scanner,dicthan,accdict,envhan,xml_dict,xml_inout,xml_parser
     {$IFDEF MDEBUG},info{$ENDIF};

type
  PLexem = ^ProfLexem;
  ProfLexem = object(MObject)
      fSymb: LexemRec;
      fCnt: integer;
     constructor Init(aKind:char; aNr,aCnt:integer);
    end;

  TLexemCollection = object(MSortedCollection)
     function Compare(Key1, Key2: Pointer): Integer; virtual;
    end;

  PIrrVocabulary = ^TIrrVocabulary;
  TIrrVocabulary = object(MObject)
     nName: NameStr; Base: SymbolCounters;
     Lexems: TLexemCollection;
    constructor Init(const fName:string; var fBase:SymbolCounters);
    procedure AddLexem(const aLexem:ProfLexem);
   end;

constructor ProfLexem.Init;
begin
 fSymb.Kind:=aKind; fSymb.Nr:=aNr; fCnt:=aCnt;
end;

constructor TIrrVocabulary.Init;
begin
  nName:=fName; Base:=fBase;
  Lexems.Init(4,1);
end;

procedure TIrrVocabulary.AddLexem;
 var i:integer; lLexem: ProfLexem;
begin lLexem.Init(aLexem.fSymb.Kind,aLexem.fSymb.Nr,0);
 if Lexems.Search(addr(lLexem),i) then
   inc(PLexem(Lexems.Items^[i])^.fCnt)
 else Lexems.Insert(new(PLexem,Init(aLexem.fSymb.Kind,aLexem.fSymb.Nr,1)));
end;

function TLexemCollection.Compare;
begin
 if PLexem(Key1)^.fSymb.Kind < PLexem(Key2)^.fSymb.Kind then Compare:=-1
 else if PLexem(Key1)^.fSymb.Kind > PLexem(Key2)^.fSymb.Kind then Compare:=1
 else if PLexem(Key1)^.fSymb.Nr < PLexem(Key2)^.fSymb.Nr then Compare:=-1
 else if PLexem(Key1)^.fSymb.Nr > PLexem(Key2)^.fSymb.Nr then Compare:=1
 else Compare:=0
end;

var VocCollection: MCollection;

procedure ReadVCL;
 var i,lLexNr: integer; c,lLexKind:char;
     lTokBase: SymbolCounters;
     lVocs: MStringList;
     lName: string;
     lInFile: XMLInStreamPtr;
     lDictBase: AbsVocabularyPtr;
begin
 lVocs.Init(8);
 FillChar( lTokBase, sizeof(lTokBase), 0);
 FileExam(MizFileName+'.vcl');
 lInFile:=new(XMLInStreamPtr,OpenFile(MizFileName+'.vcl'));
 lInFile^.NextElementState;
 lInFile^.NextElementState;
 while not (lInFile^.nState = eEnd) and (lInFile^.nElName = XMLElemName[elVocabulary]) do
 begin
  lInFile^.NextElementState;
  lName:= lInFile^.GetAttr( XMLAttrName[atName]);
  lInFile^.AcceptEndState; lInFile^.NextElementState; // end of elArticleID
  lDictBase:=new(AbsVocabularyPtr,Init);
  lDictBase^.fSymbolCnt:=lTokBase;
  while not (lInFile^.nState = eEnd) and (lInFile^.nElName = XMLElemName[elSymbolCount]) do
  begin
   lLexKind:=lInFile^.GetAttr(XMLAttrName[atKind])[1];
   lLexNr:=lInFile^.GetIntAttr(XMLAttrName[atNr]);
   inc(lDictBase^.fSymbolCnt[ lLexKind], lLexNr);
   lInFile^.AcceptEndState; lInFile^.NextElementState;
  end;
  lVocs.AddObject( lName, lDictBase);
  lInFile^.NextElementState;
 end;
 lInFile^.NextElementState;
 dispose(lInFile,Done);
 VocCollection.Init(10,1);
// fillchar(lTokBase,sizeof(lTokBase),0);
 VocCollection.Insert(new(PIrrVocabulary,init('',lTokBase)));
 for i:= 0 to lVocs.fCount - 1 do
 begin
  for c:= Low(SymbolCounters) to High(SymbolCounters) do
   inc( lTokBase[c], AbsVocabularyPtr( lVocs.GetObject(i))^.fSymbolCnt[c]);
  VocCollection.Insert(new(PIrrVocabulary, init( lVocs.GetString(i), lTokBase)));
 end;
end;

procedure ChkEVL;
 var i,j: integer;
 label 1,2;
begin
 Env.LoadEvl(EnvFileName+'.evl');
 for i:=1 to Env.Directive[syVocabularies].Count-1 do
  begin
   for j:=0 to VocCollection.Count-1 do
    with PIrrVocabulary(VocCollection.Items^[j])^ do
     if nName = PImpArticleId(Env.Directive[syVocabularies].Items^[i])^.fStr then
      begin
       if Lexems.Count > 0 then goto 2;
       goto 1;
      end;
1:
   Error(PImpArticleId(Env.Directive[syVocabularies].Items^[i])^.fPos,709);
   Env.Directive[syVocabularies].Items^[i]:=nil;
2:
  end;
 if ErrorNbr> 0 then Env.StoreEvl(EnvFileName+'.$ev');
end;

procedure PrintProf;
 var i,z:integer;
     Prof:text;
begin
 assign(Prof,MizFileName+'.irv'); rewrite(Prof);
 for i:=1 to VocCollection.Count-1 do
  with PIrrVocabulary(VocCollection.Items^[i])^ do
   if Lexems.Count > 0 then
    begin writeln(Prof,nName);
     with Lexems do
      for z:=0 to Count-1 do
       with PLexem(At(z))^ do
        write(Prof,fSymb.Kind,fSymb.Nr,' ',fCnt,' ');
     writeln(Prof,';');
    end;
 close(Prof);
end;

procedure ProcessText;
 var i: integer; lLexem: ProfLexem;
 label 1;
begin
 gScanner:=new(AccScannPtr, InitScanning(MizFileName+ArticleExt,EnvFileName));
 StartScaner;
 LoadPrf(EnvFileName);
 ReadToken;
 while CurWord.Kind <> EOT do
  begin
   if CurPos.Line mod 4 = 0
    then DisplayLine(CurPos.Line,ErrorNbr);
   if chr(ord(CurWord.Kind)) in ['A'..'Z'] then
   for i:=1 to VocCollection.Count-1 do
    if (PIrrVocabulary(VocCollection.Items^[i-1])^.Base[chr(ord(CurWord.Kind))]<CurWord.Nr)
      and (CurWord.Nr<=PIrrVocabulary(VocCollection.Items^[i])^.Base[chr(ord(CurWord.Kind))])
     then
    begin lLexem.Init(chr(ord(CurWord.Kind)),CurWord.Nr,0);
     dec(lLexem.fSymb.Nr,PIrrVocabulary(VocCollection.Items^[i-1])^.Base[lLexem.fSymb.Kind]);
     PIrrVocabulary(VocCollection.Items^[i])^.AddLexem(lLexem);
     goto 1;
    end;
1:
   ReadToken;
  end;
 CloseSourceFile;
 DisposePrf;
end;

begin
 DrawMizarScreen('Irrelevant ''vocabulary'' Detector');
 InitExitProc;
 GetArticleName; GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 {$IFDEF MDEBUG}
 OpenInfoFile;
 {$ENDIF}
 InitDisplayLine('Scanning');
 ReadVCL;
 ProcessText;
 PrintProf;
 if ErrorNbr <> 0 then
  begin
   DrawErrorsMsg(ErrorNbr);
   halt(1);
  end;
 ChkEVL;
 DrawErrorsMsg(ErrorNbr);
 FinishDrawing;
end.
