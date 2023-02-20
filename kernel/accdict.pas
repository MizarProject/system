(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit accdict;
interface

uses mobjects,envhan,dicthan;

type

 DictBasePtr = ^DictBaseObj;
 DictBaseObj = Object(MStrObj)
     nBase: SymbolCounters;
    constructor Init(fIdent:string);
  end;

var
 gSymBase  : SymbolCounters;
 DictBase  : MSortedStrList;

procedure ProcessVocabularies;

procedure PrintVocabulariesList;

procedure PrintPRF(fFileName:string);
procedure PrintStrictPRF(fFileName:string);

implementation

uses pcmizver,mizenv,librenv,errhan,_formats,
     scanner,mscanner,xml_dict,xml_inout;

var
  TokCounter: SymbolCounters;
  gTokens : TokensCollection;

constructor DictBaseObj.Init(fIdent:string);
begin
 inherited Init(fIdent);
 fillchar(nBase,sizeof(nBase),0);
end;

procedure PrintPRF(fFileName:string);
 var i:integer;
     DctFile: text;
begin
 assign(DctFile,fFileName+'.prf'); rewrite(DctFile);
 Writeln(DctFile,gSymBase['M'],' ',gSymBase['G'],' ',gSymBase['R'],' ');
 if gSymBase['M'] > 0 then
  begin
   for i:=1 to gSymBase['M'] do
    write(DctFile,ModeMaxArgs.fList^[i],' ');
   writeln(DctFile);
  end;
 if gSymBase['G'] > 0 then
  begin
   for i:=1 to gSymBase['G'] do
    write(DctFile,StructModeMaxArgs.fList^[i],' ');
   writeln(DctFile);
  end;
 if gSymBase['R'] > 0 then
  begin
   for i:=1 to gSymBase['R'] do
    write(DctFile,PredMaxArgs.fList^[i],' ');
   writeln(DctFile);
  end;
 ModeMaxArgs.Done;
 StructModeMaxArgs.Done;
 PredMaxArgs.Done;
 close(DctFile);
end;

procedure PrintStrictPRF(fFileName:string);
 var DctFile: text;
begin
 assign(DctFile,fFileName+'.prf'); rewrite(DctFile);
 Writeln(DctFile,'0 0 0 ');
 close(DctFile);
end;

procedure CollectTheorems;
 var lToken: LexemRec;
 procedure AddTheoremIdent(fIdent:PImpArticleId);
  begin inc(TokCounter['A']);
    lToken.Nr:=TokCounter['A'];
    with fIdent^ do
       if not gTokens.CollectToken(lToken,fStr)
      then Error(fPos,815);
  end;
 procedure AddNotationsIdent(fIdent:PImpArticleId);
  begin
    with fIdent^ do
     begin
      if Env.Directive[syTheorems].IndexOfStr(fStr)<0 then
       begin
        inc(TokCounter['A']);
        lToken.Nr:=TokCounter['A'];
        if not gTokens.CollectToken(lToken,fStr)
         then Error(fPos,815);
       end;
     end;
  end;
 var z: integer;
begin
 lToken.Kind:='A';
 with Env.Directive[syTheorems] do
  for z:=0 to Count-1 do AddTheoremIdent(PImpArticleId(Items^[z]));
 with Env.Directive[syNotations] do
  for z:=0 to Count-1 do AddNotationsIdent(PImpArticleId(Items^[z]));
end;

procedure InitDictBase;
begin
 FillChar(TokCounter,SizeOf(TokCounter),0);
 DictBase.Init(Env.Directive[syVocabularies].Count+10);
 DictBase.Insert(new(DictBasePtr,Init('')));
 gPriority.Init(10);
 TokCounter['M']:=1;             { "set" }
 TokCounter['R']:=1;             { "="   }
 TokCounter['K']:=3;             (* nawias lewy "[" i "{"  "(" *)
 TokCounter['L']:=3;             (* nawias prawy "]" i "}" ")" *)
end;

// ##TODO: try to merge with OutMarkedVocs
procedure PrintVocabulariesList;
var lOutEnvFile:XMLOutStreamObj; i:integer; c,s:char; lCounts:SymbolCounters;
begin
 lOutEnvFile.OpenFile( EnvFileName+'.vcl');
 lOutEnvFile.Out_XElStart0( XMLElemName[elVocabularies]);
 for i:=1 to DictBase.Count-1 do
  with DictBasePtr(DictBase.Items^[i])^ do
   begin
    for c:='A' to 'Z' do
      lCounts[c]:= nBase[c] - DictBasePtr(DictBase.Items^[i-1])^.nBase[c];
    lOutEnvFile.Out_XElStart0( XMLElemName[elVocabulary]);
    lOutEnvFile.Out_XElStart( XMLElemName[elArticleID]);
    lOutEnvFile.Out_XAttr( XMLAttrName[atName], fStr);
    lOutEnvFile.Out_XElEnd0;
    for s:='A' to 'Z' do if s in AvailableSymbols then
     begin
      lOutEnvFile.Out_XElStart( XMLElemName[elSymbolCount]);
      lOutEnvFile.Out_XAttr( XMLAttrName[atKind], s);
      lOutEnvFile.Out_XIntAttr( XMLAttrName[atNr], lCounts[s]);
      lOutEnvFile.Out_XElEnd0;
     end;
    lOutEnvFile.Out_XElEnd( XMLElemName[elVocabulary]);
   end;
 lOutEnvFile.Out_XElEnd( XMLElemName[elVocabularies]);
 lOutEnvFile.Done;
end;

procedure ProcessVocabularies;
 var VocFile: text;
 procedure AddIdent(fIdent:PImpArticleId);
   var lLexem: LexemRec;
  begin inc(TokCounter['A']);
    lLexem.Kind:='A';
    lLexem.Nr:=TokCounter['A'];
    with fIdent^ do
     if not gTokens.CollectToken(lLexem,fStr)
      then Error(fPos,815);
  end;
 procedure ProcessVocabulary(fIdent:PImpArticleId);
  procedure AddSymbol(fSymbol: PSymbol);
     var lSymbol: LexemRec; lPrior: integer;
   begin
    with fSymbol^ do
     begin
      lSymbol.Kind:=Kind;
      inc(TokCounter[lSymbol.Kind]);
      lSymbol.Nr:=TokCounter[lSymbol.Kind];
      lPrior:=Prior;
      if lPrior = 0 then lPrior:=StandardPriority;
      case Kind of
      'O': gPriority.Assign(ord('O'),TokCounter['O'],lPrior);
      'K': gPriority.Assign(ord('K'),TokCounter['K'],lPrior);
      'L': gPriority.Assign(ord('L'),TokCounter['L'],lPrior);
      end;
      if not gTokens.CollectToken(lSymbol,Repr)
       then Error(fIdent^.fPos,815);
      if (Infinitive <> '') and  not gTokens.CollectToken(lSymbol,Infinitive)
       then Error(fIdent^.fPos,815);
     end;
   end;
  var lDictBase: DictBasePtr;
      lVoc: PVocabulary;
      z: integer;
  label 1;
 begin
  if MFileExists('dict'+DirSeparator+LowerCase(fIdent^.fStr)+'.voc') then
   begin
     lVoc:=GetPrivateVoc('dict'+DirSeparator+LowerCase(fIdent^.fStr)+'.voc');
     if lVoc = nil then begin Error(fIdent^.fPos,801); goto 1 end;
     with lVoc^.Reprs do
      for z:=0 to Count-1 do AddSymbol(PSymbol(Items^[z]));
     LocFilesCollection.Insert(
     New(PFileDescr,Init('dict'+DirSeparator+LowerCase(fIdent^.fStr)+'.voc',0)));
   end
  else
   begin
     lVoc:=GetPublicVoc(fIdent^.fStr,VocFile);
     if lVoc = nil then begin Error(fIdent^.fPos,801); goto 1 end;
     with lVoc^.Reprs do
      for z:=0 to Count-1 do AddSymbol(PSymbol(Items^[z]));
   end;
1:
  lDictBase:=New(DictBasePtr,Init(fIdent^.fStr));
  lDictBase^.nBase:=TokCounter;
  DictBase.Insert(lDictBase);
 end;
  var z: integer;
begin
 FileExam(MizFiles+'mizar.dct');
 gTokens.LoadDct(MizFiles+'mizar');
 InitDictBase;
 FileExam(MizFiles+MML+'.vct');
 Assign(VocFile,MizFiles+MML+'.vct');
 Reset(VocFile);
 with Env.Directive[syVocabularies] do
  for z:=0 to Count-1 do
   begin
    CurPos:=PImpArticleId(Items^[z])^.fPos;
    ProcessVocabulary(PImpArticleId(Items^[z]));
   end;
 close(VocFile);
 gSymBase:=TokCounter;
 with Env.Directive[syTheorems] do
  for z:=0 to Count-1 do AddIdent(PImpArticleId(Items^[z]));
 with Env.Directive[sySchemes] do
  for z:=0 to Count-1 do
    if gTokens.IndexOfStr(PImpArticleId(Items^[z])^.fStr) = -1 then
      AddIdent(PImpArticleId(Items^[z]));
 gTokens.SaveDct(EnvFileName);
 gTokens.SaveXDct(EnvFileName+'.dcx');
 gTokens.Done;
end;

end.
