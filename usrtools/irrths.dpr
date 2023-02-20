(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program IrrTheoremsAndSchemes;

uses mobjects,pcmizver,mconsole,mizenv,monitor,errhan,envhan,mscanner,scanner
{$IFDEF MDEBUG} ,info {$ENDIF};

var
  ImpTheorems,ImpSchemes: NatSet;
  SchemeIdents: MSortedStrList;
  IrrSch,IrrThe: integer;

procedure InitArticle;
 var lEdeEvl:boolean;
begin
 lEdeEvl:=false;
 if paramcount = 2 then
  begin
   if ParamStr(2) = '-a' then
    lEdeEvl:=true
  end
 else if paramcount > 2 then
  if ParamStr(3) = '-a' then
    lEdeEvl:=true;
 if lEdeEvl then
   Env.LoadEvl(EnvFileName+'.$ev')
  else
   Env.LoadEvl(EnvFileName+'.evl');
 SchemeIdents.Init(30);
 {InitInFileBuff;}
 ImpTheorems.Init(Env.Directive[syTheorems].Count,10);
 ImpTheorems.Duplicates:=false;
 ImpSchemes.Init(Env.Directive[sySchemes].Count,10);
 ImpSchemes.Duplicates:=false;
end;

procedure ProcessText;
begin
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 ReadToken;
 while (CurWord.Kind <> sy_Begin) and (CurWord.Kind <> EOT)
  do ReadToken;
 while CurWord.Kind <> EOT{'!'} do
  begin
   if CurPos.Line mod 4 = 0
    then DisplayLine(CurPos.Line,ErrorNbr);
   case CurWord.Kind of
   MMLIdentifier {'A'}:
    begin
     DisplayLine(CurPos.Line,ErrorNbr);
     ImpTheorems.InsertElem(CurWord.Nr);
    end;
   sy_From:
    begin
     ReadToken;
     if CurWord.Kind <> Identifier {'I'} then
      begin
       DisplayLine(CurPos.Line,ErrorNbr);
       ImpSchemes.InsertElem(CurWord.Nr);
      end;
    end;
   end;
   ReadToken;
  end;
 FinishScanning;
end;

procedure FinishArticle;
 var i: integer;
     lTokens: TokensCollection;
     lToken: TokenPtr;
begin
 IrrThe:=0;
 IrrSch:=0;
 lTokens.LoadDct(EnvFileName);
 for i:=0 to Env.Directive[syTheorems].Count-1 do
  with PImpArticleId(Env.Directive[syTheorems].Items^[i])^ do
  begin
    lToken:=TokenPtr(lTokens.ObjectOf(fStr));
    assert(lToken<>nil,'2382');
    if not ImpTheorems.HasInDom(lToken^.fLexem.Nr) then
     begin
      Error(PImpArticleId(Env.Directive[syTheorems].Items^[i])^.fPos,706);
      inc(IrrThe);
      Env.Directive[syTheorems].Items^[i]:=nil;
    end;
  end;
 for i:=0 to Env.Directive[sySchemes].Count-1 do
  with PImpArticleId(Env.Directive[sySchemes].Items^[i])^ do
  begin lToken:=TokenPtr(lTokens.ObjectOf(fStr));
    assert(lToken<>nil,'2382');
    if not ImpSchemes.HasInDom(lToken^.fLexem.Nr) then
     begin
      Error(PImpArticleId(Env.Directive[sySchemes].Items^[i])^.fPos,707);
      inc(IrrSch);
      Env.Directive[sySchemes].Items^[i]:=nil;
     end;
  end;
 if (IrrThe > 0) or (IrrSch > 0) then
  Env.StoreEvl(EnvFileName+'.$ev');
end;

begin
  DrawMizarScreen('Irrelevant ''theorems'' & ''schemes'' Detector');
  GetArticleName; GetEnvironName;
  GetOptions;
  InitExitProc;
  FileExam(MizFileName+ArticleExt);
  OpenErrors(MizFileName);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
  InitArticle;
  InitDisplayLine('Scanning');
  ProcessText;
  FinishArticle;
  writeln;
  if IrrThe = 1 then
    writeln('**** One irrelevant ''theorems'' directive detected.')
   else if IrrThe > 1 then
    writeln('**** ',IrrThe,' irrelevant ''theorems'' directives detected.');
  if IrrSch = 1 then
    writeln('**** One irrelevant ''schemes'' directive detected.')
   else if IrrSch > 1 then
    writeln('**** ',IrrSch,' irrelevant ''schemes'' directives detected.');
  FinishDrawing;
end.
