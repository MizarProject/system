(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program CheckingVocabulary;

uses mobjects,pcmizver,mizenv,inout,librenv,monitor,mscanner,scanner,accdict,
     envhan,dicthan,mconsole;

var gPrivVoc: PVocabulary;
    gVocsColl: MStringList;
    gTokens: TokensCollection;

function FindSymbol(fRepr:string):PSymbol;
 function EqSymbol(Item:PSymbol): boolean;
 begin
  EqSymbol:=Item^.Repr = fRepr;
 end;
  var z: integer;
begin
 FindSymbol := nil;
 with gPrivVoc^.Reprs do
 for z:=0 to Count-1 do
  if EqSymbol(Items^[z]) then
   begin FindSymbol :=Items^[z]; exit end;
end;

procedure CheckSymbolsIn(fName:NameStr; fVoc:PVocabulary);
  var lPrintMsg: boolean;
  procedure PrintToken(fSymb: PSymbol);
    var lSymb:PSymbol;
  begin
   lPrintMsg:=false;
   lSymb:=FindSymbol(fSymb^.Repr);
   if lSymb <> nil then
    begin lPrintMsg:=true;
     writeln('Equal symbols:');
     writeln(fName,':',' ':9-length(fName),fSymb^.SymbolStr);
     writeln(ArticleName,':',' ':9-length(ArticleName),lSymb^.SymbolStr);
    end;
  end;
 var z:integer;
begin
 with fVoc^.Reprs do
  for z:=0 to Count-1 do PrintToken(PSymbol(Items^[z]));
 if lPrintMsg then writeln;
end;

procedure CheckWithMMLVocs;
 var i:integer;
begin
 with gVocsColl do
 for i:=0 to fCount-1 do
  begin
   CheckSymbolsIn(GetString(i),PVocabulary(GetObject(i)));
  end;
end;

procedure CheckSymbols;
 procedure ChkSymbol(fSymb:PSymbol);
  var i: integer;
 begin
  with fSymb^ do
   begin
    if (Capital[Kind]=0) or not (Kind in AvailableSymbols) then
      begin
       writeln('wrong symbol kind: ',fSymb^.SymbolStr);
      end;
    if pos('::',Repr) <> 0 then
      begin
       writeln('comment "::" in symbol: ',fSymb^.SymbolStr);
      end;
    if pos(' ',Repr) <> 0 then
      begin
       writeln('space in symbol: ',fSymb^.SymbolStr);
      end;
    if (length(Repr) = 1) and (UpCase(Repr[1]) in ['A'..'Z']) then
      begin
       writeln('one letter symbol: ',fSymb^.SymbolStr);
      end;
    for i:=1 to length(Repr) do
     if Repr[i] in [#0..#31,#127..#255] then
      begin
       writeln('wrong character in symbol: ',fSymb^.SymbolStr);
      end;
   end;
 end;
 var lDict: PVocabulary;
     z: integer;
begin
 FileExam(MizFiles+'mizar.dct');
 gTokens.LoadDct(MizFiles+'mizar');
 lDict:=new(PVocabulary,Init);
 for z:=0 to gTokens.Count-1 do
   lDict^.Reprs.Insert(new(PSymbol, Init('A',(TokenPtr(gTokens.Items^[z])^.fStr),'',0)));
 CheckSymbolsIn('mizar',lDict);
 Dispose(lDict,Done);
 gTokens.Done;
 with gPrivVoc^.Reprs do
  for z:=0 to Count-1 do ChkSymbol(PSymbol(Items^[z]));
end;

procedure BadParameters;
begin
 if ParamCount > 0 then Exit;
 Noise;
 writeln('Syntax:  checkvoc  vocabularyname');
 FinishDrawing;
 halt(2);
end;

 var gVocsFileName: string;

procedure InitParams;
begin
 BadParameters;
 GetMizFileName('.voc');
 if ParamCount >= 2 then GetFileName(2,'.vct',gVocsFileName)
  else gVocsFileName:=MizFiles+MML+'.vct';
end;

begin
 DrawMizarScreen('Checking Vocabulary');
 InitParams;
 if not MFileExists(MizFileName) then
  if MFileExists(MizFileName+'.voc')
   then MizFileName:=MizFileName+'.voc'
  else if MFileExists('dict'+DirSeparator+MizFileName+'.voc')
   then MizFileName:='dict'+DirSeparator+MizFileName+'.voc'
  else if MFileExists('dict'+DirSeparator+MizFileName)
   then MizFileName:='dict'+DirSeparator+MizFileName;
 gPrivVoc:=GetPrivateVoc(MizFileName);
 if gPrivVoc = nil then
  begin
   DrawMessage('Can''t open '' '+ChangeFileExt(MizFileName,'.voc'),'');
   exit;
  end;
 LoadMmlVcb(gVocsFileName,gVocsColl);
 CheckWithMMLVocs;
 CheckSymbols;
 gVocsColl.Done;
 FinishDrawing;
end.

