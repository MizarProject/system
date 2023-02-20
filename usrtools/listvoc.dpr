(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32} 
{$APPTYPE CONSOLE}
{$ENDIF}
program ListVoc;

uses mobjects,pcmizver,mizenv,librenv,monitor,dicthan,mconsole;

var VocList: MStringCollection;
    PrintAllVocs: boolean;
    gVocsColl: MStringList;

procedure PrintSymbolsInVoc(fName:string);
 var lVoc:PVocabulary;
     z: integer;
begin
 fName:=UpperCase(fName);
 lVoc:=PVocabulary(gVocsColl.ObjectOf(fName));
 if lVoc <> nil then
 begin
{  writeln(fName,':',gVocsColl.GetString(i));}
  with lVoc^.Reprs do
   for z:=0 to Count-1 do
    writeln(PSymbol(Items^[z])^.SymbolStr);
 end;
end;

procedure PrintVocs(const fVocList:MStringCollection);
 procedure PrintVoc(P:PString);
  begin
   writeln('Vocabulary: ',P^); writeln;
   PrintSymbolsInVoc(P^); writeln;
  end;
 var z: integer;
begin
 with fVocList do
  for z:=0 to Count-1 do PrintVoc(PString(Items^[z]));
end;

procedure InitAllVocs;
 var i:integer;
begin
 VocList.Init(gVocsColl.fCount,0);
 for i:=0 to gVocsColl.fCount-1 do
   VocList.Insert(NewStr(gVocsColl.GetString(i)));
end;

procedure CheckParameters;
begin
 if ParamCount > 0 then Exit;
 Noise;
 DrawMizarScreen('List Vocabulary');
 writeln('Syntax:  listvoc  vocabularynames ...');
 FinishDrawing;
 halt(2);
end;

procedure InitParams;
 var P,D,N,E:String;
begin
 PrintAllVocs:=false;
 CheckParameters;
 P:=paramstr(1);
 MizFileName:=MizFiles+MML+'.vct';
 if pos('-f',P) = 1 then
  begin Delete(P,1,2);
   P:=UpperCase(P);
   D:=ExtractFileDir(P);
   if Pos('.',ExtractFileName(P)) > 0
    then N:=Copy(ExtractFileName(P),1,Pos('.',ExtractFileName(P))-1)
    else N:=ExtractFileName(P);
   E:=ExtractFileExt(P);
   if E='' then E:='.vct';
   MizFileName:=D+N+E;
   if ParamCount > 1 then GetSortedNames(2,VocList)
   else PrintAllVocs:=true;
   exit
  end
 else if pos('-a',P) = 1 then
  begin
   PrintAllVocs:=true;
   exit
  end;
 GetSortedNames(1,VocList);
end;

begin
 DrawMizarScreen('List of Vocabularies');
 InitParams;
 FileExam(MizFileName);
 InitExitProc;
 LoadMmlVcb(MizFileName,gVocsColl);
 if PrintAllVocs then InitAllVocs;
 PrintVocs(VocList);
 gVocsColl.Done;
 VocList.Done;
 FinishDrawing;
end.
