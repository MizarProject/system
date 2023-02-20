(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program FindVocabulary;

uses mobjects,pcmizver,mizenv,librenv,monitor,dicthan,mconsole;

var gPattern: string;
    gSymbolKind: set of 'A'..'Z';
    gUpperCase,gWholeWord,gSpecSymbols: boolean;
    gVocsColl: MStringList;

procedure PrintSymbolsInVoc(fName:string);
  var lFirstToken: boolean;
  procedure PrintToken(P: PSymbol);
    var lRepr: string;
   begin
    with P^ do
     begin
      if not (Kind in gSymbolKind) then exit;
      lRepr:=Repr;
      if gUpperCase then lRepr:=UpperCase(lRepr);
      if gWholeWord then
       begin if lRepr <> gPattern then exit end
      else if Pos(gPattern,lRepr) = 0 then exit;
      if lFirstToken then
       begin lFirstToken:=false;
         writeln('vocabulary: ',fName);
       end;
      write(Kind,Repr,' ');
      if (Kind='O') and (Prior<>StandardPriority) then write(' ',Prior);
      writeln;
     end;
   end;
 var lVoc:PVocabulary;
     z: integer;
begin
 fName:=UpperCase(fName);
 lFirstToken:=true;
 lVoc:=PVocabulary(gVocsColl.ObjectOf(fName));
 if lVoc <> nil then
  begin
   with lVoc^.Reprs do for z:=0 to Count-1 do PrintToken(PSymbol(At(z)));
   if not lFirstToken then writeln;
  end;
end;

procedure PrintAllVocs;
 var i:integer;
begin
 for i:=0 to gVocsColl.fCount-1 do
  PrintSymbolsInVoc(gVocsColl.GetString(i));
end;

procedure BadParameters;
begin
 if ParamCount > 0 then Exit;
 Noise;
 DrawMizarScreen('Find Vocabulary');
 writeln('Syntax:  findvoc [-iswGKLMORUV] searchstring');
 writeln;
 writeln('Options are one or more option characters preceeded by "-"');
 writeln('   -i  Ignore case');
 writeln('   -w  Word search');
 writeln('   -s  Ignore special symbols');
 writeln('   -G  structure symbol');
 writeln('   -K  left bracket symbol');
 writeln('   -L  right bracket symbol');
 writeln('   -M  mode symbol');
 writeln('   -O  functor symbol');
 writeln('   -R  predicate symbol');
 writeln('   -U  selector symbol');
 writeln('   -V  attribute symbol');
 writeln('The following symbols are treated specially:');
{ writeln('   ^  start of symbol');
 writeln('   $  end of symbol');}
 writeln('   \b  |');
 writeln('   \l  <');
 writeln('   \g  >');
 writeln('   \  quote next character:');
 writeln;
 halt(2);
end;

procedure InitParams;
 var lNr,i:byte; c: char; lPattern: string;
begin
 BadParameters;
 gUpperCase:=false;
 gWholeWord:=false;
 gSpecSymbols:=true;
 lPattern:=ParamStr(1);
 gSymbolKind:=[];
 lNr:=2;
 if lPattern[1]='-' then
  begin
   if pos('i',lPattern) <> 0 then gUpperCase:=true;
   if pos('w',lPattern) <> 0 then gWholeWord:=true;
   if pos('s',lPattern) <> 0 then gSpecSymbols:=false;
   for c:='A' to 'Z' do
    if c in AvailableSymbols then
     if pos(c,lPattern) <> 0
      then gSymbolKind:=gSymbolKind+[c];
   if ParamCount < 2 then BadParameters;
   lPattern:=ParamStr(2);
   lNr:=3;
  end;
 if gSymbolKind=[] then gSymbolKind:=AvailableSymbols;
 if not gSpecSymbols then gPattern:=lPattern
 else
  begin i:=1; gPattern:='';
   while i<=length(lPattern) do
    begin
     if lPattern[i] = '\' then
      begin inc(i);
       if i<=length(lPattern) then
        begin
         case lPattern[i] of
          'b': gPattern:=gPattern+'|';
          'g': gPattern:=gPattern+'>';
          'l': gPattern:=gPattern+'<';
          else gPattern:=gPattern+lPattern[i];
         end;
        end;
      end
     else gPattern:=gPattern+lPattern[i];
     inc(i);
    end;
  end;
 if gUpperCase then gPattern:=UpperCase(gPattern);
 if ParamCount >= lNr then GetFileName(lNr,'.vct',MizFileName)
  else MizFileName:=MizFiles+MML+'.vct';
end;

begin
 InitParams;
 DrawMizarScreen('FindVoc');
 InitExitProc;
 LoadMmlVcb(MizFileName,gVocsColl);
 PrintAllVocs;
 gVocsColl.Done;
 FinishDrawing;
end.
