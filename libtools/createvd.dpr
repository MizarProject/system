(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program CreateEvd;

uses pcmizver,mizenv,mconsole,monitor,envhan,mobjects,mscanner,info;

var Target: text;
    TargetCol: integer;

procedure WriteString(aStr: string; fTail: integer);
 begin
  aStr := ' ' + aStr;
  if (TargetCol + Length(aStr) + fTail) <= 79 then
   begin
    write(Target,aStr);
    inc(TargetCol,Length(aStr));
   end
  else
   begin
    writeln(Target);
    write(Target,' ':5,aStr);
    TargetCol:=Length(aStr) + 5;
   end;
 end;

procedure PrintDirectives(const DirName:string; const Directive: MSortedStrList);
 var i,FirstDir: integer;
begin
 if Directive.Count = 0 then exit;
 FirstDir:=0;
 if PImpArticleId(Directive.Items^[0])^.fStr='HIDDEN' then
  begin FirstDir:=1; if Directive.Count = 1 then exit end;
 write(Target,' ',DirName); TargetCol:=Length(DirName)+1;
 for i:=FirstDir to Directive.Count-2 do
  begin
   WriteString(PImpArticleId(Directive.Items^[i])^.fStr,1);
   write(Target,','); inc(TargetCol);
  end;
 WriteString(PImpArticleId(Directive.Items^[Directive.Count-1])^.fStr,1);
 writeln(Target,';');
end;

procedure CopyComments;
 var CMM: text; CommBuf: array[1..$1000] of char;
     Line: string;
begin
 if MFileExists(MizFileName+'.cmm') then
  begin
   assign(CMM,MizFileName+'.cmm'); settextbuf(CMM,CommBuf);
   reset(CMM);
   while not eof(CMM) do
    begin
     readln(CMM,Line); writeln(Target,Line);
    end;
   writeln(Target);
   close(CMM);
  end;
end;

var TargetBuf: array[1..$1000] of char;

begin
 DrawMizarScreen('Creating Environment Description');
 InitExitProc;
 GetMizFileName('.evl'); GetEnvironName;
{$IFDEF MDEBUG}
  OpenInfoFile;
{$ENDIF}
 DrawArticleName(MizFileName);
 assign(Target,MizFileName+'.evd'); settextbuf(Target,TargetBuf);
 rewrite(Target);
 CopyComments;
 writeln(Target,'environ'); writeln(Target);
 Env.LoadEvl(EnvFileName+ArticleExt);
 PrintDirectives('vocabularies',Env.Directive[syVocabularies]);
 PrintDirectives('notations',Env.Directive[syNotations]);
 PrintDirectives('constructors',Env.Directive[syConstructors]);
 PrintDirectives('registrations',Env.Directive[syRegistrations]);
 PrintDirectives('requirements',Env.Directive[syRequirements]);
 PrintDirectives('definitions',Env.Directive[syDefinitions]);
 PrintDirectives('equalities',Env.Directive[syEqualities]);
 PrintDirectives('expansions',Env.Directive[syExpansions]);
 PrintDirectives('theorems',Env.Directive[syTheorems]);
 PrintDirectives('schemes',Env.Directive[sySchemes]);
 writeln(Target);
 close(Target);
 FinishDrawing;
end.
