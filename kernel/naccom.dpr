(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program Accomodator;

uses librenv,pcmizver,mizenv,errhan,monitor,envhan,inlibr,acc_han,
     mconsole
{$IFDEF MDEBUG} ,info {$ENDIF}
;

var _MizExitProc:pointer;

procedure PCMizExitProc;
 var I: integer;
begin
 ExitProc:=_MizExitProc;
 {$I-}
 I:=IOResult;
 {$I+}
 CloseLogFile;
end;

begin
 DrawMizarScreen('New Accomodator');
 GetArticleName; GetEnvironName;
 FileExam(MizFileName+ArticleExt);
 DrawArticleName(MizFileName+ArticleExt);
 InitExitProc;
 _MizExitProc := ExitProc;
 ExitProc:=@PCMizExitProc;
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 OpenErrors(MizFileName);
 OpenLogFile;
 GetAccOptions;
 FileExam(MizFiles+MML+'.vct');
 LocFilesCollection.Insert(New(PFileDescr,Init(MizFiles+MML+'.vct',0)));
 DrawPass('-Parsing'); 
 Env.ReadEnvironment;
 Localization:=true;
 if ErrorNbr=0 then
  begin
   Accomodate;
   LocFilesCollection.StoreFIL(MizFileName+'.fil');
  end;
 DrawErrorsMsg(ErrorNbr);
 FinishDrawing;
end.
