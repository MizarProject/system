(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program EnvGet;

uses pcmizver, librenv, mizenv, errhan, monitor, mconsole,
  envhan,  inlibr 
  {$IFDEF MDEBUG},info {$ENDIF};

var _MizExitProc:pointer;

procedure PCMizExitProc;
begin
 ExitProc:=_MizExitProc;
 {$I-}
 if IOResult<>0 then;
// CloseLogFile;
 if IOResult<>0 then;
 {$I+}
end;

begin
 DrawMizarScreen('Environment Dumper');
 GetArticleName; GetEnvironName;
 FileExam(MizFileName+ArticleExt);
 DrawArticleName(MizFileName+ArticleExt);
 InitExitProc;
 _MizExitProc := ExitProc;
 ExitProc:=@PCMizExitProc;
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 OpenErrors(MizFileName);
// OpenLogFile;
// GetAccOptions;
 FileExam(MizFiles+MML+'.vct');
 LocFilesCollection.Insert(New(PFileDescr,Init(MizFiles+MML+'.vct',0)));
// DrawPass('-Parsing');
 Env.ReadEnvironment;
 Env.StoreEvl(EnvFileName+'.evl');
 CloseErrors;
(* if ErrorNbr=0 then
  begin
   Accomodate;
   LocFilesCollection.StoreFIL(MizFileName+'.fil');
  end; *)
 DrawErrorsMsg(ErrorNbr);
FinishDrawing;
end.






