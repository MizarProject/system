(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit mstate;

interface

procedure InitPass(const aPassName: string);
procedure FinishPass;
procedure InitProcessing(const aProgName,aExt: string);
procedure ProcessingEnding;

implementation

uses mizenv, pcmizver, monitor, errhan, mconsole, mtime
{$IFDEF MDEBUG} ,info {$ENDIF};

var PassTime: longint;

procedure InitPass(const aPassName: string);
begin
 CurPos.Line:=1;
 CurPos.Col:=1;
 InitDisplayLine(aPassName);
 TimeMark(PassTime);
end;

procedure FinishPass;
begin
 FinishingPass:=true;
 if QuietMode then DisplayLine(CurPos.Line,ErrorNbr);
 FinishingPass:=false;
 DrawTime('  '+ReportTime(PassTime));
end;

procedure MError(Pos:Position; ErrNr:integer);
begin
 WriteError(Pos,ErrNr);
 DisplayLine(CurPos.Line,ErrorNbr);
end;

var _ExitProc: pointer;
procedure MizarExitProc;
begin
 ExitProc:=_ExitProc;
 {$I-}
 if IOResult<>0 then;
 if not StopOnError then DisplayLine(CurPos.Line,ErrorNbr);
 PutError:=WriteError;
 DrawVerifierExit(ReportTime(gStartTime));
{ Halt(ErrorCode);}
 {$I+}
end;

procedure InitProcessing(const aProgName,aExt: string);
begin
  DrawMizarScreen(aProgName);
  if paramcount<1 then EmptyParameterList;
  GetArticleName;
  GetEnvironName;
  DrawArticleName(MizFileName+aExt);
  GetOptions;
  InitExitProc;
  FileExam(MizFileName+aExt);
  _ExitProc := ExitProc;
  ExitProc:=@MizarExitProc;
  PutError:=MError;
  OpenErrors(MizFileName);
{$IFDEF MDEBUG}
  OpenInfoFile;
{$ENDIF}
end;

procedure ProcessingEnding;
begin
 if ErrorNbr > 0 then
  begin
   DrawErrorsMsg(ErrorNbr);
   FinishDrawing;
  end;
end;

end.

