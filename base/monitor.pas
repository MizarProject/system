(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit monitor;

interface

procedure InitExitProc;

implementation

uses
{$IFDEF FPC}
  {$IFNDEF WIN32}
   baseunix,
  {$ENDIF}
{$ENDIF}
mizenv,errhan,mconsole
{$IFDEF WIN32} ,windows {$ENDIF}
{$IFDEF MDEBUG} ,info {$ENDIF};

var
   _ExitProc: pointer;
   _IOResult:integer;

procedure _Halt_(ErrorCode: word);
begin
  _IOResult:=IOResult;
   ErrorAddr:=nil;
  if ErrorCode>1 then
   case ErrorCode of
    2..4:     begin ErrImm(1000+ErrorCode);
                DrawMessage('I/O error',ErrMsg[ErrorCode])
              end;
    5..6:     begin ErrImm(1000+ErrorCode); BugInProcessor end;
    12:       begin ErrImm(1000+ErrorCode); BugInProcessor end;
    97,98,99:
     begin ErrImm(RTErrorCode);
      case RTErrorCode of
       800,804:  DrawMessage('Library Corrupted','');
       857:      DrawMessage('Connection Fault','');
//       900..999: DrawMessage('Mizar parameter overflow: '+IntToStr(RTErrorCode),'');
       1255:     DrawMessage('User break','');
       else
       if OverflowErroor then
        DrawMessage('Mizar parameter overflow: '+IntToStr(RTErrorCode),'')
       else BugInProcessor
      end;
     end;
    100..101: begin ErrImm(1000+ErrorCode);
                DrawMessage('I/O error',ErrMsg[ErrorCode-95]);
              end;
    102..106: begin ErrImm(1000+ErrorCode); BugInProcessor end;
    150..162: begin ErrImm(1000+ErrorCode);
                DrawMessage('I/O error','Critical disk error');
              end;
    200..201: begin ErrImm(1000+ErrorCode); BugInProcessor end;
    202:      begin ErrImm(1000+ErrorCode); DrawMessage('Stack overflow error','') end;
    203,204:  begin ErrImm(1000+ErrorCode); DrawMessage('Heap overflow error','') end;
    208:      begin ErrImm(1000+ErrorCode); DrawMessage('Overlay manager not installed','') end;
    209:      begin ErrImm(1000+ErrorCode); DrawMessage('Overlay file read error','') end;
    210..212: begin ErrImm(1000+ErrorCode); BugInProcessor end;
    213:      begin ErrImm(1000+ErrorCode); DrawMessage('Collection Index out of range','') end;
    214:      begin ErrImm(1000+ErrorCode); DrawMessage('Collection overflow error','') end;
    215:      begin ErrImm(1000+ErrorCode); DrawMessage('Arithmetic overflow error','') end;
    216:      begin ErrImm(1000+ErrorCode); DrawMessage('General Protection fault','') end;
    217:      begin ErrImm(1000+ErrorCode); DrawMessage('Segmentation fault','') end;
    218..254: begin ErrImm(1000+ErrorCode); BugInProcessor end;
    255:      ErrImm(1000+ErrorCode);
    else
     begin ErrImm(ErrorCode);
      if OverflowErroor then
       DrawMessage('Mizar parameter overflow error','')
      else BugInProcessor
     end;
   end;
  CloseErrors;
  ExitProc:=_ExitProc;
  if (ErrorCode = 0) and (ErrorNbr <> 0) then Halt(1) else Halt(ErrorCode);
end;

procedure MizExitProc;
begin
{$IFDEF IODEBUG}
  ExitProc:=_ExitProc;
{$ELSE}
  _Halt_(ExitCode);
{$ENDIF}
end;

procedure InitExitProc;
begin ExitProc := @MizExitProc end;


{$IFDEF FPC}
 {$IFNDEF WIN32}
procedure CatchSignal(aSig : Integer);cdecl;
begin
  case aSig of
    SIGINT,SIGQUIT,SIGTERM:      
     begin
      CtrlCPressed:=true;
      RunTimeError(1255);
     end;
  end;
end;

var NewSignal, OldSigInt : SignalHandler;

procedure InitCtrl;
begin
  NewSignal:=SignalHandler(@CatchSignal);
  OldSigInt:=fpSignal(SIGINT,NewSignal);
  OldSigInt:=fpSignal(SIGQUIT,NewSignal);
  OldSigInt:=fpSignal(SIGTERM,NewSignal);
end;
 {$ENDIF}
{$ENDIF}

{$IFDEF WIN32}
 {$IFDEF FPC}
function CtrlSignal(aSignal: DWORD): WINBOOL ;stdcall;
 {$ENDIF}
 {$IFDEF DELPHI}
function CtrlSignal(aSignal: DWORD): BOOL; cdecl;
 {$ENDIF}
begin
  { TRUE: do not call next handler in the queue, FALSE: call it }
  {case aSignal of
    CTRL_C_EVENT: Writeln('Ctrl+C');
    CTRL_BREAK_EVENT: Writeln('Ctrl+Break');
    CTRL_CLOSE_EVENT: Writeln('Close');
    CTRL_LOGOFF_EVENT: Writeln('Logoff');
    CTRL_SHUTDOWN_EVENT: Writeln('Shutdown');
  else
    Writeln('Unexpected signal');
  end;}
  CtrlCPressed:=true;
  RunTimeError(1255);
  CtrlSignal := true;
  {ExitProcess(1);}
end;
 {$IFDEF FPC}
procedure InitCtrl;
begin
 SetConsoleCtrlHandler(CtrlSignal, TRUE);
end;
 {$ENDIF}
 {$IFDEF DELPHI}
procedure InitCtrl;
var
  ConsoleMode,lConsoleMode: DWORD;
begin
  if GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), ConsoleMode) then
  begin
    lConsoleMode := ConsoleMode or ENABLE_PROCESSED_INPUT;
     { Treat Ctrl+C as a signal }
    if SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), lConsoleMode) then
    begin
      SetConsoleCtrlHandler(@CtrlSignal, TRUE);
    end;
  end;
end;
 {$ENDIF}
{$ENDIF}

begin
  _ExitProc := ExitProc;
   InitCtrl;
end.
