(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit mtime;

interface

procedure TimeMark(var W:longint);
function ElapsedTime(W:longint): longint;
procedure MUnpackTime(W:longint; var H,M,S,F: word);
function ReportTime(W:longint): string;

var
  gStartTime: longint;

implementation

{$IFDEF DELPHI}
uses windows;
const cmSecs = 1000;
{$ENDIF}
{$IFDEF FPC}
uses dos;
const cmSecs = 100;
type
   TSystemTime =
     record
      wHour:word;
      wMinute:word;
      wSecond:word;
      wMilliseconds:word;
     end;
procedure GetLocalTime(var aTime: TSystemTime);
begin
 with aTime do GetTime(wHour,wMinute,wSecond,wMilliseconds);
end;
{$ENDIF}

function SystemTimeToMiliSec(const fTime: TSystemTime): longint;
begin
 SystemTimeToMiliSec:=fTime.wHour*(3600*cmSecs)+
                      fTime.wMinute*longint(60*cmSecs)+
                      fTime.wSecond*cmSecs+
                      fTime.wMilliseconds;
end;

procedure TimeMark(var W:longint);
 var SystemTime: TSystemTime;
begin
 GetLocalTime(SystemTime);
 W:=SystemTimeToMiliSec(SystemTime);
end;

function ElapsedTime(W:longint): longint;
 var T : longint;
     SystemTime: TSystemTime;
begin
 GetLocalTime(SystemTime);
 T := SystemTimeToMiliSec(SystemTime)-W;
 if T < 0 then T:=86400*cmSecs+T;
 ElapsedTime:=T;
end;

procedure MUnpackTime(W:longint; var H,M,S,F: word);
begin
 H := W div (3600*cmSecs);
 M := (W-H*3600*cmSecs) div (60*cmSecs);
 S := (W-H*3600*cmSecs-M*60*cmSecs) div cmSecs;
 F := W-H*3600*cmSecs-M*60*cmSecs-S*cmSecs;
end;

function LeadingZero(w : Word) : String;
var lStr: string;
begin
  Str(w:0,lStr);
  if Length(lStr) = 1 then lStr := '0' + lStr;
  LeadingZero := lStr;
end;

function ReportTime(W:longint): string;
 var H,M,S,F: word; lTimeStr: string;
begin
 MUnpackTime(ElapsedTime(W),H,M,S,F);
 if F >= (cmSecs div 2) then inc(S);
 if H<>0 then
  begin Str(H,lTimeStr); lTimeStr:=lTimeStr+'.'+LeadingZero(M) end
 else Str(M:2,lTimeStr);
 ReportTime:=lTimeStr+':'+LeadingZero(S);
end;

begin
 TimeMark(gStartTime);
end.
