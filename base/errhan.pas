(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit errhan;

interface

type Position = record Line,Col: integer end;

     ErrorReport = procedure(Pos:Position; ErrNr:integer);

const ZeroPos : Position = (Line:0; Col:0);

var  CurPos:    Position;
     ErrorNbr:  integer;

     PutError: ErrorReport = nil;

     RTErrorCode: integer = 0;
     OverflowErroor: boolean = false;

procedure Error(Pos:Position; ErrNr:integer);
procedure ErrImm(ErrNr:integer);

procedure WriteError(Pos:Position; ErrNr:integer);
procedure OpenErrors(FileName:string);
procedure AppendErrors(FileName:string);
procedure EraseErrors;
procedure CloseErrors;

procedure OverflowError(ErrorCode:word);
procedure Mizassert (ErrorCode:word; Cond:boolean);
procedure RunTimeError(ErrorCode:word);

implementation

uses mconsole,mizenv;

procedure Error(Pos:Position; ErrNr:integer);
begin inc(ErrorNbr);
 if @PutError <> nil then PutError(Pos,ErrNr);
 if StopOnError then 
  begin 
   DrawMessage('Stopped on first error',''); 
   Halt(1); 
  end;
end;

procedure ErrImm(ErrNr:integer);
begin
 Error(CurPos,ErrNr);
end;

var
  Errors: text;
  OpenedErrors: boolean = false;

procedure WriteError(Pos:Position; ErrNr:integer);
begin
 if not OpenedErrors then RunTimeError(2001);
 with Pos do writeln(Errors,Line,' ',Col,' ',ErrNr);
end;

procedure OpenErrors(FileName:string);
begin
 if ExtractFileExt(FileName)='' then FileName:=FileName+'.err';
 assign(Errors,FileName);
{$I-}
 rewrite(Errors);
{$I+}
 if IOResult <> 0 then
  begin
   DrawMessage('Can''t open errors file '''+FileName+''' for writing','');
   halt(1);
  end;
 OpenedErrors:=true;
 ErrorNbr:=0;  with CurPos do begin Line:=1; Col:=1 end;
 if @PutError = nil then PutError:=WriteError;
end;

procedure AppendErrors(FileName:string);
begin
 OpenedErrors:=true;
 if ExtractFileExt(FileName)=''  then FileName:=FileName+'.err';
 assign(Errors,FileName);
 ErrorNbr:=0;
 with CurPos do begin Line:=1; Col:=1 end;
 {$I-} append(Errors); {$I+}
 if ioresult<>0 then rewrite(Errors);
end;

procedure EraseErrors;
begin
 if OpenedErrors then
  begin
   OpenedErrors:=false;
   close(Errors); erase(Errors);
  end;
end;

procedure CloseErrors;
begin
 if OpenedErrors then
  begin
   OpenedErrors:=false;
   close(Errors);
  end;
end;

procedure OverflowError(ErrorCode:word);
begin
 RTErrorCode:=ErrorCode;
 OverflowErroor:=true;
 RunError(97);
end;

procedure Mizassert( ErrorCode:word; Cond: boolean );
begin
 if not Cond then
  begin
   RTErrorCode:=ErrorCode;
   RunError(98);
  end;
end;

procedure RunTimeError(ErrorCode:word);
begin
 RTErrorCode:=ErrorCode;
 RunError(99);
end;

end.
