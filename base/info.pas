(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit info;

interface

uses errhan;

var InfoFile: text;

procedure InfoChar ( C: char );
procedure InfoInt ( I: integer );
procedure InfoWord ( C: char; I: integer );
procedure InfoNewLine;
procedure InfoString ( S: string );
procedure InfoPos ( Pos: Position );
procedure InfoCurPos;

procedure OpenInfoFile;
procedure CloseInfofile;

implementation

uses mizenv,mconsole;

procedure InfoChar ( C: char );
begin write(InfoFile,C) end;

procedure InfoInt ( I: integer );
begin write(InfoFile,I,' ') end;

procedure InfoWord ( C: char; I: integer );
begin write(InfoFile,C,I,' ') end;

procedure InfoNewLine;
begin writeln(InfoFile) end;

procedure InfoString ( S: string );
begin write(InfoFile,S) end;

procedure InfoPos ( Pos: Position );
begin with Pos do write(InfoFile,Line,' ',Col,' ') end;

procedure InfoCurPos;
begin with CurPos do write(InfoFile,Line,' ',Col,' ') end;

var _InfoExitProc:pointer;

procedure InfoExitProc;
begin
 CloseInfoFile;
 ExitProc:=_InfoExitProc;
end;

procedure OpenInfoFile;
begin
 assign(InfoFile,MizFileName+'.inf');
 rewrite(InfoFile);
 writeln(InfoFile,'Mizared article: "',MizFileName,'"');
 _InfoExitProc := ExitProc;
 ExitProc:=@InfoExitProc;
end;

procedure CloseInfofile;
begin close(InfoFile) end;

end.
