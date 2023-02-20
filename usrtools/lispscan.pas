(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)
// Scanner modifications for Emacs Lisp output of formula skeletons

unit lispscan;

interface

uses mobjects,limits,syntax,mscanner,scanner,inout;

procedure LispReadToken;

var PrintLevel:integer = 0;  	// could be boolean now
var SkipFirst:boolean = false; 	// skip the printing
var gPrintedStr: string = '';	// all is printed to this string
implementation

uses errhan;

// Protect Lisp characters
procedure PrintRepr(fString: string);
 var i: integer;
begin
 for i:=1 to length(fString) do
 begin
  if fString[i] in [ '''','\','"','?'] then
   gPrintedStr:= gPrintedStr + '\';
  gPrintedStr:= gPrintedStr + fString[i];
 end;
end;

// ReadToken procedure changed to do Emacs Lisp output
// of tokens when the parser tells so
procedure LispReadToken;
var i:integer;
begin
 ReadToken;
 if PrintLevel > 0 then
 if SkipFirst then SkipFirst:=false
 else
 begin
   PrintRepr(PrevWord.Spelling);
   if PrevPos.Line <> CurPos.Line then
    gPrintedStr:= gPrintedStr + ' '
   else
   for i:=1 to CurPos.Col -
        (PrevPos.Col + length(CurWord.Spelling)) do
    gPrintedStr:= gPrintedStr + ' ';
 end;  
end; { LispReadToken }

end.

