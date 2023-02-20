(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit mizenv;

interface

var MizFileName, EnvFileName, ArticleName, ArticleID, ArticleExt: string;

procedure SetStringLength(var aString: String; aLength: Integer);
function TrimStringLeft(aString: String): String;
function TrimStringRight(aString: String): String;
function TrimString(const aString: string): string;
function UpperCase(const aStr:string): string;
function MizLoCase(aChar: Char): Char;
function LowerCase(const aStr: string): string;
function IntToStr(aInt: integer): string;

function MFileExists(const aFileName : String) : Boolean;
procedure EraseFile(const aFileName:string);
procedure RenameFile(const aName1,aName2:string);
function GetFileTime(aFileName: String): Longint;
procedure SplitFileName(const aFileName: String; var aDir, aName, aExt: String);
function TruncDir(const aFileName: String): String;
function TruncExt(const aFileName: String): String;
function ExtractFileDir(const aFileName: String): String;
function ExtractFileName(const aFileName: String): String;
function ExtractFileExt(const aFileName: String): String;
function ChangeFileExt(const aFileName,aFileExt: string): string;

function GetEnvStr(aEnvName: string): string;

procedure FileExam(const aFileName: string);
procedure EnvFileExam(const aFileExt:string);

procedure GetFileName(ParamNr:byte; DefaultExt:string; var aFileName:string);
procedure GetFileExtName(Nr:byte; DefaultExt:string;
                         var aFileName:string; var aFileExt:string);
procedure GetMizFileName(aFileExt:string);
procedure GetArticleName;
procedure GetEnvironName;

function IsMMLIdentifier(const aID: string): boolean;

implementation

uses
{$IFDEF DELPHI}
  IOUtils,sysutils,windows,
{$ENDIF}
{$IFDEF FPC}
  dos,sysutils,
{$ENDIF}
  mconsole;

procedure SetStringLength(var aString: String; aLength: Integer);
var I, L: Integer;
begin
  L := Length(aString);
  if aLength <= L then
    Delete(aString, aLength + 1, L - aLength)
  else
    for I := 1 to aLength - L do aString := aString + ' ';
end;

function TrimStringLeft(aString: String): String;
begin
  while (Length(aString) > 0) and (aString[1] = ' ') do Delete(aString, 1, 1);
  TrimStringLeft := aString;
end;

function TrimStringRight(aString: String): String;
begin
  while (Length(aString) > 0) and (aString[Length(aString)] = ' ') do
    Delete(aString, Length(aString), 1);
  TrimStringRight := aString;
end;

function TrimString(const aString: String): String;
begin
  TrimString := TrimStringRight(TrimStringLeft(aString));
end;

function UpperCase(const aStr:string): string;
 var k:integer; lStr: string;
begin
 lStr:=aStr;
 for k:=1 to length(aStr) do lStr[k]:=UpCase(aStr[k]);
 UpperCase:=lStr;
end;

function MizLoCase(aChar: Char): Char;
begin
 if aChar in ['A'..'Z'] then
   MizLoCase := Chr(Ord('a') + Ord(aChar) - Ord('A'))
 else
   MizLoCase := aChar;
end;

function LowerCase(const aStr: string): string;
 var i: integer;
     lStr: string;
begin
  lStr:=aStr;
  for i:=1 to Length(aStr) do lStr[i]:=MizLoCase(aStr[i]);
  LowerCase:=lStr;
end;

function IntToStr(aInt: integer): string;
 var lStr: string;
begin
 Str(aInt,lStr);
 IntToStr:=lStr;
end;

function MFileExists(const aFileName : String) : Boolean;
begin
 MFileExists:=FileExists(aFileName);
end;

procedure EraseFile(const aFileName:string);
begin
 SysUtils.DeleteFile(aFileName);
end;

procedure RenameFile(const aName1,aName2:string);
begin
 if MFileExists(aName1) then
   EraseFile(aName2);
 SysUtils.RenameFile(aName1,aName2);
end;

function GetFileTime(aFileName: String): Longint;
begin
 GetFileTime := FileAge(aFileName);
end;

procedure SplitFileName(const aFileName: String; var aDir, aName, aExt: String);
begin
{$IFDEF FPC}
 aDir := SysUtils.ExtractFilePath(aFileName);
 aName := SysUtils.ExtractFileName(aFileName);
 aExt := SysUtils.ExtractFileExt(aFileName);
{$ENDIF}
{$IFDEF DELPHI}
 aDir := TPath.GetDirectoryName(aFileName);
 aName := TPath.GetFileName(aFileName);
 aExt := TPath.GetExtension(aFileName);
{$ENDIF}
end;

function TruncDir(const aFileName: String): String;
var
  Dir, lName, Ext: String;
begin
  SplitFileName(aFileName, Dir, lName, Ext);
  TruncDir := lName + Ext;
end;

function TruncExt(const aFileName: String): String;
var
  Dir, lName, Ext: String;
begin
  SplitFileName(aFileName, Dir, lName, Ext);
  TruncExt := Dir + lName;
end;

function ExtractFileDir(const aFileName: String): String;
 var
   Dir, lName, Ext: String;
begin
  SplitFileName(aFileName, Dir, lName, Ext);
  ExtractFileDir := Dir;
end;

function ExtractFileName(const aFileName: String): String;
var
  Dir, lName, Ext: String;
begin
  SplitFileName(aFileName, Dir, lName, Ext);
  ExtractFileName := lName;
end;

function ExtractFileExt(const aFileName: String): String;
var
  Dir, lName, Ext: String;
begin
  SplitFileName(aFileName, Dir, lName, Ext);
  ExtractFileExt := Ext;
end;

function ChangeFileExt(const aFileName,aFileExt: string): string;
begin
 ChangeFileExt:=SysUtils.ChangeFileExt(aFileName,aFileExt);
end;

function GetEnvStr(aEnvName: string): string;
{$IFDEF FPC}
begin
 GetEnvStr:=GetEnv(aEnvname);
end;
{$ENDIF}
{$IFDEF DELPHI}
 const cchBuffer=255;
 var lName,lpszTempPath: array[0..cchBuffer] of char;
     i: integer;
     lStr: string;
begin
 lStr:='';
 for i:=1 to length(aEnvname) do
  lName[i-1]:=aEnvname[i];
 lName[length(aEnvname)]:=#0;

 if GetEnvironmentVariable(lName,lpszTempPath,cchBuffer) > 0 then
  begin
   for i:=0 to cchBuffer do
    begin
     if lpszTempPath[i]=#0 then break;
     lStr:=lStr+lpszTempPath[i];
    end;
  end;
 GetEnvStr:=lStr;
end;
{ restored for DELPHI4 compatibility ;-(
begin
 GetEnvStr:=GetEnvironmentVariable(aEnvname);
end;}
{$ENDIF}

procedure FileExam(const aFileName: string);
 var Source: file; I: byte;
begin
  if aFileName = '' then
    begin DrawMessage('Can''t open '' .miz ''','');
     halt(1);
    end;
  FileMode:=0;
  assign(Source,aFileName); {$I-} reset(Source); {$I+}
  I:=ioresult;
  if I<>0 then DrawIOREsult(aFileName,I);
  close(Source);
  FileMode:=2;
end;

procedure EnvFileExam(const aFileExt:string);
begin
  if not MFileExists(EnvFileName+aFileExt) then
   begin
     DrawMessage('Can''t open '' '+EnvFileName+aFileExt+' ''','');
     Halt(1);
   end;
end;

procedure GetFileName(ParamNr:byte; DefaultExt:string; var aFileName:string);
 var  lFileExt: string;
begin
 if ParamNr <= paramcount then
  begin aFileName:=paramstr(ParamNr);
    lFileExt:=ExtractFileExt(aFileName);
    if lFileExt='' then aFileName:=ChangeFileExt(aFileName,DefaultExt);
    exit
  end;
 aFileName:='';
end;

procedure GetFileExtName(Nr:byte; DefaultExt:string;
                      var aFileName:string; var aFileExt:string);
begin
 if Nr <= paramcount then
  begin aFileName:=paramstr(Nr);
    aFileExt:=ExtractFileExt(aFileName);
    if aFileExt='' then aFileExt:=DefaultExt
    else aFileName:=ChangeFileExt(aFileName,'');
    exit
  end;
 aFileName:=''; aFileExt:='';
end;

procedure GetMizFileName(aFileExt:string);
var i:Integer;
begin
 MizFileName:=''; ArticleName:=''; ArticleExt:=''; EnvFileName:='';
 for i:=1 to ParamCount do
  if ParamStr(i)[1]<>'-' then
   begin
    MizFileName:=ParamStr(i);
    GetFileExtName(i,aFileExt,MizFileName,ArticleExt);
    ArticleName:=ExtractFileName(MizFileName);
    ArticleID:=UpperCase(ArticleName);
    if not IsMMLIdentifier(ArticleName) then
     begin
      DrawMessage('Only letters, numbers and _ allowed in Mizar file names','');
      halt(1);
     end;
    EnvFileName:=MizFileName;
    exit;
   end;
end;

procedure GetArticleName;
begin
 GetMizFileName('.miz');
end;

procedure GetEnvironName;
var i,c:Integer;
begin
 if MizFileName = '' then GetArticleName;
 EnvFileName:=MizFileName;
 c:=0;
 for i:=1 to ParamCount do
  if (ParamStr(i)[1]<>'-') then
   begin
    inc(c);
    if c=2 then EnvFileName:=ParamStr(i);
   end;
end;

function IsMMLIdentifier(const aID: string): boolean;
 const Allowed: array[chr(0)..chr(255)] of byte =
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
 var i: integer;
begin
  for i:=1 to length(aID) do
   if Allowed[aID[i]] = 0 then
    begin
     IsMMLIdentifier:=false;
     exit;
    end;
  IsMMLIdentifier:=true;
end;

end.
