(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit pcmizver;

interface

const 
   PCMizarReleaseNbr = 8;
   PCMizarVersionNbr = 1;
   PCMizarVariantNbr = 12;

   CurrentYear = 2022;

{$IFDEF WIN32}
   DirSeparator = '\';
{$ELSE}
   DirSeparator = '/';
{$ENDIF}

function  PCMizarVersionStr: string;
function  VersionStr: string;
function  PlatformNameStr: string;
function  Copyright : string;

implementation

function Copyright : string;
var s:string;
begin
 str(CurrentYear,s);
 Copyright:='Copyright (c) 1990-'+s+' Association of Mizar Users';
end;

function  VersionStr: string;
 var lRel,lVer,lVar: string[2]; lStr:string;
begin
 Str(PCMizarReleaseNbr,lRel);
 Str(PCMizarVersionNbr,lVer);
 Str(PCMizarVariantNbr,lVar);
 if length(lVar) = 1 then lVar:='0'+lVar;
{$IFDEF VERALPHA}
 lStr:='-alpha';
{$ELSE}
 lStr:='';
{$ENDIF}
 VersionStr:=lRel+'.'+lVer+'.'+lVar+lStr;
end;

function  PlatformNameStr: string;
 var lStr: string;
begin lStr:='';
{$IFDEF WIN32}
 lStr:=lStr+'Win32';
{$ENDIF}
{$IFDEF LINUX}
 lStr:=lStr+'Linux';
{$ENDIF}
{$IFDEF SOLARIS}
 lStr:=lStr+'Solaris';
{$ENDIF}
{$IFDEF FREEBSD}
 lStr:=lStr+'FreeBSD';
{$ENDIF}
{$IFDEF DARWIN}
 lStr:=lStr+'Darwin';
{$ENDIF}
{$IFDEF FPC}
 lStr:=lStr+'/FPC';
{$ENDIF}
{$IFDEF DELPHI}
 lStr:=lStr+'/Delphi';
{$ENDIF}
 PlatformNameStr:=lStr;
end;

function  PCMizarVersionStr: string;
begin
 PCMizarVersionStr:='Mizar Ver. '+VersionStr;
end;

end.







