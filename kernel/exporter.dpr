(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program Exporter;

uses extunit
     {$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

begin
    MSMExportArticle;
end.
