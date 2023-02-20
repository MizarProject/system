(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program CheckIteratives;

uses verfinit,prepobj,mizprep,itr_prep;

begin
 gFullChecking	:= true;
 ReviewArticle('Irrelevant Iterative Steps Detector',IterEqChecker,
               new(IterEqPBlockPtr,Init(blMain)));
end.
