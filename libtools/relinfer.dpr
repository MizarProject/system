(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program RelInfer;

uses verfinit,prepobj,mizprep,inf_prep;

begin
 ReviewArticle('Irrelevant Inferences Detector',nil,
               new(InfPBlockPtr,Init(blMain)));
end.
