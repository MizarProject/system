(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program MizarVerifier;

uses errhan,verfinit
{$IFNDEF PDEBUG}{$IFNDEF ADEBUG},mizprep{$ENDIF} {$ENDIF};

begin
 MSMVerifyArticle('Verifier based on More Strict Mizar Processor', VerifierError
 {$IFNDEF PDEBUG} {$IFNDEF ADEBUG},ClassicalChecker{$ENDIF} {$ENDIF}
 );
end.

