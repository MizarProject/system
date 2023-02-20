(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program Transfer;

uses pcmizver,mizenv,mconsole,monitor,prelhan
     {$IFDEF MDEBUG} ,info {$ENDIF};

{------------------------------------------------------}

begin
  DrawMizarScreen('Transferer');
  if ParamCount=0 then
   begin
    Noise;
    Writeln('Syntax:  transfer  articlename');
    FinishDrawing;
    halt;
   end;
  GetMizFileName('');
  InitExitProc;
  {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
  GetTransfOptions;
  PRELTransfer;
  FinishDrawing;
end.

