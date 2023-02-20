(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program AbsEdt;

uses pcmizver,mizenv,mconsole,errhan,monitor,absact,parser,mscanner
    {$IFDEF MDEBUG} ,info {$ENDIF};

begin
 DrawMizarScreen('Editing Abstract');
 InitExitProc;
 GetArticleName; GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 InitArticle;
 InitDisplayLine('Parsing '+MizFileName);
 FileExam(EnvFileName+'.dct');
 FileExam(EnvFileName+'.prf');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 Parse;
 FinishScanning;
 DrawErrorsMsg(ErrorNbr);
 FinishDrawing;
end.
