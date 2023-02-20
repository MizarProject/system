(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program ChkLab;

uses pcmizver,mizenv,errhan,monitor,irlabact,parser,mscanner,mconsole
     {$IFDEF MDEBUG} ,info {$ENDIF};

begin
 DrawMizarScreen('Irrelevant Label Detector');
 InitExitProc;
 GetArticleName; GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 InitArticle;
 InitDisplayLine('Parsing');
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 Parse;
 FinishScanning;
 dec(ErrorNbr,IrrLabNbr);
 if IrrLabNbr=1 then writeln('**** One Irrelevant Label Found ****')
  else if IrrLabNbr>1 then writeln('**** ',IrrLabNbr,' Irrelevant Labels Found ****');
 DrawErrorsMsg(ErrorNbr);
 FinishDrawing;
end.
