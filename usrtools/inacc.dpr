(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program InAccItems;

uses pcmizver,mizenv,monitor,parser,errhan,inaccact,mscanner,mconsole
    {$IFDEF MDEBUG} ,info {$ENDIF};

begin
 DrawMizarScreen('Inaccessible Items Detector');
 InitExitProc;
 GetArticleName; GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 InitDisplayLine('Parsing');
 InitArticle;
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 Parse;
 FinishScanning;
 writeln;
 dec(ErrorNbr,InAccItemsNbr*2+InaccLinkNbr);
 if InaccItemsNbr=1 then writeln('**** One Inaccessible Item Found ****')
  else if InaccItemsNbr>1 then writeln('**** ',InaccItemsNbr,' Inaccessible Items Found ****');
 if InaccLinkNbr=1 then writeln('**** One Inaccessible Linkage Found ****')
  else if InaccLinkNbr>1 then writeln('**** ',InaccLinkNbr,' Inaccessible Linkages Found ****');
 DrawErrorsMsg(ErrorNbr);
 FinishDrawing;
end.
