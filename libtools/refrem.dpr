(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program ReferenceRemoving;

 uses pcmizver,mizenv,errhan,mconsole,monitor,rpremact,parser,mscanner
 {$IFDEF MDEBUG},info {$ENDIF};

begin
 DrawMizarScreen('Irrelevant References Editor');
 GetArticleName; GetEnvironName;
 GetOptions;
 InitExitProc;
 FileExam(MizFileName+ArticleExt);
 DrawArticleName(MizFileName+ArticleExt);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 OpenErrors(MizFileName);
 IrrPremErrors.ReadErrors(MizFileName+'.$er');
 if IrrPremErrors.Count = 0 then begin IrrPremErrors.Done;  halt end;
 gEDT.Init(200,100);
 RpremactInitArticle;
 InitDisplayLine('Parser  ');
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 Parse;
 FinishScanning;
 if not IrrPremErrors.EofErrors then
  DrawMessage('Internal error','There are some unused errors');
 IrrPremErrors.Done;
 gEDT.StoreEDT(MizFileName);
 gEDT.Done;
 FinishDrawing;
 DrawErrorsMSg(ErrorNbr);
 FinishDrawing;
end.
