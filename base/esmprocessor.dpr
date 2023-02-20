(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program Even_More_Strict_Mizar_Processor;

uses mizenv, mstate, errhan, mconsole,
     _formats, wsmarticle,
     block_and_item, first_identification
{$IFDEF MDEBUG} ,info {$ENDIF};

procedure ESMAnalyzer;
 var lWSTextProper: wsTextProperPtr;
     lMizArticle: ProcessingArticlePtr;
begin
 FileExam(EnvFileName+'.frx');
 gFormatsColl.LoadFormats(EnvFileName+'.frx');
 lWSTextProper:=Read_WSMizArticle((MizFileName+'.wsx'));
 lMizArticle:=new(FirstIdentArticlePtr,Init(lWSTextProper));
 InitPass('First Identyfication ');
 lMizArticle^.nDisplayInformationOnScreen:=true;
 lMizArticle^.Process_Article;
 FinishPass;
// InitPass('XML                  ');
 Write_MSMizArticle(lWSTextProper,MizFileName+'.msx');
// FinishPass;
 InitPass('Printing             ');
 Print_MSMizArticle(lWSTextProper,MizFileName+'.msm');
 FinishPass;
 dispose(lMizArticle,Done);
// dispose(lWSTextProper,Done);
end;

begin
 InitProcessing('More Strict Mizar Processor','.msm');
 ESMAnalyzer;
 ProcessingEnding;
end.

