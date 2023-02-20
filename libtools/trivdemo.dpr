(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program TrivDemo;

uses pcmizver,mconsole,mobjects,mizenv,monitor,errhan,parser,mscanner,scanner,
     parseraddition,_formats,wsmarticle,first_identification,
     trans2analyzer,analyzer,prephan,prepobj,td_prep,checker
{$IFDEF TRIVDEMO}
     ,edt_han
{$ENDIF}
{$IFDEF MDEBUG} ,info {$ENDIF};

{$IFDEF TRIVDEMO}
var i : integer;
{$ENDIF}

begin
 DrawMizarScreen('Trivial Proofs Detector');
 if paramcount<1 then EmptyParameterList;
 GetArticleName; GetEnvironName;
 GetOptions;
 InitExitProc;
 FileExam(MizFileName+ArticleExt);
 DrawArticleName(MizFileName+ArticleExt);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 OpenErrors(MizFileName);

{$IFDEF TRIVDEMO}
   gLabsPos:=0;
{$ENDIF}

 InitDisplayLine('Parser  ');
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 InitWSMizarArticle;
 Parse;
 gFormatsColl.StoreFormats(EnvFileName+'.frx');
 FinishScanning;
 Write_WSMizArticle(gWSTextProper,EnvFileName+'.wsx');
 InitDisplayLine('MSM     ');
 MSMAnalyzer;
 Transfer2Analyzer;

 InitDisplayLine('Analyser');
 Analyze;
 DisposeAnalyze;
 DisplayLine(CurPos.Line,ErrorNbr);
 CurPos.Line:=1;

{$IFDEF TRIVDEMO}
 Assign(gEdt, MizFileName+'.edt');
 Rewrite(gEdt);
{$ENDIF}

 InitDisplayLine('Proofs  ');
 gPrBlockPtr:=new(TDemoPBlockPtr,Init(blMain));
 TDemoPBlockPtr(gPrBlockPtr)^.InitPrepData;
 Prepare;
 DisplayLine(CurPos.Line,ErrorNbr);
 DrawErrorsMSg(ErrorNbr);
 FinishDrawing;

{$IFDEF TRIVDEMO}
 for i:=0 to gStartPos.Count-1 do
 begin
    writeln(gEdt,'d',StrPos(PPosition(gStartPos.Items^[i])^.Pos),' ',StrPos(PPosition(gEndPos.Items^[i])^.Pos));
    if (MStrPtr(gI.Items^[i])^.fStr <> '') then
       writeln(gEdt,'iby ',MStrPtr(gI.Items^[i])^.fStr);
 end;

 Close(gEdt);
{$ENDIF}

end.
