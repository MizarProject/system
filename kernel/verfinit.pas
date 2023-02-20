(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit verfinit;

interface

uses errhan,mizprep;

procedure MSMVerifyArticle(const aProgName: string; aError: ErrorReport
{$IFNDEF PDEBUG} {$IFNDEF ADEBUG}; aChecker: Procedure0{$ENDIF} {$ENDIF}
 );

procedure ReviewArticle(const aName:string; aChkInf: Procedure0; aPBloc: MizPBlockPtr);

procedure VerifierError(Pos:Position; ErrNr:integer);

implementation

uses
  mizenv,pcmizver,monitor,mconsole,mtime,_formats, parser, mscanner,
  parseraddition, wsmarticle ,first_identification, trans2analyzer
  {$IFNDEF PDEBUG}, analyzer
   {$IFNDEF ADEBUG}, prepobj, prephan,  schemes
   {$ENDIF}
  {$ENDIF}
  {$IFDEF MDEBUG} ,info {$ENDIF};

var PassTime: longint;

procedure InitPass(const aPassName: string);
begin
  with CurPos do begin Line:=1; Col:=1 end;
  InitDisplayLine(aPassName);
  TimeMark(PassTime);
{$IFDEF MDEBUG}
  writeln(InfoFile); writeln(InfoFile,' ':30,aPassName);
  writeln(InfoFile);
{$ENDIF}
end;

procedure FinishPass;
begin
 FinishingPass:=true;
 if QuietMode then DisplayLine(CurPos.Line,ErrorNbr);
 FinishingPass:=false;
 DrawTime('  '+ReportTime(PassTime));
end;

procedure FollowingPass(const aPassName: string);
begin FinishPass; InitPass(aPassName) end;

procedure VerifierError(Pos:Position; ErrNr:integer);
begin
 WriteError(Pos,ErrNr);
 DisplayLine(CurPos.Line,ErrorNbr);
end;

var _MizExitProc: pointer;

procedure VerifierExitProc;{(ErrorCode: word);} {*MZ kto tego ErrorCode uzywa?}
begin
 ExitProc:=_MizExitProc;
 {$I-}
 if IOResult<>0 then;
 if not StopOnError then DisplayLine(CurPos.Line,ErrorNbr);
 PutError:=WriteError;
 DrawVerifierExit(ReportTime(gStartTime));
{ Halt(ErrorCode);}
 {$I+}
end;

procedure InitVerifier(const aProgName: string; aError: ErrorReport);
begin
  DrawMizarScreen(aProgName);
  if paramcount<1 then EmptyParameterList;
  GetArticleName; GetEnvironName;
  DrawArticleName(MizFileName+ArticleExt);
  GetOptions;
  InitExitProc;
  FileExam(MizFileName+ArticleExt);
  _MizExitProc := ExitProc;   ExitProc:=@VerifierExitProc;
  PutError:=aError;
  OpenErrors(MizFileName);
{$IFDEF MDEBUG}
  OpenInfoFile;
{$ENDIF}
end;

procedure MSMVerifyArticle;
begin
  InitVerifier(aProgname,aError);
{------------------ Parsing ------------------------}
  if not CheckerOnly then
   begin
    InitPass('Parser  ');
    FileExam(EnvFileName+'.dct');
    InitScanning(MizFileName+ArticleExt,EnvFileName);
    InitWSMizarArticle;
    Parse;
    gFormatsColl.StoreFormats(EnvFileName+'.frx');
    gFormatsColl.Done;
    FinishScanning;
    Write_WSMizArticle(gWSTextProper,EnvFileName+'.wsx');
    FollowingPass('MSM     ');
    MSMAnalyzer;
    Transfer2Analyzer;
   end;
  {$IFNDEF PDEBUG}
  if not ParserOnly then
   begin
{------------------ Analyser ------------------------}
    if not CheckerOnly then
     begin
      FollowingPass('Analyzer');
      Analyze;
      DisposeAnalyze;
     end;
{------------------ Checker ------------------------}
    {$IFNDEF ADEBUG}
    if not AnalyzerOnly then
     begin
      if CheckerOnly then
       InitPass('Checker ')
      else FollowingPass('Checker ');
      ChkInference:=aChecker;
      gPrBlockPtr:=new(MizPBlockPtr,Init(blMain));
      MizPBlockPtr(gPrBlockPtr)^.InitPrepData;
      Prepare;
     end;
    {$ENDIF}
   end;
  {$ENDIF}
  FinishPass;
end;

procedure ReviewArticle(const aName:string; aChkInf: Procedure0; aPBloc: MizPBlockPtr);
begin
 DrawMizarScreen(aName);
 if paramcount<1 then EmptyParameterList;
 GetArticleName; GetEnvironName;
 GetOptions;
 InitExitProc;
 FileExam(MizFileName+ArticleExt);
 DrawArticleName(MizFileName+ArticleExt);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 OpenErrors(MizFileName);

 InitDisplayLine('Parser  ');
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 InitWSMizarArticle;
 Parse;
 gFormatsColl.StoreFormats(EnvFileName+'.frx');
 gFormatsColl.Done;
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

 InitDisplayLine('Checker ');
 if @aChkInf <> nil then
  ChkInference:=aChkInf;
 gPrBlockPtr:=aPBloc;
 MizPBlockPtr(gPrBlockPtr)^.InitPrepData;
 Prepare;
 DisplayLine(CurPos.Line,ErrorNbr);
 DrawErrorsMSg(ErrorNbr);
 FinishDrawing;

end;

end.
