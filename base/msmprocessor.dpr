(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program More_Strict_Mizar_Processor;

uses mizenv, pcmizver, monitor, errhan, mconsole, mtime,
     _formats, wsmarticle, block_and_item,first_identification
{$IFDEF MDEBUG} ,info {$ENDIF};

var PassTime: longint;

procedure InitPass(const aPassName: string);
begin
 CurPos.Line:=1;
 CurPos.Col:=1;
 InitDisplayLine(aPassName);
 TimeMark(PassTime);
end;

procedure FinishPass;
begin
 FinishingPass:=true;
 if QuietMode then DisplayLine(CurPos.Line,ErrorNbr);
 FinishingPass:=false;
 DrawTime('  '+ReportTime(PassTime));
end;

procedure ParserError(Pos:Position; ErrNr:integer);
begin
 WriteError(Pos,ErrNr);
 DisplayLine(CurPos.Line,ErrorNbr);
end;

var _WSMizExitProc: pointer;
procedure WSMizarExitProc;
begin
 ExitProc:=_WSMizExitProc;
 {$I-}
 if IOResult<>0 then;
 if not StopOnError then DisplayLine(CurPos.Line,ErrorNbr);
 PutError:=WriteError;
 DrawVerifierExit(ReportTime(gStartTime));
{ Halt(ErrorCode);}
 {$I+}
end;

procedure InitAnalyzing(const aProgName: string);
begin
  DrawMizarScreen(aProgName);
  if paramcount<1 then EmptyParameterList;
  GetArticleName;
  GetEnvironName;
  DrawArticleName(MizFileName+'.wsx');
  GetOptions;
  InitExitProc;
  FileExam(MizFileName+'.wsx');
  _WSMizExitProc := ExitProc;
  ExitProc:=@WSMizarExitProc;
  PutError:=ParserError;
  OpenErrors(MizFileName);
{$IFDEF MDEBUG}
  OpenInfoFile;
{$ENDIF}
end;

procedure MSMAnalyzer;
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
 InitAnalyzing('More Strict Mizar Processor');
// Parsing;
 MSMAnalyzer;
 if ErrorNbr > 0 then
  begin
   DrawErrorsMsg(ErrorNbr);
   FinishDrawing;
  end;
end.

