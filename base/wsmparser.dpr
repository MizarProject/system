(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program Weakly_Strict_Mizar_Parser;

uses mizenv, pcmizver, monitor, errhan, mconsole, mtime,
     _formats, parseraddition, syntax,
     parser, mscanner, wsmarticle, xml_parser
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

procedure InitParsing(const aProgName: string);
begin
  DrawMizarScreen(aProgName);
  if paramcount<1 then EmptyParameterList;
  GetArticleName;
  GetEnvironName;
  DrawArticleName(MizFileName+ArticleExt);
  GetOptions;
  InitExitProc;
  FileExam(MizFileName+ArticleExt);
  _WSMizExitProc := ExitProc;
  ExitProc:=@WSMizarExitProc;
  PutError:=ParserError;
  OpenErrors(MizFileName);
{$IFDEF MDEBUG}
  OpenInfoFile;
{$ENDIF}
end;

procedure InitWSMizArticle;
begin
 gWSTextProper:=new(wsTextProperPtr,Init(ArticleID,ArticleExt,CurPos));
 gLastWSBlock:=gWSTextProper;
 gLastWSItem:=nil;
 gBlockPtr:=new(extBlockPtr, Init(blMain));
end;

procedure Parsing;
begin
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 InitWSMizArticle;
 InitPass('Parser  ');
 Parse;
 gFormatsColl.Done;
 FinishScanning;
 Write_WSMizArticle(gWSTextProper,MizFileName+'.wsx');
 FinishPass;
end;

begin
 InitParsing('Pure Parser for Mizar');
 Parsing;
 if ErrorNbr > 0 then
  begin
   DrawErrorsMsg(ErrorNbr);
   FinishDrawing;
  end;
 Print_WSMizArticle(gWSTextProper,MizFileName+'.wsm');
end.

