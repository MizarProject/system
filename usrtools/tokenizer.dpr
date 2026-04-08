(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program Tokenizer;

uses mizenv,monitor,scanner,mscanner,mconsole,errhan,xml_dict,xml_inout;

var
  gWords: array of MTokenPtr;   // of MTokenPtr - all words in article
  gWordsNbr: integer;

procedure AddToken;
begin
 ReadToken;
 if length(gWords) = 1 + gWordsNbr then
  begin
   setlength(gWords, 2*length(gWords));
  end;
 gWords[gWordsNbr]:=new(MTokenPtr,Init(gScanner^.fLexem.Kind,gScanner^.fLexem.Nr,gScanner^.fStr,gScanner^.fPos));
 inc(gWordsNbr);
end;

procedure WriteTOCfile;
var i: integer;
var lFile: XMLOutStreamObj;
begin
 lFile.OpenFile(EnvFileName+'.tok');
 lFile.Out_XElStart0( XMLElemName[elTokens]);
 for i:=0 to gWordsNbr-1 do
  begin
   lFile.Out_XElStart( XMLElemName[elToken]);
   lFile.Out_XQuotedAttr( XMLAttrName[atSpelling], MTokenPtr(gWords[i])^.fStr);
   lFile.Out_PosAsAttrs(MTokenPtr(gWords[i])^.fPos);
   lFile.Out_XElEnd0;
  end;
 lFile.Out_XElEnd( XMLElemName[elTokens]);
 lFile.Done;
end;

procedure ProcessArticle;
begin
 setlength(gWords,100000);
 gWordsNbr:=0;
 AddToken;
 while CurWord.Kind <> EOT do
  begin
   AddToken;
  end;
 WriteTOCfile;
 gWords:=nil;
end;

BEGIN
 InitExitProc;
 GetArticleName;
 GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 ProcessArticle;
 FinishScanning;
 FinishDrawing;
END.
