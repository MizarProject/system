(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program Hereby2ThusNow;

uses pcmizver,mizenv,errhan,monitor,mscanner,mconsole
     {$IFDEF MDEBUG} ,info {$ENDIF};

var edt: Text;

Procedure ProcessText;
Begin
   Assign(edt, MizFileName+'.edt');
   Rewrite(edt);
   ReadToken;
   while CurWord.Kind <> EOT do
   begin
      if CurWord.Kind = sy_Hereby then
      begin
	 Error(CurPos,705);
	 WriteLn(edt, 'd', CurPos.Line, ' ', CurPos.Col-5, ' ', CurPos.Line, ' ', CurPos.Col);
	 WriteLn(edt, 'ithus now');
      end;
      ReadToken;
   end;
   Close(edt);
End;

BEGIN
 writeln('hereby -> thus now, ',PCMizarVersionStr);
 writeln(Copyright);
 InitExitProc;
 GetArticleName; GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 Write('Parsing');
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 ProcessText;
 FinishScanning;
 FinishDrawing;  
END.
