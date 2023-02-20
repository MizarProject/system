(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program MakeEnvironment;

uses mobjects,pcmizver,mizenv,librenv,mconsole,monitor,errhan,
     mscanner,envhan,inlibr,acc_han
{$IFDEF MDEBUG} ,info {$ENDIF};
 var OldEvl: EnvironmentDescr;

 function EqEvl(const EVL1,EVL2: MSortedStrList): boolean;
   var k: integer;
  begin EqEvl:=false;
   if EVL1.Count <> EVL2.Count then exit;
   for k:=0 to EVL1.Count-1 do
    if PImpArticleId(EVL1.Items^[k])^.fStr <>
       PImpArticleId(EVL2.Items^[k])^.fStr then exit;
   EqEvl:=true;
  end;
 procedure CheckEvl;
   var lTime,lEVLTime,lFILTime: longint;
       lLocFiles: FileDescrCollection;
       i: integer;
       lDir: DirectiveKind;
       lvct:string;
   label 1;
  begin
   for lDir:=low(DirectiveKind) to High(DirectiveKind) do
    if not EqEvl(OldEvl.Directive[lDir],Env.Directive[lDir]) then
     begin Accomodation:=true; exit end;
   if MFileExists(MizFileName+'.fil') then
    begin lLocFiles.LoadFil(MizFileName+'.fil');
     lFILTime:=GetFileTime(MizFileName+'.fil');
     lEVLTime:=GetFileTime(EnvFileName+'.evl');
     if lEVLTime > lFILTime then
      begin Accomodation:=true; goto 1 end;
     if lLocFiles.Count <> 0 then
       for i:=0 to lLocFiles.Count-1 do
        with PFileDescr(lLocFiles.Items^[i])^ do
         begin
          lvct:=nName^;
          delete(lvct,1,length(lvct)-4);
          if (lvct='.vct') and (nName^<>MizFiles+MML+'.vct') then begin Accomodation:=true; goto 1 end;
          lTime:=GetFileTime(nName^);
          if Time <> lTime then begin Accomodation:=true; goto 1 end;
          if lTime > lFILTime then begin Accomodation:=true; goto 1 end;
         end
     else Accomodation:=true;
1:
     lLocFiles.Done;
    end
   else begin Accomodation:=true; exit end;
  end;

var _MizExitProc:pointer;

procedure PCMizExitProc;
begin
 ExitProc:=_MizExitProc;
 {$I-}
 if IOResult<>0 then;
 if LogOpened then CloseLogFile;
 if IOResult<>0 then;
 {$I+}
end;

begin
 DrawMizarScreen('Make Environment');
 GetArticleName; GetEnvironName;
 FileExam(MizFileName+ArticleExt);
 InitExitProc;
 _MizExitProc := ExitProc;
 ExitProc:=@PCMizExitProc;
 {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
 OpenErrors(MizFileName);
 InitAccOptions;
 GetMEOptions;
 FileExam(MizFiles+MML+'.vct');
 LocFilesCollection.Insert(New(PFileDescr,Init(MizFiles+MML+'.vct',0)));
 Env.ReadEnvironment;
 if ErrorNbr > 0 then
  begin
   DrawErrorsMsg(ErrorNbr);
   Halt(1)
  end;
 if not Accomodation then
  if MFileExists(EnvFileName+'.evl') then
   begin
    OldEvl.LoadEvl(EnvFileName+'.evl');
    CheckEvl;
   end
  else Accomodation:=true;
 if Accomodation then
  begin
   OpenLogFile;
   Localization:=NewAccom;
   Accomodate;
   if ErrorNbr > 0 then
    begin
     DrawErrorsMsg(ErrorNbr);
     Halt(1)
    end;
   LocFilesCollection.StoreFIL(MizFileName+'.fil');
  end;
 FinishDrawing;
end.



