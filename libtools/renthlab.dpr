(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program RenThLab;

uses pcmizver,mconsole,mizenv,errhan,monitor,parser,mscanner,
     syntax,edt_han,labelact;

type
 ThLabItemPtr = ^ThLabItemObj;
 ThLabItemObj =
  object(LabItemObj)
   constructor Init(fKind:ItemKind);
   procedure Pop; virtual;
   procedure ProcessMeans; virtual;
   procedure ProcessEquals; virtual;
   procedure InsertLabel; virtual;
   procedure InsertDefiniensLabel; virtual;
  end;

 ThLabBlockPtr = ^ThLabBlockObj;
 ThLabBlockObj =
  object(LabIdBlockObj)
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
  end;

 ThLabelPtr = ^ThLabelObj;
 ThLabelObj = Object(LabelDescrObj)
    LabRep: string;
   constructor Init(const fIdent:string; fKind:ItemKind);
   procedure ProcessReference; virtual;
  end;

 var
  gDfNbr,gThNbr,gLmNbr,gLocNbr: integer;
  gEDT: EdtCollection;

procedure InitArticle;
begin
 gBlockPtr:=new(ThLabBlockPtr, Init(blMain));
end;

constructor ThLabItemObj.Init;
begin inherited Init(fKind);
 case fKind of
  itTheorem: inc(gThNbr);
  itCanceled:
   if  gBlockPtr^.nBlockKind = blMain then
    begin
     if AheadWord.Nr = 0 then inc(gThNbr)
     else inc(gThNbr,AheadWord.Nr)
    end
   else
    begin
     if AheadWord.Nr = 0 then inc(gDfNbr)
     else inc(gDfNbr,AheadWord.Nr);
    end;
 end;
 DisplayLine(CurPos.Line,ErrorNbr);
end;

procedure ThLabItemObj.Pop;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Pop;
end;

procedure ThLabItemObj.ProcessMeans;
begin
 inc(gDfNbr);
end;

procedure ThLabItemObj.ProcessEquals;
begin
 inc(gDfNbr);
end;

procedure ThLabItemObj.InsertLabel;
begin
 gLab.Push(new(ThLabelPtr,Init(CurWord.Spelling,nItemKind)));
end;

procedure ThLabItemObj.InsertDefiniensLabel;
begin
 gDefLab.Push(new(ThLabelPtr,Init(CurWord.Spelling,nItemKind)));
end;

constructor ThLabBlockObj.Init;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Init(fBlockKind);
 case fBlockKind of
 blMain:
  begin
   gEDT.Init(200,100);
   gThNbr:=0; gLmNbr:=0; gDfNbr:=0; gLocNbr:=0;
  end;
 else if BlockPtr(Previous)^.nBlockKind = blMain  then gLocNbr:=0;
 end;
end;

procedure ThLabBlockObj.CreateItem;
begin gItemPtr:=new(ThLabItemPtr, Init(fItemKind)) end;

procedure ThLabBlockObj.CreateBlock;
begin gBlockPtr:=new(ThLabBlockPtr,Init(fBlockKind)) end;

procedure ThLabBlockObj.Pop;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 case nBlockKind of
 blMain:
  begin
   if gEdt.Count > 0 then gEDT.StoreEdt(MizFileName);
  end;
 end;
 inherited Pop;
end;

constructor ThLabelObj.Init;
 var lPos: Position;
begin inherited Init(fIdent,fKind);
 LabRep:='';
 lPos.Line:=CurPos.Line;
 lPos.Col:=CurPos.Col-length(CurWord.Spelling)+1;
 case fKind of
 itDefPred, itDefFunc, itDefMode, itDefAttr:
  begin
   Str(gDfNbr,LabRep);
   LabRep:='Def'+LabRep;
   gEDT.Insert_d(lPos,CurPos);
   gEDT.Insert_i(LabRep);
  end;
 itTheorem:
  begin
   Str(gThNbr,LabRep);
   LabRep:='Th'+LabRep;
   gEDT.Insert_d(lPos,CurPos);
   gEDT.Insert_i(LabRep);
  end;
 itRegularStatement, itChoice:
  if BlockPtr(gBlockPtr)^.nBlockKind = blMain then
    begin
     inc(gLmNbr);
     Str(gLmNbr,LabRep);
     LabRep:='Lm'+LabRep;
     gEDT.Insert_d(lPos,CurPos);
     gEDT.Insert_i(LabRep);
    end
  else
    begin inc(gLocNbr);
     Str(gLocNbr,LabRep);
     LabRep:='A'+LabRep;
     gEDT.Insert_d(lPos,CurPos);
     gEDT.Insert_i(LabRep);
    end;
 itDefinition, itSchemeBlock, itSchemeHead,
 itGeneralization, itLociDeclaration,itExistentialAssumption,
 itConclusion, itCaseHead, itSupposeHead, itAssumption:
   begin inc(gLocNbr);
     Str(gLocNbr,LabRep);
     LabRep:='A'+LabRep;
     gEDT.Insert_d(lPos,CurPos);
     gEDT.Insert_i(LabRep);
   end;
 end;
end;

procedure ThLabelObj.ProcessReference;
 var lPos:Position;
begin inherited ProcessReference;
 if LabRep <> '' then
  begin
   lPos.Line:=CurPos.Line;
   lPos.Col:=CurPos.Col-length(CurWord.Spelling)+1;
   gEDT.Insert_d(lPos,CurPos);
   gEDT.Insert_i(LabRep);
  end;
end;

begin
 DrawMizarScreen('Theorems Label Editor');
 InitExitProc;
 GetArticleName; GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 InitDisplayLine('Parsing');
 InitArticle;
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 Parse;
 FinishScanning;
 DisplayLine(CurPos.Line,ErrorNbr);
 DrawErrorsMSg(ErrorNbr);
 FinishDrawing;
end.
