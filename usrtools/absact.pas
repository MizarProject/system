(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit absact;

interface

procedure InitArticle;

implementation

uses syntax,mscanner,errhan,mobjects,mizenv,pragmas;

type

 AbsItemPtr = ^AbsItemObj;
 AbsItemObj =
  object(ItemObj)
    nItemPos,nJustBeg,nMeansPos: Position;
    nJustified,nLabeled: boolean;
   procedure ProcessLabel; virtual;
   procedure StartTheoremBody; virtual;
   procedure StartSimpleJustification; virtual;
   procedure FinishSimpleJustification; virtual;
   procedure FinishSchemeDeclaration; virtual;
   procedure ProcessMeans; virtual;
   procedure ProcessEquals; virtual;
   procedure ProcessDefiniensLabel; virtual;
   procedure StartDefiniens; virtual;
   procedure StartEquals; virtual;
   constructor Init(fItemKind:ItemKind);
   procedure Pop; virtual;
  end;

 AbsBlockPtr = ^AbsBlockObj;
 AbsBlockObj =
  object(BlockObj)
   nJustBeg, nJustEnd: Position;
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
   procedure ProcessRedefine; virtual;
   procedure ProcessBegin; virtual;
   procedure ProcessPragma; virtual;
   procedure StartSchemeDemonstration; virtual;
   procedure FinishSchemeDemonstration; virtual;

   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
  end;

 RemBlockPtr = ^RemBlockObj;
 RemBlockObj =
  object(BlockObj)
    nPrevPos: Position;
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
  end;

var targetBuff: PByteArray;
    TargetFile, AliasFile: text;
    SomethingToBeRemoved: boolean;
    FirstRemovable: Position;
    gDefCnt,gTheoCnt,gSchCnt: integer;
    gSchemeDemonstration: boolean;

procedure PrintPos(const fPos:Position);
begin write(TargetFile,fPos.Line,' ',fPos.Col,' ') end;

procedure DeleteText(const fStart,fEnd:Position);
begin write(TargetFile,'d'); PrintPos(fStart); PrintPos(fEnd);
 writeln(TargetFile);
end;

procedure InsertLine(const fPos:Position);
begin write(TargetFile,'g'); PrintPos(fPos); writeln(TargetFile);
 writeln(TargetFile,'l');
end;

procedure InsertTextAt(fPos:Position; fText:string);
begin write(TargetFile,'g'); PrintPos(fPos); writeln(TargetFile);
 writeln(TargetFile,'i',fText);
end;

procedure InitArticle;
begin
 new(targetBuff);
 assign(TargetFile,MizFileName+'.edt');
 SetTextBuf(TargetFile,targetBuff^);
 rewrite(TargetFile);
 gBlockPtr:=new(AbsBlockPtr, Init(blMain));
 gDefCnt:=0;
 gTheoCnt:=0;
 gSchCnt:=0;
 Assign(AliasFile, MizFileName+'.ali'); Rewrite(AliasFile);
end;

procedure SwitchOffSomethingToBeRemoved;
begin
 if SomethingToBeRemoved then
  begin SomethingToBeRemoved:=false;
   if FirstRemovable.Line = CurPos.Line then InsertLine(FirstRemovable);
   if FirstRemovable.Line+1 >= CurPos.Line then
    InsertLine(FirstRemovable);
   DeleteText(FirstRemovable,PrevPos);
  end;
end;

constructor AbsItemObj.Init;
 var lStr: string;
     lNextPos, lPrevPos: Position;
begin inherited Init(fItemKind);
 nJustified:=false; nItemPos:=CurPos;
 lNextPos:=nItemPos; inc(lNextPos.Col);
 SwitchOffSomethingToBeRemoved;
 case nItemKind of
  itTheorem,itAxiom:
   begin
    inc(gTheoCnt);
    Str(gTheoCnt,lStr);
    InsertTextAt(lNextPos,' :: '+UpperCase(ArticleName)+':'+lStr);
   end;
  itDefinition: begin FirstRemovable:=CurPos; inc(FirstRemovable.Col); end;
  itSchemeHead:
   begin
    inc(gSchCnt);
    Str(gSchCnt,lStr);
    lPrevPos:=PrevPos; inc(lPrevPos.Col);
    InsertTextAt(lPrevPos,' :: '+UpperCase(ArticleName)+':sch '+lStr);
    if lPrevPos.Line = CurPos.Line then
     InsertLine(lPrevPos);
//    DeleteText(lPrevPos, nItemPos);
//    if AheadPos.Line = CurPos.Line then InsertLine(lNextPos);
    Writeln(AliasFile, CurWord.Spelling, ' => '+UpperCase(ArticleName)+':sch '+lStr);
   end;
 end;
end;

procedure AbsItemObj.Pop;
begin
 FirstRemovable:=CurPos; inc(FirstRemovable.Col);
 inherited Pop;
end;

procedure AbsItemObj.ProcessLabel;
 var IdentBeg,NextPos: Position;
begin
 IdentBeg:=CurPos; dec(IdentBeg.Col,length(CurWord.Spelling)-1);
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
  if CurPos.Line <> AheadPos.Line then
   begin DeleteText(IdentBeg,CurPos); DeleteText(AheadPos,AheadPos) end
  else DeleteText(IdentBeg,AheadPos);
end;

procedure AbsItemObj.StartTheoremBody;
 var ThBoPos,ThLaPos: Position; lStr: string;
begin
 if nItemPos.Line = CurPos.Line then
  begin ThBoPos:=CurPos; dec(ThBoPos.Col,length(CurWord.Spelling)-1);
   InsertLine(ThBoPos);
  end;
end;

procedure AbsItemObj.StartSimpleJustification;
begin
 nJustified:=true; nJustBeg:=PrevPos;
end;

procedure AbsItemObj.FinishSimpleJustification;
begin
 if nJustified and
  ((nJustBeg.Line <> PrevPos.Line) or (nJustBeg.Col <> PrevPos.Col)) then
   begin inc(nJustBeg.Col);
    DeleteText(nJustBeg,PrevPos);
   end;
end;

procedure AbsItemObj.FinishSchemeDeclaration;
begin
   AbsBlockPtr(gBlockPtr)^.nJustBeg:=PrevPos;
   inc(AbsBlockPtr(gBlockPtr)^.nJustBeg.Col);
end;

procedure AbsItemObj.ProcessMeans;
 var lStr:string;
begin nMeansPos:=CurPos; inc(nMeansPos.Col);
 inc(gDefCnt); Str(gDefCnt,lStr);
 InsertLine(nMeansPos);
 InsertTextAt(nMeansPos,':: '+UpperCase(ArticleName)+':def '+lStr);
 nLabeled:=AheadWord.Kind = sy_Colon;
end;

procedure AbsItemObj.ProcessEquals;
 var lStr:string;
begin nMeansPos:=CurPos; inc(nMeansPos.Col);
 inc(gDefCnt); Str(gDefCnt,lStr);
 InsertLine(nMeansPos);
 InsertTextAt(nMeansPos,':: '+UpperCase(ArticleName)+':def '+lStr);
 nLabeled:=AheadWord.Kind = sy_Colon;
end;

procedure AbsItemObj.ProcessDefiniensLabel;
 var lFirstColon,lLabBeg: Position;
begin
 if nLabeled then
 begin lFirstColon:=PrevPos;
  if lFirstColon.Line = AheadPos.Line then
   DeleteText(lFirstColon,AheadPos)
  else
  if lFirstColon.Line = CurPos.Line then
   begin DeleteText(lFirstColon,CurPos); DeleteText(AheadPos,AheadPos) end
  else
   begin lLabBeg:=CurPos; dec(lLabBeg.Col,length(CurWord.Spelling)-1);
    DeleteText(lFirstColon,lFirstColon); DeleteText(lLabBeg,AheadPos)
   end;
 end;
end;

procedure AbsItemObj.StartDefiniens;
 var lPos:Position;
begin
 if nMeansPos.Line = CurPos.Line then
  begin lPos:=CurPos; dec(lPos.Col,Length(CurWord.Spelling)-1);
   InsertLine(lPos);
  end;
end;

procedure AbsItemObj.StartEquals;
 var lPos:Position;
begin
 if nMeansPos.Line = CurPos.Line then
  begin lPos:=CurPos; dec(lPos.Col,Length(CurWord.Spelling)-1);
   InsertLine(lPos);
  end;
end;

procedure AbsBlockObj.ProcessPragma;
 var lNbr: integer;
     lKind: char;
begin
 SwitchOffSomethingToBeRemoved;
 FirstRemovable.Col:=1;
 FirstRemovable.Line:=CurPos.Line+1;
 CanceledPragma(CurWord.Spelling,lKind,lNbr);
 case lKind of
  'T':
   if nBlockKind = blMain then
    inc(gTheoCnt,lNbr);
  'D':
   if nBlockKind in [blDefinition,blMain] then
    inc(gDefCnt,lNbr);
  'S':
   if nBlockKind = blMain then
    inc(gSchCnt,lNbr);
  end
end;

procedure AbsBlockObj.StartSchemeDemonstration;
begin
 gSchemeDemonstration:=true;
end;

procedure AbsBlockObj.FinishSchemeDemonstration;
begin
 gSchemeDemonstration:=false;
 nJustEnd:=PrevPos;
end;

constructor AbsBlockObj.Init;
begin inherited Init(fBlockKind);
 if nBlockKind = blPublicScheme then
  SwitchOffSomethingToBeRemoved;
end;

procedure AbsBlockObj.Pop;
begin
 case nBlockKind of
  blMain:
   begin
    close(TargetFile);
    Close(AliasFile);
   end;
  blDefinition, blRegistration:
   SwitchOffSomethingToBeRemoved;
  blPublicScheme: DeleteText(nJustBeg,nJustEnd);
 end;
 inherited Pop;
end;

procedure AbsBlockObj.ProcessRedefine;
begin
 if CurWord.Kind=sy_Redefine then
 begin
  SwitchOffSomethingToBeRemoved;
  FirstRemovable:=CurPos; inc(FirstRemovable.Col);
 end;
end;

procedure AbsBlockObj.ProcessBegin;
begin
 SwitchOffSomethingToBeRemoved;
 FirstRemovable:=CurPos; inc(FirstRemovable.Col);
end;

procedure AbsBlockObj.CreateItem;
begin
 case fItemKind of
  itRegularStatement,itCorrCond,itCorrectness,
  itConstantDefinition,itPrivFuncDefinition,itPrivPredDefinition,
  itChoice,itReconsider:
   begin gItemPtr:=new(ItemPtr, Init(fItemKind));
    SomethingToBeRemoved:=true;
   end;
 else if gSchemeDemonstration then gItemPtr:=new(ItemPtr, Init(fItemKind))
 else gItemPtr:=new(AbsItemPtr, Init(fItemKind));
 end;
end;

procedure AbsBlockObj.CreateBlock;
begin
 case fBlockKind of
  blDefinition: gBlockPtr:=new(AbsBlockPtr,Init(blDefinition));
  blRegistration: gBlockPtr:=new(AbsBlockPtr,Init(blRegistration));
  blPublicScheme: gBlockPtr:=new(AbsBlockPtr,Init(blPublicScheme));
  blProof:
   if (gItemPtr^.nItemKind in [itTheorem,itProperty,itPropertyRegistration])
//   or gSchemeDemonstration
//     or (nBlockKind = blPublicScheme) then
    then gBlockPtr:=new(RemBlockPtr,Init(fBlockKind))
    else gBlockPtr:=new(BlockPtr,Init(fBlockKind));
  else gBlockPtr:=new(BlockPtr,Init(fBlockKind));
 end;
end;

constructor RemBlockObj.Init;
begin inherited Init(fBlockKind);
 nPrevPos:=PrevPos; inc(nPrevPos.Col)
end;

procedure RemBlockObj.Pop;
begin DeleteText(nPrevPos,CurPos);
 inherited Pop;
end;

end.
