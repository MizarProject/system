(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit inaccact;

interface

procedure InitArticle;

var InaccItemsNbr,InaccLinkNbr: integer;

implementation

uses mconsole,mobjects,mizenv,monitor,errhan,syntax,edt_han,mscanner
{$IFDEF MDEBUG} ,info {$ENDIF};

type

 InaccItemPtr = ^InaccItemObj;
 InaccItemObj =
  object(ItemObj)
    nItemPos: Position;
    nLabelled: boolean;
   constructor Init(fKind:ItemKind);
   procedure Pop; virtual;
   procedure FinishFixedVariables; virtual;
   procedure ProcessLabel; virtual;
  end;

 InaccBlockPtr = ^InaccBlockObj;
 InaccBlockObj =
  object(BlockObj)
    nLinkedItem,nPendingRemoving,nItemsRemoving,nPragmaOcc: boolean;
    nStartPos,nLastPos,nFinishPos,nPragmaPos: Position;
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
   procedure ProcessLink; virtual;
   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;

   procedure ProcessPragma; virtual;
   procedure RemoveItems;
  end;

var gInaccItems: PosCollection;
    gDefinitionsRemoving: boolean;
    gDefinitionPos,gSuchThatPos,gLinkPrevPos,gBegLinkPos,gEndLinkPos: Position;
    gSuchThatOcc: boolean;
    gLabNbr: integer;

procedure InitArticle;
begin
 gBlockPtr:=new(InaccBlockPtr, Init(blMain));
end;

constructor InaccItemObj.Init;
begin inherited Init(fKind);
 nLabelled:=false; nItemPos:=PrevPos;
 gLabNbr:=0;
 gSuchThatOcc:=false;
 with InaccBlockPtr(gBlockPtr)^ do
   case fKind of
    itRegularStatement,itChoice:;
    else
     if nLinkedItem then
       begin
        if nPendingRemoving then
         begin
          nFinishPos:=nLastPos;
          nPendingRemoving:=false;
          RemoveItems;
         end
        else nItemsRemoving:=false;
       end
      else
       case fKind of
        itDefinition:
         begin gDefinitionsRemoving:=true;
          gDefinitionPos:=PrevPos;
         end;
        itDefPred, itDefFunc, itDefMode, itDefAttr, itDefStruct,
        itCluster, itIdentify, itReduction, itPropertyRegistration,
        itAttrAntonym, itAttrSynonym, itFuncNotation, itModeNotation, itPredSynonym, itPredAntonym:
         begin gDefinitionsRemoving:=false;
          RemoveItems;
         end;
        else RemoveItems;
       end;
   end;
 DisplayLine(CurPos.Line,ErrorNbr);
end;

procedure InaccItemObj.FinishFixedVariables;
begin
 gSuchThatPos:=PrevPos; inc(gSuchThatPos.Col);
 gSuchThatOcc:=CurWord.Kind=sy_Such;
end;

procedure InaccItemObj.ProcessLabel;
begin
 nLabelled:=(CurWord.Kind = Identifier) and (AheadWord.Kind = sy_Colon);
 if nLabelled then inc(gLabNbr);
end;

procedure InaccItemObj.Pop;
 var lPos: Position;
begin
 with InaccBlockPtr(gBlockPtr)^ do
  begin
   case nItemKind of
   itRegularStatement:
    if nLinkedItem then
     begin
      if nLabelled then
       begin
        if nPendingRemoving then
         begin
          nFinishPos:=nLastPos;
          nPendingRemoving:=false;
          RemoveItems;
         end
        else nItemsRemoving:=false;
       end
      else if nItemsRemoving then
       nFinishPos:=CurPos
      else
       begin
        nStartPos:=gLinkPrevPos;
        if nPragmaOcc then
         begin
           nStartPos:=nPragmaPos;
           inc(nStartPos.Line);
           nStartPos.Col:=0;
         end;
        nFinishPos:=CurPos;
        nItemsRemoving:=true;
        nPendingRemoving:=false;
       end;
     end
    else if nLabelled then
     RemoveItems
    else
     begin
      if nItemsRemoving then
       begin nPendingRemoving:=true;
        nLastPos:=nFinishPos;
       end
      else
       begin nStartPos:=nItemPos;
        if nPragmaOcc then
         begin
           nStartPos:=nPragmaPos;
           inc(nStartPos.Line);
           nStartPos.Col:=0;
         end;
        nItemsRemoving:=true;
       end;
      nFinishPos:=CurPos;
     end;
   itDefinition:
    if gDefinitionsRemoving then
     begin
      if nItemsRemoving then
       begin nPendingRemoving:=true;
        nLastPos:=nFinishPos;
       end
      else
       begin nStartPos:=gDefinitionPos;
        nItemsRemoving:=true;
       end;
      nFinishPos:=CurPos;
     end
    else RemoveItems;
   itChoice:
    begin
     if gSuchThatOcc and (gLabNbr = 0) then
      begin
       RemoveItems;
       if nLinkedItem then
        begin Error(gBegLinkPos,603);
         inc(InaccLinkNbr);
         gInaccItems.Insert(new(ErrDesPtr,Init(gBegLinkPos,0)));
         gInaccItems.Insert(new(ErrDesPtr,Init(gEndLinkPos,1)));
        end;
       inc(InaccItemsNbr);
       Error(gSuchThatPos,612);
       lPos:=CurPos; dec(lPos.Col);
       Error(lPos,613);
       gInaccItems.Insert(new(ErrDesPtr,Init(gSuchThatPos,0)));
       gInaccItems.Insert(new(ErrDesPtr,Init(lPos,1)));
      end
     else if nLinkedItem then
       begin
        if nPendingRemoving then
         begin
          nFinishPos:=nLastPos;
          nPendingRemoving:=false;
          RemoveItems;
         end
        else nItemsRemoving:=false;
       end
     else RemoveItems;
    end;
   end;
   nLinkedItem:=false;
   nPragmaOcc:=false;
  end;
 inherited Pop;
 DisplayLine(CurPos.Line,ErrorNbr);
end;

constructor InaccBlockObj.Init;
begin inherited Init(fBlockKind);
 nLinkedItem:=false;
 nItemsRemoving:=false;
 nPendingRemoving:=false;
 nPragmaOcc:=false;
 if fBlockKind = blMain then
  begin InaccItemsNbr:=0;
   InaccLinkNbr:=0;
   gInaccItems.Init(200,100);
  end;
 DisplayLine(CurPos.Line,ErrorNbr);
end;

procedure InaccBlockObj.ProcessPragma;
begin
 nPragmaOcc:=true;
 nPragmaPos:=CurPos;
 gDefinitionsRemoving:=false;
 RemoveItems;
end;

procedure InaccBlockObj.ProcessLink;
begin
 nLinkedItem:=(CurWord.Kind = sy_Hence) or (CurWord.Kind = sy_Then);
 gLinkPrevPos:=PrevPos;
 gBegLinkPos.Line:=CurPos.Line;
 gBegLinkPos.Col:=CurPos.Col-length(CurWord.Spelling)+1;
 gEndLinkPos.Line:=AheadPos.Line;
 gEndLinkPos.Col:=AHeadPos.Col-length(AheadWord.Spelling);
end;

procedure InaccBlockObj.CreateItem;
begin gItemPtr:=new(InaccItemPtr, Init(fItemKind)) end;

procedure InaccBlockObj.CreateBlock;
begin gBlockPtr:=new(InaccBlockPtr,Init(fBlockKind)) end;

procedure SaveEdtFile;
  var lPos:Position;
      gEdt: EdtCollection;
 procedure PrintCommand(Item:ErrDesPtr); far;
  begin
   with Item^ do
    if Code = 0 then lPos:=Pos
    else gEdt.Insert_d(lPos,Pos);
  end;
 var i,Depth,k: integer;
begin
  Depth:=0;
  for i:=0 to gInaccItems.Count-1 do
   with ErrDesPtr(gInaccItems.Items^[i])^ do
    if Code = 0 then
     begin
      if Depth > 0 then gInaccItems.Items^[i]:=nil;
      inc(Depth);
     end
    else
     begin dec(Depth);
      if Depth > 0 then gInaccItems.Items^[i]:=nil;
     end;
  gInaccItems.Pack;
  gEdt.Init(200,100);
  with gInaccItems do
   for k:=0 to Count-1 do PrintCommand(Items^[k]);
  if gEdt.Count > 0 then gEdt.StoreEDT(MizFileName);
  gEdt.Done;
  gInaccItems.Done;
end;

procedure InaccBlockObj.Pop;
begin
 RemoveItems;
 if (nBlockKind = blMain) and (gInaccItems.Count > 0) then SaveEdtFile;
 inherited Pop;
 DisplayLine(CurPos.Line,ErrorNbr);
end;

procedure InaccBlockObj.RemoveItems;
begin
 if nItemsRemoving then
  begin
   inc(InaccItemsNbr);
   inc(nStartPos.Col);
   Error(nStartPos,610);
   Error(nFinishPos,611);
   gInaccItems.Insert(new(ErrDesPtr,Init(nStartPos,0)));
   gInaccItems.Insert(new(ErrDesPtr,Init(nFinishPos,1)));
   nItemsRemoving:=false;
   nPendingRemoving:=false;
  end;
end;

end.

