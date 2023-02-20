(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit irlabact;

interface

procedure InitArticle;

var IrrLabNbr: integer;

implementation

uses mconsole,mobjects,limits,errhan,mizenv,monitor,mscanner,syntax,edt_han
{$IFDEF MDEBUG} ,info {$ENDIF};

type

 IrrLabItemPtr = ^IrrLabItemObj;
 IrrLabItemObj =
  object(ItemObj)
   constructor Init(fKind:ItemKind);
   procedure Pop; virtual;
   procedure ProcessLabel; virtual;
   procedure ProcessDefiniensLabel; virtual;
   procedure ProcessPrivateReference; virtual;
  end;

 IrrLabBlockPtr = ^IrrLabBlockObj;
 IrrLabBlockObj =
  object(BlockObj)
    nLabNbr:integer;
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
  end;

 LabelDescr =
  record rName:string;
   LabPos,BegPos,EndPos: Position;
   RefNbr: Word;
  end;

var
  gLabNbr,gDefLabNbr: integer;
  Lab: array[1..MaxLabNbr] of LabelDescr;
  DefLab: array[1..MaxLabNbr] of LabelDescr;
  gEDT: EdtCollection;

procedure InitArticle;
begin
 gBlockPtr:=new(IrrLabBlockPtr, Init(blMain));
end;

constructor IrrLabItemObj.Init;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Init(fKind);
end;

procedure IrrLabItemObj.Pop;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Pop;
end;

constructor IrrLabBlockObj.Init;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Init(fBlockKind);
 nLabNbr:=gLabNbr;
 case fBlockKind of
  blDefinition: gDefLabNbr:=0;
  blMain:
   begin
    IrrLabNbr:=0; gLabNbr:=0;
    gEDT.Init(200,100);
   end;
 end;
end;

procedure IrrLabBlockObj.Pop;
 var i: integer;
begin
 for i:=nLabNbr+1 to gLabNbr do with Lab[i] do
  if RefNbr=0 then
   begin Error(LabPos,601); inc(IrrLabNbr);
    gEDT.Insert_d(BegPos,EndPos);
   end;
 DisplayLine(CurPos.Line,ErrorNbr);
 gLabNbr:=nLabNbr;
 case nBlockKind of
  blDefinition:
   for i:=1 to gDefLabNbr do
    begin IncIndex(gLabNbr,LabIndex); Lab[gLabNbr]:=DefLab[i] end;
  blDiffuse,blProof,blCase,blSuppose:;
  blMain:
   begin
    if IrrLabNbr > 0 then gEDT.StoreEDT(MizFileName);
    gEDT.Done
   end;
 end;
 inherited Pop;
end;

procedure IrrLabBlockObj.CreateItem;
begin gItemPtr:=new(IrrLabItemPtr, Init(fItemKind)) end;

procedure IrrLabBlockObj.CreateBlock;
begin gBlockPtr:=new(IrrLabBlockPtr,Init(fBlockKind)) end;

procedure IrrLabItemObj.ProcessLabel;
begin
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
  begin  IncIndex(gLabNbr,LabIndex);
   with Lab[gLabNbr] do
   begin rName:=CurWord.Spelling; LabPos:=CurPos; RefNbr:=0;
     BegPos.Line:=CurPos.Line;
     BegPos.Col:=CurPos.Col-length(rName)+1;
     EndPos:=AheadPos;
    end;
  end;
end;

procedure IrrLabItemObj.ProcessDefiniensLabel;
begin
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
  begin  IncIndex(gDefLabNbr,LabIndex);
   with DefLab[gDefLabNbr] do
    begin rName:=CurWord.Spelling; LabPos:=CurPos; RefNbr:=0;
     BegPos:=PrevPos; EndPos:=AHeadPos;
    end;
  end;
end;

procedure IrrLabItemObj.ProcessPrivateReference;
 var k:integer; lSpelling:String;
begin lSpelling:=CurWord.Spelling;
 for k:=gLabNbr downto 1 do
  if Lab[k].rName = lSpelling then
   begin
    inc(Lab[k].RefNbr);
    exit;
   end;
 Error(CurPos,144);
end;

end.
