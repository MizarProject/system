(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit labelact;

interface

uses mobjects,errhan,syntax;

type
 ProcessNode = procedure (fNodePtr: StackedPtr);
 MatchNode = function (fNodePtr: StackedPtr): boolean;

 StackPtr = ^StackObj;
 StackObj = object(MObject)
     Top: StackedPtr;
    constructor Init;
    procedure DeleteTo(N: StackedPtr);
    destructor Done; virtual;
    procedure Clear;
    procedure Mark(var N: StackedPtr);
    procedure Push(N: StackedPtr);
    procedure Append(var fStack: StackObj);
    function Pop: StackedPtr;
    procedure PopAndDelete;
    function isEmpty: boolean;
    function Entries: longint;
    function LastThat(M: MatchNode): StackedPtr;
    procedure ProcessTo(N: StackedPtr; P: ProcessNode);
    procedure ForEach(P: ProcessNode);
  end;

 IdentPtr = ^IdentObj;
 IdentObj = Object(StackedObj)
    Ident: string;
   constructor Init(const fIdent: string);
   destructor Done; virtual;
   procedure ProcessReference; virtual;
  end;

 LabIdItemPtr = ^LabIdItemObj;
 LabIdItemObj =
  object(ItemObj)
   procedure ProcessLabel; virtual;
   procedure ProcessDefiniensLabel; virtual;
   procedure ProcessPrivateReference; virtual;

   procedure InsertLabel; virtual;
   procedure InsertDefiniensLabel; virtual;
  end;

 LabIdBlockPtr = ^LabIdBlockObj;
 LabIdBlockObj =
  object(BlockObj)
    nLabMark: StackedPtr;
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
  end;

 LabelPtr = ^LabelObj;
 LabelObj = Object(IdentObj)
   Kind: ItemKind;
   constructor Init(const fIdent:string; fKind:ItemKind);
  end;

 LabelDescrPtr = ^LabelDescrObj;
 LabelDescrObj = Object(LabelObj)
   LabPos,LabBegPos,ColonPos,PrevWordPos: Position;
   RefNbr: Word;
   constructor Init(fIdent:string; fKind:ItemKind);
   procedure ProcessReference; virtual;
  end;

 LabItemPtr = ^LabItemObj;
 LabItemObj =
  object(LabIdItemObj)
   procedure InsertLabel; virtual;
   procedure InsertDefiniensLabel; virtual;
  end;

function LastLabel(const fIdent:string): IdentPtr;

var gLab,gDefLab: StackObj;

implementation

uses mscanner
{$IFDEF MDEBUG} ,info {$ENDIF};

constructor StackObj.Init;
begin
 Top:=nil;
end;

procedure StackObj.DeleteTo;
begin
 while Top <> N do PopAndDelete;
end;

destructor StackObj.Done;
begin
 DeleteTo(nil);
end;

procedure StackObj.Clear;
begin
 Top:=nil;
end;

procedure StackObj.Mark;
begin
 N:=Top;
end;

procedure StackObj.Push;
begin
 N^.Previous:=Top;
 Top:=N;
end;

procedure StackObj.Append;
 var L: StackedPtr;
begin L:=fStack.Top;
 if L = nil then exit;
 while L^.Previous <> nil do L:=L^.Previous;
 L^.Previous:=Top;
 Top:=fStack.Top;
 fStack.Clear;
end;

function StackObj.Pop;
begin Pop:=Top;
 if Top = nil then exit;
 Top:=Top^.Previous;
end;

procedure StackObj.PopAndDelete;
var p:StackedPtr;
begin if Top = nil then exit;
 p:=Pop;
 p^.Done;
end;

function StackObj.isEmpty;
begin
 isEmpty:=Top=nil;
end;

function StackObj.Entries;
 var L: StackedPtr; N: longint;
begin L:=Top; N:=0;
 while L <> nil do  begin inc(N); L:=L^.Previous end;
 Entries:=N;
end;

function StackObj.LastThat;
 var L: StackedPtr;
begin L:=Top;
 while L <> nil do
  begin
   if M(L) then begin LastThat:=L; exit end;
   L:=L^.Previous;
  end;
 LastThat:=nil;
end;

procedure StackObj.ProcessTo;
 var L: StackedPtr;
begin L:=Top;
 while L <> N do begin P(L); L:=L^.Previous end;
end;

procedure StackObj.ForEach;
begin
 ProcessTo(nil,P);
end;

constructor IdentObj.Init;
begin Previous:=nil;
 Ident:=fIdent;
end;

destructor IdentObj.Done;
begin
end;

procedure IdentObj.ProcessReference;
begin
end;

procedure LabIdItemObj.InsertLabel;
begin
   gLab.Push(new(IdentPtr,Init(CurWord.Spelling)));
end;

procedure LabIdItemObj.InsertDefiniensLabel;
begin
   gDefLab.Push(new(IdentPtr,Init(CurWord.Spelling)));
end;

procedure LabIdItemObj.ProcessLabel;
begin
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
  InsertLabel;
end;

procedure LabIdItemObj.ProcessDefiniensLabel;
begin
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
  InsertDefiniensLabel;
end;

function MatchCurLabel(N: StackedPtr): boolean;
 var lSpelling: string;
begin
 lSpelling:=CurWord.Spelling;
 MatchCurLabel:=lSpelling = IdentPtr(N)^.Ident;
end;

procedure LabIdItemObj.ProcessPrivateReference;
 var lLab: IdentPtr;
begin
 lLab:=IdentPtr(gLab.LastThat(MatchCurLabel));
 if lLab <> nil then lLab^.ProcessReference
  else Error(CurPos,144);
end;

constructor LabIdBlockObj.Init;
begin
 inherited Init(fBlockKind);
 gLab.Mark(nLabMark);
 case fBlockKind of
  blDefinition: gDefLab.Init;
  blMain: gLab.Init;
 end;
end;

procedure LabIdBlockObj.Pop;
begin
 gLab.DeleteTo(nLabMark);
 case nBlockKind of
  blDefinition: gLab.Append(gDefLab);
  blMain: gLab.Done;
 end;
 inherited Pop;
end;

constructor LabelObj.Init;
begin Previous:=nil;
 Ident:=fIdent;
 Kind:=fKind;
end;

constructor LabelDescrObj.Init;
begin Previous:=nil;
 Ident:=fIdent;
 Kind:=fKind;
 LabPos:=CurPos;
 LabBegPos.Line:=CurPos.Line;
 LabBegPos.Col:=CurPos.Col-length(CurWord.Spelling)+1;
 ColonPos:=AHeadPos;
 PrevWordPos:=PrevPos;
 RefNbr:=0;
end;

procedure LabelDescrObj.ProcessReference;
begin
 inc(RefNbr);
end;

procedure LabItemObj.InsertLabel;
begin
   gLab.Push(new(LabelDescrPtr,Init(CurWord.Spelling,nItemKind)));
end;

procedure LabItemObj.InsertDefiniensLabel;
begin
   gDefLab.Push(new(LabelDescrPtr,Init(CurWord.Spelling,nItemKind)));
end;

var MatchedLabel: string;
function MatchLabel(N: StackedPtr): boolean;
begin
 MatchLabel:=MatchedLabel = IdentPtr(N)^.Ident;
end;

function LastLabel;
begin MatchedLabel:=fIdent;
 LastLabel:=IdentPtr(gLab.LastThat(MatchLabel));
end;

var AuxLabNbr,AuxLabBase:Word;
procedure InitAuxiliaryLabels;
 var Lab: text;
begin
 assign(Lab,'auxlabs.doc');
 {$I-} reset(Lab); {$I+}
 AuxLabNbr:=0;
 if ioresult = 0 then begin readln(Lab,AuxLabNbr); close(Lab) end;
 AuxLabBase:=AuxLabNbr;
end;

procedure SaveAuxiliaryLabels;
 var Lab: text;
begin
 if AuxLabBase = AuxLabNbr then exit;
 assign(Lab,'auxlabs.doc');
 rewrite(Lab);
 writeln(Lab,AuxLabNbr);
 close(Lab);
end;

function GenAuxiliaryLabel: string;
 var Nr:string; lLab:string;
begin
  inc(AuxLabNbr);
  str(AuxLabNbr,Nr);
  lLab:='_'+Nr+'_';
  GenAuxiliaryLabel:=lLab;
end;
end.

