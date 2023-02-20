(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit td_prep;

interface

uses mobjects,errhan,justhan,correl,mizprep,prepobj,propcoll;

type

 TDemoPBlockPtr = ^TDemoPBlockObj;
 TDemoPBlockObj = object(MizPBlockObj)
  nExtLabs: MCollection;
  constructor Init(fBlockKind:BlockKind);
  destructor Done; virtual;
  function CreateItem(fItemKind:ItemKind):PrepItemPtr; virtual;
  procedure CreateBlock(fBlockKind:BlockKind); virtual;
  function GetPrevious : TDemoPBlockPtr;
  procedure FinishQuotableBlock; virtual;
 end;

 TDemoPItemPtr = ^TDemoPItemObj;
 TDemoPItemObj = object(MizPItemObj)
  function  GetPrevious : TDemoPItemPtr;
  function  GetBlock : TDemoPBlockPtr;
  procedure Justify(fInferencePtr:pointer); virtual;
 end;

 TDemoInferPtr = ^TDemoInferObj;
 TDemoInferObj = object(ChInferObj)
   procedure Justify(fBlock:TDemoPBlockPtr);
 end;

var gChkDiffuse: boolean = true;

var
   gEdt			  : text;
   gStartPos, gEndPos, gI : MCollection;
{$IFDEF TRIVDEMO}
   gLabsPos		  : integer;
   gLabs		  : array[1..10000] of record Nr : integer; Pos: Position; Lab: string; end;
{$ENDIF}

implementation

uses mconsole,mizenv,monitor,limits,checker,envhan
{$IFDEF TRIVDEMO}
     ,mscanner,edt_han,block_and_item,first_identification, wsmarticle
{$ENDIF}
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

(********** Standard overloaded stuff **********)

function  TDemoPItemObj.GetPrevious : TDemoPItemPtr;
begin GetPrevious := TDemoPItemPtr(Previous); end;

function  TDemoPItemObj.GetBlock : TDemoPBlockPtr;
begin GetBlock := TDemoPBlockPtr(nBlock); end;

{$IFDEF TRIVDEMO}
type
  TDArticlePtr = ^TDArticleObj;
  TDArticleObj = object(ProcessingArticleObj)
   constructor Init(aWSTextProper:WSTextProperPtr);
   destructor Done; virtual;
   procedure Process_PrivateReference(var aRef: LocalReferencePtr); virtual;
  end;

destructor TDArticleObj.Done;
begin
 inherited Done;
end;

constructor TDArticleObj.Init(aWSTextProper:WSTextProperPtr);
begin
 inherited Init(aWSTextProper);
end;

procedure TDArticleObj.Process_PrivateReference(var aRef: LocalReferencePtr);
begin
 Inc(gLabsPos);
 with MSLocalReferencePtr(aRef)^ do
  begin
   gLabs[gLabsPos].Nr:=nLabNr;
   gLabs[gLabsPos].Pos:=nRefPos;
   gLabs[gLabsPos].Lab:=IdentRepr(nLabId);
  end;
end;
{$ENDIF}

function  TDemoPBlockObj.GetPrevious : TDemoPBlockPtr;
begin GetPrevious := TDemoPBlockPtr(Previous);
end;

function TDemoPBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm	:= new(TDemoPItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem	:= nCurrItm;
end;

procedure TDemoPBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr	:= new(TDemoPBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;
end;

(***********************************************)

constructor TDemoPBlockObj.Init(fBlockKind:BlockKind);
{$IFDEF TRIVDEMO}
 var lWSTextProper: wsTextProperPtr;
     lMizArticle: ProcessingArticlePtr;
{$ENDIF}
begin
 inherited Init(fBlockKind);
 nExtLabs.Init(0,10);
 if fBlockKind = blMain then
 begin
{$IFDEF TRIVDEMO}
    lWSTextProper:=Read_MSMizArticle((EnvFileName+'.msx'));
    lMizArticle:=new(TDArticlePtr,Init(lWSTextProper));
    lMizArticle^.nDisplayInformationOnScreen:=true;
    lMizArticle^.Process_Article;
    dispose(lMizArticle,Done);
    Env.LoadEvl(MizFileName+'.evl');
{$ENDIF}
    gStartPos.Init(0,10);
    gEndPos.Init(0,10);
    gI.Init(0,10);
 end;
end;

destructor TDemoPBlockObj.Done;
begin
 nExtLabs.Done;
{$IFDEF TRIVDEMO}
 if nBlockKind = blMain then
    Env.Done;
{$ENDIF}
 inherited Done;
end;

{$IFDEF TRIVDEMO}
function Pos1IncludedIn2(const Pos1, Pos2 : Position):boolean;
begin
   if Pos1.Line > Pos2.Line then Pos1IncludedIn2:=true
   else if Pos1.Line < Pos2.Line then Pos1IncludedIn2:=false
   else if Pos1.Col > Pos2.Col then Pos1IncludedIn2:=true
   else if Pos1.Col < Pos2.Col then Pos1IncludedIn2:=false
   else Pos1IncludedIn2:=false;
end;

procedure AddInferToEdt(aStartPos, aEndPos : Position; AddInfer: string);
var
   i : integer;
begin
   i:=gStartPos.Count-1;
   while (i >= 0) and Pos1IncludedIn2(PPosition(gStartPos.Items^[i])^.Pos,aStartPos) do
   begin
      gStartPos.AtFree(i);
      gEndPos.AtFree(i);
      gI.AtFree(i);
      dec(i);
   end;
   gStartPos.Insert(new(PPosition,Init(aStartPos)));
   gEndPos.Insert(new(PPosition,Init(aEndPos)));
   gI.Insert(new(MStrPtr,Init(AddInfer)));
end; { AddInferToEdt }

function FindLab(aPos : Position): string;
var
   i : integer;
begin
   for i:=gLabsPos downto 1 do
      if (gLabs[i].Pos.Line = aPos.Line) and (gLabs[i].Pos.Col = aPos.Col) then
      begin
	 FindLab:=gLabs[i].Lab;
	 break;
      end;
end; { FindLab }

procedure Edit(aExtLabs: MCollection; aStartPos, aEndPos:Position);
var
   fStartPos : Position;
   xstr	     : string;
   AddInfer  : string;
   i	     : integer;
begin
   AddInfer:='';
   
   fStartPos:=aStartPos;
   Dec(fStartPos.Col,4); //proof
   with aExtLabs do
   begin
      if Count > 0 then
      begin
	 if typeof(PReference(aExtLabs.Items^[0])^) = typeof(TPrivateReference) then
	    AddInfer:=Concat(AddInfer,FindLab(PPrivateReference(aExtLabs.Items^[0])^.RefPos))
	 else
	    if typeof(PReference(aExtLabs.Items^[0])^) = typeof(TDefinitionReference) then
	    begin
	       str(PDefinitionReference(aExtLabs.Items^[0])^.DefNr,xstr);
	       AddInfer:=Concat(AddInfer,PImpArticleId(Env.Directive[syTheorems].Items[PDefinitionReference(aExtLabs.Items^[0])^.ArticleNr-1])^.fStr,
				':def ',xstr);
	    end else
	       if typeof(PReference(aExtLabs.Items^[0])^) = typeof(TTheoremReference) then
	       begin
		  str(PTheoremReference(aExtLabs.Items^[0])^.TheoNr,xstr);
		  AddInfer:=Concat(AddInfer,PImpArticleId(Env.Directive[syTheorems].Items[PTheoremReference(aExtLabs.Items^[0])^.ArticleNr-1])^.fStr,
				   ':',xstr);
	       end;
	 for i:=1 to Count-1 do
	    if typeof(PReference(aExtLabs.Items^[i])^) = typeof(TPrivateReference) then
	       AddInfer:=Concat(AddInfer,',',FindLab(PPrivateReference(aExtLabs.Items^[i])^.RefPos))
	    else
	       if typeof(PReference(aExtLabs.Items^[i])^) = typeof(TDefinitionReference) then
	       begin
		  str(PDefinitionReference(aExtLabs.Items^[i])^.DefNr,xstr);
		  AddInfer:=Concat(AddInfer,',',PImpArticleId(Env.Directive[syTheorems].Items[PDefinitionReference(aExtLabs.Items^[i])^.ArticleNr-1])^.fStr,
				   ':def ',xstr);
	       end else
		  if typeof(PReference(aExtLabs.Items^[i])^) = typeof(TTheoremReference) then
		  begin
		     str(PTheoremReference(aExtLabs.Items^[i])^.TheoNr,xstr);
		     AddInfer:=Concat(AddInfer,',',PImpArticleId(Env.Directive[syTheorems].Items[PTheoremReference(aExtLabs.Items^[i])^.ArticleNr-1])^.fStr,
				      ':',xstr);
		  end;
      end; {if}
   end; {with}
   AddInferToEdt(fStartPos,aEndPos,AddInfer);
end;
{$ENDIF}

// ##NOTE: Scheme proofs were ignored in the old version,
//         so we do it too.

procedure TDemoPBlockObj.FinishQuotableBlock;
var i	     : integer; lThesis: FrmPtr;  
begin	     
 inherited FinishQuotableBlock;
 if ((not gChkDiffuse) and (nBlockKind = blDiffuse))
    or (GetPrevious^.nBlockKind = blPublicScheme)
 then exit;

 PremNbr	:= 0;
 InferOK	:= true;
 lThesis	:= NewNeg( Propositions^.GetCurrProp^.nSentence);
 InsertRef(nStartPos, lThesis);
 
 with nExtLabs do
 if Count < MaxPremNbr then
  for i:=0 to Count-1 do PReference(Items^[i])^.CollectRef1;

 if InferOK then
 begin
  ChErrNr.Init(10,10);
  InferenceChecker(Reference,PremNbr, GetPrevious^.nVarNbr);
  if ChErrNr.Count = 0 then
  begin   
     Error(nStartPos,607);
{$IFDEF TRIVDEMO}
     if nBlockKind <> blDiffuse then Edit(nExtLabs,nStartPos,CurPos);
{$ENDIF}
  end;  
  ChErrNr.Done;
 end;
 if lThesis^.FrmSort = #170 then dispose(lThesis);
 
end;

procedure TDemoPItemObj.Justify(fInferencePtr:pointer);
begin
 TDemoInferPtr(fInferencePtr)^.Justify(GetBlock);
 Propositions^.GetCurrProp^.nSimpleJustif := fInferencePtr;
end;

procedure TDemoInferObj.Justify(fBlock:TDemoPBlockPtr);
procedure CollectExternalRef(Item:PReference);
var lOccurrs: boolean;  var j: integer;
 begin
  with fBlock^.nExtLabs do
  begin
   lOccurrs := false;
   for j:= 0 to Count-1 do
    if EqRefs(Item, PReference(Items^[j])) 
    then begin lOccurrs:= true; break; end;
   
   if (not lOccurrs )
      and ( (typeof(Item^) <> typeof(TPrivateReference) )
            or not (fBlock^.IntroducedLabel(PPrivateReference(Item)^.LabNr) ))
   then Insert(Item^.CopyObject);
  end;
 end;
var i: integer;
begin CurPos:=nPos;
  DisplayLine(CurPos.Line,ErrorNbr);
  case nInferSort of
   '''','"' :
     with nReferences do
      for i:=0 to Count-1 do CollectExternalRef(Items^[i]);
  end;
end;

end.
