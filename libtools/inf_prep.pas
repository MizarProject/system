(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// This unit tries in each "by" inference to replace each
// private reference with its own references.

unit inf_prep;

interface

uses mobjects,justhan,correl,stdprep,prephan,prepobj,mizprep;

var gCheckLinkedRefs 	: boolean = true; // expand linked references
var gCheckNonLinkedRefs	: boolean = true; // ex[and normal references

type
 
 InfPBlockPtr = ^InfPBlockObj;
 InfPBlockObj = object(MizPBlockObj)
  function CreateItem(fItemKind:ItemKind):PrepItemPtr; virtual;
  procedure CreateBlock(fBlockKind:BlockKind); virtual;
  function GetPrevious : InfPBlockPtr;
 end;
 
 InfPItemPtr = ^InfPItemObj;
 InfPItemObj = object(MizPItemObj)
  function  GetPrevious : InfPItemPtr;
  function  GetBlock : InfPBlockPtr;
  procedure Justify(fInferencePtr:pointer); virtual;
 end;
  
 SimpleJustInferPtr = ^SimpleJustInferObj;
 SimpleJustInferObj = object(ChInferObj)
  procedure Justify(fBlock:MizPBlockPtr);
 end;

implementation

uses mconsole,errhan,mizenv,monitor,inout,limits,lexicon,
     inenviro,schemhan,schemes,checker,propcoll
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF};


(********** Standard overloaded stuff **********)

function  InfPItemObj.GetPrevious : InfPItemPtr;
begin GetPrevious := InfPItemPtr(Previous); end;   

function  InfPItemObj.GetBlock : InfPBlockPtr;
begin GetBlock := InfPBlockPtr(nBlock); end;

function  InfPBlockObj.GetPrevious : InfPBlockPtr;
begin GetPrevious := InfPBlockPtr(Previous); end;   

function InfPBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm	:= new(InfPItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem	:= nCurrItm;
end;

procedure InfPBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr	:= new(InfPBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;  
end;

(***********************************************)

procedure InfPItemObj.Justify(fInferencePtr:pointer);
begin
 SimpleJustInferPtr(fInferencePtr)^.Justify(GetBlock);
 Propositions^.GetCurrProp^.nSimpleJustif := fInferencePtr;
end;


// Try to replace private refs with their own refs.
// Report Error 604 (resp. 605 for linked ref) for each success.
procedure SimpleJustInferObj.Justify(fBlock:MizPBlockPtr);

procedure CheckIt(fPos:Position; fError: integer);
begin
 if not InferOK then exit;
 ChErrNr.Init(10,10);
 InferenceChecker( Reference, PremNbr, fBlock^.nVarNbr);
 if ChErrNr.Count = 0 then Error(fPos, fError);
 ChErrNr.Done;
end;

var
 lThesis		: FrmPtr;
 lInf			: InferencePtr;
 lRef			: PPrivateReference;
 i,z,lPremNbr,llPremNbr	: integer;
begin
 DisplayLine(CurPos.Line,ErrorNbr); 
 lThesis	:= NewNeg(Propositions^.GetCurrProp^.nSentence);
  
 // expanding the linked reference - only if link has simple justif.
 if gCheckLinkedRefs and nLinked
    and Assigned(Propositions^.GetPrevProp^.nSimpleJustif) then
 begin
  lInf		:= Propositions^.GetPrevProp^.nSimpleJustif;  
  lPremNbr 	:= lInf^.nReferences.Count;

						
  if lInf^.nLinked then inc(lPremNbr);
  
  if nReferences.Count + lPremNbr < MaxPremNbr then
   begin
    PremNbr	:= 0; InferOK	:= true; InsertRef(nPos,lThesis);
    if lInf^.nLinked then  	// add the link's own link
     InsertRef(nPos,Propositions^.GetNthPrevQuotable(2)^.nSentence);
    CollectRefs;			// add normal refs
    ChInferPtr(lInf)^.CollectRefs; 	// add the expanded
    CheckIt(nPos, 605);
   end;
 end;
 
 // expanding normal (nonlinked) references
 if gCheckNonLinkedRefs then with nReferences do
 begin
 
  // added thesis, one less - is expanded, 0-based to 1-based 
  lPremNbr := nReferences.Count;   
  if nLinked then inc(lPremNbr);
    
  for i:= 0 to Count - 1 do
   if typeof(PReference(Items^[i])^) = typeof(TPrivateReference) then
   begin
    lRef       	:= PPrivateReference(Items^[i]);
    lInf       	:= Propositions^.GetLabeled(lRef^.LabNr)^.nSimpleJustif;

    if nInferSort=ikInfFrom then continue; // skipping schemes
    if not Assigned(lInf) then continue;  // the ref has no simple justif
    
    llPremNbr	:= lPremNbr;
    if lInf^.nLinked then inc(llPremNbr);
    
    if llPremNbr + lInf^.nReferences.Count <= MaxPremNbr then
    begin
     PremNbr	:= 0; InferOK	:= true; InsertRef(nPos,lThesis);
     
     // insert my refs without the ommited
     if nLinked then
      InsertRef(nPos,Propositions^.GetPrevProp^.nSentence);
     for z:=0 to Count-1 do if (z <> i) then
      PReference(Items^[z])^.CollectRef1;
     
     // insert the lInf's references
     if lInf^.nLinked then  	// add the link's own link
      InsertRef(nPos,
                Propositions^.FirstQuotableBefore(lRef^.LabNr)^.nSentence);
     ChInferPtr(lInf)^.CollectRefs;  		// add the expanded
     CheckIt(lRef^.RefPos, 604);
    end;
   end;
 end;
 
 if lThesis^.FrmSort = ikFrmNeg then dispose(lThesis);
 
end;

end.
