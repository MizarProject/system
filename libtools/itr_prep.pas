(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// Detects irrelevant iterative steps

unit itr_prep;

interface

uses mobjects,mconsole,justhan,correl,stdprep,mizprep,prepobj,
     prephan,propcoll;

type

 IterEqPBlockPtr = ^IterEqPBlockObj;
 IterEqPBlockObj = object(MizPBlockObj)
  function CreateItem(fItemKind:ItemKind):PrepItemPtr; virtual;
  procedure CreateBlock(fBlockKind:BlockKind); virtual;
  function GetPrevious : IterEqPBlockPtr;
 end;

 IterEqPItemPtr = ^IterEqPItemObj;
 IterEqPItemObj = object(MizPItemObj)
  function  GetPrevious : IterEqPItemPtr;
  function  GetBlock : IterEqPBlockPtr;
  procedure Justify(fInferencePtr:pointer); virtual;
  procedure FinishIterEquality; virtual;
  procedure FinishSimpleJustification; virtual;
 end;

// If false, the step following an irrelevant step
// is not checked.
var gFullChecking: boolean = false;

// This is used instead of the ClassicalChecker
procedure IterEqChecker(fBlock:MizPBlockPtr);

implementation

uses errhan,mizenv,monitor,inout,limits,inenviro,schemhan,schemes,
     checker,lexicon
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

(********** Standard overloaded stuff **********)

function  IterEqPItemObj.GetPrevious : IterEqPItemPtr;
begin GetPrevious := IterEqPItemPtr(Previous); end;

function  IterEqPItemObj.GetBlock : IterEqPBlockPtr;
begin GetBlock := IterEqPBlockPtr(nBlock); end;

function  IterEqPBlockObj.GetPrevious : IterEqPBlockPtr;
begin GetPrevious := IterEqPBlockPtr(Previous); end;   

function IterEqPBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm	:= new(IterEqPItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem	:= nCurrItm;
end;

procedure IterEqPBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr	:= new(IterEqPBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;  
end;

(***********************************************)


// Passes the info that the previous step was irrlevant
var gIrrStepFound: boolean = false;

// This is used instead of the ClassicalChecker
procedure IterEqChecker(fBlock:MizPBlockPtr);
begin
 InferenceChecker(Reference,PremNbr,fBlock^.nVarNbr);
 if ChErrNr.Count = 0 then
 begin
  Error(CurPos,746);
  gIrrStepFound := true;
 end;
end; 
 
// Ignore normal simple justifs. this means that we also
// do not collect the simplejustifs for such propositions
procedure IterEqPItemObj.FinishSimpleJustification; begin end; 

procedure IterEqPItemObj.Justify(fInferencePtr:pointer);
begin
 if gIrrStepFound then
 begin
  gIrrStepFound := false;
  if gFullChecking then inherited Justify(fInferencePtr);
 end
 else inherited Justify(fInferencePtr);
end;

// Combine references of two subsequent steps and try
// to prove the equality without the middle term.
procedure IterEqPItemObj.FinishIterEquality;
var
 lLeft,lRight	: TrmPtr;
 lFrm		: FrmPtr;
 i,lPremNbr 	: integer;
 lProp		: PrepProposPtr;
 lCurStep,
 lNextStep	: IterStepPtr;
begin
 gIrrStepFound	:= false;
 lLeft		:= GetBlock^.nLeftSide;
 
 with  gPrep^.nLastIterSteps^ do
 begin
  for i:= 0 to Count - 2 do
  begin
   lCurStep    	:= IterStepPtr(Items^[i]);
   lNextStep   	:= IterStepPtr(Items^[i+1]);
   lCurStep^.nReferences.CopyItems(lNextStep^.nReferences);
   
   lCurStep^.nInferSort	:= ikInfBy;  // to avoid RunTimeError for schemes
   lRight      	:= lNextStep^.nEquatedTrm;
   lFrm		:= NewEqFrm(CopyTerm(lLeft), CopyTerm(lRight));
   lProp	:= new(PrepProposPtr,
                       Init( 0, lFrm, lCurStep^.nPos, propIterStep));
   
   Propositions^.InsertUnlabeled(lProp);
   
   lPremNbr	:= lCurStep^.nReferences.Count + 1;
   if lCurStep^.nLinked then inc(lPremNbr);
   if lPremNbr  <= MaxPremNbr then
    Justify(ChInferPtr(Items^[i]));
   
   lLeft	:= lCurStep^.nEquatedTrm;
  end;
  
  lLeft		:= GetBlock^.nLeftSide;
  lFrm 		:= NewEqFrm(lLeft, CopyTerm(lRight));
  lProp		:= new(PrepProposPtr, Init( nItemLab, lFrm, lNextStep^.nPos,
                                            propIterResult));
 end;
 
 if nItemLab <> 0 then Propositions^.InsertLabeled(lProp)
 else Propositions^.InsertUnlabeled(lProp);
 
 gPrep^.nLastIterSteps^.DeleteAll;  // justifications now kept in props
 GetBlock^.nLeftSide := nil; // safety
end; 

end.
