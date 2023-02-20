(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// Detects unnecessary premises in simple justifications

unit relpprep;

interface

uses mobjects,mconsole,justhan,correl,stdprep,mizprep,prepobj,
     prephan,propcoll;

procedure RelevantPremises(fBlock:MizPBlockPtr);
procedure FindRelPrem(fBlock:MizPBlockPtr);

type
 
 RelPremPBlockPtr = ^RelPremPBlockObj;
 RelPremPBlockObj = object(MizPBlockObj)
  function CreateItem(fItemKind:ItemKind):PrepItemPtr; virtual;
  procedure CreateBlock(fBlockKind:BlockKind); virtual;
  function GetPrevious : RelPremPBlockPtr;
 end;
 
 RelPremPItemPtr = ^RelPremPItemObj;
 RelPremPItemObj = object(MizPItemObj)
  function  GetPrevious : RelPremPItemPtr;
  function  GetBlock : RelPremPBlockPtr;
  procedure Justify(fInferencePtr:pointer); virtual;
 end;
  
implementation

uses errhan,mizenv,monitor,inout,limits,lexicon,
     inenviro,schemhan,schemes,checker
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

(********** Standard overloaded stuff **********)

function  RelPremPItemObj.GetPrevious : RelPremPItemPtr;
begin GetPrevious := RelPremPItemPtr(Previous); end;   

function  RelPremPItemObj.GetBlock : RelPremPBlockPtr;
begin GetBlock := RelPremPBlockPtr(nBlock); end;

function  RelPremPBlockObj.GetPrevious : RelPremPBlockPtr;
begin GetPrevious := RelPremPBlockPtr(Previous); end;   

function RelPremPBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm	:= new(RelPremPItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem	:= nCurrItm;
end;

procedure RelPremPBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr	:= new(RelPremPBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;  
end;

(***********************************************)

// tells whether the last inference was linked
var gLinked : boolean; 

// The checking procedure used in relprem.
// Tries to ommit each reference and find a proof without it.
procedure RelevantPremises(fBlock:MizPBlockPtr);
var i,r,e: integer; lReference: RefSntArr; b:byte;
begin
 InferenceChecker(Reference,PremNbr,fBlock^.nVarNbr);
 if ChErrNr.Count <> 0 then
  with ChErrNr do
   for e:=0 to Count-1 do ErrImm(Items^[e].X)
 else
  // try to skip the i-th reference, the 1st is the thesis
   for i:=2 to PremNbr do 
   begin
    for r:=1 to i-1 do lReference[r]:=Reference[r];
    for r:=i+1 to PremNbr do lReference[r-1]:=Reference[r];
    ChErrNr.DeleteAll;
    InferenceChecker(lReference,PremNbr-1,fBlock^.nVarNbr);
    if ChErrNr.Count = 0 then
     if (i = 2) and gLinked then Error(RefPos[i],603)
     else Error(RefPos[i],602);
{    with ChErrNr do
     for e:=0 to Count-1 do
     begin
      b:=byte(Items^[e].X);
      if not (b in [1,4,19]) then
       ErrImm(Items^[e].X);
     end;
}     
   end;
end;

  // The checking procedure used in chkrprem.
  // A greedy version of RelevantPremises, which tries to
  // find a maximal set of references that are simultaneously
  // unnecessary.
//procedure FindRelPrem(fBlock:MizPBlockPtr);
//var i,r,lPremNbr: integer; lReference,wReference: RefSntArr;
//begin
// lReference:=Reference; wReference:=Reference;
// lPremNbr:=PremNbr;
// for i:=PremNbr downto 2 do
// begin
  // Skip the i-th reference by using smaller PremNbr
  // and shifting the reference array above i.
  // If checker succeeds, decrease PremNbr again, and
  // continue trying skipping references smaller than i.
//  for r:=i+1 to lPremNbr do wReference[r-1]:=lReference[r];
//  ChErrNr.DeleteAll;
//  InferenceChecker(wReference,lPremNbr-1,fBlock.nVarNbr);
//  if ChErrNr.Count = 0 then
//  begin
//   if (i=2) and gLinked then
//    Error(RefPos[2],603)
//   else Error(RefPos[i],602);
//   dec(lPremNbr);
//   lReference:=wReference;
//  end;
// end;
//end;

// RM - New implementation of procedure used in chkrprem.
procedure FindRelPrem(fBlock:MizPBlockPtr);
var i,r,k,lPremNbr: integer; wReference: RefSntArr;
    bReference: array[1..MaxPremNbr] of Boolean;
begin
 lPremNbr:=PremNbr;
 for i:=1 to PremNbr do bReference[i]:=true;
 for i:=PremNbr downto 2 do
 begin
  bReference[i]:=false;
  k:=1;
  for r:=1 to PremNbr do if bReference[r] then
  begin
   wReference[k]:=Reference[r];
   inc(k);
  end;
  ChErrNr.DeleteAll;
  InferenceChecker(wReference,lPremNbr-1,fBlock^.nVarNbr);
  if ChErrNr.Count = 0 then
  begin
   if (i=2) and gLinked then
    Error(RefPos[2],603)
   else Error(RefPos[i],602);
   dec(lPremNbr);
  end
  else bReference[i]:=true;
 end;
end;

// ignore scheme inferences
procedure RelPremPItemObj.Justify(fInferencePtr:pointer);
begin
 if ChInferPtr(fInferencePtr)^.nInferSort = ikInfBy then
 begin
  gLinked := ChInferPtr(fInferencePtr)^.nLinked;
  inherited Justify(fInferencePtr);
 end
 else Propositions^.GetCurrProp^.nSimpleJustif := fInferencePtr;
end;

end.
