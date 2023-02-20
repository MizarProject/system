(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

//  Implements a collection of propositions available for
//  checker.

unit propcoll;

interface

uses mobjects,correl,errhan,justhan;

type
 
 PropositionKind = (
  propUnknown,
  propAssumption,
  propExistentialAssumption, // unquotable
  propExistentialAssumptionResult,
  propChoiceJustification, // unquotable  
  propChoiceResult,
  propReconsideringJustification, // unquotable  
  propCase,
  propSuppose,
  propPerCasesJustification, // unquotable  
  propNormalLemma,
  propTheorem,
  propDefTheorem,
// all properties unquotable
  propUnexpectedProperty,
  propSymmetryProperty,
  propReflexivityProperty,
  propIrreflexivityProperty,
  propAssociativityProperty,
  propTransitivityProperty,
  propCommutativityProperty,
  propConnectednessProperty,
  propAsymmetryProperty,
  propIdempotenceProperty,
  propInvolutivenessProperty,
  propProjectivityProperty,
  propSethoodProperty,

  propCorrCondUnknown,
  propCorrCondCoherence,
  propCorrCondCompatibility,
  propCorrCondConsistency,
  propCorrCondCxistence,
  propCorrCondUniqueness,
  propCorrCondReducibility,
  propCorrectnessConjunction,

  propSchemeThesis, // unquotable
  propSchemePremise,

  propDiffuseResult,
  propIterStep, // unquotable  
  propIterResult
                          );
 
const UnquotablePropKinds = [
 propExistentialAssumption,
 propChoiceJustification,
 propReconsideringJustification,
 propPerCasesJustification,
 
 propUnexpectedProperty,
 propSymmetryProperty,
 propReflexivityProperty,
 propIrreflexivityProperty,
 propAssociativityProperty,
 propTransitivityProperty,
 propCommutativityProperty,
 propConnectednessProperty,
 propAsymmetryProperty,
 propIdempotenceProperty,
 propInvolutivenessProperty,
 propProjectivityProperty,
 propSethoodProperty,

 propCorrCondUnknown,
 propCorrCondCoherence,
 propCorrCondCompatibility,
 propCorrCondConsistency,
 propCorrCondCxistence,
 propCorrCondUniqueness,
 propCorrCondReducibility,
 propCorrectnessConjunction,

 propSchemeThesis,
 propIterStep
                            ];


const PropertyPropKinds =
       [ propUnexpectedProperty ..  propSethoodProperty];

const CorrCondPropKinds =
       [  propCorrCondUnknown ..  propCorrectnessConjunction];

type
// Propositions used in preparator
 PrepProposPtr = ^PrepProposObj;
 PrepProposObj = object(PropositionObj)
  nKind: PropositionKind;
  nSimpleJustif: InferencePtr;    // remembers its simple justification
                                   // if given, or nil
  nUserData: PObject;         // whatever you need
  constructor Init(fLabNr:integer; fSentence:FrmPtr;
                   fPos:Position; fKind:PropositionKind);
  destructor Done; virtual;
 end;
  
// The default list contains both labeled and unlabeled,
// in their order in article. There are NO nils for blocks,
// we always keep accessible propositions only.
// The current block's label is reserved using
// gUndefinedLab as value of the Labeled field.
// Items^[Labeled[i]] contains proposition with label i.
 PropListPtr= ^MPropList;
 MPropList=
  object(MList)
   Labeled:IntSequence; // indeces in Items
   constructor Init(aLimit: Integer);
   destructor Done; virtual;
   procedure InsertUnlabeled(aItem : Pointer); virtual;
   procedure InsertLabeled(aItem : Pointer); virtual;
   procedure SkipBlockLabel; virtual;
   function ExtractCurrent: PrepProposPtr; virtual;
   function GetLabeled(aIndex:integer): PrepProposPtr; virtual;
   function GetByOrder(aIndex:integer): PrepProposPtr;
   function GetLastLab: integer; virtual;
   // This is private! fWhere is index to Items not a label number!
   function IndexOfQuotableBefore(fWhere:integer):integer; virtual;
   function FirstQuotableBefore(fLab:integer): PrepProposPtr; virtual;
   function GetNthPrevQuotable(fOffset:integer): PrepProposPtr; virtual;
   function IndexOfPrev: integer; virtual;
   function GetPrevProp: PrepProposPtr; virtual;
   function GetCurrProp: PrepProposPtr; virtual;
   procedure SetCurrProp(aItem : Pointer); virtual;
   procedure FreePropsAndLabs(aIndex,aLab:integer); virtual;
  end;
 
  var Propositions: PropListPtr;
  
  const gUndefinedLab = -1;
  
implementation

var gIncorrectProp: PrepProposPtr;

constructor PrepProposObj.Init(fLabNr:integer; fSentence:FrmPtr;
                               fPos:Position; fKind:PropositionKind);
begin
 inherited Init(fLabNr, 0, fSentence, fPos);
 nKind:= fKind;
 nSimpleJustif:= nil;
 nUserData:= nil;
end;

destructor PrepProposObj.Done;
begin
 if Assigned(nSentence) then dispose(nSentence,Done);
 if Assigned(nSimpleJustif) then dispose(nSimpleJustif,Done);
 if Assigned(nUserData) then dispose(nUserData,Done);
 MObject.Done;
end; 

constructor MPropList.Init(aLimit: Integer);
begin
 inherited Init(aLimit);
 Labeled.Init(aLimit);
 Labeled.Insert(gUndefinedLab); // the 0th position is invalid
end; 

destructor MPropList.Done;
begin
 Labeled.Done;
 inherited Done;
end; 

procedure MPropList.InsertUnlabeled(aItem : Pointer);
begin Insert(aItem); end;

procedure MPropList.InsertLabeled(aItem : Pointer);
begin
 InsertUnlabeled(aItem);
 Labeled.Insert(Count-1);
end;

// the numbering is not contiguous - block label
// precedes labels inside the block
procedure MPropList.SkipBlockLabel;
begin Labeled.Insert(gUndefinedLab); end;

function MPropList.ExtractCurrent: PrepProposPtr;
begin
 ExtractCurrent := Items^[Count-1];
 dec(Count);
 if Labeled.Value(Labeled.fCount-1) = Count then
  dec(Labeled.fCount);
end;

function MPropList.GetByOrder(aIndex:integer): PrepProposPtr;
begin GetByOrder := At(aIndex); end;

// ##TODO: labnr=0 seems to be used as a fallback in anal for
//         bad references. I now return the gIncorrectProp,
//         but such inferences should rather be marked as bad in anal,
//         and ignored completely.
function MPropList.GetLabeled(aIndex:integer): PrepProposPtr;
begin
 if aIndex = 0 then Getlabeled := gIncorrectProp
 else GetLabeled:= GetByOrder(Labeled.Value(aIndex));
end;

function MPropList.GetLastLab: integer;
begin GetLastLab:= Labeled.fCount - 1; end;


// This is private, fWhere is index to Items not a label number!
function MPropList.IndexOfQuotableBefore(fWhere:integer):integer;
begin
 dec(fWhere);
 if fWhere < 0 then fWhere := 0;   // be nice to losers
 while (fWhere > 0) and ( GetByOrder(fWhere)^.nKind in UnquotablePropKinds )
 do dec(fWhere);
 IndexOfQuotableBefore := fWhere;
end;

function MPropList.FirstQuotableBefore(fLab:integer): PrepProposPtr;
begin
 FirstQuotableBefore := GetByOrder(IndexOfQuotableBefore(
                                    Labeled.Value(fLab)));
end;

// fOffset is counted from backwards; we are skipping unquotables
// and ignoring fOffset = 0
function MPropList.GetNthPrevQuotable(fOffset:integer): PrepProposPtr;
var i,j: integer;
begin
 j:= Count -1;
 for i:= 1 to fOffset do j := IndexOfQuotableBefore(j); 
 if (j < 1) or (j >= Count - 1) then GetNthPrevQuotable := gIncorrectProp
 else GetNthPrevQuotable := GetByOrder(j);
end; 

// result  is index to Items not a label number!
function MPropList.IndexOfPrev: integer;
begin IndexOfPrev:= Count - 2; end;

function MPropList.GetPrevProp: PrepProposPtr;
begin GetPrevProp := GetByOrder( IndexOfPrev); end;

function MPropList.GetCurrProp: PrepProposPtr;
begin GetCurrProp := GetByOrder(Count - 1); end;

procedure MPropList.SetCurrProp(aItem : Pointer);
begin Items^[Count - 1] := aItem; end;

procedure MPropList.FreePropsAndLabs(aIndex,aLab:integer);
begin
 FreeItemsFrom(aIndex); // ok with nils
 Labeled.fCount := aLab;
end;

begin
 gIncorrectProp := new(PrepProposPtr,
                       Init(0, NewInCorFrm, ZeroPos, propUnknown));

end.


