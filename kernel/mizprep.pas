(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit mizprep;

interface

uses mobjects,justhan,correl,prephan,prepobj,errhan,propcoll,stdprep;

type
 
 MizPBlockPtr = ^MizPBlockObj;
 MizPBlockObj =
  object(StdBlockObj)
   procedure InitPrepData; virtual;
   // this is a bit silly, but FPC does not allow other result type
   function CreateItem(fItemKind:ItemKind):PrepItemPtr; virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
   function GetPrevious : MizPBlockPtr;
  end;

 MizPItemPtr = ^MizPItemObj;
 MizPItemObj =
  object(StdItemObj)
   function  GetPrevious : MizPItemPtr;
   function  GetBlock : MizPBlockPtr;
   procedure FinishIterEquality; virtual;
   procedure FinishSimpleJustification; virtual;
   procedure Justify(fInferencePtr:pointer); virtual;
  end;
 
 ChInferPtr = ^ChInferObj;
 ChInferObj =
  object(InferenceObj)
   procedure Justify(fBlock:MizPBlockPtr);
   procedure CollectRefs;
   procedure SchemeInfer;
  end;

 Procedure0 = procedure(fBlock:MizPBlockPtr);

var ChkInference: Procedure0;

procedure ClassicalChecker(fBlock:MizPBlockPtr);
 
implementation

uses mconsole,mizenv,inout,limits,identify,schemhan,schemes,checker,
     builtin,lexicon,pragmas
{$IFDEF MDEBUG}
    ,info,outinfo
{$ENDIF}; 

procedure ClassicalChecker(fBlock:MizPBlockPtr);
 var i: integer;
begin
 InferenceChecker(Reference,PremNbr,fBlock^.nVarNbr);
 with ChErrNr do
 for i:=0 to Count-1 do
  ErrImm(Items^[i].X);
end;

(********** Standard overloaded stuff **********)

function  MizPItemObj.GetPrevious : MizPItemPtr;
begin GetPrevious := MizPItemPtr(Previous); end;   

function  MizPItemObj.GetBlock : MizPBlockPtr;
begin GetBlock := MizPBlockPtr(nBlock); end;

function  MizPBlockObj.GetPrevious : MizPBlockPtr;
begin GetPrevious := MizPBlockPtr(Previous); end;   

function MizPBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm:= new(MizPItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem:= nCurrItm;
end;

procedure MizPBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr:= new(MizPBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;  
end;

(********** Inherited namespace **********)

procedure MizPBlockObj.InitPrepData;
begin
 Infile.OpenFile(MizFileName+'.ref');
 ReferNbrLoad(SchReferNbr);
 ReferNbrLoad(TheoReferNbr);
 ReferNbrLoad(DefReferNbr);
 {ReverseRequirements;}
 InFile.Done;
 inherited InitPrepData(false);
end;


procedure MizPItemObj.FinishSimpleJustification;
begin
 Justify(gPrep^.nLastInference);
 gPrep^.nLastInference := nil; // justifications now kept in props
end; 

// Needed parts of gPrep^.nLastIterSteps are copied and it is left to be
// disposed automatically by prephan.
procedure MizPItemObj.FinishIterEquality;
var
 lLeft,lRight: TrmPtr;
 lFrm: FrmPtr;
 z: integer;
 lProp: PrepProposPtr;
begin
 lLeft:= GetBlock^.nLeftSide;
 with gPrep^.nLastIterSteps^ do
 begin
  for z:=0 to Count-1 do
  begin
    lRight:= IterStepPtr(Items^[z])^.nEquatedTrm;
    lFrm:= NewEqFrm(CopyTerm(lLeft), CopyTerm(lRight));
    lProp:= new(PrepProposPtr,
                       Init( 0, lFrm, IterStepPtr(Items^[z])^.nPos,
                             propIterStep));
    Propositions^.InsertUnlabeled(lProp);
    Justify(ChInferPtr(Items^[z]));
    lLeft:= lRight;
  end;
 
  lLeft:= GetBlock^.nLeftSide;
  lFrm := NewEqFrm(lLeft, CopyTerm(lRight));
  lProp:= new(PrepProposPtr,
                       Init( nItemLab, lFrm,
                             IterStepPtr(Items^[Count-1])^.nPos,
                             propIterResult));
 end;
 
 if nItemLab <> 0 then Propositions^.InsertLabeled(lProp)
 else Propositions^.InsertUnlabeled(lProp);
 
// ###TODO: do this in all preparators! 
 gPrep^.nLastIterSteps^.DeleteAll;  // justifications now kept in props
 GetBlock^.nLeftSide := nil; // safety
end;

(********** Internal methods **********)

procedure MizPItemObj.Justify(fInferencePtr:pointer);
begin
 ChInferPtr(fInferencePtr)^.Justify(GetBlock);
 Propositions^.GetCurrProp^.nSimpleJustif := fInferencePtr;
end;


procedure ChInferObj.CollectRefs;
  var z: integer;
begin
 with nReferences do for z:=0 to Count-1 do
  PReference(Items^[z])^.CollectRef1
end;

procedure ChInferObj.SchemeInfer;
var
 lSchemeNr: integer; lSchFrmList: MCollection;
 lScheme: SchRefPtr;
begin
 lSchemeNr:= nSchemeNr;
 DisplayLine(CurPos.Line,ErrorNbr);
 PremNbr:= 0;
 InferOK:= true;
 CollectRefs;
 
// ##TODO: check that these two can happen 
 lScheme:= SchRefPtr(Ord_Scheme.FirstThat(nSchFileNr));
 if lScheme = nil then begin ErrImm(191); exit end;
 lScheme:= SchRefPtr(Ord_Scheme.ObjectOf(nSchFileNr, lSchemeNr));
 if lScheme = nil then begin ErrImm(196); exit end;
 
// Unquotable schemes cannot be quoted,
// something went wrong if they occur here
 case lScheme^.nSchState of
  schStateCanceled:begin ErrImm(193); exit end;
  schStateMissingConstrs: begin ErrImm(195); exit end;
  schStateBeingProved:begin ErrImm(196); exit end;
  schStateErrorOccurred:exit;
  schStateQuotable:;                  // continue
 else RunTimeError(errUnexpectedSchState);
 end;
 
 if PremNbr <> lScheme^.SchProps.Count - 1 then
 begin ErrImm(194); exit end;
 if InferOK and (Propositions^.GetCurrProp^.nSentence^.FrmSort<>ikError)
 then begin
  lSchFrmList.Init(PremNbr+1,0);
  lSchFrmList.Insert(Propositions^.GetCurrProp^.nSentence);
  move(Reference[1], lSchFrmList.Items^[1],PremNbr*sizeof(FrmPtr));
  lSchFrmList.Count:=PremNbr+1;
  Schematize(lScheme^, lSchFrmList);
  lSchFrmList.DeleteAll; lSchFrmList.Done;
 end;
{? gInference.Done;?}
   { ? Schemat zdefiniowany w artykule i wogole nie uzywany,
       nie zostanie rozdysponowany.
   }
 SchReferNbr.Down(nSchFileNr, lSchemeNr);
 if not SchReferNbr.HasInDom(nSchFileNr, lSchemeNr) then     
 with Scheme(nSchFileNr, lSchemeNr)^ do
 begin SchTypes.Done; SchProps.Done end;
end;

procedure ChInferObj.Justify(fBlock:MizPBlockPtr);
var lThesis : FrmPtr;
   i	    : Integer;
begin
 CurPos:=nPos;   {*AN removed 28.08.2000, and restored back 07.09.2000}
  case nInferSort of
   ikInfBy:
    begin
       for i:=0 to VerifyPragmaIntervals.Count-1 do
	  with VerifyPragmaIntervals.Items^[i] do
	     if (nPos.line>=X) and (nPos.Line<=Y) then exit;
     DisplayLine(CurPos.Line,ErrorNbr);
     PremNbr:=0; InferOK:=true;
     lThesis:=NewNeg(Propositions^.GetCurrProp^.nSentence);
     InsertRef(nPos,lThesis);
     if nLinked then InsertRef(nPos,Propositions^.GetPrevProp^.nSentence);
     CollectRefs;
     ChErrNr.Init(0,4);
     ChErrNr.Duplicates:=true;
    if InferOK then ChkInference(fBlock);
     ChErrNr.Done;
     if lThesis^.FrmSort = ikFrmNeg then dispose(lThesis);
    end;
   ikInfFrom :
	      begin
		 for i:=0 to SchemePragmaIntervals.Count-1 do
		    with SchemePragmaIntervals.Items^[i] do
		       if (nPos.line>=X) and (nPos.Line<=Y) then exit;
		 SchemeInfer;
	      end;
  end;
end;

end.

