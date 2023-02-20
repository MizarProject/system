(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// The standard preparator object, corresponding to the old LevelObj.
// Does almost anything needed for checker - inserting propositions,
// local constants, schemes, definienda, clusters (also rounding up),
// etc. The rest neded for checker is done in mizprep, which
// coresponds to the old MizPrepObj

// ##TODO: Some things which are now wired into the prephan, should
//         be eventually moved here, e.g. inserting of constructors
//         into the global tables, or inferconstcollecting.

unit stdprep;

interface

uses mobjects,justhan,iocorrel,correl,prephan,prepobj,errhan,propcoll;

type

// ##TODO: end position?
 StdBlockPtr = ^StdBlockObj;
 StdBlockObj =
  object(PrepBlockObj)
// counts for global arrays -
// used often and initialized with parent's value
   nVarNbr: integer; // last var used in this block

// stored counts for global MCollections - initialized with -1
// and used only for restoring parent after child block exit
   nInferConstNbr: integer;
   nLocFuncNbr: integer;
   nPropNbr: integer;
   nLabNbr: integer;

// nLabeled replaced with macro (nLabel = 0)
   nLabel: integer; // label of this block
   nStartPos: Position;
   nThesisKind: PropositionKind; // only for quotable blocks
   nThesis: FrmPtr;
   nLeftSide: TrmPtr;  // for iterative equalities
// ##TODO: Scheme should be changed to MCollection, nSchLab would
//         probably not be needed at all
   nSchLab: integer; // nonzero only for blPublicScheme
   nSchErrOcc: boolean; // replacement for the ErrOcc global
      // remembers if error occurred
   nDefined: MList; // CONSTRPTRs and ClusterPtrs defined in the block
   constructor Init(fBlockKind:BlockKind);
   destructor Done; virtual;
   procedure FinishBlock; virtual;
   procedure InitPrepData(OnlyTheorems:boolean);
   procedure StartPublicScheme; virtual;

   // this is a bit silly, but FPC does not allow other result type
   function CreateItem(fItemKind:ItemKind):PrepItemPtr; virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
   function GetPrevious : StdBlockPtr;
   procedure DebugBlockCreate(fBlockKind:BlockKind); virtual;
   procedure DebugBlockEnd(fBlockKind:BlockKind); virtual;
   procedure ProcessBlockLabel; virtual;
   procedure ProcessBlockPosition; virtual;
   procedure ProcessSchemeLabel; virtual;

   procedure StartBlockThesis; virtual;
   procedure FinishBlockThesis; virtual;
   procedure FinishMain; virtual;
   procedure FinishDefinition; virtual;
   procedure FinishRegistration; virtual;
   procedure FinishNotation; virtual;
   procedure FinishCase; virtual;
   procedure FinishSuppose; virtual;
   procedure FinishCaseList; virtual;
   procedure FinishProof; virtual;
   procedure FinishDiffuse; virtual;
   procedure FinishPublicScheme; virtual;
   procedure FinishQuotableBlock; virtual;
   function  IntroducedLabel(fLabNr:integer):boolean; virtual;
   procedure ProcessProposition; virtual;
   procedure ProcessFormula; virtual;
   procedure ProcessTerm; virtual;
   procedure ProcessType; virtual;
   procedure ProcessTypeList; virtual;
   procedure ProcessIterSteps; virtual;
   procedure ProcessDefiniens; virtual;
   procedure ProcessProperty; virtual;
   procedure ProcessRCluster; virtual;
   procedure ProcessCCluster; virtual;
   procedure ProcessFCluster; virtual;
   procedure ProcessConstructor; virtual;
  end;

 StdItemPtr = ^StdItemObj;
 StdItemObj =
  object(PrepItemObj)
   nItemLab: integer; // this changes if multiple propositions
   constructor Init(fItemKind:ItemKind; fBlock: PrepBlockPtr);
   destructor Done; virtual;
   function  GetPrevious : StdItemPtr;
   function  GetBlock : StdBlockPtr;
   procedure ProcessLabel; virtual;
   procedure StartQuotableProposition(fKind: PropositionKind); virtual;
   procedure FinishQuotableProposition(fKind: PropositionKind); virtual;
   procedure StartUnquotableProposition(fKind: PropositionKind); virtual;
   procedure FinishUnquotableProposition(fKind: PropositionKind); virtual;
   procedure FinishFixedVariable; virtual;
   procedure StartReconsideredTerm; virtual;
   procedure FinishReconsideredTerm; virtual;
   procedure StartExemplifyingTerm; virtual;
   procedure FinishExemplifyingTerm; virtual;
   procedure FinishConstantDefinition; virtual;
   procedure FinishPrivFuncDefinition; virtual;
   procedure StartIterEquality; virtual;
   procedure FinishIterEquality; virtual;
   procedure StartSchemeTypes; virtual;
   procedure FinishSchemeTypes; virtual;
   procedure FinishSchemePremise; virtual;
   procedure StartSchemePremises; virtual;
   procedure FinishDefiniens; virtual;
   procedure FinishSchFuncSegment; virtual;
   procedure FinishSchemeThesis; virtual;
   procedure FinishIdentify; virtual;
   procedure FinishReduction; virtual;
   procedure FinishExistentialCluster; virtual;
   procedure FinishConditionalCluster; virtual;
   procedure FinishFunctorCluster; virtual;

   procedure InsertConstantType( fTyp: TypPtr); virtual;
   procedure ProcessConstantDef; virtual;
  end;

implementation

uses mconsole,mizenv,inout,limits,xml_parser,xmldict,xmlpars,
     identify,schemhan,builtin,lexicon,enums
{$IFDEF CH_REPORT}
     ,req_info,librenv
{$ENDIF}                                         
{$IFDEF MDEBUG},info,outinfo{$ENDIF};

(********** Standard overloaded stuff **********)

function  StdItemObj.GetPrevious : StdItemPtr;
begin GetPrevious := StdItemPtr(Previous); end;

function  StdItemObj.GetBlock : StdBlockPtr;
begin GetBlock := StdBlockPtr(nBlock); end;

function  StdBlockObj.GetPrevious : StdBlockPtr;
begin GetPrevious := StdBlockPtr(Previous); end;

function StdBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm:= new(StdItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem:= nCurrItm;
end;

procedure StdBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr:= new(StdBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;
end;

(***********************************************)

procedure StdBlockObj.DebugBlockCreate(fBlockKind:BlockKind);
begin
 inherited DebugBlockCreate(fBlockKind);
 {$IFDEF MDEBUG}
// InfoString('; labeled and all: ');
// InfoInt(Propositions^.Labeled.fCount-1);
// InfoInt(Propositions^.Count-1);
// InfoString('; nVarNbr, InferConstNbr: ');
// InfoInt(nVarNbr);
// InfoInt(InferConstDef.Count);
 {$ENDIF}
end;

procedure StdBlockObj.DebugBlockEnd(fBlockKind:BlockKind);
begin
 inherited DebugBlockEnd(fBlockKind);
 {$IFDEF MDEBUG}
// InfoString('; labeled and all: ');
// InfoInt(Propositions^.Labeled.fCount-1);
// InfoInt(Propositions^.Count-1);
// InfoString('; nVarNbr, InferConstNbr: ');
// InfoInt(nVarNbr);
// InfoInt(InferConstDef.Count);
 {$ENDIF}
end;

// ##TODO: either global arrays with counts or global MCollections
//         should be used, having both complicates the "block"
//         semantics.
//         For global arrays, the current count is gPrBlockPtr^.nXXXNbr
//         while for global MCollection it is its count, and its last
//         value is stored when creating a child block.
constructor StdBlockObj.Init(fBlockKind:BlockKind);
procedure ZeroInit;
begin
 nVarNbr:= 0;
 nInferConstNbr:= -1;  // safety
 nLocFuncNbr:= -1;  // safety
 nPropNbr:= -1;  // safety
 nLabNbr:=-1;  // safety
 nLabel:=0;
 nStartPos:=ZeroPos;
 nThesisKind:= propUnknown;
 nThesis:= nil;
 nLeftSide := nil;
 nSchLab:= 0;
 nSchErrOcc:=false;
 nDefined.Init(0);
end;
begin
 MarkTermsInTTColl;
 inherited Init(fBlockKind);
 ZeroInit;

  if Assigned(Previous) then
 begin
  nVarNbr:= GetPrevious^.nVarNbr;
  InferConstDef.Mark(GetPrevious^.nInferConstNbr);
  GetPrevious^.nLocFuncNbr:= LocFuncDef.Count;
  GetPrevious^.nPropNbr:= Propositions^.Count;
  GetPrevious^.nLabNbr:= Propositions^.Labeled.fCount;
 end;

{$IFDEF DISPCTRL}
 nMemAvail:=MemAvail;
 {writeln(InfoFile,'<<<MemAvail=',MemAvail); InfoHeap;}
{$ENDIF}
end;

destructor StdBlockObj.Done;
begin
 nDefined.DeleteAll;	// the constructors are needed - no disposing!
 nDefined.Done;
 if Assigned(nThesis) then dispose(nThesis, Done);
 if Assigned(Previous) then
 begin
  GetPrevious^.nInferConstNbr:= -1;
  GetPrevious^.nLocFuncNbr:= -1;
  GetPrevious^.nPropNbr:= -1;
  GetPrevious^.nLabNbr:= -1;
 end;
 inherited Done;
 RemoveTermsFromTTColl;
end;

procedure StdBlockObj.ProcessProposition;
begin
  CollectConstInFrm(gPrep^.nLastProposition^.nSentence);
end;

procedure StdBlockObj.ProcessFormula;
begin
 CollectConstInFrm(gPrep^.nLastFormula);
end;

procedure StdBlockObj.ProcessTerm;
begin
 CollectConstInTrm(gPrep^.nLastTerm);
end;

procedure StdBlockObj.ProcessType;
begin
 CollectConstInTyp(gPrep^.nLastType);
end;

procedure StdBlockObj.ProcessTypeList;
begin
 CollectConstInTypList(gPrep^.nLastTypeList^);
end;

procedure StdBlockObj.ProcessIterSteps;
 var k: integer;
begin
 with gPrep^.nLastIterSteps^ do
  for k:=0 to Count-1 do
   with IterStepPtr(Items^[k])^ do
    begin CollectConstInTrm(nEquatedTrm);
    end;
end;

procedure StdBlockObj.ProcessDefiniens;
// var k: integer;
begin
{ with gPrep^.nLastDefiniens^ do
  if Definiens <> nil then
  with Definiens^ do
  begin
   CollectConstInTypList(PrimaryList);
   if Assumptions <> nil then CollectConstInFrm(Assumptions);
   with nPartialDefinientia do
    for k:=0 to Count-1 do
    with PartDefPtr(Items^[k])^ do
     begin
      CollectConstInFrm(FrmPtr(nGuard));
      case DefSort of
      'm': CollectConstInFrm(FrmPtr(nPartDefiniens));
      'e': CollectConstInTrm(TrmPtr(nPartDefiniens));
      end;
     end;
   if nOtherwise <> nil then
    case DefSort of
    'm': CollectConstInFrm(FrmPtr(nOtherwise));
    'e': CollectConstInTrm(TrmPtr(nOtherwise));
    end;
  end;}
end;

procedure StdBlockObj.ProcessProperty;
begin
 if gPrep^.nLastProperty = nil then exit;
 with PropertyPtr(gPrep^.nLastProperty)^ do
 begin
   CollectConstInTyp(TypPtr(nObject));
//   CollectConstInTypList(nPrimaryList);
 end;
end;

procedure StdBlockObj.ProcessRCluster;
begin
 if gPrep^.nLastDefCluster = nil then exit;
 nDefined.Insert( gPrep^.nLastDefCluster);
 with RClusterPtr(gPrep^.nLastDefCluster)^ do
 begin
   CollectConstInCluster(nConsequent.Upper);
//  nie ma chyba sensu wykonywanie kolekcjonowania klustrow
//   CollectConstInTypList(nPrimaryList);
//   CollectConstInTyp(nClusterType);
 end;
end;

procedure StdBlockObj.ProcessCCluster;
begin
 if gPrep^.nLastDefCluster = nil then exit;
 nDefined.Insert( gPrep^.nLastDefCluster);
 with CClusterPtr(gPrep^.nLastDefCluster)^ do
 begin
   CollectConstInCluster(nConsequent.Upper);
//  nie ma chyba sensu wykonywanie kolekcjonowania klustrow
//   CollectConstInTypList(nPrimaryList);
//   CollectConstInTyp(nClusterType);
//   CollectConstInCluster(nAntecedent);
 end;
end;

procedure StdBlockObj.ProcessFCluster;
begin
 if gPrep^.nLastDefCluster = nil then exit;
 nDefined.Insert( gPrep^.nLastDefCluster);
 with FClusterPtr(gPrep^.nLastDefCluster)^ do
 begin
   CollectConstInCluster(nConsequent.Upper);
//  nie ma chyba sensu wykonywanie kolekcjonowania klustrow
//   CollectConstInTypList(nPrimaryList);
//   CollectConstInTrm(nClusterTerm);
 end;
end;

procedure StdBlockObj.ProcessConstructor;
begin
 nDefined.Insert( gPrep^.nLastConstructor);
 with gPrep^.nLastConstructor^ do
  case fConstrKind of
   coAttribute,coPredicate: ;
   coFunctor,coMode,coSelector,coAggregate:
    with ConstrTypPtr(gPrep^.nLastConstructor)^ do
    CollectConstInTyp(fConstrTyp);
   coStructMode:
    with StructConstrPtr(gPrep^.nLastConstructor)^ do
    CollectConstInTypList(fPrefixes);
  end;
end;

var gLocalConstOcc: boolean;
    gVarNbrBase: integer;
    gLocalConsts: NatSet;
procedure CheckForLocalConst(var fTrm:TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  case TrmSort of
  ikTrmConstant:
    if VarNr>gVarNbrBase then
     gLocalConstOcc:=true;
  ikTrmInfConst:
    if gLocalConsts.HasInDom(VarNr) then
     gLocalConstOcc:=true;
  end;
end;

var gNonLocalConstNr: NatFunc;
procedure RenumNonLocalConst(var fTrm:TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  case TrmSort of
  ikTrmInfConst:
   if gNonLocalConstNr.HasInDom(VarNr) then
    VarNr:=gNonLocalConstNr.Value(VarNr)
  end;
end;

procedure FreeConstDef(aVarNbr,aInferConstNbr: integer);
 var I,j,k,lCount: integer;
     lEqConst: NatSet;
     lEqChanged: boolean;
begin
 gVarNbrBase:=aVarNbr;
 lCount := InferConstDef.Count;
 k:=0;
 for I:=0 to InferConstDef.Count-1 do
  if InferConstDef.fIndex^[I] < aInferConstNbr then
   begin
    InferConstDef.fIndex^[k]:=InferConstDef.fIndex^[I];
    inc(k);
   end;
 mizassert(3287,k = aInferConstNbr);
 InferConstDef.Count:=aInferConstNbr;
 gLocalConsts.Init(lCount-aInferConstNbr,4);
 I:=aInferConstNbr;
 while I < lCount do
  with ConstDefPtr(InferConstDef.Items^[I])^ do
   if gLocalConsts.HasInDom(I) then inc(I)
    else
    begin gLocalConstOcc:=false;
     WithinTerm(fDef,CheckForLocalConst);
     if not gLocalConstOcc then
      fTyp^.WithinType(CheckForLocalConst);
     if gLocalConstOcc then
      begin
       gLocalConsts.InsertElem(I);
       I:=aInferConstNbr;
      end
     else inc(I);
    end;
 for I:=aInferConstNbr to lCount-1 do
  if gLocalConsts.HasInDom(I) then
    Dispose(PObject(InferConstDef.Items^[I]), Done)
   else
   begin
    gNonLocalConstNr.Assign(I,InferConstDef.Count);
    InferConstDef.Insert(InferConstDef.Items^[I]);
   end;
 for I:=aInferConstNbr to InferConstDef.Count-1 do
  with ConstDefPtr(InferConstDef.Items^[I])^ do
  begin
   WithinTerm(fDef,RenumNonLocalConst);
   fTyp^.WithinType(RenumNonLocalConst);
  end;
 for I:=0 to InferConstDef.Count-1 do
  with ConstDefPtr(InferConstDef.Items^[I])^ do
  begin
   lEqChanged:=false;
   for j:=0 to fEqConst.Count - 1 do
    if gNonLocalConstNr.HasInDom(fEqConst.Items^[j].X) then
     lEqChanged:=true;
   if lEqChanged then
    begin
     lEqConst.Init(0,4);
      for j:=0 to fEqConst.Count - 1 do
      if gNonLocalConstNr.HasInDom(fEqConst.Items^[j].X) then
        lEqConst.InsertElem(gNonLocalConstNr.Value(fEqConst.Items^[j].X))
       else if fEqConst.Items^[j].X < InferConstDef.Count then
        lEqConst.InsertElem(fEqConst.Items^[j].X);
      fEqConst.Done;
      fEqConst.MoveNatSet(lEqConst);
    end;
  end;
{$IFDEF MDEBUG}
//writeln(infofile,'po InferConstDef.Count=',InferConstDef.Count);
//for II:=0 to InferConstDef.Count-1 do InfoInferConstDef(II);
{$ENDIF}
end;

procedure StdBlockObj.FinishBlock;
var k,i:integer;
begin
 for k:=GetPrevious^.nVarNbr+1 to nVarNbr do
  begin
   dispose(FixedVar[k].nTyp,Done);
   if FixedVar[k].nDef <> nil then DisposeTrm(FixedVar[k].nDef);
  end;
// this cleans the reserved block label too, if any
 Propositions^.FreePropsAndLabs(GetPrevious^.nPropNbr,
                               GetPrevious^.nLabNbr);
 LocFuncDef.FreeItemsFrom(GetPrevious^.nLocFuncNbr);
 inherited FinishBlock;
end;

procedure StdBlockObj.FinishMain;
begin
 InferConstDef.FreeItemsFrom(GetPrevious^.nInferConstNbr);
end;

procedure StdBlockObj.FinishDefinition;
 var i: integer; lConstr: ConstrPtr;
begin
 gNonLocalConstNr.InitNatFunc(0,10);
 FreeConstDef(GetPrevious^.nVarNbr,GetPrevious^.nInferConstNbr);
 for i:=0 to nDefined.Count-1 do
 begin
  // struct definitions also contain the strict registratration
  if typeof(PObject(nDefined.Items^[i])^) = typeof(RClusterObj) then
   WithinCluster( ClusterPtr( nDefined.Items^[i]), RenumNonLocalConst)
  else
  begin
  lConstr:=  ConstrPtr( nDefined.Items^[i]);
  WithinTypeColl( lConstr^.nPrimaries, RenumNonLocalConst);
  if lConstr^.fConstrKind in TypedConstrKinds then
   with ConstrTypPtr( lConstr)^ do
    fConstrTyp^.WithinType(RenumNonLocalConst);
  if lConstr^.fConstrKind = coStructMode then
   with StructConstrPtr( lConstr)^ do
    WithinTypeColl( fPrefixes, RenumNonLocalConst);
  end;
 end;
 gNonLocalConstNr.Done;
end;

procedure StdBlockObj.FinishRegistration;
 var i: integer;
begin
 gNonLocalConstNr.InitNatFunc(0,10);
 FreeConstDef(GetPrevious^.nVarNbr,GetPrevious^.nInferConstNbr);
 for i:=0 to nDefined.Count-1 do
  WithinCluster( ClusterPtr( nDefined.Items^[i]), RenumNonLocalConst);
 gNonLocalConstNr.Done;
end;

procedure StdBlockObj.FinishNotation;
begin
 InferConstDef.FreeItemsFrom(GetPrevious^.nInferConstNbr);
end;

procedure StdBlockObj.FinishCase;
begin
 InferConstDef.FreeItemsFrom(GetPrevious^.nInferConstNbr);
end;

procedure StdBlockObj.FinishSuppose;
begin
 InferConstDef.FreeItemsFrom(GetPrevious^.nInferConstNbr);
end;

procedure StdBlockObj.FinishCaseList;
begin
 InferConstDef.FreeItemsFrom(GetPrevious^.nInferConstNbr);
end;

procedure StdBlockObj.FinishProof;
begin
 InferConstDef.FreeItemsFrom(GetPrevious^.nInferConstNbr);
 FinishQuotableBlock;
end;

procedure StdBlockObj.FinishDiffuse;
begin
 gNonLocalConstNr.InitNatFunc(0,10);
 FreeConstDef(GetPrevious^.nVarNbr,GetPrevious^.nInferConstNbr);
 WithinFormula(nThesis,RenumNonLocalConst);
 gNonLocalConstNr.Done;
 FinishQuotableBlock;
end;

procedure StdBlockObj.FinishQuotableBlock;
var lProp: PrepProposPtr;
begin
 if nThesisKind = propUnknown then
 begin
  Mizassert(errUnexpected, nBlockKind = blDiffuse);
  nThesisKind := propDiffuseResult;
 end;
 lProp:= new(PrepProposPtr,
             Init(nLabel, nThesis^.CopyFormula, nStartPos, nThesisKind));
 if nLabel <> 0 then
 begin
  Mizassert(2532, Propositions^.GetLastLab + 1 = nLabel);
  Propositions^.InsertLabeled(lProp);
 end
 else Propositions^.InsertUnlabeled(lProp);
// nThesis := nil;
end;

// ##NOTE: This can be used only on blocks on the current stack.
//         Labels of unfinished blocks are not accessible.
function StdBlockObj.IntroducedLabel(fLabNr:integer):boolean;
var lMin,lMax: integer;
begin
 if fLabNr = nLabel then begin IntroducedLabel:= false; exit; end;
 if not Assigned(Previous) then lMin := 1
 else lMin := GetPrevious^.nLabNbr;
 if nLabNbr = -1 then lMax := Propositions^.GetLastLab
 else lMax := nLabNbr -1;
 IntroducedLabel := (fLabNr >= lMin) and (fLabNr <= lMax);
end;

// ##TODO: gSchPos is not used any more, should be removed completely
procedure StdBlockObj.StartPublicScheme;
begin  end;

procedure StdBlockObj.ProcessSchemeLabel;
 var lSch: SchRefPtr;
begin
 Mizassert(errSchemeBlockExpected, nBlockKind = blPublicScheme);
 nSchLab:= Infile.Current.Nr;
 lSch:= new(SchRefPtr,Init1);
 lSch^.fKey.Y:= nSchLab;
 lSch^.nSchState:= schStateBeingProved;
 Ord_Scheme.Insert(lSch);
end;

// ##TODO: Using the global variable CurSchFuncTyp looks
//          unnecessary here, replace it with a slot of the block
// ##BUG?: It is strange, but only result types of scheme functors
//         are now needed in schematizer, types of arguments
//         (also for scheme predicates) are not
procedure StdItemObj.StartSchemeTypes;
begin CurSchFuncTyp.Init(0,MaxArgNbr); end;

procedure StdItemObj.FinishSchFuncSegment;
begin
 CurSchFuncTyp.Insert(gPrep^.nLastType);
 gPrep^.nLastType:= nil;
end;

// ##TODO: the 'move' should be replaced here
procedure StdItemObj.FinishSchemeTypes;
begin
 CurSchFuncTyp.SetLimit(0);
 move(CurSchFuncTyp,Scheme(0,GetBlock^.nSchLab)^.SchTypes,
      SizeOf(MCollection));
end;

// reserve place for thesis it comes first
procedure StdItemObj.StartSchemePremises;
begin
 with Scheme(0,GetBlock^.nSchLab)^ do
 begin
  SchProps.Init(1,4);
  SchProps.Insert(nil);
 end;
end;

procedure StdItemObj.FinishSchemePremise;
begin
 if Propositions^.GetCurrProp^.nSentence^.FrmSort=ikError then
  GetBlock^.nSchErrOcc := true;
 with Scheme(0,GetBlock^.nSchLab)^ do
  SchProps.Insert(Propositions^.GetCurrProp^.nSentence^.CopyFormula);
end;

procedure StdItemObj.FinishSchemeThesis;
begin
 if Propositions^.GetCurrProp^.nSentence^.FrmSort=ikError then
  GetBlock^.nSchErrOcc := true;
 with Scheme(0,GetBlock^.nSchLab)^ do
  SchProps.AtPut(0,Propositions^.GetCurrProp^.nSentence^.CopyFormula);
end;

// for correct error messaging
// schStateUnquotable has priority over schStateErrorOccurred
// here, schStateErrorOccurred means that it is labeled
procedure StdBlockObj.FinishPublicScheme;
 var i: integer;
begin
 with Scheme(0,nSchLab)^ do
 begin
  IncBounVarNbr:=true;
  for i:=0 to SchTypes.Count-1 do
   TypPtr(SchTypes.Items^[i])^.WithinType(ExpandInferConsts);
  for i:=0 to SchProps.Count-1 do
   WithinFormula(FrmPtr(SchProps.Items^[i]),ExpandInferConsts);
  IncBounVarNbr:=false;
  InferConstDef.FreeItemsFrom(GetPrevious^.nInferConstNbr);
 end; 
 CurSchFuncTyp.DeleteAll;
 with Scheme(0,nSchLab)^ do
 begin
  if (nSchLab = 0) then
   nSchState:= schStateUnquotable
  else if nSchErrOcc then
   nSchState:= schStateErrorOccurred
  else nSchState:= schStateQuotable;
 end;
end;

procedure InLibraries(OnlyTheorems:boolean);
 var LibrNr,TheoNr,SchNr: integer;
     lSnt:FrmPtr; lKind: char;
     lSchRec: SchRefPtr;
     lInEnvFile: InEnvFilePtr;
begin
 with OrdTheorem do begin Init(100,16); Insert(new(SntRefPtr,Init(0,0,nil))) end;
 with DefTheorem do begin Init(100,16); Insert(new(SntRefPtr,Init(0,0,nil))) end;
 Ord_Scheme.Init(0, 16);
 Ord_Scheme.Insert(new(SchRefPtr, Init1));
// FileExam(EnvFileName+'.eth');
 if MFileExists(EnvFileName+'.eth') then
 begin
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.eth'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elTheorems);
  NextElementState;
  while not (nState = eEnd) do
  begin
   XMLASSERT( nElKind = elTheorem);
   LibrNr:= GetIntAttr( atArticleNr);
   TheoNr:= GetIntAttr( atNr);
   lKind:= GetAttr( atKind)[1];
   NextElementState;
   case lKind of
    'T':
      if TheoReferNbr.HasInDom(LibrNr,TheoNr) then
      begin
       lSnt:=lInEnvFile.In_Formula;
       CollectConstInFrm(lSnt);
       OrdTheorem.Insert(new(SntRefPtr,Init(LibrNr,TheoNr,lSnt)));
      end
      else
      begin
       while (nElKind <> elTheorem){ (nState = eEnd)} do
        NextTag;
      end;
    'D':
     if DefReferNbr.HasInDom(LibrNr,TheoNr) then
     begin
      lSnt:=lInEnvFile.In_Formula;
      CollectConstInFrm(lSnt);
      DefTheorem.Insert(new(SntRefPtr,Init(LibrNr,TheoNr,lSnt)));
     end
     else
      begin
       while (nElKind <> elTheorem){ (nState = eEnd)} do
        NextTag;
      end
   end;
   NextElementState;
  end;
 end;
 dispose(lInEnvFile,Done);
 end;

 TheoReferNbr.Done;
 DefReferNbr.Done;

 if OnlyTheorems then exit;
// FileExam(EnvFileName+'.esh');
 if MFileExists(EnvFileName+'.esh') then
 begin
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.esh'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elSchemes);
  NextElementState;
  while not (nState = eEnd) do
  begin
   XMLASSERT( nElKind = elScheme);
   LibrNr:= GetIntAttr( atArticleNr);
   SchNr:= GetIntAttr( atNr);
   if SchReferNbr.HasInDom(LibrNr,SchNr) then
   begin
    NextElementState;
    lSchRec:=new(SchRefPtr,Init1);
    with lSchRec^ do
    begin
     fKey.X:=LibrNr;
     fKey.Y:=SchNr;
     lInEnvFile.In_ArgColl(CurSchFuncTyp);
     lInEnvFile.In_FormulaColl(SchProps);
     SchTypes.MoveCollection(CurSchFuncTyp);
     if SchProps.Count > 0 then
      nSchState:= schStateQuotable
     else
      if LibrNr > 0 then
        nSchState:= schStateCanceled
      else nSchState:= schStateMissingConstrs;
    end;
//   if SchReferNbr.HasInDom(LibrNr,SchNr) then
    Ord_Scheme.Insert(lSchRec);
   end
   else
   begin
    repeat
     NextTag;
    until (nElKind = elScheme){ (nState = eEnd)};
   end;
   NextElementState;
   // ##TODO: implement Done for SchRefPtr
  end;
 end;
 dispose(lInEnvFile,Done);
 end;
end;

procedure CollectFuncInTrm(fTrm:TrmPtr; var fSet: NatSet); forward;

procedure CollectFuncTrmList(fTrmList:TrmList; var fSet: NatSet);
begin
  while fTrmList<>nil do
   with fTrmList^ do
    begin CollectFuncInTrm(XTrmPtr,fSet); fTrmList:=NextTrm end;
end;

procedure CollectFuncInTrm(fTrm:TrmPtr; var fSet: NatSet);
  var z: integer;
begin
 with TrmPtr(fTrm)^ do
  case TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmEqConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmIt: ;
   ikTrmNumeral,ikError: ;
   ikTrmFunctor:
    begin CollectFuncTrmList(FuncTrmPtr(fTrm)^.FuncArgs,fSet);
     fSet.InsertElem(FuncTrmPtr(fTrm)^.FuncNr);
     fSet.InsertElem(AdjustedFuncNr(fTrm));
    end;
   ikTrmSchFunc,ikTrmAggreg,ikTrmSelector:
    CollectFuncTrmList(FuncTrmPtr(fTrm)^.FuncArgs,fSet);
   ikTrmPrivFunc:
    begin CollectFuncTrmList(FuncTrmPtr(fTrm)^.FuncArgs,fSet);
     CollectFuncInTrm(LocFuncTrmPtr(fTrm)^.FuncExp,fSet);
    end;
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
     begin
{
      with LambdaArgs do
       for z:=0 to Count-1 do
       begin
        inc(BoundVarNbr);
        CollectFuncInTyp(TypPtr(Items^[z]));
       end;
      WithinTrm(LambdaScope);
      CollectFuncInFrm(Compr);
      dec(BoundVarNbr,LambdaArgs.Count);
}
     end;
   ikTrmChoice:
     begin
//      CollectFuncInTyp(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
     end
   else
begin
{$IFDEF MDEBUG}
InfoChar(TrmSort);
{$ENDIF}
    RunTimeError(2853);
end;
  end;
end;

procedure StdItemObj.FinishDefiniens;
 var lFuncDef: EqualsDefPtr;
     i,lFunc,lBoundvarNbr: integer;
     lArgs: TrmList;
     lTrm: TrmPtr;
     lFuncSet: NatSet;
begin
 EqDefinientia.Insert(gPrep^.nLastDefiniens);
 ExDefinientia.Insert(gPrep^.nLastDefiniens);
 lFuncDef:=EqualsExpansion(gPrep^.nLastDefiniens);
 if lFuncDef <> nil then
  if TrmPtr(lFuncDef^.nExpansion)^.TrmSort = ikTrmError then
   Dispose(lFuncDef,Done)
  else
   begin
    lFuncSet.Init(0,4);
    CollectFuncInTrm(TrmPtr(lFuncDef^.nExpansion),lFuncSet);
    if lFuncSet.HasInDom(FuncTrmPtr(lFuncDef^.nPattern)^.FuncNr) then
     Dispose(lFuncDef,Done)
    else
     begin
      for i:=0 to InferConstDef.Count-1 do
      with ConstDefPtr(InferConstDef.Items^[i])^ do
       if fDef^.TrmSort = ikTrmFunctor then
       begin
        AdjustTrm(fDef,lFunc,lArgs);
        if gPrep^.nLastDefiniens^.nConstr.Nr = lFunc then
        begin
         lTrm:=ExpandTrmIfEqual(lFuncDef,lArgs);
         if lTrm <> nil then
         begin
          lBoundvarNbr:=BoundvarNbr;
          BoundvarNbr:=0;
          CollectConstInTrm(lTrm);
          BoundvarNbr:=lBoundvarNbr;
          mizassert(4369,lTrm^.TrmSort = ikTrmInfConst);
          fEqConst.InsertElem(VarTrmPtr(lTrm)^.VarNr);
          DisposeTrm(lTrm);
         end;
        end;
       end;
      gEquals.InsertAtLexem(gPrep^.nLastDefiniens^.nConstr,lFuncDef);
     end;
   end;
 gPrep^.nLastDefiniens := nil;
end;

// ##CHANGED: It is easier to start using the cluster right after it is
//            verified (and not only after the block end), so I am doing
//            it this way. It corresponds to constructor treatment, and
//            defnienda and deftheorems should be fixed this way too.

procedure StdItemObj.FinishIdentify;
 var i,k,lBoundvarNbr: integer;
     lWideningTyps: boolean;
     lArgs: TrmList;
     lTrm: TrmPtr;
     lConstr: Lexem;
begin
 if gPrep^.nLastIdentify = nil then exit;
 gIdentifications.Insert(gPrep^.nLastIdentify);
 if gPrep^.nLastIdentify^.nConstrKind = ikTrmFunctor then
 begin
  for i:=0 to InferConstDef.Count-1 do
    with ConstDefPtr(InferConstDef.Items^[i])^ do
     if fDef^.TrmSort = ikTrmFunctor then
      begin fillchar(gSubstTrm,sizeof(gSubstTrm),0);
       if EsTrm(TrmPtr(gPrep^.nLastIdentify^.nPattern[0]),fDef) then
        if  CheckLociTypes(gPrep^.nLastIdentify^.nPrimaryList) then
        begin
         lWideningTyps:=true;
         for k:=0 to gPrep^.nLastIdentify^.nEqArgs .Count-1 do
          with gPrep^.nLastIdentify^.nEqArgs.Items^[k] do
           begin
            mizassert(3931,gSubstTrm[Y]=nil);
            if not TypPtr(gPrep^.nLastIdentify^.nPrimaryList.Items^[Y-1])^.
                     IsWiderThan(TypPtr(gPrep^.nLastIdentify^.nPrimaryList.Items^[X-1])^.CopyType) then
             begin lWideningTyps:=false; break end;
            gSubstTrm[Y]:=CopyTerm(gSubstTrm[X]);
           end;
         if lWideningTyps then
         begin
           lTrm:=InstSubstTrm(TrmPtr(gPrep^.nLastIdentify^.nPattern[1]));
           lBoundvarNbr:=BoundvarNbr;
           BoundvarNbr:=0;
           CollectConstInTrm(lTrm);
           BoundvarNbr:=lBoundvarNbr;
           mizassert(4369,lTrm^.TrmSort = ikTrmInfConst);
           fEqConst.InsertElem(VarTrmPtr(lTrm)^.VarNr);
           DisposeTrm(lTrm);
         end;
        end;
       DisposeSubstTrm;
      end;
   lConstr.Kind:=gPrep^.nLastIdentify^.nConstrKind;
   lConstr.Nr:=AdjustedNr(coFunctor,FuncTrmPtr(gPrep^.nLastIdentify^.nPattern[0])^.FuncNr);
   gFuncIds.InsertAtLexem(lConstr,gPrep^.nLastIdentify);
 end;
 gPrep^.nLastIdentify:=nil;
end;

procedure StdItemObj.FinishReduction;
begin
 if gPrep^.nLastReduction = nil then exit;
 gReductions.Insert(gPrep^.nLastReduction);
 gPrep^.nLastReduction:=nil;
end;

procedure StdItemObj.FinishExistentialCluster;
begin
 if gPrep^.nLastDefCluster = nil then exit;
 RegisteredCluster.Insert(gPrep^.nLastDefCluster);
 gPrep^.nLastDefCluster:= nil;
end;

// ##TODO: This rounding up on new clusters is copied from the old prep,
//         only because it is too wrong to be fixed immediatelly.
// Problems:
//  - only types of inferconsts are rounded, not types of Consts or
//    all terms containing variables, types of numerals are not rounded
//    after FunctorClusters.
//  - this causes incompatibility when gCollecting is false.
//  - the procedures should call those in correl, not to copy&paste them,
//    otherwise next change will cause hardly detectable problems
//  - the implementation is technically wrong: when something is rounded
//    in RoundUpFurther, it should be added to gRoundedUp.

var gFirstRoundedUp: integer;
    gRoundedUp: NatSet;

procedure RoundUpFurther;
 var i:integer;
     lTrmList:TrmList;
     lClusterPtr:AttrCollectionPtr;
 label ToBeRounded;
begin
 for i:=gFirstRoundedUp to InferConstDef.Count-1 do
 case TrmPtr(ConstDefPtr(InferConstDef.Items^[i])^.fDef)^.TrmSort of
 ikTrmFunctor,ikTrmSelector,ikTrmAggreg,ikTrmSchFunc:
  with FuncTrmPtr(ConstDefPtr(InferConstDef.Items^[i])^.fDef)^ do
   begin lTrmList:=FuncArgs;
    while lTrmList <> nil do
     with lTrmList^, VarTrmPtr(XTrmPtr)^ do
      begin
       if (TrmSort=ikTrmInfConst) and gRoundedUp.HasInDom(VarNr) then goto ToBeRounded;
       lTrmList:=NextTrm;
      end;
    continue;
ToBeRounded:
    dispose(ConstDefPtr(InferConstDef.Items^[i])^.fTyp,Done);
    ConstDefPtr(InferConstDef.Items^[i])^.fTyp:=RoundUpTrmType(ConstDefPtr(InferConstDef.Items^[i])^.fDef);
    lClusterPtr:=CopyCluster(ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster);
    lClusterPtr^.RoundUpWith(ConstDefPtr(InferConstDef.Items^[i])^.fTyp);
    CollectConstInCluster(lClusterPtr);
    dispose(ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster,Done);
    ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster:=lClusterPtr;
   end;
 ikTrmNumeral:
   begin
    lClusterPtr:=CopyCluster(ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster);
    lClusterPtr^.RoundUpWith(ConstDefPtr(InferConstDef.Items^[i])^.fTyp);
    CollectConstInCluster(lClusterPtr);
    dispose(ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster,Done);
    ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster:=lClusterPtr;
   end;
 ikTrmConstant:
   begin
   end;
 ikTrmFraenkel:
   begin
   end;
 end;
end;

procedure StdItemObj.FinishConditionalCluster;
 var lRoundingUpOccurs: boolean;
     i,k: integer;
     lTyp: TypPtr;
     lClusterPtr:AttrCollectionPtr;
begin
 if gPrep^.nLastDefCluster = nil then exit;
 ConditionalCluster.Insert(gPrep^.nLastDefCluster);
 gPrep^.nLastDefCluster:= nil;

 lRoundingUpOccurs:=false; gRoundedUp.Init(0,8);
 for i:=0 to InferConstDef.Count-1 do
  with ConstDefPtr(InferConstDef.Items^[i])^.fTyp^ do
  begin
   with CClusterPtr(ConditionalCluster.Items^[ConditionalCluster.Count-1])^ do
    if TypReachable( nClusterType, ConstDefPtr(InferConstDef.Items^[i])^.fTyp) then
    begin fillchar(gSubstTrm,sizeof(gSubstTrm),0);
     if nAntecedent^.IsSubsetOf(UpperCluster,EsAttrRev) and
       not nConsequent.Upper^.IsSubsetOf(UpperCluster,AttrEquals) then
      begin
       lTyp:=nClusterType^.WideningOf(ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.CopyType);
       if lTyp <> nil then
        begin
         if CompEsTyp(nClusterType,lTyp,false) and
           nClusterType^.LowerCluster^.IsSubsetOf(lTyp^.UpperCluster,EsAttrRev) and
           CheckLociTypes(nPrimaryList) then
          begin lClusterPtr:=CopyCluster(UpperCluster);
           InitInst;
           lClusterPtr^.EnlargeBy(nConsequent.Upper);
           StopInst;
           if not lClusterPtr^.fConsistent then ErrImm(5);
           lClusterPtr^.RoundUpWith(ConstDefPtr(InferConstDef.Items^[i])^.fTyp);
           CollectConstInCluster(lClusterPtr);
           dispose(ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster,Done);
           ConstDefPtr(InferConstDef.Items^[i])^.fTyp^.UpperCluster:=lClusterPtr;
           if not lRoundingUpOccurs then
            begin lRoundingUpOccurs:=true; gFirstRoundedUp:=i end;
           gRoundedUp.InsertElem(i);
          end;
         dispose(lTyp,Done);
        end;
      end;
     DisposeSubstTrm;
    end;
  end;
 if lRoundingUpOccurs then begin RoundUpFurther; gRoundedUp.Done end;
 NonZeroTyp^.RoundUp;
end;

procedure StdItemObj.FinishFunctorCluster;
 var lRoundingUpOccurs: boolean;
     i: integer;
     lClusterPtr:AttrCollectionPtr;
begin
 if gPrep^.nLastDefCluster = nil then exit;
 FunctorCluster.Insert(gPrep^.nLastDefCluster);
 gPrep^.nLastDefCluster:= nil;
 lRoundingUpOccurs:=false;
 gRoundedUp.Init(0,8);
 for i:=0 to InferConstDef.Count-1 do
  with ConstDefPtr(InferConstDef.Items^[i])^ do
   begin
    with FClusterPtr(FunctorCluster.Items^[FunctorCluster.Count-1])^ do
     if CmpFuncTrm( nClusterTerm, ConstDefPtr(InferConstDef.Items^[i])^.fDef) = 0 then
     begin
      lClusterPtr:=CopyCluster(fTyp^.UpperCluster);
      RoundUpWith(FunctorCluster.Items^[FunctorCluster.Count-1],fDef,fTyp,lClusterPtr);
      if not lClusterPtr^.fConsistent then ErrImm(5);
      lClusterPtr^.RoundUpWith(fTyp);
      CollectConstInCluster(lClusterPtr);
      dispose(fTyp^.UpperCluster,Done);
      fTyp^.UpperCluster:=lClusterPtr;
      if not lRoundingUpOccurs then
       begin lRoundingUpOccurs:=true; gFirstRoundedUp:=i end;
      gRoundedUp.InsertElem(i);
     end;
   end;
 if lRoundingUpOccurs then
  begin RoundUpFurther; gRoundedUp.Done end;
end;

procedure CollectConstInEnvConstructors;
 var i: integer; c:ConstructorsKind;
begin
 CollectConstInTyp(NonZeroTyp);
 for c := Low(ConstructorsKind) to High(ConstructorsKind) do
  with Constr[c] do
   case c of
    coFunctor,
    coMode,
    coAggregate,
    coSelector:
     for i:=0 to Count-1 do
      with ConstrTypPtr( Items^[i])^ do
       CollectConstInTyp( fConstrTyp);
    coStructMode:
     for i:=0 to Count-1 do
      with StructConstrPtr( Items^[i])^ do
       CollectConstInTypList( fPrefixes);
   end;
 for i:=0 to RegisteredCluster.Count-1 do
  with RClusterPtr(RegisteredCluster.Items^[i])^ do
   begin
     CollectConstInCluster(nConsequent.Upper);
//  nie ma chyba sensu wykonywanie kolekcjonowania klastrow
//   CollectConstInTypList(nPrimaryList);
//   CollectConstInTyp(nClusterType);
   end;
 for i:=0 to ConditionalCluster.Count-1 do
  with CClusterPtr(ConditionalCluster.Items^[i])^ do
   begin
     CollectConstInCluster(nConsequent.Upper);
//  nie ma chyba sensu wykonywanie kolekcjonowania klastrow
//   CollectConstInTypList(nPrimaryList);
//   CollectConstInTyp(nClusterType);
//   CollectConstInCluster(nAntecedent);
   end;
 for i:=0 to FunctorCluster.Count-1 do
  with FClusterPtr(FunctorCluster.Items^[i])^ do
   begin
     CollectConstInCluster(nConsequent.Upper);
//  nie ma chyba sensu wykonywanie kolekcjonowania klustrow
//   CollectConstInTypList(nPrimaryList);
//   CollectConstInTrm(nClusterTerm);
   end;
end;

procedure StdBlockObj.InitPrepData(OnlyTheorems:boolean);
 var i,j,k: integer;
     lFuncDef: EqualsDefPtr;
     lConstr: Lexem;
     lFuncSet: NatSet;
     lClusterPtr: AttrCollectionPtr;
begin
{$IFDEF CH_REPORT}
 CHReport.OpenFileWithXSL(MizFileName+'.bex');
 CHReport.Out_XElStart( elByExplanations);
 CHReport.Out_XAttr( atAid, ArticleID);
 CHReport.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 CHReport.Out_XAttrEnd;
{$ENDIF}
 InferConstDef.InitSorted(100,CompConstDef);
 InferConsts.Init(0);
 Load_EnvConstructors;
// Definientia.Init(20);
 EqDefinientia.Init(20);
 ExDefinientia.Init(20);
// LoadDefinitions;
 LoadEqualities;
 LoadExpansions;
 gPropertiesList.Init(0);
 LoadPropertiesReg;
 gIdentifications.Init(0);
 LoadIdentify;
 gReductions.Init(0);
 LoadReductions;
 gEquals.Init(16);
 for i:=0 to EqDefinientia.Count-1 do
  begin
   lFuncDef:=EqualsExpansion(EqDefinientia.Items^[i]);
   if lFuncDef <> nil then
   begin
    lFuncSet.Init(0,4);
    CollectFuncInTrm(TrmPtr(lFuncDef^.nExpansion),lFuncSet);
    if lFuncSet.HasInDom(FuncTrmPtr(lFuncDef^.nPattern)^.FuncNr) then
     Dispose(lFuncDef,Done)
    else
     gEquals.InsertAtLexem(DefiniensPtr(EqDefinientia.Items^[i])^.nConstr,lFuncDef);
   end;
  end;
 gFuncIds.Init(16);
 for i:=0 to gIdentifications.Count-1 do
  if IdentifyPtr(gIdentifications.Items^[i])^.nConstrKind = ikTrmFunctor then
  begin
   lConstr.Kind:=IdentifyPtr(gIdentifications.Items^[i])^.nConstrKind;
   lConstr.Nr:=AdjustedNr(coFunctor,FuncTrmPtr(IdentifyPtr(gIdentifications.Items^[i])^.nPattern[0])^.FuncNr);
   gFuncIds.InsertAtLexem(lConstr,gIdentifications.Items^[i]);
  end;

 for i:=0 to gIdentifications.Count-1 do
  with IdentifyPtr(gIdentifications.Items^[i])^.nPrimaryList do
    for j:=0 to Count-1 do
      begin
        move(Items^,LocArgTyp[1],Count*sizeof(pointer));
        lClusterPtr:=CopyCluster(TypPtr(Items^[j])^.LowerCluster);
        lClusterPtr^.RoundUpWith(TypPtr(Items^[j]));
        dispose(TypPtr(Items^[j])^.UpperCluster,Done);
        TypPtr(Items^[j])^.UpperCluster:=lClusterPtr;
        gTermCollection.FreeAll;
      end;

 CollectConstInEnvConstructors;
{ for i:=0 to Definientia.Count-1 do
 with DefiniensPtr(Definientia.Items^[i])^ do
  if Definiens <> nil then
  with Definiens^ do
  begin
   CollectConstInTypList(PrimaryList);
   if Assumptions <> nil then CollectConstInFrm(Assumptions);
   with nPartialDefinientia do
    for k:=0 to Count-1 do
    with PartDefPtr(Items^[k])^ do
     begin
      CollectConstInFrm(FrmPtr(nGuard));
      case DefSort of
      'm': CollectConstInFrm(FrmPtr(nPartDefiniens));
      'e': CollectConstInTrm(TrmPtr(nPartDefiniens));
      end;
     end;
   if nOtherwise <> nil then
    case DefSort of
    'm': CollectConstInFrm(FrmPtr(nOtherwise));
    'e': CollectConstInTrm(TrmPtr(nOtherwise));
    end;
  end;}
 Propositions := new(PropListPtr, Init(MaxLabNbr));
 LocFuncDef.Init(MaxFuncVarNbr);
 InLibraries(OnlyTheorems);
end;

// the block reserves its label, if it has any
procedure StdBlockObj.ProcessBlockLabel;
var lProp: PrepProposPtr;
begin
 nLabel := InFile.Current.Nr;
 if nBlockKind = blProof then
 begin
  lProp:= Propositions^.ExtractCurrent;
  nThesisKind:= lProp^.nKind;
  nThesis := lProp^.nSentence;
  lProp^.nSentence:= nil;
  dispose(lProp, Done);
  // the disposal will be done according to the parent, so we have to
  // fix its nPropNbr - this corresponds to Diffuse
  dec(GetPrevious^.nPropNbr);
 end;
 if nLabel <> 0 then
 begin
  if nBlockKind = blProof then dec(GetPrevious^.nLabNbr);
  Propositions^.SkipBlockLabel;
  Mizassert(2532, nLabel = Propositions^.GetLastLab);
 end;
end;

procedure StdBlockObj.ProcessBlockPosition;
begin nStartPos := CurPos; end;

// the block exit destroys its inference constants,
// so we collect them in its thesis only after the block exits
// ##TODO: the collecting would be more robust if we kept usage
//         count for each InferConst and disposed them accordingly
procedure StdBlockObj.StartBlockThesis;
begin
end;

procedure StdBlockObj.FinishBlockThesis;
begin
 if Assigned(nThesis) then
  Mizassert(errUnexpected,
            (nBlockKind = blProof) and EqFrm(nThesis, gPrep^.nLastFormula))
 else begin
  nThesis := gPrep^.nLastFormula;
  gPrep^.nLastFormula:= nil;
 end;
end;

constructor StdItemObj.Init(fItemKind:ItemKind; fBlock: PrepBlockPtr);
begin
 MarkTermsInTTColl;
 inherited Init(fItemKind, fBlock);
 nItemLab:= 0;
end;

destructor StdItemObj.Done;
begin
 inherited Done;
 RemoveTermsFromTTColl;
end;

// ##CHANGED: gLabeled is now gItemPtr^.nItemLab <>0
// ##NOTE: nItemLab changes if multiple propositions in one item
procedure StdItemObj.ProcessLabel;
begin
 nItemLab := InFile.Current.Nr;
 if nItemLab <> 0 then
  begin
   Mizassert(2529,nItemLab <= MaxLabNbr);
   Mizassert(2530,Propositions^.GetLastLab + 1 = nItemLab);
  end;
end;

procedure StdItemObj.StartQuotableProposition(fKind: PropositionKind);
begin
 inherited StartQuotableProposition(fKind);
end;

procedure StdItemObj.FinishQuotableProposition(fKind: PropositionKind);
begin
 Mizassert(errUnexpected, nItemLab = gPrep^.nLastProposition^.nLabNr);
 if nItemLab <> 0 then Propositions^.InsertLabeled(gPrep^.nLastProposition)
 else Propositions^.InsertUnlabeled(gPrep^.nLastProposition);
 gPrep^.nLastProposition:= nil;
end;

// ##TODO: there should be additional control for unquotables
procedure StdItemObj.StartUnquotableProposition(fKind: PropositionKind);
begin end;

procedure StdItemObj.FinishUnquotableProposition(fKind: PropositionKind);
begin
 Propositions^.InsertUnlabeled(gPrep^.nLastProposition);
 gPrep^.nLastProposition:= nil;
end;

// internal
procedure StdItemObj.InsertConstantType( fTyp: TypPtr);
begin
 inc(GetBlock^.nVarNbr);
 Mizassert(2535,GetBlock^.nVarNbr <= MaxVarNbr);
 FixedVar[GetBlock^.nVarNbr].nTyp:= fTyp;
 FixedVar[GetBlock^.nVarNbr].nDef:= nil;
end;

// internal
procedure StdItemObj.ProcessConstantDef;
begin
 FixedVar[GetBlock^.nVarNbr].nDef:= gPrep^.nLastTerm;
 gPrep^.nLastTerm := nil;
end;

procedure StdItemObj.FinishFixedVariable;
begin
 InsertConstantType(gPrep^.nLastType);
 gPrep^.nLastType:= nil;
end;

// must copy, can be used for more terms
procedure StdItemObj.StartReconsideredTerm;
begin InsertConstantType(gPrep^.nLastType^.CopyType); end;

procedure StdItemObj.FinishReconsideredTerm;
begin ProcessConstantDef; end;

// nothing for itSimpleExemplification - no constant created;
// the copying here now unnecessary, keeping if in the future
// take x,y,z = 1 is allowed
procedure StdItemObj.StartExemplifyingTerm;
begin InsertConstantType(gPrep^.nLastType^.CopyType); end;

procedure StdItemObj.FinishExemplifyingTerm;
begin ProcessConstantDef; end;

// probably only needed for numeration, the term is ignored
procedure StdItemObj.FinishConstantDefinition;
begin
 InsertConstantType(gPrep^.nLastType);
 gPrep^.nLastType:= nil;
end;

procedure StdItemObj.FinishPrivFuncDefinition;
begin with gPrep^ do
 begin
  LocFuncDef.Insert(new(FuncDefPtr,Init(0,nLastTypeList^,nLastTerm,nLastType)));
  nLastTypeList:= nil;
  nLastTerm:= nil;
  nLastType:= nil;
 end;
end;

procedure StdItemObj.StartIterEquality;
begin
 StdBlockPtr(nBlock)^.nLeftSide:= gPrep^.nLastTerm;
 gPrep^.nLastTerm:= nil;
end;

// Needed parts of gPrep^.nLastIterSteps are copied and it is left to be
// disposed automatically by prephan.
procedure StdItemObj.FinishIterEquality;
var lFrm:FrmPtr; lProp: PrepProposPtr;
begin
 with gPrep^.nLastIterSteps^, IterStepPtr(Items^[Count-1])^ do
 begin
  lFrm:= NewEqFrm(StdBlockPtr(nBlock)^.nLeftSide, CopyTerm(nEquatedTrm));
  lProp:= new(PrepProposPtr,
               Init( nItemLab, lFrm, IterStepPtr(Items^[Count-1])^.nPos,
                     propIterResult));
 end;

 if nItemLab <> 0 then Propositions^.InsertLabeled(lProp)
 else Propositions^.InsertUnlabeled(lProp);

 StdBlockPtr(nBlock)^.nLeftSide := nil; // safety
end;

end.

