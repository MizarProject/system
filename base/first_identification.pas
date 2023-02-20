(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit first_identification;

interface

uses errhan, mobjects, mscanner, syntax,
     abstract_syntax, parseraddition,
     block_and_item, wsmarticle, xml_inout;

type

 MSLabelPtr = ^MSLabelObj;
 MSLabelObj = object(LabelObj)
   nSerialNr,nLabelNr: integer;
  constructor Init(aLabelId:integer; const aPos:Position; aNr,aLabelNr:integer);
 end;

 MSLocalReferencePtr = ^MSLocalReferenceObj;
 MSLocalReferenceObj =
  object(LocalReferenceObj)
    nRefNr,nLabNr: integer;
   constructor Init(aLabId:integer; const aPos:Position; aRef,aLab: integer);
  end;

 MSSchemeJustificationPtr = ^MSSchemeJustificationObj;
 MSSchemeJustificationObj =
  object(SchemeJustificationObj)
    nSchemeNr: integer; { 0 dla standardowych inferencji }
   constructor Init(const aPos:Position; aArticleNr,aNr,aSchNr:integer);
  end;

 MSStraightforwardJustificationPtr = ^MSStraightforwardJustificationObj;
 MSStraightforwardJustificationObj =
  object(StraightforwardJustificationObj)
    nLabelNr: integer;
   constructor Init(const aPos:Position; aLinked:boolean; const aLinkPos:Position;
                    aPrevLabelNr:integer);
   destructor Done; virtual;
  end;

 MSSchemePtr = ^MSSchemeObj;
 MSSchemeObj =
  object(SchemeObj)
   nSchemeNr: integer;
   constructor Init(aIdNr,aSchNr:integer; const aPos:Position; aParams:PList;
                    aPrems:PList; aConcl:FormulaPtr);
  end;

 VariableKind = (FreeVar,ReservedVar,Bound,Constant,DefConstant,
                 SchematicFunc,PrivateFunc,SchematicPred,PrivatePred);

 LocalDefinitionsKind = SchematicFunc..PrivatePred;

 MSVariablePtr = ^MSVariableObj;
 MSVariableObj = object(VariableObj)
    nOrigin,nVarKind: VariableKind;
    nSerialNr,nVarNr: integer;
   constructor Init(const aPos:Position; aIdentNr:integer;
                    aOrigin,aVarKind:VariableKind; aNr,aVarNr: integer);
 end;

 MSLocusPtr = ^MSLocusObj;
 MSLocusObj = object(LocusObj)
    nOrigin,nVarKind: VariableKind;
    nSerialNr,nVarNr: integer;
   constructor Init(const aPos:Position; aIdentNr:integer;
                    aOrigin,aVarKind:VariableKind; aNr,aVarNr: integer);
 end;

 MSSimpleTermPtr = ^MSSimpleTermObj;
 MSSimpleTermObj = object(SimpleTermObj)
    nOrigin,nVarKind: VariableKind;
    nSerialNr,nVarNr: integer;
   constructor Init(const aPos:Position; aIdentNr:integer;
                    aOrigin,aVarKind:VariableKind; aNr,aVarNr: integer);
 end;

 MSPrivateFunctorTermPtr = ^MSPrivateFunctorTermObj;
 MSPrivateFunctorTermObj = object(PrivateFunctorTermObj)
   nFuncKind: LocalDefinitionsKind;
   nSerialNr,nFuncNr: integer;
  constructor Init(const aPos:Position; aFunctorIdNr:integer; aArgs:PList;
                   aFuncKind:LocalDefinitionsKind; aNr,aFuncNr: integer);
  destructor Done; virtual;
 end;

 MSInternalSelectorTermPtr = ^MSInternalSelectorTermObj;
 MSInternalSelectorTermObj = object(InternalSelectorTermObj)
     nVarNr: integer;
    constructor Init(const aPos:Position; aSelectorNr,aVarNr:integer);
  end;

 MSPrivatePredicativeFormulaPtr = ^MSPrivatePredicativeFormulaObj;
 MSPrivatePredicativeFormulaObj = object(PrivatePredicativeFormulaObj)
   nPredKind: LocalDefinitionsKind;
   nSerialNr,nPredNr: integer;
  constructor Init(const aPos:Position; aPredIdNr:integer; aArgs:PList;
                   aPredKind:LocalDefinitionsKind; aNr,aPredNr: integer);
  destructor Done; virtual;
 end;

 MSReservedDscrTypePtr = ^MSReservedDscrTypeObj;
 MSReservedDscrTypeObj = object(TypeExpressionObj)
   nIdent: integer;
   nResTypeNr: integer;
   nResSubst: Int2PairOfIntFunc;
  constructor Init(const aPos:Position; aIdent:integer; aResTyp:integer);
  destructor Done; virtual;
 end;

 MSImplicitlyQualifiedSegmentPtr = ^MSImplicitlyQualifiedSegmentObj;
 MSImplicitlyQualifiedSegmentObj = object(ImplicitlyQualifiedSegmentObj)
    nResType: MSReservedDscrTypePtr;
   constructor Init(const aPos:Position; aIdent:VariablePtr; aResTyp:MSReservedDscrTypePtr);
   destructor Done; virtual;
  end;

 MSReservationSegmentPtr = ^MSReservationSegmentObj;
 MSReservationSegmentObj =
  object(ReservationSegmentObj)
    nResVars: NatSetPtr;
    nFreeVars: PList;
   constructor Init(aIdentifiers:PList; aType:TypePtr; const aVars:NatSet);
   destructor Done; virtual;
  end;

{----------------------------------------------------------------}

 FreeVarsInExprPtr =  ^FreeVarsInExprObj;
 FreeVarsInExprObj =
  object(WithinExprObj)
    nFreeVars: NatSet;
   constructor Init(aExpKind:ExpKind);
   destructor Done; virtual;
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
   procedure Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Complete_FreeVarsInExpr;
  end;

 Free2ResVarsInExprPtr =  ^Free2ResVarsInExprObj;
 Free2ResVarsInExprObj =
  object(WithinExprObj)
    nFreeVar: integer;
   constructor Init(aExpKind:ExpKind; aFreeVar:integer);
   destructor Done; virtual;
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
   procedure Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
  end;

 AdjustFreeVarScopePtr =  ^AdjustFreeVarScopeObj;
 AdjustFreeVarScopeObj =
  object(WithinExprObj)
    nFreeVar: integer;
    nAdjScopeNbr: integer;
    nAdjScopeIndex,nScopeIndex: string;
    nBranchNr:char;
    nFreeVarSets: array of NatSet;
   constructor Init(aExpKind:ExpKind);
   destructor Done; virtual;
   procedure Initialize(aFreeVar:integer);
   procedure AdjustFreeVarScope; virtual;
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
   procedure Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FinishFraenkelTerm(var aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FraenkelTermsScope ( var aFrm:FormulaPtr ); virtual;
   procedure Process_SimpleFraenkelTerm(var aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_QuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
  end;

 SetBoundVarStackedPtr = ^SetBoundVarStackedObj;
 SetBoundVarStackedObj =
  object(MObject)
    nVarNbr,nBoundVarNbr: integer;
   constructor Init;
   destructor Done; virtual;
  end;

 AddFreeVarInScopePtr =  ^AddFreeVarInScopeObj;
 AddFreeVarInScopeObj =
  object(WithinExprObj)
    nFreeVar: integer;
    nAdjScopeIndex,nScopeIndex: string;
    nBranchNr:char;
   constructor Init(aExpKind:ExpKind; aFreeVar:integer; aAdjScopeIndex:string);
   destructor Done; virtual;
   function CreateExpressionsVariableLevel: biStackedPtr; virtual;  
   procedure Process_Variable( var aVar: VariablePtr); virtual;
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
   procedure Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FinishFraenkelTerm(var aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FraenkelTermsScope ( var aFrm:FormulaPtr ); virtual;
   procedure Process_SimpleFraenkelTerm(var aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_QuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
  end;

{----------------------------------------------------------------}

 SetVarInExpressionPtr =  ^SetVarInExpressionObj;
 SetVarInExpressionObj =
  object(WithinExprObj)

   constructor Init(aExpKind:ExpKind);
   destructor Done; virtual;
   function CreateExpressionsVariableLevel: biStackedPtr; virtual;  
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
   procedure Process_Variable( var aVar: VariablePtr); virtual;
   procedure Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
  end;
{----------------------------------------------------------------}

 NoImplicitQualificationErrorPtr =  ^NoImplicitQualificationErrorObj;
 NoImplicitQualificationErrorObj =
  object(WithinExprObj)
    nFreeVar: integer;
   constructor Init(aExpKind:ExpKind; aFreeVar:integer);
   destructor Done; virtual;
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
  end;

{----------------------------------------------------------------}
 FirstIdItemPtr = ^FirstIdItemObj;
 FirstIdItemObj =
  object(biItemObj)
    nItemWithBlock: boolean;
    nLastPropsNbr,nLastLabelNbr: integer;
  constructor Init(aItemKind:ItemKind);
  end;

 FirstIdBlockPtr = ^FirstIdBlockObj;
 FirstIdBlockObj =
  object(biBlockObj)
    nLabNbr,nLabCntr,nPropsNbr,nLabelNr: integer;
    nLocFuncNbr,nLocPredNbr,
    nPrivFuncBase,nPrivPredBase,
    nVarNbr,nConstVarNbr: integer;
   constructor Init(aBlockKind:BlockKind);
  end;

 FirstIdSubexprPtr = ^FirstIdSubexprObj;
 FirstIdSubexprObj =
  object(biSubexpObj)
    nVarNbr: integer;
   constructor Init(aVarNbr:integer);
  end;

  FirstIdentArticlePtr = ^FirstIdentArticleObj;
  FirstIdentArticleObj = object(ProcessingArticleObj)

     nLastFreeVars: NatSet;
     nProcessingReservationSegment: boolean;

   constructor Init(aWSTextProper:WSTextProperPtr);
   destructor Done; virtual;
   function CreateBlock(fBlockKind:BlockKind): biBlockPtr; virtual;
   function CreateItem(fItemKind: ItemKind): biItemPtr; virtual;
   function CreateExpressionsVariableLevel: biSubexpPtr; virtual;

   procedure Process_Label(var aLab:LabelPtr); virtual;
   procedure Process_DefiniensLabel(var aLab:LabelPtr); virtual;
   procedure Process_PrivateReference(var aRef: LocalReferencePtr); virtual;
   procedure Process_StraightforwardJustification(aInf: StraightforwardJustificationPtr); virtual;
   procedure Process_SchemeJustification(var aInf: SchemeJustificationPtr); virtual;
   procedure Process_SchemeHead(var aSch:SchemePtr); virtual;
   procedure Process_SchemePremises(aSchema:SchemePtr); virtual;
   procedure Process_SchemeConclusion(var aFrm:FormulaPtr); virtual;

   procedure Process_StartBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_FinishBlock(aWSBlock:WSBlockPtr); virtual;

   procedure Process_Pragma(aPragma: PragmaPtr); virtual;

   procedure Process_ImplicitlyRedefinedVariable(aRes:TypeChangePtr); virtual;
   procedure Process_RedefinedVariable(aRes:TypeChangePtr); virtual;
   procedure Process_ExemplifyingVariable(aExampl:ExamplePtr); virtual;
   procedure Process_ImplicitExamplification(aExampl:ExamplePtr); virtual;
   procedure Process_DefConstant(var aVar: VariablePtr); virtual;
   procedure Process_FieldInStructureDefinition(aField:FieldSymbolPtr); virtual;
   procedure Process_FieldsInStructureDefinition(aStruct:StructureDefinitionPtr); virtual;
   procedure Process_ConstantDefinition(aDef:ConstantDefinitionPtr); virtual;

   procedure Process_StartItem(aWSItem:WSItemPtr); virtual;
   procedure Process_FinishItem(aWSItem:WSItemPtr); virtual;


   procedure Process_Variable( var aVar: VariablePtr); virtual;

   procedure Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Process_FixedVariable( var aVar: VariablePtr); virtual;
   procedure Process_ImplicitlyQualifiedFixedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Process_ChoiceVariable( var aVar: VariablePtr); virtual;
   procedure Process_ImplicitlyQualifiedChoiceVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;

   procedure Process_StartVariableSegment; virtual;
   procedure Process_FinishVariableSegment; virtual;
   procedure Process_FinishOneVariableSegment; virtual;
   procedure Process_StartReconsideringVariableSegment; virtual;
   procedure Process_FinishReconsideringVariableSegment; virtual;

   procedure Process_PrivatePredicateFormula ( var aFrm:PrivatePredicativeFormulaPtr ); virtual;

   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;

   procedure Process_PrivateFunctorTerm ( var aTrm: PrivateFunctorTermPtr ); virtual;
   procedure Process_InternalSelectorTerm( var aTrm: InternalSelectorTermPtr ); virtual;

   procedure Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr); virtual;

   procedure Process_FinishFraenkelTerm(aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FraenkelTermsScope ( var aFrm:FormulaPtr ); virtual;

   procedure Process_StartQuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;

   procedure Process_FinishQuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
   procedure Process_Locus( var aLocus: LocusPtr); virtual;
   procedure Process_Pattern(aPattern: PatternPtr); virtual;
   procedure Process_LociEqualities(aEqLociList:PList); virtual;

   procedure Process_StartReservedVariables( aResVars: ReservationSegmentPtr); virtual;
   procedure Process_FinishReservedVariables(var  aResVars: ReservationSegmentPtr); virtual;
   procedure Process_ReservedVariable( var aVar: VariablePtr); virtual;

   procedure Process_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_Proposition(aProp:PropositionPtr); virtual;
   procedure Process_DefiniensFormula(var aFrm:FormulaPtr); virtual;
   procedure Process_TypeWithFVScope ( var aTyp: TypePtr ); virtual;
   procedure Process_Term ( var aTrm: TermPtr ); virtual;
   procedure Process_TermWithFVScope ( var aTrm: TermPtr ); virtual;
   procedure Process_FormulaWithFVScope ( var aFrm:FormulaPtr ); virtual;

   procedure Process_StartSchemeSegment(aSgm:SchemeSegmentPtr); virtual;
   procedure Process_FinishSchemeSegment(aSgm:SchemeSegmentPtr); virtual;
   procedure Process_SchemePredicateVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;
   procedure Process_SchemeFunctorVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;

   procedure Process_StartPrivateFunctor; virtual;
   procedure Process_FinishPrivateFunctor; virtual;
   procedure Process_LocalFunctorVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;
   procedure Process_StartPrivatePredicate; virtual;
   procedure Process_FinishPrivatePredicate; virtual;
   procedure Process_LocalPredicateVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;

  end;


{----------------------------------------------------------------}

 MSOutMizFilePtr = ^MSOutMizFileObj;
 MSOutMizFileObj =
  object(OutWSMizFileObj)
   constructor OpenFile(const aFileName:string );
   destructor Done; virtual;

   procedure Out_Label(aLab:LabelPtr); virtual;
   procedure Out_LocalReference(aRef: LocalReferencePtr); virtual;
   procedure Out_Link(aInf: JustificationPtr); virtual;
   procedure Out_SchemeJustification(aInf: SchemeJustificationPtr); virtual;

   procedure Out_Variable( aVar: VariablePtr); virtual;
   procedure Out_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Out_ReservedVariable( aVar: VariablePtr); virtual;
   procedure Out_SimpleTerm ( aTrm: SimpleTermPtr ); virtual;
   procedure Out_Locus( aLocus: LocusPtr); virtual;
   procedure Out_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr ); virtual;
   procedure Out_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr ); virtual;
   procedure Out_InternalSelectorTerm ( aTrm: InternalSelectorTermPtr ); virtual;
   procedure Out_Type ( aTyp: TypePtr ); virtual;

   procedure Out_ReservationSegment(aRes:ReservationSegmentPtr); virtual;
   procedure Out_SchemeNameInSchemeHead(aSch: SchemePtr); virtual;
  end;

 MSInMizFilePtr = ^MSInMizFileObj;
 MSInMizFileObj =
  object(InWSMizFileObj)
   constructor OpenFile(const aFileName:string );
   destructor Done; virtual;

   function Read_ReservationSegment: ReservationSegmentPtr; virtual;
   function Read_SchemeNameInSchemeHead: SchemePtr; virtual;
   function Read_Label: LabelPtr; virtual;
   function Read_LocalReference: LocalReferencePtr; virtual;
   function Read_StraightforwardJustification: StraightforwardJustificationPtr; virtual;
   function Read_SchemeJustification: SchemeJustificationPtr; virtual;

   function Read_Locus: LocusPtr; virtual;
   function Read_Variable: VariablePtr; virtual;
   function Read_ImplicitlyQualifiedSegment: ImplicitlyQualifiedSegmentPtr; virtual;
   function Read_SimpleTerm: SimpleTermPtr; virtual;
   function Read_PrivateFunctorTerm: PrivateFunctorTermPtr; virtual;
   function Read_InternalSelectorTerm: InternalSelectorTermPtr; virtual;
   function Read_PrivatePredicativeFormula:PrivatePredicativeFormulaPtr; virtual;
   function Read_Type: TypePtr; virtual;

 end;

 MSMizarPrinterPtr =  ^MSMizarPrinterObj;
 MSMizarPrinterObj =
  object(WSMizarPrinterObj)
   constructor OpenFile(const aFileName:string );
   destructor Done; virtual;

   procedure Print_Label(aLab:LabelPtr); virtual;
   procedure Print_Reference(aRef: LocalReferencePtr); virtual;
   procedure Print_StraightforwardJustification(aInf: StraightforwardJustificationPtr); virtual;
   procedure Print_SchemeNameInSchemeHead(aSch: SchemePtr); virtual;
   procedure Print_SchemeNameInJustification(aInf: SchemeJustificationPtr); virtual;
   procedure Print_Linkage; virtual;

   procedure Print_Variable( aVar: VariablePtr); virtual;
   procedure Print_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Print_SimpleTermTerm ( aTrm: SimpleTermPtr ); virtual;
   procedure Print_Locus( aLocus: LocusPtr); virtual;
   procedure Print_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr ); virtual;
   procedure Print_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr ); virtual;

   procedure Print_ReservedType(aResType: TypePtr); virtual;
  end;

procedure Write_MSMizArticle(aMSTextProper:WSTextProperPtr; aFileName:string);
function Read_MSMizArticle(aFileName:string): WSTextProperPtr;
procedure Print_MSMizArticle(aMSTextProper:WSTextProperPtr; aFileName:string);
procedure MSMAnalyzer;

const
 VariableKindName: array[VariableKind] of string =
 (
  'Free',
  'Reserved',
  'Bound',
  'Constant',
  'DefConstant',
  'SchemataFunc',
  'PrivateFunc',
  'SchemataPred',
  'PrivatePred'
 );

const
  VarFirstLetter: array[VariableKind] of char = ('X','R','B','C','D','F','H','P','S');

implementation

uses mizenv, mconsole, parser, _formats, xml_dict, xml_parser, pragmas, limits
{$IFDEF MDEBUG} ,info {$ENDIF};

var
// Labels Identification
  gLabIdents,gDefLabels:
    array of record nLabId,nPropNr,nLabNr: integer; nDefiniensLab,nPending: boolean; end;
  gLabNbr,gLabCntr,gDefLabNbr,gDefLabBase: integer;
  gPropsNbr: integer;
// SchemeIdentification
  gSchemeIdent: NatSeq;
  gCanceledSch: integer;
// Variables identification
  gVariable: array of
    record
     nVarIdent:integer;
     nVarPos:Position;
     nOrigin,nVarKind: VariableKind;
     nSerialNr: integer;
     nVarNr: integer;
     nPending: boolean;
    end;
  gVarNbr,gVarSegmentBase,gRecVarSegmentBase,gDefVarBase: integer;
  gBoundVarNbr,gConstVarNbr: integer;
  gBoundsSerialNbr,gConstsSerialNbr,gDefConstsSerialNbr: integer;

  Involved: IntSet;
  gSkelConstSet: IntSet;

// Second order variables identification
  gPredVars: array of
    record
     nPredId: integer;
     nPredIdPos: Position;
     nPredArity: integer;
     nPredKind: LocalDefinitionsKind;
     nSerialNr,nPredNr: integer;
    end;
  gFuncVars: array of
    record
     nFuncId: integer;
     nFuncIdPos: Position;
     nFuncArity: integer;
     nFuncKind: LocalDefinitionsKind;
     nSerialNr: integer;
     nFuncNr: integer;
    end;
  gLocFuncNbr,gLocPredNbr,
  gAvailableLocPredNbr,gAvailableLocFuncNbr: integer;
  gSerialPrivFuncNbr,gSerialPrivPredNbr,
  gPrivFuncNbr,gPrivPredNbr: integer;
  gSchFuncNbr,gSchPredNbr,
  gSchFuncBase,gSchPredBase: integer;
  gArityOfLocal: integer;
  gFieldNbr:integer;
  gField: array of integer;
// Reserved Variables
  gResVarNbr,gResVarBase,gResTypeNbr:integer;
  gReservedVar: array of
    record
     nResVarId:Integer;
     nResVarPos:Position;
     nResVarNr:integer;
     nPending: boolean;
     nResTypeNr: integer;
     nFreeVars: NatSet;
    end;
  gReservedType: array of
    record
     nTyp: TypePtr;
     nFreeArgs: NatSet;
    end;

function LookUp_Var(aId:integer): integer;
 var k:integer;
begin
 for k:=gVarNbr downto 1 do
  with gVariable[k] do
   if aId = nVarIdent then
    begin
     result:=k;
     exit;
    end;
 result:=0;
end;

function LookUp_ReservedVar(aId:integer): integer;
 var k:integer;
begin
 for k:=gResVarNbr downto 1 do
  with gReservedVar[k] do
   if aId = nResVarId then
    begin
     result:=k;
     exit;
    end;
 result:=0;
end;

procedure AppVar(aIdent:Integer; aVarOrigin,aVarKind:VariableKind; const aPos:Position);
begin
 inc(gVarNbr);
 if gVarNbr >= length(gVariable) then
  setlength(gVariable,2*length(gVariable));
 with gVariable[gVarNbr] do
  begin
   nOrigin:=aVarOrigin;
   nVarKind:=aVarKind;
   nVarPos:=aPos;
   nVarIdent:=aIdent;
   nPending:=true;
   case aVarOrigin of
    Bound:
     begin
      inc(gBoundsSerialNbr);
      nSerialNr:=gBoundsSerialNbr;  //nVarNbr-nVarBase;
      inc(gBoundVarNbr);
      nVarNr:=gBoundVarNbr;
     end;
    Constant:
     begin
      inc(gConstsSerialNbr);
      nSerialNr:=gConstsSerialNbr;  //nVarNbr;
      inc(gConstVarNbr);
      nVarNr:=gConstVarNbr;
     end;
    ReservedVar:
     begin
      nOrigin:=ReservedVar;
      case aVarKind of
      Bound:
       begin
        inc(gBoundVarNbr);
        nVarNr:=gBoundVarNbr;
       end;
      Constant:
       begin
        inc(gConstVarNbr);
        nVarNr:=gConstVarNbr;
       end;
      end;
     end;
    FreeVar:
     nSerialNr:=gResVarNbr;  //??
    DefConstant:
     begin
      inc(gDefConstsSerialNbr);
      nSerialNr:=gDefConstsSerialNbr;  //nVarNbr;
      inc(gConstVarNbr);
      nVarNr:=gConstVarNbr;
     end;
   end;
  end;
end;

constructor MSVariableObj.Init(const aPos:Position; aIdentNr:integer;
                               aOrigin,aVarKind:VariableKind; aNr,aVarNr:integer);
begin
 inherited Init(aPos,aIdentNr);
 nOrigin:=aOrigin;
 nVarKind:=aVarKind;
 nSerialNr:=aNr;
 nVarNr:=aVarNr;
end;

constructor MSLocusObj.Init(const aPos:Position; aIdentNr:integer;
                    aOrigin,aVarKind:VariableKind; aNr,aVarNr:integer);
begin
 inherited Init(aPos,aIdentNr);
 nOrigin:=aOrigin;
 nVarKind:=aVarKind;
 nSerialNr:=aNr;
 nVarNr:=aVarNr;
end;

constructor MSSimpleTermObj.Init(const aPos:Position; aIdentNr:integer;
                    aOrigin,aVarKind: VariableKind; aNr,aVarNr:integer);
begin
 inherited Init(aPos,aIdentNr);
 nOrigin:=aOrigin;
 nVarKind:=aVarKind;
 nSerialNr:=aNr;
 nVarNr:=aVarNr;
end;

constructor MSPrivateFunctorTermObj.Init(const aPos:Position; aFunctorIdNr:integer;
                                    aArgs:PList; aFuncKind:LocalDefinitionsKind; aNr,aFuncNr: integer);
begin
 inherited Init(aPos,aFunctorIdNr,aArgs);
 nFuncKind:=aFuncKind;
 nSerialNr:=aNr;
 nFuncNr:=aFuncNr;
end;

destructor MSPrivateFunctorTermObj.Done;
begin inherited Done;
end;

constructor MSInternalSelectorTermObj.Init(const aPos:Position; aSelectorNr,aVarNr:integer);
begin
 inherited Init(aPos,aSelectorNr);
 nVarNr:=aVarNr;
end;

constructor MSPrivatePredicativeFormulaObj.Init(const aPos:Position; aPredIdNr:integer;
                                            aArgs:PList; aPredKind:LocalDefinitionsKind; aNr,aPredNr:integer);
begin
 inherited Init(aPos,aPredIdNr,aArgs);
 nPredKind:=aPredKind;
 nSerialNr:=aNr;
 nPredNr:=aPredNr;
end;

destructor MSPrivatePredicativeFormulaObj.Done;
begin inherited Done;
end;

constructor MSReservedDscrTypeObj.Init(const aPos:Position; aIdent:integer; aResTyp:integer);
begin
 nTypePos:=aPos;
 nTypeSort:=wsReservedDscrType;
 nIdent:=aIdent;
 nResTypeNr:=aResTyp;
 nResSubst.Init(0);
end;

destructor MSReservedDscrTypeObj.Done;
begin inherited Done;
 nResSubst.Done;
end;

constructor MSImplicitlyQualifiedSegmentObj.Init(const aPos:Position; aIdent:VariablePtr; aResTyp:MSReservedDscrTypePtr);
begin
 inherited Init(aPos,aIdent);
 nResType:=aResTyp;
 end;

destructor MSImplicitlyQualifiedSegmentObj.Done;
begin inherited Done;
 dispose(nResType,Done);
end;

constructor MSReservationSegmentObj.Init(aIdentifiers:PList; aType:TypePtr; const aVars:NatSet);
begin
 inherited Init(aIdentifiers,aType);
 nResVars:=new(NatSetPtr,CopyNatSet(aVars));
 nFreeVars:=new(PList,Init(0));
end;

destructor MSReservationSegmentObj.Done;
begin
  inherited Done;
  dispose(nResVars,Done);
end;

{----------------------------------------------------------------}

constructor FreeVarsInExprObj.Init(aExpKind:ExpKind);
begin
 inherited Init(aExpKind);
 nFreeVars.Init(0,MaxResIdNbr);
end;

destructor FreeVarsInExprObj.Done;
begin
 inherited Done;
end;

procedure FreeVarsInExprObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
begin
 with MSSimpleTermPtr(aTrm)^ do
 begin
  if nOrigin = FreeVar then
   begin
    nFreeVars.InsertElem( nSerialNr);
   end;
 end;
end;

procedure FreeVarsInExprObj.Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
 var i,k: integer;
begin
 Process_Variable( aSegm^.nIdentifier);
 with MSImplicitlyQualifiedSegmentPtr(aSegm)^ do
  for i:=0 to nResType^.nResSubst.fCount-1 do
    with nResType^.nResSubst.fList[i] do
     if VariableKind(Y1) = ReservedVar then
      nFreeVars.InsertElem(Y2);
end;

procedure Complete_FreeVars(var aFreeVars: NatSet);
 var lFreeVars:  NatSet;
     i,lVarNr: integer;
begin
  lFreeVars.CopyNatSet(aFreeVars);
  while lFreeVars.Count > 0 do
   with gReservedVar[lFreeVars.Items^[lFreeVars.Count-1].X] do
   begin
    for i:=0 to gReservedType[nResTypeNr].nFreeArgs.Count-1 do
     begin
      lVarNr:=LookUp_Var(gReservedVar[gReservedType[nResTypeNr].nFreeArgs.Items^[i].X].nResVarId);
      if lVarNr = 0 then
       begin
        lFreeVars.InsertElem(gReservedType[nResTypeNr].nFreeArgs.Items^[i].X);
        aFreeVars.InsertElem(gReservedType[nResTypeNr].nFreeArgs.Items^[i].X);
       end;
     end;
    lFreeVars.AtDelete(lFreeVars.Count-1);
   end;
  lFreeVars.Done;
end;

procedure FreeVarsInExprObj.Complete_FreeVarsInExpr;
begin
 Complete_FreeVars(nFreeVars);
end;

{----------------------------------------------------------------}

constructor Free2ResVarsInExprObj.Init(aExpKind:ExpKind; aFreeVar:integer);
begin
 inherited Init(aExpKind);
 nFreeVar:=aFreeVar;
end;

destructor Free2ResVarsInExprObj.Done;
begin
 inherited Done;
end;

procedure Free2ResVarsInExprObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
begin
 with MSSimpleTermPtr(aTrm)^ do
 begin
  if (nOrigin = FreeVar) and (nSerialNr = nFreeVar) then
   begin
    nOrigin:=ReservedVar;
   end;
 end;
end;

procedure Free2ResVarsInExprObj.Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Process_Variable( aSegm^.nIdentifier);
end;

{----------------------------------------------------------------}

constructor AdjustFreeVarScopeObj.Init(aExpKind:ExpKind);
begin
 inherited Init(aExpKind);
 Initialize(0);
end;

procedure AdjustFreeVarScopeObj.Initialize(aFreeVar:integer);
begin
 nScopeIndex:='';
 nAdjScopeIndex:='';
 nAdjScopeNbr:=0;
 nBranchNr:=pred('A');
 nFreeVar:=aFreeVar;
end;

destructor AdjustFreeVarScopeObj.Done;
begin
 inherited Done;
end;

procedure AdjustFreeVarScopeObj.AdjustFreeVarScope;
 var lDifference,i: integer;
 label 1;
begin
  if nAdjScopeNbr >0 then
   begin
    if  nAdjScopeIndex <> nScopeIndex then
     begin
       if length(nAdjScopeIndex) <= length(nScopeIndex) then
        lDifference:=length(nAdjScopeIndex)
       else lDifference:=length(nScopeIndex);
       for i:=1 to lDifference do
        if nAdjScopeIndex[i] <> nScopeIndex[i] then
         begin lDifference:=i; goto 1 end;
       inc(lDifference);
1:
       delete(nAdjScopeIndex,lDifference,length(nAdjScopeIndex));
     end;
   end
  else nAdjScopeIndex:=nScopeIndex;
  inc(nAdjScopeNbr);
end;

procedure AdjustFreeVarScopeObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
 var lDifference,k:integer; //FreeIndex: string;
begin
 with MSSimpleTermPtr(aTrm)^ do
 if nOrigin in [FreeVar,ReservedVar] then
 begin
  if nSerialNr = nFreeVar then
   begin
    AdjustFreeVarScope;
   end
  else
   with gReservedType[gReservedVar[nSerialNr].nResTypeNr] do
   begin
    for k:=0 to nFreeArgs.Count - 1 do
     with gReservedVar[nFreeArgs.Items^[k].X] do
      if nResVarNr = nFreeVar then
       begin
        AdjustFreeVarScope;
        exit;
       end
   end;
 end;
end;

procedure AdjustFreeVarScopeObj.Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
 var lDifference,i:integer;
 label 1;
begin
 with MSImplicitlyQualifiedSegmentPtr(aSegm)^ do
 begin
  Process_Variable( nIdentifier);
  if gReservedType[gReservedVar[MSVariablePtr(aSegm^.nIdentifier)^.nSerialNr].nResTypeNr].nFreeArgs.HasInDom(nFreeVar)
      and
     not MSImplicitlyQualifiedSegmentPtr(aSegm)^.nResType^.nResSubst.HasInDom(nFreeVar) then
   AdjustFreeVarScope;
 end;

end;

procedure AdjustFreeVarScopeObj.Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr);
begin
 inc(nBranchNr);
 nScopeIndex:=nScopeIndex+nBranchNr;
 nBranchNr:=pred('A');
end;

procedure AdjustFreeVarScopeObj.Process_FinishFraenkelTerm(var aTrm:SimpleFraenkelTermPtr);
begin
 nBranchNr:=nScopeIndex[length(nScopeIndex)];
 delete(nScopeIndex,length(nScopeIndex),1);
// if length(nScopeIndex) = 0 then exit;
end;

procedure AdjustFreeVarScopeObj.Process_FraenkelTermsScope ( var aFrm:FormulaPtr );
begin
 inc(nBranchNr); nScopeIndex:=nScopeIndex+nBranchNr;
 nBranchNr:=pred('A');
 Process_Formula(aFrm);
 nBranchNr:=nScopeIndex[length(nScopeIndex)];
 delete(nScopeIndex,length(nScopeIndex),1);
end;

procedure AdjustFreeVarScopeObj.Process_SimpleFraenkelTerm(var aTrm:SimpleFraenkelTermPtr);
 var i: integer;
begin
 with aTrm^ do
 begin
  for i := 0 to nPostqualification^.Count - 1 do
   Process_VariablesSegment(QualifiedSegmentPtr(nPostqualification^.Items^[i]));
  for i := 0 to nPostqualification^.Count - 1 do
    if (QualifiedSegmentPtr(nPostqualification^.Items^[i])^.nSegmentSort = ikImplQualifiedSegm) and
     (MSVariablePtr(ImplicitlyQualifiedSegmentPtr(nPostqualification^.Items^[i]).nIdentifier)^.nSerialNr = nFreeVar)then
   exit;
  Process_Term(nSample);
 end;
end;

procedure AdjustFreeVarScopeObj.Process_QuantifiedFormula ( aFrm: QuantifiedFormulaPtr );
begin
 Process_VariablesSegment(aFrm^.nSegment);
 if (aFrm^.nSegment^.nSegmentSort = ikImplQualifiedSegm) and
     (MSVariablePtr(ImplicitlyQualifiedSegmentPtr(aFrm^.nSegment).nIdentifier)^.nSerialNr = nFreeVar)then
   exit;
 Process_Formula(aFrm^.nScope);
end;

{----------------------------------------------------------------}

constructor SetBoundVarStackedObj.Init;
begin
  nVarNbr:=gVarNbr;
  nBoundVarNbr:=gBoundVarNbr;
end;

destructor SetBoundVarStackedObj.Done;
begin
  gVarNbr:=nVarNbr;
  gBoundVarNbr:=nBoundVarNbr;
end;

{----------------------------------------------------------------}

constructor AddFreeVarInScopeObj.Init(aExpKind:ExpKind; aFreeVar:integer; aAdjScopeIndex:string);
begin
 inherited Init(aExpKind);
 nScopeIndex:='';
 nBranchNr:=pred('A');
 nFreeVar:=aFreeVar;
 nAdjScopeIndex:=aAdjScopeIndex;

 gBoundVarNbr:=0;
end;

destructor AddFreeVarInScopeObj.Done;
begin
 inherited Done;
end;

function AddFreeVarInScopeObj.CreateExpressionsVariableLevel: biStackedPtr;
begin
 result:=biStackedPtr(new(SetBoundVarStackedPtr, Init));
end;

procedure AddFreeVarInScopeObj.Process_Variable( var aVar: VariablePtr);
begin
 with MSVariablePtr(aVar) ^ do
 begin
  AppVar(nIdent,nOrigin,Bound,nVarPos);
  nVarKind:=Bound;
  nVarNr:=gBoundVarNbr;
 end;
end;

procedure AddFreeVarInScopeObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
 var lVarNr: integer;
begin
 with MSSimpleTermPtr(aTrm)^ do
 begin
  if (nOrigin = FreeVar) and (nSerialNr = nFreeVar) then
  begin
    nOrigin:=ReservedVar;
    nVarKind:=Bound;
  end;
 end;
end;

procedure AssignFreeVars(const aPos:Position; aFreeVar: integer; var aResSubst: Int2PairOfIntFunc);
 var i,lVarNr: integer;
begin
 with gReservedType[gReservedVar[aFreeVar].nResTypeNr] do
 begin
  for i:=0 to nFreeArgs.Count - 1 do
   with gReservedVar[nFreeArgs.Items^[i].X] do
   begin
     lVarNr:=LookUp_Var(nResVarId);
     if lVarNr <> 0 then
      begin
       if not (gVariable[lVarNr].nOrigin in [FreeVar,ReservedVar]) then
         Error(aPos,163);
       aResSubst.Assign(nFreeArgs.Items^[i].X,ord(gVariable[lVarNr].nVarKind),gVariable[lVarNr].nVarNr);
      end
     else aResSubst.Assign(nFreeArgs.Items^[i].X,ord(ReservedVar),nResVarNr);
   end;
 end;
end;

procedure AddFreeVarInScopeObj.Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr);
begin
 inc(nBranchNr); nScopeIndex:=nScopeIndex+nBranchNr;
 nBranchNr:=pred('A');
end;

procedure AddFreeVarInScopeObj.Process_FinishFraenkelTerm(var aTrm:SimpleFraenkelTermPtr);
 var lF2RV: Free2ResVarsInExprPtr;
     lVar: MSVariablePtr;
     lSemList: PList;
     i: integer;
     lSegm: MSImplicitlyQualifiedSegmentPtr;
begin
 if nScopeIndex = nAdjScopeIndex then
  with aTrm^,gReservedVar[nFreeVar] do
   begin
    lF2RV:=new(Free2ResVarsInExprPtr, Init(exFormula,0));
    lVar:=new(MSVariablePtr,Init(nTermPos,nResVarId,ReservedVar,Bound,nFreeVar,0));
    lSegm:=new(MSImplicitlyQualifiedSegmentPtr,Init(nTermPos,lVar,new(MSReservedDscrTypePtr,Init(nTermPos,lVar^.nIdent,nResTypeNr))));
    AssignFreeVars(aTrm^.nTermPos,nFreeVar,lSegm^.nResType^.nResSubst);
    lSemList:=New(PList,Init(0));
    lSemList^.Insert(lSegm);
    for i:=0 to nPostqualification^.Count-1 do
      lSemList^.Insert(nPostqualification^.Items^[i]);
    nPostqualification^.DeleteAll;
    dispose(nPostqualification,Done);
    nPostqualification:=lSemList;
    lF2RV^.nFreeVar:=nFreeVar;
    lF2RV^.Process_Term(TermPtr(aTrm));
   end;
 nBranchNr:=nScopeIndex[length(nScopeIndex)];
 delete(nScopeIndex,length(nScopeIndex),1);
end;

procedure AddFreeVarInScopeObj.Process_FraenkelTermsScope ( var aFrm:FormulaPtr );
 var lF2RV: Free2ResVarsInExprPtr;
     lVar: MSVariablePtr;
     lSegm: MSImplicitlyQualifiedSegmentPtr;
begin
 inc(nBranchNr); nScopeIndex:=nScopeIndex+nBranchNr;
 nBranchNr:=pred('A');
 Process_Formula(aFrm);
 if nScopeIndex = nAdjScopeIndex then
  with gReservedVar[nFreeVar] do
   begin
    lF2RV:=new(Free2ResVarsInExprPtr, Init(exFormula,0));
    lVar:=new(MSVariablePtr,Init(aFrm^.nFormulaPos,nResVarId,ReservedVar,Bound,nFreeVar,0));
    lSegm:=new(MSImplicitlyQualifiedSegmentPtr,Init(aFrm^.nFormulaPos,lVar,
                  new(MSReservedDscrTypePtr,Init(aFrm^.nFormulaPos,lVar^.nIdent,nResTypeNr))));
    AssignFreeVars(aFrm^.nFormulaPos,nFreeVar,lSegm^.nResType^.nResSubst);
    aFrm:=new(UniversalFormulaPtr,Init(aFrm^.nFormulaPos,lSegm,aFrm));
    lF2RV^.nFreeVar:=nFreeVar;
    lF2RV^.Process_Formula(aFrm);
   end;
 nBranchNr:=nScopeIndex[length(nScopeIndex)];
 delete(nScopeIndex,length(nScopeIndex),1);
end;

procedure AddFreeVarInScopeObj.Process_SimpleFraenkelTerm(var aTrm:SimpleFraenkelTermPtr);
 var i: integer;
begin
 with aTrm^ do
 begin
  for i := 0 to nPostqualification^.Count - 1 do
   Process_VariablesSegment(QualifiedSegmentPtr(nPostqualification^.Items^[i]));
  for i := 0 to nPostqualification^.Count - 1 do
    if (QualifiedSegmentPtr(nPostqualification^.Items^[i])^.nSegmentSort = ikImplQualifiedSegm) and
     (MSVariablePtr(ImplicitlyQualifiedSegmentPtr(nPostqualification^.Items^[i]).nIdentifier)^.nSerialNr = nFreeVar)then
   exit;
  Process_Term(nSample);
 end;
end;

procedure AddFreeVarInScopeObj.Process_QuantifiedFormula ( aFrm: QuantifiedFormulaPtr );
begin
 Process_VariablesSegment(aFrm^.nSegment);
 if (aFrm^.nSegment^.nSegmentSort = ikImplQualifiedSegm) and
     (MSVariablePtr(ImplicitlyQualifiedSegmentPtr(aFrm^.nSegment).nIdentifier)^.nSerialNr = nFreeVar)then
   exit;
 Process_Formula(aFrm^.nScope);
end;

{----------------------------------------------------------------}

constructor SetVarInExpressionObj.Init(aExpKind:ExpKind);
begin
 inherited Init(aExpKind);
 gBoundVarNbr:=0;
end;

destructor SetVarInExpressionObj.Done;
begin
 inherited Done;
end;

function SetVarInExpressionObj.CreateExpressionsVariableLevel: biStackedPtr;
begin
 result:=biStackedPtr(new(SetBoundVarStackedPtr, Init));
end;

procedure SetVarInExpressionObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
 var lVarNr: integer;
begin
 with MSSimpleTermPtr(aTrm)^ do
  if nVarKind = Bound then
   begin
    lVarNr:=LookUp_Var(nIdent);
//    mizassert(2310,lVarNr <> 0);
    nVarNr:=gVariable[lVarNr].nVarNr;
   end
  else if nOrigin = ReservedVar then
   begin
    lVarNr:=LookUp_Var(nIdent);
    nVarKind:=gVariable[lVarNr].nVarKind;
    nVarNr:=gVariable[lVarNr].nVarNr;
   end;
end;

procedure SetVarInExpressionObj.Process_Variable( var aVar: VariablePtr);
begin
 with MSVariablePtr(aVar) ^ do
 begin
  AppVar(nIdent,nOrigin,Bound,nVarPos);
  nVarKind:=Bound;
  nVarNr:=gBoundVarNbr;
 end;
end;

procedure SetVarInExpressionObj.Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
 var lVarNr,lNr,i: integer;
     lResSubst: Int2PairOfIntFunc;
begin
 with MSImplicitlyQualifiedSegmentPtr(aSegm)^ do
 begin
   Process_Variable(nIdentifier);
   lResSubst.Init(nResType^.nResSubst.fCount);
   with nResType^.nResSubst do
    begin
     for i:=0 to fCount - 1 do
      if VariableKind(fList[i].Y1) in [ReservedVar,Bound] then
       with gReservedVar[fList[i].X] do
       begin
         lVarNr:=LookUp_Var(nResVarId);
         if lVarNr <> 0 then
          with gVariable[lVarNr] do
          begin
           fList[i].Y1:=ord(nVarKind);
           fList[i].Y2:=nVarNr;
          end
       end
    end;
 end;
end;

{----------------------------------------------------------------}

constructor NoImplicitQualificationErrorObj.Init(aExpKind:ExpKind; aFreeVar:Integer);
begin
 inherited Init(aExpKind);
 nFreeVar:=aFreeVar;
end;

destructor NoImplicitQualificationErrorObj.Done;
begin
 inherited Done;
end;

procedure NoImplicitQualificationErrorObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
 var k:integer;
begin
 with MSSimpleTermPtr(aTrm)^ do
 if nOrigin in [FreeVar,ReservedVar] then
 begin
  if (nFreeVar = 0) or (nSerialNr = 0) then exit;
  if nSerialNr = nFreeVar then
   begin
    Error(nTermPos,143);
   end
  else
   with gReservedType[gReservedVar[nSerialNr].nResTypeNr] do
   begin
    for k:=0 to nFreeArgs.Count - 1 do
     with gReservedVar[nFreeArgs.Items^[k].X] do
      if nResVarNr = nFreeVar then
       begin
        Error(nTermPos,143);
        exit;
       end
   end;
 end;
end;

{----------------------------------------------------------------}

procedure AppFuncVar(aId:integer; aPos:Position; aFuncVarKind:LocalDefinitionsKind; aNr,aFuncNr: integer; aArity:integer);
begin Inc(gLocFuncNbr);
 if gLocFuncNbr >= length(gFuncVars) then
  setlength(gFuncVars,2*length(gFuncVars));
 with gFuncVars[gLocFuncNbr] do
  begin
   nFuncId:=aId;
   nFuncIdPos:=aPos;
   nFuncArity:=aArity;
   nFuncKind:=aFuncVarKind;
   nSerialNr:=aNr;
   nFuncNr:=aFuncNr;
  end;
end;

procedure AppPredVar(aId:integer; aPos:Position; aPredVarKind:LocalDefinitionsKind; aNr,aPredNr: integer; aArity:integer);
begin Inc(gLocPredNbr);
 if gLocPredNbr >= length(gPredVars) then
  setlength(gPredVars,2*length(gPredVars));
 with gPredVars[gLocPredNbr] do
  begin
   nPredId:=aId;
   nPredIdPos:=aPos;
   nPredArity:=aArity;
   nPredKind:=aPredVarKind;
   nSerialNr:=aNr;
   nPredNr:=aPredNr;
  end;
end;

function LookUp_PredVar(aId:integer; aArity:integer): integer;
 var k:integer;
begin
 for k:=gAvailableLocPredNbr downto 1 do
  with gPredVars[k] do
   if (nPredId = aId) and (nPredArity = aArity) then
    begin
     result:=k;
     exit;
    end;
 result:=0;
end;

function LookUp_FuncVar (aId:integer; aArity:integer): integer;
 var k:integer;
begin
 for k:=gAvailableLocFuncNbr downto 1 do
  with gFuncVars[k] do
   if (nFuncId = aId) and (nFuncArity = aArity) then
    begin
     result:=k;
     exit;
    end;
 result:=0;
end;

{----------------------------------------------------------------}

constructor FirstIdItemObj.Init(aItemKind:ItemKind);
begin inherited Init(aItemKind);
end;

constructor FirstIdBlockObj.Init(aBlockKind:BlockKind);
begin inherited Init(aBlockKind);
 nLabNbr:=0;
 nPropsNbr:=0;
end;

constructor FirstIdSubexprObj.Init(aVarNbr:integer);
begin
 nVarNbr:=aVarNbr;
end;

{-------------------------------------------------------------------------}

function AnalyzeSimpleTerm ( aIdent: integer; const aTermPos:Position ): MSSimpleTermPtr;
 var lVarNr,lSerialNr,lNr: integer;
     lTrmPtr: MSSimpleTermPtr;
     lOrigin,lVarKind: VariableKind;
begin
  lVarNr:=LookUp_Var(aIdent);
  lVarKind:=Constant;
  lOrigin:=Constant;
  lSerialNr:=0;
  lNr:=0;
  if lVarNr <> 0 then
   begin
     if not gVariable[lVarNr].nPending then
      begin
       lOrigin:=gVariable[lVarNr].nOrigin;
       lVarKind:=gVariable[lVarNr].nVarKind;
       lSerialNr:=gVariable[lVarNr].nSerialNr;
       lNr:=gVariable[lVarNr].nVarNr;
      end
     else Error(aTermPos,160);
   end
  else
   begin
     lVarNr:=LookUp_ReservedVar(aIdent);
     lOrigin:=FreeVar;
     lVarKind:=FreeVar;
     if lVarNr <> 0 then
      begin
       if gReservedVar[lVarNr].nPending then
         Error(aTermPos,160);
       lSerialNr:=gReservedVar[lVarNr].nResVarNr;
      end
     else Error(aTermPos,140);
   end;
  lTrmPtr:=new(MSSimpleTermPtr,Init(aTermPos,aIdent,lOrigin,lVarKind,lSerialNr,lNr));
  result:=lTrmPtr;
end;

{-------------------------------------------------------------------------}

constructor FirstIdentArticleObj.Init(aWSTextProper:WSTextProperPtr);
begin
 inherited Init(aWSTextProper);
// Labels Identification
 CurPos.Line:=1;
 CurPos.Col:=1;
 gLabNbr:=0;
 gLabCntr:=0;
 gDefLabBase:=0;
 setlength(gLabIdents,MaxLabNbr);
 gPropsNbr:=0;
 gDefLabNbr:=0;
 setlength(gDefLabels,MaxDefLabNbr);

 gSchemeIdent.Init(0,8);
 gCanceledSch:=0;

// Variables Identification
 gVarNbr:=0;
 gDefVarBase:=0;

 gBoundsSerialNbr:=0;
 gConstsSerialNbr:=0;
 gDefConstsSerialNbr:=0;

 gBoundVarNbr:=0;
 gConstVarNbr:=0;

 setlength(gVariable,MaxVarNbr);
 with gVariable[0] do
  begin
    nVarIdent:=0;
    nOrigin:=FreeVar;
    nSerialNr:=0;
    nVarKind:=FreeVar;
    nVarNr:=0;
    nVarPos:=CurPos;
    nPending:=true;
  end;
 gVarSegmentBase:=0;
 gRecVarSegmentBase:=0;
 setlength(gField,MaxArgNbr);
 gField[0]:=0;
 Involved.Init(0);
 gSkelConstSet.Init(0);
// Reserved Variable
 gResVarNbr:=0;
 gResVarBase:=0;
 gResTypeNbr:=0;
 setlength(gReservedVar,MaxResIdNbr);
 setlength(gReservedType,MaxResIdNbr);
 nProcessingReservationSegment:=false;
//  Second odrder variavles
 setlength(gPredVars,MaxPredVarNbr);
 setlength(gFuncVars,MaxFuncVarNbr);
 with gPredVars[0] do
  begin
   nPredId:=0;
   nPredArity:=0;
   nPredKind:=PrivatePred;
   nSerialNr:=0;
   nPredNr:=0;
  end;
 with gFuncVars[0] do
  begin
   nFuncId:=0;
   nFuncArity:=0;
   nFuncKind:=PrivateFunc;
   nSerialNr:=0;
   nFuncNr:=0;
  end;
 with gReservedVar[0] do
  begin
   nResVarId:=0;
   nResVarNr:=0;
   nResVarPos.Line:=1;
   nResVarPos.Col:=1;
   nResTypeNr:=0;
   nPending:=true;
   nFreeVars.Init(0,4);
  end;
 with gReservedType[0] do
  begin
   nTyp:=new(IncorrectTypePtr,Init(CurPos));
   nFreeArgs.Init(0,0);
  end;

 nLastFreeVars.Init(0,0);
 gLocFuncNbr:=0;
 gLocPredNbr:=0;
 gAvailableLocFuncNbr:=gLocFuncNbr;
 gAvailableLocPredNbr:=gLocPredNbr;
 gSchPredNbr:=0;
 gSchFuncNbr:=0;
 gSchFuncBase:=0;
 gSchPredBase:=0;
 gSerialPrivPredNbr:=0;
 gSerialPrivFuncNbr:=0;
 gPrivFuncNbr:=0;
 gPrivPredNbr:=0;
 gArityOfLocal:=0;
end;

destructor FirstIdentArticleObj.Done;
begin
 gSchemeIdent.Done;
 inherited Done;
end;

function FirstIdentArticleObj.CreateBlock(fBlockKind:BlockKind): biBlockPtr;
begin
 result:=new(FirstIdBlockPtr,Init(fBlockKind));
end;

function FirstIdentArticleObj.CreateItem(fItemKind:ItemKind): biItemPtr;
begin
 result:=new(FirstIdItemPtr, Init(fItemKind));
end;

function FirstIdentArticleObj.CreateExpressionsVariableLevel: biSubexpPtr;

begin
 result:=new(FirstIdSubexprPtr, Init(gVarNbr));
end;

procedure FirstIdentArticleObj.Process_PrivateReference(var aRef: LocalReferencePtr);
 var k,lRefNr,lLabNr:integer;
     lRef: LocalReferencePtr;
 label Found;
begin
 for k:=gLabNbr downto 1 do
  if gLabIdents[k].nLabId = aRef^.nLabId then
   begin
    lRefNr:=gLabIdents[k].nPropNr;
    lLabNr:=gLabIdents[k].nLabNr;
    if gLabIdents[k].nPending then
     begin
      Error(aRef^.nRefPos,145);
      lRefNr:=0;
      lLabNr:=0;
     end;
    goto Found;
   end;
 Error(aRef^.nRefPos,144);
 lRefNr:=0;
 lLabNr:=0;
Found:
 lRef:=aRef;
 with aRef^ do
  aRef:=new(MSLocalReferencePtr,Init(nLabId,nRefPos,lRefNr,lLabNr));
 dispose(lRef,Done);
end;

procedure FirstIdentArticleObj.Process_Label(var aLab:LabelPtr);
 var lLab: LabelPtr;
     lLabelNr: integer;
begin
 inc(gPropsNbr);
 if aLab <> nil then
  begin
   lLab:=aLab;
   with lLab^ do
   begin
    lLabelNr:=0;
    if nLabelIdNr > 0 then
     begin
      if gLabNbr >= length(gLabIdents) then
        setlength(gLabIdents,2*length(gLabIdents));
      inc(gLabNbr);
      inc(gLabCntr);
      lLabelNr:=gLabNbr-gDefLabNbr;
      with gLabIdents[gLabNbr] do
       begin
	nLabId:=nLabelIdNr;
        nPropNr:=gPropsNbr;
        nLabNr:=gLabCntr;
        nPending:=true;
        nDefiniensLab:=false;
       end;
     end;
    aLab:=new(MSLabelPtr,Init(nLabelIdNr,nLabelPos,gPropsNbr,lLabelNr));
    dispose(lLab,Done);
   end;
  end
 else
  aLab:=new(MSLabelPtr,Init(0,CurPos,gPropsNbr,0));
end;

procedure FirstIdentArticleObj.Process_DefiniensLabel(var aLab:LabelPtr);
 var lLab: LabelPtr;
     lLabelNr: integer;
begin
 inc(gPropsNbr);
 lLab:=aLab;
 with lLab^ do
 begin
  lLabelNr:=0;
  if nLabelIdNr > 0 then
   begin
    if gDefLabNbr >= length(gDefLabels) then
      setlength(gDefLabels,2*length(gDefLabels));
    inc(gDefLabNbr);
    lLabelNr:=gDefLabBase+gDefLabNbr;
    with gDefLabels[gDefLabNbr] do
     begin
      nLabId:=nLabelIdNr;
      nPropNr:=gPropsNbr;
      nLabNr:=0;
      nPending:=true;
      nDefiniensLab:=true;
     end;
    if gLabNbr >= length(gLabIdents) then
      setlength(gLabIdents,2*length(gLabIdents));
    inc(gLabNbr);
    gLabIdents[gLabNbr]:=gDefLabels[gDefLabNbr];
   end;
  aLab:=new(MSLabelPtr,Init(nLabelIdNr,nLabelPos,gPropsNbr,lLabelNr));
 end;
 dispose(lLab,Done);
end;

procedure FirstIdentArticleObj.Process_StraightforwardJustification(aInf: StraightforwardJustificationPtr);
 var lRefs: PList;
     k: integer;
begin
 Process_References(aInf^.nReferences);
 if aInf^.nLinked then
  begin
   lRefs:=new(PList,Init(aInf^.nReferences.Count+1));
   lRefs^.Insert(new(MSLocalReferencePtr, Init(0,aInf^.nLinkPos,
                              FirstIdBlockPtr(nBlockPtr)^.nPropsNbr,gLabCntr)));
   for k := 0 to aInf^.nReferences.Count - 1 do
    lRefs^.Insert(aInf^.nReferences.Items^[k]);
   aInf^.nReferences^.DeleteAll;
   dispose(aInf^.nReferences,Done);
   aInf^.nReferences:=lRefs;
  end;
end;

procedure FirstIdentArticleObj.Process_SchemeJustification(var aInf: SchemeJustificationPtr);
 var lInf: MSSchemeJustificationPtr;
begin
  with SchemeJustificationPtr(aInf)^ do
   begin
    lInf:=new(MSSchemeJustificationPtr,Init(nInfPos,nSchFileNr,nSchemeIdNr,0));
    lInf^.nReferences:=nReferences;
   end;
  dispose(aInf);
  aInf:=lInf;
  with MSSchemeJustificationPtr(aInf)^ do
  begin
    if nSchFileNr > 0 then
     begin
      if nSchemeIdNr = 0 then ErrImm(146);
     end
    else if nSchemeIdNr > 0 then
     begin
      nSchemeNr:=gSchemeIdent.IndexOf(nSchemeIdNr)+1+gCanceledSch;
      if nSchemeNr = 0 then ErrImm(191);
     end;
    Process_References(nReferences);
  end;
end;

procedure FirstIdentArticleObj.Process_SchemeHead(var aSch: SchemePtr);
 var lSch: MSSchemePtr;
begin
 gSchFuncBase:=gSchFuncNbr;
 gSchPredBase:=gSchPredNbr;
 with SchemePtr(aSch)^ do
  lSch:=new(MSSchemePtr,Init(nSchemeIdNr,0,nSchemePos,nSchemeParams,nSchemePremises,nSchemeConclusion));
 dispose(aSch);
 aSch:=lSch;
 with MSSchemePtr(aSch)^ do
  begin
   gSchemeIdent.InsertElem(nSchemeIdNr);
   nSchemeNr:=gSchemeIdent.Count+gCanceledSch;
  end;
 inherited Process_SchemeHead(aSch);
end;

procedure FirstIdentArticleObj.Process_SchemePremises(aSchema:SchemePtr);
begin inherited Process_SchemePremises(aSchema);
end;

procedure FirstIdentArticleObj.Process_StartBlock(aWSBlock:WSBlockPtr);
 var i: integer;
     lItems: PList;
     lItemPtr: wsItemPtr;
     lVar: VariablePtr;
begin
  FirstIdBlockPtr(nStackArr[nStackCnt])^.nLabNbr:=gLabNbr;
  FirstIdBlockPtr(nStackArr[nStackCnt])^.nLabCntr:=gLabCntr;
  FirstIdBlockPtr(nStackArr[nStackCnt])^.nPropsNbr:=gPropsNbr+1;

  FirstIdBlockPtr(nStackArr[nStackCnt])^.nLocFuncNbr:=gLocFuncNbr;
  FirstIdBlockPtr(nStackArr[nStackCnt])^.nLocPredNbr:=gLocPredNbr;
  gAvailableLocFuncNbr:=gLocFuncNbr;
  gAvailableLocPredNbr:=gLocPredNbr;
  FirstIdBlockPtr(nStackArr[nStackCnt])^.nVarNbr:=gVarNbr;

  FirstIdBlockPtr(nStackArr[nStackCnt])^.nConstVarNbr:=gConstVarNbr;
  FirstIdBlockPtr(nStackArr[nStackCnt])^.nPrivFuncBase:=gPrivFuncNbr;
  FirstIdBlockPtr(nStackArr[nStackCnt])^.nPrivPredBase:=gPrivPredNbr;

  with aWSBlock^ do
  case nBlockKind of
   blNotation,
   blRegistration,
   blDefinition:
    begin
     gDefLabNbr:=0;
     gDefLabBase:=gLabNbr;
     gDefVarBase:=gVarNbr;
     gSkelConstSet.fCount:=0;
    end;
   blProof:
    with nLastFreeVars do
    if Count > 0 then
    begin
     lItems:=New(PList,Init(0));
     for i:=0 to Count-1 do
      with gReservedVar[Items^[i].X] do
       begin
        lItemPtr:=new(wsItemPtr,Init(itGeneralization,nBlockPos));
//        lVar:=new(MSVariablePtr,Init(nBlockPos,nResVarId,ReservedVar,Constant,Items^[i].X,gConstVarNbr+i+1));
        lVar:=new(VariablePtr,Init(nBlockPos,nResVarId));
        lItemPtr^.nContent:= new(ImplicitlyQualifiedSegmentPtr,Init(nBlockPos,lVar));
//        ,new(MSReservedDscrTypePtr,Init(nBlockPos,lVar^.nIdent,nResTypeNr))));
        lItems^.Insert(lItemPtr);
       end;
      for i:=0 to nItems^.Count-1 do
       lItems^.Insert(nItems^.Items^[i]);
      nItems^.DeleteAll;
      dispose(nItems,Done);
      nItems:=lItems;
      nLastFreeVars.Done;
    end;
  end;
end;

procedure FirstIdentArticleObj.Process_FinishBlock(aWSBlock:WSBlockPtr);
 var i: integer;
begin
  gLabNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nLabNbr;
  gLabCntr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nLabCntr;
  gPropsNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nPropsNbr;
  if nStackCnt > 0 then
   FirstIdItemPtr(nStackArr[nStackCnt-1])^.nItemWithBlock:=true;
  case aWSBlock^.nBlockKind of
   blDefinition:
    begin
     if gLabNbr+gDefLabNbr >= length(gLabIdents) then
      setlength(gLabIdents,2*length(gLabIdents)+gDefLabNbr);
     for i:=1 to gDefLabNbr do
      begin
       inc(gLabNbr);
       inc(gLabCntr);
       gLabIdents[gLabNbr]:=gDefLabels[i];
       gLabIdents[gLabNbr].nLabNr:=gLabCntr;
       gLabIdents[gLabNbr].nPending:=false;
      end;
     gDefLabNbr:=0;
    end;
   blHereby,blDiffuse,blCase,blSuppose: ;
   blProof: ;
   blMain: ;
  end;
  gVarNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nVarNbr;
  gConstVarNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nConstVarNbr;
  gLocFuncNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nLocFuncNbr;
  gLocPredNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nLocPredNbr;
  gAvailableLocFuncNbr:=gLocFuncNbr;
  gAvailableLocPredNbr:=gLocPredNbr;
  gPrivFuncNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nPrivFuncBase;
  gPrivPredNbr:=FirstIdBlockPtr(nStackArr[nStackCnt])^.nPrivPredBase;
end;

procedure FirstIdentArticleObj.Process_Pragma(aPragma: PragmaPtr);
 var lNbr: integer;
     lKind: char;
begin
 with aPragma^ do
  begin
   CanceledPragma(nPragmaStr,lKind,lNbr);
   if lKind = 'S' then inc(gCanceledSch,lNbr);
  end;
end;

procedure FirstIdentArticleObj.Process_ImplicitlyRedefinedVariable(aRes:TypeChangePtr);
begin
 with aRes^,nVar^ do
 begin
  nTermExpr:=AnalyzeSimpleTerm(nIdent,nVarPos);
  nTypeChangeKind:=Equating;
  Process_DefConstant(nVar);
 end;
end;

procedure FirstIdentArticleObj.Process_RedefinedVariable(aRes:TypeChangePtr);
begin
 with aRes^ do
 begin
  Process_DefConstant(nVar);
  Process_TermWithFVScope(nTermExpr);
 end;
end;

procedure FirstIdentArticleObj.Process_ExemplifyingVariable(aExampl:ExamplePtr);
 var lTrm: MSSimpleTermPtr;
begin
 with aExampl^,nVarId^ do
 begin
  if nTermExpr = nil then
   begin
    lTrm:=AnalyzeSimpleTerm(nIdent,nVarPos);
   end;
  Process_DefConstant(nVarId);
  if nTermExpr <> nil then
    Process_TermWithFVScope(nTermExpr)
   else nTermExpr:=lTrm;
 end;
end;

procedure FirstIdentArticleObj.Process_ImplicitExamplification(aExampl:ExamplePtr);
begin
 Process_TermWithFVScope(aExampl^.nTermExpr);
end;

procedure FirstIdentArticleObj.Process_DefConstant(var aVar: VariablePtr);
 var lVar: MSVariablePtr;
begin
 with aVar^ do
  begin
    AppVar(nIdent,DefConstant,Constant,nVarPos);
    lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,DefConstant,Constant,gVariable[gVarNbr].nSerialNr,gConstVarNbr));
    dispose(aVar,Done);
    aVar:=lVar;
  end;
end;

procedure FirstIdentArticleObj.Process_FieldInStructureDefinition(aField:FieldSymbolPtr);
 var lFormatNr: integer;
begin 
 with aField^ do
  begin
    lFormatNr:=gFormatsColl.CollectPrefixForm('U',nFieldSymbol,1);
    Inc(gFieldNbr);
    if gFieldNbr >= length(gField) then
     setlength(gField,2*length(gField));
    gField[gFieldNbr]:=lFormatNr;
  end;
end;

procedure FirstIdentArticleObj.Process_FieldsInStructureDefinition(aStruct:StructureDefinitionPtr);
begin
 gFieldNbr:=0;
 inherited Process_FieldsInStructureDefinition(aStruct);
end;

procedure FirstIdentArticleObj.Process_ConstantDefinition(aDef:ConstantDefinitionPtr);
begin
 with aDef^ do
  begin
    Process_DefConstant(nVarId);
    Process_TermWithFVScope(nTermExpr);
  end;
end;

procedure FirstIdentArticleObj.Process_StartItem(aWSItem:WSItemPtr);
 var i: integer;
begin
  FirstIdItemPtr(nStackArr[nStackCnt])^.nItemWithBlock:=false;
  FirstIdItemPtr(nStackArr[nStackCnt])^.nLastLabelNbr:=gLabNbr;
  case aWSItem^.nItemKind of
    itCorrCond:
     with FirstIdBlockPtr(gBlockPtr)^ do
      begin
(*
       if (CorrectnessConditionPtr(aWSItem^.nContent)^.nCorrCondSort = syCoherence) and
        ((gDefiningWay = dfMeans) or gRedefinitions) then
         for i:=nLabNbr+1 to nLabNbr do nPendingLabs.InsertElem(i);
       if (gDefKind = itCluster) { existence or coherence } then
         for i:=nLabNbr+1 to nLabNbr do nPendingLabs.InsertElem(i);
*)
      end;
    itCorrectness:
     with FirstIdBlockPtr(gBlockPtr)^ do
      begin
(*
       if (gDefKind = itCluster) { existence or coherence } then
         for i:=nLabNbr+1 to nLabNbr do nPendingLabs.InsertElem(i);
*)
      end;
(*
   with MSBlockPtr(gBlockPtr)^ do
    begin
     if (syCoherence in gCorrectnessConditions) and
        ((gDefiningWay = dfMeans) or gRedefinitions) then
      for i:= nLabNbr+1 to gLabNbr do nPendingLabs.InsertElem(i);
     case gDefKind of
      itCluster:
       if gCorrectnessConditions <= [syCoherence,syEsxistence] then
        begin
         if syExistence in gCorrectnessConditions then
          for i:= nLabNbr+1 to gLabNbr` do nPendingLabs.InsertElem(i);
        end;
     end;
    end;

*)
    itRegularStatement,itConclusion,itTheorem:
      FirstIdItemPtr(nStackArr[nStackCnt])^.nLastPropsNbr:=gPropsNbr+1;
//    itDefPred, itDefFunc, itDefMode, itDefAttr,
//    itDefStruct,
//    itPredSynonym, itPredAntonym, itFuncNotation, itModeNotation,
//    itAttrSynonym, itAttrAntonym,
//    itCluster,
//    itIdentify, itReduction, itPropertyRegistration:
//       ;
  end;
end;

procedure FirstIdentArticleObj.Process_FinishItem(aWSItem:WSItemPtr);
 var i: integer;
begin
  case aWSItem^.nItemKind of
    itRegularStatement, itConclusion, itTheorem:
     if not FirstIdItemPtr(nStackArr[nStackCnt])^.nItemWithBlock then
      FirstIdBlockPtr(nBlockPtr)^.nPropsNbr:=gPropsNbr
     else FirstIdBlockPtr(nBlockPtr)^.nPropsNbr:=FirstIdItemPtr(nStackArr[nStackCnt])^.nLastPropsNbr;
    itAssumption, itCaseHead, itSupposeHead:
      FirstIdBlockPtr(nBlockPtr)^.nPropsNbr:=gPropsNbr;
    else
      FirstIdBlockPtr(nBlockPtr)^.nPropsNbr:=gPropsNbr+1;
  end;
  for i:=FirstIdItemPtr(nStackArr[nStackCnt])^.nLastLabelNbr+1 to gLabNbr do
   if not gLabIdents[i].nDefiniensLab then
    gLabIdents[i].nPending:=false;
end;

{----------------------------------------------------------------}

procedure FirstIdentArticleObj.Process_Variable( var aVar: VariablePtr);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
 begin
  AppVar(nIdent,Bound,Bound,nVarPos);
  lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,Bound,Bound,gVariable[gVarNbr].nSerialNr,gBoundVarNbr));
  dispose(aVar,Done);
  aVar:=lVar;
 end;
end;

procedure MatchingImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr; aKind:VariableKind);
 var lVarNr,rVarNr,i,j: integer;
     lSegm: MSImplicitlyQualifiedSegmentPtr;
     lResDesType: MSReservedDscrTypePtr;
begin
 with aSegm^ do
 begin
   lVarNr:=LookUp_ReservedVar(nIdentifier^.nIdent);
   if lVarNr = 0 then
    begin
      Error(nSegmPos,140);
    end;
   MSVariablePtr(nIdentifier)^.nSerialNr:=lVarNr;
   gVariable[gVarNbr].nSerialNr:=lVarNr;
   lResDesType:=new(MSReservedDscrTypePtr,Init(nSegmPos,nIdentifier^.nIdent,gReservedVar[lVarNr].nResTypeNr));
   AssignFreeVars(nSegmPos,lVarNr,lResDesType^.nResSubst);
   case aKind of
    Constant,DefConstant:
     with lResDesType^.nResSubst do
      for j:=0 to fCount - 1 do
       if fList[j].Y1 = ord(ReservedVar) then
        begin
         fList[j].Y1:=ord(aKind);
         fList[j].Y2:=0;
         Error(nSegmPos,143);
        end;
   end;
   lSegm:=new(MSImplicitlyQualifiedSegmentPtr,Init(nSegmPos,nIdentifier,lResDesType));
   dispose(aSegm);
   aSegm:=lSegm;
 end;
end;

procedure FirstIdentArticleObj.Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Process_Variable( aSegm^.nIdentifier);
 MSVariablePtr(aSegm^.nIdentifier)^.nOrigin:=ReservedVar;
 gVariable[gVarNbr].nOrigin:=ReservedVar;
 MatchingImplicitlyQualifiedVariable( aSegm, Bound);
end;

procedure FirstIdentArticleObj.Process_FixedVariable( var aVar: VariablePtr);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
 begin
  AppVar(nIdent,Constant,Constant,nVarPos);
  lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,Constant,Constant,gVariable[gVarNbr].nSerialNr,gConstVarNbr));
  dispose(aVar,Done);
  aVar:=lVar;
 end;
 gSkelConstSet.Insert(gVarNbr);
end;

procedure FirstIdentArticleObj.Process_ImplicitlyQualifiedFixedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
 var lVarNr: integer;
     lSegm: MSImplicitlyQualifiedSegmentPtr;
     lVar: MSVariablePtr;
begin
 Process_FixedVariable( aSegm^.nIdentifier);
 MSVariablePtr(aSegm^.nIdentifier)^.nOrigin:=ReservedVar;
 gVariable[gVarNbr].nOrigin:=ReservedVar;
 MatchingImplicitlyQualifiedVariable( aSegm, Constant);
end;

procedure FirstIdentArticleObj.Process_ChoiceVariable( var aVar: VariablePtr);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
 begin
  AppVar(nIdent,Constant,Constant,nVarPos);
  lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,Constant,Constant,gVariable[gVarNbr].nSerialNr,gConstVarNbr));
  dispose(aVar,Done);
  aVar:=lVar;
 end;
end;

procedure FirstIdentArticleObj.Process_ImplicitlyQualifiedChoiceVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
 var lVarNr: integer;
     lSegm: MSImplicitlyQualifiedSegmentPtr;
     lVar: MSVariablePtr;
begin
 Process_ChoiceVariable( aSegm^.nIdentifier);
 MSVariablePtr(aSegm^.nIdentifier)^.nOrigin:=ReservedVar;
 gVariable[gVarNbr].nOrigin:=ReservedVar;
 MatchingImplicitlyQualifiedVariable( aSegm, Constant);
end;

procedure FirstIdentArticleObj.Process_StartVariableSegment;
begin
 gVarSegmentBase:=gVarNbr;
end;

procedure FirstIdentArticleObj.Process_FinishVariableSegment;
 var k:integer;
begin
 for k:=gVarSegmentBase+1 to gVarNbr do
  gVariable[k].nPending:=false;
 gVarSegmentBase:=gVarNbr;
end;

procedure FirstIdentArticleObj.Process_StartReconsideringVariableSegment;
begin
 gRecVarSegmentBase:=gVarNbr;
end;

procedure FirstIdentArticleObj.Process_FinishReconsideringVariableSegment;
 var k:integer;
begin
 for k:=gRecVarSegmentBase+1 to gVarNbr do
  gVariable[k].nPending:=false;
 gRecVarSegmentBase:=gVarNbr;
end;

procedure FirstIdentArticleObj.Process_FinishOneVariableSegment;
begin
 gVariable[gVarNbr].nPending:=false;
end;

procedure FirstIdentArticleObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
 var lVarNr: integer;
     lTrmPtr: MSSimpleTermPtr;
     lVarKind: VariableKind;
begin
 with aTrm^ do
 begin
  lTrmPtr:=AnalyzeSimpleTerm ( nIdent, nTermPos);
  dispose(aTrm,Done);
  aTrm:=lTrmPtr;
 end;
end;

procedure FirstIdentArticleObj.Process_PrivateFunctorTerm ( var aTrm: PrivateFunctorTermPtr );
 var lFuncNr: integer;
     lTrm: MSPrivateFunctorTermPtr;
begin
 with PrivateFunctorTermPtr(aTrm)^ do
  begin
   lFuncNr:=LookUp_FuncVar(nFunctorIdent,nArgs^.Count);
   if lFuncNr = 0 then
    begin
      Error(nTermPos,148);
    end;
   Process_TermList(nArgs);
   with gFuncVars[lFuncNr] do
    lTrm:=new(MSPrivateFunctorTermPtr,Init(nTermPos,nFunctorIdent,nArgs,
                                           nFuncKind,nSerialNr,nFuncNr));
    dispose(aTrm); // only aTrm^ object may be deleted. Do not dispose argumets!
    aTrm:=lTrm;
  end;
end;

procedure FirstIdentArticleObj.Process_InternalSelectorTerm( var aTrm: InternalSelectorTermPtr );
 var lVarNr,lFormatNr: integer;
     lTrm: MSInternalSelectorTermPtr;
 label Found;
begin
  with aTrm^ do
   begin
    lFormatNr:=gFormatsColl.LookUp_PrefixFormat('U',nSelectorSymbol,1);
    for lVarNr:=1 to gFieldNbr do
     if gField[lVarNr] = lFormatNr then goto Found;
    lVarNr:=0;
    Error(nTermPos,154);
Found:
    lTrm:=new(MSInternalSelectorTermPtr,Init(nTermPos,nSelectorSymbol,gConstVarNbr+lVarNr));
    dispose(aTrm); // only aTrm^ object may be deleted. Do not dispose argumets!
    aTrm:=lTrm;
   end;
end;

procedure FirstIdentArticleObj.Process_StartFraenkelTerm;
begin
  FirstIdSubexprPtr(nStackArr[nStackCnt])^.nVarNbr:=gVarNbr;
end;

procedure FirstIdentArticleObj.Process_FinishFraenkelTerm;
begin
  gVarNbr:=FirstIdSubexprPtr(nStackArr[nStackCnt])^.nVarNbr;
end;

procedure FirstIdentArticleObj.Process_FraenkelTermsScope ( var aFrm:FormulaPtr );
begin
  Process_Formula(aFrm);
end;

procedure FirstIdentArticleObj.Process_StartQuantifiedFormula(aFrm:QuantifiedFormulaPtr);
begin
  FirstIdSubexprPtr(nStackArr[nStackCnt])^.nVarNbr:=gVarNbr;
end;

procedure FirstIdentArticleObj.Process_FinishQuantifiedFormula(aFrm:QuantifiedFormulaPtr);
begin
  gVarNbr:=FirstIdSubexprPtr(nStackArr[nStackCnt])^.nVarNbr;
end;

procedure FirstIdentArticleObj.Process_PrivatePredicateFormula ( var aFrm:PrivatePredicativeFormulaPtr );
 var lPredNr: integer;
     lFrm: PrivatePredicativeFormulaPtr;
begin
  with PrivatePredicativeFormulaPtr(aFrm)^ do
   begin
    lPredNr:=LookUp_PredVar(nPredIdNr,nArgs^.Count);
    if lPredNr = 0 then
     begin
      Error(nFormulaPos,149);
     end;
    Process_TermList( nArgs);
    with gPredVars[lPredNr] do
     lFrm:=new(MSPrivatePredicativeFormulaPtr,Init(nFormulaPos,nPredIdNr,nArgs,
                                                  nPredKind,nSerialNr,nPredNr));
    dispose(aFrm); // only aFrm^ object may be deleted. Do not dispose argumets!
    aFrm:=lFrm;
   end;
end;

{----------------------------------------------------------------}

procedure FirstIdentArticleObj.Process_StartReservedVariables(aResVars: ReservationSegmentPtr);
begin
 gResVarBase:=gResVarNbr;
 gVarSegmentBase:=gVarNbr;
 nProcessingReservationSegment:=true;
end;

procedure FirstIdentArticleObj.Process_Locus( var aLocus: LocusPtr);
 var lVarNr,k: integer;
     lLocusPtr: MSLocusPtr;
     lVarKind: VariableKind;
begin
 with aLocus ^ do
 begin
  lVarNr:=LookUp_Var(nVarId);
  if lVarNr = 0 then
   Error(nVarIdPos,140);
  with gVariable[lVarNr] do
   lLocusPtr:=new(MSLocusPtr,Init(nVarIdPos,nVarId,
                     nOrigin,nVarKind,
                     nSerialNr,nVarNr));
  dispose(aLocus,Done);
  aLocus:=lLocusPtr;
  for k:=gVarNbr downto gDefVarBase+1 do
   with gVariable[k] do
   if nVarIdent = lLocusPtr^.nVarId then
    begin
     if gSkelConstSet.IsInSet(k) then
      begin
       if Involved.IsInSet(nVarNr) then
        begin
         Error(lLocusPtr^.nVarIdPos,141);
         lLocusPtr^.nVarNr:=0;
        end;
       Involved.Insert(nVarNr);
      end
     else
      begin
       Error(lLocusPtr^.nVarIdPos,179);
       lLocusPtr^.nVarNr:=0;
      end;
     exit;
    end;
  Error(lLocusPtr^.nVarIdPos,142);
  lLocusPtr^.nVarNr:=0;
 end;
end;

procedure FirstIdentArticleObj.Process_Pattern(aPattern: PatternPtr);
begin
// Involved.Clear;
 Involved.fCount:=0;
 case aPattern^.nPatternSort of
 itDefPred:
  with PredicatePatternPtr(aPattern)^ do
  begin
   Process_Loci(nLeftArgs);
//   nPredSymbol
   Process_Loci(nRightArgs);
  end;
 itDefFunc:
  begin
    case FunctorPatternPtr(aPattern)^.nFunctKind of
     InfixFunctor:
     with InfixFunctorPatternPtr(aPattern)^ do
      begin
       Process_Loci(nLeftArgs);
//       nOperSymb
       Process_Loci(nRightArgs);
      end;
     CircumfixFunctor:
     with CircumfixFunctorPatternPtr(aPattern)^ do
      begin
//       nLeftBracketSymb
       Process_Loci(nArgs);
//       nRightBracketSymb
      end;
    end;
  end;
 itDefMode:
  with ModePatternPtr(aPattern)^ do
  begin
//   nModeSymbol
    Process_Loci(nArgs);
  end;
 itDefStruct:
  with ModePatternPtr(aPattern)^ do
  begin
//   nModeSymbol
    Process_Loci(nArgs);
  end;
 itDefAttr:
  with AttributePatternPtr(aPattern)^ do
  begin
   Process_Locus(nArg);
//   nAttrSymbol;
   Process_Loci(nArgs);
   if MSLocusPtr(nArg)^.nVarNr <> gVarNbr then
     begin
      Error(MSLocusPtr(nArg)^.nVarIdPos,374);
     end;
  end;
 end;
end;

procedure FirstIdentArticleObj.Process_LociEqualities(aEqLociList:PList);
begin
 Involved.fCount:=0;
 inherited Process_LociEqualities(aEqLociList);
end;

procedure FirstIdentArticleObj.Process_ReservedVariable( var aVar: VariablePtr);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
 begin
  inc(gResVarNbr);
  if gResVarNbr >= length(gReservedVar) then
   setlength(gReservedVar,2*length(gReservedVar));
  with gReservedVar[gResVarNbr] do
  begin
   nResVarId:=aVar^.nIdent;
   nResVarPos:=aVar^.nVarPos;
   nResVarNr:=gResVarNbr;
   nPending:=true;
   nResTypeNr:=gResTypeNbr+1;
   nFreeVars.Init(0,4);
  end;
  lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,ReservedVar,ReservedVar,gResVarNbr,0));
  dispose(aVar,Done);
  aVar:=lVar;
 end;
end;

procedure FirstIdentArticleObj.Process_FinishReservedVariables(var aResVars: ReservationSegmentPtr);
 var i,k,lVarNr,rVarNr,lVarNbr:integer;
     lFV: FreeVarsInExprPtr;
     lF2RV: Free2ResVarsInExprPtr;
     lSBV: SetVarInExpressionPtr;
     lVars: NatSet;
     lRes: MSReservationSegmentPtr;
     lResType: MSReservedDscrTypePtr;
     lFreeVars: PList;
begin
 inc(gResTypeNbr);
 if gResTypeNbr >= length(gReservedType) then
  setlength(gReservedType,2*length(gReservedType));
 gReservedType[gResTypeNbr].nTyp:=aResVars^.nResType;
 lFV:=new(FreeVarsInExprPtr, Init(exType));
 lFV^.Process_Type(aResVars^.nResType);
 if lFV^.nFreeVars.Count > 0 then
 begin
  lFV^.Complete_FreeVarsInExpr;
 end;
 lF2RV:=new(Free2ResVarsInExprPtr, Init(exFormula,0));
 for k:=0 to lFV^.nFreeVars.Count - 1 do
  with gReservedVar[lFV^.nFreeVars.Items^[k].X] do
   begin
    lF2RV^.nFreeVar:=lFV^.nFreeVars.Items^[k].X;
    lF2RV^.Process_Type(aResVars^.nResType);
   end;
 dispose(lF2RV,Done);
 lVarNbr:=gVarNbr;
// gBoundVarNbr:=0;
 lFreeVars:=new(PList,Init(0));
 for k:=0 to lFV^.nFreeVars.Count - 1 do
  with gReservedVar[lFV^.nFreeVars.Items^[k].X] do
   if nResVarId > 0 then
   begin
    AppVar(nResVarId,ReservedVar,Bound,nResVarPos);
    rVarNr:=LookUp_ReservedVar(nResVarId);
    if rVarNr = 0 then
     begin
      Error(nResVarPos,140);     //???
     end;
    gVariable[gVarNbr].nVarNr:=gBoundVarNbr;
    lResType:=new(MSReservedDscrTypePtr,Init(nResVarPos,nResVarId,nResTypeNr));
    with gReservedType[nResTypeNr] do
     for i:=0 to nFreeArgs.Count - 1 do
      with gReservedVar[nFreeArgs.Items^[i].X] do
      begin
       lVarNr:=LookUp_Var(nResVarId);
       if not (gVariable[lVarNr].nOrigin in [FreeVar,ReservedVar]) then
         Error(nResVarPos,163);
       lResType^.nResSubst.Assign(nFreeArgs.Items^[i].X,ord(gVariable[lVarNr].nVarKind),gVariable[lVarNr].nVarNr);
      end;
    lFreeVars^.Insert(lResType);
   end;
 lSBV:=new(SetVarInExpressionPtr, Init(exType));
 lSBV^.Process_Type(aResVars^.nResType);
 dispose(lSBV,Done);
 gVarNbr:=lVarNbr;
 gBoundVarNbr:=0;
 lVars.CopyNatSet(lFV^.nFreeVars);
 while lVars.Count > 0 do
  with gReservedVar[lVars.Items^[lVars.Count-1].X] do
  begin
    lVars.EnlargeBy(gReservedType[nResTypeNr].nFreeArgs);
    lFV^.nFreeVars.EnlargeBy(gReservedType[nResTypeNr].nFreeArgs);
    lVars.AtDelete(lVars.Count-1);
  end;
 lRes:=new(MSReservationSegmentPtr,Init(aResVars^.nIdentifiers,aResVars^.nResType,lFV^.nFreeVars));
 lRes^.nFreeVars:=lFreeVars;
 dispose(aResVars);
 aResVars:=lRes;
 gReservedType[gResTypeNbr].nFreeArgs.MoveNatSet(lFV^.nFreeVars);
 dispose(lFV,Done);
 for k:=gResVarBase+1 to gResVarNbr do
  begin
   gReservedVar[k].nPending:=false;
  end;
 gResVarBase:=gResVarNbr;
 nProcessingReservationSegment:=false;
end;

procedure FirstIdentArticleObj.Process_StartSchemeSegment(aSgm:SchemeSegmentPtr);
begin
  gAvailableLocPredNbr:=gLocPredNbr;
  gAvailableLocFuncNbr:=gLocFuncNbr;
end;

procedure FirstIdentArticleObj.Process_FinishSchemeSegment(aSgm:SchemeSegmentPtr);
begin
  gAvailableLocPredNbr:=gLocPredNbr;
  gAvailableLocFuncNbr:=gLocFuncNbr;
end;

procedure FirstIdentArticleObj.Process_SchemePredicateVariable( var aVar: VariablePtr; aArgNbr: integer);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
  begin
   if aArgNbr > MaxArgNbr then
    begin Error(nVarPos,180);
    end;
   inc(gSchPredNbr);
   lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,SchematicPred,SchematicPred,gSchPredNbr,gSchPredNbr-gSchPredBase));
   AppPredVar(nIdent,nVarPos,SchematicPred,gSchPredNbr,gSchPredNbr-gSchPredBase,aArgNbr);
   dispose(aVar,Done);
   aVar:=lVar;
  end;
end;

procedure FirstIdentArticleObj.Process_SchemeFunctorVariable( var aVar: VariablePtr; aArgNbr: integer);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
  begin
   if aArgNbr > MaxArgNbr then
    begin Error(nVarPos,180);
    end;
   inc(gSchFuncNbr);
   lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,SchematicFunc,SchematicFunc,gSchFuncNbr,gSchFuncNbr-gSchFuncBase));
   AppFuncVar(nIdent,nVarPos,SchematicFunc,gSchFuncNbr,gSchFuncNbr-gSchFuncBase,aArgNbr);
   dispose(aVar,Done);
   aVar:=lVar;
  end;
end;

procedure FirstIdentArticleObj.Process_SchemeConclusion( var aFrm:FormulaPtr );
begin
  Process_FormulaWithFVScope(aFrm);
  nLastFreeVars.Done;
end;

procedure FirstIdentArticleObj.Process_StartPrivateFunctor;
begin
  gAvailableLocFuncNbr:=gLocFuncNbr;
end;

procedure FirstIdentArticleObj.Process_FinishPrivateFunctor;
begin
  gAvailableLocFuncNbr:=gLocFuncNbr;
  gArityOfLocal:=0;
end;

procedure FirstIdentArticleObj.Process_LocalFunctorVariable( var aVar: VariablePtr; aArgNbr: integer);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
  begin
   gArityOfLocal:=aArgNbr;
   if aArgNbr > MaxArgNbr then
    begin Error(nVarPos,180);
    end;
   inc(gSerialPrivFuncNbr);
   inc(gPrivFuncNbr);
   lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,PrivateFunc,PrivateFunc,gSerialPrivFuncNbr,gPrivFuncNbr));
   AppFuncVar(nIdent,nVarPos,PrivateFunc,gSerialPrivFuncNbr,gPrivFuncNbr,aArgNbr);
   dispose(aVar,Done);
   aVar:=lVar;
  end;
end;

procedure FirstIdentArticleObj.Process_StartPrivatePredicate;
begin
  gAvailableLocPredNbr:=gLocPredNbr;
end;

procedure FirstIdentArticleObj.Process_FinishPrivatePredicate;
begin
  gAvailableLocPredNbr:=gLocPredNbr;
  gArityOfLocal:=0;
  nLastFreeVars.Done;
end;

procedure FirstIdentArticleObj.Process_LocalPredicateVariable( var aVar: VariablePtr; aArgNbr: integer);
 var lVar: MSVariablePtr;
begin
 with aVar ^ do
  begin
   gArityOfLocal:=aArgNbr;
   if aArgNbr > MaxArgNbr then
    begin Error(nVarPos,180);
    end;
   inc(gSerialPrivPredNbr);
   inc(gPrivPredNbr);
   lVar:=new(MSVariablePtr,Init(nVarPos,nIdent,PrivatePred,PrivatePred,gSerialPrivPredNbr,gPrivPredNbr));
   AppPredVar(nIdent,nVarPos,PrivatePred,gSerialPrivPredNbr,gPrivPredNbr,aArgNbr);
   dispose(aVar,Done);
   aVar:=lVar;
  end;
end;

procedure FirstIdentArticleObj.Process_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr);
begin
  with aCStm^ do
  begin
   Process_Label(nProp^.nLab);
   Process_FormulaWithFVScope(nProp^.nSentence);
   if nJustification^.nInfSort <> infProof then
    nLastFreeVars.Done;
   Process_Justification(nJustification,aBlock);
  end;
end;

procedure FirstIdentArticleObj.Process_Proposition(aProp:PropositionPtr);
begin
 Process_Label(aProp^.nLab);
 Process_FormulaWithFVScope(aProp^.nSentence);
 nLastFreeVars.Done;
end;

procedure FirstIdentArticleObj.Process_DefiniensFormula(var aFrm:FormulaPtr );
begin
  Process_FormulaWithFVScope(aFrm);
  nLastFreeVars.Done;
end;

procedure FirstIdentArticleObj.Process_Term ( var aTrm: TermPtr );
begin
 case aTrm^.nTermSort of
  wsPlaceholderTerm:
   with PlaceholderTermPtr(aTrm)^ do
   begin
    if nLocusNr <= gArityOfLocal then
    else
     begin
      dispose(aTrm,Done);
      aTrm:=new(IncorrectTermPtr, Init(nTermPos));
      ErrImm(181);
     end;
   end;
 end;
 inherited Process_Term(aTrm);
end;

procedure FirstIdentArticleObj.Process_TypeWithFVScope ( var aTyp: TypePtr );
 var lFV: FreeVarsInExprPtr;
     lF2RV: Free2ResVarsInExprPtr;
     lAFV: AdjustFreeVarScopePtr;
     lAddFV: AddFreeVarInScopePtr;
     lSBV: SetVarInExpressionPtr;
     lNIQErr: NoImplicitQualificationErrorPtr;
     i,lVarNr: integer;
     lVar: MSVariablePtr;
     lPos: Position;
begin
  Process_Type(aTyp);
  lPos:=aTyp^.nTypePos;
  lFV:=new(FreeVarsInExprPtr, Init(exType));
  lFV^.Process_Type(aTyp);
  if lFV^.nFreeVars.Count > 0 then
   begin
    lFV^.Complete_FreeVarsInExpr;
    lF2RV:=new(Free2ResVarsInExprPtr, Init(exType,0));
    lAFV:=new(AdjustFreeVarScopePtr, Init(exType));
    lNIQErr:=New(NoImplicitQualificationErrorPtr,Init(exType,0));
    with lFV^.nFreeVars do
    begin
      for i:=Count-1 downto 0 do
      with gReservedVar[Items^[i].X] do
       begin
        lAFV^.Initialize(Items^[i].X);
        lAFV^.Process_Type(aTyp);
        if length(lAFV^.nAdjScopeIndex) = 0 then
         begin
          if not nProcessingReservationSegment then
           begin
            lNIQErr^.nFreeVar:=Items^[i].X;
            lNIQErr^.Process_Type(aTyp);
            if Items^[i].X <> 0 then
             Error(lPos,143);
           end;
         end
        else
         begin
          lAddFV:=new(AddFreeVarInScopePtr,
                       Init(exType,Items^[i].X,lAFV^.nAdjScopeIndex));
          lAddFV^.Process_Type(aTyp);
          dispose(lAddFV,Done);
         end;
       end;
    end;
    dispose(lF2RV,Done);
    dispose(lNIQErr,Done);
   end;
  dispose(lFV,Done);
//!!
  nLastFreeVars.Done;
  lSBV:=new(SetVarInExpressionPtr, Init(exType));
  lSBV^.Process_Type(aTyp);
  dispose(lSBV,Done);
end;

procedure FirstIdentArticleObj.Process_TermWithFVScope ( var aTrm: TermPtr );
 var lFV: FreeVarsInExprPtr;
     lF2RV: Free2ResVarsInExprPtr;
     lAFV: AdjustFreeVarScopePtr;
     lAddFV: AddFreeVarInScopePtr;
     lSBV: SetVarInExpressionPtr;
     lNIQErr: NoImplicitQualificationErrorPtr;
     i,lVarNr: integer;
     lVar: MSVariablePtr;
     lPos: Position;
begin
  Process_Term(aTrm);
  lPos:=aTrm^.nTermPos;
  lFV:=new(FreeVarsInExprPtr, Init(exTerm));
  lFV^.Process_Term(aTrm);
  if lFV^.nFreeVars.Count > 0 then
   begin
    lFV^.Complete_FreeVarsInExpr;
    lF2RV:=new(Free2ResVarsInExprPtr, Init(exTerm,0));
    lAFV:=new(AdjustFreeVarScopePtr, Init(exTerm));
    lNIQErr:=New(NoImplicitQualificationErrorPtr,Init(exType,0));
    with lFV^.nFreeVars do
    begin
      for i:=Count-1 downto 0 do
      with gReservedVar[Items^[i].X] do
       begin
        lAFV^.Initialize(Items^[i].X);
        lAFV^.Process_Term(aTrm);
        if length(lAFV^.nAdjScopeIndex) = 0 then
         begin
          lNIQErr^.nFreeVar:=Items^[i].X;
          lNIQErr^.Process_Term(aTrm);
          if Items^[i].X <> 0 then
           Error(lPos,143);
         end
        else
         begin
          lAddFV:=new(AddFreeVarInScopePtr,
                       Init(exTerm,Items^[i].X,lAFV^.nAdjScopeIndex));
          lAddFV^.Process_Term(aTrm);
          dispose(lAddFV,Done);
         end;
       end;
    end;
    dispose(lF2RV,Done);
    dispose(lNIQErr,Done);
   end;
  dispose(lFV,Done);
//!!
  nLastFreeVars.Done;
  lSBV:=new(SetVarInExpressionPtr, Init(exTerm));
  lSBV^.Process_Term(aTrm);
  dispose(lSBV,Done);
end;

procedure FirstIdentArticleObj.Process_FormulaWithFVScope ( var aFrm:FormulaPtr );
 var lFV: FreeVarsInExprPtr;
     lF2RV: Free2ResVarsInExprPtr;
     lAFV: AdjustFreeVarScopePtr;
     lAddFV: AddFreeVarInScopePtr;
     lSBV: SetVarInExpressionPtr;
     i: integer;
     lVar: MSVariablePtr;
     lPos: Position;
     lSegm: MSImplicitlyQualifiedSegmentPtr;
     lFrm: FormulaPtr;
 label 1;
begin
  Process_Formula(aFrm);
  lPos:=aFrm^.nFormulaPos;
  lFV:=new(FreeVarsInExprPtr, Init(exFormula));
  lFV^.Process_Formula(aFrm);
//
  if lFV^.nFreeVars.Count > 0 then
   begin
    lFV^.Complete_FreeVarsInExpr;
    lF2RV:=new(Free2ResVarsInExprPtr, Init(exFormula,0));
    lAFV:=new(AdjustFreeVarScopePtr, Init(exFormula));
    with lFV^.nFreeVars do
    begin
     i:=Count-1;
     while (i >= 0) and (Count > 0) do
     begin
      lAFV^.Initialize(Items^[i].X);
      lAFV^.Process_Formula(aFrm);
      if length(lAFV^.nAdjScopeIndex) = 0 then
       begin
        lFrm:=aFrm;
        while lFrm^.nFormulaSort in
                 [wsUniversalFormula,wsExistentialFormula,wsNegatedFormula] do
         begin
          if  lFrm^.nFormulaSort = wsNegatedFormula
            then lFrm:=NegativeFormulaPtr(lFrm)^.nArg
           else with QuantifiedFormulaPtr(lFrm)^ do
            begin
              if (nSegment^.nSegmentSort = ikImplQualifiedSegm) and
                 (MSVariablePtr(ImplicitlyQualifiedSegmentPtr(nSegment)^.nIdentifier)^.nSerialNr = Items^[i].X) then
               begin
                AtDelete(i);
                goto 1;
               end;
              lFrm:=QuantifiedFormulaPtr(lFrm)^.nScope;
            end;
         end;
        with gReservedVar[Items^[i].X] do
        begin
         lVar:=new(MSVariablePtr,Init(lPos,nResVarId,ReservedVar,Bound,Items^[i].X,0));
         lSegm:=new(MSImplicitlyQualifiedSegmentPtr,Init(lPos,lVar,
                                 new(MSReservedDscrTypePtr,Init(lPos,lVar^.nIdent,nResTypeNr))));
        end;
        AssignFreeVars(lSegm^.nSegmPos,Items^[i].X,lSegm^.nResType^.nResSubst);
        aFrm:=new(UniversalFormulaPtr,Init(lPos,lSegm,aFrm));
        lF2RV^.nFreeVar:=Items^[i].X;
        lF2RV^.Process_Formula(aFrm);
1:
       end
      else
       begin
        lAddFV:=new(AddFreeVarInScopePtr,
                     Init(exFormula,Items^[i].X,lAFV^.nAdjScopeIndex));
        lAddFV^.Process_Formula(aFrm);
        dispose(lAddFV,Done);
        AtDelete(i);
       end;
      dec(i);
     end;
    end;
    dispose(lF2RV,Done);
    dispose(lAFV,Done);
   end;
//
  if lFV^.nFreeVars.Count > 0 then
    nLastFreeVars.MoveNatSet(lFV^.nFreeVars);
  dispose(lFV,Done);
  lSBV:=new(SetVarInExpressionPtr, Init(exTerm));
  lSBV^.Process_Formula(aFrm);
  dispose(lSBV,Done);
end;

{----------------------------------------------------------------}

constructor MSLocalReferenceObj.Init(aLabId:integer; const aPos:Position; aRef,aLab:integer);
begin inherited Init(aLabId,aPos);
 nRefNr:=aRef;
 nLabNr:=aLab;
end;

constructor MSLabelObj.Init(aLabelId: integer; const aPos:Position; aNr,aLabelNr: integer);
begin
 inherited Init(aLabelId,aPos);
 nSerialNr:=aNr;
 nLabelNr:=aLabelNr;
end;

constructor MSSchemeJustificationObj.Init(const aPos:Position; aArticleNr,aNr,aSchNr:integer);
begin
 inherited Init(aPos,aArticleNr,aNr);
 nSchemeNr:=aSchNr;
end;

constructor MSStraightforwardJustificationObj.Init(const aPos:Position;
                                            aLinked:boolean; const aLinkPos:Position;
                                            aPrevLabelNr:integer);
begin
 inherited Init(aPos,aLinked,AlinkPos);
 nLabelNr:=aPrevLabelNr;
end;

destructor MSStraightforwardJustificationObj.Done;
begin
 inherited Done;
end;

constructor MSSchemeObj.Init(aIdNr,AschNr:integer; const aPos:Position; aParams:PList;
                            aPrems:PList; aConcl:FormulaPtr);
begin
 inherited Init(aIdNr,aPos,aParams,aPrems,aConcl);
 nSchemeNr:=aSchNr;
end;

{----------------------------------------------------------------}

constructor MSOutMizFileObj.OpenFile(const aFileName:string);
begin
 inherited OpenFile( aFileName);
end;

destructor MSOutMizFileObj.Done;
begin
 inherited Done;
end;


procedure MSOutMizFileObj.Out_Label(aLab:LabelPtr);
begin
 if aLab <> nil then
 begin
  Out_XElStart( XMLElemName[elLabel]);
  Out_XIntAttr( XMLAttrName[atIdNr], aLab^.nLabelIdNr);
  if nMizarAppearance then
   Out_XAttr( XMLAttrName[atSpelling], IdentRepr(aLab^.nLabelIdNr));
  Out_PosAsAttrs(aLab^.nLabelPos);
  Out_XIntAttr( XMLAttrName[atSerialNr], MSLabelPtr(aLab)^.nSerialNr);
  Out_XIntAttr( XMLAttrName[atLabelNr], MSLabelPtr(aLab)^.nLabelNr);
  Out_XElEnd0;
 end;
end;

procedure MSOutMizFileObj.Out_LocalReference(aRef: LocalReferencePtr);
begin
 with MSLocalReferencePtr(aRef)^ do
  begin
   Out_XElStart(ReferenceKindName[LocalReference]);
   Out_PosAsAttrs(nRefPos);
   Out_XIntAttr( XMLAttrName[atIdNr], nLabId);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nLabId));
   Out_XIntAttr( XMLAttrName[atSerialNr], nRefNr);
   Out_XIntAttr( XMLAttrName[atLabelNr], nLabNr);
   Out_XElEnd0;
  end;
end;

procedure MSOutMizFileObj.Out_Link(aInf: JustificationPtr);
begin
  with MSStraightforwardJustificationPtr(aInf)^ do
   if nLinked then
   begin
     Out_XElStart(XMLElemName[elLink]);
     Out_PosAsAttrs(nLinkPos);
     Out_XIntAttr( XMLAttrName[atLabelNr],nLabelNr);
     Out_XElEnd0;
   end;
end;

procedure MSOutMizFileObj.Out_SchemeJustification(aInf: SchemeJustificationPtr);
begin
 with MSSchemeJustificationPtr(aInf)^ do
  begin
   Out_XElStart(InferenceName[infSchemeJustification]);
   Out_XIntAttr( XMLAttrName[atNr],nSchFileNr);
   Out_XIntAttr( XMLAttrName[atIdNr],nSchemeIdNr);
   Out_XIntAttr( XMLAttrName[atSchNr],nSchemeNr);
   if nMizarAppearance then
    if nSchFileNr > 0 then
     Out_XAttr( XMLAttrName[atSpelling], MMLIdentifierName[nSchFileNr])
    else if nSchemeIdNr > 0 then
     Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nSchemeIdNr));
   Out_PosAsAttrs(nInfPos);
   Out_XIntAttr( XMLAttrName[atPosLine], nSchemeInfPos.Line);
   Out_XIntAttr( XMLAttrName[atPosCol], nSchemeInfPos.Col);
   Out_XAttrEnd;
   Out_References(nReferences);
   Out_XElEnd(InferenceName[infSchemeJustification]);
  end;
end;

procedure MSOutMizFileObj.Out_Variable( aVar: VariablePtr);
begin
 with MSVariablePtr(aVar)^ do
 begin
   Out_XElStart( XMLElemName[elVariable]);
   Out_XIntAttr( XMLAttrName[atIdNr], nIdent);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nIdent));
   Out_PosAsAttrs(nVarPos);
   Out_XAttr( XMLAttrName[atOrigin], VariableKindName[MSVariablePtr(aVar)^.nOrigin]);
   Out_XAttr( XMLAttrName[atKind], VariableKindName[MSVariablePtr(aVar)^.nVarKind]);
   Out_XIntAttr( XMLAttrName[atSerialNr], nSerialNr);
   Out_XIntAttr( XMLAttrName[atVarNr], nVarNr);
   Out_XElEnd0
 end;
end;

procedure MSOutMizFileObj.Out_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr);
 var i: integer;
begin
 with MSImplicitlyQualifiedSegmentPtr(aSegm)^,nResType^ do
 begin
  Out_XElStart( SegmentKindName[ikImplQualifiedSegm]);
  Out_PosAsAttrs(nSegmPos);
  Out_XIntAttr( XMLAttrName[atNr], nResTypeNr);
  Out_XAttrEnd;
  Out_Variable(nIdentifier);
  for i:=0 to nResSubst.fCount-1 do
   with nResSubst.fList[i] do
    begin
     Out_XElStart( XMLElemName[elSubstitution]);
     Out_XIntAttr( XMLAttrName[atX],X);
     Out_XIntAttr( XMLAttrName[atY1],Y1);
     Out_XIntAttr( XMLAttrName[atY2],Y2);
     Out_XElEnd0;
    end;
  Out_XElEnd( SegmentKindName[ikImplQualifiedSegm]);
 end;
end;

procedure MSOutMizFileObj.Out_ReservedVariable( aVar: VariablePtr);
begin
 Out_Variable(aVar);
end;

procedure MSOutMizFileObj.Out_SimpleTerm ( aTrm: SimpleTermPtr );
begin
  Out_XElStart( TermName[wsSimpleTerm]);
  Out_XIntAttr( XMLAttrName[atIdNr], aTrm^.nIdent);
  if nMizarAppearance then
   Out_XAttr( XMLAttrName[atSpelling], IdentRepr(aTrm^.nIdent));
  Out_PosAsAttrs(aTrm^.nTermPos);
  Out_XAttr( XMLAttrName[atOrigin], VariableKindName[MSSimpleTermPtr(aTrm)^.nOrigin]);
  Out_XAttr( XMLAttrName[atKind], VariableKindName[MSSimpleTermPtr(aTrm)^.nVarKind]);
  Out_XIntAttr( XMLAttrName[atSerialNr], MSSimpleTermPtr(aTrm)^.nSerialNr);
  Out_XIntAttr( XMLAttrName[atVarNr], MSSimpleTermPtr(aTrm)^.nVarNr);
  Out_XElEnd0;
end;

procedure MSOutMizFileObj.Out_Locus( aLocus: LocusPtr);
begin
 with MSLocusPtr(aLocus)^ do
 begin
   Out_XElStart( XMLElemName[elLocus]);
   Out_XIntAttr( XMLAttrName[atIdNr], nVarId);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nVarId));
   Out_PosAsAttrs(nVarIdPos);
   Out_XAttr( XMLAttrName[atOrigin], VariableKindName[MSLocusPtr(aLocus)^.nOrigin]);
   Out_XAttr( XMLAttrName[atKind], VariableKindName[MSLocusPtr(aLocus)^.nOrigin]);
   Out_XIntAttr( XMLAttrName[atSerialNr],nSerialNr);
   Out_XIntAttr( XMLAttrName[atVarNr], nVarNr);
   Out_XElEnd0
 end;
end;

procedure MSOutMizFileObj.Out_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr );
begin
 with MSPrivateFunctorTermPtr(aTrm)^ do
 begin
  Out_XElStart(TermName[wsPrivateFunctorTerm]);
  Out_XIntAttr( XMLAttrName[atIdNr], nFunctorIdent);
  if nMizarAppearance then
   Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nFunctorIdent));
  Out_PosAsAttrs(nTermPos);
  Out_XAttr( XMLAttrName[atShape], VariableKindName[nFuncKind]);
  Out_XIntAttr( XMLAttrName[atSerialNr], nSerialNr);
  Out_XIntAttr( XMLAttrName[atNr], nFuncNr);
  if nArgs^.Count = 0 then Out_XElEnd0
  else begin
   Out_XAttrEnd;
   Out_TermList( nArgs);
   Out_XElEnd( TermName[wsPrivateFunctorTerm]);
  end;
 end;
end;

procedure MSOutMizFileObj.Out_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr );
begin
 with MSPrivatePredicativeFormulaPtr(aFrm)^ do
 begin
  Out_XElStart(FormulaName[wsPrivatePredicateFormula]);
  Out_XIntAttr( XMLAttrName[atIdNr], nPredIdNr);
  if nMizarAppearance then
   Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nPredIdNr));
  Out_PosAsAttrs(nFormulaPos);
  Out_XAttr( XMLAttrName[atShape], VariableKindName[nPredKind]);
  Out_XIntAttr( XMLAttrName[atSerialNr], nSerialNr);
  Out_XIntAttr( XMLAttrName[atNr], nPredNr);
  if nArgs^.Count = 0 then Out_XElEnd0
   else begin
    Out_XAttrEnd;
    Out_TermList( nArgs);
    Out_XElEnd( FormulaName[wsPrivatePredicateFormula]);
   end;
 end;
end;

procedure MSOutMizFileObj.Out_InternalSelectorTerm ( aTrm: InternalSelectorTermPtr );
begin
  with MSInternalSelectorTermPtr(aTrm)^ do
  begin
    Out_XElStart(TermName[wsInternalSelectorTerm]);
    Out_XIntAttr( XMLAttrName[atNr], nSelectorSymbol);
    Out_XIntAttr( XMLAttrName[atVarNr], nVarNr);
    if nMizarAppearance then
     Out_XAttr( XMLAttrName[atSpelling], SelectorName[nSelectorSymbol]);
    Out_PosAsAttrs(nTermPos);
    Out_XElEnd0;
  end;
end;

procedure MSOutMizFileObj.Out_Type ( aTyp: TypePtr);
 var i: integer;
begin
 with aTyp^ do
  case aTyp^.nTypeSort of
  wsReservedDscrType:
   with MSReservedDscrTypePtr(aTyp)^ do
   begin
     Out_XElStart( TypeName[wsReservedDscrType] );
     Out_XIntAttr( XMLAttrName[atIdNr], nIdent);
     Out_XIntAttr( XMLAttrName[atNr], nResTypeNr);
     Out_PosAsAttrs(nTypePos);
     if  nResSubst.fCount = 0 then Out_XElEnd0
     else begin
      Out_XAttrEnd;
      for i:=0 to nResSubst.fCount-1 do
       with nResSubst.fList[i] do
        begin
         Out_XElStart( XMLElemName[elSubstitution]);
         Out_XIntAttr( XMLAttrName[atX],X);
         Out_XIntAttr( XMLAttrName[atY1],Y1);
         Out_XIntAttr( XMLAttrName[atY2],Y2);
         Out_XElEnd0;
        end;
      Out_XElEnd( TypeName[wsReservedDscrType] );
     end;
   end
  else
   inherited Out_Type(aTyp);
  end;
end;

procedure MSOutMizFileObj.Out_ReservationSegment(aRes:ReservationSegmentPtr);
 var i: integer;
begin
  with MSReservationSegmentPtr(aRes)^ do
   begin
    Out_XElStart0( XMLElemName[elVariables]);
    for i:=0 to nIdentifiers.Count-1 do
     Out_ReservedVariable( nIdentifiers.Items^[i]);
    Out_XElEnd( XMLElemName[elVariables]);
    Out_TypeList(nFreeVars);
//    Out_XElStart0( XMLElemName[elTypeSpecification]);
    Out_Type(nResType);
//    Out_XElEnd( XMLElemName[elTypeSpecification]);
    for i:=0 to nResVars^.Count-1 do
     with nResVars^.Items^[i] do
     begin
      Out_XElStart( XMLElemName[elSetMember]);
      Out_XIntAttr( XMLAttrName[atX],X);
      Out_XElEnd0;
     end;
   end;
end;

procedure MSOutMizFileObj.Out_SchemeNameInSchemeHead(aSch: SchemePtr);
begin
 Out_XIntAttr( XMLAttrName[atIdNr], aSch^.nSchemeIdNr);
 if nMizarAppearance then
  Out_XAttr( XMLAttrName[atSpelling], IdentRepr(aSch^.nSchemeIdNr));
 Out_XIntAttr( XMLAttrName[atNr], MSSchemePtr(aSch)^.nSchemeNr);
end;

procedure Write_MSMizArticle(aMSTextProper:WSTextProperPtr; aFileName:string);
 var lWSMizOutput: MSOutMizFilePtr;
begin
  InitScannerNames;
  lWSMizOutput:=new(MSOutMizFilePtr,OpenFile(aFileName));
  lWSMizOutput^.nMizarAppearance:=true;
  lWSMizOutput^.Out_TextProper(aMSTextProper);
  dispose(lWSMizOutput,Done);
end;

constructor MSInMizFileObj.OpenFile(const aFileName:string );
begin
 inherited OpenFile(aFileName);
end;

destructor MSInMizFileObj.Done;
begin
 inherited Done;
end;

function MSInMizFileObj.Read_ReservationSegment: ReservationSegmentPtr;
 var lList,lTypList: PList;
     lTyp: TypePtr;
     lVars: NatSet;
begin
 lList:=new(PList,Init(0));
 NextElementState; //elVariables
 while (nState = eStart) and (nElName = XMLElemName[elVariable]) do
  lList^.Insert(Read_Variable);
 NextElementState;
 lTypList:=Read_TypeList;
 lTyp:=Read_Type;
 lVars.Init(0,MaxResIdNbr);
 while (nState = eStart) and (nElName = XMLElemName[elSetMember]) do
  begin
   lVars.InsertElem(GetIntAttr(XMLAttrName[atX]));
   NextElementState;
   NextElementState;
  end;
 result:=new(MSReservationSegmentPtr,Init(lList,lTyp,lVars));
 MSReservationSegmentPtr(result)^.nFreeVars:=lTypList;
end;

function MSInMizFileObj.Read_SchemeNameInSchemeHead: SchemePtr;
 var lIdNr,lNr: Integer;
     lPos: Position;
begin
 lPos:=GetAttrPos;
 lIdNr:=GetIntAttr(XMLAttrName[atIdNr]);
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 result:=new(MSSchemePtr,Init(lIdNr,lNr,lPos,nil,nil,nil));
end;

function MSInMizFileObj.Read_Label: LabelPtr;
 var lLabPos: Position;
     lLabId,lNr,lLabelNr: Integer;
begin
  result:=nil;
  if (nState= eStart)  and (nElName = XMLElemName[elLabel]) then
   begin
    lLabId:=GetIntAttr(XMLAttrName[atIdNr]);
    lLabPos:=GetAttrPos;
    lNr:=GetIntAttr(XMLAttrName[atSerialNr]);
    lLabelNr:=GetIntAttr(XMLAttrName[atLabelNr]);
    NextElementState;
    NextElementState;
    result:=new(MSLabelPtr,Init(lLabId,lLabPos,lNr,lLabelNr));
   end;
end;

function MSInMizFileObj.Read_StraightforwardJustification: StraightforwardJustificationPtr;
 var lPos,lLinkPos: Position;
     lLinked: boolean;
     lLabelNr: integer;
begin
  lPos:=GetAttrPos;
  NextElementState;
  lLinked:=false;
  lLinkPos:=lPos;
  lLabelNr:=0;
  if nelName = XMLElemName[elLink] then
   begin
     lLinked:=true;
     lLinkPos:=GetAttrPos;
     lLabelNr:=GetIntAttr(XMLAttrName[atLabelNr]);
     NextElementState;
     NextElementState;
   end;
  result:=new(MSStraightforwardJustificationPtr,Init(lPos,lLinked,lLinkPos,lLabelNr));
  StraightforwardJustificationPtr(result)^.nReferences:=Read_References;
  NextElementState;
end;

function MSInMizFileObj.Read_SchemeJustification: SchemeJustificationPtr;
 var lInfPos,lPos: Position;
     lNr,lIdNr,lSchNr: integer;
begin
 lInfPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 lIdNr:=GetIntAttr(XMLAttrName[atIdNr]);
 lSchNr:=GetIntAttr(XMLAttrName[atSchNr]);
 lPos.Line:=GetIntAttr( XMLAttrName[atPosLine]);
 lPos.Col:= GetIntAttr( XMLAttrName[atPosCol]);
 NextElementState;
 result:=new(MSSchemeJustificationPtr,Init(lInfPos,lNr,lIdNr,lSchNr));
 SchemeJustificationPtr(result)^.nSchemeInfPos:=lPos;
 SchemeJustificationPtr(result)^.nReferences:=Read_References;
 NextElementState;
end;

function MSInMizFileObj.Read_LocalReference: LocalReferencePtr;
 var lPos: Position;
     lNr,lRefNr,lLabNr: integer;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  lRefNr:=GetIntAttr(XMLAttrName[atSerialNr]);
  lLabNr:=GetIntAttr(XMLAttrName[atLabelNr]);
  NextElementState;
  NextElementState;
  result:=new(MSLocalReferencePtr, Init(lNr,lPos,lRefNr,lLabNr));
end;

function Str2VariableKind(const aName:string):VariableKind;
begin
  result:=Constant;
  if aName = VariableKindName[FreeVar] then
    result:=FreeVar
  else if aName = VariableKindName[ReservedVar] then
    result:=ReservedVar
  else if aName = VariableKindName[Bound] then
    result:=Bound
  else if aName = VariableKindName[Constant] then
    result:=Constant
  else if aName = VariableKindName[DefConstant] then
    result:=DefConstant
  else if aName = VariableKindName[SchematicFunc] then
    result:=SchematicFunc
  else if aName = VariableKindName[PrivateFunc] then
    result:=PrivateFunc
  else if aName = VariableKindName[SchematicPred] then
    result:=SchematicPred
  else if aName = VariableKindName[PrivatePred] then
    result:=PrivatePred;
end;

function MSInMizFileObj.Read_Locus: LocusPtr;
 var lPos:Position;
     lNr,lSerialNr,lVarNr: integer;
     lOrigin,lVarKind: VariableKind;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  lVarKind:=Str2VariableKind(GetAttr(XMLAttrName[atKind]));
  lOrigin:=Str2VariableKind(GetAttr(XMLAttrName[atOrigin]));
  lSerialNr:=GetIntAttr(XMLAttrName[atSerialNr]);
  lVarNr:=GetIntAttr(XMLAttrName[atVarNr]);
  NextElementState;
  result:=new(MSLocusPtr,Init(lPos,lNr,lOrigin,lVarKind,lSerialNr,lVarNr));
  NextElementState;
end;

function MSInMizFileObj.Read_Variable: VariablePtr;
 var lPos:Position;
     lNr,lSerialNr,lVarNr: integer;
     lOrigin,lVarKind: VariableKind;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  lOrigin:=Str2VariableKind(GetAttr(XMLAttrName[atOrigin]));
  lVarKind:=Str2VariableKind(GetAttr(XMLAttrName[atKind]));
  lSerialNr:=GetIntAttr(XMLAttrName[atSerialNr]);
  lVarNr:=GetIntAttr(XMLAttrName[atVarNr]);
  NextElementState;
  result:=new(MSVariablePtr,Init(lPos,lNr,lOrigin,lVarKind,lSerialNr,lVarNr));
  NextElementState;
end;

function MSInMizFileObj.Read_ImplicitlyQualifiedSegment: ImplicitlyQualifiedSegmentPtr;
 var lPos: Position;
     lVar: VariablePtr;
     lResDesType: MSReservedDscrTypePtr;
     lNr: integer;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 NextElementState;
 lVar:=Read_Variable;
 lResDesType:=new(MSReservedDscrTypePtr,Init(lPos,lVar^.nIdent,lNr));
 while (nState = eStart) and (nElName = XMLElemName[elSubstitution]) do
  begin
    lResDesType^.nResSubst.Assign(GetIntAttr(XMLAttrName[atX]),
                                  GetIntAttr(XMLAttrName[atY1]),
                                  GetIntAttr(XMLAttrName[atY2]));
    NextElementState;
    NextElementState;
  end;
 result:=new(MSImplicitlyQualifiedSegmentPtr,Init(lPos,lVar,lResDesType));
 NextElementState;
end;

function MSInMizFileObj.Read_SimpleTerm: SimpleTermPtr;
 var lPos:Position;
     lNr,lSerialNr,lVarNr: integer;
     lOrigin,lVarKind: VariableKind;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  NextElementState;
  lOrigin:=Str2VariableKind(GetAttr(XMLAttrName[atOrigin]));
  lVarKind:=Str2VariableKind(GetAttr(XMLAttrName[atKind]));
  lSerialNr:=GetIntAttr(XMLAttrName[atSerialNr]);
  lVarNr:=GetIntAttr(XMLAttrName[atVarNr]);
  result:=new(MSSimpleTermPtr,Init(lPos,lNr,lOrigin,lVarKind,lSerialNr,lVarNr));
  NextElementState;
end;

function MSInMizFileObj.Read_PrivateFunctorTerm: PrivateFunctorTermPtr;
 var lPos:Position;
     lNr,lSerialNr,lFuncNr: integer;
     lFuncKind: LocalDefinitionsKind;
     lFuncKindName: string;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atIdNr]);
 lFuncKindName:=GetAttr(XMLAttrName[atShape]);
 if lFuncKindName = VariableKindName[PrivateFunc] then
   lFuncKind:=PrivateFunc
 else lFuncKind:=SchematicFunc;
 lSerialNr:=GetIntAttr(XMLAttrName[atSerialNr]);
 lFuncNr:=GetIntAttr(XMLAttrName[atNr]);
 NextElementState;
 result:=new(MSPrivateFunctorTermPtr,Init(lPos,lNr,Read_TermList,lFuncKind,lSerialNr,lFuncNr));
 NextElementState;
end;

function MSInMizFileObj.Read_InternalSelectorTerm: InternalSelectorTermPtr;
 var lPos:Position;
     lNr,lVarNr: integer;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 lVarNr:=GetIntAttr(XMLAttrName[atVarNr]);
 NextElementState;
 result:=new(MSInternalSelectorTermPtr,Init(lPos,lNr,lVarNr));
 NextElementState;
end;

function MSInMizFileObj.Read_PrivatePredicativeFormula:PrivatePredicativeFormulaPtr;
 var lPos:Position;
     lNr,lSerialNr,lPredNr: integer;
     lPredKind: LocalDefinitionsKind;
     lPredKindName: string;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atIdNr]);
 lPredKindName:=GetAttr(XMLAttrName[atShape]);
 if lPredKindName = VariableKindName[PrivatePred] then
   lPredKind:=PrivatePred
 else lPredKind:=SchematicPred;
 lSerialNr:=GetIntAttr(XMLAttrName[atSerialNr]);
 lPredNr:=GetIntAttr(XMLAttrName[atNr]);
 NextElementState;
 Result:=new(MSPrivatePredicativeFormulaPtr,Init(lPos,lNr,Read_TermList,lPredKind,lSerialNr,lPredNr));
 NextElementState;
end;

function MSInMizFileObj.Read_Type: TypePtr;
 var lPos: Position;
     lIdNr,lNr: integer;
begin
  if nElName = TypeName[wsReservedDscrType] then
   begin
    lPos:=GetAttrPos;
    lIdNr:=GetIntAttr(XMLAttrName[atIdNr]);
    lNr:=GetIntAttr(XMLAttrName[atNr]);
    NextElementState;
    result:=new(MSReservedDscrTypePtr,Init(lPos,lIdNr,lNr));
    MSReservedDscrTypePtr(result)^.nResSubst.Init(0);
    while (nState = eStart) and (nElName = XMLElemName[elSubstitution]) do
     begin
       MSReservedDscrTypePtr(result)^.nResSubst.Assign(GetIntAttr(XMLAttrName[atX]),
                                                       GetIntAttr(XMLAttrName[atY1]),
                                                       GetIntAttr(XMLAttrName[atY2]));
       NextElementState;
       NextElementState;
     end;
    NextElementState;
   end
  else
   result:=inherited Read_Type;
end;

function Read_MSMizArticle(aFileName:string): WSTextProperPtr;
 var lInFile: MSInMizFilePtr;
begin
 InitWSLookupTables;
 lInFile:=new(MSInMizFilePtr,OpenFile(aFileName));
 result:=WSTextProperPtr(lInFile^.Read_TextProper);
 dispose(lInFile,Done);
 DisposeWSLookupTables;
end;

constructor MSMizarPrinterObj.OpenFile(const aFileName:string);
begin
 inherited OpenFile(aFileName);
end;

destructor MSMizarPrinterObj.Done;
begin
 inherited Done;
end;

procedure MSMizarPrinterObj.Print_Label(aLab:LabelPtr);
begin
 if aLab <> nil then
  begin
   Print_Char('L');
   Print_Number(MSLabelPtr(aLab)^.nSerialNr);
   Print_String(':');
  end;
end;

procedure MSMizarPrinterObj.Print_Reference(aRef: LocalReferencePtr);
begin
  Print_Char('L');
  Print_Number(MSLocalReferencePtr(aRef)^.nRefNr);
end;

procedure MSMizarPrinterObj.Print_StraightforwardJustification(aInf: StraightforwardJustificationPtr);
begin
  with MSStraightforwardJustificationPtr(aInf)^ do
  begin
   if nReferences^.Count <> 0 then
    begin
     Print_String(TokenName[sy_By]);
     Print_References(nReferences);
    end;
  end;
end;

procedure MSMizarPrinterObj.Print_SchemeNameInSchemeHead(aSch: SchemePtr);
begin
  with aSch^ do
  begin
    Print_String(IdentRepr(nSchemeIdNr));
  end;
end;

procedure MSMizarPrinterObj.Print_SchemeNameInJustification(aInf: SchemeJustificationPtr);
begin
 with aInf^ do
 begin
   Print_String(IdentRepr(nSchemeIdNr));
 end;
end;

procedure MSMizarPrinterObj.Print_Variable( aVar: VariablePtr);
begin
 with MSVariablePtr(aVar)^ do
 begin
  Print_Char(VarFirstLetter[nOrigin]);
  Print_Number(nSerialNr);
 end;
end;

procedure MSMizarPrinterObj.Print_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Print_Variable( aSegm^.nIdentifier);
end;

procedure MSMizarPrinterObj.Print_SimpleTermTerm ( aTrm: SimpleTermPtr );
begin
 with MSSimpleTermPtr(aTrm)^ do
   begin
    Print_Char(VarFirstLetter[nOrigin]);
    Print_Number(nSerialNr);
   end;
end;

procedure MSMizarPrinterObj.Print_Locus( aLocus: LocusPtr);
begin
 with MSLocusPtr(aLocus)^ do
 begin
   Print_Char(VarFirstLetter[nOrigin]);
   Print_Number(nSerialNr);
 end;
end;

procedure MSMizarPrinterObj.Print_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr );
begin
  Print_Char(VarFirstLetter[MSPrivateFunctorTermPtr(aTrm)^.nFuncKind]);
  Print_Number(MSPrivateFunctorTermPtr(aTrm)^.nSerialNr);
  Print_String('(');
  Print_OpenTermList(aTrm^.nArgs);
  Print_String(')');
end;

procedure MSMizarPrinterObj.Print_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr );
begin
 with MSPrivatePredicativeFormulaPtr(aFrm)^ do
 begin
  Print_Char(VarFirstLetter[nPredKind]);
  Print_Number(nSerialNr);
  Print_String('[');
  Print_OpenTermList( nArgs);
  Print_String(']');
 end;
end;

procedure MSMizarPrinterObj.Print_Linkage;
begin
end;

procedure MSMizarPrinterObj.Print_ReservedType(aResType: TypePtr);
begin
 Print_Type(aResType);
end;

procedure Print_MSMizArticle(aMSTextProper:WSTextProperPtr; aFileName:string);
 var lWSMizOutput: MSMizarPrinterPtr;
begin
 InitScannerNames;
 lWSMizOutput:=new(MSMizarPrinterPtr,OpenFile(aFileName));
 lWSMizOutput^.Print_TextProper(aMSTextProper);
 dispose(lWSMizOutput,Done);
end;

{----------------------------------------------------------------}

procedure MSMAnalyzer;
 var lWSTextProper: wsTextProperPtr;
     lMizArticle: ProcessingArticlePtr;
begin
 lWSTextProper:=Read_WSMizArticle(MizFileName+'.wsx');
 lMizArticle:=new(FirstIdentArticlePtr,Init(lWSTextProper));
 lMizArticle^.nDisplayInformationOnScreen:=true;
 lMizArticle^.Process_Article;
 Write_MSMizArticle(lWSTextProper,MizFileName+'.msx');
 dispose(lMizArticle,Done);
end;

end.
