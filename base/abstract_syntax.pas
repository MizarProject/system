(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

 unit abstract_syntax;

interface

uses errhan,mobjects,syntax;

type

   TypeSort = (  wsErrorType,
                 wsStandardType,
                 wsStructureType,
                 wsClusteredType,
                 wsReservedDscrType
               );

   { Initial structures }

   TypePtr = ^TypeExpressionObj;
   TypeExpressionObj = object(MObject)
      nTypeSort: TypeSort;
      nTypePos: Position;
   end;

   TermSort = (  wsErrorTerm,
                 wsPlaceholderTerm,
                 wsNumeralTerm,
                 wsSimpleTerm,
                 wsPrivateFunctorTerm,
                 wsInfixTerm,
                 wsCircumfixTerm,
                 wsAggregateTerm,
                 wsForgetfulFunctorTerm,
                 wsInternalForgetfulFunctorTerm,
                 wsSelectorTerm,
                 wsInternalSelectorTerm,
                 wsQualificationTerm,
                 wsGlobalChoiceTerm,
                 wsSimpleFraenkelTerm,
                 wsFraenkelTerm,
                 wsItTerm,
                 wsExactlyTerm
              );

   TermPtr = ^TermExpressionObj;
   TermExpressionObj = object(MObject)
      nTermSort: TermSort;
      nTermPos: Position;
   end;

   FormulaSort = (  wsErrorFormula,
                    wsThesis,
                    wsContradiction,
                    wsRightSideOfPredicativeFormula,
                    wsPredicativeFormula,
                    wsMultiPredicativeFormula,
                    wsPrivatePredicateFormula,
                    wsAttributiveFormula,
                    wsQualifyingFormula,
                    wsUniversalFormula,
                    wsExistentialFormula,
                    wsNegatedFormula,
                    wsConjunctiveFormula,
                    wsDisjunctiveFormula,
                    wsConditionalFormula,
                    wsBiconditionalFormula,
                    wsFlexaryConjunctiveFormula,
                    wsFlexaryDisjunctiveFormula
                 );

   FormulaPtr = ^FormulaExpressionObj;
   FormulaExpressionObj = object(MObject)
      nFormulaSort: FormulaSort;
      nFormulaPos: Position;
   end;

   AdjectiveSort = (wsNegatedAdjective,wsAdjective);

   AdjectiveExpressionPtr = ^AdjectiveExpressionObj;
   AdjectiveExpressionObj = object(MObject)
      nAdjectivePos: Position;
      nAdjectiveSort: AdjectiveSort;
      constructor Init(const aPos:Position; aSort: AdjectiveSort);
      destructor Done; virtual;
   end;

   NegatedAdjectivePtr = ^NegatedAdjectiveObj;
   NegatedAdjectiveObj = object(AdjectiveExpressionObj)
       nArg: AdjectiveExpressionPtr; { of TermPtr, visible arguments }
      constructor Init(const aPos:Position; aArg:AdjectiveExpressionPtr);
      destructor Done; virtual;
   end;

   AdjectivePtr = ^AdjectiveObj;
   AdjectiveObj = object(AdjectiveExpressionObj)
      nAdjectiveSymbol: integer;
      nNegated: boolean;
      nArgs: PList; { of TermPtr, visible arguments }
      constructor Init(const aPos:Position; aAdjectiveNr: integer; aArgs:PList);
      destructor Done; virtual;
   end;

   { Auxiliary structures }

  SegmentKind = ( ikImplQualifiedSegm, ikExplQualifiedSegm);

   VariablePtr = ^VariableObj;
   VariableObj = object(MObject)
     nIdent: integer; { identifier number }
     nVarPos: Position;
     constructor Init(const aPos:Position; aIdentNr:integer);
   end;

   QualifiedSegmentPtr = ^QualifiedSegmentObj;
   QualifiedSegmentObj = object(MObject)
      nSegmPos: Position;
      nSegmentSort: SegmentKind;
      constructor Init(const aPos:Position; aSort:SegmentKind);
   end;

   ImplicitlyQualifiedSegmentPtr = ^ImplicitlyQualifiedSegmentObj;
   ImplicitlyQualifiedSegmentObj = object(QualifiedSegmentObj)
      nIdentifier: VariablePtr;
      constructor Init(const aPos:Position; aIdentifier:VariablePtr);
      destructor Done; virtual;
   end;

   ExplicitlyQualifiedSegmentPtr = ^ExplicitlyQualifiedSegmentObj;
   ExplicitlyQualifiedSegmentObj = object(QualifiedSegmentObj)
      nIdentifiers: PList; { of identifier numbers }
      nType: TypePtr;
      constructor Init(const aPos:Position; aIdentifiers:PList; aType:TypePtr);
      destructor Done; virtual;
   end;

   { Terms }

   SimpleTermPtr = ^SimpleTermObj;
   SimpleTermObj = object(TermExpressionObj)
      nIdent: integer; { identifier number }
      constructor Init(const aPos:Position; aIdentNr:integer);
   end;

   PlaceholderTermPtr = ^PlaceholderTermObj;  //placeholder
   PlaceholderTermObj = object(TermExpressionObj)
      nLocusNr: integer; { $1, ... }
      constructor Init(const aPos:Position; aLocusNr:integer);
   end;

   NumeralTermPtr = ^NumeralTermObj;
   NumeralTermObj = object(TermExpressionObj)
      nValue: integer;
      constructor Init(const aPos:Position; aValue:integer);
   end;

   InfixTermPtr = ^InfixTermObj;
   InfixTermObj = object(TermExpressionObj)
      nFunctorSymbol: integer;
      nLeftArgs,nRightArgs: PList;
      constructor Init(const aPos:Position; aFunctorNr:integer; aLeftArgs,aRightArgs:PList);
      destructor Done; virtual;
   end;

   TermWithArgumentsPtr = ^TermWithArgumentsObj;
   TermWithArgumentsObj = object(TermExpressionObj)
      nArgs: PList;
      constructor Init(const aPos:Position; aKind:TermSort; aArgs:PList);
      destructor Done; virtual;
   end;

   CircumfixTermPtr = ^CircumfixTermObj;
   CircumfixTermObj = object(TermWithArgumentsObj)
      nLeftBracketSymbol,nRightBracketSymbol: integer;
      constructor Init(const aPos:Position; aLeftBracketNr,aRightBracketNr:integer; aArgs:PList);
      destructor Done; virtual;
   end;

   PrivateFunctorTermPtr = ^PrivateFunctorTermObj;
   PrivateFunctorTermObj = object(TermWithArgumentsObj)
      nFunctorIdent: integer;
      constructor Init(const aPos:Position; aFunctorIdNr:integer; aArgs:PList);
      destructor Done; virtual;
   end;

   OneArgumentTermPtr = ^OneArgumentTermObj;
   OneArgumentTermObj = object(TermExpressionObj)
      nArg: TermPtr;
      constructor Init(const aPos:Position; aKind:TermSort; aArg:TermPtr);
      destructor Done; virtual;
   end;

   SelectorTermPtr = ^SelectorTermObj;
   SelectorTermObj = object(OneArgumentTermObj)
      nSelectorSymbol: integer;
      constructor Init(const aPos:Position; aSelectorNr:integer; aArg:TermPtr);
      destructor Done; virtual;
   end;

   InternalSelectorTermPtr = ^InternalSelectorTermObj;
   InternalSelectorTermObj = object(TermExpressionObj)
      nSelectorSymbol: integer;
      constructor Init(const aPos:Position; aSelectorNr:integer);
   end;

   AggregateTermPtr = ^AggregateTermObj;
   AggregateTermObj = object(TermWithArgumentsObj)
      nStructSymbol: integer;
      constructor Init(const aPos:Position; aStructSymbol:integer; aArgs:PList);
      destructor Done; virtual;
   end;

   ForgetfulFunctorTermPtr = ^ForgetfulFunctorTermObj;
   ForgetfulFunctorTermObj = object(OneArgumentTermObj)
      nStructSymbol: integer;
      constructor Init(const aPos:Position; aStructSymbol:integer; aArg:TermPtr);
      destructor Done; virtual;
   end;

   InternalForgetfulFunctorTermPtr = ^InternalForgetfulFunctorTermObj;
   InternalForgetfulFunctorTermObj = object(TermExpressionObj)
      nStructSymbol: integer;
      constructor Init(const aPos:Position; aStructSymbol:integer);
   end;

   SimpleFraenkelTermPtr = ^SimpleFraenkelTermObj;
   SimpleFraenkelTermObj = object(TermExpressionObj)
      nPostqualification: PList; { of segments }
      nSample: TermPtr;
      constructor Init(const aPos:Position; aPostqual:PList; aSample:TermPtr);
      destructor Done; virtual;
   end;

   FraenkelTermPtr = ^FraenkelTermObj;
   FraenkelTermObj = object(SimpleFraenkelTermObj)
      nFormula: FormulaPtr;
      constructor Init(const aPos:Position; aPostqual:PList; aSample:TermPtr; aFormula:FormulaPtr);
      destructor Done; virtual;
   end;

   ExactlyTermPtr = ^ExactlyTermObj;
   ExactlyTermObj = object(TermExpressionObj)
      nSubject: TermPtr;
      constructor Init(const aPos:Position; aSubject:TermPtr);
      destructor Done; virtual;
   end;

   QualifiedTermPtr = ^QualifiedTermObj;
   QualifiedTermObj = object(ExactlyTermObj)
      nQualification: TypePtr;
      constructor Init(const aPos:Position; aSubject:TermPtr; aType:TypePtr);
      destructor Done; virtual;
   end;

   ChoiceTermPtr = ^ChoiceTermObj;
   ChoiceTermObj = object(TermExpressionObj)
      nChoiceType: TypePtr;
      constructor Init(const aPos:Position; aType:TypePtr);
      destructor Done; virtual;
   end;

   ItTermPtr = ^ItTermObj;
   ItTermObj = object(TermExpressionObj)
      constructor Init(const aPos:Position);
   end;

   IncorrectTermPtr = ^IncorrectTermObj;
   IncorrectTermObj = object(TermExpressionObj)
      constructor Init(const aPos:Position);
   end;

   { Types }

   RadixTypePtr = ^RadixTypeObj;
   RadixTypeObj = object(TypeExpressionObj)
      nArgs: PList; { of }
      constructor Init(const aPos:Position; aKind: TypeSort; aArgs:PList);
      destructor Done; virtual;
   end;

   StandardTypePtr = ^StandardTypeObj;
   StandardTypeObj = object(RadixTypeObj)
      nModeSymbol: integer;
      constructor Init(const aPos:Position; aModeSymbol:integer; aArgs:PList);
      destructor Done; virtual;
   end;

   StructTypePtr = ^StructTypeObj;
   StructTypeObj = object(RadixTypeObj)
      nStructSymbol: integer;
      constructor Init(const aPos:Position; aStructSymbol:integer; aArgs:PList);
      destructor Done; virtual;
   end;

   ClusteredTypePtr = ^ClusteredTypeObj;
   ClusteredTypeObj = object(TypeExpressionObj)
      nAdjectiveCluster: PList;
      nType: TypePtr;
      constructor Init(const aPos:Position; aCluster:PList; aType:TypePtr);
      destructor Done; virtual;
   end;

   IncorrectTypePtr = ^IncorrectTypeObj;
   IncorrectTypeObj = object(TypeExpressionObj)
      constructor Init(const aPos:Position);
   end;

   { Formulas }

   RightSideOfPredicativeFormulaPtr = ^RightSideOfPredicativeFormulaObj;
   RightSideOfPredicativeFormulaObj = object(FormulaExpressionObj)
      nPredNr: integer;
      nRightArgs: PList;
     constructor Init(const aPos:Position; aPredNr:integer; aRightArgs:PList);
     destructor Done; virtual;
   end;

   PredicativeFormulaPtr = ^PredicativeFormulaObj;
   PredicativeFormulaObj = object(RightSideOfPredicativeFormulaObj)
      nLeftArgs: PList;
     constructor Init(const aPos:Position; aPredNr:integer; aLeftArgs,aRightArgs:PList);
     destructor Done; virtual;
   end;

   MultiPredicativeFormulaPtr = ^MultiPredicativeFormulaObj;
   MultiPredicativeFormulaObj = object(FormulaExpressionObj)
       nScraps: PList;
      constructor Init(const aPos:Position; aScraps:PList);
      destructor Done; virtual;
   end;

   AttributiveFormulaPtr = ^AttributiveFormulaObj;
   AttributiveFormulaObj = object(FormulaExpressionObj)
      nSubject: TermPtr;
      nAdjectives: PList;
      constructor Init(const aPos:Position; aSubject:TermPtr; aAdjectives:PList);
      destructor Done; virtual;
   end;

   PrivatePredicativeFormulaPtr = ^PrivatePredicativeFormulaObj;
   PrivatePredicativeFormulaObj = object(FormulaExpressionObj)
      nPredIdNr: integer;
      nArgs: PList;
      constructor Init(const aPos:Position; aPredIdNr:integer; aArgs:PList);
      destructor Done; virtual;
   end;

   QualifyingFormulaPtr = ^QualifyingFormulaObj;
   QualifyingFormulaObj = object(FormulaExpressionObj)
      nSubject: TermPtr;
      nType: TypePtr;
      constructor Init(const aPos:Position; aSubject:TermPtr; aType:TypePtr);
      destructor Done; virtual;
   end;

   NegativeFormulaPtr = ^NegativeFormulaObj;
   NegativeFormulaObj = object(FormulaExpressionObj)
      nArg: FormulaPtr;
      constructor Init(const aPos:Position; aArg:FormulaPtr);
      destructor Done; virtual;
   end;

   BinaryFormulaPtr = ^BinaryArgumentsFormula;
   BinaryArgumentsFormula = object(FormulaExpressionObj)
      nLeftArg,nRightArg: FormulaPtr;
      constructor Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
      destructor Done; virtual;
   end;

   ConjunctiveFormulaPtr = ^ConjunctiveFormulaObj;
   ConjunctiveFormulaObj = object(BinaryArgumentsFormula)
      constructor Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
   end;

   DisjunctiveFormulaPtr = ^DisjunctiveFormulaObj;
   DisjunctiveFormulaObj = object(BinaryArgumentsFormula)
      constructor Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
   end;

   ConditionalFormulaPtr = ^ConditionalFormulaObj;
   ConditionalFormulaObj = object(BinaryArgumentsFormula)
      constructor Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
   end;

   BiconditionalFormulaPtr = ^BiconditionalFormulaObj;
   BiconditionalFormulaObj = object(BinaryArgumentsFormula)
      constructor Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
   end;

   FlexaryConjunctiveFormulaPtr = ^FlexaryConjunctiveFormulaObj;
   FlexaryConjunctiveFormulaObj = object(BinaryArgumentsFormula)
      constructor Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
   end;

   FlexaryDisjunctiveFormulaPtr = ^FlexaryDisjunctiveFormulaObj;
   FlexaryDisjunctiveFormulaObj = object(BinaryArgumentsFormula)
      constructor Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
   end;

   QuantifiedFormulaPtr = ^QuantifiedFormulaObj;
   QuantifiedFormulaObj = object(FormulaExpressionObj)
      nSegment: QualifiedSegmentPtr;
      nScope: FormulaPtr;
      constructor Init(const aPos:Position; aSegment:QualifiedSegmentPtr; aScope:FormulaPtr);
      destructor Done; virtual;
   end;

   UniversalFormulaPtr = ^UniversalFormulaObj;
   UniversalFormulaObj = object(QuantifiedFormulaObj)
      constructor Init(const aPos:Position; aSegment:QualifiedSegmentPtr; aScope:FormulaPtr);
   end;

   ExistentialFormulaPtr = ^ExistentialFormulaObj;
   ExistentialFormulaObj = object(QuantifiedFormulaObj)
      constructor Init(const aPos:Position; aSegment:QualifiedSegmentPtr; aScope:FormulaPtr);
   end;

   ContradictionFormulaPtr = ^ContradictionFormulaObj;
   ContradictionFormulaObj = object(FormulaExpressionObj)
      constructor Init(const aPos:Position);
   end;

   ThesisFormulaPtr = ^ThesisFormulaObj;
   ThesisFormulaObj = object(FormulaExpressionObj)
      constructor Init(const aPos:Position);
   end;

   IncorrectFormulaPtr = ^IncorrectFormula;
   IncorrectFormula = object(FormulaExpressionObj)
      constructor Init(const aPos:Position);
   end;

{----------------------------------------------------------------}

 biStackedPtr = ^biStackedObj;
 biStackedObj =
  object(MObject)

  end;

 WithinExprPtr =  ^WithinExprObj;
 WithinExprObj =
  object(MObject)
    nExpKind: ExpKind;
    nStackArr: array of  biStackedPtr;
    nStackCnt: integer;

   constructor Init(aExpKind:ExpKind);
   destructor Done; virtual;
   function CreateExpressionsVariableLevel: biStackedPtr; virtual;  //??

   procedure Process_Adjective(aAttr:AdjectiveExpressionPtr ); virtual;
   procedure Process_AdjectiveList( aCluster: PList ); virtual;
   procedure Process_Variable( var aVar: VariablePtr); virtual;
   procedure Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Process_VariablesSegment( aSegm: QualifiedSegmentPtr); virtual;
   procedure Process_StartVariableSegment; virtual;
   procedure Process_FinishVariableSegment; virtual;

   procedure Process_Type ( aTyp: TypePtr ); virtual;
   procedure Process_BinaryFormula ( aFrm:BinaryFormulaPtr ); virtual;
   procedure Process_StartQuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
   procedure Process_QuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
   procedure Process_FinishQuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
   procedure Process_Formula ( aFrm:FormulaPtr ); virtual;
   procedure Process_TermList ( aTrmList:PList ); virtual;
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
   procedure Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FinishFraenkelTerm(var aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FraenkelTermsScope ( var aFrm:FormulaPtr ); virtual;
   procedure Process_SimpleFraenkelTerm(var aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_Term ( var aTrm: TermPtr ); virtual;

  end;

implementation

constructor VariableObj.Init(const aPos:Position; aIdentNr:integer);
begin
   nIdent:=aIdentNr;
   nVarPos:=aPos;
end;

constructor QualifiedSegmentObj.Init(const aPos:Position; aSort:SegmentKind);
begin
   nSegmPos:=aPos;
   nSegmentSort:=aSort;
end;

constructor ImplicitlyQualifiedSegmentObj.Init(const aPos:Position; aIdentifier:VariablePtr);
begin
   inherited Init(aPos,ikImplQualifiedSegm);
   nIdentifier:=aIdentifier;
end;

destructor ImplicitlyQualifiedSegmentObj.Done;
begin
  dispose(nIdentifier,Done);
end;

constructor ExplicitlyQualifiedSegmentObj.Init(const aPos:Position; aIdentifiers:PList;
                                              aType:TypePtr);
begin
   inherited Init(aPos,ikExplQualifiedSegm);
   nIdentifiers:=aIdentifiers;
   nType:=aType;
end;

destructor ExplicitlyQualifiedSegmentObj.Done;
begin
   dispose(nIdentifiers,Done);
   dispose(nType,Done);
end;

{ Attributes }

constructor AdjectiveExpressionObj.Init(const aPos:Position; aSort: AdjectiveSort);
begin
  nAdjectivePos:=aPos;
  nAdjectiveSort:=aSort;
end;

destructor AdjectiveExpressionObj.Done;
begin
end;

constructor AdjectiveObj.Init(const aPos:Position; aAdjectiveNr:integer; aArgs:PList);
begin
  inherited Init(aPos,wsAdjective);
  nAdjectiveSymbol:=aAdjectiveNr;
  nArgs:=aArgs;
end;

destructor AdjectiveObj.Done;
begin
  dispose(nArgs,Done);
end;

constructor NegatedAdjectiveObj.Init(const aPos:Position; aArg:AdjectiveExpressionPtr);
begin
  inherited Init(aPos,wsNegatedAdjective);
  nArg:=aArg;
end;

destructor NegatedAdjectiveObj.Done;
begin
  dispose(nArg,Done);
end;

{ Terms }

constructor SimpleTermObj.Init(const aPos:Position; aIdentNr:integer);
begin
   nTermPos:=aPos;
   nTermSort:=wsSimpleTerm;
   nIdent:=aIdentNr;
end;

constructor PlaceholderTermObj.Init(const aPos:Position; aLocusNr:integer);
begin
   nTermPos:=aPos;
   nTermSort:=wsPlaceholderTerm;
   nLocusNr:=aLocusNr;
end;

constructor NumeralTermObj.Init(const aPos:Position; aValue:integer);
begin
   nTermPos:=aPos;
   nTermSort:=wsNumeralTerm;
   nValue:=aValue;
end;

constructor InfixTermObj.Init(const aPos:Position; aFunctorNr:integer;
                              aLeftArgs,aRightArgs:PList);
begin
   nTermPos:=aPos;
   nTermSort:=wsInfixTerm;
   nFunctorSymbol:=aFunctorNr;
   nLeftArgs:=aLeftArgs;
   nRightArgs:=aRightArgs;
end;

destructor InfixTermObj.Done;
begin
   dispose(nLeftArgs,Done);
   dispose(nRightArgs,Done);
end;

constructor TermWithArgumentsObj.Init(const aPos:Position; aKind:TermSort; aArgs:PList);
begin
   nTermPos:=aPos;
   nTermSort:=aKind;
   nArgs:=aArgs;
end;

destructor TermWithArgumentsObj.Done;
begin
   dispose(nArgs,Done);
end;

constructor CircumfixTermObj.Init(const aPos:Position;
                           aLeftBracketNr,aRightBracketNr:integer; aArgs:PList);
begin
   inherited Init(aPos,wsCircumfixTerm,aArgs);
   nLeftBracketSymbol:=aLeftBracketNr;
   nRightBracketSymbol:=aRightBracketNr;
end;

destructor CircumfixTermObj.Done;
begin
   dispose(nArgs,Done);
end;

constructor PrivateFunctorTermObj.Init(const aPos:Position; aFunctorIdNr:integer;
                                    aArgs:PList);
begin
   inherited Init(aPos,wsPrivateFunctorTerm,aArgs);
   nFunctorIdent:=aFunctorIdNr;
end;

destructor PrivateFunctorTermObj.Done;
begin
   dispose(nArgs,Done);
end;

constructor OneArgumentTermObj.Init(const aPos:Position; aKind:TermSort; aArg:TermPtr);
begin
   nTermPos:=aPos;
   nTermSort:=aKind;
   nArg:=aArg;
end;

destructor OneArgumentTermObj.Done;
begin
   dispose(nArg,Done);
end;

constructor SelectorTermObj.Init(const aPos:Position; aSelectorNr:integer; aArg:TermPtr);
begin
   inherited Init(Apos,wsSelectorTerm,aArg);
   nSelectorSymbol:=aSelectorNr;
end;

destructor SelectorTermObj.Done;
begin
   dispose(nArg,Done);
end;

constructor InternalSelectorTermObj.Init(const aPos:Position; aSelectorNr:integer);
begin
   nTermPos:=aPos;
   nTermSort:=wsInternalSelectorTerm;
   nSelectorSymbol:=aSelectorNr;
end;

constructor AggregateTermObj.Init(const aPos:Position; aStructSymbol:integer;
                                 aArgs:PList);
begin
   inherited Init(aPos,wsAggregateTerm,aArgs);
   nStructSymbol:=aStructSymbol;
end;

destructor AggregateTermObj.Done;
begin
   dispose(nArgs,Done);
end;

constructor ForgetfulFunctorTermObj.Init(const aPos:Position; aStructSymbol:integer;
                                       aArg:TermPtr);
begin
   inherited Init(aPos,wsForgetfulFunctorTerm,aArg);
   nStructSymbol:=aStructSymbol;
end;

destructor ForgetfulFunctorTermObj.Done;
begin
   dispose(nArg,Done);
end;

constructor InternalForgetfulFunctorTermObj.Init(const aPos:Position; aStructSymbol:integer);
begin
   nTermPos:=aPos;
   nTermSort:= wsInternalForgetfulFunctorTerm;
   nStructSymbol:=aStructSymbol;
end;

constructor SimpleFraenkelTermObj.Init(const aPos:Position; aPostqual:PList; aSample:TermPtr);
begin
   nTermPos:=aPos;
   nTermSort:=wsSimpleFraenkelTerm;
   nPostqualification:=aPostqual;
   nSample:=aSample;
end;

destructor SimpleFraenkelTermObj.Done;
begin
   dispose(nSample,Done);
end;

constructor FraenkelTermObj.Init(const aPos:Position; aPostqual:PList; aSample:TermPtr;
                               aFormula:FormulaPtr);
begin
   nTermPos:=aPos;
   nTermSort:=wsFraenkelTerm;
   nPostqualification:=aPostqual;
   nSample:=aSample;
   nFormula:=aFormula;
end;

destructor FraenkelTermObj.Done;
begin
   dispose(nSample,Done);
   dispose(nPostqualification,Done);
   dispose(nFormula,Done);
end;

constructor QualifiedTermObj.Init(const aPos:Position; aSubject:TermPtr; aType:TypePtr);
begin
   nTermPos:=aPos;
   nTermSort:=wsQualificationTerm;
   nSubject:=aSubject;
   nQualification:=aType;
end;

destructor QualifiedTermObj.Done;
begin
   dispose(nSubject,Done);
   dispose(nQualification,Done);
end;

constructor ExactlyTermObj.Init(const aPos:Position; aSubject:TermPtr);
begin
   nTermPos:=aPos;
   nTermSort:=wsExactlyTerm;
   nSubject:=aSubject;
end;

destructor ExactlyTermObj.Done;
begin
   dispose(nSubject,Done);
end;

constructor ChoiceTermObj.Init(const aPos:Position; aType:TypePtr);
begin
   nTermPos:=aPos;
   nTermSort:=wsGlobalChoiceTerm;
   nChoiceType:=aType;
end;

destructor ChoiceTermObj.Done;
begin
   dispose(nChoiceType,Done);
end;

constructor ItTermObj.Init(const aPos:Position);
begin
   nTermPos:=aPos;
   nTermSort:=wsItTerm;
end;

constructor IncorrectTermObj.Init(const aPos:Position);
begin
   nTermPos:=aPos;
   nTermSort:=wsErrorTerm;
end;

{ Types }

constructor RadixTypeObj.Init(const aPos:Position; aKind:TypeSort; aArgs:PList);
begin
   nTypePos:=aPos;
   nTypeSort:=aKind;
   nArgs:=aArgs;
end;

destructor RadixTypeObj.Done;
begin
   dispose(nArgs,Done);
end;

constructor StandardTypeObj.Init(const aPos:Position; aModeSymbol:integer; aArgs:PList);
begin
   inherited Init(aPos,wsStandardType,aArgs);
   nModeSymbol:=aModeSymbol;
end;

destructor StandardTypeObj.Done;
begin
 inherited Done;
end;

constructor StructTypeObj.Init(const aPos:Position; aStructSymbol:integer; aArgs:PList);
begin
   inherited Init(aPos,wsStructureType,aArgs);
   nStructSymbol:=aStructSymbol;
end;

destructor StructTypeObj.Done;
begin
 inherited Done;
end;

constructor ClusteredTypeObj.Init(const aPos:Position; aCluster:PList; aType:TypePtr);
begin
   nTypePos:=aPos;
   nTypeSort:=wsClusteredType;
   nAdjectiveCluster:=aCluster;
   nType:=aType;
end;

destructor ClusteredTypeObj.Done;
begin
   dispose(nAdjectiveCluster,Done);
   dispose(nType,Done);
end;


constructor IncorrectTypeObj.Init(const aPos:Position);
begin
   nTypePos:=aPos;
   nTypeSort:=wsErrorType;
end;

{ Formulas }

constructor RightSideOfPredicativeFormulaObj.Init(const aPos:Position; aPredNr:integer; aRightArgs:PList);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsRightSideOfPredicativeFormula;
   nPredNr:=aPredNr;
   nRightArgs:=aRightArgs;
end;

destructor RightSideOfPredicativeFormulaObj.Done;
begin
   dispose(nRightArgs,Done);
end;

constructor PredicativeFormulaObj.Init(const aPos:Position; aPredNr:integer; aLeftArgs,aRightArgs:PList);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsPredicativeFormula;
   nPredNr:=aPredNr;
   nLeftArgs:=aLeftArgs;
   nRightArgs:=aRightArgs;
end;

destructor PredicativeFormulaObj.Done;
begin
   dispose(nLeftArgs,Done);
   dispose(nRightArgs,Done);
end;

constructor MultiPredicativeFormulaObj.Init(const aPos:Position; aScraps:PList);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsMultiPredicativeFormula;
   nScraps:=aScraps;
end;

destructor MultiPredicativeFormulaObj.Done;
begin
   dispose(nScraps,Done);
end;

constructor AttributiveFormulaObj.Init
   (const aPos:Position; aSubject:TermPtr; aAdjectives:PList);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsAttributiveFormula;
   nSubject:=aSubject;
   nAdjectives:=aAdjectives;
end;

destructor AttributiveFormulaObj.Done;
begin
   dispose(nSubject,Done);
   dispose(nAdjectives,Done);
end;

constructor PrivatePredicativeFormulaObj.Init(const aPos:Position; aPredIdNr:integer;
                                            aArgs:PList);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsPrivatePredicateFormula;
   nPredIdNr:=aPredIdNr;
   nArgs:=aArgs;
end;

destructor PrivatePredicativeFormulaObj.Done;
begin
   dispose(nArgs,Done);
end;

constructor QualifyingFormulaObj.Init(const aPos:Position; aSubject:TermPtr; aType:TypePtr);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsQualifyingFormula;
   nSubject:=aSubject;
   nType:=aType;
end;

destructor QualifyingFormulaObj.Done;
begin
   dispose(nSubject,Done);
   dispose(nType,Done);
end;

constructor NegativeFormulaObj.Init(const aPos:Position; aArg:FormulaPtr);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsNegatedFormula;
   nArg:=aArg;
end;

destructor NegativeFormulaObj.Done;
begin
   dispose(nArg,Done);
end;

constructor BinaryArgumentsFormula.Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
begin
   nFormulaPos:=aPos;
   nLeftArg:=aLeftArg;
   nRightArg:=aRightArg;
end;

destructor BinaryArgumentsFormula.Done;
begin
   dispose(nLeftArg,Done);
   dispose(nRightArg,Done);
end;

constructor ConjunctiveFormulaObj.Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
begin
   inherited Init(aPos,aLeftArg,aRightArg);
   nFormulaSort:=wsConjunctiveFormula;
end;

constructor DisjunctiveFormulaObj.Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
begin
   inherited Init(aPos,aLeftArg,aRightArg);
   nFormulaSort:=wsDisjunctiveFormula;
end;

constructor ConditionalFormulaObj.Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
begin
   inherited Init(aPos,aLeftArg,aRightArg);
   nFormulaSort:=wsConditionalFormula;
end;

constructor BiconditionalFormulaObj.Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
begin
   inherited Init(aPos,aLeftArg,aRightArg);
   nFormulaSort:=wsBiconditionalFormula;
end;

constructor FlexaryConjunctiveFormulaObj.Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
begin
   inherited Init(aPos,aLeftArg,aRightArg);
   nFormulaSort:=wsFlexaryConjunctiveFormula;
end;

constructor FlexaryDisjunctiveFormulaObj.Init(const aPos:Position; aLeftArg,aRightArg:FormulaPtr);
begin
   inherited Init(aPos,aLeftArg,aRightArg);
   nFormulaSort:=wsFlexaryDisjunctiveFormula;
end;

constructor QuantifiedFormulaObj.Init(const aPos:Position; aSegment:QualifiedSegmentPtr;
                                    aScope:FormulaPtr);
begin
   nFormulaPos:=aPos;
   nSegment:=aSegment;
   nScope:=aScope;
end;

destructor QuantifiedFormulaObj.Done;
begin
   dispose(nSegment,Done);
   dispose(nScope,Done);
end;

constructor UniversalFormulaObj.Init(const aPos:Position; aSegment:QualifiedSegmentPtr;
                                   aScope:FormulaPtr);
begin
   inherited Init(aPos,aSegment,aScope);
   nFormulaSort:=wsUniversalFormula;
end;

constructor ExistentialFormulaObj.Init(const aPos:Position; aSegment:QualifiedSegmentPtr;
                                     aScope:FormulaPtr);
begin
   inherited Init(aPos,aSegment,aScope);
   nFormulaSort:=wsExistentialFormula;
end;

constructor ContradictionFormulaObj.Init(const aPos:Position);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsContradiction;
end;

constructor ThesisFormulaObj.Init(const aPos:Position);
begin
   nFormulaPos:=aPos;
   nFormulaSort:=wsThesis;
end;

constructor IncorrectFormula.Init(const aPos:Position);
begin
   nFormulaPos:=aPos;
   nformulaSort:=wsErrorFormula;
end;

{-------------------------------------------------------------------------}

constructor WithinExprObj.Init(aExpKind:ExpKind);
begin
 setlength(nStackArr,50);
 nStackCnt:=0;
 nExpKind:=aExpKind;
end;

destructor WithinExprObj.Done;
begin
 inherited Done;
end;

function WithinExprObj.CreateExpressionsVariableLevel: biStackedPtr;
begin
 result:=new(biStackedPtr, Init);
end;

procedure WithinExprObj.Process_Adjective(aAttr:AdjectiveExpressionPtr);
begin
 case aAttr^.nAdjectiveSort of
  wsAdjective:
   begin
   Process_TermList( AdjectivePtr(aAttr)^.nArgs );
//    nAdjectiveSymbol;
   end;
  wsNegatedAdjective:
   Process_Adjective( NegatedAdjectivePtr(aAttr)^.nArg );
 end;
end;

procedure WithinExprObj.Process_AdjectiveList(aCluster: PList);
 var i: integer;
begin
 with aCluster^ do
  for i:=0 to Count-1 do
    Process_Adjective( Items^[i]);
end;

procedure WithinExprObj.Process_Variable( var aVar: VariablePtr);
begin
end;

procedure WithinExprObj.Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Process_Variable( aSegm^.nIdentifier);
end;

procedure WithinExprObj.Process_VariablesSegment( aSegm: QualifiedSegmentPtr);
 var i: integer;
begin
 Process_StartVariableSegment;
 case aSegm^.nSegmentSort of
 ikImplQualifiedSegm:
   Process_ImplicitlyQualifiedVariable(ImplicitlyQualifiedSegmentPtr(aSegm));
 ikExplQualifiedSegm:
  with ExplicitlyQualifiedSegmentPtr(aSegm)^ do
  begin
   for i:=0 to nIdentifiers.Count-1 do
     Process_Variable( VariablePtr(nIdentifiers.Items^[i]));
   Process_Type(nType);
  end;
 end;
 Process_FinishVariableSegment;
end;

procedure WithinExprObj.Process_StartVariableSegment;
begin
end;

procedure WithinExprObj.Process_FinishVariableSegment;
begin
end;

procedure WithinExprObj.Process_TermList ( aTrmList:PList );
 var i: integer;
begin
  for i:=0 to aTrmList^.Count-1 do
    Process_Term(TermPtr(aTrmList^.Items^[i]));
end;

procedure WithinExprObj.Process_Type ( aTyp: TypePtr);
begin
 with aTyp^ do
 begin
  case aTyp^.nTypeSort of
   wsStandardType:
    with StandardTypePtr(aTyp)^ do
    begin
//       nModeSymbol
      Process_TermList(nArgs);
    end;
   wsStructureType:
    with StructTypePtr(aTyp)^ do
    begin
//       nStructSymbol
     Process_TermList(nArgs);
    end;
   wsClusteredType:
    with ClusteredTypePtr(aTyp)^ do
    begin
     Process_AdjectiveList(nAdjectiveCluster);
     Process_Type(nType);
    end;
   wsErrorType: ;
  end;
 end;
end;

procedure WithinExprObj.Process_BinaryFormula ( aFrm:BinaryFormulaPtr );
begin
 Process_Formula(aFrm^.nLeftArg);
 Process_Formula(aFrm^.nRightArg);
end;

procedure WithinExprObj.Process_StartQuantifiedFormula(aFrm:QuantifiedFormulaPtr);
begin
end;

procedure WithinExprObj.Process_FinishQuantifiedFormula(aFrm:QuantifiedFormulaPtr);
begin
end;

procedure WithinExprObj.Process_QuantifiedFormula ( aFrm: QuantifiedFormulaPtr );
begin
  Process_VariablesSegment(aFrm^.nSegment);
  Process_Formula(aFrm^.nScope);
end;

procedure WithinExprObj.Process_Formula ( aFrm: FormulaPtr );
 var i: integer;
begin
 case aFrm^.nFormulaSort of
  wsNegatedFormula:
    Process_Formula(NegativeFormulaPtr(aFrm)^.nArg);
  wsConjunctiveFormula,wsDisjunctiveFormula,
  wsConditionalFormula,wsBiconditionalFormula,
  wsFlexaryConjunctiveFormula,wsFlexaryDisjunctiveFormula:
    Process_BinaryFormula(BinaryFormulaPtr(aFrm));
  wsRightSideOfPredicativeFormula:
   with RightSideOfPredicativeFormulaPtr(aFrm)^ do
   begin
//    nPredNr
     Process_TermList( nRightArgs);
   end;
  wsPredicativeFormula:
   with PredicativeFormulaPtr(aFrm)^ do
   begin
     Process_TermList( nLeftArgs);
//    nPredNr
     Process_TermList( nRightArgs);
   end;
  wsMultiPredicativeFormula:
   with MultiPredicativeFormulaPtr(aFrm)^ do
   begin
     for i := 0 to nScraps.Count - 1 do
      Process_Formula(nScraps.Items^[i]);
   end;
  wsPrivatePredicateFormula:
   with PrivatePredicativeFormulaPtr(aFrm)^ do
   begin
//    nPredIdNr
    Process_TermList( nArgs);
   end;
  wsAttributiveFormula:
   with AttributiveFormulaPtr(aFrm)^ do
   begin
    Process_Term(nSubject);
    Process_AdjectiveList(nAdjectives);
   end;
  wsQualifyingFormula:
   with QualifyingFormulaPtr(aFrm)^ do
   begin
    Process_Term(nSubject);
    Process_Type(nType);
   end;
  wsExistentialFormula, wsUniversalFormula:
   with QuantifiedFormulaPtr( aFrm)^ do
   begin
    inc(nStackCnt);
    if nStackCnt > length(nStackArr) then
     setlength(nStackArr,2*length(nStackArr));
    nStackArr[nStackCnt]:=CreateExpressionsVariableLevel;
    Process_StartQuantifiedFormula(QuantifiedFormulaPtr(aFrm));
    Process_QuantifiedFormula(QuantifiedFormulaPtr(aFrm));
    Process_FinishQuantifiedFormula(QuantifiedFormulaPtr(aFrm));
    dispose(nStackArr[nStackCnt],Done);
    dec(nStackCnt);
   end;
  wsContradiction: ;
  wsThesis: ;
  wsErrorFormula: ;
 end;
end;

procedure WithinExprObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
begin
end;

procedure WithinExprObj.Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr);
begin
end;

procedure WithinExprObj.Process_FinishFraenkelTerm(var aTrm:SimpleFraenkelTermPtr);
begin
end;

procedure WithinExprObj.Process_FraenkelTermsScope( var aFrm:FormulaPtr );
begin
  Process_Formula(aFrm);
end;

procedure WithinExprObj.Process_SimpleFraenkelTerm(var aTrm:SimpleFraenkelTermPtr);
 var i: integer;
begin
 with aTrm^ do
 begin
  for i := 0 to nPostqualification^.Count - 1 do
    Process_VariablesSegment(QualifiedSegmentPtr(nPostqualification^.Items^[i]));
  Process_Term(nSample);
 end;
end;

procedure WithinExprObj.Process_Term ( var aTrm: TermPtr );
begin
 case aTrm^.nTermSort of
  wsPlaceholderTerm: ;
//    PlaceholderTermPtr(aTrm)^.nLocusNr
  wsSimpleTerm:
   Process_SimpleTerm (SimpleTermPtr(aTrm));
  wsNumeralTerm: ;
//    NumeralTermPtr(aTrm)^.nValue
  wsInfixTerm:
   with InfixTermPtr(aTrm)^ do
   begin
     Process_TermList( nLeftArgs);
//    nFunctorSymbol
     Process_TermList( nRightArgs);
   end;
   wsCircumfixTerm:
    with CircumfixTermPtr(aTrm)^ do
    begin
//     nLeftBracketSymbol
     Process_TermList(nArgs);
//     nRightBracketSymbol
    end;
   wsPrivateFunctorTerm:
    with PrivateFunctorTermPtr(aTrm)^ do
    begin
//     nFunctorIdent
     Process_TermList(nArgs);
    end;
   wsAggregateTerm:
    with AggregateTermPtr(aTrm)^ do
    begin
//      nStructSymbol
      Process_TermList( nArgs);
    end;
   wsSelectorTerm:
    with SelectorTermPtr(aTrm)^ do
    begin
//     nSelectorSymbol
     Process_Term( nArg);
    end;
   wsInternalSelectorTerm: ;
//    InternalSelectorTermPtr(aTrm)^.nSelectorSymbol
   wsForgetfulFunctorTerm:
    with ForgetfulFunctorTermPtr(aTrm)^ do
    begin
//     nStructSymbol
     Process_Term( nArg);
    end;
   wsInternalForgetfulFunctorTerm: ;
//    InternalForgetfulFunctorTermPtr(aTrm)^.nStructSymbol
   wsSimpleFraenkelTerm,wsFraenkelTerm:
    with FraenkelTermPtr(aTrm)^ do
    begin
     inc(nStackCnt);
     if nStackCnt > length(nStackArr) then
      setlength(nStackArr,2*length(nStackArr));
     nStackArr[nStackCnt]:=CreateExpressionsVariableLevel;
     Process_StartFraenkelTerm(SimpleFraenkelTermPtr(aTrm));
     Process_SimpleFraenkelTerm(SimpleFraenkelTermPtr(aTrm));
     if aTrm^.nTermSort = wsFraenkelTerm then
      Process_FraenkelTermsScope(FraenkelTermPtr(aTrm)^.nFormula);
     Process_FinishFraenkelTerm(SimpleFraenkelTermPtr(aTrm));
     dispose(nStackArr[nStackCnt],Done);
     dec(nStackCnt);
    end;
   wsQualificationTerm:
    with QualifiedTermPtr(aTrm)^ do
    begin
      Process_Term(nSubject);
      Process_Type(nQualification);
    end;
   wsExactlyTerm:
     Process_Term(ExactlyTermPtr(aTrm)^.nSubject);
   wsGlobalChoiceTerm:
     Process_Type(ChoiceTermPtr(aTrm)^.nChoiceType);
   wsItTerm: ;
   wsErrorTerm: ;
  end;
end;

end.
