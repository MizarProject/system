(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit block_and_item;

interface

uses syntax, errhan, mobjects, mscanner,
     abstract_syntax, parseraddition, wsmarticle;

type

{----------------------------------------------------------------}

 biSubexpPtr = ^biSubexpObj;
 biSubexpObj =
  object(biStackedObj)
   constructor Init;
  end;

 biExpressionPtr = ^biExpressionObj;
 biExpressionObj =
  object(biStackedObj)
    nExpKind: ExpKind;
   constructor Init(fExpKind:ExpKind);
  end;

 biItemPtr = ^biItemObj;
 biItemObj =
  object(biStackedObj)
    nItemKind: ItemKind;
   constructor Init(fItemKind:ItemKind);
  end;

 biBlockPtr = ^biBlockObj;
 biBlockObj =
  object(biStackedObj)
    nBlockKind:BlockKind;
   constructor Init(fBlockKind:BlockKind);
  end;

 BlockAndItemPtr =  ^BlockAndItemObj;
 BlockAndItemObj =
  object(MObject)
    nTextProper: WSTextProperPtr;
    nDisplayInformationOnScreen: boolean;

    nBlockPtr: WSBlockPtr;
    nItemPtr: WSItemPtr;
    nStackArr: array of  biStackedPtr;
    nStackCnt: integer;

   constructor Init(aWSTextProper:WSTextProperPtr);
   destructor Done; virtual;

   procedure Process_Article; virtual;

   function CreateBlock(fBlockKind:BlockKind): biBlockPtr; virtual;
   function CreateItem(fItemKind: ItemKind): biItemPtr; virtual;
   procedure Process_StartBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_FinishBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_Block(aWSBlock:WSBlockPtr); virtual;
   procedure Process_Definition(aWSBlock:WSBlockPtr); virtual;
   procedure Process_CaseBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_Scheme(aWSBlock:WSBlockPtr); virtual;
   procedure Process_StartItem(aWSItem:WSItemPtr); virtual;
   procedure Process_FinishItem(aWSItem:WSItemPtr); virtual;
   procedure Process_Item(aWSItem:WSItemPtr); virtual;
   procedure Process_ItemsContent(aItemKind:ItemKind; const aItemPos:Position; aContent: PObject); virtual;
   procedure Process_PrivateReference(var aRef: LocalReferencePtr); virtual;
   procedure Process_LibraryReference(aRef: LibraryReferencePtr); virtual;
   procedure Process_References(aRefs: PList); virtual;
   procedure Process_StraightforwardJustification(aInf: StraightforwardJustificationPtr); virtual;
   procedure Process_SchemeJustification(aInf: SchemeJustificationPtr); virtual;
   procedure Process_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_IterartiveEquality(aRStm:IterativeEqualityPtr); virtual;

  end;

 ProcessingArticlePtr =  ^ProcessingArticleObj;
 ProcessingArticleObj =
  object(MObject)
    nDisplayInformationOnScreen: boolean;

    nStackArr: array of  biStackedPtr;
    nStackCnt: integer;

    nTextProper: WSTextProperPtr;
    nBlockPtr: biBlockPtr;

   constructor Init(aWSTextProper:WSTextProperPtr);
   destructor Done; virtual;
   function CreateBlock(fBlockKind:BlockKind): biBlockPtr; virtual;
   function CreateItem(fItemKind: ItemKind): biItemPtr; virtual;
   function CreateExpressionsVariableLevel: biSubexpPtr; virtual;  //??

   function CreateExpression(fExpKind:ExpKind): biExpressionPtr; virtual;  //??

   procedure Process_Article; virtual;

   procedure Process_StartBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_FinishBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_Block(aWSBlock:WSBlockPtr); virtual;
   procedure Process_Definition(aWSBlock:WSBlockPtr); virtual;
   procedure Process_CaseBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_Scheme(aWSBlock:WSBlockPtr); virtual;
   procedure Process_SchemePremises(aSchema:SchemePtr); virtual;

   procedure Process_ImplicitlyRedefinedVariable(aRes:TypeChangePtr); virtual;
   procedure Process_RedefinedVariable(aRes:TypeChangePtr); virtual;
   procedure Process_ExemplifyingVariable(aExampl:ExamplePtr); virtual;
   procedure Process_ImplicitExamplification(aExampl:ExamplePtr); virtual;
   procedure Process_FieldInStructureDefinition(aField:FieldSymbolPtr); virtual;
   procedure Process_FieldsInStructureDefinition(aStruct:StructureDefinitionPtr); virtual;
   procedure Process_ConstantDefinition(aDef:ConstantDefinitionPtr); virtual;

   procedure Process_Section; virtual;
   procedure Process_Pragma(aPragma: PragmaPtr); virtual;

   procedure Process_Theorem(aCStm:CompactStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_StartItem(aWSItem:WSItemPtr); virtual;
   procedure Process_FinishItem(aWSItem:WSItemPtr); virtual;
   procedure Process_Item(aWSItem:WSItemPtr); virtual;

   procedure Process_Adjective(aAttr:AdjectiveExpressionPtr ); virtual;
   procedure Process_AdjectiveList( aCluster: PList ); virtual;
   procedure Process_Variable( var aVar: VariablePtr); virtual;
   procedure Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Process_VariablesSegment( var aSegm: QualifiedSegmentPtr); virtual;
   procedure Process_FixedVariable( var aVar: VariablePtr); virtual;
   procedure Process_ImplicitlyQualifiedFixedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Process_FixedVariablesSegment( var aSegm: QualifiedSegmentPtr); virtual;
   procedure Process_ChoiceVariable( var aVar: VariablePtr); virtual;
   procedure Process_ImplicitlyQualifiedChoiceVariable( var aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Process_ChoiceVariablesSegment( var aSegm: QualifiedSegmentPtr); virtual;
   procedure Process_StartVariableSegment; virtual;
   procedure Process_FinishVariableSegment; virtual;
   procedure Process_FinishOneVariableSegment; virtual;
   procedure Process_StartReconsideringVariableSegment; virtual;
   procedure Process_FinishReconsideringVariableSegment; virtual;

   procedure Process_StartReservedVariables( aResVars: ReservationSegmentPtr); virtual;
   procedure Process_FinishReservedVariables(var aResVars: ReservationSegmentPtr); virtual;
   procedure Process_ReservedVariable( var aVar: VariablePtr); virtual;
   procedure Process_StartPrivateFunctor; virtual;
   procedure Process_FinishPrivateFunctor; virtual;
   procedure Process_LocalFunctorVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;
   procedure Process_StartPrivatePredicate; virtual;
   procedure Process_FinishPrivatePredicate; virtual;
   procedure Process_LocalPredicateVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;

   procedure Process_StartSchemeSegment(aSgm:SchemeSegmentPtr); virtual;
   procedure Process_FinishSchemeSegment(aSgm:SchemeSegmentPtr); virtual;
   procedure Process_SchemePredicateVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;
   procedure Process_SchemeFunctorVariable( var aVar: VariablePtr; aArgNbr: integer); virtual;
   procedure Process_SchemeConclusion(var aFrm:FormulaPtr); virtual;

   procedure Process_Type ( aTyp: TypePtr ); virtual;
   procedure Process_BinaryFormula ( aFrm:BinaryFormulaPtr ); virtual;
   procedure Process_PrivatePredicateFormula ( var aFrm:PrivatePredicativeFormulaPtr ); virtual;
   procedure Process_StartQuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
   procedure Process_FinishQuantifiedFormula(aFrm:QuantifiedFormulaPtr); virtual;
   procedure Process_Formula ( var aFrm:FormulaPtr ); virtual;
   procedure Process_TermList ( aTrmList:PList ); virtual;
   procedure Process_SimpleTerm ( var aTrm: SimpleTermPtr ); virtual;
   procedure Process_PrivateFunctorTerm ( var aTrm: PrivateFunctorTermPtr ); virtual;
   procedure Process_InternalSelectorTerm( var aTrm: InternalSelectorTermPtr ); virtual;
   procedure Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FinishFraenkelTerm(aTrm:SimpleFraenkelTermPtr); virtual;
   procedure Process_FraenkelTermsScope ( var aFrm:FormulaPtr ); virtual;
   procedure Process_Term ( var aTrm: TermPtr ); virtual;

   procedure Process_TypeWithFVScope ( var aTyp: TypePtr ); virtual;
   procedure Process_TermWithFVScope ( var aTrm: TermPtr ); virtual;
   procedure Process_FormulaWithFVScope ( var aFrm:FormulaPtr ); virtual;
   procedure Process_TypeList ( aTypeList: PList ); virtual;


   procedure Process_Label(var aLab:LabelPtr); virtual;
   procedure Process_DefiniensLabel(var aLab:LabelPtr); virtual;

   procedure Process_PrivateReference(var aRef: LocalReferencePtr); virtual;
   procedure Process_LibraryReference(var aRef: LibraryReferencePtr); virtual;
   procedure Process_References(aRefs: PList); virtual;
   procedure Process_StraightforwardJustification(aInf: StraightforwardJustificationPtr); virtual;
   procedure Process_SchemeJustification(var aInf: SchemeJustificationPtr); virtual;
   procedure Process_Justification(var aInf: JustificationPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_SchemeHead(var aSch:SchemePtr); virtual;
   procedure Process_ChoiceStatement(aChoice: ChoiceStatementPtr); virtual;
   procedure Process_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_Proposition(aProp:PropositionPtr); virtual;
   procedure Process_ReservationSegment(var aRes: ReservationSegmentPtr); virtual;

   procedure Process_Conditions(aCond: PList); virtual;
   procedure Process_AssumptionConditions(aCond: AssumptionPtr); virtual;

   procedure Process_Pattern(aPattern: PatternPtr); virtual;
   procedure Process_Locus( var aLocus: LocusPtr); virtual;
   procedure Process_Loci( aLoci: PList); virtual;
   procedure Process_LociEqualities(aEqLociList:PList); virtual;
   procedure Process_Definiens(aDef:DefiniensPtr); virtual;
   procedure Process_DefiniensFormula(var aFrm:FormulaPtr); virtual;
  end;

//procedure Process_WSMizArticle(aWSTextProper:wsTextProperPtr; aFileName:string);


implementation

uses mizenv, mconsole, parser, _formats, xml_dict, xml_parser
{$IFDEF MDEBUG} ,info {$ENDIF};

{----------------------------------------------------------------}
constructor biSubexpObj.Init;
begin
end;

constructor biExpressionObj.Init(fExpKind:ExpKind);
begin
 nExpKind:=fExpKind
end;

constructor biItemObj.Init(fItemKind:ItemKind);
begin
 nItemKind:=fItemKind
end;

constructor biBlockObj.Init(fBlockKind:BlockKind);
begin
 nBlockKind:=fBlockKind
end;

{-------------------------------------------------------------------------}

constructor BlockAndItemObj.Init(aWSTextProper:WSTextProperPtr);
begin
 nTextProper:=aWSTextProper;
 nDisplayInformationOnScreen:=false;
end;

destructor BlockAndItemObj.Done;
begin
 dispose(nTextProper,Done);
 inherited Done;
end;

procedure BlockAndItemObj.Process_Article;
 var i: integer;
begin
 setlength(nStackArr,50);
 nStackCnt:=0;
 nStackArr[0]:=CreateBlock(blMain);
// nBlockPtr:=biBlockPtr(nStackArr[0]);
 nBlockPtr:=nil;
 nItemPtr:=nil;
 with nTextProper^ do
 begin
  for i := 0 to nItems.Count - 1 do
    Process_Item(nItems.Items^[i]);
 end;
 MizAssert(2841,nStackCnt=0);
 dispose(nStackArr[nStackCnt],Done);
end;

function BlockAndItemObj.CreateBlock(fBlockKind:BlockKind): biBlockPtr;
begin
 result:=new(biBlockPtr,Init(fBlockKind));
end;

function BlockAndItemObj.CreateItem(fItemKind:ItemKind): biItemPtr;
begin
 result:=new(biItemPtr, Init(fItemKind));
end;

procedure BlockAndItemObj.Process_StartBlock(aWSBlock:WSBlockPtr);
begin
end;

procedure BlockAndItemObj.Process_FinishBlock(aWSBlock:WSBlockPtr);
begin
end;

procedure BlockAndItemObj.Process_Block(aWSBlock:WSBlockPtr);
 var i: integer;
     lBlockPtr: WSBlockPtr;
     lItemPtr: WSItemPtr;
begin
 if aWSBlock <> nil then
  with aWSBlock^ do
   begin
    CurPos:=nBlockPos;
    if nDisplayInformationOnScreen then
      DisplayLine(CurPos.Line,ErrorNbr);
    lItemPtr:=nItemPtr;
    lBlockPtr:=nBlockPtr;
    nBlockPtr:=aWSBlock;
    inc(nStackCnt);
    if nStackCnt >= length(nStackArr) then
      setlength(nStackArr,2*length(nStackArr)+1);
    nStackArr[nStackCnt]:=CreateBlock(nBlockKind);
    Process_StartBlock(aWSBlock);
    for i := 0 to nItems.Count - 1 do
      Process_Item(nItems.Items^[i]);
    Process_FinishBlock(aWSBlock);
    if nDisplayInformationOnScreen then
      DisplayLine(nBlockEndPos.Line,ErrorNbr);
    dispose(nStackArr[nStackCnt],Done);
    dec(nStackCnt);
    nItemPtr:=lItemPtr;
    nBlockPtr:=lBlockPtr;
   end;
end;

procedure BlockAndItemObj.Process_StartItem(aWSItem:WSItemPtr);
begin
end;

procedure BlockAndItemObj.Process_FinishItem(aWSItem:WSItemPtr);
begin
end;

procedure BlockAndItemObj.Process_ItemsContent(aItemKind:ItemKind; const aItemPos:Position; aContent: PObject);
begin
 case aItemKind of
  itDefinition: ;
  itSchemeBlock: ;
  itSchemeHead:
   with SchemePtr(aContent)^ do
   begin
   end;
  itTheorem:
   with CompactStatementPtr(aContent)^ do
   begin
   end;
  itAxiom:
   begin
   end;
  itReservation:
   with ReservationSegmentPtr(aContent)^ do
   begin
   end;
  itRegularStatement:
   with RegularStatementPtr(aContent)^ do
   begin
   end;
  itChoice:
   with ChoiceStatementPtr(aContent)^ do
   begin
   end;
  itReconsider:
   with TypeChangingStatementPtr(aContent)^ do
   begin
   end;
  itPrivFuncDefinition:
   with PrivateFunctorDefinitionPtr(aContent)^ do
   begin
   end;
  itPrivPredDefinition:
   with PrivatePredicateDefinitionPtr(aContent)^ do
   begin
   end;
  itConstantDefinition:
   with ConstantDefinitionPtr(aContent)^ do
   begin
   end;
  itLociDeclaration,
  itGeneralization:
   with QualifiedSegmentPtr(aContent)^ do
   begin
   end;
  itAssumption:
   with AssumptionPtr(aContent)^ do
   begin
   end;
  itExistentialAssumption:
   with ExistentialAssumptionPtr(aContent)^ do
   begin
   end;
  itExemplification:
   with ExamplePtr(aContent)^ do
   begin
   end;
  itPerCases:
   with JustificationPtr(aContent)^ do
   begin
   end;
  itConclusion:
   with RegularStatementPtr(aContent)^ do
   begin
   end;
  itCaseBlock: ;
  itCaseHead,
  itSupposeHead:
   with AssumptionPtr(aContent)^ do
   begin
   end;
  itCorrCond:
   with CorrectnessConditionPtr(aContent)^ do
   begin
   end;
  itCorrectness:
   with CorrectnessPtr(aContent)^ do
   begin
   end;
  itProperty:
   begin
    with PropertyPtr(aContent)^ do
    begin
    end;
   end;
  itDefMode:
   with ModeDefinitionPtr(aContent)^ do
   begin
   end;
  itDefAttr:
   with AttributeDefinitionPtr(aContent)^ do
   begin
   end;
  itDefPred:
   with PredicateDefinitionPtr(aContent)^ do
   begin
   end;
  itDefFunc:
   with FunctorDefinitionPtr(aContent)^ do
   begin
   end;
  itDefStruct:
   with StructureDefinitionPtr(aContent)^ do
   begin
   end;
  itPredSynonym,
  itFuncNotation, itModeNotation,
  itAttrSynonym:
   with NotationDeclarationPtr(aContent)^ do
   begin
   end;
  itPredAntonym,itAttrAntonym:
   with NotationDeclarationPtr(aContent)^ do
   begin
   end;
  itCluster:
   with ClusterPtr(aContent)^ do
   begin
   end;
  itIdentify:
   with IdentifyRegistrationPtr(aContent)^ do
   begin
   end;
  itReduction:
   with ReduceRegistrationPtr(aContent)^ do
    begin
    end;
  itPropertyRegistration:
   with PropertyRegistrationPtr(aContent)^ do
   begin
   end;
  itSection: ;
  itPragma:;
  itIncorrItem:;
  end;
end;

procedure BlockAndItemObj.Process_PrivateReference(var aRef: LocalReferencePtr);
begin
//     LocalReferencePtr(Refs^.Items^[i])^.nLabId
end;

procedure BlockAndItemObj.Process_LibraryReference(aRef: LibraryReferencePtr);
begin
  with aRef^ do
  case nRefSort of
   TheoremReference:
    begin
//     TheoremReferencePtr(aRefs^.Items^[i])^.nArticleNr
//     TheoremReferencePtr(aRefs^.Items^[i])^.nTheoNr
    end;
   DefinitionReference:
    begin
//     DefinitionReferencePtr(aRefs^.Items^[i])^.nArticleNr
//     DefinitionReferencePtr(aRefs^.Items^[i])^.nTheoNr
    end;
  end;
end;

procedure BlockAndItemObj.Process_References(aRefs: PList);
 var i: integer;
begin
 for i:= 0 to aRefs^.Count-1 do
  with ReferencePtr(aRefs^.Items^[i])^ do
  case nRefSort of
   LocalReference:
     Process_PrivateReference(LocalReferencePtr(aRefs^.Items^[i]));
   TheoremReference, DefinitionReference:
     Process_LibraryReference(LibraryReferencePtr(aRefs^.Items^[i]));
  end;
end;

procedure BlockAndItemObj.Process_StraightforwardJustification(aInf: StraightforwardJustificationPtr);
begin
 Process_References(aInf^.nReferences);
end;

procedure BlockAndItemObj.Process_SchemeJustification(aInf: SchemeJustificationPtr);
begin
 Process_References(aInf^.nReferences);
end;

procedure BlockAndItemObj.Process_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr);
begin
  case aInf^.nInfSort of
   infStraightforwardJustification:
    Process_StraightforwardJustification(StraightforwardJustificationPtr(aInf));
   infSchemeJustification:
    Process_SchemeJustification(SchemeJustificationPtr(aInf));
   infProof:
    Process_Block(aBlock);
   infError: ;
  end;
end;

procedure BlockAndItemObj.Process_IterartiveEquality(aRStm:IterativeEqualityPtr);
 var i: integer;
begin
 with IterativeEqualityPtr(aRSTm)^ do
  for i := 0 to nIterSteps^.Count - 1 do
   with IterativeStepPtr(nIterSteps^.Items^[i])^ do
   begin
//    nTerm
    Process_Justification(JustificationPtr(nJustification),nil);
   end;
end;


procedure BlockAndItemObj.Process_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr);
begin
  with aRStm^ do
   case nStatementSort of
   stDiffuseStatement:
     Process_Block(aBlock);
   stCompactStatement:
    Process_Justification(JustificationPtr(CompactStatementPtr(aRStm)^.nJustification),aBlock);
   stIterativeEquality:
    begin
     Process_Justification(JustificationPtr(CompactStatementPtr(aRStm)^.nJustification),aBlock);
     Process_IterartiveEquality(IterativeEqualityPtr(aRSTm));
    end;
   end;
end;

procedure BlockAndItemObj.Process_Definition(aWSBlock:WSBlockPtr);
begin
  Process_Block(aWSBlock);
end;

procedure BlockAndItemObj.Process_CaseBlock(aWSBlock:WSBlockPtr);
begin
  Process_Block(aWSBlock);
end;

procedure BlockAndItemObj.Process_Scheme(aWSBlock:WSBlockPtr);
begin
  Process_Block(aWSBlock);
end;

procedure BlockAndItemObj.Process_Item(aWSItem:WSItemPtr);
 var i,j,lIndent: integer;
     lBlockPtr: WSBlockPtr;
     lItemPtr: WSItemPtr;
begin
 with aWSItem^ do
 begin
  CurPos:=nItemPos;
  if nDisplayInformationOnScreen then
    DisplayLine(CurPos.Line,ErrorNbr);
  lItemPtr:=nItemPtr;
  lBlockPtr:=nBlockPtr;
  nItemPtr:=aWSItem;
  Process_StartItem(aWSItem);
  Process_ItemsContent(nItemKind,nItemPos,nContent);
  case aWSItem^.nItemKind of
    itDefinition:
      Process_Definition(nBlock);
    itSchemeBlock:
      Process_Scheme(nBlock);
    itSchemeHead:;
    itTheorem:
      Process_Justification(JustificationPtr(CompactStatementPtr(nContent)^.nJustification),nBlock);
    itAxiom:;
    itReservation: ;
    itRegularStatement:
      Process_RegularStatement(RegularStatementPtr(nContent),nBlock);
    itChoice:
      Process_Justification(JustificationPtr(ChoiceStatementPtr(nContent)^.nJustification),nil);
    itReconsider:
      Process_Justification(JustificationPtr(TypeChangingStatementPtr(nContent)^.nJustification),nil);
    itPrivFuncDefinition: ;
    itPrivPredDefinition: ;
    itConstantDefinition: ;
    itLociDeclaration,
    itGeneralization: ;
    itAssumption: ;
    itExistentialAssumption: ;
    itExemplification: ;
    itPerCases:
      Process_Justification(JustificationPtr(nContent),nil);
    itConclusion:
      Process_RegularStatement(RegularStatementPtr(nContent),nBlock);
    itCaseBlock:
      Process_CaseBlock(nBlock);
    itCaseHead,
    itSupposeHead: ;
    itCorrCond:
     Process_Justification(CorrectnessConditionPtr(nContent)^.nJustification,nBlock);
    itCorrectness:
      Process_Justification(CorrectnessPtr(nContent)^.nJustification,nBlock);
    itProperty:
      Process_Justification(PropertyPtr(nContent)^.nJustification,nBlock);
    itDefMode: ;
    itDefAttr: ;
    itDefPred: ;
    itDefFunc: ;
    itDefStruct: ;
    itPredSynonym,
    itFuncNotation, itModeNotation,
    itAttrSynonym: ;
    itPredAntonym,itAttrAntonym: ;
    itCluster: ;
    itIdentify: ;
    itReduction:;
    itPropertyRegistration:
     case PropertyRegistrationPtr(nContent)^.nPropertySort of
     sySethood:
      Process_Justification(SethoodRegistrationPtr(nContent)^.nJustification,nBlock);
     end;
    itSection: ;
    itPragma:;
    itIncorrItem:;
  end;
  CurPos:=nItemEndPos;
  Process_FinishItem(aWSItem);
  nItemPtr:=lItemPtr;
  nBlockPtr:=lBlockPtr;
 end;
end;

{-------------------------------------------------------------------------}

constructor ProcessingArticleObj.Init(aWSTextProper:WSTextProperPtr);
begin
 nTextProper:=aWSTextProper;
 nDisplayInformationOnScreen:=false;
end;

destructor ProcessingArticleObj.Done;
begin
 dispose(nTextProper,Done);
 inherited Done;
end;

procedure ProcessingArticleObj.Process_Adjective(aAttr:AdjectiveExpressionPtr);
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

procedure ProcessingArticleObj.Process_AdjectiveList(aCluster: PList);
 var i: integer;
begin
 with aCluster^ do
  for i:=0 to Count-1 do
   Process_Adjective( Items^[i]);
end;

procedure ProcessingArticleObj.Process_Variable( var aVar: VariablePtr);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_ImplicitlyQualifiedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Process_Variable( aSegm^.nIdentifier);
end;

procedure ProcessingArticleObj.Process_FixedVariable( var aVar: VariablePtr);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_ImplicitlyQualifiedFixedVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Process_FixedVariable( aSegm^.nIdentifier);
end;

procedure ProcessingArticleObj.Process_ChoiceVariable( var aVar: VariablePtr);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_ImplicitlyQualifiedChoiceVariable( var aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Process_ChoiceVariable( aSegm^.nIdentifier);
end;

procedure ProcessingArticleObj.Process_StartReservedVariables( aResVars: ReservationSegmentPtr);
begin
end;

procedure ProcessingArticleObj.Process_FinishReservedVariables(var aResVars: ReservationSegmentPtr);
begin
end;

procedure ProcessingArticleObj.Process_ReservedVariable( var aVar: VariablePtr);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_VariablesSegment( var aSegm: QualifiedSegmentPtr);
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

procedure ProcessingArticleObj.Process_FixedVariablesSegment( var aSegm: QualifiedSegmentPtr);
 var i: integer;
begin
 Process_StartVariableSegment;
 case aSegm^.nSegmentSort of
 ikImplQualifiedSegm:
   Process_ImplicitlyQualifiedFixedVariable( ImplicitlyQualifiedSegmentPtr(aSegm));
 ikExplQualifiedSegm:
  with ExplicitlyQualifiedSegmentPtr(aSegm)^ do
  begin
   for i:=0 to nIdentifiers.Count-1 do
     Process_FixedVariable( VariablePtr(nIdentifiers.Items^[i]));
   Process_TypeWithFVScope(nType);
  end;
 end;
 Process_FinishVariableSegment;
end;

procedure ProcessingArticleObj.Process_ChoiceVariablesSegment( var aSegm: QualifiedSegmentPtr);
 var i: integer;
begin
 Process_StartVariableSegment;
 case aSegm^.nSegmentSort of
 ikImplQualifiedSegm:
   Process_ImplicitlyQualifiedChoiceVariable( ImplicitlyQualifiedSegmentPtr(aSegm));
 ikExplQualifiedSegm:
  with ExplicitlyQualifiedSegmentPtr(aSegm)^ do
  begin
   for i:=0 to nIdentifiers.Count-1 do
     Process_ChoiceVariable( VariablePtr(nIdentifiers.Items^[i]));
   Process_TypeWithFVScope(nType);
  end;
 end;
 Process_FinishVariableSegment;
end;

procedure ProcessingArticleObj.Process_StartVariableSegment;
begin
end;

procedure ProcessingArticleObj.Process_FinishVariableSegment;
begin
end;

procedure ProcessingArticleObj.Process_FinishOneVariableSegment;
begin
end;

procedure ProcessingArticleObj.Process_StartReconsideringVariableSegment;
begin
end;

procedure ProcessingArticleObj.Process_FinishReconsideringVariableSegment;
begin
end;

procedure ProcessingArticleObj.Process_StartPrivateFunctor;
begin
end;

procedure ProcessingArticleObj.Process_FinishPrivateFunctor;
begin
end;

procedure ProcessingArticleObj.Process_LocalFunctorVariable( var aVar: VariablePtr; aArgNbr: integer);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_StartPrivatePredicate;
begin

end;

procedure ProcessingArticleObj.Process_FinishPrivatePredicate;
begin

end;

procedure ProcessingArticleObj.Process_LocalPredicateVariable( var aVar: VariablePtr; aArgNbr: integer);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_StartSchemeSegment(aSgm:SchemeSegmentPtr);
begin

end;

procedure ProcessingArticleObj.Process_FinishSchemeSegment(aSgm:SchemeSegmentPtr);
begin

end;

procedure ProcessingArticleObj.Process_SchemePredicateVariable( var aVar: VariablePtr; aArgNbr: integer);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_SchemeFunctorVariable( var aVar: VariablePtr; aArgNbr: integer);
begin
 with aVar ^ do
 begin
//  nIdent;
 end;
end;

procedure ProcessingArticleObj.Process_SchemeConclusion( var aFrm:FormulaPtr );
begin
  Process_FormulaWithFVScope(aFrm);
end;

procedure ProcessingArticleObj.Process_TermList ( aTrmList:PList );
 var i: integer;
begin
  for i:=0 to aTrmList^.Count-1 do
    Process_Term(TermPtr(aTrmList^.Items^[i]));
end;

procedure ProcessingArticleObj.Process_Type ( aTyp: TypePtr);
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
   wsErrorType:
    begin
//     Out_XElWithPos(TypeName[wsErrorType],nTypePos);
    end;
  end;
 end;
end;

procedure ProcessingArticleObj.Process_BinaryFormula ( aFrm:BinaryFormulaPtr );
begin
 Process_Formula(aFrm^.nLeftArg);
 Process_Formula(aFrm^.nRightArg);
end;

procedure ProcessingArticleObj.Process_PrivatePredicateFormula ( var aFrm:PrivatePredicativeFormulaPtr );
begin
   with PrivatePredicativeFormulaPtr(aFrm)^ do
   begin
//    nPredIdNr
    Process_TermList( nArgs);
   end;
end;

procedure ProcessingArticleObj.Process_StartQuantifiedFormula(aFrm:QuantifiedFormulaPtr);
begin

end;

procedure ProcessingArticleObj.Process_FinishQuantifiedFormula(aFrm:QuantifiedFormulaPtr);
begin

end;

procedure ProcessingArticleObj.Process_Formula ( var aFrm: FormulaPtr );
 var i: integer;
begin
 case aFrm^.nFormulaSort of
  wsNegatedFormula:
    Process_Formula(NegativeFormulaPtr(aFrm)^.nArg);
  wsConjunctiveFormula,wsDisjunctiveFormula,
  wsConditionalFormula,wsBiconditionalFormula,
  wsFlexaryConjunctiveFormula,wsFlexaryDisjunctiveFormula:
    Process_BinaryFormula(BinaryFormulaPtr(aFrm));
  wsPredicativeFormula:
   with PredicativeFormulaPtr(aFrm)^ do
   begin
     Process_TermList( nLeftArgs);
//    nPredNr]
     Process_TermList( nRightArgs);
   end;
  wsRightSideOfPredicativeFormula:
   with RightSideOfPredicativeFormulaPtr(aFrm)^ do
   begin
//    nPredNr
     Process_TermList( nRightArgs);
   end;
  wsMultiPredicativeFormula:
   with MultiPredicativeFormulaPtr(aFrm)^ do
   begin
     for i := 0 to nScraps.Count - 1 do
      Process_Formula(FormulaPtr(nScraps.Items^[i]));
   end;
  wsPrivatePredicateFormula:
   Process_PrivatePredicateFormula (PrivatePredicativeFormulaPtr(aFrm));
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
    if nStackCnt >= length(nStackArr) then
     setlength(nStackArr,2*length(nStackArr));
    nStackArr[nStackCnt]:=CreateExpressionsVariableLevel;
    Process_StartQuantifiedFormula(QuantifiedFormulaPtr(aFrm));
    Process_VariablesSegment(QuantifiedFormulaPtr(aFrm)^.nSegment);
    Process_Formula(QuantifiedFormulaPtr(aFrm)^.nScope);
    Process_FinishQuantifiedFormula(QuantifiedFormulaPtr(aFrm));
    dispose(nStackArr[nStackCnt],Done);
    dec(nStackCnt);
   end;
  wsContradiction:
    begin
    end;
  wsThesis:
    begin
    end;
  wsErrorFormula:
    begin
    end;
 end;
end;

procedure ProcessingArticleObj.Process_SimpleTerm ( var aTrm: SimpleTermPtr );
begin
//    SimpleTermPtr(aTrm)^.nIdent
end;

procedure ProcessingArticleObj.Process_PrivateFunctorTerm ( var aTrm: PrivateFunctorTermPtr );
begin
  with aTrm^ do
   begin
//     nFunctorIdent
    Process_TermList(nArgs);
   end;
end;

procedure ProcessingArticleObj.Process_InternalSelectorTerm( var aTrm: InternalSelectorTermPtr );
begin
  with aTrm^ do
   begin
//     nSelectorSymbol
   end;
end;

procedure ProcessingArticleObj.Process_StartFraenkelTerm(aTrm:SimpleFraenkelTermPtr);
begin

end;

procedure ProcessingArticleObj.Process_FinishFraenkelTerm(aTrm:SimpleFraenkelTermPtr);
begin

end;

procedure ProcessingArticleObj.Process_FormulaWithFVScope ( var aFrm:FormulaPtr );
begin
  Process_Formula(aFrm);
end;

procedure ProcessingArticleObj.Process_Term ( var aTrm: TermPtr );
 var i,j: integer;
begin
 case aTrm^.nTermSort of
  wsPlaceholderTerm:
   begin
//    Process_Number(PlaceholderTermPtr(aTrm)^.nLocusNr);
   end;
  wsSimpleTerm:
   Process_SimpleTerm (SimpleTermPtr(aTrm));
  wsNumeralTerm:
   begin
//    Process_Number(NumeralTermPtr(aTrm)^.nValue);
   end;
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
    Process_PrivateFunctorTerm (PrivateFunctorTermPtr(aTrm));
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
   wsInternalSelectorTerm:
    Process_InternalSelectorTerm(InternalSelectorTermPtr(aTrm));
   wsForgetfulFunctorTerm:
    with ForgetfulFunctorTermPtr(aTrm)^ do
    begin
//     nStructSymbol
     Process_Term( nArg);
    end;
   wsInternalForgetfulFunctorTerm:
    with InternalForgetfulFunctorTermPtr(aTrm)^ do
    begin
//      nStructSymbol
    end;
   wsFraenkelTerm:
    with FraenkelTermPtr(aTrm)^ do
    begin
     inc(nStackCnt);
     if nStackCnt >= length(nStackArr) then
      setlength(nStackArr,2*length(nStackArr));
     nStackArr[nStackCnt]:=CreateExpressionsVariableLevel;
     Process_StartFraenkelTerm(FraenkelTermPtr(aTrm));
     for i := 0 to nPostqualification^.Count - 1 do
       Process_VariablesSegment(QualifiedSegmentPtr(nPostqualification^.Items^[i]));
     Process_Term(nSample);
     Process_FraenkelTermsScope(nFormula);
     Process_FinishFraenkelTerm(FraenkelTermPtr(aTrm));
     dispose(nStackArr[nStackCnt],Done);
     dec(nStackCnt);
    end;
   wsSimpleFraenkelTerm:
    with SimpleFraenkelTermPtr(aTrm)^ do
    begin
     inc(nStackCnt);
     if nStackCnt >= length(nStackArr) then
      setlength(nStackArr,2*length(nStackArr));
     nStackArr[nStackCnt]:=CreateExpressionsVariableLevel;
     Process_StartFraenkelTerm(FraenkelTermPtr(aTrm));
     for i := 0 to nPostqualification^.Count - 1 do
       Process_VariablesSegment(QualifiedSegmentPtr(nPostqualification^.Items^[i]));
     Process_Term(nSample);
     Process_FinishFraenkelTerm(FraenkelTermPtr(aTrm));
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
    with ExactlyTermPtr(aTrm)^ do
     Process_Term(nSubject);
   wsGlobalChoiceTerm:
     Process_Type(ChoiceTermPtr(aTrm)^.nChoiceType);
   wsItTerm:
    begin
    end;
   wsErrorTerm:
//   Out_XEl1( TermName[wsErrorTerm]);
  end;
end;

procedure ProcessingArticleObj.Process_TypeWithFVScope ( var aTyp: TypePtr );
begin
  Process_Type(aTyp);
end;

procedure ProcessingArticleObj.Process_TermWithFVScope ( var aTrm: TermPtr );
begin
  Process_Term(aTrm);
end;

procedure ProcessingArticleObj.Process_FraenkelTermsScope ( var aFrm:FormulaPtr );
begin
  Process_Formula(aFrm);
end;

procedure ProcessingArticleObj.Process_TypeList ( aTypeList:PList );
 var i: integer;
begin
  for i:=0 to aTypeList^.Count-1 do
    Process_TypeWithFVScope(TypePtr(aTypeList^.Items^[i]));
end;

procedure ProcessingArticleObj.Process_Label(var aLab:LabelPtr);
begin
 if (aLab <> nil) and (aLab.nLabelIdNr > 0) then
  begin
//   aLab^.nLabelIdNr
  end;
end;

procedure ProcessingArticleObj.Process_DefiniensLabel(var aLab:LabelPtr);
begin
 if (aLab <> nil) and (aLab.nLabelIdNr > 0) then
  begin
//   aLab^.nLabelIdNr
  end;
end;

procedure ProcessingArticleObj.Process_DefiniensFormula(var aFrm:FormulaPtr );
begin
  Process_FormulaWithFVScope(aFrm);
end;

procedure ProcessingArticleObj.Process_Proposition(aProp:PropositionPtr);
begin
// CurPos:=aProp^.nSntPos;
 Process_Label(aProp^.nLab);
 Process_FormulaWithFVScope(aProp^.nSentence);
end;

procedure ProcessingArticleObj.Process_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr);
begin
  with aCStm^ do
  begin
   Process_Label(nProp^.nLab);
   Process_FormulaWithFVScope(nProp^.nSentence);
   Process_Justification(nJustification,aBlock);
  end;
end;

procedure ProcessingArticleObj.Process_ReservationSegment(var aRes: ReservationSegmentPtr);
 var i: integer;
begin
   with aRes^ do
   begin
    Process_StartReservedVariables(aRes);
    for i:=0 to nIdentifiers.Count-1 do
      Process_ReservedVariable( VariablePtr(nIdentifiers.Items^[i]));
    Process_TypeWithFVScope(nResType);
    Process_FinishReservedVariables(aRes);
   end;
end;

procedure ProcessingArticleObj.Process_ChoiceStatement(aChoice: ChoiceStatementPtr);
 var i: integer;
begin
  with aChoice^ do
  begin
    for i:= 0 to  nQualVars^.Count-1 do
      Process_ChoiceVariablesSegment( QualifiedSegmentPtr(nQualVars^.Items^[i]));
    Process_Conditions(nConditions);
    Process_Justification(JustificationPtr(nJustification),nil);
  end;
end;

procedure ProcessingArticleObj.Process_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr);
 var i: integer;
begin
 case aRStm^.nStatementSort of
 stDiffuseStatement:
  begin
   Process_Label(DiffuseStatementPtr(aRStm)^.nLab);
   Process_Block(aBlock);
  end;
 stCompactStatement:
   Process_CompactStatement(CompactStatementPtr(aRStm),aBlock);
 stIterativeEquality:
  begin
   Process_CompactStatement(CompactStatementPtr(aRStm),nil);
   with IterativeEqualityPtr(aRStm)^ do
    for i := 0 to nIterSteps^.Count - 1 do
     with IterativeStepPtr(nIterSteps^.Items^[i])^ do
     begin
      Process_TermWithFVScope(nTerm);
      Process_Justification(JustificationPtr(nJustification),nil);
     end;
  end;
 end;
end;

procedure ProcessingArticleObj.Process_PrivateReference(var aRef: LocalReferencePtr);
begin
//     LocalReferencePtr(Refs^.Items^[i])^.nLabId
end;

procedure ProcessingArticleObj.Process_LibraryReference(var aRef: LibraryReferencePtr);
begin
  with aRef^ do
  case nRefSort of
   TheoremReference:
    begin
//     TheoremReferencePtr(aRefs^.Items^[i])^.nArticleNr
//     TheoremReferencePtr(aRefs^.Items^[i])^.nTheoNr
    end;
   DefinitionReference:
    begin
//     DefinitionReferencePtr(aRefs^.Items^[i])^.nArticleNr
//     DefinitionReferencePtr(aRefs^.Items^[i])^.nTheoNr
    end;
  end;
end;

procedure ProcessingArticleObj.Process_References(aRefs: PList);
 var i: integer;
begin
 for i:= 0 to aRefs^.Count-1 do
  with ReferencePtr(aRefs^.Items^[i])^ do
  case nRefSort of
   LocalReference:
     Process_PrivateReference(LocalReferencePtr(aRefs^.Items^[i]));
   TheoremReference, DefinitionReference:
     Process_LibraryReference(LibraryReferencePtr(aRefs^.Items^[i]));
  end;
end;

procedure ProcessingArticleObj.Process_StraightforwardJustification(aInf: StraightforwardJustificationPtr);
begin
 Process_References(aInf^.nReferences);
end;

procedure ProcessingArticleObj.Process_SchemeJustification(var aInf: SchemeJustificationPtr);
begin
  Process_References(aInf^.nReferences);
end;

procedure ProcessingArticleObj.Process_Justification(var aInf: JustificationPtr; aBlock:wsBlockPtr);
begin
  case aInf^.nInfSort of
   infStraightforwardJustification:
    Process_StraightforwardJustification(StraightforwardJustificationPtr(aInf));
   infSchemeJustification:
    Process_SchemeJustification(SchemeJustificationPtr(aInf));
   infProof:
    Process_Block(aBlock);
   infError: ;
  end;
end;

procedure ProcessingArticleObj.Process_SchemeHead(var aSch: SchemePtr);
 var i,j: integer;
begin
 with aSch^ do
  begin
    for j:=0 to nSchemeParams.Count-1 do
    begin
     Process_StartSchemeSegment(SchemeSegmentPtr(nSchemeParams.Items^[j]));
     case SchemeSegmentPtr(nSchemeParams.Items^[j])^.nSegmSort of
      PredicateSegment:
       with SchemeSegmentPtr(nSchemeParams.Items^[j])^ do
       begin
        for i:=0 to nVars.Count-1 do
          Process_SchemePredicateVariable( VariablePtr(nVars.Items^[i]),nTypeExpList.Count);
        Process_TypeList(nTypeExpList);
       end;
      FunctorSegment:
       with FunctorSegmentPtr(nSchemeParams.Items^[j])^ do
       begin
        for i:=0 to nVars.Count-1 do
          Process_SchemeFunctorVariable( VariablePtr(nVars.Items^[i]),nTypeExpList.Count);
        Process_TypeList(nTypeExpList);
        Process_TypeWithFVScope(nSpecification);
       end;
     end;
     Process_FinishSchemeSegment(SchemeSegmentPtr(nSchemeParams.Items^[j]));
    end;
  end;
end;

procedure ProcessingArticleObj.Process_Conditions(aCond: PList);
 var i: integer;
begin
 for i:=0 to aCond^.Count-1 do
   Process_Proposition(aCond^.Items^[i]);
end;

procedure ProcessingArticleObj.Process_AssumptionConditions(aCond: AssumptionPtr);
begin
 case aCond^.nAssumptionSort of
 SingleAssumption:
   Process_Proposition(SingleAssumptionPtr(aCond)^.nProp);
 CollectiveAssumption:
   Process_Conditions(CollectiveAssumptionPtr(aCond)^.nConditions);
 end;
end;

procedure ProcessingArticleObj.Process_Locus( var aLocus: LocusPtr);
begin
 with aLocus ^ do
 begin
//   nVarId
 end;
end;

procedure ProcessingArticleObj.Process_Loci( aLoci: PList);
 var i: integer;
begin
 if (aLoci = nil) or (aLoci^.Count = 0) then
 else
  begin
   for i:=0 to aLoci^.Count-1 do
     Process_Locus(LocusPtr(aLoci^.Items^[i]));
  end;
end;

procedure ProcessingArticleObj.Process_Pattern(aPattern: PatternPtr);
begin
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
   Process_Loci(nArgs);
//   nAttrSymbol;
  end;
 end;
end;

procedure ProcessingArticleObj.Process_LociEqualities(aEqLociList:PList);
 var i: integer;
begin
 for i := 0 to aEqLociList.Count - 1 do
  with LociEqualityPtr(aEqLociList^.Items^[i])^ do
  begin
   Process_Locus(nLeftLocus);
   Process_Locus(nRightLocus);
  end;
end;

procedure ProcessingArticleObj.Process_Definiens(aDef:DefiniensPtr);
 var i: integer;
begin
  if aDef <> nil then
  with DefiniensPtr(aDef)^ do
   begin
    case nDefSort of
    SimpleDefiniens:
     begin
      Process_DefiniensLabel(nDefLabel);
      with SimpleDefiniensPtr(aDef)^,nExpression^ do
      begin
       case nExprKind of
        exTerm: Process_TermWithFVScope(TermPtr(nExpr));
        exFormula: Process_DefiniensFormula(FormulaPtr(nExpr));
       end;
      end;
     end;
    ConditionalDefiniens:
     begin
      Process_DefiniensLabel(nDefLabel);
      with ConditionalDefiniensPtr(aDef)^ do
      begin
       for i:=0 to nConditionalDefiniensList^.Count-1 do
         with PartDefPtr(nConditionalDefiniensList^.Items^[I])^ do
         begin
          with nPartDefiniens^ do
           case nExprKind of
            exTerm: Process_TermWithFVScope(TermPtr(nExpr));
            exFormula: Process_DefiniensFormula(FormulaPtr(nExpr));
           end;
          Process_DefiniensFormula(nGuard);
         end;
       if nOtherwise <> nil then
        with nOtherwise^ do
         case nExprKind of
          exTerm: Process_TermWithFVScope(TermPtr(nExpr));
          exFormula: Process_DefiniensFormula(FormulaPtr(nExpr));
         end;
      end;
     end;
    end;
   end;
end;

procedure ProcessingArticleObj.Process_StartBlock(aWSBlock:WSBlockPtr);
begin
end;

procedure ProcessingArticleObj.Process_FinishBlock(aWSBlock:WSBlockPtr);
begin
end;

procedure ProcessingArticleObj.Process_Block(aWSBlock:WSBlockPtr);
 var i: integer;
     lBlockPtr: biBlockPtr;
begin
 with aWSBlock^ do
 begin
  lBlockPtr:=nBlockPtr;
  inc(nStackCnt);
  if nStackCnt >= length(nStackArr) then
    setlength(nStackArr,2*length(nStackArr));
  nStackArr[nStackCnt]:=CreateBlock(nBlockKind);
  CurPos:=nBlockPos;
  Process_StartBlock(aWSBlock);
  nBlockPtr:=biBlockPtr(nStackArr[nStackCnt]);
  case nBlockKind of
  blDiffuse:
   begin
   end;
  blHereby:
   begin
   end;
  blProof:
   begin
   end;
  blDefinition:
   begin
   end;
  blNotation:
   begin
   end;
  blRegistration:
   begin
   end;
  blCase: ;
  blSuppose: ;
  blPublicScheme: ;
  end;
  for i := 0 to nItems.Count - 1 do
    Process_Item(nItems.Items^[i]);
  CurPos:=nBlockEndPos;
 end;
 Process_FinishBlock(aWSBlock);
 nBlockPtr:=lBlockPtr;
 dispose(nStackArr[nStackCnt],Done);
 dec(nStackCnt);
end;

function ProcessingArticleObj.CreateBlock(fBlockKind:BlockKind): biBlockPtr;
begin
 result:=new(biBlockPtr,Init(fBlockKind));
end;

function ProcessingArticleObj.CreateItem(fItemKind:ItemKind): biItemPtr;
begin
 result:=new(biItemPtr, Init(fItemKind));
end;

function ProcessingArticleObj.CreateExpression(fExpKind:ExpKind): biExpressionPtr;
begin
 result:=new(biExpressionPtr, Init(fExpKind));
end;

function ProcessingArticleObj.CreateExpressionsVariableLevel: biSubexpPtr;
begin
 result:=new(biSubexpPtr, Init);
end;

procedure ProcessingArticleObj.Process_Article;
 var i: integer;
begin
 setlength(nStackArr,50);
 nStackCnt:=0;
 nStackArr[0]:=CreateBlock(blMain);
 nBlockPtr:=biBlockPtr(nStackArr[0]);
 with nTextProper^ do
 begin
  for i := 0 to nItems.Count - 1 do
    Process_Item(nItems.Items^[i]);
 end;
 MizAssert(2841,nStackCnt=0);
 dispose(nStackArr[nStackCnt],Done);
end;

procedure ProcessingArticleObj.Process_Definition(aWSBlock:WSBlockPtr);
begin
  Process_Block(aWSBlock);
end;

procedure ProcessingArticleObj.Process_CaseBlock(aWSBlock:WSBlockPtr);
begin
  Process_Block(aWSBlock);
end;

procedure ProcessingArticleObj.Process_Scheme(aWSBlock:WSBlockPtr);
begin
  Process_Block(aWSBlock);
end;

procedure ProcessingArticleObj.Process_SchemePremises(aSchema:SchemePtr);
 var i: integer;
begin
 with aSchema^ do
  begin
    Process_Conditions(nSchemePremises);
  end;
end;

procedure ProcessingArticleObj.Process_ImplicitlyRedefinedVariable(aRes:TypeChangePtr);
begin
end;

procedure ProcessingArticleObj.Process_RedefinedVariable(aRes:TypeChangePtr);
begin
//  aRes^.nVariable
  Process_TermWithFVScope(aRes^.nTermExpr);
end;

procedure ProcessingArticleObj.Process_ExemplifyingVariable(aExampl:ExamplePtr);
begin
 if aExampl^.nTermExpr <> nil then
   Process_TermWithFVScope(aExampl^.nTermExpr);
end;

procedure ProcessingArticleObj.Process_ImplicitExamplification(aExampl:ExamplePtr);
begin
  Process_TermWithFVScope(aExampl^.nTermExpr);
end;

procedure ProcessingArticleObj.Process_FieldInStructureDefinition(aField:FieldSymbolPtr);
begin
   with aField^ do
    begin
//           nFieldSymbol
    end;
end;

procedure ProcessingArticleObj.Process_FieldsInStructureDefinition(aStruct:StructureDefinitionPtr);
 var i,j: integer;
begin
 with aStruct^ do
   for i := 0 to nSgmFields^.Count - 1 do
    with FieldSegmentPtr(nSgmFields^.Items^[i])^ do
     begin
      for j := 0 to nFields^.Count - 1 do
        Process_FieldInStructureDefinition(FieldSymbolPtr(nFields^.Items^[j]));
      Process_TypeWithFVScope(nSpecification);
     end;
end;

procedure ProcessingArticleObj.Process_ConstantDefinition(aDef:ConstantDefinitionPtr);
begin
 with aDef^ do
  begin
//    nVarId
    Process_TermWithFVScope(nTermExpr);
  end;
end;

procedure ProcessingArticleObj.Process_Section;
begin
end;

procedure ProcessingArticleObj.Process_Pragma(aPragma: PragmaPtr);
begin
end;

procedure ProcessingArticleObj.Process_Theorem(aCStm:CompactStatementPtr; aBlock:wsBlockPtr);
begin
 Process_CompactStatement(aCStm,aBlock);
end;

procedure ProcessingArticleObj.Process_StartItem(aWSItem:WSItemPtr);
begin
end;

procedure ProcessingArticleObj.Process_FinishItem(aWSItem:WSItemPtr);
begin
end;

procedure ProcessingArticleObj.Process_Item(aWSItem:WSItemPtr);
 var i,j,lIndent: integer;
begin
 with aWSItem^ do
 begin
  CurPos:=nItemPos;
  if nDisplayInformationOnScreen then
    DisplayLine(CurPos.Line,ErrorNbr);
  inc(nStackCnt);
  if nStackCnt >= length(nStackArr) then
    setlength(nStackArr,2*length(nStackArr));
  nStackArr[nStackCnt]:=CreateItem(nItemKind);
  Process_StartItem(aWSItem);
  case aWSItem^.nItemKind of
  itDefinition:
    Process_Definition(nBlock);
  itSchemeBlock:
    Process_Scheme(nBlock);
  itSchemeHead:
   with SchemePtr(nContent)^ do
   begin
    Process_SchemeHead(SchemePtr(nContent));
    Process_SchemeConclusion(SchemePtr(nContent)^.nSchemeConclusion);
    Process_SchemePremises(SchemePtr(nContent));
   end;
  itTheorem:
    Process_Theorem(CompactStatementPtr(nContent),nBlock);
  itAxiom:
   begin
   end;
  itReservation:
    Process_ReservationSegment(ReservationSegmentPtr(nContent));
  itRegularStatement:
    Process_RegularStatement(RegularStatementPtr(nContent),nBlock);
  itChoice:
    Process_ChoiceStatement(ChoiceStatementPtr(nContent));
  itReconsider:
   with TypeChangingStatementPtr(nContent)^ do
   begin
    Process_StartReconsideringVariableSegment;
    for i:=0 to nTypeChangeList.Count-1 do
     begin
      case TypeChangePtr(nTypeChangeList.Items^[i])^.nTypeChangeKind of
       Equating:
         Process_RedefinedVariable(TypeChangePtr(nTypeChangeList.Items^[i]));
       VariableIdentifier:
         Process_ImplicitlyRedefinedVariable(TypeChangePtr(nTypeChangeList.Items^[i]));
      end;
     end;
    Process_TypeWithFVScope(nTypeExpr);
    Process_Justification(JustificationPtr(nJustification),nil);
    Process_FinishReconsideringVariableSegment;
   end;
  itPrivFuncDefinition:
   with PrivateFunctorDefinitionPtr(nContent)^ do
   begin
    Process_StartPrivateFunctor;
    Process_LocalFunctorVariable(nFuncId,nTypeExpList.Count);
    Process_TypeList(nTypeExpList);
    Process_TermWithFVScope(nTermExpr);
    Process_FinishPrivateFunctor;
   end;
  itPrivPredDefinition:
   with PrivatePredicateDefinitionPtr(nContent)^ do
   begin
    Process_StartPrivatePredicate;
    Process_LocalPredicateVariable(nPredId,nTypeExpList.Count);
    Process_TypeList(nTypeExpList);
    Process_FormulaWithFVScope(nSentence);
    Process_FinishPrivatePredicate;
   end;
  itConstantDefinition:
   begin
    Process_StartVariableSegment;
    Process_ConstantDefinition(ConstantDefinitionPtr(nContent));
    Process_FinishOneVariableSegment;
   end;
  itLociDeclaration,
  itGeneralization:
    Process_FixedVariablesSegment( QualifiedSegmentPtr(nContent));
  itAssumption:
    Process_AssumptionConditions(AssumptionPtr(nContent));
  itExistentialAssumption:
   with ExistentialAssumptionPtr(nContent)^ do
    begin
     for i := 0 to  nQVars^.Count-1 do
       Process_ChoiceVariablesSegment( QualifiedSegmentPtr(nQVars^.Items^[i]));
     Process_Conditions(nConditions);
    end;
  itExemplification:
   with ExamplePtr(nContent)^ do
   begin
    if nVarId <> nil then
     begin
      Process_StartVariableSegment;
      Process_ExemplifyingVariable(ExamplePtr(nContent));
      Process_FinishOneVariableSegment;
     end
    else
      Process_ImplicitExamplification(ExamplePtr(nContent));
   end;
  itPerCases:
    Process_Justification(JustificationPtr(nContent),nil);
  itConclusion:
    Process_RegularStatement(RegularStatementPtr(nContent),nBlock);
  itCaseBlock:
    Process_CaseBlock(nBlock);
  itCaseHead,
  itSupposeHead:
    Process_AssumptionConditions(AssumptionPtr(nContent));
  itCorrCond:
   begin
//    CorrectnessConditionPtr(nContent)^.nCorrCondSort
    Process_Justification(CorrectnessConditionPtr(nContent)^.nJustification,nBlock);
   end;
  itCorrectness:
    Process_Justification(CorrectnessPtr(nContent)^.nJustification,nBlock);
  itProperty:
   begin
//    PropertyPtr(nContent)^.nPropertySort
    Process_Justification(PropertyPtr(nContent)^.nJustification,nBlock);
   end;
  itDefMode:
   with ModeDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
     end;
    Process_Pattern(nDefModePattern);
    case nDefKind of
    defExpandableMode:
      Process_TypeWithFVScope(ExpandableModeDefinitionPtr(nContent)^.nExpansion);
    defStandardMode:
     with StandardModeDefinitionPtr(nContent)^ do
     begin
      if nSpecification <> nil then
        Process_TypeWithFVScope(nSpecification);
      if nDefiniens <> nil then
        Process_Definiens(nDefiniens);
     end;
    end;
   end;
  itDefAttr:
   with AttributeDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
     end;
    Process_Pattern(nDefAttrPattern);
    Process_Definiens(nDefiniens);
   end;
  itDefPred:
   with PredicateDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
     end;
    Process_Pattern(nDefPredPattern);
    if nDefiniens <> nil then
     Process_Definiens(nDefiniens);
   end;
  itDefFunc:
   with FunctorDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
     end;
    Process_Pattern(nDefFuncPattern);
    if nSpecification <> nil then
      Process_TypeWithFVScope(nSpecification);
    case nDefiningWay of
     dfEmpty:;
     dfMeans:
      begin
      end;
     dfEquals:
      begin
      end;
    end;
    Process_Definiens(nDefiniens);
   end;
  itDefStruct:
   with StructureDefinitionPtr(nContent)^ do
   begin
     for i := 0 to nAncestors.Count - 1 do
      Process_TypeWithFVScope(TypePtr(nAncestors^.Items^[i]));
//     nStructSymb
     Process_Pattern(nDefStructPattern);
     Process_FieldsInStructureDefinition(StructureDefinitionPtr(nContent));
   end;
  itPredSynonym,
  itFuncNotation, itModeNotation,
  itAttrSynonym:
   with NotationDeclarationPtr(nContent)^ do
   begin
    Process_Pattern(nNewPattern);
    Process_Pattern(nOriginPattern);
   end;
  itPredAntonym,itAttrAntonym:
   with NotationDeclarationPtr(nContent)^ do
   begin
    Process_Pattern(nNewPattern);
    Process_Pattern(nOriginPattern);
   end;
  itCluster:
   begin
    case ClusterPtr(nContent)^.nClusterKind of
     ExistentialRegistration:
      with EClusterPtr(nContent)^ do
       begin
        Process_AdjectiveList(nConsequent);
        Process_TypeWithFVScope(nClusterType);
       end;
     ConditionalRegistration:
      with CClusterPtr(nContent)^ do
       begin
        Process_AdjectiveList(nAntecedent);
        Process_AdjectiveList(nConsequent);
        Process_TypeWithFVScope(nClusterType);
       end;
     FunctorialRegistration:
      with FClusterPtr(nContent)^ do
       begin
        Process_TermWithFVScope(nClusterTerm);
        Process_AdjectiveList(nConsequent);
        if nClusterType <> nil then
         Process_TypeWithFVScope(nClusterType);
       end;
    end;
   end;
  itIdentify:
   with IdentifyRegistrationPtr(nContent)^ do
   begin
    Process_Pattern(nNewPattern);
    Process_Pattern(nOriginPattern);
    if nEqLociList <> nil then
     Process_LociEqualities(nEqLociList);
   end;
  itReduction:
   with ReduceRegistrationPtr(nContent)^ do
    begin
     Process_TermWithFVScope(nNewTerm);
     Process_TermWithFVScope(nOriginTerm);
    end;
  itPropertyRegistration:
   case PropertyRegistrationPtr(nContent)^.nPropertySort of
   sySethood:
    with SethoodRegistrationPtr(nContent)^ do
    begin
     //nPropertySort
     Process_TypeWithFVScope(nSethoodType);
     Process_Justification(nJustification,nBlock);
    end;
   end;
  itSection:
    Process_Section;
  itPragma:
    Process_Pragma(PragmaPtr(nContent));
  itIncorrItem:;
  end;
  CurPos:=nItemEndPos;
 end;
 Process_FinishItem(aWSItem);
 dispose(nStackArr[nStackCnt],Done);
 dec(nStackCnt);
end;

{procedure Process_WSMizArticle(aWSTextProper:wsTextProperPtr; aFileName:string);
 var lMizArticle: ProcessingArticlePtr;
begin
  lMizArticle:=new(ProcessingArticlePtr,Init(aFileName));
  lMizArticle^.Process_TextProper(aWSTextProper);
  dispose(lMizArticle,Done);
end;
}
{----------------------------------------------------------------}

end.
