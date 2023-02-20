(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit wsmarticle;

interface

uses mobjects, errhan, mscanner, syntax, abstract_syntax, xml_dict, xml_inout;

type
 wsBlockPtr = ^wsBlock;
 wsBlock = object(MObject)
  nBlockKind: BlockKind;
  nItems: PList;
  nBlockPos,nBlockEndPos: Position;
  constructor Init(aBlokKind:BlockKind; const aPos:Position);
  destructor Done; virtual;
 end;

 wsItemPtr = ^wsItem;
 wsItem = object(MObject)
  nItemKind: ItemKind;
  nItemPos,nItemEndPos: Position;
  nContent: PObject;
  nBlock: wsBlockPtr;
  constructor Init(aItemKind:ItemKind; const aPos:Position);
  destructor Done; virtual;
 end;

 wsTextProperPtr = ^wsTextProper;
 wsTextProper = object(wsBlock)
  nArticleID, nArticleExt: string;
  constructor Init(const aArticleID,aArticleExt:string; const aPos:Position);
  destructor Done; virtual;

  function NewBlock(aBlockKind:BlockKind; const aPos:Position): wsBlockPtr;
  function NewItem(aItemKind:ItemKind; const aPos:Position): wsItemPtr;
 end;

 PragmaPtr = ^PragmaObj;
 PragmaObj = object(MObject)
  nPragmaStr: string;
  constructor Init(aStr: string);
 end;

 LabelPtr = ^LabelObj;
 LabelObj = object(MObject)
   nLabelIdNr: integer;
   nLabelPos: Position;
  constructor Init(aLabelId:integer; const aPos:Position);
 end;

 PropositionPtr = ^PropositionObj;
 PropositionObj = object(Mobject)
   nLab: LabelPtr;
   nSntPos: Position;
   nSentence: FormulaPtr;
  constructor Init(aLab:LabelPtr; aSentence:FormulaPtr; const aSntPos:Position);
  destructor Done; virtual;
 end;

 ReferenceKind = ( LocalReference,
                   TheoremReference,
                   DefinitionReference
                  );

 InferenceKind = ( infError,
                   infStraightforwardJustification,
                   infSchemeJustification,
                   infProof,
                   infSkippedProof);

 ReferencePtr = ^ReferenceObj;
 ReferenceObj =
   object(MObject)
     nRefSort: ReferenceKind;
     nRefPos: Position;
   end;

 LocalReferencePtr = ^LocalReferenceObj;
 LocalReferenceObj =
   object(ReferenceObj)
     nLabId: integer;
    constructor Init(aLabId:integer; const aPos:Position);
   end;

 LibraryReferencePtr = ^LibraryReferenceObj;
 LibraryReferenceObj =
   object(ReferenceObj)
    nArticleNr: integer;
   end;

 TheoremReferencePtr = ^TheoremReferenceObj;
 TheoremReferenceObj =
   object(LibraryReferenceObj)
    nTheoNr:integer;
    constructor Init(aArticleNr,aTheoNr:integer; const aPos:Position);
   end;

 DefinitionReferencePtr = ^DefinitionReferenceObj;
 DefinitionReferenceObj =
   object(LibraryReferenceObj)
    nDefNr:integer;
    constructor Init(aArticleNr,aDefNr:integer; const aPos:Position);
   end;

 JustificationPtr = ^JustificationObj;
 JustificationObj =
  object(MObject)
   nInfSort: InferenceKind;
   nInfPos: Position;
   constructor Init(aInferSort:InferenceKind; const aPos: Position);
  end;

 SimpleJustificationPtr = ^SimpleJustificationObj;
 SimpleJustificationObj =
  object(JustificationObj)
   nReferences: PList;
   constructor Init(aInferSort:InferenceKind; const aPos: Position);
   destructor Done; virtual;
  end;

 StraightforwardJustificationPtr = ^StraightforwardJustificationObj;
 StraightforwardJustificationObj =
  object(SimpleJustificationObj)
   nLinked: boolean;
   nLinkPos: Position;
   constructor Init(const aPos:Position; aLinked:boolean; const aLinkPos:Position);
   destructor Done; virtual;
  end;

 SchemeJustificationPtr = ^SchemeJustificationObj;
 SchemeJustificationObj =
  object(SimpleJustificationObj)
   nSchFileNr: integer; {0 for schemes from current article and > 0 for library refernces }
   nSchemeIdNr: integer; { a number of a scheme for library reference ( nSchFileNr > 0) or
                           a number of an identifier name for scheme name from current article}
   nSchemeInfPos: Position;
   constructor Init(const aPos:Position; aArticleNr,aNr:integer);
   destructor Done; virtual;
  end;

 SchemeSegmentKind = (PredicateSegment,FunctorSegment);

 SchemeSegmentPtr = ^SchemeSegmentObj;
 SchemeSegmentObj =
  object(MObject)
   nSegmPos: Position;
   nSegmSort: SchemeSegmentKind;
   nVars: PList;
   nTypeExpList: PList;
   constructor Init(const aPos:Position; aSegmSort:SchemeSegmentKind;
                   aVars,aTypeExpList: PList);
   destructor Done; virtual;
  end;

 PredicateSegmentPtr = SchemeSegmentPtr;
 FunctorSegmentPtr = ^FunctorSegmentObj;
 FunctorSegmentObj =
  object(SchemeSegmentObj)
   nSpecification: TypePtr;
   constructor Init(const aPos:Position;
                   aVars,aTypeExpList: PList; aSpecification: TypePtr);
   destructor Done; virtual;
  end;

 SchemePtr = ^SchemeObj;
 SchemeObj =
  object(MObject)
   nSchemeIdNr: integer;
   nSchemePos: Position;
   nSchemeParams: PList;
   nSchemeConclusion: FormulaPtr;
   nSchemePremises: PList;
   constructor Init(aIdNr:integer; const aPos:Position; aParams:PList;
                    aPrems:PList; aConcl:FormulaPtr);
   destructor Done; virtual;
  end;

 ReservationSegmentPtr = ^ReservationSegmentObj;
 ReservationSegmentObj =
  object(MObject)
   nIdentifiers: PList;
   nResType: TypePtr;
   constructor Init(aIdentifiers:PList; aType:TypePtr);
   destructor Done; virtual;
  end;

 PrivateFunctorDefinitionPtr = ^PrivateFunctorDefinitionObj;
 PrivateFunctorDefinitionObj =
  object(MObject)
   nFuncId: VariablePtr;
   nTypeExpList: PList;
   nTermExpr: TermPtr;
   constructor Init(aFuncId:VariablePtr; aTypeExpList:Plist; aTerm:TermPtr);
   destructor Done; virtual;
  end;

 PrivatePredicateDefinitionPtr = ^PrivatePredicateDefinitionObj;
 PrivatePredicateDefinitionObj =
  object(MObject)
   nPredId: VariablePtr;
   nTypeExpList: PList;
   nSentence: FormulaPtr;
   constructor Init(aPredId:VariablePtr; aTypeExpList:Plist; aSnt:FormulaPtr);
   destructor Done; virtual;
  end;

 ConstantDefinitionPtr = ^ConstantDefinitionObj;
 ConstantDefinitionObj =
  object(MObject)
   nVarId: VariablePtr;
   nTermExpr: TermPtr;
   constructor Init(aVarId:VariablePtr; aTerm:TermPtr);
   destructor Done; virtual;
  end;

 TypeChangeSort =  (Equating,VariableIdentifier);

 TypeChangePtr = ^TypeChangeObj;
 TypeChangeObj =
  object(MObject)
    nTypeChangeKind: TypeChangeSort;
    nVar: VariablePtr;
    nTermExpr: TermPtr;
   constructor Init(aKind:TypeChangeSort; aVar:VariablePtr; aTerm:TermPtr);
   destructor Done; virtual;
  end;

 ExamplePtr = ^ExampleObj;
 ExampleObj =
  object(MObject)
   nVarId: VariablePtr;
   nTermExpr: TermPtr;
   constructor Init(aVarId:VariablePtr; aTerm:TermPtr);
   destructor Done; virtual;
  end;

 TypeChangingStatementPtr = ^TypeChangingStatementObj;
 TypeChangingStatementObj =
  object(MObject)
   nTypeChangeList: PList;
   nTypeExpr: TypePtr;
   nJustification: SimpleJustificationPtr;
   constructor Init(aTypeChangeList: PList; aTypeExpr: TypePtr;
                    aJustification:SimpleJustificationPtr);
   destructor Done; virtual;
  end;

 ChoiceStatementPtr = ^ChoiceStatementObj;
 ChoiceStatementObj =
  object(MObject)
   nQualVars: PList;
   nConditions: PList;
   nJustification: SimpleJustificationPtr;
   constructor Init(aQualVars,aConds:PList; aJustification:SimpleJustificationPtr);
   destructor Done; virtual;
  end;

 RegularStatementKind =  (stDiffuseStatement,stCompactStatement,stIterativeEquality);

 RegularStatementPtr = ^RegularStatementObj;
 RegularStatementObj =
  object(MObject)
   nStatementSort: RegularStatementKind;
   nLab: LabelPtr;
   constructor Init(aStatementSort:RegularStatementKind);
   destructor Done; virtual;
  end;

 DiffuseStatementPtr = ^DiffuseStatementObj;
 DiffuseStatementObj =
  object(RegularStatementObj)
   constructor Init(aLab: LabelPtr; aStatementSort:RegularStatementKind);
   destructor Done; virtual;
  end;

 CompactStatementPtr =  ^CompactStatementObj;
 CompactStatementObj =
  object(RegularStatementObj)
   nProp: PropositionPtr;
   nJustification: JustificationPtr;
   constructor Init(aProp:PropositionPtr; aJustification:JustificationPtr);
   destructor Done; virtual;
  end;

  IterativeStepPtr = ^IterativeStepObj;
  IterativeStepObj =
   object(MObject)
    nIterPos: Position;
    nTerm: TermPtr;
    nJustification: SimpleJustificationPtr;
    constructor Init(const aPos:Position; aTerm: TermPtr; aJustification:JustificationPtr);
    destructor Done; virtual;
   end;

  IterativeEqualityPtr = ^IterativeEqualityObj;
  IterativeEqualityObj =
   object(CompactStatementObj)
    nIterSteps: PList;
    constructor Init(aProp:PropositionPtr; aJustification:JustificationPtr; aIters: PList);
    destructor Done; virtual;
   end;

  FieldSymbolPtr = ^FieldSymbolObj;
  FieldSymbolObj =
   object(MObject)
     nFieldPos: Position;
     nFieldSymbol: integer;
    constructor Init(const aPos:Position; aFieldSymbNr:integer);
   end;

  FieldSegmentPtr = ^FieldSegmentObj;
  FieldSegmentObj =
   object(MObject)
     nFieldSegmPos: Position;
     nFields: PList;
     nSpecification: TypePtr;
    constructor Init(const aPos:Position; aFields:PList; aSpec:TypePtr);
    destructor Done; virtual;
   end;

  LocusPtr = ^LocusObj;
  LocusObj =
   object(MObject)
    nVarId: integer;
    nVarIdPos: Position;
    constructor Init(const aPos:Position; aIdentNr:integer);
   end;

  PatternPtr = ^PatternObj;
  PatternObj =
   object(Mobject)
     nPatternPos: Position;
     nPatternSort: ItemKind;
    constructor Init(const aPos:Position; aSort:ItemKind);
   end;

  ModePatternPtr = ^ModePatternObj;
  ModePatternObj =
   object(PatternObj)
     nModeSymbol: Integer;
     nArgs: PList;
    constructor Init(const aPos:Position; aSymb:integer; aArgs:PList);
    destructor Done; virtual;
   end;

  AttributePatternPtr = ^AttributePatternObj;
  AttributePatternObj =
   object(PatternObj)
     nAttrSymbol: Integer;
     nArg: LocusPtr;
     nArgs: PList;
    constructor Init(const aPos:Position; aArg:LocusPtr; aSymb:integer; aArgs:PList);
    destructor Done; virtual;
   end;

  PredicatePatternPtr = ^PredicatePatternObj;
  PredicatePatternObj =
   object(PatternObj)
     nPredSymbol: Integer;
     nLeftArgs,nRightArgs: PList;
    constructor Init(const aPos:Position; aLArgs:PList; aSymb:integer; aRArgs:PList);
    destructor Done; virtual;
   end;

  FunctorSort = (InfixFunctor,CircumfixFunctor);

  FunctorPatternPtr = ^FunctorPatternObj;
  FunctorPatternObj =
   object(PatternObj)
     nFunctKind: FunctorSort;
    constructor Init(const aPos:Position; aKind: FunctorSort);
   end;

  CircumfixFunctorPatternPtr = ^CircumfixFunctorPatternObj;
  CircumfixFunctorPatternObj =
   object(FunctorPatternObj)
     nLeftBracketSymb,nRightBracketSymb: integer;
     nArgs: PList;
    constructor Init(const aPos:Position; aLBSymb,aRBSymb:integer; aArgs:PList);
    destructor Done; virtual;
   end;

  InfixFunctorPatternPtr = ^InfixFunctorPatternObj;
  InfixFunctorPatternObj =
   object(FunctorPatternObj)
     nOperSymb: integer;
     nLeftArgs,nRightArgs: PList;
    constructor Init(const aPos:Position; aLArgs:PList; aSymb:integer; aRArgs:PList);
    destructor Done; virtual;
   end;

  StructureDefinitionPtr = ^StructureDefinitionObj;
  StructureDefinitionObj =
   object(MObject)
     nStrPos: Position;
     nAncestors: PList;
     nDefStructPattern: ModePatternPtr;
//     nStructSymb: integer;
//     nOverArgs: PList;
     nSgmFields: PList;
    constructor Init(const aPos:Position; aAncestors:PList; aStructSymb:integer;
                     aOverArgs:PList; aFields:PList);
    destructor Done; virtual;
   end;

 HowToDefine = (dfEmpty,dfMeans,dfEquals);
 DefiniensSort = (SimpleDefiniens,ConditionalDefiniens);

 DefiniensPtr = ^DefiniensObj;
 DefiniensObj =
  object(MObject)
    nDefSort: DefiniensSort;
    nDefPos: Position;
    nDefLabel: LabelPtr;
    constructor Init(const aPos: Position; aLab:LabelPtr; aKind:DefiniensSort);
    destructor Done; virtual;
  end;

  DefExpressionPtr = ^DefExpressionObj;
  DefExpressionObj =
   object(MObject)
    nExprKind: ExpKind;
    nExpr: PObject;
    constructor Init(aKind:ExpKind; aExpr:PObject);
    destructor Done; virtual;
   end;

 SimpleDefiniensPtr = ^SimpleDefiniensObj;
 SimpleDefiniensObj =
  object(DefiniensObj)
    nExpression: DefExpressionPtr;
    constructor Init(const aPos: Position; aLab:LabelPtr; aDef:DefExpressionPtr);
    destructor Done; virtual;
  end;

 PartDefPtr = ^PartDefObj;
 PartDefObj =
  object(MObject)
   nPartDefiniens: DefExpressionPtr;
   nGuard: FormulaPtr;
   constructor Init(aPartDef:DefExpressionPtr; aGuard:FormulaPtr);
   destructor Done; virtual;
  end;

 ConditionalDefiniensPtr = ^ConditionalDefiniensObj;
 ConditionalDefiniensObj =
  object(DefiniensObj)
    nConditionalDefiniensList: PList;
    nOtherwise: DefExpressionPtr;
    constructor Init(const aPos:Position; aLab:LabelPtr; aPartialDefs:PList; aOtherwise:DefExpressionPtr);
    destructor Done; virtual;
  end;

 ModeDefinitionSort = (defExpandableMode,defStandardMode);

 ModeDefinitionPtr = ^ModeDefinitionObj;
 ModeDefinitionObj =
  object(MObject)
    nDefKind: ModeDefinitionSort;
    nDefModePos: Position;
    nDefModePattern: ModePatternPtr;
    nRedefinition: boolean;
    constructor Init(const aPos: Position; aDefKind:ModeDefinitionSort; aRedef: boolean;
                     aPattern: ModePatternPtr);
    destructor Done; virtual;
  end;

 ExpandableModeDefinitionPtr =  ^ExpandableModeDefinitionObj;
 ExpandableModeDefinitionObj =
  object(ModeDefinitionObj)
    nExpansion: TypePtr;
    constructor Init(const aPos:Position; aPattern:ModePatternPtr; aExp:TypePtr);
    destructor Done; virtual;
  end;

 StandardModeDefinitionPtr =  ^StandardModeDefinitionObj;
 StandardModeDefinitionObj =
  object(ModeDefinitionObj)
    nSpecification: TypePtr;
    nDefiniens: DefiniensPtr;
    constructor Init(const aPos:Position; aRedef:boolean; aPattern:ModePatternPtr;
                     aSpec:TypePtr; aDef:DefiniensPtr);
    destructor Done; virtual;
  end;

 AttributeDefinitionPtr = ^AttributeDefinitionObj;
 AttributeDefinitionObj =
  object(MObject)
    nDefAttrPos: Position;
    nDefAttrPattern: AttributePatternPtr;
    nRedefinition: boolean;
    nDefiniens: DefiniensPtr;
    constructor Init(const aPos:Position; aRedef:boolean; aPattern:AttributePatternPtr;
                     aDef:DefiniensPtr);
    destructor Done; virtual;
  end;

 PredicateDefinitionPtr = ^PredicateDefinitionObj;
 PredicateDefinitionObj =
  object(MObject)
    nDefPredPos: Position;
    nDefPredPattern: PredicatePatternPtr;
    nRedefinition: boolean;
    nDefiniens: DefiniensPtr;
    constructor Init(const aPos:Position; aRedef:boolean; aPattern:PredicatePatternPtr;
                     aDef:DefiniensPtr);
    destructor Done; virtual;
  end;

 FunctorDefinitionPtr = ^FunctorDefinitionObj;
 FunctorDefinitionObj =
  object(MObject)
    nDefFuncPos: Position;
    nDefFuncPattern: FunctorPatternPtr;
    nRedefinition: boolean;
    nSpecification: TypePtr;
    nDefiningWay: HowToDefine;
    nDefiniens: DefiniensPtr;
    constructor Init(const aPos:Position; aRedef:boolean; aPattern:FunctorPatternPtr;
                     aSpec:TypePtr; aDefWay:HowToDefine; aDef:DefiniensPtr);
    destructor Done; virtual;
  end;

 NotationDeclarationPtr = ^NotationDeclarationObj;
 NotationDeclarationObj =
  object(mObject)
    nNotationPos: Position;
    nNotationSort: ItemKind;
    nOriginPattern,nNewPattern: PatternPtr;
    constructor Init(const aPos:Position; aNSort:ItemKind; aNewPatt,aOrigPatt:PatternPtr);
    destructor Done; virtual;
  end;

 AssumptionKind =  (SingleAssumption,CollectiveAssumption,ExistentialAssumption);

 AssumptionPtr = ^AssumptionObj;
 AssumptionObj =
  object(MObject)
    nAssumptionPos: Position;
    nAssumptionSort: AssumptionKind;
    constructor Init(const aPos:Position; aSort:AssumptionKind);
  end;

  SingleAssumptionPtr = ^SingleAssumptionObj;
  SingleAssumptionObj =
   object(AssumptionObj)
    nProp: PropositionPtr;
    constructor Init(const aPos:Position; aProp:PropositionPtr);
    destructor Done; virtual;
   end;

  CollectiveAssumptionPtr = ^CollectiveAssumptionObj;
  CollectiveAssumptionObj =
   object(AssumptionObj)
    nConditions: PList;
    constructor Init(const aPos:Position; aProps:PList);
    destructor Done; virtual;
   end;

  ExistentialAssumptionPtr = ^ExistentialAssumptionObj;
  ExistentialAssumptionObj =
   object(CollectiveAssumptionObj)
    nQVars: PList;
    constructor Init(const aPos:Position; aQVars,aProps:PList);
    destructor Done; virtual;
   end;

 CorrectnessPtr =^CorrectnessObj;
 CorrectnessObj =
  object(MObject)
   nCorrCondPos: Position;
   nJustification: JustificationPtr;
   constructor Init(const aPos:Position; aJustification:JustificationPtr);
   destructor Done; virtual;
  end;

 CorrectnessConditionPtr =^CorrectnessConditionObj;
 CorrectnessConditionObj =
  object(CorrectnessObj)
   nCorrCondSort: CorrectnessKind;
   constructor Init(const aPos:Position; aSort:CorrectnessKind;
                    aJustification:JustificationPtr);
   destructor Done; virtual;
  end;

 CorrectnessConditionsSet = set of CorrectnessKind;

 CorrectnessConditionsPtr =^CorrectnessConditionsObj;
 CorrectnessConditionsObj =
  object(CorrectnessObj)
    nConditions: CorrectnessConditionsSet;
   constructor Init(const aPos:Position; const aConds: CorrectnessConditionsSet;
                    aJustification:JustificationPtr);
   destructor Done; virtual;
  end;

 PropertyPtr =^PropertyObj;
 PropertyObj =
  object(MObject)
   nPropertyPos: Position;
   nPropertySort: PropertyKind;
   nJustification: JustificationPtr;
   constructor Init(const aPos:Position; aSort:PropertyKind; aJustification:JustificationPtr);
   destructor Done; virtual;
  end;

 ClusterRegistrationKind = (ExistentialRegistration,ConditionalRegistration,
                            FunctorialRegistration);

 ClusterPtr = ^ClusterObj;
 ClusterObj =
  object(MObject)
   nClusterPos: Position;
   nClusterKind: ClusterRegistrationKind;
   nConsequent: PList;
   nClusterType: TypePtr;
   constructor Init(const aPos: Position; aKind:ClusterRegistrationKind; aCons:PList; aTyp:TypePtr);
   destructor Done; virtual;
  end;

 EClusterPtr = ^EClusterObj;
 EClusterObj =
  object(ClusterObj)
   constructor Init(const aPos: Position; aCons:PList; aTyp:TypePtr);
   destructor Done; virtual;
  end;

 CClusterPtr = ^CClusterObj;
 CClusterObj =
  object(ClusterObj)
   nAntecedent: PList;
   constructor Init(const aPos: Position; aAntec,aCons:PList; aTyp:TypePtr);
   destructor Done; virtual;
  end;

 FClusterPtr = ^FClusterObj;
 FClusterObj =
  object(ClusterObj)
   nClusterTerm: TermPtr;
   constructor Init(const aPos: Position; aTrm:TermPtr; aCons:PList; aTyp:TypePtr);
   destructor Done; virtual;
  end;

 LociEqualityPtr = ^LociEqualityObj;
 LociEqualityObj =
  object(mObject)
    nEqPos: Position;
    nLeftLocus,nRightLocus: LocusPtr;
    constructor Init(const aPos:Position; aLeftLocus,aRightLocus:LocusPtr);
    destructor Done; virtual;
  end;

 IdentifyRegistrationPtr = ^IdentifyRegistrationObj;
 IdentifyRegistrationObj =
  object(mObject)
    nIdentifyPos: Position;
    nOriginPattern,nNewPattern: PatternPtr;
    nEqLociList:PList;
    constructor Init(const aPos:Position; aNewPatt,aOrigPatt:PatternPtr; aEqList:PList);
    destructor Done; virtual;
  end;

 PropertyRegistrationPtr = ^PropertyRegistrationObj;
 PropertyRegistrationObj =
  object(mObject)
    nPropertyPos: Position;
    nPropertySort: PropertyKind;
    constructor Init(const aPos:Position; aKind:PropertyKind);
    destructor Done; virtual;
  end;

 SethoodRegistrationPtr = ^SethoodRegistrationObj;
 SethoodRegistrationObj =
  object(PropertyRegistrationObj)
    nSethoodType: TypePtr;
    nJustification: JustificationPtr;
    constructor Init(const aPos:Position; aKind:PropertyKind; aType:TypePtr);
    destructor Done; virtual;
  end;

 ReduceRegistrationPtr = ^ReduceRegistrationObj;
 ReduceRegistrationObj =
  object(MObject)
    nReducePos: Position;
    nOriginTerm,nNewTerm:TermPtr;
    constructor Init(const aPos:Position; aOrigTerm,aNewTerm:TermPtr);
    destructor Done; virtual;
  end;

 OutWSMizFilePtr = ^OutWSMizFileObj;
 OutWSMizFileObj =
  object(XMLOutStreamObj)
    nDisplayInformationOnScreen: boolean;
    nMizarAppearance: boolean;
   constructor OpenFile(const aFileName:string );
   constructor OpenFileWithXSL(const aFileName:string );
   destructor Done; virtual;

   procedure Out_TextProper(aWSTextProper:WSTextProperPtr); virtual;
   procedure Out_Block(aWSBlock:WSBlockPtr); virtual;
   procedure Out_Item(aWSItem:WSItemPtr); virtual;

   procedure Out_ItemContentsAttr(aWSItem:WSItemPtr); virtual;
   procedure Out_ItemContents(aWSItem:WSItemPtr); virtual;

   procedure Out_Variable( aVar: VariablePtr); virtual;
   procedure Out_ReservedVariable( aVar: VariablePtr); virtual;

   procedure Out_TermList ( aTrmList:PList ); virtual;
   procedure Out_Adjective(aAttr:AdjectiveExpressionPtr ); virtual;
   procedure Out_AdjectiveList( aCluster: PList ); virtual;
   procedure Out_Type ( aTyp: TypePtr ); virtual;
   procedure Out_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Out_VariableSegment( aSegm: QualifiedSegmentPtr); virtual;
   procedure Out_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr ); virtual;
   procedure Out_Formula ( aFrm:FormulaPtr ); virtual;
   procedure Out_Term ( aTrm: TermPtr ); virtual;
   procedure Out_SimpleTerm ( aTrm: SimpleTermPtr ); virtual;
   procedure Out_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr ); virtual;
   procedure Out_InternalSelectorTerm ( aTrm: InternalSelectorTermPtr ); virtual;

   procedure Out_TypeList ( aTypeList: PList ); virtual;

   procedure Out_Locus( aLocus: LocusPtr); virtual;
   procedure Out_Loci( aLoci: PList); virtual;
   procedure Out_Pattern(aPattern: PatternPtr); virtual;

   procedure Out_Label(aLab:LabelPtr); virtual;
   procedure Out_Definiens(aDef:DefiniensPtr); virtual;

   procedure Out_ReservationSegment(aRes:ReservationSegmentPtr); virtual;
   procedure Out_SchemeNameInSchemeHead(aSch: SchemePtr); virtual;
   procedure Out_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Out_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Out_Proposition(aProp:PropositionPtr); virtual;
   procedure Out_LocalReference(aRef: LocalReferencePtr); virtual;
   procedure Out_References(aRefs: PList); virtual;
   procedure Out_Link(aInf: JustificationPtr); virtual;
   procedure Out_SchemeJustification(aInf: SchemeJustificationPtr); virtual;
   procedure Out_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr); virtual;
  end;

 InWSMizFilePtr = ^InWSMizFileObj;
 InWSMizFileObj =
  object(XMLInStreamObj)
     nDisplayInformationOnScreen: boolean;

   constructor OpenFile(const aFileName:string );
   destructor Done; virtual;

   function GetAttrValue(const aAttrName:string): string;
   function GetAttrPos: Position;

   function Read_TextProper: wsTextProperPtr; virtual;
   function Read_Block: wsBlockPtr; virtual;
   function Read_Item: wsItemPtr; virtual;

   procedure Read_ItemContentsAttr(aItem: wsItemPtr; var aShape: string); virtual;
   procedure Read_ItemContents(aItem: wsItemPtr; const aShape: string); virtual;

   function Read_TermList:PList; virtual;
   function Read_Adjective:AdjectiveExpressionPtr; virtual;
   function Read_AdjectiveList: PList; virtual;
   function Read_Type: TypePtr; virtual;
   function Read_Variable: VariablePtr; virtual;
   function Read_ImplicitlyQualifiedSegment: ImplicitlyQualifiedSegmentPtr; virtual;
   function Read_VariableSegment: QualifiedSegmentPtr; virtual;
   function Read_PrivatePredicativeFormula:PrivatePredicativeFormulaPtr; virtual;
   function Read_Formula:FormulaPtr; virtual;
   function Read_SimpleTerm: SimpleTermPtr; virtual;
   function Read_PrivateFunctorTerm: PrivateFunctorTermPtr; virtual;
   function Read_InternalSelectorTerm: InternalSelectorTermPtr; virtual;
   function Read_Term: TermPtr; virtual;

   function Read_TypeList: PList; virtual;

   function Read_Locus: LocusPtr; virtual;
   function Read_Loci: PList; virtual;

   function Read_ModePattern: ModePatternPtr; virtual;
   function Read_AttributePattern: AttributePatternPtr; virtual;
   function Read_FunctorPattern: FunctorPatternPtr; virtual;
   function Read_PredicatePattern: PredicatePatternPtr; virtual;
   function Read_Pattern: PatternPtr; virtual;

   function Read_Definiens: DefiniensPtr; virtual;

   function Read_ReservationSegment: ReservationSegmentPtr; virtual;
   function Read_SchemeNameInSchemeHead: SchemePtr; virtual;
   function Read_Label: LabelPtr; virtual;
   function Read_Proposition: PropositionPtr; virtual;
   function Read_CompactStatement: CompactStatementPtr; virtual;
   function Read_LocalReference: LocalReferencePtr; virtual;
   function Read_References: PList; virtual;
   function Read_StraightforwardJustification: StraightforwardJustificationPtr; virtual;
   function Read_SchemeJustification: SchemeJustificationPtr; virtual;
   function Read_Justification: JustificationPtr; virtual;
   function Read_RegularStatement(const aShape: string): RegularStatementPtr; virtual;
  end;

 WSMizarPrinterPtr =  ^WSMizarPrinterObj;
 WSMizarPrinterObj =
  object(TXTStreamObj)
    nDisplayInformationOnScreen: boolean;
    nIndent:	integer; 	// indenting
   constructor OpenFile(const aFileName:string );
   destructor Done; virtual;

   procedure Print_Char( AChar: char );
   procedure Print_NewLine;
   procedure Print_Number( const aNumber: integer);
   procedure Print_String( const aString: string);
//   procedure Print_Spaces(aNbr:integer);
   procedure Print_Indent;

   procedure Print_TextProper(aWSTextProper:WSTextProperPtr); virtual;
   procedure Print_Item(aWSItem:WSItemPtr); virtual;
   procedure Print_SchemeNameInSchemeHead(aSch: SchemePtr); virtual;
   procedure Print_Block(aWSBlock:WSBlockPtr); virtual;

   procedure Print_Adjective(aAttr:AdjectiveExpressionPtr ); virtual;
   procedure Print_AdjectiveList( aCluster: PList ); virtual;
   procedure Print_Variable( aVar: VariablePtr); virtual;
   procedure Print_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr); virtual;
   procedure Print_VariableSegment( aSegm: QualifiedSegmentPtr); virtual;
   procedure Print_Type ( aTyp: TypePtr ); virtual;
   procedure Print_BinaryFormula ( aFrm:BinaryFormulaPtr ); virtual;
   procedure Print_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr ); virtual;
   procedure Print_Formula ( aFrm:FormulaPtr ); virtual;
   procedure Print_OpenTermList ( aTrmList:PList ); virtual;
   procedure Print_TermList ( aTrmList:PList ); virtual;
   procedure Print_SimpleTermTerm ( aTrm: SimpleTermPtr ); virtual;
   procedure Print_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr ); virtual;
   procedure Print_Term ( aTrm: TermPtr ); virtual;

   procedure Print_TypeList ( aTypeList: PList ); virtual;


   procedure Print_Label(aLab:LabelPtr); virtual;

   procedure Print_Reference(aRef: LocalReferencePtr); virtual;
   procedure Print_References(aRefs: PList); virtual;
   procedure Print_StraightforwardJustification(aInf: StraightforwardJustificationPtr); virtual;
   procedure Print_SchemeNameInJustification(aInf: SchemeJustificationPtr); virtual;
   procedure Print_SchemeJustification(aInf: SchemeJustificationPtr); virtual;
   procedure Print_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr); virtual;
   procedure Print_Linkage; virtual;
   procedure Print_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Print_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Print_Proposition(aProp:PropositionPtr); virtual;

   procedure Print_Conditions(aCond: PList);
   procedure Print_AssumptionConditions(aCond: AssumptionPtr); virtual;

   procedure Print_Pattern(aPattern: PatternPtr); virtual;
   procedure Print_Locus( aLocus: LocusPtr); virtual;
   procedure Print_Loci( aLoci: PList); virtual;
   procedure Print_Definiens(aDef:DefiniensPtr); virtual;

   procedure Print_ReservedType(aResType: TypePtr); virtual;
  end;

const

  BlockName: array[BlockKind] of string =
   (
    {blMain}          'Text-Proper',
    {blDiffuse}       'Now-Reasoning',
    {blHereby}        'Hereby-Reasoning',
    {blProof}         'Proof',
    {blDefinition}    'Definitional-Block',
    {blNotation}      'Notation-Block',
    {blRegistration}  'Registration-Block',
    {blCase}          'Case',
    {blSuppose}       'Suppose',
    {blPublicScheme}  'Scheme-Block'
   );

  ItemName: array[ItemKind] of string =
  ({itIncorrItem}         'Incorrect-Item',
   {itDefinition}         'Definition-Item',
   {itSchemeBlock}        'Scheme-Block-Item',
   {itSchemeHead}         'Scheme-Head',
   {itTheorem}            'Theorem-Item',
   {itAxiom}              'Axiom-Item',
   {itReservation}        'Reservation',
   {itCanceled}           'canceled-pragmma',
   {itSection}            'Section-Pragma',
   {itRegularStatement}   'Regular-Statement',
   {itChoice}             'Choice-Statement',
   {itReconsider}         'Type-Changing-Statement',
   {itPrivFuncDefinition} 'Private-Functor-Definition',
   {itPrivPredDefinition} 'Private-Predicate-Definition',
   {itConstantDefinition} 'Constant-Definition',
   {itGeneralization}     'Generalization',
   {itLociDecraration}    'Loci-Declaration',
   {itExistentialAssumption} 'Existential-Assumption',
   {itExemplification}    'Exemplification',
   {itPerCases}           'Per-Cases',
   {itConclusion}         'Conclusion',
   {itCaseBlock}          'Case-Block',
   {itCaseHead}           'Case-Head',
   {itSupposeHead}        'Suppose-Head',
   {itAssumption}         'Assumption',
   {itCorrCond}           'Correctness-Condition',
   {itCorrectness}        'Correctness',
   {itProperty}           'Property',
   {itDefPred}            'Predicate-Definition',
   {itDefFunc}            'Functor-Definition',
   {itDefMode}            'Mode-Definition',
   {itDefAttr}            'Attribute-Definition',
   {itDefStruct}          'Structure-Definition',
   {itPredSynonym}        'Pred-Synonym',
   {itPredAntonym}        'Pred-Antonym',
   {itFuncNotation}       'Func-Synonym',
   {itModeNotation}       'Mode-Synonym',
   {itAttrSynonym}        'Attr-Synonym',
   {itAttrAntonym}        'Attr-Antonym',
   {itCluster}            'Cluster',
   {itIdentify}           'Identify',
   {itReduction}          'Reduction',
   {itPropertyRegistration} 'Property-Registration',
   {itPragma}             'Pragma'
  );

  ExpName: array[ExpKind] of string =
   ( 'Null-Expression',
     'Type-Expression',
     'Term-Expression',
     'Formula-Expression',
     'Reservation-Type',
     'Adjective-Cluster'
    );

  SegmentKindName: array[SegmentKind] of string  =
   ( 'Implicitly-Qualified-Segment',
     'Explicitly-Qualified-Segment');


 TypeName:  array[TypeSort] of string =
   ( 'Type-Error',
     'Standard-Type',
     'Struct-Type',
     'Clustered-Type',
     'ReservedDscr-Type'
   );

  TermName: array[TermSort] of string =
   (  'Term-Error',
      'Placeholder-Term',
      'Numeral-Term',
      'Simple-Term',
      'Private-Functor-Term',
      'Infix-Term',
      'Circumfix-Term',
      'Aggregate-Term',
      'Forgetful-Functor-Term',
      'Internal-Forgetful-Functor-Term',
      'Selector-Term',
      'Internal-Selector-Term',
      'Qualification-Term',
      'Global-Choice-Term',
      'Simple-Fraenkel-Term',
      'Fraenkel-Term',
      'it-Term',
      'Exactly-Qualification-Term'
   );

  FormulaName: array[FormulaSort] of string =
   ( 'Formula-Error',
     'Thesis',
     'Contradiction',
     'RightSideOf-Predicative-Formula',
     'Predicative-Formula',
     'Multi-Predicative-Formula',
     'Private-Predicate-Formula',
     'Attributive-Formula',
     'Qualifying-Formula',
     'Universal-Quantifier-Formula',
     'Existential-Quantifier-Formula',
     'Negated-Formula',
     'Conjunctive-Formula',
     'Disjunctive-Formula',
     'Conditional-Formula',
     'Biconditional-Formula',
     'FlexaryConjunctive-Formula',
     'FlexaryDisjunctive-Formula'
   );

   AdjectiveSortName: array[AdjectiveSort] of string =
    ( 'NegatedAdjective',
      'Adjective'
    );


 ReferenceKindName: array[ReferenceKind] of string =
  ( 'Local-Reference',
    'Theorem-Reference',
    'Definition-Reference'
  );

 InferenceName: array[InferenceKind] of string =
  ( 'Inference-Error',
    'Straightforward-Justification',
    'Scheme-Justification',
    'Proof',
    'SkippedProof'
  );

 RegularStatementName: array[RegularStatementKind] of string  =
  ( 'Diffuse-Statement',
    'Compact-Statement',
    'Iterative-Equality'
  );

 ClusterRegistrationName: array[ClusterRegistrationKind] of string =
  ( 'Existential-Registration',
    'Conditional-Registration',
    'Functorial-Registration'
  );

 ModeDefinitionSortName: array[ModeDefinitionSort] of string =
  ( 'Expandable-Mode',
    'Standard-Mode'
  );

 DefiningWayName: array[HowToDefine] of string =
  ( 'No-Definiens',
    'Means',
    'Equals'
  );

 DefiniensKindName: array[DefiniensSort] of string =
  ( 'Simple-Definiens',
     'Conditional-Definiens'
  );


 DefPatternName: array[itDefPred..itDefStruct] of string =
  ( 'Predicate-Pattern',
    'Functor-Pattern',
    'Mode-Pattern',
    'Attribute-Pattern',
    'Structure-Pattern'
  );

 FunctorPatternName: array[FunctorSort] of string =
  ( 'Operation-Functor-Pattern',
    'Bracket-Functor-Pattern'
  );

  AssumptionKindName: array[AssumptionKind] of string =
   ( 'Single-Assumption',
     'Collective-Assumption',
     'Existential-Assumption'
   );

  SchemeSegmentName: array[SchemeSegmentKind] of string =
   ( 'Predicate-Segment',
     'Functor-Segment'
   );

function IdentRepr(aIdNr:integer):string;
procedure InitWSLookupTables;
procedure DisposeWSLookupTables;

procedure InitScannerNames;

procedure Write_WSMizArticle(aWSTextProper:wsTextProperPtr; aFileName:string);

function Read_WSMizArticle(aFileName:string): wsTextProperPtr;

procedure Print_WSMizArticle(aWSTextProper:wsTextProperPtr; aFileName:string);

var
 IdentifierName,AttributeName,StructureName,ModeName,PredicateName,FunctorName,
 SelectorName,LeftBracketName,RightBracketName,MMLIdentifierName: array of string;

implementation

uses mizenv, mconsole, librenv, scanner, xml_parser
{$IFDEF MDEBUG} ,info {$ENDIF};

constructor wsTextProper.Init(const aArticleID, aArticleExt: string; const aPos:Position);
begin
  inherited Init(blMain,aPos);
  nArticleID:=aArticleID;
  nArticleExt:=aArticleExt;
end;

destructor wsTextProper.Done;
begin
  inherited Done;
end;

function wsTextProper.NewBlock(aBlockKind: BlockKind; const aPos:Position): wsBlockPtr;
begin
 result:=new(WSBlockPtr,Init(aBlockKind,CurPos));
end;

function wsTextProper.NewItem(aItemKind:ItemKind; const aPos:Position): wsItemPtr;
begin
 result:=new(wsItemPtr,Init(aItemKind,CurPos));
end;

constructor wsBlock.Init(aBlokKind: BlockKind; const aPos:Position);
begin
  nBlockKind:=aBlokKind;
  nBlockPos:=aPos;
  nBlockEndPos:=aPos;
  nItems:=New(PList,Init(0));
end;

destructor wsBlock.Done;
begin
  dispose(nItems,Done);
  inherited Done;
end;

constructor wsItem.Init(aItemKind: ItemKind; const aPos:Position);
begin
  nItemKind:=aItemKind;
  nItemPos:=aPos;
  nItemEndPos:=aPos;
  nContent:=nil;
  nBlock:=nil;
end;

destructor wsItem.Done;
begin
  if nBlock <> nil then dispose(nBlock,Done);
  inherited Done;
end;

constructor PragmaObj.Init(aStr: string);
begin
  nPragmaStr:=aStr;
end;

constructor LabelObj.Init(aLabelId: integer; const aPos:Position);
begin
 nLabelIdNr:=aLabelId;
 nLabelPos:=aPos;
end;

constructor PropositionObj.Init(alab:LabelPtr; aSentence: FormulaPtr; const aSntPos:Position);
begin
 nLab:=aLab;
 nSntPos:=aSntPos;
 nSentence:=aSentence;
end;

destructor PropositionObj.Done;
begin
 dispose(nLab,Done);
 dispose(nSentence,Done);
end;

constructor LocalReferenceObj.Init(aLabId:integer; const aPos:Position);
begin
 nRefSort:=LocalReference;
 nLabId:= aLabId;
 nRefPos:=aPos
end;

constructor TheoremReferenceObj.Init(aArticleNr,aTheoNr:integer; const aPos:Position);
begin nRefSort:=TheoremReference;
 nArticleNr:=aArticleNr;
 nTheoNr:=aTheoNr;
 nRefPos:=aPos
end;

constructor DefinitionReferenceObj.Init(aArticleNr,aDefNr:integer; const aPos:Position);
begin nRefSort:=DefinitionReference;
 nArticleNr:=aArticleNr;
 nDefNr:=aDefNr;
 nRefPos:=aPos
end;

constructor JustificationObj.Init(aInferSort:InferenceKind; const aPos: Position);
begin
 nInfSort:=aInferSort;
 nInfPos:=aPos;
end;

constructor SimpleJustificationObj.Init(aInferSort:InferenceKind; const aPos: Position);
begin
 inherited Init(aInferSort,aPos);
 nReferences:=new(Plist,Init(0));
end;

destructor SimpleJustificationObj.Done;
begin dispose(nReferences,Done);
 inherited Done;
end;

constructor StraightforwardJustificationObj.Init(const aPos:Position;
                                             aLinked:boolean; const aLinkPos:Position);
begin
 inherited Init(infStraightforwardJustification,aPos);
 nLinked:=aLinked;
 nLinkPos:=aLinkPos;
end;

destructor StraightforwardJustificationObj.Done;
begin
 inherited Done;
end;

constructor SchemeJustificationObj.Init(const aPos:Position; aArticleNr,aNr:integer);
begin
 inherited Init(infSchemeJustification,aPos);
 nSchFileNr:=aArticleNr;
 nSchemeIdNr:=aNr;
 nSchemeInfPos:=aPos;
end;

destructor SchemeJustificationObj.Done;
begin
 inherited Done;
end;

constructor SchemeSegmentObj.Init(const aPos:Position; aSegmSort:SchemeSegmentKind;
                   aVars,aTypeExpList: PList);
begin
 nSegmPos:=aPos;
 nSegmSort:=aSegmSort;
 nVars:=aVars;
 nTypeExpList:=aTypeExpList;
end;

destructor SchemeSegmentObj.Done;
begin
 dispose(nVars,Done);
 dispose(nTypeExpList,Done);
end;

constructor FunctorSegmentObj.Init(const aPos:Position;
                   aVars,aTypeExpList: PList; aSpecification: TypePtr);
begin
 inherited Init(aPos,FunctorSegment,aVars,aTypeExpList);
 nSpecification:=aSpecification;
end;

destructor FunctorSegmentObj.Done;
begin
 dispose(nSpecification,Done);
 inherited Done;
end;

constructor SchemeObj.Init(aIdNr:integer; const aPos:Position; aParams:PList;
                            aPrems:PList; aConcl:FormulaPtr);
begin
 nSchemeIdNr:=aIdNr;
 nSchemePos:=aPos;
 nSchemeParams:=aParams;
 nSchemeConclusion:=aConcl;
 nSchemePremises:=aPrems;
end;

destructor SchemeObj.Done;
begin
 dispose(nSchemeParams,Done);
 dispose(nSchemeConclusion,Done);
 dispose(nSchemePremises,Done);
end;

constructor ReservationSegmentObj.Init(aIdentifiers:PList; aType:TypePtr);
begin
  nIdentifiers:=aIdentifiers;
  nResType:=aType;
end;

destructor ReservationSegmentObj.Done;
begin
  dispose(nIdentifiers,Done);
  dispose(nResType,Done);
end;

constructor PrivateFunctorDefinitionObj.Init(aFuncId:VariablePtr; aTypeExpList:Plist; aTerm:TermPtr);
begin
 nFuncId:=aFuncId;
 nTypeExpList:=aTypeExpList;
 nTermExpr:=aTerm;
end;

destructor PrivateFunctorDefinitionObj.Done;
begin
 dispose(nFuncId,Done);
 dispose(nTypeExpList,Done);
 dispose(nTermExpr,Done);
end;

constructor PrivatePredicateDefinitionObj.Init(aPredId:VariablePtr; aTypeExpList:Plist; aSnt:FormulaPtr);
begin
 nPredId:=aPredId;
 nTypeExpList:=aTypeExpList;
 nSentence:=aSnt;
end;

destructor PrivatePredicateDefinitionObj.Done;
begin
 dispose(nPredId,Done);
 dispose(nTypeExpList,Done);
 dispose(nSentence,Done);
end;

constructor ConstantDefinitionObj.Init(aVarId:VariablePtr; aTerm:TermPtr);
begin
 nVarId:=aVarId;
 nTermExpr:=aTerm;
end;

destructor ConstantDefinitionObj.Done;
begin
 dispose(nVarId,Done);
 dispose(nTermExpr,Done);
end;

constructor TypeChangeObj.Init(aKind:TypeChangeSort; aVar:VariablePtr; aTerm:TermPtr);
begin
 nTypeChangeKind:=aKind;
 nVar:=aVar;
 nTermExpr:=aTerm;
end;

destructor TypeChangeObj.Done;
begin
 dispose(nVar,Done);
 if nTermExpr <> nil then
   dispose(nTermExpr,Done);
end;

constructor ExampleObj.Init(aVarId:VariablePtr; aTerm:TermPtr);
begin
 nVarId:=aVarId;
 nTermExpr:=aTerm;
end;

destructor ExampleObj.Done;
begin
 if nVarId <> nil then dispose(nVarId,Done);
 if nTermExpr <> nil then dispose(nTermExpr,Done);
end;

constructor TypeChangingStatementObj.Init(aTypeChangeList: PList; aTypeExpr: TypePtr;
                                          aJustification:SimpleJustificationPtr);
begin
 nTypeChangeList:=aTypeChangeList;
 nTypeExpr:=aTypeExpr;
 nJustification:=aJustification;
end;

destructor TypeChangingStatementObj.Done;
begin
 dispose(nTypeChangeList,Done);
 dispose(nTypeExpr,Done);
 dispose(nJustification,Done);
end;

constructor ChoiceStatementObj.Init(aQualVars,aConds:PList; aJustification:SimpleJustificationPtr);
begin
 nQualVars:=aQualVars;
 nConditions:=aConds;
 nJustification:=aJustification;
end;

destructor ChoiceStatementObj.Done;
begin
 dispose(nQualVars,Done);
 dispose(nConditions,Done);
 dispose(nJustification,Done);
end;

constructor RegularStatementObj.Init(aStatementSort: RegularStatementKind);
begin
 nStatementSort:=aStatementSort;
end;

destructor RegularStatementObj.Done;
begin
 inherited Done;
end;

constructor DiffuseStatementObj.Init(aLab: LabelPtr; aStatementSort: RegularStatementKind);
begin
 inherited Init(stDiffuseStatement);
 nLab:=aLab;
 nStatementSort:=aStatementSort;
end;

destructor DiffuseStatementObj.Done;
begin
 dispose(nLab,Done);
end;

constructor CompactStatementObj.Init(aProp:PropositionPtr; aJustification:JustificationPtr);
begin
 inherited Init(stCompactStatement);
 nProp:=aProp;
 nJustification:=aJustification;
end;

destructor CompactStatementObj.Done;
begin
 if nJustification <> nil then dispose(nJustification,Done);
 inherited Done;
end;

constructor IterativeStepObj.Init(const aPos:Position; aTerm: TermPtr; aJustification:JustificationPtr);
begin
 nIterPos:=aPos;
 nTerm:=aTerm;
 nJustification:=SimpleJustificationPtr(aJustification);
end;

destructor IterativeStepObj.Done;
begin
 dispose(nTerm,Done);
 dispose(nJustification,Done);
end;

constructor IterativeEqualityObj.Init(aProp:PropositionPtr;aJustification:JustificationPtr; aIters: PList);
begin
 inherited Init(aProp,aJustification);
 nStatementSort:=stIterativeEquality;
 nIterSteps:=aIters;
end;

destructor IterativeEqualityObj.Done;
begin
 dispose(nIterSteps,Done);
 inherited Done;
end;

constructor FieldSymbolObj.Init(const aPos:Position; aFieldSymbNr:integer);
begin
 nFieldPos:=aPos;
 nFieldSymbol:=aFieldSymbNr;
end;

constructor FieldSegmentObj.Init(const aPos:Position; aFields:PList; aSpec:TypePtr);
begin
  nFieldSegmPos:=aPos;
  nFields:=aFields;
  nSpecification:=aSpec;
end;

destructor FieldSegmentObj.Done;
begin
 dispose(nFields,Done);
 dispose(nSpecification,Done);
end;

constructor LocusObj.Init(const aPos:Position; aIdentNr:integer);
begin
 nVarId:=aIdentNr;
 nVarIdPos:=aPos;
end;

constructor StructureDefinitionObj.Init(const aPos:Position; aAncestors:PList;
                     aStructSymb:integer; aOverArgs:PList; aFields:PList);
begin
 nStrPos:=aPos;
 nAncestors:=aAncestors;
 nDefStructPattern:=new(ModePatternPtr,Init(aPos,aStructSymb,aOverArgs));
 nDefStructPattern^.nPatternSort:=itDefStruct;
 nSgmFields:=aFields;
end;

destructor StructureDefinitionObj.Done;
begin
 dispose(nAncestors,Done);
 dispose(nDefStructPattern,Done);
 dispose(nSgmFields,Done);
end;

constructor PatternObj.Init(const aPos:Position; aSort:ItemKind);
begin
 nPatternPos:=aPos;
 nPatternSort:=aSort;
end;

constructor ModePatternObj.Init(const aPos:Position; aSymb:integer; aArgs:PList);
begin
 inherited Init(aPos,itDefMode);
 nModeSymbol:=aSymb;
 nArgs:=aArgs;
end;

destructor ModePatternObj.Done;
begin
 dispose(nArgs,Done);
end;

constructor AttributePatternObj.Init(const aPos:Position; aArg:LocusPtr; aSymb:integer; aArgs:PList);
begin
 inherited Init(aPos,itDefAttr);
 nAttrSymbol:=aSymb;
 nArg:=aArg;
 nArgs:=aArgs;
end;

destructor AttributePatternObj.Done;
begin
 dispose(nArg,Done);
 dispose(nArgs,Done);
end;

constructor PredicatePatternObj.Init(const aPos:Position; aLArgs:PList; aSymb:integer; aRArgs:PList);
begin
 inherited Init(aPos,itDefPred);
 nPredSymbol:=aSymb;
 nLeftArgs:=aLArgs;
 nRightArgs:=aRArgs;
end;

destructor PredicatePatternObj.Done;
begin
 dispose(nLeftArgs,Done);
 dispose(nRightArgs,Done);
end;

constructor FunctorPatternObj.Init(const aPos:Position; aKind:FunctorSort);
begin
 inherited Init(aPos,itDefFunc);
 nFunctKind:=aKind;
end;

constructor CircumfixFunctorPatternObj.Init(const aPos:Position; aLBSymb,aRBSymb:integer; aArgs:PList);
begin
 inherited Init(aPos,CircumfixFunctor);
 nLeftBracketSymb:=aLBSymb;
 nRightBracketSymb:=aRBSymb;
 nArgs:=aArgs;
end;

destructor CircumfixFunctorPatternObj.Done;
begin
 dispose(nArgs,Done);
end;

constructor InfixFunctorPatternObj.Init(const aPos:Position; aLArgs:PList; aSymb:integer; aRArgs:PList);
begin
 inherited Init(aPos,InfixFunctor);
 nOperSymb:=aSymb;
 nLeftArgs:=aLArgs;
 nRightArgs:=aRArgs;
end;

destructor InfixFunctorPatternObj.Done;
begin
 dispose(nLeftArgs,Done);
 dispose(nRightArgs,Done);
end;

constructor DefiniensObj.Init(const aPos: Position; aLab:LabelPtr; aKind:DefiniensSort);
begin
  nDefSort:=aKind;
  nDefPos:=aPos;
  nDefLabel:=aLab;
end;

destructor DefiniensObj.Done;
begin
 if nDefLabel <> nil then
   dispose(nDefLabel,Done);
end;

constructor DefExpressionObj.Init(aKind:ExpKind; aExpr:Pobject);
begin
 nExprKind:=aKind;
 nExpr:=aExpr;
end;

destructor DefExpressionObj.Done;
begin
 dispose(nExpr,Done);
end;

constructor SimpleDefiniensObj.Init(const aPos:Position; aLab:LabelPtr; aDef:DefExpressionPtr);
begin
 inherited Init(aPos,aLab,SimpleDefiniens);
 nExpression:=aDef;
end;

destructor SimpleDefiniensObj.Done;
begin
 dispose(nExpression,Done);
 inherited Done;
end;

constructor PartDefObj.Init(aPartDef:DefExpressionPtr; aGuard:FormulaPtr);
begin
 nGuard:=aGuard;
 nPartDefiniens:=aPartDef;
end;

destructor PartDefObj.Done;
begin dispose(nPartDefiniens,Done);
 dispose(nGuard,Done);
end;

constructor ConditionalDefiniensObj.Init(const aPos:Position; aLab:LabelPtr; aPartialDefs:PList; aOtherwise:DefExpressionPtr);
begin
 inherited Init(aPos,aLab,ConditionalDefiniens);
 nConditionalDefiniensList:=aPartialDefs;
 nOtherwise:=aOtherwise;
end;

destructor ConditionalDefiniensObj.Done;
begin
 if nOtherwise <> nil then dispose(nOtherwise,Done);
 dispose(nConditionalDefiniensList,Done);
 inherited Done;
end;

constructor ModeDefinitionObj.Init(const aPos: Position; aDefKind:ModeDefinitionSort;
                      aRedef: boolean; aPattern: ModePatternPtr);
begin
 nDefKind:=aDefKind;
 nDefModePos:=aPos;
 nRedefinition:=aRedef;
 nDefModePattern:=aPattern;
end;

destructor ModeDefinitionObj.Done;
begin
 dispose(nDefModePattern,Done);
end;

constructor ExpandableModeDefinitionObj.Init(const aPos: Position; aPattern: ModePatternPtr; aExp:TypePtr);
begin
  inherited Init(aPos,defExpandableMode,false,aPattern);
  nExpansion:=aExp;
end;

destructor ExpandableModeDefinitionObj.Done;
begin
 dispose(nExpansion,Done);
 inherited Done;
end;

constructor StandardModeDefinitionObj.Init(const aPos: Position;  aRedef: boolean;
                              aPattern: ModePatternPtr; aSpec:TypePtr;
                              aDef:DefiniensPtr);
begin
  inherited Init(aPos,defStandardMode,aRedef,aPattern);
  nSpecification:=aSpec;
  nDefiniens:=aDef;
end;

destructor StandardModeDefinitionObj.Done;
begin
 dispose(nSpecification,Done);
 dispose(nDefiniens,Done);
 inherited Done;
end;

constructor AttributeDefinitionObj.Init(const aPos: Position;  aRedef: boolean;
                                        aPattern:AttributePatternPtr; aDef:DefiniensPtr);
begin
 nDefAttrPos:=aPos;
 nRedefinition:=aRedef;
 nDefAttrPattern:=aPattern;
 nDefiniens:=aDef;
end;

destructor AttributeDefinitionObj.Done;
begin
 dispose(nDefAttrPattern,Done);
 dispose(nDefiniens,Done);
end;

constructor PredicateDefinitionObj.Init(const aPos: Position;  aRedef: boolean;
                                        aPattern:PredicatePatternPtr; aDef:DefiniensPtr);
begin
 nDefPredPos:=aPos;
 nRedefinition:=aRedef;
 nDefPredPattern:=aPattern;
 nDefiniens:=aDef;
end;

destructor PredicateDefinitionObj.Done;
begin
 dispose(nDefPredPattern,Done);
 dispose(nDefiniens,Done);
end;

constructor FunctorDefinitionObj.Init(const aPos: Position;  aRedef: boolean;
                                        aPattern:FunctorPatternPtr; aSpec: TypePtr;
                                        aDefWay:HowToDefine; aDef:DefiniensPtr);
begin
 nDefFuncPos:=aPos;
 nRedefinition:=aRedef;
 nDefFuncPattern:=aPattern;
 nSpecification:=aSpec;
 nDefiningWay:=aDefWay;
 nDefiniens:=aDef;
end;

destructor FunctorDefinitionObj.Done;
begin
 dispose(nDefFuncPattern,Done);
 dispose(nDefiniens,Done);
end;

constructor NotationDeclarationObj.Init(const aPos:Position; aNSort:ItemKind; aNewPatt,aOrigPatt:PatternPtr);
begin
 nNotationPos:=aPos;
 nNotationSort:=aNSort;
 nOriginPattern:=aOrigPatt;
 nNewPattern:=aNewPatt;
end;

destructor NotationDeclarationObj.Done;
begin
 dispose(nOriginPattern,Done);
 dispose(nNewPattern,Done);
end;

constructor AssumptionObj.Init(const aPos:Position; aSort:AssumptionKind);
begin
  nAssumptionPos:=aPos;
  nAssumptionSort:=aSort;
end;

constructor SingleAssumptionObj.Init(const aPos:Position; aProp:PropositionPtr);
begin
  inherited Init(aPos,SingleAssumption);
  nProp:=aProp;
end;

destructor SingleAssumptionObj.Done;
begin
 dispose(nProp,Done);
end;

constructor CollectiveAssumptionObj.Init(const aPos:Position; aProps:PList);
begin
  inherited Init(aPos,CollectiveAssumption);
  nConditions:=aProps;
end;

destructor CollectiveAssumptionObj.Done;
begin
 dispose(nConditions,Done);
end;

constructor ExistentialAssumptionObj.Init(const aPos:Position; aQVars,aProps:PList);
begin
  AssumptionObj.Init(aPos,CollectiveAssumption);
  nConditions:=aProps;
  nQVars:=aQVars;
end;

destructor ExistentialAssumptionObj.Done;
begin
 dispose(nQVars,Done);
 inherited Done;
end;

constructor CorrectnessObj.Init(const aPos:Position; aJustification:JustificationPtr);
begin
 nCorrCondPos:=aPos;
 nJustification:=aJustification;
end;

destructor CorrectnessObj.Done;
begin
 dispose(nJustification,Done);
end;

constructor CorrectnessConditionObj.Init(const aPos:Position; aSort:CorrectnessKind;
                    aJustification:JustificationPtr);
begin
 inherited Init(aPos,aJustification);
 nCorrCondSort:=aSort;
end;

destructor CorrectnessConditionObj.Done;
begin
 inherited Done;
end;

constructor CorrectnessConditionsObj.Init(const aPos:Position; const aConds: CorrectnessConditionsSet;
                    aJustification:JustificationPtr);
begin
 inherited Init(aPos,aJustification);
 nConditions:=aConds;
end;

destructor CorrectnessConditionsObj.Done;
begin
 inherited Done;
end;

constructor PropertyObj.Init(const aPos:Position; aSort:PropertyKind;
                    aJustification:JustificationPtr);
begin
 nPropertyPos:=aPos;
 nPropertySort:=aSort;
 nJustification:=aJustification;
end;

destructor PropertyObj.Done;
begin
 inherited Done;
end;

constructor ClusterObj.Init(const aPos: Position; aKind:ClusterRegistrationKind; aCons:PList; aTyp:TypePtr);
begin
 nClusterPos:=aPos;
 nClusterKind:=aKind;
 nConsequent:=aCons;
 nClusterType:=aTyp;
end;

destructor ClusterObj.Done;
begin
 dispose(nConsequent,Done);
end;

constructor EClusterObj.Init(const aPos: Position; aCons:PList; aTyp:TypePtr);
begin
 ClusterObj.Init(aPos,ExistentialRegistration,aCons,aTyp);
end;

destructor EClusterObj.Done;
begin
 if nClusterType <> nil then dispose(nClusterType,Done);
 inherited Done;
end;

constructor CClusterObj.Init(const aPos:Position; aAntec,aCons:PList; aTyp:TypePtr);
begin
 ClusterObj.Init(aPos,ConditionalRegistration,aCons,aTyp);
 nAntecedent:=aAntec;
end;

destructor CClusterObj.Done;
begin
 dispose(nAntecedent,Done);
 inherited Done;
end;

constructor FClusterObj.Init(const aPos: Position; aTrm:TermPtr; aCons:PList; aTyp:TypePtr);
begin
 ClusterObj.Init(aPos,FunctorialRegistration,aCons,aTyp);
 nClusterTerm:=aTrm;
end;

destructor FClusterObj.Done;
begin
 if nClusterTerm <> nil then Dispose(nClusterTerm,Done);
 if nClusterType <> nil then dispose(nClusterType,Done);
 inherited Done;
end;

constructor LociEqualityObj.Init(const aPos:Position; aLeftLocus,aRightLocus:LocusPtr);
begin
 nEqPos:=aPos;
 nLeftLocus:=aLeftLocus;
 nRightLocus:=aRightLocus;
end;

destructor LociEqualityObj.Done;
begin
 Dispose(nLeftLocus,Done);
 dispose(nRightLocus,Done);
end;

constructor IdentifyRegistrationObj.Init(const aPos:Position; aNewPatt,aOrigPatt:PatternPtr; aEqList:PList);
begin
 nIdentifyPos:=aPos;
 nOriginPattern:=aOrigPatt;
 nNewPattern:=aNewPatt;
 nEqLociList:=aEqList;
end;

destructor IdentifyRegistrationObj.Done;
begin
 dispose(nOriginPattern,Done);
 dispose(nNewPattern,Done);
 if nEqLociList <> nil then
  dispose(nEqLociList,Done);
end;

constructor PropertyRegistrationObj.Init(const aPos:Position; aKind:PropertyKind);
begin
 nPropertyPos:=aPos;
 nPropertySort:=aKind;
end;

destructor PropertyRegistrationObj.Done;
begin
end;

constructor SethoodRegistrationObj.Init(const aPos:Position; aKind:PropertyKind; aType:TypePtr);
begin inherited Init(aPos,aKind);
 nSethoodType:=aType;
 nJustification:=nil;
end;

destructor SethoodRegistrationObj.Done;
begin
// if nSethoodType <> nil then
 dispose(nSethoodType,Done);
// if nJustification <> nil  then
 dispose(nJustification,Done);
 inherited Done;
end;

constructor ReduceRegistrationObj.Init(const aPos:Position; aOrigTerm,aNewTerm:TermPtr);
begin
 nReducePos:=aPos;
 nOriginTerm:=aOrigTerm;
 nNewTerm:=aNewTerm;
end;

destructor ReduceRegistrationObj.Done;
begin
 dispose(nOriginTerm,Done);
 dispose(nNewTerm,Done);
end;

(********** OutXVRFFileObj **********)

function CapitalizeName(aName: string): string;
begin
 result:=aName;
 if aName[1] in ['a'..'z'] then
  dec(Result[1], ord('a') - ord('A'))
end;

function UncapitalizeName(aName: string): string;
begin
 result:=aName;
 if aName[1] in ['A'..'Z'] then
  inc(Result[1], ord('a') - ord('A'))
end;

procedure InitScannerNames;
 var i,lCnt,lNr: integer;
     lDct: text;
     lInFile: XMLInStreamPtr;
     lKind,lDummy:AnsiChar; lString: string;
begin
 assign(lDct,MizFileName+'.dct');
 reset(lDct);
 lCnt:=0;
 while not seekEof(lDct) do
  begin
   readln(lDct);
   inc(lCnt);
  end;
 setlength(AttributeName,lCnt);
 setlength(StructureName,lCnt);
 setlength(ModeName,lCnt);
 setlength(PredicateName,lCnt);
 setlength(FunctorName,lCnt);
 setlength(SelectorName,lCnt);
 setlength(LeftBracketName,lCnt);
 setlength(RightBracketName,lCnt);
 setlength(MMLIdentifierName,lCnt);
 reset(lDct);
 while not seekEof(lDct) do
  begin
   readln(lDct,lKind,lNr,lDummy,lString);
   case lKind of
   'A': MMLIdentifierName[lNr]:=QuoteStrForXML(lString);
   'G': StructureName[lNr]:=QuoteStrForXML(lString);
   'M': ModeName[lNr]:=QuoteStrForXML(lString);
   'K': LeftBracketName[lNr]:=QuoteStrForXML(lString);
   'L': RightBracketName[lNr]:=QuoteStrForXML(lString);
   'O': FunctorName[lNr]:=QuoteStrForXML(lString);
   'R': PredicateName[lNr]:=QuoteStrForXML(lString);
   'U': SelectorName[lNr]:=QuoteStrForXML(lString);
   'V': AttributeName[lNr]:=QuoteStrForXML(lString);
   end;
  end;
 close(lDct);
 AttributeName[StrictSym]:='strict';
 ModeName[SetSym]:='set';
 PredicateName[EqualitySym]:='=';
 LeftBracketName[SquareBracket]:='[';
 LeftBracketName[CurlyBracket]:='{';
 LeftBracketName[RoundedBracket]:='(';
 RightBracketName[SquareBracket]:=']';
 RightBracketName[CurlyBracket]:='}';
 RightBracketName[RoundedBracket]:=')';
//Identifiers
 assign(lDct,MizFileName+'.idx');
 reset(lDct);
 lCnt:=0;
 while not seekEof(lDct) do
  begin
   readln(lDct);
   inc(lCnt);
  end;
 close(lDct);
 setlength(IdentifierName,lCnt);
 IdentifierName[0]:='';
 lInFile:=new(XMLInStreamPtr,OpenFile(MizFileName+'.idx'));
 lInFile^.NextElementState;
 lInFile^.NextElementState;
 while (lInFile.nState = eStart) and (lInFile.nElName = XMLElemName[elSymbol]) do
 begin
  lNr:=lInFile^.GetIntAttr('nr');
  lString:=lInFile^.GetAttr('name');
  IdentifierName[lNr]:=lString;
  lInFile^.NextElementState;
  lInFile^.NextElementState;
 end;
 dispose(lInFile,Done);
// for i := 0 to gScanner^.fIdents.Count - 1 do
//  IdentifierName[i+1]:=TokenPtr(gScanner^.fIdents.Items^[i])^.fStr;

end;

function IdentRepr(aIdNr:integer):string;
begin
 mizassert(2000,aIdNr <= length(IdentifierName));
 if aIdNr > 0 then
  IdentRepr := IdentifierName[aIdNr]
//  TokenPtr(gScanner^.fIdents.Items^[aIdNr-1])^.fStr
 else IdentRepr := '';
end;

{-------------------------------------------------------------------------}

constructor OutWSMizFileObj.OpenFile(const aFileName:string);
begin
 inherited OpenFile( aFileName);
 nMizarAppearance:=false;
 nDisplayInformationOnScreen:=false;
end;

constructor OutWSMizFileObj.OpenFileWithXSL(const aFileName:string);
begin
 inherited OpenFile( aFileName);
 OutString('<?xml-stylesheet type="text/xml" href="file://'+MizFiles+'wsmiz.xml"?>'+#10);
 nMizarAppearance:=false;
end;

destructor OutWSMizFileObj.Done;
begin
 inherited Done;
end;

procedure OutWSMizFileObj.Out_TextProper(aWSTextProper:WSTextProperPtr);
 var i: integer;
begin
 with aWSTextProper^ do
 begin
   Out_XElStart(BlockName[blMain]);
   Out_XAttr( XMLAttrName[atArticleId], nArticleId);
   Out_XAttr( XMLAttrName[atArticleExt], nArticleExt);
   Out_PosAsAttrs(nBlockPos);
   Out_XAttrEnd;
   for i := 0 to nItems^.Count - 1 do
     Out_Item(nItems.Items^[i]);
   Out_XElEnd( BlockName[blMain]);
 end;
end;

procedure OutWSMizFileObj.Out_Block(aWSBlock:WSBlockPtr);
 var i: integer;
begin
 with aWSBlock^ do
 begin
   Out_XElStart( XMLElemName[elBlock]);
   Out_XAttr( XMLAttrName[atKind], BlockName[nBlockKind]);
   CurPos:=nBlockPos;
   Out_PosAsAttrs(nBlockPos);
   Out_XIntAttr( XMLAttrName[atPosLine], nBlockEndPos.Line);
   Out_XIntAttr( XMLAttrName[atPosCol], nBlockEndPos.Col);
   Out_XAttrEnd;
   for i := 0 to nItems^.Count - 1 do
    begin
     Out_Item(nItems^.Items^[i]);
    end;
   Out_XElEnd( XMLElemName[elBlock]);
 end;
end;

procedure OutWSMizFileObj.Out_TermList ( aTrmList:PList );
 var i: integer;
begin
 for i:=0 to aTrmList^.Count-1 do
   Out_Term(aTrmList^.Items^[i]);
end;

procedure OutWSMizFileObj.Out_Adjective(aAttr:AdjectiveExpressionPtr);
begin
 case aAttr^.nAdjectiveSort of
 wsAdjective:
  begin
   Out_XElStart( XMLElemName[elAdjective]);
   with AdjectivePtr(aAttr)^ do
   begin
    Out_XIntAttr( XMLAttrName[atNr],nAdjectiveSymbol );
    if nMizarAppearance then
     Out_XAttr( XMLAttrName[atSpelling], AttributeName[nAdjectiveSymbol]);
    Out_PosAsAttrs(nAdjectivePos);
    if nArgs^.Count = 0 then
     Out_XElEnd0
    else
     begin
      Out_XAttrEnd;
      Out_TermList( nArgs );
      Out_XElEnd( XMLElemName[elAdjective]);
     end;
   end;
  end;
 wsNegatedAdjective:
  begin
   Out_XElStart( XMLElemName[elNegatedAdjective]);
   with NegatedAdjectivePtr(aAttr)^ do
   begin
    Out_PosAsAttrs(nAdjectivePos);
    Out_XAttrEnd;
    Out_Adjective( nArg );
   end;
   Out_XElEnd( XMLElemName[elNegatedAdjective]);
  end;
 end;
end;

procedure OutWSMizFileObj.Out_AdjectiveList(aCluster: PList);
 var i: integer;
begin
 Out_XElStart( XMLElemName[elAdjectiveCluster]);
 if aCluster^.Count = 0 then begin Out_XElEnd0; exit; end;
 Out_XAttrEnd;
 with aCluster^ do
  for i:=0 to Count-1 do
   Out_Adjective( Items^[i]);
 Out_XElEnd( XMLElemName[elAdjectiveCluster]);
end;

procedure OutWSMizFileObj.Out_Type ( aTyp: TypePtr);
begin
 with aTyp^ do
  case aTyp^.nTypeSort of
   wsStandardType:
    with StandardTypePtr(aTyp)^ do
    begin
     Out_XElStart( TypeName[wsStandardType] );
     Out_XIntAttr( XMLAttrName[atNr], nModeSymbol );
     if nMizarAppearance then
      Out_XAttr( XMLAttrName[atSpelling], ModeName[nModeSymbol]);
     Out_PosAsAttrs(nTypePos);
     if  nArgs^.Count = 0 then Out_XElEnd0
     else begin
      Out_XAttrEnd;
      Out_TermList( nArgs );
      Out_XElEnd( TypeName[wsStandardType] );
     end;
    end;
   wsStructureType:
    with StructTypePtr(aTyp)^ do
    begin
     Out_XElStart( TypeName[wsStructureType] );
     Out_XIntAttr( XMLAttrName[atNr], nStructSymbol );
     if nMizarAppearance then
      Out_XAttr( XMLAttrName[atSpelling], StructureName[nStructSymbol]);
     Out_PosAsAttrs(nTypePos);
     if  nArgs^.Count = 0 then Out_XElEnd0
     else begin
      Out_XAttrEnd;
      Out_TermList( nArgs );
      Out_XElEnd( TypeName[wsStructureType] );
     end;
    end;
   wsClusteredType:
    with ClusteredTypePtr(aTyp)^ do
    begin
     Out_XElStart( TypeName[wsClusteredType] );
     Out_PosAsAttrs(nTypePos);
     Out_XAttrEnd;
     Out_AdjectiveList(nAdjectiveCluster);
     Out_Type(nType);
     Out_XElEnd( TypeName[wsClusteredType] );
    end;
   wsErrorType:
    begin
     Out_XElWithPos(TypeName[wsErrorType],nTypePos);
    end;
  end;
end;

procedure OutWSMizFileObj.Out_Variable( aVar: VariablePtr);
begin
 with aVar ^ do
 begin
   Out_XElStart( XMLElemName[elVariable]);
   Out_XIntAttr( XMLAttrName[atIdNr], nIdent);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nIdent));
   Out_PosAsAttrs(nVarPos);
   Out_XElEnd0
 end;
end;

procedure OutWSMizFileObj.Out_ReservedVariable( aVar: VariablePtr);
begin
 Out_Variable(aVar);
end;

procedure OutWSMizFileObj.Out_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr);
begin
  Out_XElStart( SegmentKindName[ikImplQualifiedSegm]);
  Out_PosAsAttrs(aSegm^.nSegmPos);
  Out_XAttrEnd;
  Out_Variable( aSegm^.nIdentifier);
  Out_XElEnd( SegmentKindName[ikImplQualifiedSegm]);
end;

procedure OutWSMizFileObj.Out_VariableSegment( aSegm: QualifiedSegmentPtr);
 var i: integer;
begin
 case aSegm^.nSegmentSort of
 ikImplQualifiedSegm:
  Out_ImplicitlyQualifiedVariable(ImplicitlyQualifiedSegmentPtr(aSegm));
 ikExplQualifiedSegm:
  with ExplicitlyQualifiedSegmentPtr(aSegm)^ do
  begin
   Out_XElStart(SegmentKindName[ikExplQualifiedSegm]);
   Out_PosAsAttrs(nSegmPos);
   Out_XAttrEnd;
   Out_XElStart0( XMLElemName[elVariables]);
   for i:=0 to nIdentifiers^.Count-1 do
     Out_Variable( nIdentifiers^.Items^[i]);
   Out_XElEnd( XMLElemName[elVariables]);
   Out_Type(nType);
   Out_XElEnd( SegmentKindName[ikExplQualifiedSegm]);
  end;
 end;
end;

procedure OutWSMizFileObj.Out_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr );
begin
 with PrivatePredicativeFormulaPtr(aFrm)^ do
  begin
   Out_XElStart(FormulaName[wsPrivatePredicateFormula]);
   Out_XIntAttr( XMLAttrName[atIdNr], nPredIdNr);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nPredIdNr));
   Out_PosAsAttrs(nFormulaPos);
   if nArgs^.Count = 0 then Out_XElEnd0
    else begin
     Out_XAttrEnd;
     Out_TermList( nArgs);
     Out_XElEnd( FormulaName[wsPrivatePredicateFormula]);
    end;
  end;
end;

procedure OutWSMizFileObj.Out_Formula ( aFrm: FormulaPtr );
 var i: integer;
begin
 case aFrm^.nFormulaSort of
  wsNegatedFormula:
    begin
     Out_XElStart(FormulaName[wsNegatedFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     Out_Formula( NegativeFormulaPtr(aFrm)^.nArg);
     Out_XElEnd( FormulaName[wsNegatedFormula]);
    end;
  wsConjunctiveFormula:
    begin
     Out_XElStart( FormulaName[wsConjunctiveFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     Out_Formula(BinaryFormulaPtr(aFrm)^.nLeftArg);
     Out_Formula(BinaryFormulaPtr(aFrm)^.nRightArg);
     Out_XElEnd( FormulaName[wsConjunctiveFormula]);
    end;
  wsDisjunctiveFormula:
    begin
     Out_XElStart( FormulaName[wsDisjunctiveFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     Out_Formula(BinaryFormulaPtr(aFrm)^.nLeftArg);
     Out_Formula(BinaryFormulaPtr(aFrm)^.nRightArg);
     Out_XElEnd( FormulaName[wsDisjunctiveFormula]);
    end;
  wsConditionalFormula:
    begin
     Out_XElStart( FormulaName[wsConditionalFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     Out_Formula(BinaryFormulaPtr(aFrm)^.nLeftArg);
     Out_Formula(BinaryFormulaPtr(aFrm)^.nRightArg);
     Out_XElEnd( FormulaName[wsConditionalFormula]);
    end;
  wsBiconditionalFormula:
    begin
     Out_XElStart( FormulaName[wsBiconditionalFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     Out_Formula(BinaryFormulaPtr(aFrm)^.nLeftArg);
     Out_Formula(BinaryFormulaPtr(aFrm)^.nRightArg);
     Out_XElEnd( FormulaName[wsBiconditionalFormula]);
    end;
  wsFlexaryConjunctiveFormula:
    begin
     Out_XElStart( FormulaName[wsFlexaryConjunctiveFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     Out_Formula(BinaryFormulaPtr(aFrm)^.nLeftArg);
     Out_Formula(BinaryFormulaPtr(aFrm)^.nRightArg);
     Out_XElEnd( FormulaName[wsFlexaryConjunctiveFormula]);
    end;
  wsFlexaryDisjunctiveFormula:
    begin
     Out_XElStart( FormulaName[wsFlexaryDisjunctiveFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     Out_Formula(BinaryFormulaPtr(aFrm)^.nLeftArg);
     Out_Formula(BinaryFormulaPtr(aFrm)^.nRightArg);
     Out_XElEnd( FormulaName[wsFlexaryDisjunctiveFormula]);
    end;
  wsPredicativeFormula:
   with PredicativeFormulaPtr(aFrm)^ do
   begin
    Out_XElStart(FormulaName[wsPredicativeFormula]);
    Out_XIntAttr( XMLAttrName[atNr], nPredNr);
    if nMizarAppearance then
     Out_XAttr( XMLAttrName[atSpelling], PredicateName[nPredNr]);
    Out_PosAsAttrs(nFormulaPos);
    Out_XAttrEnd;
    if nLeftArgs^.Count = 0 then
     Out_XEl1(XMLElemName[elArguments])
    else
     begin
      Out_XElStart0(XMLElemName[elArguments]);
      Out_TermList( nLeftArgs);
      Out_XElEnd( XMLElemName[elArguments]);
     end;
    if nRightArgs^.Count = 0 then
     Out_XEl1(XMLElemName[elArguments])
    else
     begin
      Out_XElStart0(XMLElemName[elArguments]);
      Out_TermList( nRightArgs);
      Out_XElEnd( XMLElemName[elArguments]);
     end;
    Out_XElEnd( FormulaName[wsPredicativeFormula]);
   end;
  wsRightSideOfPredicativeFormula:
   with RightSideOfPredicativeFormulaPtr(aFrm)^ do
   begin
    Out_XElStart(FormulaName[wsRightSideOfPredicativeFormula]);
    Out_XIntAttr( XMLAttrName[atNr], nPredNr);
    if nMizarAppearance then
     Out_XAttr( XMLAttrName[atSpelling], PredicateName[nPredNr]);
    Out_PosAsAttrs(nFormulaPos);
    Out_XAttrEnd;
    if nRightArgs^.Count = 0 then
     Out_XEl1(XMLElemName[elArguments])
    else
     begin
      Out_XElStart0(XMLElemName[elArguments]);
      Out_TermList( nRightArgs);
      Out_XElEnd( XMLElemName[elArguments]);
     end;
    Out_XElEnd( FormulaName[wsRightSideOfPredicativeFormula]);
   end;
  wsMultiPredicativeFormula:
   with MultiPredicativeFormulaPtr(aFrm)^ do
    begin
     Out_XElStart( FormulaName[wsMultiPredicativeFormula]);
     Out_PosAsAttrs(aFrm^.nFormulaPos);
     Out_XAttrEnd;
     for i:=0 to nScraps.Count - 1 do
      Out_Formula(nScraps^.Items^[i]);
     Out_XElEnd( FormulaName[wsMultiPredicativeFormula]);
    end;
  wsPrivatePredicateFormula:
   Out_PrivatePredicativeFormula(PrivatePredicativeFormulaPtr(aFrm));
  wsAttributiveFormula:
   with AttributiveFormulaPtr(aFrm)^ do
   begin
    Out_XElStart(FormulaName[wsAttributiveFormula]);
    Out_PosAsAttrs(nFormulaPos);
    Out_XAttrEnd;
    Out_Term(nSubject);
    Out_AdjectiveList(nAdjectives);
    Out_XElEnd( FormulaName[wsAttributiveFormula]);
   end;
  wsQualifyingFormula:
   with QualifyingFormulaPtr(aFrm)^ do
   begin
    Out_XElStart(FormulaName[wsQualifyingFormula]);
    Out_PosAsAttrs(nFormulaPos);
    Out_XAttrEnd;
    Out_Term(nSubject);
    Out_Type(nType);
    Out_XElEnd( FormulaName[wsQualifyingFormula]);
   end;
  wsUniversalFormula:
   with QuantifiedFormulaPtr( aFrm)^ do
   begin
    Out_XElStart(FormulaName[wsUniversalFormula]);
    Out_PosAsAttrs(nFormulaPos);
    Out_XAttrEnd;
    Out_VariableSegment(QuantifiedFormulaPtr(aFrm)^.nSegment);
    Out_Formula(QuantifiedFormulaPtr(aFrm)^.nScope);
    Out_XElEnd( FormulaName[wsUniversalFormula]);
   end;
  wsExistentialFormula:
   with QuantifiedFormulaPtr( aFrm)^ do
   begin
    Out_XElStart(FormulaName[wsExistentialFormula]);
    Out_PosAsAttrs(nFormulaPos);
    Out_XAttrEnd;
    Out_VariableSegment(QuantifiedFormulaPtr(aFrm)^.nSegment);
    Out_Formula(QuantifiedFormulaPtr(aFrm)^.nScope);
    Out_XElEnd( FormulaName[wsExistentialFormula]);
   end;
  wsContradiction:
    begin
     Out_XElWithPos(FormulaName[wsContradiction],aFrm^.nFormulaPos);
    end;
   wsThesis:
    begin
     Out_XElWithPos(FormulaName[wsThesis],aFrm^.nFormulaPos);
    end;
  wsErrorFormula:
    begin
     Out_XElWithPos(FormulaName[wsErrorFormula],aFrm^.nFormulaPos);
    end;
 end;
end;

procedure OutWSMizFileObj.Out_SimpleTerm ( aTrm: SimpleTermPtr );
begin
  Out_XElStart( TermName[wsSimpleTerm]);
  Out_XIntAttr( XMLAttrName[atIdNr], aTrm^.nIdent);
  if nMizarAppearance then
   Out_XAttr( XMLAttrName[atSpelling], IdentRepr(aTrm^.nIdent));
  Out_PosAsAttrs(aTrm^.nTermPos);
  Out_XElEnd0;
end;

procedure OutWSMizFileObj.Out_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr );
begin
  with PrivateFunctorTermPtr(aTrm)^ do
  begin
    Out_XElStart(TermName[wsPrivateFunctorTerm]);
    Out_XIntAttr( XMLAttrName[atIdNr], nFunctorIdent);
    if nMizarAppearance then
     Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nFunctorIdent));
    Out_PosAsAttrs(nTermPos);
    if nArgs^.Count = 0 then Out_XElEnd0
    else begin
      Out_XAttrEnd;
      Out_TermList( nArgs);
      Out_XElEnd( TermName[wsPrivateFunctorTerm]);
    end;
  end;
end;

procedure OutWSMizFileObj.Out_InternalSelectorTerm ( aTrm: InternalSelectorTermPtr );
begin
  with aTrm^ do
  begin
    Out_XElStart(TermName[wsInternalSelectorTerm]);
    Out_XIntAttr( XMLAttrName[atNr], nSelectorSymbol);
    if nMizarAppearance then
     Out_XAttr( XMLAttrName[atSpelling], SelectorName[nSelectorSymbol]);
    Out_PosAsAttrs(nTermPos);
    Out_XElEnd0;
  end;
end;

procedure OutWSMizFileObj.Out_Term ( aTrm: TermPtr );
 var i: integer;
begin
  case aTrm^.nTermSort of
   wsPlaceholderTerm:
    begin
      Out_XElStart( TermName[wsPlaceholderTerm]);
      Out_XIntAttr( XMLAttrName[atNr], PlaceholderTermPtr(aTrm)^.nLocusNr);
      if nMizarAppearance then
       Out_XAttr( XMLAttrName[atSpelling], QuoteStrForXML(PlaceHolderName[PlaceholderTermPtr(aTrm)^.nLocusNr]));
      Out_PosAsAttrs(aTrm^.nTermPos);
      Out_XElEnd0;
    end;
   wsSimpleTerm:
    Out_SimpleTerm(SimpleTermPtr(aTrm));
   wsNumeralTerm:
    begin                 ;
      Out_XElStart( TermName[wsNumeralTerm]);
      Out_XIntAttr( XMLAttrName[atNumber], NumeralTermPtr(aTrm)^.nValue);
      Out_PosAsAttrs(aTrm^.nTermPos);
      Out_XElEnd0;
    end;
   wsInfixTerm:
    with InfixTermPtr(aTrm)^ do
    begin
      Out_XElStart(TermName[wsInfixTerm]);
      Out_XIntAttr( XMLAttrName[atNr], nFunctorSymbol);
      if nMizarAppearance then
       Out_XAttr( XMLAttrName[atSpelling], FunctorName[nFunctorSymbol]);
      Out_PosAsAttrs(nTermPos);
      Out_XAttrEnd;
      if nLeftArgs^.Count = 0 then
        Out_XEl1(XMLElemName[elArguments])
      else
        begin
         Out_XElStart0(XMLElemName[elArguments]);
         Out_TermList( nLeftArgs);
         Out_XElEnd( XMLElemName[elArguments]);
        end;
       if nRightArgs^.Count = 0 then
         Out_XEl1(XMLElemName[elArguments])
       else
        begin
         Out_XElStart0(XMLElemName[elArguments]);
         Out_TermList( nRightArgs);
         Out_XElEnd(XMLElemName[elArguments]);
        end;
      Out_XElEnd(TermName[wsInfixTerm]);
    end;
   wsCircumfixTerm:
    with CircumfixTermPtr(aTrm)^ do
    begin
      Out_XElStart(TermName[wsCircumfixTerm]);
      Out_XIntAttr( XMLAttrName[atNr], nLeftBracketSymbol);
      if nMizarAppearance then
       Out_XAttr( XMLAttrName[atSpelling], LeftBracketName[nLeftBracketSymbol]);
      Out_PosAsAttrs(nTermPos);
      Out_XAttrEnd;
      Out_XElStart(XMLElemName[elRightCircumflexSymbol]);
      Out_XIntAttr( XMLAttrName[atNr], nRightBracketSymbol);
      if nMizarAppearance then
       Out_XAttr( XMLAttrName[atSpelling], RightBracketName[nRightBracketSymbol]);
      Out_PosAsAttrs(nTermPos);
      Out_XElEnd0;
      Out_TermList( nArgs);
      Out_XElEnd( TermName[wsCircumfixTerm]);
    end;
   wsPrivateFunctorTerm:
    Out_PrivateFunctorTerm(PrivateFunctorTermPtr(aTrm));
   wsAggregateTerm:
    with AggregateTermPtr(aTrm)^ do
    begin
      Out_XElStart(TermName[wsAggregateTerm]);
      Out_XIntAttr( XMLAttrName[atNr], nStructSymbol);
      if nMizarAppearance then
       Out_XAttr( XMLAttrName[atSpelling], StructureName[nStructSymbol]);
      Out_PosAsAttrs(nTermPos);
      if nArgs^.Count = 0 then Out_XElEnd0
      else begin
        Out_XAttrEnd;
        Out_TermList( nArgs);
        Out_XElEnd( TermName[wsAggregateTerm]);
      end;
    end;
   wsSelectorTerm:
    with SelectorTermPtr(aTrm)^ do
    begin
      Out_XElStart(TermName[wsSelectorTerm]);
      Out_XIntAttr( XMLAttrName[atNr], nSelectorSymbol);
      if nMizarAppearance then
       Out_XAttr( XMLAttrName[atSpelling], SelectorName[nSelectorSymbol]);
      Out_PosAsAttrs(nTermPos);
      Out_XAttrEnd;
      Out_Term( nArg);
      Out_XElEnd( TermName[wsSelectorTerm]);
    end;
   wsInternalSelectorTerm:
    Out_InternalSelectorTerm(InternalSelectorTermPtr(aTrm));
   wsForgetfulFunctorTerm:
    with ForgetfulFunctorTermPtr(aTrm)^ do
    begin
      Out_XElStart(TermName[wsForgetfulFunctorTerm]);
      Out_XIntAttr( XMLAttrName[atNr], nStructSymbol);
      if nMizarAppearance then
       Out_XAttr( XMLAttrName[atSpelling], StructureName[nStructSymbol]);
      Out_PosAsAttrs(nTermPos);
      Out_XAttrEnd;
      Out_Term( nArg);
      Out_XElEnd( TermName[wsForgetfulFunctorTerm]);
    end;
   wsInternalForgetfulFunctorTerm:
    with InternalForgetfulFunctorTermPtr(aTrm)^ do
    begin
      Out_XElStart(TermName[wsInternalForgetfulFunctorTerm]);
      Out_XIntAttr( XMLAttrName[atNr], nStructSymbol);
      Out_PosAsAttrs(nTermPos);
      Out_XElEnd0;
    end;
   wsFraenkelTerm:
    with FraenkelTermPtr(aTrm)^ do
    begin
     Out_XElStart( TermName[wsFraenkelTerm]);
     Out_PosAsAttrs(nTermPos);
     Out_XAttrEnd;
     for i := 0 to nPostqualification^.Count - 1 do
      Out_VariableSegment(nPostqualification^.Items^[i]);
     Out_Term(nSample);
     Out_Formula(nFormula);
     Out_XElEnd( TermName[wsFraenkelTerm]);
    end;
   wsSimpleFraenkelTerm:
    with SimpleFraenkelTermPtr(aTrm)^ do
    begin
     Out_XElStart( TermName[wsSimpleFraenkelTerm]);
     Out_PosAsAttrs(nTermPos);
     Out_XAttrEnd;
     for i := 0 to nPostqualification^.Count - 1 do
      Out_VariableSegment(nPostqualification^.Items^[i]);
     Out_Term(nSample);
     Out_XElEnd( TermName[wsSimpleFraenkelTerm]);
    end;
   wsQualificationTerm:
    with QualifiedTermPtr(aTrm)^ do
    begin
     Out_XElStart( TermName[wsQualificationTerm]);
     Out_PosAsAttrs(nTermPos);
     Out_XAttrEnd;
     Out_Term(nSubject);
     Out_Type(nQualification);
     Out_XElEnd( TermName[wsQualificationTerm]);
    end;
   wsExactlyTerm:
    with ExactlyTermPtr(aTrm)^ do
    begin
     Out_XElStart( TermName[wsQualificationTerm]);
     Out_PosAsAttrs(nTermPos);
     Out_XAttrEnd;
     Out_Term(nSubject);
     Out_XElEnd( TermName[wsQualificationTerm]);
    end;
   wsGlobalChoiceTerm:
    begin
     Out_XElStart(TermName[wsGlobalChoiceTerm]);
     Out_PosAsAttrs(aTrm^.nTermPos);
     Out_XAttrEnd;
     Out_Type(ChoiceTermPtr(aTrm)^.nChoiceType);
     Out_XElEnd(TermName[wsGlobalChoiceTerm]);
    end;
   wsItTerm:
    Out_XElWithPos(TermName[wsItTerm],aTrm^.nTermPos);
   wsErrorTerm:
    Out_XEl1( TermName[wsErrorTerm]);
  end;
end;

procedure OutWSMizFileObj.Out_TypeList ( aTypeList: PList );
 var i: integer;
begin
 Out_XElStart0(XMLElemName[elTypeList]);
 for i:=0 to aTypeList^.Count-1 do
  Out_Type(aTypeList^.Items^[i]);
 Out_XElEnd( XMLElemName[elTypeList]);
end;

procedure OutWSMizFileObj.Out_Locus( aLocus: LocusPtr);
begin
 with aLocus ^ do
 begin
   Out_XElStart( XMLElemName[elLocus]);
   Out_XIntAttr( XMLAttrName[atIdNr], nVarId);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nVarId));
   Out_PosAsAttrs(nVarIdPos);
   Out_XElEnd0
 end;
end;

procedure OutWSMizFileObj.Out_Loci( aLoci: PList);
 var i: integer;
begin
 if (aLoci = nil) or (aLoci^.Count = 0) then
  Out_XEl1(XMLElemName[elLoci])
 else
  begin
   Out_XElStart0(XMLElemName[elLoci]);
   for i:=0 to aLoci^.Count-1 do
     Out_Locus(aLoci^.Items^[i]);
   Out_XElEnd( XMLElemName[elLoci]);
  end;
end;

procedure OutWSMizFileObj.Out_Pattern(aPattern: PatternPtr);
begin
 case aPattern^.nPatternSort of
 itDefPred:
  with PredicatePatternPtr(aPattern)^ do
  begin
   Out_XElStart(DefPatternName[itDefPred]);
   Out_XIntAttr( XMLAttrName[atNr], nPredSymbol );
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], PredicateName[nPredSymbol]);
   Out_PosAsAttrs(nPatternPos);
   Out_XAttrEnd;
   Out_Loci(nLeftArgs);
   Out_Loci(nRightArgs);
   Out_XElEnd( DefPatternName[itDefPred]);
  end;
 itDefFunc:
  begin
    case FunctorPatternPtr(aPattern)^.nFunctKind of
     InfixFunctor:
     with InfixFunctorPatternPtr(aPattern)^ do
      begin
       Out_XElStart(FunctorPatternName[InfixFunctor]);
       Out_XIntAttr( XMLAttrName[atNr], nOperSymb);
       if nMizarAppearance then
        Out_XAttr( XMLAttrName[atSpelling], FunctorName[nOperSymb]);
       Out_PosAsAttrs(nPatternPos);
       Out_XAttrEnd;
       Out_Loci(nLeftArgs);
       Out_Loci(nRightArgs);
       Out_XElEnd( FunctorPatternName[InfixFunctor]);
      end;
     CircumfixFunctor:
     with CircumfixFunctorPatternPtr(aPattern)^ do
      begin
       Out_XElStart(FunctorPatternName[CircumfixFunctor]);
       Out_XIntAttr( XMLAttrName[atNr], nLeftBracketSymb);
       if nMizarAppearance then
        Out_XAttr( XMLAttrName[atSpelling], LeftBracketName[nLeftBracketSymb]);
       Out_PosAsAttrs(nPatternPos);
       Out_XAttrEnd;
       Out_XElStart(XMLElemName[elRightCircumflexSymbol]);
       Out_XIntAttr( XMLAttrName[atNr], nRightBracketSymb);
       if nMizarAppearance then
        Out_XAttr( XMLAttrName[atSpelling], RightBracketName[nRightBracketSymb]);
       Out_XAttrEnd;
       Out_XElEnd(XMLElemName[elRightCircumflexSymbol]);
       Out_Loci(nArgs);
       Out_XElEnd( FunctorPatternName[CircumfixFunctor]);
      end;
    end;
  end;
 itDefMode:
  with ModePatternPtr(aPattern)^ do
  begin
   Out_XElStart(DefPatternName[itDefMode]);
   Out_XIntAttr( XMLAttrName[atNr], nModeSymbol);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], ModeName[nModeSymbol]);
   Out_PosAsAttrs(nPatternPos);
   Out_XAttrEnd;
   Out_Loci(nArgs);
   Out_XElEnd(DefPatternName[itDefMode]);
  end;
 itDefAttr:
  with AttributePatternPtr(aPattern)^ do
  begin
   Out_XElStart(DefPatternName[itDefAttr]);
   Out_XIntAttr( XMLAttrName[atNr], nAttrSymbol );
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], AttributeName[nAttrSymbol]);
   Out_PosAsAttrs(nPatternPos);
   Out_XAttrEnd;
   Out_Locus(nArg);
   Out_Loci(nArgs);
   Out_XElEnd( DefPatternName[itDefAttr]);
  end;
 end;
end;

procedure OutWSMizFileObj.Out_Label(aLab:LabelPtr);
begin
 if (aLab <> nil) { and (aLab.nLabelIdNr > 0) } then
  begin
   Out_XElStart( XMLElemName[elLabel]);
   Out_XIntAttr( XMLAttrName[atIdNr], aLab^.nLabelIdNr);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(aLab^.nLabelIdNr));
   Out_PosAsAttrs(aLab^.nLabelPos);
   Out_XElEnd0
  end;
end;

procedure OutWSMizFileObj.Out_Definiens(aDef:DefiniensPtr);
 var i: integer;
     lExprKind: ExpKind;
begin
  if aDef <> nil then
  with DefiniensPtr(aDef)^ do
   begin
    Out_XElStart( XMLElemName[elDefiniens]);
    Out_PosAsAttrs(nDefPos);
    case nDefSort of
    SimpleDefiniens:
     with SimpleDefiniensPtr(aDef)^,nExpression^ do
     begin
      Out_XAttr( XMLAttrName[atKind],DefiniensKindName[SimpleDefiniens]);
      Out_XAttr( XMLAttrName[atShape],ExpName[nExprKind]);
      Out_XAttrEnd;
//      if nDefLabel <> nil then
      Out_Label(nDefLabel);
      case nExprKind of
       exTerm: Out_Term(TermPtr(nExpr));
       exFormula: Out_Formula(FormulaPtr(nExpr));
      end;
     end;
    ConditionalDefiniens:
     with ConditionalDefiniensPtr(aDef)^ do
     begin
      Out_XAttr( XMLAttrName[atKind],DefiniensKindName[ConditionalDefiniens]);
      lExprKind:=exFormula;
      if nOtherwise <> nil then
       lExprKind:=nOtherwise^.nExprKind
      else if nConditionalDefiniensList^.Count > 0 then
       lExprKind:=PartDefPtr(nConditionalDefiniensList^.Items^[0])^.nPartDefiniens^.nExprKind;
      Out_XAttr( XMLAttrName[atShape],ExpName[lExprKind]);
      Out_XAttrEnd;
//      if nDefLabel <> nil then
      Out_Label(nDefLabel);
      for i:=0 to nConditionalDefiniensList^.Count-1 do
       with PartDefPtr(nConditionalDefiniensList^.Items^[I])^ do
        begin
         Out_XElStart0(XMLElemName[elPartialDefiniens]);
         with nPartDefiniens^ do
          case nExprKind of
           exTerm: Out_Term(TermPtr(nExpr));
           exFormula: Out_Formula(FormulaPtr(nExpr));
          end;
         Out_Formula(nGuard);
         Out_XElEnd(XMLElemName[elPartialDefiniens]);
        end;
      if nOtherwise <> nil then
       with nOtherwise^ do
        case nExprKind of
         exTerm: Out_Term(TermPtr(nExpr));
         exFormula: Out_Formula(FormulaPtr(nExpr));
        end;
     end;
    end;
    Out_XElEnd( XMLElemName[elDefiniens]);
   end;
end;

procedure OutWSMizFileObj.Out_Proposition(aProp:PropositionPtr);
begin
 Out_XElStart(XMLElemName[elProposition]);
// Out_PosAsAttrs(aProp^.nSntPos);
 Out_XAttrEnd;
 Out_Label(aProp^.nLab);
 Out_Formula(aProp^.nSentence);
 Out_XElEnd( XMLElemName[elProposition]);
end;

procedure OutWSMizFileObj.Out_LocalReference(aRef: LocalReferencePtr);
begin
 with LocalReferencePtr(aRef)^ do
  begin
   Out_XElStart(ReferenceKindName[LocalReference]);
   Out_PosAsAttrs(nRefPos);
   Out_XIntAttr( XMLAttrName[atIdNr], nLabId);
   if nMizarAppearance then
    Out_XAttr( XMLAttrName[atSpelling], IdentRepr(nLabId));
   Out_XElEnd0;
  end;
end;

procedure OutWSMizFileObj.Out_References(aRefs: PList);
 var i: integer;
begin
 for i:= 0 to aRefs^.Count-1 do
  with ReferencePtr(aRefs^.Items^[i])^ do
   case nRefSort of
   LocalReference:
    Out_LocalReference(aRefs^.Items^[i]);
   TheoremReference:
    begin
     Out_XElStart(ReferenceKindName[TheoremReference]);
     Out_PosAsAttrs(nRefPos);
     Out_XIntAttr( XMLAttrName[atNr], TheoremReferencePtr(aRefs^.Items^[i])^.nArticleNr);
     if nMizarAppearance then
      Out_XAttr( XMLAttrName[atSpelling], MMLIdentifierName[TheoremReferencePtr(aRefs^.Items^[i])^.nArticleNr]);
     Out_XIntAttr( XMLAttrName[atNumber], TheoremReferencePtr(aRefs^.Items^[i])^.nTheoNr);
     Out_XElEnd0;
    end;
   DefinitionReference:
    begin
     Out_XElStart(ReferenceKindName[DefinitionReference]);
     Out_PosAsAttrs(nRefPos);
     Out_XIntAttr( XMLAttrName[atNr], DefinitionReferencePtr(aRefs^.Items^[i])^.nArticleNr);
     if nMizarAppearance then
      Out_XAttr( XMLAttrName[atSpelling], MMLIdentifierName[TheoremReferencePtr(aRefs^.Items^[i])^.nArticleNr]);
     Out_XIntAttr( XMLAttrName[atNumber], DefinitionReferencePtr(aRefs^.Items^[i])^.nDefNr);
     Out_XElEnd0;
    end;
   end;
end;

procedure OutWSMizFileObj.Out_Link(aInf: JustificationPtr);
begin
   with StraightforwardJustificationPtr(aInf)^ do
    if nLinked then
    begin
     Out_XElStart(XMLElemName[elLink]);
     Out_PosAsAttrs(nLinkPos);
     Out_XElEnd0;
    end;
end;

procedure OutWSMizFileObj.Out_SchemeJustification(aInf: SchemeJustificationPtr);
begin
 with aInf^ do
  begin
   Out_XElStart(InferenceName[infSchemeJustification]);
   Out_XIntAttr( XMLAttrName[atNr],nSchFileNr);
   Out_XIntAttr( XMLAttrName[atIdNr],nSchemeIdNr);
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

procedure OutWSMizFileObj.Out_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr);
begin
  case aInf^.nInfSort of
   infStraightforwardJustification:
   with StraightforwardJustificationPtr(aInf)^ do
    begin
     Out_XElStart(InferenceName[infStraightforwardJustification]);
     Out_PosAsAttrs(nInfPos);
     if not nLinked and (nReferences^.Count=0) then
      Out_XElEnd0
     else
      begin
       Out_XAttrEnd;
       Out_Link(aInf);
       Out_References(nReferences);
       Out_XElEnd(InferenceName[infStraightforwardJustification]);
      end;
    end;
   infSchemeJustification:
    Out_SchemeJustification(SchemeJustificationPtr(aInf));
   infError:
    Out_XElWithPos(InferenceName[infError],aInf^.nInfPos);
   infSkippedProof:
    Out_XElWithPos(InferenceName[infSkippedProof],aInf^.nInfPos);
   infProof:
    Out_Block(aBlock);
  end;
end;

procedure OutWSMizFileObj.Out_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr);
begin
  with aCStm^ do
  begin
    Out_Proposition(nProp);
    Out_Justification(nJustification,aBlock);
  end;
end;

procedure OutWSMizFileObj.Out_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr);
 var i: integer;
begin
 case aRStm^.nStatementSort of
 stDiffuseStatement:
  begin
    Out_Label(DiffuseStatementPtr(aRStm)^.nLab);
    Out_Block(aBlock);
  end;
 stCompactStatement:
  Out_CompactStatement(CompactStatementPtr(aRStm),aBlock);
 stIterativeEquality:
  begin
   Out_CompactStatement(CompactStatementPtr(aRStm),nil);
   with IterativeEqualityPtr(aRStm)^ do
    for i := 0 to nIterSteps^.Count - 1 do
     with IterativeStepPtr(nIterSteps^.Items^[i])^ do
     begin
      Out_XElStart(XMLElemName[elIterativeStep]);
      Out_PosAsAttrs(nIterPos);
      Out_XAttrEnd;
      Out_Term(nTerm);
      Out_Justification(nJustification,nil);
      Out_XElEnd(XMLElemName[elIterativeStep]);
     end;
  end;
 end;
end;

procedure OutWSMizFileObj.Out_ReservationSegment(aRes:ReservationSegmentPtr);
 var i: integer;
begin
   with aRes^ do
   begin
    Out_XElStart0( XMLElemName[elVariables]);
    for i:=0 to nIdentifiers^.Count-1 do
     Out_ReservedVariable( nIdentifiers^.Items^[i]);
    Out_XElEnd( XMLElemName[elVariables]);
//    Out_XElStart0( XMLElemName[elTypeSpecification]);
    Out_Type(nResType);
//    Out_XElEnd( XMLElemName[elTypeSpecification]);
   end;
end;

procedure OutWSMizFileObj.Out_SchemeNameInSchemeHead(aSch: SchemePtr);
begin
 Out_XIntAttr( XMLAttrName[atIdNr], aSch^.nSchemeIdNr);
 if nMizarAppearance then
  Out_XAttr( XMLAttrName[atSpelling], IdentRepr(aSch^.nSchemeIdNr));
end;

procedure OutWSMizFileObj.Out_ItemContentsAttr(aWSItem:WSItemPtr);
begin
 with aWSItem^ do
 begin
   CurPos:=nItemPos;
   if nDisplayInformationOnScreen then
     DisplayLine(CurPos.Line,ErrorNbr);
   case nItemKind of
    itDefinition, itSchemeBlock, itSchemeHead, itTheorem, itAxiom,
    itReservation:;
    itSection:;
    itConclusion,
    itRegularStatement:
     case RegularStatementPtr(nContent)^.nStatementSort of
     stDiffuseStatement:
       Out_XAttr( XMLAttrName[atShape], RegularStatementName[stDiffuseStatement]);
     stCompactStatement:
       Out_XAttr( XMLAttrName[atShape], RegularStatementName[stCompactStatement]);
     stIterativeEquality:
       Out_XAttr( XMLAttrName[atShape], RegularStatementName[stIterativeEquality]);
     end;
    itChoice, itReconsider,
    itPrivFuncDefinition, itPrivPredDefinition, itConstantDefinition,
    itGeneralization, itLociDeclaration,itExistentialAssumption, itExemplification,
    itPerCases, itCaseBlock:;
    itCaseHead, itSupposeHead,
    itAssumption:;
    itCorrCond:
      Out_XAttr( XMLAttrName[atCondition],
                 CorrectnessName[CorrectnessConditionPtr(nContent)^.nCorrCondSort]);
    itCorrectness:
      Out_XAttr( XMLAttrName[atCondition],CorrectnessName[syCorrectness]);
    itProperty:
      Out_XAttr( XMLAttrName[atProperty],PropertyName[PropertyPtr(nContent)^.nPropertySort]);
    itDefFunc:
      Out_XAttr( XMLAttrName[atShape],DefiningWayName[FunctorDefinitionPtr(nContent)^.nDefiningWay]);
    itDefPred, itDefMode, itDefAttr,
    itDefStruct,
    itPredSynonym, itPredAntonym, itFuncNotation, itModeNotation,
    itAttrSynonym, itAttrAntonym,
    itCluster,
    itIdentify, itReduction:;
    itPropertyRegistration:
      Out_XAttr( XMLAttrName[atProperty],PropertyName[PropertyPtr(nContent)^.nPropertySort]);
    itPragma:
      Out_XAttr( XMLAttrName[atSpelling],QuoteStrForXML(PragmaPtr(nContent)^.nPragmaStr));
   end;
 end;
end;

procedure OutWSMizFileObj.Out_ItemContents(aWSItem:WSItemPtr);
 var i,j: integer;
     s: CorrectnessKind;
begin
 with aWSItem^ do
 begin
  case nItemKind of
  itDefinition:
    Out_Block(nBlock);
  itSchemeBlock:
    Out_Block(nBlock);
  itSchemeHead:
   with SchemePtr(nContent)^ do
   begin
     Out_XElStart( XMLElemName[elScheme]);
     Out_SchemeNameInSchemeHead(SchemePtr(nContent));
     Out_XElEnd0;
     Out_XElStart0(XMLElemName[elSchematicVariables]);
     for j:=0 to nSchemeParams^.Count-1 do
      case SchemeSegmentPtr(nSchemeParams.Items^[j])^.nSegmSort of
      PredicateSegment:
       with PredicateSegmentPtr(nSchemeParams.Items^[j])^ do
       begin
        Out_XElStart( SchemeSegmentName[PredicateSegment]);
        Out_PosAsAttrs(nSegmPos);
        Out_XAttrEnd;
        Out_XElStart0( XMLElemName[elVariables]);
        for i:=0 to nVars^.Count-1 do
          Out_Variable( nVars.Items^[i]);
        Out_XElEnd( XMLElemName[elVariables]);
        Out_TypeList(nTypeExpList);
        Out_XElEnd( SchemeSegmentName[PredicateSegment]);
       end;
      FunctorSegment:
       with FunctorSegmentPtr(nSchemeParams.Items^[j])^ do
       begin
        Out_XElStart( SchemeSegmentName[FunctorSegment]);
        Out_PosAsAttrs(nSegmPos);
        Out_XAttrEnd;
        Out_XElStart0( XMLElemName[elVariables]);
        for i:=0 to nVars^.Count-1 do
         Out_Variable( nVars.Items^[i]);
        Out_XElEnd( XMLElemName[elVariables]);
        Out_TypeList(nTypeExpList);
        Out_XElStart0( XMLElemName[elTypeSpecification]);
        Out_Type(nSpecification);
        Out_XElEnd( XMLElemName[elTypeSpecification]);
        Out_XElEnd( SchemeSegmentName[FunctorSegment]);
       end;
      end;
     Out_XElEnd( XMLElemName[elSchematicVariables]);
     Out_Formula(nSchemeConclusion);
     if (nSchemePremises <> nil) and (nSchemePremises^.Count > 0) then
      begin
        Out_XElStart0( XMLElemName[elProvisionalFormulas]);
        for i:=0 to nSchemePremises^.Count-1 do
         Out_Proposition(nSchemePremises^.Items^[i]);
        Out_XElEnd( XMLElemName[elProvisionalFormulas]);
      end;
   end;
  itTheorem:
    Out_CompactStatement(CompactStatementPtr(nContent),nBlock);
  itAxiom:
   begin

   end;
  itReservation:
   Out_ReservationSegment(ReservationSegmentPtr(nContent));
  itSection:;
  itConclusion,
  itRegularStatement:
   Out_RegularStatement(RegularStatementPtr(nContent),nBlock);
  itChoice:
   with ChoiceStatementPtr(nContent)^ do
   begin
    for i:= 0 to  nQualVars^.Count-1 do
      Out_VariableSegment( nQualVars^.Items^[i]);
    Out_XElStart0( XMLElemName[elConditions]);
    for i:=0 to nConditions^.Count-1 do
      Out_Proposition(nConditions^.Items^[i]);
    Out_XElEnd( XMLElemName[elConditions]);
    Out_Justification(nJustification,nil);
   end;
  itReconsider:
   with TypeChangingStatementPtr(nContent)^ do
   begin
    for i:=0 to nTypeChangeList^.Count-1 do
     case TypeChangePtr(nTypeChangeList.Items^[i])^.nTypeChangeKind of
     Equating:
      begin
       Out_XElStart0( XMLElemName[elEquality]);
       Out_Variable(TypeChangePtr(nTypeChangeList.Items^[i])^.nVar);
       Out_Term(TypeChangePtr(nTypeChangeList.Items^[i])^.nTermExpr);
       Out_XElEnd( XMLElemName[elEquality]);
      end;
     VariableIdentifier:
      begin
       Out_Variable(TypeChangePtr(nTypeChangeList.Items^[i])^.nVar);
      end;
     end;
    Out_Type(nTypeExpr);
    Out_Justification(nJustification,nil);
   end;
  itPrivFuncDefinition:
   with PrivateFunctorDefinitionPtr(nContent)^ do
   begin
    Out_Variable(nFuncId);
    Out_TypeList(nTypeExpList);
    Out_Term(nTermExpr);
   end;
  itPrivPredDefinition:
   with PrivatePredicateDefinitionPtr(nContent)^ do
   begin
    Out_Variable(nPredId);
    Out_TypeList(nTypeExpList);
    Out_Formula(nSentence);
   end;
  itConstantDefinition:
   with ConstantDefinitionPtr(nContent)^ do
   begin
    Out_Variable(nVarId);
    Out_Term(nTermExpr);
   end;
  itLociDeclaration,
  itGeneralization:
    Out_VariableSegment( QualifiedSegmentPtr(nContent));
  itCaseHead,itSupposeHead,
  itAssumption:
   case AssumptionPtr(nContent)^.nAssumptionSort of
   SingleAssumption:
    begin
      Out_XElStart( AssumptionKindName[SingleAssumption]);
      Out_PosAsAttrs(AssumptionPtr(nContent)^.nAssumptionPos);
      Out_XAttrEnd;
      Out_Proposition(SingleAssumptionPtr(nContent)^.nProp);
      Out_XElEnd( AssumptionKindName[SingleAssumption]);
    end;
   CollectiveAssumption:
    begin
      Out_XElStart( AssumptionKindName[CollectiveAssumption]);
      Out_PosAsAttrs(AssumptionPtr(nContent)^.nAssumptionPos);
      Out_XAttrEnd;
      Out_XElStart0( XMLElemName[elConditions]);
      with CollectiveAssumptionPtr(nContent)^ do
       for i:=0 to nConditions^.Count-1 do
        Out_Proposition(nConditions^.Items^[i]);
      Out_XElEnd( XMLElemName[elConditions]);
      Out_XElEnd( AssumptionKindName[CollectiveAssumption]);
    end;
   end;
  itExistentialAssumption:
   with ExistentialAssumptionPtr(nContent)^ do
    begin
     for i:= 0 to  nQVars^.Count-1 do
      Out_VariableSegment( nQVars^.Items^[i]);
     Out_XElStart0( XMLElemName[elConditions]);
     for i:=0 to nConditions^.Count-1 do
      Out_Proposition(nConditions^.Items^[i]);
     Out_XElEnd( XMLElemName[elConditions]);
    end;
  itExemplification:
   with ExamplePtr(nContent)^ do
   begin
    if nVarId <> nil then
      Out_Variable(nVarId);
    if nTermExpr <> nil then
      Out_Term(nTermExpr);
   end;
  itPerCases:
    Out_Justification(JustificationPtr(nContent),nil);
  itCaseBlock:
    Out_Block(nBlock);
  itCorrCond:
    Out_Justification(CorrectnessConditionPtr(nContent)^.nJustification,nBlock);
  itCorrectness:
   begin
    Out_XElStart0( XMLElemName[elCorrectnessConditions]);
    for s in CorrectnessConditionsPtr(nContent)^.nConditions do
     begin
      Out_XElStart(ItemName[itCorrectness]);
      Out_XAttr( XMLAttrName[atCondition],CorrectnessName[s]);
      Out_XElEnd0;
     end;
    Out_XElEnd( XMLElemName[elCorrectnessConditions]);
    Out_Justification(CorrectnessPtr(nContent)^.nJustification,nBlock);
   end;
  itProperty:
    Out_Justification(PropertyPtr(nContent)^.nJustification,nBlock);
  itDefMode:
   with ModeDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     Out_XEl1(XMLElemName[elRedefine]);
    Out_Pattern(nDefModePattern);
    case nDefKind of
    defExpandableMode:
     begin
      Out_XElStart0( ModeDefinitionSortName[defExpandableMode]);
      Out_Type(ExpandableModeDefinitionPtr(nContent)^.nExpansion);
      Out_XElEnd( ModeDefinitionSortName[defExpandableMode]);
     end;
    defStandardMode:
     with StandardModeDefinitionPtr(nContent)^ do
     begin
      Out_XElStart0( ModeDefinitionSortName[defStandardMode]);
      if nSpecification <> nil then
       begin
        Out_XElStart0( XMLElemName[elTypeSpecification]);
        Out_Type(nSpecification);
        Out_XElEnd( XMLElemName[elTypeSpecification]);
       end;
      Out_Definiens(nDefiniens);
      Out_XElEnd( ModeDefinitionSortName[defStandardMode]);
     end;
    end;
   end;
  itDefAttr:
   with AttributeDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     Out_XEl1(XMLElemName[elRedefine]);
    Out_Pattern(nDefAttrPattern);
    Out_Definiens(nDefiniens);
   end;
  itDefPred:
   with PredicateDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     Out_XEl1(XMLElemName[elRedefine]);
    Out_Pattern(nDefPredPattern);
    Out_Definiens(nDefiniens);
   end;
  itDefFunc:
   with FunctorDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     Out_XEl1(XMLElemName[elRedefine]);
    Out_Pattern(nDefFuncPattern);
    if nSpecification <> nil then
     begin
      Out_XElStart0( XMLElemName[elTypeSpecification]);
      Out_Type(nSpecification);
      Out_XElEnd( XMLElemName[elTypeSpecification]);
     end;
    Out_Definiens(nDefiniens);
   end;
  itDefStruct:
   with StructureDefinitionPtr(nContent)^ do
   begin
     Out_XElStart0( XMLElemName[elAncestors]);
     for i := 0 to nAncestors^.Count - 1 do
      Out_Type(nAncestors^.Items^[i]);
     Out_XElEnd( XMLElemName[elAncestors]);
     Out_XElStart(DefPatternName[itDefStruct]);
     Out_XIntAttr( XMLAttrName[atNr],nDefStructPattern^.nModeSymbol);
     if nMizarAppearance then
      Out_XAttr( XMLAttrName[atSpelling], StructureName[nDefStructPattern^.nModeSymbol]);
     Out_PosAsAttrs(nStrPos);
     Out_XAttrEnd;
     Out_Loci(nDefStructPattern^.nArgs);
     for i := 0 to nSgmFields^.Count - 1 do
      with FieldSegmentPtr(nSgmFields^.Items^[i])^ do
       begin
        Out_XElStart(XMLElemName[elFieldSegment]);
        Out_PosAsAttrs(nFieldSegmPos);
        Out_XAttrEnd;
        for j := 0 to nFields^.Count - 1 do
         with FieldSymbolPtr(nFields^.Items^[j])^ do
          begin
           Out_XElStart( XMLElemName[elSelector]);
           Out_XIntAttr( XMLAttrName[atNr], nFieldSymbol);
           if nMizarAppearance then
            Out_XAttr( XMLAttrName[atSpelling], SelectorName[nFieldSymbol]);
           Out_PosAsAttrs(nFieldPos);
           Out_XElEnd0
          end;
        Out_Type(nSpecification);
        Out_XElEnd(XMLElemName[elFieldSegment]);
       end;
     Out_XElEnd( DefPatternName[itDefStruct]);
   end;
  itPredSynonym, itPredAntonym, itFuncNotation, itModeNotation,
  itAttrSynonym, itAttrAntonym:
   with NotationDeclarationPtr(nContent)^ do
   begin
//    nNotationPos: Position;
//    nNotationSort: ItemKind;
     Out_Pattern(nOriginPattern);
     Out_Pattern(nNewPattern);
   end;
  itCluster:
   case ClusterPtr(nContent)^.nClusterKind of
    ExistentialRegistration :
     with EClusterPtr(nContent)^ do
     begin
      Out_XElStart( ClusterRegistrationName[ExistentialRegistration]);
      Out_PosAsAttrs(nClusterPos);
      Out_XAttrEnd;
      Out_AdjectiveList(nConsequent);
      Out_Type(nClusterType);
      Out_XElEnd(ClusterRegistrationName[ExistentialRegistration]);
     end;
    ConditionalRegistration :
     with CClusterPtr(nContent)^ do
     begin
      Out_XElStart( ClusterRegistrationName[ConditionalRegistration]);
      Out_PosAsAttrs(nClusterPos);
      Out_XAttrEnd;
      Out_AdjectiveList(nAntecedent);
      Out_AdjectiveList(nConsequent);
      Out_Type(nClusterType);
      Out_XElEnd(ClusterRegistrationName[ConditionalRegistration]);
     end;
    FunctorialRegistration:
     with FClusterPtr(nContent)^ do
     begin
      Out_XElStart( ClusterRegistrationName[FunctorialRegistration]);
      Out_PosAsAttrs(nClusterPos);
      Out_XAttrEnd;
      Out_Term(nClusterTerm);
      Out_AdjectiveList(nConsequent);
      if nClusterType <> nil then
       Out_Type(nClusterType);
      Out_XElEnd(ClusterRegistrationName[FunctorialRegistration]);
     end;
   end;
  itIdentify:
   with IdentifyRegistrationPtr(nContent)^ do
   begin
     Out_Pattern(nOriginPattern);
     Out_Pattern(nNewPattern);
     if nEqLociList <> nil then
      begin
       for i := 0 to nEqLociList^.Count - 1 do
        with LociEqualityPtr(nEqLociList^.Items^[i])^ do
        begin
         Out_XElStart(XMLElemName[elLociEquality]);
         Out_PosAsAttrs(nEqPos);
         Out_XAttrEnd;
         Out_Locus(nLeftLocus);
         Out_Locus(nRightLocus);
         Out_XElEnd(XMLElemName[elLociEquality]);
        end;
      end;
   end;
  itPropertyRegistration:
   case PropertyRegistrationPtr(nContent)^.nPropertySort of
   sySethood:
    with SethoodRegistrationPtr(nContent)^ do
     begin
      Out_Type(nSethoodType);
      Out_Justification(nJustification,nBlock);
     end;
   end;
  itReduction:
   with ReduceRegistrationPtr(nContent)^ do
   begin
     Out_Term(nOriginTerm);
     Out_Term(nNewTerm);
   end;
  itPragma: ;
  itIncorrItem:;
  end;
 end;
end;

procedure OutWSMizFileObj.Out_Item(aWSItem:WSItemPtr);
 var i,j: integer;
begin
 with aWSItem^ do
 begin
   CurPos:=nItemPos;
   Out_XElStart(XMLElemName[elItem]);
   Out_XAttr( XMLAttrName[atKind], ItemName[nItemKind]);
   if nContent <> nil then
     Out_ItemContentsAttr(aWsItem);
   Out_PosAsAttrs(nItemPos);
   Out_XIntAttr( XMLAttrName[atPosLine], nItemEndPos.Line);
   Out_XIntAttr( XMLAttrName[atPosCol], nItemEndPos.Col);
   Out_XAttrEnd;
   if nContent = nil then
    begin
     if nBlock <> nil then
       Out_Block(nBlock);
    end
   else Out_ItemContents(aWsItem);
   Out_XElEnd( XMLElemName[elItem]);
 end;
end;

procedure Write_WSMizArticle(aWSTextProper:wsTextProperPtr; aFileName:string);
 var lWSMizOutput: OutWSMizFilePtr;
begin
  InitScannerNames;
  lWSMizOutput:=new(OutWSMizFilePtr,OpenFile(aFileName));
  lWSMizOutput^.nMizarAppearance:=true;
  lWSMizOutput^.Out_TextProper(aWSTextProper);
  dispose(lWSMizOutput,Done);
end;

constructor InWSMizFileObj.OpenFile(const aFileName:string );
begin
 inherited OpenFile(aFileName);
 nDisplayInformationOnScreen:=false;
end;

destructor InWSMizFileObj.Done;
begin
 inherited Done;
end;

function InWSMizFileObj.GetAttrValue(const aAttrName:string): string;
 var lObj: PObject;
begin
 result:='';
 lObj:=nAttrVals.ObjectOf(aAttrName);
 if lObj <> nil then
   result:=XMLAttrPtr(lObj)^.nValue;
end;

function InWSMizFileObj.GetAttrPos: Position;
 var lLine,lCol: XMLAttrPtr;
     lCode: integer;
begin
 result.Line:=1;
 result.Col:=1;
 lLine:=XMLAttrPtr(nAttrVals.ObjectOf(XMLAttrName[atLine]));
 lCol:=XMLAttrPtr(nAttrVals.ObjectOf(XMLAttrName[atCol]));
 if (lLine <> nil) and (lCol <> nil) then
  begin
   Val(lLine^.nValue, result.Line,lCode);
   Val(lCol^.nValue, result.Col,lCode);
  end;
end;

var
  ElemLookupTable,AttrLookupTable,BlockLookUpTable,ItemLookUpTable,
  FormulaKindLookupTable,TermKindLookupTable,
  PatternKindLookupTable,
  CorrectnessKindLookupTable,PropertyKindLookupTable: MSortedStrList;

procedure InitWSLookupTables;
var e: XMLElemKind;
    a: XMLAttrKind;
    b: BlockKind;
    i: ItemKind;
    f: FormulaSort;
    t: TermSort;
    p: PropertyKind;
    c: CorrectnessKind;
begin
 ElemLookupTable.Init( Ord( High( XMLElemKind)) + 1);
 AttrLookupTable.Init( Ord( High( XMLAttrKind)) + 1);
 BlockLookupTable.Init( Ord( High( BlockKind)) + 1);
 ItemLookupTable.Init( Ord( High( ItemKind)) + 1);
 FormulaKindLookupTable.Init( Ord( High( FormulaSort)) + 1);
 TermKindLookupTable.Init( Ord( High( TermSort)) + 1);
 PatternKindLookupTable.Init( Ord(itDefStruct)- Ord(itDefPred) + 1);
 CorrectnessKindLookupTable.Init(ord( High(CorrectnessKind)) + 1);
 PropertyKindLookupTable.Init( ord( High(PropertyKind)) + 1);

 for e:= Low( XMLElemKind) to High( XMLElemKind) do
  ElemLookupTable.Insert( new( MStrPtr, Init( XMLElemName[ e] )));
 for a:= Low( XMLAttrKind) to High( XMLAttrKind) do
  AttrLookupTable.Insert( new( MStrPtr, Init( XMLAttrName[ a] )));
 for b:= Low( BlockKind) to High( BlockKind) do
  BlockLookupTable.Insert( new( MStrPtr, Init( BlockName[ b] )));
 for i:= Low( ItemKind) to High( ItemKind) do
  ItemLookupTable.Insert( new( MStrPtr, Init( ItemName[ i] )));
 for f:= Low( FormulaSort) to High( FormulaSort) do
  FormulaKindLookupTable.Insert( new( MStrPtr, Init( FormulaName[ f] )));
 for t:= Low( TermSort) to High( TermSort) do
  TermKindLookupTable.Insert( new( MStrPtr, Init( TermName[ t] )));
 for i:= itDefPred to itDefStruct do
  PatternKindLookupTable.Insert( new( MStrPtr, Init( DefPatternName[ i] )));
 for p:= Low( PropertyKind) to High( PropertyKind) do
  PropertyKindLookupTable.Insert( new( MStrPtr, Init( PropertyName[ p] )));
 for c:= Low( CorrectnessKind) to High( CorrectnessKind) do
  CorrectnessKindLookupTable.Insert( new( MStrPtr, Init( CorrectnessName[ c] )));
end;

procedure DisposeWSLookupTables;
begin
 ElemLookupTable.Done;
 AttrLookupTable.Done;
 BlockLookupTable.Done;
 ItemLookupTable.Done;
 FormulaKindLookupTable.Done;
 TermKindLookupTable.Done;
 CorrectnessKindLookupTable.Done;
 PropertyKindLookupTable.Done;
end;

function Str2XMLElemKind( aStr: string): XMLElemKind;
var lNr:integer;
begin
 lNr:= ElemLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2XMLElemKind:= XMLElemKind( lNr)
 else Str2XMLElemKind:= elUnknown;
end;

function Str2XMLAttrKind( aStr: string): XMLAttrKind;
var lNr:integer;
begin
 lNr:= AttrLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2XMLAttrKind:= XMLAttrKind( lNr)
 else Str2XMLAttrKind:= atUnknown;
end;

function Str2BlockKind( aStr: string): BlockKind;
var lNr:integer;
begin
 lNr:= BlockLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2BlockKind:= BlockKind( lNr)
 else Str2BlockKind:= blMain;
end;

function Str2ItemKind( aStr: string): ItemKind;
var lNr:integer;
begin
 lNr:= ItemLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2ItemKind:= ItemKind( lNr)
 else Str2ItemKind:=itIncorrItem;
end;

function Str2PatterenKind( aStr: string): ItemKind;
var lNr:integer;
begin
 lNr:= PatternKindLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2PatterenKind:= ItemKind(Ord(ItDefPred)+lNr)
 else Str2PatterenKind:=itIncorrItem;
end;

function Str2FormulaKind( aStr: string): FormulaSort;
var lNr:integer;
begin
 lNr:= FormulaKindLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2FormulaKind:= FormulaSort( lNr)
 else Str2FormulaKind:=wsErrorFormula;
end;

function Str2TermKind( aStr: string): TermSort;
var lNr:integer;
begin
 lNr:= TermKindLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2TermKind:= TermSort( lNr)
 else Str2TermKind:=wsErrorTerm;
end;

function Str2PropertyKind( aStr: string): PropertyKind;
var lNr:integer;
begin
 lNr:= PropertyKindLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2PropertyKind:= PropertyKind( lNr)
end;

function Str2CorrectnessKind( aStr: string): CorrectnessKind;
var lNr:integer;
begin
 lNr:= CorrectnessKindLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2CorrectnessKind:= CorrectnessKind( lNr)
end;

function InWSMizFileObj.Read_TermList:PList;
begin
 result :=new(PList,Init(0));
 while nState <> eEnd do
   result^.Insert(Read_Term);
end;

function InWSMizFileObj.Read_Adjective:AdjectiveExpressionPtr;
 var lAttrNr: integer;
     lPos: Position;
     lNoneOcc: Boolean;
begin
  if nElName = AdjectiveSortName[wsAdjective] then
    begin
     lPos:=GetAttrPos;
     lAttrNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(AdjectivePtr,Init(lPos,lAttrNr,Read_TermList));
     NextElementState;
    end
  else
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(NegatedAdjectivePtr,Init(lPos,Read_Adjective));
     NextElementState;
    end;
end;

function InWSMizFileObj.Read_AdjectiveList: PList;
begin
  result:=new(Plist,Init(0));
  NextElementState;   //
  while nState <> eEnd do
   result^.Insert(Read_Adjective);
  NextElementState;
end;

function InWSMizFileObj.Read_Type: TypePtr;
 var lList: Plist;
     lPos: Position;
     lModeSymbol:integer;
begin
  if nElName = TypeName[wsStandardType] then
    begin
     lPos:=GetAttrPos;
     lModeSymbol:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(StandardTypePtr,Init(lPos,lModeSymbol,Read_TermList));
     NextElementState;
    end
  else if nElName = TypeName[wsStructureType] then
    begin
     lPos:=GetAttrPos;
     lModeSymbol:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(StructTypePtr,Init(lPos,lModeSymbol,Read_TermList));
     NextElementState;
    end
  else if nElName = TypeName[wsClusteredType] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lList:=Read_AdjectiveList;
     result:=new(ClusteredTypePtr,Init(lPos,lList,Read_Type));
     NextElementState;
    end
  else
//   wsErrorType:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(IncorrectTypePtr,Init(lPos));
     NextElementState;
    end
end;

function InWSMizFileObj.Read_Variable: VariablePtr;
 var lPos:Position;
     lNr: integer;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  NextElementState;
  result:=new(VariablePtr,Init(lPos,lNr));
  NextElementState;
end;

function InWSMizFileObj.Read_ImplicitlyQualifiedSegment: ImplicitlyQualifiedSegmentPtr;
 var lPos: Position;
begin
 lPos:=GetAttrPos;
 NextElementState;
 result:=new(ImplicitlyQualifiedSegmentPtr,Init(lPos,Read_Variable));
 NextElementState;
end;

function InWSMizFileObj.Read_VariableSegment: QualifiedSegmentPtr;
 var lPos: Position;
     lVar: VariablePtr;
     lList: PList;
begin
  if nElName = SegmentKindName[ikImplQualifiedSegm] then
    begin
     result:=Read_ImplicitlyQualifiedSegment;
    end
  else if nElName = SegmentKindName[ikExplQualifiedSegm] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lList:=new(PList,Init(0));
     NextElementState; //elVariables
     while (nState = eStart) and (nElName = XMLElemName[elVariable]) do
       lList^.Insert(Read_Variable);
     NextElementState;
     result:=new(ExplicitlyQualifiedSegmentPtr,Init(lPos,lList,Read_Type));
     NextElementState;
    end
end;

function InWSMizFileObj.Read_PrivatePredicativeFormula:PrivatePredicativeFormulaPtr;
 var lPos:Position;
     lNr: integer;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  NextElementState;
  Result:=new(PrivatePredicativeFormulaPtr,Init(lPos,lNr,Read_TermList));
  NextElementState;
end;

function InWSMizFileObj.Read_Formula:FormulaPtr;
 var lPos:Position;
     lNr: integer;
     lList: PList;
     lFrm: FormulaPtr;
     lTrm: TermPtr;
     lSgm: QualifiedSegmentPtr;
begin
  case Str2FormulaKind(nElName) of
  wsNegatedFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(NegativeFormulaPtr,Init(lPos,Read_Formula));
     NextElementState;
    end;
  wsConjunctiveFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lFrm:=Read_Formula;
     result:=new(ConjunctiveFormulaPtr,Init(lPos,lFrm,Read_Formula));
     NextElementState;
    end;
  wsDisjunctiveFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lFrm:=Read_Formula;
     result:=new(DisjunctiveFormulaPtr,Init(lPos,lFrm,Read_Formula));
     NextElementState;
    end;
  wsConditionalFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lFrm:=Read_Formula;
     result:=new(ConditionalFormulaPtr,Init(lPos,lFrm,Read_Formula));
     NextElementState;
    end;
  wsBiconditionalFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lFrm:=Read_Formula;
     result:=new(BiconditionalFormulaPtr,Init(lPos,lFrm,Read_Formula));
     NextElementState;
    end;
  wsFlexaryConjunctiveFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lFrm:=Read_Formula;
     result:=new(FlexaryConjunctiveFormulaPtr,Init(lPos,lFrm,Read_Formula));
     NextElementState;
    end;
  wsFlexaryDisjunctiveFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lFrm:=Read_Formula;
     result:=new(FlexaryDisjunctiveFormulaPtr,Init(lPos,lFrm,Read_Formula));
     NextElementState;
    end;
  wsPredicativeFormula:
   begin
    lPos:=GetAttrPos;
    lNr:=GetIntAttr(XMLAttrName[atNr]);
    NextElementState;
    NextElementState; // Arguments
    lList:=Read_TermList;
    NextElementState; // Arguments
    NextElementState; // Arguments
    Result:=new(PredicativeFormulaPtr,Init(lPos,lNr,lList,Read_TermList));
    NextElementState;
    NextElementState;
   end;
  wsRightSideOfPredicativeFormula:
   begin
    lPos:=GetAttrPos;
    lNr:=GetIntAttr(XMLAttrName[atNr]);
    NextElementState;
    NextElementState; // Arguments
    Result:=new(RightSideOfPredicativeFormulaPtr,Init(lPos,lNr,Read_TermList));
    NextElementState;
    NextElementState;
   end;
  wsMultiPredicativeFormula:
   begin
    lPos:=GetAttrPos;
    NextElementState;
    lList:=new(PList,Init(0));
    while nState <> eEnd do
     lList^.Insert(Read_Formula);
    result:=new(MultiPredicativeFormulaPtr,Init(lPos,lList));
    NextElementState;
   end;
  wsPrivatePredicateFormula:
   begin
    Result:=Read_PrivatePredicativeFormula;
   end;
  wsAttributiveFormula:
   begin
    lPos:=GetAttrPos;
    NextElementState;
    lTrm:=Read_Term;
    Result:=new(AttributiveFormulaPtr,Init(lPos,lTrm,Read_AdjectiveList));
    NextElementState;
   end;
  wsQualifyingFormula:
   begin
    lPos:=GetAttrPos;
    NextElementState;
    lTrm:=Read_Term;
    Result:=new(QualifyingFormulaPtr,Init(lPos,lTrm,Read_Type));
    NextElementState;
   end;
  wsUniversalFormula:
   begin
    lPos:=GetAttrPos;
    NextElementState;
    lSgm:=Read_VariableSegment;
    Result:=new(UniversalFormulaPtr,Init(lPos,lSgm,Read_Formula));
    NextElementState;
   end;
  wsExistentialFormula:
   begin
    lPos:=GetAttrPos;
    NextElementState;
    lSgm:=Read_VariableSegment;
    Result:=new(ExistentialFormulaPtr,Init(lPos,lSgm,Read_Formula));
    NextElementState;
   end;
  wsContradiction:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(ContradictionFormulaPtr,Init(lPos));
     NextElementState;
    end;
  wsThesis:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(ThesisFormulaPtr,Init(lPos));
     NextElementState;
    end;
  wsErrorFormula:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(IncorrectFormulaPtr,Init(lPos));
     NextElementState;
    end;
 end;
end;

function InWSMizFileObj.Read_SimpleTerm: SimpleTermPtr;
 var lPos:Position;
     lNr: integer;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atIdNr]);
 NextElementState;
 result:=new(SimpleTermPtr,Init(lPos,lNr));
 NextElementState;
end;

function InWSMizFileObj.Read_PrivateFunctorTerm: PrivateFunctorTermPtr;
 var lPos:Position;
     lNr: integer;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atIdNr]);
 NextElementState;
 result:=new(PrivateFunctorTermPtr,Init(lPos,lNr,Read_TermList));
 NextElementState;
end;

function InWSMizFileObj.Read_InternalSelectorTerm: InternalSelectorTermPtr;
 var lPos:Position;
     lNr: integer;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 NextElementState;
 result:=new(InternalSelectorTermPtr,Init(lPos,lNr));
 NextElementState;
end;

function InWSMizFileObj.Read_Term: TermPtr;
 var lPos,lRPos:Position;
     lNr,lRNr: integer;
     lList: PList;
     lTrm: TermPtr;
begin
  case Str2TermKind(nElName) of
   wsPlaceholderTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(PlaceholderTermPtr,Init(lPos,lNr));
     NextElementState;
    end;
   wsSimpleTerm:
    begin
     result:=Read_SimpleTerm;
    end;
   wsNumeralTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNumber]);
     NextElementState;
     result:=new(NumeralTermPtr, Init(lPos,lNr));
     NextElementState;
    end;
   wsInfixTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     NextElementState; // Arguments
     lList:=Read_TermList;
     NextElementState; // Arguments
     NextElementState; // Arguments
     result:=new(InfixTermPtr,Init(lPos,lNr,lList,Read_TermList));
     NextElementState;
     NextElementState;
    end;
   wsCircumfixTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     NextElementState;
     lRNr:=GetIntAttr(XMLAttrName[atNr]);
     lRPos:=GetAttrPos;
     NextElementState;
     result:=new(CircumfixTermPtr,Init(lPos,lNr,lRNr,Read_TermList));
     NextElementState;
    end;
   wsPrivateFunctorTerm:
    begin
     result:=Read_PrivateFunctorTerm;
    end;
   wsAggregateTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(AggregateTermPtr,Init(lPos,lNr,Read_TermList));
     NextElementState;
    end;
   wsSelectorTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(SelectorTermPtr,Init(lPos,lNr,Read_Term));
     NextElementState;
    end;
   wsInternalSelectorTerm:
    result:=Read_InternalSelectorTerm;
   wsForgetfulFunctorTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(ForgetfulFunctorTermPtr,Init(lPos,lNr,Read_Term));
     NextElementState;
    end;
   wsInternalForgetfulFunctorTerm:
    begin
     lPos:=GetAttrPos;
     lNr:=GetIntAttr(XMLAttrName[atNr]);
     NextElementState;
     result:=new(InternalForgetfulFunctorTermPtr,Init(lPos,lNr));
     NextElementState;
    end;
   wsFraenkelTerm:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lList:=new(PList,Init(0));
     while (nState = eStart) and
           ((nElName = SegmentKindName[ikImplQualifiedSegm]) or
            (nElName = SegmentKindName[ikExplQualifiedSegm])) do
       lList^.Insert(Read_VariableSegment);
     lTrm:=Read_Term;
     result:=new(FraenkelTermPtr,Init(lPos,lList,lTrm,Read_Formula));
     NextElementState;
    end;
   wsSimpleFraenkelTerm:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lList:=new(PList,Init(0));
     while (nState = eStart) and
           ((nElName = SegmentKindName[ikImplQualifiedSegm]) or
            (nElName = SegmentKindName[ikExplQualifiedSegm])) do
       lList^.Insert(Read_VariableSegment);
     lTrm:=Read_Term;
     result:=new(SimpleFraenkelTermPtr,Init(lPos,lList,lTrm));
     NextElementState;
    end;
   wsQualificationTerm:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lTrm:=Read_Term;
     Result:=new(QualifiedTermPtr,Init(lPos,lTrm,Read_Type));
     NextElementState;
    end;
   wsExactlyTerm:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     Result:=new(ExactlyTermPtr,Init(lPos,Read_Term));
     NextElementState;
    end;
   wsGlobalChoiceTerm:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     Result:=new(ChoiceTermPtr,Init(lPos,Read_Type));
     NextElementState;
    end;
   wsItTerm:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     Result:=new(ItTermPtr,Init(lPos));
     NextElementState;
    end;
   wsErrorTerm:
    begin
     lPos:=GetAttrPos;
     NextElementState;
     Result:=new(IncorrectTermPtr,Init(lPos));
     NextElementState;
    end;
  end;
end;

function InWSMizFileObj.Read_TypeList: PList;
begin
 NextElementState;
 result :=new(PList,Init(0));
 while nState <> eEnd do
   result^.Insert(Read_Type);
 NextElementState;
end;

function InWSMizFileObj.Read_Locus: LocusPtr;
 var lPos:Position;
     lNr: integer;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  NextElementState;
  result:=new(LocusPtr,Init(lPos,lNr));
  NextElementState;
end;

function InWSMizFileObj.Read_Loci: PList;
begin
 NextElementState;
 result :=new(PList,Init(0));
 while nState <> eEnd do
   result^.Insert(Read_Locus);
 NextElementState;
end;

function InWSMizFileObj.Read_ModePattern: ModePatternPtr;
 var lPos:Position;
     lNr: integer;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 NextElementState;
 result:=new(ModePatternPtr,Init(lPos,lNr,Read_Loci));
 NextElementState;
end;

function InWSMizFileObj.Read_AttributePattern: AttributePatternPtr;
 var lPos:Position;
     lNr: integer;
     lArg: LocusPtr;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 NextElementState;
 lArg:=Read_Locus;
 result:=new(AttributePatternPtr,Init(lPos,lArg,lNr,Read_Loci));
 NextElementState;
end;

function InWSMizFileObj.Read_FunctorPattern: FunctorPatternPtr;
 var lPos,lRPos:Position;
     lNr,lRNr: integer;
     lArgs: PList;
begin
 if nState= eStart then
  if nElName = FunctorPatternName[InfixFunctor] then
   begin
    lPos:=GetAttrPos;
    lNr:=GetIntAttr(XMLAttrName[atNr]);
    NextElementState;
    lArgs:=Read_Loci;
    result:=new(InfixFunctorPatternPtr,Init(lPos,lArgs,lNr,Read_Loci));
    NextElementState;
   end
  else if nElName = FunctorPatternName[CircumfixFunctor] then
   begin
    lPos:=GetAttrPos;
    lNr:=GetIntAttr(XMLAttrName[atNr]);
    NextElementState;
    lRNr:=GetIntAttr(XMLAttrName[atNr]);
    NextElementState;
    NextElementState;
    result:=new(CircumfixFunctorPatternPtr,Init(lPos,lNr,lRNr,Read_Loci));
    NextElementState;
   end;
end;

function InWSMizFileObj.Read_PredicatePattern: PredicatePatternPtr;
 var lPos,lRPos:Position;
     lNr,lRNr: integer;
     lArgs: PList;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atNr]);
  NextElementState;
  lArgs:=Read_Loci;
  result:=new(PredicatePatternPtr,Init(lPos,lArgs,lNr,Read_Loci));
  NextElementState;
end;

function InWSMizFileObj.Read_Pattern: PatternPtr;
begin
  case Str2PatterenKind(nElName) of
  itDefPred: result:=Read_PredicatePattern;
  itDefFunc: result:=Read_FunctorPattern;
  itDefMode: result:=Read_ModePattern;
  itDefAttr: result:=Read_AttributePattern;
  else
   if (nElName = FunctorPatternName[InfixFunctor]) or
      (nElName = FunctorPatternName[CircumfixFunctor])
    then result:=Read_FunctorPattern
   else result:=nil;
  end;
end;

function InWSMizFileObj.Read_Definiens: DefiniensPtr;
 var lPos: Position;
     lKind,lShape: string;
     lLab: LabelPtr;
     lExpr: PObject;
     lExpKind: ExpKind;
     lList: PList;
     lOtherwise: DefExpressionPtr;
begin
 result:=nil;
 if (nState= eStart) and (nElName = XMLElemName[elDefiniens]) then
 begin
  lPos:=GetAttrPos;
  lKind:=GetAttr(XMLAttrName[atKind]);
  lShape:=GetAttr(XMLAttrName[atShape]);
  NextElementState;
  lLab:=Read_Label;
  if lKind = DefiniensKindName[SimpleDefiniens] then
   begin
    lExpKind:=exFormula;
    if lShape = ExpName[exTerm] then
     lExpKind:=exTerm;
    case lExpKind of
     exTerm: lExpr:=Read_Term;
     exFormula: lExpr:=Read_Formula;
    end;
    result:=new(SimpleDefiniensPtr,Init(lPos,lLab,
                          new(DefExpressionPtr,Init(lExpKind,lExpr))));
   end
  else
   begin
    lList:=new(Plist,Init(0));
    while (nState= eStart) and (nElName = XMLElemName[elPartialDefiniens]) do
     begin
      NextElementState;
      lExpKind:=exFormula;
      if lShape = ExpName[exTerm] then
       lExpKind:=exTerm;
      case lExpKind of
       exTerm: lExpr:=Read_Term;
       exFormula: lExpr:=Read_Formula;
      end;
      lList^.Insert(new(PartDefPtr,Init(new(DefExpressionPtr,Init(lExpKind,lExpr)),Read_Formula)));
      NextElementState;
     end;
    lOtherwise:=nil;
    if nState <> eEnd then
     begin
      lExpKind:=exFormula;
      if lShape = ExpName[exTerm] then
       lExpKind:=exTerm;
      case lExpKind of
       exTerm: lExpr:=Read_Term;
       exFormula: lExpr:=Read_Formula;
      end;
      lOtherwise:=new(DefExpressionPtr,Init(lExpKind,lExpr));
     end;
    result:=new(ConditionalDefiniensPtr,Init(lPos,lLab,lList,lOtherwise))
   end;
  NextElementState;
 end;
end;

function InWSMizFileObj.Read_Label: LabelPtr;
 var lLabPos: Position;
     lLabId: Integer;
begin
  result:=nil;
  if (nState= eStart)  and (nElName = XMLElemName[elLabel]) then
  begin
   lLabId:=GetIntAttr(XMLAttrName[atIdNr]);
   lLabPos:=GetAttrPos;
   NextElementState;
   NextElementState;
   result:=new(LabelPtr,Init(lLabId,lLabPos));
  end;
end;

function InWSMizFileObj.Read_Proposition: PropositionPtr;
 var lPos: Position;
     lLab: LabelPtr;
begin
  NextElementState;
  lLab:=Read_label;
  result:=new(PropositionPtr,Init(lLab,Read_Formula,lPos));
  NextElementState;
end;

function InWSMizFileObj.Read_LocalReference: LocalReferencePtr;
 var lPos: Position;
     lNr: integer;
begin
  lPos:=GetAttrPos;
  lNr:=GetIntAttr(XMLAttrName[atIdNr]);
  NextElementState;
  NextElementState;
  result:=new(LocalReferencePtr, Init(lNr,lPos));
end;

function InWSMizFileObj.Read_References: PList;
 var lPos: Position;
     lNr,lFileNr: integer;
begin
  result:=new(Plist,Init(0));
  while nState <> eEnd do
  if nElName = ReferenceKindName[LocalReference] then
    begin
     result^.Insert(Read_LocalReference)
    end
  else if nElName = ReferenceKindName[TheoremReference] then
    begin
     lPos:=GetAttrPos;
     lFileNr:=GetIntAttr(XMLAttrName[atNr]);
     lNr:=GetIntAttr(XMLAttrName[atNumber]);
     NextElementState;
     NextElementState;
     result^.Insert(new(TheoremReferencePtr, Init(lFileNr,lNr,lPos)))
    end
  else if nElName = ReferenceKindName[DefinitionReference] then
    begin
     lPos:=GetAttrPos;
     lFileNr:=GetIntAttr(XMLAttrName[atNr]);
     lNr:=GetIntAttr(XMLAttrName[atNumber]);
     NextElementState;
     NextElementState;
     result^.Insert(new(DefinitionReferencePtr, Init(lFileNr,lNr,lPos)))
    end;
end;


function InWSMizFileObj.Read_ReservationSegment: ReservationSegmentPtr;
 var lList: PList;
begin
 lList:=new(PList,Init(0));
 NextElementState; //elVariables
 while (nState = eStart) and (nElName = XMLElemName[elVariable]) do
  lList^.Insert(Read_Variable);
 NextElementState;
//    NextElementState;
 result:=new(ReservationSegmentPtr,Init(lList,Read_Type));
//    NextElementState;
end;

function InWSMizFileObj.Read_SchemeNameInSchemeHead: SchemePtr;
 var lNr: Integer;
     lPos: Position;
begin
 lPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atIdNr]);
 result:=new(SchemePtr,Init(lNr,lPos,nil,nil,nil));
end;

function InWSMizFileObj.Read_CompactStatement: CompactStatementPtr;
 var lProp: PropositionPtr;
begin
  lProp:=Read_Proposition;
  result:=new(CompactStatementPtr,Init(lProp,Read_Justification));
end;

function InWSMizFileObj.Read_StraightforwardJustification: StraightforwardJustificationPtr;
 var lPos,lLinkPos: Position;
     lLinked: boolean;
begin
  lPos:=GetAttrPos;
  NextElementState;
  lLinked:=false;
  lLinkPos:=lPos;
  if nelName = XMLElemName[elLink] then
   begin
     lLinked:=true;
     lLinkPos:=GetAttrPos;
     NextElementState;
     NextElementState;
   end;
  result:=new(StraightforwardJustificationPtr,Init(lPos,lLinked,lLinkPos));
  StraightforwardJustificationPtr(result)^.nReferences:=Read_References;
  NextElementState;
end;

function InWSMizFileObj.Read_SchemeJustification: SchemeJustificationPtr;
 var lInfPos,lPos: Position;
     lNr,lIdNr: integer;
begin
 lInfPos:=GetAttrPos;
 lNr:=GetIntAttr(XMLAttrName[atNr]);
 lIdNr:=GetIntAttr(XMLAttrName[atIdNr]);
 lPos.Line:=GetIntAttr( XMLAttrName[atPosLine]);
 lPos.Col:= GetIntAttr( XMLAttrName[atPosCol]);
 NextElementState;
 result:=new(SchemeJustificationPtr,Init(lInfPos,lNr,lIdNr));
 SchemeJustificationPtr(result)^.nSchemeInfPos:=lPos;
 SchemeJustificationPtr(result)^.nReferences:=Read_References;
 NextElementState;
end;

function InWSMizFileObj.Read_Justification: JustificationPtr;
 var lPos: Position;
begin
 if nState= eStart then
  if nElName = InferenceName[infStraightforwardJustification] then
    result:=Read_StraightforwardJustification
  else if nElName = InferenceName[infSchemeJustification] then
    result:=Read_SchemeJustification
  else if nElName = InferenceName[infError] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(JustificationPtr,Init(infError,lPos));
     NextElementState;
    end
  else if nElName = InferenceName[infSkippedProof] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     result:=new(JustificationPtr,Init(infSkippedProof,lPos));
     NextElementState;
    end
  else
    result:=new(JustificationPtr,Init(infProof,CurPos));
end;

function InWSMizFileObj.Read_RegularStatement(const aShape: string): RegularStatementPtr;
 var lPos: Position;
    lIdNr: integer;
    lTrm: TermPtr;
    lCStm: CompactStatementPtr;
    lLab: LabelPtr;
begin
  if aShape = RegularStatementName[stDiffuseStatement] then
  begin
    lLab:=Read_Label;
    result:=new(DiffuseStatementPtr,Init(lLab,stDiffuseStatement));
  end
 else if aShape = RegularStatementName[stCompactStatement] then
  begin
    result:=Read_CompactStatement;
  end
 else if aShape = RegularStatementName[stIterativeEquality] then
  begin
    lCStm:=Read_CompactStatement;
    result:=new(IterativeEqualityPtr,Init(lCStm^.nProp,lCStm^.nJustification,new(PList,Init(0))));
    while (nState= eStart) and (nElName = XMLElemName[elIterativeStep]) do
     begin
      lPos:=GetAttrPos;
      NextElementState;
      lTrm:=Read_Term;
      IterativeEqualityPtr(result)^.nIterSteps^.Insert(new(IterativeStepPtr,Init(lPos,lTrm,Read_Justification)));
      NextElementState;
     end;
  end;
end;

procedure InWSMizFileObj.Read_ItemContentsAttr(aItem: wsItemPtr; var aShape: string);
begin
 aShape:='';
 case aItem^.nItemKind of
  itIncorrItem:;
  itDefinition, itSchemeBlock, itSchemeHead, itTheorem, itAxiom,
  itReservation:;
  itSection:;
  itConclusion,
  itRegularStatement:
    aShape:=GetAttr(XMLAttrName[atShape]);
  itChoice, itReconsider,
  itPrivFuncDefinition, itPrivPredDefinition, itConstantDefinition,
  itGeneralization, itLociDeclaration,itExistentialAssumption, itExemplification,
  itPerCases, itCaseBlock:;
  itCaseHead, itSupposeHead,
  itAssumption:;
  itCorrCond:
    aItem^.nContent:=new(CorrectnessConditionPtr,
                Init(CurPos,Str2CorrectnessKind(GetAttr(XMLAttrName[atCondition])),nil));
  itCorrectness:
    aItem^.nContent:=new(CorrectnessConditionsPtr,Init(CurPos,[],nil));
  itProperty:
    aShape:=GetAttr(XMLAttrName[atProperty]);
  itDefFunc:
    aShape:=GetAttr(XMLAttrName[atShape]);
  itDefPred, itDefMode, itDefAttr,
  itDefStruct,
  itPredSynonym, itPredAntonym, itFuncNotation, itModeNotation,
  itAttrSynonym, itAttrAntonym,
  itCluster,
  itIdentify, itReduction:;
  itPropertyRegistration:
    aShape:=GetAttr(XMLAttrName[atProperty]);
  itPragma:
    aItem^.nContent:=new(PragmaPtr,Init(XMLToStr(GetAttr(XMLAttrName[atSpelling]))));
 end;
end;

procedure InWSMizFileObj.Read_ItemContents(aItem: wsItemPtr; const aShape: string);
 var lList,lCons,lConds,lVars,lFields,lTyps,lSels: PList;
     lType: TypePtr;
     lNr: Integer;
     lVar: VariablePtr;
     lLocus: LocusPtr;
     lTrm: TermPtr;
     lPos,lFieldSgmPos: Position;
     lRedefinition: boolean;
     lPattern: PatternPtr;
     lDef: HowToDefine;
     lPropertySort: PropertyKind;
begin
  lPos:=CurPos;
  case aItem^.nItemKind of
  itIncorrItem:;
  itDefinition:;
  itSchemeBlock:;
  itSchemeHead:
   begin
    aItem^.nContent:=Read_SchemeNameInSchemeHead;
    NextElementState;
    NextElementState;
    NextElementState;   //elSchematicVariables
    lList:=new(PList,Init(0));
    while (nState = eStart) and
          ((nElName = SchemeSegmentName[PredicateSegment]) or
           (nElName = SchemeSegmentName[FunctorSegment])) do
     if nElName = SchemeSegmentName[PredicateSegment] then
      begin
       lPos:=GetAttrPos;
       NextElementState;
       lVars:=new(PList,Init(0));
       NextElementState; //elVariables
       while (nState = eStart) and (nElName = XMLElemName[elVariable]) do
         lVars^.Insert(Read_Variable);
       NextElementState;
       lList^.Insert(new(PredicateSegmentPtr,Init(lPos,PredicateSegment,lVars,Read_TypeList)));
       NextElementState;
      end
     else
      begin
       lPos:=GetAttrPos;
       NextElementState;
       lVars:=new(PList,Init(0));
       NextElementState; //elVariables
       while (nState = eStart) and (nElName = XMLElemName[elVariable]) do
         lVars^.Insert(Read_Variable);
       NextElementState;
       lTyps:=Read_TypeList;
       NextElementState;
       lList^.Insert(new(FunctorSegmentPtr,Init(lPos,lVars,lTyps,Read_Type)));
       NextElementState;
       NextElementState;
      end;
     SchemePtr(aItem^.nContent)^.nSchemeParams:=lList;
     NextElementState; //elSchematicVariables
     SchemePtr(aItem^.nContent)^.nSchemeConclusion:=Read_Formula;
     lConds:=new(PList,Init(0));
     if (nState = eStart) and (nElName = XMLElemName[elProvisionalFormulas]) then
      begin
       NextElementState;
       while (nState = eStart) and (nElName = XMLElemName[elProposition]) do
        lConds^.Insert(Read_Proposition);
       NextElementState;
      end;
     SchemePtr(aItem^.nContent)^.nSchemePremises:=lConds;
   end;
  itTheorem:
   aItem^.nContent:=Read_CompactStatement;
  itAxiom:
   begin
   end;
  itReservation:
   aItem^.nContent:=Read_ReservationSegment;
  itSection:;
  itChoice:
   begin
     lList:=new(PList,Init(0));
     while (nState = eStart) and
          ((nElName = SegmentKindName[ikImplQualifiedSegm]) or
           (nElName = SegmentKindName[ikExplQualifiedSegm])) do
      lList^.Insert(Read_VariableSegment);
     NextElementState;
     lConds:=nil;
     if nElName = XMLElemName[elProposition] then
       begin
        lConds:=new(PList,Init(0));
        while (nState = eStart) and (nElName = XMLElemName[elProposition]) do
         lConds^.Insert(Read_Proposition);
       end;
     NextElementState;
     aItem^.nContent:=new(ChoiceStatementPtr,Init(lList,lConds,
                 SimpleJustificationPtr(Read_Justification)));
   end;
  itReconsider:
   begin
    lList:=new(PList,Init(0));
    while (nState = eStart) and
          ((nElName = XMLElemName[elEquality]) or (nElName = XMLElemName[elVariable])) do
     if nElName = XMLElemName[elVariable] then
      lList^.Insert(new(TypeChangePtr,Init(VariableIdentifier,Read_Variable,nil)))
     else
      begin
       NextElementState;
       lVar:=Read_Variable;
       lList^.Insert(new(TypeChangePtr,Init(Equating,lVar,Read_Term)));
       NextElementState;
      end;
    lType:=Read_Type;
    aItem^.nContent:=new(TypeChangingStatementPtr,
                Init(lList,lType,SimpleJustificationPtr(Read_Justification)));
   end;
  itPrivFuncDefinition:
   begin
    lVar:=Read_Variable;
    lList:=Read_TypeList;
    aItem^.nContent:=new(PrivateFunctorDefinitionPtr,Init(lVar,lList,Read_Term));
   end;
  itPrivPredDefinition:
   begin
    lVar:=Read_Variable;
    lList:=Read_TypeList;
    aItem^.nContent:=new(PrivatePredicateDefinitionPtr,Init(lVar,lList,Read_Formula));
   end;
  itConstantDefinition:
   begin
    lVar:=Read_Variable;
    aItem^.nContent:=new(ConstantDefinitionPtr,Init(lVar,Read_Term));
   end;
  itLociDeclaration,
  itGeneralization:
    aItem^.nContent:=Read_VariableSegment;
  itPerCases:
    aItem^.nContent:=Read_Justification;
  itCaseBlock: ;
  itCorrCond:
   begin
    CorrectnessConditionPtr(aItem^.nContent)^.nJustification:=Read_Justification;
   end;
  itCorrectness:
   begin
    NextElementState;
    while (nState = eStart) and (nElName = ItemName[itCorrectness]) do
     begin
       NextElementState;
       include(CorrectnessConditionsPtr(aItem^.nContent)^.nConditions,
               Str2CorrectnessKind(GetAttr(XMLAttrName[atCondition])));
       NextElementState;
     end;
    NextElementState;
    CorrectnessConditionPtr(aItem^.nContent)^.nJustification:=Read_Justification;
   end;
  itProperty:
    aItem^.nContent:=new(PropertyPtr,Init(lPos,Str2PropertyKind(aShape),Read_Justification));
  itConclusion,
  itRegularStatement:
   aItem^.nContent:=Read_RegularStatement(aShape);
  itCaseHead,itSupposeHead,
  itAssumption:
   if nState= eStart then
   if nElName = AssumptionKindName[SingleAssumption] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     aItem^.nContent:=new(SingleAssumptionPtr,Init(lPos,Read_Proposition));
     NextElementState;
    end
   else if nElName = AssumptionKindName[CollectiveAssumption] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     aItem^.nContent:=new(CollectiveAssumptionPtr,Init(lPos,new(PList,Init(0))));
     NextElementState;
     while (nState = eStart) and (nElName = XMLElemName[elProposition]) do
       CollectiveAssumptionPtr(aItem^.nContent)^.nConditions^.Insert(Read_Proposition);
     NextElementState;
     NextElementState;
    end;
  itExistentialAssumption:
    begin
     aItem^.nContent:=new(ExistentialAssumptionPtr,Init(lPos,new(PList,Init(0)),new(PList,Init(0))));
     while (nState = eStart) and
           ((nElName = SegmentKindName[ikImplQualifiedSegm]) or
            (nElName = SegmentKindName[ikExplQualifiedSegm])) do
       ExistentialAssumptionPtr(aItem^.nContent)^.nQVars^.Insert(Read_VariableSegment);
     NextElementState;
     while (nState = eStart) and (nElName = XMLElemName[elProposition]) do
       ExistentialAssumptionPtr(aItem^.nContent)^.nConditions^.Insert(Read_Proposition);
     NextElementState;
    end;
  itExemplification:
   begin
     lVar:=nil;
     if (nState = eStart) and (nElName = XMLElemName[elVariable]) then
      lVar:=Read_Variable;
     lTrm:=nil;
     if nState <> eEnd then
      lTrm:=Read_Term;
     aItem^.nContent:=new(ExamplePtr,Init(lVar,lTrm));
   end;
  itDefPred:
   begin
    lRedefinition:=false;
    if (nState= eStart) and (nElName = XMLElemName[elRedefine]) then
     begin
      NextElementState;
      NextElementState;
      lRedefinition:=true;
     end;
    lPattern:=Read_PredicatePattern;
    aItem^.nContent:=new(PredicateDefinitionPtr,
            Init(lPos,lRedefinition,PredicatePatternPtr(lPattern),
                 Read_Definiens));
   end;
  itDefFunc:
   begin
    lRedefinition:=false;
    if (nState= eStart) and (nElName = XMLElemName[elRedefine]) then
     begin
      NextElementState;
      NextElementState;
      lRedefinition:=true;
     end;
    lPattern:=Read_FunctorPattern;
    lType:=nil;
    if (nState= eStart) and (nElName = XMLElemName[elTypeSpecification]) then
     begin
      NextElementState;
      lType:=Read_Type;
      NextElementState;
     end;
    if aShape = DefiningWayName[dfMeans] then
      lDef:=dfMeans
    else if aShape = DefiningWayName[dfEquals] then
      lDef:=dfEquals
    else lDef:=dfEmpty;
    case lDef of
    dfEquals:
     aItem^.nContent:=new(FunctorDefinitionPtr,
            Init(lPos,lRedefinition,FunctorPatternPtr(lPattern),
                 lType,lDef,Read_Definiens));
    dfMeans:
     aItem^.nContent:=new(FunctorDefinitionPtr,
            Init(lPos,lRedefinition,FunctorPatternPtr(lPattern),
                 lType,lDef,Read_Definiens));
    dfEmpty:
     aItem^.nContent:=new(FunctorDefinitionPtr,
            Init(lPos,lRedefinition,FunctorPatternPtr(lPattern),
                 lType,lDef,nil));

    end;
   end;
  itDefMode:
   begin
    lRedefinition:=false;
    if (nState= eStart) and (nElName = XMLElemName[elRedefine]) then
     begin
      NextElementState;
      NextElementState;
      lRedefinition:=true;
     end;
    lPattern:=Read_ModePattern;
    if (nState= eStart) and (nElName = ModeDefinitionSortName[defExpandableMode]) then
     begin
      NextElementState;
      aItem^.nContent:=new(ExpandableModeDefinitionPtr,Init(CurPos,ModePatternPtr(lPattern),Read_Type));
      NextElementState;
     end
    else if (nState= eStart) and (nElName = ModeDefinitionSortName[defStandardMode]) then
     begin
      NextElementState;
      lType:=nil;
      if (nState= eStart) and (nElName = XMLElemName[elTypeSpecification]) then
       begin
        NextElementState;
        lType:=Read_Type;
        NextElementState;
       end;
      aItem^.nContent:=new(StandardModeDefinitionPtr,Init(CurPos,lRedefinition,ModePatternPtr(lPattern),
                                                 lType,Read_Definiens));
      NextElementState;
     end;
   end;
  itDefAttr:
   begin
    lRedefinition:=false;
    if (nState= eStart) and (nElName = XMLElemName[elRedefine]) then
     begin
      NextElementState;
      NextElementState;
      lRedefinition:=true;
     end;
    lPattern:=Read_AttributePattern;
    aItem^.nContent:=new(AttributeDefinitionPtr,Init(CurPos,lRedefinition,AttributePatternPtr(lPattern),
                                            Read_Definiens));
   end;
  itDefStruct:
   begin
    NextElementState;
    lTyps:=new(PList,Init(0));
    while nState <> eEnd do
     lTyps^.Insert(Read_Type);
    NextElementState;
    lPos:=GetAttrPos;
    lNr:=GetIntAttr(XMLAttrName[atNr]);
    NextElementState;
    lList:=nil;
    if (nState = eStart) and (nElName = XMLElemName[elLoci]) then
     lList:=Read_Loci;
    lFields:=new(PList,Init(0));
    while (nState = eStart) and (nElName = XMLElemName[elFieldSegment]) do
     begin
      lFieldSgmPos:=GetAttrPos;
      NextElementState;
      lSels:=new(PList,Init(0));
      while (nState = eStart) and (nElName = XMLElemName[elSelector]) do
        begin
         lSels^.Insert(new(FieldSymbolPtr,Init(GetAttrPos,GetIntAttr(XMLAttrName[atNr]))));
         NextElementState;
         NextElementState;
        end;
      lFields^.Insert(new(FieldSegmentPtr,Init(lFieldSgmPos,lSels,Read_Type)));
      NextElementState;
     end;
    NextElementState;
    aItem^.nContent:=new(StructureDefinitionPtr,Init(lPos,lTyps,lNr,lList,lFields));
   end;
  itPredSynonym, itPredAntonym, itFuncNotation, itModeNotation,
  itAttrSynonym, itAttrAntonym:
   begin
    lPattern:=Read_Pattern;
    aItem^.nContent:= new(NotationDeclarationPtr,
            Init(lPos,aItem^.nItemKind,Read_Pattern,lPattern));
   end;
  itCluster:
   if nState= eStart then
   if nElName = ClusterRegistrationName[ExistentialRegistration] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lList:=Read_AdjectiveList;
     aItem^.nContent:=new(EClusterPtr,Init(lPos,lList,Read_Type));
     NextElementState;
    end
   else if nElName = ClusterRegistrationName[ConditionalRegistration] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lList:=Read_AdjectiveList;
     lCons:=Read_AdjectiveList;
     aItem^.nContent:=new(CClusterPtr,Init(lPos,lList,lCons,Read_Type));
     NextElementState;
    end
   else if nElName = ClusterRegistrationName[FunctorialRegistration] then
    begin
     lPos:=GetAttrPos;
     NextElementState;
     lTrm:=Read_Term;
     lCons:=Read_AdjectiveList;
     lType:=nil;
     if nState <> eEnd then
      lType:=Read_Type;
     aItem^.nContent:=new(FClusterPtr,Init(lPos,lTrm,lCons,lType));
     NextElementState;
    end;
  itIdentify:
   begin
    lPattern:=Read_Pattern;
    aItem^.nContent:=new(IdentifyRegistrationPtr,Init(lPos,Read_Pattern,lPattern,
                                             new(PList,Init(0))));
    while (nState = eStart) and (nElName = XMLElemName[elLociEquality]) do
     begin
      lPos:=GetAttrPos;
      NextElementState;
      lLocus:=Read_Locus;
      IdentifyRegistrationPtr(aItem^.nContent)^.nEqLociList^.Insert(new(LociEqualityPtr,Init(lPos,lLocus,Read_Locus)));
      NextElementState;
     end;
   end;
  itPropertyRegistration:
   begin
     lPropertySort:=Str2PropertyKind(aShape);
     case lPropertySort of
     sySethood:
       begin
        aItem^.nContent:=new(SethoodRegistrationPtr,Init(lPos,lPropertySort,Read_Type));
        SethoodRegistrationPtr(aItem^.nContent)^.nJustification:=Read_Justification;
       end;
     end;
   end;
  itReduction:
   begin
    lTrm:=Read_Term;
    aItem^.nContent:=new(ReduceRegistrationPtr,Init(lPos,Read_Term,lTrm));
   end;
  itPragma: ;
  end;
end;

function InWSMizFileObj.Read_TextProper: wsTextProperPtr;
 var lPos: Position;
begin
 NextElementState;
 lPos.Line:=GetIntAttr( XMLAttrName[atLine]);
 lPos.Col:= GetIntAttr( XMLAttrName[atCol]);
 result:=new(wsTextProperPtr,Init(GetAttr(XMLAttrName[atArticleID]),
                                  GetAttr(XMLAttrName[atArticleExt]),lPos));
 if nDisplayInformationOnScreen then
   DisplayLine(result^.nBlockPos.Line,0);
 CurPos:=result^.nBlockPos;
 if (nState= eStart) and (nElName = BlockName[blMain]) then
 begin
  NextElementState;
  while (nState= eStart) and (nElName = XMLElemName[elItem]) do
   result^.nItems^.Insert(Read_Item);
 end;
 NextElementState;
end;

function InWSMizFileObj.Read_Block: wsBlockPtr;
 var lPos: Position;
begin
 lPos.Line:=GetIntAttr( XMLAttrName[atLine]);
 lPos.Col:= GetIntAttr( XMLAttrName[atCol]);
 result:=new(WSBlockPtr,Init(Str2BlockKind(GetAttr(XMLAttrName[atKind])),lPos));
 if nDisplayInformationOnScreen then
   DisplayLine(result^.nBlockPos.Line,0);
 lPos.Line:=GetIntAttr( XMLAttrName[atPosLine]);
 lPos.Col:= GetIntAttr( XMLAttrName[atPosCol]);
 result^.nBlockEndPos:=lPos;
 CurPos:=result^.nBlockPos;
 NextElementState;
 while (nState= eStart) and (nElName = XMLElemName[elItem]) do
   result^.nItems^.Insert(Read_Item);
 CurPos:=result^.nBlockEndPos;
 NextElementState;
end;

function InWSMizFileObj.Read_Item: wsItemPtr;
 var lStartTagNbr: integer;
     lItemKind: ItemKind;
     lShape: string;
     lPos: Position;
begin
 lItemKind:=Str2ItemKind(GetAttr(XMLAttrName[atKind]));
 lPos.Line:=GetIntAttr( XMLAttrName[atLine]);
 lPos.Col:= GetIntAttr( XMLAttrName[atCol]);
 CurPos:=lPos;
 if nDisplayInformationOnScreen then
   DisplayLine(lPos.Line,0);
 result:=new(WSItemPtr,Init(lItemKind,lPos));
 lPos.Line:=GetIntAttr( XMLAttrName[atPosLine]);
 lPos.Col:= GetIntAttr( XMLAttrName[atPosCol]);
 result^.nItemEndPos:=lPos;
 result^.nContent:=nil;
 Read_ItemContentsAttr(result,lShape);
 NextElementState;
 lStartTagNbr := 0;
 if nState <> eEnd then
  begin
    Read_ItemContents(result,lShape);
    if (nState= eStart) and (nElName = XMLElemName[elBlock]) then
     result^.nBlock:=Read_Block
    else if result^.nContent = nil then
     begin
      repeat
       if nState = eStart then
        inc(lStartTagNbr)
       else dec(lStartTagNbr);
       NextElementState;
      until ((nState = eEnd) and (lStartTagNbr = 0)) or
            ((nState = eStart) and (nElName = XMLElemName[elBlock]));
      if (nState= eStart) and (nElName = XMLElemName[elBlock]) then
       result^.nBlock:=Read_Block;
     end;
  end;
 CurPos:=lPos;
 NextElementState;
end;

function Read_WSMizArticle(aFileName:string): wsTextProperPtr;
 var lInFile: InWSMizFilePtr;
begin
 InitWSLookupTables;
 lInFile:=new(InWSMizFilePtr,OpenFile(aFileName));
 result:=lInFile^.Read_TextProper;
 dispose(lInFile,Done);
 DisposeWSLookupTables;
end;

constructor WSMizarPrinterObj.OpenFile(const aFileName:string);
begin
 inherited InitFile(AFileName);
 rewrite(nFile);
 nIndent := 0;
 nDisplayInformationOnScreen:=false;
end;

destructor WSMizarPrinterObj.Done;
begin
 close(nFile);
 inherited Done;
end;

procedure WSMizarPrinterObj.Print_Char ( aChar: char );
begin
 write(nFile,aChar);
end;

procedure WSMizarPrinterObj.Print_NewLine;
begin
 writeln(nFile);
// Print_Char(#10)
end;

procedure WSMizarPrinterObj.Print_Number( const aNumber: integer);
begin
 write(nFile,aNumber);
 Print_Char(' ');
end;

procedure WSMizarPrinterObj.Print_String ( const aString: string );
 var i: integer;
begin
// for i:=1 to length(aString) do
//  Print_Char(aString[i]);
 write(nFile,XMLToStr(aString)); // ?? czy na pewno trzeba robic konwersje
 Print_Char(' ');
end;

//procedure WSMizarPrinterObj.Print_Spaces(aNbr:integer);
//begin
// while aNbr > 0 do begin Print_Char(' '); dec(aNbr) end;
//end;

procedure WSMizarPrinterObj.Print_Indent;
 var i:integer;
begin
 for i:=1 to nIndent do Print_Char(' ');
end;

procedure WSMizarPrinterObj.Print_Adjective(aAttr:AdjectiveExpressionPtr);
begin
 case aAttr^.nAdjectiveSort of
 wsAdjective:
 with AdjectivePtr(aAttr)^ do
  begin
   if  nArgs^.Count <> 0 then
     Print_TermList( nArgs );
   Print_String(AttributeName[nAdjectiveSymbol]);
  end;
 wsNegatedAdjective:
  begin
   Print_String(TokenName[sy_Non]);
   Print_Adjective(NegatedAdjectivePtr(aAttr)^.nArg);
  end;
 end;
end;

procedure WSMizarPrinterObj.Print_AdjectiveList(aCluster: PList);
 var i: integer;
begin
 with aCluster^ do
  for i:=0 to Count-1 do
   begin
    Print_Adjective( Items^[i]);
   end;
end;

procedure WSMizarPrinterObj.Print_Variable( aVar: VariablePtr);
begin
 with aVar ^ do
 begin
  Print_String(IdentRepr(nIdent));
 end;
end;

procedure WSMizarPrinterObj.Print_ImplicitlyQualifiedVariable( aSegm: ImplicitlyQualifiedSegmentPtr);
begin
 Print_Variable( aSegm^.nIdentifier);
end;

procedure WSMizarPrinterObj.Print_VariableSegment( aSegm: QualifiedSegmentPtr);
 var i: integer;
begin
 case aSegm^.nSegmentSort of
 ikImplQualifiedSegm:
   Print_ImplicitlyQualifiedVariable( ImplicitlyQualifiedSegmentPtr(aSegm));
 ikExplQualifiedSegm:
  with ExplicitlyQualifiedSegmentPtr(aSegm)^ do
  begin
   Print_Variable( nIdentifiers.Items^[0]);
   for i:=1 to nIdentifiers^.Count-1 do
    begin
     Print_String(',');
     Print_Variable( nIdentifiers^.Items^[i]);
    end;
   Print_String(TokenName[sy_Be]);
   Print_Type(nType);
  end;
 end;
end;

procedure WSMizarPrinterObj.Print_OpenTermList ( aTrmList:PList );
 var i: integer;
begin
 if aTrmList^.Count > 0 then
 begin
  Print_Term(aTrmList^.Items^[0]);
  for i:=1 to aTrmList^.Count-1 do
   begin
    Print_String(',');
    Print_Term(aTrmList^.Items^[i]);
   end;
 end;
end;

procedure WSMizarPrinterObj.Print_TermList ( aTrmList:PList );
 var i: integer;
begin
 if aTrmList^.Count > 0 then
 begin
  Print_String('(');
  Print_Term(aTrmList^.Items^[0]);
  for i:=1 to aTrmList^.Count-1 do
   begin
    Print_String(',');
    Print_Term(aTrmList^.Items^[i]);
   end;
  Print_String(')');
 end;
end;

procedure WSMizarPrinterObj.Print_Type ( aTyp: TypePtr);
begin
 with aTyp^ do
 begin
  case aTyp^.nTypeSort of
   wsStandardType:
    with StandardTypePtr(aTyp)^ do
    begin
     if  nArgs^.Count = 0 then
       Print_String(ModeName[nModeSymbol])
     else
      begin
       Print_String('(');
       Print_String(ModeName[nModeSymbol]);
       Print_String(TokenName[sy_Of]);
       Print_OpenTermList(nArgs);
       Print_String(')');
      end;
    end;
   wsStructureType:
    with StructTypePtr(aTyp)^ do
    begin
     if  nArgs^.Count = 0 then
       Print_String(StructureName[nStructSymbol])
     else
      begin
       Print_String('(');
       Print_String(StructureName[nStructSymbol]);
       Print_String(TokenName[sy_Over]);
       Print_OpenTermList(nArgs);
       Print_String(')');
      end;
    end;
   wsClusteredType:
    with ClusteredTypePtr(aTyp)^ do
    begin
//     Print_String('(');
     Print_AdjectiveList(nAdjectiveCluster);
     Print_Type(nType);
//     Print_String(')');
    end;
   wsErrorType:
    begin
//     Out_XElWithPos(TypeName[wsErrorType],nTypePos);
    end;
  end;
 end;
end;

procedure WSMizarPrinterObj.Print_BinaryFormula ( aFrm:BinaryFormulaPtr );
begin
 Print_String('(');
 Print_Formula(aFrm^.nLeftArg);
 case aFrm^.nFormulaSort of
  wsConjunctiveFormula: Print_String(TokenName[sy_Ampersand]);
  wsDisjunctiveFormula: Print_String(TokenName[sy_Or]);
  wsConditionalFormula: Print_String(TokenName[sy_Implies]);
  wsBiconditionalFormula: Print_String(TokenName[sy_Iff]);
  wsFlexaryConjunctiveFormula:
   begin Print_String(TokenName[sy_Ampersand]);
    Print_String(TokenName[sy_Ellipsis]);
    Print_String(TokenName[sy_Ampersand]);
   end;
  wsFlexaryDisjunctiveFormula:
   begin Print_String(TokenName[sy_Or]);
    Print_String(TokenName[sy_Ellipsis]);
    Print_String(TokenName[sy_Or]);
   end;
 end;
 Print_Formula(aFrm^.nRightArg);
 Print_String(')');
end;

procedure WSMizarPrinterObj.Print_PrivatePredicativeFormula ( aFrm: PrivatePredicativeFormulaPtr );
begin
 with PrivatePredicativeFormulaPtr(aFrm)^ do
 begin
  Print_String(IdentRepr(nPredIdNr));
  Print_String('[');
  Print_OpenTermList( nArgs);
  Print_String(']');
 end;
end;

procedure WSMizarPrinterObj.Print_Formula ( aFrm: FormulaPtr );
 var i: Integer;
     lNeg: boolean;
     lFrm: FormulaPtr;
begin
 case aFrm^.nFormulaSort of
  wsNegatedFormula:
   begin
    Print_String(TokenName[sy_Not]);
    Print_Formula(NegativeFormulaPtr(aFrm)^.nArg);
   end;
  wsConjunctiveFormula,wsDisjunctiveFormula,
  wsConditionalFormula,wsBiconditionalFormula,
  wsFlexaryConjunctiveFormula,wsFlexaryDisjunctiveFormula:
    Print_BinaryFormula(BinaryFormulaPtr(aFrm));
  wsPredicativeFormula:
   with PredicativeFormulaPtr(aFrm)^ do
   begin
    Print_String('(');
    if nLeftArgs^.Count <> 0 then
     begin
      Print_OpenTermList( nLeftArgs);
     end;
    Print_String(PredicateName[nPredNr]);
    if nRightArgs^.Count <> 0 then
     begin
      Print_OpenTermList( nRightArgs);
     end;
    Print_String(')');
   end;
  wsMultiPredicativeFormula:
   with MultiPredicativeFormulaPtr(aFrm)^ do
   begin
    Print_String('(');
    lFrm:=nScraps.Items^[0];
    lNeg:=lFrm^.nFormulaSort = wsNegatedFormula;
    if lNeg then
     lFrm:=NegativeFormulaPtr(lFrm)^.nArg;
    with PredicativeFormulaPtr(lFrm)^ do
    begin
     if nLeftArgs^.Count <> 0 then
      Print_OpenTermList( nLeftArgs);
     if lNeg then
      begin
       Print_String(TokenName[sy_Does]);
       Print_String(TokenName[sy_Not]);
      end;
     Print_String(PredicateName[nPredNr]);
     if nRightArgs^.Count <> 0 then
      Print_OpenTermList( nRightArgs);
    end;
    for i:=1 to nScraps.Count - 1 do
     begin
      lFrm:=nScraps.Items^[i];
      lNeg:=lFrm^.nFormulaSort = wsNegatedFormula;
      if lNeg then
       lFrm:=NegativeFormulaPtr(lFrm)^.nArg;
      with RightSideOfPredicativeFormulaPtr(lFrm)^ do
      begin
       if lNeg then
        begin
         Print_String(TokenName[sy_Does]);
         Print_String(TokenName[sy_Not]);
        end;
       Print_String(PredicateName[nPredNr]);
       if nRightArgs^.Count <> 0 then
        Print_OpenTermList( nRightArgs);
      end;
     end;
    Print_String(')');
   end;
  wsPrivatePredicateFormula:
   Print_PrivatePredicativeFormula ( PrivatePredicativeFormulaPtr(aFrm));
  wsAttributiveFormula:
   with AttributiveFormulaPtr(aFrm)^ do
   begin
    Print_String('(');
    Print_Term(nSubject);
    Print_String(TokenName[sy_Is]);
    Print_AdjectiveList(nAdjectives);
    Print_String(')');
   end;
  wsQualifyingFormula:
   with QualifyingFormulaPtr(aFrm)^ do
   begin
    Print_String('(');
    Print_Term(nSubject);
    Print_String(TokenName[sy_Is]);
    Print_Type(nType);
    Print_String(')');
   end;
  wsUniversalFormula:
   with QuantifiedFormulaPtr( aFrm)^ do
   begin
    Print_String('(');
    Print_String(TokenName[sy_For]);
    Print_VariableSegment(QuantifiedFormulaPtr(aFrm)^.nSegment);
    Print_String(TokenName[sy_Holds]);
    Print_Formula(QuantifiedFormulaPtr(aFrm)^.nScope);
    Print_String(')');
   end;
  wsExistentialFormula:
   with QuantifiedFormulaPtr( aFrm)^ do
   begin
    Print_String('(');
    Print_String(TokenName[sy_Ex]);
    Print_VariableSegment(QuantifiedFormulaPtr(aFrm)^.nSegment);
    Print_String(TokenName[sy_St]);
    Print_Formula(QuantifiedFormulaPtr(aFrm)^.nScope);
    Print_String(')');
   end;
  wsContradiction:
    begin
     Print_String(TokenName[sy_Contradiction]);
    end;
   wsThesis:
    begin
     Print_String(TokenName[sy_Thesis]);
    end;
  wsErrorFormula:
    begin
//     Out_XElWithPos(FormulaName[wsErrorFormula],aFrm^.nFormulaPos);
    end;
 end;
end;


procedure WSMizarPrinterObj.Print_SimpleTermTerm ( aTrm: SimpleTermPtr );
begin
  Print_String(IdentRepr(SimpleTermPtr(aTrm)^.nIdent));
end;

procedure WSMizarPrinterObj.Print_PrivateFunctorTerm ( aTrm: PrivateFunctorTermPtr );
begin
  Print_String(IdentRepr(aTrm^.nFunctorIdent));
  Print_String('(');
  Print_OpenTermList(aTrm^.nArgs);
  Print_String(')');
end;

procedure WSMizarPrinterObj.Print_Term ( aTrm: TermPtr );
 var i,j: integer;
     lPrintWhere: boolean;
begin
 case aTrm^.nTermSort of
  wsPlaceholderTerm:
   begin
    Print_Char('$');
    Print_Number(PlaceholderTermPtr(aTrm)^.nLocusNr);
   end;
  wsSimpleTerm:
   begin
    Print_SimpleTermTerm(SimpleTermPtr(aTrm));
   end;
  wsNumeralTerm:
   begin
    Print_Number(NumeralTermPtr(aTrm)^.nValue);
   end;
  wsInfixTerm:
   with InfixTermPtr(aTrm)^ do
   begin
    Print_String('(');
    if nLeftArgs^.Count <> 0 then
     begin
      Print_TermList( nLeftArgs);
     end;
    Print_String(FunctorName[nFunctorSymbol]);
    if nRightArgs^.Count <> 0 then
     begin
      Print_TermList( nRightArgs);
     end;
    Print_String(')');
   end;
   wsCircumfixTerm:
    with CircumfixTermPtr(aTrm)^ do
    begin
     Print_String(LeftBracketName[nLeftBracketSymbol]);
     Print_OpenTermList(nArgs);
     Print_String(RightBracketName[nRightBracketSymbol]);
    end;
   wsPrivateFunctorTerm:
    Print_PrivateFunctorTerm(PrivateFunctorTermPtr(aTrm));
   wsAggregateTerm:
    with AggregateTermPtr(aTrm)^ do
    begin
      Print_String(StructureName[nStructSymbol]);
      Print_String(TokenName[sy_StructLeftBracket]);
      Print_OpenTermList( nArgs);
      Print_String(TokenName[sy_StructRightBracket]);
    end;
   wsSelectorTerm:
    with SelectorTermPtr(aTrm)^ do
    begin
     Print_String('(');
     Print_String(TokenName[sy_The]);
     Print_String(SelectorName[nSelectorSymbol]);
     Print_String(TokenName[sy_Of]);
     Print_Term( nArg);
     Print_String(')');
    end;
   wsInternalSelectorTerm:
    with InternalSelectorTermPtr(aTrm)^ do
    begin
     Print_String(TokenName[sy_The]);
     Print_String(SelectorName[nSelectorSymbol]);
    end;
   wsForgetfulFunctorTerm:
    with ForgetfulFunctorTermPtr(aTrm)^ do
    begin
     Print_String('(');
     Print_String(TokenName[sy_The]);
     Print_String(StructureName[nStructSymbol]);
     Print_String(TokenName[sy_Of]);
     Print_Term( nArg);
     Print_String(')');
    end;
   wsInternalForgetfulFunctorTerm:
    with InternalForgetfulFunctorTermPtr(aTrm)^ do
    begin
     Print_String('(');
      Print_String(TokenName[sy_The]);
      Print_String(StructureName[nStructSymbol]);
     Print_String(')');
    end;
   wsFraenkelTerm:
    with FraenkelTermPtr(aTrm)^ do
    begin
     Print_String('{');
     Print_Term(nSample);
     if nPostqualification^.Count > 0 then
      begin
       lPrintWhere:=true;
       for i := 0 to nPostqualification^.Count - 1 do
        case QualifiedSegmentPtr(nPostqualification^.Items^[i])^.nSegmentSort of
        ikImplQualifiedSegm:
         with ImplicitlyQualifiedSegmentPtr(nPostqualification^.Items^[i])^ do
         begin
          Print_String(TokenName[sy_Where]);
          Print_Variable( nIdentifier);
//          lPrintWhere:=true;
         end;
        ikExplQualifiedSegm:
         with ExplicitlyQualifiedSegmentPtr(nPostqualification^.Items^[i])^ do
          begin
           if lPrintWhere then
            begin
             Print_String(TokenName[sy_Where]);
             lPrintWhere:=false;
            end;
           Print_Variable( nIdentifiers.Items^[0]);
           for j:=1 to nIdentifiers^.Count-1 do
            begin
             Print_String(',');
             Print_Variable( nIdentifiers^.Items^[j]);
            end;
           Print_String(TokenName[sy_Is]);
           Print_Type(nType);
           if i < nPostqualification^.Count - 1 then Print_String(',');
          end;
        end;
      end;
     Print_String(':');
     Print_Formula(nFormula);
     Print_String('}');
    end;
   wsSimpleFraenkelTerm:
    with SimpleFraenkelTermPtr(aTrm)^ do
    begin
     Print_String('(');
     Print_String(TokenName[sy_The]);
     Print_String(TokenName[sy_Set]);
     Print_String(TokenName[sy_Of]);
     Print_String(TokenName[sy_All]);
     Print_Term(nSample);
     if nPostqualification^.Count > 0 then
      begin
       lPrintWhere:=true;
       for i := 0 to nPostqualification^.Count - 1 do
        case QualifiedSegmentPtr(nPostqualification^.Items^[i])^.nSegmentSort of
        ikImplQualifiedSegm:
         with ImplicitlyQualifiedSegmentPtr(nPostqualification^.Items^[i])^ do
         begin
          Print_String(TokenName[sy_Where]);
          Print_Variable( nIdentifier);
//          lPrintWhere:=true;
         end;
        ikExplQualifiedSegm:
         with ExplicitlyQualifiedSegmentPtr(nPostqualification^.Items^[i])^ do
          begin
           if lPrintWhere then
            begin
             Print_String(TokenName[sy_Where]);
             lPrintWhere:=false;
            end;
           Print_Variable( nIdentifiers.Items^[0]);
           for j:=1 to nIdentifiers^.Count-1 do
            begin
             Print_String(',');
             Print_Variable( nIdentifiers^.Items^[j]);
            end;
           Print_String(TokenName[sy_Is]);
           Print_Type(nType);
           if i < nPostqualification^.Count - 1 then Print_String(',');
          end;
        end;
      end;
     Print_String(')');
    end;
   wsQualificationTerm:
    with QualifiedTermPtr(aTrm)^ do
    begin
     Print_String('(');
     Print_Term(nSubject);
     Print_String(TokenName[sy_Qua]);
     Print_Type(nQualification);
     Print_String(')');
    end;
   wsExactlyTerm:
    with ExactlyTermPtr(aTrm)^ do
    begin
     Print_Term(nSubject);
     Print_String(TokenName[sy_Exactly]);
    end;
   wsGlobalChoiceTerm:
    begin
     Print_String('(');
     Print_String(TokenName[sy_The]);
     Print_Type(ChoiceTermPtr(aTrm)^.nChoiceType);
     Print_String(')');
    end;
   wsItTerm:
    begin
     Print_String(TokenName[sy_It]);
    end;
   wsErrorTerm:
//   Out_XEl1( TermName[wsErrorTerm]);
  end;
end;

procedure WSMizarPrinterObj.Print_TypeList ( aTypeList:PList );
 var i: integer;
begin
 if aTypeList^.Count > 0 then
 begin
  Print_Type(aTypeList^.Items^[0]);
  for i:=1 to aTypeList^.Count-1 do
   begin
    Print_String(',');
    Print_Type(aTypeList^.Items^[i]);
   end;
 end;
end;

procedure WSMizarPrinterObj.Print_Label(aLab:LabelPtr);
begin
 if (aLab <> nil) and (aLab.nLabelIdNr > 0) then
  begin
   Print_String(IdentRepr(aLab^.nLabelIdNr));
   Print_String(':');
  end;
end;

procedure WSMizarPrinterObj.Print_Proposition(aProp:PropositionPtr);
begin
 Print_Label(aProp^.nLab);
 Print_Formula(aProp^.nSentence);
end;

procedure WSMizarPrinterObj.Print_CompactStatement(aCStm:CompactStatementPtr; aBlock:wsBlockPtr);
begin
  with aCStm^ do
  begin
   Print_Proposition(nProp);
   Print_Justification(nJustification,aBlock);
  end;
end;


procedure WSMizarPrinterObj.Print_Linkage;
begin
 Print_String(TokenName[sy_Then]);
end;

procedure WSMizarPrinterObj.Print_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr);
 var i: integer;
begin
 case aRStm^.nStatementSort of
 stDiffuseStatement:
  begin
   Print_Label(DiffuseStatementPtr(aRStm)^.nLab);
   Print_Block(aBlock);
  end;
 stCompactStatement:
  begin
   if (CompactStatementPtr(aRStm)^.nJustification^.nInfSort = infStraightforwardJustification) and
      StraightforwardJustificationPtr(CompactStatementPtr(aRStm)^.nJustification)^.nLinked then
    begin
     Print_Linkage;
    end;
   Print_CompactStatement(CompactStatementPtr(aRStm),aBlock);
  end;
 stIterativeEquality:
  begin
   if (CompactStatementPtr(aRStm)^.nJustification^.nInfSort = infStraightforwardJustification) and
      StraightforwardJustificationPtr(CompactStatementPtr(aRStm)^.nJustification)^.nLinked then
    begin
     Print_Linkage;
    end;
   Print_CompactStatement(CompactStatementPtr(aRStm),nil);
   with IterativeEqualityPtr(aRStm)^ do
    for i := 0 to nIterSteps^.Count - 1 do
     with IterativeStepPtr(nIterSteps^.Items^[i])^ do
     begin
      Print_NewLine;
      Print_String(TokenName[sy_DotEquals]);
      Print_Term(nTerm);
      Print_Justification(nJustification,nil);
     end;
  end;
 end;
end;

procedure WSMizarPrinterObj.Print_Reference(aRef: LocalReferencePtr);
begin
 Print_String(IdentRepr(aRef^.nLabId));
end;

procedure WSMizarPrinterObj.Print_References(aRefs: PList);
 var i: integer;
begin
 for i:= 0 to aRefs^.Count-1 do
  with ReferencePtr(aRefs^.Items^[i])^ do
 begin
  case nRefSort of
   LocalReference:
    begin
     Print_Reference(aRefs^.Items^[i]);
    end;
   TheoremReference:
    begin
     Print_String(MMLIdentifierName[TheoremReferencePtr(aRefs^.Items^[i])^.nArticleNr]);
     Print_String(':');
     Print_Number(TheoremReferencePtr(aRefs^.Items^[i])^.nTheoNr);
    end;
   DefinitionReference:
    begin
     Print_String(MMLIdentifierName[DefinitionReferencePtr(aRefs^.Items^[i])^.nArticleNr]);
     Print_String(':');
     Print_String('def');
     Print_Number(DefinitionReferencePtr(aRefs^.Items^[i])^.nDEfNr);
    end;
  end;
  if i < aRefs^.Count-1 then
    Print_String(',');
 end;
end;

procedure WSMizarPrinterObj.Print_StraightforwardJustification(aInf: StraightforwardJustificationPtr);
begin
  with aInf^ do
  begin
   if nReferences^.Count <> 0 then
    begin
     Print_String(TokenName[sy_By]);
     Print_References(nReferences);
    end;
  end;
end;

procedure WSMizarPrinterObj.Print_SchemeNameInJustification(aInf: SchemeJustificationPtr);
begin
  Print_String(IdentRepr(aInf^.nSchemeIdNr));
end;

procedure WSMizarPrinterObj.Print_SchemeJustification(aInf: SchemeJustificationPtr);
begin
  with aInf^ do
  begin
    Print_String(TokenName[sy_From]);
    if nSchFileNr > 0 then
     begin
      Print_String(MMLIdentifierName[nSchFileNr]);
      Print_String(':');
      Print_String('sch');
      Print_Number(nSchemeIdNr);
     end
    else if nSchemeIdNr > 0 then
     Print_SchemeNameInJustification(aInf);
    if nReferences^.Count > 0 then
     begin
      Print_String('(');
      Print_References(nReferences);
      Print_String(')');
     end;
  end;
end;

procedure WSMizarPrinterObj.Print_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr);
begin
  case aInf^.nInfSort of
   infStraightforwardJustification:
     Print_StraightforwardJustification(StraightforwardJustificationPtr(aInf));
   infSchemeJustification:
     Print_SchemeJustification(SchemeJustificationPtr(aInf));
   infError,infSkippedProof:
    begin
    end;
   infProof:
    Print_Block(aBlock);
  end;
end;

procedure WSMizarPrinterObj.Print_Conditions(aCond: PList);
 var i: integer;
begin
 Print_String(TokenName[sy_That]);
 Print_NewLine;
 Print_Proposition(aCond^.Items^[0]);
 for i:=1 to aCond^.Count-1 do
  begin
   Print_String(TokenName[sy_And]);
   Print_NewLine;
   Print_Proposition(aCond^.Items^[i]);
  end;
end;

procedure WSMizarPrinterObj.Print_AssumptionConditions(aCond: AssumptionPtr);
begin
 case aCond^.nAssumptionSort of
 SingleAssumption:
  begin
   Print_Proposition(SingleAssumptionPtr(aCond)^.nProp);
  end;
 CollectiveAssumption:
  begin
   Print_Conditions(CollectiveAssumptionPtr(aCond)^.nConditions);
  end;
 end;
end;

procedure WSMizarPrinterObj.Print_Locus( aLocus: LocusPtr);
begin
 with aLocus ^ do
 begin
   Print_String(IdentRepr(nVarId));
 end;
end;

procedure WSMizarPrinterObj.Print_Loci( aLoci: PList);
 var i: integer;
begin
 if (aLoci = nil) or (aLoci^.Count = 0) then
 else
  begin
   Print_Locus(aLoci^.Items^[0]);
   for i:=1 to aLoci^.Count-1 do
    begin
     Print_String(',');
     Print_Locus(aLoci^.Items^[i]);
    end;
  end;
end;

procedure WSMizarPrinterObj.Print_Pattern(aPattern: PatternPtr);
begin
 case aPattern^.nPatternSort of
 itDefPred:
  with PredicatePatternPtr(aPattern)^ do
  begin
   Print_Loci(nLeftArgs);
   Print_String(PredicateName[nPredSymbol]);
   Print_Loci(nRightArgs);
  end;
 itDefFunc:
  begin
    case FunctorPatternPtr(aPattern)^.nFunctKind of
     InfixFunctor:
     with InfixFunctorPatternPtr(aPattern)^ do
      begin
       if (nLeftArgs <> nil) and (nLeftArgs^.Count >1) then
         Print_String('(');
       Print_Loci(nLeftArgs);
       if (nLeftArgs <> nil) and (nLeftArgs^.Count >1) then
         Print_String(')');
       Print_String(FunctorName[nOperSymb]);
       if (nRightArgs <> nil) and (nRightArgs^.Count >1) then
         Print_String('(');
       Print_Loci(nRightArgs);
       if (nRightArgs <> nil) and (nRightArgs^.Count >1) then
         Print_String(')');
      end;
     CircumfixFunctor:
     with CircumfixFunctorPatternPtr(aPattern)^ do
      begin
       Print_String(LeftBracketName[nLeftBracketSymb]);
       Print_Loci(nArgs);
       Print_String(RightBracketName[nRightBracketSymb]);
      end;
    end;
  end;
 itDefMode:
  with ModePatternPtr(aPattern)^ do
  begin
   Print_String(ModeName[nModeSymbol]);
   if (nArgs <> nil) and (nArgs^.Count > 0) then
   begin
    Print_String(TokenName[sy_Of]);
    Print_Loci(nArgs);
   end;
  end;
 itDefAttr:
  with AttributePatternPtr(aPattern)^ do
  begin
   Print_Locus(nArg);
   Print_String(TokenName[sy_Is]);
   Print_Loci(nArgs);
   Print_String(AttributeName[nAttrSymbol]);
  end;
 end;
end;

procedure WSMizarPrinterObj.Print_Definiens(aDef:DefiniensPtr);
 var i: integer;
begin
  if aDef <> nil then
  with DefiniensPtr(aDef)^ do
   begin
    case nDefSort of
    SimpleDefiniens:
     begin
      if (nDefLabel <> nil) and (nDefLabel^.nLabelIdNr > 0) then
       begin
        Print_String(':');
        Print_Label(nDefLabel);
       end;
      with SimpleDefiniensPtr(aDef)^,nExpression^ do
       case nExprKind of
        exTerm: Print_Term(TermPtr(nExpr));
        exFormula: Print_Formula(FormulaPtr(nExpr));
       end;
     end;
    ConditionalDefiniens:
     begin
      if (nDefLabel <> nil) and (nDefLabel^.nLabelIdNr > 0) then
       begin
        Print_String(':');
        Print_Label(nDefLabel);
       end;
      with ConditionalDefiniensPtr(aDef)^ do
      begin
       for i:=0 to nConditionalDefiniensList^.Count-1 do
        begin
         with PartDefPtr(nConditionalDefiniensList^.Items^[I])^ do
          begin
           with nPartDefiniens^ do
            case nExprKind of
             exTerm: Print_Term(TermPtr(nExpr));
             exFormula: Print_Formula(FormulaPtr(nExpr));
            end;
           Print_String(TokenName[sy_If]);
           Print_Formula(nGuard);
          end;
         if (i>=0) and (i<nConditionalDefiniensList^.Count-1) then
          begin
           Print_String(',');
           Print_NewLine;
          end;
        end;
       if nOtherwise <> nil then
        with nOtherwise^ do
         begin
          Print_String(TokenName[sy_Otherwise]);
          case nExprKind of
           exTerm: Print_Term(TermPtr(nExpr));
           exFormula: Print_Formula(FormulaPtr(nExpr));
          end;
         end;
      end;
     end;
    end;
   end;
end;

procedure WSMizarPrinterObj.Print_Block(aWSBlock:WSBlockPtr);
 var i,lIndent: integer;
begin
 with aWSBlock^ do
 begin
  lIndent:=nIndent;
  Print_NewLine;
  Print_Indent;
  case nBlockKind of
  blDiffuse:
   begin
    Print_String(TokenName[sy_Now]);
    Print_NewLine;
   end;
  blHereby:
   begin
    Print_String(TokenName[sy_Now]);
    Print_NewLine;
   end;
  blProof:
   begin
    Print_String(TokenName[sy_Proof]);
    Print_NewLine;
   end;
  blDefinition:
   begin
    Print_String(TokenName[sy_Definition]);
    Print_NewLine;
   end;
  blNotation:
   begin
    Print_String(TokenName[sy_Notation]);
    Print_NewLine;
   end;
  blRegistration:
   begin
    Print_String(TokenName[sy_Registration]);
    Print_NewLine;
   end;
  blCase:
   Print_String(TokenName[sy_Case]);
  blSuppose:
   Print_String(TokenName[sy_Suppose]);
  blPublicScheme:
   ;
  end;
  for i := 0 to nItems^.Count - 1 do
   begin
    Print_Item(nItems^.Items^[i]);
   end;
  nIndent:=lIndent;
  Print_Indent;
  Print_String(TokenName[sy_End]);
 end;
end;

procedure WSMizarPrinterObj.Print_TextProper(aWSTextProper:WSTextProperPtr);
 var i: integer;
begin
 with aWSTextProper^ do
 begin
//Print_String(TokenName[sy_Begin]);
//Print_NewLine;
  for i := 0 to nItems^.Count - 1 do
    Print_Item(nItems^.Items^[i]);
 end;
end;

procedure WSMizarPrinterObj.Print_ReservedType(aResType: TypePtr);
begin
 Print_Type(aResType);
end;

procedure WSMizarPrinterObj.Print_SchemeNameInSchemeHead(aSch: SchemePtr);
begin
  Print_String(IdentRepr(aSch^.nSchemeIdNr));
end;

procedure WSMizarPrinterObj.Print_Item(aWSItem:WSItemPtr);
 var i,j,lIndent: integer;
begin
 with aWSItem^ do
 begin  CurPos:=nItemPos;
  if nDisplayInformationOnScreen then
    DisplayLine(CurPos.Line,ErrorNbr);
  case nItemKind of
  itDefinition:
   begin
    Print_Block(nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itSchemeBlock:
   begin
    Print_Block(nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itSchemeHead:
   with SchemePtr(nContent)^ do
   begin
    Print_String(TokenName[sy_Scheme]);
    Print_SchemeNameInSchemeHead(SchemePtr(nContent));
    Print_String('{');
    for j:=0 to nSchemeParams^.Count-1 do
    begin
     case SchemeSegmentPtr(nSchemeParams^.Items^[j])^.nSegmSort of
      PredicateSegment:
       with PredicateSegmentPtr(nSchemeParams^.Items^[j])^ do
       begin
        Print_Variable( nVars^.Items^[0]);
        for i:=1 to nVars^.Count-1 do
         begin
          Print_String(',');
          Print_Variable( nVars^.Items^[i]);
         end;
        Print_String('[');
        Print_TypeList(nTypeExpList);
        Print_String(']');
       end;
      FunctorSegment:
       with FunctorSegmentPtr(nSchemeParams^.Items^[j])^ do
       begin
        Print_Variable( nVars^.Items^[0]);
        for i:=1 to nVars.Count-1 do
         begin
          Print_String(',');
          Print_Variable( nVars^.Items^[i]);
         end;
        Print_String('(');
        Print_TypeList(nTypeExpList);
        Print_String(')');
        Print_String(TokenName[sy_Arrow]);
        Print_Type(nSpecification);
       end;
     end;
     if (j >= 0) and (j < nSchemeParams^.Count-1) then
       Print_String(',');
    end;
    Print_String('}');
    Print_String(':');
    Print_Newline;
    Print_Formula(nSchemeConclusion);
    Print_NewLine;
    if (nSchemePremises <> nil) and (nSchemePremises^.Count > 0) then
     begin
      Print_String(TokenName[sy_Provided]);
      Print_Proposition(nSchemePremises^.Items^[0]);
      for i:=1 to nSchemePremises^.Count-1 do
       begin
        Print_String(TokenName[sy_And]);
        Print_NewLine;
        Print_Proposition(nSchemePremises^.Items^[i]);
       end;
     end;
    Print_String(TokenName[sy_Proof]);
    Print_NewLine;
   end;
  itTheorem:
   with CompactStatementPtr(nContent)^ do
   begin
    Print_NewLine;
    nIndent:=0;
    Print_String(TokenName[sy_Theorem]);
    Print_Label(nProp^.nLab);
    Print_NewLine;
    nIndent:=2;
    Print_Indent;
    Print_Formula(nProp^.nSentence);
    nIndent:=0;
    Print_Justification(nJustification,nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itAxiom:
   begin

   end;
  itReservation:
   with ReservationSegmentPtr(nContent)^ do
   begin
    Print_NewLine;
    Print_String(TokenName[sy_reserve]);
    Print_Variable( nIdentifiers.Items^[0]);
    for i:=1 to nIdentifiers^.Count-1 do
     begin
      Print_String(',');
      Print_Variable( nIdentifiers^.Items^[i]);
     end;
    Print_String(TokenName[sy_For]);
    Print_ReservedType(nResType);
    Print_String(';');
    Print_NewLine;
   end;
  itSection:
   begin
    Print_NewLine;
    Print_String(TokenName[sy_Begin]);
    Print_NewLine;
   end;
  itRegularStatement:
   begin
    Print_RegularStatement(RegularStatementPtr(nContent),nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itChoice:
   with ChoiceStatementPtr(nContent)^ do
   begin
   if (nJustification^.nInfSort = infStraightforwardJustification) and
      StraightforwardJustificationPtr(nJustification)^.nLinked then
    begin
     Print_Linkage;
    end;
    Print_String(TokenName[sy_Consider]);
    Print_VariableSegment( nQualVars^.Items^[0]);
    for i:= 1 to  nQualVars^.Count-1 do
     begin
      Print_String(',');
      Print_VariableSegment( nQualVars^.Items^[i]);
     end;
    if (nConditions <> nil) and (nConditions^.Count > 0) then
     begin
      Print_String(TokenName[sy_Such]);
      Print_Conditions(nConditions);
     end;
    Print_Justification(nJustification,nil);
    Print_String(';');
    Print_NewLine;
   end;
  itReconsider:
   with TypeChangingStatementPtr(nContent)^ do
   begin
   if (nJustification^.nInfSort = infStraightforwardJustification) and
      StraightforwardJustificationPtr(nJustification)^.nLinked then
    begin
     Print_Linkage;
    end;
    Print_String(TokenName[sy_Reconsider]);
    for i:=0 to nTypeChangeList^.Count-1 do
     begin
      case TypeChangePtr(nTypeChangeList^.Items^[i])^.nTypeChangeKind of
       Equating:
        begin
         Print_Variable(TypeChangePtr(nTypeChangeList^.Items^[i])^.nVar);
         Print_String('=');
         Print_Term(TypeChangePtr(nTypeChangeList^.Items^[i])^.nTermExpr);
        end;
       VariableIdentifier:
        begin
         Print_Variable(TypeChangePtr(nTypeChangeList.Items^[i])^.nVar);
        end;
      end;
      if (i >= 0) and (i < nTypeChangeList^.Count-1) then
       Print_String(',');
     end;
    Print_String(TokenName[sy_As]);
    Print_Type(nTypeExpr);
    Print_Justification(nJustification,nil);
    Print_String(';');
    Print_NewLine;
   end;
  itPrivFuncDefinition:
   with PrivateFunctorDefinitionPtr(nContent)^ do
   begin
    Print_String(TokenName[sy_DefFunc]);
    Print_Variable(nFuncId);
    Print_String('(');
    Print_TypeList(nTypeExpList);
    Print_String(')');
    Print_String('=');
    Print_Term(nTermExpr);
    Print_String(';');
    Print_NewLine;
   end;
  itPrivPredDefinition:
   with PrivatePredicateDefinitionPtr(nContent)^ do
   begin
    Print_String(TokenName[sy_DefPred]);
    Print_Variable(nPredId);
    Print_String('[');
    Print_TypeList(nTypeExpList);
    Print_String(']');
    Print_String(TokenName[sy_Means]);
    Print_Formula(nSentence);
    Print_String(';');
    Print_NewLine;
   end;
  itConstantDefinition:
   with ConstantDefinitionPtr(nContent)^ do
   begin
    Print_String(TokenName[sy_Set]);
    Print_Variable(nVarId);
    Print_String('=');
    Print_Term(nTermExpr);
    Print_String(';');
    Print_NewLine;
   end;
  itLociDeclaration,
  itGeneralization:
   begin
    Print_String(TokenName[sy_Let]);
    Print_VariableSegment( QualifiedSegmentPtr(nContent));
    Print_String(';');
    Print_NewLine;
   end;
  itAssumption:
   begin
    Print_String(TokenName[sy_Assume]);
    Print_AssumptionConditions(AssumptionPtr(nContent));
    Print_String(';');
    Print_NewLine;
   end;
  itExistentialAssumption:
   with ExistentialAssumptionPtr(nContent)^ do
    begin
     Print_String(TokenName[sy_Given]);
     Print_VariableSegment( nQVars^.Items^[0]);
     for i := 1 to  nQVars^.Count-1 do
      begin
       Print_String(',');
       Print_VariableSegment( nQVars^.Items^[i]);
      end;
     Print_String(TokenName[sy_Such]);
     Print_String(TokenName[sy_That]);
     Print_NewLine;
     Print_Proposition(nConditions^.Items^[0]);
     for i:=1 to nConditions^.Count-1 do
      begin
       Print_String(TokenName[sy_And]);
       Print_NewLine;
       Print_Proposition(nConditions^.Items^[i]);
      end;
     Print_String(';');
     Print_NewLine;
    end;
  itExemplification:
   with ExamplePtr(nContent)^ do
   begin
    Print_String(TokenName[sy_Take]);
    if nVarId <> nil then
     begin
      Print_Variable(nVarId);
      if nTermExpr <> nil then
      begin
       Print_String('=');
      end;
     end;
    if nTermExpr <> nil then
      Print_Term(nTermExpr);
    Print_String(';');
    Print_NewLine;
   end;
  itPerCases:
   begin
   if (JustificationPtr(nContent)^.nInfSort = infStraightforwardJustification) and
      StraightforwardJustificationPtr(nContent)^.nLinked then
    begin
     Print_Linkage;
    end;
    Print_String(TokenName[sy_Per]);
    Print_String(TokenName[sy_Cases]);
    Print_Justification(JustificationPtr(nContent),nil);
    Print_String(';');
    Print_NewLine;
   end;
  itConclusion:
   begin
    Print_String(TokenName[sy_Thus]);
    Print_RegularStatement(RegularStatementPtr(nContent),nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itCaseBlock:
    begin
     Print_Block(nBlock);
     Print_String(';');
     Print_NewLine;
    end;
  itCaseHead,
  itSupposeHead:
   begin
    Print_AssumptionConditions(AssumptionPtr(nContent));
    Print_String(';');
    Print_NewLine;
   end;
  itCorrCond:
   begin
    Print_String(CorrectnessName[CorrectnessConditionPtr(nContent)^.nCorrCondSort]);
    Print_Justification(CorrectnessConditionPtr(nContent)^.nJustification,nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itCorrectness:
   begin
    Print_String(TokenName[sy_Correctness]);
    Print_Justification(CorrectnessPtr(nContent)^.nJustification,nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itProperty:
   begin
    Print_String(PropertyName[PropertyPtr(nContent)^.nPropertySort]);
    Print_Justification(PropertyPtr(nContent)^.nJustification,nBlock);
    Print_String(';');
    Print_NewLine;
   end;
  itDefMode:
   with ModeDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
      Print_String(TokenName[sy_Redefine]);
     end;
    Print_String(TokenName[sy_Mode]);
    Print_Pattern(nDefModePattern);
    case nDefKind of
    defExpandableMode:
     begin
      Print_String(TokenName[sy_Is]);
      Print_Type(ExpandableModeDefinitionPtr(nContent)^.nExpansion);
     end;
    defStandardMode:
     with StandardModeDefinitionPtr(nContent)^ do
     begin
      if nSpecification <> nil then
       begin
        Print_String(TokenName[sy_Arrow]);
        Print_Type(nSpecification);
       end;
      if nDefiniens <> nil then
       begin
        Print_String(TokenName[sy_Means]);
        Print_NewLine;
        Print_Definiens(nDefiniens);
       end;
     end;
    end;
    Print_String(';');
    Print_NewLine;
   end;
  itDefAttr:
   with AttributeDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
      Print_String(TokenName[sy_Redefine]);
     end;
    Print_String(TokenName[sy_Attr]);
    Print_Pattern(nDefAttrPattern);
    Print_String(TokenName[sy_Means]);
    Print_NewLine;
    Print_Definiens(nDefiniens);
    Print_String(';');
    Print_NewLine;
   end;
  itDefPred:
   with PredicateDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
      Print_String(TokenName[sy_Redefine]);
     end;
    Print_String(TokenName[sy_Pred]);
    Print_Pattern(nDefPredPattern);
    if nDefiniens <> nil then
    begin
     Print_String(TokenName[sy_Means]);
     Print_NewLine;
     Print_Definiens(nDefiniens);
    end;
    Print_String(';');
    Print_NewLine;
   end;
  itDefFunc:
   with FunctorDefinitionPtr(nContent)^ do
   begin
    if nRedefinition then
     begin
      Print_String(TokenName[sy_Redefine]);
     end;
    Print_String(TokenName[sy_Func]);
    Print_Pattern(nDefFuncPattern);
    if nSpecification <> nil then
     begin
      Print_String(TokenName[sy_Arrow]);
      Print_Type(nSpecification);
     end;
    case nDefiningWay of
     dfEmpty:;
     dfMeans:
     begin
      Print_String(TokenName[sy_Means]);
      Print_NewLine;
     end;
     dfEquals:
     begin
      Print_String(TokenName[sy_Equals]);
     end;
    end;
    Print_Definiens(nDefiniens);
    Print_String(';');
    Print_NewLine;
   end;
  itDefStruct:
   with StructureDefinitionPtr(nContent)^ do
   begin
     Print_String(TokenName[sy_Struct]);
     if nAncestors^.Count > 0 then
      begin
       Print_String('(');
       Print_Type(nAncestors^.Items^[0]);
       for i := 1 to nAncestors^.Count - 1 do
        begin
         Print_String(',');
         Print_Type(nAncestors^.Items^[i]);
        end;
       Print_String(')');
      end;
     Print_String(StructureName[nDefStructPattern^.nModeSymbol]);
     if (nDefStructPattern^.nArgs <> nil) and (nDefStructPattern^.nArgs^.Count > 0) then
     begin
       Print_String(TokenName[sy_Over]);
       Print_Loci(nDefStructPattern^.nArgs);
     end;
     Print_String(TokenName[sy_StructLeftBracket]);
     for i := 0 to nSgmFields^.Count - 1 do
      with FieldSegmentPtr(nSgmFields^.Items^[i])^ do
       begin
        Print_String(SelectorName[FieldSymbolPtr(nFields^.Items^[0])^.nFieldSymbol]);
        for j := 1 to nFields^.Count - 1 do
         with FieldSymbolPtr(nFields^.Items^[j])^ do
          begin
           Print_String(',');
           Print_String(SelectorName[nFieldSymbol]);
          end;
        Print_String(TokenName[sy_Arrow]);
        Print_Type(nSpecification);
        if (i >= 0) and (i < nSgmFields^.Count-1) then
         Print_String(',');
       end;
     Print_String(TokenName[sy_StructRightBracket]);
     Print_String(';');
     Print_NewLine;
   end;
  itPredSynonym,
  itFuncNotation, itModeNotation,
  itAttrSynonym:
   with NotationDeclarationPtr(nContent)^ do
   begin
    Print_String(TokenName[sy_Synonym]);
    Print_Pattern(nNewPattern);
    Print_String(TokenName[sy_For]);
    Print_Pattern(nOriginPattern);
    Print_String(';');
    Print_NewLine;
   end;
  itPredAntonym,itAttrAntonym:
   with NotationDeclarationPtr(nContent)^ do
   begin
    Print_String(TokenName[sy_Antonym]);
    Print_Pattern(nNewPattern);
    Print_String(TokenName[sy_For]);
    Print_Pattern(nOriginPattern);
    Print_String(';');
    Print_NewLine;
   end;
  itCluster:
   begin
    Print_String(TokenName[sy_Cluster]);
    case ClusterPtr(nContent)^.nClusterKind of
     ExistentialRegistration:
      with EClusterPtr(nContent)^ do
       begin
        Print_AdjectiveList(nConsequent);
        Print_String(TokenName[sy_For]);
        Print_Type(nClusterType);
       end;
     ConditionalRegistration:
      with CClusterPtr(nContent)^ do
       begin
        Print_AdjectiveList(nAntecedent);
        Print_String(TokenName[sy_Arrow]);
        Print_AdjectiveList(nConsequent);
        Print_String(TokenName[sy_For]);
        Print_Type(nClusterType);
       end;
     FunctorialRegistration:
      with FClusterPtr(nContent)^ do
       begin
        Print_Term(nClusterTerm);
        Print_String(TokenName[sy_Arrow]);
        Print_AdjectiveList(nConsequent);
        if nClusterType <> nil then
         begin
          Print_String(TokenName[sy_For]);
          Print_Type(nClusterType);
         end;
       end;
    end;
    Print_String(';');
    Print_NewLine;
   end;
  itIdentify:
   with IdentifyRegistrationPtr(nContent)^ do
   begin
    Print_String(TokenName[sy_Identify]);
    Print_Pattern(nNewPattern);
    Print_String(TokenName[sy_With]);
    Print_Pattern(nOriginPattern);
    if (nEqLociList <> nil) and (nEqLociList^.Count > 0) then
    begin
     Print_String(TokenName[sy_When]);
     for i := 0 to nEqLociList^.Count - 1 do
      with LociEqualityPtr(nEqLociList^.Items^[i])^ do
      begin
       Print_Locus(nLeftLocus);
       Print_String('=');
       Print_Locus(nRightLocus);
       if (i >= 0) and (i < nEqLociList^.Count-1) then
         Print_String(',');
      end;
    end;
    Print_String(';');
    Print_NewLine;
   end;
  itPropertyRegistration:
   case PropertyRegistrationPtr(nContent)^.nPropertySort of
   sySethood:
    with SethoodRegistrationPtr(nContent)^ do
    begin
     Print_String(PropertyName[nPropertySort]);
     Print_String(TokenName[sy_Of]);
     Print_Type(nSethoodType);
     Print_Justification(nJustification,nBlock);
     Print_String(';');
     Print_NewLine;
    end;
   end;
  itReduction:
   begin
    with ReduceRegistrationPtr(nContent)^ do
    begin
     Print_String(TokenName[sy_Reduce]);
     Print_Term(nOriginTerm);
     Print_String(TokenName[sy_To]);
     Print_Term(nNewTerm);
    end;
    Print_String(';');
    Print_NewLine;
  end;
  itPragma:
   begin
    Print_NewLine;
    Print_String('::'+PragmaPtr(nContent)^.nPragmaStr);
    Print_NewLine;
   end;
  itIncorrItem:;
  end;
 end;
end;

procedure Print_WSMizArticle(aWSTextProper:wsTextProperPtr; aFileName:string);
 var lWSMizOutput: WSMizarPrinterPtr;
begin
  InitScannerNames;
  lWSMizOutput:=new(WSMizarPrinterPtr,OpenFile(aFileName));
  lWSMizOutput^.Print_TextProper(aWSTextProper);
  dispose(lWSMizOutput,Done);
end;

end.
