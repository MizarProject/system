(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)
unit parseraddition;

interface

uses syntax, errhan, mobjects, mscanner, abstract_syntax, wsmarticle, xml_inout;

procedure InitWsMizarArticle;

type

 extBlockPtr = ^extBlockObj;
 extBlockObj =
  object(BlockObj)
    nLastWSItem: WSItemPtr;
    nLastWSBlock: WSBlockPtr;

    nLinked,nLinkAllowed,nLinkProhibited:boolean;
    nLinkPos:Position;

    nInDiffuse:boolean;
    nLastSentence: FormulaPtr;
 
    nHasAssumptions: Boolean;

   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
   procedure StartProperText; virtual;
   procedure ProcessRedefine; virtual;
   procedure ProcessLink; virtual;
   procedure ProcessBegin; virtual;
   procedure ProcessPragma; virtual;
   procedure StartSchemeDemonstration; virtual;
   procedure FinishSchemeDemonstration; virtual;
   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
  end;

 extItemPtr = ^extItemObj;
 extItemObj =
  object(ItemObj)

    nItemPos:Position;
    nLastWSItem: WSItemPtr;

    nLabelIdNr: integer;
    nLabelIdPos:Position;
    nLabel: LabelPtr;

    nPropPos: Position;

    nInference: JustificationPtr;
    nLinkable: boolean;

    nRegularStatementKind: RegularStatementKind;

    nItAllowed: boolean;

   constructor Init(fKind:ItemKind);
   procedure Pop; virtual;

   procedure StartSentence; virtual;

   procedure StartAttributes; virtual;
   procedure FinishAntecedent; virtual;
   procedure FinishConsequent; virtual;
   procedure FinishClusterTerm; virtual;

   procedure StartFuncIdentify; virtual;
   procedure ProcessFuncIdentify; virtual;
   procedure CompleteFuncIdentify; virtual;
   procedure ProcessLeftLocus; virtual;
   procedure ProcessRightLocus; virtual;

   procedure StartFuncReduction; virtual;
   procedure ProcessFuncReduction; virtual;

   procedure FinishPrivateConstant; virtual;
   procedure StartFixedVariables; virtual;
   procedure ProcessFixedVariable; virtual;
   procedure StartFixedSegment; virtual;
   procedure ProcessBeing; virtual;
   procedure FinishFixedSegment; virtual;
   procedure FinishFixedVariables; virtual;
   procedure StartAssumption; virtual;
   procedure FinishAssumption; virtual;
   procedure StartCollectiveAssumption; virtual;
   procedure ProcessMeans; virtual;
   procedure FinishOtherwise; virtual;
   procedure FinishDefiniens; virtual;
   procedure StartDefiniens; virtual;
   procedure StartGuard; virtual;
   procedure FinishGuard; virtual;
   procedure ProcessEquals; virtual;

   procedure StartExpansion; virtual;
   procedure FinishSpecification; virtual;
   procedure StartConstructionType; virtual;
   procedure FinishConstructionType; virtual;
   procedure StartAttributePattern; virtual;
   procedure FinishAttributePattern; virtual;
   procedure FinishSethoodProperties; virtual;
   procedure StartModePattern; virtual;
   procedure FinishModePattern; virtual;
   procedure StartPredicatePattern; virtual;
   procedure ProcessPredicateSymbol; virtual;
   procedure FinishPredicatePattern; virtual;
   procedure StartFunctorPattern; virtual;
   procedure ProcessFunctorSymbol; virtual;
   procedure FinishFunctorPattern; virtual;

   procedure ProcessAttrAntonym; virtual;
   procedure ProcessAttrSynonym; virtual;
   procedure ProcessPredAntonym; virtual;
   procedure ProcessPredSynonym; virtual;
   procedure ProcessFuncSynonym; virtual;
   procedure ProcessModeSynonym; virtual;

   procedure StartVisible; virtual;
   procedure ProcessVisible; virtual;
   procedure FinishPrefix; virtual;
   procedure ProcessStructureSymbol; virtual;
   procedure StartFields; virtual;
   procedure FinishFields; virtual;
   procedure StartAggrPattSegment; virtual;
   procedure ProcessField; virtual;
   procedure FinishAggrPattSegment; virtual;
   procedure ProcessSchemeName; virtual;
   procedure StartSchemeSegment; virtual;
   procedure StartSchemeQualification; virtual;
   procedure FinishSchemeQualification; virtual;
   procedure ProcessSchemeVariable; virtual;
   procedure FinishSchemeSegment; virtual;
   procedure FinishSchemeThesis; virtual;
   procedure FinishSchemePremise; virtual;

   procedure StartReservationSegment; virtual;
   procedure ProcessReservedIdentifier; virtual;
   procedure FinishReservationSegment; virtual;
   procedure StartPrivateDefiniendum; virtual;
   procedure FinishLocusType; virtual;

   procedure CreateExpression(fExpKind:ExpKind); virtual;

   procedure StartPrivateConstant; virtual;
   procedure StartPrivateDefiniens; virtual;
   procedure FinishPrivateFuncDefinienition; virtual;
   procedure FinishPrivatePredDefinienition; virtual;
   procedure ProcessReconsideredVariable; virtual;
   procedure FinishReconsideredTerm; virtual;
   procedure FinishDefaultTerm; virtual;
   procedure FinishCondition; virtual;
   procedure FinishHypothesis; virtual;
   procedure ProcessExemplifyingVariable; virtual;
   procedure FinishExemplifyingVariable; virtual;
   procedure StartExemplifyingTerm; virtual;
   procedure FinishExemplifyingTerm; virtual;
   procedure ProcessCorrectness; virtual;
   procedure ProcessLabel; virtual;
   procedure StartRegularStatement; virtual;
   procedure ProcessDefiniensLabel; virtual;
   procedure FinishCompactStatement; virtual;
   procedure StartIterativeStep; virtual;
   procedure FinishIterativeStep; virtual;

   { J u s t i f i c a t i o n }

   procedure ProcessSchemeReference; virtual;
   procedure ProcessPrivateReference; virtual;
   procedure StartLibraryReferences; virtual;
   procedure StartSchemeLibraryReference; virtual;
   procedure ProcessDef; virtual;
   procedure ProcessTheoremNumber; virtual;
   procedure ProcessSchemeNumber; virtual;
   procedure StartJustification; virtual;
   procedure StartSimpleJustification; virtual;
   procedure FinishSimpleJustification; virtual;

  end;

 extSubexpPtr = ^extSubexpObj;
 extSubexpObj =
  object(SubexpObj)
    nTermBase,nRightArgBase: integer;
    nSubexpPos,nNotPos,nRestrPos: Position;
    nQuaPos:Position;
    nSpelling: Integer;
    nSymbolNr,nRSymbolNr: integer;
    nConnective,nNextWord: TokenKind;
    nModeKind: TokenKind;
    nModeNr: integer;
    nRightSideOfPredPos: Position;
    nMultipredicateList:MList;

    nSample: TermPtr;
    nAllPos: Position;
    nPostQualList: MList; { teraz jest to kolekcja MultipleTypeExp }
    nQualifiedSegments: MList;
    nSegmentIdentColl: MList; // keeps spellings of vars
    nSegmentPos: Position;

    nFirstSententialOperand: FormulaPtr;
    nRestriction: FormulaPtr;

    nAttrCollection: MList;

    nNoneOcc: boolean;
    nNonPos:Position;
    nPostNegated: boolean;

    nArgListNbr: integer;
    nArgList: array of record Start,Length: integer; end;
    nFunc: array of record Instance,SymPri: integer; FuncPos: Position; end;

   constructor Init;

   procedure ProcessSimpleTerm; virtual;
   procedure StartFraenkelTerm; virtual;
   procedure StartPostQualification; virtual;
   procedure StartPostqualifyingSegment; virtual;
   procedure ProcessPostqualifiedVariable; virtual;
   procedure StartPostQualificationSpecyfication; virtual;
   procedure FinishPostqualifyingSegment; virtual;
   procedure FinishFraenkelTerm; virtual;
   procedure StartSimpleFraenkelTerm; virtual;
   procedure FinishSimpleFraenkelTerm; virtual;
   procedure ProcessThesis; virtual;
   procedure StartPrivateTerm; virtual;
   procedure FinishPrivateTerm; virtual;
   procedure StartBracketedTerm; virtual;
   procedure FinishBracketedTerm; virtual;
   procedure StartAggregateTerm; virtual;
   procedure FinishAggregateTerm; virtual;
   procedure StartSelectorTerm; virtual;
   procedure FinishSelectorTerm; virtual;
   procedure StartForgetfulTerm; virtual;
   procedure FinishForgetfulTerm; virtual;
   procedure StartChoiceTerm;  virtual;
   procedure FinishChoiceTerm;  virtual;
   procedure ProcessNumeralTerm; virtual;
   procedure ProcessItTerm; virtual;
   procedure ProcessLocusTerm; virtual;
   procedure ProcessQua; virtual;
   procedure FinishQualifiedTerm; virtual;
   procedure ProcessExactly; virtual;
   procedure StartLongTerm; virtual;
   procedure ProcessFunctorSymbol; virtual;
   procedure FinishArgList; virtual;
   procedure FinishLongTerm; virtual;
   procedure FinishArgument; virtual;
   procedure FinishTerm; virtual;
   procedure StartType; virtual;
   procedure ProcessModeSymbol; virtual;
   procedure FinishType; virtual;
   procedure CompleteType; virtual;
   procedure ProcessAtomicFormula; virtual;
   procedure ProcessPredicateSymbol; virtual;
   procedure ProcessRightSideOfPredicateSymbol; virtual;
   procedure FinishPredicativeFormula; virtual;
   procedure FinishRightSideOfPredicativeFormula; virtual;
   procedure StartMultiPredicativeFormula; virtual;
   procedure FinishMultiPredicativeFormula; virtual;
   procedure StartPrivateFormula; virtual;
   procedure FinishPrivateFormula; virtual;
   procedure ProcessContradiction; virtual;

   procedure ProcessNegative; virtual;

   procedure ProcessNegation; virtual;
   procedure FinishQualifyingFormula; virtual;
   procedure FinishAttributiveFormula; virtual;
   procedure ProcessBinaryConnective; virtual;
   procedure ProcessFlexDisjunction; virtual;
   procedure ProcessFlexConjunction; virtual;
   procedure StartRestriction; virtual;
   procedure FinishRestriction; virtual;
   procedure FinishBinaryFormula; virtual;
   procedure FinishFlexDisjunction; virtual;
   procedure FinishFlexConjunction; virtual;
   procedure StartExistential; virtual;
   procedure FinishExistential; virtual;
   procedure StartUniversal; virtual;
   procedure FinishUniversal; virtual;
   procedure StartQualifiedSegment; virtual;
   procedure StartQualifyingType; virtual;
   procedure FinishQualifiedSegment; virtual;
   procedure ProcessVariable; virtual;
   procedure StartAttributes; virtual;
   procedure ProcessNon; virtual;
   procedure ProcessAttribute; virtual;
   procedure StartAttributeArguments; virtual;
   procedure CompleteAttributeArguments; virtual;
   procedure FinishAttributeArguments; virtual;
   procedure CompleteAdjectiveCluster; virtual;
   procedure CompleteClusterTerm; virtual;

   procedure InsertIncorrType; virtual;
   procedure InsertIncorrTerm; virtual;
   procedure InsertIncorrBasic; virtual;
   procedure InsertIncorrFormula; virtual;
  end;

 extExpressionPtr = ^extExpressionObj;
 extExpressionObj =
  object(ExpressionObj)
   constructor Init(fExpKind:ExpKind);
   procedure CreateSubexpression; virtual;
  end;

function GetIdentifier:integer;
function CreateArgs(aBase:integer): PList;

var
  gWSTextProper: wsTextProperPtr;
  gLastWSBlock: WSBlockPtr;
  gLastWSItem: WSItemPtr;

  gLastType: TypePtr;
  gLastTerm: TermPtr;
  gLastFormula: FormulaPtr;
  gLastAdjective: AdjectiveExpressionPtr;

  gSubItemKind: TokenKind;

  gSchemeParams: PList;
  gTypeList: MList;
  gSchVarIds: MList;
  gTHEFileNr:integer;
  gDefinitional: boolean;

  gPrivateId: Integer;
  gPrivateIdPos: Position;

  gReconsiderList: PList;

  gParamNbr: integer;
  gLeftLociNbr: integer;
  gDefKind: ItemKind;
  gFieldsNbr:integer;
  gDefLabId:integer;
  gDefLabPos: Position;
  gDefLabel: LabelPtr;
  gQualifiedSegment: MList;
  gQualifiedSegmentList: PList;
  gSegmentPos: Position;
  gCorrectnessConditions : CorrectnessConditionsSet;
  gDefiningWay: HowToDefine;
  gRedefinitions: boolean;
  gConstructorNr: integer;

  gProofCnt: integer;
  dol_Allowed, it_Allowed,in_AggrPattern:boolean;

  gResIdents: PList;
  gResPos: Position;
  gSubItemPos: Position;


implementation

uses mizenv, mconsole, parser, _formats, pragmas
{$IFDEF MDEBUG} ,info {$ENDIF};

const
  MaxSubTermNbr   =   64;

var
  TermNbr: integer;
  Term: array of TermPtr;

  gLeftLoci,gParams: PList;
  gDefPos: Position;
  gLocus: LocusPtr;
  gPatternPos: Position;
  gPattern: PatternPtr;
  gNewPatternPos: Position;
  gNewPattern: PatternPtr;
  gLeftTermInReduction: TermPtr;

  gLeftLocus: LocusPtr;
  gIdenifyEqLociList: PList;

  gStructPrefixes: PList;
  gStrPos,gSgmPos: Position;
  gStructFields: PList;
  gStructFieldsSegment: PList;

  nDefiniensProhibited: boolean;
  gExpandable:boolean;
  gSpecification: TypePtr;

  gDefiniens: DefiniensPtr;
  gPartialDefs: PList;
  gPartDef: PObject;
  gMeansPos: Position;
  gOtherwise: PObject;

  gAttrColl: PList;
  gAntecedent,gConsequent:PList;
  gClusterTerm: TermPtr;
  gClusterSort: ClusterRegistrationKind;
  gPropertySort: PropertyKind;
  gCorrCondSort: CorrectnessKind;

  gLocalScheme: boolean;
  gSchemeIdNr: integer;
  gSchemePos,gSchemeIdPos: Position;
  gSchemeConclusion: FormulaPtr;
  gSchemePremises: PList;


  gSuchThatOcc: boolean;
  gSuchPos,gThatPos: Position;
  gPremises: PList;

  gIterPos: Position;
  gIterativeSteps: PList;
  gIterativeLastFormula: FormulaPtr;
  gInference: JustificationPtr;

function GetIdentifier:integer;
begin
 result:=0;
 if CurWord.Kind = Identifier then result:=CurWord.Nr
end;

procedure InitWsMizarArticle;
begin
 gWSTextProper:=new(wsTextProperPtr,Init(ArticleID,ArticleExt,CurPos));
 gLastWSBlock:=gWSTextProper;
 gLastWSItem:=nil;
 gBlockPtr:=new(extBlockPtr, Init(blMain));
end;

constructor extBlockObj.Init(fBlockKind:BlockKind);
begin
 inherited Init(fBlockKind);
 nLinked:=false;
 nLinkPos:=CurPos;
 nLinkAllowed:=false;
 nLinkProhibited:=true;
 nHasAssumptions:=false;
 gRedefinitions:=false;

 nLastWSItem:=gLastWSItem;
 nLastWSBlock:=gLastWSBlock;
 if nBlockKind = blMain then
   begin
    nInDiffuse:=true;
    gProofCnt:=0;
    FileExam(EnvFileName+'.frm');
    gFormatsColl.LoadFormats(EnvFileName+'.frm');
    gFormatsBase:=gFormatsColl.Count;
    setlength(Term,MaxSubTermNbr);
   end
  else
   begin
    gLastWSBlock:=gWsTextProper^.NewBlock(nBlockKind,CurPos);
    mizassert(2341,gLastWSItem<>nil);
    if gLastWSItem^.nItemKind in [itDefinition,itRegularStatement,itSchemeBlock,
                                  itTheorem,itConclusion,itCaseBlock,itCorrCond,
                                  itCorrectness,itProperty,itPropertyRegistration] then
      wsItemPtr(gLastWSItem).nBlock:=gLastWSBlock;
    case nBlockKind of
      blDefinition: nInDiffuse:=false;
      blNotation: nInDiffuse:=false;
      blDiffuse: nInDiffuse:=true;
      blHereby: nInDiffuse:=true;
      blProof:
       begin
        nLastSentence:=gLastFormula;
        inc(gProofCnt);
       end;
      blCase: nInDiffuse:=extBlockPtr(Previous)^.nInDiffuse;
      blSuppose: nInDiffuse:=extBlockPtr(Previous)^.nInDiffuse;
      blRegistration: nInDiffuse:=false;
      blPublicScheme: nInDiffuse:=false;
    end;
   end
end;

procedure extBlockObj.Pop;
begin
 gLastWSBlock^.nBlockEndPos:=CurPos;
 case nBlockKind of
  blDefinition: ;
  blProof:
   begin
    gLastFormula:=nLastSentence;
    dec(gProofCnt);
   end;
  blMain:
   begin
   end;
  blDiffuse:;
  blHereby:;
  blNotation:;
  blRegistration:;
  blCase:;
  blSuppose:;
  blPublicScheme:;
 end;
 gLastWSItem:=nLastWSItem;
 gLastWSBlock:=nLastWSBlock;
 inherited Pop;
end;

procedure extBlockObj.ProcessBegin;
begin
 nLinkAllowed:=false;
 nLinkProhibited:=true;
 gLastWSItem:=gWsTextProper^.NewItem(itSection,CurPos);
 nLastWSItem:=gLastWSItem;
 gLastWSBlock^.nItems.Insert(gLastWSItem);
end;

procedure extBlockObj.ProcessPragma;
begin
 nLinkAllowed:=false;
 nLinkProhibited:=true;
 gLastWSItem:=gWsTextProper^.NewItem(itPragma,CurPos);
 gLastWSItem^.nContent:=new(PragmaPtr,Init(CurWord.Spelling));
 nLastWSItem:=gLastWSItem;
 gLastWSBlock^.nItems.Insert(gLastWSItem);
end;

procedure extBlockObj.StartProperText;
begin
 gWSTextProper^.nBlockPos:=CurPos;
end;

procedure extBlockObj.ProcessRedefine;
begin
 gRedefinitions:=CurWord.Kind = sy_Redefine;
end;

procedure extBlockObj.ProcessLink;
begin
 if CurWord.Kind in [sy_Then,sy_Hence] then
  begin
   if nLinkProhibited then ErrImm(164);
   nLinked:=true;
   nLinkPos:=CurPos;
  end;
end;

procedure extBlockObj.StartSchemeDemonstration;
begin
 inc(gProofCnt);
 if not ProofPragma then
  begin
   gLastWSItem:=gWsTextProper^.NewItem(itConclusion,CurPos);
   gLastWSItem^.nContent:=
     new(CompactStatementPtr,
         Init(new(PropositionPtr,
                   Init(new(LabelPtr,Init(0,CurPos)),
                        new(ThesisFormulaPtr,Init(CurPos)),CurPos)),
                        new(JustificationPtr,Init(infSkippedProof,CurPos))));
   gLastWSBlock^.nItems.Insert(gLastWSItem);
  end;
end;

procedure extBlockObj.FinishSchemeDemonstration;
begin
 dec(gProofCnt);
end;

procedure extBlockObj.CreateItem(fItemKind:ItemKind);
begin
 gItemPtr:=new(extItemPtr, Init(fItemKind));
end;

procedure extBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 gBlockPtr:=new(extBlockPtr,Init(fBlockKind))
end;

constructor extItemObj.Init(fKind:ItemKind);
begin
 inherited Init(fKind);
 nItemPos:=CurPos;
 gClusterSort:=ExistentialRegistration;
 nItAllowed:=false;
 it_Allowed:=false;
 in_AggrPattern:=false;
 dol_Allowed:=false;
 gSpecification:=nil;
 gLastType:=nil;
 gLastFormula:=nil;
 gLastTerm:=nil;
 { Przygotowanie definiensow: }
 nDefiniensProhibited:=false;
  { Ew. zakaz przy obiektach ekspandowanych }
 gDefiningWay:=dfEmpty;
 gDefiniens:=nil;
 gPartialDefs:=nil;
 nLinkable:=false;
 mizassert(2343,gLastWSBlock<>nil);
 if not (nItemKind in [itReservation,itConstantDefinition,itExemplification,
                       itGeneralization,itLociDeclaration]) then
   begin
    gLastWSItem:=gWsTextProper^.NewItem(fKind,CurPos);
    nLastWSItem:=gLastWSItem;
   end;
 case nItemKind of
  itGeneralization: ;
  itExistentialAssumption: ExtBlockPtr(gBlockPtr)^.nHasAssumptions:=true;
  itProperty:
   begin
    gPropertySort:=PropertyKind(CurWord.Nr);
    case PropertyKind(CurWord.Nr) of
     sySymmetry,syReflexivity,syIrreflexivity,syTransitivity,syConnectedness,syAsymmetry:
      if gDefKind<>itDefPred then begin ErrImm(81); gPropertySort:=sErrProperty; end;
     syAssociativity,syCommutativity,syIdempotence:
      if gDefKind<>itDefFunc then begin ErrImm(82); gPropertySort:=sErrProperty; end;
     syInvolutiveness,syProjectivity:
      if gDefKind<>itDefFunc then begin ErrImm(83); gPropertySort:=sErrProperty; end;
     sySethood:
      if (gDefKind<>itDefMode) or gExpandable then begin ErrImm(86); gPropertySort:=sErrProperty; end;
    end;
   end;
  itReconsider:
    gReconsiderList:=new(PList,Init(0));
  itRegularStatement: nLinkable:=true;
  itConclusion:
    nLinkable:=true;
  itPerCases: ;
  itCaseHead:
   if AheadWord.Kind <> sy_That then nLinkable:=true;
  itSupposeHead:
   if AheadWord.Kind <> sy_That then nLinkable:=true;
  itTheorem:
    nLinkable:=true;
  itAxiom:
    if not AxiomsAllowed then ErrImm(66);
  itChoice: ;
  itAssumption:
   begin
    if AheadWord.Kind <> sy_That then nLinkable:=true;
    gPremises:=nil;
   end;
  itLociDeclaration: ;
  itDefMode:
   begin
    nItAllowed:=true;
    gCorrectnessConditions:=[];
    gExpandable:=false;
    gDefPos:=CurPos;
    gDefKind:=nItemKind
   end;
  itDefAttr:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind
   end;
  itAttrSynonym:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind
   end;
  itAttrAntonym:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind
   end;
  itModeNotation:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind
   end;
  itDefFunc:
   begin
    nItAllowed:=true;
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind
   end;
  itFuncNotation:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind;
   end;
  itDefPred,
  itPredSynonym,
  itCluster,
  itIdentify,
  itReduction:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind;
   end;
  itPropertyRegistration:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind;
    gPropertySort:=PropertyKind(CurWord.Nr);
   end;
  itDefStruct:
   begin
    gCorrectnessConditions:=[];
    gDefPos:=CurPos;
    gDefKind:=nItemKind;
    gStructPrefixes:=new(PList,Init(0));
   end;
  itCanceled:
   begin
    ErrImm(88);
   end;
  itCorrCond:
   if CorrectnessKind(CurWord.Nr) in gCorrectnessConditions then
    begin
     exclude(gCorrectnessConditions,CorrectnessKind(CurWord.Nr));
     gCorrCondSort:=CorrectnessKind(CurWord.Nr);
     if (gRedefinitions and (gCorrCondSort=syCoherence) and ExtBlockPtr(gBlockPtr)^.nHasAssumptions) then ErrImm(243);
    end
   else
    begin
     ErrImm(72);
     gCorrCondSort:=CorrectnessKind(0);
    end;
  itCorrectness: if (gRedefinitions and ExtBlockPtr(gBlockPtr)^.nHasAssumptions) then ErrImm(243);
  itDefinition, itSchemeHead, itReservation,
  itPrivFuncDefinition, itPrivPredDefinition,
  itConstantDefinition, itExemplification:;
  itCaseBlock:;
  itSchemeBlock:
   begin
    gLocalScheme:=CurWord.Kind <> sy_Scheme;
    gSchemePos:=CurPos;
   end;
 end;
 if not (nItemKind in [itReservation,itConstantDefinition,itExemplification,
                       itGeneralization,itLociDeclaration]) then
   gLastWSBlock^.nItems.Insert(gLastWSItem);
end;

procedure extItemObj.Pop;
 var k:integer;
begin
 gLastWSItem^.nItemEndPos:=PrevPos;
 case nItemKind of
  itDefPred, itDefFunc, itDefMode, itDefAttr:
   begin
    if gDefiningWay <> dfEmpty then
     begin
      if nDefiniensProhibited and not AxiomsAllowed then
       begin
        Error(gMeansPos,254);
        gDefiningWay:=dfEmpty;
       end;
     end
    else if not gRedefinitions and not nDefiniensProhibited and not AxiomsAllowed then
      SemErr(253);
   end;
 end;
 case nItemKind of
  itTheorem:
   nLastWSItem^.nContent:=
     new(CompactStatementPtr,
         Init(new(PropositionPtr,Init(nLabel,
                                      gLastFormula,nPropPos)),
                                      nInference));
  itPerCases:
   nLastWSItem^.nContent:=nInference;
  itPrivFuncDefinition:
   begin
   end;
  itPrivPredDefinition:
   begin
   end;
  itReconsider:
    nLastWSItem^.nContent:=
        new(TypeChangingStatementPtr,
            Init(gReconsiderList,gLastType,SimpleJustificationPtr(nInference)));
  itChoice:
   begin
    nLastWSItem^.nContent:=
         new(ChoiceStatementPtr,
             Init(gQualifiedSegmentList,gPremises,SimpleJustificationPtr(nInference)));
    gPremises:=nil;
   end;
  itConclusion,
  itRegularStatement:
   case nRegularStatementKind of
    stDiffuseStatement:
     nLastWSItem^.nContent:=
         new(DiffuseStatementPtr,Init(nLabel,
                                          stDiffuseStatement));
    stCompactStatement:
     nLastWSItem^.nContent:=
         new(CompactStatementPtr,
             Init(new(PropositionPtr,Init(nLabel,
                                          gLastFormula,nPropPos)),
                                          nInference));
    stIterativeEquality:
     nLastWSItem^.nContent:=
         new(IterativeEqualityPtr,
             Init(new(PropositionPtr,Init(nLabel,
                  gIterativeLastFormula,nPropPos)),
                  gInference,gIterativeSteps));
   end;
  itGeneralization,
  itLociDeclaration:
   begin
     for k := 0 to gQualifiedSegmentList^.Count-1 do
      begin
       gLastWSItem:=gWsTextProper^.NewItem(nItemKind,
           QualifiedSegmentPtr(gQualifiedSegmentList^.Items^[k])^.nSegmPos);
       nLastWSItem:=gLastWSItem;
       gLastWSItem^.nContent:=gQualifiedSegmentList^.Items^[k];
       if k = gQualifiedSegmentList^.Count-1 then
         gLastWSItem^.nItemEndPos:=PrevPos
       else
        gLastWSItem^.nItemEndPos:=QualifiedSegmentPtr(gQualifiedSegmentList^.Items^[k+1])^.nSegmPos;
       gQualifiedSegmentList^.Items^[k]:=nil;
       gLastWSBlock^.nItems.Insert(gLastWSItem);
      end;
     dispose(gQualifiedSegmentList,Done);
     if gPremises <> nil then
      begin
       gLastWSItem:=gWsTextProper^.NewItem(itAssumption,gSuchPos);
       gLastWSItem^.nContent:=new(CollectiveAssumptionPtr,Init(gThatPos,gPremises));
       gPremises:=nil;
       gLastWSItem^.nItemEndPos:=PrevPos;
       nLastWSItem:=gLastWSItem;
       gLastWSBlock^.nItems.Insert(gLastWSItem);
      end;
   end;
  itExistentialAssumption:
   begin
    nLastWSItem^.nContent:=
         new(ExistentialAssumptionPtr,
             Init(nItemPos,gQualifiedSegmentList,gPremises));
    gPremises:=nil;
   end;
  itSupposeHead,itCaseHead,
  itAssumption:
    if gPremises <> nil then
     begin
      gLastWSItem^.nContent:=new(CollectiveAssumptionPtr,Init(gThatPos,gPremises));
      gPremises:=nil;
     end
    else
      gLastWSItem^.nContent:=
        new(SingleAssumptionPtr,Init(nItemPos,
            new(PropositionPtr,Init(nLabel,
                                    gLastFormula,nPropPos))));
  itDefMode:
   begin
     if gExpandable  then
      nLastWSItem^.nContent:=
        new(ExpandableModeDefinitionPtr,
            Init(gPatternPos,ModePatternPtr(gPattern),gLastType))
     else
      begin
       nLastWSItem^.nContent:=
         new(StandardModeDefinitionPtr,
            Init(gPatternPos,gRedefinitions,ModePatternPtr(gPattern),
                 gSpecification,gDefiniens));
       if not gRedefinitions then
        include(gCorrectnessConditions,syExistence);
      end;
   end;
  itDefFunc:
   begin
     nLastWSItem^.nContent:=
         new(FunctorDefinitionPtr,
            Init(gPatternPos,gRedefinitions,FunctorPatternPtr(gPattern),
                 gSpecification,gDefiningWay,gDefiniens));

   end;
  itDefAttr:
   begin
     nLastWSItem^.nContent:=
         new(AttributeDefinitionPtr,
            Init(gPatternPos,gRedefinitions,AttributePatternPtr(gPattern),gDefiniens));
   end;
  itDefPred:
   begin
     nLastWSItem^.nContent:=
         new(PredicateDefinitionPtr,
            Init(gPatternPos,gRedefinitions,PredicatePatternPtr(gPattern),gDefiniens));
   end;
  itDefStruct:
   begin
    nLastWSItem^.nContent:=
        new(StructureDefinitionPtr,
            Init(gPatternPos,gStructPrefixes,gConstructorNr,gParams,gStructFields));
   end;
  itPredSynonym, itPredAntonym, itFuncNotation, itModeNotation,
  itAttrSynonym, itAttrAntonym:
    nLastWSItem^.nContent:=
        new(NotationDeclarationPtr,
            Init(gNewPatternPos,nItemKind,gNewPattern,gPattern));
  itCluster:
   begin
    case gClusterSort of
      ExistentialRegistration:
       begin
        nLastWSItem^.nContent:=new(EClusterPtr,Init(nItemPos,gConsequent,gLastType));
        include(gCorrectnessConditions,syExistence)
       end;
      ConditionalRegistration:
       begin
        nLastWSItem^.nContent:=
          new(CClusterPtr,Init(nItemPos,gAntecedent,gConsequent,gLastType));
        include(gCorrectnessConditions,syCoherence);
       end;
      FunctorialRegistration:
       begin
        nLastWSItem^.nContent:=
          new(FClusterPtr,Init(nItemPos,gClusterTerm,gConsequent,gLastType));
        include(gCorrectnessConditions,syCoherence);
       end;
    end;
   end;
  itIdentify:
   begin
    nLastWSItem^.nContent:=
     new(IdentifyRegistrationPtr,Init(nItemPos,gNewPattern,gPattern,gIdenifyEqLociList));
    include(gCorrectnessConditions,syCompatibility);
   end;
  itReduction:
   begin
    nLastWSItem^.nContent:=
     new(ReduceRegistrationPtr,Init(nItemPos,gLeftTermInReduction,gLastTerm));
    include(gCorrectnessConditions,syReducibility);
   end;
  itPropertyRegistration:
   SethoodRegistrationPtr(nLastWSItem^.nContent)^.nJustification:=nInference;
  itCorrCond:
   nLastWSItem^.nContent:=new(CorrectnessConditionPtr,
                                Init(nItemPos,gCorrCondSort,nInference));
  itCorrectness:
   nLastWSItem^.nContent:=new(CorrectnessConditionsPtr,
                                Init(nItemPos,gCorrectnessConditions,nInference));
  itProperty:
   nLastWSItem^.nContent:=new(PropertyPtr,Init(nItemPos,gPropertySort,nInference));
  itSchemeHead:
   nLastWSItem^.nContent:=
    new(SchemePtr,Init(gSchemeIdNr,gSchemeIdPos,gSchemeParams,gSchemePremises,gSchemeConclusion));
  itPragma: ;
  itDefinition:;
  itSchemeBlock:;
  itReservation:;
  itExemplification:;
  itCaseBlock:;
 end;
 with extBlockPtr(gBlockPtr)^ do
  begin
   if nLinked then
    begin
     Error(nLinkPos,178);
     nLinked:=false
    end;
   nLinkAllowed:=nLinkable;
   nLinkProhibited:=not nLinkable;
   if not StillCorrect then
    begin
     nLinkAllowed:=false;
     nLinkProhibited:=false
    end;
  end;
 if gDefiningWay <> dfEmpty then
  begin
   if gDefiniens^.nDefSort = ConditionalDefiniens then
    include(gCorrectnessConditions,syConsistency);
   if gRedefinitions then
    include(gCorrectnessConditions,syCompatibility);
  end;
 inherited Pop;
end;

procedure extItemObj.ProcessModeSynonym;
begin
 gNewPatternPos:=gPatternPos;
 gNewPattern:=gPattern;
end;

procedure extItemObj.ProcessAttrSynonym;
begin
 gNewPatternPos:=gPatternPos;
 gNewPattern:=gPattern;
end;

procedure extItemObj.ProcessAttrAntonym;
begin
 gNewPatternPos:=gPatternPos;
 gNewPattern:=gPattern;
end;

procedure extItemObj.ProcessPredSynonym;
begin
 gNewPatternPos:=gPatternPos;
 gNewPattern:=gPattern;
end;

procedure extItemObj.ProcessPredAntonym;
begin
 gNewPatternPos:=gPatternPos;
 gNewPattern:=gPattern;
end;

procedure extItemObj.ProcessFuncSynonym;
begin
 gNewPatternPos:=gPatternPos;
 gNewPattern:=gPattern;
end;

procedure extItemObj.StartAttributes;
begin
 gAttrColl:=new(PList,Init(6));
end;

procedure extItemObj.StartSentence;
begin
 nPropPos:=CurPos;
end;

procedure extItemObj.FinishAntecedent;
begin
 gClusterSort:=ConditionalRegistration;
 gAntecedent:=gAttrColl;
end;

procedure extItemObj.FinishConsequent;
begin
 gConsequent:=gAttrColl;
end;

procedure extItemObj.FinishClusterTerm;
begin
 gClusterSort:=FunctorialRegistration;
 gClusterTerm:=gLastTerm;
end;

procedure extItemObj.StartFuncIdentify;
begin
end;

procedure extItemObj.ProcessFuncIdentify;
begin
 gNewPatternPos:=gPatternPos;
 gNewPattern:=gPattern;
end;

procedure extItemObj.CompleteFuncIdentify;
begin
 gIdenifyEqLociList:=nil;
 if CurWord.Kind = sy_When then
  gIdenifyEqLociList:=new(PList,Init(0));
end;

procedure extItemObj.ProcessLeftLocus;
begin
 gLeftLocus:=new(LocusPtr,Init(CurPos,GetIdentifier));
end;

procedure extItemObj.ProcessRightLocus;
begin
 gIdenifyEqLociList.Insert(new(LociEqualityPtr,
                       Init(PrevPos,gLeftLocus,new(LocusPtr,Init(CurPos,GetIdentifier)))));
end;

procedure extItemObj.StartFuncReduction;
begin
end;

procedure extItemObj.ProcessFuncReduction;
begin
 gNewPatternPos:=gPatternPos;
 gLeftTermInReduction:=gLastTerm;
end;

procedure extItemObj.StartFixedVariables;
begin
 gQualifiedSegmentList:=new(PList,Init(0));
end;

procedure extItemObj.StartFixedSegment;
begin
 gQualifiedSegment.Init(0);
 gSegmentPos:=CurPos;
end;

procedure extItemObj.ProcessFixedVariable;
begin
 gQualifiedSegment.Insert(new(VariablePtr,Init(CurPos,GetIdentifier)));
end;

procedure extItemObj.ProcessBeing;
begin
 gLastType:=nil;
end;

procedure extItemObj.FinishFixedSegment;
 var k:integer;
begin
 if gLastType <> nil then
   begin
     gQualifiedSegmentList^.Insert(new(ExplicitlyQualifiedSegmentPtr,
                           Init(gSegmentPos,new(PList,MoveList(gQualifiedSegment)),gLastType)));
     gQualifiedSegment.DeleteAll;
   end
  else
   begin
    for k := 0 to gQualifiedSegment.Count - 1 do
     begin
      gQualifiedSegmentList^.Insert(new(ImplicitlyQualifiedSegmentPtr,
                            Init(VariablePtr(gQualifiedSegment.Items^[k])^.nVarPos,
                                 gQualifiedSegment.Items^[k])));
      gQualifiedSegment.Items^[k]:=nil;
     end;
   end;
  gQualifiedSegment.Done;
end;

procedure extItemObj.FinishFixedVariables;
begin
 gSuchThatOcc:=CurWord.Kind = sy_Such;
 gSuchPos:=CurPos;
 gPremises:=nil;
end;

procedure extItemObj.StartAssumption;
begin
 gPremises:=new(PList,Init(0));
 gThatPos:=CurPos;
end;

procedure extItemObj.FinishAssumption;
begin
 ExtBlockPtr(gBlockPtr)^.nHasAssumptions:=true;
end;

procedure extItemObj.StartCollectiveAssumption;
begin
 gPremises:=new(PList,Init(0));
 gThatPos:=CurPos;
end;

procedure extItemObj.ProcessMeans;
begin
 gDefLabId:= 0;
 gDefLabPos:=CurPos;
 gDefiningWay:=dfMeans;
 gOtherwise:=nil;
 gMeansPos:=CurPos
end;

procedure extItemObj.ProcessEquals;
begin
 gDefLabId:= 0;
 gDefLabPos:=CurPos;
 gDefiningWay:=dfEquals;
 gOtherwise:=nil;
 gMeansPos:=CurPos;
end;

procedure extItemObj.FinishOtherwise;
begin
 if gDefiningWay = dfEquals then
  gOtherwise:=gLastTerm
 else gOtherwise:=gLastFormula;
end;

procedure extItemObj.StartDefiniens;
begin
 it_Allowed:=nItAllowed;
end;

procedure extItemObj.StartGuard;
begin
 if gPartialDefs = nil then
  gPartialDefs:=new(PList,Init(0));
 it_Allowed:=false;
 if gDefiningWay = dfMeans  then
   gPartDef:=gLastFormula
 else gPartDef:=gLastTerm;
end;

procedure extItemObj.FinishGuard;
begin
 it_Allowed:=nItAllowed;
 case gDefiningWay of
 dfMeans:
   gPartialDefs.Insert(new(PartDefPtr,Init(new(DefExpressionPtr,Init(exFormula,gPartDef)),gLastFormula)));
 dfEquals:
   gPartialDefs.Insert(new(PartDefPtr,Init(new(DefExpressionPtr,Init(exTerm,gPartDef)),gLastFormula)));
 end;
end;

procedure extItemObj.FinishSpecification;
begin
 gSpecification:=gLastType;
end;

procedure extItemObj.FinishConstructionType;
begin
 gSpecification:=gLastType;
end;

procedure extItemObj.StartExpansion;
begin
 if gRedefinitions then  ErrImm(271);
 nDefiniensProhibited:=true;
 gExpandable:=true;
end;

procedure extItemObj.StartAttributePattern;
begin
 gParamNbr:=0;
 gParams:=nil;
 gLocus:=new(LocusPtr,Init(CurPos,GetIdentifier));
end;

procedure extItemObj.FinishAttributePattern;
 var lFormatNr: integer;
begin
 lFormatNr:=0;
 if (CurWord.Kind = AttributeSymbol) and stillcorrect then
  lFormatNr:=gFormatsColl.CollectPrefixForm('V',CurWord.Nr,gParamNbr);
 gPatternPos:=CurPos;
 gConstructorNr:=CurWord.Nr;
 gPattern:=new(AttributePatternPtr,Init(gPatternPos,gLocus,gConstructorNr,gParams));
end;

procedure extItemObj.FinishSethoodProperties;
begin
 nLastWSItem^.nContent:=
   new(SethoodRegistrationPtr,Init(nItemPos,gPropertySort,gLastType));
end;

procedure extItemObj.StartModePattern;
begin
 gParamNbr:=0;
 gParams:=nil;
 gPatternPos:=CurPos;
 gConstructorNr:=CurWord.Nr;
end;

procedure extItemObj.FinishModePattern;
 var lFormatNr: integer;
begin
 lFormatNr:=0;
 if StillCorrect then
  lFormatNr:=gFormatsColl.CollectPrefixForm('M',gConstructorNr,gParamNbr);
 gPattern:=new(ModePatternPtr,Init(gPatternPos,gConstructorNr,gParams));
end;

procedure extItemObj.StartPredicatePattern;
begin
 gParamNbr:=0;
 gParams:=nil;
end;

procedure extItemObj.ProcessPredicateSymbol;
begin
 gPatternPos:=CurPos;
 gLeftLociNbr:=gParamNbr;
 gLeftLoci:=gParams;
 gParamNbr:=0;
 gParams:=nil;
 gConstructorNr:=CurWord.Nr;
end;

procedure extItemObj.FinishPredicatePattern;
 var lFormatNr: integer;
begin
 lFormatNr:=0;
 if StillCorrect then
   lFormatNr:=gFormatsColl.CollectPredForm(gConstructorNr,gLeftLociNbr,gParamNbr);
 gPattern:=new(PredicatePatternPtr,Init(gPatternPos,gLeftLoci,gConstructorNr,gParams));
end;

procedure extItemObj.StartFunctorPattern;
begin
 gPatternPos:=CurPos;
 gSubItemKind:=CurWord.Kind;
 case CurWord.Kind of
  LeftCircumfixSymbol: gConstructorNr:=CurWord.Nr;
  sy_LeftSquareBracket:
   begin
    gSubItemKind:=LeftCircumfixSymbol;
    gConstructorNr:=SquareBracket
   end;
  sy_LeftCurlyBracket:
   begin
    gSubItemKind:=LeftCircumfixSymbol;
    gConstructorNr:=CurlyBracket
   end;
  else gConstructorNr:=0;
 end;
 gParamNbr:=0;
 gParams:=nil;
end;

procedure extItemObj.ProcessFunctorSymbol;
begin
 gPatternPos:=CurPos;
 if CurWord.Kind = InfixOperatorSymbol then
  begin
   gSubItemKind:=InfixOperatorSymbol;
   gConstructorNr:=CurWord.Nr;
   gLeftLociNbr:=gParamNbr;
   gLeftLoci:=gParams;
   gParamNbr:=0;
   gParams:=nil;
  end;
end;

procedure extItemObj.FinishFunctorPattern;
 var lConstructorNr,lFormatNr: integer;
begin
 lFormatNr:=0;
 case gSubItemKind of
  LeftCircumfixSymbol:
   begin
    lConstructorNr:=CurWord.Nr;
    if StillCorrect then
     lFormatNr:=gFormatsColl.CollectBracketForm(gConstructorNr,lConstructorNr,gParamNbr,0,0);
    gPattern:=new(CircumfixFunctorPatternPtr,Init(gPatternPos,gConstructorNr,lConstructorNr,gParams));
   end;
  InfixOperatorSymbol:
   begin
    if StillCorrect then
     lFormatNr:=gFormatsColl.CollectFuncForm(gConstructorNr,gLeftLociNbr,gParamNbr);
    gPattern:=new(InfixFunctorPatternPtr,Init(gPatternPos,gLeftLoci,gConstructorNr,gParams));
   end;
  else
    gPattern:=new(InfixFunctorPatternPtr,Init(gPatternPos,gLeftLoci,gConstructorNr,gParams));
 end;
end;

procedure extItemObj.StartVisible;
begin
 gParams:=new(PList,Init(0));
end;

procedure extItemObj.ProcessVisible;
begin
 inc(gParamNbr);
 if gParams<>nil then
   gParams^.Insert(new(LocusPtr,Init(CurPos,GetIdentifier)));
end;

procedure extItemObj.FinishPrefix;
begin
 gStructPrefixes.Insert(gLastType);
end;

procedure extItemObj.ProcessStructureSymbol;
 var lFormatNr: integer;
begin
 gConstructorNr:=0;
 gPatternPos:=CurPos;
 if CurWord.Kind = StructureSymbol then gConstructorNr:=CurWord.Nr;
 lFormatNr:=gFormatsColl.CollectPrefixForm('J',gConstructorNr,1);
 gParamNbr:=0;
 gParams:=nil;
end;

procedure extItemObj.StartFields;
 var lFormatNr: integer;
begin
 lFormatNr:=gFormatsColl.CollectPrefixForm('L',gConstructorNr,gParamNbr);
 in_AggrPattern:=true;
 gStructFields:=new(PList,Init(0));
 gFieldsNbr:=0;
end;

procedure extItemObj.FinishFields;
 var lFormatNr: integer;
begin
 lFormatNr:=gFormatsColl.CollectPrefixForm('G',gConstructorNr,gFieldsNbr);
end;

procedure extItemObj.StartAggrPattSegment;
begin
 gStructFieldsSegment:=new(Plist,Init(0));
 gSgmPos:=CurPos;
end;

procedure extItemObj.ProcessField;
 var lFormatNr: integer;
begin
 lFormatNr:=gFormatsColl.CollectPrefixForm('U',CurWord.Nr,1);
 gStructFieldsSegment^.Insert(new(FieldSymbolPtr,Init(CurPos,CurWord.Nr)));
 Inc(gFieldsNbr);
end;

procedure extItemObj.FinishAggrPattSegment;
begin
 gStructFields.Insert(new(FieldSegmentPtr,Init(gSgmPos,gStructFieldsSegment,gLastType)));
end;

procedure extItemObj.ProcessSchemeName;
begin
 gSchemeIdNr:=GetIdentifier;
 gSchemeIdPos:=CurPos;
 gSchemeParams:=new(PList,Init(0));
end;

procedure extItemObj.StartSchemeQualification;
begin
 gSubItemKind:=CurWord.Kind;
 gTypeList.Init(4);
end;

procedure extItemObj.FinishSchemeQualification;
begin
 gSubItemPos:=CurPos
end;

procedure extItemObj.StartSchemeSegment;
begin
 gSubItemPos:=CurPos;
 gSchVarIds.Init(2);
end;

procedure extItemObj.ProcessSchemeVariable;
begin
 gSchVarIds.Insert(new(VariablePtr,Init(CurPos,GetIdentifier)));
end;

procedure extItemObj.FinishSchemeSegment;
begin
 case gSubItemKind of
  sy_LeftParanthesis:
   begin
    gSchemeParams.Insert(new(FunctorSegmentPtr,Init(gSubItemPos,
                             new(PList,MoveList(gSchVarIds)),
                             new(PList,MoveList(gTypeList)),gLastType)));
   end;
  sy_LeftSquareBracket:
   begin
    gSchemeParams.Insert(new(SchemeSegmentPtr,Init(gSubItemPos,PredicateSegment,
                             new(PList,MoveList(gSchVarIds)),
                             new(PList,MoveList(gTypeList)))));
   end;
 end;
end;

procedure extItemObj.FinishSchemeThesis;
begin
 gSchemeConclusion:=gLastFormula;
// gSchemePremises:=nil;
// if CurWord.Kind = sy_Provided then
 gSchemePremises:=new(Plist,Init(0));
end;

procedure extItemObj.FinishSchemePremise;
begin
 gSchemePremises^.Insert(new(PropositionPtr,
                  Init(nLabel,
                       gLastFormula,nPropPos)));
end;

procedure extItemObj.StartReservationSegment;
begin
 gResIdents:=new(Plist,Init(0));
 gResPos:=CurPos;
end;

procedure extItemObj.ProcessReservedIdentifier;
begin
 gResIdents^.Insert(new(VariablePtr,Init(CurPos,GetIdentifier)));
end;

procedure extItemObj.FinishReservationSegment;
begin
 gLastWSItem:=gWsTextProper^.NewItem(itReservation,gResPos);
 nLastWSItem:=gLastWSItem;
 gLastWSItem^.nContent:=new(ReservationSegmentPtr,Init(gResIdents,gLastType));
 gLastWSItem^.nItemEndPos:=PrevPos;
 gLastWSBlock^.nItems.Insert(gLastWSItem);
end;

procedure extItemObj.StartPrivateDefiniendum;
begin
 gPrivateId:=GetIdentifier;
 gPrivateIdPos:=CurPos;
 dol_Allowed:=true;
 gTypeList.Init(4);
end;

procedure extItemObj.FinishLocusType;
begin
 gTypeList.Insert(gLastType);
end;

procedure extItemObj.CreateExpression(fExpKind:ExpKind);
begin
 gExpPtr:=new(extExpressionPtr,Init(fExpKind));
end;

procedure extItemObj.FinishPrivateConstant;
begin
 gLastWSItem:=gWsTextProper^.NewItem(itConstantDefinition,nItemPos);
 nLastWSItem:=gLastWSItem;
 gLastWSItem^.nContent:=
    new(ConstantDefinitionPtr,
        Init(new(VariablePtr,Init(gPrivateIdPos,gPrivateId)),gLastTerm));
 gLastWSItem^.nItemEndPos:=PrevPos;
 gLastWSBlock^.nItems.Insert(gLastWSItem);
 nItemPos:=CurPos;
end;

procedure extItemObj.StartPrivateConstant;
begin
 gPrivateId:=GetIdentifier;
 gPrivateIdPos:=CurPos;
end;

procedure extItemObj.StartPrivateDefiniens;
begin
 dol_Allowed:=true;
end;

procedure extItemObj.FinishPrivateFuncDefinienition;
begin
  nLastWSItem^.nContent:=
      new(PrivateFunctorDefinitionPtr,
          Init(new(VariablePtr,Init(gPrivateIdPos,gPrivateId)),
               new(PList,MoveList(gTypeList)),gLastTerm));
end;

procedure extItemObj.FinishPrivatePredDefinienition;
begin
  nLastWSItem^.nContent:=
      new(PrivatePredicateDefinitionPtr,
          Init(new(VariablePtr,Init(gPrivateIdPos,gPrivateId)),
               new(PList,MoveList(gTypeList)),gLastFormula));
end;

procedure extItemObj.ProcessReconsideredVariable;
begin
 gPrivateId:=GetIdentifier;
 gPrivateIdPos:=CurPos;
end;

procedure extItemObj.FinishReconsideredTerm;
begin
 gReconsiderList^.Insert(new(TypeChangePtr,
       Init(Equating,new(VariablePtr,Init(gPrivateIdPos,gPrivateId)),gLastTerm)));
end;

procedure extItemObj.FinishDefaultTerm;
begin
 gReconsiderList^.Insert(new(TypeChangePtr,Init(VariableIdentifier,
                              new(VariablePtr,Init(gPrivateIdPos,gPrivateId)),nil)));
end;

procedure extItemObj.FinishCondition;
begin
 if gPremises = nil then
  gPremises:=new(PList,Init(0));
 gPremises^.Insert(new(PropositionPtr,
                  Init(nLabel,
                       gLastFormula,nPropPos)));
end;

procedure extItemObj.FinishHypothesis;
begin
 if gPremises <> nil then
   gPremises^.Insert(new(PropositionPtr,
                  Init(nLabel,
                       gLastFormula,nPropPos)));
end;

procedure extItemObj.ProcessExemplifyingVariable;
begin
 gPrivateId:=GetIdentifier;
 gPrivateIdPos:=CurPos;
end;

procedure extItemObj.FinishExemplifyingVariable;
begin
 gLastWSItem:=gWsTextProper^.NewItem(itExemplification,nItemPos);
 nLastWSItem:=gLastWSItem;
 gLastWSItem^.nContent:=new(ExamplePtr,Init(new(VariablePtr,
                                     Init(gPrivateIdPos,gPrivateId)),gLastTerm));
 gLastWSItem^.nItemEndPos:=PrevPos;
 gLastWSBlock^.nItems.Insert(gLastWSItem);
 nItemPos:=CurPos;
end;

procedure extItemObj.StartExemplifyingTerm;
begin
 if (CurWord.Kind=Identifier) and extBlockPtr(gBlockPtr)^.nInDiffuse and
    ((AheadWord.Kind=sy_Comma) or (AheadWord.Kind=sy_Semicolon)) then
  begin
   gPrivateId:=GetIdentifier;
   gPrivateIdPos:=CurPos;
  end
 else gPrivateId:=0;
end;

procedure extItemObj.FinishExemplifyingTerm;
begin
 gLastWSItem:=gWsTextProper^.NewItem(itExemplification,nItemPos);
 nLastWSItem:=gLastWSItem;
 if gPrivateId <> 0 then
   gLastWSItem^.nContent:=
       new(ExamplePtr,Init(new(VariablePtr,Init(gPrivateIdPos,gPrivateId)),nil))
// else if gLastTerm^.nTermSort = wsSimpleTerm then
//   with SimpleTermPtr(gLastTerm)^ do
//   gLastWSItem^.nContent:=
//       new(ExamplePtr,Init(new(VariablePtr,Init(nTermPos,nIdent)),nil))
 else
   gLastWSItem^.nContent:=new(ExamplePtr,Init(nil,gLastTerm));
 gLastWSItem^.nItemEndPos:=PrevPos;
 gLastWSBlock^.nItems.Insert(gLastWSItem);
 nItemPos:=CurPos;
end;

procedure extItemObj.ProcessCorrectness;
begin
 if CurWord.Kind <> sy_Correctness then
  if (gCorrectnessConditions <> []) and not AxiomsAllowed then
   Error(gDefPos,73);
end;

procedure extItemObj.StartConstructionType;
begin
 if gRedefinitions and (CurWord.Kind = sy_Arrow) then
  include(gCorrectnessConditions,syCoherence);
end;

procedure extItemObj.ProcessLabel;
begin
 nLabelIdNr:=0;
 nLabelIdPos:=CurPos;
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
    nLabelIdNr:=CurWord.Nr;
 nLabel:=new(LabelPtr,Init(nLabelIdNr,nLabelIdPos));
end;

procedure extItemObj.StartRegularStatement;
begin
 if CurWord.Kind=sy_Now then
  nRegularStatementKind:=stDiffuseStatement
 else nRegularStatementKind:=stCompactStatement;
end;

procedure extItemObj.ProcessDefiniensLabel;
begin
 gDefLabId:= 0;
 gDefLabPos:=CurPos;
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
   gDefLabId:= CurWord.Nr;
 gDefLabel:= new(LabelPtr,Init(gDefLabId,gDefLabPos));
end;

procedure extItemObj.ProcessSchemeReference;
begin
 if CurWord.Kind = Identifier then
 begin
  SchemeJustificationPtr(nInference)^.nSchemeIdNr:=CurWord.Nr;
  SchemeJustificationPtr(nInference)^.nSchemeInfPos:=CurPos;
 end;
end;

procedure extItemObj.StartLibraryReferences;
begin
 gTHEFileNr:=CurWord.Nr;
end;

procedure extItemObj.StartSchemeLibraryReference;
begin
 gTHEFileNr:=CurWord.Nr;
end;

procedure extItemObj.ProcessPrivateReference;
begin
 SimpleJustificationPtr(nInference)^.nReferences^.Insert(new(LocalReferencePtr,
                                              Init(GetIdentifier,CurPos)));
end;

procedure extItemObj.ProcessDef;
begin
 gDefinitional:=(CurWord.Kind = ReferenceSort) and (CurWord.Nr = ord(syDef))
end;

procedure extItemObj.ProcessTheoremNumber;
 var lRefPtr: ReferencePtr;
begin
 if CurWord.Kind <> Numeral then exit;
 if CurWord.Nr = 0 then
  begin
   ErrImm(146);
   exit
  end;
 if gDefinitional then
   lRefPtr:=new(DefinitionReferencePtr, Init(gTHEFileNr,CurWord.Nr,CurPos))
 else
   lRefPtr:=new(TheoremReferencePtr, Init(gTHEFileNr,CurWord.Nr,CurPos));
 SimpleJustificationPtr(nInference)^.nReferences^.Insert(lRefPtr);
end;

procedure extItemObj.ProcessSchemeNumber;
begin
 if CurWord.Kind <> Numeral then exit;
 if CurWord.Nr = 0 then
  begin
   ErrImm(146);
   exit
  end;
 with SchemeJustificationPtr(nInference)^ do
 begin
  nSchFileNr:=gTHEFileNr;
  nSchemeIdNr:=CurWord.Nr;
  nSchemeInfPos:=PrevPos;
 end;
end;

procedure extItemObj.StartJustification;
begin
 nInference:=nil;
 if CurWord.Kind = sy_Proof then
  begin
   if ProofPragma then
    nInference:=new(JustificationPtr,Init(infProof,CurPos))
   else
    nInference:=new(JustificationPtr,Init(infSkippedProof,CurPos))
  end;
end;

procedure extItemObj.StartSimpleJustification;
begin
 case CurWord.Kind of
  sy_From:
   nInference:=new(SchemeJustificationPtr,Init(CurPos,0,0));
  sy_By:
   with extBlockPtr(gBlockPtr)^ do
    nInference:=new(StraightforwardJustificationPtr,Init(CurPos,nLinked,nLinkPos));
  else
   with extBlockPtr(gBlockPtr)^ do
    nInference:=new(StraightforwardJustificationPtr,Init(PrevPos,nLinked,nLinkPos));
 end;
end;

procedure extItemObj.FinishSimpleJustification;
begin
 with extBlockPtr(gBlockPtr)^ do
  begin
   if not StillCorrect
      or (CurWord.Kind <> sy_Semicolon) and (CurWord.Kind <> sy_DotEquals)
      or (nInference^.nInfSort = infStraightforwardJustification) and (byte(nLinked) > byte(nLinkAllowed))
      or (nInference^.nInfSort = infSchemeJustification) and (SchemeJustificationPtr(nInference)^.nSchemeIdNr = 0)
    then nInference^.nInfSort:=infError;
  end;
 if (nInference^.nInfSort = infStraightforwardJustification) or (nInference^.nInfSort = infError)
   then extBlockPtr(gBlockPtr)^.nLinked:=false;
end;

procedure extItemObj.FinishCompactStatement;
begin
 if CurWord.Kind = sy_DotEquals then
  begin
   gIterativeLastFormula:=gLastFormula;
   nRegularStatementKind:=stIterativeEquality;
   extBlockPtr(gBlockPtr)^.nLinked:=false;
   gIterativeSteps:=new(PList,Init(0));
   gInference:=nInference;
  end;
end;

procedure extItemObj.StartIterativeStep;
begin
 gIterPos:=CurPos;
end;

procedure extItemObj.FinishIterativeStep;
begin
 gIterativeSteps^.Insert(new(IterativeStepPtr,Init(gIterPos,gLastTerm,nInference)));
end;

procedure extItemObj.FinishDefiniens;
 var lExp: DefExpressionPtr;
begin
 case gDefiningWay of
 dfMeans:
  if gPartialDefs <> nil then
   begin
    lExp:=nil;
    if gOtherwise <> nil then
      lExp:=new(DefExpressionPtr,Init(exFormula,gOtherwise));
    gDefiniens:=new(ConditionalDefiniensPtr,Init(gMeansPos,gDefLabel,gPartialDefs,lExp))
   end
  else
   gDefiniens:=new(SimpleDefiniensPtr,Init(gMeansPos,gDefLabel,
                   new(DefExpressionPtr,Init(exFormula,gLastFormula))));
 dfEquals:
  if gPartialDefs <> nil then
   begin
    lExp:=nil;
    if gOtherwise <> nil then
      lExp:=new(DefExpressionPtr,Init(exTerm,gOtherwise));
    gDefiniens:=new(ConditionalDefiniensPtr,Init(gMeansPos,gDefLabel,gPartialDefs,lExp))
   end
  else
   gDefiniens:=new(SimpleDefiniensPtr,Init(gMeansPos,gDefLabel,new(DefExpressionPtr,Init(exTerm,gLastTerm))));
 end;
 if not gRedefinitions and (nItemKind = itDefFunc) then
  begin
   if gDefiningWay = dfMeans then
     gCorrectnessConditions:=[syExistence,syUniqueness]
    else if gDefiningWay = dfEquals then
     gCorrectnessConditions:=[syCoherence];
  end;
end;

 { S u b e x p r e s s i o n s   h a n d l i n g }

constructor extSubexpObj.Init;
 const MaxArgListNbr = 20;
begin
 inherited Init;
 nRestriction:=nil;
 nTermBase:=TermNbr;
 nArgListNbr:=0;
 setlength(nArgList,MaxArgListNbr+1);
 setlength(nFunc,MaxArgListNbr+1);
 nArgList[0].Start:=TermNbr+1;
end;

procedure extSubexpObj.StartAttributes;
begin
 nAttrCollection.Init(0);
 gLastType:=nil;
end;

procedure extSubexpObj.ProcessNon;
begin
 nNoneOcc:=CurWord.Kind = sy_Non;
 nNonPos:=CurPos;
end;

// Pop Args From The Terms Stack
function CreateArgs(aBase:integer): PList;
 var k:integer;
     lList: PList;
begin
 lList:=new(PList,Init(TermNbr-aBase));
 for k:=aBase to TermNbr do
  lList.Insert(Term[k]);
 TermNbr:=aBase-1;
 CreateArgs:=lList;
end;

procedure extSubexpObj.ProcessAttribute;
 var lFormatNr:integer;
begin
 if CurWord.Kind = AttributeSymbol then
  begin
   lFormatNr:=gFormatsColl.LookUp_PrefixFormat('V',CurWord.Nr,TermNbr-nTermBase+1);
   if lFormatNr = 0 then
    begin
     gLastAdjective:=new(AdjectivePtr,Init(CurPos,0,CreateArgs(nTermBase+1)));
     Error(CurPos,175)
    end
   else
    begin
     gLastAdjective:=new(AdjectivePtr,Init(CurPos,CurWord.Nr,CreateArgs(nTermBase+1)));
     if nNoneOcc then
       gLastAdjective:=new(NegatedAdjectivePtr,Init(nNonPos,gLastAdjective));
    end;
  end
 else
  begin
    gLastAdjective:=new(AdjectivePtr,Init(CurPos,0,CreateArgs(nTermBase+1)));
  end;
 nAttrCollection.Insert(gLastAdjective);
end;

procedure extSubexpObj.StartAttributeArguments;
begin
 nTermBase:=TermNbr;
end;

procedure extSubexpObj.CompleteAttributeArguments;
begin
 nSubexpPos:=CurPos;
 nRightArgBase:=TermNbr;
end;

procedure extSubexpObj.FinishAttributeArguments;
begin
 nSubexpPos:=CurPos;
 nRightArgBase:=TermNbr;
end;

procedure extSubexpObj.CompleteAdjectiveCluster;
begin
 gAttrColl:=new(PList,MoveList(nAttrCollection));
end;

procedure extSubexpObj.CompleteClusterTerm;
begin
 if TermNbr-nTermBase > 1 then
  begin
   ErrImm(379);
   gLastTerm:=new(IncorrectTermPtr,Init(CurPos));
  end;
end;

procedure extSubexpObj.ProcessSimpleTerm;
begin
 gLastTerm:=new(SimpleTermPtr,Init(CurPos,GetIdentifier));
end;

procedure extSubexpObj.ProcessQua;
begin
 nQuaPos:=CurPos
end;

procedure extSubexpObj.FinishQualifiedTerm;
begin
 Term[TermNbr]:=new(QualifiedTermPtr, Init(nQuaPos,Term[TermNbr],gLastType));
end;

procedure extSubexpObj.ProcessExactly;
begin
 nQuaPos:=CurPos;
 Term[TermNbr]:=new(ExactlyTermPtr, Init(nQuaPos,Term[TermNbr]));
end;

procedure CheckTermLimit;
 var l: integer;
begin
 if TermNbr >= length(Term) then
  begin
   l:=2*length(Term);
   setlength(Term,l);
  end;
end;

procedure extSubexpObj.FinishArgument;
begin
 CheckTermLimit;
 inc(TermNbr);
 Term[TermNbr]:=gLastTerm;
end;

procedure extSubexpObj.FinishTerm;
begin
 gLastTerm:=Term[TermNbr];
 dec(TermNbr);
end;

procedure extSubexpObj.StartType;
begin
 gLastType:=nil;
end;

procedure extSubexpObj.ProcessModeSymbol;
begin
 nModeKind:=CurWord.Kind;
 nModeNr:=CurWord.Nr;
 if (CurWord.Kind = sy_Set) {?and (AheadWord.Kind <> sy_Of)?} then
   nModeKind:=ModeSymbol;
 nSubexpPos:=CurPos;
end;

procedure extSubexpObj.FinishType;
 var lFormatNr: integer;
begin
 case nModeKind of
  ModeSymbol:
   begin
    lFormatNr:=gFormatsColl.LookUp_PrefixFormat('M',nModeNr,TermNbr-nTermBase);
    if lFormatNr=0 then Error(nSubexpPos,151);
    gLastType:=new(StandardTypePtr,Init(nSubexpPos,nModeNr,CreateArgs(nTermBase+1)));
   end;
  StructureSymbol:
   begin
    lFormatNr:=gFormatsColl.LookUp_PrefixFormat('L',nModeNr,TermNbr-nTermBase);
    if lFormatNr = 0 then SemErr(185);
    gLastType:=new(StructTypePtr,Init(nSubexpPos,nModeNr,CreateArgs(nTermBase+1)));
   end;
{?  sy_Set: gLastType:=new(SetTypePtr, Init(gLastType,nSubexpPos));?}
  else
   begin
    gLastType:=new(IncorrectTypePtr, Init(CurPos));
   end;
 end;
end;

procedure extSubexpObj.InsertIncorrType;
begin
 gLastType:=new(IncorrectTypePtr, Init(CurPos));
end;

procedure extSubexpObj.CompleteType;
 var j: integer;
begin
 mizassert(5433,gLastType <> nil);
 if nAttrCollection.Count > 0 then
  begin
   gLastType:=new(ClusteredTypePtr,Init(gLastType^.nTypePos,
                                        new(PList,Init(nAttrCollection.Count)),gLastType));
   for j := 0 to nAttrCollection.Count-1 do
     ClusteredTypePtr(gLastType)^.nAdjectiveCluster^.Insert(PObject(nAttrCollection.Items^[j]));
   nAttrCollection.DeleteAll;
  end;
end;

procedure extSubexpObj.StartLongTerm;
begin
 nArgListNbr:=0;
 nArgList[0].Length:=TermNbr-nTermBase;
end;

procedure extSubexpObj.FinishLongTerm;
 var
   ArgsLength: array of record l,r: integer; end;
   To_Right: array of boolean;
 procedure Exchange(i:integer);
  var l: integer;
 begin
  l:=ArgsLength[i].l;
  ArgsLength[i].l:=ArgsLength[i-1].r;
  ArgsLength[i-1].r:=l;
  To_Right[i-1]:=not To_Right[i-1];
 end;
 var
   ak,pl,ll,i,j,k,kn,Bl,new_Bl: integer;
   lTrm: TermPtr;
   lLeftArgs,lRightArgs: PList;
   DepoNbr: integer;
   Depo: array of record FuncInstNr:integer; dArgList:PList; end;
 label Corrected,AfterBalance;
begin
 setlength(ArgsLength,nArgListNbr+1);
 setlength(To_Right,nArgListNbr+1);
 setlength(Depo,nArgListNbr+1);
 ArgsLength[1].l:=nArgList[0].Length;
 To_Right[0]:=true;
 for k:=1 to nArgListNbr-1 do
  with ArgsLength[k] do
   if gPriority.Value(ord('O'),nFunc[k].Instance) < gPriority.Value(ord('O'),nFunc[k+1].Instance) then
    begin
     r:=1;
     ArgsLength[k+1].l:=nArgList[k].Length;
     To_Right[k]:=true
    end
   else
    begin
     r:=nArgList[k].Length;
     ArgsLength[k+1].l:=1;
     To_Right[k]:=false
    end;
 ArgsLength[nArgListNbr].r:=nArgList[nArgListNbr].Length;
 To_Right[nArgListNbr]:=false;
 with nFunc[1], ArgsLength[1] do
  begin
   if nArgListNbr = 1 then
    begin
     if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
      begin
       Error(FuncPos,165);
       goto AfterBalance
      end;
     goto AfterBalance;
    end;
   Bl:=1;
   if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
    begin
     Exchange(2);
     Bl:=2;
     if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
      begin
       Error(FuncPos,166);
       goto AfterBalance
      end;
    end;
  end;
 for k:=2 to nArgListNbr-1 do
  with nFunc[k], ArgsLength[k] do
   begin
    if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
     begin
      Exchange(k+1);
      new_Bl:=Bl;
      if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
       begin
        if Bl = k then
         begin
          Error(nFunc[k-1].FuncPos,168);
          Error(FuncPos,169);
          goto AfterBalance;
         end;
        Exchange(k+1);
        Exchange(k);
        new_Bl:=k;
        if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
         begin
          Exchange(k+1);
          new_Bl:=k+1;
          if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
           begin
            Error(FuncPos,167);
            goto AfterBalance
           end;
         end;
        for j:=k-1 downto Bl+1 do
         with nFunc[j], ArgsLength[j] do
          begin
           if gFormatsColl.LookUp_FuncFormat(Instance,l,r) <> 0 then goto Corrected;
           Exchange(j);
           if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
            begin
             Error(FuncPos,168);
             Error(nFunc[k].FuncPos,169);
             goto AfterBalance;
            end;
          end;
        with nFunc[Bl], ArgsLength[Bl] do
         if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
          begin
           Error(FuncPos,170);
           Error(nFunc[k].FuncPos,171);
           goto AfterBalance;
          end;
     end;
    Bl:=new_Bl;
   end;
Corrected:
  end;
 for j:=nArgListNbr downto Bl+1 do
  with nFunc[j], ArgsLength[j] do
   begin
    if gFormatsColl.LookUp_FuncFormat(Instance,l,r) <> 0 then goto AfterBalance;
    Exchange(j);
    if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
     begin
      Error(FuncPos,172);
      Error(nFunc[nArgListNbr].FuncPos,173);
      goto AfterBalance;
     end;
   end;
 with nFunc[Bl], ArgsLength[Bl] do
  if gFormatsColl.LookUp_FuncFormat(Instance,l,r) = 0 then
    begin
     Error(FuncPos,174);
     Error(nFunc[nArgListNbr].FuncPos,175);
     goto AfterBalance;
    end;
AfterBalance:
 for ak:=1 to nArgListNbr do
  begin
   ll:=1;
   pl:=1;
   if To_Right[ak-1] then ll:=nArgList[ak-1].Length;
   if not To_Right[ak] then pl:=nArgList[ak].Length;
   with nFunc[ak] do
    begin
     symPri:=gPriority.Value(ord('O'),Instance);
//?     Instance:=gFormatsColl.LookUp_FuncFormat(Instance,ll,pl);
    end;
  end;
 DepoNbr:=0;
 for kn:=nArgListNbr downto 2 do
  if To_Right[kn-1] then
   begin
    with nFunc[kn] do
     begin
      lRightArgs:=CreateArgs(nArgList[kn].Start);
      lLeftArgs:=CreateArgs(nArgList[kn-1].Start);
      lTrm:=new(InfixTermPtr,Init(FuncPos,Instance,lLeftArgs,lRightArgs));
     end;
    for j:=DepoNbr downto 1 do
     with Depo[j], nFunc[FuncInstNr] do
      begin
       if symPri <= nFunc[kn-1].SymPri then break;
       dec(DepoNbr);
       lLeftArgs:=new(PList,Init(1));
       lLeftArgs^.Insert(lTrm);
       lTrm:=new(InfixTermPtr,Init(FuncPos,Instance,lLeftArgs,dArgList));
      end;
    gLastTerm:=lTrm;
    gSubexpPtr^.FinishArgument;
   end
  else
   begin
    inc(DepoNbr);
    with Depo[DepoNbr] do
     begin
      FuncInstNr:=kn;
      dArgList:=CreateArgs(nArgList[kn].Start);
     end;
   end;
 with nFunc[1] do
  begin
   lRightArgs:=CreateArgs(nArgList[1].Start);
   lLeftArgs:=CreateArgs(nArgList[0].Start);
   lTrm:=new(InfixTermPtr,Init(FuncPos,Instance,lLeftArgs,lRightArgs));
  end;
 for j:=DepoNbr downto 1 do
  with Depo[j], nFunc[FuncInstNr] do
  begin
   lLeftArgs:=new(PList,Init(1));
   lLeftArgs^.Insert(lTrm);
   lTrm:=new(InfixTermPtr,Init(FuncPos,Instance,lLeftArgs,dArgList));
  end;
 gLastTerm:=lTrm;
end;

procedure extSubexpObj.ProcessFunctorSymbol;
 var l: integer;
begin
 inc(nArgListNbr);
 if nArgListNbr >= length(nFunc) then
  begin
   l:=2*length(nFunc)+1;
   setlength(nArgList,l);
   setlength(nFunc,l);
  end;
 nArgList[nArgListNbr].Start:=TermNbr+1;
 nFunc[nArgListNbr].FuncPos:=CurPos;
 nFunc[nArgListNbr].Instance:=CurWord.Nr;
end;

procedure extSubexpObj.FinishArgList;
begin
 nArgList[nArgListNbr].Length:=TermNbr-nArgList[nArgListNbr].Start+1;
end;

procedure extSubexpObj.StartFraenkelTerm;
begin
 nSample:=gLastTerm;
end;

procedure extSubexpObj.StartPostqualification;
begin
 nPostQualList.Init(0);
end;

procedure extSubexpObj.StartPostQualifyingSegment;
begin
 nSegmentIdentColl.Init(2);
end;

procedure extSubexpObj.ProcessPostqualifiedVariable;
begin
 nSegmentIdentColl.Insert(new(VariablePtr,Init(CurPos,GetIdentifier)));
end;

procedure extSubexpObj.StartPostqualificationSpecyfication;
begin
 nSegmentPos:=CurPos;
 gLastType:=nil;
end;

procedure extSubexpObj.FinishPostQualifyingSegment;
  var k: integer;
      lSegment: ExplicitlyQualifiedSegmentPtr;
begin
 if gLastType <> nil then
  begin
   lSegment:=new(ExplicitlyQualifiedSegmentPtr,
                   Init(nSegmentPos,new(PList,Init(0)),gLastType));
   nPostQualList.Insert(lSegment);
   for k := 0 to nSegmentIdentColl.Count - 1 do
    begin
     ExplicitlyQualifiedSegmentPtr(lSegment)^.nIdentifiers.Insert(nSegmentIdentColl.Items^[k]);
    end;
  end
 else
  begin
   for k := 0 to nSegmentIdentColl.Count - 1 do
    begin
     nPostQualList.Insert(new(ImplicitlyQualifiedSegmentPtr,
                              Init(VariablePtr(nSegmentIdentColl.Items^[k])^.nVarPos,
                                               nSegmentIdentColl.Items^[k])));
    end;
  end;
 nSegmentIdentColl.DeleteAll;
 nSegmentIdentColl.Done;
end;

procedure extSubexpObj.FinishFraenkelTerm;
begin
 gLastTerm:=new(FraenkelTermPtr,Init(CurPos,new(PList,MoveList(nPostQualList)),
                                     nSample,gLastFormula));
end;

procedure extSubexpObj.StartSimpleFraenkelTerm;
begin
 nAllPos:=CurPos;
end;

procedure extSubexpObj.FinishSimpleFraenkelTerm;
begin
 gLastTerm:=new(SimpleFraenkelTermPtr,Init(nAllPos,new(PList,MoveList(nPostQualList)),nSample));
end;

procedure extSubexpObj.StartPrivateTerm;
begin
 nSubexpPos:=CurPos;
 nSpelling:=CurWord.Nr;
end;

procedure extSubexpObj.FinishPrivateTerm;
begin
 gLastTerm:=new(PrivateFunctorTermPtr,Init(nSubexpPos,nSpelling,CreateArgs(nTermBase+1)));
end;

procedure extSubexpObj.StartBracketedTerm;
begin
 nSymbolNr:=CurWord.Nr;
end;

procedure extSubexpObj.FinishBracketedTerm;
 var lFormatNr: integer;
begin
 if StillCorrect then
  begin
   nRSymbolNr:=CurWord.Nr;
   lFormatNr:=gFormatsColl.LookUp_BracketFormat(nSymbolNr,nRSymbolNr,TermNbr-nTermBase,0,0);
   if lFormatNr=0 then SemErr(152);
   gLastTerm:=new(CircumfixTermPtr, Init(CurPos,nSymbolNr,nRSymbolNr,CreateArgs(nTermBase+1)));
  end;
end;

procedure extSubexpObj.StartAggregateTerm;
begin
 nSymbolNr:=CurWord.Nr;
end;

procedure extSubexpObj.FinishAggregateTerm;
 var lFormatNr: integer;
begin
 lFormatNr:=gFormatsColl.LookUp_PrefixFormat('G',nSymbolNr,TermNbr-nTermBase);
 if lFormatNr = 0 then Error(CurPos,176);
 gLastTerm:=new(AggregateTermPtr, Init(CurPos,nSymbolNr,CreateArgs(nTermBase+1)));
end;

procedure extSubexpObj.StartSelectorTerm;
begin
 nSymbolNr:=CurWord.Nr;
 nSubexpPos:=CurPos;
 nNextWord:=AheadWord.Kind;
end;

procedure extSubexpObj.FinishSelectorTerm;
 var lFormatNr: integer;
begin
 lFormatNr:=gFormatsColl.LookUp_PrefixFormat('U',nSymbolNr,1);
 if lFormatNr = 0 then Error(nSubexpPos,182);
 if nNextWord = sy_Of then
  gLastTerm:=new(SelectorTermPtr, Init(nSubexpPos,nSymbolNr,gLastTerm))
 else
  if in_AggrPattern then
   gLastTerm:=new(InternalSelectorTermPtr, Init(nSubexpPos,nSymbolNr))
  else
   begin
    gLastTerm:=new(IncorrectTermPtr, Init(nSubexpPos));
    Error(nSubexpPos,329)
   end;
end;

procedure extSubexpObj.StartForgetfulTerm;
begin
 nSymbolNr:=CurWord.Nr;
 nSubexpPos:=CurPos;
 nNextWord:=AheadWord.Kind;
end;

procedure extSubexpObj.FinishForgetfulTerm;
 var lFormatNr: integer;
begin
 lFormatNr:=0;
 if StillCorrect then
  begin
   lFormatNr:=gFormatsColl.LookUp_PrefixFormat('J',nSymbolNr,1);
   if lFormatNr = 0 then Error(nSubexpPos,184);
  end;
 gLastTerm:= new(ForgetfulFunctorTermPtr, Init(nSubexpPos,nSymbolNr,gLastTerm));
end;

procedure extSubexpObj.StartChoiceTerm;
begin
 nSubexpPos:=CurPos;
end;

procedure extSubexpObj.FinishChoiceTerm;
begin
 gLastTerm:=new(ChoiceTermPtr,Init(nSubexpPos,gLastType));
end;

procedure extSubexpObj.ProcessNumeralTerm;
begin
 gLastTerm:=new(NumeralTermPtr, Init(CurPos,CurWord.Nr));
end;

procedure extSubexpObj.ProcessItTerm;
begin
 if it_Allowed then gLastTerm:=new(ItTermPtr, Init(CurPos))
  else
  begin
   gLastTerm:=new(IncorrectTermPtr, Init(CurPos));
   ErrImm(251)
  end;
end;

procedure extSubexpObj.ProcessLocusTerm;
begin
 if dol_Allowed then
  gLastTerm:=new(PlaceholderTermPtr, Init(CurPos,CurWord.Nr))
 else
  begin
   gLastTerm:=new(IncorrectTermPtr, Init(CurPos));
   ErrImm(181)
  end;
end;

procedure extSubexpObj.InsertIncorrTerm;
begin
 gLastTerm:=new(IncorrectTermPtr, Init(CurPos));
end;

procedure extSubexpObj.InsertIncorrBasic;
begin
 gLastFormula:=new(IncorrectFormulaPtr,Init(CurPos));
 TermNbr:=nTermBase;
end;

procedure extSubexpObj.InsertIncorrFormula;
begin
 gLastFormula:=new(IncorrectFormulaPtr,Init(CurPos));
end;

procedure extSubexpObj.ProcessThesis;
begin
 if gProofCnt > 0 then
  gLastFormula:=new(ThesisFormulaPtr,Init(CurPos))
 else
  begin
   ErrImm(65);
   gLastFormula:=new(IncorrectFormulaPtr,Init(CurPos));
  end;
end;

procedure extSubexpObj.ProcessAtomicFormula;
 const MaxArgListNbr = 20;
begin
 nSubexpPos:=CurPos;
 nSymbolNr:=0;
 case CurWord.Kind of
  sy_Is:
   if TermNbr - nTermBase <> 1 then
    begin
     ErrImm(157);
     TermNbr:=nTermBase;
     InsertIncorrTerm;
     FinishArgument;
     { Trzeba chyba wstawic recovery dla TermNbr = nTermBase }
    end;
 end;
 nRightArgBase:=TermNbr;
 nTermBase:=TermNbr;
 nPostNegated:=false;
 nArgListNbr:=0;
 nArgList[0].Start:=TermNbr+1;
end;

procedure extSubexpObj.ProcessPredicateSymbol;
begin
 nSubexpPos:=CurPos;
 case CurWord.Kind of
  sy_Equal,PredicateSymbol: nSymbolNr:=CurWord.Nr;
  else nSymbolNr:=0;
 end;
 nRightArgBase:=TermNbr;
end;

procedure extSubexpObj.ProcessRightSideOfPredicateSymbol;
begin
 nRightSideOfPredPos:=CurPos;
 case CurWord.Kind of
  sy_Equal,PredicateSymbol: nSymbolNr:=CurWord.Nr;
  else nSymbolNr:=0;
 end;
 nRightArgBase:=TermNbr;
end;

procedure extSubexpObj.FinishPredicativeFormula;
 var lLeftArgs,lRightArgs: PList;
     lFormatNr: integer;
begin
 lFormatNr:=gFormatsColl.LookUp_PredFormat(nSymbolNr,nRightArgBase-nTermBase,TermNbr-nRightArgBase);
 if lFormatNr = 0 then Error(nSubexpPos,153);
 lRightArgs:=CreateArgs(nRightArgBase+1);
 lLeftArgs:=CreateArgs(nTermBase+1);
 gLastFormula:=new(PredicativeFormulaPtr,Init(nSubexpPos,nSymbolNr,lLeftArgs,lRightArgs));
end;

procedure extSubexpObj.FinishRightSideOfPredicativeFormula;
 var lRightArgs: PList;
     lLeftArgsNbr,lFormatNr: integer;
     lFrm:FormulaPtr;
begin
 lFrm:=gLastFormula;
 if lFrm^.nFormulaSort = wsNegatedFormula then
   lFrm:=NegativeFormulaPtr(lFrm)^.nArg;
 lLeftArgsNbr:=RightSideOfPredicativeFormulaPtr(lFrm)^.nRightArgs^.Count;
 lFormatNr:=gFormatsColl.LookUp_PredFormat(nSymbolNr,lLeftArgsNbr,TermNbr-nRightArgBase);
 if lFormatNr = 0 then Error(nSubexpPos,153);
 lRightArgs:=CreateArgs(nRightArgBase+1);
 gLastFormula:=new(RightSideOfPredicativeFormulaPtr,Init(nSubexpPos,nSymbolNr,lRightArgs));
 nMultiPredicateList.Insert(gLastFormula);
end;

procedure extSubexpObj.StartMultiPredicativeFormula;
begin
 nMultiPredicateList.Init(4);
 nMultiPredicateList.Insert(gLastFormula);
end;

procedure extSubexpObj.FinishMultiPredicativeFormula;
begin
 gLastFormula:=new(MultiPredicativeFormulaPtr,Init(nSubexpPos,new(PList,MoveList(nMultiPredicateList))));
end;

procedure extSubexpObj.FinishQualifyingFormula;
 var j: integer;
begin
 mizassert(5430,gLastType <> nil);
 if nAttrCollection.Count > 0 then
  begin
   gLastType:=new(ClusteredTypePtr,
      Init(gLastType^.nTypePos,new(PList,Init(nAttrCollection.Count)),gLastType));
   for j := 0 to nAttrCollection.Count-1 do
     ClusteredTypePtr(gLastType)^.nAdjectiveCluster^.Insert(PObject(nAttrCollection.Items^[j]));
  end;
 gLastFormula:=new(QualifyingFormulaPtr,Init(nSubexpPos,Term[TermNbr],gLastType));
 if nPostNegated then
   gLastFormula:=new(NegativeFormulaPtr,Init(nNotPos,gLastFormula));
 dec(TermNbr);
end;

procedure extSubexpObj.FinishAttributiveFormula;
begin
 gLastFormula:=
   new(AttributiveFormulaPtr,Init(nSubExpPos,Term[TermNbr],new(PList,MoveList(nAttrCollection))));
 if nPostNegated then
   gLastFormula:=new(NegativeFormulaPtr,Init(nNotPos,gLastFormula));
 dec(TermNbr);
end;

procedure extSubexpObj.StartPrivateFormula;
begin
 nTermBase:=TermNbr;
 nSubexpPos:=CurPos;
 nSpelling:=CurWord.Nr;
end;

procedure extSubexpObj.FinishPrivateFormula;
begin
 gLastFormula:=new(PrivatePredicativeFormulaPtr,Init(nSubexpPos,nSpelling,CreateArgs(nTermBase+1)));
end;

procedure extSubexpObj.ProcessContradiction;
begin
 gLastFormula:=new(ContradictionFormulaPtr,Init(CurPos));
end;

procedure extSubexpObj.ProcessNegative;
begin
 gLastFormula:=new(NegativeFormulaPtr,Init(CurPos,gLastFormula));
end;

procedure extSubexpObj.ProcessNegation;
begin
 nPostNegated:=not nPostNegated;
 nNotPos:=CurPos;
end;

procedure extSubexpObj.ProcessBinaryConnective;
begin
 nConnective:=CurWord.Kind;
 nFirstSententialOperand:=gLastFormula;
 nSubexpPos:=CurPos;
end;

procedure extSubexpObj.ProcessFlexDisjunction;
begin
 nFirstSententialOperand:=gLastFormula;
end;

procedure extSubexpObj.ProcessFlexConjunction;
begin
 nFirstSententialOperand:=gLastFormula;
end;

procedure extSubexpObj.StartRestriction;
begin
 nRestrPos:=CurPos;
end;

procedure extSubexpObj.FinishRestriction;
begin
 nRestriction:=gLastFormula;
end;

procedure extSubexpObj.FinishBinaryFormula;
begin
 case nConnective of
  sy_Implies:
   gLastFormula:=new(ConditionalFormulaPtr,Init(nSubExpPos,nFirstSententialOperand,gLastFormula));
  sy_Iff:
   gLastFormula:=new(BiconditionalFormulaPtr,Init(nSubexpPos,nFirstSententialOperand,gLastFormula));
  sy_Or:
   gLastFormula:=new(DisjunctiveFormulaPtr,Init(nSubexpPos,nFirstSententialOperand,gLastFormula));
  sy_Ampersand:
   gLastFormula:=new(ConjunctiveFormulaPtr,Init(nSubexpPos,nFirstSententialOperand,gLastFormula));
  else
   RunTimeError(3124);
 end;
end;

procedure extSubexpObj.FinishFlexDisjunction; // polaczyc z flexConj
begin
 gLastFormula:=new(FlexaryDisjunctiveFormulaPtr,
                   Init(CurPos,nFirstSententialOperand,gLastFormula));
end;

procedure extSubexpObj.FinishFlexConjunction;
begin
 gLastFormula:=new(FlexaryConjunctiveFormulaPtr,
                   Init(CurPos,nFirstSententialOperand,gLastFormula));
end;

procedure extSubexpObj.StartExistential;
begin
 nQualifiedSegments.Init(0);
 nSubexpPos:=CurPos;
end;

procedure extSubexpObj.StartUniversal;
begin
 nQualifiedSegments.Init(0);
 nSubexpPos:=CurPos;
end;

procedure extSubexpObj.StartQualifiedSegment;
begin
 nSegmentIdentColl.Init(2);
 nSegmentPos:=CurPos;
end;

procedure extSubexpObj.StartQualifyingType;
begin
 gLastType:=nil;
end;

procedure extSubexpObj.FinishQualifiedSegment;
 var k: integer;
begin
 if gLastType = nil then
   begin
    for k := 0 to nSegmentIdentColl.Count - 1 do
     begin
      nQualifiedSegments.Insert(new(ImplicitlyQualifiedSegmentPtr,
                            Init(VariablePtr(nSegmentIdentColl.Items^[k])^.nVarPos,
                                 nSegmentIdentColl.Items^[k])));
      nSegmentIdentColl.Items^[k]:=nil;
     end;
    nSegmentIdentColl.Done;
   end
  else
   begin
     nQualifiedSegments.Insert(new(ExplicitlyQualifiedSegmentPtr,
                           Init(nSegmentPos,new(PList,MoveList(nSegmentIdentColl)),gLastType)));
   end;
end;

procedure extSubexpObj.ProcessVariable;
begin
 nSegmentIdentColl.Insert(new(VariablePtr,Init(CurPos,GetIdentifier)));
end;

procedure extSubexpObj.FinishExistential;
 var k:integer;
begin
 for k:=nQualifiedSegments.Count-1 downto 1 do
  begin
    gLastFormula:=new(ExistentialFormulaPtr,
                      Init(QualifiedSegmentPtr(nQualifiedSegments.Items^[k])^.nSegmPos,
                           nQualifiedSegments.Items^[k],gLastFormula));
    nQualifiedSegments.Items^[k]:=nil;
  end;
 if nQualifiedSegments.Count > 0 then
  begin
    gLastFormula:=new(ExistentialFormulaPtr,
                      Init(nSubexpPos,nQualifiedSegments.Items^[0],gLastFormula));
    nQualifiedSegments.Items^[0]:=nil;
  end;
  nQualifiedSegments.Done;
end;

procedure extSubexpObj.FinishUniversal;
 var k:integer;
begin
 if nRestriction <> nil then
   gLastFormula:=new(ConditionalFormulaPtr,Init(nRestrPos,nRestriction,gLastFormula));
 for k:=nQualifiedSegments.Count-1 downto 1 do
  begin
    gLastFormula:=new(UniversalFormulaPtr,
                     Init(QualifiedSegmentPtr(nQualifiedSegments.Items^[k])^.nSegmPos,
                          nQualifiedSegments.Items^[k],gLastFormula));
    nQualifiedSegments.Items^[k]:=nil;
  end;
 if nQualifiedSegments.Count > 0 then
  begin
    gLastFormula:=new(UniversalFormulaPtr,
                      Init(nSubexpPos,nQualifiedSegments.Items^[0],gLastFormula));
    nQualifiedSegments.Items^[0]:=nil;
  end;
end;

constructor extExpressionObj.Init(fExpKind:ExpKind);
begin
 inherited Init(fExpKind);
 TermNbr:=0;
end;

procedure extExpressionObj.CreateSubexpression;
begin
 gSubexpPtr:=new(extSubexpPtr,Init)
end;

end.
