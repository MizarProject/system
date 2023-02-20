(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit syntax;

interface

uses mobjects,errhan;

const
 MaxVisArgNbr = 10;

type
 BlockKind =
  ( blMain, blDiffuse, blHereby, blProof, blDefinition, blNotation,
    blRegistration, blCase, blSuppose, blPublicScheme );

 ItemKind =
  ( itIncorrItem,
    itDefinition, itSchemeBlock, itSchemeHead, itTheorem, itAxiom,
    itReservation,
    itCanceled, itSection,
    itRegularStatement, itChoice, itReconsider,
    itPrivFuncDefinition, itPrivPredDefinition, itConstantDefinition,
    itGeneralization, itLociDeclaration,itExistentialAssumption, itExemplification,
    itPerCases, itConclusion, itCaseBlock, itCaseHead, itSupposeHead, itAssumption,
    itCorrCond, itCorrectness, itProperty,
    itDefPred, itDefFunc, itDefMode, itDefAttr, itDefStruct,
    itPredSynonym, itPredAntonym, itFuncNotation, itModeNotation,
    itAttrSynonym, itAttrAntonym,
    itCluster, itIdentify, itReduction, itPropertyRegistration,
    itPragma
  );

 ExpKind = ( exNull, exType, exTerm, exFormula, exResType, exAdjectiveCluster );

 BlockPtr = ^BlockObj;
 ItemPtr = ^ItemObj;

 BlockObj =
  object(StackedObj)
    nBlockKind:BlockKind;
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual; //inheritance
   destructor Done; virtual;
   procedure StartProperText; virtual;
   procedure ProcessLink; virtual;
   procedure ProcessRedefine; virtual;
   procedure ProcessBegin; virtual;
   procedure ProcessPragma; virtual;
   procedure StartAtSignProof; virtual;
   procedure FinishAtSignProof; virtual;
   procedure FinishDefinition; virtual;
   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
   procedure StartSchemeDemonstration; virtual;
   procedure FinishSchemeDemonstration; virtual;
  end;

 ItemObj =
  object(StackedObj)
    nItemKind: ItemKind;
   constructor Init(fItemKind:ItemKind);
   procedure Pop; virtual;
   destructor Done; virtual;
   procedure StartAttributes; virtual;
   procedure FinishAntecedent; virtual;
   procedure FinishConsequent; virtual;
   procedure FinishClusterTerm; virtual;
   procedure FinishClusterType; virtual;
   procedure StartSentence; virtual;
   procedure FinishSentence; virtual;
   procedure FinishPrivateConstant; virtual;
   procedure StartPrivateConstant; virtual;
   procedure ProcessReconsideredVariable; virtual;
   procedure FinishReconsidering; virtual;
   procedure FinishReconsideredTerm; virtual;
   procedure FinishDefaultTerm; virtual;
   procedure StartNewType; virtual;
   procedure StartCondition; virtual;
   procedure FinishCondition; virtual;
   procedure FinishChoice; virtual;
   procedure StartFixedVariables; virtual;
   procedure ProcessFixedVariable; virtual;
   procedure FinishFixedVariables; virtual;
   procedure ProcessBeing; virtual;
   procedure StartFixedSegment; virtual;
   procedure FinishFixedSegment; virtual;
   procedure StartAssumption; virtual;
   procedure StartCollectiveAssumption; virtual;
   procedure FinishHypothesis; virtual;
   procedure FinishAssumption; virtual;
   procedure ProcessExemplifyingVariable; virtual;
   procedure FinishExemplifyingVariable; virtual;
   procedure StartExemplifyingTerm; virtual;
   procedure FinishExemplifyingTerm; virtual;
   procedure ProcessMeans; virtual;
   procedure FinishOtherwise; virtual;
   procedure StartDefiniens; virtual;
   procedure FinishDefiniens; virtual;
   procedure StartGuard; virtual;
   procedure FinishGuard; virtual;
   procedure ProcessEquals; virtual;
   procedure StartEquals; virtual;
   procedure StartOtherwise; virtual;
   procedure ProcessCorrectness; virtual;
   procedure StartSpecification; virtual;
   procedure StartExpansion; virtual;
   procedure FinishSpecification; virtual;
   procedure StartConstructionType; virtual;
   procedure FinishConstructionType; virtual;
   procedure StartAttributePattern; virtual;
   procedure ProcessAttributePattern; virtual;
   procedure FinishAttributePattern; virtual;
   procedure StartDefPredicate; virtual;
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
   procedure CompletePredAntonymByAttr; virtual;
   procedure CompletePredSynonymByAttr; virtual;
   procedure ProcessFuncSynonym; virtual;
   procedure ProcessModeSynonym; virtual;

   procedure StartFuncIdentify; virtual;
   procedure ProcessFuncIdentify; virtual;
   procedure CompleteFuncIdentify; virtual;
   procedure StartPredIdentify; virtual;
   procedure ProcessPredIdentify; virtual;
   procedure CompleteAttrIdentify; virtual;
   procedure StartAttrIdentify; virtual;
   procedure ProcessAttrIdentify; virtual;
   procedure CompletePredIdentify; virtual;
   procedure ProcessLeftLocus; virtual;
   procedure ProcessRightLocus; virtual;

   procedure StartFuncReduction; virtual;
   procedure ProcessFuncReduction; virtual;
   procedure FinishFuncReduction; virtual;

   procedure StartSethoodProperties; virtual;
   procedure FinishSethoodProperties; virtual;

   procedure StartModePattern; virtual;
   procedure ProcessModePattern; virtual;
   procedure FinishModePattern; virtual;
   procedure StartVisible; virtual;
   procedure ProcessVisible; virtual;
   procedure FinishVisible; virtual;
   procedure StartPrefix; virtual;
   procedure FinishPrefix; virtual;
   procedure ProcessStructureSymbol; virtual;
   procedure StartFields; virtual;
   procedure FinishFields; virtual;
   procedure StartAggrPattSegment; virtual;
   procedure ProcessField; virtual;
   procedure FinishAggrPattSegment; virtual;
   procedure ProcessSchemeName; virtual;
   procedure StartSchemeSegment; virtual;
   procedure ProcessSchemeVariable; virtual;
   procedure StartSchemeQualification; virtual;
   procedure FinishSchemeQualification; virtual;
   procedure FinishSchemeSegment; virtual;
   procedure FinishSchemeHeading; virtual;
   procedure FinishSchemeDeclaration; virtual;
   procedure FinishSchemeThesis; virtual;
   procedure StartSchemePremise; virtual;
   procedure FinishSchemePremise; virtual;
   procedure StartTheoremBody; virtual;
   procedure FinishTheoremBody; virtual;
   procedure FinishTheorem; virtual;
   procedure StartReservationSegment; virtual;
   procedure ProcessReservedIdentifier; virtual;
   procedure FinishReservationSegment; virtual;
   procedure FinishReservation; virtual;
   procedure StartPrivateDefiniendum; virtual;
   procedure StartPrivateDefiniens; virtual;
   procedure FinishPrivateFuncDefinienition; virtual;
   procedure FinishPrivatePredDefinienition; virtual;
   procedure FinishLocusType; virtual;
   procedure ProcessLabel; virtual;
   procedure StartRegularStatement; virtual;
   procedure ProcessDefiniensLabel; virtual;
   procedure FinishCompactStatement; virtual;
   procedure StartIterativeStep; virtual;
   procedure ProcessIterativeStep; virtual;
   procedure FinishIterativeStep; virtual;

   { J u s t i f i c a t i o n }

   procedure ProcessSchemeReference; virtual;
   procedure StartSchemeReference; virtual;
   procedure StartReferences; virtual;
   procedure ProcessPrivateReference; virtual;
   procedure StartLibraryReferences; virtual;
   procedure StartSchemeLibraryReference; virtual;
   procedure ProcessDef; virtual;
   procedure ProcessSch; virtual;
   procedure ProcessTheoremNumber; virtual;
   procedure ProcessSchemeNumber; virtual;
   procedure FinishTheLibraryReferences; virtual;
   procedure FinishSchLibraryReferences; virtual;
   procedure FinishReferences; virtual;
   procedure FinishSchemeReference; virtual;
   procedure StartJustification; virtual;
   procedure FinishJustification; virtual;
   procedure StartSimpleJustification; virtual;
   procedure FinishSimpleJustification; virtual;


   procedure CreateExpression(fExpKind:ExpKind); virtual;
  end;

 SubexpPtr = ^SubexpObj;
 SubexpObj =
  object(StackedObj)
   constructor Init;
   destructor Done; virtual;
   procedure ProcessSimpleTerm; virtual;
   procedure StartFraenkelTerm; virtual;
   procedure StartPostqualification; virtual;
   procedure StartPostqualifyingSegment; virtual;
   procedure ProcessPostqualifiedVariable; virtual;
   procedure StartPostqualificationSpecyfication; virtual;
   procedure FinishPostqualifyingSegment; virtual;
   procedure FinishSample; virtual;
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
   procedure ProcessThe; virtual;
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
   procedure StartArgument; virtual;
   procedure FinishArgument; virtual;
   procedure FinishTerm; virtual;
   procedure StartType; virtual;
   procedure ProcessModeSymbol; virtual;
   procedure FinishType; virtual;
   procedure CompleteType; virtual;
   procedure ProcessLeftParenthesis; virtual;
   procedure ProcessRightParenthesis; virtual;
   procedure StartAtomicFormula; virtual;
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

   procedure ProcessNot; virtual;
   procedure ProcessDoesNot; virtual;
   procedure ProcessNegative; virtual;

{ Jest to tymczasowe rozwiazanie, generowanie ExpNode'ow jest takie,
  ze nie ma mozliwosci obsluzenia jednolicie negacji.
}
   procedure ProcessNegation; virtual;
   procedure FinishQualifyingFormula; virtual;
   procedure FinishAttributiveFormula; virtual;
   procedure ProcessBinaryConnective; virtual;
   procedure ProcessFlexDisjunction; virtual;
   procedure ProcessFlexConjunction; virtual;
   procedure StartRestriction; virtual;
   procedure FinishRestriction; virtual;
   procedure ProcessHolds; virtual;
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
   procedure FinishQuantified; virtual;
   procedure ProcessVariable; virtual;
   procedure StartAttributes; virtual;
   procedure StartAdjectiveCluster; virtual;
   procedure FinishAdjectiveCluster; virtual;

   procedure ProcessNon; virtual;
   procedure ProcessAttribute; virtual;
   procedure FinishAttributes; virtual;
   procedure CompleteAttributes; virtual;
   procedure StartAttributeArguments; virtual;
   procedure CompleteAttributeArguments; virtual;
   procedure FinishAttributeArguments; virtual;
   procedure CompleteAdjectiveCluster; virtual;
   procedure CompleteClusterTerm; virtual;
   procedure CompleteClusterType; virtual;
   procedure FinishEquality; virtual;
   { E r r o r s   r e c o v e r y }

   procedure InsertIncorrTerm; virtual;
   procedure InsertIncorrType; virtual;
   procedure InsertIncorrBasic; virtual;
   procedure InsertIncorrFormula; virtual;
  end;

 ExpressionPtr = ^ExpressionObj;
 ExpressionObj =
  object(MObject)
    nExpKind: ExpKind;
   constructor Init(fExpKind:ExpKind);
   procedure CreateSubexpression; virtual;
  end;

procedure KillBlock;
procedure KillItem;
procedure KillExpression;
procedure KillSubexpression;

var
   gBlockPtr	 : BlockPtr = nil;
   gItemPtr	   : ItemPtr = nil;
   gExpPtr	   : ExpressionPtr = nil;
   gSubexpPtr	 : SubexpPtr = nil;

implementation

uses mconsole
{$IFDEF MDEBUG} ,info {$ENDIF};

constructor SubexpObj.Init;
begin
  Previous:=gSubexpPtr;
end;

destructor SubexpObj.Done;
begin
  gSubexpPtr:=SubexpPtr(Previous);
end;

constructor ExpressionObj.Init(fExpKind:ExpKind);
begin
  nExpKind:=fExpKind;
end;

procedure SubexpObj.StartAttributes; begin end;

procedure SubexpObj.StartAdjectiveCluster; begin end;

procedure SubexpObj.FinishAdjectiveCluster; begin end;

procedure SubexpObj.ProcessNon; begin end;

procedure SubexpObj.ProcessAttribute; begin end;

procedure SubexpObj.FinishAttributes; begin end;

procedure SubexpObj.CompleteAttributes; begin end;

procedure SubexpObj.StartAttributeArguments; begin end;

procedure SubexpObj.CompleteAttributeArguments; begin end;

procedure SubexpObj.FinishAttributeArguments; begin end;

procedure SubexpObj.CompleteAdjectiveCluster; begin end;

procedure SubexpObj.CompleteClusterTerm; begin end;

procedure SubexpObj.CompleteClusterType; begin end;

procedure SubexpObj.ProcessSimpleTerm; begin end;

procedure SubexpObj.ProcessQua; begin end;

procedure SubexpObj.FinishQualifiedTerm; begin end;

procedure SubexpObj.ProcessExactly; begin end;

procedure SubexpObj.StartArgument; begin end;

procedure SubexpObj.FinishArgument; begin end;

procedure SubexpObj.FinishTerm; begin end;

procedure SubexpObj.StartType; begin end;

procedure SubexpObj.ProcessModeSymbol; begin end;

procedure SubexpObj.FinishType; begin end;

procedure SubexpObj.CompleteType; begin end;

procedure SubexpObj.StartLongTerm; begin end;

procedure SubexpObj.FinishLongTerm; begin end;

procedure SubexpObj.FinishArgList; begin end;

procedure SubexpObj.ProcessFunctorSymbol; begin end;

procedure SubexpObj.StartFraenkelTerm; begin end;

procedure SubexpObj.FinishSample; begin end;

procedure SubexpObj.StartPostqualification; begin end;

procedure SubexpObj.StartPostqualificationSpecyfication; begin end;

procedure SubexpObj.StartPostqualifyingSegment; begin end;

procedure SubexpObj.ProcessPostqualifiedVariable; begin end;

procedure SubexpObj.FinishPostqualifyingSegment; begin end;

procedure SubexpObj.FinishFraenkelTerm; begin end;

procedure SubexpObj.StartSimpleFraenkelTerm; begin end;

procedure SubexpObj.FinishSimpleFraenkelTerm; begin end;

procedure SubexpObj.StartPrivateTerm; begin end;

procedure SubexpObj.FinishPrivateTerm; begin end;

procedure SubexpObj.StartBracketedTerm; begin end;

procedure SubexpObj.FinishBracketedTerm; begin end;

procedure SubexpObj.StartAggregateTerm; begin end;

procedure SubexpObj.FinishAggregateTerm; begin end;

procedure SubexpObj.ProcessThe; begin end;

procedure SubexpObj.StartSelectorTerm; begin end;

procedure SubexpObj.FinishSelectorTerm; begin end;

procedure SubexpObj.StartForgetfulTerm; begin end;

procedure SubexpObj.FinishForgetfulTerm; begin end;

procedure SubexpObj.StartChoiceTerm; begin end;

procedure SubexpObj.FinishChoiceTerm; begin end;

procedure SubexpObj.ProcessNumeralTerm; begin end;

procedure SubexpObj.ProcessItTerm; begin end;

procedure SubexpObj.ProcessLocusTerm; begin end;

procedure SubexpObj.ProcessThesis; begin end;

procedure SubexpObj.StartAtomicFormula; begin end;

procedure SubexpObj.ProcessAtomicFormula; begin end;

procedure SubexpObj.ProcessPredicateSymbol; begin end;

procedure SubexpObj.ProcessRightSideOfPredicateSymbol; begin end;

procedure SubexpObj.FinishPredicativeFormula; begin end;

procedure SubexpObj.FinishRightSideOfPredicativeFormula; begin end;

procedure SubexpObj.StartMultiPredicativeFormula; begin end;

procedure SubexpObj.FinishMultiPredicativeFormula; begin end;

procedure SubexpObj.FinishQualifyingFormula; begin end;

procedure SubexpObj.FinishAttributiveFormula; begin end;

procedure SubexpObj.StartPrivateFormula; begin end;

procedure SubexpObj.FinishPrivateFormula; begin end;

procedure SubexpObj.ProcessContradiction; begin end;

procedure SubexpObj.ProcessNot; begin end;

procedure SubexpObj.ProcessDoesNot; begin end;

procedure SubexpObj.ProcessNegative; begin end;

procedure SubexpObj.ProcessNegation; begin end;

procedure SubexpObj.StartRestriction; begin end;

procedure SubexpObj.FinishRestriction; begin end;

procedure SubexpObj.ProcessHolds; begin end;

procedure SubexpObj.ProcessBinaryConnective; begin end;

procedure SubexpObj.FinishBinaryFormula; begin end;

procedure SubexpObj.ProcessFlexDisjunction; begin end;

procedure SubexpObj.ProcessFlexConjunction; begin end;

procedure SubexpObj.FinishFlexDisjunction; begin end;

procedure SubexpObj.FinishFlexConjunction; begin end;

procedure SubexpObj.StartQualifiedSegment; begin end;

procedure SubexpObj.StartQualifyingType; begin end;

procedure SubexpObj.FinishQualifiedSegment; begin end;

procedure SubexpObj.FinishQuantified; begin end;

procedure SubexpObj.ProcessVariable; begin end;

procedure SubexpObj.StartExistential; begin end;

procedure SubexpObj.FinishExistential; begin end;

procedure SubexpObj.StartUniversal; begin end;

procedure SubexpObj.FinishUniversal; begin end;

procedure SubexpObj.ProcessLeftParenthesis; begin end;

procedure SubexpObj.ProcessRightParenthesis; begin end;

procedure SubexpObj.InsertIncorrType; begin end;

procedure SubexpObj.InsertIncorrTerm; begin end;

procedure SubexpObj.InsertIncorrBasic; begin end;

procedure SubexpObj.InsertIncorrFormula; begin end;

procedure SubexpObj.FinishEquality; begin end;

procedure ExpressionObj.CreateSubexpression;
begin
  gSubexpPtr:=new(SubexpPtr, Init);
end;

constructor ItemObj.Init(fItemKind:ItemKind);
begin
  nItemKind:=fItemKind;
  Previous:=gItemPtr;
end;

procedure ItemObj.Pop;
begin
  DisplayLine(CurPos.Line,ErrorNbr);
end;

destructor ItemObj.Done;
begin
  DisplayLine(CurPos.Line,ErrorNbr);
  gItemPtr:=ItemPtr(Previous);
end;

procedure ItemObj.StartAttributes; begin end;

procedure ItemObj.FinishAntecedent; begin end;

procedure ItemObj.FinishConsequent; begin end;

procedure ItemObj.FinishClusterTerm; begin end;

procedure ItemObj.FinishClusterType; begin end;

procedure ItemObj.StartSentence; begin end;

procedure ItemObj.FinishSentence; begin end;

procedure ItemObj.FinishPrivateConstant; begin end;

procedure ItemObj.StartPrivateConstant; begin end;

procedure ItemObj.ProcessReconsideredVariable; begin end;

procedure ItemObj.FinishReconsidering; begin end;

procedure ItemObj.FinishReconsideredTerm; begin end;

procedure ItemObj.FinishDefaultTerm; begin end;

procedure ItemObj.StartNewType; begin end;

procedure ItemObj.StartCondition; begin end;

procedure ItemObj.FinishCondition; begin end;

procedure ItemObj.FinishChoice; begin end;

procedure ItemObj.StartFixedVariables; begin end;

procedure ItemObj.StartFixedSegment; begin end;

procedure ItemObj.ProcessFixedVariable; begin end;

procedure ItemObj.ProcessBeing; begin end;

procedure ItemObj.FinishFixedSegment; begin end;

procedure ItemObj.FinishFixedVariables; begin end;

procedure ItemObj.StartAssumption; begin end;

procedure ItemObj.StartCollectiveAssumption; begin end;

procedure ItemObj.FinishHypothesis; begin end;

procedure ItemObj.FinishAssumption; begin end;

procedure ItemObj.ProcessExemplifyingVariable; begin end;

procedure ItemObj.FinishExemplifyingVariable; begin end;

procedure ItemObj.StartExemplifyingTerm; begin end;

procedure ItemObj.FinishExemplifyingTerm; begin end;

procedure ItemObj.ProcessMeans; begin end;

procedure ItemObj.FinishOtherwise; begin end;

procedure ItemObj.StartDefiniens; begin end;

procedure ItemObj.FinishDefiniens; begin end;

procedure ItemObj.StartGuard; begin end;

procedure ItemObj.FinishGuard; begin end;

procedure ItemObj.StartOtherwise; begin end;

procedure ItemObj.ProcessEquals; begin end;

procedure ItemObj.StartEquals; begin end;

procedure ItemObj.ProcessCorrectness; begin end;

procedure ItemObj.FinishSpecification; begin end;

procedure ItemObj.FinishConstructionType; begin end;

procedure ItemObj.StartSpecification; begin end;

procedure ItemObj.StartExpansion; begin end;

procedure ItemObj.StartConstructionType; begin end;

procedure ItemObj.StartPredicatePattern; begin end;

procedure ItemObj.ProcessPredicateSymbol; begin end;

procedure ItemObj.FinishPredicatePattern; begin end;

procedure ItemObj.StartFunctorPattern; begin end;

procedure ItemObj.ProcessFunctorSymbol; begin end;

procedure ItemObj.FinishFunctorPattern; begin end;

procedure ItemObj.ProcessAttrAntonym; begin end;

procedure ItemObj.ProcessAttrSynonym; begin end;

procedure ItemObj.ProcessPredAntonym; begin end;

procedure ItemObj.ProcessPredSynonym; begin end;

procedure ItemObj.ProcessFuncSynonym; begin end;

procedure ItemObj.CompletePredSynonymByAttr; begin end;

procedure ItemObj.CompletePredAntonymByAttr; begin end;

procedure ItemObj.ProcessModeSynonym; begin end;

procedure ItemObj.StartFuncIdentify;  begin end;

procedure ItemObj.ProcessFuncIdentify;  begin end;

procedure ItemObj.CompleteFuncIdentify;  begin end;

procedure ItemObj.StartPredIdentify;  begin end;

procedure ItemObj.ProcessPredIdentify;  begin end;

procedure ItemObj.CompletePredIdentify;  begin end;

procedure ItemObj.StartAttrIdentify;  begin end;

procedure ItemObj.ProcessAttrIdentify;  begin end;

procedure ItemObj.CompleteAttrIdentify;  begin end;

procedure ItemObj.ProcessLeftLocus;  begin end;

procedure ItemObj.ProcessRightLocus;  begin end;

procedure ItemObj.StartFuncReduction;  begin end;

procedure ItemObj.ProcessFuncReduction;  begin end;

procedure ItemObj.FinishFuncReduction;  begin end;

procedure ItemObj.StartSethoodProperties;  begin end;

procedure ItemObj.FinishSethoodProperties;  begin end;

procedure ItemObj.StartModePattern; begin end;

procedure ItemObj.ProcessModePattern; begin end;

procedure ItemObj.FinishModePattern; begin end;

procedure ItemObj.StartAttributePattern; begin end;

procedure ItemObj.ProcessAttributePattern; begin end;

procedure ItemObj.FinishAttributePattern; begin end;

procedure ItemObj.StartDefPredicate; begin end;

procedure ItemObj.StartVisible; begin end;

procedure ItemObj.ProcessVisible; begin end;

procedure ItemObj.FinishVisible; begin end;

procedure ItemObj.StartPrefix; begin end;

procedure ItemObj.FinishPrefix; begin end;

procedure ItemObj.ProcessStructureSymbol; begin end;

procedure ItemObj.StartFields; begin end;

procedure ItemObj.FinishFields; begin end;

procedure ItemObj.StartAggrPattSegment; begin end;

procedure ItemObj.ProcessField; begin end;

procedure ItemObj.FinishAggrPattSegment; begin end;

procedure ItemObj.ProcessSchemeName; begin end;

procedure ItemObj.StartSchemeSegment; begin end;

procedure ItemObj.ProcessSchemeVariable; begin end;

procedure ItemObj.StartSchemeQualification; begin end;

procedure ItemObj.FinishSchemeQualification; begin end;

procedure ItemObj.FinishSchemeSegment; begin end;

procedure ItemObj.FinishSchemeHeading; begin end;

procedure ItemObj.FinishSchemeDeclaration; begin end;

procedure ItemObj.FinishSchemeThesis; begin end;

procedure ItemObj.StartSchemePremise; begin end;

procedure ItemObj.FinishSchemePremise; begin end;

procedure ItemObj.StartTheoremBody; begin end;

procedure ItemObj.FinishTheoremBody; begin end;

procedure ItemObj.FinishTheorem; begin end;

procedure ItemObj.StartReservationSegment; begin end;

procedure ItemObj.ProcessReservedIdentifier; begin end;

procedure ItemObj.FinishReservationSegment; begin end;

procedure ItemObj.FinishReservation; begin end;

procedure ItemObj.StartPrivateDefiniendum; begin end;

procedure ItemObj.FinishLocusType; begin end;

procedure ItemObj.StartPrivateDefiniens; begin end;

procedure ItemObj.FinishPrivateFuncDefinienition; begin end;

procedure ItemObj.FinishPrivatePredDefinienition; begin end;

procedure ItemObj.ProcessLabel; begin end;

procedure ItemObj.StartRegularStatement; begin end;

procedure ItemObj.ProcessDefiniensLabel; begin end;

procedure ItemObj.ProcessSchemeReference; begin end;

procedure ItemObj.StartSchemeReference; begin end;

procedure ItemObj.StartReferences; begin end;

procedure ItemObj.ProcessPrivateReference; begin end;

procedure ItemObj.StartLibraryReferences; begin end;

procedure ItemObj.StartSchemeLibraryReference; begin end;

procedure ItemObj.ProcessDef; begin end;

procedure ItemObj.ProcessSch; begin end;

procedure ItemObj.ProcessTheoremNumber; begin end;

procedure ItemObj.ProcessSchemeNumber; begin end;

procedure ItemObj.FinishTheLibraryReferences; begin end;

procedure ItemObj.FinishSchLibraryReferences; begin end;

procedure ItemObj.FinishReferences; begin end;

procedure ItemObj.FinishSchemeReference; begin end;

procedure ItemObj.StartJustification;  begin end;

procedure ItemObj.FinishJustification;  begin end;

procedure ItemObj.StartSimpleJustification; begin end;

procedure ItemObj.FinishSimpleJustification; begin end;

procedure ItemObj.FinishCompactStatement;  begin end;

procedure ItemObj.StartIterativeStep; begin end;

procedure ItemObj.ProcessIterativeStep; begin end;

procedure ItemObj.FinishIterativeStep; begin end;

procedure ItemObj.CreateExpression(fExpKind:ExpKind);
begin
  gExpPtr:=new(ExpressionPtr, Init(fExpKind));
end;

constructor BlockObj.Init(fBlockKind:BlockKind);
begin
  nBlockKind:=fBlockKind;
  Previous:=gBlockPtr;
end;

procedure BlockObj.Pop;
begin
end;

destructor BlockObj.Done;
begin
  gBlockPtr:=BlockPtr(Previous);
end;

procedure BlockObj.StartProperText; begin end;

procedure BlockObj.ProcessRedefine; begin end;

procedure BlockObj.ProcessLink; begin end;

procedure BlockObj.ProcessBegin; begin end;

procedure BlockObj.ProcessPragma; begin end;

procedure BlockObj.StartAtSignProof; begin end;

procedure BlockObj.FinishAtSignProof; begin end;

procedure BlockObj.FinishDefinition; begin end;

procedure BlockObj.CreateItem(fItemKind:ItemKind);
begin
  gItemPtr:=new(ItemPtr, Init(fItemKind));
end;

procedure BlockObj.CreateBlock(fBlockKind:BlockKind);
begin
  gBlockPtr:=new(BlockPtr, Init(fBlockKind));
end;

procedure BlockObj.StartSchemeDemonstration; begin end;

procedure BlockObj.FinishSchemeDemonstration; begin end;

procedure KillSubexpression;
begin
  if gSubexpPtr = nil then RunTimeError(2144)
  else dispose(gSubexpPtr, Done);
end;

procedure KillExpression;
begin
  if gExpPtr = nil then RunTimeError(2143)
  else dispose(gExpPtr, Done);
end;

procedure KillItem;
begin
  if gItemPtr = nil then RunTimeError(2142)
   else
    begin
     gItemPtr^.Pop;
     dispose(gItemPtr, Done);
    end;
  DisplayLine(CurPos.Line,ErrorNbr);
end;

procedure KillBlock;
begin
  if gBlockPtr = nil then RunTimeError(2141)
   else
    begin
     gBlockPtr^.Pop;
     dispose(gBlockPtr, Done);
    end;
  DisplayLine(CurPos.Line,ErrorNbr);
end;

end.
