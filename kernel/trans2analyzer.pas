(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit trans2analyzer;

interface

uses errhan, mobjects, mscanner, syntax,
     abstract_syntax, parseraddition,
     block_and_item, wsmarticle, first_identification;

{----------------------------------------------------------------}

type

 Transfer2AnalyserItemPtr = ^Transfer2AnalyserItemObj;
 Transfer2AnalyserItemObj =
  object(biItemObj)
   constructor Init(aItemKind:ItemKind);
  end;

 Transfer2AnalyserBlockPtr = ^Transfer2AnalyserBlockObj;
 Transfer2AnalyserBlockObj =
  object(biBlockObj)
    nConstVarNbr: integer;
   constructor Init(aBlockKind:BlockKind);
  end;

 Transfer2AnalyserPtr = ^Transfer2AnalyserObj;
 Transfer2AnalyserObj =
  object(BlockAndItemObj)

   constructor Init(aMSTextProper:WSTextProperPtr);
   destructor Done; virtual;

   procedure Process_StartBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_FinishBlock(aWSBlock:WSBlockPtr); virtual;
   function CreateBlock(fBlockKind:BlockKind): biBlockPtr; virtual;
   function CreateItem(fItemKind: ItemKind): biItemPtr; virtual;

   procedure Process_ItemsContent(aItemKind:ItemKind; const aItemPos:Position; aContent: PObject); virtual;
   procedure Process_FinishItem(aWSItem:WSItemPtr); virtual;

   procedure Process_LibraryReference(aRef: LibraryReferencePtr); virtual;
   procedure Process_SchemeJustification(aInf: SchemeJustificationPtr); virtual;
   procedure Process_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr); virtual;

   procedure Process_Label(aLab:LabelPtr); virtual;
   procedure Process_Proposition(aProp:PropositionPtr); virtual;
   procedure Process_Conditions(aCond: PList); virtual;
   procedure Process_AssumptionConditions(aCond: AssumptionPtr); virtual;
   procedure Process_CompactStatement(aCStm:CompactStatementPtr); virtual;
   procedure Process_DiffuseStatement(aRStm:DiffuseStatementPtr); virtual;
   procedure Process_RegularStatementStart(aRStm:RegularStatementPtr); virtual;
   procedure Process_IterartiveEquality(aRStm:IterativeEqualityPtr); virtual;
   procedure Process_FixedVariablesSegment( var aSegm: QualifiedSegmentPtr); virtual;

  end;

procedure Transfer2Analyzer;

{----------------------------------------------------------------}

type

 Transfer2ExporterPtr = ^Transfer2ExporterObj;
 Transfer2ExporterObj =
  object(Transfer2AnalyserObj)
   constructor Init(aMSTextProper:WSTextProperPtr);
   destructor Done; virtual;

   procedure Process_StartBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_FinishBlock(aWSBlock:WSBlockPtr); virtual;
   procedure Process_ItemsContent(aItemKind:ItemKind; const aItemPos:Position; aContent: PObject); virtual;
   procedure Process_DiffuseStatement(aRStm:DiffuseStatementPtr); virtual;
   procedure Process_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr); virtual;
   procedure Process_Scheme(aWSBlock:WSBlockPtr); virtual;

  end;

procedure Transfer2Exporter;

implementation

uses mizenv, mconsole, pragmas,
     limits, inout, schemhan, justhan,
     lexicon, _formats
{$IFDEF MDEBUG} ,info {$ENDIF};

{----------------------------------------------------------------}

procedure StoreTermList ( aTrmList:PList ); forward;

procedure StoreAdjective(aNeg:boolean; aAttr:AdjectiveExpressionPtr);
 var lFormatNr: integer;
begin
 case aAttr^.nAdjectiveSort of
 wsAdjective:
  begin
   with AdjectivePtr(aAttr)^ do
   begin
    OutFile.OutCharAndPos('.',nAdjectivePos);
    if aNeg then OutFile.OutChar('-') else OutFile.OutChar('+');
    lFormatNr:=gFormatsColl.LookUp_PrefixFormat('V',nAdjectiveSymbol,nArgs^.Count+1);
    OutFile.OutWord(ikAtrPos,lFormatNr);
    StoreTermList( nArgs );
   end;
  end;
 wsNegatedAdjective:
   with NegatedAdjectivePtr(aAttr)^ do
    StoreAdjective( true, nArg );
 end;
end;

procedure StoreAdjectiveList(aCluster: PList);
 var i: integer;
begin
 with aCluster^ do
  for i:=0 to Count-1 do
   begin
    StoreAdjective( false, Items^[i]);
   end;
 OutFile.OutChar(';');
end;

procedure StoreType ( aTyp: abstract_syntax.TypePtr);
 var lFormatNr: integer;
begin
 with aTyp^ do
 begin
  OutFile.OutPos(nTypePos);
  case nTypeSort of
   wsStandardType:
    with StandardTypePtr(aTyp)^ do
    begin
     OutFile.OutChar(ikTypMode);
     lFormatNr:=gFormatsColl.LookUp_PrefixFormat('M',nModeSymbol,nArgs^.Count);
     OutFile.OutInt(lFormatNr);
     StoreTermList( nArgs );
    end;
   wsStructureType:
    with StructTypePtr(aTyp)^ do
    begin
     OutFile.OutChar(ikTypStruct);
     lFormatNr:=gFormatsColl.LookUp_PrefixFormat('L',nStructSymbol,nArgs^.Count);
     OutFile.OutInt(lFormatNr);
     StoreTermList( nArgs );
    end;
   wsClusteredType:
    with ClusteredTypePtr(aTyp)^ do
    begin
     OutFile.OutChar(ikExpAttrTyp);
     StoreAdjectiveList(nAdjectiveCluster);
     StoreType(nType);
    end;
   wsErrorType:
     OutFile.OutChar(ikError);
  end;
 end;
end;

procedure StoreResDesType( aType: MSReservedDscrTypePtr);
 var i: Integer;
begin
 with aType^ do
 begin
  OutFile.OutPos(nTypePos);
  OutFile.OutChar(ikExpResDes);
  OutFile.OutInt(nResTypeNr);
  OutFile.OutWord('I',nIdent);
  for i:=0 to nResSubst.fCount-1 do
   with nResSubst.fList[i] do
    if ((VariableKind(Y1))in [Bound,Constant,DefConstant]) or (Y2 > 0) then
    begin
     OutFile.OutChar('.');
     case VariableKind(Y1) of
      FreeVar, ReservedVar: OutFile.OutChar(ikTrmFreeVar);
      Bound: OutFile.OutChar(ikTrmBound);
      Constant,DefConstant: OutFile.OutChar(ikTrmConstant);
     end;
     OutFile.OutInt(Y2);
    end;
  OutFile.OutChar(';');
 end;
end;

procedure StoreVariableSegment( aSegm: QualifiedSegmentPtr);
 var i: integer;
begin
 case aSegm^.nSegmentSort of
 ikImplQualifiedSegm:
  with MSImplicitlyQualifiedSegmentPtr(aSegm)^ do
  begin
   OutFile.OutWord('N',1);
   OutFile.OutWord('I',nIdentifier^.nIdent);
   StoreResDesType(nResType);
  end;
 ikExplQualifiedSegm:
  with ExplicitlyQualifiedSegmentPtr(aSegm)^ do
  begin
   OutFile.OutWord('N',nIdentifiers^.Count);
   for i:=0 to nIdentifiers^.Count-1 do
     OutFile.OutWord('I',VariablePtr(nIdentifiers^.Items^[i])^.nIdent);
   StoreType(nType);
  end;
 end;
end;

procedure StoreTerm ( aTrm: TermPtr ); forward;

procedure StoreTermListElements( aTrmList:PList );
 var i: integer;
begin
 for i:=0 to aTrmList^.Count-1 do
  begin
   OutFile.OutChar('.');
   StoreTerm(aTrmList^.Items^[i]);
  end;
end;

procedure StoreTermList ( aTrmList:PList );
begin
 StoreTermListElements(aTrmList);
 OutFile.OutChar(';');
end;

procedure StoreFormula ( aFrm: FormulaPtr );
 var lFormatNr,i,lLeftArgsCount: integer;
     lNeg: boolean;
     lFrm: FormulaPtr;
begin
 with aFrm^ do
 begin
   OutFile.OutPos(nFormulaPos);
   case nFormulaSort of
    wsNegatedFormula:
     if abstract_syntax.NegativeFormulaPtr(aFrm)^.nArg^.nFormulaSort  = wsContradiction then
       OutFile.OutChar(ikFrmVerum)
     else
      begin
       OutFile.OutChar(ikFrmNeg);
       StoreFormula( abstract_syntax.NegativeFormulaPtr(aFrm)^.nArg);
      end;
    wsConjunctiveFormula:
     with abstract_syntax.BinaryFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmConj);
       StoreFormula(nLeftArg);
       StoreFormula(nRightArg);
      end;
    wsDisjunctiveFormula:
     with abstract_syntax.BinaryFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmOr);
       StoreFormula(nLeftArg);
       StoreFormula(nRightArg);
      end;
    wsConditionalFormula:
     with abstract_syntax.BinaryFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmImplies);
       StoreFormula(nLeftArg);
       StoreFormula(nRightArg);
      end;
    wsBiconditionalFormula:
     with abstract_syntax.BinaryFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmIff);
       StoreFormula(nLeftArg);
       StoreFormula(nRightArg);
      end;
    wsFlexaryConjunctiveFormula:
     with abstract_syntax.BinaryFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmFlexConj);
       StoreFormula(nLeftArg);
       StoreFormula(nRightArg);
      end;
    wsFlexaryDisjunctiveFormula:
     with abstract_syntax.BinaryFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmFlexDisj);
       StoreFormula(nLeftArg);
       StoreFormula(nRightArg);
      end;
    wsPredicativeFormula:
     with PredicativeFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmPred);
       lFormatNr:=gFormatsColl.LookUp_PredFormat(nPredNr,nLeftArgs^.Count,nRightArgs^.Count);
       OutFile.OutInt(lFormatNr);
       StoreTermListElements( nLeftArgs);
       StoreTermList( nRightArgs);
      end;
    wsMultiPredicativeFormula:
     with MultiPredicativeFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikMultFrmPred);
       lFrm:=nScraps.Items^[0];
       StoreFormula(lFrm);
       lNeg:=lFrm^.nFormulaSort = wsNegatedFormula;
       if lNeg then
        lFrm:=NegativeFormulaPtr(lFrm)^.nArg;
       lLeftArgsCount:=PredicativeFormulaPtr(lFrm)^.nRightArgs^.Count;
       OutFile.OutChar('L');
       OutFile.OutInt(lLeftArgsCount);
       for i:=1 to nScraps.Count - 1 do
        begin
         OutFile.OutChar('.');
         lFrm:=nScraps.Items^[i];
         lNeg:=lFrm^.nFormulaSort = wsNegatedFormula;
         if lNeg then
          with NegativeFormulaPtr(nScraps.Items^[i])^ do
           begin
            OutFile.OutPos(nFormulaPos);
            OutFile.OutChar(ikFrmNeg);
            lFrm:=NegativeFormulaPtr(lFrm)^.nArg;
           end;
         with RightSideOfPredicativeFormulaPtr(lFrm)^ do
          begin
           OutFile.OutPos(nFormulaPos);
           OutFile.OutChar(ikRSFrmPred);
           lFormatNr:=gFormatsColl.LookUp_PredFormat(nPredNr,lLeftArgsCount,nRightArgs^.Count);
           OutFile.OutInt(lFormatNr);
           StoreTermList( nRightArgs);
           lLeftArgsCount:=RightSideOfPredicativeFormulaPtr(lFrm)^.nRightArgs^.Count;
          end;
        end;
       OutFile.OutChar(';');
      end;
    wsPrivatePredicateFormula:
     with MSPrivatePredicativeFormulaPtr(aFrm)^ do
      begin
       if nPredKind = SchematicPred then
         OutFile.OutChar(ikFrmSchPred)
       else OutFile.OutChar(ikFrmPrivPred);
       OutFile.OutInt(nPredNr);
       StoreTermList( nArgs);
      end;
    wsAttributiveFormula:
     with abstract_syntax.AttributiveFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmAttr);
       StoreTerm(nSubject);
       StoreAdjectiveList(nAdjectives);
      end;
    wsQualifyingFormula:
     with abstract_syntax.QualifyingFormulaPtr(aFrm)^ do
      begin
       OutFile.OutChar(ikFrmQual);
       StoreTerm(nSubject);
       StoreType(nType);
      end;
    wsUniversalFormula:
     with abstract_syntax.QuantifiedFormulaPtr( aFrm)^ do
      begin
       OutFile.OutChar(ikFrmUniv);
       StoreVariableSegment(nSegment);
       StoreFormula(nScope);
      end;
    wsExistentialFormula:
     with abstract_syntax.QuantifiedFormulaPtr( aFrm)^ do
      begin
       OutFile.OutChar(ikFrmEx);
       StoreVariableSegment(nSegment);
       StoreFormula(nScope);
      end;
    wsContradiction:
      begin
       OutFile.OutChar(ikFrmNeg);
       OutFile.OutPos(nFormulaPos);
       OutFile.OutChar(ikFrmVerum);
      end;
     wsThesis:
      OutFile.OutChar(ikFrmThesis);
    wsErrorFormula:
      OutFile.OutChar(ikError);
   end;
 end;
end;

procedure StoreTerm ( aTrm: TermPtr );
 var i,lFormatNr: integer;
begin
  with aTrm^ do
  case nTermSort of
   wsPlaceholderTerm:
    begin
     OutFile.OutChar(ikTrmLocus);
     OutFile.OutInt(PlaceholderTermPtr(aTrm)^.nLocusNr);
    end;
   wsSimpleTerm:
    with MSSimpleTermPtr(aTrm)^ do
    begin
     case nVarKind of
      FreeVar: OutFile.OutChar(ikTrmLocus);
      ReservedVar: OutFile.OutChar(ikTrmFreeVar);
      Bound: OutFile.OutChar(ikTrmBound);
      Constant,DefConstant: OutFile.OutChar(ikTrmConstant);
     end;
     OutFile.OutInt(nVarNr);
    end;
   wsNumeralTerm:
    begin                 ;
      OutFile.OutChar(ikTrmNumeral);
      OutFile.OutWord(ikTrmNumeral,abstract_syntax.NumeralTermPtr(aTrm)^.nValue);
    end;
   wsInfixTerm:
    with InfixTermPtr(aTrm)^ do
    begin
      OutFile.OutChar(ikTrmFunctor);
      lFormatNr:=gFormatsColl.LookUp_FuncFormat(nFunctorSymbol,nLeftArgs^.Count,nRightArgs^.Count);
      OutFile.OutInt(lFormatNr);
      OutFile.OutPos(nTermPos);
      StoreTermListElements(nLeftArgs);
      StoreTermList( nRightArgs);
    end;
   wsCircumfixTerm:
    with CircumfixTermPtr(aTrm)^ do
    begin
      OutFile.OutChar(ikTrmFunctor);
      lFormatNr:=gFormatsColl.LookUp_BracketFormat(nLeftBracketSymbol,nRightBracketSymbol,nArgs^.Count,0,0);
      OutFile.OutInt(lFormatNr);
      OutFile.OutPos(nTermPos);
      StoreTermList( nArgs);
    end;
   wsPrivateFunctorTerm:
    with MSPrivateFunctorTermPtr(aTrm)^ do
     begin
       if nFuncKind = SchematicFunc then
        OutFile.OutChar(ikTrmSchFunc)
       else OutFile.OutChar(ikTrmPrivFunc);
       OutFile.OutInt(nFuncNr);
       OutFile.OutPos(nTermPos);
       StoreTermList( nArgs);
     end;
   wsAggregateTerm:
    with AggregateTermPtr(aTrm)^ do
    begin
      OutFile.OutChar(ikTrmAggreg);
      lFormatNr:=gFormatsColl.LookUp_PrefixFormat('G',nStructSymbol,nArgs^.Count);
      OutFile.OutInt(lFormatNr);
      OutFile.OutPos(nTermPos);
      StoreTermList( nArgs);
    end;
   wsSelectorTerm:
    with abstract_syntax.SelectorTermPtr(aTrm)^ do
    begin
      OutFile.OutChar(ikTrmSelector);
      lFormatNr:=gFormatsColl.LookUp_PrefixFormat('U',nSelectorSymbol,1);
      OutFile.OutInt(lFormatNr);
      OutFile.OutPos(nTermPos);
      StoreTerm( nArg);
    end;
   wsInternalSelectorTerm:
    with MSInternalSelectorTermPtr(aTrm)^ do
    begin
      OutFile.OutChar(ikTrmConstant);
      OutFile.OutInt(nVarNr);
    end;
   wsForgetfulFunctorTerm:
    with ForgetfulFunctorTermPtr(aTrm)^ do
    begin
      OutFile.OutChar(ikTrmSubAgreg);
      lFormatNr:=gFormatsColl.LookUp_PrefixFormat('J',nStructSymbol,1);
      OutFile.OutInt(lFormatNr);
      OutFile.OutPos(nTermPos);
      StoreTerm( nArg);
    end;
   wsInternalForgetfulFunctorTerm:
    with InternalForgetfulFunctorTermPtr(aTrm)^ do
    begin
//      OutFile.OutChar(ExpSort);
//      StoreXIntAttr( XMLAttrName[atNr], nStructSymbol);
    end;
   wsSimpleFraenkelTerm,wsFraenkelTerm:
    with abstract_syntax.FraenkelTermPtr(aTrm)^ do
     begin
      OutFile.OutChar(ikTrmFraenkel);
      OutFile.OutPos(nTermPos);
      for i := 0 to nPostqualification^.Count - 1 do
       StoreVariableSegment(nPostqualification^.Items^[i]);
      OutFile.OutChar(';');
      StoreTerm(nSample);
      if nTermSort = wsSimpleFraenkelTerm then
       begin
        OutFile.OutPos(nTermPos);
        OutFile.OutChar(ikFrmVerum);
       end
      else StoreFormula(nFormula);
     end;
   wsQualificationTerm:
    with abstract_syntax.QualifiedTermPtr(aTrm)^ do
     begin
      OutFile.OutChar(ikTrmQua);
      StoreTerm(nSubject);
      StoreType(nQualification);
      OutFile.OutPos(nTermPos);
     end;
   wsExactlyTerm:
    with ExactlyTermPtr(aTrm)^ do
     begin
      OutFile.OutChar(ikTrmExactly);
      StoreTerm(nSubject);
      OutFile.OutPos(nTermPos);
     end;
   wsGlobalChoiceTerm:
     begin
      OutFile.OutChar(ikTrmChoice);
      OutFile.OutPos(nTermPos);
      StoreType(abstract_syntax.ChoiceTermPtr(aTrm)^.nChoiceType);
     end;
   wsItTerm:
     OutFile.OutChar(ikTrmIt);
   wsErrorTerm:
     OutFile.OutChar(ikError);
  end;
end;

procedure StoreTypeList ( aTypeList: PList );
 var i: integer;
begin
 for i:=0 to aTypeList^.Count-1 do
  begin
    OutFile.OutChar('.');
    StoreType(aTypeList^.Items^[i]);
  end;
 OutFile.OutChar(';');
end;

const InfFirstLetter: array[InferenceKind]  of char = ('?','''','"','/','i');

procedure StoreInference(aInf: JustificationPtr);
 var i,lStart: integer;
     lRef: ReferencePtr;
begin
 lStart:=0;
 with aInf^ do
 begin
  OutFile.OutChar('"');
  OutFile.OutChar(InfFirstLetter[nInfSort]); { ikInfBy lub ikInfFrom lub ikError }
  case nInfSort of
  infSchemeJustification:
   begin
     OutFile.OutInt(SchemeJustificationPtr(aInf)^.nSchFileNr);
     if SchemeJustificationPtr(aInf)^.nSchFileNr = 0 then
      OutFile.OutInt(MSSchemeJustificationPtr(aInf)^.nSchemeNr)
     else
      OutFile.OutInt(SchemeJustificationPtr(aInf)^.nSchemeIdNr);
     OutFile.OutBool(false);
   end;
  infStraightforwardJustification:
   begin
     OutFile.OutInt(0);
     OutFile.OutInt(0);
    if StraightforwardJustificationPtr(aInf)^.nLinked then
     lStart:=1;
    OutFile.OutBool(StraightforwardJustificationPtr(aInf)^.nLinked)
   end;
  infError,infSkippedProof:
   begin
     OutFile.OutInt(0);
     OutFile.OutInt(0);
     OutFile.OutBool(false);
   end;
  end;
  if not (nInfSort in [infError,infSkippedProof]) then
   with SimpleJustificationPtr(aInf)^.nReferences^ do
    for i:=lStart to Count-1 do
     begin lRef:=ReferencePtr(Items^[i]);
      case lRef^.nRefSort of
      LocalReference:
       begin OutFile.OutChar('l');
        with MSLocalReferencePtr(lRef)^ do
         begin
          if nRefNr = 0 then OutFile.OutInt(0)
           else OutFile.OutInt(nLabNr);
          OutFile.OutInt(nLabId);
          OutFile.OutPos(nRefPos)
         end;
       end;
      TheoremReference:
       begin OutFile.OutChar('t');
        with TheoremReferencePtr(lRef)^ do
         begin OutFile.OutInt(nArticleNr);
          OutFile.OutInt(nTheoNr);
          OutFile.OutPos(nRefPos);
         end;
       end;
      DefinitionReference:
       begin OutFile.OutChar('d');
        with DefinitionReferencePtr(lRef)^do
         begin OutFile.OutInt(nArticleNr);
          OutFile.OutInt(nDefNr);
          OutFile.OutPos(nRefPos);
         end;
       end
      end;
     end;
  OutFile.OutChar(';');
  OutFile.OutPos(nInfPos);
 end;
end;

procedure StoreFormat(aPatt:PatternPtr);
 var lFormatNr,k: integer;
begin
  case aPatt^.nPatternSort of
   itDefPred:
    with PredicatePatternPtr(aPatt)^ do
    begin
     lFormatNr:=gFormatsColl.CollectPredForm(nPredSymbol,nLeftArgs.Count,nRightArgs.Count);
     OutFile.OutWordAndPos('R',lFormatNr,nPatternPos);
     for k:=0 to nLeftArgs^.Count-1 do
      OutFile.OutWord(ikTrmLocus,MSLocusPtr(nLeftArgs^.Items^[k])^.nVarNr);
     for k:=0 to nRightArgs^.Count-1 do
      OutFile.OutWord(ikTrmLocus,MSLocusPtr(nRightArgs^.Items^[k])^.nVarNr);
     OutFile.OutChar(';');
    end;
   itDefFunc:
    with FunctorPatternPtr(aPatt)^ do
     begin
      case nFunctKind of
       InfixFunctor:
        with InfixFunctorPatternPtr(aPatt)^ do
        begin
         lFormatNr:=gFormatsColl.LookUp_FuncFormat(nOperSymb,nLeftArgs.Count,nRightArgs.Count);
         OutFile.OutWordAndPos('K',lFormatNr,nPatternPos);
         for k:=0 to nLeftArgs^.Count-1 do
          OutFile.OutWord(ikTrmLocus,MSLocusPtr(nLeftArgs^.Items^[k])^.nVarNr);
         for k:=0 to nRightArgs^.Count-1 do
          OutFile.OutWord(ikTrmLocus,MSLocusPtr(nRightArgs^.Items^[k])^.nVarNr);
        end;
       CircumfixFunctor:
        with CircumfixFunctorPatternPtr(aPatt)^ do
        begin
         lFormatNr:=gFormatsColl.LookUp_BracketFormat(nLeftBracketSymb,nRightBracketSymb,nArgs^.Count,0,0);
         OutFile.OutWordAndPos('K',lFormatNr,nPatternPos);
         for k:=0 to nArgs^.Count-1 do
          OutFile.OutWord(ikTrmLocus,MSLocusPtr(nArgs^.Items^[k])^.nVarNr);
        end;
      end;
      OutFile.OutChar(';');
     end;
   itDefMode:
    with ModePatternPtr(aPatt)^ do
     begin
      lFormatNr:=gFormatsColl.LookUp_PrefixFormat('M',nModeSymbol,nArgs^.Count);
      OutFile.OutWordAndPos('M',lFormatNr,nPatternPos);
      for k:=0 to nArgs^.Count-1 do
       OutFile.OutWord(ikTrmLocus,MSLocusPtr(nArgs^.Items^[k])^.nVarNr);
      OutFile.OutChar(';');
     end;
   itDefAttr:
    with AttributePatternPtr(aPatt)^ do
     begin
      lFormatNr:=gFormatsColl.LookUp_PrefixFormat('V',nAttrSymbol,nArgs^.Count+1);
      OutFile.OutWordAndPos('V',lFormatNr,nPatternPos);
      for k:=0 to nArgs^.Count-1 do
       OutFile.OutWord(ikTrmLocus,MSLocusPtr(nArgs^.Items^[k])^.nVarNr);
      OutFile.OutWord(ikTrmLocus,MSLocusPtr(nArg)^.nVarNr);
      OutFile.OutChar(';');
     end;
   itDefStruct:
    with ModePatternPtr(aPatt)^ do
     begin
      lFormatNr:=gFormatsColl.LookUp_PrefixFormat('L',nModeSymbol,nArgs^.Count);
      OutFile.OutWordAndPos('L',lFormatNr,nPatternPos);
      for k:=0 to nArgs^.Count-1 do
       OutFile.OutWord(ikTrmLocus,MSLocusPtr(nArgs^.Items^[k])^.nVarNr);
      OutFile.OutChar(';');
     end;
  end;
end;

procedure StoreDefiniens(aDef:DefiniensPtr; aDefiniensPresent:char);
 var i: integer;
begin
  if aDef <> nil then
   with DefiniensPtr(aDef)^ do
   begin
    OutFile.OutChar(aDefiniensPresent);  // 'm" or 'e'
    OutFile.OutWord('E',MSLabelPtr(nDefLabel)^.nLabelNr);
    OutFile.OutInt(MSLabelPtr(nDefLabel)^.nLabelIdNr);
    OutFile.OutPos(MSLabelPtr(nDefLabel)^.nLabelPos);
    case nDefSort of
    SimpleDefiniens:
     with SimpleDefiniensPtr(aDef)^,nExpression^ do
      begin
       case nExprKind of
        exTerm:
         begin
          OutFile.OutChar('e');
          OutFile.OutChar(';');
          OutFile.OutChar('o');
          StoreTerm(TermPtr(nExpr));
         end;
        exFormula:
         begin
          OutFile.OutChar('m');
          OutFile.OutChar(';');
          OutFile.OutChar('o');
          StoreFormula(FormulaPtr(nExpr));
         end;
       end;
      end;
    ConditionalDefiniens:
     with ConditionalDefiniensPtr(aDef)^ do
      begin
       if PartDefPtr(nConditionalDefiniensList^.Items^[0])^.nPartDefiniens^.nExprKind = exTerm
        then OutFile.OutChar('e')
        else OutFile.OutChar('m');
       for i:=0 to nConditionalDefiniensList^.Count-1 do
        with PartDefPtr(nConditionalDefiniensList^.Items^[I])^ do
         begin
          OutFile.OutChar('.');
          with nPartDefiniens^ do
           begin
            case nExprKind of
             exTerm: StoreTerm(TermPtr(nExpr));
             exFormula: StoreFormula(FormulaPtr(nExpr));
            end;
      	   end;
           StoreFormula(nGuard);
         end;
       OutFile.OutChar(';');
       if nOtherwise <> nil then
        with nOtherwise^ do
         begin
          OutFile.OutChar('o');
          case nExprKind of
           exTerm: StoreTerm(TermPtr(nExpr));
           exFormula: StoreFormula(FormulaPtr(nExpr));
          end;
         end
       else OutFile.OutChar('n');
      end;
    end;
   end;
end;

{----------------------------------------------------------------}

type
 CheckClusterTermPtr =  ^CheckClusterTermObj;
 CheckClusterTermObj =
  object(WithinExprObj)

   constructor Init(aExpKind:ExpKind);
   destructor Done; virtual;
   function CreateExpressionsVariableLevel: biStackedPtr; virtual;
   procedure Process_Term ( var aTrm: TermPtr ); virtual;
  end;

{----------------------------------------------------------------}

constructor CheckClusterTermObj.Init(aExpKind:ExpKind);
begin
 inherited Init(aExpKind);
end;

destructor CheckClusterTermObj.Done;
begin
 inherited Done;
end;

function CheckClusterTermObj.CreateExpressionsVariableLevel: biStackedPtr;
begin
 result:=new(biStackedPtr, Init);
end;

procedure CheckClusterTermObj.Process_Term ( var aTrm: TermPtr );
begin
 case aTrm^.nTermSort of
  wsGlobalChoiceTerm:
   begin
    Error(aTrm^.nTermPos,224);
    aTrm^.nTermSort:=wsErrorTerm;
   end;
  wsFraenkelTerm:
   begin
    Error(aTrm^.nTermPos,225);
    aTrm^.nTermSort:=wsErrorTerm;
   end;
 end;
 inherited Process_Term (aTrm);
end;
{----------------------------------------------------------------}

var gConstVarNbr: integer;

constructor Transfer2AnalyserItemObj.Init(aItemKind:ItemKind);
begin inherited Init(aItemKind);
end;

constructor Transfer2AnalyserBlockObj.Init(aBlockKind:BlockKind);
begin inherited Init(aBlockKind);
end;

{----------------------------------------------------------------}

constructor Transfer2AnalyserObj.Init(aMSTextProper:WSTextProperPtr);
begin
 inherited Init(aMSTextProper);
 CurPos.Line:=1;
 CurPos.Col:=1;

 TheoReferNbr.Init(0);
 TheoReferNbr.Assign(0,0,0);
 DefReferNbr.Init(0);
 DefReferNbr.Assign(0,0,0);
 SchReferNbr.Init(0);
 SchReferNbr.Assign(0,0,0);

 gConstVarNbr:=0;

 FileExam(EnvFileName+'.frx');
 gFormatsColl.LoadFormats(EnvFileName+'.frx');
 OutFile.OpenFile(MizFileName+'.par');
end;

destructor Transfer2AnalyserObj.Done;
 var i:integer;
begin
 OutFile.OutChar('!');
 OutFile.Done;

 CompletePragmas(CurPos.Line);

 OutFile.OpenFile(MizFileName+'.ref');
 for i:=0 to SchReferNbr.fCount-1 do
  with SchReferNbr.fList^[i] do
  begin
   OutFile.OutChar('.');
   OutFile.OutInt(X1);
   OutFile.OutInt(X2);
   OutFile.OutInt(Y);
   OutFile.OutNewLine;
  end;
 OutFile.OutChar(';');
 SchReferNbr.Done;
 for i:=1 to TheoReferNbr.fCount-1 do
  with TheoReferNbr.fList^[i] do
  begin
   OutFile.OutChar('.');
   OutFile.OutInt(X1);
   OutFile.OutInt(X2);
   OutFile.OutInt(Y);
   OutFile.OutNewLine;
  end;
 OutFile.OutChar(';');
 TheoReferNbr.Done;
 for i:=1 to DefReferNbr.fCount-1 do
  with DefReferNbr.fList^[i] do
  begin
   OutFile.OutChar('.');
   OutFile.OutInt(X1);
   OutFile.OutInt(X2);
   OutFile.OutInt(Y);
   OutFile.OutNewLine;
  end;
 OutFile.OutChar(';');
 DefReferNbr.Done;
 OutFile.Done;
 inherited Done;
end;

function Transfer2AnalyserObj.CreateBlock(fBlockKind:BlockKind): biBlockPtr;
begin
 result:=new(Transfer2AnalyserBlockPtr,Init(fBlockKind));
end;

function Transfer2AnalyserObj.CreateItem(fItemKind:ItemKind): biItemPtr;
begin
 result:=new(Transfer2AnalyserItemPtr, Init(fItemKind));
end;

procedure Transfer2AnalyserObj.Process_Label(aLab:LabelPtr);
begin
 if aLab <> nil  then
  with MSLabelPtr(alab)^ do
  begin
   OutFile.OutWord('E',nLabelNr);
   OutFile.OutInt(nLabelIdNr);
   OutFile.OutPos(nLabelPos);
  end;
end;

procedure Transfer2AnalyserObj.Process_Proposition(aProp:wsmarticle.PropositionPtr);
begin
 Process_Label(aProp^.nLab);
 OutFile.OutChar(ikMscPrefix);
 StoreFormula(aProp^.nSentence)
end;

procedure Transfer2AnalyserObj.Process_Conditions(aCond: PList);
 var i: integer;
begin
 for i:=0 to aCond^.Count-1 do
   Process_Proposition(aCond^.Items^[i]);
 OutFile.OutChar(';');
end;

procedure Transfer2AnalyserObj.Process_AssumptionConditions(aCond: AssumptionPtr);
begin
 case aCond^.nAssumptionSort of
 SingleAssumption:
   begin
    Process_Proposition(SingleAssumptionPtr(aCond)^.nProp);
    OutFile.OutChar(';');
   end;
 CollectiveAssumption:
   Process_Conditions(CollectiveAssumptionPtr(aCond)^.nConditions);
 end;
end;

procedure Transfer2AnalyserObj.Process_CompactStatement(aCStm:CompactStatementPtr);
begin
  with aCStm^ do
  begin
   Process_Label(nProp^.nLab);
   OutFile.OutChar(ikMscPrefix);
   StoreFormula(nProp^.nSentence)
  end;
end;

procedure Transfer2AnalyserObj.Process_DiffuseStatement(aRStm:DiffuseStatementPtr);
begin
  Process_Label(aRStm^.nLab);
end;

procedure Transfer2AnalyserObj.Process_RegularStatementStart(aRStm:RegularStatementPtr);
begin
 case aRStm^.nStatementSort of
 stDiffuseStatement:
   Process_DiffuseStatement(DiffuseStatementPtr(aRStm));
 stCompactStatement:
   Process_CompactStatement(CompactStatementPtr(aRStm));
 stIterativeEquality:
   Process_CompactStatement(CompactStatementPtr(aRStm));
 end;
end;

procedure Transfer2AnalyserObj.Process_IterartiveEquality(aRStm:IterativeEqualityPtr);
 var i: integer;
begin
 with IterativeEqualityPtr(aRSTm)^ do
  for i := 0 to nIterSteps^.Count - 1 do
   with IterativeStepPtr(nIterSteps^.Items^[i])^ do
   begin
    OutFile.OutCharAndPos(ikItmIterEquality,nIterPos);
    StoreTerm(nTerm);
    Process_Justification(JustificationPtr(nJustification),nil);
   end;
end;


procedure Transfer2AnalyserObj.Process_FixedVariablesSegment( var aSegm: QualifiedSegmentPtr);
 var i: integer;
begin
 case aSegm^.nSegmentSort of
 ikImplQualifiedSegm:
  with MSImplicitlyQualifiedSegmentPtr(aSegm)^ do
   begin
    inc(gConstVarNbr);
    OutFile.OutWord('Q',1);
    OutFile.OutWord('I',nIdentifier^.nIdent);
    StoreResDesType(nResType);
   end;
 ikExplQualifiedSegm:
  with ExplicitlyQualifiedSegmentPtr(aSegm)^ do
   begin
    OutFile.OutWord('Q',nIdentifiers.Count);
    for i:=0 to nIdentifiers.Count-1 do
     begin
      inc(gConstVarNbr);
      OutFile.OutWord('I',VariablePtr(nIdentifiers.Items^[i])^.nIdent);
     end;
    StoreType(nType);
   end;
 end;
end;

procedure Transfer2AnalyserObj.Process_ItemsContent(aItemKind:ItemKind; const aItemPos:Position; aContent: PObject);
 var i,j,k,lNbr,lFormatNr,lFieldsNbr: integer;
     lCTrm: CheckClusterTermPtr;
     lKind: char;
begin
 if aContent <> nil then
 case aItemKind of
  itDefinition: ;
  itSchemeBlock: ;
  itSchemeHead:
   with MSSchemePtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikBlcScheme,CurPos);
    OutFile.OutWord('S',nSchemeNr);
    OutFile.OutInt(nSchemeIdNr);
    OutFile.OutPos(CurPos);

    for j:=0 to nSchemeParams^.Count-1 do
    begin
     case SchemeSegmentPtr(nSchemeParams^.Items^[j])^.nSegmSort of
      PredicateSegment:
       with PredicateSegmentPtr(nSchemeParams^.Items^[j])^ do
       begin
        for i:=0 to nVars^.Count-1 do
          OutFile.OutWord(ikFrmSchPred,VariablePtr(nVars^.Items^[i])^.nIdent);
        OutFile.OutChar(';');
        StoreTypeList(nTypeExpList);
       end;
      FunctorSegment:
       with FunctorSegmentPtr(nSchemeParams^.Items^[j])^ do
       begin
        for i:=0 to nVars.Count-1 do
          OutFile.OutWord(ikTrmSchFunc,VariablePtr(nVars^.Items^[i])^.nIdent);
        OutFile.OutChar(';');
        StoreTypeList(nTypeExpList);
        OutFile.OutChar(ikMscSpecification);
        StoreType(nSpecification);
       end;
     end;
    end;
    OutFile.OutChar(';');
    OutFile.OutChar(ikMscPrefix);
    StoreFormula(nSchemeConclusion);
    Process_Conditions(nSchemePremises);
   end;
  itTheorem:
   with CompactStatementPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmTheorem,CurPos);
    Process_CompactStatement(CompactStatementPtr(aContent));
   end;
  itAxiom:
   begin
    OutFile.OutChar(ikItmTheorem)
   end;
  itReservation:
   with MSReservationSegmentPtr(aContent)^ do
   begin
    OutFile.OutChar(ikItmReservation);
    for i := 0 to nIdentifiers^.Count - 1 do
     OutFile.OutWord('I',VariablePtr(nIdentifiers^.Items^[i])^.nIdent);
    OutFile.OutChar(';');
    for i:=0 to nFreeVars.Count-1 do
      begin OutFile.OutChar('.');
       StoreResDesType(nFreeVars.Items^[i]);
      end;
    OutFile.OutChar(';');
    StoreType(nResType);
   end;
  itRegularStatement:
    Process_RegularStatementStart(RegularStatementPtr(aContent));
  itChoice:
   with ChoiceStatementPtr(aContent)^ do
   begin
    OutFile.OutChar(ikItmChoice);
    for i:= 0 to  nQualVars^.Count-1 do
     begin
      Process_FixedVariablesSegment( QualifiedSegmentPtr(nQualVars^.Items^[i]));
      OutFile.OutNewLine;
     end;
    Process_Conditions(nConditions);
   end;
  itReconsider:
   with TypeChangingStatementPtr(aContent)^ do
   begin
    OutFile.OutChar(ikItmReconsidering);
    for i:=0 to nTypeChangeList.Count-1 do
     begin
      with TypeChangePtr(nTypeChangeList.Items^[i])^ do
      case nTypeChangeKind of
       Equating:
        begin
         inc(gConstVarNbr);
         OutFile.OutWord('I',nVar^.nIdent);
         OutFile.OutChar('.');
         StoreTerm(nTermExpr)
        end;
       VariableIdentifier:
        begin
         inc(gConstVarNbr);
         OutFile.OutWord('I',nVar^.nIdent);
         OutFile.OutChar('.');
         StoreTerm(nTermExpr)
        end;
      end;
     end;
    OutFile.OutChar(';');
    OutFile.OutChar(ikMscAs);
    StoreType(nTypeExpr);
   end;
  itPrivFuncDefinition:
  with PrivateFunctorDefinitionPtr(aContent)^ do
   begin
    if nTypeExpList^.Count > MaxArgNbr then
     begin
//      Error(nFuncId^.nVarPos,180);
//      nTypeExpList^.Items^[nTypeExpList^.Count-1]:=new(IncorrectTypePtr,Init(TypePtr(nTypeExpList^.Items^[nTypeExpList^.Count-1])^.nTypePos));
//?     gTypeList.Insert(new(InCorrTypePtr,Init))
     end;
    OutFile.OutChar(ikItmPrivFunc);
    OutFile.OutWord(ikTrmPrivFunc,nFuncId^.nIdent);
    StoreTypeList(nTypeExpList);
    StoreTerm(nTermExpr);
   end;
  itPrivPredDefinition:
   with PrivatePredicateDefinitionPtr(aContent)^ do
   begin
    if nTypeExpList^.Count > MaxArgNbr then
     begin
//      Error(nPredId^.nVarPos,180);
//      nTypeExpList^.Items^[nTypeExpList^.Count-1]:=new(IncorrectTypePtr,Init(TypePtr(nTypeExpList^.Items^[nTypeExpList^.Count-1])^.nTypePos));
//?     gTypeList.Insert(new(InCorrTypePtr,Init))
     end;
    OutFile.OutChar(ikItmPrivPred);
    OutFile.OutWord(ikFrmPrivPred,nPredId^.nIdent);
    StoreTypeList(nTypeExpList);
    StoreFormula(nSentence);
   end;
  itConstantDefinition:
   with ConstantDefinitionPtr(aContent)^ do
   begin
    inc(gConstVarNbr);
    OutFile.OutChar(ikItmPrivConstant);
    OutFile.OutWord('I',nVarId^.nIdent);
    StoreTerm(nTermExpr);
   end;
  itLociDeclaration:
    with QualifiedSegmentPtr(aContent)^ do
    begin
     OutFile.OutCharAndPos(ikItmGeneralization,nSegmPos);
     Process_FixedVariablesSegment( QualifiedSegmentPtr(aContent));
    end;
  itGeneralization:
    with QualifiedSegmentPtr(aContent)^ do
    begin
      OutFile.OutCharAndPos(ikItmGeneralization,nSegmPos);
      Process_FixedVariablesSegment( QualifiedSegmentPtr(aContent));
    end;
  itAssumption:
    with AssumptionPtr(aContent)^ do
    begin
     OutFile.OutCharAndPos(ikItmAssumption,CurPos);
     Process_AssumptionConditions(AssumptionPtr(aContent));
    end;
  itExistentialAssumption:
   with ExistentialAssumptionPtr(aContent)^ do
    begin
     OutFile.OutCharAndPos(ikItmExAssumption,CurPos);
     for i := 0 to  nQVars^.Count-1 do
       Process_FixedVariablesSegment( QualifiedSegmentPtr(nQVars^.Items^[i]));
     Process_Conditions(nConditions);
    end;
  itExemplification:
   with ExamplePtr(aContent)^ do
   begin
    if nVarId <> nil then
     begin
      inc(gConstVarNbr);
      OutFile.OutCharAndPos(ikItmExemplifWithEq,CurPos);
      OutFile.OutWord('I',nVarId^.nIdent);
     end
    else
     begin
       OutFile.OutCharAndPos(ikItmSimpleExemplif,CurPos);
     end;
    StoreTerm(nTermExpr);
   end;
  itPerCases:
   with JustificationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikBlcPerCases,CurPos);
   end;
  itConclusion:
   with RegularStatementPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmConclusion,CurPos);
    Process_RegularStatementStart(RegularStatementPtr(aContent));
   end;
  itCaseBlock: ;
  itCaseHead,
  itSupposeHead:
   with AssumptionPtr(aContent)^ do
   begin
    Process_AssumptionConditions(AssumptionPtr(aContent));
   end;
  itCorrCond:
   with CorrectnessConditionPtr(aContent)^ do
   begin
    OutFile.OutWordAndPos('Y',ord(nCorrCondSort),CurPos);
   end;
  itCorrectness:
   with CorrectnessPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmCorrectness,CurPos);
   end;
  itProperty:
   begin
    OutFile.OutWordAndPos(ikTrmFreeVar,ord(PropertyPtr(aContent)^.nPropertySort),CurPos);
   end;
  itDefMode:
   with ModeDefinitionPtr(aContent)^ do
   case nDefKind of
    defExpandableMode:
     begin
      OutFile.OutCharAndPos(ikItmDefExpandMode,aItemPos);
      StoreFormat(nDefModePattern);
      StoreType(ExpandableModeDefinitionPtr(aContent)^.nExpansion);
     end;
    defStandardMode:
     with StandardModeDefinitionPtr(aContent)^ do
     begin
      if nRedefinition then
        OutFile.OutCharAndPos(ikItmRedefMode,aItemPos)
       else OutFile.OutCharAndPos(ikItmDefMode,aItemPos);
      StoreFormat(nDefModePattern);
      if nSpecification <> nil then
       begin
        OutFile.OutChar(ikMscSpecification);
        StoreType(nSpecification);
       end;
      StoreDefiniens(nDefiniens,'m');
     end;
   end;
  itDefAttr:
   with AttributeDefinitionPtr(aContent)^ do
   begin
    if nRedefinition then
      OutFile.OutCharAndPos(ikItmRedefPrAttr,aItemPos)
     else OutFile.OutCharAndPos(ikItmDefPrAttr,aItemPos);
    StoreFormat(nDefAttrPattern);
    StoreDefiniens(nDefiniens,'m');
   end;
  itDefPred:
   with PredicateDefinitionPtr(aContent)^ do
   begin
    if nRedefinition then
      OutFile.OutCharAndPos(ikItmRedefPred,aItemPos)
    else OutFile.OutCharAndPos(ikItmDefPred,aItemPos);
    StoreFormat(nDefPredPattern);
    StoreDefiniens(nDefiniens,'m');
   end;
  itDefFunc:
   with FunctorDefinitionPtr(aContent)^ do
   begin
    if nRedefinition then
      OutFile.OutCharAndPos(ikItmRedefFunc,CurPos)
     else OutFile.OutCharAndPos(ikItmDefFunc,CurPos);
    StoreFormat(nDefFuncPattern);
    if nSpecification <> nil then
     begin
      OutFile.OutChar(ikMscSpecification);
      StoreType(nSpecification);
     end;
    case nDefiningWay of
     dfEmpty:;
     dfMeans: StoreDefiniens(nDefiniens,'m');
     dfEquals: StoreDefiniens(nDefiniens,'e');
    end;
   end;
  itDefStruct:
   with StructureDefinitionPtr(aContent)^ do
   begin
     OutFile.OutCharAndPos(ikItmDefStruct,aItemPos);
     for i := 0 to nAncestors^.Count - 1 do
      begin
        OutFile.OutChar(ikMscPrefix);
        StoreType(nAncestors^.Items^[i]);
      end;
     StoreFormat(nDefStructPattern);
     lFieldsNbr:=0;
     for i := 0 to nSgmFields^.Count - 1 do
      with FieldSegmentPtr(nSgmFields^.Items^[i])^ do
       begin
        OutFile.OutWord('Q',nFields^.Count);
        for j := 0 to nFields^.Count - 1 do
         with FieldSymbolPtr(nFields^.Items^[j])^ do
          begin
            OutFile.OutWord('I',0);
            inc(lFieldsNbr);
          end;
         StoreType(nSpecification);
       end;
     lFormatNr:=gFormatsColl.LookUp_PrefixFormat('G',nDefStructPattern^.nModeSymbol,lFieldsNbr);
     OutFile.OutWordAndPos('G',lFormatNr,nStrPos);
     lFormatNr:=gFormatsColl.LookUp_PrefixFormat('J',nDefStructPattern^.nModeSymbol,1);
     OutFile.OutWordAndPos(ikTrmSubAgreg,lFormatNr,nStrPos);
     for i := 0 to nSgmFields^.Count - 1 do
      with FieldSegmentPtr(nSgmFields^.Items^[i])^ do
       begin
        for j := 0 to nFields^.Count - 1 do
         with FieldSymbolPtr(nFields^.Items^[j])^ do
          begin
            lFormatNr:=gFormatsColl.CollectPrefixForm('U',nFieldSymbol,1);
            OutFile.OutWordAndPos(ikTrmSelector,lFormatNr,nFieldPos);
//            OutFile.OutWord(ikTrmLocus,nDefStructPattern^.nArgs.Count+1);
            OutFile.OutWord(ikTrmLocus,gConstVarNbr+1);
            OutFile.OutChar(';');
          end;
       end;
     lFormatNr:=gFormatsColl.CollectPrefixForm('G',nDefStructPattern^.nModeSymbol,lFieldsNbr);
     { widoczny jest argument dogenerowany przez ANALYSER }
   end;
  itFuncNotation:
   with NotationDeclarationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmDefFunc,CurPos);
    StoreFormat(nNewPattern);
    OutFile.OutCharAndPos(ikMscSynonym,aItemPos);
    StoreFormat(nOriginPattern);
   end;
  itPredSynonym:
   with NotationDeclarationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmDefPred,aItemPos);
    StoreFormat(nNewPattern);
    OutFile.OutCharAndPos(ikMscSynonym,aItemPos);
    StoreFormat(nOriginPattern);
   end;
  itModeNotation:
   with NotationDeclarationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmDefMode{zamiana z ikItmRedefMode},aItemPos);
    StoreFormat(nNewPattern);
    OutFile.OutCharAndPos(ikMscSynonym,aItemPos);
    StoreFormat(nOriginPattern);
   end;
  itAttrSynonym:
   with NotationDeclarationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmDefPrAttr,aItemPos);
    StoreFormat(nNewPattern);
    OutFile.OutChar(ikMscSynonym);
    OutFile.OutCharAndPos(ikItmDefPrAttr,aItemPos);
    StoreFormat(nOriginPattern);
   end;
  itPredAntonym:
   with NotationDeclarationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmDefPred,CurPos);
    StoreFormat(nNewPattern);
    OutFile.OutCharAndPos(ikMscAntonym,aItemPos);
    StoreFormat(nOriginPattern);
   end;
  itAttrAntonym:
   with NotationDeclarationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikItmDefPrAttr,aItemPos);
    StoreFormat(nNewPattern);
    OutFile.OutChar(ikMscAntonym);
    OutFile.OutCharAndPos(ikItmDefPrAttr,aItemPos);
    StoreFormat(nOriginPattern);
   end;
  itCluster:
   begin
    case ClusterPtr(aContent)^.nClusterKind of
     ExistentialRegistration:
      with EClusterPtr(aContent)^ do
       begin
        OutFile.OutCharAndPos(ikItmCluRegistered,CurPos);
        StoreAdjectiveList(nConsequent);
        StoreType(nClusterType);
       end;
     ConditionalRegistration:
      with CClusterPtr(aContent)^ do
       begin
        OutFile.OutCharAndPos(ikItmCluConditional,CurPos);
        StoreAdjectiveList(nAntecedent);
        StoreAdjectiveList(nConsequent);
        StoreType(nClusterType);
       end;
     FunctorialRegistration:
      with FClusterPtr(aContent)^ do
       begin
        OutFile.OutCharAndPos(ikItmCluFunctor,CurPos);
        lCTrm:=new(CheckClusterTermPtr, Init(exTerm));
        lCTrm^.Process_Term(nClusterTerm);
        StoreTerm(nClusterTerm);
        StoreAdjectiveList(nConsequent);
        if nClusterType <> nil then
         begin
          OutFile.OutChar('.');
          StoreType(nClusterType);
         end;
       end;
    end;
   end;
  itIdentify:
   with IdentifyRegistrationPtr(aContent)^ do
   begin
    OutFile.OutCharAndPos(ikIdFunctors,aItemPos);
    StoreFormat(nNewPattern);
    StoreFormat(nOriginPattern);
    if nEqLociList <> nil then
     for i := 0 to nEqLociList^.Count - 1 do
      with LociEqualityPtr(nEqLociList^.Items^[i])^ do
      begin
       with MSLocusPtr(nLeftLocus)^ do
        OutFile.OutWordAndPos(ikTrmLocus,nVarNr,nVarIdPos);
       with MSLocusPtr(nRightLocus)^ do
        OutFile.OutWordAndPos(ikTrmLocus,nVarNr,nVarIdPos);
      end;
    OutFile.OutChar(';');
   end;
  itReduction:
   with ReduceRegistrationPtr(aContent)^ do
    begin
     OutFile.OutCharAndPos(ikReduceFunctors,aItemPos);
     StoreTerm(nOriginTerm);
     StoreTerm(nNewTerm);
    end;
  itPropertyRegistration:
   case PropertyRegistrationPtr(aContent)^.nPropertySort of
   sySethood:
    with SethoodRegistrationPtr(aContent)^ do
    begin
     OutFile.OutWordAndPos(ikProperty,ord(sySethood),nPropertyPos);
     StoreType(nSethoodType);
    end;
   end;
  itSection:
    OutFile.OutCharAndPos(ikBlcSection,aItemPos);
  itPragma:
   with PragmaPtr(aContent)^ do
    begin
     CanceledPragma(nPragmaStr,lKind,lNbr);
     if lKind in ['D','S','T'] then
      begin
       if (lKind in ['S','T']) and (nBlockPtr <> nil) then Error(aItemPos,278);
//       if (lKind in ['S','T']) and (nBlockPtr^.nBlockKind <> blMain) then Error(aItemPos,278);
//       if lNbr <= 0 then Error(aItemPos,307);
       for k:=1 to lNbr do
        begin
         OutFile.OutChar(ikItmCanceled);
         OutFile.OutChar(lKind);
        end;
      end
     else
      InsertPragma(aItemPos.Line,PragmaPtr(aContent)^.nPragmaStr);
    end;
  itIncorrItem:;
 end;
end;


procedure Transfer2AnalyserObj.Process_FinishItem(aWSItem:WSItemPtr);
begin
 OutFile.OutNewLine;
end;

procedure Transfer2AnalyserObj.Process_LibraryReference(aRef: LibraryReferencePtr);
begin
  case aRef^.nRefSort of
   TheoremReference:
    with TheoremReferencePtr(aRef)^ do
     TheoReferNbr.Up(nArticleNr,nTheoNr);
   DefinitionReference:
    with DefinitionReferencePtr(aRef)^ do
     DefReferNbr.Up(nArticleNr,nDefNr);
  end;
end;

procedure Transfer2AnalyserObj.Process_SchemeJustification(aInf: SchemeJustificationPtr);
begin
  with MSSchemeJustificationPtr(aInf)^ do
  begin
    if nSchFileNr > 0 then
     begin
      if nSchemeIdNr > 0 then
        SchReferNbr.Up(nSchFileNr,nSchemeIdNr);
     end
    else if nSchemeIdNr > 0 then
     begin
      if nSchemeNr > 0 then
        SchReferNbr.Up(0,nSchemeNr)
     end;
  end;
end;

procedure Transfer2AnalyserObj.Process_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr);
begin
 inherited Process_Justification(aInf,aBlock);
 if aInf^.nInfSort <> infProof then
   StoreInference(aInf);
end;

procedure Transfer2AnalyserObj.Process_StartBlock(aWSBlock:WSBlockPtr);
begin
 Transfer2AnalyserBlockPtr(nStackArr[nStackCnt])^.nConstVarNbr:=gConstVarNbr;
 with aWSBlock^ do
 case nBlockKind of
  blDiffuse:
   begin
    OutFile.OutNewLine;
    OutFile.OutCharAndPos(ikBlcDiffuse,nBlockPos);
    OutFile.OutNewLine;
   end;
  blHereby:
   begin
    OutFile.OutNewLine;
//    OutFile.OutCharAndPos(ikBlcHereby,nBlockKind,nBlockPos);
    OutFile.OutCharAndPos(ikBlcDiffuse,nBlockPos);
    OutFile.OutNewLine;
   end;
  blProof:
   begin
    OutFile.OutNewLine;
    OutFile.OutCharAndPos(ikBlcProof,nBlockPos);
    OutFile.OutNewLine;
   end;
  blCase:
   begin
    OutFile.OutNewLine;
    OutFile.OutCharAndPos(ikBlcCase,nBlockPos);
    OutFile.OutNewLine;
   end;
  blSuppose:
   begin
    OutFile.OutNewLine;
    OutFile.OutCharAndPos(ikBlcSuppose,nBlockPos);
    OutFile.OutNewLine;
   end;
  blDefinition:
   OutFile.OutCharAndPos(ikBlcDefinition,nBlockPos);
  blNotation:
   OutFile.OutCharAndPos(ikBlcNotation,nBlockPos);
  blRegistration:
   OutFile.OutCharAndPos(ikBlcRegistration,nBlockPos);
  blPublicScheme:
   begin
   end;
 end;
end;

procedure Transfer2AnalyserObj.Process_FinishBlock(aWSBlock:WSBlockPtr);
begin
 case aWSBlock^.nBlockKind of
  blDefinition, blNotation, blRegistration, blPublicScheme,blProof,
  blHereby,blDiffuse,blCase,blSuppose:
   begin CurPos:=aWSBlock^.nBlockEndPos;
    OutFile.OutCharAndPos(ikMscEndBlock,aWSBlock^.nBlockEndPos);
   end;
 end;
 gConstVarNbr:=Transfer2AnalyserBlockPtr(nStackArr[nStackCnt])^.nConstVarNbr;
end;

{----------------------------------------------------------------}

procedure Transfer2Analyzer;
 var lWSTextProper: wsTextProperPtr;
     lMizArticle: Transfer2AnalyserPtr;
begin
 lWSTextProper:=Read_MSMizArticle((EnvFileName+'.msx'));
 lMizArticle:=new(Transfer2AnalyserPtr,Init(lWSTextProper));
 lMizArticle^.Process_Article;
 dispose(lMizArticle,Done);
// dispose(lWSTextProper,Done);
end;

{----------------------------------------------------------------}

var gSchemeDemonstration: boolean;

constructor Transfer2ExporterObj.Init(aMSTextProper:WSTextProperPtr);
begin
 inherited Init(aMSTextProper);
 gSchemeDemonstration:=false;
end;

destructor Transfer2ExporterObj.Done;
begin
 OutFile.OutChar('!');
 OutFile.Done;
end;

procedure Transfer2ExporterObj.Process_StartBlock(aWSBlock:WSBlockPtr);
begin
 Transfer2AnalyserBlockPtr(nStackArr[nStackCnt])^.nConstVarNbr:=gConstVarNbr;
 case aWSBlock^.nBlockKind of
  blDiffuse,blHereby,blCase,blSuppose:;
   else inherited Process_StartBlock(aWSBlock);
 end;
end;

procedure Transfer2ExporterObj.Process_FinishBlock(aWSBlock:WSBlockPtr);
begin
 case aWSBlock^.nBlockKind of
  blHereby,blDiffuse,blCase,blSuppose: ;
  else inherited Process_FinishBlock(aWSBlock);
 end;
end;

procedure Transfer2ExporterObj.Process_ItemsContent(aItemKind:ItemKind; const aItemPos:Position; aContent: PObject);
 var lNbr,k: integer;
     lKind: char;
     lPos: Position;
begin
 case aItemKind of
  itRegularStatement, itExemplification, itGeneralization, itConclusion,
  itPerCases,itCaseBlock, itCaseHead, itSupposeHead:
   begin
   end;
  itPrivFuncDefinition, itPrivPredDefinition,
  itChoice, itReconsider, itConstantDefinition,
  itAssumption, itExistentialAssumption:
   if not gSchemeDemonstration then
    inherited Process_ItemsContent(aItemKind,aItemPos,aContent);
  itPragma:
   with PragmaPtr(aContent)^ do
    begin
     CanceledPragma(nPragmaStr,lKind,lNbr);
     if lKind in ['D','S','T'] then
      begin
       if (lKind in ['S','T']) and (nBlockPtr <> nil) then Error(aItemPos,278);
//       if (lKind in ['S','T']) and (nBlockPtr^.nBlockKind <> blMain) then Error(aItemPos,278);
//       if lNbr <= 0 then Error(aItemPos,307);
       for k:=1 to lNbr do
        begin
         OutFile.OutChar(ikItmCanceled);
         OutFile.OutChar(lKind);
        end;
      end
     else if (Copy(nPragmaStr,1,8) = '$SECTION') then
      begin
//       OutFile.OutCharAndPos(ikBlcSection,aItemPos);
      end
     else inherited Process_ItemsContent(aItemKind,aItemPos,aContent);
    end
  else inherited Process_ItemsContent(aItemKind,aItemPos,aContent);
 end;
end;

procedure Transfer2ExporterObj.Process_Justification(aInf: JustificationPtr; aBlock:wsBlockPtr);
begin
 case aInf^.nInfSort of
  infStraightforwardJustification:
   Process_StraightforwardJustification(StraightforwardJustificationPtr(aInf));
  infSchemeJustification:
   Process_SchemeJustification(SchemeJustificationPtr(aInf));
  infProof: ;
  infError, infSkippedProof: ;
 end;
end;

procedure Transfer2ExporterObj.Process_DiffuseStatement(aRStm:DiffuseStatementPtr);
begin
end;

procedure Transfer2ExporterObj.Process_RegularStatement(aRStm:RegularStatementPtr; aBlock:wsBlockPtr);
begin
end;

procedure Transfer2ExporterObj.Process_Scheme(aWSBlock:WSBlockPtr);
begin
  gSchemeDemonstration:=true;
  Process_Block(aWSBlock);
  gSchemeDemonstration:=false;
end;

{----------------------------------------------------------------}

procedure Transfer2Exporter;
 var lWSTextProper: wsTextProperPtr;
     lMizArticle: Transfer2AnalyserPtr;
begin
 lWSTextProper:=Read_MSMizArticle((EnvFileName+'.msx'));
 lMizArticle:=new(Transfer2ExporterPtr,Init(lWSTextProper));
 lMizArticle^.Process_Article;
 dispose(lMizArticle,Done);
// dispose(lWSTextProper,Done);
end;

end.
