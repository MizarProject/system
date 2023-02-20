(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// Parser/handler for the detailed analyzer reports

// Usable directives: 
//  DEBUG : Parsing events and errors are logged into .inf
//  NO_LAST_DISPOSE : parsed data are not disposed even when
//                    unnecessary; use for memory bugs

// DISPOSING:
// Unlike in parser, the atomic parsing events can produce bigger objects
// - we often allocate and parse whole term, formula, etc.
// They become available in the nLast... (e.g. nLastType) slot.
// The handler has to set this variables to nil, if it wants that the
// object is not disposed upon the next parsing event of the same type.
// If you do not like this behaviour, compile with NO_LAST_DISPOSE.

// NOTES:
// - watching Thesis for DiffuseStatements? - seems not needed now
// - Thesis is a separate item, usually following skeleton items.
//   We could make it part of skeleton items, but in Diffuse, they
//   are not known now - can be changed if needed.


unit prephan;

interface

uses mobjects, justhan, identify, correl, iocorrel, prepobj, propcoll
{$IFDEF CH_REPORT}
     ,req_info
{$ENDIF} 
;

procedure Prepare;
procedure InitProcessing;
{$IFDEF CH_REPORT}
var  CHReport:OutRQInfObj;
{$ENDIF} 
{$IFDEF SCH_REPORT}
var  SchReport:OutVRFFileObj;
{$ENDIF} 

type
 
// The preparator parser. This can be inherited/overloaded
// e.g. if you want to remember on the parser level more than just the
// last atomic event, or if input is other than InVRFFileObj.
// As there are no descendants now, the handlers are not virtual,
// change it when needed. 
// ##TODO: add Current and CurPos to either MizStream or MPrepParser
// ##TODO: nCollecting should be a field of InVRFFileObj 
 PPrepParser= ^MPrepParser; 
 MPrepParser= object(MObject)
  nSrc: InVRFFileObj;
  // Atomic parsed objects
  nLastInference: InferencePtr;
  nLastIterSteps: PCollection;
  nLastDefiniens: DefiniensPtr;
  nLastDefCluster: PObject;
  nLastIdentify: IdentifyPtr;
  nLastReduction: ReductionPtr;
  nLastProperty: PropertyPtr;
  nLastProposition: PrepProposPtr;
  nLastFormula: FrmPtr;
  nLastTerm: TrmPtr;
  nLastType: TypPtr;
  nLastTypeList: MListPtr;
  nLastConstructor: ConstrPtr;
  nLastThesisExpansions: NatFuncPtr;// numbers of definientia with usage counts
  constructor InitParser(const AFileName:string);
  destructor Done; virtual;
// protected
  // atomic
  procedure Parse_Proposition(fKind:PropositionKind);
  procedure Parse_Formula;
  procedure Parse_Term;
  procedure Parse_TypeExpr;
  procedure Parse_Types;
  procedure Parse_Inference;
  procedure Parse_IterSteps;
  procedure Parse_Definiens;
  procedure Parse_ThesisExpansions;
  procedure Parse_DefCluster;
  // helpers
  procedure Parse_LabelAndPosition;
  procedure Parse_BlockPositionAndLabel;
  procedure Parse_FixedVariables;
  procedure Parse_QuotableProposition(fKind: PropositionKind);
  procedure Parse_UnquotableProposition(fKind: PropositionKind);
  procedure Parse_QuotablePropositions(fKind: PropositionKind);
  // skeleton items
  procedure Parse_Generalization;
  procedure Parse_Assumption;
  procedure Parse_ExistentialAssumption;
  procedure Parse_SimpleExemplification;
  procedure Parse_ExemplificationWithEquality;
  procedure Parse_Conclusion;
  // auxiliary items
  procedure Parse_Choice;
  procedure Parse_Reconsidering;
  procedure Parse_ConstantDefinition;
  procedure Parse_DefFunc;
  procedure Parse_DefPred;
  // other stuff
  procedure Parse_Thesis;
  procedure Parse_CaseOrSupposeItem(fItKind:CaseItemKind);
  procedure Parse_BlockThesis;
  procedure Parse_CaseOrSupposeBlock(fBlKind:CaseBlKind; fDiffuse:boolean);
  procedure Parse_SimpleJustification;
  procedure Parse_AtSignProof;
  procedure Parse_PerCasesJustification;
  procedure Parse_CaseList(fDiffuse:boolean);
  procedure Parse_Reasoning(fDiffuse:boolean);
  procedure Parse_Proof;
  procedure Parse_DiffuseReasoning;
  procedure Parse_Justification;
  procedure Parse_IterativeEquality;
  procedure Parse_PrivateStatement;
  procedure Parse_AuxiliaryItem;
  procedure Parse_Section;
  procedure Parse_Reservation;
  procedure Parse_Theorem;
  procedure Parse_DefinitionalTheorem;
  procedure Parse_OneProperty;
  procedure Parse_CorrCondition;
  procedure Parse_Correctness;
  procedure Parse_OneDefiniens;
  procedure Parse_Canceled;
  procedure Parse_SchemeTypes;
  procedure Parse_SchemeStatement;
  procedure Parse_SchemePremises;
  procedure Parse_Scheme;
  procedure Parse_OneConstructor;
  procedure Parse_OneDefinition;
  procedure Parse_Definition;
  procedure Parse_OneCluster;
  procedure Parse_OneIdentify;
  procedure Parse_OneReduction;
  procedure Parse_OnePropertyRegistration;
  procedure Parse_Registration;
  procedure Parse_Notation;
  procedure Parse_Main;
 end;

var gPrep: PPrepParser;

implementation
uses
 inoutmml, builtin, errhan, mizenv, inout, lexicon, xml_parser,
 xmlpars, xmldict, librenv, mscanner

 {$IFDEF MDEBUG}
,info,outinfo
{$ENDIF}
{$IFDEF CHSTAT}
 ,SysUtils
{$ENDIF}
;

(* ##RNC:
## Reports from the Mizar checker, now only arithmetical evaluations.
## They are now only available when the verifier is compiled with a special
## directive - this should be changed to a user option eventually.   
elByExplanations =
 element elByExplanations {
   attribute atAid { xsd:string },
   elPolyEval*
   }
   
## Reports from the Mizar schematizer - scheme instantioations.
## They are now only available when the verifier is compiled with a special
## directive - this should be changed to a user option eventually.   
elFromExplanations =
 element elFromExplanations {
   attribute atAid { xsd:string },
   elSchemeInstantiation*
   }
*)
procedure Prepare;
begin
 gPrep:= new(PPrepParser, InitParser(MizFileName+'.xml'));
{$IFDEF SCH_REPORT}
 SchReport.OpenFileWithXSL(MizFileName+'.fex');
 SchReport.Out_XElStart( elFromExplanations);
 SchReport.Out_XAttr( atAid, ArticleID);
 SchReport.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 SchReport.Out_XAttrEnd;
{$ENDIF}  
 gPrep^.Parse_Main;
 dispose(gPrep, Done);
{$IFDEF CH_REPORT}
 CHReport.Out_XElEnd( elByExplanations);
 CHReport.Done;
{$ENDIF}   
{$IFDEF SCH_REPORT}
 SchReport.Out_XElEnd( elFromExplanations);
 SchReport.Done;
{$ENDIF}   
end;


procedure InitProcessing;
begin
 gPrBlockPtr:=new(PrepBlockPtr, Init(blMain));
end;

constructor MPrepParser.InitParser(const AFileName:string);
begin
 nSrc.OpenFile(AFileName);

 nLastInference:= nil;
 nLastIterSteps:= nil;
 nLastDefiniens:= nil;
 nLastIdentify:=nil;
 nLastReduction:=nil;
 nLastDefCluster:= nil;
 nLastProposition:= nil;
 nLastFormula:= nil;
 nLastTerm:= nil;
 nLastType:= nil;
 nLastTypeList:= nil;
 nLastThesisExpansions:= nil;
 nLastConstructor:= nil;
{$IFDEF CHSTAT}
 gStat.Init(0,8);
{$ENDIF}

end;


procedure DisposeIfAssigned( fPtr: PObject);
begin
{$IFNDEF NO_LAST_DISPOSE}
 if Assigned(fPtr) then dispose(fPtr, Done);
{$ENDIF}
end;

destructor MPrepParser.Done;
{$IFDEF CHSTAT}
  var i,j: integer;
      lStats: text;
{$ENDIF}
begin
 nSrc.Done;

 DisposeIfAssigned(nLastInference);
 DisposeIfAssigned(nLastIterSteps);
 DisposeIfAssigned(nLastDefiniens);
 DisposeIfAssigned(nLastIdentify);
 DisposeIfAssigned(nLastReduction);
 DisposeIfAssigned(nLastDefCluster);
 DisposeIfAssigned(nLastProposition);
 DisposeIfAssigned(nLastFormula);
 DisposeIfAssigned(nLastTerm);
 DisposeIfAssigned(nLastType);
 DisposeIfAssigned(nLastTypeList);
 DisposeIfAssigned(nLastThesisExpansions);
 DisposeIfAssigned(nLastConstructor);
{$IFDEF CHSTAT}
 if gStat.Count > 0 then
  begin
   if SysUtils.FileExists('stats.csv') then
    begin
     assign(lStats,'stats.csv');
     append(lStats);
     writeln(lStats);
    end
   else
    begin
     assign(lStats,'stats.csv');
     rewrite(lStats);
    end;
   for i := 0 to gStat.Count - 1 do
   with CHStatPtr(gStat.Items^[i])^ do
    begin
     write(lStats,ArticleName,', ',nInfPos.Line,', ',nClausesNbr);
     write(lStats,', ',nContrNr);
     write(lStats,', ',nInstStats.fCount);
     for j:=0 to nInstStats.fCount - 1 do
      write(lStats,', ',nInstStats.fList^[j]);
     writeln(lStats);
    end;
  end;
 gStat.Done;
{$ENDIF}
end;

// Patterns are completely ignored now, store them in
// nLastPattern and create the empty prepobj handlers if needed.
(* ##RNC:
## The complete article after analyzer.
## atAid specifies its name in uppercase, and atMizfiles
## optionally gives a location of the local MIZFILES directory used
## during processing the article (needed to know for browsing of
## locally installed html in MIZFILES/html).
elArticle =
 element elArticle {
   attribute atAid { xsd:string },
   attribute atMizfiles { xsd:string }?,
   ( elDefinitionBlock | elRegistrationBlock |
     elNotationBlock | elReservation | elSchemeBlock |
     elJustifiedTheorem | elDefTheorem | elDefiniens |
     elCanceled | elSection | AuxiliaryItem )*
 }
*)
procedure MPrepParser.Parse_Main;
begin
 nSrc.NextElementState;
 XMLASSERT( nSrc.nElKind = elArticle);
 nSrc.NextElementState;
 with nSrc do
 while nState <> eEnd do
 case nElKind of
  elSection: Parse_Section;
  elDefinitionBlock: Parse_Definition;
  elRegistrationBlock: Parse_Registration;
  elNotationBlock: Parse_Notation;
  elReservation: Parse_Reservation;
  elSchemeBlock: Parse_Scheme;
  elJustifiedTheorem: Parse_Theorem;
  elDefTheorem: Parse_DefinitionalTheorem;
  elDefiniens: Parse_OneDefiniens;
  elCanceled: Parse_Canceled;
  else
   Parse_AuxiliaryItem;
 end;
end;


procedure AssertCurrentIn(fExpected:TCharSet; fErr:integer);
begin
 if not (InFile.Current.Kind in fExpected) then
   UnexpectedKind(InFile.Current.Kind, fErr, fExpected);
end;

(********** Atomic parsing events **********)

// This one is not exactly atomic
procedure MPrepParser.Parse_Proposition(fKind:PropositionKind);
var lFrm:FrmPtr; lNr,ec: integer; lPos: Position; lStr:string;
begin
 DisposeIfAssigned(nLastProposition);
 with nSrc do
 begin
  XMLASSERT( nElKind = elProposition);
  InFile.Current.Kind:= 'E';
  if GetOptAttr( atNr, lStr) then Val( lStr, lNr, ec)
   else lNr:= 0;
  InFile.Current.Nr:= lNr;
  gPrBlockPtr^.nCurrItm^.ProcessLabel;
  GetPosAttrs( CurPos);
  lPos:= CurPos;
  gPrBlockPtr^.nCurrItm^.ProcessPosition;
  NextElementState;
  lFrm:= In_Formula;
//  gPrBlockPtr^.ProcessFormula;
  nLastProposition := new(PrepProposPtr,Init( lNr, lFrm, lPos, fKind));
  gPrBlockPtr^.ProcessProposition;
  NextElementState;
 end;
end;

procedure MPrepParser.Parse_Formula;
begin
 DisposeIfAssigned(nLastFormula);
 with nSrc do nLastFormula:=In_Formula;
 gPrBlockPtr^.ProcessFormula;
end;

procedure MPrepParser.Parse_Term;
begin
 DisposeIfAssigned(nLastTerm);
 with nSrc do nLastTerm:= In_Term; 
 gPrBlockPtr^.ProcessTerm;
end; { Parse_Term }

procedure MPrepParser.Parse_TypeExpr;
begin
 DisposeIfAssigned(nLastType);
 with nSrc do nLastType:= In_Type;
 gPrBlockPtr^.ProcessType;
end;

procedure MPrepParser.Parse_Types;
var lTypList:MList;
begin
 DisposeIfAssigned(nLastTypeList);
 with nSrc do In_TypeList(lTypList); 
 nLastTypeList := new(MListPtr,MoveList(lTypList));
 gPrBlockPtr^.ProcessTypeList;
end;

procedure MPrepParser.Parse_Inference;
begin
 DisposeIfAssigned(nLastInference);
 nLastInference := new(InferencePtr, Init);
 nSrc.In_Inference(nLastInference^);
end;

procedure MPrepParser.Parse_IterSteps;
 var lTrm:TrmPtr; lInf:InferenceObj;
begin
 DisposeIfAssigned(nLastIterSteps);
 nLastIterSteps := new(PCollection,Init(4,4));
 while not (nSrc.nState = eEnd) do
  begin nSrc.In_IterStep(lTrm,lInf);
   nLastIterSteps^.Insert(new(IterStepPtr,Init(lTrm,lInf)));
  end;
 gPrBlockPtr^.ProcessIterSteps;
end;

procedure MPrepParser.Parse_Definiens;
begin
 DisposeIfAssigned(nLastDefiniens);
 nLastDefiniens := nSrc.In_Definiens;
 gPrBlockPtr^.ProcessDefiniens;
end;

(* ##RNC:
## Numbers of elDefiniens used in expanding the thesis,
## together with their counts.
elThesisExpansions = 
 element elThesisExpansions { elPair* }
*)
procedure MPrepParser.Parse_ThesisExpansions;
var lFunc:NatFunc;
begin
 DisposeIfAssigned(nLastThesisExpansions);
 with nSrc do In_NatFunc( elThesisExpansions, lFunc); 
 nLastThesisExpansions := new( NatFuncPtr, MoveNatFunc( lFunc));
end;

(*const ClusterSyms = [
 ikItmCluRegistered,
 ikItmCluConditional,
 ikItmCluFunctor
                    ];
*)

procedure MPrepParser.Parse_DefCluster;
begin
 DisposeIfAssigned(nLastDefCluster);
 case nSrc.nElKind of
  elRCluster:
   begin nLastDefCluster := nSrc.In_RCluster;
    gPrBlockPtr^.ProcessRCluster;
   end;
  elCCluster:
   begin nLastDefCluster := nSrc.In_CCluster;
    gPrBlockPtr^.ProcessCCluster;
   end;
  elFCluster:
   begin nLastDefCluster := nSrc.In_FCluster;
    gPrBlockPtr^.ProcessFCluster;
   end;
 else UnexpectedElem(nSrc.nElKind, errUnexpected, ClusterElKinds);
 end;
end;

(********** Some needed parts of items **********)

// ##TODO: get rid of the InFile.Current hack here
procedure MPrepParser.Parse_LabelAndPosition;
var lStr:string; ec:integer;
begin
 InFile.Current.Kind:= 'E';
 if nSrc.GetOptAttr( atNr, lStr) then
  Val( lStr, InFile.Current.Nr, ec)
 else InFile.Current.Nr:= 0;
 gPrBlockPtr^.nCurrItm^.ProcessLabel;
 nSrc.GetPosAttrs( CurPos);
 gPrBlockPtr^.nCurrItm^.ProcessPosition;
end;

// Only quotable blocks can have nonzero labels
procedure MPrepParser.Parse_BlockPositionAndLabel;
var lStr:string; ec:integer;
begin
 with nSrc do begin
  GetPosAttrs( CurPos);
  gPrBlockPtr^.ProcessBlockPosition;
  InFile.Current.Kind:= 'E';
  if nSrc.GetOptAttr( atNr, lStr) then
   Val( lStr, InFile.Current.Nr, ec)
  else InFile.Current.Nr:= 0;
  if (not gPrBlockPtr^.BlockIsQuotable)
     and (InFile.Current.Nr <> 0)
  then UnexpectedNumber(InFile.Current.Nr, errUnexpected, 0);
  gPrBlockPtr^.ProcessBlockLabel;
 end;
end;

// ##NOTE: This cannot be loaded into a typecollection, because
//         the latter types may use the former, and we collect
//         inference constants right during reading
procedure MPrepParser.Parse_FixedVariables;
begin
 gPrBlockPtr^.nCurrItm^.StartFixedVariables;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind = elTyp) do
 begin
  gPrBlockPtr^.nCurrItm^.StartFixedVariable;
  Parse_TypeExpr;
  gPrBlockPtr^.nCurrItm^.FinishFixedVariable;
 end;
 gPrBlockPtr^.nCurrItm^.FinishFixedVariables;
end;

// DiffuseReasoning and IterativeEquality are also quotable,
// anything else?
procedure MPrepParser.Parse_QuotableProposition(fKind: PropositionKind);
begin
 gPrBlockPtr^.nCurrItm^.StartQuotableProposition(fKind);
 Mizassert(errUnexpectedProposKind, not (fKind in UnquotablePropKinds));
 Parse_Proposition(fKind);
 gPrBlockPtr^.nCurrItm^.FinishQuotableProposition(fKind);
end;

// These cannot be quoted, but often must be verified.
// That means there label must be 0, but label=0 does not
// imply unquotable.
procedure MPrepParser.Parse_UnquotableProposition(fKind: PropositionKind);
begin
 gPrBlockPtr^.nCurrItm^.StartUnquotableProposition(fKind);
 Mizassert(errUnexpectedProposKind, fKind in UnquotablePropKinds);
 Parse_Proposition(fKind);
 gPrBlockPtr^.nCurrItm^.FinishUnquotableProposition(fKind);
end;

// ##THINK: we could put the propositions into a collection, but
//          the labels would miss anyway, so let this do the upper handlers.
//          Change this if not suitable.
procedure MPrepParser.Parse_QuotablePropositions(fKind: PropositionKind);
begin
 gPrBlockPtr^.nCurrItm^.StartQuotablePropositions(fKind);
 while (nSrc.nState <> eEnd) and (nSrc.nElKind = elProposition) do
  Parse_QuotableProposition(fKind);
 gPrBlockPtr^.nCurrItm^.FinishQuotablePropositions(fKind);
end;

(********** Skeleton items **********)

(* ##RNC:
## Introduction of local constants, the numbering is automatic,
## so only types are needed.
## For easier presentation, atNr optionally contains the number   
## of the first local constant created here.
## Each type may optionally have presentational info about
## the variable (atVid) inside.   
elLet = 
 element elLet {
   attribute atNr { xsd:integer }?,   
   elTyp+ }
*)
procedure MPrepParser.Parse_Generalization;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elLet);
 lItm := gPrBlockPtr^.CreateItem(itGeneralization);
 nSrc.NextElementState;
 Parse_FixedVariables;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## One assumption may consist of several propositions.
elAssume = 
 element elAssume { elProposition+ }
*)
procedure MPrepParser.Parse_Assumption;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elAssume);
 lItm := gPrBlockPtr^.CreateItem(itAssumption);
 nSrc.NextElementState;
 Parse_QuotablePropositions(propAssumption);
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## This is existential assumption, it may be used when the normal
## assumption starts with existential quantifier, and emulates
## the use of assume followed by consider.
## First comes the reconstructed assumed existential statement, then
## the newly introduced local constant(s), and finally the proposition(s)
## containing the new local constant(s).
## For easier presentation, atNr optionally contains the number   
## of the first local constant created here.   
## Each type may optionally have presentational info about
## the variable (atVid) inside.   
elGiven = 
 element elGiven {
   attribute atNr { xsd:integer }?,
   elProposition, elTyp+, elProposition+ }
*)
procedure MPrepParser.Parse_ExistentialAssumption;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elGiven);
 lItm := gPrBlockPtr^.CreateItem(itExistentialAssumption);
 nSrc.NextElementState;
 Parse_UnquotableProposition(propExistentialAssumption);
 Parse_FixedVariables;
 Parse_QuotablePropositions(propExistentialAssumptionResult);
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## Take without equality. This does not introduce a new local constant,
## just changes the thesis.
elTake = 
 element elTake { Term }
*)
procedure MPrepParser.Parse_SimpleExemplification;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elTake);
 lItm := gPrBlockPtr^.CreateItem(itSimpleExemplification);
 nSrc.NextElementState;
 Parse_Term;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## Take with equality. This introduces a new local constant,
## whose type is given here.
## For easier presentation, atNr optionally contains the number   
## of the first local constant created here.      
## The type may optionally have presentational info about
## the variable (atVid) inside.   
elTakeAsVar = 
 element elTakeAsVar {
   attribute atNr { xsd:integer }?,
   elTyp, Term }
*)
procedure MPrepParser.Parse_ExemplificationWithEquality;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elTakeAsVar);
 lItm := gPrBlockPtr^.CreateItem(itExemplificationWithEquality);  
 nSrc.NextElementState;
 Parse_TypeExpr;
 lItm^.StartExemplifyingTerm;
 Parse_Term;
 lItm^.FinishExemplifyingTerm;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## Justified conclusion. In text, this can appear as _hence_,
## _thus_ or _hereby_ (which starts diffuse conclusion).
elConclusion = 
 element elConclusion { JustifiedProposition }
*)
procedure MPrepParser.Parse_Conclusion;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elConclusion);
 lItm := gPrBlockPtr^.CreateItem(itConclusion);
 nSrc.NextElementState;
 Parse_PrivateStatement;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(********** Auxiliary items **********)

// order is changed to make stdprep easy
(* ##RNC:
## First comes the reconstructed existential statement
## and its justification, then the new local constants
## and zero or more propositions about them.
## For easier presentation, atNr optionally contains the number   
## of the first local constant created here.      
## Each type may optionally have presentational info about
## the variable (atVid) inside.   
elConsider =
 element elConsider { 
   attribute atNr { xsd:integer }?,
   elProposition, Justification,
   elTyp+, elProposition*
 }
*)
procedure MPrepParser.Parse_Choice;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elConsider);
 lItm := gPrBlockPtr^.CreateItem(itChoice);
 nSrc.NextElementState;
 Parse_UnquotableProposition(propChoiceJustification);
 Parse_Justification;
 Parse_FixedVariables;
 Parse_QuotablePropositions(propChoiceResult);
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

// Moved type to come first here, for stdprep   
(* ##RNC:
## First comes the series of target types and reconsidered terms.
## For all these terms a new local variable with its target type
## is created, and its equality to the corresponding term is remembered.
## Finally the proposition about the typing is given and justified.
## For easier presentation, atNr optionally contains the number
## of the first local constant created here.   
## Each type may optionally have presentational info about
## the variable (atVid) inside.   
elReconsider =
 element elReconsider { 
   attribute atNr { xsd:integer }?,
   (elTyp, Term)+,
   elProposition, Justification
 }
*)
procedure MPrepParser.Parse_Reconsidering;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elReconsider);
 lItm := gPrBlockPtr^.CreateItem(itReconsider);
 nSrc.NextElementState;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind = elTyp) do
 begin
  Parse_TypeExpr;
  lItm^.StartReconsideredTerm;
  Parse_Term;
  lItm^.FinishReconsideredTerm;
 end;
 lItm^.FinishReconsidering;
 Parse_UnquotableProposition(propReconsideringJustification);
 Parse_Justification;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## This is e.g.: set a = f(b); . The type of the new local constant
## is given. This local constant is now always expanded to its
## definition, and should not directly appear in any expression,
## but it is now needed for some implementation reasons.
## For easier presentation, atNr optionally contains the number   
## of the first local constant created here.      
## The type may optionally have presentational info about
## the variable (atVid) inside.   
elSet =
 element elSet { 
   attribute atNr { xsd:integer }?,
   Term, elTyp
 }
*)
procedure MPrepParser.Parse_ConstantDefinition;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elSet);
 lItm := gPrBlockPtr^.CreateItem(itConstantDefinition);
 nSrc.NextElementState;
 Parse_Term;
 Parse_TypeExpr;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## Private functor. First come the types of arguments, then
## its definition and the result type.
## For easier presentation, atNr optionally contains number   
## of the private functor created here, and its identifier's number (atVid).
elDefFunc =
 element elDefFunc {
   attribute atNr { xsd:integer }?,
   attribute atVid { xsd:integer }?,
   elArgTypes, Term, elTyp
 }
*)
procedure MPrepParser.Parse_DefFunc;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elDefFunc); 
 lItm := gPrBlockPtr^.CreateItem(itPrivFuncDefinition);
 nSrc.NextElementState;
 XMLASSERT( nSrc.nElKind = elArgTypes);
 nSrc.NextElementState;
 Parse_Types;
 nSrc.NextElementState;
 Parse_Term;
 Parse_TypeExpr;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end; 

(* ##RNC:
## Private predicate. First come the types of arguments, then
## its definition.
## For easier presentation, atNr optionally contains number   
## of the private predicate created here, and its identifier's number (atVid).
elDefPred =
 element elDefPred { 
   attribute atNr { xsd:integer }?,
   attribute atVid { xsd:integer }?,
   elArgTypes, Formula
 }
*)
procedure MPrepParser.Parse_DefPred;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elDefPred);
 lItm := gPrBlockPtr^.CreateItem(itPrivPredDefinition);
 nSrc.NextElementState;
 XMLASSERT( nSrc.nElKind = elArgTypes);
 nSrc.NextElementState;
 Parse_Types;
 nSrc.NextElementState;
 Parse_Formula;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end; 
   
(* ##RNC:
## The changed thesis is printed after skeleton items in proofs,
## together with the numbers of definientia used for its expansion.
elThesis =
 element elThesis { 
   Formula, elThesisExpansions
 }
*)
procedure MPrepParser.Parse_Thesis;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elThesis);
 lItm := gPrBlockPtr^.CreateItem(itThesis);
 nSrc.NextElementState;
 Parse_Formula;
 Parse_ThesisExpansions;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end; { Parse_Thesis }

function CasePropKind(fItKind:CaseItemKind):PropositionKind;
begin
 case fItKind of
  itCase : CasePropKind := propCase;
  itSuppose : CasePropKind := propSuppose;
 else RunTimeError(errUnexpectedItemKind);
 end;
end;

(* ##RNC:
## Case of one or more propositions.
elCase =
 element elCase { elProposition+ }

## Supposition of one or more propositions.
elSuppose =
 element elSuppose { elProposition+ }
*)
procedure MPrepParser.Parse_CaseOrSupposeItem(fItKind:CaseItemKind);
var lItm: PrepItemPtr;
begin
 lItm := gPrBlockPtr^.CreateItem(fItKind);
 nSrc.NextElementState;
 Parse_QuotablePropositions(CasePropKind(fItKind));
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## The block thesis is printed for proofs in the beginning and
## for diffuse reasoning in the end.
## For diffuse reasoning, the series of temporary subthesis corresponding to
## all skeleton items is printed before the main theses (in the same order
## as the skeleton items in the block).   
elBlockThesis =
 element elBlockThesis { elThesis*, Formula }
*)
// Any possible temporary thesis from diffuse block's body
// is ignored here, since it might contain block-local constructs
// (which might not exist any more here).
procedure MPrepParser.Parse_BlockThesis;
begin
 XMLASSERT( nSrc.nElKind = elBlockThesis);
 gPrBlockPtr^.StartBlockThesis;
 nSrc.NextElementState;
 while (nSrc.nElKind = elThesis){ (nState = eStart)} do
 begin
  nSrc.NextTag;
  while (nSrc.nElKind <> elThesis){ (nState = eEnd)} do nSrc.NextTag;
  nSrc.NextElementState;
 end;
 Parse_Formula;
 gPrBlockPtr^.FinishBlockThesis;
 nSrc.NextElementState;
end;

(* ##RNC:
## Block starting with one case, the direct and diffuse version
## (this depends on the kind of its parent block).
## The block thesis is printed for proofs in the beginning and
## for diffuse reasoning in the end.
elCaseBlock =
 element elCaseBlock {
   Position,
   (  ( elBlockThesis, elCase, elThesis, Reasoning )
   |  ( elCase, Reasoning, elBlockThesis ) )
 }
   
## Block starting with one supposition, the direct and diffuse version
## (this depends on the kind of its parent block).
## The block thesis is printed for proofs in the beginning and
## for diffuse reasoning in the end.
elSupposeBlock =
 element elSupposeBlock {
   Position,
   (  ( elBlockThesis, elSuppose, elThesis, Reasoning )
   |  ( elSuppose, Reasoning, elBlockThesis ) )
 }
*)
procedure MPrepParser.Parse_CaseOrSupposeBlock(fBlKind:CaseBlKind;
                                               fDiffuse:boolean);
begin
 XMLASSERT( nSrc.nElKind in [elCaseBlock,elSupposeBlock]);
 gPrBlockPtr^.CreateBlock(fBlKind);
 Parse_BlockPositionAndLabel;
 nSrc.NextElementState;
 if (not fDiffuse) then Parse_BlockThesis;
 Parse_CaseOrSupposeItem(CaseFirstItemKind[fBlKind]);
 if not fDiffuse then Parse_Thesis;
 Parse_Reasoning(fDiffuse);
 if fDiffuse then Parse_BlockThesis;
 KillPBlock;
 nSrc.NextElementState;
end; 

procedure MPrepParser.Parse_SimpleJustification;
begin  with nSrc do
begin
 gPrBlockPtr^.nCurrItm^.StartSimpleJustification;
 Parse_Inference;
 gPrBlockPtr^.nCurrItm^.FinishSimpleJustification;  
end; 
end; { Parse_SimpleJustification }

(* ##RNC:
## This means that the author has skipped the proof.
## Articles with such items are not yet fully completed.
elSkippedProof =
 element elSkippedProof { empty }
*)
procedure MPrepParser.Parse_AtSignProof;
begin
 XMLASSERT( nSrc.nElKind = elSkippedProof);
 gPrBlockPtr^.nCurrItm^.ProcessAtSignProof;
 nSrc.AcceptEndState; nSrc.NextElementState;
end;

(* ##RNC:
## This justifies the case split (the disjunction of all elSuppose 
## or elCase items in direct subblocks) in elPerCasesReasoning.
## The case split is only known after all subblocks are known,
## so this is the last item in its block, not like in the Mizar text.
elPerCases =
 element elPerCases {
   elProposition, Inference
 }
*)
procedure MPrepParser.Parse_PerCasesJustification;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elPerCases);
 lItm := gPrBlockPtr^.CreateItem(itPerCases);
 nSrc.NextElementState;
 Parse_UnquotableProposition(propPerCasesJustification);
 Parse_Justification;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## Reasoning per cases. It only contains elCaseBlock or 
## elSupposeBlock subblocks, with the exception of the mandatory 
## last elPerCases justifying the case split.
## Direct and diffuse versions are possible
## (this depends on the kind of its parent block).
## The block thesis is printed for proofs in the beginning and
## for diffuse reasoning in the end.
elPerCasesReasoning =
 element elPerCasesReasoning {
   Position,
   (  ( elBlockThesis, ( elCaseBlock+ | elSupposeBlock+ ),
        elPerCases, elThesis, elEndPosition  )
   |  ( ( elCaseBlock+ | elSupposeBlock+ ),
        elPerCases, elEndPosition, elBlockThesis ) )
 }
*)
procedure MPrepParser.Parse_CaseList(fDiffuse:boolean);
begin
 XMLASSERT( nSrc.nElKind = elPerCasesReasoning);
 gPrBlockPtr^.CreateBlock(blCaseList);
 Parse_BlockPositionAndLabel;
 nSrc.NextElementState;
 if (not fDiffuse) then Parse_BlockThesis;
 while (nSrc.nState <> eEnd)
       and (nSrc.nElKind in [elCaseBlock,elSupposeBlock]) do
  case nSrc.nElKind of
   elCaseBlock: Parse_CaseOrSupposeBlock(blCase, fDiffuse);
   elSupposeBlock:Parse_CaseOrSupposeBlock(blSuppose, fDiffuse);
  else UnexpectedElem(nSrc.nElKind, errUnexpected,
                      [elCaseBlock,elSupposeBlock]);
  end;
 Parse_PerCasesJustification;
 if (not fDiffuse) then Parse_Thesis;
 nSrc.In_EndPos(CurPos);
 gPrBlockPtr^.ProcessEndPosition;
 if (fDiffuse) then Parse_BlockThesis;
 KillPBlock;
 nSrc.NextElementState;
end; 

(* ##RNC:
## Skeleton items change the InFile.Current thesis, for elProof the
## changed elThesis together with used expansions is printed
## explicitely after them.   
## elPerCasesReasoning is not included here.
SkeletonItem =   
   ( ( elLet | elConclusion | elAssume | elGiven
       | elTake | elTakeAsVar ), elThesis? )
   
## Reasoning is a series of skeleton and auxiliary items,
## finished by optional per cases reasoning.
Reasoning = 
   ( ( SkeletonItem | AuxiliaryItem )*,
     elPerCasesReasoning?, elEndPosition )
*)
procedure MPrepParser.Parse_Reasoning(fDiffuse:boolean);
procedure TryThesis;
begin if (not fDiffuse) then Parse_Thesis; end;
begin
 while (nSrc.nState <> eEnd) and (nSrc.nElKind <> elEndPosition) do
 case nSrc.nElKind of
  elLet: begin Parse_Generalization; TryThesis; end;
  elConclusion: begin Parse_Conclusion; TryThesis; end;
  elAssume: begin Parse_Assumption; TryThesis; end;
  elGiven:  begin Parse_ExistentialAssumption; TryThesis; end;
  elTake: begin Parse_SimpleExemplification; TryThesis;  end;
  elTakeAsVar: begin
                Parse_ExemplificationWithEquality;
                TryThesis;
               end;
  elPerCasesReasoning: Parse_CaseList(fDiffuse);  
  else Parse_AuxiliaryItem;
 end;
 nSrc.In_EndPos(CurPos);
 gPrBlockPtr^.ProcessEndPosition;
end;

(* ##RNC:
## Direct proof of some proposition (which is the proof's thesis).
## Label (atNr) of proof (if any) is label of its thesis, atVid is then
## the identifier nr of this label.
elProof = 
 element elProof {
   attribute atNr { xsd:integer }?,
   attribute atVid { xsd:integer }?,
   Position,
   elBlockThesis, Reasoning 
 }
*)
procedure MPrepParser.Parse_Proof;
begin
 XMLASSERT( nSrc.nElKind = elProof);
 gPrBlockPtr^.CreateBlock(blProof);
 Parse_BlockPositionAndLabel;
 nSrc.NextElementState;
 Parse_BlockThesis;
 Parse_Reasoning(false);
 KillPBlock;
 nSrc.NextElementState;
end; { Parse_Proof }

// This differs from Parse_Proof because Thesis is not known
// ###TODO: stdprep must produce properly labeled proposition
//          from the thesis here - other options are bad
(* ##RNC:
## Diffuse statement - its thesis is reconstructed in the end.
## Label (atNr) and its identifier (atVid) of diffuse statement
## (if any) is label of its thesis.
elNow = 
 element elNow {
   attribute atNr { xsd:integer }?,
   attribute atVid { xsd:integer }?,
   Position,
   Reasoning, elBlockThesis 
 }
*)
procedure MPrepParser.Parse_DiffuseReasoning;
begin
 XMLASSERT( nSrc.nElKind = elNow);
 gPrBlockPtr^.CreateBlock(blDiffuse);
 Parse_BlockPositionAndLabel;
 nSrc.NextElementState;
 Parse_Reasoning(true);
 Parse_BlockThesis;
 KillPBlock;
 nSrc.NextElementState;
end; { Parse_DiffuseReasoning }

// Allows only SimpleJustification or Proof or elSkippedProof
(* ##RNC:   
## Direct justification.   
Justification = ( Inference | elProof | elSkippedProof )
*)
procedure MPrepParser.Parse_Justification;
begin
 case nSrc.nElKind of
 elBy,elFrom,elErrorInf: Parse_SimpleJustification;
 elProof: Parse_Proof;
 elSkippedProof: Parse_AtSignProof;
 else UnexpectedElem(nSrc.nElKind, errUnhandledItemKind,
                     [elBy,elFrom,elErrorInf,elProof,elSkippedProof]);
 end;
end;

(* ##RNC:
## Iterative equality. The optional numbers (atNr) is serial label
## numbering, and original label identifier (atVid).
elIterEquality =
 element elIterEquality {
   attribute atNr { xsd:integer }?,
   attribute atVid { xsd:integer }?,
   Position,
   Term, elIterStep+
 }
*)
procedure MPrepParser.Parse_IterativeEquality;
begin
 XMLASSERT( nSrc.nElKind = elIterEquality);
 Parse_LabelAndPosition;
 nSrc.NextElementState;
 Parse_Term;
 gPrBlockPtr^.nCurrItm^.StartIterEquality;
 Parse_IterSteps;
 gPrBlockPtr^.nCurrItm^.FinishIterEquality;
 nSrc.NextElementState;
end;

(* ##RNC:
JustifiedProposition =
   ( elNow | elIterEquality | ( elProposition, Justification ) )
*)
procedure MPrepParser.Parse_PrivateStatement;
var lItm: PrepItemPtr;
begin
 lItm := gPrBlockPtr^.CreateItem(itPrivateStatement);
 case nSrc.nElKind of
  elNow: Parse_DiffuseReasoning;
  elIterEquality: Parse_IterativeEquality;
  elProposition:
   begin
    Parse_QuotableProposition(propNormalLemma);
    Parse_Justification;
   end;
 else UnexpectedElem(nSrc.nElKind, errUnexpected,
                     [elNow, elIterEquality, elProposition]);
 end;
 gPrBlockPtr^.FinishLastItem(lItm);
end;

(* ##RNC:
## Auxiliary items are items which do not change thesis.
AuxiliaryItem =
   ( JustifiedProposition | elConsider |
     elSet | elReconsider | elDefFunc | elDefPred )
*)
procedure MPrepParser.Parse_AuxiliaryItem;
begin
 case nSrc.nElKind of
  elProposition,
  elIterEquality,
  elNow: Parse_PrivateStatement;
  elConsider: Parse_Choice;
  elSet: Parse_ConstantDefinition;
  elReconsider: Parse_Reconsidering;
  elDefFunc: Parse_DefFunc;
  elDefPred: Parse_DefPred;
 else UnexpectedElem(nSrc.nElKind, errUnexpected,
                     [elProposition,elIterEquality,elNow,elConsider,
                      elSet,elReconsider,elDefFunc,elDefPred]);
 end;
end;

// ##TODO: Parser will have to be modified to output canceled thms
// ###TODO: add the number for proper numbering
(* ##RNC:
elSection =
 element elSection { empty }
*)
procedure MPrepParser.Parse_Section;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elSection );
 lItm := gPrBlockPtr^.CreateItem(itSection);
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.AcceptEndState; nSrc.NextElementState;
end;

// ##NOTE: This is a bit redundant in preparator. The variables
//         must NOT be handled as FixedVariables.
(* ##RNC:
## Reservation of a new variable for a type.
## The type may optionally have presentational info about
## the variable (atVid) inside.
elReservation =
 element elReservation {
   element elIdent { attribute atVid { xsd:integer } }+,
   elTyp
  }
*)
procedure MPrepParser.Parse_Reservation;
var lItm: PrepItemPtr; lStr: string;
    lNr,ec: integer;
begin
 XMLASSERT( nSrc.nElKind = elReservation);
 lItm := gPrBlockPtr^.CreateItem(itReservation);
 nSrc.NextElementState;
// ##TODO: keep multiple segments and multiple identifiers?
 lItm^.StartReservationSegment;
 while nSrc.nElKind = elIdent do
 begin
  InFile.Current.Kind:= 'I';
  if nSrc.GetOptAttr( atVId, lStr) then Val( lStr, lNr, ec)
   else lNr:= 0;
  InFile.Current.Nr:= lNr;
  lItm^.ProcessReservationIdentifier;
  nSrc.NextElementState;
 end;
 Parse_TypeExpr;
 lItm^.FinishReservationSegment;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;     

// ##TODO: This could be made a bit compatible with elTheorem
(* ##RNC:
## Theorem as a proposition with justification.
elJustifiedTheorem = 
 element elJustifiedTheorem {
   elProposition, Justification
 }
*)
procedure MPrepParser.Parse_Theorem;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elJustifiedTheorem);
 lItm := gPrBlockPtr^.CreateItem(itTheorem);
 nSrc.NextElementState;
 Parse_QuotableProposition(propTheorem);
 lItm^.FinishTheoremBody;
 Parse_Justification;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## Theorems created from definitions are now printed
## as separate top-level items after definitional blocks,
## atConstrKind and atConstrNr determine the defined constructor.
## If they do not appear, this is a canceled (verum) deftheorem.   
elDefTheorem = 
 element elDefTheorem {
   ( attribute atConstrKind { 'M' | 'V' | 'R' | 'K' },
     attribute atConstrNr { xsd:integer})?,  
     elProposition }
*)
procedure MPrepParser.Parse_DefinitionalTheorem;
var lItm: PrepItemPtr; lStr: string;
begin
 XMLASSERT( nSrc.nElKind = elDefTheorem);
 InFile.Current.Kind:= ikError;
 if nSrc.GetOptAttr( atConstrKind, lStr) then
  nSrc.GetLexemAttrs( atConstrKind, atConstrNr, InFile.Current);
 lItm := gPrBlockPtr^.CreateItem(itDefTheorem);
 nSrc.NextElementState;
 Parse_QuotableProposition(propDefTheorem);
 lItm^.FinishTheoremBody;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

const PropertyPropositionKind: array[PropertyKind] of PropositionKind =
       (
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
        propUnexpectedProperty		// Abstractness has no proposition
       );

(* ##RNC:
## A property of a constructor, the proposition expreesing it,
## and its justification.
elJustifiedProperty =
 element elJustifiedProperty {
   Property, elProposition, Justification
 }
*)
procedure MPrepParser.Parse_OneProperty;
var lItm: PrepItemPtr; lPropertyKind : PropertyKind;
begin
 XMLASSERT( nSrc.nElKind = elJustifiedProperty);
 nSrc.NextElementState;
 lPropertyKind:= XmlElem2Prop( nSrc.nElKind);
 lItm := gPrBlockPtr^.CreateItem(itProperty);
 nSrc.AcceptEndState; nSrc.NextElementState;
 Parse_UnquotableProposition(PropertyPropositionKind[lPropertyKind]);
 lItm^.FinishPropertyBody;
 Parse_Justification;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

// ##TODO: find out what Y0 means
function CorrPropKind( aEl:TXMLElemKind): PropositionKind;
begin
 case aEl of
  elUnknownCorrCond: 	CorrPropKind:= propCorrCondUnknown;        
  elCoherence: 		CorrPropKind:= propCorrCondCoherence;      
  elCompatibility: 	CorrPropKind:= propCorrCondCompatibility;  
  elConsistency: 	CorrPropKind:= propCorrCondConsistency;    
  elExistence: 		CorrPropKind:= propCorrCondCxistence;      
  elUniqueness: 	CorrPropKind:= propCorrCondUniqueness;
  elReducibility: 	CorrPropKind:= propCorrCondReducibility;
 else UnexpectedElem( aEl, errUnexpected, CorrElKinds);
 end;
end;

// The InFile.Current.Nr in StartCorrCond is the condition number
(* ##RNC:
## The possible correctness conditions are following.
## They can either be only stated in the elCorrectness,
## which conjugates them and proves them all, or come
## separately as a proposition with a justification.
CorrectnessCondition =
   ( elUnknownCorrCond | elCoherence | elCompatibility |
     elConsistency | elExistence | elUniqueness | elReducibility )
   
elUnknownCorrCond = 
 element elUnknownCorrCond { Formula | ( elProposition, Justification ) }
elCoherence =
 element elCoherence { Formula | ( elProposition, Justification ) }
elCompatibility = 
 element elCompatibility { Formula | ( elProposition, Justification ) }
elConsistency = 
 element elConsistency { Formula | ( elProposition, Justification ) }
elExistence = 
 element elExistence { Formula | ( elProposition, Justification ) }
elUniqueness = 
 element elUniqueness { Formula | ( elProposition, Justification ) }
elReducibility = 
 element elReducibility { Formula | ( elProposition, Justification ) }
*)
procedure MPrepParser.Parse_CorrCondition;
var lItm: PrepItemPtr; lKind: PropositionKind;
begin
 XMLASSERT( nSrc.nElKind in CorrElKinds);
 lKind:= CorrPropKind( nSrc.nElKind);
 lItm := gPrBlockPtr^.CreateItem(itCorrCond);
 nSrc.NextElementState;
 Parse_UnquotableProposition( lKind);
 lItm^.FinishCorrConditionBody;
 Parse_Justification;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

// ##NOTE: Printing the added conditions resembles printing thesis
//         after skeleton items. Such flas are not put into the 
//         proposition coll. because they are neither proved nor
//         usable for other items.
(* ##RNC:
## This is a way how to state all correctness conditions in one keyword.
## The relevant conditions are computed by the analyzer and printed
## here, their conjunction has to be justified.
elCorrectness = 
 element elCorrectness {
   CorrectnessCondition*,
   elProposition, Justification
 }
*)
procedure MPrepParser.Parse_Correctness;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elCorrectness);
 lItm := gPrBlockPtr^.CreateItem(itCorrectness);
 nSrc.NextElementState;
 while ( nSrc.nElKind in CorrElKinds) do
 begin
  InFile.Current.Nr:= CorrEl2Nr( nSrc.nElKind);
  lItm^.ProcessAddedConditionNr;
  nSrc.NextElementState;
  Parse_Formula;
  lItm^.ProcessAddedConditionFrm;
  nSrc.NextElementState;
 end;
 Parse_UnquotableProposition(propCorrectnessConjunction);
 lItm^.FinishCorrectnessBody;
 Parse_Justification;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

procedure MPrepParser.Parse_OneDefiniens;
var lItm: PrepItemPtr;
begin
 lItm := gPrBlockPtr^.CreateItem(itDefiniens);
 Parse_Definiens;
 gPrBlockPtr^.FinishLastItem(lItm);
end;

// ##TODO: Parser will have to be modified to output canceled thms
// ###TODO: add the number for proper numbering
(* ##RNC:
## Canceled theorem ( if on top-level), definition or registration
## (if inside such blocks). We should add to this the number
## of canceled items as an attribute.
elCanceled =
 element elCanceled { empty }
*)
procedure MPrepParser.Parse_Canceled;
var lItm: PrepItemPtr;
    lKind:ItemKind;
begin
 XMLASSERT( nSrc.nElKind = elCanceled);
 case nSrc.GetAttr( atKind)[1] of
  ikDefTheoremCanceled: lKind:=itCanceledDef;
  ikTheoremCanceled: lKind:=itCanceled;
  ikSchemeCanceled: lKind:=itCanceledScheme;
 end;
 lItm := gPrBlockPtr^.CreateItem(lKind);
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
 nSrc.NextElementState;
end;

(* ##RNC:
## Declaration of a scheme functor, possibly with its identifier number.
elSchemeFuncDecl =
 element elSchemeFuncDecl {
   attribute atNr { xsd:integer},
   attribute atVid { xsd:integer}?,
   elArgTypes, elTyp
 }
   
## Declaration of a scheme predicate, possibly with its identifier number.
elSchemePredDecl =   
 element elSchemePredDecl {
   attribute atNr { xsd:integer},
   attribute atVid { xsd:integer}?,
   elArgTypes
 }
*)
procedure MPrepParser.Parse_SchemeTypes;
var lItm: PrepItemPtr;
begin
 lItm := gPrBlockPtr^.CreateItem(itSchemeTypes);
 while (nSrc.nState <> eEnd)
       and (nSrc.nElKind in [elSchemeFuncDecl,elSchemePredDecl]) do
  case nSrc.nElKind of
   elSchemeFuncDecl:
    begin
     nSrc.NextElementState;
     XMLASSERT( nSrc.nElKind = elArgTypes);
     nSrc.NextElementState;
     Parse_Types;
     nSrc.NextElementState;
     Parse_TypeExpr;
     lItm^.FinishSchFuncSegment;
     nSrc.NextElementState;
    end;
   elSchemePredDecl:
    begin
     nSrc.NextElementState;
     XMLASSERT( nSrc.nElKind = elArgTypes);
     nSrc.NextElementState;
     Parse_Types;
     lItm^.FinishSchPredSegment;
     nSrc.AcceptEndState; nSrc.NextElementState;
    end;
  end;
 gPrBlockPtr^.FinishLastItem(lItm);
end;

// Scheme thesis (unlike proof thesis) is not an exportable
// result of the scheme block.
// ###TODO: add position
// ##TODO: the name FinishSchemeThesis is compatible with oldprep,
//         but st. like FinishSchemeStatementBody could be better
procedure MPrepParser.Parse_SchemeStatement;
var lItm: PrepItemPtr;
begin
 lItm := gPrBlockPtr^.CreateItem(itSchemeStatement);
 Parse_UnquotableProposition(propSchemeThesis);
 lItm^.FinishSchemeThesis;
 Parse_Justification;  // only ikMscAtProof or ikBlcProof now allowed
 gPrBlockPtr^.FinishLastItem(lItm);
end;

// This is like Parse_QuotablePropositions, but
// the Start and Finish handlers differ
procedure MPrepParser.Parse_SchemePremises;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elSchemePremises);
 lItm := gPrBlockPtr^.CreateItem(itSchemePremises);
 nSrc.NextElementState;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind = elProposition) do
 begin
  lItm^.StartSchemePremise;
  Parse_QuotableProposition(propSchemePremise);
  lItm^.FinishSchemePremise;
 end;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end; 

// The SchemeBlock also keeps the number of the scheme,
// and optionally its identifier (atVid).
// The block label must always be zero - the block has no normal
// thesis.
(* ##RNC:
## Scheme blocks are used for declaring the types of second-order
## variables appearing in a scheme, and for its justification.
## This could be a bit unified with elScheme later.
## atSchemeNr is its serial nr in the article, while atVid is
## its identifier number.   
elSchemeBlock =   
 element elSchemeBlock {
   attribute atSchemeNr { xsd:integer },
   attribute atVid { xsd:integer }?,
   Position,
   ( elSchemeFuncDecl | elSchemePredDecl )*,
   element elSchemePremises { elProposition* },
   elProposition, Justification,
   elEndPosition
   }
*)
procedure MPrepParser.Parse_Scheme;
begin
 XMLASSERT( nSrc.nElKind = elSchemeBlock);
 gPrBlockPtr^.CreateBlock(blPublicScheme);
 Parse_BlockPositionAndLabel;
 InFile.Current.Nr:= nSrc.GetIntAttr( atSchemeNr);
 gPrBlockPtr^.ProcessSchemeLabel;
 nSrc.NextElementState;
 Parse_SchemeTypes;
 Parse_SchemePremises;
 Parse_SchemeStatement;
 
 nSrc.In_EndPos(CurPos);
 gPrBlockPtr^.ProcessEndPosition;
 KillPBlock;
 nSrc.NextElementState;
end; { Parse_Scheme }

// ##TODO: The insertion into the global tables should rather
//         be done by the handler, but it needs to use the ConstrObjs
//         instead of the correl stuff.
// ###TODO: we now pass the ConstrPtr - switch to above mentioned
procedure MPrepParser.Parse_OneConstructor;
var lItm: PrepItemPtr;
begin
 lItm := gPrBlockPtr^.CreateItem(itConstructor);
 nLastConstructor := nSrc.In_Constructor1;
 Constr[ nLastConstructor^.fConstrKind].Insert( nLastConstructor);
 gPrBlockPtr^.ProcessConstructor;
 gPrBlockPtr^.FinishLastItem(lItm);
end;

function DefiniensItem(fSym:char; lRedef,lExpand: boolean): ItemKind;
begin
 if lExpand then
 begin
  Mizassert(errUnexpected, fSym = 'M');
  DefiniensItem:= itDefExpandMode; exit;
 end;
 if lRedef then
  case fSym of
   'V': DefiniensItem:= itRedefPrAttr;
   'M': DefiniensItem:= itRedefMode;
   'R': DefiniensItem:= itRedefPred;
   'K': DefiniensItem:= itRedefFunc;
  else UnexpectedKind(fSym, errUnexpected, ['V','M','R','K']);
  end
 else
  case fSym of
   'V': DefiniensItem:= itDefPrAttr;
   'M': DefiniensItem:= itDefMode;
   'R': DefiniensItem:= itDefPred;
   'K': DefiniensItem:= itDefFunc;
   'G': DefiniensItem:= itDefStruct;
  else UnexpectedKind(fSym, errUnexpected, ['V','M','R','K','G']);
  end;
end;

// ##NOTE: we could produce block for this - it depends on
//         one's Blocks & Items philosophy.
// ##TODO: we have to take special care of structures here, because
//         of the nonsesical correctness for defstruct. Fix it.
(* ##RNC:
## Definition of a functor, predicate, mode, attribute or structure.
## with optional label, properties and correctness conditions.
## Sometimes no constructor is created (e.g. for expandable modes).   
## The second optional form creating three or more constructors
## is for structure definitions, which define the aggregate functor,
## the structure mode, the strict attribute and zero or more selectors,
## and create existential registration for the strict attribute.
## If any definientia and definitional theorems are created,
## they follow immediately after the enclosing definitional block
## (this might be changed in the future).      
## Number, position, and identifier number of the definiens 
## (atVid) can be optionally given.
elDefinition = 
 element elDefinition {
   attribute atKind { 'M' | 'V' | 'R' | 'K' | 'G' },
   attribute atRedefinition { xsd:boolean }?,
   attribute atExpandable { xsd:boolean }?,
   ( attribute atNr { xsd:integer }, Position )?,
   attribute atVid { xsd:integer }?,
   ( ( CorrectnessCondition*, elCorrectness?,
       elJustifiedProperty*, elConstructor?, elPattern? )
   | ( elConstructor, elConstructor, elConstructor+,
       elRegistration, CorrectnessCondition*,
       elCorrectness?, elPattern+ ))
 }
*)
procedure MPrepParser.Parse_OneDefinition;
var
 lKind: ItemKind; lItm: PrepItemPtr; ec:integer;
 lStr:string; lExpand,lRedef:boolean;
 lPatt: PatternPtr;
begin
 XMLASSERT( nSrc.nElKind = elDefinition);
 lExpand:= false; lRedef:= false;
 if nSrc.GetOptAttr( atRedefinition, lStr) then lRedef:= lStr = 'true';  
 if nSrc.GetOptAttr( atExpandable, lStr) then lExpand:= lStr = 'true';
 lKind := DefiniensItem(nSrc. GetAttr( atKind)[1], lRedef, lExpand);
 lItm := gPrBlockPtr^.CreateItem(lKind);
 if nSrc.GetOptAttr( atNr, lStr) then
 begin
  Val( lStr, InFile.Current.Nr, ec);
  nSrc.GetPosAttrs( CurPos);
  lItm^.ProcessDefiniensLabel;
 end;
 nSrc.NextElementState;
 if lKind = itDefStruct then
 begin
  while (nSrc.nState <> eEnd) and (nSrc.nElKind = elConstructor) do
   Parse_OneConstructor;
  Parse_OneCluster;
  while (nSrc.nState <> eEnd) and (nSrc.nElKind in CorrElKinds) do
   Parse_CorrCondition;
  if (nSrc.nElKind = elCorrectness) then Parse_Correctness;
  while (nSrc.nState <> eEnd) and (nSrc.nElKind = elPattern) do
  begin lPatt:=nSrc.In_Pattern; dispose(lPatt, Done); end;
 end
 else begin
  while (nSrc.nState <> eEnd) and (nSrc.nElKind in CorrElKinds) do
   Parse_CorrCondition;
  if (nSrc.nElKind = elCorrectness) then Parse_Correctness;
  while (nSrc.nState <> eEnd) and (nSrc.nElKind = elJustifiedProperty) do
   Parse_OneProperty;
  if (nSrc.nState <> eEnd) and (nSrc.nElKind = elConstructor) then
   Parse_OneConstructor;
  if (nSrc.nState <> eEnd) and (nSrc.nElKind = elPattern) then
  begin lPatt:=nSrc.In_Pattern; dispose(lPatt, Done); end;
 end;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

// The block label must always be zero - the block has no normal
// thesis. The definitional theorems are given after the block.
// ##TODO: the anlyzer should be changed, to split the
//         definitional-block if more than one def occurs there
(* ##RNC:
## A block of one or more (possibly canceled) (re)definitions,
## possibly with assumptions. If any definientia and
## definitional theorems are created, they follow immediately
## after this block.
elDefinitionBlock =
 element elDefinitionBlock {
   Position,
   ( elLet | elAssume | elGiven | AuxiliaryItem |
     elCanceled | elDefinition )*,
   elEndPosition
 }
*)
procedure MPrepParser.Parse_Definition;
begin
 XMLASSERT( nSrc.nElKind = elDefinitionBlock);
 gPrBlockPtr^.CreateBlock(blDefinition);
 Parse_BlockPositionAndLabel;
 nSrc.NextElementState;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind <> elEndPosition) do
 case nSrc.nElKind of
  elLet: Parse_Generalization;
  elAssume: Parse_Assumption;
  elGiven: Parse_ExistentialAssumption;
  elCanceled: Parse_Canceled;
  elDefinition:  Parse_OneDefinition;
 else Parse_AuxiliaryItem;
 end;
 nSrc.In_EndPos( CurPos);
 gPrBlockPtr^.ProcessEndPosition;
 KillPBlock;
 nSrc.NextElementState;
end;

function ClusterItem(aEl:TXMLElemKind): ItemKind;
begin
 case aEl of
  elRCluster: ClusterItem := itExistentialCluster;
  elCCluster: ClusterItem := itConditionalCluster;
  elFCluster: ClusterItem := itFunctorCluster;
  else UnexpectedElem(aEl, errUnexpected, ClusterElKinds);
 end;
end;

// ##TODO: should not we enforce at least one correctness cond. here?
//         - No, this is also used inside structure definitions, where
//           no justification is supplied.
(* ##RNC:
## One justified cluster registration. The correctness conditions
## could be made more specific for each.
elRegistration =
 element elRegistration {
   ( elRCluster | elFCluster | elCCluster ),
   CorrectnessCondition*, elCorrectness?
 }
*)
procedure MPrepParser.Parse_OneCluster;
var lItm: PrepItemPtr; lKind: ItemKind;
begin
 XMLASSERT( nSrc.nElKind = elRegistration);
 nSrc.NextElementState;
 lKind := ClusterItem( nSrc.nElKind );
 lItm := gPrBlockPtr^.CreateItem(lKind);
 Parse_DefCluster;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind in CorrElKinds) do
  Parse_CorrCondition;
 if (nSrc.nElKind = elCorrectness) then Parse_Correctness;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## One justified identification registration. The correctness conditions
## could be made more specific.
elIdentifyRegistration =
 element elIdentifyRegistration {
   elIdentify, CorrectnessCondition*, elCorrectness?
 }
*)
procedure MPrepParser.Parse_OneIdentify;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elIdentifyRegistration);
 nSrc.NextElementState;
 lItm := gPrBlockPtr^.CreateItem(itIdentify);
 DisposeIfAssigned(nLastIdentify);
// if nSrc.nElKind = elIdentify then
//  begin nSrc.NextElementState;
//    if nSrc.nElKind = elErrorIdentify then
//     begin nSrc.AcceptEndState; nSrc.AcceptEndState;
//      nSrc.NextElementState;
//     end;
//  end
// else
 nLastIdentify:=nSrc.In_Identify;
 gPrBlockPtr^.ProcessIdentify;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind in CorrElKinds) do
  Parse_CorrCondition;
 if (nSrc.nElKind = elCorrectness) then Parse_Correctness;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

procedure MPrepParser.Parse_OneReduction;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elReductionRegistration);
 nSrc.NextElementState;
 lItm := gPrBlockPtr^.CreateItem(itReduction);
 DisposeIfAssigned(nLastReduction);
// if nSrc.nElKind = elReduction then
//  begin nSrc.NextElementState;
//    if nSrc.nElKind = elErrorReduction then
//     begin nSrc.AcceptEndState; nSrc.AcceptEndState;
//      nSrc.NextElementState;
//     end;
//  end
// else
 nLastReduction:=nSrc.In_Reduction;
 {$IFDEF MDEBUG}
 InfoNewLine;
 InfoString('Reduction Start'); InfoNewLine;
 if nLastReduction <> nil then
 begin
  InfoTerm(nLastReduction^.nTerms[0]); InfoNewLine;
  InfoTerm(nLastReduction^.nTerms[1]); InfoNewLine;
 end;
 InfoString('Reduction End'); InfoNewLine;
 {$ENDIF}
 gPrBlockPtr^.ProcessReduction;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind in CorrElKinds) do
  Parse_CorrCondition;
 if (nSrc.nElKind = elCorrectness) then Parse_Correctness;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

procedure MPrepParser.Parse_OnePropertyRegistration;
var lItm: PrepItemPtr;
begin
 XMLASSERT( nSrc.nElKind = elPropertyRegistration);
 nSrc.NextElementState;
 lItm := gPrBlockPtr^.CreateItem(itPropertyReg);
 DisposeIfAssigned(nLastProperty);
 nLastProperty:=nSrc.In_PropertyReg;
 gPrBlockPtr^.ProcessProperty;
 Parse_UnquotableProposition(propSethoodProperty);
 Parse_Justification;
 gPrBlockPtr^.FinishLastItem(lItm);
 nSrc.NextElementState;
end;

(* ##RNC:
## Block of cluster registrations.
##
elRegistrationBlock =
 element elRegistrationBlock {
   Position,
   ( elLet | AuxiliaryItem | elRegistration | elIdentifyRegistration | elCanceled )+,
   elEndPosition
 }
*)
procedure MPrepParser.Parse_Registration;
begin
 with nSrc do
 begin
  XMLASSERT( nElKind = elRegistrationBlock);
  gPrBlockPtr^.CreateBlock(blRegistration);
  Parse_BlockPositionAndLabel;
  nSrc.NextElementState;
  while nElKind <> elEndPosition do
   case nElKind of
    elLet: Parse_Generalization;
    elCanceled: Parse_Canceled;
    elRegistration: Parse_OneCluster;
    elIdentifyRegistration: Parse_OneIdentify;
    elReductionRegistration: Parse_OneReduction;
    elPropertyRegistration: Parse_OnePropertyRegistration;
   else Parse_AuxiliaryItem;
   end;
  In_EndPos(CurPos);
  gPrBlockPtr^.ProcessEndPosition;
  KillPBlock;
  nSrc.NextElementState;
 end;
end;

(*const NotationSyms = [
 ikItmDefMode,
 ikItmDefPred,
 ikItmDefPrAttr,
 ikItmDefFunc
                     ];
*)

// ##TODO: print the patterns?
(* ##RNC:
## Block of synonyms or antonyms. The patterns are
## semantically irrelevant and are not printed yet - fix this.
elNotationBlock =
 element elNotationBlock {
   Position,
   ( elLet | AuxiliaryItem | elPattern)*,
   elEndPosition
 }
*)
procedure MPrepParser.Parse_Notation;
var lPatt: PatternPtr;
begin
 XMLASSERT( nSrc.nElKind = elNotationBlock);
 gPrBlockPtr^.CreateBlock(blNotation);
 Parse_BlockPositionAndLabel;
 nSrc.NextElementState;
 while (nSrc.nState <> eEnd) and (nSrc.nElKind <> elEndPosition) do
 case nSrc.nElKind of
  elLet: Parse_Generalization;
  elPattern: begin lPatt:=nSrc.In_Pattern; dispose(lPatt, Done); end;
 else Parse_AuxiliaryItem;
//  if InFile.Current.Kind in NotationSyms then nSrc.InWord
 end;
 nSrc.In_EndPos( CurPos);
 gPrBlockPtr^.ProcessEndPosition;
 KillPBlock;
 nSrc.NextElementState;
end;

end.
