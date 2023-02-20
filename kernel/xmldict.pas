(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// Enumerated types for XML parsing and their translation
// to and from strings.

// the lists can be updated from Mizar.rnc (see iocorrel for how to create it)
// by the following commands:
// perl -e 'local $/;$_=<>; while(m/\b(el[A-Z]\w*)/g) {$h{$1}=()}; foreach $k (sort keys %h) {print "$k,\n"}' Mizar.rnc
// perl -e 'local $/;$_=<>; while(m/\b(at[A-Z]\w*)/g) {$h{$1}=()}; foreach $k (sort keys %h) {print "$k,\n"}' Mizar.rnc

// The names of elements are created by deleting the initial 'el',
// names of attributes by deleting the initial 'at' and downcasing.
// This needs to be done for the final version of Mizar.rnc too - e.g.:
// perl -ne 's/\bel([A-Z])/$1/g; s/\bat([A-Z]\w*)/lc($1)/eg; print $_' Mizar.rnc


unit xmldict;

interface

uses mobjects,builtin,syntax,mscanner;

// known (and only allowed) XML elements
type TXMLElemKind = (
 elUnknown,
 
 elAbstractness,
 elAdjective,
 elAnd,
 elAsymmetry,
 elArgTypes,
 elArticle,
 elArticleID,
 elAssociativity,
 elAssume,
 elBlockThesis,
 elBy,
 elByExplanations,
 elCCluster,
 elCanceled,
 elCase,
 elCaseBlock,
 elChoice,
 elCluster,
 elCoherence,
 elCommutativity,
 elCompatibility,
 elComplexNr,
 elConclusion,
 elConnectedness,
 elConsider,
 elConsistency,
 elConst,
 elConstrCount,
 elConstrCounts,
 elConstrDef,
 elConstructor,
 elConstructors,
 elCorrectness,
 elDefFunc,
 elDefMeaning,
 elDefPred,
 elDefTheorem,
 elDefiniens,
 elDefinientia,
 elDefinition,
 elDefinitionBlock,
 elDirective,
 elEndPosition,
 elEnviron,
 elEqArgs,
 elErrorCluster,
 elErrorFrm,
 elErrorIdentify,
 elErrorInf,
 elErrorReduction,
 elErrorTrm,
 elEssentials,
 elExistence,
 elExpansion,
 elFCluster,
 elField,
 elFields,
 elFlex,		     
 elFor,
 elFormat,
 elFormats,
 elFraenkel,
 elFreeVar,
 elFrom,
 elFromExplanations,
 elFunc,
 elFuncInstance,
 elGiven,
 elHereby,
 elIdempotence,
 elIdent,
 elIdentify,
 elIdentifyRegistration,
 elIdentifyRegistrations,
 elIdentifyWithExp,
 elInfConst,
 elInt,
 elInvolutiveness,
 elIrreflexivity,
 elIs,
 elIt,
 elIterEquality,
 elIterStep,
 elJustifiedProperty,
 elJustifiedTheorem,
 elLambdaVar,
 elLet,
 elLocusVar,
 elMonomial,
 elNot,
 elNotationBlock,
 elNotations,
 elNow,
 elNum,
 elPair,
 elParser,
 elPartialDef,
 elPattern,
 elPerCases,
 elPerCasesReasoning,
 elPolyEval,
 elPolynomial,
 elPos,
 elPoweredVar,
 elPred,
 elPredInstance,
 elPriority,
 elPrivFunc,
 elPrivPred,
 elProjectivity,
 elProof,
 elProperties,
 elProperty,
 elPropertyRegistration,
 elProposition,
 elQuaTrm,
 elRCluster,
 elRationalNr,
 elReconsider,
 elReducibility,
 elReduction,
 elReductionRegistration,
 elReductionRegistrations,
 elRef,
 elReflexivity,
 elRegistration,
 elRegistrationBlock,
 elRegistrations,
 elRequirement,
 elRequirements,
 elReservation,
 elSection,
 elScheme,
 elSchemeBlock,
 elSchemeFuncDecl,
 elSchemeInstantiation,
 elSchemePredDecl,
 elSchemePremises,
 elSchemes,
 elSet,
 elSethood,
 elSignature,
 elSignatureWithCounts,
 elSkippedProof,
 elStructLoci,
 elSuppose,
 elSupposeBlock,
 elSymbol,
 elSymbolCount,
 elSymbols,
 elSymmetry,
 elTake,
 elTakeAsVar,
 elTheorem,
 elTheorems,
 elThesis,
 elThesisExpansions,
 elTransitivity,
 elTyp,
 elUnexpectedProp,
 elUniqueness,
 elUnknownCorrCond,
 elVar,
 elVerum,
 elVisible,
 elVocabularies,
 elVocabulary );
 
// known XML attributes
type TXMLAttrKind = (
 atUnknown,
 
 atAbsNr,
 atAbsRedefNr,
 atAggregBase,
 atAid,
 atAntonymic,
 atArgNr,
 atArticleNr,
 atCol,
 atConstrKind,
 atConstrNr,
 atDefNr,
 atDenominator,
 atExpandable,
 atExponent,
 atFormatNr,
 atInfinitive,
 atInstNr,
 atKind,
 atLeftArgNr,
 atLine,
 atLinked,
 atMizfiles,
 atName,
 atNr,
 atNumerator,
 atPid,
 atPriority,
 atPropertyArg1,
 atPropertyArg2,
 atRedefAid,
 atRedefNr,
 atRedefinition,
 atRelNr,
 atReqName,
 atRightSymbolNr,
 atSchemeNr,
 atStructModeAggrNr,
 atSuperfluous,
 atSymbolNr,
 atValue,
 atVid,
 atX,
 atY );

type TXMLElemSet = set of TXMLElemKind;
type TXMLAttrSet = set of TXMLAttrKind;

const XMLElemName: array[TXMLElemKind] of string = ( 
 'Unknown',
 
 'Abstractness',
 'Adjective',
 'And',
 'Asymmetry',
 'ArgTypes',
 'Article',
 'ArticleID',
 'Associativity',
 'Assume',
 'BlockThesis',
 'By',
 'ByExplanations',
 'CCluster',
 'Canceled',
 'Case',
 'CaseBlock',
 'Choice',
 'Cluster',
 'Coherence',
 'Commutativity',
 'Compatibility',
 'ComplexNr',
 'Conclusion',
 'Connectedness',
 'Consider',
 'Consistency',
 'Const',
 'ConstrCount',
 'ConstrCounts',
 'ConstrDef',
 'Constructor',
 'Constructors',
 'Correctness',
 'DefFunc',
 'DefMeaning',
 'DefPred',
 'DefTheorem',
 'Definiens',
 'Definientia',
 'Definition',
 'DefinitionBlock',
 'Directive',
 'EndPosition',
 'Environ',
 'EqArgs',
 'ErrorCluster',
 'ErrorFrm',
 'ErrorIdentify',
 'ErrorInf',
 'ErrorReduction',
 'ErrorTrm',
 'Essentials',
 'Existence',
 'Expansion',
 'FCluster',
 'Field',
 'Fields',
 'FlexFrm',
 'For',
 'Format',
 'Formats',
 'Fraenkel',
 'FreeVar',
 'From',
 'FromExplanations',
 'Func',
 'FuncInstance',
 'Given',
 'Hereby',
 'Idempotence',
 'Ident',
 'Identify',
 'IdentifyRegistration',
 'IdentifyRegistrations',
 'IdentifyWithExp',
 'InfConst',
 'Int',
 'Involutiveness',
 'Irreflexivity',
 'Is',
 'It',
 'IterEquality',
 'IterStep',
 'JustifiedProperty',
 'JustifiedTheorem',
 'LambdaVar',
 'Let',
 'LocusVar',
 'Monomial',
 'Not',
 'NotationBlock',
 'Notations',
 'Now',
 'Num',
 'Pair',
 'Parser',
 'PartialDef',
 'Pattern',
 'PerCases',
 'PerCasesReasoning',
 'PolyEval',
 'Polynomial',
 'Pos',
 'PoweredVar',
 'Pred',
 'PredInstance',
 'Priority',
 'PrivFunc',
 'PrivPred',
 'Projectivity',
 'Proof',
 'Properties',
 'Property',
 'PropertyRegistration',
 'Proposition',
 'QuaTrm',
 'RCluster',
 'RationalNr',
 'Reconsider',
 'Reducibility',
 'Reduction',
 'ReductionRegistration',
 'ReductionRegistrations',
 'Ref',
 'Reflexivity',
 'Registration',
 'RegistrationBlock',
 'Registrations',
 'Requirement',
 'Requirements',
 'Reservation',
 'Section',
 'Scheme',
 'SchemeBlock',
 'SchemeFuncDecl',
 'SchemeInstantiation',
 'SchemePredDecl',
 'SchemePremises',
 'Schemes',
 'Set',
 'Sethood',
 'Signature',
 'SignatureWithCounts',
 'SkippedProof',
 'StructLoci',
 'Suppose',
 'SupposeBlock',
 'Symbol',
 'SymbolCount',
 'Symbols',
 'Symmetry',
 'Take',
 'TakeAsVar',
 'Theorem',
 'Theorems',
 'Thesis',
 'ThesisExpansions',
 'Transitivity',
 'Typ',
 'UnexpectedProp',
 'Uniqueness',
 'UnknownCorrCond',
 'Var',
 'Verum',
 'Visible',
 'Vocabularies',
 'Vocabulary' );

const XMLAttrName: array[TXMLAttrKind] of string = (
 'unknown',

 'absnr',
 'absredefnr',
 'aggregbase',
 'aid',
 'antonymic',
 'argnr',
 'articlenr',
 'col',
 'constrkind',
 'constrnr',
 'defnr',
 'denominator',
 'expandable',
 'exponent',
 'formatnr',
 'infinitive',						    
 'instnr',
 'kind',
 'leftargnr',
 'line',
 'linked',
 'mizfiles',
 'name',
 'nr',
 'numerator',
 'pid',
 'priority',
 'propertyarg1',
 'propertyarg2',
 'redefaid',
 'redefnr',
 'redefinition',
 'relnr',
 'reqname',
 'rightsymbolnr',
 'schemenr',
 'structmodeaggrnr',
 'superfluous',
 'symbolnr',
 'value',
 'vid',
 'x',
 'y' );
 
function Str2XMLElemKind( aStr: string): TXMLElemKind;
function Str2XMLAttrKind( aStr: string): TXMLAttrKind;

const TermElKinds = [
 elLocusVar,
 elVar,
 elConst,
 elChoice,
 elInfConst,
 elFreeVar,
 elLambdaVar,
 elNum,
 elFunc,
 elPrivFunc,
 elFraenkel,
 elQuaTrm,
 elIt,
 elErrorTrm ];

const FrmElKinds = [
 elNot,
 elAnd,
 elFor,
 elPred,
 elPrivPred,
 elIs,
 elFlex,		     
 elVerum,
 elErrorFrm ];

const PropElKinds = [
 elUnexpectedProp,
 elSymmetry,
 elReflexivity,
 elIrreflexivity,
 elAssociativity,
 elTransitivity,
 elCommutativity,
 elConnectedness,
 elAsymmetry,
 elIdempotence,
 elInvolutiveness,
 elProjectivity,
 elAbstractness ];

const CorrElKinds = [
 elUnknownCorrCond,
 elCoherence,
 elCompatibility,
 elConsistency,
 elExistence,
 elUniqueness,
 elReducibility ];

const ClusterElKinds = [
 elRCluster,
 elCCluster,
 elFCluster ];
 
const Nr2CorrEl: array[0..6] of TXMLElemKind = (
 elUnknownCorrCond,
 elCoherence,
 elCompatibility,
 elConsistency,
 elExistence,
 elUniqueness,
 elReducibility );

function CorrEl2Nr( aEl:TXMLElemKind):integer;

const Prop2XmlElem: array[ PropertyKind] of TXMLElemKind = (
 elUnexpectedProp,
 elSymmetry,
 elReflexivity,
 elIrreflexivity,
 elAssociativity,
 elTransitivity,
 elCommutativity,
 elConnectedness,
 elAsymmetry,
 elIdempotence,
 elInvolutiveness,
 elProjectivity,
 elSethood,
 elAbstractness );

// Blocks in parser.
// There does not seem to be a block created in parser for "per cases"
// BlockKind =
//  ( blMain, blDiffuse, blHereby, blProof, blDefinition, blNotation,
//    blRegistration, blCase, blSuppose, blPublicScheme );
const BlK2XmlElem: array[ BlockKind] of TXMLElemKind = (
 elArticle,
 elNow,
 elHereby,
 elProof,
 elDefinitionBlock,
 elNotationBlock,
 elRegistrationBlock,
 elCaseBlock,
 elSupposeBlock,
 elSchemeBlock);

function XmlElem2Prop( aElem: TXMLElemKind): PropertyKind;

const errWrongXMLElement = 7503; // Different XML element expected

implementation
uses errhan;

var ElemLookupTable: MSortedStrList;
var AttrLookupTable: MSortedStrList;

procedure InitLookupTables;
var
 lStrPtr:MStrPtr;
 i: TXMLElemKind;
 j: TXMLAttrKind;
begin
 ElemLookupTable.Init( Ord( High( TXMLElemKind)) + 1);
 AttrLookupTable.Init( Ord( High( TXMLAttrKind)) + 1);
 
 for i:= Low( TXMLElemKind) to High( TXMLElemKind) do
 begin
  lStrPtr:= new( MStrPtr, Init( XMLElemName[ i] ));
  ElemLookupTable.Insert( lStrPtr);
 end;
 
 for j:= Low( TXMLAttrKind) to High( TXMLAttrKind) do
 begin
  lStrPtr:= new( MStrPtr, Init( XMLAttrName[ j] ));
  AttrLookupTable.Insert( lStrPtr);
 end;
end;
 
function Str2XMLElemKind( aStr: string): TXMLElemKind;
var lNr:integer;
begin
 lNr:= ElemLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2XMLElemKind:= TXMLElemKind( lNr)
 else Str2XMLElemKind:= elUnknown;
end;
     
function Str2XMLAttrKind( aStr: string): TXMLAttrKind;
var lNr:integer;
begin
 lNr:= AttrLookupTable.IndexOfStr( aStr);
 if lNr > -1 then
  Str2XMLAttrKind:= TXMLAttrKind( lNr)
 else Str2XMLAttrKind:= atUnknown;
end;

function CorrEl2Nr( aEl:TXMLElemKind):integer;
begin
 case aEl of
  elUnknownCorrCond: 	CorrEl2Nr:= 0;
  elCoherence: 		CorrEl2Nr:= 1;
  elCompatibility: 	CorrEl2Nr:= 2;
  elConsistency: 	CorrEl2Nr:= 3;
  elExistence: 		CorrEl2Nr:= 4;
  elUniqueness: 	CorrEl2Nr:= 5;
  elReducibility: 	CorrEl2Nr:= 6;  
 else Mizassert( errWrongXMLElement, false);
 end;
end;

function XmlElem2Prop( aElem: TXMLElemKind): PropertyKind;
begin
 case aElem of
  elUnexpectedProp: XmlElem2Prop:= Unexpected;
  elSymmetry: XmlElem2Prop:= sySymmetry;
  elReflexivity: XmlElem2Prop:= syReflexivity;
  elIrreflexivity: XmlElem2Prop:= syIrreflexivity;
  elAssociativity: XmlElem2Prop:= syAssociativity;
  elTransitivity: XmlElem2Prop:= syTransitivity;
  elCommutativity: XmlElem2Prop:= syCommutativity;
  elConnectedness: XmlElem2Prop:= syConnectedness;
  elAsymmetry: XmlElem2Prop:= syAsymmetry;
  elIdempotence: XmlElem2Prop:= syIdempotence;
  elInvolutiveness: XmlElem2Prop:= syInvolutiveness;
  elProjectivity: XmlElem2Prop:= syProjectivity;
  elAbstractness: XmlElem2Prop:= syAbstractness;
  elSethood: XmlElem2Prop:= sySetHood;
 else MizAssert( errWrongXMLElement, false);
 end;
end;

begin
 InitLookupTables;
end.
 
