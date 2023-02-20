(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit prepobj;

// This should replace the functionality of preprep.pas,
// stdprep.pas replaces prep.pas

// GENERAL CODE PROPOSITIONS:
// - replace the error numbers everywhere with verbose constants or
//   enumerated type like errUnhandledItemKind here

// HANDLERS:
// The biggest change is that the Start handlers are called automatically
// by CreateItem in a generic procedure StartItem,
// and Finish handlers similarly by KillItem, in a generic procedure
// FinishItem. This should help XML-izing the code later.

interface

uses
 inoutmml, mobjects, errhan, mconsole, correl, lexicon, iocorrel, propcoll
{$IFDEF MDEBUG}
,info,outinfo
{$ENDIF}
;

// prep blocks and items
// changes wrt. syntax.pas:
// itRegularStatement -> itPrivateStatement
// itExemplification -> itSimpleExemplification,itExemplificationWithEquality
// added:  blCaseList, itThesis
// for percases, there is both block and item, the item only justifies the
// case split

type
BlockKind = (
 blMain,
 blDiffuse,
 blProof,
 blDefinition,
 blRegistration,
 blNotation,
 blCase,
 blSuppose,
 blPublicScheme,
 blCaseList// added  
            );

CaseBlKind = blCase .. blSuppose;

const QuotableBlKinds = [blDiffuse .. blProof];

const BlockDebugNames: array[BlockKind] of string = (
 'blMain',
 'blDiffuse',
 'blProof',
 'blDefinition',
 'blRegistration',
 'blNotation',
 'blCase',
 'blSuppose',
 'blPublicScheme',
 'blCaseList'// added  
            );

type

ItemKind = (
// SkeletonItemKind 
 itGeneralization,
 itExistentialAssumption,
 itConclusion,
 itAssumption,
 itSimpleExemplification,// added
 itExemplificationWithEquality,// added
 itPerCases,// improper, does not change thesis
 itCase,// improper, does not change thesis
 itSuppose,// improper, does not change thesis

// AuxilieryItemKind 
 itPrivateStatement,// used to be itRegularStatement
 itChoice,
 itReconsider,
 itPrivFuncDefinition,
 itPrivPredDefinition,
 itConstantDefinition,

// TopItemKind 
 itScheme,
 itTheorem,
 itDefTheorem,
 itReservation,
 itSection,
 itCanceledScheme,
 itCanceled,

// parts of definitions
// DefinitionalItemKind
 itCorrCond,
 itCorrectness,
 itConstructor,
 itProperty,
 itDefiniens,
 itDefExpandMode,
 itDefPrAttr,
 itRedefPrAttr,
 itCanceledDef,
 itDefStruct,
 itDefMode,
 itRedefMode,
 itDefFunc,
 itDefPred,
 itRedefPred,
 itRedefFunc,
 {*itDefSelector, itDefAggregate, itDefSubaggregate,*}
 itFuncSynonym,
 itPredSynonym,
 itPredAntonym,
 itModeSynonym,
 itAttrSynonym,
 itAttrAntonym,
 itIdentify,
 itReduction,
 itPropertyReg,
 itExistentialCluster,
 itConditionalCluster,
 itFunctorCluster,
 
// parts of schemes 
// SchemeItemType 
 itSchemeTypes,
 itSchemeStatement,
 itSchemePremises,
 
// SpecialItemKind 
 itIncorrItem,
 itThesis   // added
                  );

// ItemKind should rather be a union of these, if Pascal allowed 
ProperSkeletonItemKind =
 itGeneralization .. itExemplificationWithEquality;

CaseItemKind =
 itCase           .. itSuppose;

SkeletonItemKind =
 itGeneralization .. itSuppose;

AuxilieryItemKind =
 itPrivateStatement.. itConstantDefinition;

TopItemKind =
 itScheme .. itCanceled;

DefinitionalItemKind = 
 itCorrCond .. itFunctorCluster;

SchemeItemType =
 itSchemeTypes.. itSchemePremises;

SpecialItemKind =
 itIncorrItem .. itThesis;


const CaseFirstItemKind: array[CaseBlKind] of CaseItemKind = (
 itCase,
 itSuppose
  );

// ikBlcCase and ikBlcSuppose introduce blocks, the corresponding
// items must be first in the blocks
const ProperSkeletonTokens: array[ProperSkeletonItemKind] of char = (
 ikItmGeneralization, // itGeneralization,
 ikItmExAssumption, // itExistentialAssumption,
 ikItmConclusion, // itConclusion,
 ikItmAssumption, // itAssumption,
 ikItmSimpleExemplif, // itSimpleExemplification,
 ikItmExemplifWithEq// itExemplificationWithEquality,
);

const ItemDebugNames: array[ItemKind] of string = (
 // SkeletonItemKind
 'itGeneralization',
 'itExistentialAssumption',
 { itExemplification } // specialised to simple and withequality
  'itConclusion',
 'itAssumption',
 'itSimpleExemplification',// added
 'itExemplificationWithEquality',// added
 'itPerCases',
 'itCase',
 'itSuppose',

// AuxilieryItemKind
 'itPrivateStatement',// used to be itRegularStatement
 'itChoice',
 'itReconsider',
 'itPrivFuncDefinition',
 'itPrivPredDefinition',
 'itConstantDefinition',

// TopItemKind
 'itScheme',
 'itTheorem',
 'itDefTheorem',
 'itReservation',
 'itSection',
 'itCanceledScheme',
 'itCanceled',


// parts of definitions
// DefinitionalItemKind
 'itCorrCond',
 'itCorrectness',
 'itConstructor',
 'itProperty',
 'itDefiniens',
 'itDefExpandMode',
 'itDefPrAttr',
 'itRedefPrAttr',
 'itCanceledDef',
 'itDefStruct',
 'itDefMode',
 'itRedefMode',
 'itDefFunc',
 'itDefPred',
 'itRedefPred',
 'itRedefFunc',
 {*itDefSelector, itDefAggregate, itDefSubaggregate,*}
 'itFuncSynonym',
 'itPredSynonym',
 'itPredAntonym',
 'itModeSynonym',
 'itAttrSynonym',
 'itAttrAntonym',
 'itIdentify',
 'itReduction',
 'itPropertyReg',
 'itExistentialCluster',
 'itConditionalCluster',
 'itFunctorCluster',

// parts of schemes
// SchemeItemType
 'itSchemeTypes',
 'itSchemeStatement',
 'itSchemePremises',

// SpecialItemKind
 'itIncorrItem',
 'itThesis'   // added
);

type

 PrepBlockPtr = ^PrepBlockObj;
// StackedObj - pointing to the previous item in block
 PrepItemPtr = ^PrepItemObj;
 PrepItemObj =
  object(StackedObj)
   nBlock   : PrepBlockPtr;
   nItemKind: ItemKind;
   constructor Init(fItemKind:ItemKind; fBlock: PrepBlockPtr);
   destructor Done; virtual;
   procedure StartItem;
   procedure FinishItem;
   // these accessors unfortunately static
   function GetPrevious : PrepItemPtr;
   function  GetBlock : PrepBlockPtr;
   
   // Start handlers corresponding to nItemKind
   
   // SkeletonItemKind 
   procedure StartGeneralization; virtual;
   procedure StartExistentialAssumption; virtual;
   procedure StartConclusion; virtual;
   procedure StartAssumption; virtual;
   procedure StartSimpleExemplification; virtual;
   procedure StartExemplificationWithEquality; virtual;
   procedure StartPerCases; virtual;
   procedure StartCase; virtual;
   procedure StartSuppose; virtual;
   
   // AuxilieryItemKind 
   procedure StartPrivateStatement; virtual;
   procedure StartChoice; virtual;
   procedure StartReconsider; virtual;
   procedure StartPrivFuncDefinition; virtual;
   procedure StartPrivPredDefinition; virtual;
   procedure StartConstantDefinition; virtual;

   // TopItemKind 
   procedure StartScheme; virtual;
   procedure StartTheorem; virtual;
   procedure StartDefTheorem; virtual;
   procedure StartReservation; virtual;
   procedure StartSection; virtual;
   procedure StartCanceled; virtual;

   // parts of definitions
   // DefinitionalItemKind 
   procedure StartCorrCond; virtual;
   procedure StartCorrectness; virtual;
   procedure StartConstructor; virtual;
   procedure StartProperty; virtual;
   procedure StartDefiniens; virtual;
   procedure StartDefExpandMode; virtual;
   procedure StartDefPrAttr; virtual;
   procedure StartRedefPrAttr; virtual;
   procedure StartCanceledDef; virtual;
   procedure StartDefStruct; virtual;
   procedure StartDefMode; virtual;
   procedure StartRedefMode; virtual;
   procedure StartDefFunc; virtual;
   procedure StartDefPred; virtual;
   procedure StartRedefPred; virtual;
   procedure StartRedefFunc; virtual;    
   procedure StartFuncSynonym; virtual;
   procedure StartPredSynonym; virtual;
   procedure StartPredAntonym; virtual;
   procedure StartModeSynonym; virtual;
   procedure StartAttrSynonym; virtual;
   procedure StartAttrAntonym; virtual;
   procedure StartIdentify; virtual;
   procedure StartReduction; virtual;
   procedure StartPropertyReg; virtual;
   procedure StartExistentialCluster; virtual;
   procedure StartConditionalCluster; virtual;
   procedure StartFunctorCluster; virtual;

   // parts of schemes 
   // SchemeItemType 
   procedure StartSchemeTypes; virtual;
   procedure StartSchemeStatement; virtual;
   procedure StartSchemePremises; virtual;
   
   // SpecialItemKind 
   procedure StartIncorrItem; virtual;
   procedure StartThesis; virtual;  
   

// Finish handlers corresponding to nItemKind
   // SkeletonItemKind 
   procedure FinishGeneralization; virtual;
   procedure FinishExistentialAssumption; virtual;
   procedure FinishConclusion; virtual;
   procedure FinishAssumption; virtual;
   procedure FinishSimpleExemplification; virtual;
   procedure FinishExemplificationWithEquality; virtual;
   procedure FinishPerCases; virtual;
   procedure FinishCase; virtual;
   procedure FinishSuppose; virtual;
   
   // AuxilieryItemKind 
   procedure FinishPrivateStatement; virtual;
   procedure FinishChoice; virtual;
   procedure FinishReconsider; virtual;
   procedure FinishPrivFuncDefinition; virtual;
   procedure FinishPrivPredDefinition; virtual;
   procedure FinishConstantDefinition; virtual;

   // TopItemKind 
   procedure FinishScheme; virtual;
   procedure FinishTheorem; virtual;
   procedure FinishDefTheorem; virtual;
   procedure FinishReservation; virtual;
   procedure FinishSection; virtual;
   procedure FinishCanceledScheme; virtual;
   procedure FinishCanceled; virtual;

   // parts of definitions
   // DefinitionalItemKind 
   procedure FinishCorrCond; virtual;
   procedure FinishCorrectness; virtual;
   procedure FinishConstructor; virtual;
   procedure FinishProperty; virtual;
   procedure FinishDefiniens; virtual;
   procedure FinishDefExpandMode; virtual;
   procedure FinishDefPrAttr; virtual;
   procedure FinishRedefPrAttr; virtual;
   procedure FinishCanceledDef; virtual;
   procedure FinishDefAttr; virtual;
   procedure FinishDefStruct; virtual;
   procedure FinishDefMode; virtual;
   procedure FinishRedefMode; virtual;
   procedure FinishDefFunc; virtual;
   procedure FinishDefPred; virtual;
   procedure FinishRedefPred; virtual;
   procedure FinishRedefFunc; virtual;    
   procedure FinishFuncSynonym; virtual;
   procedure FinishPredSynonym; virtual;
   procedure FinishPredAntonym; virtual;
   procedure FinishModeSynonym; virtual;
   procedure FinishAttrSynonym; virtual;
   procedure FinishAttrAntonym; virtual;
   procedure FinishIdentify; virtual;
   procedure FinishReduction; virtual;
   procedure FinishPropertyReg; virtual;
   procedure FinishExistentialCluster; virtual;
   procedure FinishConditionalCluster; virtual;
   procedure FinishFunctorCluster; virtual;
   
   // parts of schemes 
   // SchemeItemType 
   procedure FinishSchemeTypes; virtual;
   procedure FinishSchemeStatement; virtual;
   procedure FinishSchemePremises; virtual;
   
   // SpecialItemKind 
   procedure FinishIncorrItem; virtual;
   procedure FinishThesis; virtual;  

   
// added for schemes
   procedure FinishSchPredSegment; virtual;
   procedure FinishSchFuncSegment; virtual;     
   
   
// Other handlers   
   procedure StartQuotableProposition(fKind: PropositionKind); virtual;
   procedure StartUnquotableProposition(fKind: PropositionKind); virtual;
   procedure StartQuotablePropositions(fKind: PropositionKind); virtual;
   procedure FinishQuotableProposition(fKind: PropositionKind); virtual;
   procedure FinishUnquotableProposition(fKind: PropositionKind); virtual;
   procedure FinishQuotablePropositions(fKind: PropositionKind); virtual;
   procedure FinishReconsidering; virtual;
   procedure StartReconsideredTerm; virtual;
   procedure FinishReconsideredTerm; virtual;

   procedure StartFixedVariables; virtual;
   procedure FinishFixedVariables; virtual;
   procedure StartFixedVariable; virtual;
   procedure FinishFixedVariable; virtual;   

   procedure StartExemplifyingTerm; virtual;
   procedure FinishExemplifyingTerm; virtual;
      
   procedure StartTheoremBody; virtual;
   procedure FinishTheoremBody; virtual;
   procedure FinishPropertyBody; virtual;
   procedure FinishCorrConditionBody; virtual;
   procedure FinishCorrectnessBody; virtual;
   procedure ProcessAddedConditionFrm; virtual;
   procedure ProcessAddedConditionNr; virtual;
   
   procedure StartReservationSegment; virtual;
   procedure ProcessReservationIdentifier; virtual;
   procedure FinishReservationSegment; virtual;
   
   procedure ProcessLabel; virtual;
   procedure ProcessPosition; virtual;
   procedure ProcessDefiniensLabel; virtual;
   procedure StartSchemePremise; virtual;
   procedure FinishSchemePremise; virtual;
   procedure FinishSchemeThesis; virtual;

   { J u s t i f i c a t i o n }
   
   procedure StartIterEquality; virtual;
   procedure FinishIterEquality; virtual;
   procedure StartSimpleJustification; virtual;
   procedure FinishSimpleJustification; virtual;
   procedure ProcessAtSignProof; virtual;
   
  end;
 
// StackedObj - Previous points to its parent block; 
 PrepBlockObj =
  object(StackedObj)
    nCurrItm  : PrepItemPtr;  // the last item in this block
    nBlockKind: BlockKind;
   constructor Init(fBlockKind:BlockKind);
   destructor Done; virtual;
   function GetPrevious : PrepBlockPtr; // unfortunately static
   procedure DebugEvent(fIndent:integer; fInfo:string);
   procedure DebugItemCreate(fItemKind:ItemKind); virtual;
   procedure DebugBlockCreate(fBlockKind:BlockKind); virtual;
   procedure DebugItemEnd(fItemKind:ItemKind); virtual;
   procedure DebugBlockEnd(fBlockKind:BlockKind); virtual;
   procedure StartBlock; virtual;
   procedure FinishBlock; virtual;
   function BlockIsQuotable: boolean;
// start and end handlers   
   procedure StartMain; virtual;
   procedure StartDiffuse; virtual;
   procedure StartProof; virtual;
   procedure StartDefinition; virtual;
   procedure StartRegistration; virtual;
   procedure StartNotation; virtual;
   procedure StartCase; virtual;
   procedure StartSuppose; virtual;
   procedure StartPublicScheme; virtual;
   procedure StartCaseList; virtual;
   
   procedure FinishMain; virtual;
   procedure FinishDiffuse; virtual;
   procedure FinishProof; virtual;
   procedure FinishDefinition; virtual;
   procedure FinishRegistration; virtual;
   procedure FinishNotation; virtual;

   procedure FinishCase; virtual;
   procedure FinishSuppose; virtual;
   procedure FinishPublicScheme; virtual;
   procedure FinishCaseList; virtual;
      
   function CreateItem(fItemKind:ItemKind): PrepItemPtr; virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
// added
   procedure ProcessBlockPosition; virtual;
   procedure ProcessBlockLabel; virtual;
   procedure ProcessEndPosition; virtual;
   procedure FinishLastItem(fItm:PrepItemPtr); virtual;
   procedure StartBlockThesis; virtual;
   procedure FinishBlockThesis; virtual;
   procedure ProcessSchemeLabel; virtual;
// new added by CzB
   procedure ProcessProposition; virtual;
   procedure ProcessFormula; virtual;
   procedure ProcessTerm; virtual;
   procedure ProcessType; virtual;
   procedure ProcessTypeList; virtual;
   procedure ProcessIterSteps; virtual;
   procedure ProcessDefiniens; virtual;
   procedure ProcessIdentify; virtual;
   procedure ProcessReduction; virtual;
   procedure ProcessProperty; virtual;
   procedure ProcessRCluster; virtual;
   procedure ProcessCCluster; virtual;
   procedure ProcessFCluster; virtual;
   procedure ProcessConstructor; virtual;
  end;

procedure KillPBlock;
// procedure KillItem;


// These globals should be wrapped in one Parser type
var
 AReport: InVRFFileObj;
 gPrBlockPtr: PrepBlockPtr;
// gItemPtr: PrepItemPtr; // replaced with gPrBlockPtr^.nCurrItm
 
const errUnhandledItemKind  = 7000;
const errUnhandledBlockKind = 7001;
const errUnexpectedItemKind = 7002;
const errUnexpectedJustificationKind = 7003;
const errDied    = 7004;
const errUnexpected    = 7005;
const errNoParentBlock    = 7006;
const errSchemeBlockExpected= 7007;
const errUnexpectedSchState= 7008;
const errUnexpectedProposKind= 7009;
const errBlockInconsistent = 7010;


implementation

// ##NOTE: This block should be copied when instantianting
//         Prep(Block|Item)Obj, and Prep replaced with your
//         object name - e.g. to Std(Block|Item)(Obj|Ptr) .

(********** Standard overloaded stuff **********)

function  PrepItemObj.GetPrevious : PrepItemPtr;
begin GetPrevious := PrepItemPtr(Previous); end;   

function  PrepItemObj.GetBlock : PrepBlockPtr;
begin GetBlock := PrepBlockPtr(nBlock); end;

function  PrepBlockObj.GetPrevious : PrepBlockPtr;
begin GetPrevious := PrepBlockPtr(Previous); end;  

function PrepBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm:= new( PrepItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem:= nCurrItm;
end;

procedure PrepBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr:= new( PrepBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;
end;

(***********************************************)

{$IFDEF MDEBUG}
const gInfoIndent = '  ';

procedure InfoCurrentIndent(fFrom:integer);
var
 lBlock: StackedPtr;
 i:integer;
begin
 lBlock:= gPrBlockPtr;
 for i:=fFrom downto 1 do lBlock:= lBlock^.Previous;
 while Assigned(lBlock) do
 begin
  InfoString(gInfoIndent);
  lBlock:= lBlock^.Previous;
 end; 
end;
{$ENDIF}

constructor PrepItemObj.Init(fItemKind:ItemKind; fBlock: PrepBlockPtr);
 var I: integer;
begin
 Mizassert(errNoParentBlock, Assigned(fBlock));
 Mizassert(errUnexpectedItemKind,
        not (fItemKind in [Low(TopItemKind) .. High(TopItemKind)])
        or (fBlock^.nBlockKind = blMain));
 fBlock^.DebugItemCreate(fItemKind);
 nBlock:= fBlock;
 nItemKind:= fItemKind;
 Previous:= nBlock^.nCurrItm;
end;

destructor PrepItemObj.Done; begin end;

// ##NOTE: This (Start|Finish)(Item|Block) mechanism based on Kind 
//         emulates object hierarchy with virtual methods.
//         I do not want to create new object for each kind, since
//         PrepItemObj will itself be often used as a parent object,
//         and sooner or later multiple inheritance would be needed.
procedure PrepItemObj.StartItem;
begin
 case nItemKind of
// SkeletonItemKind 
  itGeneralization:StartGeneralization;
  itExistentialAssumption:StartExistentialAssumption;
  itConclusion:StartConclusion;
  itAssumption:StartAssumption;
  itSimpleExemplification:StartSimpleExemplification;
  itExemplificationWithEquality:StartExemplificationWithEquality;
  itPerCases:StartPerCases;
  itCase:StartCase;
  itSuppose:StartSuppose;
  
// AuxilieryItemKind 
  itPrivateStatement:StartPrivateStatement;
  itChoice:StartChoice;
  itReconsider:StartReconsider;
  itPrivFuncDefinition:StartPrivFuncDefinition;
  itPrivPredDefinition:StartPrivPredDefinition;
  itConstantDefinition:StartConstantDefinition;

  // TopItemKind
  itScheme:StartScheme;
  itTheorem:StartTheorem;
  itDefTheorem:StartDefTheorem;
  itReservation:StartReservation;
  itSection:;
  itCanceledScheme:;
  itCanceled:StartCanceled;

  // parts of definitions
  // DefinitionalItemKind
  itCorrCond:StartCorrCond;
  itCorrectness:      StartCorrectness;
  itConstructor:StartConstructor;
  itProperty:StartProperty;
  itDefiniens:StartDefiniens;
  itDefExpandMode:StartDefExpandMode;
  itDefPrAttr:StartDefPrAttr;
  itRedefPrAttr:StartRedefPrAttr;
  itCanceledDef:StartCanceledDef;
  itDefStruct:StartDefStruct;
  itDefMode:StartDefMode;
  itRedefMode:StartRedefMode;
  itDefFunc:StartDefFunc;
  itDefPred:StartDefPred;
  itRedefPred:StartRedefPred;
  itRedefFunc:StartRedefFunc;
  itFuncSynonym:       StartFuncSynonym;
  itPredSynonym:       StartPredSynonym;
  itPredAntonym:       StartPredAntonym;
  itModeSynonym:       StartModeSynonym;
  itAttrSynonym:       StartAttrSynonym;
  itAttrAntonym:       StartAttrAntonym;

  itIdentify: StartIdentify;
  itReduction: StartReduction;
  itPropertyReg: StartPropertyReg;
  itExistentialCluster:StartExistentialCluster;
  itConditionalCluster:StartConditionalCluster;
  itFunctorCluster:StartFunctorCluster;
  
  // parts of schemes 
  // SchemeItemType 
  itSchemeTypes:       StartSchemeTypes;
  itSchemeStatement:StartSchemeStatement;
  itSchemePremises:StartSchemePremises;
  
  // SpecialItemKind 
  itIncorrItem:StartIncorrItem;
  itThesis:StartThesis;  
  
  else RunTimeError(errUnhandledItemKind)
 end;
end; 
  

procedure PrepItemObj.FinishItem;
begin
 case nItemKind of
// SkeletonItemKind 
  itGeneralization:FinishGeneralization;
  itExistentialAssumption:FinishExistentialAssumption;
  itConclusion:FinishConclusion;
  itAssumption:FinishAssumption;
  itSimpleExemplification:FinishSimpleExemplification;
  itExemplificationWithEquality:FinishExemplificationWithEquality;
  itPerCases:FinishPerCases;
  itCase:FinishCase;
  itSuppose:FinishSuppose;

// AuxilieryItemKind
  itPrivateStatement:FinishPrivateStatement;
  itChoice:FinishChoice;
  itReconsider:FinishReconsider;
  itPrivFuncDefinition:FinishPrivFuncDefinition;
  itPrivPredDefinition:FinishPrivPredDefinition;
  itConstantDefinition:FinishConstantDefinition;

  // TopItemKind
  itScheme:FinishScheme;
  itTheorem:FinishTheorem;
  itDefTheorem:FinishDefTheorem;
  itReservation:FinishReservation;
  itSection:FinishSection;
  itCanceledScheme:FinishCanceledScheme;
  itCanceled:FinishCanceled;

  // parts of definitions
  // DefinitionalItemKind 
  itCorrCond:FinishCorrCond;
  itCorrectness:       FinishCorrectness;
  itConstructor:       FinishConstructor;
  itProperty:          FinishProperty;
  itDefiniens:         FinishDefiniens;
  itDefExpandMode:     FinishDefExpandMode;
  itDefPrAttr:         FinishDefPrAttr;
  itRedefPrAttr:       FinishRedefPrAttr;
  itCanceledDef:       FinishCanceledDef;
  itDefStruct:         FinishDefStruct;
  itDefMode:           FinishDefMode;
  itRedefMode:         FinishRedefMode;
  itDefFunc:           FinishDefFunc;
  itDefPred:           FinishDefPred;
  itRedefPred:         FinishRedefPred;
  itRedefFunc:         FinishRedefFunc;
  itFuncSynonym:       FinishFuncSynonym;
  itPredSynonym:       FinishPredSynonym;
  itPredAntonym:       FinishPredAntonym;
  itModeSynonym:       FinishModeSynonym;
  itAttrSynonym:       FinishAttrSynonym;
  itAttrAntonym:       FinishAttrAntonym;
  itIdentify:          FinishIdentify;
  itReduction:         FinishReduction;
  itPropertyReg:       FinishPropertyReg;
  itExistentialCluster:FinishExistentialCluster;
  itConditionalCluster:FinishConditionalCluster;
  itFunctorCluster:    FinishFunctorCluster;

  // parts of schemes
  // SchemeItemType
  itSchemeTypes:       FinishSchemeTypes;
  itSchemeStatement:   FinishSchemeStatement;
  itSchemePremises:    FinishSchemePremises;

  // SpecialItemKind
  itIncorrItem:        FinishIncorrItem;
  itThesis:            FinishThesis;
  
  else RunTimeError(errUnhandledItemKind)
 end;
end; 

procedure PrepBlockObj.StartBlock;
begin
 case nBlockKind of
  blMain:StartMain;
  blDiffuse:      StartDiffuse;
  blProof:        StartProof;
  blDefinition:   StartDefinition;
  blRegistration: StartRegistration;
  blNotation:     StartNotation;
  blCase:         StartCase;
  blSuppose:      StartSuppose;
  blPublicScheme: StartPublicScheme;
  blCaseList:     StartCaseList;
 else RunTimeError(errUnhandledBlockKind);
 end;
end;

procedure PrepBlockObj.FinishBlock;
begin
 case nBlockKind of
  blMain: FinishMain;
  blDiffuse:      FinishDiffuse;
  blProof:        FinishProof;
  blDefinition:   FinishDefinition;
  blRegistration: FinishRegistration;
  blNotation:     FinishNotation;
  blCase:         FinishCase;
  blSuppose:      FinishSuppose;
  blPublicScheme: FinishPublicScheme;
  blCaseList:     FinishCaseList;
 else RunTimeError(errUnhandledBlockKind);
 end;
end;

constructor PrepBlockObj.Init(fBlockKind:BlockKind);
begin
 Previous:= gPrBlockPtr;
 nBlockKind:= fBlockKind;
 nCurrItm:= nil;
end;


// dispose pending items
destructor PrepBlockObj.Done;
var tmp: PrepItemPtr;
begin
 while Assigned(nCurrItm) do
 begin
  tmp := nCurrItm^.GetPrevious;
  dispose(nCurrItm, Done);
  nCurrItm:= tmp;
 end;
end;


function PrepBlockObj.BlockIsQuotable: boolean;
begin BlockIsQuotable := (nBlockKind in QuotableBlKinds); end;

procedure PrepBlockObj.ProcessBlockPosition; begin end;
procedure PrepBlockObj.ProcessBlockLabel; begin end;
procedure PrepBlockObj.ProcessEndPosition; begin end;

procedure PrepBlockObj.DebugEvent(fIndent:integer; fInfo:string);
begin
{$IFDEF MDEBUG}
 InfoNewline;
 InfoCurrentIndent(fIndent);
 InfoString(fInfo+':');
 InfoPos(CurPos); 
{$ENDIF}
end;

procedure PrepBlockObj.DebugItemCreate(fItemKind:ItemKind);
begin DebugEvent(0, 'start_it:'+ItemDebugNames[fItemKind]); end;

procedure PrepBlockObj.DebugItemEnd(fItemKind:ItemKind);
begin DebugEvent(0, 'end_it:'+ItemDebugNames[fItemKind]); end;

procedure PrepBlockObj.DebugBlockCreate(fBlockKind:BlockKind);
begin DebugEvent(0, 'start_bl:'+BlockDebugNames[fBlockKind]); end;

procedure PrepBlockObj.DebugBlockEnd(fBlockKind:BlockKind);
begin DebugEvent(1, 'end_bl:'+BlockDebugNames[fBlockKind]); end;


// Handlers corresponding to nBlockKind

procedure PrepBlockObj.StartMain; begin end;
procedure PrepBlockObj.StartDiffuse; begin end;
procedure PrepBlockObj.StartProof; begin end;
procedure PrepBlockObj.StartDefinition; begin end;
procedure PrepBlockObj.StartRegistration; begin end;
procedure PrepBlockObj.StartNotation; begin end;
procedure PrepBlockObj.StartCase; begin end;
procedure PrepBlockObj.StartSuppose; begin end;
procedure PrepBlockObj.StartPublicScheme; begin end;
procedure PrepBlockObj.StartCaseList; begin end;

procedure PrepBlockObj.FinishMain; begin end;
procedure PrepBlockObj.FinishDiffuse; begin end;
procedure PrepBlockObj.FinishProof; begin end;
procedure PrepBlockObj.FinishDefinition; begin end;
procedure PrepBlockObj.FinishRegistration; begin end;
procedure PrepBlockObj.FinishNotation; begin end;
procedure PrepBlockObj.FinishCase; begin end;
procedure PrepBlockObj.FinishSuppose; begin end;
procedure PrepBlockObj.FinishPublicScheme; begin end;
procedure PrepBlockObj.FinishCaseList; begin end;

procedure PrepBlockObj.ProcessProposition; begin end;
procedure PrepBlockObj.ProcessFormula; begin end;
procedure PrepBlockObj.ProcessTerm; begin end;
procedure PrepBlockObj.ProcessType; begin end;
procedure PrepBlockObj.ProcessTypeList; begin end;
procedure PrepBlockObj.ProcessIterSteps; begin end;
procedure PrepBlockObj.ProcessDefiniens; begin end;
procedure PrepBlockObj.ProcessIdentify; begin end;
procedure PrepBlockObj.ProcessReduction; begin end;
procedure PrepBlockObj.ProcessProperty; begin end;
procedure PrepBlockObj.ProcessRCluster; begin end;
procedure PrepBlockObj.ProcessCCluster; begin end;
procedure PrepBlockObj.ProcessFCluster; begin end;
procedure PrepBlockObj.ProcessConstructor; begin end;

// Start Handlers corresponding to nItemKind
// SkeletonItemKind 
procedure PrepItemObj.StartGeneralization; begin end;
procedure PrepItemObj.StartExistentialAssumption; begin end;
procedure PrepItemObj.StartConclusion; begin end;
procedure PrepItemObj.StartAssumption; begin end;
procedure PrepItemObj.StartSimpleExemplification; begin end;
procedure PrepItemObj.StartExemplificationWithEquality; begin end;
procedure PrepItemObj.StartPerCases; begin end;
procedure PrepItemObj.StartCase; begin end;
procedure PrepItemObj.StartSuppose; begin end;

// AuxilieryItemKind 
procedure PrepItemObj.StartPrivateStatement; begin end;
procedure PrepItemObj.StartChoice; begin end;
procedure PrepItemObj.StartReconsider; begin end;
procedure PrepItemObj.StartPrivFuncDefinition; begin end;
procedure PrepItemObj.StartPrivPredDefinition; begin end;
procedure PrepItemObj.StartConstantDefinition; begin end;

// TopItemKind 
procedure PrepItemObj.StartScheme; begin end;
procedure PrepItemObj.StartTheorem; begin end;
procedure PrepItemObj.StartDefTheorem; begin end;
procedure PrepItemObj.StartReservation; begin end;
procedure PrepItemObj.StartSection; begin end;
procedure PrepItemObj.StartCanceled; begin end;

// parts of definitions
// DefinitionalItemKind
procedure PrepItemObj.StartCorrCond; begin end;
procedure PrepItemObj.StartCorrectness; begin end;
procedure PrepItemObj.StartConstructor; begin end;
procedure PrepItemObj.StartProperty; begin end;
procedure PrepItemObj.StartDefiniens; begin end;
procedure PrepItemObj.StartDefExpandMode; begin end;
procedure PrepItemObj.StartDefPrAttr; begin end;
procedure PrepItemObj.StartRedefPrAttr; begin end;
procedure PrepItemObj.StartCanceledDef; begin end;
procedure PrepItemObj.StartDefStruct; begin end;
procedure PrepItemObj.StartDefMode; begin end;
procedure PrepItemObj.StartRedefMode; begin end;
procedure PrepItemObj.StartDefFunc; begin end;
procedure PrepItemObj.StartDefPred; begin end;
procedure PrepItemObj.StartRedefPred; begin end;
procedure PrepItemObj.StartRedefFunc; begin end;    
procedure PrepItemObj.StartFuncSynonym; begin end;
procedure PrepItemObj.StartPredSynonym; begin end;
procedure PrepItemObj.StartPredAntonym; begin end;
procedure PrepItemObj.StartModeSynonym; begin end;
procedure PrepItemObj.StartAttrSynonym; begin end;
procedure PrepItemObj.StartAttrAntonym; begin end;
procedure PrepItemObj.StartIdentify; begin end;
procedure PrepItemObj.StartReduction; begin end;
procedure PrepItemObj.StartPropertyReg; begin end;
procedure PrepItemObj.StartExistentialCluster; begin end;
procedure PrepItemObj.StartConditionalCluster; begin end;
procedure PrepItemObj.StartFunctorCluster; begin end;

// parts of schemes 
// SchemeItemType 
procedure PrepItemObj.StartSchemeTypes; begin end;
procedure PrepItemObj.StartSchemeStatement; begin end;
procedure PrepItemObj.StartSchemePremises; begin end;

// SpecialItemKind 
procedure PrepItemObj.StartIncorrItem; begin end;
procedure PrepItemObj.StartThesis; begin end;  


// Finish handlers corresponding to nItemKind
// SkeletonItemKind 
procedure PrepItemObj.FinishGeneralization; begin end;
procedure PrepItemObj.FinishExistentialAssumption; begin end;
procedure PrepItemObj.FinishConclusion; begin end;
procedure PrepItemObj.FinishAssumption; begin end;
procedure PrepItemObj.FinishSimpleExemplification; begin end;
procedure PrepItemObj.FinishExemplificationWithEquality; begin end;
procedure PrepItemObj.FinishPerCases; begin end;
procedure PrepItemObj.FinishCase; begin end;
procedure PrepItemObj.FinishSuppose; begin end;

// AuxilieryItemKind 
procedure PrepItemObj.FinishPrivateStatement; begin end;
procedure PrepItemObj.FinishChoice; begin end;
procedure PrepItemObj.FinishReconsider; begin end;
procedure PrepItemObj.FinishPrivFuncDefinition; begin end;
procedure PrepItemObj.FinishPrivPredDefinition; begin end;
procedure PrepItemObj.FinishConstantDefinition; begin end;

// TopItemKind 
procedure PrepItemObj.FinishScheme; begin end;
procedure PrepItemObj.FinishTheorem; begin end;
procedure PrepItemObj.FinishDefTheorem; begin end;
procedure PrepItemObj.FinishReservation; begin end;
procedure PrepItemObj.FinishSection; begin end;
procedure PrepItemObj.FinishCanceledScheme; begin end;
procedure PrepItemObj.FinishCanceled; begin end;

// parts of definitions
// DefinitionalItemKind 
procedure PrepItemObj.FinishCorrCond; begin end;
procedure PrepItemObj.FinishCorrectness; begin end;
procedure PrepItemObj.FinishConstructor; begin end;
procedure PrepItemObj.FinishProperty; begin end;
procedure PrepItemObj.FinishDefiniens; begin end;
procedure PrepItemObj.FinishDefExpandMode; begin end;
procedure PrepItemObj.FinishDefPrAttr; begin end;
procedure PrepItemObj.FinishRedefPrAttr; begin end;
procedure PrepItemObj.FinishCanceledDef; begin end;
procedure PrepItemObj.FinishDefAttr; begin end;
procedure PrepItemObj.FinishDefStruct; begin end;
procedure PrepItemObj.FinishDefMode; begin end;
procedure PrepItemObj.FinishRedefMode; begin end;
procedure PrepItemObj.FinishDefFunc; begin end;
procedure PrepItemObj.FinishDefPred; begin end;
procedure PrepItemObj.FinishRedefPred; begin end;
procedure PrepItemObj.FinishRedefFunc; begin end;    
procedure PrepItemObj.FinishFuncSynonym; begin end;
procedure PrepItemObj.FinishPredSynonym; begin end;
procedure PrepItemObj.FinishPredAntonym; begin end;
procedure PrepItemObj.FinishModeSynonym; begin end;
procedure PrepItemObj.FinishAttrSynonym; begin end;
procedure PrepItemObj.FinishAttrAntonym; begin end;
procedure PrepItemObj.FinishIdentify; begin end;
procedure PrepItemObj.FinishReduction; begin end;
procedure PrepItemObj.FinishPropertyReg; begin end;
procedure PrepItemObj.FinishExistentialCluster; begin end;
procedure PrepItemObj.FinishConditionalCluster; begin end;
procedure PrepItemObj.FinishFunctorCluster; begin end;

// parts of schemes 
// SchemeItemType 
procedure PrepItemObj.FinishSchemeTypes; begin end;
procedure PrepItemObj.FinishSchemeStatement; begin end;
procedure PrepItemObj.FinishSchemePremises; begin end;

// SpecialItemKind 
procedure PrepItemObj.FinishIncorrItem; begin end;
procedure PrepItemObj.FinishThesis; begin end;  


// added for schemes
procedure PrepItemObj.FinishSchPredSegment; begin end;  
procedure PrepItemObj.FinishSchFuncSegment; begin end;  

procedure PrepItemObj.StartQuotableProposition(fKind: PropositionKind);
begin end;
procedure PrepItemObj.StartUnquotableProposition(fKind: PropositionKind);
begin end;
procedure PrepItemObj.StartQuotablePropositions(fKind: PropositionKind);
begin end;
procedure PrepItemObj.FinishQuotableProposition(fKind: PropositionKind);
begin end;
procedure PrepItemObj.FinishUnquotableProposition(fKind: PropositionKind);
begin end;
procedure PrepItemObj.FinishQuotablePropositions(fKind: PropositionKind);
begin end;

procedure PrepItemObj.FinishReconsidering; begin end;
procedure PrepItemObj.StartReconsideredTerm; begin end;
procedure PrepItemObj.FinishReconsideredTerm; begin end;

procedure PrepItemObj.StartFixedVariables; begin end;
procedure PrepItemObj.FinishFixedVariables; begin end;
procedure PrepItemObj.StartFixedVariable; begin end;
procedure PrepItemObj.FinishFixedVariable; begin end;

procedure PrepItemObj.StartExemplifyingTerm; begin end;
procedure PrepItemObj.FinishExemplifyingTerm; begin end;

procedure PrepItemObj.StartTheoremBody; begin end;
procedure PrepItemObj.FinishTheoremBody; begin end;
procedure PrepItemObj.FinishPropertyBody; begin end;
procedure PrepItemObj.FinishCorrConditionBody; begin end;
procedure PrepItemObj.FinishCorrectnessBody;  begin end;
procedure PrepItemObj.ProcessAddedConditionFrm;  begin end;
procedure PrepItemObj.ProcessAddedConditionNr;  begin end;

procedure PrepItemObj.StartReservationSegment; begin end;
procedure PrepItemObj.ProcessReservationIdentifier; begin end;
procedure PrepItemObj.FinishReservationSegment; begin end;

procedure PrepItemObj.ProcessLabel; begin end;
procedure PrepItemObj.ProcessPosition; begin end;
procedure PrepItemObj.ProcessDefiniensLabel; begin end;
procedure PrepItemObj.StartSchemePremise; begin end;
procedure PrepItemObj.FinishSchemePremise; begin end;
procedure PrepItemObj.FinishSchemeThesis; begin end;

procedure PrepItemObj.StartIterEquality; begin end;
procedure PrepItemObj.FinishIterEquality; begin end;
procedure PrepItemObj.StartSimpleJustification; begin end;
procedure PrepItemObj.FinishSimpleJustification; begin end;
procedure PrepItemObj.ProcessAtSignProof; begin end;


// Overload this when disposing is unwanted - e.g.
// in utilities permutating items.
// We now mizassert that items cannot create their own children;
// if needed, remove the assertion and clean all children
// from nCurrItm up to fItm
procedure PrepBlockObj.FinishLastItem(fItm:PrepItemPtr);
begin
 Mizassert(2142, (nCurrItm = fItm) and Assigned(fItm));
 DebugItemEnd(fItm^.nItemKind);

 nCurrItm^.FinishItem;
 nCurrItm := nCurrItm^.GetPrevious;
 dispose(fItm, Done);

 DisplayLine(CurPos.Line,ErrorNbr);
end;

procedure PrepBlockObj.StartBlockThesis; begin end;
procedure PrepBlockObj.FinishBlockThesis; begin end;
procedure PrepBlockObj.ProcessSchemeLabel; begin end;

procedure KillPBlock;
var tmp: PrepBlockPtr;
begin
 Mizassert(2141, Assigned(gPrBlockPtr));
 gPrBlockPtr^.DebugBlockEnd(gPrBlockPtr^.nBlockKind);
 
 gPrBlockPtr^.FinishBlock;
 tmp:= gPrBlockPtr^.GetPrevious;
 dispose(gPrBlockPtr, Done);
 gPrBlockPtr:= tmp;
 
 DisplayLine(CurPos.Line,ErrorNbr);
end;

end.
