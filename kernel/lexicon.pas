(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit lexicon;

interface

uses syntax;

const

  itPredNotation = itPredSynonym;

(**********  Internally used symbols **********)

// ## SUGGEST: good names for RSNENTRY
// ## SUGGEST: why are predicates missing in LoadType/SaveType???

  ikError		= '?'; // #63

// Internal Expression Kind - can be translated to enum later
  ikExpError		= ikError; // unused yet
  ikExpAttrTyp		= 'a'; // #97
  ikExpResDes		= 'D'; // #114

// Internal Term Kind - can be translated to enum later
// ##SUGGEST: change 'H' to 'J' for noForgetFunctor in  acc_han
  ikTrmError		= ikError; // unused yet
  ikTrmLocus		= 'A';
  ikTrmBound		= 'B';
  ikTrmConstant		= 'C'; // conflict in acc_han
  ikTrmInfConst		= 'D'; // conflict
  ikTrmEqConst		= 'E';
  ikTrmSchFunc		= 'F'; // conflict in acc_han
  ikTrmAggreg 		= 'G';
  ikTrmPrivFunc		= 'H';
  ikTrmSubAgreg 	= 'J';
  ikTrmFunctor 		= 'K';
  ikTrmNumeral 		= 'N';
  ikTrmSelector 	= 'U';
  ikTrmFreeVar 		= 'X';
  ikTrmLambdaVar 	= 'Z';
  ikTrmQua              = #213; // #213
  ikTrmChoice           = #216; // #216
  ikTrmFraenkel		= #232; // #232
  ikTrmIt		= #234; // #234
  ikTrmExactly		= #238; // #238 seems unused

// Internal Type Kind - can be translated to enum later
// ## SUGGEST: ikTypStruct = 'L'
  ikTypError		= ikError;  // unused yet
  ikTypMode		= 'M';
  ikTypStruct		= 'G'; // 'L'
  ikTypExpMode		= 'O';
  ikTypSetOf		= 'Q';

// Internal Attribute Kind - can be translated to enum later
  ikAtrPos		= 'V';
  ikAtrNeg		= 'W';

// Internal Formula Kind - can be translated to enum later
  ikFrmError		= ikError;  // unused yet
  ikFrmThesis		= '$'; // #36
  ikFrmVerum		= '%'; // #37
  ikFrmConj 		= '&'; // #38
  ikFrmSchPred		= 'P';
  ikFrmPred	 	= 'R';
  ikRSFrmPred	 	= 'T';
  ikMultFrmPred	 	= 'p';
  ikFrmPrivPred		= 'S';
  ikFrmAttr 		= 'V';
  ikFrmQual		= #144; // #144
  ikFrmUniv		= #157; // #157
  ikFrmEx		= #156; // #156
  ikFrmNeg		= #170; // #170
  ikFrmOr		= #167; // #167
  ikFrmIff	 	= #240; // #240
  ikFrmImplies		= #243; // #243
  ikFrmFlexDisj         = 'a'; // #190 ///SPRAWDZIC
  ikFrmFlexConj         = 'b'; // #191

// Internal Inference Kind - can be translated to enum later
// ## SUGGEST: st. different from '"' for start of inference
  ikInfError		= ikError;  // unused yet
  ikInfBy		= '''';
  ikInfFrom		= '"';

// Internal Signature Kind  - used sometimes for notation or constrs
// ## SUGGEST: replace this completely with the standard K,V,M,...
  ikSgnAttribute 	= #172; // #172
  ikSgnForgetFunc 	= #233; // #233
  ikSgnSelector 	= #235; // #235
  ikSgnStructMode 	= #236; // #236
  ikSgnAggregate 	= #237; // #237
  ikSgnMode 		= #239; // #239
  ikSgnFunctor 		= #241; // #241
  ikSgnPredicate 	= #242; // #242

// Internal Cluster Kind
// ## SUGGEST: having too different registered clusters is really strange
  ikCluFunctor	 	= 'f'; // #102
  ikCluRegistered 	= #143; // #143
  ikCluReg1 		= #171; // #171
  ikCluConditional 	= #173; // #173

// Internal Identify Kind
  ikIdFunctors          = 'k'; // #107
  ikIdPredicates        = 'r'; // #114
  ikIdAttributes        = 'v'; // #118
  ikProperty            = 'X';
  ikReduceFunctors      = 'q'; // #113

// Internal Block Kind
  ikBlcRegistration	= '*'; // #42
  ikBlcDefinition	= '+'; // #43
  ikBlcNotation		= '-'; // #45
  ikBlcIgnoreProof     	= 'i'; // #47
  ikBlcProof 		= '/'; // #47
  ikBlcDiffuse		= '<'; // #60
  ikBlcHereby		= #188; // #188
  ikBlcSuppose		= #205; // #205
  ikBlcPerCases 	= #227; // #227
  ikBlcScheme		= #228; // #228
  ikBlcCase		= #231; // #231
  ikBlcSection          = #178; // #178

// Internal Item Kind
// ## SUGGEST: it seems that ikItmRedefAttr is not handled in analyzer.pas!
  ikItmCluFunctor 	= 'f'; // #102
  ikItmDefTheorem	= 't'; // #116
  ikItmCorrectness 	= #135; // #135
  ikItmDefExpandMode	= #136; // #136
  ikItmDefPrAttr 	= #146; // #146
  ikItmRedefPrAttr 	= #148; // #148
  ikItmRedefAttr	= #168; // #168
  ikItmCluRegistered 	= #143; // #143
  ikItmCanceled 	= #164; // #164
  ikItmDefAttr		= #172; // #172
  ikItmCluConditional 	= #173; // #173
  ikItmIterEquality 	= #193; // #193
  ikItmGeneralization	= #200; // #200
  ikItmSimpleExemplif	= #201; // #201
  ikItmAssumption 	= #202; // #202
  ikItmConclusion	= #203; // #203
  ikItmExAssumption	= #204; // #204
  ikItmChoice		= #206; // #206
  ikItmExemplifWithEq	= #207; // #207
  ikItmReservation 	= #215; // #215
  ikItmPrivConstant 	= #224; // #224
  ikItmReconsidering 	= #230; // #230
  ikItmTheorem		= #236; // #236
  ikItmDefStruct	= #237; // #237
  ikItmDefMode 		= #239; // #239
  ikItmRedefMode	= #240; // #240
  ikItmDefFunc		= #241; // #241
  ikItmDefPred		= #242; // #242
  ikItmRedefPred	= #243; // #243
  ikItmPrivPred		= #249; // #249
  ikItmPrivFunc		= #250; // #250
  ikItmRedefFunc	= #251; // #251

// Internal Definiens Kind - can be translated to enum later
  ikDfsEquals		= 'e'; // #101
  ikDfsMeans		= 'm'; // #109

// Internal Kind for miscellaneous other stuff
// ##SUGGEST: ikMscAs seems unused in analyzer - needed?
  ikMscEndBlock		= '>'; // #62
  ikMscAtProof		= '@'; // #64
  ikMscExpansion	= #136; // #136
  ikMscSynonym		= #196; // #196
  ikMscAntonym		= #197; // #197
  ikMscSpecification 	= #211; // #211
  ikMscAs 		= #212; // #212
  ikMscProperties 	= #227; // #227
  ikMscPrefix		= #229; // #229
  ikMscDefLabel 	= #254; // #254
  
  ikDefTheoremCanceled	  = 'D';
  ikTheoremCanceled       = 'T';
  ikSchemeCanceled        = 'S';

(********** To be completed **********)
(***
  ikIterStep		= '.'; // #46	conflict
  ikDot			= '.'; // #46	
  ikDefNodeCanceled	= ':'; // #58
  ikDefTheorem		= 'D';
  ikLabel		= 'E'; // conflict 
  ikPriority		= 'O'; // should rather be sy, but conflict
  ikSuperfluous		= 'S';
  ikTheorem		= 'T'; // conflict
  ikAttrNode		= 'T'; // probably changeble to 'V'
  ikCorrectnessCondition= 'Y';
// 'a'; // #97   too many conflicts - ignored
  ikDefCase		= 'c'; // #99
  ikDefRef		= 'd'; // #100 conflict
  ikIterEquality	= 'd'; // #100 conflict
  ikFuncCluster		= 'f'; // #102
  ikPrivRef		= 'l'; // #108
  ikNoOtherwise		= 'n'; // #110 conflict
  ikOtherwise		= 'o'; // #111
  ikAttributive		= 'p'; // #112  conflict
  ikTheoremRef		= 't'; // #116 conflict
***)

function InternKindName(fSym:char): string;

implementation

   uses errhan,mscanner;

// This is a bit difficult, the mapping is not 1-1
// This should be used for debugging only, so we can afford
// very long strings.
function InternKindName(fSym:char): string;
var s:string;
begin
 case fSym of
  ikInfFrom		: s:= 'ikInfFrom'; // '"'
  ikFrmThesis		: s:= 'ikFrmThesis'; // '$' #36
  ikFrmVerum		: s:= 'ikFrmVerum'; // '%' #37
  ikFrmConj 		: s:= 'ikFrmConj'; // '&' #38
  ikInfBy		: s:= 'ikInfBy'; // ''''
  ikBlcRegistration	: s:= 'ikBlcRegistration'; // '*' #42
  ikBlcDefinition	: s:= 'ikBlcDefinition'; // '+' #43
  ikBlcNotation		: s:= 'ikBlcNotation'; // '-' #45
  ikBlcProof 		: s:= 'ikBlcProof'; // '/' #47
  ikBlcDiffuse		: s:= 'ikBlcDiffuse'; // '<' #60
  ikMscEndBlock		: s:= 'ikMscEndBlock'; // '>' #62
  ikError		: s:= 'ikError'; // '?' #63
  ikMscAtProof		: s:= 'ikMscAtProof'; // '@' #64
  ikTrmLocus 		: s:= 'ikTrmLocus'; // 'A'
  ikTrmBound		: s:= 'ikTrmBound'; // 'B'
  ikTrmConstant		: s:= 'ikTrmConstant'; // 'C'
  ikTrmInfConst		: s:= 'ikTrmInfConst'; // 'D'
  ikTrmEqConst		: s:= 'ikTrmEqConst'; // 'E'
  ikTrmSchFunc		: s:= 'ikTrmSchFunc'; // 'F'
  ikTrmAggreg 		: s:= 'ikTrmAggreg'   // 'G'
                        + '|ikTypStruct'; // 'G'
  ikTrmPrivFunc		: s:= 'ikTrmPrivFunc'; // 'H'
  ikTrmFunctor 		: s:= 'ikTrmFunctor'; // 'K'
  ikTypMode		: s:= 'ikTypMode'; // 'M'
  ikTrmNumeral 		: s:= 'ikTrmNumeral'; // 'N'
  ikFrmSchPred		: s:= 'ikFrmSchPred'; // 'P'
  ikFrmPred	 	: s:= 'ikFrmPred'; // 'R'
  ikRSFrmPred	 	: s:= 'ikRSFrmPred'; // 'R'
  ikMultFrmPred	 	: s:= 'ikMultFrmPred'; // 'R'
  ikFrmPrivPred		: s:= 'ikFrmPrivPred'; // 'S'
  ikTrmSelector 	: s:= 'ikTrmSelector'; // 'U'
  ikAtrPos		: s:= 'ikAtrPos'   // 'V'
                        + '|ikFrmAttr'; // 'V'
  ikAtrNeg		: s:= 'ikAtrNeg'; // 'W'
  ikTrmFreeVar 		: s:= 'ikTrmFreeVar'; // 'X'
  ikTrmLambdaVar 	: s:= 'ikTrmLambdaVar'; // 'Z'
  ikDfsEquals		: s:= 'ikDfsEquals'; // 'e' #101
  ikDfsMeans		: s:= 'ikDfsMeans'; // 'm' #109
  ikItmCorrectness 	: s:= 'ikItmCorrectness'; // #135
  ikItmDefExpandMode	: s:= 'ikItmDefExpandMode'  // #136
                        + '|ikMscExpansion'; // #136
  ikCluRegistered 	: s:= 'ikCluRegistered'; // #143
  ikFrmQual		: s:= 'ikFrmQual'; // #144
  ikItmDefPrAttr 	: s:= 'ikItmDefPrAttr'; // #146
  ikItmRedefPrAttr 	: s:= 'ikItmRedefPrAttr'; // #148
  ikFrmUniv		: s:= 'ikFrmUniv'; // #157
  ikItmCanceled 	: s:= 'ikItmCanceled'; // #164
  ikItmRedefAttr	: s:= 'ikItmRedefAttr'; // #168
  ikFrmNeg		: s:= 'ikFrmNeg';     // #170
  ikCluReg1 		: s:= 'ikCluReg1'   // #171
                        + '|ikItmCluRegistered'; // #143
  ikSgnAttribute 	: s:= 'ikSgnAttribute'   // #172
                        + '|ikItmDefAttr'; // #172
  ikCluConditional 	: s:= 'ikCluConditional'   // #173
                        + '|ikItmCluConditional'; // #173
  ikItmIterEquality 	: s:= 'ikItmIterEquality'; // 193
  ikMscSynonym		: s:= 'ikMscSynonym'; // #196
  ikMscAntonym		: s:= 'ikMscAntonym'; // #197
  ikItmGeneralization	: s:= 'ikItmGeneralization'; // #200
  ikItmSimpleExemplif	: s:= 'ikItmSimpleExemplif'; // #201
  ikItmAssumption 	: s:= 'ikItmAssumption'; // #202
  ikItmConclusion	: s:= 'ikItmConclusion'; // #203
  ikItmExAssumption	: s:= 'ikItmExAssumption'; // #204
  ikBlcSuppose		: s:= 'ikBlcSuppose'; // #205
  ikItmChoice		: s:= 'ikItmChoice'; // #206
  ikItmExemplifWithEq	: s:= 'ikItmExemplifWithEq'; // #207
  ikMscSpecification 	: s:= 'ikMscSpecification'; // #211
  ikMscAs 		: s:= 'ikMscAs'; // #212
  ikTrmQua 		: s:= 'ikTrmQua'; // #213
  ikTrmChoice		: s:= 'ikTrmChoice'; // #216
  ikItmReservation 	: s:= 'ikItmReservation'; // #215
  ikItmPrivConstant 	: s:= 'ikItmPrivConstant'; // #224
  ikBlcPerCases 	: s:= 'ikBlcPerCases'   // #227
                        + '|ikMscProperties'; // #227
  ikBlcScheme		: s:= 'ikBlcScheme'; // #228
  ikMscPrefix		: s:= 'ikMscPrefix'; // #229
  ikItmReconsidering 	: s:= 'ikItmReconsidering'; // #230
  ikBlcCase		: s:= 'ikBlcCase'; // #231
  ikTrmFraenkel		: s:= 'ikTrmFraenkel'; // #232
  ikSgnForgetFunc 	: s:= 'ikSgnForgetFunc'; // #233
  ikTrmIt		: s:= 'ikTrmIt'; // #234
  ikSgnSelector 	: s:= 'ikSgnSelector'; // #235
  ikSgnStructMode 	: s:= 'ikSgnStructMode'   // #236
                        + '|ikItmTheorem'; // #236
  ikSgnAggregate 	: s:= 'ikSgnAggregate'   // #237
                        + '|ikItmDefStruct'; // #237
  ikTrmExactly		: s:= 'ikTrmExactly'; // #238
  ikSgnMode 		: s:= 'ikSgnMode'   // #239
                        + '|ikItmDefMode'; // #239
  ikItmRedefMode	: s:= 'ikItmRedefMode'   // #240
                        + '|ikFrmIff'; // #240
  ikSgnFunctor 		: s:= 'ikSgnFunctor'   // #241
                        + '|ikItmDefFunc'; // #241
  ikSgnPredicate 	: s:= 'ikSgnPredicate'   // #242
                        + '|ikItmDefPred'; // #242
  ikItmRedefPred	: s:= 'ikItmRedefPred'   // #243
                        + '|ikFrmImplies'; // #243
  ikItmPrivPred		: s:= 'ikItmPrivPred'; // #249
  ikItmPrivFunc		: s:= 'ikItmPrivFunc'; // #250
  ikItmRedefFunc	: s:= 'ikItmRedefFunc'; // #251
  ikMscDefLabel 	: s:= 'ikMscDefLabel'; // #254
  else s:= fSym;
 end;
 InternKindName := s;
end;

end.
