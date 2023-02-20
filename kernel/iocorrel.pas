(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit iocorrel;

// xml analogue of the iocorrel; should eventually replace it

// The RELAX NG compact scheme ( .rnc ) used for validation and description
// of the Mizar XML format is automagically generated from the
// comments starting with the two hashes ('#') followed by 'RNC:'.
// See http://www.relaxng.org/compact-tutorial-20030326.html for their syntax.
// The command to create Mizar.rnc is e.g.:
// cat *.pas | perl -e 'local $/;$_=<>; while(m/#[#]RNC:((.|[\n])*?)[*]\)/g) { print $1}' > Mizar.rnc
// See xmldict.pas for one-liners updating xmldict from such Mizar.rnc .
// After xmldict is updated, the "el" and "at" prefixes are stripped,
// and attributes downcased. E.g. this way:
// perl -ne 's/\bel([A-Z])/$1/g; s/\bat([A-Z]\w*)/lc($1)/eg; print $_' Mizar.rnc
// The trang translator can then be used to produce
// RELAX NG XML Schema ( .rng ) and W3C XML Schema ( .xsd ) from .rnc, e.g.:
// trang Mizar.rnc Mizar.rng; this also checks the correctness of RNC comments
// The schemas (with appropriate root element, see directory xml)
// can then be used for validation, e.g.:
// xmllint --noout --relaxng article.rng osalg_1.pre
// xmllint --noout --relaxng constructors.rng osalg_1.dco

interface

uses mobjects,inout,lexicon,builtin,correl,formats,enums,xmlpars,
     xmldict,errhan,dicthan,schemhan,identify,justhan;

type
 InVRFFileObj  = object(MizXInStream)
  function In_TermList: TrmList; virtual;
  function In_Attr:AttrPtr; virtual;
  function In_AttrColl:AttrCollectionPtr; virtual;
  function In_Type: TypPtr; virtual;
  function In_QualFormula: FrmPtr; virtual;
  function In_UnivFormula: FrmPtr; virtual;
  function In_FlexFormula: FrmPtr; virtual;
  function In_ConjFormula: FrmPtr; virtual;
  function In_NegFormula: FrmPtr; virtual;
  function In_PredFormula: FrmPtr; virtual;
  function In_LocPredFormula: FrmPtr; virtual;
  function In_UniqFormula: FrmPtr; virtual;
  function In_Formula: FrmPtr; virtual;
  procedure In_FormulaColl(var fFrmColl:MCollection); virtual;
  procedure In_TypeColl(var fTypColl:MCollection); virtual;
  procedure In_TypeList(var fTypList:MList); virtual;
  procedure In_ArgColl(var fTypColl:MCollection); virtual;
  procedure In_ArgList(var fTypList:MList); virtual;
  function In_VarTerm: VarTrmPtr; virtual;
  function In_FuncTerm: TrmPtr; virtual;
  function In_PrivFuncTerm: FuncTrmPtr; virtual;
  function In_FraenkelTerm: FraenkelTrmPtr; virtual;
  function In_UniqTerm: TrmPtr; virtual;
  function In_ChoiceTerm: TrmPtr; virtual;
  function In_Term: TrmPtr; virtual;
  function In_Constructor1: ConstrPtr;
  function In_RCluster: RClusterPtr; virtual;
  function In_CCluster: CClusterPtr; virtual;
  function In_FCluster: FClusterPtr; virtual;
  function In_Identify: IdentifyPtr; virtual;
  function In_Reduction: ReductionPtr; virtual;
  function In_PropertyReg: PropertyPtr; virtual;
  procedure In_IntRel( aElem: TXMLElemKind;
                        var aRel: IntRel); virtual;
  procedure In_IntSeq( aElem: TXMLElemKind;
                       var aSeq: IntSequence); virtual;
  procedure In_NatFunc( aElem: TXMLElemKind;
                        var aFunc: NatFunc); virtual;
  procedure In_EndPos( var aPos:Position); virtual;
  function In_Pattern: PatternPtr; virtual;
  function In_Def: DefPtr;
  function In_ConstrDef: ConstrDefPtr;
  function In_Definiens: DefiniensPtr;
  function In_EnvDefiniens: DefiniensPtr;
  procedure In_Inference(var aInf:InferenceObj);
  procedure In_IterStep(var aTrm:TrmPtr; var aInf:InferenceObj);
 end;

 OutVRFFileObj = object(MizXOutStream)
   nExpandInferConsts : boolean; // true by default
   constructor OpenFile(const AFileName:string);
   constructor OpenFileWithXSL(const AFileName:string);
   function Transf(fKind : ConstructorsKind; fNr: integer): integer; virtual;
   procedure Out_TermList ( fTrmList:TrmList ); virtual;
   procedure Out_Attr(fAttr:AttrPtr); virtual;
   procedure Out_AttrColl( aCluster: AttrCollectionPtr); virtual;
   procedure Out_Type ( fTyp: TypPtr); virtual;
   procedure Out_TypeWithId ( fTyp: TypPtr; fVarId:integer); virtual;
   procedure Out_TypeList(const fTypeColl: MList); virtual;
   procedure Out_TypeListWithIds(const fTypeColl: MList; const fIdents:IntSequence); virtual;
   procedure Out_ArgTypes(const fTypeColl: MList); virtual;
   procedure Out_FormulaColl(const fFrmColl: MCollection); virtual;
   procedure Out_QualFormula( aFrm: QualFrmPtr); virtual;
   procedure Out_UnivFormula( aFrm: UnivFrmPtr); virtual;
   procedure Out_FlexFormula( aFrm: FlexFrmPtr); virtual;
   procedure Out_ConjFormula( aFrm: ConjFrmPtr); virtual;
   procedure Out_NegFormula( aFrm: NegFrmPtr); virtual;
   procedure Out_PredFormula( aFrm: PredFrmPtr); virtual;
   procedure Out_LocPredFormula( aFrm: LocPredFrmPtr); virtual;
   procedure Out_UniqFormula( aFrm: UniqFrmPtr); virtual;
   procedure Out_Formula ( aFrm:FrmPtr ); virtual;
   procedure Out_VarTerm( fTrm: VarTrmPtr); virtual;
   procedure Out_FuncTerm( fTrm: FuncTrmPtr); virtual;
   procedure Out_PrivFuncTerm( fTrm: LocFuncTrmPtr); virtual;
   procedure Out_FraenkelTerm( fTrm: FraenkelTrmPtr); virtual;
   procedure Out_ChoiceTerm( fTrm: ChoiceTrmPtr); virtual;
   procedure Out_QuaTerm( fTrm: QuaTrmPtr); virtual;
   procedure Out_UniqTerm( fTrm: TrmPtr); virtual;
   procedure Out_Term ( fTrm: TrmPtr ); virtual;
   procedure Out_PosAsAttrs(fPos: Position); virtual;
   procedure Out_Propos( aNr,aId:integer; const aPos: Position;
                         aFrm: FrmPtr); virtual;
   procedure Out_Proposition(fProp: PropositionPtr); virtual;
   procedure Out_Propositions(var fProps:MCollection); virtual;
   procedure Out_Propositions1(var fProps:MCollection); virtual;
   procedure Out_RCluster( fCluster: RClusterPtr); virtual;
   procedure Out_CCluster( fCluster: CClusterPtr); virtual;
   procedure Out_FCluster( fCluster: FClusterPtr); virtual;
   procedure Out_ErrCluster(fClusterKind: TXMLElemKind ); virtual;
   procedure Out_Cluster( fCl:ClusterPtr); virtual;
   procedure Out_Identify( fIdentify: IdentifyPtr);
   procedure Out_Reduction( fReduction: ReductionPtr);
   procedure Out_PropertyReg(aProperty: PropertyPtr);
   procedure Out_Int( i:integer); virtual;
   procedure Out_Pair( i,j:integer); virtual;
   procedure Out_StructLoci(const fStructLoci: NatFunc); virtual;
   procedure Out_IntRel( aElem: TXMLElemKind;
                          const aRel: IntRel); virtual;
   procedure Out_IntSeq( aElem: TXMLElemKind;
                         const aVisible: IntSequence); virtual;
   procedure Out_NatFunc( aElem: TXMLElemKind;
                          const aFunc: NatFunc); virtual;
   procedure Out_Constructor(fConstr:ConstrPtr; fRelNr:integer); virtual;
   procedure Out_EndPos( const aPos:Position); virtual;
   procedure Out_Pattern( fPatt:PatternPtr; fRelNr:integer); virtual;
   procedure Out_ConstrDef( aDef: ConstrDefPtr); virtual;
   procedure Out_DefObj(const aDef: DefObj); virtual;
   procedure Out_Definiens(const aDefiniensObj: DefiniensObj;
                           fRelNr:integer); virtual;
   procedure Out_Inference(const aInf:InferenceObj);
   procedure Out_IterStep(var aInf:IterStepObj);
  end;

 InMMLFilePtr = ^InMMLFileObj;
 InMMLFileObj  = object(InVRFFileObj)
   function In_AttrColl:AttrCollectionPtr; virtual;
   function In_Type: TypPtr; virtual;
   function In_ConjFormula: FrmPtr; virtual;
   function In_RCluster: RClusterPtr; virtual;
   function In_CCluster: CClusterPtr; virtual;
   function In_FCluster: FClusterPtr; virtual;
   function In_Cluster: ClusterPtr; virtual;
   function In_PatternWithFormat: PatternPtr; virtual;
   function AssignedLibr(aName,aExt:string):string; virtual;
   procedure GetConstrNames(var aConstrIds: MStringList); virtual;
   procedure In_ConstrCounts1(var aCounts:ConstrIntArr); virtual;
   procedure In_ConstrCounts(var aCounts:ConstrIntArr); virtual;
   procedure In_Vocs( var aVocs:MStringList); virtual;
 end;

// ##NOTE: this should be used to replace the InEnv and Import methods
 InEnvFilePtr = ^InEnvFileObj;
 InEnvFileObj  = object(InMMLFileObj)
  function In_Type: TypPtr; virtual;
  function In_AttrColl:AttrCollectionPtr; virtual;
 end;


 OutMMLFileObj = object(OutVRFFileObj)
  function Transf(fKind : ConstructorsKind; fNr: integer): integer; virtual;
  procedure Out_Type ( fTyp: TypPtr); virtual;
  procedure Out_TypeWithId ( fTyp: TypPtr; fVarId:integer); virtual;
  procedure Out_RCluster( fCluster: RClusterPtr); virtual;
  procedure Out_CCluster( fCluster: CClusterPtr); virtual;
  procedure Out_FCluster( fCluster: FClusterPtr); virtual;
  procedure Out_PatternWithFormat(aPatt:PatternPtr; aFormat:FormatPtr); virtual;
  procedure Out_ArticleName( aName: string); virtual;
  procedure PutConstrNames(var aConstrIds: MStringList); virtual;
  procedure PutImpScheme(lSch:SchemePtr); virtual;
  procedure OutMarkedSgn(var fImpSgn:MList;var fSgnMarks:NatFunc; fTo:integer); virtual;
  procedure OutSgn(fImpSgn:MList; WriteCurArticle:boolean); virtual;
  procedure OutConstrCounts1(const fCounts:ConstrIntArr); virtual;
  procedure OutConstrCounts(const fCounts:ConstrIntArr); virtual;
  procedure Out_VocItem( aName:string;
                         const aSyms: SymbolCounters); virtual;
  procedure Out_Vocs(var fVocabularies:MStringList); virtual;
  procedure OutMarkedVocs(var fVocabularies:MStringList;var fDictMarks:NatFunc); virtual;
 end;

 OutEnvFileObj = object(OutMMLFileObj)
  function Transf(fKind : ConstructorsKind; fNr: integer): integer; virtual;
 end;
  
procedure Load_EnvConstructors;
procedure LoadDefinitions;
procedure LoadEqualities;
procedure LoadExpansions;
procedure LoadIdentify;
procedure LoadReductions;
procedure LoadPropertiesReg;

var 
// tells if gTrans should be used for translating constr. numbers
// ##TODO: try to get rid of this - local var in OutMMLFileObj? 
  DoCtrans:boolean = true;
  gTrans: array[ConstructorsKind] of IntSequence;
  gChosen: array[ConstructorsKind] of NatSet;
 
implementation  

uses mizenv,limits,librenv,xml_parser,mscanner
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

function XVarTrm2El( aKind: char): TXMLElemKind;
begin
 case aKind of
  ikTrmLocus: 		XVarTrm2El:= elLocusVar;
  ikTrmBound:		XVarTrm2El:= elVar;
  ikTrmConstant:     	XVarTrm2El:= elConst;
  ikTrmInfConst:      	XVarTrm2El:= elInfConst;
  ikTrmFreeVar:       	XVarTrm2El:= elFreeVar;
  ikTrmLambdaVar:	XVarTrm2El:= elLambdaVar;
  ikTrmNumeral:       	XVarTrm2El:= elNum;
 end;
end;

function XVarEl2Kind( aElem: TXMLElemKind): char;
begin
 case aElem of
  elLocusVar:  	XVarEl2Kind:= ikTrmLocus;
  elVar:       	XVarEl2Kind:= ikTrmBound;
  elConst:     	XVarEl2Kind:= ikTrmConstant;
  elInfConst:  	XVarEl2Kind:= ikTrmInfConst;
  elFreeVar:   	XVarEl2Kind:= ikTrmFreeVar;
  elLambdaVar: 	XVarEl2Kind:= ikTrmLambdaVar;
  elNum:       	XVarEl2Kind:= ikTrmNumeral;
 end;
end;

// ends by reaching the element-end tag or a nonterm element
function InVRFFileObj.In_TermList: TrmList;
var lTrmElem:TrmElem; Sentinel:TrmList;
begin
 Sentinel:=addr(lTrmElem);
 while not (nState = eEnd) and (nElKind in TermElKinds) do
 begin
  new(Sentinel^.NextTrm);
  Sentinel:=Sentinel^.NextTrm;
  Sentinel^.XTrmPtr:=In_Term;
 end;
 Sentinel^.NextTrm:=nil; In_TermList:=lTrmElem.NextTrm;
end; 

// ends by reaching the element-end tag or a nontype element
procedure InVRFFileObj.In_FormulaColl(var fFrmColl:MCollection);
begin
 fFrmColl.Init(2,2);
 while not (nState = eEnd) and (nElKind in FrmElKinds) do
  fFrmColl.Insert(In_Formula);
 fFrmColl.Prune;
end;

// ends by reaching the element-end tag or a nontype element
procedure InVRFFileObj.In_TypeColl(var fTypColl:MCollection);
begin
 fTypColl.Init(0,8);
 while not (nState = eEnd) and (nElKind = elTyp) do
  fTypColl.Insert(In_Type);
 fTypColl.SetLimit(0);
end;

// ends by reaching the element-end tag or a nontype element
procedure InVRFFileObj.In_TypeList(var fTypList:MList);
begin
 fTypList.Init(0);
 while not (nState = eEnd) and (nElKind = elTyp) do
  fTypList.Insert(In_Type);
 fTypList.SetLimit(0);
end;

function InVRFFileObj.In_Attr: AttrPtr;
var lNeg:byte; {i,}lNr{,ec}: integer; lName:string;
begin
 XMLASSERT( nElKind = elAdjective);
 lNeg:= 1;
 lNr:=GetIntAttr(atNr);
 if GetOptAttr(atValue,lName) then
   lNeg:=Ord(GetAttr(atValue) = 'true');
 NextElementState;
 In_Attr:= new(AttrPtr, Init( lNeg, lNr, In_TermList));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

function InVRFFileObj.In_AttrColl:AttrCollectionPtr;
var lClusterPtr: AttrCollectionPtr;
begin
 XMLASSERT( nElKind = elCluster);
 lClusterPtr:=new(AttrCollectionPtr,Init(0,4));
 NextElementState;
 while not (nState = eEnd) do
  lClusterPtr^.Insert( In_Attr);
 NextElementState;
 In_AttrColl:=lClusterPtr;
end;

function InMMLFileObj.In_AttrColl:AttrCollectionPtr;
var lClusterPtr: AttrCollectionPtr; 
begin
 XMLASSERT( nElKind = elCluster);
 lClusterPtr:=new(AttrCollectionPtr,Init(0,4));
 NextElementState;
 while not (nState = eEnd) do
  lClusterPtr^.AtInsert(lClusterPtr^.Count, In_Attr);
 NextElementState;
 In_AttrColl:=lClusterPtr;
end;

function InEnvFileObj.In_AttrColl:AttrCollectionPtr;
begin In_AttrColl:= InVRFFileObj.In_AttrColl; end;

function InVRFFileObj.In_Type: TypPtr;
var
 lWhat: Lexem;
 lLowerCluster,lUpperCluster:AttrCollectionPtr;
 lModArgs: TrmList;
begin
 XMLASSERT( nElKind = elTyp);
 GetLexemAttrs( atKind, atNr, lWhat);
 if lWhat.Kind = 'L' then lWhat.Kind:= ikTypStruct;
 NextElementState;
 case lWhat.Kind of
  ikTypMode,ikTypStruct:
   begin 
    lLowerCluster:=In_AttrColl;
    lUpperCluster:=In_AttrColl;
    lModArgs:=In_TermList;
   end;
  'e': // 'errortyp'
   begin
    lWhat.Kind := ikError;
    lLowerCluster:=NewEmptyCluster;
    lUpperCluster:=NewEmptyCluster;
    lModArgs:=nil;
   end;
  else RunTimeError( 2036 );
 end;
 In_Type:= new(TypPtr,Init(lWhat.Kind,lLowerCluster,lUpperCluster,
                           lWhat.Nr,lModArgs));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

// ###TODO: BUG POSSIBLE: the MML and Env methods used only the lower cluster,
//          I think it does not hurt to have in the DB the uppercluster too,
//          but it might affect the accommodation (even if we completely
//          ignore it) or cause some segfaults - check this. 
// ###TODO: for debugging the rest only the lowercluster is left here now,
//          revert to above mentioned if everything works
function InMMLFileObj.In_Type: TypPtr;
var
 lLexem: Lexem;
 lClusterPtr:AttrCollectionPtr;
 lModArgs: TrmList;
begin
 XMLASSERT( nElKind = elTyp);
 GetLexemAttrs( atKind, atNr, lLexem);
 if lLexem.Kind = 'L' then lLexem.Kind:= ikTypStruct;
 Mizassert(2036, lLexem.Kind in [ikTypMode,ikTypStruct]);
 NextElementState;
 lClusterPtr:=In_AttrColl;
 lModArgs:=In_TermList;
 In_Type:=new(TypPtr,Init(lLexem.Kind,lClusterPtr,
                          CopyCluster(lClusterPtr),lLexem.Nr,lModArgs));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

var
  AfterClusters: boolean = false;

function InEnvFileObj.In_Type: TypPtr;
var
 lLexem: Lexem;
 lModArgs:TrmList;
 lTyp,lResultTyp:TypPtr;
 lClusterPtr,lClPtr: AttrCollectionPtr;
begin
 XMLASSERT( nElKind = elTyp);
 GetLexemAttrs( atKind, atNr, lLexem);
 if lLexem.Kind = 'L' then lLexem.Kind:= ikTypStruct;
 Mizassert(2036, lLexem.Kind in [ikTypMode,ikTypStruct]);
 NextElementState;
 lClusterPtr	:= In_AttrColl;
 lModArgs	:= In_TermList;
 lTyp		:= new(TypPtr,Init(lLexem.Kind,lClusterPtr,lClusterPtr,
                                   lLexem.Nr,lModArgs));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
 with lTyp^ do
 begin
  lClusterPtr:=CopyCluster(lClusterPtr);
  if AfterClusters then
  begin
   if lLexem.Kind = ikTypMode then
   begin
    lResultTyp	:= ConstrTypPtr( Constr[coMode].Items^[lLexem.Nr])^.fConstrTyp;
    lClPtr	:= InstCluster( lResultTyp^.UpperCluster, lModArgs);
    lClusterPtr^.EnlargeBy( lClPtr);
    dispose( lClPtr, Done);
   end;
   lClusterPtr^.RoundUpWith(lTyp);
  end;
  UpperCluster	:= lClusterPtr;
 end;
 In_Type	:= lTyp;
end;

// formulas
function InVRFFileObj.In_QualFormula: FrmPtr;
var lTrm:TrmPtr;
begin
 NextElementState;
 lTrm:=In_Term;
 In_QualFormula:= NewQualFrm(lTrm,In_Type);
end;

function InVRFFileObj.In_UnivFormula: FrmPtr;
var lTyp: TypPtr;
begin
 NextElementState;
 inc(BoundVarNbr); lTyp:=In_Type;
 BoundVar[BoundVarNbr]:= lTyp;
 In_UnivFormula:= NewUniv(lTyp,In_Formula);
 dec(BoundVarNbr);
end;

function InVRFFileObj.In_FlexFormula: FrmPtr;
var lLeftOrigFrm,lRightOrigFrm,lDefiniens:FrmPtr;
lLeftTrm,lRightTrm:TrmPtr;
begin
 NextElementState;
 lLeftOrigFrm:=In_Formula;
 lRightOrigFrm:=In_Formula;
 lLeftTrm:=In_Term;
 lRightTrm:=In_Term;
 lDefiniens:=In_Formula;
 In_FlexFormula:=NewFlexFrm(lLeftOrigFrm,lRightOrigFrm,lDefiniens,lLeftTrm,lRightTrm);
end;

function InVRFFileObj.In_ConjFormula: FrmPtr;
var lFrm:FrmPtr; lConjuncts:MCollection;
begin
 NextElementState;
 lConjuncts.Init(2,4);
 while not (nState = eEnd) do
 begin
  lFrm:= In_Formula;
  case lFrm^.FrmSort of
   ikFrmVerum: ;
   ikFrmConj:
    begin
     lConjuncts.AppendTo(ConjFrmPtr(lFrm)^.Conjuncts);
     dispose(lFrm);
    end
  else lConjuncts.Insert(lFrm);
  end;
 end;
 In_ConjFormula:=NewConjFrm(lConjuncts);
end;

function InMMLFileObj.In_ConjFormula: FrmPtr;
var lConjuncts:MCollection;
begin
 NextElementState;
 In_FormulaColl(lConjuncts);
 In_ConjFormula:=new(ConjFrmPtr,Init(lConjuncts));
 MizAssert( errElRedundant, nState = eEnd);
end;

function InVRFFileObj.In_NegFormula: FrmPtr;
begin
 NextElementState;
 In_NegFormula:= NewNegDis( In_Formula);
end;

function InVRFFileObj.In_PredFormula: FrmPtr;
var lLast:Lexem;
begin
 GetLexemAttrs( atKind, atNr, lLast);
 NextElementState;
 In_PredFormula:= NewPredFrm(lLast.Kind,lLast.Nr,In_TermList,0);
end;

function InVRFFileObj.In_LocPredFormula: FrmPtr;
var lConstructor:integer; lTrmList: TrmList; lFrm: FrmPtr;
begin
 lConstructor:= GetIntAttr( atNr);
 NextElementState;
 lTrmList:=In_TermList;
 lFrm:=In_Formula;
 In_LocPredFormula:= NewLocPredFrm(lConstructor,lTrmList,lFrm);
end;

// ##TODO: elThesis missing - but probably not needed
function InVRFFileObj.In_UniqFormula: FrmPtr;
begin
 case nElKind of
  elVerum: In_UniqFormula:=NewVerum;
  elErrorFrm: In_UniqFormula:=NewInCorFrm;
 end;
 AcceptEndState;
end;

// ikFrmThesis was not allowed here in correl
function InVRFFileObj.In_Formula: FrmPtr;
begin
 case nElKind of
  elNot:	In_Formula := In_NegFormula;
  elAnd:	In_Formula := In_ConjFormula;
  elFor:	In_Formula := In_UnivFormula;
  elPred:	In_Formula := In_PredFormula;
  elPrivPred:	In_Formula := In_LocPredFormula;
  elIs:		In_Formula := In_QualFormula;
  elFlex:	In_Formula := In_FlexFormula;
  elVerum,
  elErrorFrm:	In_Formula := In_UniqFormula;
 else UnexpectedElem( nElKind, 2037, FrmElKinds);
 end;
 NextElementState;
end;


// terms
function  InVRFFileObj.In_VarTerm: VarTrmPtr;
begin
 In_VarTerm:=NewVarTrm( XVarEl2Kind( nElKind), GetIntAttr( atNr));
 AcceptEndState;
end;

function InVRFFileObj.In_FuncTerm: TrmPtr;
var lWhat: Lexem;
begin
 GetLexemAttrs( atKind, atNr, lWhat);
 NextElementState;
 In_FuncTerm:= NewLocFuncTrm(lWhat.Kind, lWhat.Nr, In_TermList);
 MizAssert( errElRedundant, nState = eEnd);
end;

function InVRFFileObj.In_PrivFuncTerm: FuncTrmPtr;
var lExp: TrmPtr; lFuncNr:integer; lFuncArgs:TrmList;
begin
 lFuncNr:= GetIntAttr( atNr);
 NextElementState;
 lExp:= In_Term;
 lFuncArgs:= In_TermList;
 In_PrivFuncTerm:=NewPrivFuncTrm(lFuncNr,lFuncArgs,lExp);
 MizAssert( errElRedundant, nState = eEnd);
end;

function InVRFFileObj.In_FraenkelTerm: FraenkelTrmPtr;
var
 lBoundVarNbr:integer;
 lLambdaArgs:MCollection;
 lLambdaScope:TrmPtr;
 lCompr:FrmPtr;
 lTyp:TypPtr;
begin
 lBoundVarNbr:= BoundVarNbr;
 NextElementState;
 lLambdaArgs.Init(4,4);
 while not (nState = eEnd) and (nElKind = elTyp) do
 begin
  inc(BoundVarNbr);
  lTyp:= In_Type;
  lLambdaArgs.Insert(lTyp);
  BoundVar[BoundVarNbr]:=lTyp;
 end;
 lLambdaArgs.SetLimit(0);
 lLambdaScope:= In_Term;
 lCompr:= In_Formula;
 In_FraenkelTerm:= NewFraenkelTrm(lLambdaScope,lCompr,lLambdaArgs);
 BoundVarNbr:= lBoundVarNbr;
 MizAssert( errElRedundant, nState = eEnd);
end;

function InVRFFileObj.In_ChoiceTerm: TrmPtr;
begin
 NextElementState;
 In_ChoiceTerm:= NewChoiceTrm(In_Type);
 MizAssert( errElRedundant, nState = eEnd);
end;

function InVRFFileObj.In_UniqTerm: TrmPtr;
begin
 case nElKind of
  elIt:		In_UniqTerm:= NewItTrm;
  elErrorTrm:	In_UniqTerm:= NewIncorTrm;
 end;
 AcceptEndState;
end;

// ikTrmInfConst and ikTrmQua were not allowed in correl
function InVRFFileObj.In_Term: TrmPtr;
begin
 case nElKind of
  elLocusVar,
  elVar,
  elConst,
  elInfConst,
  elFreeVar,
  elLambdaVar,
  elNum:	In_Term:= In_VarTerm;
  elFunc:	In_Term:= In_FuncTerm;
  elPrivFunc:	In_Term:= In_PrivFuncTerm;
  elFraenkel:	In_Term:= In_FraenkelTerm;
  elChoice:     In_Term:= In_ChoiceTerm;
  elIt,
  elErrorTrm:	In_Term:= In_UniqTerm;
 else UnexpectedElem( nElKind, 2038, TermElKinds);
 end;
 NextElementState;
end;

procedure InVRFFileObj.In_ArgColl(var fTypColl:MCollection);
begin
 XMLASSERT( nElKind = elArgTypes);
 NextElementState;
 In_TypeColl( fTypColl);
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

procedure InVRFFileObj.In_ArgList(var fTypList:MList);
begin
 XMLASSERT( nElKind = elArgTypes);
 NextElementState;
 In_TypeList( fTypList);
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

function InVRFFileObj.In_Constructor1: ConstrPtr;
var
 lConstr: ConstrPtr; lKind: ConstructorsKind;
 {i,}lNr,lSuperfluous,lStructModeAggrNr,lAggregBase{,ec}: integer;
 lAid: string;
begin
 XMLASSERT( nElKind = elConstructor);
 lKind:= ConstructorKind( GetAttr( atKind)[1]);
 lNr:= GetIntAttr( atNr);
 lAid:= GetAttr( atAid);
 case lKind of
  coFunctor, coMode, coAttribute, coSelector:
   		lConstr:=	new(ConstrTypPtr,Init(lKind,lNr,lAid));
  coPredicate: 	lConstr:=	new(ConstrPtr,Init(lKind,lNr,lAid));
  coStructMode: lConstr:=	new(StructConstrPtr,Init(lNr,lAid));
  coAggregate: 	lConstr:=	new(AggrConstrPtr,Init(lNr,lAid));
 end;

 with lConstr^ do
 begin
  GetOptIntAttr(atRedefNr,fWhichConstrNr);
  if GetOptIntAttr(atSuperfluous,lSuperfluous) then
   fSuperfluous:=lSuperfluous;
  GetOptIntAttr(atStructModeAggrNr,lStructModeAggrNr);
  GetOptIntAttr(atAggregBase,lAggregBase);
  NextElementState;
  
  if nElKind = elProperties then
  begin
   fFirstArg:= GetIntAttr( atPropertyArg1);
   fSecondArg:= GetIntAttr( atPropertyArg2);
   NextElementState;
   while not (nState = eEnd) do
   begin
    include( fProperties, XmlElem2Prop( nElKind));
    AcceptEndState; NextElementState;
   end;
   NextElementState;
  end;
  In_ArgColl( nPrimaries);
 end;
 
 if lConstr^.fConstrKind in TypedConstrKinds then
  ConstrTypPtr(lConstr)^.fConstrTyp:=In_Type;

 
 if lConstr^.fConstrKind = coStructMode then
  with StructConstrPtr(lConstr)^ do
 begin
  fStructModeAggrNr:= lStructModeAggrNr;
  fPrefixes.Init(4,4);
  while (nElKind = elTyp) do
   fPrefixes.Insert(In_Type);
  XMLASSERT( nElKind = elFields);
  fFields:= new(NatSetPtr,Init(2,2));
  NextElementState;
  while not (nState = eEnd) do
  begin
   fFields^.InsertElem( GetIntAttr( atNr));
   AcceptEndState; NextElementState;
  end;
  NextElementState;
 end;
 
 if lConstr^.fConstrKind = coAggregate then
  with AggrConstrPtr(lConstr)^ do
 begin
  fAggregBase:= lAggregBase;
  XMLASSERT( nElKind = elFields);
  fAggrColl:=new(PCollection, Init(2,2));
  NextElementState;
  while not (nState = eEnd) do
  begin
   fAggrColl^.Insert( new( PIntItem, Init( GetIntAttr( atNr))));
   AcceptEndState; NextElementState;
  end;
  NextElementState;
 end;

 NextElementState;
 In_Constructor1 := lConstr;
end;

function InVRFFileObj.In_RCluster: RClusterPtr;
var
 lConseq: ClusterRec;
 lPrimaryColl : MCollection;
 lTyp: TypPtr;
 lArticle: string;
 lAbsNr: integer;
begin
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 NextElementState;
 if nElKind = elErrorCluster then
 begin
  In_RCluster:=nil; AcceptEndState;
  AcceptEndState; NextElementState; exit;
 end;
 In_ArgColl( lPrimaryColl);
 lTyp:= In_Type;
 lConseq.Lower:= In_AttrColl;
 lConseq.Upper:= In_AttrColl;
 In_RCluster:= new(RClusterPtr, Init(lAbsNr,lArticle,lConseq, lPrimaryColl, lTyp));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

// no uppercluster for Consequent??
function InMMLFileObj.In_RCluster: RClusterPtr;
var
 lConseq: ClusterRec;
 lPrimaryColl : MCollection;
 lTyp: TypPtr;
 lArticle: string;
 lAbsNr: integer;
begin
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 NextElementState;
 if nElKind = elErrorCluster then
 begin
  In_RCluster:=nil; AcceptEndState;
  AcceptEndState; NextElementState; exit;
 end;
 In_ArgColl( lPrimaryColl);
 lTyp:= In_Type;
 lConseq.Lower:= In_AttrColl;
 lConseq.Upper:= CopyCluster(lConseq.Lower);
 In_RCluster:= new(RClusterPtr, Init(lAbsNr,lArticle,lConseq, lPrimaryColl, lTyp));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

function InVRFFileObj.In_CCluster: CClusterPtr;
var
 lConseq: ClusterRec;
 lAntec: AttrCollectionPtr;
 lPrimaryColl : MCollection;
 lTyp: TypPtr;
 lArticle: string;
 lAbsNr: integer;
begin
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 NextElementState;
 if nElKind = elErrorCluster then
 begin
  In_CCluster:=nil; AcceptEndState;
  AcceptEndState; NextElementState; exit;
 end;
 In_ArgColl( lPrimaryColl);
 lAntec:= In_AttrColl;
 lTyp:= In_Type;
 lConseq.Lower:= In_AttrColl;
 lConseq.Upper:= In_AttrColl;
 In_CCluster:= new(CClusterPtr,
                   Init(lAbsNr,lArticle,lAntec, lConseq, lPrimaryColl, lTyp));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

// no uppercluster for Consequent??
function InMMLFileObj.In_CCluster: CClusterPtr;
var
 lConseq: ClusterRec;
 lAntec: AttrCollectionPtr;
 lPrimaryColl : MCollection;
 lTyp: TypPtr;
 lArticle: string;
 lAbsNr: integer;
begin
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 NextElementState;
 if nElKind = elErrorCluster then
 begin
  In_CCluster:=nil; AcceptEndState;
  AcceptEndState; NextElementState; exit;
 end;
 In_ArgColl( lPrimaryColl);
 lAntec:= In_AttrColl;
 lTyp:= In_Type;
 lConseq.Lower:= In_AttrColl;
 lConseq.Upper:=CopyCluster(lConseq.Lower);
 In_CCluster:= new(CClusterPtr,
                   Init(lAbsNr,lArticle,lAntec, lConseq, lPrimaryColl, lTyp));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

function InVRFFileObj.In_FCluster: FClusterPtr;
var
 lConseq: ClusterRec;
 lPrimaryColl : MCollection;
 lTerm: TrmPtr;
 lTyp: TypPtr;
 lArticle: string;
 lAbsNr: integer;
begin
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 NextElementState;
 if nElKind = elErrorCluster then
 begin
  In_FCluster:=nil; AcceptEndState;
  AcceptEndState; NextElementState; exit;
 end;
 In_ArgColl( lPrimaryColl);
 lTerm:= In_Term;
 lConseq.Lower:= In_AttrColl;
 lConseq.Upper:= In_AttrColl;
 lTyp:=nil;
 if nElKind = elTyp then
  lTyp:= In_Type;
 In_FCluster:= new(FClusterPtr,
                   Init(lAbsNr,lArticle,lConseq, lPrimaryColl, lTerm,lTyp));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

// no uppercluster for Consequent??
function InMMLFileObj.In_FCluster: FClusterPtr;
var
 lConseq: ClusterRec;
 lPrimaryColl : MCollection;
 lTerm: TrmPtr;
 lTyp: TypPtr;
 lArticle: string;
 lAbsNr: integer;
begin
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 NextElementState;
 if nElKind = elErrorCluster then
 begin
  In_FCluster:=nil; AcceptEndState;
  AcceptEndState; NextElementState; exit;
 end;
 In_ArgColl( lPrimaryColl);
 lTerm:= In_Term;
 lConseq.Lower:= In_AttrColl;
 lConseq.Upper:=CopyCluster(lConseq.Lower);
 lTyp:=nil;
 if nElKind = elTyp then
  lTyp:= In_Type;
 In_FCluster:= new(FClusterPtr,
                   Init(lAbsNr,lArticle,lConseq, lPrimaryColl, lTerm,lTyp));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
end;

function InVRFFileObj.In_Identify: IdentifyPtr;
 var lIdentify: IdentifyPtr;
     lArticle: string;
     lAbsNr: integer;
     lKind: Char;
     lPrimaryColl : MCollection;
     lPattern, rPattern: ExprPtr;
     lEqArgs: IntRel;

begin
 XMLASSERT( nElKind = elIdentify);
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 lKind:= GetAttr( atConstrKind)[1];
 NextElementState;
 if nElKind = elErrorIdentify then
  begin In_Identify:=nil;
   AcceptEndState; AcceptEndState;
   NextElementState;
   exit;
  end;
 In_TypeColl( lPrimaryColl);
 case lKind of
  ikTrmFunctor: begin lPattern:=In_Term; rPattern:=In_Term end;
  ikFrmAttr,ikFrmPred: begin lPattern:=In_Formula; rPattern:=In_Formula end;
 end;
 In_IntRel(elEqArgs,lEqArgs);
 lIdentify:=new(IdentifyPtr,Init(lAbsNr,lArticle,lKind,
                                 lPrimaryColl,lPattern,rPattern,lEqArgs));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
 In_Identify:=lIdentify;
end;

function InVRFFileObj.In_Reduction: ReductionPtr;
 var lReduction: ReductionPtr;
     lArticle: string;
     lAbsNr: integer;
     lPrimaryColl : MCollection;
     lTerm, rTerm: TrmPtr;
begin
 XMLASSERT( nElKind = elReduction);
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 NextElementState;
 if nElKind = elErrorReduction then
  begin In_Reduction:=nil;
   AcceptEndState; AcceptEndState;
   NextElementState;
   exit;
  end;
 In_TypeColl( lPrimaryColl);
 lTerm:=In_Term; rTerm:=In_Term;
 lReduction:=new(ReductionPtr,Init(lAbsNr,lArticle,
                                 lPrimaryColl,lTerm,rTerm));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
 In_Reduction:=lReduction;
end;


function InVRFFileObj.In_PropertyReg: PropertyPtr;
 var lProperty: PropertyPtr;
     lArticle: string;
     lAbsNr,lPropertyKind: integer;
     lPrimaryColl : MCollection;
     lObject: PObject;
begin
 XMLASSERT( nElKind = elProperty);
 lArticle:= GetAttr( atAid);
 lAbsNr:= GetIntAttr( atNr);
 lPropertyKind:=GetIntAttr( atX);
 NextElementState;
 In_ArgList( lPrimaryColl);
 lObject:=In_Type;
 lProperty:=new(PropertyPtr,Init(lAbsNr,lArticle,
                                 lPrimaryColl,lPropertyKind,lObject));
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
 In_PropertyReg:=lProperty;
end;

// ##TODO: unified I/O for VRF and MML - now postponing because
//         there might be conflict caused by ikCluFunctor = ikItmCluFunctor
function InMMLFileObj.In_Cluster: ClusterPtr;
begin
 case nElKind of
  elRCluster		: In_Cluster	:= In_RCluster;
  elFCluster		: In_Cluster	:= In_FCluster;
  elCCluster		: In_Cluster	:= In_CCluster;
 else RunTimeError(3333);
 end;
end;

procedure InVRFFileObj.In_IntRel( aElem: TXMLElemKind;
                        var aRel: IntRel);
begin
 XMLASSERT( nElKind = aElem);
 aRel.Init(0);
 NextElementState;
 while not (nState = eEnd) do
 begin
  XMLASSERT( nElKind = elPair);
  aRel.AssignPair( GetIntAttr( atX), GetIntAttr( atY));
  AcceptEndState; NextElementState;
 end;
 aRel.SetLimit(0);
 NextElementState;
end;

procedure InVRFFileObj.In_IntSeq( aElem: TXMLElemKind;
                                  var aSeq: IntSequence);
begin
 XMLASSERT( nElKind = aElem);
 aSeq.Init(0);
 NextElementState;
 while not (nState = eEnd) do
 begin
  aSeq.Insert( GetIntAttr( atX));
  AcceptEndState; NextElementState;
 end;
 NextElementState;
end;

procedure InVRFFileObj.In_NatFunc( aElem: TXMLElemKind;
                                  var aFunc: NatFunc);
begin
 XMLASSERT( nElKind = aElem);
 aFunc.InitNatFunc(8,8);
 NextElementState;
 while not (nState = eEnd) do
 begin
  XMLASSERT( nElKind = elPair);
  aFunc.Assign( GetIntAttr( atX), GetIntAttr( atY));
  AcceptEndState; NextElementState;
 end;
 aFunc.SetLimit(0);
 NextElementState;
end;

procedure InVRFFileObj.In_EndPos( var aPos:Position);
begin
 XMLASSERT( nElKind = elEndPosition);
 GetPosAttrs( aPos);
 AcceptEndState; NextElementState;
end;

function InVRFFileObj.In_Pattern: PatternPtr;
var
 lPattern: PatternPtr; lKind: NotationKind;
 lLex,lConstr: Lexem; lAnt,lStr,lAid: string; lNr,ec: integer;
begin
 lNr:= GetIntAttr( atNr);
 lAid:= GetAttr( atAid);
 GetLexemAttrs( atKind, atFormatNr, lLex);
 GetLexemAttrs( atConstrKind, atConstrNr, lConstr);
 if not GetOptAttr( atAntonymic, lAnt) then lAnt:= 'false';
 lKind:= NotatKind( lLex.Kind);
 lPattern:= new( PatternPtr, Init( lKind, lNr, lAid));
 if GetOptAttr( atRedefNr, lStr) then 
  Val( lStr, lPattern^.fRedefNr, ec);
 NextElementState;
 
 with lPattern^ do
 begin
  fFormNr:= lLex.Nr;
  rConstr:= lConstr;
  fAntonymic:= lAnt = 'true';
  
  fPrimTypes.Init(MaxArgNbr);
  NextElementState;
  while not (nState = eEnd) and (nElKind = elTyp) do
  begin
   fPrimTypes.Insert(In_Type);
   LocArgTyp[fPrimTypes.Count]:= fPrimTypes.Last;
  end;
  fPrimTypes.SetLimit(0);
  MizAssert( errElRedundant, nState = eEnd);
  NextElementState;  
  In_IntSeq( elVisible, Visible);
  
  if not (nState = eEnd) then
  begin
   XMLASSERT( nElKind = elExpansion);
   NextElementState;
   Expansion:= In_Type;
   NextElementState;
  end;
 end;
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
 In_Pattern:= lPattern;
end;
 
 
function InMMLFileObj.In_PatternWithFormat: PatternPtr;
var
 lPattern: PatternPtr; lKind: NotationKind;
 lConstr: Lexem; lAnt,lStr,lAid: string; lNr,ec:integer;
begin
 lNr:= GetIntAttr( atNr);
 lAid:= GetAttr( atAid);
 GetLexemAttrs( atConstrKind, atConstrNr, lConstr);
 if not GetOptAttr( atAntonymic, lAnt) then lAnt:= 'false';
 lKind:= NotatKind( GetAttr( atKind)[1]);
 lPattern:= new( PatternPtr, Init( lKind, lNr, lAid));
 if GetOptAttr( atRedefNr, lStr) then
  Val( lStr, lPattern^.fRedefNr, ec);
 NextElementState;
 
 with lPattern^ do
 begin
  rConstr:= lConstr;
  fAntonymic:= lAnt = 'true';
  fFormat:= In_Format( Self);
  In_ArgList( fPrimTypes);
  In_IntSeq( elVisible, Visible);
  
  if not (nState = eEnd) then
  begin
   XMLASSERT( nElKind = elExpansion);
   NextElementState;
   Expansion:= In_Type;
   NextElementState;
  end;
 end;
 MizAssert( errElRedundant, nState = eEnd);
 NextElementState;
 In_PatternWithFormat:= lPattern;
end;

function InVRFFileObj.In_Def: DefPtr;
var
  lDefSort     : char;
  lPartialPart : MCollection;
  lOtherwise   : PObject;
  lCaseDef     : PObject;
  lGuard       : FrmPtr;
begin
  lDefSort:= GetAttr( atKind)[1];
  NextElementState;
  lPartialPart.Init(0,4);
  while not (nState = eEnd) and (nElKind = elPartialDef) do
  begin
   NextElementState;
   case lDefSort of
    'm': lCaseDef:=In_Formula;
    'e': lCaseDef:=In_Term;
   end;
   lGuard:=In_Formula;
   lPartialPart.Insert(new(PartDefPtr,Init(lCaseDef,lGuard)));
   NextElementState;
  end;
  lOtherWise:=nil;
//AN: work-around FPC 2.2.x bug - restore to check if it's been fixed!
//  if not (nState = eEnd) and (nElKind in (FrmElKinds + TermElKinds)) then
  if (nState <> eEnd) and ((nElKind in FrmElKinds) or (nElKind in TermElKinds)) then
   case lDefSort of
    'm': lOtherWise:=In_Formula;
    'e': lOtherWise:=In_Term;
   end;
  MizAssert( errElRedundant, nState = eEnd);
  NextElementState;
  In_Def:=new(DefPtr,Init(lDefSort,lPartialPart,lOtherwise));
end;

function InVRFFileObj.In_ConstrDef: ConstrDefPtr;
var
  lConstr      : Lexem;
  lPrimaryColl : MCollection;
  lPattern     : PObject;
begin
  XMLASSERT( nElKind = elConstrDef);
  GetLexemAttrs( atConstrKind, atConstrNr, lConstr);
  NextElementState;
  In_TypeColl( lPrimaryColl);
  case lConstr.Kind of
  ikTrmFunctor: lPattern:=In_Term;
  ikFrmAttr,ikFrmPred: lPattern:=In_Formula;
  end;
  MizAssert( errElRedundant, nState = eEnd);
  NextElementState;
  In_ConstrDef:=new(ConstrDefPtr,InitConstrDef(lConstr.Kind,lConstr.Nr,
                                               lPrimaryColl,lPattern));
end;

function InVRFFileObj.In_Definiens: DefiniensPtr;
var
  lConstr      : Lexem;
  lPrimaryColl : MCollection;
  lEssentials  : IntSequence;
  lAssum       : FrmPtr;
  lDef	       : DefPtr;
  lDefNr       : integer;
  lArticle     : string;
begin
  XMLASSERT( nElKind = elDefiniens);
  GetLexemAttrs( atConstrKind, atConstrNr, lConstr);
  lArticle:= GetAttr( atAid);
  lDefNr:= GetIntAttr( atDefNr);
  NextElementState;
  In_TypeColl( lPrimaryColl);
  In_IntSeq( elEssentials, lEssentials);
  if nElKind in FrmElKinds then lAssum:=In_Formula
  else lAssum:=NewVerum;
  lDef:=In_Def;
  MizAssert( errElRedundant, nState = eEnd);
  NextElementState;
  In_Definiens:=new(DefiniensPtr,Init(lConstr.Kind,lConstr.Nr,lDefNr,0,lArticle,
                                      lPrimaryColl,lEssentials,lAssum,lDef));
end;

// ##TODO: the InEnvXXX stuff should be merged with the InVRF and InMML
//         stuff; there is too much of these I/O flavors
// ###TODO: kill this; why the LocArgTyp stuff here???
function InVRFFileObj.In_EnvDefiniens: DefiniensPtr;
var
  lConstr      : Lexem;
  lPrimaryColl : MCollection;
  lEssentials  : IntSequence;
  lAssum       : FrmPtr;
  lDef	       : DefPtr;
  lTyp	       : TypPtr;
  lDefNr       : integer;
  lArticle     : string;
begin
  XMLASSERT( nElKind = elDefiniens);
  GetLexemAttrs( atConstrKind, atConstrNr, lConstr);
  lArticle:= GetAttr( atAid);
  lDefNr:= GetIntAttr( atDefNr);
  NextElementState;
// ###TODO: this is the part which differs from InVRF - really needed?
  lPrimaryColl.Init(0,MaxArgNbr);
  while nElKind = elTyp do
  begin
   lTyp:=In_Type;
   lPrimaryColl.Insert(lTyp);
   LocArgTyp[lPrimaryColl.Count]:=lTyp;
  end;
  lPrimaryColl.SetLimit(0);
  In_IntSeq( elEssentials, lEssentials);
  if nElKind in FrmElKinds then lAssum:=In_Formula
   else lAssum:=NewVerum;
  lDef:=In_Def;
  MizAssert( errElRedundant, nState = eEnd);
  NextElementState;
 In_EnvDefiniens:=new(DefiniensPtr,Init(lConstr.Kind,lConstr.Nr,lDefNr,0,lArticle,
                                  lPrimaryColl,lEssentials,lAssum,lDef));
end;

procedure LoadDefinitions;
var lInEnvFile: InEnvFilePtr;
begin
 if not MFileExists(EnvFileName+'.dfs') then exit;
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.dfs'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elDefinientia);
  NextElementState;
  while not (nState = eEnd) do
  begin
   Definientia.Insert( In_EnvDefiniens);
   gTermCollection.FreeAll;
  end;
 end;
 dispose(lInEnvFile,Done);
end;

procedure LoadEqualities;
var lInEnvFile: InEnvFilePtr;
begin
 if not MFileExists(EnvFileName+'.dfe') then exit;
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.dfe'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elDefinientia);
  NextElementState;
  while not (nState = eEnd) do
  begin
   EqDefinientia.Insert( In_EnvDefiniens);
   gTermCollection.FreeAll;
  end;
 end;
 dispose(lInEnvFile,Done);
end;

procedure LoadExpansions;
var lInEnvFile: InEnvFilePtr;
begin
 if not MFileExists(EnvFileName+'.dfx') then exit;
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.dfx'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elDefinientia);
  NextElementState;
  while not (nState = eEnd) do
  begin
   ExDefinientia.Insert( In_EnvDefiniens);
   gTermCollection.FreeAll;
  end;
 end;
 dispose(lInEnvFile,Done);
end;

procedure LoadIdentify;
var lInEnvFile: InEnvFilePtr;
begin
 if not MFileExists(EnvFileName+'.eid') then exit;
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.eid'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elIdentifyRegistrations);
  NextElementState;
  while not (nState = eEnd) do
  begin
   gIdentifications.Insert( In_Identify);
   gTermCollection.FreeAll;
  end;
 end;
 dispose(lInEnvFile,Done);
end;

procedure LoadReductions;
var lInEnvFile: InEnvFilePtr;
begin
 if not MFileExists(EnvFileName+'.erd') then exit;
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.erd'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elReductionRegistrations);
  NextElementState;
  while not (nState = eEnd) do
  begin
   gReductions.Insert( In_Reduction);
   gTermCollection.FreeAll;
  end;
 end;
 dispose(lInEnvFile,Done);
end;

procedure LoadPropertiesReg;
var lInEnvFile: InEnvFilePtr;
begin
 if not MFileExists(EnvFileName+'.epr') then exit;
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.epr'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elPropertyRegistration);
  NextElementState;
  while not (nState = eEnd) do
  begin
   gPropertiesList.Insert( In_PropertyReg);
   gTermCollection.FreeAll;
  end;
 end;
 dispose(lInEnvFile,Done);
end;

procedure InVRFFileObj.In_Inference(var aInf:InferenceObj);
var lStr: string;
    lLabNr,lArticleNr,lTheoNr,lDefNr: integer;
    lRefPos: Position;
begin
 aInf.Init;
 with aInf do
 begin
  XMLASSERT( nElKind in [ elBy, elFrom, elErrorInf]);
  if nElKind = elErrorInf then
  begin
   nInferSort:= ikError; AcceptEndState;
   NextElementState; exit;
  end;
  if nElKind = elBy then
  begin
   nInferSort:= ikInfBy;
   if GetOptAttr( atLinked, lStr) then nLinked:= lStr = 'true';
  end
  else begin
   nInferSort:= ikInfFrom;
   nSchFileNr:= GetIntAttr( atArticleNr);
   nSchemeNr:= GetIntAttr( atNr);
  end;
  GetPosAttrs( nPos);
  NextElementState;
  while not (nState = eEnd) do
   if not GetOptAttr( atKind, lStr) then
    begin
     XMLASSERT( nElKind = elRef);
     lLabNr:= GetIntAttr( atNr);
     GetPosAttrs(lRefPos);
     AcceptEndState; NextElementState;
     nReferences.Insert(new(PPrivateReference,Init(lLabnR,0,lRefPos)))
    end
   else if lStr[1] = 'T' then
    begin
     XMLASSERT( nElKind = elRef);
     lTheoNr:= GetIntAttr( atNr);
     lArticleNr:= GetIntAttr( atArticleNr);
     GetPosAttrs( lRefPos);
     AcceptEndState; NextElementState;
     nReferences.Insert(new(PTheoremReference,Init(lArticleNr,lTheoNr,lRefPos)));
    end
   else
    begin
     XMLASSERT( nElKind = elRef);
     lDefNr:= GetIntAttr( atNr);
     lArticleNr:= GetIntAttr( atArticleNr);
     GetPosAttrs( lRefPos);
     AcceptEndState; NextElementState;
     nReferences.Insert(new(PDefinitionReference,Init(lArticleNr,lDefNr,lRefPos)));
    end;
  nReferences.SetLimit(0);
  NextElementState;
 end;
end;

procedure InVRFFileObj.In_IterStep(var aTrm:TrmPtr; var aInf:InferenceObj);
begin
 XMLASSERT( nElKind = elIterStep);
 NextElementState;
 aTrm:=In_Term;
 In_Inference(aInf);
 NextElementState;
end;

function InMMLFileObj.AssignedLibr(aName,aExt:string):string;
 var lPath: string;
begin lPath:=LibraryPath(LowerCase(aName),aExt);
 AssignedLibr:=lPath;
 if lPath='' then exit;
 InMMLFileObj.OpenFile(lPath);
end;

procedure InMMLFileObj.GetConstrNames(var aConstrIds: MStringList);
begin
 XMLASSERT( nElKind = elSignature);
 aConstrIds.Init(40);
 NextElementState;
 while not (nState = eEnd) do
 begin
  aConstrIds.AddString( GetAttr( atName));
  AcceptEndState; NextElementState;
 end;
 NextElementState;
end;

procedure InMMLFileObj.In_ConstrCounts1(var aCounts:ConstrIntArr);
var lLex: Lexem;
begin
 while not (nState = eEnd) do
 begin
  XMLASSERT( nElKind = elConstrCount);
  GetLexemAttrs( atKind, atNr, lLex);
  aCounts[ ConstructorKind( lLex.Kind)]:= lLex.Nr;
  AcceptEndState; NextElementState;
 end;
end;

procedure InMMLFileObj.In_ConstrCounts(var aCounts:ConstrIntArr);
begin
 XMLASSERT( nElKind = elConstrCounts); 
 NextElementState;
 In_ConstrCounts1( aCounts);
 NextElementState;
end;

procedure InMMLFileObj.In_Vocs( var aVocs:MStringList);
var 
 lName: string; lLex: Lexem;
 lTokBase: SymbolCounters;
 lDictBase: AbsVocabularyPtr;
begin
 XMLASSERT( nElKind = elVocabularies);
 aVocs.Init(8);
 FillChar( lTokBase, sizeof(lTokBase), 0);
 NextElementState;
 while not (nState = eEnd) and (nElKind = elVocabulary) do 
 begin
  NextElementState;
  lName:= GetAttr( atName);
  AcceptEndState; NextElementState; // end of elArticleID
  lDictBase:=new(AbsVocabularyPtr,Init);
  lDictBase^.fSymbolCnt:=lTokBase;
  while not (nState = eEnd) and (nElKind = elSymbolCount) do
  begin
   GetLexemAttrs( atKind, atNr, lLex);
   inc(lDictBase^.fSymbolCnt[ lLex.Kind], lLex.Nr);
   AcceptEndState; NextElementState;
  end;
  aVocs.AddObject( lName, lDictBase);
  NextElementState;
 end;
 NextElementState;
end;


(********** OutXVRFFileObj **********)

constructor OutVRFFileObj.OpenFile(const AFileName:string);
begin
 inherited OpenFile( AFileName);
 nExpandInferConsts := true;
end;

// add the stylesheet procesing info
constructor OutVRFFileObj.OpenFileWithXSL(const AFileName:string);
begin
 OpenFile( AFileName);
 OutString('<?xml-stylesheet type="text/xml" href="file://' +
            MizFiles + 'miz.xml"?>'+ #10);
end;


// the renumbering functions
function OutVRFFileObj.Transf(fKind : ConstructorsKind; fNr: integer):integer;
begin Transf := fNr; end;

// ##TODO: remove the global variable DoCtrans completely,
//         use overloading of the Transf function
function OutMMLFileObj.Transf(fKind : ConstructorsKind; fNr: integer):integer;
begin
   if DoCtrans then
    Transf := gTrans[fKind].Value(fNr)
  else
    Transf := fNr;
end; 

function OutEnvFileObj.Transf(fKind : ConstructorsKind; fNr: integer): integer;
begin
 Transf := gChosen[ fKind].ElemNr( fNr) + 1;
end;

procedure OutVRFFileObj.Out_TermList ( fTrmList:TrmList );
begin
 while fTrmList<>nil do
  begin Out_Term(fTrmList^.XtrmPtr);
   fTrmList:=fTrmList^.NextTrm;
  end;
end;

(* ##RNC:
## Adjective is a possibly negated (and paramaterized) attribute
## Optionally the article id (atAid) and order in article (atAbsNr)
## can be given. If available, presentational info
## (number of the Pattern) is given in atPid. The heuristic for
## for displaying clusters is that attributes without atPid have been
## added automatically by cluster mechanisms.   
## The attribute kind (atKind) 'V' can be added explicitly.   
elAdjective =
 element elAdjective {
   attribute atNr { xsd:integer },
   attribute atValue { xsd:boolean }?,
   ( attribute atAbsNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   attribute atKind { "V" }?,
   attribute atPid { xsd:integer }?,
   Term*
 }
*)
procedure OutVRFFileObj.Out_Attr(fAttr:AttrPtr);
begin
 Out_XElStart( elAdjective);
 with fAttr^ do
 begin
  Out_XIntAttr( atNr, Transf( coAttribute, fAttrNr));
  if fNeg = 0 then Out_XAttr( atValue, 'false');
  if nPattNr <> 0 then Out_XIntAttr( atPid, nPattNr);
  if not Assigned( fAttrArgs) then Out_XElEnd0
  else begin
   Out_XAttrEnd;
   Out_TermList( fAttrArgs);
   Out_XElEnd( elAdjective);
  end;
 end;
end;

(* ##RNC:
## Cluster of adjectives
elCluster =   
 element elCluster {
   elAdjective*
}
*)
procedure OutVRFFileObj.Out_AttrColl(aCluster: AttrCollectionPtr);
var i: integer;
begin
 Out_XElStart( elCluster);
 if aCluster^.Count = 0 then begin Out_XElEnd0; exit; end;
 Out_XAttrEnd;
 with aCluster^ do for i:=0 to Count-1 do Out_Attr( Items^[i]);
 Out_XElEnd( elCluster);
end;

(* ##RNC:
## Parameterized type - either mode or structure
## The kinds "L" and "G" are equivalent, "G" is going to be
## replaced by more correct "L" in Mizar gradually.   
## First goes the LowerCluster, than UpperCluster
## Optionally the article id (atAid) and order in article (atAbsNr)
## can be given. If available, presentational info
## (number of the Pattern) is given in atPid, and   
## presentational info about variable introduced (e.g. in elFraenkel)
## may be given in atVid.
elTyp =
 element elTyp {
   attribute atKind { "M" | "G" | "L" | "errortyp" },
   attribute atNr { xsd:integer }?,
   ( attribute atAbsNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   attribute atPid { xsd:integer }?,
   attribute atVid { xsd:integer }?,
   elCluster*,
   Term*
 }
*)
procedure OutVRFFileObj.Out_Type ( fTyp: TypPtr);
begin Out_TypeWithId ( fTyp, 0); end;

procedure OutVRFFileObj.Out_TypeWithId ( fTyp: TypPtr; fVarId:integer);
begin
 Out_XElStart( elTyp);
 with TypPtr(fTyp)^ do
 begin
  case TypSort of
   ikTypMode,ikTypStruct:
    begin
     Out_XAttr( atKind, TypSort);
     Out_XIntAttr( atNr, Transf( ConstructorKind( TypSort), ModNr));
     if nPattNr <> 0 then Out_XIntAttr( atPid, nPattNr);
     if fVarId <> 0 then Out_XIntAttr( atVid, fVarId);
     Out_XAttrEnd;
     Out_AttrColl( LowerCluster);
     Out_AttrColl( UpperCluster);
     Out_TermList( ModArgs);
     Out_XElEnd( elTyp);
    end;
   ikError:
    begin
     Out_XAttr( atKind, 'errortyp');
     Out_XIntAttr( atNr, 0);
     Out_XAttrEnd;
     Out_XElEnd( elTyp);
    end;
  else
  begin
{$IFDEF MDEBUG}
   InfoChar(TypSort);
{$ENDIF}
   RunTimeError(2039);
  end;   
  end;
 end;
end;

procedure OutMMLFileObj.Out_Type ( fTyp: TypPtr);
begin Out_TypeWithId ( fTyp, 0); end;

// no uppercluster, no error??
procedure OutMMLFileObj.Out_TypeWithId ( fTyp: TypPtr; fVarId:integer);
begin
 Out_XElStart( elTyp);
 with TypPtr(fTyp)^ do
 begin
  Out_XAttr( atKind, TypSort);
  Out_XIntAttr( atNr, Transf( ConstructorKind( TypSort), ModNr));
  if nPattNr <> 0 then Out_XIntAttr( atPid, nPattNr);
  if fVarId <> 0 then Out_XIntAttr( atVid, fVarId);
  Out_XAttrEnd;
  Out_AttrColl( LowerCluster);
  Out_TermList( ModArgs);
 end;
 Out_XElEnd( elTyp); 
end; 

procedure OutVRFFileObj.Out_TypeList(const fTypeColl: MList);
 var z: integer;
begin
 with fTypeColl do for z:=0 to Count-1 do Out_Type(TypPtr(Items^[z]));
end;

procedure OutVRFFileObj.Out_TypeListWithIds(const fTypeColl: MList; const fIdents:IntSequence);
 var z: integer;
begin
 Mizassert(errBadTypeIds, fIdents.fCount = fTypeColl.Count);
 with fTypeColl do for z:=0 to Count-1 do Out_TypeWithId(TypPtr(Items^[z]), fIdents.Value(z));
end;

procedure OutVRFFileObj.Out_FormulaColl(const fFrmColl: MCollection);
  var z: integer;
begin
  with fFrmColl do for z:=0 to Count-1 do Out_Formula(FrmPtr(Items^[z]));
end;

(* ##RNC:
## Qualification formula (claims that a term has certaing type)
elIs =
 element elIs {
   Term, elTyp
 }
*)
procedure OutVRFFileObj.Out_QualFormula( aFrm: QualFrmPtr);
begin
 Out_XElStart0( elIs);
 Out_Term(aFrm^.QualTrm);
 Out_Type(aFrm^.QualTyp);
 Out_XElEnd( elIs);
end;

(* ##RNC:
## Universally quantified formula
## If available, presentational info is given in atPid.   
## If available, numbere of the variable identifier is
## given in atVid.      
elFor =
 element elFor {
   attribute atPid { xsd:integer }?,
   attribute atVid { xsd:integer }?,
   elTyp, Formula
 }
*)
procedure OutVRFFileObj.Out_UnivFormula( aFrm: UnivFrmPtr);
begin
 if (aFrm^.nPattNr <> 0) or (aFrm^.nVarId <> 0) then
 begin
  Out_XElStart( elFor);
  Out_XIntAttr( atPid, aFrm^.nPattNr);
  Out_XIntAttr( atVid, aFrm^.nVarId);
  Out_XAttrEnd;
 end
 else Out_XElStart0( elFor);
 Out_Type(aFrm^.Quantified);
 Out_Formula(aFrm^.Scope);
 Out_XElEnd( elFor);
end;

procedure OutVRFFileObj.Out_FlexFormula( aFrm: FlexFrmPtr);
begin
 if (aFrm^.nPattNr <> 0) {or (aFrm^.nVarId <> 0)} then
 begin
  Out_XElStart( elFlex);
  Out_XIntAttr( atPid, aFrm^.nPattNr);
{  Out_XIntAttr( atVid, aFrm^.nVarId); }
  Out_XAttrEnd;
 end
 else Out_XElStart0( elFlex);
 Out_Formula(aFrm^.nLeftOrigFrm);
 Out_Formula(aFrm^.nRightOrigFrm);
 Out_Term(aFrm^.nLeftTrm);
 Out_Term(aFrm^.nRightTrm);
 Out_Formula(aFrm^.nExpansion);
 Out_XElEnd( elFlex);
end;

(* ##RNC:
## Conjunctive formula.
## If available, presentational info is given in atPid.
elAnd =
 element elAnd {
   attribute atPid { xsd:integer }?,
   Formula*
 }
*)
procedure OutVRFFileObj.Out_ConjFormula( aFrm: ConjFrmPtr);
begin
 if aFrm^.nPattNr <> 0 then
 begin
  Out_XElStart( elAnd);
  Out_XIntAttr( atPid, aFrm^.nPattNr);
  Out_XAttrEnd;
 end
 else Out_XElStart0( elAnd);
 Out_FormulaColl(aFrm^.Conjuncts);
 Out_XElEnd( elAnd);
end;

(* ##RNC:
## Negation.
## If available, presentational info is given in atPid.
elNot =
 element elNot {
   attribute atPid { xsd:integer }?,
   Formula
 }
*)
procedure OutVRFFileObj.Out_NegFormula( aFrm: NegFrmPtr);
begin
 if aFrm^.nPattNr <> 0 then
 begin
  Out_XElStart( elNot);
  Out_XIntAttr( atPid, aFrm^.nPattNr);
  Out_XAttrEnd;
 end
 else Out_XElStart0( elNot);
 Out_Formula( aFrm^.NegArg);
 Out_XElEnd( elNot);
end;

(* ##RNC:
## Atomic predicate formulas - schematic, attributive and normal
## Optionally the article id (atAid) and order in article (atAbsNr)
## can be given. If available, presentational info
## (number of the Pattern) is given in atPid.
elPred =
 element elPred {
   attribute atKind { "P" | "V" | "R" },
   attribute atNr { xsd:integer },
   ( attribute atAbsNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   attribute atPid { xsd:integer }?,
   Term*
 }
*)
procedure OutVRFFileObj.Out_PredFormula( aFrm: PredFrmPtr);
begin
 Out_XElStart( elPred);
 with aFrm^ do
 begin
  Out_XAttr( atKind, aFrm^.FrmSort);
  if FrmSort in [ikFrmAttr,ikFrmPred] then
   Out_XIntAttr( atNr, Transf( ConstructorKind( FrmSort), PredNr))
  else Out_XIntAttr( atNr, PredNr);
  if nPattNr <> 0 then Out_XIntAttr( atPid, nPattNr);
  if not Assigned( PredArgs) then begin Out_XElEnd0; exit; end;
  Out_XAttrEnd;
  Out_TermList( PredArgs);
  Out_XElEnd( elPred);
 end;
end;

(* ##RNC:
## Private predicate with arguments is a shorthand for another formula
elPrivPred =
 element elPrivPred {
   attribute atNr { xsd:integer },
   Term*,
   Formula
 }
*)
procedure OutVRFFileObj.Out_LocPredFormula( aFrm: LocPredFrmPtr);
begin
 Out_XElStart( elPrivPred);
 with aFrm^ do
 begin
  Out_XIntAttr( atNr, PredNr);
  Out_XAttrEnd;
  Out_TermList( PredArgs);
  Out_Formula( PredExp);
 end;
 Out_XElEnd( elPrivPred);
end;

(* ##RNC:
## Verum (true formula)
elVerum =
 element elVerum { empty }
      
## Incorrect (erroneous formula) - e.g. containing undefined symbols
elErrorFrm =
 element elErrorFrm { empty }
*)
procedure OutVRFFileObj.Out_UniqFormula( aFrm: UniqFrmPtr);
begin
 with aFrm^ do
  case FrmSort of
   ikFrmVerum: Out_XEl1( elVerum);
//   ikFrmThesis:Out_XEl1( elThesis);
   ikFrmError: Out_XEl1( elErrorFrm);
  end;
end;

(* ##RNC:
Formula =
   ( elNot | elAnd | elFor | elPred | elPrivPred | elIs
   | elVerum | elErrorFrm )
*)
procedure OutVRFFileObj.Out_Formula ( aFrm:FrmPtr );
begin
 case aFrm^.FrmSort of
  ikFrmNeg	: Out_NegFormula( NegFrmPtr( aFrm));
  ikFrmConj	: Out_ConjFormula( ConjFrmPtr( aFrm));
  ikFrmSchPred,
  ikFrmAttr,
  ikFrmPred	: Out_PredFormula( PredFrmPtr(aFrm));
  ikFrmPrivPred	: Out_LocPredFormula( LocPredFrmPtr( aFrm));
  ikFrmUniv	: Out_UnivFormula( UnivFrmPtr( aFrm));
  ikFrmQual	: Out_QualFormula( QualFrmPtr( aFrm));
  ikFrmFlexConj,
  ikFrmFlexDisj	: Out_FlexFormula( FlexFrmPtr( aFrm));
  ikFrmVerum,
  ikFrmError	: Out_UniqFormula( UniqFrmPtr( aFrm));
 end;
end;


(* ##RNC:
## Normal bound variable (deBruijn index).
## Their types are given in quantification - see elFor, elFraenkel   
elVar =
 element elVar { attribute atNr { xsd:integer } }
   
## Locus variable used usually for pattern matching.
## Their types are given elsewhere in data using them - see e.g. elConstructor
elLocusVar =
 element elLocusVar { attribute atNr { xsd:integer } }
   
## Free variable - used only internally in checker
elFreeVar =
 element elFreeVar { attribute atNr { xsd:integer } }
   
## Lambda variable - unused now
elLambdaVar =
 element elLambdaVar { attribute atNr { xsd:integer } }
   
## Normal local constant introduced e.g. by elLet or elConsider
## presentational info may be given in atVid.
elConst =
   element elConst {
      attribute atNr { xsd:integer },
      attribute atVid { xsd:integer }?
   }
   
## Inference constant - used for internal term sharing
elInfConst =
 element elInfConst { attribute atNr { xsd:integer } }
   
## Numeral
elNum =
 element elNum { attribute atNr { xsd:integer } }
*)
procedure OutVRFFileObj.Out_VarTerm( fTrm: VarTrmPtr);
begin
 Out_XElStart( XVarTrm2El( fTrm^.TrmSort));
 Out_XIntAttr( atNr, fTrm^.VarNr);
 if fTrm^.TrmSort = ikTrmConstant then
  Out_XIntAttr( atVid, FixedVar[fTrm^.VarNr].nIdent);
 Out_XElEnd0;
end;

(* ##RNC:
## Functor terms - schematic, aggregates, normal and selectors
## Optionally the article id (atAid) and order in article (atAbsNr)
## can be given. If available, presentational info
## (number of the Pattern) is given in atPid.
elFunc =
 element elFunc {
   attribute atKind { "F" | "G" | "K" | "U" },
   attribute atNr { xsd:integer },
   ( attribute atAbsNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   attribute atPid { xsd:integer }?,
   Term*
 }
*)
procedure OutVRFFileObj.Out_FuncTerm( fTrm: FuncTrmPtr);
begin
 Out_XElStart( elFunc);
 Out_XAttr( atKind, fTrm^.TrmSort);
 with FuncTrmPtr(fTrm)^ do
 begin
  if TrmSort in [ ikTrmAggreg, ikTrmFunctor, ikTrmSelector ] then
   Out_XIntAttr( atNr, Transf( ConstructorKind( TrmSort), FuncNr))
  else Out_XIntAttr( atNr, FuncNr);
  if nPattNr <> 0 then Out_XIntAttr( atPid, nPattNr);
  if not Assigned( FuncArgs) then begin Out_XElEnd0; exit; end;
  Out_XAttrEnd;
  Out_TermList( FuncArgs);
  Out_XElEnd( elFunc);
 end;
end;

(* ##RNC:
## Private functor with arguments is a shorthand for another term.
## The first (mandatory) term is the expansion, arguments follow.
elPrivFunc =
 element elPrivFunc {
   attribute atNr { xsd:integer },
   Term+
 }
*)
procedure OutVRFFileObj.Out_PrivFuncTerm( fTrm: LocFuncTrmPtr);
begin
 Out_XElStart( elPrivFunc);
 with LocFuncTrmPtr( fTrm)^ do
 begin
  Out_XIntAttr( atNr, FuncNr);
  Out_XAttrEnd;
  Out_Term( FuncExp);
  Out_TermList( FuncArgs);
  Out_XElEnd( elPrivFunc);
 end;
end;

(* ##RNC:
## Fraenkel term is defined by the types of its lambda arguments,
## its lambda term and the separating formula.
## Each type may optionally have presentational info about
## the variable (atVid) inside.
elFraenkel =
 element elFraenkel {
   elTyp*, Term, Formula
 }
*)
procedure OutVRFFileObj.Out_FraenkelTerm( fTrm: FraenkelTrmPtr);
begin
 Out_XElStart0( elFraenkel);
 Out_TypeListWithIds(fTrm^.LambdaArgs,fTrm^.nIdents);
 Out_Term(fTrm^.LambdaScope);
 Out_Formula(fTrm^.Compr);
 Out_XElEnd( elFraenkel);
end;

(* ##RNC:
## Choice term is defined by the type of its argument,
elChoice =
 element elChoice {
   elTyp
 }
*)
procedure OutVRFFileObj.Out_ChoiceTerm( fTrm: ChoiceTrmPtr);
begin
 Out_XElStart0( elChoice);
 Out_Type(fTrm^.ChoiceTyp);
 Out_XElEnd( elChoice);
end;

// ##TODO: why is ikTrmQua in output and not input?
(* ##RNC:
## Qua terms capture the retyping term qua type construct,
## but they are probably no longer used on this level.
elQuaTrm =
 element elQuaTrm {
   Term, elTyp
 }
*)
procedure OutVRFFileObj.Out_QuaTerm( fTrm: QuaTrmPtr);
begin 
 Out_XElStart0( elQuaTrm);
 Out_Term(fTrm^.TrmProper);
 Out_Type(fTrm^.Qua);
 Out_XElEnd( elQuaTrm);
end;

(* ##RNC:
## _It_ is a special term used in definitions.
##  Probably no longer used on this level.
elIt =
 element elIt { empty }
   
## Incorrect (erroneous term) - e.g. containing undefined symbols
elErrorTrm =
 element elErrorTrm { empty }   
*)
procedure OutVRFFileObj.Out_UniqTerm( fTrm: TrmPtr);
begin
  case fTrm^.TrmSort of
   ikTrmIt: Out_XEl1( elIt);
   ikTrmError: Out_XEl1( elErrorTrm);
  end;
end;

// ###TODO: do ikTrmLambdaVar,ikTrmQua,ikTrmIt still exist?

(* ##RNC:
Term =
   ( elVar | elLocusVar | elFreeVar | elLambdaVar
   | elConst | elInfConst | elNum
   | elFunc | elPrivFunc | elFraenkel
   | elQuaTrm | elIt | elChoice | elErrorTrm)
*)
procedure OutVRFFileObj.Out_Term ( fTrm: TrmPtr );
begin
 with TrmPtr(fTrm)^ do
  case TrmSort of
   ikTrmInfConst: if nExpandInferConsts then Out_Term(
                   ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm)^.VarNr])^.fDef)
                  else Out_VarTerm( VarTrmPtr( fTrm));
   ikTrmLocus,
   ikTrmBound,
   ikTrmConstant,
   ikTrmFreeVar,
   ikTrmLambdaVar,
   ikTrmNumeral		: Out_VarTerm( VarTrmPtr( fTrm));
   ikTrmSchFunc,
   ikTrmAggreg,
   ikTrmFunctor,
   ikTrmSelector	: Out_FuncTerm( FuncTrmPtr( fTrm));
   ikTrmPrivFunc	: Out_PrivFuncTerm( LocFuncTrmPtr( fTrm));
   ikTrmFraenkel	: Out_FraenkelTerm( FraenkelTrmPtr( fTrm));
   ikTrmQua		: Out_QuaTerm( QuaTrmPtr( fTrm));
   ikTrmChoice          : Out_ChoiceTerm(ChoiceTrmPtr(fTrm));
   ikTrmIt,
   ikError		: Out_UniqTerm( fTrm);
   else begin
{$IFDEF MDEBUG}
    InfoChar(TrmSort);
{$ENDIF}
    RunTimeError(2041);
   end;
  end;
end;

(* ##RNC:
Position = 
   attribute atLine { xsd:integer},
   attribute atCol { xsd:integer}
*)
procedure OutVRFFileObj.Out_PosAsAttrs(fPos: Position);
begin
  Out_XIntAttr( atLine, fPos.Line);
  Out_XIntAttr( atCol, fPos.Col);
end;

// ##TODO: reading now only in prephan
(* ##RNC:
## Proposition is a sentence with position and possible label (and its identifier).
elProposition =
 element elProposition {
   Position,
   attribute atNr { xsd:integer}?,
   attribute atVid { xsd:integer}?,
   Formula
 }
*)
procedure OutVRFFileObj.Out_Propos( aNr,aId:integer; const aPos: Position;
                                    aFrm: FrmPtr);
begin
 Out_XElStart( elProposition);
 Out_PosAsAttrs( aPos);
 if aNr > 0 then
 begin
  Out_XIntAttr( atNr, aNr);
  Out_XIntAttr( atVid, aId);
 end;
 Out_XAttrEnd;
 Out_Formula( aFrm);
 Out_XElEnd( elProposition);
end;

procedure OutVRFFileObj.Out_Proposition(fProp: PropositionPtr);
begin with fProp^ do Out_Propos( nLabNr, nLabId, nPos, nSentence) end;

// ###TODO: this is compatible with the old analyzer writepropositions,
//         but should be removed
procedure OutVRFFileObj.Out_Propositions(var fProps:MCollection);
var z: integer;
begin
 with fProps do for z:=0 to Count-1 do
  Out_Proposition(PropositionPtr(Items^[z]));
end;

procedure OutVRFFileObj.Out_Propositions1(var fProps:MCollection);
begin Out_Propositions(fProps); end;

(* ##RNC:
## Argument types of constructors, patterns, clusters, etc.
elArgTypes =   
 element elArgTypes { elTyp* }
*)
procedure OutVRFFileObj.Out_ArgTypes( const fTypeColl: MList);
begin
 if fTypeColl.Count = 0 then Out_XEl1( elArgTypes)
 else
 begin
  Out_XElStart0( elArgTypes);
  Out_TypeList( fTypeColl);
  Out_XElEnd( elArgTypes);
 end;
end;

(* ##RNC:
## This encodes error during cluster processing
elErrorCluster =
 element elErrorCluster { empty }   
   
## Existential (registration) cluster.
## This says that exists elTyp with elCluster (optionally followed
## by its extended version created by rounding up in current environment).   
## Optionally the article id (atAid) and order in article (atNr)
## can be given.   
elRCluster =   
 element elRCluster {
   ( attribute atNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   ( elErrorCluster | (elArgTypes, elTyp, elCluster, elCluster?) )
   }
*)
// see also OutVRFFileObj.Out_ErrCluster
procedure OutVRFFileObj.Out_RCluster( fCluster: RClusterPtr);
begin with fCluster^ do
 begin
  Out_XElStart( elRCluster);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttrEnd;
  Out_ArgTypes( nPrimaryList);
  Out_Type( nClusterType);
  Out_AttrColl( nConsequent.Lower);
  Out_AttrColl( nConsequent.Upper);
  Out_XElEnd( elRCluster);
 end;
end;

// no uppercluster for Consequent??
procedure OutMMLFileObj.Out_RCluster( fCluster: RClusterPtr);
begin with fCluster^ do
 begin
  Out_XElStart( elRCluster);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttrEnd;
  Out_ArgTypes( nPrimaryList);
  Out_Type( nClusterType);
  Out_AttrColl( nConsequent.Lower);
  Out_XElEnd( elRCluster);
 end;
end;
(* ##RNC:
## Conditional cluster.
## This says that elTyp with the first cluster has also the second
## (optionally followed by its extended version created by rounding
## up in current environment).
## Optionally the article id (atAid) and order in article (atNr)
## can be given.
elCCluster =
 element elCCluster {
   ( attribute atNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   ( elErrorCluster | (elArgTypes, elCluster, elTyp, elCluster, elCluster?) )
   }
*)
// see also OutVRFFileObj.Out_ErrCluster
procedure OutVRFFileObj.Out_CCluster( fCluster: CClusterPtr);
begin with fCluster^ do
 begin
  Out_XElStart( elCCluster);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttrEnd;
  Out_ArgTypes( nPrimaryList);
  Out_AttrColl(fCluster^.nAntecedent);
  Out_Type( nClusterType);
  Out_AttrColl( nConsequent.Lower);
  Out_AttrColl( nConsequent.Upper);
  Out_XElEnd( elCCluster);
 end;
end;

// no uppercluster for Consequent??
procedure OutMMLFileObj.Out_CCluster( fCluster: CClusterPtr);
begin with fCluster^ do
 begin
  Out_XElStart( elCCluster);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttrEnd;
  Out_ArgTypes( nPrimaryList);
  Out_AttrColl(fCluster^.nAntecedent);
  Out_Type( nClusterType);
  Out_AttrColl( nConsequent.Lower);
  Out_XElEnd( elCCluster);
 end;
end;

(* ##RNC:
## Functor (term) cluster.
## This says that Term with elArgTypes has elCluster (optionally followed
## by its extended version created by rounding up in current environment),
## optionally with explicit elTyp.
## Optionally the article id (atAid) and order in article (atNr)
## can be given.
elFCluster =
 element elFCluster {
   ( attribute atNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   ( elErrorCluster | (elArgTypes, Term, elCluster, elCluster?, elTyp?) )
   }
*)
// see also OutVRFFileObj.Out_ErrCluster
procedure OutVRFFileObj.Out_FCluster( fCluster: FClusterPtr);
begin with fCluster^ do
 begin
  Out_XElStart( elFCluster);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttrEnd;
  Out_ArgTypes(nPrimaryList);
  Out_Term(nClusterTerm);
  Out_AttrColl(nConsequent.Lower);
  Out_AttrColl(nConsequent.Upper);
  if nClusterType <> nil then
    Out_Type( nClusterType);
  Out_XElEnd( elFCluster);
 end;
end;

// Error Clusters: Existential, Conditional, Functor
procedure OutVRFFileObj.Out_ErrCluster( fClusterKind: TXMLElemKind );
begin
   Out_XElStart(fClusterKind);
   Out_XAttr( atAid, ArticleID);
   Out_XIntAttr( atNr, 0);
   Out_XAttrEnd;
   Out_XEl1(elErrorCluster);
   Out_XElEnd(fClusterKind);
end;

// no uppercluster for Consequent??
procedure OutMMLFileObj.Out_FCluster( fCluster: FClusterPtr);
begin with fCluster^ do
 begin
  Out_XElStart( elFCluster);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttrEnd;
  Out_ArgTypes(nPrimaryList);
  Out_Term(nClusterTerm);
  Out_AttrColl(nConsequent.Lower);
  if nClusterType <> nil then
    Out_Type( nClusterType);
  Out_XElEnd( elFCluster);
 end;
end;

procedure OutVRFFileObj.Out_Cluster( fCl:ClusterPtr);
begin
 case fCl^.nClusterKind of
  clRegistered	: Out_RCluster(RClusterPtr(fCl));
  clFunctor	: Out_FCluster(FClusterPtr(fCl));
  clConditional	: Out_CCluster(CClusterPtr(fCl));
 end;
end;

(* ##RNC:
## Identification (unoriented, this is not used currently, see identifyWithExp instead).
## This says that two terms with the two constructors at the top
## are equal when the pairs of their arguments specified in elEqArgs
## are equal.
## Optionally the article id (atAid) and order in article (atNr)
## can be given.
elIdentify =
 element elIdentify {
   attribute atNr { xsd:integer },
      attribute atAid { xsd:string },
      attribute atConstrKind { 'K' | 'U' | 'G' | 'V' | 'R'},
    ( elErrorIdentify |
      ( elTyp*,
       ((Term, Term) | (Formula, Formula)),
        element elEqArgs { elPair* }
      ))
    }
*)
procedure OutVRFFileObj.Out_Identify( fIdentify: IdentifyPtr);
begin
 with fIdentify^ do
 begin
  Out_XElStart( elIdentify);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttr( atConstrKind, nConstrKind);
  Out_XAttrEnd;
  Out_TypeList(nPrimaryList);
  case nConstrKind of
  ikTrmFunctor:
   begin Out_Term(TrmPtr(nPattern[0])); Out_Term(TrmPtr(nPattern[1])) end;
  ikFrmAttr, ikFrmPred:
   begin Out_Formula(FrmPtr(nPattern[0])); Out_Formula(FrmPtr(nPattern[1])) end;
  end;
  Out_IntRel(elEqArgs,nEqArgs);
  Out_XElEnd( elIdentify);
 end;
end;

procedure OutVRFFileObj.Out_Reduction( fReduction: ReductionPtr);
begin
 with fReduction^ do
 begin
  Out_XElStart( elReduction);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XAttrEnd;
  Out_TypeList(nPrimaryList);
  Out_Term(TrmPtr(nTerms[0]));
  Out_Term(TrmPtr(nTerms[1]));
  Out_XElEnd( elReduction);
 end;
end;

procedure OutVRFFileObj.Out_PropertyReg(aProperty: PropertyPtr);
begin
 with aProperty^ do
 begin
  Out_XElStart( elProperty);
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atNr, nAbsNr);
  Out_XIntAttr( atX,nPropertyKind);
  Out_XAttrEnd;
  Out_ArgTypes( nPrimaryList);
  Out_Type(TypPtr(nObject));
  Out_XElEnd( elProperty);
 end;

end;

(* ##RNC:
## This encodes error during identification processing
elErrorIdentify =
 element elErrorIdentify { empty }

## Identification (oriented, currently used version).
## This says to identify anything matching the first term
## or formula pattern (with ConstrKind and ConstrNr as the top
## constructor) with the second pattern (instantiated by the matching).
## The type requirements for the matching (i.e. the loci) are given first.
## Note that this works only one way, if you want it also the other way, the
## symmetrical variant has to be explicitly stated as another identification.
## Optionally the article id (atAid) and order in article (atNr)
## can be given.
elIdentifyWithExp =
 element elIdentifyWithExp {
   ( attribute atNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   ( elErrorIdentify
     | ( attribute atConstrKind { 'K' | 'U' | 'G' | 'V' | 'R'},
         attribute atConstrNr { xsd:integer},
         elTyp*,
         ( (Term, Term) | (Formula, Formula) )
       )
   )
   }
*)
(* ##RNC:
## Schemes keep types of their second-order variables.
## First comes the scheme thesis, then the premises.
## The article number and order in article can be given,
## otherwise it belongs to the current article and order is implicit.
## Optional aid attribute specifies article name.
elScheme =
 element elScheme {
   ( attribute atArticleNr { xsd:integer },
     attribute atNr { xsd:integer } )?,
   attribute atAid { xsd:string }?,
   elArgTypes, Formula, Formula*
   }
*)
procedure OutMMLFileObj.PutImpScheme(lSch:SchemePtr);
begin
 if lSch^.fSchProps.Count = 0 then
   Out_XEl1( elCanceled)
 else
  begin
   Out_XElStart0( elScheme);
   Out_ArgTypes(lSch^.fSchTypes);
   Out_FormulaColl(lSch^.fSchProps);
   Out_XElEnd( elScheme);
  end;
end;


(* ##RNC:
## This is now only the unique name of an article.
elArticleID =   
 element elArticleID { 
    attribute atName { xsd:string }
   }
*)
procedure OutMMLFileObj.Out_ArticleName( aName: string);
begin
   Out_XElStart( elArticleID);
   Out_XAttr( atName, aName);
   Out_XElEnd0;
end;

(* ##RNC:
## Signature is a list of articles from which we import constructors.
elSignature =   
 element elSignature { elArticleID* }
*)
procedure OutMMLFileObj.PutConstrNames(var aConstrIds: MStringList);
var i: integer;
begin
 Out_XElStart0( elSignature);
 with aConstrIds do
 for i:=0 to fCount-1 do
   Out_ArticleName( GetString(i));
 Out_XElEnd( elSignature); 
end;

procedure OutMMLFileObj.OutSgn(fImpSgn:MList; WriteCurArticle:boolean);
var i: integer; c: ConstructorsKind;
begin
 if fImpSgn.Count = 0 then Out_XEl1( elSignature)
 else
 begin
  Out_XElStart0( elSignature);
  for i:=0 to fImpSgn.Count-1 do
   Out_ArticleName(MStrPtr( fImpSgn.Items^[i])^.fStr);
  if WriteCurArticle then
   for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
    if Constr[c].Count > ConstrBase[c] then 
    begin
     Out_ArticleName(UpperCase(ArticleName));
     break;
    end;
  Out_XElEnd( elSignature);
 end;
end;

procedure OutMMLFileObj.OutMarkedSgn(var fImpSgn:MList;
                                     var fSgnMarks:NatFunc; fTo:integer);
var i: integer;
begin
 Out_XElStart0( elSignature);
 for i := 0 to fTo do
  if fSgnMarks.HasInDom(i) then
   Out_ArticleName( MStrPtr( fImpSgn.Items^[ i])^.fStr);
 Out_XElEnd( elSignature);
end;

(* ##RNC:
## Constructor counts are used probably for renumerating.
## The article named can be given if not implicit.   
## This implementation might change in some time.
elConstrCounts = 
 element elConstrCounts {
   attribute atName { xsd:string }?,
   element elConstrCount {
    attribute atKind { 'M' | 'L' | 'V' | 'R' | 'K' | 'U' | 'G' },
    attribute atNr { xsd:integer }
   }*
  }   
*) 
procedure OutMMLFileObj.OutConstrCounts1(const fCounts:ConstrIntArr);
var c:ConstructorsKind; 
begin
 for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  if fCounts[c] <> 0 then
  begin
   Out_XElStart( elConstrCount);
   Out_XAttr( atKind, ConstructorRepr(c));
   Out_XIntAttr( atNr, fCounts[c]);
   Out_XElEnd0;
  end;
end;

procedure OutMMLFileObj.OutConstrCounts(const fCounts:ConstrIntArr);
begin
 Out_XElStart0( elConstrCounts);
 OutConstrCounts1( fCounts);
 Out_XElEnd( elConstrCounts);
end;

(* ##RNC:
## Vocabularies keep for each article its symbol numbers.
## This implementation might change in some time.
elVocabularies = 
 element elVocabularies {
   element elVocabulary {
    elArticleID,
    element elSymbolCount {
     attribute atKind {'G'|'K'|'L'|'M'|'O'|'R'|'U'|'V'},
     attribute atNr { xsd:integer }
    }*
   }*
  }
*)
procedure OutMMLFileObj.Out_VocItem( aName:string;
                                     const aSyms: SymbolCounters);
var c : char;
begin
 Out_XElStart0( elVocabulary);
 Out_ArticleName( aName);
 for c:='A' to 'Z' do if c in AvailableSymbols then
 begin
  Out_XElStart( elSymbolCount);
  Out_XAttr( atKind, c);
  Out_XIntAttr( atNr, aSyms[c]);
  Out_XElEnd0;
 end;
 Out_XElEnd( elVocabulary);
end;

procedure OutMMLFileObj.Out_Vocs(var fVocabularies:MStringList);
var i:integer;
begin
 Out_XElStart0( elVocabularies);
 with fVocabularies do
  for i:=0 to fCount-1 do 
   Out_VocItem( GetString(i),
                AbsVocabularyPtr(GetObject(i))^.fSymbolCnt);
 Out_XElEnd( elVocabularies);
end;

procedure OutMMLFileObj.OutMarkedVocs(var fVocabularies:MStringList;
                                      var fDictMarks:NatFunc);
var i:integer;
begin
 Out_XElStart0( elVocabularies);
 with fVocabularies do
  for i:=0 to fCount-1 do if fDictMarks.HasInDom(i) then   
   Out_VocItem( GetString(i),
                AbsVocabularyPtr(GetObject(i))^.fSymbolCnt);
 Out_XElEnd( elVocabularies);
end;


(* ##RNC:
## Single integer
elInt =
 element elInt {
   attribute atX { xsd:integer }
  }
*)
procedure OutVRFFileObj.Out_Int( i:integer);
begin
   Out_XElStart( elInt);
   Out_XIntAttr( atX, i);
   Out_XElEnd0; 
end;

(* ##RNC:
## This is a pair of integers
elPair =
 element elPair {
   attribute atX { xsd:integer },
   attribute atY { xsd:integer }
  }
*)   
procedure OutVRFFileObj.Out_Pair( i,j:integer);
begin
   Out_XElStart( elPair);
   Out_XIntAttr( atX, i);
   Out_XIntAttr( atY, j);
   Out_XElEnd0; 
end;


(* ##RNC:   
## Structural loci are not used yet (that is all I know about them).
elStructLoci = 
 element elStructLoci { elPair* }
*)   
procedure OutVRFFileObj.Out_StructLoci(const fStructLoci: NatFunc);
var i: integer;
begin
 with fStructLoci do
  if Count = 0 then Out_XEl1( elStructLoci)
  else
  begin
   Out_XElStart0( elStructLoci);
   for i:=0 to Count-1 do
    Out_Pair(Items^[i].X, Transf( coStructMode, Items^[i].Y));
   Out_XElEnd( elStructLoci);
  end;
end;

// ##TODO: add position
(* ##RNC:
## Specify fields of aggregates and structmodes by their relative
## atNr.
## Optionally the article id (atAid) and order in article (atAbsNr)
## can be given.
## The selector kind (atKind) 'U' can can be added explicitly.
elFields = 
 element elFields {
   element elField {
     attribute atNr { xsd:integer},
     attribute atKind { 'U' }?,
     attribute atAid { xsd:string}?,
     attribute atAbsNr { xsd:integer}?
   }*
}

Property = 
   ( element elUnexpectedProp { empty }
   | element elSymmetry { empty }
   | element elReflexivity { empty }
   | element elIrreflexivity { empty }
   | element elAssociativity { empty }
   | element elTransitivity { empty }
   | element elCommutativity { empty }
   | element elConnectedness { empty }
   | element elAsymmetry { empty }
   | element elIdempotence { empty }
   | element elInvolutiveness { empty }
   | element elProjectivity { empty }
   | element elAbstractness { empty } )
   
## Properties of constructors; if some given, the first and the   
## second argument to which they apply must be specified.
elProperties =   
 element elProperties {
   attribute atPropertyArg1 { xsd:integer},
   attribute atPropertyArg2 { xsd:integer},
   Property+
 }
   
## Constructors are functors, predicates, attributes, etc.
## atNr, atKind and atAid (article id) determine the constructor
## absolutely in MML, atRelNr optionally gives its serial number
## in environment for a particular article (it is not in prels).   
## All have  (possibly empty) properties, argtypes
## and some have one or more mother types.
## The optional final elFields are selectors for agrregates and structmodes.
## atAggregBase is for aggregates (maybe OVER-arguments),
## atStructModeAggrNr is for structmodes (nr of corresponding aggregate).
## atAbsRedefNr and atRedefAid optionally give absolute address of
## a redefinition.   
elConstructor =
 element elConstructor {
   attribute atKind { 'M' | 'L' | 'V' | 'R' | 'K' | 'U' | 'G' },
   attribute atNr { xsd:integer},
   attribute atAid { xsd:string},
   attribute atRelNr { xsd:integer}?,
   ( attribute atRedefNr { xsd:integer},
     attribute atSuperfluous { xsd:integer},
     ( attribute atAbsRedefNr { xsd:integer},
       attribute atRedefAid { xsd:string} )?
   )?,
   ( attribute atStructModeAggrNr { xsd:integer}
   | attribute atAggregBase { xsd:integer})?,
   elProperties?, elArgTypes, elStructLoci?, elTyp*, elFields?
 }
*)
// fRelNr=0 means that the relative nr is not printed, used for prels 
procedure OutVRFFileObj.Out_Constructor(fConstr:ConstrPtr; fRelNr:integer);
procedure Out_ConstrTypes;
begin
 with fConstr^ do
 begin
  Out_ArgTypes( nPrimaries);
// StructLoci now always empty
//  Out_StructLoci(fStructLoci);
  if fConstrKind in TypedConstrKinds then
   Out_Type(ConstrTypPtr(fConstr)^.fConstrTyp);
 end;
end;
var j: integer; p:PropertyKind;
begin
 Out_XElStart( elConstructor);
 with fConstr^ do
 begin
  Out_XAttr( atKind, ConstructorRepr( fConstrKind));
  Out_XIntAttr( atNr, fAbsNr);
  Out_XAttr( atAid, fArticle);
  if fRelNr <> 0 then Out_XIntAttr( atRelNr, fRelNr);
  case fConstrKind of
   coFunctor,
   coMode,
   coAttribute,
   coPredicate,
   coSelector:
    begin
     if fWhichConstrNr > 0 then
     begin
      Out_XIntAttr( atRedefNr, Transf( fConstrKind, fWhichConstrNr));
      Out_XIntAttr( atSuperfluous, fSuperfluous);
     end;
     Out_XAttrEnd;
     if fProperties <> [] then
     begin
      Out_XElStart( elProperties);
      Out_XIntAttr( atPropertyArg1, fFirstArg);
      Out_XIntAttr( atPropertyArg2, fSecondArg);
      Out_XAttrEnd;
      for p:=Low(PropertyKind) to High(PropertyKind) do
       if p in fProperties then Out_XEl1( Prop2XmlElem[ p]);
      Out_XElEnd( elProperties);
     end;
     Out_ConstrTypes;
    end;
   coStructMode:
    with StructConstrPtr(fConstr)^ do
   begin
    Out_XIntAttr( atStructModeAggrNr, Transf(fConstrKind, fStructModeAggrNr));
    Out_XAttrEnd;
    Out_ConstrTypes;
    Out_TypeList( fPrefixes);
    Out_XElStart0( elFields);
    with fFields^ do
     for j:=0 to Count-1 do
     begin
      Out_XElStart( elField);
      Out_XIntAttr( atNr, Transf(coSelector,Items^[j].X));
      Out_XElEnd0; 
     end;
    Out_XElEnd( elFields);
   end;
   coAggregate:
    with AggrConstrPtr(fConstr)^ do
   begin
    Out_XIntAttr( atAggregBase, fAggregBase);
    Out_XAttrEnd;
    Out_ConstrTypes;
    Out_XElStart0( elFields);
    with fAggrColl^ do
     for j:=0 to Count-1 do
     begin
      Out_XElStart( elField);
      Out_XIntAttr( atNr, Transf(coSelector,PIntItem(Items^[j])^.IntKey));
      Out_XElEnd0;
     end;
    Out_XElEnd( elFields);
   end;
  end;
 end;
 Out_XElEnd( elConstructor);
end;

procedure OutVRFFileObj.Out_IntRel( aElem: TXMLElemKind;
                          const aRel: IntRel);
var i: integer;
begin
 with aRel do
  if Count = 0 then Out_XEl1( aElem)
  else
  begin
   Out_XElStart0( aElem);
   for i:=0 to Count-1 do
    Out_Pair( IntPair(Items^[i]).X, IntPair(Items^[i]).Y);
   Out_XElEnd( aElem);
  end;
end;

procedure OutVRFFileObj.Out_IntSeq( aElem: TXMLElemKind;
                                    const aVisible: IntSequence);
var k: integer;
begin
 with aVisible do
  if fCount = 0 then Out_XEl1( aElem)
  else
  begin
   Out_XElStart0( aElem);
   for k:=0 to fCount-1 do
    Out_Int( fList^[k]);
   Out_XElEnd( aElem);
  end;
end;

procedure OutVRFFileObj.Out_NatFunc( aElem: TXMLElemKind;
                                    const aFunc: NatFunc);
var i: integer;
begin
 with aFunc do
  if Count = 0 then Out_XEl1( aElem)
  else
  begin
   Out_XElStart0( aElem);
   for i:=0 to Count-1 do
    Out_Pair( IntPair(Items^[i]).X, IntPair(Items^[i]).Y);
   Out_XElEnd( aElem);
  end;
end;

(* ##RNC:   
## Ending position (e.g. of blocks).
elEndPosition = 
 element elEndPosition { Position }
*)
procedure OutVRFFileObj.Out_EndPos( const aPos:Position);
begin
 Out_XElStart( elEndPosition);
 Out_PosAsAttrs( aPos);
 Out_XElEnd0;
end;

// ##TODO: expansion also for other notation kinds (e.g. 'J')?
// ###TODO: the value 'J' for atConstrKind should be forbidden.
//   and the grammar strangthened to have either expansion or atConstrKind
(* ##RNC:
## Patterns map formats with argtypes to constructors.
## The format is either specified as a number (then it must
## be available in some table), or is given explicitely.
## elVisible are indeces of visible (nonhidden) arguments.
## If antonymic, its constructor has to be negated. Mode patterns
## can have expansion instead of just a constructor - this might
## be done for other patterns too, or replaced by the _equals_
## mechanism. The J (forgetful functor) patterns are actually
## an example of another expanded patterns, but the expansion 
## is uniform for all of them, so it does not have to be given.
## The invalid ConstrKind J is now used for forgetful functors,
## this should be changed.   
## Optionally the article id (atAid) and order in article (atNr)
## can be given.  atRelNr optionally gives its serial number
## in environment for a particular article (it is not in prels). 
## atRedefNr optonally gives the relative number of the 
## original pattern to which the current is defined as synonym/antonym.
elPattern =
 element elPattern {
   attribute atKind { 'M' | 'L' | 'V' | 'R' | 'K' | 'U' | 'G' | 'J' },
   ( attribute atNr { xsd:integer },
     attribute atAid { xsd:string } )?,
   ( attribute atFormatNr { xsd:integer}
   | elFormat ),
   attribute atConstrKind { 'M' | 'L' | 'V' | 'R' | 'K' | 'U' | 'G' | 'J' },
   attribute atConstrNr { xsd:integer},
   attribute atAntonymic { xsd:boolean }?,
   attribute atRelNr { xsd:integer }?,
   attribute atRedefNr { xsd:integer }?,
   elArgTypes,
   element elVisible { elInt*},
   element elExpansion { elTyp }?
 }
*)
procedure OutVRFFileObj.Out_Pattern( fPatt:PatternPtr; fRelNr:integer);
begin
 with fPatt^ do
 begin
  if fFormNr = 0 then exit;
  Out_XElStart( elPattern);
  Out_XAttr( atKind, NotationRepr( fKind));
  Out_XIntAttr( atNr, fAbsNr);
  Out_XAttr( atAid, fArticle);
  Out_XIntAttr( atFormatNr, fFormNr);
  Out_XAttr( atConstrKind, rConstr.Kind);
  with rConstr do
   Out_XIntAttr( atConstrNr, Transf( ConstructorKind(Kind), Nr));
  if fAntonymic then Out_XAttr( atAntonymic, 'true');
  if fRelNr <> 0 then Out_XIntAttr( atRelNr, fRelNr);
  if fRedefNr <> 0 then Out_XIntAttr( atRedefNr, fRedefNr);
  Out_XAttrEnd;
  Out_ArgTypes( fPrimTypes);
  Out_IntSeq( elVisible, Visible);
  if Expansion<>nil then
  begin
   Out_XElStart0( elExpansion);
   Out_Type(Expansion);
   Out_XElEnd( elExpansion);
  end;
  Out_XElEnd( elPattern);
 end;
end;


(* ##RNC:
## elConstrDef holds a term together with types of its variables and 
## the top-level functor. Used now mainly for identify.
elConstrDef =
 element elConstrDef {
   attribute atConstrKind { 'K' | 'U' | 'G' },
   attribute atConstrNr { xsd:integer},
   elTyp*, Term?
 }
*)

procedure OutVRFFileObj.Out_ConstrDef( aDef:ConstrDefPtr);
begin
 with aDef^ do
 begin
  Out_XElStart( elConstrDef);
  Out_XAttr( atConstrKind, nConstr.Kind);
  Out_XIntAttr( atConstrNr, Transf(ConstructorKind(nConstr.Kind),
                                   nConstr.Nr));
  Out_XAttrEnd;
  Out_TypeList(PrimaryList);
  case nConstr.Kind of
  ikTrmFunctor: Out_Term(TrmPtr(nPattern));
  ikFrmAttr, ikFrmPred: Out_Formula(FrmPtr(nPattern));
  end;
  Out_XElEnd( elConstrDef);
 end;
end;

(* ##RNC:
## elDefMeaning consists of the formulas and terms defining a constructor.
## It can be either defined by _equals_ (terms) or
## by _means_ (formulas). It may contain several
## partial (case) definitions - first in them comes the
## definition (term or formula) valid in that case and second comes
## the case formula. The final term or formula specifies the default
## case, it is mandatory if no partial definitions are given.
## If no default is given, the disjunction of all case formulas must
## be true (this have to be proved using the _consistency_ condition).
elDefMeaning =
 element elDefMeaning {
   attribute atKind { 'e' | 'm' },
   element elPartialDef { ( Formula | Term ), Formula }*,
   (Formula | Term)?
 }
*)

procedure OutVRFFileObj.Out_DefObj(const aDef:DefObj);
var k: integer;
begin
 with aDef, nPartialDefinientia do
 begin
  Out_XElStart( elDefMeaning);
  Out_XAttr( atKind, DefSort);
  Out_XAttrEnd;
  for k:=0 to Count-1 do
   with PartDefPtr(Items^[k])^ do
  begin
   Out_XElStart0( elPartialDef);
   case DefSort of
    'm': Out_Formula(FrmPtr(nPartDefiniens));
    'e': Out_Term(TrmPtr(nPartDefiniens));
   else RunTimeError(2541);
   end;
   Out_Formula(FrmPtr(nGuard));
   Out_XElEnd( elPartialDef);
  end;
  if nOtherWise<>nil then
   case DefSort of
    'm': Out_Formula(FrmPtr(nOtherWise));
    'e': Out_Term(TrmPtr(nOtherWise));
   else RunTimeError(2542);
   end;
  Out_XElEnd( elDefMeaning);
 end;
end;

(* ##RNC:
## Definiens of a constructor. This overlaps a bit with elConstructor.
## atDefNr is the number of the corresponding definitional theorem, and
## atVid optionally its label's identifier.   
## First come the argument types and possibly also the result type.
## The optional formula is conjunction of all assumptions if any given.
## If this is a redefinition, essentials are indeces of arguments
## corresponding to the arguments of original, otherwise it is just
## identity. This could be now encode with just one number like the
## atSuperfluous does for elConstructor.
## Optionally the article id (atAid) and order in article (atNr)
## can be given.
## atRelNr optionally gives its serial number
## in environment for a particular article (it is not in prels).
## atVid gives a number of the label identifier if present.   
elDefiniens =
 element elDefiniens {
   attribute atConstrKind { 'M' | 'L' | 'V' | 'R' | 'K' | 'U' | 'G' },
   attribute atConstrNr { xsd:integer},
   attribute atDefNr { xsd:integer},
   attribute atVid { xsd:integer}?,
   attribute atAid { xsd:string },
   attribute atNr { xsd:integer }?,
   attribute atRelNr { xsd:integer }?,
   elTyp*,
   element elEssentials { elInt*},
   Formula?,
   elDefMeaning
  }
*)
// fRelNr=0 means that the relative nr is not printed, used for prels
procedure OutVRFFileObj.Out_Definiens(const aDefiniensObj:DefiniensObj;
                                      fRelNr:integer);
begin
 with aDefiniensObj do
 begin
  Out_XElStart( elDefiniens);
  Out_XAttr( atConstrKind, nConstr.Kind);
  Out_XIntAttr( atConstrNr, Transf(ConstructorKind(nConstr.Kind),
                                   nConstr.Nr));
  Out_XAttr( atAid, nArticle);
  Out_XIntAttr( atDefNr, nDefNr);
  if nLabId <> 0 then Out_XIntAttr( atVid, nLabId);
  if fRelNr <> 0 then Out_XIntAttr( atRelNr, fRelNr);
  Out_XAttrEnd;
  Out_TypeList(PrimaryList);
  Out_IntSeq( elEssentials, Essentials);
  if (Assumptions <> nil) and (Assumptions^.FrmSort <> ikFrmVerum) then
   Out_Formula(Assumptions);
  Out_DefObj( Definiens^ );
  Out_XElEnd( elDefiniens);
 end;
end;

(* ##RNC:
## Reference can be either private (coming from the current article)
## - their number is the position at the stack of accessible
## references (so it is not unique), or library - these additionally
## contain their kind (theorem or definition) and article nr.
## The position in the inference is kept for error messaging.
## For a private inference, the vid attribute optionally tells
## its identifier's number.
elRef =
 element elRef {
   attribute atNr { xsd:integer },
   attribute atVid { xsd:integer }?,
   ( attribute atArticleNr { xsd:integer},
     attribute atKind { 'T' | 'D' } )?,
   Position
 }

Inference = ( elBy | elFrom | element elErrorInf { empty } )

## elBy encodes one simple justification.
elBy =
 element elBy {
   Position,
   attribute atLinked { xsd:boolean}?,
   elRef*
 }

## elFrom encodes one scheme justification, it cannot be linked.
elFrom =
 element elFrom {
   Position,
   attribute atArticleNr { xsd:integer},
   attribute atNr { xsd:integer},
   elRef*
 }
*)
procedure OutVRFFileObj.Out_Inference(const aInf:InferenceObj);
var i: integer;
begin
 with aInf do
 begin
   if nInferSort = ikError then
    begin Out_XEl1( elErrorInf); exit; end;
   if nInferSort = ikBlcIgnoreProof then
    begin Out_XEl1( elSkippedProof); exit; end;
   if nInferSort= ikInfBy then
    begin
     Out_XElStart( elBy);
     if nLinked then Out_XAttr( atLinked, 'true');
    end
   else
   begin
    Out_XElStart( elFrom);
    Out_XIntAttr( atArticleNr, nSchFileNr);
    Out_XIntAttr( atNr, nSchemeNr);
   end;
   Out_PosAsAttrs( nPos);
   if nReferences.Count = 0 then Out_XElEnd0
   else
   begin
    Out_XAttrEnd;
    for i:=0 to nReferences.Count-1 do
     case PReference(nReferences.Items^[i])^.nRefSort of
     'l':
      with PPrivateReference(nReferences.Items^[i])^ do
       begin
        Out_XElStart( elRef);
        Out_XIntAttr( atNr, LabNr);
        if (nLabId <> 0) then Out_XIntAttr( atVid, nLabId);
        Out_PosAsAttrs( RefPos);
        Out_XElEnd0;
       end;
     't':
      with PTheoremReference(nReferences.Items^[i])^ do
       begin
        Out_XElStart( elRef);
        Out_XIntAttr( atNr, TheoNr);
        Out_XIntAttr( atArticleNr, ArticleNr);
        Out_XAttr( atKind, 'T');
        Out_PosAsAttrs( RefPos);
        Out_XElEnd0;
       end;
     'd':
      with PDefinitionReference(nReferences.Items^[i])^do
       begin
        Out_XElStart( elRef);
        Out_XIntAttr( atNr, DefNr);
        Out_XIntAttr( atArticleNr, ArticleNr);
        Out_XAttr( atKind, 'D');
        Out_PosAsAttrs( RefPos);
        Out_XElEnd0;
       end
     else RunTimeError(8001);
     end;
    if nInferSort= ikInfBy then Out_XElEnd( elBy)
     else Out_XElEnd( elFrom);
   end;
 end;
end;

(* ##RNC:
## This is one step in an iterative equation.
elIterStep =
 element elIterStep { Term, Inference }
*)
procedure OutVRFFileObj.Out_IterStep(var aInf:IterStepObj);
begin
 Out_XElStart0( elIterStep);
 Out_Term(aInf.nEquatedTrm);
 Out_Inference(aInf);
 Out_XElEnd( elIterStep);
end;

// additionally outputs the aFormat
procedure OutMMLFileObj.Out_PatternWithFormat(aPatt:PatternPtr; aFormat:FormatPtr);
begin
 with aPatt^ do
 begin
  Out_XElStart( elPattern);
  Out_XAttr( atKind, NotationRepr( fKind));
  Out_XIntAttr( atNr, fAbsNr);
  Out_XAttr( atAid, fArticle);
  Out_XAttr( atConstrKind, rConstr.Kind);
  with rConstr do
   Out_XIntAttr( atConstrNr, Transf( ConstructorKind(Kind), Nr));
  if fAntonymic then Out_XAttr( atAntonymic, 'true');
  if fRedefNr <> 0 then Out_XIntAttr( atRedefNr, fRedefNr);
  Out_XAttrEnd;
  Out_Format( Self, aFormat,0);
  Out_ArgTypes( fPrimTypes);
  Out_IntSeq( elVisible, Visible);
  if Expansion<>nil then
  begin
   Out_XElStart0( elExpansion);
   Out_Type(Expansion);
   Out_XElEnd( elExpansion);
  end;
  Out_XElEnd( elPattern);
 end;
end;

procedure Load_EnvConstructors;
 var k,l: integer;
     lClusterPtr: AttrCollectionPtr;
     lTyp: TypPtr;
     lReq: Requirement;
     lInFile: MizInStream;
     lInEnvFile: InEnvFilePtr;
     lConstr: ConstrPtr;
     c: ConstructorsKind;
begin
 AfterClusters:=false;
 gTermCollection.Init(40,40);
 gAttrCollected:=true;
 gAttrCollection.Init(0);
 InitConstructors;
{ ----- Requirements ------ }
 FileExam(EnvFileName+'.ere');
 lInFile.OpenFile(EnvFileName+'.ere');
 for lReq:=low(Requirement) to High(Requirement) do
  lInFile.InInt(gBuiltIn[lReq]);
 lInFile.Done;
 InitReverseRequirements;
{ Przy zaokraglaniu typow te dwa typy moga byc potrzebne. Tak
  na prawde to powinny przyjsc z akomodatora, a jak nie to trzeba je
  wstepnie okreslic !!!!!
}
{ -----  Initialization of the Zero & the NonZero types ----- }
 if (gBuiltIn[rqElement] <> 0) and (gBuiltIn[rqOmega] <> 0) then
   NonZeroTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                               gBuiltIn[rqElement],
                               NewTrmList(NewFuncTrm(gBuiltIn[rqOmega],nil),nil))
//  else NonZeroTyp:=new(TypPtr,Init(ikTypMode,NewEmptyCluster,NewEmptyCluster,gBuiltIn[rqAny],Nil));
  else NonZeroTyp:=new(TypPtr,Init(ikTypMode,NewEmptyCluster,NewEmptyCluster,gBuiltIn[rqSetMode],Nil));
//!!!set powyzej powinno byc gBuiltIn[rqAny] zamiast gBuiltIn[rqSetMode]
{ -----  Constructors ----- }
 FileExam(EnvFileName+'.atr');
// ##NOTE: this is InEnvFileObj because of uppercluster treatment
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.atr'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elConstructors);
  NextElementState;
  while not (nState = eEnd) do
  begin
   lConstr := In_Constructor1;
   Constr[ lConstr^.fConstrKind].Insert( lConstr);
  end;
 end;
 dispose(lInEnvFile,Done);
 { ----- Clusters ----- }
 FileExam(EnvFileName+'.ecl');
 lInEnvFile:=new(InEnvFilePtr,OpenFile(EnvFileName+'.ecl'));
 with lInEnvFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elRegistrations);
  NextElementState;
  while not (nState = eEnd) do
  begin
   XMLASSERT( nElKind in ClusterElKinds);
   case nElKind of
    elRCluster: RegisteredCluster.Insert( In_Cluster);
    elFCluster: FunctorCluster.Insert( In_Cluster);
    elCCluster: ConditionalCluster.Insert( In_Cluster);
   end;
  end;
 end;
 dispose(lInEnvFile,Done);
{}
 FillChar(LocArgTyp,SizeOf(LocArgTyp),0);
{ ----- Round up of Upper clusters ----- }
 lClusterPtr:=new(AttrCollectionPtr,Init(0,4));
 if gBuiltIn[rqZero]<> 0 then 
  lClusterPtr^.InsertAttr(gBuiltIn[rqZero],0,nil);
// if gBuiltIn[rqNatural]<> 0 then
//  lClusterPtr^.InsertAttr(gBuiltIn[rqNatural],1,nil);
 if (gBuiltIn[rqElement] > 0) and
    (gBuiltIn[rqOmega] > 0) and
    (gBuiltIn[rqPositive] > 0)
   then lClusterPtr^.InsertAttr(gBuiltIn[rqPositive],1,nil);
 lClusterPtr^.RoundUpWith(NonZeroTyp);
 dispose(NonZeroTyp^.UpperCluster,Done);
 NonZeroTyp^.UpperCluster:=lClusterPtr;
 gTermCollection.FreeAll;
{ To dla porzadku, nalezaloby raczej zachowac wygenerowany obiekt
  ( NAT ), bo sie moze pozniej przydac, wtedy jednak nie mozemy
  stosowac FreeAll przy zaokraglaniu typow konstruktorow.
}
 for k:=0 to RegisteredCluster.Count-1 do
  with RClusterPtr(RegisteredCluster.Items^[k])^ do
   begin
    with nPrimaryList do
     for l:=0 to Count-1 do
      begin
       move(nPrimaryList.Items^,LocArgTyp[1],nPrimaryList.Count*sizeof(pointer));
       lClusterPtr:=CopyCluster(TypPtr(Items^[l])^.UpperCluster);
       lClusterPtr^.RoundUpWith(TypPtr(Items^[l]));
       dispose(TypPtr(Items^[l])^.UpperCluster,Done);
       TypPtr(Items^[l])^.UpperCluster:=lClusterPtr;
       gTermCollection.FreeAll;
      end;
    move(nPrimaryList.Items^,LocArgTyp[1],nPrimaryList.Count*sizeof(pointer));
    nConsequent.Upper^.RoundUpWith(nClusterType);
//     if not nConsequent.Upper^.fConsistent then Error(CurPos,95);
    gTermCollection.FreeAll;
   end;
 for k:=0 to FunctorCluster.Count-1 do
  with FClusterPtr(FunctorCluster.Items^[k])^ do
   begin
    move(nPrimaryList.Items^,LocArgTyp[1],nPrimaryList.Count*sizeof(pointer));
    if nClusterType = nil then
     lTyp:=RoundUpTrmType(nClusterTerm)
    else lTyp:=nClusterType;
    nConsequent.Upper^.EnlargeBy(lTyp^.UpperCluster);
    nConsequent.Upper^.RoundUpWith(lTyp);
//     if not nConsequent.Upper^.fConsistent then Error(CurPos,95);
    if nClusterType = nil then
      Dispose(lTyp,Done);
    gTermCollection.FreeAll;
   end;
// ##TODO: why not round up coAggregate too?
 for c := Low(ConstructorsKind) to High(ConstructorsKind) do
  if c in [ coMode, coFunctor, coSelector] then
   for k:=1 to Constr[c].Count - 1 do
   begin
    ConstrTypPtr( Constr[c].Items^[k])^.RoundUp;
    gTermCollection.FreeAll;
   end;
 gTermCollection.Done;
 AfterClusters:=true;
end;

end.

