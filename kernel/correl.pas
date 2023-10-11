(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit correl;

interface

uses errhan,limits,builtin,mobjects,inout,roundcl,formats,numbers,enums,mscanner;

var ItIsChecker: boolean = false;
    DisposingSuperclusters: boolean = false;

type
   TrmPtr = ^TrmObj; AttrPtr = ^ AttrObj; TypPtr = ^TypObj; FrmPtr = ^FrmObj;
   AttrCollectionPtr = ^MAttrCollection;
   TrmList = ^TrmElem;
   TrmElem = record NextTrm: TrmList; XTrmPtr: TrmPtr end;

   DoInTrm = procedure (var fTrm: TrmPtr);
   EqualAttrs = function (aAttr1,AAttr2: AttrPtr): boolean;
   EqualTyps = function (aTyp1,aTyp2: TypPtr): boolean;
   EqualTrms = function (aTrm1,aTrm2: TrmPtr): boolean;
   EqualFrms = function (aFrm1,aFrm2: FrmPtr): boolean;

   ClusterRec =
    record
      Lower,Upper: AttrCollectionPtr;
    end;

   ExprPtr = ^CorrObj;
   CorrObj =
    object(MObject)
      nPattNr: integer;
      constructor Init;
      constructor InitP(aPattNr:integer);
      destructor Done; virtual;
    end;
   TrmObj =
    object(CorrObj)
      TrmSort: char; TrmInfo: integer;
      constructor Init;
      constructor InitP(aPattNr:integer);
      destructor Done; virtual;
    end;
   VarTrmPtr = ^VarTrmObj;
   VarTrmObj =
     object(TrmObj)
      VarNr: integer;
      constructor Init;
      constructor InitP(aPattNr:integer);
      destructor Done; virtual;
     end;
   FuncTrmPtr = ^FuncTrmObj;
   FuncTrmObj =
     object(TrmObj)
      FuncNr: integer;
      FuncArgs: TrmList;
      constructor Init;
      constructor InitP(aPattNr:integer);
      destructor Done; virtual;
     end;
   LocFuncTrmPtr = ^LocFuncTrmObj;
   LocFuncTrmObj =
     object(FuncTrmObj)
       FuncExp: TrmPtr;
      constructor Init;
      constructor InitP(aPattNr:integer);
      destructor Done; virtual;
     end;
   FraenkelTrmPtr = ^FraenkelTrmObj;
   FraenkelTrmObj =
     object(TrmObj)
      LambdaArgs: MCollection;
      LambdaScope: TrmPtr; Compr: FrmPtr;
      nIdents: IntSequence; // identifiers of vars (0 for unknown)
      constructor Init;
      constructor InitP(aPattNr:integer);
      destructor Done; virtual;
     end;
   QuaTrmPtr = ^QuaTrmObj;
   QuaTrmObj =
     object(TrmObj)
      TrmProper: TrmPtr; Qua: TypPtr;
      constructor Init;
      constructor InitP(aPattNr:integer);
      destructor Done; virtual;
     end;
   ChoiceTrmPtr = ^ChoiceTrmObj;
   ChoiceTrmObj =
     object(TrmObj)
       ChoiceTyp: TypPtr;
      constructor Init(aTyp:TypPtr);
      constructor InitP(aPattNr:integer; aTyp:TypPtr);
      destructor Done; virtual;
     end;

   TypObj =
     object(CorrObj)
      LowerCluster,UpperCluster: AttrCollectionPtr;
      ModNr: integer;
      TypSort: char;
      ModArgs: TrmList;
      constructor Init(fSort:char; fLAttr,fUAttr: AttrCollectionPtr;
                       fMod:integer; fArgs:TrmList);
      constructor InitP(fSort:char; fLAttr,fUAttr: AttrCollectionPtr;
                        fMod:integer; fArgs:TrmList; fPattNr:integer);
      destructor Done; virtual;

       { metody statyczne }
      function CopyType: TypPtr;
      function InstTyp(fTrmList:TrmList):TypPtr;
      procedure RoundUp;
      function DecreasingAttrs(fTarget:TypPtr; aEqAttr: EqualAttrs):boolean;
      procedure AdjustTyp(var fMode:integer; var fArgs:TrmList);
      function EqRadices(fTyp2:TypPtr):boolean;
      procedure WithinType(P:DoInTrm);
      function WidenToStruct: TypPtr;
      function WideningOf(SourceTyp: TypPtr): TypPtr;
      function IsWiderThan(fTypPtr: TypPtr): boolean;
      function Widening: TypPtr;
      {!!!!! procedure WidenOnce;}
     end;

   FrmObj =
    object(CorrObj)
     FrmSort: char;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
     procedure SetPidIfKind( aKind:char; aPattNr:integer);
    end;
   PredFrmPtr = ^PredFrmObj;
   PredFrmObj =
    object(FrmObj)
      PredNr: integer; PredArgs: TrmList;
     constructor Init(fPredKind:char; fPredNr:integer; fPredArgs:TrmList);
     constructor InitP(fPredKind:char; fPredNr:integer;
                       fPredArgs:TrmList; fPattNr:integer);
     destructor Done; virtual;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   LocPredFrmPtr = ^LocPredFrmObj;
   LocPredFrmObj =
    object(PredFrmObj)
      PredExp: FrmPtr;
     constructor Init(aPredNr:integer; aPredArgs:TrmList; aExp: FrmPtr);
     constructor InitP(aPredNr:integer; aPredArgs:TrmList;
                       aExp: FrmPtr; aPattNr:integer);
     destructor Done; virtual;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   QualFrmPtr = ^QualFrmObj;
   QualFrmObj =
    object(FrmObj)
      QualTrm: TrmPtr; QualTyp: TypPtr;
     constructor Init(fQualTrm: TrmPtr; fQualTyp: TypPtr);
     constructor InitP(fQualTrm: TrmPtr; fQualTyp: TypPtr; fPattNr:integer);
     destructor Done; virtual;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   NegFrmPtr = ^NegFrmObj;
   NegFrmObj =
    object(FrmObj)
      NegArg: FrmPtr;
     constructor Init(fNegArg: FrmPtr);
     constructor InitP(fNegArg: FrmPtr; fPattNr:integer);
     destructor Done; virtual;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   BinFrmPtr = ^BinFrmObj;
   BinFrmObj =
    object(FrmObj)
      nLeftArg,nRightArg: FrmPtr;
     constructor Init(aFrmSort:char; aLeftArg,aRightArg:FrmPtr);
     destructor Done; virtual;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   ConjFrmPtr = ^ConjFrmObj;
   ConjFrmObj =
    object(FrmObj)
      Conjuncts: MCollection;
     constructor Init(const fConjuncts: MCollection);
     constructor InitP(const fConjuncts: MCollection; fPattNr:integer);
     destructor Done; virtual;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   UnivFrmPtr = ^UnivFrmObj;
   UnivFrmObj =
    object(FrmObj)
      nVarId: integer;
      Quantified: TypPtr; Scope: FrmPtr;
     constructor Init(fSort:char; fQuantified: TypPtr; fScope: FrmPtr);
     constructor InitP(fQuantified: TypPtr; fScope: FrmPtr; fPattNr:integer);
     constructor InitI(aQuantified: TypPtr; aScope: FrmPtr; aVarId:integer);
     destructor Done; virtual;
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   UniqFrmPtr = ^UniqFrmObj;
   UniqFrmObj =
    object(FrmObj)
     constructor Init(fKind: char);
     constructor InitP(fKind: char; fPattNr:integer);
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
   FlexFrmPtr = ^FlexFrmObj;
   FlexFrmObj =
    object(FrmObj)
     nLeftOrigFrm,nRightOrigFrm:FrmPtr;
     nLeftTrm,nRightTrm:TrmPtr;
     nExpansion:FrmPtr; // generated formula (univ or exist)
     constructor Init(fLeftOrigFrm,fRightOrigFrm:FrmPtr; fLeftTrm,fRightTrm:TrmPtr);
     constructor InitD(fLeftOrigFrm,fRightOrigFrm,fExpansion:FrmPtr; fLeftTrm,fRightTrm:TrmPtr);
     function CopyFormula: FrmPtr; virtual;
     procedure WithinFrm; virtual;
    end;
{---------------------------------------------------------------------}
   AttrObj = Object(CorrObj)
      fAttrNr: integer;
      fNeg: word;
      fAttrArgs: Trmlist;
      fCollected: boolean;
     constructor Init(aNeg:byte; aAttr:integer; aArgs:TrmList);
     constructor InitP(aNeg:byte; aAttr:integer;
                       aArgs:TrmList; aPattNr:integer);
     destructor Done; virtual;
     function CopyAttribute: AttrPtr;
     procedure AdjustAttr(var aAttrNr:integer; var aArgs:TrmList);
     function AdjustedAttrNr: integer;
     procedure WithinAttr(P:DoInTrm);
   end;
   MAttrCollection = object(MSortedCollection)
      fConsistent: boolean;
     constructor Init(ALimit, ADelta: Integer);
     function Compare(Key1, Key2: Pointer): Integer; virtual;
     procedure FreeItem(Item: Pointer); virtual;
     destructor Refuted; virtual;
     function GetAttr(aAttrNr: integer; aAttrArgs: TrmList): AttrPtr; virtual;
     procedure AtInsert(Index: Integer; Item: Pointer); virtual;
     procedure InsertAttr(aAttrNr:integer; aNeg:byte; aArgs: TrmList); virtual;
     procedure Insert(aItem: Pointer); virtual;
     procedure WithinAttrCollection(P:DoInTrm);
     procedure RoundUpWith(aTyp:TypPtr);
     procedure ClearPids; virtual;
     constructor CopyAll(aOrigin:AttrCollectionPtr);
     constructor CopyAllowed(aTyp:TypPtr; aOrigin:AttrCollectionPtr);
     procedure EnlargeBy(aAnother:MListPtr); virtual;
     function IsSubsetOf(aClu: AttrCollectionPtr; aEqAttr: EqualAttrs): boolean; virtual;
     function IsEqualTo(aClu: AttrCollectionPtr; aEqAttr: EqualAttrs): boolean; virtual;
   end;

 LociSubstitution =  array[1..2*MaxArgNbr] of TrmPtr;

function TypReachable( fWider,fNarrower: TypPtr): boolean;
function CmpFuncTrm( fTrm1,fTrm2: TrmPtr): integer;
function CmpFuncCluster(fCl1,fCl2: Pointer): integer;

procedure SkipLocPred(var aFrm: FrmPtr);

var
    ItTyp:TypPtr;
    LocArgTyp: array[1..2*MaxArgNbr+1] of TypPtr;
    BoundVarNbr: integer;
    FreeVarType: MList;
    BoundVar: array[1..MaxVarNbr] of TypPtr;
    FixedVar: array[1..MaxVarNbr] of
     record nIdent: integer;
      nTyp: TypPtr;
      nExp: boolean;
      nDef: TrmPtr;
      nSkelConstNr: integer;
     end;
    CurSchFuncTyp: MCollection;

type
  ConstDefPtr = ^ConstDefObj;
  ConstDefObj = object(MObject)
    fDef: TrmPtr;
    fEqConst: NatSet;
    nSetting: integer;
    fTyp: TypPtr;
    fDetermined: boolean;
    fNumericValue: RComplex;
   constructor Init(aDef: TrmPtr; aTyp: TypPtr);
   destructor Done; virtual;
  end;

var
    InferConstDef: MSortedExtList;
    InferConsts: IntSet;
    gOnlyConstants: boolean = true;
  { Zmienna ta jest uzywana do kontroli, czy term moze byc kolekcjonowany,
    tzn. czy sklada sie jedynie ze stalych.
  }

type
  ArityObj =
    object(MObject)
      nId: integer;
      fPrimaries: MList;
     constructor Init;
     destructor Done; virtual;
    end;

  FuncDefPtr = ^FuncDefObj;
  FuncDefObj =
    object(ArityObj)
      fFuncDef: TrmPtr;
      fFuncTyp: TypPtr;
     constructor Init(aId:integer; var aPrimArgs: MList; aFuncDef: TrmPtr; aFuncTyp: TypPtr);
     destructor Done; virtual;
    end;

  LocPredDefPtr = ^LocPredDefObj;
  LocPredDefObj =
    object(ArityObj)
      fPredDef: FrmPtr;
     constructor Init(aId:integer; var aPrimArgs: MList; aPredDef: FrmPtr);
     destructor Done; virtual;
    end;

var
  LocFuncDef,LocPredDef: MList;

type
   PropertySet = set of PropertyKind;
   PropertiesRec =
    record Properties: set of PropertyKind; nFirstArg,nSecondArg: integer end;

   ConstrPtr = ^ConstrObj;
   ConstrObj = object(MObject)
    fConstrKind: ConstructorsKind;
    fArticle: string;   	// article where defined
    fAbsNr: integer;		// absolute nr (i.e. in its article)
    nPrimaries: MCollection;   	// list of arguments
    fStructLoci: NatFunc;	// not used yet
    fWhichConstrNr:integer;	// nr. of its redefined constr (or 0)
    fSuperfluous:byte;		// nr. of arguments added to the redefined constr.
    fProperties: PropertySet;
    fFirstArg,fSecondArg: integer;// for fProperties
   constructor InitInternal(aKind: ConstructorsKind; aNr: integer;
                            aArticle: string);
   constructor Init(aKind: ConstructorsKind; aNr: integer;
                    aArticle: string);
   constructor InitForPattern(aKind: ConstructorsKind; aNr: integer;
                              aArticle: string; const aPrimArgs:MList);
   destructor Done; virtual;
   procedure SetRedef( aWhich,aSuperfluous:integer );
   procedure SetProperties(aProperties: PropertiesRec);
   procedure GetProperties(var aProperties: PropertiesRec);
  end;

 ConstrTypPtr = ^ConstrTypObj;
 ConstrTypObj =
  object(ConstrObj)
    fConstrTyp:TypPtr;
   constructor Init(aKind: ConstructorsKind; aNr: integer;
                    aArticle: string);
   constructor InitForPattern(aKind: ConstructorsKind; aNr: integer;
                              aArticle: string;
                              const aPrimArgs: MList; fTyp:TypPtr);
   destructor Done; virtual;
   procedure RoundUp;
  end;

 StructConstrPtr = ^StructConstrObj;
 StructConstrObj =
  object(ConstrObj)
   fPrefixes: MCollection;
   fStructModeAggrNr: integer;	// nr of the corresponding coAggregate
   fFields:NatSetPtr;	       	// probably selectors
   constructor Init(aNr: integer; aArticle: string);
   destructor Done; virtual;
   constructor InitForPattern(aNr: integer; aArticle: string;
                              const aPrimArgs: MList);
  end;

 AggrConstrPtr = ^AggrConstrObj;
 AggrConstrObj =
  object(ConstrTypObj)
     fAggregBase: integer;	// nr. of "over" arguments (they come first)
     fAggrColl:PCollection; 	// contains numbers (PIntItems) of selectors
   constructor Init(aNr: integer; aArticle: string);
   destructor Done; virtual;
   constructor InitForPattern(aNr: integer; aArticle: string;
                              const aPrimArgs: MList; fTyp:TypPtr);
  end;

  PatternPtr = ^PatternObj;
  PatternObj = object(MObject)
   fKind: NotationKind;
   fArticle: string;   	// article where defined
   fAbsNr: integer;     // absolute nr (i.e. in its article)
   fFormNr: integer;	// fFormNr is a number in gFormats, while
   fFormat: FormatPtr;	// fFormat is the format pointer directly -
   rConstr: Lexem;	// - only one of them should be set
   fRedefNr: integer;     // relative number of origin for synonyms/antonyms
   fPrimTypes: MList;
   Visible:IntSequence;
   fAntonymic: boolean;
   Expansion: TypPtr;
   constructor Init( aKind: NotationKind; aNr: integer; aArticle: string);
   destructor Done; virtual;
  end;

var
 Constr: array[ConstructorsKind] of MCollection;
 ConstrBase: ConstrIntArr;

{ Registering CLUSTERS }
type
 ClusterPtr = ^ClusterObj;
 RClusterPtr = ^RClusterObj;
 CClusterPtr = ^CClusterObj;
 FClusterPtr = ^FClusterObj;
 ClusterObj =
  object(MObject)
   nClusterKind: ClusterKind;
   nPrimaryList: MCollection;
   nConsequent: ClusterRec;
   nClusterType:TypPtr;
   nArticle: string;   	// article where defined
   nAbsNr: integer;     // absolute nr (i.e. in its article)
   constructor Init(aNr: integer; aArticle: string; const aCons:ClusterRec; var aColl:MCollection);
   destructor Done; virtual;
  end;

 RClusterObj =
  object(ClusterObj)
   constructor Init(aNr: integer; aArticle: string; const aCons:ClusterRec; var aColl:MCollection; aTyp:TypPtr);
   constructor RegisterCluster(aNr: integer; aArticle: string; aClu:AttrCollectionPtr; var fPrim:MCollection; fTyp:TypPtr);
   destructor Done; virtual;
  end;

 CClusterObj =
  object(ClusterObj)
   nAntecedent: AttrCollectionPtr;
   constructor Init(aNr: integer; aArticle: string; aAntec:AttrCollectionPtr; const aCons: ClusterRec;
                    var aColl:MCollection;
                    aTyp:TypPtr);
   constructor RegisterCluster(aNr: integer; aArticle: string; aClu1,aClu2:AttrCollectionPtr; var fPrim:MCollection; fTyp:TypPtr);
   destructor Done; virtual;
  end;

 FClusterObj =
  object(ClusterObj)
   nClusterTerm: TrmPtr;
   constructor Init(aNr: integer; aArticle: string; const aCons: ClusterRec; var aColl:MCollection; aTrm:TrmPtr;
                    aTyp:TypPtr);
   constructor RegisterCluster(aNr: integer; aArticle: string; aClu:AttrCollectionPtr; var fPrim:MCollection; fTrm:TrmPtr;
                        aTyp:TypPtr);
   destructor Done; virtual;
  end;

function RoundUpWith(fCluster: FClusterPtr; fTrm:TrmPtr; fTyp: TypPtr; var fClusterPtr: AttrCollectionPtr): boolean;

 // max(MaxFuncNbr,MaxPredNbr,MaxAttrNbr,MaxModeNbr)
 const MaxRedefNbr	= MaxFuncNbr;
 var
    gDefBase:integer;
    RegisteredCluster: MExtList;
    FunctorCluster: MSortedExtList;
    ConditionalCluster: MCondClList;
    gAttrCollected: boolean = false;
    gAttrCollection: MList;
    RegClusterBase,CondClusterBase,FuncClusterBase,RegPropertiesBase:integer;
    gSubstTrm: LociSubstitution;
    NonZeroTyp: TypPtr;

function CreateArgList(aLength:integer): TrmList;
function CreateArgList1: TrmList;
procedure DisposeSubstTrm;

procedure InitConstructors;
procedure DisposeConstructors;

procedure ChangeToLoci(var fTrm:TrmPtr);

function NewTrmList ( fTrm:TrmPtr; fTrmList:TrmList ):TrmList;
function AddToTrmList ( fTrmList:TrmList; fTrm:TrmPtr):TrmList;

function NewVarTrm ( fSort:char; fNr:integer ):VarTrmPtr;
function NewFuncTrm ( fFunc:integer; fArgs: TrmList ):TrmPtr;
function NewLocFuncTrm(fSort:char;fFunc:integer;fArgs:TrmList):FuncTrmPtr;
function NewPrivFuncTrm(aFunc:integer;aArgs:TrmList; aExp:TrmPtr):FuncTrmPtr;
function NewItTrm: TrmPtr;
function NewFraenkelTrm(fTrm:TrmPtr;fFrm:FrmPtr; var fColl:MCollection): FraenkelTrmPtr;
function NewFraenkelTrmI(fTrm:TrmPtr;fFrm:FrmPtr; var fColl:MCollection; var fIdents:IntSequence): FraenkelTrmPtr;
function NewQuaTrm(fTrmProper:TrmPtr; fTyp:TypPtr):TrmPtr;
function NewChoiceTrm(aTyp: TypPtr): TrmPtr;
function NewInCorTrm: TrmPtr;

function NewEmptyCluster: AttrCollectionPtr;
function NewStandardTyp (fSort:char; fLAttr,fUAttr: AttrCollectionPtr;
                         fMod:integer; fArgs:TrmList ):TypPtr;
function NewInCorTyp: TypPtr;

function NewVerum:FrmPtr;
function NewNeg ( fArg:FrmPtr ):FrmPtr;
function NewNegDis ( fArg:FrmPtr ):FrmPtr;
function NewUniv ( fQuant:TypPtr; fScope:FrmPtr ):FrmPtr;
function NewUnivI(aVarID:integer; aQuant:TypPtr; aScope:FrmPtr):FrmPtr;
function NewPredFrm ( fSort:char; fPred:integer; fArgs:TrmList; fPattNr:integer ):FrmPtr;
function NewLocPredFrm ( aPred:integer; aArgs:TrmList; aExp:FrmPtr ):FrmPtr;
function NewQualFrm ( fTrm:TrmPtr; fTyp:TypPtr ):FrmPtr;
function NewEqFrm ( fLeft,fRight:TrmPtr ):FrmPtr;
function NewInCorFrm: FrmPtr;

function NewExis(fTyp:TypPtr; fFrm:FrmPtr):FrmPtr;
function NewConjFrm (const fList:MCollection):FrmPtr;
function NewConj (Arg1,Arg2:FrmPtr ):FrmPtr;
function NewImpl(fArg1,fArg2:FrmPtr):FrmPtr;
function NewBicond ( Arg1,Arg2:FrmPtr ):FrmPtr;
function NewDisj(fArg1,fArg2:FrmPtr):FrmPtr;
function NewFlexConj (Arg1,Arg2:FrmPtr):FrmPtr;
function NewFlexDisj(fArg1,fArg2:FrmPtr):FrmPtr;
function NewFlexFrm(fOrigFrm1,fOrigFrm2,fExpansion:FrmPtr; fLeftTrm,fRightTrm:TrmPtr):FrmPtr;
function NewExpansion(fTyp:TypPtr; fLeftGuardFrm,fRightGuardFrm,fFrm:FrmPtr):FrmPtr;
function FraenkelFrm(fTrm,fOpFr:TrmPtr): FrmPtr;

function LastElem(fTrmList:TrmList):TrmList;
function LastArg(fTrmList:TrmList):TrmPtr;
function NbrOfElem(fL: TrmList): integer;
procedure GetArgs1(fFirst:integer; var fFirstTrm:TrmPtr; fList:TrmList);
procedure GetArgs2(fFirst,fSecond:integer; var fFirstTrm,fSecondTrm:TrmPtr;
                  fList:TrmList);
procedure GetBinArgs(aFrm:FrmPtr; var aLeft,aRight: TrmPtr);
function SwitchArgs(fFirst,fSecond:integer; fList:TrmList): TrmList;
function SwapArguments(fList:TrmList; fFirst,fSecond:word):TrmList;
procedure RemoveQua(fTL: TrmList);

function CopyTerm ( fTrm:TrmPtr ):TrmPtr;
function CopyTermList ( fTL:TrmList ):TrmList;
function CopyTermList1 ( fTL:TrmList ):TrmList; {kopia bez ostatniego elementu}
function CopyCluster(aClu: AttrCollectionPtr): AttrCollectionPtr;
function AdjustedType ( fTyp:TypPtr ):TypPtr;
procedure CopyTypeColl(const aSrc: MList; var aTrg: MList);

function CopyExpTrm ( fTrm:TrmPtr ):TrmPtr;
function CopyExpTyp(aTyp:TypPtr): TypPtr;
function CopyExpFrm(fFrm:FrmPtr):FrmPtr;

function OriginalNr( c:ConstructorsKind; aNr:integer ):integer;
function AdjustedNr( c:ConstructorsKind; aNr:integer ):integer;
function AdjustedFuncNr(aTrm:TrmPtr):integer;

function MotherStructNr( fSelectNr:integer ):integer;

procedure AdjustTrm ( fTrm:TrmPtr; var fFunc:integer; var fArgs:TrmList );
procedure AdjustFrm ( fFrm:PredFrmPtr; var fPred:integer; var fArgs:TrmList );
procedure AdjustAttrFrm ( fFrm:PredFrmPtr; var fAttr:integer; var fArgs:TrmList );
function AdjustTrmList(aKind:char; aNr:integer; aTrmList:TrmList):TrmList;

function EquateTrmsLists(fTrm,aTrm:TrmList; aEqTrms: EqualTrms): boolean;

function EqAttr(fAttr1, fAttr2: AttrPtr): boolean;
function EqTrm ( fTrm1,fTrm2:TrmPtr ):boolean;
function EqTyp ( fTyp1,fTyp2:TypPtr ):boolean;
function EqFrm ( fFrm1,fFrm2:FrmPtr ):boolean;
function EqTrmList(fTL1,fTL2: TrmList): boolean;

function StrictEqTyp(fTyp1,fTyp2:TypPtr):boolean;
function StrictEqTrm ( fTrm1,fTrm2:TrmPtr ):boolean;
function StrictEqFrm ( fFrm1,fFrm2:FrmPtr ):boolean;
function StrictEqAttr(fAttr1, fAttr2: AttrPtr): boolean;

procedure InitInst{(const aSubstTrm: LociSubstitution)};
procedure InitInstList(aTrmList:TrmList);
procedure StopInst;
function InstCluster(aClu: AttrCollectionPtr; aTrmList:TrmList): AttrCollectionPtr;
function InstTrm ( fTrm:TrmPtr; fTrmList:TrmList):TrmPtr;
function InstFrm ( fFrm:FrmPtr; fTrmList:TrmList):FrmPtr;
function InstQual( fFrm:FrmPtr; fTrmList:TrmList; fTrm:TrmPtr ):FrmPtr;
function InstTrmInTyp ( fTyp:TypPtr; fTrm:TrmPtr ):TypPtr;
function InstSubstTrm ( fTrm:TrmPtr):TrmPtr;
function InstSubstFrm ( fFrm:FrmPtr):FrmPtr;

var IncBounVarNbr: boolean = false;
procedure WithinTerm ( fTrm:TrmPtr; P:DoInTrm );
procedure WithinFormula ( fFrm:FrmPtr; P:DoInTrm );

procedure WithinTypeColl (var fColl:MCollection; P:DoInTrm );
procedure WithinCluster ( fCluster:ClusterPtr; P:DoInTrm );

procedure ExpandInferConsts(var fTrm:TrmPtr);

procedure ExpPrivFuncInTrm(var fTrm:TrmPtr);

function CopyTrmType(fTrm:TrmPtr): TypPtr;
function RoundUpTrmType(fTrm:TrmPtr): TypPtr;
function GetTrmType(fTrm:TrmPtr):TypPtr;

function EsAttr(fAttr1, fAttr2: AttrPtr): boolean;
function EsAttrRev(fAttr1, fAttr2: AttrPtr): boolean;
function EsTyp(fTyp,aTyp: TypPtr):boolean;
function EsTrmList(fTrm1,fTrm2:TrmList):boolean;
function EsTrm(fTrm,aTrm:TrmPtr):boolean;
function CompEsTyp(fTyp,aTyp:TypPtr; fExactly:boolean): boolean;
function CheckLociTypes(const fList:MList):boolean;
function CheckLociTypesN(const fList:MList):boolean;
function CheckTypes(aPattern:PatternPtr; aTrmList:TrmList): boolean;
function Agree(fTrmList:TrmList; const fTypList:MList):boolean;

function AttrEquals(Key1, Key2: AttrPtr): boolean;

var gStrictCompare: boolean = true;
function CompAbsAttr(aAttr1, aAttr2: AttrPtr): Integer;
function CompAttr(aAttr1, aAttr2: pointer): Integer;
function ExtCompAttr(aAttr1, aAttr2: pointer): Integer;
function CompRdTrms(fTrm1,fTrm2: pointer):integer;
function CompTrms(fTrm1,fTrm2: pointer):integer;

function ReconSelectTrm(fSelector:integer; fLastArg:TrmPtr; fTyp:TypPtr):TrmPtr;
function ReconAggregTrm(fStruct:integer; fLastArg:TrmPtr; fTyp:TypPtr):TrmPtr;

function EqualClusters(fTyp1,fTyp2:TypPtr; aEqAttr: EqualAttrs): boolean;

procedure DisposeTrm(fTrm:TrmPtr);
procedure DisposeTrmList(fTrmList:TrmList);
procedure DisposeListOfTerms(fList:TrmList);
procedure DisposeTrmMList(var aList:MListPtr);

procedure InsertArgument(fInt:integer);

procedure ChangeBound(var fTrm: TrmPtr);
procedure ChChangeBound(var fTrm: TrmPtr);
var  gBoundBase: integer;
procedure FrRenBound(var fTrm: TrmPtr);

var gTrmList: TrmList;
   gExactly: array[1..2*MaxArgNbr] of boolean;

type
 LexemPtr = ^LexemObj;
 LexemObj = object (MObject)
   fLexem: Lexem;
  constructor Init(const alexem: Lexem);
 end;

 MSortedLexemList = object(MSortedList)
  constructor Init(ALimit: Integer);
  function IndexOfLexem(const aLexem: Lexem): Integer; virtual;
  function ObjectOf(const alexem: Lexem): LexemPtr; virtual;
 end;

 MLexemAndListPtr = ^MlexemAndList;
 MLexemAndList = object(LexemObj)
   fList: MList;
  constructor Init(const alexem: Lexem);
  destructor Done; virtual;
 end;

 MLexemAndListList = object(MSortedLexemList)
  constructor Init(ALimit: Integer);
  procedure InsertAtLexem(aLexem: Lexem; aItem: Pointer);virtual;
 end;

{^ Kolekcjonowanie termow ^}

 TTPairPtr = ^TTPairObj;
 TTPairObj =
  object(MObject)
   nTermScope:integer;
   nTrm: TrmPtr; nTyp: TypPtr;
   constructor Init(fTrm:TrmPtr);
   destructor Done; virtual;
  end;
 TTCollection =
 object(MSortedCollection)
  function Compare(Key1,Key2:pointer): integer; virtual;
  function KeyOf(Item:pointer): pointer; virtual;
 end;

var gTermCollection: TTCollection;
    InCorrTrmList: TrmList;
    gTermScope:integer=0;

procedure MarkTermsInTTColl;
function InsertTermInTTColl(fTrm:TrmPtr): integer;
procedure RemoveTermsFromTTColl;

{^ rozszerzanie structur ^}

var gWidStruct: MCollection;
    gWidStructFound: Boolean;
    gTargetStructNr:integer;

procedure WidenningPath(fStructNr:integer);

const errBadTypeIds = 2583;

function ReductionAllowed(fTrm1,fTrm2:TrmPtr):boolean; //true if fTrm2 is a strict subterm of fTrm1

implementation

uses lexicon,iocorrel,ellipses
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF}
{$IFDEF DEBUG_ELLIPSES}{$IFNDEF MDEBUG} ,info,outinfo {$ENDIF}{$ENDIF}
;

procedure WithinTrm(var fTrm:TrmPtr); forward;

{ Przerabianie termow na obiekty }

constructor CorrObj.Init;
begin inherited Init; nPattNr:= 0; end;

constructor CorrObj.InitP(aPattNr:integer);
begin Init; nPattNr:= aPattNr; end;

constructor TrmObj.Init;
begin inherited Init;
TrmInfo:=0;
end;

constructor TrmObj.InitP(aPattNr:integer);
begin Init; nPattNr:= aPattNr; end;

constructor VarTrmObj.Init;
begin inherited Init;
end;

constructor VarTrmObj.InitP(aPattNr:integer);
begin Init;  nPattNr:= aPattNr; end;

constructor FuncTrmObj.Init;
begin inherited Init;
end;

constructor FuncTrmObj.InitP(aPattNr:integer);
begin Init;  nPattNr:= aPattNr; end;

constructor LocFuncTrmObj.Init;
begin inherited Init;
end;

constructor LocFuncTrmObj.InitP(aPattNr:integer);
begin Init;  nPattNr:= aPattNr; end;

constructor FraenkelTrmObj.Init;
begin inherited Init; nIdents.Init(0);
end;

constructor FraenkelTrmObj.InitP(aPattNr:integer);
begin Init;  nPattNr:= aPattNr; end;

constructor QuaTrmObj.Init;
begin inherited Init;
end;

constructor QuaTrmObj.InitP(aPattNr:integer);
begin Init;  nPattNr:= aPattNr; end;

constructor ChoiceTrmObj.Init(aTyp: TypPtr);
begin inherited Init;

 ChoiceTyp:=aTyp;
end;

constructor ChoiceTrmObj.InitP(aPattNr: Integer; aTyp: TypPtr);
begin
 Init(aTyp);  nPattNr:= aPattNr;
end;

function CompareLexemPtr(aKey1, aKey2: Pointer): Integer;
 var lInt: integer;
begin
 lInt:=CompareInt(ord(LexemPtr(aKey1)^.fLexem.Kind),ord(LexemPtr(aKey2)^.fLexem.Kind));
 if lInt <> 0 then begin CompareLexemPtr:=lInt; exit end;
 lInt:=CompareInt(LexemPtr(aKey1)^.fLexem.Nr,LexemPtr(aKey2)^.fLexem.Nr);
 if lInt <> 0 then begin CompareLexemPtr:=lInt; exit end;
 CompareLexemPtr:=0;
end;

constructor LexemObj.Init(const alexem: Lexem);
begin
 fLexem:=alexem;
end;

constructor MSortedLexemList.Init(ALimit: Integer);
begin
 inherited InitSorted(ALimit,CompareLexemPtr);
end;

function MSortedLexemList.IndexOfLexem(const aLexem: Lexem): Integer;
 var I: Integer; lLexemObj: LexemObj;
 begin
 IndexOfLexem := -1;
 if @fCompare = nil then
  begin
   ListError(coSortedListError,0);
   exit;
  end;
 lLexemObj.Init(aLexem);
 if Find(@lLexemObj, I) then
  begin
   if I < Count then IndexOfLexem := fIndex^[I];
  end;
end;

function MSortedLexemList.ObjectOf(const aLexem: Lexem): LexemPtr;
 var I: integer;
begin
 ObjectOf:=nil;
 I:=IndexOfLexem(aLexem);
 if I>=0 then ObjectOf:=Items^[I];
end;

constructor MLexemAndList.Init(const alexem: Lexem);
begin
 fLexem:=alexem;
 fList.Init(8);
end;

destructor MLexemAndList.Done;
begin
 fList.Done;
end;

constructor MLexemAndListList.Init(ALimit: Integer);
begin
 inherited Init(ALimit);
end;

procedure MLexemAndListList.InsertAtlexem( aLexem: Lexem; aItem: Pointer);
 var lLexemPtr: MLexemAndListPtr;
begin
 lLexemPtr:=MLexemAndListPtr(ObjectOf(aLexem));
 if lLexemPtr = nil then
  begin
   lLexemPtr:=new(MLexemAndListPtr,Init(aLexem));
   Insert(lLexemPtr);
  end;
 lLexemPtr^.fList.Insert(aItem);
end;

{v Kolekcjonowanie termow v}

procedure SkipLocPred(var aFrm: FrmPtr);
 var lFrm: FrmPtr;
begin
 repeat
  while aFrm^.FrmSort=ikFrmPrivPred do
    if LocPredFrmPtr(aFrm)^.PredExp^.FrmSort = ikError then exit
    else aFrm:=LocPredFrmPtr(aFrm)^.PredExp;
  if (aFrm^.FrmSort=ikFrmNeg) and (NegFrmPtr(aFrm)^.NegArg^.FrmSort = ikFrmPrivPred) then
   begin
    lFrm:=LocPredFrmPtr(NegFrmPtr(aFrm)^.NegArg)^.PredExp;
    if lFrm^.FrmSort = ikError then exit;
    while lFrm^.FrmSort=ikFrmPrivPred do
      lFrm:=LocPredFrmPtr(lFrm)^.PredExp;
    if lFrm^.FrmSort = ikError then exit;
    if lFrm^.FrmSort = ikFrmNeg then
      aFrm:=NegFrmPtr(lFrm)^.NegArg;
   end
 until aFrm^.FrmSort<>ikFrmPrivPred;
end;

function CompareTrms(fTrm1,fTrm2:TrmPtr):integer; forward;

function CompareTrmLists(fTrmList1,fTrmList2:TrmList):integer;
 var lInt:integer;
 {! uwaga zaklada sie ze lsity sa rowne !}
begin CompareTrmLists:=0;
 while fTrmList1 <> nil do
  begin lInt:=CompareTrms(fTrmList1^.XTrmPtr,fTrmList2^.XTrmPtr);
   if lInt <> 0 then begin CompareTrmLists:=lInt; exit end;
   fTrmList1:=fTrmList1^.NextTrm; fTrmList2:=fTrmList2^.NextTrm;
  end;
end;

function CompareAttr(aAttr1, aAttr2: AttrPtr): Integer;
  var lInt,lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 aAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 aAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 lInt:=CompareInt(lAttrNr1,lAttrNr2);
 if lInt <> 0 then begin CompareAttr:=lInt; exit end;
 lInt:=CompareInt(ord(aAttr1^.fNeg),ord(aAttr2^.fNeg));
 if lInt <> 0 then begin CompareAttr:=lInt; exit end;
 CompareAttr:=CompareTrmLists(lArgs1,lArgs2);
end;

function CompareClusters(aClu1,aClu2: AttrCollectionPtr): integer;
 var i,lInt: integer;
begin
  lInt:=CompareInt(aClu1^.Count,aClu2^.Count);
  if lInt <> 0 then begin CompareClusters:=lInt; exit end;
  for i:=0 to aClu1^.Count-1 do
   begin
    lInt:=CompareAttr(AttrPtr(AttrCollectionPtr(aClu1)^.Items^[i]),AttrPtr(AttrCollectionPtr(aClu2)^.Items^[i]));
    if lInt <> 0 then begin CompareClusters:=lInt; exit end;
   end;
  CompareClusters:=0;
end;

function CompareTyps(fTyp1,fTyp2:TypPtr):integer; forward;

function CompareTypColls(const fColl1,fColl2:MCollection):integer;
 { kolekcje typow }
 var lint,k:integer;
begin CompareTypColls:=0;
 lInt:=CompareInt(fColl1.Count,fColl2.Count);
 if lInt <> 0 then begin CompareTypColls:=lInt; exit end;
 for k:=0 to fColl1.Count-1 do
  begin
   lInt:=CompareTyps(fColl1.Items^[k],fColl2.Items^[k]);
   if lInt <> 0 then begin CompareTypColls:=lInt; exit end;
  end;
end;

function CompareTyps(fTyp1,fTyp2:TypPtr):integer;
 var lInt:integer;
begin
 with fTyp1^ do
 begin
   lInt:=CompareInt(ord(TypSort),ord(fTyp2^.TypSort));
   if lInt <> 0 then begin CompareTyps:=lInt; exit end;
   lInt:=CompareInt(ModNr,fTyp2^.ModNr);
   if lInt <> 0 then begin CompareTyps:=lInt; exit end;
   lInt:=CompareClusters(LowerCluster,fTyp2^.LowerCluster);
   if lInt <> 0 then begin CompareTyps:=lInt; exit end;
   CompareTyps:=CompareTrmLists(ModArgs,fTyp2^.ModArgs);
 end;
end;

function CompareFrms(fFrm1,fFrm2:FrmPtr):integer;
 var lInt,k:integer;
begin CompareFrms:=0;
 SkipLocPred(fFrm1);
// SkipLocPred(fFrm1); To be checked later whether SkipLocPred(fFrm2) is not needed here!!!
 lInt:=CompareInt(ord(fFrm1^.FrmSort),ord(fFrm2^.FrmSort));
 if lInt <> 0 then begin CompareFrms:=lInt; exit end;
 case fFrm1^.FrmSort of
  ikFrmVerum,ikFrmThesis: ;
  ikFrmNeg: CompareFrms:=CompareFrms(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmQual:
    with QualFrmPtr(fFrm1)^ do
    begin
     lInt:=CompareTrms(QualTrm,QualFrmPtr(fFrm2)^.QualTrm);
     if lInt <> 0 then begin CompareFrms:=lInt; exit end;
     CompareFrms:=CompareTyps(QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
    end;
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^ do
    begin
     lInt:=CompareInt(Conjuncts.Count,ConjFrmPtr(fFrm2)^.Conjuncts.Count);
     if lInt <> 0 then begin CompareFrms:=lInt; exit end;
     for k:=0 to Conjuncts.Count-1 do
      begin
       lInt:=CompareFrms(Conjuncts.Items^[k],ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k]);
       if lInt <> 0 then begin CompareFrms:=lInt; exit end;
      end;
    end;
   ikFrmSchPred,ikFrmAttr,ikFrmPrivPred,ikFrmPred:
    with PredFrmPtr(fFrm1)^ do
    begin
     lInt:=CompareInt(PredNr,PredFrmPtr(fFrm2)^.PredNr);
     if lInt <> 0 then begin CompareFrms:=lInt; exit end;
     CompareFrms:=CompareTrmLists(PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
    end;
   ikFrmUniv:
    with UnivFrmPtr(fFrm1)^ do
    begin
      lInt:=CompareTyps(Quantified,UnivFrmPtr(fFrm2)^.Quantified);
      if lInt <> 0 then begin CompareFrms:=lInt; exit end;
      CompareFrms:=CompareFrms(Scope,UnivFrmPtr(fFrm2)^.Scope);
    end;
   ikError: ;
   else RunTimeError(2049);
  end;
end;

function SizeOfTrm(fTrm:TrmPtr):integer; FORWARD;

function SizeOfTrmList(fTrmList:TrmList):integer;
 var s:integer;
begin s:=0;
 while fTrmList <> nil do
  with fTrmList^ do begin inc(s,SizeOfTrm(XTrmPtr)); fTrmList:=NextTrm end;
 SizeOfTrmList:=s;
end;

function SizeOfTyp(fTyp:TypPtr):integer;
begin
 with fTyp^ do
  case TypSort of
   ikTypMode,ikTypStruct: SizeOfTyp:=SizeOfTrmList(ModArgs) + 1;
    else SizeOfTyp:=1;
  end;
end;

function SizeOfTrm(fTrm:TrmPtr):integer;
begin
 case fTrm^.TrmSort of
  ikTrmSchFunc,ikTrmAggreg,ikTrmFunctor,ikTrmSelector:
   SizeOfTrm:=SizeOfTrmList(FuncTrmPtr(fTrm)^.FuncArgs) + 1;
  ikTrmPrivFunc:
   SizeOfTrm:=SizeOfTrm(LocFuncTrmPtr(fTrm)^.FuncExp);
  ikTrmChoice:
   SizeOfTrm:=SizeOfTyp(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
  ikTrmQua:
   with QuaTrmPtr(fTrm)^ do
    SizeOfTrm:=SizeOfTrm(TrmProper) + SizeOfTyp(Qua);
  else SizeOfTrm:=1;
 end;
end;

function CompareTrms(fTrm1,fTrm2:TrmPtr):integer;
 var lInt:integer;
begin CompareTrms:=0;
 lInt:=CompareInt(SizeOfTrm(fTrm1),SizeOfTrm(fTrm2));
 if lInt <> 0 then begin CompareTrms:=lInt; exit end;
 while fTrm1^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm1)^.FuncExp^.TrmSort=ikError then break
  else fTrm1:=LocFuncTrmPtr(fTrm1)^.FuncExp;
 while fTrm2^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm2)^.FuncExp^.TrmSort=ikError then break
  else fTrm2:=LocFuncTrmPtr(fTrm2)^.FuncExp;
 with fTrm1^ do
  begin
   lInt:=CompareInt(ord(TrmSort),ord(fTrm2^.TrmSort));
   if lInt <> 0 then begin CompareTrms:=lInt; exit end;
   case TrmSort of
    ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral,ikTrmEqConst:
    with VarTrmPtr(fTrm1)^ do
     begin
      CompareTrms:=CompareInt(VarNr,VarTrmPtr(fTrm2)^.VarNr);
     end;
    ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmFunctor,ikTrmSelector:
    with FuncTrmPtr(fTrm1)^ do
     begin
      lInt:=CompareInt(FuncNr,FuncTrmPtr(fTrm2)^.FuncNr);
      if lInt <> 0 then begin CompareTrms:=lInt; exit end;
      CompareTrms:=CompareTrmLists(FuncArgs,FuncTrmPtr(fTrm2)^.FuncArgs);
     end;
    ikTrmFraenkel:
     with FraenkelTrmPtr(fTrm1)^  do
      begin
       lInt:=CompareTrms(LambdaScope,FraenkelTrmPtr(fTrm2)^.LambdaScope);
       if lInt <> 0 then begin CompareTrms:=lInt; exit end;
       lInt:=CompareFrms(Compr,FraenkelTrmPtr(fTrm2)^.Compr);
       if lInt <> 0 then begin CompareTrms:=lInt; exit end;
       CompareTrms:=CompareTypColls(LambdaArgs,FraenkelTrmPtr(fTrm2)^.LambdaArgs);
      end;
    ikTrmChoice:
       CompareTrms:=CompareTyps(ChoiceTrmPtr(fTrm1)^.ChoiceTyp,
                                ChoiceTrmPtr(fTrm2)^.ChoiceTyp);
    ikTrmQua:
     with QuaTrmPtr(fTrm1)^ do
      begin
       lInt:=CompareTrms(TrmProper,QuaTrmPtr(fTrm2)^.TrmProper);
       if lInt <> 0 then begin CompareTrms:=lInt; exit end;
       CompareTrms:=CompareTyps(Qua,QuaTrmPtr(fTrm2)^.Qua);
      end;
    ikTrmIt: ;
    else
     begin
{$IFDEF MDEBUG}
writeln(InfoFile,'CompareTrms:TrmSort=',TrmSort,'|');
{$ENDIF}
       RunTimeError(2341);
     end;
   end;
  end;
end;

{v Kolekcjonowanie termow v}

function TTCollection.KeyOf(Item:pointer): pointer;
begin KeyOf:=TTPairPtr(Item)^.nTrm;
end;

function TTCollection.Compare(Key1,Key2:pointer): integer;
begin Compare:=CompareTrms(TrmPtr(Key1),TrmPtr(Key2));
end;

constructor TTPairObj.Init(fTrm:TrmPtr);
 var lClusterPtr: AttrCollectionPtr;
begin nTermScope:=gTermScope;
 nTrm:=CopyTerm(fTrm);
 nTyp:=CopyTrmType(fTrm);
end;

destructor TTPairObj.Done;
begin DisposeTrm(nTrm);
 dispose(nTyp,Done);
end;

procedure MarkTermsInTTColl;
begin
 inc(gTermScope);
end;

function RoundUpTrmTypeWithType(lTyp:TypPtr; fTrm:TrmPtr): TypPtr;
var i,lLeft,lRight:integer;
    lClusterPtr: AttrCollectionPtr;
    lFunctorCluster: FClusterPtr;
    lKey: FClusterObj;
    lIntSet: IntSet;
 label Inconsistent{,1};
begin
 if fTrm^.TrmSort in [ikTrmFunctor,ikTrmSelector,ikTrmAggreg] then
  begin
   lClusterPtr:=CopyCluster(lTyp^.UpperCluster);
   lKey.nClusterTerm:=fTrm;
   if FunctorCluster.FindInterval( @lKey, lLeft, lRight) then
   begin
    i:=lLeft;
    lIntSet.Init(0);
    while i <= lRight do
     begin
      if not lIntSet.IsInSet(i) then
       begin lFunctorCluster:=FunctorCluster.AtIndex(i);
        if RoundUpWith(lFunctorCluster,fTrm,lTyp,lClusterPtr) then
         begin
          lClusterPtr^.RoundUpWith(lTyp);
          dispose(lTyp^.UpperCluster,Done);
          lTyp^.UpperCluster:=lClusterPtr;
          if not lClusterPtr^.fConsistent then
           begin
            lIntSet.Done;
            goto Inconsistent;
           end;
          lClusterPtr:=CopyCluster(lTyp^.UpperCluster);
          lIntSet.Insert(i);
          if lFunctorCluster^.nClusterType <> nil then
           begin i:=lLeft; continue end;
         end
        else if lFunctorCluster^.nClusterType = nil then
         lIntSet.Insert(i);
       end;
      inc(i);
     end;
     lIntSet.Done;
   end;
Inconsistent:
  end;
 RoundUpTrmTypeWithType:=lTyp;
end;

function InsertTermInTTColl(fTrm:TrmPtr): integer;
 var lPlace,i:integer;
     lTTPairPtr: TTPairPtr;
     lClusterPtr: AttrCollectionPtr;
     lTrmList: TrmList;
     lTrm:TrmPtr;
begin
 case fTrm^.TrmSort of
  ikTrmFunctor,ikTrmSelector,ikTrmAggreg:
   begin
    with FuncTrmPtr(fTrm)^ do
     begin
      lTrmList:=FuncArgs;
      while lTrmList<>nil do
       begin
        lTrm:=lTrmList^.XTrmPtr;
        while lTrm^.TrmSort=ikTrmPrivFunc do
         if LocFuncTrmPtr(lTrm)^.FuncExp^.TrmSort=ikError then break
         else lTrm:=LocFuncTrmPtr(lTrm)^.FuncExp;
        if lTrm^.TrmSort in [ikTrmFunctor,ikTrmSelector,ikTrmAggreg] then
         InsertTermInTTColl(lTrm);
        lTrmList:=lTrmList^.NextTrm;
       end;
     end;
   end;
 end;
 with gTermCollection do
  if not Search(fTrm,lPlace) then
   begin
    lTTPairPtr:=new(TTPairPtr,Init(fTrm));
    AtInsert(lPlace,lTTPairPtr);
    lTTPairPtr^.nTyp:=RoundUpTrmTypeWithType(lTTPairPtr^.nTyp,fTrm);
    lClusterPtr:=CopyCluster(lTTPairPtr^.nTyp^.UpperCluster);
    lClusterPtr^.RoundUpWith(lTTPairPtr^.nTyp);
    dispose(lTTPairPtr^.nTyp^.UpperCluster,Done);
    lTTPairPtr^.nTyp^.UpperCluster:=lClusterPtr;
    for i := 0 to Count-1 do
     if lTTPairPtr = Items^[i] then
      begin
       lPlace:=i;
       break
      end;
//    Search(fTrm,lPlace);       // np. nie dziala w przypadku argumentow z fraenkelem
    mizassert(2888,lPlace>=0);
     // moga pojawic sie termy w  wyniku zawolania procedur RoundUp zmieni sie index termu na kolekcji posortowanej
   end;
 result:=lPlace;
end;

procedure RemoveTermsFromTTColl;
 var k:integer;
begin dec(gTermScope);
 with gTermCollection do
  for k:=0 to Count-1 do
   if TTPairPtr(Items^[k])^.nTermScope > gTermScope then
   begin
    dispose(TTPairPtr(Items^[k]),Done);
    Items^[k]:=nil
   end;
 gTermCollection.Pack;
end;

{^ Kolekcjonowanie termow ^}

function FuncRedefNr(fNr:integer): integer;
begin
 with ConstrPtr( Constr[ coFunctor].Items^[ fNr])^ do
 if fWhichConstrNr <> 0 then
  FuncRedefNr:= fWhichConstrNr
 else FuncRedefNr:= fNr;
end;

// this is now very simple, ikTrmFunctor > any other ikTrm,
// all other ikTrm equal, and two ikTrmFunctor by adjusted number
function CmpFuncTrm( fTrm1,fTrm2: TrmPtr): integer;
begin
  if fTrm1^.TrmSort <> ikTrmFunctor then
  begin
   if fTrm2^.TrmSort <> ikTrmFunctor then CmpFuncTrm:= 0
   else CmpFuncTrm:= -1
  end
  else
  begin
   if fTrm2^.TrmSort <> ikTrmFunctor then CmpFuncTrm:= 1
   else
    CmpFuncTrm := CompareInt( FuncRedefNr( FuncTrmPtr( fTrm1)^.FuncNr),
                              FuncRedefNr( FuncTrmPtr( fTrm2)^.FuncNr));
  end;
end;

function CmpFuncCluster(fCl1,fCl2: Pointer): integer;
begin   {?}
 CmpFuncCluster := CmpFuncTrm( FClusterPtr(fCl1)^.nClusterTerm,
                               FClusterPtr(fCl2)^.nClusterTerm);
end;

procedure InsertArgument(fInt:integer);
begin
 Mizassert(2537,gTrmList<>nil);
 if gTrmList^.XTrmPtr^.TrmSort = ikTrmExactly then gExactly[fInt]:=true;
 gSubstTrm[fInt]:=gTrmList^.XTrmPtr;
 gTrmList:=gTrmList^.NextTrm;
end;

constructor PredFrmObj.Init(fPredKind:char; fPredNr:integer; fPredArgs:TrmList);
begin FrmSort:=fPredKind;
 PredNr:=fPredNr; PredArgs:=fPredArgs;
end;

constructor PredFrmObj.InitP(fPredKind:char; fPredNr:integer;
                             fPredArgs:TrmList; fPattNr:integer);
begin Init(fPredKind, fPredNr, fPredArgs); nPattNr:= fPattNr; end;

destructor PredFrmObj.Done;
begin DisposeTrmList(PredArgs) end;

constructor LocPredFrmObj.Init(aPredNr:integer; aPredArgs:TrmList; aExp:FrmPtr);
begin FrmSort:=ikFrmPrivPred;
 PredNr:=aPredNr; PredArgs:=aPredArgs;
 PredExp:=aExp;
end;

constructor LocPredFrmObj.InitP(aPredNr:integer; aPredArgs:TrmList;
                                aExp:FrmPtr; aPattNr:integer);
begin Init(aPredNr, aPredArgs, aExp); nPattNr:= aPattNr; end;

destructor LocPredFrmObj.Done;
begin DisposeTrmList(PredArgs);
 dispose(PredExp, Done);
end;

constructor QualFrmObj.Init(fQualTrm: TrmPtr; fQualTyp: TypPtr);
begin FrmSort:=ikFrmQual; QualTrm:=fQualTrm; QualTyp:=fQualTyp end;

constructor QualFrmObj.InitP(fQualTrm: TrmPtr; fQualTyp: TypPtr;
                             fPattNr:integer);
begin Init(fQualTrm, fQualTyp); nPattNr:= fPattNr; end;

destructor QualFrmObj.Done;
begin DisposeTrm(QualTrm); dispose(QualTyp, Done) end;

constructor NegFrmObj.Init(fNegArg: FrmPtr);
begin
 FrmSort:=ikFrmNeg;
 NegArg:=fNegArg
end;

constructor NegFrmObj.InitP(fNegArg: FrmPtr; fPattNr:integer);
begin Init(fNegArg); nPattNr:= fPattNr; end;

destructor NegFrmObj.Done;
begin dispose(NegArg, Done) end;

constructor BinFrmObj.Init(aFrmSort:char; aLeftArg,aRightArg: FrmPtr);
begin FrmSort:=aFrmSort;
 nLeftArg:=aLeftArg;
 nRightArg:=aRightArg;
end;

destructor BinFrmObj.Done;
begin dispose(nLeftArg, Done); dispose(nRightArg, Done) end;

constructor ConjFrmObj.Init(const fConjuncts: MCollection);
begin FrmSort:=ikFrmConj; move(fConjuncts,Conjuncts,SizeOf(MCollection)) end;

constructor ConjFrmObj.InitP(const fConjuncts: MCollection; fPattNr:integer);
begin Init(fConjuncts); nPattNr:= fPattNr; end;

destructor ConjFrmObj.Done;
begin Conjuncts.Done end;

constructor UnivFrmObj.Init(fSort:char; fQuantified: TypPtr; fScope: FrmPtr);
begin nVarId:=0; FrmSort:=fSort; Quantified:=fQuantified; Scope:=fScope end;

constructor UnivFrmObj.InitP(fQuantified: TypPtr; fScope: FrmPtr;
                             fPattNr:integer);
begin Init(ikFrmUniv,fQuantified, fScope); nPattNr:= fPattNr; end;

constructor UnivFrmObj.InitI(aQuantified:TypPtr; aScope:FrmPtr; aVarId:integer);
begin InitP(aQuantified, aScope,0); nVarId:=aVarId; end;

destructor UnivFrmObj.Done;
begin dispose(Quantified,Done); dispose(Scope,Done) end;

constructor UniqFrmObj.Init(fKind: char);
begin FrmSort:=fKind end;

constructor UniqFrmObj.InitP(fKind: char; fPattNr:integer);
begin Init(fKind); nPattNr:= fPattNr; end;

constructor FlexFrmObj.Init(fLeftOrigFrm,fRightOrigFrm:FrmPtr; fLeftTrm,fRightTrm:TrmPtr);
begin
 FrmSort:=ikFrmFlexConj;
 nLeftOrigFrm:=fLeftOrigFrm;
 nRightOrigFrm:=fRightOrigFrm;
 nLeftTrm:=fLeftTrm;
 nRightTrm:=fRightTrm;
end;

constructor FlexFrmObj.InitD(fLeftOrigFrm,fRightOrigFrm,fExpansion:FrmPtr; fLeftTrm,fRightTrm:TrmPtr);
begin
 Init(fLeftOrigFrm,fRightOrigFrm,fLeftTrm,fRightTrm);
 nExpansion:=fExpansion;
end; 

function FlexFrmObj.CopyFormula: FrmPtr;
begin
 CopyFormula:=new(FlexFrmPtr,InitD( nLeftOrigFrm.CopyFormula, nRightOrigFrm.CopyFormula,
                                    nExpansion.CopyFormula,CopyTerm(nLeftTrm),
                                    CopyTerm(nRightTrm)));
 end;

procedure FlexFrmObj.WithinFrm;
begin
 nLeftOrigFrm.WithinFrm;
 nRightOrigFrm.WithinFrm;
 nExpansion.WithinFrm;
 WithinTrm(nLeftTrm);
 WithinTrm(nRightTrm);
end;

procedure ChangeToLoci(var fTrm:TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do if TrmSort=ikTrmConstant then
  if (VarNr>gDefBase) and (FixedVar[VarNr].nSkelConstNr<>0) then
   begin TrmSort:=ikTrmLocus; VarNr:=FixedVar[VarNr].nSkelConstNr end;
end;

constructor ArityObj.Init;
begin
 nId:=0;
 fPrimaries.Init(5);
end;

destructor ArityObj.Done;
begin
 fPrimaries.Done;
 inherited Done;
end;

constructor FuncDefObj.Init(aId:integer; var aPrimArgs: MList; aFuncDef: TrmPtr; aFuncTyp: TypPtr);
begin
 nId:=aId;
 fPrimaries.MoveList(aPrimArgs);
 fFuncDef:=aFuncDef;
 fFuncTyp:=aFuncTyp;
end;

destructor FuncDefObj.Done;
begin
 DisposeTrm(fFuncDef);
 if fFuncTyp <> nil then
  Dispose(fFuncTyp,Done);
 inherited Done;
end;

constructor LocPredDefObj.Init(aId:integer; var aPrimArgs: MList; aPredDef: FrmPtr);
begin
 nId:=aId;
 fPrimaries.MoveList(aPrimArgs);
 fPredDef:=aPredDef;
end;

destructor LocPredDefObj.Done;
begin
 dispose(fPredDef,Done);
 inherited Done;
end;

constructor ConstrObj.InitInternal(aKind: ConstructorsKind; aNr: integer;
                                  aArticle: string);
begin
 fConstrKind	:= aKind;
 fAbsNr		:= aNr;
 fArticle	:= aArticle;
 fWhichConstrNr	:= 0;
 fSuperfluous	:= 0;
 fProperties	:= [];
 fFirstArg	:= 0;
 fSecondArg	:= 0;
end;

constructor ConstrObj.Init(aKind: ConstructorsKind; aNr: integer;
                           aArticle: string);
begin
 InitInternal( aKind, aNr, aArticle);
 nPrimaries.Init(0,4);
 fStructLoci.InitNatFunc(0,4);
end;

// ##NOTE: used for initializing from a patternobj
// ##REQUIRE: aPrimArgs contains proper Loci variables (ikTrmLocus)
constructor ConstrObj.InitForPattern(aKind: ConstructorsKind; aNr: integer;
                                     aArticle: string; const aPrimArgs:MList);
var i:integer;
begin
 InitInternal( aKind, aNr, aArticle);  
 nPrimaries.Init(aPrimArgs.Count,0);
 for i:=0 to aPrimArgs.Count-1 do
  nPrimaries.Insert(TypPtr(aPrimArgs.Items^[i])^.CopyType);
 fStructLoci.InitNatFunc(0,4);
end;

// ###TODO: change this to only Done when debugged
destructor ConstrObj.Done;
begin
 nPrimaries.DeleteAll;
 nPrimaries.Done;
 fStructLoci.Done;
end;

procedure ConstrObj.SetProperties(aProperties: PropertiesRec);
begin
 fProperties	:= aProperties.Properties;
 fFirstArg	:= aProperties.nFirstArg;
 fSecondArg	:= aProperties.nSecondArg;
end;

procedure ConstrObj.GetProperties( var aProperties: PropertiesRec);
begin
 aProperties.Properties	:= fProperties;
 aProperties.nFirstArg  := fFirstArg;
 aProperties.nSecondArg := fSecondArg;
end;

procedure ConstrObj.SetRedef( aWhich,aSuperfluous:integer );
begin
 fWhichConstrNr	:= aWhich;
 fSuperfluous	:= aSuperfluous;
end;

constructor ConstrTypObj.Init(aKind: ConstructorsKind; aNr: integer;
                              aArticle: string);
begin
 inherited Init(aKind, aNr, aArticle);
 fConstrTyp	:= nil;
end;

constructor ConstrTypObj.InitForPattern(aKind: ConstructorsKind; aNr: integer;
                                        aArticle: string;
                                        const aPrimArgs: MList; fTyp:TypPtr);
begin
 inherited InitForPattern( aKind, aNr, aArticle, aPrimArgs);
 fConstrTyp:=fTyp^.CopyType;
 fConstrTyp^.WithinType(ChangeToLoci);
end;

procedure ConstrTypObj.RoundUp;
 var lClusterPtr,lClPtr: AttrCollectionPtr;
begin
 with fConstrTyp^ do
  begin
   lClusterPtr:=CopyCluster(LowerCluster);
   if TypSort = ikTypMode then
    begin
     lClPtr:=InstCluster(ConstrTypPtr(Constr[coMode].Items^[ModNr]
                                        )^.fConstrTyp^.UpperCluster,ModArgs);
     lClusterPtr^.EnlargeBy(lClPtr);
     dispose(lClPtr,Done);
    end;
   move(nPrimaries.Items^,LocArgTyp[1],nPrimaries.Count*sizeof(pointer));
   lClusterPtr^.RoundUpWith(fConstrTyp);
   dispose(UpperCluster,Done);
   UpperCluster:=lClusterPtr;
  end;
end;

destructor ConstrTypObj.Done;
begin
 if fConstrTyp<>nil then dispose(fConstrTyp,Done);
 inherited Done;
end;

constructor StructConstrObj.Init( aNr: integer; aArticle: string);
begin
 fPrefixes.Init(0,8);
 fStructModeAggrNr:=0;
 fFields:=new(NatSetPtr,Init(0,8));
 inherited Init(coStructMode, aNr, aArticle);
end;

constructor StructConstrObj.InitForPattern(aNr: integer; aArticle: string;
                                           const aPrimArgs: MList);
begin
 inherited InitForPattern( coStructMode, aNr, aArticle, aPrimArgs); 
 fPrefixes.Init(0,8);
 fStructModeAggrNr:=0;
 fFields:=new(NatSetPtr,Init(0,8));
end; 

destructor StructConstrObj.Done;
begin
 fPrefixes.Done;
 if fFields<>nil then dispose(fFields,Done);
 inherited Done;
end;

constructor AggrConstrObj.Init( aNr: integer; aArticle: string);
begin
 fAggrColl:=new(PCollection,Init(0,8));
 fAggregBase:=0;
 inherited Init(CoAggregate, aNr, aArticle);
end;

constructor AggrConstrObj.InitForPattern( aNr: integer; aArticle: string;
                                          const aPrimArgs: MList; fTyp:TypPtr);
begin
 inherited InitForPattern( coAggregate, aNr, aArticle, aPrimArgs, fTyp);
 fAggregBase:=0;
 fAggrColl:=new(PCollection,Init(0,8));
end;

destructor AggrConstrObj.Done;
begin
 if fAggrColl<>nil then dispose(fAggrColl,Done);
 inherited Done;
end;


constructor PatternObj.Init( aKind: NotationKind; aNr: integer;
                             aArticle: string);
begin
  fKind:= aKind;
  fAbsNr:= aNr;
  fArticle:= aArticle;
  rConstr.Kind:=ikError;
  fRedefNr:= 0;
  fFormNr:=0;
  fFormat:=nil;
  fAntonymic:=false;
  Expansion:=nil;
end;

destructor PatternObj.Done;
begin
 fPrimTypes.Done;
 if fFormat<>nil then  dispose(fFormat,Done);
 Visible.Done;
 if Expansion <> nil then dispose(Expansion,Done);
end;

constructor ClusterObj.Init(aNr: integer; aArticle: string;
                            const aCons:ClusterRec; var aColl:MCollection);
begin
 nAbsNr:= aNr;
 nArticle:= aArticle;
 nConsequent:=aCons;
 nPrimaryList.MoveCollection(aColl);
end;

destructor ClusterObj.Done;
begin
 nConsequent.Lower^.Done;
 nConsequent.Upper^.Done;
 nPrimaryList.Done;
end;

constructor RClusterObj.Init(aNr: integer; aArticle: string;
                             const aCons:ClusterRec; var aColl:MCollection; aTyp:TypPtr);
begin
 ClusterObj.Init(aNr, aArticle, aCons, aColl);
 nClusterKind	:= clRegistered;
 nClusterType	:= aTyp;
end;

destructor RClusterObj.Done;
begin
 if nClusterType<>nil then dispose(nClusterType,Done);
 inherited Done;
end;

constructor RClusterObj.RegisterCluster(aNr: integer; aArticle: string; aClu:AttrCollectionPtr;
                                 var fPrim:MCollection; fTyp:TypPtr);
var lTyp:TypPtr;
begin
 nClusterKind:= clRegistered;
 nAbsNr:= aNr;
 nArticle:= aArticle;
 move(fPrim.Items^,LocArgTyp[1],fPrim.Count*sizeof(pointer));
 nConsequent.Lower:=CopyCluster(aClu);
 nConsequent.Lower^.EnlargeBy(fTyp^.UpperCluster); //LowerCluster ??
 nConsequent.Upper:=CopyCluster(aClu);
 nConsequent.Upper^.EnlargeBy(fTyp^.UpperCluster);
 nConsequent.Upper^.RoundUpWith(fTyp);
 if not nConsequent.Upper^.fConsistent then Error(CurPos,95);
 nConsequent.Lower^.WithinAttrCollection(ChangeToLoci);
 nConsequent.Upper^.WithinAttrCollection(ChangeToLoci);
 nPrimaryList.MoveCollection(fPrim);
 { Typ tutaj jest adjustowany, tzn. niektore atrybuty mogly stracic
   sens !!!!!!!!!!!!! To wogole jest jakis dziwny problem,
   Zeby atrybut byl klastrowalny, to wszystkie lokusy musza wystapic
   jako argumenty jego typu, tzn. ze nie wolno tutaj adjustowac,
   bo jakies atrybuty moga przestac byc klastrowalne !!!!!!!
 }
 lTyp:=AdjustedType(fTyp);
 dispose(lTyp^.LowerCluster,Done);
 lTyp^.LowerCluster:=NewEmptyCluster;
 dispose(lTyp^.UpperCluster,Done);
 lTyp^.UpperCluster:=NewEmptyCluster;
 lTyp^.WithinType(ChangeToLoci);
 nClusterType:=lTyp;
end;

constructor CClusterObj.Init(aNr: integer; aArticle: string; aAntec:AttrCollectionPtr;
                             const aCons: ClusterRec;
                             var aColl:MCollection; aTyp:TypPtr);
begin
 ClusterObj.Init(aNr, aArticle, aCons, aColl);
 nClusterKind	:= clConditional;
 nAntecedent	:= aAntec;
 nClusterType	:= aTyp;
end;

destructor CClusterObj.Done;
begin
 nAntecedent^.Done;
 inherited Done;
end;

constructor CClusterObj.RegisterCluster(aNr: integer; aArticle: string; aClu1,aClu2:AttrCollectionPtr;
                                 var fPrim:MCollection; fTyp:TypPtr);
var lTyp:TypPtr;
begin
 nClusterKind:= clConditional;
 nAbsNr:= aNr;
 nArticle:= aArticle;
 nAntecedent:=aClu1;
 nConsequent.Lower:=aClu2;
 nConsequent.Upper:=CopyCluster(aClu2);
 nPrimaryList.MoveCollection(fPrim);
 nClusterType:=fTyp^.CopyType;
 nConsequent.Lower^.WithinAttrCollection(ChangeToLoci);
 nConsequent.Upper^.WithinAttrCollection(ChangeToLoci);
 nClusterType^.WithinType(ChangeToLoci);
 nAntecedent^.WithinAttrCollection(ChangeToLoci);
end;

constructor FClusterObj.Init(aNr: integer; aArticle: string;
                             const aCons: ClusterREc; var aColl:MCollection;
                             aTrm:TrmPtr; aTyp:TypPtr);
begin
 ClusterObj.Init(aNr, aArticle, aCons, aColl);
 nClusterKind	:= clFunctor;
 nClusterTerm	:= aTrm;
 nClusterType	:= aTyp;
end;

destructor FClusterObj.Done;
begin
 if nClusterTerm <> nil then DisposeTrm(nClusterTerm);
 if nClusterType <> nil then dispose(nClusterType,Done);
 inherited Done;
end;

constructor FClusterObj.RegisterCluster(aNr: integer; aArticle: string; aClu:AttrCollectionPtr;
                                 var fPrim:MCollection; fTrm:TrmPtr; aTyp:TypPtr);
var
 lTyp:TypPtr;
 lFuncNr:integer; A:TrmList;
begin
 nClusterKind:= clFunctor;
 nAbsNr:= aNr;
 nArticle:= aArticle;
 case fTrm^.TrmSort of
  ikTrmFunctor,ikTrmSelector,ikTrmAggreg:
   begin
    if fTrm^.TrmSort = ikTrmFunctor then
     begin
      AdjustTrm(fTrm,lFuncNr,A);
      nClusterTerm:=NewFuncTrm(lFuncNr,CopyTermList(A));
//      nClusterTerm^.nPattNr:=fTrm.nPattNr;
     end
    else nClusterTerm:=CopyTerm(fTrm);
    move(fPrim.Items^,LocArgTyp[1],fPrim.Count*sizeof(pointer));
    nClusterType:=nil;
    if aTyp <> nil then
     begin
      nClusterType:=aTyp^.CopyType;
      lTyp:=nClusterType
     end
    else lTyp:=RoundUpTrmType(nClusterTerm);
    nConsequent.Lower:=CopyCluster(aClu);
    nConsequent.Upper:=CopyCluster(aClu);
    nConsequent.Upper^.EnlargeBy(lTyp^.UpperCluster);
    nConsequent.Upper^.RoundUpWith(lTyp);
    if not nConsequent.Upper^.fConsistent then Error(CurPos,95);
    if nClusterType = nil then
     dispose(lTyp,Done);
    WithinTerm(nClusterTerm,ChangeToLoci);
    nConsequent.Lower^.WithinAttrCollection(ChangeToLoci);
    nConsequent.Upper^.WithinAttrCollection(ChangeToLoci);
    nPrimaryList.MoveCollection(fPrim);
    if nClusterType <> nil then
      nClusterType^.WithinType(ChangeToLoci);
   end;
  else
   begin
    nClusterTerm:=NewIncorTrm;
    nConsequent.Lower:=NewEmptyCluster;
    nConsequent.Upper:=NewEmptyCluster;
    nPrimaryList.Init(0,0);
   end;
 end;
end;

procedure InitConstructors;
 var c:ConstructorsKind;
begin
 BoundVarNbr:=0;
 new(InCorrTrmList);
 with InCorrTrmList^ do
 begin
  XTrmPtr:=NewInCorTrm;
  NextTrm:=nil;
 end;
 for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
 begin
  Constr[c].Init(128,128);
  case c of
   coPredicate: Constr[c].Insert( new(ConstrPtr,Init(c,0,'')));
   coAggregate: Constr[c].Insert( new(AggrConstrPtr,Init(0,'')));
   coStructMode: Constr[c].Insert( new(StructConstrPtr,Init(0,'')));
  else Constr[c].Insert( new(ConstrTypPtr,Init(c,0,'')));
  end;
  if c in TypedConstrKinds then
   ConstrTypPtr( Constr[c].Items^[0])^.fConstrTyp := NewIncorTyp;
 end;
 { Klastry }
 RegisteredCluster.Init(100);
 ConditionalCluster.Init(100);
 FunctorCluster.InitSorted( 100, CmpFuncCluster);
 { local }
 LocPredDef.Init(MaxPredVarNbr);
 LocFuncDef.Init(MaxFuncVarNbr);
end;

procedure DisposeConstructors;
var c:ConstructorsKind;
begin
 for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
  Constr[c].Done;
 RegisteredCluster.Done;
 FunctorCluster.Done;
 ConditionalCluster.Done;
 dispose(NonZeroTyp,Done);
 DisposeTrmList(InCorrTrmList);
end;

function CreateArgList(aLength:integer): TrmList;
 var k: integer; lArgs: TrmList;
begin lArgs:=nil;
 for k:=aLength downto 1 do
  lArgs:=NewTrmList(gSubstTrm[k],lArgs);
 CreateArgList:=lArgs;
end;

function CreateArgList1: TrmList;
 var k: integer; lTrmList: TrmList;
begin lTrmList:=nil;
 lTrmList:=nil;
 for k:=2*MaxArgNbr downto 1 do
  if gSubstTrm[k] <> nil then
   lTrmList:=NewTrmList(gSubstTrm[k],lTrmList);
 CreateArgList1:=lTrmList;
end;


procedure DisposeSubstTrm;
 var k: integer;
begin
   for k:=1 to 2*MaxArgNbr do
    if gSubstTrm[k] <> nil then
     begin DisposeTrm(gSubstTrm[k]); gSubstTrm[k]:=nil end;
end;

function NewTrmList ( fTrm:TrmPtr; fTrmList:TrmList ):TrmList;
 var lTrmList: TrmList;
begin
 if fTrm^.TrmSort = ikError then begin NewTrmList:=InCorrTrmList; Exit end;
 new(lTrmList); NewTrmList:=lTrmList;
 with lTrmList^ do begin XTrmPtr:=fTrm; NextTrm:=fTrmList end;
end;

function AddToTrmList( fTrmList:TrmList; fTrm:TrmPtr ):TrmList;
 var lTrmList: TrmList;
begin
 if fTrm^.TrmSort = ikError then begin AddToTrmList:=InCorrTrmList; Exit end;
 if fTrmList=nil then
  begin AddToTrmList:=NewTrmList(fTrm,nil); exit end;
 lTrmList:=LastElem(fTrmList);
 lTrmList^.NextTrm:=NewTrmList(fTrm,nil);
 AddToTrmList:=fTrmList;
end;

function NewVarTrm ( fSort:char; fNr:integer ):VarTrmPtr;
 var lTrm: VarTrmPtr;
begin
 lTrm:=new(VarTrmPtr,Init);
 NewVarTrm:=lTrm;
 with lTrm^ do begin TrmSort:=fSort; VarNr:=fNr end;
end;

function NewFuncTrm ( fFunc:integer; fArgs: TrmList ):TrmPtr;
 var lTrm: FuncTrmPtr;
begin
 if fFunc = 0 then
  begin NewFuncTrm:=NewIncorTrm; exit end;
 lTrm:=new(FuncTrmPtr,Init);
 NewFuncTrm:=lTrm;
 with FuncTrmPtr(lTrm)^ do
  begin TrmSort:=ikTrmFunctor; FuncNr:=fFunc; FuncArgs:=fArgs;
   TrmInfo:=0;
  end;
end;

function NewLocFuncTrm(fSort:char;fFunc:integer;fArgs:TrmList):FuncTrmPtr;
 var lTrm: FuncTrmPtr;
begin
 lTrm:=new(FuncTrmPtr,Init);
 NewLocFuncTrm:=lTrm;
 with lTrm^ do
  begin TrmSort:=fSort; FuncNr:=fFunc; FuncArgs:=fArgs;
   TrmInfo:=0;
  end;
end;

function NewPrivFuncTrm(aFunc:integer;aArgs:TrmList; aExp:TrmPtr):FuncTrmPtr;
 var lTrm: LocFuncTrmPtr;
begin
 lTrm:=new(LocFuncTrmPtr,Init);
 NewPrivFuncTrm:=lTrm;
 with lTrm^ do
  begin TrmSort:=ikTrmPrivFunc; FuncNr:=aFunc; FuncArgs:=aArgs; FuncExp:=aExp;
   TrmInfo:=0;
  end;
end;

function NewFraenkelTrm(fTrm:TrmPtr;fFrm:FrmPtr; var fColl:MCollection): FraenkelTrmPtr;
 var lIdents: IntSequence; i:integer;
begin
 lIdents.Init(fColl.Count);
 for i:=0 to fColl.Count-1 do lIdents.Insert(0);
 NewFraenkelTrm:= NewFraenkelTrmI(fTrm,fFrm,fColl,lIdents);
end;

function NewFraenkelTrmI(fTrm:TrmPtr;fFrm:FrmPtr; var fColl:MCollection; var fIdents:IntSequence): FraenkelTrmPtr;
var lTrm:FraenkelTrmPtr;
begin
 lTrm:=new(FraenkelTrmPtr,Init);
 NewFraenkelTrmI:=lTrm;
 with FraenkelTrmPtr(lTrm)^ do
  begin TrmSort:=ikTrmFraenkel; LambdaScope:=fTrm; Compr:=fFrm;
   Mizassert(errBadTypeIds, fIdents.fCount = fColl.Count);
   LambdaArgs.MoveCollection(fColl);
   nIdents.SetCapacity( fIdents.fCount);
   nIdents.AddSequence( fIdents);
  end;
end;

function NewQuaTrm(fTrmProper:TrmPtr; fTyp:TypPtr):TrmPtr;
 var lTrm:QuaTrmPtr;
begin
 lTrm:=new(QuaTrmPtr,Init);
 NewQuaTrm:=lTrm;
 with lTrm^ do
  begin TrmSort:=ikTrmQua; TrmProper:=fTrmProper; Qua:=fTyp end;
end;

function NewChoiceTrm(aTyp: TypPtr): TrmPtr;
var lTrm: TrmPtr;
begin
  lTrm:=new(ChoiceTrmPtr,Init(aTyp));
  lTrm^.TrmSort:=ikTrmChoice; 
  NewChoiceTrm:=lTrm;
end;

function NewItTrm: TrmPtr;
var lTrm: TrmPtr;
begin
 lTrm:=new(TrmPtr,Init);
 lTrm^.TrmSort:=ikTrmIt; NewItTrm:=lTrm;
end;

function NewInCorTrm: TrmPtr;
var lTrm:TrmPtr;
begin
 lTrm:=new(TrmPtr,Init);
 lTrm^.TrmSort:=ikError; NewInCorTrm:=lTrm;
end;

constructor AttrObj.Init(aNeg:byte; aAttr:integer; aArgs:TrmList);
begin
  fNeg:=aNeg;
  fAttrNr:=aAttr;
  fAttrArgs:=aArgs;
  fCollected:=false;
end;

constructor AttrObj.InitP(aNeg:byte; aAttr:integer;
                          aArgs:TrmList; aPattNr:integer);
begin Init(aNeg, aAttr, aArgs); nPattNr:= aPattNr; end;

constructor TypObj.Init(fSort:char; fLAttr,fUAttr: AttrCollectionPtr; fMod:integer; fArgs:TrmList);
begin TypSort:=fSort;
 LowerCluster:=fLAttr; UpperCluster:=fUAttr;
 ModNr:=fMod;
 ModArgs:=fArgs;
end;

constructor TypObj.InitP(fSort:char; fLAttr,fUAttr: AttrCollectionPtr;
                         fMod:integer; fArgs:TrmList; fPattNr:integer);
begin Init(fSort, fLAttr,fUAttr, fMod, fArgs); nPattNr:= fPattNr; end;

function NewEmptyCluster: AttrCollectionPtr;
begin
 NewEmptyCluster:=new(AttrCollectionPtr,Init(0,4));
end;

function NewStandardTyp (fSort:char; fLAttr,fUAttr: AttrCollectionPtr;
                          fMod:integer; fArgs:TrmList ):TypPtr;
begin
 if fMod <> 0 then
  NewStandardTyp:=new(TypPtr,Init(fSort,fLAttr,fUAttr,fMod,fArgs))
 else NewStandardTyp:=NewInCorTyp;
end;

function NewInCorTyp: TypPtr;
begin
 NewIncorTyp:=new(TypPtr,Init(ikError,NewEmptyCluster,NewEmptyCluster,0,nil));
end;

function NewVerum:FrmPtr;
begin NewVerum:=new(UniqFrmPtr,Init(ikFrmVerum)) end;

function NewNeg ( fArg:FrmPtr ):FrmPtr;
begin
 case fArg^.FrmSort of
  ikFrmNeg: begin NewNeg:=NegFrmPtr(fArg)^.NegArg end;
  ikError: NewNeg:=fArg;
  else NewNeg:=new(NegFrmPtr,Init(fArg));
 end;
end;

function NewNegDis ( fArg:FrmPtr ):FrmPtr;
begin
 case fArg^.FrmSort of
  ikFrmNeg: begin NewNegDis:=NegFrmPtr(fArg)^.NegArg; dispose(fArg) end;
  ikError: NewNegDis:=fArg;
  else NewNegDis:=new(NegFrmPtr,Init(fArg));
 end;
end;

function NewConjFrm(const fList:MCollection):FrmPtr;
begin
 case fList.Count of
  0: NewConjFrm:=NewVerum;
  1:
   begin NewConjFrm:=FrmPtr(fList.Items^[0]);
    fList.DeleteAll; fList.Done;
   end;
  else NewConjFrm:=new(ConjFrmPtr,Init(fList));
 end;
end;

function NewUniv ( fQuant:TypPtr; fScope:FrmPtr ):FrmPtr;
begin
 if (fQuant^.TypSort=ikError) or (fScope^.FrmSort=ikError) then
   begin NewUniv:=NewIncorFrm; exit end;
 NewUniv:=new(UnivFrmPtr,Init(ikFrmUniv,fQuant,fScope));
end;

function NewUnivI(aVarID:integer; aQuant:TypPtr; aScope:FrmPtr):FrmPtr;
begin
 if (aQuant^.TypSort=ikError) or (aScope^.FrmSort=ikError) then
   begin NewUnivI:=NewIncorFrm; exit end;
 NewUnivI:=new(UnivFrmPtr,InitI(aQuant,aScope,aVarId));
end;

function NewPredFrm ( fSort:char; fPred:integer; fArgs:TrmList; fPattNr:integer):FrmPtr;
begin NewPredFrm:=new(PredFrmPtr,InitP(fSort,fPred,fArgs,fPattNr)) end;

function NewLocPredFrm ( aPred:integer; aArgs:TrmList; aExp:FrmPtr ):FrmPtr;
begin NewLocPredFrm:=new(LocPredFrmPtr,Init(aPred,aArgs,aExp)) end;

function NewQualFrm ( fTrm:TrmPtr; fTyp:TypPtr ):FrmPtr;
begin
 if (fTyp^.TypSort=ikError) or (fTrm^.TrmSort=ikError) then
   begin NewQualFrm:=NewIncorFrm; exit end;
 NewQualFrm:=new(QualFrmPtr,Init(fTrm,fTyp));
end;

function NewEqFrm ( fLeft,fRight:TrmPtr ):FrmPtr;
begin
 if (fLeft^.TrmSort=ikError) or (fRight^.TrmSort=ikError) then
   begin NewEqFrm:=NewIncorFrm; exit end;
 NewEqFrm:=new(PredFrmPtr,Init(ikFrmPred,gBuiltIn[rqEqualsTo],NewTrmList(fLeft,NewTrmList(fRight,nil))));
end;

function NewInCorFrm: FrmPtr;
begin NewInCorFrm:=new(UniqFrmPtr,Init(ikError)) end;

function NewConj (Arg1,Arg2:FrmPtr ):FrmPtr;
 var lConjuncts:MCollection;
begin
 case Arg1^.FrmSort of
  ikError: begin NewConj:=NewInCorFrm; exit end;
  ikFrmVerum: begin NewConj:=Arg2; dispose(Arg1,Done); exit end;
  ikFrmConj:
   begin
    with ConjFrmPtr(Arg1)^.Conjuncts do
     case Arg2^.FrmSort of
      ikFrmConj:
       begin AppendTo(ConjFrmPtr(Arg2)^.Conjuncts);
        ConjFrmPtr(Arg2)^.Conjuncts.DeleteAll;
        dispose(Arg2,Done);
       end;
      ikFrmVerum: dispose(Arg2,Done);
      ikError: begin NewConj:=NewInCorFrm; exit end;
      else Insert(Arg2);
     end;
    NewConj:=Arg1; exit;
   end;
 end;
 case Arg2^.FrmSort of
  ikError: NewConj:=NewInCorFrm;
  ikFrmVerum: begin NewConj:=Arg1; dispose(Arg2,Done) end;
  ikFrmConj:
   begin NewConj:=Arg2;
    ConjFrmPtr(Arg2)^.Conjuncts.AtInsert(0,Arg1);
   end;
  else
   begin lConjuncts.Init(2,2);
    lConjuncts.Insert(Arg1); lConjuncts.Insert(Arg2);
    NewConj:=NewConjFrm(lConjuncts);
   end;
 end;
end;

function NewFlexConj(Arg1,Arg2:FrmPtr):FrmPtr;
begin
 NewFlexConj:=ProcessEllipses(Arg1,Arg2,true);
end;

function NewFlexDisj(fArg1,fArg2:FrmPtr):FrmPtr;
begin
 NewFlexDisj:=ProcessEllipses(fArg1,fArg2,false);
end;

function NewFlexFrm(fOrigFrm1,fOrigFrm2,fExpansion:FrmPtr; fLeftTrm,fRightTrm:TrmPtr):FrmPtr;
var lResult:FlexFrmPtr;
begin
 lResult:=new(FlexFrmPtr,Init(fOrigFrm1,fOrigFrm2,fLeftTrm,fRightTrm));
 lResult^.nExpansion:=fExpansion;
 lResult^.FrmSort:=ikFrmFlexConj;
 NewFlexFrm:=lResult;
end;

function NewExpansion(fTyp:TypPtr; fLeftGuardFrm,fRightGuardFrm,fFrm:FrmPtr):FrmPtr;
begin
 NewExpansion:=NewUniv(fTyp,NewNeg(NewConj(NewConj(fLeftGuardFrm,fRightGuardFrm),fFrm)));
end;

function FraenkelFrm(fTrm,fOpFr:TrmPtr): FrmPtr;
 var lFrm:FrmPtr; lTrm: TrmPtr;i:integer;
begin
 with FraenkelTrmPtr(fOpFr)^ do
  begin lTrm:=CopyTerm(fTrm);
   lFrm:=NewEqFrm(lTrm,CopyTerm(LambdaScope));
   lFrm:=NewNeg(NewConj(lFrm,Compr^.CopyFormula));
   for i:= LambdaArgs.Count-1 downto 0 do
    lFrm:=NewUniv(TypPtr(LambdaArgs.Items^[i])^.CopyType,lFrm);
  end;
  FraenkelFrm:=lFrm;
end;

// ##NOTE: This is a hack. A clean solution would be
// postponing of normalization of formulas to preparator.
// Formulas can disappear during simplification
// of double negations and merging of conjunctive formulas.
// The information kept in nPattNr is therefore not sufficient
// for complete reconstruction, however it should handle the worst
// cases. Additionally, we insert the information also at subformulas
// cretaed during expansion of logical connectives. So even if
// formula with pid_Or (which is usually NegFrmPtr) is simplified out,
// its child with pid_Or_And will often survive, and give us info,
// that this comes from a simplified OR formula.

const pid_Ex 		= -1; // usually NegFrmPtr
const pid_Ex_Univ 	= -2; // usually UnivFrmPtr
const pid_Ex_InnerNot 	= -3; // usually NegFrmPtr
const pid_Impl 		= -4; // usually NegFrmPtr
const pid_Impl_And 	= -5; // usually ConjFrmPtr       
const pid_Impl_RightNot = -6; // usually NegFrmPtr
const pid_Iff 		= -7; // usually ConjFrmPtr
const pid_Or 		= -8; // usually NegFrmPtr
const pid_Or_And 	= -9; // usually ConjFrmPtr
const pid_Or_LeftNot 	= -10; // usually NegFrmPtr
const pid_Or_RightNot 	= -11; // usually NegFrmPtr


function NewExis(fTyp:TypPtr; fFrm:FrmPtr):FrmPtr;
var lNeg1,lNeg2,lUniv:FrmPtr;
begin
 lNeg1:= NewNegDis(fFrm); lNeg1^.SetPidIfKind(ikFrmNeg,pid_Ex_InnerNot);
 lUniv:= NewUniv(fTyp,lNeg1); lUniv^.SetPidIfKind(ikFrmUniv,pid_Ex_Univ);
 lNeg2:= NewNeg(lUniv); lNeg2^.SetPidIfKind(ikFrmNeg,pid_Ex);
 NewExis:= lNeg2;
end;
// NewExis:=NewNeg(NewUniv(fTyp,NewNegDis(fFrm))) end;       

function NewImpl(fArg1,fArg2:FrmPtr):FrmPtr;
var lNeg1,lNeg2,lConj:FrmPtr;
begin
 lNeg1:= NewNegDis(fArg2); lNeg1^.SetPidIfKind(ikFrmNeg,pid_Impl_RightNot);
 lConj:= NewConj(fArg1,lNeg1); lConj^.SetPidIfKind(ikFrmConj,pid_Impl_And);
 lNeg2:= NewNegDis(lConj); lNeg2^.SetPidIfKind(ikFrmNeg,pid_Impl);
 NewImpl:= lNeg2;
end;
// begin NewImpl:=NewNegDis(NewConj(fArg1,NewNegDis(fArg2))) end;

function NewDisj(fArg1,fArg2:FrmPtr):FrmPtr;
var lNeg0,lNeg1,lNeg2,lConj:FrmPtr;
begin
 lNeg0:= NewNegDis(fArg1); lNeg0^.SetPidIfKind(ikFrmNeg,pid_Or_LeftNot);
 lNeg1:= NewNegDis(fArg2); lNeg1^.SetPidIfKind(ikFrmNeg,pid_Or_RightNot);
 lConj:= NewConj(lNeg0,lNeg1); lConj^.SetPidIfKind(ikFrmConj,pid_Or_And);
 lNeg2:= NewNegDis(lConj); lNeg2^.SetPidIfKind(ikFrmNeg,pid_Or);
 NewDisj:= lNeg2;
end;
//  NewDisj:=NewNegDis(NewConj(NewNegDis(fArg1),NewNegDis(fArg2))) end;

function NewBicond ( Arg1,Arg2:FrmPtr ):FrmPtr;
 var lFrm,lConj: FrmPtr;
begin lFrm:=Arg1^.CopyFormula;
 lFrm:=NewImpl(lFrm,Arg2^.CopyFormula);
 lConj:=NewConj(lFrm,NewImpl(Arg2,Arg1));
 lConj^.SetPidIfKind(ikFrmConj,pid_Iff);
 NewBicond:= lConj;
end;

function LastElem(fTrmList:TrmList):TrmList;
begin LastElem:=nil;
 if fTrmList = nil then exit;
 while fTrmList^.NextTrm <> nil do fTrmList:=fTrmList^.NextTrm;
 LastElem:=fTrmList;
end;

function LastArg(fTrmList:TrmList):TrmPtr;
begin mizassert(2513,fTrmList<>nil);
 LastArg:=LastElem(fTrmList)^.XTrmPtr;
end;

function NbrOfElem(fL:TrmList): integer;
 var e: integer;
begin e:=0;
 while fL<>nil do
  begin inc(e); fL:=fL^.NextTrm; end;
 NbrOfElem:=e;
end;

procedure GetArgs1(fFirst:integer; var fFirstTrm:TrmPtr; fList:TrmList);
 var i:integer;
begin i:=0;
 while fList<> nil do
  with fList^ do
   begin inc(i);
    if i = fFirst then begin fFirstTrm:=CopyTerm(XTrmPtr); exit; end;
    fList:=NextTrm;
   end;
 RunTimeError(3122);
end;

procedure GetArgs2(fFirst,fSecond:integer; var fFirstTrm,fSecondTrm:TrmPtr;
                  fList:TrmList);
 var i:integer;
begin i:=0;
 while fList<> nil do
  with fList^ do
   begin inc(i);
    if i = fFirst then fFirstTrm:=CopyTerm(XTrmPtr)
    else if i = fSecond then fSecondTrm:=CopyTerm(XTrmPtr);
    fList:=NextTrm;
   end;
end;

procedure GetBinArgs(aFrm:FrmPtr; var aLeft,aRight: TrmPtr);
 var lPred: integer; A: TrmList;
begin
 AdjustFrm(PredFrmPtr(aFrm),lPred,A);
 MizAssert(2581,A<>nil);
 aLeft:=A^.XTrmPtr;
 MizAssert(2582,A^.NextTrm <> nil);
 aRight:=A^.NextTrm^.XTrmPtr;
end;

function SwitchArgs(fFirst,fSecond:integer; fList:TrmList): TrmList;
 var lFirstTrm,lSecondTrm:TrmPtr;
     lTrmElem:TrmElem; lTrmList:TrmList;
     i:integer;
begin i:=0;
 mizassert(2345,NbrOfElem(flist)>=2);
 GetArgs2(fFirst,fSecond,lFirstTrm,lSecondTrm,fList);
 lTrmList:=addr(lTrmElem);
 while fList <> nil do
  with fList^ do
   begin inc(i);
    if i = fFirst then lTrmList^.NextTrm:=NewTrmList(lSecondTrm,nil) else
    if i = fSecond then lTrmList^.NextTrm:=NewTrmList(lFirstTrm,nil) else
      lTrmList^.NextTrm:=NewTrmList(CopyTerm(XTrmPtr),nil);
    lTrmList:=lTrmList^.NextTrm;
    fList:=NextTrm;
   end;
 SwitchArgs:=lTrmElem.NextTrm;
end;

function SwapArguments(fList:TrmList; fFirst,fSecond:word):TrmList;
 var lResult,lList1,lList2:Trmlist; lTrm:TrmPtr; c:integer;
begin
 lResult:=CopyTermList(fList);
 if fFirst > fSecond then
  begin c:=fSecond; fSecond:=fFirst; fFirst:=c end;
 lList1:=lResult; c:=0;
 while lList1 <> nil do
  with lList1^ do
   begin inc(c);
    if fFirst = c then break;
    lList1:=NextTrm;
   end;
 lList2:=lList1^.NextTrm;
 while lList2 <> nil do
  with lList2^ do
   begin inc(c);
    if fSecond = c then break;
    lList2:=NextTrm;
   end;
 mizassert(2578,lList2 <> nil );
 lTrm:=lList1^.XTrmPtr;
 lList1^.XTrmPtr:=lList2^.XTrmPtr;
 lList2^.XTrmPtr:=lTrm;
 SwapArguments:=lResult;
end;

procedure RemoveQua(fTL: TrmList);
 var lTrm:TrmPtr;
begin
 while fTL<>nil do
  with fTL^ do
   begin
    if XTrmPtr^.TrmSort=ikTrmQua then
     begin lTrm:=XTrmPtr;
      XTrmPtr:=CopyTerm(QuaTrmPtr(XTrmPtr)^.TrmProper);
      dispose(lTrm,Done);
     end;
    fTL:=NextTrm;
   end;
end;

constructor MAttrCollection.Init(ALimit, ADelta: Integer);
begin
  inherited Init(Alimit,ADelta);
  fConsistent:=true;
end;

procedure MAttrCollection.FreeItem(Item: Pointer);
begin
  if (Item <> nil) and not AttrPtr(Item)^.fCollected
   then Dispose(PObject(Item), Done);
end;

destructor MAttrCollection.Refuted;
begin Done; fConsistent:=false end;

function CompTrmLists(fTrmList1,fTrmList2:TrmList):integer;
 var lInt:integer;
 {! uwaga zaklada sie ze listy sa rowne !}
begin CompTrmLists:=0;
 while fTrmList1 <> nil do
  begin lInt:=CompTrms(fTrmList1^.XTrmPtr,fTrmList2^.XTrmPtr);
   if lInt <> 0 then begin CompTrmLists:=lInt; exit end;
   fTrmList1:=fTrmList1^.NextTrm; fTrmList2:=fTrmList2^.NextTrm;
  end;
end;

function CompAbsAttr(aAttr1, aAttr2: AttrPtr): Integer;
  var lInt,lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 aAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 aAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 lInt:=CompareInt(lAttrNr1,lAttrNr2);
 if lInt <> 0 then begin CompAbsAttr:=lInt; exit end;
 CompAbsAttr:=CompTrmLists(lArgs1,lArgs2);
end;

function CompAttr(aAttr1, aAttr2: pointer): Integer;
  var lInt: integer;
begin
 lInt:=CompAbsAttr(AttrPtr(aAttr1),AttrPtr(aAttr2));
 if lInt <> 0 then begin CompAttr:=lInt; exit end;
 CompAttr:=CompareInt(ord(AttrPtr(aAttr1)^.fNeg),ord(AttrPtr(aAttr2)^.fNeg));
end;

function ExtCompAttr(aAttr1, aAttr2: pointer): Integer;
  var lInt: integer;
begin
 lInt:=CompAttr(AttrPtr(aAttr1),AttrPtr(aAttr2));
 if lInt <> 0 then begin ExtCompAttr:=lInt; exit end;
 ExtCompAttr:=CompareInt(AttrPtr(aAttr1)^.nPattNr,AttrPtr(aAttr2)^.nPattNr);
end;

function CompClusters(aClu1,aClu2: AttrCollectionPtr): integer;
 var i,lInt: integer;
begin
  lInt:=CompareInt(aClu1^.Count,aClu2^.Count);
  if lInt <> 0 then begin CompClusters:=lInt; exit end;
  for i:=0 to aClu1^.Count-1 do
   begin
    lInt:=CompAttr(AttrPtr(AttrCollectionPtr(aClu1)^.Items^[i]),AttrPtr(AttrCollectionPtr(aClu2)^.Items^[i]));
    if lInt <> 0 then begin CompClusters:=lInt; exit end;
   end;
  CompClusters:=0;
end;

function CompTyps(fTyp1,fTyp2:TypPtr):integer; forward;

function CompTypColls(const fColl1,fColl2:MCollection):integer;
 var lint,k:integer;
begin CompTypColls:=0;
 lInt:=CompareInt(fColl1.Count,fColl2.Count);
 if lInt <> 0 then begin CompTypColls:=lInt; exit end;
 for k:=0 to fColl1.Count-1 do
  begin
   lInt:=CompTyps(fColl1.Items^[k],fColl2.Items^[k]);
   if lInt <> 0 then begin CompTypColls:=lInt; exit end;
  end;
end;

function CompRdTrms(fTrm1,fTrm2: pointer):integer;
begin
 gStrictCompare:=false;
 CompRdTrms:=CompTrms(fTrm1,fTrm2);
 gStrictCompare:=true;
end;

function CompTyps(fTyp1,fTyp2:TypPtr):integer;
var lInt:integer;
//    lModNr1,lModNr2:integer; lArgs1,lArgs2: TrmList;
begin CompTyps:=0;
 with fTyp1^ do
 begin
   lInt:=CompareInt(ord(TypSort),ord(fTyp2^.TypSort));
   if lInt <> 0 then begin CompTyps:=lInt; exit end;
   lInt:=CompareInt(ModNr,TypPtr(fTyp2)^.ModNr);
   if lInt <> 0 then begin CompTyps:=lInt; exit end;
   lInt:=CompClusters(LowerCluster,fTyp2^.LowerCluster);
   if lInt <> 0 then begin CompTyps:=lInt; exit end;
   CompTyps:=CompTrmLists(ModArgs,TypPtr(fTyp2)^.ModArgs);
{   case TypSort of
    ikTypMode:
     begin TypPtr(fTyp1)^.AdjustTyp(lModNr1,lArgs1);
       TypPtr(fTyp2)^.AdjustTyp(lModNr2,lArgs2);
      end;
    ikTypStruct:
     begin lModNr1:=ModNr; lArgs1:=ModArgs;
      lModNr2:=fTyp2^.ModNr; lArgs2:=fTyp2^.ModArgs;
     end;
    ikError: ;
    else RunTimeError(2048);
   end;
   lInt:=CompareInt(lModNr1,lModNr2);
   lInt:=CompClusters(LowerCluster,fTyp2^.LowerCluster);
   if lInt <> 0 then begin CompTyps:=lInt; exit end;
   CompTyps:=CompTrmLists(lArgs1,lArgs2);}
 end;
end;

function CompFrms(fFrm1,fFrm2:FrmPtr):integer;
 var lInt,k,lPredNr1,lPredNr2:integer; lArgs1,lArgs2: TrmList;
begin CompFrms:=0;
 lInt:=CompareInt(ord(fFrm1^.FrmSort),ord(fFrm2^.FrmSort));
 if lInt <> 0 then begin CompFrms:=lInt; exit end;
 case fFrm1^.FrmSort of
  ikFrmVerum,ikFrmThesis: ;
  ikFrmNeg: CompFrms:=CompFrms(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmQual:
    with QualFrmPtr(fFrm1)^ do
    begin
     lInt:=CompTrms(QualTrm,QualFrmPtr(fFrm2)^.QualTrm);
     if lInt <> 0 then begin CompFrms:=lInt; exit end;
     CompFrms:=CompTyps(QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
    end;
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^ do
    begin
     lInt:=CompareInt(Conjuncts.Count,ConjFrmPtr(fFrm2)^.Conjuncts.Count);
     if lInt <> 0 then begin CompFrms:=lInt; exit end;
     for k:=0 to Conjuncts.Count-1 do
      begin
       lInt:=CompFrms(Conjuncts.Items^[k],ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k]);
       if lInt <> 0 then begin CompFrms:=lInt; exit end;
      end;
    end;
   ikFrmSchPred,ikFrmPrivPred:
    with PredFrmPtr(fFrm1)^ do
    begin
     lInt:=CompareInt(PredNr,PredFrmPtr(fFrm2)^.PredNr);
     if lInt <> 0 then begin CompFrms:=lInt; exit end;
     CompFrms:=CompTrmLists(PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
    end;
   ikFrmAttr:
    if gStrictCompare then
     with PredFrmPtr(fFrm1)^ do
      begin
       lInt:=CompareInt(PredNr,PredFrmPtr(fFrm2)^.PredNr);
       if lInt <> 0 then begin CompFrms:=lInt; exit end;
       CompFrms:=CompTrmLists(PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
      end
     else
      begin
       AdjustAttrFrm(PredFrmPtr(fFrm1),lPredNr1,lArgs1);
       AdjustAttrFrm(PredFrmPtr(fFrm2),lPredNr2,lArgs2);
       lInt:=CompareInt(lPredNr1,lPredNr2);
       if lInt <> 0 then begin CompFrms:=lInt; exit end;
       CompFrms:=CompTrmLists(lArgs1,lArgs2);
      end;
   ikFrmPred:
    if gStrictCompare then
     with PredFrmPtr(fFrm1)^ do
      begin
       lInt:=CompareInt(PredNr,PredFrmPtr(fFrm2)^.PredNr);
       if lInt <> 0 then begin CompFrms:=lInt; exit end;
       CompFrms:=CompTrmLists(PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
      end
     else
      begin AdjustFrm(PredFrmPtr(fFrm1),lPredNr1,lArgs1);
       AdjustFrm(PredFrmPtr(fFrm2),lPredNr2,lArgs2);
       lInt:=CompareInt(lPredNr1,lPredNr2);
       if lInt <> 0 then begin CompFrms:=lInt; exit end;
  {     if lPredNr1=gBuiltIn[rqEqualsTo] then
         begin
          Left1:=lArgs1^.XTrmPtr;
          Right1:=lArgs1^.NextTrm^.XTrmPtr;
          Left2:=lArgs2^.XTrmPtr;
          Right2:=lArgs2^.NextTrm^.XTrmPtr;
          CompFrms:=CompTrms(Left1,Left2) and CompTrms(Right1,Right2) or
                  CompTrms(Right1,Left2) and CompTrms(Left1,Right2);
         end
        else}
        CompFrms:=CompTrmLists(lArgs1,lArgs2);
      end;
   ikFrmUniv:
    with UnivFrmPtr(fFrm1)^ do
    begin
      lInt:=CompTyps(Quantified,UnivFrmPtr(fFrm2)^.Quantified);
      if lInt <> 0 then begin CompFrms:=lInt; exit end;
      CompFrms:=CompFrms(Scope,UnivFrmPtr(fFrm2)^.Scope);
    end;
   ikError: ;
   else RunTimeError(2049);
  end;
end;

function CompTrms(fTrm1,fTrm2: pointer):integer;
 var lInt,lFuncNr1,lFuncNr2:integer; lArgs1,lArgs2: TrmList;
begin CompTrms:=0;
 if not gStrictCompare then
 begin
  while TrmPtr(fTrm1)^.TrmSort=ikTrmPrivFunc do
   if LocFuncTrmPtr(fTrm1)^.FuncExp^.TrmSort=ikError then break
   else fTrm1:=LocFuncTrmPtr(fTrm1)^.FuncExp;
  while TrmPtr(fTrm2)^.TrmSort=ikTrmPrivFunc do
   if LocFuncTrmPtr(fTrm2)^.FuncExp^.TrmSort=ikError then break
   else fTrm2:=LocFuncTrmPtr(fTrm2)^.FuncExp;
 end;
 with TrmPtr(fTrm1)^ do
  begin
   lInt:=CompareInt(ord(TrmSort),ord(TrmPtr(fTrm2)^.TrmSort));
   if lInt <> 0 then begin CompTrms:=lInt; exit end;
   case TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral,ikTrmEqConst:
    with VarTrmPtr(fTrm1)^ do
     begin
      CompTrms:=CompareInt(VarNr,VarTrmPtr(fTrm2)^.VarNr);
     end;
   ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmSelector:
    with FuncTrmPtr(fTrm1)^ do
     begin
      lInt:=CompareInt(FuncNr,FuncTrmPtr(fTrm2)^.FuncNr);
      if lInt <> 0 then begin CompTrms:=lInt; exit end;
      CompTrms:=CompTrmLists(FuncArgs,FuncTrmPtr(fTrm2)^.FuncArgs);
     end;
   ikTrmFunctor:
    with FuncTrmPtr(fTrm1)^ do
    if gStrictCompare then
    with FuncTrmPtr(fTrm1)^ do
     begin
      lInt:=CompareInt(FuncNr,FuncTrmPtr(fTrm2)^.FuncNr);
      if lInt <> 0 then begin CompTrms:=lInt; exit end;
      CompTrms:=CompTrmLists(FuncArgs,FuncTrmPtr(fTrm2)^.FuncArgs);
     end
   else
     begin AdjustTrm(fTrm1,lFuncNr1,lArgs1); AdjustTrm(fTrm2,lFuncNr2,lArgs2);
      lInt:=CompareInt(lFuncNr1,lFuncNr2);
      if lInt <> 0 then begin CompTrms:=lInt; exit end;
      CompTrms:=CompTrmLists(lArgs1,lArgs2);
     end;
   ikTrmFraenkel:
     with FraenkelTrmPtr(fTrm1)^  do
      begin
       lInt:=CompTypColls(LambdaArgs,FraenkelTrmPtr(fTrm2)^.LambdaArgs);
       if lInt <> 0 then begin CompTrms:=lInt; exit end;
       lInt:=CompTrms(LambdaScope,FraenkelTrmPtr(fTrm2)^.LambdaScope);
       if lInt <> 0 then begin CompTrms:=lInt; exit end;
       CompTrms:=CompFrms(Compr,FraenkelTrmPtr(fTrm2)^.Compr);
      end;
   ikTrmChoice:
     with ChoiceTrmPtr(fTrm1)^  do
      CompTrms:=CompTyps(ChoiceTyp,ChoiceTrmPtr(fTrm2)^.ChoiceTyp);
   ikTrmQua:
     with QuaTrmPtr(fTrm1)^ do
      begin
       lInt:=CompTrms(TrmProper,QuaTrmPtr(fTrm2)^.TrmProper);
       if lInt <> 0 then begin CompTrms:=lInt; exit end;
       CompTrms:=CompTyps(Qua,QuaTrmPtr(fTrm2)^.Qua);
      end;
   ikTrmIt: ;
    else
     begin
{$IFDEF MDEBUG}
writeln(InfoFile,'CompTrms:TrmSort=',TrmSort,'|');
{$ENDIF}
       RunTimeError(2351);
     end;
   end;
  end;
end;

function MAttrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Compare:=CompAbsAttr(Key1,Key2);
end;

procedure MAttrCollection.AtInsert(Index: Integer; Item: Pointer);
 var lNr: integer;
begin
 if gAttrCollected and (AttrPtr(Item)^.fAttrArgs = nil) then
  begin
    lNr:=2*AttrPtr(Item)^.fAttrNr+AttrPtr(Item)^.fNeg;
    if (lNr >= gAttrCollection.Count) or
       (gAttrCollection.Items^[lNr] = nil) then
     begin
      AttrPtr(Item)^.fCollected:=true;
      gAttrCollection.AtInsert(lNr,Item);
     end
    else
     begin
      if not AttrPtr(Item)^.fCollected then
        dispose(AttrPtr(Item),Done);
      Item:=gAttrCollection.Items^[lNr];
     end;
  end;
 inherited AtInsert(Index,Item);
end;

procedure MAttrCollection.InsertAttr(aAttrNr:integer; aNeg:byte; aArgs: TrmList);
 var lIndex: integer; lAttrPtr: AttrPtr;
begin
 if not fConsistent then
  begin
   DisposeTrmList(aArgs);
   exit
  end;
 lAttrPtr:=new(AttrPtr,Init(aNeg,aAttrNr,aArgs));
 if Search(lAttrPtr,lIndex) then
  begin
   if AttrPtr(Items^[lIndex])^.fNeg<>aNeg then
    Refuted;
   dispose(lAttrPtr,Done);
   exit;
  end;
 AtInsert(lIndex,lAttrPtr);
end;

procedure MAttrCollection.Insert(aItem: Pointer);
 var lIndex: integer;
begin
 if not fConsistent then
  begin
   dispose(AttrPtr(aItem),Done);
   exit
  end;
 if Search(aItem,lIndex) then
  begin
   if AttrPtr(Items^[lIndex])^.fNeg<>AttrPtr(aItem)^.fNeg then
    Refuted;
   dispose(AttrPtr(aItem),Done);
   exit;
  end;
 AtInsert(lIndex,aItem);
end;

function AttrEquals(Key1, Key2: AttrPtr): boolean;
  var lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 Key1^.AdjustAttr(lAttrNr1,lArgs1);
 Key2^.AdjustAttr(lAttrNr2,lArgs2);
 AttrEquals := (lAttrNr1 = lAttrNr2) and (Key1^.fNeg = Key2^.fNeg);
end;

function MAttrCollection.GetAttr(aAttrNr: integer; aAttrArgs: TrmList): AttrPtr;
 var lIndex: integer; lAttrPtr: AttrPtr;
begin GetAttr:=nil;
  lAttrPtr:=new(AttrPtr,init(0,aAttrNr,aAttrArgs));
  if Search(lAttrPtr,lIndex) then
    GetAttr:=Items^[lIndex];
  dispose(lAttrPtr,Done);
end;

constructor MAttrCollection.CopyAll(aOrigin:AttrCollectionPtr);
 var i: integer;
begin
 Init(aOrigin^.Count,aOrigin^.Delta);
 mizassert(4000,aOrigin^.Count >= 0);
 for i:=0 to aOrigin^.Count-1 do
  AtInsert(i,AttrPtr(aOrigin^.Items^[i])^.CopyAttribute);
 fConsistent:=aOrigin^.fConsistent;
end;


procedure MAttrCollection.ClearPids;
 var i: integer; lOrig,lNew:AttrPtr;
begin
 for i:=0 to Count-1 do
 begin
  lOrig:= AttrPtr(Items^[i]);
  if lOrig^.nPattNr > 0 then
  begin
   // need a new copy, since we may be collecting attributes
   lNew:= new(AttrPtr,Init(lOrig^.fNeg,lOrig^.fAttrNr,
                           CopyTermList(lOrig^.fAttrArgs)));
   if not lOrig^.fCollected then dispose(lOrig,Done);
   Items^[i]:= lNew;
  end;
 end;
end;


constructor MAttrCollection.CopyAllowed(aTyp:TypPtr; aOrigin:AttrCollectionPtr);
  var lModNr,lAttrNr,i:integer; A:TrmList;
begin
 Init(aOrigin^.Count,aOrigin^.Delta);
 with aTyp^ do
  begin mizassert(2599,TypSort=ikTypMode); AdjustTyp(lModNr,A) end;
 for i:=0 to aOrigin^.Count-1 do
 begin
  lAttrNr:= AttrPtr(aOrigin^.Items^[i])^.AdjustedAttrNr;
  with ConstrTypPtr( Constr[ coAttribute].Items^[ lAttrNr])^.fConstrTyp^ do
   case TypSort of
    ikTypMode:
     if lModNr <> ModNr
     then Insert(AttrPtr(aOrigin^.Items^[i])^.CopyAttribute);
    ikTypStruct: Insert(AttrPtr(aOrigin^.Items^[i])^.CopyAttribute);
   end;
  end;
end;

procedure MAttrCollection.EnlargeBy(aAnother:MListPtr);
 var i,j,lCount,lLimit:integer;
     lItems:PItemList;
     lAttrPtr: AttrPtr;
begin
 if fConsistent then
  begin
   if not AttrCollectionPtr(aAnother)^.fConsistent
    then begin Refuted; exit end;
   if aAnother^.Count = 0
    then exit;
   lCount:=Count; lItems:=Items; lLimit:=Limit;
   Limit:=0;
   Count:=0; SetLimit(lCount+aAnother^.Count);
   i:=0;
   j:=0;
   while i < lCount do
   begin
    lAttrPtr:=AttrPtr(aAnother^.Items^[j])^.CopyAttribute;
    case CompAbsAttr(lItems^[i],lAttrPtr) of
     -1:
      begin
       AtInsert(Count,lItems^[i]); inc(i);
       dispose(lAttrPtr,Done);
       continue
      end;
     0:
      begin
       dispose(lAttrPtr,Done);
       if AttrPtr(aAnother^.Items^[j])^.fNeg <> AttrPtr(lItems^[i])^.fNeg then
        begin Refuted;
         FreeMem(lItems,lLimit*SizeOf(AttrPtr));
         exit
        end;
       AtInsert(Count,lItems^[i]); inc(i);
      end;
     1: AtInsert(Count,lAttrPtr);
    end;
    inc(j);
    if j>= aAnother^.Count then break;
   end;
   if i >= lCount then
    begin
     for j:=j to aAnother^.Count-1 do
      AtInsert(Count,AttrPtr(aAnother^.Items^[j])^.CopyAttribute)
    end
   else
    for i:=i to lCount-1 do AtInsert(Count,lItems^[i]);
   SetLimit(0);
   FreeMem(lItems,lLimit*SizeOf(AttrPtr));
  end;
end;

//AN!
{
function MAttrCollection.IsSubsetOf(aClu: AttrCollectionPtr; aEqAttr: EqualAttrs): boolean;
 var i,j,lAttrNr: integer;
begin IsSubsetOf:=false;
 if aClu^.Count < Count then exit;
 j:=0;
 for i:=0 to Count-1 do
  begin lAttrNr:=AttrPtr(Items^[i])^.AdjustedAttrNr;
   while (j < aClu^.Count) and (AttrPtr(aClu^.Items^[j])^.AdjustedAttrNr < lAttrNr) do inc(j);
   if (j = aClu^.Count) or not aEqAttr(aClu^.Items^[j],Items^[i]) then exit;
  end;
 IsSubsetOf:=true;
end;
}
function MAttrCollection.IsSubsetOf(aClu: AttrCollectionPtr; aEqAttr: EqualAttrs): boolean;
 var i,j,c: integer;
     lSubstTrm: LociSubstitution;
begin IsSubsetOf:=false;
 if aClu^.Count < Count then exit;
 c:=0;
 for i:=0 to Count-1 do
   for j:=0 to aClu^.Count -1 do
    begin lSubstTrm:=gSubstTrm;
     if aEqAttr(aClu^.Items^[j],Items^[i]) then
      begin inc(c); break; end
     else gSubstTrm:=lSubstTrm;
    end;
 if c=count then IsSubsetOf:=true;
end;

function MAttrCollection.IsEqualTo(aClu: AttrCollectionPtr; aEqAttr: EqualAttrs): boolean;
 var I: integer;
begin IsEqualTo:=false;
 if Count <> aClu^.Count then exit;
 for I:=0 to Count-1 do
  if not aEqAttr(Items^[I],aClu^.Items^[I]) then exit;
 IsEqualTo:=true;
end;

function CompEsTyp(fTyp,aTyp:TypPtr; fExactly:boolean): boolean;
 var aModNr: integer; A: TrmList;
begin CompEsTyp:=false;
 if fTyp^.TypSort<>aTyp^.TypSort then exit;
 if fTyp^.ModNr=aTyp^.ModNr then
  begin
   CompEsTyp:=EsTrmList(fTyp^.ModArgs,aTyp^.ModArgs);
   exit
  end;
 if (fTyp^.TypSort=ikTypMode) and not fExactly then
  begin aTyp^.AdjustTyp(aModNr,A);
   if fTyp^.ModNr=aModNr then
    CompEsTyp:=EsTrmList(fTyp^.ModArgs,A);
  end;
end;

/// procedura podobna do CheckLociTypes.
/// Glowna rozniaca, to fakt ze nie wszystkie pola w tablicy gSubstTrm
/// sa wypwlnione wykorzytywna w Identify, gdzie moze sie to zdarzyc.
function CheckLociTypes(const fList:MList):boolean;
 var wTyp:TypPtr;
     lSubstTrm: array[0..2*MaxArgNbr-1] of LociSubstitution;
     lSubstTyp: array[0..2*MaxArgNbr-1] of TypPtr;
     lLocusNr,llLocusNr,i:integer;
 label Again,BackTrack,koniec;
begin
 CheckLociTypes:=true;
 fillchar(lSubstTyp,SizeOf(lSubstTyp),0);
 { lLocusNr jest liczony od 0 }
 lLocusNr:=fList.Count;
 while (lLocusNr <> 0) and (gSubstTrm[lLocusNr]=nil) do dec(lLocusNr);
 with fList do
  while lLocusNr <> 0 do
   begin
    mizassert(2558,gSubstTrm[lLocusNr]<>nil);
    lSubstTrm[lLocusNr-1]:=gSubstTrm;
    if itisChecker then wTyp:=RoundUpTrmType(gSubstTrm[lLocusNr])
     else wTyp:=GetTrmType(gSubstTrm[lLocusNr]);
    dec(lLocusNr);
    gSubstTrm:=lSubstTrm[lLocusNr];
    if not wTyp^.DecreasingAttrs(TypPtr(Items^[lLocusNr]),EsAttrRev{Equals}) then
     begin dispose(wTyp,Done); goto BackTrack end;
Again:
    wTyp:=TypPtr(Items^[lLocusNr])^.WideningOf(wTyp);
    if wTyp <> nil then
     begin
      if lSubstTyp[lLocusNr]<> nil then dispose(lSubstTyp[lLocusNr],Done);
      lSubstTyp[lLocusNr]:=wTyp;
      if CompEsTyp(TypPtr(Items^[lLocusNr]),wTyp,false) then
       begin
        while (lLocusNr <> 0) and (gSubstTrm[lLocusNr]=nil) do dec(lLocusNr);
        continue;
       end;
      for i:=1 to lLocusNr do
       if gSubstTrm[i] <> lSubstTrm[lLocusNr,i] then
        begin DisposeTrm(gSubstTrm[i]); gSubstTrm[i]:=nil end;
     end
    else
BackTrack:
     repeat
      if lSubstTyp[lLocusNr] <> nil then dispose(lSubstTyp[lLocusNr],Done);
      inc(lLocusNr);
      llLocusNr:=lLocusNr;
      repeat
       inc(llLocusNr);
      until (llLocusNr >= Count) or (gSubstTrm[llLocusNr]<>nil);
      if llLocusNr >= Count then
       begin
        DisposeSubstTrm;
        CheckLociTypes:=false;
        exit
       end;
      lLocusNr:=llLocusNr-1;
     until
      ((lSubstTyp[lLocusNr]^.TypSort = ikTypMode) or
       (lSubstTyp[lLocusNr]^.ModNr <> 1)) and
      (TypPtr(Items^[lLocusNr])^.TypSort = ikTypMode) and
      (OriginalNr( coMode,TypPtr(Items^[lLocusNr])^.ModNr) = 0);
    wTyp:=lSubstTyp[lLocusNr]^.Widening;
    dispose(lSubstTyp[lLocusNr],Done); lSubstTyp[lLocusNr]:=nil;
    if wTyp = nil then goto BackTrack;
    for i:=1 to lLocusNr do
     if gSubstTrm[i] <> lSubstTrm[lLocusNr,i] then
      begin DisposeTrm(gSubstTrm[i]); gSubstTrm[i]:=nil end;
    goto Again;
   end;

koniec:

 for i:=0 to fList.Count-1 do
  if lSubstTyp[i]<>nil then dispose(lSubstTyp[i],Done);
end;

function CheckLociTypesN(const fList:MList):boolean;
 var wTyp:TypPtr;
     lSubstTrm: array[0..2*MaxArgNbr-1] of LociSubstitution;
     lSubstTyp: array[0..2*MaxArgNbr-1] of TypPtr;
     lLocusNr,i:integer;
 label Again,BackTrack;
begin CheckLociTypesN:=true;
 fillchar(lSubstTyp,SizeOf(lSubstTyp),0);
 { lLocusNr jest liczony od 0 }
 lLocusNr:=fList.Count;
 with fList do
  while lLocusNr <> 0 do
   begin
    mizassert(2552,gSubstTrm[lLocusNr]<>nil);
    lSubstTrm[lLocusNr-1]:=gSubstTrm;
    if itisChecker then wTyp:=RoundUpTrmType(gSubstTrm[lLocusNr])
     else wTyp:=GetTrmType(gSubstTrm[lLocusNr]);
    dec(lLocusNr);
    gSubstTrm:=lSubstTrm[lLocusNr];
    if not wTyp^.DecreasingAttrs(TypPtr(Items^[lLocusNr]),EsAttrREv{Equals}) then
     begin dispose(wTyp,Done); goto BackTrack end;
    lSubstTrm[lLocusNr]:=gSubstTrm;
Again:
    wTyp:=TypPtr(Items^[lLocusNr])^.WideningOf(wTyp);
    if wTyp <> nil then
     begin
      if lSubstTyp[lLocusNr]<> nil then dispose(lSubstTyp[lLocusNr],Done);
      lSubstTyp[lLocusNr]:=wTyp;
      if CompEsTyp(TypPtr(Items^[lLocusNr]),wTyp,false) then continue;
      for i:=1 to lLocusNr do
       if gSubstTrm[i] <> lSubstTrm[lLocusNr,i] then
        begin DisposeTrm(gSubstTrm[i]); gSubstTrm[i]:=nil end;
     end
    else
BackTrack:
     repeat
      if lSubstTyp[lLocusNr] <> nil then dispose(lSubstTyp[lLocusNr],Done);
      inc(lLocusNr);
      if lLocusNr = Count then
       begin
        DisposeSubstTrm;
        CheckLociTypesN:=false;
        exit
       end;
     until
      ((lSubstTyp[lLocusNr]^.TypSort = ikTypMode) or
       (lSubstTyp[lLocusNr]^.ModNr <> gBuiltIn[rqAny])) and
      (TypPtr(Items^[lLocusNr])^.TypSort = ikTypMode) and
      (OriginalNr( coMode,TypPtr(Items^[lLocusNr])^.ModNr) = 0);
    wTyp:=lSubstTyp[lLocusNr]^.Widening;
    dispose(lSubstTyp[lLocusNr],Done); lSubstTyp[lLocusNr]:=nil;
    if wTyp = nil then goto BackTrack;
    for i:=1 to lLocusNr do
     if gSubstTrm[i] <> lSubstTrm[lLocusNr,i] then
      begin DisposeTrm(gSubstTrm[i]); gSubstTrm[i]:=nil end;
    goto Again;
   end;
{ Tablice gSubTrm trzeba zostawic wypelniana, a przynajmniej sa
  2 warianty: w niektorych przypadkach tylko sprawdzenie, a w niektorych
  trzeba ja wykorzystac do podstawiania.
  Trzeba jednak zlikwidowac lSubstTyp, ktory powinien byc caly
  wypelniony.
}
 for i:=0 to fList.Count-1 do dispose(lSubstTyp[i],Done);
{ Nie lepiej byloby z lSubstTyp zrobic kolekcje ? }
end;

{ Z drugiej strony jezeli lokusy i tak sa sprawdzane, to nie ma problemu,
  bo typ jest aktualizowalny. Logicznie wiec to bledne nie jest. Co jest
  natomiast beldne, to to, ze nie daje sie odtworzyc (jezeli atrybuty
  wystepuja przy takim typie, ich listy argumentow.
  !!!!!!!!!!!!
  Trzeba wiec rozroznic 2 rzeczy:
  - poprawnosc logiczna: wystarczy, zeby typ dawal sie zaktualizowac,
    wtedy mozna by wariant typu zmienic na zaktualizowany
  - poprawnosc techniczna: jezeli nie jest zaktualizowany, to wtedy
    nie ma moznosci zrekonstruowania listy argumentow
}

function CheckArgType(const aPrim: MList; fk:integer):boolean;
 var sTyp,lTyp,wTyp:TypPtr;
     lSubstTrm: LociSubstitution;
     lArgNr:integer;
     lClusterPtr:AttrCollectionPtr;
 label 22,23;
begin CheckArgType:=true;
 lTyp:=aPrim.Items^[fk-1]; lSubstTrm:=gSubstTrm;
 mizassert(2536,lSubstTrm[fk]<>nil);
{----}
 wTyp:=GetTrmType(lSubstTrm[fk]);
{----}
 lClusterPtr:=CopyCluster(wTyp^.UpperCluster);
 lClusterPtr^.RoundUpWith(wTyp);
 dispose(wTyp^.UpperCluster,Done);
 wTyp^.UpperCluster:=lClusterPtr;
 gSubstTrm:=lSubstTrm;
 if not wTyp^.DecreasingAttrs(lTyp,EsAttrRev) then
  begin CheckArgType:=false; goto 22 end;
 lSubstTrm:=gSubstTrm;
 if not gExactly[fk] then
  begin
   wTyp:=lTyp^.WideningOf(wTyp);
   if wTyp=nil then
    begin CheckArgType:=false; exit end;
  end;
 if CompEsTyp(lTyp,wTyp,gExactly[fk]) then
  begin if fk=1 then goto 22;
   if CheckArgType(aPrim,fk-1) then goto 22;
  end;
 for lArgNr:=1 to fk-1 do
  if gSubstTrm[lArgNr] <> lSubstTrm[lArgNr] then
   begin DisposeTrm(gSubstTrm[lArgNr]); gSubstTrm[lArgNr]:=nil end;
 if gExactly[fk] then goto 23;
 if (lTyp^.TypSort<>ikTypMode) or (OriginalNr( coMode,lTyp^.ModNr)<>0) then goto 23;
 repeat
  if (wTyp^.TypSort=ikTypMode) and (wTyp^.ModNr=gBuiltIn[rqAny]) then goto 23;
  sTyp:=wTyp^.Widening;
  dispose(wTyp,Done); wTyp:=sTyp;
  wTyp:=lTyp^.WideningOf(wTyp);
  if wTyp = nil then begin CheckArgType:=false; exit end;
  if CompEsTyp(lTyp,wTyp,false) then
   begin if fk=1 then goto 22; if CheckArgType(aPrim,fk-1) then goto 22 end;
  for lArgNr:=1 to fk-1 do
   if gSubstTrm[lArgNr] <> lSubstTrm[lArgNr] then
    begin DisposeTrm(gSubstTrm[lArgNr]); gSubstTrm[lArgNr]:=nil end;
 until false;
23: CheckArgType:=false;
22: dispose(wTyp,Done);
end;

function CheckTypes(aPattern:PatternPtr; aTrmList:TrmList): boolean;
 var k:integer;
begin fillchar(gSubstTrm,sizeof(gSubstTrm),0);
 fillchar(gExactly,sizeof(gExactly),0);
 CheckTypes:=aTrmList=nil;
 if aPattern^.fPrimTypes.Count > 0 then
  begin gTrmList:=aTrmList;
    for k:=0 to aPattern^.Visible.fCount-1 do
     InsertArgument(aPattern^.Visible.fList^[k]);
   Mizassert(2538,gTrmList=nil);
   CheckTypes:=CheckArgType(aPattern^.fPrimTypes,aPattern^.fPrimTypes.Count);
  end;
end;

function Agree(fTrmList:TrmList; const fTypList:MList):boolean;
 var i:integer;
begin
 fillchar(gSubstTrm,sizeof(gSubstTrm),0);
 fillchar(gExactly,sizeof(gExactly),0);
 Agree:=fTrmList=nil;
 if fTypList.Count > 0 then
  begin gTrmList:=fTrmList;
   for i:=0 to fTypList.Count-1 do
     InsertArgument(i+1);
   Mizassert(2559,gTrmList=nil);
   Agree:=CheckArgType(fTypList,fTypList.Count);
  end;
end;

procedure MAttrCollection.RoundUpWith(aTyp:TypPtr);
begin ConditionalCluster.RoundUpCluster( @Self, aTyp); end;

procedure ChangeBound(var fTrm: TrmPtr);
 begin
  with VarTrmPtr(fTrm)^ do
   if TrmSort=ikTrmBound then inc(VarNr,BoundVarNbr);
 end;

procedure ChChangeBound(var fTrm: TrmPtr);
begin
 if (fTrm^.TrmSort=ikTrmBound) and (VarTrmPtr(fTrm)^.VarNr>BoundVarNbr) then
  dec(VarTrmPtr(fTrm)^.VarNr,BoundVarNbr);
end;

var Instantiation: array[1..MaxInstTrmNbr] of TrmPtr;
     Instantiated: boolean = false;

function CopyTermList ( fTL:TrmList ):TrmList;
 var lTrmElem:TrmElem; Sentinel:TrmList;
begin Sentinel:=addr(lTrmElem);
 while fTL <> nil do
  begin new(Sentinel^.NextTrm); Sentinel:=Sentinel^.NextTrm;
   Sentinel^.XTrmPtr:=CopyTerm(fTL^.XTrmPtr); fTl:=fTL^.NextTrm;
  end;
 Sentinel^.NextTrm:=nil;
 CopyTermList:=lTrmElem.NextTrm;
end;

function CopyTermList1 ( fTL:TrmList ):TrmList;
 var lTrmElem:TrmElem; Sentinel:TrmList;
begin CopyTermList1:=nil;
 if fTL = nil then exit;
 Sentinel:=addr(lTrmElem);
 while fTL^.NextTrm <> nil do
  begin new(Sentinel^.NextTrm); Sentinel:=Sentinel^.NextTrm;
   Sentinel^.XTrmPtr:=CopyTerm(fTL^.XTrmPtr); fTl:=fTL^.NextTrm;
  end;
 Sentinel^.NextTrm:=nil;
 CopyTermList1:=lTrmElem.NextTrm; {###}
end;

function AttrObj.CopyAttribute: AttrPtr;
begin
 CopyAttribute:=new(AttrPtr,InitP(fNeg,fAttrNr,CopyTermList(fAttrArgs),nPattNr));
end;

function CopyCluster(aClu: AttrCollectionPtr): AttrCollectionPtr;
begin
 CopyCluster:=new(AttrCollectionPtr,CopyAll(aClu));
end;

function TypObj.CopyType: TypPtr;
begin
 CopyType:=
  new(TypPtr,InitP(TypSort,CopyCluster(LowerCluster),CopyCluster(UpperCluster),
                   ModNr,CopyTermList(ModArgs),nPattNr));
end;

procedure CopyTypeColl(const aSrc: MList; var aTrg: MList);
 var z: integer;
begin
 aTrg.Init(aSrc.Count);
 for z:=0 to aSrc.Count-1 do
  begin aTrg.Insert(TypPtr(aSrc.Items^[z])^.CopyType) end;
end;

function FrmObj.CopyFormula: FrmPtr;
begin Abstract1; CopyFormula:=nil;  end;

procedure FrmObj.SetPidIfKind( aKind:char; aPattNr:integer);
begin
 if (FrmSort = aKind) and (nPattNr = 0) then nPattNr:= aPattNr;
end;

function QualFrmObj.CopyFormula: FrmPtr;
 var lTrm: TrmPtr;
begin lTrm:=CopyTerm(QualTrm);
 CopyFormula:=new(QualFrmPtr,InitP(lTrm,QualTyp^.CopyType,nPattNr));
end;

function UnivFrmObj.CopyFormula: FrmPtr;
 var lQuantified: TypPtr;
     lFrm: UnivFrmPtr;
begin
 inc(BoundVarNbr);
 lQuantified:=Quantified^.CopyType;
 lFrm:=new(UnivFrmPtr,InitP(lQuantified,Scope^.CopyFormula,nPattNr));
 lFrm^.nVarId:=nVarId;
 CopyFormula:=lFrm;
 dec(BoundVarNbr);
end;

function ConjFrmObj.CopyFormula: FrmPtr;
 var lConjuncts:MCollection; z: integer;
begin
 lConjuncts.Init(Conjuncts.Count,Conjuncts.Delta);
 with Conjuncts do for z:=0 to Count-1 do
  lConjuncts.Insert(FrmPtr(Items^[z])^.CopyFormula);
 CopyFormula:=new(ConjFrmPtr,InitP(lConjuncts,nPattNr));
end;

function BinFrmObj.CopyFormula: FrmPtr;
begin
 CopyFormula:=new(BinFrmPtr,Init(FrmSort,nLeftArg^.CopyFormula,nRightArg^.CopyFormula));
end;

function NegFrmObj.CopyFormula: FrmPtr;
begin CopyFormula:=new(NegFrmPtr,InitP(NegArg^.CopyFormula,nPattNr)) end;

function PredFrmObj.CopyFormula: FrmPtr;
begin CopyFormula:=new(PredFrmPtr,InitP(FrmSort,PredNr,
                                        CopyTermList(PredArgs),nPattNr)) end;

function LocPredFrmObj.CopyFormula: FrmPtr;
begin
 CopyFormula:=new(LocPredFrmPtr,InitP(PredNr,CopyTermList(PredArgs),
                                      PredExp^.CopyFormula,nPattNr))
end;

function UniqFrmObj.CopyFormula: FrmPtr;
begin CopyFormula:=new(UniqFrmPtr,InitP(FrmSort,nPattNr)) end;

function AdjustedType ( fTyp:TypPtr ):TypPtr;
 var lModNr:integer; lModArgs,A: TrmList;
begin
 with fTyp^ do
  begin
   case TypSort of
    ikTypMode: begin fTyp^.AdjustTyp(lModNr,A); lModArgs:=CopyTermList(A) end;
    ikTypStruct: begin lModNr:=ModNr; lModArgs:=CopyTermList(ModArgs) end;
    ikError: begin lModNr:=0; lModArgs:=nil end;
    else
begin
{$IFDEF MDEBUG}
writeln(InfoFile,TypSort,'|');
{$ENDIF}
     RunTimeError(2133);
end;
   end;
//   AdjustedType:=new(TypPtr,InitP(TypSort,CopyCluster(LowerCluster),CopyCluster(UpperCluster),
//		     lModNr,lModArgs,fTyp.nPattNr));
   AdjustedType:=new(TypPtr,Init(TypSort,CopyCluster(LowerCluster),CopyCluster(UpperCluster),
		     lModNr,lModArgs));
  end;
end;

function CopyExpTrmList ( fTL:TrmList ):TrmList;
 var lTrmElem:TrmElem; Sentinel:TrmList;
begin Sentinel:=addr(lTrmElem);
 while fTL <> nil do
  begin new(Sentinel^.NextTrm); Sentinel:=Sentinel^.NextTrm;
   Sentinel^.XTrmPtr:=CopyExpTrm(fTL^.XTrmPtr); fTl:=fTL^.NextTrm;
  end;
 Sentinel^.NextTrm:=nil;
 CopyExpTrmList:=lTrmElem.NextTrm;
end;

function CopyExpCluster(aClu: AttrCollectionPtr): AttrCollectionPtr;
 var i: integer; lCluster:AttrCollectionPtr;
begin
 lCluster:=new(AttrCollectionPtr,Init(aClu^.Count,aClu^.Delta));
 with lCluster^ do
 begin
  for i:=0 to aClu^.Count-1 do
   with AttrPtr(aClu^.Items^[i])^ do
    AtInsert(i,new(AttrPtr,Init(fNeg,fAttrNr,CopyExpTrmList(fAttrArgs))));
   fConsistent:=aClu^.fConsistent;
 end;
 CopyExpCluster:=lCluster;
end;

function CopyExpTyp(aTyp:TypPtr): TypPtr;
begin
 with aTyp^ do
 CopyExpTyp:=
  new(TypPtr,Init(TypSort,CopyExpCluster(LowerCluster),CopyExpCluster(UpperCluster),
                   ModNr,CopyExpTrmList(ModArgs)));
end;

function CopyExpFrm(fFrm:FrmPtr):FrmPtr;
 var lConjuncts: MCollection;
     lFrm: FrmPtr;
     z,n: integer;
begin
 with fFrm^ do
  case FrmSort of
   ikFrmConj:
    with ConjFrmPtr(fFrm)^.Conjuncts do
    begin lConjuncts.Init(Count,2);
     for z:=0 to Count-1 do
      begin lFrm:=CopyExpFrm(FrmPtr(Items^[z]));
       if lFrm^.FrmSort = ikFrmConj then
        for n:=0 to ConjFrmPtr(lFrm)^.Conjuncts.Count-1 do
         lConjuncts.Insert(ConjFrmPtr(lFrm)^.Conjuncts.Items^[n])
       else lConjuncts.Insert(lFrm);
      end;
     CopyExpFrm:=new(ConjFrmPtr, Init(lConjuncts));
    end;
   ikFrmUniv:
    begin
     lFrm:=
      new(UnivFrmPtr,
       Init(ikFrmUniv,CopyExpTyp(UnivFrmPtr(fFrm)^.Quantified),CopyExpFrm(UnivFrmPtr(fFrm)^.Scope)));
     UnivFrmPtr(lFrm)^.nVarId:=UnivFrmPtr(fFrm)^.nVarId;
     CopyExpFrm:=lFrm;
    end;
   ikFrmQual:
    CopyExpFrm:=
     new(QualFrmPtr,Init(CopyExpTrm(QualFrmPtr(fFrm)^.QualTrm),CopyExpTyp(QualFrmPtr(fFrm)^.QualTyp)));
   ikFrmSchPred,ikFrmPred,ikFrmAttr:
    CopyExpFrm:=
     new(PredFrmPtr,Init(FrmSort,PredFrmPtr(fFrm)^.PredNr,
                 CopyExpTrmList(PredFrmPtr(fFrm)^.PredArgs)));
   ikFrmPrivPred:
    if LocPredFrmPtr(fFrm)^.PredExp^.FrmSort <> ikError then
     CopyExpFrm:=CopyExpFrm(LocPredFrmPtr(fFrm)^.PredExp)
    else CopyExpFrm:=
     new(LocPredFrmPtr,Init(PredFrmPtr(fFrm)^.PredNr,
                            CopyExpTrmList(LocPredFrmPtr(fFrm)^.PredArgs),
                            CopyExpFrm(LocPredFrmPtr(fFrm)^.PredExp)));
   ikFrmNeg:
    CopyExpFrm:=
     NewNeg(CopyExpFrm(NegFrmPtr(fFrm)^.NegArg));
   ikFrmFlexConj: CopyExpFrm:=NewFlexFrm(
      CopyExpFrm(FlexFrmPtr(fFrm)^.nLeftOrigFrm),
      CopyExpFrm(FlexFrmPtr(fFrm)^.nRightOrigFrm),
      CopyExpFrm(FlexFrmPtr(fFrm)^.nExpansion),
      CopyExpTrm(FlexFrmPtr(fFrm)^.nLeftTrm),
      CopyExpTrm(FlexFrmPtr(fFrm)^.nRightTrm)
                                        );
   ikFrmVerum: CopyExpFrm:= NewVerum;
   ikError: CopyExpFrm:=NewIncorFrm;
   else
begin
{$IFDEF MDEBUG}
InfoChar(FrmSort);
{$ENDIF}
   RunTimeError(2643);
end;
   end;
 end;

function CopyExpTrm ( fTrm:TrmPtr ):TrmPtr;
 var z:integer;
     lLambdaArgs:MCollection;
begin
 if fTrm^.TrmSort = ikTrmEqConst then begin CopyExpTrm:=fTrm; exit end;
 with fTrm^ do
  case TrmSort of
   ikTrmSchFunc,ikTrmFunctor,ikTrmSelector,ikTrmAggreg:
    with FuncTrmPtr(fTrm)^ do
     CopyExpTrm:=NewLocFuncTrm(TrmSort,FuncNr,CopyExpTrmList(FuncArgs));
   ikTrmPrivFunc:
    with FuncTrmPtr(fTrm)^ do
     if LocFuncTrmPtr(fTrm)^.FuncExp^.TrmSort <> ikError then
      CopyExpTrm:=CopyExpTrm(LocFuncTrmPtr(fTrm)^.FuncExp)
     else //CopyExpTrm:=NewLocFuncTrm(TrmSort,FuncNr,CopyExpTrmList(FuncArgs));
     with LocFuncTrmPtr(fTrm)^ do
      CopyExpTrm:=NewPrivFuncTrm(FuncNr,CopyExpTrmList(FuncArgs),CopyExpTrm(FuncExp));
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
    begin
     lLambdaArgs.Init(LambdaArgs.Count,4);
     with LambdaArgs do
      for z:=0 to Count-1 do
       lLambdaArgs.Insert(CopyExpTyp(TypPtr(Items^[z])));
     CopyExpTrm:=NewFraenkelTrmI(CopyExpTrm(LambdaScope),CopyExpFrm(Compr),lLambdaArgs,nIdents);
    end;
   ikTrmChoice:
    with ChoiceTrmPtr(fTrm)^ do
     CopyExpTrm:=NewChoiceTrm(CopyExpTyp(ChoiceTyp));
   ikTrmQua:
    with QuaTrmPtr(fTrm)^ do
     CopyExpTrm:=NewQuaTrm(CopyExpTrm(TrmProper),CopyExpTyp(Qua));
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmNumeral,ikTrmFreeVar,ikTrmLambdaVar:
    with VarTrmPtr(fTrm)^ do
    CopyExpTrm:=NewVarTrm(TrmSort,VarNr);
   ikTrmIt: CopyExpTrm:=NewItTrm;
   ikError: CopyExpTrm:=NewIncorTrm;
   else
begin
{$IFDEF MDEBUG}
InfoString('(CopyExpTrm)TrmSort='); InfoChar(TrmSort); flush(InfoFile);
{$ENDIF}
   RunTimeError(2644);
end;
  end;
end;

procedure FrRenBound(var fTrm: TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if (TrmSort=ikTrmBound) then inc(VarNr,gBoundBase);
end;

var  BoundBase: integer;

procedure RenBound(var fTrm: TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if (TrmSort=ikTrmBound) and (VarNr>BoundBase)
   then inc(VarNr,BoundVarNbr-BoundBase);
end;

function CopyTerm ( fTrm:TrmPtr ):TrmPtr;
 var lTrm,lRes:TrmPtr; i,z:integer;
     lLambdaArgs:MCollection;
begin
 if fTrm^.TrmSort = ikTrmEqConst then begin CopyTerm:=fTrm; exit end;
 if Instantiated then
  with VarTrmPtr(fTrm)^ do
   if TrmSort = ikTrmLocus then
    begin Instantiated:=false;
      lTrm:=CopyTerm(Instantiation[VarNr]);
      Instantiated:=true;
      WithinTerm(lTrm,RenBound);
      CopyTerm:=lTrm;
      exit;
    end;
 with fTrm^ do
  case TrmSort of
   ikTrmSchFunc,ikTrmFunctor,ikTrmSelector,ikTrmAggreg:
    with FuncTrmPtr(fTrm)^ do
     lRes:=NewLocFuncTrm(TrmSort,FuncNr,CopyTermList(FuncArgs));
   ikTrmPrivFunc:
    with LocFuncTrmPtr(fTrm)^ do
     lRes:=NewPrivFuncTrm(FuncNr,CopyTermList(FuncArgs),CopyTerm(FuncExp));
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
    begin i:=BoundvarNbr;
     lLambdaArgs.Init(LambdaArgs.Count,4);
     with LambdaArgs do for z:=0 to Count-1 do
      begin  inc(BoundVarNbr);
       lLambdaArgs.Insert(TypPtr(Items^[z])^.CopyType);
      end;
     lTrm:=CopyTerm(LambdaScope);
     lRes:=NewFraenkelTrmI(lTrm,Compr^.CopyFormula,lLambdaArgs,nIdents);
     BoundvarNbr:=i;
    end;
   ikTrmChoice:
    lRes:=NewChoiceTrm(ChoiceTrmPtr(fTrm)^.ChoiceTyp^.CopyType);
   ikTrmQua:
    begin lTrm:=CopyTerm(QuaTrmPtr(fTrm)^.TrmProper);
     lRes:=NewQuaTrm(lTrm,QuaTrmPtr(fTrm)^.Qua^.CopyType);
    end;
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmNumeral,ikTrmFreeVar,ikTrmLambdaVar:
    with VarTrmPtr(fTrm)^ do
     lRes:=NewVarTrm(TrmSort,VarNr);
   ikTrmIt: lRes:=NewItTrm;
   ikError: lRes:=NewIncorTrm;
   else
begin
{$IFDEF MDEBUG}
InfoString('(CopyTerm)TrmSort='); InfoChar(TrmSort); flush(InfoFile);
{$ENDIF}
   RunTimeError(2044);
end;
  end;
 lRes^.nPattNr:= fTrm^.nPattNr;
 CopyTerm:= lRes;
end;

function TypObj.DecreasingAttrs(fTarget:TypPtr; aEqAttr: EqualAttrs):boolean;
begin
 if fTarget^.LowerCluster^.Count = 0 then
  begin DecreasingAttrs:=true; exit end;
 DecreasingAttrs:=
  fTarget^.LowerCluster^.IsSubsetOf(UpperCluster,aEqAttr);
end;

procedure TypObj.RoundUp;
 var lClusterPtr:AttrCollectionPtr;
begin
 lClusterPtr:=CopyCluster(UpperCluster);
 lClusterPtr^.RoundUpWith(@Self);
 dispose(UpperCluster,Done);
 UpperCluster:=lClusterPtr;
end;

function OriginalNr( c:ConstructorsKind; aNr:integer ):integer;
begin
 OriginalNr:= ConstrPtr(Constr[c].At(aNr))^.fWhichConstrNr;
end;

function AdjustedNr( c:ConstructorsKind; aNr:integer ):integer;
 var lNr: integer;
begin
 lNr:= ConstrPtr(Constr[c].At(aNr))^.fWhichConstrNr;
 if lNr = 0 then AdjustedNr:=aNr
  else AdjustedNr:=lNr;
end;

function AdjustedFuncNr(aTrm:TrmPtr):integer;
begin
 case aTrm^.TrmSort of
  ikTrmFunctor:
   with FuncTrmPtr(aTrm)^,
  ConstrTypPtr(Constr[ coFunctor].At(FuncNr))^ do
   if fWhichConstrNr = 0 then
    AdjustedFuncNr:= FuncNr
   else  AdjustedFuncNr:= fWhichConstrNr;
  ikTrmSchFunc,
  ikTrmAggreg,
  ikTrmSelector: AdjustedFuncNr:=FuncTrmPtr(aTrm)^.FuncNr;
 else AdjustedFuncNr:=0;
 end;
end;

function MotherStructNr( fSelectNr:integer ):integer;
var k:integer;
begin
 MotherStructNr:=0;
 for k:=1 to Constr[coStructMode].Count-1 do
  with StructConstrPtr( Constr[coStructMode].At(k))^ do
   if fFields^.HasInDom(fSelectNr) then
   begin MotherStructNr:=k; exit end;
 { niedostepny selektor }
  ErrImm(135);
end;

procedure AdjustTrm ( fTrm:TrmPtr; var fFunc:integer; var fArgs:TrmList );
 var k: integer;
begin
 with FuncTrmPtr(fTrm)^,
 ConstrTypPtr( Constr[ coFunctor].At(FuncNr))^ do
 begin
  fArgs:=FuncArgs;
  if fWhichConstrNr<>0 then
  begin
   fFunc:=fWhichConstrNr;
   for k:=1 to fSuperfluous do fArgs:=fArgs^.NextTrm;
  end
  else fFunc:=FuncNr;
 end;
end;

procedure TypObj.AdjustTyp(var fMode:integer; var fArgs:TrmList);
var k: integer;
begin
 Mizassert( 2148, TypSort in [ikTypStruct, ikTypMode]);
 if TypSort=ikTypStruct then
  begin
   fMode:=ModNr; fArgs:=ModArgs;
  end
 else  
  with ConstrTypPtr( Constr[ coMode].At( ModNr))^ do
   begin fArgs:=ModArgs;
    if fWhichConstrNr<>0 then
     begin
      fMode:=fWhichConstrNr;
      for k:=1 to fSuperfluous do fArgs:=fArgs^.NextTrm;
      end
    else fMode:=ModNr;
   end;
end;

procedure AttrObj.AdjustAttr(var aAttrNr:integer; var aArgs:TrmList);
 var k: integer;
begin
 aAttrNr:=fAttrNr;
 aArgs:=fAttrArgs;
 with ConstrTypPtr( Constr[ coAttribute].At( fAttrNr))^ do
  begin
   if fWhichConstrNr<>0 then
    begin aAttrNr:=fWhichConstrNr;
     for k:=1 to fSuperfluous do aArgs:=aArgs^.NextTrm;
    end;
  end;
end;

function AttrObj.AdjustedAttrNr: integer;
begin
 AdjustedAttrNr:=fAttrNr;
 with ConstrTypPtr( Constr[ coAttribute].Items^[ fAttrNr])^ do
  if fWhichConstrNr<>0 then
   AdjustedAttrNr:=fWhichConstrNr;
end;

procedure AdjustFrm ( fFrm:PredFrmPtr; var fPred:integer; var fArgs:TrmList );
 var k: integer;
begin
 with fFrm^, ConstrPtr( Constr[ coPredicate].Items^[ PredNr])^ do
  begin fPred:=PredNr; fArgs:=PredArgs;
   if fWhichConstrNr<>0 then
    begin fPred:=fWhichConstrNr;
     for k:=1 to fSuperfluous do fArgs:=fArgs^.NextTrm;
    end;
  end;
end;

// ##TODO: merge with previous
procedure AdjustAttrFrm ( fFrm:PredFrmPtr; var fAttr:integer; var fArgs:TrmList );
 var k: integer;
begin
 with fFrm^, ConstrPtr( Constr[ coAttribute].Items^[ PredNr])^ do
  begin fAttr:=PredNr; fArgs:=PredArgs;
   if fWhichConstrNr<>0 then
    begin fAttr:=fWhichConstrNr;
     for k:=1 to fSuperfluous do fArgs:=fArgs^.NextTrm;
    end;
  end;
end;

// ##TODO: using ConstructorKind here is risky if we ever switch to
//         enumeration types for Trm and Frm Kinds
function AdjustTrmList(aKind:char; aNr:integer; aTrmList:TrmList):TrmList;
var i:integer; lSuperfluous:integer;
begin
 case aKind of
  ikFrmPred,
  ikTrmFunctor,
  ikFrmAttr,
  ikTypMode:
   with ConstrPtr( Constr[ ConstructorKind(aKind)].At( aNr))^ do
   lSuperfluous:=fSuperfluous;
 else RunTimeError(2224);
 end;
 for i:=1 to lSuperfluous do aTrmList:=aTrmList^.NextTrm;
 AdjustTrmList:=aTrmList;
end;

function EquateTrmsLists(fTrm,aTrm:TrmList; aEqTrms: EqualTrms): boolean;
begin EquateTrmsLists:=false;
 while (fTrm<>nil) and (aTrm<>nil) do
  begin if not aEqTrms(fTrm^.XTrmPtr,aTrm^.XtrmPtr) then exit;
   fTrm:=fTrm^.NextTrm; aTrm:=aTrm^.NextTrm;
  end;
 EquateTrmsLists:=fTrm=aTrm;
end;

function StrictEqTrmList(fTL1,fTL2: TrmList): boolean;
begin
 StrictEqTrmList:=EquateTrmsLists(fTL1,fTL2,StrictEqTrm);
end;

function EqTrmList(fTL1,fTL2: TrmList): boolean;
begin
 EqTrmList:=EquateTrmsLists(fTL1,fTL2,EqTrm);
end;

function StrictEqAttr(fAttr1, fAttr2: AttrPtr): boolean;
begin
 StrictEqAttr := (fAttr1^.fAttrNr = fAttr2^.fAttrNr) and
               (fAttr1^.fNeg = fAttr2^.fNeg) and
               StrictEqTrmList(fAttr1^.fAttrArgs,fAttr2^.fAttrArgs);
end;

function StrictEqTyp(fTyp1,fTyp2:TypPtr):boolean;
begin StrictEqTyp:=false;
 with TypPtr(FTyp1)^ do
  if TypSort=TypPtr(fTyp2)^.TypSort then
   case TypSort of
    ikTypMode,ikTypStruct:
     if (ModNr=TypPtr(fTyp2)^.ModNr) and
        EqualClusters(TypPtr(fTyp1),TypPtr(fTyp2),StrictEqAttr)
     then
      StrictEqTyp:=EqTrmList(ModArgs,TypPtr(fTyp2)^.ModArgs);
    ikError: StrictEqTyp:=true;
    else
begin
{$IFDEF MDEBUG}
write(InfoFile,TypSort);
{$ENDIF}
     RunTimeError(2004);
end;
   end;
end;

function EqAttr(fAttr1, fAttr2: AttrPtr): boolean;
  var lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 fAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 fAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 EqAttr := (lAttrNr1 = lAttrNr2) and
           (fAttr1^.fNeg = fAttr2^.fNeg) and
           EqTrmList(lArgs1,lArgs2);
end;

function EqTyp ( fTyp1,fTyp2:TypPtr ):boolean;
 var ModNr1,ModNr2: integer; A1,A2: TrmList;
begin EqTyp:=false;
 with TypPtr(FTyp1)^ do
  if TypSort=TypPtr(fTyp2)^.TypSort then
   case TypSort of
    ikTypMode:
     if EqualClusters(TypPtr(fTyp1),TypPtr(fTyp2),EqAttr) then
      begin TypPtr(fTyp1)^.AdjustTyp(ModNr1,A1); TypPtr(fTyp2)^.AdjustTyp(ModNr2,A2);
       if ModNr1=ModNr2 then EqTyp:=EqTrmList(A1,A2);
      end;
    ikTypStruct:
     if EqualClusters(TypPtr(fTyp1),TypPtr(fTyp2),EqAttr) then
      if ModNr=TypPtr(fTyp2)^.ModNr then
       EqTyp:=EqTrmList(ModArgs,TypPtr(fTyp2)^.ModArgs);
    ikError: EqTyp:=true;
    else RunTimeError(2048);
   end;
end;

function EqFrm ( fFrm1,fFrm2:FrmPtr ):boolean;
 var PredNr1,PredNr2,k,l: integer; A1,A2: TrmList;
     Left1,Right1,Left2,Right2: TrmPtr;
     lFrm: FrmPtr;
     lConj1,lConj2: MList;
begin EqFrm:=false;
 SkipLocPred(fFrm1); SkipLocPred(fFrm2);
 if fFrm1^.FrmSort=fFrm2^.FrmSort then
  case fFrm1^.FrmSort of
   ikFrmVerum,ikFrmThesis: EqFrm:=true;
   ikFrmNeg: EqFrm:=EqFrm(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmQual:
    if EqTrm(QualFrmPtr(fFrm1)^.QualTrm,QualFrmPtr(fFrm2)^.QualTrm) then
     EqFrm:=EqTyp(QualFrmPtr(fFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^.Conjuncts do
     if Count = ConjFrmPtr(fFrm2)^.Conjuncts.Count then
      begin
       for k:=0 to Count-1 do
        if not EqFrm(FrmPtr(Items^[k]),FrmPtr(ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k])) then
          exit;
       EqFrm:=true;
      end
     else
      begin
       lConj1.Init(Count);
       for k:=0 to Count-1 do
        begin lFrm:=Items^[k];
         SkipLocPred(lFrm);
         if lFrm^.FrmSort = ikFrmConj then
          for l:=0 to ConjFrmPtr(lFrm)^.Conjuncts.Count-1 do
           lConj1.Insert(ConjFrmPtr(lFrm)^.Conjuncts.Items^[l])
         else lConj1.Insert(lFrm);
        end;
       lConj2.Init(ConjFrmPtr(fFrm2)^.Conjuncts.Count);
       for k:=0 to ConjFrmPtr(fFrm2)^.Conjuncts.Count-1 do
        begin lFrm:=ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k];
         SkipLocPred(lFrm);
         if lFrm^.FrmSort = ikFrmConj then
          for l:=0 to ConjFrmPtr(lFrm)^.Conjuncts.Count-1 do
           lConj2.Insert(ConjFrmPtr(lFrm)^.Conjuncts.Items^[l])
         else lConj2.Insert(lFrm);
        end;
       if lConj1.Count = lConj2.Count then
        begin
         for k:=0 to lConj1.Count-1 do
          if not EqFrm(FrmPtr(lConj1.Items^[k]),FrmPtr(lConj2.Items^[k])) then
           begin
            lConj1.DeleteAll; lConj1.Done;
            lConj2.DeleteAll; lConj2.Done;
            exit;
           end;
         EqFrm:=true;
        end;
       lConj1.DeleteAll; lConj1.Done;
       lConj2.DeleteAll; lConj2.Done;
      end;
   ikFrmSchPred,ikFrmPrivPred:
    if PredFrmPtr(fFrm1)^.PredNr=PredFrmPtr(fFrm2)^.PredNr then
     EqFrm:=EqTrmList(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
   ikFrmAttr:
    begin AdjustAttrFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustAttrFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
      EqFrm:=EqTrmList(A1,A2);
    end;
   ikFrmPred:
    begin AdjustFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
      if PredNr1=gBuiltIn[rqEqualsTo] then
       begin
        Left1:=A1^.XTrmPtr;
        Right1:=A1^.NextTrm^.XTrmPtr;
        Left2:=A2^.XTrmPtr;
        Right2:=A2^.NextTrm^.XTrmPtr;
        EqFrm:= EqTrm(Left1,Left2) and EqTrm(Right1,Right2) or
                EqTrm(Right1,Left2) and EqTrm(Left1,Right2);
       end
      else EqFrm:=EqTrmList(A1,A2);
    end;
   ikFrmUniv:
    if EqTyp(UnivFrmPtr(fFrm1)^.Quantified,UnivFrmPtr(fFrm2)^.Quantified) then
     EqFrm:=EqFrm(UnivFrmPtr(fFrm1)^.Scope,UnivFrmPtr(fFrm2)^.Scope);
   ikFrmFlexConj:
    begin
     EqFrm:=
      EqFrm(FlexFrmPtr(fFrm1)^.nLeftOrigFrm,FlexFrmPtr(fFrm2)^.nLeftOrigFrm) and
      EqFrm(FlexFrmPtr(fFrm1)^.nRightOrigFrm,FlexFrmPtr(fFrm2)^.nRightOrigFrm) and
      EqFrm(FlexFrmPtr(fFrm1)^.nExpansion,FlexFrmPtr(fFrm2)^.nExpansion);
    end;
   ikError: EqFrm:=true;
   else RunTimeError(2049);
  end;
end;

function EqTrm ( fTrm1,fTrm2:TrmPtr):boolean;
  var FuncNr1,FuncNr2,i: integer; A1,A2: TrmList;
begin EqTrm:=false;
 while fTrm1^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm1)^.FuncExp^.TrmSort=ikError then break
  else fTrm1:=LocFuncTrmPtr(fTrm1)^.FuncExp;
 while fTrm2^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm2)^.FuncExp^.TrmSort=ikError then break
  else fTrm2:=LocFuncTrmPtr(fTrm2)^.FuncExp;
 with TrmPtr(fTrm2)^ do
  if TrmSort=TrmPtr(fTrm1)^.TrmSort then
   case TrmSort of
    ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmEqConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral:
     with VarTrmPtr(fTrm2)^ do
      EqTrm:=VarTrmPtr(fTrm1)^.VarNr=VarNr;
    ikTrmInfConst:
     with VarTrmPtr(fTrm2)^ do
     begin
      if VarTrmPtr(fTrm1)^.VarNr = VarNr then begin EqTrm:=true; exit end;
      EqTrm:=EqTrm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm1)^.VarNr])^.fDef,
                   ConstDefPtr(InferConstDef.Items^[VarNr])^.fDef);
     end;
    ikTrmFunctor:
     begin AdjustTrm(fTrm1,FuncNr1,A1); AdjustTrm(fTrm2,FuncNr2,A2);
      if FuncNr1=FuncNr2 then EqTrm:=EqTrmList(A1,A2);
     end;
    ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmSelector:
     with FuncTrmPtr(fTrm2)^ do
      if FuncTrmPtr(fTrm1)^.FuncNr=FuncNr then
       EqTrm:=EqTrmList(FuncTrmPtr(fTrm1)^.FuncArgs,FuncArgs);
    ikTrmFraenkel:
     with FraenkelTrmPtr(fTrm2)^ do
      if FraenkelTrmPtr(fTrm1)^.LambdaArgs.Count = LambdaArgs.Count then
       begin
        for i:=0 to LambdaArgs.Count-1 do
         if not EqTyp(LambdaArgs.Items^[i],FraenkelTrmPtr(fTrm1)^.LambdaArgs.Items^[i]) then
          exit;
        if EqTrm(FraenkelTrmPtr(fTrm1)^.LambdaScope,LambdaScope) then
         EqTrm:=EqFrm(FraenkelTrmPtr(fTrm1)^.Compr,Compr);
       end;
    ikTrmChoice:
     with ChoiceTrmPtr(fTrm2)^ do
      EqTrm:=EqTyp(ChoiceTrmPtr(fTrm1)^.ChoiceTyp,ChoiceTyp);
    ikTrmIt,ikError: EqTrm:=true;
    else RunTimeError(2050);
   end
     else
   if VarTrmPtr(fTrm1)^.TrmSort = ikTrmInfConst then
    begin
     EqTrm:=EqTrm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm1)^.VarNr])^.fDef,fTrm2);
    end
  else
   if TrmSort = ikTrmInfConst then
    begin
     EqTrm:=EqTrm(fTrm1,ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm2)^.VarNr])^.fDef);
    end;
end;

function StrictEqTrm ( fTrm1,fTrm2:TrmPtr ):boolean;
  var i: integer;
begin StrictEqTrm:=false;
 while fTrm1^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm1)^.FuncExp^.TrmSort=ikError then break
  else fTrm1:=LocFuncTrmPtr(fTrm1)^.FuncExp;
 while fTrm2^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm2)^.FuncExp^.TrmSort=ikError then break
  else fTrm2:=LocFuncTrmPtr(fTrm2)^.FuncExp;
 with TrmPtr(fTrm2)^ do
  if TrmSort=TrmPtr(fTrm1)^.TrmSort then
   case TrmSort of
    ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmEqConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral:
     with VarTrmPtr(fTrm2)^ do
      StrictEqTrm:=VarTrmPtr(fTrm1)^.VarNr=VarNr;
    ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmSelector,ikTrmFunctor:
     with FuncTrmPtr(fTrm2)^ do
      if FuncTrmPtr(fTrm1)^.FuncNr=FuncNr then
       StrictEqTrm:=StrictEqTrmList(FuncTrmPtr(fTrm1)^.FuncArgs,FuncArgs);
    ikTrmFraenkel:
     with FraenkelTrmPtr(fTrm2)^ do
      if FraenkelTrmPtr(fTrm1)^.LambdaArgs.Count = LambdaArgs.Count then
       begin
        for i:=0 to LambdaArgs.Count-1 do
         if not EqTyp(LambdaArgs.Items^[i],FraenkelTrmPtr(fTrm1)^.LambdaArgs.Items^[i]) then
          exit;
        if StrictEqTrm(FraenkelTrmPtr(fTrm1)^.LambdaScope,LambdaScope) then
         StrictEqTrm:=EqFrm(FraenkelTrmPtr(fTrm1)^.Compr,Compr);
       end;
    ikTrmChoice:
     with ChoiceTrmPtr(fTrm2)^ do
      StrictEqTrm:=EqTyp(ChoiceTrmPtr(fTrm1)^.ChoiceTyp,ChoiceTyp);
    ikTrmIt,ikError: StrictEqTrm:=true;
    else RunTimeError(2050);
   end;
end;

function StrictEqFrm (fFrm1,fFrm2:FrmPtr ):boolean;
 var PredNr1,PredNr2,k,l: integer; A1,A2: TrmList;
     Left1,Right1,Left2,Right2: TrmPtr;
     lFrm: FrmPtr;
     lConj1,lConj2: MList;
begin StrictEqFrm:=false;
 SkipLocPred(fFrm1); SkipLocPred(fFrm2);
 if fFrm1^.FrmSort=fFrm2^.FrmSort then
  case fFrm1^.FrmSort of
   ikFrmVerum,ikFrmThesis: StrictEqFrm:=true;
   ikFrmNeg: StrictEqFrm:=StrictEqFrm(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmQual:
    if StrictEqTrm(QualFrmPtr(fFrm1)^.QualTrm,QualFrmPtr(fFrm2)^.QualTrm) then
     StrictEqFrm:=StrictEqTyp(QualFrmPtr(fFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^.Conjuncts do
     if Count = ConjFrmPtr(fFrm2)^.Conjuncts.Count then
      begin
       for k:=0 to Count-1 do
        if not StrictEqFrm(FrmPtr(Items^[k]),FrmPtr(ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k])) then
          exit;
       StrictEqFrm:=true;
      end
     else
      begin
       lConj1.Init(Count);
       for k:=0 to Count-1 do
        begin lFrm:=Items^[k];
         SkipLocPred(lFrm);
         if lFrm^.FrmSort = ikFrmConj then
          for l:=0 to ConjFrmPtr(lFrm)^.Conjuncts.Count-1 do
           lConj1.Insert(ConjFrmPtr(lFrm)^.Conjuncts.Items^[l])
         else lConj1.Insert(lFrm);
        end;
       lConj2.Init(ConjFrmPtr(fFrm2)^.Conjuncts.Count);
       for k:=0 to ConjFrmPtr(fFrm2)^.Conjuncts.Count-1 do
        begin lFrm:=ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k];
         SkipLocPred(lFrm);
         if lFrm^.FrmSort = ikFrmConj then
          for l:=0 to ConjFrmPtr(lFrm)^.Conjuncts.Count-1 do
           lConj2.Insert(ConjFrmPtr(lFrm)^.Conjuncts.Items^[l])
         else lConj2.Insert(lFrm);
        end;
       if lConj1.Count = lConj2.Count then
        begin
         for k:=0 to lConj1.Count-1 do
          if not StrictEqFrm(FrmPtr(lConj1.Items^[k]),FrmPtr(lConj2.Items^[k])) then
           begin
            lConj1.DeleteAll; lConj1.Done;
            lConj2.DeleteAll; lConj2.Done;
            exit;
           end;
         StrictEqFrm:=true;
        end;
       lConj1.DeleteAll; lConj1.Done;
       lConj2.DeleteAll; lConj2.Done;
      end;
   ikFrmSchPred,ikFrmPrivPred:
    if PredFrmPtr(fFrm1)^.PredNr = PredFrmPtr(fFrm2)^.PredNr then
     StrictEqFrm:=StrictEqTrmList(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
   ikFrmAttr:
    begin 
     if PredFrmPtr(fFrm1)^.PredNr = PredFrmPtr(fFrm2)^.PredNr then
      StrictEqFrm:=StrictEqTrmList(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
    end;
   ikFrmPred:
    begin 
     PredNr1 := PredFrmPtr(fFrm1)^.PredNr;
     PredNr2 := PredFrmPtr(fFrm2)^.PredNr;
     A1 := PredFrmPtr(fFrm1)^.PredArgs;
     A2 := PredFrmPtr(fFrm2)^.PredArgs;
     if PredNr1=PredNr2 then
      if PredNr1=gBuiltIn[rqEqualsTo] then
      begin
        Left1:=A1^.XTrmPtr;
        Right1:=A1^.NextTrm^.XTrmPtr;
        Left2:=A2^.XTrmPtr;
        Right2:=A2^.NextTrm^.XTrmPtr;
        StrictEqFrm:= StrictEqTrm(Left1,Left2) and StrictEqTrm(Right1,Right2) or
                StrictEqTrm(Right1,Left2) and StrictEqTrm(Left1,Right2);
       end
      else StrictEqFrm:=StrictEqTrmList(A1,A2);
    end;
   ikFrmUniv:
    if StrictEqTyp(UnivFrmPtr(fFrm1)^.Quantified,UnivFrmPtr(fFrm2)^.Quantified) then
     StrictEqFrm:=StrictEqFrm(UnivFrmPtr(fFrm1)^.Scope,UnivFrmPtr(fFrm2)^.Scope);
   ikFrmFlexConj:
    StrictEqFrm:=
     StrictEqFrm(FlexFrmPtr(fFrm1)^.nLeftOrigFrm,FlexFrmPtr(fFrm2)^.nLeftOrigFrm) and
     StrictEqFrm(FlexFrmPtr(fFrm1)^.nRightOrigFrm,FlexFrmPtr(fFrm2)^.nRightOrigFrm) and
     StrictEqFrm(FlexFrmPtr(fFrm1)^.nExpansion,FlexFrmPtr(fFrm2)^.nExpansion);
   ikError: StrictEqFrm:=true;
   else RunTimeError(2049);
  end;
end;

function TypObj.EqRadices(fTyp2:TypPtr):boolean;
 var ModNr1,ModNr2: integer; A1,A2: TrmList;
begin EqRadices:=false;
 if TypSort=fTyp2^.TypSort then
  case TypSort of
   ikTypMode:
     begin AdjustTyp(ModNr1,A1); fTyp2^.AdjustTyp(ModNr2,A2);
      EqRadices:= (ModNr1=ModNr2) and EqTrmList(A1,A2);
     end;
   ikTypStruct:
    EqRadices:= (ModNr=fTyp2^.ModNr) and EqTrmList(ModArgs,fTyp2^.ModArgs);
   else RunTimeError(2051);
  end;
end;

procedure InitInst{(const aSubstTrm: LociSubstitution)};
 var k: integer;
begin Instantiated:=true;
 BoundBase:=BoundVarNbr;
 for k:=1 to 2*MaxArgNbr do
   Instantiation[k]:=gSubstTrm[k];
end;

procedure StopInst;
begin
 Instantiated:=false;
end;

procedure InitInstList(aTrmList:TrmList);
 var k: integer;
begin Instantiated:=true; k:=1; BoundBase:=BoundVarNbr;
 while aTrmList<>nil do
  with aTrmList^ do
   begin Instantiation[k]:=XTrmPtr; inc(k); aTrmList:=NextTrm end;
end;

function InstCluster(aClu: AttrCollectionPtr; aTrmList:TrmList): AttrCollectionPtr;
 var k: integer;
begin Instantiated:=true; k:=1; BoundBase:=BoundVarNbr;
 while aTrmList<>nil do
  with aTrmList^ do
   begin Instantiation[k]:=XTrmPtr; inc(k); aTrmList:=NextTrm end;
 InstCluster:=CopyCluster(aClu);
 Instantiated:=false;
end;

function TypObj.InstTyp(fTrmList:TrmList):TypPtr;
 var k: integer;
begin Instantiated:=true; k:=1; BoundBase:=BoundVarNbr;
 while fTrmList<>nil do
  with fTrmList^ do
   begin Instantiation[k]:=XTrmPtr; inc(k); fTrmList:=NextTrm end;
 InstTyp:=CopyType;
 Instantiated:=false;
end;

function InstTrm ( fTrm:TrmPtr; fTrmList:TrmList):TrmPtr;
 var k: integer;
begin Instantiated:=true; k:=1; BoundBase:=BoundVarNbr;
 while fTrmList<>nil do with fTrmList^ do
   begin Instantiation[k]:=XTrmPtr; inc(k); fTrmList:=NextTrm end;
 InstTrm:=CopyTerm(fTrm);
 Instantiated:=false;
end;

function InstFrm ( fFrm:FrmPtr; fTrmList:TrmList):FrmPtr;
 var k: integer;
begin Instantiated:=true; k:=1; BoundBase:=BoundVarNbr;
 while fTrmList<>nil do with fTrmList^ do
   begin Instantiation[k]:=XTrmPtr;
    inc(k);
    fTrmList:=NextTrm
   end;
 InstFrm:=fFrm^.CopyFormula;
 Instantiated:=false;
end;

function InstSubstTrm ( fTrm:TrmPtr):TrmPtr;
 var k: integer;
begin Instantiated:=true;
 BoundBase:=BoundVarNbr;
 for k:=1 to 2*MaxArgNbr do
  if gSubstTrm[k] <> nil then Instantiation[k]:=gSubstTrm[k]
  else break;
 InstSubstTrm:=CopyTerm(fTrm);
 Instantiated:=false;
end;

function InstSubstFrm ( fFrm:FrmPtr):FrmPtr;
 var k: integer;
begin Instantiated:=true;
 BoundBase:=BoundVarNbr;
 for k:=1 to 2*MaxArgNbr do
  if gSubstTrm[k] <> nil then Instantiation[k]:=gSubstTrm[k]
  else break;
 InstSubstFrm:=fFrm^.CopyFormula;
 Instantiated:=false;
end;

function InstQual( fFrm:FrmPtr; fTrmList:TrmList; fTrm:TrmPtr ):FrmPtr;
 var k: integer;
begin Instantiated:=true; k:=1; BoundBase:=BoundVarNbr;
 while fTrmList<>nil do with fTrmList^ do
   begin Instantiation[k]:=XTrmPtr; inc(k); fTrmList:=NextTrm end;
 Instantiation[k]:=fTrm;
 InstQual:=fFrm^.CopyFormula;
 Instantiated:=false;
end;

function InstTrmInTyp ( fTyp:TypPtr; fTrm:TrmPtr ):TypPtr;
begin Instantiated:=true; BoundBase:=BoundVarNbr;
 Instantiation[1]:=fTrm;
 InstTrmInTyp:=fTyp^.CopyType;
 Instantiated:=false;
end;

var  WithinVar: DoInTrm;

procedure WithinTrmList(fTrmList:TrmList);
 var i:integer;
begin
 for i:=1 to MaxElemNbr do
  if fTrmList=nil then exit else
   with fTrmList^ do
    begin WithinTrm(XTrmPtr); fTrmList:=NextTrm end;
 RunTimeError(2018);
end;

procedure WithinAttrColl(aClu: AttrCollectionPtr);
 var i: integer;
begin
 with aClu^ do
  for i:=0 to Count-1 do
   WithinTrmList(AttrPtr(Items^[i])^.fAttrArgs);
end;

procedure WithinTyp(fTyp:TypPtr);
begin
 with TypPtr(fTyp)^ do
 begin
  WithinAttrColl(LowerCluster);
  WithinAttrColl(UpperCluster);
  case TypSort of ikTypMode,ikTypStruct: WithinTrmList(ModArgs) end;
 end;
end;

procedure FrmObj.WithinFrm;
begin Abstract1 end;

procedure QualFrmObj.WithinFrm;
begin WithinTrm(QualTrm); WithinTyp(QualTyp) end;

procedure UnivFrmObj.WithinFrm;
begin
 if IncBounVarNbr then inc(BoundVarNbr);
 WithinTyp(Quantified);
 Scope^.WithinFrm;
 if IncBounVarNbr then dec(BoundVarNbr);
end;

procedure ConjFrmObj.WithinFrm;
  var z: integer;
begin
  with Conjuncts do for z:=0 to Count-1 do
   FrmPtr(Items^[z])^.WithinFrm;
end;

procedure BinFrmObj.WithinFrm;
begin
 nLeftArg^.WithinFrm;
 nRightArg^.WithinFrm;
end;

procedure NegFrmObj.WithinFrm;
begin NegArg^.WithinFrm end;

procedure PredFrmObj.WithinFrm;
begin WithinTrmList(PredArgs) end;

procedure LocPredFrmObj.WithinFrm;
begin WithinTrmList(PredArgs);
 PredExp^.WithinFrm;
end;

procedure UniqFrmObj.WithinFrm;
begin end;

procedure WithinTrm(var fTrm:TrmPtr);
  var z: integer;
begin
 with TrmPtr(fTrm)^ do
  case TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmEqConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmIt: WithinVar(fTrm);
   ikTrmNumeral,ikError: ;
   ikTrmSchFunc,ikTrmAggreg,ikTrmFunctor,ikTrmSelector: WithinTrmList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmPrivFunc:
    begin WithinTrmList(FuncTrmPtr(fTrm)^.FuncArgs);
     WithinTrm(LocFuncTrmPtr(fTrm)^.FuncExp);
    end;
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
     begin
      with LambdaArgs do
       for z:=0 to Count-1 do
       begin
        if IncBounVarNbr then inc(BoundVarNbr);
        WithinTyp(TypPtr(Items^[z]));
       end;
      WithinTrm(LambdaScope);
      Compr^.WithinFrm;
      if IncBounVarNbr then dec(BoundVarNbr,LambdaArgs.Count);
     end;
   ikTrmChoice: WithinTyp(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
   else
begin
{$IFDEF MDEBUG}
InfoChar(TrmSort);
{$ENDIF}
    RunTimeError(2053);
end;
  end;
end;

procedure WithinTypColl(var fColl: MCollection);
var i:integer;
begin
 for i:= 0 to fColl.Count-1 do WithinTyp(fColl.Items^[i]);
end;

procedure WithinRClu(fCluster: RClusterPtr);
begin with fCluster^ do
 begin
  WithinAttrColl(nConsequent.Upper);
  WithinTypColl(nPrimaryList);
  WithinTyp(nClusterType);
 end;
end;

procedure WithinCClu(fCluster: CClusterPtr);
begin
 with fCluster^ do
 begin
  WithinAttrColl(nConsequent.Upper);
  WithinTypColl(nPrimaryList);
  WithinTyp(nClusterType);
 end;
 WithinAttrColl(fCluster^.nAntecedent);
end;

procedure WithinFClu(fCluster: FClusterPtr);
begin with fCluster^ do
 begin
  WithinAttrColl(nConsequent.Upper);
  WithinTypColl(nPrimaryList);
  WithinTrm(nClusterTerm);
  if nClusterType <> nil then
   WithinTyp(nClusterType);
 end;
end;

procedure WithinTerm ( fTrm:TrmPtr; P:DoInTrm );
  var l: DoInTrm;
 begin l:=WithinVar; WithinVar:=P; WithinTrm(fTrm);  WithinVar:=l end;

procedure AttrObj.WithinAttr(P:DoInTrm);
  var l: DoInTrm;
 begin l:=WithinVar; WithinVar:=P; WithinTrmList(fAttrArgs); WithinVar:=l end;

procedure MAttrCollection.WithinAttrCollection(P:DoInTrm);
  var l: DoInTrm;
 begin l:=WithinVar; WithinVar:=P; WithinAttrColl(@Self); WithinVar:=l end;

procedure TypObj.WithinType(P:DoInTrm);
  var l: DoInTrm;
 begin l:=WithinVar; WithinVar:=P; WithinTyp(@Self); WithinVar:=l end;

procedure WithinFormula ( fFrm:FrmPtr; P:DoInTrm );
  var l: DoInTrm;
 begin l:=WithinVar; WithinVar:=P; fFrm^.WithinFrm; WithinVar:=l end;

procedure WithinTypeColl (var fColl:MCollection; P:DoInTrm );
var l: DoInTrm;
begin l:=WithinVar; WithinVar:=P; WithinTypColl(fColl); WithinVar:=l end;

procedure WithinRCluster ( fCluster:RClusterPtr; P:DoInTrm );
var l: DoInTrm;
begin l:=WithinVar; WithinVar:=P; WithinRClu(fCluster); WithinVar:=l end;

procedure WithinCCluster ( fCluster:CClusterPtr; P:DoInTrm );
var l: DoInTrm;
begin l:=WithinVar; WithinVar:=P; WithinCClu(fCluster); WithinVar:=l end;

procedure WithinFCluster ( fCluster:FClusterPtr; P:DoInTrm );
var l: DoInTrm;
begin l:=WithinVar; WithinVar:=P; WithinFClu(fCluster); WithinVar:=l end;

procedure WithinCluster ( fCluster:ClusterPtr; P:DoInTrm );
begin
 case fCluster^.nClusterKind of
  clRegistered: WithinRCluster( RClusterPtr(fCluster), P);
  clConditional: WithinCCluster( CClusterPtr(fCluster), P);
  clFunctor: WithinFCluster( FClusterPtr(fCluster), P);
 end;
end;


// ##ENSURE: This can only be called when WithinVar is assigned!!
// ##TODO: uncomment the disposing
procedure ExpandInferConsts(var fTrm:TrmPtr);
var lTrm: TrmPtr;
begin
 if (fTrm^.TrmSort = ikTrmInfConst) then
 begin
  lTrm := CopyTerm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm)^.VarNr])^.fDef);
  dispose(fTrm, Done);
  gBoundBase:=BoundVarNbr;
  WithInTerm(lTrm,FrRenBound);
  WithinTrm(lTrm);
  fTrm := lTrm;
 end;
end;

{       if nClusterTerm^.T.FuncNr = fTrm^.T.FuncNr then}
{ ????? Ograniczenie implementacyjne }
  { !!!!!!!!
    Brak kontroli, czy atrybuty z ClusterColl.Items^[nConsequent] maja sens dla
    typu lTyp. Jezeli nie maja, spowoduja bledy runtime'owe !!!!!!!!
    Czy to nie dotyczy takze klastrow warunkowych, gdy typ nie daje
    sie rozszerzyc do typu atrybutu.
  }

function RoundUpWith(fCluster: FClusterPtr; fTrm:TrmPtr; fTyp: TypPtr;
                     var fClusterPtr: AttrCollectionPtr): boolean;
 var k: integer;
     lEq: boolean;
     lSubstTrm: LociSubstitution;
     lTrm: TrmPtr;
     lTyp: TypPtr;
  label 1;
begin RoundUpWith:=false;
 with fCluster^ do
 begin
  fillchar(gSubstTrm,sizeof(gSubstTrm),0);
//  if not nConsequent.Upper^.IsSubsetOf(fClusterPtr,AttrEquals) then
  begin
   lSubstTrm:=gSubstTrm;
   lEq:=EsTrm(nClusterTerm,fTrm) and CheckLociTypes(nPrimaryList);
   if not lEq then
    if fTrm^.TrmSort = ikTrmFunctor then
    with FuncTrmPtr(fTrm)^ do
    begin
     with ConstrTypPtr(Constr[coFunctor].Items^[FuncNr])^ do
      if syCommutativity in fProperties then
       begin
        lTrm:=NewFuncTrm(FuncNr,SwapArguments(FuncArgs,fFirstArg,fSecondArg));
        for k:=1 to 2*MaxArgNbr do
         if (gSubstTrm[k] <> nil) and (lSubstTrm[k] = nil) then
          begin DisposeTrm(gSubstTrm[k]); gSubstTrm[k]:=nil end;
        lEq:=EsTrm(nClusterTerm,lTrm) and CheckLociTypes(nPrimaryList);
        DisposeTrm(lTrm);
       end;
    end;
   if lEq then
   begin
    if nClusterType <> nil then
     begin
      lTyp:=nClusterType^.WideningOf(fTyp^.CopyType);
      if lTyp = nil then goto 1;
      if not CompEsTyp(nClusterType,lTyp,false) or
         not nClusterType^.LowerCluster^.IsSubsetOf(lTyp^.UpperCluster,EsAttrRev) or
         not CheckLociTypes(nPrimaryList) then
       begin Dispose(lTyp,Done); goto 1 end;
      Dispose(lTyp,Done);
     end;
    InitInst;
    fClusterPtr^.EnlargeBy(nConsequent.Upper);
    StopInst;
    RoundUpWith:=true;
   end;
1:
   DisposeSubstTrm;
  end;
 end;
end;

function RoundUpTrmType(fTrm:TrmPtr): TypPtr;
begin
 while fTrm^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm)^.FuncExp^.TrmSort=ikError then break
  else fTrm:=LocFuncTrmPtr(fTrm)^.FuncExp;
 RoundUpTrmType:=RoundUpTrmTypeWithType(CopyTrmType(fTrm),fTrm);
end;

function GetTrmType(fTrm:TrmPtr):TypPtr;
 var lPlace:integer;
begin
 case fTrm^.TrmSort of
 ikTrmFunctor,ikTrmSelector,ikTrmAggreg:
  begin
   if not gTermCollection.Search(fTrm,lPlace) then
    lPlace:=InsertTermInTTColl(fTrm);
   GetTrmType:=TTPairPtr(gTermCollection.Items^[lPlace])^.nTyp^.CopyType;
  end;
// ikTrmPrivFunc: GetTrmType:=GetTrmType(LocFuncTrmPtr(fTrm)^.FuncExp);
 else GetTrmType:=CopyTrmType(fTrm);
 end;
end;

function CopyTrmType(fTrm:TrmPtr): TypPtr;
 var lTypPtr:TypPtr;
begin
 with fTrm^ do
  case TrmSort of
   ikTrmBound: CopyTrmType:=BoundVar[VarTrmPtr(fTrm)^.VarNr]^.CopyType;
   ikTrmConstant:
    begin lTypPtr:=FixedVar[VarTrmPtr(fTrm)^.VarNr].nTyp^.CopyType;
     lTypPtr^.WithinType(ChangeBound);
     CopyTrmType:=lTypPtr;
    end;
   ikTrmInfConst:
     CopyTrmType:=ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm)^.VarNr])^.fTyp^.CopyType;
//   ikTrmNumeral: CopyTrmType:=NonZeroTyp^.CopyType;
   ikTrmNumeral:
    with VarTrmPtr(fTrm)^ do
      CopyTrmType:=NonZeroTyp^.CopyType;
   ikTrmLocus: CopyTrmType:=LocArgTyp[VarTrmPtr(fTrm)^.VarNr]^.CopyType;
//   ikTrmFreeVar: CopyTrmType:=FreeVar[VarTrmPtr(fTrm)^.VarNr]^.CopyType;
   ikTrmSchFunc:
    CopyTrmType:=TypPtr(CurSchFuncTyp.Items^[FuncTrmPtr(fTrm)^.FuncNr-1])^.CopyType;
   ikTrmPrivFunc:
   with LocFuncTrmPtr(fTrm)^ do
    CopyTrmType:=FuncDefPtr(LocFuncDef.Items^[FuncNr-1])^.fFuncTyp^.InstTyp(FuncArgs);
   ikTrmFunctor,
   ikTrmSelector,
   ikTrmAggreg:
    with FuncTrmPtr(fTrm)^,ConstrTypPtr(Constr[ ConstructorKind( TrmSort)].Items^[FuncNr])^ do
    CopyTrmType:= fConstrTyp^.InstTyp(FuncArgs);
   ikTrmFraenkel:
    CopyTrmType:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                 gBuiltIn[rqSetMode],nil);
   ikTrmChoice: CopyTrmType:=ChoiceTrmPtr(fTrm)^.ChoiceTyp^.CopyType;
   ikTrmIt: CopyTrmType:=ItTyp^.CopyType;
   ikTrmQua: CopyTrmType:=QuaTrmPtr(fTrm)^.Qua^.CopyType;
   ikError: CopyTrmType:=NewInCorTyp;
   else
begin
{$IFDEF MDEBUG}
InfoChar(TrmSort); writeln(InfoFile,'|');
{$ENDIF}
      RunTimeError(2054);
end;
  end;
end;

function TypObj.WidenToStruct: TypPtr;
 var lTyp1,lTyp2:TypPtr;
begin WidenToStruct:=nil; lTyp1:=CopyType;
 while lTyp1^.TypSort = ikTypMode do
  begin
   lTyp2:=lTyp1^.Widening;
   dispose(lTyp1,Done);
   lTyp1:=lTyp2; if lTyp1 = nil then exit;
  end;
 WidenToStruct:=lTyp1;
end;

procedure WidenningPath(fStructNr:integer);
var k,lInd:integer;
begin
 lInd:=gWidStruct.Count; gWidStruct.Insert(nil);
 with StructConstrPtr( Constr[ coStructMode].At( fStructNr))^ do
 for k:=0 to fPrefixes.Count-1 do
  begin
   if TypPtr(fPrefixes.Items^[k])^.ModNr = gTargetStructNr then
    begin gWidStructFound:=true;
     gWidStruct.AtPut(lInd,fPrefixes.Items^[k]);
     exit;
    end;
   WidenningPath(TypPtr(fPrefixes.Items^[k])^.ModNr);
   if gWidStructFound then
    begin gWidStruct.AtPut(lInd,fPrefixes.Items^[k]);
     exit;
    end;
  end;
 gWidStruct.AtDelete(lInd);
end;

// This is a cheap pretest which avoids expensive type copying
function TypReachable( fWider,fNarrower: TypPtr): boolean;
var lModNr:integer;
begin
 TypReachable:= true;
 if ( fWider^.TypSort <> ikTypMode )
    or ( fNarrower^.TypSort <> ikTypMode ) then exit;
 if fWider^.ModNr = gBuiltIn[ rqAny] then exit;
 lModNr:= fWider^.ModNr;
 with ConstrTypPtr( Constr[ coMode].At( lModNr))^ do
  if fWhichConstrNr <> 0 then lModNr := fWhichConstrNr;
 while (fNarrower^.TypSort = ikTypMode) and (fNarrower^.ModNr >= lModNr) do
 begin
  if (fNarrower^.ModNr = lModNr) or
      (OriginalNr( coMode, fNarrower^.ModNr)=lModNr) then exit;
  with ConstrTypPtr( Constr[ coMode].At( fNarrower^.ModNr))^ do
   fNarrower:= fConstrTyp;
 end;
 TypReachable:= false;
end;

function TypObj.WideningOf(SourceTyp: TypPtr): TypPtr;
 { Wymaga dysponowalnego argumentu i daje wynik dysponowalny.
   Nie zajmuje sie atrybutami.
 }
 var lTrmList:TrmList; lTypPtr1,lTypPtr2: TypPtr;
     z,lModNr: integer;
begin WideningOf:=nil;
 case TypSort of
  ikTypMode:
   begin
    AdjustTyp(lModNr,lTrmList);
    lTypPtr1:=SourceTyp;
    while (lTypPtr1^.TypSort = ikTypMode) and (lTypPtr1^.ModNr >= lModNr) do
     begin
      if (lTypPtr1^.ModNr = lModNr) or
         (OriginalNr( coMode,lTypPtr1^.ModNr)=lModNr) then
        begin WideningOf:=lTypPtr1; exit end;
      lTypPtr2:=lTypPtr1^.Widening;
      dispose(lTypPtr1,Done);
      lTypPtr1:=lTypPtr2;
     end;
//!!!set - poczatek: wyrzucic przypadek z gBuiltIn[rqSetMode]
    if (lTypPtr1^.TypSort = ikTypStruct) and (lModNr=gBuiltIn[rqSetMode])
     then WideningOf:=new(TypPtr,Init(ikTypMode,
                                      NewEmptyCluster,NewEmptyCluster,
                                      gBuiltIn[rqSetMode],Nil));
//!!!set - koniec wyrzucic przypadek z gBuiltIn[rqSetMode]
    if (lTypPtr1^.TypSort = ikTypStruct) and (lModNr=gBuiltIn[rqAny])
//     then WideningOf:=CopyType;
     then WideningOf:=new(TypPtr,Init(ikTypMode,
                                      NewEmptyCluster,NewEmptyCluster,
                                      gBuiltIn[rqAny],Nil));
    dispose(lTypPtr1,Done);
   end;
  ikTypStruct:
   begin lTypPtr1:=SourceTyp^.WidenToStruct;
    dispose(SourceTyp,Done);
    if lTypPtr1 = nil then exit;
    if ModNr=lTypPtr1^.ModNr then begin WideningOf:=lTypPtr1; exit end;
    with StructConstrPtr( Constr[ coStructMode].Items^[ ModNr])^ do
     if fFields^.Count = 0 then exit
     else if fFields^.IsSubsetOf(StructConstrPtr( Constr[ coStructMode].Items^[
                                                   lTypPtr1^.ModNr])^.fFields^)
     then
     begin gWidStruct.Init(0,5);
      gWidStructFound:=false; gTargetStructNr:=ModNr;
      WidenningPath(lTypPtr1^.ModNr);
      if gWidStructFound then
       begin
        with gWidStruct do
         for z:=0 to Count-1 do
          begin
           lTypPtr2:=TypPtr(Items^[z])^.InstTyp(lTypPtr1^.ModArgs);
           dispose(lTypPtr1,Done);
           lTypPtr1:=lTypPtr2;
          end;
        WideningOf:=lTypPtr1;
        gWidStruct.DeleteAll; gWidStruct.Done; exit;
       end;
      gWidStruct.DeleteAll; gWidStruct.Done;
     end;
    dispose(lTypPtr1,Done);
   end;
  ikError:;
  else
begin
{$IFDEF MDEBUG}
InfoChar(TypSort);
{$ENDIF}
    RunTimeError(2058);
end;
  end;
end;

function TypObj.Widening: TypPtr;
 var lTyp:TypPtr; lClusterPtr:AttrCollectionPtr;
begin Widening:=nil;
 case TypSort of
  ikTypMode:
   begin
    if ModNr = gBuiltIn[rqAny] then exit;
    with ConstrTypPtr( Constr[ coMode].At( ModNr))^ do
     lTyp:= fConstrTyp^.InstTyp(ModArgs);
    lClusterPtr:=new(AttrCollectionPtr,CopyAllowed(@Self,UpperCluster));
    dispose(lTyp^.UpperCluster,Done);
    lTyp^.UpperCluster:=lClusterPtr;
   end;
  ikTypStruct: lTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                            gBuiltIn[rqSetMode],nil);
//  ikTypStruct: lTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
//                            gBuiltIn[rqAny],nil);
//!!!set powyzej powinno byc gBuiltIn[rqAny] zamiast gBuiltIn[rqSetMode]
  else RunTimeError(2223);
 end;
 Widening:=lTyp;
end;

function TypObj.IsWiderThan(fTypPtr: TypPtr): boolean;
 var lModNr: integer; lTrmList:TrmList;
     lTypPtr:TypPtr;
 label 1;
begin IsWiderThan:=false;
 if fTypPtr^.DecreasingAttrs(@Self,EqAttr) then
// begin fTypPtr:=WideningOf(fTypPtr);
//  if fTypPtr = nil then exit;
//  IsWiderThan:=EqRadices(fTypPtr);
// end;
 case TypSort of
  ikTypMode:
   begin AdjustTyp(lModNr,lTrmList);
//    if lModNr = gBuiltIn[rqAny] then begin IsWiderThan:=true; goto 1 end;
    while (fTypPtr^.TypSort = ikTypMode) and (fTypPtr^.ModNr >= lModNr) do
     begin
      if EqRadices(fTypPtr) then begin IsWiderThan:=true; goto 1 end;
//    Uwaga: tutaj dziala inaczej niz w WideningOf
      lTypPtr:=fTypPtr^.Widening;
      dispose(fTypPtr,Done);
      fTypPtr:=lTypPtr;
     end;
//!!!set - poczatek: wyrzucic przypadek z gBuiltIn[rqSetMode]
    if (fTypPtr^.TypSort = ikTypStruct) and (lModNr=gBuiltIn[rqSetMode]) then
     IsWiderThan:=true;
//!!!set - koniec wyrzucic przypadek z gBuiltIn[rqSetMode]
    if (fTypPtr^.TypSort = ikTypStruct) and (lModNr=gBuiltIn[rqAny]) then
     IsWiderThan:=true;
   end;
  ikTypStruct:
   begin
    fTypPtr:=WideningOf(fTypPtr);
    if fTypPtr = nil then exit;
    if EqRadices(fTypPtr) then IsWiderThan:=true;
   end;
 end;
1:
 dispose(fTypPtr,Done);
end;

function EsTrmList(fTrm1,fTrm2:TrmList):boolean;
begin
 EsTrmList:=EquateTrmsLists(fTrm1,fTrm2,EsTrm);
end;

function EsAttr(fAttr1, fAttr2: AttrPtr): boolean;
  var lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 fAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 fAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 EsAttr := (lAttrNr1 = lAttrNr2) and
           (fAttr1^.fNeg = fAttr2^.fNeg) and
           EsTrmList(lArgs1,lArgs2);
end;

function EsAttrRev(fAttr1, fAttr2: AttrPtr): boolean;
begin
 EsAttrRev:=EsAttr(fAttr2,fAttr1);
end;

function EsTyp(fTyp,aTyp: TypPtr):boolean;
 var aModNr: integer; A: TrmList;
begin EsTyp:=false;
 with fTyp^ do
 if TypSort = aTyp^.TypSort then
//  if not EqualClusters(fTyp,aTyp,EsAttr) then exit;
  if LowerCluster^.IsSubsetOf(aTyp^.UpperCluster,EsAttrRev) and
     aTyp^.LowerCluster^.IsSubsetOf(UpperCluster,EsAttr) then
  begin
    if ModNr=aTyp^.ModNr then
     begin EsTyp:=EsTrmList(ModArgs,aTyp^.ModArgs); exit end;
    case TypSort of
     ikTypMode:
      begin
       aTyp^.AdjustTyp(aModNr,A);
       if ModNr=aModNr then EsTyp:=EsTrmList(ModArgs,A);
      end;
     ikTypStruct: ;
     else RunTimeError(2102);
    end;
  end;
end;

function EsFrm( fFrm,aFrm:FrmPtr ): boolean;
 var aPredNr,k: integer; A: TrmList;
begin EsFrm:=false;
 SkipLocPred(fFrm);
 SkipLocPred(aFrm);
 if fFrm^.FrmSort=aFrm^.FrmSort then
 case fFrm^.FrmSort of
  ikFrmVerum,ikFrmThesis: EsFrm:=true;
  ikFrmNeg: EsFrm:=EsFrm(NegFrmPtr(fFrm)^.NegArg,NegFrmPtr(aFrm)^.NegArg);
  ikFrmQual:
   if EsTrm(QualFrmPtr(fFrm)^.QualTrm,QualFrmPtr(aFrm)^.QualTrm) then
    EsFrm:=EsTyp(QualFrmPtr(fFrm)^.QualTyp,QualFrmPtr(aFrm)^.QualTyp);
  ikFrmConj:
   with ConjFrmPtr(fFrm)^.Conjuncts do
    if Count = ConjFrmPtr(aFrm)^.Conjuncts.Count then
     begin
      for k:=0 to Count-1 do
       if not EsFrm(FrmPtr(Items^[k]),FrmPtr(ConjFrmPtr(aFrm)^.Conjuncts.Items^[k])) then
        exit;
      EsFrm:=true;
     end;
  ikFrmSchPred,ikFrmPrivPred:
   if PredFrmPtr(fFrm)^.PredNr=PredFrmPtr(aFrm)^.PredNr then
    EsFrm:=EsTrmList(PredFrmPtr(fFrm)^.PredArgs,PredFrmPtr(aFrm)^.PredArgs);
  ikFrmAttr:
   begin
    if PredFrmPtr(fFrm)^.PredNr=PredFrmPtr(aFrm)^.PredNr then
     begin
      EsFrm:=EsTrmList(PredFrmPtr(fFrm)^.PredArgs,PredFrmPtr(aFrm)^.PredArgs);
      exit;
     end;
    AdjustAttrFrm(PredFrmPtr(aFrm),aPredNr,A);
    if PredFrmPtr(fFrm)^.PredNr=aPredNr then
     EsFrm:=EsTrmList(PredFrmPtr(fFrm)^.PredArgs,A);
   end;
  ikFrmPred:
   begin
    if PredFrmPtr(fFrm)^.PredNr=PredFrmPtr(aFrm)^.PredNr then
     begin
      EsFrm:=EsTrmList(PredFrmPtr(fFrm)^.PredArgs,PredFrmPtr(aFrm)^.PredArgs);
      exit;
     end;
    AdjustFrm(PredFrmPtr(aFrm),aPredNr,A);
    if PredFrmPtr(fFrm)^.PredNr=aPredNr then
     EsFrm:=EsTrmList(PredFrmPtr(fFrm)^.PredArgs,A);
   end;
  ikFrmUniv:
   if EsTyp(UnivFrmPtr(fFrm)^.Quantified,UnivFrmPtr(aFrm)^.Quantified) then
    EsFrm:=EsFrm(UnivFrmPtr(fFrm)^.Scope,UnivFrmPtr(aFrm)^.Scope);
  else RunTimeError(2103);
 end;
end;

function EsTrm(fTrm,aTrm:TrmPtr):boolean;
 var aFuncNr,fFuncNr,i: integer; A1,A2: TrmList;
begin
 with TrmPtr(fTrm)^ do
  begin
  if TrmSort=ikTrmLocus then
   with VarTrmPtr(fTrm)^ do
   begin
    if gSubstTrm[VarNr]=nil then
     begin EsTrm:=true; gSubstTrm[VarNr]:=CopyTerm(aTrm); exit end;
    if gSubstTrm[VarNr]^.TrmSort=ikTrmQua then
     begin EsTrm:=EqTrm(aTrm,QuaTrmPtr(gSubstTrm[VarNr])^.TrmProper);
      exit;
     end;
    EsTrm:=true;
    if not EqTrm(aTrm,gSubstTrm[VarNr]) then EsTrm:=false;
    exit;
   end;
  EsTrm:=false;
  if TrmSort=TrmPtr(aTrm)^.TrmSort then
   case TrmSort of
   ikTrmBound,ikTrmConstant,ikTrmFreeVar,ikTrmLambdaVar,ikTrmEqConst,
   ikTrmInfConst,ikTrmNumeral:
    with VarTrmPtr(fTrm)^ do
     EsTrm:=VarNr=VarTrmPtr(aTrm)^.VarNr;
   ikTrmFunctor:
    with FuncTrmPtr(fTrm)^ do
     begin
      if FuncNr=FuncTrmPtr(aTrm)^.FuncNr then
       begin EsTrm:=EsTrmList(FuncArgs,FuncTrmPtr(aTrm)^.FuncArgs); exit end;
      AdjustTrm(fTrm,fFuncNr,A1);
      AdjustTrm(aTrm,aFuncNr,A2);
      if fFuncNr=aFuncNr then EsTrm:=EsTrmList(A1,A2);
     end;
   ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmSelector:
    with FuncTrmPtr(fTrm)^ do
     if FuncNr=FuncTrmPtr(aTrm)^.FuncNr then
      EsTrm:=EsTrmList(FuncArgs,FuncTrmPtr(aTrm)^.FuncArgs);
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
     if FraenkelTrmPtr(aTrm)^.LambdaArgs.Count = LambdaArgs.Count then
      begin
       for i:=0 to LambdaArgs.Count-1 do
        if not EsTyp(LambdaArgs.Items^[i],FraenkelTrmPtr(aTrm)^.LambdaArgs.Items^[i]) then
         exit;
       if EsTrm(LambdaScope,FraenkelTrmPtr(aTrm)^.LambdaScope) then
        EsTrm:=EsFrm(Compr,FraenkelTrmPtr(aTrm)^.Compr);
      end;
   ikTrmChoice:
     with ChoiceTrmPtr(fTrm)^ do
      EsTrm:=EsTyp(ChoiceTyp,ChoiceTrmPtr(aTrm)^.ChoiceTyp);
   ikTrmIt: EsTrm:=true;
   ikError: ;
   else
    begin
{$IFDEF MDEBUG}
InfoChar(TrmSort);
{$ENDIF}
      RunTimeError(2104);
    end;
   end
   else if TrmPtr(aTrm)^.TrmSort = ikTrmInfConst then
     EsTrm:=EsTrm(fTrm,ConstDefPtr(InferConstDef.Items^[VarTrmPtr(aTrm)^.VarNr])^.fDef)
   else if TrmPtr(fTrm)^.TrmSort = ikTrmInfConst then
     EsTrm:=EsTrm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm)^.VarNr])^.fDef,aTrm)
   else if (TrmPtr(fTrm)^.TrmSort = ikTrmPrivFunc) or (TrmPtr(aTrm)^.TrmSort = ikTrmPrivFunc) then
    begin
     while fTrm^.TrmSort=ikTrmPrivFunc do
      if LocFuncTrmPtr(fTrm)^.FuncExp^.TrmSort=ikError
       then exit
      else fTrm:=LocFuncTrmPtr(fTrm)^.FuncExp;
     while aTrm^.TrmSort=ikTrmPrivFunc do
      if LocFuncTrmPtr(aTrm)^.FuncExp^.TrmSort=ikError
       then exit
      else aTrm:=LocFuncTrmPtr(aTrm)^.FuncExp;
     EsTrm:=EsTrm(fTrm,aTrm);
    end;
 end;
end;

function ReconSelectTrm(fSelector:integer; fLastArg:TrmPtr; fTyp:TypPtr):TrmPtr;
 var lMotherStructNr:integer; lMotherStruct: TypPtr;
     lTrmList,lTrmList1:TrmList;
begin Mizassert(2554,fTyp^.TypSort=ikTypStruct); ReconSelectTrm:=nil;
 lMotherStructNr:=MotherStructNr(fSelector);
 if lMotherStructNr = 0 then exit;
 lMotherStruct:=NewStandardTyp(ikTypStruct,NewEmptyCluster,NewEmptyCluster,
                                lMotherStructNr,nil);
 fTyp:=lMotherStruct^.WideningOf(fTyp^.CopyType);
{W blednych tekstach moze sie zdarzyc, ze fTyp=nil,
 trzeba to jeszcze dokladniej przeanalizowac, ale jezeli jest zgloszony
 errFieldTypeInconsistent (#92), to co wlasciwie mozna zrobic ?
 Nim zrobi sie cos madrzejszego, (jezeli) to wyrzucamy asercje.
 A moze to idzie raczej o blad #93, nie obsluzpny prefiks.
}
 if fTyp = nil then exit;
 if fTyp^.ModArgs = nil then lTrmList:=NewTrmList(fLastArg,nil) else
  begin lTrmList:=CopyTermList(fTyp^.ModArgs);
   lTrmList1:=lTrmList;
   while lTrmList1^.NextTrm <> nil do lTrmList1:=lTrmList1^.NextTrm;
   lTrmList1^.NextTrm:=NewTrmList(fLastArg,nil);
  end;
 dispose(fTyp,Done); dispose(lMotherStruct,Done);
 ReconSelectTrm:=NewLocFuncTrm(ikTrmSelector,fSelector,lTrmList);
end;

function ReconAggregTrm(fStruct:integer; fLastArg:TrmPtr; fTyp:TypPtr):TrmPtr;
var lTrmElem:TrmElem; lTrmList:TrmList;
     lAggrNr,z:integer;
     lTyp,lTyp1:TypPtr;
     lErr: boolean;
     llTrm:TrmPtr;
begin
 lErr:=false;
 with StructConstrPtr( Constr[ coStructMode].Items^[fStruct])^ do
 begin
  if fFields^.Count = 0 then
  begin ReconAggregTrm:=NewIncorTrm; exit end;
  lAggrNr:= fStructModeAggrNr;
 end;
 { odciety funktor agregujacy }
 if lAggrNr = 0 then begin ReconAggregTrm:=NewIncorTrm; exit end;
 { ! niepoprawna konstrukcja, w ogole rozszerzanie jest nie
    do typu ale do modu, wiec fakt, ze ten typ jest niepoprawny nie
    ma znaczenia.
  }
 lTyp1:=NewStandardTyp(ikTypStruct,NewEmptyCluster,NewEmptyCluster,fStruct,nil);
 lTyp:=lTyp1^.WideningOf(fTyp);
 if lTyp = nil then begin ReconAggregTrm:=NewIncorTrm; exit end;
 lTrmList:=addr(lTrmElem);
 lTrmElem.NextTrm:=CopyTermList(lTyp^.ModArgs);
 while lTrmList^.NextTrm <> nil do lTrmList:=lTrmList^.NextTrm;
 dispose(lTyp1,Done);
 with AggrConstrPtr( Constr[ coAggregate].At( lAggrNr))^.fAggrColl^ do
  for z:=0 to Count-1 do
   begin
    llTrm:=ReconSelectTrm(PIntItem(Items^[z])^.IntKey,CopyTerm(fLastArg),lTyp);
    if llTrm = nil then lErr:=true; {errors recovery}
    lTrmList^.NextTrm:= NewTrmList(llTrm,nil);
    if ConstrTypPtr( Constr[ coSelector].Items^[PIntItem(Items^[z])^.IntKey]
                      )^.fConstrTyp^.TypSort = ikError
    then lErr:=true;
    lTrmList:=lTrmList^.NextTrm;
   end;
 dispose(lTyp,Done);
 if lErr then ReconAggregTrm:=NewIncorTrm
  else ReconAggregTrm:=NewLocFuncTrm(ikTrmAggreg,lAggrNr,lTrmElem.NextTrm);
end;

function EqualClusters(fTyp1,fTyp2:TypPtr; aEqAttr: EqualAttrs): boolean;
begin
 EqualClusters:=fTyp1^.LowerCluster^.IsSubsetOf(fTyp2^.UpperCluster,aEqAttr) and
  fTyp2^.LowerCluster^.IsSubsetOf(fTyp1^.UpperCluster,aEqAttr);
 { Byc moze nalezy tu przeprowadzic jakies optymizacje, bo
   chyba czesto sie bedzie zdarzac, ze numery beda rowne }
end;

procedure DisposeTrmMList(var aList:MListPtr);
 var i: integer;
begin
 for i:=0 to aList^.Count-1 do
   DisposeTrm(aList^.Items^[i]);
 aList^.DeleteAll;
 dispose(aList,Done);
end;

procedure DisposeTrmList(fTrmList:TrmList);
 var ll:TrmList;
begin
 if fTrmList = InCorrTrmList then exit;
 while fTrmList <> nil do
  begin ll:=fTrmList^.NextTrm;
   if not DisposingSuperclusters then DisposeTrm(fTrmList^.XTrmPtr);
   dispose(fTrmList);
   fTrmList:=ll;
  end;
end;

procedure DisposeListOfTerms(fList:TrmList);
 var lList:TrmList;
begin
 if fList = IncorrTrmList then exit;
 while fList <> nil do
  begin lList:=fList;
   fList:=fList^.NextTrm;
   dispose(lList);
  end;
end;

destructor AttrObj.Done;
begin
   DisposeTrmList(fAttrArgs);
end;

destructor TypObj.Done;
begin
 DisposeTrmList(ModArgs);
 Dispose(LowerCluster,Done);
 Dispose(UpperCluster,Done);
end;

destructor CorrObj.Done;
begin  inherited Done; end;

destructor TrmObj.Done;
begin
 inherited Done;
end;

destructor VarTrmObj.Done;
begin
  case TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral,ikError,ikTrmIt:;
   else
begin
{$IFDEF MDEBUG}
InfoChar(TrmSort);
{$ENDIF}
    RunTimeError(2062);
end;
  end;
 inherited Done;
end;

destructor FuncTrmObj.Done;
begin
   DisposeTrmList(FuncArgs);
end;

destructor LocFuncTrmObj.Done;
begin DisposeTrmList(FuncArgs);
 DisposeTrm(FuncExp);
end;

destructor FraenkelTrmObj.Done;
begin
 LambdaArgs.Done; DisposeTrm(LambdaScope); dispose(Compr,Done); nIdents.Done;
end;

destructor QuaTrmObj.Done;
begin
 DisposeTrm(TrmProper); dispose(Qua,Done);
end;

destructor ChoiceTrmObj.Done;
begin
 dispose(ChoiceTyp,Done);
end;

procedure DisposeTrm(fTrm:TrmPtr);
begin
 if (fTrm <> nil) and (fTrm^.TrmSort <> ikTrmEqConst) then dispose(fTrm,Done);
end;

constructor ConstDefObj.Init(aDef: TrmPtr; aTyp: TypPtr);
begin
 fDef:=aDef;
 fEqConst.Init(0,4);
 nSetting:=-1;
 fTyp:=aTyp;
 fDetermined:=false;
end;

destructor ConstDefObj.Done;
begin
 DisposeTrm(fDef);
 dispose(fTyp,Done);
 fEqConst.Done;
end;

procedure ExpPrivFuncInTrmList(fTrmList:TrmList);
 var i:integer;
begin
 for i:=1 to MaxElemNbr do
  if fTrmList=nil then exit
  else with fTrmList^ do
    begin ExpPrivFuncInTrm(XTrmPtr); fTrmList:=NextTrm end;
 RunTimeError(2818);
end;

procedure ExpPrivFuncInAttrColl(aClu: AttrCollectionPtr);
 var i: integer;
begin
 with aClu^ do
  for i:=0 to Count-1 do
   ExpPrivFuncInTrmList(AttrPtr(Items^[i])^.fAttrArgs);
end;

procedure ExpPrivFuncInTyp(fTyp:TypPtr);
begin
 with TypPtr(fTyp)^ do
 begin
  ExpPrivFuncInAttrColl(LowerCluster);
  ExpPrivFuncInAttrColl(UpperCluster);
  case TypSort of ikTypMode,ikTypStruct: ExpPrivFuncInTrmList(ModArgs) end;
 end;
end;

procedure ExpPrivFuncInTrm(var fTrm:TrmPtr);
  var z: integer;
      lTrm:TrmPtr;
      lFrm: FrmPtr;
begin
 with TrmPtr(fTrm)^ do
  case TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmEqConst,ikTrmFreeVar,
    ikTrmLambdaVar,ikTrmIt: ;
   ikTrmNumeral,ikError: ;
   ikTrmSchFunc,ikTrmAggreg,ikTrmFunctor,ikTrmSelector: ExpPrivFuncInTrmList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmPrivFunc:
    begin
     DisposeTrmList(FuncTrmPtr(fTrm)^.FuncArgs);
     fTrm:=LocFuncTrmPtr(fTrm)^.FuncExp;
     ExpPrivFuncInTrm(fTrm);
    end;
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
     begin
      with LambdaArgs do
       for z:=0 to Count-1 do
        ExpPrivFuncInTyp(TypPtr(Items^[z]));
      ExpPrivFuncInTrm(LambdaScope);
      lFrm:=Compr;
      Compr:=CopyExpFrm(Compr);
      dispose(lFrm,Done);
     end;
   ikTrmChoice: ExpPrivFuncInTyp(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
   else
begin
{$IFDEF MDEBUG}
InfoChar(TrmSort);
{$ENDIF}
    RunTimeError(2853);
end;
  end;
end;

function ReductionAllowed(fTrm1,fTrm2:TrmPtr):boolean; //true if fTrm2 is a strict subterm of fTrm1

var aFound: boolean;

function IsSubterm(fTrm1,fTrm2:TrmPtr):boolean; forward;

function IsSubtermList(fTrmList:TrmList;fTrm:TrmPtr):boolean;
var lList:TrmList;
var lTermFound:boolean;
begin
 if aFound then begin IsSubtermList:=true; exit; end;
 lTermFound:=false;
 lList:=fTrmList;
 while (lList <> nil) and not lTermFound do
  begin
   if lList^.XTrmPtr^.TrmSort in [ikTrmChoice,ikTrmFraenkel] then
    begin     
     Error(CurPos,258); //new error
     break;  
    end;
   lTermFound:=EqTrm(lList^.XTrmPtr,fTrm);
   if lTermFound then
    begin
     aFound:=true;
     break;
    end
   else IsSubterm(lList^.XTrmPtr,fTrm);
   lList:=lList^.NextTrm;
  end;
 IsSubtermList:=aFound;
end;

function IsSubterm(fTrm1,fTrm2:TrmPtr):boolean; //true if fTrm2 is a strict subterm of fTrm1
var temp:boolean;
begin
 if aFound then begin IsSubterm:=true; exit; end;
 IsSubterm:=false;
 while fTrm1^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm1)^.FuncExp^.TrmSort=ikError then break
  else fTrm1:=LocFuncTrmPtr(fTrm1)^.FuncExp;
 while fTrm2^.TrmSort=ikTrmPrivFunc do
  if LocFuncTrmPtr(fTrm2)^.FuncExp^.TrmSort=ikError then break
  else fTrm2:=LocFuncTrmPtr(fTrm2)^.FuncExp;
 case fTrm1^.TrmSort of
  ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmEqConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral,ikTrmInfConst : 
   begin
    temp:=EqTrm(fTrm1,fTrm2);
    if temp then aFound:=true;
    IsSubterm:=temp;
   end;
  ikTrmFunctor: IsSubterm:=IsSubtermList(FuncTrmPtr(fTrm1)^.FuncArgs,fTrm2);
  ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmSelector: IsSubterm:=IsSubtermList(FuncTrmPtr(fTrm1)^.FuncArgs,fTrm2);
  ikTrmChoice: ;
  ikTrmFraenkel,ikTrmIt,ikError: ;//
  else RunTimeError(2052); // new error
 end;
end;

var FuncNr1: integer; A1: TrmList;

begin
 ReductionAllowed:=false;
 if fTrm1^.TrmSort <> ikTrmFunctor then exit;
 aFound:=false;
 AdjustTrm(fTrm1,FuncNr1,A1);
 ReductionAllowed:=IsSubtermList(A1,fTrm2);
end;

end.
