(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit inoutmml;

interface

uses mobjects,limits,envhan,inout,identify,lexicon,
     builtin,formats,dicthan,correl,iocorrel,enums;

type
   ConstrIntSetArr = array[ConstructorsKind] of NatSet;
   ConstrIntFuncArr = array[ConstructorsKind] of NatFunc;
   ConstrMListArr = array[ConstructorsKind] of MList;

{from inlibr ... needed for transferer too}
{ the name of this object should be changed}
  ImpConstrPtr = ^ImpConstrObj;
  ImpConstrObj = Object(MStrObj)
      nBase: ConstrIntArr;
    constructor Init(fIdent:string);
  end;

 TheoremPtr =  ^ TheoremObj;

 ConstrNbrPtr = ^ConstrNbrObj;
 ConstrNbrObj = object(MObject)
     fConstrNbr: ConstrIntArr;
   constructor Init;
 end;

 TheoremObj = object(MObject)
   fTheoKind: Char;
   fTheoNr: word;
   fDefConstr: Lexem;  // defined constr. for definitional, otherwise ikError
   fTheorem: FrmPtr;
  constructor Init(aKind: char; aNr: word; aConstr: Lexem; aTheo: FrmPtr);
  destructor Done; virtual;
 end;

{ Marked should be changed to return integer}  
 ConstrCounterObj = object(MObject)
     fNbr: ConstrIntFuncArr;
    constructor Init;
    destructor Done; virtual;
    procedure Prune; virtual; 
    procedure Reset; virtual;
    function CountAll: integer; virtual;
    procedure AddConstr( const aCounter: ConstrCounterObj); virtual;
    function  Marked(fKind:ConstructorsKind; fNr:integer):boolean;
    procedure MarkTrm(fTrm: TrmPtr); virtual;
    procedure MarkTrmList(fTrmList:TrmList); virtual;
    procedure MarkAttributes(aCluster: AttrCollectionPtr); virtual;
    procedure MarkTyp(fTyp: TypPtr); virtual;
    procedure MarkTypeList(const fTypList: MList); virtual;
    procedure MarkTypColl(const fTypList: MList); virtual;
    procedure MarkFrm(fFrm:FrmPtr); virtual;
    procedure MarkFrmColl(const fFrmList: MCollection); virtual;
    procedure MarkClusterObj(fCl:ClusterPtr); virtual;
    procedure MarkIdentifyObj(fId:IdentifyPtr); virtual;
    procedure MarkReductionObj(fId:ReductionPtr); virtual;
    procedure MarkPropertyObj(fId:PropertyPtr); virtual;
    procedure MarkPatternObj(fPatt:PatternPtr); virtual;
    procedure MarkConstrDefObj(fDef:ConstrDefPtr); virtual;
    procedure MarkDefiniensObj(fDef:DefiniensPtr); virtual;
    procedure MarkConstr(fConstr:ConstrPtr); virtual;
 end;

{ we mark  types od consts here too, to have marked really all consts}
StatsObj = object(ConstrCounterObj)
    fVarNbr: NatFunc;
    constructor Init;
    destructor Done; virtual;
    procedure Prune; virtual; 
    procedure Reset; virtual;
    function CountAll: integer; virtual;
    procedure AddStats( const aStats: StatsObj); virtual;
    procedure ResetConstrs;
    function  VarMarked(fNr:integer):boolean;
    function  AnyVarMarked:boolean;
    procedure MarkTrm(fTrm: TrmPtr); virtual;
 end; 

{ not used yet}
const XMLTermSymbols = [ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmNumeral,ikTrmSchFunc,'K','G','U','L','I','Q',ikError];
const XMLFrmSymbols = ['N',ikFrmConj,ikFrmSchPred,'V',ikFrmPred,'U','Q',ikFrmVerum,ikFrmThesis,ikError];

function XMLFrm (fKind	: char): char;
function InternFrm(fKind	: char): char;
function XMLTerm(fKind	: char): char;
function InternTerm(fKind	: char): char;
{ this temporarily says to MarkType if it works with MMlType or not; it
will be removed when clusters are pointers}

var

  gImpSgnNames: MList; {was in outlibr}
{put here from xmlmml, used for Transf in originl procs
for debugging}
gConstrNbr: ConstrIntArr;

{ moved from inlibr, because used in transfere too}
gMarked: ConstrCounterObj; {array[ConstructorsKind] of NatSet;}
 ImpConstr:array [1..MaxImpConstrNbr+1] of ImpConstrPtr;
 ImpConstrNbr,ConstructorsBase: integer;

procedure UnexpectedKind(fGot:char; fErr:integer;
                           fExpected: TCharSet);
procedure UnexpectedNumber(fGot:integer; fErr:integer;
                           fExpected: integer);

implementation

uses {$IFDEF MDEBUG} info, outinfo, {$ENDIF}
 mizenv,librenv,errhan,mscanner;

{+-------------------------------------------------------------------+}

procedure UnexpectedKind(fGot:char; fErr:integer;
                           fExpected: TCharSet);
var c:char;
begin
 {$IFDEF MDEBUG}
 InfoNewLine;
 InfoString('Unexpected token:'+ InternKindName(fGot)+'; expected:');
 for c := Low(char) to High(char) do
  if c in fExpected then InfoString(InternKindName(c)+',');
 {$ENDIF}
 RunTimeError(fErr);
end; 

procedure UnexpectedNumber(fGot:integer; fErr:integer;
                           fExpected: integer);
begin
 {$IFDEF MDEBUG}
 InfoNewLine;
 InfoString('Unexpected number:'+ IntToStr(fGot)+'; expected:'
           + IntToStr(fExpected));
 {$ENDIF}
 RunTimeError(fErr);
end; 

{ this might become some enumerated type later}
function XMLFrm(fKind	: char): char;
begin
  case fKInd of
  ikFrmNeg	 : XMLFrm:= 'N';
  ikFrmUniv : XMLFrm:= 'U';
  ikFrmQual : XMLFrm:= 'Q';
  else XMLFrm:= fKind;
  end; { case }
end;

function InternFrm(fKind : char): char;
begin
  case fKInd of
  'N' : InternFrm:= ikFrmNeg;
  'U' : InternFrm:= ikFrmUniv;
  'Q' : InternFrm:= ikFrmQual;
  else InternFrm:= fKind;
  end; { case }
end;

function XMLTerm(fKind : char): char;
begin
  case fKInd of
  ikTrmFraenkel	 : XMLTerm:= 'L';
  ikTrmIt : XMLTerm:= 'I';
  ikTrmQua : XMLTerm:= 'Q';
  else XMLTerm:= fKind;
  end; { case }
end;

function InternTerm(fKind : char): char;
begin
  case fKInd of
  'L' : InternTerm:= ikTrmFraenkel;
  'I' : InternTerm:= ikTrmIt;
  'Q' : InternTerm:= ikTrmQua;
  else InternTerm:= fKind;
  end; { case }
end;

{+-------------------------------------------------------------------+}

constructor ImpConstrObj.Init(fIdent:string);
var c : ConstructorsKind;
begin
 inherited Init(fIdent);
 for c := Low(ConstructorsKind) to High(ConstructorsKind) do
     nBase[c] := 0;
end;

{+-------------------------------------------------------------------+}
{+-------------------------------------------------------------------+}
{+-------------------------------------------------------------------+}

constructor ConstrCounterObj.Init;
 var c: ConstructorsKind;
begin
  for c:=coMode to coAggregate do fNbr[c].InitNatFunc(0,100);
end;

destructor ConstrCounterObj.Done;
 var c: ConstructorsKind;
begin
  for c:=coMode to coAggregate do fNbr[c].Done;
end;

procedure ConstrCounterObj.Prune;
 var c: ConstructorsKind;
begin
  for c:=coMode to coAggregate do fNbr[c].SetLimit( fNbr[c].Count);
end;

procedure ConstrCounterObj.Reset;
 var c: ConstructorsKind;
begin
  for c:=coMode to coAggregate do fNbr[c].DeleteAll;
end;

function ConstrCounterObj.CountAll: integer;
var c: ConstructorsKind; l:integer;
begin
 l:= 0;
 for c:=coMode to coAggregate do inc( l, fNbr[c].CountAll);
 CountAll:= l;
end;

procedure ConstrCounterObj.AddConstr( const aCounter: ConstrCounterObj);
var c: ConstructorsKind;
begin
  for c:=coMode to coAggregate do fNbr[c].Add( aCounter.fNbr[c]);
end;

function ConstrCounterObj.Marked(fKind:ConstructorsKind; fNr:integer):boolean;
begin 
  Marked := fNbr[fKind].HasInDom(fNr);
end;

procedure ConstrCounterObj.MarkTrmList(fTrmList:TrmList);
begin
 while fTrmList <> nil do
  begin
   MarkTrm(fTrmList^.XTrmPtr);
   fTrmList:=fTrmList^.NextTrm;
  end;
end;

procedure ConstrCounterObj.MarkTypeList(const fTypList: MList);
  var z: integer;
begin
 with fTypList do for z:=0 to Count-1 do
  MarkTyp(TypPtr(Items^[z]));
end;

procedure ConstrCounterObj.MarkTypColl(const fTypList: MList);
  var z: integer;
begin
 with fTypList do for z:=0 to Count-1 do
  MarkTyp(TypPtr(Items^[z]));
end;

procedure ConstrCounterObj.MarkAttributes(aCluster: AttrCollectionPtr);
 var i: integer;
begin
 with aCluster^ do
 for i:=0 to Count-1 do
  begin
   fNbr[coAttribute].Up(AttrPtr(Items^[i])^.fAttrNr);
   MarkTrmList(AttrPtr(Items^[i])^.fAttrArgs)
  end;
end;

procedure ConstrCounterObj.MarkTyp(fTyp: TypPtr);
begin
 MarkAttributes(fTyp^.LowerCluster);
 with fTyp^ do
   case TypSort of
   ikTypMode: begin fNbr[coMode].Up(ModNr); MarkTrmList(ModArgs) end;
   ikTypStruct: begin fNbr[coStructMode].Up(ModNr); MarkTrmList(ModArgs) end;
   end;
end;

procedure ConstrCounterObj.MarkFrmColl(const fFrmList: MCollection);
   var z: integer;
begin
 with fFrmList do for z:=0 to Count-1 do MarkFrm(FrmPtr(Items^[z]));
end;

procedure ConstrCounterObj.MarkFrm(fFrm:FrmPtr);
begin
   case FrmPtr(fFrm)^.FrmSort of
    ikFrmNeg: MarkFrm(NegFrmPtr(fFrm)^.NegArg);
    ikFrmConj: MarkFrmColl(ConjFrmPtr(fFrm)^.Conjuncts);
    ikFrmSchPred: MarkTrmList(PredFrmPtr(fFrm)^.PredArgs);
    ikFrmPrivPred:
     begin
      MarkTrmList( LocPredFrmPtr(fFrm)^.PredArgs);
      MarkFrm( LocPredFrmPtr(fFrm)^.PredExp);
     end;
    ikFrmAttr:
     begin fNbr[coAttribute].Up(PredFrmPtr(fFrm)^.PredNr);
      MarkTrmList(PredFrmPtr(fFrm)^.PredArgs)
     end;
    ikFrmPred:
     begin fNbr[coPredicate].Up(PredFrmPtr(fFrm)^.PredNr);
      MarkTrmList(PredFrmPtr(fFrm)^.PredArgs)
     end;
    ikFrmUniv:
     begin MarkTyp(UnivFrmPtr(fFrm)^.Quantified);
      MarkFrm(UnivFrmPtr(fFrm)^.Scope)
     end;
    ikFrmQual:
     begin MarkTrm(QualFrmPtr(fFrm)^.QualTrm);
      MarkTyp(QualFrmPtr(fFrm)^.QualTyp)
     end;
    ikFrmFlexConj:
     with FlexFrmPtr(fFrm)^ do
      begin MarkFrm(nLeftOrigFrm);
       MarkFrm(nRightOrigFrm);
       MarkFrm(nExpansion);
       MarkTrm(nLeftTrm);
       MarkTrm(nRightTrm);
     end;
    ikFrmVerum,ikError: ;
   else RunTimeError(2499);
   end;
end;

procedure ConstrCounterObj.MarkTrm(fTrm: TrmPtr);
begin
  case TrmPtr(fTrm)^.TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmIt,ikTrmNumeral,ikError: ;
   ikTrmInfConst: with VarTrmPtr(fTrm)^ do
         MarkTrm(ConstDefPtr(InferConstDef.Items^[VarNr])^.fDef);
   ikTrmSchFunc: MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmPrivFunc:
    begin
     MarkTrmList(LocFuncTrmPtr(fTrm)^.FuncArgs);
     MarkTrm(LocFuncTrmPtr(fTrm)^.FuncExp);
    end;
   ikTrmFunctor:
    begin fNbr[coFunctor].Up(FuncTrmPtr(fTrm)^.FuncNr);
     MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs)
    end;
   ikTrmAggreg:
    begin fNbr[coAggregate].Up(FuncTrmPtr(fTrm)^.FuncNr);
     MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs)
    end;
   ikTrmSelector:
    begin fNbr[coSelector].Up(FuncTrmPtr(fTrm)^.FuncNr);
     MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs)
    end;
   ikTrmFraenkel: with FraenkelTrmPtr(fTrm)^ do
    begin
     MarkTypColl(LambdaArgs); MarkTrm(LambdaScope);
     MarkFrm(Compr);
    end;
   ikTrmChoice:
    MarkTyp(ChoiceTrmPtr(fTrm)^.ChoiceTyp); 
  end;
end;

procedure ConstrCounterObj.MarkClusterObj(fCl:ClusterPtr);
begin
  with fCl^ do
  begin
    MarkAttributes(nConsequent.Lower);
    MarkTypColl(nPrimaryList);
    case nClusterKind of
    clRegistered :
       MarkTyp(RClusterPtr(fCl)^.nClusterType);
    clConditional:
      with CClusterPtr(fCl)^ do
      begin
        MarkTyp(nClusterType);
	MarkAttributes(nAntecedent);
      end;
    clFunctor    :
      begin
       MarkTrm(FClusterPtr(fCl)^.nClusterTerm);
       if FClusterPtr(fCl)^.nClusterType <> nil then
        MarkTyp(FClusterPtr(fCl)^.nClusterType);
      end;
    end; { case }
  end;
end;

procedure ConstrCounterObj.MarkIdentifyObj(fId:IdentifyPtr);
begin
  with fId^ do
  begin
    MarkTypColl(nPrimaryList);
    case nConstrKind of
    ikTrmFunctor:
      begin MarkTrm(TrmPtr(nPattern[0])); MarkTrm(TrmPtr(nPattern[1])) end;
    ikFrmAttr,ikFrmPred:
      begin MarkFrm(FrmPtr(nPattern[0])); MarkFrm(FrmPtr(nPattern[1])) end;
    end;
  end;
end;

procedure ConstrCounterObj.MarkReductionObj(fId:ReductionPtr);
begin
 with fId^ do
 begin
  MarkTypColl(nPrimaryList);
  MarkTrm(TrmPtr(nTerms[0]));
  MarkTrm(TrmPtr(nTerms[1]));
 end;
end;

procedure ConstrCounterObj.MarkPropertyObj(fId:PropertyPtr);
begin
  with fId^ do
  begin
    MarkTypColl(nPrimaryList);
//    nObject: PObject;
    case PropertyKind(nPropertyKind) of
    sySethood:
     MarkTyp(TypPtr(nObject));
    end;
  end;
end;

procedure ConstrCounterObj.MarkPatternObj(fPatt:PatternPtr);
begin
  with fPatt^ do
  begin
    MarkTypeList(fPrimTypes);
    fNbr[ConstructorKind(rConstr.Kind)].Up(rConstr.Nr);
    if Expansion<>nil then MarkTyp(Expansion);
  end;
end;

procedure ConstrCounterObj.MarkConstrDefObj(fDef:ConstrDefPtr);
var i:integer;
begin
  with fDef^ do
  begin
    fNbr[ConstructorKind(nConstr.Kind)].Up(nConstr.Nr);
    MarkTypColl(PrimaryList);
    case nConstr.Kind of
    ikTrmFunctor: MarkTrm(TrmPtr(nPattern));
    ikFrmAttr,ikFrmPred: MarkFrm(FrmPtr(nPattern));
    end;
  end;
end;

procedure ConstrCounterObj.MarkDefiniensObj(fDef:DefiniensPtr);
var i:integer;
begin
  with fDef^ do
  begin
    fNbr[ConstructorKind(nConstr.Kind)].Up(nConstr.Nr);
    MarkTypColl(PrimaryList);
    MarkFrm(Assumptions);
    with Definiens^,nPartialDefinientia do
    begin
      for i:=0 to Count-1 do
      begin
        case DefSort of
        'm': MarkFrm(FrmPtr(PartDefPtr(Items^[i])^.nPartDefiniens));
        'e': MarkTrm(TrmPtr(PartDefPtr(Items^[i])^.nPartDefiniens));
        end;
	MarkFrm(FrmPtr(PartDefPtr(Items^[i])^.nGuard));
      end;
      if nOtherwise <> nil then
       case DefSort of
       'm': MarkFrm(FrmPtr(nOtherwise));
       'e': MarkTrm(TrmPtr(nOtherwise));
       end;
    end;
  end;
end; { ConstrCounterObj.MarkDefiniensObj }

{ this is used for transferer, it should resemble MarkSgn, but marks prefixes too for structmodes like ProcessTYP; why this difference??}
procedure ConstrCounterObj.MarkConstr(fConstr:ConstrPtr);
var k:integer;
begin
 with fConstr^ do
  begin
   MarkTypeList(nPrimaries);
   if fConstrKind in TypedConstrKinds then
    MarkTyp(ConstrTypPtr(fConstr)^.fConstrTyp);
   case fConstrKind of
   coStructMode :
     with StructConstrPtr(fConstr)^ do
     begin
     { now commented and put to ImpMultConstrObj.Mark}
       MarkTypColl(fPrefixes);
       for k:=0 to fFields^.Count-1 do fNbr[coSelector].Up(fFields^.Items^[k].X);
     end;
   coAggregate  :
     with AggrConstrPtr(fConstr)^ do
     for k:=0 to fAggrColl^.Count-1 do
       fNbr[coSelector].Up(PIntItem(fAggrColl^.Items^[k])^.IntKey);
   end; { case }
   if fWhichConstrNr > 0 then
      fNbr[fConstrKind].Up(fWhichConstrNr);
  end;
end; { ConstrCounterObj.MarkConstr }

{+-------------------------------------------------------------------+}

constructor StatsObj.Init;
begin
  inherited Init;
  fVarNbr.InitNatFunc(0,16);
end;

destructor StatsObj.Done;
begin
  fVarNbr.Done;
  inherited Done;
end;

procedure StatsObj.Prune;
begin
 fVarNbr.SetLimit( fVarNbr.Count);
 inherited Prune;
end;

procedure StatsObj.Reset;
begin
  inherited Reset;
  fVarNbr.DeleteAll;
end;

function StatsObj.CountAll: integer;
begin
 CountAll:= fVarNbr.CountAll + Inherited CountAll;
end;

procedure StatsObj.AddStats( const aStats: StatsObj); 
begin
 AddConstr( aStats);
 fVarNbr.Add( aStats.fVarNbr);
end;

procedure StatsObj.ResetConstrs;
begin ConstrCounterObj.Reset; end;

function StatsObj.VarMarked(fNr:integer):boolean; 
begin VarMarked := fVarNbr.HasInDom(fNr); end; 

function StatsObj.AnyVarMarked:boolean; 
begin AnyVarMarked := (fVarNbr.Count > 0); end;

{ we mark vars too, var defs too}
procedure StatsObj.MarkTrm(fTrm: TrmPtr);
begin
  case TrmPtr(fTrm)^.TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmIt,ikTrmNumeral,ikError: ;
   ikTrmConstant: with VarTrmPtr(fTrm)^ do
   begin fVarNbr.Up(VarNr);
     MarkTyp(FixedVar[VarNr].nTyp);
     if  FixedVar[VarNr].nDef <> nil then
     begin fNbr[coPredicate].Up(gBuiltIn[rqEqualsTo]);
           MarkTrm(FixedVar[VarNr].nDef);
     end;   
   end; 
   ikTrmInfConst: with VarTrmPtr(fTrm)^ do
         MarkTrm(ConstDefPtr(InferConstDef.Items^[VarNr])^.fDef);
   ikTrmSchFunc: MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmPrivFunc:
    begin
     MarkTrmList(LocFuncTrmPtr(fTrm)^.FuncArgs);
     MarkTrm(LocFuncTrmPtr(fTrm)^.FuncExp);
    end;
   ikTrmFunctor:
    begin fNbr[coFunctor].Up(FuncTrmPtr(fTrm)^.FuncNr);
     MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs)
    end;
   ikTrmAggreg:
    begin fNbr[coAggregate].Up(FuncTrmPtr(fTrm)^.FuncNr);
     MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs)
    end;
   ikTrmSelector:
    begin fNbr[coSelector].Up(FuncTrmPtr(fTrm)^.FuncNr);
     MarkTrmList(FuncTrmPtr(fTrm)^.FuncArgs)
    end;
   ikTrmFraenkel: with FraenkelTrmPtr(fTrm)^ do
    begin
     MarkTypColl(LambdaArgs); MarkTrm(LambdaScope);
     MarkFrm(Compr);
    end;
   ikTrmChoice: MarkTyp(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
  end;
end;

{+-------------------------------------------------------------------+}
{+-------------------------------------------------------------------+}
{+-------------------------------------------------------------------+}

constructor ConstrNbrObj.Init;
 var c:ConstructorsKind;
begin
 for c:=low(ConstructorsKind) to high(ConstructorsKind) do
  fConstrNbr[c]:=0;
end;

{+-------------------------------------------------------------------+}

constructor TheoremObj.Init(aKind: char; aNr: word;
                            aConstr: Lexem; aTheo: FrmPtr);
begin
 fTheoKind:= aKind;
 fTheoNr:= aNr;
 fDefConstr:= aConstr;
 fTheorem:= aTheo;
end;

destructor TheoremObj.Done;
begin
 if fTheorem<>nil then fTheorem^.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}
{+-------------------------------------------------------------------+}
{+-------------------------------------------------------------------+}
end.
