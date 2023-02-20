(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit equalizer;

interface

uses mobjects,limits,numbers,correl;

type
 ValRec = record
      Determined: boolean;
      NumericValue: RComplex;
     end;

const
  One: ValRec = (Determined:true;
                 NumericValue:(Re:(Num:'1'; Den:'1'); Im:(Num:'0'; Den:'1')));
  ImUnit: ValRec = (Determined:true;
                 NumericValue:(Re:(Num:'0'; Den:'1'); Im:(Num:'1'; Den:'1')));

var
  TrmS:
    array[1..MaxTrmNbr] of
     record
       Term: TrmPtr;
       NumValue: ValRec;
       PolynomialValues: MSortedCollection;
       EqClass: TrmList;
       XTypClass: MCollection;
       SuperCluster: AttrCollectionPtr;
     end;
  TrmNbr: integer;
  TrmOvfl: boolean;
  Contr: integer;
  EqClassNbr: integer;
  NegBas,PosBas: MCollection;

procedure Equate(var fEval:NatFunc);

procedure DispEquations;
procedure DispEqClassInTrms;

procedure InsertNonEmpty(fTrmInfo1,fTrmInfo2:integer);
procedure InsertNonZero(fTrmInfo1,fTrmInfo2:integer);

procedure SetContr(i: integer);

implementation

uses errhan,builtin,lexicon,enums,identify,polynom,ellipses,prechecker,mscanner
{$IFDEF MINI_PROFILER},miniprof {$ENDIF}
{$IFDEF CH_REPORT},req_info,prephan{$ENDIF}
{$IFDEF MDEBUG},info,outinfo{$ENDIF};

type
  ConCollection =
   object(TIntKeyCollection)
    procedure AllocTerm(fTrm:TrmPtr);
    function FindTerm(fTrm:TrmPtr): TrmPtr;
   end;

  ConstrItem = ^ChConstrObj;
  ChConstrObj =
   object(TIntItem)
     fTerms: MSortedList;
    constructor Init(aTrm:TrmPtr);
    destructor Done; virtual;
   end;

var
  DTrm: MList;
  FuncTrmList: array[FuncTrmExprKind] of ConCollection;
  ChoiceTerm,FrOper: MSortedList;
  clash: boolean;
  AllowedCCluster,AllowedFCluster : MList;
  gDependencies: MList;
  ANEqStack: NatSet;
  
  procedure SetContr(i: integer);
{$IFDEF TEST_CONTR}
 var f_contr: text;
{$ENDIF}
  begin
   Contr:=i;
{$IFDEF TEST_CONTR}
   if i > 0 then
    begin
     assign(f_contr,'contr.txt');
     {$I-}append(f_contr);{$I+}
     if IOResult <> 0 then rewrite(f_contr);
     writeln(f_contr,i);
     close(f_contr);
    end;
{$ENDIF}
  end;
  
{$IFDEF MDEBUG}
procedure InfoEqClass(e: integer);
 var i: integer;
begin
//  writeln(infofile,' -------------------------------');
 with TrmS[e] do
 if EqClass = nil then
 begin writeln(infofile,'Trms[',e,'] = Trms[',Term^.TrmInfo,']');
//  writeln(infofile,'EqClass=nil');
 end
 else
 begin write(infofile,'Trms[',e,'] ');
//  writeln(infofile,'EqClassNr=',Term^.TrmInfo);
  infoterm(Term);
  infonewline;
  with NumValue do
  begin
    if  Determined then
     begin
      write(infofile,' determined: ');
      infocomplex(NumericValue);
      infonewline;
     end;
//    writeln(infofile,' wartosc wielomianowa');
    for i:=0 to PolynomialValues.Count-1 do
     begin
      write(infofile,' ',i,':-- ');
      infopolynomial(PolynomialValues.Items^[i]);
      infonewline;
     end;
  end;
  write(infofile,' eqlista termow: ');
  infotermlist(EqClass);
  infonewline;
  write(infofile,' eqlista typow: ');
  InfoTypeColl(XTypClass); infonewline;
  write(infofile,' supercluster: ');
  infocluster(SuperCluster); infonewline;
 end;
end;

procedure InfoEqClasses;
 var e:integer;
begin
 InfoNewLine;
 for e:=1 to TrmNbr do InfoEqClass(e);
 flush(infofile);
end;

procedure InfoFuncTrmList;
 var e:FuncTrmExprKind;
     i,z:integer;
begin
 InfoNewLine; InfoString('InfoFuncTrmList'); InfoNewLine;
 for e := Low(FuncTrmExprKind) to High(FuncTrmExprKind) do
  with FuncTrmList[e] do
   for z:=0 to Count-1 do
    with ConstrItem(Items^[z])^.fTerms do
     for i:=0 to Count-1 do
      begin
       InfoTerm(At(i));
       InfoNewLine;
      end;

 InfoNewLine; InfoString('InfoFrOper'); InfoNewLine;
 with FrOper do
  for i:=0 to Count-1 do
   begin
    InfoTerm(At(i));
    InfoNewLine;
   end;
   
 InfoNewLine; InfoString('InfoChoiceTerm'); InfoNewLine;
 with ChoiceTerm do
  for i:=0 to Count-1 do
   begin
    InfoTerm(At(i));
    InfoNewLine;
   end;
 
 flush(InfoFile);
end;
{$ENDIF}

function EqTrms ( fTrm1,fTrm2:TrmPtr ): boolean; FORWARD;
function EqTyps ( fTyp1,fTyp2:TypPtr ): boolean; FORWARD;
function EqFrms ( fFrm1,fFrm2:FrmPtr ): boolean; FORWARD;

function EqTrmLists(FTL1,FTL2: TrmList): boolean;
begin
 while (FTL1 <> nil) and (FTL2 <> nil) do
  begin
   if not EqTrms(FTL1^.XTrmPtr,FTL2^.XTrmPtr) then
     begin EqTrmLists:=false;
      exit
     end;
   FTL1 := FTL1^.NextTrm; FTL2 := FTL2^.NextTrm;
  end;
 EqTrmLists := FTL1 = FTL2;
end;

function EqButLast(FTL1,FTL2: TrmList): boolean;
begin mizassert(2000,(FTL1 <> nil) and (FTL2 <> nil));
 while (FTL1^.NextTrm <> nil) and (FTL2^.NextTrm <> nil) do
  begin
   if not EqTrms(FTL1^.XTrmPtr,FTL2^.XTrmPtr) then
     begin EqButLast:=false;
      exit
     end;
   FTL1 := FTL1^.NextTrm; FTL2 := FTL2^.NextTrm;
  end;
 EqButLast := FTL1^.NextTrm=FTL2^.NextTrm;
end;

function EqAttrs(fAttr1, fAttr2: AttrPtr): boolean;
  var lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 fAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 fAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 EqAttrs := (lAttrNr1 = lAttrNr2) and
            (fAttr1^.fNeg = fAttr2^.fNeg) and
             EqTrmLists(lArgs1,lArgs2);
end;

function EqTyps ( fTyp1,fTyp2:TypPtr ): boolean;
 var ModNr1,ModNr2: integer;
     A1,A2: TrmList;
begin EqTyps:=false;
 if fTyp1^.TypSort=fTyp2^.TypSort then
  case fTyp1^.TypSort of
   ikTypMode:
    begin fTyp1^.AdjustTyp(ModNr1,A1); fTyp2^.AdjustTyp(ModNr2,A2);
     if (ModNr1=ModNr2) and EqualClusters(fTyp1,fTyp2,EqAttrs) then
       EqTyps:=EqTrmLists(A1,A2);
    end;
   ikTypStruct:
    if (fTyp1^.ModNr = fTyp2^.ModNr) and
       EqualClusters(fTyp1,fTyp2,EqAttrs) then
     EqTyps:=EqTrmlists(fTyp1^.ModArgs,fTyp2^.ModArgs);
   else RunTimeError(2020);
  end;
end;

function EqFrms ( fFrm1,fFrm2:FrmPtr ): boolean;
 var PredNr1,PredNr2,i: integer;
     lFlag: Boolean;
     lTrmList,lTrmList1,A1,A2:TrmList;
begin EqFrms := false;
 with fFrm1^ do IF FrmSort=fFrm2^.FrmSort then
  case FrmSort of
   ikFrmNeg:
    EqFrms := EqFrms(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmQual:
    if EqTrms(QualFrmPtr(fFrm1)^.QualTrm,QualFrmPtr(fFrm2)^.QualTrm) then
     EqFrms:=EqTyps(QualFrmPtr(fFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^.Conjuncts do
     if Count = ConjFrmPtr(fFrm2)^.Conjuncts.Count then
      begin
       for i:=0 to Count-1 do
        if not EqFrms(FrmPtr(Items^[i]),FrmPtr(ConjFrmPtr(fFrm2)^.Conjuncts.Items^[i])) then
         exit;
       EqFrms:=true;
      end;
   ikFrmSchPred,ikFrmPrivPred:
    if PredFrmPtr(fFrm1)^.PredNr=PredFrmPtr(fFrm2)^.PredNr then
     EqFrms := EqTrmLists(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
   ikFrmAttr:
    begin
     AdjustAttrFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustAttrFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
      EqFrms := EqTrmLists(A1,A2);
    end;
   ikFrmPred:
    begin
     AdjustFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
      begin
       if EqTrmLists(A1,A2) then
        begin EqFrms:=true;
         exit
        end;
{ ### To powinno byc nasladowane w innych procedurach porownan
  - np. schematyzator - ale nie ma analogii dla "connectedness"
}
      if sySymmetry in ConstrPtr( Constr[ coPredicate].At(
                                 PredFrmPtr(fFrm1)^.PredNr))^.fProperties then
       with PredFrmPtr(fFrm1)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
        begin
         lTrmList:=SwitchArgs(fFirstArg,fSecondArg,PredFrmPtr(fFrm1)^.PredArgs);
         lTrmList1:=AdjustTrmList(ikFrmPred,PredFrmPtr(fFrm1)^.PredNr,lTrmList);
         lFlag:=EqTrmLists(lTrmList1,A2);
         DisposeTrmList(lTrmList);
         EqFrms:=lFlag;
        end
      else if sySymmetry in ConstrPtr( Constr[ coPredicate].At(
                                 PredFrmPtr(fFrm2)^.PredNr))^.fProperties then
       with PredFrmPtr(fFrm2)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
        begin
         lTrmList:=SwitchArgs(fFirstArg,fSecondArg,PredArgs);
         lTrmList1:=AdjustTrmList(ikFrmPred,PredNr,lTrmList);
         lFlag:=EqTrmLists(A1,lTrmList1);
         DisposeTrmList(lTrmList);
         EqFrms:=lFlag;
        end
      end;
    end;
   ikFrmUniv:
    if EqTyps(UnivFrmPtr(fFrm1)^.Quantified,UnivFrmPtr(fFrm2)^.Quantified) then
     EqFrms := EqFrms(UnivFrmPtr(fFrm1)^.Scope,UnivFrmPtr(fFrm2)^.Scope);
   ikFrmVerum,ikFrmThesis,ikFrmError:
    EqFrms:=true;
   else RunTimeError(2021);
  end;
end;

function EqTrms ( fTrm1,fTrm2:TrmPtr ): boolean;
  var FuncNr1,FuncNr2,i: integer; A1,A2: TrmList;
begin EqTrms:=false;
 with fTrm1^ do
  begin
   if TrmInfo<>0 then
    begin EqTrms:=TrmInfo=fTrm2^.TrmInfo;
     exit
    end;
   if TrmSort = fTrm2^.TrmSort then
    case TrmSort of
     ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmNumeral:
      with VarTrmPtr(fTrm1)^ do
       EqTrms:=VarNr=VarTrmPtr(fTrm2)^.VarNr;
     ikTrmFunctor:
      begin AdjustTrm(fTrm1,FuncNr1,A1); AdjustTrm(fTrm2,FuncNr2,A2);
       if FuncNr1=FuncNr2 then EqTrms:=EqTrmLists(A1,A2);
      end;
     ikTrmSchFunc,ikTrmPrivFunc,ikTrmAggreg,ikTrmSelector:
      with FuncTrmPtr(fTrm1)^ do
      if FuncNr=FuncTrmPtr(fTrm2)^.FuncNr then
       EqTrms:=EqTrmLists(FuncArgs,FuncTrmPtr(fTrm2)^.FuncArgs);
     ikTrmFraenkel:
      with FraenkelTrmPtr(fTrm1)^ do
       if LambdaArgs.Count = FraenkelTrmPtr(fTrm2)^.LambdaArgs.Count then
        begin
         for i := 0 to LambdaArgs.Count-1 do
          if not EqTyps(LambdaArgs.Items^[i],FraenkelTrmPtr(fTrm2)^.LambdaArgs.Items^[i]) then
            exit;
         EqTrms := EqTrms(LambdaScope,FraenkelTrmPtr(fTrm2)^.LambdaScope)
               and EqFrms(Compr,FraenkelTrmPtr(fTrm2)^.Compr);
        end;
     ikTrmChoice:
      with ChoiceTrmPtr(fTrm1)^ do
       EqTrms:=EqTyps(ChoiceTyp,ChoiceTrmPtr(fTrm2)^.ChoiceTyp);
     ikTrmError: EqTrms:=true;
     else
      begin
{$IFDEF MDEBUG}
InfoString('TrmSort='); InfoChar(fTrm1^.TrmSort); flush(InfoFile);
{$ENDIF}
   RunTimeError(2022);
      end;
    end;
  end;
end;

procedure ConCollection.AllocTerm(fTrm:TrmPtr);
 var lTrmNr:integer;
begin
 case TrmPtr(fTrm)^.TrmSort of
 ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmFreeVar,ikTrmLambdaVar,
 ikTrmNumeral,ikTrmEqConst:
  if Search(addr(VarTrmPtr(fTrm)^.VarNr),lTrmNr) then
   with ConstrItem(Items^[lTrmNr])^ do
    begin
     fTerms.Insert(fTrm);
    end
   else AtInsert(lTrmNr,new(ConstrItem, Init(fTrm)));
 ikTrmSchFunc,ikTrmPrivFunc,ikTrmAggreg,ikTrmFunctor,ikTrmSelector:
  if Search(addr(FuncTrmPtr(fTrm)^.FuncNr),lTrmNr) then
   with ConstrItem(Items^[lTrmNr])^ do
    begin
     fTerms.Insert(fTrm);
    end
   else AtInsert(lTrmNr,new(ConstrItem, Init(fTrm)));
 end;
end;

function ConCollection.FindTerm(fTrm:TrmPtr): TrmPtr;
 var lFuncNr,ii: integer;
begin
 FindTerm:=nil;
 if Search(addr(FuncTrmPtr(fTrm)^.FuncNr),lFuncNr) then
  with ConstrItem(Items^[lFuncNr])^ do
   begin
//    gStrictCompare:=false;
//    if fTerms.Search(fTrm,ii) then
    for ii:=0 to fTerms.count-1 do
     if EqTrms(fTrm,fTerms.Items^[ii]) then
      begin FindTerm:=fTerms.Items^[ii];
       exit
      end;
//    gStrictCompare:=true;
   end;
end;

procedure DisposeTrmRecAndList(fTrm:FuncTrmPtr);
 var ll,ll1: TrmList;
begin
 with fTrm^ do
  case TrmSort of
   ikTrmSchFunc,ikTrmPrivFunc,ikTrmAggreg,ikTrmFunctor,ikTrmSelector:
    begin ll:=FuncArgs;
     while ll <> nil do
      begin ll1:=ll^.NextTrm;
       dispose(ll);
       ll:=ll1
      end;
     dispose(fTrm)
    end;
   else
    begin
{$IFDEF MDEBUG}
InfoChar(TrmSort);
{$ENDIF}
    RunTimeError(2136);
    end;
  end;
end;

constructor ChConstrObj.Init(aTrm:TrmPtr);
begin IntKey:=FuncTrmPtr(aTrm)^.FuncNr;
// fTerms.InitSorted(0,CompRdTrms);
 fTerms.Init(0);
 fTerms.Insert(aTrm);
end;

destructor ChConstrObj.Done;
begin
 fTerms.DeleteAll;
 fTerms.Done;
end;

var ThereAreBound: boolean;
procedure CheckBound(var fTrm:TrmPtr);
begin
 if (fTrm^.TrmSort=ikTrmBound) and (VarTrmPtr(fTrm)^.VarNr<=BoundVarNbr)
  then ThereAreBound:=true;
end;

procedure YTerm(var FTrm:TrmPtr); FORWARD;
procedure YType(FTyp:TypPtr); FORWARD;

procedure InsertType(fTyp:TypPtr; aTrmNr: integer);
{ zakldamy,ze ftyp, jest zuzywany }
 var x,z,lCount: integer;
     lTypPtr: TypPtr;
 label 1;
begin
 with Trms[aTrmNr] do
 begin
   repeat
    for z:=0 to XTypClass.Count-1 do
     with TypPtr(XTypClass.Items^[z])^ do
      if (TypSort = fTyp^.TypSort) and (ModNr = fTyp^.ModNr) and
          EqTrmLists(ModArgs,fTyp^.ModArgs) then
      begin
        if fTyp^.UpperCluster^.IsSubsetOf(SuperCluster,EqAttrs) then
         begin dispose(fTyp,Done);
           exit
         end;
        with fTyp^.UpperCluster^ do
         for x:=0 to Count-1 do
          SuperCluster^.Insert(AttrPtr(Items^[x])^.CopyAttribute);
        if not SuperCluster^.fConsistent
         then SetContr(1);
        dispose(fTyp,Done);
        exit;
      end;
    YType(fTyp);
    with fTyp^.UpperCluster^ do
     for x:=0 to Count-1 do
       SuperCluster^.Insert(AttrPtr(Items^[x])^.CopyAttribute);
    if not SuperCluster^.fConsistent then SetContr(2);
    fTyp^.LowerCluster^.Done;
    fTyp^.UpperCluster^.Done;
    XTypClass.Insert(fTyp);
    if (fTyp^.TypSort<>ikTypMode) or (fTyp^.ModNr = 1) then break;
    fTyp:=fTyp^.Widening;
   until false;
   lCount:=XTypClass.Count;
   if fTyp^.TypSort = ikTypStruct then
   repeat
    with StructConstrPtr( Constr[ coStructMode].At( fTyp^.ModNr))^ do
    for z:=0 to fPrefixes.Count-1 do
     begin
      lTypPtr:=TypPtr(fPrefixes.Items^[z])^.InstTyp(fTyp^.ModArgs);
      YType(lTypPtr);
      lTypPtr^.LowerCluster^.Done;
      lTypPtr^.UpperCluster^.Done;
      for x:=0 to XTypClass.Count-1 do
       with TypPtr(XTypClass.Items^[x])^ do
        if (TypSort = lTypPtr^.TypSort) and (ModNr = lTypPtr^.ModNr) and
           EqTrmLists(ModArgs,lTypPtr^.ModArgs) then
        begin dispose(lTypPtr,Done);
         goto 1
        end;
      XTypClass.Insert(lTypPtr);
1:
     end;
    if lCount >= XTypClass.Count then break;
    fTyp:=XTypClass.Items^[lCount];
    inc(lCount);
   until false;
 end;
end;

procedure YTermList(FTL: TrmList);
begin
 while FTL<>nil do
  begin YTerm(FTL^.XTrmPtr);
   FTL:=FTL^.NextTrm
  end;
end;

procedure YAttr(aAttr:AttrPtr);
begin
 YTermList(aAttr^.fAttrArgs);
end;

procedure YCluster(aClu:AttrCollectionPtr);
 var i: integer;
begin
 with aClu^ do
 for i:=0 to Count-1 do
  YAttr(AttrPtr(Items^[i]));
end;

procedure YType(FTyp:TypPtr);
begin
 if FTyp<>nil then
 with fTyp^ do
  begin
   YCluster(LowerCluster);
   YCluster(UpperCluster);
   YTermList(ModArgs);
  end;
end;

procedure YFormula(FFrm:FrmPtr);
 var z: integer;
begin
 with FFrm^ do
 case FrmSort of
  ikFrmNeg: YFormula(NegFrmPtr(fFrm)^.NEGArg);
  ikFrmConj:
    for z:=0 to ConjFrmPtr(fFrm)^.Conjuncts.Count-1 do
     YFormula(FrmPtr(ConjFrmPtr(fFrm)^.Conjuncts.Items^[z]));
  ikFrmUniv:
   begin inc(BoundVarNbr);
    YType(UnivFrmPtr(fFrm)^.Quantified);
    YFormula(UnivFrmPtr(fFrm)^.Scope);
    dec(BoundVarNbr);
   end;
  ikFrmSchPred,ikFrmPrivPred,ikFrmAttr,ikFrmPred: YTermList(PredFrmPtr(fFrm)^.PredArgs);
  ikFrmQual:
   begin YType(QualFrmPtr(fFrm)^.QualTyp);
     YTerm(QualFrmPtr(fFrm)^.QualTrm)
   end;
  ikFrmFlexConj:
   with FlexFrmPtr(fFrm)^ do
    begin
     YFormula(nLeftOrigFrm);
     YFormula(nRightOrigFrm);
//       YFormula(nExpansion);
    end;
  ikFrmVerum,ikFrmError: ;
  else
   begin
{$IFDEF MDEBUG}
InfoChar(FrmSort);
{$ENDIF}
    RunTimeError(2023);
   end;
 end;
end;

procedure YEqClass(fTrm:TrmPtr);
begin
 if TrmNbr=MaxTrmNbr then
   TrmOvfl:=true
  else inc(TrmNbr);
 fTrm^.TrmInfo:=TrmNbr;
 with TrmS[TrmNbr] do
  begin
   { !!!!!!!!!!! kontrola rozmiaru }
   inc(EqClassNbr);
   Term:=NewVarTrm(ikTrmEqConst,EqClassNbr);
   Term^.TrmInfo:=TrmNbr;
   NumValue.Determined:=false;
   PolynomialValues.InitSorted(0,4,StdComparePolynomials);
   EqClass:=NewTrmList(fTrm,nil);
   XTypClass.Init(4,4);
//  inicjalizacja z typem set (najbardziej ogolnym)
   XTypClass.Insert(NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                             gBuiltIn[rqAny],nil));
//!!!set - poczatek: wyrzucic przypadek z gBuiltIn[rqSetMode]
//   XTypClass.Insert(NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
//                             gBuiltIn[rqSetMode],nil));
//!!!set - koniec wyrzucic przypadek z gBuiltIn[rqSetMode]
   SuperCluster:=new(AttrCollectionPtr,Init(0,4));
  end;
end;

function YArgList(aArgs:TrmList): boolean;
 var HH: TrmList;
begin YArgList:=false;
 YTermList(aArgs);
 HH:=aArgs;
 while HH<>nil do
  begin
   if HH^.XTrmPtr^.TrmInfo=0 then exit;
   HH:=HH^.NextTrm
  end;
 YArgList:=true;
end;

var E_free: boolean;

procedure Check_E(var fTrm:TrmPtr);
begin
 if fTrm^.TrmSort=ikTrmEqConst then E_free:=false;
end;

procedure YTerm(var FTrm:TrmPtr);
var ii,z: integer;
    AType: TypPtr;
    lCommTrm,lCommTrm1,OrgTrm,lTrm: TrmPtr;
    lArgList,lArgList1,lArgList2: TrmList;
    lFunc,lFunc1,lIndex,lBoundVarNbr,lConstNr: integer;
 label toexit;
begin
//writeln(InfoFile,'  yterm<:',Emb);
//InfoTerm(fTrm); InfoNewline;
//inc(Emb);
 with FTrm^ do
  begin
   case TrmSort of
    ikTrmBound,ikTrmEqConst: goto toexit;
    ikTrmInfConst:
     with VarTrmPtr(fTrm)^ do
     begin
      if (VarNr < DTrm.Count) and (DTrm.Items^[VarNr]<>nil) then
       begin lConstNr:=VarNr;
        dispose(fTrm);
        fTrm:=DTrm.Items^[lConstNr];
        goto toexit
       end;
      YEqClass(fTrm);
      DTrm.AtInsert(VarNr,TrmS[fTrm^.TrmInfo].Term);
      with ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm)^.VarNr])^ do
       if fDetermined then
       with TrmS[fTrm^.TrmInfo] do
        begin NumValue.Determined:=true;
         NumValue.NumericValue:=fNumericValue;
        end;
      aType:=CopyExpTyp(ConstDefPtr(InferConstDef.Items^[VarNr])^.fTyp);
      YType(aType);
      InsertType(aType,fTrm^.TrmInfo);
      fTrm:=TrmS[fTrm^.TrmInfo].Term;
      goto toexit;
     end;
    ikTrmFunctor:
     with FuncTrmPtr(FTrm)^ do
     begin
      OrgTrm:=CopyTerm(FTrm);
      if not YArgList(FuncArgs) then
       begin DisposeTrm(OrgTrm);
        goto toexit
       end;
      lArgList1:=CopyTermList(FuncArgs);
      E_free:=true;
      WithinTerm(OrgTrm,Check_E);
      if E_free then aType:=RoundUpTrmType(OrgTrm)
       else AType:=CopyTrmType(OrgTrm);
      AdjustTrm(fTrm,lFunc,lArgList);
      if FuncTrmList[expTrmFunctor].Search(@lFunc,lIndex) then
       begin
         with ConstrItem(FuncTrmList[expTrmFunctor].Items^[lIndex])^.fTerms do
         for ii:=0 to Count-1 do
         begin
          if EqTrmLists(lArgList,FuncTrmPtr(Items^[ii])^.FuncArgs) then
           begin
            DisposeTrmRecAndList(FuncTrmPtr(fTrm));
            DisposeTrmList(lArglist1);
            fTrm:=TrmS[FuncTrmPtr(Items^[ii])^.TrmInfo].Term;
//causes loop
//::            YType(aType);
//::            InsertType(aType,Trms[FuncTrmPtr(Items^[ii])^.TrmInfo].Term^.TrmInfo);
            DisposeTrm(OrgTrm);
            goto toexit;
           end;
         end;
       end;
      lTrm:=fTrm;
      fTrm:=NewFuncTrm(lFunc,CopyTermList(lArgList));
      DisposeTrm(lTrm);
      YEqClass(fTrm);
      FuncTrmList[expTrmFunctor].AllocTerm(fTrm);
      YType(aType);
      InsertType(aType,fTrm^.TrmInfo);
      lCommTrm1:=nil;
      with FuncTrmPtr(OrgTrm)^ do
      begin
       if gRevReq[AdjustedFuncNr(fTrm)] = rqZeroNumber then
        begin TrmS[fTrm^.TrmInfo].NumValue.Determined:=true;
         TrmS[fTrm^.TrmInfo].NumValue.NumericValue:=CZero;
        end;
       with ConstrTypPtr( Constr[ coFunctor].At(FuncNr))^ do
        if syCommutativity in fProperties then
        begin
         lCommTrm:=NewFuncTrm(FuncNr,SwapArguments(lArgList1,fFirstArg,fSecondArg));
         AdjustTrm(lCommTrm,lFunc1,lArgList2);
         lCommTrm1:=NewFuncTrm(lFunc1,CopyTermList(lArgList2));
         DisposeTrm(lCommTrm);
         lCommTrm1^.TrmInfo:=TrmNbr;
         TrmS[fTrm^.TrmInfo].EqClass^.NextTrm:=NewTrmList(lCommTrm1,nil);
        end;
       DisposeTrm(OrgTrm);
       DisposeTrmList(lArglist1);
      end;
      if lCommTrm1 <> nil then FuncTrmList[expTrmFunctor].AllocTerm(lCommTrm1);
      fTrm:=TrmS[fTrm^.TrmInfo].Term;
      goto toexit;
     end;
    ikTrmSchFunc:
    begin
     with FuncTrmPtr(fTrm)^ do
      if not YArgList(FuncArgs) then goto toexit;
     YEqClass(fTrm);
    end;
    ikTrmPrivFunc:
    begin
     with FuncTrmPtr(fTrm)^ do
      if not YArgList(FuncArgs) then goto toexit;
     YEqClass(fTrm);
     FuncTrmList[expTrmPrivFunc].AllocTerm(fTrm);
    end;
    ikTrmAggreg:
     with FuncTrmPtr(fTrm)^ do
     begin
      if not YArgList(FuncArgs) then goto toexit;
      if FuncTrmList[expTrmAggreg].Search(@FuncNr,lFunc) then
       begin
        with ConstrItem(FuncTrmList[expTrmAggreg].Items^[lFunc])^.fTerms do
         for ii:=0 to Count-1 do
         begin
          if EqTrmLists(FuncArgs,FuncTrmPtr(Items^[ii])^.FuncArgs) then
           begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
            fTrm:=TrmS[FuncTrmPtr(Items^[ii])^.TrmInfo].Term;
            goto toexit;
           end;
         end;
       end;
      YEqClass(fTrm);
      FuncTrmList[expTrmAggreg].AllocTerm(fTrm);
     end;
    ikTrmSelector:
     with FuncTrmPtr(fTrm)^ do
     begin
      if not YArgList(FuncArgs) then goto toexit;
      lTrm:=FuncTrmList[expTrmSelector].FindTerm(fTrm);
      if lTrm <> nil then
       begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
         FTrm:=TrmS[lTrm^.TrmInfo].Term;
         goto toexit;
       end;
      YEqClass(fTrm);
      FuncTrmList[expTrmSelector].AllocTerm(fTrm);
     end;
    ikTrmFraenkel:
     with FraenkelTrmPtr(fTrm)^ do
     begin lBoundVarNbr:=BoundVarNbr;
      for z:=0 to LambdaArgs.Count-1 do
       begin
        inc(BoundVarNbr);
        YType(TypPtr(LambdaArgs.Items^[z]));
       end;
      YTerm(LambdaScope);
      YFormula(Compr);
      BoundVarNbr:=lBoundVarNbr;
      ThereAreBound:=false;
      WithInTerm(fTrm,CheckBound);
// Uwaga powinno byc to stala
mizassert(2027,ThereAreBound);
      if ThereAreBound then
       begin {DisposeTrm(fTrm);}
        goto toexit
       end;
      WithInTerm(fTrm,ChChangeBound);
      if FrOper.Search(fTrm,ii) then
        begin DisposeTrm(fTrm);
          fTrm:=TrmS[FraenkelTrmPtr(FrOper.Items^[ii])^.TrmInfo].Term;
          goto toexit;
        end;
      YEqClass(fTrm);
      FrOper.Insert(fTrm);
     end;
    ikTrmChoice:
     with ChoiceTrmPtr(fTrm)^ do
     begin YType(ChoiceTyp);
      ThereAreBound:=false;
      WithInTerm(fTrm,CheckBound);
      if ThereAreBound then
       begin {DisposeTrm(fTrm);}
        goto toexit
       end;
      if ChoiceTerm.Search(fTrm,ii) then
        begin DisposeTrm(fTrm);
          fTrm:=TrmS[ChoiceTrmPtr(ChoiceTerm.Items^[ii])^.TrmInfo].Term;
          goto toexit;
        end;
      YEqClass(fTrm);
      ChoiceTerm.Insert(fTrm);
     end;
    else
     begin
{$IFDEF MDEBUG}
writeln(InfoFile,TrmSort,'|2024');
{$ENDIF}
      RunTimeError(2024);
     end;
   end;
  end;
 AType:=CopyTrmType(fTrm);
 YType(AType);
 InsertType(aType,fTrm^.TrmInfo);
 fTrm:=TrmS[fTrm^.TrmInfo].Term;
toexit:
//dec(Emb);
//writeln(infofile,'  yterm>: ',Emb);
end;

procedure YYTerm(fTrm:TrmPtr; var fi:integer);
var lArgList,lArglist1,aArgList: TrmList;
    lFunc,lFunc1,lIndex,k,ii,iNr,r: integer;
    lCommTrm,lTrm: TrmPtr;
    lTyp: TypPtr;
begin
//writeln(InfoFile,' yyterm<:');
//InfoTerm(fTrm); InfoNewline;
 with FTrm^ do
  case TrmSort of
  ikTrmNumeral:
   with VarTrmPtr(fTrm)^ do
    for ii:=1 to TrmNbr do
     with Trms[ii], NumValue do
      if (EqClass <> nil) and Determined and
         IsEQWithInt(NumericValue,VarNr) then
       begin DisposeTrm(fTrm);
        fi:=Term^.TrmInfo;
        exit
       end;
  ikTrmFunctor:
   with FuncTrmPtr(FTrm)^ do
    begin
     YTermList(FuncArgs);
     lCommTrm:=nil;
     with ConstrTypPtr(Constr[coFunctor].Items^[FuncNr])^ do
      if syCommutativity in fProperties then
       begin
        lCommTrm:=NewFuncTrm(FuncNr,SwapArguments(FuncArgs,fFirstArg,fSecondArg));
        AdjustTrm(lCommTrm,lFunc1,lArgList1);
       end;
     AdjustTrm(fTrm,lFunc,lArgList);
     if FuncTrmList[expTrmFunctor].Search(@lFunc,lIndex) then
      with ConstrItem(FuncTrmList[expTrmFunctor].Items^[lIndex])^.fTerms do
       begin
        for ii:=0 to Count-1 do
         with FuncTrmPtr(Items^[ii])^ do
          begin
           if EqTrmLists(lArgList,FuncArgs) then
            begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
             fi:=TrmInfo;
             exit;
            end;
           if (lCommTrm <> nil) and EqTrmLists(lArgList1,FuncArgs) then
            begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
             fi:=TrmInfo;
             DisposeTrm(lCommTrm);
             exit;
            end;
          end;
       end;
    end;
  ikTrmSchFunc:
    with FuncTrmPtr(fTrm)^ do
    begin
      YTermList(FuncArgs);
      lTrm:=FuncTrmList[expTrmSchFunc].FindTerm(fTrm);
      if lTrm <> nil then
       begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
        fi:=lTrm^.TrmInfo;
        exit;
       end;
    end;
  ikTrmPrivFunc:
    with FuncTrmPtr(fTrm)^ do
    begin
      YTermList(FuncArgs);
      lTrm:=FuncTrmList[expTrmPrivFunc].FindTerm(fTrm);
      if lTrm <> nil then
       begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
        fi:=lTrm^.TrmInfo;
        exit;
       end;
    end;
  ikTrmSelector:
    with FuncTrmPtr(FTrm)^ do
    begin
      YTermList(FuncArgs);
      lTrm:=FuncTrmList[expTrmSelector].FindTerm(fTrm);
      if lTrm <> nil then
       begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
        fi:=lTrm^.TrmInfo;
        exit;
       end;
    end;
  ikTrmAggreg:
    with FuncTrmPtr(FTrm)^ do
    begin
     YTermList(FuncArgs);
     if FuncTrmList[expTrmAggreg].Search(@FuncNr,lFunc) then
      begin
       with ConstrItem(FuncTrmList[expTrmAggreg].Items^[lFunc])^.fTerms do
        for ii:=0 to Count-1 do
        begin
{ Trzeba zadjustowac agregaty, tzn. obciac argumnety "over".
  Nalezaloby zapisac oryginalny typ, ale dlaczego nie jest
  to robione w wariancie ikTrmFunctor
}
         lArgList:=FuncArgs;
         lArgList1:=FuncTrmPtr(Items^[ii])^.FuncArgs;
         for k:=1 to AggrConstrPtr( Constr[ coAggregate].At( FuncNr))^.fAggregBase do
          begin lArgList:=lArgList^.NextTrm;
           lArgList1:=lArgList1^.NextTrm;
          end;
         if EqTrmLists(lArgList,lArgList1) then
          begin DisposeTrmRecAndList(FuncTrmPtr(fTrm));
           fi:=FuncTrmPtr(Items^[ii])^.TrmInfo;
           exit;
          end;
        end;
      end;
    end;
  ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
     begin
      for ii:=0 to LambdaArgs.Count-1 do
       begin
        inc(BoundVarNbr);
        YType(TypPtr(LambdaArgs.Items^[ii]));
       end;
      YTerm(LambdaScope);
      YFormula(Compr);
      dec(BoundVarNbr,LambdaArgs.Count);
      ThereAreBound:=false;
      WithInTerm(fTrm,CheckBound);
      if ThereAreBound then
       begin DisposeTrm(fTrm);
        exit
       end;
      WithInTerm(fTrm,ChChangeBound);
      if FrOper.Search(fTrm,ii) then
        begin DisposeTrm(fTrm);
          fi:=TrmPtr(FrOper.Items^[ii])^.TrmInfo;
          exit;
        end;
      end;
  ikTrmChoice:
    with ChoiceTrmPtr(fTrm)^ do
     begin YType(ChoiceTyp);
      if ChoiceTerm.Search(fTrm,ii) then
        begin DisposeTrm(fTrm);
          fi:=TrmPtr(ChoiceTerm.Items^[ii])^.TrmInfo;
          exit;
        end;
     end;
//    ikTrmEqConst,
  ikTrmInfConst,ikTrmConstant:
   exit;
  else
   begin
{$IFDEF MDEBUG}
writeln(InfoFile,TrmSort,'|2026');
{$ENDIF}
    RunTimeError(2026);
   end;
  end;

 with TrmS[TrmS[fi].Term^.TrmInfo] do
 case fTrm^.TrmSort of
  ikTrmNumeral:
   with VarTrmPtr(fTrm)^,NumValue do
   begin
     if not Determined then
      begin
       NumValue.Determined:=true;
       NumValue.NumericValue:=IntToComplex(VarNr);
      end
     else if not IsEQWithInt(NumericValue,VarNr) then
       SetContr(3);
     fTrm^.TrmInfo:=fi;
    end;
  ikTrmFunctor:
    begin
     iNr:=AdjustedFuncNr(fTrm);
     if gRevReq[iNr] = rqImaginaryUnit then
     begin NumValue.Determined:=true;
      NumValue.NumericValue:=CImUnit;
     end;
     if gRevReq[iNr] = rqZeroNumber then
      begin NumValue.Determined:=true;
       NumValue.NumericValue:=CZero;
      end;
     lTrm:=fTrm;
     fTrm:=NewFuncTrm(lFunc,CopyTermList(lArgList));
     fTrm^.TrmInfo:=fi;
     DisposeTrm(lTrm);
     FuncTrmList[expTrmFunctor].AllocTerm(fTrm);
     if lCommTrm <> nil then
      begin
       lTrm:=NewFuncTrm(lFunc1,CopyTermList(lArgList1));
       DisposeTrm(lCommTrm);
       lTrm^.TrmInfo:=fi;
       EqClass:=NewTrmList(lTrm,EqClass);
       FuncTrmList[expTrmFunctor].AllocTerm(lTrm);
      end;
     EqClass:=NewTrmList(fTrm,EqClass);
    end;
  else
    begin
     FTrm^.TrmInfo:=fi;
     EqClass:=NewTrmList(fTrm,EqClass);
     case fTrm^.TrmSort of
     ikTrmSchFunc: FuncTrmList[expTrmSchFunc].AllocTerm(fTrm);
     ikTrmPrivFunc: FuncTrmList[expTrmPrivFunc].AllocTerm(fTrm);
     ikTrmSelector: FuncTrmList[expTrmSelector].AllocTerm(fTrm);
     ikTrmAggreg: FuncTrmList[expTrmAggreg].AllocTerm(fTrm);
     ikTrmFraenkel: FrOper.Insert(fTrm);
     ikTrmChoice: ChoiceTerm.Insert(fTrm);
     end;
    end;
 end;
//writeln(InfoFile,' yyterm:>');
end;

function SubstituteVariable(aVar:integer; aTerm:PolynomialPtr; var aSubst:NatSet): boolean;
 var t,i,lVar: integer;
     lPolynomialValues: MList;
begin
  SubstituteVariable:=false;
  if aTerm^.HasTheVariable(aVar) then exit;
  lVar:=Trms[aVar].Term^.TrmInfo;
  for t:=1 to TrmNbr do
  with TrmS[t] do
  if (t <> lVar) and (EqClass <> nil) then
   begin
    lPolynomialValues.Init(0);
    i:=0;
    while i < PolynomialValues.Count do
     begin
      if PolynomialPtr(PolynomialValues.Items^[i])^.HasTheVariable(aVar) then
      begin
       PolynomialPtr(PolynomialValues.Items^[i])^.InsertValue(aVar,aTerm);
       if PolynomialPtr(PolynomialValues.Items^[i])^.IsNumeric then
        begin
          if not NumValue.Determined then
           begin
            aSubst.InsertElem(t);
            NumValue.Determined:=true;
            PolynomialPtr(PolynomialValues.Items^[i])^.GetNumeric(NumValue.NumericValue);
           end
          else if not PolynomialPtr(PolynomialValues.Items^[i])^.IsNumericEqualWith(NumValue.NumericValue) then
           SetContr(4);
        end;
          lPolynomialValues.Insert(PolynomialValues.Items^[i]);
          PolynomialValues.AtDelete(i);
          SubstituteVariable:=true;
      end else inc(i);
     end;
    for i:=0 to lPolynomialValues.Count-1 do
     PolynomialValues.Insert(lPolynomialValues.Items^[i]);
    lPolynomialValues.DeleteAll;
    lPolynomialValues.Done;
   end;
end;

procedure SubstitutePendingVars(var aPendingVarSubst:NatSet);
 var t,k: integer;
     lSubst: boolean;
begin
 while aPendingVarSubst.Count > 0 do
  begin
    k:=TrmS[aPendingVarSubst.Items^[0].X].Term^.TrmInfo;
    aPendingVarSubst.AtDelete(0);
    with TrmS[k] do
    begin
     mizassert(3453,EqClass <> nil);
     if TrmS[k].PolynomialValues.Count = 1 then
      if SubstituteVariable(k,TrmS[k].PolynomialValues.Items^[0],aPendingVarSubst) then
       lSubst:=true;
    end;
  end;

 repeat
  for t:=1 to TrmNbr do
   with TrmS[t] do
    if (EqClass <> nil) and NumValue.Determined and
       (PolynomialValues.Count = 1) then
     aPendingVarSubst.InsertElem(t);
  lSubst:=false;
  while aPendingVarSubst.Count > 0 do
  begin
    k:=TrmS[aPendingVarSubst.Items^[0].X].Term^.TrmInfo;
    aPendingVarSubst.AtDelete(0);
    with TrmS[k] do
    begin
     mizassert(3453,EqClass <> nil);
     if TrmS[k].PolynomialValues.Count = 1 then
      if SubstituteVariable(k,TrmS[k].PolynomialValues.Items^[0],aPendingVarSubst) then
       lSubst:=true;
    end;
  end;
 until not lSubst;
 aPendingVarSubst.Done;
end;

procedure UnionTrms(fEqNr1,fEqNr2:integer);
 var p,z,i,j:integer;
     LTL1: TrmList;
     lClusterPtr: AttrCollectionPtr;
     lPendingVarSubst: NatSet;
begin
  fEqNr1:=TrmS[fEqNr1].Term^.TrmInfo; fEqNr2:=TrmS[fEqNr2].Term^.TrmInfo;
  if fEqNr1 <> fEqNr2 then
  begin
   if fEqNr1 < fEqNr2 then
    begin p:=fEqNr1;
      fEqNr1:=fEqNr2;
      fEqNr2:=p
    end;
   clash:=true;
   if Trms[fEqNr1].NumValue.Determined then
    if Trms[fEqNr2].NumValue.Determined then
     begin
      if not AreEqComplex(Trms[fEqNr1].NumValue.NumericValue,Trms[fEqNr2].NumValue.NumericValue) then
       begin SetContr(5);
        exit
       end;
     end
    else
     begin
      Trms[fEqNr2].NumValue.Determined:=Trms[fEqNr1].NumValue.Determined;
      Trms[fEqNr2].NumValue.NumericValue:=Trms[fEqNr1].NumValue.NumericValue;
     end;
//    Laczenie eqclass
//    wynik Trms[fEqNr2].EqClass
   LTL1:=Trms[fEqNr1].EqClass;
   while LTL1<>nil do
    begin TrmS[LTL1^.XTrmPtr^.TrmInfo].Term^.TrmInfo:=fEqNr2;
     LTL1:=LTL1^.NextTrm;
    end;
   LTL1:=LastElem(Trms[fEqNr2].EqClass);
   LTL1^.NextTrm:=Trms[fEqNr1].EqClass;
   Trms[fEqNr1].EqClass:=nil;
//    Laczenie superclusterow
//    wynik Trms[fEqNr2].SuperCluster
//    po polaczeniu eqclass moze pojawic sie nowa sprzcznosc.
    with Trms[fEqNr2],SuperCluster^ do
    begin
     lClusterPtr:=new(AttrCollectionPtr,Init(Count,4));
     for z:=0 to Count-1 do
      lClusterPtr^.Insert(AttrPtr(Items^[z])^.CopyAttribute);
     for z:=0 to Trms[fEqNr1].SuperCluster^.Count-1 do
       lClusterPtr^.Insert(AttrPtr(Trms[fEqNr1].SuperCluster^.Items^[z])^.CopyAttribute);
     dispose(Supercluster,Done);
     Supercluster:=lClusterPtr;
    end;
   if not Trms[fEqNr2].SuperCluster^.fConsistent then SetContr(6);
   dispose(Trms[fEqNr1].SuperCluster,Done);
   Trms[fEqNr1].SuperCluster := nil;
//   Laczenie listy typow
   for z:=0 to Trms[fEqNr1].XTypClass.Count-1 do
    InsertType(Trms[fEqNr1].XTypClass.Items^[z],fEqNr2);
   Trms[fEqNr1].XTypClass.DeleteAll;
//    wartosc wielomianowa
   Trms[fEqNr2].PolynomialValues.AppendTo(Trms[fEqNr1].PolynomialValues);
   Trms[fEqNr1].PolynomialValues.DeleteAll;
   with Trms[fEqNr2] do
   if PolynomialValues.Count=2 then
   begin
//    eliminacja pojedynczych zmiennych poprzez ich podstawienie
//    eqklasa zawiera dwie wartosci wielamianowe (wynik polaczenia)
    lPendingVarSubst.Init(0,8);
    i:=PolynomialPtr(PolynomialValues.Items^[0])^.IsVariable;
    j:=PolynomialPtr(PolynomialValues.Items^[1])^.IsVariable;
    if i > 0 then
     begin
      if (j > 0) and (i < j) then
       begin
        PolynomialValues.AtFree(1);
        SubstituteVariable(j,PolynomialValues.Items^[0],lPendingVarSubst);
       end
      else
       begin
        PolynomialValues.AtFree(0);
        SubstituteVariable(i,PolynomialValues.Items^[0],lPendingVarSubst);
       end
     end
    else if j > 0 then
     begin
      PolynomialValues.AtFree(1);
      SubstituteVariable(j,PolynomialValues.Items^[0],lPendingVarSubst);
     end;
    SubstitutePendingVars(lPendingVarSubst);
   end;
  end;
end;

procedure RatAdd(fVal1,fVal2:ValRec; var fVal:ValRec);
begin fVal.Determined:=false;
 if fVal1.Determined and fVal2.Determined then
  begin fVal.Determined:=true;
   fVal.NumericValue:=ComplexAdd(fVal1.NumericValue,fVal2.NumericValue);
  end;
end;

procedure RatSucc(fVal1:ValRec; var fVal:ValRec);
begin
 fVal.Determined:=false;
 if fVal1.Determined and IsNaturalNumber(fVal1.NumericValue) then
  RatAdd(fVal1,One,fVal);
end;

procedure RatOpp(fVal1:ValRec; var fVal:ValRec);
begin fVal.Determined:=false;
 if fVal1.Determined then
  begin fVal.Determined:=true;
   fVal.NumericValue:=ComplexNeg(fVal1.NumericValue);
  end;
end;

procedure RatMult(fVal1,fVal2:ValRec; var fVal:ValRec);
begin fVal.Determined:=false;
 if fVal1.Determined and fVal2.Determined then
  begin fVal.Determined:=true;
   fVal.NumericValue:=ComplexMult(fVal1.NumericValue,fVal2.NumericValue);
  end;
end;

procedure RatInv(fVal1:ValRec; var fVal:ValRec);
begin fVal.Determined:=false;
 if fVal1.Determined and not IsEqWithInt(fVal1.NumericValue,0) then
  begin fVal.Determined:=true;
   fVal.NumericValue:=ComplexInv(fVal1.NumericValue);
  end;
end;

procedure AddEquality(var aEquals: IntRel; LArg,RArg: integer);
 var x: integer;
begin
 if LArg = RArg then exit;
 if LArg > RArg then
  begin x:=LArg;
   LArg:=RArg;
   RArg:=x
  end;
 aEquals.AssignPair(LArg,RArg);
end;

procedure ProcessLinearEquations(var aEquals:IntRel; var aPolynomials:MCollection);
 var i,j,l,i1,i2,t1,t2,lVar: integer;
     lPolynomial,lEq: PolynomialPtr;
     lPolynomials,lEquations: MSortedCollection;
     lVariables: MSortedList;
     lMonomial: MonomialPtr;
 label 1,2;
begin
 aPolynomials.Init(0,8);
 if aEquals.Count = 0 then exit;
 lPolynomials.InitSorted(0,4,ComparePolynomials);
 for i:=0 to aEquals.Count-1 do
  with aEquals.Items^[i] do
   if (TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Count > 0) and
      (TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Count > 0) then
   begin
    lPolynomial:=DiffPolynomials(TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Items^[0],
                                 TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Items^[0]);
    if lPolynomial^.IsNumeric then
     begin
      if not lPolynomial^.IsNumericEqualWith(CZero) then
       begin dispose(lPolynomial,Done); SetContr(7);
        exit
       end;
      dispose(lPolynomial,Done);
     end
    else lPolynomials.Insert(lPolynomial);
   end;
 if lPolynomials.Count = 0 then
  begin
   lPolynomials.Done;
   exit;
  end;
 lEquations.InitSorted(0,4,ComparePolynomials);
 lVariables.InitSorted(0,CompareMonomials);
 for i:=0 to lPolynomials.Count-1 do
  with PolynomialPtr(lPolynomials.Items^[i])^ do
  begin
    lEq:=new(PolynomialPtr,InitSorted(0,4,CompareMonomials));
    for j:=0 to Count-1 do
    if MonomialPtr(Items^[j])^.IsNumeric then
     lEq^.Insert(new(MonomialPtr,Init(MonomialPtr(Items^[j])^.nCoefficient)))
    else
     begin
      lMonomial:=new(MonomialPtr,Init(COne));
      lMonomial^.nPowerProduct.CopyNatFunc(MonomialPtr(Items^[j])^.nPowerProduct);
      if not lVariables.Search(lMonomial,l) then
       lVariables.Insert(lMonomial)
      else dispose(lMonomial,Done);
      lEq^.Insert(new(MonomialPtr,InitMonomial(MonomialPtr(Items^[j])^.nCoefficient,l)));
     end;
    lEquations.Insert(lEq);
  end;
 lPolynomials.Done;
 GaussElimination(lEquations,Contr);
 SetContr(Contr);
 if Contr > 0 then
  begin
   lEquations.Done;
   lVariables.Done;
   exit;
  end;
 for i:=0 to lEquations.Count-1 do
  with PolynomialPtr(lEquations.Items^[i])^ do
  begin
    lPolynomial:=new(PolynomialPtr,InitSorted(0,4,CompareMonomials));
    for j:=0 to Count-1 do
    if MonomialPtr(Items^[j])^.IsNumeric then
     lPolynomial^.Insert(new(MonomialPtr,Init(MonomialPtr(Items^[j])^.nCoefficient)))
    else
     begin
      lVar:=MonomialPtr(Items^[j])^.nPowerProduct.Items^[0].X;
      lMonomial:=MonomialPtr(MonomialPtr(lVariables.Items^[lVar])^.MCopy);
      lMonomial.nCoefficient:=MonomialPtr(Items^[j])^.nCoefficient;
      lPolynomial^.Insert(lMonomial);
     end;
   aPolynomials.Insert(lPolynomial);
  end;
{$IFDEF MDEBUG}
//writeln(infofile,'Rozwiniete rownosci ("Gauss" elimination)');
//for i:=0 to aPolynomials.Count-1 do
//begin
//write(infofile,i,': ');
//infopolynomial(aPolynomials.Items^[i]);
//infonewline;
//end;
{$ENDIF};
 for t1:=1 to TrmNbr do
  if (Trms[t1].EqClass<>nil) and (Trms[t1].PolynomialValues.Count>0) then
  for t2:=t1+1 to TrmNbr do
   if (Trms[t2].EqClass<>nil) and (Trms[t2].PolynomialValues.Count>0) and
      (aEquals.IndexOf(t1,t2) < 0) and (aEquals.IndexOf(t2,t1) < 0) then
   begin
    for i1:=0 to TrmS[t1].PolynomialValues.Count-1 do
    for i2:=0 to TrmS[t2].PolynomialValues.Count-1 do
    begin
     lPolynomial:=DiffPolynomials(TrmS[t1].PolynomialValues.Items^[i1],
                                  TrmS[t2].PolynomialValues.Items^[i2]);
     lEq:=new(PolynomialPtr,InitSorted(0,4,CompareMonomials));
     with lPolynomial^ do
      for j:=0 to Count-1 do
       if MonomialPtr(Items^[j])^.IsNumeric then
        lEq^.Insert(new(MonomialPtr,Init(MonomialPtr(Items^[j])^.nCoefficient)))
       else
        begin
         lMonomial:=new(MonomialPtr,Init(COne));
         lMonomial^.nPowerProduct.CopyNatFunc(MonomialPtr(Items^[j])^.nPowerProduct);
         if not lVariables.Search(lMonomial,l) then
          begin dispose(lMonomial,Done);
           goto 1;
          end;
         lEq^.Insert(new(MonomialPtr,InitMonomial(MonomialPtr(Items^[j])^.nCoefficient,l)));
        end;
     if lEq^.IsNumeric then
      begin
       if lEq^.IsNumericEqualWith(CZero) then
        begin
          AddEquality(aEquals,t1,t2);
         dispose(lPolynomial,Done);
         dispose(lEq,Done);
         goto 2;
        end;
       goto 1;
      end;
     lEq:=LinearEquationReduce(lEq,lEquations);
     if lEq^.IsNumeric then
      begin
       if lEq^.IsNumericEqualWith(CZero) then
        begin
         AddEquality(aEquals,t1,t2);
         dispose(lPolynomial,Done);
         dispose(lEq,Done);
         goto 2;
        end;
       goto 1;
      end;
1:
     dispose(lPolynomial,Done);
     dispose(lEq,Done);
    end;
2:
   end;
 lEquations.Done;
 lVariables.Done;
end;

procedure ProcessPolynomialValues(var aEquals:IntRel; var aPolynomials:MCollection);
 var
   i,i1,i2,t1,t2: integer;
   lPolynomial	: PolynomialPtr;
   aTransOrder	: NatFuncPtr;
   rPolynomials	: MCollection;
 label 1;
begin
 if aPolynomials.Count = 0 then exit;
  new(aTransOrder,InitNatFunc(20,20));
  rPolynomials.CopyList(aPolynomials);
  for t1:=1 to TrmNbr do
   if (Trms[t1].EqClass<>nil) and (Trms[t1].PolynomialValues.Count>0) then
    for i:=0 to Trms[t1].PolynomialValues.Count-1 do
      rPolynomials.Insert(MListPtr(Trms[t1].PolynomialValues.Items^[i])^.MCopy);
   RecodeVariableOrder(rPolynomials,aPolynomials.Count,aTransOrder);
{$IFDEF GDEBUG}
   writeln(infofile,'--------------');infostring('TransOrder: ');infonatfunc(aTransOrder^);infonewline;
{$ENDIF};
   rPolynomials.Done;
{$IFDEF GDEBUG}
writeln(infofile,'Before Groebner');
for i:=0 to aPolynomials.Count-1 do
begin
write(infofile,i,': ');
infopolynomial(aPolynomials.Items^[i]);
infonewline;
end;
{$ENDIF};
   RecodeVariables(aPolynomials,aTransOrder);
{$IFDEF GDEBUG}
   writeln(infofile,'TrmNbr=',trmnbr);
{$ENDIF};
   for t1:=1 to TrmNbr do
    if (Trms[t1].EqClass<>nil) and (Trms[t1].PolynomialValues.Count>0) then
      begin
{$IFDEF GDEBUG}
 write(infofile,'trms[',t1,']=');
 for i:=0 to Trms[t1].PolynomialValues.Count-1 do
 begin
    infopolynomial(PolynomialPtr(Trms[t1].PolynomialValues.Items^[i]));infonewline;
 end;
{$ENDIF};
       RecodeVariables(Trms[t1].PolynomialValues,aTransOrder);
      end;
   dispose(aTransOrder,Done);
   ExtendToGroebnerBasis0(aPolynomials);
{$IFDEF GDEBUG}
writeln(infofile,'After Groebner');
for i:=0 to aPolynomials.Count-1 do
begin
write(infofile,i,': ');
infopolynomial(aPolynomials.Items^[i]);
infonewline;
end;
 if not CheckGroebnerBasis(aPolynomials) then
   begin writeln(infofile,'NO GROEBNER!!!!!!!!!!!!!!!');
    halt(1);
   end;
{$ENDIF};
 for t1:=1 to TrmNbr do
  if (Trms[t1].EqClass<>nil) and (Trms[t1].PolynomialValues.Count>0) then
   for t2:=t1+1 to TrmNbr do
   begin
    if (Trms[t2].EqClass<>nil) and (Trms[t2].PolynomialValues.Count>0) and
       (aEquals.IndexOf(t1,t2) < 0) and (aEquals.IndexOf(t2,t1) < 0) then
    for i1:=0 to TrmS[t1].PolynomialValues.Count-1 do
     for i2:=0 to TrmS[t2].PolynomialValues.Count-1 do
     begin
      lPolynomial:=DiffPolynomials(TrmS[t1].PolynomialValues.Items^[i1],
                                   TrmS[t2].PolynomialValues.Items^[i2]);
      lPolynomial:=Reduction(lPolynomial,aPolynomials);
      if lPolynomial^.IsNumeric then
       if lPolynomial^.IsNumericEqualWith(CZero) then
        begin
         AddEquality(aEquals,t1,t2);
         dispose(lPolynomial,Done);
         goto 1;
        end;
      dispose(lPolynomial,Done);
     end;
1:
    end;
end;

procedure AddPolynomialValues;
 var k,t,i,i1,i2,lArg1,lArg2,lCount,lLevel: integer;
     lVal: ValRec;
     lEqClass: TrmList;
     lTrm: TrmPtr;
     lPolynomial1,lPolynomial2: PolynomialPtr;
     lAddedValues: NatSet;
     ArgsNr: array[1..MaxTrmNbr] of NatSet;
     lPolynomialValues: array[1..MaxTrmNbr] of MSortedCollection;
     lValues: array[1..MaxTrmNbr] of MList;
begin
  for t:=1 to TrmNbr do
   with TrmS[t] do
   begin
    ArgsNr[t].Init(0,4);
    lPolynomialValues[t].InitSorted(0,4,StdComparePolynomials);
    if (EqClass <> nil) and (PolynomialValues.Count <> 0) then
    begin
     lEqClass:=EqClass;
     while lEqClass <> nil do
     begin
      lTrm:=lEqClass^.XTrmPtr;
      lEqClass:=lEqClass^.NextTrm;
      if lTrm^.TrmSort = ikTrmFunctor then
       with FuncTrmPtr(lTrm)^ do
       begin
        case gRevReq[FuncNr] of
        rqRealAdd, rqRealMult, rqRealDiff:
          begin
           lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           lArg2:=Trms[FuncArgs^.NextTrm^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           if (lArg1 <> t) and (lArg2 <> t) then
           begin
             ArgsNr[t].InsertElem(lArg1);
             ArgsNr[t].InsertElem(lArg2);
             for i1:=0 to Trms[lArg1].PolynomialValues.Count-1 do
              for i2:=0 to Trms[lArg2].PolynomialValues.Count-1 do
              begin
               lPolynomial1:=PolynomialPtr(PolynomialPtr(Trms[lArg1].PolynomialValues.Items^[i1])^.MCopy);
               lPolynomial2:=PolynomialPtr(PolynomialPtr(Trms[lArg2].PolynomialValues.Items^[i2])^.MCopy);
               case gRevReq[FuncNr] of
                rqRealAdd:
                 lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,lPolynomial2));
                rqRealMult:
                 lPolynomialValues[t].Insert(MultPolynomials(lPolynomial1,lPolynomial2));
                rqRealDiff:
                 lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,NMultPolynomial(CMinusOne,lPolynomial2)));
               end;
               dispose(lPolynomial2,Done);
               dispose(lPolynomial1,Done);
              end;
           end;
          end;
        rqRealNeg:
          begin
           lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           if lArg1 <> t then
           begin
             ArgsNr[t].InsertElem(lArg1);
             for i1:=0 to Trms[lArg1].PolynomialValues.Count-1 do
              begin
               lPolynomial1:=PolynomialPtr(PolynomialPtr(Trms[lArg1].PolynomialValues.Items^[i1])^.MCopy);
               lPolynomialValues[t].Insert(NMultPolynomial(CMinusOne,lPolynomial1));
               dispose(lPolynomial1,Done);
              end;
           end;
          end;
        rqRealDiv:
          begin
           lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           lArg2:=Trms[FuncArgs^.NextTrm^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           RatInv(Trms[lArg2].NumValue,lVal);
           if lVal.Determined and (lArg1 <> t) then
            begin
             ArgsNr[t].InsertElem(lArg1);
             for i1:=0 to Trms[lArg1].PolynomialValues.Count-1 do
             begin
              lPolynomial1:=PolynomialPtr(PolynomialPtr(Trms[lArg1].PolynomialValues.Items^[i1])^.MCopy);
              lPolynomialValues[t].Insert(NMultPolynomial(lVal.NumericValue,lPolynomial1));
              dispose(lPolynomial1,Done);
             end
            end
          end;
        end;
       end;
     end;
    end;
   end;
  lAddedValues.Init(0,4);
  for t:=1 to TrmNbr do
   with TrmS[t] do
   begin
    lValues[t].Init(0);
    for i:=0 to lPolynomialValues[t].Count-1 do
     if not PolynomialValues.Search(lPolynomialValues[t].Items^[i],lCount) then
      begin
       lValues[t].Insert(lPolynomialValues[t].Items^[i]);
       PolynomialValues.AtInsert(lCount, lPolynomialValues[t].Items^[i]);
       lAddedValues.InsertElem(t);
      end
     else dispose(PolynomialPtr(lPolynomialValues[t].Items^[i]),Done);
    lPolynomialValues[t].DeleteAll;
   end;
{$IFDEF MDEBUG}
writeln(InfoFile,'---------------------- w trakcie AddValues');
infoeqclasses;
writeln(InfoFile,'---------------------- koniec wydruku w trakcie AddValues');
{$ENDIF};
 lLevel:=0;
 repeat
  for k:=0 to lAddedValues.Count-1 do
   begin
//with lAddedValues.Items^[0] do
//writeln(infofile,'obrabia added: ',X,'->',Y);
    with lAddedValues.Items^[0] do
     for t:=1 to TrmNbr do
      with TrmS[t] do
       if (T <> X) and ArgsNr[t].HasInDom(X) then
       begin
         mizassert(4623,PolynomialValues.Count <> 0);
         lEqClass:=EqClass;
         while lEqClass <> nil do
         begin
          lTrm:=lEqClass^.XTrmPtr;
          lEqClass:=lEqClass^.NextTrm;
          if lTrm^.TrmSort = ikTrmFunctor then
           with FuncTrmPtr(lTrm)^ do
           begin
            case gRevReq[FuncNr] of
            rqRealAdd, rqRealMult, rqRealDiff:
              begin
               lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
               lArg2:=Trms[FuncArgs^.NextTrm^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
               if (lArg1 = X) and (lArg2 = X) then
                begin
                 for i:=0 to lValues[X].Count-1 do
                 begin
                   lPolynomial1:=lValues[X].Items^[i];
                   case gRevReq[FuncNr] of
                    rqRealAdd:
                     lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,lPolynomial1));
                    rqRealMult:
                     lPolynomialValues[t].Insert(MultPolynomials(lPolynomial1,lPolynomial1));
                    rqRealDiff:
                     lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,NMultPolynomial(CMinusOne,lPolynomial1)));
                   end;
                 end;
                end
               else if (lArg1 = X) and (lArg2 <> t) then
                begin
                 for i:=0 to lValues[X].Count-1 do
                 begin
                   lPolynomial1:=lValues[X].Items^[i];
                   for i2:=0 to Trms[lArg2].PolynomialValues.Count-1 do
                   begin
                    lPolynomial2:=PolynomialPtr(PolynomialPtr(Trms[lArg2].PolynomialValues.Items^[i2])^.MCopy);
                    case gRevReq[FuncNr] of
                     rqRealAdd:
                      lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,lPolynomial2));
                     rqRealMult:
                      lPolynomialValues[t].Insert(MultPolynomials(lPolynomial1,lPolynomial2));
                     rqRealDiff:
                      lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,NMultPolynomial(CMinusOne,lPolynomial2)));
                    end;
                    dispose(lPolynomial2,Done);
                   end;
                 end;
                end
               else  if (lArg1 <> t) and (lArg2 = X) then
                begin
                 for i:=0 to lValues[X].Count-1 do
                 begin
                   lPolynomial2:=lValues[X].Items^[i];
                   for i1:=0 to Trms[lArg1].PolynomialValues.Count-1 do
                   begin
                    lPolynomial1:=PolynomialPtr(PolynomialPtr(Trms[lArg1].PolynomialValues.Items^[i1])^.MCopy);
                    case gRevReq[FuncNr] of
                     rqRealAdd:
                      lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,lPolynomial2));
                     rqRealMult:
                      lPolynomialValues[t].Insert(MultPolynomials(lPolynomial1,lPolynomial2));
                     rqRealDiff:
                      lPolynomialValues[t].Insert(AddPolynomials(lPolynomial1,NMultPolynomial(CMinusOne,lPolynomial2)));
                    end;
                    dispose(lPolynomial1,Done);
                   end;
                 end;
                end
              end;
            rqRealNeg:
              begin
               lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
               if lArg1 = X then
                 for i:=0 to lValues[X].Count-1 do
                  lPolynomialValues[t].Insert(NMultPolynomial(CMinusOne,lValues[X].Items^[i]));
              end;
            rqRealDiv:
              begin
               lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
               if lArg1 = X then
               begin
                lArg2:=Trms[FuncArgs^.NextTrm^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
                RatInv(Trms[lArg2].NumValue,lVal);
                if lVal.Determined then
                 for i:=0 to lValues[X].Count-1 do
                  lPolynomialValues[t].Insert(NMultPolynomial(lVal.NumericValue,lValues[X].Items^[i]));
               end;
              end;
            end;
           end;
         end;
//infoeqclass(t);
       end;
   end;
  inc(lLevel);
  if lLevel > 5 then break;
  for t:=1 to TrmNbr do
   with TrmS[t] do
   begin
    lValues[t].DeleteAll;
//    lValues[t].Init(0);
    for i:=0 to lPolynomialValues[t].Count-1 do
     if not PolynomialValues.Search(lPolynomialValues[t].Items^[i],lCount) then
      begin
       lValues[t].Insert(lPolynomialValues[t].Items^[i]);
       PolynomialValues.AtInsert(lCount, lPolynomialValues[t].Items^[i]);
       lAddedValues.InsertElem(t);
      end
     else dispose(PolynomialPtr(lPolynomialValues[t].Items^[i]),Done);
    lPolynomialValues[t].DeleteAll;
   end;
 until lAddedValues.Count = 0;
 lAddedValues.Done;
 for t:=1 to TrmNbr do
  begin
   lPolynomialValues[t].Done;
   lValues[t].DeleteAll;
   lValues[t].Done;
  end;
end;

//Start of RoundUpSuperCluster

type
   ANClusterPtr = ^ANClusterObj;
   ANClusterObj =
   object(MObject)
     nInd: integer;
     nCluster: AttrCollectionPtr;
     constructor Init(aInd: integer; aCluster: AttrCollectionPtr);
     destructor Done; virtual;
   end;

//Start of ANInstCollection

type
 ANInstCollectionPtr = ^ANInstCollection;
 ANInstCollection =
  object(MCollection)
    Status: TLatStatus;

   procedure InsertAndAbsorb(fElem:NatFuncPtr);
   procedure UnionWith(fColl:ANInstCollectionPtr);
   procedure JoinWith(fColl:ANInstCollectionPtr); virtual;
   constructor InitTop;
   constructor InitBottom;
   destructor Done; virtual;
   constructor InitSingle(fAtom:NatFuncPtr);

{$IFDEF MDEBUG}
   procedure InfoLatColl;
{$ENDIF}
  end;

{$IFDEF MDEBUG}
procedure InfoSubstAN(aSubst: NatFuncPtr);
  var k: integer;
begin
 with aSubst^ do
  begin
   for k:=0 to Count-1 do
    with Items^[k] do
    begin
     if k > 0 then
     write(infofile,', ');
     write(infofile,'A',X,' <- ','E',Y);
    end;
  end;
end;
procedure ANInstCollection.InfoLatColl;
  var z: integer;
begin writeln(InfoFile,'Count=',Count);
 for z:=0 to Count-1 do
  if Items = nil then break
  else
  begin
   write(InfoFile,z,': ');
   InfoSubstAN(Items^[z]);
   infonewline;
  end;
end;
{$ENDIF}

const NilPtr : pointer = nil;

constructor ANInstCollection.InitTop;
begin Items:=NilPtr;
 Count:=1;
 Limit:=1;
 Delta:=0;
 Status:=Top;
end;

constructor ANInstCollection.InitBottom;
begin Items:=nil;
 Count:=0;
 Limit:=0;
 Delta:=0;
 Status:=Regular;
end;

destructor ANInstCollection.Done;
begin
  if status=top then
   begin count:=0;
    limit:=0;
   end
  else inherited Done;
end;

constructor ANInstCollection.InitSingle(fAtom:NatFuncPtr);
begin
 Init(1,0); Count:=1; AtPut(0,fAtom); Status:=Regular;
end;

//Below is the difference compared to PreInstCollection

procedure ANInstCollection.InsertAndAbsorb(fElem:NatFuncPtr);
 var i:integer; lAtom:NatFuncPtr;
 label ItIsWeaker;
begin
  i:=0;
  while i < Count do
   begin lAtom:=Items^[i];
    case lAtom^.CompareWith(fElem^) of
     0: inc(i);
    -1: goto ItIsWeaker;
     1: begin dispose(fElem,Done); exit end;
    end;
   end;
  Insert(fElem); exit;
ItIsWeaker:
  FreeItem(lAtom); AtPut(i,fElem); inc(i);
  while i < Count do
   begin lAtom:=Items^[i];
     if lAtom^.WeakerThan(fElem^) then
      begin
       AtFree(i)
      end
    else inc(i);
   end;
end;

procedure ANInstCollection.UnionWith(fColl:ANInstCollectionPtr);
var z: integer;
begin
 if Status = Top then
   begin dispose(fColl,Done);
    exit
   end;
  if fColl^.Count = 0 then
   begin dispose(fcoll,Done);
    exit;
   end;
  if fColl^.Status = Top then
   begin
    Done;
    ANInstCollection.InitTop;
    dispose(fcoll,Done);
    exit
   end;
  SetLimit(Count+fColl^.Count);
  for z:=0 to fColl^.Count-1 do InsertAndAbsorb(fColl.Items^[z]);
  fColl^.DeleteAll;
  dispose(fcoll,Done);
  SetLimit(0);
  Status:=Regular;
end;

procedure ANInstCollection.JoinWith(fColl:ANInstCollectionPtr);
 var tTLC:ANInstCollection;
     lAtom,lAtom1: NatFuncPtr;
     z,z1: integer;
begin
 { Top & Bottom }
 if fColl^.Status = Top then begin dispose(fColl,Done); exit; end;
 if Status = Top then
 begin
    ANInstCollection.InitBottom;
    SetLimit(fColl^.count);
    for z:=0 to fColl^.Count-1 do Insert(fColl^.Items^[z]);
    fColl^.DeleteAll;
    dispose(fColl,Done);
    exit;
 end;
 if fColl^.Count = 0 then
  begin
     Done;
     ANInstCollection.InitBottom;
     dispose(fColl,Done);
     exit;
  end;
 if Count = 0 then
   begin dispose(fColl,Done);
    exit
   end;

  tTLC.Init(count*fcoll^.count,0);
  for z:=0 to count -1 do
   for z1:=0 to fcoll^.count -1 do
    begin
     lAtom:=NatFuncPtr(items^[z]);
     lAtom1:=lAtom^.JoinAtom(NatFuncPtr(fColl^.items^[z1]));
     if lAtom1<>nil then tTLC.Insert(lAtom1);
    end;
  Done;
  ANInstCollection.InitBottom;
  SetLimit(tTLC.count);
  for z:=0 to tTLC.count-1 do insert(tTLC.Items^[z]);
  tTLC.DeleteAll;
  tTLC.Done;
  dispose(fColl,Done);
  SetLimit(0);
  Status:=Regular;
{ Pewnie warto miec osobna informacje, czy to jest Bottom i raczej uzywac tego }
end;

//End of ANInstCollection
var
   gANInstantiation: NatFuncPtr;
   gFound: boolean;
   gY: Integer;

constructor ANClusterObj.Init(aInd : integer; aCluster: AttrCollectionPtr);
begin
 nInd:=aInd;
 nCluster:=aCluster;
end;

destructor ANClusterObj.Done;
begin
  dispose(nCluster,Done);
end;

//Checks if a formula contains D constants unavailable in the inference
procedure CheckForDConsts(var fTrm :TrmPtr );
begin
   if not gFound then
      if (fTrm^.TrmSort=ikTrmInfConst) then
	 if (VarTrmPtr(fTrm)^.VarNr>DTrm.Count-1) or (DTrm.Items^[VarTrmPtr(fTrm)^.VarNr]=nil) then gFound:=true;
end;

//Checks for a certain E class in arguments
procedure CheckForClass(var fTrm: TrmPtr );
begin
///!!! Must be outside of DependsOn, otherwise fTrm is wrong!
   //   writeln(infofile,'checking: ',ftrm^.trmsort,ftrm^.trminfo);
   if (not gFound) and (fTrm^.TrmSort=ikTrmEqConst) and (TrmS[fTrm^.TrmInfo].Term^.TrmInfo=gY)
      then gFound:=true;
end;

function AllowedAttributeInCluster(aCluster:AttrCollectionPtr):AttrCollectionPtr;
 var lCluster: AttrCollectionPtr;
     i: integer;
begin
  lCluster:=new(AttrCollectionPtr,Init(aCluster^.Count,0));
  with aCluster^ do
  for i:=0 to Count-1 do
   begin
    gFound:=false;
    AttrPtr(Items^[i])^.WithinAttr(CheckForDConsts);
    if not gFound then
     lCluster^.Insert(AttrPtr(Items^[i])^.CopyAttribute);
   end;
  AllowedAttributeInCluster:=lCluster;
end;

procedure InitAllowedClusters;
 var ANj: integer;
     lCluster: AttrCollectionPtr;
begin
   for ANj:=0 to ConditionalCluster.Count-1 do
    begin
      lCluster:=AllowedAttributeInCluster(CClusterPtr(ConditionalCluster.Items^[anj])^.nConsequent.upper);
      if lCluster.Count > 0 then
       AllowedCCluster.Insert(new(ANClusterPtr,Init(ANj,lCluster)))
      else dispose(lCluster,Done);
    end;
   for ANj:=0 to FunctorCluster.Count-1 do
    begin
      lCluster:=AllowedAttributeInCluster(FClusterPtr(FunctorCluster.Items^[anj])^.nConsequent.upper);
      if lCluster.Count > 0 then
       AllowedFCluster.Insert(new(ANClusterPtr,Init(ANj,lCluster)))
      else dispose(lCluster,Done);
    end;
end;

//Checks class dependence
function DependsOn(x,y:integer):boolean;
var
   al: TrmList;
begin
   if (TrmS[x].EqClass<>nil) and (TrmS[y].EqClass<>nil) then
   begin
      gFound:=false;
      gY:=y;
      with TrmS[x] do
      begin
       al:=EqClass;
       while al<>nil do
       begin
          WithinTerm(al^.XTrmPtr,@CheckForClass);
          al:=al^.NextTrm;
       end;
       if not gFound then WithinTypeColl(XTypClass,@CheckForClass);
       if not gFound then SuperCluster^.WithinAttrCollection(@CheckForClass);
      end;
      DependsOn:=gFound;
   end
   else DependsOn:=false
end;

//Calculates the set of dependent classes
function DependentClasses(enr:Integer):NatSetPtr;
var i: integer;
    r: NatSetPtr;
begin
   if TrmS[enr].EqClass<>nil then
    begin
      new(r,Init(10,10));
      for i:=1 to TrmNbr do
       if DependsOn(i,enr) then r^.InsertElem(i);
        DependentClasses:=r;
    end
   else DependentClasses:=nil;
end;

function InstantiateTerm(fcluster:PCollection;etrm,atrm:TrmPtr): ANInstcollectionptr; forward;
function InstantiateType(ccluster:PCollection;enr:integer;atyp:TypPtr): ANInstcollectionptr; forward;

//Matches loci with classes in attributes
function InstantiateAttr(fcluster:PCollection; enr:integer; aa:AttrPtr): ANInstcollectionptr;
var r,z,v,int1: ANInstCollectionPtr;
   el,al: TrmList;
   j,t,u: integer;
   f: boolean;
begin
   new(r,InitBottom);
   f:=false;
   for j:=0 to Trms[enr].SuperCluster^.Count-1 do
   begin
    AttrPtr(Trms[enr].SuperCluster^.Items^[j])^.AdjustAttr(t,el);
    aa^.AdjustAttr(u,al);
    if (t=u) and (AttrPtr(Trms[enr].SuperCluster^.Items^[j])^.fNeg=aa^.fNeg)
     then
      begin
       if al=nil then
        begin
          dispose(r,Done); //1
          new(z,InitTop);
          InstantiateAttr:=z;
          exit;
        end;
       f:=true;
       new(v,InitTop);
       while al<>nil do
        begin
          int1:=instantiateTerm(fcluster,el^.XTrmPtr,al^.XTrmPtr);
          v^.JoinWith(int1);
          al:=al^.NextTrm;
          el:=el^.NextTrm;
        end;
       r^.UnionWith(v);
      end;
   end;
   if not f then
   begin
      dispose(r,Done);
      new(r,InitBottom);
      instantiateAttr:=r;
      exit;
   end;
   instantiateattr:=r;
end;

//Matches loci with classes in attribute collections
function InstantiateAttrs(fcluster:PCollection; enr:integer; alc:AttrCollectionPtr): ANInstcollectionptr;
var r,int1: ANInstCollectionPtr;
   i: integer;
begin
   new(r,InitTop);
   for i:=0 to alc^.Count-1 do
   begin
      int1:=InstantiateAttr(fcluster,enr,AttrPtr(alc^.Items^[i]));
      r^.JoinWith(int1);
   end;
   instantiateattrs:=r;
end;

//Matches loci with classes in terms
function InstantiateTerm(fcluster:PCollection; etrm,atrm:trmptr ): ANInstcollectionptr;
var r,z,v,int1: ANInstCollectionPtr;
   enr,t,u: integer;
   tl,el,al,atl: TrmList;
   n: NatFuncPtr;
   t1: TypPtr;
begin
(*  {$IFDEF MDEBUG}
  writeln(infofile,'InstantiateTerm:');
  infostring('e=');infoterm(etrm);writeln(infofile,' trminfo=',etrm^.TrmInfo);
  infostring('a=');infoterm(atrm);writeln(infofile,' trminfo=',atrm^.TrmInfo);
  flush(infofile);
  {$ENDIF} *)
   if eTrm^.TrmSort<>ikTrmEqConst then
    begin
      mizassert(1898,etrm^.TrmSort in [ikTrmFunctor,ikTrmNumeral,ikTrmSelector,ikTrmAggreg]);
      if eTrm^.TrmSort=ikTrmNumeral then
       if (aTrm^.TrmSort=ikTrmNumeral) and (VarTrmPtr(eTrm)^.VarNr=VarTrmPtr(aTrm)^.VarNr) then
        begin
          new(z,InitTop);
          InstantiateTerm:=z;
          exit;
        end
       else
        begin
          new(z,InitBottom);
          InstantiateTerm:=z;
          exit;
        end;
      if (eTrm^.TrmSort=atrm^.TrmSort) then
       begin
       if eTrm^.TrmSort=ikTrmFunctor then
        begin AdjustTrm(eTrm,t,el);
          AdjustTrm(aTrm,u,al);
        end
       else
        begin
          t:=FuncTrmPtr(eTrm)^.FuncNr;
          el:=FuncTrmPtr(eTrm)^.FuncArgs;
          u:=FuncTrmPtr(aTrm)^.FuncNr;
          al:=FuncTrmPtr(aTrm)^.FuncArgs;
        end;
       if t=u then
        begin
          new(r,InitTop);
          while el<>nil do
           begin
             int1:=instantiateTerm(fcluster,el^.XTrmPtr,al^.XTrmPtr);
             r^.JoinWith(int1);
             el:=el^.NextTrm;
             al:=al^.NextTrm;
           end;
          InstantiateTerm:=r;
          exit;
        end;
       end
      else
       begin
        new(r,InitBottom);
        InstantiateTerm:=r;
        exit;
       end;
    end;
   enr:=Trms[etrm^.TrmInfo].Term^.TrmInfo; //??? really needed TrmInfo twice?
   if atrm^.TrmSort=ikTrmLocus then
    begin
      with TrmS[enr] do
      begin
       t1:=fcluster^.Items^[VarTrmPtr(atrm)^.VarNr-1];
       z:=InstantiateType(fcluster,enr,t1);
       if z^.Count=0 then
        begin
          InstantiateTerm:=z;
          exit;
        end;
// int2:=InstantiateRadixType(fcluster,enr,t1);
// z^.JoinWith(int2);
// if z^.Count=0 then
// begin
//    InstantiateTerm:=z;
//    exit;
// end;
       new(n,InitNatFunc(1,1));
       n^.Assign(VarTrmPtr(atrm)^.VarNr,VarTrmPtr(TrmS[enr].Term)^.VarNr);
       new(r,InitSingle(n));
       z^.JoinWith(r);
       InstantiateTerm:=z;
      end;
    end
   else
   begin
      mizassert(1888,atrm^.TrmSort in [ikTrmFunctor,ikTrmNumeral,ikTrmSelector,ikTrmAggreg]);
      if aTrm^.TrmSort=ikTrmNumeral then
       begin
        if (TrmS[enr].NumValue.Determined) and
            (CompareComplex(TrmS[enr].NumValue.NumericValue,IntToComplex(VarTrmPtr(aTrm)^.VarNr))=0) then
         begin
           new(z,InitTop);
           InstantiateTerm:=z;
           exit;
         end;
        new(z,InitBottom);
        InstantiateTerm:=z;
        exit;
       end;
      if aTrm^.TrmSort=ikTrmFunctor then AdjustTrm(aTrm,u,atl)
      else
       begin
        u:=FuncTrmPtr(aTrm)^.FuncNr;
        atl:=FuncTrmPtr(aTrm)^.FuncArgs;
       end;
      new(r,InitBottom);
      with trms[enr] do
      begin
       tl:=EqClass;
       while tl<>nil do
        begin
          if tl^.XTrmPtr^.TrmSort=aTrm^.TrmSort then
          begin
             if tl^.XTrmPtr^.TrmSort=ikTrmFunctor then AdjustTrm(tl^.XTrmPtr,t,el)
             else
                begin t:=FuncTrmPtr(tl^.XTrmPtr)^.FuncNr;el:=FuncTrmPtr(tl^.XTrmPtr)^.FuncArgs;
                end;
             if t=u then
             begin
              new(z,InitTop);
              al:=atl;
              while el<>nil do
              begin
                 v:=InstantiateTerm(fcluster,el^.XTrmPtr,al^.XTrmPtr);
                 z^.JoinWith(v);
                 el:=el^.NextTrm;
                 al:=al^.NextTrm;
              end;
              r^.UnionWith(z);
             end;
          end;
          tl:=tl^.NextTrm;
        end;
       instantiateTerm:=r;
      end;
   end;
end;

//Matches loci with classes in radix types
function InstantiateRadixType(ccluster:PCollection; enr:integer; atyp:TypPtr): ANInstcollectionptr;
var r,z,int2: ANInstCollectionPtr;
   i,t,u: integer;
   el,al,atl,atm: TrmList;
begin
   enr:=Trms[enr].Term^.TrmInfo;
   atyp^.AdjustTyp(t,atl);
   new(r,InitBottom);
   with TrmS[enr] do
   begin
      for i:=0 to XTypClass.Count-1 do
      with TypPtr(XTypClass.Items^[i])^ do
       begin
        AdjustTyp(u,atm);
        if (TypSort=atyp^.TypSort) and (u=t) then
        begin
          if atl=nil then
           begin
            dispose(r,Done);
            new(r,InitTop);
            InstantiateRadixType:=r;
            exit;
           end;
          new(z,InitTop);
          el:=atm;
          al:=atl;
          while al<>nil do
           begin
            int2:=instantiateTerm(ccluster,el^.XTrmPtr,al^.XTrmPtr);
            z^.JoinWith(int2);
            el:=el^.NextTrm;
            al:=al^.NextTrm;
           end;
          r^.UnionWith(z);
        end;
       end;
      InstantiateRadixType:=r;
   end;
end;

//Matches loci with classes in types
function InstantiateType(ccluster:PCollection; enr:integer; atyp:TypPtr): ANInstcollectionptr;
var r,s: ANInstCollectionPtr;
begin
   r:=InstantiateRadixType(ccluster,enr,atyp);
   s:=InstantiateAttrs(ccluster,enr,atyp^.LowerCluster);
   r^.JoinWith(s);
   InstantiateType:=r;
end;

//Matches loci with classes in term registrations
function InstantiateFCluster(fcluster:fclusterptr; enr:integer): ANInstcollectionptr;
var r,s : ANInstCollectionPtr;
begin
   r:=InstantiateTerm(@fcluster^.nPrimaryList,Trms[enr].Term,fcluster^.nClusterTerm);
   if fcluster^.nClusterType<>nil then
   begin
      s:=InstantiateType(@fcluster^.nPrimaryList,enr,fcluster^.nClusterType);
      r^.JoinWith(s);
   end;
   InstantiateFCluster:=r;
end;

//Matches loci with classes in conditional registrations
function InstantiateCCluster(ccluster:cclusterptr; enr:integer): ANInstcollectionptr;
var r,s: ANInstCollectionPtr;
begin
   r:=InstantiateType(@ccluster^.nPrimaryList,enr,ccluster^.nClusterType);
// the attributes from the type are copied to the antecedent, so it should be enough to use RadixType above
   s:=InstantiateAttrs(@ccluster^.nPrimaryList,enr,ccluster^.nAntecedent);
   r^.JoinWith(s);
   InstantiateCCluster:=r;
end;

function LocateTerm(aTrm:TrmPtr): TrmPtr; FORWARD;

//Tries to match a list of terms using the function below
function LocateTermList(aList:TrmList): TrmList;
var l,la : TrmList;
    t:TrmPtr;
begin
   la:=nil;
   l:=aList;
   while l<>nil do
   begin
      t:=LocateTerm(l^.XTrmPtr);
      if t=nil then
      begin
       if la<>nil then DisposeListOfTerms(la);
       LocateTermList:=nil;
       exit;
      end;
      la:=AddToTrmList(la,t);
      l:=l^.NextTrm;
   end;
   LocateTermList:=la;
end;

//Tries to match a (possibly external) term to an existing class
//It depends on gANInstantiation! - should probably be changed to another parameter
function LocateTerm(aTrm:TrmPtr): TrmPtr;
var i,g : Integer; l,la:TrmList;
begin
  LocateTerm:=nil;
  case aTrm^.TrmSort of
   ikTrmLocus:
    begin
       g:=gANInstantiation^.Value(VarTrmPtr(aTrm)^.VarNr);
       for i:=1 to TrmNbr do
        if VarTrmPtr(TrmS[i].Term)^.VarNr=g then
          begin LocateTerm:=TrmS[i].Term;
           exit;
          end;
    end;
   ikTrmInfConst:
    for i:=1 to TrmNbr do
     begin
       l:=Trms[i].EqClass;
       while l<>nil do
       begin
        if (l^.XTrmPtr^.TrmSort=ikTrmInfConst) and (VarTrmPtr(l^.XTrmPtr)^.VarNr=VarTrmPtr(aTrm)^.VarNr) then
        begin
           LocateTerm:=TrmS[i].Term;
           exit;
        end;
        l:=l^.NextTrm;
       end;
     end;
   ikTrmNumeral:
    for i:=1 to TrmNbr do
     with Trms[i], NumValue do
      if (EqClass <> nil) and Determined and
         IsEQWithInt(NumericValue,VarTrmPtr(aTrm)^.VarNr) then
       begin LocateTerm:=TrmS[i].Term;
        exit;
       end;
   ikTrmFunctor,ikTrmSelector,ikTrmAggreg:
    for i:=1 to TrmNbr do
     with Trms[i] do
      if EqClass<>nil then
      begin
       l:=EqClass;
       while l<>nil do
       begin
        if (l^.XTrmPtr^.TrmSort=aTrm^.TrmSort) and (FuncTrmPtr(l^.XTrmPtr)^.FuncNr=FuncTrmPtr(aTrm)^.FuncNr) then
         begin
          with FuncTrmPtr(l^.XTrmPtr)^ do
           begin
             if FuncArgs=nil then
               begin LocateTerm:=TrmS[i].Term;
                exit;
               end;
              la:=LocateTermList(FuncTrmPtr(aTrm)^.FuncArgs);
              if (la<>nil) and (EqTrmLists(la,FuncArgs)) then
              begin
               DisposeListofTerms(la);
               LocateTerm:=TrmS[i].Term;
               exit;
              end;
           end;
         end;
        l:=l^.NextTrm;
       end;
      end;
   end;
end;

//Converts attribute arguments to respective E classes
function LocatedAttr(aAttr:AttrPtr): AttrPtr;
var a: AttrPtr;
    l: TrmList;
begin
  if aAttr^.fAttrArgs=nil then LocatedAttr:=aAttr^.CopyAttribute
  else
   begin
      l:=LocateTermList(aAttr^.fAttrArgs);
      if l=nil then LocatedAttr:=nil else
      begin
       a:=AttrPtr(PObject(aAttr)^.CopyObject);
       a^.fAttrArgs:=l;
       LocatedAttr:=a;
      end;
   end;
end;

//Converts attribute collections to hold attributes with E arguments
function LocatedCluster(aCluster:AttrCollectionPtr): AttrCollectionPtr;
var i: Integer;
    r:AttrCollectionPtr;
    a:AttrPtr;
begin
   new(r,Init(10,10));
   for i:=0 to aCluster^.Count-1 do
   begin
      a:=LocatedAttr(AttrPtr(aCluster^.Items^[i]));
      if a<>nil then r^.Insert(a);
   end;
   LocatedCluster:=r;
end;

//Applies all possible registrations to a given E class
function RoundUpSuperCluster(enr:Integer): Boolean;
var r,added: Boolean;
   i,j,c: Integer;
   inst: ANInstCollectionPtr;
   ANCluster,LCluster: AttrCollectionPtr;
begin
  r:=false;
  with TrmS[enr] do
   begin
    if EqClass=nil then
     begin RoundUpSuperCluster:=false;
      exit;
     end;
//AN We repeat the loop as rounding must be independent of the order of registrations!
    added:=true;
    while added do
     begin
       added:=false;
       for i:=0 to AllowedCCluster.Count-1 do
        with ANClusterPtr(AllowedCCluster.items^[i])^ do
        begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'CClusternr=',i);
{$ENDIF}
          inst:=InstantiateCCluster(cclusterptr(conditionalcluster.items^[nInd]),enr);
          if inst^.status=TOP then
           begin
            ANCluster:=nCluster;
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('clusterupper=');infocluster(ancluster);infonewline;
{$ENDIF}
            LCluster:=LocatedCluster(ANCluster);
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('locatedcluster=');infocluster(lcluster);infonewline;
infostring('supercluster=');infocluster(supercluster);infonewline;
{$ENDIF}
            c:=supercluster.count;
            SuperCluster^.EnlargeBy(LCluster);
            if supercluster^.count<>c then
             begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'ADDEDC1:',supercluster^.count,' ',c);
{$ENDIF}
              r:=true;
              added:=true;
             end;
             dispose(lcluster,Done);
            if not SuperCluster^.fConsistent then
             begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'FOUNDC1');
{$ENDIF}
               SetContr(8);
               dispose(inst,Done);
               RoundUpSuperCluster:=true;
               exit;
             end;
           end
          else
           for j:=0 to inst^.count-1 do
            begin
             gANInstantiation:=inst^.Items^[j];
             ANCluster:=nCluster;
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('clusterupper=');infocluster(ancluster);infonewline;
{$ENDIF}
             LCluster:=LocatedCluster(ANCluster);
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('locatedcluster=');infocluster(lcluster);infonewline;
infostring('supercluster=');infocluster(supercluster);infonewline;
{$ENDIF}
             c:=supercluster.count;
             SuperCluster^.EnlargeBy(LCluster);
             if supercluster^.count<>c then
              begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'ADDEDC2:',supercluster^.count,' ',c);
{$ENDIF}
                 r:=true;
                 added:=true;
              end;
             dispose(LCluster,Done);
             if not SuperCluster^.fConsistent then
              begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'FOUNDC2');
{$ENDIF}
                 SetContr(9);
                 dispose(inst,Done);
                 RoundUpSuperCluster:=true;
                 exit;
              end;
            end;
          dispose(inst,Done);
        end;
       for i:=0 to AllowedFCluster.Count-1 do
        with ANClusterPtr(AllowedFCluster.items^[i])^ do
         begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'FClusternr=',i);
{$ENDIF}
          inst:=InstantiateFCluster(fclusterptr(functorcluster.items^[nInd]),enr);
          if inst^.status=TOP then
           begin
            ANCluster:=nCluster;
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('clusterupper=');infocluster(ancluster);infonewline;
{$ENDIF}
            LCluster:=LocatedCluster(ANCluster);
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('locatedcluster=');infocluster(lcluster);infonewline;
infostring('supercluster=');infocluster(supercluster);infonewline;
{$ENDIF}
            c:=supercluster.count;
            SuperCluster^.EnlargeBy(LCluster);
            if supercluster^.count<>c then
             begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'ADDEDF1:',supercluster^.count,' ',c);
{$ENDIF}
              r:=true;
              added:=true;
             end;
            dispose(lcluster,Done);
            if not SuperCluster^.fConsistent then
             begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'FOUNDF1');
{$ENDIF}
              SetContr(10);
              dispose(inst,Done);
              RoundUpSuperCluster:=true;
              exit;
             end;
           end
          else
           begin
            for j:=0 to inst^.count-1 do
             begin
              gANInstantiation:=inst^.Items^[j];
              ANCluster:=nCluster;
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('clusterupper=');infocluster(ancluster);infonewline;
{$ENDIF}
              LCluster:=LocatedCluster(ANCluster);
{$IFDEF DEBUGSUPERCLUSTERS}
infostring('locatedcluster=');infocluster(lcluster);infonewline;
infostring('supercluster=');infocluster(supercluster);infonewline;
{$ENDIF}
              c:=supercluster.count;
              SuperCluster^.EnlargeBy(LCluster);
              if supercluster^.count<>c then
              begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'ADDEDF2:',supercluster^.count,' ',c);
{$ENDIF}
               r:=true;
               added:=true;
              end;
              dispose(lcluster,Done);
              if not SuperCluster^.fConsistent then
              begin
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'FOUNDF2');
{$ENDIF}
               SetContr(11);
               dispose(inst,Done);
               RoundUpSuperCluster:=true;
               exit;
              end;
             end;
           end;
          dispose(inst,Done);
         end;
     end;
   end;
  RoundUpSuperCluster:=r;
end;

//End of RoundUpSuperCluster

procedure InitEmptyInEqClass(var lEqPendings: IntRel);
 var ii: integer;
     lAttr: AttrPtr;
     lUniqueTrm: TrmPtr;
begin
//  Empty
 if gBuiltIn[rqEmptySet] <> 0 then
 begin
  ii:=1;
  while ii <= TrmNbr do
   with TrmS[ii] do
   begin
    if  EqClass<>nil then
     begin
      lAttr:=SuperCluster^.GetAttr(gBuiltIn[rqEmpty],nil);
      if (lAttr<>nil) and (lAttr^.fNeg=ord(true)) then
        begin lUniqueTrm:=NewFuncTrm(gBuiltIn[rqEmptySet],nil);
         YTerm(lUniqueTrm);
//         UnionTrms(ii,lUniqueTrm^.TrmInfo);
         if ii <> lUniqueTrm^.TrmInfo then
          AddEquality(lEqPendings,ii,lUniqueTrm^.TrmInfo);
        end;
     end;
    inc(ii);
   end;
 end;
 if gBuiltIn[rqZeroNumber] <> 0 then
 begin
  ii:=1;
  while ii <= TrmNbr do
   with TrmS[ii] do
   begin
    if  EqClass<>nil then
     begin
      lAttr:=SuperCluster^.GetAttr(gBuiltIn[rqZero],nil);
      if (lAttr<>nil) and (lAttr^.fNeg=ord(true)) then
        begin lUniqueTrm:=NewFuncTrm(gBuiltIn[rqZeroNumber],nil);
         YTerm(lUniqueTrm);
//         UnionTrms(ii,lUniqueTrm^.TrmInfo);
         if ii <> lUniqueTrm^.TrmInfo then
          AddEquality(lEqPendings,ii,lUniqueTrm^.TrmInfo);
        end;
     end;
    inc(ii);
   end;
 end;
end;

function IsStrict(Item:AttrPtr):boolean;
begin
 with ConstrPtr( Constr[ coAttribute].At( Item^.fAttrNr))^ do
  IsStrict:= (syAbstractness in fProperties) and (Item^.fNeg = 1);
end;

procedure InitStructuresInEqClass;
 var ii,x,z,lStrictStruct: integer;
     lEqs: IntRel;
     lTrm: TrmPtr;
begin
// Structury
  { Nie obrabiamy na razie selektorowych fuktorow majacych typ "strict" }
  ii:=1;
  while ii <= TrmNbr do
   with TrmS[ii] do
   begin
     if  EqClass<>nil then
     begin
       lStrictStruct:=0;
       with SuperCluster^ do
        for x:=0 to Count-1 do
         with AttrPtr(Items^[x])^,
          ConstrTypPtr(Constr[ coAttribute].Items^[fAttrNr])^.fConstrTyp^ do
          if IsStrict( AttrPtr(Items^[x])) then
           if lStrictStruct = 0 then lStrictStruct:=ModNr
            { Nie uwzgledniamy roznych ukladow "over" }
           else if ModNr <> lStrictStruct then
            begin
             SetContr(12);
             exit;
            end;
       if lStrictStruct <> 0 then
       begin
        lEqs.Init(8);
        with XTypClass do
        for z:=0 to Count-1 do
         with TypPtr(Items^[z])^ do
          if (TypSort = ikTypStruct) and (lStrictStruct = ModNr) then
          begin
           lTrm:=ReconAggregTrm(lStrictStruct,Term,TypPtr(Items^[z])^.CopyType);
           if lTrm^.TrmSort <> ikTrmError then
            begin
             YTerm(lTrm);
//             UnionTrms(agTrm^.TrmInfo,Term^.TrmInfo);
             if lTrm^.TrmInfo <> Term^.TrmInfo then
             AddEquality(lEqs,lTrm^.TrmInfo,Term^.TrmInfo);
            end
           else DisposeTrm(lTrm);
          end;
         while lEqs.Count > 0 do
          begin
           with lEqs.Items^[0] do UnionTrms(X,Y);
           lEqs.AtDelete(0);
          end;
       end;
     end;
     inc(ii);
   end;
end;

//Executes one reduction
procedure ExecuteReduction(enr,aRedNr:Integer);
var FuncNr1: integer; A1: TrmList;
begin
 with ReductionPtr(gReductions.At(aRedNr))^ do
  if nTerms[1]^.TrmSort = ikTrmFunctor then
   begin
    AdjustTrm(nTerms[1],FuncNr1,A1);        
    UnionTrms(enr,LocateTerm(NewFuncTrm(FuncNr1,A1)).TrmInfo);
   end else
    UnionTrms(enr,LocateTerm(nTerms[1]).TrmInfo);
end;

//Applies all possible reductions to a given E class
function ProcessReductionsAN(enr:Integer):boolean;
var i: Integer;
   inst0: ANInstCollectionPtr;
begin
 ProcessReductionsAN:=false;
 with TrmS[enr] do
  begin
   if EqClass = nil then exit;
   for i:=0 to gReductions.Count-1 do
    with ReductionPtr(gReductions.At(i))^ do
     begin
      inst0:=InstantiateTerm(@nPrimaryList,Term,nTerms[0]);
      case inst0^.Status of
       Regular:
        if inst0^.count > 0 then
         begin
          gANInstantiation:=inst0^.At(0);
          ExecuteReduction(enr,i);
          ProcessReductionsAN:=true;
         end;
       Top:
        begin
         ExecuteReduction(enr,i);
         ProcessReductionsAN:=true;
        end;
      end;
      dispose(inst0,Done);
     end;
  end;
end;

procedure ProcessReductions;
var k:integer;
begin
 if TrmNbr > 0 then
  for k:=0 to DTrm.Count-1 do
   if DTrm.At(k) <> nil then
    ProcessReductionsAN(TrmPtr(DTrm.At(k)).TrmInfo);
end;

function IsComplexNumber(aCluster: AttrCollectionPtr): boolean;
 var lAttr: AttrPtr;
begin
 lAttr:=aCluster^.GetAttr(gBuiltIn[rqComplex],nil);
 IsComplexNumber:=(lAttr<>nil) and (lAttr^.fNeg=ord(true));
end;

procedure InsertComplex(aNr: integer);
begin
  with TrmS[aNr] do
  begin
   MizAssert(7321,EqClass <> nil);
   if not IsComplexNumber(SuperCluster) then
    begin
      SuperCluster^.InsertAttr(gBuiltIn[rqComplex],ord(true),nil);
      if not SuperCluster^.fConsistent then SetContr(13);
    end;
  end;
end;

procedure InitSuperClusterForComplex;
 var ii: integer;
     lTL: TrmList;
begin
// inserting attribute complex into a SuperCluster of a EqClass
// which contains an standard complex operations ( *,+,-,/ )
// or it is an argument of it
  if gBuiltIn[rqComplex] = 0 then exit;

  for ii:=1 to TrmNbr do
  with TrmS[ii] do
   begin
     LTL:=EqClass;
     while LTL<>nil do
     begin
      with FuncTrmPtr(LTL^.XTrmPtr)^ do
      case TrmSort of
      ikTrmInfConst:
       if NumValue.Determined then
        InsertComplex(ii);
      ikTrmFunctor:
       case gRevReq[FuncNr] of
       rqImaginaryUnit:
        InsertComplex(ii);
       rqRealAdd,  rqRealMult, rqRealDiff, rqRealDiv:
        begin
         InsertComplex(Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo);
         InsertComplex(Trms[FuncArgs^.NextTrm^.XTrmPtr^.TrmInfo].Term^.TrmInfo);
         InsertComplex(ii);
        end;
       rqRealNeg, rqRealInv:
        begin
         InsertComplex(Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo);
         InsertComplex(ii);
        end;
       end;
      end;
      lTL:=lTL^.NextTrm;
     end;
   end;
end;

function IsItEmptySet(aTrmList: TrmList): boolean;
begin
 while aTrmList <> nil do
 begin
  with FuncTrmPtr(aTrmList^.XTrmPtr)^ do
   if (TrmSort=ikTrmFunctor) and (FuncNr=gBuiltIn[rqEmptySet]) then
     begin IsItEmptySet:=true;
      exit;
     end
    else aTrmList:=aTrmList^.NextTrm;
 end;
 IsItEmptySet:=false;
end;

procedure EquateComplexValue(aTrmInfo:integer; V:ValRec);
 var i: Integer;
begin
(*** if V <= MaxConstInt then***)
  if Trms[aTrmInfo].NumValue.Determined then
   begin
     if not AreEqComplex(Trms[aTrmInfo].NumValue.NumericValue,V.NumericValue) then
       begin SetContr(14);
        exit
       end
   end
  else
   begin
     for i:=1 to TrmNbr do
      if Trms[i].EqClass<>nil then
       if Trms[i].NumValue.Determined and
          AreEqComplex(Trms[i].NumValue.NumericValue,V.NumericValue)  then
         begin UnionTrms(aTrmInfo,i);
          if Contr > 0 then exit; break
          { skad tu moze byc sprzecznosc ??? }
         end;
     clash:=true;
     Trms[aTrmInfo].NumValue.Determined:=true;
     Trms[aTrmInfo].NumValue.NumericValue:=V.NumericValue;
   end;
end;

procedure XXX(ff: char; const FTL: MList);
 var cl: boolean;
     lArgs1,lArgs2: TrmList;
     i,l1,l2: integer;
 label Different;
begin
 if FTL.Count=0 then exit;
 with FTL do
 for l1:=0 to Count-2 do
  begin
   for l2:=l1+1 to Count-1 do
    with FuncTrmPtr(Items^[l2])^ do
    begin
     if FuncTrmPtr(Items^[l1])^.TrmInfo<>TrmInfo then
      begin cl:=false;
       case ff of
{ co z adjustacja wariantu ikTrmFunctor ? }
        ikTrmFunctor:
         begin
          lArgs1:=AdjustTrmList(ikTrmFunctor,FuncTrmPtr(Items^[l1])^.FuncNr,FuncTrmPtr(Items^[l1])^.FuncArgs);
          lArgs2:=AdjustTrmList(ikTrmFunctor,FuncNr,FuncArgs);
          cl:=EqTrmLists(lArgs1,lArgs2);
         end;
        ikTrmSchFunc,ikTrmPrivFunc: cl:=EqTrmLists(FuncTrmPtr(FuncTrmPtr(Items^[l1]))^.FuncArgs,FuncArgs);
{ Trzeba usunac nieistotne }
        ikTrmAggreg:
         begin
          lArgs1:=FuncTrmPtr(Items^[l1])^.FuncArgs;
          lArgs2:=FuncArgs;
          for i:=1 to AggrConstrPtr( Constr[ coAggregate].At( FuncNr))^.fAggregBase do
           begin lArgs1:=lArgs1^.NextTrm;
            lArgs2:=lArgs2^.NextTrm;
           end;
          cl:=EqTrmLists(lArgs1,lArgs2);
         end;
        ikTrmSelector: cl:=EqTrms(LastArg(FuncTrmPtr(FuncTrmPtr(Items^[l1]))^.FuncArgs),LastArg(FuncArgs));
        ikTrmFraenkel:
         with FraenkelTrmPtr(Items^[l2])^ do
          if FraenkelTrmPtr(FuncTrmPtr(Items^[l1]))^.LambdaArgs.Count = LambdaArgs.Count then
           begin
            for i:=0 to LambdaArgs.Count-1 do
             if not EqTyps(LambdaArgs.Items^[i],
                            FraenkelTrmPtr(FuncTrmPtr(Items^[l1]))^.LambdaArgs.Items^[i]) then
              goto Different;
            cl:= EqTrms(LambdaScope,FraenkelTrmPtr(FuncTrmPtr(Items^[l1]))^.LambdaScope) and
                 EqFrms(Compr,FraenkelTrmPtr(FuncTrmPtr(Items^[l1]))^.Compr);
           end;
        ikTrmChoice:
          cl:=EqTyps(ChoiceTrmPtr(Items^[l1])^.ChoiceTyp,
                     ChoiceTrmPtr(Items^[l2])^.ChoiceTyp);
       end;
       if cl then
        begin UnionTrms(FuncTrmPtr(Items^[l1])^.TrmInfo,TrmInfo);
         if Contr > 0 then exit;
        end;
      end;
Different:
    end;
 end;
end;

procedure Identities(aArithmIncl: boolean);
 var lStructArg1,lStructArg2:TrmList;
     k,EqNr1,EqNr2:integer;
     lt1,lt2: TrmPtr;
     F1: TrmList;
     lFunctor: integer;
     lTrmInfo: integer;
     z,ii,ll: integer;
     lVal:ValRec;
     e: ComplexTrmExprKind;
 label Found,Next;
begin
 repeat
  for z:=0 to FuncTrmList[expTrmAggreg].Count-1 do
   begin
    with ConstrItem(FuncTrmList[expTrmAggreg].Items^[z])^.fTerms do
     for ii:=0 to Count-1 do
     with FuncTrmPtr(Items^[ii])^ do
      begin
       for ll:=ii+1 to Count-1 do
        begin
         if TrmS[FuncTrmPtr(Items^[ll])^.TrmInfo].Term^.TrmInfo
          = TrmS[TrmInfo].Term^.TrmInfo then goto Found;
        end;
       goto Next;
Found:
       { Przewijamy nieistotne argumenty }
       lStructArg1:=FuncArgs;
       lStructArg2:=FuncTrmPtr(Items^[ll])^.FuncArgs;
       for k:=1 to AggrConstrPtr( Constr[ coAggregate].At( FuncNr))^.fAggregBase do
        begin lStructArg1:=lStructArg1^.NextTrm;
         lStructArg2:=lStructArg2^.NextTrm;
        end;
       while lStructArg1 <> nil do
        begin
         mizassert(2999,lStructArg1^.XTrmPtr^.TrmInfo<=TrmNbr);
         EqNr1:=TrmS[lStructArg1^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
         EqNr2:=TrmS[lStructArg2^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
         if EqNr1 <> EqNr2 then UnionTrms(EqNr1,EqNr2);
         lStructArg1:=lStructArg1^.NextTrm; lStructArg2:=lStructArg2^.NextTrm;
        end;
Next:
      end;
   end;

  with FuncTrmList[expTrmFunctor] do
   for z:=0 to Count-1 do
    with ConstrItem(FuncTrmList[expTrmFunctor].Items^[z])^ do
     begin
     { Idempotence }
       with ConstrTypPtr( Constr[ coFunctor].At(IntKey))^ do
        if syIdempotence in fProperties then
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           GetArgs2(fFirstArg,fSecondArg,lt1,lt2,FuncArgs);
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           if TrmS[lt1^.TrmInfo].Term^.TrmInfo = TrmS[lt2^.TrmInfo].Term^.TrmInfo
            then UnionTrms(lTrmInfo,lt1^.TrmInfo);
          end;
        end;
     { Involutiveness }
       if aArithmIncl or (IntKey <> gBuiltIn[rqRealNeg])
           or (IntKey <> gBuiltIn[rqRealInv]) then
       with ConstrTypPtr( Constr[ coFunctor].At(IntKey))^ do
        if syInvolutiveness in fProperties then
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           GetArgs1(fFirstArg,lt1,FuncArgs);
           lTrmInfo:=TrmS[lt1^.TrmInfo].Term^.TrmInfo;
           F1:=TrmS[lTrmInfo].EqClass;
           while F1<>nil do
            begin
             if (FuncTrmPtr(F1^.XTrmPtr)^.TrmSort = ikTrmFunctor) then
             if FuncTrmPtr(F1^.XTrmPtr)^.FuncNr = IntKey then
             if EqButLast(FuncArgs,FuncTrmPtr(F1^.XTrmPtr)^.FuncArgs) then
              begin
               GetArgs1(fFirstArg,lt2,FuncTrmPtr(F1^.XTrmPtr)^.FuncArgs);
               UnionTrms(TrmS[TrmInfo].Term^.TrmInfo,lt2^.TrmInfo);
              end;
             F1:=F1^.NextTrm;
            end;
          end;
        end;
     { Projectivity }
       with ConstrTypPtr( Constr[ coFunctor].At(IntKey))^ do
        if syProjectivity in fProperties then
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           GetArgs1(fFirstArg,lt1,FuncArgs);
           lTrmInfo:=TrmS[lt1^.TrmInfo].Term^.TrmInfo;
           F1:=TrmS[lTrmInfo].EqClass;
           while F1<>nil do
            begin
             if (FuncTrmPtr(F1^.XTrmPtr)^.TrmSort = ikTrmFunctor) then
             if FuncTrmPtr(F1^.XTrmPtr)^.FuncNr = IntKey then
             if EqButLast(FuncArgs,FuncTrmPtr(F1^.XTrmPtr)^.FuncArgs) then
                   UnionTrms(TrmS[TrmInfo].Term^.TrmInfo,lt1^.TrmInfo);
             F1:=F1^.NextTrm;
            end;
          end;
        end;
     { --- BuiltIn --- }
      case gRevReq[IntKey] of
       rqNone:;
     { --- BOOLE --- }
       rqUnion:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr; lt2:=FuncArgs^.NextTrm^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           if IsItEmptySet(TrmS[lt1^.TrmInfo].EqClass) then
           UnionTrms(lTrmInfo,lt2^.TrmInfo);
          end;
        end;
       rqIntersection:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr; lt2:=FuncArgs^.NextTrm^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           if IsItEmptySet(TrmS[lt1^.TrmInfo].EqClass) then
           UnionTrms(lTrmInfo,lt1^.TrmInfo);
          end;
        end;
       rqSubtraction:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr; lt2:=FuncArgs^.NextTrm^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           if IsItEmptySet(TrmS[lt1^.TrmInfo].EqClass) then
           UnionTrms(lTrmInfo,lt1^.TrmInfo);
           if IsItEmptySet(TrmS[lt2^.TrmInfo].EqClass) then
           UnionTrms(lTrmInfo,lt1^.TrmInfo);
          end;
        end;
       rqSymmetricDifference:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr; lt2:=FuncArgs^.NextTrm^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           if IsItEmptySet(TrmS[lt2^.TrmInfo].EqClass) then
           UnionTrms(lTrmInfo,lt1^.TrmInfo);
          end;
        end;
     { --- NUMERALS --- }
       rqSucc:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           RatSucc(Trms[lt1^.TrmInfo].NumValue,lVal);
           if lVal.Determined then
           begin
{$IFDEF CH_REPORT}
            CHReport.Out_NumReq2(rqSucc,Trms[lt1^.TrmInfo].NumValue.NumericValue,lVal.NumericValue);
{$ENDIF}
            EquateComplexValue(lTrmInfo,lVal);
            if Contr > 0 then exit
           end;
          end;
        end;
      end;
     { --- COMPLEX --- }
      if aArithmIncl then
      case gRevReq[IntKey] of
       rqRealAdd:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr; lt2:=FuncArgs^.NextTrm^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           if Trms[lt1^.TrmInfo].NumValue.Determined then
             if IsEqWithInt(Trms[lt1^.TrmInfo].NumValue.NumericValue,0) then
               UnionTrms(lTrmInfo,lt2^.TrmInfo);
           RatAdd(Trms[lt1^.TrmInfo].NumValue,Trms[lt2^.TrmInfo].NumValue,lVal);
           if lVal.Determined then
           begin
{$IFDEF CH_REPORT}
//              CHReport.Out_NumReq2(rqRealAdd,
//                                   Trms[lt1^.TrmInfo].NumValue.NumericValue,
//                                   Trms[lt2^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
            EquateComplexValue(lTrmInfo,lVal);
            if Contr > 0 then exit
           end;
          end;
        end;
       rqRealMult:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin lt1:=FuncArgs^.XTrmPtr;
           lt2:=FuncArgs^.NextTrm^.XTrmPtr;
           if Trms[lt1^.TrmInfo].NumValue.Determined then
            begin
             if IsEqWithInt(Trms[lt1^.TrmInfo].NumValue.NumericValue,0) then
               UnionTrms(TrmInfo,lt1^.TrmInfo)
             else if IsEqWithInt(Trms[lt1^.TrmInfo].NumValue.NumericValue,1) then
               UnionTrms(TrmInfo,lt2^.TrmInfo);
             lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
             RatMult(Trms[lt1^.TrmInfo].NumValue,Trms[lt2^.TrmInfo].NumValue,lVal);
             if lVal.Determined then
             begin
{$IFDEF CH_REPORT}
//                CHReport.Out_NumReq2(rqRealMult,
//                                     Trms[lt1^.TrmInfo].NumValue.NumericValue,
//                                     Trms[lt2^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
              EquateComplexValue(lTrmInfo,lVal);
              if Contr > 0 then exit
             end;
            end;
          end;
        end;
       rqRealNeg:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           RatOpp(Trms[lt1^.TrmInfo].NumValue,lVal);
           if lVal.Determined then
           begin
{$IFDEF CH_REPORT}
//              CHReport.Out_NumReq1(rqRealNeg,
//                                   Trms[lt1^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
            EquateComplexValue(lTrmInfo,lVal);
            if Contr > 0 then exit
           end;
          end;
        end;
       rqRealInv:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr;
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           RatInv(Trms[lt1^.TrmInfo].NumValue,lVal);
           if lVal.Determined then
           begin
{$IFDEF CH_REPORT}
//              CHReport.Out_NumReq1(rqRealInv,
//                                   Trms[lt1^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
            EquateComplexValue(lTrmInfo,lVal);
            if Contr > 0 then exit
           end;
          end;
        end;
       rqRealDiff:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
          begin
           lt1:=FuncArgs^.XTrmPtr; lt2:=FuncArgs^.NextTrm^.XTrmPtr;
            { odejmowanie zera }
           lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
           if Trms[lt2^.TrmInfo].NumValue.Determined then
            if IsEqWithInt(Trms[lt2^.TrmInfo].NumValue.NumericValue,0) then
              UnionTrms(lTrmInfo,lt1^.TrmInfo);
            { birzemy przeciwny do drugie argumentu }
           RatOpp(Trms[lt2^.TrmInfo].NumValue,lVal);
            { odejmowanie od zera }
           if lVal.Determined then
              { dodajemy przeciwny do drugiego argumentu }
            begin RatAdd(Trms[lt1^.TrmInfo].NumValue,lVal,lVal);
             if lVal.Determined then
             begin
{$IFDEF CH_REPORT}
//                CHReport.Out_NumReq2(rqRealDiff,
//                                    Trms[lt1^.TrmInfo].NumValue.NumericValue,
//                                     Trms[lt2^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
              EquateComplexValue(lTrmInfo,lVal);
              if Contr > 0 then exit
             end;
            end;
          end;
        end;
       rqRealDiv:
        begin
         for ii:=0 to fTerms.Count-1 do
          with FuncTrmPtr(fTerms.Items^[ii])^ do
           begin lt1:=FuncArgs^.XTrmPtr;
            lt2:=FuncArgs^.NextTrm^.XTrmPtr;
            if Trms[lt1^.TrmInfo].NumValue.Determined then
              { dzielenie zera }
              if IsEqWithInt(Trms[lt1^.TrmInfo].NumValue.NumericValue,0) then
               UnionTrms(TrmInfo,lt1^.TrmInfo);
            lTrmInfo:=TrmS[TrmInfo].Term^.TrmInfo;
            RatInv(Trms[lt2^.TrmInfo].NumValue,lVal);
            if lVal.Determined then
             begin
               { dzielenie przez 1 }
              if IsEqWithInt(lVal.NumericValue,1) then
               UnionTrms(TrmInfo,lt1^.TrmInfo);
              RatMult(Trms[lt1^.TrmInfo].NumValue,lVal,lVal);
              if lVal.Determined then
              begin
{$IFDEF CH_REPORT}
//                 CHReport.Out_NumReq2(rqRealDiv,
//                                      Trms[lt1^.TrmInfo].NumValue.NumericValue,
//                                      Trms[lt2^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
               EquateComplexValue(lTrmInfo,lVal);
               if Contr > 0 then exit
              end;
             end;
           end;
        end;
      end;
     end;

  if not clash then exit;

  repeat
    clash:=false;
    for e := Low(FuncTrmExprKind) to High(FuncTrmExprKind) do
     with FuncTrmList[e] do
      for z:=0 to Count-1 do
       begin
        XXX(TrmKindArr[e],ConstrItem(Items^[z])^.fTerms);
        if Contr > 0 then exit;
       end;
    XXX(ikTrmFraenkel,FrOper);
    if Contr > 0 then exit;
    XXX(ikTrmChoice,ChoiceTerm);
    if Contr > 0 then exit;
  until not clash;
  ProcessReductions;
 until false;
end;

procedure UnionEqualsForNonComplex(var aEqPendings:IntRel);
 var ii,lEqPendingsNbr: integer;
begin
  ii:=0;
  lEqPendingsNbr:=aEqPendings.Count;
  while ii < aEqPendings.Count do
  with aEqPendings.Items^[ii] do
  if IsComplexNumber(TrmS[TrmS[X].Term^.TrmInfo].SuperCluster) or
     IsComplexNumber(TrmS[TrmS[Y].Term^.TrmInfo].SuperCluster) then
   begin
    InsertComplex(TrmS[X].Term^.TrmInfo);
    InsertComplex(TrmS[Y].Term^.TrmInfo);
    inc(ii)
   end
  else
   begin UnionTrms(X,Y);
    aEqPendings.AtDelete(ii);
   end;
  if Contr > 0 then exit;
// Czy jest to poprawne wolanie?
  if lEqPendingsNbr <> aEqPendings.Count then
   Identities(false);
end;

procedure SubstVar(lVar,sVar:integer);
 var t,i: integer;
     lPolynomialValues: MList;
     lPolynomial: PolynomialPtr;
begin
  lPolynomial:=new(PolynomialPtr,InitWithMonomial(COne,sVar));
  for t:=1 to TrmNbr do
   with TrmS[t] do
   if (t <> lVar) and (EqClass <> nil) then
   begin
    lPolynomialValues.Init(0);
    i:=0;
    while i < PolynomialValues.Count do
    begin
      if PolynomialPtr(PolynomialValues.Items^[i])^.HasTheVariable(lVar) then
      begin
       PolynomialPtr(PolynomialValues.Items^[i])^.InsertValue(lVar,lPolynomial);
       if PolynomialPtr(PolynomialValues.Items^[i])^.IsNumeric then
        begin
          if not NumValue.Determined then
           begin
            NumValue.Determined:=true;
            PolynomialPtr(PolynomialValues.Items^[i])^.GetNumeric(NumValue.NumericValue);
           end
          else if not PolynomialPtr(PolynomialValues.Items^[i])^.IsNumericEqualWith(NumValue.NumericValue) then
           SetContr(15);
        end;
       lPolynomialValues.Insert(PolynomialValues.Items^[i]);
       PolynomialValues.AtDelete(i);
      end else inc(i);
     end;
    for i:=0 to lPolynomialValues.Count-1 do
     PolynomialValues.Insert(lPolynomialValues.Items^[i]);
    lPolynomialValues.DeleteAll;
    lPolynomialValues.Done;
   end;
  Dispose(lPolynomial,Done);
end;

procedure ClearPolynomialValues;
 var t: integer;
begin
 for t:=1 to TrmNbr do
  with TrmS[t] do
   if (Term^.TrmInfo <> t) and
      (TrmS[Term^.TrmInfo].PolynomialValues.Count > 0) then
    SubstVar(t,Term^.TrmInfo);
end;

procedure InitPolynomialValues;
 var t,k,lArg1,lArg2: integer;
     lTrm: TrmPtr;
     lVal: ValRec;
     lPolynomial1,lPolynomial2: PolynomialPtr;
     lPendingVarSubst: NatSet;
     lEqClass: TrmList;
begin
 if gBuiltIn[rqComplex] = 0 then exit;
 lPendingVarSubst.Init(0,8);
 for t:=1 to TrmNbr do
  with TrmS[t] do
   if EqClass <> nil then
   begin
     lEqClass:=EqClass;
     while lEqClass <> nil do
     begin
      lTrm:=lEqClass^.XTrmPtr;
      lEqClass:=lEqClass^.NextTrm;
      case lTrm^.TrmSort of
      ikTrmSchFunc,ikTrmPrivFunc,ikTrmAggreg,ikTrmSelector,ikTrmFraenkel,ikTrmEqConst:
       if IsComplexNumber(SuperCluster) then
        PolynomialValues.Insert(new(PolynomialPtr,InitWithMonomial(COne,t)));
      ikTrmInfConst:
       if NumValue.Determined then
        begin
          PolynomialValues.Insert(new(PolynomialPtr,InitWithNumeric(NumValue.NumericValue)));
          lPendingVarSubst.InsertElem(t);
        end
       else if IsComplexNumber(SuperCluster) then
        if PolynomialValues.Count = 0 then
         PolynomialValues.Insert(new(PolynomialPtr,InitWithMonomial(COne,t)));
      ikTrmFunctor:
       with FuncTrmPtr(lTrm)^ do
       begin
        case gRevReq[FuncNr] of
        rqImaginaryUnit:
          begin
           MizAssert(3292,NumValue.Determined);
           PolynomialValues.Insert(new(PolynomialPtr,InitWithNumeric(CImUnit)));
           lPendingVarSubst.InsertElem(t);
          end;
        rqRealAdd, rqRealMult, rqRealDiff:
          begin
           lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           lArg2:=Trms[FuncArgs^.NextTrm^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           lPolynomial1:=new(PolynomialPtr,InitWithMonomial(COne,lArg1));
           lPolynomial2:=new(PolynomialPtr,InitWithMonomial(COne,lArg2));
           case gRevReq[FuncNr] of
            rqRealAdd:
             PolynomialValues.Insert(AddPolynomials(lPolynomial1,lPolynomial2));
            rqRealMult:
             PolynomialValues.Insert(MultPolynomials(lPolynomial1,lPolynomial2));
            rqRealDiff:
             PolynomialValues.Insert(AddPolynomials(lPolynomial1,NMultPolynomial(CMinusOne,lPolynomial2)));
           end;
           lPendingVarSubst.InsertElem(t);
           dispose(lPolynomial2,Done);
           dispose(lPolynomial1,Done);
          end;
        rqRealNeg:
          begin
           lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           lPolynomial1:=new(PolynomialPtr,InitWithMonomial(COne,lArg1));
           PolynomialValues.Insert(NMultPolynomial(CMinusOne,lPolynomial1));
           lPendingVarSubst.InsertElem(t);
           dispose(lPolynomial1,Done);
          end;
        rqRealInv:
          begin
           PolynomialValues.Insert(new(PolynomialPtr,InitWithMonomial(COne,t)));
          end;
        rqRealDiv:
          begin
           lArg1:=Trms[FuncArgs^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           lArg2:=Trms[FuncArgs^.NextTrm^.XTrmPtr^.TrmInfo].Term^.TrmInfo;
           RatInv(Trms[lArg2].NumValue,lVal);
           if lVal.Determined then
            begin
             lPolynomial1:=new(PolynomialPtr,InitWithMonomial(COne,lArg1));
             PolynomialValues.Insert(NMultPolynomial(lVal.NumericValue,lPolynomial1));
             lPendingVarSubst.InsertElem(t);
             dispose(lPolynomial1,Done);
            end
           else
            begin
            PolynomialValues.Insert(new(PolynomialPtr,InitWithMonomial(COne,t)));
          end;
          end;
        else if NumValue.Determined then
          begin
            PolynomialValues.Insert(new(PolynomialPtr,InitWithNumeric(NumValue.NumericValue)));
            lPendingVarSubst.InsertElem(t);
          end
        else if IsComplexNumber(SuperCluster) then
          PolynomialValues.Insert(new(PolynomialPtr,InitWithMonomial(COne,t)));
        end;
       end;
      ikTrmChoice:;
      else RunTimeError(2625);
      end;
     end;
    if PolynomialValues.Count > 1 then
     begin
      for k:=0 to PolynomialValues.Count-1 do
       if PolynomialPtr(PolynomialValues.Items^[k])^.IsVariable = t then
        begin PolynomialValues.AtFree(k);
         break
        end;
     end;
   end;
{$IFDEF MDEBUG}
//writeln(InfoFile,'---------------------- w trakcie InitPolynomialValues');
//infoeqclasses;
//writeln(InfoFile,'---------------------- koniec wydruku w trakcie InitPolynomialValues');
{$ENDIF};
  SubstitutePendingVars(lPendingVarSubst);
end;

(*procedure InitPolynomialValues;
 var t: integer;
begin
 if gBuiltIn[rqComplex] = 0 then exit;
 for t:=1 to TrmNbr do
  YValue(t);
{$IFDEF MDEBUG}
//writeln(InfoFile,'---------------------- w trakcie InitPolynomialValues');
//infoeqclasses;
//writeln(InfoFile,'---------------------- koniec wydruku w trakcie InitPolynomialValues');
{$ENDIF};
end;*)

procedure SubsituteSettings(var aEquals:IntRel);
 var lPendingVarSubst: NatSet;
begin
 lPendingVarSubst.Init(0,8);
 while aEquals.Count > 0 do
  begin
   with aEquals.Items^[0] do
   begin
    if (TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Count > 0) and
       (TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Count > 0) then
      SubstituteVariable(X,TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Items^[0],lPendingVarSubst);
    UnionTrms(X,Y);
   end;
   aEquals.AtDelete(0);
  end;
  SubstitutePendingVars(lPendingVarSubst);
  aEquals.Done;
end;

procedure EquatePolynomialValues(var aEquals:IntRel);
 var i,j,j1,j2,lVar1,lVar2,t: integer;
     lPendingVarSubst: NatSet;
     lPolynomial: PolynomialPtr;
 label 1;
begin
 if gBuiltIn[rqComplex] = 0 then exit;
 lPendingVarSubst.Init(0,8);
 repeat
  for t:=1 to TrmNbr do
   with TrmS[t] do
   if (EqClass <> nil) and (PolynomialValues.Count>0) then
   begin
    i:=PolynomialPtr(PolynomialValues.Items^[0])^.IsVariable;
    if i > 0 then
     begin
      if i <> t then
       begin
        j:=PolynomialPtr(TrmS[Trms[i].Term^.TrmInfo].PolynomialValues.Items^[0])^.IsVariable;
        if j = i then
         begin
          if t > i then
           SubstituteVariable(t,TrmS[Trms[i].Term^.TrmInfo].PolynomialValues.Items^[0],lPendingVarSubst)
          else
           begin
            lPolynomial:=new(PolynomialPtr,InitWithMonomial(COne,t));
            SubstituteVariable(i,lPolynomial,lPendingVarSubst);
            Dispose(lPolynomial,Done);
           end;
          UnionTrms(t,i);
         end;
       end
     end
   end;
  SubstitutePendingVars(lPendingVarSubst);
  i:=0;
  while i < aEquals.Count do
  begin
   with aEquals.Items^[i] do
   if (TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Count > 0) and
      (TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Count > 0) then
   begin
    for j1:=0 to TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Count-1 do
     for j2:=0 to TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Count-1 do
      if ComparePolynomials(TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Items^[j1],
                            TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Items^[j2]) = 0 then
      begin
       UnionTrms(X,Y);
       aEquals.AtDelete(i);
       goto 1;
      end;
    lVar1:=PolynomialPtr(TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Items^[0])^.IsUniVariantVariable;
    lVar2:=PolynomialPtr(TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Items^[0])^.IsUniVariantVariable;
    if (lVar1 = TrmS[X].Term^.TrmInfo) then
     begin
      if lVar2 = TrmS[Y].Term^.TrmInfo then
       begin
        UnionTrms(X,Y);
        aEquals.AtDelete(i);
        continue;
       end
      else if (TrmS[TrmS[Y].Term^.TrmInfo].NumValue.Determined
          and (TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Count = 1)) then
       begin
        SubstituteVariable(X,TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Items^[0],lPendingVarSubst);
        UnionTrms(X,Y);
        aEquals.AtDelete(i);
        continue;
       end
     end
    else if  (TrmS[TrmS[X].Term^.TrmInfo].NumValue.Determined
          and (TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Count = 1))then
      if lVar2 = Y then
       begin
        SubstituteVariable(Y,TrmS[TrmS[X].Term^.TrmInfo].PolynomialValues.Items^[0],lPendingVarSubst);
        UnionTrms(X,Y);
        aEquals.AtDelete(i);
        continue;
       end
      else if PolynomialPtr(TrmS[TrmS[Y].Term^.TrmInfo].PolynomialValues.Items^[0])^.IsNumeric then
       SetContr(16);
   end;
   inc(i);
1:
  end;
 until lPendingVarSubst.Count  = 0;
 lPendingVarSubst.Done;
end;

  procedure EquatePolynomials;
   var i1,i2,j1,j2: integer;
   label 1;
  begin
{$IFDEF MINI_PROFILER}
MiniProfiler.SectionBegin ('3.1. EquatePolynomials');
{$ENDIF}
   for i1:=1 to TrmNbr do
    if (Trms[i1].EqClass <> nil) and (Trms[i1].PolynomialValues.Count > 0) then
     for i2:=i1+1 to TrmNbr do
     begin
       if (Trms[i2].EqClass <> nil) and (Trms[i2].PolynomialValues.Count > 0) then
        for j1:=0 to Trms[i1].PolynomialValues.Count-1 do
         for j2:=0 to Trms[i2].PolynomialValues.Count-1 do
          if ComparePolynomials(Trms[i1].PolynomialValues.Items^[j1],
                                Trms[i2].PolynomialValues.Items^[j2]) = 0 then
          begin
           UnionTrms(i1,i2);
           if Contr > 0 then
            begin
{$IFDEF MINI_PROFILER}
MiniProfiler.SectionEnd;
{$ENDIF}
             exit;
            end;
           goto 1;
          end;
1:
     end;
{$IFDEF MINI_PROFILER}
MiniProfiler.SectionEnd;
{$ENDIF}
  end;

procedure RenumEqClasses;
 var ii,aii,xx: integer;
     lClusterPtr: AttrCollectionPtr;
begin
 EqClassNbr:=0;
 for ii:=1 to TrmNbr do
   with Trms[ii] do
    if  EqClass<>nil then
     begin inc(EqClassNbr);
      VarTrmPtr(Trms[ii].Term)^.VarNr:=EqClassNbr
     end;
 for ii:=1 to TrmNbr do
  with Trms[ii].Term^ do
   begin TrmSort:=ikTrmEqConst;
     aii:=VarTrmPtr(Trms[TrmInfo].Term)^.VarNr;{aii wprowadzona dla GPC !}
     VarTrmPtr(Trms[ii].Term)^.VarNr:=aii
  end;
 for ii:=1 to TrmNbr do
  with TrmS[ii] do
   if  EqClass<>nil then
   begin
    with SuperCluster^ do
     begin
      lClusterPtr:=new(AttrCollectionPtr,Init(Count,4));
      for xx:=0 to Count-1 do
       lClusterPtr^.Insert(AttrPtr(Items^[xx])^.CopyAttribute);
     end;
    if not lClusterPtr^.fConsistent then SetContr(17);
    dispose(Supercluster,Done);
    Supercluster:=lClusterPtr;
    if Contr > 0 then exit;
   end;
end;

procedure ContradictionVerify;
 var ii,j: integer;
     lAttr: AttrPtr;
     hQualTyp: TypPtr;
     lLeftArg,lRightArg:TrmPtr;
begin
 for ii:=0 to NegBas.Count-1 do
 begin
  case FrmPtr(NegBas.Items^[ii])^.FrmSort of
   ikFrmAttr,ikFrmSchPred,ikFrmPrivPred,ikFrmPred:
    with PredFrmPtr(NegBas.Items^[ii])^ do
    begin
     case FrmSort of
     ikFrmAttr:
      begin
       lAttr:=Trms[LastArg(PredArgs)^.TrmInfo].SuperCluster^.GetAttr(PredNr,CopyTermList1(PredArgs));
       if (lAttr<>nil) and (lAttr^.fNeg=ord(true)) then
        begin SetContr(18);
         exit
        end;
      end;
     ikFrmPred:
      with ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
      if syReflexivity in fProperties then
       begin
        GetArgs2(fFirstArg,fSecondArg,lLeftArg,lRightArg,PredArgs);
        if lLeftArg^.TrmInfo = lRightArg^.TrmInfo then
         begin SetContr(19);
          exit
         end;
       end;
     end;
     for j:=0 to PosBas.Count-1 do
      if EqFrms(PosBas.Items^[j],NegBas.Items^[ii]) then
       begin SetContr(20);
        exit
       end;
    end;
   ikFrmQual:
    begin hQualTyp:=QualFrmPtr(NegBas.Items^[ii])^.QualTyp;
     with hQualTyp^,Trms[QualFrmPtr(NegBas.Items^[ii])^.QualTrm^.TrmInfo].XTypClass do
      for j:=0 to Count-1 do
       if EqRadices(TypPtr(Items^[j])) then
        begin SetContr(21);
         exit
        end;
    end;
  end;
 end;
end;

procedure InsertNonEmpty(fTrmInfo1,fTrmInfo2:integer);
 var lAttr: AttrPtr;
begin
 lAttr:=TrmS[fTrmInfo1].SuperCluster^.GetAttr(gBuiltIn[rqEmpty],nil);
 if (lAttr<>nil) and (lAttr^.fNeg=ord(true)) then
  begin
   TrmS[fTrmInfo2].SuperCluster^.InsertAttr(gBuiltIn[rqEmpty],0,nil);
   if not TrmS[fTrmInfo2].SuperCluster^.fConsistent then SetContr(22);
  end;
end;

procedure InsertNonZero(fTrmInfo1,fTrmInfo2:integer);
 var lAttr: AttrPtr;
begin
 lAttr:=TrmS[fTrmInfo1].SuperCluster^.GetAttr(gBuiltIn[rqZero],nil);
 if (lAttr<>nil) and (lAttr^.fNeg=ord(true)) then
  begin
   TrmS[fTrmInfo2].SuperCluster^.InsertAttr(gBuiltIn[rqZero],0,nil);
   if not TrmS[fTrmInfo2].SuperCluster^.fConsistent then SetContr(23);
  end;
end;

procedure Equate(var fEval:NatFunc);
  var
      j,ii,jj,xi: integer;
      lFrm: FrmPtr;
      lPred,lPred2:integer;
      lArgs,lArgs2: TrmList;
      lLeftArg,lRightArg:TrmPtr;
      lEqClass: TrmList;
      lTyp:TypPtr;
      LTrm1,LTrm2: TrmPtr;
      agTyp,gTyp:TypPtr;
      lSetting,lEqPendings: IntRel;
//      lConstSet: NatSet;
      lPolynomials: MCollection;
      lInsTyp: TypPtr; // Rozszerzenia typow uzywane przy negatywnych atrybutywnych.
      llFrm:QualFrmPtr; // pomocnicza przy standaryzacji atrybutywnych, do wyrzucenia !!!
      xx,lTrmInfo1,lTrmInfo2: integer;
      hQualTyp,xType,lQualTyp: TypPtr;
      lEmptyTrm,xTerm,xTerm1,lTrm,lTrm3,lTrm4: TrmPtr;
      k,PredNr2:integer;
      lTrmList,lTrmList1,A1,A2:TrmList;
      lTypClass: MCollection;
      CollectedE: NatSet;
      lTl,llTL1,LTL1,LTL2,sTrmList,selTrmList:TrmList;
      llInsTyp: TypPtr;
      lAttr,lPositive,lNegative,lZero,rPositive,rNegative,rZero: AttrPtr;
      lTrmInfo,lPredNr: integer;
      lEqList: integer;
      lFrm1: FrmPtr;
      z,zz,zz1,zz2,b11,b11ModNr,lModNr1,lModNr2:integer;
      b11Args: TrmList;
      AddedAttr,lVerifyAttr: boolean;
      lAttrTyp:TypPtr;
      lAttrArgs: TrmList;
      lNumValue: ValRec;
      lLatOvfl: boolean;
      e: ComplexTrmExprKind;
      ANi,ANj,ANk,ANl,ANm,ANn: Integer;
      ns1,ns2: NatSetPtr;
   label 111,63,EndEquate;
begin { Equate }
{$IFDEF MDEBUG}
//writeln(infofile,'InferConstDef.Count=',InferConstDef.Count);
//for II:=0 to InferConstDef.Count-1 do InfoInferConstDef(II);
{$ENDIF}
   EqClassNbr:=0;

   PosBas.Init(8,16); NegBas.Init(8,16);
   TrmNbr:=0;
   TrmOvfl:=false;
   DTrm.Init(MaxInferConstNbr);
   for e := Low(FuncTrmExprKind) to High(FuncTrmExprKind) do
    FuncTrmList[e].Init(0,10);
   FrOper.InitSorted(0,CompRdTrms);
   ChoiceTerm.InitSorted(0,CompRdTrms);
   SetContr(0); clash:=false;

   lEqPendings.Init(8);
   lSetting.Init(8);
   AllowedCCluster.Init(ConditionalCluster.Count);
   AllowedFCluster.Init(FunctorCluster.Count);
   gDependencies.Init(TrmNbr);
   ANEqStack.Init(TrmNbr,10);

(*   lConstSet.Init(0,8);
   lConstSet.Init(gTrms.Count,8);
   for j:=0 to gTrms.Count-1 do
    with TrmsPtr(gTrms.Items^[j])^ do
    begin
     xTerm:=NewVarTrm(ikTrmInfConst,nConstrNr);
     YTerm(xTerm);
     with ConstDefPtr(InferConstDef.Items^[nConstrNr])^ do
     begin
//      lConstSet.InsertElem(nConstrNr);
      ii:=xTerm^.TrmInfo;
      lConstSet.InsertElem(ii);
      for z:=0 to fEqConst.Count-1 do
       begin
        lTrm:=NewVarTrm(ikTrmInfConst,fEqConst.Items^[z].X);
        YTerm(lTrm);
        if ii <> lTrm^.TrmInfo then
         AddEquality(lEqPendings,ii,lTrm^.TrmInfo);
       end;
      lTrm:=CopyExpTrm(fDef);
      YYTerm(lTrm,ii);
      if ii <> xTerm^.TrmInfo then
        AddEquality(lSetting,ii,xTerm^.TrmInfo);
      if TrmOvfl then begin ChError(14); goto EndEquate1 end;
      if fDef^.TrmSort = ikTrmFunctor then
       with TrmS[ii] do
        begin
         mizassert(3271,EqClass^.XTrmPtr^.TrmSort = ikTrmFunctor);
         for z:=0 to 1 do
          for k:=0 to nIdentify[z].Count-1 do
           begin
            if not gFuncIdentify.Search(@nIdentify[z].Items^[k].X,xi) then
             gFuncIdentify.AtInsert(xi,new(IdTrmItem, Init(nIdentify[z].Items^[k].X)));
            with IdTrmItem(gFuncIdentify.Items^[xi])^ do
             begin
              IdTrms[z].Insert(EqClass^.XTrmPtr);
              with ConstrTypPtr(Constr[coFunctor].Items^[FuncTrmPtr(fDef)^.FuncNr])^ do
               if Commutativity in fProperties then
                IdTrms[z].Insert(EqClass^.NextTrm^.XTrmPtr);
             end;
           end;
        end;
     end;
    end;
*)

   for j:=0 to Basic.Count-1 do
    if fEval.HasInDom(j) and (fEval.Value(j) = 1) then
    case FrmPtr(Basic.Items^[j])^.FrmSort of
     ikFrmQual:
      begin
       xType:=QualFrmPtr(Basic.Items^[j])^.QualTyp^.CopyType;
       xTerm:=CopyTerm(QualFrmPtr(Basic.Items^[j])^.QualTrm);
       YType(xType);
       YTerm(xTerm);
       InsertType(xType,xTerm^.TrmInfo);
      end;
     ikFrmAttr:
      with PredFrmPtr(Basic.Items^[j])^ do
       begin
        lTrmList:=CopyTermList1(PredArgs);
        lTrm:=CopyTerm(LastArg(PredArgs));
        YTermList(lTrmList);
        YTerm(lTrm);
        Trms[Trms[lTrm^.TrmInfo].Term^.TrmInfo].Supercluster^.InsertAttr(PredNr,1,lTrmList);
        if not Trms[Trms[lTrm^.TrmInfo].Term^.TrmInfo].Supercluster^.fConsistent
         then
          begin
           SetContr(24);
           exit;
          end;
       end;
     ikFrmPred:
      begin
       AdjustFrm(PredFrmPtr(Basic.Items^[j]),lPred,lArgs);
       if lPred=gBuiltIn[rqEqualsTo] then
        begin
         xTerm:=CopyTerm(lArgs^.XTrmPtr);
         xTerm1:=CopyTerm(lArgs^.NextTrm^.XTrmPtr);
         YTerm(xTerm);
         YTerm(xTerm1);
         if xTerm^.TrmInfo <> xTerm1^.TrmInfo then
          AddEquality(lEqPendings,xTerm^.TrmInfo,xTerm1^.TrmInfo);
        end
       else goto 63;
      end;
     else
63:
      begin
       lFrm:=FrmPtr(Basic.Items^[j])^.CopyFormula;
       YFormula(lFrm);
{ Trzeba by przeeksperymentowac, czy nie mozna alokowac juz zadjustowana,
  co najwyzej zalokowac przed adjustacja, ale wpisywac zadjustowana do
  PosBas.Sentence
}
       PosBas.Insert(lFrm);
       {$IFDEF MDEBUG}
//       InfoString('PosBas: '); InfoFormula(lFrm); InfoNewLine;
       {$ENDIF}
      end;
    end;
   if Contr > 0 then
   begin
    exit;
   end;

   for j:=0 to Basic.Count-1 do
    if fEval.HasInDom(j) and (fEval.Value(j) = 0) then
     if FrmPtr(Basic.Items^[j])^.FrmSort  = ikFrmAttr then
      with PredFrmPtr(Basic.Items^[j])^ do
       begin
        lTrmList:=CopyTermList1(PredArgs);
        lTrm:=CopyTerm(LastArg(PredArgs));
        YTermList(lTrmList);
        YTerm(lTrm);
        Trms[Trms[lTrm^.TrmInfo].Term^.TrmInfo].Supercluster^.InsertAttr(PredNr,0,lTrmList);
        if not Trms[Trms[lTrm^.TrmInfo].Term^.TrmInfo].Supercluster^.fConsistent then
          begin
           SetContr(25);
           exit;
          end;
       end
     else
       begin
        lFrm:=FrmPtr(Basic.Items^[j])^.CopyFormula;
        YFormula(lFrm);
        NegBas.Insert(lFrm);
        {$IFDEF MDEBUG}
//        InfoString('NegBas: '); InfoFormula(lFrm); InfoNewLine;
        {$ENDIF}
       end;
   if Contr > 0 then
   begin
    exit;
   end;

// Looking for occurrences of external (not allowed) constants in registrations

   InitAllowedClusters;

   for II:=0 to PosBas.Count-1 do
    with FrmPtr(PosBas.Items^[II])^ do
     case FrmSort of
      ikFrmPred:
       with PredFrmPtr(PosBas.Items^[II])^,
              ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
       begin
        if syAsymmetry in fProperties then
         begin
          z:=BasicFrmNr(PosBas.Items^[II],NegBas);
          if z < 0 then
           NegBas.Insert(NewPredFrm(FrmSort,PredNr,SwitchArgs(fFirstArg,fSecondArg,PredArgs),0));
         end;
       end;
     end;
   if Contr > 0 then
   begin
    exit;
   end;

   for II:=0 to NegBas.Count-1 do
    with FrmPtr(NegBas.Items^[II])^ do
     case FrmSort of
      ikFrmPred:
       with PredFrmPtr(NegBas.Items^[II])^,
              ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
        if syConnectedness in fProperties then
         begin
           z:=BasicFrmNr(NegBas.Items^[II],PosBas);
           if z < 0 then
            PosBas.Insert(NewPredFrm(FrmSort,PredNr,SwitchArgs(fFirstArg,fSecondArg,PredArgs),0));
         end;
     end;
  if Contr > 0 then
   begin
    exit;
   end;

  ii:=1;
  while ii <= TrmNbr do
   with TrmS[ii] do
    begin
     lTL:=EqClass;
     while lTL <> nil do
      with lTL^.XTrmPtr^ do
       begin
        if (TrmSort = ikTrmInfConst)
//            and not lConstSet.HasInDom(VarTrmPtr(lTL^.XTrmPtr)^.VarNr)
            then
         with ConstDefPtr(InferConstDef.Items^[VarTrmPtr(lTL^.XTrmPtr)^.VarNr])^ do
          begin
           for z:=0 to fEqConst.Count-1 do
            begin
             lTrm:=NewVarTrm(ikTrmInfConst,fEqConst.Items^[z].X);
             YTerm(lTrm);
             if ii <> lTrm^.TrmInfo then
             begin
//              if nSetting = fEqConst.Items^[z].X then
//               AddEquality(lSetting,ii,lTerm^.TrmInfo);
//              else
              AddEquality(lEqPendings,ii,lTrm^.TrmInfo);
             end;
            end;
           lTrm:=CopyExpTrm(fDef);
           xi:=ii;
           YYTerm(lTrm,xi);
           if ii <> xi then
            AddEquality(lSetting,ii,xi);
          end;
        lTL:=lTL^.NextTrm;
       end;
     inc(ii);
    end;
  if Contr > 0 then
   begin
    exit;
   end;
  InitEmptyInEqClass(lEqPendings);
  if Contr > 0 then
   begin
    exit;
   end;
  InitStructuresInEqClass;
  if TrmOvfl then
   begin
    exit;
   end;
  if Contr > 0 then
   begin
    exit;
   end;

  ProcessReductions;

  InitSuperClusterForComplex;
  if Contr > 0 then exit;
  UnionEqualsForNonComplex(lEqPendings);
  if Contr > 0 then exit;
// Initialisation of the Polynomial Values for numbers
  InitPolynomialValues;
  if Contr > 0 then exit;
  SubsituteSettings(lSetting);
  if Contr > 0 then exit;
  ClearPolynomialValues;
  if Contr > 0 then exit;
  EquatePolynomialValues(lEqPendings);
  if Contr > 0 then exit;
  EquatePolynomials;
  if Contr > 0 then exit;
  ClearPolynomialValues;
  if Contr > 0 then exit;

  ProcessLinearEquations(lEqPendings,lPolynomials);
  if Contr > 0 then exit;
  while lEqPendings.Count > 0 do
  begin
   with lEqPendings.Items^[0] do
    UnionTrms(X,Y);
   lEqPendings.AtDelete(0);
  end;
  lEqPendings.Done;
  if Contr > 0 then exit;
  EquatePolynomials;
  if Contr > 0 then exit;
  repeat
   ClearPolynomialValues;
   if Contr > 0 then exit;
   Identities(true);
   if Contr > 0 then exit;
   EquatePolynomials;
   if Contr > 0 then exit;
  until not clash;
  RenumEqClasses;
  if Contr > 0 then exit;
  ContradictionVerify;
  if Contr > 0 then exit;

//  ProcessPolynomialValues(lEqPendings,lPolynomials);
  lPolynomials.Done;
  if Contr > 0 then exit;
  if lEqPendings.Count > 0 then
  begin
    repeat
     with lEqPendings.Items^[0] do
      UnionTrms(X,Y);
     lEqPendings.AtDelete(0);
    until lEqPendings.Count = 0;
    lEqPendings.Done;
    if Contr > 0 then exit;
    EquatePolynomials;
    if Contr > 0 then exit;
    repeat
     ClearPolynomialValues;
     if Contr > 0 then exit;
     Identities(true);
     if Contr > 0 then exit;
     EquatePolynomials;
     if Contr > 0 then exit;
    until not clash;
    { po cholera ta zmiana numeracji ? }
    RenumEqClasses;
    if Contr > 0 then exit;
    ContradictionVerify;
    if Contr > 0 then exit;
  end;

   for ii:=0 to NegBas.Count-1 do
    with FrmPtr(NegBas.Items^[ii])^ do
     if FrmSort = ikFrmPred then
      with PredFrmPtr(NegBas.Items^[II])^,
           ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
       if syReflexivity in fProperties then
       begin
         GetArgs2(fFirstArg,fSecondArg,lLeftArg,lRightArg,PredArgs);
         lTrmInfo1:=lLeftArg^.TrmInfo;
         lTrmInfo2:=lRightArg^.TrmInfo;
         InsertNonEmpty(lTrmInfo1,lTrmInfo2);
         InsertNonEmpty(lTrmInfo2,lTrmInfo1);
         InsertNonZero(lTrmInfo1,lTrmInfo2);
         InsertNonZero(lTrmInfo2,lTrmInfo1);
         if Contr > 0 then exit;
       end;

   AddedAttr:=true;
   while AddedAttr do
   begin
   AddedAttr:=false;
   for ii:=0 to PosBas.Count-1 do
    with FrmPtr(PosBas.Items^[ii])^ do
    begin
     if FrmSort = ikFrmPred then
      begin
       AdjustFrm(PredFrmPtr(PosBas.Items^[ii]),lPred,lArgs);
       if lPred = gBuiltIn[rqLessOrEqual] then
         begin lLeftArg:=lArgs^.XTrmPtr;
          lRightArg:=lArgs^.NextTrm^.XTrmPtr;
          if (gBuiltIn[rqPositive] > 0) and (gBuiltIn[rqNegative] > 0) then
           begin
             lPositive:=Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil);
             lNegative:=Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil);
             rPositive:=Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil);
             rNegative:=Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil);
             if (lPositive<>nil) and (lPositive^.fNeg=ord(true)) then
               begin
                if Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil)=nil
                 then AddedAttr:=true;
                Trms[lRightArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqPositive],ord(true),nil);
                if not Trms[lRightArg^.TrmInfo].Supercluster^.fConsistent then
                  begin
                   SetContr(26);
                   exit;
                  end;
               end;
             if (rNegative<>nil) and (rNegative^.fNeg=ord(true)) then
               begin
                if Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil)=nil then AddedAttr:=true;
                Trms[lLeftArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqNegative],ord(true),nil);
                if not Trms[lLeftArg^.TrmInfo].Supercluster^.fConsistent then
                  begin
                    SetContr(27);
                    exit;
                  end;
               end;
             if (lNegative<>nil) and (lNegative^.fNeg=ord(false)) then
               begin
                if Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil)=nil then AddedAttr:=true;
                Trms[lRightArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqNegative],ord(false),nil);
                if not Trms[lRightArg^.TrmInfo].Supercluster^.fConsistent then
                  begin
                   SetContr(28);
                   exit;
                  end;
               end;
             if (rPositive<>nil) and (rPositive^.fNeg=ord(false)) then
               begin
                if Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil)=nil
                 then AddedAttr:=true;
                Trms[lLeftArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqPositive],ord(false),nil);
                if not Trms[lLeftArg^.TrmInfo].Supercluster^.fConsistent then
                  begin
                   SetContr(29);
                   exit;
                  end;
               end;
             if gBuiltIn[rqZero] > 0 then
              begin
               lZero:=Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqZero],nil);
               rZero:=Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqZero],nil);
               if (rZero<>nil) and (rZero^.fNeg=ord(false)) and
                   ((lNegative<>nil) and (lNegative^.fNeg=ord(false))) then
//                    or ((lZero<>nil) and (lZero^.fNeg=ord(true))) then
                begin
                 if Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil)=nil then AddedAttr:=true;
                 Trms[lRightArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqPositive],ord(true),nil);
                 if not Trms[lRightArg^.TrmInfo].Supercluster^.fConsistent then
                   begin
                    SetContr(30);
                    exit;
                   end;
                end;
               if (lZero<>nil) and (lZero^.fNeg=ord(false)) and
                     ((rPositive<>nil) and (rPositive^.fNeg=ord(false))) then
//                       or ((rZero<>nil) and (rZero^.fNeg=ord(true))) then
                begin
                 if Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil)=nil
                  then AddedAttr:=true;
                 Trms[lLeftArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqNegative],ord(true),nil);
                 if not Trms[lLeftArg^.TrmInfo].Supercluster^.fConsistent then
                   begin
                    SetContr(31);
                    exit;
                   end;
                end
              end;
           end;
           if Trms[lRightArg^.TrmInfo].NumValue.Determined and
             Trms[lLeftArg^.TrmInfo].NumValue.Determined and
              IsRationalGT(Trms[lRightArg^.TrmInfo].NumValue.NumericValue,
                           Trms[lLeftArg^.TrmInfo].NumValue.NumericValue) then
           begin
{$IFDEF CH_REPORT}
            CHReport.Out_NegNumReq2(rqLessOrEqual,
                           Trms[lLeftArg^.TrmInfo].NumValue.NumericValue,
                           Trms[lRightArg^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
             SetContr(32);
             exit;
           end;
         end
        else if lPred = gBuiltIn[rqBelongsTo] then
         begin
          lLeftArg:=lArgs^.XTrmPtr; lRightArg:=lArgs^.NextTrm^.XTrmPtr;
          if gBuiltIn[rqEmpty] > 0 then
           Trms[lRightArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqEmpty],0,nil);
          if not Trms[lRightArg^.TrmInfo].Supercluster^.fConsistent then
           begin
            SetContr(33);
            exit;
           end;
          if gBuiltIn[rqElement] > 0 then
           begin
            lInsTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                  gBuiltIn[rqElement],NewTrmList(lRightArg,nil));
            InsertType(lInsTyp,lLeftArg^.TrmInfo);
          end;
         end
        else if (lPred = gBuiltIn[rqInclusion]) and
                (gBuiltIn[rqElement] > 0) and (gBuiltIn[rqPowerSet] > 0)  then
         begin lLeftArg:=lArgs^.XTrmPtr;
          lRightArg:=lArgs^.NextTrm^.XTrmPtr;
          { Czy tu przenosic niepustosc na nadzbior ? }
          lRightArg:=
            NewFuncTrm(gBuiltIn[rqPowerSet],NewTrmList(CopyTerm(lRightArg),nil));
          YTerm(lRightArg);
          lInsTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                  gBuiltIn[rqElement],NewTrmList(lRightArg,nil));
          InsertType(lInsTyp,lLeftArg^.TrmInfo);
         end;
      end;
    end;
   end;

          { Zaszycie (czesciowe) BOOLE:11, :) :) }
   for b11:=0 to PosBas.Count-1 do
   for ii:=0 to PosBas.Count-1 do
    with FrmPtr(PosBas.Items^[ii])^ do
     if FrmSort = ikFrmPred then
      begin AdjustFrm(PredFrmPtr(PosBas.Items^[ii]),lPred,lArgs);
       if lPred = gBuiltIn[rqBelongsTo] then
         begin lLeftArg:=lArgs^.XTrmPtr;
          lRightArg:=lArgs^.NextTrm^.XTrmPtr;
          lTypClass.Init(0,4);
          for z:=0 to TrmS[lRightArg^.TrmInfo].XTypClass.Count-1 do
           begin
             TypPtr(TrmS[lRightArg^.TrmInfo].XTypClass.Items^[z])^.AdjustTyp(b11ModNr,b11Args);
             if (TypPtr(TrmS[lRightArg^.TrmInfo].XTypClass.Items^[z])^.TypSort = ikTypMode)
                and (b11ModNr = gBuiltIn[rqElement]) then
              begin
               llTL1:=Trms[b11Args^.XTrmPtr^.TrmInfo].EqClass;
               while llTL1<>nil do
                begin
                 if (llTL1^.XTrmPtr^.TrmSort = ikTrmFunctor) and
                    (FuncTrmPtr(llTL1^.XTrmPtr)^.FuncNr = gBuiltIn[rqPowerSet]) then
                  begin
                   lTrmInfo:=FuncTrmPtr(llTL1^.XTrmPtr)^.FuncArgs^.XTrmPtr^.TrmInfo;
                   {!!!uwaga czy zawsze jest gBuiltIn[rqEmpty] <> 0}
                   Trms[lTrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqEmpty],0,nil);
                   if not Trms[lTrmInfo].Supercluster^.fConsistent then SetContr(34);
                   llInsTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                            gBuiltIn[rqElement],
                     NewTrmList(FuncTrmPtr(llTL1^.XTrmPtr)^.FuncArgs^.XTrmPtr,nil));
                   lTypClass.Insert(llInsTyp);
                  end;
                 llTL1:=llTL1^.NextTrm;
                end;
              end;
           end;
          if Contr > 0 then
           begin lTypClass.Done;
            exit
           end;
          for z:=0 to lTypClass.Count-1 do
           InsertType(TypPtr(lTypClass.Items^[z]),lLeftArg^.TrmInfo);
          lTypClass.DeleteAll;
          lTypClass.Done;
         end;
      end;
  { ### Tutaj jest problem - szukamy sprzecznosci miedzy formulami,
    nalezaloby wykorzystac "connectedness" - chyba nalezy szukac
    tutaj rozwiazan, a nie dogenerowywac formule.
  }

{$IFDEF MDEBUG}
//writeln(InfoFile,'TRMS ----');
//infoeqclasses;
{$ENDIF} ;

   AddedAttr:=true;
   while AddedAttr do
   begin
    AddedAttr:=false;
    for ii:=0 to NegBas.Count-1 do
     begin
      case FrmPtr(NegBas.Items^[ii])^.FrmSort of
       ikFrmAttr:
        begin
          with PredFrmPtr(NegBas.Items^[ii])^ do
           lAttr:=Trms[LastArg(PredArgs)^.TrmInfo].SuperCluster^.GetAttr(PredNr,CopyTermList1(PredArgs));
          if (lAttr<>nil) and (lAttr^.fNeg=ord(true)) then
           begin
            SetContr(35);
            exit;
           end;
         goto 111;
        end;
       ikFrmSchPred,ikFrmPrivPred:
  111:  begin
         for j:=0 to PosBas.Count-1 do
          if FrmPtr(PosBas.Items^[j])^.FrmSort=FrmPtr(NegBas.Items^[ii])^.FrmSort then
           if PredFrmPtr(PosBas.Items^[j])^.PredNr=PredFrmPtr(NegBas.Items^[ii])^.PredNr then
            if EqTrmLists(PredFrmPtr(PosBas.Items^[j])^.PredArgs,PredFrmPtr(NegBas.Items^[ii])^.PredArgs) then
             begin
              SetContr(36);
              exit;
             end;
        end;
       ikFrmPred:
        begin AdjustFrm(PredFrmPtr(NegBas.Items^[ii]),lPred,lArgs);
         if lPred =  gBuiltIn[rqLessOrEqual] then
           begin lLeftArg:=lArgs^.XTrmPtr; lRightArg:=lArgs^.NextTrm^.XTrmPtr;
            if (gBuiltIn[rqPositive] > 0) and (gBuiltIn[rqNegative] > 0) then
             begin
               lPositive:=Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil);
               lNegative:=Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil);
               rPositive:=Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil);
               rNegative:=Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil);
               if (lPositive<>nil) and (lPositive^.fNeg=ord(false)) then
                 begin
                  if Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqNegative],nil)=nil then AddedAttr:=true;
                  Trms[lRightArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqNegative],ord(true),nil);
                  if not Trms[lRightArg^.TrmInfo].Supercluster^.fConsistent then
                   begin
                    SetContr(37);
                    exit;
                   end;
                 end;
               if (rNegative<>nil) and (rNegative^.fNeg=ord(false)) then
                 begin
                  if Trms[lLeftArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqPositive],nil)=nil
                   then AddedAttr:=true;
                  Trms[lLeftArg^.TrmInfo].Supercluster^.InsertAttr(gBuiltIn[rqPositive],ord(true),nil);
                  if not Trms[lLeftArg^.TrmInfo].Supercluster^.fConsistent then
                    begin
                     SetContr(38);
                     exit;
                    end;
                 end;
             end;
            if Trms[lRightArg^.TrmInfo].NumValue.Determined and
             Trms[lLeftArg^.TrmInfo].NumValue.Determined and
             IsRationalLE(Trms[lLeftArg^.TrmInfo].NumValue.NumericValue,
                          Trms[lRightArg^.TrmInfo].NumValue.NumericValue) then
            begin
{$IFDEF CH_REPORT}
             CHReport.Out_NumReq2(rqLessOrEqual,
                         Trms[lLeftArg^.TrmInfo].NumValue.NumericValue,
                         Trms[lRightArg^.TrmInfo].NumValue.NumericValue);
{$ENDIF}
             SetContr(39);
             exit;
            end;
           end
         else if (lPred = gBuiltIn[rqBelongsTo]) and
                (gBuiltIn[rqElement] > 0 ) then
           begin lLeftArg:=lArgs^.XTrmPtr;
            lRightArg:=lArgs^.NextTrm^.XTrmPtr;
           { Sprawdzamy, czy zbior RightArg jest niepusty }
            lAttr:=Trms[lRightArg^.TrmInfo].Supercluster^.GetAttr(gBuiltIn[rqEmpty],nil);
            if (lAttr<>nil) and (lAttr^.fNeg=ord(false)) then
             begin
              lTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                    gBuiltIn[rqElement],
                                    NewTrmList(CopyTerm(lRightArg),nil));
               with Trms[lLeftArg^.TrmInfo].XTypClass do
               begin
                zz1 := 0;
                while (zz1 < Count) and not(TypPtr(Items^[zz1])^.DecreasingAttrs(lTyp,EqAttrs)  {***}
                                            and lTyp^.EqRadices(Items^[zz1])) do Inc(zz1);
                if zz1 < Count then
                 begin dispose(lTyp,Done);
                  SetContr(40);
                  exit
                 end;
               end;
              dispose(lTyp,Done);
             end;
           end
         else if (lPred = gBuiltIn[rqInclusion]) and
                 (gBuiltIn[rqElement] > 0) and (gBuiltIn[rqPowerSet] > 0)  then
           begin lLeftArg:=lArgs^.XTrmPtr;
            lRightArg:=lArgs^.NextTrm^.XTrmPtr;
            lRightArg:=
              NewFuncTrm(gBuiltIn[rqPowerSet],NewTrmList(CopyTerm(lRightArg),nil));
            YTerm(lRightArg);
            lTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                 gBuiltIn[rqElement],NewTrmList(lRightArg,nil));
             with Trms[lLeftArg^.TrmInfo].XTypClass do
             begin
              zz2:=0;
              while (zz2 < Count) and not(TypPtr(Items^[zz2])^.DecreasingAttrs(lTyp,EqAttrs)  {***}
                    and lTyp^.EqRadices(Items^[zz2])) do Inc(zz2);
              if zz2 < Count then
               begin dispose(lTyp,Done);
                SetContr(41);
                exit
               end;
             end;
            dispose(lTyp,Done);
           end;
         for j:=0 to PosBas.Count-1 do
          if EqFrms(PosBas.Items^[j],NegBas.Items^[ii]) then
           begin
            SetContr(42);
            exit;
           end;
        end;
       ikFrmQual:
        begin hQualTyp:=QualFrmPtr(NegBas.Items^[ii])^.QualTyp;
         with hQualTyp^,Trms[QualFrmPtr(NegBas.Items^[ii])^.QualTrm^.TrmInfo].XTypClass do
          for zz:=0 to Count-1 do
           if EqRadices(TypPtr(Items^[zz])) then
            begin
             SetContr(43);
             exit;
            end;
        end;
      end;
     end;
   end;

//Start of EqStack

{$IFDEF DEBUGSUPERCLUSTERS}
writeln(InfoFile,'---------------------- ANEqClasses:');
infoeqclasses;
writeln(InfoFile,'---------------------- ');
infonewline;writeln(infofile,'CCLUSTERS:',ConditionalCluster.Count);
for ANj:=0 to ConditionalCluster.Count-1 do
begin
infonewline;writeln(infofile,'(',anj,'):');
infostring('ante:');infocluster(cclusterptr(conditionalcluster.items^[anj])^.nantecedent);infonewline;
infostring('cons:');infocluster(cclusterptr(conditionalcluster.items^[anj])^.nconsequent.upper);infonewline;
infostring('type:');infotype(cclusterptr(conditionalcluster.items^[anj])^.nclustertype);infonewline;
infostring('prim:');infotypecoll(cclusterptr(conditionalcluster.items^[anj])^.nprimarylist);infonewline;
end;
infonewline;writeln(infofile,'FCLUSTERS:',FunctorCluster.Count);
for ANj:=0 to FunctorCluster.Count-1 do
begin
infonewline;writeln(infofile,'(',anj,'):');
infostring('cons:');infocluster(fclusterptr(functorcluster.items^[anj])^.nconsequent.upper);infonewline;
infostring('type:');if fclusterptr(functorcluster.items^[anj])^.nclustertype <> nil then infotype(fclusterptr(functorcluster.items^[anj])^.nclustertype) else write(infofile,'NIL type');infonewline;
infostring('term:');infoterm(fclusterptr(functorcluster.items^[anj])^.nclusterterm);infonewline;
infostring('prim:');infotypecoll(fclusterptr(functorcluster.items^[anj])^.nprimarylist);infonewline;
end;
{$ENDIF}

{$IFDEF MINI_PROFILER}
MiniProfiler.SectionBegin ('3.2. SuperclusterRound');
{$ENDIF}
   for ANi:=1 to trmnbr do
   begin
     ns1:=DependentClasses(ANi);
     gDependencies.Insert(ns1);
{$IFDEF DEBUGSUPERCLUSTERS}
if ns1<>nil then begin write(infofile,'Dep[',ani,']:');infonatset(ns1^);infonewline; end;
{$ENDIF}
   end;
   for ANi:=1 to trmnbr do
   begin
    if TrmS[ANi].EqClass<>nil then
     ANEqStack.InsertElem(ANi);
   end;
   while ANEqStack.Count > 0 do
   begin
     ANi:=ANEqStack.Items^[0].X;
     ANEqStack.DeleteElem(ANi);
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'Rounding:    ',ani);
{$ENDIF}
     if RoundUpSuperCluster(ANi) then
      begin
       if Contr > 0 then
        begin
{$IFDEF MINI_PROFILER}
MiniProfiler.SectionEnd;
{$ENDIF}
         exit;
        end;
       for ANj:=1 to trmnbr do
        if DependsOn(ANi,ANj) then
         with NatFuncPtr(gDependencies.Items^[anj-1])^ do
          if not HasInDom(ANi) then
          begin
           InsertElem(ANi);
{$IFDEF DEBUGSUPERCLUSTERS}
writeln(infofile,'Extra inserted:',ani,' w ', anj);
{$ENDIF}
          end;
       ns1:=DependentClasses(ANi);
       if ns1 <> nil then
       begin
        for ANj:=0 to ns1^.Count-1 do
          ANEqStack.InsertElem(ns1^.items^[anj].X);
        dispose(ns1,Done);
       end;
      end;
     if Contr > 0 then
      begin
{$IFDEF MINI_PROFILER}
MiniProfiler.SectionEnd;
{$ENDIF}
       exit;
      end;
   end;
{$IFDEF MINI_PROFILER}
MiniProfiler.SectionEnd;
{$ENDIF}

//End of EqStack

{$IFDEF MDEBUG}
//writeln(InfoFile,'TRMS ----');
//infoeqclasses;
{$ENDIF} ;

EndEquate:

end { Equate };

procedure DispEquations;
 var ii: integer;
     e: ComplexTrmExprKind;
begin
  NegBas.Done;
  PosBas.Done;

  DTrm.DeleteAll;
  DTrm.Done;
  for e := Low(FuncTrmExprKind) to High(FuncTrmExprKind) do
   FuncTrmList[e].Done;
  FrOper.DeleteAll;
  FrOper.Done;
  ChoiceTerm.DeleteAll;
  ChoiceTerm.Done;
//    lConstSet.Done;

  AllowedCCluster.Done;
  AllowedFCluster.Done;

  gDependencies.Done;
  ANEqStack.Done;

  for ii:=1 to TrmNbr do
   begin
    if TrmS[ii].EqClass <> nil then
    begin
     DisposingSuperclusters:=true;
     dispose(TrmS[ii].SuperCluster,Done);
     DisposingSuperclusters:=false;
    end; 
    dispose(TrmS[ii].Term);
   end;
end;

procedure DispEqClassInTrms;
 var ii: integer;
begin
 for ii:=1 to TrmNbr do
  with Trms[ii] do
  begin
   if EqClass <> nil then DisposeTrmList(EqClass);
   XTypClass.Done;
  end;
end;

end.
