(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit unifier;

interface

uses mobjects,limits,correl,enums,equalizer;

var
  ETrm: array[1..MaxTrmNbr] of VarTrmObj;
  EqClassVal: array[1..MaxTrmNbr] of ValRec;
  EqClassType: array[1..MaxTrmNbr] of MCollection;
  EqClassSuperCluster: array[1..MaxTrmNbr] of AttrCollectionPtr;
  DConstEqClass: NatFunc;
  EqList: array[ComplexTrmExprKind] of array of MListPtr;

procedure Unification;

implementation

uses mconsole,errhan,builtin,lexicon,numbers,prechecker,mscanner
{$IFDEF CH_REPORT},req_info,prephan{$ENDIF}
{$IFDEF MDEBUG},info,outinfo{$ENDIF};

function EqTrmsClass ( fTrm1,fTrm2:TrmPtr ): boolean; FORWARD;

function EqTLClasss(FTL1,FTL2: TrmList): boolean;
begin
 while (FTL1 <> nil) and (FTL2 <> nil) do
  begin
   if not EqTrmsClass(FTL1^.XTrmPtr,FTL2^.XTrmPtr) then
     begin EqTLClasss:=false;
      exit
     end;
   FTL1 := FTL1^.NextTrm; FTL2 := FTL2^.NextTrm;
  end;
 EqTLClasss := FTL1 = FTL2
end;

function EqClassTyps ( fTyp1,fTyp2:TypPtr ): boolean; FORWARD;

function EqClassRadices ( fTyp1,fTyp2:TypPtr ): boolean;
 var ModNr1,ModNr2: integer; A1,A2: TrmList;
begin EqClassRadices:=false;
 if FTYP1^.TypSort=fTyp2^.TypSort then
  case FTYP1^.TypSort of
   ikTypMode:
     begin fTyp1^.AdjustTyp(ModNr1,A1); fTyp2^.AdjustTyp(ModNr2,A2);
      if ModNr1=ModNr2 then EqClassRadices:=EqTLClasss(A1,A2);
     end;
   ikTypStruct:
    if FTYP1^.ModNr=fTyp2^.ModNr then
     EqClassRadices:=EqTLClasss(FTYP1^.ModArgs,fTyp2^.ModArgs);
  end;
end;

function EqClassAttrs(fAttr1,fAttr2:AttrPtr): boolean;
  var lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 fAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 fAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 EqClassAttrs := (lAttrNr1 = lAttrNr2) and
           (fAttr1^.fNeg = fAttr2^.fNeg) and
           EqTLClasss(lArgs1,lArgs2);
end;

function EqClassTyps ( fTyp1,fTyp2:TypPtr ): boolean;
begin EqClassTyps:=false;
 if EqualClusters(fTyp1,fTyp2,EqClassAttrs) then
  EqClassTyps:=EqClassRadices(fTyp1,fTyp2);
end;

function EqFrmsClass ( fFrm1,fFrm2:FrmPtr ): boolean;
 var PredNr1,PredNr2,i: integer; A1,A2: TrmList;
     lFlag: boolean; lTrmList,lTrmList1:TrmList;
begin EqFrmsClass := false;
 if fFrm1^.FrmSort=fFrm2^.FrmSort then
  case fFrm1^.FrmSort of
   ikFrmNeg: EqFrmsClass := EqFrmsClass(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^.Conjuncts do
     if Count = ConjFrmPtr(fFrm2)^.Conjuncts.Count then
      begin
       for i:=0 to Count-1 do
        if not EqFrmsClass(FrmPtr(Items^[i]),FrmPtr(ConjFrmPtr(fFrm2)^.Conjuncts.Items^[i])) then
         exit;
       EqFrmsClass:=true;
      end;
   ikFrmPred:
    begin
     AdjustFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
       begin
        if EqTLClasss(A1,A2) then
         begin EqFrmsClass:=true;
          exit
         end;
{ ### Podobnie tutaj potrzebujemy reguly zastepowania, nie mozna wiec tego
  przenosci na "connectedness".
}
       if sySymmetry in ConstrPtr( Constr[ coPredicate].At(
                                 PredFrmPtr(fFrm1)^.PredNr))^.fProperties then
        with PredFrmPtr(fFrm1)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
       begin
        lTrmList:=SwitchArgs(fFirstArg,fSecondArg,PredArgs);
        lTrmList1:=AdjustTrmList(ikFrmPred,PredNr,lTrmList);
        lFlag:=EqTLClasss(lTrmList1,A2);
        DisposeTrmList(lTrmList);
        EqFrmsClass:=lFlag;
       end
       else
        if sySymmetry in ConstrPtr( Constr[ coPredicate].At(
                                 PredFrmPtr(fFrm2)^.PredNr))^.fProperties then
         with PredFrmPtr(fFrm2)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
        begin
         lTrmList:=SwitchArgs(fFirstArg,fSecondArg,PredArgs);
         lTrmList1:=AdjustTrmList(ikFrmPred,PredNr,lTrmList);
         lFlag:=EqTLClasss(A1,lTrmList1);
         DisposeTrmList(lTrmList);
         EqFrmsClass:=lFlag;
        end
      end;
    end;
   ikFrmUniv:
    if EqClassTyps(UnivFrmPtr(fFrm1)^.Quantified,UnivFrmPtr(fFrm2)^.Quantified) then
     EqFrmsClass := EqFrmsClass(UnivFrmPtr(fFrm1)^.Scope,UnivFrmPtr(fFrm2)^.Scope);
   ikFrmQual:
    if EqTrmsClass(QualFrmPtr(fFrm1)^.QualTrm,QualFrmPtr(fFrm2)^.QualTrm) then
     EqFrmsClass:=EqClassTyps(QualFrmPtr(fFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
   ikFrmAttr:
    begin
     AdjustAttrFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustAttrFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
      EqFrmsClass := EqTLClasss(A1,A2);
    end;
   ikFrmSchPred,ikFrmPrivPred:
    if PredFrmPtr(fFrm1)^.PredNr=PredFrmPtr(fFrm2)^.PredNr then
     EqFrmsClass := EqTLClasss(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
   ikFrmVerum: EqFrmsClass := true;
  end;
end;

function EqClassNr(FTrm:TrmPtr): integer;
 label 1,2,3,4,5;
 var LTA: array[1..MaxArgNbr] of integer;
     LT,I,J,FuncNr1,FuncNr2: integer;
     lTL,lTL1,Args1,Args2: TrmList;
begin EqClassNr:=0;
 case FTrm^.TrmSort of
  ikTrmEqConst: EqClassNr:=VarTrmPtr(FTrm)^.VarNr;
  ikTrmNumeral:
    for i:=1 to EqClassNbr do
     with EqClassVal[i] do
     if Determined and IsEqWithInt(NumericValue,VarTrmPtr(FTrm)^.VarNr) then
        begin EqClassNr:=i;
         exit
        end;
  ikTrmInfConst:
   if DConstEqClass.HasInDom(VarTrmPtr(FTrm)^.VarNr) then
    EqClassNr:=DConstEqClass.Value(VarTrmPtr(FTrm)^.VarNr);
  ikTrmFunctor:
   begin I:=1;
    lTL:=FuncTrmPtr(FTrm)^.FuncArgs;
    while lTL <> nil do
     begin LTA[I]:=EqClassNr(lTL^.XTrmPtr);
       if LTA[I]=0 then exit;
       lTL:=lTL^.NextTrm; inc(I);
     end;
    for J:=1 to EqClassNbr do
    begin
     for LT:=0 to EqList[expTrmFunctor][j].Count-1 do
       begin I:=1;
         lTL:=FuncTrmPtr(EqList[expTrmFunctor][j]^.Items^[LT])^.FuncArgs;
        if FuncTrmPtr(FTrm)^.FuncNr<>FuncTrmPtr(EqList[expTrmFunctor][j]^.Items^[LT])^.FuncNr then
         begin AdjustTrm(FTrm,FuncNr1,Args1);
           AdjustTrm(EqList[expTrmFunctor][j]^.Items^[LT],FuncNr2,Args2);
           if FuncNr1<>FuncNr2 then goto 1;
           lTL1:=FuncTrmPtr(FTrm)^.FuncArgs;
           while lTl1<>Args1 do
            begin inc(I);
             lTl1:=lTl1^.NextTrm
            end;
           lTl:=Args2;
         end;
        while lTL<>nil do
         begin
           if VarTrmPtr(lTL^.XTrmPtr)^.VarNr<>LTA[I] then goto 1;
           lTL:=lTL^.NextTrm; inc(I);
         end;
        EqClassNr:=J; exit;
1:
       end;
    end;
   end;
  ikTrmAggreg:
   begin I:=1;
    lTL:=FuncTrmPtr(FTrm)^.FuncArgs;
    while lTL <> nil do
     begin LTA[I]:=EqClassNr(lTL^.XTrmPtr);
       if LTA[I]=0 then exit;
       lTL:=lTL^.NextTrm; inc(I);
     end;
    for J:=1 to EqClassNbr do
    begin
      for LT:=0 to EqList[expTrmAggreg][j]^.Count-1 do
      begin I:=1;
        lTL:=FuncTrmPtr(EqList[expTrmAggreg][j]^.Items^[LT])^.FuncArgs;
        if FuncTrmPtr(FTrm)^.FuncNr<>FuncTrmPtr(EqList[expTrmAggreg][j]^.Items^[LT])^.FuncNr then goto 2;
        while lTL<>nil do
         begin
           if VarTrmPtr(lTL^.XTrmPtr)^.VarNr<>LTA[I] then goto 2;
           lTL:=lTL^.NextTrm; inc(I);
         end;
        EqClassNr:=J; exit;
 2:
      end;
     end;
   end;
  ikTrmSchFunc:
   begin I:=1;
    lTL:=FuncTrmPtr(FTrm)^.FuncArgs;
    while lTL <> nil do
     begin LTA[I]:=EqClassNr(lTL^.XTrmPtr);
       if LTA[I]=0 then exit;
       lTL:=lTL^.NextTrm; inc(I);
     end;
    for J:=1 to EqClassNbr do
     begin
      for LT:=0 to EqList[expTrmSchFunc][j]^.Count-1 do
      begin I:=1;
         lTL:=FuncTrmPtr(EqList[expTrmSchFunc][j]^.Items^[LT])^.FuncArgs;
        if FuncTrmPtr(FTrm)^.FuncNr<>FuncTrmPtr(EqList[expTrmSchFunc][j]^.Items^[LT])^.FuncNr then goto 3;
        while lTL<>nil do
         begin
           if VarTrmPtr(lTL^.XTrmPtr)^.VarNr<>LTA[I] then goto 3;
           lTL:=lTL^.NextTrm; inc(I);
         end;
        EqClassNr:=J; exit;
 3:
      end;
     end;
   end;
  ikTrmPrivFunc:
   begin I:=1;
    lTL:=FuncTrmPtr(FTrm)^.FuncArgs;
    while lTL <> nil do
     begin LTA[I]:=EqClassNr(lTL^.XTrmPtr);
       if LTA[I]=0 then exit;
       lTL:=lTL^.NextTrm; inc(I);
     end;
    for J:=1 to EqClassNbr do
     begin
      for LT:=0 to EqList[expTrmPrivFunc][j]^.Count-1 do
      begin I:=1;
        lTL:=FuncTrmPtr(EqList[expTrmPrivFunc][j]^.Items^[LT])^.FuncArgs;
        if FuncTrmPtr(FTrm)^.FuncNr<>FuncTrmPtr(EqList[expTrmPrivFunc][j]^.Items^[LT])^.FuncNr then goto 3;
        while lTL<>nil do
         begin
           if VarTrmPtr(lTL^.XTrmPtr)^.VarNr<>LTA[I] then goto 3;
           lTL:=lTL^.NextTrm; inc(I);
         end;
        EqClassNr:=J; exit;
 5:
      end;
     end;
   end;
  ikTrmSelector:
   begin I:=1;
    lTL:=FuncTrmPtr(FTrm)^.FuncArgs;
    while lTL <> nil do
     begin LTA[I]:=EqClassNr(lTL^.XTrmPtr);
       if LTA[I]=0 then exit;
       lTL:=lTL^.NextTrm; inc(I);
     end;
    for J:=1 to EqClassNbr do
     begin
      for LT:=0 to EqList[expTrmSelector][j]^.Count-1 do
      begin I:=1;
        lTL:=FuncTrmPtr(EqList[expTrmSelector][j]^.Items^[LT])^.FuncArgs;
        if FuncTrmPtr(FTrm)^.FuncNr<>FuncTrmPtr(EqList[expTrmSelector][j]^.Items^[LT])^.FuncNr then goto 4;
        while lTL<>nil do
         begin
           if VarTrmPtr(lTL^.XTrmPtr)^.VarNr<>LTA[I] then goto 4;
           lTL:=lTL^.NextTrm; inc(I);
         end;
        EqClassNr:=J; exit;
 4:
      end;
     end;
   end;
  ikTrmFraenkel:
   begin
    for j:=1 to EqClassNbr do
    begin
      for LT:=0 to EqList[expTrmFraenkel][j]^.Count-1 do
      if EqTrmsClass(fTrm,EqList[expTrmFraenkel][j]^.Items^[LT]) then
       begin EqClassNr:=j;
        exit
       end;
    end;
   end;
  ikTrmChoice:
   begin
    for j:=1 to EqClassNbr do
    begin
      for LT:=0 to EqList[expTrmChoice][j]^.Count-1 do
      if EqTrmsClass(fTrm,EqList[expTrmChoice][j]^.Items^[LT]) then
       begin EqClassNr:=j;
        exit
       end;
    end;
   end;
  ikTrmBound,ikTrmLocus:;
  else
begin
{$IFDEF MDEBUG}
writeln(InfoFile,fTrm^.TrmSort,'|');
{$ENDIF}
   RunTimeError(2033);
end;
 end;
end;

function EqTrmsClass ( fTrm1,fTrm2:TrmPtr ): boolean;
  var FuncNr1,FuncNr2,i: integer;
      A1,A2: TrmList;
begin EqTrmsClass:=false;
 if fTrm2^.TrmSort=fTrm1^.TrmSort then
  case fTrm2^.TrmSort of
   ikTrmBound,ikTrmEqConst: EqTrmsClass:=VarTrmPtr(fTrm1)^.VarNr=VarTrmPtr(fTrm2)^.VarNr;
   ikTrmFunctor:
    begin AdjustTrm(fTrm1,FuncNr1,A1); AdjustTrm(fTrm2,FuncNr2,A2);
     if FuncNr1=FuncNr2 then EqTrmsClass:=EqTLClasss(A1,A2);
    end;
   ikTrmSchFunc,ikTrmPrivFunc,ikTrmAggreg,ikTrmSelector:
        if FuncTrmPtr(fTrm1)^.FuncNr=FuncTrmPtr(fTrm2)^.FuncNr then
          EqTrmsClass:=EqTLClasss(FuncTrmPtr(fTrm1)^.FuncArgs,FuncTrmPtr(fTrm2)^.FuncArgs);
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm1)^ do
     if LambdaArgs.Count = FraenkelTrmPtr(fTrm2)^.LambdaArgs.Count then
      begin
       for i:=0 to LambdaArgs.Count-1 do
        if not EqClassTyps(LambdaArgs.Items^[i],FraenkelTrmPtr(fTrm2)^.LambdaArgs.Items^[i]) then
          exit;
       if EqTrmsClass(LambdaScope,FraenkelTrmPtr(fTrm2)^.LambdaScope) then
        EqTrmsClass := EqFrmsClass(Compr,FraenkelTrmPtr(fTrm2)^.Compr);
      end;
   ikTrmChoice:
    with ChoiceTrmPtr(fTrm1)^ do
     EqTrmsClass := EqClassTyps(ChoiceTyp,ChoiceTrmPtr(fTrm2)^.ChoiceTyp);
  end
 else if fTrm2^.TrmSort=ikTrmEqConst then
  EqTrmsClass:=EqClassNr(fTrm1)=VarTrmPtr(fTrm2)^.VarNr;
end;

var FreeBasic: MCollection;
    FreeVarBase: integer;

procedure SetFreeVarInTrm(var fTrm:TrmPtr);
begin
 if fTrm^.Trmsort=ikTrmBound then
  with VarTrmPtr(fTrm)^ do
  if VarNr>BoundVarNbr then dec(VarNr,BoundVarNbr)
   else
    begin TrmSort:=ikTrmFreeVar;
     inc(VarNr,FreeVarBase)
    end;
end;

procedure RemoveIntQuantifier(var aFrm:FrmPtr); forward;

procedure RemoveExtQuantifier(var aFrm:FrmPtr);
 var lFrm: FrmPtr;
     lConjuncts:MCollection;
     i: integer;
begin
 case aFrm^.FrmSort of
  ikFrmNeg:
   begin
    RemoveIntQuantifier(NegFrmPtr(aFrm)^.NegArg);
    if NegFrmPtr(aFrm)^.NegArg^.FrmSort = ikFrmNeg then
     begin
      lFrm:=aFrm;
      aFrm:=NegFrmPtr(NegFrmPtr(aFrm)^.NegArg)^.NegArg;
      dispose(NegFrmPtr(lFrm)^.NegArg);
      dispose(lFrm);
     end;
   end;
  ikFrmUniv:
    begin
     InitVarBase;
     BoundVarNbr:=0;
     repeat
       inc(BoundVarNbr);
       NewVariable(UnivFrmPtr(aFrm)^.Quantified^.CopyType);
       lFrm:=aFrm;
       aFrm:=UnivFrmPtr(aFrm)^.Scope;
       dispose(lFrm);
     until aFrm^.FrmSort<>ikFrmUniv;
     WithinFormula(aFrm,SetVarInTrm);
     RemoveExtQuantifier(aFrm);
    end;
  ikFrmConj:
   with ConjFrmPtr(aFrm)^ do
    begin
     lConjuncts.Init(Conjuncts.Count,2);
     for i:=0 to Conjuncts.Count-1 do
      begin lFrm:=ConjFrmPtr(Conjuncts.Items^[i]);
       RemoveExtQuantifier(lFrm);
       if lFrm^.FrmSort = ikFrmConj then
        begin lConjuncts.AppendTo(ConjFrmPtr(lFrm)^.Conjuncts);
         ConjFrmPtr(lFrm)^.Conjuncts.DeleteAll;
         dispose(lFrm,Done);
        end
       else lConjuncts.Insert(lFrm);
      end;
     Conjuncts.DeleteAll;
     Conjuncts.Done;
     Conjuncts.MoveCollection(lConjuncts);
    end;
  ikFrmPrivPred,ikFrmSchPred,ikFrmAttr,ikFrmPred,ikFrmQual,ikFrmThesis,ikFrmVerum,ikFrmError,ikFrmFlexConj:;
  else RunTimeError(2640);
 end;
end;

procedure RemoveIntQuantifier(var aFrm:FrmPtr);
 var lFrm: FrmPtr;
     lConjuncts:MCollection;
     i: integer;
begin
 case aFrm^.FrmSort of
  ikFrmNeg:
   begin
    RemoveExtQuantifier(NegFrmPtr(aFrm)^.NegArg);
    if NegFrmPtr(aFrm)^.NegArg^.FrmSort = ikFrmNeg then
     begin
      lFrm:=aFrm;
      aFrm:=NegFrmPtr(NegFrmPtr(aFrm)^.NegArg)^.NegArg;
      dispose(NegFrmPtr(lFrm)^.NegArg);
      dispose(lFrm);
     end;
   end;
  ikFrmConj:
   with ConjFrmPtr(aFrm)^ do
    begin
     lConjuncts.Init(Conjuncts.Count,2);
     for i:=0 to Conjuncts.Count-1 do
      begin lFrm:=ConjFrmPtr(Conjuncts.Items^[i]);
       RemoveIntQuantifier(lFrm);
       if lFrm^.FrmSort = ikFrmConj then
        begin lConjuncts.AppendTo(ConjFrmPtr(lFrm)^.Conjuncts);
         ConjFrmPtr(lFrm)^.Conjuncts.DeleteAll;
         dispose(lFrm,Done);
        end
       else lConjuncts.Insert(lFrm);
      end;
     Conjuncts.DeleteAll;
     Conjuncts.Done;
     Conjuncts.MoveCollection(lConjuncts);
    end;
  ikFrmUniv,ikFrmPrivPred,ikFrmSchPred,ikFrmAttr,ikFrmPred,ikFrmQual,
  ikFrmThesis,ikFrmVerum,ikFrmError,ikFrmFlexConj: ;
  else RunTimeError(2641);
 end;
end;

var gFreeVarsBase,gFreeVarsNbr: integer;

procedure SetNewFreeVarInTrm(var fTrm:TrmPtr);
begin
 if fTrm^.Trmsort=ikTrmFreeVar then
  with VarTrmPtr(fTrm)^ do
  if VarNr <= gFreeVarsnbr then inc(VarNr,gFreeVarsNbr-gFreeVarsBase)
end;

procedure InitFreeVarBase;
begin
 FreeVarBase:=FreeVarType.Count;
end;

procedure NewFreeVar(aTyp:TypPtr);
 var lTyp: TypPtr;
begin
 lTyp:=aTyp^.CopyType;
 lTyp^.WithinType(SetFreeVarInTrm);
 FreeVarType.Insert(lTyp);
end;

function StandarizeTerm(fTrm:TrmPtr): TrmPtr; FORWARD;

function StandarizeTermList ( fTL:TrmList ):TrmList;
 var lTrmElem:TrmElem; Sentinel:TrmList;
begin Sentinel:=addr(lTrmElem);
 while fTL <> nil do
  begin new(Sentinel^.NextTrm); Sentinel:=Sentinel^.NextTrm;
   Sentinel^.XTrmPtr:=StandarizeTerm(fTL^.XTrmPtr); fTl:=fTL^.NextTrm;
  end;
 Sentinel^.NextTrm:=nil;
 StandarizeTermList:=lTrmElem.NextTrm;
end;

function StandarizeType ( fTyp:TypPtr ): TypPtr;
 var lModArgs:TrmList;
begin
 with fTyp^ do
  begin
   lModArgs:=StandarizeTermList(ModArgs);
   StandarizeType:=new(TypPtr,Init(TypSort,
     InstCluster(LowerCluster,lModArgs),InstCluster(UpperCluster,lModArgs),
      ModNr,lModArgs));
  end;
end;

function StandarizeTerm(fTrm:TrmPtr): TrmPtr;
begin
 StandarizeTerm:=CopyTerm(fTrm);
end;

function ChReconQualFrm(aSubject:TrmPtr; aAttrNr:integer; aSign:boolean; aArgs:TrmList): QualFrmPtr;
 var lTyp:TypPtr;
     lClusterPtr:AttrCollectionPtr;
begin
 lTyp:=CopyTrmType(aSubject);
 lClusterPtr:=CopyCluster(lTyp^.LowerCluster);
 lClusterPtr^.InsertAttr(aAttrNr,ord(aSign),CopyTermList(aArgs)); {??}
 if not lClusterPtr^.fConsistent then
  begin ChReconQualFrm:=QualFrmPtr(NewNeg(NewVerum));
   dispose(lTyp,Done);
   DisposeTrm(aSubject);
   DisposeTrmList(aArgs);
   dispose(lClusterPtr,Done);
   exit;
  end;
 dispose(lTyp^.LowerCluster,Done);
 lTyp^.LowerCluster:=lClusterPtr;
 dispose(lTyp^.UpperCluster,Done);
 lTyp^.UpperCluster:=CopyCluster(lTyp^.LowerCluster);
 ChReconQualFrm:=QualFrmPtr(NewQualFrm(aSubject,lTyp));
end;

function NegativelyStandarized(fFrm:FrmPtr):FrmPtr; FORWARD;

function PositivelyStandarized(fFrm:FrmPtr): FrmPtr;
 var lConjuncts: MCollection;
     lQuantified: TypPtr;
     lTrm:TrmPtr;
     lReconQualFrm:FrmPtr;
     z: integer;
begin
   {$IFDEF MDEBUG}
   InfoString('PS: '); InfoFormula(fFrm); InfoNewLine;
   {$ENDIF}
 with fFrm^ do
  case FrmSort of
   ikFrmConj:
    with ConjFrmPtr(fFrm)^.Conjuncts do
    begin lConjuncts.Init(Count,2);
     for z:=0 to Count-1 do
      begin
       lConjuncts.Insert(PositivelyStandarized(FrmPtr(Items^[z])))
      end;
     PositivelyStandarized:=new(ConjFrmPtr, Init(lConjuncts));
    end;
   ikFrmUniv:
    begin
     inc(BoundVarNbr);
     lQuantified:=StandarizeType(UnivFrmPtr(fFrm)^.Quantified);
     BoundVar[BoundVarNbr]:=lQuantified;
     PositivelyStandarized:=
      new(UnivFrmPtr, Init(ikFrmUniv,lQuantified,PositivelyStandarized(UnivFrmPtr(fFrm)^.Scope)));
     dec(BoundVarNbr);
    end;
   ikFrmQual:
    PositivelyStandarized:=
     new(QualFrmPtr, Init(StandarizeTerm(QualFrmPtr(fFrm)^.QualTrm),StandarizeType(QualFrmPtr(fFrm)^.QualTyp)));
   ikFrmAttr:
     begin lTrm:=StandarizeTerm(LastArg(PredFrmPtr(fFrm)^.PredArgs));
      if lTrm^.TrmSort <> ikTrmEqConst then
       begin lReconQualFrm:=ChReconQualFrm(lTrm,PredFrmPtr(fFrm)^.PredNr,true,
                               CopyTermList1(PredFrmPtr(fFrm)^.PredArgs));
        PositivelyStandarized:=PositivelyStandarized(lReconQualFrm);
        dispose(lReconQualFrm,Done); { podoptymalizowac ! }
       end
      else PositivelyStandarized:=fFrm^.CopyFormula;
     end;
   ikFrmSchPred,ikFrmPred:
    PositivelyStandarized:=
     new(PredFrmPtr, Init(FrmSort,PredFrmPtr(fFrm)^.PredNr,
                 StandarizeTermList(PredFrmPtr(fFrm)^.PredArgs)));
   ikFrmPrivPred:
    PositivelyStandarized:=
     new(LocPredFrmPtr, Init(LocPredFrmPtr(fFrm)^.PredNr,
                 StandarizeTermList(LocPredFrmPtr(fFrm)^.PredArgs),
                 CopyExpFrm(LocPredFrmPtr(fFrm)^.PredExp)));
   ikFrmNeg:
    PositivelyStandarized:=
     NewNegDis(NegativelyStandarized(NegFrmPtr(fFrm)^.NegArg));
   ikFrmFlexConj: PositivelyStandarized:=fFrm^.CopyFormula;
   ikFrmVerum: PositivelyStandarized:= NewVerum;
   else
    begin
{$IFDEF MDEBUG}
writeln(infofile,'PositivelyStandarized');
InfoChar(FrmSort);
{$ENDIF}
     RunTimeError(2043);
    end;
   end;
 end;

function NegativelyStandarized(fFrm:FrmPtr): FrmPtr;
 var lConjuncts: MCollection;
     lTrm: TrmPtr;
     lReconQualFrm: FrmPtr;
     z: integer;
begin
 with fFrm^ do
  case FrmSort of
   ikFrmConj:
    with ConjFrmPtr(fFrm)^.Conjuncts do
     begin lConjuncts.Init(Count,2);
      for z:=0 to Count-1 do
       begin
        lConjuncts.Insert(NegativelyStandarized(FrmPtr(Items^[z])))
       end;
      NegativelyStandarized:=new(ConjFrmPtr, Init(lConjuncts));
     end;
   ikFrmUniv: NegativelyStandarized:=fFrm^.CopyFormula;
   ikFrmQual:
    NegativelyStandarized:=
     new(QualFrmPtr,Init(StandarizeTerm(QualFrmPtr(fFrm)^.QualTrm),
                         StandarizeType(QualFrmPtr(fFrm)^.QualTyp)));
   ikFrmAttr:
     begin
      { Standaryzacja nie wplywa na ilosc argumentow }
      lTrm:=StandarizeTerm(LastArg(PredFrmPtr(fFrm)^.PredArgs));
{!!!
 fakt ze parametr jest "true" pwoduje, ze ChReconQualFrm nie potrafi go
 zamienic na "contradiction", chyba nalezy dac parametr "false"
 i zanegowac ????
 To zalezy od tego czy kwantyfikar wprowadzajacy podmiot jest
 pod negacja, czy nie, ale drazni brak symetrii
!!!}
      if lTrm^.TrmSort <> ikTrmEqConst then
       begin lReconQualFrm:=ChReconQualFrm(lTrm,PredFrmPtr(fFrm)^.PredNr,true,
                             CopyTermList1(PredFrmPtr(fFrm)^.PredArgs));
        NegativelyStandarized:=NegativelyStandarized(lReconQualFrm);
        dispose(lReconQualFrm,Done);
       end
      else NegativelyStandarized:=fFrm^.CopyFormula;
     end;
   ikFrmSchPred,ikFrmPred:
    NegativelyStandarized:=
     new(PredFrmPtr, Init(FrmSort,PredFrmPtr(fFrm)^.PredNr,
          StandarizeTermList(PredFrmPtr(fFrm)^.PredArgs)));
   ikFrmPrivPred:
    NegativelyStandarized:=
     new(LocPredFrmPtr, Init(LocPredFrmPtr(fFrm)^.PredNr,
                 StandarizeTermList(LocPredFrmPtr(fFrm)^.PredArgs),
                 CopyExpFrm(LocPredFrmPtr(fFrm)^.PredExp)));
   ikFrmNeg:
    NegativelyStandarized:=
     NewNegDis(PositivelyStandarized(NegFrmPtr(fFrm)^.NegArg));
   ikFrmFlexConj: NegativelyStandarized:=fFrm^.CopyFormula;
   ikFrmVerum: NegativelyStandarized:= NewVerum;
   else
    begin
{$IFDEF MDEBUG}
writeln(infofile,'PositivelyStandarized');
InfoChar(FrmSort);
{$ENDIF}
     RunTimeError(2043);
    end;
   end;
end;

type

 InstCollectionPtr = ^InstCollection;
 InstCollection =
  object(PreInstCollection)
   { konstruktory dla list podstawien }
    constructor UNITrm(FTrm1,FTrm2:TrmPtr);
    constructor UNIFraenkelTrm(FTrm1,FTrm2:TrmPtr);
    constructor UNITrmList(FTL1,FTL2:TrmList);
    constructor UNIAttr(aAttr1,aAttr2:AttrPtr);
    constructor UNIAttr1(aAttr1,aAttr2:AttrPtr);
    constructor UNIInclClusters(aClu1,aClu2:AttrCollectionPtr; aArgOrd:boolean);
    constructor UNITyp(FTyp1,FTyp2:TypPtr);
    constructor UNIRadices(fTyp1,fTyp2:TypPtr);
    constructor UNIEqClassTyps(fEqNr:integer; fTyp:TypPtr);
    constructor UNIFunc(FTrm1,FTrm2:TrmPtr);
    constructor UNIFrm(FFrm1,FFrm2:FrmPtr);
    constructor UniEqClassAndFreeVar(fEq,fVarNr:integer);
    constructor UniAttrFrm(fFrm:FrmPtr; fSign:boolean);
    constructor COMPInstAsFalse(FFrm:FrmPtr);
    constructor COMPInstAsTrue(FFrm:FrmPtr);

    procedure UnifyReflexive(fFrm:FrmPtr);

    constructor UnifyAttrs(aAttr1,aAttr2:AttrPtr);
    constructor UnifyTrmsWithConsts(aLeft,aRight:TrmPtr);
    constructor UnifyInclClusters(aClu1,aClu2: AttrCollectionPtr);
    constructor UnifyTypsWithConsts(aTyp1,aTyp2:TypPtr);
    constructor UnifySymmetricPredFrm(aFrm1,aFrm2:FrmPtr);
    constructor UnifyBasicFrm(aFrm1,aFrm2:FrmPtr);
    constructor UnifyTrmList(aTL1,aTL2:TrmList);

  end;


type

  PCollNrItem = ^TCollNrItem;
  TCollNrItem =
   object(TIntItem)
    Coll: InstCollection;
    constructor Init(fNr:integer; fColl:InstCollectionPtr);
    destructor Done; virtual;
   end;

destructor TCollNrItem.Done;
begin
 Coll.Done;
 TIntItem.Done;
end;

constructor TCollNrItem.Init(fNr:integer; fColl:InstCollectionPtr);
begin IntKey:=fNr;
 Coll.InitBottom; Coll:=fColl^;
end;

var
  UnifBase: integer;
  FreeVarUnifs: TIntKeyCollection;
  ExGenerators: MCollection;

constructor InstCollection.UNITrmList(FTL1,FTL2:TrmList);
  var lInsts1: InstCollection;
begin InitBottom;      { !!!! cos to dziwnie napisane }
 if FTL1=nil then
  begin
   if FTL2=nil then InitTop;
   exit
  end;
 UNITrm(FTL1^.XTrmPtr,FTL2^.XTrmPtr);
 if nOverflow then exit;
 FTL1 := FTL1^.NextTrm; FTL2 := FTL2^.NextTrm;
 while (FTL1 <> nil) and (FTL2 <> nil) do
  begin lInsts1.UNITrm(FTL1^.XTrmPtr,FTL2^.XTrmPtr);
   if lInsts1.nOverflow then
     begin Done;
      Count:=0;
      nOverflow:=true;
      exit
     end;
   JoinWith(@lInsts1);
   if Count = 0 then exit;
   FTL1 := FTL1^.NextTrm; FTL2 := FTL2^.NextTrm;
  end;
 if FTL1<>FTL2 then
  begin Done;
   Count:=0
  end;
end;

constructor InstCollection.UNIRadices(fTyp1,fTyp2:TypPtr);
 var ModNr1,ModNr2: integer; A1,A2: TrmList;
begin InitBottom;
 with FTyp1^ do
  if TypSort=fTyp2^.TypSort then
  begin
   case TypSort of
    ikTypMode:
     begin
      fTyp1^.AdjustTyp(ModNr1,A1); fTyp2^.AdjustTyp(ModNr2,A2);
     end;
    ikTypStruct:
      begin ModNr1:=fTyp1^.ModNr; ModNr2:=fTyp2^.ModNr;
       A1:=fTyp1^.ModArgs; A2:=fTyp2^.ModArgs;
      end;
    else RunTimeError(2029);
   end;
   if ModNr1=ModNr2 then
    UniTrmList(A1,A2);
  end;
end;

constructor InstCollection.UNIAttr(aAttr1,aAttr2:AttrPtr);
  var lAttrNr1,lAttrNr2: integer;
      lArgs1,lArgs2: TrmList;
begin InitBottom;
 aAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 aAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 if (lAttrNr1 = lAttrNr2) and (aAttr1^.fNeg = aAttr2^.fNeg) then
  UNITrmList(lArgs1,lArgs2);
end;

constructor InstCollection.UNIAttr1(aAttr1,aAttr2:AttrPtr);
  var lAttrNr1,lAttrNr2: integer;
      lArgs1,lArgs2: TrmList;
begin InitBottom;
 aAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 aAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 if (lAttrNr1 = lAttrNr2) and (aAttr1^.fNeg <> aAttr2^.fNeg) then
  UNITrmList(lArgs1,lArgs2);
end;

constructor InstCollection.UNIInclClusters(aClu1,aClu2:AttrCollectionPtr; aArgOrd:boolean);
 var i,j,k,lAttrNr: integer;
     lInstColl,lInstColl1,lInstColl2: InstCollection;
begin
 InitBottom;
 lInstColl.InitTop;
 j:=0;
 for i:=0 to aClu1^.Count-1 do
  begin lAttrNr:=AttrPtr(aClu1^.Items^[i])^.AdjustedAttrNr;
   while (j < aClu2^.Count) and (AttrPtr(aClu2^.Items^[j])^.AdjustedAttrNr < lAttrNr) do inc(j);
   if (j = aClu2^.Count) then exit;
   lInstColl1.InitBottom;
   k:=j;
   repeat
    if aArgOrd then
     lInstColl2.UniAttr(AttrPtr(aClu1^.Items^[i]),AttrPtr(aClu2^.Items^[k]))
    else lInstColl2.UniAttr(AttrPtr(aClu2^.Items^[k]),AttrPtr(aClu1^.Items^[i]));
    lInstColl1.UnionWith(@lInstColl2);
    inc(k);
   until (k>=aClu2^.Count) or (AttrPtr(aClu2^.Items^[k])^.AdjustedAttrNr > lAttrNr);
   lInstColl.JoinWith(@lInstColl1);
   if lInstColl.Count = 0 then exit;
  end;
 UnionWith(@lInstColl);
end;

constructor InstCollection.UNIEqClassTyps(fEqNr:integer; fTyp:TypPtr);
 var lInstColl: InstCollection;
     z: integer;
begin InitBottom;
 for z:=0 to EqClassType[fEqNr].Count-1 do
 with EqClassType[fEqNr] do
  begin
    lInstColl.UNIRadices(fTyp,TypPtr(Items^[z]));
    UnionWith(@lInstColl);
  end;
 lInstColl.UNIInclClusters(fTyp^.LowerCluster,EqClassSuperCluster[fEqNr],true);
 JoinWith(@lInstColl);
end;

constructor InstCollection.UNIFunc(FTrm1,FTrm2:TrmPtr);
var  FuncNr1,FuncNr2: integer;
     Args1,Args2: TrmList;
begin InitBottom;
 if FTrm2^.TrmSort=ikTrmFunctor then
  if FuncTrmPtr(FTrm2)^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr then
    UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,FuncTrmPtr(FTrm2)^.FuncArgs)
  else
    begin AdjustTrm(FTrm1,FuncNr1,Args1); AdjustTrm(FTrm2,FuncNr2,Args2);
      if FuncNr1=FuncNr2 then UNITrmList(Args1,Args2);
    end;
end;

constructor InstCollection.UNIFrm(FFrm1,FFrm2:FrmPtr);
 var PredNr1,PredNr2,i: integer;
     A1,A2,lTrmList,lTrmList1: TrmList;
     lInstColl:InstCollection;
begin InitBottom;
 if FFrm1^.FrmSort=FFrm2^.FrmSort then
  case FFrm1^.FrmSort of
   ikFrmVerum: InitTop;
   ikFrmNeg:
    UNIFrm(NegFrmPtr(FFrm1)^.NegArg,NegFrmPtr(FFrm2)^.NegArg);
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^.Conjuncts do
     if Count = ConjFrmPtr(fFrm2)^.Conjuncts.Count then
      begin InitTop;
       for i:=0 to Count-1 do
        begin
         lInstColl.UniFrm(FrmPtr(Items^[i]),FrmPtr(ConjFrmPtr(fFrm2)^.Conjuncts.Items^[i]));
         JoinWith(@lInstColl);
         if Self.Count = 0 then exit;
        end;
      end;
   ikFrmPred:
    begin
     AdjustFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustFrm(PredFrmPtr(fFrm2),PredNr2,A2);
{ ### To znowu procedura porownywania, i znowu nie mozna tutaj przetwarzac
  "connectedness" przez analogie.
}
     if PredNr1=PredNr2 then
      begin
       UniTrmList(A1,A2);
       if sySymmetry in ConstrPtr( Constr[ coPredicate].At(
                                  PredFrmPtr(fFrm1)^.PredNr))^.fProperties then
        with PredFrmPtr(fFrm1)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
       begin
        lTrmList:=SwitchArgs(fFirstArg,fSecondArg,PredArgs);
        lTrmList1:=AdjustTrmList(ikFrmPred,PredNr,lTrmList);
        lInstColl.UniTrmList(lTrmList1,A2);
        DisposeTrmList(lTrmList);
        UnionWith(@lInstColl);
       end
       else
        if sySymmetry in ConstrPtr( Constr[ coPredicate].At(
                                   PredFrmPtr(fFrm2)^.PredNr))^.fProperties then
         with PredFrmPtr(fFrm2)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
        begin
         lTrmList:=SwitchArgs(fFirstArg,fSecondArg,PredArgs);
         lTrmList1:=AdjustTrmList(ikFrmPred,PredNr,lTrmList);
         lInstColl.UniTrmList(A1,lTrmList1);
         DisposeTrmList(lTrmList);
         UnionWith(@lInstColl);
        end
      end;
    end;
   ikFrmSchPred,ikFrmPrivPred:
    if PredFrmPtr(FFrm1)^.PredNr=PredFrmPtr(FFrm2)^.PredNr then
     UNITrmList(PredFrmPtr(FFrm1)^.PredArgs,PredFrmPtr(FFrm2)^.PredArgs);
   ikFrmAttr:
    begin
     AdjustAttrFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustAttrFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
       UniTrmList(A1,A2);
    end;
   ikFrmUniv:
    begin
     UNITyp(UnivFrmPtr(FFrm1)^.Quantified,UnivFrmPtr(FFrm2)^.Quantified);
     if Count<>0 then
     begin
       inc(BoundVarNbr);
       BoundVar[BoundVarNbr]:=UnivFrmPtr(FFrm2)^.Quantified;
       lInstColl.UNIFrm(UnivFrmPtr(FFrm1)^.Scope,UnivFrmPtr(FFrm2)^.Scope);
       JoinWith(@lInstColl);
       dec(BoundVarNbr);
      end;
    end;
   ikFrmQual:
    begin UniTrm(QualFrmPtr(FFrm1)^.QualTrm,QualFrmPtr(fFrm2)^.QualTrm);
     if Count<>0 then
      begin
       lInstColl.UniTyp(QualFrmPtr(FFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
       JoinWith(@lInstColl);
      end;
    end;
   ikFrmFlexConj:
    begin
     UNIFrm(FlexFrmPtr(FFrm1)^.nLeftOrigFrm,FlexFrmPtr(FFrm2)^.nLeftOrigFrm);
     if Count<>0 then
      begin
       lInstColl.UNIFrm(FlexFrmPtr(FFrm1)^.nRightOrigFrm,FlexFrmPtr(FFrm2)^.nRightOrigFrm);
       JoinWith(@lInstColl);
      end;
    end;
   else RunTimeError(2034);
  end;
end;

constructor InstCollection.UniEqClassAndFreeVar(fEq,fVarNr:integer);
 var lPlace,lKey,z: integer;
     lInst: NatFunc;
 label Completed;
begin lKey:=fVarNr * MaxTrmNbr + fEq;
 if FreeVarUnifs.Search(@lKey,lPlace) then
  begin
   Self:=PCollNrItem(FreeVarUnifs.Items^[lPlace])^.Coll;
   exit
  end;
 UNIEqClassTyps(fEq,FreeVarType.Items^[fVarNr-1]);
 if Count = 0 then goto Completed;
 if Status = Top then
  begin
   lInst.InitNatFunc(2,4);
   lInst.Assign(fVarNr-1,fEq);
   InitSingle(NatFuncPtr(lInst.CopyObject));
   goto Completed;
  end;
 if Status = Generator then ReNew;
 for z:=0 to Count-1 do NatFuncPtr(Items^[z])^.Assign(fVarNr-1,fEq);
Completed:
 Status:=Generator;
 FreeVarUnifs.AtInsert(lPlace,new(PCollNrItem, Init(lKey,@Self)));
end;

constructor InstCollection.UNIFraenkelTrm(fTrm1,fTrm2:TrmPtr);
 var lBoundVarNbr,i:integer;
     lInstColl:InstCollection;
begin mizassert(2599,fTrm2^.TrmSort = ikTrmFraenkel);
 InitBottom;
 with FraenkelTrmPtr(fTrm1)^ do
  if LambdaArgs.Count = FraenkelTrmPtr(fTrm2)^.LambdaArgs.Count then
   begin lBoundVarNbr:=BoundVarNbr; InitTop;
    for i:=0 to LambdaArgs.Count-1 do
     begin
      inc(BoundVarNbr);
      lInstColl.UniTyp(LambdaArgs.Items^[i],FraenkelTrmPtr(fTrm2)^.LambdaArgs.Items^[i]);
      JoinWith(@lInstColl);
     end;
    if Count <> 0 then
     begin
      lInstColl.UniTrm(LambdaScope,FraenkelTrmPtr(fTrm2)^.LambdaScope);
      JoinWith(@lInstColl);
     end;
    if Count <> 0 then
     begin
      lInstColl.UniFrm(Compr,FraenkelTrmPtr(fTrm2)^.Compr);
      JoinWith(@lInstColl);
     end;
    BoundVarNbr:=lBoundVarNbr;
   end;
end;

constructor InstCollection.UNITrm(FTrm1,FTrm2:TrmPtr);
var lTL,lUnifBase,lEq: integer;
    lInstColl: InstCollection;
begin InitBottom;
 case FTrm1^.TrmSort of
  ikTrmFreeVar:
    begin lEq:=EqClassNr(Ftrm2);
     if lEq > 0 then UniEqClassAndFreeVar(lEq,VarTrmPtr(FTrm1)^.VarNr);
    end;
  ikTrmBound:
   if FTrm2^.TrmSort=ikTrmBound then
    if (VarTrmPtr(FTrm1)^.VarNr-UnifBase)=VarTrmPtr(FTrm2)^.VarNr then
     InitTop;
  ikTrmAggreg:
   begin
    if (FTrm2^.TrmSort=ikTrmAggreg) and (FuncTrmPtr(FTrm2)^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr)
     then UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,FuncTrmPtr(FTrm2)^.FuncArgs);
    for lTL:=0 to EqList[expTrmAggreg][EqClassNr(Ftrm2)]^.Count-1 do
     begin
      if FuncTrmPtr(EqList[expTrmAggreg][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr then
       begin lInstColl.UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,
                                  FuncTrmPtr(EqList[expTrmAggreg][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncArgs);
        UnionWith(@lInstColl);
       end;
     end;
   end;
  ikTrmSchFunc:
   begin
    if (FTrm2^.TrmSort=FTrm1^.TrmSort) and (FuncTrmPtr(FTrm2)^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr)
     then UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,FuncTrmPtr(FTrm2)^.FuncArgs);
    for lTL:=0 to EqList[expTrmSchFunc][EqClassNr(Ftrm2)]^.Count-1 do
     begin
      if FuncTrmPtr(EqList[expTrmSchFunc][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr then
       begin lInstColl.UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,
                             FuncTrmPtr(EqList[expTrmSchFunc][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncArgs);
        UnionWith(@lInstColl);
       end;
     end;
   end;
  ikTrmPrivFunc:
   begin
    if (FTrm2^.TrmSort=FTrm1^.TrmSort) and (FuncTrmPtr(FTrm2)^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr)
     then UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,FuncTrmPtr(FTrm2)^.FuncArgs);
    for lTL:=0 to EqList[expTrmPrivFunc][EqClassNr(Ftrm2)]^.Count-1 do
     begin
      if FuncTrmPtr(EqList[expTrmPrivFunc][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr then
       begin lInstColl.UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,
                             FuncTrmPtr(EqList[expTrmPrivFunc][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncArgs);
        UnionWith(@lInstColl);
       end;
     end;
   end;
  ikTrmFunctor:
   begin
    UNIFunc(FTrm1,FTrm2);
    for lTL:=0 to EqList[expTrmFunctor][EqClassNr(Ftrm2)]^.Count-1 do
     begin lInstColl.UNIFunc(FTrm1,EqList[expTrmFunctor][EqClassNr(Ftrm2)]^.Items^[LTL]);
      UnionWith(@lInstColl);
     end;
   end;
  ikTrmSelector:
   begin
     if (FTrm2^.TrmSort=FTrm1^.TrmSort) and (FuncTrmPtr(FTrm2)^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr)
      then UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,FuncTrmPtr(FTrm2)^.FuncArgs);
     for lTL:=0 to EqList[expTrmSelector][EqClassNr(Ftrm2)]^.Count-1 do
      begin
       if FuncTrmPtr(EqList[expTrmSelector][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncNr=FuncTrmPtr(FTrm1)^.FuncNr then
        begin lInstColl.UNITrmList(FuncTrmPtr(FTrm1)^.FuncArgs,
                            FuncTrmPtr(EqList[expTrmSelector][EqClassNr(Ftrm2)]^.Items^[LTL])^.FuncArgs);
        UnionWith(@lInstColl);
        end;
      end;
   end;
  ikTrmFraenkel:
   begin
    if fTrm2^.TrmSort = ikTrmFraenkel then UniFraenkelTrm(fTrm1,fTrm2);
    for lTL:=0 to EqList[expTrmFraenkel][EqClassNr(Ftrm2)]^.Count-1 do
      begin lUnifBase:=UnifBase; UnifBase:=BoundVarNbr;
       lInstColl.UniFraenkelTrm(fTrm1,EqList[expTrmFraenkel][EqClassNr(Ftrm2)]^.Items^[LTL]);
       UnionWith(@lInstColl);
       UnifBase:=lUnifBase;
      end;
   end;
  ikTrmChoice:
   begin
    if fTrm2^.TrmSort = ikTrmChoice then
     UniTyp(ChoiceTrmPtr(fTrm1)^.ChoiceTyp,ChoiceTrmPtr(fTrm2)^.ChoiceTyp);
    for lTL:=0 to EqList[expTrmChoice][EqClassNr(Ftrm2)]^.Count-1 do
     begin
      lInstColl.UniTyp(ChoiceTrmPtr(fTrm1)^.ChoiceTyp,
                       ChoiceTrmPtr(EqList[expTrmChoice][EqClassNr(Ftrm2)]^.Items^[LTL])^.ChoiceTyp);
      UnionWith(@lInstColl);
     end;
   end;
  ikTrmEqConst: if VarTrmPtr(FTrm1)^.VarNr = EqClassNr(Ftrm2) then InitTop;
  ikTrmNumeral,ikTrmConstant,ikTrmInfConst: if EqClassNr(Ftrm1)=EqClassNr(Ftrm2) then InitTop;
  else RunTimeError(2035);
 end;
end;

constructor InstCollection.UNITyp(FTyp1,FTyp2:TypPtr);
var lInstColl,lInstColl1: InstCollection;
begin InitBottom;
 if fTyp1^.TypSort=fTyp2^.TypSort then
 begin
  lInstColl.UNIInclClusters(fTyp1^.LowerCluster,fTyp2^.UpperCluster,true);
  if lInstColl.Count<>0 then
   begin
    lInstColl1.UNIInclClusters(fTyp2^.LowerCluster,fTyp1^.UpperCluster,false);
    lInstColl.JoinWith(@lInstColl1);
    if lInstColl.Count<>0 then
     begin
      lInstColl1.UniRadices(FTyp1,FTyp2);
      lInstColl.JoinWith(@lInstColl1);
      UnionWith(@lInstColl);
     end;
   end;
 end;
end;

constructor InstCollection.UniAttrFrm(fFrm:FrmPtr; fSign:boolean);
 var lLeft:TrmPtr;
     lAttrArgs: TrmList;
     j:integer;
     lInstColl: InstCollection;
begin
 InitBottom;
 with PredFrmPtr(fFrm)^ do
  begin
   lLeft:=LastArg(PredArgs);
   MizAssert(2551,lLeft^.TrmSort=ikTrmEqConst);
   lAttrArgs:=CopyTermList1(PredArgs);
   lInstColl.InitBottom;
   with EqClassSuperCluster[VarTrmPtr(lLeft)^.VarNr]^ do
    for j:=0 to Count-1 do
     with AttrPtr(Items^[j])^ do
      if (fAttrNr = PredNr) and (fNeg = ord(fSign)) then
       begin
        lInstColl.UniTrmList(lAttrArgs,fAttrArgs);
        UnionWith(@lInstColl);
       end;
  end;
 DisposeTrmList(lAttrArgs);
end;

procedure InstCollection.UnifyReflexive(fFrm:FrmPtr);
 var k:integer;
     lInstColl,lInstColl1: InstCollection;
     lLeft,lRight:TrmPtr;
begin
 with PredFrmPtr(fFrm)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
 begin
  GetArgs2(fFirstArg,fSecondArg,lLeft,lRight,PredArgs);
//  InitBottom;
  for K:=1 to EqClassNbr do
   begin
    lInstColl.UNITrm(lLeft,addr(ETrm[K]));
    if lInstColl.Count<>0 then
    begin
     lInstColl1.UNITrm(lRight,addr(ETrm[K]));
     lInstColl.JoinWith(@lInstColl1);
     UnionWith(@lInstColl);
    end;
   end;
  DisposeTrm(lLeft); DisposeTrm(lRight);
 end;
end;

constructor InstCollection.UnifyTrmsWithConsts(aLeft,aRight:TrmPtr);
 var K: integer;
     lInstColl,lInstColl1: InstCollection;
begin
  InitBottom;
  for K:=1 to EqClassNbr do
   begin
    lInstColl.UNITrm(aLeft,addr(ETrm[K]));
    if lInstColl.nOverflow then exit;
    if lInstColl.Count<>0 then
    begin
     lInstColl1.UNITrm(aRight,addr(ETrm[K]));
     if lInstColl1.nOverflow then
      begin Done;
       Count:=0;
       nOverflow:=true;
       exit
      end;
     lInstColl.JoinWith(@lInstColl1);
     if lInstColl.nOverflow then
      begin Done;
       Count:=0;
       nOverflow:=true;
       exit
      end;
     UnionWith(@lInstColl);
     if nOverflow then exit;
    end;
   end;
end;

constructor InstCollection.UnifyTrmList(aTL1,aTL2:TrmList);
  var lUnifColl: ULCollection;
      lInsts: InstCollectionPtr;
begin InitBottom;
 if (aTL1=nil) or (aTL2=nil) then
  begin
   if aTL1=aTL2 then InitTop;
   exit
  end;
 lUnifColl.Init(0,8);
 while (aTL1 <> nil) and (aTL2 <> nil) do
  begin
   lInsts:=new(InstCollectionPtr,UnifyTrmsWithConsts(aTL1^.XTrmPtr,aTL2^.XTrmPtr));
   if lInsts^.nOverflow then
    begin lUnifColl.Done;
     dispose(lInsts,Done);
     Done;
     Count:=0;
     nOverflow:=true;
     exit
    end;
   if lInsts^.Count = 0 then
    begin lUnifColl.Done;
     dispose(lInsts,Done);
     exit;
    end;
   if lInsts^.Status = Top then dispose(lInsts,Done)
   else lUnifColl.Insert(lInsts);
   aTL1 := aTL1^.NextTrm; aTL2 := aTL2^.NextTrm;
  end;
 JoinInstList(lUnifColl);
 if aTL1<>aTL2 then
  begin Done;
   Count:=0
  end;
end;

constructor InstCollection.UnifyAttrs(aAttr1,aAttr2: AttrPtr);
  var lAttrNr1,lAttrNr2: integer;
      lArgs1,lArgs2: TrmList;
begin InitBottom;
 aAttr1^.AdjustAttr(lAttrNr1,lArgs1);
 aAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 if (lAttrNr1 = lAttrNr2) and (aAttr1^.fNeg = aAttr2^.fNeg) then
  UnifyTrmList(lArgs1,lArgs2);
end;

constructor InstCollection.UnifyInclClusters(aClu1,aClu2:AttrCollectionPtr);
 var i,j,lAttrNr: integer;
     lInsts: InstCollection;
begin InitBottom;
 if aClu2^.Count > aClu1^.Count then
  exit;
 if aClu1^.Count = 0 then
  begin InitTop;
   exit
  end;
 j:=0;
 lAttrNr:=AttrPtr(aClu1^.Items^[0])^.AdjustedAttrNr;
 while (j < aClu2^.Count) and (AttrPtr(aClu2^.Items^[j])^.AdjustedAttrNr < lAttrNr)
  do inc(j);
 if j = aClu2^.Count then
  exit;
 UnifyAttrs(aClu1^.Items^[0],aClu2^.Items^[j]);
 if Count = 0 then
  exit;
 for i:=1 to aClu1^.Count-1 do
  begin
   lAttrNr:=AttrPtr(aClu1^.Items^[i])^.AdjustedAttrNr;
   while (j < aClu2^.Count) and (AttrPtr(aClu2^.Items^[j])^.AdjustedAttrNr < lAttrNr)
    do inc(j);
   if j = aClu2^.Count then
    exit;
   lInsts.UnifyAttrs(aClu1^.Items^[i],aClu2^.Items^[j]);
   JoinWith(@lInsts);
   if Count = 0 then
    exit;
  end;
end;

constructor InstCollection.UnifyTypsWithConsts(aTyp1,aTyp2:TypPtr);
var lInstColl,lInstColl1,lInstColl2: InstCollection;
    ModNr1,ModNr2: integer;
    A1,A2: TrmList;
begin InitBottom;
 if aTyp1^.TypSort=aTyp2^.TypSort then
 begin
  lInstColl.UnifyInclClusters(aTyp1^.LowerCluster,aTyp2^.UpperCluster);
  if lInstColl.Count<>0 then
   begin
    lInstColl1.UnifyInclClusters(aTyp2^.LowerCluster,aTyp1^.UpperCluster);
    lInstColl.JoinWith(@lInstColl1);
    if lInstColl.Count<>0 then
     if aTyp1^.TypSort = aTyp2^.TypSort then
      begin
       case aTyp1^.TypSort of
       ikTypMode:
        begin
         aTyp1^.AdjustTyp(ModNr1,A1);
         aTyp2^.AdjustTyp(ModNr2,A2);
        end;
       ikTypStruct:
        begin ModNr1:=aTyp1^.ModNr;
         ModNr2:=aTyp2^.ModNr;
         A1:=aTyp1^.ModArgs;
         A2:=aTyp2^.ModArgs;
        end;
       else RunTimeError(2629);
       end;
       if ModNr1=ModNr2 then
        begin
         lInstColl2.UnifyTrmList(A1,A2);
         lInstColl.JoinWith(@lInstColl2);
        end; 
      end;
    UnionWith(@lInstColl);
   end;
 end;
end;

constructor InstCollection.UnifySymmetricPredFrm(aFrm1,aFrm2:FrmPtr);
 var lTrmList,lTrmList1,lTrmList2: TrmList;
begin InitBottom;
 mizassert(3891,(aFrm1^.FrmSort = aFrm2^.FrmSort) and
          (aFrm1^.FrmSort = ikFrmPred));
  if sySymmetry in ConstrPtr( Constr[ coPredicate].At(
                            PredFrmPtr(aFrm1)^.PredNr))^.fProperties then
   begin
    with PredFrmPtr(aFrm1)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
     begin
      lTrmList:=SwitchArgs(fFirstArg,fSecondArg,PredArgs);
      lTrmList1:=AdjustTrmList(ikFrmPred,PredNr,lTrmList);
     end;
    with PredFrmPtr(aFrm1)^ do
      lTrmList2:=AdjustTrmList(ikFrmPred,PredNr,PredArgs);
    UnifyTrmList(lTrmList1,lTrmList2);
    DisposeTrmList(lTrmList);
   end
end;

constructor InstCollection.UnifyBasicFrm(aFrm1,aFrm2:FrmPtr);
 var PredNr1,PredNr2: integer;
     A1,A2: TrmList;
     lInstCol: InstCollection;
begin InitBottom;
//writeln(infofile,'UnifyBasicFrm(');
//infoformula(aFrm1); infonewline;
//infoformula(aFrm2); infonewline;
 if aFrm1^.FrmSort = aFrm2^.FrmSort then
  case aFrm1^.FrmSort of
   ikFrmPred:
    begin
     AdjustFrm(PredFrmPtr(aFrm1),PredNr1,A1);
     AdjustFrm(PredFrmPtr(aFrm2),PredNr2,A2);
     if PredNr1 = PredNr2 then
      begin
       UnifyTrmList(A1,A2);
{
       lInstCol.UnifySymmetricPredFrm(aFrm1,aFrm2);
       UnionWith(@lInstCol);
       lInstCol.UnifySymmetricPredFrm(aFrm2,aFrm1);
       UnionWith(@lInstCol);
}
      end;
    end;
   ikFrmSchPred,ikFrmPrivPred:
    if PredFrmPtr(aFrm1)^.PredNr=PredFrmPtr(aFrm2)^.PredNr then
     UnifyTrmList(PredFrmPtr(aFrm1)^.PredArgs,PredFrmPtr(aFrm2)^.PredArgs);
   ikFrmAttr:
    begin
     AdjustAttrFrm(PredFrmPtr(aFrm1),PredNr1,A1);
     AdjustAttrFrm(PredFrmPtr(aFrm2),PredNr2,A2);
     if PredNr1 = PredNr2 then
      UnifyTrmList(A1,A2);
    end;
   ikFrmUniv:
    begin
    end;
   ikFrmQual:
    begin
     UnifyTrmsWithConsts(QualFrmPtr(aFrm1)^.QualTrm,QualFrmPtr(aFrm2)^.QualTrm);
     if Count<>0 then
      begin
       lInstCol.UnifyTypsWithConsts(QualFrmPtr(aFrm1)^.QualTyp,QualFrmPtr(aFrm2)^.QualTyp);
       JoinWith(@lInstCol);
      end;
    end;
   else RunTimeError(2734);
  end;
//if nOverflow then
//begin
//writeln(infofile,'UnifyBasicFrm(');
//infoformula(aFrm1); infonewline;
//infoformula(aFrm2); infonewline;
//writeln(infofile,'**Overflow**');
//end;
end;

constructor InstCollection.COMPInstAsTrue(FFrm:FrmPtr);
 var K,i,m,j,x,z,v,lPred,lMode,ModNr1,PredNr2: integer;
     lLeft,lRight,lTrm: TrmPtr;
     A1,A2,A,lTrmList,lTrmList1: TrmList;
     lAttrTyp,lQualTyp,lTypPtr1,lTypPtr2: TypPtr;
     llColl,lInstColl,lInstColl1,lInstColl2: InstCollection;
     lPositive,lNegative,lZero,rPositive,rNegative,rZero,lAttr,lAttr1: AttrPtr;
     lEqOcc: boolean;
 label 1;
begin
 MizAssert(2358, (FFrm^.FrmSort <> ikFrmNeg) and
                (FFrm^.FrmSort <> ikFrmConj) and
                (FFrm^.FrmSort <> ikFrmVerum));
 InitBottom;
 case FFrm^.FrmSort of
 ikFrmPred:
   begin
    AdjustFrm(PredFrmPtr(fFrm),lPred,A);
    with PredFrmPtr(fFrm)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
     begin
      if syIrreflexivity in fProperties then
        UnifyReflexive(fFrm);
     end;
    if lPred = gBuiltIn[rqBelongsTo] then
      begin
       GetBinArgs(fFrm,lLeft,lRight);
       for K:=1 to EqClassNbr do
        begin
         lAttr:=EqClassSuperCluster[k]^.GetAttr(gBuiltIn[rqEmpty],nil);
         if (lAttr<>nil) and (lAttr^.fNeg=ord(true)) then
         begin
          lInstColl.UNITrm(lRight,addr(ETrm[K]));
          UnionWith(@lInstColl);
         end;
        end;
       for K := 0 to NegBas.Count-1 do
        with QualFrmPtr(NegBas.Items^[K])^ do
         if (FrmSort = ikFrmQual) and (QualTyp^.TypSort = ikTypMode) then
          begin QualTyp^.AdjustTyp(lMode,A);
           if lMode = gBuiltIn[rqElement] then
            begin lInstColl.UNITrm(lLeft,QualTrm);
             if lInstColl.Count <> 0 then
              begin lInstColl1.UNITrm(lRight,A^.XTrmPtr);
               lInstColl.JoinWith(@lInstColl1);
               UnionWith(@lInstColl);
              end;
            end;
          end;
      end
    else if (lPred = gBuiltIn[rqInclusion]) and
            (gBuiltIn[rqPowerSet] > 0 ) then
      begin
       GetBinArgs(fFrm,lLeft,lRight);
       for K := 0 to NegBas.Count-1 do
       with QualFrmPtr(NegBas.Items^[K])^ do
        if (FrmSort = ikFrmQual) and (QualTyp^.TypSort = ikTypMode) then
         begin QualTyp^.AdjustTyp(lMode,A);
          if lMode = gBuiltIn[rqElement] then
           begin lInstColl.UNITrm(lLeft,QualTrm);
            if lInstColl.Count <> 0 then
             begin
              case lRight^.TrmSort of
               ikTrmEqConst:
                begin
                 lTrm:=NewFuncTrm(gBuiltIn[rqPowerSet],NewTrmList(lRight,nil));
                 if EqClassNr(lTrm) = VarTrmPtr(A^.XTrmPtr)^.VarNr then
                   UnionWith(@lInstColl);
                 DisposeTrm(lTrm);
                end;
               else
                begin
                 lTrm:=NewFuncTrm(gBuiltIn[rqPowerSet],NewTrmList(CopyTerm(lRight),nil));
                 lInstColl1.UNITrm(lTrm,A^.XtrmPtr);
                 DisposeTrm(lTrm);
                 lInstColl.JoinWith(@lInstColl1);
                 UnionWith(@lInstColl);
                end;
              end;
             end;
           end;
         end
      end
     else if lPred = gBuiltIn[rqLessOrEqual] then
      begin
{ Sprawdzamy z klasami, ktore maja okreslony NatVal , dobierajac tak aby
nie zachodzila nierownosc. To jest wariant negatywny.
}
       GetBinArgs(fFrm,lLeft,lRight);
       for k:=1 to EqClassNbr do
        with EqClassVal[k] do
         if Determined then
          begin
           lInstColl.UNITrm(lLeft,addr(ETrm[K]));
           if lInstColl.Count <> 0 then
            begin lInstColl1.InitBottom;
             for i:=1 to EqClassNbr do
              if EqClassVal[i].Determined then
               if IsRationalGT(EqClassVal[i].NumericValue,EqClassVal[k].NumericValue) then
                begin
{$IFDEF CH_REPORT}
                 CHReport.Out_NegNumReq2(
                  rqLessOrEqual,EqClassVal[k].NumericValue,
                  EqClassVal[i].NumericValue);
{$ENDIF}
                 lInstColl2.UNITrm(lRight,addr(ETrm[i]));
                 lInstColl1.UnionWith(@lInstColl2);
                end;
             lInstColl.JoinWith(@lInstColl1);
             UnionWith(@lInstColl);
            end;
          end;
       for k:=1 to EqClassNbr do
        with EqClassVal[k] do
          begin
           lInstColl.UNITrm(lLeft,addr(ETrm[K]));
           if lInstColl.Count <> 0 then
            begin lInstColl1.InitBottom;
             for i:=1 to EqClassNbr do
              begin
               lPositive:=EqClassSuperCluster[k]^.GetAttr(gBuiltIn[rqPositive],nil);
               lNegative:=EqClassSuperCluster[k]^.GetAttr(gBuiltIn[rqNegative],nil);
               rPositive:=EqClassSuperCluster[i]^.GetAttr(gBuiltIn[rqPositive],nil);
               rNegative:=EqClassSuperCluster[i]^.GetAttr(gBuiltIn[rqNegative],nil);
               lEqOcc:=false;
               if (lPositive<>nil) and (lPositive^.fNeg=ord(true)) and
                  (rPositive<>nil) and (rPositive^.fNeg=ord(false))
                 then lEqOcc:=true;
               if (rNegative<>nil) and (rNegative^.fNeg=ord(true)) and
                  (lNegative<>nil) and (lNegative^.fNeg=ord(false))
                 then lEqOcc:=true;
               if lEqOcc then
                begin
                 lInstColl2.UNITrm(lRight,addr(ETrm[i]));
                 lInstColl1.UnionWith(@lInstColl2);
                end;
              end;
             lInstColl.JoinWith(@lInstColl1);
             UnionWith(@lInstColl);
            end;
          end;
      end
     else if lPred = gBuiltIn[rqEqualsTo] then
      begin
       GetBinArgs(fFrm,lLeft,lRight);
       for k:=1 to EqClassNbr do
        if EqClassVal[k].Determined then
         begin
          lInstColl.UNITrm(lLeft,addr(ETrm[K]));
          if lInstColl.Count <> 0 then
           begin lInstColl1.InitBottom;
            for i:=1 to EqClassNbr do
             if (i <> k) and (EqClassVal[i].Determined) then
               begin
                MizAssert(2596,not AreEqComplex(EqClassVal[k].NumericValue,EqClassVal[i].NumericValue));
                lInstColl2.UNITrm(lRight,addr(ETrm[i]));
                lInstColl1.UnionWith(@lInstColl2);
               end;
            lInstColl.JoinWith(@lInstColl1);
            UnionWith(@lInstColl);
           end;
         end;
       end;
   end;
  ikFrmAttr:
   begin UniAttrFrm(fFrm,false);
    goto 1
   end;
  ikFrmQual:
   begin
    for K := 0 to NegBas.Count-1 do
    with QualFrmPtr(NegBas.Items^[K])^ do
     if FrmSort = ikFrmQual then
      begin lInstColl.UNITrm(QualFrmPtr(fFrm)^.QualTrm,QualTrm);
       if lInstColl.Count<>0 then
        begin lInstColl1.InitBottom;
         case QualTyp^.TypSort of
          ikTypMode:
{ Poniewaz QualTyp nie moze byc Any ("x is not Any" powinno bylo wczesniej
doprowadzic do sprzecznosci, to "QualFrmPtr(fFrm)^.QualTyp" nie moze, zeby
cokolwiek robic, byc strukturowy. "lTypPtr1" trzeba rozszerzac tak dlugo,
az dojdziemy do typu strukturowego lub typu pozniejszego niz "ModNr1".
pozniej.
}
           if QualFrmPtr(fFrm)^.QualTyp^.TypSort = ikTypMode then
            begin lTypPtr1:=QualFrmPtr(fFrm)^.QualTyp^.CopyType;
             QualTyp^.AdjustTyp(ModNr1,A1);
             mizassert(2600,ModNr1<>1);
             while (lTypPtr1^.TypSort = ikTypMode) and (lTypPtr1^.ModNr >= ModNr1) do
              begin lInstColl2.UNIRadices(lTypPtr1,QualTyp);
               lInstColl1.UnionWith(@lInstColl2);
               lTypPtr2:=lTypPtr1^.Widening;
               dispose(lTypPtr1,Done); lTypPtr1:=lTypPtr2;
              end;
             dispose(lTypPtr1,Done);
            end;
          ikTypStruct:
           begin
            lTypPtr1:=QualFrmPtr(fFrm)^.QualTyp^.CopyType;
            lTypPtr1:=QualTyp^.WideningOf(lTypPtr1);
            if lTypPtr1 <> nil then
             begin lInstColl1.UNIRadices(lTypPtr1,QualTyp);
              dispose(lTypPtr1,Done);
             end;
           end;
          else RunTimeError(2031);
         end;
         lInstColl.JoinWith(@lInstColl1); UnionWith(@lInstColl);
        end;
      end;
{ Teraz mozna poszukac atrybutow.
???????????????????  ale zostala zrobiona standaryzacja !!!
Do obalenie mozna uzyc UpperCluster, bo to sa wszystkie atrybuty,
ktore mozna miec. Problem polega na tym, ze jezeli atrybut ma
hidden arguments, to trzeba moc je odtworzyc, tzn. unifikacja
musi sie zaczac od typu !
}
    for k:=1 to EqClassNbr do
     begin
      lInstColl.UNITrm(QualFrmPtr(fFrm)^.QualTrm,addr(ETrm[k]));
      if lInstColl.Count<>0 then
       begin
        lInstColl1.InitBottom;
        lQualTyp:=QualFrmPtr(fFrm)^.QualTyp;
        with lQualTyp^.UpperCluster^ do
        for x:=0 to Count-1 do
         for v:=0 to EqClassSuperCluster[k]^.count-1 do
          begin
           llColl.UNIAttr1(AttrPtr(Items^[x]),AttrPtr(EqClassSuperCluster[k]^.Items^[v]));
           lInstColl1.UnionWith(@llColl);
          end;
        lInstColl.JoinWith(@lInstColl1);
        UnionWith(@lInstColl);
       end;
     end;
    goto 1;
   end;
 end;
 for K := 0 to NegBas.Count-1 do
  begin
   lInstColl.UNIFrm(fFrm,NegBas.Items^[K]);
   UnionWith(@lInstColl);
  end;
1:
 for k:=FreeVarUnifs.Count-1 downto 0 do
  if PCollNrItem(FreeVarUNifs.Items^[k])^.IntKey div MaxTrmNbr > FreeVarType.Count then
   begin
    ExGenerators.Insert((PCollNrItem(FreeVarUnifs.Items^[k])^.Coll.CopyObject));
    with PCollNrItem(FreeVarUnifs.Items^[k])^.Coll do
     begin Count:=0;
      Limit:=0;
      Items:=nil
     end;
    dispose(PCollNrItem(FreeVarUnifs.Items^[k]),Done);
    FreeVarUnifs.AtDelete(k);
   end;
end;

constructor InstCollection.COMPInstAsFalse(FFrm:FrmPtr);
 var lPred,i,j,K,lCount,PredNr2,lEqNr,z: integer;
     lInsts,lInsts1: InstCollectionPtr;
     lLeft,lRight,lTrm: TrmPtr; A,A2,lTrmList,lTrmList1: TrmList;
     lTyp: TypPtr;
     lUnifColl,lUnifColl1: ULCollection;
     lInstColl,lInstColl1,lInstColl2: InstCollection;
     lPositive,lNegative,lZero,rPositive,rNegative,rZero,lAttr: AttrPtr;
     lEqOcc: boolean;
     lObj: NatFuncPtr;
     lList: InstCollection;
 label 11,12,55;
begin
 MizAssert(2359, (FFrm^.FrmSort <> ikFrmNeg) and
                 (FFrm^.FrmSort <> ikFrmConj) and
                 (FFrm^.FrmSort <> ikFrmVerum));
 InitBottom;
 with FFrm^ do
 case FrmSort of
  ikFrmPred:
   begin
    AdjustFrm(PredFrmPtr(FFrm),lPred,A);
    with PredFrmPtr(fFrm)^,ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
     begin
      if syReflexivity in fProperties then
        UnifyReflexive(fFrm);
     end;
    if (lPred = gBuiltIn[rqBelongsTo]) and
       (gBuiltIn[rqElement] > 0 ) then
      begin
       GetBinArgs(fFrm,lLeft,lRight);
       for K:=1 to EqClassNbr do
       begin
        lAttr:=EqClassSuperCluster[k]^.GetAttr(gBuiltIn[rqEmpty],nil);
        if (lAttr<>nil) and (lAttr^.fNeg=ord(false)) then
        begin
         lInstColl.UNITrm(lRight,addr(ETrm[K]));
         if lInstColl.Count <> 0 then
          begin
           lInstColl1.InitBottom;
           lTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                gBuiltIn[rqElement],
                                NewTrmList(addr(ETrm[K]),nil));
           for i:=1 to EqClassNbr do
            begin
             with EqClassType[i] do
              for j:=0 to Count-1 do
               if TypPtr(Items^[j])^.EqRadices(lTyp) then goto 11;
             continue;
11:
             lInstColl2.UniTrm(lLeft,addr(ETrm[i]));
             lInstColl1.UnionWith(@lInstColl2);
            end;
           dispose(lTyp,Done);
           lInstColl.JoinWith(@lInstColl1);
           UnionWith(@lInstColl);
          end;
        end;
       end;
       goto 55;
      end
     else if (lPred = gBuiltIn[rqInclusion]) and
            (gBuiltIn[rqElement] > 0) and (gBuiltIn[rqPowerSet] > 0) then
      begin
       GetBinArgs(fFrm,lLeft,lRight);
       for K:=1 to EqClassNbr do
        begin
         lInstColl.UNITrm(lRight,addr(ETrm[K]));
         if lInstColl.Count <> 0 then
          begin
           lInstColl1.InitBottom;
           lTrm:=NewFuncTrm(gBuiltIn[rqPowerSet],NewTrmList(addr(ETrm[K]),nil));
           lEqNr:=EqClassNr(lTrm); DisposeTrm(lTrm);
           if lEqNr <> 0 then
            begin
             lTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                  gBuiltIn[rqElement],
                                  NewTrmList(addr(ETrm[lEqNr]),nil));
             for i:=1 to EqClassNbr do
              begin
               with EqClassType[i] do
                for j:=0 to Count-1 do
                 if TypPtr(Items^[j])^.EqRadices(lTyp) then goto 12;
               continue;
12:
               lInstColl2.UniTrm(lLeft,addr(ETrm[i]));
               lInstColl1.UnionWith(@lInstColl2);
              end;
             dispose(lTyp,Done);
            end;
           lInstColl.JoinWith(@lInstColl1);
           UnionWith(@lInstColl);
          end;
       end;
      end
     else if lPred = gBuiltIn[rqLessOrEqual] then
      begin
       GetBinArgs(fFrm,lLeft,lRight);
       (* po cholere to ???, a jak potrzebne, to dlaczego wyzej nie ma *)
       for K := 0 to PosBas.Count-1 do
        begin
         lInstColl.UNIFrm(FFrm,PosBas.Items^[K]);
         UnionWith(@lInstColl);
        end;
       for k:=1 to EqClassNbr do
        with EqClassVal[k] do
        if Determined then
         begin
          lInstColl.UNITrm(lLeft,addr(ETrm[K]));
          if lInstColl.Count <> 0 then
           begin lInstColl1.InitBottom;
            for i:=1 to EqClassNbr do
             if EqClassVal[i].Determined then
             if IsRationalLE(EqClassVal[k].NumericValue,EqClassVal[i].NumericValue) then
               begin
{$IFDEF CH_REPORT}
                CHReport.Out_NumReq2(rqLessOrEqual,EqClassVal[k].NumericValue,
                           EqClassVal[i].NumericValue);
{$ENDIF}
                lInstColl2.UNITrm(lRight,addr(ETrm[i]));
                lInstColl1.UnionWith(@lInstColl2);
               end;
            lInstColl.JoinWith(@lInstColl1);
            UnionWith(@lInstColl);
           end;
         end;
       for k:=1 to EqClassNbr do
        with EqClassVal[k] do
         begin
          lInstColl.UNITrm(lLeft,addr(ETrm[K]));
          if lInstColl.Count <> 0 then
           begin lInstColl1.InitBottom;
            for i:=1 to EqClassNbr do
             begin
               lPositive:=EqClassSuperCluster[k]^.GetAttr(gBuiltIn[rqPositive],nil);
               lNegative:=EqClassSuperCluster[k]^.GetAttr(gBuiltIn[rqNegative],nil);
               rPositive:=EqClassSuperCluster[i]^.GetAttr(gBuiltIn[rqPositive],nil);
               rNegative:=EqClassSuperCluster[i]^.GetAttr(gBuiltIn[rqNegative],nil);
               lEqOcc:=false;
               if (lPositive<>nil) and (lPositive^.fNeg=ord(false)) then
                lEqOcc:=(rNegative<>nil) and (rNegative^.fNeg=ord(false));
               if lEqOcc then
                begin
                 lInstColl2.UNITrm(lRight,addr(ETrm[i]));
                 lInstColl1.UnionWith(@lInstColl2);
                end;
             end;
            lInstColl.JoinWith(@lInstColl1);
            UnionWith(@lInstColl);
           end;
         end;
      end
     else if lPred <> gBuiltIn[rqEqualsTo] then
       { nie ma potrzeby sprawdzania rownosci, ich juz nie ma }
      for K := 0 to PosBas.Count-1 do
       begin
        lInstColl.UNIFrm(FFrm,PosBas.Items^[K]);
        UnionWith(@lInstColl);
       end;
   end;
  ikFrmAttr:
   begin UniAttrFrm(fFrm,true);
   end;
  ikFrmQual:
   begin
     for k:=1 to EqClassNbr do
      begin
       lInstColl.UNITrm(QualFrmPtr(fFrm)^.QualTrm,addr(ETrm[k]));
       if lInstColl.Count <> 0 then
        begin
         lInstColl1.UNIEqClassTyps(k,QualFrmPtr(fFrm)^.QualTyp);
         lInstColl.JoinWith(@lInstColl1);
         UnionWith(@lInstColl);
        end;
      end;
   end;
  else
55: begin
     for K := 0 to PosBas.Count-1 do
     begin
       lInstColl.UNIFrm(FFrm,PosBas.Items^[K]);
       UnionWith(@lInstColl);
      end;
    end;
 end;
end;

//
function AttrAreSimilar(fAttr,aAttr:AttrPtr): boolean;
begin
 AttrAreSimilar := (fAttr^.fAttrNr = aAttr^.fAttrNr) and
               (fAttr^.fNeg = aAttr^.fNeg);
end;

function ClustersAreSimilar(fClu,aClu:AttrCollectionPtr): boolean;
 var I: integer;
begin ClustersAreSimilar:=false;
 with fClu^ do
 begin
  if Count <> aClu^.Count then exit;
  for I:=0 to Count-1 do
   if not AttrAreSimilar(Items^[I],aClu^.Items^[I]) then exit;
 end;
 ClustersAreSimilar:=true;
end;

function TypsAreSimilar(fTyp,aTyp:TypPtr): boolean;
 var aModNr: integer;
     A: TrmList;
begin TypsAreSimilar:=false;
 with fTyp^ do
 if TypSort = aTyp^.TypSort then
  if ClustersAreSimilar(LowerCluster,aTyp^.LowerCluster) then
  begin
    if ModNr=aTyp^.ModNr then
     begin TypsAreSimilar:=true;
      exit
     end;
    case TypSort of
     ikTypMode:
      begin
       aTyp^.AdjustTyp(aModNr,A);
       if ModNr=aModNr then TypsAreSimilar:=true;
      end;
     ikTypStruct: ;
     else RunTimeError(2902);
    end;
  end;
end;

function FrmsAreSimilar( fFrm,aFrm:FrmPtr ): boolean;
 var aPredNr,fPredNr,k: integer;
     aTrmList,fTrmList: TrmList;
begin
//writeln(infofile,'FrmsAreSimilar(');
//infoformula(fFrm); infonewline;
//infoformula(aFrm); infonewline;
 FrmsAreSimilar:=false;
 if fFrm^.FrmSort=aFrm^.FrmSort then
 case fFrm^.FrmSort of
  ikFrmVerum,ikFrmThesis: FrmsAreSimilar:=true;
  ikFrmNeg:
   FrmsAreSimilar:=FrmsAreSimilar(NegFrmPtr(fFrm)^.NegArg,NegFrmPtr(aFrm)^.NegArg);
  ikFrmQual:
   FrmsAreSimilar:=TypsAreSimilar(QualFrmPtr(fFrm)^.QualTyp,QualFrmPtr(aFrm)^.QualTyp);
  ikFrmConj:
   with ConjFrmPtr(fFrm)^.Conjuncts do
    if Count = ConjFrmPtr(aFrm)^.Conjuncts.Count then
     begin
      for k:=0 to Count-1 do
       if not FrmsAreSimilar(FrmPtr(Items^[k]),FrmPtr(ConjFrmPtr(aFrm)^.Conjuncts.Items^[k])) then
        exit;
      FrmsAreSimilar:=true;
     end;
  ikFrmSchPred,ikFrmPrivPred:
   if PredFrmPtr(fFrm)^.PredNr=PredFrmPtr(aFrm)^.PredNr then
    FrmsAreSimilar:=true;
  ikFrmAttr:
   begin AdjustAttrFrm(PredFrmPtr(aFrm),aPredNr,aTrmList);
    AdjustAttrFrm(PredFrmPtr(fFrm),fPredNr,fTrmList);
    if fPredNr=aPredNr then
     FrmsAreSimilar:=true;
   end;
  ikFrmPred:
   begin AdjustFrm(PredFrmPtr(aFrm),aPredNr,aTrmList);
    AdjustFrm(PredFrmPtr(fFrm),fPredNr,fTrmList);
    if fPredNr=aPredNr then
     FrmsAreSimilar:=true;
   end;
  ikFrmUniv:
   begin
//   if TypsAreSimilar(UnivFrmPtr(fFrm)^.Quantified,UnivFrmPtr(aFrm)^.Quantified) then
//    FrmsAreSimilar:=FrmsAreSimilar(UnivFrmPtr(fFrm)^.Scope,UnivFrmPtr(aFrm)^.Scope);
   end;
  ikFrmFlexConj:;
  else RunTimeError(3903);
 end;
end;


type
  ComplimentaryLIteralsPtr= ^ComplimentaryLIterals;
  ComplimentaryLIterals =
  object(MObject)
    nClauseNr1,nClauseNr2,
    nAtomNr1,nAtomNr2: integer;
    nInsts: InstCollectionPtr;
    constructor Init(aClauseNr1,aClauseNr2,aAtomNr1,aAtomNr2: integer;
                     aInsts: InstCollectionPtr);
    destructor Done; virtual;
  end;

constructor ComplimentaryLiterals.Init(aClauseNr1,aClauseNr2,aAtomNr1,aAtomNr2:integer;
                                aInsts:InstCollectionPtr);
begin
  nClauseNr1:=aClauseNr1;
  nClauseNr2:=aClauseNr2;
  nAtomNr1:=aAtomNr1;
  nAtomNr2:=aAtomNr2;
  nInsts:= aInsts;
end;

destructor ComplimentaryLiterals.Done;
begin
  if nInsts <> nil then
   dispose(nInsts,Done);
end;

function ResolventVerify(const aAllClauses:InstCollection; const aComplimentaryLiterals:MList): boolean;
 var i,j: integer;
     lClause1,lClause2: NatFuncPtr;
     lInstColl: InstCollection;
     lInsts: InstCollectionPtr;
     lContr: boolean;
     lUnifColl: ULCollection;
 label 2;
begin
 ResolventVerify:=false;
 lContr:=false;
 for i := 0 to aComplimentaryLiterals.Count - 1 do
  with ComplimentaryLiteralsPtr(aComplimentaryLiterals.Items^[i])^ do
  if nInsts <> nil then
   begin
    lClause1:=aAllClauses.Items^[nClauseNr1];
    lClause2:=aAllClauses.Items^[nClauseNr2];
{$IFDEF MDEBUG}
writeln(infofile,'Unification of the two basic sentences (simultanous substitution): ',nAtomNr1+1,' & ',nAtomNr2+1);
if lClause1^.Items^[nAtomNr1].Y = 0 then infochar('-');
infoformula(FreeBasic.Items^[lClause1^.Items^[nAtomNr1].X]); infonewline;
if lClause2^.Items^[nAtomNr2].Y = 0 then infochar('-');
infoformula(FreeBasic.Items^[lClause2^.Items^[nAtomNr2].X]); infonewline;
{$ENDIF}
    lUnifColl.Init(0,8);
    lUnifColl.Insert(nInsts);
    nInsts:=nil;
{$IFDEF MDEBUG}
writeln(infofile,'Substitution in the first clause');
{$ENDIF}
    with lClause1^ do
     for j:=0 to Count-1 do if j <> nAtomNr1 then
      with Items^[j] do
       begin
        if Y = 1 then
         lInsts:=new(InstCollectionPtr,COMPInstAsFalse(FreeBasic.Items^[X]))
        else lInsts:=new(InstCollectionPtr,COMPInstAsTrue(FreeBasic.Items^[X]));
        if lInsts^.nOverflow then
         begin
          dispose(lInsts, Done);
{$IFDEF MDEBUG}
writeln(infofile,'Failed Unification with - Overflow!: ',j+1);
{$ENDIF}
          goto 2;
         end;
        if lInsts^.Count = 0 then
         begin dispose(lInsts,Done);
{$IFDEF MDEBUG}
writeln(infofile,'Failed Unification with: ',j+1);
{$ENDIF}
          goto 2;
         end;
{$IFDEF MDEBUG}
writeln(infofile,'Unification with: ',j+1);
if Y = 0 then infochar('-');
infoformula(FreeBasic.Items^[X]);infonewline;
lInsts^.InfoLatColl;
{$ENDIF}
        if lInsts^.Status = Top then dispose(lInsts,Done)
         else lUnifColl.Insert(lInsts);
       end;
{$IFDEF MDEBUG}
writeln(infofile,'Substitution in the second clause');
{$ENDIF}
    with lClause2^ do
     for j:=0 to Count-1 do if j <> nAtomNr2 then
      with Items^[j] do
       begin
        if Y = 1 then
         lInsts:=new(InstCollectionPtr,COMPInstAsFalse(FreeBasic.Items^[X]))
        else lInsts:=new(InstCollectionPtr,COMPInstAsTrue(FreeBasic.Items^[X]));
        if lInsts^.nOverflow then
         begin
          dispose(lInsts, Done);
{$IFDEF MDEBUG}
writeln(infofile,'Failed Unification with - Overflow!: ',j+1);
{$ENDIF}
          goto 2;
         end;
        if lInsts^.Count = 0 then
         begin dispose(lInsts,Done);
{$IFDEF MDEBUG}
writeln(infofile,'Failed Unification with: ',j+1);
{$ENDIF}
           goto 2;
         end;
{$IFDEF MDEBUG}
writeln(infofile,'Unification with: ',j+1);
if Y = 1 then infochar('-');
infoformula(FreeBasic.Items^[X]); infonewline;
lInsts^.InfoLatColl;
{$ENDIF}
        if lInsts^.Status = Top
         then dispose(lInsts,Done)
         else lUnifColl.Insert(lInsts);
       end;
    lInstColl.JoinInstList(lUnifColl);
{$IFDEF MDEBUG}
if lInstColl.Count<>0 then
begin
writeln(infofile,'*** Unify: final substitutions');
lInstColl.InfoLatColl;
end
else
writeln(infofile,'Failed Unification: ',nAtomNr1+1,' & ',nAtomNr2+1);
{$ENDIF}
    lContr:=lInstColl.Count<>0;
    lInstColl.Done;
2:
    if lContr then
     begin
{$IFDEF CHSTAT}
      with CHStatPtr(gStat.Items^[gStat.Count-1])^ do
       nContrNr:=i;
{$ENDIF}
      ResolventVerify:=true;
      break;
     end;
   end;
end;

procedure CreateClauses(const aUniv:MCollection; var aAllClauses:InstCollection);
var i: integer;
    lFrm: FrmPtr;
    lNormalForm{,lClauses}: InstCollection;
//    lTyp: TypPtr;
//    lClause: NatFuncPtr;
begin
 aAllClauses.InitBottom;
 for i:=0 to aUniv.Count-1 do
  begin
   lFrm:=PositivelyStandarized(aUniv.Items^[i]);
   BoundVarNbr:=0;
   gFreeVarsBase:=FreeVarType.Count;
   RemoveExtQuantifier(lFrm);
   BoundVarNbr:=0;
   gFreeVarsNbr:=FreeVarType.Count;
   lNormalForm.NormalizeAsFalse(lFrm);
{$IFDEF MDEBUG}
writeln(infofile,'Clauses of the universal sentence nr: ',i);
//InfoInference(lNormalForm,FreeBasic);
{$ENDIF}
(*
   if lNormalForm.Count <> 0 then
    begin
     lClauses.InitSingle(lNormalForm.Items^[0]);
     lClauses.SetLimit(lNormalForm.Limit);
     lNormalForm.Items^[0]:=nil;
     for j := 1 to lNormalForm.Count - 1 do
      begin
       for k := gFreeVarsBase to gFreeVarsNbr - 1 do
        begin
         lTyp:=TypPtr(FreeVarType.Items^[k])^.CopyType;
         lTyp^.WithinType(SetNewFreeVarInTrm);
         FreeVarType.Insert(lTyp);
        end;
       lClause:=new(NatFuncPtr,InitNatFunc(NatFuncPtr(lNormalForm.Items^[j])^.Count,4));
       for k := 0 to NatFuncPtr(lNormalForm.Items^[j])^.Count - 1 do
        with NatFuncPtr(lNormalForm.Items^[j])^.Items^[k] do
        begin
         lFrm:=FrmPtr(FreeBasic.Items^[X])^.CopyFormula;
         WithinFormula(lFrm,SetNewFreeVarInTrm);
         lClause^.Assign(NewBasic(lFrm,true),Y);
        end;
       lClauses.InsertAndAbsorb(lClause);
//       lClauses.InsertAndAbsorb(lNormalForm.Items^[j]);
//       lNormalForm.Items^[j]:=nil;
      end;
     lNormalForm.Done;
{$IFDEF MDEBUG}
writeln(infofile,'Clauses with distributed free variables ');
InfoInference(lClauses,FreeBasic);
{$ENDIF}
     aAllClauses.UnionWith(@lClauses);
    end;
*)
   aAllClauses.UnionWith(@lNormalForm);
  end;
end;

procedure CollectComplementaryLiterals(const aAllClauses:InstCollection;
                                       var aComplimentaryLiterals:MList);
 var i1,i2,j1,j2: integer;
     lClause1,lClause2: NatFuncPtr;
     lInsts: InstCollectionPtr;
begin
  aComplimentaryLiterals.Init(0);
  for i1 := 0 to aAllClauses.Count - 1 do
   for i2 := i1+1 to aAllClauses.Count - 1 do
   begin
    lClause1:=aAllClauses.Items^[i1];
    lClause2:=aAllClauses.Items^[i2];
{$IFDEF MDEBUG}
writeln(infofile,'Clauses: ',i1,' & ',i2);
infoeval(lClause1,FreeBasic);
infoeval(lClause2,FreeBasic);
{$ENDIF}
    for j1:=0 to lClause1^.Count-1 do
     for j2:=0 to lClause2^.Count-1 do
     begin
      if FrmsAreSimilar(FreeBasic.Items^[lClause1^.Items^[j1].X],
                       FreeBasic.Items^[lClause2^.Items^[j2].X]) and
         (lClause1^.Items^[j1].Y <> lClause2^.Items^[j2].Y) then
        begin
          lInsts:=new(InstCollectionPtr,
                       UnifyBasicFrm(FreeBasic.Items^[lClause1^.Items^[j1].X],
                                     FreeBasic.Items^[lClause2^.Items^[j2].X]));
          if lInsts^.nOverflow then
           begin
            dispose(lInsts,Done);
            aComplimentaryLiterals.Insert(new(ComplimentaryLiteralsPtr,
                                          Init(i1,i2,j1,j2,nil)));
            exit
           end;
          if (lInsts^.Count = 0) or (lInsts^.Status = Top) then
           dispose(lInsts,Done)
          else
           begin
            aComplimentaryLiterals.Insert(new(ComplimentaryLiteralsPtr,
                                          Init(i1,i2,j1,j2,lInsts)));
           end;
        end;
     end;
   end;
end;

function Unifiable(const aUniv:MCollection): boolean;
 var i,lInstNbr: integer;
     lAllClauses: InstCollection;
     lComplimentaryLiterals: MList;
begin
{$IFDEF MDEBUG}
writeln(InfoFile,'Checking for Unification  (Unifiable) ------------------: ');
{$ENDIF}
 Unifiable:=false;
//
//  Initialiazing Clauses - universal sentences with disjunctive normal form
 FreeVarType.Init(4);
 UnifBase:=0;
 FreeVarUnifs.Init(EqClassNbr,MaxInstTrmNbr);
 ExGenerators.Init(EqClassNbr,MaxInstTrmNbr);
 InitVarBase:=InitFreeVarBase;
 SetVarInTrm:=SetFreeVarInTrm;
 NewVariable:=NewFreeVar;
 BoundVarNbr:=0;
 FreeBasic.Init(8,16);
 gBasicPtr:=addr(FreeBasic);
//
//  Counting Clauses
 CreateClauses(aUniv,lAllClauses);
{$IFDEF MDEBUG}
writeln(infofile,'All Clauses numbers: ', lAllClauses.Count);
InfoInference(lAllClauses,FreeBasic);
{$ENDIF}
{$IFDEF CHSTAT}
 if lAllClauses.Count > 0 then
 begin
  gStat.Insert(new(CHStatPtr,Init(CurPos)));
  with CHStatPtr(gStat.Items^[gStat.Count-1])^ do
   nClausesNbr:=lAllClauses.Count;
 end;
{$ENDIF}
//
//  Looking for unifiable atoms (literals)
//  The number of all clauses is limited
if (lAllClauses.Count >= 2) and (lAllClauses.Count <= 4) then //  >= 2 !!
 begin
  CollectComplementaryLiterals(lAllClauses,lComplimentaryLiterals);
{$IFDEF MDEBUG}
writeln(InfoFile,'Unifiable complimentary atoms numbers: ',lComplimentaryLiterals.Count);
for i := 0 to lComplimentaryLiterals.Count - 1 do
with ComplimentaryLiteralsPtr(lComplimentaryLiterals.Items^[i])^ do
begin
writeln(infofile,'Unification of the two basic sentences (simultanous substitution): ',nAtomNr1+1,' & ',nAtomNr2+1);
if NatFuncPtr(lAllClauses.Items^[nClauseNr1])^.Items^[nAtomNr1].Y = 0 then infochar('-');
infoformula(FreeBasic.Items^[NatFuncPtr(lAllClauses.Items^[nClauseNr1])^.Items^[nAtomNr1].X]);
infonewline;
if NatFuncPtr(lAllClauses.Items^[nClauseNr2])^.Items^[nAtomNr2].Y = 0 then infochar('-');
infoformula(FreeBasic.Items^[NatFuncPtr(lAllClauses.Items^[nClauseNr2])^.Items^[nAtomNr2].X]);
infonewline;
if nInsts = nil then
writeln(infofile,' *InstOverflow*')
else nInsts^.InfoLatColl;
end;
{$ENDIF}
{$IFDEF CHSTAT}
  if lAllClauses.Count > 0 then
   if lComplimentaryLiterals.Count = 0 then
    gStat.AtFree(gStat.Count-1)
   else with CHStatPtr(gStat.Items^[gStat.Count-1])^ do
    for i := 0 to lComplimentaryLiterals.Count - 1 do
    with ComplimentaryLiteralsPtr(lComplimentaryLiterals.Items^[i])^ do
    begin
     if nInsts = nil then
      nInstStats.Insert(MaxInstNbr)
     else nInstStats.Insert(nInsts.Count);
    end;
{$ENDIF}
//
   lInstNbr:=0;
   for i := 0 to lComplimentaryLiterals.Count - 1 do
    with ComplimentaryLiteralsPtr(lComplimentaryLiterals.Items^[i])^ do
     if nInsts = nil then
       inc(lInstNbr,MaxInstNbr)
     else inc(lInstNbr,nInsts.Count);
//
// The number of complimentary literals is limited
   if not LatOvfl and //(lInstNbr < MaxInstNbr) and
      (lComplimentaryLiterals.Count >= 1)
      and (lComplimentaryLiterals.Count <= 1) //2!!
      then //  >= 1 !!
     Unifiable:=ResolventVerify(lAllClauses,lComplimentaryLiterals);
   lComplimentaryLiterals.Done;
 end;
 lAllClauses.Done;
//
 FreeBasic.Done;
 FreeVarType.Done;
 FreeVarUnifs.Done;
 gLatStatus:=Generator;
 ExGenerators.Done;
 FreeVarType.Done;
 gLatStatus:=Regular;
end;

function Verify(fFrm:FrmPtr): boolean;
 var lFrm:FrmPtr;
      i,j: integer;
      lVerified: boolean;
      lUnifColl: ULCollection;
      lInstColl: InstCollection;
      lInsts: InstCollectionPtr;
      lNormalForm: InstCollection;
 label 1,2;
begin
   lFrm:=PositivelyStandarized(fFrm);
{$IFDEF MDEBUG}
writeln(InfoFile,'Verify ------------------: ',FreeVarBase);
InfoFormula(lFrm); InfoNewline;
{$ENDIF}
   FreeVarType.Init(4);
   UnifBase:=0;
   FreeVarUnifs.Init(EqClassNbr,MaxInstTrmNbr);
   ExGenerators.Init(EqClassNbr,MaxInstTrmNbr);
   InitVarBase:=InitFreeVarBase;
   SetVarInTrm:=SetFreeVarInTrm;
   NewVariable:=NewFreeVar;
   BoundVarNbr:=0;
   RemoveExtQuantifier(lFrm);
{$IFDEF MDEBUG}
writeln(InfoFile,'Verify: ');
InfoFormula(lFrm); InfoNewline;
{$ENDIF}
   BoundVarNbr:=0;
   FreeBasic.Init(8,16);
   gBasicPtr:=addr(FreeBasic);
   lNormalForm.NormalizeAsFalse(lFrm);
{$IFDEF MDEBUG}
writeln(infofile,'clauses:');
InfoInference(lNormalForm,FreeBasic);
{$ENDIF}
   for i:=0 to lNormalForm.Count-1 do
   begin
    if lNormalForm.Items = nil then
     begin lVerified:=true;
      goto 1
     end;
    with NatFuncPtr(lNormalForm.Items^[i])^ do
     begin
      lUnifColl.Init(0,8);
      for j:=0 to Count-1 do
      with Items^[j] do
       begin
        if Y = 1 then
         lInsts:=new(InstCollectionPtr,COMPInstAsFalse(FreeBasic.Items^[X]))
        else lInsts:=new(InstCollectionPtr,COMPInstAsTrue(FreeBasic.Items^[X]));
        if lInsts^.Count = 0 then
         begin lUnifColl.Done;
          dispose(lInsts,Done);
          goto 2;
         end;
        if lInsts^.Status = Top then dispose(lInsts,Done)
        else lUnifColl.Insert(lInsts);
       end;
      lInstColl.JoinInstList(lUnifColl);
      lVerified:=lInstColl.Count<>0;
      if lVerified then
       begin
{$IFDEF MDEBUG}
writeln(infofile,'*** Verify: substitutions ');
lInstColl.InfoLatColl;
{$ENDIF}
        lInstColl.Done;
        goto 1;
       end;
      lInstColl.Done;
     end;
2:
    if LatOvfl then break;
   end;
   lVerified:=false;
1:
   dispose(lFrm,Done);
   lNormalForm.Done;
   FreeBasic.Done;
   FreeVarType.Done;
   FreeVarUnifs.Done; mizassert(2550,FreeVarUnifs.Count=0);
   gLatStatus:=Generator;
   ExGenerators.Done;
   FreeVarType.Done;
   gLatStatus:=Regular;
   Verify:=lVerified;
end;

procedure Unification;
 var ii,jj,lPredNr,lEqList: integer;
     UNIV,UNIVPair: MCollection;
     lLatOvfl: boolean;
     lFrm,lFrm1: FrmPtr;
     lTrmList: TrmList;
 label 10;
begin
 BoundVarNbr:=0;

 UNIV.Init(0,2); UNIVPair.Init(0,2);
 for ii:=0 to PosBas.Count-1 do
  if FrmPtr(PosBas.Items^[ii])^.FrmSort=ikFrmUniv then
    UNIV.Insert(PosBas.Items^[ii]);

 lLatOvfl:=LatOvfl;
 LatOvfl:=false;
 for ii:=0 to UNIV.Count-1 do
  begin
   if Verify(UNIV.Items^[ii]) then
    begin
     SetContr(45);
     goto 10;
    end;
   if LatOvfl then break;
  end;
 lLatOvfl:=LatOvfl or lLatOvfl;
 LatOvfl:=false;
//
 if not SwitchOffUnifier then
  begin
// The unification check based on the whole UNIV collection has been replaced by the following code
// selecting only pairs of universal statements from the collection - this should eliminate simple cases
// of the lack of reference monotonicity due to the hardcoded numerical limits in the implementation.
{
   if Unifiable(UNIV) then
    begin
      SetContr(46);
      goto 10;
    end;
   lLatOvfl:=LatOvfl or lLatOvfl;
   LatOvfl:=false;
}
    for ii:=0 to UNIV.Count-1 do
     for jj:=ii to UNIV.Count-1 do
      begin
       UNIVPair.Insert(UNIV.Items^[ii]);
       if jj<>ii then
        UNIVPair.Insert(UNIV.Items^[jj]);
       if Unifiable(UNIVPair) then
        begin
         SetContr(47);
         goto 10;
        end;
       lLatOvfl:=LatOvfl or lLatOvfl;
       LatOvfl:=false;
       UNIVPair.DeleteAll;
      end;
  end;
 for ii:=0 to PosBas.Count-1 do
  if FrmPtr(PosBas.Items^[ii])^.FrmSort=ikFrmPred then
    begin
     AdjustFrm(PredFrmPtr(PosBas.Items^[ii]),lPredNr,lTrmList);
     if (lPredNr=gBuiltIn[rqBelongsTo]) then
      begin
       for lEqList:=0 to EqList[expTrmFraenkel][VarTrmPtr(lTrmList^.NextTrm^.XTrmPtr)^.VarNr]^.Count-1 do
        with TrmPtr(EqList[expTrmFraenkel][VarTrmPtr(lTrmList^.NextTrm^.XTrmPtr)^.VarNr]^.Items^[lEqList])^ do
         begin
          if TrmSort=ikTrmFraenkel then
           begin
            lFrm1:=FraenkelFrm(lTrmList^.XTrmPtr,EqList[expTrmFraenkel][VarTrmPtr(lTrmList^.NextTrm^.XTrmPtr)^.VarNr]^.Items^[lEqList]);
            BoundVarNbr:=0;
            lFrm:=NewNegDis(DistributeQuantifiers(lFrm1));
            dispose(lFrm1,Done);
            if Verify(lFrm) then
             begin
              dispose(lFrm,Done);
              SetContr(48);
              goto 10;
             end;
            dispose(lFrm,Done);
           end;
         end;
      end;
    end;

 lLatOvfl:=LatOvfl or lLatOvfl;
 LatOvfl:=false;
 for ii:=0 to NegBas.Count-1 do
  if FrmPtr(NegBas.Items^[ii])^.FrmSort=ikFrmPred then
    begin
     AdjustFrm(PredFrmPtr(NegBas.Items^[ii]),lPredNr,lTrmList);
     if (lPredNr=gBuiltIn[rqBelongsTo]) then
      begin
       for lEqList:=0 to EqList[expTrmFraenkel][VarTrmPtr(lTrmList^.NextTrm^.XTrmPtr)^.VarNr]^.Count-1 do
        with TrmPtr(EqList[expTrmFraenkel][VarTrmPtr(lTrmList^.NextTrm^.XTrmPtr)^.VarNr]^.Items^[lEqList])^ do
        begin
         if TrmSort=ikTrmFraenkel then
          begin
           lFrm:=FraenkelFrm(lTrmList^.XTrmPtr,EqList[expTrmFraenkel][VarTrmPtr(lTrmList^.NextTrm^.XTrmPtr)^.VarNr]^.Items^[lEqList]);
           BoundVarNbr:=0;
           lFrm1:=DistributeQuantifiers(lFrm);
           dispose(lFrm,Done);
           if Verify(lFrm1) then
            begin
             dispose(lFrm1,Done);
             SetContr(49);
             goto 10;
            end;
           dispose(lFrm1,Done);
          end;
        end;
      end;
    end;
  LatOvfl:=lLatOvfl;

{$IFDEF MDEBUG}
writeln(InfoFile,'Failed'); flush(InfoFile);
{$ENDIF} ;

10:
  UNIVPair.DeleteAll; Univ.DeleteAll; Univ.Done;
end;

end.
