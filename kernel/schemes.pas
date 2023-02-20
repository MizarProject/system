(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit schemes;

interface

uses mobjects,errhan,schemhan;

var
  gSchPos: Position;

procedure Schematize(const fSch:SchRefItem; const aSntList:MList);

implementation

uses limits,correl,builtin,justhan,lexicon,enums
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF}
{$IFDEF SCH_REPORT} ,prephan,xmldict {$ENDIF};
var
 gFConstSubst: MList;
 gFFuncSubst,gFPredSubst: BinIntFunc;
 gCurSchTypes: MCollection;
 gSchErrorNr: integer;

var ThereAreBound: boolean;
procedure SchRenBound(var fTrm: TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if TrmSort=ikTrmBound then
   if VarNr>BoundVarNbr
    then dec(VarNr,BoundVarNbr)
   else ThereAreBound:=true;
end;

procedure SchChkBound(var fTrm: TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if (TrmSort=ikTrmBound) and (VarNr<=BoundVarNbr) then
   else ThereAreBound:=true;
end;

function SchEqTrm(fTrm1,fTrm2:TrmPtr):boolean; forward;

function SchEqTrmList(fTL1,fTL2:TrmList): boolean;
begin SchEqTrmList:=false;
 while (fTL1<>nil) and (fTL2<>nil) do
  begin if not SchEqTrm(fTL1^.XTrmPtr,fTL2^.XtrmPtr) then exit;
   fTL1:=fTL1^.NextTrm; fTL2:=fTL2^.NextTrm;
  end;
 SchEqTrmList:=fTL1=fTL2;
end;

function SchEqAttr(fAttr1,fAttr2: AttrPtr): boolean;
  var lAttrNr1,lAttrNr2: integer; lArgs1,lArgs2: TrmList;
begin
 fAttr1^.AdjustAttr(lAttrNr1,lArgs1); fAttr2^.AdjustAttr(lAttrNr2,lArgs2);
 SchEqAttr := (lAttrNr1 = lAttrNr2) and (fAttr1^.fNeg = fAttr2^.fNeg) and
           SchEqTrmList(lArgs1,lArgs2);
end;

function SchIsSubsetOf(fClu,aClu:AttrCollectionPtr): boolean;
 var i,j,k,lAttrNr: integer;
     lFConstSubst: MList; lFFuncSubst,lFPredSubst: BinIntFunc;
begin SchIsSubsetOf:=false;
 with fClu^ do
 begin
  if aClu^.Count < Count then exit;
  lAttrNr:=0;
  lFConstSubst.Init(MaxSchFuncNbr+1);
  for i:=0 to Count-1 do
   for j:=0 to aClu^.Count -1 do
    begin //lSubstTrm:=gSubstTrm;
     for k:=0 to MaxSchFuncNbr do
      lFConstSubst.Items^[k]:=gFConstSubst.Items^[k];
     lFFuncSubst.CopyBinIntFunc(gFFuncSubst);
     lFPredSubst.CopyBinIntFunc(gFPredSubst);
     if SchEqAttr(Items^[i],aClu^.Items^[j]) then
      begin inc(lAttrNr); break; end
     else
      begin
     //gSubstTrm:=lSubstTrm;
       for k:=0 to MaxSchFuncNbr do
        gFConstSubst.Items^[k]:=lFConstSubst.Items^[k];
       gFFuncSubst.fCount:=lFFuncSubst.fCount;
       for k:=0 to lFFuncSubst.fCount-1 do
        gFFuncSubst.fList^[k]:=lFFuncSubst.fList^[k];
       gFPredSubst.fCount:=lFPredSubst.fCount;
       for k:=0 to lFPredSubst.fCount-1 do
        gFPredSubst.fList^[k]:=lFPredSubst.fList^[k];
      end;
     lFFuncSubst.Done;
     lFPredSubst.Done;
    end;
  if lAttrNr = count then SchIsSubsetOf:=true;
  lFConstSubst.DeleteAll;
  lFConstSubst.Done;
 end;
end;

function SchIsSupersetOf(fClu,aClu:AttrCollectionPtr): boolean;
 var i,j,k,lAttrNr: integer;
     lFConstSubst: MList; lFFuncSubst,lFPredSubst: BinIntFunc;
begin SchIsSupersetOf:=false;
 with fClu^ do
 begin
  if aClu^.Count > Count then exit;
  lAttrNr:=0;
  lFConstSubst.Init(MaxSchFuncNbr+1);
  for j:=0 to aClu^.Count -1 do
   for i:=0 to Count-1 do
    begin //lSubstTrm:=gSubstTrm;
     for k:=0 to MaxSchFuncNbr do lFConstSubst.Items^[k]:=gFConstSubst.Items^[k];
     lFFuncSubst.CopyBinIntFunc(gFFuncSubst);
     lFPredSubst.CopyBinIntFunc(gFPredSubst);
     if SchEqAttr(Items^[i],aClu^.Items^[j]) then
      begin inc(lAttrNr); break; end
     else
      begin
     //gSubstTrm:=lSubstTrm;
       for k:=0 to MaxSchFuncNbr do
        gFConstSubst.Items^[k]:=lFConstSubst.Items^[k];
       gFFuncSubst.fCount:=lFFuncSubst.fCount;
       for k:=0 to lFFuncSubst.fCount-1 do
        gFFuncSubst.fList^[k]:=lFFuncSubst.fList^[k];
       gFPredSubst.fCount:=lFPredSubst.fCount;
       for k:=0 to lFPredSubst.fCount-1 do
        gFPredSubst.fList^[k]:=lFPredSubst.fList^[k];
      end;
     lFFuncSubst.Done;
     lFPredSubst.Done;
    end;
  if lAttrNr = aClu^.count then SchIsSupersetOf:=true;
  lFConstSubst.DeleteAll;
  lFConstSubst.Done;
 end;
end;

function SchEqualClusters(fTyp1,fTyp2:TypPtr): boolean;
begin
 SchEqualClusters:=SchIsSubsetOf(fTyp1^.LowerCluster,fTyp2^.UpperCluster) and
  SchIsSupersetOf(fTyp1^.UpperCluster,fTyp2^.LowerCluster);
end;

function SchCompTyp(fTyp1,fTyp2:TypPtr):boolean;
 var ModNr1,ModNr2: integer; A1,A2: TrmList;
begin SchCompTyp:=false;
 with FTyp1^ do
  if TypSort=fTyp2^.TypSort then
   case TypSort of
    ikTypMode:
      begin fTyp1^.AdjustTyp(ModNr1,A1); fTyp2^.AdjustTyp(ModNr2,A2);
       if ModNr1=ModNr2 then SchCompTyp:=SchEqTrmList(A1,A2);
      end;
    ikTypStruct:
      if ModNr=fTyp2^.ModNr then
       SchCompTyp:=SchEqTrmList(ModArgs,fTyp2^.ModArgs);
    ikError: SchCompTyp:=true;
    else RunTimeError(2451);
   end;
end;

function SchEqTyp( fTyp1,fTyp2:TypPtr ):boolean;
 var ModNr1,ModNr2: integer; A1,A2: TrmList;
begin SchEqTyp:=false;
 with FTyp1^ do
  if TypSort=TypPtr(fTyp2)^.TypSort then
   case TypSort of
    ikTypMode:
     if SchEqualClusters(TypPtr(fTyp1),TypPtr(fTyp2)) then
      begin TypPtr(fTyp1)^.AdjustTyp(ModNr1,A1); TypPtr(fTyp2)^.AdjustTyp(ModNr2,A2);
       if ModNr1=ModNr2 then SchEqTyp:=SchEqTrmList(A1,A2);
      end;
    ikTypStruct:
     if SchEqualClusters(TypPtr(fTyp1),TypPtr(fTyp2)) then
      if ModNr=TypPtr(fTyp2)^.ModNr then
       SchEqTyp:=SchEqTrmList(ModArgs,TypPtr(fTyp2)^.ModArgs);
    ikError: SchEqTyp:=true;
    else RunTimeError(2448);
   end;
end;

function SchEqFrm(fFrm1,fFrm2:FrmPtr):boolean;
 var PredNr1,PredNr2,k,l,lValue: integer; A1,A2: TrmList;
     Left1,Right1,Left2,Right2: TrmPtr;
     lFrm: FrmPtr;
     lNeg: boolean;
begin SchEqFrm:=false;
 lNeg:=false;
 if (fFrm1^.FrmSort = ikFrmNeg) and (fFrm2^.FrmSort <> ikFrmNeg) then
  begin lNeg:=true;
    fFrm1:=NegFrmPtr(fFrm1)^.NegArg;
  end
 else if (fFrm2^.FrmSort = ikFrmNeg) and (fFrm1^.FrmSort <> ikFrmNeg) then
  begin lNeg:=true;
    fFrm2:=NegFrmPtr(fFrm2)^.NegArg;
  end;
 if fFrm1^.FrmSort = ikFrmSchPred then
  with PredFrmPtr(fFrm1)^ do
  begin
   case fFrm2^.FrmSort of
   ikFrmPred,ikFrmPrivPred,ikFrmSchPred:
    begin
     if gFPredSubst.HasInDom(Ord(fFrm2^.FrmSort),PredNr) then
      begin
       lValue:=gFPredSubst.Value(Ord(fFrm2^.FrmSort),PredNr);
       if lNeg and (lValue < 0) then lValue:=-lValue;
       if lValue = PredFrmPtr(fFrm2)^.PredNr then
        if SchEqTrmList(PredArgs,PredFrmPtr(fFrm2)^.PredArgs) then
         SchEqFrm:=true
        else gSchErrorNr:=23
       else gSchErrorNr:=24;
      end
     else
      begin
       case fFrm2^.FrmSort of
       ikFrmPrivPred:
        if gFPredSubst.HasInDom(Ord(ikFrmPred),PredNr) or
           gFPredSubst.HasInDom(Ord(ikFrmSchPred),PredNr) then
         begin
          gSchErrorNr:=31;
          exit;
         end;
       ikFrmSchPred:
        if gFPredSubst.HasInDom(Ord(ikFrmPred),PredNr) or
           gFPredSubst.HasInDom(Ord(ikFrmPrivPred),PredNr) then
         begin
          gSchErrorNr:=31;
          exit;
         end;
       ikFrmPred:
        if gFPredSubst.HasInDom(Ord(ikFrmSchPred),PredNr) or
           gFPredSubst.HasInDom(Ord(ikFrmPrivPred),PredNr) then
         begin
          gSchErrorNr:=31;
          exit;
         end;
       else RunTimeError(2452);
       end;
       lValue:=PredFrmPtr(fFrm2)^.PredNr;
       if lNeg then lValue:=-lValue;
       gFPredSubst.Assign(Ord(fFrm2^.FrmSort),PredNr,PredFrmPtr(fFrm2)^.PredNr);
       if SchEqTrmList(PredArgs,PredFrmPtr(fFrm2)^.PredArgs) then
         SchEqFrm:=true
       else gSchErrorNr:=23;
      end
    end;
   else gSchErrorNr:=22;
   end
  end
 else if (fFrm1^.FrmSort=fFrm2^.FrmSort) and (not lNeg) then
  case fFrm1^.FrmSort of
   ikFrmVerum,ikFrmThesis: SchEqFrm:=true;
   ikFrmNeg: SchEqFrm:=SchEqFrm(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmQual:
    if SchEqTrm(QualFrmPtr(fFrm1)^.QualTrm,QualFrmPtr(fFrm2)^.QualTrm) then
     SchEqFrm:=SchEqTyp(QualFrmPtr(fFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^.Conjuncts do
     if Count = ConjFrmPtr(fFrm2)^.Conjuncts.Count then
      begin
       for k:=0 to Count-1 do
        if not SchEqFrm(FrmPtr(Items^[k]),ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k]) then
          exit;
       SchEqFrm:=true;
      end;
   ikFrmAttr:
    begin AdjustAttrFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustAttrFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
      SchEqFrm:=SchEqTrmList(A1,A2);
    end;
   ikFrmPred:
    begin AdjustFrm(PredFrmPtr(fFrm1),PredNr1,A1);
     AdjustFrm(PredFrmPtr(fFrm2),PredNr2,A2);
     if PredNr1=PredNr2 then
      if PredNr1=gBuiltIn[rqEqualsTo] then
       begin
        Left1:=A1^.XTrmPtr; Right1:=A1^.NextTrm^.XTrmPtr;
        Left2:=A2^.XTrmPtr; Right2:=A2^.NextTrm^.XTrmPtr;
        if SchEqTrm(Left1,Left2) and SchEqTrm(Right1,Right2) then
          SchEqFrm:=true
//        else symetria - wyglada, ze nie mozna, ale .....
//       SchEqTrm(Right1,Left2) and SchEqTrm(Left1,Right2)};
       end
      else SchEqFrm:=SchEqTrmList(A1,A2);
    end;
   ikFrmUniv:
   begin
    MarkTermsInTTColl;
    inc(BoundVarNbr);
    if SchEqTyp(UnivFrmPtr(fFrm1)^.Quantified,UnivFrmPtr(fFrm2)^.Quantified) then
     begin
      BoundVar[BoundVarNbr]:=UnivFrmPtr(fFrm2)^.Quantified;
      SchEqFrm:=SchEqFrm(UnivFrmPtr(fFrm1)^.Scope,UnivFrmPtr(fFrm2)^.Scope);
      dec(BoundVarNbr);
     end;
    RemoveTermsFromTTColl;
   end;
   ikFrmFlexConj: SchEqFrm:=
     SchEqFrm(FlexFrmPtr(fFrm1)^.nLeftOrigFrm,FlexFrmPtr(fFrm2)^.nLeftOrigFrm) and
     SchEqFrm(FlexFrmPtr(fFrm1)^.nRightOrigFrm,FlexFrmPtr(fFrm2)^.nRightOrigFrm);
   ikError: SchEqFrm:=true;
   else RunTimeError(2449);
  end
 else if fFrm2^.FrmSort = ikFrmPrivPred then
  SchEqFrm:=SchEqFrm(fFrm1,LocPredFrmPtr(fFrm2)^.PredExp);
end;

function SchWidennings(aSource,fTarget: TypPtr): boolean;
 var lFConstSubst: MList;
     lFFuncSubst,lFPredSubst: BinIntFunc;
 procedure RetriveSubst;
  var i: integer;
 begin
  gFConstSubst.Count:=lFConstSubst.Count;
  for i:=0 to lFConstSubst.Count-1 do
    gFConstSubst.Items^[i]:=lFConstSubst.Items^[i];
  gFFuncSubst.fCount:=lFFuncSubst.fCount;
  for i:=0 to lFFuncSubst.fCount-1 do
    gFFuncSubst.fList^[i]:=lFFuncSubst.fList^[i];
  gFPredSubst.fCount:=lFPredSubst.fCount;
  for i:=0 to lFPredSubst.fCount-1 do
    gFPredSubst.fList^[i]:=lFPredSubst.fList^[i];
 end;
 var lTyp,wTyp: TypPtr; z: integer;
begin SchWidennings:=false;
// if (aSource = nil) or (aSource^.TypSort=ikError) then exit;
 if not SchIsSubsetOf(fTarget^.LowerCluster,aSource^.UpperCluster) then exit;
 lFConstSubst.CopyList(gFConstSubst);
 lFFuncSubst.CopyBinIntFunc(gFFuncSubst);
 lFPredSubst.CopyBinIntFunc(gFPredSubst);
 if SchCompTyp(fTarget,aSource) then
  begin SchWidennings:=true; exit end;
 if (fTarget^.TypSort = aSource^.TypSort) and
    (fTarget^.ModNr=aSource^.ModNr) then exit;
 RetriveSubst;
 with fTarget^ do
 case TypSort of
  ikTypMode:
   begin
    lTyp:=aSource^.Widening;
    if lTyp = nil then exit;
    wTyp:=fTarget^.WideningOf(lTyp);
    if wTyp<>nil then
    begin
     RetriveSubst;
     if SchCompTyp(fTarget,wTyp) then
      begin dispose(wTyp,Done);
       SchWidennings:=true;
       exit
      end;
     if (fTarget^.TypSort = ikTypMode) and (OriginalNr( coMode,fTarget^.ModNr) = 0) then
      repeat
       if (wTyp^.TypSort=ikTypMode) and (wTyp^.ModNr=gBuiltIn[rqAny]) then
        begin dispose(wTyp,Done); exit end;
       lTyp:=wTyp^.Widening;
       dispose(wTyp,Done);
       wTyp:=fTarget^.WideningOf(lTyp);
       if wTyp <> nil then
        begin
         RetriveSubst;
         if SchCompTyp(fTarget,wTyp) then
          begin dispose(wTyp,Done);
           SchWidennings:=true; exit
          end;
        end;
      until wTyp = nil
     else dispose(wTyp,Done);
    end;
   end;
  ikTypStruct:
   begin
    lTyp:=aSource^.WidenToStruct;
    if lTyp = nil then exit;
    if ModNr=lTyp^.ModNr then
     begin
      if SchCompTyp(fTarget,lTyp) then
        SchWidennings:=true;
      dispose(lTyp,Done);
      exit
     end;
    RetriveSubst;
    with StructConstrPtr( Constr[ coStructMode].Items^[ModNr])^ do
    if (fFields^.Count > 0) and
        fFields^.IsSubsetOf(
         StructConstrPtr( Constr[ coStructMode].Items^[lTyp^.ModNr])^.fFields^)
    then
     begin
      SchWidennings:=true;
      gWidStruct.Init(0,5);
      gWidStructFound:=false; gTargetStructNr:=ModNr;
      WidenningPath(lTyp^.ModNr);
      if gWidStructFound then
       begin
        with gWidStruct do
         for z:=0 to Count-1 do
          begin
           wTyp:=TypPtr(Items^[z])^.InstTyp(lTyp^.ModArgs);
           dispose(lTyp,Done); lTyp:=wTyp;
          end;
        if ModNr=lTyp^.ModNr then
         begin
          if SchCompTyp(fTarget,lTyp) then
           SchWidennings:=true;
         end;
       end;
      gWidStruct.DeleteAll; gWidStruct.Done;
     end;
    dispose(lTyp,Done);
   end;
 end;
end;

function SchEqTrm(fTrm1,fTrm2:TrmPtr):boolean;
  var FuncNr1,FuncNr2,i,lBoundVarNbr: integer;
      A1,A2: TrmList;
      fTyp,wTyp:TypPtr;
      lTrm: TrmPtr;
begin SchEqTrm:=false;
 if TrmPtr(fTrm1)^.TrmSort = ikTrmSchFunc then
  with FuncTrmPtr(fTrm1)^ do
   begin
    if FuncArgs = nil then
     begin
      lTrm:=CopyTerm(fTrm2);
      ThereAreBound:=false;
      WithinTerm(lTrm,SchRenBound);
      if gFConstSubst.Items^[FuncNr] <> nil then
       begin
        if EqTrm(gFConstSubst.Items^[FuncNr],lTrm) then
         SchEqTrm:=true
        else gSchErrorNr:=25;
        dispose(lTrm,Done);
       end
      else
       begin
        if ThereAreBound then
         begin dispose(lTrm,Done);
          gSchErrorNr:=27;
         end
        else
         begin wTyp:=GetTrmType(lTrm);
          if SchWidennings(wTyp,gCurSchTypes.Items^[FuncNr-1]) then
           begin
            gFConstSubst.Items^[FuncNr]:=lTrm;
            SchEqTrm:=true;
           end
          else gSchErrorNr:=26;
          dispose(wTyp,Done);
         end;
       end;
     end
    else if fTrm2^.TrmSort = ikTrmInfConst then
     begin
      lTrm:=CopyTerm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm2)^.VarNr])^.fDef);
      gBoundBase:=BoundVarNbr;
      WithInTerm(lTrm,FrRenBound);
      SchEqTrm:=SchEqTrm(fTrm1,lTrm);
      DisposeTrm(lTrm);
     end
    else
     begin
      ThereAreBound:=false;
      WithinTerm(fTrm2,SchChkBound);
{      if ThereAreBound and (gFConstSubst.Items^[FuncNr] <> nil) then
       begin
        lTrm:=gFConstSubst.Items^[FuncNr];
        gFConstSubst.Items^[FuncNr]:=nil;
        if not SchEqTrm(fTrm1,lTrm) then
          gSchErrorNr:=33;
         dispose(lTrm,Done);
         exit;
       end;}
      case fTrm2^.TrmSort of
      ikTrmFunctor,ikTrmPrivFunc,ikTrmSchFunc:
       begin
        if gFFuncSubst.HasInDom(Ord(fTrm2^.TrmSort),FuncNr) then
         begin
          if gFFuncSubst.Value(Ord(fTrm2^.TrmSort),FuncNr) = FuncTrmPtr(fTrm2)^.FuncNr then
           if SchEqTrmList(FuncArgs,FuncTrmPtr(fTrm2)^.FuncArgs) then
            SchEqTrm:=true
           else gSchErrorNr:=28
          else gSchErrorNr:=29
         end
        else
         begin
          wTyp:=GetTrmType(fTrm2);
          if SchWidennings(wTyp,gCurSchTypes.Items^[FuncNr-1]) then
           begin
             case fTrm2^.TrmSort of
             ikTrmPrivFunc:
              if gFFuncSubst.HasInDom(Ord(ikTrmFunctor),FuncNr) or
                 gFFuncSubst.HasInDom(Ord(ikTrmSchFunc),FuncNr) then
               begin
                gSchErrorNr:=32;
                exit;
               end;
             ikTrmSchFunc:
              if gFFuncSubst.HasInDom(Ord(ikTrmFunctor),FuncNr) or
                 gFFuncSubst.HasInDom(Ord(ikTrmPrivFunc),FuncNr) then
               begin
                gSchErrorNr:=32;
                exit;
               end;
             ikTrmFunctor:
              if gFFuncSubst.HasInDom(Ord(ikTrmSchFunc),FuncNr) or
                 gFFuncSubst.HasInDom(Ord(ikTrmPrivFunc),FuncNr) then
               begin
                gSchErrorNr:=32;
                exit;
               end;
             else RunTimeError(2452);
            end;
            gFFuncSubst.Assign(Ord(fTrm2^.TrmSort),FuncNr,FuncTrmPtr(fTrm2)^.FuncNr);
            if SchEqTrmList(FuncArgs,FuncTrmPtr(fTrm2)^.FuncArgs) then
             SchEqTrm:=true
            else gSchErrorNr:=28
           end
          else gSchErrorNr:=30;
          dispose(wTyp,Done);
        end;
       end
      else gSchErrorNr:=21;
      end;
     end;
   end
 else
  with TrmPtr(fTrm2)^ do
  if TrmSort=TrmPtr(fTrm1)^.TrmSort then
   case TrmSort of
    ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmEqConst,
    ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral:
     with VarTrmPtr(fTrm2)^ do
      SchEqTrm:=VarTrmPtr(fTrm1)^.VarNr=VarNr;
    ikTrmInfConst:
     with VarTrmPtr(fTrm2)^ do
     begin
      if VarTrmPtr(fTrm1)^.VarNr = VarNr then begin SchEqTrm:=true; exit end;
      SchEqTrm:=SchEqTrm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm1)^.VarNr])^.fDef,
                   ConstDefPtr(InferConstDef.Items^[VarNr])^.fDef);
     end;
    ikTrmFunctor:
     begin AdjustTrm(fTrm1,FuncNr1,A1); AdjustTrm(fTrm2,FuncNr2,A2);
      if FuncNr1=FuncNr2 then SchEqTrm:=SchEqTrmList(A1,A2);
     end;
    ikTrmAggreg,ikTrmSelector:
     with FuncTrmPtr(fTrm2)^ do
      if FuncTrmPtr(fTrm1)^.FuncNr=FuncNr then
       SchEqTrm:=SchEqTrmList(FuncTrmPtr(fTrm1)^.FuncArgs,FuncArgs);
    ikTrmFraenkel:
     with FraenkelTrmPtr(fTrm2)^ do
      if FraenkelTrmPtr(fTrm1)^.LambdaArgs.Count = LambdaArgs.Count then
       begin
        MarkTermsInTTColl;
        lBoundVarNbr:=BoundVarNbr;
        for i:=0 to LambdaArgs.Count-1 do
         begin
          inc(BoundVarNbr);
          if not SchEqTyp(FraenkelTrmPtr(fTrm1)^.LambdaArgs.Items^[i],LambdaArgs.Items^[i]) then
           begin BoundVarNbr:=lBoundVarNbr;
            RemoveTermsFromTTColl;
            exit;
           end;
          BoundVar[BoundVarNbr]:=LambdaArgs.Items^[i];
         end;
        if SchEqTrm(FraenkelTrmPtr(fTrm1)^.LambdaScope,LambdaScope) then
         SchEqTrm:=SchEqFrm(FraenkelTrmPtr(fTrm1)^.Compr,Compr);
        BoundVarNbr:=lBoundVarNbr;
        RemoveTermsFromTTColl;
       end;
    ikTrmChoice:
     with ChoiceTrmPtr(fTrm2)^ do
      SchEqTrm:=SchEqTyp(ChoiceTrmPtr(fTrm1)^.ChoiceTyp,ChoiceTyp);
    ikTrmIt,ikError: SchEqTrm:=true;
    else RunTimeError(2450);
   end
  else
   if VarTrmPtr(fTrm1)^.TrmSort = ikTrmInfConst then
    begin
     lTrm:=CopyTerm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm1)^.VarNr])^.fDef);
     gBoundBase:=BoundVarNbr;
     WithInTerm(lTrm,FrRenBound);
     SchEqTrm:=SchEqTrm(lTrm,fTrm2);
     DisposeTrm(lTrm);
    end
  else
   if TrmSort = ikTrmInfConst then
    begin
     lTrm:=CopyTerm(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(fTrm2)^.VarNr])^.fDef);
     gBoundBase:=BoundVarNbr;
     WithInTerm(lTrm,FrRenBound);
     SchEqTrm:=SchEqTrm(fTrm1,lTrm);
     DisposeTrm(lTrm);
    end;
end;

(* ##RNC:
## Instantions of scheme functors and predicates.
## and predicates. Scheme functors can be instantiated 
## to other functors or terms (if zero arity). Scheme predicates   
## can be instantiated to other predicates   
elSchemeInstantiation =
 element elSchemeInstantiation {
   Position,
   element elFuncInstance {
     attribute atInstNr { xsd:integer },
     ((attribute atKind { "F" | "H" | "G" | "K" | "U" },
       attribute atNr { xsd:integer },
       ( attribute atAbsNr { xsd:integer },
         attribute atAid { xsd:string } )? )
       | Term )
   }*,
   element elPredInstance {
     attribute atInstNr { xsd:integer },
     attribute atKind { "P" | "S" | "V" | "R" },
     attribute atNr { xsd:integer },
     ( attribute atAbsNr { xsd:integer },
       attribute atAid { xsd:string } )?
   }*
 }
*)
procedure Schematize(const fSch:SchRefItem; const aSntList:MList);
 var lSntNr,ii: integer;
begin
{$IFDEF MDEBUG}
writeln(infofile,'------- Schematyzacja: ', Curpos.Line);
{$ENDIF};
 MarkTermsInTTColl;
 BoundVarNbr:=0;
 move(fSch.SchTypes,gCurSchTypes,SizeOf(MCollection));
 gFConstSubst.Init(MaxSchFuncNbr+1);
 for ii:=0 to MaxSchFuncNbr do gFConstSubst.Items^[ii]:=nil;
 gFFuncSubst.Init(MaxSchFuncNbr);
 gFPredSubst.Init(MaxSchPredNbr);
 gSchErrorNr:=20;
 for lSntNr:=0 to fSch.SchProps.Count-1 do
  begin
   if not SchEqFrm(FrmPtr(fSch.SchProps.Items^[lSntNr]),
                   FrmPtr(aSntList.Items^[lSntNr])) then
    begin
     if lSntNr = 0
      then Error(CurPos,gSchErrorNr)
      else Error(RefPos[lSntNr],gSchErrorNr);
{$IFDEF MDEBUG}
writeln(infofile,'****** (',gSchErrorNr,') nieudana schematyzacja: ', Curpos.Line);
writeln(infofile,' nierowne zdania: ',lSntNr);
Infoformula(FrmPtr(fSch.SchProps.Items^[lSntNr])); infonewline;
writeln(infofile,'zdanie 2');
Infoformula(FrmPtr(aSntList.Items^[lSntNr])); infonewline;
writeln(infofile,'*****************************');
{$ENDIF};
     RemoveTermsFromTTColl;
     exit;
    end;
  end;

{$IFDEF MDEBUG}
writeln(infofile,'--------- OK OK OK schematyzacja', Curpos.Line);
writeln(infofile,'Stale: ');
for ii:=0 to MaxSchFuncNbr do
if gFConstSubst.Items^[ii]<> nil then
begin write(infofile,'const[',ii,']=');
Infoterm(gFConstSubst.Items^[ii]); infonewline;
end;
writeln(infofile,'Funkcje: ',gFFuncSubst.fCount);
InfoBinIntFunc(gFFuncSubst); infonewline;
writeln(infofile,'Predykaty: ',gFPredSubst.fCount);
InfoBinIntFunc(gFPredSubst); infonewline;
{$ENDIF};
{$IFDEF SCH_REPORT}
with SchReport do
begin
 Out_XElStart( elSchemeInstantiation);
 Out_PosAsAttrs( CurPos);
 Out_XAttrEnd;
 for ii:=0 to MaxSchFuncNbr do
  if gFConstSubst.Items^[ii]<> nil then
  begin
   Out_XElStart( elFuncInstance);
   Out_XIntAttr( atInstNr, ii);
   Out_XAttrEnd;
   Out_Term(gFConstSubst.Items^[ii]);
   Out_XElEnd( elFuncInstance);
  end;
 with gFFuncSubst do
  for ii:=0 to fCount-1 do
   with fList^[ii] do
  begin
   Out_XElStart( elFuncInstance);
   Out_XIntAttr( atInstNr, X2);
   Out_XAttr( atKind, char(X1));
   Out_XIntAttr( atNr, Y);
   Out_XElEnd0;
  end;
 with gFPredSubst do
  for ii:=0 to fCount-1 do
   with fList^[ii] do
  begin
   Out_XElStart( elPredInstance);
   Out_XIntAttr( atInstNr, X2);
   Out_XAttr( atKind, char(X1));
   Out_XIntAttr( atNr, Y);
   Out_XElEnd0;
  end;
 Out_XElEnd( elSchemeInstantiation);
end;
{$ENDIF};
 gFConstSubst.Done;
 gFFuncSubst.Done;
 gFPredSubst.Done;
 RemoveTermsFromTTColl;
end;

end.
