(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit checker;

interface

uses mobjects,justhan;

var ChErrNr: NatSet;

procedure InferenceChecker(const aInference:RefSntArr; aInfNbr:integer; aVarNbr:integer);

implementation

uses errhan,builtin,lexicon,correl,enums,prechecker,equalizer,unifier,mscanner
{$IFDEF MINI_PROFILER},miniprof
//MiniProfiler.SectionBegin ('Equate');
//MiniProfiler.SectionEnd;
{$ENDIF}
{$IFDEF MDEBUG},info,outinfo{$ENDIF};

procedure ChError(E:integer);
begin
 ChErrNr.InsertElem(e);
end;

function ContradictoryAttrs(aClu1,aClu2:AttrCollectionPtr):boolean;
 var i,j: integer;
begin ContradictoryAttrs:=false;
 if (aClu1^.Count = 0) or (aClu2^.Count = 0)
  then exit;
 i:=0;
 j:=0;
 repeat
  case CompAbsAttr(aClu1^.Items^[i],aClu2^.Items^[j]) of
  -1:
   begin inc(i);
    continue
   end;
   0:
   begin
    if AttrPtr(aClu1^.Items^[i])^.fNeg <> AttrPtr(aClu2^.Items^[j])^.fNeg then
     begin ContradictoryAttrs:=true;
       exit
     end;
    inc(i);
   end;
   1:;
  end;
  inc(j);
 until (i >= aClu1^.Count) or (j >= aClu2^.Count);
end;

procedure AllocInequality(FTrm1,FTrm2:TrmPtr);
 var i,lTrmInfo1,lTrmInfo2: integer;
     Left,Right: TrmPtr;
begin
   for i:=0 to NegBas.Count-1 do
    with PredFrmPtr(NegBas.Items^[i])^ do
     if (FrmSort=ikFrmPred) and (PredNr = gBuiltIn[rqEqualsTo]) then
      begin Left:=PredArgs^.XTrmPtr;
       Right:=PredArgs^.NextTrm^.XTrmPtr;
       if (Left=FTrm1) and (Right=FTrm2) or (Left=FTrm2) and (Right=FTrm1)
        then exit;
      end;
   NegBas.Insert(NewEqFrm(FTrm1,FTrm2));
   lTrmInfo1:=fTrm1^.TrmInfo;
   lTrmInfo2:=fTrm2^.TrmInfo;
   InsertNonEmpty(lTrmInfo1,lTrmInfo2);
   InsertNonEmpty(lTrmInfo2,lTrmInfo1);
   InsertNonZero(lTrmInfo1,lTrmInfo2);
   InsertNonZero(lTrmInfo2,lTrmInfo1);
end;

function OneDiffInTrmLists(FTL1,FTL2:TrmList; var FTrm1,FTrm2:TrmPtr): boolean;
 var Diff: boolean;
begin Diff:=false;
   while FTL1<>nil do with FTL1^,XTrmPtr^ do
    begin
     if TrmInfo<>FTL2^.XTrmPtr^.TrmInfo then
      begin
       if Diff then
         begin OneDiffInTrmLists:=false;
          exit
         end;
       Diff:=true;
       FTrm1:=XTrmPtr;
       FTrm2:=FTL2^.XTrmPtr;
      end;
     FTL1:=NextTrm;
     FTL2:=FTL2^.NextTrm;
    end;
   OneDiffInTrmLists:=true;
   Mizassert(2577,Diff);
end;

procedure PreUnification;
 var ii,jj,j,z,zz,lPred,lPred2,lModNr1,lModNr2,lTrmInfo1,lTrmInfo2: integer;
     lArgs,LTL1,LTL2,lArgs2,A1,A2: TrmList;
     lLeftArg,lRightArg,LTrm1,LTrm2,lTrm3,lTrm4: TrmPtr;
     hQualTyp: TypPtr;
begin
   {            OneDiffRule;            }

  for ii:=0 to PosBas.Count-1 do
   if FrmPtr(PosBas.Items^[ii])^.FrmSort = ikFrmPred then
     with PredFrmPtr(PosBas.Items^[II])^,
          ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
     if syIrreflexivity in fProperties then
      begin
       GetArgs2(fFirstArg,fSecondArg,lLeftArg,lRightArg,PredArgs);
       lTrmInfo1:=lLeftArg^.TrmInfo;
       lTrmInfo2:=lRightArg^.TrmInfo;
       if lTrmInfo1 = lTrmInfo2 then
        begin
          SetContr(50);
          exit;
        end;
       AllocInequality(TrmS[lTrmInfo1].Term,TrmS[lTrmInfo2].Term);
      end;
   if Contr > 0 then exit;

  for ii:=0 to NegBas.Count-1 do
   if FrmPtr(NegBas.Items^[ii])^.FrmSort = ikFrmPred then
    with PredFrmPtr(NegBas.Items^[II])^,
         ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
     if syReflexivity in fProperties then
      begin
       GetArgs2(fFirstArg,fSecondArg,lLeftArg,lRightArg,PredArgs);
       lTrmInfo1:=lLeftArg^.TrmInfo;
       lTrmInfo2:=lRightArg^.TrmInfo;
       if lTrmInfo1 = lTrmInfo2 then
        begin
          SetContr(51);
          exit;
        end;
       AllocInequality(TrmS[lTrmInfo1].Term,TrmS[lTrmInfo2].Term);
      end;
   if Contr > 0 then exit;

  { One difference rule dla atrybutow}
{  for ii:= 1 to TrmNbr do
   if TrmS[ii].EqClass <> nil then
    for jj:= ii+1 to TrmNbr do
     if TrmS[jj].EqClass <> nil then
      if ContradictoryAttrs(TrmS[ii].SuperCluster,TrmS[jj].SuperCluster) then
        AllocInequality(TrmS[ii].Term,TrmS[jj].Term);
   if Contr > 0 then exit;}

  { One difference rule dla atrybutow }
  for ii:= 1 to TrmNbr do
   if TrmS[ii].EqClass <> nil then
    for jj:= ii+1 to TrmNbr do
     if TrmS[jj].EqClass <> nil then
      if ContradictoryAttrs(TrmS[ii].SuperCluster,TrmS[jj].SuperCluster) then
        AllocInequality(TrmS[ii].Term,TrmS[jj].Term);
   if Contr > 0 then exit;

  {   stara OneDiffRule;   }
   ii:=0;
   while ii < NegBas.Count do
     with FrmPtr(NegBas.Items^[ii])^ do
      begin
       case FrmSort of
        ikFrmPred:
         begin AdjustFrm(PredFrmPtr(NegBas.Items^[ii]),lPred,lArgs);
          { Brak symetrii dla Irreflexivity  !!!!!!!
            Chodzi o pozytywne zdanie z przeciwzwrotnym predykatem.
            Zdaje sie, ze mamy w bazie takiego przypadku, ale jezeli np.
            bedzie wprowadzona "irreflexivity" dla "#195#174", wtedy
            nie bedzie to po prostu przetwarzane !
          }
         with PredFrmPtr(NegBas.Items^[II])^,
              ConstrPtr( Constr[ coPredicate].At(PredNr))^ do
           begin
            if syReflexivity in fProperties then
             begin
              GetArgs2(fFirstArg,fSecondArg,lLeftArg,lRightArg,PredArgs);
              LTL1:=Trms[lLeftArg^.TrmInfo].EqClass;
              while LTL1<>nil do with FuncTrmPtr(LTL1^.XTrmPtr)^ do
               begin
                case TrmSort of
                 ikTrmFunctor,ikTrmSchFunc,ikTrmPrivFunc,ikTrmAggreg,ikTrmSelector:
                  begin
                   LTL2:=Trms[lRightArg^.TrmInfo].EqClass;
                   while LTL2<>nil do
                    begin
                     if TrmSort = LTL2^.XTrmPtr^.TrmSort then
                      if FuncNr = FuncTrmPtr(LTL2^.XTrmPtr)^.FuncNr then
                       if OneDiffInTrmLists(FuncArgs,FuncTrmPtr(LTL2^.XTrmPtr)^.FuncArgs,LTrm1,LTrm2)
                        then AllocInequality(LTrm1,LTrm2);
                     LTL2:=LTL2^.NextTrm;
                    end;
                  end;
                end;
                LTL1:=LTL1^.NextTrm;
               end;
             end;
           end;
          if lPred <> gBuiltIn[rqEqualsTo] then
           for j:=0 to PosBas.Count-1 do
            if FrmPtr(PosBas.Items^[j])^.FrmSort = FrmSort then
             begin AdjustFrm(PredFrmPtr(PosBas.Items^[j]),lPred2,lArgs2);
              if lPred2=lPred then
               if OneDiffInTrmLists(lArgs2,lArgs,LTrm1,LTrm2)
                then AllocInequality(LTrm1,LTrm2);
             end;
         end;
        ikFrmSchPred,ikFrmPrivPred,ikFrmAttr:
         begin
          for j:=0 to PosBas.Count-1 do
           if FrmPtr(PosBas.Items^[j])^.FrmSort = FrmSort then
            if PredFrmPtr(PosBas.Items^[j])^.PredNr = PredFrmPtr(NegBas.Items^[ii])^.PredNr then
             if OneDiffInTrmLists(PredFrmPtr(PosBas.Items^[j])^.PredArgs,PredFrmPtr(NegBas.Items^[ii])^.PredArgs,LTrm1,LTrm2)
             then AllocInequality(LTrm1,LTrm2);
         end;
        ikFrmQual:
         begin
          hQualTyp:=QualFrmPtr(NegBas.Items^[ii])^.QualTyp;
          with hQualTyp^ do
           begin
            for z:=0 to Trms[QualFrmPtr(NegBas.Items^[ii])^.QualTrm^.TrmInfo].XTypClass.Count-1 do
             with TypPtr(Trms[QualFrmPtr(NegBas.Items^[ii])^.QualTrm^.TrmInfo].XTypClass.Items^[z])^ do
             if TypSort = hQualTyp^.TypSort then
//             if EqualClusters(Trms[QualFrmPtr(NegBas.Items^[ii])^.QualTrm^.TrmInfo].XTypClass.Items^[z],hQualTyp,AttrEquals) then
             if hQualTyp^.LowerCluster^.IsSubsetOf(Trms[QualFrmPtr(NegBas.Items^[ii])^.QualTrm^.TrmInfo].SuperCluster,AttrEquals) then
              case TypSort of
              ikTypMode:
               begin
                AdjustTyp(lModNr1, A1);
                hQualTyp^.AdjustTyp(lModNr2, A2);
                if (lModNr1 = lModNr2) and OneDiffInTrmLists(A1,A2,lTrm3,lTrm4) then
                  AllocInequality(lTrm3,lTrm4);
               end;
              ikTypStruct:
//                if (ModNr = hQualTyp^.ModNr) and OneDiffInTrmLists(ModArgs,hQualTyp^.ModArgs,lTrm3,lTrm4) then
//                  AllocInequality(lTrm3,lTrm4);
              end;
             for j:=1 to TrmNbr do
              if j <> QualFrmPtr(NegBas.Items^[ii])^.QualTrm^.TrmInfo then
               with TrmS[j], XTypClass do
                if EqClass <> nil then
                  for zz:=0 to Count-1 do
                   if EqRadices(TypPtr(Items^[zz])) then
                    begin
                     AllocInequality(QualFrmPtr(NegBas.Items^[ii])^.QualTrm,Term);
                     break;
                    end;
           end;
         end;
       end;
       inc(ii);
      end;
end;

procedure InitUnifier;
 var ii: integer;
     e: ComplexTrmExprKind;
     lTL1,lTL2: TrmList;
begin
 for e := Low(ComplexTrmExprKind) to High(ComplexTrmExprKind) do
  begin
   setlength(EqList[e],EqClassNbr+1);
   for ii:=0 to EqClassNbr do EqList[e][ii]:=new(MListPtr,Init(0));
  end;

 DConstEqClass.InitNatFunc(0,TrmNbr);
 for ii:=1 to TrmNbr do with Trms[ii] do
  if EqClass<>nil then
   begin
     lTL1:=EqClass;
     while lTL1<> nil do
      with lTL1^ do
      begin
       lTL2:=NextTrm;
       case XTrmPtr^.TrmSort of
        ikTrmInfConst:
         begin
          DConstEqClass.Assign(VarTrmPtr(XTrmPtr)^.VarNr,VarTrmPtr(Term)^.VarNr);
          dispose(XTrmPtr); dispose(lTL1);
         end;
        ikTrmNumeral:
         begin
          dispose(XTrmPtr);
          dispose(lTL1)
         end;
        ikTrmFunctor:
          EqList[expTrmFunctor][VarTrmPtr(Term)^.VarNr]^.Insert(lTL1^.XTrmPtr);
        ikTrmSchFunc:
          EqList[expTrmSchFunc][VarTrmPtr(Term)^.VarNr]^.Insert(lTL1^.XTrmPtr);
        ikTrmPrivFunc:
          EqList[expTrmPrivFunc][VarTrmPtr(Term)^.VarNr]^.Insert(lTL1^.XTrmPtr);
        ikTrmAggreg:
          EqList[expTrmAggreg][VarTrmPtr(Term)^.VarNr]^.Insert(lTL1^.XTrmPtr);
        ikTrmSelector:
          EqList[expTrmSelector][VarTrmPtr(Term)^.VarNr]^.Insert(lTL1^.XTrmPtr);
        ikTrmFraenkel:
          EqList[expTrmFraenkel][VarTrmPtr(Term)^.VarNr]^.Insert(lTL1^.XTrmPtr);
        ikTrmChoice:
          EqList[expTrmChoice][VarTrmPtr(Term)^.VarNr]^.Insert(lTL1^.XTrmPtr);
       end;
       lTL1:=lTL2;
      end;
     EqClassVal[VarTrmPtr(Term)^.VarNr]:=NumValue;
     move(XTypClass,EqClassType[VarTrmPtr(Term)^.VarNr],SizeOf(XTypClass));
     EqClassSuperCluster[VarTrmPtr(Term)^.VarNr]:=SuperCluster;
   end;
 for ii:=1 to EqClassNbr do
  with ETrm[ii] do
   begin TrmSort:=ikTrmEqConst;
    VarNr:=ii;
    TrmInfo:=ii
   end;
end;

procedure DispUnifier;
 var ii: integer;
     e: ComplexTrmExprKind;
begin
  for e := Low(ComplexTrmExprKind) to High(ComplexTrmExprKind) do
  begin
    for ii:=1 to EqClassNbr do DisposeTrmMList(EqList[e][ii]);
  end;
  for ii:=1 to EqClassNbr do EqClassType[ii].Done;
end;

procedure InferenceChecker(const aInference:RefSntArr; aInfNbr:integer; aVarNbr:integer);
 var i,locInferConstNbr: integer;
     lNormalForm: PreInstCollection;
//     lBoundVarNbr:integer;
begin
 MarkTermsInTTColl;
 InferConstDef.Mark(locInferConstNbr);
 BoundVarNbr:=0;

 ConstOvfl:=false;
 gVarNbr:=aVarNbr;
 LatOvfl:=false;

 PreCheck(aInference,aInfNbr,lNormalForm);
{$IFDEF CHSTAT}
//if not LatOvfl then gStat.Up(NormalForm.Count);
{$ENDIF}

 if TrivialError then
  begin
{$IFDEF MDEBUG}
writeln(InfoFile,'*** 1 ***'); flush(InfoFile);
{$ENDIF}
   ChError(1);
  end
 else if LatOvfl then
  begin
{$IFDEF MDEBUG}
writeln(InfoFile,'*** 8 ***'); flush(InfoFile);
{$ENDIF}
    ChError(8);
  end
 else
  begin
// CollectAllConstInInference;
   for i:=0 to lNormalForm.Count-1 do
    begin
{$IFDEF MDEBUG}
writeln(infofile,'Disjunct: ',i);
InfoEval(NatFuncPtr(lNormalForm.Items^[i]),Basic);
{$ENDIF}
     ItIsChecker:=true;
//     lBoundVarNbr:=BoundVarNbr;
     BoundVarNbr:=0;
     Equate(NatFuncPtr(lNormalForm.Items^[i])^);
     if Contr > 0 then
      DispEqClassInTrms
     else
      begin
       PreUnification;
       if Contr > 0 then
        DispEqClassInTrms
       else
        begin
         InitUnifier;
         Unification;
         DispUnifier;
         if Contr = 0 then
         begin
{$IFDEF MDEBUG}
writeln(InfoFile,'*** 4 ***'); flush(InfoFile);
{$ENDIF}
          ChError(4);
         end;
        end;
      end;
     DispEquations;
//     BoundVarNbr:=lBoundVarNbr;
     ItIsChecker:=false;
    end;
   if LatOvfl then
    begin
{$IFDEF MDEBUG}
writeln(InfoFile,'*** 9 ***');
{$ENDIF}
     ChError(9);
    end;
   if TrmOvfl then
    begin
{$IFDEF MDEBUG}
writeln(InfoFile,'*** 14 ***'); flush(InfoFile);
{$ENDIF}
     ChError(14);
    end;
  end;
 if ConstOvfl then
  begin
{$IFDEF MDEBUG}
writeln(InfoFile,'*** 11 ***');
{$ENDIF}
    ChError(11);
  end;

{$IFDEF MDEBUG}
flush(InfoFile);
{$ENDIF} ;

 lNormalForm.Done;
 Basic.Done;
 for i:=aVarNbr+1 to gVarNbr do
  dispose(FixedVar[i].nTyp,Done);
 InferConstDef.FreeItemsFrom(locInferConstNbr);

 RemoveTermsFromTTColl;
end;

end.
