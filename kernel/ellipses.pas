(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit ellipses;

interface

uses errhan,inout,limits,correl,mobjects,express,enums,identify;

type PP = ^TrmPtr;
type
   PTermPair = ^TTermPair;
   TTermPair = object(MCollection)
      nTerm1, nTerm2: PP;
      nArgs: PCollection;
      nPredPair: PTermPair;
      constructor Init(aTerm1,aTerm2:PP;aPredPair:PTermPair);
   end;

   function ProcessEllipses(fFrm1,fFrm2:FrmPtr; fConj:boolean):FrmPtr;
   
   {
   function EllipsesEqTrm(fTrm1,fTrm2:TrmPtr):boolean;
   }
   function EllipsesEqTyp(fTyp1,fTyp2:TypPtr):boolean;
   function EllipsesEqFrm(fFrm1,fFrm2:FrmPtr):boolean;
   
   function ExpandFlexAsTrue(fFrm:FrmPtr):FrmPtr;
   
implementation

uses lexicon,iocorrel,builtin,roundcl,formats,numbers
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF}
{$IFDEF DEBUG_ELLIPSES}{$IFNDEF MDEBUG} ,info,outinfo {$ENDIF}{$ENDIF}
;

var StartCollection: PCollection; // of PTermPair
var PathsCollection,ResultsCollection: PCollection; // of PTermPair
   
constructor TTermPair.Init(aTerm1,aTerm2:PP;aPredPair:PTermPair);
begin
 nTerm1:=aTerm1;
 nTerm2:=aTerm2;
 nArgs:=nil;
 nPredPair:=aPredPair;
end;

procedure InsertTermsToStartCollection(fTrmList1,fTrmList2:TrmList);
var A1,A2:TrmList;
begin
 A1:=fTrmList1;
 A2:=fTrmList2;
 while A1 <> nil do
  begin
   StartCollection^.Insert(new(PTermPair,Init(@(A1^.XTrmPtr),@(A2^.XTrmPtr),nil)));
   A1:=A1^.NextTrm;
   A2:=A2^.NextTrm;
  end;
end;

procedure CollectTermsFromAdjectives(fAdj1,fAdj2:AttrPtr);
begin
 if (fAdj1^.fAttrNr = fAdj2^.fAttrNr) and (fAdj1^.fNeg = fAdj2^.fNeg)
 then InsertTermsToStartCollection(fAdj1^.fAttrArgs,fAdj2^.fAttrArgs);
end;

procedure CollectTermsFromTypes(fTyp1,fTyp2:TypPtr);
var i:integer;
begin   
 if fTyp1^.TypSort = fTyp2^.TypSort then
  begin
   case fTyp1^.TypSort of
    ikTypMode,ikTypStruct:
     if fTyp1^.ModNr = fTyp2^.ModNr// and EqualClusters(fTyp1,fTyp2,StrictEqAttr)
     then begin
      InsertTermsToStartCollection(fTyp1^.ModArgs,fTyp2^.ModArgs);
      for i:=0 to fTyp1^.LowerCluster^.Count - 1 do
       CollectTermsFromAdjectives(fTyp1^.LowerCluster^.Items[i],fTyp2^.LowerCluster^.Items[i]);
      for i:=0 to fTyp1^.UpperCluster^.Count - 1 do
       CollectTermsFromAdjectives(fTyp1^.UpperCluster^.Items[i],fTyp2^.UpperCluster^.Items[i]);
     end;
   end;
  end;
end;

procedure CollectTermsFromFormulas(fFrm1,fFrm2:FrmPtr);
var k:integer;
begin
 if fFrm1^.FrmSort = fFrm2^.FrmSort then
  case fFrm1^.FrmSort of
   ikFrmNeg: CollectTermsFromFormulas(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmConj:
    for k:=0 to ConjFrmPtr(fFrm1)^.Conjuncts.Count-1 do
       CollectTermsFromFormulas(FrmPtr(ConjFrmPtr(fFrm1)^.Conjuncts.Items^[k]),
                                FrmPtr(ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k]));
   ikFrmQual:
    begin
     StartCollection^.Insert(new(PTermPair,
                                 Init(@(QualFrmPtr(fFrm1)^.QualTrm),@(QualFrmPtr(fFrm2)^.QualTrm),nil)));
     CollectTermsFromTypes(QualFrmPtr(fFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
    end;
   ikFrmPred,ikFrmSchPred,ikFrmAttr:
    InsertTermsToStartCollection(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
   ikFrmPrivPred:
    begin  
     InsertTermsToStartCollection(LocPredFrmPtr(fFrm1)^.PredArgs,LocPredFrmPtr(fFrm2)^.PredArgs);
     CollectTermsFromFormulas(LocPredFrmPtr(fFrm1)^.PredExp,LocPredFrmPtr(fFrm2)^.PredExp);
    end;
   ikFrmUniv:
    begin
     CollectTermsFromTypes(UnivFrmPtr(fFrm1)^.Quantified,UnivFrmPtr(fFrm2)^.Quantified);
     CollectTermsFromFormulas(UnivFrmPtr(fFrm1)^.Scope,UnivFrmPtr(fFrm2)^.Scope);
    end;
   ikFrmFlexConj:
    begin
     CollectTermsFromFormulas(FlexFrmPtr(fFrm1)^.nLeftOrigFrm,FlexFrmPtr(fFrm2)^.nLeftOrigFrm);
     CollectTermsFromFormulas(FlexFrmPtr(fFrm1)^.nRightOrigFrm,FlexFrmPtr(fFrm2)^.nRightOrigFrm);
    end;
   ikFrmVerum,ikFrmThesis,ikFrmError: ;
   else RunTimeError(2610);
  end;
end;

{$IFDEF DEBUG_ELLIPSES}
procedure PrintCollection(fColl:PCollection);
var i:integer;
var lPair:PTermPair;
begin
 if fColl <> nil then
  for i:=0 to fColl^.Count-1 do
   begin
    lPair:=PTermPair(fColl^.At(i));
    InfoTerm(lPair^.nTerm1^);
    InfoString(' -> ');
    InfoTerm(lPair^.nTerm2^);
    InfoString(' <-- ');
    if lPair^.nPredPair <> nil then InfoTerm(lPair^.nPredPair^.nTerm1^);
    InfoNewLine;
    PrintCollection(lPair^.nArgs);
   end;
end;
{$ENDIF}

procedure PrepareSubtermsCollection(fPair:PTermPair);
var lTrm1,lTrm2:TrmPtr;
var A1,A2:TrmList;
var i:integer;
begin
 lTrm1:=fPair^.nTerm1^;
 lTrm2:=fPair^.nTerm2^;
 case lTrm1^.TrmSort of
  ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmEqConst,ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral: ;
  ikTrmFraenkel: ;
  ikTrmChoice:
   if lTrm1^.TrmSort = lTrm2^.TrmSort then
    if ChoiceTrmPtr(lTrm1)^.ChoiceTyp^.ModNr = ChoiceTrmPtr(lTrm2)^.ChoiceTyp^.ModNr then
     begin // klastry opracowac
      fPair^.nArgs:=new(PCollection,Init(8,8));
      A1:=ChoiceTrmPtr(lTrm1)^.ChoiceTyp^.ModArgs;
      A2:=ChoiceTrmPtr(lTrm2)^.ChoiceTyp^.ModArgs;
      while A1 <> nil do
       begin
        fPair^.nArgs^.Insert(new(PTermPair,Init(@(A1^.XTrmPtr),@(A2^.XTrmPtr),fPair)));
        A1:=A1^.NextTrm;
        A2:=A2^.NextTrm;
       end;
      for i:=0 to fPair^.nArgs^.Count-1 do
       PrepareSubtermsCollection(PTermPair(fPair^.nArgs^.At(i)));               
     end;
  ikTrmIt,ikError: ;
  ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmSelector,ikTrmFunctor:
   begin
    if lTrm1^.TrmSort = lTrm2^.TrmSort then
     if FuncTrmPtr(lTrm1)^.FuncNr = FuncTrmPtr(lTrm2)^.FuncNr then
      begin
       fPair^.nArgs:=new(PCollection,Init(8,8));
       A1:=FuncTrmPtr(lTrm1)^.FuncArgs;
       A2:=FuncTrmPtr(lTrm2)^.FuncArgs;
       while A1 <> nil do
        begin
         fPair^.nArgs^.Insert(new(PTermPair,Init(@(A1^.XTrmPtr),@(A2^.XTrmPtr),fPair)));
         A1:=A1^.NextTrm;
         A2:=A2^.NextTrm;
        end;
       for i:=0 to fPair^.nArgs^.Count-1 do
        PrepareSubtermsCollection(PTermPair(fPair^.nArgs^.At(i)));
      end;                     
   end;
 else RunTimeError(2150);
 end;
end;

procedure PreparePathsCollection(fColl:PCollection);
var i:integer;
var lPair:PTermPair;
begin
 if fColl <> nil then
  for i:=0 to fColl^.Count-1 do
   begin
    lPair:=PTermPair(fColl^.At(i));
    if lPair^.nArgs <> nil then PreparePathsCollection(lPair^.nArgs)
    else if not StrictEqTrm(lPair^.nTerm1^,lPair^.nTerm2^) then PathsCollection^.Insert(lPair);
   end;
end;

procedure PrepareStructure;
var i:integer;
begin
 for i:=0 to StartCollection^.Count-1 do
  PrepareSubtermsCollection(PTermPair(StartCollection^.At(i)));
 PathsCollection:=new(PCollection,Init(64,64)); { to store leaves }
 PreparePathsCollection(StartCollection);
 ResultsCollection:=new(PCollection,Init(64,64));
 ResultsCollection^.Count:=PathsCollection^.Count;
end;

{
function EllipsesEqTrmList(fTL1,fTL2:TrmList):boolean;
begin
 EllipsesEqTrmList:=EquateTrmsLists(fTL1,fTL2,StrictEqTrm);
end;

function EllipsesEqTrm(fTrm1,fTrm2:TrmPtr):boolean;
  var i: integer;
begin EllipsesEqTrm:=false;
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
      EllipsesEqTrm:=VarTrmPtr(fTrm1)^.VarNr=VarNr;
    ikTrmSchFunc,ikTrmAggreg,ikTrmPrivFunc,ikTrmSelector,ikTrmFunctor:
     with FuncTrmPtr(fTrm2)^ do
      if FuncTrmPtr(fTrm1)^.FuncNr=FuncNr then
       EllipsesEqTrm:=EllipsesEqTrmList(FuncTrmPtr(fTrm1)^.FuncArgs,FuncArgs);
    ikTrmFraenkel:
     with FraenkelTrmPtr(fTrm2)^ do
      if FraenkelTrmPtr(fTrm1)^.LambdaArgs.Count = LambdaArgs.Count then
       begin
        for i:=0 to LambdaArgs.Count-1 do
         if not EqTyp(LambdaArgs.Items^[i],FraenkelTrmPtr(fTrm1)^.LambdaArgs.Items^[i]) then
          exit;
        if EllipsesEqTrm(FraenkelTrmPtr(fTrm1)^.LambdaScope,LambdaScope) then
         EllipsesEqTrm:=EqFrm(FraenkelTrmPtr(fTrm1)^.Compr,Compr);
       end;
    ikTrmChoice:
     with ChoiceTrmPtr(fTrm2)^ do
      EllipsesEqTrm:=EqTyp(ChoiceTrmPtr(fTrm1)^.ChoiceTyp,ChoiceTyp);
    ikTrmIt,ikError: EllipsesEqTrm:=true;
    else RunTimeError(2050);
   end;
end;
}
   
function EllipsesEqAttr(fAttr1,fAttr2:AttrPtr):boolean;
begin
 EllipsesEqAttr:=(fAttr1^.fAttrNr = fAttr2^.fAttrNr) and
    (fAttr1^.fNeg = fAttr2^.fNeg);// and EllipsesEqTrmList(fAttr1^.fAttrArgs,fAttr2^.fAttrArgs);
end;

function EllipsesEqTyp(fTyp1,fTyp2:TypPtr):boolean;
begin EllipsesEqTyp:=false;
 with TypPtr(FTyp1)^ do
  if TypSort=TypPtr(fTyp2)^.TypSort then
   case TypSort of
    ikTypMode,ikTypStruct:
     if (ModNr=TypPtr(fTyp2)^.ModNr) and EqualClusters(TypPtr(fTyp1),TypPtr(fTyp2),EllipsesEqAttr)
     then EllipsesEqTyp:=true;//EqTrmList(ModArgs,TypPtr(fTyp2)^.ModArgs);
    ikError: EllipsesEqTyp:=true;
   else
    RunTimeError(2004);
   end;
end;

function EllipsesEqFrm(fFrm1,fFrm2:FrmPtr):boolean;
 var PredNr1,PredNr2,k,l: integer; A1,A2: TrmList;
     Left1,Right1,Left2,Right2: TrmPtr;
     lFrm: FrmPtr;
     lConj1,lConj2: MList;
begin EllipsesEqFrm:=false;
 SkipLocPred(fFrm1); SkipLocPred(fFrm2);
 if fFrm1^.FrmSort=fFrm2^.FrmSort then
  case fFrm1^.FrmSort of
   ikFrmVerum,ikFrmThesis: EllipsesEqFrm:=true;
   ikFrmNeg: EllipsesEqFrm:=EllipsesEqFrm(NegFrmPtr(fFrm1)^.NegArg,NegFrmPtr(fFrm2)^.NegArg);
   ikFrmQual:
//    if EllipsesEqTrm(QualFrmPtr(fFrm1)^.QualTrm,QualFrmPtr(fFrm2)^.QualTrm) then
     EllipsesEqFrm:=EllipsesEqTyp(QualFrmPtr(fFrm1)^.QualTyp,QualFrmPtr(fFrm2)^.QualTyp);
   ikFrmConj:
    with ConjFrmPtr(fFrm1)^.Conjuncts do
     if Count = ConjFrmPtr(fFrm2)^.Conjuncts.Count then
      begin
       for k:=0 to Count-1 do
        if not EllipsesEqFrm(FrmPtr(Items^[k]),FrmPtr(ConjFrmPtr(fFrm2)^.Conjuncts.Items^[k])) then
          exit;
       EllipsesEqFrm:=true;
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
          if not EllipsesEqFrm(FrmPtr(lConj1.Items^[k]),FrmPtr(lConj2.Items^[k])) then
           begin
            lConj1.DeleteAll; lConj1.Done;
            lConj2.DeleteAll; lConj2.Done;
            exit;
           end;
         EllipsesEqFrm:=true;
        end;
       lConj1.DeleteAll; lConj1.Done;
       lConj2.DeleteAll; lConj2.Done;
      end;
   ikFrmSchPred,ikFrmPrivPred:
    if PredFrmPtr(fFrm1)^.PredNr = PredFrmPtr(fFrm2)^.PredNr then
     EllipsesEqFrm:=true;//EllipsesEqTrmList(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
   ikFrmAttr:
    begin
//       EllipsesEqFrm:= PredFrmPtr(fFrm1)^.PredNr = PredFrmPtr(fFrm2)^.PredNr;
     if PredFrmPtr(fFrm1)^.PredNr = PredFrmPtr(fFrm2)^.PredNr then
      EllipsesEqFrm:=true;//EqTrmList(PredFrmPtr(fFrm1)^.PredArgs,PredFrmPtr(fFrm2)^.PredArgs);
    end;
   ikFrmPred:
    begin
     PredNr1:=PredFrmPtr(fFrm1)^.PredNr;
     PredNr2:=PredFrmPtr(fFrm2)^.PredNr;
     A1:=PredFrmPtr(fFrm1)^.PredArgs;
     A2:=PredFrmPtr(fFrm2)^.PredArgs;
     if PredNr1=PredNr2 then
      if PredNr1=gBuiltIn[rqEqualsTo] then
      begin
        Left1:=A1^.XTrmPtr;
        Right1:=A1^.NextTrm^.XTrmPtr;
        Left2:=A2^.XTrmPtr;
        Right2:=A2^.NextTrm^.XTrmPtr;
        EllipsesEqFrm:=true;// EllipsesEqTrm(Left1,Left2) and EllipsesEqTrm(Right1,Right2) or
//                EllipsesEqTrm(Right1,Left2) and EllipsesEqTrm(Left1,Right2);
       end
      else EllipsesEqFrm:=true;//EllipsesEqTrmList(A1,A2);
    end;
   ikFrmUniv:
    if EllipsesEqTyp(UnivFrmPtr(fFrm1)^.Quantified,UnivFrmPtr(fFrm2)^.Quantified) then
     EllipsesEqFrm:=EllipsesEqFrm(UnivFrmPtr(fFrm1)^.Scope,UnivFrmPtr(fFrm2)^.Scope);
   ikFrmFlexConj:
    EllipsesEqFrm:=
     EllipsesEqFrm(FlexFrmPtr(fFrm1)^.nLeftOrigFrm,FlexFrmPtr(fFrm2)^.nLeftOrigFrm) and
      EllipsesEqFrm(FlexFrmPtr(fFrm1)^.nRightOrigFrm,FlexFrmPtr(fFrm2)^.nRightOrigFrm);
   ikError: EllipsesEqFrm:=true;
   else RunTimeError(2047);
  end;
end;

function NewNaturalCluster:AttrCollectionPtr;
var lColl:AttrCollectionPtr;
begin
 lColl:=new(AttrCollectionPtr,Init(1,0));
 lColl.InsertAttr(gBuiltIn[rqNatural],1,nil);
 NewNaturalCluster:=lColl;
end;

function HasNaturalAdjective(fTrm:TrmPtr):boolean;
begin
 HasNaturalAdjective:=NewNaturalCluster^.IsSubsetOf(GetTrmType(fTrm)^.UpperCluster,StrictEqAttr);
end;

function ComparePairs(fPair1,fPair2:PTermPair):boolean;
begin 
 if not (HasNaturalAdjective(fPair1^.nTerm1^) and HasNaturalAdjective(fPair1^.nTerm2^) and
         HasNaturalAdjective(fPair2^.nTerm1^) and HasNaturalAdjective(fPair2^.nTerm2^) )
 then ComparePairs:=false
 else
  ComparePairs:=StrictEqTrm(fPair1^.nTerm1^,fPair2^.nTerm1^) and StrictEqTrm(fPair1^.nTerm2^,fPair2^.nTerm2^);
end;

function FindDifference:PTermPair;
var OK:boolean;
var a:integer;
var lPair0,lPair:PTermPair;
begin
 FindDifference:=nil;
 OK:=true;
 if PathsCollection^.Count = 0 then exit;
 lPair0:=PathsCollection^.At(0);
 ResultsCollection^.Items[0]:=lPair0;
 while lPair0 <> nil do
  begin
   for a:=1 to PathsCollection^.Count-1 do
    begin 
     OK:=false;
     lPair:=PathsCollection^.At(a); 
     while lPair <> nil do
     begin
      {$IFDEF DEBUG_ELLIPSES}
      InfoNewLine; InfoString('Compare'); InfoNewLine;
      InfoTerm(lPair^.nTerm1^);
      InfoString(' -> ');
      InfoTerm(lPair^.nTerm2^);
      {$ENDIF} 
      if ComparePairs(lPair0,lPair) then
       begin
        ResultsCollection^.Items[a]:=lPair;
        OK:=true;
        break;
       end;
      lPair:=lPair^.nPredPair;
     end;
     if not OK then break;
    end;
   if OK then break; // different differences
   lPair0:=lPair0^.nPredPair;
   ResultsCollection^.Items[0]:=lPair0;
  end;
 FindDifference:=lPair0;
end;

var gLociNr:integer; // number of variable A

procedure IncBoundVar(var fTrm: TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if TrmSort = ikTrmBound then
   if VarNr >= gLociNr then inc(VarNr);
end;

procedure LociToBoundVar(var fTrm: TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if TrmSort = ikTrmLocus then
   begin
    TrmSort:=ikTrmBound;
    VarNr:=gLociNr;
   end;
end;

{
procedure LociToBoundVarNew(var fTrm: TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  case TrmSort of
   ikTrmLocus:
    begin
     TrmSort:=ikTrmBound;
     VarNr:=gLociNr;
    end;
   ikTrmBound: if VarNr >= gLociNr then inc(VarNr);
  end;
end;
}

function ProcessEllipses(fFrm1,fFrm2:FrmPtr; fConj:boolean):FrmPtr;
var lPair:PTermPair;
var i:integer;
var lFrm1,lFrm2,lResult,lExpansion:FrmPtr;
var lTyp:TypPtr; // natural number
var lLeftGuardFrm,lRightGuardFrm:FrmPtr; // t1 <= i , i <= t2
var lLeftGuardTrm,lRightGuardTrm:TrmPtr; // t1 , t2
var lLeftList,lRightList:TrmList;
begin
 ProcessEllipses:=NewIncorFrm;
 {$IFDEF DEBUG_ELLIPSES}
 InfoNewLine;
 InfoString('Start ProcessEllipses: '); InfoNewLine;
 InfoString('fFrm1: '); InfoFormula(fFrm1); InfoNewLine;
 InfoString('fFrm2: '); InfoFormula(fFrm2); InfoNewLine;
 {$ENDIF}
 if (fFrm1^.FrmSort = ikFrmError) or (fFrm2^.FrmSort = ikFrmError) then
  begin
   Error(CurPos,280); // incorrect arguments
   exit;
  end;
 if gBuiltIn[rqNatural] = 0 then
  begin
   Error(CurPos,281); // requirements NUMERALS required
   exit;
  end;
 if gBuiltIn[rqLessOrEqual] = 0 then
  begin
   Error(CurPos,282); // requirements REAL required
   exit;
  end;
 lFrm1:=fFrm1.CopyFormula;
 lFrm2:=fFrm2.CopyFormula;
 if StrictEqFrm(lFrm1,lFrm2) then
  begin
//      ProcessEllipses:=fFrm1;
   Error(CurPos,283);
   exit;
  end;
 if not EllipsesEqFrm(lFrm1,lFrm2) then
  begin
   Error(CurPos,284); // different shapes of formulas
   exit;
  end;
 StartCollection:=new(PCollection,Init(8,8));
 CollectTermsFromFormulas(lFrm1,lFrm2);
 PrepareStructure;
 {$IFDEF DEBUG_ELLIPSES}
 InfoNewLine;
 InfoString('StartCollection:'); InfoNewLine;
 PrintCollection(StartCollection);
 InfoString('End StartCollection'); InfoNewLine;
 InfoString('Paths: '); InfoNewLine;
 for i:=0 to PathsCollection^.Count-1 do
  begin
   InfoTerm(PTermPair(PathsCollection^.At(i))^.nTerm1^);
   InfoString(' =>> ');
   InfoTerm(PTermPair(PathsCollection^.At(i))^.nTerm2^);
   InfoNewLine;
  end;
 InfoString('End Paths'); InfoNewLine;
 {$ENDIF}
 lPair:=FindDifference;
 {$IFDEF DEBUG_ELLIPSES}
 InfoNewLine; InfoString('Difference:'); InfoNewLine;
 if lPair <> nil then
  begin
   InfoTerm(lPair^.nTerm1^);
   InfoString(' # ');
   InfoTerm(lPair^.nTerm2^);
   InfoNewLine;
  end;
 InfoString('End Difference'); InfoNewLine;
 {$ENDIF}
 if lPair = nil then Error(CurPos,285) // too many differences (zla pozycja)
 else
  begin
   if not HasNaturalAdjective(lPair^.nTerm1^) then
    begin
     Error(CurPos,286); // natural missing in left quard
     exit;
    end
   else if not HasNaturalAdjective(lPair^.nTerm2^) then
    begin
     Error(CurPos,287); // natural missing in right quard
     exit;
    end;
   {$IFDEF DEBUG_ELLIPSES}
   InfoString('WithinTerm: '); InfoNewLine;
   InfoTerm(lPair^.nTerm1^); InfoNewLine;
   InfoTerm(lPair^.nTerm2^); InfoNewLine;
   {$ENDIF}      
   lLeftGuardTrm:=CopyTerm(lPair^.nTerm1^);
   lRightGuardTrm:=CopyTerm(lPair^.nTerm2^);
   
   gLociNr:=BoundVarNbr+1;
         
   for i:=0 to ResultsCollection^.Count - 1 do
    begin         
     PTermPair(ResultsCollection^.At(i))^.nTerm1^:=NewVarTrm(ikTrmLocus,gLociNr);
     PTermPair(ResultsCollection^.At(i))^.nTerm2^:=NewVarTrm(ikTrmLocus,gLociNr);
    end;
   {$IFDEF DEBUG_ELLIPSES}
   InfoNewLine;
   InfoString('End Renumbering:'); InfoNewLine;
   InfoString('KONIEC:'); InfoNewLine;
   InfoFormula(lFrm1); InfoNewLine;
   InfoFormula(lFrm2); InfoNewLine;
   InfoString('END KONIEC:'); InfoNewLine;
   {$ENDIF}
   
   /// generation of result
      
   {$IFDEF DEBUG_ELLIPSES}      
   InfoString(' lLeftGuardTrm przed: ');
   InfoTerm(lLeftGuardTrm); InfoNewLine;
   {$ENDIF}      
      
   {$IFDEF DEBUG_ELLIPSES}      
   InfoString(' lLeftGuardTrm po: ');
   InfoTerm(lLeftGuardTrm); InfoNewLine;
   InfoString(' lRightGuardTrm przed: ');
   InfoTerm(lRightGuardTrm); InfoNewLine;
   {$ENDIF}         
   
   {$IFDEF DEBUG_ELLIPSES}      
   InfoString(' lRightGuardTrm po: ');
   InfoTerm(lRightGuardTrm); InfoNewLine;
   {$ENDIF}      
   
   lTyp:=NewStandardTyp(ikTypMode,NewNaturalCluster,NewNaturalCluster,gBuiltIn[rqSetMode],nil);
   lTyp^.RoundUp;
   {$IFDEF DEBUG_ELLIPSES}      
   InfoString('New Type: '); InfoType(lTyp); InfoNewLine;
   {$ENDIF}      
   
   lLeftList:=NewTrmList(lLeftGuardTrm, NewTrmList(NewVarTrm(ikTrmLocus,gLociNr),nil)); // t1 <= i;
   lRightList:=NewTrmList(NewVarTrm(ikTrmLocus,gLociNr), NewTrmList(lRightGuardTrm,nil)); // i <= t2
   
   lLeftGuardFrm:=new(PredFrmPtr,Init(ikFrmPred,gBuiltIn[rqLessOrEqual],lLeftList));
   lRightGuardFrm:=new(PredFrmPtr,Init(ikFrmPred,gBuiltIn[rqLessOrEqual],lRightList));

   {$IFDEF DEBUG_ELLIPSES}
   InfoString('lLeftGuardFrm: ');
   InfoFormula(lLeftGuardFrm); InfoNewLine;
   InfoString('lRightGuardFrm: ');
   InfoFormula(lRightGuardFrm); InfoNewLine;
   {$ENDIF}      

   if fConj then
    begin 
     lExpansion:=NewExpansion(lTyp,lLeftGuardFrm^.CopyFormula,lRightGuardFrm^.CopyFormula,NewNeg(lFrm1));
     WithinFormula(lExpansion,IncBoundVar);
     WithinFormula(lExpansion,LociToBoundVar);
     lResult:=new(FlexFrmPtr,InitD(fFrm1,fFrm2,lExpansion,lLeftGuardTrm,lRightGuardTrm));
    end
   else
    begin 
     lExpansion:=NewExpansion(lTyp,lLeftGuardFrm^.CopyFormula,lRightGuardFrm^.CopyFormula,lFrm1);
     WithinFormula(lExpansion,IncBoundVar);
     WithinFormula(lExpansion,LociToBoundVar);
     lResult:=NewNeg(new(FlexFrmPtr,InitD(NewNeg(fFrm1),NewNeg(fFrm2),lExpansion,lLeftGuardTrm,lRightGuardTrm)));
    end;

   {$IFDEF DEBUG_ELLIPSES}      
   InfoNewLine;
   InfoString('ProcessEllipses: '); InfoNewLine;
   InfoFormula(lResult); InfoNewLine;
   {$ENDIF}      
   
   ProcessEllipses:=lResult;
  end;
 dispose(StartCollection,Done);
 //   dispose(PathsCollection,Done);   
end;

// for CHECKER
   
var gCurrentFlexConjunctNr:integer;
var gZeroFunctor:TrmPtr;

procedure ReplacePlaceHolderByConjunctNumber(var fTrm: TrmPtr);
begin
 if fTrm^.TrmSort = ikTrmBound then     
  with VarTrmPtr(fTrm)^ do
   if VarNr = 1 then
    begin
     if gCurrentFlexConjunctNr <> 0 then
      begin    
       TrmSort:=ikTrmNumeral;
       VarNr:=gCurrentFlexConjunctNr;
      end else
       fTrm:=CopyTerm(gZeroFunctor);
    end else Dec(VarNr);
end;

function ExpandFlex(fFrm:FrmPtr):FrmPtr;
var lConjFrmPtr,lScope:FrmPtr;
var z,lower,i:integer;
var FuncNr:integer; A1:TrmList;
begin
 MizAssert(2500, FFrm^.FrmSort = ikFrmFlexConj);
 lConjFrmPtr:=NewVerum;
 lower:=-1;
 with FlexFrmPtr(fFrm)^ do
  begin
   case nLeftTrm^.TrmSort of
    ikTrmNumeral: lower:=VarTrmPtr(nLeftTrm)^.VarNr;
    ikTrmFunctor:
     begin  
      gZeroFunctor:=CopyTerm(nLeftTrm);
      AdjustTrm(gZeroFunctor,FuncNr,A1);
      if FuncNr = gBuiltIn[rqZeroNumber] then lower:=0;
     end;
   end;
   if (lower >= 0) and (nRightTrm^.TrmSort = ikTrmNumeral) and
      (VarTrmPtr(nRightTrm)^.VarNr - lower <= 100) then
    begin
     for z:=0 to VarTrmPtr(nRightTrm)^.VarNr - lower do
      begin
       gCurrentFlexConjunctNr := lower + z;
       lScope:=NewVerum;
       with ConjFrmPtr(NegFrmPtr(UnivFrmPtr(nExpansion)^.Scope)^.NegArg)^.Conjuncts do
        for i:=2 to Count-1 do
         lScope:=NewConj(lScope,FrmPtr(Items^[i]).CopyFormula);
       WithinFormula(lScope,ReplacePlaceHolderByConjunctNumber);
       lConjFrmPtr:=NewConj(lConjFrmPtr,NewNeg(lScope));
      end;
    end;
  end;
 ExpandFlex:=lConjFrmPtr;
end;

function ExpandFlexAsFalse(fFrm:FrmPtr):FrmPtr; forward;

function ExpandFlexAsTrue(fFrm:FrmPtr):FrmPtr;
var lConjFrmPtr:FrmPtr;
var z:integer;
begin
 case fFrm^.FrmSort of
  ikFrmNeg: ExpandFlexAsTrue:=NewNeg(ExpandFlexAsFalse(NegFrmPtr(fFrm)^.NegArg));
  ikFrmConj: 
   with ConjFrmPtr(fFrm)^.Conjuncts do
    begin
     lConjFrmPtr:=NewVerum;
     for z:=0 to Count-1 do
      lConjFrmPtr:=NewConj(lConjFrmPtr,ExpandFlexAsTrue(ConjFrmPtr(Items^[z])));
     ExpandFlexAsTrue:=lConjFrmPtr;
    end;
  ikFrmFlexConj:
   begin
    lConjFrmPtr:=ExpandFlex(fFrm);
    if lConjFrmPtr^.FrmSort = ikFrmVerum
     then ExpandFlexAsTrue:=NewConj(CopyExpFrm(fFrm),CopyExpFrm(FlexFrmPtr(fFrm)^.nExpansion))
     else ExpandFlexAsTrue:=NewConj(CopyExpFrm(fFrm),NewConj(CopyExpFrm(FlexFrmPtr(fFrm)^.nExpansion),lConjFrmPtr));
   end;
  ikFrmVerum,ikFrmThesis,ikFrmQual,ikFrmUniv,ikFrmSchPred,ikFrmAttr,ikFrmPrivPred,ikFrmPred,ikError:
   ExpandFlexAsTrue:=CopyExpFrm(fFrm);
  else RunTimeError(2501);
 end;
end;

function ExpandFlexAsFalse(fFrm:FrmPtr):FrmPtr;
var lConjFrmPtr:FrmPtr;
var z:integer;
begin
 case fFrm^.FrmSort of
  ikFrmNeg: ExpandFlexAsFalse:=NewNeg(ExpandFlexAsTrue(NegFrmPtr(fFrm)^.NegArg));
  ikFrmConj: 
   with ConjFrmPtr(fFrm)^.Conjuncts do
    begin
     lConjFrmPtr:=NewVerum;
     for z:=0 to Count-1 do
      lConjFrmPtr:=NewConj(lConjFrmPtr,ExpandFlexAsFalse(ConjFrmPtr(Items^[z])));
     ExpandFlexAsFalse:=lConjFrmPtr;
    end;
  ikFrmFlexConj:
   begin
    lConjFrmPtr:=ExpandFlex(fFrm);
    if lConjFrmPtr^.FrmSort = ikFrmVerum
     then ExpandFlexAsFalse:=NewDisj(CopyExpFrm(fFrm),CopyExpFrm(FlexFrmPtr(fFrm)^.nExpansion))
     else ExpandFlexAsFalse:=NewDisj(CopyExpFrm(fFrm),NewDisj(CopyExpFrm(FlexFrmPtr(fFrm)^.nExpansion),lConjFrmPtr));
   end;
  ikFrmVerum,ikFrmThesis,ikFrmQual,ikFrmUniv,ikFrmSchPred,ikFrmAttr,ikFrmPrivPred,ikFrmPred,ikError:
   ExpandFlexAsFalse:=CopyExpFrm(fFrm);
  else RunTimeError(2502);
 end;
end;

end.
