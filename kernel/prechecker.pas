(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit prechecker;

interface

uses mobjects,justhan,correl;

type
 TLatStatus = ( Top, Generator, Regular );

 ULCollection =
  object(MSortedCollection)
   function Compare(Key1,Key2:pointer): integer; virtual;
  end;

 PreInstCollectionPtr = ^PreInstCollection;
 PreInstCollection =
  object(MCollection)
    Status: TLatStatus;
    nOverflow: boolean;

   constructor Init(ALimit, ADelta: Integer);
   procedure InsertAndAbsorb(fElem:NatFuncPtr);
   procedure UnionWith(fColl:PreInstCollectionPtr);
   procedure JoinWith(fColl:PreInstCollectionPtr); virtual;
   procedure ReNew;
   constructor InitTop;
   constructor InitBottom;
   destructor Done; virtual;
   constructor InitSingle(fAtom:NatFuncPtr);
   constructor JoinWithAtom(fElem:NatFuncPtr; fColl:PreInstCollectionPtr);
   constructor JoinInstList(var aUnifColl:ULCollection);

   procedure ListError(aCode,aInfo:integer); virtual;

   procedure InsertEval(var fEval:NatFunc);
     { InitEval musi miec swiezo zbudowana funkcje, wykorzystywana jedynie
       do celow inicjacji, jak nie to trzeba ja skopiowac !
     }
    { konstruktory dla form normalnych }
   constructor SingleEval(var fEval:NatFunc);
     { SingleEval musi miec swiezo zbudowana funkcje, wykorzystywana jedynie
       do celow inicjacji, jak nie to trzeba ja skopiowac !
     }
   constructor NormalizeAsTrue(fFrm:FrmPtr);
   constructor NormalizeAsFalse(fFrm:FrmPtr);
{$IFDEF MDEBUG}
   procedure InfoLatColl;
{$ENDIF}
  end;

var
  InitVarBase: procedure;
  SetVarInTrm: procedure (var fTrm: TrmPtr);
  NewVariable: procedure (aTyp: TypPtr);

var
  ConstOvfl: boolean;
  LatOvfl: boolean;
  TrivialError: boolean;

  gVarNbr: integer;
  gLatStatus: TLatStatus=Regular;
  Basic: MCollection;
  gBasicPtr: PCollection;

function BasicFrmNr(aFrm:FrmPtr; const aBasic:MCollection): integer;

function DistributeQuantifiers(fFrmPtr:FrmPtr): FrmPtr;

procedure PreCheck(const aInference:RefSntArr; aInfNbr:integer;
                    var NormalForm:PreInstCollection);

{$IFDEF MDEBUG}
procedure infoeval(aEval:NatFuncPtr; const aBasic:MCollection);
procedure InfoInference(const aNormalForm:PreInstCollection; const aBasic:MCollection);
{$ENDIF}

implementation

uses errhan,lexicon,limits,inout,enums,identify,ellipses
{$IFDEF MDEBUG},info,outinfo{$ENDIF};

function ULCollection.Compare(Key1,Key2:pointer): integer;
begin Compare:=1;
 if PCollection(Key1)^.Count >= PCollection(Key2)^.Count then Compare:=-1;
end;

const
  EvalLimit = 4;
  EvalDelta = 4;

procedure PreInstCollection.ListError(aCode,aInfo:integer);
begin
{$IFDEF MDEBUG}
case aCode of
 coOverFlow: writeln(InfoFile,'InstCollection coOverFlow ',aInfo);
 coIndexError: writeln(InfoFile,'InstCollection coIndexError ',aInfo);
 else writeln(InfoFile,'UnKnown InstCollection Error',aInfo);
end;
writeln(InfoFile,'Count=',Count,' Limit=',Limit,' Delta=',Delta);
{writeln(InfoFile,'MemAvail=',MemAvail);}
{$ENDIF}
 nOverflow:=True;
 if aCode = coOverflow then
//  if Delta = 0 then
   begin
    Done;
    LatOvfl:=true; Count:=0;
//    if aInfo >= MaxInstNbr then begin LatOvfl:=true; Count:=0 end
//     else MCollection.ListError(aCode,aInfo);
   end
//  else begin ErrImm(16); Done end
 else inherited ListError(aCode,aInfo);
end;

{$IFDEF MDEBUG}
procedure InfoSubst(aSubst: NatFuncPtr);
  var k: integer;
begin
 with aSubst^ do
  begin
   for k:=0 to Count-1 do
    with Items^[k] do
    begin
     if k > 0 then
     write(infofile,', ');
     write(infofile,'X',X+1,'/','E',Y);
    end;
  end;
end;
procedure PreInstCollection.InfoLatColl;
  var z: integer;
begin writeln(InfoFile,'Count=',Count);
 for z:=0 to Count-1 do
  if Items = nil then break
  else
  begin
   write(InfoFile,z,': ');
   InfoSubst(Items^[z]);
   infonewline;
  end;
end;
{$ENDIF}

constructor PreInstCollection.Init(ALimit, ADelta: Integer);
begin
 inherited Init(ALimit, ADelta);
 nOverflow:=false;
end;

const NilPtr : pointer = nil;

constructor PreInstCollection.InitTop;
begin
 Items:=NilPtr;
 Count:=1;
 Limit:=1;
 Delta:=0;
 Status:=Top;
 nOverflow:=false;
end;

constructor PreInstCollection.InitBottom;
begin
 Items:=nil;
 Count:=0;
 Limit:=0;
 Delta:=0;
 Status:=Regular;
 nOverflow:=false;
end;

destructor PreInstCollection.Done;
begin
 if Status >= gLatStatus then
  MCollection.Done;
end;

constructor PreInstCollection.InitSingle(fAtom:NatFuncPtr);
begin
 Init(1,0);
 Count:=1;
 AtPut(0,fAtom);
 Status:=Regular;
 nOverflow:=false;
end;

constructor PreInstCollection.JoinWithAtom(fElem:NatFuncPtr; fColl:PreInstCollectionPtr);
 var z: integer;
     lAtom: NatFuncPtr;
begin Init(fColl^.Count,0);
 nOverflow:=false;
 for z:=0 to fColl^.Count-1 do
  begin lAtom:=NatFuncPtr(fColl^.Items^[z])^.JoinAtom(fElem);
   if lAtom <> nil then Insert(lAtom);
   if nOverflow then exit;
  end;
 SetLimit(Count);
 Status:=Regular;
end;

constructor PreInstCollection.JoinInstList(var aUnifColl:ULCollection);
 var z: integer;
     lInsts,lInsts1: PreInstCollectionPtr;
     lList: PreInstCollection;
     lUnifColl1: ULCollection;
     lObj: NatFuncPtr;
begin
 if aUnifColl.Count=0 then
  begin InitTop;
    aUnifColl.Done;
    exit
  end;
 while aUnifColl.Count > 1 do
  begin
   lInsts:=aUnifColl.Items^[aUnifColl.Count-1];
   dec(aUnifColl.Count);
   case lInsts^.Count of
    1:
     begin
      lUnifColl1.Init(aUnifColl.Count,0);
      lObj:=lInsts^.Items^[0];
      for z:=0 to aUnifColl.Count-1 do
       begin
        lList.JoinWithAtom(lObj,PreInstCollectionPtr(aUnifColl.Items^[z]));
        dispose(PreInstCollectionPtr(aUnifColl.Items^[z]),Done);
        lUnifColl1.Insert(lList.CopyObject);
       end;
      aUnifColl.DeleteAll; aUnifColl.Done;
      aUnifColl:=lUnifColl1;
     end
    else
     begin
      lInsts1:=aUnifColl.Items^[aUnifColl.Count-1];
      dec(aUnifColl.Count);
      lInsts1^.JoinWith(lInsts);
      if lInsts1^.nOverflow then
        begin
         dispose(lInsts, Done);
         dispose(lInsts1, Done);
         InitBottom;
         aUnifColl.Done;
         nOverflow:=true;
         exit
        end;
      aUnifColl.Insert(lInsts1);
     end;
   end;
   dispose(lInsts, Done);
  end;
 Self:=PreInstCollectionPtr(aUnifColl.Items^[0])^;
(* with PreInstCollectionPtr(aUnifColl.Items^[0])^ do
  begin DeleteAll; Limit:=0 { czy to nie jest blad w dysponowaniu ?} end;
 {kompilator Kylix 2 zglosil Internal error C1422}*)
 PreInstCollectionPtr(aUnifColl.Items^[0])^.DeleteAll;
 PreInstCollectionPtr(aUnifColl.Items^[0])^.Limit:=0;
 aUnifColl.Done;
end;

procedure PreInstCollection.InsertAndAbsorb(fElem:NatFuncPtr);
 var i:integer;
     lAtom:NatFuncPtr;
 label ItIsWeaker;
begin
 i:=0;
 while i < Count do
  begin lAtom:=Items^[i];
   case lAtom^.CompareWith(fElem^) of
     0: inc(i);
     1: goto ItIsWeaker;
    -1: begin dispose(fElem,Done); exit end;
   end;
  end;
 Insert(fElem);
 exit;
ItIsWeaker:
 FreeItem(lAtom);
 AtPut(i,fElem); inc(i);
 while i < Count do
  begin lAtom:=Items^[i];
   if fElem^.WeakerThan(lAtom^) then
     begin FreeItem(lAtom);
      AtDelete(i)
     end
    else inc(i);
  end;
end;

{ Renew powinno byc zawsze ! uzyte, jezeli kolekcja generujaca
  zmienia status:
  Tzn. jest do niej inne dojscie, niz z listy generatorow, w ten
  sposob generatory, moga byc rozdysponowywane, mimo, ze
  byly byc moze juz dysponowane, np. kiedy tworzono liste.
}
procedure PreInstCollection.Renew;
  var lItems: PItemList;
      k,lCount: integer;
begin lItems:=Items;
 lCount:=Count;
 DeleteAll;
 Limit:=0;
 SetLimit(lCount);
 Count:=lCount;
 for k:=0 to Count-1 do
  Items^[k]:=new(NatFuncPtr,CopyNatFunc(NatFuncPtr(lItems^[k])^));
 Status:=Regular;
end;

procedure PreInstCollection.UnionWith(fColl:PreInstCollectionPtr);
  var lColl: PreInstCollection;
      z: integer;
begin
 if nOverflow then
   begin fColl.Done;
    exit
   end;
 if fColl^.nOverflow then
  begin Done;
   Count:=0;
   nOverflow:=true;
   exit
   end;
 if Count = 0 then
  begin Done;
   Self:=fColl^;
   exit
  end;
 if Status = Top then
  begin fColl^.Done;
   exit
  end;
 if fColl^.Count = 0 then exit;
 if fColl^.Status = Top then
  begin
   Done;
   PreInstCollection.InitTop;
   exit
  end;
 lColl.Init(0,0);
 lColl:=fColl^;
 if Status = Generator then ReNew;
 if lColl.Status = Generator then lColl.ReNew;
 SetLimit(Count+lColl.Count);
 for z:=0 to lColl.Count-1 do
  begin InsertAndAbsorb(lColl.Items^[z]);
   if nOverflow then
    begin
     lColl.Done;
     exit
    end;
  end;
 SetLimit(0);
 Status:=Regular;
 lColl.DeleteAll;
 lColl.Done;
end;

procedure PreInstCollection.JoinWith(fColl:PreInstCollectionPtr);
 var tTLC: PreInstCollection;
     NewSize: longint;
     lAtom: NatFuncPtr;
     z,z1: integer;
begin
 { Top & Bottom }
 if Status = Top then
   begin
    PreInstCollection.InitBottom;
    Self:=fColl^;
    exit
   end;
 if fColl^.Status = Top then exit;
 if fColl^.Count = 0 then
  begin
   if Status = Generator then
     begin
      Count:=0;
      Limit:=0;
      Items:=nil
     end
    else Done;
   exit;
  end;
 if Count = 0 then
  begin fColl^.Done;
   exit
  end;
{ Tutaj na pewno mozna podoptymalizowac, jezeli ktoras
  ze stron jest regular, a druga ma dlugosc 1 }
 NewSize:=Count*fColl^.Count;
 if NewSize > MaxInstNbr then NewSize:=MaxInstNbr;
 if (NewSize = 1) and (Status = Regular) then
  begin lAtom:=NatFuncPtr(Items^[0])^.JoinAtom(fColl^.Items^[0]);
   dispose(NatFuncPtr(Items^[0]), Done);
   if lAtom <> nil then AtPut(0,lAtom) else
     begin DeleteAll;
      Done
     end;
   fColl^.Done;
   exit;
  end;
 tTLC.Init(0,0);
 tTLC:=Self;
 Limit:=0;
 Count:=0;
 SetLimit(NewSize);
 for z:=0 to fColl^.Count-1 do
  for z1:=0 to tTLC.Count-1 do
  begin lAtom:=NatFuncPtr(tTLC.Items^[z1])^.JoinAtom(fColl^.Items^[z]);
   if lAtom <> nil then InsertAndAbsorb(lAtom);
   if nOverflow then
    begin tTLC.Done;
     fColl^.Done;
     exit
    end;
  end;
 tTLC.Done;
 fColl^.Done;
 SetLimit(0);
 Status:=Regular;
{ Pewnie warto miec osobna informacje, czy to jest Bottom i raczej uzywac tego }
end;

procedure PreInstCollection.InsertEval(var fEval:NatFunc);
begin
 InsertAndAbsorb(NatFuncPtr(fEval.CopyObject));
end;

constructor PreInstCollection.SingleEval(var fEval:NatFunc);
begin
 InitSingle(NatFuncPtr(fEval.CopyObject));
end;

{$IFDEF MDEBUG}
procedure infoeval(aEval:NatFuncPtr; const aBasic:MCollection);
 var i,k: integer;
begin
  writeln(infofile,'&{');
  k:=1;
  for i:=0 to aBasic.Count-1 do
   begin
    if aEval^.HasInDom(i) then
    begin
     write(infofile,k,': ');
     inc(k);
     if aEval^.Value(i)=0 then
      begin
//       if not Sign then
       write(infofile,'-');
      end;
//     else if Sign then write(infofile,'-');
     InfoFormula(aBasic.Items^[I]); infonewline;
    end;
   end;
  writeln(infofile,' }');
end;
procedure InfoInference(const aNormalForm:PreInstCollection; const aBasic:MCollection);
 var z: integer;
begin
 for z:=0 to aNormalForm.Count-1 do
 begin
//   if Sign then write(infofile,'-');
   if aNormalForm.Items = nil then
    begin writeln(infofile,'&{%}');
     break
    end;
   infoeval(NatFuncPtr(aNormalForm.Items^[z]),aBasic);
 end;
end;
{$ENDIF}

function BasicFrmNr(aFrm:FrmPtr; const aBasic:MCollection): integer;
 var i: integer;
begin
 for I:=0 to aBasic.Count-1 do
  if EqFrm(aFrm,aBasic.Items^[I]) then
   begin BasicFrmNr:=i;
    exit
   end;
 BasicFrmNr:=-1;
end;

function NewBasic(fFrm:FrmPtr; Disposable:boolean): integer;
 var I: integer;
begin
 I:=BasicFrmNr(fFrm,gBasicPtr^);
 if I >=0 then
   begin
     if Disposable then dispose(fFrm,Done);
     NewBasic:=I;
     exit
    end;
 if not Disposable then fFrm:=fFrm^.CopyFormula;
 gBasicPtr^.Insert(fFrm);
 NewBasic:=gBasicPtr^.Count-1;
end;

constructor PreInstCollection.NormalizeAsFalse(fFrm: FrmPtr);
 var lEvalL: PreInstCollection;
     lsnt: FrmPtr;
     lNegBas: NatFunc;
     z: integer;
begin
 with fFrm^ do
  case FrmSort of
   ikFrmNeg: NormalizeAsTrue(NegFrmPtr(fFrm)^.NegArg);
   ikFrmConj:
    begin InitBottom;
     for z:=0 to ConjFrmPtr(fFrm)^.Conjuncts.Count-1 do
      begin
       lEvalL.NormalizeAsFalse(FrmPtr(ConjFrmPtr(fFrm)^.Conjuncts.Items^[z]));
       UnionWith(@lEvalL);
      end;
    end;
   ikFrmVerum: InitBottom;
   else
    begin
      lsnt:=fFrm^.CopyFormula;
      WithinFormula(lsnt,SetVarInTrm);
      lNegBas.InitNatFunc(2,4);
      lNegBas.Assign(NewBasic(lsnt,true),0);
      SingleEval(lNegBas);
    end;
  end;
end;

constructor PreInstCollection.NormalizeAsTrue(fFrm: FrmPtr);
 var lEvalL: PreInstCollection;
     lsnt: FrmPtr;
     lPosBas: NatFunc;
     z: integer;
begin
 with fFrm^ do
  case FrmSort of
   ikFrmNeg: NormalizeAsFalse(NegFrmPtr(fFrm)^.NegArg);
   ikFrmConj:
    begin
     InitTop;
     for z:=0 to ConjFrmPtr(fFrm)^.Conjuncts.Count-1 do
      begin
       lEvalL.NormalizeAsTrue(FrmPtr(ConjFrmPtr(fFrm)^.Conjuncts.Items^[z]));
       JoinWith(@lEvalL);
      end;
    end;
   ikFrmVerum: InitTop;
   else
    begin
      lsnt:=fFrm^.CopyFormula;
      WithinFormula(lsnt,SetVarInTrm);
      lPosBas.InitNatFunc(2,4);
      lPosBas.Assign(NewBasic(lsnt,true),1);
      SingleEval(lPosBas);
    end;
  end;
end;

var NonFictitious:boolean;
procedure CheckQuantified(var fTrm:TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if (TrmSort=ikTrmBound) and (VarNr=BoundVarNbr) then
   NonFictitious:=true;
end;

procedure RemoveFictitious(var fTrm:TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if (TrmSort = ikTrmBound) and (VarNr > BoundVarNbr) then dec(VarNr);
end;

function DistributeQuantifiers(fFrmPtr:FrmPtr):FrmPtr;
 var lFrmPtr: FrmPtr;
     i,z: integer;
begin
 case fFrmPtr^.FrmSort of
  ikFrmNeg:
   DistributeQuantifiers:=
      NewNegDis(DistributeQuantifiers(NegFrmPtr(fFrmPtr)^.NegArg));
  ikFrmConj:
   with ConjFrmPtr(fFrmPtr)^.Conjuncts do
    begin lFrmPtr:=NewVerum;
     for z:=0 to Count-1 do
      lFrmPtr:=NewConj(lFrmPtr,DistributeQuantifiers(ConjFrmPtr(Items^[z])));
     DistributeQuantifiers:=lFrmPtr;
    end;
  ikFrmUniv:
   with UnivFrmPtr(fFrmPtr)^ do
    begin inc(BoundVarNbr);
     lFrmPtr:=DistributeQuantifiers(Scope);
     if lFrmPtr^.FrmSort = ikFrmConj then
      with ConjFrmPtr(lFrmPtr)^.Conjuncts do
       begin
        for i:=0 to Count-1 do
         begin NonFictitious:=false;
          WithinFormula(Items^[i],CheckQuantified);
          if NonFictitious then
           Items^[i]:=NewUniv(CopyExpTyp(Quantified),Items^[i])
          else WithinFormula(Items^[i],RemoveFictitious);
         end;
        DistributeQuantifiers:=lFrmPtr;
       end
     else DistributeQuantifiers:=NewUniv(CopyExpTyp(Quantified),lFrmPtr);
     dec(BoundVarNbr);
    end;
  ikFrmPrivPred:
   if LocPredFrmPtr(fFrmPtr)^.PredExp^.FrmSort<>ikFrmError then
    DistributeQuantifiers:=DistributeQuantifiers(LocPredFrmPtr(fFrmPtr)^.PredExp)
   else DistributeQuantifiers:=CopyExpFrm(fFrmPtr);
  ikFrmSchPred,ikFrmAttr,ikFrmPred,ikFrmQual,ikFrmThesis,ikFrmVerum,ikFrmFlexConj,ikFrmError :
   DistributeQuantifiers:=CopyExpFrm(fFrmPtr);
  else RunTimeError(2040);
 end;
end;

var
  ConstBase: integer;

procedure SetConstInTrm(var fTrm: TrmPtr);
begin
 if fTrm^.Trmsort=ikTrmBound then
  with VarTrmPtr(fTrm)^ do
  if VarNr>BoundVarNbr then dec(VarNr,BoundVarNbr)
   else
    begin TrmSort:=ikTrmConstant;
     inc(VarNr,ConstBase)
    end;
end;

procedure InitConstBase;
begin
 ConstBase:=gVarNbr;
end;

procedure NewConst(aTyp: TypPtr);
 var lTyp: TypPtr;
begin
 if gVarNbr >= MaxVarNbr then
  begin ConstOvfl:=true;
   exit
  end;
 lTyp:=aTyp^.CopyType;
 lTyp^.WithinType(SetConstInTrm);
 inc(gVarNbr);
 CollectConstInTyp(lTyp);
 FixedVar[gVarNbr].nTyp:=lTyp;
 FixedVar[gVarNbr].nDef:=nil;
end;

procedure CollectBasics(fFrm: FrmPtr);
 var z: integer;
begin
 with fFrm^ do
  case FrmSort of
   ikFrmNeg: CollectBasics(NegFrmPtr(fFrm)^.NegArg);
   ikFrmConj:
    for z:=0 to ConjFrmPtr(fFrm)^.Conjuncts.Count-1 do
      begin
       CollectBasics(FrmPtr(ConjFrmPtr(fFrm)^.Conjuncts.Items^[z]));
      end;
   ikFrmVerum:;
   else
    NewBasic(fFrm^.CopyFormula,true);
  end;
end;

procedure RemoveIntQuantifier(var aFrm: FrmPtr); forward;

procedure RemoveExtQuantifier(var aFrm: FrmPtr);
 var lFrm: FrmPtr;
     lConjuncts: MCollection;
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

procedure RemoveIntQuantifier(var aFrm: FrmPtr);
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

procedure CreateInference(var fNormalForm:PreInstCollection);
 var lEvalList,lEvalLIst1,lEvalLIst2,LNormalForm: PreInstCollection;
     lEval,llEval,lNegBas: NatFunc;
     I,z,zCount,x,zAggrNr,zz,lQualified: integer;
     lFrm,llFrm: FrmPtr;
     lQual: MCollection;
     lSubject,llSubject,llTrm:TrmPtr;
     lQualification,lQualTyp,llQual,llTyp: TypPtr;
     lClusterPtr: AttrCollectionPtr;
begin { CreateInference }
 z:=0; zCount:=fNormalForm.Count;
 while z < zCount do
 begin
  lQual.Init(0,4);
  lEval.CopyNatFunc(NatFuncPtr(fNormalForm.Items^[z])^);
  for I:=0 to Basic.Count-1 do
   if NatFuncPtr(fNormalForm.Items^[z])^.HasInDom(I)
     and (NatFuncPtr(fNormalForm.Items^[z])^.Value(I) = 0)
     and (FrmPtr(Basic.Items^[I])^.FrmSort = ikFrmQual) then
   begin
    lEval.DeleteElem(I);
    lQual.Insert(Basic.Items^[I]);
   end;
  if lQual.Count>0 then
   begin
    LNormalForm.SingleEval(lEval);
    for I:=0 to lQual.Count-1 do
     begin LFrm:=lQual.Items^[I];
      lSubject:=CopyTerm(QualFrmPtr(lFrm)^.QualTrm);
      lQualification:=QualFrmPtr(lFrm)^.QualTyp^.CopyType;
      dispose(lQualification^.LowerCluster,Done);
      lQualification^.LowerCluster:=NewEmptyCluster;
      dispose(lQualification^.UpperCluster,Done);
      lQualification^.UpperCluster:=NewEmptyCluster;
      lQualified:=NewBasic(NewQualFrm(CopyTerm(lSubject),lQualification^.CopyType),true);
{ tworzymy nowa lEvalList }
      lEvalList.Init(EvalLimit,EvalDelta);
      lEvalList.Status:=Regular;
      with QualFrmPtr(lFrm)^.QualTyp^.LowerCluster^ do
      for x:=0 to Count-1 do
       begin
        lQualTyp:=lQualification^.CopyType;
        with AttrPtr(Items^[x])^,
        ConstrTypPtr( Constr[ coAttribute].Items^[fAttrNr])^ do
         begin
          lClusterPtr:=InstCluster(fConstrTyp^.UpperCluster,fAttrArgs);
          lClusterPtr^.InsertAttr(fAttrNr,ord(not boolean(fNeg)),CopyTermList(fAttrArgs));
          CollectConstInCluster(lClusterPtr);
         end;
        dispose(lQualTyp^.LowerCluster,Done);
        lQualTyp^.LowerCluster:=lClusterPtr;
        dispose(lQualTyp^.UpperCluster,Done);
        lQualTyp^.UpperCluster:=CopyCluster(lClusterPtr);
        llEval.InitNatFunc(2,4);
        llEval.Assign(NewBasic(NewQualFrm(CopyTerm(lSubject),lQualTyp),true),1);
        lEvalList.InsertEval(llEval);
       end;
      lNegBas.InitNatFunc(2,4);
      lNegBas.Assign(lQualified,0);
      lEvalList1.SingleEval(lNegBas);
{ Nie jest tego typu, tzn. w przypadku typu strukturalnego, ma
takze nieodpowiednie pola.
}
      if lQualification^.TypSort = ikTypStruct then
       begin
        llTyp:=CopyTrmType(lSubject);
        llTyp:=lQualification^.WideningOf(llTyp);
        if llTyp <> nil then
         begin dispose(llTyp,Done);
          with StructConstrPtr(Constr[ coStructMode].At(lQualification^.ModNr))^ do
           zAggrNr:=fStructModeAggrNr;
          lEvalList2.Init(EvalLimit,EvalDelta);
          lEvalList2.Status:=Regular;
          with AggrConstrPtr( Constr[ coAggregate].At( zAggrNr))^ do
           for zz:=0 to fAggrColl^.Count-1 do
           begin llTyp:=CopyTrmType(lSubject);
            llTyp:=lQualification^.WideningOf(llTyp);
            if llTyp <> nil then
             begin
              llSubject:=ReconSelectTrm(PIntItem(fAggrColl^.Items^[zz])^.IntKey,CopyTerm(lSubject),llTyp);
              dispose(llTyp,Done);
              llTrm:=ReconSelectTrm(PIntItem(fAggrColl^.Items^[zz])^.IntKey,CopyTerm(lSubject),lQualification);
              llQual:=CopyTrmType(llTrm);
              DisposeTrm(llTrm);
              llFrm:=NewQualFrm(llSubject,llQual);
              lEval.InitNatFunc(2,4);
              lEval.Assign(NewBasic(llFrm,true),0);
              lEvalList2.InsertEval(lEval);
             end;
           end;
          lEvalList1.JoinWith(@lEvalList2);
         end;
       end;
      lEvalList.UnionWith(@lEvalList1);
      lNormalForm.JoinWith(@lEvalList);
{ Przetwarzanie typow strukturalnych, trzeba zmienic kolejnosc,
powstaja negatywne kwalifikujace, moga miec atrybuty ! }
      dispose(lQualification,Done);
      DisposeTrm(lSubject);
     end;
    fNormalForm.AtDelete(z);
    dec(zCount);
    fNormalForm.AppendTo(lNormalForm);
   end
  else inc(z);
  lQual.DeleteAll; lQual.Done;
 end;
end;

procedure WellMatchedExpansions(aWord:Lexem; aArgs:TrmList; var aExp:MList);
 var i: integer;
     lFrm,lFrm1: FrmPtr;
begin
 for i:=ExDefinientia.Count-1 downto 0 do
  with DefiniensPtr(ExDefinientia.Items^[i])^,Definiens^ do
   begin
    if (Assumptions <> nil) and (Assumptions^.FrmSort <> ikFrmVerum)
     then continue;
    if nPartialDefinientia.Count > 0 then continue;
    if DefSort <> 'm' then continue;
    if Matches(aWord,aArgs,DefiniensPtr(ExDefinientia.Items^[i])) then
     begin
      if FrmPtr(nOtherwise)^.FrmSort = ikError then continue;
      lFrm1:=FrmPtr(nOtherwise)^.CopyFormula;
      WithinFormula(lFrm1,ChangeBound);
//      lFrm:=InstSubstFrm(FrmPtr(nOtherwise));
      lFrm:=InstSubstFrm(lFrm1);
      dispose(lFrm1,Done);
      aExp.Insert(lFrm);
     end
    else DisposeSubstTrm;
   end;
end;

procedure FunctorExpansions(aTrm:TrmPtr; var aExp:MList);
 var lWord: Lexem;
     lArgs,lElem: TrmList;
begin
  with ConstDefPtr(InferConstDef.Items^[VarTrmPtr(aTrm)^.VarNr])^ do
   if fDef^.TrmSort = ikTrmFunctor then
   begin
     lWord.Kind:=ikTrmFunctor;
     AdjustTrm(fDef,lWord.Nr,lArgs);
     lElem:=nil;
     if lArgs = nil then
      lArgs:=NewTrmList(aTrm,nil)
     else
      begin lElem:=LastElem(lArgs);
       lElem^.NextTrm:=NewTrmList(aTrm,nil);
      end;
     WellMatchedExpansions(lWord,lArgs,aExp);
     if lElem <> nil then
      begin dispose(lElem^.NextTrm);
       lElem ^.NextTrm:=nil 
      end
     else dispose(lArgs);
   end;
end;

procedure AtomicFormulaExpansions(aFrm:FrmPtr; var aExp: MList);
 var lWord: Lexem;
     lArgs,lElem: TrmList;
begin
 aExp.Init(0);
 lWord.Kind:=aFrm^.FrmSort;
 case lWord.Kind of
  ikFrmPred: AdjustFrm(PredFrmPtr(aFrm),lWord.Nr,lArgs);
  ikFrmAttr: AdjustAttrFrm(PredFrmPtr(aFrm),lWord.Nr,lArgs);
{  ikFrmQual:
   with QualFrmPtr(aFrm)^,QualTyp^ do
    if (TypSort = ikTypMode) and (LowerCluster^.Count = 0) then
     begin lWord.Kind:=ikTypMode;
      QualTyp^.AdjustTyp(lWord.Nr,lArgs);
      lElem:=nil;
      if lArgs = nil then
       lArgs:=NewTrmList(QualTrm,nil)
      else
       begin lElem:=LastElem(lArgs);
        lElem^.NextTrm:=NewTrmList(QualTrm,nil);
       end;
     end; }
  else exit;
 end;
 WellMatchedExpansions(lWord,lArgs,aExp);
{ if lWord.Kind = ikFrmPred then
  begin
   if lWord.Nr = gBuiltIn[rqBelongsTo] then
    with ConstDefPtr(InferConstDef.Items^[VarTrmPtr(lArgs^.NextTrm^.XTrmPtr)^.VarNr])^ do
     if fDef^.TrmSort = ikTrmFraenkel then
      aExp.Insert(FraenkelFrm(lArgs^.XTrmPtr,fDef));
//   if lWord.Nr = gBuiltIn[rqEqualsTo] then
//    begin
//     FunctorExpansions(lArgs^.XTrmPtr,aExp);
//     FunctorExpansions(lArgs^.NextTrm^.XTrmPtr,aExp);
//    end;
  end; }
 if lWord.Kind = ikTypMode then
  if lElem <> nil then
   begin dispose(lElem^.NextTrm);
     lElem ^.NextTrm:=nil
   end
  else dispose(lArgs);
end;

var
  gTrms: NatSet;

procedure CollectConst(var aTrm:TrmPtr);
begin
 with VarTrmPtr(aTrm)^ do
  if TrmSort = ikTrmInfConst then
   with VarTrmPtr(aTrm)^ do
   begin
    if not gTrms.HasInDom(VarNr) then
      begin
       ConstDefPtr(InferConstDef.Items^[VarNr])^.fTyp^.WithinType(CollectConst);
       gTrms.InsertElem(VarNr);
      end;
   end;
end;

procedure CollectAllConstInInference;
 var i,x: integer;
begin
 gTrms.Init(0,16);
 for i:=0 to Basic.Count-1 do
  WithinFormula(Basic.Items^[i],CollectConst);
 i:=0;
 while i < gTrms.Count do
  with ConstDefPtr(InferConstDef.Items^[gTrms.Items^[i].X])^ do
  begin
   for x:=0 to fEqConst.Count-1 do
    begin
     if not gTrms.HasInDom(fEqConst.Items^[x].X) then
      begin
       ConstDefPtr(InferConstDef.Items^[fEqConst.Items^[x].X])^.fTyp^.WithinType(CollectConst);
       gTrms.InsertElem(fEqConst.Items^[x].X)
      end;
    end;
   WithinTerm(fDef,CollectConst);
   inc(i);
  end;
 if gTrms.Count >= MaxTrmNbr then
  gTrms.Done;
end;

function ExpandAtomicFormula(fFrm:FrmPtr;fSign:boolean):FrmPtr;
var lExpandFrm: FrmPtr;
    lExpansions: MList;
    d: integer;
begin
 lExpandFrm:=CopyExpFrm(fFrm);
 AtomicFormulaExpansions(lExpandFrm,lExpansions);
 for d:=0 to lExpansions.Count-1 do
  if fSign then lExpandFrm:=NewConj(lExpandFrm,lExpansions.Items[d])
   else lExpandFrm:=NewDisj(lExpandFrm,lExpansions.Items[d]);
 ExpandAtomicFormula:=lExpandFrm;
end;

function ExpandAtomicAsFalse(fFrm:FrmPtr):FrmPtr; forward;

function ExpandAtomicAsTrue(fFrm:FrmPtr):FrmPtr;
var lConjFrmPtr: FrmPtr;
    z: integer;
begin
 case fFrm^.FrmSort of
  ikFrmNeg:
   ExpandAtomicAsTrue:=NewNeg(ExpandAtomicAsFalse(NegFrmPtr(fFrm)^.NegArg));
  ikFrmConj:
   with ConjFrmPtr(fFrm)^.Conjuncts do
    begin
     lConjFrmPtr:=NewVerum;
     for z:=0 to Count-1 do
      lConjFrmPtr:=NewConj(lConjFrmPtr,ExpandAtomicAsTrue(ConjFrmPtr(Items^[z])));
     ExpandAtomicAsTrue:=lConjFrmPtr;
    end;
  {ikFrmQual,}ikFrmAttr,ikFrmPred:
   ExpandAtomicAsTrue:=ExpandAtomicFormula(fFrm,true);
  ikFrmQual,ikFrmVerum,ikFrmThesis,ikFrmUniv,ikFrmSchPred,ikFrmPrivPred,ikFrmFlexConj,ikError:
   ExpandAtomicAsTrue:=CopyExpFrm(fFrm);
  else RunTimeError(2801);
 end;
end;

function ExpandAtomicAsFalse(fFrm:FrmPtr):FrmPtr;
var lConjFrmPtr:FrmPtr;
    z:integer;
begin
 case fFrm^.FrmSort of
  ikFrmNeg:
   ExpandAtomicAsFalse:=NewNeg(ExpandAtomicAsTrue(NegFrmPtr(fFrm)^.NegArg));
  ikFrmConj:
   with ConjFrmPtr(fFrm)^.Conjuncts do
    begin
     lConjFrmPtr:=NewVerum;
     for z:=0 to Count-1 do
      lConjFrmPtr:=NewConj(lConjFrmPtr,ExpandAtomicAsFalse(ConjFrmPtr(Items^[z])));
     ExpandAtomicAsFalse:=lConjFrmPtr;
    end;
  {ikFrmQual,}ikFrmAttr,ikFrmPred:
   ExpandAtomicAsFalse:=ExpandAtomicFormula(fFrm,false);
  ikFrmQual,ikFrmVerum,ikFrmThesis,{ikFrmUniv,}ikFrmSchPred,ikFrmPrivPred,ikFrmFlexConj,ikError:
   ExpandAtomicAsFalse:=CopyExpFrm(fFrm);
  ikFrmUniv:
   with UnivFrmPtr(fFrm)^ do
    begin
     inc(BoundVarNbr);        
     BoundVar[BoundVarNbr]:=Quantified;
     ExpandAtomicAsFalse:=NewUniv(CopyExpTyp(Quantified),ExpandAtomicAsFalse(Scope));
     dec(BoundVarNbr);        
    end;
  else RunTimeError(2802);
 end;
end;

// global count for easy debugging of Precheck
{$IFDEF MDEBUG}
var gPrecheckNr: integer = 0;
{$ENDIF}
procedure PreCheck(const aInference:RefSntArr; aInfNbr:integer;
                    var NormalForm:PreInstCollection);
  var
   i: integer;
   lChkFrm: FrmPtr;
   lInference:RefSntArr;
begin { PreChecker }

 ConstBase:=gVarNbr;

{$IFDEF MDEBUG}
inc(gPrecheckNr);
writeln(InfoFile);
writeln(InfoFile,'Inference: -------------------------');
writeln(InfoFile,'Line=',CurPos.Line,', Col=',CurPos.Col,', gPreCheckNr=',gPreCheckNr);
for i:=1 to aInfNbr do
begin
writeln(InfoFile,'i=',i);
InfoFormula(aInference[i]); InfoNewline;
flush(InfoFile);   
end;
//InfoBuildIn;
{$ENDIF}

 // including definitional expansions
 BoundVarNbr:=0;
 for i:=1 to aInfNbr do lInference[i]:=ExpandAtomicAsTrue(aInference[i]);
 // including expansions of ellipsis
 for i:=1 to aInfNbr do
  begin
//   lChkFrm:=ExpandFlexAsTrue(aInference[i]);
//   dispose(lInference[i],Done);
//   lInference[i]:=lChkFrm;
   lInference[i]:=ExpandFlexAsTrue(lInference[i]);
  end;

 lChkFrm:=NewVerum;
 for i:=1 to aInfNbr do
  begin
   lChkFrm:=NewConj(lChkFrm,DistributeQuantifiers(lInference[i]));
   dispose(lInference[i],Done);
  end;

{$IFDEF MDEBUG}
InfoNewLine; InfoString('lChkFrm '); InfoFormula(lChkFrm); InfoNewLine; flush(InfoFile);
{$ENDIF}

 InitVarBase:=InitConstBase;
 SetVarInTrm:=SetConstInTrm;
 NewVariable:=NewConst;

 RemoveIntQuantifier(lChkFrm);

 // ustawinie BoundVarNbr na 0 konieczne dla prawidlowego kolekcjonowania stalych
 BoundVarNbr:=0;

 CollectConstInFrm(lChkFrm);

 ConstBase:=gVarNbr;
 Basic.Init(8,16);
 gBasicPtr:=addr(Basic);
 TrivialError:=false;

 NormalForm.NormalizeAsTrue(lChkFrm);

 dispose(lChkFrm,Done);
 for i:=0 to NormalForm.Count-1 do
  if NormalForm.Items = nil then
   begin
    TrivialError:=true;
    exit
   end;
 CreateInference(NormalForm);

{$IFDEF MDEBUG}
 if not LatOvfl then
  begin
writeln(infofile,'postac normalna:');
InfoInference(NormalForm,Basic);
flush(InfoFile);
  end;
{$ENDIF}
end { PreChecker };

end.
