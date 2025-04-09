(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit polynom;

interface
uses mobjects,numbers;

type
  MonomialPtr = ^MonomialObj;
  MonomialObj = object(MObject)
    nCoefficient: RComplex;
    nPowerProduct: NatFunc;
   constructor Init(const aCoefficient: RComplex);
   constructor InitMonomial(const aCoefficient: RComplex; aVarNr: integer);
   destructor Done; virtual;
   function MCopy: PObject; virtual;
   function IsNumeric: boolean;
   function IsUniVariant: boolean;
   function IsVariable: integer;
  end;

  PolynomialPtr = ^PolynomialObj;
  PolynomialObj = object(MSortedCollection)
   constructor Init;
   constructor InitWithNumeric(const aValue: RComplex);
   constructor InitWithMonomial(const aCoefficient: RComplex; aVarNr: integer);
   destructor Done; virtual;
   procedure Insert(aItem: Pointer); virtual;
   function IsNumeric: boolean;
   function IsNumericEqualWith(const aNumber: RComplex): boolean;
   procedure GetNumeric(var aNumber: RComplex);
   function IsUniVariantVariable: integer;
   function IsVariable: integer;
   function Misses(var aVars: NatSet): boolean;
   function HasTheVariable(aVarNr: integer): boolean;
   procedure InsertValue(aVarNr: integer; aPolynomial: PolynomialPtr);
  end;

{$IFDEF MDEBUG}
procedure infomonomial(fMon: MonomialPtr);
procedure infopolynomial(fPol: PolynomialPtr);
{$ENDIF}

function LexCompareMonomials(aKey1, aKey2: Pointer): Integer;
function GrLexCompareMonomials(aKey1, aKey2: Pointer): Integer;
function GRevLexCompareMonomials(aKey1, aKey2: Pointer): Integer;
function StdCompareMonomials(aKey1, aKey2: Pointer): Integer;
function CompareMonomials(aKey1, aKey2: Pointer): Integer;
function StdComparePolynomials(aKey1, aKey2: Pointer): Integer;
function ComparePolynomials(aKey1, aKey2: Pointer): Integer;

function DivMonomials(aMonomial1,aMonomial2: MonomialPtr): MonomialPtr;

function AddPolynomials(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;
function MultPolynomials(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;
function DiffPolynomials(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;
function NthPower(aPolynomial: PolynomialPtr; N: integer): PolynomialPtr;
function NMultPolynomial(const aNumber: RComplex; aPolynomial: PolynomialPtr): PolynomialPtr;
function MMultPolynomial(aMonomial:MonomialPtr; aPolynomial: PolynomialPtr): PolynomialPtr;

function SPolynomial(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;

var GroebnerOverflow:Boolean;
function SimpleReduction(aPolynomial: PolynomialPtr; const aPolynomials: MList): PolynomialPtr;
function Reduction(aPolynomial: PolynomialPtr; const aPolynomials: MList): PolynomialPtr;
procedure ReduceBasis(var aPolynomials : MList);
procedure ExtendToGroebnerBasis0(var aPolynomials : MList);
procedure ExtendToGroebnerBasis(var aPolynomials : MList);
function CheckGroebnerBasis(aPolynomials : MList):boolean;
procedure RecodeVariableOrder(var aPolynomials : MList;EqCount:Integer;var aTransOrder:NatFuncPtr);
procedure RecodeVariables(var aPolynomialValues : MList; aTransOrder:NatFuncPtr);

procedure GaussElimination(var aEqs: MSortedCollection; var aContr: integer);

function LinearEquationReduce(aEq: PolynomialPtr; const aEquations: MList): PolynomialPtr;


implementation
uses errhan {$IFDEF MDEBUG} ,info,outinfo{$ENDIF};

{$IFDEF MDEBUG}
procedure infomonomial(fMon: MonomialPtr);
 var k: integer;
begin
 with fMon^ do
 begin
  infocomplex(nCoefficient);
  for k:=0 to nPowerProduct.Count-1 do
   begin
    write(infofile,'*X',nPowerProduct.items^[k].X,'^',nPowerProduct.items^[k].Y);
   end;
 end;
end;

procedure infopolynomial(fPol: PolynomialPtr);
 var j: integer;
begin
 with fPol^ do
 begin
  for j:=0 to Count-1 do
   begin
    if j > 0 then
     write(infofile,'+');
    write(infofile,'(');
    infoMonomial(MonomialPtr(Items^[j]));
    write(infofile,')');
   end;
 end;
end;
{$ENDIF}

constructor MonomialObj.Init(const aCoefficient: RComplex);
begin
 nCoefficient:=aCoefficient;
 nPowerProduct.InitNatFunc(0,4);
end;

constructor MonomialObj.InitMonomial(const aCoefficient: RComplex; aVarNr: integer);
begin
 Init(aCoefficient);
 nPowerProduct.InitNatFunc(1,2);
 nPowerProduct.Assign(aVarNr,1);
end;

destructor MonomialObj.Done;
begin
 nPowerProduct.Done;
end;

function MonomialObj.MCopy: PObject;
var lRes: PObject;
begin
 lRes:=new(MonomialPtr,Init(self.nCoefficient));
 MonomialPtr(lRes)^.nPowerProduct.CopyNatFunc(self.nPowerProduct);
 MCopy := lRes;
end;

function MonomialObj.IsNumeric: boolean;
begin
  IsNumeric := nPowerProduct.Count = 0;
end;

function MonomialObj.IsUniVariant: boolean;
begin
  IsUniVariant := nPowerProduct.Count = 1;
end;

function MonomialObj.IsVariable: integer;
begin IsVariable:=0;
  if isUnivariant and (nPowerProduct.Items^[0].Y = 1)
                  and (CompareComplex(nCoefficient,COne) = 0) then
   IsVariable:=nPowerProduct.Items^[0].X;
end;

function LexCompareMonomials(aKey1, aKey2: Pointer): Integer;
var i,lComp : integer;
begin
   with MonomialPtr(aKey1)^.nPowerProduct,MonomialPtr(aKey2)^ do
   begin
      for i:=0 to Count-1 do
      begin
	 if i<nPowerProduct.Count then
	    lComp:=CompareInt(Items^[i].X,nPowerProduct.Items^[i].X)
	 else lComp:=-1;
	 if lComp <> 0 then begin LexCompareMonomials:=lComp; exit end;
	 lComp:=-CompareInt(Items^[i].Y,nPowerProduct.Items^[i].Y);
	 if lComp <> 0 then begin LexCompareMonomials:=lComp; exit end;
      end;
      LexCompareMonomials:=-CompareInt(Count,nPowerProduct.Count);
   end;
end;

function GrLexCompareMonomials(aKey1, aKey2: Pointer): Integer;
var lComp : integer;
begin
   lComp:=-CompareInt(MonomialPtr(aKey1)^.nPowerProduct.CountAll,
		     MonomialPtr(aKey2)^.nPowerProduct.CountAll);
   if lComp = 0 then lComp:=LexCompareMonomials(aKey1,aKey2);
   GrLexCompareMonomials:=lComp;
end;

function GRevLexCompareMonomials(aKey1, aKey2: Pointer): Integer;
var i,lComp : integer;
begin		 
   lComp:=-CompareInt(MonomialPtr(aKey1)^.nPowerProduct.CountAll,
		     MonomialPtr(aKey2)^.nPowerProduct.CountAll);
   if lComp = 0 then
      with MonomialPtr(aKey1)^.nPowerProduct,MonomialPtr(aKey2)^ do
      begin
	 for i:=0 to Count-1 do
	 begin
	    if nPowerProduct.Count-1-i>=0 then
	       lComp:=CompareInt(Items^[Count-1-i].X,nPowerProduct.Items^[nPowerProduct.Count-1-i].X)
	    else lComp:=1;
	    if lComp <> 0 then begin GRevLexCompareMonomials:=lComp; exit end;
	    lComp:=CompareInt(Items^[Count-1-i].Y,nPowerProduct.Items^[nPowerProduct.Count-1-i].Y);
	    if lComp <> 0 then begin GRevLexCompareMonomials:=lComp; exit end;
	 end;
	 lComp:=CompareInt(Count,nPowerProduct.Count);
      end;
   GRevLexCompareMonomials:=lComp;
end;

function StdCompareMonomials(aKey1, aKey2: Pointer): Integer;
 var lComp: integer;
begin
  lComp:=CompareInt(MonomialPtr(aKey1)^.nPowerProduct.CountAll,
                    MonomialPtr(aKey2)^.nPowerProduct.CountAll);
  if lComp = 0 then
   StdCompareMonomials:=CompareNatFunc(@(MonomialPtr(aKey1)^.nPowerProduct),
                                    @(MonomialPtr(aKey2)^.nPowerProduct))
   else StdCompareMonomials:=lComp;
end;

function StdComparePolynomials(aKey1, aKey2: Pointer): Integer;
 var i,lInt: integer;
begin
 with PolynomialPtr(aKey1)^ do
 begin
  lInt:=CompareInt(Count,NatFuncPtr(aKey2)^.Count);
  if lInt <> 0 then begin StdComparePolynomials:=lInt; exit end;
  for i:=0 to Count-1 do
  begin
    lInt:=StdCompareMonomials(Items^[i],PolynomialPtr(aKey2)^.Items^[i]);
    if lInt <> 0 then begin StdComparePolynomials:=lInt; exit end;
    lInt:=CompareComplex(MonomialPtr(Items^[i])^.nCoefficient,
                         MonomialPtr(PolynomialPtr(aKey2)^.Items^[i])^.nCoefficient);
    if lInt <> 0 then begin StdComparePolynomials:=lInt; exit end;
  end;
  StdComparePolynomials:=0;
 end;
end;

function CompareMonomials(aKey1, aKey2: Pointer): Integer;
begin
   CompareMonomials:=LexCompareMonomials(aKey1, aKey2);
end;

function ComparePolynomials(aKey1, aKey2: Pointer): Integer;
 var i,lInt: integer;
begin
 with PolynomialPtr(aKey1)^ do
 begin
  if NatFuncPtr(aKey2)^.Count = 0 then begin ComparePolynomials:=1; exit end;
  if Count = 0 then begin ComparePolynomials:=-1; exit end;
  i:=0;
  repeat
    lInt:=CompareMonomials(Items^[i],PolynomialPtr(aKey2)^.Items^[i]);
    if lInt <> 0 then begin ComparePolynomials:=lInt; exit end;
    lInt:=CompareComplex(MonomialPtr(Items^[i])^.nCoefficient,
                         MonomialPtr(PolynomialPtr(aKey2)^.Items^[i])^.nCoefficient);
    if lInt <> 0 then begin ComparePolynomials:=lInt; exit end;
    inc(i);
  until (i = Count) or (i = NatFuncPtr(aKey2)^.Count);
  ComparePolynomials:=CompareInt(Count,NatFuncPtr(aKey2)^.Count);
 end;
end;

function DivMonomials(aMonomial1,aMonomial2: MonomialPtr): MonomialPtr;
 var lMonomial: MonomialPtr;
     lPair: IntPair;
     k,d: integer;
begin
 DivMonomials:=nil;
 with aMonomial1^ do
 begin
   if not nPowerProduct.IsMultipleOf(aMonomial2^.nPowerProduct) then exit;
   if IsEqWithInt(aMonomial2^.nCoefficient,0) then exit;
   lMonomial:=new(MonomialPtr,Init(ComplexDiv(nCoefficient,aMonomial2^.nCoefficient)));
   lMonomial^.nPowerProduct.SetLimit(nPowerProduct.Limit);
   for k:=0 to nPowerProduct.Count-1 do
    if not aMonomial2^.nPowerProduct.HasInDom(nPowerProduct.Items^[k].X) then
     lMonomial^.nPowerProduct.AtInsert(lMonomial^.nPowerProduct.Count,nPowerProduct.Items^[k])
    else
     begin
      d:=nPowerProduct.Items^[k].Y-aMonomial2^.nPowerProduct.Value(nPowerproduct.Items^[k].X);
      MizAssert(4328, d >= 0);
      if d > 0 then
       begin
        lPair.X:=nPowerProduct.Items^[k].X;
        lPair.Y:=d;
        lMonomial^.nPowerProduct.AtInsert(lMonomial^.nPowerProduct.Count,lPair);
       end;
     end;
   DivMonomials:=lMonomial;
 end;
end;

function AddPolynomials(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;
var lRes: PolynomialPtr;
    lMonomial: MonomialPtr;
    lVal: RComplex;
    i,j,lInt: integer;
begin
// lRes:=PolynomialPtr(aPolynomial1^.MCopy);
// for i:=0 to aPolynomial2^.Count-1 do
//  lRes^.Insert(MonomialPtr(aPolynomial2^.Items^[i])^.MCopy);
// AddPolynomials := lRes;
 lRes:=new(PolynomialPtr,InitSorted(0,4,CompareMonomials));
 i:=0; j:=0;
 while (i < aPolynomial1^.Count) and (j < aPolynomial2^.Count) do
   begin
     lInt:=CompareMonomials(aPolynomial1^.Items^[i],aPolynomial2^.Items^[j]);
     case lInt of
     -1:
       begin
        lRes^.Insert(MonomialPtr(aPolynomial1^.Items^[i])^.MCopy);
        inc(i);
       end;
     0:
       begin
        lVal:=ComplexAdd(MonomialPtr(aPolynomial1^.Items^[i])^.nCoefficient,
                    MonomialPtr(aPolynomial2^.Items^[j])^.nCoefficient);
        if not IsEqWithInt(lVal,0) then
         begin
          lMonomial:=MonomialPtr(MonomialPtr(aPolynomial1^.Items^[i])^.MCopy);
          lMonomial^.nCoefficient:=lVal;
          lRes^.Insert(lMonomial);
         end;
        inc(i); inc(j);
       end;
     1:
       begin
        lRes^.Insert(MonomialPtr(aPolynomial2^.Items^[j])^.MCopy);
        inc(j);
       end;
     end;
   end;
 while i < aPolynomial1^.Count do
  begin
   lRes^.Insert(MonomialPtr(aPolynomial1^.Items^[i])^.MCopy);
   inc(i);
  end;
 while j < aPolynomial2^.Count do
  begin
   lRes^.Insert(MonomialPtr(aPolynomial2^.Items^[j])^.MCopy);
   inc(j);
  end;
 if lRes^.Count = 0 then
   lRes:=new(PolynomialPtr,InitWithNumeric(CZero));
 AddPolynomials:=lRes;
end;

function MultPolynomials(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;
var
 i,j: integer; lVal: RComplex;
 lMonomial: MonomialPtr;
 lRes: PolynomialPtr;
begin
 if aPolynomial1^.IsNumericEqualWith(CZero) or aPolynomial2^.IsNumericEqualWith(CZero) then
  begin
   MultPolynomials:=new(PolynomialPtr,InitWithNumeric(CZero));
   exit;
  end;
 lRes:=new(PolynomialPtr,InitSorted(0,4,CompareMonomials));
 for i:=0 to aPolynomial1^.Count-1 do
  for j:=0 to aPolynomial2^.Count-1 do
   begin
    lVal:=ComplexMult(MonomialPtr(aPolynomial1^.Items^[i])^.nCoefficient,
            MonomialPtr(aPolynomial2^.Items^[j])^.nCoefficient);
    lMonomial:=new(MonomialPtr,Init(lVal));
    lMonomial^.nPowerProduct.CopyNatFunc(MonomialPtr(aPolynomial1^.Items^[i])^.nPowerProduct);
    lMonomial^.nPowerProduct.Add(MonomialPtr(aPolynomial2^.Items^[j])^.nPowerProduct);
    lRes^.Insert(lMonomial);
   end;
 MultPolynomials:=lRes;
end;

function DiffPolynomials(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;
var //lPolynomial,
    lRes: PolynomialPtr;
    lMonomial: MonomialPtr;
    lVal: RComplex;
    i,j,lInt: integer;
begin
// lPolynomial:=NMultPolynomial(CMinusOne,aPolynomial2);
// DiffPolynomials:=AddPolynomials(aPolynomial1,lPolynomial);
// dispose(lPolynomial,Done);
 lRes:=new(PolynomialPtr,InitSorted(0,4,CompareMonomials));
 i:=0; j:=0;
 while (i < aPolynomial1^.Count) and (j < aPolynomial2^.Count) do
   begin
     lInt:=CompareMonomials(aPolynomial1^.Items^[i],aPolynomial2^.Items^[j]);
     case lInt of
     -1:
       begin
        lRes^.Insert(MonomialPtr(aPolynomial1^.Items^[i])^.MCopy);
        inc(i);
       end;
     0:
       begin
        lVal:=ComplexSub(MonomialPtr(aPolynomial1^.Items^[i])^.nCoefficient,
                    MonomialPtr(aPolynomial2^.Items^[j])^.nCoefficient);
        if not IsEqWithInt(lVal,0) then
         begin
          lMonomial:=MonomialPtr(MonomialPtr(aPolynomial1^.Items^[i])^.MCopy);
          lMonomial^.nCoefficient:=lVal;
          lRes^.Insert(lMonomial);
         end;
        inc(i); inc(j);
       end;
     1:
       begin
        lMonomial:=MonomialPtr(MonomialPtr(aPolynomial2^.Items^[j])^.MCopy);
        lVal:=ComplexNeg(MonomialPtr(aPolynomial2^.Items^[j])^.nCoefficient);
        lMonomial^.nCoefficient:=lVal;
        lRes^.Insert(lMonomial);
        inc(j);
       end;
     end;
   end;
 while i < aPolynomial1^.Count do
  begin
   lRes^.Insert(MonomialPtr(aPolynomial1^.Items^[i])^.MCopy);
   inc(i);
  end;
 while j < aPolynomial2^.Count do
  begin
   lMonomial:=MonomialPtr(MonomialPtr(aPolynomial2^.Items^[j])^.MCopy);
   lVal:=ComplexNeg(MonomialPtr(aPolynomial2^.Items^[j])^.nCoefficient);
   lMonomial^.nCoefficient:=lVal;
   lRes^.Insert(lMonomial);
   inc(j);
  end;
 if lRes^.Count = 0 then
   lRes:=new(PolynomialPtr,InitWithNumeric(CZero));
 DiffPolynomials:=lRes;
end;

function NthPower(aPolynomial: PolynomialPtr; N: integer): PolynomialPtr;
 var i: integer; lPolynomial,lRes: PolynomialPtr;
begin
 if N = 0 then
  begin
   NthPower:=new(PolynomialPtr,InitWithNumeric(COne));
   exit;
  end;
 lRes:=PolynomialPtr(aPolynomial^.MCopy);
 for i:=2 to N do
  begin
   lPolynomial:=lRes;
   lRes:=MultPolynomials(lRes,aPolynomial);
   dispose(lPolynomial,Done);
  end;
 NthPower:= lRes;
end;

function NMultPolynomial(const aNumber: RComplex; aPolynomial: PolynomialPtr): PolynomialPtr;
 var i: integer; lVal: RComplex; lRes: PolynomialPtr;
begin
 if IsEqWithInt(aNumber,0) then
  begin
   NMultPolynomial:=new(PolynomialPtr,InitWithNumeric(aNumber));
   exit;
  end;
 lRes:=PolynomialPtr(aPolynomial^.MCopy);
 if IsEqWithInt(aNumber,1) then
 begin
  NMultPolynomial:= lRes;
  exit;
 end;
 for i:=0 to lRes^.Count-1 do
  begin
   lVal:=ComplexMult(aNumber,MonomialPtr(lRes^.Items^[i])^.nCoefficient);
   MonomialPtr(lRes^.Items^[i])^.nCoefficient:=lVal;
  end;
 NMultPolynomial:=lRes;
end;

function MMultPolynomial(aMonomial: MonomialPtr; aPolynomial: PolynomialPtr): PolynomialPtr;
 var j: integer; lRes: PolynomialPtr;
begin
 if IsEqWithInt(aMonomial^.nCoefficient,0) then
  begin
   MMultPolynomial:=new(PolynomialPtr,InitWithNumeric(CZero));
   exit;
  end;
 if aMonomial^.IsNumeric then
 begin
  MMultPolynomial:= NMultPolynomial(aMonomial^.nCoefficient,aPolynomial);
  exit;
 end;
 lRes:=PolynomialPtr(aPolynomial^.MCopy);
 for j:=0 to lRes^.Count-1 do
  begin
   MonomialPtr(lRes^.Items^[j])^.nCoefficient:=
     ComplexMult(aMonomial^.nCoefficient,
                 MonomialPtr(aPolynomial^.Items^[j])^.nCoefficient);
   MonomialPtr(lRes^.Items^[j])^.nPowerProduct.Add(aMonomial^.nPowerProduct);
  end;
 MMultPolynomial := lRes;
end;

function SPolynomial(aPolynomial1,aPolynomial2: PolynomialPtr): PolynomialPtr;
 var lMonomial: MonomialPtr;
 begin
    if (aPolynomial1^.IsNumericEqualWith(CZero)) or (aPolynomial2^.IsNumericEqualWith(CZero)) then
    begin
       SPolynomial:=new(PolynomialPtr,InitWithNumeric(CZero));
       exit;
    end;
  lMonomial:=new(MonomialPtr,Init(COne));
  lMonomial^.nPowerProduct.LCM(MonomialPtr(PolynomialPtr(aPolynomial1)^.Items^[0])^.nPowerProduct,
                               MonomialPtr(PolynomialPtr(aPolynomial2)^.Items^[0])^.nPowerProduct);
  SPolynomial:=DiffPolynomials(MMultPolynomial(DivMonomials(lMonomial,PolynomialPtr(aPolynomial1)^.Items^[0]),aPolynomial1),
                          MMultPolynomial(DivMonomials(lMonomial,PolynomialPtr(aPolynomial2)^.Items^[0]),aPolynomial2));
  dispose(lMonomial,Done);
end;

function SimpleReduction(aPolynomial: PolynomialPtr; const aPolynomials: MList): PolynomialPtr;
var
   i	      : integer;
   nEq,tEq,aP : PolynomialPtr;
   lMonomial  : MonomialPtr;
   reduced    : boolean;
begin
   aP:=PolynomialPtr(aPolynomial^.MCopy);
   repeat
      reduced:=false;
      for i:=0 to aPolynomials.Count-1 do
	 if MonomialPtr(aP^.Items^[0])^.nPowerProduct.
	    IsMultipleOf(MonomialPtr(PolynomialPtr(aPolynomials.Items^[i])^.Items^[0])^.nPowerProduct) then
	 begin
	    lMonomial:=DivMonomials(aP^.Items^[0],PolynomialPtr(aPolynomials.Items^[i])^.Items^[0]);
	    MizAssert(4235,lMonomial<>nil);
	    nEq:=MMultPolynomial(lMonomial,aPolynomials.Items^[i]);
	    dispose(lMonomial,Done);
	    tEq:=DiffPolynomials(aP,nEq);
	    dispose(nEq,Done);
	    dispose(aP,Done);
	    aP:=tEq;
	    reduced:=true;
{	    if aPolynomial^.IsNumeric then}
	    break;
	 end;
   until not reduced;
   SimpleReduction:=aP;
end;

function Reduction(aPolynomial: PolynomialPtr; const aPolynomials: MList): PolynomialPtr;
var i			  : integer;
   rPolynomial,nEq,tEq,aP : PolynomialPtr;
   uMonomial		  : MonomialPtr;
   dividing		  : boolean;
begin
   aP:=PolynomialPtr(aPolynomial^.MCopy);
   new(rPolynomial,InitWithNumeric(CZero));
   repeat
      i:=0;
      dividing:=true;
      while (i<=aPolynomials.Count-1) and (dividing) do
	 if (MonomialPtr(aP^.Items^[0])^.nPowerProduct.
	    IsMultipleOf(MonomialPtr(PolynomialPtr(aPolynomials.Items^[i])^.Items^[0])^.nPowerProduct))
	    and (not PolynomialPtr(aPolynomials.Items^[i])^.IsNumericEqualWith(CZero)) then
	 begin
	    uMonomial:=DivMonomials(aP^.Items^[0],PolynomialPtr(aPolynomials.Items^[i])^.Items^[0]);
	    MizAssert(4235,uMonomial<>nil);
	    nEq:=MMultPolynomial(uMonomial,aPolynomials.Items^[i]);
	    dispose(uMonomial,Done);
	    tEq:=DiffPolynomials(aP,nEq);
	    dispose(nEq,Done);
	    dispose(aP,Done);
	    aP:=tEq;
	    dividing:=false;
	 end
	 else inc(i);
      if dividing then
      begin
	 new(nEq,InitWithNumeric(COne));
	 tEq:=MMultPolynomial(MonomialPtr(aP^.Items^[0]),nEq);
	 dispose(nEq,Done);
	 nEq:=AddPolynomials(rPolynomial,tEq);
	 dispose(rPolynomial,Done);
	 rPolynomial:=nEq;
	 nEq:=DiffPolynomials(aP,tEq);	 
	 dispose(aP,Done);
	 aP:=nEq;
	 dispose(tEq,Done);
      end;
   until aP^.IsNumericEqualWith(CZero);
   dispose(aP,Done);
   Reduction:=rPolynomial;
end;

procedure ReduceBasis(var aPolynomials : MList);
var
   fPol,fRed	: PolynomialPtr;
   i,j		: integer;
   nPolynomials	: MList;
begin
   j:=0;
   while j<>aPolynomials.Count do
      for i:=0 to aPolynomials.Count-1 do
      begin
	 nPolynomials.CopyList(aPolynomials);
	 fPol:=PolynomialPtr(PolynomialPtr(nPolynomials.Items^[i])^.MCopy);
	 nPolynomials.Items^[i]:=nil;
	 nPolynomials.Pack;
	 fRed:=Reduction(fPol,nPolynomials);
	 nPolynomials.Done;
	 if ComparePolynomials(aPolynomials.Items^[i],fRed)<>0 then
	 begin
	    dispose(PolynomialPtr(aPolynomials.Items^[i]),Done);
	    aPolynomials.Items^[i]:=fRed;
	    j:=0;
	    break;
	 end
	 else
	 begin
	    dispose(fRed,Done);
	    inc(j);
	 end;
      end;
end;

procedure ExtendToGroebnerBasis0(var aPolynomials : MList);
var
   lMon,rMon	   : MonomialPtr;
   fPol,fRed : PolynomialPtr;
   i,j		   : integer;
   bif		   : IntRel;
begin
   {$IFDEF GDEBUG}
   writeln(infofile,'^^^^^^^^^^^^Groebner0:');
   for i:=0 to aPolynomials.Count-1 do
   begin
      write(infofile,i,': ');
      infopolynomial(aPolynomials.Items^[i]);
      infonewline;
   end;
   {$ENDIF};
   ReduceBasis(aPolynomials);
   {$IFDEF GDEBUG}
   writeln(infofile,'^^^^^^^^^^^^Reduced Groebner0:');
   for i:=0 to aPolynomials.Count-1 do
   begin
      write(infofile,i,': ');
      infopolynomial(aPolynomials.Items^[i]);
      infonewline;
   end;
   {$ENDIF};
   bif.Init(aPolynomials.Count);
   for i:=0 to aPolynomials.Count-2 do
      for j:=i+1 to aPolynomials.Count-1 do
      begin
	 bif.AssignPair(i,j);
      end;
   {$IFDEF GDEBUG}
   for i:=0 to bif.Count-1 do
      write(infofile,'(',bif.Items^[i].X,',',bif.Items^[i].Y,')');
   infonewline;
   {$ENDIF};
   while bif.Count>0 do
   begin
      fPol:=SPolynomial(aPolynomials.Items^[bif.Items^[0].X],aPolynomials.Items^[bif.Items^[0].Y]);
      {$IFDEF GDEBUG}
      write(infofile,'spoly=',bif.Items^[0].X,' ',bif.Items^[0].Y,' = ');infopolynomial(fpol);infonewline;      
      {$ENDIF};
      
      lMon:=new(MonomialPtr,Init(COne));
      rMon:=new(MonomialPtr,Init(COne));
      lMon^.nPowerProduct.LCM(MonomialPtr(PolynomialPtr(aPolynomials.Items^[bif.Items^[0].X])^.Items^[0])^.nPowerProduct,
			      MonomialPtr(PolynomialPtr(aPolynomials.Items^[bif.Items^[0].Y])^.Items^[0])^.nPowerProduct);
      
      rMon^.nPowerProduct.CopyNatFunc(MonomialPtr(PolynomialPtr(aPolynomials.Items^[bif.Items^[0].X])^.Items^[0])^.nPowerProduct);
      rMon^.nPowerProduct.Add(MonomialPtr(PolynomialPtr(aPolynomials.Items^[bif.Items^[0].Y])^.Items^[0])^.nPowerProduct);
      bif.AtDelete(0);
      if CompareMonomials(lMon,rMon) <> 0 then
      begin      
	 fRed:=Reduction(fPol,aPolynomials);
	 dispose(fPol,Done);
	 {$IFDEF GDEBUG}
	 infostring('fred=');infopolynomial(fred);infonewline;
	 {$ENDIF};
	 if not fRed^.IsNumericEqualWith(CZero) then
	 begin
	    for i:=0 to aPolynomials.Count-1 do
	       bif.AssignPair(i,aPolynomials.Count);
	    aPolynomials.Insert(fRed);
	    {$IFDEF GDEBUG}
	    for i:=0 to bif.Count-1 do
	       write(infofile,'(',bif.Items^[i].X,',',bif.Items^[i].Y,')');
	    infonewline;
	    {$ENDIF};
	 end
	 else dispose(fRed,Done);
      end
      else
      begin
	 dispose(fPol,Done);
	 {$IFDEF GDEBUG}
	 infostring('fred(II)=0');infonewline;
	 {$ENDIF};
      end;	    
      dispose(lMon,Done);
      dispose(rMon,Done);
   end;
end;

procedure ExtendToGroebnerBasis(var aPolynomials : MList);
var
   lMon,rMon	   : MonomialPtr;
   fPol,fRed,fRed1 : PolynomialPtr;
   i,j		   : integer;
   ps		   : MCollection;
   nP		   : MList;
begin
   GroebnerOverflow:=false;
   {$IFDEF GDEBUG}
   writeln(infofile,'^^^^^^^^^^^^Groebner:');
   for i:=0 to aPolynomials.Count-1 do
   begin
      write(infofile,i,': ');
      infopolynomial(aPolynomials.Items^[i]);
      infonewline;
   end;
   {$ENDIF};
   ReduceBasis(aPolynomials);
   {$IFDEF GDEBUG}
   writeln(infofile,'^^^^^^^^^^^^Reduced Groebner:');
   for i:=0 to aPolynomials.Count-1 do
   begin
      write(infofile,i,': ');
      infopolynomial(aPolynomials.Items^[i]);
      infonewline;
   end;
   {$ENDIF};
   ps.Init(aPolynomials.Count,20);
   for i:=0 to aPolynomials.Count-2 do
      for j:=i+1 to aPolynomials.Count-1 do
      begin
	 lMon:=new(MonomialPtr,Init(COne));
	 rMon:=new(MonomialPtr,Init(COne));
	 lMon^.nPowerProduct.LCM(MonomialPtr(PolynomialPtr(aPolynomials.Items^[i])^.Items^[0])^.nPowerProduct,
				 MonomialPtr(PolynomialPtr(aPolynomials.Items^[j])^.Items^[0])^.nPowerProduct);
      
	 rMon^.nPowerProduct.CopyNatFunc(MonomialPtr(PolynomialPtr(aPolynomials.Items^[i])^.Items^[0])^.nPowerProduct);
	 rMon^.nPowerProduct.Add(MonomialPtr(PolynomialPtr(aPolynomials.Items^[j])^.Items^[0])^.nPowerProduct);
	 if CompareMonomials(lMon,rMon) <> 0 then
	 begin
	    ps.Insert(SPolynomial(aPolynomials.Items^[i],aPolynomials.Items^[j]));
	 end;   
	 dispose(lMon,Done);
	 dispose(rMon,Done);
      end;
   while ps.Count>0 do
   begin
      if ps.count>19 then begin GroebnerOverflow:=true; exit; end;
      fPol:=ps.At(ps.Count-1);
      ps.AtDelete(ps.Count-1);
      {$IFDEF GDEBUG}
      infostring('fpol=');infopolynomial(fpol);infonewline;
      {$ENDIF};
      fRed:=Reduction(fPol,aPolynomials);
      dispose(fPol,Done);
      {$IFDEF GDEBUG}
      infostring('fred=');infopolynomial(fred);infonewline;
      {$ENDIF};
      if not fRed^.IsNumericEqualWith(CZero) then
      begin
	 nP.Init(1);
	 nP.Insert(fRed^.MCopy);
	 for i:=0 to aPolynomials.Count-1 do
	 begin
	    {$IFDEF GDEBUG}
	    writeln(infofile,'trying to reduce ',i);
	    {$ENDIF};
	    fPol:=PolynomialPtr(PolynomialPtr(aPolynomials.Items^[i])^.MCopy);
	    fRed1:=Reduction(fPol,nP);
	    dispose(fPol,Done);
	    if ComparePolynomials(fRed1,aPolynomials.Items^[i])<>0 then
	    begin
	       {$IFDEF GDEBUG}
	       writeln(infofile,'reduced');
	       {$ENDIF};
	       ps.Insert(fRed1);
	       dispose(PolynomialPtr(aPolynomials.Items^[i]),Done);
	       aPolynomials.Items^[i]:=nil;
	    end
	    else dispose(fRed1,Done);
	 end;
	 aPolynomials.Pack;
	 aPolynomials.Insert(fRed);
	 for i:=0 to aPolynomials.Count-2 do
	 begin
	    lMon:=new(MonomialPtr,Init(COne));
	    rMon:=new(MonomialPtr,Init(COne));
	    lMon^.nPowerProduct.LCM(MonomialPtr(PolynomialPtr(aPolynomials.Items^[i])^.Items^[0])^.nPowerProduct,
				 MonomialPtr(PolynomialPtr(aPolynomials.Items^[aPolynomials.Count-1])^.Items^[0])^.nPowerProduct);
	    
	    rMon^.nPowerProduct.CopyNatFunc(MonomialPtr(PolynomialPtr(aPolynomials.Items^[i])^.Items^[0])^.nPowerProduct);
	    rMon^.nPowerProduct.Add(MonomialPtr(PolynomialPtr(aPolynomials.Items^[aPolynomials.Count-1])^.Items^[0])^.nPowerProduct);
	    if CompareMonomials(lMon,rMon) <> 0 then
	    begin
	       ps.Insert(SPolynomial(aPolynomials.Items^[i],aPolynomials.Items^[aPolynomials.Count-1]));
	    end;
	    dispose(lMon,Done);
	    dispose(rMon,Done);
	 end;
	 nP.Done;
      end
      else dispose(fRed,Done);
   end;
end;

function CheckGroebnerBasis(aPolynomials : MList):boolean;
var i,j	     : Integer;
   fPol,fRed : PolynomialPtr;
begin
   CheckGroebnerBasis:=true;
   for i:=0 to aPolynomials.Count-2 do
      for j:=i+1 to aPolynomials.Count-1 do
      begin
	 fPol:=Spolynomial(PolynomialPtr(aPolynomials.Items^[i]),PolynomialPtr(aPolynomials.Items^[j]));
	 {$IFDEF GDEBUG}
	 write(infofile,'Spoly ',i,' ',j,' = ');infopolynomial(fPol);infonewline;
	 {$ENDIF};
	 fRed:=Reduction(fPol,aPolynomials);
	 dispose(fPol,Done);
	 {$IFDEF GDEBUG}
	 write(infofile,'Reduced = ');infopolynomial(fRed);infonewline;
	 {$ENDIF};
	 if not fRed^.IsNumericEqualWith(CZero) then begin CheckGroebnerBasis:=false; dispose(fRed,Done); exit; end
	 else
	 dispose(fRed,Done);
      end;   
end;

procedure RecodeVariableOrder(var aPolynomials : MList;EqCount:Integer;var aTransOrder:NatFuncPtr);
var
   uniq,nuniq : NatFuncPtr;
   i,j,k,e	      : Integer;
begin
   new(uniq,InitNatFunc(10,10));
   new(nuniq,InitNatFunc(10,10));
   new(aTransOrder,InitNatFunc(10,10));
   for i:=0 to EqCount-1 do
      with PolynomialPtr(aPolynomials.Items^[i])^ do
	 for j:=0 to Count-1 do
	    with MonomialPtr(Items^[j])^.nPowerProduct do
	       if Count>1 then
		  for k:=0 to Count-1 do
		     nuniq.Up(Items^[k].X);
		     
   for i:=0 to EqCount-1 do
      with PolynomialPtr(aPolynomials.Items^[i])^ do
	 for j:=0 to Count-1 do
	    with MonomialPtr(Items^[j])^.nPowerProduct do
	       if (Count=1) and (not nuniq.HasInDom(Items^[0].X))then
		  if Items^[0].Y=1 then uniq.Up(Items^[0].X);
   dispose(nuniq,Done);
   e:=1;
   for i:=0 to uniq.Count-1 do
      with uniq.Items^[i] do
	 if Y=1 then
	 begin
	    aTransOrder.Assign(X,e);
	    inc(e);
	 end;
   dispose(uniq,Done);

   for i:=0 to aPolynomials.Count-1 do
      with PolynomialPtr(aPolynomials.Items^[i])^ do
	 for j:=0 to Count-1 do
	 begin
	    for k:=0 to MonomialPtr(Items^[j])^.nPowerProduct.Count-1 do
	       if not aTransOrder.HasInDom(MonomialPtr(Items^[j])^.nPowerProduct.Items^[k].X) then
	       begin
		  aTransOrder.Assign(MonomialPtr(Items^[j])^.nPowerProduct.Items^[k].X,e);
		  inc(e);
	       end;
	 end;
end;

procedure RecodeVariables(var aPolynomialValues : MList; aTransOrder:NatFuncPtr);
var
   i,j,k: Integer;
   l	       : MCollection;
   p	       : PolynomialPtr;
   m	       : MonomialPtr;

begin
   l.CopyList(aPolynomialValues);
   aPolynomialValues.FreeAll;
   for i:=0 to l.Count-1 do
      with PolynomialPtr(l.Items^[i])^ do
	 for j:=0 to Count-1 do
	 begin
	    new(m,Init(MonomialPtr(Items^[j])^.nCoefficient));	       
	    for k:=0 to MonomialPtr(Items^[j])^.nPowerProduct.Count-1 do
	       m^.nPowerProduct.Assign(aTransOrder.Value(MonomialPtr(Items^[j])^.nPowerProduct.Items^[k].X),MonomialPtr(Items^[j])^.nPowerProduct.Items^[k].Y);
	    dispose(MonomialPtr(Items^[j]),Done);
	    Items^[j]:=m;
	 end;
   
   for i:=0 to l.Count-1 do
   begin
      new(p,Init);
      with PolynomialPtr(l.Items^[i])^ do	 
	 for j:=0 to Count-1 do
	    p.Insert(MonomialPtr(Items^[j])^.MCopy);
      aPolynomialValues.Insert(p);
   end;
   l.Done;
end;

constructor PolynomialObj.Init;
begin
 InitSorted(0,4,CompareMonomials);
end;

destructor PolynomialObj.Done;
begin
 inherited Done;
end;

constructor PolynomialObj.InitWithNumeric(const aValue: RComplex);
begin
 InitSorted(0,4,CompareMonomials);
 Insert(new(MonomialPtr,Init(aValue)));
end;

constructor PolynomialObj.InitWithMonomial(const aCoefficient: RComplex; aVarNr: integer);
begin
 InitSorted(0,4,CompareMonomials);
 Insert(new(MonomialPtr,InitMonomial(aCoefficient,aVarNr)));
end;

procedure PolynomialObj.Insert(aItem: Pointer);
 var lInd: integer; lVal: RComplex;
begin
  if Search(aItem,lInd) then
   begin
    lVal:=ComplexAdd(MonomialPtr(aItem)^.nCoefficient,
                     MonomialPtr(Items^[lInd])^.nCoefficient);
    if IsEqWithInt(lVal,0) then
     begin
      if Count > 1 then
       begin AtDelete(lInd);
        dispose(MonomialPtr(aItem),Done);
        exit
       end;
      if MonomialPtr(Items^[lInd])^.nPowerProduct.Count > 0 then
       begin
        MonomialPtr(Items^[lInd])^.nPowerProduct.DeleteAll;
        MonomialPtr(Items^[lInd])^.nPowerProduct.SetLimit(0);
       end;
     end;
    MonomialPtr(Items^[lInd])^.nCoefficient:=lVal;
    dispose(MonomialPtr(aItem),Done);
   end
  else
   begin
    if IsEqWithInt(MonomialPtr(aItem)^.nCoefficient,0) and (Count>0) then
     begin
      dispose(MonomialPtr(aItem),Done);
      exit;
     end;
    if (Count>0) and IsEqWithInt(MonomialPtr(Items^[0])^.nCoefficient,0) then
     begin
      dispose(MonomialPtr(Items^[0]),Done);
      AtDelete(0);
     end;
    AtInsert(lInd,aItem);
//    if IsEqWithInt(MonomialPtr(Items^[0])^.nCoefficient,0) and (Count>1) then
//     begin AtDelete(0); exit end;
   end;
end;

function PolynomialObj.IsNumeric: boolean;
begin IsNumeric:=false;
 if Count = 1 then
  IsNumeric := MonomialPtr(Items^[0])^.nPowerProduct.Count = 0;
end;

function PolynomialObj.IsNumericEqualWith(const aNumber: RComplex): boolean;
 var lNumber: RComplex;
begin IsNumericEqualWith:=false;
 if isNumeric then
  begin GetNumeric(lNumber);
   IsNumericEqualWith:=CompareComplex(lNumber,aNumber)=0;
  end;
end;

function PolynomialObj.IsUniVariantVariable: integer;
begin IsUniVariantVariable:=0;
 if (Count = 1) and MonomialPtr(Items^[0])^.IsUniVariant
     and (MonomialPtr(Items^[0])^.nPowerProduct.Items^[0].Y = 1) then
  IsUniVariantVariable := MonomialPtr(Items^[0])^.nPowerProduct.Items^[0].X;
end;

function PolynomialObj.IsVariable: integer;
begin IsVariable:=0;
 if Count = 1 then
  IsVariable := MonomialPtr(Items^[0])^.isVariable;
end;

procedure PolynomialObj.GetNumeric(var aNumber: RComplex);
begin
 if Count > 0 then
  if MonomialPtr(Items^[0])^.nPowerProduct.Count = 0 then
   aNumber := MonomialPtr(Items^[0])^.nCoefficient;
end;

function PolynomialObj.Misses(var aVars:NatSet): boolean;
 var i: integer;
begin
 for i:=0 to Count-1 do
  if not MonomialPtr(Items^[i])^.nPowerProduct.Misses(aVars) then
   begin Misses:=false; exit end;
 Misses:=true;
end;

function PolynomialObj.HasTheVariable(aVarNr: integer): boolean;
 var i: integer;
begin HasTheVariable:=false;
 for i:=0 to Count-1 do
  if MonomialPtr(Items^[i])^.nPowerProduct.HasInDom(aVarNr) then
   begin HasTheVariable:=true; exit end;
end;

procedure PolynomialObj.InsertValue(aVarNr: integer; aPolynomial: PolynomialPtr);
 var lPol:PolynomialObj;
     lPower: integer;
     lPolynomial,mPolynomial: PolynomialPtr;
begin
 lPol.MoveCollection(Self);
 SetLimit(lPol.Count);
 while lPol.Count >0 do
 if MonomialPtr(lPol.Items^[0])^.nPowerProduct.HasInDom(aVarNr) then
  begin
   if not aPolynomial^.IsNumericEqualWith(CZero) then
    begin
     lPower:=MonomialPtr(lPol.Items^[0])^.nPowerProduct.Value(aVarNr);
     MonomialPtr(lPol.Items^[0])^.nPowerProduct.DeleteElem(aVarNr);
     lPolynomial:=NthPower(aPolynomial,lPower);
     mPolynomial:=MMultPolynomial(lPol.Items^[0],lPolynomial);
     dispose(lPolynomial,Done);
     AppendTo(mPolynomial^);
     dispose(mPolynomial,Done);
    end;
   lPol.AtFree(0);
  end
 else
  begin Insert(lPol.Items^[0]);
   lPol.AtDelete(0);
  end;
 if Count = 0 then
  Insert(new(MonomialPtr,Init(CZero)));
 lPol.Done;
end;

procedure GaussElimination(var aEqs: MSortedCollection; var aContr: integer);
 var i,j,v,lVar: integer;
     lEq,nEq,tEq: PolynomialPtr;
     lEquations: MSortedCollection;
begin
{$IFDEF GDEBUG}
writeln(infofile,'Przed Gauss');
for i:=0 to aEqs.Count-1 do
begin
write(infofile,i,': ');
infopolynomial(aEqs.Items^[i]);
infonewline;
end;
{$ENDIF};
// normalize  polynomials
 for i:=0 to aEqs.Count-1 do
  if not IsEqWithInt(MonomialPtr(PolynomialPtr(aEqs.Items^[i])^.Items^[0])^.nCoefficient,1) then
  begin
   lEq:=NMultPolynomial(ComplexInv(MonomialPtr(PolynomialPtr(aEqs.Items^[i])^.Items^[0])^.nCoefficient),
                                aEqs.Items^[i]);
   dispose(PolynomialPtr(aEqs.Items^[i]),Done);
   aEqs.Items^[i]:=lEq;
  end;
 if aEqs.Count <= 1 then exit;
 lEquations.InitSorted(0,4,ComparePolynomials);
 while aEqs.Count > 0 do
  begin
   lEq:=aEqs.Items^[0];
   lVar:=MonomialPtr(lEq^.Items^[0])^.nPowerProduct.Items^[0].X;
   aEqs.AtDelete(0);
   while aEqs.Count > 0 do
   begin
    if lVar <> MonomialPtr(PolynomialPtr(aEqs.Items^[0])^.Items^[0])^.nPowerProduct.Items^[0].X
     then break;
    nEq:=DiffPolynomials(lEq,aEqs.Items^[0]);
    dispose(PolynomialPtr(aEqs.Items^[0]),Done);
    aEqs.AtDelete(0);
    if nEq^.IsNumeric then
     begin
      if not nEq^.IsNumericEqualWith(CZero) then
       begin dispose(nEq,Done); aContr:=44; exit end;
      dispose(nEq,Done);
     end
    else if IsEqWithInt(MonomialPtr(nEq^.Items^[0])^.nCoefficient,1) then
      aEqs.Insert(nEq)
     else
      begin
       tEq:=NMultPolynomial(ComplexInv(MonomialPtr(nEq^.Items^[0])^.nCoefficient),nEq);
       dispose(nEq,Done);
       aEqs.Insert(tEq);
      end;
   end;
   lEquations.Insert(lEq);
  end;
{$IFDEF GDEBUG}
writeln(infofile,'Gauss - po 1 etap');
for i:=0 to lequations.Count-1 do
begin
write(infofile,i,': ');
infopolynomial(lequations.Items^[i]);
infonewline;
end;
{$ENDIF};
 for i:=1 to lEquations.Count-1 do
  begin
    lEq:=lEquations.Items^[i];
    lVar:=MonomialPtr(lEq^.Items^[0])^.nPowerProduct.Items^[0].X;
    for j:=0 to i-1 do
    begin
     v:=0;
     while (v < PolynomialPtr(lEquations.Items^[j])^.Count) and
           (MonomialPtr(PolynomialPtr(lEquations.Items^[j])^.Items^[v])^.nPowerProduct.Count > 0 ) and
           (lVar > MonomialPtr(PolynomialPtr(lEquations.Items^[j])^.Items^[v])^.nPowerProduct.Items^[0].X) do
       inc(v);
     if (v < PolynomialPtr(lEquations.Items^[j])^.Count) and
        (MonomialPtr(PolynomialPtr(lEquations.Items^[j])^.Items^[v])^.nPowerProduct.Count > 0 ) and
        (lVar = MonomialPtr(PolynomialPtr(lEquations.Items^[j])^.Items^[v])^.nPowerProduct.Items^[0].X) then
      begin
       if IsEqWithInt(MonomialPtr(PolynomialPtr(lEquations.Items^[j])^.Items^[v])^.nCoefficient,1) then
        tEq:=DiffPolynomials(lEquations.Items^[j],lEq)
       else
        begin
         nEq:=NMultPolynomial(MonomialPtr(PolynomialPtr(lEquations.Items^[j])^.Items^[v])^.nCoefficient,lEq);
         tEq:=DiffPolynomials(lEquations.Items^[j],nEq);
         dispose(nEq,Done);
        end;
       dispose(PolynomialPtr(lEquations.Items^[j]),Done);
       lEquations.Items^[j]:=tEq;
      end;
    end;
  end;
 aEqs.Done;
 aEqs:=lEquations;
{$IFDEF GDEBUG}
writeln(infofile,'Po Gauss');
for i:=0 to aEqs.Count-1 do
begin
write(infofile,i,': ');
infopolynomial(aEqs.Items^[i]);
infonewline;
end;
{$ENDIF};
end;

function LinearEquationReduce(aEq: PolynomialPtr; const aEquations: MList): PolynomialPtr;
  var i: integer;
      nEq,tEq: PolynomialPtr;
begin
 for i:=0 to aEquations.Count-1 do
  if MonomialPtr(PolynomialPtr(aEquations.Items^[i])^.Items^[0])^.nPowerProduct.Items^[0].X
      = MonomialPtr(aEq^.Items^[0])^.nPowerProduct.Items^[0].X then
   begin
     if IsEqWithInt(MonomialPtr(aEq^.Items^[0])^.nCoefficient,1) then
      begin
       tEq:=DiffPolynomials(aEq,aEquations.Items^[i]);
      end
     else
      begin
       nEq:=NMultPolynomial(MonomialPtr(aEq^.Items^[0])^.nCoefficient,aEquations.Items^[i]);
       tEq:=DiffPolynomials(aEq,nEq);
       dispose(nEq,Done);
      end;
     dispose(aEq,Done);
     aEq:=tEq;
     if aEq^.IsNumeric then break;
   end;
 LinearEquationReduce:=aEq;
end;

end.
