(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit req_info;

// Functions reporting usage of (now only numerical)
// requirements in checker

interface
uses mobjects,numbers,polynom,builtin,iocorrel,correl,enums;

const NumRequirement = [
 {--- HIDDEN             ---}
 rqEqualsTo,            //2
 {--- XCMPLX_0/ARITHM     ---}
 rqRealAdd,             //12
 rqRealMult,            //13
 {--- XREAL_0/REAL        ---}
 rqLessOrEqual,         //14
 {--- ORDINAL1/NUMERALS   ---}
 rqSucc,                //15
 {--- XCMPLX_0/ARITHM     ---}
 rqRealNeg,             //21
 rqRealInv,             //22
 rqRealDiff,            //23
 rqRealDiv             //24
 ];

// All are now functors - we use it below
const FuncNumReqs1 = [rqSucc, rqRealNeg, rqRealInv];
// Either predicates or functors -p we use it below
const PredNumReqs2 = [rqEqualsTo, rqLessOrEqual];
const FuncNumReqs2 = [rqRealAdd, rqRealMult, rqRealDiff, rqRealDiv];

const errBadReqArity = 7700; // bad arity requirement passed

type
 OutRQInfObj = object(OutVRFFileObj)
  procedure Out_NumReq1(r:Requirement; const z1 : RComplex);
  procedure Out_NumReq2(r:Requirement; const z1,z2 : RComplex);
  procedure Out_NumReq3(r:Requirement; const z1,z2,z3 : RComplex);
  procedure Out_NegNumReq2(r:Requirement; const z1,z2 : RComplex);
  procedure Out_NumReq2Val(r:Requirement; const z1,z2 : RComplex; aVal:boolean);
  procedure Out_PolyReq1(r:Requirement; aPol: PolynomialPtr);
  procedure Out_PolyReq2(r:Requirement; aPol1,aPol2: PolynomialPtr);
  procedure Out_Requirement(r:Requirement; aKind: ConstructorsKind);
  procedure Out_RationalNr(const r: Rational);
  procedure Out_ComplexNr(const z: RComplex);
  procedure Out_Monomial(aMon: MonomialPtr);
  procedure Out_Polynomial(aPol: PolynomialPtr);
 
 end;

implementation
uses errhan,xmldict,inout;


(* ##RNC:
## Builtin numerical evaluations of arithmetical functors 
## and predicates. For predicates, the value can be false.
## Arguments are generally polynoms with complex coefficients.
## For functors, the last polynom is the result value.
elPolyEval =
 element elPolyEval {
   Position,
   attribute atValue { xsd:boolean }?,
   elRequirement,
   GeneralPolynomial+
 }
*)
procedure OutRQInfObj.Out_NumReq1(r:Requirement; const z1 : RComplex);
begin
 // Mizassert( errBadReqArity, r in FuncNumReqs1);
  // only existing requirements are printed
 if gBuiltIn[r] > 0 then
 begin
  Out_XElStart( elPolyEval);
  Out_PosAsAttrs( CurPos);
  Out_XAttrEnd;
  Out_Requirement(r, coFunctor);
  Out_ComplexNr(z1);
  Out_XElEnd( elPolyEval);
 end;
end;

procedure OutRQInfObj.Out_NumReq2(r:Requirement; const z1,z2 : RComplex);
begin Out_NumReq2Val(r, z1, z2, true); end;

procedure OutRQInfObj.Out_NegNumReq2(r:Requirement; const z1,z2 : RComplex);
begin Out_NumReq2Val(r, z1, z2, false); end;

procedure OutRQInfObj.Out_NumReq2Val(r:Requirement; const z1,z2 : RComplex;
                                    aVal: boolean);
var lKind: ConstructorsKind;
begin
 if r in FuncNumReqs1 then lKind:= coFunctor
 else if r in PredNumReqs2 then lKind:= coPredicate
 else Mizassert( errBadReqArity, false);
 // only existing requirements are printed
 if gBuiltIn[r] > 0 then
 begin
  Out_XElStart( elPolyEval);
  Out_PosAsAttrs( CurPos);
  if not aVal then  Out_XAttr( atValue, 'false');
  Out_XAttrEnd;
  Out_Requirement(r, lKind);
  Out_ComplexNr(z1);
  Out_ComplexNr(z2);
  Out_XElEnd( elPolyEval);
 end;
end;

procedure OutRQInfObj.Out_NumReq3(r:Requirement; const z1,z2,z3 : RComplex);
begin
 Mizassert( errBadReqArity, r in FuncNumReqs2);
 // only existing requirements are printed
 if gBuiltIn[r] > 0 then
 begin
  Out_XElStart( elPolyEval);
  Out_PosAsAttrs( CurPos);
  Out_XAttrEnd;
  Out_Requirement(r, coFunctor);
  Out_ComplexNr(z1);
  Out_ComplexNr(z2);
  Out_ComplexNr(z3);
  Out_XElEnd( elPolyEval);
 end
end;


procedure OutRQInfObj.Out_PolyReq1(r:Requirement; aPol: PolynomialPtr);
begin
 Mizassert( errBadReqArity, r in FuncNumReqs1);
 Out_XElStart( elPolyEval);
 Out_PosAsAttrs( CurPos);
 Out_XAttrEnd;
 Out_Requirement(r, coFunctor);
 Out_Polynomial( aPol);
 Out_XElEnd( elPolyEval);
end;

procedure OutRQInfObj.Out_PolyReq2(r:Requirement; aPol1,aPol2: PolynomialPtr);
var lKind: ConstructorsKind;
begin
 if r in FuncNumReqs2 then lKind:= coFunctor
 else if r in PredNumReqs2 then lKind:= coPredicate
 else Mizassert( errBadReqArity, false);
 Out_XElStart( elPolyEval);
 Out_PosAsAttrs( CurPos);
 Out_XAttrEnd;
 Out_Requirement(r, lKind);
 Out_Polynomial( aPol1);
 Out_Polynomial( aPol2);
 Out_XElEnd( elPolyEval);
end;

// description is in impobjs
procedure OutRQInfObj.Out_Requirement(r:Requirement; aKind:ConstructorsKind);
begin
  Out_XElStart( elRequirement);
  Out_XAttr( atReqName, ReqName[r]);
  Out_XAttr( atConstrKind, ConstructorRepr( aKind));
  Out_XIntAttr( atConstrNr, Transf( aKind, gBuiltIn[r]));
  Out_XIntAttr( atNr, ord(r));
  Out_XElEnd0;
end;

(* ##RNC:
## Rational numbers
elRationalNr =
 element elRationalNr {
   attribute atNumerator { xsd:integer },
   attribute atDenominator { xsd:integer }
 }
*)
procedure OutRQInfObj.Out_RationalNr(const r: Rational);
begin 
  Out_XElStart( elRationalNr);
  Out_XAttr( atNumerator, r.Num);
  Out_XAttr( atDenominator, r.Den);
  Out_XElEnd0;
end;
  
(* ##RNC:
Number =
   ( elRationalNr | elComplexNr )
   
## Complex rational numbers used in Mizar
elComplexNr = element elComplexNr {elRationalNr, elRationalNr}
*)
procedure OutRQInfObj.Out_ComplexNr(const z: RComplex);
begin
 if (z.Im.Num = '0') then Out_RationalNr( z.Re) else
 begin
  Out_XElStart0( elComplexNr);
  Out_RationalNr( z.Im);
  Out_RationalNr( z.Re);
  Out_XElEnd( elComplexNr);
 end;
end;

(* ##RNC:
## Monomial has a coefficient and a series of variables with their exponents. 
elMonomial =
 element elMonomial {
   Number,
   element elPoweredVar {
     attribute atNr { xsd:integer },
     attribute atExponent { xsd:integer }
   }*
}   
*)
procedure OutRQInfObj.Out_Monomial(aMon: MonomialPtr);
var k: integer;
begin
 with aMon^ do
 begin
  Out_XElStart0( elMonomial);
  Out_ComplexNr( nCoefficient);
  for k:=0 to nPowerProduct.Count-1 do
  begin
   Out_XElStart( elPoweredVar);
   Out_XIntAttr( atNr, nPowerProduct.items^[k].X);
   Out_XIntAttr( atExponent, nPowerProduct.items^[k].Y);
   Out_XElEnd0;
  end;
  Out_XElEnd( elMonomial);
 end;
end;

(* ##RNC:
GeneralPolynomial =
   ( elPolynomial | Number )
   
## Polynomial consists of several monomials.
elPolynomial = element elPolynomial { elMonomial+}
*)
procedure OutRQInfObj.Out_Polynomial(aPol: PolynomialPtr);
var z: RComplex; j: integer;
begin
 if aPol^.IsNumeric then
 begin aPol^.GetNumeric(z); Out_ComplexNr(z) end
 else with aPol^ do
 begin
  Out_XElStart0( elPolynomial);
  for j:=0 to Count-1 do
   Out_Monomial(MonomialPtr(Items^[j]));
  Out_XElEnd( elPolynomial);
 end;
end;




end.
