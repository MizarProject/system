(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit builtin;

interface

uses limits;

type

 Requirement = ( rqNone,
                  {--- HIDDEN             ---}
                 rqAny,                 //1
                 rqSetMode,             //2
                 rqEqualsTo,            //3
                 rqBelongsTo,           //4
                  {--- XBOOLE_0/BOOLE      ---}
                 rqEmpty,               //5
                 rqEmptySet,            //6
                  {--- SUBSET_1/SUBSET     ---}
                 rqElement,             //7
                  {--- ZFMISC_1/SUBSET ---}
                 rqPowerSet,            //8
                  {--- TARSKI/SUBSET       ---}
                 rqInclusion,           //9
                  {--- SUBSET_1/SUBSET     ---}
                 rqSubDomElem,          //10
                  {--- NUMBERS             ---}
                 rqRealDom,             //11
                 rqNatDom,              //12
                  {--- XCMPLX_0/ARITHM     ---}
                 rqRealAdd,             //13
                 rqRealMult,            //14
                  {--- XREAL_0/REAL        ---}
                 rqLessOrEqual,         //15
                  {--- ORDINAL1/NUMERALS   ---}
                 rqSucc,                //16
                  {--- XBOOLE_0/BOOLE)     ---}
                 rqUnion,               //17
                 rqIntersection,        //18
                 rqSubtraction,         //19
                 rqSymmetricDifference, //20
                 rqMeets,               //21
                  {--- XCMPLX_0/ARITHM     ---}
                 rqRealNeg,             //22
                 rqRealInv,             //23
                 rqRealDiff,            //24
                 rqRealDiv,             //25
                  {--- XREAL_0             ---}
                 rqReal,                //26
                  {--- XREAL_0/REAL        ---}
                 rqPositive,            //27
                 rqNegative,            //28
                  {--- ORDINAL1/NUMERALS   ---}
                 rqNatural,             //29
                  {--- XCMPLX_0/COMPLEX    ---}
                 rqImaginaryUnit,       //30
                 rqComplex,             //31
                   {--- ORDINAL1/NUMERALS   ---}
                 rqOmega,               //32
                 rqZeroNumber,          //33
                 rqZero                 //34
              );

const ReqName: array[Requirement] of string = (
 'rqNone',
 {--- HIDDEN             ---}
 'rqAny',                 //1
 'rqSetMode',             //2
 'rqEqualsTo',            //3
 'rqBelongsTo',           //4
 {--- XBOOLE_0/BOOLE      ---}
 'rqEmpty',               //5
 'rqEmptySet',            //6
 {--- SUBSET_1/SUBSET     ---}
 'rqElement',             //7
 {--- ZFMISC_1/SUBSET ---}
 'rqPowerSet',            //8
 {--- TARSKI/SUBSET       ---}
 'rqInclusion',           //9
 {--- SUBSET_1/SUBSET     ---}
 'rqSubDomElem',          //10
 {--- NUMBERS             ---}
 'rqRealDom',             //11
 'rqNatDom',              //12
 {--- XCMPLX_0/ARITHM     ---}
 'rqRealAdd',             //13
 'rqRealMult',            //14
 {--- XREAL_0/REAL        ---}
 'rqLessOrEqual',         //15
 {--- ORDINAL1/NUMERALS   ---}
 'rqSucc',                //16
 {--- XBOOLE_0/BOOLE)     ---}
 'rqUnion',               //17
 'rqIntersection',        //18
 'rqSubtraction',         //19
 'rqSymmetricDifference', //20
 'rqMeets',               //21
 {--- XCMPLX_0/ARITHM     ---}
 'rqRealNeg',             //22
 'rqRealInv',             //23
 'rqRealDiff',            //24
 'rqRealDiv',             //25
 {--- XREAL_0             ---}
 'rqReal',                //26
 {--- XREAL_0/REAL        ---}
 'rqPositive',            //27
 'rqNegative',            //28
 {--- ORDINAL1/NUMERALS   ---}
 'rqNatural',             //29
 {--- XCMPLX_0/COMPLEX    ---}
 'rqImaginaryUnit',       //30
 'rqComplex',             //31
 {--- ORDINAL1/NUMERALS   ---}
 'rqOmega',               //32
 'rqZeroNumber',          //33
 'rqZero'                 //34
                                              );

const

 FunctorRequirement: set of Requirement =
                [rqEmptySet,
                 rqPowerSet,
                 rqRealAdd,
                 rqRealMult,
                 rqSucc,
                 rqUnion,
                 rqIntersection,
                 rqSubtraction,
                 rqSymmetricDifference,
                 rqRealNeg,
                 rqRealInv,
                 rqRealDiff,
                 rqRealDiv,
                 rqImaginaryUnit,
                 rqOmega,
                 rqZeroNumber];

 { Homonymic and special symbols in buildin vocabulery HIDDEN}
 StrictSym = 1;  {"strict"}
 SetSym = 1;     {'set'}
 EqualitySym = 1;  {'='}
 Square = 1;     {'[' ']'}
 Curled = 2;     (* '{' '}' *)
 Rounded = 3;    {'(' ')'}

var
 gBuiltIn: array[Requirement] of integer =
     (0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
 gRevReq: array[0..MaxFuncNbr] of Requirement;

procedure InitReverseRequirements;

implementation

procedure InitReverseRequirements;
 var i:integer; r:Requirement;
begin
 for i:=0 to MaxFuncNbr do gRevReq[i]:=rqNone;
 for r:=low(Requirement) to high(Requirement) do
  if r in FunctorRequirement then gRevReq[gBuiltIn[r]]:=r;
end;

end.
