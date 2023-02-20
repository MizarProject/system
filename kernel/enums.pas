(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit enums;

// Definitions of some enumerated types and simple
// (usually conversion) functions on them mainly used in correl

interface

uses limits, lexicon;

type

 ConstructorsKind = (coMode,coStructMode,coAttribute,coPredicate,
                     coFunctor,coSelector,coAggregate);

 NotationKind = (noMode,noStructMode,noAttribute,noPredicate,
                 noFunctor,noSelector,noAggregate,noForgetFunctor);

 ClusterKind = (clRegistered,clConditional,clFunctor);

 SymbolKind = (symMode,symStructure,symAttribute,symPredicate,
               symFunctor,symLeftBracket,symRightBracket,symSelector);

 ComplexTrmExprKind  =
   ( expTrmFunctor,expTrmAggreg,expTrmSelector,expTrmPrivFunc,expTrmSchFunc,
     expTrmChoice,expTrmFraenkel );
 FuncTrmExprKind = expTrmFunctor..expTrmSchFunc;

 ConstrIntArr = array[ConstructorsKind] of integer;
 NotationIntArr = array[NotationKind] of integer;
 SymbolIntArr  = array[SymbolKind] of integer;

var
 TrmKindArr: array[ComplexTrmExprKind] of char
   = ( ikTrmFunctor,
       ikTrmAggreg,
       ikTrmSelector,
       ikTrmPrivFunc,
       ikTrmSchFunc,
       ikTrmChoice,
       ikTrmFraenkel );

 function ConstructorRepr(fKind: ConstructorsKind): char;
 function ConstructorKind(fKind: char): ConstructorsKind;
 function SgnKind(fKind: char): ConstructorsKind;
 function SgnRepr(fKind: ConstructorsKind): char;

 function NotationRepr(fKind: NotationKind): char;
 function NotatKind(fKind: char): NotationKind;
 function NotatSgnKind(fKind: char): NotationKind;
 function NotationSgnRepr(fKind: NotationKind): char;

 // could be arrays probably
 function ConstrIndex( fKind: ConstructorsKind): IndexSort;
 function NotatIndex( fKind: NotationKind): IndexSort;

 function MaxNotatNbr(fKind: NotationKind): integer;

 // constructor kinds for which type is printed
 const TypedConstrKinds =
        [coFunctor, coMode, coAttribute, coAggregate, coSelector];

 const MaxRenumNbr = MaxFuncNbr; { TODO: should be in limits depending on
                                   symbol and constructor numbers}


implementation
uses errhan;

function ConstructorRepr(fKind: ConstructorsKind): char;
begin
 case fKind of
  coMode: ConstructorRepr:='M';
  coStructMode: ConstructorRepr:='L';
  coAttribute: ConstructorRepr:='V';
  coPredicate: ConstructorRepr:='R';
  coFunctor: ConstructorRepr:='K';
  coSelector: ConstructorRepr:='U';
  coAggregate: ConstructorRepr:='G';
  else RunTimeError(2210);
 end;
end;

function ConstructorKind(fKind: char): ConstructorsKind;
begin
 case fKInd of
  'M': ConstructorKind:=coMode;
  'L': ConstructorKind:=coStructMode;
  'V': ConstructorKind:=coAttribute;
  'R': ConstructorKind:=coPredicate;
  'K': ConstructorKind:=coFunctor;
  'U': ConstructorKind:=coSelector;
  'G','J': ConstructorKind:=coAggregate;
  else RunTimeError(2211);
 end;
end;

function NotationRepr(fKind: NotationKind): char;
begin
 case fKInd of
  noFunctor    : NotationRepr:= 'K' {ikSgnFunctor};
  noAttribute  : NotationRepr:= 'V' {ikSgnAttribute};
  noMode       : NotationRepr:= 'M' {ikSgnMode};
  noPredicate  : NotationRepr:= 'R' {ikSgnPredicate};
  noStructMode : NotationRepr:= 'L' {ikSgnStructMode};
  noAggregate  : NotationRepr:= 'G' {ikSgnAggregate};
  noSelector   : NotationRepr:= 'U' {ikSgnSelector};
  noForgetFunctor: NotationRepr:= 'J' {ikSgnForgetFunc};
  else RunTimeError(2212);
 end;
end;

function NotationSgnRepr(fKind: NotationKind): char;
begin
 case fKInd of
  noFunctor    : NotationSgnRepr:= ikSgnFunctor;
  noAttribute  : NotationSgnRepr:= ikSgnAttribute;
  noMode       : NotationSgnRepr:= ikSgnMode;
  noPredicate  : NotationSgnRepr:= ikSgnPredicate;
  noStructMode : NotationSgnRepr:= ikSgnStructMode;
  noAggregate  : NotationSgnRepr:= ikSgnAggregate;
  noSelector   : NotationSgnRepr:= ikSgnSelector;
  noForgetFunctor: NotationSgnRepr:= ikSgnForgetFunc;
  else RunTimeError(2213);
 end;
end;

function NotatKind(fKind: char): NotationKind;
begin
 case fKInd of
  'K' : NotatKind:= noFunctor;
  'V' : NotatKind:= noAttribute;
  'M' : NotatKind:= noMode;
  'R' : NotatKind:= noPredicate;
  'L' : NotatKind:= noStructMode;
  'G' : NotatKind:= noAggregate;
  'U' : NotatKind:= noSelector;
  'J' : NotatKind:= noForgetFunctor;
  else RunTimeError(2214);
 end;
end;

function NotatSgnKind(fKind: char): NotationKind;
begin
 case fKInd of
  ikSgnFunctor		: NotatSgnKind:= noFunctor;
  ikSgnAttribute  	: NotatSgnKind:= noAttribute;
  ikSgnMode      	: NotatSgnKind:= noMode;
  ikSgnPredicate  	: NotatSgnKind:= noPredicate;
  ikSgnStructMode  	: NotatSgnKind:= noStructMode;
  ikSgnAggregate  	: NotatSgnKind:= noAggregate;
  ikSgnSelector  	: NotatSgnKind:= noSelector;
  ikSgnForgetFunc  : NotatSgnKind:= noForgetFunctor;
 else RunTimeError(2215);
 end;
end;

function NotatIndex( fKind: NotationKind): IndexSort;
begin
 case fKInd of
  noFunctor    : NotatIndex:= FuncPattIndex;
  noAttribute  : NotatIndex:= AttrPattIndex;
  noMode       : NotatIndex:= ModePattIndex;
  noPredicate  : NotatIndex:= PredPattIndex;
  noStructMode : NotatIndex:= StructPattIndex;
  noAggregate  : NotatIndex:= StructPattIndex;
  noSelector   : NotatIndex:= SelectPattIndex;
  noForgetFunctor: NotatIndex:= StructPattIndex;
  else RunTimeError(2216);
 end;
end;

function MaxNotatNbr(fKind: NotationKind): integer;
begin MaxNotatNbr:= IndexLimit[ NotatIndex( fKind)]; end;

// ##TODO: This duplicates ConstructorKind
function SgnKind(fKind: char): ConstructorsKind;
begin
 case fKind of
  ikSgnMode: 		SgnKind:=coMode;
  ikSgnStructMode: 	SgnKind:=coStructMode;
  ikSgnAttribute: 	SgnKind:=coAttribute;
  ikSgnPredicate: 	SgnKind:=coPredicate;
  ikSgnFunctor: 	SgnKind:=coFunctor;
  ikSgnSelector: 	SgnKind:=coSelector;
  ikSgnAggregate: 	SgnKind:=coAggregate;
  else RunTimeError(2217);
 end;
end;

// ##TODO: This duplicates ConstructorRepr
function SgnRepr(fKind: ConstructorsKind): char;
begin
 case fKind of
  coMode:      	SgnRepr:=ikSgnMode;     
  coStructMode:	SgnRepr:=ikSgnStructMode;
  coAttribute:	SgnRepr:=ikSgnAttribute;
  coPredicate:	SgnRepr:=ikSgnPredicate;
  coFunctor:  	SgnRepr:=ikSgnFunctor;
  coSelector: 	SgnRepr:=ikSgnSelector; 
  coAggregate:	SgnRepr:=ikSgnAggregate;
  else RunTimeError(2218);
 end;
end;

function ConstrIndex( fKind: ConstructorsKind): IndexSort;
begin
 case fKind of
  coMode:      	ConstrIndex:=ModeIndex;     
  coStructMode:	ConstrIndex:=StructIndex;
  coAttribute:	ConstrIndex:=AttrIndex;
  coPredicate:	ConstrIndex:=PredIndex;
  coFunctor:  	ConstrIndex:=FuncIndex;
  coSelector: 	ConstrIndex:=SelectIndex; 
  coAggregate:	ConstrIndex:=StructIndex;
  else RunTimeError(2219);
 end;
end;

end.
