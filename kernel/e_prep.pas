(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// The preparator objects for exporter.

// This is used only to collect schemes, theorems and definitions.
// Clusters, constructors, notation and definienda are saved before
// disposing analyzer data so they are ignored here.


unit e_prep;

interface

uses mobjects,justhan,correl,limits,prepobj,schemhan,inout;

type

 ExtPBlockPtr = ^ExtPBlockObj;
 ExtPBlockObj =
  object(PrepBlockObj)
   nSchLab: integer; // nonzero only for blPublicScheme
   procedure InitPrepData; virtual;
   // this is a bit silly, but FPC does not allow other result type
   function CreateItem(fItemKind:ItemKind):PrepItemPtr; virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
   function GetPrevious : ExtPBlockPtr;
   procedure ProcessSchemeLabel; virtual;
  end;

 ExtPItemPtr = ^ExtPItemObj;
 ExtPItemObj =
  object(PrepItemObj)
   nDefThConstr: Lexem; // nonzero only for itDefTheorem
   function  GetPrevious : ExtPItemPtr;
   function  GetBlock : ExtPBlockPtr;
   procedure StartSchemeTypes; virtual;
   procedure FinishSchFuncSegment; virtual;
   procedure FinishSchemeTypes; virtual;
   procedure StartSchemePremises; virtual;
   procedure FinishSchemePremise; virtual;
   procedure FinishSchemeThesis; virtual;
   procedure StartDefTheorem; virtual;
   procedure FinishTheoremBody; virtual;
   procedure FinishCanceledScheme; virtual;
   procedure FinishCanceled; virtual;
   procedure FinishCanceledDef; virtual;
  end;

var gTheorem: array of 
     record Definitional:boolean; nDefConstr: Lexem; Theo: FrmPtr end;
    gScheme: array of
     record nCanceled: boolean; nSchemeDef: SchRefPtr; end;
    gTheoremNbr,gSchemeNbr: integer;

implementation

uses errhan,mizenv,lexicon,prephan,iocorrel,propcoll
{$IFDEF MDEBUG}
    ,info,outinfo
{$ENDIF};

(********** Standard overloaded stuff **********)

function  ExtPItemObj.GetPrevious : ExtPItemPtr;
begin GetPrevious := ExtPItemPtr(Previous); end;

function  ExtPItemObj.GetBlock : ExtPBlockPtr;
begin GetBlock := ExtPBlockPtr(nBlock); end;

function  ExtPBlockObj.GetPrevious : ExtPBlockPtr;
begin GetPrevious := ExtPBlockPtr(Previous); end;

function ExtPBlockObj.CreateItem(fItemKind:ItemKind): PrepItemPtr;
begin
 nCurrItm:= new(ExtPItemPtr, Init(fItemKind, @Self));
 nCurrItm^.StartItem;
 CreateItem:= nCurrItm;
end;

procedure ExtPBlockObj.CreateBlock(fBlockKind:BlockKind);
begin
 DebugBlockCreate(fBlockKind);
 gPrBlockPtr:= new(ExtPBlockPtr,Init(fBlockKind));
 gPrBlockPtr^.StartBlock;  
end;

(***********************************************)

procedure ExtPBlockObj.ProcessSchemeLabel;
 var lSch: SchRefPtr;
begin
 Mizassert(errSchemeBlockExpected, nBlockKind = blPublicScheme);
 nSchLab:= InFile.Current.Nr;
 lSch:= new(SchRefPtr,Init1);
 lSch^.fKey.Y:= nSchLab;
 lSch^.nSchState:= schStateBeingProved;
 Ord_Scheme.Insert(lSch);
 if gSchemeNbr >= length(gScheme) then
  setlength(gScheme,2*length(gScheme));
 inc(gSchemeNbr);
 with gScheme[gSchemeNbr] do
 begin nCanceled:=false;
   nSchemeDef:=lSch;
 end;
end;

procedure ExtPItemObj.StartSchemeTypes;
begin CurSchFuncTyp.Init(0,MaxArgNbr); end;

procedure ExtPItemObj.FinishSchFuncSegment;
begin
 CurSchFuncTyp.Insert(gPrep^.nLastType);
 gPrep^.nLastType:= nil;
end; 

// ##TODO: the 'move' should be replaced here
procedure ExtPItemObj.FinishSchemeTypes;
begin
 CurSchFuncTyp.SetLimit(0);
 move(CurSchFuncTyp,Scheme(0,GetBlock^.nSchLab)^.SchTypes,SizeOf(MCollection));
end;

// reserve place for thesis it comes first
procedure ExtPItemObj.StartSchemePremises;
begin
 with Scheme(0,GetBlock^.nSchLab)^ do
 begin
  SchProps.Init(1,4); 
  SchProps.Insert(nil);
 end;
end; 
 
procedure ExtPItemObj.FinishSchemePremise;
begin
 with Scheme(0,GetBlock^.nSchLab)^ do
  SchProps.Insert(gPrep^.nLastProposition^.nSentence^.CopyFormula);
end;

procedure ExtPItemObj.FinishSchemeThesis;
begin
 with Scheme(0,GetBlock^.nSchLab)^ do
 begin
  SchProps.AtPut(0, gPrep^.nLastProposition^.nSentence^.CopyFormula);
  nSchState:= schStateQuotable; // safety
 end;
end;

procedure ExtPItemObj.StartDefTheorem;
begin nDefThConstr:= InFile.Current; end;

procedure ExtPItemObj.FinishTheoremBody;
begin
 if gTheoremNbr >= length(gTheorem) then
  setlength(gTheorem,2*length(gTheorem));
 inc(gTheoremNbr);
 with gTheorem[gTheoremNbr] do
 begin
  Theo := gPrep^.nLastProposition^.nSentence^.CopyFormula;
  Definitional:= (gPrep^.nLastProposition^.nKind = propDefTheorem);
  nDefConstr:= nDefThConstr;
 end;
end;

procedure ExtPItemObj.FinishCanceledScheme;
begin
 if gSchemeNbr >= length(gScheme) then
  setlength(gScheme,2*length(gScheme));
 inc(gSchemeNbr);
 with gScheme[gSchemeNbr] do
 begin
   nCanceled:=true;
   nSchemeDef:=nil;
 end;
end;

procedure ExtPItemObj.FinishCanceled;
begin
 if gTheoremNbr >= length(gTheorem) then
  setlength(gTheorem,2*length(gTheorem));
 inc(gTheoremNbr);
 with gTheorem[gTheoremNbr] do
 begin
  Theo:=NewVerum;
  Definitional:=false;
  nDefConstr.Kind:=ikError;
 end;
end;

procedure ExtPItemObj.FinishCanceledDef;
begin
 if gTheoremNbr >= length(gTheorem) then
  setlength(gTheorem,2*length(gTheorem));
 inc(gTheoremNbr);
 with gTheorem[gTheoremNbr] do
 begin
  Theo:=NewVerum;
  Definitional:=true;
  nDefConstr.Kind:=ikError;
 end;
end;

procedure ExtPBlockObj.InitPrepData;
begin
 Load_EnvConstructors;
 Ord_Scheme.Init(0, 16);
 Ord_Scheme.Insert(new(SchRefPtr, Init1));
 setlength(gTheorem,MaxTheoNbr);
 gTheoremNbr:=0;
 setlength(gScheme,MaxTheoNbr);
 with gScheme[0] do
  begin
   nCanceled:=true;
   nSchemeDef:=nil;
  end;
 gSchemeNbr:=0;
end;

end.
