(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit ref_unit;

interface

uses mobjects, syntax, ref_han, edt_han, errhan;

type
 SpecStringCollPtr = ^SpecStringCollObj;
 SpecStringCollObj = object(TIntKeyCollection)
   nPrefix: string;
   constructor Init(fPrefix: string);
   destructor Done; virtual;
   procedure InsertToColl(fStr :string); virtual;
 end;			    

 AllReferencesPtr = ^AllReferencesObj;
 AllReferencesObj = object(MObject)
    AllLocal: PCollection;
    Local, Definitions, Lemmas, Theorems: SpecStringCollPtr;
    LibRef: PLibrRefCollection;
  constructor Init;
  destructor Done; virtual;
  procedure PutLocLemTh(const fColl: SpecStringCollPtr; var fEdt: text); virtual;
  procedure PutLibRef(var fEdt: text); virtual;
  procedure PrintRefToEdt(var fEdt: text); virtual;
  procedure PrintRefToEdtNotSort(var fEdt: text); virtual;
 end;
 
 MainNonSortItemPtr = ^MainNonSortItemObj;
 MainNonSortItemObj = object(ItemObj)
  constructor Init(fKind:ItemKind);
  procedure Pop; virtual;
  procedure StartSimpleJustification; virtual;
  procedure FinishSimpleJustification; virtual;
  procedure ProcessPrivateReference; virtual;
  procedure StartLibraryReferences; virtual;
  procedure ProcessDef; virtual;
  procedure ProcessTheoremNumber; virtual;
 end;

 MainSortItemPtr = ^MainSortItemObj;
 MainSortItemObj = object(MainNonSortItemObj)
  constructor Init(fKind:ItemKind);
  procedure Pop; virtual;
  procedure ProcessPrivateReference; virtual;
 end;

var
   gAllReferences : AllReferencesPtr;
   gStartRefPos	  : Position;
   gEndRefPos	  : Position;
 
implementation

uses lexicon, mscanner;

var
   gDefOcc    : LibrRefKind;
   gLibrIdent : string;
 
{ ALLREFERENCES }
   
constructor AllReferencesObj.Init;
begin
   inherited Init;
   AllLocal := new(PCollection, Init(10,10));
   Local := new(SpecStringCollPtr, Init('A'));
   Definitions := new(SpecStringCollPtr, Init('Def'));
   Lemmas := new(SpecStringCollPtr, Init('Lm'));
   Theorems := new(SpecStringCollPtr, Init('Th'));
   LibRef := new(PLibrRefCollection, Init(10,5));
end;

destructor AllReferencesObj.Done;
begin
   Dispose(AllLocal, Done);
   Dispose(Local, Done);
   Dispose(Lemmas, Done);
   Dispose(Definitions, Done);
   Dispose(Theorems, Done);
   Dispose(LibRef, Done);
   inherited Done;
end;

procedure AllReferencesObj.PutLocLemTh
(const fColl: SpecStringCollPtr; var fEdt: text);
var
   i : Integer;
begin
   WriteLn(fEdt, 'i', fColl^.nPrefix,PIntItem(fColl^.At(0))^.IntKey);
   for i:=1 to fColl^.Count - 1 do
      WriteLn(fEdt, 'i,', fColl^.nPrefix,PIntItem(fColl^.At(i))^.IntKey);
end;

procedure AllReferencesObj.PutLibRef(var fEdt: text);
var
   i : Integer;
begin
   WriteLn(fEdt, 'i', PLibrReference(LibRef^.At(0))^.fStr, ':',TheoremNr(PLibrReference(LibRef^.At(0))^));
   for i:=1 to LibRef^.Count - 1 do
      if PLibrReference(LibRef^.At(i))^.fStr =
	 PLibrReference(LibRef^.At(i-1))^.fStr
	 then WriteLn(fEdt, 'i,', TheoremNr(PLibrReference(LibRef^.At(i))^))
      else WriteLn(fEdt, 'i,', PLibrReference(LibRef^.At(i))^.fStr, ':',TheoremNr(PLibrReference(LibRef^.At(i))^));
end;

procedure AllReferencesObj.PrintRefToEdt(var fEdt: text);
begin
   if Local^.Count > 0 then PutLocLemTh(Local,fEdt);
   if Definitions^.Count > 0 then
   begin
      if Local^.Count > 0 then WriteLn(fEdt, 'i,');
      PutLocLemTh(Definitions,fEdt);
   end;
   if Lemmas^.Count > 0 then
   begin
      if (Definitions^.Count > 0) or (Local^.Count > 0) then
	 WriteLn(fEdt, 'i,');
      PutLocLemTh(Lemmas,fEdt);
   end;
   if Theorems^.Count > 0 then
   begin
      if (Lemmas^.Count > 0) or (Definitions^.Count > 0) or
	 (Local^.Count > 0) then WriteLn(fEdt, 'i,');
      PutLocLemTh(Theorems,fEdt);
   end;
   if LibRef^.Count > 0 then
   begin
      if (Theorems^.Count > 0) or (Lemmas^.Count > 0) or
	 (Definitions^.Count > 0) or (Local^.Count > 0) then
	 WriteLn(fEdt, 'i,');
      PutLibRef(fEdt);
   end;
end;

procedure AllReferencesObj.PrintRefToEdtNotSort(var fEdt: text);
var i : integer;
begin
   if AllLocal^.Count > 0 then
   begin
      WriteLn(fEdt, 'i', MStrPtr(AllLocal^.At(0))^.fStr);
      for i:=1 to AllLocal^.Count - 1 do
	 WriteLn(fEdt, 'i,', MStrPtr(AllLocal^.At(i))^.fStr);
   end;
   if LibRef^.Count > 0 then
   begin
      if AllLocal^.Count > 0 then WriteLn(fEdt, 'i,');
      PutLibRef(fEdt);
   end;
end;

{ SPECSTRINGCOLL }

constructor SpecStringCollObj.Init;
begin
   inherited Init(10,10);
   nPrefix := fPrefix;
end;

destructor SpecStringCollObj.Done;
begin
   inherited Done;
end;

procedure SpecStringCollObj.InsertToColl(fStr :string);
var
   lNr, lCode : integer;
   lStr, lSp  : string;
begin
   lSp := CurWord.Spelling;
   case lSp[1] of
     'A' : lStr := Copy(lSp,2,length(lSp));
     'L' : lStr := Copy(lSp,3,length(lSp));
     'D' : lStr := Copy(lSp,4,length(lSp));
     'T' : lStr := Copy(lSp,3,length(lSp));
   end; { case }
   Val(lStr,lNr,lCode);
   if lCode <> 0 then
   begin
     writeln;
     writeln('You have to rename all labels. (', lSp, ' ', lStr, ')');
     writeln;
     Halt(10);
   end;
   Insert(new(PIntItem, Init(lNr)));
end;

{ ITEM }

constructor MainSortItemObj.Init;
begin
   inherited Init(fKind);
end;

procedure MainSortItemObj.Pop;
begin
   inherited Pop;
end;

procedure MainSortItemObj.ProcessPrivateReference;
begin
   case (CurWord.Spelling)[1] of
     'D' : gAllReferences^.Definitions^.InsertToColl(CurWord.Spelling);
     'L' : gAllReferences^.Lemmas^.InsertToColl(CurWord.Spelling);
     'T' : gAllReferences^.Theorems^.InsertToColl(CurWord.Spelling);
     'A' : gAllReferences^.Local^.InsertToColl(CurWord.Spelling);
   else
   begin
      writeln;
      writeln('You have to rename all labels. (', CurWord.Spelling, ')');
      writeln;
      Halt(10);
   end;
   end;
end;

constructor MainNonSortItemObj.Init;
begin
   inherited Init(fKind);
end;

procedure MainNonSortItemObj.Pop;
begin
   inherited Pop;
end;

procedure MainNonSortItemObj.ProcessDef;
begin  
   gDefOcc:=lrTh;   
   if (CurWord.Kind = ReferenceSort) and (CurWord.Nr = ord(syDef)) then gDefOcc:=lrDef; 
end;

procedure MainNonSortItemObj.StartSimpleJustification;
begin
   gStartRefPos.Line := CurPos.Line;
   gStartRefPos.Col := CurPos.Col-1;
   gAllReferences := new(AllReferencesPtr, Init);
end;

procedure MainNonSortItemObj.FinishSimpleJustification;
begin
   gEndRefPos := PrevPos;
end;

procedure MainNonSortItemObj.ProcessPrivateReference;
begin
   gAllReferences^.AllLocal^.Insert(new(MStrPtr, Init(CurWord.Spelling)));
end;

procedure MainNonSortItemObj.StartLibraryReferences;
begin
   gLibrIdent := CurWord.Spelling;
end;

procedure MainNonSortItemObj.ProcessTheoremNumber;
begin
   gAllReferences^.LibRef^.Insert(new(PLibrReference,
				  Init(gLibrIdent, gDefOcc, CurWord.Nr)));
end;

end.
