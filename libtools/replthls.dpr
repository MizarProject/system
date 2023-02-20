(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program It_changes_one_library_reference_by_a_list;

uses mobjects, mconsole, mizenv, monitor, errhan, mscanner, syntax, parser,
     ref_han, ref_unit, envhan, edt_han;

const
   ArraySize = 100;
   
type
  ChangeItemPtr = ^ChangeItemObj;
  ChangeItemObj = object(MainNonSortItemObj)
    constructor Init(fKind:ItemKind);
    procedure StartSimpleJustification; virtual;
    procedure FinishSimpleJustification; virtual;
    procedure ModifyReferences;
  end;
   
  ChangeBlockPtr = ^ChangeBlockObj;
  ChangeBlockObj = object(BlockObj)
    constructor Init(fBlockKind:BlockKind);
    procedure Pop; virtual;
    procedure CreateItem(fItemKind:ItemKind); virtual;
    procedure CreateBlock(fBlockKind:BlockKind); virtual;
  end;
  
  InputColl = object(MList)
    constructor Init;
    destructor Done; virtual;
  end;  

  IntArray = array[0..ArraySize] of boolean; {which lists should be added}
    
var
   ToCorrect	  : Boolean;
   gEdt		  : Text;
   old_coll	  : InputColl; {to store references which will be substituted}
   inp_coll	  : InputColl; {to store lists of references
			       by which we substitute}
   gOption        : string;    {add,replace}
   
procedure PrintRef(r : PLibrReference);
begin
   writeln(r^.fStr,' ', r^.Kind=lrDef,' ', r^.Nr);
end; { PrintRef }

procedure PrintInpColl;
var
   i, j	 : integer;
   lList : PLibrRefCollection;
begin
   writeln; writeln;
   writeln('Input Collection');
   for i:=0 to inp_coll.Count-1 do
   begin
      writeln;
      PrintRef(PLibrReference(old_coll.Items^[i]));
      lList:=inp_coll.Items^[i];
      for j:=0 to lList^.Count-1 do
	 PrintRef(PLibrReference(lList^.At(j)));
   end;
   writeln; writeln;
end; { PrintInpColl }

procedure ReadInputFile(const fname : string);
var
   f	: text;
   l, s	: string;
   d	: LibrRefKind;
   n	: integer;
   list	: PLibrRefCollection;
begin
   inp_coll.Init;
   old_coll.Init;
   Assign(f, fname);
   Reset(f);
   ReadLn(f,gOption);
   repeat
      ReadLn(f,l);
      SplitLibrReference(l,s,d,n);
      old_coll.Insert(new(PLibrreference, Init(s,d,n)));
      list:=new(PLibrRefCollection,Init(3,5));
      repeat
	 ReadLn(f,l);
	 if l <> ';' then
	 begin
	    SplitLibrReference(l,s,d,n);
	    list^.Insert(new(PLibrReference, Init(s,d,n)));
	 end;
      until l = ';';
      inp_coll.Insert(list);
   until eof(f);
   Close(f);
end; { ReadInputFile }

procedure InitArticle;
var s : string;
begin
   gBlockPtr:=new(ChangeBlockPtr, Init(blMain));
   ToCorrect:=False;
   Assign(gEdt, MizFileName+'.edt');
   Rewrite(gEdt);
   s:='replthls.doc';
   if MFileExists(s) then ReadInputFile(s) else
   begin
      Writeln;
      Writeln;
      Writeln('!!!  File ', s, ' does not exist  !!!');
      Writeln;
      Writeln;
      Halt(20);
   end;
   Env.ReadEnvironment;
end;

constructor ChangeItemObj.Init;
begin
   inherited Init(fKind);
end;

procedure ChangeItemObj.StartSimpleJustification;
begin
   inherited StartSimpleJustification;
   ToCorrect:=CurWord.Kind = sy_By;
end;

procedure ChangeItemObj.FinishSimpleJustification;
begin
   inherited FinishSimpleJustification;
   if ToCorrect then
   begin
      ModifyReferences;
      EdtDeleteText(gEdt,gStartRefPos,gEndRefPos);
      if (gAllReferences^.LibRef^.Count > 0) or
//	 (gAllReferences^.Theorems^.Count > 0) or
//	 (gAllReferences^.Lemmas^.Count > 0) or
//	 (gAllReferences^.Definitions^.Count > 0) or
//	 (gAllReferences^.Local^.Count > 0) or
	 (gAllReferences^.AllLocal^.Count > 0)
	 then WriteLn(gEdt, 'iby ');
      gAllReferences^.PrintRefToEdtNotSort(gEdt);
   end;
   Dispose(gAllReferences, Done);
   ToCorrect:=False;
end;

function Compare2LibrRefs(r1,r2	: PLibrReference): boolean;
begin
   Compare2LibrRefs :=
   (r1^.fStr = r2^.fStr) and
   (r1^.Kind = r2^.Kind) and
   (r1^.Nr = r2^.Nr);   
end; { Compare2LibrRefs }

procedure AddTheoremDir(fname : string);
var
   lPos	: Position;
begin
   lPos.Line:=1; lPos.Col:=1;
   if Env.Directive[syTheorems].IndexOfStr(fname) = -1 then
      Env.Directive[syTheorems].Insert(new(PImpArticleId,Init(fname,lPos)));
end; { AddTheoremDir }

procedure ChangeItemObj.ModifyReferences;
var
   i, j, k, all	: integer;
   lList	: PLibrRefCollection;
   lRef		: PLibrReference;
   arr		: IntArray;
begin
   for i:=0 to ArraySize do arr[i]:=false;
   all:=gAllReferences^.LibRef^.Count-1;
   for i:=0 to all do
      for j:=0 to old_coll.Count-1 do
      begin
	 if Compare2LibrRefs
	    (PLibrReference(gAllReferences^.LibRef^.At(i)),
	     PLibrReference(old_coll.Items^[j])) then
	 begin
	    arr[j]:=true;
	    if gOption = 'replace' then
	       gAllReferences^.LibRef^.Items^[i] := nil;
	    break;
	 end;
      end;   
   if gOption = 'replace' then
      gAllReferences^.LibRef^.Pack;
   for j:=0 to ArraySize do
      if arr[j] then
      begin
	 lList:=inp_coll.Items^[j];
	 for k:=0 to lList^.Count-1 do
	 begin
	    lRef:=PLibrReference(lList^.At(k));
	    gAllReferences^.LibRef^.Insert(new(PLibrReference,Init(lRef^.fStr,
					   lRef^.Kind,lRef^.Nr)));
	    AddTheoremDir(lRef^.fStr);
	 end;
      end;
end;

constructor ChangeBlockObj.Init;
begin
   inherited Init(fBlockKind);
end;

procedure ChangeBlockObj.CreateItem;
begin
   gItemPtr:=new(ChangeItemPtr, Init(fItemKind));
end;

procedure ChangeBlockObj.CreateBlock;
begin
   gBlockPtr:=new(ChangeBlockPtr,Init(fBlockKind));
end;

procedure ChangeBlockObj.Pop;
begin
   if nBlockKind = blMain then
   begin
      Close(gEdt);
      old_coll.Done;
      inp_coll.Done;
      Env.StoreEvl(MizFileName+'.evl');
   end;
   inherited Pop;
end;

constructor InputColl.Init;
begin
   inherited Init(100);
end; 

destructor InputColl.Done;
begin
   inherited Done;
end; 

begin
   DrawMizarScreen('References Changer');
   InitExitProc;
   GetArticleName; GetEnvironName;
   GetOptions;
   FileExam(MizFileName+ArticleExt);
   OpenErrors(MizFileName);
   InitArticle;
   InitDisplayLine('Processing');
   FileExam(EnvFileName+'.dct');
   InitScanning(MizFileName+ArticleExt,EnvFileName);
   Parse;
   FinishScanning;
   writeln;
   FinishDrawing;
end.


{
The first line of an input file has to contain an option. Possible options
are: add, replace.

Each block should be finished by ;

example:

replace
FUNCT_1:8
TARSKI:2
ABIAN:2
;
FUNCT_1:98
TARSKI:3
FUNCT_1:def 1
;

explanation:

funct_1:8 will be substituted by tarski:2 and abian:2

funct_1:98 will be substituted by tarski:3 and funct_1:def 1
}

