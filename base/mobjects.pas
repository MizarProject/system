(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit mobjects;

interface
uses numbers;

const

{ Maximum MCollection size }
  MaxSize = 2000000;

  MaxCollectionSize = MaxSize div SizeOf(Pointer);

{ Maximum MStringList size }

  MaxListSize = MaxSize  div (sizeof(Pointer)*2);

{ Maximum IntegerList size }

  MaxIntegerListSize = MaxSize  div (sizeof(integer));

{ MCollection error codes }

  coIndexError = -1;              { Index out of range }
  coOverflow   = -2;              { Overflow }
  coConsistentError = -3;
  coDuplicate   = -5;             { Duplicate }
  coSortedListError = -6;
  coIndexExtError = -7;

type

{ String pointers }

  PString = ^ShortString;

{ Character set type }

  PCharSet = ^TCharSet;
  TCharSet = set of Char;

{ General arrays }

  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;

{ MObject base object }

  PObject = ^MObject;
  ObjectPtr = PObject;
  MObject = object
    constructor Init;
    procedure Free;
    destructor Done; virtual;
    function CopyObject: PObject;
    function MCopy: PObject; virtual;
  end;

{ Specyfic objects based on MObjects for collections}

 
 PStr = ^MStrObj;
 MStrPtr = PStr;
 MStrObj = object(MObject)
     fStr: string;
    constructor Init(const aStr: string);
 end;

{ MCollection types }

  PItemList = ^MItemList;
  MItemList = array[0..MaxCollectionSize - 1] of Pointer;

{ MList object }

  PList = ^MList;
  MListPtr = PList;
  MList = object(MObject)
     Items: PItemList;
     Count: Integer;
     Limit: Integer;
    constructor Init(ALimit: Integer);
    constructor MoveList(var aAnother: MList);
    constructor CopyList(var aAnother: MList);
    destructor Done; virtual;
    function MCopy: PObject; virtual;

    procedure ListError(aCode, aInfo: Integer); virtual;

    function At(Index: Integer): Pointer;
    function Last: Pointer;
    procedure Insert(aItem: Pointer); virtual;
    procedure AtInsert(aIndex: Integer; aItem: Pointer); virtual;
    procedure InsertList(var aAnother: MList); virtual;
    function GetObject(aIndex: integer): Pointer; virtual;
    function IndexOf(aItem: Pointer): Integer; virtual;
    procedure DeleteAll; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    procedure FreeAll; virtual;
    procedure FreeItemsFrom(aIndex:integer); virtual;
    procedure Pack; virtual;
    procedure SetLimit(ALimit: Integer); virtual;

    procedure AppendTo(var fAnother:MList); virtual;
    procedure TransferItems(var fAnother:MList); virtual;
    procedure CopyItems(var fOrigin:MList); virtual;
  end;

{ MCollection object }

  PCollection = ^MCollection;
  MCollection = object(MList)
     Delta: Integer;
    constructor Init(ALimit, ADelta: Integer);
    destructor Done; virtual;

    procedure AtDelete(Index: Integer);
    procedure AtFree(Index: Integer);
    procedure AtInsert(Index: Integer; Item: Pointer); virtual;
    procedure AtPut(Index: Integer; Item: Pointer);
    procedure Delete(Item: Pointer);
    procedure Free(Item: Pointer);
    procedure Insert(aItem: Pointer); virtual;
    procedure Pack; virtual;

    constructor MoveCollection(var fAnother:MCollection);
    constructor MoveList(var aAnother:MList);
    constructor CopyList(var aAnother: MList);
    constructor CopyCollection(var AAnother:MCollection);
    constructor Singleton(fSing:PObject; fDelta:Integer);

    procedure Prune; virtual;
   end;

{ MExtList object }

  MExtListPtr = ^MExtList;
  MExtList = object(MList)
     fExtCount: Integer;
    constructor Init(aLimit: Integer);
    destructor Done; virtual;
    procedure Insert(aItem: Pointer); virtual;
    procedure Mark(var aIndex: integer); virtual;
    procedure FreeItemsFrom(aIndex:integer); virtual;
    procedure DeleteAll; virtual;
    procedure FreeAll; virtual;
    procedure Pack; virtual;
    procedure InsertExt(AItem: Pointer); virtual;
    procedure SetLimit(ALimit: Integer); virtual;
    procedure AddExtObject; virtual;
    procedure AddExtItems; virtual;
    procedure DeleteExtItems;
    procedure FreeExtItems;
  end;

{MSortedList Object}

  IndexListPtr = ^MIndexList;
  MIndexList = array[0..MaxCollectionSize - 1] of integer;
  CompareProc = function (aItem1, aItem2: Pointer): Integer;

  MSortedList = object(MList)
     fIndex: IndexListPtr;
     fCompare: CompareProc;
    constructor Init(ALimit: Integer);
    constructor InitSorted(aLimit: Integer; aCompare: CompareProc);
    constructor MoveList(var aAnother: MList);
    constructor CopyList(const aAnother: MList);
    procedure AtInsert(aIndex:integer; aItem: Pointer); virtual;
    procedure Insert(aItem: Pointer); virtual;
    function IndexOf(aItem: Pointer): Integer; virtual;
    procedure Sort(aCompare: CompareProc);
    procedure SetLimit(ALimit: Integer); virtual;
    function Find(aKey: Pointer; var aIndex: Integer): Boolean; virtual;
    function Search(aKey: Pointer; var aIndex: Integer): Boolean; virtual;
    procedure Pack; virtual;
    procedure FreeItemsFrom(aIndex:integer); virtual;
  end;
  
  MSortedExtList = object(MExtList)
   fIndex: IndexListPtr;
   fCompare: CompareProc;
   constructor Init(ALimit: Integer);
   constructor InitSorted(aLimit: Integer; aCompare: CompareProc);
   destructor Done; virtual;
   function Find(aKey: Pointer; var aIndex: Integer): Boolean; virtual;
   function  FindRight(aKey: Pointer; var aIndex: Integer): Boolean; virtual;
   function FindInterval(aKey: Pointer; var aLeft,aRight: Integer): Boolean; virtual;
   function AtIndex( aIndex: integer): Pointer; virtual;
   procedure Insert(aItem: Pointer); virtual;
   procedure Pack; virtual;
   procedure InsertExt(AItem: Pointer); virtual;
   procedure SetLimit(ALimit: Integer); virtual;
   procedure FreeItemsFrom(aIndex:integer); virtual;
   procedure AddExtObject; virtual;
   procedure AddExtItems; virtual;
  end;

  MSortedStrList = object(MSortedList)
    constructor Init(ALimit: Integer);
    function IndexOfStr(const aStr: string): Integer; virtual;
    function ObjectOf(const aStr: string): PObject; virtual;
  end;

{ MSortedCollection object }

  PSortedCollection = ^MSortedCollection;
  MSortedCollection = object(MCollection)
     Duplicates: Boolean;
     fCompare: CompareProc;
    constructor Init(ALimit, ADelta: Integer);
    constructor InitSorted(ALimit, ADelta: Integer; aCompare: CompareProc);
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    function IndexOf(aItem: Pointer): Integer; virtual;
    procedure Insert(aItem: Pointer); virtual;
    procedure InsertD(Item: Pointer); virtual;
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: Integer): Boolean; virtual;
  end;

{ MStringCollection object }

  PStringCollection = ^MStringCollection;
  MStringCollection = object(MSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
  end;

{ UnsortedStringCollection }

  PUsortedStringCollection = ^StringColl;
  StringColl = object(MCollection)
   procedure FreeItem(Item:pointer); virtual;
  end;

{ MIntCollection object }

  IntPair = record
    X,Y : integer;
  end;

  IntPairItemPtr = ^IntPairItem;
  IntPairItem = object(MObject)
     fKey: IntPair;
    constructor Init(X,Y: integer);
  end;

  IntPtr = ^integer;

  PIntItem = ^TIntItem;
  TIntItem = object(MObject)
    IntKey: integer;
   constructor Init(fInt:integer);
  end;

  PIntKeyCollection = ^TIntKeyCollection;
  TIntKeyCollection = object(MSortedCollection)
    function KeyOf(Item:pointer):pointer; virtual;
    function Compare(Key1,Key2:pointer): integer; virtual;
  end;

  IntPairKeyCollectionPtr = ^IntPairKeyCollection;
  IntPairKeyCollection = object(MSortedCollection)
    function Compare(Key1,Key2:pointer): integer; virtual;
    function ObjectOf(X,Y: integer): IntPairItemPtr; virtual;
    function FirstThat(X: integer): IntPairItemPtr; virtual;
  end;

{Stacked Object (List of objects)}

  StackedPtr = ^StackedObj;
  StackedObj = object(MObject)
     Previous: StackedPtr;
    constructor Init;
    destructor Done; virtual;
  end;

{ MStringList object }

  MDuplicates = (dupIgnore, dupAccept, dupError);

  PStringItem = ^MStringItem;
  MStringItem = record
    fString: PString;
    fObject: PObject;
  end;

  PStringItemList = ^MStringItemList;
  MStringItemList = array[0..MaxListSize] of MStringItem;

  PStringList = ^MStringList;
  MStringList = object(MObject)
     fList: PStringItemList;
     fCount: integer;
     fCapacity: integer;
     fSorted: Boolean;
     fDuplicate: MDuplicates;

    constructor Init(aCapacity: Integer);
    constructor MoveStringList(var aAnother:MStringList);

    {-- Internal methods- do not use them directly --}
    procedure StringListError(Code, Info: Integer); virtual;
    procedure Grow;
    procedure QuickSort(L, R: integer);
    procedure ExchangeItems(Index1, Index2: integer);
    procedure InsertItem(aIndex: integer; const aStr: string);
    {--                                            --}

    procedure SetSorted(aValue: Boolean);
    procedure Sort; virtual;
    function GetString(aIndex: integer): string; virtual;
    function GetObject(aIndex: integer): PObject; virtual;
    procedure PutString(aIndex: integer; const aStr: string); virtual;
    procedure PutObject(aIndex: integer; aObject: PObject); virtual;
    procedure SetCapacity(aCapacity: integer); virtual;

    destructor Done; virtual;

    function AddString(const aStr: string): integer; virtual;
    function AddObject(const aStr: string; aObject: PObject): integer; virtual;
    procedure AddStrings(var aStrings: MStringList); virtual;
    procedure Clear; virtual;
    procedure Delete(aIndex: integer); virtual;
    procedure Exchange(Index1, Index2: integer); virtual;
    procedure MoveObject(CurIndex, NewIndex: integer); virtual;

    function Find(const aStr: string; var aIndex: integer): Boolean; virtual;
    function IndexOf(const aStr: string): integer; virtual;
    function ObjectOf(const aStr: string): PObject; virtual;
    function IndexOfObject(aObject: PObject): integer;
    procedure Insert(aIndex: integer; const aStr: string); virtual;
    procedure InsertObject(aIndex: integer; const aStr: string; aObject: PObject);
  end;

{ Partial Integers Functions }

  IntTriplet = record
    X1,X2,Y : integer;
  end;

const
  MaxIntPairSize = MaxSize div SizeOf(IntPair);
  MaxIntTripletSize = MaxSize div SizeOf(IntTriplet);

type
  IntPairListPtr = ^IntPairList;
  IntPairList = array[0..MaxIntPairSize- 1] of IntPair;

  IntPairSeqPtr = ^BinIntFunc;
  IntPairSeq = object(MObject)
      Items: IntPairListPtr;
      Count: Integer;
      Limit: Integer;
    constructor Init(aLimit: Integer);
    procedure NatSetError(Code, Info: Integer); virtual;
    destructor Done; virtual;
    procedure SetLimit(aLimit: Integer); virtual;

    procedure Insert(const aItem: IntPair); virtual;
    procedure AtDelete(aIndex: Integer);
    procedure DeleteAll;

    procedure AssignPair(X,Y:integer); virtual;
  end;

  IntRelPtr = ^IntRel;
  IntRel = object(IntPairSeq)

    constructor Init(aLimit: Integer);

    procedure Insert(const aItem: IntPair); virtual;
    procedure AtInsert(aIndex: integer; const aItem: IntPair); virtual;

    function Search(X,Y: integer; var aIndex: Integer): Boolean; virtual;
    function IndexOf(X,Y:integer): integer;
    constructor CopyIntRel(var aFunc: IntRel);

    function IsMember(X,Y:integer): boolean; virtual;
    procedure AssignPair(X,Y:integer); virtual;
  end;

  NatSetPtr = ^NatSet;
  NatSet = object(IntRel)
      Delta: Integer;
      Duplicates: Boolean;
    constructor Init(aLimit, aDelta: Integer);
    constructor InitWithElement(X:integer);
    destructor Done; virtual;
    procedure Insert(const aItem: IntPair); virtual;
    function SearchPair(X: integer; var Index: Integer): Boolean; virtual;
    function ElemNr(X:integer): integer;
     {********************************************}
    constructor CopyNatSet(const fFunc: NatSet);
    procedure InsertElem(X:integer); virtual;
    procedure DeleteElem(fElem:integer); virtual;
    procedure EnlargeBy(const fAnother: NatSet); {? virtual;?}
    procedure ComplementOf(const fAnother: NatSet);
    procedure IntersectWith(const fAnother: NatSet);
     {********************************************}
    function HasInDom(fElem:integer): boolean; virtual;
    function IsEqualTo(const fFunc: NatSet): boolean;
    function IsSubsetOf(const fFunc: NatSet): boolean;
    function IsSupersetOf(const fFunc: NatSet): boolean;
    function Misses(const fFunc: NatSet): boolean;
    constructor MoveNatSet(var fFunc: NatSet);
  end;

  NatFuncPtr = ^NatFunc;
  NatFunc = object(NatSet)
     nConsistent: boolean;
    constructor InitNatFunc(ALimit, ADelta: Integer);
    constructor CopyNatFunc(const fFunc: NatFunc);
    constructor MoveNatFunc(var fFunc: NatFunc);
    constructor LCM(const aFunc1,aFunc2: NatFunc);
    procedure Assign(X,Y:integer); virtual;
    procedure Up(X: integer); virtual;
    procedure Down(X: integer); virtual;
    function Value(fElem: integer): integer; virtual;
    procedure Join(const fFunc: NatFunc);
    destructor Refuted; virtual;
    procedure EnlargeBy(fAnother:NatFuncPtr); {? virtual;?}
    function JoinAtom(fLatAtom:NatFuncPtr): NatFuncPtr;
    function CompareWith(const fNatFunc:NatFunc): integer;
    function WeakerThan(const fNatFunc:NatFunc): boolean;
    function IsMultipleOf(const fNatFunc:NatFunc): boolean;
    procedure Add(const aFunc:NatFunc);
    function CountAll: integer; virtual;
  end;

  NatSeq = object(NatFunc)
    constructor InitNatSeq(ALimit, ADelta: Integer);
     procedure InsertElem(X:integer); virtual;
     function Value(fElem: integer): integer; virtual;
     function IndexOf(Y: integer): integer;
    end;

  IntegerListPtr = ^IntegerList;
  IntegerList = array[0..MaxIntegerListSize- 1] of integer;

  PIntSequence = ^IntSequence;
  IntSequencePtr = PIntSequence;
  IntSequence = object(MObject)
     fList: IntegerListPtr;
     fCount: integer;
     fCapacity: integer;
    constructor Init(aCapacity: Integer);
    constructor CopySequence(const aSeq: IntSequence);
    constructor MoveSequence(var aSeq: IntSequence);
    destructor Done; virtual;
    procedure IntListError(Code, Info: Integer); virtual;
    procedure SetCapacity(aCapacity: integer); virtual;
    procedure Clear; virtual;
    function Insert(aInt: integer): integer; virtual;
    procedure AddSequence(const aSeq: IntSequence); virtual;
    function IndexOf(aInt: integer): integer; virtual;
    procedure AtDelete(aIndex: integer); virtual;
    function Value(aIndex:integer): integer; virtual;
    procedure AtInsert(aIndex,aInt: integer); virtual;
    procedure AtPut(aIndex,aInt: integer); virtual;
   end;

  PIntSet = ^IntSet;
  IntSetPtr = pIntSet; 
  IntSet = object(IntSequence)
    function Insert(aInt: integer): integer; virtual;
    function DeleteInt(aInt: integer): integer; virtual;
    function Find(aInt: integer; var aIndex: integer): Boolean; virtual;
    function IndexOf(aInt: integer): integer; virtual;
    procedure AtInsert(aIndex,aInt: integer); virtual;
    function IsInSet(aInt:integer): boolean; virtual;
    function IsEqualTo(const aSet: IntSet): boolean; virtual;
    function IsSubsetOf(const aSet: IntSet): boolean; virtual;
    function IsSupersetOf(var aSet: IntSet): boolean; virtual;
    function Misses(var aSet: IntSet): boolean; virtual;
  end;

  IntTripletListPtr = ^IntTripletList;
  IntTripletList = array[0..MaxIntTripletSize- 1] of IntTriplet;

  BinIntFuncPtr = ^BinIntFunc;
  BinIntFunc = object(MObject)
      fList: IntTripletListPtr;
      fCount: Integer;
      fCapacity: Integer;
    constructor Init(aLimit: Integer);
    procedure BinIntFuncError(aCode, aInfo: Integer); virtual;
    destructor Done; virtual;

    procedure Insert(const aItem: IntTriplet); virtual;
    procedure AtDelete(aIndex: Integer);
    procedure SetCapacity(aLimit: Integer); virtual;
    procedure DeleteAll;
    function Search(X1,X2: integer; var aIndex: Integer): Boolean; virtual;
    function IndexOf(X1,X2:integer): integer;
    constructor CopyBinIntFunc(var aFunc: BinIntFunc);

    function HasInDom(X1,X2:integer): boolean; virtual;
    procedure Assign(X1,X2,Y:integer); virtual;
    procedure Up(X1,X2:integer); virtual;
    procedure Down(X1,X2:integer); virtual;
    function Value(X1,X2:integer): integer; virtual;
    procedure Add(const aFunc: BinIntFunc); virtual;
    function CountAll: integer; virtual;
  end;

  Int2PairOfInt = record X,Y1,Y2: integer; end;

  Int2PairOfIntFuncPtr = ^Int2PairOfIntFunc;
  Int2PairOfIntFunc = object(MObject)
      fList: array of Int2PairOfInt;
      fCount: Integer;
      fCapacity: Integer;
    constructor Init(aLimit: Integer);
    procedure Int2PairOfIntFuncError(aCode, aInfo: Integer); virtual;
    destructor Done; virtual;

    procedure Insert(const aItem: Int2PairOfInt); virtual;
    procedure AtDelete(aIndex: Integer);
    procedure SetCapacity(aLimit: Integer); virtual;
    procedure DeleteAll;
    function Search(X: integer; var aIndex: Integer): Boolean; virtual;
    function IndexOf(X:integer): integer;
    constructor CopyInt2PairOfIntFunc(var aFunc: Int2PairOfIntFunc);

    function HasInDom(X:integer): boolean; virtual;
    procedure Assign(X,Y1,Y2:integer); virtual;
    function Value(X:integer): IntPair; virtual;
  end;

{ Comparing Strings wrt MStrObj}

function CompareStringPtr(aKey1, aKey2: Pointer): Integer;

{ Comparing Strings and integers }
function CompareStr(aStr1, aStr2: string): Integer;
function CompareIntPairs(X1, Y1, X2,Y2: Longint): Integer;

{ Dynamic string handling routines }

function NewStr(const S: String): PString;
procedure DisposeStr(P: PString);

function GrowLimit(aLimit: integer): integer;
{ Abstract notification procedure }

function CompareNatFunc(aKey1, aKey2: Pointer): Integer;

procedure Abstract1;

var EmptyNatFunc: NatFunc;

implementation
{$IFDEF MDEBUG} uses info;{$ENDIF}

procedure Abstract1;
begin
  RunError(211);
end;

{ MObject }

constructor MObject.Init;
{$IFDEF VER70}
type Image = record Link: Word; Data: record end; end;
{$ENDIF}
begin
{$IFDEF VER70}
 FillChar(Image(Self).Data, SizeOf(Self) - SizeOf(MObject), 0);
{$ENDIF}
end;

procedure MObject.Free;
begin
  Dispose(PObject(@Self), Done);
end;

destructor MObject.Done;
begin
end;

function MObject.CopyObject: PObject;
 var lObject:PObject;
begin GetMem(lObject,SizeOf(Self));
 Move(Self,lObject^,SizeOf(Self));
 CopyObject:=lObject;
end;

function MObject.MCopy: PObject;
begin
 MCopy:=CopyObject;
end;

{ Specyfic objects based on MObjects for collections}

constructor MStrObj.Init(const aStr: string);
begin
 fStr:=aStr;
end;

{ MCollection }

constructor MCollection.Init(ALimit, ADelta: Integer);
begin
  MObject.Init;
  Items := nil;
  Count := 0;
  Limit := 0;
  Delta := ADelta;
  SetLimit(ALimit);
end;

destructor MCollection.Done;
begin
  FreeAll;
  SetLimit(0);
end;

procedure MCollection.AtDelete(Index: Integer);
 var i: integer;
begin
   if (Index < 0) or (Index >= Count) then
   begin
      ListError(coIndexError,0);
      exit;
   end;
   if Index < pred(Count) then
     for i:=Index to Count-2 do Items^[i]:=Items^[i+1];
     {move( Items^[succ(Index)], Items^[Index], (Count-index)*sizeof(pointer));}
   Dec(Count);
end;

procedure MCollection.AtFree(Index: Integer);
var lItem: Pointer;
begin
  lItem := At(Index);
  AtDelete(Index);
  FreeItem(lItem);
end;

procedure MCollection.AtInsert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or ( Index > Count) then
   begin
      ListError(coIndexError,0);
      exit;
   end;
  if Limit = Count then
   begin
    if Delta = 0 then
     begin
        ListError(coOverFlow,0);
        exit;
     end;
    SetLimit( Limit+Delta);
   end;
  If Index <> Count then 
      move( Items^[Index], Items^[Index+1], (Count - index)*sizeof(pointer));
  Items^[Index] := Item;
  Inc(Count);
end;

procedure MCollection.AtPut(Index: Integer; Item: Pointer);
begin
   if (Index < 0) or (Index >= Count) then
     ListError(coIndexError,0)
   else Items^[Index] := Item;
end;

procedure MCollection.Delete(Item: Pointer);
begin
  AtDelete(IndexOf(Item));
end;

procedure MCollection.Free(Item: Pointer);
begin
  Delete(Item);
  FreeItem(Item);
end;

procedure MCollection.Insert(aItem: Pointer);
begin
  AtInsert(Count, aItem);
end;

procedure MCollection.Pack;
 var i: integer;
begin
  for i := pred(Count) downto 0 do
   if Items^[i] = nil then AtDelete(i);
end;

constructor MCollection.MoveCollection(var fAnother:MCollection);
begin
 Init(0,fAnother.Delta);
 TransferItems(fAnother)
end;

constructor MCollection.CopyCollection(var AAnother:MCollection);
 var i: integer;
begin
 Init(AAnother.Limit,AAnother.Delta);
 for i:=0 to AAnother.Count-1 do
  Insert(aAnother.Items^[i]);
// ??? Insert(PObject(aAnother.Items^[i])^.MCopy); ???
end;

constructor MCollection.Singleton(fSing:PObject; fDelta:Integer);
begin Init(2,fDelta); Insert(fSing) end;

procedure MCollection.Prune;
begin SetLimit(0) end;

constructor MCollection.MoveList(var aAnother:MList);
begin
 inherited MoveList(aAnother);
 Delta := 2;
end;

constructor MCollection.CopyList(var aAnother:MList);
begin
 inherited CopyList(aAnother);
 Delta := 2;
end;

{ Simple Collection }

function GrowLimit(aLimit: integer): integer;
begin GrowLimit:=4;
  if aLimit > 64 then GrowLimit := aLimit div 4
  else if aLimit > 8 then GrowLimit := 16;
end;

constructor MList.Init(aLimit: Integer);
begin
 MObject.Init;
 Items := nil;
 Count := 0;
 Limit := 0;
 SetLimit(aLimit);
end;

constructor MList.MoveList(var aAnother:MList);
begin
 MObject.Init;
 Count := aAnother.Count;
 Limit := aAnother.Limit;
 Items := aAnother.Items;
 aAnother.DeleteAll;
 aAnother.Limit:=0;
 aAnother.Items:=nil;
end;

constructor MList.CopyList(var aAnother:MList);
begin
 MObject.Init;
 Items := nil;
 Count := 0;
 Limit := 0;
 SetLimit(aAnother.Limit);
 InsertList(aAnother);
end;

destructor MList.Done;
begin
  FreeAll;
  SetLimit(0);
  inherited Done;
end;

function MList.MCopy: PObject;
  var lList: PObject; i: integer;
begin
 lList:=CopyObject;
 GetMem(PList(lList)^.Items, self.Limit * SizeOf(Pointer));
 for i:=0 to self.Count-1 do
  PList(lList)^.Items^[i]:=PObject(self.Items^[i])^.MCopy;
 MCopy:=lList;
end;

function MList.At(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= Count) then
   begin
     ListError(coIndexError,0);
     At :=nil;
   end
  else At := Items^[Index];
end;

function MList.Last: Pointer;
begin Last:= At( Count - 1); end;

procedure MList.Insert(aItem: Pointer);
begin
 if Limit = Count then
   SetLimit(Limit+GrowLimit(Limit));
 Items^[Count] := aItem;
 Inc(Count);
end;

procedure MList.AtInsert(aIndex: Integer; aItem: Pointer);
 var i,lLimit: integer;
begin
  if aIndex < 0 then
   begin
    ListError(coIndexError,0);
    exit;
   end;
  if (aIndex >= Limit) or ((aIndex = Count) and (Limit = Count)) then
   begin
    lLimit:=Limit+GrowLimit(Limit);
    while aIndex+1 > lLimit do
     lLimit:=lLimit+GrowLimit(lLimit);
    SetLimit(lLimit);
   end;
  for i:=Count to aIndex-1 do
   Items^[i]:=nil;
  Items^[aIndex]:=aItem;
  if aIndex >= Count then
   Count:=aIndex+1;
end;

procedure MList.InsertList(var aAnother: MList);
 var i: integer;
begin
 for i:=0 to pred(aAnother.Count) do
  Insert(PObject(aAnother.Items^[i])^.MCopy);
end;

function MList.GetObject(aIndex: integer): Pointer;
begin
 if (aIndex < 0) or (aIndex >= Count) then
  begin
    ListError(coIndexError,0);
    GetObject :=nil;
  end
 else GetObject := Items^[aIndex];
end;

procedure MList.ListError(aCode, aInfo: Integer);
begin
  RunError(212 - aCode);
end;

function MList.IndexOf(aItem: Pointer): Integer;
 var i : integer;
begin
 IndexOf := -1;
 for i := 0 to pred(Count) do
  if aItem = Items^[i] then
   begin
    IndexOf := i;
    break
   end
end;

procedure MList.DeleteAll;
begin
  Count := 0;
end;

procedure MList.FreeItem(Item: Pointer);
begin
  if Item <> nil then Dispose(PObject(Item), Done);
end;

procedure MList.FreeAll;
begin
 FreeItemsFrom(0);
end;

procedure MList.FreeItemsFrom(aIndex:integer);
 var I:integer;
begin
 for I:=Count-1 downto aIndex do FreeItem(Items^[I]);
 Count:=aIndex;
end;

procedure MList.Pack;
 var i,k: integer;
begin
  for i := Count-1 downto 0 do
   if Items^[i] = nil then
    begin
     for k:=i to Count-2 do Items^[k]:=Items^[k+1];
     dec(Count);
    end;
end;

procedure MList.SetLimit(ALimit: Integer);
 var lItems: PItemList;
begin
 if ALimit < Count then ALimit := Count;
 if ALimit > MaxCollectionSize then ALimit := MaxCollectionSize;
 if ALimit <> Limit then
  begin
   if ALimit = 0 then lItems := nil else
    begin
      GetMem(lItems, ALimit * SizeOf(Pointer));
      if ((Count) <> 0) and (Items <> nil) then
        Move(Items^, lItems^, Count*SizeOf(Pointer));
    end;
   if Limit <> 0 then FreeMem(Items, Limit*SizeOf(Pointer));
   Items := lItems;
   Limit := ALimit;
  end;
end;

procedure MList.AppendTo(var fAnother:MList);
 var k:integer;
begin SetLimit(Count+fAnother.Count);
 for k:=0 to fAnother.Count-1 do Insert(fAnother.Items^[k]);
 fAnother.DeleteAll; fAnother.Done;
end;

procedure MList.TransferItems(var fAnother:MList);
{ Przeznaczeniem tej procedury jest uzycie jej w konstruktorach Move,
  ktore wykonuja jakgdyby pelna instrukcje przypisania (razem z VMTP).
}
begin Self:=fAnother; fAnother.DeleteAll;
 fAnother.Limit:=0; fAnother.Items:=nil;
 { Nie wolno uzyc SetLimit, bo rozdysponuje Items. }
end;

procedure MList.CopyItems(var fOrigin:MList);
 var i:integer;
begin
 for i:=0 to fOrigin.Count-1 do
  Insert(PObject(fOrigin.Items^[i])^.CopyObject);
end;

{ Simple Stacked (Extendable) Collection }

constructor MExtList.Init(ALimit: Integer);
begin
 MObject.Init;
 Items := nil;
 Count := 0;
 Limit := 0;
 SetLimit(ALimit);
 fExtCount:=0;
end;

destructor MExtList.Done;
begin
 FreeExtItems;
 inherited Done;
end;

procedure MExtList.Insert(aItem: Pointer);
begin
 if fExtCount <> 0 then
  begin
    ListError(coIndexExtError,0);
    exit;
  end;
 if Limit = Count then
   SetLimit(Limit+GrowLimit(Limit));
 Items^[Count] := aItem;
 Inc(Count);
end;

procedure MExtList.DeleteAll;
begin
 if fExtCount <> 0 then
  begin
    ListError(coIndexExtError,0);
    exit;
  end;
 inherited DeleteAll;
end;

procedure MExtList.FreeAll;
begin
 if fExtCount <> 0 then
  begin
    ListError(coIndexExtError,0);
    exit;
  end;
 inherited FreeAll;
end;

procedure MExtList.Pack;
begin
 if fExtCount <> 0 then
  begin
    ListError(coIndexExtError,0);
    exit;
  end;
 inherited Pack;
end;

procedure MExtList.InsertExt(AItem: Pointer);
begin
 if Limit = Count+fExtCount then
   SetLimit(Limit+GrowLimit(Limit));
 Items^[Count+fExtCount] := AItem;
 Inc(fExtCount);
end;

procedure MExtList.SetLimit(ALimit: Integer);
 var lItems: PItemList;
begin
 if ALimit < Count+fExtCount then ALimit := Count+fExtCount;
 if ALimit > MaxCollectionSize then ALimit := MaxCollectionSize;
 if ALimit <> Limit then
  begin
   if ALimit = 0 then lItems := nil else
    begin
      GetMem(lItems, ALimit * SizeOf(Pointer));
      if ((Count+fExtCount) <> 0) and (Items <> nil) then
        Move(Items^, lItems^, (Count+fExtCount)*SizeOf(Pointer));
    end;
   if Limit <> 0 then FreeMem(Items, Limit*SizeOf(Pointer));
   Items := lItems;
   Limit := ALimit;
  end;
end;

procedure MExtList.Mark(var aIndex:integer);
begin
 aIndex:=Count;
end;

procedure MExtList.FreeItemsFrom(aIndex:integer);
 var I:integer;
begin
 if fExtCount <> 0 then
  begin
    ListError(coIndexExtError,0);
    exit;
  end;
 for I:=Count-1 downto aIndex do
  if Items^[I] <> nil then Dispose(PObject(Items^[I]), Done);
 Count:=aIndex;
end;

procedure MExtList.AddExtObject;
begin
 if fExtCount <= 0 then
  begin
    ListError(coIndexExtError,0);
    exit;
  end;
 inc(Count);
 dec(fExtCount);
end;

procedure MExtList.AddExtItems;
begin
 Count:=Count+fExtCount;
 fExtCount:=0;
end;

procedure MExtList.DeleteExtItems;
begin
 fExtCount:=0;
end;

procedure MExtList.FreeExtItems;
 var I: integer;
begin
 for I := 0 to fExtCount-1 do
  if Items^[Count+I] <> nil then Dispose(PObject(Items^[Count+I]), Done);
 fExtCount := 0;
end;

// MSortedExtList always with possible duplicate keys,
// allways sorted
constructor MSortedExtList.Init(ALimit: Integer);
begin ListError(coIndexExtError,0); end;

constructor MSortedExtList.InitSorted(aLimit: Integer; aCompare: CompareProc);
begin
 inherited Init( aLimit);
 fCompare := aCompare;
end;

destructor MSortedExtList.Done;
begin
 inherited Done;
end;

// find the left-most if duplicates
function MSortedExtList.Find(aKey: Pointer; var aIndex: Integer): Boolean;
var L,H,I,C: integer;
begin
 if not Assigned( fCompare) then ListError(coIndexExtError,0);
 Find := False;
 L := 0;
 H := Count-1;
 while L <= H do
  begin
   I := (L + H) shr 1;
   C := fCompare(Items^[fIndex^[I]], aKey);
   if C < 0 then L := I + 1
   else
    begin
     H := I - 1;
     if C = 0 then Find := True;
    end;
  end;
 aIndex := L;
end;

// find the left-most with higher aKey, this is where we can insert
function MSortedExtList.FindRight(aKey: Pointer; var aIndex: Integer): Boolean;
begin
 if Find(aKey, aIndex) then
 begin
   while (aIndex < Count)
         and (0 = fCompare(Items^[fIndex^[ aIndex]], aKey)) do Inc( aIndex);
   FindRight := true;
 end
 else FindRight := false;
end;

// find the interval of equal guys
function MSortedExtList.FindInterval(aKey: Pointer; var aLeft,aRight: Integer): Boolean;
begin
 if Find(aKey, aLeft) then
 begin
  aRight:= aLeft + 1;
  while (aRight < Count)
        and (0 = fCompare(Items^[fIndex^[ aRight]], aKey)) do Inc( aRight);
  dec( aRight);
  FindInterval := true;
 end
 else begin aRight:= aLeft - 1; FindInterval := false; end;
end;

function MSortedExtList.AtIndex( aIndex: integer): Pointer;
begin
 if (aIndex < 0) or (aIndex >= Count) then
  ListError(coIndexExtError,0);
 AtIndex:= Items^[fIndex^[ aIndex]];
end;
   
procedure MSortedExtList.Insert(aItem: Pointer);
begin
 if fExtCount <> 0 then ListError(coIndexExtError,0);
 InsertExt( aItem);
 AddExtObject;
end;

procedure MSortedExtList.Pack;
begin ListError(coIndexExtError,0); end;

procedure MSortedExtList.InsertExt(AItem: Pointer);
begin
  if Limit = Count+fExtCount then
   SetLimit(Limit+GrowLimit(Limit));
 Items^[Count+fExtCount] := AItem;
 Inc(fExtCount);
end;

procedure MSortedExtList.SetLimit(ALimit: Integer);
var lItems: PItemList; lIndex: IndexListPtr;
begin
 Count:= Count + fExtCount;
 if aLimit < Count then aLimit := Count;
 if aLimit > MaxCollectionSize then aLimit := MaxCollectionSize;
 if aLimit <> Limit then
  begin
   if aLimit = 0 then
    begin
     lItems:=nil;
     lIndex:=nil;
    end
   else
    begin
     GetMem(lItems, aLimit*SizeOf(Pointer));
     GetMem(lIndex, aLimit*SizeOf(integer));
     if Count <> 0 then
       begin
        if Items <> nil then
         begin
          Move(Items^, lItems^, Count*SizeOf(Pointer));
          Move(fIndex^, lIndex^, Count*SizeOf(integer));
         end;
       end;
    end;
   if Limit <> 0 then
    begin
     FreeMem(Items, Limit*SizeOf(Pointer));
     FreeMem(fIndex, Limit*SizeOf(integer));
    end;
   Items := lItems;
   fIndex := lIndex;
   Limit := aLimit;
  end;
 Count:= Count - fExtCount;
end;

procedure MSortedExtList.FreeItemsFrom(aIndex:integer);
var I,k:integer;
begin
 if fExtCount <> 0 then ListError(coIndexExtError,0);
 if aIndex = Count then exit;
 for I:=aIndex to Count-1 do
  if Items^[I] <> nil then Dispose(PObject(Items^[I]), Done);
 k:=0;
 for I:=0 to Count-1 do
  begin
   if fIndex^[I] < aIndex then
    begin
     fIndex^[k]:=fIndex^[I];
     inc(k);
    end;
  end;
 if k <> aIndex then ListError(coSortedListError, 0);
 Count:=aIndex;
end;

procedure MSortedExtList.AddExtObject;
var lIndex: integer;
begin
 if fExtCount <= 0 then ListError(coIndexExtError,0);
 FindRight( Items^[Count], lIndex);
 If lIndex <> Count then
  move(fIndex^[lIndex], fIndex^[lIndex+1],(Count-lIndex)*sizeof(integer));
 fIndex^[lIndex] := Count;
 inc(Count);
 dec(fExtCount);
end;

procedure MSortedExtList.AddExtItems;
begin while fExtCount > 0 do AddExtObject; end;

{ MSortedList  object}

constructor MSortedList.Init(aLimit: Integer);
begin
 MObject.Init;
 Items := nil;
 Count := 0;
 Limit := 0;
 fIndex:=nil;
 fCompare:=nil;
 SetLimit(ALimit);
end;

constructor MSortedList.InitSorted(aLimit: Integer; aCompare: CompareProc);
begin
 MObject.Init;
 Items := nil;
 Count := 0;
 Limit := 0;
 fIndex:=nil;
 fCompare:=aCompare;
 SetLimit(ALimit);
end;

constructor MSortedList.MoveList(var aAnother:MList);
 var I: integer;
begin
 Items := aAnother.Items;
 Count := aAnother.Count;
 Limit := aAnother.Limit;
 GetMem(fIndex, Limit * SizeOf(integer));
 fCompare:=nil;
 for I:=0 to pred(aAnother.Count) do fIndex^[I]:=I;
 aAnother.DeleteAll;
 aAnother.Limit:=0; aAnother.Items:=nil;
end;

constructor MSortedList.CopyList(const aAnother:MList);
 var i: integer;
begin
 MObject.Init;
 Items := nil;
 Count := 0;
 Limit := 0;
 fIndex:=nil;
 fCompare:=nil;
 SetLimit(aAnother.Limit);
 Count := aAnother.Count;
 for i:=0 to Count-1 do
  begin
    Items^[i]:=PObject(aAnother.Items^[i])^.MCopy;
    fIndex^[I]:=I;
  end;
end;

{ used in CollectCluster not to repeat the search, should
  be used only when @fCompare <> nil }
procedure MSortedList.AtInsert(aIndex:integer; aItem: Pointer);
begin
   if Limit = Count then
     SetLimit(Limit+GrowLimit(Limit));
   If aIndex <> Count then
     move(fIndex^[aIndex], fIndex^[aIndex+1],(Count-aIndex)*sizeof(integer));
   Items^[Count] := aItem;
   fIndex^[aIndex] := Count;
   Inc(Count);
end;

procedure MSortedList.Insert(aItem: Pointer);
 var lIndex: Integer;
begin
 if @fCompare = nil then
  begin
   if Limit = Count then
     SetLimit(Limit+GrowLimit(Limit));
   Items^[Count] := aItem;
   fIndex^[Count] := Count;
   Inc(Count);
   exit;
  end;
 if not Find(aItem, lIndex) then
   AtInsert(lIndex,aItem);
end;

procedure MSortedList.SetLimit(aLimit: Integer);
 var lItems: PItemList; lIndex: IndexListPtr;
begin
 if aLimit < Count then aLimit := Count;
 if aLimit > MaxCollectionSize then aLimit := MaxCollectionSize;
 if aLimit <> Limit then
  begin
   if aLimit = 0 then
    begin
     lItems:=nil;
     lIndex:=nil;
    end
   else
    begin
     GetMem(lItems, aLimit*SizeOf(Pointer));
     GetMem(lIndex, aLimit*SizeOf(integer));
     if Count <> 0 then
       begin
        if Items <> nil then
         begin
          Move(Items^, lItems^, Count*SizeOf(Pointer));
          Move(fIndex^, lIndex^, Count*SizeOf(integer));
         end;
       end;
    end;
   if Limit <> 0 then
    begin
     FreeMem(Items, Limit*SizeOf(Pointer));
     FreeMem(fIndex, Limit*SizeOf(integer));
    end;
   Items := lItems;
   fIndex := lIndex;
   Limit := aLimit;
  end;
end;

procedure ListQuickSort(aList: PItemList; aIndex: IndexListPtr;
                    L, R: Integer; aCompare: CompareProc);
 var I, J, T : Integer; P: Pointer;
begin
 repeat
   I := L;
   J := R;
   P := aList^[aIndex^[(L + R) shr 1]];
   repeat
     while aCompare(aList^[aIndex^[I]], P) < 0 do Inc(I);
     while aCompare(aList^[aIndex^[J]], P) > 0 do Dec(J);
     if I <= J then
      begin
       T := aIndex^[I];
       aIndex^[I] := aIndex^[J];
       aIndex^[J] := T;
       Inc(I);
       Dec(J);
      end;
   until I > J;
   if L < J then ListQuickSort(aList, aIndex, L, J, aCompare);
   L := I;
 until I >= R;
end;

procedure MSortedList.Sort(aCompare: CompareProc);
 var I: integer;
begin
 fCompare:=aCompare;
 for I:=0 to Count-1 do fIndex^[I]:=I;
 if (Count > 0) then
   ListQuickSort(Items, fIndex, 0, Count-1, aCompare);
end;

function MSortedList.Find(aKey: Pointer; var aIndex: Integer): Boolean;
 var L, H, I, C: Integer;
begin
 Find := False;
 if @fCompare = nil then
  begin
   aIndex := Count;
   for I := 0 to Count-1 do
    if aKey = Items^[I] then
     begin
      Find := True;
      aIndex := I;
      break
     end;
   exit;
  end;
 L := 0;
 H := Count-1;
 while L <= H do
  begin
   I := (L + H) shr 1;
   C := fCompare(Items^[fIndex^[I]], aKey);
   if C < 0 then L := I + 1
   else
    begin
     H := I - 1;
     if C = 0 then
      begin
       Find := True;
       L := I;
      end;
    end;
  end;
 aIndex := L;
end;

function MSortedList.Search(aKey: Pointer; var aIndex: Integer): Boolean;
 var I: Integer;
begin
 aIndex:=Count;
 Search:=false;
 if Find(aKey, I) then
  begin Search:=true;
   aIndex := fIndex^[I];
  end;
end;

function MSortedList.IndexOf(aItem: Pointer): Integer;
 var I: Integer;
begin
 if @fCompare = nil then
  begin
   ListError(coSortedListError,0);
   exit;
  end;
 IndexOf := -1;
 if Find(aItem, I) then
  begin
   {if I < fCount then }
   IndexOf := fIndex^[I];
  end;
end;

procedure MSortedList.Pack;
 var lCount: integer;
begin
 lCount:=Count;
 inherited Pack;
 if(@fCompare <> nil) and (lCount > Count) then Sort(fCompare);
end;

procedure MSortedList.FreeItemsFrom(aIndex:integer);
 var I,k:integer;
begin
 if aIndex = Count then exit;
 for I:=aIndex to Count-1 do
  if Items^[I] <> nil then Dispose(PObject(Items^[I]), Done);
 k:=0;
 for I:=0 to Count-1 do
  begin
   if fIndex^[I] < aIndex then
    begin
     fIndex^[k]:=fIndex^[I];
     inc(k);
    end;
  end;
 if k <> aIndex then ListError(coSortedListError, 0);
 Count:=aIndex;
end;

{ MSortedStrList}

function CompareStringPtr(aKey1, aKey2: Pointer): Integer;
begin
  if PStr(aKey1)^.fStr < PStr(aKey2)^.fStr then
    CompareStringPtr := -1
  else if PStr(aKey1)^.fStr = PStr(aKey2)^.fStr then
    CompareStringPtr := 0
  else
    CompareStringPtr := 1;
end;

constructor MSortedStrList.Init(ALimit: Integer);
begin
 InitSorted(ALimit,CompareStringPtr);
end;

function MSortedStrList.IndexOfStr(const aStr: string): Integer;
 var I: Integer; lStringObj: MStrObj;
begin
 IndexOfStr := -1;
 if @fCompare = nil then
  begin
   ListError(coSortedListError,0);
   exit;
  end;
 lStringObj.Init(aStr);
 if Find(@lStringObj, I) then
  begin
   if I < Count then IndexOfStr := fIndex^[I];
  end;
end;

function MSortedStrList.ObjectOf(const aStr: string): PObject;
 var I: integer;
begin
 ObjectOf:=nil;
 I:=IndexOfStr(aStr);
 if I>=0 then ObjectOf:=Items^[I];
end;

{ MSortedCollection }

constructor MSortedCollection.Init(aLimit, aDelta: integer);
begin
  inherited Init(ALimit, ADelta);
  Duplicates := False;
  fCompare := nil;
end;

constructor MSortedCollection.InitSorted(aLimit, aDelta: integer; aCompare: CompareProc);
begin
  inherited Init(ALimit, ADelta);
  Duplicates := False;
  fCompare := aCompare;
end;

function MSortedCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  if @fCompare = nil then Abstract1;
  Compare := fCompare(Key1, Key2);
end;

function MSortedCollection.IndexOf(aItem: Pointer): Integer;
var I: Integer;
begin
  IndexOf := -1;
  if Search(KeyOf(aItem), I) then
  begin
    if Duplicates then
      while (I < Count) and (aItem <> Items^[I]) do Inc(I);
    if I < Count then IndexOf := I;
  end;
end;

procedure MSortedCollection.Insert(aItem: Pointer);
var I: Integer;
begin
  if not Search(KeyOf(aItem), I) or Duplicates then AtInsert(I, aItem);
end;

procedure MSortedCollection.InsertD(Item: Pointer);
var I: Integer;
begin
  if not Search(KeyOf(Item), I) or Duplicates then AtInsert(I, Item)
  else Dispose(PObject(Item),Done);;
end;

function MSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  KeyOf := Item;
end;

function MSortedCollection.Search(Key: Pointer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Search := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(Items^[I]), Key);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        if not Duplicates then L := I;
      end;
    end;
  end;
  Index := L;
end;

function CompareIntPairs(X1, Y1, X2,Y2: Longint): Integer;
var lRes: integer;
begin
 lRes := CompareInt(X1,X2);
 if lRes = 0 then
  lRes := CompareInt(Y1,Y2);
 CompareIntPairs := lRes;
end;

{ MStringCollection }

function CompareStr(aStr1, aStr2: string): Integer;
begin
  if aStr1 < aStr2 then
    CompareStr := -1
  else if aStr1 = aStr2 then
    CompareStr := 0
  else
    CompareStr := 1;
end;

function MStringCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Compare := CompareStr(PString(Key1)^,PString(Key2)^);
end;

procedure MStringCollection.FreeItem(Item: Pointer);
begin
  DisposeStr(Item);
end;

{ UnsortedStringCollection }

procedure StringColl.FreeItem(Item:pointer);
begin
  DisposeStr(Item);
end;

{ MIntCollection }
constructor TIntItem.Init(fInt:integer);
begin IntKey:=fInt; end;

function TIntKeyCollection.KeyOf(Item:pointer):pointer;
begin KeyOf:=addr(PIntItem(Item)^.IntKey); end;

function TIntKeyCollection.Compare(Key1,Key2:pointer): integer;
begin Compare:=1;
 if IntPtr(Key1)^ < IntPtr(Key2)^ then begin Compare:=-1; exit end;
 if IntPtr(Key1)^ = IntPtr(Key2)^ then Compare:=0;
end;

constructor IntPairItem.Init(X,Y: integer);
begin fKey.X:=X; fKey.Y:=Y; end;

function IntPairKeyCollection.Compare(Key1,Key2:pointer): integer;
begin
 Compare:=CompareIntPairs(IntPairItemPtr(Key1)^.fKey.X,IntPairItemPtr(Key1)^.fKey.Y,
            IntPairItemPtr(Key2)^.fKey.X,IntPairItemPtr(Key2)^.fKey.Y);
end;

function IntPairKeyCollection.ObjectOf(X,Y: integer): IntPairItemPtr;
 var lPairItem: IntPairItem; I: integer;
begin ObjectOf:=nil;
  lPairItem.Init(X,Y);
  if Search(addr(lpairItem),I) then
   ObjectOf:=Items^[I];
end;

function IntPairKeyCollection.FirstThat(X: integer): IntPairItemPtr;
 var I: integer;
begin FirstThat:=nil;
  for i:=0 to Count-1 do
   if IntPairItemPtr(Items^[I])^.fKey.X = X then
    begin FirstThat:=Items^[I]; exit end;
end;

{Stacked Object (List of objects)}

constructor StackedObj.Init;
begin Abstract1; end;

destructor StackedObj.Done;
begin Abstract1; end;

{--------------------  MStringList ---------------------------------}

constructor MStringList.Init(aCapacity: integer);
begin
  MObject.Init;
  fList := nil;
  fCount := 0;
  fCapacity := 0;
  fSorted := false;
  fDuplicate:=dupError;
  SetCapacity(aCapacity);
end;

constructor MStringList.MoveStringList(var aAnother:MStringList);
begin
 MObject.Init;
 fCount := aAnother.fCount;
 fCapacity := aAnother.fCapacity;
 fSorted := aAnother.fSorted;
 fList := aAnother.fList;
 fDuplicate := aAnother.fDuplicate;
 
 aAnother.fCount := 0;
 aAnother.fCapacity:=0;
 aAnother.fList:=nil;
end;

destructor MStringList.Done;
 var I: integer;
begin
  inherited Done;
  for I:=0 to fCount-1 do
   with fList^[I] do
   begin
    DisposeStr(fString);
    if fObject <> nil then Dispose(fObject,Done);
   end;
  fCount := 0;
  SetCapacity(0);
end;

function MStringList.AddString(const aStr: string): integer;
 var lResult: integer;
begin
  if not fSorted then
    lResult := fCount
  else
    if Find(aStr, lResult) then
    begin AddString:=lResult;
     case fDuplicate of
       dupIgnore: Exit;
       dupError: StringListError(coDuplicate, 0);
     end;
    end;
  InsertItem(lResult, aStr);
  AddString:=lResult;
end;

function MStringList.AddObject(const aStr: string; aObject: PObject): integer;
 var lResult: integer;
begin
  lResult := AddString(aStr);
  PutObject(lResult, aObject);
  AddObject:=lResult;
end;

procedure MStringList.AddStrings(var aStrings: MStringList);
var I,r: integer;
begin
  for I := 0 to aStrings.fCount - 1 do
    r:=AddObject(aStrings.fList^[I].fString^, aStrings.fList^[I].fObject);
end;

procedure MStringList.Clear;
 var I: integer;
begin
  if fCount <> 0 then
  begin
    for I:=0 to fCount-1 do DisposeStr(fList^[I].fString);
    fCount := 0;
    SetCapacity(0);
  end;
end;

procedure MStringList.Delete(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex >= fCount) then
   StringListError(coIndexError, aIndex);
  DisposeStr(fList^[aIndex].fString);
  Dec(fCount);
  if aIndex < fCount then
    Move(fList^[aIndex + 1], fList^[aIndex],
      (fCount - aIndex) * SizeOf(MStringItem));
end;

procedure MStringList.Exchange(Index1, Index2: integer);
begin
  if (Index1 < 0) or (Index1 >= fCount) then
   StringListError(coIndexError, Index1);
  if (Index2 < 0) or (Index2 >= fCount) then
   StringListError(coIndexError, Index2);
  ExchangeItems(Index1, Index2);
end;

procedure MStringList.ExchangeItems(Index1, Index2: integer);
var Temp: MStringItem;
begin
  Temp := fList^[Index1];
  fList^[Index1] := fList^[Index2];
  fList^[Index2] := Temp;
end;

function MStringList.Find(const aStr: string; var aIndex: integer): Boolean;
var
  L, H, I, C: integer;
  lResult: boolean;
begin
  lResult := False;
  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(fList^[I].fString^, aStr);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        lResult := True;
        if fDuplicate <> dupAccept then L := I;
      end;
    end;
  end;
  aIndex := L;
  Find:=lResult;
end;

procedure MStringList.StringListError(Code, Info: Integer);
begin
  RunError(212 - Code); {! poprawic bledy}
end;

function MStringList.GetString(aIndex: integer): string;
begin
  if (aIndex < 0) or (aIndex >= fCount) then
   StringListError(coIndexError, aIndex);
  GetString:='';
  if fList^[aIndex].fString <> nil then
   GetString := fList^[aIndex].fString^;
end;

function MStringList.GetObject(aIndex: integer): PObject;
begin
  if (aIndex < 0) or (aIndex >= fCount) then
   StringListError(coIndexError, aIndex);
  GetObject := fList^[aIndex].fObject;
end;

procedure MStringList.Grow;
var
  Delta: integer;
begin
  if fCapacity > 64 then Delta := fCapacity div 4 else
    if fCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(fCapacity + Delta);
end;

function MStringList.IndexOf(const aStr: string): integer;
 var lResult: integer;
begin
  if not fSorted then
   begin
    for lResult := 0 to fCount - 1 do
     if CompareStr(fList^[lResult].fString^, aStr) = 0 then
      begin
       IndexOf:=lResult;
       Exit;
      end;
    lResult := -1;
   end
  else
    if not Find(aStr, lResult) then lResult := -1;
  IndexOf:=lResult;
end;

function MStringList.ObjectOf(const aStr: string): PObject;
 var I: integer;
begin
 ObjectOf:=nil;
 I:=IndexOf(aStr);
 if I>=0 then ObjectOf:=fList^[I].fObject;
end;

procedure MStringList.Insert(aIndex: integer; const aStr: string);
begin
  if fSorted then
   StringListError(coSortedListError, 0);
  if (aIndex < 0) or (aIndex > fCount) then
   StringListError(coIndexError, aIndex);
  InsertItem(aIndex, aStr);
end;

procedure MStringList.InsertItem(aIndex: integer; const aStr: string);
begin
  if fCount = fCapacity then Grow;
  if aIndex < fCount then
    Move(fList^[aIndex], fList^[aIndex + 1],
      (fCount - aIndex) * SizeOf(MStringItem));
  with fList^[aIndex] do
  begin
    fObject := nil;
    fString := NewStr(aStr);
  end;
  Inc(fCount);
end;

function MStringList.IndexOfObject(aObject: PObject): integer;
 var lResult: integer;
begin
  for lResult := 0 to fCount - 1 do
    if GetObject(lResult) = aObject then
     begin
      IndexOfObject:=lResult;
      Exit;
     end;
  IndexOfObject := -1;
end;

procedure MStringList.InsertObject(aIndex: integer; const aStr: string; aObject: PObject);
begin
  Insert(aIndex, aStr);
  PutObject(aIndex, aObject);
end;

procedure MStringList.MoveObject(CurIndex, NewIndex: integer);
var
  TempObject: PObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    TempString := GetString(CurIndex);
    TempObject := GetObject(CurIndex);
    Delete(CurIndex);
    InsertObject(NewIndex, TempString, TempObject);
  end;
end;

procedure MStringList.PutString(aIndex: integer; const aStr: string);
begin
  if fSorted then StringListError(coSortedListError, 0);
  if (aIndex < 0) or (aIndex >= fCount) then
   StringListError(coIndexError, aIndex);
  fList^[aIndex].fString := NewStr(aStr);
end;

procedure MStringList.PutObject(aIndex: integer; aObject: PObject);
begin
  if (aIndex < 0) or (aIndex >= fCount) then
   StringListError(coIndexError, aIndex);
  fList^[aIndex].fObject := aObject;
end;

procedure MStringList.QuickSort(L, R: integer);
var
  I, J: integer;
  P: string;
begin
  repeat
    I := L;
    J := R;
    P := fList^[(L + R) shr 1].fString^;
    repeat
      while CompareStr(fList^[I].fString^, P) < 0 do Inc(I);
      while CompareStr(fList^[J].fString^, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure MStringList.SetCapacity(aCapacity: integer);
var
  lList: PStringItemList;
begin
  if aCapacity < fCount then aCapacity := fCount;
  if aCapacity > MaxListSize then aCapacity := MaxListSize;
  if aCapacity <> fCapacity then
  begin
    if aCapacity = 0 then lList := nil else
    begin
      GetMem(lList, aCapacity * SizeOf(MStringItem));
      if (fCount <> 0) and (fList <> nil) then
        Move(fList^, lList^, fCount * SizeOf(MStringItem));
    end;
    if fCapacity <> 0 then FreeMem(fList, fCapacity * SizeOf(MStringItem));
    fList := lList;
    fCapacity := aCapacity;
  end;
{  ReallocMem(fList, NewCapacity * SizeOf(MStringItem));
  fCapacity := NewCapacity;}
end;

procedure MStringList.SetSorted(aValue: Boolean);
begin
  if fSorted <> aValue then
  begin
    if aValue then Sort;
    fSorted := aValue;
  end;
end;

procedure MStringList.Sort;
begin
  if not fSorted and (fCount > 1) then
  begin fSorted:=true;
    QuickSort(0, fCount - 1);
  end;
end;


{ Dynamic string handling routines }

function NewStr(const S: String): PString;
var
  P: PString;
begin
  if S = '' then P := nil else
  begin
    GetMem(P, Length(S) + 1);
    P^ := S;
  end;
  NewStr := P;
end;

procedure DisposeStr(P: PString);
begin
  if P <> nil then FreeMem(P, Length(P^) + 1);
end;

{ Pairs of an Integers}

procedure IntPairSeq.NatSetError(Code, Info: Integer);
begin
  RunError(212 - Code);
end;

constructor IntPairSeq.Init(aLimit: Integer);
begin
  MObject.Init;
  Items := nil;
  Count := 0;
  Limit := 0;
  SetLimit(aLimit);
end;

destructor IntPairSeq.Done;
begin
  Count := 0;
  SetLimit(0);
end;

procedure IntPairSeq.Insert(const aItem: IntPair);
begin
 if Count >= MaxIntPairSize then NatSetError(coOverflow,0);
 if Limit = Count then
   SetLimit(Limit+ GrowLimit(Limit));
 Items^[Count] := aItem;
 Inc(Count);
end;

procedure IntPairSeq.AtDelete(aIndex: Integer);
 var i: integer;
begin
   if (aIndex < 0) or (aIndex >= Count) then
   begin
     NatSetError(coIndexError,0);
     exit;
   end;
   if aIndex < Count-1 then
     for i:=aIndex to Count-2 do Items^[i]:=Items^[i+1];
     {move( Items^[succ(Index)], Items^[Index], (Count-Index)*sizeof(IntTriplet));}
   Dec(Count);
end;

procedure IntPairSeq.SetLimit(aLimit: Integer);
 var aItems: IntPairListPtr;
begin
  if aLimit < Count then aLimit := Count;
  if aLimit > MaxIntPairSize then ALimit := MaxIntPairSize;
  if aLimit <> Limit then
  begin
    if ALimit = 0 then AItems := nil else
    begin
      GetMem(AItems, ALimit * SizeOf(IntPair));
      if (Count <> 0) and (Items <> nil) then
        Move(Items^, aItems^, Count * SizeOf(IntPair));
    end;
    if Limit <> 0 then FreeMem(Items, Limit * SizeOf(IntPair));
    Items := aItems;
    Limit := aLimit;
  end;
end;

procedure IntPairSeq.DeleteAll;
begin
  Count := 0;
end;

procedure IntPairSeq.AssignPair(X,Y:integer);
 var lIntPair: IntPair;
begin
  lIntPair.X:=X;
  lIntPair.Y:=Y;
  Insert(lIntPair);
end;

// IntRel

constructor IntRel.Init(aLimit: Integer);
begin
  inherited Init(aLimit);
end;

procedure IntRel.Insert(const aItem: IntPair);
 var I: Integer;
begin
  if not Search(aItem.X,aItem.Y, I) then
   begin
     if (I < 0) or ( I > Count) then
      begin
         NatSetError(coIndexError,0);
         exit;
      end;
     if Count >= MaxIntPairSize then NatSetError(coOverflow,0);

     if Limit = Count then
       SetLimit(Limit+ GrowLimit(Limit));
     If I <> Count then  
         move( Items^[I], Items^[I+1],(Count - I)*sizeof(IntPair));
     Items^[I] := aItem;
     Inc(Count);
   end;
end;

procedure IntRel.AtInsert(aIndex: integer; const aItem: IntPair);
begin
  if (aIndex < 0) or (aIndex > Count) then
   NatSetError(coIndexError, aIndex);
  if Count = Limit then
    SetLimit(Limit + GrowLimit(Limit));
  if aIndex < Limit then
    move( Items^[aIndex], Items^[aIndex+1],(Count - aIndex)*sizeof(IntPair));
  Items^[aIndex] := aItem;
  Inc(Count);
end;

function IntRel.Search(X,Y: integer; var aIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Search := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareIntPairs(Items^[I].X, Items^[I].Y, X, Y);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        L := I;
      end;
    end;
  end;
  aIndex := L;
end;

constructor IntRel.CopyIntRel(var aFunc: IntRel);
begin
 Init(aFunc.Limit);
 move(aFunc.Items^,Items^,aFunc.Limit*sizeof(IntPair));
 Count:=aFunc.Count;
end;

function IntRel.IndexOf(X,Y:integer): integer;
 var I: integer;
begin IndexOf:=-1;
 if Search(X,Y, I) then IndexOf:=I;
end;

function IntRel.IsMember(X,Y:integer): boolean;
 var I: integer;
begin
 IsMember:=Search(X,Y, I);
end;

procedure IntRel.AssignPair(X,Y:integer);
begin
  if IsMember(X,Y) then exit;
  inherited AssignPair(X,Y);
end;

{ Partial Integers Functions }

constructor NatSet.Init(aLimit, aDelta: Integer);
begin
  MObject.Init;
  Items := nil;
  Count := 0;
  Limit := 0;
  Delta := ADelta;
  SetLimit(ALimit);
  Duplicates := False;
end;

constructor NatSet.InitWithElement(X:integer);
begin
 Init(0,4);
 InSertElem(X);
end;

destructor NatSet.Done;
begin
  Count := 0;
  SetLimit(0);
end;

procedure NatSet.Insert(const aItem: IntPair);
var
  I: Integer;
begin
  if not SearchPair(aItem.X, I) or Duplicates then
   begin
     if (I < 0) or ( I > Count) then
      begin
         NatSetError(coIndexError,0);
         exit;
      end;
     if Limit = Count then
      begin
       if Delta = 0 then
        begin
           NatSetError(coOverFlow,0);
           exit;
        end;
       SetLimit(Limit+Delta);
      end;
     If I <> Count then  
         move( Items^[I], Items^[I+1],(Count - I)*sizeof(IntPair));
     Items^[I] := aItem;
     Inc(Count);
   end;
end;

function Equals(Key1, Key2: IntPair): boolean;
begin
 Equals := (Key1.X = Key2.X) and (Key1.Y = Key2.Y);
end;

function NatSet.SearchPair(X: integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  SearchPair := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareInt(Items^[I].X, X);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        SearchPair := True;
        if not Duplicates then L := I;
      end;
    end;
  end;
  Index := L;
end;

constructor NatSet.CopyNatSet(const fFunc: NatSet);
begin
 Init(fFunc.Limit,fFunc.Delta);
 move(fFunc.Items^,Items^,fFunc.Limit*sizeof(IntPair));
 Count:=fFunc.Count;
end;

constructor NatSet.MoveNatSet(var fFunc: NatSet);
begin
 Init(fFunc.Limit,fFunc.Delta);
 Self:=fFunc;
 fFunc.DeleteAll;
 fFunc.Limit:=0;
 fFunc.Items:=nil;
end;

procedure NatSet.EnlargeBy(const fAnother: NatSet);
 var I: integer;
begin
 for I:=0 to fAnother.Count-1 do
  InsertElem(fAnother.Items^[i].X);
end;

procedure NatSet.ComplementOf(const fAnother: NatSet);
 var I: integer;
begin
 for I:=0 to fAnother.Count-1 do
  DeleteElem(fAnother.Items^[i].X);
end;

procedure NatSet.IntersectWith(const fAnother: NatSet);
 var k: integer;
begin
 k:=0;
 while k < Count do
  if not fAnother.HasInDom(Items^[k].X) then
   AtDelete(k)
  else inc(k);
end;

procedure NatSet.InsertElem(X:integer);
 var lIntPair: IntPair;
begin lIntPair.X:=X; lIntPair.Y:=0;
 Insert(lIntPair);
end;

procedure NatSet.DeleteElem(fElem:integer);
 var I: integer;
begin
 if SearchPair(fElem, I) then AtDelete(I);
end;

function NatSet.HasInDom(fElem:integer): boolean;
 var I: integer;
begin
 HasInDom:=SearchPair(fElem, I);
end;

function NatSet.IsEqualTo(const fFunc: NatSet): boolean;
 var I: integer;
begin IsEqualTo:=false;
 if Count <> fFunc.Count then exit;
 for I:=0 to Count-1 do if not Equals(Items^[I],fFunc.Items^[I]) then exit;
 IsEqualTo:=true;
end;

function NatSet.IsSubsetOf(const fFunc: NatSet): boolean;
 var i,j,k,c: integer;
(***begin IsSubsetOf:=false;
 if Count > fFunc.Count then exit;
 for k:=0 to Count-1 do
  if not fFunc.SearchPair(Items^[k],I) then exit
   else if Items^[k] <> fFunc.Items^[I] then exit;
 IsSubsetOf:=true;
end;***)
 { Jezeli sprawdzamy, czy mala funkcja jest zawarta w duzej, to to wykomentowane
   moze byc lepsze }
begin IsSubsetOf:=false; c:=fFunc.Count; if c < Count then exit;
 j:=0;
 for i:=0 to Count-1 do
  begin k:=Items^[i].X;
   while (j < c) and (fFunc.Items^[j].X < k) do inc(j);
   if (j = c) or not Equals(fFunc.Items^[j],Items^[i]) then exit;
  end;
 IsSubsetOf:=true;
end;

function NatSet.IsSupersetOf(const fFunc: NatSet): boolean;
begin
 IsSupersetOf:=fFunc.IsSubsetOf(Self);
end;

function NatSet.Misses(const fFunc: NatSet): boolean;
 var I,k: integer;
begin
 if Count > fFunc.Count then
  begin
   for k:=0 to fFunc.Count-1 do
    if SearchPair(fFunc.Items^[k].X,I) then begin Misses:=false; exit end
  end
 else
  begin
   for k:=0 to Count-1 do
    if fFunc.SearchPair(Items^[k].X,I) then begin Misses:=false; exit end;
  end;
 Misses:=true;
end;

function NatSet.ElemNr(X:integer): integer;
 var I: integer;
begin ElemNr:=-1;
 if SearchPair(X, I) then ElemNr:=I;
end;

constructor NatFunc.InitNatFunc(ALimit, ADelta: Integer);
begin
 inherited Init(ALimit, ADelta);
 nConsistent:=true;
end;

constructor NatFunc.CopyNatFunc(const fFunc: NatFunc);
begin
 Init(fFunc.Limit,fFunc.Delta);
 move(fFunc.Items^,Items^,fFunc.Limit*sizeof(IntPair));
 Count:=fFunc.Count;
 nConsistent:=fFunc.nConsistent;
end;

constructor NatFunc.MoveNatFunc(var fFunc: NatFunc);
begin
 Init(fFunc.Limit,fFunc.Delta);
 Self:=fFunc;
 fFunc.DeleteAll;
 fFunc.Limit:=0;
 fFunc.Items:=nil;
end;

constructor NatFunc.LCM(const aFunc1,aFunc2: NatFunc);
 var i,j,m: integer;
begin
 m:=aFunc2.Delta;
 if aFunc1.Delta > m then m:=aFunc1.Delta;
 InitNatFunc(aFunc1.Limit+aFunc2.Limit,m);
 i:=0; j:=0;
 while (i < aFunc1.Count) and (j < aFunc2.Count) do
  case CompareInt(aFunc1.Items^[i].X,aFunc2.Items^[j].X) of
  -1: begin Insert(aFunc1.Items^[i]); inc(i) end;
   0:
    begin
     m:=aFunc1.Items^[i].Y;
     if aFunc2.Items^[j].Y > m then m:=aFunc2.Items^[j].Y;
     Assign(aFunc1.Items^[i].X,m);
     inc(i); inc(j);
    end;
   1: begin Insert(aFunc2.Items^[j]); inc(j) end;
  end;
 if i >= aFunc1.Count then
  for j:=j to aFunc2.Count-1 do Insert(aFunc2.Items^[j])
 else
  for i:=i to aFunc1.Count-1 do Insert(aFunc1.Items^[i]);
end;

procedure NatFunc.Assign(X,Y:integer);
 var lIntPair: IntPair;
begin
 if nConsistent then
  begin
   if HasInDom(X) and (Value(X) <> Y) then begin Refuted; exit end;
   lIntPair.X:=X; lIntPair.Y:=Y;
   Insert(lIntPair);
  end;
end;

procedure NatFunc.Up(X: integer);
 var I: integer; lIntPair: IntPair;
begin
 if nConsistent then
  begin
   if SearchPair(X, I) then
    inc(Items^[I].Y)
   else
    begin lIntPair.X:=X; lIntPair.Y:=1;
     Insert(lIntPair);
    end;
  end;
end;

procedure NatFunc.Down(X: integer);
 var I: integer;
begin
 if nConsistent then
  begin
   if SearchPair(X, I) then
    begin
     dec(Items^[I].Y);
     if Items^[I].Y = 0 then AtDelete(I);
    end
   else NatSetError(coConsistentError,0);
  end;
end;

function NatFunc.Value(fElem: integer): integer;
 var I: integer;
begin
 if SearchPair(fElem, I) then Value:=Items^[I].Y
  else NatSetError(coDuplicate,0);
end;

destructor NatFunc.Refuted;
begin inherited Done; nConsistent:=false end;

{ Wyglada na to, ze ponizej podane procedury "Join" i "EnlargeBy"
  robia to samo, "EnlargeBy" powinna byc szybsza dla malych kolekcji.
  Jezeli tak nie jest nie warto tracic kodu i mozna ja wyrzucic.
  Z drugiej strony procedury te maja byc glownie stosowane do
  (bardzo) malych kolekcji.
}

procedure NatFunc.Join(const fFunc: NatFunc);
 var I,k: integer;
begin
 if nConsistent then
  begin
   if not fFunc.nConsistent then begin Refuted; exit end;
   for k:=0 to fFunc.Count-1 do
    if SearchPair(fFunc.Items^[k].X,I) then
     begin
      if not Equals(Items^[I],fFunc.Items^[k]) then begin Refuted; exit end;
     end
    else Insert(fFunc.Items^[k]);
  end;
end;

procedure NatFunc.EnlargeBy(fAnother:NatFuncPtr); {? virtual;?}
 var i,j,lCount,lLimit:integer; lItems:IntPairListPtr;
begin
 if nConsistent then
  begin if not fAnother^.nConsistent then begin Refuted; exit end;
   if fAnother^.Count = 0 then exit;
   lCount:=Count; lItems:=Items; lLimit:=Limit; Limit:=0;
   Count:=0; SetLimit(lCount+fAnother^.Count);
   i:=0; j:=0;
   while (i < lCount) and (j < fAnother^.Count) do
    case CompareInt(lItems^[i].X,fAnother^.Items^[j].X) of
     -1: begin Insert(lItems^[i]); inc(i) end;
      0:
      begin
       if Equals(lItems^[i],fAnother^.Items^[j]) then Insert(lItems^[i])
        else begin Refuted; FreeMem(lItems,lLimit*SizeOf(IntPair)); exit end;
       inc(i); inc(j);
      end;
      1: begin Insert(fAnother^.Items^[j]); inc(j) end;
    end;
   if i >= lCount then
    for j:=j to fAnother^.Count-1 do Insert(fAnother^.Items^[j])
   else for i:=i to lCount-1 do Insert(lItems^[i]);
   SetLimit(0); FreeMem(lItems,lLimit*SizeOf(IntPair));
  end;
end;

function NatFunc.JoinAtom(fLatAtom:NatFuncPtr): NatFuncPtr;
 var lEval: NatFunc;
begin JoinAtom:=nil;
 lEval.CopyNatFunc(Self);
 lEval.EnlargeBy(fLatAtom);
 if lEval.nConsistent then JoinAtom:=NatFuncPtr(lEval.CopyObject);
end;

function CompareNatFunc(aKey1, aKey2: Pointer): Integer;
 var i,lInt: integer;
begin
 with NatFuncPtr(aKey1)^ do
 begin
  lInt:=CompareInt(Count,NatFuncPtr(aKey2)^.Count);
  if lInt <> 0 then begin CompareNatFunc:=lInt; exit end;
  for i:=0 to Count-1 do
   begin
    lInt:=CompareInt(Items^[i].X,NatFuncPtr(aKey2)^.Items^[i].X);
    if lInt <> 0 then begin CompareNatFunc:=lInt; exit end;
    lInt:=CompareInt(Items^[i].Y,NatFuncPtr(aKey2)^.Items^[i].Y);
    if lInt <> 0 then begin CompareNatFunc:=lInt; exit end;
   end;
 end;
 CompareNatFunc:=0;
end;

function NatFunc.WeakerThan(const fNatFunc:NatFunc): boolean;
 var i,k:integer;
begin WeakerThan:=false;
 if Count <= fNatFunc.Count then
  begin
   for k:=0 to Count-1 do
    begin i:=Items^[k].X;
     if not fNatFunc.HasInDom(i) then exit;
     if Items^[k].Y <> fNatFunc.Value(i) then exit;
    end;
   WeakerThan:=true;
  end;
end;

function NatFunc.IsMultipleOf(const fNatFunc:NatFunc): boolean;
 var k,l: integer;
begin IsMultipleOf:=false;
 if fNatFunc.Count <= Count then
  begin
{
   l:=0;
   for k:=0 to fNatFunc.Count-1 do
    begin
     while (l < Count) and (Items^[l].X < fNatFunc.Items^[k].X) do inc(l);
     if fNatFunc.Items^[k].X <> Items^[l].X then exit;
     if fNatFunc.Items^[k].Y > Items^[l].Y then exit;
    end;
}

     for k:=0 to fNatFunc.Count-1 do
	if not HasInDom(fNatFunc.Items^[k].X) then exit
	else
	   if Value(fNatFunc.Items^[k].X)<fNatFunc.Items^[k].Y then exit;
     
   IsMultipleOf:=true;
  end;
end;

{Uzywajac WeakerThan mozna skrocic CompareWith !!!}

function NatFunc.CompareWith(const fNatFunc:NatFunc): integer;
 var i,k:integer;
begin CompareWith:=0;
 if Count <= fNatFunc.Count then
  begin
   for k:=0 to Count-1 do
    begin i:=Items^[k].X;
     if not fNatFunc.HasInDom(i) then exit;
     if Items^[k].Y <> fNatFunc.Value(i) then exit;
    end;
   CompareWith:=-1; exit;
  end;
 if fNatFunc.Count <= Count then
  begin
   for k:=0 to fNatFunc.Count-1 do
    begin i:=fNatFunc.Items^[k].X;
     if not HasInDom(i) then exit;
     if fNatFunc.Items^[k].Y <> Value(i) then exit;
    end;
   CompareWith:=1; exit;
  end;
end;

procedure NatFunc.Add(const aFunc:NatFunc);
 var k,l:integer;
begin
 l:=0;
 for k:=0 to aFunc.Count-1 do
 begin
  while (l < Count) and (Items^[l].X < aFunc.Items^[k].X) do inc(l);
  if (l < Count) and (Items^[l].X = aFunc.Items^[k].X) then
   inc(Items^[l].Y,aFunc.Items^[k].Y)
  else AtInsert(l,aFunc.Items^[k]);
 end;
end;

function NatFunc.CountAll: integer;
var k,l: integer;
begin
 l:= 0;
 for k:=0 to Count-1 do inc(l,Items^[k].Y);
 CountAll:=l;
end;

constructor NatSeq.InitNatSeq(ALimit, ADelta: Integer);
begin
 inherited Init(ALimit, ADelta);
 nConsistent:=true;
end;

procedure NatSeq.InsertElem(X:integer);
 var lPair: IntPair;
begin lPair.X:=Count; lPair.Y:=X;
 inherited Insert(lPair);
end;

function NatSeq.Value(fElem: integer): integer;
begin
 if {(0<=ind) and }(fElem<count) then
  Value:=Items^[fElem].Y
 else Value:=0;
end;

function NatSeq.IndexOf(Y: integer): integer;
 var lResult: integer;
begin
 for lResult :=Count-1 downto 0 do
  if Items^[lResult].Y = Y then
   begin IndexOf:=lResult; exit end;
 IndexOf:=-1;
end;

{  Integer Sequences & Sets}

procedure IntQuickSort(aList: IntegerListPtr; L, R: integer);
 var I, J,P,lTemp: integer;
begin
  repeat
    I := L;
    J := R;
    P := aList^[(L + R) shr 1];
    repeat
      while CompareInt(aList^[I], P) < 0 do Inc(I);
      while CompareInt(aList^[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        lTemp := aList^[I];
        aList^[I] := aList^[J];
        aList^[J] := lTemp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then IntQuickSort(aList, L, J);
    L := I;
  until I >= R;
end;

constructor IntSequence.Init(aCapacity: integer);
begin
  inherited Init;
  fList := nil;
  fCount := 0;
  fCapacity := 0;
  SetCapacity(aCapacity);
end;

constructor IntSequence.CopySequence(const aSeq: IntSequence);
begin
 Init(aSeq.fCapacity);
 AddSequence(aSeq);
end;

constructor IntSequence.MoveSequence(var aSeq: IntSequence);
begin
  inherited Init;
  fCount := aSeq.fCount;
  fCapacity := aSeq.fCapacity;
  fList := aSeq.fList;
  aSeq.fCount := 0;
  aSeq.fCapacity:=0;
  aSeq.fList:=nil;
end;

destructor IntSequence.Done;
begin
  inherited Done;
  fCount := 0;
  SetCapacity(0);
end;

function IntSequence.Insert(aInt: integer): integer;
begin
  if fCount = fCapacity then
    SetCapacity(fCapacity + GrowLimit(fCapacity));
  fList^[fCount]:=aInt;
  Insert:=fCount;
  Inc(fCount);
end;

procedure IntSequence.AddSequence(const aSeq: IntSequence);
 var I,r: integer;
begin
  for I := 0 to aSeq.fCount - 1 do
    r:=Insert(aSeq.fList^[I]);
end;

procedure IntSequence.Clear;
begin
  if fCount <> 0 then
  begin
    fCount := 0;
    SetCapacity(0);
  end;
end;

procedure IntSequence.AtDelete(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex >= fCount) then
   IntListError(coIndexError, aIndex);
  Dec(fCount);
  if aIndex < fCount then
    Move(fList^[aIndex+1], fList^[aIndex], (fCount-aIndex)*SizeOf(integer));
end;

procedure IntSequence.IntListError(Code, Info: Integer);
begin
  RunError(212 - Code); {! poprawic bledy}
end;

function IntSequence.Value(aIndex: integer): Integer;
begin
  if (aIndex < 0) or (aIndex >= fCount) then
   IntListError(coIndexError, aIndex);
  Value := fList^[aIndex];
end;

function IntSequence.IndexOf(aInt: integer): integer;
 var lResult: integer;
begin
 for lResult :=fCount-1 downto 0 do
  if fList^[lResult] = aInt then
   begin IndexOf:=lResult; exit end;
 IndexOf:=-1;
end;

procedure IntSequence.AtInsert(aIndex,aInt: integer);
begin
  if (aIndex < 0) or (aIndex > fCount) then
   IntListError(coIndexError, aIndex);
  if fCount = fCapacity then
    SetCapacity(fCapacity + GrowLimit(fCapacity));
  if aIndex < fCount then
    Move(fList^[aIndex], fList^[aIndex+1],(fCount-aIndex)*SizeOf(integer));
  fList^[aIndex]:=aInt;
  Inc(fCount);
end;

procedure IntSequence.AtPut(aIndex,aInt: integer);
begin
  if (aIndex < 0) or (aIndex >= fCount) then
   IntListError(coIndexError, aIndex);
  fList^[aIndex]:=aInt;
end;

procedure IntSequence.SetCapacity(aCapacity: integer);
 var lList: IntegerListPtr;
begin
  if aCapacity < fCount then aCapacity := fCount;
  if aCapacity > MaxListSize then aCapacity := MaxListSize;
  if aCapacity <> fCapacity then
  begin
    if aCapacity = 0 then lList := nil else
    begin
      GetMem(lList, aCapacity * SizeOf(integer));
      if (fCount <> 0) and (fList <> nil) then
        Move(fList^, lList^, fCount * SizeOf(integer));
    end;
    if fCapacity <> 0 then
     FreeMem(fList, fCapacity * SizeOf(integer));
    fList := lList;
    fCapacity := aCapacity;
  end;
end;

function IntSet.Insert(aInt: integer): integer;
 var lIndex: integer;
begin
  if Find(aInt, lIndex) then
   begin Insert:=lIndex; exit end;
  if fCount = fCapacity then
    SetCapacity(fCapacity + GrowLimit(fCapacity));
  if lIndex < fCount then
    Move(fList^[lIndex], fList^[lIndex+1],(fCount-lIndex)*SizeOf(integer));
  fList^[lIndex]:=aInt;
  Inc(fCount);
  Insert:=lIndex;
end;

function IntSet.DeleteInt(aInt: integer): integer;
 var lIndex: integer;
begin
 DeleteInt:=-1;
 if Find(aInt, lIndex) then
  begin DeleteInt:=lIndex; AtDelete(lIndex) end
end;

function IntSet.Find(aInt: integer; var aIndex: integer): Boolean;
 var L, H, I, C: integer;
begin
  Find := False;
  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareInt(fList^[I], aInt);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Find := True;
        L := I;
      end;
    end;
  end;
  aIndex := L;
end;

function IntSet.IndexOf(aInt: integer): integer;
 var lResult: integer;
begin
  if not Find(aInt, lResult) then lResult := -1;
  IndexOf:=lResult;
end;

procedure IntSet.AtInsert(aIndex,aInt: integer);
begin
  IntListError(coSortedListError, 0);
end;

function IntSet.IsInSet(aInt: integer): boolean;
 var I: integer;
begin
 IsInSet:=Find(aInt, I);
end;

function IntSet.IsEqualTo(const aSet: IntSet): boolean;
 var I: integer;
begin IsEqualTo:=false;
 if fCount <> aSet.fCount then exit;
 for I:=0 to fCount-1 do
  if fList^[I]<>aSet.fList^[I] then exit;
 IsEqualTo:=true;
end;

function IntSet.IsSubsetOf(const aSet: IntSet): boolean;
 var i,j,lInt: integer;
begin IsSubsetOf:=false;
 if aSet.fCount < fCount then exit;
 j:=0;
 for i:=0 to fCount-1 do
  begin lInt:=fList^[i];
   while (j < aSet.fCount) and (aSet.fList^[j] < lInt) do inc(j);
   if (j = aSet.fCount) or (aSet.fList^[j]<>fList^[i]) then exit;
  end;
 IsSubsetOf:=true;
end;

function IntSet.IsSupersetOf(var aSet: IntSet): boolean;
begin
 IsSupersetOf:=aSet.IsSubsetOf(Self);
end;

function IntSet.Misses(var aSet: IntSet): boolean;
 var k: integer;
begin
 if fCount > aSet.fCount then
  begin
   for k:=0 to aSet.fCount-1 do
    if IsInSet(aSet.fList^[k]) then begin Misses:=false; exit end
  end
 else
  begin
   for k:=0 to fCount-1 do
    if aSet.IsInSet(fList^[k]) then begin Misses:=false; exit end;
  end;
 Misses:=true;
end;

{ Partial Binary Integers Functions }

procedure BinIntFunc.BinIntFuncError(aCode, aInfo: Integer);
begin
  RunError(212 - aCode);
end;

constructor BinIntFunc.Init(aLimit: Integer);
begin
  MObject.Init;
  fList := nil;
  fCount := 0;
  fCapacity := 0;
  SetCapacity(aLimit);
end;

destructor BinIntFunc.Done;
begin
  fCount := 0;
  SetCapacity(0);
end;

procedure BinIntFunc.Insert(const aItem: IntTriplet);
 var I: Integer;
begin
  if not Search(aItem.X1,aItem.X2, I) then
   begin
     if (I < 0) or ( I > fCount) then
      begin
         BinIntFuncError(coIndexError,0);
         exit;
      end;
     if fCapacity = fCount then
       SetCapacity(fCapacity+ GrowLimit(fCapacity));
     If I <> fCount then
         move( fList^[I], fList^[I+1],(fCount - I)*sizeof(IntTriplet));
     fList^[I] := aItem;
     Inc(fCount);
   end;
end;

procedure BinIntFunc.AtDelete(aIndex: Integer);
 var i: integer;
begin
   if (aIndex < 0) or (aIndex >= fCount) then
   begin
     BinIntFuncError(coIndexError,0);
     exit;
   end;
   if aIndex < fCount-1 then
     for i:=aIndex to fCount-2 do fList^[i]:=fList^[i+1];
     {move( Items^[succ(Index)], Items^[Index], (Count-Index)*sizeof(IntTriplet));}
   Dec(fCount);
end;

procedure BinIntFunc.SetCapacity(aLimit: Integer);
 var aItems: IntTripletListPtr;
begin
  if aLimit < fCount then aLimit := fCount;
  if aLimit > MaxIntTripletSize then ALimit := MaxIntTripletSize;
  if aLimit <> fCapacity then
  begin
    if ALimit = 0 then AItems := nil else
    begin
      GetMem(AItems, ALimit * SizeOf(IntTriplet));
      if (fCount <> 0) and (fList <> nil) then
        Move(fList^, aItems^, fCount * SizeOf(IntTriplet));
    end;
    if fCapacity <> 0 then FreeMem(fList, fCapacity * SizeOf(IntTriplet));
    fList := aItems;
    fCapacity := aLimit;
  end;
end;

procedure BinIntFunc.DeleteAll;
begin
  fCount := 0;
end;

function BinIntFunc.Search(X1,X2: integer; var aIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Search := False;
  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareIntPairs(fList^[I].X1, fList^[I].X2, X1, X2);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        L := I;
      end;
    end;
  end;
  aIndex := L;
end;

constructor BinIntFunc.CopyBinIntFunc(var aFunc: BinIntFunc);
begin
 Init(aFunc.fCapacity);
 move(aFunc.fList^,fList^,aFunc.fCapacity*sizeof(IntTriplet));
 fCount:=aFunc.fCount;
end;

function BinIntFunc.IndexOf(X1,X2:integer): integer;
 var I: integer;
begin IndexOf:=-1;
 if Search(X1,X2, I) then IndexOf:=I;
end;

function BinIntFunc.HasInDom(X1,X2:integer): boolean;
 var I: integer;
begin
 HasInDom:=Search(X1,X2, I);
end;

procedure BinIntFunc.Assign(X1,X2, Y:integer);
 var lIntTriplet: IntTriplet;
begin
  if HasInDom(X1,X2) and (Value(X1,X2) <> Y) then
    begin
     BinIntFuncError(coDuplicate,0);
     exit
    end;
  lIntTriplet.X1:=X1;
  lIntTriplet.X2:=X2;
  lIntTriplet.Y:=Y;
  Insert(lIntTriplet);
end;

procedure BinIntFunc.Up(X1,X2:integer);
 var I: integer; lIntTriplet: IntTriplet;
begin
 if Search(X1,X2, I) then
  inc(fList^[I].Y)
 else
  begin lIntTriplet.X1:=X1; lIntTriplet.X2:=X2;
   lIntTriplet.Y:=1;
   Insert(lIntTriplet);
  end;
end;

procedure BinIntFunc.Down(X1,X2:integer);
 var I: integer;
begin
 if Search(X1,X2, I) then
  begin
   dec(fList^[I].Y);
   if fList^[I].Y = 0 then AtDelete(I);
  end
 else BinIntFuncError(coConsistentError,0);
end;

function BinIntFunc.Value(X1,X2: integer): integer;
 var I: integer;
begin
 if Search(X1,X2, I) then Value:=fList^[I].Y
  else BinIntFuncError(coDuplicate,0);
end;

// ##TODO: this is inefficient, since the search is repeated
//         in the Assign method; fix this both here and in
//         other similar methods
procedure BinIntFunc.Add(const aFunc: BinIntFunc);
var k,l:integer;
begin
 for k:=0 to aFunc.fCount-1 do
  if Search( aFunc.fList^[k].X1, aFunc.fList^[k].X2, l) then
   inc( fList^[l].Y, aFunc.fList^[k].Y)
  else Assign( aFunc.fList^[k].X1, aFunc.fList^[k].X2, aFunc.fList^[k].Y);
end;

function BinIntFunc.CountAll: integer;
var k,l: integer;
begin
 l:= 0;
 for k:=0 to fCount-1 do inc( l, fList^[k].Y);
 CountAll:= l;
end;

{ Partial Integers to Pair of Integers Functions }

procedure Int2PairOfIntFunc.Int2PairOfIntFuncError(aCode, aInfo: Integer);
begin
  RunError(212 - aCode);
end;

constructor Int2PairOfIntFunc.Init(aLimit: Integer);
begin
  MObject.Init;
  fList := nil;
  fCount := 0;
  fCapacity := 0;
  SetCapacity(aLimit);
end;

destructor Int2PairOfIntFunc.Done;
begin
  fCount := 0;
  SetCapacity(0);
end;

procedure Int2PairOfIntFunc.Insert(const aItem: Int2PairOfInt);
 var I: Integer;
begin
  if not Search(aItem.X, I) then
   begin
     if (I < 0) or ( I > fCount) then
      begin
         Int2PairOfIntFuncError(coIndexError,0);
         exit;
      end;
     if fCapacity = fCount then
       SetCapacity(fCapacity+ GrowLimit(fCapacity));
     If I <> fCount then
         move( fList[I], fList[I+1],(fCount - I)*sizeof(Int2PairOfInt));
     fList[I] := aItem;
     Inc(fCount);
   end
 else if (fList[I].Y1 <> aItem.Y1) or (fList[I].Y2 <> aItem.Y2) then
  begin
   Int2PairOfIntFuncError(coDuplicate,0);
   exit;
  end;

end;

procedure Int2PairOfIntFunc.AtDelete(aIndex: Integer);
 var i: integer;
begin
   if (aIndex < 0) or (aIndex >= fCount) then
   begin
     Int2PairOfIntFuncError(coIndexError,0);
     exit;
   end;
   if aIndex < fCount-1 then
     for i:=aIndex to fCount-2 do fList[i]:=fList[i+1];
     {move( Items^[succ(Index)], Items^[Index], (Count-Index)*sizeof(IntTriplet));}
   dec(fCount);
end;

procedure Int2PairOfIntFunc.SetCapacity(aLimit: Integer);
begin
  if aLimit < fCount then aLimit := fCount;
  setlength(fList,aLimit);
  fCapacity := aLimit;
end;

procedure Int2PairOfIntFunc.DeleteAll;
begin
  fCount := 0;
end;

function Int2PairOfIntFunc.Search(X: integer; var aIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Search := False;
  L := 0;
  H := fCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareInt(fList[I].X, X);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        L := I;
      end;
    end;
  end;
  aIndex := L;
end;

constructor Int2PairOfIntFunc.CopyInt2PairOfIntFunc(var aFunc: Int2PairOfIntFunc);
begin
 Init(aFunc.fCapacity);
 move(aFunc.fList[0],fList[0],aFunc.fCapacity*sizeof(Int2PairOfInt));
 fCount:=aFunc.fCount;
end;

function Int2PairOfIntFunc.IndexOf(X:integer): integer;
 var I: integer;
begin IndexOf:=-1;
 if Search(X, I) then IndexOf:=I;
end;

function Int2PairOfIntFunc.HasInDom(X:integer): boolean;
 var I: integer;
begin
 HasInDom:=Search(X, I);
end;

procedure Int2PairOfIntFunc.Assign(X, Y1,Y2:integer);
 var lInt2PairOfInt: Int2PairOfInt;
begin
  lInt2PairOfInt.X:=X;
  lInt2PairOfInt.Y1:=Y1;
  lInt2PairOfInt.Y2:=Y2;
  Insert(lInt2PairOfInt);
end;

function Int2PairOfIntFunc.Value(X: integer): IntPair;
 var I: integer;
begin
 if Search(X, I) then
  begin
   result.X:=fList[I].Y1;
   result.Y:=fList[I].Y2;
  end
 else Int2PairOfIntFuncError(coDuplicate,0);
end;

// ##TODO: this is inefficient, since the search is repeated
//         in the Assign method; fix this both here and in
//         other similar methods
begin
 EmptyNatFunc.Init(0,0);
end.
