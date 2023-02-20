(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit schemhan;

interface

uses errhan,correl,mobjects
{$IFDEF MDEBUG}
 ,info
{$ENDIF};

type
 SchemeStateKind =
  (
    schStateCanceled,
    schStateMissingConstrs,// used to be PremNbr = -3
    schStateBeingProved,// used to be PremNbr = -2
    schStateErrorOccurred,// used to be PremNbr = -1
    schStateUnquotable,// used to be PremNbr = -1
    schStateQuotable// only this kind can be used
  );

 SchemePtr =  ^ SchemeObj;
 SchemeObj = object(MObject)
   fSchNr: word;
   fSchTypes,fSchProps: MCollection;
  constructor Init(aNr: word);//(const aName: string);
  destructor Done; virtual;
 end;

 SchRefPtr = ^SchRefItem;
 SchRefItem = object(IntPairItem)
  SchTypes,
  SchProps: MCollection;
  nSchState: SchemeStateKind;
  constructor Init(aMMLNr,aNr: integer;
                   var aSchTypes,aSchProps: MCollection;
                   fSchState: SchemeStateKind); 
  constructor Init1; 
 end;

var
 SchReferNbr: BinIntFunc;
 Ord_Scheme: IntPairKeyCollection;

function Scheme(FileNr,SchNr: integer): SchRefPtr;

implementation

constructor SchemeObj.Init(aNr: word);
begin
 fSchNr:=aNr;
 fSchTypes.Init(4,4);
 fSchProps.Init (4,4);
end;

destructor SchemeObj.Done;
begin
 fSchTypes.Done;
 fSchProps.Done;
 inherited Done;
end;

function Scheme(FileNr,SchNr: integer): SchRefPtr;
begin
 Result:=SchRefPtr(Ord_Scheme.ObjectOf(FileNr,SchNr));
end;

constructor SchRefItem.Init(aMMLNr,aNr: integer;
                            var aSchTypes,aSchProps: MCollection;
                            fSchState: SchemeStateKind);
begin
 fKey.X:= aMMLNr;
 fKey.Y:= aNr;
 SchTypes.MoveCollection(aSchTypes);
 SchProps.MoveCollection(aSchProps);
 nSchState:= fSchState;
end;

constructor SchRefItem.Init1;
begin
 fKey.X:= 0;
 fKey.Y:= 0;
 SchTypes.Init(0,4);
 SchProps.Init(0,4);
 nSchState:= schStateErrorOccurred;
end;

end.
