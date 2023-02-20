(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit express;

interface

uses errhan,mobjects,inout;

type
  ExpPtr = ^ Expression;
  Expression =
   object(MObject)
     ExpPos: Position;
     ExpSort: char;
    constructor InitExp(aSort:char);
    function Analyze: pointer; virtual;
    procedure ExpError(fNr:integer);
   end;

implementation

constructor Expression.InitExp(aSort:char);
begin ExpPos:=CurPos;
 ExpSort:=aSort;
end;

function Expression.Analyze: pointer;
begin Abstract1; Analyze:= nil; end;

procedure Expression.ExpError(fNr:integer);
begin Error(ExpPos,fNr) end;

end.
