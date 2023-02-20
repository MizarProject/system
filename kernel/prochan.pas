(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit prochan;

interface

type
 DoInElem = procedure (fElem: pointer);
 PtrList = ^PtrElem;
 PtrElem = record NextPtr: PtrList; ElemPtr: pointer end;
 EqElem = function (fArg1,fArg2:pointer) : boolean;

procedure ProcessPtrList(fList:PtrList; P:DoInElem);
function EqPtrLists(fList1,fList2:PtrList; Eq:EqElem) : boolean;
function EqPtrElems(fList1,fList2:PtrList; Eq:EqElem) : boolean;

implementation
 uses errhan,limits;

procedure ProcessPtrList(fList:PtrList; P:DoInElem);
 var i:integer;
begin
 for i:=1 to MaxElemNbr do
  if fList=nil then exit else
   with fList^ do begin P(ElemPtr); fList:=NextPtr end;
 RunTimeError(2018);
end;

                     function EqPtrLists
   (fList1,fList2:PtrList; Eq:EqElem) : boolean;
begin EqPtrLists:=false;
 while (fList1<>nil) and (fList2<>nil) do
  begin if not Eq(fList1^.ElemPtr,fList2^.ElemPtr) then exit;
   fList1:=fList1^.NextPtr; fList2:=fList2^.NextPtr;
  end;
 EqPtrLists:=fList1=fList2;
end;

                     function EqPtrElems
   (fList1,fList2:PtrList; Eq:EqElem) : boolean;
begin EqPtrElems:=false;
 while fList1<>nil do
  begin mizassert(2505,fList2<>nil);
   if not Eq(fList1^.ElemPtr,fList2^.ElemPtr) then exit;
   fList1:=fList1^.NextPtr; fList2:=fList2^.NextPtr;
  end;
 mizassert(2506,fList2=nil); EqPtrElems:=true;
end;


end.
