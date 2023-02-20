(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit heapctrl;

interface

 function BlockSize(Size:pointer):longint;
 procedure InfoHeap;
 procedure HeapControl(fErrNr:integer);

implementation

 uses info;

type
 PFreeRec = ^TFreeRec;
 TFreeRec =
  record Next: PFreeRec; Size: Pointer; end;
function BlockSize(Size:pointer):longint;
 type PtrRec = record Lo, Hi: Word end;
begin BlockSize:=Longint(PtrRec(Size).Hi)*16 + PtrRec(Size).Lo end;

procedure InfoHeap;
 var lPtr: PFreeRec;
     k:integer;
begin lPtr:=FreeList; k:=0;
 writeln(InfoFile,'FreeList=',BlockSize(FreeList));
 if FreeList <> HeapPtr then
  while lPtr <> HeapPtr do
   begin inc(k); write('.');
    writeln(InfoFile,BlockSize(lPtr),
                '..',BlockSize(lPtr)+BlockSize(lPtr^.Size),
                ' (',BlockSize(lPtr^.Size),')');

    lPtr:=lPtr^.Next;
    if k > 1000 then
     begin writeln(InfoFile,'Przerwane'); RunError(3000) end;
   end;
 writeln(InfoFile,'HeapPtr=',BlockSize(HeapPtr));
{ writeln(InfoFile,'MaxAvail=',MaxAvail);}
end;

 procedure HeapControl(fErrNr:integer);
 var lPtr: PFreeRec; lPrev: longint;
begin lPtr:=FreeList; lPrev:=0;
 if FreeList <> HeapPtr then
  begin  lPtr:=PFreeRec(FreeList)^.Next;
   while lPtr <> HeapPtr do
    begin
     if lPrev >= BlockSize(lPtr) then
      begin writeln(InfoFile,'*** ',fErrNr,' ***');
       RunError(fErrNr);
      end;
     lPrev:=BlockSize(lPtr)+BlockSize(lPtr^.Size);
     if lPrev > BlockSize(HeapPtr) then
      begin writeln(InfoFile,'**** ',fErrNr,' ****');
       RunError(fErrNr);
      end;
     lPtr:=lPtr^.Next;
    end;
  end;
end;

end.
