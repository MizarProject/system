(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit ref_han;

interface
uses mobjects,envhan;

type
  LibrRefKind = (lrWrong,
                 lrTh, lrDef, lrSch, lrExreg, lrFuncreg, lrCondreg,
                 lrAll);
  PLibrReference = ^LibrReference;
  LibrReference = object(MStrObj)
     Kind: LibrRefKInd;
     Nr: integer;
     constructor Init(fIdent:string; fDef: LibrRefKind; fNr:integer);
     function Repr: string; virtual;
   end;

  PLibrRefCollection = ^LibrRefCollection;
  LibrRefCollection = object(MSortedCollection)
     function Compare(Key1, Key2: Pointer): Integer; virtual;
    end;

  PReplLibrRef = ^ReplLibrRef;
  ReplLibrRef = object(LibrReference)
     Repl: LibrReference;
     constructor Init(fIdent1:string; fDef1: LibrRefKInd; fNr1:integer;
                      fIdent2:string; fDef2: LibrRefKInd; fNr2:integer);
   end;

  PCommLibrRef = ^CommLibrRef;
  CommLibrRef = object(LibrReference)
     Comm: PString;
     constructor Init(fIdent:string; fDef: LibrRefKInd; fNr:integer;
                      fComm:string);
     destructor Done; virtual;
   end;

  PCntLibrRef = ^CntLibrRef;
  CntLibrRef = object(LibrReference)
     Cnt: longint;
     constructor Init(fIdent:string; fDef: LibrRefKInd; fNr:integer;
                      fCnt:longint);
     procedure Increment(fCnt: integer);
   end;

function TheoremNr(var fTheo:LibrReference): string;
procedure SplitLibrReference(fLine:string;
            var fIdent:string; var fDef:LibrRefKind; var fNr: integer);

implementation

uses mizenv;

constructor LibrReference.Init;
begin fStr:=fIdent; Kind:=fDef; Nr:=fNr end;

function LibrReference.Repr;
 var lRepr,lNrRepr: string;
begin
 lRepr:=fStr+':';
 case Kind of
  lrth: {lRepr:=lRepr+'th '};
  lrdef: lRepr:=lRepr+'def ';
  lrsch: lRepr:=lRepr+'sch ';
  lrexreg: lRepr:=lRepr+'exreg ';
  lrfuncreg: lRepr:=lRepr+'funcreg ';
  lrcondreg: lRepr:=lRepr+'condreg ';
 end;
 str(Nr,lNrRepr);
 Repr:=lRepr+lNrRepr;
end;

function CompNums(fN1,fN2:PLibrReference):integer;
begin
 if fN1^.fStr = fN2^.fStr then
  begin
   if integer(fN1^.Kind) < integer(fN2^.Kind) then CompNums:=-1
   else if integer(fN1^.KInd) > integer(fN2^.Kind) then CompNums:=1
   else if fN1^.Nr < fN2^.Nr then CompNums:=-1
   else if fN1^.Nr > fN2^.Nr then CompNums:=1
   else CompNums:=0
  end
 else if fN1^.fStr < fN2^.fStr then CompNums:=-1
 else CompNums:=1
end;

function LibrRefCollection.Compare;
begin
 Compare:= CompNums(PLibrReference(Key1),PLibrReference(Key2));
end;

constructor ReplLibrRef.Init;
begin fStr:=fIdent1; Kind:=fDef1; Nr:=fNr1;
 Repl.Init(fIdent2,fDef2,fNr2);
end;

constructor CommLibrRef.Init;
begin fStr:=fIdent; Kind:=fDef; Nr:=fNr;
 Comm:=nil;
 if fComm <> '' then Comm:=NewStr(fComm);
end;

destructor CommLibrRef.Done;
begin
 if Comm <> nil then DisposeStr(Comm);
end;

constructor CntLibrRef.Init;
begin fStr:=fIdent; Kind:=fDef; Nr:=fNr;
 Cnt:=fCnt;
end;

procedure CntLibrRef.Increment;
begin
 inc(Cnt,fCnt);
end;

function TheoremNr(var fTheo:LibrReference): string;
 var s: string;
begin
  Str(fTheo.Nr, s);
  case fTheo.Kind of
   lrth: {s:='th '+s};
   lrdef: s:='def '+s;
   lrsch: s:='sch '+s;
   lrexreg: s:='exreg '+s;
   lrfuncreg: s:='funcreg '+s;
   lrcondreg: s:='condreg '+s;
  end;
  TheoremNr:=s;
end;

procedure SplitLibrReference(fLine:string;
            var fIdent:string; var fDef:LibrRefKind; var fNr: integer);
  var l,n,c: integer;
begin
  l:=pos(':',fLine);
  if l = 0 then
   begin
    fIdent:=''; fDef:=lrTh; fNr:=0;
    exit;
   end;
  fIdent:=TrimString(copy(fLine,1,l-1));
  UpperCase(fIdent);
  fDef:=lrTh; inc(l);
  while fLine[l] = ' ' do inc(l);
  if fLine[l] = 'd' then
   begin fDef:=lrDef; inc(l,4);
    while fLine[l] = ' ' do inc(l);
   end;
  val(TrimString(copy(fLine,l,length(fLine))),fNr,c);
end;

end.
