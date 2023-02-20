(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit pragmas;

interface

uses mobjects;

var
   VerifyPragmaOn,VerifyPragmaOff : NatSet;
   VerifyPragmaIntervals: NatFunc;
   SchemePragmaOn,SchemePragmaOff: NatSet;
   SchemePragmaIntervals : NatFunc;
   ProofPragma: Boolean = true;

procedure SetParserPragma(aPrg: string);
procedure InsertPragma(aLine: integer; aPrg: string);
procedure CompletePragmas(aLine: integer);

procedure CanceledPragma(const aPrg:string; var aKind: char; var aNbr: integer);

implementation

uses mizenv;

procedure CanceledPragma(const aPrg:string; var aKind: char; var aNbr: integer);
 var lStr: string;
     k,lCod: integer;
begin
 aKind:=' ';
 if (Copy(aPrg,1,2) = '$C') then
  begin
   if (length(aPrg) >= 3) and (aPrg[3] in ['D','S','T']) then
    begin
     aKind:=aPrg[3];
     lStr:=TrimString(Copy(aPrg,4,length(aPrg)-3));
     aNbr:=1;
     if length(lStr) > 0 then
      begin
       k:=1;
       while (k <= length(lStr)) and (lStr[k] in ['0'..'9']) do inc(k);
       delete(lStr,k,length(lStr));
       if length(lStr) > 0 then
        Val(lStr,aNbr,lCod);
      end;
    end;
  end;
end;

procedure SetParserPragma(aPrg: string);
begin
   if copy(aPrg,1,3)='$P+' then
   begin
      ProofPragma:=true;
   end;
   if copy(aPrg,1,3)='$P-' then
   begin
      ProofPragma:=false;
   end;
end;

procedure InsertPragma(aLine: integer; aPrg: string);
begin
   if copy(aPrg,1,3)='$V+' then
   begin
      VerifyPragmaOn.InsertElem(aLine);
   end;
   if copy(aPrg,1,3)='$V-' then
   begin
      VerifyPragmaOff.InsertElem(aLine);
   end;

   if copy(aPrg,1,3)='$S+' then
   begin
      SchemePragmaOn.InsertElem(aLine);
   end;
   if copy(aPrg,1,3)='$S-' then
   begin
      SchemePragmaOff.InsertElem(aLine);
   end;
end;

procedure CompletePragmas(aLine: integer);
var i,j,a,b : integer; f:boolean;
begin
  for i:=0 to VerifyPragmaOff.Count-1 do
   begin
     f:=false;
     a:=VerifyPragmaOff.Items^[i].X;
     for j:=0 to VerifyPragmaOn.Count-1 do
      begin
       b:=VerifyPragmaOn.Items^[j].X;
       if b >= a then
        begin
         VerifyPragmaIntervals.Assign(a,b);
         f:=true;
         break;
        end;
      end;
     if not f then VerifyPragmaIntervals.Assign(a,aLine);
   end;
  for i:=0 to SchemePragmaOff.Count-1 do
   begin
     f:=false;
     a:=SchemePragmaOff.Items^[i].X;
     for j:=0 to SchemePragmaOn.Count-1 do
      begin
       b:=SchemePragmaOn.Items^[j].X;
       if b >= a then
        begin
         SchemePragmaIntervals.Assign(a,b);
         f:=true;
         break;
        end;
      end;
     if not f then SchemePragmaIntervals.Assign(a,aLine);
   end;
end;

begin

   VerifyPragmaOn.Init(10,10);
   VerifyPragmaOff.Init(10,10);
   VerifyPragmaIntervals.InitNatFunc(10,10);
   SchemePragmaOn.Init(10,10);
   SchemePragmaOff.Init(10,10);
   SchemePragmaIntervals.InitNatFunc(10,10);

end.
