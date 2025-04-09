(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit numbers;

// This unit provides basic arithmetic operations on arbitrarily long
// signed integers (represented as strings with or without a leading '-')
// and rational complex numbers.
// *Note1: All _XXX functions presuppose that arguments are positive.
// *Note2: There''s no check whether the parameters contain only digits and '-'.
// *Note3: DEBUGNUM conditional variable can be used (with DEBUG) for testing.
   
interface

function Add(a,b: string): string;
function Sub(a,b: string): string;
function Mul(a,b: string): string;
function Diva(a,b: string): string; // *Note: divides absolute values and preserves the sign of the division
function _Div(a,b: string): string;
function _Mod(a,b: string): string;
function GCD(a,b: string): string; // *Note: always returns a positive value
function LCM(a,b: string): string; // *Note: always returns a positive value
function Abs(a :string ): string;
function IsPrime(a: string): boolean;
function Divides(a,b: string): boolean;

type
   Rational = record Num,Den : string end;
   RComplex = record  Re,Im: Rational end;

const
   RZero:Rational = (Num:'0'; Den:'1');
   ROne:Rational  = (Num:'1'; Den:'1');
   CZero: RComplex     = (Re:(Num:'0'; Den:'1'); Im:(Num:'0'; Den:'1'));
   COne: RComplex      = (Re:(Num:'1'; Den:'1'); Im:(Num:'0'; Den:'1'));
   CMinusOne: RComplex = (Re:(Num:'-1'; Den:'1'); Im:(Num:'0'; Den:'1'));
   CImUnit: RComplex   = (Re:(Num:'0'; Den:'1'); Im:(Num:'1'; Den:'1'));

procedure RationalReduce(var r: Rational);
function RationalAdd(const r1,r2: Rational): Rational;
function RationalSub(const r1,r2: Rational): Rational;
function RationalNeg(const r1: Rational): Rational;
function RationalMult(const r1,r2: Rational): Rational;
function RationalInv(const r: Rational): Rational;
function RationalDiv(const r1,r2: Rational): Rational;
function RationalEq(const r1,r2: Rational): boolean;
function RationalLE(const r1,r2: Rational): boolean;
function RationalGT(const r1,r2: Rational): boolean;

function IsIntegerNumber(const z: RComplex): boolean;
function IsNaturalNumber(const z: RComplex): boolean;
function IsPrimeNumber(const z: RComplex): boolean;

function AreEqComplex(const z1,z2: RComplex): boolean;
function IsEqWithInt(const z: RComplex; n: longint): boolean;
function IsRationalLE(const z1,z2 : RComplex): boolean;
function IsRationalGT(const z1,z2 : RComplex): boolean;

function IntToComplex(x: integer): RComplex;
function ComplexAdd (const z1,z2: RComplex): RComplex;
function ComplexSub(const z1,z2: RComplex): RComplex;
function ComplexNeg (const z: RComplex): RComplex;
function ComplexMult(const z1,z2: RComplex): RComplex;
function ComplexInv(const z: RComplex): RComplex;
function ComplexDiv(const z1,z2: RComplex): RComplex;
function ComplexNorm(const z: RComplex): Rational;

function CompareInt(X1,X2: Longint): Integer;
function CompareIntStr(X1,X2: String): Integer;
function CompareComplex(const z1,z2: RComplex): Integer;

implementation

uses mizenv
{$IFDEF CH_REPORT},req_info,prephan,builtin{$ENDIF}      
{$IFDEF MDEBUG},info{$ENDIF};

function trimlz(a :string ):string;
var i : integer;
begin
   if (a='0') or (a='') then trimlz:=a else
   begin
   i:=0;
   repeat
   i:=i+1;
   if a[i]<>'0' then break;
   until i=length(a);
   trimlz:=copy(a,i,length(a));
   end;
end;

procedure checkzero(var a,b :string);
var a1,b1 : string;
begin
   if copy(a,1,2)='-0' then
   begin
      {$IFDEF DEBUGNUM}
      writeln(infofile,'a=-0');
      {$ENDIF}
      a:='0';
   end;
   if copy(b,1,2)='-0' then
   begin
      {$IFDEF DEBUGNUM}
      writeln(infofile,'b=-0');
      {$ENDIF}
      b:='0';
   end;
   a1:=trimlz(a); if a1<>a then
   begin
      {$IFDEF DEBUGNUM}
      writeln(infofile,'ZEROS1:',a);
      {$ENDIF}
      a:=a1;
   end;
   b1:=trimlz(b); if b1<>b then
   begin
      {$IFDEF DEBUGNUM}
      writeln(infofile,'ZEROS2:',b);
      {$ENDIF}
      b:=b1;
   end;
end;

function Abs(a :string ):string;
begin
   if length(a)>0 then if a[1]='-' then delete(a,1,1);
   Abs:=a;
end;

function _leq(a,b :string):boolean;
var i,x,y,z:integer;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_leq(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if length(a)<length(b) then _leq:=true
   else if length(a)>length(b) then _leq:=false
   else begin
      for i:=1 to length(a) do
      begin val(a[i],x,z);val(b[i],y,z);
	 if x>y then begin _leq:=false; exit; end;
	 if x<y then begin _leq:=true; exit; end;
      end;
      _leq:=true;
   end;
end;

function leq(a,b :string):boolean;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'leq(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if a=b then leq:=true else
      begin
	 if (a[1]='-') and (b[1]<>'-') then leq:=true;
	 if (a[1]='-') and (b[1]='-') then leq:=not _leq(abs(a),abs(b));
	 if (a[1]<>'-') and (b[1]='-') then leq:=false;
	 if (a[1]<>'-') and (b[1]<>'-') then leq:=_leq(a,b);
      end;
end;

function geq(a,b :string):boolean;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'geq(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   geq:=(not leq(a,b)) or (a=b);
end;

function le(a,b :string):boolean;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'le(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   le:=(a<>b) and (leq(a,b));
end;

function gt(a,b :string):boolean;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'gt(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   gt:= not leq(a,b);
end;

function _Add(a,b :string):string;
var c,x,y,z,v : integer;i:integer; a1,b1,s,r : string;
begin
   a1:=a; b1:=b;
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_Add(',a1,',',b1,')');
   {$ENDIF}
   checkzero(a1,b1);
   if length(a1)<length(b1) then begin s:=b1; b1:=a1; a1:=s; end;
   r:='';
   c:=0;
   begin
      for i:=0 to length(b1)-1 do
      begin
	 val(a1[length(a1)-i],x,z);
	 val(b1[length(b1)-i],y,z);
	 if x+y+c>9 then begin v:=(x+y+c)-10; c:=1; end else begin v:=x+y+c; c:=0; end;
	 str(v,s);
	 r:=s+r;
      end;
      for i:=length(b1) to length(a1)-1 do
      begin
	 val(a1[length(a1)-i],x,z);
	 if x+c>9 then begin v:=(x+c)-10; c:=1; end else begin v:=x+c; c:=0; end;
	 str(v,s);
	 r:=s+r;
      end;
      if c=1 then r:='1'+r;
   end;
   _add:=trimlz(r);
end;

function _Sub(a,b :string):string;
var x,y,z,v : integer;i:integer; a1,b1,s,r : string;

   procedure Borrow(k :integer);
   var xx,zz : integer;sx:string;
   begin
      val(a1[k-1],xx,zz);
      if xx>=1 then begin xx:=xx-1; str(xx,sx); a1[k-1]:=sx[1]; end
      else begin a1[k-1]:='9'; borrow(k-1); end;
   end;

begin
   a1:=a; b1:=b;
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_Sub(',a1,',',b1,')');
   {$ENDIF}
   checkzero(a1,b1);
   if not _leq(b1,a1) then begin s:=b1; b1:=a1; a1:=s; end;
   r:='';
   begin
      for i:=0 to length(b1)-1 do
      begin
	 val(a1[length(a1)-i],x,z);
	 val(b1[length(b1)-i],y,z);
	 if x<y then
	    begin
	       borrow(length(a1)-i);
	       x:=x+10;
	    end;
	 v:=x-y;
	 str(v,s);
	 r:=s+r;
      end;
      for i:=length(a1)-length(b1) downto 1 do
      begin
	 r:=a1[i]+r;
      end;
   end;
   _sub:=trimlz(r);
end;

function _Mul1(a:string ;y :integer ):string;
var c,x,z,v : integer;i:integer; s,r : string;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_Mul1(',a,',',y,')');
   {$ENDIF}
   r:='';
   c:=0;
   begin
      for i:=0 to length(a)-1 do
      begin
	 val(a[length(a)-i],x,z);
	 if x*y+c>9 then begin v:=(x*y+c) mod 10; c:=(x*y+c) div 10; end else begin v:=x*y+c; c:=0; end;
	 str(v,s);
	 r:=s+r;
      end;
      if c<>0 then
      begin
	 str(c,s);
	 r:=s+r;
      end;
   end;
   _mul1:=trimlz(r);
end;

function _Mul(a,b :string):string;
var y,z : integer;i,j:integer; a1,b1,s,r : string;
begin
   a1:=a;b1:=b;
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_Mul(',a1,',',b1,')');
   {$ENDIF}
   checkzero(a1,b1);
   if length(a1)<length(b1) then begin s:=b1; b1:=a1; a1:=s; end;
   r:='0';
   for i:=0 to length(b1)-1 do
   begin
      val(b1[length(b1)-i],y,z);
      s:=_mul1(a1,y);
      for j:=0 to i-1 do s:=s+'0';
      r:=_add(r,s);
   end;
   _mul:=trimlz(r);
end;

function _Div1(a,b : string):string;
var i:integer; r : string;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_Div1(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if not _leq(b,a) then _div1:='0'
   else
      for i:=9 downto 1 do
      begin
	 str(i,r);
	 if _leq(_mul(b,r),a) then
	 begin
	    _div1:=trimlz(r);
	    exit;
	 end;
      end;
end;

function _Div(a,b : string):string;
var z,c,i:integer; s,r,rs : string;b_GPC:boolean;

   procedure gets;
   var j : integer;
   begin
      c:=1;
      s:=_sub(s,_mul(rs,b));
      if (s='0') and (trimlz(copy(a,z+c,length(a)))='0') then
      begin
	 {$IFDEF DEBUGNUM}
	 writeln(infofile,'Rewriting zeros:',copy(a,z+c,length(a)));
	 {$ENDIF}
	 r:=r+copy(a,z+c,length(a)); exit;
      end;
      if z+1<=length(a) then begin s:=s+a[z+1]; inc(c); if (not _leq(b,s)) then r:=r+'0'; end;
      while (not _leq(b,s)) and (z+c<=length(a)) do
      begin
	 s:=s+a[z+c];
	 inc(c);
	 if (not _leq(b,s)) then r:=r+'0';
      end;
      z:=z+c-1;
   end; { gets }

begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_Div(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if a=b then _div:='1' else
   if not _leq(b,a) then _div:='0' else
   begin
      s:='';r:='';z:=1;
      for i:=1 to length(b) do s:=s+a[i];
      if not _leq(b,s) then
      begin
	 s:=s+a[length(b)+1];
	 z:=length(b)+1;
      end
      else
      begin
	 z:=length(b);
      end;
      repeat
         rs:=_div1(s,b);
         r:=r+rs;
	 gets;
	 b_GPC:= _leq(b,s);
      until not b_GPC;
      _div:=trimlz(r);
   end;
end;

function _Mod(a,b : string):string;
var r : string;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'_Mod(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if le(a,b) then r:=a
   else r:=_sub(a,_mul(b,_div(a,b)));
   _Mod:=trimlz(r);
   {$IFDEF DEBUGNUM}
    writeln(infofile,'End _Mod:',r);
   {$ENDIF}
end;

function GCD(a,b:string):string;
label ex;var a1,b1,p,r:string;
 begin
   a1:=a; b1:=b;
   {$IFDEF DEBUGNUM}
    writeln(infofile,'GCD(',a1,',',b1,')');
   {$ENDIF}
    checkzero(a1,b1);
    a1:=abs(a1);b1:=abs(b1);
    if (a1='1') or (b1='1') then begin r:='1'; goto ex; end;
    if (a1='0') and (b1<>'0') then begin r:=b1; goto ex; end;
    if (b1='0') and (a1<>'0') then begin r:=a1; goto ex; end;
    if a1=b1 then begin GCD:=a1; r:=a1; goto ex; end;
    while gt(b1,'0') do begin p:=b1; b1:= _mod(a1,b1); a1:=p end;
    r:=a1;
    ex:
    GCD:=r;
   {$IFDEF DEBUGNUM}
    writeln(infofile,'End GCD:',r);
   {$ENDIF}
end;

function LCM(a,b:string):string;
var a1,b1,r:string;
 begin
   a1:=a; b1:=b;
   {$IFDEF DEBUGNUM}
    writeln(infofile,'LCM(',a1,',',b1,')');
   {$ENDIF}
    checkzero(a1,b1);
    a1:=abs(a1);b1:=abs(b1);
    r:=Diva(Mul(a1,b1),GCD(a1,b1));
    LCM:=r;
   {$IFDEF DEBUGNUM}
    writeln(infofile,'End LCM:',r);
   {$ENDIF}
end;

function Add(a,b :string ):string;
label ex; var r : string;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'Add(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if (a[1]='-') and (b[1]='-') then begin r:='-'+_Add(abs(a),abs(b)); if r='-0' then r:='0'; goto ex; end;
   if (a[1]<>'-') and (b[1]<>'-') then begin r:=_Add(a,b); goto ex; end;
   if (a[1]='-') and (b[1]<>'-') then
      if gt(abs(a),b) then begin r:='-'+_Sub(abs(a),b); if r='-0' then r:='0'; goto ex; end
      else begin r:=_Sub(abs(a),b); goto ex; end;
   if (a[1]<>'-') and (b[1]='-') then
      if gt(abs(b),a) then begin r:='-'+_Sub(abs(b),a); if r='-0' then r:='0'; goto ex; end
      else begin r:=_Sub(abs(b),a); goto ex; end;
   ex:
   Add:=r;
   {$IFDEF DEBUGNUM}
   writeln(infofile,'End Add:',r);
   {$ENDIF}
end;

function Sub(a,b :string ):string;
label ex; var r : string;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'Sub(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if (a[1]='-') and (b[1]<>'-') then begin r:='-'+_Add(abs(a),b); if r='-0' then r:='0'; goto ex; end;
   if (a[1]<>'-') and (b[1]='-') then begin r:=_Add(a,abs(b)); goto ex; end;
   if (a[1]='-') and (b[1]='-') then
      if gt(abs(a),abs(b)) then begin r:='-'+_Sub(abs(a),abs(b)); if r='-0' then r:='0'; goto ex; end
      else begin r:=_Sub(abs(a),abs(b)); goto ex; end;
   if (a[1]<>'-') and (b[1]<>'-') then
      if gt(b,a) then begin r:='-'+_Sub(b,a); if r='-0' then r:='0'; goto ex; end
      else begin r:=_Sub(a,b); goto ex; end;
   ex:
   Sub:=r;
   {$IFDEF DEBUGNUM}
   writeln(infofile,'End Sub:',r);
   {$ENDIF}
end;

function Mul(a,b :string ):string;
label ex; var r : string;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'Mul(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if ((a[1]='-') and (b[1]<>'-')) or ((a[1]<>'-') and (b[1]='-'))
      then
      begin
	 r:='-'+_Mul(abs(a),abs(b));
	 if r='-0' then r:='0';
      end
   else r:=_Mul(abs(a),abs(b));
   ex:
   Mul:=r;
   {$IFDEF DEBUGNUM}
   writeln(infofile,'End Mul:',r);
   {$ENDIF}
end;

function DivA(a,b :string ):string;
label ex; var r : string;
begin
   {$IFDEF DEBUGNUM}
   writeln(infofile,'DivA(',a,',',b,')');
   {$ENDIF}
   checkzero(a,b);
   if ((a[1]='-') and (b[1]<>'-')) or ((a[1]<>'-') and (b[1]='-'))
      then
      begin
	 r:='-'+_Div(abs(a),abs(b));
	 if r='-0' then r:='0';
      end
   else r:=_Div(abs(a),abs(b));
   ex:
   DivA:=r;
   {$IFDEF DEBUGNUM}
   writeln(infofile,'End DivA:',r);
   {$ENDIF}
end;

function IsPrime(a: string): boolean;
var i: string;
r: boolean;
begin
    if leq('2',a) then 
	begin
	    r:=true;
	    i:='2';
	    while leq(Mul(i,i),a) do
	    begin
		if GCD(a,i)=i then
		begin
		    r:=false;
		    break;
		end;
		i:=Add(i,'1');
	    end;
	end
    else r:=false;
    IsPrime:=r;
end;

function Divides(a,b: string): boolean;
var r: boolean;
begin
    r:=GCD(a,b)=abs(a);
    Divides:=r;
end;

procedure RationalReduce(var r: Rational);
 var lGcd:string;
begin
 lGcd := gcd(r.Num,r.Den);
 r.Num := diva(r.Num,lGcd);
 r.Den := diva(r.Den,lGcd);
end;

function RationalAdd(const r1,r2: Rational): Rational;
var lRes: Rational;
begin
 lRes.Num := Add(Mul(r1.Num,r2.Den),Mul(r1.Den,r2.Num));
 lRes.Den := Mul(r1.Den,r2.Den);
 RationalReduce(lRes);
 RationalAdd:=lRes;
end;

function RationalSub(const r1,r2: Rational): Rational;
var lRes: Rational;
begin
 lRes.Num := Sub(Mul(r1.Num,r2.Den),Mul(r1.Den,r2.Num));
 lRes.Den := Mul(r1.Den,r2.Den);
 RationalReduce(lRes);
 RationalSub:= lRes;
end;

function RationalNeg(const r1: Rational): Rational;
var lRes: Rational;
begin
 lRes.Num:= Mul('-1',r1.Num);
 lRes.Den:= r1.Den;
 RationalNeg := lRes;
end;

function RationalMult(const r1,r2: Rational): Rational;
var lRes: Rational;
begin
 lRes.Num := Mul(r1.Num,r2.Num);
 lRes.Den := Mul(r1.Den,r2.Den);
 RationalReduce(lRes);
 RationalMult:= lRes;
end;

function RationalInv(const r: Rational): Rational;
var lRes: Rational;
begin
 if r.Num <> '0' then
 begin
  if le(r.Num,'0') then
   lRes.Num := Mul('-1',r.Den)
  else lRes.Num := r.Den;
  lRes.Den := Abs(r.Num);
 end
 else lRes := RZero;
 RationalInv:= lRes;
end;

function RationalDiv(const r1,r2: Rational): Rational;
begin
 RationalDiv := RationalMult(r1,RationalInv(r2));
end;

function RationalEq(const r1,r2: Rational): boolean;
begin
 RationalEq := (r1.Num = r2.Num) and (r1.Den = r2.Den);
end;

function RationalLE(const r1,r2: Rational): boolean;
begin
 RationalLE := leq(Mul(r1.Num,r2.Den),Mul(r1.Den,r2.Num));
end;

function RationalGT(const r1,r2: Rational): boolean;
begin
 RationalGT := not RationalLE(r1,r2);
end;

function IsIntegerNumber(const z: RComplex): boolean;
begin
 IsIntegerNumber := (z.Im.Num = '0') and (z.Re.Den = '1');
end;

function IsNaturalNumber(const z: RComplex): boolean;
begin
 IsNaturalNumber := (z.Im.Num = '0') and (z.Re.Den = '1') and (geq(z.Re.Num,'0'));
end;

function IsPrimeNumber(const z: RComplex): boolean;
begin
    if IsNaturalNumber(z) and IsPrime(z.Re.Num) then IsPrimeNumber := true
    else IsPrimeNumber := false;
end;

function AreEqComplex(const z1,z2: RComplex): boolean;
begin
 AreEqComplex := RationalEq(z1.Re,z2.Re) and RationalEq(z1.Im,z2.Im);
end;

function IsEqWithInt(const z: RComplex; n: longint): boolean;
var s : string;
begin
   str(n,s);
   IsEqWithInt := (z.Im.Num = '0') and (z.Re.Num = s) and (z.Re.Den = '1');
end;

function IsRationalLE(const z1,z2 : RComplex): boolean;
begin
 IsRationalLE := (z1.Im.Num = '0') and (z2.Im.Num = '0') and RationalLE(z1.Re,z2.Re);
end;

function IsRationalGT(const z1,z2 : RComplex): boolean;
begin
 IsRationalGT := (z1.Im.Num = '0') and (z2.Im.Num = '0') and RationalGT(z1.Re,z2.Re);
end;

function IntToComplex(x: integer): RComplex;
var lRes:RComplex;
begin
 lRes:=COne;
 lRes.Re.Num:=IntToStr(x);
 IntToComplex:= lRes;
end;

function ComplexAdd(const z1,z2 : RComplex): RComplex;
var lRes:RComplex;
begin
 lRes.Re := RationalAdd(z1.Re,z2.Re);
 lRes.Im := RationalAdd(z1.Im,z2.Im);
{$IFDEF CH_REPORT}
 CHReport.Out_NumReq3(rqRealAdd, z1, z2, lRes);
{$ENDIF} 
 ComplexAdd:= lRes;
end;

function ComplexSub(const z1,z2: RComplex): RComplex;
var lRes:RComplex;
begin
 lRes.Re := RationalSub(z1.Re,z2.Re);
 lRes.Im := RationalSub(z1.Im,z2.Im);
{$IFDEF CH_REPORT}
   CHReport.Out_NumReq3(rqRealDiff, z1, z2, lRes);
{$ENDIF}  
 ComplexSub:= lRes;
end;

function ComplexNeg(const z: RComplex): RComplex;
var lRes:RComplex;
begin
 lRes.Re := RationalNeg(z.Re);
 lRes.Im := RationalNeg(z.Im);
{$IFDEF CH_REPORT}
   CHReport.Out_NumReq2(rqRealNeg, z, lRes);
{$ENDIF}  
 ComplexNeg:=lRes;
end;

function ComplexMult(const z1,z2 : RComplex): RComplex;
var lRes:RComplex;
begin
 if IsEqWithInt(z1, -1) then ComplexMult:= ComplexNeg(z2) else
  if IsEqWithInt(z2, -1) then ComplexMult:= ComplexNeg(z1) else
 begin
 lRes.Re := RationalSub(RationalMult(z1.Re,z2.Re),RationalMult(z1.Im,z2.Im));
 lRes.Im := RationalAdd(RationalMult(z1.Re,z2.Im),RationalMult(z1.Im,z2.Re));
 ComplexMult:=lRes;
{$IFDEF CH_REPORT}
 CHReport.Out_NumReq3(rqRealMult, z1, z2, lRes);
{$ENDIF}  
 end;
end;

function ComplexDiv(const z1,z2: RComplex) : RComplex;
 var lDenom : Rational;
 lRes:RComplex;
begin
 lRes:=CZero;
 with z2 do
  lDenom := RationalAdd(RationalMult(Re,Re),RationalMult(Im,Im));
 if lDenom.Num = '0' then
  begin
//        writeln('******** function Cdiv ********');
//        writeln('******* DIVISION BY ZERO ******');
//        halt
  end
 else
  begin
   lRes.Re := RationalDiv(RationalAdd(RationalMult(z1.Re,z2.Re),
                                        RationalMult(z1.Im,z2.Im)),
                            lDenom);
   lRes.Im := RationalDiv(RationalSub(RationalMult(z1.Im,z2.Re),
                                         RationalMult(z1.Re,z2.Im)),
                            lDenom);
{$IFDEF CH_REPORT}
   CHReport.Out_NumReq3(rqRealDiv, z1, z2, lRes);
{$ENDIF}     
  end;
 ComplexDiv:=lRes;
end;

function ComplexInv(const z: RComplex): RComplex;
begin
 ComplexInv:=ComplexDiv(COne,z);
end;

function ComplexNorm(const z: RComplex): Rational;
begin
 ComplexNorm:=RationalAdd(RationalMult(Z.Re,Z.Re),RationalMult(Z.Im,Z.Im));
end;

function CompareInt(X1, X2: Longint): Integer;
begin
 if X1 = X2 then CompareInt := 0
 else if X1 > X2 then CompareInt := 1
 else CompareInt := -1;
end;

function CompareIntStr(X1, X2: String): Integer;
begin
 if X1 = X2 then CompareIntStr := 0
 else if gt(X1,X2) then CompareIntStr := 1
 else CompareIntStr := -1;
end;

function CompareComplex(const z1,z2: RComplex): Integer;
 var lInt: integer;
begin
  lInt:=CompareIntStr(z1.Re.Num,z2.Re.Num);
  if lInt <> 0 then begin CompareComplex:=lInt; exit end;
  lInt:=CompareIntStr(z1.Re.Den,z2.Re.Den);
  if lInt <> 0 then begin CompareComplex:=lInt; exit end;
  lInt:=CompareIntStr(z1.Im.Num,z2.Im.Num);
  if lInt <> 0 then begin CompareComplex:=lInt; exit end;
  CompareComplex:=CompareIntStr(z1.Im.Den,z2.Im.Den);
end;

end.
