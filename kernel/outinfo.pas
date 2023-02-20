(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit outinfo;

interface

uses info,inout,correl,mobjects,numbers;

procedure InfoLexem(const aLexem: Lexem);
procedure InfoTerm ( fTrm: TrmPtr );
procedure InfoTermList ( fTrmList: TrmList );
procedure InfoTermMList (var aList: MList);
procedure InfoType ( fTyp:TypPtr);
procedure InfoAttr(aAttr: AttrPtr);
procedure InfoCluster(fCol:AttrCollectionPtr);
procedure InfoTypeList (const fTypList:MList );
procedure InfoTypeColl (const fTypList:MCollection );
procedure InfoFormula ( fFrm: FrmPtr );
procedure InfoFormulaList (const fFrmList:MCollection );

procedure InfoNatSet(const aFunc: NatSet);
procedure InfoNatFunc(const aFunc: NatFunc);
procedure InfoBinIntFunc(const aFunc: BinIntFunc);
procedure InfoComplex(const c: RComplex);

procedure InfoInferConstDef(II: integer);
procedure InfoBuildIn;

implementation

uses lexicon,limits,builtin,enums;

procedure InfoLexem(const aLexem: Lexem);
begin
 infoWord(aLexem.Kind,aLexem.Nr);
end;

procedure InfoTermMList (var aList: MList);
 var i: integer;
begin
 InfoChar('(');
 for i:=0 to aList.Count-1 do
  InfoTerm(aList.Items^[i]);
 InfoChar(')');
end;

procedure InfoTermList ( fTrmList: TrmList );
begin
 if fTrmList = IncorrTrmList then
  begin
   write(infofile,'?trmlist');
   exit
  end;
 InfoChar('(');
 while fTrmList<> nil do
  begin InfoTerm(fTrmList^.XTrmPtr);
   fTrmList:=fTrmList^.NextTrm;
  end;
 InfoChar(')');
end;

procedure InfoTypeList (const fTypList:MList );
  var z: integer;
begin
 with fTypList do for z:=0 to Count-1 do InfoType(TypPtr(Items^[z]));
 InfoChar(';');
end;

procedure InfoTypeColl (const fTypList:MCollection );
  var z: integer;
begin
 with fTypList do for z:=0 to Count-1 do InfoType(TypPtr(Items^[z]));
 InfoChar(';');
end;

procedure InfoAttr(aAttr: AttrPtr);
  var lAttrNr: integer; lArgs: TrmList;
begin
  if aAttr^.fNeg = 0 then InfoChar('-');
  InfoWord('V',aAttr^.fAttrNr);
  InfoTermList(aAttr^.fAttrArgs);
  aAttr^.AdjustAttr(lAttrNr,lArgs);
  if lAttrNr <> aAttr^.fAttrNr then
   begin
//    infochar('(');
    InfoWord('V',lAttrNr);
    InfoTermList(lArgs);
//    infochar(')');
   end;
end;

procedure InfoCluster(fCol:AttrCollectionPtr);
 var i: integer;
begin
 infochar('[');
 with fCol^ do
 begin
  for i:=0 to Count-1 do InfoAttr(AttrPtr(Items^[i]));
  if not fConsistent then infochar(ikError);
 end;
 infochar(']');
end;


procedure InfoType ( fTyp:TypPtr);
 var lLC,lUC:integer;
begin
 with fTyp^ do
  case TypSort of
   ikTypMode,ikTypStruct:
    begin
     InfoCluster(LowerCluster);
     InfoCluster(UpperCluster);
     InfoWord(TypSort,ModNr);
     InfoTermList(ModArgs);
    end;
   ikError: InfoChar(TypSort);
  else writeln(infofile,'TypSort=',TypSort);
 end;
end;

procedure InfoFormula ( fFrm: FrmPtr );
begin
  with fFrm^ do
   case FrmSort of
    ikFrmNeg: begin InfoChar('-'); InfoFormula(NegFrmPtr(fFrm)^.NegArg) end;
    ikFrmConj:
     begin InfoChar(ikFrmConj);
      InfoFormulaList(ConjFrmPtr(fFrm)^.Conjuncts);
     end;
    ikFrmSchPred,ikFrmAttr,ikFrmPred:
     begin InfoWord(FrmSort,PredFrmPtr(fFrm)^.PredNr);
      InfoTermList(PredFrmPtr(fFrm)^.PredArgs)
     end;
    ikFrmPrivPred:
     begin InfoWord(FrmSort,PredFrmPtr(fFrm)^.PredNr); InfoTermList(PredFrmPtr(fFrm)^.PredArgs);
      infochar('['); infoformula(LocPredFrmPtr(fFrm)^.PredExp); infochar(']');
     end;
    ikFrmUniv:
     begin InfoChar('f');
      InfoType(UnivFrmPtr(fFrm)^.Quantified); InfoFormula(UnivFrmPtr(fFrm)^.Scope);
     end;
    ikFrmQual:
     begin InfoChar('q');
      InfoTerm(QualFrmPtr(fFrm)^.QualTrm); InfoType(QualFrmPtr(fFrm)^.QualTyp);
     end;
    ikFrmFlexConj{,ikFrmFlexDisj}:
     with FlexFrmPtr(fFrm)^ do
      begin
       InfoChar(FrmSort); InfoNewLine;
       InfoString('nLeftTerm  '); InfoTerm(nLeftTrm); InfoNewLine;
       InfoString('nRightTerm  '); InfoTerm(nRightTrm); InfoNewLine;
       InfoString('nLeftOrigFrm  '); InfoFormula(nLeftOrigFrm); InfoNewLine;
       InfoString('nRightOrigFrm  '); InfoFormula(nRightOrigFrm); InfoNewLine;
       InfoString('nExpansion  '); InfoFormula(nExpansion); InfoNewLine;
      end;
    ikFrmThesis,ikFrmVerum,ikError: InfoChar(FrmSort);    
    else writeln(infofile,'FrmSort=',FrmSort);
   end;
end;

procedure InfoFormulaList(const fFrmList:MCollection );
 var i: Integer;
begin
 InfoChar('{');
 with fFrmList do
  for i:=0 to Count-1 do InfoFormula(FrmPtr(Items^[i]));
 InfoChar('}');
end;

procedure InfoTerm ( fTrm: TrmPtr );
 var lNr: integer;
begin
 with fTrm^ do
  case TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmInfConst,ikTrmEqConst,
   ikTrmFreeVar,ikTrmLambdaVar,ikTrmNumeral: 
    with VarTrmPtr(fTrm)^ do
     begin  
      InfoWord(TrmSort,VarNr);
      if TrmInfo <> 0 then InfoInt(TrmInfo);			    
     end;
   ikTrmFunctor:
    with FuncTrmPtr(fTrm)^ do
    begin
     lNr:=OriginalNr( coFunctor,FuncNr);
     InfoWord(TrmSort,FuncNr);
     if TrmInfo <> 0 then InfoInt(TrmInfo);			    
     if lNr <> 0 then
      begin
       infostring('=> K'); infoint(lNr);// infochar(')');
      end;
     InfoTermList(FuncArgs)
    end;
   ikTrmSchFunc,ikTrmAggreg,ikTrmSelector:
    with FuncTrmPtr(fTrm)^ do
    begin InfoWord(TrmSort,FuncNr);
     InfoTermList(FuncArgs)
    end;
   ikTrmPrivFunc:
    with FuncTrmPtr(fTrm)^ do
    begin InfoWord(TrmSort,FuncNr); InfoTermList(FuncArgs);
     infochar('('); infoterm(LocFuncTrmPtr(fTrm)^.FuncExp); infochar(')');
    end;
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
    begin InfoChar(ikTrmFraenkel);
     InfoTypeColl(LambdaArgs);
     InfoTerm(LambdaScope);
     InfoFormula(Compr);
    end;
   ikTrmChoice:
    begin InfoChar('c'); InfoChar('h'); InfoChar(' ');
      InfoType(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
    end;
   ikTrmQua:
    with QuaTrmPtr(fTrm)^ do
    begin InfoChar(ikTrmQua);
     InfoTerm(TrmProper);
     InfoType(Qua)
    end;
   ikTrmIt,ikError: InfoChar(TrmSort);
   else writeln(infofile,'TrmSort=',TrmSort);
  end;
end;

procedure InfoNatSet(const aFunc: NatSet);
var i : Integer;
begin 
   with aFunc do
      for i:=0 to Count-1 do
	 with Items^[i] do
	 begin
	    InfoWord(' ',X);
	 end;
end;

procedure InfoNatFunc(const aFunc: NatFunc);
 var i: Integer;
begin
 with aFunc do
 for i:=0 to Count-1 do
 with Items^[i] do
  begin
   InfoWord('X',X);
   InfoWord('Y',Y);
  end;
end;

procedure InfoBinIntFunc(const aFunc: BinIntFunc);
 var i: Integer;
begin
 with aFunc do
 for i:=0 to fCount-1 do
 with fList^[i] do
  begin
   InfoWord('X',X1);
   InfoWord('Y',X2);
   InfoWord('Z',Y);
  end;
end;

procedure infocomplex(const c: RComplex);
begin
 with c do
 begin
  if (Re.Num='0') and (Im.Num='0') then
   begin
    write(infofile,'0');
    exit;
   end;
  if Re.Num<>'0' then
   if Re.Den='1' then
    write(infofile,Re.Num)
   else write(infofile,Re.Num,'/',Re.Den);
  if Im.Num<>'0' then
   begin
    if Re.Num<>'0' then
     write(infofile,'+');
    if Im.Den='1' then
     write(infofile,Im.Num)
     else write(infofile,Im.Num,'/',Im.Den);
    write(infofile,'*i');
   end;
 end;
end;

procedure InfoInferConstDef(II: integer);
 var z: integer;
begin
 with ConstDefPtr(InferConstDef.Items^[II])^ do
 begin
  write(infofile,'InferConstDef[',ii,'] ');
  if fDetermined then
   begin
    write(infofile,'(');
    infocomplex(fNumericValue);
    write(infofile,') ');
   end;
  infoterm(fDef); infonewline;
  infotype(fTyp); infonewline;
  if fEqConst.Count > 0 then
   begin
    for z:=0 to fEqConst.Count-1 do write(infofile,' := ',fEqConst.Items^[z].X);
    infonewline;
   end;
 end;
end;

procedure InfoBuildIn;
begin
  writeln(InfoFile,' tabela gBuiltIn:');
  writeln(InfoFile,'= : R', gBuiltIn[rqEqualsTo]);
  writeln(InfoFile,'in : R', gBuiltIn[rqBelongsTo]);           //3
                    {--- XBOOLE_0/BOOLE      ---}
  writeln(InfoFile,'is empty : V', gBuiltIn[rqEmpty]);         //4
  writeln(InfoFile,'{} : K', gBuiltIn[rqEmptySet]);            //5
                    {--- SUBSET_1/SUBSET     ---}
  writeln(InfoFile,'Element of :M', gBuiltIn[rqElement]);      //6
                    {--- ZFMISC_1/SUBSET ---}
  writeln(InfoFile,'boole : K', gBuiltIn[rqPowerSet]);         //7
                    {--- TARSKI/SUBSET       ---}
  writeln(InfoFile,'c= : R', gBuiltIn[rqInclusion]);           //8
                    {--- SUBSET_1/SUBSET     ---}
  writeln(InfoFile,'Subset : M', gBuiltIn[rqSubDomElem]);      //9
                    {--- NUMBERS             ---}
  writeln(InfoFile,'REAL : K', gBuiltIn[rqRealDom]);           //10
  writeln(InfoFile,'NAT : K', gBuiltIn[rqNatDom]);             //11
                    {--- XCMPLX_0/ARITHM     ---}
  writeln(InfoFile,'+ : K', gBuiltIn[rqRealAdd]);              //12
  writeln(InfoFile,'* : K', gBuiltIn[rqRealMult]);             //13
                    {--- XREAL_0/REAL        ---}
  writeln(InfoFile,'<= : R', gBuiltIn[rqLessOrEqual]);         //14
                    {--- ORDINAL1/NUMERALS   ---}
  writeln(InfoFile,'succ : K', gBuiltIn[rqSucc]);              //15
                    {--- XBOOLE_0/BOOLE)     ---}
  writeln(InfoFile,'\/ : K', gBuiltIn[rqUnion]);               //16
  writeln(InfoFile,'/\ : K', gBuiltIn[rqIntersection]);        //17
  writeln(InfoFile,'\ : K', gBuiltIn[rqSubtraction]);          //18
  writeln(InfoFile,' : K', gBuiltIn[rqSymmetricDifference]);   //19
  writeln(InfoFile,'meets : R', gBuiltIn[rqMeets]);            //20
                    {--- XCMPLX_0/ARITHM     ---}
  writeln(InfoFile,'- : K', gBuiltIn[rqRealNeg]);              //21
  writeln(InfoFile,'" : K', gBuiltIn[rqRealInv]);              //22
  writeln(InfoFile,'- : K', gBuiltIn[rqRealDiff]);             //23
  writeln(InfoFile,'/ : K', gBuiltIn[rqRealDiv]);              //24
                    {--- XREAL_0             ---}
  writeln(InfoFile,'is real : V', gBuiltIn[rqReal]);           //25
                    {--- XREAL_0/REAL        ---}
  writeln(InfoFile,'is positive : V', gBuiltIn[rqPositive]);   //26
  writeln(InfoFile,'is negative : V', gBuiltIn[rqNegative]);   //27
                    {--- ORDINAL2/NUMERALS   ---}
  writeln(InfoFile,'is natural : V', gBuiltIn[rqNatural]);     //28
                    {--- XCMPLX_0/COMPLEX    ---}
  writeln(InfoFile,'<i> : K', gBuiltIn[rqImaginaryUnit]);      //29
  writeln(InfoFile,'is complex : V', gBuiltIn[rqComplex]);     //30
                     {--- ORDINAL2/NUMERALS   ---}
  writeln(InfoFile,'omega : K', gBuiltIn[rqOmega]);            //32
  writeln(InfoFile,'0 : K', gBuiltIn[rqZeroNumber]);           //33
  writeln(InfoFile,'is zero : V', gBuiltIn[rqZero]);           //34
end;

end.
