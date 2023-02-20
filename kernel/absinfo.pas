(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit absinfo;

interface

uses generato,limits,express,mobjects;

procedure AbsTerm(fTrm: ExpPtr);
procedure AbsType(fTyp: ExpPtr);
procedure AbsFormula(fFrm: ExpPtr);
{procedure AbsTypeList(fTypList: aTypList);}
procedure AbsSigmaRec(var fSigma:SigmaRec);
procedure AbsThetaRec(var fTheta:ThetaRec);
procedure AbsResDes(var fResDes: ResDes);
procedure AbsResList(var fResList:MCollection);
procedure AbsTermList(var fTrmList: MCollection);

procedure AbsmultipleTypeExp(fPtr:multipleTypePtr);

implementation

uses info,errhan,lexicon;

{+--------------------P-I-S-A-N-I-E----------------------------------+}

procedure AbsmultipleTypeExp(fPtr:multipleTypePtr);
begin
 with fPtr^ do
  begin AbsType(nType); InfoString(' times '); InfoInt(nNumberOfCopies); end;
end;
{                 procedure AbsTypeList;
 var i:integer;
begin
 for i:=1 to MaxElemNbr do
  if fTypList=nil then begin InfoChar(';'); exit end else
   with fTypList^ do begin AbsType(TypPtr); fTypList:=NextTyp end;
 RunError(2001);
end;}

procedure AbsTermList(var fTrmList: MCollection);
 var i,z:integer;
 procedure AbsItem(Item:ExpPtr);
 begin AbsTerm(Item);
 end;
begin
 with fTrmList do for z:=0 to Count-1 do AbsItem(ExpPtr(Items^[z]));
end;

procedure AbsType(fTyp: ExpPtr);
begin
 with fTyp^ do
  if TypeOf(fTyp^) = TypeOf(AttributedType) then
   with AttributedTypePtr(fTyp)^ do
    begin AbsType(Podmiot); InfoPos(ExpPos);  end
  else if TypeOf(fTyp^) = TypeOf(TypeExp) then
   with TypePtr(fTyp)^ do
    begin InfoWord(ExpSort,nConstrNr); InfoPos(ExpPos); AbsTermList(Argumenty) end
  else if TypeOf(fTyp^) = TypeOf(ResDesNode) then
   with TypePtr(fTyp)^ do
    begin InfoWord(ExpSort,nConstrNr); InfoPos(ExpPos); AbsTermList(Argumenty) end
(*+  else if TypeOf(fTyp^) = TypeOf(SetType) then
   with SetTypePtr(fTyp)^ do
   {sySet:} begin InfoChar(sySet); InfoPos(ExpPos); AbsType(BasicType) end+*)
  else if TypeOf(fTyp^) = TypeOf(InCorrType) then
   with InCorrTypePtr(fTyp)^ do
   {ikError:} InfoChar(ikError)
   else begin writeln(InfoFile,'Abstype ?'); RunTimeError(2003) end;
 end;

procedure AbsResDes(var fResDes: ResDes);
 procedure AbsVar(Item:SimpleTermPtr);
 begin with Item^ do write(InfoFile,'"',ExpSort,'"',VarNr,' ');
 end;
  var z: integer;
begin InfoWord('D',fResDes.ResNr);
write(InfoFile,'Count=',fResDes.Subst.Count);
with fResDes.Subst do for z:=0 to Count-1 do AbsVar(SimpleTermPtr(Items^[z]));
 InfoChar(';');
end;

procedure AbsResList(var fResList:MCollection);
 procedure lAbsResDes(Item:ResDesPtr);
 begin AbsType(Item) end;
  var z: integer;
begin
 with fResList do for z:=0 to Count-1 do lAbsResDes(ResDesPtr(Items^[z]));

 InfoChar(';');
end;

procedure AbsFormula(fFrm: ExpPtr);
begin
  with fFrm^ do
   if TypeOf(fFrm^) = TypeOf(PredicateFormula) then
     with PredicateFormulaPtr(fFrm)^ do
      begin InfoWord(ExpSort,nConstrNr); InfoPos(ExpPos);
       AbsTermList(Argumenty);
      end
   else if TypeOf(fFrm^) = TypeOf(QualifyingFormula) then
     with QualifyingFormulaPtr(fFrm)^ do
      begin InfoChar(ikFrmQual);
       AbsTerm(Obiekt); AbsType(Kwalifikacja);
      end
   else if TypeOf(fFrm^) = TypeOf(NegativeFormula) then
     with NegativeFormulaPtr(fFrm)^ do
      begin InfoChar(ikFrmNeg); AbsFormula(NegArg) end
   else if TypeOf(fFrm^) = TypeOf(BinaryFormula) then
     with BinaryFormulaPtr(fFrm)^ do
      begin InfoChar(ExpSort);
       AbsFormula(BinArg1); AbsFormula(BinArg2);
      end
   else if TypeOf(fFrm^) = TypeOf(QuantifiedFormula) then
     with QuantifiedFormulaPtr(fFrm)^ do
      begin InfoWord('Q',QuantVars.fCount);
       AbsType(Quantified); AbsFormula(Scope);
      end
   else if TypeOf(fFrm^) = TypeOf(ThesisFormula) then
     InfoChar('$')
   else if TypeOf(fFrm^) = TypeOf(VerumFormula) then
     InfoChar('%')
   else if TypeOf(fFrm^) = TypeOf(InCorrFormula) then
     InfoChar(ikError)
   else InfoChar('@');
end;

procedure AbsTerm(fTrm: ExpPtr);
 procedure AbsMultiType(Item:multipleTypePtr);
 begin
  with Item^ do
   begin AbsType(nType); InfoString(' times '); InfoInt(nNumberOfCopies);
    writeln(InfoFile);
   end;
 end;
  var z: integer;
begin
 with fTrm^ do
  if TypeOf(fTrm^) = TypeOf(SimpleTerm) then
   with SimpleTermPtr(fTrm)^ do
    InfoWord(ExpSort,VarNr)
  else if TypeOf(fTrm^) = TypeOf(NumeralTerm) then
   InfoWord('N',NumeralTermPtr(fTrm)^.nNatVal)
  else if TypeOf(fTrm^) = TypeOf(FunctorTerm) then
   with FunctorTermPtr(fTrm)^ do
    begin InfoWord(ExpSort,nConstrNr); InfoPos(ExpPos); AbsTermList(Argumenty) end
  else if TypeOf(fTrm^) = TypeOf(SelectorTerm) then
   with SelectorTermPtr(fTrm)^ do
    begin InfoWord('U',Select); InfoPos(ExpPos); AbsTerm(Struct) end
  else if TypeOf(fTrm^) = TypeOf(FraenkelTerm) then
   with FraenkelTermPtr(fTrm)^ do
    begin InfoChar(ikTrmFraenkel); InfoPos(ExpPos);
     with Kappa(Sample)^ do
      begin
{       AbsResList(nBoundVars);}
       with nBoundVars do for z:=0 to Count-1 do AbsMultiType(multipleTypePtr(Items^[z]));
       AbsTerm(nExpressionPtr);
      end;
     AbsSigmaRec(Compr^);
    end
  else if TypeOf(fTrm^) = TypeOf(QualifiedTerm) then
   with QualifiedTermPtr(fTrm)^ do
    begin InfoChar(ikTrmQua); InfoPos(ExpPos);
     AbsTerm(Obiekt); AbsType(Kwalifikacja);
    end
  else if TypeOf(fTrm^) = TypeOf(ItTerm) then
   InfoChar(ikTrmIt)
  else if TypeOf(fTrm^) = TypeOf(InCorrTerm) then
   InfoChar(ikError)
  else InfoChar('@');
end;

procedure AbsSigmaRec(var fSigma:SigmaRec);
begin
 with fSigma do
  begin AbsResList(nFreeVars); AbsFormula(nExpressionPtr) end;
end;

procedure AbsThetaRec(var fTheta:ThetaRec);
begin
 with fTheta do
  begin AbsResList(nFreeVars); AbsType(nExpressionPtr) end;
end;

end.
