(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit identify;

interface

uses mobjects,correl,inout,limits,enums;

type
 PartDefPtr = ^PartDefObj;
 PartDefObj =
  object(MObject)
     nPartDefiniens,nGuard: PObject;
   constructor Init(fPartDef,fGuard:PObject);
   destructor Done; virtual;
  end;

 DefPtr = ^DefObj;
 DefObj =
  object(MObject)
     DefSort:char;
     nPartialDefinientia: MCollection;
     nOtherwise: PObject;
    constructor Init(fSort:char;var fPartialDefs:MCollection; fOtherwise: PObject);
    destructor Done; virtual;
  end;

 ConstrDescrPtr = ^ConstrDescrObj;
 ConstrDescrObj =
  object(MObject)
    nDefNr: integer;         // nr of corresponding def. theorem
    nArticle: string;        // article where defined
    nConstr: Lexem;          // constructor expanded
    PrimaryList: MList;
   constructor Init(aKind:char; aNr:integer;
                    var aPrimaryList:MList; aPattern: PObject);
   destructor Done; virtual;
  end;

 ConstrDefPtr = ^ConstrDefObj;
 ConstrDefObj =
  object(ConstrDescrObj)
    nPattern: PObject;
   constructor Init(aKind:char; aNr:integer; aPattern: PObject);
   constructor InitConstrDef(aKind:char; aNr:integer;
                             var aPrimaryList:MList; aPattern: PObject);
   destructor Done; virtual;
  end;

// ##TODO: label and position?
 DefiniensPtr = ^DefiniensObj;
 DefiniensObj =
  object(ConstrDefObj)
    nLabId: integer;   // label of corresponding def. theorem
    Essentials: IntSequence;
    Assumptions:FrmPtr;
    Definiens: DefPtr;
   function SpreadFrm(fArgs: TrmList; Negated,Conclusion: boolean):FrmPtr; virtual;
   constructor Init(fKind:char; fNr,fDefNr,fLabId:integer; fArticle: string;
                    var fPrimaryList:MList;
                    var fEssentials: IntSequence;
                    fAssum:FrmPtr; fDef:DefPtr);
   destructor Done; virtual;
  end;

 ExpConstrDefPtr = ^ExpConstrDefObj;
 ExpConstrDefObj = object(ConstrDefObj)
    nExpansion: PObject;
   constructor Init(const aConstr: Lexem; var aPrimaryList:MList;
                    aPattern,aDef: PObject);
   destructor Done; virtual;
  end;

 EqualsDefPtr = ^EqualsDefObj;
 EqualsDefObj = object(ExpConstrDefObj)
    nEssentials: IntSequence;
//   constructor Init(const aConstr: Lexem; var aPrimaryList:MList;
//                    aPattern,aDef: PObject);
   constructor Init(var aPrimArgs: MList; aFuncDef: TrmPtr);
   destructor Done; virtual;
  end;

 IdentifyPtr = ^IdentifyObj;
 IdentifyObj = object(MObject)
    nAbsNr: integer;         // absolute nr in its article
    nArticle: string;        // article where defined
    nConstrKind: char;
    nPrimaryList: MList;
    nPattern: array[0..1] of ExprPtr;
    nEqArgs: IntRel;
    constructor Init(aAbsNr:integer; aArticle:string;
                     aKInd:char; var aPrimaryList:MList;
                     aPattern1,aPattern2:ExprPtr;
                     var aEqArgs: IntRel);
   destructor Done; virtual;
  end;

 ReductionPtr = ^ReductionObj;
 ReductionObj = object(MObject)
    nAbsNr: integer;         // absolute nr in its article
    nArticle: string;        // article where defined
    nPrimaryList: MList;
    nTerms: array[0..1] of TrmPtr;
    constructor Init(aAbsNr:integer; aArticle:string;
                     var aPrimaryList:MList;
                     aTerm1,aTerm2:TrmPtr);
   destructor Done; virtual;
  end;

  PropertyPtr = ^PropertyObj;
  PropertyObj = object(MObject)
    nAbsNr: integer;         // absolute nr in its article
    nArticle: string;        // article where defined
    nPrimaryList: MList;
    nPropertyKind: integer;
    nObject: PObject;
    constructor Init(aAbsNr:integer; aArticle:string;
                     var aPrimaryList:MList;
                     aPropKind: integer;
                     aObject: PObject);
   destructor Done; virtual;
  end;

var
  Definientia, EqDefinientia,ExDefinientia,gIdentifications, gReductions, gPropertiesList: MList;

function EqualsExpansion(aDef: DefiniensPtr): EqualsDefPtr;
function MakeDefiniens(Negated:boolean; fDef:DefPtr): FrmPtr;
function Matches(aWord:Lexem; aArgs:TrmList; aDef:DefiniensPtr):boolean;

procedure CollectConstInTrm(var aTrm: TrmPtr);
procedure CollectConstInFrm(aFrm: FrmPtr);
procedure CollectConstInCluster(var aClu: AttrCollectionPtr);
procedure CollectConstInTyp(aTyp: TypPtr);
procedure CollectConstInTypList(var aColl: MList);
procedure CollectConstInFrmCollection(var aColl: MList);

function CompConstDef(aDef1,aDef2: pointer):integer;

procedure InitEssentialsArgs(aArgs:TrmList; const aEssentials:IntSequence);
function ExpandTrmIfEqual(aFuncDef: EqualsDefPtr; aArgs: TrmList): TrmPtr;
function FrOpVarTypeOK(fTyp: TypPtr): boolean;

// Functor by Equals Definitions
var gEquals,gFuncIds: MLexemAndListList;

{$IFDEF MDEBUG}
procedure InfoDefiniens(Item:DefiniensPtr); far;
{$ENDIF}

implementation

uses lexicon,mizenv,errhan,numbers,builtin,mscanner
{$IFDEF MDEBUG},info,outinfo{$ENDIF};

constructor ConstrDescrObj.Init(aKind:char; aNr:integer;
                                var aPrimaryList:MList; aPattern: PObject);
begin
 nConstr.Kind:=aKind; nConstr.Nr:=aNr;
 nDefNr:= 0; nArticle:= '';
 PrimaryList.MoveList(aPrimaryList);
end;

destructor ConstrDescrObj.Done;
begin
 PrimaryList.Done;
end;

constructor ConstrDefObj.Init (aKind:char; aNr:integer; aPattern: PObject);
begin
 nConstr.Kind:=aKind; nConstr.Nr:=aNr;
 PrimaryList.Init(MaxArgNbr);
 nPattern:=aPattern;
end;

constructor ConstrDefObj.InitConstrDef(aKind:char; aNr:integer;
                               var aPrimaryList:MList; aPattern: PObject);
begin
 nConstr.Kind:=aKind; nConstr.Nr:=aNr;
 nDefNr:= 0; nArticle:= '';
 PrimaryList.MoveList(aPrimaryList);
 nPattern:=aPattern;
end;

destructor ConstrDefObj.Done;
begin
 PrimaryList.Done;
 if nPattern <> nil then dispose(nPattern,Done);
end;

constructor DefiniensObj.Init(fKind:char; fNr,fDefNr,fLabId:integer;
                              fArticle: string; var fPrimaryList:MList;
                              var fEssentials: IntSequence;
                              fAssum:FrmPtr; fDef:DefPtr);
begin
 nConstr.Kind:=fKind; nConstr.Nr:=fNr;
 nDefNr:= fDefNr; nLabId:= fLabId;
 nArticle:= fArticle;
 PrimaryList.MoveList(fPrimaryList);
 nPattern:=nil;
 Essentials.CopySequence(fEssentials);
 Assumptions:=fAssum;
 Definiens:=fDef
end;

destructor DefiniensObj.Done;
begin
 inherited Done;
 Essentials.Done;
 if Assumptions <> nil then dispose(Assumptions,Done);
 dispose(Definiens,Done);
end;

function MakeDefiniens(Negated:boolean; fDef:DefPtr): FrmPtr;
 var lFrm,lOtherGuard,dFrm:FrmPtr;
     z: integer;
begin
 if fDef = nil then begin MakeDefiniens:=NewInCorFrm; exit end;
 with fDef^ do
  if nPartialDefinientia.Count = 0 then
   begin dFrm:=FrmPtr(nOtherwise)^.CopyFormula;
     if Negated then dFrm:=NewNegDis(dFrm)
   end
  else
   begin dFrm:=NewNeg(NewVerum); lOtherGuard:=NewVerum;
    with nPartialDefinientia do
     for z:=0 to Count-1 do
      with PartDefPtr(Items^[z])^ do
       begin
        if Negated then
          lFrm:=NewConj(FrmPtr(nGuard)^.CopyFormula,
                        NewNegDis(FrmPtr(nPartDefiniens)^.CopyFormula))
         else lFrm:=NewConj(FrmPtr(nGuard)^.CopyFormula,
                             FrmPtr(nPartDefiniens)^.CopyFormula);
        dFrm:=NewDisj(dFrm,lFrm);
        if fDef^.nOtherWise <> nil then
         lOtherGuard:=NewConj(lOtherGuard,NewNegDis(FrmPtr(nGuard)^.CopyFormula));
       end;
    if nOtherWise<>nil then
     begin
      if Negated then lFrm:=NewConj(lOtherGuard,NewNegDis(FrmPtr(nOtherWise)^.CopyFormula))
      else lFrm:=NewConj(lOtherGuard,FrmPtr(nOtherWise)^.CopyFormula);
     dFrm:=NewDisj(dFrm,lFrm);
     end
    else dispose(lOtherGuard,Done);
   end;
 MakeDefiniens:=dFrm;
end;

function DefiniensObj.SpreadFrm(fArgs: TrmList; Negated,Conclusion: boolean):FrmPtr;
 var lFrm: FrmPtr;
begin
 lFrm:=MakeDefiniens(Negated,Definiens);
 if lFrm^.FrmSort = ikError then begin SpreadFrm:=NewInCorFrm; exit end;
 if Assumptions <> nil then
 if Conclusion then lFrm:=NewConj(Assumptions^.CopyFormula,lFrm)
 else lFrm:=NewNeg(NewConj(Assumptions^.CopyFormula,NewNeg(lFrm)));
 SpreadFrm:=InstFrm(lFrm,fArgs); dispose(lFrm,Done);
 DisposeTrmList(fArgs);
end;

constructor PartDefObj.Init(fPartDef,fGuard:PObject);
begin
 nGuard:=fGuard; nPartDefiniens:=fPartDef;
end;

destructor PartDefObj.Done;
begin dispose(nPartDefiniens,Done); dispose(nGuard,Done) end;

constructor DefObj.Init(fSort:char;var fPartialDefs:MCollection; fOtherwise:PObject);
begin DefSort:=fSort;
 nPartialDefinientia.MoveCollection(fPartialDefs);
 nOtherwise:=fOtherwise;
end;

destructor DefObj.Done;
begin
 if nOtherwise <> nil then dispose(nOtherwise,Done);
 nPartialDefinientia.Done;
end;

// ##TODO: inherit the info methods from iocorrel
{$IFDEF MDEBUG}
procedure InfoDefiniens(Item:DefiniensPtr); far;
  var k:integer;
begin
  with Item^ do
   begin
    writeln(infofile,'Definicaja: ',nConstr.Kind,nConstr.Nr);
    with PrimaryList do
     for k:=0 to Count-1 do infoType(Items^[k]);
    infoChar(';'); infoNewLine;
    for k:=0 to Essentials.fCount-1 do
     infoWord('A',Essentials.fList^[k]);
    infoChar(';'); infoNewLine;
    if (Assumptions <> nil) and (Assumptions^.FrmSort <> ikFrmVerum) then
      begin infoChar('a'); infoformula(Assumptions); infoNewLine end;
    if Definiens <> nil then
     with Definiens^, nPartialDefinientia do
      begin infoChar(DefSort);
        for k:=0 to Count-1 do
         with PartDefPtr(Items^[k])^ do
          begin infoChar('c');
           case DefSort of
           'm': infoformula(FrmPtr(nPartDefiniens));
           'e': infoterm(TrmPtr(nPartDefiniens));
           end;
           infoformula(FrmPtr(nGuard));
           infoNewLine;
          end;
        if nOtherWise <> nil then
          begin infoChar('o');
            case DefSort of
            'm': infoformula(FrmPtr(nOtherWise));
            'e': infoterm(TrmPtr(nOtherWise));
            end;
           infoNewLine;
          end;
      end;
   end;
 end;
{$ENDIF}

function EqualsExpansion(aDef: DefiniensPtr): EqualsDefPtr;
 var i:integer;
     lTypColl: MList;
     lFuncDef: EqualsDefPtr;
     lTrm: TrmPtr;
     lArgs: TrmList;
begin
  EqualsExpansion:=nil;
  with aDef^ do
  if Definiens <> nil then
   with Definiens^ do
   begin
    if nConstr.Kind <> ikTrmFunctor then exit;
    if (Assumptions <> nil) and (Assumptions^.FrmSort <> ikFrmVerum) then
     exit;
    if DefSort <> 'e' then exit;
    if nPartialDefinientia.Count > 0 then exit;
    if nOtherWise = nil then exit;
    CopyTypeColl(PrimaryList,lTypColl);
    lTypColl.FreeItemsFrom(lTypColl.Count-1);
    lTrm:=CopyTerm(TrmPtr(nOtherWise));
//    CollectConstInTypList(lTypColl);
//    CollectConstInTrm(lTrm);
    lFuncDef:=new(EqualsDefPtr,Init(lTypColl,lTrm));
    lFuncDef^.nEssentials.CopySequence(Essentials);
    lFuncDef^.nEssentials.AtDelete(lFuncDef^.nEssentials.fCount-1);
    lArgs:=nil;
    for i:=Essentials.fCount-1 downto 0 do
     lArgs:=NewTrmList(NewVarTrm(ikTrmLocus,Essentials.fList^[i]),lArgs);
    lFuncDef^.nPattern:=NewFuncTrm(nConstr.Nr,lArgs);
    EqualsExpansion:=lFuncDef;
   end;
end;

function Matches(aWord:Lexem; aArgs:TrmList; aDef:DefiniensPtr):boolean;
begin Matches:=false;
 with aDef^ do
  begin
   fillchar(gSubstTrm,sizeof(gSubstTrm),0);
   if (aWord.Kind <> nConstr.Kind) or (aWord.Nr <> nConstr.Nr) then exit;
   if Essentials.fCount = 0 then begin Matches:=true; exit end;
   InitEssentialsArgs(aArgs,Essentials);
   if CheckLociTypesN(PrimaryList) then
    Matches:=true;
  end;
end;

constructor ExpConstrDefObj.Init(const aConstr: Lexem; var aPrimaryList:MList;
                                 aPattern,aDef: PObject);
begin
 nConstr:=aConstr;
 nDefNr:= 0; nArticle:= '';
 PrimaryList.MoveList(aPrimaryList);
 nPattern:=aPattern;
 nExpansion:=aDef;
end;

destructor ExpConstrDefObj.Done;
begin
 inherited Done;
 if nExpansion <> nil then dispose(nExpansion,Done);
end;

constructor EqualsDefObj.Init(var aPrimArgs: MList; aFuncDef: TrmPtr);
begin
 nConstr.Kind:=ikTrmFunctor;
 nDefNr:= 0; nArticle:= '';
 PrimaryList.MoveList(aPrimArgs);
 nPattern:=nil;
 nExpansion:=aFuncDef;
 nEssentials.Init(0);
end;

destructor EqualsDefObj.Done;
begin
 nEssentials.Done;
 inherited Done;
end;

constructor IdentifyObj.Init(aAbsNr:integer; aArticle:string;
                              aKind: char; var aPrimaryList:MList;
                              aPattern1,aPattern2:ExprPtr;
                              var aEqArgs: IntRel);
begin
 nAbsNr:= aAbsNr;
 nArticle:= aArticle;
 nConstrKind:=aKind;
 nPrimaryList.MoveList(aPrimaryList);
 nPattern[0]:=aPattern1;
 nPattern[1]:=aPattern2;
 nEqArgs.CopyIntRel(aEqArgs);
end;

destructor IdentifyObj.Done;
begin
 nPrimaryList.Done;
 if nPattern[0] <> nil then dispose(nPattern[0],Done);
 if nPattern[1] <> nil then dispose(nPattern[1],Done);
 nEqArgs.Done;
end;

constructor ReductionObj.Init(aAbsNr:integer; aArticle:string;
                              var aPrimaryList:MList;
                              aTerm1,aTerm2:TrmPtr);
begin
 nAbsNr:= aAbsNr;
 nArticle:= aArticle;
 nPrimaryList.MoveList(aPrimaryList);
 nTerms[0]:=aTerm1;
 nTerms[1]:=aTerm2;
end;

destructor ReductionObj.Done;
begin
 nPrimaryList.Done;
 if nTerms[0] <> nil then dispose(nTerms[0],Done);
 if nTerms[1] <> nil then dispose(nTerms[1],Done);
end;

constructor PropertyObj.Init(aAbsNr:integer; aArticle:string;
                              var aPrimaryList:MList;
                              aPropKind:integer;
                              aObject:PObject);
begin
 nAbsNr:= aAbsNr;
 nArticle:= aArticle;
 nPrimaryList.MoveList(aPrimaryList);
 nPropertyKind:=aPropKind;
 nObject:=aObject;
end;

destructor PropertyObj.Done;
begin
 nPrimaryList.Done;
 if nObject <> nil then dispose(nObject,Done);
end;

{+    Kolekcjonowanie stalych w checkerze   +}

var ThereAreBound: boolean;
procedure CheckBound(var fTrm: TrmPtr);
begin
 if (fTrm^.TrmSort = ikTrmBound) and (VarTrmPtr(fTrm)^.VarNr <= BoundVarNbr)
  then ThereAreBound:=true;
end;

function CompConstDef(aDef1,aDef2: pointer):integer;
begin
 CompConstDef:=CompTrms(ConstDefPtr(aDef1)^.fDef,ConstDefPtr(aDef2)^.fDef);
end;

procedure CollectConstInTrmList(fTL:TrmList);
begin
 while fTL <> nil do
  begin
   CollectConstInTrm(fTL^.XTrmPtr);
   fTL:=fTL^.NextTrm;
  end;
end;

procedure CollectConstInAttr(aAttr: AttrPtr);
begin
 CollectConstInTrmList(aAttr^.fAttrArgs);
end;

procedure CollectConstInCluster(var aClu: AttrCollectionPtr);
  var lClusterPtr: AttrCollectionPtr;
      lAttr: AttrPtr;
      i: Integer;
begin
 lClusterPtr:=new(AttrCollectionPtr,Init(0,4));
 for i:=0 to aClu^.Count-1 do
  begin
   with AttrPtr(aClu^.Items^[i])^ do
    lAttr:=new(AttrPtr,Init(fNeg,fAttrNr,CopyTermList(fAttrArgs)));
   CollectConstInAttr(lAttr);
   lClusterPtr^.Insert(lAttr);
  end;
 dispose(aClu,Done);
 aClu:=lClusterPtr;
end;

Procedure CollectConstInTyp(aTyp: TypPtr);
begin
  with aTyp^ do
  if TypSort <> ikTypError then
  begin
   CollectConstInCluster(LowerCluster);
   CollectConstInCluster(UpperCluster);
   CollectConstInTrmList(ModArgs);
  end;
end;

var gCollConstLevel: integer = 0;

function CollectInferConst(fTrm:TrmPtr):TrmPtr;
  var i,lFunc: integer;
      lTyp: TypPtr;
      lArgs: TrmList;
      lTrm1,lTrm2: TrmPtr;
      lClusterPtr: AttrCollectionPtr;
      lConstDefObj: ConstDefObj;
      lConstDef,lVal1,lVal2: ConstDefPtr;
 begin CollectInferConst:=fTrm;
  inc(gCollConstLevel);
  if gOnlyConstants then
   begin
    lConstDefObj.fDef:=fTrm;
    if InferConstDef.Find(@lConstDefObj,i) then
      begin CollectInferConst:=NewVarTrm(ikTrmInfConst,InferConstDef.fIndex^[i]);
       DisposeTrm(fTrm);
       dec(gCollConstLevel);
       exit;
      end;
    lTyp:=RoundUpTrmType(fTrm);
//    CollectConstInTyp(lTyp);
    CollectInferConst:=NewVarTrm(ikTrmInfConst,InferConstDef.Count);
    lClusterPtr:=CopyCluster(lTyp^.UpperCluster);
    lClusterPtr^.RoundUpWith(lTyp);
    dispose(lTyp^.UpperCluster,Done);
    lTyp^.UpperCluster:=lClusterPtr;
    lConstDef:=new(ConstDefPtr,Init(fTrm,lTyp));
    case fTrm^.TrmSort of
     ikTrmNumeral:
      begin
       lConstDef^.fDetermined:=true;
       lConstDef^.fNumericValue:=IntToComplex(VarTrmPtr(fTrm)^.VarNr);
      end;
     ikTrmFunctor:
      begin
       AdjustTrm(fTrm,lFunc,lArgs);
       case gRevReq[lFunc] of
        rqImaginaryUnit:
         begin
          lConstDef^.fDetermined:=true;
          lConstDef^.fNumericValue:=CImUnit;
         end;
        rqRealAdd, rqRealMult, rqRealDiff, rqRealDiv:
         begin
          lTrm1:=lArgs^.XTrmPtr;
          lTrm2:=lArgs^.NextTrm^.XTrmPtr;
          while lTrm1^.TrmSort=ikTrmPrivFunc do
           if LocFuncTrmPtr(lTrm1)^.FuncExp^.TrmSort=ikError then break
            else lTrm1:=LocFuncTrmPtr(lTrm1)^.FuncExp;
          while lTrm2^.TrmSort=ikTrmPrivFunc do
           if LocFuncTrmPtr(lTrm2)^.FuncExp^.TrmSort=ikError then break
            else lTrm2:=LocFuncTrmPtr(lTrm2)^.FuncExp;
          lVal1:=InferConstDef.Items^[VarTrmPtr(lTrm1)^.VarNr];
          lVal2:=InferConstDef.Items^[VarTrmPtr(lTrm2)^.VarNr];
          if lVal1^.fDetermined and lVal2^.fDetermined then
           begin lConstDef^.fDetermined:=true;
            case gRevReq[lFunc] of
            rqRealAdd:
             lConstDef^.fNumericValue:=ComplexAdd(lVal1^.fNumericValue,lVal2^.fNumericValue);
            rqRealMult:
             lConstDef^.fNumericValue:=ComplexMult(lVal1^.fNumericValue,lVal2^.fNumericValue);
            rqRealDiff:
             lConstDef^.fNumericValue:=ComplexSub(lVal1^.fNumericValue,lVal2^.fNumericValue);
            rqRealDiv:
             if not IsEqWithInt(lVal2^.fNumericValue,0) then
              lConstDef^.fNumericValue:=ComplexDiv(lVal1^.fNumericValue,lVal2^.fNumericValue)
             else lConstDef^.fDetermined:=false;
            end;
           end;
         end;
        rqRealNeg, rqRealInv:
         begin
          lTrm1:=lArgs^.XTrmPtr;
          while lTrm1^.TrmSort=ikTrmPrivFunc do
           if LocFuncTrmPtr(lTrm1)^.FuncExp^.TrmSort=ikError then break
            else lTrm1:=LocFuncTrmPtr(lTrm1)^.FuncExp;
          lVal1:=InferConstDef.Items^[VarTrmPtr(lTrm1)^.VarNr];
          if lVal1^.fDetermined then
           begin lConstDef^.fDetermined:=true;
            case gRevReq[lFunc] of
            rqRealNeg:
             lConstDef^.fNumericValue:=ComplexNeg(lVal1^.fNumericValue);
            rqRealInv:
             if not IsEqWithInt(lVal1^.fNumericValue,0) then
              lConstDef^.fNumericValue:=ComplexInv(lVal1^.fNumericValue)
             else lConstDef^.fDetermined:=false;
            end;
           end;
         end;
       end;
      end;
    end;
    InferConstDef.Insert(lConstDef);
    CollectConstInTyp(lConstDef^.fTyp);
   end;
  dec(gCollConstLevel);
 end;

procedure InitEssentialsArgs(aArgs:TrmList; const aEssentials:IntSequence);
 var k: integer; lTrmList: TrmList;
begin
 gTrmList:=CopyTermList(aArgs); lTrmList:=gTrmList;
 for k:=0 to aEssentials.fCount-1 do
   InsertArgument(aEssentials.fList^[k]);
 Mizassert(2553,gTrmList=nil);
 DisposeListOfTerms(lTrmList);
end;

function ExpandTrmIfEqual(aFuncDef: EqualsDefPtr; aArgs: TrmList): TrmPtr;
begin
 ExpandTrmIfEqual:=nil;
 with aFuncDef^ do
  begin
   InitEssentialsArgs(aArgs,nEssentials);
   if CheckLociTypesN(PrimaryList) then
     ExpandTrmIfEqual:=InstSubstTrm(TrmPtr(nExpansion))
   else DisposeSubstTrm;
  end;
end;

function is_SethoodType(aTyp:TypPtr): boolean;
 var i: integer;
     lTyp: TypPtr;
begin is_SethoodType:=false;
 for i:=0 to gPropertiesList.Count-1 do
  with PropertyPtr(gPropertiesList.Items^[i])^ do
   if (nPropertyKind = ord(sySethood)) and
      TypReachable( TypPtr(nObject), aTyp) then
    begin fillchar(gSubstTrm,sizeof(gSubstTrm),0);
     lTyp:=TypPtr(nObject)^.WideningOf(aTyp^.CopyType);
     if lTyp <> nil then
      begin
       if CompEsTyp(TypPtr(nObject),lTyp,false) and
          TypPtr(nObject)^.LowerCluster^.IsSubsetOf(lTyp^.UpperCluster,EsAttrRev) and
          CheckLociTypes(nPrimaryList) then
        begin
         is_SethoodType:=true;
         dispose(lTyp,Done);
         exit;
        end;
       dispose(lTyp,Done);
      end;
    end;
end;

function FrOpVarTypeOK(fTyp: TypPtr): boolean;
 var i: integer;
// var lTyp1,lTyp2:TypPtr;
// label 12;
begin FrOpVarTypeOK:=false;
(* lTyp1:=fTyp^.CopyType;
 while (lTyp1^.TypSort = ikTypMode) and (lTyp1^.ModNr >= gBuiltIn[rqElement]) do
  with lTyp1^ do
   begin
    if (ModNr = gBuiltIn[rqElement]) or (OriginalNr( coMode,ModNr)=gBuiltIn[rqElement]) then
     begin FrOpVarTypeOK:=true; goto 12 end;
    lTyp2:=lTyp1^.Widening;
    dispose(lTyp1,Done); lTyp1:=lTyp2;
   end;
12:
 dispose(lTyp1,Done);
*)
 if (fTyp^.TypSort = ikTypMode) and
    (sySethood in ConstrTypPtr(Constr[coMode].Items^[fTyp^.ModNr])^.fProperties) then
   begin
    FrOpVarTypeOK:=true;
   end
  else
   begin
    FrOpVarTypeOK:=is_SethoodType(fTyp);
   end;
end;

var gEqualsExpansionLevel: integer = 0;

procedure CollectEqualsComst(aTrm: TrmPtr; var aEqConst:NatSet);
 var i,k,lFunc,lBoundvarNbr: integer;
     lLexem: Lexem;
     lLexemPtr: MLexemAndListPtr;
     lArgs: TrmList;
     lTrm: TrmPtr;
     lWideningTyps: boolean;
begin
  inc(gCollConstLevel);
  aEqConst.Init(0,4);
  AdjustTrm(aTrm,lFunc,lArgs);
  if (gEqualsExpansionLevel >= 3) or InferConsts.IsInSet(lFunc) then
   begin
//   Maximal equals expansion level is checked here
    dec(gCollConstLevel);
    exit;
   end;
  lLexem.Kind:=ikTrmFunctor;
  lLexem.Nr:=lFunc;
//   Equals expansion
  lLexemPtr:=MLexemAndListPtr(gEquals.ObjectOf(lLexem));
  if lLexemPtr <> nil then
  begin
   for i:=0 to lLexemPtr^.fList.Count-1 do
    with EqualsDefPtr(lLexemPtr^.fList.Items^[i])^ do
    begin
     lTrm:=ExpandTrmIfEqual(lLexemPtr^.fList.Items^[i],lArgs);
     if lTrm <> nil then
      begin
       ExpPrivFuncInTrm(lTrm);
       inc(gEqualsExpansionLevel);
       InferConsts.Insert(lFunc);
       lBoundvarNbr:=BoundvarNbr;
       BoundvarNbr:=0;
       CollectConstInTrm(lTrm);
       BoundvarNbr:=lBoundvarNbr;
       dec(gEqualsExpansionLevel);
       InferConsts.DeleteInt(lFunc);
       mizassert(4383,lTrm^.TrmSort = ikTrmInfConst);
       aEqConst.InsertElem(VarTrmPtr(lTrm)^.VarNr);
       DisposeTrm(lTrm);
      end
    end;
  end;
//   Identify with equals expansion
  lLexemPtr:=MLexemAndListPtr(gFuncIds.ObjectOf(lLexem));
  if lLexemPtr <> nil then
  begin
   for i:=0 to lLexemPtr^.fList.Count-1 do
    with IdentifyPtr(lLexemPtr^.fList.Items^[i])^ do
    begin
     fillchar(gSubstTrm,sizeof(gSubstTrm),0);
     if EsTrm(TrmPtr(nPattern[0]),aTrm) and
        CheckLociTypes(nPrimaryList) then
      begin
       lWideningTyps:=true;
       for k:=0 to nEqArgs .Count-1 do
        with nEqArgs.Items^[k] do
         begin
          mizassert(3931,gSubstTrm[Y]=nil);
          if not TypPtr(nPrimaryList.Items^[Y-1])^.IsWiderThan(TypPtr(nPrimaryList.Items^[X-1])^.CopyType) then
           begin lWideningTyps:=false; break end;
          gSubstTrm[Y]:=CopyTerm(gSubstTrm[X]);
         end;
       if lWideningTyps then
       begin
         lTrm:=InstSubstTrm(TrmPtr(nPattern[1]));
         ExpPrivFuncInTrm(lTrm);
         inc(gEqualsExpansionLevel);
         InferConsts.Insert(lFunc);
         lBoundvarNbr:=BoundvarNbr;
         BoundvarNbr:=0;
         CollectConstInTrm(lTrm);
         BoundvarNbr:=lBoundvarNbr;
         dec(gEqualsExpansionLevel);
         InferConsts.DeleteInt(lFunc);
         mizassert(4383,lTrm^.TrmSort = ikTrmInfConst);
         aEqConst.InsertElem(VarTrmPtr(lTrm)^.VarNr);
         DisposeTrm(lTrm);
       end;
      end
    end;
  end;
  dec(gCollConstLevel);
end;

procedure CollectConstInTrm(var aTrm:TrmPtr);
 var lOnlyConstants: boolean;
     i,lEqualsExpansionLevel: integer;
     lTrm: TrmPtr;
     lEqConst: NatSet;
     lConstDefObj: ConstDefObj;
begin lOnlyConstants:=gOnlyConstants; gOnlyConstants:=true;
  lEqualsExpansionLevel:=gEqualsExpansionLevel;
  gEqualsExpansionLevel:=0;
  inc(gCollConstLevel);
  case aTrm^.TrmSort of
   ikTrmFunctor:
   with FuncTrmPtr(aTrm)^ do
    begin
     CollectConstInTrmList(FuncArgs);
     if gOnlyConstants then
      begin
       lConstDefObj.fDef:=aTrm;
       if InferConstDef.Find(@lConstDefObj,i) then
        begin
         DisposeTrm(aTrm);
         aTrm:=NewVarTrm(ikTrmInfConst,InferConstDef.fIndex^[i]);
        end
       else
        begin
         aTrm:=CollectInferConst(aTrm);
         gEqualsExpansionLevel:=lEqualsExpansionLevel;
         CollectEqualsComst(ConstDefPtr(InferConstDef.Items^[VarTrmPtr(aTrm)^.VarNr])^.fDef,lEqConst);
         ConstDefPtr(InferConstDef.Items^[VarTrmPtr(aTrm)^.VarNr])^.fEqConst.EnlargeBy(lEqConst);
         lEqConst.Done;
       end;
      end;
    end;
   ikTrmSelector,ikTrmAggreg,ikTrmSchFunc:
    begin
     CollectConstInTrmList(FuncTrmPtr(aTrm)^.FuncArgs);
     aTrm:=CollectInferConst(aTrm);
    end;
   ikTrmPrivFunc:
    with LocFuncTrmPtr(aTrm)^ do
    begin
     CollectConstInTrmList(FuncArgs);
     CollectConstInTrm(FuncExp);
    end;
   ikTrmFraenkel:
    begin
     with FraenkelTrmPtr(aTrm)^ do
      begin
       for i:=0 to LambdaArgs.Count-1 do
        begin
         inc(BoundvarNbr);
         CollectConstInTyp(TypPtr(LambdaArgs.Items^[i]));
        end;
       CollectConstInTrm(LambdaScope);
       CollectConstInFrm(Compr);
       dec(BoundvarNbr,LambdaArgs.Count);
       ThereAreBound:=false;
       WithInTerm(aTrm,CheckBound);
       gOnlyConstants:=not ThereAreBound;
       if gOnlyConstants then
        begin
         WithInTerm(aTrm,ChChangeBound);
         aTrm:=CollectInferConst(aTrm);
        end;
      end;
    end;
   ikTrmChoice:
    with ChoiceTrmPtr(aTrm)^ do
    begin
     CollectConstInTyp(ChoiceTyp);
     aTrm:=CollectInferConst(aTrm);
    end;
   ikTrmNumeral:
    aTrm:=CollectInferConst(aTrm);
   ikTrmConstant:
    begin
     lEqConst.Init(0,4);
     with VarTrmPtr(aTrm)^ do
      if (FixedVar[VarNr].nDef <> nil) and
         (FixedVar[VarNr].nDef^.TrmSort <> ikTrmError) then
      begin
       lTrm:=CopyExpTrm(FixedVar[VarNr].nDef);
       CollectConstInTrm(lTrm);
       if gOnlyConstants then
        begin
         mizassert(4382,lTrm^.TrmSort  = ikTrmInfConst);
         lEqConst.InsertElem(VarTrmPtr(lTrm)^.VarNr);
         DisposeTrm(lTrm);
        end;
       gOnlyConstants:=true;
      end;
     aTrm:=CollectInferConst(aTrm);
     ConstDefPtr(InferConstDef.Items^[VarTrmPtr(aTrm)^.VarNr])^.fEqConst.EnlargeBy(lEqConst);
     if lEqConst.Count > 0 then
      ConstDefPtr(InferConstDef.Items^[VarTrmPtr(aTrm)^.VarNr])^.nSetting:=lEqConst.Items^[0].X;
     lEqConst.Done;
    end;
   ikTrmLocus,ikTrmFreeVar,ikTrmLambdaVar,ikError,ikTrmBound:
    begin
     gOnlyConstants:=false;
    end;
   ikTrmInfConst:;
   else { ikTrmExactly, ikTrmQua, ikTrmIt, and ikTrmEqConst cannot occur at checker ! }
    begin
{$IFDEF MDEBUG}
InfoString('TrmSort='); InfoChar(aTrm^.TrmSort); flush(InfoFile);
{$ENDIF}
    RunTimeError(2845);
   end;
  end;
  gOnlyConstants:=lOnlyConstants and gOnlyConstants;
  gEqualsExpansionLevel:=lEqualsExpansionLevel;
  dec(gCollConstLevel);
end;

procedure CollectConstInTypList(var aColl: MList);
 var i: integer;
begin
  for i:=0 to aColl.Count-1 do
   CollectConstInTyp(TypPtr(aColl.Items^[i]));
end;

procedure CollectConstInFrm(aFrm: FrmPtr);
begin
  case aFrm^.FrmSort of
   ikFrmConj:
    CollectConstInFrmCollection(ConjFrmPtr(aFrm)^.Conjuncts);
   ikFrmUniv:
    begin
     inc(BoundVarNbr);
     CollectConstInTyp(UnivFrmPtr(aFrm)^.Quantified);
//     BoundVar[BoundVarNbr]:=UnivFrmPtr(aFrm)^.Quantified;
     CollectConstInFrm(UnivFrmPtr(aFrm)^.Scope);
     dec(BoundVarNbr);
    end;
   ikFrmQual:
    begin
     CollectConstInTrm(QualFrmPtr(aFrm)^.QualTrm);
     CollectConstInTyp(QualFrmPtr(aFrm)^.QualTyp);
    end;
   ikFrmAttr,
   ikFrmSchPred,ikFrmPred:
    CollectConstInTrmList(PredFrmPtr(aFrm)^.PredArgs);
   ikFrmPrivPred:
    begin
     CollectConstInTrmList(LocPredFrmPtr(aFrm)^.PredArgs);
     CollectConstInFrm(LocPredFrmPtr(aFrm)^.PredExp);
    end;
   ikFrmNeg:
     CollectConstInFrm(NegFrmPtr(aFrm)^.NegArg);
   ikFrmFlexConj:
    begin
     CollectConstInFrm(FlexFrmPtr(aFrm)^.nLeftOrigFrm);
     CollectConstInFrm(FlexFrmPtr(aFrm)^.nRightOrigFrm);
     CollectConstInFrm(FlexFrmPtr(aFrm)^.nExpansion);
    end;
   ikFrmVerum: ;
   ikFrmError: ;
   else
    begin
{$IFDEF MDEBUG}
writeln(infofile,'CollectConstInFrm');
InfoChar(aFrm^.FrmSort);
{$ENDIF}
     RunTimeError(2843);
    end;
   end;
 end;

procedure CollectConstInFrmCollection(var aColl: MList);
 var i: integer;
begin
  for i:=0 to aColl.Count-1 do
   CollectConstInFrm(FrmPtr(aColl.Items^[i]));
end;

end.
