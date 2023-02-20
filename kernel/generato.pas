(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit generato;

interface

uses errhan,inout,limits,correl,mobjects,express,enums,identify;

type
 RSNENTRY = ^RSNREC;
 RSNREC =
  record PreviousEntry: RSNENTRY;
   case FORM: char of
    'D','C': (SkList: MCollection; // types of local consts
              SkIdents: IntSequence;// identifier of local consts
              SkOrigTyps: MCollection; // types of local consts before generalizing
              SkFrstConstNr: integer); // nr of the first local const (needed when generalizing the scope)
    'A','B': (SkSnt,                // skeleton formula after generalizing constants
              DSnt: FrmPtr);        // skeletong fla before generalizing constants
  end;

 LevelRec =
  record
   LocPredNbr,LocFuncNbr,
   VarNbr,SkelConstNbr,DemBase,GenCount:integer;
   LastEntry: RSNENTRY;
   Err:boolean;
   Thesis:FrmPtr;
  end;

          { listy zmiennych wolnych }

   ResDesPtr = ^ResDesNode;

   ResDes = record ResNr: integer; Subst: MCollection end;

          { obiekty ze zmiennymi wolnymi }

   Sigma = ^SigmaRec;
   Kappa = ^KappaRec;
   Theta = ^ThetaRec;

   VarScope = object(MObject)
      nFreeVars: MCollection;
      nExpressionPtr: ExpPtr;
   end;
   SigmaRec  = object(VarScope)
     constructor Init;
     destructor Done; virtual;
   end;
   ThetaRec  = object(VarScope)
     constructor Init;
   end;
   KappaRec = object(VarScope)
      nBoundVars: MCollection; {teraz jest to kolekcja multipleTypePtr}
     constructor Init(fTrm: ExpPtr);
     constructor InitWithBound(var fList:MCollection; fTrm: ExpPtr);
   end;

                 { konstruktory }

   AttrNodePtr = ^AttrNode;
   AttrNode =
    object(MObject)
      nInt: integer;
      nPos:Position;
      nNeg: boolean;
      Argumenty: MCollection;
     constructor Init(fi:integer; fPos:Position; fNeg:boolean);
     destructor Done; virtual;
   end;

   StandardExpression = object(Expression)
       nConstrNr: integer;
       Argumenty: MCollection;
      constructor Init(fSort:char; fPos:Position; fNr,fBase:integer);
      destructor Done; virtual;
    end;

   AttributedExpression = object(Expression)
       Podmiot: ExpPtr;
       Przymiotniki: MList;
      constructor Init(fSort:char; fSubject:ExpPtr; fPos:Position; var fList:MList);
      destructor Done; virtual;
    end;

   QualifiedExpression = object(Expression)
       Obiekt,Kwalifikacja: ExpPtr;
      constructor Init(fSort:char; fTrm:ExpPtr; fTyp:ExpPtr; fPos:Position);
      destructor Done; virtual;
    end;

   ResDesNode = object(StandardExpression)
      nIdent: integer;
     constructor Init(fi:integer; const fColl:MCollection);
     constructor InitResNr(fi:integer);
     function Analyze: pointer; virtual;
    end;

   SimpleTerm = object(Expression)
       VarNr: integer;
      constructor Init(fKind:char; fNr:integer);
      function Analyze: pointer; virtual;
     end;
   NumeralTerm = object(Expression)
       nNatVal: integer;
      constructor Init(fNatVal:integer);
      function Analyze: pointer; virtual;
     end;
   FunctorTerm =
    object(StandardExpression)
      constructor InitWithArgs(fTrmSort:char; fPos:Position; fFuncNr:integer;
                               var fFuncArgs:MCollection);
      function Analyze: pointer; virtual;
     end;
   SelectorTerm = object(Expression)
       Select: integer;
       Struct: ExpPtr;
      constructor Init(fSelect:integer; fPos:Position; fStruct: ExpPtr);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
     end;
   SubAggrTerm = object(Expression)
       SubAggr: integer;
       FullAggr: ExpPtr;
      constructor Init(fSubAggr:integer; fPos:Position; fFullAggr: ExpPtr);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
   end;
   FraenkelTerm = object(Expression)
       Sample: Kappa;
       Compr: Sigma;
      constructor Init(fSample: Kappa; fCompr:Sigma; fPos:Position);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
   end;
   QualifiedTerm = object(QualifiedExpression)
      function Analyze: pointer; virtual;
   end;
   ChoiceTerm = object(EXpression)
       ChoiceType: ExpPtr;
      constructor Init(aPos:Position; aTyp: ExpPtr);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
   end;
   ItTerm = object(Expression)
      function Analyze: pointer; virtual;
      constructor Init;
   end;
   InCorrTerm = object(Expression)
      function Analyze: pointer; virtual;
      constructor Init;
   end;

   SimpleTermPtr = ^SimpleTerm;
   NumeralTermPtr = ^NumeralTerm;
   FunctorTermPtr = ^FunctorTerm;
   SelectorTermPtr = ^SelectorTerm;
   SubAggrTermPtr = ^SubAggrTerm;
   FraenkelTermPtr = ^FraenkelTerm;
   QualifiedTermPtr = ^QualifiedTerm;
   ChoiceTermPtr = ^ChoiceTerm;
   ItTermPtr = ^ItTerm;
   InCorrTermPtr = ^InCorrTerm;

   AttributedType = object(AttributedExpression)
     constructor Init(fSort:char; fSubject:ExpPtr; fPos:Position;
                      var fList:MList; fFrom, fTo: integer);
     function Analyze: pointer; virtual;
    end;
   TypeExp = object(StandardExpression)
     function Analyze: pointer; virtual;
   end;
   InCorrType = object(Expression)
     function Analyze: pointer; virtual;
     constructor Init;
   end;

   multipleTypeExp = object(MObject)
        nNumberOfCopies: integer; // nr of identifiers (= nIdent.fCount)
        nType: ExpPtr;
        nIdents: IntSequence;  // spelling of identifiers
     constructor Init(fType:ExpPtr; const fIdents:IntSequence);
     destructor Done; virtual;
   end;

   AttributedTypePtr = ^AttributedType;
   TypePtr = ^TypeExp;
   InCorrTypePtr = ^ InCorrType;
   multipleTypePtr = ^multipleTypeExp;

   PredicateFormula = object(StandardExpression)
      function Analyze: pointer; virtual;
   end;
   MultiPredicateFormula = object(Expression)
       Argumenty: MCollection;
       LeftArgsCount: integer;
      constructor Init(fPos:Position);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
   end;
   AttributiveFormula = object(AttributedExpression)
      function Analyze: pointer; virtual;
   end;
   QualifyingFormula = object(QualifiedExpression)
      function Analyze: pointer; virtual;
   end;
   NegativeFormula = object(Expression)
       NegArg: ExpPtr;
      constructor Init(fArg:ExpPtr);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
   end;
   BinaryFormula = object(Expression)
       BinArg1,BinArg2: ExpPtr;
      constructor Init(fch:char; fArg1,fArg2:ExpPtr);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
    end;
   QuantifiedFormula = object(Expression)
       QuantVars: IntSequence;
       Quantified,Scope: ExpPtr;
      constructor Init(fSort:char; fQuantNbr:integer; fTyp:ExpPtr; fScope:ExpPtr);
      destructor Done; virtual;
      function Analyze: pointer; virtual;
   end;
   VerumFormula = object(Expression)
      function Analyze: pointer; virtual;
      constructor Init;
    end;
   ThesisFormula = object(Expression)
      function Analyze: pointer; virtual;
      constructor Init;
   end;
   InCorrFormula = object(Expression)
      function Analyze: pointer; virtual;
      constructor Init;
   end;

   PredicateFormulaPtr = ^PredicateFormula;
   MultiPredicateFormulaPtr = ^MultiPredicateFormula;
   QualifyingFormulaPtr = ^QualifyingFormula;
   AttributiveFormulaPtr = ^AttributiveFormula;
   NegativeFormulaPtr = ^NegativeFormula;
   BinaryFormulaPtr = ^BinaryFormula;
   QuantifiedFormulaPtr = ^QuantifiedFormula;
   VerumFormulaPtr = ^VerumFormula;
   ThesisFormulaPtr = ^ThesisFormula;
   IncorrFormulaPtr = ^IncorrFormula;

  Variables = array[1..MaxVarNbr] of
  record VarIdent:integer;
   Variable:Lexem;
   VarPos:Position;
   Default : ResDes;
   Pending: integer;
   nLocus:boolean;
  end;

                   { generatory }

var g: LevelRec;
 gExportableItem: boolean =false;
 gConstInExportableItemOcc: boolean =false;
 gFraenkelTermAllowed: boolean = true;
 TermNbr: integer;
 Term: array[1..MaxSubTermNbr] of ExpPtr;
 SchFuncArity: array[1..MaxFuncVarNbr] of
  record nId: integer;
   nArity: MList;
  end;
 SchPredArity: array[1..MaxPredVarNbr] of
  record nId: integer;
   nArity: MList;
  end;
 ReservedVar: array[1..MaxResNbr] of TypPtr;

procedure InsArgs(fBase:integer; var fList:MCollection);
function AnalyzeAttribute(aItem:AttrNodePtr; fTyp:TypPtr):AttrPtr;
procedure AnalyzeAttributes(fPos:Position; fTyp:TypPtr; const fAttrs: MList);

procedure LoadIPNColl(var fColl:MList);

function LoadTerm:ExpPtr;
function LoadType:ExpPtr;
function LoadFormula:ExpPtr;

var
 Notat: array[NotationKind] of MExtList;
 NotatBase: array[NotationKind] of integer;

implementation

uses lexicon,builtin
{$IFDEF MDEBUG}
 ,info,outinfo
{$ENDIF} ;

constructor multipleTypeExp.Init(fType:ExpPtr; const fIdents:IntSequence);
begin
 nType:=fType;
 nNumberOfCopies:= fIdents.fCount;
 nIdents.CopySequence(fIdents);
end;

destructor multipleTypeExp.Done;
begin
 dispose(nType,Done);
 nIdents.Done;
end;

constructor AttrNode.Init(fi:integer; fPos:Position; fNeg:boolean);
begin //!
 nInt:=fi; nPos:=fPos; nNeg:=fNeg;
 Argumenty.Init(0,2);
end;

destructor AttrNode.Done;
begin
 Argumenty.Done;
end;

procedure InsArgs(fBase:integer; var fList:MCollection);
 var k:integer;
begin fList.Init(TermNbr-fBase+1,2);
 for k:=fBase to TermNbr do fList.Insert(Term[k]);
 TermNbr:=fBase-1;
end;

constructor SimpleTerm.Init(fKind:char; fNr:integer);
begin inherited InitExp(fKind);
 VarNr:=fNr;
end;

constructor NumeralTerm.Init(fNatVal:integer);
begin ExpSort:=ikTrmNumeral; nNatVal:=fNatVal end;

constructor StandardExpression.Init(fSort:char; fPos:Position;
                                    fNr,fBase:integer);
begin ExpSort:=fSort;
 ExpSort:=fSort; ExpPos:=fPos; nConstrNr:=fNr;
 InsArgs(fBase,Argumenty);
end;

destructor StandardExpression.Done;
begin
 Argumenty.Done;
end;

constructor FunctorTerm.InitWithArgs(fTrmSort:char; fPos:Position;
                                     fFuncNr:integer; var fFuncArgs:MCollection);
begin ExpSort:=fTrmSort;
 ExpSort:=fTrmSort; ExpPos:=fPos; nConstrNr:=fFuncNr;
 Argumenty.MoveCollection(fFuncArgs);
end;

constructor SelectorTerm.Init(fSelect:integer; fPos:Position; fStruct: ExpPtr);
begin ExpSort:=ikTrmSelector; ExpPos:=fPos; Select:=fSelect; Struct:=fStruct end;

destructor SelectorTerm.Done;
begin dispose(Struct,Done) end;

constructor SubAggrTerm.Init(fSubAggr:integer; fPos:Position; fFullAggr: ExpPtr);
begin ExpSort:=ikTrmSubAgreg; ExpPos:=fPos; SubAggr:=fSubAggr;
 FullAggr:=fFullAggr;
end;

destructor SubAggrTerm.Done;
begin dispose(FullAggr,Done);
end;

constructor FraenkelTerm.Init(fSample: Kappa; fCompr:Sigma; fPos:Position);
begin ExpSort:=ikTrmFraenkel;
 Sample:=fSample; Compr:=fCompr;
 ExpPos:=fPos;
end;

constructor ChoiceTerm.Init(aPos:Position; aTyp: ExpPtr);
begin ExpSort:=ikTrmChoice; ExpPos:=aPos;
 ChoiceType:=aTyp;
end;

destructor ChoiceTerm.Done;
begin dispose(ChoiceType,Done);
end;

constructor ItTerm.Init;
begin ExpSort:=ikTrmIt end;

constructor InCorrTerm.Init;
begin inherited InitExp(ikError) end;

destructor FraenkelTerm.Done;
begin
 with Sample^ do
  begin {nFreeVars.Done;}
   nBoundVars.Done;
   dispose(nExpressionPtr,Done);
  end;
 dispose(Sample);
 dispose(Compr^.nExpressionPtr,Done);
 dispose(Compr);
end;

constructor QualifiedExpression.Init(fSort:char; fTrm:ExpPtr; fTyp:ExpPtr; fPos:Position);
begin ExpSort:=fSort;
 Obiekt:=fTrm; Kwalifikacja:=fTyp; ExpPos:=fPos
end;

destructor QualifiedExpression.Done;
begin dispose(Obiekt,Done); dispose(Kwalifikacja,Done) end;

constructor NegativeFormula.Init(fArg:ExpPtr);
begin inherited InitExp(ikFrmNeg);
 NegArg := fArg;
end;

destructor NegativeFormula.Done;
begin dispose(NegArg,Done) end;

constructor BinaryFormula.Init(fch:char; fArg1,fArg2:ExpPtr);
begin inherited InitExp(fCh);
 Binarg1 := fArg1; Binarg2 := fArg2;
end;

destructor BinaryFormula.Done;
begin
 dispose(Binarg1,Done); dispose(Binarg2,Done);
end;

constructor QuantifiedFormula.Init(fSort:char; fQuantNbr: integer; fTyp:ExpPtr; fScope: ExpPtr);
begin inherited InitExp(fSort);
 QuantVars.Init(0);
 Quantified:=fTyp; Scope:=fScope;
end;

destructor QuantifiedFormula.Done;
begin
 QuantVars.Done;
 dispose(Quantified,Done); dispose(Scope,Done);
end;

constructor ResDesNode.Init(fi:integer; const fColl:MCollection);
begin ExpSort:=ikExpResDes; ExpPos:=CurPos;
 nConstrNr:=fi;
 Argumenty.Init(0,0); Argumenty:=fColl;
 nIdent:=0;
end;

constructor ResDesNode.InitResNr(fi:integer);
begin ExpSort:=ikExpResDes; nConstrNr:=fi;
 nIdent:=0;
end;

constructor SigmaRec.Init;
begin nExpressionPtr:=nil end;

destructor SigmaRec.Done;
begin
 mizassert(2599,nFreeVars.Count=0);
 dispose(nExpressionPtr,Done);
end;

constructor ThetaRec.Init;
begin nExpressionPtr:=nil end;

constructor KappaRec.Init(fTrm: ExpPtr);
begin nBoundVars.Init(0,5); nExpressionPtr:=fTrm end;

constructor KappaRec.InitWithBound(var fList:MCollection; fTrm: ExpPtr);
begin
 nFreeVars.Init(0,4);
 nBoundVars.MoveCollection(fList); nExpressionPtr:=fTrm
end;

function ActualTyp(fArgs:TrmList; fOrigin,fTypNr:integer): TypPtr;
 var K,lConstr: integer;
begin ActualTyp:=nil;
 lConstr:=PatternPtr(Notat[noMode].Items^[fTypNr])^.fFormNr;
 for K:=Notat[noMode].Count-1 downto fTypNr do
  with PatternPtr(Notat[noMode].Items^[K])^ do
  if (fFormNr=lConstr) and (OriginalNr( coMode,rConstr.Nr)=fOrigin) then
  if CheckTypes(Notat[noMode].Items^[K],fArgs) then
   begin ActualTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                                    rConstr.Nr,CreateArgList(fPrimTypes.Count));
     DisposeListOfTerms(fArgs);
    exit;
   end;
 with PatternPtr(Notat[noMode].Items^[fTypNr])^ do
  if CheckTypes(Notat[noMode].Items^[fTypNr],fArgs) then
   begin
    ActualTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,NewEmptyCluster,
                               rConstr.Nr,CreateArgList(fPrimTypes.Count));
    DisposeListOfTerms(fArgs);
   end
  else RunTimeError(2112);
end;

function AnalyzeTermList(const fTL: MCollection): TrmList;
 var lTrm: TrmPtr; lTrmList: TrmList; Previous:^TrmList; OK: boolean;
     z: integer;
begin Previous:=addr(lTrmList); OK:=true;
 with fTL do
  for z:=0 to Count-1 do
  begin
   lTrm:=ExpPtr(Items^[z])^.Analyze;
   if lTrm^.TrmSort=ikError then OK:=false;
   new(Previous^); Previous^^.XTrmPtr:=lTrm;
   Previous:=addr(Previous^^.NextTrm);
  end;
 Previous^:=nil;
 if OK then AnalyzeTermList:=lTrmList
 else AnalyzeTermList:=InCorrTrmList;
end;

function AnalyzeAttribute(aItem:AttrNodePtr; fTyp:TypPtr): AttrPtr;
 var lTrmList,ltl,lCompleteArgs: TrmList; OriginLen,j,K,i,lAttrNr: integer;
begin AnalyzeAttribute:=nil;
 if fTyp^.TypSort = ikError then exit;
 IncIndex(BoundVarNbr,VarIndex);
 BoundVar[BoundVarNbr]:=fTyp;
 lTrmList:=AnalyzeTermList(aItem^.Argumenty);
 if lTrmList = InCorrTrmList then
  begin
   dec(BoundVarNbr);
   exit;
  end;
 if lTrmList <> nil then
  begin lTl:=LastElem(lTrmList);
    lTl^.NextTrm:=NewTrmList(NewVarTrm(ikTrmBound,BoundVarNbr),nil);
  end
 else lTrmList:=NewTrmList(NewVarTrm(ikTrmBound,BoundVarNbr),nil);
 for K:=Notat[noAttribute].Count-1 downto 0 do
  with PatternPtr(Notat[noAttribute].Items^[K])^ do
   if aItem^.nInt=fFormNr then
    begin
     if CheckTypes(Notat[noAttribute].Items^[K],lTrmList) then
      begin
       lAttrNr:=rConstr.Nr;
       if lAttrNr = 0 then break;
       with ConstrTypPtr( Constr[ coAttribute].At( rConstr.Nr))^ do
       begin
        if fWhichConstrNr<>0 then lAttrNr:=fWhichConstrNr;
        lCompleteArgs:=CreateArgList(fPrimTypes.Count-1);{###}
        for i:=1 to fSuperfluous do lCompleteArgs:=lCompleteArgs^.NextTrm;
        RemoveQua(lCompleteArgs);
        OriginLen:= nPrimaries.Count;
       end;
       for j:=1 to fPrimTypes.Count-OriginLen do
        begin
         ltl:=lCompleteArgs;
         lCompleteArgs:=lCompleteArgs^.NextTrm;
         DisposeTrm(ltl^.XTrmPtr);
         Dispose(ltl);
        end;
       AnalyzeAttribute:=new(AttrPtr,initP(ord(fAntonymic),lAttrNr,
                                        lCompleteArgs,K+1));
       dec(BoundVarNbr);
       lTl:=LastElem(lTrmList);
       DisposeTrm(lTl^.XTrmPtr);
       DisposeListOfTerms(lTrmList);
       exit;
      end;
    end;
 dec(BoundVarNbr);
 DisposeTrmList(lTrmList);
end;

procedure AnalyzeAttributes(fPos:Position; fTyp:TypPtr; const fAttrs: MList);
 var lClusterPtr: AttrCollectionPtr;
     lAttr: AttrPtr;
     lTyp: TypPtr;
     L,i,z:integer;
begin
 lTyp:=fTyp^.CopyType;
 with fAttrs do
  for z:=Count-1 downto 0 do
   if AttrNodePtr(Items^[z])^.nInt <> 0 then
   begin
    lAttr:=AnalyzeAttribute(AttrNodePtr(Items^[z]),lTyp);
    if lAttr = nil then
     begin Error(AttrNodePtr(Items^[z])^.nPos,115); continue end;
    if AttrNodePtr(Items^[z])^.nNeg then
     begin
      if lAttr^.fNeg = 0 then lAttr^.fNeg:=0
      else lAttr^.fNeg:=1
     end
    else if lAttr^.fNeg = 0 then lAttr^.fNeg:=1
     else lAttr^.fNeg:=0;
    lTyp^.LowerCluster^.Insert(lAttr^.CopyAttribute);
    if not lTyp^.LowerCluster^.fConsistent then
     begin Error(fPos,95);
      fTyp^.TypSort:=ikError;
      dispose(lTyp,Done);
      dispose(lAttr,Done); //!!
      exit
     end;
    lTyp^.UpperCluster^.Insert(lAttr{^.CopyAttribute});
    lTyp^.RoundUp;
   end;
 lClusterPtr:=CopyCluster(lTyp^.LowerCluster);
 for L:= RegisteredCluster.Count-1 downto 0 do
  with RClusterPtr(RegisteredCluster.Items^[L])^ do
   begin
     fillchar(gSubstTrm,sizeof(gSubstTrm),0);
     if lTyp^.LowerCluster^.IsSubsetOf(nConsequent.Upper,EsAttr) then
      if CompEsTyp(nClusterType,fTyp,false) then
      begin
       if CheckLociTypes(nPrimaryList) then
        begin
//         InitInst;
         lClusterPtr:=CopyCluster(lTyp^.LowerCluster);
         lClusterPtr^.EnlargeBy(fTyp^.UpperCluster);
//         StopInst;
         DisposeSubstTrm;
         lClusterPtr^.RoundUpWith(fTyp);
         if not lClusterPtr^.fConsistent then
          begin Error(fPos,95);
           dispose(lClusterPtr,Done);
           fTyp^.TypSort:=ikError;
          end
         else
          begin
           dispose(fTyp^.LowerCluster,Done);
           fTyp^.LowerCluster:=CopyCluster(lTyp^.LowerCluster);
           dispose(fTyp^.UpperCluster,Done);
           fTyp^.UpperCluster:=lClusterPtr;
          end;
         dispose(lTyp,Done);
         exit;
        end;
      end;
     DisposeSubstTrm;
   end;
 Error(fPos,136);
 dispose(lTyp,Done);
 fTyp^.TypSort:=ikError;
end;

constructor AttributedType.Init(fSort:char;fSubject:ExpPtr;
                                      fPos:Position;
                                      var fList:MList; ffrom, fTo: integer);
 var i: integer;
begin ExpSort:=fSort;
 Podmiot:=fSubject; ExpPos:=fPos;
 Przymiotniki.Init(fTo-fFrom+1);
 for i:=fFrom to fTo do
  Przymiotniki.Insert(PObject(fList.Items^[i])^.MCopy);
end;

function AttributedType.Analyze: pointer;
 var lTyp:TypPtr;
begin mizassert(2550,Przymiotniki.Count<>0);
 lTyp:=Podmiot^.Analyze;
 AnalyzeAttributes(Podmiot^.ExpPos,lTyp,Przymiotniki);
 Analyze:=lTyp;
end;

function TypeExp.Analyze: pointer;
 var lTyp: TypPtr; lTrmList,ltl,lModArgs: TrmList; OriginLen,i,K: integer;
     lClusterPtr,lClPtr: AttrCollectionPtr;
begin
 lTrmList:=AnalyzeTermList(Self.Argumenty);
 if (lTrmList = InCorrTrmList) or (Self.nConstrNr=0)
   then begin Analyze:=NewInCorTyp; exit end;
 case Self.ExpSort of
  ikTypMode:
   begin
    for K:=Notat[noMode].Count-1 downto 0 do
     with PatternPtr(Notat[noMode].Items^[K])^ do
     if Self.nConstrNr=fFormNr then
      if CheckTypes(Notat[noMode].Items^[K],lTrmList) then
       begin
        lModArgs:=CreateArgList(fPrimTypes.Count);
        RemoveQua(lModArgs);
        if Expansion=nil then
         with ConstrTypPtr( Constr[ coMode].At( rConstr.Nr))^ do
        begin
          OriginLen:= nPrimaries.Count;
          for i:=1 to fPrimTypes.Count-OriginLen do
           begin
            ltl:=lModArgs;
            lModArgs:=lModArgs^.NextTrm;
            DisposeTrm(ltl^.XTrmPtr);
            Dispose(ltl);
           end;
          lTyp:=NewStandardTyp(ikTypMode,NewEmptyCluster,
                    InstCluster(fConstrTyp^.UpperCluster,lModArgs),
                                rConstr.Nr,lModArgs);
         end
        else
         begin
          lTyp:=Expansion^.InstTyp(lModArgs);
          DisposeTrmList(lModArgs);
         end;
        lClusterPtr:=CopyCluster(lTyp^.UpperCluster);
        if (lTyp^.TypSort = ikTypMode) and (lTyp^.ModNr <> gBuiltIn[rqAny]) then
         with ConstrTypPtr( Constr[ coMode].At( lTyp^.ModNr))^ do
         begin
          lClPtr:=InstCluster(fConstrTyp^.UpperCluster,lTyp^.ModArgs);
          lClusterPtr^.EnlargeBy(lClPtr);
          dispose(lClPtr,Done);
         end;
        lClusterPtr^.RoundUpWith(lTyp);
        dispose(lTyp^.UpperCluster,Done);
        lTyp^.UpperCluster:=lClusterPtr;
        lTyp^.nPattNr:= K+1;
        Analyze:=lTyp;
        DisposeListOfTerms(lTrmList);
        exit;
       end;
    ExpError(101); Analyze:=NewInCorTyp;
   end;
  ikTypStruct:
   begin
    for K:=Notat[noStructMode].Count-1 downto 0 do
     with PatternPtr(Notat[noStructMode].Items^[K])^ do
     if Self.nConstrNr=fFormNr then
      if CheckTypes(Notat[noStructMode].Items^[K],lTrmList) then
       begin lModArgs:=CreateArgList(fPrimTypes.Count);
        RemoveQua(lModArgs);
        Analyze:=NewStandardTyp(ikTypStruct,NewEmptyCluster,NewEmptyCluster,
                                 rConstr.Nr,lModArgs);
        DisposeListOfTerms(lTrmList);
        exit;
       end;
    ExpError(104); Analyze:=NewInCorTyp;
   end;
 end;
end;

function InCorrType.Analyze: pointer;
begin Analyze:=NewInCorTyp end;

constructor IncorrType.Init;
begin inherited InitExp(ikError) end;

var
 ResSubst: array[1..MaxVarNbr] of Lexem;
 ResSubstNbr: integer;

procedure SubstVar(var fTrm:TrmPtr);
begin
 with VarTrmPtr(fTrm)^ do
  if (TrmSort=ikTrmBound) and (VarNr <= ResSubstNbr) then
   with ResSubst[VarNr] do begin TrmSort:=Kind; VarNr:=Nr end;
end;

function ResDesNode.Analyze: pointer;
 var lTyp: TypPtr; Failed:boolean; z: integer;
begin
 if nConstrNr<>0 then lTyp:=ReservedVar[nConstrNr]^.CopyType
  else lTyp:=NewInCorTyp;
 ResSubstNbr:=0; Failed:=false;
 with Argumenty do
  for z:=0 to Count-1 do
   with SimpleTermPtr(Items^[z])^ do
    begin inc(ResSubstNbr);
     ResSubst[ResSubstNbr].Kind:=ExpSort;
     ResSubst[ResSubstNbr].Nr:=VarNr;
     if VarNr = 0 then Failed:=true;
    end;
 if Failed then lTyp:=NewInCorTyp;
 if ResSubstNbr<>0 then lTyp^.WithinType(SubstVar);
 Analyze:=lTyp;
end;

function NegativeFormula.Analyze: pointer;
begin
 Analyze:=NewNegDis(NegArg^.Analyze);
end;

function BinaryFormula.Analyze: pointer;
 var lFrm: FrmPtr;
begin
 lFrm:=Binarg1^.Analyze;
 case ExpSort of
  ikFrmOr: Analyze:= NewDisj(lFrm,Binarg2^.Analyze);
  ikFrmImplies: Analyze:=NewImpl(lFrm,Binarg2^.Analyze);
  ikFrmIff: Analyze:=NewBicond(lFrm,Binarg2^.Analyze);
  ikFrmConj: Analyze:=NewConj(lFrm,BinArg2^.Analyze);
  ikFrmFlexConj: Analyze:=NewFlexConj(lFrm,BinArg2^.Analyze);
  ikFrmFlexDisj: Analyze:=NewFlexDisj(lFrm,BinArg2^.Analyze);
 end;
end;

var gQuantBase,gQuantInc: integer;

procedure ReBase(var fTrm:TrmPtr);
 begin
  with VarTrmPtr(fTrm)^ do
   if (TrmSort=ikTrmBound) and (VarNr > gQuantBase)
    then inc(VarNr,gQuantInc);
 end;

function QuantifiedFormula.Analyze: pointer;
 var lFrm:FrmPtr; lQuantBase,i:integer;
     lQuantified:TypPtr;
begin
 {----}
 MarkTermsInTTColl;
 {----}
 lQuantBase:=BoundVarNbr;
 inc(BoundVarNbr,QuantVars.fCount);
 lQuantified:=Quantified^.Analyze;
 Mizassert(2543,BoundVarNbr<=MaxVarNbr);
 gQuantBase:=lQuantBase;
 for i:=lQuantBase+1 to BoundVarNbr do
  begin gQuantInc:=i-lQuantBase-1;
   BoundVar[i]:=lQuantified^.CopyType;
   BoundVar[i]^.WithinType(ReBase);
  end;
 lFrm:=Scope^.Analyze;
 if ExpSort = ikFrmEx then
  lFrm:=NewNegDis(lFrm);
 for i:=BoundVarNbr downto lQuantBase+1 do
  lFrm:=NewUnivI(QuantVars.fList^[i-lQuantBase-1],BoundVar[i],lFrm);
 if ExpSort = ikFrmEx then
  lFrm:=NewNegDis(lFrm);
 Analyze:=lFrm;
 dispose(lQuantified,Done);
 BoundVarNbr:=lQuantBase;
 {----}
 RemoveTermsFromTTColl;
 {----}
end;


function AnalyzePredicateFormula(const ExpPos:Position;lTrmList:TrmList; nConstrNr:integer): FrmPtr;
 var i,k,lPredNr,OriginLen:integer;
     lPredArgs,ltl:TrmList;
     lFrm2:FrmPtr;
begin
  for k:=Notat[noPredicate].Count-1  downto 0 do
   with PatternPtr(Notat[noPredicate].Items^[k])^ do
   if fFormNr=nConstrNr then
   begin
    if CheckTypes(Notat[noPredicate].Items^[k],lTrmList) then
    begin
     lPredNr:=rConstr.Nr;
     if lPredNr = 0 then
      begin result:=NewIncorFrm; exit end;
     lPredArgs:=CreateArgList(fPrimTypes.Count);
     OriginLen:=ConstrPtr(Constr[coPredicate].At(rConstr.Nr))^.nPrimaries.Count;
     for i:=1 to fPrimTypes.Count-OriginLen do
      begin
       ltl:=lPredArgs;
       lPredArgs:=lPredArgs^.NextTrm;
       DisposeTrm(ltl^.XTrmPtr);
       Dispose(ltl);
      end;
     lFrm2:=NewPredFrm(ikFrmPred,lPredNr,lPredArgs,K+1);
     RemoveQua(lPredArgs);
     if  PatternPtr(Notat[noPredicate].Items^[k])^.fAntonymic then result:=NewNeg(lFrm2)
      else result:=lFrm2;
     exit;
    end;
   end;
  Error(ExpPos,102);
  result:=NewIncorFrm;
end;

function PredicateFormula.Analyze: pointer;
 var lPredArgs,ltl,lTrmList:TrmList; OriginLen,i,lPredNr,k:integer;
     lFrm1,lFrm2:FrmPtr;
begin
 case ExpSort of
  ikFrmPrivPred:
   begin
    if gExportableItem then
      ExpError(220);
    lTrmList:=AnalyzeTermList(Argumenty);
    if (lTrmList=InCorrTrmList) or (nConstrNr=0) then
     begin Analyze:=NewInCorFrm; exit end;
    if not Agree(lTrmList,LocPredDefPtr(LocPredDef.Items^[nConstrNr-1])^.fPrimaries) then
     begin ExpError(121); Analyze:=NewInCorFrm; exit end;
    RemoveQua(lTrmList);
    lFrm1:=LocPredDefPtr(LocPredDef.Items^[nConstrNr-1])^.fPredDef^.CopyFormula;
{ !!!!!!!!!!!! Nie zadziala z operatorem Fraenkla ? }
    WithinFormula(lFrm1,ChangeBound);
    Analyze:=NewLocPredFrm(nConstrNr,lTrmList,InstFrm(lFrm1,lTrmList));
//!!     Analyze:=InstFrm(lFrm1,lTrmList);  // linia do wyrzucenia jezeli odslonic poprzednia
    dispose(lFrm1,Done);
//    DisposeTrmList(lTrmList);
   end;
  ikFrmSchPred:
   begin lPredArgs:=AnalyzeTermList(Argumenty);
    if (lPredArgs=InCorrTrmList) or (nConstrNr=0) then
     begin Analyze:=NewIncorFrm; exit end;
    if not Agree(lPredArgs,SchPredArity[nConstrNr].nArity) then
     begin ExpError(120); Analyze:=NewIncorFrm end
    else
     Analyze:=NewPredFrm(ExpSort,nConstrNr,lPredArgs,0);
    RemoveQua(lPredArgs);
   end;
  ikFrmPred:
   begin
    lTrmList:=AnalyzeTermList(Argumenty);
    if (lTrmList=InCorrTrmList) or (nConstrNr=0) then
     begin Analyze:=NewIncorFrm; exit end;
    Analyze:=AnalyzePredicateFormula(ExpPos,lTrmList,nConstrNr);
    DisposeListOfTerms(lTrmList);
   end;
 end;
end;

function MultiPredicateFormula.Analyze: pointer;
 var i,k: integer;
     lNeg: boolean;
     lFrm: FrmPtr;
     lTrmList,rTrmList,lTL,lTList:TrmList;
     lExpr: ExpPtr;
begin
  lExpr:=ExpPtr(Argumenty.Items^[0]);
  lNeg:=lExpr^.ExpSort = ikFrmNeg;
  if lNeg then
   begin
    lExpr:=NegativeFormulaPtr(lExpr)^.NegArg;
    dispose(ExpPtr(Argumenty.Items^[0]));
   end;
  with PredicateFormulaPtr(lExpr)^ do
   begin
    lTrmList:=AnalyzeTermList(Argumenty);
    if (lTrmList=InCorrTrmList) or (nConstrNr=0) then
     begin
      result:=NewIncorFrm;
      exit
     end;
    result:=AnalyzePredicateFormula(ExpPos,lTrmList,nConstrNr);
    if FrmPtr(result)^.FrmSort = ikError then
      exit;
    if lNeg then
      result:=NewNeg(result);
     for k:=1 to LeftArgsCount do
      begin
       lTList:=lTrmList;
       lTrmList:=lTrmList^.NextTrm;
       dispose(lTList);
      end;
   end;
  for i := 1 to Argumenty.Count - 1 do
   begin
    lExpr:=ExpPtr(Argumenty.Items^[i]);
    lNeg:=lExpr^.ExpSort = ikFrmNeg;
    if lNeg then
     begin
      lExpr:=NegativeFormulaPtr(lExpr)^.NegArg;
      dispose(ExpPtr(Argumenty.Items^[i]));
     end;
    with PredicateFormulaPtr(lExpr)^ do
     begin
      rTrmList:=AnalyzeTermList(Argumenty);
      if (rTrmList=InCorrTrmList) or (nConstrNr=0) then
       begin
        result:=NewIncorFrm;
        exit
       end;
      lTl:=lTrmList;
      while lTl <> nil do
       begin
        lTL^.XTrmPtr:=CopyTerm(lTL^.XTrmPtr);
        lTl:=lTL^.NextTrm;
       end;
      lTl:=lTrmList;
      while lTl^.NextTrm <> nil do
       lTl:=lTl^.NextTrm;
      lTl^.NextTrm:=rTrmList;
      lFrm:=AnalyzePredicateFormula(ExpPos,lTrmList,nConstrNr);
      if lFrm^.FrmSort = ikError then
       begin
        result:=lFrm;
        exit
       end;
      if lNeg then
        lFrm:=NewNeg(lFrm);
      result:=NewConj(result,lFrm);
      lTL:=lTrmlist;
      while lTl <> rTrmList do
       begin
         lTList:=lTL;
         lTl:=lTl^.NextTrm;
         dispose(lTList);
       end;
      lTrmlist:=rTrmList;
     end;
   end;
end;

constructor MultiPredicateFormula.Init(fPos:Position);
begin
 ExpSort:=ikMultFrmPred;
 ExpPos:=fPos;
 Argumenty.Init(0,4);
 LeftArgsCount:=0;
end;

destructor MultiPredicateFormula.Done;
begin
 Argumenty.Done;
end;

constructor VerumFormula.Init;
begin inherited InitExp(ikFrmVerum) end;

function VerumFormula.Analyze: pointer;
begin Analyze:=NewVerum end;

constructor ThesisFormula.Init;
begin inherited InitExp(ikFrmThesis) end;

function ThesisFormula.Analyze: pointer;
begin
 if g.Thesis = nil then
  begin ExpError(65); Analyze:=NewIncorFrm end
 else Analyze:=g.Thesis^.CopyFormula;
end;

constructor InCorrFormula.Init;
begin inherited InitExp(ikError) end;

function InCorrFormula.Analyze: pointer;
begin Analyze:=NewIncorFrm end;

function QualifyingFormula.Analyze: pointer;
 var lFrm:FrmPtr; lTrm: TrmPtr;
begin
 lTrm:=Obiekt^.Analyze;
 lFrm:=NewQualFrm(lTrm,Kwalifikacja^.Analyze);
 Analyze:=lFrm;
 if lFrm^.FrmSort=ikFrmQual then
  with QualFrmPtr(lFrm)^ do
   if QualTrm^.TrmSort=ikTrmQua then
    QualTrm:=QuaTrmPtr(QualTrm)^.TrmProper;
end;

function SimpleTerm.Analyze: pointer;
 var lTrm: TrmPtr;
begin if VarNr = 0 then begin Analyze:=NewInCorTrm; exit end;
 case ExpSort of
 ikTrmBound:
  if BoundVar[VarNr]^.TypSort=ikError then
   begin Analyze:=NewInCorTrm; exit end;
 ikTrmConstant:
  begin
   if gExportableItem and (FixedVar[VarNr].nSkelConstNr = 0) then
     begin
      gConstInExportableItemOcc:=true;
      ExpError(222);
     end;
   if FixedVar[VarNr].nExp then
    begin lTrm:=CopyTerm(FixedVar[VarNr].nDef);
     WithinTerm(lTrm,ChangeBound);
     Analyze:=lTrm; exit;
    end;
   if FixedVar[VarNr].nTyp^.TypSort=ikError then
    begin Analyze:=NewInCorTrm; exit end;
  end;
 ikTrmLocus:
  if LocArgTyp[VarNr]^.TypSort=ikError then
   begin Analyze:=NewInCorTrm; exit end;
 end;
 Analyze:=NewVarTrm(ExpSort,VarNr);
end;

function FunctorTerm.Analyze: pointer;
 var lTrm: TrmPtr; lCompleteArgs,lTrmList, ltl: TrmList; K, OriginLen,i: integer;
begin
 if nConstrNr = 0 then begin Analyze:=NewIncorTrm; exit end;
 lTrmList:=AnalyzeTermList(Self.Argumenty);
 if lTrmList = InCorrTrmList then begin Analyze:=NewInCorTrm; exit end;
 case ExpSort of
 ikTrmSchFunc:
  begin
   if not Agree(lTrmList,SchFuncArity[nConstrNr].nArity) then
    begin ExpError(122); Analyze:=NewInCorTrm; exit end;
   if TypPtr(CurSchFuncTyp.Items^[nConstrNr-1])^.TypSort=ikError then
    begin Analyze:=NewInCorTrm; exit end;
   RemoveQua(lTrmList);
   Analyze:=NewLocFuncTrm(ikTrmSchFunc,nConstrNr,lTrmList);
  end;
 ikTrmPrivFunc:
  begin
   if gExportableItem then
    ExpError(221);
   if not Agree(lTrmList,FuncDefPtr(LocFuncDef.Items^[nConstrNr-1])^.fPrimaries) then
    begin ExpError(123); Analyze:=NewInCorTrm; exit end;
   RemoveQua(lTrmList);
   lTrm:=CopyTerm(FuncDefPtr(LocFuncDef.Items^[nConstrNr-1])^.fFuncDef);
{ !!!!!!!!!!!! Nie zadziala z operatorem Fraenkla ? }
   WithinTerm(lTrm,ChangeBound);
   Analyze:=NewPrivFuncTrm(nConstrNr,lTrmList,InstTrm(lTrm,lTrmList));
//!!   Analyze:=InstTrm(lTrm,lTrmList); // linia do wyrzucenia jezeli odslonic poprzednia
   DisposeTrm(lTrm);
//  DisposeTrmList(lTrmList);
  end;
 ikTrmAggreg:
  begin
   for K:=Notat[noAggregate].Count-1 downto 0 do
    with PatternPtr(Notat[noAggregate].Items^[K])^ do
     if fFormNr = nConstrNr then
      if CheckTypes(Notat[noAggregate].Items^[K],lTrmList) then
       begin
        if rConstr.Nr = 0 then
         begin Analyze:=NewIncorTrm; exit end;
        lCompleteArgs:=CreateArgList(fPrimTypes.Count);
        RemoveQua(lCompleteArgs);
        with ConstrTypPtr( Constr[ coAggregate].At( rConstr.Nr))^ do
         if fConstrTyp^.TypSort=ikError then
          begin Analyze:=NewIncorTrm; exit end;
        lTrm:=NewLocFuncTrm(ikTrmAggreg,rConstr.Nr,lCompleteArgs);
        Analyze:=lTrm;
        DisposeListOfTerms(lTrmList);
        InsertTermInTTColl(lTrm);
        exit;
       end;
   ExpError(105); Analyze:=NewIncorTrm;
  end;
 ikTrmFunctor:
  begin
    for K:=Notat[noFunctor].Count-1 downto 0 do
     with PatternPtr(Notat[noFunctor].Items^[K])^ do
      if fFormNr = nConstrNr then
      if CheckTypes(Notat[noFunctor].Items^[K],lTrmList) then
      begin
       if rConstr.Nr = 0 then
        begin Analyze:=NewIncorTrm; exit end;
       lCompleteArgs:=CreateArgList(fPrimTypes.Count);
       RemoveQua(lCompleteArgs);
       OriginLen:=ConstrTypPtr(Constr[coFunctor].Items^[rConstr.Nr])^.nPrimaries.Count;
       for i:=1 to fPrimTypes.Count-OriginLen do
        begin
         ltl:=lCompleteArgs;
         lCompleteArgs:=lCompleteArgs^.NextTrm;
         DisposeTrm(ltl^.XTrmPtr);
         Dispose(ltl);
        end;
       if ConstrTypPtr(Constr[coFunctor].Items^[rConstr.Nr])^.fConstrTyp^.TypSort=ikError then
        begin Analyze:=NewIncorTrm; exit end;
       lTrm:=NewFuncTrm(rConstr.Nr,lCompleteArgs);
       lTrm^.nPattNr:= K+1;
       Analyze:=lTrm;
       DisposeListOfTerms(lTrmList);
       InsertTermInTTColl(lTrm);
       exit;
      end;
    ExpError(103);
    Analyze:=NewIncorTrm;
    end;
 end;
end;

function ChoiceTerm.Analyze: pointer;
 var lTyp: TypPtr;
begin
 lTyp:=ChoiceType^.Analyze;
 if lTyp^.TypSort=ikError then
  begin Analyze:=NewIncorTrm; exit end;
 Analyze:=NewChoiceTrm(lTyp);
end;

function QualifiedTerm.Analyze: pointer;
 var lTrm1,lTrmProper: TrmPtr;
     lTyp: TypPtr;
begin
 lTrmProper:=Obiekt^.Analyze;
 lTyp:=Kwalifikacja^.Analyze;
 if (lTrmProper^.TrmSort=ikError) or (lTyp^.TypSort=ikError)
  then begin Analyze:=NewIncorTrm; exit end;
 if not lTyp^.IsWiderThan(GetTrmType(lTrmProper)) then ExpError(116);
   { ponizej to usuwanie wielokrotnych "qua" }
 if lTrmProper^.TrmSort=ikTrmQua then
  begin lTrm1:=lTrmProper;
   lTrmProper:=CopyTerm(QuaTrmPtr(lTrmProper)^.TrmProper);
   dispose(lTrm1,Done);
  end;
 Analyze:=NewQuaTrm(lTrmProper,lTyp);
end;

function SelectorTerm.Analyze: pointer;
 var lTrmList,lCompleteArgs: TrmList; lTrm: TrmPtr; k: integer;
     lList:MCollection;
begin
 lList.Init(1,0); lList.Insert(Struct);
 lTrmList:=AnalyzeTermList(lList);
 lList.DeleteAll; lList.Done;
 if (lTrmList=InCorrTrmList) or (Select=0) then
  begin Analyze:=NewIncorTrm; exit end;
 for K:=Notat[noSelector].Count-1 downto 0 do
  with PatternPtr(Notat[noSelector].Items^[K])^ do
  if fFormNr = Select then
  if CheckTypes(Notat[noSelector].Items^[K],lTrmList) then
   begin
    lCompleteArgs:=CreateArgList(fPrimTypes.Count);
    RemoveQua(lCompleteArgs);
    with ConstrTypPtr( Constr[ coSelector].At( rConstr.Nr))^ do
    if fConstrTyp^.TypSort=ikError then
     begin Analyze:=NewIncorTrm; exit end;
    lTrm:=NewLocFuncTrm(ikTrmSelector,rConstr.Nr,lCompleteArgs);
    Analyze:=lTrm;
    DisposeListOfTerms(lTrmList);
    {----}
    InsertTermInTTColl(lTrm);
    {----}
    exit;
   end;
 ExpError(126); Analyze:=NewIncorTrm;
end;

function SubAggrTerm.Analyze: pointer;
 var lTrmList: TrmList;
     lTrm,tSubAggrTerm: TrmPtr;
     k: integer;
begin
 tSubAggrTerm:=FullAggr^.Analyze;
 lTrmList:=NewTrmList(tSubAggrTerm,nil);
 if (lTrmList=InCorrTrmList) or (SubAggr=0) then
  begin Analyze:=NewIncorTrm; exit end;
 for K:=Notat[noForgetFunctor].Count-1 downto 0 do
  with PatternPtr(Notat[noForgetFunctor].Items^[K])^ do
  if fFormNr = SubAggr then
  if CheckTypes(Notat[noForgetFunctor].Items^[K],lTrmList) then
   begin
    lTrm:=ReconAggregTrm(rConstr.Nr,tSubAggrTerm,CopyTrmType(tSubAggrTerm));
    if lTrm^.TrmSort = ikError then ErrImm(135);
    Analyze:=lTrm;
    DisposeSubstTrm;
    DisposeListOfTerms(lTrmList);
    exit;
   end;
 ExpError(126); Analyze:=NewIncorTrm;
 DisposeTrm(tSubAggrTerm);
end;

function FraenkelTerm.Analyze: pointer;
 var lLambdaScope: TrmPtr; lBoundBase,lBoundVarNbr,k,z,i: integer;
     lCompr:FrmPtr; lLambdaArgs:MCollection; lIdents: IntSequence;
 label rt;
begin
 {----}
 MarkTermsInTTColl;
 {----}
 lBoundBase:=BoundVarNbr;
 with Sample^ do
  begin
   lIdents.Init(2 * nBoundVars.Count); //just estimate
   with nBoundVars do
    for z:=0 to Count-1 do
     for i:=1 to multipleTypePtr(Items^[z])^.nNumberOfCopies do
      begin inc(BoundVarNbr);
       BoundVar[BoundVarNbr]:=multipleTypePtr(Items^[z])^.nType^.Analyze;
       lIdents.Insert(multipleTypePtr(Items^[z])^.nIdents.Value(i-1));
      end;
   lLambdaScope:=nExpressionPtr^.Analyze;
  end;
 if lLambdaScope^.TrmSort=ikError then
  begin Analyze:=NewIncorTrm; BoundVarNbr:=lBoundBase; goto rt end;
 if lLambdaScope^.TrmSort=ikTrmQua then
  lLambdaScope:=QuaTrmPtr(lLambdaScope)^.TrmProper;
 lCompr:=Compr^.nExpressionPtr^.Analyze;
 if lCompr^.FrmSort=ikError then
  begin Analyze:=NewIncorTrm; BoundVarNbr:=lBoundBase; goto rt end;
 lBoundVarNbr:=BoundVarNbr; BoundVarNbr:=lBoundBase;
 lLambdaArgs.Init(lBoundVarNbr-lBoundBase,4);
 for k:=lBoundBase+1 to lBoundVarNbr do
  begin
   if BoundVar[k]^.TypSort=ikError then begin Analyze:=NewIncorTrm; goto rt end;
//   if (gBuiltIn[rqElement] = 0)
//    then begin ExpError(137); Analyze:=NewIncorTrm; goto rt end;
   if not FrOpVarTypeOK(BoundVar[k])
    then begin ExpError(129); Analyze:=NewIncorTrm; goto rt end;
   lLambdaArgs.Insert(BoundVar[k]);
  end;
 Analyze:=NewFraenkelTrmI(lLambdaScope,lCompr,lLambdaArgs,lIdents);
rt:
 RemoveTermsFromTTColl;
end;

function ItTerm.Analyze: pointer;
begin
 if ItTyp^.TypSort=ikError then begin Analyze:=NewInCorTrm; exit end;
 Analyze:=NewItTrm;
end;

function InCorrTerm.Analyze: pointer;
begin  Analyze:=NewInCorTrm end;

function NumeralTerm.Analyze: pointer;
begin  Analyze:=NewVarTrm(ikTrmNumeral,nNatVal) end;

constructor AttributedExpression.Init(fSort:char;fSubject:ExpPtr;
                                      fPos:Position; var fList:MList);
begin ExpSort:=fSort;
 Podmiot:=fSubject; ExpPos:=fPos; Przymiotniki.CopyList(fList);
end;

destructor AttributedExpression.Done;
begin dispose(Podmiot,Done); Przymiotniki.Done end;

function AnalyzeAttrFrm(fItem:AttrNodePtr; fSubject:TrmPtr) : FrmPtr;
 var k,nAttrNr,OriginLen,j:integer; lPredArgs,A,lTrmList,ltl:TrmList;
     lFrm1,lFrm2:FrmPtr;
begin
 lTrmList:=AnalyzeTermList(fItem^.Argumenty);
 if lTrmList = InCorrTrmList then begin AnalyzeAttrFrm:=NewInCorFrm; exit end;
 if lTrmList <> nil then
  begin lTl:=LastElem(lTrmList);
    lTl^.NextTrm:=NewTrmList(fSubject,nil);
  end
 else lTrmList:=NewTrmList(fSubject,nil);
 with fItem^ do
  if nInt > 0 then
  for k:=Notat[noAttribute].Count-1 downto 0 do
   with PatternPtr(Notat[noAttribute].Items^[K])^ do
    if (fFormNr = nInt) and (rConstr.Nr > 0) then
     if CheckTypes(Notat[noAttribute].Items^[K],lTrmList) then
      begin
       lPredArgs:=CreateArgList(fPrimTypes.Count); RemoveQua(lPredArgs);
       OriginLen:=ConstrTypPtr(Constr[ coAttribute].Items^[rConstr.Nr])^.nPrimaries.Count;
       for j:=1 to fPrimTypes.Count-OriginLen do
        begin
         ltl:=lPredArgs;
         lPredArgs:=lPredArgs^.NextTrm;
         DisposeTrm(ltl^.XTrmPtr);
         Dispose(ltl);
        end;
       lFrm1:=NewPredFrm(ikFrmAttr,rConstr.Nr,lPredArgs,K+1);
       AdjustAttrFrm(PredFrmPtr(lFrm1),nAttrNr,A); {###}
       lFrm2:=NewPredFrm(ikFrmAttr,nAttrNr,CopyTermList(A),K+1); dispose(lFrm1,Done);
       AnalyzeAttrFrm:=lFrm2;
       if fAntonymic <> nNeg then AnalyzeAttrFrm:=new(NegFrmPtr,Init(lFrm2));
       DisposeListOfTerms(lTrmList);
       exit;
      end;
 AnalyzeAttrFrm:=nil;
 DisposeListOfTerms(lTrmList);
end;

function AttributiveFormula.Analyze: pointer;
 var lTrm:TrmPtr; lConjuncts:MCollection; lErr:Boolean; lFrm:FrmPtr;
     z: integer;
begin
 lTrm:=Podmiot^.Analyze;
 if lTrm^.TrmSort=ikError then begin Analyze:=NewInCorFrm; exit end;
 lConjuncts.Init(Przymiotniki.Count,2); lErr:=false;
 with Przymiotniki do
  for z:=0 to Count-1 do
  begin
   lFrm:=AnalyzeAttrFrm(AttrNodePtr(Items^[z]),CopyTerm(lTrm));
   if lFrm=nil then
    begin Error(AttrNodePtr(Items^[z])^.nPos,106); lErr:=true end;
   lConjuncts.Insert(lFrm);
  end;
 DisposeTrm(lTrm); lConjuncts.Prune;
 if lErr then Analyze:=NewInCorFrm
  else Analyze:=NewConjFrm(lConjuncts);
end;

procedure LoadIPNColl(var fColl:MList);
 var lPtr: AttrNodePtr; lNeg:boolean; lExpPtr:ExpPtr;
begin
 fColl.Init(2);
 InFile.InWord;
 while InFile.Current.Kind <> ';' do
  begin InFile.InPos(CurPos); InFile.InWord;
   lNeg:=InFile.Current.Kind='-'; InFile.InWord;
   lPtr:=new(AttrNodePtr,Init(InFile.Current.Nr,CurPos,lNeg));
   lPtr^.Argumenty.Init(0,2); InFile.InWord;
   while InFile.Current.Kind <> ';' do
   begin
    lExpPtr:=LoadTerm;
    lPtr^.Argumenty.Insert(lExpPtr); InFile.InWord;
   end;
   fColl.Insert(lPtr);
   InFile.InWord;
  end;
end;

function LoadFormula:ExpPtr;
 var lExpPtr,lFrm:ExpPtr;
     i,lQuantNbr:integer;
begin InFile.InPos(CurPos); InFile.InWord;
 case InFile.Current.Kind of
  ikFrmPrivPred,ikFrmSchPred,ikFrmPred,ikRSFrmPred:
   begin lFrm:=new(PredicateFormulaPtr,InitExp(InFile.Current.Kind));
    with PredicateFormulaPtr(lFrm)^ do
     begin nConstrNr:=InFile.Current.Nr;
      ExpPos:=CurPos; Argumenty.Init(2,2); InFile.InWord;
      while InFile.Current.Kind <> ';' do
       begin
        lExpPtr:=LoadTerm;
        Argumenty.Insert(lExpPtr); InFile.InWord;
       end;
     end;
   end;
  ikMultFrmPred:
   begin
    lFrm:=new(MultiPredicateFormulaPtr,Init(CurPos));
    MultiPredicateFormulaPtr(lFrm)^.Argumenty.Insert(LoadFormula);
    InFile.InWord;
    MultiPredicateFormulaPtr(lFrm)^.LeftArgsCount:=InFile.Current.Nr;
    InFile.InWord;
    while InFile.Current.Kind <> ';' do
     begin
      MultiPredicateFormulaPtr(lFrm)^.Argumenty.Insert(LoadFormula);
      InFile.InWord;
     end;
   end;
  ikFrmQual:
   begin lFrm:=new(QualifyingFormulaPtr,InitExp(InFile.Current.Kind));
    with QualifyingFormulaPtr(lFrm)^ do
     begin
      Obiekt:=LoadTerm;
      Kwalifikacja:=LoadType;
      ExpPos:=CurPos;
     end;
   end;
  ikFrmAttr:
   begin lFrm:=new(AttributiveFormulaPtr,InitExp(InFile.Current.Kind));
    with AttributiveFormulaPtr(lFrm)^ do
     begin
      Podmiot:=LoadTerm;
      ExpPos:=CurPos;
      LoadIPNColl(Przymiotniki);
     end;
   end;
  ikFrmNeg:
   begin lFrm:=new(NegativeFormulaPtr,InitExp(InFile.Current.Kind));
    NegativeFormulaPtr(lFrm)^.NegArg:=LoadFormula;
   end;
  ikFrmImplies,ikFrmIff,ikFrmOr,ikFrmConj,ikFrmFlexDisj,ikFrmFlexConj:
   begin lFrm:=new(BinaryFormulaPtr,InitExp(InFile.Current.Kind));
    with BinaryFormulaPtr(lFrm)^ do
     begin
       BinArg1:=LoadFormula; BinArg2:=LoadFormula;
     end;
   end;
  ikFrmUniv,ikFrmEx:
   begin lFrm:=new(QuantifiedFormulaPtr,InitExp(InFile.Current.Kind));
    with QuantifiedFormulaPtr(lFrm)^ do
     begin
      InFile.InWord; lQuantNbr:=InFile.Current.Nr;
      QuantVars.Init(lQuantNbr);
      for i:=1 to lQuantNbr do
       begin InFile.InWord;
        QuantVars.Insert(InFile.Current.Nr);
       end;
      Quantified:=LoadType;
      Scope:=LoadFormula;
     end;
   end;
  ikFrmVerum: lFrm:=new(VerumFormulaPtr,Init);
  ikFrmThesis: lFrm:=new(ThesisFormulaPtr,Init);
  ikError: lFrm:=new(InCorrFormulaPtr,Init);
  else RunTimeError(2403);
 end;
 LoadFormula:=lFrm;
end;

function LoadTerm:ExpPtr;
 var lTrm,lType,lExpPtr:ExpPtr;
     lNr,lNumberOfCopies,i:integer;
     lPos:Position;
     lmExpPtr:multipleTypePtr;
     lIdents: IntSequence;
begin InFile.InWord;
 case InFile.Current.Kind of
  ikTrmBound,ikTrmConstant,ikTrmLocus:
   begin
    lTrm:=new(SimpleTermPtr,Init(InFile.Current.Kind,InFile.Current.Nr));
   end;
  ikTrmNumeral:
   begin InFile.InWord;
    lTrm:=new(NumeralTermPtr,Init(InFile.Current.Nr));
   end;
  ikTrmPrivFunc,ikTrmSchFunc,ikTrmFunctor,ikTrmAggreg:
   begin lTrm:=new(FunctorTermPtr,InitExp(InFile.Current.Kind));
    with FunctorTermPtr(lTrm)^ do
     begin ExpSort:=InFile.Current.Kind;
      nConstrNr:=InFile.Current.Nr;
      InFile.InPos(CurPos);  ExpPos:=CurPos;
      Argumenty.Init(2,2); InFile.InWord;
      while InFile.Current.Kind <> ';' do
       begin
        lExpPtr:=LoadTerm;
        Argumenty.Insert(lExpPtr); InFile.InWord;
       end;
     end;
   end;
  ikTrmSelector:
   begin lNr:=InFile.Current.Nr; InFile.InPos(CurPos);
    lPos:=CurPos;
    lExpPtr:=LoadTerm;
    lTrm:=new(SelectorTermPtr,Init(lNr,lPos,lExpPtr));
   end;
  ikTrmSubAgreg:
   begin lNr:=InFile.Current.Nr; InFile.InPos(CurPos);
    lPos:=CurPos;
    lExpPtr:=LoadTerm;
    lTrm:=new(SubAggrTermPtr,Init(lNr,lPos,lExpPtr));
   end;
  ikTrmChoice:
   begin InFile.InPos(CurPos);
    lPos:=CurPos;
    lExpPtr:=LoadType;
    lTrm:=new(ChoiceTermPtr,Init(lPos,lExpPtr));
   end;
  ikTrmFraenkel:
   begin lTrm:=new(FraenkelTermPtr,InitExp(InFile.Current.Kind));
    with FraenkelTermPtr(lTrm)^ do
     begin InFile.InPos(CurPos); ExpPos:=CurPos;
      if not gFraenkelTermAllowed then
       ErrImm(259);
      new(Sample);
      with Sample^ do
       begin
        nFreeVars.Init(0,0);
        nBoundVars.Init(2,2);
        InFile.InWord;
        while InFile.Current.Kind <> ';' do
         begin
          lNumberOfCopies:=InFile.Current.Nr;
          lIdents.Init(lNumberOfCopies);
          for i:=1 to lNumberOfCopies do
          begin InFile.InWord;
            lIdents.Insert(InFile.Current.Nr);
          end;
          lType:=LoadType;
          lmExpPtr:=new(multipleTypePtr,Init(lType,lIdents));
          nBoundVars.Insert(lmExpPtr);
          InFile.InWord;
         end;
        nExpressionPtr:=LoadTerm;
       end;
      new(Compr);
      with Compr^ do
       begin nFreeVars.Init(0,0); nExpressionPtr:=LoadFormula end;
     end;
   end;
  ikTrmQua,ikTrmExactly:
   begin lTrm:=new(QualifiedTermPtr,InitExp(InFile.Current.Kind));
    with QualifiedTermPtr(lTrm)^ do
     begin
      Obiekt:=LoadTerm;
      Kwalifikacja:=LoadType;
      InFile.InPos(CurPos);
      ExpPos:=CurPos;
     end;
   end;
  ikTrmIt: lTrm:=New(ItTermPtr,Init);
  ikError: lTrm:=New(IncorrTermPtr,Init);
  else RunTimeError(2402);
 end;
 LoadTerm:=lTrm;
end;

function LoadType:ExpPtr;
 var lTyp,lExpPtr:ExpPtr;
begin InFile.InPos(CurPos); InFile.InWord;
 case InFile.Current.Kind of
  ikTypMode,ikTypStruct:
   begin lTyp:=new(TypePtr,InitExp(InFile.Current.Kind));
    with TypePtr(lTyp)^ do
     begin ExpSort:=InFile.Current.Kind;
      nConstrNr:=InFile.Current.Nr;
      ExpPos:=CurPos; Argumenty.Init(2,2);
      InFile.InWord;
      while InFile.Current.Kind <> ';' do
       begin
        lExpPtr:=LoadTerm;
        Argumenty.Insert(lExpPtr); InFile.InWord;
       end;
     end;
   end;
  ikExpResDes:
   begin lTyp:=new(ResDesPtr,InitExp(InFile.Current.Kind));
    with ResDesPtr(lTyp)^ do
     begin nConstrNr:=InFile.Current.Nr;
      InFile.InWord; nIdent:=InFile.Current.Nr;
      ExpPos:=CurPos;
      Argumenty.Init(2,2); InFile.InWord;
      while InFile.Current.Kind <> ';' do
       begin
        lExpPtr:=LoadTerm;
        Argumenty.Insert(lExpPtr); InFile.InWord;
       end;
     end;
   end;
  ikExpAttrTyp:
   begin lTyp:=new(AttributedTypePtr,InitExp(InFile.Current.Kind));
    with AttributedTypePtr(lTyp)^ do
     begin
      LoadIPNColl(Przymiotniki);
      Podmiot:=LoadType;
      ExpPos:=CurPos;
     end;
   end;
  ikError: lTyp:=New(IncorrTypePtr,Init);
  else RunTimeError(2401);
 end;
 LoadType:=lTyp;
end;

end.
