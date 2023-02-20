(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit inlibr;
interface

uses correl,limits,mobjects,envhan,inout,inoutmml,identify,impobjs,enums,iocorrel;

var LogOpened : boolean = false;


function AssignedLibr(fName,fExt:string):string;

procedure TransMMLTerm(aTrm: TrmPtr);
procedure TransMMLTermList(aTrmList: TrmList);
procedure TransMMLType(aTyp: TypPtr);
procedure TransMMLTypeColl(const aTypColl:MList);
procedure TransMMLTypeList(const aTypList:MList);
procedure TransMMLStructLoci(const aStructLoci: NatFunc);
procedure TransMMLCluster(aCluster: AttrCollectionPtr);
procedure TransMMLIdentify(aItem:IdentifyPtr);
procedure TransMMLReduction(aItem:ReductionPtr);
procedure TransMMLProperty(aItem:PropertyPtr);
procedure TransMMLFormula(aFrm: FrmPtr);
procedure TransMMLFormulaColl(const fFrmColl:MCollection);

procedure TransMMLDefiniens(aItem:DefiniensPtr);

procedure InitConstrNbr;
procedure InitSignatures;
procedure GetSignature(fName:string);

procedure InitDCOBases(const fName:string; var aImp: ImpObj);
procedure RestoreDCOBases;

function AccessibleTerm ( fTrm: TrmPtr ): boolean;
function AccessibleTermList ( fTrmList:TrmList ): boolean;
function AccessibleType ( fTyp: TypPtr): boolean;
function AccessibleFormula ( fFrm: FrmPtr): boolean;
function AccessibleFormulaColl (var fFrmList: MCollection): boolean;
function AccessibleTypeColl (var fTypList:MList): boolean;
function AccessibleCluster(fCluster:AttrCollectionPtr): boolean;
function AccesibleIdentify(fId:IdentifyPtr): boolean;
function AccesibleReduction(fId:ReductionPtr): boolean;
function AccesibleProperty(fPr:PropertyPtr): boolean;

function AccesibleDefiniens(fDefiniens:DefiniensPtr): boolean;

procedure ChooseFunctor(fNr: integer);
procedure ChooseMode(fNr: integer);
procedure ChooseAttribute(fNr: integer);
procedure ChoosePredicate(fNr: integer);
procedure ChooseSelector(fNr: integer);
procedure ChooseAggregate(fNr: integer);
procedure ChooseStructMode(fNr: integer);
function ChosenTerm ( fTrm: TrmPtr ): boolean;
function ChosenTermList ( fTrmList:TrmList ): boolean;
function ChosenCluster(fCluster: AttrCollectionPtr ): boolean;
function ChosenType ( fTyp: TypPtr): boolean;
function ChosenFormula ( fFrm: FrmPtr): boolean;
function ChosenFormulaColl (var fFrmList: MCollection): boolean;
function ChosenTypeColl (var fTypList:MCollection ): boolean;

procedure InitAccessibility;
procedure FinishAccessibility;
procedure AccessibilityConstr(const fConstr: Lexem);
procedure AccessibilityTrm(fTrm: TrmPtr);
procedure AccessibilityTrmList(fTrmList:TrmList);
procedure AccessibilityAttributes(fCluster:AttrCollectionPtr);
procedure AccessibilityTyp(fTyp: TypPtr);
procedure AccessibilityTypColl(const fTypList: MCollection);
procedure AccessibilityFrm(fFrm:FrmPtr);
procedure AccessibilityFrmColl(const fFrmList: MCollection);
procedure AccessibilityConstrTyp(fConstr:ConstrTypPtr);
procedure AccessibilityConstructor(fConstr:ConstrPtr);

function ImpConstrNr(fConstr: Lexem): integer;

procedure InConstrNum;

var

 gRefused,gInaccessible: array[ConstructorsKind] of NatSet;

 InMMLFile: InMMLFilePtr;

 gLogFile: text;

const
 gMaxConstrNbr: ConstrIntArr =
    (MaxModeNbr,MaxStructNbr,MaxAttrNbr,MaxPredNbr,
     MaxFuncNbr,MaxSelectNbr,MaxStructNbr);

 procedure OpenLogFile;
 procedure CloseLogFile;

implementation

uses lexicon,mizenv,librenv,errhan,builtin,xml_parser,xmlpars,xmldict,mscanner
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

procedure ConnectionFault ( Err:word; Pos:Position; N: string );
begin CurPos:=Pos;
{$IFDEF MDEBUG}
writeln(InfoFile,'Connection Fault: ',N);
{$ENDIF}
 RunTimeError(Err);
end;

function ImpConstrNr(fConstr: Lexem): integer;
 var i: integer; c: ConstructorsKind;
begin
  c:=ConstructorKind(FConstr.Kind);
  for i:=1 to ImpConstrNbr do
   with ImpConstr[i]^ do
    if (nBase[c]+1 <= fConstr.Nr) and ( fConstr.Nr<=ImpConstr[i+1]^.nBase[c])
     then
      begin
       ImpConstrNr:=i;
       exit;
      end;
 ImpConstrNr:=0;
end;

var
 ImpConstrBase,ClusterBase: integer;

{+-------------------------------------------------------------------+}
 
// ##TODO: this should be just some 'Trans' walker  
procedure TransMMLTermList(aTrmList: TrmList);
begin
 while aTrmList<>nil do
  begin TransMMLTerm(aTrmList^.XTrmPtr); aTrmList:=aTrmList^.NextTrm end;
end;

procedure TransMMLTypeColl(const aTypColl:MList);
 var i: integer;
begin
 with aTypColl do
 for i:=0 to Count-1 do TransMMLType(Items^[i]);
end;

procedure TransMMLTypeList(const aTypList:MList);
 var i: integer;
begin
 with aTypList do
 for i:=0 to Count-1 do TransMMLType(Items^[i]);
end;

procedure TransMMLStructLoci(const aStructLoci: NatFunc);
 var i: integer;
begin
 with aStructLoci do
 for i:=0 to Count-1 do
   Items^[i].Y:=gTrans[coStructMode].Value(Items^[i].Y);
end;

procedure TransMMLCluster(aCluster: AttrCollectionPtr);
  var i: integer;
begin
 with aCluster^ do
 for i:=0 to Count-1 do
  with AttrPtr(Items^[i])^ do
  begin
   fAttrNr:=gTrans[coAttribute].Value(fAttrNr);
   TransMMLTermList(fAttrArgs);
  end;
end;

procedure TransMMLType(aTyp: TypPtr);
begin
 with aTyp^ do
  begin
   TransMMLCluster(LowerCluster);
//   TransMMLCluster(UpperCluster);
   case TypSort of
    ikTypMode: ModNr:=gTrans[coMode].Value(ModNr);
    ikTypStruct: ModNr:=gTrans[coStructMode].Value(ModNr);
     else RunTimeError(1800);
    end;
   TransMMLTermList(ModArgs);
  end;
end;

procedure TransMMLFormulaColl(const fFrmColl:MCollection);
 var i: integer;
begin
  with fFrmColl do
  for i:=0 to Count-1 do
   TransMMLFormula(Items^[i]);
end;

procedure TransMMLFormula(aFrm: FrmPtr);
begin
 case FrmPtr(aFrm)^.FrmSort of
  ikFrmNeg:
    TransMMLFormula(NegFrmPtr(aFrm)^.NegArg);
  ikFrmConj:
    TransMMLFormulaColl(ConjFrmPtr(afrm)^.Conjuncts);
  ikFrmUniv:
   with UnivFrmPtr(aFrm)^ do
   begin TransMMLType(TypPtr(Quantified));
    TransMMLFormula(Scope);
   end;
  ikFrmSchPred:
    TransMMLTermList(PredFrmPtr(aFrm)^.PredArgs);
  ikFrmAttr:
   with PredFrmPtr(aFrm)^ do
   begin
    PredNr:=gTrans[coAttribute].Value(PredNr);
    TransMMLTermList(PredArgs);
   end;
  ikFrmPred:
   with PredFrmPtr(aFrm)^ do
   begin
    PredNr:=gTrans[coPredicate].Value(PredNr);
    TransMMLTermList(PredArgs);
   end;
  ikFrmQual:
   with QualFrmPtr(aFrm)^ do
   begin TransMMLTerm(QualTrm);
    TransMMLType(TypPtr(QualTyp));
   end;
  ikFrmFlexConj:
   with FlexFrmPtr(aFrm)^ do
   begin TransMMLFormula(nLeftOrigFrm);
    TransMMLFormula(nRightOrigFrm);
    TransMMLTerm(nLeftTrm);
    TransMMLTerm(nRightTrm);
    TransMMLFormula(nExpansion);
   end;
  ikFrmVerum:;
  else RunTimeError(1808);
 end;
end;

procedure TransMMLTerm(aTrm: TrmPtr);
begin
 case TrmPtr(atrm)^.TrmSort of
  ikTrmLocus,ikTrmBound,ikTrmNumeral:;
  ikTrmFunctor:
    with FuncTrmPtr(aTrm)^ do
     begin
      FuncNr:=gTrans[coFunctor].Value(FuncNr);
      TransMMLTermList(FuncArgs);
     end;
   ikTrmAggreg:
    with FuncTrmPtr(aTrm)^ do
     begin FuncNr:=gTrans[coAggregate].Value(FuncNr);
      TransMMLTermList(FuncArgs);
     end;
   ikTrmSchFunc:
    TransMMLTermList(FuncTrmPtr(aTrm)^.FuncArgs);
   ikTrmSelector:
    with FuncTrmPtr(aTrm)^ do
     begin FuncNr:=gTrans[coSelector].Value(FuncNr);
      TransMMLTermList(FuncArgs);
     end;
   ikTrmFraenkel:
    with FraenkelTrmPtr(aTrm)^ do
    begin
     TransMMLTypeColl(LambdaArgs);
     TransMMLTerm(LambdaScope);
     TransMMLFormula(Compr);
    end;
   ikTrmChoice:
    TransMMLType(ChoiceTrmPtr(aTrm)^.ChoiceTyp);
   ikTrmIt: ;
   else RunTimeError(1800);
  end;
end;

procedure TransMMLIdentify(aItem:IdentifyPtr);
  var n: integer;
begin
 with aItem^ do
  begin
   TransMMLTypeColl(nPrimaryList);
   case nConstrKind of
   ikTrmFunctor: begin TransMMLTerm(TrmPtr(nPattern[0])); TransMMLTerm(TrmPtr(nPattern[1])) end;
   ikFrmAttr,ikFrmPred: begin TransMMLFormula(FrmPtr(nPattern[0])); TransMMLFormula(FrmPtr(nPattern[1])) end;
   end;
  end;
end;

procedure TransMMLReduction(aItem:ReductionPtr);
begin
 with aItem^ do
  begin
   TransMMLTypeColl(nPrimaryList);
   TransMMLTerm(TrmPtr(nTerms[0]));
   TransMMLTerm(TrmPtr(nTerms[1]));
  end;
end;

procedure TransMMLProperty(aItem:PropertyPtr);
begin
 with aItem^ do
  begin
   TransMMLTypeColl(nPrimaryList);
   case PropertyKind(nPropertyKind) of
    sySethood: TransMMLType(TypPtr(nObject));
   end;
  end;
end;

procedure TransMMLDefiniens(aItem:DefiniensPtr);
var n: integer;
begin
 with aItem^ do
  begin
   nConstr.Nr:=gTrans[ConstructorKind(nConstr.Kind)].Value(nConstr.Nr);
   TransMMLTypeColl(PrimaryList);
   if Assumptions <> nil then
    TransMMLFormula(Assumptions);
   with Definiens^, nPartialDefinientia do
   begin
    if nOtherwise<>nil then
     begin
      case DefSort of
      'm': TransMMLFormula(FrmPtr(nOtherwise));
      'e': TransMMLTerm(TrmPtr(nOtherwise));
      end;
     end;
    for n:=0 to Count-1 do
     with PartDefPtr(Items^[n])^ do
      begin
       case DefSort of
       'm': TransMMLFormula(FrmPtr(nPartDefiniens));
       'e': TransMMLTerm(TrmPtr(nPartDefiniens));
       end;
       TransMMLFormula(FrmPtr(nGuard));
      end;
   end;
  end;
end;

{+-------------------------------------------------------------------+}

function AssignedLibr(fName,fExt:string):string;
 var fPath: string;
begin fPath:=LibraryPath(LowerCase(fName),fExt);
 AssignedLibr:=fPath; if fPath='' then exit;
 InMMLFile:=new(InMMLFilePtr,OpenFile(fPath));
end;

var
 gConstrNr: NatSeq;

procedure AddConstructorsName(fName:string);
begin
 IncIndex(ImpConstrNbr,ImpSgnIndex);
 gConstrNr.InsertElem(ImpConstrNbr);
 fname:=UpperCase(fName);
 ImpConstr[ImpConstrNbr]:=new(ImpConstrPtr,Init(fName));
end;

{ seems only dco uses it now, should be rmoved completely together
with GetSignature}
procedure GetConstructorsNames(fName:string);
var
 i,j:integer; lName:string;
 lSgn: MStringList; lOccurs: boolean;
begin
 InMMLFile:=new(InMMLFilePtr,OpenFile(LibraryPath(LowerCase(fName),'.dco')));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elConstructors);
  NextElementState;
  GetConstrNames( lSgn);
 end;
 dispose(InMMLFile,Done);
 gConstrNr.Init(40,40);
 gConstrNr.InsertElem(0);
 ImpConstrBase:=ImpConstrNbr;
 for i:=0 to lSgn.fCount-1 do
 begin
  lName:= lSgn.GetString( i);
  lOccurs:= false;
  for j:=1 to ImpConstrNbr do
   if lName=ImpConstr[j]^.fStr then
   begin lOccurs:=true; break end;
  if lOccurs then gConstrNr.InsertElem(j)
  else AddConstructorsName(lName);
 end;
 lSgn.Done;
end;

procedure InitSignatures;
begin
 ImpConstrNbr:=0;
 gConstrNr.Init(40,40);
 gConstrNr.InsertElem(0);
 ImpConstrBase:=0;
 fillchar(ImpConstr,sizeof(ImpConstr),0);
end;

procedure InitConstrNbr;
 var c: ConstructorsKind;
begin
 for c:=coMode to coAggregate do
  gConstrNbr[c]:=0;
// gClusterColl.InitSorted(2,CompareClusters);
// gClusterColl.Insert(new(AttrCollectionPtr, Init(0,4)));
end;

procedure InitTrans;
 var c: ConstructorsKind;
begin
 for c:=coMode to coAggregate do
  begin
   gTrans[c].Init(MaxRenumNbr);
   gTrans[c].Insert(0);
  end;
end;

procedure DoneTrans;
 var c: ConstructorsKind;
begin
 for c:=coMode to coAggregate do
  begin
   gTrans[c].Done;
  end;
end;

procedure AssignSTables(i: integer);
 var j: integer; c: ConstructorsKind;
begin
  for c:=coMode to coAggregate do
   for j:=ImpConstr[i]^.nBase[c]+1 to ImpConstr[i+1]^.nBase[c] do
    gTrans[c].Insert(j);
end;

procedure ReadConstructors;
var i,j,Imp: integer;
    lFields:NatSetPtr;
    lAggrColl:PCollection;
    c: ConstructorsKind;
    lImpConstr: ImpConstructorsObj;
label NextImp;
begin
 for Imp:=ImpConstrBase+1 to ImpConstrNbr do
   begin
    ImpConstr[Imp]^.nBase:=gConstrNbr;
    lImpConstr.GetConstructors(LibraryPath(LowerCase(ImpConstr[Imp]^.fStr),'.dco'));
    if lImpConstr.fConstructors.Count = 0 then
     begin
      writeln(gLogFile,'**** Cannot find constructors file: ',ImpConstr[Imp]^.fStr);
      ConnectionFault(808,CurPos,ImpConstr[Imp]^.fStr);
     end;
    InitTrans;
    writeln(gLogFile,'  <',LibraryPath(LowerCase(ImpConstr[Imp]^.fStr),'.dco'));
    with lImpConstr.fConstrIdents do
     for j:=0 to fCount-1 do
      begin
       for i:=1 to Imp-1 do
        if fList^[j].fString^=ImpConstr[i]^.fStr then
         begin AssignSTables(i); goto NextImp end;
       writeln(gLogFile,'**** Cannot find constructors name on constructor list: ',fList^[j].fString^);
       ConnectionFault(825,CurPos,ImpConstr[Imp]^.fStr);
NextImp:
       end;
    for c:=low(ConstructorsKind) to high(ConstructorsKind) do
      begin { ilosc konstruktor/ow  z danego artykulu!!!};
       for j:=1 to lImpConstr.fConstrNbr[c] do
        gTrans[c].Insert(gConstrNbr[c]+j);
      end;
    // ##TODO: this should be just some 'Trans' walker applied to ConstrObj
    with lImpConstr.fConstructors do
     for i:=0 to Count-1 do
     begin TransMMLTypeList(ConstrPtr(Items^[i])^.nPrimaries);
      case ConstrPtr(Items^[i])^.fConstrKind of
      coFunctor:
      with ConstrTypPtr(Items^[i])^ do
       begin
        TransMMLType(fConstrTyp);
        fWhichConstrNr:=gTrans[coFunctor].Value(fWhichConstrNr);
       end;
      coAttribute:
       with ConstrTypPtr(Items^[i])^ do
       begin
         TransMMLType(fConstrTyp);
         fWhichConstrNr:=gTrans[coAttribute].Value(fWhichConstrNr);
       end;
      coMode:
       with ConstrTypPtr(Items^[i])^ do
       begin
        TransMMLType(fConstrTyp);
        fWhichConstrNr:=gTrans[coMode].Value(fWhichConstrNr);
       end;
      coPredicate:
       with ConstrPtr(Items^[i])^ do
       begin
        fWhichConstrNr:=gTrans[coPredicate].Value(fWhichConstrNr);
       end;
      coStructMode:
       with StructConstrPtr(Items^[i])^ do
       begin
        fStructModeAggrNr:=gTrans[coAggregate].Value(fStructModeAggrNr);
        with fFields^ do
        begin
         lFields:=new(NatSetPtr,Init(Count,0));
         for j:=0 to Count-1 do
          lFields^.InsertElem(gTrans[coSelector].Value(Items^[j].X));
        end;
        dispose(fFields,Done);
        fFields:=lFields;
        with fPrefixes do
          for j:=0 to Count-1 do TransMMLType(Items^[j]);
       end;
      coAggregate:
        with AggrConstrPtr(Items^[i])^ do
        begin
         TransMMLType(fConstrTyp);
         with fAggrColl^ do
          begin lAggrColl:=new(PCollection, Init(Count,0));
           for j:=0 to Count-1 do
           lAggrColl^.Insert(new(PIntItem,
                 init(gTrans[coSelector].Value(PIntItem(Items^[j])^.IntKey))));
          end;
         dispose(fAggrColl,Done);
         fAggrColl:=lAggrColl;
        end;
      coSelector:
        with ConstrTypPtr(Items^[i])^ do
        begin
         TransMMLType(fConstrTyp);
        end;
      end;
     end;
    with lImpConstr.fConstructors do
     for i:=0 to Count-1 do
      with ConstrPtr(Items^[i])^ do
     begin
      IncIndex( gConstrNbr[ fConstrKind], ConstrIndex( fConstrKind));
      Constr[ fConstrKind].Insert( Items^[i]);
      mizassert(8001, gConstrNbr[ fConstrKind] = Constr[ fConstrKind].Count-1);
     end;
    DoneTrans;
    lImpConstr.fConstructors.DeleteAll;
    lImpConstr.Done;
   end;
 ImpConstr[ImpConstrNbr+1]:=new(ImpConstrPtr,Init(''));
 ImpConstr[ImpConstrNbr+1]^.nBase:=gConstrNbr;
end;

procedure GetSignature(fName:string);
 var i: integer;
begin
 GetConstructorsNames(fName);
 for i:=1 to ImpConstrNbr do
  if fName=ImpConstr[i]^.fStr then
   begin
{    dispose(InMMLFile,Done);}
    ReadConstructors;
    exit;
   end;
 AddConstructorsName(fName);
{ dispose(InMMLFile,Done);}
 ReadConstructors;
end;

procedure ReadDCOBases;
var
 i,j,Imp: integer; lSgn:MStringList;
 lName: string; lPath: string; c:ConstructorsKind;
 lOccurs: boolean; lNbrs:ConstrIntArr;
begin
 for Imp:=ImpConstrBase+1 to ImpConstrNbr do
   begin
    ImpConstr[Imp]^.nBase:=gConstrNbr;
    lPath:=LibraryPath(LowerCase(ImpConstr[Imp]^.fStr),'.dco');
    if lPath = '' then
     begin
      writeln(gLogFile,'**** Cannot find constructors file: ',ImpConstr[Imp]^.fStr);
      ConnectionFault(804,CurPos,ImpConstr[Imp]^.fStr);
     end;
    writeln(gLogFile,'  <',lPath);
    InMMLFile:=new(InMMLFilePtr,OpenFile(lPath));
    with InMMLFile^ do
    begin
     NextElementState;
     XMLASSERT( nElKind = elConstructors);
     NextElementState;
     GetConstrNames( lSgn);
     fillchar( lNbrs, sizeof(lNbrs), 0);
     In_ConstrCounts( lNbrs);
    end;
    dispose(InMMLFile,Done);
    
    for j:=0 to lSgn.fCount-1 do
    begin
     lName:= lSgn.GetString( j);
     lOccurs:= false;
     for i:=1 to Imp-1 do
      if lName=ImpConstr[i]^.fStr then
      begin lOccurs:=true; break end;
     
     if not lOccurs then
     begin
      writeln(gLogFile,'**** Cannot find constructors name on constructor list: ',lName);
      ConnectionFault(825,CurPos,ImpConstr[Imp]^.fStr);
     end;
    end;
    lSgn.Done;
    
    for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
     inc( gConstrNbr[c], lNbrs[c]);
   end;
 ImpConstr[ImpConstrNbr+1]:=new(ImpConstrPtr,Init(''));
 ImpConstr[ImpConstrNbr+1]^.nBase:=gConstrNbr;
end;

procedure InitDCOBases(const fName:string; var aImp: ImpObj);
 var i,n: integer; lName:string;
 label NextSgn;
begin
 writeln(gLogFile,'  <',fName);
 ConstructorsBase:=ImpConstrNbr;
 gConstrNr.Init(40,40);
 gConstrNr.InsertElem(0);
 ImpConstrBase:=ImpConstrNbr;
 for n:=0 to aImp.fConstrIdents.fCount-1 do
   begin lName:= aImp.fConstrIdents.fList^[n].fString^;
    for i:=1 to ImpConstrNbr do
     if lName=ImpConstr[i]^.fStr then
      begin
       gConstrNr.InsertElem(i);
       goto NextSgn
      end;
    AddConstructorsName(lName);
NextSgn:
   end;
 ReadDCOBases;
 if ImpConstrNbr<>ConstructorsBase then
  begin
   writeln(gLogFile,'   The cutting off constructors:');
   for i:=ConstructorsBase+1 to ImpConstrNbr do
    writeln(gLogFile,'    - ',ImpConstr[i]^.fStr);
  end;
 InitTrans;
 for i:= 1 to gConstrNr.Count-1 do AssignSTables(gConstrNr.Value(i));
end;

procedure RestoreDCOBases;
 var i: integer;
begin
 for i:=ConstructorsBase+2 to ImpConstrNbr+1 do
  begin
   dispose(ImpConstr[i],Done);
  end;
 ImpConstrNbr:=ConstructorsBase;
 with ImpConstr[ImpConstrNbr+1]^ do
  begin
    gConstrNbr:=nBase;
  end;
 DoneTrans;
end;


// ##TODO: it seems that gChosen is always identity and gRefused
//         always empty - are  all the following needed?
// ##TODO: this should be just some 'Accessible' walker  
function AccessibleTermList ( fTrmList:TrmList ): boolean;
begin
 while fTrmList <> nil do
  begin
   if not AccessibleTerm(fTrmList^.XTrmPtr)
    then begin AccessibleTermList:=false; exit end;
   fTrmList:=fTrmList^.NextTrm;
  end;
 AccessibleTermList:=true;
end;

function AccessibleTypeColl (var fTypList:MList): boolean;
 var i: integer;
begin
 for i:=0 to fTypList.Count-1 do
  if not AccessibleType(TypPtr(fTypList.Items^[i])) then
   begin AccessibleTypeColl:=false; exit end;
 AccessibleTypeColl:=true;
end;

function AccessibleCluster(fCluster:AttrCollectionPtr): boolean;
 var i: integer;
begin AccessibleCluster:=false;
 with fCluster^ do
 begin
  for i:=0 to Count-1 do
   begin
    if not gChosen[coAttribute].HasInDom(AttrPtr(Items^[i])^.fAttrNr) then exit;
    if not AccessibleTermList(AttrPtr(Items^[i])^.fAttrArgs) then exit; {??}
   end;
 end;
 AccessibleCluster:=true;
end;

function AccessibleType ( fTyp: TypPtr): boolean;
begin AccessibleType:=false;
 with fTyp^ do
  begin
   if not AccessibleCluster(LowerCluster) then exit;
   case TypSort of
    ikTypStruct:
      AccessibleType:=gChosen[coStructMode].HasInDom(ModNr) and
         AccessibleTermList(ModArgs);
    ikTypMode:
      AccessibleType:=gChosen[coMode].HasInDom(ModNr) and
                   AccessibleTermList(ModArgs);
   end;
  end;
end;

function AccessibleFormulaColl (var fFrmList: MCollection): boolean;
 var i: integer;
begin
 for i:=0 to fFrmList.Count-1 do
  if not AccessibleFormula(FrmPtr(FFrmList.Items^[i]))
   then begin AccessibleFormulaColl:=false; exit end;
 AccessibleFormulaColl:=true;
end;

function AccessibleFormula ( fFrm: FrmPtr): boolean;
begin
  case fFrm^.FrmSort of
   ikFrmNeg: AccessibleFormula:=AccessibleFormula(NegFrmPtr(fFrm)^.NegArg);
    ikFrmConj: AccessibleFormula:=AccessibleFormulaColl(ConjFrmPtr(fFrm)^.Conjuncts);
    ikFrmSchPred: AccessibleFormula:=AccessibleTermList(PredFrmPtr(fFrm)^.PredArgs);
    ikFrmAttr:
     AccessibleFormula:=
        gChosen[coAttribute].HasInDom(PredFrmPtr(fFrm)^.PredNr) and
                         AccessibleTermList(PredFrmPtr(fFrm)^.PredArgs);
    ikFrmPred:
     AccessibleFormula:=
        gChosen[coPredicate].HasInDom(PredFrmPtr(fFrm)^.PredNr) and
                         AccessibleTermList(PredFrmPtr(fFrm)^.PredArgs);
    ikFrmUniv:
     with UnivFrmPtr(fFrm)^ do
      AccessibleFormula:=AccessibleType(Quantified) and
                         AccessibleFormula(Scope);
    ikFrmQual:
     with QualFrmPtr(fFrm)^ do
      AccessibleFormula:=AccessibleTerm(QualTrm) and
                         AccessibleType(QualTyp);
    ikFrmFlexConj:
     with FlexFrmPtr(fFrm)^ do
      AccessibleFormula:=AccessibleFormula(nLeftOrigFrm) and
                         AccessibleFormula(nRightOrigFrm);
    ikFrmVerum,ikError: AccessibleFormula:=true;
    else RunTimeError(2207);
   end;
end;


function AccessibleTerm ( fTrm: TrmPtr ): boolean;
begin
  case TrmPtr(fTrm)^.TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmNumeral,ikTrmIt: AccessibleTerm:=true;
   ikTrmSchFunc: AccessibleTerm:=AccessibleTermList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmFunctor:
    AccessibleTerm:=gChosen[coFunctor].HasInDom(FuncTrmPtr(fTrm)^.FuncNr) and
                     AccessibleTermList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmAggreg:
    AccessibleTerm:=gChosen[coAggregate].HasInDom(FuncTrmPtr(fTrm)^.FuncNr) and
                     AccessibleTermList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmSelector:
    AccessibleTerm:=gChosen[coSelector].HasInDom(FuncTrmPtr(fTrm)^.FuncNr) and
                     AccessibleTermList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
     AccessibleTerm:=AccessibleTypeColl(LambdaArgs) and
                     AccessibleTerm(LambdaScope) and
                     AccessibleFormula(Compr);
   ikTrmChoice: AccessibleTerm:=AccessibleType(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
   else RunTimeError(2208);
  end;
end;

function AccesibleIdentify(fId:IdentifyPtr): boolean;
begin AccesibleIdentify:=false;
  with fId^ do
   begin
    if not AccessibleTypeColl(nPrimaryList) then exit;
    case nConstrKind of
     ikTrmFunctor:
      begin if not AccessibleTerm(TrmPtr(nPattern[0])) then exit;
       if not AccessibleTerm(TrmPtr(nPattern[1])) then exit;
      end;
     ikFrmPred,ikFrmAttr:
      begin if not AccessibleFormula(FrmPtr(nPattern[0])) then exit;
       if not AccessibleFormula(FrmPtr(nPattern[1])) then exit;
      end;
    end;
   end;
  AccesibleIdentify:=true;
end;

function AccesibleReduction(fId:ReductionPtr): boolean;
begin AccesibleReduction:=false;
 with fId^ do
  begin
   if not AccessibleTypeColl(nPrimaryList) then exit;
   if not AccessibleTerm(TrmPtr(nTerms[0])) then exit;
   if not AccessibleTerm(TrmPtr(nTerms[1])) then exit;
  end;
 AccesibleReduction:=true;
end;

function AccesibleProperty(fPr:PropertyPtr): boolean;
begin AccesibleProperty:=false;
  with fPr^ do
   begin
    if not AccessibleTypeColl(nPrimaryList) then exit;
    case PropertyKind(nPropertyKind) of
     sySethood:
      begin
       if not AccessibleType(TypPtr(nObject)) then exit;
      end;
    end;
   end;
  AccesibleProperty:=true;
end;

function AccesibleDefiniens(fDefiniens:DefiniensPtr): boolean;
 var z: integer;
begin AccesibleDefiniens:=false;
  with fDefiniens^ do
   begin
    case nConstr.Kind of
     ikFrmPred:    if not gChosen[coPredicate].HasInDom(nConstr.Nr) then exit;
     ikTypMode:    if not gChosen[coMode].HasInDom(nConstr.Nr) then exit;
     ikTrmFunctor: if not gChosen[coFunctor].HasInDom(nConstr.Nr) then exit;
     ikFrmAttr:    if not gChosen[coAttribute].HasInDom(nConstr.Nr) then exit;
    end;
    if not AccessibleTypeColl(PrimaryList) then exit;
    if (Assumptions <> nil) and not AccessibleFormula(Assumptions) then exit;
    if Definiens=nil then exit;
    with Definiens^ do
     begin
       with nPartialDefinientia do
        for z:=0 to Count-1 do
         with PartDefPtr(Items^[z])^ do
         begin
          case DefSort of
          'm': if not AccessibleFormula(FrmPtr(nPartDefiniens)) then exit;
          'e': if not AccessibleTerm(TrmPtr(nPartDefiniens)) then exit;
          end;
          if not AccessibleFormula(FrmPtr(nGuard)) then exit;
         end;
       if nOtherwise<>nil then
        case DefSort of
        'm': if not AccessibleFormula(FrmPtr(nOtherwise)) then exit;
        'e': if not AccessibleTerm(TrmPtr(nOtherwise)) then exit;
        end;
      end;
   end;
  AccesibleDefiniens:=true;
end;

// ##TODO: this should be just some 'Chosen' walker
procedure ChooseConstr1(aKind: ConstructorsKind; fNr: integer);
 var n: integer;
begin
 if gChosen[aKind].HasInDom(fNr) then exit;
 if gRefused[aKind].HasInDom(fNr) then exit;
 if not gMarked.Marked(aKind,fNr) then
  begin gRefused[aKind].InsertElem(fNr); exit end;
 with ConstrPtr(Constr[aKind].Items^[fNr])^ do
 begin
  if not ChosenTypeColl(nPrimaries) then
  begin gRefused[aKind].InsertElem(fNr); exit end;
  if aKind in TypedConstrKinds then
   with ConstrTypPtr(Constr[aKind].Items^[fNr])^ do
    if not ChosenType(fConstrTyp) then
    begin gRefused[aKind].InsertElem(fNr); exit end;
  if fWhichConstrNr>0 then
  begin ChooseConstr1( aKind, fWhichConstrNr);
  if not gChosen[aKind].HasInDom( fWhichConstrNr) then
    begin gRefused[aKind].InsertElem(fNr); exit end;
  end;
 end;
 gChosen[aKind].InsertElem(fNr);
end;

procedure ChooseFunctor(fNr: integer);
begin ChooseConstr1( coFunctor, fNr); end;

procedure ChooseMode(fNr: integer);
begin ChooseConstr1( coMode, fNr); end;

procedure ChooseAttribute(fNr: integer);
begin  ChooseConstr1( coAttribute, fNr); end;

procedure ChoosePredicate(fNr: integer);
begin  ChooseConstr1( coPredicate, fNr); end;

procedure ChooseSelector(fNr: integer);
begin ChooseConstr1( coSelector, fNr); end;

procedure ChooseAggregate(fNr: integer);
var k: integer;
begin
 if gChosen[coAggregate].HasInDom(fNr) then exit;
 if gRefused[coAggregate].HasInDom(fNr) then exit;
 if not gMarked.Marked(coAggregate,fNr) then
 begin gRefused[coAggregate].InsertElem(fNr); exit end;
 with AggrConstrPtr( Constr[ coAggregate].Items^[fNr])^ do
 begin
  if not ChosenType( fConstrTyp) then
  begin gRefused[coAggregate].InsertElem(fNr); exit end;
  if not ChosenTypeColl(nPrimaries) then
  begin gRefused[coAttribute].InsertElem(fNr); exit end;
  if not ChosenType(fConstrTyp) then
  begin gRefused[coAttribute].InsertElem(fNr); exit end;
  for k:=0 to fAggrColl^.Count-1 do
   with PIntItem(fAggrColl^.Items^[k])^ do
  begin
   ChooseSelector(IntKey);
   if gRefused[coSelector].HasInDom(IntKey) then
   begin gRefused[coAggregate].InsertElem(fNr); exit end;
  end;
  gChosen[coAggregate].InsertElem(fNr);
 end;
end;

procedure ChooseStructMode(fNr: integer);
 var k: integer;
begin
 if gChosen[coStructMode].HasInDom(fNr) then exit;
 if gRefused[coStructMode].HasInDom(fNr) then exit;
 if not gMarked.Marked(coStructMode,fNr) then
 begin gRefused[coStructMode].InsertElem(fNr); exit end;
 with StructConstrPtr( Constr[ coStructMode].At( fNr))^ do 
 begin
  if not ChosenTypeColl(nPrimaries) then
  begin gRefused[coStructMode].InsertElem(fNr); exit end;
  for k:=0 to fPrefixes.Count-1 do
   if not ChosenType(fPrefixes.Items^[k]) then
   begin gRefused[coStructMode].InsertElem(fNr); exit end;
 end;
 gChosen[coStructMode].InsertElem(fNr);
end;

function ChosenTermList ( fTrmList:TrmList ): boolean;
begin
 while fTrmList <> nil do
  begin
   if not ChosenTerm(fTrmList^.XTrmPtr)
    then begin ChosenTermList:=false; exit end;
   fTrmList:=fTrmList^.NextTrm;
  end;
 ChosenTermList:=true;
end;

function ChosenTypeColl (var fTypList:MCollection ): boolean;
 var i: integer;
begin
 for i:=0 to fTypList.Count-1 do
  if not ChosenType(TypPtr(fTypList.Items^[i])) then
   begin ChosenTypeColl:=false; exit end;
 ChosenTypeColl:=true;
end;

function ChosenCluster(fCluster: AttrCollectionPtr ): boolean;
 var i: integer;
begin ChosenCluster:=false;
 with fCluster^ do
  for i:=0 to Count-1 do
   begin ChooseAttribute(AttrPtr(Items^[i])^.fAttrNr);
    if not gChosen[coAttribute].HasInDom(AttrPtr(Items^[i])^.fAttrNr) then exit;
   end;
 ChosenCluster:=true;
end;

function ChosenType ( fTyp: TypPtr): boolean;
begin ChosenType:=false;
 with fTyp^ do
  begin if not ChosenCluster(LowerCluster) then exit;
   case TypSort of
    ikTypStruct:
     begin ChooseStructMode(ModNr);
      if not gChosen[coStructMode].HasInDom(ModNr) then exit;
     end;
    ikTypMode:
     begin ChooseMode(ModNr);
      if not gChosen[coMode].HasInDom(ModNr) then exit;
     end;
   end;
   ChosenType:=ChosenTermList(ModArgs);
  end;
end;

function ChosenFormulaColl (var fFrmList: MCollection): boolean;
 var i: integer;
begin
 for i:=0 to fFrmList.Count-1 do
  if not ChosenFormula(FrmPtr(FFrmList.Items^[i]))
   then begin ChosenFormulaColl:=false; exit end;
 ChosenFormulaColl:=true;
end;

function ChosenFormula ( fFrm: FrmPtr): boolean;
begin ChosenFormula:=false;
  case fFrm^.FrmSort of
   ikFrmNeg: ChosenFormula:=ChosenFormula(NegFrmPtr(fFrm)^.NegArg);
    ikFrmConj: ChosenFormula:=ChosenFormulaColl(ConjFrmPtr(fFrm)^.Conjuncts);
    ikFrmSchPred: ChosenFormula:=ChosenTermList(PredFrmPtr(fFrm)^.PredArgs);
    ikFrmAttr:
     begin ChooseAttribute(PredFrmPtr(fFrm)^.PredNr);
      if not gChosen[coAttribute].HasInDom(PredFrmPtr(fFrm)^.PredNr) then exit;
      ChosenFormula:=ChosenTermList(PredFrmPtr(fFrm)^.PredArgs);
     end;
    ikFrmPred:
     begin ChoosePredicate(PredFrmPtr(fFrm)^.PredNr);
      if not gChosen[coPredicate].HasInDom(PredFrmPtr(fFrm)^.PredNr) then exit;
      ChosenFormula:=ChosenTermList(PredFrmPtr(fFrm)^.PredArgs);
     end;
    ikFrmUniv:
     with UnivFrmPtr(fFrm)^ do
      ChosenFormula:=ChosenType(Quantified) and ChosenFormula(Scope);
    ikFrmQual:
     with QualFrmPtr(fFrm)^ do
      ChosenFormula:=ChosenTerm(QualTrm) and ChosenType(QualTyp);
    ikFrmVerum,ikError: ChosenFormula:=true;
   end;
end;

function ChosenTerm ( fTrm: TrmPtr ): boolean;
begin ChosenTerm:=false;
  case TrmPtr(fTrm)^.TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmConstant,ikTrmNumeral,ikTrmIt: ChosenTerm:=true;
   ikTrmSchFunc: ChosenTerm:=ChosenTermList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmFunctor:
    begin ChooseFunctor(FuncTrmPtr(fTrm)^.FuncNr);
     if not gChosen[coFunctor].HasInDom(FuncTrmPtr(fTrm)^.FuncNr) then exit;
     ChosenTerm:=ChosenTermList(FuncTrmPtr(fTrm)^.FuncArgs);
    end;
   ikTrmAggreg:
    begin ChooseAggregate(FuncTrmPtr(fTrm)^.FuncNr);
     if not gChosen[coAggregate].HasInDom(FuncTrmPtr(fTrm)^.FuncNr) then exit;
     ChosenTerm:=ChosenTermList(FuncTrmPtr(fTrm)^.FuncArgs);
    end;
   ikTrmSelector:
    begin ChooseSelector(FuncTrmPtr(fTrm)^.FuncNr);
     if not gChosen[coSelector].HasInDom(FuncTrmPtr(fTrm)^.FuncNr) then exit;
     ChosenTerm:=ChosenTermList(FuncTrmPtr(fTrm)^.FuncArgs);
    end;
   ikTrmFraenkel:
    with FraenkelTrmPtr(fTrm)^ do
     ChosenTerm:=ChosenTypeColl(LambdaArgs) and
                     ChosenTerm(LambdaScope) and
                     ChosenFormula(Compr);
    ikTrmChoice:
     ChosenTerm:=ChosenType(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
  end;
end;

procedure InitAccessibility;
  var c: ConstructorsKind;
begin
 for c:=coMode to coAggregate do gInaccessible[c].Init(0,100);
end;

procedure FinishAccessibility;
  var c: ConstructorsKind;
begin
 for c:=coMode to coAggregate do gInaccessible[c].Done;
end;

procedure AccessibilityConstr(const fConstr: Lexem);
 var c: ConstructorsKind;
begin
 c:=ConstructorKind(FConstr.Kind);
 case c of
  coMode,
  coFunctor,
  coAttribute,
  coAggregate,
  coSelector:
   AccessibilityConstrTyp( Constr[c].Items^[fConstr.Nr]);
 coPredicate:
   AccessibilityConstructor( Constr[c].Items^[fConstr.Nr]);
 coStructMode:
  with StructConstrPtr( Constr[ coStructMode].At( fConstr.Nr))^ do
  AccessibilityConstrTyp( Constr[ coAggregate].At( fStructModeAggrNr));
 end;
end;

procedure AccessibilityTrmList(fTrmList:TrmList);
begin
 while fTrmList <> nil do
  begin
   AccessibilityTrm(fTrmList^.XTrmPtr);
   fTrmList:=fTrmList^.NextTrm;
  end;
end;

procedure AccessibilityTypColl(const fTypList: MCollection);
   var z: integer;
begin
 with fTypList do for z:=0 to Count-1 do
  AccessibilityTyp(TypPtr(Items^[z]));
end;

procedure AccessibilityAttributes(fCluster:AttrCollectionPtr);
 procedure AccessibilityAttr(Item:AttrPtr);
  begin
   if gInaccessible[coAttribute].HasInDom(AttrPtr(Item)^.fAttrNr) then exit;
   if gChosen[coAttribute].HasInDom(AttrPtr(Item)^.fAttrNr) then exit;
   gInaccessible[coAttribute].InsertElem(AttrPtr(Item)^.fAttrNr);
   if AttrPtr(Item)^.fAttrNr > ImpConstr[ConstructorsBase+1]^.nBase[coAttribute] then exit;
   AccessibilityConstrTyp(Constr[ coAttribute].Items^[AttrPtr(Item)^.fAttrNr]);
  end;
 var i: integer;
begin
 with fCluster^ do
 for i:=0 to Count-1 do
  AccessibilityAttr(AttrPtr(Items^[i]));
end;

procedure AccessibilityTyp(fTyp: TypPtr);
var k: integer; lSelConstr: ConstrTypPtr;
begin
 with TypPtr(fTyp)^ do
  begin
   AccessibilityAttributes(LowerCluster);
   case TypSort of
   ikTypMode:
    begin
     AccessibilityTrmList(ModArgs);
     if gInaccessible[coMode].HasInDom(ModNr) then exit;
     if gChosen[coMode].HasInDom(ModNr) then exit;
     gInaccessible[coMode].InsertElem(ModNr);
     if ModNr > ImpConstr[ConstructorsBase+1]^.nBase[coMode] then exit;
     AccessibilityConstrTyp( Constr[ coMode].At( ModNr));
    end;
   ikTypStruct:
    begin
     AccessibilityTrmList(ModArgs);
     if gInaccessible[coStructMode].HasInDom(ModNr) then exit;
     if gChosen[coStructMode].HasInDom(ModNr) then exit;
     gInaccessible[coStructMode].InsertElem(ModNr);
     if ModNr > ImpConstr[ConstructorsBase+1]^.nBase[coStructMode] then exit;
     with StructConstrPtr( Constr[ coStructMode].At( ModNr))^ do
    begin
     if not gInaccessible[coAggregate].HasInDom(fStructModeAggrNr) and
         not gChosen[coAggregate].HasInDom(fStructModeAggrNr) then
     begin
      gInaccessible[coAggregate].InsertElem(fStructModeAggrNr);
      if fStructModeAggrNr <= ImpConstr[ConstructorsBase+1]^.nBase[coAggregate] then
       AccessibilityConstrTyp(Constr[coAggregate].Items^[fStructModeAggrNr]);
     end;
     for k:=0 to fFields^.Count-1 do
      if not gInaccessible[coSelector].HasInDom(fFields^.Items^[k].X) and
          not gChosen[coSelector].HasInDom(fFields^.Items^[k].X) then
      begin
       gInaccessible[coSelector].InsertElem(fFields^.Items^[k].X);
       if fFields^.Items^[k].X<= ImpConstr[ConstructorsBase+1]^.nBase[coSelector] then
       begin
        lSelConstr:= Constr[coSelector].Items^[ fFields^.Items^[k].X];
        AccessibilityTypColl( lSelConstr^.nPrimaries);
        AccessibilityTyp( lSelConstr^.fConstrTyp);
       end;
      end;
     for k:=0 to fPrefixes.Count-1 do
      AccessibilityTyp(fPrefixes.Items^[k]);
    end;
    end;
   end;
  end;
end;

procedure AccessibilityFrmColl(const fFrmList: MCollection);
  var z: integer;
begin
 with fFrmList do for z:=0 to Count-1 do
  AccessibilityFrm(FrmPtr(Items^[z]));
end;

procedure AccessibilityFrm(fFrm:FrmPtr);
begin
 case FrmPtr(fFrm)^.FrmSort of
  ikFrmNeg: AccessibilityFrm(NegFrmPtr(fFrm)^.NegArg);
  ikFrmConj: AccessibilityFrmColl(ConjFrmPtr(fFrm)^.Conjuncts);
  ikFrmSchPred: AccessibilityTrmList(PredFrmPtr(fFrm)^.PredArgs);
  ikFrmAttr:
   begin
    AccessibilityTrmList(PredFrmPtr(fFrm)^.PredArgs);
    if gInaccessible[coAttribute].HasInDom((PredFrmPtr(fFrm)^.PredNr)) then exit;
    if gChosen[coAttribute].HasInDom(PredFrmPtr(fFrm)^.PredNr) then exit;
    gInaccessible[coAttribute].InsertElem(PredFrmPtr(fFrm)^.PredNr);
    if PredFrmPtr(fFrm)^.PredNr > ImpConstr[ConstructorsBase+1]^.nBase[coAttribute] then exit;
    AccessibilityConstrTyp(Constr[ coAttribute].Items^[PredFrmPtr(fFrm)^.PredNr]);
   end;
  ikFrmPred:
   begin
    AccessibilityTrmList(PredFrmPtr(fFrm)^.PredArgs);
    if gInaccessible[coPredicate].HasInDom((PredFrmPtr(fFrm)^.PredNr)) then exit;
    if gChosen[coPredicate].HasInDom(PredFrmPtr(fFrm)^.PredNr) then exit;
    gInaccessible[coPredicate].InsertElem(PredFrmPtr(fFrm)^.PredNr);
    if PredFrmPtr(fFrm)^.PredNr > ImpConstr[ConstructorsBase+1]^.nBase[coPredicate] then exit;
   end;
  ikFrmUniv:
   begin AccessibilityTyp(UnivFrmPtr(fFrm)^.Quantified);
    AccessibilityFrm(UnivFrmPtr(fFrm)^.Scope)
   end;
  ikFrmQual:
   begin AccessibilityTrm(QualFrmPtr(fFrm)^.QualTrm);
    AccessibilityTyp(QualFrmPtr(fFrm)^.QualTyp)
   end;
  ikFrmVerum,ikError: ;
 end;
end;

procedure AccessibilityTrm(fTrm: TrmPtr);
 var k:integer;
begin
  case TrmPtr(fTrm)^.TrmSort of
   ikTrmLocus,ikTrmBound,ikTrmIt,ikTrmNumeral,ikError: ;
   ikTrmSchFunc: AccessibilityTrmList(FuncTrmPtr(fTrm)^.FuncArgs);
   ikTrmFunctor:
    with FuncTrmPtr(fTrm)^ do
    begin
     AccessibilityTrmList(FuncArgs);
     if gInaccessible[coFunctor].HasInDom(FuncNr) then exit;
     if gChosen[coFunctor].HasInDom(FuncNr) then exit;
     gInaccessible[coFunctor].InsertElem(FuncNr);
     if FuncNr > ImpConstr[ConstructorsBase+1]^.nBase[coFunctor] then exit;
     AccessibilityConstrTyp(Constr[coFunctor].Items^[FuncNr]);
    end;
   ikTrmAggreg:
    with FuncTrmPtr(fTrm)^ do
    begin
     AccessibilityTrmList(FuncArgs);
     if gInaccessible[coAggregate].HasInDom(FuncNr) then exit;
     if gChosen[coAggregate].HasInDom(FuncNr) then exit;
     gInaccessible[coAggregate].InsertElem(FuncNr);
     if FuncNr > ImpConstr[ConstructorsBase+1]^.nBase[coAggregate] then exit;
     with AggrConstrPtr( Constr[ coAggregate].At( FuncNr))^ do
      for k:=0 to fAggrColl^.Count-1 do
       with PIntItem(fAggrColl^.Items^[k])^ do
        if not gInaccessible[coSelector].HasInDom(IntKey) and
            not gChosen[coSelector].HasInDom(IntKey) then
        begin
         gInaccessible[coSelector].InsertElem(IntKey);
         if IntKey <= ImpConstr[ConstructorsBase+1]^.nBase[coSelector] then
          AccessibilityConstrTyp( Constr[coSelector].Items^[IntKey]);
        end;
     AccessibilityConstrTyp( Constr[ coAggregate].At( FuncNr));
    end;
   ikTrmSelector:
    with FuncTrmPtr(fTrm)^ do
    begin
     AccessibilityTrmList(FuncArgs);
     if gInaccessible[coSelector].HasInDom(FuncNr) then exit;
     if gChosen[coSelector].HasInDom(FuncNr) then exit;
     gInaccessible[coSelector].InsertElem(FuncNr);
     if FuncNr > ImpConstr[ConstructorsBase+1]^.nBase[coSelector] then exit;
     AccessibilityConstrTyp( Constr[coSelector].Items^[FuncNr]);
    end;
   ikTrmFraenkel: with FraenkelTrmPtr(fTrm)^ do
     begin
       AccessibilityTypColl(LambdaArgs); AccessibilityTrm(LambdaScope);
       AccessibilityFrm(Compr);
     end;
   ikTrmChoice:
     AccessibilityTyp(ChoiceTrmPtr(fTrm)^.ChoiceTyp);
  end;
end;

procedure AccessibilityConstrTyp(fConstr:ConstrTypPtr);
begin
 with fConstr^ do
  begin
   AccessibilityTyp(fConstrTyp);
   AccessibilityTypColl(nPrimaries);
  end;
end;

procedure AccessibilityConstructor(fConstr:ConstrPtr);
begin
 with fConstr^ do
  begin
   AccessibilityTypColl(nPrimaries);
  end;
end;

procedure InConstrNum;
var
 i,lInt: integer; lInFile: MizInStream;
 c: ConstructorsKind;
begin
 FileExam(EnvFileName+'.cho');
 lInFile.OpenFile(EnvFileName+'.cho');
 for c:=coMode to coAggregate do
   begin
    lInFile.InWord;
    gTrans[c].Init(gMaxConstrNbr[c]+1);
    gTrans[c].Insert(0);
    for i:=1 to lInFile.Current.Nr do
     begin
      lInFile.InInt(lInt);
      gTrans[c].Insert(lInt);
     end;
    lInFile.InInt(gConstrNbr[c]);
   end;
  lInFile.Done;
end;

procedure OpenLogFile;
begin
 assign(gLogFile,MizFileName+'.log');
 rewrite(gLogFile);
 LogOpened:=true;
end;

procedure CloseLogFile;
begin
 close(gLogFile);
 LogOpened:=false;
end;

end.
