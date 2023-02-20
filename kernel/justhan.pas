(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit justhan;

interface

uses mobjects,errhan,inout,limits,correl;

type
 PropositionPtr = ^PropositionObj;
 PropositionObj =
  object(MObject)
    nLabNr,nLabId:integer;
    nPos:Position;
    nSentence:FrmPtr;
   constructor Init(fLabNr,fLabId:integer; fSentence:FrmPtr; fPos:Position);
   destructor Done; virtual;
  end;

 RefSntArr = array[1..MaxPremNbr] of FrmPtr;
 PReference = ^TReference;
 TReference =
   object(MObject)
     nRefSort: char;
     RefPos: Position;
    procedure CollectRef; virtual;
    procedure CollectRef1; virtual;
   end;
 PPrivateReference = ^TPrivateReference;
 TPrivateReference =
   object(TReference)
    LabNr: integer;
    nLabId: integer;
    constructor Init(fLabNr,fLabId:integer; fPos:Position);
    procedure CollectRef; virtual;
    procedure CollectRef1; virtual;
   end;
 PLibraryReference = ^TLibraryReference;
 TLibraryReference =
   object(TReference)
    ArticleNr: integer;
   end;
 PTheoremReference = ^TTheoremReference;
 TTheoremReference =
   object(TLibraryReference)
    TheoNr:integer;
    constructor Init(fArticleNr,fTheoNr:integer; fPos:Position);
    procedure CollectRef; virtual;
    procedure CollectRef1; virtual;
   end;
 PDefinitionReference = ^TDefinitionReference;
 TDefinitionReference =
   object(TLibraryReference)
    DefNr:integer;
    constructor Init(fArticleNr,fDefNr:integer; fPos:Position);
    procedure CollectRef; virtual;
    procedure CollectRef1; virtual;
   end;

 InferencePtr = ^InferenceObj;
 InferenceObj =
  object(MObject)
   nInferSort: char; { ikInfBy lub ikInfFrom }
   nSchFileNr: integer; {0 dla odwolan do biezacego artykulu }
   nSchemeNr: integer; { 0 dla standardowych inferencji }
   nLinked: boolean;
   nReferences: MCollection;
   nPos: Position;
   constructor Init;
   destructor Done; virtual;
  end;

 IterStepPtr = ^IterStepObj;
 IterStepObj =
  object(InferenceObj)
   nEquatedTrm: TrmPtr;
   constructor Init(fTrm:TrmPtr; const fInference:InferenceObj);
   destructor Done; virtual;
   procedure Justify; virtual;
  end;

 SntRefPtr = ^SntRefItem;
 SntRefItem = object(IntPairItem)
    fSnt: FrmPtr;
   constructor Init(aMMLNr,aNr: integer; aFrm:FrmPtr);
 end;

function EqRefs(fRef1,fRef2: PReference): boolean;

// The Proposition array starts from DefaultPreviousLab,
// normal labels start from DefaultCurrentLab + 1
const DefaultCurrentLab  =  0;
const DefaultPreviousLab =  DefaultCurrentLab - 1;

var
  TheoReferNbr,DefReferNbr: BinIntFunc;

  InferOK: boolean;
  PremNbr: integer;
  Reference: RefSntArr;
  RefPos: array[1..MaxPremNbr] of Position;
  Proposition: array[DefaultPreviousLab..MaxLabNbr] of FrmPtr;
// ##TODO: replace the old CollectRef with CollectRef1
  OrdTheorem,DefTheorem: IntPairKeyCollection;

{$IFDEF CHSTAT}
type
 CHStatPtr = ^CHStatObj;
 CHStatObj =
   object(MObject)
     nInfPos:Position;
     nClausesNbr,nContrNr: integer;
     nInstStats: IntSequence;
    constructor Init(aPos:Position);
    destructor Done; virtual;
   end;
var
  gStat: MCollection;
{$ENDIF}

procedure ReferNbrLoad(var aRefs: BinIntFunc);
procedure InsertRef(fPos: Position; fSnt:FrmPtr);

implementation

uses propcoll,lexicon,xmlpars,xmldict
{$IFDEF MDEBUG}
 ,info,outinfo
{$ENDIF};

{$IFDEF CHSTAT}
constructor CHStatObj.Init(aPos:Position);
begin
  nInfPos:=aPos;
  nClausesNbr:=0;
  nContrNr:=-1;
  nInstStats.Init(0);
end;

destructor CHStatObj.Done;
begin
 nInstStats.Done;
 inherited Done;
end;
{$ENDIF}

constructor PropositionObj.Init(fLabNr,fLabId:integer; fSentence:FrmPtr; fPos:Position);
begin inherited Init;
 nLabNr:=fLabNr; nLabId:= fLabId;
 nSentence:=fSentence; nPos:=fPos;
end;

destructor PropositionObj.Done;
begin dispose(nSentence,Done);
 inherited Done;
end;

constructor InferenceObj.Init;
begin
 nReferences.Init(0,8);
 nSchFileNr:= 0;
 nSchemeNr:=0;
 nLinked:=false;
end;

destructor InferenceObj.Done;
begin nReferences.Done;
 inherited Done;
end;

constructor IterStepObj.Init(fTrm:TrmPtr; const fInference:InferenceObj);
begin
 nInferSort:=fInference.nInferSort;
 nSchFileNr:=fInference.nSchFileNr;  
 nSchemeNr:=fInference.nSchemeNr;
 nLinked:=fInference.nLinked;
 nReferences.Init(0,0);
 nReferences:=fInference.nReferences;
 nPos:=fInference.nPos;
 nEquatedTrm:=fTrm;
end;

destructor IterStepObj.Done;
begin DisposeTrm(nEquatedTrm);
 inherited Done;
end;

procedure IterStepObj.Justify;
begin abstract1; end;

constructor TPrivateReference.Init(fLabNr,fLabId:integer; fPos:Position);
begin nRefSort:='l'; LabNr:=fLabNr; nLabId:= fLabId; RefPos:=fPos end;

constructor TTheoremReference.Init(fArticleNr,fTheoNr:integer; fPos:Position);
begin nRefSort:='t'; ArticleNr:=fArticleNr; TheoNr:=fTheoNr; RefPos:=fPos end;

constructor TDefinitionReference.Init(fArticleNr,fDefNr:integer; fPos:Position);
begin nRefSort:='d'; ArticleNr:=fArticleNr; DefNr:=fDefNr; RefPos:=fPos end;

procedure TReference.CollectRef;
begin Abstract1 end;

procedure TReference.CollectRef1;
begin Abstract1 end;

procedure InsertRef(fPos: Position; fSnt:FrmPtr);
begin if fSnt^.FrmSort=ikError then InferOK:=false;
 if PremNbr < MaxPremNbr then
  begin
   inc(PremNbr);
   Reference[PremNbr]:=fSnt;
   RefPos[PremNbr]:=fPos;
  end
 else if InferOK then
  begin InferOK:=false;
   Error(fPos,473);
  end;
//  RunTimeError(2131);
end;

procedure TPrivateReference.CollectRef;
begin InsertRef(RefPos,Proposition[LabNr]) end;

procedure TPrivateReference.CollectRef1;
begin InsertRef(RefPos,Propositions^.GetLabeled(LabNr)^.nSentence); end;

procedure TTheoremReference.CollectRef;
 var lFrm:SntRefPtr;
begin
 if ArticleNr=0 then begin InferOK:=false; exit end;
 lFrm:=SntRefPtr(OrdTheorem.FirstThat(ArticleNr));
 if lFrm = nil then
  begin Error(RefPos,190); InferOK:=false; exit end;
 lFrm:=SntRefPtr(OrdTheorem.ObjectOf(ArticleNr,TheoNr));
 if (lFrm = nil) or (lFrm^.fSnt^.FrmSort = ikFrmVerum) then
  begin Error(RefPos,192); InferOK:=false; exit end;
 InsertRef(RefPos,lFrm^.fSnt);
end;

procedure TTheoremReference.CollectRef1;
begin CollectRef end;

procedure TDefinitionReference.CollectRef;
 var lFrm:SntRefPtr;
begin
 if ArticleNr=0 then begin InferOK:=false; exit end;
 lFrm:=SntRefPtr(DefTheorem.FirstThat(ArticleNr));
 if lFrm = nil then
  begin Error(RefPos,190); InferOK:=false; exit end;
 lFrm:=SntRefPtr(DefTheorem.ObjectOf(ArticleNr,DefNr));
 if (lFrm = nil) or (lFrm^.fSnt^.FrmSort = ikFrmVerum) then
  begin Error(RefPos,192); InferOK:=false; exit end;
 InsertRef(RefPos,lFrm^.fSnt);
end;

constructor SntRefItem.Init(aMMLNr,aNr: integer; aFrm: FrmPtr);
begin
 fKey.X:=aMMLNr;
 fKey.Y:=aNr;
 fSnt:=aFrm;
end;

procedure TDefinitionReference.CollectRef1;
begin CollectRef end;

procedure ReferNbrLoad(var aRefs: BinIntFunc);
 var lIntTriplet: IntTriplet;
begin
 with aRefs do
 begin Init(100);
  Assign(0,0,0);
  InFile.InWord;
  while InFile.Current.Kind <> ';' do
  with lIntTriplet do
  begin
   InFile.InInt(X1); InFile.InInt(X2);
   InFile.InInt(Y);
   Insert(lIntTriplet);
   InFile.InWord;
  end;
 end;
end;

const errUnknownReferenceType = 8000;

function EqRefs(fRef1,fRef2: PReference): boolean;
begin
 EqRefs := false;
 if fRef1^.nRefSort = fRef2^.nRefSort then
 case fRef1^.nRefSort of
 'l':
  EqRefs:=PPrivateReference(fRef1)^.LabNr = PPrivateReference(fRef2)^.LabNr;
 't':
  EqRefs:= (PTheoremReference(fRef1)^.ArticleNr = PTheoremReference(fRef2)^.ArticleNr)
   and (PTheoremReference(fRef1)^.TheoNr = PTheoremReference(fRef2)^.TheoNr);
 'd':
  EqRefs:= (PDefinitionReference(fRef1)^.ArticleNr= PDefinitionReference(fRef2)^.ArticleNr)
   and (PDefinitionReference(fRef1)^.DefNr = PDefinitionReference(fRef2)^.DefNr)
 else RunTimeError(errUnknownReferenceType);
 end;
end;

end.
