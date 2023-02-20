(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

(*********************************************************
    High level import objects used in accomodator and
    transferer, possibly also in some new environment
    pruning utilities; redefined for xml in ximpobjs
 *********************************************************)

unit impobjs;

interface

uses mobjects,inoutmml,inout,formats,dicthan,enums,iocorrel;
type
{ for use by ImpRequirementsObj}
 ReqPtr =  ^ ReqObj;
 ReqObj = object(MObject)
   fConstr : Lexem;
   fReqNr  : integer;
  constructor Init(aKind: char; aNr,aReqNr: integer);
  destructor Done; virtual;
 end;

{ used for import objects ... not yet}
ImportKind = (impUnknown,impConstructors,impNotation,impClusters,impIdentify,impReduction,
        impProperties,
	      impDefinientia,impTheorems,impSchemes,impRequirements
	      { special marking objs for trnasferer}
	      ,impMultConstr,impMarkNotation
	      );

 ImpMultConstrPtr      = ^ ImpMultConstrObj;
 ImpConstructorsPtr    = ^ ImpConstructorsObj;
 ImpTheoremsPtr	       = ^ ImpTheoremsObj;
 ImpRequirementsPtr    = ^ ImpRequirementsObj;
 ImpSchemePtr          = ^ ImpSchemeObj;
 ImpDefinientiaPtr     = ^ ImpDefinientiaObj;
 ImpClustersPtr	       = ^ ImpClustersObj;
 ImpIdentifyPtr	       = ^ ImpIdentifyObj;
 ImpReductionPtr       = ^ ImpReductionObj;
 ImpPropertiesPtr      = ^ ImpPropertiesObj;
 ImpNotationPtr	       = ^ ImpNotationObj;
 ImpNotationMarkingPtr = ^ ImpNotationMarkingObj;
 ImpPtr		       = ^ ImpObj;

 ImpObj = object(MObject)
   fImpKind: ImportKind;
   fConstrIdents: MStringList;
   { contains pointers to constrs, Patterns etc.}
{   fImported:MList;} { content depends on kind}
  constructor Init;
  destructor Done; virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
{  constructor GetFromLibr(const aName: string);
  procedure Get; abstract;
  procedure Put; virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  procedure GetForTransfer;}
 end;

(*** inoutmml ***)
 ImpConstructorsObj = Object(ImpObj)
    fConstrNbr: ConstrIntArr;
    fConstructors: MList;
   constructor Init;
   constructor GetConstructors(const aPath: string);
   destructor Done; virtual;
  end;

{ CAUTION!
  order of sgns when inserted into in ImpMultConstrObj is important for
  Renumerate! }
 ImpMultConstrObj = Object(ImpObj)
   fConstrNbr,fBases: ConstrIntArr; {fBases saves the bases before dco}
   fImpSgn:MList; { of ImpConstrPtrs; ImpAttrSgn from transferer; watch
                    for MaxImpNbr}
   fSgnMarks:NatFunc;	{ keeps marks for fImpSgn}
   fConstrCounts:ConstrCounterObj; { keeps marks fo fConstr}
   fConstrs: ConstrMListArr; { MLists of ConstrPtrs}
   constructor Init;
   procedure AddBases2Sgn;
   procedure AssignGTrans;
   constructor LoadDCO(const aPath:string);
   constructor GetACO;      {transferer}
   function AddDCO:boolean; virtual; {transferer}
   procedure InitMark;
   procedure Mark1Sgn(fNr:integer);
   procedure Mark1DCO;
   procedure Mark(const fFrom:ConstrIntArr);
   procedure Renumerate(AddArticleConstrs:boolean);
   procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
   destructor Done; virtual;
{$IFDEF MDEBUG}
   procedure InfoBase;
{$ENDIF}
  end;

 ImpTheoremsObj = object(ImpObj)
   fTheorems: MList;
  constructor Init;
  constructor GetTheorems(const aPath: string);
  procedure PutTheorems(const aPath: string);virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  destructor Done; virtual;
 end;

 ImpRequirementsObj = object(ImpObj)
   fRequirements: MList;
  constructor Init;
  constructor GetRequirements(const aPath: string);
{  procedure PutRequirements(const aName: string);}
  destructor Done; virtual;
 end;

 ImpSchemeObj = object(ImpObj)
   fSchemes: MList;
  constructor Init;
  constructor GetSchemes(const aPath: string);
  procedure PutSchemes(const aPath: string);virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  destructor Done; virtual;
 end;

 ImpDefinientiaObj = object(ImpObj)
   fDefinientia: MList;
  constructor Init;
  constructor GetDefinientia(const aPath: string);
  procedure PutDefinientia(const aPath: string); virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  destructor Done; virtual;
 end;

 ImpClustersObj = object(ImpObj)
   fClusters: MList;
  constructor Init;
  constructor GetClusters(const aPath: string);
  procedure PutClusters(const aPath: string);virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  destructor Done; virtual;
 end;

 ImpIdentifyObj = object(ImpObj)
   fIdentify: MList;
  constructor Init;
  constructor GetIdentify(const aPath: string);
  procedure PutIdentify(const aPath: string);virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  destructor Done; virtual;
 end;

 ImpReductionObj = object(ImpObj)
   fReductions: MList;
  constructor Init;
  constructor GetReductions(const aPath: string);
  procedure PutReductions(const aPath: string);virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  destructor Done; virtual;
 end;

 ImpPropertiesObj = object(ImpObj)
   fProperties: MList;
  constructor Init;
  constructor GetProperties(const aPath: string);
  procedure PutProperties(const aPath: string);virtual;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  procedure  Mark(var fCounts:ConstrCounterObj);
  destructor Done; virtual;
 end;

 ImpFormatsObj = object(MObject)
   fVocabularies: MStringList; { this is NOT sorted!}
   fFormats: FormatsList;
  constructor Init;
  constructor GetFormats(const aPath: string);
  procedure PutFormats(const aPath: string);
  destructor Done; virtual;
 end;

 ImpFormatsMarkingObj = object(ImpFormatsObj)
   fDictMarks:NatFunc;	{ keeps marks for fVocabularies}
   fSymbolTrans: SymbolIntSeqArr;
  constructor Init;
  constructor GetFormats(const aPath: string);
  procedure MarkDicts;
  procedure Renumerate;
  procedure PutMarked(const aName:string); virtual;
  destructor Done; virtual;
 end;

 {TODO: it seems fFormats is not needed}
 ImpNotationObj = object(ImpObj)
   fVocabularies: MStringList; { this is NOT sorted!}
   fFormats: FormatsList;
   fNotation: MList;
  constructor Init;
  constructor GetNotation(const aPath: string);
  procedure PutNotation(const aPath: string);
  procedure  Mark(var fCounts:ConstrCounterObj); {used rather for ImpNotationMarkingObj }
  destructor Done; virtual;
 end;

 { used for marking of formats in transferer}
{TODO: array['A'..'Z'] should be consistently replced by
array[SymbolKind] everywhere in the source}
 ImpNotationMarkingObj = object(ImpNotationObj)
   fDictMarks:NatFunc;	{ keeps marks for fVocabularies}
   fSymbolTrans: SymbolIntSeqArr;
  constructor Init;
  constructor GetNotation(const aPath: string);
  procedure MarkDicts;
  procedure Renumerate;
  procedure PutMarked(fMConstr:ImpMultConstrPtr;const aName:string); virtual;
  destructor Done; virtual;
 end;

implementation
uses lexicon,errhan,correl,mizenv,builtin,schemhan,
     identify,xml_parser,xmldict,xmlpars
{$IFDEF MDEBUG}
,info,outinfo
{$ENDIF}
;

constructor ImpObj.Init;
begin
 fImpKind:= impUnknown;
 fConstrIdents.Init(0);
end;

destructor ImpObj.Done;
begin
 fConstrIdents.Done;
end;

procedure ImpObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName:string);
begin end;

{ not done yet, could be more general approach to ImpObjs}
function NewImpObj(fImpKind:ImportKind):ImpPtr;
begin
  case fImpKind of
  impUnkNown	  : NewImpObj:= new(ImpPtr,Init);
  impConstructors : NewImpObj:= new(ImpConstructorsPtr,Init);
  impNotation	  : NewImpObj:= new(ImpNotationPtr,Init);
  impClusters	  : NewImpObj:= new(ImpClustersPtr,Init);
  impIdentify	  : NewImpObj:= new(ImpIdentifyPtr,Init);
  impReduction	  : NewImpObj:= new(ImpReductionPtr,Init);
  impProperties	  : NewImpObj:= new(ImpPropertiesPtr,Init);
  impDefinientia  : NewImpObj:= new(ImpDefinientiaPtr,Init);
  impTheorems	  : NewImpObj:= new(ImpTheoremsPtr,Init);
  impSchemes	  : NewImpObj:= new(ImpSchemePtr,Init);
  impRequirements : NewImpObj:= new(ImpRequirementsPtr,Init);
  impMultConstr	  : NewImpObj:= new(ImpMultConstrPtr,Init);
  impMarkNotation : NewImpObj:= new(ImpNotationMarkingPtr,Init);
  end;
end; { NewImpObj }

constructor ImpFormatsObj.Init;
begin
 fVocabularies.Init(0);
 fFormats.Init(0);
end;

constructor ImpFormatsObj.GetFormats(const aPath: string);
var lInMMLFile: InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 lInMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with lInMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elFormats);
  NextElementState;
  lInMMLFile.In_Vocs( fVocabularies);
  fFormats.Init(8);
  while not (nState = eEnd) do
  begin
   XMLASSERT(nElKind = elFormat);
   fFormats.Insert( In_Format(lInMMLFile^));
  end;
 end;
 dispose(lInMMLFile,Done);
end;

procedure ImpFormatsObj.PutFormats(const aPath: string);
var lOutMMLFile: OutMMLFileObj; i: integer;
begin
 lOutMMLFile.OpenFile(aPath);
 lOutMMLFile.Out_XElStart0( elFormats);
 lOutMMLFile.Out_Vocs( fVocabularies);
 with fFormats do
  for i:=0 to Count-1 do
   Out_Format( lOutMMLFile, Items^[i],0);
 lOutMMLFile.Out_XElEnd( elFormats);
 lOutMMLFile.Done;
end;

destructor ImpFormatsObj.Done;
begin
 fVocabularies.Done;
 fFormats.Done;
 inherited Done;
end;

constructor ImpFormatsMarkingObj.Init;
begin
  inherited Init;
  fDictMarks.InitNatFunc(0,0);
end;

constructor ImpFormatsMarkingObj.GetFormats(const aPath: string);
begin
  inherited GetFormats(aPath);
end;

procedure ImpFormatsMarkingObj.MarkDicts;
var oldcumulnbr,cumulnbr: array['A'..'Z'] of integer;
    i,j:integer;
    c:char;
begin
  fDictMarks.InitNatFunc(fVocabularies.fCount,0);
  fillchar(cumulnbr,sizeof(cumulnbr),0);
  for j:=0 to fVocabularies.fCount-1 do
    with fVocabularies,AbsVocabularyPtr(GetObject(j))^ do
    begin
      for c:='A' to 'Z' do if c in AvailableSymbols then
      begin
        oldcumulnbr[c] := cumulnbr[c];
        inc(cumulnbr[c],fSymbolCnt[c]);
      end;
      for i:=0 to fFormats.Count-1 do
      with FormatPtr(fFormats.Items^[i])^,fSymbol do
       if (Nr <= cumulnbr[Kind]) and (oldcumulnbr[Kind] < Nr) then
        begin
         fDictMarks.Up(j);
         break;
        end
       else if (Kind = 'K') then with BracketFormatPtr(fFormats.Items^[i])^ do
        if (fRightSymbolNr <= cumulnbr['L'])
             and (oldcumulnbr['L'] < fRightSymbolNr) then
         begin
          fDictMarks.Up(j);
          break;
        end;
    end;
end; { ImpFormatsMarkingObj.MarkDicts }

procedure ImpFormatsMarkingObj.Renumerate;
var i,j:integer;
c:char;
_last,pos:array['A'..'Z'] of integer;
begin
  for c:='A' to 'Z' do if c in AvailableSymbols then
  begin
    fSymbolTrans[c].Init(MaxRenumNbr);
    fSymbolTrans[c].Insert(0);
    fSymbolTrans[c].fCount := MaxRenumNbr-1;
    { only AtPut can be used from this place on}
    pos[c] := 0; { cumulative count}
    _last[c] := 0; {counts selected}
  end;

  for i:=0 to fVocabularies.fCount-1 do
  with fVocabularies,AbsVocabularyPtr(GetObject(i))^ do
   begin
    for c:='A' to 'Z' do if c in AvailableSymbols then
     for j:=0 to fSymbolCnt[c]-1 do
      begin
       inc(pos[c]);
       if fDictMarks.HasInDom(i) then
        begin
         inc(_last[c]);
         fSymbolTrans[c].AtPut(pos[c],_last[c]);
        end;
      end;
   end;
end; { ImpFormatsMarkingObj.Renumerate }

procedure ImpFormatsMarkingObj.PutMarked(const aName: string);
var lOutMMLFile: OutMMLFileObj; i: integer;
begin
 lOutMMLFile.OpenFile(aName);
 lOutMMLFile.Out_XElStart0( elFormats);
 lOutMMLFile.OutMarkedVocs(fVocabularies,fDictMarks);
 with fFormats do
  for i:=0 to Count-1 do
   Out_Format(lOutMMLFile,Items^[i],0);
 lOutMMLFile.Out_XElEnd( elFormats);
 lOutMMLFile.Done;
end; { ImpFormatsMarkingObj.PutMarked }

destructor ImpFormatsMarkingObj.Done;
var c:char;
begin
 for c:='A' to 'Z' do if c in AvailableSymbols then
  fSymbolTrans[c].Done;
 fDictMarks.Done;
 inherited Done;
end; { ImpFormatsMarkingObj.Done }

{ }

constructor ImpNotationObj.Init;
begin
 fImpKind:= impNotation;
 fConstrIdents.Init(0);
 fVocabularies.Init(0);
 fFormats.Init(0);
 fNotation.Init(8);
end;

constructor ImpNotationObj.GetNotation(const aPath: string);
var lInMMLFile: InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impNotation;
 fNotation.Init(50);
 lInMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with lInMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elNotations);
  NextElementState;
  lInMMLFile.GetConstrNames(fConstrIdents);
  lInMMLFile.In_Vocs( fVocabularies);
  fFormats.Init(8);
  while not (nState = eEnd) do
  begin
   XMLASSERT(nElKind = elPattern);
   fNotation.Insert( lInMMLFile.In_PatternWithFormat);
  end;
 end;
 dispose(lInMMLFile,Done);
end;

{ this is used for ImpNotationMarkingObj too}
procedure  ImpNotationObj.Mark(var fCounts:ConstrCounterObj);
var i:integer;
begin
  with fNotation do
  for i:=0 to Count-1 do
    fCounts.MarkPatternObj(Items^[i]);
end; { ImpNotationObj.Mark }

procedure ImpNotationObj.PutNotation(const aPath: string);
var lOutMMLFile: OutMMLFileObj; i: integer;
begin
 lOutMMLFile.OpenFile(aPath);
 lOutMMLFile.Out_XElStart0( elNotations);
 lOutMMLFile.PutConstrNames(fConstrIdents);
 lOutMMLFile.Out_Vocs( fVocabularies);
 with fNotation do
  for i:=0 to Count-1 do
   lOutMMLFile.Out_PatternWithFormat(Items^[i],
                                     PatternPtr( Items^[i])^.fFormat);
 lOutMMLFile.Out_XElEnd( elNotations);
 lOutMMLFile.Done;
end;

destructor ImpNotationObj.Done;
begin
 fVocabularies.Done;
 fNotation.Done;
 inherited Done;
end;

constructor ImpNotationMarkingObj.Init;
begin
  inherited Init;
  fDictMarks.InitNatFunc(0,0);
  fImpKind:= impMarkNotation;
end;
{fDictMarks is initialised in MarkDict
fSymbolTrans is initialised in Renumerate}
{ so maybe this has no right to exist}

constructor ImpNotationMarkingObj.GetNotation(const aPath: string);
begin
  inherited GetNotation(aPath);
  fImpKind:= impMarkNotation;
end;

{ fixed for noncummulative counts}
{TODO: corresponds to the original but one of the
inequalities should have < instead of <= probably}
{ done it, but should check how this really works in the
transferer}
procedure ImpNotationMarkingObj.MarkDicts;
var oldcumulnbr,cumulnbr: array['A'..'Z'] of integer;
i,j:integer;
c:char;
begin
  fDictMarks.InitNatFunc(fVocabularies.fCount,0);
  fillchar(cumulnbr,sizeof(cumulnbr),0);

  for j:=0 to fVocabularies.fCount-1 do
    with fVocabularies,AbsVocabularyPtr(GetObject(j))^ do
    begin
      for c:='A' to 'Z' do if c in AvailableSymbols then
      begin
        oldcumulnbr[c] := cumulnbr[c];
	inc(cumulnbr[c],fSymbolCnt[c]);
      end;
      for i:=0 to fNotation.Count-1 do
      with PatternPtr(fNotation.Items^[i])^,fFormat^,fSymbol do
        if (Nr <= cumulnbr[Kind]) and (oldcumulnbr[Kind] < Nr) then
        begin
         fDictMarks.Up(j);
	 break;
        end
	else if (Kind = 'K') then with BracketFormatPtr(fFormat)^ do
	if (fRightSymbolNr <= cumulnbr['L'])
	and (oldcumulnbr['L'] < fRightSymbolNr) then
	begin
	  fDictMarks.Up(j);
	  break;
	end;
    end;
end; { ImpNotationMarkingObj.MarkDicts }

{ fixed for noncummulative counts}
procedure ImpNotationMarkingObj.Renumerate;
var i,j:integer;
c:char;
_last,pos:array['A'..'Z'] of integer;
begin
  for c:='A' to 'Z' do if c in AvailableSymbols then
  begin
    fSymbolTrans[c].Init(MaxRenumNbr);
    fSymbolTrans[c].Insert(0);
    fSymbolTrans[c].fCount := MaxRenumNbr-1;
    { only AtPut can be used from this place on}
    pos[c] := 0; { cumulative count}
    _last[c] := 0; {counts selected}
  end;

  for i:=0 to fVocabularies.fCount-1 do
  with fVocabularies,AbsVocabularyPtr(GetObject(i))^ do
   begin
    for c:='A' to 'Z' do if c in AvailableSymbols then
    for j:=0 to fSymbolCnt[c]-1 do begin
      inc(pos[c]);
      if fDictMarks.HasInDom(i) then begin
        inc(_last[c]);
	fSymbolTrans[c].AtPut(pos[c],_last[c]);
      end;
    end;
   end;
end; { ImpNotationMarkingObj.Renumerate }

procedure ImpNotationMarkingObj.PutMarked(fMConstr:ImpMultConstrPtr;
                                          const aName: string);
var lOutMMLFile: OutMMLFileObj; i: integer;
begin
 lOutMMLFile.OpenFile(aName);
 lOutMMLFile.Out_XElStart0( elNotations);
 with fMConstr^ do
  lOutMMLFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
 lOutMMLFile.OutMarkedVocs(fVocabularies,fDictMarks);
 with fNotation do
  for i:=0 to Count-1 do
   lOutMMLFile.Out_PatternWithFormat(Items^[i],
                                     PatternPtr( Items^[i])^.fFormat);
 lOutMMLFile.Out_XElEnd( elNotations);
 lOutMMLFile.Done;
end; { ImpNotationMarkingObj.PutMarked }

destructor ImpNotationMarkingObj.Done;
var c:char;
begin
 for c:='A' to 'Z' do if c in AvailableSymbols then
    fSymbolTrans[c].Done;
 fDictMarks.Done;
 inherited Done;
end; { ImpNotationMarkinge }

constructor ImpConstructorsObj.Init;
begin
 fImpKind:= impConstructors;
 fConstrIdents.Init(0);
 fillchar(fConstrNbr,sizeof(fConstrNbr),0);
 fConstructors.Init(8);
end;

constructor ImpConstructorsObj.GetConstructors(const aPath: string);
var InMMLFile : InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impConstructors;
 fConstructors.Init(50);
 fillchar(fConstrNbr,sizeof(fConstrNbr),0);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elConstructors);
  NextElementState;
  GetConstrNames(fConstrIdents);
  In_ConstrCounts( fConstrNbr);
  while not (nState = eEnd) do
   fConstructors.Insert( In_Constructor1);
 end;
 dispose(InMMLFile,Done);
end; { ImpConstructorsObj.GetConstructors } 

{like AddBases or ReadDCO, but uses fImpSgn instead of fConstrIdents,
to do marking etc. (unlike AddBases, the bases are increased each time
to correspond to .sgl or new .aco reading (like in ReadDCO))}
procedure ImpMultConstrObj.AddBases2Sgn;
 var i: integer; lSgn: MStringList;
     lName,lPath : string;
     c		 : ConstructorsKind; 
     lImpConstr	 : ImpConstrPtr;
     InMMLFile	 : InMMLFilePtr;
begin
 with fImpSgn do
 begin
  for i:= 0 to Count-1 do
  begin
   InMMLFile:=new(InMMLFilePtr,Init);
   lImpConstr := fImpSgn.Items^[i];
   lPath:=InMMLFile.AssignedLibr(lImpConstr^.fStr,'.dco');
   if lPath = '' then RunTimeError(808);
   with InMMLFile^ do
   begin
    NextElementState;
    XMLASSERT( nElKind = elConstructors);
    NextElementState;
    GetConstrNames( lSgn); lSgn.Done; // just skipping
    In_ConstrCounts( lImpConstr^.nBase);
   end;
   dispose(InMMLFile,Done);
  end;
  for i:= 1 to Count-1 do with ImpConstrPtr(Items^[i])^ do
   for c := Low(ConstructorsKind) to High(ConstructorsKind) do
    inc(nBase[c],ImpConstrPtr(Items^[i-1])^.nBase[c]);
 end;
end; { ImpMultConstrObj.AddBases2Sgn }

procedure ImpMultConstrObj.AssignGTrans;
 var j,i: integer; c: ConstructorsKind;
begin
  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  begin
    gTrans[c].Init(MaxRenumNbr);
    gTrans[c].Insert(0);
  end;
  with fImpSgn do begin
   for c:=Low(ConstructorsKind)  to High(ConstructorsKind)  do
    for j:=1 to ImpConstrPtr(Items^[0])^.nBase[c] do
     gTrans[c].Insert(j);
   for i:= 1 to Count-1 do with ImpConstrPtr(Items^[i-1])^ do
    for c:=Low(ConstructorsKind)  to High(ConstructorsKind)  do
     for j:=nBase[c]+1 to ImpConstrPtr(fImpSgn.Items^[i])^.nBase[c] do
       gTrans[c].Insert(j);
  end;
end; { ImpMultConstrObj.AssignGTrans }

constructor  ImpMultConstrObj.LoadDCO(const aPath:string);
var lImpConstr:ImpConstrPtr;
    i : integer;
    InMMLFile: InMMLFilePtr;
begin
  if aPath =  '' then begin Init; exit end;

  fImpKind:= impMultConstr;
  fImpSgn.Init(16);
  fSgnMarks.InitNatFunc(0,16);
  fConstrCounts.Init;

  InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
  with InMMLFile^ do
   begin
    NextElementState;
//    XMLASSERT( nElKind = elConstructors);
    NextElementState;
    GetConstrNames( fConstrIdents);
   end;
  dispose(InMMLFile,Done);
  with fConstrIdents do
  for i:=0 to fCount-1 do
  begin
    lImpConstr:= new(ImpConstrPtr,Init(GetString(i)));
    fImpSgn.Insert(lImpConstr);
  end;
  AddBases2Sgn;
  AssignGTrans;
end; { ImpMultConstrObj.LoadDCO }

{ for transferer; bases are in the fConstrNbr field; why is the
first mode added here?? ... commented and seems to work}
{ clustercollecting is done in the origibal version, but no gTTrans;
clustre collecting is not done here ... seems useless, but can be added }

constructor ImpMultConstrObj.GetACO;
var
  c:ConstructorsKind;
  lConstr:ConstrPtr;
  lImpConstr:ImpConstrPtr;
  lName:string;
  InMMLFile: InMMLFilePtr;
begin
 FileExam(MizFileName+'.aco');
 InMMLFile:=new(InMMLFilePtr,OpenFile(MizFileName+'.aco'));
 fImpKind:= impMultConstr;
 fConstrCounts.Init;
 fImpSgn.Init(20);
 fConstrIdents.Init(0);
 fillchar(fConstrNbr,sizeof(fConstrNbr),0);
 for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
    fConstrs[c].Init(16);
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elConstructors);
  NextElementState;
  XMLASSERT( nElKind = elSignatureWithCounts);
  NextElementState;
  while not (nState = eEnd) do
  begin
   XMLASSERT( nElKind = elConstrCounts);
   lImpConstr := new( ImpConstrPtr,Init( GetAttr( atName)));
   NextElementState;
   In_ConstrCounts1( lImpConstr^.nBase);
   fImpSgn.Insert( lImpConstr);
   NextElementState;
  end;
  NextElementState;
  fSgnMarks.InitNatFunc(0,fImpSgn.Count+1);
  while not (nState = eEnd) do
  begin
   lConstr := In_Constructor1;
   fConstrs[lConstr^.fConstrKind].Insert(lConstr);
  end;
 end;
 dispose(InMMLFile,Done);
 for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  fBases[c] := fConstrs[c].Count;
end; { ImpConstructorsObj.GetACO }

function ImpMultConstrObj.AddDCO:boolean; 
var
  c:ConstructorsKind;
  lConstr:ConstrPtr;
  lImpConstr:ImpConstrPtr;
  InMMLFile: InMMLFilePtr;
begin
 if not MFileExists(MizFileName+'.dco') then
 begin AddDCO := false; exit; end
 else AddDCO := true;
 InMMLFile:=new(InMMLFilePtr,OpenFile(MizFileName+'.dco'));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elConstructors);
  NextElementState;
  GetConstrNames(fConstrIdents);
  In_ConstrCounts( fConstrNbr);
  while not (nState = eEnd) do
  begin
   lConstr := In_Constructor1;
   fConstrs[lConstr^.fConstrKind].Insert(lConstr);
  end;
 end;
 dispose(InMMLFile,Done);
 lImpConstr:= new( ImpConstrPtr,Init(UpperCase(ArticleName)));
 for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  lImpConstr^.nBase[c] := fConstrs[c].Count;

 fImpSgn.Insert(lImpConstr);
end; { ImpConstructorsObj.AddDCO }

{was in transferer}
{$IFDEF MDEBUG} 
procedure ImpMultConstrObj.InfoBase;
var c:ConstructorsKind;
    i:integer; 
begin
  with fImpSgn do
    for i:= 0 to Count-1 do with ImpConstrPtr(Items^[i])^ do
      for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
        InfoInt(nBase[c]);
end; { ImpMultConstrObj.InfoBase }
{$ENDIF} 	

constructor ImpMultConstrObj.Init;
var c:ConstructorsKind; 
begin
 fImpKind:= impMultConstr; 
 fImpSgn.Init(16);
 fSgnMarks.InitNatFunc(0,16);
 fConstrCounts.Init;
  
 fConstrIdents.Init(0);
 fillchar(fConstrNbr,sizeof(fConstrNbr),0);
 for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
 fConstrs[c].Init(16);
end;

{TODO: fSgnMark should be initialised to true as in transferer ... ok}
procedure ImpMultConstrObj.InitMark;
var c:ConstructorsKind;
    j:integer; 
begin
  fSgnMarks.DeleteAll;
  for j:=0 to fImpSgn.Count-1 do fSgnMarks.Up(j);
  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
    fConstrCounts.fNbr[c].DeleteAll;
end; 

{ for transferer, fFROM are usually Bases ... they have to be saved when
reading .aco;}
{ just a number could be passed instead of the fFrom}
{ this replaces ProcesTYP in transferer; unllike Mark1
(MarkSgn in transferer) ir marks redefs too}
{TODO: experiments show, that two marking procedures are redundant,
put the additional things done here right to MarkConstr}
procedure  ImpMultConstrObj.Mark(const fFrom:ConstrIntArr); 
var i:integer;
c: ConstructorsKind;
lConstr: ConstrTypPtr;
begin
  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  with fConstrs[c],fConstrCounts do
  for i:=fFrom[c] to Count-1 do with ConstrPtr(Items^[i])^ do
  begin
    MarkConstr(ConstrPtr(Items^[i]));
    if (fWhichConstrNr > 0) and (fConstrKind in [coFunctor,coMode]) then
    begin 
        lConstr := Items^[fWhichConstrNr-1]; {0-based numbering}
	MarkTypeList(lConstr^.nPrimaries);
	MarkTyp(lConstr^.fConstrTyp);
    end; 	
    if  fConstrKind = coStructMode then { why not this in MarkSgn??}
     with StructConstrPtr(Items^[i])^ do
       MarkTypColl(fPrefixes); 
  end; 
end; { ImpConstructorsObj.Mark }

{MarkSgn from  transferer; no redef types marking ... will be changed}
{CAUTION! originally, ImpSgnAttr is 1-based!}
{fConstrs[c] are 0-based, but nBases are 1-based}
{TODO: some assertion that fNr>0 should be added here!}
procedure  ImpMultConstrObj.Mark1Sgn(fNr:integer); 
var i:integer;
var c:ConstructorsKind; 
begin
  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  with ImpConstrPtr(fImpSgn.Items^[fNr-1])^ do
  for i:=nBase[c] to ImpConstrPtr(fImpSgn.Items^[fNr])^.nBase[c]-1 do
    fConstrCounts.MarkConstr(ConstrPtr(fConstrs[c].Items^[i]));
end; { ImpMultConstrObj.Mark1Sgn }

{ like in inlibr}
procedure ImpMultConstrObj.Mark1DCO;
var i,j:integer;
c:ConstructorsKind ;
label DoMark,DoMark1;
begin
  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
   for i:=1 to ImpConstrPtr(fImpSgn.Items^[0])^.nBase[c] do
    if fConstrCounts.Marked(c,i) then goto DoMark1;

  fSgnMarks.DeleteElem(0);
DoMark1:
      
  for j:=fImpSgn.Count-1 downto 1 do
  begin 
   for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
    with ImpConstrPtr(fImpSgn.Items^[j-1])^ do
     for i:=nBase[c]+1 to ImpConstrPtr(fImpSgn.Items^[j])^.nBase[c] do
      if fConstrCounts.Marked(c,i) then goto DoMark;

   fSgnMarks.DeleteElem(j); continue;
DoMark:
  end; 
end; { ImpMultConstrObj.Mark1DCO }
  
{ first, if a constr from some sgn is marked, marks the whole sgn,
CAUTION!:
  this is done from the last sgn, probably believing that we can
  thus only mark constrs from earlier signatures, so order of sgns in
  ImpMultConstrObj is important!
then we initialise gTrans and do the standard renumerating only for
the marked sgns
TODO: Is InitNatSeq ok for gTrans?? ... gTrans[c].Done should be called if
used multiple times
}
{ AddArticleConstr tells, if we insert the constrs from the article
in gTrans too}
{TODO: change -2 to -1 here when cumulative counts removed}
procedure ImpMultConstrObj.Renumerate(AddArticleConstrs:boolean ); 
var i,j:integer;
c:ConstructorsKind ;
_last: ConstrIntArr;
label DoMark;
begin  
  for j:=fImpSgn.Count-1 downto 1 do
  begin 
   for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
    with ImpConstrPtr(fImpSgn.Items^[j-1])^ do
     for i:=nBase[c]+1 to ImpConstrPtr(fImpSgn.Items^[j])^.nBase[c] do
      if fConstrCounts.Marked(c,i) then goto DoMark;

   fSgnMarks.DeleteElem(j); continue;
DoMark:
   Mark1Sgn(j);
  end; 

  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  begin 
    gTrans[c].Init(MaxRenumNbr);
    gTrans[c].Insert(0);
    gTrans[c].fCount := MaxRenumNbr-1;
    _last[c] := 0;
    { only AtPut can be used from this place on}
  end;
    
  fSgnMarks.Up(0); { HIDDEN? }

  { added, we have no zeroed element to use the other loop}
  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
    for j:=1 to ImpConstrPtr(fImpSgn.Items^[0])^.nBase[c] do
    begin
      inc(_last[c]);
      gTrans[c].AtPut(j,_last[c]);
    end; 

  for i:=1 to fImpSgn.Count-1 do
   if fSgnMarks.HasInDom(i) then
    for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
     with ImpConstrPtr(fImpSgn.Items^[i-1])^ do
      for j:=nBase[c]+1 to ImpConstrPtr(fImpSgn.Items^[i])^.nBase[c] do
      begin
        inc(_last[c]);
        gTrans[c].AtPut(j,_last[c]);
      end;
(* TODO: remove this completely seems this is done above
  if AddArticleConstrs then
   for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
    for j:=fBases[c] to  fConstrs[c].fCount-1 do
    begin
      inc(_last[c]);
      gTrans[c].AtPut(j,_last[c]);
    end;
*)
end; { ImpMultConstrObj.Mark1Sgn }

procedure ImpMultConstrObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
 var lOutMMlFile: OutMMLFileObj;
 procedure DoOne(c:ConstructorsKind);
  var i:integer;
  begin
    for i:=fBases[c] to fConstrs[c].Count-1 do
      lOutMMlFile.Out_Constructor(fConstrs[c].Items^[i], i + 1);
  end; { DoOne }
begin
 lOutMMlFile.OpenFile(aName);
 lOutMMLFile.Out_XElStart0( elConstructors);
 lOutMMlFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-2);
 lOutMMlFile.OutConstrCounts(fConstrNbr);
// ###TODO: export in MML order
  DoOne(coAttribute);
  DoOne(coFunctor);
  DoOne(coMode);
  DoOne(coPredicate);
  DoOne(coStructMode);
  DoOne(coAggregate);
  DoOne(coSelector);
  lOutMMLFile.Out_XElEnd( elConstructors);
  lOutMMlFile.Done;
end; { ImpMultConstrObj.PutMarked }

{ to be finished}
destructor ImpMultConstrObj.Done;
var c:ConstructorsKind;
begin
  fImpSgn.Done;
  for c:= Low(ConstructorsKind) to High(ConstructorsKind) do
  fConstrs[c].Done;
  fSgnMarks.Done;
  fConstrCounts.Done;
  inherited Done;
end;

destructor ImpConstructorsObj.Done;
begin
 fConstructors.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}
{ clusters from inoutmml}

constructor ImpClustersObj.Init;
begin
 fImpKind:= impClusters;
 fConstrIdents.Init(0);
 fClusters.Init(8);
end;

constructor ImpClustersObj.GetClusters(const aPath: string);
var InMMLFile: InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impClusters;
 fClusters.Init(20);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elRegistrations);
  NextElementState;
  GetConstrNames( fConstrIdents);
  while not (nState = eEnd) do
   fClusters.Insert( In_Cluster);
 end;
 dispose(InMMLFile,Done);
end;

procedure ImpClustersObj.Mark(var fCounts:ConstrCounterObj);
var i:integer;
begin
  with fClusters do
  for i:=0 to Count-1 do
    fCounts.MarkClusterObj(Items^[i]);
end; { ImpClustersObj.Mark }

procedure ImpClustersObj.PutClusters(const aPath: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aPath);
 with lOutMMlFile do
 begin
  Out_XElStart0( elRegistrations);
  PutConstrNames(fConstrIdents);
  with fClusters do
   for i:=0 to Count-1 do Out_Cluster(Items^[i]);
  Out_XElEnd( elRegistrations);
 end;
 lOutMMlFile.Done;
end;

procedure ImpClustersObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aName);
 lOutMMlFile.Out_XElStart0( elRegistrations);
 with fMConstr^ do
  lOutMMlFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
 with fClusters do
  for i:=0 to Count-1 do
   lOutMMlFile.Out_Cluster(Items^[i]);
 lOutMMlFile.Out_XElEnd( elRegistrations);
 lOutMMlFile.Done;
end;

destructor ImpClustersObj.Done;
begin
 fClusters.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}
{ Identify }

constructor ImpIdentifyObj.Init;
begin
 fImpKind:= impIdentify;
 fConstrIdents.Init(0);
 fIdentify.Init(8);
end;

constructor ImpIdentifyObj.GetIdentify(const aPath: string);
var InMMLFile: InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impIdentify;
 fIdentify.Init(20);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elIdentifyRegistrations);
  NextElementState;
  GetConstrNames( fConstrIdents);
  while not (nState = eEnd) do
   fIdentify.Insert( In_Identify);
 end;
 dispose(InMMLFile,Done);
end;

procedure ImpIdentifyObj.Mark(var fCounts:ConstrCounterObj);
var i:integer;
begin
  with fIdentify do
  for i:=0 to Count-1 do
    fCounts.MarkIdentifyObj(Items^[i]);
end; { ImpIdentifyObj.Mark }

procedure ImpIdentifyObj.PutIdentify(const aPath: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aPath);
 with lOutMMlFile do
 begin
  Out_XElStart0( elIdentifyRegistrations);
  PutConstrNames(fConstrIdents);
  with fIdentify do
   for i:=0 to Count-1 do Out_Identify(Items^[i]);
  Out_XElEnd( elIdentifyRegistrations);
 end;
 lOutMMlFile.Done;
end;

procedure ImpIdentifyObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aName);
 lOutMMlFile.Out_XElStart0( elIdentifyRegistrations);
 with fMConstr^ do
  lOutMMlFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
 with fIdentify do
  for i:=0 to Count-1 do
   lOutMMlFile.Out_Identify(Items^[i]);
 lOutMMlFile.Out_XElEnd( elIdentifyRegistrations);
 lOutMMlFile.Done;
end;

destructor ImpIdentifyObj.Done;
begin
 fIdentify.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}
{ Reduction }

constructor ImpReductionObj.Init;
begin
 fImpKind:= impReduction;
 fConstrIdents.Init(0);
 fReductions.Init(8);
end;

constructor ImpReductionObj.GetReductions(const aPath: string);
var InMMLFile: InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impReduction;
 fReductions.Init(20);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elReductionRegistrations);
  NextElementState;
  GetConstrNames( fConstrIdents);
  while not (nState = eEnd) do
   fReductions.Insert( In_Reduction);
 end;
 dispose(InMMLFile,Done);
end;

procedure ImpReductionObj.Mark(var fCounts:ConstrCounterObj);
var i:integer;
begin
  with fReductions do
  for i:=0 to Count-1 do
   fCounts.MarkReductionObj(Items^[i]);
end;

procedure ImpReductionObj.PutReductions(const aPath: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aPath);
 with lOutMMlFile do
 begin
  Out_XElStart0( elReductionRegistrations);
  PutConstrNames(fConstrIdents);
  with fReductions do
   for i:=0 to Count-1 do Out_Reduction(Items^[i]);
  Out_XElEnd( elReductionRegistrations);
 end;
 lOutMMlFile.Done;
end;

procedure ImpReductionObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aName);
 lOutMMlFile.Out_XElStart0( elReductionRegistrations);
 with fMConstr^ do
  lOutMMlFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
 with fReductions do
  for i:=0 to Count-1 do
   lOutMMlFile.Out_Reduction(Items^[i]);
 lOutMMlFile.Out_XElEnd( elReductionRegistrations);
 lOutMMlFile.Done;
end;

destructor ImpReductionObj.Done;
begin
 fReductions.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}
{ Properties }

constructor ImpPropertiesObj.Init;
begin
 fImpKind:= impProperties;
 fConstrIdents.Init(0);
 fProperties.Init(8);
end;

constructor ImpPropertiesObj.GetProperties(const aPath: string);
var InMMLFile: InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impProperties;
 fProperties.Init(20);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elPropertyRegistration);
  NextElementState;
  GetConstrNames( fConstrIdents);
  while not (nState = eEnd) do
   fProperties.Insert( In_PropertyReg);
 end;
 dispose(InMMLFile,Done);
end;

procedure ImpPropertiesObj.Mark(var fCounts:ConstrCounterObj);
var i:integer;
begin
  with fProperties do
  for i:=0 to Count-1 do
    fCounts.MarkPropertyObj(Items^[i]);
end; { ImpPropertiesObj.Mark }

procedure ImpPropertiesObj.PutProperties(const aPath: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aPath);
 with lOutMMlFile do
 begin
  Out_XElStart0( elPropertyRegistration);
  PutConstrNames(fConstrIdents);
  with fProperties do
   for i:=0 to Count-1 do Out_PropertyReg(Items^[i]);
  Out_XElEnd( elPropertyRegistration);
 end;
 lOutMMlFile.Done;
end;

procedure ImpPropertiesObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aName);
 lOutMMlFile.Out_XElStart0( elPropertyRegistration);
 with fMConstr^ do
  lOutMMlFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
 with fProperties do
  for i:=0 to Count-1 do
   lOutMMlFile.Out_PropertyReg(Items^[i]);
 lOutMMlFile.Out_XElEnd( elPropertyRegistration);
 lOutMMlFile.Done;
end;

destructor ImpPropertiesObj.Done;
begin
 fProperties.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}

constructor ImpTheoremsObj.Init;
begin
 fImpKind:= impTheorems;
 fConstrIdents.Init(0);
 fTheorems.Init(16);
end;

{ some more info could be on theorems ... e.g. article and number }

constructor ImpTheoremsObj.GetTheorems(const aPath: string);
var
 lKind: char;
 lThNbr,lDefNbr,lNr: integer;
 InMMLFile: InMMLFilePtr;
 lStr: string;
 lConstr: Lexem;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impTheorems;
 fTheorems.Init(16);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elTheorems);
  NextElementState;
  GetConstrNames(fConstrIdents);
  lThNbr:=0; lDefNbr:=0;
  while not (nState = eEnd) do
  begin
   XMLASSERT( nElKind = elTheorem);
   lKind:= GetAttr( atKind)[1];
   lConstr.Kind:= ikError;
   if 'D' = lKind then
   begin
    inc(lDefNbr); lNr:=lDefNbr;
    if GetOptAttr( atConstrKind, lStr) then
     GetLexemAttrs( atConstrKind, atConstrNr, lConstr);
   end
   else begin inc(lThNbr); lNr:=lThNbr end;
   NextElementState;
   fTheorems.Insert(new(TheoremPtr,Init(lKind,lNr,lConstr,In_Formula)));
   NextElementState;
  end;
 end;
 dispose(InMMLFile,Done);
end;

procedure  ImpTheoremsObj.Mark(var fCounts:ConstrCounterObj);
var i:integer;
begin
  with fTheorems,fCounts do
  for i:=0 to Count-1 do
   with TheoremPtr(Items^[i])^ do
   begin
    MarkFrm(fTheorem);
    if ('D' = fTheoKind) and  (fDefConstr.Kind <> ikError) then
     fNbr[coAttribute].Up(fDefConstr.Nr);
   end;
end; { ImpTheoremsObj.Mark }

procedure ImpTheoremsObj.PutTheorems(const aPath: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aPath);
 with lOutMMlFile do
 begin
  Out_XElStart0( elTheorems);
  PutConstrNames(fConstrIdents);
  with fTheorems do
   for i:=0 to Count-1 do
    with TheoremPtr(Items^[i])^ do
     begin
      Out_XElStart( elTheorem);
      Out_XAttr( atKind, fTheoKind);
      if ('D' = fTheoKind) and  (fDefConstr.Kind <> ikError) then
       with fDefConstr do
      begin
       Out_XAttr( atConstrKind, Kind);
       Out_XIntAttr( atConstrNr, Transf( ConstructorKind(Kind), Nr));
      end;
      Out_XAttrEnd;
      Out_Formula( fTheorem);
      Out_XElEnd( elTheorem);
     end;
  Out_XElEnd( elTheorems);
 end;
 lOutMMlFile.Done;
end;

procedure ImpTheoremsObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
var lOutMMlFile: OutMMLFileObj; i: integer;
begin
 lOutMMlFile.OpenFile(aName);
 with lOutMMlFile do
 begin
  Out_XElStart0( elTheorems);
  with fMConstr^ do
   OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
  with fTheorems do
   for i:=0 to Count-1 do
    with TheoremPtr(Items^[i])^ do
     begin
      Out_XElStart( elTheorem);
      Out_XAttr( atKind, fTheoKind);
      if ('D' = fTheoKind) and  (fDefConstr.Kind <> ikError) then
       with fDefConstr do
      begin
       Out_XAttr( atConstrKind, Kind);
       Out_XIntAttr( atConstrNr, Transf( ConstructorKind(Kind), Nr));
      end;
      Out_XAttrEnd;
      Out_Formula( fTheorem);
      Out_XElEnd( elTheorem);
     end;
  Out_XElEnd( elTheorems);
 end;
 lOutMMlFile.Done;
end;

destructor ImpTheoremsObj.Done;
begin
 fConstrIdents.Done;
 fTheorems.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}

constructor ReqObj.Init(aKind: char; aNr,aReqNr: integer);
begin
  fConstr.Kind :=  aKind;
  fConstr.Nr :=  aNr;
  fReqNr  :=  aReqNr;
end;

destructor ReqObj.Done;
begin inherited Done;
end;

constructor ImpRequirementsObj.Init;
begin
  fImpKind:= impRequirements;
  fConstrIdents.Init(0);
  fRequirements.Init(8);
end; { ImpRequirementsObj.Init }

(* ##RNC:
## Requirement is a constructor specially treated by the system.
## We give its internal number and optionally its name and
## the article id (atAid) and order in article (atAbsNr).
elRequirement =
 element elRequirement {
   attribute atConstrKind { 'M' | 'L' | 'V' | 'R' | 'K' | 'U' | 'G' },
   attribute atConstrNr { xsd:integer },
   attribute atNr { xsd:integer },
   attribute atReqName { xsd:string }?, 
   ( attribute atAbsNr { xsd:integer },
     attribute atAid { xsd:string } )?
} 
   
## Requirements (now only the exported form).
elRequirements = 
 element elRequirements {
   elSignature,
   elRequirement*
 }
*)
constructor ImpRequirementsObj.GetRequirements(const aPath: string);
 var
  lReqNr    : integer; lLex: Lexem;
  InMMLFile : InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impRequirements;
 fRequirements.Init(8);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elRequirements);
  NextElementState;
  GetConstrNames(fConstrIdents);
  while not (nState = eEnd) do
  begin
   XMLASSERT(nElKind = elRequirement);
   GetLexemAttrs( atConstrKind, atConstrNr, lLex);
   lReqNr:=GetIntAttr( atNr);
   fRequirements.Insert(new(ReqPtr,Init(lLex.Kind,lLex.Nr,lReqNr)));
   AcceptEndState;
   NextElementState;
  end;
 end;
 dispose(InMMLFile,Done);
end; { ImpRequirementsObj.GetRequirements }

{  procedure PutRequirements(const aName: string);}
destructor ImpRequirementsObj.Done;
begin
   fConstrIdents.Done;
   fRequirements.Done;
   inherited Done;
end; { ImpRequirementsObj.Done }

{+-------------------------------------------------------------------+}

constructor ImpDefinientiaObj.Init;
begin
 fImpKind:= impDefinientia;
 fConstrIdents.Init(0);
 fDefinientia.Init(8);
end;

constructor ImpDefinientiaObj.GetDefinientia(const aPath: string);
var lInMMLFile: InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impDefinientia;
 fDefinientia.Init(20);
 lInMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with lInMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elDefinientia);
  NextElementState;
  GetConstrNames(fConstrIdents);
  while not (nState = eEnd) do
   fDefinientia.Insert( In_Definiens);
 end;
 dispose(lInMMLFile,Done);
end; { ImpDefinientiaObj.GetDefinientia }
  
procedure  ImpDefinientiaObj.Mark(var fCounts:ConstrCounterObj);
var i:integer;
begin
  with fDefinientia do
  for i:=0 to Count-1 do
    fCounts.MarkDefiniensObj(Items^[i]);
end; { ImpDefinientiaObj.Mark }

procedure ImpDefinientiaObj.PutDefinientia(const aPath: string);
 var lOutMMlFile: OutMMLFileObj;
     i: integer;
begin
 lOutMMlFile.OpenFile(aPath);
 lOutMMlFile.Out_XElStart0( elDefinientia);
 lOutMMlFile.PutConstrNames(fConstrIdents);
 with fDefinientia do
  for i:=0 to Count-1 do
   lOutMMlFile.Out_Definiens( DefiniensPtr( Items^[i])^, 0);
 lOutMMlFile.Out_XElEnd( elDefinientia);
 lOutMMlFile.Done;
end;

procedure ImpDefinientiaObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
 var lOutLibrFile: OutMMLFileObj;
     i: integer;
begin
 lOutLibrFile.OpenFile(aName);
 lOutLibrFile.Out_XElStart0( elDefinientia);
 with fMConstr^ do
    lOutLibrFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
 with fDefinientia do
  for i:=0 to Count-1 do
   lOutLibrFile.Out_Definiens( DefiniensPtr( Items^[i])^, 0);
 lOutLibrFile.Out_XElEnd( elDefinientia);
 lOutLibrFile.Done;
end;

destructor ImpDefinientiaObj.Done;
begin
 fDefinientia.Done;
 inherited Done;
end;

{+-------------------------------------------------------------------+}

constructor ImpSchemeObj.Init;
begin
 fImpKind:= impSchemes;;
 fConstrIdents.Init(0);
 fSchemes.Init(0);
end;

{ why two .sch reading funcs here??? }

{ this  slightly differs from the original, error 2200 cannot occur
here }

constructor ImpSchemeObj.GetSchemes(const aPath: string);
 var
  lSchNr    : integer;
  InMMLFile : InMMLFilePtr;
begin
 if aPath =  '' then begin Init; exit end;
 fImpKind:= impSchemes;;
 fSchemes.Init(8);
 InMMLFile:=new(InMMLFilePtr,OpenFile(aPath));
 with InMMLFile^ do
 begin
  NextElementState;
  XMLASSERT( nElKind = elSchemes);
  NextElementState;
  GetConstrNames(fConstrIdents);
  lSchNr:=1; {numeracja zaczyna sie od 1}
  while not (nState = eEnd) do
  begin
   if nElKind = elCanceled then
    begin
     fSchemes.Insert(new(SchemePtr, Init(lSchNr)));
     NextElementState;
    end
   else
    begin
     XMLASSERT( nElKind = elScheme);
     fSchemes.Insert(new(SchemePtr, Init(lSchNr)));
     NextElementState;
     with SchemePtr(fSchemes.Items^[lSchNr-1])^ do
     begin
      In_ArgColl( fSchTypes);
      In_FormulaColl( fSchProps);
     end;
    end;
   NextElementState;
   inc(lSchNr);
  end;
 end;
 dispose(InMMLFile,Done);
end;

procedure  ImpSchemeObj.Mark(var fCounts:ConstrCounterObj); 
var i:integer;
begin
  with fSchemes,fCounts do
  for i:=0 to Count-1 do with SchemePtr(Items^[i])^ do
  begin
    MarkTypColl(fSchTypes);
    MarkFrmColl(fSchProps);
  end;   
end; { ImpSchemesObj.Mark }
  
procedure ImpSchemeObj.PutSchemes(const aPath: string);
 var lOutMMlFile: OutMMLFileObj;
     i: integer;
begin
 lOutMMlFile.OpenFile(aPath);
 lOutMMLFile.Out_XElStart0( elSchemes);
 lOutMMlFile.PutConstrNames(fConstrIdents);
 with fSchemes do
 begin
  for i:=0 to Count-1 do
   lOutMMlFile.PutImpScheme(Items^[i]);
 end;
 lOutMMLFile.Out_XElEnd( elSchemes);
 lOutMMlFile.Done;
end;

procedure ImpSchemeObj.PutMarked(fMConstr:ImpMultConstrPtr; const aName: string);
 var lOutMMlFile: OutMMLFileObj;
     i: integer;
begin
 lOutMMlFile.OpenFile(aName);
 lOutMMLFile.Out_XElStart0( elSchemes);
 with fMConstr^ do
  lOutMMlFile.OutMarkedSgn(fImpSgn,fSgnMarks,fImpSgn.Count-1);
 with fSchemes do
 begin
  for i:=0 to Count-1 do
   lOutMMlFile.PutImpScheme(Items^[i]);
 end;
 lOutMMLFile.Out_XElEnd( elSchemes);
 lOutMMlFile.Done;
end;

destructor ImpSchemeObj.Done;
begin
 fSchemes.Done;
 inherited Done;
end;

end.




