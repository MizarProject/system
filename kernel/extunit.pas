(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit extunit;

interface

procedure MSMExportArticle;

implementation

uses pcmizver,mizenv,monitor,errhan,mscanner,parser,
     mconsole,inlibr,mobjects,
     _formats, parseraddition, wsmarticle ,first_identification, trans2analyzer,
     xml_inout,dicthan,
     identify, prepobj, prephan, e_prep,
     inout,inoutmml, builtin, schemhan, lexicon,
     correl,generato,analyzer,enums,
     iocorrel,xmldict,xmlpars,outlibr
     {$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

procedure AddArticleConstr;
var i: integer; c:ConstructorsKind;
begin
 for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
  for i:=1 to Constr[c].Count-ConstrBase[c] do
   gTrans[c].Insert(gConstrNbr[c]+i);
end;

procedure SaveFormats;
var
 z: integer;
 c: char;
 lOutMMLFile: XMLOutStreamObj;
 lInFile: InMMLFilePtr;
 lVocs: MStringList;
begin
 if gFormatsBase < gFormatsColl.Count then
  begin
    FileExam(EnvFileName+'.vcl');
    lInFile:=new(InMMLFilePtr,OpenFile(EnvFileName+'.vcl'));
    lInFile^.NextElementState;
    lInFile^.In_Vocs( lVocs);
    dispose(lInFile,Done);
    lOutMMLFile.OpenFile(MizFileName+'.dfr');
    with lOutMMLFile do
    begin
     Out_XElStart0( XMLElemName[elFormats]);
     Out_XElStart0( XMLElemName[elVocabularies]);
     with lVocs do
      for z:=0 to fCount-1 do
       begin
        Out_XElStart0( XMLElemName[elVocabulary]);
        Out_XElStart( XMLElemName[elArticleID]);
        Out_XAttr( XMLAttrName[atName], GetString(z));
        Out_XElEnd0;
        for c:='A' to 'Z' do if c in AvailableSymbols then
         begin
          Out_XElStart( XMLElemName[elSymbolCount]);
          Out_XAttr( XMLAttrName[atKind], c);
          Out_XIntAttr( XMLAttrName[atNr], AbsVocabularyPtr(GetObject(z))^.fSymbolCnt[c]);
          Out_XElEnd0;
         end;
        Out_XElEnd( XMLElemName[elVocabulary]);
       end;
     Out_XElEnd( XMLElemName[elVocabularies]);
     with gFormatsColl do
     for z:=gFormatsBase to Count-1 do
       MFormatPtr(Items^[z])^.Out_Format( lOutMMLFile,0{z + 1});
     Out_XElEnd( XMLElemName[elFormats]);
    end;
    lOutMMLFile.Done;
  end;
end;

procedure SaveSignature;
var
 i: integer; lDoWrite: boolean;
 lOutMMLFile: OutMMLFileObj;
 lCounts: ConstrIntArr;
 lInFile: InMMLFilePtr;
 lVocs: MStringList;
 nk:NotationKind;
 c:ConstructorsKind;
 lPattern: PatternPtr;
begin
 lDoWrite:= false;
 for nk:=Low(NotationKind) to High(NotationKind) do
  if Notat[nk].Count > NotatBase[nk] then lDoWrite:= true;
 if lDoWrite then
 begin
  FileExam(EnvFileName+'.vcl');
  lInFile:=new(InMMLFilePtr,OpenFile(EnvFileName+'.vcl'));
  lInFile^.NextElementState;
  lInFile^.In_Vocs( lVocs);
  dispose(lInFile,Done);
  lOutMMLFile.OpenFile(MizFileName+'.dno');
  with lOutMMLFile do
  begin
   Out_XElStart0( elNotations);
   OutSgn( gImpSgnNames, true);
   Out_Vocs( lVocs);
   for nk:=Low(NotationKind) to High(NotationKind) do
    for i:=NotatBase[nk] to Notat[nk].Count-1  do
    begin
     lPattern:= Notat[nk].Items^[i];
     Out_PatternWithFormat(lPattern, gFormatsColl.Items^[lPattern^.fFormNr-1]);
    end;
   Out_XElEnd( elNotations);
  end;
  lVocs.Done;
  lOutMMLFile.Done;
 end;
// Constructors
// ##TODO: we can have empty dco
 lDoWrite:= false;
 for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
  if Constr[c].Count > ConstrBase[c] then lDoWrite:= true;
 if lDoWrite then
 begin
  lOutMMLFile.OpenFile(MizFileName+'.dco');
  lOutMMLFile.Out_XElStart0( elConstructors);
  lOutMMLFile.OutSgn(gImpSgnNames, false);
  for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
   lCounts[c]:= Constr[c].Count - ConstrBase[c];
  lOutMMLFile.OutConstrCounts( lCounts);
  for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
   for i:=ConstrBase[c] to Constr[c].Count-1 do
// i as relative nr. is correct here, since
// Constr[c].Items[i] contains constructor nr. i (not i+1)
    lOutMMLFile.Out_Constructor( Constr[c].Items^[i], i);
  lOutMMLFile.Out_XElEnd( elConstructors);
  lOutMMLFile.Done;
 end;
// Clusters
 if (RegClusterBase < RegisteredCluster.Count) or
    (FuncClusterBase < FunctorCluster.Count) or
    (CondClusterBase < ConditionalCluster.Count) then
 begin
  lOutMMLFile.OpenFile(MizFileName+'.dcl');
  lOutMMLFile.Out_XElStart0( elRegistrations);
  lOutMMLFile.OutSgn( gImpSgnNames, true);
// ##NOTE: until unified, lOutMMLFile must be OutMMLFileObj not OutEnvFileObj
// ##TODO: write this as a loop going from Low(ClusterKind)
  for i:=RegClusterBase to RegisteredCluster.Count-1 do
   lOutMMLFile.Out_Cluster(RegisteredCluster.Items^[i]);
  for i:=FuncClusterBase to FunctorCluster.Count-1 do
   lOutMMLFile.Out_Cluster(FunctorCluster.Items^[i]);
  for i:=CondClusterBase to ConditionalCluster.Count-1 do
   lOutMMLFile.Out_Cluster(ConditionalCluster.Items^[i]);
  lOutMMLFile.Out_XElEnd( elRegistrations);
  lOutMMLFile.Done;
 end;
end;

procedure SaveIdentify;
var i: integer; lOutMMLFile: OutMMLFileObj;
begin
 if gIdentifications.Count > 0 then
 begin
  lOutMMLFile.OpenFile(MizFileName+'.did');
  lOutMMLFile.Out_XElStart0( elIdentifyRegistrations);
  lOutMMLFile.OutSgn( gImpSgnNames, true);
  for i:=0 to gIdentifications.Count-1 do
   lOutMMLFile.Out_Identify(gIdentifications.Items^[i]);
  lOutMMLFile.Out_XElEnd( elIdentifyRegistrations);
  lOutMMLFile.Done;
 end;
end;

procedure SaveReductions;
var i: integer; lOutMMLFile: OutMMLFileObj;
begin
 if gReductions.Count > 0 then
 begin
  lOutMMLFile.OpenFile(MizFileName+'.drd');
  lOutMMLFile.Out_XElStart0( elReductionRegistrations);
  lOutMMLFile.OutSgn( gImpSgnNames, true);
  for i:=0 to gReductions.Count-1 do
   lOutMMLFile.Out_Reduction(gReductions.Items^[i]);
  lOutMMLFile.Out_XElEnd( elReductionRegistrations);
  lOutMMLFile.Done;
 end;
end;

procedure SaveProperties;
var i: integer; lOutMMLFile: OutMMLFileObj;
begin
 if RegPropertiesBase < gPropertiesList.Count then
 begin
  lOutMMLFile.OpenFile(MizFileName+'.dpr');
  lOutMMLFile.Out_XElStart0( elPropertyRegistration);
  lOutMMLFile.OutSgn( gImpSgnNames, true);
  for i:=RegPropertiesBase to gPropertiesList.Count-1 do
   lOutMMLFile.Out_PropertyReg(gPropertiesList.Items^[i]);
  lOutMMLFile.Out_XElEnd( elPropertyRegistration);
  lOutMMLFile.Done;
 end;
end;

procedure SaveDefinitions;
var k: integer; lOutMMLFile: OutMMLFileObj;
begin
 if Definientia.Count > 0 then
 begin
  lOutMMLFile.OpenFile(MizFileName+'.def');
  lOutMMLFile.Out_XElStart0( elDefinientia);
  lOutMMLFile.OutSgn( gImpSgnNames, true);
  with Definientia do
   for k:=0 to Count-1 do lOutMMLFile.Out_Definiens(DefiniensPtr(Items^[k])^ ,0);
  lOutMMLFile.Out_XElEnd( elDefinientia);
  lOutMMLFile.Done;
 end;
end;

procedure SaveTheoremsAndSchemes;
 var i: integer; lOutMMLFile: OutMMLFileObj;
begin
  if gTheoremNbr > 0 then
  begin
   lOutMMLFile.OpenFile(MizFileName+'.the');
   lOutMMLFile.Out_XElStart0( elTheorems);
   lOutMMLFile.OutSgn( gImpSgnNames, true);
   for i:=1 to gTheoremNbr do
    with gTheorem[i], lOutMMLFile do
     begin
      Out_XElStart( elTheorem);
      if Definitional then
       begin
        Out_XAttr( atKind, 'D');
        if nDefConstr.Kind <> ikError then
         with nDefConstr do
         begin
          Out_XAttr( atConstrKind, Kind);
          Out_XIntAttr( atConstrNr, Transf( ConstructorKind(Kind), Nr));
         end;
       end
      else Out_XAttr( atKind, 'T');
      Out_XAttrEnd;
      Out_Formula(Theo);
      Out_XElEnd( elTheorem);
     end;
   lOutMMLFile.Out_XElEnd( elTheorems);
   lOutMMLFile.Done;
  end;
  if gSchemeNbr > 0 then
  begin
   lOutMMLFile.OpenFile(MizFileName+'.sch');
   lOutMMLFile.Out_XElStart0( elSchemes);
   lOutMMLFile.OutSgn( gImpSgnNames, true);
   for i:=1 to gSchemeNbr do
    with lOutMMLFile, gScheme[i] do
     if nCanceled then
       Out_XEl1( elCanceled)
     else
      with nSchemeDef^ do
      begin
       Out_XElStart0( elScheme);
       Out_ArgTypes( SchTypes);
       Out_FormulaColl( SchProps);
       Out_XElEnd( elScheme);
      end;
    lOutMMLFile.Out_XElEnd( elSchemes);
    lOutMMLFile.Done;
  end;
end;

procedure MSMExportArticle;
begin
  DrawMizarScreen('Exporter based on MSM Processor');
  GetArticleName; GetEnvironName;
  GetOptions;
  InitExitProc;
  FileExam(MizFileName+ArticleExt);
  DrawArticleName(MizFileName+ArticleExt);

  {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
  OpenErrors(MizFileName);

  InitDisplayLine('- Parsing  ');
  FileExam(EnvFileName+'.dct');
  FileExam(EnvFileName+'.prf');
  InitScanning(MizFileName+ArticleExt,EnvFileName);
  InitWSMizarArticle;
  Parse;
  gFormatsColl.StoreFormats(EnvFileName+'.frx');
  gFormatsColl.Done;
  FinishScanning;
  Write_WSMizArticle(gWSTextProper,EnvFileName+'.wsx');

  InitDisplayLine('- MSM      ');
  MSMAnalyzer;
  Transfer2Exporter;

  InitDisplayLine('- Signature');
  AxiomsAllowed:=true;
  Verifying:=false;
  Analyze;
  if ErrorNbr = 0 then
  begin
   SaveFormats;
   InConstrNum;
   ReadSignatureList;
   AddArticleConstr;
   SaveSignature;
   SaveDefinitions;
   SaveIdentify;
   SaveReductions;
   SaveProperties;
   gFormatsColl.Done;
   DisposeAnalyze;
   InitDisplayLine('- Theorems ');
   gPrBlockPtr:=new(ExtPBlockPtr,Init(blMain));
   ExtPBlockPtr(gPrBlockPtr)^.InitPrepData;
   Prepare;
   SaveTheoremsAndSchemes;
  end;
  DrawErrorsMsg(ErrorNbr);
  FinishDrawing;
end;

end.
