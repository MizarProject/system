(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit acc_han;
interface

uses mobjects,envhan,inoutmml,enums;

procedure Accomodate;

var Localization: boolean = false;

type
  AccImpNotationPtr = ^AccImpNotationObj;
  AccImpNotationObj = Object(MStrObj)
      nBase: NotationIntArr;
    constructor Init(fIdent:string; var fNotat: NotationIntArr);
  end;

var
  gNotat: NotationIntArr;

implementation

uses pcmizver,mizenv,librenv,errhan,mconsole,limits,generato,
     inout,builtin,mscanner,scanner,_formats,dicthan,accdict,
     correl,inlibr,identify,impobjs,iocorrel,schemhan,
     xmldict,xmlpars
{$IFDEF MDEBUG} ,info,outinfo {$ENDIF};

constructor AccImpNotationObj.Init(fIdent:string; var fNotat: NotationIntArr);
begin
 inherited Init(fIdent);
 nBase:=fNotat;
end;

var
  gNotation: MCollection;
  gAccNotat: array[NotationKind] of NatSet;

procedure InitFormats;
 var i,r: integer;
begin
 gFormatsColl.Init(100);
 gFormatsColl.Insert(new(MPrefixFormatPtr,Init('V',StrictSym,1)));   { format dla atrybutu 'strict' }
 ModeMaxArgs.Init(gSymBase['M']+1);
 for i:=1 to gSymBase['M']+1 do r:=ModeMaxArgs.Insert(0);
 StructModeMaxArgs.Init(gSymBase['G']+1);
 for i:=1 to gSymBase['G']+1 do r:=StructModeMaxArgs.Insert(0);
 PredMaxArgs.Init(gSymBase['R']+1);
 for i:=1 to gSymBase['R']+1 do r:=PredMaxArgs.Insert(0);
end;
   
var SymbolNbr: SymbolCounters;
    SymbolNr: array['G'..'V'] of NatFunc;

{ the format of .aco is changed a bit ... it was used only for transferer
so should be ok; basically, .sgl is added to it, so it has articlenames
and constrcounts too}
(* ##RNC:
## Constructors, either imported from other articles ( after accommodation)
## - the signature is implicit in that case, or exported from the current 
## article - then the signature has to be specified.
## atAid optionally specifies its article's name in uppercase.      
elConstructors = 
 element elConstructors {
   attribute atAid { xsd:string }?,
   ( element elSignatureWithCounts { elConstrCounts* }
   | ( elSignature, elConstrCounts ) )?,
   elConstructor*
   }
*)
procedure SaveAConstructors;
var c: ConstructorsKind; lOutEnvFile: OutEnvFileObj; i: integer; 
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.aco');
 with lOutEnvFile do
 begin
  Out_XElStart( elConstructors);
  Out_XAttr( atAid, ArticleID);
  Out_XMizQuotedAttr( atMizfiles, MizFiles);
  Out_XAttrEnd;
  Out_XElStart0( elSignatureWithCounts);
  for i:=1 to ImpConstrNbr do
  begin
   Out_XElStart( elConstrCounts);
   Out_XAttr( atName, ImpConstr[i]^.fStr);
   Out_XAttrEnd;
   OutConstrCounts1( ImpConstr[i+1]^.nBase);
   Out_XElEnd( elConstrCounts);
  end;
  Out_XElEnd( elSignatureWithCounts);
  for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
   for i:=1 to gConstrNbr[c] do
    lOutEnvFile.Out_Constructor( Constr[c].Items^[i], i);
  Out_XElEnd( elConstructors);
 end;
 lOutEnvFile.Done;
end; { SaveAConstructors } 

// ##TODO: unify with SaveAConstructors
// ###TODO: return to the loop if possible
procedure SaveConstructors;
var
  lOutEnvFile: OutEnvFileObj;
 procedure OutOne( c:ConstructorsKind);
 var 
 i: integer;
 begin
   for i:=1 to gConstrNbr[c] do
// ##TODO: this check is now always true   
   if gChosen[c].HasInDom(i) then
    lOutEnvFile.Out_Constructor( Constr[c].Items^[i], i);
 end;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.atr');
 lOutEnvFile.Out_XElStart( elConstructors);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
// for c:=Low(ConstructorsKind) to High(ConstructorsKind) do
 OutOne( coAttribute);  OutOne( coFunctor);  OutOne( coMode);
 OutOne( coPredicate);  OutOne( coStructMode);  OutOne( coAggregate);
 OutOne( coSelector);
 lOutEnvFile.Out_XElEnd( elConstructors);
 lOutEnvFile.Done;
end;

procedure ProcessConstructors;
var lImp: integer; lPath: string;
begin
 writeln(gLogFile,'Constructors');
 InitConstructors;
 InitConstrNbr;
 InitSignatures;
 for lImp:=0 to Env.Directive[syConstructors].Count-1 do
  with PImpArticleId(Env.Directive[syConstructors].Items^[lImp])^ do
   begin CurPos:=fPos;
    lPath:=LibraryPath(LowerCase(fStr),'.dco');
    if lPath<>'' then GetSignature(fStr)
     else Error(fPos,808);
   end;
 ImpConstr[ImpConstrNbr+1]:=new(ImpConstrPtr,Init(''));
 ImpConstr[ImpConstrNbr+1]^.nBase:=gConstrNbr;
end;

function AccessibleConstr(const fWord:Lexem): boolean;
begin AccessibleConstr:=false;
 with fWord do
 begin
   case Kind of
    'K': if not gChosen[coFunctor].HasInDom(Nr) then exit;
    'G': if not gChosen[coAggregate].HasInDom(Nr) then exit;
    'U': if not gChosen[coSelector].HasInDom(Nr) then exit;
    'M': if (Nr <> 0) and not gChosen[coMode].HasInDom(Nr) then exit;
    'L':  if not gChosen[coStructMode].HasInDom(Nr) then exit;
    'R':  if not gChosen[coPredicate].HasInDom(Nr) then exit;
    'V':  if not gChosen[coAttribute].HasInDom(Nr) then exit;
   end;
  AccessibleConstr:=true;
 end;
end;

procedure ProcessRequirements;
 var lImp,lConstrNr,i: integer;
     lCurrWord: Lexem;
     lReq: Requirement;
     lOutEnvFile: MizOutStream; // ###TODO: OutEnvFileObj;
     lImpReqs : ImpRequirementsObj;
 label 1;
begin
 writeln(gLogFile,'Requirements');
 for lImp:=0 to Env.Directive[syRequirements].Count-1 do
  with PImpArticleId(Env.Directive[syRequirements].Items^[lImp])^ do
   begin CurPos:=fPos;
{       LoadDCOBases(lPath);}
       lImpReqs.GetRequirements(LibraryPath(LowerCase(fStr),'.dre'));
    if lImpReqs.fRequirements.Count > 0 then
    begin
       InitDCOBases(LibraryPath(LowerCase(fStr),'.dre'),lImpReqs);
       for i:=0 to lImpReqs.fRequirements.Count-1 do
       with ReqPtr(lImpReqs.fRequirements.Items^[i])^ do
        begin
         lCurrWord.Kind := fConstr.Kind;
         lCurrWord.Nr:=gTrans[ConstructorKind(fConstr.Kind)].Value(fConstr.Nr);
         if not AccessibleConstr(lCurrWord) then
         begin ErrImm(856);
           writeln(gLogFile,'  Inaccessible requirements for: ',
                            lCurrWord.Kind,lCurrWord.Nr);
           goto 1;
         end
         else case fConstr.Kind of
          'K': lConstrNr:=gChosen[coFunctor].ElemNr(lCurrWord.Nr)+1;
          'G': lConstrNr:=gChosen[coAggregate].ElemNr(lCurrWord.Nr)+1;
          'U': lConstrNr:=gChosen[coSelector].ElemNr(lCurrWord.Nr)+1;
          'M': lConstrNr:=gChosen[coMode].ElemNr(lCurrWord.Nr)+1;
          'L': lConstrNr:=gChosen[coStructMode].ElemNr(lCurrWord.Nr)+1;
          'R': lConstrNr:=gChosen[coPredicate].ElemNr(lCurrWord.Nr)+1;
          'V': lConstrNr:=gChosen[coAttribute].ElemNr(lCurrWord.Nr)+1;
         end; { case }
         if (0<=fReqNr) and (fReqNr<=ord(high(Requirement))) then
          if (gBuiltIn[Requirement(fReqNr)] <> 0) and
             (gBuiltIn[Requirement(fReqNr)] <> lConstrNr)
             then
           begin ErrImm(858); goto 1 end
          else gBuiltIn[Requirement(fReqNr)]:=lConstrNr
         else begin ErrImm(857); goto 1 end;
        end;
1:
       RestoreDCOBases;
      end
     else Error(fPos,855);
   end;
 lOutEnvFile.OpenFile(EnvFileName+'.ere');
 for lReq:=low(Requirement) to high(Requirement) do
  begin lOutEnvFile.OutInt(gBuiltIn[lReq]); lOutEnvFile.OutNewLine end;
 lOutEnvFile.Done;
end; { ProcessRequirements }
 

procedure ChooseConstrutors;
 var i,lConstr: integer; c: ConstructorsKind;
begin
 gMarked.Init;

 for lConstr:=1 to ImpConstrNbr do
 with ImpConstr[lConstr]^,gMarked do
{ !!!!!!!!!!!!!!!!!!!!! markowanie konstruktorow jezeli sa dolaczone
  if Env.Directive[syConstructors].Exist(fStr) then
}
  begin
   if Localization and (Env.Directive[syConstructors].IndexOfStr(fStr)<0)
    then continue;
   for i:= nBase[coMode]+1 to ImpConstr[lConstr+1]^.nBase[coMode] do
    fNbr[coMode].Up(i);
   for i:=nBase[coFunctor]+1 to ImpConstr[lConstr+1]^.nBase[coFunctor] do
    fNbr[coFunctor].Up(i);
   for i:=nBase[coAttribute]+1 to ImpConstr[lConstr+1]^.nBase[coAttribute] do
    fNbr[coAttribute].Up(i);
   for i:=nBase[coPredicate]+1 to ImpConstr[lConstr+1]^.nBase[coPredicate] do
    fNbr[coPredicate].Up(i);
   for i:=nBase[coStructMode]+1 to ImpConstr[lConstr+1]^.nBase[coStructMode] do
    fNbr[coStructMode].Up(i);
   for i:=nBase[coAggregate]+1 to ImpConstr[lConstr+1]^.nBase[coAggregate] do
    fNbr[coAggregate].Up(i);
   for i:=nBase[coSelector]+1 to ImpConstr[lConstr+1]^.nBase[coSelector] do
    fNbr[coSelector].Up(i);    
  end;

 for c:=coMode to coAggregate do
  begin gChosen[c].Init(0,100); gRefused[c].Init(0,100) end;

 gChosen[coMode].InsertElem(1);
 for i:=2 to gConstrNbr[coMode] do ChooseMode(i);
 for i:=1 to gConstrNbr[coFunctor] do
  ChooseFunctor(i);
 for i:=1 to gConstrNbr[coAttribute] do ChooseAttribute(i);
 for i:=1 to gConstrNbr[coPredicate] do ChoosePredicate(i);
 for i:=1 to gConstrNbr[coAggregate] do ChooseAggregate(i);
 for i:=1 to gConstrNbr[coStructMode] do ChooseStructMode(i);
 for i:=1 to gConstrNbr[coSelector] do ChooseSelector(i);

// gChosenClusters.Init(0,100);
// gChosenClusters.InsertElem(0);
// for i:=0 to gClusterColl.fCount-1 do
// if ChosenCluster(gClusterColl.fItems^[i]) then gChosenClusters.InsertElem(i);

end;

procedure SaveFRM;
begin
 gFormatsColl.StoreFormats(EnvFileName+'.frm');
end;

procedure InitNotations;
var nk: NotationKind;
begin
 for nk:=Low(NotationKind) to High(NotationKind) do
  Notat[ nk].Init( MaxNotatNbr( nk));
end;

function AccessiblePattern(fPatt:PatternPtr): boolean;
 var i: integer;
begin AccessiblePattern:=false;
 with fPatt^ do
 begin
  for i:=0 to fPrimTypes.Count-1 do
   if not AccessibleType(fPrimTypes.Items^[i]) then exit;
  with rConstr do
   case Kind of
    'K': if not gChosen[coFunctor].HasInDom(Nr) then exit;
    'G': if not gChosen[coAggregate].HasInDom(Nr) then exit;
    'U': if not gChosen[coSelector].HasInDom(Nr) then exit;
    'M': if (Nr > 0) and not gChosen[coMode].HasInDom(Nr) then exit;
    'L':  if not gChosen[coStructMode].HasInDom(Nr) then exit;
    'R':  if not gChosen[coPredicate].HasInDom(Nr) then exit;
    'V':  if not gChosen[coAttribute].HasInDom(Nr) then exit;
    'J':  if not gChosen[coAggregate].HasInDom(Nr) then exit;
   end;
  AccessiblePattern:=true;
 end;
end;

function CollectFormat( aFormat: MFormatPtr): integer;
var lNr,lRightSymbolNr,lFormatNr: integer;
begin
 lFormatNr:=0;
 case aFormat^.fSymbol.Kind of
  'R':
   with MInfixFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr['R'].Value(fSymbol.Nr);
   if lNr <= gSymBase['R'] then
   begin
    lFormatNr:=gFormatsColl.CollectPredForm(lNr,fLeftArgsNbr,fRightArgsNbr);
    if PredMaxArgs.fList^[lNr] < fRightArgsNbr then
     PredMaxArgs.fList^[lNr] := fRightArgsNbr;
   end;
  end;
  'V','U':
   with MPrefixFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr[ fSymbol.Kind].Value(fSymbol.Nr);
   if lNr<=gSymBase[ fSymbol.Kind] then
    lFormatNr:=gFormatsColl.CollectPrefixForm( fSymbol.Kind, lNr, fRightArgsNbr);
  end;
  'J':
   with MPrefixFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr['G'].Value(fSymbol.Nr);
   if lNr <= gSymBase['G'] then
    lFormatNr:=gFormatsColl.CollectPrefixForm( fSymbol.Kind, lNr, fRightArgsNbr);
  end;
  'M':
   with MPrefixFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr['M'].Value(fSymbol.Nr);
   if lNr <= gSymBase['M'] then
   begin
    lFormatNr:=gFormatsColl.CollectPrefixForm('M',lNr,fRightArgsNbr);
    if ModeMaxArgs.fList^[lNr] < fRightArgsNbr then
     ModeMaxArgs.fList^[lNr] := fRightArgsNbr;
   end;
  end;
  'G':
   with MPrefixFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr['G'].Value(fSymbol.Nr);
   if lNr <= gSymBase['G'] then
    lFormatNr:=gFormatsColl.CollectPrefixForm('G',lNr,fRightArgsNbr);
  end;
  'L':
   with MPrefixFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr['G'].Value(fSymbol.Nr);
   if lNr <= gSymBase['G'] then
   begin
    lFormatNr:=gFormatsColl.CollectPrefixForm('L',lNr,fRightArgsNbr);
    if StructModeMaxArgs.fList^[lNr] < fRightArgsNbr then
     StructModeMaxArgs.fList^[lNr] := fRightArgsNbr;
   end;
  end;
  'O':
   with MInfixFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr['O'].Value(fSymbol.Nr);
   if lNr<=gSymBase['O'] then
    lFormatNr:=gFormatsColl.CollectFuncForm(lNr,fLeftArgsNbr,fRightArgsNbr);
  end;
  'K':
   with MBracketFormatPtr(aFormat)^ do
  begin
   lNr:=SymbolNr['K'].Value(fSymbol.Nr);
   lRightSymbolNr:=SymbolNr['L'].Value(fRightSymbolNr);
   if (lNr<=gSymBase['K']) and (lRightSymbolNr<=gSymBase['L']) then
    lFormatNr:=gFormatsColl.CollectBracketForm(lNr,lRightSymbolNr,fArgsNbr,fLeftArgsNbr,fRightArgsNbr);
  end;
 else RunTimeError(2093);
 end; { case }
 CollectFormat:= lFormatNr;
end;
 
procedure ProcessNotation;
 var Imp,lAccNbr,i,j,l,r: integer;
     c: char;
     n: NotationKind;
     lImpNotation: ImpNotationObj;
     lPattern:PatternPtr;
     lTokBase: SymbolCounters;
     lDictBase: DictBasePtr;
     lDictList: IntSequence;
begin
 writeln(gLogFile,'Notations');
 InitFormats;
 InitNotations;
 gNotation.Init(50,10);
 for n:=noMode to noForgetFunctor do
  gAccNotat[n].Init(10,10);

 for Imp:=0 to Env.Directive[syNotations].Count-1 do
  with PImpArticleId(Env.Directive[syNotations].Items^[Imp])^ do
 begin
  lImpNotation.GetNotation(LibraryPath(LowerCase(fStr),'.dno'));
{TODO reove after debugging, PutNotation will be in XML too}
{$ifdef   IMPDEBUG}
  lImpNotation.PutNotation(fStr+'test');
{$endif}
  if lImpNotation.fNotation.Count <= 0 then Error(fPos,803)
  else
  begin
     CurPos:=fPos;
     gNotation.Insert(new(AccImpNotationPtr, Init(fStr,gNotat)));
     lAccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.dno'),lImpNotation);
     lDictList.Init(0);
     r:=lDictList.Insert(0);
     FillChar(SymbolNbr,sizeof(SymbolNbr),0);
     with lImpNotation.fVocabularies do
     for l:=0 to fCount-1 do
      begin
        i:=DictBase.IndexOfStr(fList^[l].fString^);
        with AbsVocabularyPtr(fList^[l].fObject)^ do
        if i>0 then
         begin
          for c:='G' to 'V' do
           begin
            with DictBase do
            if (DictBasePtr(Items^[i])^.nBase[c] -
                DictBasePtr(Items^[i-1])^.nBase[c]) <> fSymbolCnt[c]
             then
              begin
               RunTimeError(802);
              end;
            inc(SymbolNbr[c],fSymbolCnt[c]);
           end;
          r:=lDictList.Insert(i);
         end
        else
         begin
          lTokBase:=DictBasePtr(DictBase.Items^[DictBase.Count-1])^.nBase;
          r:=lDictList.Insert(DictBase.Count);
          lDictBase:=new(DictBasePtr,Init(fList^[l].fString^));
          lDictBase^.nBase:=lTokBase;
          for c:='G' to 'V' do
           begin inc(lDictBase^.nBase[c],fSymbolCnt[c]);
            inc(SymbolNbr[c],fSymbolCnt[c]);
           end;
          DictBase.Insert(lDictBase);
         end;
      end;
     for c:='G' to 'V' do
      begin
       SymbolNr[c].InitNatFunc(SymbolNbr[c]+1,0);
       SymbolNr[c].Assign(0,0);
      end;
     fillchar(SymbolNbr,sizeof(SymbolNbr),0);
     with lDictList do
     for i:=1 to fCOunt-1 do
      begin
       for c:='G' to 'V' do
        for j:=DictBasePtr(DictBase.Items^[fList^[i]-1])^.nBase[c]+1
            to DictBasePtr(DictBase.Items^[fList^[i]])^.nBase[c] do
         begin
          inc(SymbolNbr[c]);
          SymbolNr[c].Assign(SymbolNbr[c],j);
         end;
      end;
     lDictList.Done;
     
     with lImpNotation.fNotation do
      for i:=0 to Count-1 do
      with PatternPtr(Items^[i])^ do
       begin
        TransMMLTypeList(fPrimTypes);
        if Expansion <> nil then
          TransMMLType(Expansion);
       end;
     
     for i:=0 to lImpNotation.fNotation.Count-1 do
     with PatternPtr( lImpNotation.fNotation.Items^[i])^ do
     begin
      lPattern:= lImpNotation.fNotation.Items^[i];
      fFormNr:= CollectFormat( fFormat);
      if fFormat<>nil then  dispose(fFormat,Done);
      fFormat:= nil;
      rConstr.Nr:=gTrans[ConstructorKind(rConstr.Kind)].Value(rConstr.Nr);
      
      IncIndex(gNotat[ fKind ], NotatIndex( fKind));
      if not AccessiblePattern(lPattern) then
       lPattern^.fFormNr:=0
      else if (lPattern^.Expansion <> nil) and
               not AccessibleType(lPattern^.Expansion) then
       lPattern^.fFormNr:=0;       
      if (lPattern^.fFormNr<>0) then
      begin
       gAccNotat[ fKind].InsertElem(gNotat[ fKind]);
       inc(lAccNbr);
      end;
      Notat[ fKind].Insert(lPattern);
      lImpNotation.fNotation.Items^[i]:= nil;
     end; { of the lImpNotation loop}
     
     if lAccNbr = 0 then
     begin
      writeln(gLogFile,'**** Empty directive ****');
      ErrImm(830);
     end;
  end; { else }
  RestoreDCOBases;
  lImpNotation.Done;
  for c:='G' to 'V' do SymbolNr[c].Done;
 end;
 gNotation.Insert(new(AccImpNotationPtr, Init(ArticleName,gNotat)));
end;

procedure ProcessFormatsFrom;
 var Imp,i,j,l,r,lFormatNr: integer;
     c: char;
     lImpNotation: ImpNotationObj;
     lTokBase: SymbolCounters;
     lDictBase: DictBasePtr;
     lDictList: IntSequence;
begin
 writeln(gLogFile,'Formats');
 for Imp:=0 to Env.Directive[syNotations].Count-1 do
 with PImpArticleId(Env.Directive[syNotations].Items^[Imp])^ do
 begin
  lImpNotation.GetNotation(LibraryPath(LowerCase(fStr),'.dno'));
  if lImpNotation.fNotation.Count <= 0 then Error(fPos,803)
  else
   begin
     CurPos:=fPos;
     lDictList.Init(0);
     r:=lDictList.Insert(0);
     FillChar(SymbolNbr,sizeof(SymbolNbr),0);
     with lImpNotation.fVocabularies do
     for l:=0 to fCount-1 do
      begin
        i:=DictBase.IndexOfStr(fList^[l].fString^);
        with AbsVocabularyPtr(fList^[l].fObject)^ do
        if i>0 then
         begin
          for c:='G' to 'V' do
           begin
            with DictBase do
            if (DictBasePtr(Items^[i])^.nBase[c] -
                DictBasePtr(Items^[i-1])^.nBase[c]) <> fSymbolCnt[c]
             then
              begin
               RunTimeError(802);
              end;
            inc(SymbolNbr[c],fSymbolCnt[c]);
           end;
          r:=lDictList.Insert(i);
         end
        else
         begin
          lTokBase:=DictBasePtr(DictBase.Items^[DictBase.Count-1])^.nBase;
          r:=lDictList.Insert(DictBase.Count);
          lDictBase:=new(DictBasePtr,Init(fList^[l].fString^));
          lDictBase^.nBase:=lTokBase;
          for c:='G' to 'V' do
           begin inc(lDictBase^.nBase[c],fSymbolCnt[c]);
            inc(SymbolNbr[c],fSymbolCnt[c]);
           end;
          DictBase.Insert(lDictBase);
         end;
      end;

     fillchar(SymbolNr,sizeof(SymbolNr),0);
     for c:='G' to 'V' do
      begin
       SymbolNr[c].InitNatFunc(SymbolNbr[c]+1,0);
       SymbolNr[c].Assign(0,0);
      end;
     fillchar(SymbolNbr,sizeof(SymbolNbr),0);
     with lDictList do
     for i:=1 to fCOunt-1 do
      begin
       for c:='G' to 'V' do
        for j:=DictBasePtr(DictBase.Items^[fList^[i]-1])^.nBase[c]+1
            to DictBasePtr(DictBase.Items^[fList^[i]])^.nBase[c] do
         begin
          inc(SymbolNbr[c]);
          SymbolNr[c].Assign(SymbolNbr[c],j);
         end;
      end;
     lDictList.Done;
     with lImpNotation,fNotation do
      for i:=0 to Count-1 do
       lFormatNr:= CollectFormat( PatternPtr(Items^[i])^.fFormat);
     lImpNotation.Done;
     for c:='G' to 'V' do SymbolNr[c].Done;
    end;
  end;
end;

(* ##RNC:
## Notations, either imported from other articles ( after accommodation)
## - the signature is implicit in that case, or exported from the current
## article - then the signature and vocabularies have to be specified.
## atAid optionally specifies article's name in uppercase.   
elNotations = 
 element elNotations {
   attribute atAid { xsd:string }?,
   (elSignature, elVocabularies)?, elPattern* }
*)
procedure OutNotations;
var i,j: integer;
n: NotationKind; lOutEnvFile: OutEnvFileObj;
lOutFile: MizOutStream; // ###TODO: OutEnvFileObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.eno');
 with lOutEnvFile do
 begin
  Out_XElStart( elNotations);
  Out_XAttr( atAid, ArticleID);
  Out_XMizQuotedAttr( atMizfiles, MizFiles);
  Out_XAttrEnd;
  for n:=Low(NotationKind) to High(NotationKind) do
  begin
   j:= 0;
   for i:=0 to Notat[n].Count-1 do
    if PatternPtr(Notat[n].Items^[i])^.fFormNr <> 0 then
    begin inc(j); Out_Pattern(Notat[n].Items^[i], j); end;
  end;
  Out_XElEnd( elNotations);
  Done;
 end;
 lOutFile.OpenFile(EnvFileName+'.nol');
 lOutFile.OutInt(gNotation.Count); lOutFile.OutNewLine;
 for i:=0 to gNotation.Count-1 do
  with AccImpNotationPtr(gNotation.Items^[i])^ do
  begin
   lOutFile.OutString(fStr); lOutFile.OutNewLine;
   for n:=Low(NotationKind) to High(NotationKind) do
    lOutFile.OutInt(nBase[n]);
   lOutFile.OutNewLine;
  end;
 lOutFile.OutChar('!'); lOutFile.Done;
 
// ##TODO: this is used only in JFM stuff 
 lOutFile.OpenFile(EnvFileName+'.ano');
 for n:=Low(NotationKind) to High(NotationKind) do
  with gAccNotat[n] do
   begin
    lOutFile.OutWord( NotationRepr(n), Count);
    for i:=0 to Count-1 do lOutFile.OutInt(Items^[i].X);
    lOutFile.OutNewLine;
   end;
 lOutFile.Done;
end;

procedure DisposeNotations;
var n: NotationKind;
begin
 gNotation.Done;
 for n:=noMode to noForgetFunctor do
 begin
  Notat[n].Done;
  gAccNotat[n].Done;
 end;
end;

procedure OunInaccessibility;
 var u: ConstructorsKind;
     lUConstrNr,j: integer;
     lUConstr: Lexem;
begin
 for u:=coMode to coAggregate do
  with gInaccessible[u] do
  if Count>0 then
   begin
    for j:=0 to Count-1 do
     begin
      lUConstr.Kind:=ConstructorRepr(u);
      lUConstr.Nr:=Items^[j].X;
      lUConstrNr:=ImpConstrNr(lUConstr);
      with ImpConstr[lUConstrNr]^ do
       begin
        write(gLogFile,'      -');
        if Env.Directive[syConstructors].IndexOfStr(fStr)>=0
         then write(gLogFile,' ')
         else write(gLogFile,'*');
        writeln(gLogFile,fStr,':',lUConstr.Kind,lUConstr.Nr-nBase[u]);
       end;
{      with ImpConstr[lUConstrNr]^ do
       writeln(gLogFile,'      - ',fStr,':',lUConstr.Kind,lUConstr.Nr-nBase[u]);}
     end;
   end;
end;

procedure OutConstructorsList;
 var i,lConstrNr,lUAConstrNbr: integer; c: ConstructorsKind;
     lConstr: Lexem; lOutEnvFile: MizOutStream; // ###TODO: OutEnvFileObj;
begin
 lOutEnvFile.OpenFile(EnvFileName+'.sgl');
 lOutEnvFile.OutInt(ImpConstrNbr); lOutEnvFile.OutNewLine;
 for i:=1 to ImpConstrNbr do
  begin lOutEnvFile.OutString(ImpConstr[i]^.fStr); lOutEnvFile.OutNewLine end;
 for i:=1 to ImpConstrNbr+1 do
 with ImpConstr[i]^ do
  begin lOutEnvFile.OutInt(nBase[coFunctor]); lOutEnvFile.OutInt(nBase[coPredicate]);
   lOutEnvFile.OutInt(nBase[coAttribute]); lOutEnvFile.OutInt(nBase[coMode]);
   lOutEnvFile.OutInt(nBase[coStructMode]); lOutEnvFile.OutInt(nBase[coAggregate]);
   lOutEnvFile.OutInt(nBase[coSelector]);
   lOutEnvFile.OutNewLine;
  end;
 lOutEnvFile.Done;

 writeln(gLogFile,'  The list of all imported constructors');
 write(gLogFile,'   ');
 for c:=coMode to coAggregate do
  write(gLogFile,ConstructorRepr(c):4);
 writeln(gLogFile);
 for i:=1 to ImpConstrNbr do
  begin
   write(gLogFile,'   ');
   if Env.Directive[syConstructors].IndexOfStr(ImpConstr[i]^.fStr)>=0 then
    write(gLogFile,'+')
   else write(gLogFile,'-');
   writeln(gLogFile,ImpConstr[i]^.fStr);
   with ImpConstr[i+1]^ do
    begin
     write(gLogFile,'   ');
     for c:=coMode to coAggregate do
      if ImpConstr[i]^.nBase[c] <> nBase[c] then
       begin
        if nBase[c]-ImpConstr[i]^.nBase[c] < 10 then
         write(gLogFile,' ':2)
        else if nBase[c]-ImpConstr[i]^.nBase[c] < 100 then
         write(gLogFile,' ');
        write(gLogFile,'+',nBase[c]-ImpConstr[i]^.nBase[c]);
       end
      else write(gLogFile,' ':4);
     writeln(gLogFile);
     write(gLogFile,'   ');
     for c:=coMode to coAggregate do
      write(gLogFile,nBase[c]:4);
     writeln(gLogFile);
    end;
  end;

 lOutEnvFile.OpenFile(EnvFileName+'.cho');
 for c:=coMode to coAggregate do
  with gChosen[c] do
   begin
    lOutEnvFile.OutWord(ConstructorRepr(c),Count);
    for i:=0 to Count-1 do lOutEnvFile.OutInt(Items^[i].X);
    lOutEnvFile.OutNewLine;
    lOutEnvFile.OutInt(gConstrNbr[c]);
    lOutEnvFile.OutNewLine;
   end;
{ with gChosenClusters do
  begin
   lOutEnvFile.OutWord('C',Count);
   for i:=0 to Count-1 do lOutEnvFile.OutInt(Items^[i].X);
  end;
}
 lOutEnvFile.Done;

 lUAConstrNbr:=0;
 for c:=coMode to coAggregate do
  begin
   with gRefused[c] do
    if Count > 0 then
    begin
     if lUAConstrNbr = 0 then
      begin
       writeln(gLogFile,'  Inaccessible Constructors');
       writeln(gLogFile);
      end;
     inc(lUAConstrNbr);
     case c of
      coMode: writeln(gLogFile,'   Modes: M');
      coStructMode: writeln(gLogFile,'   Structural Modes: G');
      coAttribute: writeln(gLogFile,'   Attributes: V');
      coPredicate: writeln(gLogFile,'   Predicates: R');
      coFunctor: writeln(gLogFile,'   Functors: K');
      coSelector: writeln(gLogFile,'   Selectors: U');
      coAggregate: writeln(gLogFile,'   Aggregates: L');
     end;
     for i:=0 to Count-1 do
      begin
       lConstr.Kind:=ConstructorRepr(c);
       lConstr.Nr:=Items^[i].X;
       lConstrNr:=ImpConstrNr(lConstr);
       with ImpConstr[lConstrNr]^ do
        begin
         write(gLogFile,'   -');
         if Env.Directive[syConstructors].IndexOfStr(fStr)>=0
          then write(gLogFile,' ')
          else write(gLogFile,'*');
         writeln(gLogFile,fStr,':',lConstr.Kind,lConstr.Nr-nBase[c]);
        end;
        InitAccessibility;
        AccessibilityConstr(lConstr);
        OunInaccessibility;
        FinishAccessibility;
      end;
     writeln(gLogFile);
    end;
  end;
end;


var gAccRegistrations: array of boolean;

// ##TODO: this could be simplified now with the cluster objects unified
(* ##RNC:
## Registrations, either imported from other articles ( after accommodation)
## - the signature is implicit in that case, or exported from the current
## article - then the signature has to be specified.
## atAid optionally specifies its article's name in uppercase.
elRegistrations =
 element elRegistrations {
   attribute atAid { xsd:string }?,
   elSignature?,
   ( elRCluster | elCCluster | elFCluster )*
   }
*)
procedure ProcessClusters;
 var i,l,
     lAccNbr,lInaccNbr: integer;
     lConsequent: ClusterRec;
     lAntecedent: AttrCollectionPtr;
     lClusterKind: ClusterKind;
     lPrimaryList: MCollection;
     lClusterTerm: TrmPtr;
     lClusterType: TypPtr;
     lOutEnvFile: OutEnvFileObj;
     lImpClusters      : ImpClustersObj;
     lRegNr,lFunNr,lCondNr: integer;
 label 1;
begin
 writeln(gLogFile,'Registrations');
 RegisteredCluster.Init(100);
 ConditionalCluster.Init(100);
 FunctorCluster.InitSorted( 100, CmpFuncCluster);
 for i:=0 to Env.Directive[syRegistrations].Count-1 do
  with PImpArticleId(Env.Directive[syRegistrations].Items^[i])^ do
  begin
   lImpClusters.GetClusters(LibraryPath(LowerCase(fStr),'.dcl'));
   if lImpClusters.fClusters.Count > 0 then
    begin
     CurPos:=fPos;
     lAccNbr:=0;
     lInaccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.dcl'),lImpClusters);
     with lImpClusters.fClusters do
      for l:=0 to Count-1 do
       case ClusterPtr(Items^[l])^.nClusterKind of
       clRegistered:
         with RClusterPtr(Items^[l])^ do
          begin
           TransMMLCluster(nConsequent.Lower);
           TransMMLTypeColl(nPrimaryList);
           TransMMLType(nClusterType);
          end;
       clFunctor:
         with FClusterPtr(Items^[l])^ do
          begin
           TransMMLCluster(nConsequent.Lower);
           TransMMLTypeColl(nPrimaryList);
           TransMMLTerm(nClusterTerm);
           if nClusterType <> nil then
            TransMMLType(nClusterType);
          end;
       clConditional:
         with CClusterPtr(Items^[l])^ do
          begin
           TransMMLCluster(nAntecedent);
           TransMMLTypeColl(nPrimaryList);
           TransMMLType(nClusterType);
           TransMMLCluster(nConsequent.Lower);
          end;
       end;
     lRegNr:=0;
     lFunNr:=0;
     lCondNr:=0;
     with lImpClusters.fClusters do
      for l:=0 to Count-1 do
       begin
        lClusterKind:=ClusterPtr(Items^[l])^.nClusterKind;
        lPrimaryList.CopyCollection(ClusterPtr(Items^[l])^.nPrimaryList);
        inc(lRegNr);
        case lClusterKind of
        clRegistered:
         with RClusterPtr(Items^[l])^ do
          begin
           lConsequent.Lower:=CopyCluster(nConsequent.Lower);
           lConsequent.Upper:=CopyCluster(nConsequent.Lower);
           lClusterType:=nClusterType;
           if AccessibleTypeColl(nPrimaryList) and
              AccessibleType(nClusterType) then
            begin
             inc(lAccNbr);
             if not AccessibleCluster(lConsequent.Lower) then goto 1;
             RegisteredCluster.Insert(new(RClusterPtr,
                 Init(lRegNr,fStr,lConsequent,nPrimaryList,nClusterType)));
             Items^[l]:=nil;
             continue;
            end
          end;
        clFunctor:
         with FClusterPtr(Items^[l])^ do
          begin
           lConsequent.Lower:=CopyCluster(nConsequent.Lower);
           lConsequent.Upper:=CopyCluster(nConsequent.Lower);
           lClusterTerm:=nClusterTerm;
           inc(lFunNr);
           if AccessibleTypeColl(nPrimaryList) and
              AccessibleTerm(nClusterTerm) then
            begin
             if nClusterType <> nil then
              begin
               if not AccessibleType(nClusterType) then goto 1;
              end;
             inc(lAccNbr);
             if not AccessibleCluster(lConsequent.Lower) then goto 1;
             FunctorCluster.Insert(new(FClusterPtr,
                    Init(lFunNr,fStr,lConsequent,nPrimaryList,nClusterTerm,nClusterType)));
             Items^[l]:=nil;
             continue;
            end
          end;
        clConditional:
         with CClusterPtr(Items^[l])^ do
          begin
           lConsequent.Lower:=CopyCluster(nConsequent.Lower);
           lConsequent.Upper:=CopyCluster(nConsequent.Lower);
           lAntecedent:=CopyCluster(nAntecedent);
           lClusterType:=nClusterType;
           inc(lCondNr);
           if AccessibleTypeColl(nPrimaryList) and
              AccessibleType(nClusterType) then
            begin
             inc(lAccNbr);
             if not AccessibleCluster(lAntecedent) then goto 1;
             if not AccessibleCluster(lConsequent.Lower) then goto 1;
             ConditionalCluster.Insert(new(CClusterPtr,
                  Init(lCondNr,fStr,lAntecedent,lConsequent,nPrimaryList,nClusterType)));
             Items^[l]:=nil;
             continue;
            end
          end;
       end;
1:
       inc(lInaccNbr);
       if lInaccNbr = 1 then
        begin
         writeln(gLogFile,'  Inaccessible Clusters');
         writeln(gLogFile);
        end;
       InitAccessibility;
       case lClusterKind of
        clRegistered: writeln(gLogFile,'   Existential Cluster: ',RegisteredCluster.Count);
        clFunctor: writeln(gLogFile,'   Functor Cluster: ',FunctorCluster.Count);
        clConditional: writeln(gLogFile,'   Conditional Cluster: ',ConditionalCluster.Count);
       end;
       AccessibilityTypColl(lPrimaryList);
       if lClusterKind = clFunctor then AccessibilityTrm(lClusterTerm)
        else AccessibilityTyp(lClusterType);
       AccessibilityAttributes(lConsequent.Lower);
       if lClusterKind = clConditional then AccessibilityAttributes(lAntecedent);
       OunInaccessibility;
       FinishAccessibility;
       writeln(gLogFile);
       lPrimaryList.DeleteAll;
       lPrimaryList.Done;
      end;
     if lAccNbr <> 0 then
       gAccRegistrations[i]:=true;
(*     if lAccNbr = 0 then
      begin
       writeln(gLogFile,'**** Empty directive ****');
       if LibraryPath(LowerCase(fStr),'.did') = '' then
        ErrImm(831);
      end;*)
     RestoreDCOBases;
     lImpClusters.Done;
    end
//   else if LibraryPath(LowerCase(fStr),'.did') = '' then
//    Error(fPos,809);
  end;

(***
 InitTypes;
 InitRounding;
***)

 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.ecl');
 with lOutEnvFile do
 begin
  Out_XElStart( elRegistrations);
  lOutEnvFile.Out_XAttr( atAid, ArticleID);
  lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
  lOutEnvFile.Out_XAttrEnd;
  // ##TODO: write this as a loop going from Low(ClusterKind)
  for i:=0 to RegisteredCluster.Count-1 do
   Out_Cluster(RegisteredCluster.Items^[i]);
  for i:=0 to FunctorCluster.Count-1 do
   Out_Cluster(FunctorCluster.Items^[i]);
  for i:=0 to ConditionalCluster.Count-1 do
   Out_Cluster(ConditionalCluster.Items^[i]);
  Out_XElEnd( elRegistrations);
  Done;
 end;
end;

(* ##RNC:
## Identifications, either imported from other articles ( after accommodation)
## - the signature is implicit in that case, or exported from the current
## article - then the signature has to be specified.
## atAid optionally specifies its article's name in uppercase.   
elIdentifyRegistrations =
 element elIdentifyRegistrations {
   attribute atAid { xsd:string }?,
   elSignature?,
   elIdentify*
   }
*)  
procedure ProcessIdentify;
var lAccNbr,i,z: integer; lOutEnvFile: OutEnvFileObj;
     lImpIds,lIds: ImpIdentifyObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.eid');
 if Env.Directive[syRegistrations].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 writeln(gLogFile,'Registrations(Identify)');
 lIds.Init;
 for i:=0 to Env.Directive[syRegistrations].Count-1 do
  with PImpArticleId(Env.Directive[syRegistrations].Items^[i])^ do
  begin
   lImpIds.GetIdentify(LibraryPath(LowerCase(fStr),'.did'));
   if lImpIds.fIdentify.Count > 0 then
    begin
     CurPos:=fPos;
     lAccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.did'),lImpIds);
     with lImpIds.fIdentify do
     for z:=0 to Count-1 do
      TransMMLIdentify(Items^[z]);
     with lImpIds.fIdentify do
     for z:=0 to Count-1 do
      if AccesibleIdentify(Items^[z]) then
       begin
        inc(lAccNbr);
        lIds.fIdentify.Insert(Items^[z]);
        Items^[z]:=nil;
       end
      else
       begin {Log File info}
       end;
     RestoreDCOBases;
     if lAccNbr <> 0 then
      gAccRegistrations[i]:=true;
(*     if lAccNbr = 0 then
      begin
       writeln(gLogFile,'**** Empty directive ****');
       if LibraryPath(LowerCase(fStr),'.dcl') = '' then
        ErrImm(831);
      end;*)
     lImpIds.Done;
    end
//   else if LibraryPath(LowerCase(fStr),'.dcl') = '' then
//     Error(fPos,809);
  end;

 lOutEnvFile.Out_XElStart( elIdentifyRegistrations);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
 with lIds.fIdentify do
  for i:=0 to Count-1 do
   lOutEnvFile.Out_Identify( Items^[i]);
 lOutEnvFile.Out_XElEnd( elIdentifyRegistrations);
 lOutEnvFile.Done;
 lIds.Done;
end;

procedure ProcessReductions;
var lAccNbr,i,z: integer; lOutEnvFile: OutEnvFileObj;
     lImpIds,lIds: ImpReductionObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.erd');
 if Env.Directive[syRegistrations].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 writeln(gLogFile,'Registrations(Reduction)');
 lIds.Init;
 for i:=0 to Env.Directive[syRegistrations].Count-1 do
  with PImpArticleId(Env.Directive[syRegistrations].Items^[i])^ do
  begin
   lImpIds.GetReductions(LibraryPath(LowerCase(fStr),'.drd'));
   if lImpIds.fReductions.Count > 0 then
    begin
     CurPos:=fPos;
     lAccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.drd'),lImpIds);
     with lImpIds.fReductions do
     for z:=0 to Count-1 do
      TransMMLReduction(Items^[z]);
     with lImpIds.fReductions do
     for z:=0 to Count-1 do
      if AccesibleReduction(Items^[z]) then
       begin
        inc(lAccNbr);
        lIds.fReductions.Insert(Items^[z]);
        Items^[z]:=nil;
       end
      else
       begin {Log File info}
       end;
     RestoreDCOBases;
     if lAccNbr <> 0 then
      gAccRegistrations[i]:=true;
(*     if lAccNbr = 0 then
      begin
       writeln(gLogFile,'**** Empty directive ****');
       if LibraryPath(LowerCase(fStr),'.dcl') = '' then
        ErrImm(831);
      end;*)
     lImpIds.Done;
    end
//   else if LibraryPath(LowerCase(fStr),'.dcl') = '' then
//     Error(fPos,809);
  end;

 lOutEnvFile.Out_XElStart( elReductionRegistrations);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
 with lIds.fReductions do
  for i:=0 to Count-1 do
   lOutEnvFile.Out_Reduction( Items^[i]);
 lOutEnvFile.Out_XElEnd( elReductionRegistrations);
 lOutEnvFile.Done;
 lIds.Done;
end;

procedure ProcessProperties;
var lAccNbr,i,z: integer; lOutEnvFile: OutEnvFileObj;
     lImpPrs,lPrs: ImpPropertiesObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.epr');
 if Env.Directive[syRegistrations].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 writeln(gLogFile,'Registrations(Properties)');
 lPrs.Init;
 for i:=0 to Env.Directive[syRegistrations].Count-1 do
  with PImpArticleId(Env.Directive[syRegistrations].Items^[i])^ do
  begin
   lImpPrs.GetProperties(LibraryPath(LowerCase(fStr),'.dpr'));
   if lImpPrs.fProperties.Count > 0 then
    begin
     CurPos:=fPos;
     lAccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.dpr'),lImpPrs);
     with lImpPrs.fProperties do
     for z:=0 to Count-1 do
      TransMMLProperty(Items^[z]);
     with lImpPrs.fProperties do
     for z:=0 to Count-1 do
      if AccesibleProperty(Items^[z]) then
       begin
        inc(lAccNbr);
        lPrs.fProperties.Insert(Items^[z]);
        Items^[z]:=nil;
       end
      else
       begin {Log File info}
       end;
     RestoreDCOBases;
     if lAccNbr <> 0 then
       gAccRegistrations[i]:=true;
(*     if lAccNbr = 0 then
      begin
       writeln(gLogFile,'**** Empty directive ****');
       if LibraryPath(LowerCase(fStr),'.dpr') = '' then
        ErrImm(831);
      end;*)
     lImpPrs.Done;
    end
//   else if LibraryPath(LowerCase(fStr),'.dpr') = '' then
//     Error(fPos,809);
  end;

 lOutEnvFile.Out_XElStart( elPropertyRegistration);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
 with lPrs.fProperties do
  for i:=0 to Count-1 do
   lOutEnvFile.Out_PropertyReg( Items^[i]);
 lOutEnvFile.Out_XElEnd( elPropertyRegistration);
 lOutEnvFile.Done;
 lPrs.Done;
end;

(* ##RNC:
## Definientia, either imported from other articles ( after accommodation)
## - the signature is implicit in that case, or exported from the current
## article - then the signature has to be specified.
## atAid optionally specifies article's name in uppercase.
elDefinientia =
 element elDefinientia {
   attribute atAid { xsd:string }?,
   elSignature?, elDefiniens* }
*)
procedure ProcessDefinitions;
var lAccNbr,i,z: integer; lOutEnvFile: OutEnvFileObj;
     lImpDefs,lDefs : ImpDefinientiaObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.dfs');
 if Env.Directive[syDefinitions].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 writeln(gLogFile,'Definitions');
 lDefs.Init;
 for i:=0 to Env.Directive[syDefinitions].Count-1 do
  with PImpArticleId(Env.Directive[syDefinitions].Items^[i])^ do
  begin
   lImpDefs.GetDefinientia(LibraryPath(LowerCase(fStr),'.def'));
   if lImpDefs.fDefinientia.Count > 0 then
    begin
     CurPos:=fPos;
     lAccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.def'),lImpDefs);
     with lImpDefs.fDefinientia do
     for z:=0 to Count-1 do
      TransMMLDefiniens(Items^[z]);
     with lImpDefs.fDefinientia do
     for z:=0 to Count-1 do
      if AccesibleDefiniens(Items^[z]) then
       begin
        inc(lAccNbr);
        lDefs.fDefinientia.Insert(Items^[z]);
        Items^[z]:=nil;
       end
      else
       begin {Log File info}
       end;
     RestoreDCOBases;
     if lAccNbr = 0 then
      begin
       writeln(gLogFile,'**** Empty directive ****');
       ErrImm(832);
      end;
     lImpDefs.Done;
    end else Error(fPos,805);
  end;

 lOutEnvFile.Out_XElStart( elDefinientia);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
 with lDefs.fDefinientia do
  for i:=0 to Count-1 do
   lOutEnvFile.Out_Definiens( DefiniensPtr( Items^[i])^, i+1);
 lOutEnvFile.Out_XElEnd( elDefinientia);
 lOutEnvFile.Done;
 lDefs.Done;
end;

procedure ProcessEqualities;
var lAccNbr,i,z: integer; lOutEnvFile: OutEnvFileObj;
     lImpDefs,lDefs : ImpDefinientiaObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.dfe');
 if Env.Directive[syEqualities].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 writeln(gLogFile,'Definitions');
 lDefs.Init;
 for i:=0 to Env.Directive[syEqualities].Count-1 do
  with PImpArticleId(Env.Directive[syEqualities].Items^[i])^ do
  begin
   lImpDefs.GetDefinientia(LibraryPath(LowerCase(fStr),'.def'));
   if lImpDefs.fDefinientia.Count > 0 then
    begin
     CurPos:=fPos;
     lAccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.def'),lImpDefs);
     with lImpDefs.fDefinientia do
     for z:=0 to Count-1 do
      TransMMLDefiniens(Items^[z]);
     with lImpDefs.fDefinientia do
     for z:=0 to Count-1 do
      if AccesibleDefiniens(Items^[z]) then
       begin
        inc(lAccNbr);
        lDefs.fDefinientia.Insert(Items^[z]);
        Items^[z]:=nil;
       end
      else
       begin {Log File info}
       end;
     RestoreDCOBases;
     if lAccNbr = 0 then
      begin
       writeln(gLogFile,'**** Empty directive ****');
       ErrImm(832);
      end;
     lImpDefs.Done;
    end else Error(fPos,805);
  end;

 lOutEnvFile.Out_XElStart( elDefinientia);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
 with lDefs.fDefinientia do
  for i:=0 to Count-1 do
   lOutEnvFile.Out_Definiens( DefiniensPtr( Items^[i])^, i+1);
 lOutEnvFile.Out_XElEnd( elDefinientia);
 lOutEnvFile.Done;
 lDefs.Done;
end;

procedure ProcessExpansions;
var lAccNbr,i,z: integer; lOutEnvFile: OutEnvFileObj;
     lImpDefs,lDefs : ImpDefinientiaObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.dfx');
 if Env.Directive[syExpansions].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 writeln(gLogFile,'Definitions');
 lDefs.Init;
 for i:=0 to Env.Directive[syExpansions].Count-1 do
  with PImpArticleId(Env.Directive[syExpansions].Items^[i])^ do
  begin
   lImpDefs.GetDefinientia(LibraryPath(LowerCase(fStr),'.def'));
   if lImpDefs.fDefinientia.Count > 0 then
    begin
     CurPos:=fPos;
     lAccNbr:=0;
     InitDCOBases(LibraryPath(LowerCase(fStr),'.def'),lImpDefs);
     with lImpDefs.fDefinientia do
     for z:=0 to Count-1 do
      TransMMLDefiniens(Items^[z]);
     with lImpDefs.fDefinientia do
     for z:=0 to Count-1 do
      if AccesibleDefiniens(Items^[z]) then
       begin
        inc(lAccNbr);
        lDefs.fDefinientia.Insert(Items^[z]);
        Items^[z]:=nil;
       end
      else
       begin {Log File info}
       end;
     RestoreDCOBases;
     if lAccNbr = 0 then
      begin
       writeln(gLogFile,'**** Empty directive ****');
       ErrImm(832);
      end;
     lImpDefs.Done;
    end else Error(fPos,805);
  end;

 lOutEnvFile.Out_XElStart( elDefinientia);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
 with lDefs.fDefinientia do
  for i:=0 to Count-1 do
   lOutEnvFile.Out_Definiens( DefiniensPtr( Items^[i])^, i+1);
 lOutEnvFile.Out_XElEnd( elDefinientia);
 lOutEnvFile.Done;
 lDefs.Done;
end;

(* ##RNC:
## Theorems, either imported from other articles ( after accommodation)
## - the signature is implicit in that case, or exported from the current
## article - then the signature has to be specified.
## They can be either ordinary or definitional.
## The article number and order in article can be given,
## otherwise it belongs to the current article and order is implicit.
## Optional aid attribute specifies article name.
## atConstrKind and atConstrNr determine for def. theorems
## the defined constructor. If they do not appear (and atKind='D'), 
## then this is a canceled (verum) deftheorem.    
elTheorems = 
 element elTheorems { 
   attribute atAid { xsd:string }?,
   elSignature?,
   element elTheorem {
   ( attribute atArticleNr { xsd:integer },
     attribute atNr { xsd:integer } )?,
   ( attribute atConstrKind { 'M' | 'V' | 'R' | 'K' },
     attribute atConstrNr { xsd:integer})?, 
    attribute atAid { xsd:string }?,
    attribute atKind { 'T' | 'D' },
    Formula
   }*
 }
*)
procedure ProcessTheorems;
 var
  LibrNr,TheoNr,lThNr,lDefNr,lCurNr,i,lIDNr: integer;
  lAcc: NatSet;
  lTokens: TokensCollection;
  lToken: TokenPtr;
  lOutEnvFile: OutEnvFileObj;
  lImpTheo: ImpTheoremsObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.eth');
 if Env.Directive[syTheorems].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 lOutEnvFile.Out_XElStart( elTheorems);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
 writeln(gLogFile,'Theorems');
 lTokens.LoadDct(EnvFileName);
 for LibrNr:=0 to Env.Directive[syTheorems].Count-1 do
  with PImpArticleId(Env.Directive[syTheorems].Items^[LibrNr])^ do
   begin CurPos:=fPos;
    DisplayLine(CurPos.Line,ErrorNbr);
    lImpTheo.GetTheorems(LibraryPath(LowerCase(fStr),'.the'));
    if lImpTheo.fTheorems.Count>0 then
     begin
      InitDCOBases(LibraryPath(LowerCase(fStr),'.the'),lImpTheo);
      for i:=0 to lImpTheo.fTheorems.Count-1 do
       with TheoremPtr(lImpTheo.fTheorems.Items^[i])^ do
      begin
       TransMMLFormula(fTheorem);
       if fDefConstr.Kind <> '?' then
        fDefConstr.Nr:=
         gTrans[ConstructorKind(fDefConstr.Kind)].Value(fDefConstr.Nr);
      end;
      CurPos:= fPos;
     end;
    with lImpTheo do
    if fTheorems.Count > 0 then
     begin
      lAcc.Init(fTheorems.Count,0);
      for TheoNr:=0 to fTheorems.Count-1 do
       if AccessibleFormula( TheoremPtr(fTheorems.Items^[TheoNr])^.fTheorem)
       then lAcc.InsertElem( TheoNr);
      RestoreDCOBases;
      lToken:=TokenPtr(lTokens.ObjectOf(fStr));
      mizassert(2382,lToken<>nil);
      with lOutEnvFile do
      begin
       lDefNr:= 0; lThNr:= 0;
       for TheoNr:=0 to fTheorems.Count-1 do
        with TheoremPtr(fTheorems.Items^[TheoNr])^ do
       begin
        if ('D' = fTheoKind) then inc(lDefNr) else inc(lThNr);
        if ('D' = fTheoKind) then lCurNr:= lDefNr else lCurNr:= lThNr;
        if lAcc.HasInDom(TheoNr) then
        begin
         Out_XElStart( elTheorem);
         Out_XIntAttr( atArticleNr, lToken^.fLexem.Nr);
         Out_XIntAttr( atNr, lCurNr);
         Out_XAttr( atAid, lToken^.fStr);
         Out_XAttr( atKind, fTheoKind);
         if ('D' = fTheoKind) and  (fDefConstr.Kind <> '?') then
          with fDefConstr do
         begin
          Out_XAttr( atConstrKind, Kind);
          Out_XIntAttr( atConstrNr, Transf( ConstructorKind(Kind), Nr));
         end;
         Out_XAttrEnd;
         Out_Formula(fTheorem);
         Out_XElEnd( elTheorem);
        end;
       end;
      end;
      if lAcc.Count = 0 then
        begin
         writeln(gLogFile,'**** Empty directive ****');
         Error(fPos,833);
        end;
      lAcc.Done;
     end
    else
     begin
      RestoreDCOBases;
      Error(fPos,806);
     end;
   end;
 lTokens.Done;
 lOutEnvFile.Out_XElEnd( elTheorems);
 lOutEnvFile.Done;
end;


(* ##RNC:
## Schemes, either imported from other articles ( after accommodation)
## - the signature is implicit in that case, or exported from the current 
## article - then the signature has to be specified.
## atAid optionally specifies article's name in uppercase.
elSchemes = 
 element elSchemes {
   attribute atAid { xsd:string }?,
   elSignature?, elScheme* }
*)
procedure ProcessSchemes;
 var
  LibrNr,SchNr,lAccNbr: integer;
  lImpSchemes: ImpSchemeObj;
  LibrSch: MList;
  lTokens: TokensCollection;
  lToken: TokenPtr;
  lOutEnvFile: OutEnvFileObj;
begin
 lOutEnvFile.OpenFileWithXSL(EnvFileName+'.esh');
 if Env.Directive[sySchemes].Count = 0 then
  begin lOutEnvFile.EraseFile; exit end;
 lOutEnvFile.Out_XElStart( elSchemes);
 lOutEnvFile.Out_XAttr( atAid, ArticleID);
 lOutEnvFile.Out_XMizQuotedAttr( atMizfiles, MizFiles);
 lOutEnvFile.Out_XAttrEnd;
// lOutEnvFile.OutInt(Env.Directive[sySchemes].Count); lOutEnvFile.OutNewLine;
 writeln(gLogFile,'Schemes');
// lSchemes.Init;
 lTokens.LoadDct(EnvFileName);
 for LibrNr:=0 to Env.Directive[sySchemes].Count-1 do
  with PImpArticleId(Env.Directive[sySchemes].Items^[LibrNr])^ do
   begin
     lImpSchemes.GetSchemes(LibraryPath(LowerCase(fStr),'.sch'));
     if lImpSchemes.fSchemes.Count>0 then
      begin
       CurPos:=fPos;
       lAccNbr:=0;
       InitDCOBases(LibraryPath(LowerCase(fStr),'.sch'),lImpSchemes);
       for SchNr:=0 to lImpSchemes.fSchemes.Count-1 do
        with SchemePtr(lImpSchemes.fSchemes.Items^[SchNr])^ do
        begin TransMMLTypeColl(fSchTypes);
         TransMMLFormulaColl(fSchProps);
        end;
       for SchNr:=0 to lImpSchemes.fSchemes.Count-1 do
        with SchemePtr(lImpSchemes.fSchemes.Items^[SchNr])^ do
        begin
          if AccessibleTypeColl(fSchTypes) and
                 AccessibleFormulaColl(fSchProps) then
           begin
            inc(lAccNbr);
//            lSchemes.fSchemes.Insert(lImpSchemes.fSchemes.Items^[SchNr]);
//            lImpSchemes.fSchemes.Items^[SchNr]:=nil;
            lToken:=TokenPtr(lTokens.ObjectOf(fStr));
            mizassert(2383,lToken<>nil);
            lOutEnvFile.Out_XElStart( elScheme);
            lOutEnvFile.Out_XIntAttr( atArticleNr, lToken^.fLexem.Nr);
            lOutEnvFile.Out_XIntAttr( atNr, SchNr+1);
            lOutEnvFile.Out_XAttr( atAid, lToken^.fStr);
            lOutEnvFile.Out_XAttrEnd;
            lOutEnvFile.Out_ArgTypes( fSchTypes);
            lOutEnvFile.Out_FormulaColl(fSchProps);
            lOutEnvFile.Out_XElEnd( elScheme);
           end;
          fSchTypes.Done;
          fSchProps.Done;
        end;
       RestoreDCOBases;
       if lAccNbr = 0 then
        begin
         writeln(gLogFile,'**** Empty directive ****');
         ErrImm(834);
        end;
       lImpSchemes.Done;
      end
     else Error(fPos,807);
   end;
 lTokens.Done;
 lOutEnvFile.Out_XElEnd( elSchemes);
 lOutEnvFile.Done;
end;

procedure Accomodate;
 var i: integer;
begin
 Env.StoreEvl(EnvFileName+'.evl');
{ DCODocs.Init(100);} { average is 70 .dco for article}
 DoCtrans := false;
 if VocabulariesProcessing then
   begin
    with CurPos do begin Line:=1; Col:=1 end;
    InitDisplayLine('-Vocabularies ');
    ProcessVocabularies;
    DisplayLine(CurPos.Line,ErrorNbr);
    PrintVocabulariesList;
   end;
 if ConstructorsProcessing then
   begin
    with CurPos do begin Line:=1; Col:=1 end;
    InitDisplayLine('-Constructors ');
    ProcessConstructors;
    ChooseConstrutors;
    OutConstructorsList;
{$IFNDEF TESTNOACO}
    SaveAConstructors;
{$ENDIF}
    SaveConstructors;
    DisplayLine(CurPos.Line,ErrorNbr);
    with CurPos do begin Line:=1; Col:=1 end;
    InitDisplayLine('-Requirements ');
    ProcessRequirements;
    DisplayLine(CurPos.Line,ErrorNbr);
   end;
 setlength(gAccRegistrations,Env.Directive[syRegistrations].Count);
 for i := 0 to Env.Directive[syRegistrations].Count - 1 do
   gAccRegistrations[i]:=false;
 if ClustersProcessing then
   begin
    with CurPos do begin Line:=1; Col:=1 end;
    InitDisplayLine('-Registrations');
    ProcessClusters;
    DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if NotationsProcessing
  then
   begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Notations    ');
     ProcessNotation;
     SaveFRM;
     OutNotations;
     gFormatsColl.Done;
     gPriority.Done;
     DisposeNotations;
     if VocabulariesProcessing then PrintPRF(EnvFileName);
     DisplayLine(CurPos.Line,ErrorNbr);
   end
 else if FormatsProcessing then
   begin
    with CurPos do begin Line:=1; Col:=1 end;
    InitDisplayLine('-Formats      ');
    InitFormats;
    InitNotations;
    InitConstrNbr;
    InitSignatures;
    ProcessFormatsFrom;
    SaveFRM;
    gFormatsColl.Done;
    gPriority.Done;
    if VocabulariesProcessing then PrintPRF(EnvFileName);
    DisplayLine(CurPos.Line,ErrorNbr);
   end
  else if VocabulariesProcessing then PrintStrictPRF(EnvFileName);
// if DefinitionsProcessing or TheoremsProcessing or SchemesProcessing
//  then writeln;
 if IdentifyProcessing then
   begin
    if Env.Directive[syRegistrations].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Identify     ');
    end;
    ProcessIdentify;
    if Env.Directive[syRegistrations].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if ReductionProcessing then
   begin
    if Env.Directive[syRegistrations].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Reductions   ');
    end;
    ProcessReductions;
    if Env.Directive[syRegistrations].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if PropertiesProcessing then
  begin
    if Env.Directive[syRegistrations].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Properties   ');
    end;
    ProcessProperties;
    if Env.Directive[syRegistrations].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if ClustersProcessing and IdentifyProcessing and ReductionProcessing and PropertiesProcessing then
  begin
   for i := 0 to Env.Directive[syRegistrations].Count - 1 do
    with PImpArticleId(Env.Directive[syRegistrations].Items^[i])^do
    if not gAccRegistrations[i] then
     if (LibraryPath(LowerCase(fStr),'.dcl') = '') and
       (LibraryPath(LowerCase(fStr),'.did') = '') and
       (LibraryPath(LowerCase(fStr),'.drd') = '') and
       (LibraryPath(LowerCase(fStr),'.dpr') = '') then
      Error(fPos,809)
     else Error(fPos,831);
  end;
 if DefinitionsProcessing then
   begin
    if Env.Directive[syDefinitions].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Definitions  ');
    end;
    ProcessDefinitions;
    if Env.Directive[syDefinitions].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if EqualitiesProcessing then
   begin
    if Env.Directive[syEqualities].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Equalities   ');
    end;
    ProcessEqualities;
    if Env.Directive[syEqualities].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if ExpansionsProcessing then
   begin
    if Env.Directive[syExpansions].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Expansions   ');
    end;
    ProcessExpansions;
    if Env.Directive[syExpansions].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if TheoremsProcessing then
   begin
    if Env.Directive[syTheorems].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Theorems     ');
    end;
    ProcessTheorems;
    if Env.Directive[syTheorems].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 if SchemesProcessing then
   begin
    if Env.Directive[sySchemes].Count > 0 then
    begin
     with CurPos do begin Line:=1; Col:=1 end;
     InitDisplayLine('-Schemes      ');
    end;
    ProcessSchemes;
    if Env.Directive[sySchemes].Count > 0
     then DisplayLine(CurPos.Line,ErrorNbr);
   end;
 writeln;

{  DCODocs.Done;}
end;

end.
