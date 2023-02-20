(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit dicthan;
interface

uses mobjects;

const
  StandardPriority = 64;
  AvailableSymbols = ['G','K','L','M','O','R','U','V'];

type
  SymbolCounters = array['A'..'Z'] of word;
  SymbolIntSeqArr = array['A'..'Z'] of IntSequence;

  PSymbol = ^TSymbol;
  TSymbol  = object(MObject)
    Kind: Char;
    Repr,Infinitive: string;
    Prior: Byte;
   constructor Init(fKind:Char; fRepr,fInfinitive:String; fPriority:byte);
   constructor Extract(const aLine: String);
   function SymbolStr: String;
   constructor Load(var aText: text);
   procedure Store(var aText: text);
   destructor Done; virtual;
  end;

  AbsVocabularyPtr = ^AbsVocabularyObj;
  AbsVocabularyObj = object(MObject)
    fSymbolCnt: SymbolCounters;
   constructor Init;
   destructor Done; virtual;
  end;

  PVocabulary = ^TVocabulary;
  TVocabulary = object(AbsVocabularyObj)
    Reprs: MCollection;
   constructor Init;
   constructor ReadPrivateVoc(const aFileName: string);
   constructor LoadVoc(var aText: text);
   procedure StoreVoc(const aFileName: string; var aText: text);
   destructor Done; virtual;
  end;

function GetPrivateVoc(const fName:string):PVocabulary;
function GetPublicVoc(const fName:string; var fVocFile:text):PVocabulary;

procedure LoadMmlVcb(const aFileName: string; var aMmlVcb: MStringList);
procedure StoreMmlVcb(const aFileName: string; const aMmlVcb: MStringList);
procedure StoreMmlVcbX(const aFileName: string; const aMmlVcb: MStringList);

implementation

uses mizenv,xml_inout,xml_dict;

function IsValidSymbol(const aLine: string): boolean;
 var lLine: string;
     lKind: char;
     lPriority,lPos,lCode: integer;
begin
 IsValidSymbol:=false;
 lLine:=TrimString(aLine);
 if length(lLine)<=1 then exit;
 lKind:=lLine[1];
 if not (lKind in AvailableSymbols) then exit;
 Delete(lLine,1,1);
 case lKind of
 'O':
  begin
   IsValidSymbol:=true;
   lPos:=Pos(' ',lLine);
   if lPos <> 0 then
   begin
    Val(TrimString(Copy(lLine,lPos,length(lLine))),lPriority,lCode);
    lLine:=TrimString(Copy(lLine,1,lPos-1));
    IsValidSymbol:=(lCode = 0) and (lLine<>'');
   end;
  end;
 'R':
  begin
   lPos:=Pos(' ',lLine);
   if lPos <> 0 then
   begin
    lLine:=TrimString(Copy(lLine,lPos,length(lLine)));
    if Pos(' ',lLine) > 0 then exit;
   end;
   IsValidSymbol:=true;
  end
 else
  begin
   if Pos(' ',lLine) > 0 then exit;
   IsValidSymbol:=true;
  end;
 end;
end;

constructor TSymbol.Init(fKind:Char; fRepr,fInfinitive:String; fPriority:byte);
begin
 Kind:=fKind; Repr:=fRepr;
 Prior:=fPriority;
 Infinitive:='';
end;

constructor TSymbol.Extract(const aLine: string);
 var lPos,lCode:integer; lRepr: string;
begin
 Kind:=aLine[1];
 Repr:=TrimString(Copy(aLine,2,length(aLine)));
 Prior:=0;
 Infinitive:='';
 case Kind of
 'O':
  begin
   lPos:=Pos(' ',Repr);
   Prior:=StandardPriority;
   if lPos <> 0 then
    begin lRepr:=Repr; Repr:='';
     Val(TrimString(Copy(lRepr,lPos+1,length(lRepr))),Prior,lCode);
     Repr:=TrimString(Copy(lRepr,1,lPos-1));
    end;
  end;
 'R':
  begin
   lPos:=Pos(' ',Repr);
   if lPos <> 0 then
    begin lRepr:=Repr; Repr:='';
     Repr:=TrimString(Copy(lRepr,1,lPos-1));
     Infinitive:=TrimString(Copy(lRepr,lPos+1,length(lRepr)));
    end;
  end;
 end;
end;

function TSymbol.SymbolStr: String;
 var lStr,lIntStr: string;
begin
  lStr:=Kind+Repr;
  case Kind of
  'O':
   if Prior <> StandardPriority then
   begin
    Str(Prior,lIntStr);
    lStr:=lStr+' '+lIntStr;
   end;
  'R':
   if Infinitive <> '' then
    lStr:=lStr+' '+Infinitive;
  end;
  SymbolStr:=lStr;
end;

constructor TSymbol.Load(var aText: text);
 var lDictLine: string;
begin
 ReadLn(aText,lDictLine);
 lDictLine:=TrimString(lDictLine);
 if Length(lDictLine) = 0 then exit;
 Repr:=''; Prior:=0; Infinitive:='';
 if IsValidSymbol(lDictLine) then
   Extract(lDictLine);
end;

procedure TSymbol.Store(var aText: text);
begin
 WriteLn(aText,SymbolStr);
end;

destructor TSymbol.Done;
begin
 Repr:='';
 Infinitive:='';
end;

constructor AbsVocabularyObj.Init;
begin
 FillChar(fSymbolCnt,SizeOf(fSymbolCnt),0);
end;

destructor AbsVocabularyObj.Done;
begin
end;

constructor TVocabulary.Init;
begin
 FillChar(fSymbolCnt,SizeOf(fSymbolCnt),0);
 Reprs.Init(10,10);
end;

destructor TVocabulary.Done;
begin
 Reprs.Done;
end;

constructor TVocabulary.ReadPrivateVoc(const aFileName: string);
 var lDict: text;
     lDictLine: string;
     lSymbol: PSymbol;
begin
 Init;
 Assign(lDict,aFileName);
 {$I-} reset(lDict); {$I+}
 if ioresult <> 0 then exit;
 while not seekEOF(lDict) do
 begin
   readln(lDict,lDictLine);
   lDictLine:=TrimString(lDictLine);
   if length(lDictLine) > 1 then
   begin
     lSymbol:=new(PSymbol,Extract(lDictLine));
     if IsValidSymbol(lDictLine) then
      begin
       inc(fSymbolCnt[lSymbol^.Kind]);
       Reprs.Insert(lSymbol);
      end;
   end;
  end;
 Close(lDict);
end;

constructor TVocabulary.LoadVoc(var aText: text);
 var i, lSymbNbr, lNbr: integer;
     lKind,lDummy,c: Char;
begin
 lSymbNbr:=0;
 for c:='A' to 'Z' do if c in AvailableSymbols then
  begin
   Read(aText, lKind, lNbr, lDummy);
   fSymbolCnt[c]:=lNbr;
   Inc(lSymbNbr, fSymbolCnt[c]);
  end;
 ReadLn(aText);
 Reprs.Init(10,10);
 for i:=1 to lSymbNbr do
  begin
   Reprs.Insert(new(PSymbol,Load(aText)));
  end;
end;

procedure TVocabulary.StoreVoc(const aFileName: string; var aText: text);
 var i: Byte; c: Char;
begin
 WriteLn(aText, '#', aFileName);
 for c:='A' to 'Z' do
  if c in AvailableSymbols then Write(aText, c,fSymbolCnt[c], ' ');
 WriteLn(aText);
 for i:=0 to Reprs.Count - 1 do PSymbol(Reprs.Items^[i])^.Store(aText);
end;

function GetPrivateVoc(const fName:string):PVocabulary;
 var lName: string;
begin
 lName:=fName;
 if ExtractFileExt(lName) = '' then lName:=lName+'.voc';
 if not MFileExists(lName) then
  begin GetPrivateVoc:=nil;
   exit;
  end;
 GetPrivateVoc:=new(PVocabulary,ReadPrivateVoc(lName));
end;

function GetPublicVoc(const fName:string; var fVocFile:text): PVocabulary;
 var lLine: string;
begin GetPublicVoc:=nil;
 reset(fVocFile);
 while not eof(fVocFile) do
 begin readln(fVocFile,lLine);
  if (length(lLIne)>0) and (lLine[1]='#') and
     (copy(lLine,2,length(lLine)) = fName) then
   begin
    GetPublicVoc:=new(PVocabulary,LoadVoc(fVocFile));
    exit;
   end;
 end;
end;

procedure LoadMmlVcb(const aFileName: string; var aMmlVcb: MStringList);
 var lFile: text;
     lDummy: char;
     lDictName: string;
     r:Integer;
begin
  FileExam(aFileName);
  Assign(lFile, aFileName);
  Reset(lFile);
  aMmlVcb.Init(1000);
  aMmlVcb.fSorted:=true;
  while not eof (lFile) do
   begin
    ReadLn(lFile, lDummy, lDictName);
    r:=aMmlVcb.AddObject(lDictName,new(PVocabulary,LoadVoc(lFile)));
   end;
 Close(lFile);
end;

procedure StoreMmlVcb(const aFileName: string; const aMmlVcb: MStringList);
 var lFile: text;
     i: Integer;
begin
  Assign(lFile, aFileName);
  Rewrite(lFile);
  with aMmlVcb do
   for i:=0 to fCount - 1 do
    PVocabulary(fList^[i].fObject)^.StoreVoc(fList^[i].fString^,lFile);
  Close(lFile);
end;

procedure StoreMmlVcbX(const aFileName: string; const aMmlVcb: MStringList);
var i,s: Integer;
    c: char;
    VCXfile: XMLOutStreamPtr;
begin
 VCXfile:=new(XMLOutStreamPtr,OpenFile(aFileName));
 VCXfile.Out_XElStart0(XMLElemName[elVocabularies]);
 with aMmlVcb do
  for i:=0 to fCount - 1 do
   with PVocabulary(fList^[i].fObject)^ do
    begin
     VCXfile.Out_XElStart(XMLElemName[elVocabulary]);
     VCXfile.Out_XAttr(XMLAttrName[atName],fList^[i].fString^);
     VCXfile.Out_XAttrEnd;
     // Kinds
     for c:='A' to 'Z' do
      if c in AvailableSymbols then
       begin
        VCXfile.Out_XElStart(XMLElemName[elSymbolCount]);
        VCXfile.Out_XAttr(XMLAttrName[atKind],c);
        VCXfile.Out_XIntAttr(XMLAttrName[atNr],fSymbolCnt[c]);
        VCXfile.Out_XElEnd0;
       end;
     // Symbols
     VCXfile.Out_XElStart0(XMLElemName[elSymbols]);
     for s:=0 to Reprs.Count - 1 do
      with PSymbol(Reprs.Items[s])^ do
       begin
        VCXfile.Out_XElStart(XMLElemName[elSymbol]);
        VCXfile.Out_XAttr(XMLAttrName[atKind],Kind);
        VCXfile.Out_XAttr(XMLAttrName[atName],QuoteStrForXML(Repr));
        case Kind of
         'O': VCXfile.Out_XIntAttr(XMLAttrName[atPriority],Prior);
         'R': if Infinitive <> '' then VCXfile.Out_XAttr(XMLAttrName[atInfinitive],Infinitive);
        end;
        VCXfile.Out_XElEnd0;
       end;
     VCXfile.Out_XElEnd(XMLElemName[elSymbols]);
     VCXfile.Out_XElEnd(XMLElemName[elVocabulary]);
   end;
 VCXfile.Out_XElEnd(XMLElemName[elVocabularies]);
 dispose(VCXfile,Done);
end;

end.
