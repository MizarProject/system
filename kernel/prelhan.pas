(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

unit prelhan;

interface

procedure PRELTransfer;
procedure CreatePREL;

implementation

uses
  pcmizver,mizenv,mconsole,
  mobjects,iocorrel,formats,inoutmml,impobjs,
  correl,generato,identify
  {$IFDEF MDEBUG}, info, outinfo {$ENDIF};

var AllConstrs: ImpMultConstrObj;

function Exist( Ext:string):boolean;
begin
 Exist:=MFileExists(MizFileName+'.'+Ext);
end;

function LibrFileName(Ext: string):string;
begin
  if PublicLibr then
    LibrFileName:= 'prel'+DirSeparator+ArticleName[1]+DirSeparator+ArticleName+'.'+Ext
   else LibrFileName:= 'prel'+DirSeparator+ArticleName+'.'+Ext;
end; { LibrFileName }

procedure ProcessTYP;
var existsdco:boolean;
begin
  with AllConstrs do
  begin
    DoCTrans := false;
    existsdco := AddDCO;
    if not existsdco then exit;
    InitMark;
    Mark(fBases);
    Renumerate(true);
    DoCTrans := true;
    DrawPass('Constructors file is transfered into the local library');
    PutMarked(nil,LibrFileName('dco'));
{ writes a file in the old format }
{$ifdef IMPDEBUG}
   PutOldConstructors(LibrFileName('test'));
   ImpMultConstrObj(AllConstrs).PutOldConstructors(LibrFileName('test1'));
{$ENDIF}
  end;
end;

procedure Formats;
var lFormats: ImpFormatsMarkingObj;
begin
  if not Exist('dfr') then exit;
  with lFormats do
  begin
    DrawPass('Formats file is transfered into the local library');
    DoCTrans := false;
    GetFormats(MizFileName+'.dfr');
    MarkDicts;
    Renumerate;
    DoCTrans := true;
    DoSTrans := true;
    SymbolTrans:= @fSymbolTrans;
    PutMarked(LibrFileName('dfr'));
    DoCTrans := false;
    DoSTrans := false;
    Done;
  end;
end;

procedure Signature;
var lNotation: ImpNotationMarkingObj;
begin
  if not Exist('dno') then exit;
  with lNotation do
  begin
    DrawPass('Notations file is transfered into the local library');
    DoCTrans := false;
    GetNotation(MizFileName+'.dno');
{ TODO remove this after debugging, PutNotation will be in XML too}
{$IFDEF IMPDEBUG}
    PutNotation(MizFileName+'test1'+'.dno');
    ImpNotationMarkingObj(lNotation).PutNotation(MizFileName+'test2'+'.dno');
{$ENDIF}
    MarkDicts;
    Renumerate;
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);
    AllConstrs.Renumerate(false);
    DoCTrans := true;
    DoSTrans := true;
    SymbolTrans:= @fSymbolTrans; {TODO: quite risky, it will get disposed}
    PutMarked(@AllConstrs,LibrFileName('dno'));

    DoCTrans := false;
    DoSTrans := false;
    Done;
  end;
end;

procedure Clusters;
var lClusters: ImpClustersObj;
begin
  if not Exist('dcl') then exit;
  with lClusters do
  begin
    DrawPass('Cluster registrations file is transfered into the local library');
    DoCTrans := false;
    GetClusters(MizFileName+'.dcl');
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);
    AllConstrs.Renumerate(false);
    DoCTrans := true;
    PutMarked(@AllConstrs,LibrFileName('dcl'));
    Done;
  end;
end;

procedure Identify;
var lFuncIds: ImpIdentifyObj;
begin
  if not Exist('did') then exit;
  with lFuncIds do
  begin
    DrawPass('Identify registrations file is transfered into the local library');
    DoCTrans := false;
    GetIdentify(MizFileName+'.did');
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);
    AllConstrs.Renumerate(false);
    DoCTrans := true;
    PutMarked(@AllConstrs,LibrFileName('did'));
    Done;
  end;
end;

procedure Reductions;
var lFuncIds: ImpReductionObj;
begin
  if not Exist('drd') then exit;
  with lFuncIds do
  begin
    DrawPass('Reduction registrations file is transfered into the local library');
    DoCTrans := false;
    GetReductions(MizFileName+'.drd');
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);
    AllConstrs.Renumerate(false);
    DoCTrans := true;
    PutMarked(@AllConstrs,LibrFileName('drd'));
    Done;
  end;
end;

procedure Properties;
var lFuncIds: ImpPropertiesObj;
begin
  if not Exist('dpr') then exit;
  with lFuncIds do
  begin
    DrawPass('Properties registrations file is transfered into the local library');
    DoCTrans := false;
    GetProperties(MizFileName+'.dpr');
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);
    AllConstrs.Renumerate(false);
    DoCTrans := true;
    PutMarked(@AllConstrs,LibrFileName('dpr'));
    Done;
  end;
end;

{REPLACED}
procedure Definitions;
var lDefinientia: ImpDefinientiaObj;
begin
  if not Exist('def') then exit;
  with lDefinientia do
  begin
    DrawPass('Definitions file is transfered into the local library');
    DoCTrans := false;
    GetDefinientia(MizFileName+'.def');
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);;
    AllConstrs.Renumerate(false);
    DoCtrans := true;
    PutMarked(@AllConstrs,LibrFileName('def'));
    Done;
  end;
end;

{REPLACED}
procedure Theorems;
var lTheorems: ImpTheoremsObj;
begin
  if not Exist('the') then exit;
  with lTheorems do
  begin
    DrawPass('Theorems file is transfered into the local library');
    DoCTrans := false;
    GetTheorems(MizFileName+'.the');
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);
    AllConstrs.Renumerate(false);
    DoCtrans := true;
    PutMarked(@AllConstrs,LibrFileName('the'));
    Done;
  end;
end;

{REPLACED}
procedure Schemes;
var lSchemes: ImpSchemeObj;
begin
  if not Exist('sch') then exit;
  with lSchemes do
  begin
    DrawPass('Schemes file is transfered into the local library');
    DoCTrans := false;
    GetSchemes(MizFileName+'.sch');
    AllConstrs.InitMark;
    Mark(AllConstrs.fConstrCounts);
    AllConstrs.Renumerate(false);
    DoCtrans := true;
    PutMarked(@AllConstrs,LibrFileName('sch'));
    Done;
  end;
end;

procedure CreatePREL;
 var i:integer;
begin
 {$I-} mkdir('prel'); i:=ioresult;
  if PublicLibr then
   begin
     mkdir('prel'+DirSeparator+ArticleName[1]);
     i:=ioresult
   end;
 {$I+}
end;

{------------------------------------------------------}

procedure PRELTransfer;
begin
  CreatePREL;
  DoSTrans := false;
  DoCTrans := false;
  Formats;
  AllConstrs.GetACO;
  ProcessTyp;
  Signature;
  Clusters;
  Identify;
  Reductions;
  Properties;
  Definitions;
  Theorems;
  Schemes;
end;

end.
