(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit librenv;

interface

uses mobjects;

const
   MML = 'mml';
   EnvMizFiles = 'MIZFILES';

var MizPath, MizFiles: string;

function LibraryPath(fName,fExt: string): string;

procedure GetSortedNames(fParam:byte; var fList:MStringCollection);
procedure GetNames(fParam:byte; var fList: StringColl);

procedure ReadSortedNames(fName:string; var fList:MStringCollection);
procedure ReadNames(fName:string; var fList: StringColl);

type
  PFileDescr = ^FileDescr;
  FileDescr = object(MObject)
    nName: PString;
    Time: LongInt;
    constructor Init(fIdent:string; fTime:LongInt);
    destructor Done; virtual;
   end;

  PFileDescrCollection = ^FileDescrCollection;
  FileDescrCollection = object(MSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure StoreFIL(fName:string);
    constructor LoadFIL(fName:string);
    procedure InsertTimes;
   end;

var LocFilesCollection: FileDescrCollection;

implementation

uses
{$IFDEF WIN32}
 windows,
{$ENDIF}
 mizenv,pcmizver,mconsole;

constructor FileDescr.Init(fIdent:string; fTime:LongInt);
begin
 nName:=NewStr(fIdent); Time:=fTime;
end;

destructor FileDescr.Done;
begin
 DisposeStr(nName);
end;

function FileDescrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
 if PFileDescr(Key1)^.nName^ < PFileDescr(Key2)^.nName^ then Compare:=-1
 else if PFileDescr(Key1)^.nName^ = PFileDescr(Key2)^.nName^ then Compare:=0
 else Compare:=1;
end;

procedure FileDescrCollection.InsertTimes;
 var z: integer;
begin
 for z:=0 to Count-1 do
  with PFileDescr(Items^[z])^ do Time:=GetFileTime(nName^);
end;

function LibraryPath(fName,fExt: string): string;
begin
 LibraryPath:='';
 if MFileExists('prel'+DirSeparator+fName+fExt) then
  begin
   LocFilesCollection.Insert(New(PFileDescr,Init('prel'+DirSeparator+fName+fExt,0)));
   LibraryPath:='prel'+DirSeparator+fName+fExt; exit
  end;
 if MFileExists(MizFiles+'prel'+DirSeparator+fName[1]+DirSeparator+fName+fExt) then
  LibraryPath:=MizFiles+'prel'+DirSeparator+fName[1]+DirSeparator+fName+fExt;
end;

constructor FileDescrCollection.LoadFIL(fName:string);
 var FIL: text; lName: string; lTime: longint;
begin
 assign(FIL,fName); reset(FIL);
 Init(0,10);
 while not eof(FIL) do
  begin Readln(FIL,lName);
   Readln(FIL,lTime);
   Insert(new(PFileDescr,Init(lName,lTime)));
  end;
 close(FIL);
end;

procedure FileDescrCollection.StoreFIL(fName:string);
 var FIL: text; i: integer;
begin
 EraseFile(fName);
 assign(FIL,fName); rewrite(FIL);
 InsertTimes;
 for i:=0 to Count-1 do
  with PFileDescr(Items^[i])^ do
  begin writeln(FIL,nName^); writeln(FIL,Time) end;
 Close(FIL);
end;

procedure ReadSortedNames(fName:string; var fList:MStringCollection);
 var NamesFile: text;
begin
 if fName[1]='@' then
  begin
   Delete(fName,1,1);
   FileExam(fName);
   assign(NamesFile,fName);
   reset(NamesFile);
   fList.Init(100,100);
   while not seekEof(NamesFile) do
    begin
     readln(NamesFile,fName);
     fList.Insert(NewStr(fName));
    end;
   exit;
  end;
 fList.Init(2,10);
 fList.Insert(NewStr(fName));
end;

procedure ReadNames(fName:string; var fList: StringColl);
 var NamesFile: text;
begin
 if fName[1]='@' then
  begin
   Delete(fName,1,1);
   FileExam(fName);
   assign(NamesFile,fName);
   reset(NamesFile);
   fList.Init(10,10);
   while not seekEof(NamesFile) do
    begin
     readln(NamesFile,fName);
     fList.Insert(NewStr(fName));
    end;
   exit;
  end;
 fList.Init(2,10);
 fList.Insert(NewStr(fName));
end;

procedure GetSortedNames(fParam:byte; var fList:MStringCollection);
 var FileName:string;
     NamesFile: text;
     i: integer;
begin
 if ParamCount < fParam then  begin fList.Init(0,0); exit end;
 FileName:=ParamStr(fParam);
 if FileName[1]='@' then
  begin
   Delete(FileName,1,1);
   FileExam(FileName);
   assign(NamesFile,FileName);
   reset(NamesFile);
   fList.Init(10,10);
   while not seekEof(NamesFile) do
    begin
     readln(NamesFile,FileName);
     fList.Insert(NewStr(TrimString(FileName)));
    end;
   exit;
  end;
 fList.Init(2,8);
 fList.Insert(NewStr(FileName));
 for i:=fParam+1 to ParamCount do
  begin
   FileName:=paramstr(i);
   fList.Insert(NewStr(FileName));
  end;
end;

procedure GetNames(fParam:byte; var fList: StringColl);
 var FileName:string;
     NamesFile: text;
     i: integer;
begin
 if ParamCount < fParam then  begin fList.Init(0,0); exit end;
 FileName:=ParamStr(fParam);
 if FileName[1]='@' then
  begin
   Delete(FileName,1,1);
   FileExam(FileName);
   assign(NamesFile,FileName);
   reset(NamesFile);
   fList.Init(10,10);
   while not seekEof(NamesFile) do
    begin
     readln(NamesFile,FileName);
     fList.Insert(NewStr(TrimString(FileName)));
    end;
   exit;
  end;
 fList.Init(2,8);
 fList.Insert(NewStr(FileName));
 for i:=fParam+1 to ParamCount do
  begin
   FileName:=paramstr(i);
   fList.Insert(NewStr(FileName));
  end;
end;

procedure CheckCompatibility;
 var lFile: text;
     lLine,lVer1,lVer2,l: string;
     lPos,lCode: integer;
     lMizarReleaseNbr,lMizarVersionNbr,lMizarVariantNbr: integer;
begin
 FileExam(MizFiles+MML+'.ini');
 assign(lFile,MizFiles+MML+'.ini');
 reset(lFile);
 lMizarReleaseNbr:=-1;
 lMizarVersionNbr:=-1;
 lMizarVariantNbr:=-1;
 while not seekeof(lFile) do
 begin
  readln(lFile,lLine);
  lPos:=Pos('MizarReleaseNbr=',lLine);
  if lPos > 0 then
   begin
    delete(lLine,1,lPos+15);
    Val(lLIne,lMizarReleaseNbr,lCode);
   end;
  lPos:=Pos('MizarVersionNbr=',lLine);
  if lPos > 0 then
   begin
    delete(lLine,1,lPos+15);
    Val(lLIne,lMizarVersionNbr,lCode);
   end;
  lPos:=Pos('MizarVariantNbr=',lLine);
  if lPos > 0 then
   begin
    delete(lLine,1,lPos+15);
    Val(lLIne,lMizarVariantNbr,lCode);
   end;
 end;
 close(lFile);
 if (lMizarReleaseNbr=PCMizarReleaseNbr) and
    (lMizarVersionNbr=PCMizarVersionNbr) then
   begin
//    if PCMizarVariantNbr > lMizarVariantNbr then
//    begin DrawMessage('Incompatible version of the Mizar System and the MML','');
//     halt(1);
//    end;
   end
  else
   begin
    str(PCMizarReleaseNbr,l); lVer1:=l;
    str(PCMizarVersionNbr,l); lVer1:=lVer1+'.'+l;
    str(PCMizarVariantNbr,l); lVer1:=lVer1+'.'+l;
    str(lMizarReleaseNbr,l); lVer2:=l;
    str(lMizarVersionNbr,l); lVer2:=lVer2+'.'+l;
    str(lMizarVariantNbr,l); lVer2:=lVer2+'.'+l;
    DrawMessage('Mizar System ver. '+lVer1+' is incompatible with the MML version imported ('+lVer2+')','Please check '+MizFiles+'mml.ini');
    halt(1);
   end;
end;

procedure InitLibrEnv;
begin
 LocFilesCollection.Init(0,20);
 MizPath:=ExtractFileDir(ParamStr(0));
 MizFiles:=GetEnvStr(EnvMizFiles);
 if MizFiles='' then MizFiles:=MizPath;
 if MizFiles<>'' then
    if MizFiles[length(MizFiles)]<>DirSeparator then MizFiles:=MizFiles+DirSeparator;
 if MizFiles='' then Mizfiles:=DirSeparator;
 MizFileName:=''; EnvFileName:=''; ArticleName:=''; ArticleExt:='';
end;

begin
 InitLibrEnv;
 CheckCompatibility;
end.
