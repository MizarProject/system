(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit edt_han;
interface

uses mizenv,mobjects,errhan;

procedure EdtPrintPos(var fEdt : text; const fPos : Position);
procedure EdtDeleteText(var fEdt : text; const fStart,fEnd : Position);
procedure EdtGotoPos(var fEdt : text; const fPos:Position);

type

 PPosition = ^PositionDes;
 PositionDes =
  object(MObject)
   Pos: Position;
   constructor Init(fPos:Position);
  end;

 PosCollectionPtr = ^PosCollection;
 PosCollection =
  object(MSortedCollection)
   function Compare(Key1,Key2:pointer): integer; virtual;
   function ThereIsPos(const fPos : Position): boolean; virtual;
  end;

 ErrDesPtr = ^ErrDes;
 ErrDes =
   object(PositionDes)
     Code: integer;
    constructor Init(fPos:Position; fCode:integer);
   end;

 PErrorsCollection =  ^ErrorsCollection;
 ErrorsCollection =
  object(PosCollection)
    CurError: ErrDesPtr;
    ErrNr: integer;
   procedure ResetErrors;
   constructor ReadErrors(const fName:string);
   function EofErrors: boolean;
   procedure GetError;
   procedure RewriteErrors;
   procedure AddError(fPos:Position; fCode:integer);
   procedure PrintErrors(const fName:string);
   constructor Init(ALimit, ADelta: Integer);
  end;

 PCommand = ^EDTCommand;
 EDTCommand =
  object(PositionDes)
    NextPos: Position;
    Command: char;
    Params: PString;
   constructor Init(fCommand:char; fPos:Position; const fStr:String);
   constructor Assign(fCommand:char; fPos,fNextPos:Position; const fStr:String);
   destructor Done; virtual;
  end;

 EDTCollection =
  object(PosCollection)
    CurPos: Position;
   constructor Init(ALimit, ADelta: Integer);
   procedure Insert_g(fPos:Position);
   procedure Insert_s(fPos:Position);
   procedure Insert_d(fBegPos,fEndPos:Position);
   procedure Insert_i(fString:string);
   procedure Insert_l(fString:string);
   procedure ReadCommand(var fEdtFile:text);
   constructor LoadEdt(const fName:string);
   procedure StoreEdt(const fName:string);
   procedure AppendEdtFile(const fName:string);
  end;

procedure EditFile(const SourceFileName,TargetFileName: string; Copying:boolean);

function StrPos(fPos:Position): string;

var
 gForceShortLines: boolean;

implementation

uses mscanner,scanner,mconsole;

constructor PositionDes.Init(fPos:Position);
begin
 Pos:=fPos;
end;

constructor ErrDes.Init(fPos:Position; fCode:integer);
begin
 Pos:=fPos; Code:=fCode;
end;

constructor ErrorsCollection.Init(ALimit, ADelta: Integer);
begin inherited Init(ALimit, ADelta);
 ErrNr:=0;
 CurError:=nil;
end;

procedure ErrorsCollection.GetError;
begin
 if ErrNr < Count then
  begin CurError:=Items^[ErrNr]; inc(ErrNr) end
 else CurError:=nil;
end;

procedure ErrorsCollection.ResetErrors;
begin
 ErrNr:=0; GetError;
end;

function ErrorsCollection.EofErrors;
begin
 EofErrors:=ErrNr>=Count;
end;

procedure ErrorsCollection.AddError(fPos:Position; fCode:integer);
begin
 Insert(New(ErrDesPtr,Init(fPos,fCode)));
end;

procedure ErrorsCollection.RewriteErrors;
begin
 Init(200,100);
end;

constructor ErrorsCollection.ReadErrors;
var  ErrorsFile: text; ErrorsBuff: array[1..$1000] of char;
     lName,lExt: string;
     lPos: Position; lCode: integer;
begin
 Init(200,100);
 lExt:=ExtractFileExt(fName);
 lName:=fName;
 if lExt='' then lName:=fName+'.err';
 assign(ErrorsFile,lName); settextbuf(ErrorsFile,ErrorsBuff);
 {$I-} reset(ErrorsFile); {$I+}
 if ioresult=0 then
  begin
    while not seekeof(ErrorsFile) do
     begin
       with lPos do readln(ErrorsFile,Line,Col,lCode);
       Insert(New(ErrDesPtr,Init(lPos,lCode)));
     end;
   close(ErrorsFile);
  end;
 GetError;
end;

procedure ErrorsCollection.PrintErrors(const fName:string);
var  Errors: text; ErrorsBuff: array[1..$1000] of char;
     lName,lExt: string;
     i: integer;
 procedure PrintError(Item:ErrDesPtr);
  begin
   with Item^ do writeln(Errors,StrPos(Pos),' ',Code);
  end;
begin
 lExt:=ExtractFileExt(fName);
 lName:=fName;
 if lExt='' then lName:=fName+'.err';
 assign(Errors,lName); settextbuf(Errors,ErrorsBuff);
 rewrite(Errors);
 for i:=0 to Count-1 do
  PrintError(Items^[i]);
 close(Errors);
end;

constructor EDTCommand.Init(fCommand:char; fPos:Position; const fStr:String);
begin
 Pos:=fPos;
 NextPos:=fPos;
 Command:=fCommand;
 Params:=nil;
 if fStr <> '' then Params:=NewStr(fStr);
end;

constructor EDTCommand.Assign(fCommand:char; fPos,fNextPos:Position; const fStr:String);
begin
 Init(fCommand,fPos,fStr);
 NextPos:=fNextPos;
end;

destructor EDTCommand.Done;
begin
 if Params <> nil then DisposeStr(Params);
end;

constructor EDTCollection.Init;
begin inherited Init(ALimit, ADelta);
 CurPos.Col:=1; CurPos.Line:=1;
end;

function StrPos(fPos:Position): string;
 var lLine,lCol:string;
begin
 with fPos do begin Str(Line,lLine); Str(Col,lCol) end;
 StrPos:=lLine+' '+lCol;
end;

procedure EDTCollection.Insert_g(fPos:Position);
 var i: integer; lPosDes: PositionDes;
begin lPosDes.Init(fPos);
 for i:=0 to Count-1 do
  if Compare(addr(lPosDes),Items^[i]) = 1 then
   with PCommand(Items^[i])^ do
    begin
     if (fPos.Line < NextPos.Line) or
      (fPos.Line = NextPos.Line) and (fPos.Col <= NextPos.Col) then
      case Command of 's','d': begin CurPos:=NextPos; exit end; end
    end
  else if (fPos.Line = PCommand(Items^[i])^.Pos.Line) and
          (fPos.Col = PCommand(Items^[i])^.Pos.Col) then
    begin CurPos:=fPos; exit end;
 CurPos:=fPos;
 Insert(new(PCommand,Init('g',fPos,'')));
end;

procedure EDTCollection.Insert_s(fPos:Position);
begin
 Insert(new(PCommand,Assign('s',CurPos,fPos,'')));
 CurPos:=fPos;
end;

procedure EDTCollection.Insert_d(fBegPos,fEndPos:Position);
begin
 CurPos:=fEndPos;
 inc(CurPos.Col);
 Insert(new(PCommand,Assign('d',fBegPos,fEndPos,'')));
end;

procedure EDTCollection.Insert_i(fString:string);
begin
 Insert(new(PCommand,Init('i',CurPos,fString)));
end;

procedure EDTCollection.Insert_l(fString:string);
begin
 Insert(new(PCommand,Init('l',CurPos,fString)));
end;

function PosCollection.Compare(Key1,Key2:pointer): integer;
begin Compare:=-1;
 if (PPosition(Key1)^.Pos.Line > PPosition(Key2)^.Pos.Line)
 or (PPosition(Key1)^.Pos.Line = PPosition(Key2)^.Pos.Line)
 and (PPosition(Key1)^.Pos.Col > PPosition(Key2)^.Pos.Col) then Compare:=1;
end;

function PosCollection.ThereIsPos(const fPos : Position): boolean;
var i : integer;
begin
 ThereIsPos := false;
 for i:=Count - 1 downto 0 do
    if (PPosition(At(i))^.Pos.Line = fPos.Line) and
       (PPosition(At(i))^.Pos.Col = fPos.Col) then
    begin
       ThereIsPos := true;
       Break;
    end;
end;
   
procedure EdtCollection.ReadCommand(var fEdtFile:text);
 var lCommand:char; lPos,lPos1,lPos2:Position; lString:string;
begin
 read(fEdtFile,lCommand);
 with lPos do
 case lCommand of
  'g': begin with lPos do readln(fEdtFile,Line,Col); Insert_g(lPos) end;
  's': begin with lPos do readln(fEdtFile,Line,Col); Insert_s(lPos) end;
  'd':
   begin
    with lPos1 do read(fEdtFile,Line,Col);
    with lPos2 do readln(fEdtFile,Line,Col);
    Insert_d(lPos1,lPos2)
   end;
  'i': begin readln(fEdtFile,lString); Insert_i(lString) end;
  'l': begin readln(fEdtFile,lString); Insert_i(lString) end;
 end;
end;

constructor EdtCollection.LoadEdt(const fName:string);
 var EdtFile:text; targetBuff:array[1..$1000] of char;
     lName,lExt: string;
begin
 lExt:=ExtractFileExt(fname);
 lName:=fName;
 if lExt='' then lName:=fName+'.edt';
 assign(EdtFile,lName);
 SetTextBuf(EdtFile,targetBuff);
 reset(EdtFile);
 Init(100,100);
 while not seekeof(EdtFile) do ReadCommand(EdtFile);
 close(EdtFile);
end;

procedure EdtCollection.StoreEDT(const fName:string);
 var EdtFile: text;
 procedure PrintCommand(fC:PCommand);
  begin
   with fC^ do
    begin
     write(EdtFile,Command);
     case Command of
      'g','s':  write(EdtFile,StrPos(NextPos));
      'd':  write(EdtFile,StrPos(Pos),' ',StrPos(NextPos));
     end;
     if Params <> nil then  write(EdtFile,Params^);
     writeln(EdtFile);
    end;
  end;
 var targetBuff: array[1..$1000] of char;
     lName,lExt: string;
     i:integer;
begin
 lExt:=ExtractFileExt(fname);
 lName:=fName;
 if lExt='' then lName:=fName+'.edt';
 assign(EdtFile,lName);
 SetTextBuf(EdtFile,targetBuff);
 rewrite(EdtFile);
 for I:=0 to Count-1 do PrintCommand(Items^[i]);
 close(EdtFile);
end;

procedure EdtCollection.AppendEdtFile(const fName:string);
 var EdtFile:text; targetBuff:array[1..$1000] of char;
     lName,lExt: string;
begin
 lExt:=ExtractFileExt(fname);
 lName:=fName;
 if lExt='' then lName:=fName+'.edt';
 assign(EdtFile,lName);
 SetTextBuf(EdtFile,targetBuff);
 reset(EdtFile);
 CurPos.Col:=0; CurPos.Line:=1;
 while not seekeof(EdtFile) do ReadCommand(EdtFile);
 StoreEdt(fName);
end;

type
 NamedBufferPtr = ^NamedBuffer;
 NamedBuffer = object(MStrObj)
   fBuffer: MStringList;
   constructor Init(aName: string);
   procedure Add(aStr: string);
   procedure Insert(var Target: text);
 end;

 BufferCollection = object(MSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    function SearchBuf(aBuf: string; var Index: Integer): Boolean; virtual;
 end;

function BufferCollection.Compare;
begin
 Result:=CompareStr(MStrPtr(Key1)^.fStr, MStrPtr(Key2)^.fStr);
end;

function BufferCollection.SearchBuf;
 var lStr: MStrPtr;
begin
 new(lStr, Init(aBuf));
 Result:=Search(lStr, Index);
 dispose(lStr, Done);
end;

constructor NamedBuffer.Init;
begin inherited Init(aName);
 fBuffer.Init(10);
end;

procedure NamedBuffer.Add;
begin
 fBuffer.AddString(aStr);
end;

procedure NamedBuffer.Insert;
 var i: integer;
begin
 for i:=0 to fBuffer.fCount-2 do
  writeln(Target, fBuffer.GetString(i));
 if fBuffer.fCount > 0 then
  write(Target, fBuffer.GetString(fBuffer.fCount-1));
end;

procedure EditFile(const SourceFileName,TargetFileName:string; Copying:boolean);
 var
  SourceBuff, TargetBuff: array[0..$3000] of char;
  SourceFile,TargetFile,PosTranspFile: text;
  CurPos,BegPosTransp,PosTransp: Position;
  SourceLine,TargetLine: string;
  InstrNr: integer;
  BugInInstructions: boolean;

 procedure BugInInstruction;
 begin BugInInstructions:=true;
  DrawMessage('Bug Instruction: '+IntToStr(InstrNr),'');
  halt(1);
 end;

 procedure GetLine;
 begin inc(CurPos.Line); CurPos.Col:=1;
  if eof(SourceFile) then begin BugInInstruction; exit end;
  readln(SourceFile,SourceLine)
 end;

 procedure PrintLine;
 begin writeln(TargetFile,TrimStringRight(TargetLine)); TargetLine:='' end;

 procedure CompleteLine;
  var lLine: string;
 begin
  if CurPos.Col <= length(SourceLine) then
   begin lLine:=Copy(SourceLine,CurPos.Col,length(SourceLine));
    if gForceShortLines then
     if length(TargetLine) + length(TrimStringRight(lLine)) >= MaxLineLength then
      PrintLine;
    TargetLine:=TargetLine+lLine;
   end;
  PrintLine;
 end;

 procedure ProcessLinesTo(fPos: Position);
  var lLine: string;
 begin
  if CurPos.Line < fPos.Line then
   begin
    CompleteLine;
    GetLine; if BugInInstructions then exit;
    while CurPos.Line < fPos.Line do
     begin TargetLine:=SourceLine; PrintLine;
      GetLine; if BugInInstructions then exit;
     end;
   end;
  if fPos.Col < CurPos.Col then begin BugInInstruction; exit end;
  if CurPos.Col <= length(SourceLine) then
   begin lLine:=Copy(SourceLine,CurPos.Col,fPos.Col-CurPos.Col);
    if gForceShortLines then
     if length(TargetLine) + length(lLine) >= MaxLineLength
      then PrintLine;
    TargetLine:=TargetLine+lLine;
   end;
  CurPos.Col:=fPos.Col;
  BegPosTransp:=CurPos; PosTransp.Line:=0; PosTransp.Col:=0;
 end;

 procedure SkipLinesTo(fPos: Position);
 begin
  while CurPos.Line < fPos.Line do
   begin GetLine; if BugInInstructions then exit end;
  if fPos.Col < CurPos.Col then begin BugInInstruction; exit end;
  CurPos.Col:=fPos.Col;
 end;

 var Buffers: BufferCollection;

 procedure TakeBufferTo(aBuf: string; fPos: Position; aCut: boolean);
  var lLine: string; lBuf: NamedBufferPtr;
 begin
  new(lBuf, Init(aBuf));
  Buffers.Insert(lBuf);
  if CurPos.Line < fPos.Line then
   begin
    lBuf^.Add(Copy(SourceLine,CurPos.Col,length(SourceLine)));
    if not aCut then CompleteLine;
    GetLine; if BugInInstructions then exit;
    while CurPos.Line < fPos.Line do
     begin lBuf^.Add(SourceLine);
      if not aCut then begin TargetLine:=SourceLine; PrintLine; end;
      GetLine;
      if BugInInstructions then exit;
     end;
   end;
  if fPos.Col < CurPos.Col then begin BugInInstruction; exit end;
  if CurPos.Col <= length(SourceLine)+1 then
   begin lLine:=Copy(SourceLine,CurPos.Col,fPos.Col-CurPos.Col);
    lBuf^.Add(lLine);
    if not aCut then
     begin
      if gForceShortLines then
       if length(TargetLine) + length(lLine) >= MaxLineLength
        then PrintLine;
      TargetLine:=TargetLine+lLine;
     end;
   end;
  CurPos.Col:=fPos.Col;
  BegPosTransp:=CurPos; PosTransp.Line:=0; PosTransp.Col:=0;
 end;

 var InsertedStr: string;

 procedure AddString;
 begin
  if gForceShortLines then
   if length(TargetLine)+length(InsertedStr) >= MaxLineLength
    then PrintLine;
  TargetLine:=TargetLine+InsertedStr;
 end;

 procedure InsertBuffer(aBuf: string);
  var i,j: integer; lBuf: NamedBufferPtr;
 begin
  if Buffers.SearchBuf(aBuf, i) then
   begin
    lBuf:=Buffers.At(i);
    if lBuf^.fBuffer.fCount > 0 then
     begin
      InsertedStr:=lBuf^.fBuffer.GetString(0);
      AddString;
     end;
    for j:=1 to lBuf^.fBuffer.fCount-1 do
     begin
      PrintLine;
      InsertedStr:=lBuf^.fBuffer.GetString(j);
      AddString;
     end;
   end;
 end;

 var EdtInstrFileName: string;

 procedure GetEdtInstrName;
 begin
   EdtInstrFileName:=ChangeFileExt(SourceFileName,'.edt');
 end;

 var PosTranspFileName: string;

 procedure GetPosTranspName;
 begin
   PosTranspFileName:=ChangeFileExt(TargetFileName,'.ptr');
 end;

 var
  InstrFile: text; InstrBuff: array[1..$1000] of char;
  InstrKind,DummyChar: char;
  FirstPos,LastPos,NewPos: Position;
  Count,I: integer;
  BufName: string;

 label Koniec;

begin
 Buffers.Init(0,10);
 GetEdtInstrName;
 assign(InstrFile,EdtInstrFileName); reset(InstrFile);
 settextbuf(InstrFile,InstrBuff);
 if not Copying and seekeof(InstrFile) then begin close(InstrFile); exit end;
 assign(SourceFile,SourceFileName); settextbuf(SourceFile,SourceBuff);
 reset(SourceFile); CurPos.Line:=0;
 InstrNr:=0;
 BugInInstructions:=false;
 GetLine;
 assign(TargetFile,TargetFileName); settextbuf(TargetFile,TargetBuff);
 rewrite(TargetFile);
 GetPosTranspName;
 assign(PosTranspFile,PosTranspFileName); rewrite(PosTranspFile);
 TargetLine:='';
 while not SeekEof(InstrFile) do
  begin
   {$I-} read(InstrFile,InstrKind); {$I+}
   if ioresult<>0 then begin BugInInstruction; goto Koniec end;
   inc(InstrNr);
   case InstrKind of
    'g':
     begin {$I-} with NewPos do readln(InstrFile,Line,Col); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      if NewPos.Line < CurPos.Line
       then begin BugInInstruction; goto Koniec end;
      ProcessLinesTo(NewPos);
      if BugInInstructions then goto Koniec;
     end;
    'G':
     begin {$I-} readln(InstrFile,Count); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      if Count<=0 then begin BugInInstruction; goto Koniec end;
      NewPos:=CurPos; with NewPos do begin inc(Line,Count); Col:=1 end;
      ProcessLinesTo(NewPos);
      if BugInInstructions then goto Koniec;
     end;
    'i':
     begin {$I-} readln(InstrFile,InsertedStr); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      AddString;
     end;
    'l':
     begin {$I-} readln(InstrFile,InsertedStr); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      AddString;
      PrintLine;
     end;
    'L':
     begin {$I-} readln(InstrFile,Count); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      if Count<=0 then begin BugInInstruction; goto Koniec end;
      PrintLine;
      for I:=1 to Count-1 do writeln(TargetLine);
     end;
    'D':
     begin {$I-} readln(InstrFile,Count); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      if Count<=0 then begin BugInInstruction; goto Koniec end;
      NewPos:=CurPos;
      with NewPos do begin inc(Line,Count); Col:=1 end;
      SkipLinesTo(NewPos);
      if BugInInstructions then goto Koniec;
     end;
    's':
     begin {$I-} with NewPos do readln(InstrFile,Line,Col); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      if NewPos.Line < CurPos.Line
       then begin BugInInstruction; goto Koniec end;
      SkipLinesTo(NewPos);
      if BugInInstructions then goto Koniec;
     end;
    'd':
     begin {$I-}
      with FirstPos do read(InstrFile,Line,Col,DummyChar);
      with LastPos do begin readln(InstrFile,Line,Col,BufName); inc(Col) end;
      {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      if FirstPos.Line < CurPos.Line
       then begin BugInInstruction; goto Koniec end;
      ProcessLinesTo(FirstPos);
      if BugInInstructions then goto Koniec;
      if LastPos.Line < CurPos.Line
       then begin BugInInstruction; goto Koniec end;
      while (BufName <> '') and (BufName[1] = ' ') do delete(BufName, 1, 1);
      if BufName <> '' then TakeBufferTo(BufName, LastPos, true)
      else SkipLinesTo(LastPos);
      if BugInInstructions then goto Koniec;
     end;
    'b': {pobierz do bufora}
     begin {$I-}
      with FirstPos do read(InstrFile,Line,Col,DummyChar);
      with LastPos do begin readln(InstrFile,Line,Col,DummyChar,BufName); inc(Col) end;
      {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      if FirstPos.Line < CurPos.Line
       then begin BugInInstruction; goto Koniec end;
      ProcessLinesTo(FirstPos);
      if BugInInstructions then goto Koniec;
      if LastPos.Line < CurPos.Line
       then begin BugInInstruction; goto Koniec end;
      TakeBufferTo(BufName, LastPos, false);
      if BugInInstructions then goto Koniec;
     end;
    'x':
     begin {$I-} readln(InstrFile, BufName); {$I+}
      if ioresult<>0 then begin BugInInstruction; goto Koniec end;
      InsertBuffer(BufName);
     end;
    else begin readln(InstrFile); BugInInstruction; goto Koniec end;
   end;
  end;
Koniec:
 if CurPos.Line > 0 then CompleteLine;
 while not eof(SourceFile) do
  begin GetLine; writeln(TargetFile,SourceLine) end;
 close(InstrFile);
 close(SourceFile);
 close(TargetFile);
 close(PosTranspFile);
end;

procedure EdtPrintPos(var fEdt : text; const fPos:Position);
begin
   Write(fEdt, fPos.Line,' ',fPos.Col,' ')
end;

procedure EdtDeleteText(var fEdt : text; const fStart,fEnd:Position);
begin
   Write(fEdt,'d');
   EdtPrintPos(fEdt,fStart);
   EdtPrintPos(fEdt,fEnd);
   Writeln(fEdt);
end;

procedure EdtGotoPos(var fEdt : text; const fPos:Position);
begin
   Write(fEdt, 'g');
   EdtPrintPos(fEdt, fPos);
   Writeln(fEdt);
end;

begin
 gForceShortLines:=True;
end.
