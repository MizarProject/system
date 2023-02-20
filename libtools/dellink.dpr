(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
uses mconsole,mizenv,monitor,errhan,mscanner,syntax,parser;

type
 LabelItemPtr = ^LabelItemObj;
 LabelItemObj = object(ItemObj)
  constructor Init(fKind:ItemKind);
  procedure Pop; virtual;
  procedure ProcessLabel; virtual;
  procedure ProcessDefiniensLabel; virtual;
  procedure ProcessPrivateReference; virtual;
  procedure StartSimpleJustification; virtual;
 end;

 LabelBlockPtr = ^LabelBlockObj;
 LabelBlockObj = object(BlockObj)
  constructor Init(fBlockKind:BlockKind);
  procedure Pop; virtual;
  procedure ProcessLink; virtual;
  procedure CreateItem(fItemKind:ItemKind); virtual;
  procedure CreateBlock(fBlockKind:BlockKind); virtual;
 end;

 LabLevel = record
  Lab: String;
  Lev: Integer;
  isDef: Boolean;
 end;

var
 edt :text;
 arrLab : Array[1..10000] of LabLevel;
 indexLab,indexThen,curLevel : Integer;
 iDelLink,iDelLinkHence : Boolean;

procedure InitArticle;
begin
 gBlockPtr:=New(LabelBlockPtr, Init(blMain));
end;

constructor LabelItemObj.Init;
begin inherited Init(fKind);
end;

procedure LabelItemObj.Pop;
begin
 inherited Pop;
end;

procedure LabelItemObj.ProcessLabel;
begin
 inc(indexLab);
 if AheadWord.Kind=sy_Colon then
 begin
  arrLab[indexLab].Lab:=CurWord.Spelling;
  arrLab[indexLab].Lev:=curLevel;
  arrLab[indexLab].isDef:=false;
  writeln(edt,'d',CurPos.Line,' ',
          CurPos.Col-length(CurWord.Spelling)+1,' ',AheadPos.line,' ',
          AheadPos.Col);
  writeln(edt,'i','R',indexLab,':');
 end else
  if CurWord.Kind=sy_Hereby then
  begin
   arrLab[indexLab].Lab:='0';
   arrLab[indexLab].Lev:=curLevel;
   arrLab[indexLab].isDef:=false;
   writeln(edt,'d',CurPos.Line,' ',
           CurPos.Col-length(CurWord.Spelling)+1,' ',CurPos.Line,' ',
           CurPos.Col);
   writeln(edt,'i','thus R',indexLab,': now');
  end else
  begin
   arrLab[indexLab].Lab:='0';
   arrLab[indexLab].Lev:=curLevel;
   arrLab[indexLab].isDef:=false;
   writeln(edt,'g',CurPos.Line,' ',
           CurPos.Col-length(CurWord.Spelling)+1);
   if (CurPos.Line=PrevPos.Line) and
      (CurPos.Col-length(CurWord.Spelling)=PrevPos.Col)
    then
    if iDelLink and not iDelLinkHence
     then writeln(edt,'i','R',indexLab,': ')
     else writeln(edt,'i',' R',indexLab,': ')
    else writeln(edt,'i','R',indexLab,': ');
  end;
end;

procedure LabelItemObj.ProcessDefiniensLabel;
begin
 inc(indexLab);
 if AheadWord.Kind=sy_Colon then
 begin
  arrLab[indexLab].Lab:=CurWord.Spelling;
  arrLab[indexLab].Lev:=curLevel-1;
  arrLab[indexLab].isDef:=true;
  writeln(edt,'d',CurPos.Line,' ',
          CurPos.Col-length(CurWord.Spelling)+1,' ',AheadPos.line,' ',
          AheadPos.Col);
  writeln(edt,'i','R',indexLab,':');
 end else
 begin
  arrLab[indexLab].Lab:='0';
  arrLab[indexLab].Lev:=curLevel-1;
  arrLab[indexLab].isDef:=true;
  writeln(edt,'g',CurPos.Line,' ',
          CurPos.Col-length(CurWord.Spelling)+1);
  writeln(edt,'i',':R',indexLab,': ');
 end;
end;

procedure LabelItemObj.ProcessPrivateReference;
 var i,minLevel: Integer;
begin
 minLevel:=curLevel;
 for i:= indexLab downto 1 do
 begin
  if (arrLab[i].Lev < minLevel) and not arrLab[i].isDef then
   minLevel:=arrLab[i].Lev;
  if (arrLab[i].Lev+1 < minLevel) and arrLab[i].isDef then
   minLevel:=arrLab[i].Lev+1;
  if (CurWord.Spelling=arrLab[i].Lab) and
     (minLevel>=arrLab[i].Lev) then
  begin
   writeln(edt,'d',CurPos.Line,' ',
           CurPos.Col-length(CurWord.Spelling)+1,' ',CurPos.Line,' ',
           CurPos.Col);
   writeln(edt,'i','R',i);
   break;
  end;
 end;
end;

procedure LabelItemObj.StartSimpleJustification;
begin
 if iDelLink then
 begin
  if CurWord.Kind=sy_By then
  begin
   writeln(edt,'g',AheadPos.Line,' ',
           AheadPos.Col-length(AheadWord.Spelling)+1);
   writeln(edt,'i','R',indexThen,',');
  end;
  if CurWord.Kind=sy_Semicolon then
  begin
   writeln(edt,'g',CurPos.Line,' ',CurPos.Col);
   writeln(edt,'i',' by R',indexThen);
  end;
  if CurWord.Kind=sy_DotEquals then
  begin
   writeln(edt,'g',PrevPos.Line,' ',PrevPos.Col+1);
   writeln(edt,'i',' by R',indexThen);
  end;
 end;
 iDelLink:=False;
 iDelLinkHence:=False;
end;

constructor LabelBlockObj.Init;
begin
 inherited Init(fBlockKind);
 if nBlockKind=blMain then
 begin
  AssignFile(edt,MizFileName+'.edt');
  Rewrite(edt);
 end;
 inc(curLevel);
end;

procedure LabelBlockObj.Pop;
begin
 dec(curLevel);
 if (nBlockKind=blCase) or (nBlockKind=blSuppose) then
  dec(arrLab[indexLab].Lev);
 if nBlockKind=blMain then CloseFile(edt);
 inherited Pop;
end;

procedure LabelBlockObj.ProcessLink;
 var i: Integer;
begin
 if CurWord.Kind=sy_Then then
 begin
  if CurPos.Line<>PrevPos.Line then
   writeln(edt,'d',CurPos.Line,' ',
           CurPos.Col-length(CurWord.Spelling)+1,' ',AheadPos.Line,
           ' ',AheadPos.Col-length(AheadWord.Spelling)) else
   writeln(edt,'d',PrevPos.Line,' ',PrevPos.Col+1,' ',CurPos.Line,' ',
           CurPos.Col);
  iDelLink:=True;
  for i:=indexLab downto 1 do
   if curLevel>=arrLab[i].Lev then
   begin
    indexThen:=i;
    break;
   end;
 end;
 if CurWord.Kind=sy_Hence then
 begin
  writeln(edt,'d',CurPos.Line,' ',
          CurPos.Col-length(CurWord.Spelling)+1,' ',CurPos.Line,' ',
          CurPos.Col);
  writeln(edt,'i','thus');
  iDelLink:=True;
  iDelLinkHence:=True;
  for i:=indexLab downto 1 do
   if curLevel>=arrLab[i].Lev then
   begin
    indexThen:=i;
    break;
   end;
 end;
end;

procedure LabelBlockObj.CreateItem;
begin
 gItemPtr:=New(LabelItemPtr, Init(fItemKind));
end;

procedure LabelBlockObj.CreateBlock;
begin
 gBlockPtr:=New(LabelBlockPtr,Init(fBlockKind));
end;

begin
 indexLab:=0;
 iDelLink:=False;
 iDelLinkHence:=False;
 curLevel:=0;
 DrawMizarScreen('Delinker');
 if paramcount<1 then EmptyParameterList;
 InitExitProc;
 GetArticleName;
 GetEnvironName;
 GetOptions;
 FileExam(MizFileName+ArticleExt);
 OpenErrors(MizFileName);
 InitArticle;
 InitDisplayLine('Parsing');
 FileExam(EnvFileName+'.dct');
 InitScanning(MizFileName+ArticleExt,EnvFileName);
 Parse;
 FinishScanning;
 FinishDrawing;
end.
