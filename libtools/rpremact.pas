(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit rpremact;
interface

uses edt_han;

procedure RpremactInitArticle;

var
 gEdt: EdtCollection;
 IrrPremErrors: ErrorsCollection;

implementation

uses mconsole,mobjects,errhan,mizenv,monitor,syntax,inout,mscanner
     {$IFDEF MDEBUG} ,info {$ENDIF};

type

 WRefDes = ^RefDes;
 RefDes =
   object(MObject)
    Prev,Curr,Ahead: Position;
    IrrPremise:boolean;
    nWord:Token;
    constructor Init(fWord:Token; fPrev,fCurr,fAhead:Position);
   end;

 RPremItemPtr = ^RPremItemObj;
 RPremItemObj =
  object(ItemObj)
   constructor Init(fKind:ItemKind);
   procedure Pop; virtual;
   procedure StartReferences; virtual;
   procedure ProcessPrivateReference; virtual;
   procedure StartLibraryReferences; virtual;
   procedure ProcessDef; virtual;
   procedure ProcessTheoremNumber; virtual;
   procedure FinishTheLibraryReferences; virtual;
   procedure FinishReferences; virtual;
   procedure StartSimpleJustification; virtual;
   procedure FinishSimpleJustification; virtual;
  end;

 RPremBlockPtr = ^RPremBlockObj;
 RPremBlockObj =
  object(BlockObj)
   constructor Init(fBlockKind:BlockKind);
   procedure Pop; virtual;
   procedure ProcessLink; virtual;
   procedure CreateItem(fItemKind:ItemKind); virtual;
   procedure CreateBlock(fBlockKind:BlockKind); virtual;
  end;

var
 LibrRef: WRefDes;
 LibrRefNbr,LibrIrrRefNbr,PremNbr,IrrPremNbr:integer;
 gLinked:boolean;
 LinkPos: record
	     Prev,Curr,Ahead: Position;
	     end;
 JustDes: MCollection;


constructor RefDes.Init;
begin
 nWord:=fWord;
 IrrPremise:=false;
 Prev:=fPrev; Curr:=fCurr; Ahead:=fAhead;
end;

procedure RpremactInitArticle;
begin
 gBlockPtr:=new(RPremBlockPtr, Init(blMain));
end;


constructor RPremItemObj.Init;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Init(fKind);
end;

procedure RPremItemObj.StartReferences;
begin
end;

procedure RPremItemObj.ProcessDef;
begin
 if (CurWord.Kind = ReferenceSort) and (CurWord.Nr = ord(syDef)) then
  JustDes.Insert(new(WRefDes,Init(CurWord,PrevPos,CurPos,AheadPos)));
end;

procedure RPremItemObj.ProcessTheoremNumber;
 var lWRefDes:WRefDes;
begin
 inc(PremNbr);
 inc(LibrRefNbr);
 lWRefDes:=new(WRefDes,Init(CurWord,PrevPos,CurPos,AheadPos));
 with IrrPremErrors do if CurError<>nil then
  if (CurPos.Line = CurError^.Pos.Line) and (CurPos.Col = CurError^.Pos.Col) then
   begin
    if CurError^.Code <> 602 then
     begin DrawMessage('Wrong error number,','Error #602 expected');
      Halt(1);
     end;
    lWRefDes^.IrrPremise:=true;
    if (WRefDes(JustDes.Items^[JustDes.Count-1])^.nWord.Kind = ReferenceSort) and 
    (WRefDes(JustDes.Items^[JustDes.Count-1])^.nWord.Nr = ord(syDef)) then
      WRefDes(JustDes.Items^[JustDes.Count-1])^.IrrPremise:=true;
    inc(IrrPremNbr);
    inc(LibrIrrRefNbr);
    GetError;
   end;
 JustDes.Insert(lWRefDes);
end;

procedure RPremItemObj.StartLibraryReferences;
begin
 LibrRef:=new(WRefDes,Init(CurWord,PrevPos,CurPos,AheadPos));
 JustDes.Insert(LibrRef);
 LibrRefNbr:=0;
 LibrIrrRefNbr:=0;
end;

procedure RPremItemObj.FinishTheLibraryReferences;
begin
  if LibrRefNbr=LibrIrrRefNbr then LibrRef^.IrrPremise:=true;
end;

procedure RPremItemObj.ProcessPrivateReference;
 var lWRefDes:WRefDes;
begin
 lWRefDes:=new(WRefDes,Init(CurWord,PrevPos,CurPos,AheadPos));
 inc(PremNbr);
 with IrrPremErrors do if CurError<>nil then
  if (CurPos.Line = CurError^.Pos.Line) and (CurPos.Col = CurError^.Pos.Col) then
   begin
    if CurError^.Code <> 602 then
     begin DrawMessage('Wrong error number','Error #602 expected');
      Halt(1);
     end;
    lWRefDes^.IrrPremise:=true;
    inc(IrrPremNbr);
    GetError;
   end;
 JustDes.Insert(lWRefDes);
end;

procedure RPremItemObj.FinishReferences;
begin
end;

procedure RPremItemObj.StartSimpleJustification;
 var lJustPos,lPos: Position;
begin
 PremNbr:=0; IrrPremNbr:=0;
 JustDes.Init(10,10);
 if CurWord.Kind = sy_By then lJustPos:=CurPos else lJustPos:=PrevPos;
 with IrrPremErrors do if CurError<>nil then
  if (lJustPos.Line = CurError^.Pos.Line) and
     (lJustPos.Col = CurError^.Pos.Col) then
   begin
    if (CurError^.Code <> 603) or not gLinked then
     begin DrawMessage('Wrong error numer','Linkage expected');
      gEDT.StoreEDT(MizFileName);
      gEdt.Done;
      Halt(1);
     end;
    with LinkPos do
    if nItemKind = itConclusion then
     begin lPos:=Curr; dec(lPos.Col,4);
      gEdt.Insert_d(lPos,Curr);
      gEdt.Insert_i('thus');
     end
    else if Curr.Line < Ahead.Line then
     begin lPos:=Prev; inc(lPos.Col);
      gEdt.Insert_d(lPos,Curr);
     end
    else
     begin lPos:=Curr; dec(lPos.Col,3);
      gEdt.Insert_d(lPos,Ahead);
     end;
    GetError;
   end;
 if CurWord.Kind = sy_By then
   JustDes.Insert(new(WRefDes,Init(CurWord,PrevPos,CurPos,AheadPos)));
end;

procedure RPremItemObj.FinishSimpleJustification;
 var i,lIrr,lBegRefNr,lRefNr:integer; lBegPos,lEndPos: Position;
begin
 JustDes.Insert(new(WRefDes,Init(CurWord,PrevPos,CurPos,AheadPos)));
 if IrrPremNbr > 0 then
 begin
    lIrr:=0;
    for i:=0 to JustDes.Count-1 do if WRefDes(JustDes.Items^[i])^.IrrPremise then inc(lIrr); 
   if PremNbr = IrrPremNbr then
    begin
      with WRefDes(JustDes.Items^[0])^.Prev do
       begin lBegPos.Line:=Line; lBegPos.Col:=Col+1 end;
      if CurWord.Kind = sy_Semicolon then
        with CurPos do begin lEndPos.Line:=Line; lEndPos.Col:=Col-1 end
       else lEndPos:=PrevPos;
      gEdt.Insert_d(lBegPos,lEndPos);
    end
   else
    begin lBegRefNr:=1;
     repeat
       while (lBegRefNr<JustDes.Count-1) and
             (not WRefDes(JustDes.Items^[lBegRefNr])^.IrrPremise)
         do inc(lBegRefNr);
	lRefNr:=lBegRefNr+1; dec(lIrr);
       while (lRefNr<JustDes.Count) and
             (WRefDes(JustDes.Items^[lRefNr])^.IrrPremise)
         do begin inc(lRefNr); dec(lIrr) end;
       if lBegRefNr = 1 then
        begin
         with WRefDes(JustDes.Items^[1])^,Curr do
          begin lBegPos.Line:=Line;
	     lBegPos.Col:=Col-length(nWord.Spelling)+1;
          end;
         lEndPos:=WRefDes(JustDes.Items^[lRefNr])^.Prev;
         gEdt.Insert_d(lBegPos,lEndPos);
        end
       else if lRefNr >= JustDes.Count-1 then
        begin
         lBegPos:=WRefDes(JustDes.Items^[lBegRefNr])^.Prev;
         if CurWord.Kind = sy_Semicolon then
           with CurPos do begin lEndPos.Line:=Line; lEndPos.Col:=Col-1 end
         else lEndPos:=PrevPos;
         gEdt.Insert_d(lBegPos,lEndPos);
        end
      else
       begin
         with WRefDes(JustDes.Items^[lBegRefNr])^,Prev do
          begin lBegPos.Line:=Line; lBegPos.Col:=Col+1 end;
         lEndPos:=WRefDes(JustDes.Items^[lRefNr])^.Prev;
        gEdt.Insert_d(lBegPos,lEndPos);
       end;
      lBegRefNr:=lRefNr;
     until lIrr = 0;
    end
  end;
 JustDes.Done;
end;

procedure RPremItemObj.Pop;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Pop;
end;

constructor RPremBlockObj.Init;
begin
 DisplayLine(CurPos.Line,ErrorNbr);
 inherited Init(fBlockKind);
 gLinked:=false;
end;

procedure RPremBlockObj.Pop;
begin
 inherited Pop;
 DisplayLine(CurPos.Line,ErrorNbr);
end;

procedure RPremBlockObj.CreateItem;
begin gItemPtr:=new(RPremItemPtr, Init(fItemKind)) end;

procedure RPremBlockObj.CreateBlock;
begin gBlockPtr:=new(RPremBlockPtr,Init(fBlockKind)) end;

procedure RPremBlockObj.ProcessLink;
begin
 gLinked:=(CurWord.Kind = sy_Then) or (CurWord.Kind = sy_Hence);
 with LinkPos do
  begin Prev:=PrevPos; Curr:=CurPos;
     Ahead:=AheadPos; dec(Ahead.Col,length(AheadWord.Spelling));
  end;
end;

end.

