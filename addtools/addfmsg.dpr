(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program AddFMsg;

uses mizenv,errhan,mconsole;

const
  MaxErrNbr   = 3000;
  MaxError    = 9999;
  MaxColumn   =  80;

var
  Source, Messages, Errors: text;
  SourceBuff, MessagesBuff: array[0..$4000] of char;
  EndOfMessage: boolean;
  ErrorsBuff: array[0..$1000] of char;
  Occurs: array[1..MaxError] of boolean;
  Line, Col, Code, ErrNbr, ErrCode, i: integer;
  Buffer:   string;

procedure GetErrCode;
  var Err,FirstDig,Count,Nbr: integer;
  label 1,2;
begin
  for FirstDig:=2 to length(Buffer) do
    if Buffer[FirstDig]<>' ' then goto 1;
  FirstDig:=length(Buffer)+1;
1:
  for Count:=FirstDig to length(Buffer) do
    if not (Buffer[Count] in ['0'..'9']) then goto 2;
  Count:=length(Buffer)+1;
2:
  val(copy(Buffer,FirstDig,Count-FirstDig),Nbr,Err);
  if (Err=0) and (Nbr>ErrCode) then ErrCode:=Nbr else inc(ErrCode);
end;

procedure ReadMsg;
begin if eof(Messages) then begin Buffer:=''; EndOfMessage:=true; exit end;
  readln(Messages,Buffer);
  EndOfMessage := (length(Buffer)<>0) and (Buffer[1]='#');
end;

label EndOfErrors;

begin { Main }

 if paramcount <> 2 then
  begin
    Noise;
    DrawMizarScreen('Appending Errors Explanations');
    writeln('Syntax:  addfmsg ArticleName ErrorsExplanationFileName');
    FinishDrawing;
    Halt(1);
  end;

 GetMizFileName('.miz');

 ErrNbr:=0;
 for i:=1 to MaxError do Occurs[i]:=false;
 assign(Errors,MizFileName+'.err'); settextbuf(Errors,ErrorsBuff);
 {$I-} reset(Errors); {$I+}
 if ioresult<>0
   then begin writeln(^G+'Can''t open ',MizFileName+'.err'); halt(1) end;
 while not seekeof(Errors) do
  begin if ErrNbr >= MaxErrNbr then goto EndOfErrors;
   inc(ErrNbr);
   readln(Errors,Line,Col,Code);
   if (Code > 0) and (Code <= MaxError) then Occurs[Code]:=true;
  end;
EndOfErrors:
 close(Errors);
 if ErrNbr = 0 then halt;
 assign(Source,MizFileName+ArticleExt);
 settextbuf(Source,SourceBuff);
 {$I-} append(Source); {$I+}
 if ioresult<>0
  then begin writeln(^G+'Can''t open ',MizFileName,ArticleExt); halt(1); end;

 assign(Messages,paramstr(2)+'.msg');
 settextbuf(Messages,MessagesBuff);
 {$I-} reset(Messages);{$I+}
 if ioresult<>0
  then begin writeln(^G+'Can''t open ',paramstr(2)+'.msg'); halt(1) end;
 repeat ReadMsg until EndOfMessage;
 readln(Messages);
 ReadMsg;
 if not EndOfMessage then ReadMsg;
 ErrCode:=0;
 GetErrCode;
 Buffer:=' ';
 writeln(Source,'::>');
 ReadMsg;
 for i:=1 to MaxError do
  if Occurs[i] then
   begin
    while ErrCode < i do
     begin while not EndOfMessage do ReadMsg; GetErrCode; ReadMsg end;
    if i=ErrCode then
     begin
      if EndOfMessage then writeln(Source,'::> ',ErrCode:1,':   ?')
      else
       begin writeln(Source,'::> ',ErrCode:1,': ',Buffer);
        ReadMsg;
        while not EndOfMessage do
         begin writeln(Source,'::> ',Buffer); ReadMsg end;
       end;
      GetErrCode;
      ReadMsg;
     end
    else writeln(Source,'::> ',i:1,':   ???');
   end;
 close(Source);
end.
