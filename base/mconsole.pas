(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit mconsole;

interface

procedure InitDisplayLine(const aComment:string);
procedure NoDisplayLine(fLine,fErrNbr: integer);

const DisplayLine: procedure(fLine,fErrNbr: integer) = NoDisplayLine;

procedure DrawMizarScreen(const aApplicationName:string);
procedure DrawArticleName(const fName:string);

procedure DrawStr(const aStr:string);
procedure FinishDrawing;

procedure EmptyParameterList;
procedure Noise;
procedure DrawPass(const aName: string);
procedure DrawTime(const aTime: string);
procedure DrawVerifierExit(const aTime: string);

procedure DrawMessage(const Msg1,Msg2:string);
procedure BugInProcessor;
procedure DrawIOResult(const FileName:string; I:byte);
procedure DrawErrorsMsg(aErrorNbr: integer);

procedure DisplayLineInCurPos(fLine,fErrNbr: integer);

const
  ErrMsg: array[1..6] of string[20] =
       ('',
        'File not found',
        'Path not found',
        'Too many open files',
        'Disk read error',
        'Disk write error'
       );

{Accommodator specific options:}

var
 InsertHiddenFiles,
 VocabulariesProcessing,FormatsProcessing,NotationsProcessing,
 SignatureProcessing,
 ConstructorsProcessing,
 ClustersProcessing,IdentifyProcessing,ReductionProcessing,PropertiesProcessing,
 DefinitionsProcessing,EqualitiesProcessing,ExpansionsProcessing,
 TheoremsProcessing,SchemesProcessing,TheoremListsProcessing,SchemeListsProcessing:boolean;

procedure InitAccOptions;
procedure GetAccOptions;

{MakeEnv specific options:}

var Accomodation : boolean = false;
    NewAccom : boolean = false;

procedure GetMEOptions;

{Transfer specific options:}

var PublicLibr: boolean;

procedure GetTransfOptions;


{Other options: }

var
 CtrlCPressed : boolean = false;
 LongLines : boolean = false;
 QuietMode : boolean = false;
 StopOnError: boolean = false;

 FinishingPass: boolean = false;
 ParserOnly : boolean = false;
 AnalyzerOnly : boolean = false;
 CheckerOnly : boolean = false;
 SwitchOffUnifier: boolean = false;

 AxiomsAllowed: boolean = false;

procedure GetOptions;

implementation

uses pcmizver,mizenv;

procedure InitAccOptions;
begin
 InsertHiddenFiles:=true;
 VocabulariesProcessing:=true;
 FormatsProcessing:=true;
 NotationsProcessing:=true;
 SignatureProcessing:=true;
 ConstructorsProcessing:=true;
 ClustersProcessing:=true;
 IdentifyProcessing:=true;
 ReductionProcessing:=true;
 PropertiesProcessing:=true;
 DefinitionsProcessing:=true;
 EqualitiesProcessing:=true;
 ExpansionsProcessing:=true;
 TheoremsProcessing:=true;
 SchemesProcessing:=true;
 TheoremListsProcessing:=false;
 SchemeListsProcessing:=false;
end;

procedure ResetAccOptions;
begin
 InsertHiddenFiles:=true;
 VocabulariesProcessing:=false;
 FormatsProcessing:=false;
 NotationsProcessing:=false;
 SignatureProcessing:=false;
 ConstructorsProcessing:=false;
 ClustersProcessing:=false;
 IdentifyProcessing:=false;
 ReductionProcessing:=false;
 PropertiesProcessing:=false;
 DefinitionsProcessing:=false;
 EqualitiesProcessing:=false;
 ExpansionsProcessing:=false;
 TheoremsProcessing:=false;
 SchemesProcessing:=false;
 TheoremListsProcessing:=false;
 SchemeListsProcessing:=false;
end;

procedure GetAccOptions;
 var i,j: integer;
begin
 InitAccOptions;
 for j:=1 to ParamCount do
  if ParamStr(j)[1]='-' then
   for i:=2 to Length(ParamStr(j)) do
    case ParamStr(j)[i] of
     'v': begin ResetAccOptions; VocabulariesProcessing:=true end;
     'f','p':
      begin
       ResetAccOptions;
       VocabulariesProcessing:=true;
       FormatsProcessing:=true;
      end;
     'P':
      begin
       ResetAccOptions;
       VocabulariesProcessing:=true;
       FormatsProcessing:=true;
       TheoremListsProcessing:=true;
       SchemeListsProcessing:=true;
      end;
     'e':
      begin
       ResetAccOptions;
       VocabulariesProcessing:=true;
       FormatsProcessing:=true;
       ConstructorsProcessing:=true;
       SignatureProcessing:=true;
       ClustersProcessing:=true;
       NotationsProcessing:=true;
      end;
     'h': begin InsertHiddenFiles:=false; end;
     'l': LongLines:=true;
     'q': QuietMode:=true;
     's': StopOnError:=true;
    end;
end;

procedure GetMEOptions;
 var i,j: integer;
begin
 for j:=1 to ParamCount do
  if ParamStr(j)[1]='-' then
   for i:=2 to Length(ParamStr(j)) do
    case ParamStr(j)[i] of
     'n': NewAccom:=true;
     'a': Accomodation:=true;
     'l': LongLines:=true;
     'q': QuietMode:=true;
     's': StopOnError:=true;
    end;
end;

procedure GetOptions;
 var i,j: integer;
begin
 for j:=1 to ParamCount do
  if ParamStr(j)[1]='-' then
   for i:=2 to Length(ParamStr(j)) do
    case ParamStr(j)[i] of
     'q': QuietMode:=true;
     'p': ParserOnly:=true;
     'a': AnalyzerOnly:=true;
     'c': CheckerOnly:=true;
     'l': LongLines:=true;
     's': StopOnError:=true;
     'u': SwitchOffUnifier:=true;
     'x': AxiomsAllowed:=true;
      else break;
    end;
 if ArticleExt = '.axm' then
   AxiomsAllowed:=true;
end;

procedure GetTransfOptions;
 var lOption: string;
begin
 PublicLibr:=false;
 if ParamCount>=2 then
  begin lOption:=paramstr(2);
   if (length(lOption) = 2) and (lOption[1] in ['/','-']) then
    PublicLibr:=upcase(lOption[2]) = 'P';
  end
end;

{+-------------------------------------------------------------------+}

var gComment: string = '';

{$I-}

procedure NoDisplayLine(fLine,fErrNbr: integer);
begin
end;

procedure InitDisplayLine(const aComment:string);
begin
 gComment:=aComment;
 writeln;
 write(aComment);
 DisplayLine:=DisplayLineInCurPos
end;

procedure DrawStr(const aStr:string);
begin write(aStr) end;

procedure FinishDrawing;
begin writeln; end;

procedure DrawTPass(const fPassName:string);
begin write(fPassName) end;

procedure DrawMizarScreen(const aApplicationName:string);
begin
 writeln(aApplicationName,', ',PCMizarVersionStr,' (',PlatformNameStr,')');
 writeln(Copyright);
end;

procedure Noise;
begin
 {$IFNDEF WIN32}
 write(^G^G^G);
 {$ENDIF}
end;

procedure EmptyParameterList;
begin
 Noise;
 writeln; writeln('****  Empty Parameter List ? ****');
 halt(2);
end;

procedure DrawArticleName(const fName:string);
begin
 writeln('Processing: ',fName);
end;

procedure DrawPass(const aName: string);
begin
 writeln;
 write(aName);
end;

procedure DrawTime(const aTime: string);
begin
 write(aTime);
end;

procedure DrawVerifierExit(const aTime: string);
begin
 writeln;
 writeln('Time of mizaring:',aTime);
end;

procedure DisplayLineInCurPos(fLine,fErrNbr: integer);
begin
 if (not CtrlCPressed) and (not QuietMode)then
  begin
   write(^M+gComment+' [',fLine:4);
   if fErrNbr>0 then write(' *',fErrNbr);  
   write(']' );
  end; 
 if FinishingPass then
  begin
   write(' [',fLine:4);
   if fErrNbr>0 then write(' *',fErrNbr);  
   write(']' );
  end; 
end;

procedure DrawMessage(const Msg1,Msg2:string);
 var Lh: byte;
begin
  Noise;
  writeln;
  write('**** ',Msg1);
  Lh:=length(Msg1);
  if length(Msg2)>Lh then Lh:=length(Msg2);
  if Lh > length(Msg1) then write(' ':Lh-length(Msg1));
  writeln(' ****');
  if Msg2<>'' then
   begin write('**** ',Msg2);
     if Lh > length(Msg2) then write(' ':Lh-length(Msg2));
     writeln(' ****');
   end;
end;

procedure BugInProcessor;
begin
  DrawMessage('Internal Error','');
end;

procedure DrawIOResult(const FileName:string; I:byte);
begin
 if I in [2..6,12,100] then
   begin
     if I=12 then I:=7 else if I=100 then I:=8;
     DrawMessage(ErrMsg[I],'Can''t open '' '+FileName+' ''')
   end
  else DrawMessage('Can''t open '' '+FileName+' ''','');
 halt(1);
end;

procedure DrawErrorsMSg(aErrorNbr: integer);
begin
  if aErrorNbr > 0 then
  begin
    writeln;
    if aErrorNbr = 1 then
      writeln('**** 1 error detected')
    else
      writeln('**** ', aErrorNbr, ' errors detected');
  end;
end;

end.

