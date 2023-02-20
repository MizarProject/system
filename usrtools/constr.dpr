(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program FindTheConstr;

uses mobjects,pcmizver,mizenv,monitor,errhan,correl,inout,inlibr,limits,
     envhan,ref_han,prochan,enums,iocorrel,
     inoutmml,impobjs,schemhan,
     {$IFDEF MDEBUG} info,outinfo, {$ENDIF}
     librenv,mconsole;

var
  gClusters:  ImpClustersObj;
  gTheorems:  ImpTheoremsObj;
  gSchemes:  ImpSchemeObj;
  gConstr: MSortedStrList;
  gSpec: boolean;
  gTheo: LibrReference;

var AllConstrs : ImpMultConstrObj ;


procedure PrintTooBigNumber(fThe:PLibrReference);
begin
 writeln;
 write('Too big number in ');
 case fThe^.Kind of
  lrTh, lrDef: write('theorem ');
  lrExreg..lrCondreg: write('cluster ');
 end;
 writeln(fThe^.fStr,':',TheoremNr(fThe^));
end;

procedure PrintCanceledTheorem(fThe:PLibrReference);
begin
 writeln;
 writeln('Theorem ',fThe^.fStr,':',TheoremNr(fThe^),' is canceled');
end;

procedure PrintAllConstructors;
 var i,j,k,defnr,thnr,enr,fnr,cnr:integer;
  gRefGlobal, gAllGlobal: MStringCollection;
  tkind:char;
  iKind: ClusterKind;
  add:string;
  function ClStr(clk : ClusterKind):string;
  begin
   case clk of
   clRegistered	 : begin inc(enr); ClStr:= 'exreg '+IntToStr(enr); end; 
   clFunctor	 : begin inc(fnr); ClStr:= 'funcreg '+IntToStr(fnr); end; 
   clConditional : begin inc(cnr); ClStr:= 'condreg '+IntToStr(cnr); end; 
   end; 
  end; 
  procedure PrintConstrs(refs:boolean);
  var i:integer; 
  begin 
    if MizFileName <> ''
    then writeln(' (with respect to ', MizFileName, ')')
    else writeln;

{ we omit HIDDEN}
    with AllConstrs do
      for i := 1 to fImpSgn.Count-1 do
        if fSgnMarks.HasInDom(i) then
	 if (gConstr.Count = 0) or
	     (gConstr.IndexOfStr(ImpConstrPtr(fImpSgn.Items^[i])^.fStr)<0)
	 then with ImpConstrPtr(fImpSgn.Items^[i])^ do
	 begin	 	 
	   writeln(fStr);
	   if refs then 
	     gRefGlobal.Insert(NewStr(fStr));
	   gAllGlobal.Insert(NewStr(fStr));
	 end; 
  end; { PrintConstrs }

begin
 gRefGlobal.Init(10,10);
 gAllGlobal.Init(10,10);
 if gTheo.Kind in [lrAll, lrTh, lrDef] then with gTheorems,fTheorems do
 begin
    defnr:= 0; thnr := 0;
   if gTheo.Kind = lrDef then tKind:= 'D' else tKind:= 'T';
   for j:=0 to Count-1 do
   with TheoremPtr(Items^[j])^ do
    if ((gTheo.Kind = lrAll) or (fTheoKind = tKind)) then
      begin
       if fTheoKind = 'D' then
       begin add:= 'def '; inc(defnr); k:= defnr; end
       else begin add:=''; inc(thnr); k:= thnr; end; 
       if TheoremPtr(Items^[j])^.fTheorem^.FrmSort='%' then continue;
       AllConstrs.InitMark;
       AllConstrs.fConstrCounts.MarkFrm(fTheorem);
       AllConstrs.Mark1DCO;
       writeln;       
       write  ('Constructors for ',UpperCase(ArticleName),':',add,k);
       PrintConstrs(true);
      end;
   writeln('    ==================================================');
 end;

 if gTheo.Kind in [lrAll, lrSch] then
 with gSchemes,fSchemes do
  begin
   for j:=0 to Count-1 do
     with  SchemePtr(Items^[j])^ do
      begin
       AllConstrs.InitMark;
       AllConstrs.fConstrCounts.MarkTypColl(fSchTypes);
       AllConstrs.fConstrCounts.MarkFrmColl(fSchProps);
       AllConstrs.Mark1DCO;
       writeln;
       write  ('Constructors for ',UpperCase(ArticleName),':sch ', j+1);
       PrintConstrs(true);  
      end;
   writeln('    ==================================================');
  end;

  if gTheo.Kind in [lrAll, lrExreg,lrFuncreg,lrCondreg] then
  begin
  enr:= 0; fnr:= 0; cnr:= 0; 
  if gTheo.Kind = lrExreg then iKind := clRegistered
       else if gTheo.Kind = lrFuncreg then iKind := clFunctor
         else iKind := clConditional;

  with gClusters,fClusters do
  for j:=0 to Count-1 do with ClusterPtr(Items^[j])^ do
   if ((gTheo.Kind = lrAll) or (nClusterKind = iKind)) then
    begin
     AllConstrs.InitMark;
     AllConstrs.fConstrCounts.MarkClusterObj(Items^[j]);
     AllConstrs.Mark1DCO;
     writeln;
     write  ('Constructors for ',UpperCase(ArticleName),':',clstr(nClusterKind));
     PrintConstrs(false);
    end; 
   writeln('    ===================================================');
  end;
 
writeln('    ==== Constructors for all theorems and schemes ====');
 for i:=0 to gRefGlobal.Count-1 do
  writeln(PString(gRefGlobal.At(i))^);
writeln('    ===================================================');
writeln('    ============ Constructors for all items ===========');
 for i:=0 to gAllGlobal.Count-1 do
  writeln(PString(gAllGlobal.At(i))^);
end; { PrintAllConstructors }
  
  
  

procedure PrintConstructors(fThe:PLibrReference);
var i,j:integer;
    tKind:char;
    iKind:ClusterKind;
begin
 j:= 0; 
 with fThe^,AllConstrs do
 begin
  AllConstrs.InitMark;
  case Kind of
   lrSch: with gSchemes,fSchemes do
    begin
     if Nr > Count then begin PrintTooBigNumber(fThe); exit end;
     with SchemePtr(Items^[Nr-1])^ do 
      begin
       fConstrCounts.MarkTypColl(fSchTypes);
       fConstrCounts.MarkFrmColl(fSchProps);
      end;
    end;

   lrDef,lrTh: with gTheorems,fTheorems do
    begin
     if Kind = lrDef then tKind:= 'D' else tKind:= 'T';
     for i:= 0 to Count-1 do
      with TheoremPtr(Items^[i])^ do
       if fTheoKind = tKind then
       begin
        inc(j);
	if j = Nr then break;
       end; 
     if (j = Nr) then
     begin 
       if TheoremPtr(Items^[i])^.fTheorem^.FrmSort='%'
       then begin PrintCanceledTheorem(fThe); exit end;       
       fConstrCounts.MarkFrm(TheoremPtr(Items^[i])^.fTheorem);
     end
     else begin PrintTooBigNumber(fThe); exit end;
    end;   

   lrExreg,lrFuncreg,lrCondreg:
   with gClusters,fClusters do
    begin
     if Kind = lrExreg then iKind := clRegistered
       else if Kind = lrFuncreg then iKind := clFunctor
         else iKind := clConditional;

     for i:= 0 to Count-1 do
      with ClusterPtr(Items^[i])^ do
       if nClusterKind = iKind then
       begin
        inc(j);
	if j = Nr then break;
       end; 
       
     if (j = Nr) then fConstrCounts.MarkClusterObj(Items^[i])
       else begin PrintTooBigNumber(fThe); exit end;
    end;   
     
  end;
  AllConstrs.Mark1DCO;
  writeln;
  write  ('Constructors for ',UpperCase(fThe^.fStr),':',TheoremNr(fThe^));
  if MizFileName <> '' then writeln(' (with respect to ', MizFileName, ')')
  else writeln;
  { decoded meaning of the following: gConstr contains article constrs
  only if we want to print "relatively"; in that case, we omit them }
  { we omit HIDDEN}
  with AllConstrs do
  for i := 1 to fImpSgn.Count-1 do
   if fSgnMarks.HasInDom(i) then
    if (gConstr.Count = 0) or
       (gConstr.IndexOfStr(ImpConstrPtr(fImpSgn.Items^[i])^.fStr)<0)
    then
      writeln(ImpConstrPtr(fImpSgn.Items^[i])^.fStr);
 end;
end;

procedure ReadSGL;
 var lImpConstrNbr,i: integer; lName:string;  lInFile: MizInStream;
begin
 if MizFileName = '' then begin gConstr.Init(0); exit end;
 FileExam(MizFileName+'.sgl');
 lInFile.OpenFile(MizFileName+'.sgl');
 lInFile.InInt(lImpConstrNbr);
 gConstr.Init(lImpConstrNbr);
 for i:=1 to lImpConstrNbr do
  begin lInFile.InString(lName);
   gConstr.Insert(new(MStrPtr,Init(lName)));
  end;
 lInFile.Done;
end;

procedure BadParameters(fMsg: string);
begin
 Noise;
 if fMsg <> '' then begin writeln; writeln(fMsg); writeln end;
 writeln('Syntax:  constr [-f CheckingArticleFileName] <LibrRef>');
 writeln('      where <LibrRef> is ArticleName[:[th|def|sch|exreg|funcreg|condreg] number]');
 writeln;
 halt(2);
end;

procedure InitParams;
 var lPath: string; i,l,Code: integer;
     P: string;
begin
 if ParamCount = 0 then BadParameters('');
 P:=paramstr(1);
 l:=1;
 MizFileName:='';
 if pos('-f',P) = 1 then
  begin Delete(P,1,2);
   inc(l);
   if P = '' then
    begin
     if ParamCount < l then BadParameters('');
     P:=ParamStr(l);
     inc(l);
    end;
{   UpperCase(P); -kartoteki pozostaja bez zmian}
   MizFileName:=P;
   ArticleExt:=ExtractFileExt(P);
   if ArticleExt = '' then ArticleExt:='.miz'
   else MizFileName:=ChangeFileExt(P,'');
   EnvFileName:=MizFileName;
   if ParamCount < l then BadParameters('');
  end;
 lPath:='';
 gSpec:=true;
 for i:=l to ParamCount do lPath:=lPath+ParamStr(i);
 l:=pos(':',lPath);
 if l = 0 then {BadParameters('')}
  begin l:=length(lPath)+1; lPath:=lPath+':0'; gSpec:=false;
  end;
 ArticleName:=LowerCase(copy(lPath,1,l-1));
 gTheo.fStr:=ArticleName;
 delete(lPath,1,l);
 gTheo.Kind:=lrAll;
 if pos('def',lPath) = 1 then
  begin
   gTheo.Kind:=lrDef;
   delete(lPath,1,3);
  end
 else
 if pos('sch',lPath) = 1 then
  begin
   gTheo.Kind:=lrSch;
   delete(lPath,1,3);
  end
 else
 if pos('th',lPath) = 1 then
  begin
   gTheo.Kind:=lrTh;
   delete(lPath,1,2);
  end
 else
 if pos('exreg',lPath) = 1 then
  begin
   gTheo.Kind:=lrExreg;
   delete(lPath,1,5);
  end
 else
 if pos('funcreg',lPath) = 1 then
  begin
   gTheo.Kind:=lrFuncreg;
   delete(lPath,1,7);
  end
 else
 if pos('condreg',lPath) = 1 then
  begin
   gTheo.Kind:=lrCondreg;
   delete(lPath,1,7);
  end
 else
  gTheo.Kind:=lrTh;
 val(lPath,l, Code);
 if (Code <> 0) or (l < 0) then
  if (lPath = '') or (lPath = ' ') then gSpec:=false
  else BadParameters('');
 if l >255 then BadParameters('Too big number');
 gTheo.Nr:=l;
end;

var errortab: array[0..2] of boolean;

begin
  DrawMizarScreen('Required Constructors Directives');
  InitParams;
  InitExitProc;
  {$IFDEF MDEBUG} OpenInfoFile; {$ENDIF}
  {InitInFileBuff;}
  OpenErrors('$err');  
  DoCTrans := true;
{  InitSignatures;
  InitConstrNbr;}
  assign(gLogFile,'constr.log');
  rewrite(gLogFile);
  case gTheo.Kind of
   lrExreg, lrFuncreg, lrCondreg:
   begin 
   AllConstrs.LoadDCO(LibraryPath(LowerCase(ArticleName),'.dcl'));
   gClusters.GetClusters(LibraryPath(LowerCase(ArticleName),'.dcl'));
   end; 
   lrTh, lrDef:
   begin 
   AllConstrs.LoadDCO(LibraryPath(LowerCase(ArticleName),'.the'));
   gTheorems.GetTheorems(LibraryPath(LowerCase(ArticleName),'.the'));
   end; 
   lrSch:
   begin 
   AllConstrs.LoadDCO(LibraryPath(LowerCase(ArticleName),'.sch'));
   gSchemes.GetSchemes(LibraryPath(LowerCase(ArticleName),'.sch'));
   end;
   {lrAll was unimplemented, removed}
  end;
  errortab[0]:=false;
  with gClusters do
    if fClusters.Count  = 0
    then errortab[0]:=true;
  errortab[1]:=false;
  with gTheorems do
    if  fTheorems.Count = 0
    then errortab[1]:=true;
  errortab[2]:=false;
  with gSchemes do
    if  fSchemes.Count = 0
    then errortab[2]:=true;
  if errortab[1] and (gTheo.Kind in [lrTh, lrDef]) then
    begin
     writeln('Can''t find ',ArticleName+'.the,');
     Noise; exit;
    end;
  if errortab[2] and (gTheo.Kind = lrSch) then
    begin
     writeln('Can''t find ',ArticleName+'.sch,');
     Noise; exit;
    end;
  if errortab[0] and (gTheo.Kind in [lrExreg, lrFuncreg, lrCondreg]) then
    begin
     writeln('Can''t find ',ArticleName+'.dcl');
     Noise; exit;
    end;
  if (gTheo.Nr = 0) and errortab[0] and errortab[1] and errortab[2] then
   begin
    writeln('Can''t find any ',ArticleName+'.* file,');
    Noise; exit;
   end;
  ReadSGL;
{    if MizFileName <> '' then InConstrNum;}
  if gTheo.Nr <> 0 then PrintConstructors(@gTheo)
  else PrintAllConstructors;
  close(gLogFile);
  erase(gLogFile);
  EraseErrors;
  FinishDrawing;
end.
