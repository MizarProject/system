(* Unit tests for negation handling soundness in kernel/schemes.pas. *)
program test_schemes_soundness;

uses
  mobjects, errhan, mizenv, mconsole, builtin, lexicon, enums, correl, schemhan, schemes;

var
  gLastErrNr: integer = 0;
  gPassed: integer = 0;
  gFailed: integer = 0;

procedure CaptureError(Pos: Position; ErrNr: integer);
begin
  gLastErrNr := ErrNr;
end;

function MkObjTyp: TypPtr;
begin
  Result := NewStandardTyp(ikTypMode, NewEmptyCluster, NewEmptyCluster, 1, nil);
end;

function MkVar(aNr: integer): TrmPtr;
begin
  Result := TrmPtr(NewVarTrm(ikTrmBound, aNr));
end;

function MkEqXX(aVarNr: integer): FrmPtr;
begin
  Result := NewPredFrm(ikFrmPred, gBuiltIn[rqEqualsTo],
    NewTrmList(MkVar(aVarNr), NewTrmList(MkVar(aVarNr), nil)), 0);
end;

function MkSchP(aPredNr, aVarNr: integer): FrmPtr;
begin
  Result := NewPredFrm(ikFrmSchPred, aPredNr,
    NewTrmList(MkVar(aVarNr), NewTrmList(MkVar(aVarNr), nil)), 0);
end;

function MkPriv(aPredNr, aVarNr: integer; aExp: FrmPtr): FrmPtr;
begin
  Result := NewLocPredFrm(aPredNr, NewTrmList(MkVar(aVarNr), nil), aExp);
end;

function MkForall(aScope: FrmPtr): FrmPtr;
begin
  Result := NewUniv(MkObjTyp, aScope);
end;

function MkExists(aBody: FrmPtr): FrmPtr;
begin
  Result := NewNeg(MkForall(NewNeg(aBody)));
end;

procedure Check(const aName: string;
                const aSchProps, aInstProps: array of FrmPtr;
                aExpectOk: boolean; aExpectErr: integer);
var
  lTypes, lProps: MCollection;
  lSch: SchRefItem;
  lSnts: MList;
  i: integer;
  lOk: boolean;
begin
  lTypes.Init(0, 4);
  lProps.Init(Length(aSchProps), 4);
  for i := 0 to High(aSchProps) do lProps.Insert(aSchProps[i]);
  lSch.Init(0, 1, lTypes, lProps, schStateQuotable);

  lSnts.Init(Length(aInstProps));
  for i := 0 to High(aInstProps) do lSnts.Insert(aInstProps[i]);

  ErrorNbr := 0;
  gLastErrNr := 0;
  CurPos.Line := 1;
  CurPos.Col := 1;
  Schematize(lSch, lSnts);

  if aExpectOk then
    lOk := (ErrorNbr = 0)
  else if aExpectErr > 0 then
    lOk := (ErrorNbr > 0) and (gLastErrNr = aExpectErr)
  else
    lOk := (ErrorNbr > 0);

  if lOk then
  begin
    inc(gPassed);
    writeln('PASS: ', aName);
  end
  else
  begin
    inc(gFailed);
    writeln('FAIL: ', aName, ' (ErrorNbr=', ErrorNbr, ', LastErrNr=', gLastErrNr, ')');
  end;

  lSnts.Done;
  lSch.SchTypes.Done;
  lSch.SchProps.Done;
end;

procedure InitHidden;
var
  lMode: ConstrTypPtr;
begin
  StopOnError := false;
  PutError := @CaptureError;
  gTermCollection.Init(40, 40);
  InitConstructors;

  lMode := new(ConstrTypPtr, Init(coMode, 1, 'HIDDEN'));
  lMode^.fConstrTyp := MkObjTyp;
  Constr[coMode].Insert(lMode);
  gBuiltIn[rqAny] := 1;

  Constr[coPredicate].Insert(new(ConstrPtr, Init(coPredicate, 1, 'HIDDEN')));
  gBuiltIn[rqEqualsTo] := 1;
end;

begin
  InitHidden;

  (* Soundness bug 1: first bind of scheme predicate under negation must store -PredNr. *)
  Check('reject (ex x st x<>x) from (ex x st x=x) via negated first bind',
    [ MkExists(MkSchP(1, 1)), MkExists(MkSchP(1, 1)) ],
    [ MkExists(NewNeg(MkEqXX(1))), MkExists(MkEqXX(1)) ],
    false, 24);

  Check('reject (for x holds x<>x) from (for x holds x=x) via negated first bind',
    [ MkForall(MkSchP(1, 1)), MkForall(MkSchP(1, 1)) ],
    [ MkForall(NewNeg(MkEqXX(1))), MkForall(MkEqXX(1)) ],
    false, 24);

  (* Soundness bug 2: unfolding private predicates must preserve stripped negation. *)
  Check('reject (for x holds not Q[x]) from (for x holds x=x) when Q[x] <=> x=x',
    [ MkForall(MkEqXX(1)) ],
    [ MkForall(NewNeg(MkPriv(1, 1, MkEqXX(1)))) ],
    false, 0);

  Check('reject (ex x st not Q[x]) from (ex x st x=x) when Q[x] <=> x=x',
    [ MkExists(MkEqXX(1)) ],
    [ MkExists(NewNeg(MkPriv(1, 1, MkEqXX(1)))) ],
    false, 0);

  Check('reject chained private pred with wrong polarity (not Q1, Q1<=>Q2<=>x=x)',
    [ MkForall(MkEqXX(1)) ],
    [ MkForall(NewNeg(MkPriv(1, 1, MkPriv(2, 1, MkEqXX(1))))) ],
    false, 0);

  Check('reject alternating private pred with wrong polarity (not Q1, Q1<=>not Q2, Q2<=>x<>x)',
    [ MkForall(MkEqXX(1)) ],
    [ MkForall(NewNeg(MkPriv(1, 1, NewNeg(MkPriv(2, 1, NewNeg(MkEqXX(1))))))) ],
    false, 0);

  Check('reject negated scheme matching positive chained private pred (x<>x vs Q1<=>Q2<=>x=x)',
    [ MkForall(NewNeg(MkEqXX(1))) ],
    [ MkForall(MkPriv(1, 1, MkPriv(2, 1, MkEqXX(1)))) ],
    false, 0);

  (* Valid instantiations must succeed. *)
  Check('accept consistent positive bind (P := =)',
    [ MkExists(MkSchP(1, 1)), MkExists(MkSchP(1, 1)) ],
    [ MkExists(MkEqXX(1)), MkExists(MkEqXX(1)) ],
    true, 0);

  Check('accept consistent negated bind (P := <>)',
    [ MkForall(NewNeg(MkSchP(1, 1))), MkForall(NewNeg(MkSchP(1, 1))) ],
    [ MkForall(MkEqXX(1)), MkForall(MkEqXX(1)) ],
    true, 0);

  Check('accept mixed-polarity negated bind (not (P & not P) with P := <>)',
    [ MkForall(NewNeg(NewConj(MkSchP(1, 1), NewNeg(MkSchP(1, 1))))) ],
    [ MkForall(NewNeg(NewConj(NewNeg(MkEqXX(1)), MkEqXX(1)))) ],
    true, 0);

  Check('accept positive private pred (x=x vs Q<=>x=x)',
    [ MkForall(MkEqXX(1)) ],
    [ MkForall(MkPriv(1, 1, MkEqXX(1))) ],
    true, 0);

  Check('accept double-negated private pred (x=x vs not R, R<=>x<>x)',
    [ MkForall(MkEqXX(1)) ],
    [ MkForall(NewNeg(MkPriv(1, 1, NewNeg(MkEqXX(1))))) ],
    true, 0);

  Check('accept negated scheme matching negated private pred (x<>x vs R<=>x<>x)',
    [ MkForall(NewNeg(MkEqXX(1))) ],
    [ MkForall(MkPriv(1, 1, NewNeg(MkEqXX(1)))) ],
    true, 0);

  Check('accept chained double-negated private pred (x=x vs not Q1, Q1<=>Q2<=>x<>x)',
    [ MkForall(MkEqXX(1)) ],
    [ MkForall(NewNeg(MkPriv(1, 1, MkPriv(2, 1, NewNeg(MkEqXX(1)))))) ],
    true, 0);

  writeln('Summary: ', gPassed, ' passed, ', gFailed, ' failed.');
  if gFailed > 0 then Halt(1);
end.
