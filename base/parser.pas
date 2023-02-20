(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit parser;

interface
uses mscanner;

var StillCorrect: boolean = true;

type ReadTokenProcedure = Procedure;

const ReadTokenProc: ReadTokenProcedure = ReadToken;

procedure Parse;
procedure SemErr(fErrNr: integer);

implementation

uses syntax,errhan,pragmas
{$IFDEF MDEBUG} ,info {$ENDIF};

const

paUnexpOf		 = 183;
paUnexpOver		 = 184;
paUnexpEquals		 = 186;
paUnexpAntonym1		 = 198;
paUnexpAntonym2		 = 198;
paUnexpSynonym		 = 199;
paUnpairedSymbol	 = 214;
paEndExp		 = 215;
paUnexpHereby		 = 216;
paAdjClusterExp = 223;
paUnexpReconsider        = 228;
paPerExp		 = 231;
paSupposeOrCaseExp	 = 232;
paOfExp		 = 256;
paUnexpRedef = 273;
paAllExp = 275;
paIdentExp1		 = 300;
paIdentExp2		 = 300;
paIdentExp3		 = 300;
paIdentExp4		 = 300;
paIdentExp5		 = 300;
paIdentExp6		 = 300;
paIdentExp7		 = 300;
paIdentExp8		 = 300;
paIdentExp9		 = 300;
paIdentExp10		 = 300;
paIdentExp11		 = 300;
paIdentExp12		 = 300;
paIdentExp13		 = 300;
paWrongPredPattern	 = 301;
paFunctExp1		 = 302;
paFunctExp2		 = 302;
paFunctExp3		 = 302;
paFunctExp4		 = 302;
paWrongModePatternBeg	 = 303;
paStructExp1		 = 304;
paSelectExp1		 = 305;
paAttrExp1		 = 306;
paAttrExp2		 = 306;
paAttrExp3		 = 306;
paNumExp		 = 307;
paWrongReferenceBeg	 = 308;
paTypeOrAttrExp		 = 309;
paRightBraExp1		 = 310;
paRightBraExp2		 = 310;
paWrongRightBracket1	 = 311;
paWrongRightBracket2	 = 311;
paDefExp		 = 312;
paSchExp		 = 313;
paWrongPattBeg1          = 314;
paWrongPattBeg2          = 314;
paWrongPattBeg3          = 314;
paWrongModePatternSet	 = 315;
paWrongAfterThe		 = 320;
paWrongPredSymbol	 = 321;
paSemicolonExp		 = 330;
paUnexpConnective	 = 336;
paWrongScopeBeg		 = 340;
paThatExp1		 = 350;
paThatExp2		 = 350;
paCasesExp		 = 351;
paLeftParenthExp	 = 360;
paLeftSquareExp		 = 361;
paLeftCurledExp		 = 362;
paLeftDoubleExp1	 = 363;
paLeftDoubleExp3	 = 363;
paWrongSchemeVarQual	 = 364;
paRightParenthExp1	 = 370;
paRightParenthExp2	 = 370;
paRightParenthExp3	 = 370;
paRightParenthExp4	 = 370;
paRightParenthExp5	 = 370;
paRightParenthExp6	 = 370;
paRightParenthExp7	 = 370;
paRightParenthExp8	 = 370;
paRightParenthExp9	 = 370;
paRightParenthExp10	 = 370;
paRightParenthExp11	 = 370;
paRightSquareExp1	 = 371;
paRightSquareExp2	 = 371;
paRightSquareExp3	 = 371;
paRightSquareExp4	 = 371;
paRightSquareExp5	 = 371;
paRightCurledExp1	 = 372;
paRightCurledExp2	 = 372;
paRightCurledExp3	 = 372;
paRightDoubleExp1	 = 373;
paRightDoubleExp2	 = 373;
paWrongAttrPrefixExpr = 375;
paWrongAttrArgumentSuffix = 376;
paTypeExpInAdjectiveCluster = 377;
paEqualityExp1		 = 380;
paEqualityExp2		 = 380;
paIfExp			 = 381;
paForExp		 = 382;
paIsExp			 = 383;
paColonExp1		 = 384;
paColonExp2		 = 384;
paColonExp3		 = 384;
paColonExp4		 = 384;
paArrowExp1		 = 385;
paArrowExp2		 = 385;
paMeansExp		 = 386;
paStExp			 = 387;
paAsExp			 = 388;
paProofExp		 = 389;
paWithExp                = 390;
paWrongItemBeg		 = 391;
paUnexpItemBeg		 = 392;
paWrongJustificationBeg	 = 395;
paWrongFormulaBeg	 = 396;
paWrongTermBeg		 = 397;
paWrongRadTypeBeg	 = 398;
paWrongFunctorPatternBeg = 399;
paStillNotImplemented = 400;
paNotExpected = 401;
paInfinitiveExp = 402;
paSuchExp = 403;
paToExp = 404;
paTypeUnexpInClusterRegistration = 405;
paForOrArrowExpected = 406;

const
  gMainSet: set of TokenKind =
    [ sy_Begin,sy_Semicolon,sy_Proof,sy_Now,sy_Hereby,sy_Definition,
      sy_End, sy_Theorem,sy_Reserve,
      sy_Notation,sy_Registration,
      sy_Scheme,EOT,
      sy_Deffunc,sy_Defpred,
      sy_Reconsider,sy_Consider,sy_Then,
//      sy_Let,sy_Given,sy_Assume,sy_Take,sy_Thus,
      sy_Per,sy_Case,sy_Suppose
    ];

var gAddSymbolsSet: set of char = [];

procedure SynErr(fPos:Position; fErrNr:integer);
begin
 if StillCorrect then
  begin
   StillCorrect:=false;
   if CurWord.Kind = sy_Error then
    begin
     if CurWord.Nr <> scTooLongLineErrorNr then ErrImm(CurWord.Nr)
     else Error(fPos,fErrNr);
    end
   else Error(fPos,fErrNr);
   while not (CurWord.Kind in gMainSet) do ReadTokenProc;
  end;
end;

procedure MissingWord(fErrNr:integer);
 var lPos: Position;
begin
 lPos:=PrevPos;
 inc(lPos.Col);
 SynErr(lPos,fErrNr)
end;

procedure WrongWord(fErrNr:integer);
begin
 SynErr(CurPos,fErrNr)
end;

procedure Semicolon;
begin
 KillItem;
 if CurWord.Kind <> sy_Semicolon then
  MissingWord(paSemicolonExp);
 if CurWord.Kind = sy_Semicolon then ReadTokenProc;
end;

procedure AcceptEnd(fPos:Position);
begin
 if CurWord.Kind = sy_End then ReadTokenProc else
  begin
   Error(fPos,paEndExp);
   MissingWord(paUnpairedSymbol)
  end;
end;

procedure ReadWord;
begin
 Mizassert(2546,StillCorrect);
 ReadTokenProc
end;

function Occurs(fW:TokenKind):boolean;
begin
 Occurs:=false;
 if CurWord.Kind=FW then
  begin
   ReadWord;
   Occurs:=true
  end
end;

procedure Accept(fCh:TokenKind; fErrNr:integer);
begin
 if not Occurs(fCh) then MissingWord(fErrNr)
end;

procedure SemErr(fErrNr:integer);
begin
 if StillCorrect then ErrImm(fErrNr)
end;

 { E x p r e s s i o n s }

const
 TermBegSys:set of TokenKind =
   [ Identifier,InfixOperatorSymbol,Numeral,LeftCircumfixSymbol,sy_LeftParanthesis,
     sy_It,sy_LeftCurlyBracket,sy_LeftSquareBracket,sy_The,sy_Dolar,Structuresymbol ];

procedure OpenParenth(var fParenthCnt:integer);
begin
 fParenthCnt:=0;
 while CurWord.Kind = sy_LeftParanthesis do
  begin
   gSubexpPtr^.ProcessLeftParenthesis;
   ReadWord;
   inc(fParenthCnt);
  end;
end;

procedure CloseParenth(var fParenthCnt:integer);
begin
 while (CurWord.Kind = sy_RightParanthesis) and (fParenthCnt > 0) do
  begin
   dec(fParenthCnt);
   gSubexpPtr^.ProcessRightParenthesis;
   ReadWord;
  end;
end;

procedure TypeSubexpression; forward;

procedure AppendQua;
begin
 while CurWord.Kind = sy_Qua do
  begin
   gSubexpPtr^.ProcessQua;
   ReadWord;
   TypeSubexpression;
   gSubexpPtr^.FinishQualifiedTerm;
  end;
if CurWord.Kind = sy_Exactly then
  begin
   gSubexpPtr^.ProcessExactly;
   ReadWord
  end;
end;

procedure GetArguments(const fArgsNbr:integer); forward;

procedure BracketedTerm;
begin
 gSubexpPtr^.StartBracketedTerm;
 ReadWord;
 GetArguments(MaxVisArgNbr);
 gSubexpPtr^.FinishBracketedTerm;
end;

procedure TermSubexpression; forward;

procedure FormulaSubexpression; forward;

procedure ArgumentsTail(fArgsNbr:integer); forward;


procedure ProcessPostqualification;
begin
 gSubexpPtr^.StartPostqualification;
 while CurWord.Kind = sy_Where do
  begin
   repeat
    gSubexpPtr^.StartPostqualifyingSegment;
    ReadWord;
    repeat
     gSubexpPtr^.ProcessPostqualifiedVariable;
     Accept(Identifier,paIdentExp1);
    until not Occurs(sy_Comma);
    gSubexpPtr^.StartPostqualificationSpecyfication;
    if CurWord.Kind in [sy_Is,sy_are] then
     begin
      ReadWord;
      TypeSubexpression;
     end;
    gSubexpPtr^.FinishPostqualifyingSegment;
   until CurWord.Kind <> sy_Comma;
  end;
end;

procedure GetClosedSubterm;
begin
 case CurWord.Kind of
  Identifier:
   if AheadWord.Kind = sy_LeftParanthesis then
    begin
     gSubexpPtr^.StartPrivateTerm;
     ReadWord;
     ReadWord;
     if CurWord.Kind <> sy_RightParanthesis then GetArguments(MaxVisArgNbr);
     gSubexpPtr^.FinishPrivateTerm;
     Accept(sy_RightParanthesis,paRightParenthExp2);
    end
   else
    begin
     gSubexpPtr^.ProcessSimpleTerm;
     ReadWord
    end;
  StructureSymbol:
   begin
    gSubexpPtr^.StartAggregateTerm;
    ReadWord;
    Accept(sy_StructLeftBracket,paLeftDoubleExp1);
    GetArguments(MaxVisArgNbr);
    gSubexpPtr^.FinishAggregateTerm;
    Accept(sy_StructRightBracket,paRightDoubleExp1);
   end;
  Numeral:
   begin
    gSubexpPtr^.ProcessNumeralTerm;
    ReadWord
   end;
  LeftCircumfixSymbol,sy_LeftSquareBracket:
   begin
    BracketedTerm;
    case Curword.Kind of
     sy_RightSquareBracket,sy_RightCurlyBracket,sy_RightParanthesis: ReadWord;
     else Accept(RightCircumfixSymbol,paRightBraExp1);
    end;
   end;
  sy_It:
   begin
    gSubexpPtr^.ProcessItTerm;
    ReadWord
   end;
  sy_Dolar:
   begin
    gSubexpPtr^.ProcessLocusTerm;
    ReadWord
   end;
  sy_LeftCurlyBracket:
   begin
    gSubexpPtr^.StartBracketedTerm;
    ReadWord;
    TermSubexpression;
    if (CurWord.Kind = sy_Colon) or (CurWord.Kind = sy_Where) then
     begin
      gSubexpPtr^.StartFraenkelTerm;
      ProcessPostqualification;
      gSubexpPtr^.FinishSample;
      Accept(sy_Colon,paColonExp1);
      FormulaSubexpression;
      gSubexpPtr^.FinishFraenkelTerm;
      Accept(sy_RightCurlyBracket,paRightCurledExp1);
     end
    else
     begin
      gSubexpPtr^.FinishArgument;
      ArgumentsTail(MaxVisArgNbr-1);
      gSubexpPtr^.FinishBracketedTerm;
      case Curword.Kind of
       sy_RightSquareBracket,sy_RightCurlyBracket,sy_RightParanthesis: ReadWord;
       else Accept(RightCircumfixSymbol,paRightBraExp1);
      end;
     end;
   end;
  sy_The:
   begin
    gSubexpPtr^.ProcessThe;
    ReadWord;
    case CurWord.Kind of
     SelectorSymbol:
      begin
       gSubexpPtr^.StartSelectorTerm;
       ReadWord;
       if Occurs(sy_Of) then TermSubexpression;
       gSubexpPtr^.FinishSelectorTerm;
      end;
     StructureSymbol:
      if AheadWord.Kind = sy_Of then
       begin
        gSubexpPtr^.StartForgetfulTerm;
        ReadWord;
        Accept(sy_Of,paOfExp);
        TermSubexpression;
        gSubexpPtr^.FinishForgetfulTerm;
       end
      else
       begin
        gSubexpPtr^.StartChoiceTerm;
        TypeSubexpression;
        gSubexpPtr^.FinishChoiceTerm;
       end;
     sy_Set:
      if AheadWord.Kind = sy_Of then
       begin
        ReadWord; //set
        ReadWord; //of
        gSubexpPtr^.StartSimpleFraenkelTerm;
        Accept(sy_All,paAllExp);
        TermSubexpression;
        gSubexpPtr^.StartFraenkelTerm;
        ProcessPostqualification;
        gSubexpPtr^.FinishSimpleFraenkelTerm;
       end
      else
       begin
        gSubexpPtr^.StartChoiceTerm;
        TypeSubexpression;
        gSubexpPtr^.FinishChoiceTerm;
       end;
     ModeSymbol,AttributeSymbol,sy_Non,sy_LeftParanthesis,Identifier,
     InfixOperatorSymbol,Numeral,LeftCircumfixSymbol,sy_It,sy_LeftCurlyBracket,
     sy_LeftSquareBracket,sy_The,sy_Dolar:
      begin
       gSubexpPtr^.StartChoiceTerm;
       TypeSubexpression;
       gSubexpPtr^.FinishChoiceTerm;
      end
     else
      begin
       gSubexpPtr^.InsertIncorrTerm;
       WrongWord(paWrongAfterThe)
      end;
    end;
   end;
  else RunTimeError(2133);
 end;
end;

procedure CompleteArgument(var fParenthCnt:integer);
begin
 gSubexpPtr^.FinishArgument;
 repeat
  AppendQua;
  CloseParenth(fParenthCnt);
 until CurWord.Kind<>sy_Qua (***and (CurWord.Kind <> sy_Exactly)***);
end;

procedure AppendFunc(var fParenthCnt: integer);
begin
 while CurWord.Kind = InfixOperatorSymbol do
  begin
   gSubexpPtr^.StartLongTerm;
   repeat
    gSubexpPtr^.ProcessFunctorSymbol;
    ReadWord;
    case CurWord.Kind of
     sy_LeftParanthesis:
      begin
       gSubexpPtr^.ProcessLeftParenthesis;
       ReadWord;
       GetArguments(MaxVisArgNbr);
       gSubexpPtr^.ProcessRightParenthesis;
       Accept(sy_RightParanthesis,paRightParenthExp3);
      end;
     Identifier,Numeral,LeftCircumfixSymbol,sy_It,sy_LeftCurlyBracket,
     sy_LeftSquareBracket,sy_The,sy_Dolar,StructureSymbol:
{ Chyba po prostu TermSubexpression }
      begin
       gExpPtr^.CreateSubexpression;
       GetClosedSubterm;
       gSubexpPtr^.FinishArgument;
       KillSubexpression;
      end;
    end;
    gSubexpPtr^.FinishArgList;
   until CurWord.Kind <> InfixOperatorSymbol;
   gSubexpPtr^.FinishLongTerm;
   CompleteArgument(fParenthCnt);
  end;
end;

procedure ProcessArguments;
 var lParenthCnt: integer;
begin
  OpenParenth(lParenthCnt);
  case CurWord.Kind of
  Identifier,Numeral,LeftCircumfixSymbol,sy_It,sy_LeftCurlyBracket,
  sy_LeftSquareBracket,sy_The,sy_Dolar,StructureSymbol:
   begin
    GetClosedSubterm;
    CompleteArgument(lParenthCnt);
   end;
  InfixOperatorSymbol:;
  else
   begin
    gSubexpPtr^.InsertIncorrTerm;
    gSubexpPtr^.FinishArgument;
    WrongWord(paWrongTermBeg);
   end;
  end;
  repeat
    AppendFunc(lParenthCnt);
    if CurWord.Kind = sy_Comma then
     begin
      ArgumentsTail(MaxVisArgNbr-1);
      if (lParenthCnt > 0) and (CurWord.Kind = sy_RightParanthesis) then
       begin
         dec(lParenthCnt);
         gSubexpPtr^.ProcessRightParenthesis;
         ReadWord;
       end;
     end;
  until CurWord.Kind <> InfixOperatorSymbol;
  while lParenthCnt > 0 do
   begin
     gSubexpPtr^.ProcessRightParenthesis;
     Accept(sy_RightParanthesis,paRightParenthExp1);
     dec(lParenthCnt);
   end;
end;

procedure ProcessAttributes;
begin
  while (CurWord.Kind in [AttributeSymbol,sy_Non]) or
        (CurWord.Kind in (TermBegSys - [sy_LeftParanthesis,StructureSymbol])) or
        ((CurWord.Kind = sy_LeftParanthesis) and
              not(AheadWord.Kind in [sy_Set,ModeSymbol,StructureSymbol])) or
        (CurWord.Kind =  StructureSymbol) and (AheadWord.Kind = sy_StructLeftBracket)
    do
   begin
     gSubexpPtr^.ProcessNon;
     if CurWord.Kind = sy_Non then ReadWord;
     if (CurWord.Kind in (TermBegSys - [sy_LeftParanthesis,StructureSymbol])) or
        ((CurWord.Kind = sy_LeftParanthesis) and
              not(AheadWord.Kind in [sy_Set,ModeSymbol,StructureSymbol])) or
        (CurWord.Kind =  StructureSymbol) and (AheadWord.Kind = sy_StructLeftBracket)
       then
      begin
       gSubexpPtr^.StartAttributeArguments;
       ProcessArguments;
       gSubexpPtr^.CompleteAttributeArguments;
      end;
     if CurWord.Kind = AttributeSymbol then
      begin
       gSubexpPtr^.ProcessAttribute;
       ReadWord;
      end
     else
      begin
       SynErr(CurPos,paAttrExp1)
      end;
   end;
end;

procedure GetAdjectiveCluster;
begin
 gSubexpPtr^.StartAdjectiveCluster;
 ProcessAttributes;
 gSubexpPtr^.FinishAdjectiveCluster;
end;

procedure RadixTypeSubexpression;
 var lSymbol,lParenthCnt: integer;
begin
 lParenthCnt:=0;
 if  CurWord.Kind = sy_LeftParanthesis then
  begin
   gSubexpPtr^.ProcessLeftParenthesis;
   ReadWord;
   inc(lParenthCnt);
  end;
 gSubexpPtr^.ProcessModeSymbol;
 case CurWord.Kind of
  sy_Set:
   begin ReadWord; {? if Occurs(syOf) then TypeSubexpression
     {?  zawieszone na czas zmiany semantyki ?}
   end;
  ModeSymbol:
   begin lSymbol:=CurWord.Nr; ReadWord;
    if CurWord.Kind = sy_Of then
     if ModeMaxArgs.fList^[lSymbol] = 0 then WrongWord(paUnexpOf)
      else begin ReadWord; GetArguments(ModeMaxArgs.fList^[lSymbol]) end;
   end;
  StructureSymbol:
   begin
    lSymbol:=CurWord.Nr;
    ReadWord;
    if CurWord.Kind = sy_Over then
     if StructModeMaxArgs.fList^[lSymbol] = 0
      then WrongWord(paUnexpOver)
      else
       begin
        ReadWord;
        GetArguments(StructModeMaxArgs.fList^[lSymbol])
       end;
   end;
  else
   begin MissingWord(paWrongRadTypeBeg); gSubexpPtr^.InsertIncorrType end;
 end;
 if lParenthCnt > 0 then
  begin
   gSubexpPtr^.ProcessRightParenthesis;
   Accept(sy_RightParanthesis,paRightParenthExp1);
  end;
 gSubexpPtr^.FinishType;
end;

procedure TypeSubexpression;
begin
 gExpPtr^.CreateSubexpression;
 gSubexpPtr^.StartType;
 gSubexpPtr^.StartAttributes;
 GetAdjectiveCluster;
 RadixTypeSubexpression;
 gSubexpPtr^.CompleteAttributes;
 gSubexpPtr^.CompleteType;
 KillSubexpression;
end;

procedure TermSubexpression;
 var lParenthCnt: integer;
begin
 gExpPtr^.CreateSubexpression;
 OpenParenth(lParenthCnt);
 case CurWord.Kind of
  Identifier,Numeral,LeftCircumfixSymbol,sy_It,sy_LeftCurlyBracket,
  sy_LeftSquareBracket,sy_The,sy_Dolar,StructureSymbol:
   begin
    GetClosedSubterm;
    CompleteArgument(lParenthCnt);
   end;
  InfixOperatorSymbol:;
  else
   begin
    gSubexpPtr^.InsertIncorrTerm;
    gSubexpPtr^.FinishArgument;
    WrongWord(paWrongTermBeg);
   end;
 end;
 AppendFunc(lParenthCnt);
 while lParenthCnt > 0 do
  begin
   ArgumentsTail(MaxVisArgNbr-1);
   dec(lParenthCnt);
   gSubexpPtr^.ProcessRightParenthesis;
   Accept(sy_RightParanthesis,paRightParenthExp10);
   if CurWord.Kind <> InfixOperatorSymbol then MissingWord(paFunctExp3);
   AppendFunc(lParenthCnt);
  end;
 gSubexpPtr^.FinishTerm;
 KillSubexpression;
end;

procedure ArgumentsTail(fArgsNbr:integer);
begin
 while (fArgsNbr > 0) and Occurs(sy_Comma) do
  begin
   gSubexpPtr^.StartArgument;
   TermSubexpression;
   gSubexpPtr^.FinishArgument;
   dec(fArgsNbr);
  end;
end;

procedure GetArguments(const fArgsNbr:integer);
begin
 if fArgsNbr > 0 then
  begin
   TermSubexpression;
   gSubexpPtr^.FinishArgument;
   ArgumentsTail(fArgsNbr-1);
  end;
end;

procedure QuantifiedVariables;
begin
 repeat
  gSubexpPtr^.StartQualifiedSegment;
  ReadWord;
  repeat
    gSubexpPtr^.ProcessVariable;
    Accept(Identifier,paIdentExp2);
  until not Occurs(sy_Comma);
  gSubexpPtr^.StartQualifyingType;
  if Occurs(sy_Be) or Occurs(sy_Being) then TypeSubexpression;
  gSubexpPtr^.FinishQualifiedSegment;
 until CurWord.Kind <> sy_Comma;
end;

procedure ExistentialFormula;
begin
 gSubexpPtr^.StartExistential;
 QuantifiedVariables;
 gSubexpPtr^.FinishQuantified;
 Accept(sy_St,paStExp);
 FormulaSubexpression;
 gSubexpPtr^.FinishExistential;
end;

procedure UniversalFormula;
begin
 gSubexpPtr^.StartUniversal;
 QuantifiedVariables;
 gSubexpPtr^.FinishQuantified;
 if CurWord.Kind = sy_St then
  begin
   gSubexpPtr^.StartRestriction;
   ReadWord;
   FormulaSubexpression;
   gSubexpPtr^.FinishRestriction;
  end;
 case CurWord.Kind of
  sy_Holds:
   begin
    gSubexpPtr^.ProcessHolds;
    ReadWord
   end;
  sy_For, sy_Ex: ;
  else
   begin
    gSubexpPtr^.InsertIncorrFormula;
    MissingWord(paWrongScopeBeg)
   end;
 end;
 FormulaSubexpression;
 gSubexpPtr^.FinishUniversal;
end;

procedure ConditionalTail; forward;

procedure CompleteRightSideOfThePredicativeFormula(aPredSymbol:integer);
begin
  gSubexpPtr^.ProcessRightSideOfPredicateSymbol;
  ReadWord;
  if CurWord.Kind in TermBegSys then
    GetArguments(PredMaxArgs.fList^[aPredSymbol]);
  gSubexpPtr^.FinishRightSideOfPredicativeFormula;
end;

procedure CompleteMultiPredicativeFormula;
begin
  gSubexpPtr^.StartMultiPredicativeFormula;
  repeat
   case CurWord.Kind of
   sy_Equal, PredicateSymbol:
    CompleteRightSideOfThePredicativeFormula(CurWord.Nr);
   sy_Does,sy_Do:
    begin
     gSubexpPtr^.ProcessDoesNot;
     ReadWord;
     Accept(sy_Not,paNotExpected);
     if CurWord.Kind in [PredicateSymbol,sy_Equal] then
      begin
       CompleteRightSideOfThePredicativeFormula(CurWord.Nr);
       gSubexpPtr^.ProcessNegative;
      end
     else
      begin
       gSubExpPtr^.InsertIncorrFormula;
       SynErr(CurPos,paInfinitiveExp)
      end;
    end;
   end;
  until not (CurWord.Kind in [sy_Equal,PredicateSymbol,sy_Does,sy_Do]);
  gSubexpPtr^.FinishMultiPredicativeFormula;
end;

procedure CompletePredicativeFormula(aPredSymbol:integer);
begin
  gSubexpPtr^.ProcessPredicateSymbol;
  ReadWord;
  if CurWord.Kind in TermBegSys then
    GetArguments(PredMaxArgs.fList^[aPredSymbol]);
  gSubexpPtr^.FinishPredicativeFormula;
end;

procedure CompleteAtomicFormula(var aParenthCnt:integer);
 var lPredSymbol: integer;
 label Predicate;
begin
  repeat
   AppendFunc(aParenthCnt);
   if CurWord.Kind = sy_Comma then
    begin
     ArgumentsTail(MaxVisArgNbr-1);
     if (aParenthCnt > 0) and (CurWord.Kind = sy_RightParanthesis) then
      begin
       dec(aParenthCnt);
       gSubexpPtr^.ProcessRightParenthesis;
       ReadWord;
       if CurWord.Kind <> InfixOperatorSymbol then MissingWord(paFunctExp1);
      end;
    end;
  until CurWord.Kind <> InfixOperatorSymbol;
  case CurWord.Kind of
   sy_Equal,PredicateSymbol:
    begin
     CompletePredicativeFormula(CurWord.Nr);
     if CurWord.Kind in [sy_Equal,PredicateSymbol,sy_Does,sy_Do] then
       CompleteMultiPredicativeFormula
    end;
   sy_Does,sy_Do:
    begin
     gSubexpPtr^.ProcessDoesNot;
     ReadWord;
     Accept(sy_Not,paNotExpected);
     if CurWord.Kind in [PredicateSymbol,sy_Equal] then
      begin
       CompletePredicativeFormula(CurWord.Nr);
       gSubexpPtr^.ProcessNegative;
       if CurWord.Kind in [sy_Equal,PredicateSymbol,sy_Does,sy_Do] then
        CompleteMultiPredicativeFormula
      end
     else
      begin
       gSubExpPtr^.InsertIncorrFormula;
       SynErr(CurPos,paInfinitiveExp)
      end;
    end;
   sy_Is:
    begin
     gSubexpPtr^.ProcessAtomicFormula;
     ReadWord;
     if (CurWord.Kind = sy_Not) and
        (AheadWord.Kind in TermBegSys+
          [ModeSymbol,StructureSymbol,sy_Set,AttributeSymbol,sy_Non]) or
       (CurWord.Kind in TermBegSys+
          [ModeSymbol,StructureSymbol,sy_Set,AttributeSymbol,sy_Non]) then
      begin
       gSubexpPtr^.StartType;
       gSubexpPtr^.StartAttributes;
       if CurWord.Kind = sy_Not then
        begin
         gSubexpPtr^.ProcessNegation;
         ReadWord;
        end;
       GetAdjectiveCluster;
       case CurWord.Kind of
        sy_LeftParanthesis,ModeSymbol,StructureSymbol,sy_Set:
         begin
          RadixTypeSubexpression;
          gSubexpPtr^.CompleteAttributes;
          gSubexpPtr^.CompleteType;
          gSubexpPtr^.FinishQualifyingFormula;
         end;
        else
         begin
          gSubexpPtr^.CompleteAttributes;
          gSubexpPtr^.FinishAttributiveFormula;
         end;
       end;
      end
     else
      begin
        gSubExpPtr^.InsertIncorrFormula;
        WrongWord(paTypeOrAttrExp);
      end;
    end;
   else
    begin
     gSubexpPtr^.ProcessAtomicFormula;
     MissingWord(paWrongPredSymbol);
     gSubexpPtr^.InsertIncorrBasic;
    end;
  end;
end;

procedure ViableFormula;
 var lParenthCnt:integer;
 label NotPrivate;
begin
 gExpPtr^.CreateSubexpression;
 OpenParenth(lParenthCnt);
 case CurWord.Kind of
  sy_For: UniversalFormula;
  sy_Ex: ExistentialFormula;
{ !!!!!!!!!!!!!!! Kolejnosc }
  sy_Contradiction:
   begin
    gSubexpPtr^.ProcessContradiction;
    ReadWord;
   end;
  sy_Thesis:
   begin
    gSubexpPtr^.ProcessThesis;
    ReadWord;
   end;
  sy_Not:
   begin
    gSubexpPtr^.ProcessNot;
    ReadWord;
    ViableFormula;
    KillSubexpression;
    gSubexpPtr^.ProcessNegative;
   end;
  Identifier:
   if AheadWord.Kind = sy_LeftSquareBracket then
    begin
     gSubexpPtr^.StartPrivateFormula;
     ReadWord;
     ReadWord;
     if CurWord.Kind <> sy_RightSquareBracket
      then GetArguments(MaxVisArgNbr);
     Accept(sy_RightSquareBracket,paRightSquareExp2);
     gSubexpPtr^.FinishPrivateFormula;
    end
   else goto NotPrivate;
  Numeral,LeftCircumfixSymbol,sy_It,sy_LeftCurlyBracket,sy_LeftSquareBracket,
  sy_The,sy_Dolar,StructureSymbol:
NotPrivate:
   begin
    gSubexpPtr^.StartAtomicFormula;
    { ??? TermSubexpression }
    GetClosedSubterm;
    CompleteArgument(lParenthCnt);
    CompleteAtomicFormula(lParenthCnt);
   end;
  InfixOperatorSymbol,PredicateSymbol,sy_Does,sy_Do,sy_Equal:
   begin
    gSubexpPtr^.StartAtomicFormula;
    CompleteAtomicFormula(lParenthCnt);
   end;
  else
   begin
    gSubexpPtr^.InsertIncorrFormula;
    WrongWord(paWrongFormulaBeg)
   end;
 end;
 while lParenthCnt > 0 do
  begin
   ConditionalTail;
   gSubexpPtr^.ProcessRightParenthesis;
   Accept(sy_RightParanthesis,paRightParenthExp4);
   dec(lParenthCnt);
   CloseParenth(lParenthCnt);
  end;
end;

procedure ConjunctiveTail;
begin
 while (CurWord.Kind = sy_Ampersand) and (AheadWord.Kind <> sy_Ellipsis) do
  begin
   gSubexpPtr^.ProcessBinaryConnective;
   ReadWord;
   ViableFormula;
   KillSubexpression;
   gSubexpPtr^.FinishBinaryFormula;
  end;
end;

procedure FlexConjunctiveTail;
begin
 ConjunctiveTail;
 if CurWord.Kind = sy_Ampersand then
 begin
  Assert(AheadWord.Kind=sy_Ellipsis);
  ReadWord; ReadWord;
  Accept(sy_Ampersand,402);
  gSubexpPtr^.ProcessFlexConjunction;
  ViableFormula;
  ConjunctiveTail;
  KillSubexpression;
  gSubexpPtr^.FinishFlexConjunction;
 end;
end;

procedure DisjunctiveTail;
begin
 FlexConjunctiveTail;
 while (CurWord.Kind = sy_Or) and (AheadWord.Kind <> sy_Ellipsis) do
  begin
   gSubexpPtr^.ProcessBinaryConnective;
   ReadWord;
   ViableFormula;
   FlexConjunctiveTail;
   KillSubexpression;
   gSubexpPtr^.FinishBinaryFormula;
  end;
end;

procedure FlexDisjunctiveTail;
begin DisjunctiveTail;
 if CurWord.Kind = sy_Or then
  begin
   Assert(AheadWord.Kind=sy_Ellipsis);
   ReadWord; ReadWord;
   Accept(sy_Or,401);
   gSubexpPtr^.ProcessFlexDisjunction;
   ViableFormula;
   DisjunctiveTail;
   KillSubexpression;
   gSubexpPtr^.FinishFlexDisjunction;
  end;
end;

procedure ConditionalTail;
begin
 FlexDisjunctiveTail;
 case CurWord.Kind of
  sy_Implies,sy_Iff:
   begin
    gSubexpPtr^.ProcessBinaryConnective;
    ReadWord;
    ViableFormula;
    FlexDisjunctiveTail;
    KillSubexpression;
    gSubexpPtr^.FinishBinaryFormula;
    case CurWord.Kind of
     sy_Implies, sy_Iff: WrongWord(paUnexpConnective);
    end;
   end;
 end;
end;

procedure FormulaSubexpression;
begin
 ViableFormula;
 ConditionalTail;
 KillSubexpression;
end;

       { C o m m u n i c a t i o n   w i t h   I t e m s }

procedure TermExpression;
begin
 gItemPtr^.CreateExpression(exTerm);
 TermSubexpression;
 KillExpression;
end;

procedure TypeExpression;
begin
 gItemPtr^.CreateExpression(exType);
 TypeSubexpression;
 KillExpression;
end;

procedure FormulaExpression;
begin
 gItemPtr^.CreateExpression(exFormula);
 FormulaSubexpression;
 KillExpression;
end;

             { M i s c e l l a n e o u s }

procedure ProcessLab;
begin
 gItemPtr^.ProcessLabel;
 if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Colon) then
  begin
   ReadWord;
   ReadWord
  end;
end;

procedure ProcessSentence;
begin
 gItemPtr^.StartSentence;
 FormulaExpression;
 gItemPtr^.FinishSentence;
end;

procedure InCorrSentence;
begin
 gItemPtr^.StartSentence;
 gItemPtr^.CreateExpression(exFormula);
 gExpPtr^.CreateSubexpression;
 gSubexpPtr^.InsertIncorrFormula;
 KillSubexpression;
 KillExpression;
 gItemPtr^.FinishSentence;
end;

procedure InCorrStatement;
begin
 gItemPtr^.ProcessLabel;
 gItemPtr^.StartRegularStatement;
 InCorrSentence;
end;

procedure ProcessHypotheses;
begin
 repeat
  ProcessLab;
  ProcessSentence;
  gItemPtr^.FinishHypothesis;
 until not Occurs(sy_And)
end;

procedure Assumption;
begin
 if CurWord.Kind = sy_That then
  begin
   gItemPtr^.StartCollectiveAssumption; 
   ReadWord;
   ProcessHypotheses
  end
 else
  begin
   ProcessLab;
   ProcessSentence;
   gItemPtr^.FinishHypothesis;
  end;
 gItemPtr^.FinishAssumption;
end;

procedure FixedVariables;
begin
 gItemPtr^.StartFixedVariables;
 repeat
  gItemPtr^.StartFixedSegment;
  repeat
   gItemPtr^.ProcessFixedVariable;
   Accept(Identifier,paIdentExp4);
  until not Occurs(sy_Comma);
  gItemPtr^.ProcessBeing;
  if Occurs(sy_Be) or Occurs(sy_Being) then TypeExpression;
  gItemPtr^.FinishFixedSegment;
 until not Occurs(sy_Comma);
 gItemPtr^.FinishFixedVariables;
end;

procedure ProcessChoice;
begin
 FixedVariables;
 Accept(sy_Such,paSuchExp);
 Accept(sy_That,paThatExp2);
 repeat 
  gItemPtr^.StartCondition;
  ProcessLab;
  ProcessSentence;
  gItemPtr^.FinishCondition;
 until not Occurs(sy_And);
 gItemPtr^.FinishChoice;
end;

procedure Generalization;
begin
 ReadWord;
 FixedVariables;
 if Occurs(sy_Such) then
  begin
   gItemPtr^.StartAssumption;
   Accept(sy_That,paThatExp1);
   ProcessHypotheses;
   gItemPtr^.FinishAssumption;
  end;
end;

procedure ExistentialAssumption;
begin
 gBlockPtr^.CreateItem(itExistentialAssumption);
 ReadWord;
 ProcessChoice;
end;

procedure Canceled;
begin
 gBlockPtr^.CreateItem(itCanceled);
 ReadWord;
 if CurWord.Kind = Numeral then ReadWord;
 gItemPtr^.FinishTheorem;
end;

         { S i m p l e   J u s t i f i c a t i o n s }

procedure GetReferences;
begin
 gItemPtr^.StartReferences;
 repeat
  ReadWord;
  case CurWord.Kind of
   MMLIdentifier:
    begin
     gItemPtr^.StartLibraryReferences;
     ReadWord;
     if CurWord.Kind = sy_Colon then
      repeat
       ReadWord;
       gItemPtr^.ProcessDef;
       if CurWord.Kind = ReferenceSort then
        begin
         if CurWord.Nr <> ord(syDef) then ErrImm(paDefExp);
         ReadWord;
        end;
       gItemPtr^.ProcessTheoremNumber;
       Accept(Numeral,paNumExp);
      until (CurWord.Kind <> sy_Comma)
       or (AheadWord.Kind = Identifier) or (AheadWord.Kind = MMLIdentifier)
     else MissingWord(paColonExp4);
     gItemPtr^.FinishTheLibraryReferences;
    end;
   Identifier:
    begin
     gItemPtr^.ProcessPrivateReference;
     ReadWord
    end;
   else WrongWord(paWrongReferenceBeg);
  end;
 until CurWord.Kind <> sy_Comma;
 gItemPtr^.FinishReferences;
end;

procedure GetSchemeReference;
begin
  gItemPtr^.StartSchemeReference;
  ReadWord;
  case CurWord.Kind of
   MMLIdentifier:
    begin
     gItemPtr^.StartSchemeLibraryReference;
     ReadWord;
     if CurWord.Kind = sy_Colon then
      begin
       ReadWord;
       gItemPtr^.ProcessSch;
       if CurWord.Kind = ReferenceSort then
        begin
         if CurWord.Nr <> ord(sySch) then ErrImm(paSchExp);
         ReadWord;
        end
       else ErrImm(paSchExp);
       gItemPtr^.ProcessSchemeNumber;
       Accept(Numeral,paNumExp);
      end
     else MissingWord(paColonExp4);
     gItemPtr^.FinishSchLibraryReferences;
    end;
   Identifier:
    begin
     gItemPtr^.ProcessSchemeReference;
     ReadWord
    end;
   else WrongWord(paWrongReferenceBeg);
  end;
  if CurWord.Kind = sy_LeftParanthesis then
   begin
    GetReferences;
    Accept(sy_RightParanthesis,paRightParenthExp7)
   end;
  gItemPtr^.FinishSchemeReference;
end;

procedure SimpleJustification;
begin
 gItemPtr^.StartSimpleJustification;
 case CurWord.Kind of
  sy_By: GetReferences;
  sy_Semicolon,sy_DotEquals: ;
  sy_From: GetSchemeReference;
  else WrongWord(paWrongJustificationBeg);
 end;
 gItemPtr^.FinishSimpleJustification;
end;

         { S t a t e m e n t s   &   R e a s o n i n g s }

procedure Reasoning; forward;

procedure IgnoreProof;
 var lCounter: integer; ReasPos:Position;
begin
 gBlockPtr^.StartAtSignProof;
 ReasPos:=CurPos;
 ReadTokenProc;
 lCounter:=1;
 repeat
  case CurWord.Kind of
   sy_Proof,sy_Now,sy_Hereby,sy_Case,sy_Suppose: inc(lCounter);
   sy_End: dec(lCounter);
   sy_Reserve,sy_Scheme,sy_Theorem,sy_Definition,sy_Begin,sy_Notation,
   sy_Registration,EOT:
    begin
     AcceptEnd(ReasPos);
     exit
    end;
  end;
  ReadTokenProc;
 until lCounter=0;
 gBlockPtr^.FinishAtSignProof;
end;

procedure Justification;
begin
 gItemPtr^.StartJustification;
 case CurWord.Kind of
  sy_Proof:
    if ProofPragma then Reasoning
    else IgnoreProof;
  else SimpleJustification;
 end;
 gItemPtr^.FinishJustification;
end;

procedure ReadTypeList;
begin
 case CurWord.Kind of
  sy_RightSquareBracket,sy_RightParanthesis:;
  else
   repeat
    TypeExpression;
    gItemPtr^.FinishLocusType
   until not Occurs(sy_Comma);
 end;
end;

procedure RegularStatement; forward;

procedure PrivateItem;
begin
 gBlockPtr^.ProcessLink;
 if CurWord.Kind = sy_Then then ReadWord;
 case CurWord.Kind of
  sy_Deffunc:
   begin
    gBlockPtr^.CreateItem(itPrivFuncDefinition);
    ReadWord;
    gItemPtr^.StartPrivateDefiniendum;
    Accept(Identifier,paIdentExp6);
    Accept(sy_LeftParanthesis,paLeftParenthExp);
    ReadTypeList;
    Accept(sy_RightParanthesis,paRightParenthExp8);
    gItemPtr^.StartPrivateDefiniens;
    Accept(sy_Equal,paEqualityExp1);
    TermExpression;
    gItemPtr^.FinishPrivateFuncDefinienition;
   end;
  sy_Defpred:
   begin
    gBlockPtr^.CreateItem(itPrivPredDefinition);
    ReadWord;
    gItemPtr^.StartPrivateDefiniendum;
    Accept(Identifier,paIdentExp7);
    Accept(sy_LeftSquareBracket,paLeftSquareExp);
    ReadTypeList;
    Accept(sy_RightSquareBracket,paRightSquareExp4);
    gItemPtr^.StartPrivateDefiniens;
    Accept(sy_Means,paMeansExp);
    FormulaExpression;
    gItemPtr^.FinishPrivatePredDefinienition;
   end;
  sy_Set:
   begin
    gBlockPtr^.CreateItem(itConstantDefinition);
    ReadWord;
    repeat
     gItemPtr^.StartPrivateConstant;
     Accept(Identifier,paIdentExp8);
     Accept(sy_Equal,paEqualityExp2);
     TermExpression;
     gItemPtr^.FinishPrivateConstant;
    until not Occurs(sy_Comma);
   end;
  sy_Reconsider:
   begin
    gBlockPtr^.CreateItem(itReconsider);
    ReadWord;
    repeat
     gItemPtr^.ProcessReconsideredVariable;
     Accept(Identifier,paIdentExp9);
     case CurWord.Kind of
     sy_Equal:
       begin
        ReadWord;
        TermExpression;
        gItemPtr^.FinishReconsideredTerm;
       end;
      else gItemPtr^.FinishDefaultTerm;
     end;
    until not Occurs(sy_Comma);
    gItemPtr^.StartNewType;
    Accept(sy_As,paAsExp);
    TypeExpression;
    gItemPtr^.FinishReconsidering;  
    SimpleJustification;
   end;
  sy_Consider:
   begin
    gBlockPtr^.CreateItem(itChoice);
    ReadWord;
    ProcessChoice;
    SimpleJustification;
   end;
  Identifier,sy_Now,sy_For,sy_Ex,sy_Not,sy_Thesis,sy_LeftSquareBracket,
  sy_Contradiction,PredicateSymbol,sy_Does,sy_Do,sy_Equal,InfixOperatorSymbol,
  Numeral,LeftCircumfixSymbol,sy_LeftParanthesis,sy_It,sy_Dolar,
  StructureSymbol,sy_The,sy_LeftCurlyBracket,sy_Proof:
   begin
    gBlockPtr^.CreateItem(itRegularStatement);
    RegularStatement;
   end;
  else
   begin
    gBlockPtr^.CreateItem(itIncorrItem);
    WrongWord(paWrongItemBeg);
   end;
 end;
end;

procedure ProcessPragmas;
begin
 while CurWord.Kind = Pragma do
 begin
   SetParserPragma(CurWord.Spelling);
   gBlockPtr^.ProcessPragma;
   ReadTokenProc;
 end;
end;

procedure LinearReasoning;
begin
 while CurWord.Kind <> sy_End do
  begin
   StillCorrect:=true;
   ProcessPragmas;
   case CurWord.Kind of
    sy_Let:
     begin
      gBlockPtr^.CreateItem(itGeneralization);
      Generalization;
     end;
    sy_Given: ExistentialAssumption;
    sy_Assume:
     begin
      gBlockPtr^.CreateItem(itAssumption);
      ReadWord;
      Assumption;
     end;
    sy_Take:
     begin
      gBlockPtr^.CreateItem(itExemplification);
      ReadWord;
      repeat
       if (CurWord.Kind=Identifier) and (AheadWord.Kind=sy_Equal) then
        begin
         gItemPtr^.ProcessExemplifyingVariable;
         ReadWord;
         ReadWord;
         TermExpression;
         gItemPtr^.FinishExemplifyingVariable;
        end
       else
        begin
         gItemPtr^.StartExemplifyingTerm;
         TermExpression;
         gItemPtr^.FinishExemplifyingTerm;
        end;
      until not Occurs(sy_Comma);
     end;
    sy_Hereby:
     begin
      gBlockPtr^.CreateItem(itConclusion);
      Reasoning;
     end;
    sy_Hence:
     begin
      gBlockPtr^.ProcessLink;
      ReadWord;
      gBlockPtr^.CreateItem(itConclusion);
      RegularStatement;
     end;
    sy_Thus:
     begin
      ReadWord;
      gBlockPtr^.ProcessLink;
      if CurWord.Kind = sy_Then then ReadWord;
      gBlockPtr^.CreateItem(itConclusion);
      RegularStatement;
     end;
    sy_Per: exit;
    sy_Case,sy_Suppose: exit;
    sy_Reserve,sy_Scheme,sy_Theorem,sy_Definition,sy_Begin,sy_Notation,
    sy_Registration,EOT: exit;
    sy_Then:
     begin
      if AheadWord.Kind = sy_Per then
       begin
        gBlockPtr^.ProcessLink;
        ReadWord;
        exit;
       end
      else
       PrivateItem;
     end;
    else
     PrivateItem;
   end;
   Semicolon;
  end;
end;

procedure NonBlockReasoning;
 var CasePos: Position; lCaseKind:TokenKind;
 procedure ProcessCase;
 begin
  Assumption;
  Semicolon;
  LinearReasoning;
  if CurWord.Kind = sy_Per then
   NonBlockReasoning;
  KillBlock;
  AcceptEnd(CasePos);
  Semicolon;
 end;
begin
 case CurWord.Kind of
  sy_Per,sy_Case,sy_Suppose:
   begin
    gBlockPtr^.CreateItem(itPerCases);
    Accept(sy_Per,paPerExp);
    Accept(sy_Cases,paCasesExp);
    SimpleJustification; 
    Semicolon;
    lCaseKind:=CurWord.Kind;
    if (CurWord.Kind <> sy_Case) and (CurWord.Kind <> sy_Suppose) then
     begin
      MissingWord(paSupposeOrCaseExp);
      lCaseKind:=sy_Suppose;
      gBlockPtr^.CreateItem(itCaseBlock);
      gBlockPtr^.CreateBlock(blSuppose);
      gBlockPtr^.CreateItem(itSupposeHead);
      StillCorrect:=true;
      CasePos:=CurPos;
      ProcessCase;
     end;
    repeat
      while (CurWord.Kind = sy_Case) or (CurWord.Kind = sy_Suppose) do
      begin
       gBlockPtr^.CreateItem(itCaseBlock);
       if lCaseKind = sy_Case then gBlockPtr^.CreateBlock(blCase)
        else gBlockPtr^.CreateBlock(blSuppose);
       CasePos:=CurPos;
       StillCorrect:=true;
       if lCaseKind = sy_Case then gBlockPtr^.CreateItem(itCaseHead)
        else gBlockPtr^.CreateItem(itSupposeHead);
       if CurWord.Kind <> lCaseKind then ErrImm(58);
       ReadWord;
       ProcessCase;
      end;
      case Curword.Kind of
      sy_Reserve,sy_Scheme,sy_Theorem,sy_Definition,sy_Begin,sy_Notation,
      sy_Registration,EOT: exit;
      sy_End: ;
      else
       begin
        MissingWord(paSupposeOrCaseExp);
        gBlockPtr^.CreateItem(itCaseBlock);
        if lCaseKind = sy_Case then gBlockPtr^.CreateBlock(blCase)
         else gBlockPtr^.CreateBlock(blSuppose);
        if lCaseKind = sy_Case then gBlockPtr^.CreateItem(itCaseHead)
         else gBlockPtr^.CreateItem(itSupposeHead);
        StillCorrect:=true;
        CasePos:=CurPos;
        ProcessCase;
       end;
      end;
     until (Curword.Kind = sy_End);
   end;
 end;
end;

procedure Reasoning;
 var ReasPos: Position;
begin
 ReasPos:=CurPos;
 case CurWord.Kind of
  sy_Proof:
   begin
    gBlockPtr^.CreateBlock(blProof);
    ReadTokenProc;
   end;
  sy_Hereby:
   begin
    gBlockPtr^.CreateBlock(blHereby);
    ReadTokenProc;
   end;
  sy_Now:
   begin
    gBlockPtr^.CreateBlock(blDiffuse);
    ReadTokenProc;
   end;
  else
   begin
    gBlockPtr^.CreateBlock(blProof);
    WrongWord(paProofExp);
   end;
 end;
 LinearReasoning;
 NonBlockReasoning;
 KillBlock;
 AcceptEnd(ReasPos);
end;

procedure RegularStatement;
begin
 ProcessLab;
 gItemPtr^.StartRegularStatement;
 case CurWord.Kind of
  sy_Now: Reasoning;
  else
   begin
    ProcessSentence;
    case CurWord.Kind of
     sy_Proof:
      begin
       gItemPtr^.StartJustification;
       if ProofPragma then Reasoning else IgnoreProof;
       gItemPtr^.FinishJustification;
      end;
     else
      begin
       gItemPtr^.StartJustification;
       SimpleJustification;
       gItemPtr^.FinishJustification;
       gItemPtr^.FinishCompactStatement;
       while CurWord.Kind = sy_DotEquals do
        begin
         gItemPtr^.StartIterativeStep;
         ReadWord;
         TermExpression;
         gItemPtr^.ProcessIterativeStep; 
         gItemPtr^.StartJustification;
         SimpleJustification;
         gItemPtr^.FinishJustification;
         gItemPtr^.FinishIterativeStep;
        end;
      end;
    end;
   end;
 end;
end;

            { P a t t e r n s }

var gVisibleNbr: integer;

procedure GetVisible;
begin
 gItemPtr^.ProcessVisible;
 inc(gVisibleNbr);
 Accept(Identifier,paIdentExp3);
end;

procedure ReadVisible;
begin
 gItemPtr^.StartVisible;
 gVisibleNbr:=0;
 repeat
  GetVisible;
 until not Occurs(sy_Comma);
 gItemPtr^.FinishVisible; 
end;

procedure GetModePattern;
 var lModesymbol:integer;
begin
 gItemPtr^.StartModePattern;
 case CurWord.Kind of
  sy_Set: 
   begin
    if AheadWord.Kind = sy_Of then WrongWord(paWrongModePatternSet)
    else ReadWord;
   end;
  ModeSymbol:
   begin
    lModeSymbol:=CurWord.Nr;
    gVisibleNbr:=0;
    ReadWord;
    gItemPtr^.ProcessModePattern;
    if Occurs(sy_Of) then ReadVisible;
    if (ModeMaxArgs.fList^[lModeSymbol] < gVisibleNbr) or
     (ModeMaxArgs.fList^[lModeSymbol] = $FF) then
     ModeMaxArgs.fList^[lModeSymbol] := gVisibleNbr;
   end
  else WrongWord(paWrongModePatternBeg);
 end;
 gItemPtr^.FinishModePattern;
end;

procedure ReadParams;
begin
 if Occurs(sy_LeftParanthesis) then
   begin
    ReadVisible;
    Accept(sy_RightParanthesis,paRightParenthExp5)
   end
  else if CurWord.Kind = Identifier then
   begin
    gItemPtr^.StartVisible;
    GetVisible;
    gItemPtr^.FinishVisible;
   end;
end;

procedure GetAttrPattern;
begin
 gItemPtr^.StartAttributePattern;
 gVisibleNbr:=0;
 GetVisible;
 gItemPtr^.ProcessAttributePattern;
 Accept(sy_Is,paIsExp);
 if Occurs(sy_LeftParanthesis) then
   begin
    ReadVisible;
    Accept(sy_RightParanthesis,paRightParenthExp11)
   end
  else if CurWord.Kind = Identifier then ReadVisible;
 gItemPtr^.FinishAttributePattern;
 Accept(AttributeSymbol,paAttrExp2);
end;

procedure GetFuncPattern;
begin
 gItemPtr^.StartFunctorPattern;
 case CurWord.Kind of
  Identifier,InfixOperatorSymbol,sy_LeftParanthesis:
   begin
    ReadParams;
    gItemPtr^.ProcessFunctorSymbol;
    Accept(InfixOperatorSymbol,paFunctExp2);
    ReadParams;
    gItemPtr^.FinishFunctorPattern;
   end;
  LeftCircumfixSymbol,sy_LeftSquareBracket,sy_LeftCurlyBracket:
   begin
    ReadWord;
    ReadVisible;
    gItemPtr^.FinishFunctorPattern;
    case Curword.Kind of
     sy_RightSquareBracket,sy_RightCurlyBracket,sy_RightParanthesis: ReadWord;
     else Accept(RightCircumfixSymbol,paRightBraExp2);
    end;
   end;
  else
   begin
    WrongWord(paWrongFunctorPatternBeg);
    gItemPtr^.FinishFunctorPattern;
   end;
 end;
end;

procedure GetPredPattern;
 var lPredSymbol: integer;
begin
 gItemPtr^.StartPredicatePattern;
 if CurWord.Kind = Identifier then ReadVisible;
 gItemPtr^.ProcessPredicateSymbol;
 case CurWord.Kind of
 sy_Equal,PredicateSymbol:
   begin
    lPredSymbol:=CurWord.Nr;
    if CurWord.Kind =sy_Equal then lPredSymbol:=EqualitySym;
    gVisibleNbr:=0;
    ReadWord;
    if CurWord.Kind = Identifier then ReadVisible;
    if (PredMaxArgs.fList^[lPredSymbol] < gVisibleNbr) or
     (PredMaxArgs.fList^[lPredSymbol] = $FF) then
     PredMaxArgs.fList^[lPredSymbol] := gVisibleNbr;
   end;
  else WrongWord(paWrongPredPattern);
 end;
 gItemPtr^.FinishPredicatePattern;
end;

procedure Specification;
begin
 gItemPtr^.StartSpecification;
 Accept(sy_Arrow,paArrowExp1);
 TypeExpression;
 gItemPtr^.FinishSpecification;
end;

procedure GetStructPatterns;
 var lStructureSymbol: integer;
begin
 gBlockPtr^.CreateItem(itDefStruct);
 ReadWord;
 if CurWord.Kind = sy_LeftParanthesis then
  begin
   repeat
    gItemPtr^.StartPrefix;
    ReadWord;
    TypeExpression;
    gItemPtr^.FinishPrefix;
   until CurWord.Kind <> sy_Comma;
   Accept(sy_RightParanthesis,paRightParenthExp6);
  end;
 gItemPtr^.ProcessStructureSymbol;
 lStructureSymbol := $FF;
 if CurWord.Kind = StructureSymbol then lStructureSymbol:=CurWord.Nr;
 Accept(StructureSymbol,paStructExp1);
 if Occurs(sy_Over) then ReadVisible;
 gItemPtr^.StartFields;
 if lStructureSymbol <> $FF then
  if (StructModeMaxArgs.fList^[lStructureSymbol] < gVisibleNbr) or
   (StructModeMaxArgs.fList^[lStructureSymbol] = $FF) then
   StructModeMaxArgs.fList^[lStructureSymbol] := gVisibleNbr;
 Accept(sy_StructLeftBracket,paLeftDoubleExp3);
 repeat
  gItemPtr^.StartAggrPattSegment;
  repeat
   gItemPtr^.ProcessField;
   Accept(SelectorSymbol,paSelectExp1);
  until not Occurs(sy_Comma);
  Specification;
  gItemPtr^.FinishAggrPattSegment;
 until not Occurs(sy_Comma);
 gItemPtr^.FinishFields;
 Accept(sy_StructRightBracket,paRightDoubleExp2);
end;

       { D e f i n i t i o n s }

procedure ConstructionType;
begin
 gItemPtr^.StartConstructionType;
 if CurWord.Kind = sy_Arrow then
  begin
   ReadWord;
   TypeExpression
  end;
 gItemPtr^.FinishConstructionType;
end;

procedure Correctness;
begin
 while CurWord.Kind = sy_CorrectnessCondition do
  begin
   StillCorrect:=true;
   gBlockPtr^.CreateItem(itCorrCond);
   ReadWord;
   Justification; 
   Semicolon;
  end;
 gItemPtr^.ProcessCorrectness;  {o jaki tu item chodzi? definitional-item?}
 if CurWord.Kind = sy_Correctness then
  begin
   StillCorrect:=true;
   gBlockPtr^.CreateItem(itCorrectness);
   ReadWord;
   Justification; 
   Semicolon;
  end;
end;

procedure Definition;
 var lDefKind: TokenKind;
     lDefiniensExpected: boolean;
begin
 lDefKind:=CurWord.Kind;
 lDefiniensExpected:=true;
 case CurWord.Kind of
  sy_Mode:
   begin
    gBlockPtr^.CreateItem(itDefMode);
    ReadWord;
    GetModePattern;
    case CurWord.Kind of
     sy_Is:
      begin
       gItemPtr^.StartExpansion;
       ReadWord;
       TypeExpression;
       lDefiniensExpected:=false;
      end;
     else ConstructionType;
    end;
   end;
  sy_Attr:
   begin
    gBlockPtr^.CreateItem(itDefAttr);
    ReadWord;
    GetAttrPattern;
   end;
  sy_Struct:
   begin
    GetStructPatterns;
    lDefiniensExpected:=false;
   end;
  sy_Func:
   begin
    gBlockPtr^.CreateItem(itDefFunc);
    ReadWord;
    GetFuncPattern;
    ConstructionType;
   end;
  sy_Pred:
   begin
    gBlockPtr^.CreateItem(itDefPred);
    ReadWord;
    gItemPtr^.StartDefPredicate;
    GetPredPattern;
   end;
 end;
 if lDefiniensExpected then
 case CurWord.Kind of
 sy_Means:
  begin
   gItemPtr^.ProcessMeans;
   ReadWord;
   if Occurs(sy_Colon) then
    begin
     gItemPtr^.ProcessDefiniensLabel;
     Accept(Identifier,paIdentExp10);
     Accept(sy_Colon,paColonExp2);
    end
   else gItemPtr^.ProcessDefiniensLabel;
   gItemPtr^.StartDefiniens;
   FormulaExpression;
   if CurWord.Kind = sy_If then
    begin
     gItemPtr^.StartGuard;
     ReadWord;
     FormulaExpression;
     gItemPtr^.FinishGuard;
     while Occurs(sy_Comma) do
      begin
       FormulaExpression;
       gItemPtr^.StartGuard;
       Accept(sy_If,paIfExp);
       FormulaExpression;
       gItemPtr^.FinishGuard;
      end;
     if CurWord.Kind = sy_Otherwise then
      begin
       gItemPtr^.StartOtherwise;
       ReadWord;
       FormulaExpression;
       gItemPtr^.FinishOtherwise;
      end;
    end
   else gItemPtr^.FinishOtherwise;
   gItemPtr^.FinishDefiniens;
  end;
 sy_Equals:
  if lDefKind <> sy_Func then
   begin
    WrongWord(paUnexpEquals);
   end
  else
   begin
    gItemPtr^.ProcessEquals;
    ReadWord;
    if Occurs(sy_Colon) then
     begin
      gItemPtr^.ProcessDefiniensLabel;
      Accept(Identifier,paIdentExp10);
      Accept(sy_Colon,paColonExp2);
     end
    else gItemPtr^.ProcessDefiniensLabel;
    gItemPtr^.StartEquals;
    TermExpression;
    if CurWord.Kind = sy_If then
     begin
      gItemPtr^.StartGuard;
      ReadWord;
      FormulaExpression;
      gItemPtr^.FinishGuard;
      while Occurs(sy_Comma) do
       begin
        TermExpression;
        gItemPtr^.StartGuard;
        Accept(sy_If,paIfExp);
        FormulaExpression;
        gItemPtr^.FinishGuard;
       end;
      if CurWord.Kind = sy_Otherwise then
       begin
        gItemPtr^.StartOtherwise;
        ReadWord;
        TermExpression;
        gItemPtr^.FinishOtherwise;
       end;
     end
    else gItemPtr^.FinishOtherwise;
    gItemPtr^.FinishDefiniens;
   end;
 end;
 Semicolon;
 Correctness;
 while (CurWord.Kind = sy_Property) do
  begin
   gBlockPtr^.CreateItem(itProperty);
   StillCorrect:=true;
   ReadWord;
   Justification;
   Semicolon;
  end;
 gBlockPtr^.FinishDefinition;
end;

function CurrPatternKind: TokenKind;
begin
 if CurWord.Kind = ModeSymbol then
   CurrPatternKind:=ModeSymbol
 else if CurWord.Kind = StructureSymbol then
   CurrPatternKind:=StructureSymbol
 else if (CurWord.Kind = Identifier) and (AheadWord.Kind = sy_Is) then
   CurrPatternKind:=AttributeSymbol
 else if (CurWord.Kind in [LeftCircumfixSymbol,sy_LeftCurlyBracket,
                           sy_LeftSquareBracket,sy_LeftParanthesis,
                           InfixOperatorSymbol]) or
         (CurWord.Kind = Identifier) and (AheadWord.Kind = InfixOperatorSymbol) then
   CurrPatternKind:=InfixOperatorSymbol
 else if (CurWord.Kind = PredicateSymbol) or (CurWord.Kind =sy_Equal) or
         (CurWord.Kind = Identifier) and
         (AheadWord.Kind in [sy_Comma,PredicateSymbol,sy_Equal]) then
   CurrPatternKind:=PredicateSymbol
 else if (CurWord.Kind = sy_The) and (AheadWord.Kind = SelectorSymbol) then
   CurrPatternKind:=SelectorSymbol
 else if (CurWord.Kind = sy_The) and (AheadWord.Kind = StructureSymbol) then
   CurrPatternKind:=ForgetfulFunctor
 else CurrPatternKind:=sy_Error;
end;

procedure Synonym;
begin
 ReadWord;
 case CurrPatternKind of
 ModeSymbol:
  begin {Mode synonym}
   gBlockPtr^.CreateItem(itModeNotation);
   GetModePattern;
   gItemPtr^.ProcessModeSynonym;
   Accept(sy_For, paForExp);
   GetModePattern;
  end;
 AttributeSymbol:
  begin {Attribute synonym}
   gBlockPtr^.CreateItem(itAttrSynonym);
   GetAttrPattern;
   gItemPtr^.ProcessAttrSynonym;
   Accept(sy_For, paForExp);
   GetAttrPattern;
  end;
 InfixOperatorSymbol:
  begin {Functor synonym}
   gBlockPtr^.CreateItem(itFuncNotation);
   GetFuncPattern;
   gItemPtr^.ProcessFuncSynonym;
   Accept(sy_For, paForExp);
   GetFuncPattern;
  end;
 PredicateSymbol:
  begin {Predicate synonym}
   gBlockPtr^.CreateItem(itPredSynonym);
   gItemPtr^.StartDefPredicate;
   GetPredPattern;
   gItemPtr^.ProcessPredSynonym;
   Accept(sy_For, paForExp);
   GetPredPattern;
  end
 else
  begin
   gBlockPtr^.CreateItem(itIncorrItem);
   ErrImm(paWrongPattBeg1);
  end;
 end;
end;

procedure Antonym;
begin
 ReadWord;
 case CurrPatternKind of
 Attributesymbol:
  begin {Attribute antonym}
   gBlockPtr^.CreateItem(itAttrAntonym);
   GetAttrPattern;
   gItemPtr^.ProcessAttrAntonym;
   Accept(sy_For, paForExp);
   GetAttrPattern;
  end;
 PredicateSymbol:
  begin {Predicate antonym}
   gBlockPtr^.CreateItem(itPredAntonym);
   gItemPtr^.StartDefPredicate;
   GetPredPattern;
   gItemPtr^.ProcessPredAntonym;
   Accept(sy_For, paForExp);
   GetPredPattern;
  end
 else
  begin
   gBlockPtr^.CreateItem(itIncorrItem);
   ErrImm(paWrongPattBeg2);
  end;
 end;
end;

procedure UnexpectedItem;
begin
  case CurWord.Kind of
   sy_Case,sy_Suppose,sy_Hereby:
    begin
     ErrImm(paWrongItemBeg);
     ReadWord;
     if CurWord.Kind  = sy_That then ReadWord;
     PrivateItem;
    end;
   sy_Per:
    begin
     gBlockPtr^.CreateItem(itIncorrItem);
     ErrImm(paWrongItemBeg);
     ReadWord;
     if CurWord.Kind  = sy_Cases then
      begin
       ReadWord;
       InCorrStatement;
       SimpleJustification;
      end;
    end;
   else
    begin
     ErrImm(paUnexpItemBeg);
     StillCorrect:=true;
     PrivateItem;
    end;
  end;
end;

procedure DefinitionalBlock;
 var DefPos:Position;
begin
 gBlockPtr^.CreateItem(itDefinition);
 gBlockPtr^.CreateBlock(blDefinition);
 DefPos:=CurPos;
 ReadWord;
 while CurWord.Kind <> sy_End do
  begin
   StillCorrect:=true;
   gBlockPtr^.ProcessRedefine;
   if Occurs(sy_Redefine) then
    if not (CurWord.Kind in [sy_Mode,sy_Attr,sy_Func,sy_Pred]) then
      Error(PrevPos,paUnexpRedef);
   case CurWord.Kind of
    sy_Mode,sy_Attr,sy_Struct,sy_Func,sy_Pred:
      Definition;
    sy_Begin,EOT,sy_Reserve,sy_Scheme,sy_Theorem,
    sy_Definition,sy_Registration,sy_Notation: break;
    Pragma: ProcessPragmas;
    else
     begin
      case CurWord.Kind of
       sy_Let:
        begin
         gBlockPtr^.CreateItem(itLociDeclaration);
         Generalization;
        end;
       sy_Given: ExistentialAssumption;
       sy_Assume:
        begin
         gBlockPtr^.CreateItem(itAssumption);
         ReadWord;
         Assumption;
        end;
       sy_Canceled: Canceled;
       sy_Case,sy_Suppose,sy_Per,sy_Hereby: UnexpectedItem;
       else PrivateItem;
      end;
      Semicolon;
     end;
   end;
  end;
 KillBlock;
 AcceptEnd(DefPos);
end;

procedure NotationBlock;
 var DefPos:Position;
begin
 gBlockPtr^.CreateItem(itDefinition);
 gBlockPtr^.CreateBlock(blNotation);
 DefPos:=CurPos;
 ReadWord;
 while CurWord.Kind <> sy_End do
  begin
   StillCorrect:=true;
   case CurWord.Kind of
    sy_Begin,EOT,sy_Reserve,sy_Scheme,sy_Theorem,sy_Definition,
    sy_Registration,sy_Notation: break;
    Pragma: ProcessPragmas;
    else
     begin
      case CurWord.Kind of
       sy_Synonym: Synonym;
       sy_Antonym: Antonym;
       sy_Let:
        begin
         gBlockPtr^.CreateItem(itLociDeclaration);
         ReadWord;
         FixedVariables;
        end;
      else UnexpectedItem;
      end;
      Semicolon;
     end;
   end;
  end;
 KillBlock;
 AcceptEnd(DefPos);
end;

procedure ATTSubexpression(var aExpKind: ExpKind);
 var lAttrExp: boolean;
begin
 aExpKind:=exNull;
 gSubexpPtr^.StartAttributes;
  while (CurWord.Kind in [AttributeSymbol,sy_Non]) or
        (CurWord.Kind in (TermBegSys - [sy_LeftParanthesis,StructureSymbol])) or
        ((CurWord.Kind = sy_LeftParanthesis) and
              not(AheadWord.Kind in [sy_Set,ModeSymbol,StructureSymbol])) or
        (CurWord.Kind =  StructureSymbol) and (AheadWord.Kind = sy_StructLeftBracket)
  do
  begin
   gSubexpPtr^.ProcessNon;
   lAttrExp:=CurWord.Kind = sy_Non;
   if CurWord.Kind = sy_Non then ReadWord;
   if (CurWord.Kind in (TermBegSys - [StructureSymbol])) or
      (CurWord.Kind =  StructureSymbol) and (AheadWord.Kind = sy_StructLeftBracket)
     then
    begin
     if aExpKind = exNull then aExpKind:=exTerm;
     gSubexpPtr^.StartAttributeArguments;
     ProcessArguments;
     gSubexpPtr^.FinishAttributeArguments; //
    end;
   if CurWord.Kind = AttributeSymbol then
    begin
      aExpKind:=exAdjectiveCluster;
      gSubexpPtr^.ProcessAttribute;
      ReadWord;
    end
   else
    begin
      if lAttrExp or (aExpKind = exAdjectiveCluster) then
       begin
         gSubexpPtr^.ProcessAttribute;
         SynErr(CurPos,paAttrExp3);
       end;
      break;
    end;
  end;
 gSubexpPtr^.CompleteAttributes;
end;

procedure RegisterCluster;
 var lExpKind: ExpKind;
begin
 gBlockPtr^.CreateItem(itCluster);
 ReadWord;
 if (CurWord.Kind = Identifier) and (AheadWord.Kind = sy_Arrow)
  then ErrImm(paFunctExp4);
 gItemPtr^.StartAttributes;
 gItemPtr^.CreateExpression(exAdjectiveCluster);
 gExpPtr^.CreateSubexpression;
 ATTSubexpression(lExpKind);
 case lExpKind of
  exTerm: gSubexpPtr^.CompleteClusterTerm;
  exNull,exAdjectiveCluster: gSubexpPtr^.CompleteAdjectiveCluster;
 end;
 KillSubexpression;
 KillExpression;
 case lExpKind of
 exTerm:
  begin
    gItemPtr^.FinishClusterTerm;
    Accept(sy_Arrow,paArrowExp2);
    gItemPtr^.CreateExpression(exAdjectiveCluster);
    gExpPtr^.CreateSubexpression;
    gSubexpPtr^.StartAttributes;
    ATTSubexpression(lExpKind);
    if lExpKind <> exAdjectiveCluster then
     begin
      ErrImm(paAdjClusterExp)
     end;
    gSubexpPtr^.CompleteAdjectiveCluster;
    KillSubexpression;
    KillExpression;
    gItemPtr^.FinishConsequent;
    if CurWord.Kind = sy_For then
     begin
       ReadWord;
       gItemPtr^.CreateExpression(exType);
       gExpPtr^.CreateSubexpression;
       gSubexpPtr^.StartType;
       gSubexpPtr^.StartAttributes;
       GetAdjectiveCluster;
       RadixTypeSubexpression;
       gSubexpPtr^.CompleteAttributes;
       gSubexpPtr^.CompleteType;
       gSubexpPtr^.CompleteClusterType;
       KillSubexpression;
       KillExpression;
     end;
    gItemPtr^.FinishClusterType;
  end;
 exNull,exAdjectiveCluster:
  case CurWord.Kind of
  sy_Arrow:
   begin
     gItemPtr^.FinishAntecedent;
     ReadWord;
     gItemPtr^.CreateExpression(exAdjectiveCluster);
     gExpPtr^.CreateSubexpression;
     gSubexpPtr^.StartAttributes;
     ATTSubexpression(lExpKind);
     if lExpKind <> exAdjectiveCluster then
       begin
        ErrImm(paAdjClusterExp);
       end;
     gSubexpPtr^.CompleteAdjectiveCluster;
     KillSubexpression;
     KillExpression;
     gItemPtr^.FinishConsequent;
     Accept(sy_For,paForExp);
     gItemPtr^.CreateExpression(exType);
     gExpPtr^.CreateSubexpression;
     gSubexpPtr^.StartType;
     gSubexpPtr^.StartAttributes;
     GetAdjectiveCluster;
     RadixTypeSubexpression;
     gSubexpPtr^.CompleteAttributes;
     gSubexpPtr^.CompleteType;
     gSubexpPtr^.CompleteClusterType;
     KillSubexpression;
     KillExpression;
     gItemPtr^.FinishClusterType;
   end;
  sy_For:
   begin
     gItemPtr^.FinishConsequent;
     ReadWord;
     gItemPtr^.CreateExpression(exType);
     gExpPtr^.CreateSubexpression;
     gSubexpPtr^.StartType;
     gSubexpPtr^.StartAttributes;
     GetAdjectiveCluster;
     RadixTypeSubexpression;
     gSubexpPtr^.CompleteAttributes;
     gSubexpPtr^.CompleteType;
     gSubexpPtr^.CompleteClusterType;
     KillSubexpression;
     KillExpression;
     gItemPtr^.FinishClusterType;
   end;
  else
   begin
     SynErr(CurPos,paForOrArrowExpected);
     gItemPtr^.FinishConsequent;
     gItemPtr^.CreateExpression(exType);
     gExpPtr^.CreateSubexpression;
     gSubexpPtr^.StartType;
     gSubexpPtr^.InsertIncorrType;
     gSubexpPtr^.CompleteType;
     gSubexpPtr^.CompleteClusterType;
     KillSubexpression;
     KillExpression;
     gItemPtr^.FinishClusterType;
   end;
  end;
 end;
 Semicolon;
 Correctness;
end;

procedure Reduction;
 var lExpKind: ExpKind;
begin
 gBlockPtr^.CreateItem(itReduction);
 ReadWord;
 if (CurWord.Kind = Identifier) and (AheadWord.Kind = sy_Arrow)
  then ErrImm(paFunctExp4);
 gItemPtr^.StartFuncReduction;
 TermExpression;
 gItemPtr^.ProcessFuncReduction;
 Accept(sy_To,paToExp);
 TermExpression;
 gItemPtr^.FinishFuncReduction;
 Semicolon;
 Correctness;
end;

procedure Identification;
begin
  gBlockPtr^.CreateItem(itIdentify);
  ReadWord;
{  case CurrPatternKind of
  AttributeSymbol:
   begin
    gItemPtr^.StartAttrIdentify;
    GetAttrPattern;
    gItemPtr^.ProcessAttrIdentify;
    Accept(sy_With, paWithExp);
    GetAttrPattern;
    gItemPtr^.CompleteAttrIdentify;
   end;
  PredicateSymbol:
   begin
    gItemPtr^.StartPredIdentify;
    GetPredPattern;
    gItemPtr^.ProcessPredIdentify;
    Accept(sy_With, paWithExp);
    GetPredPattern;
    gItemPtr^.CompletePredIdentify;
   end;
  InfixOperatorSymbol:}
//   begin
    gItemPtr^.StartFuncIdentify;
    GetFuncPattern;
    gItemPtr^.ProcessFuncIdentify;
    Accept(sy_With, paWithExp);
    GetFuncPattern;
    gItemPtr^.CompleteFuncIdentify;
//   end;
{  end;}
  if CurWord.Kind = sy_When then
   begin
    ReadWord;
    repeat
     gItemPtr^.ProcessLeftLocus;
     Accept(Identifier,paIdentExp3);
     Accept(sy_Equal,paEqualityExp1);
     gItemPtr^.ProcessRightLocus;
     Accept(Identifier,paIdentExp3);
    until not Occurs(sy_Comma);
   end;
  Semicolon;
  Correctness;
end;

procedure RegisterProperty;
begin
 gBlockPtr^.CreateItem(itPropertyRegistration);
 case PropertyKind(CurWord.Nr) of
 sySethood:
  begin
   ReadWord;
   Accept(sy_of, paOfExp);
   gItemPtr^.StartSethoodProperties;
   TypeExpression;
   gItemPtr^.FinishSethoodProperties;
   Justification;
  end;
 else
  begin
   SynErr(CurPos,paStillNotImplemented);
  end;
 end;
 Semicolon;
end;

procedure RegistrationBlock;
 var DefPos:Position;
begin
 gBlockPtr^.CreateItem(itDefinition);
 gBlockPtr^.CreateBlock(blRegistration);
 DefPos:=CurPos;
 ReadWord;
 while CurWord.Kind <> sy_End do
  begin
   StillCorrect:=true;
   case CurWord.Kind of
    sy_Cluster: RegisterCluster;
    sy_Reduce: Reduction;
    sy_Identify: Identification;
    sy_Property: RegisterProperty;
    sy_Begin,EOT,sy_Reserve,sy_Scheme,sy_Theorem,sy_Definition,
    sy_Registration,sy_Notation: break;
    Pragma: ProcessPragmas;
    else
     begin
      case CurWord.Kind of
       sy_Let:
        begin
         gBlockPtr^.CreateItem(itLociDeclaration);
         ReadWord;
         FixedVariables;
        end;
       sy_Canceled: Canceled;
       sy_Case,sy_Suppose,sy_Per,sy_Hereby: UnexpectedItem;
       else PrivateItem;
      end;
      Semicolon;
     end;
   end;
  end;
 KillBlock;
 AcceptEnd(DefPos);
end;

procedure Reservation;
begin
 gBlockPtr^.CreateItem(itReservation);
 ReadWord;
 repeat
   gItemPtr^.StartReservationSegment;
   repeat
    gItemPtr^.ProcessReservedIdentifier;
    Accept(Identifier,paIdentExp11);
   until not Occurs(sy_Comma);
   Accept(sy_For,paForExp);
   gItemPtr^.CreateExpression(exResType);
   TypeSubexpression;
   KillExpression;
   gItemPtr^.FinishReservationSegment;
 until not Occurs(sy_Comma);
 gItemPtr^.FinishReservation;
end;

procedure Theorem;
begin
  gBlockPtr^.CreateItem(itTheorem);
  ReadWord;
  ProcessLab;
  gItemPtr^.StartTheoremBody;
  ProcessSentence;
  gItemPtr^.FinishTheoremBody;
  Justification;
  gItemPtr^.FinishTheorem;
end;

procedure Axiom;
begin
  gBlockPtr^.CreateItem(itAxiom);
  ReadWord;
  ProcessLab;
  gItemPtr^.StartTheoremBody;
  ProcessSentence;
  gItemPtr^.FinishTheoremBody;
  gItemPtr^.FinishTheorem;
end;
               { M a i n   ( w i t h   S c h e m e s ) }

procedure SchemeBlock;
 var SchemePos: Position;
begin
 gBlockPtr^.CreateItem(itSchemeBlock);
 gBlockPtr^.CreateBlock(blPublicScheme);
 ReadWord;
 gBlockPtr^.CreateItem(itSchemeHead);
 gItemPtr^.ProcessSchemeName;
 SchemePos:=PrevPos;
 if CurWord.Kind = Identifier then ReadWord;
 Accept(sy_LeftCurlyBracket,paLeftCurledExp);
 repeat
  gItemPtr^.StartSchemeSegment;
  repeat
   gItemPtr^.ProcessSchemeVariable;
   Accept(Identifier,paIdentExp13);
  until not Occurs(sy_Comma);
  gItemPtr^.StartSchemeQualification;
  case CurWord.Kind of
   sy_LeftSquareBracket:
    begin
     ReadWord;
     ReadTypeList;
     gItemPtr^.FinishSchemeQualification;
     Accept(sy_RightSquareBracket,paRightSquareExp5);
    end;
   sy_LeftParanthesis:
    begin
     ReadWord;
     ReadTypeList;
     gItemPtr^.FinishSchemeQualification;
     Accept(sy_RightParanthesis,paRightParenthExp9);
     Specification;
    end;
   else
    begin
     ErrImm(paWrongSchemeVarQual);
     gItemPtr^.FinishSchemeQualification;
     Specification;
    end;
//     WrongWord(paWrongSchemeVarQual);
  end;
  gItemPtr^.FinishSchemeSegment;
 until not Occurs(sy_Comma);
 Accept(sy_RightCurlyBracket,paRightCurledExp3);
 gItemPtr^.FinishSchemeHeading;
 Accept(sy_Colon,paColonExp3);
 FormulaExpression;
 gItemPtr^.FinishSchemeThesis;
 if CurWord.Kind = sy_Provided then
  repeat
   gItemPtr^.StartSchemePremise;
   ReadWord;
   ProcessLab;
   ProcessSentence;
   gItemPtr^.FinishSchemePremise;
  until CurWord.Kind <> sy_And;
 gItemPtr^.FinishSchemeDeclaration;
 if CurWord.Kind = sy_Proof then
   begin
     KillItem; // only KillItem which is running out of Semicolon procedure
     if not ProofPragma then
      begin
       gBlockPtr^.StartSchemeDemonstration;
       IgnoreProof;
       gBlockPtr^.FinishSchemeDemonstration;
      end
     else
      begin
       StillCorrect:=true;
       Accept(sy_Proof,paProofExp);
       gBlockPtr^.StartSchemeDemonstration;
       LinearReasoning;
       if CurWord.Kind = sy_Per then NonBlockReasoning;
       AcceptEnd(SchemePos);
       gBlockPtr^.FinishSchemeDemonstration;
      end;
   end
  else
   begin
    Semicolon;
    if not ProofPragma then
      begin
       gBlockPtr^.StartSchemeDemonstration;
       IgnoreProof;
       gBlockPtr^.FinishSchemeDemonstration;
      end
     else
      begin
       StillCorrect:=true;
       if CurWord.Kind = sy_Proof then
        begin
         WrongWord(paProofExp);
         StillCorrect:=true;
         ReadWord;
        end;
       gBlockPtr^.StartSchemeDemonstration;
       LinearReasoning;
       if CurWord.Kind = sy_Per then NonBlockReasoning;
       AcceptEnd(SchemePos);
       gBlockPtr^.FinishSchemeDemonstration;
      end;
   end;
 KillBlock;
end;

procedure Parse;
begin
 ReadTokenProc;
 while (CurWord.Kind <> sy_Begin) and (CurWord.Kind <> EOT) do ReadTokenProc;
 if CurWord.Kind = EOT
  then ErrImm(213)
 else
  begin
   gBlockPtr^.StartProperText;
   gBlockPtr^.ProcessBegin;
   Accept(sy_Begin,213);
   while CurWord.Kind <> EOT do
    begin
     while CurWord.Kind in [sy_Begin,Pragma] do
     begin
      ProcessPragmas;
      if CurWord.Kind = sy_Begin then
       begin
        gBlockPtr^.ProcessBegin;
        ReadTokenProc;
       end;
     end;
     StillCorrect:=true;
     if CurWord.Kind = sy_End then
      begin
       repeat
        ErrImm(216);
        ReadTokenProc;
        if CurWord.Kind = sy_Semicolon then ReadTokenProc;
       until CurWord.Kind <> sy_End;
       if CurWord.Kind = sy_Begin then continue;
      end;
     if CurWord.Kind = EOT then break;
     case CurWord.Kind of
      sy_Scheme: SchemeBlock;
      sy_Definition: DefinitionalBlock;
      sy_Notation: NotationBlock;
      sy_Registration: RegistrationBlock;
      sy_Reserve: Reservation;
      sy_Theorem: Theorem;
      sy_Axiom: Axiom;
      sy_Canceled: Canceled;
//      sy_Let,sy_Given:;
//      sy_Take:;
//      sy_Thus:
//      sy_Assume:
      sy_Case,sy_Suppose, sy_Per,sy_Hereby: UnexpectedItem;
      else PrivateItem;
     end;
     Semicolon;
    end;
  end;
 KillBlock;
end;

end.
