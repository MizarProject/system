(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit mscanner;

interface

uses errhan,mobjects,scanner;

type
 TokenKind = (
    {  #0  } syT0,
    {  #1  } syT1,
    {  #2  } syT2,
    {  #3  } syT3,
    {  #4  } syT4,
    {  #5  } syT5,
    {  #6  } syT6,
    {  #7  } syT7,
    {  #8  } syT8,
    {  #9  } syT9,
    {  #10 } syT10,
    {  #11 } syT11,
    {  #12 } syT12,
    {  #13 } syT13,
    {  #14 } syT14,
    {  #15 } syT15,
    {  #16 } syT16,
    {  #17 } syT17,
    {  #18 } syT18,
    {  #19 } syT19,
    {  #20 } syT20,
    {  #21 } syT21,
    {  #22 } syT22,
    {  #23 } syT23,
    {  #24 } syT24,
    {  #25 } syT25,
    {  #26 } syT26,
    {  #27 } syT27,
    {  #28 } syT28,
    {  #29 } syT29,
    {  #30 } syT30,
    {  #31 } syT31,
    {  #32 } Pragma,
    {! #33 } EOT = 33,
    {" #34 } sy_from,
    {# #35 } sy_identify,
    { $ #36} sy_thesis,
    {% #37 } sy_contradiction,
    {& #38 } sy_Ampersand,
    {' #39 } sy_by,
    {( #40 } sy_LeftParanthesis,
    {) #41 } sy_RightParanthesis,
    {* #42 } sy_registration,
    {+ #43 } sy_definition,
    {, #44 } sy_Comma,
    {- #45 } sy_notation,
    {. #46 } sy_Ellipsis,
    {/ #47 } sy_proof,
    {0 #48 } syT48,
    {1 #49 } syT49,
    {2 #50 } syT50,
    {3 #51 } syT51,
    {4 #52 } syT52,
    {5 #53 } syT53,
    {6 #54 } syT54,
    {7 #55 } syT55,
    {8 #56 } syT56,
    {9 #57 } syT57,
    {: #58 } sy_Colon,
    {; #59 } sy_Semicolon,
    {< #60 } sy_now,
    {= #61 } sy_Equal,
    {> #62 } sy_end,
    {? #63 } sy_Error,
    {@ #64 } syT64,
    {A #65 } MMLIdentifier,
    {B #66 } syT66,
    {C #67 } syT67,
    {D #68 } sy_LibraryDirective, // see DirectiveKind
    {E #69 } syT69,
    {F #70 } syT70,
    {G #71 } StructureSymbol,
    {H #72 } syT72,
    {I #73 } Identifier,
    {J #74 } ForgetfulFunctor,
    {K #75 } LeftCircumfixSymbol,
    {L #76 } RightCircumfixSymbol,
    {M #77 } ModeSymbol,
    {N #78 } Numeral,
    {O #79 } InfixOperatorSymbol,
    {P #80 } syT80,
    {Q #81 } ReferenceSort, // sy_sch,  sy_def
    {R #82 } PredicateSymbol,
    {S #83 } syT83,
    {T #84 } syT84,
    {U #85 } SelectorSymbol,
    {V #86 } AttributeSymbol,
    {W #87 } syT87,
    {X #88 } sy_Property, // see PropertyKind
    {Y #89 } sy_CorrectnessCondition, // see CorrectnessKind
    {Z #90 } sy_Dolar, { $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 }
    {[ #91 } sy_LeftSquareBracket,
    {\ #92 } syT92,
    {] #93 } sy_RightSquareBracket,
    {^ #94 } syT94,
    {_ #95 } syT95,
    {` #96 } syT96,
    {a #97 } sy_according,
    {b #98 } syT98,
    {c #99 } sy_reduce,
    {d #100} syT100,
    {e #101} sy_equals,
    {f #102} syT102,
    {g #103} syT103,
    {h #104} sy_with,
    {i #105} syT105,
    {j #106} syT106,
    {k #107} syT107,
    {l #108} syT108,
    {m #109} syT109,
    {n #110} syT110,
    {o #111} syT111,
    {p #112} syT112,
    {q #113} syT113,
    {r #114} sy_wrt = 114,
    {s #115} syT115,
    {t #116} sy_to,
    {u #117} syT117,
    {v #118} syT118,
    {w #119} sy_when,
    {x #120} sy_axiom,
    {y #121} syT121,
    {z #122} syT122,
    {  #123} sy_LeftCurlyBracket,
    {| #124} syT124,
    {  #125} sy_RightCurlyBracket,
    {~ #126} syT126,
    {#127} syT127,
    {#128} syT128,
    {#129} syT129,
    {#130} syT130,
    {#131} syT131,
    {#132} syT132,
    {#133} syT133,
    {#134} syT134,
    {#135} sy_correctness = 135,
    {#136} syT136,
    {#137} syT137,
    {#138} syT138,
    {#139} syT139,
    {#140} sy_if = 140,
    {#141} syT141,
    {#142} syT142,
    {#143} syT143,
    {#144} sy_is = 144,
    {#145} sy_are,
    {#146} syT146,
    {#147} sy_otherwise,
    {#148} syT148,
    {#149} syT149,
    {#150} syT150,
    {#151} syT151,
    {#152} syT152,
    {#153} syT153,
    {#154} syT154,
    {#155} syT155,
    {#156} sy_ex = 156,
    {#157} sy_for,
    {#158} syT158,
    {#159} sy_define,
    {#160} syT160,
    {#161} sy_being,
    {#162} sy_over,
    {#163} syT163,
    {#164} sy_canceled,
    {#165} sy_do,
    {#166} sy_does,
    {#167} sy_or,
    {#168} sy_where,
    {#169} sy_non,
    {#170} sy_not,
    {#171} sy_cluster,
    {#172} sy_attr,
    {#173} syT173,
    {#174} sy_StructLeftBracket,//syLeftAngleBracket,
    {#175} sy_StructRightBracket,//syRightAngleBracket,
    {#176} sy_environ,
    {#177} syT177,
    {#178} sy_begin,
    {#179} syT179,
    {#180} syT180,
    {#181} syT181,
    {#182} syT182,
    {#183} syT183,
    {#184} syT184,
    {#185} sy_hence,
    {#186} syT186,
    {#187} syT187,
    {#188} sy_hereby,
    {#189} syT189,
    {#190} syT190,
    {#191} syT191,
    {#192} sy_then,
    {#193} sy_DotEquals,
    {#194} syT194,
    {#195} syT195,
    {#196} sy_synonym,
    {#197} sy_antonym,
    {#198} syT198,
    {#199} syT199,
    {#200} sy_let,
    {#201} sy_take,
    {#202} sy_assume,
    {#203} sy_thus,
    {#204} sy_given,
    {#205} sy_suppose,
    {#206} sy_consider,
    {#207} syT207,
    {#208} syT208,
    {#209} syT209,
    {#210} syT210,
    {#211} sy_Arrow,
    {#212} sy_as,
    {#213} sy_qua,
    {#214} sy_be,
    {#215} sy_reserve,
    {#216} syT216,
    {#217} syT217,
    {#218} syT218,
    {#219} syT219,
    {#220} syT220,
    {#221} syT221,
    {#222} syT222,
    {#223} syT223,
    {#224} sy_set,
    {#225} sy_selector,
    {#226} sy_cases,
    {#227} sy_per,
    {#228} sy_scheme,
    {#229} sy_redefine,
    {#230} sy_reconsider,
    {#231} sy_case,
    {#232} sy_prefix,
    {#233} sy_the,
    {#234} sy_it,
    {#235} sy_all,
    {#236} sy_theorem,
    {#237} sy_struct,
    {#238} sy_exactly,
    {#239} sy_mode,
    {#240} sy_iff,
    {#241} sy_func,
    {#242} sy_pred,
    {#243} sy_implies,
    {#244} sy_st,
    {#245} sy_holds,
    {#246} sy_provided,
    {#247} sy_means,
    {#248} sy_of,
    {#249} sy_defpred,
    {#250} sy_deffunc,
    {#251} sy_such,
    {#252} sy_that,
    {#253} sy_aggregate,
    {#254} sy_and
  );

 CorrectnessKind  = (syCorrectness,
   syCoherence, syCompatibility, syConsistency, syExistence, syUniqueness, syReducibility);

 PropertyKind = (sErrProperty,
     sySymmetry,syReflexivity,syIrreflexivity,
     syAssociativity,syTransitivity,syCommutativity,
     syConnectedness,syAsymmetry,syIdempotence,
     syInvolutiveness,syProjectivity,sySethood,syAbstractness);

 LibraryReferenceKind = (syThe,syDef,sySch);

 DirectiveKind =
  (syVocabularies,syNotations,
   syDefinitions,syTheorems,sySchemes,syRegistrations,
   syConstructors,syRequirements,syEqualities,syExpansions{,syProperties});

Token =
  record Kind:TokenKind;
    Nr:integer;
    Spelling: string;
  end;

const
 // Homonymic and special symbols in buildin vocabulery
 // Homonymic Selector Symbol
 StrictSym = 1;       // "strict"
 // Homonymic Mode Symbol
 SetSym = 1;          // 'set'
 // Homonymic Predicate Symbol
 EqualitySym = 1;     // '='
 // Homonymic Circumfix Symbols
 SquareBracket = 1;   // '[' ']'}
 CurlyBracket = 2;    //  '{' '}'
 RoundedBracket = 3;  //  '(' ')'

 // Error number: Too long line
 scTooLongLineErrorNr = 200;

 TokenName: array[TokenKind] of string =
  (
    {  #0  } '',
    {  #1  } '',
    {  #2  } '',
    {  #3  } '',
    {  #4  } '',
    {  #5  } '',
    {  #6  } '',
    {  #7  } '',
    {  #8  } '',
    {  #9  } '',
    {  #10 } '',
    {  #11 } '',
    {  #12 } '',
    {  #13 } '',
    {  #14 } '',
    {  #15 } '',
    {  #16 } '',
    {  #17 } '',
    {  #18 } '',
    {  #19 } '',
    {  #20 } '',
    {  #21 } '',
    {  #22 } '',
    {  #23 } '',
    {  #24 } '',
    {  #25 } '',
    {  #26 } '',
    {  #27 } '',
    {  #28 } '',
    {  #29 } '',
    {  #30 } '',
    {  #31 } '',
    {  #32 } '',
    {! #33 } '',
    {" #34 } 'from',
    {# #35 } 'identify',
    { $ #36 } 'thesis',
    {% #37 } 'contradiction',
    {& #38 } '&',
    {' #39 } 'by',
    {( #40 } '(',
    {) #41 } ')',
    {* #42 } 'registration',
    {+ #43 } 'definition',
    {, #44 } ',',
    {- #45 } 'notation',
    {. #46 } '...',
    {/ #47 } 'proof',
    {0 #48 } '',
    {1 #49 } '',
    {2 #50 } '',
    {3 #51 } '',
    {4 #52 } '',
    {5 #53 } '',
    {6 #54 } '',
    {7 #55 } '',
    {8 #56 } '',
    {9 #57 } '',
    {: #58 } ':',
    {; #59 } ';',
    {< #60 } 'now',
    {= #61 } '=',
    {> #62 } 'end',
    {? #63 } '',
    {@ #64 } '',
    {A #65 } '',
    {B #66 } '',
    {C #67 } '',
    {D #68 } 'vocabularies', //
    {E #69 } '',
    {F #70 } '',
    {G #71 } '',
    {H #72 } '',
    {I #73 } '',
    {J #74 } '',
    {K #75 } '',
    {L #76 } '',
    {M #77 } '',
    {N #78 } '',
    {O #79 } '',
    {P #80 } '',
    {Q #81 } 'def', //
    {R #82 } '',
    {S #83 } '',
    {T #84 } '',
    {U #85 } '',
    {V #86 } '',
    {W #87 } '',
    {X #88 } 'symmetry', //
    {Y #89 } 'coherence', //
    {Z #90 } '$1`', //
    {[ #91 } '[',
    {\ #92 } '',
    {] #93 } ']',
    {^ #94 } '',
    {_ #95 } '',
    {` #96 } '',
    {a #97 } 'according',
    {b #98 } '',
    {c #99 } 'reduce',
    {d #100} '',
    {e #101} 'equals',
    {f #102} '',
    {g #103} '',
    {h #104} 'with',
    {i #105} '',
    {j #106} '',
    {k #107} '',
    {l #108} '',
    {m #109} '',
    {n #110} '',
    {o #111} '',
    {p #112} '',
    {q #113} '',
    {r #114} 'wrt',
    {s #115} '',
    {t #116} 'to',
    {u #117} '',
    {v #118} '',
    {w #119} 'when',
    {x #120} 'axiom',
    {y #121} '',
    {z #122} '',
    {  #123} '{',
    {| #124} '',
    { #125} '}',
    {~ #126} '',
    {#127} 'T127',
    {#128} '',
    {#129} 'T129',
    {#130} '',
    {#131} 'T131',
    {#132} '',
    {#133} '',
    {#134} '',
    {#135} 'correctness',
    {#136} 'T136',
    {#137} '',
    {#138} '',
    {#139} '',
    {#140} 'if',
    {#141} '',
    {#142} '',
    {#143} '',
    {#144} 'is',
    {#145} 'are',
    {#146} '',
    {#147} 'otherwise',
    {#148} '',
    {#149} '',
    {#150} '',
    {#151} '',
    {#152} 'T152',
    {#153} '',
    {#154} '',
    {#155} '',
    {#156} 'ex',
    {#157} 'for',
    {#158} '',
    {#159} 'define',
    {#160} '',
    {#161} 'being',
    {#162} 'over',
    {#163} '',
    {#164} 'canceled',
    {#165} 'do',
    {#166} 'does',
    {#167} 'or',
    {#168} 'where',
    {#169} 'non',
    {#170} 'not',
    {#171} 'cluster',
    {#172} 'attr',
    {#173} '',
    {#174} '(#',
    {#175} '#)',
    {#176} 'environ',
    {#177} '',
    {#178} 'begin',
    {#179} '',
    {#180} '',
    {#181} '',
    {#182} '',
    {#183} '',
    {#184} '',
    {#185} 'hence',
    {#186} '',
    {#187} '',
    {#188} 'hereby',
    {#189} '',
    {#190} '',
    {#191} '',
    {#192} 'then',
    {#193} '.=',
    {#194} '',
    {#195} '',
    {#196} 'synonym',
    {#197} 'antonym',
    {#198} '',
    {#199} '',
    {#200} 'let',
    {#201} 'take',
    {#202} 'assume',
    {#203} 'thus',
    {#204} 'given',
    {#205} 'suppose',
    {#206} 'consider',
    {#207} '',
    {#208} '',
    {#209} '',
    {#210} '',
    {#211} '->',
    {#212} 'as',
    {#213} 'qua',
    {#214} 'be',
    {#215} 'reserve',
    {#216} '',
    {#217} '',
    {#218} '',
    {#219} '',
    {#220} '',
    {#221} '',
    {#222} '',
    {#223} '',
    {#224} 'set',
    {#225} 'selector',
    {#226} 'cases',
    {#227} 'per',
    {#228} 'scheme',
    {#229} 'redefine',
    {#230} 'reconsider',
    {#231} 'case',
    {#232} 'prefix',
    {#233} 'the',
    {#234} 'it',
    {#235} 'all',
    {#236} 'theorem',
    {#237} 'struct',
    {#238} 'exactly',
    {#239} 'mode',
    {#240} 'iff',
    {#241} 'func',
    {#242} 'pred',
    {#243} 'implies',
    {#244} 'st',
    {#245} 'holds',
    {#246} 'provided',
    {#247} 'means',
    {#248} 'of',
    {#249} 'defpred',
    {#250} 'deffunc',
    {#251} 'such',
    {#252} 'that',
    {#253} 'aggregate',
    {#254} 'and'
  );

 CorrectnessName: array[CorrectnessKind] of string =
  ( 'correctness',
    'coherence',
    'compatibility',
    'consistency',
    'existence',
    'uniqueness',
    'reducibility'
   );

 PropertyName: array[PropertyKind] of string =
   ( '',
     'symmetry',
     'reflexivity',
     'irreflexivity',
     'associativity',
     'transitivity',
     'commutativity',
     'connectedness',
     'asymmetry',
     'idempotence',
     'involutiveness',
     'projectivity',
     'sethood',
     'abstractness'
   );

 LibraryReferenceName: array[LibraryReferenceKind] of string =
   ( 'the','def','sch');

 DirectiveName: array[DirectiveKind] of string =
  ( 'vocabularies',
    'notations',
    'definitions',
    'theorems',
    'schemes',
    'registrations',
    'constructors',
    'requirements',
    'equalities',
    'expansions'
    {,'properties'}
    );

  PlaceHolderName: array[1..10] of string =
    ( '$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10' );

 Unexpected = sErrProperty;

var
 PrevWord,CurWord,AheadWord: Token;
 PrevPos,AheadPos: Position;

procedure ReadToken;

procedure LoadPrf(const aPrfFileName:string);
procedure DisposePrf;

procedure StartScaner;

procedure InitSourceFile(const aFileName, aDctFileName:string);
procedure CloseSourceFile;
procedure InitScanning(const aFileName,aDctFileName:string);
procedure FinishScanning;

var
 gScanner: MScannPtr = nil;
 ModeMaxArgs,StructModeMaxArgs,PredMaxArgs: IntSequence;

implementation

uses mizenv;

procedure ReadToken;
begin
 PrevWord:=CurWord; PrevPos:=CurPos;
 CurWord:=AheadWord; CurPos:=AheadPos;
 {'_' is not allowed in an identifiers in the text proper}
 if (CurWord.Kind = sy_Begin)
  then gScanner^.Allowed['_']:=0;
 if (CurWord.Kind = sy_Error) and
    (CurWord.Nr = scTooLongLineErrorNr)
   then ErrImm(CurWord.Nr);
 gScanner^.GetToken;
 AheadWord.Kind:=TokenKind(gScanner^.fLexem.Kind);
 AheadWord.Nr:=gScanner^.fLexem.Nr;
 AheadWord.Spelling:=gScanner^.fStr;
 AheadPos:=gScanner^.fPos;
end;

procedure LoadPrf(const aPrfFileName:string);
 var lPrf: text;
     lModeMaxArgsSize,lStructModeMaxArgsSize,lPredMaxArgsSize,i,lInt,r: integer;
begin
 assign(lPrf,aPrfFileName+'.prf'); reset(lPrf);
 Read(lPrf,lModeMaxArgsSize,lStructModeMaxArgsSize,lPredMaxArgsSize);
 ModeMaxArgs.Init(lModeMaxArgsSize+1);
 r:=ModeMaxArgs.Insert(0);
 StructModeMaxArgs.Init(lStructModeMaxArgsSize+1);
 r:=StructModeMaxArgs.Insert(0);
 PredMaxArgs.Init(lPredMaxArgsSize+1);
 r:=PredMaxArgs.Insert(0);
 for i:=1 to lModeMaxArgsSize do
  begin Read(lPrf,lInt);
   r:=ModeMaxArgs.Insert(lInt);
  end;
 for i:=1 to lStructModeMaxArgsSize do
  begin Read(lPrf,lInt);
   r:=StructModeMaxArgs.Insert(lInt);
  end;
 for i:=1 to lPredMaxArgsSize do
  begin Read(lPrf,lInt);
   r:=PredMaxArgs.Insert(lInt);
  end;
 close(lPrf);
end;

procedure DisposePrf;
begin
 ModeMaxArgs.Done;
 PredMaxArgs.Done;
 StructModeMaxArgs.Done;
end;

procedure StartScaner;
begin
 CurPos.Line:=1; CurPos.Col:=0;
 AheadWord.Kind:=TokenKind(gScanner^.fLexem.Kind);
 AheadWord.Nr:=gScanner^.fLexem.Nr;
 AheadWord.Spelling:=gScanner^.fStr;
 AheadPos:=gScanner^.fPos;
end;

procedure InitSourceFile(const aFileName,aDctFileName:string);
begin
 new(gScanner,InitScanning(aFileName,aDctFileName));
 StartScaner;
end;

procedure CloseSourceFile;
begin
 dispose(gScanner,Done);
end;

procedure InitScanning(const aFileName,aDctFileName:string);
begin
 gScanner:=new(MScannPtr, InitScanning(aFileName,aDctFileName));
 StartScaner;
 LoadPrf(aDctFileName);
end;

procedure FinishScanning;
begin
 gScanner^.fIdents.SaveXDct(EnvFileName+'.idx');
 CloseSourceFile;
 DisposePrf;
end;

end.
