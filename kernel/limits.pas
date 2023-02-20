(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit limits;
interface

const
  MaxConstInt = 32767; {maximal signed 16-bit integer}

  MaxImpConstrNbr = 400;
  MaxImpNbr = 300;

  MaxAttrPattNbr = 400;
  MaxStructPattNbr = 40;
  MaxPrefixPattNbr = 80;
  MaxSelectPattNbr = 200;

  MaxAttrNbr = 1000;

  MaxArgListNbr   =   20;
     { Maksymalna wartosc n w termach postaci
        L0 F1 L1 ... L(n-1) Fn Ln,
        gdzie Li jest lista argumetow, a Fi symbolem funkcyjnym.
       W przypadku przekroczenia nalezy uzyc nawiasow okraglych.
       Uzywana przez READCSTR.
     }

  MaxArgNbr       =   13;
     { Maksymalna ilosc argumentow konstruktorow (modow (w tym struktur),
       funkcji, predykatow (takze lokalnych)).
       Przekroczenia powoduje przerwanie przetwarzania !
       Uzywana przez ANAL CHEC PREP CORREL
     }

  MaxInstTrmNbr   =   26;
     { Maksymalna ilosc podstawianych termow, musi byc o co najmniej 1
       wieksza od MaxArgNbr.
       Przekroczenie jej jest sygnalem bledu w systemie.
       Uzywana przez CORREL CHEC
     }

  MaxInstNbr      = 6000;
     { Maksymalna ilosc podstawien w ineferencji,
       Przekroczenie jest lokalne w inferencji.
       Uzywana przez CHEC
     }

  MaxDefLabNbr    =   20;
     { Maksymalna ilosc etykiet definicyjnych w jednym zestawie
       definicji.
       Przekroczenie powoduje ignrowanie etykiet. Nalezy zamknac i
       otworzyc definicje.
       Uzywana przez MIZACT
     }

  MaxFreeNbr      =   40;
     { Maksymalna ilosc zmiennych wolnych w jednym obszarze kwantyfikowania.
       Przekroczenie powoduje zignorowanie nadmiarowych zmiennych.
       Nalezy zwiazac zmienne wolne lub uzyc definicji stalej dla  termow
       z operatorem Fraenkla.
       Uzywana przez  VARHAN i schmematyzator !
     }

  MaxFreeRangeNbr =  22;
     { Maksymalna ilosc zasiegow zmiennych wolnych w jednym obszarze
       kwantyfikowania.
       Przekroczenie powoduje przerwanie przetwarzania tekstu.
       Poprawienie takie jak przy przekroczeniu MaxFreeNbr
       Uzywana przez VARHAN
     }

  MaxFuncPattNbr  =  1000;


  MaxFuncNbr      =  1500;
     { Maksymalna ilosc funkcji publicznych w artykule, lacznie z pobranymi
       z biblioteki.
       Przekroczenie powoduje przerwanie przetwarzania.
       Nie poprawiac, czekac na nowa wersje Mizara.
       Uzywana przez ANAL CORREL IDENTS L_REAS REAS SIGNATUR
     }

  MaxFuncVarNbr   =   50;
     { Maksymalna ilosc funkcji prywatnych.
       Przekroczenie powoduje przerwanie przetwarzania tekstu.

       Uzywana przez ANAL IDENTS PREP CORREL VARHAN
     }

  MaxImpSgnNbr    =  400;
     { Maksymalna ilosc importowanych plikow sygnatur.
       Przekroczenie powoduje zignorowanie nadmiarowych dyrektyw.

     }

  MaxLabNbr       =  3000;
     { Maksymalna ilosc jednoczesnie dostepnych etykiet.
       Przekroczenie powoduje przerwanie przetwarzania tekstu.
       Wiecej haczyc.
       Uzywana przez PREP
     }

  MaxModeNbr      =   200;
     { Maksymalna ilosc modow w tekscie, lacznie z modami pobranymi
       z biblioteki.
       Przekroczenie powoduje przerwanie przetwarzania.
       Uzywana przez ANAL CORREL L_REAS REAS SIGNATUR
     }

  MaxModePattNbr  =  200;

  MaxPredPattNbr  =   400;

  MaxPredNbr      =   300;
     { Maksymalna ilosc predykatow publicznych w artykule, lacznie z pobranymi
       z biblioteki.
       Przekroczenie powoduje przerwanie przetwarzania.
       Nie poprawiac, czekac na nowa wersje Mizara.
       Uzywana przez ANAL CORREL L_REAS REAS READEF SIGNATUR
     }

  MaxPredVarNbr   =   40;
    { Maksymalna ilosc funkcji prywatnych.
       Przekroczenie powoduje przerwanie przetwarzania tekstu.

       Uzywana przez ANAL PREP CORREL VARHAN
     }

  MaxRefNbr       =  100;
     { Maksymalna laczna ilosc referencji w artykule.
       Przekroczenie powoduje przerwanie przetwarzania.

       Uzywana przez MIZACT
     }

  MaxPremNbr      =   MaxRefNbr+2;
     { Maksymalna ilosc przeslanek w inferencji, lacznie z zahaczniem
       i teza.
       Przekroczenie powoduje odrzucenie inferencji.
       Dodane 2, bo trzeba uwzglednic zahaczenie, a i negacja
       tezy liczy sie jako przeslanka.
       Uzywana przez PREP
     }

  MaxResFreeNbr   =   90;
     { Maksymalna ilosc zmiennych wolnych we wszystkich rezerwacjach.
       Przekroczenie nie jest kontrolowane !!!!!
       Uzywana przez VARHAN
     }

  MaxResIdNbr     =   90;
     { Maksymalna ilosc rezerwowanych identyfikatorow w calum artykule.
       Przekroczenie powoduje przerwanie przetwarzania artykulu.
       Uzywana przez VARHAN
     }

  MaxResNbr       =   50;
     { Maksymalna ilosc rezerwacji w artykule.
       Przekroczenie powoduje przerwanie przetwarzania artykulu.

       Uzywana przez ANAL VARHAN
     }

  MaxSchFuncNbr   =   10;
     { Maksymalna ilosc funkcji schematowych w jednym schemacie.
       Przekroczenie powoduje odrzucenie schematu.

       Uzywana przez PREP
     }

  MaxSchPredNbr   =   10;
     { Maksymalna ilosc predykatow schematowych w jednym schemacie.
       Przekroczenie powoduje odrzucenie schematu.

       Uzywana przez PREP
     }

  MaxSelectNbr    =   100;
     { Maksymalna ilosc funkcji selektorowych w artykule.
       Przekroczenie powoduje przerwanie przetwarzania.

       Uzywana przez ANAL SIGNATUR CORREL IDENTS
     }

  MaxStructNbr    =   40;
     { Maksymalna ilosc struktur w artykule.
       Przekroczenie powoduje przerwanie przetwarzania.

       Uzywana przez ANAL SIGNATUR IDENTS
     }

  MaxSubTermNbr   =   40;
     { Maksymalna ilosc podtermow jednoczesnie obliczanych przy parsing.
       Przekroczenie powoduje przerwanie przetwarzania.
       Wiecej nawiasow okraglych lub zdefiniowac stale.
       Uzywana przez PARSER
     }

  MaxTheoNbr      =  2000;
     { Maksymalna ilosc twierdzen na jednym pliku. Powinna byc mniejsza
       od maksymalnej mocy zbioru.
       Przekroczenie powoduje odrzucenie uzasadnienia.
       Uzywana przez PREP
     }

  MaxTrmNbr       =  4000;
     { Maksymalna ilosc termow stalych w jednej inferencji.
       Przekroczenie powoduje odrzucenie inferencji.
       Uzywana przez CHEC IDENTS
     }

  MaxVarNbr       =  650;
     { Maksymalna ilosc zmiennych dostepnych jednoczesnie. Lacznie
       z zmiennymi kwantyfikowanymi, wygenerowanymi przez rezoner
       i prechecker.
       Przekroczenie powoduje rozne reakcje zaleznie od miejsce
       przekroczenia.
       Uzywana przez ANAL CHEC IDENTS L_REAS PARSER PREP REAS CORREL VARHAN
     }

  MaxElemNbr      =   20;
     { Maksymalna ilosc elementow na liscie.
       Przekroczenie powoduje przerwanie przetwarzania.
       Uzywana przez ABSTRACT
     }

  MaxInferConstNbr = 3500;
     { Maksymalna ilosc kolekcjonowanych termow funktorowych
     }

type
 IndexSort = (ArgListIndex,
              SubTermIndex,
              LabIndex,
              RefIndex,
              FreeRangeIndex,
              ResIndex,
              ResFreeIndex,
              VarIndex,
              ResIdIndex,
              FuncVarIndex,
              PredVarIndex,
              ImpSgnIndex,
              PrimIndex, {do wyrzucenia, nie jest uzywany pozostawiono
                          ze wzledu na synchronizacje numerow bledow}
              PredIndex,
              PredPattIndex,
              FuncIndex,
              FuncPattIndex,
              ModeIndex,
              ModePattIndex,
              AttrIndex,
              AttrPattIndex,
              StructIndex,
              StructPattIndex,
              SelectIndex,
              SelectPattIndex,
              RegClusterIndex, {do wyrzucenia, nie jest uzywany pozostawiono
                                ze wzledu na synchronizacje numerow bledow}
              ArgIndex,
              InferConstIndex
             );

procedure IncIndex(var fIndex:integer; fSort: IndexSort);

const
  IndexLimit : array[IndexSort] of integer
   = ( MaxArgListNbr,       { 911, pozycja bledu kretynska }
       MaxSubTermNbr,       { 912 }
       MaxLabNbr,           { 913 }
       MaxRefNbr,           { 914 }
       MaxFreeRangeNbr,     { 915 }
       MaxResNbr,           { 916 }
       MaxResFreeNbr,       { 917 }
       MaxVarNbr,           { 918 }
       MaxResIdNbr,         { 919 }
       MaxFuncVarNbr,       { 920 }
       MaxPredVarNbr,       { 921 }
       MaxImpSgnNbr,        { 922 }
       0,                   { 923 blad nie wystepuje, zostawiono ze wzgledu
                                  na synchronizacje numerow bledow}
       MaxPredNbr,          { 924 }
       MaxPredPattNbr,      { 925 }
       MaxFuncNbr,          { 926 }
       MaxFuncPattNbr,      { 927 }
       MaxModeNbr,          { 928 }
       MaxModePattNbr,      { 929 }
       MaxAttrNbr,          { 930 }
       MaxAttrPattNbr,      { 931 }
       MaxStructNbr,        { 932 }
       MaxStructPattNbr,    { 933 }
       MaxSelectNbr,        { 934 }
       MaxSelectPattNbr,    { 935 }
       0,                   { 936 blad nie wystepuje, zostawiono ze wzgledu
                                  na synchronizacje numerow bledow}
       MaxArgNbr,           { 937 }
       MaxInferConstNbr     { 938 }
     );
  
implementation

uses errhan;

  
procedure IncIndex(var fIndex:integer; fSort: IndexSort);
begin if fIndex >= IndexLimit[fSort] then OverflowError(911+ord(fSort));
 inc(fIndex);
end;

end.

