(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

// ##TODO: kill this unit completely, it is replaced by OutMMLFileObj

unit outlibr;

interface

procedure ReadSignatureList;

// commented to find out wher it is yet needed
// var OutLibrFile: MizOutStream;

implementation

uses errhan,mizenv,correl,mobjects,inout,inoutmml,lexicon
{$IFDEF MDEBUG}
    ,info
{$ENDIF};

procedure ReadSignatureList;
 var i,lImpSgnNbr: integer; s: string; lInFile: MizInStream;
begin
 FileExam(EnvFileName+'.sgl');
 lInFile.OpenFile(EnvFileName+'.sgl');
 lInFile.InInt(lImpSgnNbr);
 gImpSgnNames.Init(lImpSgnNbr);
 for i:=1 to lImpSgnNbr do
  begin lInFile.InString(s);
   gImpSgnNames.Insert(new(MStrPtr,init(s)));
  end;
 lInFile.Done;
end;


end.
