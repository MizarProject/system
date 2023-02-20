(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

(***
  This is a data structure for fast attribute/cluster rounding-up.
  ##NOTE: any changes should take into account that rounding-up can
          now recursively invoke itself (when types of terms created
          by instantiation are needed)
  ##NOTE: the current rounding-up is a bit limited. If a conditional
          cluster C is (theoretically) applicable more than once to some
         input cluster S, e.g. when S = [V1(K1), V1(K2)] and 
         C = V1(A1) -> V2(A1), the rounding-up will be used only once,
         here to add V2(K1), and not V2(K2). On the other hand, if some
         other cond. cluster add V1(K0) after this, then C will fire again,
         (supposing K0 < K1). Our algorithm should be independent of
         this limitation, since it might be fixed in the future.
         This also means that the order of condcluster applications
         matters, so our algorithm may give slightly different results than  
         the original implementation.  
  
  
  The rounding algorithm is graph-chasing, working as follows:
  
  For each adjusted attr. number A we store the set of condclusters Cl(A)
  having A in its antecedent in a global table indexed by A. We
  also keep usage counts Usage(A) for each A, which are initially 0.
  Each condcluster has associated FIRE count, which is initially set to
  the count of its antecedent cluster.
  We start with a cluster S1 and typ T, our goal is to round up S1.
  Usage(X) is increased for each X in S1.
  Each time when Usage(A) is increased,
  we decrease FIRE(X) for all X in Cl(A) (all clusters containing A).
  If FIRE(X) reaches 0, we try the SubsetOf check between nAntecedent(X)
  and S1 and the type check for T, and increase the Usage(A) for
  added attributes. This is repeated whenever FIRE(X) is =< 0 and is being
  decreased (i.e. some attribute is getting different args).
  
  
***)  
unit roundcl;

interface

// We want to use this in correl, so try to
// avoid correl's stuff in interface
uses mobjects,limits;

type
 
 // This is used for getting counts of positive and negative attrs in
 // a cluster - yes, we must deal with multiple occurrences.  
 TAttrNbrs = array [0..1] of NatFuncPtr;
 
 // ##NOTE: this is inherited from MExtList, but take care with the
 //         ancestor methods, which are not defined here.
 MCondClList = object(MExtList)
  nAttrClusters: array [0..1, 1 .. MaxAttrNbr] of NatSet;
  nInitClFire  : IntSequence;  // initial fire counts, used for
                                // lClFire initialisation in RoundUpCluster
  constructor Init(aLimit: Integer);
  destructor Done; virtual;
  procedure Insert(aItem: Pointer); virtual;
  procedure Pack; virtual;
  procedure AddExtObject; virtual;
  procedure AddExtItems; virtual;
  procedure RoundUpCluster( aCluster: Pointer; aTyp:Pointer);
 end;

implementation

uses errhan,correl,lexicon,builtin;

// Try to round up fCl and fTyp with fCond.
// fCl can become inconsistent after this
function TryRounding( fCond: CClusterPtr; fTyp: TypPtr;
                      fCl: AttrCollectionPtr): boolean;
var lTyp: TypPtr; k:integer;
begin
 TryRounding:= false;
 if not TypReachable( fCond^.nClusterType, fTyp) then exit;
 fillchar( gSubstTrm, sizeof( gSubstTrm), 0);
 if fCond^.nAntecedent^.IsSubsetOf( fCl, EsAttrRev)
    and ( not fCond^.nConsequent.Upper^.IsSubsetOf( fCl, AttrEquals) ) then
 begin
  lTyp:= fCond^.nClusterType^.WideningOf( fTyp^.CopyType);
  if lTyp <> nil then
  begin
   if CompEsTyp( fCond^.nClusterType, lTyp, false) then
     if fCond^.nClusterType^.LowerCluster^.IsSubsetOf(lTyp^.UpperCluster,EsAttrRev) then
      if CheckLociTypes( fCond^.nPrimaryList) then
   begin
    InitInst;
    fCl^.EnlargeBy( fCond^.nConsequent.Upper);
    TryRounding:= true;
    StopInst;
   end;
   dispose( lTyp, Done);
  end;
 end;
 DisposeSubstTrm;
end;

constructor MCondClList.Init(aLimit: Integer);
var i:integer;
begin
 inherited Init( aLimit);
 for i:= 1 to MaxAttrNbr do
 begin
  nAttrClusters[0][i].Init( 8, 8);
  nAttrClusters[1][i].Init( 8, 8);
 end;
 nInitClFire.Init( ALimit);
end;

destructor MCondClList.Done;
var i:integer;
begin
 for i:= 1 to MaxAttrNbr do
 begin
  nAttrClusters[0][i].Done;
  nAttrClusters[1][i].Done;
 end;
 nInitClFire.Done;
 inherited Done;
end;

procedure MCondClList.Pack;
begin ListError(coIndexExtError,0); end;

// prepare the indexing by adding the added ccluster's number into
// its antecedent's attribute slots
procedure MCondClList.AddExtObject;
var i: integer;
begin
 if fExtCount <= 0 then ListError(coIndexExtError,0);
 inc(Count);
 dec(fExtCount);
 with CClusterPtr( Items^[ Count - 1])^ do
 begin
  for i:= 0 to nAntecedent^.Count - 1 do
   with AttrPtr(nAntecedent^.Items^[i])^ do
    nAttrClusters[ fNeg][ AdjustedAttrNr].InsertElem( Count - 1);
 
  nInitClFire.Insert( nAntecedent^.Count);
 end;
end;

procedure MCondClList.AddExtItems;
begin while fExtCount > 0 do AddExtObject; end;

procedure MCondClList.Insert(aItem: Pointer);
begin
 if fExtCount <> 0 then ListError(coIndexExtError,0);
 InsertExt( aItem);
 AddExtObject;
end;

// return the counts in aCluster
// ##TODO: this is a bit silly, we could get this info
//         directly from EnlargeBy - later
procedure CountAdjAttrs( aCluster: AttrCollectionPtr; var aAttrNbrs: TAttrNbrs);
var i:integer;
begin
 aAttrNbrs[0]^.DeleteAll;  aAttrNbrs[1]^.DeleteAll;
 for i:= 0 to aCluster^.Count - 1 do
  with AttrPtr( aCluster^.Items^[i])^ do
   aAttrNbrs[ fNeg]^.Up( AdjustedAttrNr);
end;

function PopNatSet( var fStack:NatSet): integer;
begin
 if 0 > fStack.Count then fStack.NatSetError(coIndexError,0);
 PopNatSet:= fStack.Items^[ fStack.Count - 1].X;
 dec( fStack.Count);
end;

// The main rounding up procedure
procedure MCondClList.RoundUpCluster( aCluster: Pointer; aTyp:Pointer);
var
 lJobs: NatSet; // stack of condclusters for rounding
 lClFire: IntSequence;  // fire counts for clusters
 lOldAttrNbrs: TAttrNbrs;// old counts for attributes
 lNewAttrNbrs: TAttrNbrs;// new counts for attributes
 
// Put the counts of positive and negative attrs in aCluster
// to lNewAttrNbrs, then compare them with lOldAttrNbrs and update it,
// decreasing the lClFire count when st. was added, and inserting
// the firing conditional clusters into lJobs.
procedure HandleUsageAndFire;
var
 lNeg,lPos,i,k,lAdjNr,lVal,lClNr:integer;
 lTmp: TAttrNbrs;
begin
 CountAdjAttrs( AttrCollectionPtr( aCluster), lNewAttrNbrs);
 for lNeg:= 0 to 1 do
  for i:= 0 to lNewAttrNbrs[ lNeg]^.Count - 1 do
  begin
   lAdjNr:= lNewAttrNbrs[ lNeg]^.Items^[i].X;
   lVal:= lNewAttrNbrs[ lNeg]^.Items^[i].Y;
   if ( not lOldAttrNbrs[ lNeg]^.SearchPair( lAdjNr, lPos) )
      or ( lOldAttrNbrs[ lNeg]^.Items^[ lPos].Y < lVal ) then
    for k:= 0 to nAttrClusters[ lNeg][ lAdjNr].Count - 1 do
    begin
     lClNr:= nAttrClusters[ lNeg][ lAdjNr].Items^[ k].X;
     dec( lClFire.fList^[ lClNr]);
     if 0 >= lClFire.fList^[ lClNr] then lJobs.InsertElem( lClNr);
    end;
  end;
 lTmp:= lOldAttrNbrs;
 lOldAttrNbrs:= lNewAttrNbrs;
 lNewAttrNbrs:= lTmp;
end;

var lLast,j: integer;
begin

 // initialize the fires and the usage
 lClFire.Init( nInitClFire.fCapacity);
 lClFire.fCount:= nInitClFire.fCount;
 move( nInitClFire.fList^, lClFire.fList^,
       nInitClFire.fCount * SizeOf(Integer));

 lJobs.Init( 32, 32);
 for j:= 0 to 1 do lOldAttrNbrs[j]:= new( NatFuncPtr, InitNatFunc( 32, 32));
 for j:= 0 to 1 do lNewAttrNbrs[j]:= new( NatFuncPtr, InitNatFunc( 32, 32));
 
 // insert those with empty nAntecedent
 for j:= 0 to lClFire.fCount - 1 do
  if lClFire.fList^[ j] = 0 then lJobs.InsertElem( j);
 
 // initialize usage with aCluster
 HandleUsageAndFire;
 
 while AttrCollectionPtr(aCluster)^.fConsistent and ( lJobs.Count > 0) do
 begin
  lLast:= PopNatSet( lJobs);
  if TryRounding( Items^[ lLast], aTyp, aCluster) then
   HandleUsageAndFire;
 end;

 lJobs.Done;
 lClFire.Done;
 for j:= 0 to 1 do dispose( lOldAttrNbrs[j], Done);
 for j:= 0 to 1 do dispose( lNewAttrNbrs[j], Done);
end;  
  
end.
