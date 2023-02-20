(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

unit xml_dict;

interface

uses mobjects;

// known (and only allowed) XML elements
type
 XMLElemKind =
 (
   elUnknown,
   elAdjective,
   elAdjectiveCluster,
   elArticleID,
   elAncestors,
   elArguments,
   elBlock,
   elConditions,
   elCorrectnessConditions,
   elDefiniens,
   elDirective,
   elEnviron,
   elEquality,
   elFieldSegment,
   elFormat,
   elFormats,
   elIdent,
   elItem,
   elIterativeStep,
   elLabel,
   elLink,
   elLoci,
   elLociEquality,
   elLocus,
   elNegatedAdjective,
   elPartialDefiniens,
   elPriority,
   elProposition,
   elProvisionalFormulas,
   elRedefine,
   elRightCircumflexSymbol,
   elSchematicVariables,
   elScheme,
   elSelector,
   elSetMember,
   elSkippedProof,
   elSymbol,
   elSymbolCount,
   elSymbols,
   elSubstitution,
   elTypeSpecification,
   elTypeList,
   elVariable,
   elVariables,
   elVocabularies,
   elVocabulary
 );

// known XML attributes
 XMLAttrKind =
 (
   atUnknown,
   atAid,
   atArgNr,
   atArticleId,
   atArticleExt,
   atCol,
   atCondition,
   atConstrNr,
   atIdNr,
   atInfinitive,
   atKind,
   atLabelNr,
   atLeftArgNr,
   atLine,
   atMizfiles,
   atName,
   atNegated,
   atNr,
   atNumber,
   atOrigin,
   atPosLine,
   atPosCol,
   atPriority,
   atProperty,
   atRightSymbolNr,
   atSchNr,
   atSerialNr,
   atShape,
   atSpelling,
   atSymbolNr,
   atValue,
   atVarNr,
   atVarSort,
   atX,
   atX1,
   atX2,
   atY,
   atY1,
   atY2
 );

const
 XMLElemName: array[XMLElemKind] of string =
 (
   'Unknown',
   'Adjective',
   'Adjective-Cluster',
   'ArticleID',
   'Ancestors',
   'Arguments',
   'Block',
   'Conditions',
   'CorrectnessConditions',
   'Definiens',
   'Directive',
   'Environ',
   'Equality',
   'Field-Segment',
   'Format',
   'Formats',
   'Ident',
   'Item',
   'Iterative-Step',
   'Label',
   'Link',
   'Loci',
   'LociEquality',
   'Locus',
   'NegatedAdjective',
   'Partial-Definiens',
   'Priority',
   'Proposition',
   'Provisional-Formulas',
   'Redefine',
   'Right-Circumflex-Symbol',
   'Schematic-Variables',
   'Scheme',
   'Selector',
   'SetMember',
   'elSkippedProof',
   'Symbol',
   'SymbolCount',
   'Symbols',
   'Substitution',
   'Type-Specification',
   'Type-List',
   'Variable',
   'Variables',
   'Vocabularies',
   'Vocabulary'
  );

XMLAttrName: array[XMLAttrKind] of string =
 (
   'unknown',
   'aid',
   'argnr',
   'articleid',
   'articleext',
   'col',
   'condition',
   'constrnr',
   'idnr',
   'infinitive',
   'kind',
   'labelnr',
   'leftargnr',
   'line',
   'mizfiles',
   'name',
   'negated',
   'nr',
   'number',
   'origin',
   'posline',
   'poscol',
   'priority',
   'property',
   'rightsymbolnr',
   'schnr',
   'serialnr',
   'shape',
   'spelling',
   'symbolnr',
   'value',
   'varnr',
   'varsort',
   'x',
   'x1',
   'x2',
   'y',
   'y1',
   'y2'
 );


implementation

end.

