/*
 * parser.ts
 * Copyright (c) Microsoft Corporation.
 * Licensed under the MIT license.
 * Author: Eric Traut
 *
 * Based on code from python-language-server repository:
 *  https://github.com/Microsoft/python-language-server
 *
 * Parser for the Python language. Converts a stream of tokens
 * into an abstract syntax tree (AST).
 */

import Char from 'typescript-char';

import { IPythonMode } from '../analyzer/sourceFile';
import { appendArray } from '../common/collectionUtils';
import { assert, assertDefined } from '../common/debug';
import { Diagnostic, DiagnosticAddendum } from '../common/diagnostic';
import { DiagnosticSink } from '../common/diagnosticSink';
import { getFileExtension, stripFileExtension } from '../common/pathUtils';
import { convertOffsetsToRange, convertPositionToOffset } from '../common/positionUtils';
import { latestStablePythonVersion, PythonVersion } from '../common/pythonVersion';
import { TextRange } from '../common/textRange';
import { TextRangeCollection } from '../common/textRangeCollection';
import { timingStats } from '../common/timing';
import { Localizer } from '../localization/localize';
import {
    ArgumentCategory,
    ArgumentNode,
    AssertNode,
    AssignmentExpressionNode,
    AssignmentNode,
    AugmentedAssignmentNode,
    AwaitNode,
    BinaryOperationNode,
    BreakNode,
    CAddressOfNode,
    CallNode,
    CaseNode,
    CBlockTrailNode,
    CBlockTrailType,
    CCallbackNode,
    CCastNode,
    CClassExtNode,
    CDefineNode,
    CEnumNode,
    CExternNode,
    CFunctionNode,
    CGilNode,
    ClassNode,
    CNewNode,
    ConstantNode,
    ContinueNode,
    CParameterNode,
    CPropertyNode,
    CSizeOfNode,
    CStructNode,
    CStructType,
    CTrailType,
    CTupleTypeNode,
    CTypeDefNode,
    CTypeNode,
    CTypeTrailNode,
    CVarTrailNode,
    DecoratorNode,
    DelNode,
    DictionaryEntryNode,
    DictionaryExpandEntryNode,
    DictionaryKeyEntryNode,
    DictionaryNode,
    EllipsisNode,
    ErrorExpressionCategory,
    ErrorNode,
    ExceptNode,
    ExpressionNode,
    extendRange,
    FormatStringNode,
    ForNode,
    FunctionAnnotationNode,
    FunctionNode,
    getNextNodeId,
    GlobalNode,
    IfNode,
    ImportAsNode,
    ImportFromAsNode,
    ImportFromNode,
    ImportNode,
    IndexNode,
    LambdaNode,
    ListComprehensionForIfNode,
    ListComprehensionForNode,
    ListComprehensionIfNode,
    ListComprehensionNode,
    ListNode,
    MatchNode,
    MemberAccessNode,
    ModuleNameNode,
    ModuleNode,
    NameNode,
    NonlocalNode,
    NumberNode,
    ParameterCategory,
    ParameterNode,
    ParseNode,
    ParseNodeType,
    PassNode,
    PatternAsNode,
    PatternAtomNode,
    PatternCaptureNode,
    PatternClassArgumentNode,
    PatternClassNode,
    PatternLiteralNode,
    PatternMappingEntryNode,
    PatternMappingExpandEntryNode,
    PatternMappingKeyEntryNode,
    PatternMappingNode,
    PatternSequenceNode,
    PatternValueNode,
    RaiseNode,
    ReturnNode,
    SetNode,
    SliceNode,
    StatementListNode,
    StatementNode,
    StringListNode,
    StringNode,
    SuiteNode,
    TernaryNode,
    TryNode,
    TupleNode,
    TypeAliasNode,
    TypeAnnotationNode,
    TypeParameterCategory,
    TypeParameterListNode,
    TypeParameterNode,
    UnaryOperationNode,
    UnpackNode,
    WhileNode,
    WithItemNode,
    WithNode,
    YieldFromNode,
    YieldNode,
} from './parseNodes';
import * as StringTokenUtils from './stringTokenUtils';
import { CppOperatorSymbol, Tokenizer, TokenizerOutput } from './tokenizer';
import {
    DedentToken,
    IdentifierToken,
    IndentToken,
    KeywordToken,
    KeywordType,
    NumberToken,
    OperatorToken,
    OperatorType,
    softKeywords,
    StringToken,
    StringTokenFlags,
    Token,
    TokenType,
} from './tokenizerTypes';
// ! Cython
import { numericModifiers, varModifiers } from './tokenizerTypes';

interface ListResult<T> {
    list: T[];
    trailingComma: boolean;
    parseError?: ErrorNode | undefined;
}

interface SubscriptListResult {
    list: ArgumentNode[];
    trailingComma: boolean;
}

// ! Cython
interface CDeclResult {
    node: ParseNode;
    pointers: boolean;
}

export class ParseOptions {
    constructor() {
        this.isStubFile = false;
        this.pythonVersion = latestStablePythonVersion;
        this.reportInvalidStringEscapeSequence = false;
        this.skipFunctionAndClassBody = false;
        this.ipythonMode = IPythonMode.None;
        this.reportErrorsForParsedStringContents = false;
    }

    isStubFile: boolean;
    pythonVersion: PythonVersion;
    reportInvalidStringEscapeSequence: boolean;
    skipFunctionAndClassBody: boolean;
    ipythonMode: IPythonMode;
    reportErrorsForParsedStringContents: boolean;
}

export interface ParseResults {
    text: string;
    parseTree: ModuleNode;
    importedModules: ModuleImport[];
    futureImports: Map<string, boolean>;
    tokenizerOutput: TokenizerOutput;
    containsWildcardImport: boolean;
    typingSymbolAliases: Map<string, string>;
}

export interface ParseExpressionTextResults {
    parseTree?: ExpressionNode | FunctionAnnotationNode | undefined;
    lines: TextRangeCollection<TextRange>;
    diagnostics: Diagnostic[];
}

export interface ModuleImport {
    nameNode: ModuleNameNode;
    leadingDots: number;
    nameParts: string[];

    // Used for "from X import Y" pattern. An empty
    // array implies "from X import *".
    importedSymbols: string[] | undefined;

    // ! Cython
    isCython?: boolean; // false: python; true: cython; undefined: unknown
    cythonExt?: string;
}

export interface ArgListResult {
    args: ArgumentNode[];
    trailingComma: boolean;
}

const enum ParseTextMode {
    Expression,
    VariableAnnotation,
    FunctionAnnotation,
}

// Limit the max child node depth to prevent stack overflows.
const maxChildNodeDepth = 256;

export class Parser {
    private _fileContents?: string;
    private _tokenizerOutput?: TokenizerOutput;
    private _tokenIndex = 0;
    private _areErrorsSuppressed = false;
    private _parseOptions: ParseOptions = new ParseOptions();
    private _diagSink: DiagnosticSink = new DiagnosticSink();
    private _isInLoop = false;
    private _isInFunction = false;
    private _isInFinally = false;
    private _isParsingTypeAnnotation = false;
    private _isParsingIndexTrailer = false;
    private _isParsingQuotedText = false;
    private _futureImportMap = new Map<string, boolean>();
    private _importedModules: ModuleImport[] = [];
    private _containsWildcardImport = false;
    private _assignmentExpressionsAllowed = true;
    private _typingImportAliases: string[] = [];
    private _typingSymbolAliases: Map<string, string> = new Map<string, string>();

    // ! Cython
    private _isParsingCExtern = false;
    private _isParsingCStruct = false;
    private _isParsingCFused = false;
    private _isParsingCClass = false;

    parseSourceFile(fileContents: string, parseOptions: ParseOptions, diagSink: DiagnosticSink): ParseResults {
        timingStats.tokenizeFileTime.timeOperation(() => {
            this._startNewParse(fileContents, 0, fileContents.length, parseOptions, diagSink);
        });

        // ! Cython
        // Always allow forward references
        this._futureImportMap.set('annotations', true);

        const moduleNode = ModuleNode.create({ start: 0, length: fileContents.length });

        timingStats.parseFileTime.timeOperation(() => {
            while (!this._atEof()) {
                if (!this._consumeTokenIfType(TokenType.NewLine)) {
                    // Handle a common error case and try to recover.
                    const nextToken = this._peekToken();
                    if (nextToken.type === TokenType.Indent) {
                        this._getNextToken();
                        const indentToken = nextToken as IndentToken;
                        if (indentToken.isIndentAmbiguous) {
                            this._addError(Localizer.Diagnostic.inconsistentTabs(), indentToken);
                        } else {
                            this._addError(Localizer.Diagnostic.unexpectedIndent(), nextToken);
                        }
                    }

                    const statement = this._parseStatement();
                    if (!statement) {
                        // Perform basic error recovery to get to the next line.
                        this._consumeTokensUntilType([TokenType.NewLine]);
                    } else {
                        statement.parent = moduleNode;
                        moduleNode.statements.push(statement);
                    }
                }
            }
        });

        assert(this._tokenizerOutput !== undefined);
        return {
            text: fileContents,
            parseTree: moduleNode,
            importedModules: this._importedModules,
            futureImports: this._futureImportMap,
            tokenizerOutput: this._tokenizerOutput!,
            containsWildcardImport: this._containsWildcardImport,
            typingSymbolAliases: this._typingSymbolAliases,
        };
    }

    parseTextExpression(
        fileContents: string,
        textOffset: number,
        textLength: number,
        parseOptions: ParseOptions,
        parseTextMode = ParseTextMode.Expression,
        initialParenDepth = 0,
        typingSymbolAliases?: Map<string, string>
    ): ParseExpressionTextResults {
        const diagSink = new DiagnosticSink();
        this._startNewParse(fileContents, textOffset, textLength, parseOptions, diagSink, initialParenDepth);

        if (typingSymbolAliases) {
            this._typingSymbolAliases = new Map<string, string>(typingSymbolAliases);
        }

        let parseTree: ExpressionNode | FunctionAnnotationNode | undefined;
        if (parseTextMode === ParseTextMode.VariableAnnotation) {
            this._isParsingQuotedText = true;
            parseTree = this._parseTypeAnnotation();
        } else if (parseTextMode === ParseTextMode.FunctionAnnotation) {
            this._isParsingQuotedText = true;
            parseTree = this._parseFunctionTypeAnnotation();
        } else {
            const exprListResult = this._parseTestOrStarExpressionList(
                /* allowAssignmentExpression */ false,
                /* allowMultipleUnpack */ true
            );
            if (exprListResult.parseError) {
                parseTree = exprListResult.parseError;
            } else {
                if (exprListResult.list.length === 0) {
                    this._addError(Localizer.Diagnostic.expectedExpr(), this._peekToken());
                }
                parseTree = this._makeExpressionOrTuple(exprListResult, /* enclosedInParens */ false);
            }
        }

        if (this._peekTokenType() === TokenType.NewLine) {
            this._getNextToken();
        }

        if (!this._atEof()) {
            this._addError(Localizer.Diagnostic.unexpectedExprToken(), this._peekToken());
        }

        return {
            parseTree,
            lines: this._tokenizerOutput!.lines,
            diagnostics: diagSink.fetchAndClear(),
        };
    }

    private _startNewParse(
        fileContents: string,
        textOffset: number,
        textLength: number,
        parseOptions: ParseOptions,
        diagSink: DiagnosticSink,
        initialParenDepth = 0
    ) {
        this._fileContents = fileContents;
        this._parseOptions = parseOptions;
        this._diagSink = diagSink;

        // Tokenize the file contents.
        const tokenizer = new Tokenizer();
        this._tokenizerOutput = tokenizer.tokenize(
            fileContents,
            textOffset,
            textLength,
            initialParenDepth,
            this._parseOptions.ipythonMode
        );
        this._tokenIndex = 0;
    }

    // stmt: simple_stmt | compound_stmt
    // compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt
    //   | funcdef | classdef | decorated | async_stmt
    private _parseStatement(): StatementNode | ErrorNode | undefined {
        // Handle the errant condition of a dedent token here to provide
        // better recovery.
        if (this._consumeTokenIfType(TokenType.Dedent)) {
            this._addError(Localizer.Diagnostic.unexpectedUnindent(), this._peekToken());
        }

        switch (this._peekKeywordType()) {
            case KeywordType.If:
                return this._parseIfStatement();

            case KeywordType.While:
                return this._parseWhileStatement();

            case KeywordType.For:
                return this._parseForStatement();

            case KeywordType.Try:
                return this._parseTryStatement();

            case KeywordType.With:
                return this._parseWithStatement();

            case KeywordType.Def:
                return this._parseFunctionDef();

            case KeywordType.Class:
                return this._parseClassDef();

            case KeywordType.Async:
                return this._parseAsyncStatement();

            case KeywordType.Match: {
                // Match is considered a "soft" keyword, so we will treat
                // it as an identifier if it is followed by an unexpected
                // token.
                const peekToken = this._peekToken(1);
                let isInvalidMatchToken = false;

                if (
                    peekToken.type === TokenType.Colon ||
                    peekToken.type === TokenType.Semicolon ||
                    peekToken.type === TokenType.Comma ||
                    peekToken.type === TokenType.Dot ||
                    peekToken.type === TokenType.NewLine ||
                    peekToken.type === TokenType.EndOfStream
                ) {
                    isInvalidMatchToken = true;
                } else if (peekToken.type === TokenType.Operator) {
                    const operatorToken = peekToken as OperatorToken;
                    if (
                        operatorToken.operatorType !== OperatorType.Multiply &&
                        operatorToken.operatorType !== OperatorType.Subtract
                    ) {
                        isInvalidMatchToken = true;
                    }
                }

                if (!isInvalidMatchToken) {
                    // Try to parse the match statement. If it doesn't appear to
                    // be a match statement, treat as a non-keyword and reparse.
                    const matchStatement = this._parseMatchStatement();
                    if (matchStatement) {
                        return matchStatement;
                    }
                }
                break;
            }
        }

        if (this._peekOperatorType() === OperatorType.MatrixMultiply) {
            return this._parseDecorated();
        }

        // ! Cython
        let cythonStatement: StatementNode | undefined = undefined;
        switch (this._peekKeywordType()) {
            case KeywordType.Ctypedef:
                cythonStatement = this._parseCTypeDef();
                break;
            case KeywordType.Cdef:
                cythonStatement = this._parseCDef();
                break;
            case KeywordType.Cpdef:
                cythonStatement = this._parseCpdef();
                break;
            case KeywordType.DEF:
                cythonStatement = this._parseCDefine();
                break;
            case KeywordType.IF:
                cythonStatement = this._parseCIfStatement();
                break;
            case KeywordType.Include:
                cythonStatement = this._parseInclude();
                break;
        }
        if (cythonStatement) {
            if (CFunctionNode.isInstance(cythonStatement) && cythonStatement.isForwardDeclaration) {
                this._expectNewLine();
                this._consumeTokenIfType(TokenType.NewLine);
            }
            return cythonStatement;
        }
        if (this._isParsingCClass) {
            // Check if this could be a legacy property declaration
            const propToken = this._peekToken();
            const ident = this._peekToken(1);
            const colon = this._peekToken(2);
            if (
                propToken.type === TokenType.Identifier &&
                (propToken as IdentifierToken).value === 'property' &&
                (ident.type === TokenType.Identifier || this._softKeywordToIdentifier(ident)) &&
                colon.type === TokenType.Colon
            ) {
                return this._parseCLegacyProperty();
            }
        }

        return this._parseSimpleStatement();
    }

    // async_stmt: 'async' (funcdef | with_stmt | for_stmt)
    private _parseAsyncStatement(): StatementNode | undefined {
        const asyncToken = this._getKeywordToken(KeywordType.Async);

        switch (this._peekKeywordType()) {
            case KeywordType.Def:
                return this._parseFunctionDef(asyncToken);

            case KeywordType.With:
                return this._parseWithStatement(asyncToken);

            case KeywordType.For:
                return this._parseForStatement(asyncToken);
        }

        this._addError(Localizer.Diagnostic.unexpectedAsyncToken(), asyncToken);

        return undefined;
    }

    // type_alias_stmt: "type" name [type_param_seq] = expr
    private _parseTypeAliasStatement(): TypeAliasNode {
        const typeToken = this._getKeywordToken(KeywordType.Type);

        if (!this._parseOptions.isStubFile && this._getLanguageVersion() < PythonVersion.V3_12) {
            this._addError(Localizer.Diagnostic.typeAliasStatementIllegal(), typeToken);
        }

        const nameToken = this._getTokenIfIdentifier();
        assert(nameToken !== undefined);
        const name = NameNode.create(nameToken);

        let typeParameters: TypeParameterListNode | undefined;
        if (this._peekToken().type === TokenType.OpenBracket) {
            typeParameters = this._parseTypeParameterList();
        }

        const assignToken = this._peekToken();
        if (
            assignToken.type !== TokenType.Operator ||
            (assignToken as OperatorToken).operatorType !== OperatorType.Assign
        ) {
            this._addError(Localizer.Diagnostic.expectedEquals(), assignToken);
        } else {
            this._getNextToken();
        }

        const expression = this._parseOrTest();

        return TypeAliasNode.create(typeToken, name, expression, typeParameters);
    }

    // type_param_seq: '[' (type_param ',')+ ']'
    // ! Cython
    private _parseTypeParameterList(isCppClass = false): TypeParameterListNode {
        const typeVariableNodes: TypeParameterNode[] = [];

        const openBracketToken = this._getNextToken();
        assert(openBracketToken.type === TokenType.OpenBracket);

        while (true) {
            const firstToken = this._peekToken();

            if (firstToken.type === TokenType.CloseBracket) {
                if (typeVariableNodes.length === 0) {
                    this._addError(Localizer.Diagnostic.typeParametersMissing(), this._peekToken());
                }
                break;
            }

            const typeVarNode = this._parseTypeParameter();
            if (!typeVarNode) {
                break;
            }

            // ! Cython
            // Allow default template arguments
            // TODO: Consume for now. Need to implement evaluation
            if (isCppClass && this._consumeTokenIfOperator(OperatorType.Assign)) {
                let defaultValue: ExpressionNode | ParameterNode;
                const defaultToken = this._peekToken();
                if (this._consumeTokenIfOperator(OperatorType.Multiply)) {
                    defaultValue = ParameterNode.create(defaultToken, ParameterCategory.Simple);
                } else {
                    defaultValue = this._parseTestExpression(/* allowAssignmentExpression */ false);
                }
                typeVarNode.defaultValue = defaultValue;
                defaultValue.parent = typeVarNode;
                extendRange(typeVarNode, defaultValue);
            }

            typeVariableNodes.push(typeVarNode);

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                break;
            }
        }

        const closingToken = this._peekToken();
        if (closingToken.type !== TokenType.CloseBracket) {
            this._addError(Localizer.Diagnostic.expectedCloseBracket(), this._peekToken());
            this._consumeTokensUntilType([TokenType.NewLine, TokenType.CloseBracket, TokenType.Colon]);
        } else {
            this._getNextToken();
        }

        return TypeParameterListNode.create(openBracketToken, closingToken, typeVariableNodes);
    }

    // type_param: ['*' | '**'] NAME [':' expr]
    private _parseTypeParameter(): TypeParameterNode | undefined {
        let typeParamCategory = TypeParameterCategory.TypeVar;
        if (this._consumeTokenIfOperator(OperatorType.Multiply)) {
            typeParamCategory = TypeParameterCategory.TypeVarTuple;
        } else if (this._consumeTokenIfOperator(OperatorType.Power)) {
            typeParamCategory = TypeParameterCategory.ParamSpec;
        }

        const nameToken = this._getTokenIfIdentifier();
        if (!nameToken) {
            this._addError(Localizer.Diagnostic.expectedTypeParameterName(), this._peekToken());
            return undefined;
        }

        const name = NameNode.create(nameToken);

        let boundExpression: ExpressionNode | undefined;
        if (this._peekTokenType() === TokenType.Colon) {
            this._getNextToken();
            boundExpression = this._parseTestExpression(/* allowAssignmentExpression */ false);

            if (typeParamCategory !== TypeParameterCategory.TypeVar) {
                this._addError(Localizer.Diagnostic.typeParameterBoundNotAllowed(), boundExpression);
            }
        }

        return TypeParameterNode.create(name, typeParamCategory, boundExpression);
    }

    // match_stmt: "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT
    // subject_expr:
    //     | star_named_expression ',' star_named_expressions?
    //     | named_expression
    private _parseMatchStatement(): MatchNode | undefined {
        // Parse the subject expression with errors suppressed. If it's not
        // followed by a colon, we'll assume this is not a match statement.
        // We need to do this because "match" is considered a soft keyword,
        // and we need to distinguish between "match(2)" and "match (2):"
        // and between "match[2]" and "match [2]:"
        let smellsLikeMatchStatement = false;
        this._suppressErrors(() => {
            const curTokenIndex = this._tokenIndex;

            this._getKeywordToken(KeywordType.Match);
            const expression = this._parseTestOrStarListAsExpression(
                /* allowAssignmentExpression */ true,
                /* allowMultipleUnpack */ true,
                ErrorExpressionCategory.MissingPatternSubject,
                Localizer.Diagnostic.expectedReturnExpr()
            );
            smellsLikeMatchStatement =
                expression.nodeType !== ParseNodeType.Error && this._peekToken().type === TokenType.Colon;

            // Set the token index back to the start.
            this._tokenIndex = curTokenIndex;
        });

        if (!smellsLikeMatchStatement) {
            return undefined;
        }

        const matchToken = this._getKeywordToken(KeywordType.Match);

        const subjectExpression = this._parseTestOrStarListAsExpression(
            /* allowAssignmentExpression */ true,
            /* allowMultipleUnpack */ true,
            ErrorExpressionCategory.MissingPatternSubject,
            Localizer.Diagnostic.expectedReturnExpr()
        );
        const matchNode = MatchNode.create(matchToken, subjectExpression);

        const nextToken = this._peekToken();

        if (!this._consumeTokenIfType(TokenType.Colon)) {
            this._addError(Localizer.Diagnostic.expectedColon(), nextToken);

            // Try to perform parse recovery by consuming tokens until
            // we find the end of the line.
            if (this._consumeTokensUntilType([TokenType.NewLine, TokenType.Colon])) {
                this._getNextToken();
            }
        } else if (!this._consumeTokenIfType(TokenType.NewLine)) {
            this._addError(Localizer.Diagnostic.expectedNewline(), nextToken);
        } else {
            const possibleIndent = this._peekToken();
            if (!this._consumeTokenIfType(TokenType.Indent)) {
                this._addError(Localizer.Diagnostic.expectedIndentedBlock(), this._peekToken());
            } else {
                const indentToken = possibleIndent as IndentToken;
                if (indentToken.isIndentAmbiguous) {
                    this._addError(Localizer.Diagnostic.inconsistentTabs(), indentToken);
                }
            }

            while (true) {
                // Handle a common error here and see if we can recover.
                const nextToken = this._peekToken();
                if (nextToken.type === TokenType.Indent) {
                    this._getNextToken();
                    const indentToken = nextToken as IndentToken;
                    if (indentToken.isIndentAmbiguous) {
                        this._addError(Localizer.Diagnostic.inconsistentTabs(), indentToken);
                    } else {
                        this._addError(Localizer.Diagnostic.unexpectedIndent(), nextToken);
                    }
                }

                const caseStatement = this._parseCaseStatement();
                if (!caseStatement) {
                    // Perform basic error recovery to get to the next line.
                    if (this._consumeTokensUntilType([TokenType.NewLine, TokenType.Colon])) {
                        this._getNextToken();
                    }
                } else {
                    caseStatement.parent = matchNode;
                    matchNode.cases.push(caseStatement);
                }

                const dedentToken = this._peekToken() as DedentToken;
                if (this._consumeTokenIfType(TokenType.Dedent)) {
                    if (!dedentToken.matchesIndent) {
                        this._addError(Localizer.Diagnostic.inconsistentIndent(), dedentToken);
                    }
                    if (dedentToken.isDedentAmbiguous) {
                        this._addError(Localizer.Diagnostic.inconsistentTabs(), dedentToken);
                    }
                    break;
                }

                if (this._peekTokenType() === TokenType.EndOfStream) {
                    break;
                }
            }

            if (matchNode.cases.length > 0) {
                extendRange(matchNode, matchNode.cases[matchNode.cases.length - 1]);
            } else {
                this._addError(Localizer.Diagnostic.zeroCaseStatementsFound(), matchToken);
            }
        }

        // This feature requires Python 3.10.
        if (this._getLanguageVersion() < PythonVersion.V3_10) {
            this._addError(Localizer.Diagnostic.matchIncompatible(), matchToken);
        }

        // Validate that only the last entry uses an irrefutable pattern.
        for (let i = 0; i < matchNode.cases.length - 1; i++) {
            const caseNode = matchNode.cases[i];
            if (!caseNode.guardExpression && caseNode.isIrrefutable) {
                this._addError(Localizer.Diagnostic.casePatternIsIrrefutable(), caseNode.pattern);
            }
        }

        return matchNode;
    }

    // case_block: "case" patterns [guard] ':' block
    // patterns: sequence_pattern | as_pattern
    // guard: 'if' named_expression
    private _parseCaseStatement(): CaseNode | undefined {
        const caseToken = this._peekToken();

        if (!this._consumeTokenIfKeyword(KeywordType.Case)) {
            this._addError(Localizer.Diagnostic.expectedCase(), caseToken);
            return undefined;
        }

        const patternList = this._parsePatternSequence();
        let casePattern: PatternAtomNode;

        if (patternList.parseError) {
            casePattern = patternList.parseError;
        } else if (patternList.list.length === 0) {
            this._addError(Localizer.Diagnostic.expectedPatternExpr(), this._peekToken());
            casePattern = ErrorNode.create(caseToken, ErrorExpressionCategory.MissingPattern);
        } else if (patternList.list.length === 1 && !patternList.trailingComma) {
            const pattern = patternList.list[0].orPatterns[0];

            if (pattern.nodeType === ParseNodeType.PatternCapture && pattern.isStar) {
                casePattern = PatternSequenceNode.create(patternList.list[0], patternList.list);
            } else {
                casePattern = patternList.list[0];
            }
        } else {
            casePattern = PatternSequenceNode.create(patternList.list[0], patternList.list);
        }

        let guardExpression: ExpressionNode | undefined;
        if (this._consumeTokenIfKeyword(KeywordType.If)) {
            guardExpression = this._parseTestExpression(/* allowAssignmentExpression */ true);
        }

        const suite = this._parseSuite(this._isInFunction);
        return CaseNode.create(caseToken, casePattern, this._isPatternIrrefutable(casePattern), guardExpression, suite);
    }

    // PEP 634 defines the concept of an "irrefutable" pattern - a pattern that
    // will always be matched.
    private _isPatternIrrefutable(node: PatternAtomNode): boolean {
        if (node.nodeType === ParseNodeType.PatternCapture) {
            return true;
        }

        if (node.nodeType === ParseNodeType.PatternAs) {
            return node.orPatterns.some((pattern) => this._isPatternIrrefutable(pattern));
        }

        return false;
    }

    private _getPatternTargetNames(node: PatternAtomNode, nameMap: Map<string, boolean>): void {
        switch (node.nodeType) {
            case ParseNodeType.PatternSequence: {
                node.entries.forEach((subpattern) => {
                    this._getPatternTargetNames(subpattern, nameMap);
                });
                break;
            }

            case ParseNodeType.PatternClass: {
                node.arguments.forEach((arg) => {
                    this._getPatternTargetNames(arg.pattern, nameMap);
                });
                break;
            }

            case ParseNodeType.PatternAs: {
                if (node.target) {
                    nameMap.set(node.target.value, true);
                }
                node.orPatterns.forEach((subpattern) => {
                    this._getPatternTargetNames(subpattern, nameMap);
                });
                break;
            }

            case ParseNodeType.PatternCapture: {
                if (!node.isWildcard) {
                    nameMap.set(node.target.value, true);
                }
                break;
            }

            case ParseNodeType.PatternMapping: {
                node.entries.forEach((mapEntry) => {
                    if (mapEntry.nodeType === ParseNodeType.PatternMappingExpandEntry) {
                        nameMap.set(mapEntry.target.value, true);
                    } else {
                        this._getPatternTargetNames(mapEntry.keyPattern, nameMap);
                        this._getPatternTargetNames(mapEntry.valuePattern, nameMap);
                    }
                });
                break;
            }

            case ParseNodeType.PatternLiteral:
            case ParseNodeType.PatternValue:
            case ParseNodeType.Error: {
                break;
            }
        }
    }

    private _parsePatternSequence() {
        const patternList = this._parseExpressionListGeneric(() => this._parsePatternAs());

        // Check for more than one star entry.
        const starEntries = patternList.list.filter(
            (entry) =>
                entry.orPatterns.length === 1 &&
                entry.orPatterns[0].nodeType === ParseNodeType.PatternCapture &&
                entry.orPatterns[0].isStar
        );
        if (starEntries.length > 1) {
            this._addError(Localizer.Diagnostic.duplicateStarPattern(), starEntries[1].orPatterns[0]);
        }

        // Look for redundant capture targets.
        const captureTargetMap = new Map<string, PatternAtomNode>();
        patternList.list.forEach((asPattern) => {
            asPattern.orPatterns.forEach((patternAtom) => {
                if (
                    patternAtom.nodeType === ParseNodeType.PatternCapture &&
                    !patternAtom.isStar &&
                    !patternAtom.isWildcard
                ) {
                    if (captureTargetMap.has(patternAtom.target.value)) {
                        this._addError(
                            Localizer.Diagnostic.duplicateCapturePatternTarget().format({
                                name: patternAtom.target.value,
                            }),
                            patternAtom
                        );
                    } else {
                        captureTargetMap.set(patternAtom.target.value, patternAtom);
                    }
                }
            });
        });

        return patternList;
    }

    // as_pattern: or_pattern ['as' NAME]
    // or_pattern: '|'.pattern_atom+
    private _parsePatternAs(): PatternAsNode {
        const orPatterns: PatternAtomNode[] = [];

        while (true) {
            const patternAtom = this._parsePatternAtom();
            orPatterns.push(patternAtom);

            if (!this._consumeTokenIfOperator(OperatorType.BitwiseOr)) {
                break;
            }
        }

        if (orPatterns.length > 1) {
            // Star patterns cannot be ORed with other patterns.
            orPatterns.forEach((patternAtom) => {
                if (patternAtom.nodeType === ParseNodeType.PatternCapture && patternAtom.isStar) {
                    this._addError(Localizer.Diagnostic.starPatternInOrPattern(), patternAtom);
                }
            });
        }

        let target: NameNode | undefined;
        if (this._consumeTokenIfKeyword(KeywordType.As)) {
            const nameToken = this._getTokenIfIdentifier();
            if (nameToken) {
                target = NameNode.create(nameToken);
            } else {
                this._addError(Localizer.Diagnostic.expectedNameAfterAs(), this._peekToken());
            }
        }

        // Star patterns cannot be used with AS pattern.
        if (
            target &&
            orPatterns.length === 1 &&
            orPatterns[0].nodeType === ParseNodeType.PatternCapture &&
            orPatterns[0].isStar
        ) {
            this._addError(Localizer.Diagnostic.starPatternInAsPattern(), orPatterns[0]);
        }

        // Validate that irrefutable patterns are not in any entries other than the last.
        orPatterns.forEach((orPattern, index) => {
            if (index < orPatterns.length - 1 && this._isPatternIrrefutable(orPattern)) {
                this._addError(Localizer.Diagnostic.orPatternIrrefutable(), orPattern);
            }
        });

        // Validate that all bound variables are the same within all or patterns.
        const fullNameMap = new Map<string, boolean>();
        orPatterns.forEach((orPattern) => {
            this._getPatternTargetNames(orPattern, fullNameMap);
        });

        orPatterns.forEach((orPattern) => {
            const localNameMap = new Map<string, boolean>();
            this._getPatternTargetNames(orPattern, localNameMap);

            if (localNameMap.size < fullNameMap.size) {
                const missingNames = Array.from(fullNameMap.keys()).filter((name) => !localNameMap.has(name));
                const diag = new DiagnosticAddendum();
                diag.addMessage(
                    Localizer.DiagnosticAddendum.orPatternMissingName().format({
                        name: missingNames.map((name) => `"${name}"`).join(', '),
                    })
                );
                this._addError(Localizer.Diagnostic.orPatternMissingName() + diag.getString(), orPattern);
            }
        });

        return PatternAsNode.create(orPatterns, target);
    }

    // pattern_atom:
    //     | literal_pattern
    //     | name_or_attr
    //     | '(' as_pattern ')'
    //     | '[' [sequence_pattern] ']'
    //     | '(' [sequence_pattern] ')'
    //     | '{' [items_pattern] '}'
    //     | name_or_attr '(' [pattern_arguments ','?] ')'
    // name_or_attr: attr | NAME
    // attr: name_or_attr '.' NAME
    // sequence_pattern: ','.maybe_star_pattern+ ','?
    // maybe_star_pattern: '*' NAME | pattern
    // items_pattern: ','.key_value_pattern+ ','?
    private _parsePatternAtom(): PatternAtomNode {
        const patternLiteral = this._parsePatternLiteral();
        if (patternLiteral) {
            return patternLiteral;
        }

        const patternCaptureOrValue = this._parsePatternCaptureOrValue();
        if (patternCaptureOrValue) {
            const openParenToken = this._peekToken();
            if (
                patternCaptureOrValue.nodeType === ParseNodeType.Error ||
                !this._consumeTokenIfType(TokenType.OpenParenthesis)
            ) {
                return patternCaptureOrValue;
            }

            const args = this._parseClassPatternArgList();

            const classNameExpr =
                patternCaptureOrValue.nodeType === ParseNodeType.PatternCapture
                    ? patternCaptureOrValue.target
                    : patternCaptureOrValue.expression;
            const classPattern = PatternClassNode.create(classNameExpr, args);

            if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                this._addError(Localizer.Diagnostic.expectedCloseParen(), openParenToken);

                // Consume the remainder of tokens on the line for error
                // recovery.
                this._consumeTokensUntilType([TokenType.NewLine]);

                // Extend the node's range to include the rest of the line.
                // This helps the signatureHelpProvider.
                extendRange(classPattern, this._peekToken());
            }

            return classPattern;
        }

        const nextToken = this._peekToken();
        const nextOperator = this._peekOperatorType();

        if (nextOperator === OperatorType.Multiply) {
            const starToken = this._getNextToken();
            const identifierToken = this._getTokenIfIdentifier();
            if (!identifierToken) {
                this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
                return ErrorNode.create(starToken, ErrorExpressionCategory.MissingExpression);
            } else {
                return PatternCaptureNode.create(NameNode.create(identifierToken), starToken);
            }
        }

        if (nextToken.type === TokenType.OpenParenthesis || nextToken.type === TokenType.OpenBracket) {
            const startToken = this._getNextToken();
            const patternList = this._parsePatternSequence();
            let casePattern: PatternAtomNode;

            if (patternList.parseError) {
                casePattern = patternList.parseError;
            } else if (
                patternList.list.length === 1 &&
                !patternList.trailingComma &&
                startToken.type === TokenType.OpenParenthesis
            ) {
                const pattern = patternList.list[0].orPatterns[0];

                if (pattern.nodeType === ParseNodeType.PatternCapture && pattern.isStar) {
                    casePattern = PatternSequenceNode.create(startToken, patternList.list);
                } else {
                    casePattern = patternList.list[0];
                }

                extendRange(casePattern, nextToken);
            } else {
                casePattern = PatternSequenceNode.create(startToken, patternList.list);
            }

            const endToken = this._peekToken();
            if (
                this._consumeTokenIfType(
                    nextToken.type === TokenType.OpenParenthesis ? TokenType.CloseParenthesis : TokenType.CloseBracket
                )
            ) {
                extendRange(casePattern, endToken);
            } else {
                this._addError(
                    nextToken.type === TokenType.OpenParenthesis
                        ? Localizer.Diagnostic.expectedCloseParen()
                        : Localizer.Diagnostic.expectedCloseBracket(),
                    nextToken
                );
                this._consumeTokensUntilType([
                    TokenType.Colon,
                    nextToken.type === TokenType.OpenParenthesis ? TokenType.CloseParenthesis : TokenType.CloseBracket,
                ]);
            }

            return casePattern;
        } else if (nextToken.type === TokenType.OpenCurlyBrace) {
            const firstToken = this._getNextToken();
            const mappingPattern = this._parsePatternMapping(firstToken);
            const lastToken = this._peekToken();

            if (this._consumeTokenIfType(TokenType.CloseCurlyBrace)) {
                extendRange(mappingPattern, lastToken);
            } else {
                this._addError(Localizer.Diagnostic.expectedCloseBrace(), nextToken);
                this._consumeTokensUntilType([TokenType.Colon, TokenType.CloseCurlyBrace]);
            }

            return mappingPattern;
        }

        return this._handleExpressionParseError(
            ErrorExpressionCategory.MissingPattern,
            Localizer.Diagnostic.expectedPatternExpr()
        );
    }

    // pattern_arguments:
    //     | positional_patterns [',' keyword_patterns]
    //     | keyword_patterns
    // positional_patterns: ','.as_pattern+
    // keyword_patterns: ','.keyword_pattern+
    private _parseClassPatternArgList(): PatternClassArgumentNode[] {
        const argList: PatternClassArgumentNode[] = [];
        let sawKeywordArg = false;

        while (true) {
            const nextTokenType = this._peekTokenType();
            if (
                nextTokenType === TokenType.CloseParenthesis ||
                nextTokenType === TokenType.NewLine ||
                nextTokenType === TokenType.EndOfStream
            ) {
                break;
            }

            const arg = this._parseClassPatternArgument();
            if (arg.name) {
                sawKeywordArg = true;
            } else if (sawKeywordArg && !arg.name) {
                this._addError(Localizer.Diagnostic.positionArgAfterNamedArg(), arg);
            }
            argList.push(arg);

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                break;
            }
        }

        return argList;
    }

    // keyword_pattern: NAME '=' as_pattern
    private _parseClassPatternArgument(): PatternClassArgumentNode {
        const firstToken = this._peekToken();
        const secondToken = this._peekToken(1);

        let keywordName: NameNode | undefined;

        if (
            (firstToken.type === TokenType.Identifier || firstToken.type === TokenType.Keyword) &&
            secondToken.type === TokenType.Operator &&
            (secondToken as OperatorToken).operatorType === OperatorType.Assign
        ) {
            const classNameToken = this._getTokenIfIdentifier();
            if (classNameToken !== undefined) {
                keywordName = NameNode.create(classNameToken);
                this._getNextToken();
            }
        }

        const pattern = this._parsePatternAs();

        return PatternClassArgumentNode.create(pattern, keywordName);
    }

    // literal_pattern:
    //     | signed_number
    //     | signed_number '+' NUMBER
    //     | signed_number '-' NUMBER
    //     | strings
    //     | 'None'
    //     | 'True'
    //     | 'False'
    private _parsePatternLiteral(): PatternLiteralNode | undefined {
        const nextToken = this._peekToken();
        const nextOperator = this._peekOperatorType();

        if (nextToken.type === TokenType.Number || nextOperator === OperatorType.Subtract) {
            return this._parsePatternLiteralNumber();
        }

        if (nextToken.type === TokenType.String) {
            const stringList = this._parseAtom() as StringListNode;
            assert(stringList.nodeType === ParseNodeType.StringList);

            // Check for f-strings, which are not allowed.
            stringList.strings.forEach((stringAtom) => {
                if (stringAtom.token.flags & StringTokenFlags.Format) {
                    this._addError(Localizer.Diagnostic.formatStringInPattern(), stringAtom);
                }
            });

            return PatternLiteralNode.create(stringList);
        }

        if (nextToken.type === TokenType.Keyword) {
            const keywordToken = nextToken as KeywordToken;
            if (
                keywordToken.keywordType === KeywordType.False ||
                keywordToken.keywordType === KeywordType.True ||
                keywordToken.keywordType === KeywordType.None
            ) {
                return PatternLiteralNode.create(this._parseAtom());
            }
        }

        return undefined;
    }

    // signed_number: NUMBER | '-' NUMBER
    private _parsePatternLiteralNumber(): PatternLiteralNode {
        const expression = this._parseArithmeticExpression();
        let realValue: ExpressionNode | undefined;
        let imagValue: ExpressionNode | undefined;

        if (expression.nodeType === ParseNodeType.BinaryOperation) {
            if (expression.operator === OperatorType.Subtract || expression.operator === OperatorType.Add) {
                realValue = expression.leftExpression;
                imagValue = expression.rightExpression;
            }
        } else {
            realValue = expression;
        }

        if (realValue) {
            if (realValue.nodeType === ParseNodeType.UnaryOperation && realValue.operator === OperatorType.Subtract) {
                realValue = realValue.expression;
            }

            if (realValue.nodeType !== ParseNodeType.Number || (imagValue !== undefined && realValue.isImaginary)) {
                this._addError(Localizer.Diagnostic.expectedComplexNumberLiteral(), expression);
                imagValue = undefined;
            }
        }

        if (imagValue) {
            if (imagValue.nodeType === ParseNodeType.UnaryOperation && imagValue.operator === OperatorType.Subtract) {
                imagValue = imagValue.expression;
            }

            if (imagValue.nodeType !== ParseNodeType.Number || !imagValue.isImaginary) {
                this._addError(Localizer.Diagnostic.expectedComplexNumberLiteral(), expression);
            }
        }

        return PatternLiteralNode.create(expression);
    }

    private _parsePatternMapping(firstToken: Token): PatternMappingNode | ErrorNode {
        const itemList = this._parseExpressionListGeneric(() => this._parsePatternMappingItem());

        if (itemList.list.length > 0) {
            // Verify there's at most one ** entry.
            const starStarEntries = itemList.list.filter(
                (entry) => entry.nodeType === ParseNodeType.PatternMappingExpandEntry
            );
            if (starStarEntries.length > 1) {
                this._addError(Localizer.Diagnostic.duplicateStarStarPattern(), starStarEntries[1]);
            }

            return PatternMappingNode.create(firstToken, itemList.list);
        }

        return itemList.parseError || ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingPattern);
    }

    // key_value_pattern:
    //     | (literal_pattern | attr) ':' as_pattern
    //     | '**' NAME
    private _parsePatternMappingItem(): PatternMappingEntryNode | ErrorNode {
        let keyExpression: PatternLiteralNode | PatternValueNode | ErrorNode | undefined;
        const doubleStar = this._peekToken();

        if (this._consumeTokenIfOperator(OperatorType.Power)) {
            const identifierToken = this._getTokenIfIdentifier();
            if (!identifierToken) {
                this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
                return ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingPattern);
            }

            const nameNode = NameNode.create(identifierToken);
            if (identifierToken.value === '_') {
                this._addError(Localizer.Diagnostic.starStarWildcardNotAllowed(), nameNode);
            }

            return PatternMappingExpandEntryNode.create(doubleStar, nameNode);
        }

        const patternLiteral = this._parsePatternLiteral();
        if (patternLiteral) {
            keyExpression = patternLiteral;
        } else {
            const patternCaptureOrValue = this._parsePatternCaptureOrValue();
            if (patternCaptureOrValue) {
                if (patternCaptureOrValue.nodeType === ParseNodeType.PatternValue) {
                    keyExpression = patternCaptureOrValue;
                } else {
                    this._addError(Localizer.Diagnostic.expectedPatternValue(), patternCaptureOrValue);
                    keyExpression = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingPattern);
                }
            }
        }

        if (!keyExpression) {
            this._addError(Localizer.Diagnostic.expectedPatternExpr(), this._peekToken());
            keyExpression = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingPattern);
        }

        let valuePattern: PatternAtomNode | undefined;
        if (!this._consumeTokenIfType(TokenType.Colon)) {
            this._addError(Localizer.Diagnostic.expectedColon(), this._peekToken());
            valuePattern = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingPattern);
        } else {
            valuePattern = this._parsePatternAs();
        }

        return PatternMappingKeyEntryNode.create(keyExpression, valuePattern);
    }

    private _parsePatternCaptureOrValue(): PatternCaptureNode | PatternValueNode | ErrorNode | undefined {
        const nextToken = this._peekToken();

        if (nextToken.type === TokenType.Identifier || nextToken.type === TokenType.Keyword) {
            let nameOrMember: NameNode | MemberAccessNode | undefined;

            while (true) {
                const identifierToken = this._getTokenIfIdentifier();
                if (identifierToken) {
                    const nameNode = NameNode.create(identifierToken);
                    nameOrMember = nameOrMember ? MemberAccessNode.create(nameOrMember, nameNode) : nameNode;
                } else {
                    this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
                    break;
                }

                if (!this._consumeTokenIfType(TokenType.Dot)) {
                    break;
                }
            }

            if (!nameOrMember) {
                this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
                return ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingPattern);
            }

            if (nameOrMember.nodeType === ParseNodeType.MemberAccess) {
                return PatternValueNode.create(nameOrMember);
            }

            return PatternCaptureNode.create(nameOrMember);
        }

        return undefined;
    }

    // if_stmt: 'if' test_suite ('elif' test_suite)* ['else' suite]
    // test_suite: test suite
    // test: or_test ['if' or_test 'else' test] | lambdef
    private _parseIfStatement(keywordType: KeywordType.If | KeywordType.Elif = KeywordType.If): IfNode {
        const ifOrElifToken = this._getKeywordToken(keywordType);

        const test = this._parseTestExpression(/* allowAssignmentExpression */ true);
        const suite = this._parseSuite(this._isInFunction);
        const ifNode = IfNode.create(ifOrElifToken, test, suite);

        if (this._consumeTokenIfKeyword(KeywordType.Else)) {
            ifNode.elseSuite = this._parseSuite(this._isInFunction);
            ifNode.elseSuite.parent = ifNode;
            extendRange(ifNode, ifNode.elseSuite);
        } else if (this._peekKeywordType() === KeywordType.Elif) {
            // Recursively handle an "elif" statement.
            ifNode.elseSuite = this._parseIfStatement(KeywordType.Elif);
            ifNode.elseSuite.parent = ifNode;
            extendRange(ifNode, ifNode.elseSuite);
        }

        return ifNode;
    }

    private _parseLoopSuite(): SuiteNode {
        const wasInLoop = this._isInLoop;
        const wasInFinally = this._isInFinally;
        this._isInLoop = true;
        this._isInFinally = false;

        let typeComment: StringToken | undefined;
        const suite = this._parseSuite(this._isInFunction, /* skipBody */ false, () => {
            const comment = this._getTypeAnnotationCommentText();
            if (comment) {
                typeComment = comment;
            }
        });

        this._isInLoop = wasInLoop;
        this._isInFinally = wasInFinally;

        if (typeComment) {
            suite.typeComment = typeComment;
        }

        return suite;
    }

    // suite: ':' (simple_stmt | NEWLINE INDENT stmt+ DEDENT)
    private _parseSuite(
        isFunction = false,
        skipBody = false,
        postColonCallback?: () => void,
        isCdefSuite?: boolean
    ): SuiteNode {
        const nextToken = this._peekToken();
        const suite = SuiteNode.create(nextToken);

        if (!this._consumeTokenIfType(TokenType.Colon)) {
            this._addError(Localizer.Diagnostic.expectedColon(), nextToken);

            // Try to perform parse recovery by consuming tokens.
            if (this._consumeTokensUntilType([TokenType.NewLine, TokenType.Colon])) {
                if (this._peekTokenType() === TokenType.Colon) {
                    this._getNextToken();
                } else if (this._peekToken(1).type !== TokenType.Indent) {
                    // Bail so we resume the at the next statement.
                    // We can't parse as a simple statement as we've skipped all but the newline.
                    this._getNextToken();
                    return suite;
                }
            }
        }

        if (skipBody) {
            if (this._consumeTokenIfType(TokenType.NewLine)) {
                let indent = 0;
                while (true) {
                    const nextToken = this._getNextToken();
                    if (nextToken.type === TokenType.Indent) {
                        indent++;
                    }

                    if (nextToken.type === TokenType.Dedent) {
                        if ((nextToken as DedentToken).isDedentAmbiguous) {
                            this._addError(Localizer.Diagnostic.inconsistentTabs(), nextToken);
                        }

                        indent--;

                        if (indent === 0) {
                            break;
                        }
                    }

                    if (nextToken.type === TokenType.EndOfStream) {
                        break;
                    }
                }
            } else {
                // consume tokens
                this._parseSimpleStatement();
            }

            if (this._tokenIndex > 0) {
                extendRange(suite, this._tokenizerOutput!.tokens.getItemAt(this._tokenIndex - 1));
            }

            return suite;
        }

        if (postColonCallback) {
            postColonCallback();
        }

        const wasFunction = this._isInFunction;
        this._isInFunction = isFunction;

        if (this._consumeTokenIfType(TokenType.NewLine)) {
            if (postColonCallback) {
                postColonCallback();
            }

            const possibleIndent = this._peekToken();
            if (!this._consumeTokenIfType(TokenType.Indent)) {
                this._addError(Localizer.Diagnostic.expectedIndentedBlock(), this._peekToken());
            } else {
                const indentToken = possibleIndent as IndentToken;
                if (indentToken.isIndentAmbiguous) {
                    this._addError(Localizer.Diagnostic.inconsistentTabs(), indentToken);
                }
            }

            while (true) {
                // Handle a common error here and see if we can recover.
                const nextToken = this._peekToken();
                if (nextToken.type === TokenType.Indent) {
                    this._getNextToken();
                    const indentToken = nextToken as IndentToken;
                    if (indentToken.isIndentAmbiguous) {
                        this._addError(Localizer.Diagnostic.inconsistentTabs(), indentToken);
                    } else {
                        this._addError(Localizer.Diagnostic.unexpectedIndent(), nextToken);
                    }
                }

                // ! Cython
                // Parse cython cdef statements only if isCdefSuite
                const statement = isCdefSuite ? this._parseCDefStatementInSuite() : this._parseStatement();
                if (!statement) {
                    // Perform basic error recovery to get to the next line.
                    this._consumeTokensUntilType([TokenType.NewLine]);
                } else {
                    statement.parent = suite;
                    suite.statements.push(statement);
                }

                const dedentToken = this._peekToken() as DedentToken;
                if (this._consumeTokenIfType(TokenType.Dedent)) {
                    if (!dedentToken.matchesIndent) {
                        this._addError(Localizer.Diagnostic.inconsistentIndent(), dedentToken);
                    }
                    if (dedentToken.isDedentAmbiguous) {
                        this._addError(Localizer.Diagnostic.inconsistentTabs(), dedentToken);
                    }
                    break;
                }

                if (this._peekTokenType() === TokenType.EndOfStream) {
                    break;
                }
            }
        } else {
            const simpleStatement = this._parseSimpleStatement();
            suite.statements.push(simpleStatement);
            simpleStatement.parent = suite;
        }

        if (suite.statements.length > 0) {
            extendRange(suite, suite.statements[suite.statements.length - 1]);
        }

        this._isInFunction = wasFunction;

        return suite;
    }

    // for_stmt: [async] 'for' exprlist 'in' testlist suite ['else' suite]
    private _parseForStatement(asyncToken?: KeywordToken): ForNode {
        const forToken = this._getKeywordToken(KeywordType.For);

        const targetExpr = this._parseExpressionListAsPossibleTuple(
            ErrorExpressionCategory.MissingExpression,
            Localizer.Diagnostic.expectedExpr(),
            forToken
        );

        let seqExpr: ExpressionNode;
        let forSuite: SuiteNode;
        let elseSuite: SuiteNode | undefined;

        // ! Cython
        let foundFrom = false;
        let byExpr: ExpressionNode | undefined = undefined;
        if (this._consumeTokenIfKeyword(KeywordType.From)) {
            // Handle deprecated for ... from ... by
            foundFrom = true;
        }

        if (!foundFrom && !this._consumeTokenIfKeyword(KeywordType.In)) {
            seqExpr = this._handleExpressionParseError(
                ErrorExpressionCategory.MissingIn,
                Localizer.Diagnostic.expectedIn()
            );
            forSuite = SuiteNode.create(this._peekToken());
        } else {
            seqExpr = this._parseTestOrStarListAsExpression(
                /* allowAssignmentExpression */ false,
                /* allowMultipleUnpack */ true,
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedInExpr()
            );

            if (foundFrom) {
                // TODO: Check seqexpr is lower <|<= target <|<= upper
                if (this._consumeTokenIfKeyword(KeywordType.By)) {
                    // This expression is the step size
                    byExpr = this._parseTestOrStarListAsExpression(
                        /* allowAssignmentExpression */ false,
                        /* allowMultipleUnpack */ false,
                        ErrorExpressionCategory.MissingExpression,
                        Localizer.DiagnosticCython.expectedForFromByExpr()
                    );
                }
            }

            forSuite = this._parseLoopSuite();

            // Versions of Python earlier than 3.9 didn't allow unpack operators if the
            // tuple wasn't enclosed in parentheses.
            if (this._getLanguageVersion() < PythonVersion.V3_9 && !this._parseOptions.isStubFile) {
                if (seqExpr.nodeType === ParseNodeType.Tuple && !seqExpr.enclosedInParens) {
                    let sawStar = false;
                    seqExpr.expressions.forEach((expr) => {
                        if (expr.nodeType === ParseNodeType.Unpack && !sawStar) {
                            this._addError(Localizer.Diagnostic.unpackOperatorNotAllowed(), expr);
                            sawStar = true;
                        }
                    });
                }
            }

            if (this._consumeTokenIfKeyword(KeywordType.Else)) {
                elseSuite = this._parseSuite(this._isInFunction);
            }
        }

        const forNode = ForNode.create(forToken, targetExpr, seqExpr, forSuite);
        forNode.elseSuite = elseSuite;
        if (elseSuite) {
            extendRange(forNode, elseSuite);
            elseSuite.parent = forNode;
        }

        if (asyncToken) {
            forNode.isAsync = true;
            forNode.asyncToken = asyncToken;
            extendRange(forNode, asyncToken);
        }

        if (forSuite.typeComment) {
            forNode.typeComment = forSuite.typeComment;
        }

        // ! Cython
        if (foundFrom) {
            const range = byExpr ? TextRange.combine([forToken, byExpr]) : TextRange.combine([forToken, seqExpr]);
            if (range) {
                this._addWarning(Localizer.DiagnosticCython.deprecatedForFromLoop(), range);
            }
            forNode.byExpression = byExpr;
            if (byExpr) {
                byExpr.parent = forNode;
            }
        }

        return forNode;
    }

    // comp_iter: comp_for | comp_if
    private _tryParseListComprehension(target: ParseNode): ListComprehensionNode | undefined {
        const compFor = this._tryParseCompForStatement();

        if (!compFor) {
            return undefined;
        }

        if (target.nodeType === ParseNodeType.Unpack) {
            this._addError(Localizer.Diagnostic.unpackIllegalInComprehension(), target);
        } else if (target.nodeType === ParseNodeType.DictionaryExpandEntry) {
            this._addError(Localizer.Diagnostic.dictExpandIllegalInComprehension(), target);
        }

        const listCompNode = ListComprehensionNode.create(target);

        const forIfList: ListComprehensionForIfNode[] = [compFor];
        while (true) {
            const compIter = this._tryParseCompForStatement() || this._tryParseCompIfStatement();
            if (!compIter) {
                break;
            }
            compIter.parent = listCompNode;
            forIfList.push(compIter);
        }

        listCompNode.forIfNodes = forIfList;
        if (forIfList.length > 0) {
            forIfList.forEach((comp) => {
                comp.parent = listCompNode;
            });
            extendRange(listCompNode, forIfList[forIfList.length - 1]);
        }
        return listCompNode;
    }

    // comp_for: ['async'] 'for' exprlist 'in' or_test [comp_iter]
    private _tryParseCompForStatement(): ListComprehensionForNode | undefined {
        const startTokenKeywordType = this._peekKeywordType();

        if (startTokenKeywordType === KeywordType.Async) {
            const nextToken = this._peekToken(1) as KeywordToken;
            if (nextToken.type !== TokenType.Keyword || nextToken.keywordType !== KeywordType.For) {
                return undefined;
            }
        } else if (startTokenKeywordType !== KeywordType.For) {
            return undefined;
        }

        let asyncToken: KeywordToken | undefined;
        if (this._peekKeywordType() === KeywordType.Async) {
            asyncToken = this._getKeywordToken(KeywordType.Async);
        }

        const forToken = this._getKeywordToken(KeywordType.For);

        const targetExpr = this._parseExpressionListAsPossibleTuple(
            ErrorExpressionCategory.MissingExpression,
            Localizer.Diagnostic.expectedExpr(),
            forToken
        );
        let seqExpr: ExpressionNode | undefined;

        if (!this._consumeTokenIfKeyword(KeywordType.In)) {
            seqExpr = this._handleExpressionParseError(
                ErrorExpressionCategory.MissingIn,
                Localizer.Diagnostic.expectedIn()
            );
        } else {
            this._disallowAssignmentExpression(() => {
                seqExpr = this._parseOrTest();
            });
        }

        const compForNode = ListComprehensionForNode.create(asyncToken || forToken, targetExpr, seqExpr!);

        if (asyncToken) {
            compForNode.isAsync = true;
            compForNode.asyncToken = asyncToken;
        }

        return compForNode;
    }

    // comp_if: 'if' test_nocond [comp_iter]
    // comp_iter: comp_for | comp_if
    private _tryParseCompIfStatement(): ListComprehensionIfNode | undefined {
        if (this._peekKeywordType() !== KeywordType.If) {
            return undefined;
        }

        const ifToken = this._getKeywordToken(KeywordType.If);
        const ifExpr =
            this._tryParseLambdaExpression() ||
            this._parseAssignmentExpression(/* disallowAssignmentExpression */ true);

        const compIfNode = ListComprehensionIfNode.create(ifToken, ifExpr);

        return compIfNode;
    }

    // while_stmt: 'while' test suite ['else' suite]
    private _parseWhileStatement(): WhileNode {
        const whileToken = this._getKeywordToken(KeywordType.While);

        const whileNode = WhileNode.create(
            whileToken,
            this._parseTestExpression(/* allowAssignmentExpression */ true),
            this._parseLoopSuite()
        );

        if (this._consumeTokenIfKeyword(KeywordType.Else)) {
            whileNode.elseSuite = this._parseSuite(this._isInFunction);
            whileNode.elseSuite.parent = whileNode;
            extendRange(whileNode, whileNode.elseSuite);
        }

        return whileNode;
    }

    // try_stmt: ('try' suite
    //         ((except_clause suite)+
    //             ['else' suite]
    //             ['finally' suite] |
    //         'finally' suite))
    // except_clause: 'except' [test ['as' NAME]]
    private _parseTryStatement(): TryNode {
        const tryToken = this._getKeywordToken(KeywordType.Try);
        const trySuite = this._parseSuite(this._isInFunction);
        const tryNode = TryNode.create(tryToken, trySuite);
        let sawCatchAllExcept = false;

        while (true) {
            const exceptToken = this._peekToken();
            if (!this._consumeTokenIfKeyword(KeywordType.Except)) {
                break;
            }

            // See if this is a Python 3.11 exception group.
            const possibleStarToken = this._peekToken();
            let isExceptGroup = false;
            if (this._consumeTokenIfOperator(OperatorType.Multiply)) {
                if (this._getLanguageVersion() < PythonVersion.V3_11 && !this._parseOptions.isStubFile) {
                    this._addError(Localizer.Diagnostic.exceptionGroupIncompatible(), possibleStarToken);
                }
                isExceptGroup = true;
            }

            let typeExpr: ExpressionNode | undefined;
            let symbolName: IdentifierToken | undefined;
            if (this._peekTokenType() !== TokenType.Colon) {
                typeExpr = this._parseTestExpression(/* allowAssignmentExpression */ true);

                if (this._consumeTokenIfKeyword(KeywordType.As)) {
                    symbolName = this._getTokenIfIdentifier();
                    if (!symbolName) {
                        this._addError(Localizer.Diagnostic.expectedNameAfterAs(), this._peekToken());
                    }
                } else {
                    // Handle the python 2.x syntax in a graceful manner.
                    const peekToken = this._peekToken();
                    if (this._consumeTokenIfType(TokenType.Comma)) {
                        this._addError(Localizer.Diagnostic.expectedAsAfterException(), peekToken);

                        // Parse the expression expected in python 2.x, but discard it.
                        this._parseTestExpression(/* allowAssignmentExpression */ false);
                    }
                }
            }

            if (!typeExpr) {
                if (sawCatchAllExcept) {
                    this._addError(Localizer.Diagnostic.duplicateCatchAll(), exceptToken);
                }
                sawCatchAllExcept = true;
            } else {
                if (sawCatchAllExcept) {
                    this._addError(Localizer.Diagnostic.namedExceptAfterCatchAll(), typeExpr);
                }
            }

            const exceptSuite = this._parseSuite(this._isInFunction);
            const exceptNode = ExceptNode.create(exceptToken, exceptSuite, isExceptGroup);
            if (typeExpr) {
                exceptNode.typeExpression = typeExpr;
                exceptNode.typeExpression.parent = exceptNode;
            }

            if (symbolName) {
                exceptNode.name = NameNode.create(symbolName);
                exceptNode.name.parent = exceptNode;
            }

            tryNode.exceptClauses.push(exceptNode);
            exceptNode.parent = tryNode;
        }

        if (tryNode.exceptClauses.length > 0) {
            extendRange(tryNode, tryNode.exceptClauses[tryNode.exceptClauses.length - 1]);

            if (this._consumeTokenIfKeyword(KeywordType.Else)) {
                tryNode.elseSuite = this._parseSuite(this._isInFunction);
                tryNode.elseSuite.parent = tryNode;
                extendRange(tryNode, tryNode.elseSuite);
            }
        }

        if (this._consumeTokenIfKeyword(KeywordType.Finally)) {
            tryNode.finallySuite = this._parseSuite(this._isInFunction);
            tryNode.finallySuite.parent = tryNode;
            extendRange(tryNode, tryNode.finallySuite);
        }

        if (!tryNode.finallySuite && tryNode.exceptClauses.length === 0) {
            this._addError(Localizer.Diagnostic.tryWithoutExcept(), tryToken);
        }

        return tryNode;
    }

    // funcdef: 'def' NAME parameters ['->' test] ':' suite
    // parameters: '(' [typedargslist] ')'
    private _parseFunctionDef(asyncToken?: KeywordToken, decorators?: DecoratorNode[]): FunctionNode | ErrorNode {
        const defToken = this._getKeywordToken(KeywordType.Def);

        const nameToken = this._getTokenIfIdentifier();
        if (!nameToken) {
            this._addError(Localizer.Diagnostic.expectedFunctionName(), defToken);
            return ErrorNode.create(
                defToken,
                ErrorExpressionCategory.MissingFunctionParameterList,
                undefined,
                decorators
            );
        }

        let typeParameters: TypeParameterListNode | undefined;
        const possibleOpenBracket = this._peekToken();
        if (possibleOpenBracket.type === TokenType.OpenBracket) {
            typeParameters = this._parseTypeParameterList();

            if (!this._parseOptions.isStubFile && this._getLanguageVersion() < PythonVersion.V3_12) {
                this._addError(Localizer.Diagnostic.functionTypeParametersIllegal(), typeParameters);
            }
        }
        const openParenToken = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            this._addError(Localizer.Diagnostic.expectedOpenParen(), this._peekToken());
            return ErrorNode.create(
                nameToken,
                ErrorExpressionCategory.MissingFunctionParameterList,
                NameNode.create(nameToken),
                decorators
            );
        }
        // ! Cython
        const paramList = this._parseVarArgsList(
            TokenType.CloseParenthesis,
            /* allowAnnotations */ true,
            /*tryCython*/ true
        );

        if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
            this._addError(Localizer.Diagnostic.expectedCloseParen(), openParenToken);
            this._consumeTokensUntilType([TokenType.Colon]);
        }

        let returnType: ExpressionNode | undefined;
        if (this._consumeTokenIfType(TokenType.Arrow)) {
            returnType = this._parseTypeAnnotation();
        }

        let functionTypeAnnotationToken: StringToken | undefined;
        const suite = this._parseSuite(/* isFunction */ true, this._parseOptions.skipFunctionAndClassBody, () => {
            if (!functionTypeAnnotationToken) {
                functionTypeAnnotationToken = this._getTypeAnnotationCommentText();
            }
        });

        const functionNode = FunctionNode.create(defToken, NameNode.create(nameToken), suite, typeParameters);
        if (asyncToken) {
            functionNode.isAsync = true;
            extendRange(functionNode, asyncToken);
        }

        functionNode.parameters = paramList;
        paramList.forEach((param) => {
            param.parent = functionNode;
        });

        if (decorators) {
            functionNode.decorators = decorators;
            decorators.forEach((decorator) => {
                decorator.parent = functionNode;
            });

            if (decorators.length > 0) {
                extendRange(functionNode, decorators[0]);
            }
        }

        if (returnType) {
            functionNode.returnTypeAnnotation = returnType;
            functionNode.returnTypeAnnotation.parent = functionNode;
            extendRange(functionNode, returnType);
        }

        // If there was a type annotation comment for the function,
        // parse it now.
        if (functionTypeAnnotationToken) {
            this._parseFunctionTypeAnnotationComment(functionTypeAnnotationToken, functionNode);
        }

        return functionNode;
    }

    // typedargslist: (
    //   tfpdef ['=' test] (',' tfpdef ['=' test])*
    //      [ ','
    //          [
    //              '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
    //              | '**' tfpdef [',']
    //          ]
    //      ]
    //   | '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
    //   | '**' tfpdef [','])
    // tfpdef: NAME [':' test]
    // vfpdef: NAME;
    // ! Cython
    private _parseVarArgsList(terminator: TokenType, allowAnnotations: boolean, tryCython = false): ParameterNode[] {
        const paramMap = new Map<string, string>();
        const paramList: ParameterNode[] = [];
        let sawDefaultParam = false;
        let reportedNonDefaultParamErr = false;
        let sawKeywordOnlySeparator = false;
        let sawPositionOnlySeparator = false;
        let sawKeywordOnlyParamAfterSeparator = false;
        let sawArgs = false;
        let sawKwArgs = false;

        while (true) {
            if (this._peekTokenType() === terminator) {
                break;
            }

            const param = this._parseParameter(allowAnnotations, tryCython);
            if (!param) {
                this._consumeTokensUntilType([terminator]);
                break;
            }

            if (param.name) {
                const name = param.name.value;
                if (paramMap.has(name)) {
                    this._addError(Localizer.Diagnostic.duplicateParam().format({ name }), param.name);
                } else {
                    paramMap.set(name, name);
                }
            } else if (param.category === ParameterCategory.Simple) {
                if (paramList.length === 0) {
                    this._addError(Localizer.Diagnostic.positionOnlyFirstParam(), param);
                }
            }

            if (param.category === ParameterCategory.Simple) {
                if (!param.name) {
                    if (sawPositionOnlySeparator) {
                        this._addError(Localizer.Diagnostic.duplicatePositionOnly(), param);
                    } else if (sawKeywordOnlySeparator) {
                        this._addError(Localizer.Diagnostic.positionOnlyAfterKeywordOnly(), param);
                    } else if (sawArgs) {
                        this._addError(Localizer.Diagnostic.positionOnlyAfterArgs(), param);
                    }
                    sawPositionOnlySeparator = true;
                } else {
                    if (sawKeywordOnlySeparator) {
                        sawKeywordOnlyParamAfterSeparator = true;
                    }

                    if (param.defaultValue) {
                        sawDefaultParam = true;
                    } else if (sawDefaultParam && !sawKeywordOnlySeparator && !sawArgs) {
                        // Report this error only once.
                        if (!reportedNonDefaultParamErr) {
                            this._addError(Localizer.Diagnostic.nonDefaultAfterDefault(), param);
                            reportedNonDefaultParamErr = true;
                        }
                    }
                }
            }

            paramList.push(param);

            if (param.category === ParameterCategory.VarArgList) {
                if (!param.name) {
                    if (sawKeywordOnlySeparator) {
                        this._addError(Localizer.Diagnostic.duplicateKeywordOnly(), param);
                    } else if (sawArgs) {
                        this._addError(Localizer.Diagnostic.keywordOnlyAfterArgs(), param);
                    }
                    sawKeywordOnlySeparator = true;
                } else {
                    if (sawKeywordOnlySeparator || sawArgs) {
                        this._addError(Localizer.Diagnostic.duplicateArgsParam(), param);
                    }
                    sawArgs = true;
                }
            }

            if (param.category === ParameterCategory.VarArgDictionary) {
                if (sawKwArgs) {
                    this._addError(Localizer.Diagnostic.duplicateKwargsParam(), param);
                }
                sawKwArgs = true;

                // A **kwargs cannot immediately follow a keyword-only separator ("*").
                if (sawKeywordOnlySeparator && !sawKeywordOnlyParamAfterSeparator) {
                    this._addError(Localizer.Diagnostic.keywordParameterMissing(), param);
                }
            } else if (sawKwArgs) {
                this._addError(Localizer.Diagnostic.paramAfterKwargsParam(), param);
            }

            const foundComma = this._consumeTokenIfType(TokenType.Comma);

            if (allowAnnotations && !param.typeAnnotation) {
                // Look for a type annotation comment at the end of the line.
                const typeAnnotationComment = this._parseVariableTypeAnnotationComment();
                if (typeAnnotationComment) {
                    param.typeAnnotationComment = typeAnnotationComment;
                    param.typeAnnotationComment.parent = param;
                    extendRange(param, param.typeAnnotationComment);
                }
            }

            if (!foundComma) {
                break;
            }
        }

        if (paramList.length > 0) {
            const lastParam = paramList[paramList.length - 1];
            if (lastParam.category === ParameterCategory.VarArgList && !lastParam.name) {
                this._addError(Localizer.Diagnostic.expectedNamedParameter(), lastParam);
            }
        }

        return paramList;
    }

    private _parseParameter(allowAnnotations: boolean, tryCython = false): ParameterNode {
        // ! Cython
        if (tryCython) {
            // Try parsing cython param
            const cParam = this._tryParseCLikeParameter(allowAnnotations);
            if (cParam) {
                return cParam;
            }
        }
        let starCount = 0;
        let slashCount = 0;
        const firstToken = this._peekToken();

        if (this._consumeTokenIfOperator(OperatorType.Multiply)) {
            starCount = 1;
        } else if (this._consumeTokenIfOperator(OperatorType.Power)) {
            starCount = 2;
        } else if (this._consumeTokenIfOperator(OperatorType.Divide)) {
            if (this._getLanguageVersion() < PythonVersion.V3_8 && !this._parseOptions.isStubFile) {
                this._addError(Localizer.Diagnostic.positionOnlyIncompatible(), firstToken);
            }
            slashCount = 1;
        }

        const paramName = this._getTokenIfIdentifier();
        if (!paramName) {
            if (starCount === 1) {
                const paramNode = ParameterNode.create(firstToken, ParameterCategory.VarArgList);
                return paramNode;
            } else if (slashCount === 1) {
                const paramNode = ParameterNode.create(firstToken, ParameterCategory.Simple);
                return paramNode;
            }

            // Check for the Python 2.x parameter sublist syntax and handle it gracefully.
            if (this._peekTokenType() === TokenType.OpenParenthesis) {
                const sublistStart = this._getNextToken();
                if (this._consumeTokensUntilType([TokenType.CloseParenthesis])) {
                    this._getNextToken();
                }
                this._addError(Localizer.Diagnostic.sublistParamsIncompatible(), sublistStart);
            } else {
                this._addError(Localizer.Diagnostic.expectedParamName(), this._peekToken());
            }
        }

        let paramType = ParameterCategory.Simple;
        if (starCount === 1) {
            paramType = ParameterCategory.VarArgList;
        } else if (starCount === 2) {
            paramType = ParameterCategory.VarArgDictionary;
        }
        const paramNode = ParameterNode.create(firstToken, paramType);
        if (paramName) {
            paramNode.name = NameNode.create(paramName);
            paramNode.name.parent = paramNode;
            extendRange(paramNode, paramName);
        }

        if (allowAnnotations && this._consumeTokenIfType(TokenType.Colon)) {
            paramNode.typeAnnotation = this._parseTypeAnnotation(paramType === ParameterCategory.VarArgList);
            paramNode.typeAnnotation.parent = paramNode;
            extendRange(paramNode, paramNode.typeAnnotation);
        }

        if (this._consumeTokenIfOperator(OperatorType.Assign)) {
            paramNode.defaultValue = this._parseTestExpression(/* allowAssignmentExpression */ false);
            paramNode.defaultValue.parent = paramNode;
            extendRange(paramNode, paramNode.defaultValue);

            if (starCount > 0) {
                this._addError(Localizer.Diagnostic.defaultValueNotAllowed(), paramNode.defaultValue);
            }
        }

        return paramNode;
    }

    // with_stmt: 'with' with_item (',' with_item)*  ':' suite
    // Python 3.10 adds support for optional parentheses around
    // with_item list.
    private _parseWithStatement(asyncToken?: KeywordToken): WithNode {
        const withToken = this._getKeywordToken(KeywordType.With);
        let withItemList: WithItemNode[] = [];

        const possibleParen = this._peekToken();

        // If the expression starts with a paren, parse it as though the
        // paren is enclosing the list of "with items". This is done as a
        // "dry run" to determine whether the entire list of "with items"
        // is enclosed in parentheses.
        let isParenthesizedWithItemList = false;
        if (possibleParen.type === TokenType.OpenParenthesis) {
            const openParenTokenIndex = this._tokenIndex;

            this._suppressErrors(() => {
                this._getNextToken();
                while (true) {
                    withItemList.push(this._parseWithItem());
                    if (!this._consumeTokenIfType(TokenType.Comma)) {
                        break;
                    }

                    if (this._peekToken().type === TokenType.CloseParenthesis) {
                        break;
                    }
                }

                if (
                    this._peekToken().type === TokenType.CloseParenthesis &&
                    this._peekToken(1).type === TokenType.Colon
                ) {
                    isParenthesizedWithItemList = withItemList.length !== 1 || withItemList[0].target !== undefined;
                }

                this._tokenIndex = openParenTokenIndex;
                withItemList = [];
            });
        }

        if (isParenthesizedWithItemList) {
            this._consumeTokenIfType(TokenType.OpenParenthesis);
            if (this._getLanguageVersion() < PythonVersion.V3_9) {
                this._addError(Localizer.Diagnostic.parenthesizedContextManagerIllegal(), possibleParen);
            }
        }

        while (true) {
            withItemList.push(this._parseWithItem());

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                break;
            }

            if (this._peekToken().type === TokenType.CloseParenthesis) {
                break;
            }
        }

        if (isParenthesizedWithItemList) {
            if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                this._addError(Localizer.Diagnostic.expectedCloseParen(), possibleParen);
            }
        }

        let typeComment: StringToken | undefined;
        const withSuite = this._parseSuite(this._isInFunction, /* skipBody */ false, () => {
            const comment = this._getTypeAnnotationCommentText();
            if (comment) {
                typeComment = comment;
            }
        });
        const withNode = WithNode.create(withToken, withSuite);
        if (asyncToken) {
            withNode.isAsync = true;
            withNode.asyncToken = asyncToken;
            extendRange(withNode, asyncToken);
        }

        if (typeComment) {
            withNode.typeComment = typeComment;
        }

        withNode.withItems = withItemList;
        withItemList.forEach((withItem) => {
            withItem.parent = withNode;
        });

        return withNode;
    }

    // with_item: test ['as' expr]
    private _parseWithItem(): WithItemNode {
        // ! Cython
        const expr = this._parseCGilContext() ?? this._parseTestExpression(/* allowAssignmentExpression */ true);
        const itemNode = WithItemNode.create(expr);

        if (expr.nodeType !== ParseNodeType.CGil && this._consumeTokenIfKeyword(KeywordType.As)) {
            itemNode.target = this._parseExpression(/* allowUnpack */ false);
            itemNode.target.parent = itemNode;
            extendRange(itemNode, itemNode.target);
        }

        return itemNode;
    }

    // decorators: decorator+
    // decorated: decorators (classdef | funcdef | async_funcdef)
    private _parseDecorated(/* ! Cython*/ isInCSuite = false): StatementNode {
        const decoratorList: DecoratorNode[] = [];

        while (true) {
            if (this._peekOperatorType() === OperatorType.MatrixMultiply) {
                decoratorList.push(this._parseDecorator());
            } else {
                break;
            }
        }

        const nextToken = this._peekToken() as KeywordToken;
        if (nextToken.type === TokenType.Keyword) {
            if (nextToken.keywordType === KeywordType.Async) {
                this._getNextToken();

                if (this._peekKeywordType() !== KeywordType.Def) {
                    this._addError(Localizer.Diagnostic.expectedFunctionAfterAsync(), this._peekToken());
                } else {
                    return this._parseFunctionDef(nextToken, decoratorList);
                }
            } else if (nextToken.keywordType === KeywordType.Def) {
                return this._parseFunctionDef(undefined, decoratorList);
            } else if (nextToken.keywordType === KeywordType.Class) {
                return this._parseClassDef(decoratorList);
            }
        }

        // ! Cython
        let node: ParseNode | undefined = undefined;
        if (
            nextToken.type === TokenType.Keyword &&
            (nextToken.keywordType === KeywordType.Cdef || nextToken.keywordType === KeywordType.Cpdef)
        ) {
            node = nextToken.keywordType === KeywordType.Cdef ? this._parseCDef() : this._parseCpdef();
        } else if (isInCSuite) {
            // Could be a function if in c suite
            node = this._parseCVarDecl();
        }

        if (node) {
            if (CFunctionNode.isInstance(node) || node.nodeType === ParseNodeType.Class) {
                node.decorators = decoratorList;
                decoratorList.forEach((decorator) => {
                    decorator.parent = node;
                });

                if (decoratorList.length > 0) {
                    extendRange(node, decoratorList[0]);
                }
                if (CFunctionNode.isInstance(node) && node.isForwardDeclaration) {
                    // Consume new line if forward declared
                    this._expectNewLine();
                    this._consumeTokenIfType(TokenType.NewLine);
                }
            }
            return node;
        }

        this._addError(Localizer.Diagnostic.expectedAfterDecorator(), this._peekToken());

        // Return a dummy class declaration so the completion provider has
        // some parse nodes to work with.
        return ClassNode.createDummyForDecorators(decoratorList);
    }

    // decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
    private _parseDecorator(): DecoratorNode {
        const atOperator = this._getNextToken() as OperatorToken;
        assert(atOperator.operatorType === OperatorType.MatrixMultiply);

        const expression = this._parseTestExpression(/* allowAssignmentExpression */ true);

        // Versions of Python prior to 3.9 support a limited set of
        // expression forms.
        if (this._getLanguageVersion() < PythonVersion.V3_9) {
            let isSupportedExpressionForm = false;
            if (this._isNameOrMemberAccessExpression(expression)) {
                isSupportedExpressionForm = true;
            } else if (
                expression.nodeType === ParseNodeType.Call &&
                this._isNameOrMemberAccessExpression(expression.leftExpression)
            ) {
                isSupportedExpressionForm = true;
            }

            if (!isSupportedExpressionForm) {
                this._addError(Localizer.Diagnostic.expectedDecoratorExpr(), expression);
            }
        }

        const decoratorNode = DecoratorNode.create(atOperator, expression);

        if (!this._consumeTokenIfType(TokenType.NewLine)) {
            this._addError(Localizer.Diagnostic.expectedDecoratorNewline(), this._peekToken());
            this._consumeTokensUntilType([TokenType.NewLine]);
        }

        return decoratorNode;
    }

    private _isNameOrMemberAccessExpression(expression: ExpressionNode): boolean {
        if (expression.nodeType === ParseNodeType.Name) {
            return true;
        } else if (expression.nodeType === ParseNodeType.MemberAccess) {
            return this._isNameOrMemberAccessExpression(expression.leftExpression);
        }

        return false;
    }

    // classdef: 'class' NAME ['(' [arglist] ')'] suite
    private _parseClassDef(decorators?: DecoratorNode[], isCython = false): ClassNode {
        const classToken = this._getKeywordToken(KeywordType.Class);

        let nameToken = this._getTokenIfIdentifier();
        if (!nameToken) {
            this._addError(Localizer.Diagnostic.expectedClassName(), this._peekToken());
            nameToken = IdentifierToken.create(0, 0, '', /* comments */ undefined);
        }

        let typeParameters: TypeParameterListNode | undefined;
        const possibleOpenBracket = this._peekToken();
        if (possibleOpenBracket.type === TokenType.OpenBracket) {
            typeParameters = this._parseTypeParameterList();

            if (!this._parseOptions.isStubFile && this._getLanguageVersion() < PythonVersion.V3_12) {
                this._addError(Localizer.Diagnostic.classTypeParametersIllegal(), typeParameters);
            }
        }

        let argList: ArgumentNode[] = [];
        const openParenToken = this._peekToken();
        if (this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            argList = this._parseArgList().args;

            if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                this._addError(Localizer.Diagnostic.expectedCloseParen(), openParenToken);
            }
        }

        // ! Cython
        const suite = this._parseSuite(/* isFunction */ false, this._parseOptions.skipFunctionAndClassBody, undefined);

        const classNode = ClassNode.create(classToken, NameNode.create(nameToken), suite, typeParameters);
        classNode.isCython = isCython;
        classNode.arguments = argList;
        argList.forEach((arg) => {
            arg.parent = classNode;
        });

        if (decorators) {
            classNode.decorators = decorators;
            if (decorators.length > 0) {
                decorators.forEach((decorator) => {
                    decorator.parent = classNode;
                });
                extendRange(classNode, decorators[0]);
            }
        }

        return classNode;
    }

    private _parsePassStatement(): PassNode {
        return PassNode.create(this._getKeywordToken(KeywordType.Pass));
    }

    private _parseBreakStatement(): BreakNode {
        const breakToken = this._getKeywordToken(KeywordType.Break);

        if (!this._isInLoop) {
            this._addError(Localizer.Diagnostic.breakOutsideLoop(), breakToken);
        }

        return BreakNode.create(breakToken);
    }

    private _parseContinueStatement(): ContinueNode {
        const continueToken = this._getKeywordToken(KeywordType.Continue);

        if (!this._isInLoop) {
            this._addError(Localizer.Diagnostic.continueOutsideLoop(), continueToken);
        } else if (this._isInFinally) {
            this._addError(Localizer.Diagnostic.continueInFinally(), continueToken);
        }

        return ContinueNode.create(continueToken);
    }

    // return_stmt: 'return' [testlist]
    private _parseReturnStatement(): ReturnNode {
        const returnToken = this._getKeywordToken(KeywordType.Return);

        const returnNode = ReturnNode.create(returnToken);

        if (!this._isInFunction) {
            this._addError(Localizer.Diagnostic.returnOutsideFunction(), returnToken);
        }

        if (!this._isNextTokenNeverExpression()) {
            const returnExpr = this._parseTestOrStarListAsExpression(
                /* allowAssignmentExpression */ true,
                /* allowMultipleUnpack */ true,
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedReturnExpr()
            );
            this._reportConditionalErrorForStarTupleElement(returnExpr);
            returnNode.returnExpression = returnExpr;
            returnNode.returnExpression.parent = returnNode;
            extendRange(returnNode, returnExpr);
        }

        return returnNode;
    }

    // import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
    //             'import' ('*' | '(' import_as_names ')' | import_as_names))
    // import_as_names: import_as_name (',' import_as_name)* [',']
    // import_as_name: NAME ['as' NAME]
    private _parseFromStatement(): ImportFromNode {
        const fromToken = this._getKeywordToken(KeywordType.From);

        const modName = this._parseDottedModuleName(/* allowJustDots */ true);
        const importFromNode = ImportFromNode.create(fromToken, modName);

        // Handle imports from __future__ specially because they can
        // change the way we interpret the rest of the file.
        const isFutureImport =
            modName.leadingDots === 0 && modName.nameParts.length === 1 && modName.nameParts[0].value === '__future__';

        const possibleInputToken = this._peekToken();

        // ! Cython
        let isCython: boolean | undefined = undefined;
        if (possibleInputToken.type === TokenType.Keyword) {
            const kwToken = possibleInputToken as KeywordToken;
            if (kwToken.keywordType === KeywordType.Import) {
                isCython = false;
            } else if (kwToken.keywordType === KeywordType.Cimport) {
                isCython = true;
            }
        }
        importFromNode.isCython = isCython;

        if (!this._consumeTokenIfKeyword(KeywordType.Import) && !this._consumeTokenIfKeyword(KeywordType.Cimport)) {
            this._addError(Localizer.Diagnostic.expectedImport(), this._peekToken());
            if (!modName.hasTrailingDot) {
                importFromNode.missingImportKeyword = true;
            }
        } else {
            extendRange(importFromNode, possibleInputToken);

            // Look for "*" token.
            const possibleStarToken = this._peekToken();
            if (this._consumeTokenIfOperator(OperatorType.Multiply)) {
                extendRange(importFromNode, possibleStarToken);
                importFromNode.isWildcardImport = true;
                importFromNode.wildcardToken = possibleStarToken;
                this._containsWildcardImport = true;
            } else {
                const openParenToken = this._peekToken();
                const inParen = this._consumeTokenIfType(TokenType.OpenParenthesis);
                let trailingCommaToken: Token | undefined;

                while (true) {
                    const importName = this._getTokenIfIdentifier();
                    if (!importName) {
                        break;
                    }

                    trailingCommaToken = undefined;

                    const importFromAsNode = ImportFromAsNode.create(NameNode.create(importName));

                    if (this._consumeTokenIfKeyword(KeywordType.As)) {
                        const aliasName = this._getTokenIfIdentifier();
                        if (!aliasName) {
                            this._addError(Localizer.Diagnostic.expectedImportAlias(), this._peekToken());
                        } else {
                            importFromAsNode.alias = NameNode.create(aliasName);
                            importFromAsNode.alias.parent = importFromAsNode;
                            extendRange(importFromAsNode, aliasName);
                        }
                    }

                    importFromNode.imports.push(importFromAsNode);
                    importFromAsNode.parent = importFromNode;
                    extendRange(importFromNode, importFromAsNode);

                    if (isFutureImport) {
                        // Add the future import to the map.
                        this._futureImportMap.set(importName.value, true);
                    }

                    const nextToken = this._peekToken();
                    if (!this._consumeTokenIfType(TokenType.Comma)) {
                        break;
                    }
                    trailingCommaToken = nextToken;
                }

                if (importFromNode.imports.length === 0) {
                    this._addError(Localizer.Diagnostic.expectedImportSymbols(), this._peekToken());
                }

                if (inParen) {
                    importFromNode.usesParens = true;

                    const nextToken = this._peekToken();
                    if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                        this._addError(Localizer.Diagnostic.expectedCloseParen(), openParenToken);
                    } else {
                        extendRange(importFromNode, nextToken);
                    }
                } else if (trailingCommaToken) {
                    this._addError(Localizer.Diagnostic.trailingCommaInFromImport(), trailingCommaToken);
                }
            }
        }

        this._importedModules.push({
            nameNode: importFromNode.module,
            leadingDots: importFromNode.module.leadingDots,
            nameParts: importFromNode.module.nameParts.map((p) => p.value),
            importedSymbols: importFromNode.imports.map((imp) => imp.name.value),
            isCython: isCython,
            cythonExt: isCython ? 'pxd' : undefined,
        });

        let isTypingImport = false;
        if (importFromNode.module.nameParts.length === 1) {
            const firstNamePartValue = importFromNode.module.nameParts[0].value;
            if (firstNamePartValue === 'typing' || firstNamePartValue === 'typing_extensions') {
                isTypingImport = true;
            }
        }

        if (isTypingImport) {
            const typingSymbolsOfInterest = ['Literal', 'TypeAlias', 'Annotated'];

            if (importFromNode.isWildcardImport) {
                typingSymbolsOfInterest.forEach((s) => {
                    this._typingSymbolAliases.set(s, s);
                });
            } else {
                importFromNode.imports.forEach((imp) => {
                    if (typingSymbolsOfInterest.some((s) => s === imp.name.value)) {
                        this._typingSymbolAliases.set(imp.alias?.value || imp.name.value, imp.name.value);
                    }
                });
            }
        }

        return importFromNode;
    }

    // import_name: 'import' dotted_as_names
    // dotted_as_names: dotted_as_name (',' dotted_as_name)*
    // dotted_as_name: dotted_name ['as' NAME]
    private _parseImportStatement(keyword: KeywordType): ImportNode {
        const importToken = this._getKeywordToken(keyword);

        const importNode = ImportNode.create(importToken);

        // ! Cython
        const isCython = keyword === KeywordType.Cimport ? true : false;
        importNode.isCython = isCython;

        while (true) {
            const modName = this._parseDottedModuleName();

            const importAsNode = ImportAsNode.create(modName);

            if (this._consumeTokenIfKeyword(KeywordType.As)) {
                const aliasToken = this._getTokenIfIdentifier();
                if (aliasToken) {
                    importAsNode.alias = NameNode.create(aliasToken);
                    importAsNode.alias.parent = importAsNode;
                    extendRange(importAsNode, importAsNode.alias);
                } else {
                    this._addError(Localizer.Diagnostic.expectedImportAlias(), this._peekToken());
                }
            }

            if (importAsNode.module.leadingDots > 0) {
                this._addError(Localizer.Diagnostic.relativeImportNotAllowed(), importAsNode.module);
            }

            importNode.list.push(importAsNode);
            importAsNode.parent = importNode;

            this._importedModules.push({
                nameNode: importAsNode.module,
                leadingDots: importAsNode.module.leadingDots,
                nameParts: importAsNode.module.nameParts.map((p) => p.value),
                importedSymbols: undefined,

                // ! Cython
                isCython: isCython,
                cythonExt: isCython ? 'pxd' : undefined,
            });

            if (modName.nameParts.length === 1) {
                const firstNamePartValue = modName.nameParts[0].value;
                if (firstNamePartValue === 'typing' || firstNamePartValue === 'typing_extensions') {
                    this._typingImportAliases.push(importAsNode.alias?.value || firstNamePartValue);
                }
            }

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                break;
            }
        }

        if (importNode.list.length > 0) {
            extendRange(importNode, importNode.list[importNode.list.length - 1]);
        }

        return importNode;
    }

    // ('.' | '...')* dotted_name | ('.' | '...')+
    // dotted_name: NAME ('.' NAME)*
    private _parseDottedModuleName(allowJustDots = false): ModuleNameNode {
        const moduleNameNode = ModuleNameNode.create(this._peekToken());

        while (true) {
            const token = this._getTokenIfType(TokenType.Ellipsis) ?? this._getTokenIfType(TokenType.Dot);
            if (token) {
                if (token.type === TokenType.Ellipsis) {
                    moduleNameNode.leadingDots += 3;
                } else {
                    moduleNameNode.leadingDots++;
                }

                extendRange(moduleNameNode, token);
            } else {
                break;
            }
        }

        while (true) {
            const identifier = this._getTokenIfIdentifier();
            if (!identifier) {
                if (!allowJustDots || moduleNameNode.leadingDots === 0 || moduleNameNode.nameParts.length > 0) {
                    this._addError(Localizer.Diagnostic.expectedModuleName(), this._peekToken());
                    moduleNameNode.hasTrailingDot = true;
                }
                break;
            }

            const namePart = NameNode.create(identifier);
            moduleNameNode.nameParts.push(namePart);
            namePart.parent = moduleNameNode;
            extendRange(moduleNameNode, namePart);

            const nextToken = this._peekToken();
            if (!this._consumeTokenIfType(TokenType.Dot)) {
                break;
            }

            // Extend the module name to include the dot.
            extendRange(moduleNameNode, nextToken);
        }

        return moduleNameNode;
    }

    private _parseGlobalStatement(): GlobalNode {
        const globalToken = this._getKeywordToken(KeywordType.Global);

        const globalNode = GlobalNode.create(globalToken);
        globalNode.nameList = this._parseNameList();
        if (globalNode.nameList.length > 0) {
            globalNode.nameList.forEach((name) => {
                name.parent = globalNode;
            });
            extendRange(globalNode, globalNode.nameList[globalNode.nameList.length - 1]);
        }
        return globalNode;
    }

    private _parseNonlocalStatement(): NonlocalNode {
        const nonlocalToken = this._getKeywordToken(KeywordType.Nonlocal);

        const nonlocalNode = NonlocalNode.create(nonlocalToken);
        nonlocalNode.nameList = this._parseNameList();
        if (nonlocalNode.nameList.length > 0) {
            nonlocalNode.nameList.forEach((name) => {
                name.parent = nonlocalNode;
            });
            extendRange(nonlocalNode, nonlocalNode.nameList[nonlocalNode.nameList.length - 1]);
        }
        return nonlocalNode;
    }

    private _parseNameList(): NameNode[] {
        const nameList: NameNode[] = [];

        while (true) {
            const name = this._getTokenIfIdentifier();
            if (!name) {
                this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
                break;
            }

            nameList.push(NameNode.create(name));

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                break;
            }
        }

        return nameList;
    }

    // raise_stmt: 'raise' [test ['from' test]]
    // (old) raise_stmt: 'raise' [test [',' test [',' test]]]
    private _parseRaiseStatement(): RaiseNode {
        const raiseToken = this._getKeywordToken(KeywordType.Raise);

        const raiseNode = RaiseNode.create(raiseToken);
        if (!this._isNextTokenNeverExpression()) {
            raiseNode.typeExpression = this._parseTestExpression(/* allowAssignmentExpression */ true);
            raiseNode.typeExpression.parent = raiseNode;
            extendRange(raiseNode, raiseNode.typeExpression);

            if (this._consumeTokenIfKeyword(KeywordType.From)) {
                raiseNode.valueExpression = this._parseTestExpression(/* allowAssignmentExpression */ true);
                raiseNode.valueExpression.parent = raiseNode;
                extendRange(raiseNode, raiseNode.valueExpression);
            } else {
                if (this._consumeTokenIfType(TokenType.Comma)) {
                    // Handle the Python 2.x variant
                    raiseNode.valueExpression = this._parseTestExpression(/* allowAssignmentExpression */ true);
                    raiseNode.valueExpression.parent = raiseNode;
                    extendRange(raiseNode, raiseNode.valueExpression);

                    if (this._consumeTokenIfType(TokenType.Comma)) {
                        raiseNode.tracebackExpression = this._parseTestExpression(/* allowAssignmentExpression */ true);
                        raiseNode.tracebackExpression.parent = raiseNode;
                        extendRange(raiseNode, raiseNode.tracebackExpression);
                    }
                }
            }
        }

        return raiseNode;
    }

    // assert_stmt: 'assert' test [',' test]
    private _parseAssertStatement(): AssertNode {
        const assertToken = this._getKeywordToken(KeywordType.Assert);

        const expr = this._parseTestExpression(/* allowAssignmentExpression */ false);
        const assertNode = AssertNode.create(assertToken, expr);

        if (this._consumeTokenIfType(TokenType.Comma)) {
            const exceptionExpr = this._parseTestExpression(/* allowAssignmentExpression */ false);
            assertNode.exceptionExpression = exceptionExpr;
            assertNode.exceptionExpression.parent = assertNode;
            extendRange(assertNode, exceptionExpr);
        }

        return assertNode;
    }

    // del_stmt: 'del' exprlist
    private _parseDelStatement(): DelNode {
        const delToken = this._getKeywordToken(KeywordType.Del);

        const exprListResult = this._parseExpressionList(/* allowStar */ true);
        if (!exprListResult.parseError && exprListResult.list.length === 0) {
            this._addError(Localizer.Diagnostic.expectedDelExpr(), this._peekToken());
        }
        const delNode = DelNode.create(delToken);
        delNode.expressions = exprListResult.list;
        if (delNode.expressions.length > 0) {
            delNode.expressions.forEach((expr) => {
                expr.parent = delNode;
            });
            extendRange(delNode, delNode.expressions[delNode.expressions.length - 1]);
        }
        return delNode;
    }

    // yield_expr: 'yield' [yield_arg]
    // yield_arg: 'from' test | testlist
    private _parseYieldExpression(): YieldNode | YieldFromNode {
        const yieldToken = this._getKeywordToken(KeywordType.Yield);

        const nextToken = this._peekToken();
        if (this._consumeTokenIfKeyword(KeywordType.From)) {
            if (this._getLanguageVersion() < PythonVersion.V3_3) {
                this._addError(Localizer.Diagnostic.yieldFromIllegal(), nextToken);
            }
            return YieldFromNode.create(yieldToken, this._parseTestExpression(/* allowAssignmentExpression */ true));
        }

        let exprList: ExpressionNode | undefined;
        if (!this._isNextTokenNeverExpression()) {
            exprList = this._parseTestOrStarListAsExpression(
                /* allowAssignmentExpression */ true,
                /* allowMultipleUnpack */ true,
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedYieldExpr()
            );
            this._reportConditionalErrorForStarTupleElement(exprList);
        }

        return YieldNode.create(yieldToken, exprList);
    }

    private _tryParseYieldExpression(): YieldNode | YieldFromNode | undefined {
        if (this._peekKeywordType() !== KeywordType.Yield) {
            return undefined;
        }

        return this._parseYieldExpression();
    }

    // simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
    private _parseSimpleStatement(): StatementListNode {
        const statement = StatementListNode.create(this._peekToken());

        while (true) {
            // Swallow invalid tokens to make sure we make forward progress.
            if (this._peekTokenType() === TokenType.Invalid) {
                const invalidToken = this._getNextToken();
                const text = this._fileContents!.substr(invalidToken.start, invalidToken.length);

                const firstCharCode = text.charCodeAt(0);

                // Remove any non-printable characters.
                this._addError(
                    Localizer.Diagnostic.invalidTokenChars().format({ text: `\\u${firstCharCode.toString(16)}` }),
                    invalidToken
                );
                this._consumeTokensUntilType([TokenType.NewLine]);
                break;
            }

            const smallStatement = this._parseSmallStatement();
            statement.statements.push(smallStatement);
            smallStatement.parent = statement;
            extendRange(statement, smallStatement);

            if (smallStatement.nodeType === ParseNodeType.Error) {
                // No need to log an error here. We assume that
                // it was already logged by _parseSmallStatement.
                break;
            }

            // Consume the semicolon if present.
            if (!this._consumeTokenIfType(TokenType.Semicolon)) {
                break;
            }

            const nextTokenType = this._peekTokenType();
            if (nextTokenType === TokenType.NewLine || nextTokenType === TokenType.EndOfStream) {
                break;
            }
        }

        if (!this._consumeTokenIfType(TokenType.NewLine)) {
            this._addError(Localizer.Diagnostic.expectedNewlineOrSemicolon(), this._peekToken());
        }

        return statement;
    }

    // small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
    //             import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
    // flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
    // import_stmt: import_name | import_from
    private _parseSmallStatement(): ParseNode {
        switch (this._peekKeywordType()) {
            case KeywordType.Pass:
                return this._parsePassStatement();

            case KeywordType.Break:
                return this._parseBreakStatement();

            case KeywordType.Continue:
                return this._parseContinueStatement();

            case KeywordType.Return:
                return this._parseReturnStatement();

            case KeywordType.From:
                return this._parseFromStatement();

            case KeywordType.Import:
                // ! Cython
                return this._parseImportStatement(KeywordType.Import);

            case KeywordType.Global:
                return this._parseGlobalStatement();

            case KeywordType.Nonlocal:
                return this._parseNonlocalStatement();

            case KeywordType.Raise:
                return this._parseRaiseStatement();

            case KeywordType.Assert:
                return this._parseAssertStatement();

            case KeywordType.Del:
                return this._parseDelStatement();

            case KeywordType.Yield:
                return this._parseYieldExpression();

            case KeywordType.Type: {
                // Type is considered a "soft" keyword, so we will treat it
                // as an identifier if it is followed by an unexpected token.

                const peekToken1 = this._peekToken(1);
                const peekToken2 = this._peekToken(2);
                let isInvalidTypeToken = true;

                if (peekToken1.type === TokenType.Identifier || peekToken1.type === TokenType.Keyword) {
                    if (peekToken2.type === TokenType.OpenBracket) {
                        isInvalidTypeToken = false;
                    } else if (
                        peekToken2.type === TokenType.Operator &&
                        (peekToken2 as OperatorToken).operatorType === OperatorType.Assign
                    ) {
                        isInvalidTypeToken = false;
                    }
                }

                if (!isInvalidTypeToken) {
                    return this._parseTypeAliasStatement();
                }
                break;
            }

            // ! Cython
            case KeywordType.Cimport:
                return this._parseImportStatement(KeywordType.Cimport);
        }

        return this._parseExpressionStatement();
    }

    private _makeExpressionOrTuple(
        exprListResult: ListResult<ExpressionNode>,
        enclosedInParens: boolean
    ): ExpressionNode {
        // A single-element tuple with no trailing comma is simply an expression
        // that's surrounded by parens.
        if (exprListResult.list.length === 1 && !exprListResult.trailingComma) {
            if (exprListResult.list[0].nodeType === ParseNodeType.Unpack) {
                this._addError(Localizer.Diagnostic.unpackOperatorNotAllowed(), exprListResult.list[0]);
            }
            return exprListResult.list[0];
        }

        // To accommodate empty tuples ("()"), we will reach back to get
        // the opening parenthesis as the opening token.

        const tupleStartRange: TextRange =
            exprListResult.list.length > 0 ? exprListResult.list[0] : this._peekToken(-1);

        const tupleNode = TupleNode.create(tupleStartRange, enclosedInParens);
        tupleNode.expressions = exprListResult.list;
        if (exprListResult.list.length > 0) {
            exprListResult.list.forEach((expr) => {
                expr.parent = tupleNode;
            });
            extendRange(tupleNode, exprListResult.list[exprListResult.list.length - 1]);
        }

        return tupleNode;
    }

    private _parseExpressionListAsPossibleTuple(
        errorCategory: ErrorExpressionCategory,
        errorString: string,
        errorToken: Token
    ): ExpressionNode {
        if (this._isNextTokenNeverExpression()) {
            this._addError(errorString, errorToken);
            return ErrorNode.create(errorToken, errorCategory);
        }

        const exprListResult = this._parseExpressionList(/* allowStar */ true);
        if (exprListResult.parseError) {
            return exprListResult.parseError;
        }
        return this._makeExpressionOrTuple(exprListResult, /* enclosedInParens */ false);
    }

    private _parseTestListAsExpression(errorCategory: ErrorExpressionCategory, errorString: string): ExpressionNode {
        if (this._isNextTokenNeverExpression()) {
            return this._handleExpressionParseError(errorCategory, errorString);
        }

        const exprListResult = this._parseTestExpressionList();
        if (exprListResult.parseError) {
            return exprListResult.parseError;
        }
        return this._makeExpressionOrTuple(exprListResult, /* enclosedInParens */ false);
    }

    private _parseTestOrStarListAsExpression(
        allowAssignmentExpression: boolean,
        allowMultipleUnpack: boolean,
        errorCategory: ErrorExpressionCategory,
        errorString: string
    ): ExpressionNode {
        if (this._isNextTokenNeverExpression()) {
            return this._handleExpressionParseError(errorCategory, errorString);
        }

        const exprListResult = this._parseTestOrStarExpressionList(allowAssignmentExpression, allowMultipleUnpack);
        if (exprListResult.parseError) {
            return exprListResult.parseError;
        }
        return this._makeExpressionOrTuple(exprListResult, /* enclosedInParens */ false);
    }

    private _parseExpressionList(allowStar: boolean): ListResult<ExpressionNode> {
        return this._parseExpressionListGeneric(() => this._parseExpression(allowStar));
    }

    // testlist: test (',' test)* [',']
    private _parseTestExpressionList(): ListResult<ExpressionNode> {
        return this._parseExpressionListGeneric(() => this._parseTestExpression(/* allowAssignmentExpression */ false));
    }

    private _parseTestOrStarExpressionList(
        allowAssignmentExpression: boolean,
        allowMultipleUnpack: boolean
    ): ListResult<ExpressionNode> {
        const exprListResult = this._parseExpressionListGeneric(() =>
            this._parseTestOrStarExpression(allowAssignmentExpression)
        );

        if (!allowMultipleUnpack && !exprListResult.parseError) {
            let sawStar = false;
            for (const expr of exprListResult.list) {
                if (expr.nodeType === ParseNodeType.Unpack) {
                    if (sawStar) {
                        this._addError(Localizer.Diagnostic.duplicateUnpack(), expr);
                        break;
                    }
                    sawStar = true;
                }
            }
        }

        return exprListResult;
    }

    // exp_or_star: expr | star_expr
    // expr: xor_expr ('|' xor_expr)*
    // star_expr: '*' expr
    private _parseExpression(allowUnpack: boolean): ExpressionNode {
        const startToken = this._peekToken();

        if (allowUnpack && this._consumeTokenIfOperator(OperatorType.Multiply)) {
            return UnpackNode.create(startToken, this._parseExpression(/* allowUnpack */ false));
        }

        return this._parseBitwiseOrExpression();
    }

    // test_or_star: test | star_expr
    private _parseTestOrStarExpression(allowAssignmentExpression: boolean): ExpressionNode {
        if (this._peekOperatorType() === OperatorType.Multiply) {
            return this._parseExpression(/* allowUnpack */ true);
        }

        return this._parseTestExpression(allowAssignmentExpression);
    }

    // test: or_test ['if' or_test 'else' test] | lambdef
    private _parseTestExpression(allowAssignmentExpression: boolean): ExpressionNode {
        if (this._peekKeywordType() === KeywordType.Lambda) {
            return this._parseLambdaExpression();
        }

        // ! Cython
        // Parse new expression
        const token = this._peekToken();
        if (this._consumeTokenIfKeyword(KeywordType.New)) {
            return CNewNode.create(token, this._parseAssignmentExpression(!allowAssignmentExpression));
        }

        const ifExpr = this._parseAssignmentExpression(!allowAssignmentExpression);
        if (ifExpr.nodeType === ParseNodeType.Error) {
            return ifExpr;
        }

        if (!this._consumeTokenIfKeyword(KeywordType.If)) {
            return ifExpr;
        }

        const testExpr = this._parseOrTest();
        if (testExpr.nodeType === ParseNodeType.Error) {
            return testExpr;
        }

        if (!this._consumeTokenIfKeyword(KeywordType.Else)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingElse,
                Localizer.Diagnostic.expectedElse()
            );
        }

        const elseExpr = this._parseTestExpression(/* allowAssignmentExpression */ true);
        if (elseExpr.nodeType === ParseNodeType.Error) {
            return elseExpr;
        }

        return TernaryNode.create(ifExpr, testExpr, elseExpr);
    }

    // assign_expr: NAME := test
    private _parseAssignmentExpression(disallowAssignmentExpression = false) {
        const leftExpr = this._parseOrTest();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        if (leftExpr.nodeType !== ParseNodeType.Name) {
            return leftExpr;
        }

        const walrusToken = this._peekToken();
        if (!this._consumeTokenIfOperator(OperatorType.Walrus)) {
            return leftExpr;
        }

        if (!this._assignmentExpressionsAllowed || this._isParsingTypeAnnotation || disallowAssignmentExpression) {
            this._addError(Localizer.Diagnostic.walrusNotAllowed(), walrusToken);
        }

        if (this._getLanguageVersion() < PythonVersion.V3_8) {
            this._addError(Localizer.Diagnostic.walrusIllegal(), walrusToken);
        }

        const rightExpr = this._parseTestExpression(/* allowAssignmentExpression */ false);

        return AssignmentExpressionNode.create(leftExpr, rightExpr);
    }

    // or_test: and_test ('or' and_test)*
    private _parseOrTest(): ExpressionNode {
        let leftExpr = this._parseAndTest();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        while (true) {
            const peekToken = this._peekToken();
            if (!this._consumeTokenIfKeyword(KeywordType.Or)) {
                break;
            }
            const rightExpr = this._parseAndTest();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, OperatorType.Or);
        }

        return leftExpr;
    }

    // and_test: not_test ('and' not_test)*
    private _parseAndTest(): ExpressionNode {
        let leftExpr = this._parseNotTest();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        while (true) {
            const peekToken = this._peekToken();
            if (!this._consumeTokenIfKeyword(KeywordType.And)) {
                break;
            }
            const rightExpr = this._parseNotTest();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, OperatorType.And);
        }

        return leftExpr;
    }

    // not_test: 'not' not_test | comparison
    private _parseNotTest(): ExpressionNode {
        const notToken = this._peekToken();
        if (this._consumeTokenIfKeyword(KeywordType.Not)) {
            const notExpr = this._parseNotTest();
            return this._createUnaryOperationNode(notToken, notExpr, OperatorType.Not);
        }

        return this._parseComparison();
    }

    // comparison: expr (comp_op expr)*
    // comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
    private _parseComparison(): ExpressionNode {
        let leftExpr = this._parseBitwiseOrExpression();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        while (true) {
            let comparisonOperator: OperatorType | undefined;
            const peekToken = this._peekToken();

            if (Tokenizer.isOperatorComparison(this._peekOperatorType())) {
                comparisonOperator = this._peekOperatorType();
                if (comparisonOperator === OperatorType.LessOrGreaterThan) {
                    this._addError(Localizer.Diagnostic.operatorLessOrGreaterDeprecated(), peekToken);
                    comparisonOperator = OperatorType.NotEquals;
                }
                this._getNextToken();
            } else if (this._consumeTokenIfKeyword(KeywordType.In)) {
                comparisonOperator = OperatorType.In;
            } else if (this._consumeTokenIfKeyword(KeywordType.Is)) {
                if (this._consumeTokenIfKeyword(KeywordType.Not)) {
                    comparisonOperator = OperatorType.IsNot;
                } else {
                    comparisonOperator = OperatorType.Is;
                }
            } else if (this._peekKeywordType() === KeywordType.Not) {
                const tokenAfterNot = this._peekToken(1);
                if (
                    tokenAfterNot.type === TokenType.Keyword &&
                    (tokenAfterNot as KeywordToken).keywordType === KeywordType.In
                ) {
                    this._getNextToken();
                    this._getNextToken();
                    comparisonOperator = OperatorType.NotIn;
                }
            }

            if (comparisonOperator === undefined) {
                break;
            }

            const rightExpr = this._parseComparison();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, comparisonOperator);
        }

        return leftExpr;
    }

    // expr: xor_expr ('|' xor_expr)*
    private _parseBitwiseOrExpression(): ExpressionNode {
        let leftExpr = this._parseBitwiseXorExpression();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        while (true) {
            const peekToken = this._peekToken();
            if (!this._consumeTokenIfOperator(OperatorType.BitwiseOr)) {
                break;
            }
            const rightExpr = this._parseBitwiseXorExpression();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, OperatorType.BitwiseOr);
        }

        return leftExpr;
    }

    // xor_expr: and_expr ('^' and_expr)*
    private _parseBitwiseXorExpression(): ExpressionNode {
        let leftExpr = this._parseBitwiseAndExpression();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        while (true) {
            const peekToken = this._peekToken();
            if (!this._consumeTokenIfOperator(OperatorType.BitwiseXor)) {
                break;
            }
            const rightExpr = this._parseBitwiseAndExpression();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, OperatorType.BitwiseXor);
        }

        return leftExpr;
    }

    // and_expr: shift_expr ('&' shift_expr)*
    private _parseBitwiseAndExpression(): ExpressionNode {
        let leftExpr = this._parseShiftExpression();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        while (true) {
            const peekToken = this._peekToken();
            if (!this._consumeTokenIfOperator(OperatorType.BitwiseAnd)) {
                break;
            }
            const rightExpr = this._parseShiftExpression();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, OperatorType.BitwiseAnd);
        }

        return leftExpr;
    }

    // shift_expr: arith_expr (('<<'|'>>') arith_expr)*
    private _parseShiftExpression(): ExpressionNode {
        let leftExpr = this._parseArithmeticExpression();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        let peekToken = this._peekToken();
        let nextOperator = this._peekOperatorType();
        while (nextOperator === OperatorType.LeftShift || nextOperator === OperatorType.RightShift) {
            this._getNextToken();
            const rightExpr = this._parseArithmeticExpression();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, nextOperator);
            peekToken = this._peekToken();
            nextOperator = this._peekOperatorType();
        }

        return leftExpr;
    }

    // arith_expr: term (('+'|'-') term)*
    private _parseArithmeticExpression(): ExpressionNode {
        let leftExpr = this._parseArithmeticTerm();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        let peekToken = this._peekToken();
        let nextOperator = this._peekOperatorType();
        while (nextOperator === OperatorType.Add || nextOperator === OperatorType.Subtract) {
            this._getNextToken();
            const rightExpr = this._parseArithmeticTerm();
            if (rightExpr.nodeType === ParseNodeType.Error) {
                return rightExpr;
            }

            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, nextOperator);
            peekToken = this._peekToken();
            nextOperator = this._peekOperatorType();
        }

        return leftExpr;
    }

    // term: factor (('*'|'@'|'/'|'%'|'//') factor)*
    private _parseArithmeticTerm(): ExpressionNode {
        let leftExpr = this._parseArithmeticFactor();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        let peekToken = this._peekToken();
        let nextOperator = this._peekOperatorType();
        while (
            nextOperator === OperatorType.Multiply ||
            nextOperator === OperatorType.MatrixMultiply ||
            nextOperator === OperatorType.Divide ||
            nextOperator === OperatorType.Mod ||
            nextOperator === OperatorType.FloorDivide
        ) {
            this._getNextToken();
            const rightExpr = this._parseArithmeticFactor();
            leftExpr = this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, nextOperator);
            peekToken = this._peekToken();
            nextOperator = this._peekOperatorType();
        }

        return leftExpr;
    }

    // factor: ('+'|'-'|'~') factor | power
    // power: atom_expr ['**' factor]
    private _parseArithmeticFactor(): ExpressionNode {
        const nextToken = this._peekToken();
        const nextOperator = this._peekOperatorType();
        if (
            nextOperator === OperatorType.Add ||
            nextOperator === OperatorType.Subtract ||
            nextOperator === OperatorType.BitwiseInvert
        ) {
            this._getNextToken();
            const expression = this._parseArithmeticFactor();
            return this._createUnaryOperationNode(nextToken, expression, nextOperator);
        }

        const leftExpr = this._parseAtomExpression();
        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        const peekToken = this._peekToken();
        if (this._consumeTokenIfOperator(OperatorType.Power)) {
            const rightExpr = this._parseArithmeticFactor();
            return this._createBinaryOperationNode(leftExpr, rightExpr, peekToken, OperatorType.Power);
        }

        return leftExpr;
    }

    // Determines whether the expression refers to a type exported by the typing
    // or typing_extensions modules. We can directly evaluate the types at binding
    // time. We assume here that the code isn't making use of some custom type alias
    // to refer to the typing types.
    private _isTypingAnnotation(typeAnnotation: ExpressionNode, name: string): boolean {
        if (typeAnnotation.nodeType === ParseNodeType.Name) {
            const alias = this._typingSymbolAliases.get(typeAnnotation.value);
            if (alias === name) {
                return true;
            }
        } else if (typeAnnotation.nodeType === ParseNodeType.MemberAccess) {
            if (
                typeAnnotation.leftExpression.nodeType === ParseNodeType.Name &&
                typeAnnotation.memberName.value === name
            ) {
                const baseName = typeAnnotation.leftExpression.value;
                return this._typingImportAliases.some((alias) => alias === baseName);
            }
        }

        return false;
    }

    // atom_expr: ['await'] atom trailer*
    // trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
    private _parseAtomExpression(): ExpressionNode {
        let awaitToken: KeywordToken | undefined;
        if (this._peekKeywordType() === KeywordType.Await && !this._isParsingTypeAnnotation) {
            awaitToken = this._getKeywordToken(KeywordType.Await);
            if (this._getLanguageVersion() < PythonVersion.V3_5) {
                this._addError(Localizer.Diagnostic.awaitIllegal(), awaitToken);
            }
        }

        let atomExpression = this._parseAtom();
        if (atomExpression.nodeType === ParseNodeType.Error) {
            return atomExpression;
        }

        // Consume trailers.
        while (true) {
            // Is it a function call?
            const startOfTrailerToken = this._peekToken();

            // ! Cython sizeof
            // check for possible sizeof call
            if (
                this._peekTokenType() === TokenType.OpenParenthesis &&
                atomExpression.nodeType === ParseNodeType.Name &&
                (atomExpression as NameNode).value === 'sizeof'
            ) {
                // sizeof cannot be redefined so we can evaluate it before the type evaluator
                this._getNextToken();
                const param = this._parseCParameterAnnotation();
                const closeParen = this._peekToken();
                const sizeOf = CSizeOfNode.create(atomExpression, param);
                if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                    this._addError(Localizer.Diagnostic.expectedCloseParen(), closeParen);
                    this._consumeTokensUntilType([TokenType.NewLine]);
                } else {
                    extendRange(sizeOf, closeParen);
                }
                atomExpression = sizeOf;
            } else if (this._consumeTokenIfType(TokenType.OpenParenthesis)) {
                // Generally, function calls are not allowed within type annotations,
                // but they are permitted in "Annotated" annotations.
                const wasParsingTypeAnnotation = this._isParsingTypeAnnotation;
                this._isParsingTypeAnnotation = false;

                const argListResult = this._parseArgList();
                const callNode = CallNode.create(atomExpression, argListResult.args, argListResult.trailingComma);

                if (argListResult.args.length > 1 || argListResult.trailingComma) {
                    argListResult.args.forEach((arg) => {
                        if (arg.valueExpression.nodeType === ParseNodeType.ListComprehension) {
                            if (!arg.valueExpression.isParenthesized) {
                                this._addError(Localizer.Diagnostic.generatorNotParenthesized(), arg.valueExpression);
                            }
                        }
                    });
                }

                const nextToken = this._peekToken();
                let isArgListTerminated = false;
                if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                    this._addError(Localizer.Diagnostic.expectedCloseParen(), startOfTrailerToken);

                    // Consume the remainder of tokens on the line for error
                    // recovery.
                    this._consumeTokensUntilType([TokenType.NewLine]);

                    // Extend the node's range to include the rest of the line.
                    // This helps the signatureHelpProvider.
                    extendRange(callNode, this._peekToken());
                } else {
                    extendRange(callNode, nextToken);
                    isArgListTerminated = true;
                }

                this._isParsingTypeAnnotation = wasParsingTypeAnnotation;

                if (this._isParsingTypeAnnotation) {
                    const diag = new DiagnosticAddendum();
                    if (atomExpression.nodeType === ParseNodeType.Name && atomExpression.value === 'type') {
                        diag.addMessage(Localizer.DiagnosticAddendum.useTypeInstead());
                        this._addError(Localizer.Diagnostic.typeCallNotAllowed() + diag.getString(), callNode);
                    }
                }

                atomExpression = callNode;

                if (atomExpression.maxChildDepth !== undefined && atomExpression.maxChildDepth >= maxChildNodeDepth) {
                    atomExpression = ErrorNode.create(atomExpression, ErrorExpressionCategory.MaxDepthExceeded);
                    this._addError(Localizer.Diagnostic.maxParseDepthExceeded(), atomExpression);
                }

                // If the argument list wasn't terminated, break out of the loop
                if (!isArgListTerminated) {
                    break;
                }
            } else if (this._consumeTokenIfType(TokenType.OpenBracket)) {
                // Is it an index operator?

                // This is an unfortunate hack that's necessary to accommodate 'Literal'
                // and 'Annotated' type annotations properly. We need to suspend treating
                // strings as type annotations within a Literal or Annotated subscript.
                const wasParsingIndexTrailer = this._isParsingIndexTrailer;
                const wasParsingTypeAnnotation = this._isParsingTypeAnnotation;

                if (
                    this._isTypingAnnotation(atomExpression, 'Literal') ||
                    this._isTypingAnnotation(atomExpression, 'Annotated')
                ) {
                    this._isParsingTypeAnnotation = false;
                }

                this._isParsingIndexTrailer = true;
                const subscriptList = this._parseSubscriptList();
                this._isParsingTypeAnnotation = wasParsingTypeAnnotation;
                this._isParsingIndexTrailer = wasParsingIndexTrailer;

                const closingToken = this._peekToken();

                const indexNode = IndexNode.create(
                    atomExpression,
                    subscriptList.list,
                    subscriptList.trailingComma,
                    closingToken
                );
                extendRange(indexNode, indexNode);

                if (!this._consumeTokenIfType(TokenType.CloseBracket)) {
                    // Handle the error case, but don't use the error node in this
                    // case because it creates problems for the completion provider.
                    this._handleExpressionParseError(
                        ErrorExpressionCategory.MissingIndexCloseBracket,
                        Localizer.Diagnostic.expectedCloseBracket(),
                        startOfTrailerToken,
                        indexNode
                    );
                }

                atomExpression = indexNode;

                if (atomExpression.maxChildDepth !== undefined && atomExpression.maxChildDepth >= maxChildNodeDepth) {
                    atomExpression = ErrorNode.create(atomExpression, ErrorExpressionCategory.MaxDepthExceeded);
                    this._addError(Localizer.Diagnostic.maxParseDepthExceeded(), atomExpression);
                }
            } else if (this._consumeTokenIfType(TokenType.Dot)) {
                // Is it a member access?
                const memberName = this._getTokenIfIdentifier();
                if (!memberName) {
                    return this._handleExpressionParseError(
                        ErrorExpressionCategory.MissingMemberAccessName,
                        Localizer.Diagnostic.expectedMemberName(),
                        startOfTrailerToken,
                        atomExpression,
                        [TokenType.Keyword]
                    );
                }
                atomExpression = MemberAccessNode.create(atomExpression, NameNode.create(memberName));

                if (atomExpression.maxChildDepth !== undefined && atomExpression.maxChildDepth >= maxChildNodeDepth) {
                    atomExpression = ErrorNode.create(atomExpression, ErrorExpressionCategory.MaxDepthExceeded);
                    this._addError(Localizer.Diagnostic.maxParseDepthExceeded(), atomExpression);
                }
            } else {
                break;
            }
        }

        if (awaitToken) {
            return AwaitNode.create(awaitToken, atomExpression);
        }

        return atomExpression;
    }

    // subscriptlist: subscript (',' subscript)* [',']
    private _parseSubscriptList(): SubscriptListResult {
        const argList: ArgumentNode[] = [];
        let sawKeywordArg = false;
        let trailingComma = false;

        while (true) {
            const firstToken = this._peekToken();

            if (firstToken.type !== TokenType.Colon && this._isNextTokenNeverExpression()) {
                break;
            }

            let argType = ArgumentCategory.Simple;
            if (this._consumeTokenIfOperator(OperatorType.Multiply)) {
                argType = ArgumentCategory.UnpackedList;
            } else if (this._consumeTokenIfOperator(OperatorType.Power)) {
                argType = ArgumentCategory.UnpackedDictionary;
            }

            const startOfSubscriptIndex = this._tokenIndex;
            let valueExpr = this._parsePossibleSlice();
            let nameIdentifier: IdentifierToken | undefined;

            // Is this a keyword argument?
            if (argType === ArgumentCategory.Simple) {
                if (this._consumeTokenIfOperator(OperatorType.Assign)) {
                    const nameExpr = valueExpr;
                    valueExpr = this._parsePossibleSlice();

                    if (nameExpr.nodeType === ParseNodeType.Name) {
                        nameIdentifier = nameExpr.token;
                    } else {
                        this._addError(Localizer.Diagnostic.expectedParamName(), nameExpr);
                    }
                } else if (
                    valueExpr.nodeType === ParseNodeType.Name &&
                    this._peekOperatorType() === OperatorType.Walrus
                ) {
                    this._tokenIndex = startOfSubscriptIndex;
                    valueExpr = this._parseTestExpression(/* allowAssignmentExpression */ true);

                    // Python 3.10 and newer allow assignment expressions to be used inside of a subscript.
                    if (!this._parseOptions.isStubFile && this._getLanguageVersion() < PythonVersion.V3_10) {
                        this._addError(Localizer.Diagnostic.assignmentExprInSubscript(), valueExpr);
                    }
                }
            }

            const argNode = ArgumentNode.create(firstToken, valueExpr, argType);
            if (nameIdentifier) {
                argNode.name = NameNode.create(nameIdentifier);
                argNode.name.parent = argNode;
            }

            if (argNode.name) {
                sawKeywordArg = true;
            } else if (sawKeywordArg && argNode.argumentCategory === ArgumentCategory.Simple) {
                this._addError(Localizer.Diagnostic.positionArgAfterNamedArg(), argNode);
            }
            argList.push(argNode);

            if (argNode.name) {
                this._addError(Localizer.Diagnostic.keywordSubscriptIllegal(), argNode.name);
            }

            if (argType !== ArgumentCategory.Simple) {
                const unpackAllowed =
                    this._parseOptions.isStubFile ||
                    this._isParsingQuotedText ||
                    this._getLanguageVersion() >= PythonVersion.V3_11;

                if (argType === ArgumentCategory.UnpackedDictionary || !unpackAllowed) {
                    this._addError(Localizer.Diagnostic.unpackedSubscriptIllegal(), argNode);
                }
            }

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                trailingComma = false;
                break;
            }

            trailingComma = true;
        }

        // An empty subscript list is illegal.
        if (argList.length === 0) {
            const errorNode = this._handleExpressionParseError(
                ErrorExpressionCategory.MissingIndexOrSlice,
                Localizer.Diagnostic.expectedSliceIndex(),
                /* targetToken */ undefined,
                /* childNode */ undefined,
                [TokenType.CloseBracket]
            );
            argList.push(ArgumentNode.create(this._peekToken(), errorNode, ArgumentCategory.Simple));
        }

        return {
            list: argList,
            trailingComma,
        };
    }

    // subscript: test | [test] ':' [test] [sliceop]
    // sliceop: ':' [test]
    private _parsePossibleSlice(): ExpressionNode {
        const firstToken = this._peekToken();
        const sliceExpressions: (ExpressionNode | undefined)[] = [undefined, undefined, undefined];
        let sliceIndex = 0;
        let sawColon = false;

        while (true) {
            const nextTokenType = this._peekTokenType();
            if (nextTokenType === TokenType.CloseBracket || nextTokenType === TokenType.Comma) {
                break;
            }

            if (nextTokenType !== TokenType.Colon) {
                sliceExpressions[sliceIndex] = this._parseTestExpression(/* allowAssignmentExpression */ false);
            }
            sliceIndex++;

            if (sliceIndex >= 3 || !this._consumeTokenIfType(TokenType.Colon)) {
                break;
            }
            sawColon = true;
        }

        // If this was a simple expression with no colons return it.
        if (!sawColon) {
            if (sliceExpressions[0]) {
                return sliceExpressions[0];
            }

            return ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingIndexOrSlice);
        }

        const sliceNode = SliceNode.create(firstToken);
        sliceNode.startValue = sliceExpressions[0];
        if (sliceNode.startValue) {
            sliceNode.startValue.parent = sliceNode;
        }
        sliceNode.endValue = sliceExpressions[1];
        if (sliceNode.endValue) {
            sliceNode.endValue.parent = sliceNode;
        }
        sliceNode.stepValue = sliceExpressions[2];
        if (sliceNode.stepValue) {
            sliceNode.stepValue.parent = sliceNode;
        }
        const extension = sliceExpressions[2] || sliceExpressions[1] || sliceExpressions[0];
        if (extension) {
            extendRange(sliceNode, extension);
        }

        return sliceNode;
    }

    // arglist: argument (',' argument)*  [',']
    private _parseArgList(): ArgListResult {
        const argList: ArgumentNode[] = [];
        let sawKeywordArg = false;
        let trailingComma = false;

        while (true) {
            const nextTokenType = this._peekTokenType();
            if (
                nextTokenType === TokenType.CloseParenthesis ||
                nextTokenType === TokenType.NewLine ||
                nextTokenType === TokenType.EndOfStream
            ) {
                break;
            }

            trailingComma = false;
            const arg = this._parseArgument();
            if (arg.name) {
                sawKeywordArg = true;
            } else if (sawKeywordArg && arg.argumentCategory === ArgumentCategory.Simple) {
                this._addError(Localizer.Diagnostic.positionArgAfterNamedArg(), arg);
            }
            argList.push(arg);

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                break;
            }

            trailingComma = true;
        }

        return { args: argList, trailingComma };
    }

    // argument: ( test [comp_for] |
    //             test '=' test |
    //             '**' test |
    //             '*' test )
    private _parseArgument(): ArgumentNode {
        const firstToken = this._peekToken();

        let argType = ArgumentCategory.Simple;
        if (this._consumeTokenIfOperator(OperatorType.Multiply)) {
            argType = ArgumentCategory.UnpackedList;
        } else if (this._consumeTokenIfOperator(OperatorType.Power)) {
            argType = ArgumentCategory.UnpackedDictionary;
        }

        let valueExpr = this._parseTestExpression(/* allowAssignmentExpression */ true);
        let nameIdentifier: IdentifierToken | undefined;

        if (argType === ArgumentCategory.Simple) {
            if (this._consumeTokenIfOperator(OperatorType.Assign)) {
                const nameExpr = valueExpr;
                valueExpr = this._parseTestExpression(/* allowAssignmentExpression */ false);

                if (nameExpr.nodeType === ParseNodeType.Name) {
                    nameIdentifier = nameExpr.token;
                } else {
                    this._addError(Localizer.Diagnostic.expectedParamName(), nameExpr);
                }
            } else {
                const listComp = this._tryParseListComprehension(valueExpr);
                if (listComp) {
                    valueExpr = listComp;
                }
            }
        }

        const argNode = ArgumentNode.create(firstToken, valueExpr, argType);
        if (nameIdentifier) {
            argNode.name = NameNode.create(nameIdentifier);
            argNode.name.parent = argNode;
        }

        return argNode;
    }

    // atom: ('(' [yield_expr | testlist_comp] ')' |
    //     '[' [testlist_comp] ']' |
    //     '{' [dictorsetmaker] '}' |
    //     NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False' | '__debug__')
    private _parseAtom(): ExpressionNode {
        const nextToken = this._peekToken();

        if (nextToken.type === TokenType.Ellipsis) {
            return EllipsisNode.create(this._getNextToken());
        }

        if (nextToken.type === TokenType.Number) {
            return NumberNode.create(this._getNextToken() as NumberToken);
        }

        if (nextToken.type === TokenType.Identifier) {
            return NameNode.create(this._getNextToken() as IdentifierToken);
        }

        if (nextToken.type === TokenType.String) {
            return this._parseStringList();
        }

        if (nextToken.type === TokenType.Backtick) {
            this._getNextToken();

            // Atoms with backticks are no longer allowed in Python 3.x, but they
            // were a thing in Python 2.x. We'll parse them to improve parse recovery
            // and emit an error.
            this._addError(Localizer.Diagnostic.backticksIllegal(), nextToken);

            const expressionNode = this._parseTestListAsExpression(
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedExpr()
            );

            this._consumeTokenIfType(TokenType.Backtick);
            return expressionNode;
        }

        if (nextToken.type === TokenType.OpenParenthesis) {
            const possibleTupleNode = this._parseTupleAtom();
            if (
                possibleTupleNode.nodeType === ParseNodeType.Tuple &&
                this._isParsingTypeAnnotation &&
                !this._isParsingIndexTrailer
            ) {
                // This is allowed inside of an index trailer, specifically
                // to support Tuple[()], which is the documented way to annotate
                // a zero-length tuple.
                const diag = new DiagnosticAddendum();
                diag.addMessage(Localizer.DiagnosticAddendum.useTupleInstead());
                this._addError(Localizer.Diagnostic.tupleInAnnotation() + diag.getString(), possibleTupleNode);
            }

            if (possibleTupleNode.nodeType === ParseNodeType.BinaryOperation) {
                // Mark the binary expression as parenthesized so we don't attempt
                // to use comparison chaining, which isn't appropriate when the
                // expression is parenthesized.
                possibleTupleNode.parenthesized = true;
            }

            if (possibleTupleNode.nodeType === ParseNodeType.StringList) {
                possibleTupleNode.isParenthesized = true;
            }

            if (possibleTupleNode.nodeType === ParseNodeType.ListComprehension) {
                possibleTupleNode.isParenthesized = true;
            }

            return possibleTupleNode;
        } else if (nextToken.type === TokenType.OpenBracket) {
            const listNode = this._parseListAtom();
            if (this._isParsingTypeAnnotation && !this._isParsingIndexTrailer) {
                const diag = new DiagnosticAddendum();
                diag.addMessage(Localizer.DiagnosticAddendum.useListInstead());
                this._addError(Localizer.Diagnostic.listInAnnotation() + diag.getString(), listNode);
            }
            return listNode;
        } else if (nextToken.type === TokenType.OpenCurlyBrace) {
            const dictNode = this._parseDictionaryOrSetAtom();
            if (this._isParsingTypeAnnotation) {
                const diag = new DiagnosticAddendum();
                diag.addMessage(Localizer.DiagnosticAddendum.useDictInstead());
                this._addError(Localizer.Diagnostic.dictInAnnotation() + diag.getString(), dictNode);
            }
            return dictNode;
        }

        if (nextToken.type === TokenType.Keyword) {
            const keywordToken = nextToken as KeywordToken;
            if (
                keywordToken.keywordType === KeywordType.False ||
                keywordToken.keywordType === KeywordType.True ||
                keywordToken.keywordType === KeywordType.Debug ||
                keywordToken.keywordType === KeywordType.None ||
                // ! Cython
                keywordToken.keywordType === KeywordType.NULL
            ) {
                return ConstantNode.create(this._getNextToken() as KeywordToken);
            }

            // Make an identifier out of the keyword.
            const keywordAsIdentifier = this._getTokenIfIdentifier();
            if (keywordAsIdentifier) {
                return NameNode.create(keywordAsIdentifier);
            }
        }

        // ! Cython
        if (nextToken.type === TokenType.Operator) {
            const op = nextToken as OperatorToken;
            if (op.operatorType === OperatorType.BitwiseAnd) {
                // Address Of
                // TODO: this should really be a unary operator expression
                this._getNextToken();
                return CAddressOfNode.create(op, this._parseAtomExpression());
            } else if (op.operatorType === OperatorType.LessThan) {
                this._getNextToken();
                const typeNode = this._parseCType();
                if (typeNode.nodeType === ParseNodeType.Error) {
                    return typeNode;
                }
                const ptrs = this._parsePointersOrRef(/*allowReference*/ false, /*allowCastClose*/ true);
                typeNode.operators.push(...ptrs);
                if (ptrs.length > 0) {
                    extendRange(typeNode, ptrs[ptrs.length - 1]);
                }
                const closeToken = this._peekToken() as OperatorToken;
                if (this._consumeTokenIfOperator(OperatorType.GreaterThan)) {
                    return CCastNode.create(op, typeNode, closeToken, this._parseAtomExpression());
                } else {
                    this._addError(Localizer.DiagnosticCython.expectedCastClose(), closeToken);
                }
            }
        }

        return this._handleExpressionParseError(
            ErrorExpressionCategory.MissingExpression,
            Localizer.Diagnostic.expectedExpr()
        );
    }

    // Allocates a dummy "error expression" and consumes the remainder
    // of the tokens on the line for error recovery. A partially-completed
    // child node can be passed to help the completion provider determine
    // what to do.
    private _handleExpressionParseError(
        category: ErrorExpressionCategory,
        errorMsg: string,
        targetToken?: Token,
        childNode?: ExpressionNode,
        additionalStopTokens?: TokenType[]
    ): ErrorNode {
        this._addError(errorMsg, targetToken ?? this._peekToken());

        const stopTokens = [TokenType.NewLine];
        if (additionalStopTokens) {
            appendArray(stopTokens, additionalStopTokens);
        }

        // Using token that is not consumed by error node will mess up spans in parse node.
        // Sibling nodes in parse tree shouldn't overlap each other.
        const nextToken = this._peekToken();
        const initialRange: TextRange = stopTokens.some((k) => nextToken.type === k)
            ? targetToken ?? childNode ?? TextRange.create(nextToken.start, /* length */ 0)
            : nextToken;
        const expr = ErrorNode.create(initialRange, category, childNode);
        this._consumeTokensUntilType(stopTokens);

        return expr;
    }

    // lambdef: 'lambda' [varargslist] ':' test
    private _parseLambdaExpression(allowConditional = true): LambdaNode {
        const lambdaToken = this._getKeywordToken(KeywordType.Lambda);

        const argList = this._parseVarArgsList(TokenType.Colon, /* allowAnnotations */ false);

        if (!this._consumeTokenIfType(TokenType.Colon)) {
            this._addError(Localizer.Diagnostic.expectedColon(), this._peekToken());
        }

        let testExpr: ExpressionNode;
        if (allowConditional) {
            testExpr = this._parseTestExpression(/* allowAssignmentExpression */ false);
        } else {
            testExpr = this._tryParseLambdaExpression(/* allowConditional */ false) || this._parseOrTest();
        }

        const lambdaNode = LambdaNode.create(lambdaToken, testExpr);
        lambdaNode.parameters = argList;
        argList.forEach((arg) => {
            arg.parent = lambdaNode;
        });
        return lambdaNode;
    }

    private _tryParseLambdaExpression(allowConditional = true): LambdaNode | undefined {
        if (this._peekKeywordType() !== KeywordType.Lambda) {
            return undefined;
        }

        return this._parseLambdaExpression(allowConditional);
    }

    // ('(' [yield_expr | testlist_comp] ')'
    // testlist_comp: (test | star_expr) (comp_for | (',' (test | star_expr))* [','])
    private _parseTupleAtom(): ExpressionNode {
        const startParen = this._getNextToken();
        assert(startParen.type === TokenType.OpenParenthesis);

        const yieldExpr = this._tryParseYieldExpression();
        if (yieldExpr) {
            if (this._peekTokenType() !== TokenType.CloseParenthesis) {
                return this._handleExpressionParseError(
                    ErrorExpressionCategory.MissingTupleCloseParen,
                    Localizer.Diagnostic.expectedCloseParen(),
                    startParen,
                    yieldExpr
                );
            } else {
                extendRange(yieldExpr, this._getNextToken());
            }

            return yieldExpr;
        }

        const exprListResult = this._parseTestListWithComprehension();
        const tupleOrExpression = this._makeExpressionOrTuple(exprListResult, /* enclosedInParens */ true);
        const isExpression = exprListResult.list.length === 1 && !exprListResult.trailingComma;

        if (!isExpression) {
            extendRange(tupleOrExpression, startParen);
        }

        if (this._peekTokenType() !== TokenType.CloseParenthesis) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingTupleCloseParen,
                Localizer.Diagnostic.expectedCloseParen(),
                startParen,
                exprListResult.parseError ?? tupleOrExpression
            );
        } else {
            const nextToken = this._getNextToken();
            if (!isExpression) {
                extendRange(tupleOrExpression, nextToken);
            }
        }

        return tupleOrExpression;
    }

    // '[' [testlist_comp] ']'
    // testlist_comp: (test | star_expr) (comp_for | (',' (test | star_expr))* [','])
    private _parseListAtom(): ListNode | ErrorNode {
        const startBracket = this._getNextToken();
        assert(startBracket.type === TokenType.OpenBracket);

        const exprListResult = this._parseTestListWithComprehension();
        const closeBracket: Token | undefined = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.CloseBracket)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingListCloseBracket,
                Localizer.Diagnostic.expectedCloseBracket(),
                startBracket,
                exprListResult.parseError ?? _createList()
            );
        }

        return _createList();

        function _createList() {
            const listAtom = ListNode.create(startBracket);

            if (closeBracket) {
                extendRange(listAtom, closeBracket);
            }

            if (exprListResult.list.length > 0) {
                exprListResult.list.forEach((expr) => {
                    expr.parent = listAtom;
                });
                extendRange(listAtom, exprListResult.list[exprListResult.list.length - 1]);
            }

            listAtom.entries = exprListResult.list;
            return listAtom;
        }
    }

    private _parseTestListWithComprehension(): ListResult<ExpressionNode> {
        let sawComprehension = false;

        return this._parseExpressionListGeneric(
            () => {
                let expr = this._parseTestOrStarExpression(/* allowAssignmentExpression */ true);
                const listComp = this._tryParseListComprehension(expr);
                if (listComp) {
                    expr = listComp;
                    sawComprehension = true;
                }
                return expr;
            },
            () => this._isNextTokenNeverExpression(),
            () => sawComprehension
        );
    }

    // '{' [dictorsetmaker] '}'
    // dictorsetmaker: (
    //    (dictentry (comp_for | (',' dictentry)* [',']))
    //    | (setentry (comp_for | (',' setentry)* [',']))
    // )
    // dictentry: (test ':' test | '**' expr)
    // setentry: test | star_expr
    private _parseDictionaryOrSetAtom(): DictionaryNode | SetNode {
        const startBrace = this._getNextToken();
        assert(startBrace.type === TokenType.OpenCurlyBrace);

        const dictionaryEntries: DictionaryEntryNode[] = [];
        const setEntries: ExpressionNode[] = [];
        let isDictionary = false;
        let isSet = false;
        let sawListComprehension = false;
        let isFirstEntry = true;
        let trailingCommaToken: Token | undefined;

        while (true) {
            if (this._peekTokenType() === TokenType.CloseCurlyBrace) {
                break;
            }

            trailingCommaToken = undefined;

            let doubleStarExpression: ExpressionNode | undefined;
            let keyExpression: ExpressionNode | undefined;
            let valueExpression: ExpressionNode | undefined;
            const doubleStar = this._peekToken();

            if (this._consumeTokenIfOperator(OperatorType.Power)) {
                doubleStarExpression = this._parseExpression(/* allowUnpack */ false);
            } else {
                keyExpression = this._parseTestOrStarExpression(/* allowAssignmentExpression */ true);

                if (this._consumeTokenIfType(TokenType.Colon)) {
                    valueExpression = this._parseTestExpression(/* allowAssignmentExpression */ false);
                }
            }

            if (keyExpression && valueExpression) {
                if (keyExpression.nodeType === ParseNodeType.Unpack) {
                    this._addError(Localizer.Diagnostic.unpackInDict(), keyExpression);
                }

                if (isSet) {
                    this._addError(Localizer.Diagnostic.keyValueInSet(), valueExpression);
                } else {
                    const keyEntryNode = DictionaryKeyEntryNode.create(keyExpression, valueExpression);
                    let dictEntry: DictionaryEntryNode = keyEntryNode;
                    const listComp = this._tryParseListComprehension(keyEntryNode);
                    if (listComp) {
                        dictEntry = listComp;
                        sawListComprehension = true;

                        if (!isFirstEntry) {
                            this._addError(Localizer.Diagnostic.comprehensionInDict(), dictEntry);
                        }
                    }
                    dictionaryEntries.push(dictEntry);
                    isDictionary = true;
                }
            } else if (doubleStarExpression) {
                if (isSet) {
                    this._addError(Localizer.Diagnostic.unpackInSet(), doubleStarExpression);
                } else {
                    const listEntryNode = DictionaryExpandEntryNode.create(doubleStarExpression);
                    extendRange(listEntryNode, doubleStar);
                    let expandEntryNode: DictionaryEntryNode = listEntryNode;
                    const listComp = this._tryParseListComprehension(listEntryNode);
                    if (listComp) {
                        expandEntryNode = listComp;
                        sawListComprehension = true;

                        if (!isFirstEntry) {
                            this._addError(Localizer.Diagnostic.comprehensionInDict(), doubleStarExpression);
                        }
                    }
                    dictionaryEntries.push(expandEntryNode);
                    isDictionary = true;
                }
            } else {
                assert(keyExpression !== undefined);
                if (keyExpression) {
                    if (isDictionary) {
                        const missingValueErrorNode = ErrorNode.create(
                            this._peekToken(),
                            ErrorExpressionCategory.MissingDictValue
                        );
                        const keyEntryNode = DictionaryKeyEntryNode.create(keyExpression, missingValueErrorNode);
                        dictionaryEntries.push(keyEntryNode);
                        this._addError(Localizer.Diagnostic.dictKeyValuePairs(), keyExpression);
                    } else {
                        const listComp = this._tryParseListComprehension(keyExpression);
                        if (listComp) {
                            keyExpression = listComp;
                            sawListComprehension = true;

                            if (!isFirstEntry) {
                                this._addError(Localizer.Diagnostic.comprehensionInSet(), keyExpression);
                            }
                        }
                        setEntries.push(keyExpression);
                        isSet = true;
                    }
                }
            }

            // List comprehension statements always end the list.
            if (sawListComprehension) {
                break;
            }

            if (this._peekTokenType() !== TokenType.Comma) {
                break;
            }

            trailingCommaToken = this._getNextToken();

            isFirstEntry = false;
        }

        let closeCurlyBrace: Token | undefined = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.CloseCurlyBrace)) {
            this._addError(Localizer.Diagnostic.expectedCloseBrace(), startBrace);
            closeCurlyBrace = undefined;
        }

        if (isSet) {
            const setAtom = SetNode.create(startBrace);
            if (closeCurlyBrace) {
                extendRange(setAtom, closeCurlyBrace);
            }

            if (setEntries.length > 0) {
                extendRange(setAtom, setEntries[setEntries.length - 1]);
            }

            setEntries.forEach((entry) => {
                entry.parent = setAtom;
            });

            setAtom.entries = setEntries;
            return setAtom;
        }

        const dictionaryAtom = DictionaryNode.create(startBrace);

        if (trailingCommaToken) {
            dictionaryAtom.trailingCommaToken = trailingCommaToken;
            extendRange(dictionaryAtom, trailingCommaToken);
        }

        if (closeCurlyBrace) {
            extendRange(dictionaryAtom, closeCurlyBrace);
        }

        if (dictionaryEntries.length > 0) {
            dictionaryEntries.forEach((entry) => {
                entry.parent = dictionaryAtom;
            });
            extendRange(dictionaryAtom, dictionaryEntries[dictionaryEntries.length - 1]);
        }
        dictionaryAtom.entries = dictionaryEntries;
        return dictionaryAtom;
    }

    private _parseExpressionListGeneric<T extends ParseNode = ExpressionNode>(
        parser: () => T | ErrorNode,
        terminalCheck: () => boolean = () => this._isNextTokenNeverExpression(),
        finalEntryCheck: () => boolean = () => false
    ): ListResult<T> {
        let trailingComma = false;
        const list: T[] = [];
        let parseError: ErrorNode | undefined;

        while (true) {
            if (terminalCheck()) {
                break;
            }

            const expr = parser();
            if (expr.nodeType === ParseNodeType.Error) {
                parseError = expr as ErrorNode;
                break;
            }
            list.push(expr);

            // Should we stop without checking for a trailing comma?
            if (finalEntryCheck()) {
                break;
            }

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                trailingComma = false;
                break;
            }

            trailingComma = true;
        }

        return { trailingComma, list, parseError };
    }

    // expr_stmt: testlist_star_expr (annassign | augassign (yield_expr | testlist) |
    //                     ('=' (yield_expr | testlist_star_expr))*)
    // testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
    // annassign: ':' test ['=' (yield_expr | testlist_star_expr)]
    // augassign: ('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
    //             '<<=' | '>>=' | '**=' | '//=')
    private _parseExpressionStatement(): ExpressionNode {
        let leftExpr = this._parseTestOrStarListAsExpression(
            /* allowAssignmentExpression */ false,
            /* allowMultipleUnpack */ false,
            ErrorExpressionCategory.MissingExpression,
            Localizer.Diagnostic.expectedExpr()
        );
        let annotationExpr: ExpressionNode | undefined;

        if (leftExpr.nodeType === ParseNodeType.Error) {
            return leftExpr;
        }

        // Is this a type annotation assignment?
        if (this._consumeTokenIfType(TokenType.Colon)) {
            annotationExpr = this._parseTypeAnnotation();
            leftExpr = TypeAnnotationNode.create(leftExpr, annotationExpr);

            if (!this._parseOptions.isStubFile && this._getLanguageVersion() < PythonVersion.V3_6) {
                this._addError(Localizer.Diagnostic.varAnnotationIllegal(), annotationExpr);
            }

            if (!this._consumeTokenIfOperator(OperatorType.Assign)) {
                return leftExpr;
            }

            // This is an unfortunate hack that's necessary to accommodate 'TypeAlias'
            // declarations properly. We need to treat this assignment differently than
            // most because the expression on the right side is treated like a type
            // annotation and therefore allows string-literal forward declarations.
            const isTypeAliasDeclaration = this._isTypingAnnotation(annotationExpr, 'TypeAlias');

            const wasParsingTypeAnnotation = this._isParsingTypeAnnotation;
            if (isTypeAliasDeclaration) {
                this._isParsingTypeAnnotation = true;
            }

            const rightExpr =
                this._tryParseYieldExpression() ||
                this._parseTestOrStarListAsExpression(
                    /* allowAssignmentExpression */ false,
                    /* allowMultipleUnpack */ true,
                    ErrorExpressionCategory.MissingExpression,
                    Localizer.Diagnostic.expectedAssignRightHandExpr()
                );

            this._isParsingTypeAnnotation = wasParsingTypeAnnotation;

            return AssignmentNode.create(leftExpr, rightExpr);
        }

        // Is this a simple assignment?
        if (this._consumeTokenIfOperator(OperatorType.Assign)) {
            return this._parseChainAssignments(leftExpr);
        }

        if (Tokenizer.isOperatorAssignment(this._peekOperatorType())) {
            const operatorToken = this._getNextToken() as OperatorToken;

            const rightExpr =
                this._tryParseYieldExpression() ||
                this._parseTestListAsExpression(
                    ErrorExpressionCategory.MissingExpression,
                    Localizer.Diagnostic.expectedBinaryRightHandExpr()
                );

            // Make a shallow copy of the dest expression but give it a new ID.
            const destExpr = Object.assign({}, leftExpr);
            destExpr.id = getNextNodeId();

            return AugmentedAssignmentNode.create(leftExpr, rightExpr, operatorToken.operatorType, destExpr);
        }

        // ! Cython deprecated print
        // this could be a python2 print statement
        // We'll verify this in type evaluator
        if (
            leftExpr.nodeType === ParseNodeType.Name &&
            leftExpr.value === 'print' &&
            this._peekTokenType() !== TokenType.NewLine
        ) {
            const argToken = this._peekToken();
            const nextExpression = this._parseTestExpression(/*allowAssigmentExpression*/ false);
            if (nextExpression.nodeType !== ParseNodeType.Error) {
                const arg = ArgumentNode.create(argToken, nextExpression, ArgumentCategory.Simple);
                leftExpr = CallNode.create(leftExpr, [arg], false);
                leftExpr.possibleDeprecatedPrint = true;
            }
        }

        return leftExpr;
    }

    private _parseChainAssignments(leftExpr: ExpressionNode): ExpressionNode {
        let rightExpr =
            this._tryParseYieldExpression() ||
            this._parseTestOrStarListAsExpression(
                /* allowAssignmentExpression */ false,
                /* allowMultipleUnpack */ true,
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedAssignRightHandExpr()
            );

        if (rightExpr.nodeType === ParseNodeType.Error) {
            return AssignmentNode.create(leftExpr, rightExpr);
        }

        // Recur until we've consumed the entire chain.
        if (this._consumeTokenIfOperator(OperatorType.Assign)) {
            rightExpr = this._parseChainAssignments(rightExpr);
            if (rightExpr.nodeType === ParseNodeType.Error) {
                return rightExpr;
            }
        }

        const assignmentNode = AssignmentNode.create(leftExpr, rightExpr);

        // Look for a type annotation comment at the end of the line.
        const typeAnnotationComment = this._parseVariableTypeAnnotationComment();
        if (typeAnnotationComment) {
            assignmentNode.typeAnnotationComment = typeAnnotationComment;
            assignmentNode.typeAnnotationComment.parent = assignmentNode;
            extendRange(assignmentNode, assignmentNode.typeAnnotationComment);
        }

        return assignmentNode;
    }

    private _parseFunctionTypeAnnotation(): FunctionAnnotationNode | undefined {
        const openParenToken = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            this._addError(Localizer.Diagnostic.expectedOpenParen(), this._peekToken());
            return undefined;
        }

        let paramAnnotations: ExpressionNode[] = [];

        while (true) {
            const nextTokenType = this._peekTokenType();
            if (
                nextTokenType === TokenType.CloseParenthesis ||
                nextTokenType === TokenType.NewLine ||
                nextTokenType === TokenType.EndOfStream
            ) {
                break;
            }

            // Consume "*" or "**" indicators but don't do anything with them.
            // (We don't enforce that these are present, absent, or match
            // the corresponding parameter types.)
            this._consumeTokenIfOperator(OperatorType.Multiply) || this._consumeTokenIfOperator(OperatorType.Power);

            const paramAnnotation = this._parseTypeAnnotation();
            paramAnnotations.push(paramAnnotation);

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                break;
            }
        }

        if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
            this._addError(Localizer.Diagnostic.expectedCloseParen(), openParenToken);
            this._consumeTokensUntilType([TokenType.Colon]);
        }

        if (!this._consumeTokenIfType(TokenType.Arrow)) {
            this._addError(Localizer.Diagnostic.expectedArrow(), this._peekToken());
            return undefined;
        }

        const returnType = this._parseTypeAnnotation();

        let isParamListEllipsis = false;
        if (paramAnnotations.length === 1 && paramAnnotations[0].nodeType === ParseNodeType.Ellipsis) {
            paramAnnotations = [];
            isParamListEllipsis = true;
        }

        return FunctionAnnotationNode.create(openParenToken, isParamListEllipsis, paramAnnotations, returnType);
    }

    private _parseTypeAnnotation(allowUnpack = false): ExpressionNode {
        // Temporary set a flag that indicates we're parsing a type annotation.
        const wasParsingTypeAnnotation = this._isParsingTypeAnnotation;
        this._isParsingTypeAnnotation = true;

        // Allow unpack operators.
        const startToken = this._peekToken();
        const isUnpack = this._consumeTokenIfOperator(OperatorType.Multiply);

        if (isUnpack) {
            if (!allowUnpack) {
                this._addError(Localizer.Diagnostic.unpackInAnnotation(), startToken);
            } else if (
                !this._parseOptions.isStubFile &&
                !this._isParsingQuotedText &&
                this._getLanguageVersion() < PythonVersion.V3_11
            ) {
                this._addError(Localizer.Diagnostic.unpackedSubscriptIllegal(), startToken);
            }
        }

        let result = this._parseTestExpression(/* allowAssignmentExpression */ false);
        if (isUnpack && allowUnpack) {
            result = UnpackNode.create(startToken, result);
        }

        this._isParsingTypeAnnotation = wasParsingTypeAnnotation;

        return result;
    }

    private _reportStringTokenErrors(stringToken: StringToken, unescapedResult: StringTokenUtils.UnescapedString) {
        if (stringToken.flags & StringTokenFlags.Unterminated) {
            this._addError(Localizer.Diagnostic.stringUnterminated(), stringToken);
        }

        if (unescapedResult.nonAsciiInBytes) {
            this._addError(Localizer.Diagnostic.stringNonAsciiBytes(), stringToken);
        }

        if (stringToken.flags & StringTokenFlags.Format) {
            if (this._getLanguageVersion() < PythonVersion.V3_6) {
                this._addError(Localizer.Diagnostic.formatStringIllegal(), stringToken);
            }

            if (stringToken.flags & StringTokenFlags.Bytes) {
                this._addError(Localizer.Diagnostic.formatStringBytes(), stringToken);
            }

            if (stringToken.flags & StringTokenFlags.Unicode) {
                this._addError(Localizer.Diagnostic.formatStringUnicode(), stringToken);
            }
        }
    }

    private _makeStringNode(stringToken: StringToken): StringNode {
        const unescapedResult = StringTokenUtils.getUnescapedString(stringToken);
        this._reportStringTokenErrors(stringToken, unescapedResult);
        return StringNode.create(stringToken, unescapedResult.value, unescapedResult.unescapeErrors.length > 0);
    }

    private _getTypeAnnotationCommentText(): StringToken | undefined {
        if (this._tokenIndex === 0) {
            return undefined;
        }

        const curToken = this._tokenizerOutput!.tokens.getItemAt(this._tokenIndex - 1);
        const nextToken = this._tokenizerOutput!.tokens.getItemAt(this._tokenIndex);

        if (curToken.start + curToken.length === nextToken.start) {
            return undefined;
        }

        const interTokenContents = this._fileContents!.substring(curToken.start + curToken.length, nextToken.start);
        const commentRegEx = /^(\s*#\s*type:\s*)([^\r\n]*)/;
        const match = interTokenContents.match(commentRegEx);
        if (!match) {
            return undefined;
        }

        // Synthesize a string token and StringNode.
        const typeString = match[2];

        // Ignore all "ignore" comments. Include "[" in the regular
        // expression because mypy supports ignore comments of the
        // form ignore[errorCode, ...]. We'll treat these as regular
        // ignore statements (as though no errorCodes were included).
        if (typeString.trim().match(/^ignore(\s|\[|$)/)) {
            return undefined;
        }

        const tokenOffset = curToken.start + curToken.length + match[1].length;
        return StringToken.create(
            tokenOffset,
            typeString.length,
            StringTokenFlags.None,
            typeString,
            0,
            /* comments */ undefined
        );
    }

    private _parseVariableTypeAnnotationComment(): ExpressionNode | undefined {
        const stringToken = this._getTypeAnnotationCommentText();
        if (!stringToken) {
            return undefined;
        }

        const stringNode = this._makeStringNode(stringToken);
        const stringListNode = StringListNode.create([stringNode]);
        const parser = new Parser();
        const parseResults = parser.parseTextExpression(
            this._fileContents!,
            stringToken.start,
            stringToken.length,
            this._parseOptions,
            ParseTextMode.VariableAnnotation,
            /* initialParenDepth */ undefined,
            this._typingSymbolAliases
        );

        parseResults.diagnostics.forEach((diag) => {
            this._addError(diag.message, stringListNode);
        });

        if (!parseResults.parseTree) {
            return undefined;
        }

        assert(parseResults.parseTree.nodeType !== ParseNodeType.FunctionAnnotation);
        return parseResults.parseTree;
    }

    private _parseFunctionTypeAnnotationComment(stringToken: StringToken, functionNode: FunctionNode): void {
        const stringNode = this._makeStringNode(stringToken);
        const stringListNode = StringListNode.create([stringNode]);
        const parser = new Parser();
        const parseResults = parser.parseTextExpression(
            this._fileContents!,
            stringToken.start,
            stringToken.length,
            this._parseOptions,
            ParseTextMode.FunctionAnnotation,
            /* initialParenDepth */ undefined,
            this._typingSymbolAliases
        );

        parseResults.diagnostics.forEach((diag) => {
            this._addError(diag.message, stringListNode);
        });

        if (!parseResults.parseTree || parseResults.parseTree.nodeType !== ParseNodeType.FunctionAnnotation) {
            return;
        }

        const functionAnnotation = parseResults.parseTree;

        functionNode.functionAnnotationComment = functionAnnotation;
        functionAnnotation.parent = functionNode;
        extendRange(functionNode, functionAnnotation);
    }

    private _parseFormatStringSegment(
        stringToken: StringToken,
        segment: StringTokenUtils.FormatStringSegment,
        segmentOffset: number,
        segmentLength: number
    ) {
        assert(segment.isExpression);
        const parser = new Parser();
        const parseResults = parser.parseTextExpression(
            this._fileContents!,
            stringToken.start + stringToken.prefixLength + stringToken.quoteMarkLength + segment.offset + segmentOffset,
            segmentLength,
            this._parseOptions,
            ParseTextMode.Expression,
            /* initialParenDepth */ 1,
            this._typingSymbolAliases
        );

        parseResults.diagnostics.forEach((diag) => {
            const textRangeStart =
                (diag.range ? convertPositionToOffset(diag.range.start, parseResults.lines) : stringToken.start) ||
                stringToken.start;
            const textRangeEnd =
                (diag.range
                    ? (convertPositionToOffset(diag.range.end, parseResults.lines) || 0) + 1
                    : stringToken.start + stringToken.length) || stringToken.start + stringToken.length;
            const textRange = { start: textRangeStart, length: textRangeEnd - textRangeStart };
            this._addError(diag.message, textRange);
        });

        return parseResults.parseTree;
    }

    private _parseFormatString(stringToken: StringToken): FormatStringNode {
        const unescapedResult = StringTokenUtils.getUnescapedString(stringToken);
        this._reportStringTokenErrors(stringToken, unescapedResult);

        const formatExpressions: ExpressionNode[] = [];

        for (const segment of unescapedResult.formatStringSegments) {
            if (segment.isExpression) {
                // Determine if we need to truncate the expression because it
                // contains formatting directives that start with a ! or :.
                const segmentExprLength = this._getFormatStringExpressionLength(segment.value.trimEnd());
                const parseTree = this._parseFormatStringSegment(stringToken, segment, 0, segmentExprLength);
                if (parseTree) {
                    assert(parseTree.nodeType !== ParseNodeType.FunctionAnnotation);
                    formatExpressions.push(parseTree);
                }

                // Look for additional expressions within the format directive.
                const formatDirective = segment.value.substr(segmentExprLength);
                let braceDepth = 0;
                let startOfExprOffset = 0;
                for (let i = 0; i < formatDirective.length; i++) {
                    if (formatDirective.charCodeAt(i) === Char.OpenBrace) {
                        if (braceDepth === 0) {
                            startOfExprOffset = i + 1;
                        }
                        braceDepth++;
                    } else if (formatDirective.charCodeAt(i) === Char.CloseBrace) {
                        if (braceDepth > 0) {
                            braceDepth--;
                            if (braceDepth === 0) {
                                const formatSegmentLength = this._getFormatStringExpressionLength(
                                    segment.value.substr(segmentExprLength + startOfExprOffset, i - startOfExprOffset)
                                );
                                const parseTree = this._parseFormatStringSegment(
                                    stringToken,
                                    segment,
                                    segmentExprLength + startOfExprOffset,
                                    formatSegmentLength
                                );
                                if (parseTree) {
                                    assert(parseTree.nodeType !== ParseNodeType.FunctionAnnotation);
                                    formatExpressions.push(parseTree);
                                }
                            }
                        }
                    }
                }
            }
        }

        return FormatStringNode.create(
            stringToken,
            unescapedResult.value,
            unescapedResult.unescapeErrors.length > 0,
            formatExpressions
        );
    }

    private _getFormatStringExpressionLength(segmentValue: string): number {
        let segmentExprLength = 0;

        // PEP 498 says: Expressions cannot contain ':' or '!' outside of
        // strings or parentheses, brackets, or braces. The exception is
        // that the '!=' operator is allowed as a special case.
        const quoteStack: string[] = [];
        let braceCount = 0;
        let parenCount = 0;
        let bracketCount = 0;
        let indexOfDebugEqual: number | undefined;

        while (segmentExprLength < segmentValue.length) {
            const curChar = segmentValue[segmentExprLength];
            const ignoreSeparator = quoteStack.length > 0 || braceCount > 0 || parenCount > 0 || bracketCount > 0;
            const inString = quoteStack.length > 0;

            if (curChar === '=') {
                indexOfDebugEqual = segmentExprLength;
            } else {
                if (curChar === ':') {
                    if (!ignoreSeparator) {
                        break;
                    }
                } else if (curChar === '!') {
                    if (!ignoreSeparator) {
                        // Allow !=, as per PEP 498
                        if (
                            segmentExprLength === segmentValue.length - 1 ||
                            segmentValue[segmentExprLength + 1] !== '='
                        ) {
                            break;
                        }
                    }
                } else if (curChar === "'" || curChar === '"') {
                    let quoteSequence = curChar;
                    if (
                        segmentExprLength + 2 < segmentValue.length &&
                        segmentValue[segmentExprLength + 1] === curChar &&
                        segmentValue[segmentExprLength + 2] === curChar
                    ) {
                        quoteSequence = curChar + curChar + curChar;
                        segmentExprLength += 2;
                    }

                    if (quoteStack.length > 0 && quoteStack[quoteStack.length - 1] === quoteSequence) {
                        quoteStack.pop();
                    } else if (quoteStack.length === 0) {
                        quoteStack.push(quoteSequence);
                    }
                } else if (curChar === '(') {
                    if (!inString) {
                        parenCount++;
                    }
                } else if (curChar === ')') {
                    if (!inString && parenCount > 0) {
                        parenCount--;
                    }
                } else if (curChar === '{') {
                    if (!inString) {
                        braceCount++;
                    }
                } else if (curChar === '}') {
                    if (!inString && braceCount > 0) {
                        braceCount--;
                    }
                } else if (curChar === '[') {
                    if (!inString) {
                        bracketCount++;
                    }
                } else if (curChar === ']') {
                    if (!inString && bracketCount > 0) {
                        bracketCount--;
                    }
                }

                if (curChar !== ' ') {
                    indexOfDebugEqual = undefined;
                }
            }

            segmentExprLength++;
        }

        // Handle Python 3.8 f-string formatting expressions that
        // end in an "=".
        if (this._parseOptions.pythonVersion >= PythonVersion.V3_8 && indexOfDebugEqual !== undefined) {
            segmentExprLength = indexOfDebugEqual;
        }

        return segmentExprLength;
    }

    private _createBinaryOperationNode(
        leftExpression: ExpressionNode,
        rightExpression: ExpressionNode,
        operatorToken: Token,
        operator: OperatorType
    ) {
        // Determine if we're exceeding the max parse depth. If so, replace
        // the subnode with an error node. Otherwise we risk crashing in the binder
        // or type evaluator.
        if (leftExpression.maxChildDepth !== undefined && leftExpression.maxChildDepth >= maxChildNodeDepth) {
            leftExpression = ErrorNode.create(leftExpression, ErrorExpressionCategory.MaxDepthExceeded);
            this._addError(Localizer.Diagnostic.maxParseDepthExceeded(), leftExpression);
        }

        if (rightExpression.maxChildDepth !== undefined && rightExpression.maxChildDepth >= maxChildNodeDepth) {
            rightExpression = ErrorNode.create(rightExpression, ErrorExpressionCategory.MaxDepthExceeded);
            this._addError(Localizer.Diagnostic.maxParseDepthExceeded(), rightExpression);
        }

        return BinaryOperationNode.create(leftExpression, rightExpression, operatorToken, operator);
    }

    private _createUnaryOperationNode(operatorToken: Token, expression: ExpressionNode, operator: OperatorType) {
        // Determine if we're exceeding the max parse depth. If so, replace
        // the subnode with an error node. Otherwise we risk crashing in the binder
        // or type evaluator.
        if (expression.maxChildDepth !== undefined && expression.maxChildDepth >= maxChildNodeDepth) {
            expression = ErrorNode.create(expression, ErrorExpressionCategory.MaxDepthExceeded);
            this._addError(Localizer.Diagnostic.maxParseDepthExceeded(), expression);
        }

        return UnaryOperationNode.create(operatorToken, expression, operator);
    }

    private _parseStringList(): StringListNode {
        const stringList: (StringNode | FormatStringNode)[] = [];

        while (this._peekTokenType() === TokenType.String) {
            const stringToken = this._getNextToken() as StringToken;
            if (stringToken.flags & StringTokenFlags.Format) {
                stringList.push(this._parseFormatString(stringToken));
            } else {
                stringList.push(this._makeStringNode(stringToken));
            }
        }

        const stringNode = StringListNode.create(stringList);

        // If we're parsing a type annotation, parse the contents of the string.
        if (this._isParsingTypeAnnotation) {
            // Don't allow multiple strings because we have no way of reporting
            // parse errors that span strings.
            if (stringNode.strings.length > 1) {
                this._addError(Localizer.Diagnostic.annotationSpansStrings(), stringNode);
            } else if (stringNode.strings[0].token.flags & StringTokenFlags.Format) {
                this._addError(Localizer.Diagnostic.annotationFormatString(), stringNode);
            } else {
                const stringToken = stringNode.strings[0].token;
                const stringValue = StringTokenUtils.getUnescapedString(stringNode.strings[0].token);
                const unescapedString = stringValue.value;
                const tokenOffset = stringToken.start;
                const prefixLength = stringToken.prefixLength + stringToken.quoteMarkLength;

                // Don't allow escape characters because we have no way of mapping
                // error ranges back to the escaped text.
                if (unescapedString.length !== stringToken.length - prefixLength - stringToken.quoteMarkLength) {
                    this._addError(Localizer.Diagnostic.annotationStringEscape(), stringNode);
                } else {
                    const parser = new Parser();
                    const parseResults = parser.parseTextExpression(
                        this._fileContents!,
                        tokenOffset + prefixLength,
                        unescapedString.length,
                        this._parseOptions,
                        ParseTextMode.VariableAnnotation,
                        (stringNode.strings[0].token.flags & StringTokenFlags.Triplicate) !== 0 ? 1 : 0,
                        this._typingSymbolAliases
                    );

                    if (
                        parseResults.diagnostics.length === 0 ||
                        this._parseOptions.reportErrorsForParsedStringContents
                    ) {
                        parseResults.diagnostics.forEach((diag) => {
                            this._addError(diag.message, stringNode);
                        });

                        if (parseResults.parseTree) {
                            assert(parseResults.parseTree.nodeType !== ParseNodeType.FunctionAnnotation);
                            stringNode.typeAnnotation = parseResults.parseTree;
                            stringNode.typeAnnotation.parent = stringNode;
                        }
                    }
                }
            }
        }

        return stringNode;
    }

    // Python 3.8 added support for star (unpack) expressions in tuples
    // following a return or yield statement in cases where the tuple
    // wasn't surrounded in parentheses.
    private _reportConditionalErrorForStarTupleElement(possibleTupleExpr: ExpressionNode) {
        if (possibleTupleExpr.nodeType !== ParseNodeType.Tuple) {
            return;
        }

        if (possibleTupleExpr.enclosedInParens) {
            return;
        }

        if (this._parseOptions.pythonVersion >= PythonVersion.V3_8) {
            return;
        }

        for (const expr of possibleTupleExpr.expressions) {
            if (expr.nodeType === ParseNodeType.Unpack) {
                this._addError(Localizer.Diagnostic.unpackTuplesIllegal(), expr);
                return;
            }
        }
    }

    // Peeks at the next token and returns true if it can never
    // represent the start of an expression.
    private _isNextTokenNeverExpression(): boolean {
        const nextToken = this._peekToken();
        switch (nextToken.type) {
            case TokenType.Keyword: {
                switch (this._peekKeywordType()) {
                    case KeywordType.For:
                    case KeywordType.In:
                    case KeywordType.If:
                        return true;
                }
                break;
            }

            case TokenType.Operator: {
                switch (this._peekOperatorType()) {
                    case OperatorType.AddEqual:
                    case OperatorType.SubtractEqual:
                    case OperatorType.MultiplyEqual:
                    case OperatorType.DivideEqual:
                    case OperatorType.ModEqual:
                    case OperatorType.BitwiseAndEqual:
                    case OperatorType.BitwiseOrEqual:
                    case OperatorType.BitwiseXorEqual:
                    case OperatorType.LeftShiftEqual:
                    case OperatorType.RightShiftEqual:
                    case OperatorType.PowerEqual:
                    case OperatorType.FloorDivideEqual:
                    case OperatorType.Assign:
                        return true;
                }
                break;
            }

            case TokenType.Indent:
            case TokenType.Dedent:
            case TokenType.NewLine:
            case TokenType.EndOfStream:
            case TokenType.Semicolon:
            case TokenType.CloseParenthesis:
            case TokenType.CloseBracket:
            case TokenType.CloseCurlyBrace:
            case TokenType.Comma:
            case TokenType.Colon:
                return true;
        }

        return false;
    }

    private _disallowAssignmentExpression(callback: () => void) {
        const wasAllowed = this._assignmentExpressionsAllowed;
        this._assignmentExpressionsAllowed = false;

        callback();

        this._assignmentExpressionsAllowed = wasAllowed;
    }

    private _getNextToken(): Token {
        const token = this._tokenizerOutput!.tokens.getItemAt(this._tokenIndex);
        if (!this._atEof()) {
            this._tokenIndex++;
        }

        return token;
    }

    private _atEof(): boolean {
        // Are we pointing at the last token in the stream (which is
        // assumed to be an end-of-stream token)?
        return this._tokenIndex >= this._tokenizerOutput!.tokens.count - 1;
    }

    private _peekToken(count = 0): Token {
        if (this._tokenIndex + count < 0) {
            return this._tokenizerOutput!.tokens.getItemAt(0);
        }

        if (this._tokenIndex + count >= this._tokenizerOutput!.tokens.count) {
            return this._tokenizerOutput!.tokens.getItemAt(this._tokenizerOutput!.tokens.count - 1);
        }

        return this._tokenizerOutput!.tokens.getItemAt(this._tokenIndex + count);
    }

    private _peekTokenType(): TokenType {
        return this._peekToken().type;
    }

    private _peekKeywordType(): KeywordType | undefined {
        const nextToken = this._peekToken();
        if (nextToken.type !== TokenType.Keyword) {
            return undefined;
        }

        return (nextToken as KeywordToken).keywordType;
    }

    private _peekOperatorType(): OperatorType | undefined {
        const nextToken = this._peekToken();
        if (nextToken.type !== TokenType.Operator) {
            return undefined;
        }

        return (nextToken as OperatorToken).operatorType;
    }

    private _getTokenIfIdentifier(): IdentifierToken | undefined {
        const nextToken = this._peekToken();
        if (nextToken.type === TokenType.Identifier) {
            return this._getNextToken() as IdentifierToken;
        }

        // If the next token is invalid, treat it as an identifier.
        if (nextToken.type === TokenType.Invalid) {
            this._getNextToken();
            this._addError(Localizer.Diagnostic.invalidIdentifierChar(), nextToken);
            return IdentifierToken.create(nextToken.start, nextToken.length, '', nextToken.comments);
        }

        // If this is a "soft keyword", it can be converted into an identifier.
        if (nextToken.type === TokenType.Keyword) {
            const keywordType = this._peekKeywordType();
            if (softKeywords.find((type) => type === keywordType)) {
                const keywordText = this._fileContents!.substr(nextToken.start, nextToken.length);
                this._getNextToken();
                return IdentifierToken.create(nextToken.start, nextToken.length, keywordText, nextToken.comments);
            }
        }

        return undefined;
    }

    // Consumes tokens until the next one in the stream is
    // either a specified terminator or the end-of-stream
    // token.
    private _consumeTokensUntilType(terminators: TokenType[]): boolean {
        while (true) {
            const token = this._peekToken();
            if (terminators.some((term) => term === token.type)) {
                return true;
            }

            if (token.type === TokenType.EndOfStream) {
                return false;
            }

            this._getNextToken();
        }
    }

    private _getTokenIfType(tokenType: TokenType): Token | undefined {
        if (this._peekTokenType() === tokenType) {
            return this._getNextToken();
        }

        return undefined;
    }

    private _consumeTokenIfType(tokenType: TokenType): boolean {
        return !!this._getTokenIfType(tokenType);
    }

    private _consumeTokenIfKeyword(keywordType: KeywordType): boolean {
        if (this._peekKeywordType() === keywordType) {
            this._getNextToken();
            return true;
        }

        return false;
    }

    private _consumeTokenIfOperator(operatorType: OperatorType): boolean {
        if (this._peekOperatorType() === operatorType) {
            this._getNextToken();
            return true;
        }

        return false;
    }

    private _getKeywordToken(keywordType: KeywordType): KeywordToken {
        const keywordToken = this._getNextToken() as KeywordToken;
        assert(keywordToken.type === TokenType.Keyword);
        assert(keywordToken.keywordType === keywordType);
        return keywordToken;
    }

    private _getLanguageVersion() {
        return this._parseOptions.pythonVersion;
    }

    private _suppressErrors(callback: () => void) {
        const errorsWereSuppressed = this._areErrorsSuppressed;
        try {
            this._areErrorsSuppressed = true;
            callback();
        } finally {
            this._areErrorsSuppressed = errorsWereSuppressed;
        }
    }

    private _addError(message: string, range: TextRange) {
        assert(range !== undefined);

        if (!this._areErrorsSuppressed) {
            this._diagSink.addError(
                message,
                convertOffsetsToRange(range.start, range.start + range.length, this._tokenizerOutput!.lines)
            );
        }
    }

    // ! Cython

    private _addWarning(message: string, range: TextRange) {
        assert(range !== undefined);

        if (!this._areErrorsSuppressed) {
            this._diagSink.addWarning(
                message,
                convertOffsetsToRange(range.start, range.start + range.length, this._tokenizerOutput!.lines)
            );
        }
    }

    private _addDeprecated(message: string, range: TextRange) {
        assert(range !== undefined);

        if (!this._areErrorsSuppressed) {
            this._diagSink.addDeprecated(
                message,
                convertOffsetsToRange(range.start, range.start + range.length, this._tokenizerOutput!.lines)
            );
        }
    }

    private _pushStatements(statements: StatementListNode, node: ParseNode) {
        statements.statements.push(node);
        node.parent = statements;
        extendRange(statements, node);
    }

    private _keywordToIdentifier(token: KeywordToken) {
        const keywordText = this._fileContents!.substr(token.start, token.length);
        return IdentifierToken.create(token.start, token.length, keywordText, token.comments);
    }

    private _softKeywordToIdentifier(token: Token) {
        if (token.type === TokenType.Keyword) {
            const keywordToken = token as KeywordToken;
            const keywordType = keywordToken.keywordType;
            if (softKeywords.find((type) => type === keywordType)) {
                return this._keywordToIdentifier(keywordToken);
            }
        }
        return undefined;
    }

    // return true if next token is identifier or soft keyword
    private _peekIdentifier() {
        const token = this._peekToken();
        if (token.type === TokenType.Identifier) {
            return true;
        }
        if (token.type === TokenType.Keyword) {
            const kwToken = token as KeywordToken;
            if (softKeywords.includes(kwToken.keywordType)) {
                return true;
            }
        }
        return false;
    }

    private _rangeText(range: TextRange) {
        return this._fileContents!.substr(range.start, range.length);
    }

    // Check for new line. Consumes semicolon if present
    private _expectNewLine() {
        if (this._tokenIndex > 0 && this._peekToken(-1).type === TokenType.NewLine) {
            // If the last token was a new line then we can assume that there was a new line
            return;
        }
        const nextToken = this._peekTokenType();
        const valid = [TokenType.NewLine, TokenType.EndOfStream];
        let error = false;
        if (!valid.includes(nextToken)) {
            if (this._consumeTokenIfType(TokenType.Semicolon)) {
                if (!valid.includes(this._peekTokenType())) {
                    error = true;
                }
            } else {
                error = true;
            }
        }
        if (error) {
            this._addError(Localizer.Diagnostic.expectedNewline(), this._peekToken());
            this._consumeTokensUntilType(valid);
        }
    }

    private _isPointerToken(token: Token) {
        const types = [OperatorType.Multiply, OperatorType.Power];
        return token.type === TokenType.Operator && types.includes((token as OperatorToken).operatorType);
    }

    // ctypedef type mytype
    private _parseCTypeDef() {
        const typeDefToken = this._getKeywordToken(KeywordType.Ctypedef);
        const keyword = this._peekKeywordType();
        switch (keyword) {
            case KeywordType.Packed:
            case KeywordType.Struct:
            case KeywordType.Union:
            case KeywordType.Fused:
                return this._parseCStruct();
            case KeywordType.Enum:
                return this._parseEnum(/*cpdef*/ false);
            case KeywordType.Class:
                return this._parseCClassExt();
        }
        let node = this._parseCType();
        if (node.nodeType === ParseNodeType.CType) {
            const nameOrError = this._parseCVarName(node, /*allowReference*/ false);
            if (nameOrError.nodeType === ParseNodeType.Name) {
                this._expectNewLine();
                return CTypeDefNode.create(typeDefToken, node, nameOrError);
            } else if (nameOrError.nodeType === ParseNodeType.CCallback) {
                this._expectNewLine();
                return CTypeDefNode.create(typeDefToken, nameOrError, { ...nameOrError.name });
            }
            this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
            node = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingExpression, node);
        }

        const statements = StatementListNode.create(typeDefToken);
        statements.statements.push(node);
        extendRange(statements, node);
        node.parent = statements;
        return statements;
    }

    // parse fields. Similar to arglist. Can be single line or multiline with indent.
    private _parseEnumFields(indented: boolean) {
        const argList: AssignmentNode[] = [];
        let trailingComma = false;
        const stopTypes = [TokenType.EndOfStream];
        if (indented) {
            stopTypes.push(TokenType.Dedent);
        } else {
            stopTypes.push(TokenType.NewLine);
        }
        let index = 0;
        while (!stopTypes.includes(this._peekTokenType())) {
            trailingComma = false;
            const arg = this._parseArgument();

            // We'll store each argument as an assignment
            let assign: AssignmentNode;
            if (arg.valueExpression.nodeType === ParseNodeType.Name) {
                // Assign enum value
                // Values are incremented if not specified
                assign = AssignmentNode.create(
                    arg.valueExpression,
                    NumberNode.create(NumberToken.create(0, 0, index, true, false, undefined))
                );
                index++;
            } else if (arg.name && arg.valueExpression) {
                assign = AssignmentNode.create(arg.name, arg.valueExpression);
            } else {
                // Shouldn't get here
                this._addError(Localizer.Diagnostic.expectedAssignRightHandExpr(), this._peekToken());
                continue;
            }

            if (arg.argumentCategory !== ArgumentCategory.Simple) {
                this._addError(Localizer.Diagnostic.unpackNotAllowed(), arg);
            } else {
                argList.push(assign);
            }

            // Enum fields can be separated by commas or new lines
            trailingComma = this._consumeTokenIfType(TokenType.Comma);

            if (indented) {
                this._consumeTokenIfType(TokenType.NewLine);
            } else {
                if (!trailingComma && this._peekTokenType() !== TokenType.NewLine) {
                    this._addError(Localizer.Diagnostic.expectedNewlineOrSemicolon(), this._peekToken());
                    this._consumeTokensUntilType([TokenType.NewLine]);
                    break;
                }
            }
        }

        return { args: argList, trailingComma };
    }

    // parse enum
    private _parseEnum(cpdef: boolean) {
        const fields: ParseNode[] = [];
        let classType: ArgumentNode | undefined = undefined;
        const enumToken = this._peekToken();
        this._consumeTokenIfKeyword(KeywordType.Enum);

        // TODO: `class` cpdef not allowed when language is not C++
        const classToken = this._peekToken();
        let isClass = this._consumeTokenIfKeyword(KeywordType.Class);
        if (!cpdef && isClass) {
            this._addError(Localizer.Diagnostic.expectedIdentifier(), classToken);
            isClass = false;
        }

        // enum can be anonymous
        const nameIden = this._getTokenIfIdentifier();
        const name = nameIden ? NameNode.create(nameIden) : undefined;

        if (isClass && this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            // Type expected
            if (this._peekTokenType() === TokenType.CloseParenthesis) {
                this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
            } else {
                const argToken = this._peekToken();
                classType = ArgumentNode.create(argToken, this._parseCType(), ArgumentCategory.Simple);
            }
            if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                return this._handleExpressionParseError(
                    ErrorExpressionCategory.MissingCallCloseParen,
                    Localizer.Diagnostic.expectedCloseParen()
                );
            }
        }

        if (!this._consumeTokenIfType(TokenType.Colon)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.Diagnostic.expectedColon()
            );
        }
        // Fields can be on same line. This runs counter to most python syntax that requires indented block after colon.
        let indented = false;
        if (this._consumeTokenIfType(TokenType.NewLine)) {
            const indentToken = this._peekToken() as IndentToken;
            if (this._consumeTokenIfType(TokenType.Indent)) {
                if (indentToken.isIndentAmbiguous) {
                    this._addError(Localizer.Diagnostic.inconsistentTabs(), indentToken);
                }
            } else {
                return this._handleExpressionParseError(
                    ErrorExpressionCategory.InvalidDeclaration,
                    Localizer.Diagnostic.expectedIndentedBlock()
                );
            }
            indented = true;
        }

        const suiteStart = this._peekToken();
        fields.push(...this._parseEnumFields(indented).args);
        if (indented) {
            while (this._peekTokenType() === TokenType.NewLine) {
                this._consumeTokenIfType(TokenType.NewLine);
            }
            const dedentToken = this._peekToken() as DedentToken;
            if (this._consumeTokenIfType(TokenType.Dedent)) {
                if (!dedentToken.matchesIndent) {
                    this._addError(Localizer.Diagnostic.inconsistentIndent(), dedentToken);
                }
                if (dedentToken.isDedentAmbiguous) {
                    this._addError(Localizer.Diagnostic.inconsistentTabs(), dedentToken);
                }
            }
        }
        if (fields.length === 0) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.Diagnostic.expectedIdentifier()
            );
        }
        const suite = SuiteNode.create(suiteStart);
        const statements = StatementListNode.create(suiteStart);
        fields.forEach((f) => {
            this._pushStatements(statements, f);
        });
        suite.statements.push(statements);
        statements.parent = suite;
        extendRange(suite, statements);
        const node = CEnumNode.create(enumToken, name, suite, indented, cpdef, undefined);
        if (classType) {
            node.arguments.push(classType);
            classType.parent = node;
        }
        if (isClass) {
            node.classToken = classToken;
        }
        return node;
    }

    // Parse cython structure
    private _parseCStruct() {
        let packedToken: KeywordToken | undefined = undefined;
        if (this._peekKeywordType() === KeywordType.Packed) {
            packedToken = this._getKeywordToken(KeywordType.Packed);
        }

        let structType: CStructType | undefined = undefined;
        const startToken = this._peekToken();
        const kwType = this._peekKeywordType();
        switch (kwType) {
            case KeywordType.Struct:
                structType = CStructType.Struct;
                break;
            case KeywordType.Union:
                structType = CStructType.Union;
                break;
            case KeywordType.Fused:
                structType = CStructType.Fused;
                break;
            // TODO: Check
            // case KeywordType.Class:
            //     structType = CStructType.Class;
            //     break;
            default:
                break;
        }
        if (packedToken && structType !== CStructType.Struct) {
            return this._handleExpressionParseError(
                // TODO: Missing struct
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.DiagnosticCython.expectedStruct()
            );
        }
        if (structType === undefined) {
            return this._handleExpressionParseError(
                // TODO: Missing struct type
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.DiagnosticCython.expectedStructType()
            );
        }
        this._getNextToken();

        const iden = this._getTokenIfIdentifier();
        const name = iden ? NameNode.create(iden) : undefined;
        if (!name) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.Diagnostic.expectedIdentifier()
            );
        }

        const wasParsingCStruct = this._isParsingCStruct;
        this._isParsingCStruct = true;
        const wasParsingCFused = this._isParsingCFused;
        if (structType === CStructType.Fused) {
            this._isParsingCFused = true;
        }
        const suite = this._parseSuite(false, false, undefined, /*isCdefSuite*/ true);
        this._isParsingCStruct = wasParsingCStruct;
        this._isParsingCFused = wasParsingCFused;
        const node = CStructNode.create(startToken, name, suite, structType, packedToken, undefined);
        // TODO: Check if no fields with pass statement
        return node;
    }

    private _parseCpdef() {
        const cdefToken = this._getKeywordToken(KeywordType.Cpdef);
        const token = this._peekToken();
        const kwToken = token as KeywordToken;
        let node: ParseNode | undefined = undefined;
        switch (token.type) {
            case TokenType.Keyword:
                if (kwToken.keywordType === KeywordType.Enum) {
                    node = this._parseEnum(true);
                }
                break;
            default:
                break;
        }

        if (!node) {
            const typeNode = this._parseCType();
            if (typeNode.nodeType !== ParseNodeType.Error) {
                const possibleFunction = this._getCFunction(typeNode, true);
                if (possibleFunction) {
                    if (CFunctionNode.isInstance(possibleFunction)) {
                        possibleFunction.cpdef = true;
                    }
                    return possibleFunction;
                }
            }
        }

        if (node && node.nodeType !== ParseNodeType.Error) {
            return node;
        }
        if (!node) {
            node = ErrorNode.create(cdefToken, ErrorExpressionCategory.MissingExpression);
            // TODO: Is this the right error?
            this._addError(Localizer.Diagnostic.expectedIdentifier(), token);
        }
        const statements = StatementListNode.create(cdefToken);
        this._pushStatements(statements, node);
        return statements;
    }

    // parse cdef statement; line starting with `cdef`
    private _parseCDef() {
        const cdefToken = this._getKeywordToken(KeywordType.Cdef);

        const token = this._peekToken();
        const nextToken = this._peekToken(1);
        const kwToken = token as KeywordToken;
        let node: ParseNode | undefined = undefined;
        switch (token.type) {
            case TokenType.OpenParenthesis:
            case TokenType.Identifier: {
                if (
                    nextToken.type === TokenType.Operator &&
                    (nextToken as OperatorToken).operatorType === OperatorType.Assign
                ) {
                    // This could be a simple untyped assignment declaration
                    const statement = StatementListNode.create(token);
                    const expr = this._parseSmallStatement();
                    expr.isCython = true;
                    this._pushStatements(statement, expr);
                    this._expectNewLine();
                    this._consumeTokenIfType(TokenType.NewLine);
                    return statement;
                }
                node = this._parseCVarDecl();
                if (CFunctionNode.isInstance(node)) {
                    return node;
                }
                break;
            }
            case TokenType.Keyword: {
                if (varModifiers.includes(kwToken.keywordType) || numericModifiers.includes(kwToken.keywordType)) {
                    // Var modifier (var declaration)
                    node = this._parseCVarDecl();
                    if (CFunctionNode.isInstance(node)) {
                        return node;
                    }
                } else {
                    switch (kwToken.keywordType) {
                        case KeywordType.Nogil: {
                            node = this._parseCDefSuite(cdefToken, /*nogil*/ true);
                            const statements = StatementListNode.create(cdefToken);
                            this._pushStatements(statements, node);
                            return statements;
                        }
                        case KeywordType.Extern: {
                            if ((nextToken as KeywordToken).keywordType === KeywordType.From) {
                                return this._parseExtern(cdefToken);
                            }
                            this._consumeTokenIfKeyword(KeywordType.Extern);
                            // One line extern
                            return this._parseCVarDecl();
                        }
                        case KeywordType.Enum:
                            return this._parseEnum(false);
                        case KeywordType.Packed:
                        case KeywordType.Struct:
                        case KeywordType.Union:
                        case KeywordType.Fused:
                            return this._parseCStruct();
                        case KeywordType.Class: {
                            const wasParsingCClass = this._isParsingCClass;
                            this._isParsingCClass = true;
                            const cls = this._parseClassDef(undefined, true);
                            this._isParsingCClass = wasParsingCClass;
                            return cls;
                        }
                        case KeywordType.Cppclass:
                            return this._parseCppClassDef();
                    }
                }
                break;
            }
            case TokenType.Colon:
                node = this._parseCDefSuite(cdefToken, /*nogil*/ true);
                break;
            default:
                break;
        }
        if (!node) {
            node = ErrorNode.create(cdefToken, ErrorExpressionCategory.MissingExpression);
            // TODO: Is this the right error?
            this._addError(Localizer.Diagnostic.expectedIdentifier(), token);
        }
        const statements = StatementListNode.create(cdefToken);
        this._pushStatements(statements, node);
        if (node.nodeType !== ParseNodeType.Suite) {
            this._expectNewLine();
        }
        this._consumeTokenIfType(TokenType.NewLine);
        return statements;
    }

    private _parseSharedDecl(typeNode: CTypeNode) {
        let leftExpr: ExpressionNode = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.InvalidDeclaration);
        const nameOrError = this._parseCVarName(typeNode, /*allowReference*/ false);
        const result: CDeclResult = { node: leftExpr, pointers: false };

        if (nameOrError.nodeType === ParseNodeType.CCallback) {
            result.node = nameOrError;
            return result;
        } else if (nameOrError.nodeType !== ParseNodeType.Error) {
            leftExpr = TypeAnnotationNode.create(nameOrError, typeNode);
            nameOrError.typeNode = typeNode;
            result.pointers = typeNode.operators.length > 0;
        } else {
            leftExpr = nameOrError;
        }
        if (leftExpr.nodeType !== ParseNodeType.TypeAnnotation) {
            result.node = leftExpr;
            return result;
        }

        if (this._consumeTokenIfOperator(OperatorType.Assign)) {
            const exprListResult = this._parseExpressionListGeneric(
                /*parser*/ () => this._parseTestOrStarExpression(/*allowAssignmentExpression*/ false),
                /*terminalCheck*/ () => this._isNextTokenNeverExpression(),
                /*finalEntryCheck*/ () => true // ! Parse one item. Needed to ignore a comma/prevent list parse.
            );
            if (exprListResult.parseError) {
                result.node = exprListResult.parseError;
                return result;
            }
            const rightExpr = this._makeExpressionOrTuple(exprListResult, /* enclosedInParens */ false);

            if (rightExpr.nodeType === ParseNodeType.Error) {
                result.node = AssignmentNode.create(leftExpr, rightExpr);
                return result;
            }

            const assignmentNode = AssignmentNode.create(leftExpr, rightExpr);

            // Look for a type annotation comment at the end of the line.
            const typeAnnotationComment = this._parseVariableTypeAnnotationComment();
            if (typeAnnotationComment) {
                assignmentNode.typeAnnotationComment = typeAnnotationComment;
                assignmentNode.typeAnnotationComment.parent = assignmentNode;
                extendRange(assignmentNode, assignmentNode.typeAnnotationComment);
            }
            result.node = assignmentNode;
            return result;
        }
        result.node = leftExpr;
        return result;
    }

    // Return the number of tokens to tokentype
    // If not found before reaching a stop token, return -1
    // stopTokens always includes new line and end of stream
    private _peekToTokenType(tokenType: TokenType, stopTokens?: TokenType[]) {
        stopTokens = stopTokens ?? [];
        stopTokens.push(...[TokenType.NewLine, TokenType.EndOfStream]);
        let index = 0;
        while (!stopTokens.includes(this._peekToken(index).type)) {
            if (this._peekToken(index).type === tokenType) {
                return index;
            }
            index++;
        }
        return -1;
    }

    // return function if function is parsed. Should be called after `parseCType()`
    // Will parse up to the first open parenthesis
    // Any pointers or refs will be parsed and added to type node as well
    // If function is not returned then the index will be reset to value prior call
    private _getCFunction(typeNode: CTypeNode | undefined, cpdef: boolean) {
        const tokenIndex = this._tokenIndex;
        const operators = !cpdef ? this._parsePointersOrRef(/*allowReference*/ true) : [];
        let name: NameNode | undefined = undefined;
        let operatorSuffix: string | undefined = undefined;
        let returnType: CTypeNode | undefined = typeNode;

        if (this._peekIdentifier()) {
            // Check if this is a cpp operator method
            const operatorResult = this._parseOperatorFunctionName();
            name = operatorResult.name;
            if (!name) {
                // Normal function
                if (this._peekIdentifier()) {
                    const iden = this._getTokenIfIdentifier();
                    // Consume alias. Only matters during linking, so we don't need to store it.
                    this._consumeTokenIfType(TokenType.String);
                    let openParenIndex = 0;
                    if (this._peekTokenType() === TokenType.OpenBracket) {
                        // Could be a template argument list
                        const closeBracketIndex = this._peekToTokenType(TokenType.CloseBracket);
                        openParenIndex = closeBracketIndex > 0 ? closeBracketIndex + 1 : openParenIndex;
                    }
                    if (this._peekToken(openParenIndex).type === TokenType.OpenParenthesis) {
                        // Return type annotation present and function name and open paren present
                        if (iden) {
                            name = NameNode.create(iden);
                        }
                    }
                }
            } else {
                operatorSuffix = operatorResult.suffix;
            }
        } else if (operators.length === 0) {
            // Return type not found. If so then we had already parsed the function name
            // Consume alias. Only matters during linking, so we don't need to store it.
            this._consumeTokenIfType(TokenType.String);
            let openParenIndex = 0;
            if (this._peekTokenType() === TokenType.OpenBracket) {
                // Could be a template argument list
                const closeBracketIndex = this._peekToTokenType(TokenType.CloseBracket);
                openParenIndex = closeBracketIndex > 0 ? closeBracketIndex + 1 : openParenIndex;
            }
            if (this._peekToken(openParenIndex).type === TokenType.OpenParenthesis) {
                const narrowed = typeNode ? this._narrowCTypeToName(typeNode) : name;
                if (narrowed?.nodeType === ParseNodeType.Name) {
                    // Only function name present and open paren present
                    name = narrowed;
                    returnType = undefined;
                }
            }
        }
        if (name) {
            if (returnType && operators.length > 0) {
                returnType.operators.push(...operators);
                extendRange(returnType, operators[operators.length - 1]);
            }
            // Check if this is a callback function
            // Don't parse a callback function here
            const maybeOpenParen = this._peekToken();
            const isPointer = this._isPointerToken(this._peekToken(1));
            if (!(maybeOpenParen.type === TokenType.OpenParenthesis && isPointer)) {
                const functionNode = this._parseCFunction(name, returnType);
                if (CFunctionNode.isInstance(functionNode)) {
                    functionNode.operatorSuffix = operatorSuffix;
                }
                return functionNode;
            }
        }
        this._tokenIndex = tokenIndex;
        return undefined;
    }

    private _parseCVarDecl() {
        const startToken = this._peekToken();
        let possibleFunction: CFunctionNode | ErrorNode | undefined = undefined;

        // Only possible in cdef suite
        const kwType = this._peekKeywordType();
        switch (kwType) {
            case KeywordType.Enum:
                return this._parseEnum(false);
            case KeywordType.Packed:
            case KeywordType.Struct:
            case KeywordType.Union:
            case KeywordType.Fused:
                return this._parseCStruct();
            case KeywordType.Cppclass:
                return this._parseCppClassDef();
            case KeywordType.Inline:
                {
                    const modToken = this._getNextToken();
                    possibleFunction = this._getCFunction(undefined, false);
                    if (possibleFunction) {
                        if (CFunctionNode.isInstance(possibleFunction)) {
                            possibleFunction.modifier = kwType;
                            extendRange(possibleFunction, modToken);
                        }
                        return possibleFunction;
                    }
                }
                break;
            case KeywordType.Operator:
                {
                    // Possible cpp operator method without return type
                    possibleFunction = this._getCFunction(undefined, false);
                    if (possibleFunction) {
                        return possibleFunction;
                    }
                }
                break;
        }

        const typeNode = this._parseCType();
        const nodes: ParseNode[] = [];
        let hasPointers = false;

        if (typeNode.nodeType !== ParseNodeType.Error) {
            possibleFunction = this._getCFunction(typeNode, false);
            if (possibleFunction) {
                return possibleFunction;
            }

            let result = this._parseSharedDecl(typeNode);
            if (result.node.nodeType === ParseNodeType.CCallback) {
                const node = TypeAnnotationNode.create(result.node.name, result.node);
                const statements = StatementListNode.create(startToken);
                this._pushStatements(statements, node);
                return statements;
            }
            nodes.push(result.node);
            if (result.pointers) {
                hasPointers = true;
            }
            while (this._consumeTokenIfType(TokenType.Comma)) {
                result = this._parseSharedDecl(CTypeNode.cloneForShared(typeNode));
                nodes.push(result.node);
                if (result.pointers) {
                    hasPointers = true;
                }
            }
        } else {
            nodes.push(typeNode);
        }
        const statements = StatementListNode.create(startToken);
        nodes.forEach((n) => {
            this._pushStatements(statements, n);
        });
        if (nodes.length > 1 && hasPointers) {
            this._addWarning(Localizer.DiagnosticCython.pointersInSharedDecl(), statements);
        }
        this._expectNewLine();
        return statements;
    }

    // View args must be slices. Only step slot is allowed.
    // Each slice represents a dimension
    // The step indicates a memory layout (cython.view)
    // See `https://cython.readthedocs.io/en/latest/src/userguide/memoryviews.html`
    private _validateMemoryViewArgs(args: ArgumentNode[]) {
        let error = false;
        args.forEach((arg) => {
            const expr = arg.valueExpression;
            if (expr.nodeType === ParseNodeType.Slice) {
                if (expr.startValue || expr.endValue) {
                    error = true;
                    this._addError(Localizer.DiagnosticCython.viewSliceInvalid(), arg);
                } else if (expr.stepValue?.nodeType === ParseNodeType.Number) {
                    // Compile error if step is a number other than `1`
                    if (expr.stepValue.value !== 1) {
                        error = true;
                        this._addError(Localizer.DiagnosticCython.viewSliceStepInvalid(), expr.stepValue);
                    }
                }
            } else {
                error = true;
                this._addError(Localizer.DiagnosticCython.viewExpectedSlice(), arg);
            }
        });
        return error;
    }

    // Return name node if ctype is simple
    private _narrowCTypeToName(node: CTypeNode) {
        if (
            node.numModifiers.length === 0 &&
            node.varModifiers.length === 0 &&
            node.operators.length === 0 &&
            !node.typeTrailNode &&
            !node.varTrailNode &&
            node.expression.nodeType === ParseNodeType.Name
        ) {
            return node.expression;
        }
        return node;
    }

    // parse subscript argument for ctype in subscriptlist.
    private _parseCTypePossibleSlice(): ExpressionNode {
        const firstToken = this._peekToken();
        const sliceExpressions: (ExpressionNode | undefined)[] = [undefined, undefined, undefined];
        let sliceIndex = 0;
        let sawColon = false;

        while (true) {
            const nextTokenType = this._peekTokenType();
            if (nextTokenType === TokenType.CloseBracket || nextTokenType === TokenType.Comma) {
                break;
            }

            if (nextTokenType !== TokenType.Colon) {
                const tokenIndex = this._tokenIndex;
                const wasErrorsSuppressed = this._areErrorsSuppressed;
                this._areErrorsSuppressed = true;
                let sliceExpr: ExpressionNode = this._parseCType();
                this._areErrorsSuppressed = wasErrorsSuppressed;

                if (sliceExpr.nodeType === ParseNodeType.CType) {
                    // This could be a buffer, template, array
                    sliceExpr.operators.push(...this._parsePointersOrRef(/*allowReference*/ true));
                    sliceExpr = this._narrowCTypeToName(sliceExpr);
                } else {
                    // Most likely an array
                    // Reset token index before last parse and parse expression normally.
                    this._tokenIndex = tokenIndex;
                    sliceExpr = this._parseTestExpression(/* allowAssignmentExpression */ false);
                }
                sliceExpressions[sliceIndex] = sliceExpr;
            }
            sliceIndex++;

            if (sliceIndex >= 3 || !this._consumeTokenIfType(TokenType.Colon)) {
                break;
            }
            sawColon = true;
        }

        // If this was a simple expression with no colons return it.
        if (!sawColon) {
            if (sliceExpressions[0]) {
                return sliceExpressions[0];
            }

            return ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingIndexOrSlice);
        }

        const sliceNode = SliceNode.create(firstToken);
        sliceNode.startValue = sliceExpressions[0];
        if (sliceNode.startValue) {
            sliceNode.startValue.parent = sliceNode;
        }
        sliceNode.endValue = sliceExpressions[1];
        if (sliceNode.endValue) {
            sliceNode.endValue.parent = sliceNode;
        }
        sliceNode.stepValue = sliceExpressions[2];
        if (sliceNode.stepValue) {
            sliceNode.stepValue.parent = sliceNode;
        }
        const extension = sliceExpressions[2] || sliceExpressions[1] || sliceExpressions[0];
        if (extension) {
            extendRange(sliceNode, extension);
        }

        return sliceNode;
    }

    // parse subscriptList after type name `typename[*]`
    private _parseCTypeSubscriptList(isCParameter = false): SubscriptListResult {
        const argList: ArgumentNode[] = [];
        let trailingComma = false;
        const argType = ArgumentCategory.Simple;

        while (true) {
            const firstToken = this._peekToken();

            if (firstToken.type !== TokenType.Colon && this._isNextTokenNeverExpression()) {
                break;
            }

            // const startOfSubscriptIndex = this._tokenIndex;
            let valueExpr = this._parseCTypePossibleSlice();
            let nameIdentifier: IdentifierToken | undefined;

            // Is this a keyword argument?
            if (argType === ArgumentCategory.Simple) {
                if (this._consumeTokenIfOperator(OperatorType.Assign)) {
                    const nameExpr = valueExpr;
                    valueExpr = this._parseCTypePossibleSlice();

                    if (nameExpr.nodeType === ParseNodeType.Name) {
                        nameIdentifier = nameExpr.token;
                    } else {
                        this._addError(Localizer.Diagnostic.expectedParamName(), nameExpr);
                    }
                }
                // TODO: Check if walrus is allowed
                // else if (
                //     valueExpr.nodeType === ParseNodeType.Name &&
                //     this._peekOperatorType() === OperatorType.Walrus
                // ) {
                //     this._tokenIndex = startOfSubscriptIndex;
                //     valueExpr = this._parseTestExpression(/* allowAssignmentExpression */ true);
                // }
            }

            const argNode = ArgumentNode.create(firstToken, valueExpr, argType);
            if (nameIdentifier) {
                argNode.name = NameNode.create(nameIdentifier);
                argNode.name.parent = argNode;
            }

            argList.push(argNode);

            if (!this._consumeTokenIfType(TokenType.Comma)) {
                trailingComma = false;
                break;
            }

            trailingComma = true;
        }

        // An empty subscript list is illegal except when it is an annotation for function parameter.
        if (argList.length === 0 && !isCParameter) {
            this._handleExpressionParseError(
                ErrorExpressionCategory.MissingIndexOrSlice,
                Localizer.Diagnostic.expectedSliceIndex(),
                /* targetToken */ undefined,
                /* childNode */ undefined,
                [TokenType.CloseBracket]
            );
            //argList.push(ArgumentNode.create(this._peekToken(), errorNode, ArgumentCategory.Simple));
        }

        return {
            list: argList,
            trailingComma,
        };
    }

    // parse type trailer can signify memoryview, buffer, array or template
    private _parseCTypeTrailer(node: CTypeNode, isCParameter = false) {
        const startOfTrailerToken = this._peekToken();
        let closingToken: Token | undefined = undefined;
        let error = false;
        const trailNodes: ArgumentNode[][] = [];
        let openToken = this._peekToken();
        let trailType = CTrailType.None;

        while (this._consumeTokenIfType(TokenType.OpenBracket)) {
            openToken = this._peekToken(-1);
            const subscriptList = this._parseCTypeSubscriptList(isCParameter);
            if (!subscriptList.list.length && isCParameter) {
                // If empty list and is cparameter then treat type as a pointer
                const closingToken = this._peekToken();
                if (!this._consumeTokenIfType(TokenType.CloseBracket)) {
                    this._addError(Localizer.Diagnostic.expectedCloseBracket(), this._peekToken());
                } else {
                    extendRange(node, closingToken);
                }
                node.operators.push(OperatorToken.create(0, 0, OperatorType.Multiply, undefined));
                return;
            }

            trailNodes.push(subscriptList.list);

            if (subscriptList.list.length > 0) {
                const firstExpr = subscriptList.list[0].valueExpression;
                if (trailType === CTrailType.None && firstExpr.nodeType === ParseNodeType.Slice) {
                    if (!trailType) {
                        trailType = CTrailType.View;
                    }
                    if (this._validateMemoryViewArgs(subscriptList.list)) {
                        error = true;
                    }
                } else if (firstExpr.nodeType === ParseNodeType.Number) {
                    if (!trailType) {
                        trailType = CTrailType.Array;
                    }
                    if (!firstExpr.isInteger || subscriptList.list.length > 1) {
                        this._addError(Localizer.DiagnosticCython.expectedIndexOrIdentifier(), firstExpr);
                        error = true;
                    }
                } else if (!trailType) {
                    // Try to narrow trail type
                    trailType = CTrailType.Buffer | CTrailType.Template | CTrailType.Array;
                    if (subscriptList.list.length > 1) {
                        const arg = subscriptList.list[1];
                        if (arg.name) {
                            // Buffer requires named arguments starting at arg 1
                            // TODO: validate buffer args
                            trailType = CTrailType.Buffer;
                        } else {
                            // Multiple unnamed arguments indicates template.
                            trailType = CTrailType.Template;
                        }
                    }
                }

                if (subscriptList.trailingComma) {
                    this._addError(Localizer.DiagnosticCython.trailingCommaNotAllowed(), this._peekToken(-1));
                    error = true;
                }
            } else {
                error = true;
                // error for no args should have been handled already
            }

            if (trailNodes.length > 1) {
                // This should mean that this is a multidim array: `[ARRAY_SIZE][ARRAY_SIZE]`
                if (!(trailType & CTrailType.Array)) {
                    error = true;
                    this._addError(Localizer.DiagnosticCython.invalidNDimDeclaration(), this._peekToken(-1));
                }
                trailType = CTrailType.Array;
            }

            closingToken = this._peekToken();

            if (!this._consumeTokenIfType(TokenType.CloseBracket)) {
                // TODO: Check if this is used correctly
                // Handle the error case, but don't use the error node in this
                // case because it creates problems for the completion provider.
                this._handleExpressionParseError(
                    ErrorExpressionCategory.MissingIndexCloseBracket,
                    Localizer.Diagnostic.expectedCloseBracket(),
                    openToken
                );
                error = true;
            }
        }
        if (trailNodes.length > 0) {
            const trailNode = CTypeTrailNode.create(startOfTrailerToken, trailNodes, trailType, !error, closingToken);
            node.typeTrailNode = trailNode;
            trailNode.parent = node;
            extendRange(node, trailNode);
        }
    }

    // parse var trailer. Signifies c array. `[<integer or identifier>]`
    // can be multiple which signifies multiple dimensions: `[1][1]`
    private _parseCVarTrailer(node: CTypeNode, unsizedArray: boolean) {
        const startOfTrailerToken = this._peekToken();
        let closingToken: Token | undefined = undefined;
        let error = false;
        const trailTypes = [ParseNodeType.Name, ParseNodeType.Number, ParseNodeType.MemberAccess];
        const trailNodes: ArgumentNode[] = [];
        let openBracket = this._peekToken();

        while (this._consumeTokenIfType(TokenType.OpenBracket)) {
            openBracket = this._peekToken(-1);
            if (unsizedArray && this._peekTokenType() === TokenType.CloseBracket) {
                closingToken = this._getNextToken();
                break;
            }
            const subscriptList = this._parseSubscriptList();
            trailNodes.push(...subscriptList.list);
            if (subscriptList.list.length === 1) {
                const trailer = subscriptList.list[0].valueExpression;
                if (trailer.nodeType === ParseNodeType.Number && !trailer.isInteger) {
                    this._addError(Localizer.DiagnosticCython.expectedIndexOrIdentifier(), trailer);
                    error = true;
                } else if (!trailTypes.includes(trailer.nodeType)) {
                    this._addError(Localizer.DiagnosticCython.expectedIndexOrIdentifier(), trailer);
                    error = true;
                }
            } else {
                const rangeToUse = subscriptList.list.length > 1 ? subscriptList.list[0] : openBracket;
                this._addError(Localizer.DiagnosticCython.expectedIndexOrIdentifier(), rangeToUse);
                error = true;
            }
            if (subscriptList.trailingComma) {
                this._addError(Localizer.DiagnosticCython.trailingCommaNotAllowed(), this._peekToken(-1));
                error = true;
            }

            closingToken = this._peekToken();

            if (!this._consumeTokenIfType(TokenType.CloseBracket)) {
                // TODO: Check if this is used correctly
                // Handle the error case, but don't use the error node in this
                // case because it creates problems for the completion provider.
                this._handleExpressionParseError(
                    ErrorExpressionCategory.MissingIndexCloseBracket,
                    Localizer.Diagnostic.expectedCloseBracket(),
                    openBracket,
                    node
                );
                error = true;
            }
        }
        if (!error && openBracket.type === TokenType.OpenBracket) {
            const trailNode = CVarTrailNode.create(startOfTrailerToken, trailNodes, !error, closingToken);
            node.varTrailNode = trailNode;
            trailNode.parent = node;

            // https://cython.readthedocs.io/en/latest/src/userguide/language_basics.html
            // C style array declaration deprecated in favor of Java style
            this._addDeprecated(Localizer.DiagnosticCython.deprecatedCStyleArray(), trailNode);
            if (this._peekOperatorType() === OperatorType.Assign) {
                // Initialization of C Style array not allowed
                this._addError(Localizer.DiagnosticCython.cStyleArrayInitNotAllowed(), this._peekToken());
            }
        }
    }

    // parse from ptr or ref and name only; `**name`
    private _parseCVarName(typeNode: CTypeNode, allowReference = false, skipPointersAndRefs = false) {
        const operators: OperatorToken[] = [];
        const errorNode = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingExpression);
        const validTypes = [TokenType.Identifier, TokenType.Operator, TokenType.Keyword];
        const ptrRefTypes = [OperatorType.Multiply, OperatorType.Power, OperatorType.BitwiseAnd];
        let name: NameNode | undefined = undefined;

        while (validTypes.includes(this._peekTokenType()) && !name) {
            if (!skipPointersAndRefs) {
                const opType = this._peekOperatorType();
                if (opType) {
                    if (ptrRefTypes.includes(opType)) {
                        operators.push(...this._parsePointersOrRef(allowReference));
                        continue;
                    } else {
                        this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
                        return errorNode;
                    }
                }
            }
            const token = this._getNextToken();
            const idToken = token as IdentifierToken;

            const softKeyword = this._softKeywordToIdentifier(token);

            switch (token.type) {
                case TokenType.Identifier: {
                    name = NameNode.create(idToken);
                    break;
                }
                case TokenType.Keyword: {
                    if (softKeyword) {
                        name = NameNode.create(softKeyword);
                        break;
                    }
                }
            }
        }

        typeNode.operators.push(...operators);
        if (!name) {
            if (this._peekTokenType() === TokenType.OpenParenthesis) {
                const ptrToken = this._peekToken(1);
                if (ptrToken.type === TokenType.Operator) {
                    const opToken = ptrToken as OperatorToken;
                    if (opToken.operatorType === OperatorType.Multiply || opToken.operatorType === OperatorType.Power) {
                        return this._parseCCallback(typeNode);
                    }
                }
            }
            this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
            return errorNode;
        }
        name.typeNode = typeNode;
        this._getTokenIfType(TokenType.String); // Consume alias string
        this._parseCVarTrailer(name.typeNode, false);
        return name;
    }

    private _parsePointersOrRef(allowReference = false, allowCastClose = false) {
        const operators: OperatorToken[] = [];
        const ptrs = [OperatorType.Multiply, OperatorType.Power];
        const valid = [OperatorType.BitwiseAnd, ...ptrs];
        const opTypes: OperatorType[] = [];
        while (this._peekTokenType() === TokenType.Operator) {
            const token = this._peekToken() as OperatorToken;
            if (valid.includes(token.operatorType)) {
                this._getNextToken();
                if (opTypes.length === 0) {
                    if (ptrs.includes(token.operatorType)) {
                        opTypes.push(...ptrs);
                    } else {
                        opTypes.push(token.operatorType);
                    }
                }
                if (token.operatorType === OperatorType.BitwiseAnd && !allowReference) {
                    this._addError(Localizer.DiagnosticCython.referenceNotAllowed(), token);
                } else if (!opTypes.includes(token.operatorType)) {
                    this._addError(Localizer.DiagnosticCython.mixedPointersOrReference(), token);
                } else {
                    operators.push(token);
                }
            } else {
                break;
            }
            // else {
            //     if (!(allowCastClose && token.operatorType === OperatorType.GreaterThan)) {
            //         this._handleExpressionParseError(
            //             ErrorExpressionCategory.InvalidDeclaration,
            //             Localizer.Diagnostic.expectedIdentifier(),
            //             this._getNextToken()
            //         );
            //     }
            //     break;
            // }
        }
        return operators;
    }

    // parse param but only allow annotation
    private _parseCParameterAnnotation() {
        return this._parseCParameter(true);
    }

    // Parse c parameter. One of name or annotation can be undefined
    private _parseCParameter(onlyAnnotation = false, index = 0, isDef = false) {
        const startToken = this._peekToken();
        const typeNode = this._parseCType(/*allowEllipsis*/ true, /*isCParameter*/ true);
        const operators: OperatorToken[] = this._parsePointersOrRef(/*allowReference*/ true);

        let isNameAmbiguous = true;
        if (typeNode.nodeType === ParseNodeType.CType) {
            typeNode.operators.push(...operators);
            isNameAmbiguous = !(
                typeNode.expression.nodeType !== ParseNodeType.Name ||
                typeNode.numModifiers.length > 0 ||
                typeNode.varModifiers.length > 0 ||
                typeNode.operators.length > 0 ||
                typeNode.typeTrailNode !== undefined
            );
        }

        const param = CParameterNode.create(startToken, typeNode);
        if (onlyAnnotation) {
            param.isNameAmbiguous = false;
            return param;
        } else if (
            typeNode.nodeType === ParseNodeType.CType &&
            typeNode.expression.nodeType === ParseNodeType.Ellipsis
        ) {
            param.category = ParameterCategory.VarArgList;
            param.isNameAmbiguous = false;
            return param;
        }

        if (!param.name && typeNode.nodeType !== ParseNodeType.Error) {
            if (this._peekIdentifier() || this._peekTokenType() === TokenType.OpenParenthesis) {
                const maybeName = this._parseCVarName(typeNode, false, /*skipPointersAndRefs*/ true);
                if (maybeName.nodeType === ParseNodeType.Name) {
                    param.name = maybeName;
                    param.name.parent = param;
                    isNameAmbiguous = false;
                    extendRange(param, param.name);
                } else if (maybeName.nodeType === ParseNodeType.CCallback) {
                    param.name = { ...maybeName.name };
                    // TODO: Invalidate name
                    param.name.start = 0;
                    param.name.length = 0;
                    param.typeAnnotation = maybeName;
                    param.name.parent = param;
                    param.typeAnnotation.parent = param;
                    isNameAmbiguous = false;
                    extendRange(param, maybeName);
                }
            }
        }

        const possibleAssign = this._peekToken();
        if (this._consumeTokenIfOperator(OperatorType.Assign)) {
            if (!isNameAmbiguous && !param.name) {
                this._addError(Localizer.Diagnostic.expectedParamName(), possibleAssign);
            }
            const defaultToken = this._peekToken();
            let defaultValue: ExpressionNode | undefined = undefined;
            if (
                defaultToken.type === TokenType.QuestionMark ||
                (defaultToken.type === TokenType.Operator &&
                    (defaultToken as OperatorToken).operatorType === OperatorType.Multiply)
            ) {
                // ! Note these are not allowed in function implementations
                defaultValue = EllipsisNode.create(this._getNextToken());
                param.category = ParameterCategory.Simple;
            } else {
                defaultValue = this._parseTestExpression(/* allowAssignmentExpression */ false);
            }

            if (defaultValue) {
                param.defaultValue = defaultValue;
                param.defaultValue.parent = param;
                extendRange(param, param.defaultValue);
            }
        }

        if (isNameAmbiguous && !param.name) {
            if (
                param.typeAnnotation?.nodeType === ParseNodeType.CType &&
                param.typeAnnotation.expression.nodeType === ParseNodeType.Name
            ) {
                if (param.defaultValue && param.typeAnnotation.expression.nodeType === ParseNodeType.Name) {
                    isNameAmbiguous = false;
                    param.name = param.typeAnnotation.expression;
                    param.typeAnnotation = undefined;
                }
            }
        }

        if (!param.name && param.typeAnnotation) {
            param.name = NameNode.create(IdentifierToken.create(0, 0, `_p${index}`, undefined));
            param.name.parent = param;
        }

        // name can be ambiguous if the entire param consists of a single identifier
        param.isNameAmbiguous = !param.defaultValue && isNameAmbiguous;
        this._parseParameterNoneCheck(/*isDef*/ false);
        return param;
    }

    // Parse C like parameter in python function def
    private _tryParseCLikeParameter(allowAnnotations: boolean) {
        if (this._peekIdentifier() && this._peekToken(1).type === TokenType.Colon) {
            // Python type annotation
            return undefined;
        }
        const wasErrorsSuppressed = this._areErrorsSuppressed;
        this._areErrorsSuppressed = true;
        const index = this._tokenIndex;
        let param: ParameterNode | CParameterNode | undefined = ParameterNode.create(
            this._peekToken(),
            ParameterCategory.Simple
        );
        const cType = this._parseCType();

        if (cType.nodeType === ParseNodeType.CType) {
            this._parseCTypeTrailer(cType);
            const name = this._parseCVarName(cType, /*allowReference*/ false);
            if (name.nodeType === ParseNodeType.Name) {
                param = Object.assign(param, { isCythonAlias: true }) as CParameterNode;
                param.name = name;
                param.typeAnnotation = cType;
                name.parent = param;
                cType.parent = param;
                extendRange(param, name);
                extendRange(param, cType);
            } else {
                param = undefined;
            }
        } else {
            param = undefined;
        }

        this._areErrorsSuppressed = wasErrorsSuppressed;
        if (!param) {
            this._tokenIndex = index;
            param = this._parseParameter(allowAnnotations, /*tryCython*/ false);
        }

        if (param) {
            this._parseParameterNoneCheck(/*isDef*/ true);
        }
        if (param && this._consumeTokenIfOperator(OperatorType.Assign)) {
            param.defaultValue = this._parseTestExpression(/* allowAssignmentExpression */ false);
            param.defaultValue.parent = param;
            extendRange(param, param.defaultValue);
        }
        return param;
    }

    // Parse None check. return true if found
    private _parseParameterNoneCheck(isDef: boolean) {
        // TODO: Parse in condition: `param: type in []` ???
        const possibleNot = this._peekToken();
        const seenNot = this._peekKeywordType() === KeywordType.Not;
        const possibleNone = this._peekToken(1);
        const seenNone =
            possibleNone.type === TokenType.Keyword && (possibleNone as KeywordToken).keywordType === KeywordType.None;
        if (seenNot && seenNone) {
            if (!isDef) {
                const range = TextRange.create(possibleNot.start, possibleNot.length);
                TextRange.extend(range, TextRange.create(possibleNone.start, possibleNone.length));
                this._addError(Localizer.DiagnosticCython.noneCheckNotAllowed(), range);
            }
            this._getNextToken();
            this._getNextToken();
            return true;
        }
        return false;
    }

    private _parseCVarArgsList(terminator: TokenType, allowAnnotations: boolean): ParameterNode[] {
        const paramMap = new Map<string, string>();
        const paramList: ParameterNode[] = [];
        let sawDefaultParam = false;
        let reportedNonDefaultParamErr = false;
        let sawKeywordOnlySeparator = false;
        let sawPositionOnlySeparator = false;
        let sawKeywordOnlyParamAfterSeparator = false;
        let sawArgs = false;
        let sawKwArgs = false;
        let index = 0;

        while (true) {
            if (this._peekTokenType() === terminator) {
                break;
            }
            let param: ParameterNode | undefined;
            if (
                allowAnnotations &&
                this._peekTokenType() === TokenType.Identifier &&
                this._peekToken(1).type === TokenType.Colon
            ) {
                // Allow python type annotation
                param = this._parseParameter(allowAnnotations);
            } else {
                param = this._parseCParameter(false, index);
            }
            index++;
            if (!param) {
                this._consumeTokensUntilType([terminator]);
                break;
            }

            if (param.name) {
                const name = param.name.value;
                if (paramMap.has(name)) {
                    this._addError(Localizer.Diagnostic.duplicateParam().format({ name }), param.name);
                } else {
                    paramMap.set(name, name);
                }
            } else if (param.category === ParameterCategory.Simple) {
                if (paramList.length === 0) {
                    this._addError(Localizer.Diagnostic.positionOnlyFirstParam(), param);
                }
            }

            if (param.category === ParameterCategory.Simple) {
                if (!param.name) {
                    if (sawPositionOnlySeparator) {
                        this._addError(Localizer.Diagnostic.duplicatePositionOnly(), param);
                    } else if (sawKeywordOnlySeparator) {
                        this._addError(Localizer.Diagnostic.positionOnlyAfterKeywordOnly(), param);
                    } else if (sawArgs) {
                        this._addError(Localizer.Diagnostic.positionOnlyAfterArgs(), param);
                    }
                    sawPositionOnlySeparator = true;
                } else {
                    if (sawKeywordOnlySeparator) {
                        sawKeywordOnlyParamAfterSeparator = true;
                    }

                    if (param.defaultValue) {
                        sawDefaultParam = true;
                    } else if (sawDefaultParam && !sawKeywordOnlySeparator && !sawArgs) {
                        // Report this error only once.
                        if (!reportedNonDefaultParamErr) {
                            this._addError(Localizer.Diagnostic.nonDefaultAfterDefault(), param);
                            reportedNonDefaultParamErr = true;
                        }
                    }
                }
            }

            paramList.push(param);

            if (param.category === ParameterCategory.VarArgList) {
                if (!param.name) {
                    if (sawKeywordOnlySeparator) {
                        this._addError(Localizer.Diagnostic.duplicateKeywordOnly(), param);
                    } else if (sawArgs) {
                        this._addError(Localizer.Diagnostic.keywordOnlyAfterArgs(), param);
                    }
                    sawKeywordOnlySeparator = true;
                } else {
                    if (sawKeywordOnlySeparator || sawArgs) {
                        this._addError(Localizer.Diagnostic.duplicateArgsParam(), param);
                    }
                    sawArgs = true;
                }
            }

            if (param.category === ParameterCategory.VarArgDictionary) {
                if (sawKwArgs) {
                    this._addError(Localizer.Diagnostic.duplicateKwargsParam(), param);
                }
                sawKwArgs = true;

                // A **kwargs cannot immediately follow a keyword-only separator ("*").
                if (sawKeywordOnlySeparator && !sawKeywordOnlyParamAfterSeparator) {
                    this._addError(Localizer.Diagnostic.keywordParameterMissing(), param);
                }
            } else if (sawKwArgs) {
                this._addError(Localizer.Diagnostic.paramAfterKwargsParam(), param);
            }

            const foundComma = this._consumeTokenIfType(TokenType.Comma);

            if (allowAnnotations && !param.typeAnnotation) {
                // Look for a type annotation comment at the end of the line.
                const typeAnnotationComment = this._parseVariableTypeAnnotationComment();
                if (typeAnnotationComment) {
                    param.typeAnnotationComment = typeAnnotationComment;
                    param.typeAnnotationComment.parent = param;
                    extendRange(param, param.typeAnnotationComment);
                }
            }

            if (!foundComma) {
                break;
            }
        }

        if (paramList.length > 0) {
            const lastParam = paramList[paramList.length - 1];
            if (lastParam.category === ParameterCategory.VarArgList && !lastParam.name) {
                const annotation = lastParam.typeAnnotation;
                if (
                    !(
                        annotation?.nodeType === ParseNodeType.CType &&
                        annotation.expression.nodeType === ParseNodeType.Ellipsis
                    )
                ) {
                    this._addError(Localizer.Diagnostic.expectedNamedParameter(), lastParam);
                }
            }
        }

        return paramList;
    }

    // parse 'with gil'
    private _consumeIfWithGil() {
        const possibleWithToken = this._peekToken();
        const possibleGilToken = this._peekToken(1);
        if (possibleWithToken.type === TokenType.Keyword && possibleGilToken.type === TokenType.Keyword) {
            if (
                (possibleWithToken as KeywordToken).keywordType === KeywordType.With &&
                (possibleGilToken as KeywordToken).keywordType === KeywordType.Gil
            ) {
                this._getNextToken();
                this._getNextToken();
                return true;
            }
        }
        return false;
    }

    private _parseCBlockTrail(allowConst = false) {
        const token = this._peekToken();
        const otherTokens: Token[] = [];
        let trailType: CBlockTrailType | undefined = undefined;
        let exceptToken: Token | undefined = undefined;
        let expr: ExpressionNode | undefined = undefined;

        // TODO: Make TrailType a flag
        if (this._consumeTokenIfKeyword(KeywordType.Noexcept)) {
            trailType = CBlockTrailType.NoExcept;
        } else if (this._consumeTokenIfKeyword(KeywordType.Except)) {
            trailType = CBlockTrailType.Except;
            const op = this._peekOperatorType();
            if (op === OperatorType.Multiply || op === OperatorType.Add) {
                exceptToken = this._getNextToken();
                otherTokens.push(exceptToken);
            } else {
                expr = this._parseTestExpression(/*allowAssignmentExpression*/ false);
            }
        }

        if (this._consumeIfWithGil()) {
            otherTokens.push(this._peekToken(-1));
            trailType = CBlockTrailType.WithGil;
        } else if (this._consumeTokenIfKeyword(KeywordType.Nogil)) {
            trailType = CBlockTrailType.NoGil;
        } else if (allowConst && this._consumeTokenIfKeyword(KeywordType.Const)) {
            trailType = CBlockTrailType.Const;
        }

        if (trailType === undefined) {
            return undefined;
        }
        return CBlockTrailNode.create(token, otherTokens, trailType, exceptToken, expr);
    }

    private _parseCFunction(name: NameNode, returnType?: CTypeNode, decorators?: DecoratorNode[]) {
        let typeParams: TypeParameterListNode | undefined = undefined;
        if (this._peekTokenType() === TokenType.OpenBracket) {
            typeParams = this._parseTypeParameterList();
        }
        const openParenToken = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            this._addError(Localizer.Diagnostic.expectedOpenParen(), this._peekToken());
            return ErrorNode.create(name.token, ErrorExpressionCategory.MissingFunctionParameterList, name, decorators);
        }

        const paramList = this._parseCVarArgsList(TokenType.CloseParenthesis, /* allowAnnotations */ true);

        if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
            this._addError(Localizer.Diagnostic.expectedCloseParen(), openParenToken);
        }

        const trailBlock = this._parseCBlockTrail(/*allowConst*/ true);

        let suite: SuiteNode;
        let isForwardDecl = false;
        if (this._peekTokenType() === TokenType.Colon) {
            suite = this._parseSuite(/* isFunction */ true, this._parseOptions.skipFunctionAndClassBody);
        } else {
            // This is a forward declaration
            suite = SuiteNode.create(this._peekToken());
            isForwardDecl = true;
            this._expectNewLine();
        }

        const functionNode = CFunctionNode.create(openParenToken, name, suite, isForwardDecl);
        if (trailBlock) {
            functionNode.blockTrail = trailBlock;
            extendRange(functionNode, trailBlock);
        }
        functionNode.parameters = paramList;
        paramList.forEach((param) => {
            param.parent = functionNode;
            if (!isForwardDecl && CParameterNode.isInstance(param) && param.isNameAmbiguous && param.typeAnnotation) {
                // If this is not a forward declaration and name is ambiguous
                // assume that the annotation was the name
                const annotation = param.typeAnnotation;
                if (
                    annotation.nodeType === ParseNodeType.CType &&
                    annotation.expression.nodeType === ParseNodeType.Name
                ) {
                    param.name = annotation.expression;
                    param.name.parent = param;
                    param.typeAnnotation = undefined;
                }
            }
        });

        if (decorators) {
            functionNode.decorators = decorators;
            decorators.forEach((decorator) => {
                decorator.parent = functionNode;
            });

            if (decorators.length > 0) {
                extendRange(functionNode, decorators[0]);
            }
        }

        if (returnType) {
            functionNode.returnTypeAnnotation = returnType;
            functionNode.returnTypeAnnotation.parent = functionNode;
            extendRange(functionNode, returnType);
        }

        if (typeParams) {
            functionNode.typeParameters = typeParams;
            typeParams.parent = functionNode;
        }

        return functionNode;
    }

    // parse c callback declaration
    private _parseCCallback(returnType: CTypeNode) {
        const startToken = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.Diagnostic.expectedOpenParen(),
                this._peekToken()
            );
        }
        const pointers = this._parsePointersOrRef(/*allowReference*/ false);
        if (pointers.length === 0) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.DiagnosticCython.expectedPointer(),
                this._peekToken()
            );
        }
        const iden = this._getTokenIfIdentifier();
        if (!iden) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.Diagnostic.expectedIdentifier(),
                this._peekToken()
            );
        }
        const name = NameNode.create(iden);
        if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingCallCloseParen,
                Localizer.Diagnostic.expectedCloseParen(),
                this._peekToken()
            );
        }
        if (!this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.Diagnostic.expectedOpenParen(),
                this._peekToken()
            );
        }

        const params: CParameterNode[] = [];
        if (this._peekTokenType() !== TokenType.CloseParenthesis) {
            let param = this._parseCParameter();
            if (param.typeAnnotation && param.typeAnnotation.nodeType !== ParseNodeType.Error) {
                params.push(param);
            }
            while (this._peekTokenType() === TokenType.Comma) {
                this._getNextToken();
                param = this._parseCParameter();
                if (param.typeAnnotation && param.typeAnnotation.nodeType !== ParseNodeType.Error) {
                    params.push(param);
                }
            }
        }

        const closeToken = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingCallCloseParen,
                Localizer.Diagnostic.expectedCloseParen(),
                this._peekToken()
            );
        }

        const node = CCallbackNode.create(startToken, name);
        node.returnTypeAnnotation = returnType;
        returnType.parent = node;
        extendRange(node, returnType);
        params.forEach((p) => {
            node.parameters.push(p);
            p.parent = node;
        });
        extendRange(node, closeToken);
        const trailBlock = this._parseCBlockTrail();
        if (trailBlock) {
            node.blockTrail = trailBlock;
            extendRange(node, trailBlock);
        }
        this._expectNewLine();
        return node;
    }

    private _parseCTuple() {
        const nodes: CTypeNode[] = [];
        const startToken = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.Diagnostic.expectedOpenParen()
            );
        }
        const firstNode = this._parseCType();
        if (firstNode?.nodeType === ParseNodeType.CType) {
            firstNode.operators.push(...this._parsePointersOrRef());
            nodes.push(firstNode);
        } else {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedIdentifier()
            );
        }
        while (this._peekTokenType() !== TokenType.CloseParenthesis) {
            if (!this._consumeTokenIfType(TokenType.Comma)) {
                return this._handleExpressionParseError(
                    ErrorExpressionCategory.MissingFunctionParameterList,
                    Localizer.Diagnostic.typeArgListExpected()
                );
            }
            const nextNode = this._parseCType();
            if (nextNode?.nodeType === ParseNodeType.CType) {
                nextNode.operators.push(...this._parsePointersOrRef());
                nodes.push(nextNode);
            } else {
                return this._handleExpressionParseError(
                    ErrorExpressionCategory.MissingExpression,
                    Localizer.Diagnostic.expectedIdentifier()
                );
            }
        }
        const closeToken = this._peekToken();
        if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingTupleCloseParen,
                Localizer.Diagnostic.expectedCloseParen()
            );
        }
        if (nodes.length === 0) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedIdentifier()
            );
        }
        const tupleNode = CTupleTypeNode.create(startToken, nodes, closeToken);
        const node = CTypeNode.create(tupleNode, [], [], []);
        return node;
    }

    private _parseCType(allowEllipsis = false, isCParameter = false) {
        const signedTypes = ['char', 'short', 'int', 'long'];
        const signedMods = ['signed', 'unsigned'];
        const identifiers: IdentifierToken[] = [];
        const modifiers: KeywordToken[] = [];
        const numModifiers: IdentifierToken[] = [];
        let longCount = 0;
        const errorNode = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.MissingExpression);
        const validTypes = [TokenType.Keyword, TokenType.Identifier];
        let expression: ExpressionNode | undefined = undefined;

        if (this._peekTokenType() === TokenType.OpenParenthesis) {
            return this._parseCTuple();
        }
        if (allowEllipsis && this._peekTokenType() === TokenType.Ellipsis) {
            return CTypeNode.create(EllipsisNode.create(this._getNextToken()), modifiers, numModifiers, []);
        }
        while (validTypes.includes(this._peekTokenType())) {
            const token = this._getNextToken();
            const kwToken = token as KeywordToken;
            const idToken = token as IdentifierToken;
            const softKeyword = this._softKeywordToIdentifier(token);

            switch (token.type) {
                case TokenType.Keyword:
                    // Look for VarModifiers then numerical modifiers
                    if (varModifiers.includes(kwToken.keywordType)) {
                        modifiers.push(kwToken);
                    } else if (numericModifiers.includes(kwToken.keywordType)) {
                        numModifiers.push(this._keywordToIdentifier(kwToken));
                        if (kwToken.keywordType === KeywordType.Long) {
                            longCount++;
                        }
                    } else if (softKeyword) {
                        identifiers.push(softKeyword);
                    }
                    break;
                case TokenType.Identifier:
                    identifiers.push(idToken);
                    break;

                default:
                    // TODO: unhandled error
                    return errorNode;
            }

            // Determine if we need to break out of loop

            const nextOp = this._peekOperatorType();
            const nextToken = this._peekToken();
            const nextId = nextToken.type === TokenType.Identifier ? (nextToken as IdentifierToken) : undefined;
            const nextKeyword = this._peekKeywordType();

            // Compound types
            // I.e. `double complex` `float complex` `short int`
            const compoundTypePrefixes = ['short', 'double', 'float'];
            const compoundTypeSuffixes = ['int', 'complex'];
            const longTypes = ['int', 'short', 'double', 'complex']; // Types that can be prefixed with long

            if (nextOp || nextToken.type === TokenType.OpenBracket) {
                break;
            }

            if (nextToken.type === TokenType.Dot) {
                if (identifiers.length < 1) {
                    this._addError(Localizer.Diagnostic.expectedIdentifier(), nextToken);
                    return errorNode;
                }
                expression = NameNode.create(identifiers[identifiers.length - 1]);
                while (this._consumeTokenIfType(TokenType.Dot)) {
                    const member = this._getTokenIfIdentifier();
                    if (!member) {
                        this._addError(Localizer.Diagnostic.expectedMemberName(), this._peekToken());
                        break;
                    }
                    expression = MemberAccessNode.create(expression, NameNode.create(member));
                }
                break;
            }

            if (nextKeyword) {
                if (varModifiers.includes(nextKeyword)) {
                    if (numModifiers.length > 0 || identifiers.length > 0) {
                        // varModifier after numModifier, identifier
                        break;
                    }
                    if (modifiers.length >= 2) {
                        // Only two modifiers max are allowed
                        break;
                    }
                    if (modifiers.length === 1) {
                        // Verify modifiers are correct and are in correct order
                        if (modifiers[0].keywordType === nextKeyword) {
                            // Don't allow same modifier twice
                            break;
                        }
                        if (nextKeyword !== KeywordType.Const) {
                            // `const` should be last modifier
                            break;
                        }
                    }
                } else if (numericModifiers.includes(nextKeyword)) {
                    if (identifiers.length > 0) {
                        // numeric modifier cannot follow an identifier
                        break;
                    } else if (longCount >= 2) {
                        // Only 2 'long' tokens are allowed
                        break;
                    }
                    continue;
                } else {
                    break;
                }
            }

            if (identifiers.length >= 2) {
                // Compound Type
                break;
            }

            if (kwToken.keywordType === KeywordType.Long) {
                if (nextId && !longTypes.includes(nextId.value)) {
                    // Probably a long type
                    // I.e. `long` `long long`
                    break;
                }
            }

            if (identifiers.length === 1 && nextId) {
                if (
                    !compoundTypePrefixes.includes(identifiers[0].value) ||
                    !compoundTypeSuffixes.includes(nextId.value)
                ) {
                    // Not a valid compound type stop here
                    break;
                }
            }
        }

        if (!expression) {
            const lastNumMod = numModifiers.length ? numModifiers[numModifiers.length - 1] : undefined;
            if (identifiers.length === 0) {
                if (longCount > 0) {
                    // Type ending with `long`
                    // Remove the last `long` from num modifiers
                    // Add to identifiers
                    identifiers.push(numModifiers.pop()!);
                } else if (lastNumMod && signedMods.includes(lastNumMod.value)) {
                    // Handle case where only unsigned/signed is given with no identifier following it: `cdef unsigned* var`
                    // Create dummy expression. The type is assumed to be 'int'.
                    expression = NameNode.create(IdentifierToken.create(0, 0, 'int', undefined));
                }
            } else if (identifiers.length > 1) {
                // Compound type. Remove the first identifier and append it to numModifiers
                numModifiers.push(identifiers[0]);
                identifiers.splice(0, 1);
            }

            if (identifiers.length > 0) {
                const lastIdent = identifiers[identifiers.length - 1];
                if (identifiers.length === 1) {
                    // Handle case where only unsigned/signed is given: `cdef unsigned var`
                    if (lastNumMod && signedMods.includes(lastNumMod.value) && !signedTypes.includes(lastIdent.value)) {
                        // Create dummy expression if identifier is not a signed type. The type is assumed to be 'int'.
                        expression = NameNode.create(IdentifierToken.create(0, 0, 'int', undefined));
                        // Back up the token index
                        this._tokenIndex--;
                    }
                }
                if (!expression) {
                    expression = NameNode.create(lastIdent);
                }
            }
        }

        if (expression) {
            const node = CTypeNode.create(expression, modifiers, numModifiers, []);
            this._parseCTypeTrailer(node, isCParameter);
            if (node.typeTrailNode && node.typeTrailNode.trailType & CTrailType.Template) {
                // Create a dummy base for type evaluation
                let base = { ...node.expression, start: 0, length: 0 };
                while (this._consumeTokenIfType(TokenType.Dot)) {
                    // member access after a template; i.e. 'type[T].member'
                    const memberToken = this._getTokenIfIdentifier();
                    if (!memberToken) {
                        this._addError(Localizer.Diagnostic.expectedMemberName(), this._peekToken());
                        break;
                    }
                    const memberName = NameNode.create(memberToken);
                    node.typeTrailNode.postMemberNode = MemberAccessNode.create(base, memberName);
                    node.typeTrailNode.postMemberNode.parent = node.typeTrailNode;
                    extendRange(node.typeTrailNode, node.typeTrailNode.postMemberNode);
                    extendRange(node, node.typeTrailNode);
                    base = node.typeTrailNode.postMemberNode;
                    // TODO: Is additional template args after member allowed?
                }
            }

            return node;
        }
        this._addError(Localizer.Diagnostic.expectedIdentifier(), this._peekToken());
        return errorNode;
    }

    // Special parsing when parsing cdef statements in suite
    private _parseCDefStatementInSuite(): StatementNode | undefined {
        let statement: StatementNode;

        // Handle keywords that are unexpected
        // cdef is optional here so keyword can also be type modifiers
        const validKeyWords = [...varModifiers, ...numericModifiers, KeywordType.Extern];
        const otherKeyWords = [
            KeywordType.Struct,
            KeywordType.Packed,
            KeywordType.Enum,
            KeywordType.Union,
            KeywordType.Fused,
            KeywordType.Ctypedef,
            KeywordType.Cdef,
            KeywordType.Cpdef,
            KeywordType.Cppclass,
            KeywordType.Operator,
        ];
        if (!this._isParsingCStruct) {
            validKeyWords.push(...otherKeyWords);
        }

        // Parse allowed python statements
        const kwType = this._peekKeywordType();
        switch (kwType) {
            case KeywordType.Def:
            case KeywordType.Class:
                return this._parseStatement();
            case KeywordType.Pass: {
                statement = StatementListNode.create(this._peekToken());
                this._pushStatements(statement, this._parsePassStatement());
                this._expectNewLine();
                this._consumeTokenIfType(TokenType.NewLine);
                return statement;
            }
        }

        if (!kwType || validKeyWords.includes(kwType)) {
            if (kwType === KeywordType.Ctypedef) {
                statement = this._parseCTypeDef();
            } else if (!kwType && this._peekTokenType() === TokenType.String) {
                // Parse the inline code comment or docstring
                statement = StatementListNode.create(this._peekToken());
                const stringNode = this._parseAtom();
                this._pushStatements(statement, stringNode);
            } else if (kwType === KeywordType.Cppclass) {
                return this._parseCppClassDef();
            } else if (kwType === KeywordType.Cdef) {
                // Consume redundant 'cdef'
                this._consumeTokenIfKeyword(KeywordType.Cdef);
                return this._parseCDefStatementInSuite();
            } else if (kwType === KeywordType.Cpdef) {
                return this._parseCpdef();
            } else if (this._isParsingCFused) {
                const param = this._parseCParameterAnnotation();
                statement = StatementListNode.create(this._peekToken());
                this._pushStatements(statement, param);
            } else if (this._peekOperatorType() === OperatorType.MatrixMultiply) {
                statement = this._parseDecorated(/*isInCSuite*/ true);
            } else {
                this._consumeTokenIfKeyword(KeywordType.Extern); // Handle one line extern
                statement = this._parseCVarDecl();
            }
        } else {
            // Unexpected keyword
            const error = ErrorNode.create(this._peekToken(), ErrorExpressionCategory.InvalidDeclaration);
            this._addError(Localizer.Diagnostic.expectedIdentifier(), error);
            statement = StatementListNode.create(this._peekToken());
            this._pushStatements(statement, error);
            // Go to newline
            this._consumeTokensUntilType([TokenType.NewLine]);
        }

        const atNewLine = this._consumeTokenIfType(TokenType.NewLine);
        if (this._isParsingCStruct) {
            if (statement.nodeType === ParseNodeType.StatementList) {
                for (const expr of statement.statements) {
                    if (expr.nodeType === ParseNodeType.Assignment) {
                        this._addError(Localizer.DiagnosticCython.defaultValuesNotAllowed(), expr.rightExpression);
                    }
                }
            }
        }
        if (!atNewLine && statement.nodeType === ParseNodeType.StatementList) {
            if (statement.statements.length === 1 && statement.statements[0].nodeType === ParseNodeType.Error) {
                // If error and not at new line, consume line to avoid infinite loop
                this._consumeTokensUntilType([TokenType.NewLine]);
            }
        }
        return statement;
    }

    private _parseCDefSuite(token: Token, nogil?: boolean) {
        let hasNoGil = false;
        if (nogil && this._consumeTokenIfKeyword(KeywordType.Nogil)) {
            hasNoGil = true;
        }
        const node = this._parseSuite(false, false, undefined, /*isCdefSuite*/ true);
        node.nogil = hasNoGil;
        return node;
    }

    private _parseExtern(token: Token) {
        const wasParsingCExtern = this._isParsingCExtern;
        this._isParsingCExtern = true;
        let error = false;
        let fileNameToken: StringToken | OperatorToken | undefined = undefined;
        let nameSpaceToken: StringToken | undefined = undefined;
        if (!this._consumeTokenIfKeyword(KeywordType.Extern)) {
            this._addError(Localizer.DiagnosticCython.expectedExtern(), this._peekToken());
            error = true;
        }
        if (!error && !this._consumeTokenIfKeyword(KeywordType.From)) {
            this._addError(Localizer.DiagnosticCython.expectedFrom(), this._peekToken());
            error = true;
        }
        if (!error) {
            const maybeString = this._getTokenIfType(TokenType.String);
            if (maybeString) {
                fileNameToken = maybeString as StringToken;
            }
            if (!fileNameToken && this._peekOperatorType() === OperatorType.Multiply) {
                fileNameToken = this._getNextToken() as OperatorToken;
            }
            if (!fileNameToken) {
                this._addError(Localizer.DiagnosticCython.expectedExternFileName(), this._peekToken());
                error = true;
            }
        }
        if (!error && this._consumeTokenIfKeyword(KeywordType.Namespace)) {
            const nextToken = this._getTokenIfType(TokenType.String);
            if (nextToken) {
                nameSpaceToken = nextToken as StringToken;
            } else {
                this._addError(Localizer.DiagnosticCython.expectedNameSpace(), this._peekToken());
                error = true;
            }
        }
        if (error) {
            this._consumeTokensUntilType([TokenType.Colon, TokenType.NewLine]);
        }
        const suite = this._parseCDefSuite(this._peekToken(), /*nogil*/ true);
        const node = CExternNode.create(token, suite);
        node.fileNameToken = fileNameToken;
        node.nameSpaceToken = nameSpaceToken;
        this._isParsingCExtern = wasParsingCExtern;
        return node;
    }

    // parse define macro: `DEF name = value`
    private _parseCDefine() {
        const defToken = this._getKeywordToken(KeywordType.DEF);
        const iden = this._getTokenIfIdentifier();
        if (!iden) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedIdentifier()
            );
        }
        const name = NameNode.create(iden);
        if (!this._consumeTokenIfOperator(OperatorType.Assign)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedEquals()
            );
        }
        const expr = this._parseTestExpression(false);
        const assign = AssignmentNode.create(name, expr);
        const node = CDefineNode.create(defToken, assign);
        this._expectNewLine();
        this._consumeTokenIfType(TokenType.NewLine);
        return node;
    }

    // parse C macro IF ELSE statement
    private _parseCIfStatement(keywordType: KeywordType.IF | KeywordType.ELIF = KeywordType.IF): IfNode {
        const ifOrElifToken = this._getKeywordToken(keywordType);

        const test = this._parseTestExpression(/* allowAssignmentExpression */ true);
        const suite = this._parseSuite(this._isInFunction);
        const ifNode = IfNode.create(ifOrElifToken, test, suite);
        ifNode.cython = true;

        if (this._consumeTokenIfKeyword(KeywordType.ELSE)) {
            ifNode.elseSuite = this._parseSuite(this._isInFunction);
            ifNode.elseSuite.parent = ifNode;
            extendRange(ifNode, ifNode.elseSuite);
        } else if (this._peekKeywordType() === KeywordType.ELIF) {
            // Recursively handle an "elif" statement.
            ifNode.elseSuite = this._parseCIfStatement(KeywordType.ELIF);
            ifNode.elseSuite.parent = ifNode;
            extendRange(ifNode, ifNode.elseSuite);
        }

        return ifNode;
    }

    // include "path/to/file.pxd"
    private _parseInclude() {
        // TODO: Technically this should include the entire contents of the file
        return this._parseIncludeAsImport();
    }

    private _parseIncludeAsImport() {
        // Parse as a wild card import.
        const includeToken = this._getKeywordToken(KeywordType.Include);
        const statement = StatementListNode.create(this._peekToken());
        const stringToken = this._peekToken();
        const stringList = this._parseAtom();
        const stringNode =
            stringList.nodeType === ParseNodeType.StringList && stringList.strings.length === 1
                ? stringList.strings[stringList.strings.length - 1]
                : undefined;
        if (!stringNode || stringNode.nodeType === ParseNodeType.FormatString) {
            const errorNode = this._handleExpressionParseError(
                ErrorExpressionCategory.InvalidDeclaration,
                Localizer.DiagnosticCython.expectedStringForInclude(),
                stringToken
            );
            this._pushStatements(statement, errorNode);
            return statement;
        }

        const value = stringNode.value;

        let leadingDots = 0;
        for (const char of value) {
            if (char !== '.') {
                break;
            }
            leadingDots++;
        }

        const ext = getFileExtension(value);
        const text = stripFileExtension(value);
        // Parse tokens inside string
        const tokenizer = new Tokenizer();
        const tokens = tokenizer.tokenize(text).tokens;
        const offset = stringNode.token.prefixLength + stringNode.token.quoteMarkLength;

        const moduleName = ModuleNameNode.create(stringList);
        moduleName.leadingDots = leadingDots;

        for (let i = 0; i < tokens.count; i++) {
            let token = tokens.getItemAt(i);
            if (token.type === TokenType.Keyword) {
                token = IdentifierToken.create(
                    token.start,
                    token.length,
                    text.slice(token.start, token.start + token.length),
                    undefined
                );
            }
            if (token.type === TokenType.Identifier) {
                token.start += stringNode.start + offset; // offset for first quotation at start of string
                const name = NameNode.create(token as IdentifierToken);
                moduleName.nameParts.push(name);
                name.parent = moduleName;
            }
        }
        if (moduleName.nameParts.length > 0) {
            moduleName.nameParts[moduleName.nameParts.length - 1].length += ext.length;
        }

        const importFromNode = ImportFromNode.create(includeToken, moduleName);
        importFromNode.isWildcardImport = true;
        importFromNode.wildcardToken = OperatorToken.create(0, 0, OperatorType.Multiply, undefined);
        const pxdImport = {
            nameNode: importFromNode.module,
            leadingDots: importFromNode.module.leadingDots,
            nameParts: importFromNode.module.nameParts.map((p) => p.value),
            importedSymbols: importFromNode.imports.map((imp) => imp.name.value),
            isCython: true,
            cythonExt: ext.replace(/^\./, ''),
        };
        this._importedModules.push(pxdImport);
        this._pushStatements(statement, importFromNode);
        return statement;
    }

    // Legacy property declaration:
    // property prop_name:
    //     def __get__(self): ...
    //     def __set__(self, value): ...
    //     def __del__(self): ...
    private _parseCLegacyProperty() {
        const startToken = this._peekToken();
        const statements = StatementListNode.create(startToken);
        const propToken = this._getTokenIfIdentifier();

        if (propToken?.value === 'property') {
            const nameToken = this._getTokenIfIdentifier();
            if (!nameToken) {
                this._pushStatements(
                    statements,
                    this._handleExpressionParseError(
                        ErrorExpressionCategory.MissingExpression,
                        Localizer.Diagnostic.expectedIdentifier(),
                        this._peekToken()
                    )
                );
                return statements;
            }
            const suite = this._parseSuite();
            const node = CPropertyNode.create(propToken, nameToken, suite);
            this._addDeprecated(Localizer.DiagnosticCython.legacyPropertyDeclaration(), propToken);
            return node;
        }

        this._pushStatements(
            statements,
            this._handleExpressionParseError(
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedIdentifier(),
                startToken
            )
        );
        return statements;
    }

    private _parseCppClassDef(decorators?: DecoratorNode[]): ClassNode {
        const classToken = this._getKeywordToken(KeywordType.Cppclass);

        let nameToken = this._getTokenIfIdentifier();
        if (!nameToken) {
            this._addError(Localizer.Diagnostic.expectedClassName(), this._peekToken());
            nameToken = IdentifierToken.create(0, 0, '', /* comments */ undefined);
        }

        let typeParameters: TypeParameterListNode | undefined;
        const possibleOpenBracket = this._peekToken();
        if (possibleOpenBracket.type === TokenType.OpenBracket) {
            typeParameters = this._parseTypeParameterList(/*isCppClass*/ true);
        }

        let argList: ArgumentNode[] = [];
        const openParenToken = this._peekToken();
        if (this._consumeTokenIfType(TokenType.OpenParenthesis)) {
            argList = this._parseArgList().args;

            if (!this._consumeTokenIfType(TokenType.CloseParenthesis)) {
                this._addError(Localizer.Diagnostic.expectedCloseParen(), openParenToken);
            }
        }

        const nextToken = this._peekToken();
        let suite = SuiteNode.create(nextToken);
        if (nextToken.type === TokenType.Semicolon || nextToken.type === TokenType.NewLine) {
            this._expectNewLine();
            this._consumeTokenIfType(TokenType.NewLine);
        } else {
            suite = this._parseSuite(
                /* isFunction */ false,
                this._parseOptions.skipFunctionAndClassBody,
                undefined,
                /*isCdefSuite*/ true
            );
        }

        const classNode = ClassNode.create(classToken, NameNode.create(nameToken), suite, typeParameters);
        classNode.isCython = true;
        classNode.structType = CStructType.CppClass;
        classNode.arguments = argList;
        argList.forEach((arg) => {
            arg.parent = classNode;
        });

        if (decorators) {
            classNode.decorators = decorators;
            if (decorators.length > 0) {
                decorators.forEach((decorator) => {
                    decorator.parent = classNode;
                });
                extendRange(classNode, decorators[0]);
            }
        }

        return classNode;
    }

    // Parse with gil/nogil context
    private _parseCGilContext() {
        const token = this._peekToken();
        const kwType = this._peekKeywordType();
        if (kwType === KeywordType.Nogil || kwType === KeywordType.Gil) {
            this._getNextToken();
            const condition = this._getTokenIfType(TokenType.OpenParenthesis)
                ? this._parseTestExpression(/* allowAssignmentExpression */ false)
                : undefined;
            const closingToken = condition ? this._getTokenIfType(TokenType.CloseParenthesis) : undefined;
            if (condition && !closingToken) {
                this._addError(Localizer.Diagnostic.expectedCloseParen(), this._peekToken());
            }
            return CGilNode.create(token, kwType === KeywordType.Nogil, condition, closingToken);
        }
        return undefined;
    }

    // Parse C Extension wrapper
    // https://cython.readthedocs.io/en/latest/src/userguide/extension_types.html
    private _parseCClassExt() {
        const token = this._getKeywordToken(KeywordType.Class);
        // Name must be module.name
        const module = this._getTokenIfIdentifier();
        if (!module) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingExpression,
                Localizer.Diagnostic.expectedModuleName(),
                this._peekToken()
            );
        }
        if (!this._getTokenIfType(TokenType.Dot)) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingMemberAccessName,
                Localizer.Diagnostic.expectedMemberName(),
                this._peekToken()
            );
        }
        const name = this._getTokenIfIdentifier();
        if (!name) {
            return this._handleExpressionParseError(
                ErrorExpressionCategory.MissingMemberAccessName,
                Localizer.Diagnostic.expectedMemberName(),
                this._peekToken()
            );
        }

        const nameSpec: ParameterNode[] = [];

        //Name Spec [object object_struct_name, type type_object_name, check_size cs_option]
        const openBracket = this._getTokenIfType(TokenType.OpenBracket);
        if (openBracket) {
            while (true) {
                const argName = this._getTokenIfIdentifier();
                if (!argName) {
                    return this._handleExpressionParseError(
                        ErrorExpressionCategory.MissingExpression,
                        Localizer.Diagnostic.expectedNamedParameter(),
                        this._peekToken()
                    );
                }
                const argValue = this._getTokenIfIdentifier();
                if (!argValue) {
                    return this._handleExpressionParseError(
                        ErrorExpressionCategory.MissingExpression,
                        Localizer.Diagnostic.expectedNamedParameter(),
                        this._peekToken()
                    );
                }
                const arg = ParameterNode.create(argName, ParameterCategory.Simple);
                arg.name = NameNode.create(argName);
                arg.defaultValue = NameNode.create(argValue);
                arg.name.parent = arg;
                arg.defaultValue.parent = arg;
                extendRange(arg, arg.defaultValue);
                nameSpec.push(arg);
                if (!this._getTokenIfType(TokenType.Comma)) {
                    break;
                }
            }
        }
        if (openBracket) {
            if (nameSpec.length === 0) {
                this._addError(Localizer.Diagnostic.expectedParamName(), this._peekToken());
            }
            if (!this._getTokenIfType(TokenType.CloseBracket)) {
                return this._handleExpressionParseError(
                    ErrorExpressionCategory.MissingIndexCloseBracket,
                    Localizer.Diagnostic.expectedCloseBracket(),
                    this._peekToken()
                );
            }
        }
        const suite = this._parseSuite();
        const node = CClassExtNode.create(
            token,
            NameNode.create(module),
            NameNode.create(name),
            suite,
            undefined,
            nameSpec
        );
        node.structType = CStructType.ClassExt;
        return node;
    }

    // Create wildcard import for a matching 'pxd' import. These do not have to be explicitly imported for 'pyx' files.
    // Example: If this file path is 'package/module.pyx', import the 'package/module.pxd' file as 'from package.module cimport *'
    static getMatchingDeclarationImport(moduleName: string) {
        const module = ModuleNameNode.create(TextRange.create(0, 0));
        const nameParts = moduleName.split('.');
        for (let index = 0; index < moduleName.length; index++) {
            if (moduleName.charAt(index) === '.') {
                module.leadingDots++;
                continue;
            }
            break;
        }
        for (const part of nameParts) {
            if (part !== '') {
                const token = IdentifierToken.create(0, 0, part, undefined);
                module.nameParts.push(NameNode.create(token));
            }
        }
        const fromToken = KeywordToken.create(0, 0, KeywordType.From, undefined);
        const importFromNode = ImportFromNode.create(fromToken, module);
        importFromNode.isWildcardImport = true;
        importFromNode.wildcardToken = OperatorToken.create(0, 0, OperatorType.Multiply, undefined);
        importFromNode.isCython = true;
        const pxdImport = {
            nameNode: importFromNode.module,
            leadingDots: importFromNode.module.leadingDots,
            nameParts: importFromNode.module.nameParts.map((p) => p.value),
            importedSymbols: importFromNode.imports.map((imp) => imp.name.value),
            isCython: true,
            cythonExt: 'pxd',
        };
        const statements = StatementListNode.create(fromToken);
        statements.statements.push(importFromNode);
        importFromNode.parent = statements;

        // Invalidate starts
        importFromNode.start = -1;
        importFromNode.module.start = -1;
        importFromNode.module.nameParts.forEach((n) => {
            n.start = -1;
        });
        importFromNode.imports.forEach((n) => {
            n.start = -1;
            n.name.start = -1;
        });
        statements.start = -1;

        return { moduleNode: importFromNode, statements: statements, import: pxdImport };
    }

    // Parse operator function name: i.e. `operator++()`, `operator bool()`
    private _parseOperatorFunctionName(): { name: NameNode | undefined; suffix: string } {
        const index = this._tokenIndex;
        if (this._peekKeywordType() === KeywordType.Operator) {
            const opToken = this._getTokenIfIdentifier();
            assertDefined(opToken);
            const maybeName = this._getTokenIfIdentifier();
            if (maybeName) {
                // Type Cast operator
                // TODO: Do we constrain the type? Only 'bool' appears to be valid.
                const value = `${opToken.value} ${maybeName.value}`;
                const range = TextRange.create(opToken.start, opToken.length);
                TextRange.extend(range, TextRange.create(maybeName.start, maybeName.length));
                const nameToken = IdentifierToken.create(range.start, range.length, value, undefined);
                return { name: NameNode.create(nameToken), suffix: maybeName.value };
            }
            const token0 = this._peekToken(0);
            const token1 = this._peekToken(1);
            let suffix: string | undefined = undefined;
            if (token0.type === TokenType.OpenBracket && token1.type === TokenType.CloseBracket) {
                suffix = CppOperatorSymbol.Index;
                this._getNextToken();
            } else if (token0.type === TokenType.OpenParenthesis && token1.type === TokenType.CloseParenthesis) {
                suffix = CppOperatorSymbol.Call;
                this._getNextToken();
            } else if (token0.type === TokenType.Comma) {
                suffix = CppOperatorSymbol.Comma;
            } else if (token0.type === TokenType.Operator) {
                const op0 = token0 as OperatorToken;
                const op1 = token1.type === TokenType.Operator ? (token1 as OperatorToken) : undefined;
                switch (op0.operatorType) {
                    case OperatorType.Add:
                        suffix =
                            op1?.operatorType === OperatorType.Add ? CppOperatorSymbol.IAdd : CppOperatorSymbol.Add;
                        break;
                    case OperatorType.Subtract:
                        suffix =
                            op1?.operatorType === OperatorType.Subtract
                                ? CppOperatorSymbol.ISubtract
                                : CppOperatorSymbol.Subtract;
                        break;
                    case OperatorType.Multiply:
                        suffix = CppOperatorSymbol.Multiply;
                        break;
                    case OperatorType.Divide:
                        suffix = CppOperatorSymbol.Divide;
                        break;
                    case OperatorType.Mod:
                        suffix = CppOperatorSymbol.Mod;
                        break;
                    case OperatorType.BitwiseInvert:
                        suffix = CppOperatorSymbol.BitwiseInvert;
                        break;
                    case OperatorType.BitwiseOr:
                        suffix = CppOperatorSymbol.BitwiseOr;
                        break;
                    case OperatorType.BitwiseAnd:
                        suffix = CppOperatorSymbol.BitwiseAnd;
                        break;
                    case OperatorType.BitwiseXor:
                        suffix = CppOperatorSymbol.BitwiseXor;
                        break;
                    case OperatorType.RightShift:
                        suffix = CppOperatorSymbol.RightShift;
                        break;
                    case OperatorType.LeftShift:
                        suffix = CppOperatorSymbol.LeftShift;
                        break;
                    case OperatorType.Equals:
                        suffix = CppOperatorSymbol.Equals;
                        break;
                    case OperatorType.NotEquals:
                        suffix = CppOperatorSymbol.NotEquals;
                        break;
                    case OperatorType.GreaterThanOrEqual:
                        suffix = CppOperatorSymbol.GreaterThanOrEqual;
                        break;
                    case OperatorType.GreaterThan:
                        suffix = CppOperatorSymbol.GreaterThan;
                        break;
                    case OperatorType.LessThanOrEqual:
                        suffix = CppOperatorSymbol.LessThanOrEqual;
                        break;
                    case OperatorType.LessThan:
                        suffix = CppOperatorSymbol.LessThan;
                        break;
                    case OperatorType.Negate:
                        suffix = CppOperatorSymbol.Negate;
                        break;
                    case OperatorType.Assign:
                        suffix = CppOperatorSymbol.Assign;
                        break;
                    default:
                        suffix = undefined;
                }
                if (suffix === CppOperatorSymbol.IAdd || suffix === CppOperatorSymbol.ISubtract) {
                    this._getNextToken();
                }
            }
            if (suffix) {
                // Any suffixes with two tokens should already have one token consumed
                // This should be the last token
                const lastSuffixToken = this._getNextToken();
                // Next tokens should be '[' or '('. If not we'll assume this is not an operator method
                const nextToken = this._peekToken();
                if (nextToken.type === TokenType.OpenBracket || nextToken.type === TokenType.OpenParenthesis) {
                    const value = `${opToken.value}${suffix}`;
                    const range = TextRange.create(opToken.start, opToken.length);
                    TextRange.extend(range, TextRange.create(lastSuffixToken.start, lastSuffixToken.length));
                    const nameToken = IdentifierToken.create(range.start, range.length, value, undefined);
                    return { name: NameNode.create(nameToken), suffix: suffix };
                }
            }
        }
        this._tokenIndex = index;
        return { name: undefined, suffix: '' };
    }
}
