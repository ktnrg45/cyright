import { CancellationToken, SemanticTokensBuilder } from "vscode-languageserver"
import { PyrightServer } from "../server";
import { WorkspaceServiceInstance } from "../languageServerBase";
import {
    ParseNodeType,
    ParseNode,
    ExpressionNode,
    TypeAnnotationNode,
    ImportNode,
    ImportFromNode,
    ModuleNameNode,
    FunctionNode,
    SuiteNode,
    StatementNode,
    NameNode,
    isExpressionNode,
    ParameterNode,
} from "../parser/parseNodes";
import { ParseResults } from "../parser/parser";
import { AnalyzerFileInfo } from "../analyzer/analyzerFileInfo";
import { getFileInfo } from "../analyzer/analyzerNodeInfo";
import { convertOffsetToPosition } from "../common/positionUtils";
import { AnalyzerService } from "../analyzer/service";
import { TypeCategory, isClass, isFunction, isModule } from "../analyzer/types";
import { DeclarationType } from "../analyzer/declaration";


export const tokenTypesLegend = [
    'class', 'variable', 'namespace', 'function', 'parameter', 'enumMember',
    //
    'comment', 'string', 'keyword', 'number', 'regexp', 'operator',
    'type', 'struct', 'interface', 'enum', 'typeParameter',
    'method', 'decorator', 'macro', 'property', 'label'
];

export const tokenModifiersLegend = [
    'declaration', 'readonly',
    //
    'documentation', 'static', 'abstract', 'deprecated',
    'modification', 'async'
];

export class CythonSemanticTokenProvider {
    private _ls: PyrightServer;

    constructor(ls: PyrightServer) {
        this._ls = ls;
    }

    async provideSemanticTokensFull(filePath: string, workspace: WorkspaceServiceInstance, token: CancellationToken) {
        const results = workspace.serviceInstance.getParseResult(filePath);
        if (results) {
            const fileInfo: AnalyzerFileInfo = getFileInfo(results.parseTree);
            let builder = new CythonSemanticTokensBuilder(workspace.serviceInstance, fileInfo, results, token);
            builder.parseResults();
            return builder.build();
        }
        let builder = new SemanticTokensBuilder();
        return builder.build();

    }
}


function encodeType(typeName?: string) {
    return (typeName ? tokenTypesLegend.indexOf(typeName) : -1);
}

function encodeModifiers(tokenModifiers?: string | readonly string[]) {
    let result = 0;
    const modifiers = (tokenModifiers && !Array.isArray(tokenModifiers) ? [tokenModifiers] : tokenModifiers);
    if (!modifiers || modifiers.length === 0 || !Array.isArray(modifiers)) {
        return result;
    }
    modifiers.forEach(modifier => {
        const index = tokenModifiersLegend.indexOf(modifier);
        if (index >= 0) {
            result = result | (1 << index);
        } else {
            result = result | (1 << tokenModifiersLegend.length + 2);
        }
    });
    return result;
}

function annotationBeforeName(name?: ExpressionNode, typeAnnotation?: ExpressionNode) {
    if (typeAnnotation && name && typeAnnotation.start < name.start) {
        return true;
    }
    return false;
}


class CythonSemanticTokensBuilder extends SemanticTokensBuilder {
    private _service: AnalyzerService;
    private _results: ParseResults | undefined;
    private _fileInfo: AnalyzerFileInfo;
    private _token: CancellationToken;
    private _finished: boolean;

    constructor(service: AnalyzerService, fileInfo: AnalyzerFileInfo, results: ParseResults, token: CancellationToken) {
        super();
        this._service = service;
        this._results = results
        this._fileInfo = fileInfo;
        this._token = token;
        this._finished = false;
    }

    public parseResults() {
        this._results?.parseTree.statements.forEach(node => this.parseNode(node));
        this._finished = true;
    }

    getPosition(node: ParseNode) {
        return convertOffsetToPosition(node.start, this._fileInfo.lines);
    }

    getType(node: ExpressionNode) {
        return this._service.getEvaluator()?.getType(node);
    }

    getTypeOfFunction(node: FunctionNode) {
        return this._service.getEvaluator()?.getTypeOfFunction(node);
    }

    getTokenTypeForName(node: NameNode) {
        let tokenType = "variable";
        let evaluator = this._service.getEvaluator()
        const declarations = evaluator?.getDeclarationsForNameNode(node);
        const type = this.getType(node);
        if (type && isModule(type)) {
            tokenType = "namespace";
            return tokenType;
        }
        if (declarations && declarations.length > 0) {
            let primaryDeclaration = declarations[0];

            if (primaryDeclaration.type === DeclarationType.Alias && declarations.length > 1) {
                primaryDeclaration = declarations[1];
            }
            const resolvedDecl = evaluator?.resolveAliasDeclaration(primaryDeclaration, /* resolveLocalNames */ true);
            switch (resolvedDecl?.type) {
                case DeclarationType.Class:
                    tokenType = "class";
                    break;
                case DeclarationType.Function:
                    tokenType = "function";
                    break;
                case DeclarationType.TypeAlias:
                    if (type && isClass(type)) {
                        tokenType = "class";
                    } else if (type && isFunction(type)) {
                        tokenType = "function";
                    }
                    break;
            }
        }
        return tokenType;
    }

    pushNode(node: ParseNode, typeName?: string, modifierName?: string | string[]) {
        if (node.nodeType === ParseNodeType.Name && node.value === "NULL") {
            // TODO: implement as a KeywordToken
            return;
        }
        const pos = this.getPosition(node);
        const type = encodeType(typeName);
        const modifiers = encodeModifiers(modifierName);
        this.push(
            pos.line,
            pos.character,
            node.length,
            type,
            modifiers,
        );
    }

    pushType(node?: ExpressionNode) {
        if (!node) {
            return;
        }
        if (node.nodeType === ParseNodeType.MemberAccess) {
            this.pushType(node.leftExpression);
            this.pushType(node.memberName);
            return;
        }
        const type = this.getType(node);
        if (type) {
            switch (type.category) {
                case TypeCategory.Class:
                    this.pushNode(node, "class", "declaration");
                    break;
                case TypeCategory.Module:
                    this.pushModule(node);
                    break;
                case TypeCategory.Function:
                    this.pushNode(node, "function");
            }
        }
    }

    pushVar(node: ParseNode, tokenType = "variable", modifiers: string | string[] = "declaration") {
        this.pushNode(node, tokenType, modifiers);
    }

    pushModule(node: ModuleNameNode | ExpressionNode) {
        if (node.nodeType === ParseNodeType.ModuleName) {
            node.nameParts.forEach(item => {
                this.pushNode(item, "namespace");
            });
        } else {
            this.pushNode(node, "namespace");
        }
    }

    pushNameAndAnnotation(name?: ExpressionNode, typeAnnotation?: ExpressionNode, nameTokenType = "variable") {
        if (typeAnnotation && name) {
            // We have to build tokens in order.
            // If annotation before name, this is a cython declaration:
            // `cdef type name`
            if (annotationBeforeName(name, typeAnnotation)) {
                this.pushType(typeAnnotation);
                this.pushVar(name, nameTokenType);
            } else {
                this.pushVar(name, nameTokenType);
                this.pushType(typeAnnotation);
            }
        } else if (name) {
            this.parseExpression(name);
        }
    }

    parseNode(node: ParseNode) {
        if (this._token.isCancellationRequested) {
            return;
        }
        if (node.nodeType === ParseNodeType.Assignment || node.nodeType === ParseNodeType.AugmentedAssignment) {
            // These don't return true for `isExpressionNode()`
            this.parseExpression(node);
            return;
        }
        if (isExpressionNode(node)) {
            this.parseExpression(node);
            return;
        }

        switch (node.nodeType) {
            case ParseNodeType.Import:
            case ParseNodeType.ImportFrom:
                this.parseImport(node);
                break;
            case ParseNodeType.StatementList:
                this.parseStatements(node.statements);
                break;
            case ParseNodeType.With:
                node.withItems.forEach(item => {
                    this.parseExpression(item.expression);
                    this.parseExpression(item.target);
                });
                this.parseSuite(node.suite);
                break;
            case ParseNodeType.While:
                this.parseExpression(node.testExpression);
                this.parseSuite(node.whileSuite);
                this.parseSuite(node.elseSuite);
                break;
            case ParseNodeType.If:
                this.parseExpression(node.testExpression);
                this.parseSuite(node.ifSuite);
                if (node.elseSuite?.nodeType === ParseNodeType.If) {
                    this.parseNode(node.elseSuite);
                } else {
                    this.parseSuite(node.elseSuite);
                }
                break;
            case ParseNodeType.For:
                this.parseExpression(node.targetExpression);
                this.parseExpression(node.iterableExpression);
                this.parseSuite(node.forSuite);
                this.parseSuite(node.elseSuite);
                break;
            case ParseNodeType.Class:
                this.pushType(node.name);
                this.parseSuite(node.suite);
                break;
            case ParseNodeType.TypeAlias:
                this.pushType(node.expression);
                this.pushType(node.name);
                break;
            case ParseNodeType.Function:
                this.parseFunction(node);
                break;
            case ParseNodeType.Return:
                this.parseExpression(node.returnExpression);
                break;
            case ParseNodeType.Argument:
                this.parseExpression(node.valueExpression);
                break;
            case ParseNodeType.ListComprehensionFor:
                this.parseExpression(node.targetExpression);
                this.parseExpression(node.iterableExpression);
                break;
            case ParseNodeType.ListComprehensionIf:
                this.parseExpression(node.testExpression);
                break;
            case ParseNodeType.Parameter:
                this.parseParameter(node);
                break;
            case ParseNodeType.DictionaryExpandEntry:
                this.parseExpression(node.expandExpression);
                break;
            case ParseNodeType.DictionaryKeyEntry:
                this.parseExpression(node.keyExpression);
                this.parseExpression(node.valueExpression);
                break;
            default:
                break;
        }
    }

    parseExpression(node?: ExpressionNode) {
        if (!node) {
            return;
        }
        switch (node.nodeType) {
            case ParseNodeType.UnaryOperation:
                this.parseExpression(node.expression);
                break;
            case ParseNodeType.BinaryOperation:
                this.parseExpression(node.leftExpression);
                this.parseExpression(node.rightExpression);
                break;
            case ParseNodeType.Assignment:
                this.parseExpression(node.leftExpression);
                this.parseExpression(node.rightExpression);
                break;
            case ParseNodeType.AssignmentExpression:
                this.parseExpression(node.name);
                this.parseExpression(node.rightExpression);
                break;
            case ParseNodeType.AugmentedAssignment:
                // TODO: Is dest before or after left?
                this.parseExpression(node.destExpression);
                this.parseExpression(node.leftExpression);
                this.parseExpression(node.rightExpression);
                break;
            case ParseNodeType.TypeAnnotation:
                this.parseTypeAnnotation(node);
                break;
            case ParseNodeType.Await:
                this.parseExpression(node.expression);
                break;
            case ParseNodeType.Ternary:
                this.parseExpression(node.ifExpression);
                this.parseExpression(node.testExpression);
                this.parseExpression(node.elseExpression);
                break;
            case ParseNodeType.Unpack:
                this.parseExpression(node.expression);
                break;
            case ParseNodeType.Tuple:
                node.expressions.forEach(item => this.parseExpression(item));
                break;
            case ParseNodeType.Call:
                this.parseExpression(node.leftExpression);
                node.arguments.forEach(item => this.parseNode(item));
                break;
            case ParseNodeType.ListComprehension:
                this.parseNode(node.expression);
                node.forIfNodes.forEach(item => this.parseNode(item));
                break;
            case ParseNodeType.Index:
                this.parseExpression(node.baseExpression);
                node.items.forEach(item => this.parseNode(item));
                break;
            case ParseNodeType.Slice:
                this.parseExpression(node.startValue);
                this.parseExpression(node.endValue);
                this.parseExpression(node.stepValue);
                break;
            case ParseNodeType.Yield:
                this.parseExpression(node.expression);
                break;
            case ParseNodeType.YieldFrom:
                this.parseExpression(node.expression);
                break;
            case ParseNodeType.MemberAccess:
                this.parseExpression(node.leftExpression);
                this.parseExpression(node.memberName);
                break;
            case ParseNodeType.Lambda:
                node.parameters.forEach(item => this.parseNode(item));
                this.parseExpression(node.expression);
                break;
            case ParseNodeType.Name:
                this.pushVar(node, this.getTokenTypeForName(node));
                break;
            case ParseNodeType.Dictionary:
                node.entries.forEach(item => this.parseNode(item));
                break;
            case ParseNodeType.List:
                node.entries.forEach(item => this.parseExpression(item));
                break;
            case ParseNodeType.Set:
                node.entries.forEach(item => this.parseExpression(item));
                break;
            case ParseNodeType.StringList:
                node.strings.forEach(item => this.parseExpression(item));
                this.parseExpression(node.typeAnnotation);
                break;
            case ParseNodeType.FormatString:
                node.expressions.forEach(item => this.parseExpression(item));
                break;
            case ParseNodeType.Constant:
                break;
            case ParseNodeType.Ellipsis:
            case ParseNodeType.Number:
            case ParseNodeType.String:
                break;
        }
    }

    parseStatements(statements: StatementNode[] | ParseNode[]) {
        statements.forEach(item => this.parseNode(item));
    }

    parseSuite(node?: SuiteNode) {
        if (node) {
            this.parseStatements(node.statements);
        }
    }

    parseTypeAnnotation(node: TypeAnnotationNode) {
        this.pushNameAndAnnotation(node.valueExpression, node.typeAnnotation);
    }

    parseImport(node: ImportNode | ImportFromNode) {
        switch (node.nodeType) {
            case ParseNodeType.Import:
                node.list.forEach(item => {
                    this.pushModule(item.module);
                    if (item.alias) {
                        this.pushModule(item.alias);
                    }
                });
                break;
            case ParseNodeType.ImportFrom:
                this.pushModule(node.module);
                node.imports.forEach(item => {
                    this.pushType(item.name);
                    if (item.alias) {
                        this.pushType(item.alias);
                    }
                });
                break;
        }
    }

    parseFunction(node: FunctionNode) {
        const returnTypeBefore = annotationBeforeName(node.name, node.returnTypeAnnotation);

        if (returnTypeBefore) {
            // cdef type func()
            this.pushType(node.returnTypeAnnotation);
        }
        this.pushNode(node.name, "function", "declaration");
        node.parameters.forEach(item => this.parseParameter(item));

        if (!returnTypeBefore) {
            this.pushType(node.returnTypeAnnotation);
        }

        this.parseSuite(node.suite);
    }

    parseParameter(node: ParameterNode) {
        this.pushNameAndAnnotation(node.name, node.typeAnnotation, "parameter")
        this.parseExpression(node.defaultValue);
    }
}



