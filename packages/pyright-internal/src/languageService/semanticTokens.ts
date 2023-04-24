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
} from "../parser/parseNodes";
import { ParseResults } from "../parser/parser";
import { AnalyzerFileInfo } from "../analyzer/analyzerFileInfo";
import { getFileInfo } from "../analyzer/analyzerNodeInfo";
import { convertOffsetToPosition } from "../common/positionUtils";
import { AnalyzerService } from "../analyzer/service";
import { TypeCategory } from "../analyzer/types";


export const tokenTypesLegend = [
    'class', 'variable', 'namespace', 'function', 'parameter',
    //
    'comment', 'string', 'keyword', 'number', 'regexp', 'operator',
    'type', 'struct', 'interface', 'enum', 'typeParameter',
    'method', 'decorator', 'macro', 'property', 'label'
];

export const tokenModifiersLegend = [
    'declaration',
    //
    'documentation', 'readonly', 'static', 'abstract', 'deprecated',
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
    for (let i = 0; i < tokenModifiersLegend.length; i++) {
        const tokenModifier = modifiers[i];
        const index = tokenModifiersLegend.indexOf(tokenModifier);
        if (index >= 0) {
            result = result | (1 << index);
        } else {
            result = result | (1 << tokenModifiersLegend.length + 2);
        }
    }
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
        this._results?.parseTree.statements.forEach(node => {
            this.parseNode(node);
        });
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

    pushNode(node: ParseNode, typeName?: string, modifierName?: string | readonly string[]) {
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

    pushVar(node: ParseNode, tokenType = "variable") {
        this.pushNode(node, tokenType, "declaration");
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
        if (node.nodeType === ParseNodeType.Assignment) {
            // Not sure of impact of adding this to `isExpressionNode()`
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
            default:
                console.log(node);
                break;
        }
    }

    parseStatements(statements: StatementNode[] | ParseNode[]) {
        statements.forEach(item => {
            this.parseNode(item);
        });
    }

    parseSuite(node: SuiteNode) {
        this.parseStatements(node.statements);
    }

    parseExpression(node?: ExpressionNode) {
        if (!node) {
            return;
        }
        switch (node.nodeType) {
            case ParseNodeType.UnaryOperation:
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
                this.pushVar(node.name);
                this.parseExpression(node.rightExpression);
                break;
            case ParseNodeType.TypeAnnotation:
                this.parseTypeAnnotation(node);
                break;
            case ParseNodeType.Await:
                break;
            case ParseNodeType.Ternary:
                break;
            case ParseNodeType.Unpack:
                break;
            case ParseNodeType.Tuple:
                break;
            case ParseNodeType.Call:
                break;
            case ParseNodeType.ListComprehension:
                break;
            case ParseNodeType.Index:
                break;
            case ParseNodeType.Slice:
                break;
            case ParseNodeType.Yield:
                break;
            case ParseNodeType.YieldFrom:
                break;
            case ParseNodeType.MemberAccess:
                break;
            case ParseNodeType.Lambda:
                break;
            case ParseNodeType.Name:
                this.pushVar(node);
                break;
            case ParseNodeType.Constant:
                break;
            case ParseNodeType.Ellipsis:
                break;
            case ParseNodeType.Number:
                break;
            case ParseNodeType.String:
                break;
            case ParseNodeType.FormatString:
                break;
            case ParseNodeType.StringList:
                break;
            case ParseNodeType.Dictionary:
                break;
            case ParseNodeType.List:
                break;
            case ParseNodeType.Set:
                break;
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
        node.parameters.forEach(item => this.pushNameAndAnnotation(item.name, item.typeAnnotation, "parameter"));

        if (!returnTypeBefore) {
            this.pushType(node.returnTypeAnnotation);
        }

        this.parseSuite(node.suite);
    }
}



