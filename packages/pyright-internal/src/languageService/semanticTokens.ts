import { SemanticTokensBuilder } from "vscode-languageserver"
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
} from "../parser/parseNodes";
import { ParseResults } from "../parser/parser";
import { AnalyzerFileInfo } from "../analyzer/analyzerFileInfo";
import { getFileInfo } from "../analyzer/analyzerNodeInfo";
import { convertOffsetToPosition } from "../common/positionUtils";
import { AnalyzerService } from "../analyzer/service";
import { TypeCategory } from "../analyzer/types";


export const tokenTypesLegend = [
    'class', 'variable', 'namespace',
    //
    'comment', 'string', 'keyword', 'number', 'regexp', 'operator',
    'type', 'struct', 'interface', 'enum', 'typeParameter', 'function',
    'method', 'decorator', 'macro', 'parameter', 'property', 'label'
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

    async provideSemanticTokensFull(filePath: string, workspace: WorkspaceServiceInstance) {
        const results = workspace.serviceInstance.getParseResult(filePath);
        if (results) {
            const fileInfo: AnalyzerFileInfo = getFileInfo(results.parseTree);
            let builder = new CythonSemanticTokensBuilder(workspace.serviceInstance, fileInfo, results);
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


class CythonSemanticTokensBuilder extends SemanticTokensBuilder {
    private _service: AnalyzerService;
    private _results: ParseResults | undefined;
    private _fileInfo: AnalyzerFileInfo;

    constructor(service: AnalyzerService, fileInfo: AnalyzerFileInfo, results: ParseResults) {
        super();
        this._service = service;
        this._results = results
        this._fileInfo = fileInfo;
    }

    public parseResults() {
        this._results?.parseTree.statements.forEach(node => {
            this.parseNode(node);
        });
    }

    getPosition(node: ParseNode) {
        return convertOffsetToPosition(node.start, this._fileInfo.lines);
    }

    getType(node: ExpressionNode) {
        return this._service.getEvaluator()?.getType(node);
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

    pushType(node: ExpressionNode) {
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
            }
        }
    }

    pushVar(node: ParseNode) {
        this.pushNode(node, "variable", "declaration");
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

    parseNode(node: ParseNode) {
        switch (node.nodeType) {
            case ParseNodeType.Import:
            case ParseNodeType.ImportFrom:
                this.parseImport(node);
                break;
            case ParseNodeType.StatementList:
                node.statements.forEach(item => {
                    this.parseNode(item);
                });
                break;
            case ParseNodeType.Name:
                this.pushVar(node);
                break;
            case ParseNodeType.Class:
                this.pushType(node.name);
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
            case ParseNodeType.TypeAlias:
                this.pushType(node.expression);
                this.pushType(node.name);
                break;
            default:
                console.log(node);
                break;
        }
    }

    parseExpression(node: ExpressionNode) {
        switch (node.nodeType) {
            case ParseNodeType.TypeAnnotation:
                this.parseTypeAnnotation(node);
                break;
            case ParseNodeType.Name:
                this.parseNode(node);
                break;
        }
    }

    parseTypeAnnotation(node: TypeAnnotationNode) {
        this.pushType(node.typeAnnotation);
        this.parseNode(node.valueExpression);
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
}



