import { SemanticTokensBuilder, SemanticTokensParams } from "vscode-languageserver"
import { PyrightServer } from "../server";
import { WorkspaceServiceInstance } from "../languageServerBase";
import {
    ParseNodeType,
    ParseNode,
    NameNode,
    AssignmentNode,
    ExpressionNode,
    TypeAnnotationNode,
} from "../parser/parseNodes";
import { ParseResults } from "../parser/parser";
import { AnalyzerFileInfo } from "../analyzer/analyzerFileInfo";
import { getFileInfo } from "../analyzer/analyzerNodeInfo";
import { convertOffsetToPosition } from "../common/positionUtils";
import { AnalyzerService } from "../analyzer/service";
import { TypeCategory } from "../analyzer/types";


export const tokenTypesLegend = [
    'comment', 'string', 'keyword', 'number', 'regexp', 'operator', 'namespace',
    'type', 'struct', 'class', 'interface', 'enum', 'typeParameter', 'function',
    'method', 'decorator', 'macro', 'variable', 'parameter', 'property', 'label'
];

export const tokenModifiersLegend = [
    'declaration', 'documentation', 'readonly', 'static', 'abstract', 'deprecated',
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


function encodeType(typeName: string | undefined) {
    return (typeName ? tokenTypesLegend.indexOf(typeName) : tokenTypesLegend.length + 2);
}

function encodeModifier(modifierName: string | undefined) {
    return (modifierName ? tokenModifiersLegend.indexOf(modifierName) : -1);
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

    pushNode(node: ParseNode, typeName: string | undefined, modifierName?: string) {
        const pos = this.getPosition(node);
        this.push(
            pos.line,
            pos.character,
            node.length,
            encodeType(typeName),
            encodeModifier(modifierName),
        );
    }

    pushType(node: ExpressionNode) {
        const type = this._service.getEvaluator()?.getType(node);
        if (type) {
            if (type.category === TypeCategory.Class) {
                this.pushNode(node, "class");
            }
        }
    }

    pushVar(node: ParseNode) {
        this.pushNode(node, "variable");
    }

    parseNode(node: ParseNode) {
        switch (node.nodeType) {
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
            // default:
            //     this.pushType(node);
            //     break;
        }
    }

    parseTypeAnnotation(node: TypeAnnotationNode) {
        this.pushType(node.typeAnnotation);
        this.parseNode(node.valueExpression);
    }
}



