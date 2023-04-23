import { SemanticTokensBuilder, SemanticTokensParams } from "vscode-languageserver"
import { PyrightServer } from "../server";
import { WorkspaceServiceInstance } from "../languageServerBase";
import {
    ParseNodeType,
    ParseNode,
    NameNode,
} from "../parser/parseNodes";
import { ParseResults } from "../parser/parser";
import { AnalyzerFileInfo } from "../analyzer/analyzerFileInfo";
import { getFileInfo } from "../analyzer/analyzerNodeInfo";
import { convertOffsetToPosition } from "../common/positionUtils";


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
            let builder = new CythonSemanticTokensBuilder(results);
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
    private _results: ParseResults;
    private _fileInfo: AnalyzerFileInfo;

    constructor(results: ParseResults) {
        super();
        this._results = results
        this._fileInfo = getFileInfo(results.parseTree);
    }

    public parseResults() {
        this._results.parseTree.statements.forEach(node => {
            this.buildNode(node);
        });
    }

    getPosition(node: ParseNode) {
        return convertOffsetToPosition(node.start, this._fileInfo.lines);
    }

    pushNode(node: NameNode, typeName: string | undefined, modifierName: string | undefined) {
        const pos = this.getPosition(node);
        this.push(
            pos.line,
            pos.character,
            node.length,
            encodeType(typeName),
            encodeModifier(modifierName),
        );
    }

    buildNode(node: ParseNode) {
        switch (node.nodeType) {
            case ParseNodeType.StatementList:
                node.statements.forEach(item => {
                    this.buildNode(item);
                });
                break;
            case ParseNodeType.Class:
                this.pushNode(node.name, "class", undefined);
                break;

            default:
                break;
        }
    }
}



