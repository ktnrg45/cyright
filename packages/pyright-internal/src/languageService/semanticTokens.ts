import { SemanticTokensParams } from "vscode-languageserver"
import { PyrightServer } from "../server";
import { WorkspaceServiceInstance } from "../languageServerBase";

// TODO: Is there an interface for this?
export class CythonSemanticTokenProvider {
    private _ls: PyrightServer;

    constructor(ls: PyrightServer) {
        this._ls = ls;
    }

    async provideSemanticTokens(filePath: string, workspace: WorkspaceServiceInstance) {
        let results = workspace.serviceInstance.getParseResult(filePath);
        console.log(filePath);
    }
}