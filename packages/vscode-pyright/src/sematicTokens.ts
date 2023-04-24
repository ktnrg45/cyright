import {
    DocumentSemanticTokensProvider,
    SemanticTokensLegend,
    ExtensionContext,
    CancellationToken,
    SemanticTokens,
    TextDocument,
    languages,
} from "vscode";

import { LanguageClient, SemanticTokensParams, TextDocumentIdentifier } from "vscode-languageclient/node";
import { tokenTypesLegend, tokenModifiersLegend } from "pyright-internal/languageService/semanticTokens";


const legend = new SemanticTokensLegend(tokenTypesLegend, tokenModifiersLegend);

class SemanticTokenProvider implements DocumentSemanticTokensProvider {
    private _client: LanguageClient;

    constructor(client: LanguageClient) {
        this._client = client;
    }

    async provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): Promise<SemanticTokens> {
        const identifier: TextDocumentIdentifier = {
            uri: document.uri.toString(),
        };
        const params: SemanticTokensParams = {
            textDocument: identifier,
        };
        return await this._client.sendRequest(
            'textDocument/semanticTokens/full',
            params,
            token,
        );

    }

}

export function registerSemanticTokensProvider(context: ExtensionContext, client: LanguageClient) {
    context.subscriptions.push(
        languages.registerDocumentSemanticTokensProvider(
            {language: 'cython'}, new SemanticTokenProvider(client), legend)
        );
}