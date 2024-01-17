import {
    CancellationToken,
    DocumentSemanticTokensProvider,
    ExtensionContext,
    languages,
    SemanticTokens,
    SemanticTokensLegend,
    TextDocument,
} from 'vscode';
import { LanguageClient, SemanticTokensParams, TextDocumentIdentifier } from 'vscode-languageclient/node';

import { tokenModifiersLegend, tokenTypesLegend } from 'pyright-internal/languageService/semanticTokens';

export class SemanticTokenProvider implements DocumentSemanticTokensProvider {
    private _client: LanguageClient;
    private _context: ExtensionContext;
    private _legend: SemanticTokensLegend;

    constructor(client: LanguageClient, context: ExtensionContext) {
        this._client = client;
        this._context = context;
        this._legend = new SemanticTokensLegend(tokenTypesLegend, tokenModifiersLegend);
        this.registerSemanticTokensProvider();
    }

    registerSemanticTokensProvider() {
        this._context.subscriptions.push(
            languages.registerDocumentSemanticTokensProvider({ language: 'cython' }, this, this._legend)
        );
    }

    async provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): Promise<SemanticTokens> {
        const identifier: TextDocumentIdentifier = {
            uri: document.uri.toString(),
        };
        const params: SemanticTokensParams = {
            textDocument: identifier,
        };
        return await this._client.sendRequest('textDocument/semanticTokens/full', params, token);
    }
}
