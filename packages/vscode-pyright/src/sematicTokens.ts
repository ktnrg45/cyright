import {
    DocumentSemanticTokensProvider,
    SemanticTokensLegend,
    ExtensionContext,
    CancellationToken,
    ProviderResult,
    SemanticTokens,
    SemanticTokensBuilder,
    TextDocument,
    languages,
} from "vscode";

import { LanguageClient, SemanticTokensParams, TextDocumentIdentifier } from "vscode-languageclient/node";

const tokenTypes = new Map<string, number>();
const tokenModifiers = new Map<string, number>();

const legend = (function () {
	const tokenTypesLegend = [
		'comment', 'string', 'keyword', 'number', 'regexp', 'operator', 'namespace',
		'type', 'struct', 'class', 'interface', 'enum', 'typeParameter', 'function',
		'method', 'decorator', 'macro', 'variable', 'parameter', 'property', 'label'
	];
	tokenTypesLegend.forEach((tokenType, index) => tokenTypes.set(tokenType, index));

	const tokenModifiersLegend = [
		'declaration', 'documentation', 'readonly', 'static', 'abstract', 'deprecated',
		'modification', 'async'
	];
	tokenModifiersLegend.forEach((tokenModifier, index) => tokenModifiers.set(tokenModifier, index));

	return new SemanticTokensLegend(tokenTypesLegend, tokenModifiersLegend);
})();

class SemanticTokenProvider implements DocumentSemanticTokensProvider {
    private _client: LanguageClient;

    constructor(client: LanguageClient) {
        this._client = client;
    }

    async provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): Promise<SemanticTokens> {
        const builder = new SemanticTokensBuilder();
        const identifier: TextDocumentIdentifier = {
            uri: document.uri.toString(),
        };
        const params: SemanticTokensParams = {
            textDocument: identifier,
        };
        let req = await this._client.sendRequest(
            'textDocument/semanticTokens/full',
            params,
            token,
        );
        return builder.build();

    }

}

export function registerSemanticTokensProvider(context: ExtensionContext, client: LanguageClient) {
    context.subscriptions.push(
        languages.registerDocumentSemanticTokensProvider(
            { language: 'cython'}, new SemanticTokenProvider(client), legend)
        );
}