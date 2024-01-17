import { ExtensionContext, OutputChannel } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

import { CythonCompiler } from './compiler';
import { Installer } from './installer';
import { SemanticTokenProvider } from './semanticTokens';
import { StatusBar } from './statusBar';

// Additional misc service objects for Cython
export class CythonServices {
    readonly client: LanguageClient;
    readonly context: ExtensionContext;
    readonly statusBar: StatusBar;
    readonly compiler: CythonCompiler;
    readonly installer: Installer;
    readonly semanticTokensProvider: SemanticTokenProvider;

    constructor(client: LanguageClient, context: ExtensionContext) {
        this.client = client;
        this.context = context;
        this.statusBar = new StatusBar(context);
        this.compiler = new CythonCompiler(context);
        this.installer = new Installer(context, client);
        this.semanticTokensProvider = new SemanticTokenProvider(client, context);
    }

    updatePythonPath(outputChannel: OutputChannel, path?: string) {
        this.statusBar.update(path);
        this.compiler.setPythonPath(path);
        this.installer.onPythonPathUpdate(path);
    }
}
