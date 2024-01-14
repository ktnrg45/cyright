import { ExtensionContext } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

import { CythonCompiler } from './compiler';
import { StatusBar } from './statusBar';

// Additional misc service objects for Cython
export class CythonServices {
    readonly client: LanguageClient;
    readonly context: ExtensionContext;
    readonly statusBar: StatusBar;
    readonly compiler: CythonCompiler;

    constructor(client: LanguageClient, context: ExtensionContext) {
        this.client = client;
        this.context = context;
        this.statusBar = new StatusBar(context);
        this.compiler = new CythonCompiler(context);
    }

    updatePythonPath(path?: string) {
        this.statusBar.update(path);
        this.compiler.setPythonPath(path);
    }
}
