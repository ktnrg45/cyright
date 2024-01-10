import {
    ExtensionContext,
    StatusBarAlignment,
    StatusBarItem,
    TextEditor,
    TextEditorSelectionChangeEvent,
    ThemeColor,
    window,
} from 'vscode';

import { FullAccessHost } from 'pyright-internal/common/fullAccessHost';
import { HostKind } from 'pyright-internal/common/host';
import { getDirectoryPath, getPathComponents } from 'pyright-internal/common/pathUtils';
import { createFromRealFileSystem } from 'pyright-internal/common/realFileSystem';

const PRIORTY = 100.09999;
const BUTTON_LABEL = 'Select Interpreter';
const COMMAND = 'python.setInterpreter';

export class StatusBar {
    readonly statusBar: StatusBarItem;
    readonly context: ExtensionContext;
    readonly host: FullAccessHost;

    constructor(context: ExtensionContext) {
        this.context = context;
        this.host = FullAccessHost.createHost(
            HostKind.FullAccess,
            createFromRealFileSystem(undefined, undefined)
        ) as FullAccessHost;
        this.statusBar = window.createStatusBarItem(StatusBarAlignment.Right, PRIORTY);
        this.statusBar.tooltip = '';
        this.statusBar.color = '';
        this.statusBar.command = COMMAND;
        this.context.subscriptions.push(this.statusBar);
        this.context.subscriptions.push(window.onDidChangeActiveTextEditor(this._onDidChangeActiveTextEditor, this));
        // this.context.subscriptions.push(window.onDidChangeTextEditorSelection(this.onDidChangeActiveTextEditor, this));
        this.show();
    }

    show() {
        this.statusBar.show();
    }

    hide() {
        this.statusBar.hide();
    }

    update(pythonPath: string | undefined) {
        if (pythonPath) {
            this.statusBar.text = this._getText(pythonPath);
            this.statusBar.backgroundColor = undefined;
            this.statusBar.tooltip = pythonPath;
        } else {
            this.statusBar.backgroundColor = new ThemeColor('statusBarItem.warningBackground');
            this.statusBar.text = `$(alert) ${BUTTON_LABEL}`;
            this.statusBar.tooltip = 'Select Interpreter';
        }
        this.show();
    }

    private _onDidChangeActiveTextEditor(_: TextEditor | TextEditorSelectionChangeEvent | undefined) {
        const editor = window.activeTextEditor;
        if (!editor || editor.document.languageId !== 'cython') {
            this.hide();
        } else {
            this.show();
        }
    }

    private _getText(pythonPath: string): string {
        const version = this.host.getPythonVersionAsString(pythonPath) || 'Unknown';
        const details = this.host.getPythonExecDetails(pythonPath);

        let info = '';
        if (details && !details.isGlobal) {
            const parts = getPathComponents(getDirectoryPath(pythonPath));
            parts.pop();
            const relativeDir = parts.pop() || '';
            info = `('${relativeDir}': venv)`;
        } else {
            info = details?.arch || '';
        }
        return `${version} ${info}`;
    }
}
