import { exec, ExecException } from 'child_process';
import { ExtensionContext, OutputChannel, ProgressLocation, window } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

import { FullAccessHost } from 'pyright-internal/common/fullAccessHost';
import { HostKind } from 'pyright-internal/common/host';
import { createFromRealFileSystem } from 'pyright-internal/common/realFileSystem';

const COMMAND = '-m pip install cython';
const DISABLE_PROMPT_KEY = 'cython_disable_install_prompt';

namespace Message {
    export const Install = 'Install';
    export const Dismiss = 'Dismiss';
    export const DontShow = 'Do not show again';
    export const Success = 'Cython installed.';
    export const Fail = 'Failed to install Cython.';
    export const Prompt = 'Cython not installed.';
    export const Installing = 'Installing Cython...';
}

function install(outputChannel: OutputChannel, pythonPath: string) {
    const command = `${pythonPath} ${COMMAND}`;
    outputChannel.show();
    outputChannel.appendLine(Message.Installing);
    outputChannel.appendLine('\n' + command);
    exec(command, (error, stdout, stderr) => {
        installFinished(outputChannel, error, stdout, stderr);
    });
}

function disposeChannel(outputChannel: OutputChannel, item: string | undefined) {
    if (item === Message.Dismiss) {
        outputChannel.dispose();
        window.activeTerminal?.show();
    }
}

function onInstallSuccess(cb?: (item?: string) => void) {
    window.showInformationMessage(Message.Success, Message.Dismiss).then((item) => {
        if (cb) {
            cb(item);
        }
    });
}

function installFinished(outputChannel: OutputChannel, error: ExecException | null, stdout: string, stderr: string) {
    outputChannel.appendLine(stdout);
    outputChannel.appendLine(stderr);
    if (!error) {
        onInstallSuccess((item) => {
            outputChannel.appendLine(Message.Success);
            disposeChannel(outputChannel, item);
        });
    } else {
        outputChannel.appendLine('\n' + error.message);
        outputChannel.appendLine(Message.Fail);
        window.showErrorMessage(Message.Fail, Message.Dismiss).then((item) => {
            disposeChannel(outputChannel, item);
        });
    }
}

async function startInstall(pythonPath: string) {
    const outputChannel = window.createOutputChannel('Cython Installation');
    const options = {
        location: ProgressLocation.Notification,
        title: 'Cython',
    };

    await window.withProgress(options, async (progress) => {
        progress.report({
            message: Message.Installing,
            increment: 50,
        });

        install(outputChannel, pythonPath);
        progress.report({ increment: 100 });
    });
}

function getCythonDetails(pythonPath: string, host: FullAccessHost) {
    return host.getCythonExecDetails(pythonPath);
}

function checkCythonInstalled(outputChannel: OutputChannel, pythonPath: string | undefined, host: FullAccessHost) {
    if (!pythonPath) {
        outputChannel.appendLine('Python path is undefined');
        return false;
    }
    outputChannel.appendLine(`Checking for Cython install at path: ${pythonPath}`);
    const details = getCythonDetails(pythonPath, host);
    if (details && details.found) {
        outputChannel.appendLine(`Found Cython (${details.version}) at path: ${pythonPath}`);
    } else {
        outputChannel.appendLine(`Did not find Cython at path: ${pythonPath}`);
    }
    return details?.found ?? false;
}

export class Installer {
    private _context: ExtensionContext;
    private _client: LanguageClient;
    private _pythonPath?: string;
    private _promptVisible = false;
    private _host: FullAccessHost;

    constructor(context: ExtensionContext, client: LanguageClient) {
        this._context = context;
        this._client = client;
        this._host = FullAccessHost.createHost(
            HostKind.FullAccess,
            createFromRealFileSystem(undefined, undefined)
        ) as FullAccessHost;
    }

    onPythonPathUpdate(pythonPath: string | undefined) {
        this._pythonPath = pythonPath;
        if (!this._pythonPath) {
            this._client.outputChannel.appendLine('Installer recieved no python path');
            return;
        }

        // this._context.globalState.update(DISABLE_PROMPT_KEY, false);
        if (this._promptDisabled()) {
            this._client.outputChannel.appendLine('Installer disabled globally');
            return;
        }
        const found = checkCythonInstalled(this._client.outputChannel, this._pythonPath, this._host);
        if (!found) {
            this._prompt();
        }
    }

    private _promptDisabled() {
        return this._context.globalState.get(DISABLE_PROMPT_KEY);
    }

    private _prompt() {
        if (!this._pythonPath || this._promptVisible || this._promptDisabled()) {
            return;
        }
        this._promptVisible = true;
        const promise = window.showWarningMessage(Message.Prompt, Message.Install, Message.DontShow);
        promise.then((item) => {
            this._handleInput(item);
            this._promptVisible = false;
        });
    }

    private _handleInput(input: string | undefined) {
        switch (input) {
            case Message.Install:
                if (this._pythonPath) {
                    // Interpreter could change from when the prompt was shown so check if installed again
                    const isInstalled = checkCythonInstalled(this._client.outputChannel, this._pythonPath, this._host);
                    if (isInstalled) {
                        // Show install success if installed already
                        onInstallSuccess();
                    } else {
                        startInstall(this._pythonPath);
                    }
                }
                break;
            case Message.DontShow:
                this._context.globalState.update(DISABLE_PROMPT_KEY, true);
                break;
            default:
                break;
        }
    }
}
