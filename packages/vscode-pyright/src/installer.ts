import { ExecException, exec } from 'child_process';
import {
    ExtensionContext,
    OutputChannel,
    ProgressLocation,
    window,
} from 'vscode';

import * as pathConsts from 'pyright-internal/common/pathConsts';
import {
    combinePaths,
    getDirectoryPath,
    getPathComponents,
    isDirectory,
} from 'pyright-internal/common/pathUtils';
import { FullAccessHost } from 'pyright-internal/common/fullAccessHost';
import { versionToString } from 'pyright-internal/common/pythonVersion';
import { HostKind } from 'pyright-internal/common/host';
import { createFromRealFileSystem } from 'pyright-internal/common/realFileSystem';


const COMMAND = "-m pip install cython";
const DISABLE_PROMPT_KEY = "cython_disable_install_prompt"

namespace Message {
    export const Install = 'Install';
    export const Dismiss = 'Dismiss';
    export const DontShow = 'Do not show again';
    export const Success = 'Cython installed.';
    export const Fail = 'Failed to install Cython.';
    export const Prompt = 'Cython not installed.';
    export const Installing = 'Installing Cython...';
};

function install(outputChannel: OutputChannel, pythonPath: string) {
    const command = `${pythonPath} ${COMMAND}`;
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

function installFinished(outputChannel: OutputChannel, error: ExecException | null, stdout: string, stderr: string) {
    outputChannel.appendLine(stdout);
    outputChannel.appendLine(stderr);
    if (!error) {
        outputChannel.appendLine(Message.Success);
        window.showInformationMessage(
            Message.Success,
            Message.Dismiss,
        ).then((item) => {
            disposeChannel(outputChannel, item);
        });
    } else {
        outputChannel.appendLine('\n' + error.message);
        outputChannel.appendLine(Message.Fail);
        window.showErrorMessage(
            Message.Fail,
            Message.Dismiss,
        ).then((item) => {
            disposeChannel(outputChannel, item);
        });
    }
}

async function startInstall(outputChannel: OutputChannel, pythonPath: string) {
    const options = {
        location: ProgressLocation.Notification,
        title: 'Cython',
    };

    await window.withProgress(
        options,
        async (progress) => {
            progress.report({
                message: Message.Installing,
            });

            outputChannel.show();
            outputChannel.appendLine(Message.Installing);
            install(outputChannel, pythonPath);
        }
    );
}

function checkCythonInstalled(outputChannel: OutputChannel, pythonPath: string): boolean {
    let foundCython = false;
    let path = "";
    const fs = createFromRealFileSystem(undefined, undefined);
    const host = FullAccessHost.createHost(HostKind.FullAccess, fs);

    let parts = getPathComponents(getDirectoryPath(pythonPath));
    parts.pop(); // One dir up should be where lib path is
    const dirPath = combinePaths("", ...parts);
    const pyVer = host.getPythonVersion(pythonPath, undefined);

    if (!pyVer) {
        // Something went wrong. Avoid showing prompt
        return true;
    }

    const pyVerStr: string = `python${versionToString(pyVer)}`;
    [pathConsts.lib, pathConsts.lib64, pathConsts.libAlternate].forEach((libPath) => {
        const cythonPath = combinePaths(dirPath, libPath, pyVerStr, pathConsts.sitePackages, pathConsts.cython);
        if (fs.existsSync(cythonPath) && isDirectory(fs, cythonPath)) {
            path = cythonPath;
            foundCython = true;
        }
    });
    if (foundCython) {
        outputChannel.append("Found Cython at: ");
        outputChannel.appendLine(path);
    } else {
        outputChannel.appendLine("Cython Not found");
    }
    return foundCython;
}

function handleInput(context: ExtensionContext, pythonPath: string, input: string | undefined) {
    switch (input) {
        case Message.Install:
            const outputChannel = window.createOutputChannel('Cython Installation');
            startInstall(outputChannel, pythonPath);
            break;
        case Message.DontShow:
            context.globalState.update(DISABLE_PROMPT_KEY, true);
        default:
            break;
    }
}

function prompt(context: ExtensionContext, outputChannel: OutputChannel, pythonPath: string) {
    const promise = window.showErrorMessage(Message.Prompt, Message.Install, Message.DontShow);
    promise.then((item) => {
        handleInput(context, pythonPath, item);
    });

}

export namespace Installer {
    export function installCython(context: ExtensionContext, outputChannel: OutputChannel, pythonPath: string) {
        // context.globalState.update(DISABLE_PROMPT_KEY, false);
        if (context.globalState.get(DISABLE_PROMPT_KEY)) {
            return;
        }
        if (!checkCythonInstalled(outputChannel, pythonPath)) {
            prompt(context, outputChannel, pythonPath);
        }
    }
}
