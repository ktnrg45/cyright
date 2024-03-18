/*
 * createCythonTypeStub.ts
 *
 * Implements 'create cython typestub' command functionality.
 */

import { CancellationToken, ExecuteCommandParams } from 'vscode-languageserver';

import { OperationCanceledException } from '../common/cancellationUtils';
import { getFileExtension } from '../common/pathUtils';
import { LanguageServerInterface } from '../languageServerBase';
import { AnalyzerServiceExecutor } from '../languageService/analyzerServiceExecutor';
import { ServerCommand } from './commandController';
import { CythonTypeStubResult } from './commandResult';

export class CreateCythonTypeStubCommand implements ServerCommand {
    constructor(private _ls: LanguageServerInterface) {}

    async execute(cmdParams: ExecuteCommandParams, token: CancellationToken): Promise<any> {
        if (cmdParams.arguments && cmdParams.arguments.length >= 1) {
            const path = this._ls.decodeTextDocumentUri(cmdParams.arguments[0] as string);
            const workspace = await this._ls.getWorkspaceForFile(path);
            let stubPath = cmdParams.arguments.length >= 2 ? (cmdParams.arguments[1] as string) : '';
            if (!stubPath.length) {
                stubPath = workspace.rootPath;
            }

            const service = await AnalyzerServiceExecutor.cloneService(
                this._ls,
                workspace,
                undefined,
                this._ls.createBackgroundAnalysis()
            );

            // Only handle .pyx files
            // TODO: Possibly handle directories
            const ext = getFileExtension(path);
            if (ext.length && ext !== '.pyx') {
                return;
            }

            const result: CythonTypeStubResult = {
                path: path,
                outPaths: [],
                isFile: true,
                success: false,
            };

            try {
                const outPaths = await service.writeTypeStubInBackgroundCython(path, stubPath, token);
                service.dispose();

                const infoMessage = `Type stub was successfully created for '${path}'.`;
                this._ls.window.showInformationMessage(infoMessage);
                this._ls.reanalyze();
                result.success = true;
                result.outPaths.push(...outPaths);
            } catch (err) {
                const isCancellation = OperationCanceledException.is(err);
                if (isCancellation) {
                    const errMessage = `Type stub creation for '${path}' was canceled`;
                    this._ls.console.error(errMessage);
                } else {
                    let errMessage = '';
                    if (err instanceof Error) {
                        errMessage = ': ' + err.message;
                    }
                    errMessage = `An error occurred when creating type stub for '${path}'` + errMessage;
                    this._ls.console.error(errMessage);
                    this._ls.window.showErrorMessage(errMessage);
                }
            }
            return result;
        }
    }
}
