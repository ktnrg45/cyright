/*
 * commandResult.ts
 * Copyright (c) Microsoft Corporation.
 * Licensed under the MIT license.
 *
 * wrapper for returning custom command data
 */

import { WorkspaceEdit } from 'vscode-languageserver-types';

export interface CommandResult {
    data?: any;
    label: string;
    edits: WorkspaceEdit;
}

export namespace CommandResult {
    export function is(value: any): value is CommandResult {
        return value && value.label !== undefined && value.edits && WorkspaceEdit.is(value.edits);
    }
}

// ! Cython
export interface CythonTypeStubResult {
    path: string,
    outPaths: string[],
    isFile: boolean,
    success: boolean,
}

export namespace CythonTypeStubResult {
    export function is(value: any): value is CythonTypeStubResult {
        if (value) {
            const outPaths = value.outPaths;
            const pathsValid = (Array.isArray(outPaths) && outPaths.every((p) => typeof p === 'string'))
            return pathsValid && typeof value.path === 'string' && value.isFile !== undefined && value.success !== undefined;
        }
        return false;
    }
}
