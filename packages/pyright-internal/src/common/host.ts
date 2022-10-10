/*
 * host.ts
 * Copyright (c) Microsoft Corporation.
 * Licensed under the MIT license.
 *
 * Provides accesses to the host the language service runs on
 */

import { PythonPathResult } from '../analyzer/pythonPathUtils';
import { PythonPlatform } from './configOptions';
import { PythonVersion } from './pythonVersion';

export const enum HostKind {
    FullAccess,
    LimitedAccess,
    NoAccess,
}

export interface Host {
    readonly kind: HostKind;
    getPythonSearchPaths(pythonPath?: string, logInfo?: string[]): PythonPathResult;
    getPythonVersion(pythonPath?: string, logInfo?: string[]): PythonVersion | undefined;
    getPythonPlatform(logInfo?: string[]): PythonPlatform | undefined;
}

export interface PythonExecDetails {
    arch: string,
    isGlobal: boolean,
}

export class NoAccessHost implements Host {
    get kind(): HostKind {
        return HostKind.NoAccess;
    }

    getPythonSearchPaths(pythonPath?: string, logInfo?: string[]): PythonPathResult {
        logInfo?.push('No access to python executable.');

        return {
            paths: [],
            prefix: '',
        };
    }

    getPythonVersion(pythonPath?: string, logInfo?: string[]): PythonVersion | undefined {
        return undefined;
    }

    getPythonVersionAsString(pythonPath?: string, logInfo?: string[]): string | undefined {
        return undefined;
    }

    getPythonExecDetails(pythonPath?: string, logInfo?: string[]): PythonExecDetails | undefined {
        return undefined;
    }

    getPythonPlatform(logInfo?: string[]): PythonPlatform | undefined {
        return undefined;
    }
}

export type HostFactory = () => Host;
