/*
 * testStateUtils.ts
 * Copyright (c) Microsoft Corporation.
 * Licensed under the MIT license.
 *
 * Test helpers for TestState
 */

import assert from 'assert';
import { WorkspaceEdit } from 'vscode-languageserver-protocol';

import { createMapFromItems } from '../common/collectionUtils';
import { assertNever } from '../common/debug';
import { FileEditAction, FileEditActions } from '../common/editAction';
import { FileSystem } from '../common/fileSystem';
import { convertUriToPath, getDirectoryPath, isFile } from '../common/pathUtils';
import { convertRangeToTextRange } from '../common/positionUtils';
import { rangesAreEqual, TextRange } from '../common/textRange';
import { Range } from './harness/fourslash/fourSlashTypes';
import { TestState } from './harness/fourslash/testState';

export function verifyEdits(
    state: TestState,
    fileEditActions: FileEditActions,
    ranges: Range[],
    replacementText: string | undefined
) {
    for (const edit of fileEditActions.edits) {
        assert(
            ranges.some((r) => {
                const data = r.marker?.data as { r: string } | undefined;
                const expectedText = replacementText ?? data?.r ?? 'N/A';
                const expectedRange = state.convertPositionRange(r);
                return (
                    r.fileName === edit.filePath &&
                    rangesAreEqual(expectedRange, edit.range) &&
                    expectedText.replace(/!n!/g, '\n') === edit.replacementText
                );
            }),
            `can't find '${replacementText ?? edit.replacementText}'@'${edit.filePath}:(${edit.range.start.line},${
                edit.range.start.character
            })'`
        );
    }
}

export function applyFileOperations(state: TestState, fileEditActions: FileEditActions) {
    // Apply changes
    // First, apply text changes
    const editsPerFileMap = createMapFromItems(fileEditActions.edits, (e) => e.filePath);

    for (const [editFileName, editsPerFile] of editsPerFileMap) {
        const result = _applyEdits(state, editFileName, editsPerFile);
        state.testFS.writeFileSync(editFileName, result.text, 'utf8');

        // Update open file content if the file is in opened state.
        if (result.version) {
            let openedFilePath = editFileName;
            const renamed = fileEditActions.fileOperations.find(
                (o) => o.kind === 'rename' && o.oldFilePath === editFileName
            );
            if (renamed?.kind === 'rename') {
                openedFilePath = renamed.newFilePath;
                state.program.setFileClosed(renamed.oldFilePath);
            }

            state.program.setFileOpened(openedFilePath, result.version + 1, [{ text: result.text }]);
        }
    }

    // Second, apply filename change to disk or rename directory.
    for (const fileOperation of fileEditActions.fileOperations) {
        switch (fileOperation.kind) {
            case 'create': {
                state.testFS.mkdirpSync(getDirectoryPath(fileOperation.filePath));
                state.testFS.writeFileSync(fileOperation.filePath, '');
                break;
            }
            case 'rename': {
                if (isFile(state.testFS, fileOperation.oldFilePath)) {
                    state.testFS.mkdirpSync(getDirectoryPath(fileOperation.newFilePath));
                    state.testFS.renameSync(fileOperation.oldFilePath, fileOperation.newFilePath);

                    // Add new file as tracked file
                    state.program.addTrackedFile(fileOperation.newFilePath);
                } else {
                    state.testFS.renameSync(fileOperation.oldFilePath, fileOperation.newFilePath);
                }
                break;
            }
            case 'delete': {
                state.testFS.rimrafSync(fileOperation.filePath);
                break;
            }
            default:
                assertNever(fileOperation);
        }
    }

    // And refresh program.
    state.importResolver.invalidateCache();
    state.program.markAllFilesDirty(true);
}

function _applyEdits(state: TestState, filePath: string, edits: FileEditAction[]) {
    const sourceFile = state.program.getBoundSourceFile(filePath)!;
    const parseResults = sourceFile.getParseResults()!;

    const editsWithOffset = edits
        .map((e) => ({
            range: convertRangeToTextRange(e.range, parseResults.tokenizerOutput.lines)!,
            text: e.replacementText,
        }))
        .sort((e1, e2) => e2.range.start - e1.range.start);

    // Apply change in reverse order.
    let current = parseResults.text;
    for (const change of editsWithOffset) {
        current = current.substr(0, change.range.start) + change.text + current.substr(TextRange.getEnd(change.range));
    }

    return { version: sourceFile.getClientVersion(), text: current };
}

export function convertWorkspaceEditToFileEditActions(fs: FileSystem, edit: WorkspaceEdit): FileEditAction[] {
    const actions: FileEditAction[] = [];

    for (const kv of Object.entries(edit.changes!)) {
        const filePath = convertUriToPath(fs, kv[0]);
        kv[1].forEach((e) => actions.push({ filePath, range: e.range, replacementText: e.newText }));
    }

    return actions;
}
