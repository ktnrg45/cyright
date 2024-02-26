/*
 * utils.ts
 * Test utils
 */

import * as path from 'path';

export const sampleDir = path.join('tests', 'samples');
export function sampleFile(filename: string) {
    return path.join('../../tests-cython', sampleDir, filename);
}
