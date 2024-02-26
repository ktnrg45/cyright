/*
 * valid.test.ts
 * Test that samples do not have parsing errors
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';

import { DiagnosticCategory } from '../../common/diagnostic';
import { DiagnosticSink } from '../../common/diagnosticSink';
import * as TestUtils from '../../tests/testUtils';
import { sampleDir, sampleFile } from './utils';

const testName = path.basename(__filename).replace(path.extname(__filename), '');


const paths = fs.readdirSync(sampleDir).filter((name) => name.endsWith('.pyx') || name.endsWith('.pxd'));
paths.forEach((fn) => {
    test(`${testName} ${fn}`, () => {
        const diagSink = new DiagnosticSink();
        const parseInfo = TestUtils.parseSampleFile(sampleFile(fn), diagSink);
        const diags = diagSink.fetchAndClear();
        diags.forEach((diag) => assert.notEqual(diag.category, DiagnosticCategory.Error));
        assert.equal(parseInfo.parseResults.parseTree.statements.length > 0, true);
    });
});
