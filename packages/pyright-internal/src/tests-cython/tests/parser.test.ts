/*
 * parser.test.ts
 */

import * as assert from 'assert';

import { DiagnosticSink } from '../../common/diagnosticSink';
import * as TestUtils from '../../tests/testUtils';

function sampleFile(name: string): string {
    return `cython/${name}.pyx`
}

test('Declaration', () => {
    const diagSink = new DiagnosticSink();
    const parseInfo = TestUtils.parseSampleFile(sampleFile("declaration"), diagSink);

    assert.equal(diagSink.fetchAndClear().length, 0);
    assert.equal(parseInfo.parseResults.parseTree.statements.length, 17);
});

test('Function', () => {
    const diagSink = new DiagnosticSink();
    const parseInfo = TestUtils.parseSampleFile(sampleFile("function"), diagSink);

    assert.equal(diagSink.fetchAndClear().length, 0);
    assert.equal(parseInfo.parseResults.parseTree.statements.length, 12);
});

test('FunctionViewArrayReturn ', () => {
    const diagSink = new DiagnosticSink();
    const parseInfo = TestUtils.parseSampleFile(sampleFile("functionViewArrayReturn"), diagSink);

    assert.equal(diagSink.fetchAndClear().length, 0);
    assert.equal(parseInfo.parseResults.parseTree.statements.length, 5);
});
