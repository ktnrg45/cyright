/*
 * typeStubWriter.ts
 * Copyright (c) Microsoft Corporation.
 * Licensed under the MIT license.
 * Author: Eric Traut
 *
 * Logic to emit a type stub file for a corresponding parsed
 * and analyzed python source file.
 */

import {
    ArgumentCategory,
    ArgumentNode,
    AssignmentNode,
    AugmentedAssignmentNode,
    CDefineNode,
    CEnumNode,
    CFunctionNode,
    ClassNode,
    CStructType,
    CTrailType,
    DecoratorNode,
    ExpressionNode,
    ForNode,
    FunctionNode,
    IfNode,
    ImportFromNode,
    ImportNode,
    MemberAccessNode,
    ModuleNameNode,
    NameNode,
    ParameterCategory,
    ParameterNode,
    ParseNode,
    ParseNodeType,
    StatementListNode,
    StringNode,
    TryNode,
    TypeAliasNode,
    TypeAnnotationNode,
    TypeParameterCategory,
    TypeParameterListNode,
    TypeParameterNode,
    WhileNode,
    WithNode,
} from '../parser/parseNodes';
import { IdentifierToken, KeywordType, OperatorType } from '../parser/tokenizerTypes';
import * as AnalyzerNodeInfo from './analyzerNodeInfo';
import { isCythonBuiltIn, isCythonType, transformCythonToPython } from './cythonTransform';
import * as ParseTreeUtils from './parseTreeUtils';
import { ParseTreeWalker } from './parseTreeWalker';
import { getScopeForNode } from './scopeUtils';
import { SourceFile } from './sourceFile';
import { Symbol } from './symbol';
import * as SymbolNameUtils from './symbolNameUtils';
import { TypeEvaluator } from './typeEvaluatorTypes';
import {
    ClassType,
    isAnyOrUnknown,
    isFunction,
    isInstantiableClass,
    isNever,
    isUnknown,
    removeUnknownFromUnion,
    Type,
    TypeBase,
    TypeCategory,
} from './types';

class TrackedImport {
    constructor(public importName: string) {}

    isAccessed = false;
}

class TrackedImportAs extends TrackedImport {
    constructor(importName: string, public alias: string | undefined, public symbol: Symbol) {
        super(importName);
    }
}

interface TrackedImportSymbol {
    symbol?: Symbol | undefined;
    name: string;
    alias?: string | undefined;
    isAccessed: boolean;
}

class TrackedImportFrom extends TrackedImport {
    symbols: TrackedImportSymbol[] = [];

    constructor(importName: string, public isWildcardImport: boolean, public node?: ImportFromNode) {
        super(importName);
    }

    addSymbol(symbol: Symbol | undefined, name: string, alias: string | undefined, isAccessed = false) {
        if (!this.symbols.find((s) => s.name === name)) {
            this.symbols.push({
                symbol,
                name,
                alias,
                isAccessed,
            });
        }
    }
}

class ImportSymbolWalker extends ParseTreeWalker {
    constructor(private _accessedImportedSymbols: Map<string, boolean>, private _treatStringsAsSymbols: boolean) {
        super();
    }

    analyze(node: ExpressionNode) {
        this.walk(node);
    }

    override walk(node: ParseNode) {
        if (!AnalyzerNodeInfo.isCodeUnreachable(node)) {
            super.walk(node);
        }
    }

    override visitName(node: NameNode) {
        this._accessedImportedSymbols.set(node.value, true);
        return true;
    }

    override visitMemberAccess(node: MemberAccessNode): boolean {
        const baseExpression = this._getRecursiveModuleAccessExpression(node.leftExpression);

        if (baseExpression) {
            this._accessedImportedSymbols.set(`${baseExpression}.${node.memberName.value}`, true);
        }

        return true;
    }

    override visitString(node: StringNode) {
        if (this._treatStringsAsSymbols) {
            this._accessedImportedSymbols.set(node.value, true);
        }

        return true;
    }

    private _getRecursiveModuleAccessExpression(node: ExpressionNode): string | undefined {
        if (node.nodeType === ParseNodeType.Name) {
            return node.value;
        }

        if (node.nodeType === ParseNodeType.MemberAccess) {
            const baseExpression = this._getRecursiveModuleAccessExpression(node.leftExpression);
            if (!baseExpression) {
                return undefined;
            }

            return `${baseExpression}.${node.memberName.value}`;
        }

        return undefined;
    }
}

export class TypeStubWriter extends ParseTreeWalker {
    private _indentAmount = 0;
    private _includeAllImports = false;
    private _typeStubText = '';
    private _lineEnd = '\n';
    private _tab = '    ';
    private _classNestCount = 0;
    private _functionNestCount = 0;
    private _ifNestCount = 0;
    private _emittedSuite = false;
    private _emitDocString = true;
    private _trackedImportAs = new Map<string, TrackedImportAs>();
    private _trackedImportFrom = new Map<string, TrackedImportFrom>();
    private _accessedImportedSymbols = new Map<string, boolean>();

    // ! Cython
    private _cEnumName: NameNode | undefined = undefined;
    private _cPropertyName: NameNode | undefined = undefined;

    constructor(
        private _stubPath: string,
        private _sourceFile: SourceFile,
        private _evaluator: TypeEvaluator,
        private _isCython = false // ! Cython
    ) {
        super();

        // As a heuristic, we'll include all of the import statements
        // in "__init__.pyi" files even if they're not locally referenced
        // because these are often used as ways to re-export symbols.
        if (this._stubPath.endsWith('__init__.pyi')) {
            this._includeAllImports = true;
        }
    }

    write() {
        const parseResults = this._sourceFile.getParseResults()!;
        this._lineEnd = parseResults.tokenizerOutput.predominantEndOfLineSequence;
        this._tab = parseResults.tokenizerOutput.predominantTabSequence;

        this.walk(parseResults.parseTree);

        // ! Cython
        if (this._isCython) {
            this._visitCythonExtras();
        }

        this._writeFile();
    }

    override walk(node: ParseNode) {
        if (!AnalyzerNodeInfo.isCodeUnreachable(node)) {
            super.walk(node);
        }
    }

    override visitClass(node: ClassNode) {
        const className = node.name.value;

        this._emittedSuite = true;
        this._emitDocString = true;
        this._emitDecorators(node.decorators);
        let line = `class ${className}`;

        if (node.typeParameters) {
            line += this._printTypeParameters(node.typeParameters);
        }

        // Remove "object" from the list, since it's implied
        const args = node.arguments.filter(
            (arg) =>
                arg.name !== undefined ||
                arg.argumentCategory !== ArgumentCategory.Simple ||
                arg.valueExpression.nodeType !== ParseNodeType.Name ||
                arg.valueExpression.value !== 'object'
        );

        if (args.length > 0) {
            line += `(${args
                .map((arg) => {
                    let argString = '';
                    if (arg.name) {
                        argString = arg.name.value + '=';
                    }
                    argString += this._printExpression(arg.valueExpression);
                    return argString;
                })
                .join(', ')})`;
        }
        line += ':';
        this._emitLine(line);

        this._emitSuite(() => {
            this._classNestCount++;
            this.walk(node.suite);
            this._classNestCount--;
        });

        this._emitLine('');
        this._emitLine('');

        return false;
    }

    override visitFunction(node: FunctionNode) {
        const functionName = node.name.value;

        // ! Cython
        if (node.structType === CStructType.Property) {
            return this._visitCProperty(node);
        } else if (this._cPropertyName) {
            return this._visitCPropertyAccessor(node);
        }

        // Skip if we're already within a function or if the name is private/protected.
        if (this._functionNestCount === 0 && !this._isPrivateOrProtectedNameFunction(functionName)) {
            this._emittedSuite = true;
            this._emitDocString = true;
            this._emitDecorators(node.decorators);
            let line = node.isAsync ? 'async ' : '';
            line += `def ${functionName}`;

            if (node.typeParameters) {
                line += this._printTypeParameters(node.typeParameters);
            }

            line += `(${node.parameters.map((param, index) => this._printParameter(param, node, index)).join(', ')})`;

            let returnAnnotation: string | undefined;
            if (node.returnTypeAnnotation) {
                returnAnnotation = this._printExpression(node.returnTypeAnnotation, /* treatStringsAsSymbols */ true);
            } else if (node.functionAnnotationComment) {
                returnAnnotation = this._printExpression(
                    node.functionAnnotationComment.returnTypeAnnotation,
                    /* treatStringsAsSymbols */ true
                );
            } else {
                // Handle a few common cases where we always know the answer.
                if (node.name.value === '__init__') {
                    returnAnnotation = 'None';
                } else if (node.name.value === '__str__') {
                    returnAnnotation = 'str';
                } else if (['__int__', '__hash__'].some((name) => name === node.name.value)) {
                    returnAnnotation = 'int';
                } else if (
                    ['__eq__', '__ne__', '__gt__', '__lt__', '__ge__', '__le__'].some(
                        (name) => name === node.name.value
                    )
                ) {
                    returnAnnotation = 'bool';
                }
            }

            // ! Cython
            if (this._isCython && !returnAnnotation) {
                // If Cython try to get the return type
                returnAnnotation = this._tryGetReturnAnnotationInferred(node);
            }

            if (returnAnnotation) {
                line += ' -> ' + returnAnnotation;
            }

            line += ':';

            // If there was not return type annotation, see if we can infer
            // a type that is not unknown and add it as a comment.
            if (!returnAnnotation) {
                const functionType = this._evaluator.getTypeOfFunction(node);
                if (functionType && isFunction(functionType.functionType)) {
                    let returnType = this._evaluator.getFunctionInferredReturnType(functionType.functionType);
                    returnType = removeUnknownFromUnion(returnType);
                    if (!isNever(returnType) && !isUnknown(returnType)) {
                        line += ` # -> ${this._evaluator.printType(returnType, /* expandTypeAlias */ false)}:`;
                    }
                }
            }

            this._emitLine(line);

            this._emitSuite(() => {
                // Don't emit any nested functions.
                this._functionNestCount++;
                this.walk(node.suite);
                this._functionNestCount--;
            });

            this._emitLine('');
        }

        return false;
    }

    override visitWhile(node: WhileNode) {
        // Don't emit a doc string after the first statement.
        this._emitDocString = false;
        return false;
    }

    override visitFor(node: ForNode) {
        // Don't emit a doc string after the first statement.
        this._emitDocString = false;
        return false;
    }

    override visitTry(node: TryNode) {
        // Don't emit a doc string after the first statement.
        this._emitDocString = false;
        return false;
    }

    override visitWith(node: WithNode) {
        // Don't emit a doc string after the first statement.
        this._emitDocString = false;
        return false;
    }

    override visitIf(node: IfNode) {
        // Don't emit a doc string after the first statement.
        this._emitDocString = false;

        // Include if statements if they are located
        // at the global scope.
        if (this._functionNestCount === 0 && this._ifNestCount === 0) {
            this._ifNestCount++;
            this._emittedSuite = true;
            this._emitLine('if ' + this._printExpression(node.testExpression) + ':');
            this._emitSuite(() => {
                this.walkMultiple(node.ifSuite.statements);
            });

            const elseSuite = node.elseSuite;
            if (elseSuite) {
                this._emitLine('else:');
                this._emitSuite(() => {
                    if (elseSuite.nodeType === ParseNodeType.If) {
                        this.walkMultiple([elseSuite.testExpression, elseSuite.ifSuite, elseSuite.elseSuite]);
                    } else {
                        this.walkMultiple(elseSuite.statements);
                    }
                });
            }
            this._ifNestCount--;
        }

        return false;
    }

    override visitTypeAlias(node: TypeAliasNode): boolean {
        let line = '';
        line = this._printExpression(node.name);

        if (node.typeParameters) {
            line += this._printTypeParameters(node.typeParameters);
        }

        line += ' = ';
        line += this._printExpression(node.expression);
        this._emitLine(line);

        return false;
    }

    override visitAssignment(node: AssignmentNode) {
        // ! Cython
        if (
            node.leftExpression.nodeType === ParseNodeType.TypeAnnotation &&
            this._isAnnotationCython(node.leftExpression)
        ) {
            return false;
        }

        let isTypeAlias = false;
        let line = '';

        if (node.leftExpression.nodeType === ParseNodeType.Name) {
            // Handle "__all__" as a special case.
            if (node.leftExpression.value === '__all__') {
                if (this._functionNestCount === 0 && this._ifNestCount === 0) {
                    this._emittedSuite = true;

                    line = this._printExpression(node.leftExpression);
                    line += ' = ';
                    line += this._printExpression(node.rightExpression);
                    this._emitLine(line);
                }

                return false;
            }

            if (this._functionNestCount === 0) {
                line = this._printExpression(node.leftExpression);
                if (node.typeAnnotationComment) {
                    line += ': ' + this._printExpression(node.typeAnnotationComment, /* treatStringsAsSymbols */ true);
                }

                const valueType = this._evaluator.getType(node.leftExpression);
                if (valueType?.typeAliasInfo) {
                    isTypeAlias = true;
                } else if (node.rightExpression.nodeType === ParseNodeType.Call) {
                    // Special-case TypeVar, TypeVarTuple, ParamSpec and NewType calls. Treat
                    // them like type aliases.
                    const callBaseType = this._evaluator.getType(node.rightExpression.leftExpression);
                    if (
                        callBaseType &&
                        isInstantiableClass(callBaseType) &&
                        ClassType.isBuiltIn(callBaseType, ['TypeVar', 'TypeVarTuple', 'ParamSpec', 'NewType'])
                    ) {
                        isTypeAlias = true;
                    }
                } else if (!node.typeAnnotationComment && this._cEnumName) {
                    // ! Cython add annotation for cenum global fields
                    line += `: ${this._printExpression(this._cEnumName, /* treatStringsAsSymbols */ true)}`;
                }
            }
        } else if (node.leftExpression.nodeType === ParseNodeType.TypeAnnotation) {
            const valueExpr = node.leftExpression.valueExpression;

            if (valueExpr.nodeType === ParseNodeType.Name) {
                if (this._functionNestCount === 0) {
                    line = `${this._printExpression(valueExpr)}: ${this._printExpression(
                        node.leftExpression.typeAnnotation,
                        /* treatStringsAsSymbols */ true
                    )}`;
                }
            }
        }

        if (line) {
            this._emittedSuite = true;

            line += ' = ';

            if (isTypeAlias) {
                line += this._printExpression(node.rightExpression);
            } else {
                line += '...';
            }
            this._emitLine(line);
        }

        return false;
    }

    override visitAugmentedAssignment(node: AugmentedAssignmentNode) {
        if (node.leftExpression.nodeType === ParseNodeType.Name) {
            // Handle "__all__ +=" as a special case.
            if (node.leftExpression.value === '__all__' && node.operator === OperatorType.AddEqual) {
                if (this._functionNestCount === 0 && this._ifNestCount === 0) {
                    let line = this._printExpression(node.leftExpression);
                    line += ' += ';
                    line += this._printExpression(node.rightExpression);
                    this._emitLine(line);
                }
            }
        }

        return false;
    }

    override visitTypeAnnotation(node: TypeAnnotationNode) {
        // ! Cython
        if (this._isAnnotationCython(node)) {
            return false;
        }
        if (this._functionNestCount === 0) {
            let line = '';
            if (node.valueExpression.nodeType === ParseNodeType.Name) {
                line = this._printExpression(node.valueExpression);
            } else if (node.valueExpression.nodeType === ParseNodeType.MemberAccess) {
                const baseExpression = node.valueExpression.leftExpression;
                if (baseExpression.nodeType === ParseNodeType.Name) {
                    if (baseExpression.value === 'self') {
                        const memberName = node.valueExpression.memberName.value;
                        if (!SymbolNameUtils.isPrivateOrProtectedName(memberName)) {
                            line = this._printExpression(node.valueExpression);
                        }
                    }
                }
            }

            if (line) {
                line += ': ' + this._printExpression(node.typeAnnotation, /* treatStringsAsSymbols */ true);
                this._emitLine(line);
            }
        }

        return false;
    }

    override visitImport(node: ImportNode) {
        if (this._functionNestCount > 0 || this._classNestCount > 0) {
            return false;
        }

        const currentScope = getScopeForNode(node);
        if (currentScope) {
            // Record the input for later.
            node.list.forEach((imp) => {
                const moduleName = this._printModuleName(imp.module);
                if (!this._trackedImportAs.has(moduleName)) {
                    const symbolName = imp.alias
                        ? imp.alias.value
                        : imp.module.nameParts.length > 0
                        ? imp.module.nameParts[0].value
                        : '';
                    const symbolInfo = currentScope.lookUpSymbolRecursive(symbolName);
                    if (symbolInfo) {
                        const trackedImportAs = new TrackedImportAs(
                            moduleName,
                            imp.alias ? imp.alias.value : undefined,
                            symbolInfo.symbol
                        );
                        this._trackedImportAs.set(moduleName, trackedImportAs);
                    }
                }
            });
        }

        return false;
    }

    override visitImportFrom(node: ImportFromNode) {
        if (this._functionNestCount > 0 || this._classNestCount > 0) {
            return false;
        }

        const currentScope = getScopeForNode(node);
        if (currentScope) {
            // Record the input for later.
            const moduleName = this._printModuleName(node.module);
            let trackedImportFrom = this._trackedImportFrom.get(moduleName);
            if (!trackedImportFrom) {
                trackedImportFrom = new TrackedImportFrom(moduleName, node.isWildcardImport, node);
                this._trackedImportFrom.set(moduleName, trackedImportFrom);
            }

            node.imports.forEach((imp) => {
                const symbolName = imp.alias ? imp.alias.value : imp.name.value;
                const symbolInfo = currentScope.lookUpSymbolRecursive(symbolName);
                if (symbolInfo) {
                    trackedImportFrom!.addSymbol(
                        symbolInfo.symbol,
                        imp.name.value,
                        imp.alias ? imp.alias.value : undefined,
                        false
                    );
                }
            });
        }

        return false;
    }

    override visitStatementList(node: StatementListNode) {
        if (node.statements.length > 0 && node.statements[0].nodeType === ParseNodeType.StringList) {
            // Is this the first statement in a suite? If it's a string
            // literal, assume it's a doc string and emit it.
            if (!this._emittedSuite && this._emitDocString) {
                this._emitLine(this._printExpression(node.statements[0]));
            }
        }

        // Don't emit a doc string after the first statement.
        this._emitDocString = false;

        this.walkMultiple(node.statements);
        return false;
    }

    private _emitSuite(callback: () => void) {
        this._increaseIndent(() => {
            const prevEmittedSuite = this._emittedSuite;
            this._emittedSuite = false;

            callback();

            if (!this._emittedSuite) {
                this._emitLine('...');
            }

            this._emittedSuite = prevEmittedSuite;
        });
    }

    private _increaseIndent(callback: () => void) {
        this._indentAmount++;
        callback();
        this._indentAmount--;
    }

    private _emitDecorators(decorators: DecoratorNode[]) {
        decorators.forEach((decorator) => {
            this._emitLine('@' + this._printExpression(decorator.expression));
        });
    }

    private _printHeaderDocString() {
        return (
            '"""' +
            this._lineEnd +
            'This type stub file was generated by cyright.' +
            this._lineEnd +
            '"""' +
            this._lineEnd +
            this._lineEnd
        );
    }

    private _emitLine(line: string) {
        for (let i = 0; i < this._indentAmount; i++) {
            this._typeStubText += this._tab;
        }

        this._typeStubText += line + this._lineEnd;
    }

    private _printTypeParameters(node: TypeParameterListNode): string {
        return `[${node.parameters.map((typeParam) => this._printTypeParameter(typeParam)).join(',')}]`;
    }

    private _printTypeParameter(node: TypeParameterNode): string {
        let line = '';

        if (node.typeParamCategory === TypeParameterCategory.TypeVarTuple) {
            line += '*';
        } else if (node.typeParamCategory === TypeParameterCategory.ParamSpec) {
            line += '**';
        }

        line += node.name.value;

        if (node.boundExpression) {
            line += ': ';
            line += this._printExpression(node.boundExpression);
        }

        return line;
    }

    private _printModuleName(node: ModuleNameNode): string {
        let line = '';
        for (let i = 0; i < node.leadingDots; i++) {
            line += '.';
        }
        line += node.nameParts.map((part) => part.value).join('.');
        return line;
    }

    private _printParameter(paramNode: ParameterNode, functionNode: FunctionNode, paramIndex: number): string {
        let line = '';
        if (paramNode.category === ParameterCategory.VarArgList) {
            line += '*';
        } else if (paramNode.category === ParameterCategory.VarArgDictionary) {
            line += '**';
        }

        if (paramNode.name) {
            line += paramNode.name.value;
        } else if (paramNode.category === ParameterCategory.Simple) {
            line += '/';
        }

        const paramTypeAnnotation = ParseTreeUtils.getTypeAnnotationForParameter(functionNode, paramIndex);
        let paramType = '';
        if (paramTypeAnnotation) {
            paramType = this._printExpression(paramTypeAnnotation, /* treatStringsAsSymbols */ true);
        }

        if (paramType) {
            line += ': ' + paramType;
        }

        if (paramNode.defaultValue) {
            // Follow PEP8 spacing rules. Include spaces if type
            // annotation is present, no space otherwise.
            if (paramType) {
                line += ' = ...';
            } else {
                line += '=...';
            }
        }

        return line;
    }

    private _printExpression(node: ExpressionNode, isType = false, treatStringsAsSymbols = false): string {
        // ! Cython
        const transformed = this._printTransformedCython(node, isType);
        if (transformed) {
            return transformed;
        }

        const importSymbolWalker = new ImportSymbolWalker(this._accessedImportedSymbols, treatStringsAsSymbols);
        importSymbolWalker.analyze(node);

        let expressionFlags = isType
            ? ParseTreeUtils.PrintExpressionFlags.ForwardDeclarations
            : ParseTreeUtils.PrintExpressionFlags.None;
        expressionFlags |= ParseTreeUtils.PrintExpressionFlags.DoNotLimitStringLength;

        return ParseTreeUtils.printExpression(node, expressionFlags);
    }

    private _printTrackedImports() {
        let importStr = '';
        let lineEmitted = false;

        // Emit the "import" statements.
        this._trackedImportAs.forEach((imp) => {
            if (this._accessedImportedSymbols.get(imp.alias || imp.importName)) {
                imp.isAccessed = true;
            }

            if (imp.isAccessed || this._includeAllImports) {
                importStr += `import ${imp.importName}`;
                if (imp.alias) {
                    importStr += ` as ${imp.alias}`;
                }
                importStr += this._lineEnd;
                lineEmitted = true;
            }
        });

        // Emit the "import from" statements.
        this._trackedImportFrom.forEach((imp) => {
            imp.symbols.forEach((s) => {
                if (this._accessedImportedSymbols.get(s.alias || s.name)) {
                    s.isAccessed = true;
                }
            });

            if (imp.isWildcardImport) {
                // ! Cython
                // Don't include the matching .pxd which has an invalid range
                if (!(imp.node && imp.node.isCython && imp.node.start <= 0 && imp.node.length <= 0)) {
                    importStr += `from ${imp.importName} import *` + this._lineEnd;
                    lineEmitted = true;
                }
            }

            const sortedSymbols = imp.symbols
                .filter((s) => s.isAccessed || this._includeAllImports)
                .sort((a, b) => {
                    if (a.name < b.name) {
                        return -1;
                    } else if (a.name > b.name) {
                        return 1;
                    }
                    return 0;
                });

            if (sortedSymbols.length > 0) {
                importStr += `from ${imp.importName} import `;

                importStr += sortedSymbols
                    .map((symbol) => {
                        let symStr = symbol.name;
                        if (symbol.alias) {
                            symStr += ' as ' + symbol.alias;
                        }
                        return symStr;
                    })
                    .join(', ');

                importStr += this._lineEnd;
                lineEmitted = true;
            }
        });

        if (lineEmitted) {
            importStr += this._lineEnd;
        }

        return importStr;
    }

    private _writeFile() {
        let finalText = this._printHeaderDocString();
        finalText += this._printTrackedImports();
        finalText += this._typeStubText;

        this._sourceFile.fileSystem.writeFileSync(this._stubPath, finalText, 'utf8');
    }

    // ! Cython
    override visitCEnum(node: CEnumNode): boolean {
        if (node.cpdef) {
            const enumType = 'IntEnum';
            this._addSyntheticImport('enum', enumType);
            const alias: ClassNode = { ...CEnumNode.alias(node) };
            const arg = ArgumentNode.create(undefined, this._createDummyName(enumType), ArgumentCategory.Simple);
            alias.arguments = [arg];
            this.visitClass(alias);

            // Also add fields to parent scope if cpdef enum; (No class)
            if (!node.classToken) {
                this._cEnumName = node.name;
                this.walkMultiple(node.suite.statements);
                this._cEnumName = undefined;
            }
        }
        return false;
    }

    override visitCDefine(node: CDefineNode): boolean {
        return false;
    }

    override visitCFunction(node: CFunctionNode): boolean {
        if (node.cpdef) {
            return this.visitFunction(node);
        } else if (node.structType === CStructType.Property) {
            return this._visitCProperty(node);
        }
        return false;
    }

    private _visitCProperty(node: FunctionNode) {
        this._cPropertyName = { ...node.name, ...{ start: 0, length: 0 } };
        this.walk(node.suite);
        this._cPropertyName = undefined;
        return false;
    }

    private _visitCPropertyAccessor(node: FunctionNode) {
        const propertyName = this._cPropertyName;
        const accessorMap: Map<string, string> = new Map<string, string>([
            ['__get__', 'getter'],
            ['__set__', 'setter'],
            ['__del__', 'deleter'],
        ]);
        const accessorName = accessorMap.get(node.name.value);
        if (!accessorName || !propertyName) {
            return false;
        }
        this._cPropertyName = undefined; // Unset so we can handle the next function normally
        // Create dummy property function
        const propToken = this._createDummyIdentifier('property');
        const propExpression =
            accessorName === 'getter'
                ? NameNode.create(propToken)
                : MemberAccessNode.create({ ...propertyName }, this._createDummyName(accessorName));
        const decorator = DecoratorNode.create(propToken, propExpression);
        const functionNode = { ...node };
        functionNode.name = { ...propertyName };
        functionNode.decorators = [...node.decorators, decorator];
        functionNode.name.parent = functionNode;
        decorator.parent = functionNode;
        this.visitFunction(functionNode);
        this._cPropertyName = propertyName; // Reset
        return false;
    }

    private _printTransformedCython(node: ExpressionNode, isType = false) {
        let transformed: string | undefined = undefined;
        if (!isType) {
            return undefined;
        }
        // Show view as a buffer. Closest type is `array.array`
        // TODO: use typing.WriteableBuffer
        if (node.nodeType === ParseNodeType.CType && node.typeTrailNode?.trailType === CTrailType.View) {
            // Add buffer import
            const bufferMod = 'array';
            const bufferType = 'array';
            this._addSyntheticImport(bufferMod, bufferType);
            transformed = `${bufferType}[${this._printExpression(node.expression, isType)}]`;
        } else {
            let type = this._evaluator.getType(node);
            if (type && isCythonType(type)) {
                if (isCythonBuiltIn(type)) {
                    type = transformCythonToPython(
                        this._evaluator.getBuiltInType,
                        node,
                        type,
                        /*memberAccessName*/ undefined,
                        /*forcePython*/ true
                    );
                }
                if (isAnyOrUnknown(type)) {
                    this._addSyntheticImport('typing', 'Any');
                }
                type = TypeBase.cloneType(type);
                // Remove cython details
                type.cythonDetails = undefined;
                if (TypeBase.isInstantiable(type)) {
                    type = TypeBase.cloneTypeAsInstance(type);
                }
                transformed = this._evaluator.printType(type, /*expandTypeAlias*/ true);
            }
        }
        return transformed;
    }

    private _addSyntheticImport(moduleName: string, importName: string) {
        if (moduleName === 'builtins') {
            // Don't add builtins
            return;
        }
        let trackedImport = this._trackedImportFrom.get(moduleName);
        if (!trackedImport) {
            trackedImport = new TrackedImportFrom(moduleName, false, undefined);
            this._trackedImportFrom.set(moduleName, trackedImport);
        }
        trackedImport.addSymbol(undefined, importName, undefined, true);
    }

    private _isGlobalScopeCython() {
        return this._isCython && this._functionNestCount === 0 && this._classNestCount === 0;
    }

    private _createDummyIdentifier(name: string) {
        return IdentifierToken.create(0, 0, name, undefined);
    }
    private _createDummyName(name: string) {
        return NameNode.create(this._createDummyIdentifier(name));
    }

    private _isPrivateOrProtectedNameFunction(name: string) {
        const ignore = ['__cinit__', '__dealloc__'];
        if (this._isCython) {
            // If Cython allow protected names but disallow special cython names
            return ignore.includes(name) || SymbolNameUtils.isPrivateName(name);
        }
        return SymbolNameUtils.isPrivateOrProtectedName(name);
    }

    private _isAnnotationCython(node: TypeAnnotationNode) {
        return [ParseNodeType.CType, ParseNodeType.CCallback].includes(node.typeAnnotation.nodeType);
    }

    private _printReturnTypeInferred(type: Type) {
        const typeText = this._evaluator.printType(type);
        const addType = (tp: Type) => {
            let module: string | undefined = undefined;
            let name: string | undefined = undefined;
            switch (tp.category) {
                case TypeCategory.Class:
                    {
                        if (tp.typeAliasInfo) {
                            const idx = tp.typeAliasInfo.fullName.lastIndexOf('.');
                            module = tp.typeAliasInfo.fullName.slice(0, idx);
                            name = tp.typeAliasInfo.name;
                            if (tp.typeAliasInfo.typeArguments) {
                                tp.typeAliasInfo.typeArguments.forEach((t) => addType(t));
                            }
                        } else {
                            module = tp.details.moduleName;
                            name = tp.details.name;
                            if (tp.typeArguments) {
                                tp.typeArguments.forEach((t) => addType(t));
                            }
                        }
                        if (tp.literalValue) {
                            this._addSyntheticImport('typing', 'Literal');
                        }
                    }
                    break;
                case TypeCategory.Union:
                    tp.subtypes.forEach((t) => addType(t));
                    break;

                case TypeCategory.Any:
                case TypeCategory.Never:
                case TypeCategory.Unbound:
                case TypeCategory.Unknown:
                    this._addSyntheticImport('typing', 'Any');
                    break;
            }
            if (module && name) {
                if (typeText.match(name)) {
                    this._addSyntheticImport(module, name);
                }
            }
        };
        addType(type);

        return typeText;
    }

    private _tryGetReturnAnnotationInferred(node: FunctionNode): string | undefined {
        const functionType = this._evaluator.getTypeOfFunction(node);
        if (functionType && isFunction(functionType.functionType)) {
            let returnType = this._evaluator.getFunctionInferredReturnType(functionType.functionType);
            returnType = removeUnknownFromUnion(returnType);
            if (returnType.cythonDetails) {
                returnType = transformCythonToPython(
                    this._evaluator.getBuiltInType,
                    node,
                    returnType,
                    /*memberAccessName*/ undefined,
                    /*forcePython*/ true
                );
            }

            const text = this._printReturnTypeInferred(returnType);
            return text.length > 0 ? text : undefined;
        }
        return undefined;
    }

    private _visitCythonExtras() {
        // Visit extra nodes from matching .pxd
        let matchingImport: TrackedImportFrom | undefined = undefined;
        for (const value of this._trackedImportFrom.values()) {
            if (value.node && value.node.start < 0) {
                // matching import should have invalid start
                matchingImport = value;
                break;
            }
        }
        if (matchingImport && matchingImport.node) {
            const imports = this._sourceFile.getImports();
            // Matching pxd result should be first import
            const importResult = imports.length > 0 ? imports[0] : undefined;
            const scope = getScopeForNode(matchingImport.node);
            if (scope && importResult) {
                for (const name of scope.symbolTable.keys()) {
                    const symbol = this._evaluator.lookUpSymbolRecursive(matchingImport.node, name, true)?.symbol;
                    const decls = symbol?.getDeclarations();
                    const decl = decls && decls.length > 0 ? decls[decls.length - 1] : undefined;
                    const aliasInfo = decl ? this._evaluator.resolveAliasDeclarationWithInfo(decl, false) : undefined;
                    const aliasPath = aliasInfo?.declaration?.path;
                    const node = aliasInfo?.declaration?.node;
                    if (
                        node &&
                        aliasPath &&
                        aliasPath !== this._sourceFile.getFilePath() &&
                        importResult.resolvedPaths.includes(aliasPath)
                    ) {
                        // Check that declaration is from the matching .pxd and is not declared in this file
                        if (node.nodeType === ParseNodeType.Class && node.structType === CStructType.Enum) {
                            // Write cpdef enums declared in matching .pxd
                            const enumNode = node as unknown as CEnumNode;
                            if (enumNode.cpdef) {
                                this.visitCEnum(enumNode);
                            }
                        } else if (
                            CFunctionNode.isInstance(node) &&
                            node.cpdef &&
                            node.modifier === KeywordType.Inline
                        ) {
                            // Write inline cpdef functions
                            this.visitCFunction(node);
                        }
                    }
                }
            }
        }
    }
}
