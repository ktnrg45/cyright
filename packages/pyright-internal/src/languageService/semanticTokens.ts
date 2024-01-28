import { CancellationToken, SemanticTokens, SemanticTokensBuilder } from 'vscode-languageserver';

import { AnalyzerFileInfo } from '../analyzer/analyzerFileInfo';
import { getFileInfo } from '../analyzer/analyzerNodeInfo';
import { DeclarationType } from '../analyzer/declaration';
import { ParseTreeWalker } from '../analyzer/parseTreeWalker';
import { AnalyzerService } from '../analyzer/service';
import { TypeEvaluator } from '../analyzer/typeEvaluatorTypes';
import { isClass, isFunction, isModule, TypeCategory } from '../analyzer/types';
import { throwIfCancellationRequested } from '../common/cancellationUtils';
import { ConsoleInterface } from '../common/console';
import { convertOffsetToPosition } from '../common/positionUtils';
import { TextRange } from '../common/textRange';
import { WorkspaceServiceInstance } from '../languageServerBase';
import {
    CEnumNode,
    CFunctionDeclNode,
    CFunctionNode,
    CParameterNode,
    CStructNode,
    CTypeDefNode,
    CTypeNode,
    ExpressionNode,
    FunctionNode,
    ModuleNameNode,
    NameNode,
    ParseNode,
    ParseNodeType,
    TypeAnnotationNode,
} from '../parser/parseNodes';
import { ParseResults } from '../parser/parser';
import { PyrightServer } from '../server';

enum LegendType {
    Class = 'class',
    Variable = 'variable',
    Namespace = 'namespace',
    Function = 'function',
    Parameter = 'parameter',
    EnumMember = 'enumMember',
    TypeParameter = 'typeParameter',
    //
    Comment = 'comment',
    String = 'string',
    Number = 'number',
    Keyword = 'keyword',
    Regexp = 'regexp',
    Operator = 'operator',
    Type = 'type',
    Struct = 'struct',
    Interface = 'interface',
    Enum = 'enum',
    Method = 'method',
    Decorator = 'decorator',
    Macro = 'macro',
    Property = 'property',
    Label = 'label',
}

enum LegendMod {
    Declaration = 'declaration',
    Readonly = 'readonly',
    Modification = 'modification',
    //
    // Documentation = 'documentation',
    // Static = 'static',
    // Abstract = 'abstract',
    // Deprecated = 'deprecated',
    // Async = 'async',
}

export const tokenTypesLegend: string[] = Object.values(LegendType);
export const tokenModifiersLegend: string[] = Object.values(LegendMod);

type LegendModType = string | readonly string[] | LegendMod | readonly LegendMod[];
type LegendTypeType = string | LegendType;

function encodeType(type: LegendTypeType) {
    return tokenTypesLegend.indexOf(type);
}

function encodeModifiers(tokenModifiers?: LegendModType) {
    let result = 0;
    const modifiers = tokenModifiers && !Array.isArray(tokenModifiers) ? [tokenModifiers] : tokenModifiers;
    if (!modifiers || modifiers.length === 0 || !Array.isArray(modifiers)) {
        return result;
    }
    modifiers.forEach((modifier) => {
        const index = tokenModifiersLegend.indexOf(modifier);
        if (index >= 0) {
            result = result | (1 << index);
        } else {
            result = result | (1 << (tokenModifiersLegend.length + 2));
        }
    });
    return result;
}

// const _regexCppOperator = /^operator[+\-*/=!><]|(\+\+|--|>=|<=|==|!=|\[\]|bool)$/;

export class CythonSemanticTokenProvider {
    private _ls: PyrightServer;

    constructor(ls: PyrightServer) {
        this._ls = ls;
    }

    async provideSemanticTokensFull(filePath: string, workspace: WorkspaceServiceInstance, token: CancellationToken) {
        const results = workspace.serviceInstance.getParseResult(filePath);
        let builder = new SemanticTokensBuilder();
        if (results) {
            const fileInfo: AnalyzerFileInfo = getFileInfo(results.parseTree);
            builder = new CythonSemanticTokensBuilder(
                workspace.serviceInstance,
                this._ls.console,
                fileInfo,
                results,
                token
            );
            return builder.build();
        }
        return builder.build();
    }
}

class CythonSemanticTokensBuilder extends SemanticTokensBuilder {
    private _service: AnalyzerService;
    private _console: ConsoleInterface;
    private _fileInfo: AnalyzerFileInfo;
    private _results: ParseResults | undefined;
    private _cancellationToken: CancellationToken;
    private _finished: boolean;

    constructor(
        service: AnalyzerService,
        console: ConsoleInterface,
        fileInfo: AnalyzerFileInfo,
        results: ParseResults,
        cancellationToken: CancellationToken
    ) {
        super();
        this._service = service;
        this._console = console;
        this._fileInfo = fileInfo;
        this._results = results;
        this._cancellationToken = cancellationToken;
        this._finished = false;
    }

    override build(): SemanticTokens {
        const evaluator = this._service.getEvaluator();
        if (this._results?.parseTree && evaluator) {
            const parseTreeWalker = new SemanticTokensWalker(this, evaluator, this._cancellationToken);
            try {
                parseTreeWalker.walk(this._results.parseTree);
            } catch (error) {
                this._console.error(`SemanticTokens cancelled for file: ${this._fileInfo.filePath}`);
            }
        }
        return super.build();
    }

    getPosition(range: TextRange) {
        return convertOffsetToPosition(range.start, this._fileInfo.lines);
    }

    pushRange(range: TextRange, legendType: LegendTypeType, legendModifiers?: LegendModType) {
        const pos = this.getPosition(range);
        this.push(pos.line, pos.character, range.length, encodeType(legendType), encodeModifiers(legendModifiers));
    }
}

class SemanticTokensWalker extends ParseTreeWalker {
    private _builder: CythonSemanticTokensBuilder;
    private _evaluator: TypeEvaluator;
    private _cancellationToken: CancellationToken;

    constructor(builder: CythonSemanticTokensBuilder, evaluator: TypeEvaluator, cancellationToken: CancellationToken) {
        super();
        this._builder = builder;
        this._evaluator = evaluator;
        this._cancellationToken = cancellationToken;
    }

    getTokenTypeForName(node: NameNode) {
        let tokenType: LegendTypeType = LegendType.Variable;
        const declarations = this._evaluator.getDeclarationsForNameNode(node);
        const type = this.getType(node);
        if (type && isModule(type)) {
            tokenType = LegendType.Namespace;
            return tokenType;
        }
        if (declarations && declarations.length > 0) {
            let primaryDeclaration = declarations[0];

            if (primaryDeclaration.type === DeclarationType.Alias && declarations.length > 1) {
                primaryDeclaration = declarations[1];
            }
            const resolvedDecl = this._evaluator.resolveAliasDeclaration(
                primaryDeclaration,
                /* resolveLocalNames */ true
            );
            switch (resolvedDecl?.type) {
                case DeclarationType.Class:
                case DeclarationType.SpecialBuiltInClass:
                    tokenType = LegendType.Class;
                    break;
                case DeclarationType.Function:
                    tokenType = LegendType.Function;
                    if (primaryDeclaration.node.nodeType === ParseNodeType.Function) {
                        if (this.isProperty(primaryDeclaration.node)) {
                            tokenType = LegendType.Variable;
                        }
                    }
                    break;
                case DeclarationType.TypeAlias:
                    if (type && isClass(type)) {
                        tokenType = LegendType.Class;
                    } else if (type && isFunction(type)) {
                        tokenType = LegendType.Function;
                    }
                    break;
                case DeclarationType.Variable:
                    tokenType = LegendType.Variable;
                    break;
                case DeclarationType.Parameter:
                    tokenType = LegendType.Parameter;
                    break;
                case DeclarationType.TypeParameter:
                    tokenType = LegendType.TypeParameter;
                    break;
            }
        } else {
            tokenType = '';
        }
        return tokenType;
    }

    getType(node: ExpressionNode) {
        let type = this._evaluator.getType(node);
        if (
            type?.category === TypeCategory.Union &&
            type.subtypes.length > 0 &&
            type.subtypes.every((sub) => sub.category === TypeCategory.Module)
        ) {
            // ! Cython: Experimental: Allow imports of same name for python and cython
            type = type.subtypes[0];
        }
        return type;
    }

    isProperty(node: FunctionNode): boolean {
        let isProp = false;
        for (const dec of node.decorators) {
            const type = this.getType(dec.expression);
            if (type?.category === TypeCategory.Class) {
                if (type.details.fullName === 'builtins.property') {
                    isProp = true;
                    break;
                }
            }
        }
        return isProp;
    }

    pushRange(range: TextRange, legendType: LegendTypeType, legendModifiers?: LegendModType) {
        this._builder.pushRange(range, legendType, legendModifiers);
    }

    override walk(node: ParseNode): void {
        throwIfCancellationRequested(this._cancellationToken);
        if (node.start >= 0) {
            super.walk(node);
        }
    }

    override visitName(node: NameNode): boolean {
        const type = this.getTokenTypeForName(node);
        this.pushRange(node, type);
        return false;
    }

    override visitCType(node: CTypeNode): boolean {
        node.varModifiers.forEach((mod) => {
            this.pushRange(mod, LegendType.Keyword, LegendMod.Modification);
        });
        node.numModifiers.forEach((mod) => {
            this.pushRange(mod, LegendType.Keyword, LegendMod.Modification);
        });
        return true;
    }

    override visitCTypeDef(node: CTypeDefNode): boolean {
        if (node.expression.nodeType === ParseNodeType.CFunctionDecl) {
            this.walk(node.expression);
        } else {
            this.walkMultiple([node.expression, node.name]);
        }
        return false;
    }

    override visitCFunctionDecl(node: CFunctionDeclNode): boolean {
        this.walkMultiple([
            ...node.decorators,
            node.returnTypeAnnotation,
            node.typeParameters, // TODO
            node.name,
            ...node.parameters,
        ]);
        return false;
    }

    override visitTypeAnnotation(node: TypeAnnotationNode): boolean {
        const type = this.getType(node.valueExpression);
        if (type && (isClass(type) || isFunction(type))) {
            if (type.cythonDetails) {
                this.walkMultiple([node.typeAnnotation, node.valueExpression]);
                return false;
            }
        }
        return true;
    }

    override visitCParameter(node: CParameterNode): boolean {
        //this.walkMultiple([node.typeAnnotation, node.name]);
        this.walkMultiple([node.typeAnnotation]);
        if (node.name) {
            this.pushRange(node.name, LegendType.Parameter);
        }
        return false;
    }

    override visitModuleName(node: ModuleNameNode): boolean {
        node.nameParts.forEach((n) => this.pushRange(n, LegendType.Namespace));
        return false;
    }

    override visitCStruct(node: CStructNode): boolean {
        if (node.packedToken) {
            this.pushRange(node.packedToken, LegendType.Keyword, LegendMod.Modification);
        }
        this.pushRange(node.structToken, LegendType.Keyword, LegendMod.Modification);
        return true;
    }

    override visitCEnum(node: CEnumNode): boolean {
        this.pushRange(node.enumToken, LegendType.Keyword, LegendMod.Modification);
        return true;
    }

    override visitCFunction(node: CFunctionNode): boolean {
        this.walkMultiple([...node.decorators, node.returnTypeAnnotation, node.functionAnnotationComment]);
        this.walkMultiple([node.name, node.typeParameters, ...node.parameters, node.suite]);
        return false;
    }
}
