import { CancellationToken, SemanticTokens, SemanticTokensBuilder } from 'vscode-languageserver';

import { DeclarationType } from '../analyzer/declaration';
import { ParseTreeWalker } from '../analyzer/parseTreeWalker';
import { TypeEvaluator } from '../analyzer/typeEvaluatorTypes';
import { isClass, isFunction, isModule, TypeCategory } from '../analyzer/types';
import { throwIfCancellationRequested } from '../common/cancellationUtils';
import { convertOffsetToPosition } from '../common/positionUtils';
import { TextRange } from '../common/textRange';
import {
    CCallbackNode,
    CEnumNode,
    CFunctionNode,
    CParameterNode,
    CStructNode,
    CTypeDefNode,
    CTypeNode,
    ExpressionNode,
    FunctionNode,
    ModuleNameNode,
    NameNode,
    ParameterNode,
    ParseNode,
    ParseNodeType,
    TypeAnnotationNode,
} from '../parser/parseNodes';
import { ParseResults } from '../parser/parser';

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
    static provideSemanticTokensFull(parseResults: ParseResults, evaluator: TypeEvaluator, token: CancellationToken) {
        throwIfCancellationRequested(token);
        const builder = new CythonSemanticTokensBuilder(parseResults, evaluator, token);
        return builder.build();
    }
}

class CythonSemanticTokensBuilder extends SemanticTokensBuilder {
    private _parseResults: ParseResults;
    private _evaluator: TypeEvaluator;
    private _token: CancellationToken;

    constructor(parseResults: ParseResults, evaluator: TypeEvaluator, token: CancellationToken) {
        super();
        this._parseResults = parseResults;
        this._evaluator = evaluator;
        this._token = token;
    }

    override build(): SemanticTokens {
        const parseTreeWalker = new SemanticTokensWalker(this);
        parseTreeWalker.walk(this._parseResults.parseTree);
        return super.build();
    }

    getEvaluator() {
        return this._evaluator;
    }

    getParseResults() {
        return this._parseResults;
    }

    getCancellationToken() {
        return this._token;
    }

    getPosition(range: TextRange) {
        return convertOffsetToPosition(range.start, this._parseResults.tokenizerOutput.lines);
    }

    pushRange(range: TextRange, legendType: LegendTypeType, legendModifiers?: LegendModType) {
        const pos = this.getPosition(range);
        this.push(pos.line, pos.character, range.length, encodeType(legendType), encodeModifiers(legendModifiers));
    }
}

class SemanticTokensWalker extends ParseTreeWalker {
    private _builder: CythonSemanticTokensBuilder;

    constructor(builder: CythonSemanticTokensBuilder) {
        super();
        this._builder = builder;
    }

    private get _evaluator() {
        return this._builder.getEvaluator();
    }

    private get _cancellationToken() {
        return this._builder.getCancellationToken();
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
        if (node.expression.nodeType === ParseNodeType.CCallback) {
            this.walk(node.expression);
        } else {
            this.walkMultiple([node.expression, node.name]);
        }
        return false;
    }

    override visitCCallback(node: CCallbackNode): boolean {
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
        if (node.typeAnnotation && node.typeAnnotation.nodeType === ParseNodeType.CCallback) {
            const annotation = node.typeAnnotation;
            this.walkMultiple([annotation.returnTypeAnnotation]);
            this.pushRange(annotation.name, LegendType.Parameter);
            this.walkMultiple(annotation.parameters);
            return false;
        }
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

    override visitParameter(node: ParameterNode): boolean {
        if (node.isCythonLike) {
            this.walkMultiple([node.typeAnnotation, node.name, node.defaultValue]);
            return false;
        }
        return true;
    }
}
