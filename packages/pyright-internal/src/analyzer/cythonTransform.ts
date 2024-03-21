/*
 * cythonTransform.ts
 *
 * Python/Cython type conversions
 *
 */
import { CStructType, CTrailType, ParseNode } from '../parser/parseNodes';
import { getEnclosingModule } from './parseTreeUtils';
import { AnyType, ClassType, isClass, isFunction, Type, TypeBase, UnknownType } from './types';

const cython_builtins = 'cython_builtins';
const cythonModules = ['cython', cython_builtins];

// C Types that a Python Type can be converted into
export const validFromPythonTypeMap: Map<string, string[]> = new Map([
    ['bool', ['bint']],
    ['int', ['char', 'short', 'long', 'float', 'double']],
    ['float', ['float', 'double']],
    ['str', ['char*']],
    ['bytes', ['char*']],
]);

// Automatic type conversion to Python
const toPythonTypeMap: Map<string, string> = new Map([
    ['bint', 'bool'],
    ['char', 'int'],
    ['short', 'int'],
    ['long', 'int'],
    ['double', 'float'],
    ['char*', 'bytes'],
    ['struct', 'dict'],
    ['union', 'dict'],
    ['array', 'list'],
    // Also want to transform these so modifiers/specifiers are removed
    ['int', 'int'],
    ['float', 'float'],
]);

export function isCythonBuiltIn(type: Type) {
    if (isFunction(type) || isClass(type)) {
        if (cythonModules.includes(type.details.moduleName.split('.')[0])) {
            return true;
        }
    }
    return false;
}

export function isCythonFunction(type: Type) {
    if (isFunction(type) && isCythonBuiltIn(type)) {
        return true;
    }
    return false;
}

export function isCythonType(type: Type) {
    return !!type.cythonDetails;
}

// Transform Cython type to Python.
export function transformCythonToPython(
    getBuiltInType: (node: ParseNode, name: string) => Type,
    node: ParseNode,
    type: Type,
    memberAccess = false
): Type {
    if (!isClass(type) || !(isCythonBuiltIn(type) || isCythonType(type))) {
        return type;
    }
    const name = type.details.name;
    let key = name;
    if (type.cythonDetails?.isPointer && type.cythonDetails?.trailType !== CTrailType.Array) {
        if (name === 'char' && type.cythonDetails.ptrRefCount === 1) {
            key = 'char*';
        } else {
            key = '';
        }
    } else {
        if (type.cythonDetails?.structType === CStructType.Struct) {
            key = 'struct';
        } else if (type.cythonDetails?.trailType === CTrailType.Array) {
            key = 'array';
        } else if (type.cythonDetails?.structType === CStructType.Union && memberAccess) {
            // TODO: Check this
            // ! Unions don't seem to transform with member access
            key = 'union';
        }
    }

    let pyName = toPythonTypeMap.get(key);
    if (!pyName) {
        if (
            type.details.baseClasses.find(
                (base) => isClass(base) && base.details.fullName === `${cython_builtins}.__CythonInteger__`
            )
        ) {
            // This is a basic c integer type
            pyName = 'int';
        }
    }
    if (!pyName) {
        return UnknownType.create();
    }
    const modNode = getEnclosingModule(node);
    let pyType = getBuiltInType(modNode, pyName);

    if (isClass(pyType)) {
        switch (pyName) {
            case 'dict':
                pyType = ClassType.cloneForSpecialization(
                    pyType,
                    [TypeBase.cloneTypeAsInstance(getBuiltInType(modNode, 'str')), AnyType.create()],
                    true
                );
                break;
            case 'list':
                {
                    type = TypeBase.cloneType(type);
                    if (type.cythonDetails) {
                        if (type.cythonDetails.isPointer) {
                            // Cannot make a list of pointers
                            return UnknownType.create();
                        }
                        if (type.cythonDetails.ndims) {
                            type.cythonDetails.ndims--;
                        }
                        if (!type.cythonDetails.ndims) {
                            type.cythonDetails.trailType = undefined;
                        }
                    }
                    // Transform the containing type to python as well
                    const typeArg = transformCythonToPython(getBuiltInType, node, type, memberAccess);
                    pyType = ClassType.cloneForSpecialization(pyType, [typeArg], true);
                }
                break;
            default:
                break;
        }
    }
    return TypeBase.cloneTypeAsInstance(pyType);
}
