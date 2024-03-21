/*
 * cythonTransform.ts
 *
 * Python/Cython type conversions
 *
 */
import { CStructType, CTrailType, ParseNode } from '../parser/parseNodes';
import { getEnclosingModule } from './parseTreeUtils';
import { AnyType, ClassType, isAnyOrUnknown, isClass, isFunction, Type, TypeBase, UnknownType } from './types';

const cythonBuiltins = 'cython_builtins';
const cythonModules = ['cython', cythonBuiltins];
const cythonInteger = '__CythonInteger__';
const cythonCpp = 'libcpp';

type getBuiltInType = (node: ParseNode, name: string) => Type;

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

const toPythonTypeMapCpp: Map<string, string> = new Map([
    [`${cythonCpp}.string.string`, 'bytes'],
    [`${cythonCpp}.vector.vector`, 'list'],
    [`${cythonCpp}.utility.pair`, 'tuple'],
    [`${cythonCpp}.map.map`, 'dict'],
    [`${cythonCpp}.unordered_map.unordered_map`, 'dict'],
    [`${cythonCpp}.set.set`, 'set'],
    [`${cythonCpp}.list.list`, 'list'],
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

export function isCythonCpp(type: Type) {
    return isClass(type) && type.details.structType === CStructType.CppClass;
}

export function isBuiltInCythonCpp(type: Type) {
    if (isCythonCpp(type) && isClass(type)) {
        const moduleParts = type.details.moduleName.split('.');
        if (moduleParts.length > 0) {
            return moduleParts[0] === cythonCpp;
        }
    }
    return false;
}

function transformCythonCppToPython(
    getBuiltInType: getBuiltInType,
    node: ParseNode,
    type: ClassType,
    memberAccess = false
) {
    const pyName = toPythonTypeMapCpp.get(type.details.fullName);
    if (pyName) {
        let newType = TypeBase.cloneType(getBuiltInType(node, pyName));
        if (!isAnyOrUnknown(newType) && isClass(newType)) {
            if (newType.details.typeParameters && type.typeArguments) {
                const typeArgs: Type[] = [];
                for (let i = 0; i < type.details.typeParameters.length && i < type.typeArguments.length; i++) {
                    typeArgs.push(transformCythonToPython(getBuiltInType, node, type.typeArguments[i]));
                }
                newType = ClassType.cloneForSpecialization(newType, typeArgs, true);
            }
            type = ClassType.cloneAsInstance(newType);
        }
    }
    return type;
}

// Transform Cython type to Python.
export function transformCythonToPython(
    getBuiltInType: getBuiltInType,
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
        const isCythonInt = type.details.baseClasses.find(
            (base) => isClass(base) && base.details.fullName === `${cythonBuiltins}.${cythonInteger}`
        );
        if (isCythonInt) {
            // This is a basic c integer type
            pyName = 'int';
        } else if (isCythonCpp(type) && isClass(type)) {
            return transformCythonCppToPython(getBuiltInType, node, type, memberAccess);
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
