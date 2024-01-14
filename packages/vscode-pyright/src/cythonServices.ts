import { ExtensionContext } from 'vscode';

import { StatusBar } from './statusBar';

// Additional misc service objects for Cython
export interface CythonServices {
    statusBar: StatusBar;
}

export namespace CythonServices {
    export function create(context: ExtensionContext) {
        const services: CythonServices = {
            statusBar: new StatusBar(context),
        };
        return services;
    }
}
