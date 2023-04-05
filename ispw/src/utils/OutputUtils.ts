/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

import * as vscode from "vscode";

/**
 * Utility namespace for functions related to VSCode output channels
 */
export namespace OutputUtils {

    /**
     * Returns the instance of the Code Pipeline for Git output channel. If no output channel exists, one is created and it is shown.
     */
    let _channel: vscode.OutputChannel;
    export function getOutputChannel(): vscode.OutputChannel {
        if (!_channel) {
            _channel = vscode.window.createOutputChannel("Workbench Code Pipeline for Git");
            _channel.show(true);
        }
        return _channel;
    }
}