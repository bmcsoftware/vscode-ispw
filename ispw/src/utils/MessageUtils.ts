/**
* THESE MATERIALS CONTAIN CONFIDENTIAL INFORMATION AND TRADE SECRETS OF BMC SOFTWARE, INC. YOU SHALL MAINTAIN THE MATERIALS AS
* CONFIDENTIAL AND SHALL NOT DISCLOSE ITS CONTENTS TO ANY THIRD PARTY EXCEPT AS MAY BE REQUIRED BY LAW OR REGULATION. USE,
* DISCLOSURE, OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF BMC SOFTWARE, INC.
*
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
*/
import * as vscode from "vscode";

/**
 * Utility namespace to provide functions for showing message boxes.
 */
export namespace MessageUtils {

    /**
     * Shows an info box with the given message.
     * @param message The string message to show.
     */
    export function showInfoMessage(message: string) {
        vscode.window.showInformationMessage(message);
    }

    /**
     * Shows a warning box with the given message.
     * @param message The string message to show.
     */
    export function showWarningMessage(message: string) {
        vscode.window.showWarningMessage(message);
    }

    /**
     * Shows an error box with the given message.
     * @param message The string message to show.
     */
    export function showErrorMessage(message: string) {
        vscode.window.showErrorMessage(message);
    }

}