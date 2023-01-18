/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2022 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

import * as vscode from "vscode";
import { CommonUtils } from "./CommonUtils";
import { Constants } from "./Constants";

/**
 * Utility namespace for all validations.
 */
export namespace ValidatorUtils {

  export async function validateCESUrlWithPrompt(extensionUri: vscode.Uri): Promise<boolean> {
    const isAValidCESUrl = isValidCESUrl(extensionUri);
    if (!isAValidCESUrl) {
      await vscode.window.showInputBox(
        {
          prompt: 'Enter CES URL'
        }).then(function (response) {
          vscode.workspace.getConfiguration().update(Constants.SETTING_KEY_CES_URL, response, vscode.ConfigurationTarget.Global);
          return true;
        });
      return false;
    }
    return true;
  }

  export async function validateCESTokenWithPrompt(extensionUri: vscode.Uri): Promise<boolean> {
    const isAValidCESToken = isValidCESToken(extensionUri);
    if (!isAValidCESToken) {
      await vscode.window.showInputBox(
        {
          prompt: 'Enter CES Token'
        }).then(function (response) {
          vscode.workspace.getConfiguration().update(Constants.SETTING_KEY_CES_TOKEN, response, vscode.ConfigurationTarget.Global);
          return true;
        });
      return false;
    }
    return true;
  }

    export function isValidCESUrl(extensionUri: vscode.Uri): boolean {
        let workspaceFolder = vscode.workspace.getWorkspaceFolder(extensionUri);
        let cesUrl: string = vscode.workspace.getConfiguration(Constants.EMPTY_STRING, workspaceFolder).get<string>(Constants.SETTING_KEY_CES_URL, Constants.EMPTY_STRING);
        if (CommonUtils.isBlank(cesUrl)) {
            return false;
        }

        return true;
    }

    export function isValidCESToken(extensionUri: vscode.Uri): boolean {
        let workspaceFolder = vscode.workspace.getWorkspaceFolder(extensionUri);
        let cesToken: string = vscode.workspace.getConfiguration(Constants.EMPTY_STRING, workspaceFolder).get<string>(Constants.SETTING_KEY_CES_TOKEN, Constants.EMPTY_STRING);
        if (CommonUtils.isBlank(cesToken)) {
            return false;
        }

        return true;
    }

}

