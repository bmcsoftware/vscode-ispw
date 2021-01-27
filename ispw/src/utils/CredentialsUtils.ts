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
import { Credentials } from "../types/CredentialsCache";

/**
 * A utility namespace for functions related to credential management.
 */
export namespace CredentialsUtils {

    /**
     * Shows a series of input boxes to get the username and password from the user.
     * The return Credentials object will have either the username, password, or both undefined if the user cancels the operation.
     * The password will not be encrypted.
     */
    export async function promptForCredentials(): Promise<Credentials> {
        let credentials: Credentials = { username: undefined, password: undefined };
        // prompt for username
        await vscode.window.showInputBox({
            prompt: "Enter username"
        }).then(e => {
            if (e !== undefined) {
                credentials.username = e;
            }
        });

        if (credentials.username) {
            // if a username was entered, prompt for password
            await vscode.window.showInputBox({
                prompt: "Enter password",
                password: true
            }).then(e => {
                if (e !== undefined) {
                    credentials.password = e;
                }
            });
        }
        return credentials;
    }

    /**
     * Validates the given credentials object to make sure there is both the username and password defined.
     * @param credentials The credentials to validate
     */
    export function validateCredentials(credentials: Credentials) {
        return credentials.username !== undefined && credentials.password !== undefined;
    }
}