/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

import * as vscode from "vscode";
import fs = require('fs');
import { Constants } from '../utils/Constants';
import { CommonUtils } from '../utils/CommonUtils';

export namespace SettingsUtils {

    export async function getLoadLevelWithPrompt(): Promise<string | undefined> {

        let loadLevel = getLoadLevel();

        if (CommonUtils.isBlank(loadLevel)) {
            await vscode.window.showInputBox({
                prompt: "Enter load level",
                placeHolder: "DEV1",
                value: Constants.EMPTY_STRING
            }).then(async newLoadLevel => {
                if (CommonUtils.isNotBlank(newLoadLevel)) {
                    await vscode.workspace.getConfiguration().update(Constants.SETTING_KEY_LOAD_LEVEL, newLoadLevel, vscode.ConfigurationTarget.Global);
                    loadLevel = newLoadLevel;
                }
            });
        }

        return loadLevel;
    }

    export async function getCliLocationWithPrompt(): Promise<string | undefined> {
        let cliLocation = getCliLocation() || Constants.EMPTY_STRING;

        if (CommonUtils.isBlank(cliLocation) || !fs.existsSync(cliLocation)) {
            await vscode.window.showQuickPick(
                [
                    { 
                        label: 'Find...', 
                        description: 'Browse your file system to locate the Workbench CLI installation folder' 
                    },
                ],
                {
                    placeHolder: 'Select Workbench CLI Installation Path'
                }).then(async selection => {
                    if (!selection) {
                        return;
                    }

                    switch (selection.label) {
                        case 'Find...':
                            const options: vscode.OpenDialogOptions = {
                                canSelectFiles: false,
                                canSelectFolders: true
                            };
                            await vscode.window.showOpenDialog(options).then(async selection => {
                                if (selection) {
                                    cliLocation = selection[0].fsPath;
                                    
                                    if (CommonUtils.isNotBlank(cliLocation)) {
                                        await vscode.workspace.getConfiguration().update(Constants.SETTING_KEY_CLI_LOC, cliLocation, vscode.ConfigurationTarget.Global);
                                    }
                                }
                            });

                            break;

                        default:
                            break;
                    }
                });
        }

        return cliLocation;
    }

    export async function getAssignmentDescriptionWithPrompt(): Promise<string | undefined> {
        let assignDesc = getAssignmentDescription();

        if (CommonUtils.isBlank(assignDesc)) {
            await vscode.window.showInputBox({
                prompt: "Enter assignment description",
                placeHolder: "{user}-{project_name}",
                value: "{user}-{project_name}"
            }).then(async newAssignDesc => {
                if (CommonUtils.isNotBlank(newAssignDesc)) {
                    await vscode.workspace.getConfiguration().update(Constants.SETTING_KEY_ASSIGN_DESC, newAssignDesc, vscode.ConfigurationTarget.Global);
                    assignDesc = newAssignDesc;
                }
            });
        }

        return assignDesc;
    }

    export async function getCesUrlWithPrompt(): Promise<string | undefined> {
        let cesUrl = getCesUrl();

        if (CommonUtils.isBlank(cesUrl)) {
            await vscode.window.showInputBox(
                {
                    prompt: 'Enter CES URL'
                }).then(async function (response) {
                    await vscode.workspace.getConfiguration().update(Constants.SETTING_KEY_CES_URL, response, vscode.ConfigurationTarget.Global);
                    cesUrl = response;
                });
        }
        return cesUrl;
    }

    
    export async function getCesTokenWithPrompt(): Promise<string | undefined> {
        let cesToken = getCesToken();

        if (CommonUtils.isBlank(cesToken)) {
            await vscode.window.showInputBox(
                {
                    prompt: 'Enter CES Token'
                }).then(async function (response) {
                    await vscode.workspace.getConfiguration().update(Constants.SETTING_KEY_CES_TOKEN, response, vscode.ConfigurationTarget.Global);
                    cesToken = response;
                });
        }
        return cesToken;
    }

    /**
     * Gets the CLI location saved in the User Settings. This may or may not be defined.
     */
    export function getCliLocation(): string | undefined {
        let cliLocation: string | undefined = vscode.workspace.getConfiguration().get<string>(Constants.SETTING_KEY_CLI_LOC);
        console.debug("CLI location: " + cliLocation);
        
        return cliLocation;
    }

    /**
     * Gets the build level stored in the Resource Settings. This may or may not be defined.
     */
    export function getLoadLevel(): string | undefined {
        let loadLevel: string | undefined = vscode.workspace.getConfiguration().get<string>(Constants.SETTING_KEY_LOAD_LEVEL);

        return loadLevel;
    }

    /**
     * Gets the assignment description stored in the Resource Settings. This may or may not be defined.
     */
    export function getAssignmentDescription(): string | undefined {
        let assignmentDescription: string | undefined = vscode.workspace.getConfiguration().get<string>(Constants.SETTING_KEY_ASSIGN_DESC);

        return assignmentDescription;
    }

    /**
     * Gets the CES URL
     * @returns 
     */
    export function getCesUrl(): string | undefined {
        let cesUrl: string | undefined = vscode.workspace.getConfiguration().get<string>(Constants.SETTING_KEY_CES_URL);

        return cesUrl;
    }

    /**
     * Gets the CES Token
     * @returns 
     */
    export function getCesToken(): string | undefined {
        let cesToken: string | undefined = vscode.workspace.getConfiguration().get<string>(Constants.SETTING_KEY_CES_TOKEN);

        return cesToken;
    }
}