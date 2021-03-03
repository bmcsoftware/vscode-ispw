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
                    { label: 'Find...', description: 'Browse your file system to locate a Topaz CLI home location' },
                ],
                {
                    placeHolder: 'Select Topaz CLI location'
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
}