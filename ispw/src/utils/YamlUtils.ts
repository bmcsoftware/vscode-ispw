/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

import fs = require('fs');
import yaml = require('js-yaml');
import * as path from 'path';
import { IspwRoot } from '../types/IspwTypeMapping';
import { IspwPath } from '../types/IspwTypeMapping';
import { IspwType } from '../types/IspwTypeMapping';
import { Constants } from '../utils/Constants';
import { CommonUtils } from '../utils/CommonUtils';
import * as vscode from "vscode";

/**
 * Utility namespace for functions related to yaml processing
 */
export namespace YamlUtils {

    /**
     * Checks whether the yaml location is defined in the settings and whether the file exists on the file system.
     * If the location is not defined in settings, the path is defaulted to an ispwconfig.yml file at the root of the project.
     * This method will return false if the path is defined but the file does not exist locally.
     * 
     * @param prompt if true and config yaml cannot be found, we will popup to let user select yaml config location 
     * @param selectedFile The file selected to find an associated yaml for
     */
    export async function hasYaml(prompt: boolean = false, selectedFile: vscode.Uri): Promise<boolean> {
        let yamlLocation: string = Constants.EMPTY_STRING;

        if (prompt) {
            yamlLocation = await getYamlLocationAbsPathWithPrompt(selectedFile);
        } else {
            yamlLocation = await getYamlLocationAbsPath(selectedFile);
        }

        return fs.existsSync(yamlLocation);
    }

    /**
     * Checks whether a yaml file is defined, loads the yaml and tries to find an ISPW type that matches the selectedFile.
     * This method will return true if a type is found, false if not type is found.
     * @param selectedFile 
     * @deprecated
     */
    export async function doesYamlHaveType(selectedFile: vscode.Uri): Promise<boolean> {
        let hasType: boolean = await hasYaml(false, selectedFile);

        if (hasType) {
            let returnType: IspwType | undefined = await getTypeFromYaml(selectedFile);
            hasType = returnType !== undefined;
        }
        return hasType;
    }

    /**
     * Get type from yaml
     *
     * @param selectedFile 
     * @deprecated
     */
    export async function getTypeFromYaml(selectedFile: vscode.Uri): Promise<IspwType | undefined> {
        let mostPathCount: number = 0;
        let returnType: IspwType | undefined = undefined;
        let ispwRoot: IspwRoot | undefined = await loadYaml(selectedFile);
        if (ispwRoot) {
            let parentFolder: string = path.normalize(path.dirname(selectedFile.fsPath));
            let pathMappings: IspwPath[] = ispwRoot.ispwApplication.pathMappings;
            pathMappings.forEach(pathMap => {
                let normalizedPathMap: string = path.normalize(pathMap.path);
                if (normalizedPathMap.endsWith("\\") || normalizedPathMap.endsWith("/")) {
                    normalizedPathMap = normalizedPathMap.substring(0, normalizedPathMap.length - 1);
                }

                if (normalizedPathMap === parentFolder) {
                    pathMap.types.forEach(typeMap => {
                        let pathCount: number = (normalizedPathMap.replace("\\", "/").match('\/') || []).length;
                        if (mostPathCount < pathCount) {
                            mostPathCount = pathCount;
                            returnType = typeMap;
                        }
                    });
                }
            });
        }
        return returnType;
    }

    /**
     * Reads the yaml path from the workspace settings and returns the absolute path to the yaml. 
     * If no path is defined in the settings, the path is defaulted to a file named ispwconfig.yml at the root of the workspace folder.
     * 
     * @param selectedFile The file to get the associated yaml for.
     */
    export async function getYamlLocationAbsPath(selectedFile: vscode.Uri): Promise<string> {
        let workspaceFolder = vscode.workspace.getWorkspaceFolder(selectedFile);
        let workspaceLoc = vscode.workspace.getWorkspaceFolder(selectedFile)?.uri.fsPath || Constants.EMPTY_STRING;

        let defaultYamlLoc: string = workspaceLoc + path.sep + Constants.ISPW_CONFIG_YAML;
        let yamlLocation: string = vscode.workspace.getConfiguration(Constants.EMPTY_STRING, workspaceFolder).get<string>(Constants.SETTING_KEY_YAML_LOC, Constants.EMPTY_STRING);
        if (CommonUtils.isBlank(yamlLocation)) {
            yamlLocation = defaultYamlLoc;
            await vscode.workspace.getConfiguration(Constants.EMPTY_STRING, workspaceFolder).update(Constants.SETTING_KEY_YAML_LOC,
                "ispwconfig.yml", vscode.ConfigurationTarget.WorkspaceFolder);
        }

        if (!path.isAbsolute(yamlLocation)) {
            //yaml location is alraedy defined
            if (yamlLocation.startsWith(path.sep)) {
                yamlLocation = workspaceLoc + yamlLocation;
            } else {
                yamlLocation = workspaceLoc + path.sep + yamlLocation;
            }
        }

        console.log("getYamlLocationAbsPath: " + yamlLocation);

        return yamlLocation;
    }

    /**
     * Reads the yaml path from the workspace settings and returns the absolute path to the yaml. 
     * If no path is defined in the settings, the path is defaulted to a file named ispwconfig.yml at the root of the workspace folder.
     * 
     * If the yaml config doesn't exist, we will prompt to ask user to select a valid yaml config
     * 
     * @param selectedFile The file to get the associated yaml for.
     */
    export async function getYamlLocationAbsPathWithPrompt(selectedFile: vscode.Uri): Promise<string> {
        let workspaceFolder = vscode.workspace.getWorkspaceFolder(selectedFile);
        let workspaceUri = vscode.workspace.getWorkspaceFolder(selectedFile)?.uri;
        let workspaceLoc = vscode.workspace.getWorkspaceFolder(selectedFile)?.uri.fsPath || Constants.EMPTY_STRING;

        let yamlLocation = await getYamlLocationAbsPath(selectedFile);
        if (!fs.existsSync(yamlLocation)) {

            //if the YAML config does NOT exist, prompt for a selection
            let selection = await vscode.window.showQuickPick(
                [
                    { 
                        label: 'Find...', 
                        description: 'Browse your file system for the YAML mapping file' 
                    },
                ],
                {
                    placeHolder: 'Select ISPW YAML Mapping File'
                });

            if (selection?.label === 'Find...') {
                const options: vscode.OpenDialogOptions = {
                    canSelectFiles: true,
                    canSelectFolders: false,
                    filters: {
                        'ISPW YAML Config': ['yml']
                    },
                    defaultUri: workspaceUri
                };

                let configSelected = await vscode.window.showOpenDialog(options);
                if (configSelected) {
                    yamlLocation = configSelected[0].fsPath;

                    let relSelectedYamlLoc: string = path.relative(workspaceLoc, yamlLocation);
                    await vscode.workspace.getConfiguration(Constants.EMPTY_STRING, workspaceFolder).update(Constants.SETTING_KEY_YAML_LOC,
                        relSelectedYamlLoc, vscode.ConfigurationTarget.WorkspaceFolder);
                }
            }
        }

        console.log("getYamlLocationAbsPath with prompt: " + yamlLocation);
        return yamlLocation;
    }

    /**
     * Reads the yaml path from the workspace settings and returns the relative path to the yaml if it can be found. 
     * If no path is defined in the settings, the path is defaulted to a file named ispwconfig.yml at the root of the workspace folder.
     * The path returned will be relative to the workspace folder if it is in the folder, otherwise the absolute path will be returned.
     * @param selectedFile The file to get the associated yaml for.
     */
    export async function getYamlLocationRelPath(selectedFile: vscode.Uri): Promise<string> {
        let yamlLocationUri: string = await getYamlLocationAbsPath(selectedFile);
        let yamlLocation = vscode.workspace.asRelativePath(yamlLocationUri);
        console.log("getYamlLocationRelPath: " + yamlLocation);
        return yamlLocation;
    }

    /**
     * Retrieves the yaml file location and reads in the contents as an IspwRoot object.
     * This function will throw an error if the yaml file does not contain valid yaml.
     * This function assumes that the yaml file exists! hasYaml should be called before this function.
     * @param selectedFile The file to get the associated yaml for.
     */
    export async function loadYaml(selectedFile: vscode.Uri): Promise<IspwRoot | undefined> {
        let ispwRoot: IspwRoot | undefined = undefined;
        let yamlLocation: string = await getYamlLocationAbsPath(selectedFile);
        let loadedData = yaml.load(fs.readFileSync(yamlLocation, 'utf8').replace("!!com.compuware.ispw.cli.model.IspwRoot",""));
        console.debug(loadedData);
        if(typeof loadedData !== 'object') { throw new Error(yamlLocation + ' does not contain valid yaml'); }
        ispwRoot = (loadedData as unknown) as IspwRoot;
        return ispwRoot;
    }

}