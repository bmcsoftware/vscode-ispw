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
import fs = require('fs');
import yaml = require('js-yaml');
import * as path from 'path';
import { IspwRoot } from '../types/IspwTypeMapping';
import { IspwApplication } from '../types/IspwTypeMapping';
import { IspwPath } from '../types/IspwTypeMapping';
import { IspwType } from '../types/IspwTypeMapping';
import * as vscode from "vscode";

/**
 * Utility namespace for functions related to yaml processing
 */
export namespace YamlUtils {

    /**
     * Checks whether the yaml location is defined in the settings and whether the file exists on the file system.
     * If the location is not defined in settings, the path is defaulted to an ispwconfig.yml file at the root of the project.
     * This method will return false if the path is defined but the file does not exist locally.
     * @param selectedFile The file selected to find an associated yaml for
     */
    export function hasYaml(selectedFile: vscode.Uri): boolean {
        let yamlLocation: string = getYamlLocationAbsPath(selectedFile);
        return fs.existsSync(yamlLocation);
    }

    /**
     * Checks whether a yaml file is defined, loads the yaml and tries to find an ISPW type that matches the selectedFile.
     * This method will return true if a type is found, false if not type is found.
     * @param selectedFile 
     */
    export function doesYamlHaveType(selectedFile: vscode.Uri): boolean {
        let hasType: boolean = hasYaml(selectedFile);

        if (hasType) {
            let returnType: IspwType | undefined = getTypeFromYaml(selectedFile);
            hasType = returnType !== undefined;
        }
        return hasType;
    }

    export function getTypeFromYaml(selectedFile: vscode.Uri): IspwType | undefined {
        let mostPathCount: number = 0;
        let returnType: IspwType | undefined = undefined;
        let ispwRoot: IspwRoot | undefined = loadYaml(selectedFile);
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
     * @param selectedFile The file to get the associated yaml for.
     */
    export function getYamlLocationAbsPath(selectedFile: vscode.Uri): string {
        let yamlLocation: string = vscode.workspace.getConfiguration('ispw.yamlLocation', selectedFile).get<string>('ispw.yamlLocation', vscode.workspace.getWorkspaceFolder(selectedFile)?.uri.fsPath + "\\ispwconfig.yml");
        if (!path.isAbsolute(yamlLocation)) {
            yamlLocation = vscode.workspace.getWorkspaceFolder(selectedFile)?.uri.fsPath + yamlLocation;
        }
        console.log("getYamlLocationAbsPath: " + yamlLocation);
        return yamlLocation;
    }

    /**
     * Reads the yaml path from the workspace settings and returns the relative path to the yaml if it can be found. 
     * If no path is defined in the settings, the path is defaulted to a file named ispwconfig.yml at the root of the workspace folder.
     * The path returned will be relative to the workspace folder if it is in the folder, otherwise the absolute path will be returned.
     * @param selectedFile The file to get the associated yaml for.
     */
    export function getYamlLocationRelPath(selectedFile: vscode.Uri): string {
        let yamlLocationUri: string = getYamlLocationAbsPath(selectedFile);
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
    export function loadYaml(selectedFile: vscode.Uri): IspwRoot | undefined {
        let ispwRoot: IspwRoot | undefined = undefined;
        let yamlLocation: string = getYamlLocationAbsPath(selectedFile);
        let loadedData = yaml.load(fs.readFileSync(yamlLocation, 'utf8'));
        console.debug(loadedData);
        if (typeof loadedData !== 'object') { throw new Error(yamlLocation + ' does not contain valid yaml'); }
        ispwRoot = (loadedData as unknown) as IspwRoot;
        return ispwRoot;
    }
}