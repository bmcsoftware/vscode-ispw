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

import { CliArgs } from "../types/CliArgs";
import * as cp from "child_process";
import * as path from 'path';
import { OutputUtils } from "../Utils/OutputUtils";
import { MessageUtils } from "./MessageUtils";
import { CredentialsUtils } from "./CredentialsUtils";
import * as vscode from "vscode";
import { YamlUtils } from "./YamlUtils";
import { Credentials, CredentialsCache } from "../types/CredentialsCache";

/**
 * Utility namespace for CLI operations.
 */
export namespace CliUtils {

  /**
   * Asynchronous function to assemble all the arguments for the CLI and call the CLI.
   * @param operation The ISPW operation to be executed ('build', 'generate', 'load')
   * @param selectedFiles The files selected to run the operation on
   */
  export async function runCliCommandForOperation(operation: string, selectedFiles: vscode.Uri[]) {
    console.debug("Starting runCliCommandForOperation for operation " + operation);
    if (!validateSettings(selectedFiles)) { return; }

    let yamlLocation: string = YamlUtils.getYamlLocationRelPath(selectedFiles[0]);
    let credCache = CredentialsCache.getInstance();
    let credentials: Credentials = credCache.getCredentials();
    if (!CredentialsUtils.validateCredentials(credentials)) {
      credentials = await CredentialsUtils.promptForCredentials();
      if (CredentialsUtils.validateCredentials(credentials)) {
        credCache.saveCredentials(credentials.username, credentials.password);
      }
      else {
        console.debug("User cancelled the operation while entering credentials");
        return;
      }

    }

    // assemble the arguments
    let cliLocation: string = getCliLocation() || '';
    let args: string[] = createCommandLineArgs({
      operation: operation,
      ispwConfigPath: yamlLocation,
      username: credentials.username || '',
      password: credentials.password || '',
      componentFiles: getComponentFileNames(selectedFiles),
      ispwMappingLevel: getLoadLevel() || '',
      ispwGitAssignDesc: getAssignmentDescription() || ''
    });

    executeCliCommand(cliLocation + '\\IspwCLI.bat', args, selectedFiles);

  }

  /**
   * Creates a child process and calls the CLI
   * @param command The string CLI command to execute (the path to the ISpwCLI.bat file)
   * @param args The arguments passed to the CLI (operation, username, password, componentFiles, etc)
   * @param selectedFiles The files selected to run ISPW action against
   */
  function executeCliCommand(command: string, args: string[], selectedFiles: vscode.Uri[]): void {
    let operationToShow: string = args[args.indexOf(' -operation ') + 1];
    let fileNameToShow: string = selectedFiles.length === 1 ? path.basename(selectedFiles[0].fsPath) : selectedFiles.length + " files";

    // The spawn function runs asynchronously so that the CLI output is immediately written to the output stream.
    const child = cp.spawn(command, args, {
      shell: true,
      cwd: vscode.workspace.getWorkspaceFolder(selectedFiles[0])?.uri.fsPath
    });

    // add listener for when data is written to stdout
    child.stdout.on('data', (stdout) => {
      OutputUtils.getOutputChannel().append(stdout.toString());
    });

    // add listener for when data is written to stderr
    child.stderr.on('data', (stderr) => {
      OutputUtils.getOutputChannel().append(stderr.toString());
    });

    // add listener for when the CLI process completes
    child.on('close', code => {
      if (code === 0) {
        // pass
        MessageUtils.showInfoMessage("The " + operationToShow + " process completed for " + fileNameToShow);
      }
      else {
        // fail
        MessageUtils.showErrorMessage("The " + operationToShow + " process failed for " + fileNameToShow + ". Check the ISPW Output channel for more information.");
      }
    });
  }

  /**
    * Concatenates the given URIs into a string of relative paths separated by a ":"
    * @param selectedFiles The selected file URIs to get paths for
    */
  function getComponentFileNames(selectedFiles: vscode.Uri[]): string {
    let componentNameStr: string = "";
    selectedFiles.forEach(componentUri => {
      componentNameStr = componentNameStr + ":" + vscode.workspace.asRelativePath(componentUri);
    });
    componentNameStr = componentNameStr.substring(1);

    return componentNameStr;
  }

  /**
   * Gets the CLI location saved in the User Settings. This may or may not be defined.
   */
  export function getCliLocation(): string | undefined {
    let cliLocation: string | undefined = vscode.workspace.getConfiguration().get<string>('ispw.Topaz CLI Installation Path');
    console.debug("CLI location: " + cliLocation);
    return cliLocation;
  }

  /**
   * Gets the build level stored in the Resource Settings. This may or may not be defined.
   */
  export function getLoadLevel(): string | undefined {
    let loadLevel: string | undefined = vscode.workspace.getConfiguration().get<string>('ispw.Level');

    return loadLevel;
  }

  /**
   * Gets the assignment description stored in the Resource Settings. This may or may not be defined.
   */
  export function getAssignmentDescription(): string | undefined {
    let assignmentDescription: string | undefined = vscode.workspace.getConfiguration().get<string>('ispw.Assignment Description');

    return assignmentDescription;
  }

  /**
   * Accepts an object of type CliArgs. Processes each field in the args object and appends a string argument to a string[]. Returns the final string[].
   * The final string[] can be passed to the CLI.
   * @param args a CliArgs object that needs to be broken down into a string array. Fields will only be appended if they have values.
   */
  function createCommandLineArgs(args: CliArgs): string[] {
    let strArgs: Array<string> = [];
    if (args.codePage) { strArgs = strArgs.concat([' -code ', args.codePage]); }
    if (args.componentFiles) { strArgs = strArgs.concat([' -componentFiles ', '"' + args.componentFiles + '"']); }
    if (args.gitBranch) { strArgs = strArgs.concat([' -gitBranch ', args.gitBranch]); }
    if (args.gitCommit) { strArgs = strArgs.concat([' -gitCommit ', args.gitCommit]); }
    if (args.gitCommitFile) { strArgs = strArgs.concat([' -gitCommitFile ', args.gitCommitFile]); }
    if (args.gitFromHash) { strArgs = strArgs.concat([' -gitFromHash ', args.gitFromHash]); }
    if (args.gitLocalPath) { strArgs = strArgs.concat([' -gitLocalPath ', args.gitLocalPath]); }
    if (args.gitPassword) { strArgs = strArgs.concat([' -gitPassword ', args.gitPassword]); }
    if (args.gitRepoUrl) { strArgs = strArgs.concat([' -gitRepoUrl ', args.gitRepoUrl]); }
    if (args.gitUsername) { strArgs = strArgs.concat([' -gitUsername ', args.gitUsername]); }
    if (args.host) { strArgs = strArgs.concat([' -host ', args.host]); }
    if (args.username) { strArgs = strArgs.concat([' -id ', args.username]); }
    if (args.ispwGitAssignDesc) { strArgs = strArgs.concat([' -ispwAssignDesc ', args.ispwGitAssignDesc]); }
    if (args.checkoutLevel) { strArgs = strArgs.concat([' -ispwCheckoutLevel ', args.checkoutLevel]); }
    if (args.ispwConfigPath) { strArgs = strArgs.concat([' -ispwConfigPath ', '"' + args.ispwConfigPath + '"']); }
    if (args.containerCreation) { strArgs = strArgs.concat([' -ispwContainerCreation ', args.containerCreation]); }
    if (args.customDescription) { strArgs = strArgs.concat([' -ispwContainerDescription ', '"' + args.customDescription + '"']); }
    if (args.ispwMappingLevel) { strArgs = strArgs.concat([' -ispwMappingLevel ', args.ispwMappingLevel]); }
    if (args.application) { strArgs = strArgs.concat([' -ispwServerApp ', args.application]); }
    if (args.serverConfig) { strArgs = strArgs.concat([' -ispwServerConfig ', args.serverConfig]); }
    if (args.stream) { strArgs = strArgs.concat([' -ispwServerStream ', args.stream]); }
    if (args.operation) { strArgs = strArgs.concat([' -operation ', args.operation]); }
    if (args.password) { strArgs = strArgs.concat([' -pass ', args.password]); }
    if (args.port) { strArgs = strArgs.concat([' -port ', args.port + '']); }
    if (args.protocol) { strArgs = strArgs.concat([' -protocol ', args.protocol]); }
    if (args.targetFolder) { strArgs = strArgs.concat([' -targetFolder ', '"' + args.targetFolder + '"']); }
    if (args.timeout) { strArgs = strArgs.concat([' -timeout ', args.timeout]); }
    if (args.typeOverride) { strArgs = strArgs.concat([' -typeOverride ', args.typeOverride]); }
    if (args.vscSetting) { strArgs = strArgs.concat([' -vscSetting ', args.vscSetting]); }

    return strArgs;
  }

  /**
   * Validates that there are settings for the necessary properties including yaml location, cli location, and build level. 
   * If any settings are not defined, a warning message is shown to the user and this function returns false. This method assumes
   * that all the given file URIs are part of the same workspace folder.
   * @param selectedFiles The selected files to check for settings for.
   */
  function validateSettings(selectedFiles: vscode.Uri[]): boolean {

    let workspaceFolder: string | undefined = vscode.workspace.getWorkspaceFolder(selectedFiles[0])?.name;
    let validYaml: boolean = YamlUtils.hasYaml(selectedFiles[0]);
    let validCli: boolean = true;
    let validLevel: boolean = true;

    if (!validYaml) {
      console.debug("The ISPW YAML mapping file cannot be found for " + workspaceFolder);
      MessageUtils.showWarningMessage("The ISPW YAML mapping file cannot be found for " + workspaceFolder + ". Update the YAML Mapping File location in the Settings for the ISPW extension.");
    }

    let cliLocation: string | undefined = getCliLocation();
    if (cliLocation === undefined || cliLocation === null) {
      validCli = false;
      console.debug("An ISPW CLI path was not configured.");
      MessageUtils.showWarningMessage('An ISPW CLI path was not configured. Please configure the CLI path in the ISPW user settings.');
    }

    let loadLevel: string | undefined = getLoadLevel();
    if (loadLevel === undefined || cliLocation === null) {
      validLevel = false;
      console.debug("The ISPW build level cannot be found for " + workspaceFolder);
      MessageUtils.showWarningMessage("The ISPW build level cannot be found for " + workspaceFolder + ". Update the build level in the Settings for the ISPW extension.");
    }

    return validYaml && validCli && validLevel;
  }

}