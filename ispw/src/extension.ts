import * as vscode from 'vscode';
import * as IspwCliCommand from "./commands/CliCommand";
import { clearCredentials } from './commands/CredentialModifier';
import { MessageUtils } from './utils/MessageUtils';
import { Constants } from './utils/Constants';

// this method is called once when your extension is activated
export function activate(context: vscode.ExtensionContext) {

	console.log('Congratulations, your extension "ISPW" is now active!');

	// build commands
	let buildExplorer = vscode.commands.registerCommand(Constants.CMD_BUILD_EXPLORER, async (selectedFile: vscode.Uri) => {
		let selectedFileUris: vscode.Uri[] = await getSelectedFileUris();
		if (validateSelectFilesWorkspaceFolder(selectedFileUris)) {
			IspwCliCommand.runCommand(Constants.OP_BUILD, selectedFileUris);
		}
	});
	context.subscriptions.push(buildExplorer);
	let buildEditor = vscode.commands.registerCommand(Constants.CMD_BUILD_EDITOR, (selectedFile: vscode.Uri) => {
		IspwCliCommand.runCommand(Constants.OP_BUILD, undefined);
	});
	context.subscriptions.push(buildEditor);

	// generate commands
	let generateExplorer = vscode.commands.registerCommand(Constants.CMD_GENERATE_EXPLORER, async (selectedFile: vscode.Uri) => {
		let selectedFileUris: vscode.Uri[] = await getSelectedFileUris();
		if (validateSelectFilesWorkspaceFolder(selectedFileUris)) {
			IspwCliCommand.runCommand(Constants.OP_GENERATE, selectedFileUris);
		}
	});
	context.subscriptions.push(generateExplorer);
	let generateEditor = vscode.commands.registerCommand(Constants.CMD_GENERATE_EDITOR, (selectedFile: vscode.Uri) => {
		IspwCliCommand.runCommand(Constants.OP_GENERATE, undefined);
	});
	context.subscriptions.push(generateEditor);

	// load commands
	let loadExplorer = vscode.commands.registerCommand(Constants.CMD_LOAD_EXPLORER, async (selectedFile: vscode.Uri) => {
		let selectedFileUris: vscode.Uri[] = await getSelectedFileUris();
		if (validateSelectFilesWorkspaceFolder(selectedFileUris) === true) {
			IspwCliCommand.runCommand(Constants.OP_LOAD, selectedFileUris);
		}
	});
	context.subscriptions.push(loadExplorer);
	let loadEditor = vscode.commands.registerCommand(Constants.CMD_LOAD_EDITOR, (selectedFile: vscode.Uri) => {
		IspwCliCommand.runCommand(Constants.OP_LOAD, undefined);
	});
	context.subscriptions.push(loadEditor);

	// clear credentials
	let clearCreds = vscode.commands.registerCommand(Constants.CMD_CLEAR_CREDS, (e) => {
		clearCredentials();
	});
	context.subscriptions.push(clearCreds);
}

/**
 * Gets the file URIs of the files selected in the File Explorer view
 */
async function getSelectedFileUris(): Promise<vscode.Uri[]> {
	// get what's currently on the clipboard
	let prevText: string = await vscode.env.clipboard.readText();
	// copy selected file paths to clipboard and read
	await vscode.commands.executeCommand('copyFilePath');
	let selectedFilesStr: string = await vscode.env.clipboard.readText();
	// put old contents back on clipboard
	await vscode.env.clipboard.writeText(prevText);

	// convert string paths into file URIs
	let selectedFilesArr: string[] = selectedFilesStr.split(/\r\n/);
	console.debug("selectedFiles length: " + selectedFilesArr.length);
	let selectedFileUris: vscode.Uri[] = [];
	selectedFilesArr.forEach(filePath => {
		selectedFileUris.push(vscode.Uri.file(filePath));
	});
	return selectedFileUris;
}

/**
 * Validates that all the given file URIs are part of the same workspace folder. ISPW actions can only be performed on files within the same workspace folder.
 * If the files are not in the same folder then an error message is shown to the user and this method returns false.
 * @param selectedFiles The file URIs to validate
 */
function validateSelectFilesWorkspaceFolder(selectedFiles: vscode.Uri[]): boolean {
	let folder: vscode.WorkspaceFolder | undefined = vscode.workspace.getWorkspaceFolder(selectedFiles[0]);

	for (let i = 0; i < selectedFiles.length; i++) {
		if (folder !== vscode.workspace.getWorkspaceFolder(selectedFiles[i])) {
			MessageUtils.showErrorMessage("ISPW commands can only be executed on files within the same workspace folder.");
			return false;
		}
		folder = vscode.workspace.getWorkspaceFolder(selectedFiles[i]);
	}

	return true;
}

// this method is called when your extension is deactivated
export function deactivate() { }
