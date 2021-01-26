import * as vscode from 'vscode';
import * as IspwCliCommand from "./commands/CliCommand";
import { clearCredentials } from './commands/CredentialModifier';
import { YamlUtils } from './utils/YamlUtils';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	if ((uri: vscode.Uri) => {
		console.log("I made it here");
		return YamlUtils.hasYaml(uri);
	}) {
		vscode.commands.executeCommand('setContext', 'myContext', 'true');
	}
	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "ispw" is now active!');

	let build = vscode.commands.registerCommand('ispw.build', (selectedFile: vscode.Uri) => {
		IspwCliCommand.runCommand('build', selectedFile);
	});
	context.subscriptions.push(build);

	let generate = vscode.commands.registerCommand('ispw.generate', (selectedFile: vscode.Uri) => {
		IspwCliCommand.runCommand('generate', selectedFile);
	});
	context.subscriptions.push(generate);

	let load = vscode.commands.registerCommand('ispw.load', (selectedFile: vscode.Uri) => {
		IspwCliCommand.runCommand('load', selectedFile);
	});
	context.subscriptions.push(load);

	let clearCreds = vscode.commands.registerCommand('ispw.clearCreds', (e) => {
		clearCredentials();
	});
	context.subscriptions.push(clearCreds);
}


// this method is called when your extension is deactivated
export function deactivate() { }
