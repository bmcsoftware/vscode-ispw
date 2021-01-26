"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode = require("vscode");
const IspwCliCommand = require("./commands/CliCommand");
const CredentialModifier_1 = require("./commands/CredentialModifier");
const YamlUtils_1 = require("./utils/YamlUtils");
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
function activate(context) {
    if ((uri) => {
        console.log("I made it here");
        return YamlUtils_1.YamlUtils.hasYaml(uri);
    }) {
        vscode.commands.executeCommand('setContext', 'myContext', 'true');
    }
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('Congratulations, your extension "ispw" is now active!');
    let build = vscode.commands.registerCommand('ispw.build', (selectedFile) => {
        IspwCliCommand.runCommand('build', selectedFile);
    });
    context.subscriptions.push(build);
    let generate = vscode.commands.registerCommand('ispw.generate', (selectedFile) => {
        IspwCliCommand.runCommand('generate', selectedFile);
    });
    context.subscriptions.push(generate);
    let load = vscode.commands.registerCommand('ispw.load', (selectedFile) => {
        IspwCliCommand.runCommand('load', selectedFile);
    });
    context.subscriptions.push(load);
    let clearCreds = vscode.commands.registerCommand('ispw.clearCreds', (e) => {
        CredentialModifier_1.clearCredentials();
    });
    context.subscriptions.push(clearCreds);
}
exports.activate = activate;
// this method is called when your extension is deactivated
function deactivate() { }
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map