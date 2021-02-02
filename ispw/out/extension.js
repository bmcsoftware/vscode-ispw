"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode = __importStar(require("vscode"));
const IspwCliCommand = __importStar(require("./commands/CliCommand"));
const CredentialModifier_1 = require("./commands/CredentialModifier");
const MessageUtils_1 = require("./utils/MessageUtils");
// this method is called once when your extension is activated
function activate(context) {
    console.log('Congratulations, your extension "ispw" is now active!');
    // build commands
    let buildExplorer = vscode.commands.registerCommand('ispw.buildExplorer', (selectedFile) => __awaiter(this, void 0, void 0, function* () {
        let selectedFileUris = yield getSelectedFileUris();
        if (validateSelectFilesWorkspaceFolder(selectedFileUris)) {
            IspwCliCommand.runCommand('build', selectedFileUris);
        }
    }));
    context.subscriptions.push(buildExplorer);
    let buildEditor = vscode.commands.registerCommand('ispw.buildEditor', (selectedFile) => {
        IspwCliCommand.runCommand('build', undefined);
    });
    context.subscriptions.push(buildEditor);
    // generate commands
    let generateExplorer = vscode.commands.registerCommand('ispw.generateExplorer', (selectedFile) => __awaiter(this, void 0, void 0, function* () {
        let selectedFileUris = yield getSelectedFileUris();
        if (validateSelectFilesWorkspaceFolder(selectedFileUris)) {
            IspwCliCommand.runCommand('generate', selectedFileUris);
        }
    }));
    context.subscriptions.push(generateExplorer);
    let generateEditor = vscode.commands.registerCommand('ispw.generateEditor', (selectedFile) => {
        IspwCliCommand.runCommand('generate', undefined);
    });
    context.subscriptions.push(generateEditor);
    // load commands
    let loadExplorer = vscode.commands.registerCommand('ispw.loadExplorer', (selectedFile) => __awaiter(this, void 0, void 0, function* () {
        let selectedFileUris = yield getSelectedFileUris();
        if (validateSelectFilesWorkspaceFolder(selectedFileUris) === true) {
            IspwCliCommand.runCommand('load', selectedFileUris);
        }
    }));
    context.subscriptions.push(loadExplorer);
    let loadEditor = vscode.commands.registerCommand('ispw.loadEditor', (selectedFile) => {
        IspwCliCommand.runCommand('load', undefined);
    });
    context.subscriptions.push(loadEditor);
    // clear credentials
    let clearCreds = vscode.commands.registerCommand('ispw.clearCreds', (e) => {
        CredentialModifier_1.clearCredentials();
    });
    context.subscriptions.push(clearCreds);
}
exports.activate = activate;
/**
 * Gets the file URIs of the files selected in the File Explorer view
 */
function getSelectedFileUris() {
    return __awaiter(this, void 0, void 0, function* () {
        // get what's currently on the clipboard
        let prevText = yield vscode.env.clipboard.readText();
        // copy selected file paths to clipboard and read
        yield vscode.commands.executeCommand('copyFilePath');
        let selectedFilesStr = yield vscode.env.clipboard.readText();
        // put old contents back on clipboard
        yield vscode.env.clipboard.writeText(prevText);
        // convert string paths into file URIs
        let selectedFilesArr = selectedFilesStr.split(/\r\n/);
        console.debug("selectedFiles length: " + selectedFilesArr.length);
        let selectedFileUris = [];
        selectedFilesArr.forEach(filePath => {
            selectedFileUris.push(vscode.Uri.file(filePath));
        });
        return selectedFileUris;
    });
}
/**
 * Validates that all the given file URIs are part of the same workspace folder. ISPW actions can only be performed on files within the same workspace folder.
 * If the files are not in the same folder then an error message is shown to the user and this method returns false.
 * @param selectedFiles The file URIs to validate
 */
function validateSelectFilesWorkspaceFolder(selectedFiles) {
    let folder = vscode.workspace.getWorkspaceFolder(selectedFiles[0]);
    for (let i = 0; i < selectedFiles.length; i++) {
        if (folder !== vscode.workspace.getWorkspaceFolder(selectedFiles[i])) {
            MessageUtils_1.MessageUtils.showErrorMessage("ISPW commands can only be executed on files within the same workspace folder.");
            return false;
        }
        folder = vscode.workspace.getWorkspaceFolder(selectedFiles[i]);
    }
    return true;
}
// this method is called when your extension is deactivated
function deactivate() { }
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map