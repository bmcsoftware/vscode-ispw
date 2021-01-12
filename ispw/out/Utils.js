"use strict";
// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT license.
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
exports.Utils = void 0;
const cp = require("child_process");
const vscode = require("vscode");
var Utils;
(function (Utils) {
    function exec(command, options) {
        return new Promise((resolve, reject) => {
            cp.exec(command, options, (error, stdout, stderr) => {
                if (error) {
                    reject({ error, stdout, stderr });
                }
                resolve({ stdout, stderr });
            });
        });
    }
    let _channel;
    function getOutputChannel() {
        if (!_channel) {
            _channel = vscode.window.createOutputChannel("ISPW");
        }
        return _channel;
    }
    function executeISPWCommand(file) {
        return __awaiter(this, void 0, void 0, function* () {
            const workspaceFolders = vscode.workspace.workspaceFolders;
            if (!workspaceFolders || workspaceFolders.length === 0) {
                return;
            }
            if (file === "") {
                return;
            }
            console.log(file);
            const folderString = workspaceFolders[0].uri.fsPath;
            // TODO get the selected files per system, construct the command paramenters per different system.
            let commandLine = 'C:/Topaz/WorkbenchCLI200501-1207/IspwCLI.bat -host cw09.compuware.com -port 47623 -id ispwtpz -pass regress ';
            commandLine = commandLine.concat(' -code 1047 -timeout 0 -operation generate -targetFolder "D:/64workspace/gitmich/gitmich"');
            commandLine = commandLine.concat(" -ispwMappingLevel DEV1 ");
            commandLine = commandLine.concat(' -componentFiles "COB/').concat(file).concat('" ');
            console.log(commandLine);
            getOutputChannel().appendLine("The ISPW Generate process " + file + " has started...");
            try {
                const { stdout, stderr } = yield exec(commandLine, {
                    cwd: folderString,
                });
                if (stderr && stderr.length > 0) {
                    getOutputChannel().appendLine(stderr);
                    getOutputChannel().show(true);
                }
                if (stdout) {
                    getOutputChannel().appendLine("The ISPW Generate process is completed");
                    getOutputChannel().show(true);
                    const lines = stdout.split(/\r{0,1}\n/);
                    for (const line of lines) {
                        if (line.length === 0) {
                            continue;
                        }
                        getOutputChannel().appendLine(line);
                    }
                    getOutputChannel().show(true);
                }
            }
            catch (err) {
                const channel = getOutputChannel();
                channel.appendLine("The ISPW Generate for " + file + " failed.");
                if (err.stderr) {
                    channel.appendLine(err.stderr);
                }
                if (err.stdout) {
                    channel.appendLine(err.stdout);
                }
                channel.show(true);
            }
        });
    }
    Utils.executeISPWCommand = executeISPWCommand;
})(Utils = exports.Utils || (exports.Utils = {}));
//# sourceMappingURL=Utils.js.map