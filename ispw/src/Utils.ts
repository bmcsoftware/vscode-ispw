// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT license.

import * as path from "path";
import * as fs from "fs";
import * as cp from "child_process";
import * as vscode from "vscode";

export namespace Utils {
  function exec(
    command: string,
    options: cp.ExecOptions
  ): Promise<{ stdout: string; stderr: string }> {
    return new Promise<{ stdout: string; stderr: string }>(
      (resolve, reject) => {
        cp.exec(command, options, (error, stdout, stderr) => {
          if (error) {
            reject({ error, stdout, stderr });
          }
          resolve({ stdout, stderr });
        });
      }
    );
  }

  let _channel: vscode.OutputChannel;
  function getOutputChannel(): vscode.OutputChannel {
    if (!_channel) {
      _channel = vscode.window.createOutputChannel("ISPW");
    }
    return _channel;
  }

  export async function executeISPWCommand(file: string): Promise<void> {
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
    let commandLine =
      'C:/Topaz/WorkbenchCLI200501-1207/IspwCLI.bat -host cw09.compuware.com -port 47623 -id ispwtpz -pass regress ';
    commandLine = commandLine.concat(' -code 1047 -timeout 0 -operation generate -targetFolder "D:/64workspace/gitmich/gitmich"' );
    commandLine = commandLine.concat(" -ispwMappingLevel DEV1 ");
    commandLine = commandLine.concat(' -componentFiles "COB/').concat(file).concat('" '); 
    
    console.log(commandLine);
    getOutputChannel().appendLine("The ISPW Generate process " + file + " has started...");
    try {
      const { stdout, stderr } = await exec(commandLine,  {
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
    } catch (err) {
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
  }
}
