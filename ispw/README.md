# BMC Compuware ISPW README

## Overview

This extension enables key features from BMC Compwuare's ISPW product that will allow users to integrate their ISPW, Git, and Visual Studio Code development processes.

With the ISPW Visual Studio Code extension, you are able to load source from your Git repository into ISPW and also perform generate and build operations. The extension works by sending requests from Visual Studio Code to your Topaz Workbench CLI installation which connects to ISPW on the mainframe.

## Requirements

- Visual Studio Code version 1.52.0 or higher
- ISPW mainframe version 18.02 license and credentials
- A local Topaz Workbench CLI installation version 20.06.01 or higher
- A local folder containing mainframe source code

## Extension Settings

This extension contributes the following settings:

- `ISPW.Assignment Description`: the ISPW assignment description or description pattern to use when a new assignment is created to load tasks. There are four pattern variables that can be used in the assignment description and they are `{user}`, `{branch_name}`, `{project_name}`, and `{date}`. The values of the variables will be set dynamically when an ISPW command is run.
- `ISPW.Level`: the ISPW level to use when loading tasks
- `ISPW.Topaz CLI Installation Path`: the absolute path to the local Topaz Workbench CLI installation
- `ISPW.YAML Mapping File`: the path to the ISPW configuration yaml file. This path may be relative to the workspace folder or absolute. If a relative file path is used, the yaml file must exist in the workspace. The path must contain the file name of the ISPW configuration mapping file.

## Command Descriptions

![ISPW commands](ispw/media/ISPW-commands.png)

#### ISPW Build

Executes an ISPW CLI command to load the selected files into ISPW and immediately run a build on them. In File Explorer this command will show up in the right-click menu and will work on single and multiple selections. From the command palette or editor menu, this command will execute against the file currently open in the editor. After the task is loaded, the ISPW build request will find any other impacting tasks and add them to the generate set. This is to ensure that changes in one program did not cause compile errors in another impacting program.

#### ISPW Clear Stored Credentials

Clears the stored mainframe username and password. After using this command, you will be prompted for your username and password the next time you use an ISPW command. Use this command if you need to change the username and password that is used when connecting to ISPW on the mainframe.

#### ISPW Generate

Executes an ISPW CLI command to load the selected files into ISPW and immediately run a generate on them. In File Explorer this command will show up in the right-click menu and will work on single and multiple selections. From the command palette or editor menu, this command will execute against the file currently open in the editor.

#### ISPW Load

Executes an ISPW CLI command to load the selected files into ISPW. In File Explorer this command will show up in the right-click menu and will work on single and multiple selections. From the command palette or editor menu, this command will execute against the file currently open in the editor.

## Complete Setup Guide

#### Install the Topaz Workbench CLI

This extension sends requests to the Topaz Workbench CLI. Information about installation and use of the Topaz Workbench CLI can be found [here](https://devops.api.compuware.com/apis/topaz_cli.html#workspace).

#### Download mainframe source from ISPW

If your current source is stored in ISPW and you need to move it to Git, follow the first part of [this guide](https://devops.api.compuware.com/guidelines/ispw/GIT_to_ISPW_Integration_Tutorial.html#set-up-a-git-project-with-the-source-yaml-file-and-jenkinsfile-and-set-up-a-jenkins-multibranch-pipeline-2) to download source from ISPW into a local folder.

#### Configure the yaml file

In order for the integration between ISPW, Git, and Visual Studio Code to work you need to have a yaml file which specifies the configuration. See [this guide](https://devops.api.compuware.com/guidelines/ispw/Git%20to%20ISPW%20Integration%20-%20The%20ISPW%20YAML%20Configuration%20File.html#git-to-ispw-integration-the-ispw-yaml-configuration-file) for more information on the yaml file structure.

#### Create a Git repository to hold the mainframe source

The Git repository will be the one source of truth for your source code. The steps for setting up a git repository will vary depending on which Git client you plan to use. A guide for GitHub can be found [here](https://docs.github.com/en/github/importing-your-projects-to-github/adding-an-existing-project-to-github-using-the-command-line) and a guide for BitBucket can be found [here](https://www.atlassian.com/git/tutorials/setting-up-a-repository).

#### Install the ISPW Visual Studio Code extension

The ISPW extension can be found in the Visual Studio Code Extension Marketplace. More information on adding extensions to Visual Studio Code can be found [here](https://code.visualstudio.com/docs/introvideos/extend).

#### Configure the Visual Studio Code workspace

It is advised to set the root folder of your Git repository to be the primary workspace folder in Visual Studio Code.

#### Configure the settings for the ISPW extension

See [Extension Settings](#extension-settings) for descriptions of each of the settings. The assignment description, level, and Topaz CLI installation path are all stored at the User settings level. The YAML mapping file location can be configured at the Workspace settings level.

![example settings](ispw/media/example-settings.png)

#### Using ISPW Commands

The File Explorer has the ISPW build, generate, and load commands available in the right-click menu. To use the commands, select the file(s), right click, and select the ISPW command you want to execute. The first time you use any command, you will be prompted for any settings values that have not been filled it. You will also be prompted for your mainframe username and password. The username and password are saved for the session so you will not be prompted for them again unless you clear the stored credentials or restart Visual Studio Code.

![building from file explorer](ispw/media/file-explorer-action.gif)

The command palette has the ISPW clear stored credentials, build, generate, and load commands available. To use the commands, open a file in the editor, open the command palette and select the command. The first time you use a command, you will be prompted for any settings values that have not been filled it. You will also be prompted for your mainframe username and password. The username and password are saved for the session so you will not be prompted for them again unless you clear the stored credentials or restart Visual Studio Code.

![generating from command palette](ispw/media/command-palette-action.gif)

## Known Issues

1. The extension only supports storing a single set of credentials. If a user needs to use a different host and port, requiring different credentials, they must use the Clear Stored Credentials command.
2. ISPW commands will always appear in the File Explorer menu, even if they are not applicable to the file selected.

## Release Notes

### 1.0.0

Initial release of the ISPW extension. The extension supports Build, Generate, and Load commands.

[//]: # " Visual Studio Code's Markdown Support : http://code.visualstudio.com/docs/languages/markdown"
[//]: # "Markdown Syntax Reference: https://help.github.com/articles/markdown-basics/"

\-