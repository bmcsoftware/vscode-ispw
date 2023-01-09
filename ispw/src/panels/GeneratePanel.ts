import {
  Disposable,
  WebviewPanel,
  window,
  Uri,
  ViewColumn,
  Webview,
  workspace
} from "vscode";
import { WebViewUtils } from "../utils/WebViewUtils";
import { GenerateWithParmsController } from "../controllers/GenerateWithParmsController";
import { HtmlFormData } from "../models/GenerateWithParmsModel";
import {load} from "cheerio";
import fs from "fs";
import * as path from 'path';
import { YamlUtils } from "../utils/YamlUtils";
import { MessageUtils } from "../utils/MessageUtils";
import { SettingsUtils } from "../utils/SettingsUtils";
import { IspwRoot } from "../types/IspwTypeMapping";
import { GenerateTaskModel } from "../models/GenerateTaskModel";
import * as IspwCliCommand from "../commands/CliCommand";
import { Constants } from "../utils/Constants";
import { TaskCleanupModel } from "../models/TaskCleanupModel";
import EventEmitter from "events";

/**
 * This class creates a custom generate panel.
 *
 * It contains all the data and methods for:
 *
 * - TODO
 * - Properly cleaning up and disposing of webview resources when the panel is closed
 * - Setting the HTML (and by proxy CSS/JavaScript) content of the webview panel
 */
export class GeneratePanel {
  public static currentPanel: GeneratePanel | undefined;
  private _panel: WebviewPanel;
  private _disposables: Disposable[] = [];
  private static _generateWithParmsController: GenerateWithParmsController = new GenerateWithParmsController('WebViewUIComponent');
  public static _onLoadComplete: EventEmitter = new EventEmitter(); 

  /**
   * The AllComponentsPanel class private constructor (called only from the render method).
   *
   * @param panel A reference to the webview panel
   * @param extensionUri The URI of the directory containing the extension
   */
  public constructor(panel: WebviewPanel, cleanupModel: TaskCleanupModel, lpar: string) {
    this._panel = panel;
    // Set an event listener to listen for when the panel is disposed (i.e. when the user closes
    // the panel or when the panel is closed programmatically)
    this._panel.onDidDispose(() => {
      this.cancel(cleanupModel, lpar);
    }, null, this._disposables);
  }

  /**
   * Renders the current webview panel if it exists otherwise a new webview panel
   * will be created and displayed.
   *
   * @param extensionUri The URI of the directory containing the extension.
   */
  public static async render(extensionUri: Uri, selectedFiles: Uri[] | undefined) {
    if (selectedFiles === undefined || selectedFiles.length === 0) { // command came from command palette or editor menu
      console.debug("SelectedFiles was undefined, attempting to use editor file");
      let editorUri = window.activeTextEditor?.document.uri;
      if (editorUri === undefined || workspace.getWorkspaceFolder(editorUri) === undefined) {
        // the active output channel will also be considered a text editor, but that's not useful to us
        MessageUtils.showErrorMessage("A workspace file must be open in the editor.");
        return;
      }
      else {
        selectedFiles = [];
        selectedFiles.push(editorUri);
      }
    }
    console.log('Rendering generate with parms panel for : ' + selectedFiles[0]);
    //validate CES URL and Token
    const cesUrl = await SettingsUtils.getCesUrlWithPrompt();
    const cesToken = await SettingsUtils.getCesTokenWithPrompt();
    const isValidYaml = await YamlUtils.hasYaml(true, selectedFiles[0]);

    if (cesUrl === undefined) {
      console.debug("The CES URL cannot be found. ");
      MessageUtils.showWarningMessage("The CES URL cannot be found. Update the CES URL in the Settings for the ISPW extension.");
      return;
    }

    if (cesToken === undefined) {
      console.debug("The CES Token cannot be found. ");
      MessageUtils.showWarningMessage("The CES Token cannot be found. Update the CES Token in the Settings for the ISPW extension.");
      return;
    }

    if (!isValidYaml) {
      console.debug("The ISPW YAML mapping file cannot be found.");
      MessageUtils.showWarningMessage("The ISPW YAML mapping file cannot be found. Update the YAML Mapping File location in the Settings for the ISPW extension.");
      return;
    }
    console.log("Loading config YAML...");
    YamlUtils.loadYaml(selectedFiles[0]).then(async function (configYaml) {
      if (configYaml) {
        console.log('Config YAML loaded successfully.');
        const lpar = configYaml.ispwApplication.host + '-' + configYaml.ispwApplication.port;

        //load Selected file into ISPW
        await IspwCliCommand.runCommand(Constants.OP_LOAD, selectedFiles);
        let counter = 0;
        GeneratePanel._onLoadComplete.on('load_done', async (code) => {
          if (code === 0 && counter === 0 && selectedFiles !== undefined) {
            counter++;
            const partition = selectedFiles[0].path.split('/');
            const component = partition[partition.length - 1];

            const lastIndexOfStop = component.lastIndexOf('.');
            const taskName = component.substring(0, lastIndexOfStop);
            const taskType = GeneratePanel._getTaskType(component, configYaml);
            if (taskType === '') {
              window.showErrorMessage('No type mapping found in YAML Mapping file.');
              return;
            }
            await GeneratePanel._generateWithParmsController.getTaskDetails({
              'moduleName': taskName,
              'application': configYaml.ispwApplication.application,
              'moduleType': taskType,
              'checkoutToLevel': SettingsUtils.getLoadLevel() as string
            }, lpar).then(async function (details) {

              if (details instanceof Error) {
                console.error('Error occured while fetching task details from server : ', details.stack);
                window.showErrorMessage(details.message);
                return;
              }

              // fetch html form to create Generate with Parms panel
              console.log('Fetching Generate with Parms XML from server...');
              await GeneratePanel._generateWithParmsController.generateHTML({
                'taskId': details.taskId as string,
                'containerId': details.assignmentId as string,
                'containerType': 'A'
              }, lpar).then(function (response: HtmlFormData | Error) {
                if (response instanceof Error) {
                  console.error('Error occured while fetching XML from server : ', response.stack);
                  window.showErrorMessage(response.message);
                  return;
                }
                console.log('XML successfully fetched from server.');
                const htmlResponse = response as HtmlFormData;
                // If a webview panel does not already exist create and show a new one
                const panel = window.createWebviewPanel(
                  // Panel view type
                  'showGeneratePanel',
                  // Panel title
                  htmlResponse.title,

                  // The editor column the panel should be displayed in
                  ViewColumn.One,
                  // Extra panel configurations
                  {
                    // Enable JavaScript in the webview
                    enableScripts: true,
                  }
                );

                const taskInfo = {
                  'assignmentId': details.assignmentId as string,
                  'taskId': details.taskId as string,
                  'setId': htmlResponse.setId as string
                };

                GeneratePanel.currentPanel = new GeneratePanel(panel, taskInfo, lpar);
                htmlResponse.componentName = details.moduleName;
                htmlResponse.componentType = details.moduleType;
                //set HTML content on webview
                GeneratePanel.currentPanel._setWebviewContent(
                  panel.webview,
                  extensionUri, htmlResponse);

                //set listeners to buttons
                GeneratePanel.currentPanel._setWebViewListener(panel.webview, lpar, taskInfo);
                console.log('Rendered generate with parms panel.');
              });
            });
          }
        });
      }
    });
  }

  /**
   * Cleans up and disposes of webview resources when the webview panel is closed.
   */
  public dispose() {
    GeneratePanel.currentPanel = undefined;

    // Dispose of the current webview panel
    this._panel.dispose();

    // Dispose of all disposables (i.e. commands) for the current webview panel
    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
    console.log('Panel disposed successfuly.');
  }

  public async generate(generateDetails: GenerateTaskModel, lpar: string) {
    console.log('Starting generate with parms');
    await GeneratePanel._generateWithParmsController.generateAction(generateDetails, lpar).then((generateResponse) => {
      if (generateResponse instanceof Error) {
        console.log('Generate with parms complete with '+ generateResponse.message);
        window.showErrorMessage(generateResponse.message);
      } else {
        console.log('Generate with parms completed successfuly.');
        window.showInformationMessage('Generate for task is successful.');
      }
      this.dispose();
    });
  }

  public async cancel(cleanupModel: TaskCleanupModel, lpar: string) {
    this.dispose();
    await GeneratePanel._generateWithParmsController.taskCleanup(cleanupModel, lpar).then((response) => {
      if (response instanceof Error) {
        window.showErrorMessage(response.message);
      }
    });
  }

  /**
   * Defines and returns the HTML that should be rendered within the webview panel.
   *
   * @remarks This is also the place where references to CSS and JavaScript files/packages
   * (such as the Webview UI Toolkit) are created and inserted into the webview HTML.
   *
   * @param webview A reference to the extension webview
   * @param extensionUri The URI of the directory containing the extension
   * @returns A template string literal containing the HTML that should be
   * rendered within the webview panel
   */
  private _setWebviewContent(webView: Webview, extensionUri: Uri, formData: HtmlFormData) {

    const toolkitUri = WebViewUtils.getUri(webView, extensionUri, [
      "node_modules",
      "@vscode",
      "webview-ui-toolkit",
      "dist",
      "toolkit.js",
    ]);
    const codiconsUri = WebViewUtils.getUri(webView, extensionUri, [
      "node_modules",
      "@vscode",
      "codicons",
      "dist",
      "codicon.css",
    ]);

    const scriptUri = WebViewUtils.getUri(webView, extensionUri, ["media", "scripts", "generateWithParms.js"]);
    const styleUri = WebViewUtils.getUri(webView, extensionUri, ["media", "styles", "styleWrap.css"]);
    let htmlContent = fs.readFileSync(path.resolve(__dirname, '../../media/views/generateWithParms.html')).toString();
    htmlContent = WebViewUtils.addLibUri("${toolkitUri}", toolkitUri, htmlContent);
    htmlContent = WebViewUtils.addLibUri("${codiconsUri}", codiconsUri, htmlContent);
    htmlContent = WebViewUtils.addLibUri("${scriptUri}", scriptUri, htmlContent);
    htmlContent = WebViewUtils.addLibUri("${styleUri}", styleUri, htmlContent);
    htmlContent = WebViewUtils.resolveValues("${componentName}", formData.componentName as string, htmlContent);
    htmlContent = WebViewUtils.resolveValues("${componentType}", formData.componentType as string, htmlContent);
    let $ = load(htmlContent);
    $('#genparmform').append(formData.html);
    webView.html = $.html();
  }

  /**
   * This method listens to messages sent from JS.
   * @param webView 
   */
  private _setWebViewListener(webView: Webview, lpar: string, data: any) {
    webView.onDidReceiveMessage(async (message) => {
      const command = message.command;
      switch (command) {
        case "generate":
          const generateTaskModel: GenerateTaskModel = {
            taskId: data.taskId,
            containerId: data.assignmentId,
            containerType: 'A',
            updateDetails: message.data,
            setId: data.setId
          };
          this.generate(generateTaskModel, lpar);
          return;
        case "cancel":
          const cleanupModel: TaskCleanupModel = {
            taskId: data.taskId,
            assignmentId: data.assignmentId,
            setId: data.setId
          };
          this.cancel(cleanupModel, lpar);
          return;
        default:
          window.showErrorMessage("Invalid message recieved.");
          return;
      }
    });
  }

  private static _getTaskType(component: string, configYaml: IspwRoot): string {
    const lastIndexOfStop = component.lastIndexOf('.');
    const taskExtension = component.substring(lastIndexOfStop + 1, component.length);
    let ispwType = '';
    configYaml.ispwApplication.pathMappings.forEach(pathMapping => {
      if (ispwType !== '') {
        return;
      }
      pathMapping.types.forEach(pathType => {
        if (pathType.fileExtension === taskExtension) {
          ispwType = pathType.ispwType;
          return ispwType;
        }
      });
    });
    return ispwType;
  }
}