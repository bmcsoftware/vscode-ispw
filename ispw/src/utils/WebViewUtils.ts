import { Uri, Webview } from "vscode";

export class WebViewUtils {
  /**
 * A helper function which will get the webview URI of a given file or resource.
 *
 * @remarks This URI can be used within a webview's HTML as a link to the
 * given file/resource.
 *
 * @param webview A reference to the extension webview
 * @param extensionUri The URI of the directory containing the extension
 * @param pathList An array of strings representing the path to a file/resource
 * @returns A URI pointing to the file/resource
 */
  public static getUri(webview: Webview, extensionUri: Uri, pathList: string[]) {
    return webview.asWebviewUri(Uri.joinPath(extensionUri, ...pathList));
  }

  public static addLibUri(libText: string, libUri: Uri, htmlContent: string) {
    return htmlContent.replace(libText, decodeURIComponent(libUri.toString()));
  }

  public static resolveValues(libText: string, libVal: string, htmlContent: string) {
    return htmlContent.replace(libText, libVal);
  }
}