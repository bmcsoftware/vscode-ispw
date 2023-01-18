import { IViewerProvider } from "./viewerProvider";
//import * as model from "../models/DialogModel";
import { FieldNode,FieldType, CommonElement, ItemNode } from "../models/DialogModel";

export class WebViewUIComponentViewerProvider implements IViewerProvider {
  getHTML(model: CommonElement): string {
    return this.getAreaHTML(model);
    // todo add the JS functions based on model so that the user data will be sent to extension
  }

  getInputTextHTML(field: CommonElement): string {
    const label = field.node?.label ?? "";
    const id = field.node?.id ?? "id";
    const value = field.getFieldNodeValue();
    const target = (<FieldNode>field.node).target;

    let maxSize = "";
    if (field.node && (<FieldNode>field.node).size) {
      maxSize = 'maxlength="' + (<FieldNode>field.node).size + '"';
    }

    const html = `
    <div class="component">
      <p>${label}</p>
      <vscode-text-field id="${id}" ${maxSize} value="${value}" target="${target}"></vscode-text-field>
    </div>
    `;

    return html;
  }

  /**
   * List control not available in Webview UI Toolkit. 
   * List type with options=multiple should show as checkboxes. Otherwise, should show as combo.
   * @param field 
   * @returns 
   */
  getListHTML(field: CommonElement): string {

    //TODO need to add check for option: multiple or single.
    const html = "";

    /*if (option == "multiple") {
        html = this.getComboHTML(field);
      }
      else {
        html = this.getCheckHTML(field);
      }
    return html;
      */

    return html;
  }

  getComboHTML(field: CommonElement): string {
    const label = field.node?.label ?? "";
    const id = field.node?.id ?? "id";

    let html = `
      <div class="component">
        <p>${label}</p>
        <vscode-dropdown position="below" id="${id}">
    `;

    const childrens = field.getChildren();
    if (childrens) {
      childrens.forEach((child) => {
        const itemNode: ItemNode = <ItemNode>child;
        const itemValue: string = itemNode.value ?? "";
        const itemLabel: string = itemNode.label ?? itemValue;
        const childHtml = `<vscode-option>${itemLabel}</vscode-option>`;
        html = html.concat(childHtml);
      });
    }

    const shtml = `</vscode-dropdown></div>`;
    html = html.concat(shtml);

    return html;
  }

  getCheckHTML(field: CommonElement): string {
    const label = field.node?.label ?? "";
    const id = field.node?.id ?? "id";
    const value = (<FieldNode>field.node)?.value ?? "value";
    const target = (<FieldNode>field.node)?.target ?? "";

    //FIX the checked property was always being set to true... fixed
    const html = `
      <div class="component">
        <vscode-checkbox id="${id}" checked="${value}" target="${target}">${label}</vscode-checkbox>
      </div>
    `;
    return html;
  }

  /**
   * TODO
   * @param field 
   * @returns 
   */
  getRadioHTML(field: CommonElement): string {
    const label = field.node?.label ?? "";
    const target = (<FieldNode>field.node).target;

    let html = `
      <div class="component">
        <p>${label}</p>
        <vscode-radio-group orientation="vertical" id="${field.node?.id}" target="${target}">
        <!-- <label slot="label">${label}</label> -->
       `;
    const children = field.getChildren();
    if (children) {
      children.forEach((child) => {
        const itemNode: ItemNode = <ItemNode>child;
        // itemValue is always Y or N for radio, so it can be used to represent default checked state as well.
        const itemValue: string = itemNode.value ?? "";
        // in the case of "check" type with children, there won't be a label; use value instead
        const itemLabel: string = itemNode.label ?? itemValue;

        const childHtml = `<vscode-radio value="${itemValue}" checked="${itemValue}">${itemLabel}</vscode-radio>`;
        html = html.concat(childHtml);
      });
    }

    const shtml = `   
        </vscode-radio-group>
      </div>
    `;
    html = html.concat(shtml);
    return html;
  }

  getAreaHTML(theArea: CommonElement): string {
    const label = theArea.node?.label ?? "";

    let areaHtml = `<div class="area-container">`;
    if (label.length > 0) {
      areaHtml = areaHtml.concat(`<h3>${label}</h3>`);
    }
    
    let areaChildrenHtml = "";
    const childrens = theArea.getChildren();
    if (childrens) {
      childrens.forEach((child) => {
        if (child instanceof CommonElement) {
          const fieldType: FieldType = child.getFieldNodeType();
          let sPart = "";

          switch (fieldType) {
            case "TEXT":
              sPart = this.getInputTextHTML(child);
              break;
            case "COMBO":
              sPart = this.getComboHTML(child);
              break;
            case "LIST":
              sPart = this.getListHTML(child);
              break;  
            case "RADIO":
              sPart = this.getRadioHTML(child);
              break;
            case "CHECK":
              if (child.children && child.children.length > 0) {
                sPart = this.getRadioHTML(child);
              } else {
                sPart = this.getCheckHTML(child);
              }

              break;
          }

          areaChildrenHtml = areaChildrenHtml.concat(sPart);
        }
      });
    }

    // TODO add the JS functions based on model so that the user data will be sent to extension
    const footerHtml = `</div>`;
    areaHtml = areaHtml.concat(areaChildrenHtml, footerHtml);
    return areaHtml;
  }
}

export class DefaultViewerProvider implements IViewerProvider {
   // eslint-disable-next-line @typescript-eslint/no-unused-vars
   getHTML(_model: CommonElement): string {
    throw new Error("Method not implemented.");
  }
}
