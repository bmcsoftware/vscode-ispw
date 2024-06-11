import * as model from "../models/DialogModel";
import * as xml2js from "xml2js";
import { KeyVal } from "../models/GenerateWithParmsModel";

export function createDialogModel(xmlString: string, defaultsArr: KeyVal[]): model.CommonElement {
  let dialogModel: model.DialogNode = {
    id: "dialog1",
    label:"",
    type: "",
    nodeType: "DialogNode",
  };
  let commonElement = new model.CommonElement(dialogModel);
  const parser = new xml2js.Parser();
  parser.parseString(xmlString, (err: unknown, result: any) => {
    if (err) {
      throw new Error("Fail to parsing XML."+ err);
    }
    const dialogData = result["tns:Dialog"];

    const theDialogModel: model.DialogNode = {
      id: dialogData.$.id ? dialogData.$.id : "dialog1",
      label: dialogData.$.label ? dialogData.$.label : "",
      type: dialogData.$.type ? dialogData.$.type : "",
      nodeType: "DialogNode",
    };

    commonElement = new model.CommonElement(theDialogModel);

    const areaNodes: any[] = dialogData["tns:area"];
    areaNodes.forEach((area) => {
      const areaModel = createArea(area, defaultsArr);
      commonElement.addChild(areaModel);    
    });

    //return commonElement;
  });

  return commonElement;
}

export function createItem(data: any): model.ItemNode {
  const label: any = data.$.label;
  const value: any = data.$.value;
  const returnValue: any = data.$.return;

  let propStr = '{ "nodeType": "FieldNode", ';
  if (label) {
    propStr = propStr.concat('"label":"' + label + '",');
  }

  if (value) {
    propStr = propStr.concat('"value":"' + value + '",');
  }

  if (returnValue) {
    propStr = propStr.concat('"return":"' + returnValue + '"');
  }

  if (propStr.trim().endsWith(",")) {
    propStr = propStr.trim().substring(0, propStr.length - 1);
  }
  propStr = propStr.concat("}");

  const item: model.ItemNode = JSON.parse(propStr);
  return item;
}

export function createField(data: any, defaultArr: KeyVal[]): model.CommonElement {
  const id: any = data.$.id;
  const label: any = data.$.label;
  const value: any = data.$.value;
  let typeData: any = data.$.type;
  const size: number = data.$.size;
  const options: any = data.$.options;
  const target: any = data.$.target;
  
  let propStr = '{ "nodeType": "FieldNode", ';

  if (id) {
    propStr = propStr.concat('"id":"' + id + '",');
  }

  if (label) {
    propStr = propStr.concat('"label":"' + label + '",');
  }

  if (defaultArr) {
    defaultArr.forEach(function (a) {
      let datagroup = value.slice(1, -1).split(".");

      if (datagroup[0] === a.key) {
        a.value.forEach(element => {
          var stringElement = JSON.stringify(element);
          let keyValuePairs = stringElement.slice(2, -2) //remove first and last character
            .split(/\s*,\s*/)                     //split with optional spaces around the comma
            .map(chunk => chunk.split(":"));      //split key:value

          var fieldPair = keyValuePairs[0];
          var defaultValPair = keyValuePairs[2];
          if (fieldPair[1] === id) {
            propStr = propStr.concat('"value":"' + defaultValPair[1] + '",');
            return;
          }
        });
      }
    });
  } else if (value) {
    propStr = propStr.concat('"value":"' + value + '",');
  }

  if (typeData) {
    typeData = new String(typeData).toUpperCase();
    propStr = propStr.concat('"type": "' + typeData + '",');
  }

  if (size > 0) {
    propStr = propStr.concat('"size": ' + size + ",");
  }

  if(target) {
    propStr = propStr.concat('"target":"' + target + '",');
  }

  if (options) {
    propStr = propStr.concat('"options":"' + options + '"');
  }

  if (propStr.trim().endsWith(",")) {
    propStr = propStr.trim().substring(0, propStr.length - 1);
  }
  propStr = propStr.concat("}");

  const field: model.FieldNode = JSON.parse(propStr);
  const fieldModel = new model.CommonElement(field);

  const items: any[] = data["tns:item"];
  if (items && items !== undefined) {
    items.forEach((item) => {
      const itemModel: model.ItemNode = createItem(item);
      fieldModel.addChild(itemModel);
    });
  }

  return fieldModel;
}

export function createArea(data: any, defaultMap: KeyVal[]): model.CommonElement {
  const id: any = data.$.id;
  const label: any = data.$.label;
  const typeData: any = data.$.type;
  const columnCount: number = data.$.columnCount;

  let propStr = '{ "nodeType": "AreaNode", ';
  if (id) {
    propStr = propStr.concat('"id":"' + id + '",');
  }

  if (label) {
    propStr = propStr.concat('"label":"' + label + '",');
  }

  if (typeData) {
    propStr = propStr.concat('"type":"' + typeData + '",');
  }

  if (columnCount > 0) {
    propStr = propStr.concat('"columnCount": ' + columnCount);
  }

  if (propStr.trim().endsWith(",")) {
    propStr = propStr.trim().substring(0, propStr.length - 1);
  }
  propStr = propStr.concat("}");

  const area: model.AreaNode = JSON.parse(propStr);
  const areaModel = new model.CommonElement(area);

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const fields: any[] = data["tns:field"];
  if (fields) {
    fields.forEach((field) => {
      areaModel.addChild(createField(field, defaultMap));
    });
  }
  return areaModel;
}
