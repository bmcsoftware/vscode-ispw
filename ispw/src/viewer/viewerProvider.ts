import * as model from "../models/DialogModel";
export interface IViewerProvider {
  getHTML(model: model.CommonElement): string;
}
