import * as model from "../models/DialogModel";
import { XmlResponse, GenerateResponse, KeyVal, HtmlFormData } from "../models/GenerateWithParmsModel";
import * as util from "../utils/Util";
import {GenerateWithParmsRepo} from "../repository/GenerateWithParmsRepo";
import {
  WebViewUIComponentViewerProvider,
  DefaultViewerProvider,
} from "../viewer/DialogViewerProvider";
import { IViewerProvider } from "../viewer/viewerProvider";
import { TaskModel, TaskResponse } from "../models/TaskModel";
import { GenerateDialogModel, GenerateTaskModel } from "../models/GenerateTaskModel";
import { TaskCleanupModel, TaskCleanupRespose } from "../models/TaskCleanupModel";

export interface IDialogModelService {
  getTaskDetails(taskDetails: TaskModel, lpar: string) : Promise<TaskResponse | Error>;
  createGenParmDialogModel(generateDialogModel: GenerateDialogModel, lpar: string): Promise<HtmlFormData | Error>;
  generateAction(generateDetails: GenerateTaskModel, lpar: string): Promise<GenerateResponse | Error>;
}

export class ISPWDialogModelService implements IDialogModelService {
  private _viewerProvider: IViewerProvider;
  private _type: string;
  private _repo: GenerateWithParmsRepo;
  
  constructor(type: string) {
    this._type = type;
    if (this._type === "WebViewUIComponent") {
      this._viewerProvider = new WebViewUIComponentViewerProvider();
    } else {
      this._viewerProvider = new DefaultViewerProvider();
    }
    this._repo = new GenerateWithParmsRepo();
  }

  async getTaskDetails(taskDetails: TaskModel, lpar: string) : Promise<TaskResponse | Error> {
    return await this._repo.getTaskInfo(taskDetails, lpar).then(function(response) {
      if (response.status !== 200) {
        return new Error(response.message);
      }
      return response;
    });
  }

  async createGenParmDialogModel(generateDialogModel: GenerateDialogModel, lpar: string): Promise<HtmlFormData | Error> {
    return await this._repo.getServerParmXMLAPI(generateDialogModel, lpar).then((response : XmlResponse) => {
      if(response.status !== 200) {
        return new Error(response.message);
      }
      const dialogModel =  util.createDialogModel(response.xml, response.defaults.entry);
      if (dialogModel) {
        const children: model.ISPWNode[] = dialogModel.getChildren();
        let html = "";

        if (children) {
          children.forEach((child) => {
              const areaNode: model.CommonElement = <model.CommonElement>child;
              html = html.concat(this._viewerProvider.getHTML(areaNode));
          });
        }

        return { 'html': html, 
        title: dialogModel.label, 
        componentType: dialogModel.node?.type, 
        componentName: dialogModel.node?.label,
        setId: response.setId 
      };
        
      } else {
        throw new Error("invalid model");
      }
    });
  }

  async generateAction(generateDetails: GenerateTaskModel, lpar: string): Promise<GenerateResponse | Error> {
    const generateResponse: GenerateResponse = await this._repo.generateWithParms(generateDetails, lpar);
    if(generateResponse.status !== 200) {
      return new Error(generateResponse.message);
    }
    return generateResponse;
  }

  async taskCleanup(taskCleanup: TaskCleanupModel, lpar: string): Promise<TaskCleanupRespose | Error> {
    const taskCleanupResponse: TaskCleanupRespose = await this._repo.taskCleanup(taskCleanup, lpar);
    if(taskCleanupResponse.status !== 200) {
      return new Error(taskCleanupResponse.message);
    }
    return taskCleanupResponse;
  }

}
