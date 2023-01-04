import { ISPWDialogModelService } from "../services/ISPWDialogModelService";
import {GenerateResponse, HtmlFormData} from "../models/GenerateWithParmsModel";
import { TaskModel, TaskResponse } from "../models/TaskModel";
import { GenerateDialogModel, GenerateTaskModel } from "../models/GenerateTaskModel";
import { TaskCleanupModel, TaskCleanupRespose } from "../models/TaskCleanupModel";

export class GenerateWithParmsController {

  private _modelService: ISPWDialogModelService;
  
  constructor(type: string) {
    this._modelService = new ISPWDialogModelService(type);
  }

  public async getTaskDetails(taskDetails: TaskModel, lpar: string) : Promise<TaskResponse | Error> {
    return await this._modelService.getTaskDetails(taskDetails, lpar);
  }

  public async generateHTML(generateDialogModel: GenerateDialogModel, lpar: string): Promise<HtmlFormData | Error> {
    return await this._modelService.createGenParmDialogModel(generateDialogModel, lpar);
  }

  public async generateAction(generateDetails: GenerateTaskModel, lpar: string): Promise<GenerateResponse | Error> {
    return await this._modelService.generateAction(generateDetails, lpar);
  }

  public async taskCleanup(taskCleanup: TaskCleanupModel, lpar: string): Promise<TaskCleanupRespose | Error> {
    return await this._modelService.taskCleanup(taskCleanup, lpar);
  }
}
