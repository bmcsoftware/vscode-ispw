import { Axios, AxiosResponse } from "axios";
import { XmlResponse, GenerateResponse } from "../models/GenerateWithParmsModel";
import { workspace } from "vscode";
import { Constants } from "../utils/Constants";
import { SettingsUtils } from "../utils/SettingsUtils";
import { TaskModel, TaskResponse } from "../models/TaskModel";
import { GenerateDialogModel, GenerateTaskModel } from "../models/GenerateTaskModel";
import { TaskCleanupModel, TaskCleanupRespose } from "../models/TaskCleanupModel";

const axiosObj = new Axios({});

export class GenerateWithParmsRepo {

  public async getTaskInfo(taskDetails: TaskModel, lpar: string): Promise<TaskResponse> {
    console.log('Fetching details for task : ' + taskDetails.moduleName);
    let url: string = SettingsUtils.getCesUrl() as string;
    if (!url.endsWith(Constants.FORWARD_SLASH)) {
      url = url.concat(Constants.FORWARD_SLASH);
    }
    url = url + Constants.URL_ISPW + lpar + Constants.URL_GET_TASK_DETAILS+taskDetails.moduleName;
    console.log('gettaskdetails : ' + url);
    const token: string = SettingsUtils.getCesToken() as string;
    const requestHeader = {
      'headers': {
        'Content-Type': Constants.CONTENT_TYPE_APPLICATION_JSON,
        'Authorization': token
      },
      'params': {
        'application':taskDetails.application,
        'level': taskDetails.checkoutToLevel,
        'type':taskDetails.moduleType
      }
    };

    return await axiosObj.get(url, requestHeader).then(async function (response : AxiosResponse) {
      let taskInfo: TaskResponse = JSON.parse(response.data);
      taskInfo.status = response.status;
      return taskInfo;
    }).catch(function (error: AxiosResponse) {
      console.error(error);
      throw new Error(error.statusText);
    });
  }


  /**
   * This function makes a REST call to fetch generate with parms XML.
   */
  public async getServerParmXMLAPI(generateDialogModel: GenerateDialogModel, lpar: string): Promise<XmlResponse> {

    let url: string = workspace.getConfiguration().get(Constants.SETTING_KEY_CES_URL) as string;
    if (!url.endsWith(Constants.FORWARD_SLASH)) {
      url = url.concat(Constants.FORWARD_SLASH);
    }
    url = url + Constants.URL_ISPW + lpar + Constants.URL_GENERATE_WITH_PARM;
    console.debug('url : ' + url);
    const token: string = workspace.getConfiguration().get(Constants.SETTING_KEY_CES_TOKEN) as string;
    const requestHeader = {
      'headers': {
        'Content-Type': Constants.CONTENT_TYPE_APPLICATION_JSON,
        'Authorization': token
      }
    };
    const response = await axiosObj
      .post(url, JSON.stringify(generateDialogModel), requestHeader)
      .then(function (response: AxiosResponse) {
        var apiResponse: XmlResponse = JSON.parse(response.data);
        apiResponse.status = response.status;
        return apiResponse;
      })
      .catch(function (error: AxiosResponse) {
        console.error(error);
        throw new Error(error.statusText);
      });

    return response;
  }

  /**
   * This function generates using entered parameters.
   * @returns 
   */
  public async generateWithParms(generateDetails: GenerateTaskModel, lpar: string): Promise<GenerateResponse> {
    let url: string = workspace.getConfiguration().get(Constants.SETTING_KEY_CES_URL) as string;
    if (!url.endsWith(Constants.FORWARD_SLASH)) {
      url = url.concat(Constants.FORWARD_SLASH);
    }
    url = url + Constants.URL_ISPW + lpar + Constants.URL_UPDATE_GENERATE_WITH_PARM;
    const token: string = workspace.getConfiguration().get(Constants.SETTING_KEY_CES_TOKEN) as string;
    const requestHeader = {
      'headers': {
        'Content-Type': Constants.CONTENT_TYPE_APPLICATION_JSON,
        'Authorization': token
      }
    };
    const response = await axiosObj.post(url,
      JSON.stringify(generateDetails), requestHeader).then(function (response : AxiosResponse) {
        const generateResponse: GenerateResponse = JSON.parse(response.data);
        generateResponse.status = response.status;
        return generateResponse;
      })
      .catch(function (error: string | undefined) {
        console.error(error);
        throw new Error(error);
      });
    return response;
  }

  public async taskCleanup(taskCleanup: TaskCleanupModel, lpar: string) {
    let url: string = workspace.getConfiguration().get(Constants.SETTING_KEY_CES_URL) as string;
    if (!url.endsWith(Constants.FORWARD_SLASH)) {
      url = url.concat(Constants.FORWARD_SLASH);
    }
    url = url + Constants.URL_ISPW + lpar + Constants.URL_TASK_CLEANUP + taskCleanup.taskId;
    const token: string = workspace.getConfiguration().get(Constants.SETTING_KEY_CES_TOKEN) as string;
    const requestHeader = {
      'headers': {
        'Content-Type': Constants.CONTENT_TYPE_APPLICATION_JSON,
        'Authorization': token
      }
    };
    const response = await axiosObj.post(url,JSON.stringify(taskCleanup), requestHeader).then(function(cleanupInfo : AxiosResponse) {
      const taskCleanupResponse: TaskCleanupRespose = JSON.parse(cleanupInfo.data);
      taskCleanupResponse.status = cleanupInfo.status;
      return taskCleanupResponse;
    }).catch(function (error: string | undefined) {
      console.error(error);
      throw new Error(error);
    });
    return response;
  }
}