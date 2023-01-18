/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

export abstract class Constants {
    static readonly SETTING_KEY_ASSIGN_DESC: string = 'ISPW.Assignment Description';
    static readonly SETTING_KEY_LOAD_LEVEL: string = 'ISPW.Level';
    static readonly SETTING_KEY_CLI_LOC: string = 'ISPW.Topaz CLI Installation Path';
    static readonly SETTING_KEY_YAML_LOC: string = 'ISPW.YAML Mapping File';
    static readonly SETTING_KEY_CES_URL: string = 'ISPW.CES URL';
    static readonly SETTING_KEY_CES_TOKEN: string = 'ISPW.Security Token';

    static readonly CMD_BUILD_EXPLORER : string = 'ISPW.buildExplorer';
    static readonly CMD_GENERATE_EXPLORER : string = 'ISPW.generateExplorer';
    static readonly CMD_LOAD_EXPLORER : string = 'ISPW.loadExplorer';
    static readonly CMD_GENERATE_WITH_PARMS_EDITOR = 'ISPW.generateWithParmsEditor';
    static readonly CMD_BUILD_EDITOR : string = 'ISPW.buildEditor';
    static readonly CMD_GENERATE_EDITOR : string = 'ISPW.generateEditor';
    static readonly CMD_LOAD_EDITOR : string = 'ISPW.loadEditor';
    static readonly CMD_CLEAR_CREDS : string = 'ISPW.clearCreds';
    static readonly CMD_GENERATE_WITH_PARMS_EXPLORER = 'ISPW.generateWithParmsExplorer';

    static readonly OP_BUILD : string = 'build';
    static readonly OP_LOAD : string = 'load';
    static readonly OP_GENERATE : string = 'generate';
    static readonly OP_GENERATE_WITH_PARMS = 'generate with parms';

    static readonly EMPTY_STRING : string = '';

    static readonly ISPW_CONFIG_YAML : string = 'ispwconfig.yml';

    static readonly FORWARD_SLASH = '/';
    static readonly URL_ISPW = 'ispw' + Constants.FORWARD_SLASH;
    static readonly URL_GENERATE_WITH_PARM = Constants.FORWARD_SLASH + 'generateWithParam';
    static readonly URL_UPDATE_GENERATE_WITH_PARM = Constants.FORWARD_SLASH + 'updateGenerateWithParam';
    static readonly URL_CREATE_ASSIGNMENT_CONTAINER = Constants.FORWARD_SLASH + 'assignments';
    //static readonly URL_ADD_TASK = Constants.FORWARD_SLASH + 'task/add?checkout=true';
    static readonly URL_GET_TASK_DETAILS = Constants.FORWARD_SLASH + 'tasks' + Constants.FORWARD_SLASH;
    static readonly URL_TASK_CLEANUP = Constants.FORWARD_SLASH + 'tasks' + Constants.FORWARD_SLASH;

    static readonly CONTENT_TYPE_APPLICATION_JSON = 'application/json';
}