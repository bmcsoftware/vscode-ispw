/**
* THESE MATERIALS CONTAIN CONFIDENTIAL INFORMATION AND TRADE SECRETS OF BMC SOFTWARE, INC. YOU SHALL MAINTAIN THE MATERIALS AS
* CONFIDENTIAL AND SHALL NOT DISCLOSE ITS CONTENTS TO ANY THIRD PARTY EXCEPT AS MAY BE REQUIRED BY LAW OR REGULATION. USE,
* DISCLOSURE, OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF BMC SOFTWARE, INC.
*
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
*/

export abstract class Constants {
    static readonly SETTING_KEY_ASSIGN_DESC: string = 'ISPW.Assignment Description';
    static readonly SETTING_KEY_LOAD_LEVEL: string = 'ISPW.Level';
    static readonly SETTING_KEY_CLI_LOC: string = 'ISPW.Topaz CLI Installation Path';
    static readonly SETTING_KEY_YAML_LOC: string = 'ISPW.YAML Mapping File';

    static readonly CMD_BUILD_EXPLORER : string = 'ISPW.buildExplorer';
    static readonly CMD_GENERATE_EXPLORER : string = 'ISPW.generateExplorer';
    static readonly CMD_LOAD_EXPLORER : string = 'ISPW.loadExplorer';
    static readonly CMD_BUILD_EDITOR : string = 'ISPW.buildEditor';
    static readonly CMD_GENERATE_EDITOR : string = 'ISPW.generateEditor';
    static readonly CMD_LOAD_EDITOR : string = 'ISPW.loadEditor';
    static readonly CMD_CLEAR_CREDS : string = 'ISPW.clearCreds';

    static readonly OP_BUILD : string = 'build';
    static readonly OP_LOAD : string = 'load';
    static readonly OP_GENERATE : string = 'generate';

    static readonly EMPTY_STRING : string = '';

    static readonly ISPW_CONFIG_YAML : string = 'ispwconfig.yml';
}