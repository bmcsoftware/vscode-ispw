/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

/**
 * This file contains all the definitions for the interfaces related to ISPW type mappings. These interfaces are used to align with the contents of the ispwconfig.yml file.
 */

/**
 * The IspwRoot interface
 */
export interface IspwRoot {
    ispwApplication: IspwApplication;
}

/**
 * Interface for the ISPW application information
 */
export interface IspwApplication {
    stream: string;
    application: string;
    host: string;
    port: number;
    runtimeConfig: string;
    hostCodePage?: number;
    readWriteTimeout?: number;
    encryptionProtocol?: string;
    
    pathMappings: IspwPath[];
}

/**
 * Interface of the yaml path information
 */
export interface IspwPath {
    path: string;
    types: IspwType[];
}

/**
 * Interface for the ISPW type information
 */
export interface IspwType {
    ispwType: string;
    fileExtension: string;
    genSeq?: string;
    progType?: string;
    sql?: string;
    cics?: string;
    ims?: string;
    flag1?: string;
    flag2?: string;
    flag3?: string;
    flag4?: string;
    genParms?: string;
}