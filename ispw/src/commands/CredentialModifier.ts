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

import { Credentials, CredentialsCache } from "../types/CredentialsCache";
import { CredentialsUtils } from "../utils/CredentialsUtils";

/**
 * This function handles updating the stored credentials. If the process is cancelled at any time, the credentials will not be updated.
 */
export async function modifyCredentials(): Promise<void> {
    console.debug("Beginning modifyCredentials");
    let credCache: CredentialsCache = CredentialsCache.getInstance();
    let credentials: Credentials = await CredentialsUtils.promptForCredentials();
    if (CredentialsUtils.validateCredentials(credentials)) {
        credCache.saveCredentials(credentials.username, credentials.password);
        console.debug("Credentials were updated");
    }
    else {
        console.debug("Credentials not updated");
        return;
    }
}

/**
 * This function handles clearing the stored credentials.
 */
export function clearCredentials(): void {
    console.debug("Beginning clearCredentials");
    let credCache = CredentialsCache.getInstance();
    credCache.clearCredentials();
}