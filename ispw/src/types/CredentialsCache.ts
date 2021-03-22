/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

/**
 * A Credentials type which holds the username and password. If a username is present, that does not guarantee the presence of a password.
 */
export interface Credentials {
    username: string | undefined;
    password: string | undefined;
}

/**
 * The cache to handle storing a single username and password for a session. This class is a singleton.
 */
export class CredentialsCache {
    private static instance: CredentialsCache;
    private username: string | undefined;
    private password: string | undefined;

    /**
     * private Constructor - do not use. 
     */
    private constructor() {

    }

    /**
     * Method to get the instance of the singleton cache
     */
    public static getInstance(): CredentialsCache {
        if (CredentialsCache.instance === undefined) {
            CredentialsCache.instance = new CredentialsCache();
        }
        return CredentialsCache.instance;
    }

    /**
     * Retrieves the stored username and password and returns a Credentials object. The username and password are not guaranteed to be defined. The password will not be encrypted.
     */
    public getCredentials(): Credentials {
        return { 
            username: this.username, 
            password: this.password 
        };
    }

    /**
     * Takes the given username and password and stores them in the cache.
     * @param newUsername The string username to store
     * @param newPassword The string password to store. This should not be encrypted.
     */
    public saveCredentials(newUsername: string | undefined, newPassword: string | undefined): void {
        this.username = newUsername;
        this.password = newPassword;
    }

    /**
     * Clears the username and password from the cache, sets the values to undefined.
     */
    public clearCredentials(): void {
        this.username = undefined;
        this.password = undefined;
    }

}