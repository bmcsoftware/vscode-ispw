# To run command 'npm run test':

### 1. Add a credentials file to this folder

The file should contain the credentials to be used to log on to ISPW when running the tests.
> **credentials.json**
> ``` json
> {
>     "username": "myUsername",
>     "password": "myPassword"
> }
> ```

### 2. Modify global vscode setting at location - ``%APPDATA%\Code\User\settings.json``

### 3. Add the following values (modify '/path/to/your/ispw/cli' to your own path):

> **%APPDATA%\Code\User\settings.json**
> ``` json
> {
>     "ISPW.Assignment Description": "{user}-{project_name}",
>     "ISPW.Level": "DEV1",
>     "ISPW.Workbench CLI Installation Path": "/path/to/your/ispw/cli",
> }
> ```

These values are required to run the tests.
