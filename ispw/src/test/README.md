# To run command 'npm run test':

### 1. Modify global vscode setting at location - ``%APPDATA%\Code\User\settings.json``

### 2. Add the following values (modify '/path/to/your/ispw/cli' to your own path):

> **%APPDATA%\Code\User\settings.json**
> ``` json
> {
>     "ISPW.Assignment Description": "{user}-{project_name}",
>     "ISPW.Level": "DEV1",
>     "ISPW.Topaz CLI Installation Path": "/path/to/your/ispw/cli",
> }
> ```

These values are required to run the tests.
