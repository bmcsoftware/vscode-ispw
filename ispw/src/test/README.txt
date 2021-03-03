To run command 'npm run test',

Modify global vscode setting at location - %APPDATA%\Code\User\settings.json,
and add the following values (modify '/path/to/your/ispw/cli', for example, 'c:\\tmp\\cli200601'):

%APPDATA%\Code\User\settings.json
---------------------------------
{
    "ISPW.Assignment Description": "{user}-{project_name}",
    "ISPW.Level": "DEV1",
    "ISPW.Topaz CLI Installation Path": "/path/to/your/ispw/cli",
}
---------------------------------

These values are required to run the test.

