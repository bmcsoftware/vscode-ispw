# README

This repository contains the source for BMC AMI DevX Workbench Code Pipeline for Git extension for VS Code. The extension can be installed from [here](https://marketplace.visualstudio.com/items?itemName=BMCCompuware.ISPW).

## Getting started using the repository

1. Install nodejs
2. Clone this repository
3. From the ispw folder, run `npm install`
4. Open VSCode, set the ispw folder as your workspace root
5. To compile your changes, in the terminal view run `npm run compile`
6. To launch a new VSCode window that is running with your changes, go to the Run view, then select "Launch Code Pipeline for Git Extension" and press the play button.

## Running tests

Before attempting to run tests, update the settings file at `%APPDATA%\Code\User\settings.json` to have the following:

``` json
    "Code Pipeline for Git.Assignment Description": "${user}-${project_name}",
    "Code Pipeline for Git.Level": "DEV1",
    "Code Pipeline for Git.Workbench CLI Installation Path": "your cli path",
```

Then, add a file to your ispw/src/test folder called `credentials.json` and add the following:

``` json
{
    "username": "myUsername",
    "password": "myPassword"
}
```

Where the username and password are the one you want to have used when running the tests.

Once the files have been set up, you can execute the tests. To do so, open a command line window inside the ispw folder of this repository and run `npm run test`

You can also launch the tests from within VS Code by using the launch configuration "Test Code Pipeline for Git Extension with Demo"

## Other development tips

* To format your code changes, use  Alt + Shift + F

* If you add any new dependencies on external libraries which will be needed at runtime, make sure the library name is added to the dependencies section of the package.json file _not just the devDependencies section_. If you do not do this, the extension will fail to activate when it is installed.
