import * as assert from 'assert';
import * as IspwCliCommand from '../../commands/CliCommand';
import { CredentialsCache } from '../../types/CredentialsCache';
import { IspwType, IspwRoot, IspwPath, IspwApplication } from '../../types/IspwTypeMapping';
import { CliArgs } from '../../types/CliArgs';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
// import * as myExtension from '../../extension';

import { CliUtils } from '../../utils/CliUtils';

suite('Extension Test Suite', function () {
	vscode.window.showInformationMessage('Start all tests.');

	this.beforeAll(() => { });
	this.beforeEach(() => { });
	this.afterAll(() => { });
	this.afterEach(() => { });

	/**
	 * Test CredentialsCache
	 */
	test('test credentials cache', () => {
		let cc = CredentialsCache.getInstance();

		let user1 = 'user1';
		let pass1 = 'pass1';

		cc.saveCredentials(user1, pass1);
		let cred = cc.getCredentials();
		assert.strictEqual(cred.username, user1);
		assert.strictEqual(cred.password, pass1);

		cc.clearCredentials();
		cred = cc.getCredentials();
		assert.strictEqual(cred.username, undefined);
		assert.strictEqual(cred.password, undefined);

		let user2 = 'user2';
		let pass2 = 'pass2';

		cc.saveCredentials(user2, pass2);
		cred = cc.getCredentials();
		assert.strictEqual(cred.username, user2);
		assert.strictEqual(cred.password, pass2);

		cc.clearCredentials();
	});

	/**
	 * Test IspwTypeMapping
	 */
	test('test ispw yaml type', () => {
		let ispwTypeCob = {
			ispwType: 'COB', fileExtension: 'cbl', genSeq: 'V', progType: 'Yes',
			sql: 'No', cics: 'No', ims: 'No', flag1: 'N', flag2: 'I', flag3: 'C', flag4: 'E', genParms: 'ISPWCUTE'
		}

		let ispwTypeCobExpect = {
			ispwType: 'COB', fileExtension: 'cbl', genSeq: 'V', progType: 'Yes',
			sql: 'No', cics: 'No', ims: 'No', flag1: 'N', flag2: 'I', flag3: 'C', flag4: 'E', genParms: 'ISPWCUTE'
		}

		let ispwTypeClst = {
			ispwType: 'CLST', fileExtension: 'clst'
		}

		let ispwTypeClstExpect = {
			ispwType: 'CLST', fileExtension: 'clst'
		}

		let ispwPathCob = {
			path: '\\COB', types: { ispwTypeCob }
		}

		let ispwPathClst = {
			path: '\\CLST', types: { ispwTypeClst }
		}

		let ispwApplication = {
			stream: 'PLAY-S',
			application: 'PLAY-A',
			host: 'CW09',
			port: 47623,
			runtimeConfig: 'TPZP',
			pathMappings: { ispwPathCob, ispwPathClst }
		}

		let ispwRoot = {
			ispwApplication: ispwApplication
		}

		assert.strictEqual(ispwRoot.ispwApplication.application, 'PLAY-A');
		assert.strictEqual(ispwRoot.ispwApplication.stream, 'PLAY-S');
		assert.strictEqual(ispwRoot.ispwApplication.port, 47623);
		assert.strictEqual(ispwRoot.ispwApplication.runtimeConfig, 'TPZP');
		assert.strictEqual(ispwRoot.ispwApplication.pathMappings.ispwPathCob.path, '\\COB');
		assert.strictEqual(ispwRoot.ispwApplication.pathMappings.ispwPathClst.path, '\\CLST');

		let ispwTypeCob1 = ispwRoot.ispwApplication.pathMappings.ispwPathCob.types.ispwTypeCob;
		assert.strictEqual(ispwTypeCob === ispwTypeCob1, true);
		assert.strictEqual(ispwTypeCob1 === ispwTypeCobExpect, false);
		compareObject(ispwTypeCob1, ispwTypeCobExpect);

		let ispwTypeClst1 = ispwRoot.ispwApplication.pathMappings.ispwPathClst.types.ispwTypeClst;
		assert.strictEqual(ispwTypeClst1 === ispwTypeClst, true);
		assert.strictEqual(ispwTypeClst1 === ispwTypeClstExpect, false);
		compareObject(ispwTypeClst1, ispwTypeClstExpect);

	});

	test('test cli arguments type', () => {
		let cliArgs = {
			codePage: 'codepage',
			help: 'help',
			host: 'host',
			password: 'password',
			port: 123,
			timeout: 456,
			username: 'username',
			protocol: 'protocol',
			targetFolder: 'targetfolder',
			serverConfig: 'serverconfig',
			operation: 'operation',
			gitRepoUrl: 'gitrepourl',
			gitUsername: 'gitusername',
			gitPassword: 'gitpassword',
			gitBranch: 'gitbranch',
			gitCommit: 'gitcommit',
			gitFromHash: 'gitfromhash',
			stream: 'stream',
			application: 'application',
			checkoutLevel: 'checkoutlevel',
			containerCreation: 'containercreation',
			customDescription: 'customerdescription',
			gitLocalPath: 'gitlocalpath',
			ispwConfigPath: 'ispwconfigpath',
			gitCommitFile: 'gitcommitfile',
			vscSetting: 'vscsetting',
			componentFiles: 'componentfile',
			ispwGitAssignDesc: 'ispwgitassigndesc',
			ispwMappingLevel: 'ispwmappinglevel',
			typeOverride: 'typeoverride'
		}

		assert.strictEqual(cliArgs.codePage, 'codepage');
		assert.strictEqual(cliArgs.help, 'help');
		assert.strictEqual(cliArgs.host, 'host');
		assert.strictEqual(cliArgs.password, 'password');
		assert.strictEqual(cliArgs.port, 123);
		assert.strictEqual(cliArgs.timeout, 456);
		assert.strictEqual(cliArgs.username, 'username');
		assert.strictEqual(cliArgs.protocol, 'protocol');
		assert.strictEqual(cliArgs.targetFolder, 'targetfolder');
		assert.strictEqual(cliArgs.serverConfig, 'serverconfig');
		assert.strictEqual(cliArgs.operation, 'operation');
		assert.strictEqual(cliArgs.gitRepoUrl, 'gitrepourl');
		assert.strictEqual(cliArgs.gitUsername, 'gitusername');
		assert.strictEqual(cliArgs.gitPassword, 'gitpassword');
		assert.strictEqual(cliArgs.gitBranch, 'gitbranch');
		assert.strictEqual(cliArgs.gitCommit, 'gitcommit');
		assert.strictEqual(cliArgs.gitFromHash, 'gitfromhash');
		assert.strictEqual(cliArgs.stream, 'stream');
		assert.strictEqual(cliArgs.application, 'application');
		assert.strictEqual(cliArgs.checkoutLevel, 'checkoutlevel');
		assert.strictEqual(cliArgs.containerCreation, 'containercreation');
		assert.strictEqual(cliArgs.customDescription, 'customerdescription');
		assert.strictEqual(cliArgs.gitLocalPath, 'gitlocalpath');
		assert.strictEqual(cliArgs.ispwConfigPath, 'ispwconfigpath');
		assert.strictEqual(cliArgs.gitCommitFile, 'gitcommitfile');
		assert.strictEqual(cliArgs.vscSetting, 'vscsetting');
		assert.strictEqual(cliArgs.componentFiles, 'componentfile');
		assert.strictEqual(cliArgs.ispwGitAssignDesc, 'ispwgitassigndesc');
		assert.strictEqual(cliArgs.ispwMappingLevel, 'ispwmappinglevel');
		assert.strictEqual(cliArgs.typeOverride, 'typeoverride');
	});


	test('Sample test', () => {
		assert.strictEqual(-1, [1, 2, 3].indexOf(5));
		assert.strictEqual(-1, [1, 2, 3].indexOf(0));
	});

	test('Sample test', () => {
		assert.strictEqual(-1, [1, 2, 3].indexOf(5));
		assert.strictEqual(-1, [1, 2, 3].indexOf(0));
	});

	test('test', async () => {
		/*
		let cliLocation: string | undefined = CliUtils.getCliLocation();
		let loadLevel: string | undefined = CliUtils.getLoadLevel();
		let assignmentDesc: string | undefined = CliUtils.getAssignmentDescription();

		const ispwlevel = vscode.workspace.getConfiguration("ispw").get("ispw.Level");
		console.log(ispwlevel);
		*/

		await vscode.commands.executeCommand('ispw.clearCreds');
	});

	test('test', async () => {
		//await subscribeToTsConfigChanges();
		const document = await vscode.workspace.openTextDocument('rjk2/COB/TPROG03.cbl');
		await vscode.window.showTextDocument(document);
		//vscode.commands.executeCommand('ispw.load');
		//IspwCliCommand.runCommand('load', vscode.window.activeTextEditor?.document.uri);
	});

	//vscode.window.showInformationMessage('End all tests.');

});


function compareObject(object1: Object, object2: Object) {
	let props = Object.getOwnPropertyNames(object1);
	props.forEach(prop => {
		//console.log(`object1.${prop}=`, Reflect.get(object1, prop), `, object2.${prop}=`, Reflect.get(object2, prop));
		assert.strictEqual(Reflect.get(object1, prop), Reflect.get(object2, prop));
	});
}


function subscribeToTsConfigChanges(): vscode.Disposable[] {
	const disposables: vscode.Disposable[] = [];
	for (const workfolder of vscode.workspace.workspaceFolders || []) {
		const pattern = new vscode.RelativePattern(workfolder, "[tj]sconfig.json");
		const fileWatcher = vscode.workspace.createFileSystemWatcher(pattern);
		fileWatcher.onDidChange(() => invalidateCache(workfolder));
		disposables.push(fileWatcher);
	}
	return disposables;
}

function invalidateCache(workfolder: vscode.WorkspaceFolder) {
}