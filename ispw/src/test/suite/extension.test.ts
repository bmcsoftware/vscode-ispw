/**
* ALL BMC SOFTWARE PRODUCTS LISTED WITHIN THE MATERIALS ARE TRADEMARKS OF BMC SOFTWARE, INC. ALL OTHER COMPANY PRODUCT NAMES
* ARE TRADEMARKS OF THEIR RESPECTIVE OWNERS.
*
* (c) Copyright 2021 BMC Software, Inc.
* This code is licensed under MIT license (see LICENSE.txt for details)
*/

import * as assert from 'assert';
import { CredentialsCache } from '../../types/CredentialsCache';
import { IspwType, IspwRoot, IspwPath, IspwApplication } from '../../types/IspwTypeMapping';
import { CliArgs } from '../../types/CliArgs';
import { YamlUtils } from '../../utils/YamlUtils';
import { MessageUtils } from "../../utils/MessageUtils";
import { CliUtils } from '../../utils/CliUtils';
import { SettingsUtils } from '../../utils/SettingsUtils';
import { Constants } from '../../utils/Constants';
import { CommonUtils } from '../../utils/CommonUtils';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
// import * as myExtension from '../../extension';

suite('Extension Test Suite', function () {
	vscode.window.showInformationMessage('Start all tests.');

	this.beforeAll(() => { });

	this.beforeEach(() => {
		let cc = CredentialsCache.getInstance();
		cc.saveCredentials('xdevreg', 'regress');
	});

	this.afterAll(() => { });

	this.afterEach(() => {
		let cc = CredentialsCache.getInstance();
		cc.clearCredentials();
	});

	/**
	 * Test common utils
	 */
	test('Test common utils', () => {
		assert.strictEqual(CommonUtils.isBlank(' '), true);
		assert.strictEqual(CommonUtils.isBlank(' ispw '), false);
		assert.strictEqual(CommonUtils.isBlank(undefined), true);
		assert.strictEqual(CommonUtils.isBlank(null), true);
		assert.strictEqual(CommonUtils.isBlank(''), true);
		assert.strictEqual(CommonUtils.isBlank([]), true);
		assert.strictEqual(CommonUtils.isBlank({}), true);
		assert.strictEqual(CommonUtils.isBlank(NaN), true);
		assert.strictEqual(CommonUtils.isBlank(0), false);
		assert.strictEqual(CommonUtils.isNotBlank([0, 1]), true);
		assert.strictEqual(CommonUtils.isNotBlank('ispw'), true);
		assert.strictEqual(CommonUtils.escapeString(' '), '\" \"');
		assert.strictEqual(CommonUtils.escapeString('this\\is a\\path\\string'), '\"this\\is a\\path\\string\"');
		assert.strictEqual(CommonUtils.escapeString(undefined), '\"\"');
		assert.strictEqual(CommonUtils.escapeString(1234), '\"1234\"');
	});

	/**
	 * Test CredentialsCache
	 */
	test('Test credentials cache', () => {
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
	test('Test ispw yaml type', () => {
		let ispwTypeCob: IspwType = {
			ispwType: 'COB', fileExtension: 'cbl', genSeq: 'V', progType: 'Yes',
			sql: 'No', cics: 'No', ims: 'No', flag1: 'N', flag2: 'I', flag3: 'C', flag4: 'E', genParms: 'ISPWCUTE'
		};

		let ispwTypeCobExpect: IspwType = {
			ispwType: 'COB', fileExtension: 'cbl', genSeq: 'V', progType: 'Yes',
			sql: 'No', cics: 'No', ims: 'No', flag1: 'N', flag2: 'I', flag3: 'C', flag4: 'E', genParms: 'ISPWCUTE'
		};

		let ispwTypeClst: IspwType = {
			ispwType: 'CLST', fileExtension: 'clst'
		};

		let ispwTypeClstExpect: IspwType = {
			ispwType: 'CLST', fileExtension: 'clst'
		};

		let ispwPathCob: IspwPath = {
			path: '\\COB', types: [ispwTypeCob]
		};

		let ispwPathClst: IspwPath = {
			path: '\\CLST', types: [ispwTypeClst]
		};

		let ispwApplication: IspwApplication = {
			stream: 'PLAY-S',
			application: 'PLAY-A',
			host: 'CW09',
			port: 47623,
			runtimeConfig: 'TPZP',
			pathMappings: [ispwPathCob, ispwPathClst]
		};

		let ispwRoot: IspwRoot = {
			ispwApplication: ispwApplication
		};

		assert.strictEqual(ispwRoot.ispwApplication.application, 'PLAY-A');
		assert.strictEqual(ispwRoot.ispwApplication.stream, 'PLAY-S');
		assert.strictEqual(ispwRoot.ispwApplication.port, 47623);
		assert.strictEqual(ispwRoot.ispwApplication.runtimeConfig, 'TPZP');
		assert.strictEqual(ispwRoot.ispwApplication.pathMappings[0].path, '\\COB');
		assert.strictEqual(ispwRoot.ispwApplication.pathMappings[1].path, '\\CLST');

		let ispwTypeCob1 = ispwRoot.ispwApplication.pathMappings[0].types[0];
		assert.strictEqual(ispwTypeCob === ispwTypeCob1, true);
		assert.strictEqual(ispwTypeCob1 === ispwTypeCobExpect, false);
		compareObject(ispwTypeCob1, ispwTypeCobExpect);

		let ispwTypeClst1 = ispwRoot.ispwApplication.pathMappings[1].types[0];
		assert.strictEqual(ispwTypeClst1 === ispwTypeClst, true);
		assert.strictEqual(ispwTypeClst1 === ispwTypeClstExpect, false);
		compareObject(ispwTypeClst1, ispwTypeClstExpect);

	});

	/**
	 * Test cli arguments type
	 */
	test('Test cli arguments type', () => {
		let cliArgs: CliArgs = {
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
		};

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

	/**
	 * Test yaml utils, functions depreciated will not be tested
	 */
	test('Test YAML utils', async () => {
		let rjk2 = getRjk2();

		if (rjk2 === undefined) {
			assert.fail('Failed to find rjk2 test project');
		} else {
			let tprog03 = vscode.Uri.file(rjk2.fsPath + '\\COB\\TPROG03.cbl');

			let ispwConfigPath = await YamlUtils.getYamlLocationAbsPath(tprog03);
			console.log('ispwConfigPath=', ispwConfigPath);
			assert.strictEqual(ispwConfigPath.endsWith('demo-workspace\\rjk2\\ispwconfig.yml'), true);

			assert.strictEqual(await YamlUtils.hasYaml(false, tprog03), true);
			assert.strictEqual(await YamlUtils.getYamlLocationRelPath(tprog03), "ispwconfig.yml");
			
			/*
			let ispwRoot = YamlUtils.loadYaml(tprog03);
			assert.strictEqual(ispwRoot !== undefined, true);

			assert.strictEqual(ispwRoot?.ispwApplication.encryptionProtocol, 'None');
			assert.strictEqual(ispwRoot?.ispwApplication.hostCodePage, '1047');
			assert.strictEqual(ispwRoot?.ispwApplication.readWriteTimeout, 90);
			*/
		}
	});

	/**
	 * Test vscode settings
	 */
	test('Test vscode settings', () => {
		let cliLocation: string | undefined = SettingsUtils.getCliLocation();
		assert.strictEqual(CommonUtils.isNotBlank(cliLocation), true);
		console.log('cliLocation=', cliLocation);

		let loadLevel: string | undefined = SettingsUtils.getLoadLevel();
		assert.strictEqual(loadLevel, 'DEV1');
		console.log('loadLevel=', loadLevel);

		let assignmentDesc: string | undefined = SettingsUtils.getAssignmentDescription();
		assert.strictEqual(assignmentDesc, '{user}-{project_name}');
		console.log('assignmentDesc=', assignmentDesc);
	});

	/**
	 * Test load a specific cobol fle
	 */
	test('Test load', async () => {
		return testOperation(Constants.OP_LOAD, 'TPROG01.cbl');
	}).timeout(60000);

	/**
	 * Test generate a specific cobol fle
	 */
	test('Test generate', async () => {
		return testOperation(Constants.OP_GENERATE, 'TPROG03.cbl');
	}).timeout(90000);

	/**
	 * Test build a specific cobol fle
	 */
	test('Test build', async () => {
		return testOperation(Constants.OP_BUILD, 'TPROG01.cbl');
	}).timeout(90000);

});

/**
 * Integration test for operation
 * 
 * @param operation the operation to be performed
 */
async function testOperation(operation: string, program: string) {

	//await vscode.window.showTextDocument(document);

	let rjk2 = getRjk2();
	if (rjk2 === undefined) {
		assert.fail('Failed to find rjk2 test project');
	}

	let tprog = vscode.Uri.file(rjk2.fsPath + '\\COB\\' + program);
	let progs: vscode.Uri[] = [tprog];

	let child = await CliUtils.runCliCommandForOperation(operation, progs);
	let fileNameToShow = CliUtils.getFileNameToShow(progs);

	MessageUtils.showInfoMessage("Start " + operation + " test on program - " + program + "...");

	return new Promise<void>((done) => {
		if (child !== undefined) {
			child.on('close', code => {
				if (code === 0) {
					// pass
					MessageUtils.showInfoMessage("The " + operation + " process completed for " + fileNameToShow);
					assert.ok(operation + ' passed');
				}
				else {
					// fail
					MessageUtils.showErrorMessage("The " + operation + " process failed for " + fileNameToShow + ". Check the ISPW Output channel for more information.");
					assert.fail(operation + ' failed');
				}

				done();
			});
		} else {
			assert.fail('failed to create ' + operation + ' child process');
		}
	});
}

/**
 * Compare two objects using reflection
 * 
 * @param object1 object 1
 * @param object2 object 2
 */
function compareObject(object1: Object, object2: Object) {
	let props = Object.getOwnPropertyNames(object1);
	props.forEach(prop => {
		//console.log(`object1.${prop}=`, Reflect.get(object1, prop), `, object2.${prop}=`, Reflect.get(object2, prop));
		assert.strictEqual(Reflect.get(object1, prop), Reflect.get(object2, prop));
	});
}

/**
 * Get rjk2 project
 */
function getRjk2(): vscode.Uri | undefined {
	let wfs = vscode.workspace.workspaceFolders;
	let rjk2: vscode.Uri | undefined = undefined;

	wfs?.forEach(x => {
		if (x.name === 'rjk2') {
			rjk2 = x.uri;
		}
	});

	return rjk2;
}
