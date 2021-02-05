import * as assert from 'assert';
import * as IspwCliCommand from '../../commands/CliCommand';
import { CredentialsCache } from '../../types/CredentialsCache';
import { IspwType, IspwRoot, IspwPath, IspwApplication } from '../../types/IspwTypeMapping';
import { CliArgs } from '../../types/CliArgs';
import { YamlUtils } from '../../utils/YamlUtils'

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
// import * as myExtension from '../../extension';

import { CliUtils } from '../../utils/CliUtils';

suite('Extension Test Suite', function () {
	vscode.window.showInformationMessage('Start all tests.');

	this.beforeAll(() => {
	});
	this.beforeEach(() => { });
	this.afterAll(() => { });
	this.afterEach(() => { });

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

	test('Test cli arguments type', () => {
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

	test('Test YAML utils', () => {
		let rjk2 = getRjk2();

		if (rjk2 === undefined)
			assert.fail('Failed to find rjk2 test project')
		else {
			let tprog03 = vscode.Uri.file(rjk2.fsPath + '\\COB\\TPROG03.cbl');

			let ispwConfigPath = YamlUtils.getYamlLocationAbsPath(tprog03);
			console.log('ispwConfigPath=', ispwConfigPath);
			assert.strictEqual(ispwConfigPath.endsWith('demo-workspace\\rjk2\\ispwconfig.yml'), true);

			assert.strictEqual(YamlUtils.hasYaml(tprog03), true);
			assert.strictEqual(YamlUtils.getYamlLocationRelPath(tprog03), "ispwconfig.yml");
		}
	});

	test('Test vscode settings', () => {
		let cliLocation: string | undefined = CliUtils.getCliLocation();
		assert.strictEqual(cliLocation !== undefined && cliLocation !== '', true);
		console.log('cliLocation=', cliLocation);

		let loadLevel: string | undefined = CliUtils.getLoadLevel();
		assert.strictEqual(loadLevel, 'DEV1');
		console.log('loadLevel=', loadLevel);

		let assignmentDesc: string | undefined = CliUtils.getAssignmentDescription();
		assert.strictEqual(assignmentDesc, '${user}-${project_name}');
		console.log('assignmentDesc=', assignmentDesc);
	});

	test('Test open doc', async () => {
		//await subscribeToTsConfigChanges();
		const document = await vscode.workspace.openTextDocument('rjk2/COB/TPROG03.cbl');
		await vscode.window.showTextDocument(document);
		//vscode.commands.executeCommand('ispw.load');
		//IspwCliCommand.runCommand('load', vscode.window.activeTextEditor?.document.uri);
	});

});

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
	})

	return rjk2;
}
