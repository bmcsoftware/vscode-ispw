"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const assert = __importStar(require("assert"));
const IspwCliCommand = __importStar(require("../../commands/CliCommand"));
const CredentialsCache_1 = require("../../types/CredentialsCache");
// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
const vscode = __importStar(require("vscode"));
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
        let cc = CredentialsCache_1.CredentialsCache.getInstance();
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
        };
        let ispwTypeCobExpect = {
            ispwType: 'COB', fileExtension: 'cbl', genSeq: 'V', progType: 'Yes',
            sql: 'No', cics: 'No', ims: 'No', flag1: 'N', flag2: 'I', flag3: 'C', flag4: 'E', genParms: 'ISPWCUTE'
        };
        let ispwTypeClst = {
            ispwType: 'CLST', fileExtension: 'clst'
        };
        let ispwTypeClstExpect = {
            ispwType: 'CLST', fileExtension: 'clst'
        };
        let ispwPathCob = {
            path: '\\COB', types: { ispwTypeCob }
        };
        let ispwPathClst = {
            path: '\\CLST', types: { ispwTypeClst }
        };
        let ispwApplication = {
            stream: 'PLAY-S',
            application: 'PLAY-A',
            host: 'CW09',
            port: 47623,
            runtimeConfig: 'TPZP',
            pathMappings: { ispwPathCob, ispwPathClst }
        };
        let ispwRoot = {
            ispwApplication: ispwApplication
        };
        assert.strictEqual(ispwRoot.ispwApplication.application, 'PLAY-A');
        assert.strictEqual(ispwRoot.ispwApplication.stream, 'PLAY-S');
        assert.strictEqual(ispwRoot.ispwApplication.port, 47623);
        assert.strictEqual(ispwRoot.ispwApplication.runtimeConfig, 'TPZP');
        assert.strictEqual(ispwRoot.ispwApplication.pathMappings.ispwPathCob.path, '\\COB');
        assert.strictEqual(ispwRoot.ispwApplication.pathMappings.ispwPathClst.path, '\\CLST');
        let ispwTypeCob1 = ispwRoot.ispwApplication.pathMappings.ispwPathCob.types.ispwTypeCob;
        assert.strictEqual(ispwTypeCob === ispwTypeCob1, true);
        assert.strictEqual(ispwTypeCob1 == ispwTypeCobExpect, false);
        compareObject(ispwTypeCob1, ispwTypeCobExpect);
        let ispwTypeClst1 = ispwRoot.ispwApplication.pathMappings.ispwPathClst.types.ispwTypeClst;
        assert.strictEqual(ispwTypeClst1 === ispwTypeClst, true);
        assert.strictEqual(ispwTypeClst1 == ispwTypeClstExpect, false);
        compareObject(ispwTypeClst1, ispwTypeClstExpect);
    });
    test('Sample test', () => {
        assert.strictEqual(-1, [1, 2, 3].indexOf(5));
        assert.strictEqual(-1, [1, 2, 3].indexOf(0));
    });
    test('Sample test', () => {
        assert.strictEqual(-1, [1, 2, 3].indexOf(5));
        assert.strictEqual(-1, [1, 2, 3].indexOf(0));
    });
    test('test', () => __awaiter(this, void 0, void 0, function* () {
        /*
        let cliLocation: string | undefined = CliUtils.getCliLocation();
        let loadLevel: string | undefined = CliUtils.getLoadLevel();
        let assignmentDesc: string | undefined = CliUtils.getAssignmentDescription();

        const ispwlevel = vscode.workspace.getConfiguration("ispw").get("ispw.Level");
        console.log(ispwlevel);
        */
        yield vscode.commands.executeCommand('ispw.clearCreds');
    }));
    test('test', () => __awaiter(this, void 0, void 0, function* () {
        var _a;
        //await subscribeToTsConfigChanges();
        const document = yield vscode.workspace.openTextDocument('rjk2/COB/TPROG03.cbl');
        yield vscode.window.showTextDocument(document);
        //vscode.commands.executeCommand('ispw.load');
        IspwCliCommand.runCommand('load', (_a = vscode.window.activeTextEditor) === null || _a === void 0 ? void 0 : _a.document.uri);
    }));
    //vscode.window.showInformationMessage('End all tests.');
});
function compareObject(object1, object2) {
    let props = Object.getOwnPropertyNames(object1);
    props.forEach(prop => {
        //console.log(`object1.${prop}=`, Reflect.get(object1, prop), `, object2.${prop}=`, Reflect.get(object2, prop));
        assert.strictEqual(Reflect.get(object1, prop), Reflect.get(object2, prop));
    });
}
function subscribeToTsConfigChanges() {
    const disposables = [];
    for (const workfolder of vscode.workspace.workspaceFolders || []) {
        const pattern = new vscode.RelativePattern(workfolder, "[tj]sconfig.json");
        const fileWatcher = vscode.workspace.createFileSystemWatcher(pattern);
        fileWatcher.onDidChange(() => invalidateCache(workfolder));
        disposables.push(fileWatcher);
    }
    return disposables;
}
function invalidateCache(workfolder) {
}
//# sourceMappingURL=extension.test.js.map