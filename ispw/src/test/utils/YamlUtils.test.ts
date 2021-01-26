import * as tmp from "tmp";
import * as vscode from "vscode";
const assert = require('chai').assert;
import {YamlUtils} from '../../utils/YamlUtils';

describe('YamlUtils.test', function() {
    it('getYamlLocationAbsPath should return a string', function(){
        let fileResult: tmp.FileResult = tmp.fileSync({name: "YamlUtils_getYamlLocationAbsPath_1.cob"});
        console.log(fileResult);
        let selectedFile: vscode.Uri = vscode.Uri.file(fileResult.name);

    })
})