const vscode = acquireVsCodeApi();
window.addEventListener("load", main);

/**
 * Triggered once the DOM is loaded.
 * It sets event listeners to Generate and Cancel buttons.
 */
function main() {
  const generateButton = document.getElementById("generate-with-parms");
  generateButton.addEventListener("click", generate);

  const cancelButton = document.getElementById("cancel-generate-with-parm");
  cancelButton.addEventListener("click", cancel);
}

/**
 * Triggered on click of 'Generate' button
 */
function generate() {
  var inputElements = document.getElementsByTagName("vscode-text-field");
  var updateDetails = [];
  var counter = 0;
  for (var i = 0; i < inputElements.length; i++) {
    if (inputElements[i].value !== undefined | inputElements[i].value !== null | inputElements[i].value !== "") {
      var obj = {
        'category': "WZGPARMS",
        'name': inputElements[i].id,
        'value': inputElements[i].value
      };
      updateDetails[counter] = obj;
      counter++;
    }
  }

  var radioElements = document.getElementsByTagName("vscode-radio-group");
  for (var j = 0; j < radioElements.length; j++) {
    var radioChildren = radioElements[j].getElementsByTagName("vscode-radio");
    for (var k = 0; k < radioChildren.length; k++) {
      if (radioChildren[k].checked) {
        var radioObj = {
          'category': "WZGPARMS",
          'name': radioElements[j].id,
          'value': radioChildren[k].value
        };
        updateDetails[counter] = radioObj;
        counter++;
        break;
      }
    }
  }

  var checkboxElements = document.getElementsByTagName("vscode-checkbox");
  for( var c=0; c<checkboxElements.length; c++) {
    if(checkboxElements.checked) {
      var checkboxObj = {
        'category': "WZGPARMS",
        'name': checkboxElements[c].id,
        'value': checkboxElements[c].value
      };
      updateDetails[counter] = checkboxObj;
      counter++;
    }
  }

  vscode.postMessage({
              command: "generate",
              text: "clicked generate button",
              data: updateDetails
  });
}

function validateInput() {

}

function cancel() {
  vscode.postMessage({
    command: "cancel",
    text: "clicked cancel button"
  });
}
