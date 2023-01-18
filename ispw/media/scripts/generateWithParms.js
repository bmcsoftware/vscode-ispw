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
      var targetText = inputElements[i].getAttribute("target");
      updateDetails[counter] = targetText;
      var arr = targetText.split(".");
      var obj = {
        'category': arr[0],
        'name': arr[1],
        'value': inputElements[i].value
      };
      updateDetails[counter++] = obj;
    }
  }

  var radioElements = document.getElementsByTagName("vscode-radio-group");
  for (var j = 0; j < radioElements.length; j++) {
    var targetRadio = radioElements[j].getAttribute("target");
    var arr = targetRadio.split(".");
    var radioChildren = radioElements[j].getElementsByTagName("vscode-radio");
    for (var k = 0; k < radioChildren.length; k++) {
      if (radioChildren[k].checked) {
        var radioObj = {
          'category': arr[0],
          'name': arr[1],
          'value': radioChildren[k].value
        };
        updateDetails[counter++] = radioObj;
        break;
      }
    }
  }

  var checkboxElements = document.getElementsByTagName("vscode-checkbox");
  for( var c=0; c<checkboxElements.length; c++) {
    if(checkboxElements[c].checked) {
      var targetCheckbox = checkboxElements[c].getAttribute("target");
      var arr = targetCheckbox.split(".");
      var checkboxObj = {
        'category': arr[0],
        'name': arr[1],
        'value': checkboxElements[c].value
      };
      updateDetails[counter++] = checkboxObj;
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
