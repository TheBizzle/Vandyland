window.externalVariables = {}

var receiveMessage = function(event) {
  switch (event.data.type) {
    case "set-variable":
      externalVariables[event.data.key] = event.data.value;
      break;
  }
}

window.addEventListener("message", receiveMessage, false);

window.addEventListener("ideLoaded", (e) => parent.postMessage({ type: "ide-loaded" }, "*"), false);
