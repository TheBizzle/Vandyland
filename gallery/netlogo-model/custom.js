const myFrame  = document.getElementById("embed_frame");
const myWindow = myFrame.contentWindow;

myFrame.src = "Embed.html?disableWorkInProgress";

let waitingData = null;

// ((String, String)) => Unit
const loadProject = ([nlogo, name]) => {
  const message =
    { nlogo
    , path:  name
    , type:  "nlw-load-model"
    };
  myWindow.postMessage(message, "*");
};

const peskyLoop =
  setInterval(() => {
    if (myWindow.session !== null && myWindow.session !== undefined) {
      clearInterval(peskyLoop);
      if (waitingData !== null) {
        loadProject(waitingData);
      }
    }
  }, 50);

let receiveMessage = function(event) {
  switch (event.data.type) {
    case "import-starter":
    case "import-project":
      const parcel = [event.data.content, event.data.name];
      if (myWindow.session !== null && myWindow.session !== undefined) {
        loadProject(parcel);
      } else {
        waitingData = parcel;
      }
      break;
    case "nlw-resize":
      break;
    case "show-modal":
      showModal(event.data);
      break;
    default:
      console.log(`Lord Octopus: Ignoring message of type "${event.data.type}"`);
      console.log(event);
  }
}

window.addEventListener("message", receiveMessage, false);

// (String, Object[Any]) => Promise[Any]
const queryNLW = (type, msg = {}) => {

  const f =
    (resolve) => {

      const channel = new MessageChannel();

      channel.port1.onmessage = ({ data }) => {
        channel.port1.close();
        resolve(data);
      };

      myWindow.postMessage({ ...msg, type }, "*", [channel.port2]);

    };

  return new Promise(f);

};

async function genPair() {
  const model = await queryNLW("nlw-export-model", {});
  const view  = await queryNLW("nlw-request-view", {});
  return [model.export.result, view.base64];
};

window.galleryCustomization =
  { frame:  myFrame
  , genPair
  , name:   "netlogo-model"
  , window: myWindow
  };
