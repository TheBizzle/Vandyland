const myFrame  = document.getElementById("embed_frame");
const myWindow = myFrame.contentWindow;

myFrame.src = "Embed.html?disableWorkInProgress";

let waitingData = null;

// ((String, String)) => Unit
const loadProject = ([str, name]) => {

  const { nlogo, world } = JSON.parse(str);

  const nlogoMessage = { nlogo, path: name };
  const worldMessage = { world, type: "nlw-import-world" };

  queryNLW("nlw-load-model", nlogoMessage).then(
    () => {
      if (world !== null) {
        myWindow.postMessage(worldMessage, "*");
      }
    }
  );

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
      const content = JSON.stringify({ nlogo: event.data.content, world: null });
      const starter = [content, event.data.name];
      if (myWindow.session !== null && myWindow.session !== undefined) {
        loadProject(starter);
      } else {
        waitingData = starter;
      }
      break;
    case "import-project":
      const parcel = [event.data.content, event.data.name];
      loadProject(parcel);
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
  const state = await queryNLW("nlw-export-world", {});
  const view  = await queryNLW("nlw-request-view", {});
  const str   = JSON.stringify({ nlogo: model.export.result, world: state.export });
  return [str, view.base64];
};

window.galleryCustomization =
  { frame:  myFrame
  , genPair
  , name:   "netlogo-world"
  , window: myWindow
  };
