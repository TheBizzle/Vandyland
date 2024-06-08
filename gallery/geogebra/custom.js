const myFrame  = document.getElementById("embed_frame");
const myWindow = myFrame.contentWindow;

let waitingData = null;

const peskyLoop =
  setInterval(() => {
    if (myWindow.ggbApplet?.setBase64 !== undefined) {
      clearInterval(peskyLoop);
      if (waitingData !== null) {
        myWindow.ggbApplet.setBase64(waitingData);
      }
    }
  }, 50);

let receiveMessage = function(event) {
  switch (event.data.type) {
    case "import-starter":
    case "import-project":
      const prefixed = event.data.content
      const base64   = prefixed.slice(prefixed.indexOf(",") + 1);
      if (myWindow.ggbApplet?.setBase64 !== undefined) {
        myWindow.ggbApplet.setBase64(base64);
      } else {
        waitingData = base64;
      }
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

async function genPair() {
  const applet    = myWindow.ggbApplet;
  const xml       = await applet.getBase64();
  const base64    = await applet.getPNGBase64(0.5, true, undefined);
  const image     = `data:image/png;base64,${base64}`;
  return [xml, image];
};

window.galleryCustomization =
  { frame:  myFrame
  , genPair
  , name:   "geogebra"
  , window: myWindow
  };
