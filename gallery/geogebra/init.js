let isGalleryEnabled = true;
let waitingData      = null;

const geogeFrame  = document.getElementById("geoge_frame");
const geogeWindow = geogeFrame.contentWindow;

const peskyLoop =
  setInterval(() => {
    if (geogeWindow.ggbApplet?.setBase64 !== undefined) {
      clearInterval(peskyLoop);
      if (waitingData !== null) {
        geogeWindow.ggbApplet.setBase64(waitingData);
      }
    }
  }, 50);

let receiveMessage = function(event) {
  switch (event.data.type) {
    case "import-ggb":
      const prefixed = event.data.content
      const base64   = prefixed.slice(prefixed.indexOf(",") + 1);
      if (geogeWindow.ggbApplet?.setBase64 !== undefined) {
        geogeWindow.ggbApplet.setBase64(base64);
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

document.addEventListener("DOMContentLoaded", () => {

  window.initializeLayout();

  let sessionID = decodeURIComponent(window.location.hash.slice(1));
  while (!sessionID) {
    sessionID = prompt("An activity space is required.\n\nPlease enter the name of a new or existing activity space:");
  }
  window.location.hash = `#${sessionID}`;

});

function getThumbnailFromXML(xml) {
  const startIndex = xml.indexOf("<thumbnail>") + 11;
  const endIndex   = xml.indexOf("</thumbnail>");
  return xml.substring(startIndex, endIndex);
}

function deactivatePublishButton() {
  isGalleryEnabled = false;
  document.getElementById("gallerybutton").classList.add("disabled");
}
function activatePublishButton() {
  isGalleryEnabled = true;
  document.getElementById("gallerybutton").classList.remove("disabled");
}

async function onPublishClicked() {
  if (isGalleryEnabled) {
    const info = await promptPublishInfo();
    if (info) {
      const {username, description} = info;
      deactivatePublishButton();
      try {
        await publishProject(username, description);
        M.toast({ html: "Published to Gallery!" })
      } catch (err) {
        M.toast({ html: err.message })
      }
      activatePublishButton();
    }
  }
}

async function promptPublishInfo() {

  const deferred      = defer();
  const element       = document.getElementById("publish-info");
  const usernameEl    = element.querySelector("input");
  const descriptionEl = element.querySelector("textarea");
  const publishBtn    = element.querySelector(".publish-btn");
  const cancelBtn     = element.querySelector(".cancel-btn");

  publishBtn.onclick =
    () => {
      const username    = usernameEl.value;
      const description = descriptionEl.value;
      deferred.resolve({ username, description });
    };

  cancelBtn.onclick = () => { deferred.resolve(); };

  descriptionEl.value = "";
  M.updateTextFields();
  M.Modal.getInstance(element).open();

  return deferred.promise;

}

function defer() {
  const deferred = {};
  deferred.promise =
    new Promise(
      (resolve, reject) => {
        deferred.resolve = resolve;
        deferred.reject  = reject;
      }
    );
  return deferred;
}

async function publishProject(username, description) {

  const applet    = document.getElementById("geoge_frame").contentWindow.ggbApplet;
  const xml       = await applet.getBase64();
  const base64    = await applet.getPNGBase64(0.5, true, undefined);
  const image     = `data:image/png;base64,${base64}`;
  const sessionID = decodeURIComponent(window.location.hash.slice(1));

  const metadata = JSON.stringify({ description, uploader: username });

  let data = new FormData();
  data.append(      "data",       xml);
  data.append(     "image",     image);
  data.append(  "metadata",  metadata);
  data.append("session-id", sessionID);
  data.append(     "token", window.localStorage.getItem("student-token") || "");

  console.log("publishing...", data);

  const response = await fetch("/file-uploads", { method: "POST", body: data });

  if (response.status < 400) {
    return true;
  } else {
    throw new Error(await response.text());
  }

}
