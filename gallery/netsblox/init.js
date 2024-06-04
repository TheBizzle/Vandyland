let isGalleryEnabled = true;
let netsHasLoaded    = false;
let peskyLoop        = null;
let waitingData      = null;

const nbAPI = new EmbeddedNetsBloxAPI(document.getElementById("nets_frame"));

const netsFrame = document.getElementById("nets_frame");

let params         = new URLSearchParams(window.location.search);
let starterProject = (params.get("starterProject") === null) ? "https://editor.netsblox.org/?" : params.get("starterProject");

let un   = (params.get(   "Username") === null) ? "" : params.get(   "Username");
let pn   = (params.get("ProjectName") === null) ? "" : params.get("ProjectName");
let hash = decodeURIComponent(window.location.hash.slice(1));

netsFrame.addEventListener("load", function() {

  const onResponse = (e) => {
    if (e.data.type === "reply") {

      clearInterval(peskyLoop);
      window.removeEventListener("message", onResponse);

      let hash = decodeURIComponent(window.location.hash.slice(1));
      netsFrame.contentWindow.postMessage({ key: "domain"      , value: window.thisDomain, type: "set-variable" }, "*");
      netsFrame.contentWindow.postMessage({ key: "locationHash", value: hash             , type: "set-variable" }, "*");

      if (waitingData !== null) {
        netsFrame.contentWindow.postMessage(waitingData, "*");
        waitingData = null;
      }

      netsHasLoaded = true;

    }
  };

  window.addEventListener("message", onResponse);

  peskyLoop =
    setInterval(() => {
      netsFrame.contentWindow.postMessage({ type: "get-username" }, "*");
    }, 50);

});

netsFrame.src = `${starterProject}&Username=${un}&ProjectName=${pn}&editMode=true&hash=${hash}&gallery=${window.thisDomain}`;

let receiveMessage = function(event) {
  switch (event.data.type) {
    case "import-project":
      event.data.type = "import";
      if (netsHasLoaded) {
        netsFrame.contentWindow.postMessage(event.data, "*");
      } else {
        waitingData = event.data;
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

  const netsbloxUsername = await nbAPI.getUsername();
  if (netsbloxUsername) {
    usernameEl.value = netsbloxUsername;
  }

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

  const xml       = await nbAPI.getProjectXML();
  const image     = getThumbnailFromXML(xml);
  const sessionID = decodeURIComponent(window.location.hash.slice(1));

  const metadata = JSON.stringify({ description, uploader: username });

  let data = new FormData();
  data.append(      "data",       xml);
  data.append(     "image",     image);
  data.append(  "metadata",  metadata);
  data.append("session-id", sessionID);

  console.log("publishing...", data);

  const response = await fetch("/file-uploads", { method: "POST", body: data });

  if (response.status < 400) {
    return true;
  } else {
    throw new Error(await response.text());
  }

}
