let domain = "http://rendupo.com:8000";

let uploadInterval = undefined;
let lastUploadTime = 0;
let syncRate       = 5000;
let knownNames     = new Set();

let _sessionName = undefined;

let setSessionName = function(name) {
  _sessionName = name;
};

let getSessionName = function() {
  return _sessionName;
};

window.onEnter = function(f) { return function(e) { if (e.keyCode === 13) { return f(e); } }; };

window.startSession = function(sessionName) {

  let startup = function(sessionName) {
    setSessionName(sessionName);
    uploadInterval = setInterval(sync, syncRate);
  };

  if (sessionName === undefined) {
    fetch(domain + "/new-session", { method: "POST" }).then((x) => x.text()).then(startup);
  } else {
    startup(sessionName);
    sync()
  }

};

let sync = function() {

  let gallery = document.getElementById('gallery');

  let callback = function(entries) {

    let containerPromises =
      entries.map(function(entry) {

        let metadata = JSON.parse(entry.metadata);

        let img = document.createElement("img");
        img.classList.add("upload-image");
        img.src = entry.base64Image;
        img.onclick = function() {
          let dataPromise     = fetch(domain + "/uploads/"  + getSessionName() + "/" + entry.uploadName).then(x => x.text());
          let commentsPromise = fetch(domain + "/comments/" + getSessionName() + "/" + entry.uploadName).then(x => x.json());
          let commentURL      = domain + "/comments"
          Promise.all([dataPromise, commentsPromise]).then(([data, comments]) => showModal(getSessionName(), entry.uploadName, metadata, data, comments, entry.base64Image, commentURL));
        };

        let label       = document.createElement("span");
        let boldStr     = function(str) { return '<span style="font-weight: bold;">' + str + '</span>' };
        label.innerHTML = metadata === null ? boldStr(entry.uploadName) : boldStr(entry.uploadName) + " by " + boldStr(metadata.uploader);
        label.classList.add("upload-label")

        let container = document.createElement("div")
        container.appendChild(img);
        container.appendChild(label);
        container.classList.add("upload-container");

        return container;

      });

    Promise.all(containerPromises).then((containers) => containers.forEach((container) => gallery.appendChild(container)));

  };

  fetch(domain + "/names/" + getSessionName()).then(x => x.json()).then(
    function(names) {
      let newNames = names.filter((name) => !knownNames.has(name));
      newNames.forEach((name) => knownNames.add(name));
      let params = makeQueryString({ "session-id": getSessionName(), "names": JSON.stringify(newNames) });
      return fetch(domain + "/data-lite/", { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } });
    }
  ).then(x => x.json()).then(callback);

};

window.upload = function({ code, image, summary, uploader }) {

  if ((new Date().getTime() - lastUploadTime) > 1000) {

    let failback = function(response) {
      parent.postMessage({ success: false, type: "code-upload-response" }, "*");
      response.text().then(function(body) { alert(JSON.stringify(body)) });
    };

    let callback = function(response) {
      if (response.status === 200) {
        clearInterval(uploadInterval);
        sync();
        uploadInterval = setInterval(sync, syncRate);
        response.text().then(function(body) { parent.postMessage({ success: true, uploadName: body, type: "code-upload-response" }, "*") });
      } else {
        failback(response);
      }
    };

    let params = makeQueryString({ "session-id": getSessionName(), image, "data": code, "metadata": JSON.stringify({ summary, uploader }) });
    fetch(domain + "/uploads/", { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } }).then(callback, failback);

    lastUploadTime = new Date().getTime();

  }

  return false;

};

let receiveMessage = function(event) {
  switch (event.data.type) {
    case "export-code":
      upload(event.data);
      break;
    default:
      console.log("Vandyland: Ignoring message of type '" + event.type + "'");
  }
}

window.addEventListener("message", receiveMessage, false);
