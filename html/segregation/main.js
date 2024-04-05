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
    let token = window.localStorage.getItem("token")
    if (token !== null) {
      uploadInterval = setInterval(sync, syncRate);
    } else {
      fetch(window.thisDomain + "/uploader-token", { method: "GET" }).then((x) => x.text()).then(
        function (t) {
          window.localStorage.setItem("token", t);
          uploadInterval = setInterval(sync, syncRate);
        }
      );
    }
  };

  if (sessionName === undefined) {
    fetch(window.thisDomain + "/new-session/segregation", { method: "POST" }).then((x) => x.text()).then(startup);
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
          let dataPromise     = fetch(window.thisDomain + "/uploads/"  + getSessionName() + "/" + entry.uploadName).then(x => x.text());
          let commentsPromise = fetch(window.thisDomain + "/comments/" + getSessionName() + "/" + entry.uploadName).then(x => x.json());
          let commentURL      = window.thisDomain + "/comments"
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
        container.dataset.uploadName = entry.uploadName;

        if (entry.isOwner) {
          let template = document.getElementById("deleter-template");
          let deleter  = document.importNode(template.content, true).querySelector(".deleter");
          deleter.onclick = function() {
            if (window.confirm("Are you sure you want to delete this submission?  No one will be able to see it if you click \"OK\".")) {
              container.remove();
              let token  = window.localStorage.getItem("token") || "";
              fetch(window.thisDomain + "/uploads/"  + getSessionName() + "/" + entry.uploadName + "/" + token, { method: "DELETE" });
            }
          };
          container.appendChild(deleter);
        }

        return container;

      });

    Promise.all(containerPromises).then((containers) => containers.forEach((container) => gallery.appendChild(container)));

  };

  fetch(window.thisDomain + "/listings/" + getSessionName()).then(x => x.json()).then(
    function(listings) {

      let newNames = listings.filter((l) => !knownNames.has(l.subName) && !l.isSuppressed).map((l) => l.subName);
      newNames.forEach((name) => knownNames.add(name));

      let supNames = listings.filter((l) => l.isSuppressed).map((l) => l.subName);
      supNames.forEach((name) => {
        knownNames.delete(name)
        let elem = document.querySelector(`.upload-container[data-uploadName="${name}"]`);
        if (elem !== null) {
          elem.remove();
        }
      });

      let token  = window.localStorage.getItem("token") || "";
      let params = makeQueryString({ "session-id": getSessionName(), "names": JSON.stringify(newNames), token });

      return fetch(window.thisDomain + "/data-lite/", { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } });

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

    let token    = window.localStorage.getItem("token") || "";
    let metadata = JSON.stringify({ summary, uploader });
    let params   = makeQueryString({ "session-id": getSessionName(), image, "data": code, token, metadata });
    fetch(window.thisDomain + "/uploads/", { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } }).then(callback, failback);

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
