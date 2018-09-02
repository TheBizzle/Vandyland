let domain = "https://localhost:8001";

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

  let gallery  = document.getElementById('gallery');
  let showcase = document.getElementById('showcase');

  let callback = function(entries) {

    let containerPromises =
      entries.map(function(entry) {

        let metadata = JSON.parse(entry.metadata);

        let img = document.createElement("img");
        img.classList.add("upload-image");
        img.title = metadata === null ? entry.uploadName : entry.uploadName + " by " + metadata.uploader;
        img.src   = entry.base64Image;
        img.onclick = function() {
          let dataPromise     = fetch(domain + "/uploads/"  + getSessionName() + "/" + entry.uploadName).then(x => x.text());
          let commentsPromise = fetch(domain + "/comments/" + getSessionName() + "/" + entry.uploadName).then(x => x.json());
          let commentURL      = domain + "/comments"
          Promise.all([dataPromise, commentsPromise]).then(([data, comments]) => parent.postMessage({ type: "show-modal", sessionName: getSessionName(), uploadName: entry.uploadName, metadata: metadata, data: data, comments: comments, image: entry.base64Image, commentURL: commentURL }, "*"));
        };

        let container = document.createElement("div")
        container.appendChild(img);
        container.classList.add("upload-container");

        if (metadata.featured) {
          img      .classList.add("featured");
          container.classList.add("featured");
        }

        return container;

      });

    Promise.all(containerPromises).then((containers) => containers.forEach(function(container) {
      (container.classList.contains("featured") ? showcase : gallery).appendChild(container);
    }));

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
      response.text().then(function(body) { alert(JSON.stringify(body)) });
    };

    let callback = function(response) {
      if (response.status === 200) {
        clearInterval(uploadInterval);
        sync();
        uploadInterval = setInterval(sync, syncRate);
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
