let domain = "http://localhost:8000";

let uploadInterval = undefined;
let lastUploadTime = 0;
let knownNames     = new Set();

let setSessionName = function(name) {
  window.location.hash = name;
  document.getElementById('session-name').innerText = name;
};

let getSessionName = function() {
  return document.getElementById('session-name').innerText;
};

window.onEnter = function(f) { return function(e) { if (e.keyCode === 13) { return f(e); } }; };

window.startSession = function(sessionName) {

  document.getElementById("landing-area").classList.add   ("hidden");
  document.getElementById("main-content").classList.remove("hidden");

  let startup = function(sessionName) {
    setSessionName(sessionName);
    uploadInterval = setInterval(sync, 10000);
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

        let img = document.createElement("img");
        img.classList.add("upload-image");
        img.src = entry.base64Image;
        img.onclick = function() {
          let dataPromise     = fetch(domain + "/uploads/"  + getSessionName() + "/" + entry.uploadName).then(x => x.text());
          let commentsPromise = fetch(domain + "/comments/" + getSessionName() + "/" + entry.uploadName).then(x => x.json());
          let commentURL      = domain + "/comments"
          Promise.all([dataPromise, commentsPromise]).then(([data, comments]) => showModal(getSessionName(), entry.uploadName, entry.metadata, data, comments, entry.base64Image, commentURL));
        };

        let label   = document.createElement("span");
        let boldStr = function(str) { return '<span style="font-weight: bold;">' + str + '</span>' };
        label.innerHTML = entry.metadata === null ? boldStr(entry.uploadName) : boldStr(entry.uploadName) + " by " + boldStr(entry.metadata);

        let container = document.createElement("div")
        container.appendChild(img);
        container.appendChild(label);
        container.classList.add("upload-container");

        knownNames.add(entry.uploadName);

        return container;

      });

    Promise.all(containerPromises).then((containers) => containers.forEach((container) => gallery.appendChild(container)));

  };

  fetch(domain + "/names/" + getSessionName()).then(x => x.json()).then(
    function(names) {
      let newNames = names.filter((name) => !knownNames.has(name));
      let formData = new FormData();
      formData.append("session-id", getSessionName());
      formData.append("names"     , JSON.stringify(newNames));
      let params = Array.from(formData.entries()).map(([k, v]) => encodeURIComponent(k) + "=" + encodeURIComponent(v)).join("&");
      return fetch(domain + "/data-lite/", { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } });
    }
  ).then(x => x.json()).then(callback);

};

window.upload = function(e) {

  e.preventDefault();

  if ((new Date().getTime() - lastUploadTime) > 1000) {

    let callback = function(responseBody) {
      clearInterval(uploadInterval);
      sync();
      uploadInterval = setInterval(sync, 10000);
    };

    let failback = function() { alert(JSON.stringify(arguments)); };

    new Promise(
      function(resolve, reject) {
        let reader = new FileReader();
        reader.onloadend = function(event) {
          resolve(event.target);
        };
        reader.readAsDataURL(document.getElementById('upload-image').files[0]);
      }
    ).then(function(imageEvent) {
      if (imageEvent.result) {
        let formData = new FormData(document.getElementById("upload-form"));
        formData.set("image", imageEvent.result);
        formData.append("session-id", getSessionName());
        let params = Array.from(formData.entries()).map(([k, v]) => encodeURIComponent(k) + "=" + encodeURIComponent(v)).join("&");
        fetch(domain + "/uploads/", { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } }).then(callback, failback);
      } else {
        reject("Image conversion failed somehow...?  Error: " + JSON.stringify(imageEvent.error));
      }
    }).then(callback, failback);

    lastUploadTime = new Date().getTime();

  }

  return false;

};
