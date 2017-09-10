let domain = "http://localhost:8000";

let uploadInterval = undefined;
let lastUploadTime = 0;

let setSessionName = function(name) { document.getElementById('session-name').innerText = name; };
let getSessionName = function()     { return document.getElementById('session-name').innerText; };

window.onEnter = function(f) { return function(e) { if (e.keyCode === 13) { return f(e); } }; };

window.startSession = function(e) {

  document.getElementById("landing-area").classList.add   ("hidden");
  document.getElementById("main-content").classList.remove("hidden");

  let startup = function(sessionName) {
    setSessionName(sessionName);
    uploadInterval = setInterval(sync, 10000);
  };

  if (e === undefined) {
    fetch(domain + "/new-session", { method: "POST" }).then((x) => x.text()).then(startup);
  } else {
    startup(e.target.value);
    sync()
  }

};

let sync = function() {

  let gallery = document.getElementById('gallery');
  gallery.innerHTML = "";

  let callback = function(entries) {

    let containerPromises =
      entries.map(function(entry) {

        let metadataPromise = fetch(domain + "/uploads/" + getSessionName() + "/" + entry.uploadName + "/metadata").then(x => x.ok ? x.text() : undefined);
        return metadataPromise.then(function(metadata) {

          let img = document.createElement("img");
          img.classList.add("upload-image");
          img.src = entry.base64Image;
          img.onclick = function() {
            let dataPromise     = fetch(domain + "/uploads/" + getSessionName() + "/" + entry.uploadName              ).then(x => x.text());
            let commentsPromise = fetch(domain + "/uploads/" + getSessionName() + "/" + entry.uploadName + "/comments").then(x => x.json());
            let commentURL      = domain + "/comments"
            Promise.all([dataPromise, commentsPromise]).then(([data, comments]) => showModal(getSessionName(), entry.uploadName, metadata, data, comments, entry.base64Image, commentURL));
          };

          let label   = document.createElement("span");
          let boldStr = function(str) { return '<span style="font-weight: bold;">' + str + '</span>' };
          label.innerHTML = metadata === undefined ? boldStr(entry.uploadName) : boldStr(entry.uploadName) + " by " + boldStr(metadata);

          let container = document.createElement("div")
          container.appendChild(img);
          container.appendChild(label);
          container.classList.add("upload-container");

          return container;

        });

      });

    Promise.all(containerPromises).then((containers) => containers.forEach((container) => gallery.appendChild(container)));

  };

  fetch(domain + "/uploads/" + getSessionName()).then((x) => x.json()).then(callback);

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
