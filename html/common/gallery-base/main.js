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

let getStudentToken = function() {
  return window.localStorage.getItem("student-token");
}

let getToken = function() {
  return window.localStorage.getItem("mod-token") || getStudentToken();
}

window.onEnter = (f) => (e) => { if (e.keyCode === 13) { return f(e); } };

window.startSession = function(sessionName) {

  const templateName = window.vandyCustomization.name;

  fetch(`/api/public/galleries/${sessionName}/template-name`, { method: "GET" }).
    then(
      (response) => {
        response.text().then(
          (responseText) => {
            if (response.status !== 200) {
              alert(`Failed to lookup gallery '${sessionName}'.  Reason: ${responseText}`);
            } else if (responseText !== templateName) {
              alert(`This gallery (${sessionName}) was made for the '${responseText}' activity, but this is the '${templateName}' activity.  This gallery may not work correctly unless opened in the appropriate activity.`);
            }
          }
        )
      }
    );

  let startup = (sessionName) => {

    setSessionName(sessionName);

    let token = getToken()

    if (token !== null) {
      uploadInterval = setInterval(sync, syncRate);
    } else {
      fetch(`${window.thisDomain}/uploader-token`, { method: "GET" }).
        then((x) => x.text()).then(
          (t) => {
            window.localStorage.setItem("student-token", t);
            uploadInterval = setInterval(sync, syncRate);
          }
        );
    }

    fetch(`${window.thisDomain}/starter-config/${sessionName}`, { method: "GET" }).then(
      (res) => {
        if (res.status === 200) {
          res.text().then(
            (starterConfig) => {
              const parcel =
                { content: starterConfig
                , name:    sessionName
                , type:    "import-project"
                };
              parent.postMessage(parcel, "*");
            }
          );
        }
      }
    );

  };

  if (sessionName === undefined) {
    fetch(`${window.thisDomain}/new-session`, { method: "POST" }).
      then((x) => x.text()).then(startup);
  } else {
    startup(sessionName);
    sync()
  }

};

let sync = function() {

  let gallery  = document.getElementById("gallery");
  let showcase = document.getElementById("showcase");

  let callback = (entries) => {

    let containerPromises =
      entries.map((entry) => {

        let metadata = JSON.parse(entry.metadata);
        let img = document.createElement("img");
        img.classList.add("upload-image");
        img.title   = metadata === null ? "Anonymous project" : `Click to view project by ${metadata.uploader}`;
        img.src     = entry.base64Image;
        img.onclick = () => {
          let dataPromise     = fetch(`${window.thisDomain}/uploads/${ getSessionName()}/${entry.uploadName}`).then(x => x.text());
          let commentsPromise = fetch(`${window.thisDomain}/comments/${getSessionName()}/${entry.uploadName}`).then(x => x.json());
          let commentURL      = `${window.thisDomain}/comments`;
          Promise.all([dataPromise, commentsPromise]).then(
            ([data, comments]) => {
              const msg =
                { type:        "show-modal"
                , sessionName: getSessionName()
                , uploadName:  entry.uploadName
                , metadata
                , data
                , comments
                , image:       entry.base64Image
                , commentURL
                };
              parent.postMessage(msg, "*");
            }
          );

        };

        let label       = document.createElement("span");
        let boldStr     = (str) => `<span style="font-weight: bold;">${str}</span>`;
        label.innerHTML = entry.metadata === null ? "Anonymous project" : `Project by ${metadata.uploader}`;
        label.classList.add("upload-label")

        let container = document.createElement("div")
        container.appendChild(img);
        container.appendChild(label);
        container.classList.add("upload-container");
        container.dataset.uploadName = entry.uploadName;

        if (entry.isOwner || entry.canModerate) {
          let template = document.getElementById("deleter-template");
          let deleter  = document.importNode(template.content, true).querySelector(".deleter");
          deleter.onclick = () => {
            if (window.confirm("Are you sure you want to delete this submission?  No one will be able to see it if you click \"OK\".")) {
              container.remove();
              let token = getToken() || "";
              fetch(`${window.thisDomain}/uploads/${getSessionName()}/${entry.uploadName}/${token}`, { method: "DELETE" });
            }
          };
          container.appendChild(deleter);
        }

        if (metadata.featured) {
          img      .classList.add("featured");
          container.classList.add("featured");
        }

        return container;

      });

    Promise.all(containerPromises).then(
      (containers) => containers.forEach(
        (container) => {
          const g = (container.classList.contains("featured") ? showcase : gallery);
          g.appendChild(container);
        }
      )
    );

  };

  fetch(`${window.thisDomain}/listings/${getSessionName()}`).then(x => x.json()).then(
    (listings) => {

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

      const params =
        { "session-id": getSessionName()
        , "names":      JSON.stringify(newNames) };

      const options =
        { method:  "POST"
        , body:    window.makeQueryString(params)
        , headers: { "Content-Type": "application/x-www-form-urlencoded" }
        };

      let token  = getToken() || "";

      return fetch(`${window.thisDomain}/data-lite/${token}`, options);

    }
  ).then(x => x.json()).then(callback);

};

window.upload = function(e) {

  e.preventDefault();

  if ((new Date().getTime() - lastUploadTime) > syncRate) {

    new Promise(
      (resolve, reject) => {
        let reader = new FileReader();
        reader.onloadend = (event) => {
          resolve(event.target);
        };
        reader.readAsDataURL(document.getElementById("upload-image").files[0]);
      }
    ).then((imageEvent) => {
      if (imageEvent.result) {

        let formData = new FormData(document.getElementById("upload-form"));
        formData.set("image"     , imageEvent.result);
        formData.set("session-id", getSessionName());
        formData.set("token",      getStudentToken() || "");

        const options = { method: "POST", body: formData };

        return fetch(`${window.thisDomain}/file-uploads/`, options);

      } else {
        reject(`Image conversion failed somehow...?  Error: ${JSON.stringify(imageEvent.error)}`);
      }
    }).then((response) => {
      if (response.status === 200) {
        clearInterval(uploadInterval);
        sync();
        uploadInterval = setInterval(sync, syncRate);
      } else {
        response.text().then((body) => { alert(JSON.stringify(body)) });
      }
    });

    lastUploadTime = new Date().getTime();

    let elem = document.getElementById("upload-submit-button");
    elem.disabled = true;
    setTimeout(() => { elem.disabled = false; }, syncRate);

  }

  return false;

};
