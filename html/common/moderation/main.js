let syncInterval      = undefined;
let syncRate          = 5000;
let knownNames        = new Set();
let knownWaitingNames = new Set();

let approvers = {};
let deleters  = {};

let getSessionName = function() {
  return decodeURIComponent(window.location.hash.substring(1));
};

let getStudentToken = function() {
  return window.localStorage.getItem("student-token");
}

let getToken = function() {
  return window.localStorage.getItem("mod-token") || getStudentToken();
}

window.approve = function(uploadName) {
  approvers[uploadName]();
};

window.reject = function(uploadName) {
  deleters[uploadName]();
};

window.startSession = function(sessionName) {
  syncInterval = setInterval(sync, syncRate);
  sync()
};

window.toggleCollapsible = function(elem) {

  elem.classList.toggle('active');

  let collapsibleContent = elem.nextElementSibling;

  collapsibleContent.style.maxHeight =
    (collapsibleContent.style.maxHeight !== 0) ? 0 : `${collapsibleContent.scrollHeight}px`;

};

let syncUnapproved = function() {

  let gallery = document.getElementById('unprocessed-gallery');
  let token   = getToken() || "0cd60303-e574-4757-a0d2-34ee83bbfa1f";

  let callback = function(entries) {

    let containerPromises =
      entries.map(function(entry) {

        let metadata = JSON.parse(entry);

        let img = document.createElement("img");
        img.classList.add("upload-image");
        img.src = entry.base64Image;
        img.onclick = function() {
          fetch(window.thisDomain + "/uploads/"  + getSessionName() + "/" + entry.uploadName).then(x => x.text()).then(
            (data) =>
              showModal(getSessionName(), entry.uploadName, metadata, data, entry.base64Image, false)
          )
        };

        let label       = document.createElement("span");
        let boldStr     = function(str) { return '<span style="font-weight: bold;">' + str + '</span>' };
        label.innerHTML = metadata === null ? boldStr(entry.uploadName) : boldStr(entry.uploadName) + " by " + boldStr(metadata.uploader || "???");
        label.classList.add("upload-label")

        let container = document.createElement("div")
        container.appendChild(img);
        container.appendChild(label);
        container.classList.add("upload-container");
        container.dataset.uploadName = entry.uploadName;

        approvers[entry.uploadName] =
          function() {
            container.remove();
            window.hideModal();
            fetch(`${window.thisDomain}/uploads/${getSessionName()}/${entry.uploadName}/${token}/approve`, { method: "POST" });
          };

        deleters[entry.uploadName] =
          function() {
            if (window.confirm("Are you sure you want to discard this submission?  No one will be able to see it if you click \"OK\".")) {
              container.remove();
              window.hideModal();
              fetch(`${window.thisDomain}/uploads/${getSessionName()}/${entry.uploadName}/${token}/reject`, { method: "POST" });
            }
          };

        let template = document.getElementById("deleter-template");
        let deleter  = document.importNode(template.content, true).querySelector(".deleter");
        deleter.onclick = deleters[entry.uploadName];
        container.appendChild(deleter);

        let template2 = document.getElementById("approver-template");
        let approver  = document.importNode(template2.content, true).querySelector(".approver");
        approver.onclick = approvers[entry.uploadName];
        container.appendChild(approver);

        return container;

      });

    Promise.all(containerPromises).then(
      (containers) =>
        containers.forEach((container) => gallery.appendChild(container))
    ).then(
      () =>
        document.getElementById('queue-span').innerText = gallery.childNodes.length
    );

  };

  fetch(`${window.thisDomain}/mod-listings/${getSessionName()}/${token}`).then(
    function(response) {
      if (response.ok) {
        return response.json();
      } else {
        throw new Error(`${response.status} - ${response.statusText}`);
      }
    }
  ).then(
    function(listings) {

      let newNames = listings.filter((l) => !knownWaitingNames.has(l));
      newNames.forEach((name) => knownWaitingNames.add(name));

      let params = makeQueryString({ "session-id": getSessionName(), "names": JSON.stringify(newNames) });

      return fetch(`${window.thisDomain}/data-lite/${token}`, { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } });

    }
  ).then(
    function(response) {
      if (response.ok) {
        return response.json();
      } else {
        throw new Error(`${response.status} - ${response.statusText}`);
      }
    }
  ).then(callback).catch(
    function(e) {
      alert("You are not authorized to moderate this session.");
      window.location.href = '/html/basic';
    }
  );

};

let syncApproved = function() {

  let gallery = document.getElementById('approved-gallery');
  let token   = getToken() || "";

  let callback = function(entries) {

    let containerPromises =
      entries.map(function(entry) {

        let metadata = null;

        try {
          metadata = JSON.parse(entry.metadata);
        } catch {
          metadata = entry.metadata;
        }

        let img = document.createElement("img");
        img.classList.add("upload-image");
        img.src = entry.base64Image;
        img.onclick = function() {
          fetch(window.thisDomain + "/uploads/"  + getSessionName() + "/" + entry.uploadName).then(x => x.text()).then(
            (data) =>
              showModal(getSessionName(), entry.uploadName, metadata, data, entry.base64Image, true)
          )
        };

        let label       = document.createElement("span");
        let boldStr     = function(str) { return '<span style="font-weight: bold;">' + str + '</span>' };
        label.innerHTML = metadata === null ? boldStr(entry.uploadName) : boldStr(entry.uploadName) + " by " + boldStr(metadata.uploader || "???");
        label.classList.add("upload-label")

        let container = document.createElement("div")
        container.appendChild(img);
        container.appendChild(label);
        container.classList.add("upload-container");
        container.dataset.uploadName = entry.uploadName;

        if (entry.canModerate) {

          let template = document.getElementById("deleter-template");
          let deleter  = document.importNode(template.content, true).querySelector(".deleter");
          deleter.onclick = function() {
            if (window.confirm("Are you sure you want to delete this submission?  No one will be able to see it if you click \"OK\".")) {
              container.remove();
              fetch(`${window.thisDomain}/uploads/${getSessionName()}/${entry.uploadName}/${token}`, { method: "DELETE" });
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

      let params = makeQueryString({ "session-id": getSessionName(), "names": JSON.stringify(newNames) });

      return fetch(`${window.thisDomain}/data-lite/${token}`, { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } });

    }
  ).then(x => x.json()).then(callback);

};

let sync = function() {
  syncUnapproved();
  syncApproved();
};
