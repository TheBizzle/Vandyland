// (Event) => Unit
window.submitToView = function(e) {
  e.preventDefault();
  let viewForm  = document.getElementById("view-form");
  let selection = viewForm.querySelector(".gallery-view-item.selected");
  switch (e.submitter.id) {
    case "submit-student":
      window.open(`/html/basic/#${selection.dataset.sessionName}`, "_blank");
      break;
    case "submit-moderator":
      window.open(`/html/basic/moderation/#${selection.dataset.sessionName}`, "_blank");
      break;
    default:
      console.error(`Unknown submitter: ${e.submitter.name}`);
  }
};

// (Event) => Unit
window.submitToInit = function(e) {

  e.preventDefault();

  let initForm  = document.getElementById("init-form");
  let formData  = new FormData(initForm);
  let sessionID = formData.get("name");
  let getsPreed = formData.get("galleryMode") === "prescreen";
  let token     = window.localStorage.getItem("mod-token");

  fetch(`/new-session/${sessionID}/${getsPreed}/${token}`, { method: "POST" }).then(
    (response) => {
      if (!response.ok)
        response.text().then((reason) => alert(`Could not create new session.  ${reason}`));
      else {
        document.getElementById("sesh-name-input").value = "";
        document.getElementById("submit-init").disabled  = true;
      }
    }
  )

};

let render = function(values) {

  let galleries = document.getElementById("view-list");
  let selection = galleries.querySelector(".selected");

  galleries.innerHTML = "";

  values.forEach(({ galleryName, isPrescreened, numWaiting, uploadNames }) => {

    let template = document.getElementById("gallery-view-template");
    let gView    = document.importNode(template.content, true).querySelector(".gallery-view-item");

    gView.querySelector(".gallery-view-title").innerText = galleryName;
    gView.title                                          = galleryName;

    gView.dataset.isPrescreened = isPrescreened;
    gView.dataset.numUploads    = uploadNames.length;
    gView.dataset.numWaiting    = numWaiting;
    gView.dataset.sessionName   = galleryName;

    if (selection !== null && selection.dataset.sessionName === galleryName) {
      gView.classList.add("selected")
    }

    gView.onclick = () => {

      Array.from(galleries.querySelectorAll(".selected")).forEach(
        (el) => {
          el.classList.remove("selected");
        }
      );
      gView.classList.add("selected");

      document.getElementById("control-name"       ).innerText = galleryName;
      document.getElementById("control-name"       ).title     = galleryName;
      document.getElementById("control-uploads"    ).innerText = uploadNames.length;
      document.getElementById("control-date"       ).innerText = "TBD"
      document.getElementById("control-unmoderated").innerText = isPrescreened ? numWaiting : "N/A";

      document.getElementById("submit-student"  ).disabled  = false;
      document.getElementById("submit-moderator").disabled  = !isPrescreened;

    };

    galleries.appendChild(gView);

  });

};

window.sync = function() {

  let token = window.localStorage.getItem("mod-token");

  fetch(`${window.thisDomain}/gallery-listings/${token}`, { method: "GET" }).then((x) => x.json()).then(
    function (listings) {

      let promises =
        listings.map(({ galleryName, isPrescreened, numWaiting }) => {
          return fetch(`${window.thisDomain}/listings/${galleryName}`, { method: "GET" }).
            then((x) => x.json()).
            then((x) => { return { galleryName, isPrescreened, numWaiting, uploadNames: x } })
        });

      Promise.all(promises).then(render);

    }
  );

};

window.toggleCollapsible = function(elem) {

  let collapsibleContent = elem.nextElementSibling;

  elem.classList.toggle('active');
  collapsibleContent.classList.toggle('active');

  collapsibleContent.style.maxHeight =
    (collapsibleContent.style.maxHeight !== "0px") ? "0px" : `${collapsibleContent.scrollHeight}px`;

};
