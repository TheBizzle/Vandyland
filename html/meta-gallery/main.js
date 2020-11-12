// (Event) => Unit
window.submitToView = function(e) {
  e.preventDefault();
  let viewForm  = document.getElementById("view-form");
  let selection = viewForm.gallery.selectedOptions[0];
  switch (e.submitter.id) {
    case "submit-student":
      window.open(`/html/basic/#${selection.dataset.sessionName}`);
      break;
    case "submit-moderator":
      window.open(`/html/basic/moderation/#${selection.dataset.sessionName}`);
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
  let getsPreed = formData.get("galleryType") === "prescreen";
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

  let galleryList = document.getElementById("gallery-list");
  let selection   = galleryList.selectedOptions[0];

  while (galleryList.options.length > 0) {
    galleryList.remove(0);
  }

  values.forEach(({ galleryName, isPrescreened, numWaiting, uploadNames }) => {

    let suffix = isPrescreened ? ` | ${numWaiting} awaiting moderation` : "";

    let opt      = document.createElement("option");
    opt.text     = `${galleryName}: ${uploadNames.length} submissions${suffix}`;
    opt.selected = selection !== undefined && selection.dataset.sessionName === galleryName;
    opt.dataset.sessionName   = galleryName;
    opt.dataset.isPrescreened = isPrescreened;

    galleryList.add(opt);

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

