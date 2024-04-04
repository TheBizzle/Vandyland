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

let lastValues = [];

window.refreshSorting = function() {
  render(lastValues);
}

let render = function(values) {

  let galleries = document.getElementById("view-list");
  let selection = galleries.querySelector(".selected");

  lastValues = values;

  galleries.innerHTML = "";

  let sortedValues = values.sort(genSortingFn());

  sortedValues.forEach(({ creationTime, galleryName, isPrescreened, lastSubTime
                        , numWaiting, uploadNames }) => {

    let template = document.getElementById("gallery-view-template");
    let gView    = document.importNode(template.content, true).querySelector(".gallery-view-item");

    gView.querySelector(".gallery-view-title").innerText = galleryName;
    gView.title                                          = galleryName;

    gView.dataset.creationTime  = creationTime;
    gView.dataset.isPrescreened = isPrescreened;
    gView.dataset.lastSubTime   = lastSubTime;
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

      let creationDate = new Date(creationTime);
      let dateString   = creationDate.toLocaleDateString("en-US", { year: "2-digit", month: "numeric", day: "numeric" });

      document.getElementById("control-name"       ).innerText = galleryName;
      document.getElementById("control-name"       ).title     = galleryName;
      document.getElementById("control-uploads"    ).innerText = uploadNames.length;
      document.getElementById("control-date"       ).innerText = dateString;
      document.getElementById("control-unmoderated").innerText = isPrescreened ? numWaiting : "N/A";

      document.getElementById("submit-student"  ).disabled  = false;
      document.getElementById("submit-moderator").disabled  = !isPrescreened;

    };

    galleries.appendChild(gView);

  });

};

let genSortingFn = () => {

  let sortingType = document.getElementById("sorting-type").value;

  switch (sortingType) {
    case "Latest Initialization":
      return ((a, b) => b.creationTime - a.creationTime);
    case "Latest Submission":
      return ((a, b) => b.lastSubTime - a.lastSubTime);
    case "Alphabetical":
      return ((a, b) => a.galleryName < b.galleryName ? -1 : 1);
    case "Most Uploads":
      return ((a, b) => b.uploadNames.length - a.uploadNames.length);
    case "Most Waiting":
      return ((a, b) => b.numWaiting - a.numWaiting);
    default:
      console.error(`Invalid sorting criteria: ${sortingType}`);
  }

};

window.sync = function() {

  let token = window.localStorage.getItem("mod-token");

  fetch(`${window.thisDomain}/gallery-listings/${token}`, { method: "GET" }).then((x) => x.json()).then(
    function (listings) {

      let promises =
        listings.map(({ creationTime, galleryName, isPrescreened, lastSubTime
                      , numApproved, numWaiting }) => {
          return fetch(`${window.thisDomain}/listings/${galleryName}`, { method: "GET" }).
            then((x) => x.json()).
            then((x) => { return { creationTime, galleryName, isPrescreened
                                 , lastSubTime, numApproved, numWaiting
                                 , uploadNames: x } })
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
