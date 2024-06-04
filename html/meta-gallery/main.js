// (Event) => Unit
window.submitToView = function(e) {
  e.preventDefault();
  let viewForm  = document.getElementById("view-form");
  let selection = viewForm.querySelector(".gallery-view-item.selected");
  let data      = selection.dataset;
  switch (e.submitter.id) {
    case "submit-student":
      window.open(`/gallery/${data.template}/#${data.sessionName}`, "_blank");
      break;
    case "submit-moderator":
      window.open(`/gallery/${data.template}/moderation/#${data.sessionName}`, "_blank");
      break;
    case "clone-button":
      uploadAnother(data);
      break;
    default:
      console.error(`Unknown submitter: ${e.submitter.name}`);
  }
};

// (DOMStringMap) => Unit
const uploadAnother = ({ description, isPrescreened, sessionName, template }) => {

  const newName = window.prompt("Customize the name", sessionName);
  if (newName !== null) {

    const newDesc = window.prompt("Customize the description", description);
    if (newDesc !== null) {

      fetch(`/starter-config/${sessionName}`, { method: "GET" }).then(
        (res) => {
          if (res.status === 200) {
            return res.text();
          } else {
            return "";
          }
        }
      ).then(
        (starterConfig) => {

          const token      = window.localStorage.getItem("mod-token");
          const configFile = new Blob([starterConfig], { type: "text/plain" });

          submitNewSession( newName, template, isPrescreened, token
                          , configFile, newDesc).then(
            (response) => {
              if (!response.ok) {
                response.text().then((reason) => alert(`Could not create new session.  ${reason}`));
              }
            }
          );

        }
      );

    }

  }

};

// ( String, String, Boolean, UUID
// , Blob, String) => Promise[Response]
const submitNewSession = ( sessionID, templateName, getsPrescreened, token
                         , configFile, description) => {
  let postData = new FormData();
  postData.append("gets-prescreened", getsPrescreened);
  postData.append("token"           , token);
  postData.append("config"          , configFile, "config");
  postData.append("description"     , description);
  const options = { method: "POST", body: postData };
  return fetch(`/new-session/${templateName}/${sessionID}`, options);
};

// (Event) => Unit
window.submitToInit = function(e) {

  e.preventDefault();

  const initForm        = document.getElementById("init-form");
  const formData        = new FormData(initForm);
  const sessionID       = formData.get("name");
  const template        = formData.get("galleryTemplate");
  const getsPrescreened = formData.get("galleryMode") === "prescreen";
  const token           = window.localStorage.getItem("mod-token");
  const configFile      = new Blob([formData.get("galleryConfig")], { type: "text/plain" });
  const description     = formData.get("description");

  submitNewSession( sessionID, template, getsPrescreened, token
                  , configFile, description).then(
    (response) => {
      if (!response.ok)
        response.text().then((reason) => alert(`Could not create new session.  ${reason}`));
      else {
        document.getElementById("description"       ).value    = "";
        document.getElementById("description-inner" ).value    = "";
        document.getElementById("sesh-name-input"   ).value    = "";
        document.getElementById("starter-code-inner").value    = "";
        document.getElementById("starter-code"      ).value    = "";
        document.getElementById("starter-code-file" ).value    = null;
        document.getElementById("submit-init"       ).disabled = true;
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

  sortedValues.forEach(({ creationTime, galleryName, template, isPrescreened
                        , description, lastSubTime, numWaiting, uploadNames }) => {

    let templateHTML = document.getElementById("gallery-view-template");
    let gView        = document.importNode(templateHTML.content, true).querySelector(".gallery-view-item");

    gView.querySelector(".gallery-view-title").innerText = galleryName;
    gView.querySelector(".gallery-view-desc" ).innerText = description;
    gView.title = `${galleryName} | ${description}`;

    gView.dataset.creationTime  = creationTime;
    gView.dataset.template      = template;
    gView.dataset.description   = description;
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
      document.getElementById("control-template"   ).innerText = template;
      document.getElementById("control-template"   ).title     = template;
      document.getElementById("control-uploads"    ).innerText = uploadNames.length;
      document.getElementById("control-date"       ).innerText = dateString;
      document.getElementById("control-unmoderated").innerText = isPrescreened ? numWaiting : "N/A";
      document.getElementById("control-description").innerText = "(hover)";
      document.getElementById("control-description").title     = description;

      document.getElementById("submit-student"  ).disabled  = false;
      document.getElementById("submit-moderator").disabled  = !isPrescreened;
      document.getElementById("clone-button"    ).disabled  = false;

    };

    galleries.appendChild(gView);

  });

};

let genSortingFn = () => {

  let sortingType = document.getElementById("sorting-type").value;

  const f = (() => {
    switch (sortingType) {
      case "Latest Initialization":
        return ((a, b) => b.creationTime - a.creationTime);
      case "Latest Submission":
        return ((a, b) => b.lastSubTime - a.lastSubTime);
      case "Alphabetical":
        return ((a, b) => a.galleryName === b.galleryName ? 0 : (a.galleryName < b.galleryName ? -1 : 1));
      case "Most Uploads":
        return ((a, b) => b.uploadNames.length - a.uploadNames.length);
      case "Most Waiting":
        return ((a, b) => b.numWaiting - a.numWaiting);
      case "Template Name":
        return ((a, b) => a.template === b.template ? 0 : (a.template < b.template ? -1 : 1));
      default:
        console.error(`Invalid sorting criteria: ${sortingType}`);
    }
  })();

  return (a, b) => {
    const result = f(a, b);
    return (result !== 0) ? result : (a.galleryName < b.galleryName ? -1 : 1);
  };

};

window.sync = function() {

  let token = window.localStorage.getItem("mod-token");

  fetch(`${window.thisDomain}/gallery-listings/${token}`, { method: "GET" }).then((x) => x.json()).then(
    function (listings) {

      let promises =
        listings.map(
          (l) => {
            const url = `${window.thisDomain}/listings/${l.galleryName}`;
            return fetch(url, { method: "GET" }).
              then((x) => x.json()).
              then((x) => { return { ...l, uploadNames: x } })
          }
        );

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
