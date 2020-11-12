let syncLoopID = undefined;
let syncRate   = 5000;

window.onEnter = (f) => (e) => {
  if (e.keyCode === 13) {
    return f(e);
  }
};

let viewForm = document.getElementById("view-form");
viewForm.addEventListener('keyup',  onEnter(submitToView));
viewForm.addEventListener('submit', submitToView);

let initForm = document.getElementById("init-form");
initForm.addEventListener('keyup',  onEnter(submitToInit));
initForm.addEventListener('submit', submitToInit);

let submitStudent = document.getElementById("submit-student");
submitStudent.disabled = true;

let submitModerator = document.getElementById("submit-moderator");
submitModerator.disabled = true;

let submitInit = document.getElementById("submit-init");
submitInit.disabled = true;

document.getElementById("sesh-name-input").addEventListener('input', (e) => {
  submitInit.disabled = e.target.value.length === 0;
});

document.getElementById("gallery-list").addEventListener('change', (e) => {
  let selection = document.getElementById("gallery-list").selectedOptions[0];
  if (selection !== undefined) {
    document.getElementById("submit-student"  ).disabled = false;
    document.getElementById("submit-moderator").disabled = selection.dataset.isPrescreened === "false";
  }
});

let tokenMaybe = window.localStorage.getItem("mod-token");
if (tokenMaybe !== null) {
  sync()
  syncLoopID = setInterval(sync, syncRate);
} else {
  fetch(window.thisDomain + "/moderator-token", { method: "GET" }).then((x) => x.text()).then(
    function (t) {
      window.localStorage.setItem("mod-token", t);
      sync()
      syncLoopID = setInterval(sync, syncRate);
    }
  );
}
