let syncLoopID = undefined;
let syncRate   = 5000;

window.onEnter = (f) => (e) => {
  if (e.keyCode === 13) {
    return f(e);
  }
};

let viewForm = document.getElementById("view-form");
viewForm.addEventListener('keyup',    onEnter(submitToView));
viewForm.addEventListener('submit',   submitToView);

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

fetch(`${window.thisDomain}/gallery-types`, { method: "GET" }).then((x) => x.json()).then(
  (types) => {

    const select     = document.getElementById("gallery-template");
    select.innerHTML = "";

    types.forEach(
      (t, i) => {
        const option     = document.createElement("option");
        option.innerText = t;
        select.appendChild(option);
        if (t === "basic") {
          select.selectedIndex = i;
        }
      }
    );

  }
)

let originalStarter = null;

window.showStarterModal = function() {
  const modal     = document.getElementById("starter-modal");
  const actual    = document.getElementById("starter-code");
  const inner     = modal.querySelector("#starter-code-inner");
  inner.value     = actual.value;
  originalStarter = inner.value;
  modal.classList.remove("hidden");
};

window.hideModal = function() {
  const modal = document.getElementById("starter-modal");
  modal.querySelector("#starter-code-inner").value = originalStarter;
  originalStarter                                  = null;
  modal.classList.add("hidden");
};

document.getElementById("starter-code-file").oninput = function(e) {

  const elem = e.target;
  const file = elem.files[0];

  const reader = new FileReader();
  reader.onload = () => {
    document.getElementById("starter-code-inner").value = reader.result;
  }

  reader.readAsText(file);

};

document.getElementById("item-save-button").onclick = function() {
  const elem = document.getElementById("starter-code-inner");
  const code = elem.value;
  try {
    JSON.parse(code);
    document.getElementById("starter-code").value = code;
    hideModal();
  } catch (error) {
    alert("Could not parse starter code as JSON.  Your starter code was NOT saved!")
    console.error("Starter code failure", error);
  }
}

document.getElementById("sorting-type").addEventListener("change", (e) => {
  window.refreshSorting();
});
