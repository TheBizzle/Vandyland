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

document.getElementById("sesh-name-input").addEventListener('input', (e) => {
  const submitInit = document.getElementById("submit-init");
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

    types.sort().forEach(
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

let originalDesc = null;

window.showDescModal = function() {
  const modal  = document.getElementById("description-modal");
  const actual = document.getElementById("description");
  const inner  = modal.querySelector("#description-inner");
  inner.value  = actual.value;
  originalDesc = inner.value;
  modal.classList.remove("hidden");
};

window.hideDescModal = function() {
  const modal = document.getElementById("description-modal");
  modal.querySelector("#description-inner").value = originalDesc;
  originalDesc                                    = null;
  modal.classList.add("hidden");
};

let originalStarter = null;

window.showStarterModal = function() {
  const modal     = document.getElementById("starter-modal");
  const actual    = document.getElementById("starter-code");
  const inner     = modal.querySelector("#starter-code-inner");
  inner.value     = actual.value;
  originalStarter = inner.value;
  modal.classList.remove("hidden");
};

window.hideStarterModal = function() {
  const modal = document.getElementById("starter-modal");
  modal.querySelector("#starter-code-inner").value = originalStarter;
  originalStarter                                  = null;
  modal.classList.add("hidden");
};

document.getElementById("starter-code-file").oninput = function(e) {

  const elem = e.target;
  const file = elem.files[0];

  const mode = document.querySelector('input[name="starter-mode"]:checked').value;

  const trueReader = new FileReader();
  trueReader.onloadend = (event) => {
    document.getElementById("starter-code-inner").value = event.target.result;
  }

  switch (mode) {

    case "Plain Text":
      trueReader.readAsText(file);
      break;

    case "Base64":
      trueReader.readAsDataURL(file);
      break;

    case "Auto":

      const reader = new FileReader();

      reader.onloadend = (e) => {

        const arr   = new Uint8Array(e.target.result);
        let isASCII = true;

        for (let i = 0; i < arr.length; i++) {
          // Checking for values <= 127 didn't suffice;
          // The en-dash, for example, is in Extended ASCII at 377 AKA 0x226
          if (arr[i] === 0) {
            isASCII = false;
            break;
          }
        }

        if (isASCII) {
          trueReader.readAsText(file);
        } else {
          trueReader.readAsDataURL(file);
        }

      };

      reader.readAsArrayBuffer(file);

      break;

    default:
      console.warn(`Unknown reading mode: ${mode}`);
  }


};

document.getElementById("desc-save-button").onclick = () => {
  const elem = document.getElementById("description-inner");
  const desc = elem.value;
  document.getElementById("description").value = desc;
  hideDescModal();
};

document.getElementById("code-save-button").onclick = () => {
  const elem    = document.getElementById("starter-code-inner");
  const starter = elem.value;
  document.getElementById("starter-code").value = starter;
  hideStarterModal();
};

document.getElementById("sorting-type").addEventListener("change", (e) => {
  window.refreshSorting();
});

document.getElementById("sesh-name-input").addEventListener("input", (e) => {
  e.target.value = e.target.value.toLowerCase();
});

// On Esc, hide the modal
document.addEventListener('keyup', (e) => {
  if (e.keyCode === 27) {
    hideDescModal();
    hideStarterModal();
  }
});

document.getElementById("mode-auto").addEventListener("change", (e) => {
  document.getElementById("mode-value").innerText = "Auto (-)";
});

document.getElementById("mode-text").addEventListener("change", (e) => {
  document.getElementById("mode-value").innerText = "Plain Text";
});

document.getElementById("mode-base64").addEventListener("change", (e) => {
  document.getElementById("mode-value").innerText = "Base64";
});
