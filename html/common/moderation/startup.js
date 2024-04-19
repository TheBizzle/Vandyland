window.startSession();

let modal = document.getElementById("item-details");

window.hideModal = function() {
  modal.classList.add("hidden");
};

saveWork = function(data, uploadName) { return function() {

  let url = window.URL.createObjectURL(new Blob([data], { type: "octet/stream" }));

  let a      = document.createElement("a");
  a.href     = url;
  a.download = uploadName + ".txt";
  a.setAttribute("type", "hidden");

  document.body.appendChild(a);
  a.click();
  a.remove();

  window.URL.revokeObjectURL(url);

}};

window.showModal = function(sessionName, uploadName, metadata, data, imgSrc, isApproved) {

  if (isApproved) {
    document.getElementById("item-details").classList.add("approved");
  } else {
    document.getElementById("item-details").classList.remove("approved");
  }

  document.getElementById("item-header"         ).innerText = metadata === null ? uploadName : uploadName + " by " + metadata;
  document.getElementById("item-details-image"  ).src       = imgSrc;
  document.getElementById("item-approve-button" ).onclick   = function() { window.approve(uploadName); };
  document.getElementById("item-reject-button"  ).onclick   = function() { window.reject( uploadName); };
  document.getElementById("item-download-button").onclick   = saveWork(data, uploadName);
  document.getElementById("item-display-button" ).onclick   = function() { alert("Data: " + data); };

  document.getElementById("item-details").classList.remove("hidden");

};

window.onclick = function(e) {
  if (e.target === modal) {
    hideModal();
  }
};

// On Esc, hide the modal
document.addEventListener('keyup', function(e) { if (e.keyCode === 27) { hideModal(); } });

let makeEntriesFromFormData = function(obj, formData) {
  for (let key in (obj || {})) {
    formData.set(key, obj[key]);
  }
  return formData.entries();
};

let makeEntriesShim = function(obj, formElem) {

  let entryMap = new Map()

  if (formElem !== undefined) {
    Array.from(formElem.querySelectorAll("input[name]")).forEach((elem) => entryMap.set(elem.name, elem.value));
  }

  for (let key in (obj || {})) {
    entryMap.set(key, obj[key]);
  }

  return Array.from(entryMap.entries());

}

window.makeQueryString = function(obj, formElem) {
  let removeCRs = (str) => str.replace(/\r/g, '');
  let formData  = new FormData(formElem);
  let entries   = (formData.entries !== undefined) ? makeEntriesFromFormData(obj, formData) : makeEntriesShim(obj, formElem);
  return Array.from(entries).map(([k, v]) => encodeURIComponent(k) + "=" + encodeURIComponent(removeCRs(v))).join("&");
}
