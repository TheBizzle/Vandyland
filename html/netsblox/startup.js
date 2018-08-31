window.startSession(parent.location.hash.substring(1));

window.makeQueryString = function(obj, formElem) {

  let removeCRs = (str) => str.replace(/\r/g, '');

  let formData = new FormData(formElem);
  for (let key in (obj || {})) {
    formData.set(key, obj[key]);
  }

  return Array.from(formData.entries()).map(([k, v]) => encodeURIComponent(k) + "=" + encodeURIComponent(removeCRs(v))).join("&");

};
