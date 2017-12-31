if (window.location.hash.length > 1) {
  window.startSession(decodeURIComponent(window.location.hash.substring(1)));
}

let uploadForm = document.getElementById("upload-form");

uploadForm.addEventListener('keyup',  onEnter(upload));
uploadForm.addEventListener('submit', upload);

let modal = document.getElementById("item-details");

window.hideModal = function() {
  modal.classList.add("hidden");
};

let addCommentTo = function(commentsElem) { return function(comment) {

  let formatTime = function(time) {

    let millis  = Date.now() - new Date(time);

    let minutes = Math.floor(millis  / 60000);
    let hours   = Math.floor(minutes /    60);
    let days    = Math.floor(hours   /    24);
    let years   = Math.floor(days    /   365);

    let modMinutes = minutes %  60;
    let modHours   = hours   %  24;
    let modDays    = days    % 365;

    if (years !== 0) {
      return `${years} years, ${modDays} days, ${modHours} hours, and ${modMinutes} minutes ago`;
    } else if (days !== 0) {
      return `${modDays} days, ${modHours} hours, and ${modMinutes} minutes ago`;
    } else if (hours !== 0) {
      return `${modHours} hours and ${modMinutes} minutes ago`;
    } else {
      return `${modMinutes} minutes ago`;
    }

  }

  let makeCommentElem =
    function({ comment, author, time }) {
      let template = document.getElementById("comment-template");
      template.content.querySelector(".comment-text"  ).textContent = comment;
      template.content.querySelector(".comment-author").textContent = author;
      template.content.querySelector(".comment-time"  ).textContent = formatTime(time);
      return document.importNode(template.content, true);
    };

  commentsElem.appendChild(makeCommentElem(comment));

}};

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

window.showModal = function(sessionName, uploadName, metadata, data, comments, imgSrc, commentURL) {

  document.getElementById("item-header"         ).innerText = metadata === null ? uploadName : uploadName + " by " + metadata;
  document.getElementById("item-details-image"  ).src       = imgSrc;
  document.getElementById("item-download-button").onclick   = saveWork(data, uploadName);
  document.getElementById("item-display-button" ).onclick   = function() { alert("Data: " + data); };

  let commentsElem = document.getElementById("item-comments");
  commentsElem.innerHTML = "";
  comments.forEach(addCommentTo(commentsElem));

  document.getElementById("item-comment-area").dataset.postData = JSON.stringify([sessionName, uploadName, commentURL]);

  try { document.getElementById("item-comment-area").querySelector('.author-input').value =  localStorage.getItem("commenter-name"); } catch(e) {}

  document.getElementById("item-details").classList.remove("hidden");

};

window.onclick = function(e) {
  if (e.target === modal) {
    hideModal();
  }
};

// On Esc, hide the modal
document.addEventListener('keyup', function(e) { if (e.keyCode === 27) { hideModal(); } });

window.makeQueryString = function(obj, formElem) {

  let removeCRs = (str) => str.replace(/\r/g, '');

  let formData = new FormData(formElem);
  for (let key in (obj || {})) {
    formData.set(key, obj[key]);
  }

  return Array.from(formData.entries()).map(([k, v]) => encodeURIComponent(k) + "=" + encodeURIComponent(removeCRs(v))).join("&");

};

window.clearCommentFrom = function(id) {
  let elem = document.getElementById(id);
  elem.querySelector('.comment-box').innerText = "";
};

window.submitCommentFrom = function(id) {

  let elem    = document.getElementById(id);
  let comment = elem.querySelector('.comment-box').innerText;
  let author  = elem.querySelector('.author-input').value;

  addCommentTo(document.getElementById("item-comments"))({ comment, author, time: Date.now() });

  try { localStorage.setItem("commenter-name", author); } catch(e) {}

  elem.querySelector('.comment-box').innerText = "";

  let [sessionName, uploadName, commentURL] = JSON.parse(document.getElementById("item-comment-area").dataset.postData);

  let params = makeQueryString({ "session-id": sessionName, "item-id": uploadName, comment, author, parent: "" })
  fetch(commentURL, { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } })

};
