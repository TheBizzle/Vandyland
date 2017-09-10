let uploadForm = document.getElementById("upload-form");

uploadForm.addEventListener('keyup',  onEnter(upload));
uploadForm.addEventListener('submit', upload);

let modal = document.getElementById("item-details");

window.hideModal = function() {
  modal.classList.add("hidden");
};

let addCommentTo = function(commentsElem) { return function(comment) {

  let makeCommentElem =
    function({ comment, author, time }) {
      let template = document.getElementById("comment-template");
      template.content.querySelector(".comment-text"  ).textContent = comment;
      template.content.querySelector(".comment-author").textContent = author;
      template.content.querySelector(".comment-time"  ).textContent = (Date.now() - new Date(time)) + " milliseconds ago";
      return document.importNode(template.content, true);
    };

  commentsElem.appendChild(makeCommentElem(comment));

}};

window.showModal = function(sessionName, uploadName, metadata, data, comments, imgSrc, commentURL) {

  document.getElementById("item-header"         ).innerText = metadata === undefined ? uploadName : uploadName + " by " + metadata;
  document.getElementById("item-details-image"  ).src       = imgSrc;
  document.getElementById("item-download-button").onclick   = function() { alert("Data: " + JSON.stringify(data)); };

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

  let makeFormData =
    function(parameters) {
      let formData = new FormData();
      for (let key in parameters) {
        formData.set(key, parameters[key]);
      }
      return formData
    };

  let formData = makeFormData({ "session-id": sessionName, "item-id": uploadName, comment, author, parent: "" })
  let params   = Array.from(formData.entries()).map(([k, v]) => encodeURIComponent(k) + "=" + encodeURIComponent(v)).join("&");
  fetch(commentURL, { method: "POST", body: params, headers: { "Content-Type": "application/x-www-form-urlencoded" } })

};
