
function refresh(targets) {
  targets.forEach( target => {
    sessionStorage.setItem('scrollpos-' + target.id, target.scrollTop.toString());
  });
  window.location.reload();
}

function restoreScrollPos(targets) {
  targets.forEach( target => {
    let scrollPos = sessionStorage.getItem('scrollpos-' + target.id);
    if (scrollPos) target.scrollTop = parseInt(scrollPos);
    sessionStorage.removeItem('scrollpos-' + target.id);
  });
}

function initSSE (targets) {
  const evtSource = new EventSource("/laika/events");
  evtSource.onmessage = function(event) {
    console.log("SSE " + event.data);
    if (event.data === "refresh") refresh(targets);
  };
  evtSource.onopen = function() {
    console.log("SSE opened");
  };
  evtSource.onerror = function(event) {
    console.log("SSE error: " + JSON.stringify(event));
  };
}

function initPreview (targetIds) {
  document.addEventListener('DOMContentLoaded', () => {
    let targets = targetIds.map(id => document.getElementById(id));
    restoreScrollPos(targets);
    initSSE(targets);
  });
}
