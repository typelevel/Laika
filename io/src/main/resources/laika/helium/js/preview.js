
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

function initPreview (targetIds, pollInterval) {
  document.addEventListener('DOMContentLoaded', () => {
    let targets = targetIds.map(id => document.getElementById(id));
    restoreScrollPos(targets);
    setTimeout(() => refresh(targets), pollInterval);
  });
}
