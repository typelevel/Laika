
function switchTabs (elements, choiceName) { 
  elements.forEach(elem => {
    if (elem.dataset.choiceName == choiceName) elem.classList.add("active");
    else (elem.classList.remove("active"));
  });
}

function initTabs () {
  const groups = {"foo": []}; // TODO - populate by theme renderer
  const tabs = document.querySelectorAll(".tab")
  const content = document.querySelectorAll(".tab-content")
  tabs.forEach(tab => {
    const group = tab.parentElement.parentElement.dataset.tabGroup;
    const choiceName = tab.dataset.choiceName;
    if (group && groups[group] && choiceName) {
      groups[group].push(tab);
      tab.firstElementChild.onclick = (e) => {
        e.preventDefault();
        switchTabs(groups[group], choiceName);
      };
    }
  });
  content.forEach(c => {
    const group = c.parentElement.dataset.tabGroup;
    if (group && groups[group]) groups[group].push(c);
  });
}

document.addEventListener('DOMContentLoaded', () => {
  document.getElementById("open-nav-icon").onclick = () => {
    document.getElementById("sidebar").classList.add("nav-open");
  };
  document.getElementById("close-nav-icon").onclick = () => {
    document.getElementById("sidebar").classList.remove("nav-open");
  };
  initTabs();
});
