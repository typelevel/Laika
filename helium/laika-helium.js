
function switchTabs (elements, choiceName) { 
  elements.forEach(elem => {
    if (elem.dataset.choiceName === choiceName) elem.classList.add("active");
    else (elem.classList.remove("active"));
  });
}

function initTabs () {
  const groups = {};
  const tabs = document.querySelectorAll(".tab")
  const content = document.querySelectorAll(".tab-content")
  tabs.forEach(tab => {
    const groupName = tab.parentElement.parentElement.dataset.tabGroup;
    const choiceName = tab.dataset.choiceName;
    if (groupName && choiceName) {
      if (groups[groupName] === undefined) groups[groupName] = [];
      groups[groupName].push(tab);
      tab.firstElementChild.onclick = (e) => {
        e.preventDefault();
        switchTabs(groups[groupName], choiceName);
      };
    }
  });
  content.forEach(c => {
    const group = c.parentElement.dataset.tabGroup;
    if (group && groups[group]) groups[group].push(c);
  });
}

document.addEventListener('DOMContentLoaded', () => {
  document.getElementById("nav-icon").onclick = () => {
    document.getElementById("sidebar").classList.toggle("nav-open");
  };
  initTabs();
});
