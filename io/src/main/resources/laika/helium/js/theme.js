
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

function initNavToggle () {
  const navIcon = document.getElementById("nav-icon");
  const sidebar = document.getElementById("sidebar");
  if (navIcon && sidebar) {
    navIcon.onclick = () => {
      sidebar.classList.toggle("nav-open");
    };
  }
}

function initMenuToggles () {
  // this functionality applies to all types of menus, including the version menu
  document.querySelectorAll(".menu-container").forEach((container) => {
    const toggle = container.querySelector(".menu-toggle");
    const content = container.querySelector(".menu-content");
    if (toggle && content) {
      const closeHandler = (evt) => {
        const contentClicked = evt.target.closest(".menu-content");
        const toggleClicked = evt.target.closest(".menu-toggle");
        if ((!toggleClicked || toggleClicked !== toggle) && (!contentClicked || contentClicked !== content)) {
          content.classList.remove("menu-open");
          document.removeEventListener("click", closeHandler)
        }
      }
      toggle.onclick = () => {
        if (content.classList.toggle("menu-open")) {
          document.addEventListener("click", closeHandler);
        }
      };
    }
  });
}

function initColorModeToggle () {
  const btn = document.getElementById("theme-toggle");
  if (!btn) return;

  function currentMode () {
    const attr = document.documentElement.getAttribute("data-color-mode");
    return (attr === "light" || attr === "dark") ? attr : "auto";
  }

  function applyMode (mode) {
    if (mode === "light" || mode === "dark") {
      document.documentElement.setAttribute("data-color-mode", mode);
      try { localStorage.setItem("laika-color-mode", mode); } catch (e) {}
    } else {
      document.documentElement.removeAttribute("data-color-mode");
      try { localStorage.removeItem("laika-color-mode"); } catch (e) {}
    }
    updateUi(mode);
  }

  function updateUi (mode) {
    if (mode === "light") {
      btn.textContent = "Light";
      btn.title = "Switch to dark mode";
      btn.setAttribute("aria-label", "Switch to dark mode");
    } else if (mode === "dark") {
      btn.textContent = "Dark";
      btn.title = "Switch to system mode";
      btn.setAttribute("aria-label", "Switch to system mode");
    } else {
      btn.textContent = "Auto";
      btn.title = "Switch to light mode";
      btn.setAttribute("aria-label", "Switch to light mode");
    }
  }

  updateUi(currentMode());

  btn.addEventListener("click", () => {
    const mode = currentMode();
    const next = (mode === "auto") ? "light" : (mode === "light") ? "dark" : "auto";
    applyMode(next);
  });
}

document.addEventListener('DOMContentLoaded', () => {
  initNavToggle();
  initMenuToggles();
  initTabs();
  initColorModeToggle();
});
