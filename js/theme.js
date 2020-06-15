
document.addEventListener('DOMContentLoaded', () => {
  document.getElementById("open-nav-icon").onclick = () => {
    document.getElementById("sidebar").classList.add("nav-open");
  };
  document.getElementById("close-nav-icon").onclick = () => {
    document.getElementById("sidebar").classList.remove("nav-open");
  };
});
