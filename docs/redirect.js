(function () {
  var newOrigin = "https://open-science-course.github.io";
  var prefix = "/course_website";
  var path = location.pathname;

  if (path.indexOf(prefix) === 0) {
    path = path.slice(prefix.length) || "/";
  }
  if (path === "/index.html") {
    path = "/";
  }

  var target = newOrigin + path + location.search + location.hash;
  var link = document.getElementById("new-link");
  if (link) {
    link.href = target;
    link.textContent = target;
  }
  location.replace(target);
})();
