// https://github.com/jgm/pandoc/blob/29fa97ab96b8e2d62d48326e1b949a71dc41f47a/src/Text/Pandoc/Writers/HTML.hs#L332-L345
document.addEventListener("DOMContentLoaded", function () {
  var mathElements = document.getElementsByClassName("math");
  var macros = [];
  for (var i = 0; i < mathElements.length; i++) {
    var texText = mathElements[i].firstChild;
    if (mathElements[i].tagName == "SPAN") {
      katex.render(texText.data, mathElements[i], {
        displayMode: mathElements[i].classList.contains("display"),
        throwOnError: false,
        macros: macros,
        fleqn: false
      });
    }}});
