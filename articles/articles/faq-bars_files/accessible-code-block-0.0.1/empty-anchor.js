// Hide empty <a> tag within highlighted CodeBlock for screen reader accessibility (see https://github.com/jgm/pandoc/issues/6352#issuecomment-626106786) -->
// v0.0.1
// Written by JooYoung Seo (jooyoung@psu.edu) and Atsushi Yasumoto on June 1st, 2020.

document.addEventListener('DOMContentLoaded', function() {
  const codeList = document.getElementsByClassName("sourceCode");
  for (var i = 0; i < codeList.length; i++) {
    var linkList = codeList[i].getElementsByTagName('a');
    for (var j = 0; j < linkList.length; j++) {
      if (linkList[j].innerHTML === "") {
        linkList[j].setAttribute('aria-hidden', 'true');
      }
    }
  }
});
