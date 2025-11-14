/* http://gregfranko.com/blog/jquery-best-practices/ */
(function ($) {
  $(function () {

    $('nav.navbar').headroom();

    Toc.init({
      $nav: $("#toc"),
      $scope: $("main h2, main h3, main h4, main h5, main h6")
    });

    if ($('#toc').length) {
      $('body').scrollspy({
        target: '#toc',
        offset: $("nav.navbar").outerHeight() + 1
      });
    }

    // Activate popovers
    $('[data-bs-toggle="popover"]').popover({
      container: 'body',
      html: true,
      trigger: 'focus',
      placement: "top",
      sanitize: false,
    });

    $('[data-bs-toggle="tooltip"]').tooltip();

    /* Clipboard --------------------------*/

    function changeTooltipMessage(element, msg) {
      var tooltipOriginalTitle = element.getAttribute('data-bs-original-title');
      element.setAttribute('data-bs-original-title', msg);
      $(element).tooltip('show');
      element.setAttribute('data-bs-original-title', tooltipOriginalTitle);
    }

    if (ClipboardJS.isSupported()) {
      $(document).ready(function () {
        var copyButton = "<button type='button' class='btn btn-primary btn-copy-ex' title='Copy to clipboard' aria-label='Copy to clipboard' data-toggle='tooltip' data-placement='left' data-trigger='hover' data-clipboard-copy><i class='fa fa-copy'></i></button>";

        $("div.sourceCode").addClass("hasCopyButton");

        // Insert copy buttons:
        $(copyButton).prependTo(".hasCopyButton");

        // Initialize tooltips:
        $('.btn-copy-ex').tooltip({ container: 'body' });

        // Initialize clipboard:
        var clipboard = new ClipboardJS('[data-clipboard-copy]', {
          text: function (trigger) {
            return trigger.parentNode.textContent.replace(/\n#>[^\n]*/g, "");
          }
        });

        clipboard.on('success', function (e) {
          changeTooltipMessage(e.trigger, 'Copied!');
          e.clearSelection();
        });

        clipboard.on('error', function (e) {
          changeTooltipMessage(e.trigger, 'Press Ctrl+C or Command+C to copy');
        });

      });
    }

    /* Search marking --------------------------*/
    var url = new URL(window.location.href);
    var toMark = url.searchParams.get("q");
    var mark = new Mark("main#main");
    if (toMark) {
      mark.mark(toMark, {
        accuracy: {
          value: "complementary",
          limiters: [",", ".", ":", "/"],
        }
      });
    }

    /* Search --------------------------*/
    /* Adapted from https://github.com/rstudio/bookdown/blob/2d692ba4b61f1e466c92e78fd712b0ab08c11d31/inst/resources/bs4_book/bs4_book.js#L25 */
    // Initialise search index on focus
    var fuse;
    $("#search-input").focus(async function (e) {
      if (fuse) {
        return;
      }

      $(e.target).addClass("loading");
      var response = await fetch($("#search-input").data("search-index"));
      var data = await response.json();

      var options = {
        keys: ["what", "text", "code"],
        ignoreLocation: true,
        threshold: 0.1,
        includeMatches: true,
        includeScore: true,
      };
      fuse = new Fuse(data, options);

      $(e.target).removeClass("loading");
    });

    // Use algolia autocomplete
    var options = {
      autoselect: true,
      debug: true,
      hint: false,
      minLength: 2,
    };
    var q;
    async function searchFuse(query, callback) {
      await fuse;

      var items;
      if (!fuse) {
        items = [];
      } else {
        q = query;
        var results = fuse.search(query, { limit: 20 });
        items = results
          .filter((x) => x.score <= 0.75)
          .map((x) => x.item);
        if (items.length === 0) {
          items = [{ dir: "Sorry 😿", previous_headings: "", title: "No results found.", what: "No results found.", path: window.location.href }];
        }
      }
      callback(items);
    }
    $("#search-input").autocomplete(options, [
      {
        name: "content",
        source: searchFuse,
        templates: {
          suggestion: (s) => {
            if (s.title == s.what) {
              return `${s.dir} >	<div class="search-details"> ${s.title}</div>`;
            } else if (s.previous_headings == "") {
              return `${s.dir} >	<div class="search-details"> ${s.title}</div> > ${s.what}`;
            } else {
              return `${s.dir} >	<div class="search-details"> ${s.title}</div> > ${s.previous_headings} > ${s.what}`;
            }
          },
        },
      },
    ]).on('autocomplete:selected', function (event, s) {
      window.location.href = s.path + "?q=" + q + "#" + s.id;
    });
  });
})(window.jQuery || window.$)

document.addEventListener('keydown', function (event) {
  // Check if the pressed key is '/'
  if (event.key === '/') {
    event.preventDefault();  // Prevent any default action associated with the '/' key
    document.getElementById('search-input').focus();  // Set focus to the search input
  }
});
