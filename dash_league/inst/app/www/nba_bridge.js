// Bridge between an embedded league dashboard and the entry point that hosts it.
// The entry owns the customer's league list, so the dashboard only asks it to
// reopen the league chooser; it never receives the list itself.
(function () {
  function isEmbedded() {
    return window.parent && window.parent !== window;
  }

  window.nbaChooseLeague = function () {
    if (isEmbedded()) {
      window.parent.postMessage({ type: "nba:choose" }, "*");
    }
  };
})();
