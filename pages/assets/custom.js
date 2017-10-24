$(document).ready(function() {
  // Remove the # character from href value.
  var selectedTabDataValue = window.location.hash.substring(1);

  if (selectedTabDataValue) {
    $('a[data-value=' + selectedTabDataValue + ']').tab('show');
  }

  $('.nav-tabs a').on('click', function(event) {
    var tab = event.target.getAttribute("data-value");
    if (tab === null) return;
    window.location.hash = tab;
  });
})
