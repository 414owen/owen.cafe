(function(){
  var ml = 'multi-listing';
  var els = document.getElementsByClassName(ml);
  function expandListing(e) {
    var el = e.target;
    while(el.className.indexOf(ml) == -1) el = el.parentElement;
    el.className += ' expanded';
  }
  for(var i = 0; i < els.length; i++){
    els[i].addEventListener('click', expandListing);
  }
})();
