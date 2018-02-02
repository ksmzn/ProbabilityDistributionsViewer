// Initial Tracking Code
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');
(function(){
    var uaid = '';
    var domain = 'statdist.ksmzn.com';

    if (location.host == domain){
        uaid = 'UA-12393166-9';
    }else{
        uaid = 'UA-12393166-8';
    }

    ga('create', uaid, 'auto');
    ga('send', 'pageview');
})();

// Event Tracking Code
$(document).on('shiny:inputchanged', function(event) {
  if(['selected_language', 'tabs'].indexOf(event.name) >= 0 && event.value !== null){
    ga('send', 'event', 'input',
      'updates', event.name, event.value);
  }
})
