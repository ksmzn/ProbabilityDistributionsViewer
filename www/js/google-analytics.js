try{
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
    (function(){
        var uaid = "";
        var domain = "statdist.ksmzn.com";

        if (location.host == domain){
            uaid = 'UA-12393166-9';
        }else{
            uaid = 'UA-12393166-8';
        }

        ga('create', uaid, 'auto');
        ga('send', 'pageview');
    })();
}catch(e){
    console.error(e);
}
