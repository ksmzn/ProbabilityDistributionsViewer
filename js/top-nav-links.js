//JavaScript for hacking around the default behavior of Shiny

$("#top-nav a[data-value]").each(function() {
	// this references the DOM element and we change its 'href' attribute.
	if(this.getAttribute('data-value').substring(0,4) == 'java') {
		this.setAttribute('onClick', this.getAttribute('data-value'))
		this.setAttribute('data-toggle', null);
	} else {
		this.setAttribute('href', this.getAttribute('data-value'));
		this.setAttribute('target', '_blank');
		this.setAttribute('data-toggle', null);
	}
});


$('.fa-code').parent().parent().toggleClass('active');

