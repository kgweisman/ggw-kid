/* set up buttons on start and instructions slides */

// $('.slide#start button').click(function() {
// 	showSlide('instructions');
// })

$('.slide#start button').click(function() {
	if(turk.previewMode === false) {
		charactersSlide.makeOrder();
		charactersSlide.next();
		window.scrollTo(0, 0);
		showSlide('characters');
	}
})