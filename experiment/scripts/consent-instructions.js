/* set up buttons on consent and instructions slides */

// $('.slide#consent button').click(function() {
// 	showSlide('instructions');
// })

$('.slide#consent button').click(function() {
	if(turk.previewMode === false) {
		charactersSlide.makeOrder();
		charactersSlide.next();
		window.scrollTo(0, 0);
		showSlide('characters');
	}
})