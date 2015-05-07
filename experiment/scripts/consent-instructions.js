/* set up buttons on start and instructions slides */

// $('.slide#start button').click(function() {
// 	showSlide('instructions');
// })

// $('.slide#start button').click(function() {
// 	if(turk.previewMode === false) {
// 		charactersSlide.makeOrder();
// 		charactersSlide.next();
// 		window.scrollTo(0, 0);
// 		showSlide('characters');
// 	}
// })

$('.slide#start button').click(function() {

	// if no subid, prevent progress
	if($('input#subid').val() === "") {
		window.alert("Please enter a subid.");

	} else {
		// record subid in experiment object
		experiment.newData.subid = $('input#subid').val();
		charactersSlide.makeOrder();
		charactersSlide.next();
		window.scrollTo(0, 0);
		showSlide('characters');
	}
})

