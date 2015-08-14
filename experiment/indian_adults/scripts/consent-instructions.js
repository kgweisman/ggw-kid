/* set up buttons on start and instructions slides */

// $('.slide#start button').click(function() {
// 	showSlide('instructions');
// })

$('.slide#instructions button').click(function() {
	if(turk.previewMode === false) {
		charactersSlide.makeOrder();
		charactersSlide.next();
		window.scrollTo(0, 0);
		showSlide('characters');
	}
});

$('.slide#consent button').click(function() {

	// randomly select sequence
		surveysSlide.sequence = randomElementNR(surveysSlide.seqList);

		experiment.newData.sequence = surveysSlide.sequence.seqName;
		experiment.predicates = surveysSlide.sequence.predicateOrder;
		experiment.subsets = surveysSlide.sequence.subsetOrder;
		experiment.totalTrials = surveysSlide.sequence.subsetOrder[0].length + surveysSlide.sequence.subsetOrder[1].length + surveysSlide.sequence.subsetOrder[2].length + 2;

		// set up characters slide
		window.scrollTo(0, 0);
		showSlide('instructions');
});

$('.slide#instructions2 button').click(function() {
	if(turk.previewMode === false) {
		practiceSlide.next();
	}
});
