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

		// record or randomly select sequence
		if ($('input#sequence').val() === "random") {
			surveysSlide.sequence = randomElementNR(surveysSlide.seqList);
		} else if (["1","2","3","4","5","6"].indexOf($('input#sequence').val()) === -1) {
			window.alert("Please enter a sequence number between 1 and 6.");
		} else {
			seqNumber = parseInt($('input#sequence').val());
			surveysSlide.sequence = surveysSlide.seqList[seqNumber - 1];
		}

		experiment.newData.sequence = surveysSlide.sequence.seqName;
		experiment.blocks = surveysSlide.sequence.blocks;
		experiment.predicates = surveysSlide.sequence.predicateOrder;
		experiment.subsets = surveysSlide.sequence.subsetOrder;
		experiment.totalTrials = surveysSlide.sequence.subsetOrder[0].length + surveysSlide.sequence.subsetOrder[1].length + surveysSlide.sequence.subsetOrder[2].length + 2;

		// set up characters slide
		charactersSlide.makeOrder();
		charactersSlide.next();
		window.scrollTo(0, 0);
		showSlide('characters');
	}
})