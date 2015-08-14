/* set up comprehension check on instructions slide */
	

// set up attempts counter
var attempts = [];

// set up button behavior
$('.slide#surveys button').click(function() {

	// store condition
	// var keyCondName = currentPredicate.condName.toLowerCase().replace(/\s+/g, '');
	var keyCondName = currentPredicate.compCheck

	// store answer
	var answer = $('input[type="radio"]:checked', '#comp-check-form').val();

	if (answer === undefined) {

		// request answer
		window.alert ("Please select the best answer from the options provided.");			

	} else if (answer !== keyCondName) {

		// record attempt
		attempts.push(answer);

		if (attempts.length >= 3) {
			// give sterner warning
			window.alert("That is not the correct answer. You need to find the correct answer before you can continue this survey. If you are unable to find the correct answer, please return this HIT.");

		} else {
			// give another try
			window.alert ("That is not the correct answer. Please try again.");				
		}

	} else {

		// record attempt
		attempts.push(answer); console.log(attempts);

		// record data
		if (experiment.predicates.length > 1) { 
			experiment.newData.comprehensionCheck1 = attempts;
		} if (experiment.predicates.length === 1) {
			experiment.newData.comprehensionCheck2 = attempts;
		} if (experiment.predicates.length < 1) {
			experiment.newData.comprehensionCheck3 = attempts;
		}

		// clear attempts and radio selection
		attempts = [];
		$('input[type="radio"]:checked', '#comp-check-form').prop('checked', false);

		// go to the next trial
		experiment.next();
		window.scrollTo(0, 0);
	}

});

