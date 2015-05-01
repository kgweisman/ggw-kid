/* set up demographics slide */

// set up button behavior
$('.slide#demographics button').click(function() { 
	// record demographic info...
	// text inputs
	experiment.newData.age = $('input#age', '#demographicsForm').val();
	experiment.newData.job = $('input#job', '#demographicsForm').val(); 
	experiment.newData.country = $('input#country', '#demographicsForm').val();
	experiment.newData.children = $('input#children', '#demographicsForm').val();

	// text areas
	experiment.newData.comments = $('.slide#demographics textarea#comments').val();

	// multiple choice radios
	experiment.newData.gender = $('input[name=gender]:checked', '#demographicsForm').val();
	experiment.newData.education = $('input[name=education]:checked', '#demographicsForm').val();
	experiment.newData.englishNative = $('input[name=englishNative]:checked', '#demographicsForm').val();
	experiment.newData.maritalStatus = $('input[name=maritalStatus]:checked', '#demographicsForm').val();
	experiment.newData.vegetarian = $('input[name=vegetarian]:checked', '#demographicsForm').val();
	experiment.newData.dog = $('input[name=dog]:checked', '#demographicsForm').val();
	experiment.newData.studyMoralPhil = $('input[name=studyMoralPhil]:checked', '#demographicsForm').val();
	experiment.newData.politicalIdeology = $('input[name=politicalIdeology]:checked', '#demographicsForm').val();
	experiment.newData.beliefGod = $('input[name=beliefGod]:checked', '#demographicsForm').val();
	experiment.newData.beliefTradition = $('input[name=beliefTradition]:checked', '#demographicsForm').val();
	experiment.newData.beliefAfterlife = $('input[name=beliefAfterlife]:checked', '#demographicsForm').val();
	experiment.newData.beliefLeader = $('input[name=beliefLeader]:checked', '#demographicsForm').val();
	experiment.newData.beliefRules = $('input[name=beliefRules]:checked', '#demographicsForm').val();

	// multiple answer checkboxes
	$('input[name=ethnicity]:checked', '#demographicsForm').each(function() {
		experiment.newData.ethnicity.push($(this).val());
	});
	$('input[name=religionChild]:checked', '#demographicsForm').each(function() {
		experiment.newData.religionChild.push($(this).val());
	});
	$('input[name=religionNow]:checked', '#demographicsForm').each(function() {
		experiment.newData.religionNow.push($(this).val());
	});

	// set up results page
	resultsSlide.calculateMeans(); 
	resultsSlide.orderCharacters(); 
	resultsSlide.showOrder(); 
	window.scrollTo(0, 0);

	showSlide("results");
});