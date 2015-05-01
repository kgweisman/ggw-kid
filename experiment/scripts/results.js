/* set up results slide */

// set up button behavior
$('.slide#results button').click(function() {
	// include "opener." because experiment has been running in new window
	opener.turk.submit(experiment);
	window.scrollTo(0, 0);
	showSlide('finished');
});

// set up results slide display
var resultsSlide = {
	list: characters,
	charMeans: {
		charlie_dog: [],
		delores_gleitman_deceased: [],
		fetus: [],
		gerald_schiff_pvs: [],
		god: [],
		green_frog: [],
		kismet_robot: [],
		nicholas_gannon_baby: [],
		samantha_hill_girl: [],
		sharon_harvey_woman: [],
		toby_chimp: [],
		todd_billingsley_man: [],
		you: []
	},
	charSorted: [],
	calculateMeans: function() {
		for (i in this.charMeans) {
			var total = 0;
			array = experiment.newData.charScores[i];
			for (j = 0; j < array.length; j++) {
				total += array[j];
			}
			var mean = total/array.length;
			this.charMeans[i] = mean;
		};
		experiment.newData.charMeans = this.charMeans;
	},
	orderCharacters: function() {
		sortedCharacters = [];
		for (i in this.charMeans) {
			sortedCharacters.push([i, this.charMeans[i]]);
		}		
		sortedCharacters = sortedCharacters.sort(function(a, b) {return a[1] - b[1]});
		this.charSorted = sortedCharacters;
	},
	showOrder: function() {
		for (i = 0; i < this.charSorted.length; i++) {
			var charNum = i+1;
			var charName = this.charSorted[i][0];
			$("#ranking-intro span").text(experiment.newData.wording);
			$("p#rank"+charNum).text(characters[charName].charTitle);
			$("img#rank"+charNum).attr("src", characters[charName].imageSource);
		}
	}
}