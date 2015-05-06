// make practice "characters"

function addPracticeCharacter(charName, charTitle) {
	function PracticeCharacter(charName, charTitle) {
		this.charName = charName;
		this.charTitle = charTitle;
		this.imageSource = "images_characters/practice_"+charName+".jpg";
	};
	newPracticeCharacter = new PracticeCharacter(charName, charTitle);
	practiceCharacters[newPracticeCharacter.charName] = newPracticeCharacter;
};

practiceCharacters = {};

// make characters
addPracticeCharacter("icecream", "ice cream");
addPracticeCharacter("pizza", "pizza");
addPracticeCharacter("grapes", "grapes");
addPracticeCharacter("strawberries", "strawberries");

var practiceSlide = {
	trials: [[practiceCharacters.icecream, practiceCharacters.pizza], 
	[practiceCharacters.grapes, practiceCharacters.strawberries]],
	predicates: ["colder", "sweeter"],
	end: function() {

		experiment.next();

	},
	next: function() {

		if(this.trials.length === 0) {

			practiceSlide.end();

		} else {

			// set pair and predicate for this practice trial
			var currentPair = this.trials.shift();
			var currentPredicate = this.predicates.shift();

			// create place to store data for this trial
			var data = {
				phase: "practice",
				trialNum: 2 - practiceSlide.trials.length,
				predicate: currentPredicate,
				leftCharacter: {},
				rightCharacter: {},
				response: "",
				rt: NaN
			};

			// choose left/right sides for characters
			var sideBucket = [0,1];
			data.leftCharacter = currentPair[randomElementNR(sideBucket)];
			data.rightCharacter = currentPair[sideBucket];

			// display progress bar
			var percentComplete = (data.trialNum-1)/2 * 100;
			var percentCompleteRounded = Math.round(percentComplete);
			// $('#trial-num').text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

			// set text and images for this practice trial
			$(".slide#practice span#question").text(currentPredicate); 
			$(".slide#practice #options").text(data.leftCharacter.charTitle+", "+data.rightCharacter.charTitle);
			$("#practice #image-left").attr("src", data.leftCharacter.imageSource);
			$("#practice #image-right").attr("src", data.rightCharacter.imageSource);

			// show practice trial
			showSlide("practice");

			// record response and rt
			var startTime = (new Date()).getTime();

			var clickHandler = function(event) {
				var endTime = (new Date()).getTime();
				data.rt = endTime - startTime;
				experiment.newData.trialData.push(data);
			};

			$(".slide#practice button").click(function() {

				// record response
				data.response = $(this).attr('id');

				// store only character names instead of character objects
				data.leftCharacter = data.leftCharacter.charName;
				data.rightCharacter = data.rightCharacter.charName;

				// end trial
				clickHandler();
				$(".slide#practice button").unbind().blur();
				window.scrollTo(0, 0);
				practiceSlide.next();

			});

		}
	}
}