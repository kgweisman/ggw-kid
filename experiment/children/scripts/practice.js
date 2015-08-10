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

// set up button behavior on pre-practice slides
$(".slide#pre-practice button").click(function() {
	window.scrollTo(0, 0);
	practiceSlide.next();
});

$(".slide#easy-hard button").click(function() {
	window.scrollTo(0, 0);
	experiment.next();
});

// create practice slide
var practiceSlide = {
	trials: [[practiceCharacters.icecream, practiceCharacters.pizza], 
	[practiceCharacters.grapes, practiceCharacters.strawberries]],
	predicates: ["colder", "sweeter"],
	end: function() {

		window.scrollTo(0, 0);
		showSlide("easy-hard");

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

			// if this is the second practice trial...
			if (experiment.newData.trialData.length === 1) {

				// get the left/right side of ice cream on the last trial
				// and set the strawberries to the other side
				var icecreamSide;
				switch (experiment.newData.trialData[0].leftCharacter) {
					case "icecream": 
						data.leftCharacter = currentPair[0]; 
						data.rightCharacter = currentPair[1]; 
						break;
					case "pizza": 
						data.leftCharacter = currentPair[1]; 
						data.rightCharacter = currentPair[0]; 
						break;
					default: 
						console.log("uh-oh");
				}
				
			} else {
				
				var sideBucket = [0,1];
				data.leftCharacter = currentPair[randomElementNR(sideBucket)];
				data.rightCharacter = currentPair[sideBucket];				
			
			}

			// update progress bar
			var percentComplete = (data.trialNum-1)/experiment.totalTrials * 100;
			// var percentCompleteRounded = Math.round(percentComplete);
			// $('#trial-num').text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
			$('#practice .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#practice .progress-bar').css("width", percentComplete.toString()+"%");

			// set text and images for this practice trial
			switch (data.trialNum) {
				case 1: 
					$(".slide#practice span#explanation").text("Do you know what this is? What about this? Then, I'm going to ask you a question about them. So this time, I want to know...");
					break;
				case 2:
					$(".slide#practice span#explanation").text("Here's the next one. This time, I want to know...");
					break;
				default: 
					$(".slide#practice span#explanation").text("");
			}

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