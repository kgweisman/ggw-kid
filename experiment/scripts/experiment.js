/* set up how to display stage slide (experiment trials) */

var experiment = {
	// array for making each new trial
	trials: pairs,

	// where to store all the data
	newData: {
		// fingerprinting information
		fingerprintData: {},

		// condition and session information
		charIntroData: [],
		condIntroOrder: [],
		condition: "",
		wording: "",

		// demographic information about participant
		age: "",
		gender: "",
		job: "",
		education: "",
		ethnicity: [],
		religionChild: [],
		religionNow: [],
		country: "",
		englishNative: "",
		maritalStatus: "",
		children: "",
		vegetarian: "",
		dog: "",
		studyMoralPhil: "",
		politicalIdeology: "",
		beliefGod: "",
		beliefTradition: "",
		beliefAfterlife: "",
		beliefLeader: "",
		beliefRules: "",
		comments: "",

		// trial by trial data
		trialData: [],

		// summary data for use in results slide
		charScores: {
			grownup: [],
			baby: [],
			bear: [],
			bug: [],
			robot: [],
			computer: [],
			tree: [],
			mountain: [],
			teddybear: [],
			stapler: []
		},
		charMeans: {
			grownup: NaN,
			baby: NaN,
			bear: NaN,
			bug: NaN,
			robot: NaN,
			computer: NaN,
			tree: NaN,
			mountain: NaN,
			teddybear: NaN,
			stapler: NaN
		}
	},

	// what to do when the participant has seen all trials
	end: function() {
		showSlide("demographics");
	},

	// what happens when participant sees a new trial
	next: function() {
		if (this.trials.length === 0) {
			experiment.end();
		} else {
			// create place to store data for this trial
			var data = {
				trialNum: 46 - this.trials.length,
				leftCharacter: {},
				rightCharacter: {},
				response: "",
				rt: NaN
			};

			// assign left and right characters
			var chosenPair;
			if (experiment.newData.trialData.length < 1) {
				chosenPair = randomElementNR(experiment.trials);
			} else {
				// log previous trial's pair
				var lastLeft = experiment.newData.trialData[experiment.newData.trialData.length - 1].leftCharacter;
				var lastRight = experiment.newData.trialData[experiment.newData.trialData.length - 1].rightCharacter;

				// choose randomly from remaining pairs
				var randomPair = pairs[randomInteger(experiment.trials.length)];
				console.log(randomPair);

				// continue to choose randomly until there are no repeats from last trial
				while (randomPair[0].charName === lastLeft ||
					randomPair[0].charName === lastRight ||
					randomPair[1].charName === lastLeft ||
					randomPair[1].charName === lastRight) {
					randomPair = pairs[randomInteger(experiment.trials.length)];
				} 

				// select this pair for the trial
				chosenPair = randomPair;

				// remove chosen pair from the full pair set for the experiment
				var tempIndex = pairs.indexOf(chosenPair);
				if (tempIndex > -1) {
					pairs.splice(tempIndex, 1);
				}
			}

			// var chosenPair = randomElementNR(this.trials);
			var sideBucket = [0,1];

			data.leftCharacter = chosenPair[randomElementNR(sideBucket)];
			data.rightCharacter = chosenPair[sideBucket]

			// display progress bar
			var percentComplete = (data.trialNum-1)/46 * 100;
			var percentCompleteRounded = Math.round(percentComplete);
			// $('#trial-num').text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

			// set text and images for this trial
			$(".slide#stage #question").text("Which one is more likely to "+this.newData.wording+"?");
			$(".slide#stage #options").text("The "+data.leftCharacter.charTitle+", the "+data.rightCharacter.charTitle+", or are they both the same?");
			$("#stage #image-left").attr("src", data.leftCharacter.imageSource);
			$("#stage #image-right").attr("src", data.rightCharacter.imageSource);
			
			// show trial
			showSlide("stage");

			// record response and rt
			var startTime = (new Date()).getTime();

			var clickHandler = function(event) {
				var endTime = (new Date()).getTime();
				data.rt = endTime - startTime;
				experiment.newData.trialData.push(data);
			};

			$(".slide#stage button").click(function() { 
				// record response
				data.response = $(this).attr('id');

				// recode response as number
				switch (data.response) { 
					case "much more left":
						characterMore = data.leftCharacter.charName;
						characterLess = data.rightCharacter.charName;
						experiment.newData.charScores[characterMore].push(2);
						experiment.newData.charScores[characterLess].push(-2);
						break;
					case "slightly more left":
						characterMore = data.leftCharacter.charName;
						characterLess = data.rightCharacter.charName;
						experiment.newData.charScores[characterMore].push(1);
						experiment.newData.charScores[characterLess].push(-1);
						break;
					case "both equally":
						experiment.newData.charScores[data.leftCharacter.charName].push(0);
						experiment.newData.charScores[data.rightCharacter.charName].push(0);
						break;
					case "slightly more right":
						characterMore = data.rightCharacter.charName;
						characterLess = data.leftCharacter.charName;
						experiment.newData.charScores[characterMore].push(1);
						experiment.newData.charScores[characterLess].push(-1);
						break;
					case "much more right":
						characterMore = data.rightCharacter.charName;
						characterLess = data.leftCharacter.charName;
						experiment.newData.charScores[characterMore].push(2);
						experiment.newData.charScores[characterLess].push(-2);
						break;
					default: 
						console.log("whoops");
				}

				// store only character names instead of character objects
				data.leftCharacter = data.leftCharacter.charName;
				data.rightCharacter = data.rightCharacter.charName;

				// end trial
				clickHandler();
				$(".slide#stage button").unbind().blur();
				window.scrollTo(0, 0);
				experiment.next();
			})
		}
	}
}



