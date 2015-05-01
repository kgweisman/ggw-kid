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
		charMeans: {
			charlie_dog: NaN,
			delores_gleitman_deceased: NaN,
			fetus: NaN,
			gerald_schiff_pvs: NaN,
			god: NaN,
			green_frog: NaN,
			kismet_robot: NaN,
			nicholas_gannon_baby: NaN,
			samantha_hill_girl: NaN,
			sharon_harvey_woman: NaN,
			toby_chimp: NaN,
			todd_billingsley_man: NaN,
			you: NaN
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
				trialNum: 79 - this.trials.length,
				leftCharacter: {},
				rightCharacter: {},
				response: "",
				rt: NaN
			};

			// assign left and right characters
			var pair = randomElementNR(this.trials);
			var sideBucket = [0,1];
			data.leftCharacter = pair[randomElementNR(sideBucket)];
			data.rightCharacter = pair[sideBucket]

			// display progress bar
			var percentComplete = (data.trialNum-1)/79 * 100;
			var percentCompleteRounded = Math.round(percentComplete);
			$('#trial-num').text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

			// set text and images for this trial
			$(".slide#stage #question").text("Which character do you think is more capable of "+this.newData.wording+"?");
			$("#stage #image-left").attr("src", data.leftCharacter.imageSource);
			$("#stage #image-right").attr("src", data.rightCharacter.imageSource);
			$("#stage #text-left").text(data.leftCharacter.charTitle);
			$("#stage #text-right").text(data.rightCharacter.charTitle);
			
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



