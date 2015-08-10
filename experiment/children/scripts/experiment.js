/* set up how to display stage slide (experiment trials) */

// get date
var date = new Date();

// set up "current" variables
var currentHelper, currentBlockNum, currentPredicate, currentSubset, currentPermutation, allPermutations;
var done = false;

// create experiment object
var experiment = {
	// array for making each new trial
	blocks: [],
	helpers: ["", "You're being such a great helper! ", "You are really being an awesome helper. "],
	anothers: ["the first thing", "another thing", "the last thing"],
	predicates: [],
	subsets: [],
	totalTrials: NaN,

	// where to store all the data
	newData: {
		// fingerprinting information
		fingerprintData: {},

		// condition and session information
		charIntroData: [],
		condIntroOrder: [],
		sequence: surveysSlide.sequence,
		// condition: "",
		// wording: "",

		// demographic information about participant
		subid: "",
		gender: "",
		ethnicity: "",
		dateOfBirth: "",
		dateOfTest: date.getMonth()+1+"/"+date.getDate()+"/"+date.getFullYear(),
		timeOfTest: date.getHours()+":"+date.getMinutes(),
		testingSite: "",
		experimenter: "",
		trialComments: "",
		sessionComments: "",

		// trial by trial data
		trialData: [],

		// summary data for use in results slide
		charScores: {
			grownup: [],
			kid: [],
			baby: [],
			dog: [],
			bear: [],
			bug: [],
			robot: [],
			computer: [],
			car: [],
			// tree: [],
			// moon: [],
			// mountain: [],
			// teddybear: [],
			stapler: []
		},
		charMeans: {
			grownup: NaN,
			kid: NaN,
			baby: NaN,
			dog: NaN,
			bear: NaN,
			bug: NaN,
			robot: NaN,
			computer: NaN,
			car: NaN,
			// tree: NaN,
			// moon: NaN,
			// mountain: NaN,
			// teddybear: NaN,
			stapler: NaN
		}
	},

	// what happens after completing all trials
	end: function() {

		// show ending slide	
		showSlide("end");
		
		$('.slide#end button').click(function() { 
			// reload html to return to start slide
			location.reload();
		});

		// export data to csv
		var data = experiment.newData.trialData;
 
		function DownloadJSON2CSV(objArray) { // code source: http://www.zachhunter.com/2010/11/download-json-to-csv-using-javascript/
		    // get trial-level info
		    var array = typeof objArray != 'object' ? JSON.parse(objArray) : objArray;

		    // add subject-level info
		    for (trial in objArray) {
		    	objArray[trial].sequence = experiment.newData.sequence;
		    	objArray[trial].experimenter = experiment.newData.experimenter;
		    	objArray[trial].testingSite = experiment.newData.testingSite;
		    	objArray[trial].subid = experiment.newData.subid;
		    	objArray[trial].gender = experiment.newData.gender;
		    	objArray[trial].ethnicity = experiment.newData.ethnicity;
		    	objArray[trial].dateOfBirth = experiment.newData.dateOfBirth;
		    	objArray[trial].dateOfTest = experiment.newData.dateOfTest;
		    	objArray[trial].timeOfTest = experiment.newData.timeOfTest;
		    	// objArray[trial].charIntroData = experiment.newData.charIntroData;
		    	objArray[trial].trialComments = experiment.newData.trialComments;
		    	objArray[trial].sessionComments = experiment.newData.sessionComments;
		    };

		    // add headers in a hacky way
		    objArray.unshift({
		    	phase: "phase",
		    	trialNum: "trialNum",
		    	predicate: "predicate",
		    	leftCharacter: "leftCharacter",
		    	rightCharacter: "rightCharacter",
		    	response: "response",
		    	rt: "rt",
		    	sequence: "sequence",
		    	experimenter: "experimenter",
		    	testingSite: "testingSite",
		    	subid: "subid",
		    	gender: "gender",
		    	ethnicity: "ethnicity",
		    	dateOfBirth: "dateOfBirth",
		    	dateOfTest: "dateOfTest",
		    	timeOfTest: "timeOfTest",
		    	// charIntroData: "charIntroData",
		    	trialComments: "trialComments",
		    	sessionComments: "sessionComments"
		    });

		    // convert to csv
		    var str = '';
		     
		    for (var i = 0; i < array.length; i++) {
		        var line = '';
		        for (var index in array[i]) {
		            if(line != '') line += ','
		         
		            line += array[i][index];
		        }
		 
		        str += line + '\r\n';
		    }
		 
		    if (navigator.appName != 'Microsoft Internet Explorer') {
		        window.open('data:text/csv;charset=utf-8,' + escape(str));
		    } else {
		        var popup = window.open('','csv','');
		        popup.document.body.innerHTML = '<pre>' + str + '</pre>';
		    }          
		}

		DownloadJSON2CSV(data);
	},

	// what happens when participant sees a new trial
	next: function() {

		// if this is the very first test trial, or there are no more trials left in this block...
		if ((experiment.newData.trialData.length === 2 & done === false) ||
			currentPermutation.length === 0) { 

			// ...and this is the last block...
			if (experiment.predicates.length === 0) { 

				// ...end the experiment!
				experiment.end();

			// ...and this is NOT the last block...
			} else { 

				// ...start a new block!

				// get the new predicate and new pairs subset
				currentHelper = experiment.helpers.shift();
				currentBlockNum = experiment.blocks.shift();
				currentBlockAnother = experiment.anothers.shift();
				currentPredicate = experiment.predicates.shift();
				currentSubset = experiment.subsets.shift();

				// get all the possible permutations for the current subset
				allPermutations = permute(currentSubset);

				// find a permutation without overlaps
				errorLog = [1];
				var randomPermutation;

				while (errorLog.length > 0) {

					// choose a random permutation from the list
					randomPermutation = randomElementNR(allPermutations);
					console.log(randomPermutation);
					errorLog = [];

					// check whether there are overlaps
					for (i = 0; i < (randomPermutation.length-1); i++) {

					    if (randomPermutation[i+1][0].charName === randomPermutation[i][0].charName ||
					      randomPermutation[i+1][0].charName === randomPermutation[i][1].charName ||
					      randomPermutation[i+1][1].charName === randomPermutation[i][0].charName ||
					      randomPermutation[i+1][1].charName === randomPermutation[i][1].charName) {
					      errorLog.push(1);
						}
					}
				}

				// select this permutation as the currentPermutation
				currentPermutation = randomPermutation;

				// set up and display the instructions slide for this block
				$('.slide#surveys span#helper').text(currentHelper)
				$('.slide#surveys span#survey-descrip1').text(currentPredicate.condName)
				$('.slide#surveys span#survey-descrip2').text(currentPredicate.introLabel);
				$('.slide#surveys span#survey-descrip3').text(currentPredicate.introDescription);
				$('.slide#surveys span#survey-descrip4').text(currentPredicate.wording);
				$('.slide#surveys span#survey-descrip-another').text(currentBlockAnother);

				if (currentBlockNum === "first") {
					$('.slide#surveys span#ready').hide();
				} else {
					$('.slide#surveys span#ready').show();
					$('.slide#surveys span#survey-descrip-number').text(currentBlockNum);
				}

				showSlide("surveys");

				// mark this as done
				done = true;

			}

		// if there are more trials left in this block...
		} else { 

				// ...start a new trial!
				
				// create place to store data for this trial
				var data = {
					phase: "test",
					trialNum: experiment.newData.trialData.length + 1,
					predicate: currentPredicate.condName,
					leftCharacter: {},
					rightCharacter: {},
					response: "",
					rt: NaN
				};

				// update progress bar
				var percentComplete = (data.trialNum-1)/experiment.totalTrials * 100;
				// var percentCompleteRounded = Math.round(percentComplete);
				// $('#trial-num').text("trial "+data.trialNum.toString()+" of 78: "+percentCompleteRounded+"% complete");
				$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
				$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

				// set the left and right characters (random side assignment)
				var currentPair = currentPermutation.shift();
				data.leftCharacter = randomElementNR(currentPair);
				data.rightCharacter = currentPair[0];

				// set text and images for this trial
				$(".slide#stage #question").text(currentPredicate.wording);
				$(".slide#stage #options").text("The "+data.leftCharacter.charTitle+", the "+data.rightCharacter.charTitle);
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
					if (data.response === "bail") {
						experiment.end();
					} else {
						clickHandler();
						$(".slide#stage button").unbind().blur();
						window.scrollTo(0, 0);
						experiment.next();						
					}
					
				});

			}
		}
	}