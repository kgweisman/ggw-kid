/* set up list of characters with titles, descriptions, and image sources */ 

function addCharacter(charName, charTitle) {
	function Character(charName, charTitle) {
		this.charName = charName;
		this.charTitle = charTitle;
		this.imageSource = "images_characters/"+charName+".jpg";
	};
	newCharacter = new Character(charName, charTitle);
	characters[newCharacter.charName] = newCharacter;
};

characters = {};

// make characters
addCharacter("grownup", "grown-up");
addCharacter("kid", "kid");
addCharacter("baby", "baby");
addCharacter("bear", "bear");
addCharacter("dog", "dog");
addCharacter("bug", "bug");
addCharacter("robot", "robot");
addCharacter("car", "car");
addCharacter("computer", "computer");
// addCharacter("tree", "tree");
// addCharacter("moon", "moon");
// addCharacter("mountain", "mountain");
// addCharacter("teddybear", "teddy bear");
addCharacter("stapler", "stapler");

// create the list of all possible pairs (45)

// var pairs = []; 
// function makePairs() {
// 	var list = Object.keys(characters).map(function (key) {return characters[key]});
// 	for (j = 0; j < 10; j++) {
// 		for (k = j+1; k < 10; k++) {
// 			pairs.push([list[j], list[k]]);
// 		}
// 	};
// };
// makePairs();

// make counterbalanced subsets of pairs

var pairsSubsetA = [[characters.grownup, characters.dog], 
					[characters.kid, characters.baby], 
					[characters.kid, characters.bug], 
					[characters.baby, characters.car], 
					[characters.dog, characters.stapler], 
					[characters.bear, characters.computer], 
					[characters.bug, characters.robot], 
					[characters.robot, characters.stapler]];

var pairsSubsetB = [[characters.grownup, characters.bear], 
					[characters.kid, characters.robot], 
					[characters.baby, characters.bug], 
					[characters.dog, characters.car], 
					[characters.bear, characters.stapler], 
					[characters.robot, characters.computer], 
					[characters.car, characters.stapler]];

var pairsSubsetC = [[characters.grownup, characters.bug], 
					[characters.grownup, characters.stapler], 
					[characters.kid, characters.computer], 
					[characters.baby, characters.robot], 
					[characters.dog, characters.bear], 
					[characters.bug, characters.car], 
					[characters.robot, characters.car], 
					[characters.computer, characters.stapler]];

var pairsSubsetD = [[characters.grownup, characters.robot], 
					[characters.kid, characters.dog], 
					[characters.kid, characters.stapler], 
					[characters.baby, characters.bear], 
					[characters.dog, characters.bug], 
					[characters.bear, characters.car], 
					[characters.bug, characters.computer], 
					[characters.computer, characters.car]];

var pairsSubsetE = [[characters.grownup, characters.kid], 
					[characters.grownup, characters.computer], 
					[characters.kid, characters.car], 
					[characters.baby, characters.dog], 
					[characters.baby, characters.stapler], 
					[characters.dog, characters.robot], 
					[characters.bear, characters.bug]];

var pairsSubsetF = [[characters.grownup, characters.baby], 
					[characters.grownup, characters.car], 
					[characters.kid, characters.bear], 
					[characters.baby, characters.computer], 
					[characters.dog, characters.computer], 
					[characters.bear, characters.robot], 
					[characters.bug, characters.stapler]];

// set up how to display characters slide

var charactersSlide = {
	list: Object.keys(characters).map(function (key) {return characters[key]}),
	trials: [],
	makeOrder: function() {

		// create random order
		for (i = 0; i < 10; i++) {
			charactersSlide.trials.push(randomElementNR(charactersSlide.list));
		};

	},
	end: function() {

		showSlide("instructions2");

	},
	next: function() {

		if (charactersSlide.trials.length === 0) {

			charactersSlide.end();

		} else {

			var currentChar = charactersSlide.trials.shift();

			// set text and images for this trial
			$("#characters h2#character").text(currentChar.charTitle);
			$("#characters img#character").attr("src", currentChar.imageSource);
			
			// show trial
			showSlide("characters");

			// record character and rt
			var startTime = (new Date()).getTime();

			var clickHandler = function(event) {
				var endTime = (new Date()).getTime();
				var data = {
					trialNum: 10 - charactersSlide.trials.length,
					character: currentChar.charName,
					rt: NaN
				}
				data.rt = endTime - startTime;
				experiment.newData.charIntroData.push(data);
			};

			$(".slide#characters button").click(function() {
				clickHandler();
				$(".slide#characters button").unbind().blur();
				window.scrollTo(0, 0);
				charactersSlide.next();
			})
		}
	}
}