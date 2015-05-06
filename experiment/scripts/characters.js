/* set up list of characters with titles, descriptions, and image sources */ 

function addCharacter(charName, charTitle, charDescrip) {
	function Character(charName, charTitle, charDescrip) {
		this.charName = charName;
		this.charTitle = charTitle;
		this.imageSource = "images_characters/"+charName+".jpg";
		this.charDescrip = charDescrip;
	};
	newCharacter = new Character(charName, charTitle, charDescrip);
	characters[newCharacter.charName] = newCharacter;
};

characters = {};

// make characters
addCharacter("grownup", "grown-up", "...");
// addCharacter("kid", "kid", "...");
addCharacter("baby", "baby", "...");
addCharacter("bear", "bear", "...");
// addCharacter("dog", "dog", "...");
addCharacter("bug", "bug", "...");
addCharacter("robot", "robot", "...");
// addCharacter("car", "car", "...");
addCharacter("computer", "computer", "...");
addCharacter("tree", "tree", "...");
// addCharacter("moon", "moon", "...");
addCharacter("mountain", "mountain", "...");
addCharacter("teddybear", "teddy bear", "...");
addCharacter("stapler", "stapler", "...");

// create the list of all possible pairs (45)

var pairs = []; 
function makePairs() {
	var list = Object.keys(characters).map(function (key) {return characters[key]});
	for (j = 0; j < 10; j++) {
		for (k = j+1; k < 10; k++) {
			pairs.push([list[j], list[k]]);
		}
	};
};
makePairs();

var pairsSubsetA = [[characters.baby, characters.computer], 
					[characters.bear, characters.stapler], 
					[characters.bug, characters.computer], 
					[characters.bug, characters.tree], 
					[characters.grownup, characters.bear], 
					[characters.grownup, characters.teddybear], 
					[characters.robot, characters.mountain], 
					[characters.tree, characters.teddybear]]; 

var pairsSubsetB = [[characters.baby, characters.robot], 
					[characters.bear, characters.teddybear], 
					[characters.bear, characters.tree], 
					[characters.computer, characters.mountain], 
					[characters.grownup, characters.bug], 
					[characters.grownup, characters.stapler], 
					[characters.robot, characters.stapler]]; 

var pairsSubsetC = [[characters.baby, characters.bug], 
					[characters.baby, characters.stapler], 
					[characters.bear, characters.bug], 
					[characters.bear, characters.mountain], 
					[characters.grownup, characters.tree], 
					[characters.mountain, characters.teddybear], 
					[characters.robot, characters.computer], 
					[characters.robot, characters.teddybear]]; 

var pairsSubsetD = [[characters.baby, characters.bear], 
					[characters.baby, characters.teddybear], 
					[characters.bug, characters.robot], 
					[characters.bug, characters.stapler], 
					[characters.computer, characters.tree], 
					[characters.grownup, characters.mountain], 
					[characters.mountain, characters.stapler]]; 

var pairsSubsetE = [[characters.baby, characters.mountain], 
					[characters.bear, characters.computer], 
					[characters.bug, characters.teddybear], 
					[characters.computer, characters.teddybear], 
					[characters.grownup, characters.baby], 
					[characters.grownup, characters.robot], 
					[characters.tree, characters.mountain], 
					[characters.tree, characters.stapler]]; 

var pairsSubsetF = [[characters.baby, characters.tree], 
					[characters.bear, characters.robot], 
					[characters.bug, characters.mountain], 
					[characters.computer, characters.stapler], 
					[characters.grownup, characters.computer], 
					[characters.robot, characters.tree], 
					[characters.teddybear, characters.stapler]]; 

// set up how to display characters slide

var charactersSlide = {
	list: Object.keys(characters).map(function (key) {return characters[key]}),
	trials: [],
	makeOrder: function() {

		// create random order
		for (i = 0; i < 10; i++) {
			this.trials.push(randomElementNR(this.list));
		};

	},
	end: function() {

		experiment.next();

	},
	next: function() {

		if (this.trials.length === 0) {

			charactersSlide.end();

		} else {

			var currentChar = this.trials.shift();

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