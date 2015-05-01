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

// closer to original...
addCharacter("baby", "baby", "...");
addCharacter("bear", "bear", "...");
addCharacter("bug", "bug", "...");
addCharacter("car", "car", "...");
addCharacter("computer", "computer", "...");
addCharacter("dog", "dog", "...");
addCharacter("grownup", "grown-up", "...");
addCharacter("kid", "kid", "...");
addCharacter("moon", "moon", "...");
addCharacter("mountain", "mountain", "...");
addCharacter("robot", "robot", "...");
addCharacter("stapler", "stapler", "...");
addCharacter("teddybear", "teddy bear", "...");
addCharacter("tree", "tree", "...");

// create the list of all possible pairs (78)

var pairs = []; 
function makePairs() {
	var list = Object.keys(characters).map(function (key) {return characters[key]});
	for (j = 0; j < 13; j++) {
		for (k = j+1; k < 13; k++) {
			pairs.push([list[j], list[k]]);
		}
	};
};
makePairs();

// set up how to display characters slide

var charactersSlide = {
	list: Object.keys(characters).map(function (key) {return characters[key]}),
	trials: [],
	makeOrder: function() {
		// create random order
		for (i = 0; i < 13; i++) {
			this.trials.push(randomElementNR(this.list));
		};
	},
	end: function() {
		showSlide("surveys");
	},
	next: function() {
		if (this.trials.length === 0) {
			charactersSlide.end();
		} else {
			var currentChar = this.trials.shift();

			// set text and images for this trial
			$("#characters h2#character").text(currentChar.charTitle);
			// $("#characters h2#character").text(currentChar.charTitle.split(",")[0]);
			// $("#characters .block-text#character").text(currentChar.charTitle.split(",")[1]);
			// $("#characters .block-text#character").text(currentChar.charTitle);
			$("#characters img#character").attr("src", currentChar.imageSource);
			
			// show trial
			showSlide("characters");

			// record character and rt
			var startTime = (new Date()).getTime();

			var clickHandler = function(event) {
				var endTime = (new Date()).getTime();
				var data = {
					trialNum: 13 - charactersSlide.trials.length,
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