/* set up list of characters with titles, descriptions, and image sources */ 

function addCharacter(charName, charTitle, charDescrip) {
	function Character(charName, charTitle, charDescrip) {
		this.charName = charName;
		this.charTitle = charTitle;
		this.imageSource = "images_characters/"+charName+".png";
		this.charDescrip = charDescrip;
	};
	newCharacter = new Character(charName, charTitle, charDescrip);
	characters[newCharacter.charName] = newCharacter;
};

characters = {};

// closer to original...
addCharacter("charlie_dog", "Charlie, family dog", "Charlie is a 3-year-old Springer spaniel and a beloved member of the Graham family.");
addCharacter("delores_gleitman_deceased", "Delores Gleitman, recently deceased", "Delores Gleitman recently passed away at the age of 65. As you complete the survey, please draw upon your own personal beliefs about people who have passed away.");
addCharacter("fetus", "7-week human fetus", "At 7 weeks, a human fetus is almost half an inch long - roughly the size of a raspberry.");
addCharacter("gerald_schiff_pvs", "Gerald Schiff, in a persistent vegetative state (similar to a coma)", "Gerald Schiff has been in a persistent vegetative state (PVS) for the past six months. Although he has severe brain damage - Gerald does not appear to communicate with others or make purposeful movements - his basic bodily functions (such as breathing, sleeping, and circulation) are preserved.");
addCharacter("god", "God", "Many people believe that God is the creator of the universe and the ultimate source of knowledge, power, and love. However, please draw upon your own personal beliefs about God.");
addCharacter("green_frog", "Green frog", "The Green Frog can be found throughout eastern North America. This classic 'pond frog' is medium-sized and green or bronze in color. Daily life includes seeking out permanent ponds or slow streams with plenty of vegetation.");
addCharacter("kismet_robot", "Kismet, sociable robot", "Kismet is part of a new class of 'sociable' robots that can engage people in natural interaction. To do this, Kismet perceives a variety of natural social signals from sound and sight, and delivers his own signals back to the human partner through gaze direction, facial expression, body posture, and vocal babbles.");
addCharacter("nicholas_gannon_baby", "Nicholas Gannon, 5-month-old", "Nicholas is a five-month-old baby.");
addCharacter("samantha_hill_girl", "Samantha Hill, 5-year-old", "Samantha is a five-year-old girl who lives with her parents and older sister Jennifer.");
addCharacter("sharon_harvey_woman", "Sharon Harvey, advertising executive", "Sharon Harvey, 38, works at an advertising agency in Chicago.");
addCharacter("toby_chimp", "Wild chimpanzee", "Toby is a two-year-old wild chimpanzee living at an outdoor laboratory in Uganda.");
addCharacter("todd_billingsley_man", "Todd Billingsley, accountant", "Todd Billingsley is a thirty-year-old accountant who lives in New York City.");
addCharacter("you", "You", "When you see the mirror, please consider how you, yourself, would compare with the other choice presented.");

// more minimal...
// addCharacter("charlie_dog", "Family dog", "Charlie is a 3-year-old Springer spaniel and a beloved member of the Graham family.");
// addCharacter("delores_gleitman_deceased", "Recently deceased woman", "Delores Gleitman recently passed away at the age of 65. As you complete the survey, please draw upon your own personal beliefs about people who have passed away.");
// addCharacter("fetus", "7-week human fetus", "At 7 weeks, a human fetus is almost half an inch long - roughly the size of a raspberry.");
// addCharacter("gerald_schiff_pvs", "Man in a persistent vegetative state (similar to a coma)", "Gerald Schiff has been in a persistent vegetative state (PVS) for the past six months. Although he has severe brain damage - Gerald does not appear to communicate with others or make purposeful movements - his basic bodily functions (such as breathing, sleeping, and circulation) are preserved.");
// addCharacter("god", "God", "Many people believe that God is the creator of the universe and the ultimate source of knowledge, power, and love. However, please draw upon your own personal beliefs about God.");
// addCharacter("green_frog", "Frog", "The Green Frog can be found throughout eastern North America. This classic 'pond frog' is medium-sized and green or bronze in color. Daily life includes seeking out permanent ponds or slow streams with plenty of vegetation.");
// addCharacter("kismet_robot", "Robot", "Kismet is part of a new class of 'sociable' robots that can engage people in natural interaction. To do this, Kismet perceives a variety of natural social signals from sound and sight, and delivers his own signals back to the human partner through gaze direction, facial expression, body posture, and vocal babbles.");
// addCharacter("nicholas_gannon_baby", "5-month-old baby", "Nicholas is a five-month-old baby.");
// addCharacter("samantha_hill_girl", "5-year-old child", "Samantha is a five-year-old girl who lives with her parents and older sister Jennifer.");
// addCharacter("sharon_harvey_woman", "Adult woman", "Sharon Harvey, 38, works at an advertising agency in Chicago.");
// addCharacter("toby_chimp", "Wild chimpanzee", "Toby is a two-year-old wild chimpanzee living at an outdoor laboratory in Uganda.");
// addCharacter("todd_billingsley_man", "Adult man", "Todd Billingsley is a thirty-year-old accountant who lives in New York City.");
// addCharacter("you", "You", "When you see the mirror, please consider how you, yourself, would compare with the other choice presented.");

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