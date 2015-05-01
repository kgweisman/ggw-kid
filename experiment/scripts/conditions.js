/* set up list of conditions with wordings */ 

function addCondition(condName, wording) {
	function Condition(condName, wording) {
		this.condName = condName;
		this.wording = wording;
	};
	newCondition = new Condition(condName, wording);
	conditions[newCondition.condName] = newCondition;
};

conditions = {};
// addCondition("Communication", "conveying thoughts or feelings to others");
// addCondition("Consciousness", "having experiences and being aware of things");
addCondition("Desire", "longing or hoping for things");
// addCondition("Embarrassment", "experiencing embarrassment");
// addCondition("Emotion Recognition", "understanding how others are feeling");
// addCondition("Fear", "feeling afraid or fearful");
// addCondition("Hunger", "feeling hungry");
addCondition("Joy", "experiencing joy");
// addCondition("Memory", "remembering things");
// addCondition("Morality", "telling right from wrong and trying to do the right thing");
// addCondition("Pain", "experiencing physical or emotional pain");
// addCondition("Personality", "having personality traits that make it unique from others");
// addCondition("Planning", "making plans and working toward a goal");
// addCondition("Pleasure", "experiencing physical or emotional pleasure");
// addCondition("Pride", "experiencing pride");
// addCondition("Rage", "experiencing violent or uncontrolled anger");
// addCondition("Self-Control", "exercising self-restraint over desires, emotions, or impulses");
// addCondition("Thought", "thinking");

// set up button behaviors for surveys slide

$('.slide#surveys button').click(function() { // select condition
	experiment.newData.condition = surveysSlide.condition.condName;
	experiment.newData.wording = surveysSlide.condition.wording;
	experiment.next();
	window.scrollTo(0, 0);
});

// set up how to display surveys slide

var surveysSlide = {
	list: Object.keys(conditions).map(function (key) {return conditions[key]}),
	order: [],
	condition: ""
}

surveysSlide.condition = randomElementNR(surveysSlide.list);

$('.slide#surveys span#survey-descrip1').text(surveysSlide.condition.condName)
$('.slide#surveys span#survey-descrip2').text(surveysSlide.condition.wording);