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
addCondition("thinking", "think");
addCondition("feelings", "have feelings");
addCondition("hunger", "get hungry");

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