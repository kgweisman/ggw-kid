/* set up list of sequences with blocks */ 

function addSequence(seqName, predicateOrder, subsetOrder) {
	function Sequence(seqName, predicateOrder, subsetOrder) {
		this.seqName = seqName;
		this.predicateOrder = predicateOrder;
		this.subsetOrder = subsetOrder;
	};
	var newSequence = new Sequence(seqName, predicateOrder, subsetOrder);
	sequences[seqName] = newSequence;
}

sequences = {};
addSequence("sequence1", ["thinking", "feelings", "hunger"], 
	["pairsSubsetA", "pairsSubsetE", "pairsSubsetB"]);
addSequence("sequence2", ["thinking", "hunger", "feelings"], 
	["pairsSubsetB", "pairsSubsetC", "pairsSubsetA"]);
addSequence("sequence3", ["feelings", "thinking", "hunger"], 
	["pairsSubsetC", "pairsSubsetF", "pairsSubsetD"]);
addSequence("sequence4", ["feelings", "hunger", "thinking"], 
	["pairsSubsetD", "pairsSubsetA", "pairsSubsetC"]);
addSequence("sequence5", ["hunger", "thinking", "feelings"], 
	["pairsSubsetE", "pairsSubsetD", "pairsSubsetF"]);
addSequence("sequence6", ["hunger", "feelings", "thinking"], 
	["pairsSubsetF", "pairsSubsetB", "pairsSubsetE"]);

// set up list of conditions for blocks

function addCondition(condName, introLabel, introDescription, wording) {
	function Condition(condName, introLabel, introDescription, wording) {
		this.condName = condName;
		this.introLabel = introLabel;
		this.introDescription = introDescription;
		this.wording = wording;
	};
	var newCondition = new Condition(condName, introLabel, introDescription, wording);
	conditions[condName] = newCondition;
};

conditions = {};
addCondition("thinking", "thinking", "has thoughts or ideas", "think");
addCondition("feelings", "having feelings", "feels happy, or sad, or scared, or mad", "have feelings");
addCondition("hunger", "getting hungry", "really needs to eat some food", "get hungry");

// set up button behaviors for surveys slide

$('.slide#surveys button').click(function() { // select condition
	experiment.newData.sequence = surveysSlide.sequence.seqName;
	experiment.newData.condition = surveysSlide.condition.condName;
	experiment.newData.wording = surveysSlide.condition.wording;
	experiment.next();
	window.scrollTo(0, 0);
});

// set up how to display surveys slide

var surveysSlide = {
	condList: Object.keys(conditions).map(function (key) {return conditions[key]}),
	seqList: Object.keys(sequences).map(function (key) {return sequences[key]}),
	order: [],
	condition: ""
}

surveysSlide.sequence = randomElementNR(surveysSlide.seqList);
surveysSlide.condition = randomElementNR(surveysSlide.condList);

$('.slide#surveys span#survey-descrip1').text(surveysSlide.condition.condName)
$('.slide#surveys span#survey-descrip2').text(surveysSlide.condition.introLabel);
$('.slide#surveys span#survey-descrip3').text(surveysSlide.condition.introDescription);
$('.slide#surveys span#survey-descrip4').text(surveysSlide.condition.wording);