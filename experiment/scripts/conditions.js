/* set up list of sequences with blocks */ 

function addSequence(seqNum, predicateOrder, subsetOrder) {
	function Sequence(seqNum, predicateOrder, subsetOrder) {
		this.sequence = seqNum;
		this.predicateOrder = predicateOrder;
		this.subsetOrder = subsetOrder;
	};
	var newSequence = new Sequence(seqNum, predicateOrder, subsetOrder);
	sequences[seqNum] = newSequence;
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

function addCondition(condName, wording) {
	function Condition(condName, wording) {
		this.condName = condName;
		this.wording = wording;
	};
	var newCondition = new Condition(condName, wording);
	conditions[condName] = newCondition;
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
	condList: Object.keys(conditions).map(function (key) {return conditions[key]}),
	seqList: Object.keys(sequences).map(function (key) {return sequences[key]}),
	order: [],
	condition: ""
}

surveysSlide.sequence = randomElementNR(surveysSlide.seqList);

surveysSlide.condition = randomElementNR(surveysSlide.condList);

$('.slide#surveys span#survey-descrip1').text(surveysSlide.condition.condName)
$('.slide#surveys span#survey-descrip2').text(surveysSlide.condition.wording);