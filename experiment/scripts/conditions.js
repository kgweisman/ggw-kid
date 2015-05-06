/* set up list of conditions for blocks */ 

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
addCondition("thinking", "thinking", "have thoughts or ideas", "think");
addCondition("feelings", "feelings", "feel happy, or sad, or scared, or mad", "have feelings");
addCondition("hunger", "getting hungry", "really need to eat some food", "get hungry");


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
addSequence("sequence1", [conditions.thinking, conditions.feelings, conditions.hunger], 
	[pairsSubsetA, pairsSubsetE, pairsSubsetB]);
addSequence("sequence2", [conditions.thinking, conditions.hunger, conditions.feelings], 
	[pairsSubsetB, pairsSubsetC, pairsSubsetA]);
addSequence("sequence3", [conditions.feelings, conditions.thinking, conditions.hunger], 
	[pairsSubsetC, pairsSubsetF, pairsSubsetD]);
addSequence("sequence4", [conditions.feelings, conditions.hunger, conditions.thinking], 
	[pairsSubsetD, pairsSubsetA, pairsSubsetC]);
addSequence("sequence5", [conditions.hunger, conditions.thinking, conditions.feelings], 
	[pairsSubsetE, pairsSubsetD, pairsSubsetF]);
addSequence("sequence6", [conditions.hunger, conditions.feelings, conditions.thinking], 
	[pairsSubsetF, pairsSubsetB, pairsSubsetE]);

// set up how to display surveys slide

var surveysSlide = {
	// condList: Object.keys(conditions).map(function (key) {return conditions[key]}),
	seqList: Object.keys(sequences).map(function (key) {return sequences[key]}),
	order: [],
	// condition: ""
}

surveysSlide.sequence = randomElementNR(surveysSlide.seqList);
// surveysSlide.condition = randomElementNR(surveysSlide.condList);

// experiment.newData.sequence = surveysSlide.sequence.seqName;
// experiment.predicates = surveysSlide.sequence.predicateOrder;
// experiment.subsets = surveysSlide.sequence.subsetOrder;

// set up button behaviors

$('.slide#surveys button').click(function() {
	experiment.next();
	window.scrollTo(0, 0);
});