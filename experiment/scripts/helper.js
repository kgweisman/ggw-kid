/* set up helper functions */

// define function to show a slide
function showSlide(id) { 
	// console.log(id); // show which slide is being presented
	$(".slide").hide(); // hide all slides
	$("#"+id).show(); // show selected slide
};

// define function to get a random integer < n
function randomInteger(n) { 
	return Math.floor(Math.random()*n); 
}

// define function for random selection without replacement
function randomElementNR(bucket) { 
	var randomIndex = randomInteger(bucket.length);
	return bucket.splice(randomIndex, 1)[0];
}