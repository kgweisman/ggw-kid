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

// set up permutation function
function permute(input) {
    var permArr = [],
        usedChars = [];
    return (function main() {
        for (var i = 0; i < input.length; i++) {
            var ch = input.splice(i, 1)[0];
            usedChars.push(ch);
            if (input.length == 0) {
                permArr.push(usedChars.slice());
            }
            main();
            input.splice(i, 0, ch);
            usedChars.pop();
        }
        return permArr;
    })();
}