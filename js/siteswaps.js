function updatepub(){
	$(".sw").show();
	var array = ['h2', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'ha', 'dr']
	for (var i in array) {
	    sws=$("."+array[i])
	    if ($("#"+array[i]).prop("checked"))
		    sws.hide();
	    sws=$(".sw:not(."+array[i]+")")
	    if ($("#r"+array[i]).prop("checked"))
		    sws.hide();
	}

};

$(document).ready(function(){
    $("#pubfilter input").prop("checked", false)
    $("#pubfilter").show()
    $("#pubfilter input").change(updatepub)
    $("#pubfilter select").change(updatepub)

});
