/*
Depends on jQuery, log.debug, log.error 
TODO: Rewrite to be more testable
*/
function waitScreen(log, $){
    /* Private Variables */
    var Me = this; 
    	
	var oProgressBar = $("<div>").attr("id", "progressBar");
	
	var htmlDialog = $("<div>")
						.attr("id", "dialog-progress")
						.attr("title", "Loading...")
						.append($("<p>")
							.append("Please Wait...")
							.append($("<img>")
								.attr("style", "float: right;")
								.attr("src", "/images/ajax-loader.gif")
								.attr("alt", "Page Loading...")
								.attr("height", "16px")
								.attr("width", "16px")
							)
						)
						.append(oProgressBar);
					
    var percentComplete = 0.0;
    var totalNumberComplete = 0.0;
    var sCloseMessage = "All groups and items have been loaded.";
    
	/* Public Variables */
	this.bComplete = false; 
    this.nTotalNumber = 0.0;
	
	/* Public Methods */
    this.setCloseMessage = function(sMessage) {
        sCloseMessage = sMessage;
    };

    this.close = function() {
        Me.bComplete = true;
        htmlDialog.dialog("close");
        htmlDialog.remove();
        log.debug(sCloseMessage);
    };

    this.load = function(sTitle, nTotalNumber) {
        if (sTitle) {
            htmlDialog.attr("title", sTitle);
        }

        if (nTotalNumber) {
            oProgressBar.show();
			Me.nTotalNumber = nTotalNumber;
        } else {
            oProgressBar.hide();
        } 

        oProgressBar.progressbar({
            value: 0,
            complete: function(event, ui) {
                Me.Close();
            }
        });

        htmlDialog.dialog({
            resizable: false,
            modal: true
        });
    };

    this.itemComplete = function() {
        this.updateItems(totalNumberComplete + 1);
    };

    this.updateItems = function(nGroupNum) {

        if (nGroupNum > totalNumberComplete) {
            //Update percentages and group number here
            totalNumberComplete = nGroupNum;

            if (Me.nTotalNumber > 0) {
                percentComplete = (100 * totalNumberComplete) / Me.nTotalNumber;
            }
            else {
                log.error("The total number of items to process was not set for the splash screen.");
            }
 
            this.updatePercent(percentComplete);
        } else {
			log.error("The # of items given for the progress update is greater than the total number available.");
		}
    };

    this.updatePercent = function(nPercentComplete) {

        var nOldValue = oProgressBar.progressbar("option", "value");

        if (nPercentComplete > nOldValue) {
            oProgressBar.progressbar("option", "value", nPercentComplete);
        }
    };
};

return { 
	deps: [ "/widgets/logger.js", "/jquery/jquery-mod.js" ],
	callback: function(log, $) {
		return new waitScreen(log, $);
	}
};