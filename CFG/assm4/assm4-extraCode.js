////////////Code to add locations then display events on click
/*
// Loads the locations into the table with an on click event to display the relevent events
function fillEventsTable()
{
	// Get the (existing) table
	var table = document.getElementById("eventsTable");
	
	// Clear any existing contents
	while (table.childNodes.length > 0) {
		table.removeChild(table.firstChild);
	}

	for (var i = 0; i< locations.length; i++) {
		// New row
		var tr = document.createElement("tr");

		// Right hand cell, containing item details
		var td2 = document.createElement("td");
		
		// Title
		var divTitle = document.createElement("div");
		divTitle.setAttribute("location", locations[i].innerHTML);
		divTitle.innerHTML = locations[i].innerHTML;
		td2.appendChild(divTitle);

		tr.appendChild(td2);
		table.appendChild(tr);

		addContent(locations[i].innerHTML);

		//set on click events for display data
		tr.onclick = function() { displayEvents(); };
	}
}

//Adds the childNodes of individual events to the location
function addContent(location){
	// Add a row for each item
	for (var i = 0; i < events.length; i++) {
		if (events[i].venue = locaiton)
		{
			// New row
			var tr = document.createElement("tr");
			tr.setAttribute("class", "hidden");

			// Left hand cell, containing image
			var td1 = document.createElement("td");
			var img = document.createElement("img");
			img.setAttribute("src", events[i].imgUrl[0]);
			td1.appendChild(img);
		
			// Right hand cell, containing item details
			var td2 = document.createElement("td");
		
			// Title
			var divTitle = document.createElement("div");
			var link = document.createElement("a");	
			link.setAttribute("href", events[i].url);
			link.innerHTML = events[i].title;
			divTitle.appendChild(link);
			td2.appendChild(divTitle);

			//artist details
			var divArtist = document.createElement("div");
			var artists = events[i].artists[0];
			for (var j = 1; j < artists.length-1; j++)
			{
				artists = artists + ", " + events[i].artists[j];
			}
			artists = artists + ", " + <b> events[i].artists[j]; </b>
			divArtist.innerHTML = artists;
			td2.appendChild(divArtist);

			tr.appendChild(td1);
			tr.appendChild(td2);
			table.appendChild(tr);
		}
		else 
		{
			break;
		}
	}
}

//The on click to display the events when the location is clicked
function displayEvents()
{
	
}*/

///////////////////////////Code to set up chart
/*
function setupChart()
{
	var chartScript = document.getAttribute("type")[0];
alert(script.innerHTML);

		/*      // Load the Visualization API and the piechart package.
		      google.load('visualization', '1.0', {'packages':['corechart']});

		      // Set a callback to run when the Google Visualization API is loaded.
		      google.setOnLoadCallback(drawChart);

		      // Callback that creates and populates a data table,
		      // instantiates the pie chart, passes in the data and
		      // draws it.
		      function drawChart() {

			// Create the data table.
		
			var data = new google.visualization.DataTable();
			data.addColumn('string', 'TArtist');
			data.addColumn('number', 'PlayCounts');
			
			for (var i = 0; i < table.length; i++)
			{
				data.addRow([
					[table[i].name, table[i].playCounts],
				]);
			}
			

			// Set chart options
			var options = {'title':'PlayCounts',
				       'width':400,
				       'height':300};

			// Instantiate and draw our chart, passing in some options.
			var chart = new google.visualization.PieChart(document.getElementById('chart_div'));
			chart.draw(data, options);
      }
}*/
