// Global array to store item info
var artists = [];
var tracks = [];
var events = [];
var locations = [];

// Main entry point
function initialise()
{
	// Load the items data and put it in the table
	requestData("topartists.json", loadArtists);

	//set on click events for sorts
	setupSortClick();

	//set on click events for display data
	setupDisplayClick();

	//set on click event for search button
	setupSearch();

	//setupChart();
}

function setupSortClick()
{
	//artists
	var sortPlayCount = document.getElementById("aPlayCount");
	sortPlayCount.onclick = function() { sortArtistCount(); };
	var sortName = document.getElementById("aName");
	sortName.onclick = function() { sortArtistName(); };

	//tracks
	var sortPlayCount = document.getElementById("tPlayCount");
	sortPlayCount.onclick = function() { sortTrackCount(); };
	var sortName = document.getElementById("tName");
	sortName.onclick = function() { sortTrackName(); };
	
	//events
	var sortPlayCount = document.getElementById("date");
	sortPlayCount.onclick = function() { sortDate(); };
	var sortName = document.getElementById("headline");
	sortName.onclick = function() { sortHeadline(); };

}

function setupDisplayClick()
{
	var topArtists = document.getElementById("topArtists");
	topArtists.onclick = function() { displayArtists(); };
	var topTracks = document.getElementById("topTracks");
	topTracks.onclick = function() { displayTracks(); };
	var events = document.getElementById("events");
	events.onclick = function() { displayEvents(); };

}
// Load the artist data and put it in the table
function loadArtists(xmlhttp)
{
	artists = [];
	// Extract the list of items from JSON
	var jsonDoc = JSON.parse(xmlhttp.responseText);
	var jsonItems = jsonDoc.artists.artist;
			
	// Loop through the items
	for (var i = 0; i < jsonItems.length; i++) {
	
		// Extract basic info about each item
		var itemImgUrl = jsonItems[i].image[0]['#text'];
		var itemListeners = parseInt(jsonItems[i].listeners);
          	var itemName = jsonItems[i].name;
           	var itemPlayCount = parseInt(jsonItems[i].playcount);
           	var itemURL = jsonItems[i].url;
           	//"streamable" : "0",
           		
		// Create an object to store info about the item
		var item = {
			imgUrl : itemImgUrl,
			listeners : itemListeners,
           		name : itemName,
           		playCount : itemPlayCount,
           		url : itemURL
		};
		// And add to the (global)list of items
		artists.push(item);
	}

	// Refresh the table
	fillArtistsTable();
}

// Refresh the items table using the info in the 'items' array
function fillArtistsTable()
{
	viewSort("artistActions");
	// Get the (existing) table
	var table = document.getElementById("topArtistsTable");

	// Add a row for each item
	for (var i = 0; i < artists.length; i++) {
		// New row
		var tr = document.createElement("tr");
		
		// Left hand cell, containing image
		var td1 = document.createElement("td");
		var img = document.createElement("img");
		img.setAttribute("src", artists[i].imgUrl);
		td1.appendChild(img);
		
		// Right hand cell, containing item details
		var td2 = document.createElement("td");
		
		// Title
		var divTitle = document.createElement("div");
		var link = document.createElement("a");
		link.setAttribute("href", artists[i].url);
		link.innerHTML = artists[i].name;
		divTitle.appendChild(link);
		td2.appendChild(divTitle);

		// listeners
		var divListeners = document.createElement("div");
		divListeners.className = "listeners";
		divListeners.innerHTML = "<b>Number of listens: </b>" + artists[i].listeners;
		td2.appendChild(divListeners);

		// Play count
		var divPlayCount = document.createElement("div");
		divPlayCount.innerHTML = "<b>Play Count: </b>" +artists[i].playCount;
		td2.appendChild(divPlayCount);

		tr.appendChild(td1);
		tr.appendChild(td2);
		table.appendChild(tr);
	}
}

// Load the track data and put it in the table
function loadTracks(xmlhttp)
{
	tracks = [];
	// Extract the list of items from JSON
	var jsonDoc = JSON.parse(xmlhttp.responseText);
//var jsonItems = jsonDoc.findItemsByKeywordsResponse[0].searchResult[0].item;
	var jsonItems = jsonDoc.tracks.track;
	
	// Loop through the items
	for (var i = 0; i < jsonItems.length; i++) {
	
		// Extract basic info about each item
		var itemArtistName = jsonItems[i].artist.name;
		var itemArtistURL = jsonItems[i].artist.url;
		var itemDuration = jsonItems[i].duration;

		if (jsonItems[i].hasOwnProperty('image'))
		{
			var itemImgUrl = jsonItems[i].image[0]['#text'];
		}
		else
		{
			var itemImgUrl = findAltImg(jsonItems[i].artist.name);
		}

		var itemListeners = parseInt(jsonItems[i].listeners);
		var itemName = jsonItems[i].name;
		var itemPlayCount = parseInt(jsonItems[i].playcount);
		var itemURL = jsonItems[i].url;
           		
		// Create an object to store info about the item
		var track = {
			artistName : itemArtistName,
			artistUrl : itemArtistURL,
			duration : itemDuration,
			imgUrl : itemImgUrl,
			listeners : itemListeners,
           		name : itemName,
           		playCount : itemPlayCount,
           		url : itemURL
		};
		// And add to the (global)list of items
		tracks.push(track);
	}

	// Refresh the table
	fillTracksTable();
}

// Refresh the items table using the info in the 'items' array
function fillTracksTable()
{
	viewSort("trackActions");
	// Get the (existing) table
	var table = document.getElementById("topTracksTable");
		
	// Add a row for each item
	for (var i = 0; i < tracks.length; i++) {
		// New row
		var tr = document.createElement("tr");
		
		// Left hand cell, containing image
		var td1 = document.createElement("td");
		var img = document.createElement("img");
		img.setAttribute("src", tracks[i].imgUrl);
		td1.appendChild(img);
		
		// Right hand cell, containing item details
		var td2 = document.createElement("td");

		// Title
		var divTitle = document.createElement("div");
		var link = document.createElement("a");
		link.setAttribute("href", tracks[i].url);
		link.innerHTML = tracks[i].name;
		divTitle.appendChild(link);
		td2.appendChild(divTitle);

		//artist details
		var divArtist = document.createElement("div");
		var artistlink = document.createElement("a");
		artistlink.setAttribute("href", tracks[i].artistUrl);
		artistlink.innerHTML = "<b> Artist: </b>" + tracks[i].artistName;
		divArtist.appendChild(artistlink);
		td2.appendChild(divArtist);

		//duration
		var divDuration = document.createElement("div");
		divDuration.className = "duration";
		divDuration.innerHTML = "<b> Duration : </b>" + tracks[i].duration + "seconds";
		td2.appendChild(divDuration);

		// listeners
		var divListeners = document.createElement("div");
		divListeners.className = "listeners";
		divListeners.innerHTML = "<b>Number of listens: </b>" + tracks[i].listeners;
		td2.appendChild(divListeners);

		// Play count
		var divPlayCount = document.createElement("div");
		divPlayCount.innerHTML = "<b>Play Count: </b>" +tracks[i].playCount;
		td2.appendChild(divPlayCount);

		tr.appendChild(td1);
		tr.appendChild(td2);
		table.appendChild(tr);
	}
}

//Attempt to find alternative image for track
function findAltImg(artistName)
{
	for (var i = 0; i < artists.length; i ++)
	{
		if (artists[i].name == artistName)
		{
			
			return artists[i].imgUrl;
		}
	}
	return "";
}

// Load the artist data and put it in the table
function loadEvents(xmlhttp)
{
	events = [];
	// Extract the list of items from JSON
	var xmlDoc = xmlhttp.responseXML;

	var xmlItems = xmlDoc.getElementsByTagName("event");
		//alert(xmlItems);

	// Loop through the items
	for (var i = 0; i < xmlItems.length; i++) {
	
		// Extract basic info about each item
		var itemId = xmlItems[i].getElementsByTagName("id")[0];
		var itemTitle = xmlItems[i].getElementsByTagName("title")[0];
		var itemArtists = xmlItems[i].getElementsByTagName("artists")[0];
		var itemVenue = xmlItems[i].getElementsByTagName("venue")[0];
		var itemStartDate = xmlItems[i].getElementsByTagName("startDate")[0];
		var itemDescription = xmlItems[i].getElementsByTagName("title")[0];
		var itemImgUrl = xmlItems[i].getElementsByTagName("image");
		var itemAttendance = xmlItems[i].getElementsByTagName("attendance")[0];
		var itemReviews = xmlItems[i].getElementsByTagName("reviews")[0];
		var itemTag = xmlItems[i].getElementsByTagName("tag")[0];
		var itemUrl = xmlItems[i].getElementsByTagName("url")[1];
		var itemWebsite = xmlItems[i].getElementsByTagName("website")[0];
		var itemTickets = xmlItems[i].getElementsByTagName("tickets")[0];
		var itemCancelled = xmlItems[i].getElementsByTagName("cancelled")[0];
		var itemTags = xmlItems[i].getElementsByTagName("tags")[0];
        		
		// Create an object to store info about the item
		var event = {
			id : itemId,
			title : itemTitle,
			artists : itemArtists,
			venue : itemVenue,
			startDate : itemStartDate,
			description : itemDescription,
			imgUrl : itemImgUrl,
			attendance : itemAttendance,
			reviews : itemReviews,
			tag : itemTag,
			url : itemUrl,
			website : itemWebsite,
			tickets : itemTickets,
			cancelled : itemCancelled,
			tags : itemTags
		};

		// And add to the (global)list of items
		events.push(event);
	}
	
	// Refresh the table
	fillEventsTable();
}

//Fills the table with the events
function fillEventsTable()
{
	viewSort("eventActions");
	// Get the (existing) table
	
	var table = document.getElementById("eventsTable");
		
	// Add a row for each item
	for (var i = 0; i < events.length; i++) {
			// New row
			var tr = document.createElement("tr");

			// Left hand cell, containing image
			var td1 = document.createElement("td");
			var img = document.createElement("img");
			img.setAttribute("src", events[i].imgUrl[events[i].imgUrl.length-1].innerHTML);

			td1.appendChild(img);

			// Right hand cell, containing item details
			var td2 = document.createElement("td");
		
			// Title
			var divTitle = document.createElement("div");
			var link = document.createElement("a");	
			link.setAttribute("href", events[i].url.innerHTML);

			link.innerHTML = events[i].title.innerHTML;
			divTitle.appendChild(link);
			td2.appendChild(divTitle);

			//artist details
			var divArtist = document.createElement("div");
			var divHeadline = document.createElement("div");
			var divArtistList = document.createElement("div");

			var artistArray = events[i].artists.children;
			var artistList = "<b>Line-up: </b>" + artistArray[0].innerHTML;

			for (var j = 1; j < artistArray.length-1; j++)
			{
				artistList = artistList + ", " + artistArray[j].innerHTML;
			}

			var headliner = "<b> Headline Artist: " + artistArray[j].innerHTML + "</b>";
			divHeadline.innerHTML = headliner;
			divArtistList.innerHTML = artistList;
			divArtist.appendChild(divHeadline);
			divArtist.appendChild(divArtistList);
			td2.appendChild(divArtist);

			//event date
			var divDate = document.createElement("div");
			divDate.innerHTML = "<b>Start Date: </b>" + events[i].startDate.innerHTML;
			td2.appendChild(divDate);
			
			//tags
			var divTags = document.createElement("div");
			divTags.innerHTML = "<b> Tags: </b>";
			var tagArray = events[i].tags.children
		
			var tag = document.createElement("tag");
				tag.innerHTML = tagArray[0].innerHTML;
				divTags.appendChild(tag);

			for (var j = 1; j < tagArray.length-1; j++)
			{
				var tag = document.createElement("tag");
				tag.innerHTML = ", " +tagArray[j].innerHTML;
				divTags.appendChild(tag);
			}
				
			td2.appendChild(divTags);

			//add to table
			tr.appendChild(td1);
			tr.appendChild(td2);
			table.appendChild(tr);
	}

}



// Sort the list by number of plays
function sortArtistCount()
{
	artists.sort( function(a,b) { if (a.playCount < b.playCount) return -1; else return 1; } );
	clearTable();
	fillArtistsTable();
}

// Sort the list by number of plays
function sortArtistName()
{
	artists.sort( function(a,b) { if (a.name < b.name) return -1; else return 1; } );
	clearTable();
	fillArtistsTable();
}

// Sort the list by number of plays
function sortTrackCount()
{
	tracks.sort( function(a,b) { if (a.playCount < b.playCount) return -1; else return 1; } );
	clearTable();
	fillTracksTable();
}

// Sort the list by number of plays
function sortTrackName()
{
	tracks.sort( function(a,b) { if (a.name < b.name) 
return -1; else return 1; } );
	clearTable();
	fillTracksTable();
}

// Sort the list by start date
function sortDate()
{
	events.sort( function(a,b) { if (a.startDate < b.startDate) return -1; else return 1; } );
	clearTable();
	fillEventsTable();
}

// Sort the list by headline name
function sortHeadline()
{
	events.sort( function(a,b) { if (a.artists.getElementsByTagName("headliner")[0].innerHTML < b.artists.getElementsByTagName("headliner")[0].innerHTML) 
return -1; else return 1; } );
	clearTable();
	fillEventsTable();
}

//Displays top artists. If already displaying tracks, refreshes
function displayArtists()
{
	clearTable();

	requestData("topartists.json", loadArtists);
}

//Displays top tracks. If already displaying tracks, refreshes
function displayTracks()
{
	clearTable();

	requestData("toptracks.json", loadTracks);
}

function displayEvents()
{
	clearTable();

	requestData("events2.xml", loadEvents);
}

function clearTable()
{
	// Get the (existing) table
	var table1 = document.getElementById("topArtistsTable");
	
	// Clear any existing contents
	while (table1.childNodes.length > 0)
	{
		table1.removeChild(table1.firstChild);
	}

	// Get the (existing) table
	var table2 = document.getElementById("topTracksTable");
	
	// Clear any existing contents
	while (table2.childNodes.length > 0)
	{
		table2.removeChild(table2.firstChild);
	}

	// Get the (existing) table
	var table3 = document.getElementById("eventsTable");
	
	// Clear any existing contents
	while (table3.childNodes.length > 0)
	{
		table3.removeChild(table3.firstChild);
	}
}

//displays the correct sort buttons depending on data displayed
function viewSort (type)
{
	var actions = document.getElementById("actions");
	var p = actions.getElementsByTagName("p");

	for (var i = 0; i < p.length; i++)
	{
		if(p[i].id == type)
		{ 
			p[i].setAttribute("view", "y");
		}
		else
		{ 
			p[i].setAttribute("view", "n");
		}
	}
}

function setupSearch()
{
	var searchClick  = document.getElementById("search");
	searchClick.onclick = function() { searchTable(); };
}

function searchTable()
{
	var searchText = prompt("Please enter a word or phrase", "Last.fm" );
    
   	if (searchText != null && searchText != "" && searchText != "Last.fm") 
	{
     		var text = searchText.toLowerCase();
    	    	alert("You searched for " + text + " but search is not implemented yet");
		/*clearTable();
		searchArtist(text);
		searchTrack(text);
		searchEvent(text);

		//hide all sorts
		var actions = document.getElementById("actions");
		var p = actions.getElementsByTagName("p");

		for (var i = 0; i < p.length; i++)
		{
			p[i].setAttribute("view", "n");
		}*/
	}
}
/*
function searchArtist(text)
{
	artists = [];
	artists = artists.filter(function(artist) { return artist.search(text) > -1; } );
	console.log(artists);
	fillArtistsTable();
}

function searchTrack(text)
{
	tracks = [];
	tracks = tracks.filter(function(track) { return tracks.search(text)>-1; } );
	console.log(tracks);
	fillTracksTable();
}
	
function searchEvent(text)
{
	events = [];
	events = events.filter(function(event) { return events.search(text) > -1; } );
	console.log(events);
	fillEventsTable();
}*/
