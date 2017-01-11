// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// All of the Node.js APIs are available in this process.
require('./output/Main').main()

/*
const host = 'http://127.0.0.1:8080'
const endpoint = '/users'

let maxNoOfAttempts = 50,
    waitTimeBetweenAttempt = 250

let _fetchUserList = function(waitTime, maxAttempts, currentAttemptNo) {
    $.getJSON(host + endpoint, function(users) {
	$('#status').html(`Fetched the content after attempt no. ${currentAttemptNo}!`)
	let output = "";
	for (let i in users) {
	    let user = users[i]
	    output += `ID: ${user.userID}, FirstName: ${user.userFirstName}, LastName: ${user.userLastName}<br>`
	    
	}

	$('#userList').html(output)
    }).fail(function() {
	$('#status').html(`Attempt no. <b>$currentAttemptNo}</b>. Are you sure the server is running on <b>${host}</b> and the endpoint ${endpoint} is correct?`)
	if (currentAttemptNo < maxAttempts) {
	    setTimeout( function() {
		_fetchUserList(waitTime, maxAttempts, currentAttemptNo+1)
	    }, waitTime)
	}
    })
}

let fetchUserList = function(waitTimeBetweenAttempt, maxNoOfAttempts) {
    _fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}

fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts)
*/


