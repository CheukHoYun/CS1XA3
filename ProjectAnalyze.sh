#!/bin/bash

#Comment

getInformation(){
	theDate=$(date) && echo "The date today is: $theDate"
	theUserName=$(whoami) && echo "Current User is: $theUserName"
	echo " "
	return
}


checkUptoDate(){
	echo "Showing the git status:"
	git status -uno
}
checkUptoDate

