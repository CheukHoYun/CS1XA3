#!/bin/bash

#As an experienced (not really) functional programmer, I decided to put each functionality under a function.
#I will create a main function to make a list of functionality and let users to choose from.  

getInformation(){
	theDate=$(date) && echo "The date today is: $theDate"
	theUserName=$(whoami) && echo "Current User is: $theUserName"
	echo " "
	return
}


uncommited(){
	echo "Putting all uncommited changes into the file changes.log."
	theDate=$(date) && echo $'\n'$theDate >> changes.log	
	git fetch && git status -uno >> changes.log	


}

extractTODO(){
	echo "Extracting all lines with #TODO in the repository to the todo.log file. This process is case-sensitive."
	rm todo.log
	grep -rh '#TODO' * > todo.log
}

findHaskellError(){
	echo "Searching for errors in Haskell files in repository. You will find the errors in error.log."
	theDate=$(date) && echo $'\n'$theDate >> error.log	
	find . -name "*.hs" -exec ghc -fno-code {} \; 2>> error.log	
}

main(){
	while true; do
	read -p $'Please enter a number:\n1.Required Functionalities\n2.Additional Functionalities\n3.Exit\n> ' choice1
	case "$choice1" in
		1) subMenu1;;
		2) subMenu2;;
		3) break;;
		*) ;;
	esac
	done
}

subMenu1(){
	read -p $'Please enter a number:\n1.Check if you repo is up to date.\n2.Ouput uncommited changes.\n3.Find and output all TODOs in the project.\n4.Check and output Haskell errors.\n> ' choice2
        case "$choice2" in
                1) checkUpdate;;
                2) uncommited;;
                3) extractTODO;;
		4) findHaskellError;;
		5) subMenu1;;
		5) ;;
	esac
}

subMenu2(){
	echo "subMenu2 is also empty"
}

main
