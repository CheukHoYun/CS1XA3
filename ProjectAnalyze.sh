#!/bin/bash

#As an experienced (not really) functional programmer, I decided to put each functionality under a function.
#I will create a main function to make a list of functionality and let users to choose from. 
 

getInformation(){
	theDate=$(date) && echo "The date today is: $theDate"
	theUserName=$(whoami) && echo "Current User is: $theUserName"
	echo " "
	return
}

checkUpdate(){
	echo "-------------------------------------------------------------"
	git fetch && git status -uno
	echo "-------------------------------------------------------------"
        read -n1 -r -p "Press any key to return to main menu..." key	
}


uncommited(){
	echo "-------------------------------------------------------------"
	echo "Putting all uncommited changes into the file changes.log."
	theDate=$(date) && echo $theDate$'\n' >> changes.log	
	git fetch && git status -uno >> changes.log
	echo $'\n' >> changes.log
	git diff >> changes.log	
	echo "Jobs done."
	echo "-------------------------------------------------------------"
	read -p 'Do you want to see the log now?(Y/N)'$'\n' seeLog
	if [[ "$seeLog" =~ ^(y|yes)$ ]]
	then
	echo "---------------------HEAD OF LOG-----------------------------"
		cat changes.log
	echo "----------------------END OF LOG-----------------------------"
	
	fi
        read -n1 -r -p "Press any key to return to main menu..." key


}

extractTODO(){
	echo "-------------------------------------------------------------"
	echo "Extracting all lines with #TODO in the repository to the todo.log file."
	rm todo.log
	grep -rh '#TODO' * > todo.log
	echo "Jobs done."
	echo "-------------------------------------------------------------"
	read -p 'Do you want to see the log now?(Y/N)'$'\n' seeLog
	if [[ "$seeLog" =~ ^(y|yes)$ ]]
	then
	echo "---------------------HEAD OF LOG-----------------------------"
		cat todo.log
	echo "----------------------END OF LOG-----------------------------"
	fi
        read -n1 -r -p "Press any key to return to main menu..." key


}

findHaskellError(){
	echo "-------------------------------------------------------------"
	echo "Searching for errors in Haskell files in repository. You will find the errors in error.log."
	theDate=$(date) && echo $'\n'$theDate >> error.log	
	find . -name "*.hs" -exec ghc -fno-code {} \; 2>> error.log	
	echo "Jobs done."
	echo "-------------------------------------------------------------"
	read -p 'Do you want to see the log now?(Y/N)'$'\n' seeLog
	if [[ "$seeLog" =~ ^(y|yes)$ ]]
	then
	echo "---------------------HEAD OF LOG-----------------------------"
		cat error.log
	echo "----------------------END OF LOG-----------------------------"
	
	fi
        read -n1 -r -p "Press any key to return to main menu..." key


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
