#!/bin/bash

#As an experienced (not really) functional programmer, I decided to put each functionality under a function.
#I will create a main function to make a list of functionality and let users to choose from. 
 

getInformation(){
	theDate=$(date) && echo "The time now is: $theDate"
	theUserName=$(whoami) && echo "Current User is: $theUserName"
	return
}

checkUpdate(){
	echo "-------------------------------------------------------------"
	git fetch && git status -uno
	echo "-------------------------------------------------------------"
	hold
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
	hold

}

extractTODO(){
	echo "-------------------------------------------------------------"
	echo "Extracting all lines with #TODO in the repository to the todo.log file."
	if [ -e todo.log ]
	then
		rm todo.log
		echo "todo.log already exists. Removing it and creating a new one..."
	fi
	grep -rh --exclude={*.log,ProjectAnalyze.sh} '#TODO' *  > todo.log
	echo "Jobs done."
	echo "-------------------------------------------------------------"
	read -p 'Do you want to see the log now?(Y/N)'$'\n' seeLog
	if [[ "$seeLog" =~ ^(y|yes)$ ]]
	then
	echo "---------------------HEAD OF LOG-----------------------------"
		cat todo.log
	echo "----------------------END OF LOG-----------------------------"
	fi
	hold

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
	hold

}
hold(){
        read -n1 -r -p "Press any key to return to main menu..." key
	
}

main(){
	getInformation
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
seeCourse(){
	read -p $'Please enter a number:\n1.See course schedule.\n2.See grading scale.\n> ' choice3
        case "$choice3" in
                1) lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n '35,57p' && hold;;
                2) lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n '58,66p' && hold;;
		*) ;;
	esac

}

seeGrade(){
        echo "-------------------------------------------------------------"
	L1=$(lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | grep -n '7 Marks' | cut -f1 -d:)
	L2=$(lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | grep -n '400160537' | cut -f1 -d:)	
	lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n "$L1"','"$L2"'p'
        echo "-------------------------------------------------------------"
	read -p 'Do you want to export the grades?(Y/N)'$'\n' decision
	if [[ "$decision" =~ ^(y|yes)$ ]]
	then
	if [ -e Grades.txt ]
	then
		rm Grades.txt
		echo "Grades.txt already exists. Removing it and creating a new one..."
	fi	
	lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n "$L1"','"$L2"'p' > Grades.txt	
	fi
	echo "Done."	
	hold	
}

subMenu1(){
	read -p $'Please enter a number:\n1.Check if you repo is up to date.\n2.Ouput uncommited changes.\n3.Find and output all TODOs in the project.\n4.Check and output Haskell errors.\n> ' choice2
        case "$choice2" in
                1) checkUpdate;;
                2) uncommited;;
                3) extractTODO;;
		4) findHaskellError;;
		*) subMenu1;;
	esac
}

backUp(){
	echo "Starting the back-up process.."
	cp -R ~/CS1XA3 ~/BackUp
	tar czvf ~/Backup.tar.gz ~/BackUp
	rm -rf ~/BackUp
	echo "Back-up finished. You can now see the file \"Backup.tar.gz\" under your home directory."
	hold	
}

starWars(){
	telnet towel.blinkenlights.nl
}

subMenu2(){
	read -p $'Please enter a number:\n1.See course information.\n2.See grades.\n3.Backup the repository.\n> ' choice4
        case "$choice4" in
                1) seeCourse;;
                2) seeGrade;;
		3) backUp;;
		4) starWars;;
		*) subMenu2;;
	esac
}	

main
