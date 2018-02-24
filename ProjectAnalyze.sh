#!/bin/bash

#As an experienced (not really) functional programmer, I decided to put each functionality under a function.
#I will create a main function to make a list of functionality and let users to choose from. 
 

getInformation(){	#A simple function used to grab time and username. I wrote it to remind myself how bash scripts work.
	theDate=$(date) && echo "The time now is: $theDate"
	theUserName=$(whoami) && echo "Current User is: $theUserName"
	return
}

checkUpdate(){	#This is required functionality 1, which is supposed to check if the repo is up-to-date or not. 
	echo "-------------------------------------------------------------"
	git fetch && git status -uno	#Don't want to see untracked files, so I use -uno here.
	echo "-------------------------------------------------------------"
	hold
}


uncommited(){	#This is required functionality 2, which is to grab all uncommited changes and put it into "changes.log". It allows user to see the log immediately if they want. Information from git status and git diff are both recorded.  
	echo "-------------------------------------------------------------"
	echo "Putting all uncommited changes into changes.log..."
	theDate=$(date) && echo $theDate$'\n' >> changes.log	#Since it's a log, I would say including time for each record is nice.	
	git fetch && git status -uno >> changes.log
	echo $'\n' >> changes.log
	git diff >> changes.log	
	echo "Jobs done."
	echo "-------------------------------------------------------------"
	read -p 'Do you want to see the log now?(Y/N)'$'\n' seeLog
	if [[ "$seeLog" =~ ^(y|yes)$ ]]	#Either y,Y,yes or YES will work. Receiving other characters will return nothing.
	then
	echo "---------------------HEAD OF LOG-----------------------------"
		cat changes.log
	echo "----------------------END OF LOG-----------------------------"
	
	fi
	hold

}

extractTODO(){	#This is required functionality 3. It looks into all files, find lines with #TODO (case sensitive), and output them into "todo.log".
	echo "-------------------------------------------------------------"
	echo "Extracting all lines with #TODO in the repository to the todo.log file."
	if [ -e todo.log ]	#Check if the todo.log already exists. If it does, delete it.
	then
		rm todo.log
		echo "todo.log already exists. Removing it and creating a new one..."
	fi
	grep -rh --exclude={*.log,ProjectAnalyze.sh} '#TODO' *  > todo.log	#This script itself won't be included, and any .log files won't be searched. I only want #TODOs with actual meaning, not the ones in code or logs.
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

findHaskellError(){	#This is required functionality 4. It looks into all Haskell files, try to (not actually) compile them, and find syntax errors if there are any. It won't work for non-moduled .hs files, unless you put a line of main=undefined in your file. 
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
hold(){	#This is a simple but extremely useful "Press any key to return" function. I use this function in basically every other function to prompt the user whenever a process is finished.
        read -n1 -r -p "Press any key to return to main menu..." key
	
}

main(){	#This is main. It shows time and username, and prints the main menu.
	getInformation
	while true; do
	read -p $'Please enter a number:\n1.Required Functionalities\n2.Additional Functionalities\n3.Exit\n> ' choice1
	case "$choice1" in
		1) subMenu1;;
		2) subMenu2;;
		3) break;;
		*) ;;	#For any unexpected input, continue the loop.
	esac
	done
}
seeCourse(){	#This is the first feature of my own design. It grabs information from 1XA3 course page, and returns course schedule or grading scheme. 
	read -p $'Please enter a number:\n1.See course schedule.\n2.See grading scheme.\n> ' choice3
        case "$choice3" in
                1) lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n '35,57p' && hold;;	#It grabs the course page and cut information between certain lines. 
                2) lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n '58,66p' && hold;;
		*) ;;
	esac

}

seeGrade(){	#This is the second feature of mine. It looks for our 1XA3 grades, print it, and outputs it into a .txt file if the user wants to.
        echo "-------------------------------------------------------------"
	L1=$(lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | grep -n '7 Marks' | cut -f1 -d:)
	L2=$(lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | grep -n '400160537' | cut -f1 -d:)	
	lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n "$L1"','"$L2"'p'	#It looks for two keywords, returns their line numbers as variables, and pass these variables into sed to cut out the lines I want. This will always work even new contents are added onto the webpage, since it looks for the lines dynamically instead of using static line numbers.
        echo "-------------------------------------------------------------"
	read -p 'Do you want to export the grades?(Y/N)'$'\n' decision
	if [[ "$decision" =~ ^(y|yes)$ ]]
	then
	if [ -e Grades.txt ]
	then
		rm Grades.txt
		echo "Grades.txt already exists. Removing it and creating a new one..."
	fi	
	lynx -dump http://www.cas.mcmaster.ca/~dalvescb/ | sed -n "$L1"','"$L2"'p' > Grades.txt	#If the user wants, the script will output the grades into Grades.txt.	
	fi
	echo "Done."	
	hold	
}

subMenu1(){	#This is the first sub-menu who works for the required features. Nothing special here.
	read -p $'Please enter a number:\n1.Check if your repo is up to date.\n2.Ouput uncommited changes.\n3.Find and output all TODOs in the project.\n4.Check and output Haskell errors.\n> ' choice2
        case "$choice2" in
                1) checkUpdate;;
                2) uncommited;;
                3) extractTODO;;
		4) findHaskellError;;
		*) subMenu1;;
	esac
}

backUp(){	#This is the third feature of my own. It backups the whole repository as a .tar.gz file, and puts it under home directory.
	echo "Starting the back-up process.."
	cp -R ~/CS1XA3 ~/BackUp
	tar czvf ~/Backup.tar.gz ~/BackUp
	rm -rf ~/BackUp	#For safety, I won't modify the original directory. I copy it, zip the copy, and remove the copy.
	echo "Back-up finished. You can now see the file \"Backup.tar.gz\" under your home directory."
	hold	
}


subMenu2(){	#This is the 2nd sub-menu who works for the cutomized features. Nothing special.
	read -p $'Please enter a number:\n1.See course information.\n2.See grades.\n3.Backup the repository.\n> ' choice4
        case "$choice4" in
                1) seeCourse;;
                2) seeGrade;;
		3) backUp;;
		*) subMenu2;;
	esac
}	

main	#main is the only function called when script started.
