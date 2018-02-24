# Assignment 1
## Required features
*	Check if the repo is up-to-date.
*	Output all uncommited changes into changes.log.
*	Output all lines with #TODO found in the repo into todo.log.
*	Check all Haskell files for syntax errors, output them into error.log.
## Features of my own design
*	A main menu and several sub-menus, for users to pick functionalities easily.
*	Allow users to see logs immediately after the outputs if they want. 
*	Grab course schedule and course grading scheme, print them on the screen.
*	Grab student grades from course web page, allow users to output it into a .txt file. (www.cas.mcmaster.ca/~dalvescb, author: Curtis D'Alves)
*	Backup the whole repository as a .tar.gz file, put it under home directory.
*	Added an exit option for the script. You may exit without pressing Ctrl+C, but in a more gentle way.
## Usage
The program consists a user-friendly menu system, so there is no need for users to type in any characters other than choice numbers or (Y/N). To get access to any feature, follow the menu and type in choice numbers. Type in Y/N whenever asked.
### Feature 1-1
Type in '1' twice and you can see if the repository is up-to-date. Press any key to return to main menu.
### Feature 1-2
Type in '1' and then '2', and you can output all uncommited changes to changes.log. You will be asked if you want to see the log now. Make a choice by typing in y or n. ('Yes' and 'No' will also work.)
### Feature 1-3
Type in '1' and then '3', and you can output all lines with #TODO into todo.log. This process is case-sensitive. This process will not search inside ProjectAnalyze.sh, and will not search inside any .log file, since it will be dumb to do so. You will be asked if you want to see the log immediately.
### Feature 1-4
Type in '1' and then '4', and you can output all syntax errors that found in your repo's .hs files into error.log. You will be asked if you want to see the log immediately. 
### Feature 2-1
Type in '2' and then '1', and you can get access to some basic 1XA3 course info. You can then choose from course schedule and course grading scheme. They will be printed on you screen. 
### Feature 2-2
Type in '2' and then '2', and you can get access to 1XA3 students' current grades. This will update information from course web page so it's always new. It will be printed on your screen, and you will be asked if you want to output it into a .txt file. 
### Feature 2-3
Type in '2' and then '3', and the whole repository will be backed up as a .tar.gz file. The backup file will be under the user's home (~) directory.
### Feature 3-3
Type in '3' and you will exit the program. 
