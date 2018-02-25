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

## Other stuff
*	There are several test files under root of the repository. (HaskellFileWithErr.hs, HaskellFileWithoutErr.hs, testFileWithTodo.txt) They are just test files, and won't affect how script works.
*	Do not move the script to other directories or it won't work. For laziness I use current directory (or even ignore the directory) whenever possible. For example, I do `find . -name blahblah` and `grep *`. If you move the script to other places, these code will go wrong.
*	I used a simple way to grab course outline (line numbers). If new contents are added into the page, the line numbers may change, and the function may stop working properly. The format of the printed course schedule in feature 2-1 is a mess, but I don't want to fix it since it's still readable.
* 	The script will always grab newest grade information, and line changes won't affect this function.
*	For feature 1-4, if your haskell file is non-moduled, you would need to add 'main=undefined' to your file, or other syntax errors won't be found.

## References
*   https://stackoverflow.com/questions/3742983/how-to-get-the-contents-of-a-webpage-in-a-shell-variable
*   https://unix.stackexchange.com/questions/293940/bash-how-can-i-make-press-any-key-to-continue
*   http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_03.html
*   https://stackoverflow.com/questions/221921/use-grep-exclude-include-syntax-to-not-grep-through-certain-files
*   https://stackoverflow.com/questions/3258243/check-if-pull-needed-in-git
*   https://stackoverflow.com/questions/5683367/how-to-cropcut-text-files-based-on-starting-and-ending-line-numbers-in-cygwin
*   http://www.cas.mcmaster.ca/~dalvescb
No code was directly copy&pasted, but I wouldn't be able to finish this script without the help of above websites.
