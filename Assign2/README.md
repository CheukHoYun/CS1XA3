# Assignment 2 - Elm Architecture
## Part 1 is a simple CV page show my knowledge in HTML and CSS. Part 2 is a simple game coded in Elm, to show my skill in Elm architecture. 
View project here: http://ugweb.cas.mcmaster.ca/~yunc5/
(Might be unavailable when not using McMaster network. Try https://htmlpreview.github.io/?https://github.com/CheukHoYun/CS1XA3/blob/master/Assign2/Assignment.html to see the game page. )

## Part 1 - CV Page
It's just a simple CV page I made from templates.
Attributes:
- http://sampleresumetemplate.net/, where the template came from.
- https://www.w3schools.com/css/default.asp, where I learned a ton of CSS.

## Part 2 - Elm App
### Introduction
For the app I decided to make a flappy bird copy, and focus on emulating the physics and the hitbox, making them as accurate as possible. And I managed to do it - they work perfectly well!

### General Idea of Coding
1. Two timers used: one for tubes, which does not change its origin; one for the moving pixel, which resets its origin whenever the state of the pixel changes.
2. Print the tubes and pixel using SVG. Generate their position as a function of time. Use Newton's Law to emulate real-life movement.
3. Subscribe to keyboard key downs and time. Update the model whenever receives one of the messages.
4. When key pressed, change the state of the pixel, and make the pixel a jump.
5. For every update, check if the pixel hits the tubes or not. Since they are all rectangles, it isn't hard to find if they overlap or not.
6. Make a scoring system. Whenever a tube passes the pixel, add one score. Since there are frame-skipping, have to handle the error with a special algorithm. (It's too complicated to put here.)
7. Make a start and end state. Stop the timer when the game isn't started; freeze the scene when it ends. Print scores when it ends.
8. Use Html elements to make a simple introduction beside the game.

### Known issues
1. Pressing Spacebar may cause page scrolling.
2. The background color is sometimes blank, and it only happens to Safari.
3. Inevitable frame-skipping may cause the system fail to catch a failure, but it's a very rare circumstance.
4. The code (especially the Html part) can be rewritten and shortened. There's no issue with code efficiency, though. Just the readability. 
