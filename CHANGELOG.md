Project 2: A thrilling game with unique interesting features designed by our team.

# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)

## [1.0.0] -- 2022-8-3

### Added

- Add locked heroes image for hero choose scene

### Removed

- Effect of key K

### Changed

- Adjust the UI face

## [0.5.4] -- 2022-8-3

### Fixed

- cannot draw hero

- cannot exit after drawing hero

## [0.5.3] -- 2022-8-3

### Changed

- Elm doc
- Simplify codes

## [0.5.2] -- 2022-8-2

### Changed

- Simplify codes in ViewShop.elm

- Simplify codes in ViewEncyclopedia.elm

- Simplify codes in ViewChoose.elm

- resolved formatting issues in view functions 

## [0.5.1] -- 2022-8-2

### Changed

- Simplify codes in ShortPath.elm

- Simplify codes in ViewShop.elm

- Simplify codes in UpdateShop.elm

- Simplify codes in EnemyAction.elm

## [0.5.0] -- 2022-7-29

### Added
- all dialogues
- all documents
- added tips for T key
- formatted all documents 
### Changed

- all the fonts
- generate enemies
- rotate in update
- formatting in encyclopedia

## [0.4.9] -- 2022-7-27

### Changed

- fix the wrong text in lucky draw

## [0.4.8] -- 2022-7-28

### Added

- Map of Boss level

- Animation of map rotation

- The boss with skills of any classes

## [0.4.7] -- 2022-7-27

### Changed

- actions for enemy healer

- appearance for buttons in the shop

- fancy shop appearance

- upgrade appearance

- delete coordinate

- change turret generation attribute


## [0.4.6] -- 2022-7-27

### Added
- added UI for button

### Changed
- changed health bar color
- heroes selection scene format

### Fixed
- fixed wrong information in encyclopedia


## [0.4.5] -- 2022-7-26 

### Added
- Encyclopedia messages and gameMode
- Encyclopedia function where players can find out more about each hero
- new assets
- ViewEncyclopedia.elm file

### Changed
- some typos
## [0.4.4] -- 2022-7-26

### Added

- Add Level 3, Level 4, and Level 5
- Add three new maps: snake map, hollow map, and dynamic map
- Connect the levels, NPCs, and the task system.
- Test mode that can unlock any NPCs, heroes, and scenes to better test/debug the game

### Changed

- Only show attackable range when hero's energy is enough
- Adjust the dialogue and tutorial system
- Change the color of hero's health bar to blue
- Adjust the task board to make it looks better
- Make the first hero lucky draw free

### Fixed

- Not to show energy use for turret
- Improve the module exposing and import clear and concrete

## [0.4.3] -- 2022
### Added
- added UI for board game

### Fixed
- fixed volume adjust tab position

### Changed
- changed board game background to have simpler color
- changed formatting issues such as text alignment

## [0.4.2] -- 2022-7-24

### Added
- added SFXs for every character
- added theme songs for both rpg mode and board game mode
- added display obtained character scene
- added board game summarize scene
- added board game background images

### Fixed
- fixed select nonplayable character


## [0.4.1] -- 2022-7-24

### Changed
- changed some of the formmatting for tutorial
- corrected some typos in tutorial 


## [0.4.0] -- 2022-7-23

### Added

- spend gold coins upgrading heroes
- draw heroes
- use black hero pictures to represent locked heroes
- use ← and → to switch heroes


## [0.3.9] -- 2022-7-23

### Added

- dialogHelper and shapeHelper functions to help with the tutorial system
- more scenes for the live tutorial 
- all the scenes for the tutorial system
- added a new task to accompany the dialog at the end of the tutorial



## [0.3.8] -- 2022-7-23

### Added

- Turret class for hero, it's similar to a fixed and automatic archer

### Changed

- change the engineer skill from placing boxes to turret
- separate shop-related functions

### Fixed

- fix health exceeding maxhealth
- fix different time interval of attack range display of turret and enemy
## [0.3.7] -- 2022-7-22

### Changed

- Left click hero on the board to select hero
- Left click hexagon in movable range to move hero
- Right click hexagon in attackable range to attack
- Right click hexagon in skillable range to use skill
- Adjust the message of left and right click to simplify it

## [0.3.6] -- 2022-7-22 
### Added 
- update Tutorial 
- viewTutorial.elm file 
- changed viewTutorial function 
- dialogbox.png
- dialog scene

### Changed 
- Tutorial in update.elm 

## [0.3.5] -- 2022-7-21

### Added

- Task system with task board and current task shown on the board

- Tutorial level

- Define NPC Type and write functions for view of NPCs

- Extend the reachable range of the RPG character

- Add many new NPCs on the different scenes

## [0.3.4] -- 2022-7-18

### Added

- Add healer enemy will try to find the nearest position to heal another enemy. Otherwise, it will have the same action like the enemy warrior. (i.e. find the nearest hero and move to attack)
## [0.3.3] -- 2022-7-18

### Added

- Add the attackable range for enemies

- Add health bar over the head of heroes and enemies on the board.

### Changed

- Improve the display of the information board of heroes in the board game

- Improve the display of the information board of enemies in the board game

- Adjust the position of the EndTurn button and Turn information

- Adjust the duration of the enemy animation

## [0.3.2] -- 2022-7-17

### Added
- Animation for healing
- Animation for items like picking up the health or energy potions
- Animation when heroes or enemies are getting attacked 
- Animation for the game board

### Fixed
- viewBug for tutorial system
- formatting
- bug for turn and time 
- 4 heroes in a list bug for smartmage function 

## [0.3.1] -- 2022-7-17

### Fixed

- image lost in NoItem


## [0.3.0] -- 2022-7-16

### Changed

- Change the reachable range of the character and the spawn position after pass the doors or beat a level

### Fixed

- Fix the bug that the faceDir of the RPG character sometimes doesn't correspond to its motion direction.

- Fix the bug that the RPG character will disapper when entering the shop.

- Fix the bug that the RPG character will get stuck in wall when exiting the shop or after beating the second

## [0.2.17] -- 2022-7-16

### Add

- Healer Class: attack, heal and break mystery boxes

### Fix

- Animation of RPGcharacter
## [0.2.16] -- 2022-7-16

### Add

- Engineer Class: attack, build and destroy
## [0.2.15] -- 2022-7-16

### Add
- RPG character animations
- Board visuals to enhance game experience 
- Animations for RPG character and heroes

## [0.2.14] -- 2022-7-14

### Add

- Union type Side

### Changed
- Make stuckInWay change with the chosen Side
### Fix

- Archer Enemies can attack heroes with another enemy in between.

## [0.2.13] -- 2022-7-12

### Changed

- Make the character move more freely and improve the code quality

## [0.2.12] -- 2022-7-11

### Added 

- Dungeon room in the map

- NPCs where players can engage in a battle with them 

## [0.2.11] -- 2022-7-10

### Added

- MultiLevels

### Fixed

- EndTurn button only works in HeroTurn

### Removed

- Unused Armor

## [0.2.10] -- 2022-7-10

### Added 

- added pattern matching for gold  
### Changed

- the viewItem function to display the gold properly


## [0.2.9] -- 2022-7-10

### Added 

- tutorial system

- how to play button 

### Changed 

- boundaries of the map so that players cannot walk out of the map

## [0.2.8] -- 2022-7-10

### Added 

- A few gifs and images to assets

- Buying items in game mode

- Key C to talk to NPCs

- Talk, UpgradeHealth, UpgradeDamage and ExitShop to msg type 

- updateHealth and updateDamage functions

- Energypotion function 

- viewShopChoose to let players see what they want to buy 

- chatbox gif and shopkeeper in the shop 

### Changed

- healthpotion in InitObstacles to energypotion and gold

- optimized the updateRPG function to include the shope system 

## [0.2.7] -- 2022-7-10

### Added

- View the coins that the character has
- checkEnd and settlement awards


### Changed

- Change Gold into Gold Int
- use index to check interactions between items and (heroes & enemies)


## [0.2.6] -- 2022-7-10

### Added

- Heroes can pick coins
- bag field in Model
- coin field in Bag
- Enemies can break items

## [0.2.5] -- 2022-7-9

### Added

- Enemy Mage can break breakable obstacles

### Changed

- Move the checkAttackObstacle function from `HeroAttack.elm` to `Action.elm`
- Change the output related to attack mage from (Enemy, List Hero) to Board

### Fixed

- Enemy will seckill heroes behind the obstacle



## [0.2.4] -- 2022-7-9

### Added

- Detecting the position of the mouse and higlight the attack target when the mouse is in the attackable range.

- Choose heroes at the beginning of the board game.

- Initialize heroes in the `Board` according to the choice of the player and the current content of the `Model`

### Fixed

- Resize of RPG scenes.

- Attack for hero mage.

## [0.2.3] -- 2022-7-8

### Added
- EnergyPotion in ItemType
- SpawnCrate message
- randomCrate, generateCrate, possibleCratePosiiton functions in `update.elm` to generate crate randomly
- spawnCrate function in `updateboard.elm` to place the generated crate
- Crate.png, EnergyPotion.png, Gold.png, HealthPotion.png, HealerBlue.png, HealerRed.png
- viewCrate, viewItem functions in `view.elm` to display mystery boxes and items

### Changed
- Image file name from Bad to Red and Good to Blue
- Generalize the viewEnemy and viewHero funciton

### Removed
- Stop displaying red and logo hexagon for health potion and mystery box

### Fixed
- Prevent randomly generate enemy in the same position


## [0.2.2] -- 2022-7-6

### Added

- Subsubneighbour.

- Movement and Attack of Mage Enemy. Mage Enemy will automatically go to the nearest position where it can attack the hero. The AOE attack effect is the same as the mage hero. It will automatically choose to attack as many heroes as possible.

- Related functions of Movement and Attack of Mage Enemy.

## [0.2.1] -- 2022-7-6

### Added

- Castle, shop and Logo to gameMode 

- Left, Down, Up, Right to Dir type and key 

- Enter Bool in message to skip scenes 

- InitCharacter, InitRPG, InitLevel, Board_1, Castle_1, Shop_2 in model 

- RpgCharacter file with moveCharacter, isLegalMoveCharacter, characterPosition functions and RpgCharacter type

- UpdateCharacter, UpdateRPG, UpdateScene to ensure scene change on RPG map 

- ViewScene files with all the relevant scenes for RPG

- faceDir in model

- different cases in moveCharacter to ensure that movements will be smoother

### Changed

- RpgCharacter type in model 

- Optimized Update function to accommodate new functions for RPG 

- Optimized View function to accommodate new functions for RPG 

## [0.2.0] -- 2022-7-5

### Changed

- Change input of the previous `shortestPath` function from `Pos` to `List Pos`, and correspondingly change the input `end` to `end_list`.
- Change the `leastPathFind` functions for warriors and archers to implement the more efficient `shortestPath`.

## [0.1.1] -- 2022-7-5

### Added
- Added field spawn and index in type alias Board
- Added randomEnemies, groupClasses, groupPositions, choosePositions, and possiblePosition function
- Added spawnEnemies, mapClassEnemy function
- Added Spawn and Kill Msg
- Added Key 75 to kill all the enemy
- Added listDifference

### Changed
- display enemies' position too

## [0.1.0] -- 2022-7-4

### Added

- Heroes' information board.

- Mouse click for hero selection.

### Changed

- Render W/E/D/X/Z/A for moveable.

- Highlight all attackable hexagons (including nonsense attackable hexagons).

### Fixed

- Enemy movement bugs.

- Improve code quality.
## [0.0.10] -- 2022-7-4

### Added
- logo.png into assets/image

- more obstacles in initObstacles

- itemType, obstacleType, item, obstacle types 

- checkObstacleType and checkItemType

- view healthpotions and different type of obstacles in view 

### Changed 

- type barrier into type obstacles

- in board type, barrier field to obstacle field with List obstacles

- initBarrier to initObstacles and initBoard

- checkAttackBarrier and checkAttackTarget to work with the new List of obstacles and items 

- every barrier function in ShortestPath.elm to accommodate the new Obstacle type 

- moveHero in UpdateBoard function to accommodate the health potion feature



## [0.0.9] -- 2022-7-2

### Changed

- Change the enemy's attack range. Now the enemy archer can attack the hero behind other enemies.


## [0.0.8] -- 2022-7-2

### Added

- Smart move and Attack action of enemy archer
- Functions related to enemy archer move in `ShortestPath.elm`
- ViewLines for debug the enemy archer

### Changed 

- Move functions related to enemy action from `Update.elm` to `EnemyAction.elm`

## [0.0.7] -- 2022-7-2

### Added

- Attack for archer and mage.

- Click for attack.

- Attack barriers will break it.

### Changed

- Rewrite function to detect attackable hexagons and moveable hexagons.

## [0.0.6] -- 2022-7-2

### Added 

- Added Healer to the Hero type

- checkHeal function

- Healer into checkForEnemy

- Key 53 healer

- Healer in HeroAttackable, viewEnemy, viewHero


### Changed

- Changed the first hero on board to healer


## [0.0.5] -- 2022-7-2

### Added

- Resize of the game view.

- Detect the position of the mouse when it click on the screen and display it on the screen to debug.

- Field `mode` is added into the `Model` to check the mode.

### Changed

- Add field `heroes` in `Board` and move `time` and `critical` from `Model` to `Board`.

- Adjust all related functions to the change above accordingly.

- Adjust `update` function to make the layer more clear.

- Make the import more clear and safe.

### Removed

- Delete some unuseful functions.

## [0.0.4] -- 2022-7-1
### Added
- Added moveable and attackable fields in type alias Board in `Board.elm`
- Added vecScale and cartesianProduct functions in `Data.elm` 
- Added highlightCells, heroMoveable, and heroAttackable functions to record the list of hexagons that the selected hero (warrior and assassin only) can move and attack in `Update.elm`
- Added deselectHeroes function to deselect the hero every end of the turn so that it will stop display the information and highlight the hexagons in `Update.elm`

### Changed
- Changed viewCell function to be able to view the extra highlighted hexagons `View.elm`

## [0.0.3] -- 2022-7-1 
### Added

- leastPath which generates the current optimal path for one enemy 
- enermyWarriorAttack which attacks the heroes adjacent to the enemy
- moveSmartWarrior which intelligently controls the enemy to move and attack. (Note: now the enemy attacks with a fixed damage 5 points)

### Changed

- Move isWarriorAttackRange from `HeroAttack.elm` to `Data.elm`

### Removed

- viewRoute (no need for testing)

## [0.0.2] -- 2022-6-30 
### Added

- Added a heroattack file where all functions related to heroes attacking will be there

- Added randomDamage function with a weighted probability curve to generate critical damage

- Added critical type to determine how much bonus damage players will deal

- Added generateDamage Cmd Msg

- Added viewCritical 

- Added critical field in model

### Changed

- changed the sequence of update to accommodate the RNG

## [0.0.1] -- 2022-6-30

### Added 
- added checkEnd function

### Changed
- changed checkForEnemy, improve the code quality by using List.map
- changed viewEnemyInformation to receive different input and stop displaying enemy information when they die 
- changed viewEnemyInfo to display the text top aligned

### Removed
- 

## [GS.6] -- 2022-6-29

### Added 
- added key H for hit message
- added checkMelee, checkForEnemy, checkAttack functions to check for damage
- added the ViewAllEnemy file that contains all views related to enemies
- added the ViewAllHero file that contains all views related to heroes
- added the ViewOthers file that contains all other views
### Changed
- changed enemy in board to enemies in board type 
- changed numberOnBoard to indexOnBoard in hero type 
### Removed
- viewInfo file

## [GS.5] -- 2022-6-28

### Added

- Add `ShortestPath.elm` file

- In `ShortestPath.elm`, add `shortestPath` which takes the current board, list of heroes, begin position and end position as input; output the current shortest path in the form of List ( Pos )

- In `View.elm`, add `viewRoute` to view the shortest path generated by `shortestPath`

- In `View.elm`, add `viewCoordinate` to view the coordinate of all hexagons


## [GS.4] -- 2022-6-28

### Added
- numberOnBoard field to hero class as an index for the heroes in the game because players can choose different heroes every round
- Assasin class for initModel
- Images of heroes and enemies in assets folder for debugging and game mechanics 
- Added the images into the view function 
- Added viewInformation, viewHeroInfo functions so that the selected hero's information can be displayed
- Added resetEnergy function to reset heroes' energy every end of the turn
### Changed
- moveHero function can now only be move the character only if the hero has sufficient energy 

## [GS.3] -- 2022-6-27

### Added

- Make the motion of heroes legal.

- Add `EndTurn` to Message and render "EndTurn" button.

- Add `time` to Model, add `energy` to Hero, add `turn` to Board and add `steps` and `done` to Enemy

- Realize the change of the turn between the player and the automatic enemy.

- Realize the automatic motion of the enemy. (Not that intelligent now. Try to move towards the nearest hero 2 step each turn without bypassing barriers)

- Define the distance between two Hexagons and define function to calculate distance between two Hexagons and function to find the least distance between one Hexagon and a list of Hexagons.

### Changed

- Change the name of type Charater into Hero and so do all relative parameters.

## [GS.2] -- 2022-06-26

### Added

- Render all kinds of heroes, enemies and barriers on the board using different color shapes.

- Realize the selection of heroes by the players. (Press 1/2/3/4 to select Warrior/Archer/Assissin/Mage)

## [GS.1] -- 2022-06-25

### Added

- Basic types and classes for the board game as well as `Model` are defined.

- The coordinates of the hexagon cells are defined.

- View function for rendering the board consists of hexagon cells is written.

- Functions to move and render characters on the board are added. (Use a solid black circle to represent the character)
