/* Robot
 * Author: himasha
 * Creation date: 12/05/2020
 */
MACHINE
    Robot
    
INCLUDES
    Maze
    
PROMOTES
   getPosition, foundExit, visitedSquare, robotsRoute
   
DEFINITIONS
    // GUI animation function
    ANIMATION_FUNCTION == ({ row, col, i | col:1..7 & row:1..5 & i=0 });
    ANIMATION_FUNCTION1 == ({ row, col, i | col:1..7 & row:1..5  & i=1 & (col, 6-row) : RESTRICTED });
    ANIMATION_FUNCTION2 == ({ row, col, i | col:1..7 & row:1..5 & i=2 & col=coordX & row=(6-coordY) });
   
    // Images for the GUI
    ANIMATION_IMG0 == "/white.gif";
    ANIMATION_IMG1 == "/black.gif";
    ANIMATION_IMG2 == "/robot.gif";

SETS   
   
     TOAST_MESSAGE = {
        MoveSuccess, 
        TeleportSuccess,
        MoveFailedInternalWallRestriction, 
        TeleportFailedInternalWallRestriction,
        MoveFailedMazeBoundaryRestriction, 
        TeleportFailedMazeBoundaryRestriction, 
        ProhibitedToTeleportToExit
    }
    
 VARIABLES
   coordX, coordY


INVARIANT
    coordX : dom(PATHSQUARE) & coordY : ran(PATHSQUARE) 
    
INITIALISATION
    coordX,coordY :=  prj1(NATURAL1,NATURAL1)(START) , prj2(NATURAL1,NATURAL1)(START) 
    
OPERATIONS
  message  <-- MoveSouth = 
    IF ((coordX|->(coordY-1)) : PATHSQUARE) 
    THEN coordY := coordY-1 ||
        changeData((coordX|->coordY-1))||
        message := MoveSuccess 
    ELSIF (coordX|->(coordY-1)) : RESTRICTED
    THEN 
        message := MoveFailedInternalWallRestriction
    ELSE 
        message := MoveFailedMazeBoundaryRestriction
END;
    
  message  <-- MoveNorth = 
    IF ((coordX|->(coordY+1)) : PATHSQUARE)
    THEN coordY := coordY+1 ||
        changeData((coordX|->coordY+1))||
        message := MoveSuccess
    ELSIF (coordX|->(coordY+1)) : RESTRICTED
    THEN 
        message := MoveFailedInternalWallRestriction
    ELSE 
        message := MoveFailedMazeBoundaryRestriction
END;

 message  <-- MoveEast = 
    IF (((coordX+1)|->coordY) : PATHSQUARE)
    THEN coordX := coordX+1 ||
        changeData((coordX+1|->coordY))||
        message := MoveSuccess
    ELSIF ((coordX+1)|->coordY) : RESTRICTED
    THEN 
        message := MoveFailedInternalWallRestriction
    ELSE 
        message := MoveFailedMazeBoundaryRestriction
END;
 message  <--  MoveWest = 
    IF (((coordX-1)|->coordY) : PATHSQUARE)
    THEN coordX := coordX-1||
        changeData((coordX-1|->coordY))||
        message := MoveSuccess
    ELSIF ((coordX-1)|->coordY) : RESTRICTED
    THEN 
        message := MoveFailedInternalWallRestriction
    ELSE 
        message := MoveFailedMazeBoundaryRestriction
END;

 message  <--  Teleport(toX,toY) = 
 PRE toX:NATURAL & toY : NATURAL
 THEN
     
    IF ((toX|->toY) : PATHSQUARE & (toX|->toY) /= EXIT)
    THEN coordX := toX || coordY := toY ||
        changeData((toX|->toY))||
        message := TeleportSuccess
    ELSIF (toX|->toY) : RESTRICTED
    THEN 
        message := TeleportFailedInternalWallRestriction
    ELSIF((toX|->toY) = EXIT)
     THEN
       message := ProhibitedToTeleportToExit  
    ELSE        
        message := TeleportFailedMazeBoundaryRestriction
    END
END


 
END
