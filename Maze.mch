/* Maze
 * Author: himasha
 * Creation date: 12/05/2020
 */
MACHINE
    Maze

CONSTANTS
    GRID,RESTRICTED,PATHSQUARE,EXIT,START
     
 PROPERTIES
    
     GRID : POW (NATURAL1 * NATURAL1)  & 
     GRID = { xx, yy | xx : NATURAL1 & yy : NATURAL1 & xx <= 7 & yy <= 5 } & 
     RESTRICTED <:{ xx, yy | xx : NATURAL1 & yy : NATURAL1 & xx <= 7 & yy <= 5 } &
     RESTRICTED = { (2|->1), (6|->1), (4|->2),(6|->2), (1|->3), (2|->3),(3|->3), (4|->3),(4|->4),(6|->4), (7|->4) ,(2|->5)} &    
     PATHSQUARE = GRID-RESTRICTED &
     EXIT : PATHSQUARE & EXIT = (1|->5) &
     START : PATHSQUARE & START = (1|->1)
  SETS   
 
    ANSWERS = {Yes, No}
    
     
 VARIABLES
    square, visited
 
INVARIANT
    square : PATHSQUARE &
    visited : seq(INTEGER * INTEGER)
    
INITIALISATION
    square, visited := START,[]
OPERATIONS
    changeData(currentCell)=
    PRE currentCell:PATHSQUARE
    THEN
        square := currentCell||
        visited := visited <- (square)
    END;
        
 
 position  <-- getPosition  =
   BEGIN 
       position := square
   END;
   
   
    isExit  <-- foundExit  =
   IF
       (square=EXIT)
   THEN
       isExit := Yes
   ELSE
       isExit := No
   END;

isVisited  <-- visitedSquare(isVisitedX,isVisitedY) =
PRE 
   isVisitedX :dom(PATHSQUARE) & isVisitedY :ran(PATHSQUARE) &
   (isVisitedX|->isVisitedY):PATHSQUARE
THEN
    IF((isVisitedX|->isVisitedY):ran(visited))
    THEN
      isVisited := Yes
  ELSE
      isVisited := No
      END
        
END;


route  <-- robotsRoute =
   BEGIN 
       route := visited
   END




END

