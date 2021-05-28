/* Robot
 * Author: himasha
 * Creation date: 12/17/2020
 */
MACHINE
    Robot

INCLUDES
    Maze

PROMOTES
   getPosition , foundExit , visitedSquare , robotsRoute

SETS

     TOAST_MESSAGE = {
        MoveSuccess ,
        TeleportSuccess ,
        MoveFailedDueToAnInternalWall ,
        TeleportFailedDueToAnInternalWall ,
        MoveFailedDueToTheMazeBoundary ,
        TeleportFailedDueToTheMazeBoundary ,
        CannotTeleportToExit
    }

 ABSTRACT_VARIABLES
   stateX , stateY


INVARIANT
    stateX : dom ( POSIBLECELLS ) & stateY : ran ( POSIBLECELLS )

INITIALISATION
    stateX , stateY := prj1 ( NATURAL1 , NATURAL1 ) ( START ) , prj2 ( NATURAL1 , NATURAL1 ) ( START )

OPERATIONS
  message  <-- MoveSouth =
    IF ( ( stateX |-> ( stateY - 1 ) ) : POSIBLECELLS )
    THEN stateY := stateY - 1 ||
        changeData ( ( stateX |-> stateY - 1 ) ) ||
        message := MoveSuccess
    ELSIF ( stateX |-> ( stateY - 1 ) ) : SHADED
    THEN
        message := MoveFailedDueToAnInternalWall
    ELSE
        message := MoveFailedDueToTheMazeBoundary
END ;

  message  <-- MoveNorth =
    IF ( ( stateX |-> ( stateY + 1 ) ) : POSIBLECELLS )
    THEN stateY := stateY + 1 ||
        changeData ( ( stateX |-> stateY + 1 ) ) ||
        message := MoveSuccess
    ELSIF ( stateX |-> ( stateY + 1 ) ) : SHADED
    THEN
        message := MoveFailedDueToAnInternalWall
    ELSE
        message := MoveFailedDueToTheMazeBoundary
END ;

 message  <-- MoveEast =
    IF ( ( ( stateX + 1 ) |-> stateY ) : POSIBLECELLS )
    THEN stateX := stateX + 1 ||
        changeData ( ( stateX + 1 |-> stateY ) ) ||
        message := MoveSuccess
    ELSIF ( ( stateX + 1 ) |-> stateY ) : SHADED
    THEN
        message := MoveFailedDueToAnInternalWall
    ELSE
        message := MoveFailedDueToTheMazeBoundary
END ;
 message  <--  MoveWest =
    IF ( ( ( stateX - 1 ) |-> stateY ) : POSIBLECELLS )
    THEN stateX := stateX - 1 ||
        changeData ( ( stateX - 1 |-> stateY ) ) ||
        message := MoveSuccess
    ELSIF ( ( stateX - 1 ) |-> stateY ) : SHADED
    THEN
        message := MoveFailedDueToAnInternalWall
    ELSE
        message := MoveFailedDueToTheMazeBoundary
END ;

 message  <--  Teleport ( toX , toY ) =
 PRE toX : NATURAL & toY : NATURAL
 THEN

    IF ( ( toX |-> toY ) : POSIBLECELLS & ( toX |-> toY ) /= EXIT )
    THEN stateX := toX || stateY := toY ||
        changeData ( ( toX |-> toY ) ) ||
        message := TeleportSuccess
    ELSIF ( toX |-> toY ) : SHADED
    THEN
        message := TeleportFailedDueToAnInternalWall
    ELSIF ( ( toX |-> toY ) = EXIT )
     THEN
       message := CannotTeleportToExit
    ELSE
        message := TeleportFailedDueToTheMazeBoundary
    END
END

//message  <--  Teleport(toX,toY) = 
// PRE toX:NATURAL & toY : NATURAL
// THEN
//     
//    IF ((toX|->toY) : POSIBLECCELLS & ((toX|->toY) /= EXIT or (stateX|->stateY)/= START))
//    THEN stateX := toX || stateY := toY ||
//        changeData((toX|->toY))||
//        message := TeleportSuccess
//    ELSIF (toX|->toY) : SHADED
//    THEN 
//        message := TeleportFailedDueToAnInternalWall
//    ELSIF((toX|->toY) = EXIT & (stateX|->stateY) = START)
//     THEN
//       message := CannotTeleportToExit  
//    ELSE        
//        message := TeleportFailedDueToTheMazeBoundary
//    END
//END

END
