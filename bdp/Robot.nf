Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Robot))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Robot))==(Machine(Robot));
  Level(Machine(Robot))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Robot)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Robot))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Robot))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Robot))==(Maze);
  List_Includes(Machine(Robot))==(Maze)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Robot))==(getPosition,foundExit,visitedSquare,robotsRoute)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Robot))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Robot))==(?);
  Context_List_Variables(Machine(Robot))==(?);
  Abstract_List_Variables(Machine(Robot))==(?);
  Local_List_Variables(Machine(Robot))==(stateY,stateX);
  List_Variables(Machine(Robot))==(stateY,stateX,visited,cell);
  External_List_Variables(Machine(Robot))==(stateY,stateX,visited,cell)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Robot))==(?);
  Abstract_List_VisibleVariables(Machine(Robot))==(?);
  External_List_VisibleVariables(Machine(Robot))==(?);
  Expanded_List_VisibleVariables(Machine(Robot))==(?);
  List_VisibleVariables(Machine(Robot))==(?);
  Internal_List_VisibleVariables(Machine(Robot))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Robot))==(btrue);
  Gluing_List_Invariant(Machine(Robot))==(btrue);
  Abstract_List_Invariant(Machine(Robot))==(btrue);
  Expanded_List_Invariant(Machine(Robot))==(cell: POSIBLECELLS & visited: seq(INTEGER*INTEGER));
  Context_List_Invariant(Machine(Robot))==(btrue);
  List_Invariant(Machine(Robot))==(stateX: dom(POSIBLECELLS) & stateY: ran(POSIBLECELLS))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(Robot))==(btrue);
  Expanded_List_Assertions(Machine(Robot))==(btrue);
  Context_List_Assertions(Machine(Robot))==(btrue);
  List_Assertions(Machine(Robot))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Robot))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Robot))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Robot))==(cell,visited:=START,<>;stateX,stateY:=prj1(NATURAL1,NATURAL1)(START),prj2(NATURAL1,NATURAL1)(START));
  Context_List_Initialisation(Machine(Robot))==(skip);
  List_Initialisation(Machine(Robot))==(stateX,stateY:=prj1(NATURAL1,NATURAL1)(START),prj2(NATURAL1,NATURAL1)(START))
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Robot))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Robot),Machine(Maze))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(Robot),Machine(Maze))==(btrue);
  List_Context_Constraints(Machine(Robot))==(btrue);
  List_Constraints(Machine(Robot))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Robot))==(MoveSouth,MoveNorth,MoveEast,MoveWest,Teleport,getPosition,foundExit,visitedSquare,robotsRoute);
  List_Operations(Machine(Robot))==(MoveSouth,MoveNorth,MoveEast,MoveWest,Teleport,getPosition,foundExit,visitedSquare,robotsRoute)
END
&
THEORY ListInputX IS
  List_Input(Machine(Robot),robotsRoute)==(?);
  List_Input(Machine(Robot),visitedSquare)==(isVisitedX,isVisitedY);
  List_Input(Machine(Robot),foundExit)==(?);
  List_Input(Machine(Robot),getPosition)==(?);
  List_Input(Machine(Robot),MoveSouth)==(?);
  List_Input(Machine(Robot),MoveNorth)==(?);
  List_Input(Machine(Robot),MoveEast)==(?);
  List_Input(Machine(Robot),MoveWest)==(?);
  List_Input(Machine(Robot),Teleport)==(toX,toY)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Robot),robotsRoute)==(route);
  List_Output(Machine(Robot),visitedSquare)==(isVisited);
  List_Output(Machine(Robot),foundExit)==(isExit);
  List_Output(Machine(Robot),getPosition)==(position);
  List_Output(Machine(Robot),MoveSouth)==(message);
  List_Output(Machine(Robot),MoveNorth)==(message);
  List_Output(Machine(Robot),MoveEast)==(message);
  List_Output(Machine(Robot),MoveWest)==(message);
  List_Output(Machine(Robot),Teleport)==(message)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Robot),robotsRoute)==(route <-- robotsRoute);
  List_Header(Machine(Robot),visitedSquare)==(isVisited <-- visitedSquare(isVisitedX,isVisitedY));
  List_Header(Machine(Robot),foundExit)==(isExit <-- foundExit);
  List_Header(Machine(Robot),getPosition)==(position <-- getPosition);
  List_Header(Machine(Robot),MoveSouth)==(message <-- MoveSouth);
  List_Header(Machine(Robot),MoveNorth)==(message <-- MoveNorth);
  List_Header(Machine(Robot),MoveEast)==(message <-- MoveEast);
  List_Header(Machine(Robot),MoveWest)==(message <-- MoveWest);
  List_Header(Machine(Robot),Teleport)==(message <-- Teleport(toX,toY))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  Own_Precondition(Machine(Robot),robotsRoute)==(btrue);
  List_Precondition(Machine(Robot),robotsRoute)==(btrue);
  Own_Precondition(Machine(Robot),visitedSquare)==(isVisitedX: dom(POSIBLECELLS) & isVisitedY: ran(POSIBLECELLS) & isVisitedX|->isVisitedY: POSIBLECELLS);
  List_Precondition(Machine(Robot),visitedSquare)==(isVisitedX: dom(POSIBLECELLS) & isVisitedY: ran(POSIBLECELLS) & isVisitedX|->isVisitedY: POSIBLECELLS);
  Own_Precondition(Machine(Robot),foundExit)==(btrue);
  List_Precondition(Machine(Robot),foundExit)==(btrue);
  Own_Precondition(Machine(Robot),getPosition)==(btrue);
  List_Precondition(Machine(Robot),getPosition)==(btrue);
  List_Precondition(Machine(Robot),MoveSouth)==(btrue);
  List_Precondition(Machine(Robot),MoveNorth)==(btrue);
  List_Precondition(Machine(Robot),MoveEast)==(btrue);
  List_Precondition(Machine(Robot),MoveWest)==(btrue);
  List_Precondition(Machine(Robot),Teleport)==(toX: NATURAL & toY: NATURAL)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Robot),Teleport)==(toX: NATURAL & toY: NATURAL | toX|->toY: POSIBLECELLS & toX|->toY/=EXIT ==> (toX|->toY: POSIBLECELLS | stateX,stateY:=toX,toY || cell,visited:=toX|->toY,visited<-cell || message:=TeleportSuccess) [] not(toX|->toY: POSIBLECELLS & toX|->toY/=EXIT) ==> (toX|->toY: SHADED ==> message:=TeleportFailedDueToAnInternalWall [] not(toX|->toY: SHADED) ==> (toX|->toY = EXIT ==> message:=CannotTeleportToExit [] not(toX|->toY = EXIT) ==> message:=TeleportFailedDueToTheMazeBoundary)));
  Expanded_List_Substitution(Machine(Robot),MoveWest)==(btrue | stateX-1|->stateY: POSIBLECELLS ==> (stateX-1|->stateY: POSIBLECELLS | stateX:=stateX-1 || cell,visited:=stateX-1|->stateY,visited<-cell || message:=MoveSuccess) [] not(stateX-1|->stateY: POSIBLECELLS) ==> (stateX-1|->stateY: SHADED ==> message:=MoveFailedDueToAnInternalWall [] not(stateX-1|->stateY: SHADED) ==> message:=MoveFailedDueToTheMazeBoundary));
  Expanded_List_Substitution(Machine(Robot),MoveEast)==(btrue | stateX+1|->stateY: POSIBLECELLS ==> (stateX+1|->stateY: POSIBLECELLS | stateX:=stateX+1 || cell,visited:=stateX+1|->stateY,visited<-cell || message:=MoveSuccess) [] not(stateX+1|->stateY: POSIBLECELLS) ==> (stateX+1|->stateY: SHADED ==> message:=MoveFailedDueToAnInternalWall [] not(stateX+1|->stateY: SHADED) ==> message:=MoveFailedDueToTheMazeBoundary));
  Expanded_List_Substitution(Machine(Robot),MoveNorth)==(btrue | stateX|->stateY+1: POSIBLECELLS ==> (stateX|->stateY+1: POSIBLECELLS | stateY:=stateY+1 || cell,visited:=stateX|->stateY+1,visited<-cell || message:=MoveSuccess) [] not(stateX|->stateY+1: POSIBLECELLS) ==> (stateX|->stateY+1: SHADED ==> message:=MoveFailedDueToAnInternalWall [] not(stateX|->stateY+1: SHADED) ==> message:=MoveFailedDueToTheMazeBoundary));
  Expanded_List_Substitution(Machine(Robot),MoveSouth)==(btrue | stateX|->stateY-1: POSIBLECELLS ==> (stateX|->stateY-1: POSIBLECELLS | stateY:=stateY-1 || cell,visited:=stateX|->stateY-1,visited<-cell || message:=MoveSuccess) [] not(stateX|->stateY-1: POSIBLECELLS) ==> (stateX|->stateY-1: SHADED ==> message:=MoveFailedDueToAnInternalWall [] not(stateX|->stateY-1: SHADED) ==> message:=MoveFailedDueToTheMazeBoundary));
  List_Substitution(Machine(Robot),robotsRoute)==(route:=visited);
  Expanded_List_Substitution(Machine(Robot),robotsRoute)==(btrue | route:=visited);
  List_Substitution(Machine(Robot),visitedSquare)==(IF isVisitedX|->isVisitedY: ran(visited) THEN isVisited:=Yes ELSE isVisited:=No END);
  Expanded_List_Substitution(Machine(Robot),visitedSquare)==(isVisitedX: dom(POSIBLECELLS) & isVisitedY: ran(POSIBLECELLS) & isVisitedX|->isVisitedY: POSIBLECELLS | isVisitedX|->isVisitedY: ran(visited) ==> isVisited:=Yes [] not(isVisitedX|->isVisitedY: ran(visited)) ==> isVisited:=No);
  List_Substitution(Machine(Robot),foundExit)==(IF cell = EXIT THEN isExit:=Yes ELSE isExit:=No END);
  Expanded_List_Substitution(Machine(Robot),foundExit)==(btrue | cell = EXIT ==> isExit:=Yes [] not(cell = EXIT) ==> isExit:=No);
  List_Substitution(Machine(Robot),getPosition)==(position:=cell);
  Expanded_List_Substitution(Machine(Robot),getPosition)==(btrue | position:=cell);
  List_Substitution(Machine(Robot),MoveSouth)==(IF stateX|->stateY-1: POSIBLECELLS THEN stateY:=stateY-1 || changeData(stateX|->stateY-1) || message:=MoveSuccess ELSIF stateX|->stateY-1: SHADED THEN message:=MoveFailedDueToAnInternalWall ELSE message:=MoveFailedDueToTheMazeBoundary END);
  List_Substitution(Machine(Robot),MoveNorth)==(IF stateX|->stateY+1: POSIBLECELLS THEN stateY:=stateY+1 || changeData(stateX|->stateY+1) || message:=MoveSuccess ELSIF stateX|->stateY+1: SHADED THEN message:=MoveFailedDueToAnInternalWall ELSE message:=MoveFailedDueToTheMazeBoundary END);
  List_Substitution(Machine(Robot),MoveEast)==(IF stateX+1|->stateY: POSIBLECELLS THEN stateX:=stateX+1 || changeData(stateX+1|->stateY) || message:=MoveSuccess ELSIF stateX+1|->stateY: SHADED THEN message:=MoveFailedDueToAnInternalWall ELSE message:=MoveFailedDueToTheMazeBoundary END);
  List_Substitution(Machine(Robot),MoveWest)==(IF stateX-1|->stateY: POSIBLECELLS THEN stateX:=stateX-1 || changeData(stateX-1|->stateY) || message:=MoveSuccess ELSIF stateX-1|->stateY: SHADED THEN message:=MoveFailedDueToAnInternalWall ELSE message:=MoveFailedDueToTheMazeBoundary END);
  List_Substitution(Machine(Robot),Teleport)==(IF toX|->toY: POSIBLECELLS & toX|->toY/=EXIT THEN stateX:=toX || stateY:=toY || changeData(toX|->toY) || message:=TeleportSuccess ELSIF toX|->toY: SHADED THEN message:=TeleportFailedDueToAnInternalWall ELSIF toX|->toY = EXIT THEN message:=CannotTeleportToExit ELSE message:=TeleportFailedDueToTheMazeBoundary END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Robot))==(GRID,SHADED,POSIBLECELLS,EXIT,START);
  Inherited_List_Constants(Machine(Robot))==(GRID,SHADED,POSIBLECELLS,EXIT,START);
  List_Constants(Machine(Robot))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Robot),ANSWERS)==({Yes,No});
  Context_List_Enumerated(Machine(Robot))==(?);
  Context_List_Defered(Machine(Robot))==(?);
  Context_List_Sets(Machine(Robot))==(?);
  List_Valuable_Sets(Machine(Robot))==(?);
  Inherited_List_Enumerated(Machine(Robot))==(ANSWERS);
  Inherited_List_Defered(Machine(Robot))==(?);
  Inherited_List_Sets(Machine(Robot))==(ANSWERS);
  List_Enumerated(Machine(Robot))==(TOAST_MESSAGE);
  List_Defered(Machine(Robot))==(?);
  List_Sets(Machine(Robot))==(TOAST_MESSAGE);
  Set_Definition(Machine(Robot),TOAST_MESSAGE)==({MoveSuccess,TeleportSuccess,MoveFailedDueToAnInternalWall,TeleportFailedDueToAnInternalWall,MoveFailedDueToTheMazeBoundary,TeleportFailedDueToTheMazeBoundary,CannotTeleportToExit})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Robot))==(?);
  Expanded_List_HiddenConstants(Machine(Robot))==(?);
  List_HiddenConstants(Machine(Robot))==(?);
  External_List_HiddenConstants(Machine(Robot))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Robot))==(btrue);
  Context_List_Properties(Machine(Robot))==(btrue);
  Inherited_List_Properties(Machine(Robot))==(GRID: POW(NATURAL1*NATURAL1) & GRID = {xx,yy | xx: NATURAL1 & yy: NATURAL1 & xx<=7 & yy<=5} & SHADED <: {xx,yy | xx: NATURAL1 & yy: NATURAL1 & xx<=7 & yy<=5} & SHADED = {2|->1,6|->1,4|->2,6|->2,1|->3,2|->3,3|->3,4|->3,4|->4,6|->4,7|->4,2|->5} & POSIBLECELLS = GRID-SHADED & EXIT: POSIBLECELLS & EXIT = 1|->5 & START: POSIBLECELLS & START = 1|->1 & ANSWERS: FIN(INTEGER) & not(ANSWERS = {}));
  List_Properties(Machine(Robot))==(TOAST_MESSAGE: FIN(INTEGER) & not(TOAST_MESSAGE = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Robot),robotsRoute)==(?);
  List_ANY_Var(Machine(Robot),visitedSquare)==(?);
  List_ANY_Var(Machine(Robot),foundExit)==(?);
  List_ANY_Var(Machine(Robot),getPosition)==(?);
  List_ANY_Var(Machine(Robot),MoveSouth)==(?);
  List_ANY_Var(Machine(Robot),MoveNorth)==(?);
  List_ANY_Var(Machine(Robot),MoveEast)==(?);
  List_ANY_Var(Machine(Robot),MoveWest)==(?);
  List_ANY_Var(Machine(Robot),Teleport)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Robot)) == (TOAST_MESSAGE,MoveSuccess,TeleportSuccess,MoveFailedDueToAnInternalWall,TeleportFailedDueToAnInternalWall,MoveFailedDueToTheMazeBoundary,TeleportFailedDueToTheMazeBoundary,CannotTeleportToExit | GRID,SHADED,POSIBLECELLS,EXIT,START,ANSWERS,Yes,No | stateY,stateX | V,visited,cell | MoveSouth,MoveNorth,MoveEast,MoveWest,Teleport | getPosition,foundExit,visitedSquare,robotsRoute | included(Machine(Maze)) | ? | Robot);
  List_Of_HiddenCst_Ids(Machine(Robot)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Robot)) == (GRID,SHADED,POSIBLECELLS,EXIT,START);
  List_Of_VisibleVar_Ids(Machine(Robot)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Robot)) == (?: ?);
  List_Of_Ids(Machine(Maze)) == (GRID,SHADED,POSIBLECELLS,EXIT,START,ANSWERS,Yes,No | ? | visited,cell | ? | changeData,getPosition,foundExit,visitedSquare,robotsRoute | ? | ? | ? | Maze);
  List_Of_HiddenCst_Ids(Machine(Maze)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Maze)) == (GRID,SHADED,POSIBLECELLS,EXIT,START);
  List_Of_VisibleVar_Ids(Machine(Maze)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Maze)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Robot)) == (Type(ANSWERS) == Cst(SetOf(etype(ANSWERS,0,1)));Type(TOAST_MESSAGE) == Cst(SetOf(etype(TOAST_MESSAGE,0,6))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Robot)) == (Type(START) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(EXIT) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(POSIBLECELLS) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(SHADED) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(GRID) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(No) == Cst(etype(ANSWERS,0,1));Type(Yes) == Cst(etype(ANSWERS,0,1));Type(MoveSuccess) == Cst(etype(TOAST_MESSAGE,0,6));Type(TeleportSuccess) == Cst(etype(TOAST_MESSAGE,0,6));Type(MoveFailedDueToAnInternalWall) == Cst(etype(TOAST_MESSAGE,0,6));Type(TeleportFailedDueToAnInternalWall) == Cst(etype(TOAST_MESSAGE,0,6));Type(MoveFailedDueToTheMazeBoundary) == Cst(etype(TOAST_MESSAGE,0,6));Type(TeleportFailedDueToTheMazeBoundary) == Cst(etype(TOAST_MESSAGE,0,6));Type(CannotTeleportToExit) == Cst(etype(TOAST_MESSAGE,0,6)))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Robot)) == (Type(cell) == Mvl(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(visited) == Mvl(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(stateY) == Mvl(btype(INTEGER,?,?));Type(stateX) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Robot)) == (Type(getPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(foundExit) == Cst(etype(ANSWERS,?,?),No_type);Type(visitedSquare) == Cst(etype(ANSWERS,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(robotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(Teleport) == Cst(etype(TOAST_MESSAGE,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MoveWest) == Cst(etype(TOAST_MESSAGE,?,?),No_type);Type(MoveEast) == Cst(etype(TOAST_MESSAGE,?,?),No_type);Type(MoveNorth) == Cst(etype(TOAST_MESSAGE,?,?),No_type);Type(MoveSouth) == Cst(etype(TOAST_MESSAGE,?,?),No_type));
  Observers(Machine(Robot)) == (Type(getPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(foundExit) == Cst(etype(ANSWERS,?,?),No_type);Type(visitedSquare) == Cst(etype(ANSWERS,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(robotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
