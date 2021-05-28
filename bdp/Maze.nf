Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Maze))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Maze))==(Machine(Maze));
  Level(Machine(Maze))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Maze)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Maze))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Maze))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Maze))==(?);
  List_Includes(Machine(Maze))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Maze))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Maze))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Maze))==(?);
  Context_List_Variables(Machine(Maze))==(?);
  Abstract_List_Variables(Machine(Maze))==(?);
  Local_List_Variables(Machine(Maze))==(visited,square);
  List_Variables(Machine(Maze))==(visited,square);
  External_List_Variables(Machine(Maze))==(visited,square)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Maze))==(?);
  Abstract_List_VisibleVariables(Machine(Maze))==(?);
  External_List_VisibleVariables(Machine(Maze))==(?);
  Expanded_List_VisibleVariables(Machine(Maze))==(?);
  List_VisibleVariables(Machine(Maze))==(?);
  Internal_List_VisibleVariables(Machine(Maze))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Maze))==(btrue);
  Gluing_List_Invariant(Machine(Maze))==(btrue);
  Expanded_List_Invariant(Machine(Maze))==(btrue);
  Abstract_List_Invariant(Machine(Maze))==(btrue);
  Context_List_Invariant(Machine(Maze))==(btrue);
  List_Invariant(Machine(Maze))==(square: PATHSQUARE & visited: seq(INTEGER*INTEGER))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Maze))==(btrue);
  Abstract_List_Assertions(Machine(Maze))==(btrue);
  Context_List_Assertions(Machine(Maze))==(btrue);
  List_Assertions(Machine(Maze))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Maze))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Maze))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Maze))==(square,visited:=START,<>);
  Context_List_Initialisation(Machine(Maze))==(skip);
  List_Initialisation(Machine(Maze))==(square,visited:=START,<>)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Maze))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Maze))==(btrue);
  List_Constraints(Machine(Maze))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Maze))==(changeData,getPosition,foundExit,visitedSquare,robotsRoute);
  List_Operations(Machine(Maze))==(changeData,getPosition,foundExit,visitedSquare,robotsRoute)
END
&
THEORY ListInputX IS
  List_Input(Machine(Maze),changeData)==(currentCell);
  List_Input(Machine(Maze),getPosition)==(?);
  List_Input(Machine(Maze),foundExit)==(?);
  List_Input(Machine(Maze),visitedSquare)==(isVisitedX,isVisitedY);
  List_Input(Machine(Maze),robotsRoute)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Maze),changeData)==(?);
  List_Output(Machine(Maze),getPosition)==(position);
  List_Output(Machine(Maze),foundExit)==(isExit);
  List_Output(Machine(Maze),visitedSquare)==(isVisited);
  List_Output(Machine(Maze),robotsRoute)==(route)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Maze),changeData)==(changeData(currentCell));
  List_Header(Machine(Maze),getPosition)==(position <-- getPosition);
  List_Header(Machine(Maze),foundExit)==(isExit <-- foundExit);
  List_Header(Machine(Maze),visitedSquare)==(isVisited <-- visitedSquare(isVisitedX,isVisitedY));
  List_Header(Machine(Maze),robotsRoute)==(route <-- robotsRoute)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Maze),changeData)==(currentCell: PATHSQUARE);
  List_Precondition(Machine(Maze),getPosition)==(btrue);
  List_Precondition(Machine(Maze),foundExit)==(btrue);
  List_Precondition(Machine(Maze),visitedSquare)==(isVisitedX: dom(PATHSQUARE) & isVisitedY: ran(PATHSQUARE) & isVisitedX|->isVisitedY: PATHSQUARE);
  List_Precondition(Machine(Maze),robotsRoute)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Maze),robotsRoute)==(btrue | route:=visited);
  Expanded_List_Substitution(Machine(Maze),visitedSquare)==(isVisitedX: dom(PATHSQUARE) & isVisitedY: ran(PATHSQUARE) & isVisitedX|->isVisitedY: PATHSQUARE | isVisitedX|->isVisitedY: ran(visited) ==> isVisited:=Yes [] not(isVisitedX|->isVisitedY: ran(visited)) ==> isVisited:=No);
  Expanded_List_Substitution(Machine(Maze),foundExit)==(btrue | square = EXIT ==> isExit:=Yes [] not(square = EXIT) ==> isExit:=No);
  Expanded_List_Substitution(Machine(Maze),getPosition)==(btrue | position:=square);
  Expanded_List_Substitution(Machine(Maze),changeData)==(currentCell: PATHSQUARE | square,visited:=currentCell,visited<-square);
  List_Substitution(Machine(Maze),changeData)==(square:=currentCell || visited:=visited<-square);
  List_Substitution(Machine(Maze),getPosition)==(position:=square);
  List_Substitution(Machine(Maze),foundExit)==(IF square = EXIT THEN isExit:=Yes ELSE isExit:=No END);
  List_Substitution(Machine(Maze),visitedSquare)==(IF isVisitedX|->isVisitedY: ran(visited) THEN isVisited:=Yes ELSE isVisited:=No END);
  List_Substitution(Machine(Maze),robotsRoute)==(route:=visited)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Maze))==(GRID,RESTRICTED,PATHSQUARE,EXIT,START);
  Inherited_List_Constants(Machine(Maze))==(?);
  List_Constants(Machine(Maze))==(GRID,RESTRICTED,PATHSQUARE,EXIT,START)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Maze),ANSWERS)==({Yes,No});
  Context_List_Enumerated(Machine(Maze))==(?);
  Context_List_Defered(Machine(Maze))==(?);
  Context_List_Sets(Machine(Maze))==(?);
  List_Valuable_Sets(Machine(Maze))==(?);
  Inherited_List_Enumerated(Machine(Maze))==(?);
  Inherited_List_Defered(Machine(Maze))==(?);
  Inherited_List_Sets(Machine(Maze))==(?);
  List_Enumerated(Machine(Maze))==(ANSWERS);
  List_Defered(Machine(Maze))==(?);
  List_Sets(Machine(Maze))==(ANSWERS)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Maze))==(?);
  Expanded_List_HiddenConstants(Machine(Maze))==(?);
  List_HiddenConstants(Machine(Maze))==(?);
  External_List_HiddenConstants(Machine(Maze))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Maze))==(btrue);
  Context_List_Properties(Machine(Maze))==(btrue);
  Inherited_List_Properties(Machine(Maze))==(btrue);
  List_Properties(Machine(Maze))==(GRID: POW(NATURAL1*NATURAL1) & GRID = {xx,yy | xx: NATURAL1 & yy: NATURAL1 & xx<=7 & yy<=5} & RESTRICTED <: {xx,yy | xx: NATURAL1 & yy: NATURAL1 & xx<=7 & yy<=5} & RESTRICTED = {2|->1,6|->1,4|->2,6|->2,1|->3,2|->3,3|->3,4|->3,4|->4,6|->4,7|->4,2|->5} & PATHSQUARE = GRID-RESTRICTED & EXIT: PATHSQUARE & EXIT = 1|->5 & START: PATHSQUARE & START = 1|->1 & ANSWERS: FIN(INTEGER) & not(ANSWERS = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Maze),changeData)==(?);
  List_ANY_Var(Machine(Maze),getPosition)==(?);
  List_ANY_Var(Machine(Maze),foundExit)==(?);
  List_ANY_Var(Machine(Maze),visitedSquare)==(?);
  List_ANY_Var(Machine(Maze),robotsRoute)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Maze)) == (GRID,RESTRICTED,PATHSQUARE,EXIT,START,ANSWERS,Yes,No | ? | visited,square | ? | changeData,getPosition,foundExit,visitedSquare,robotsRoute | ? | ? | ? | Maze);
  List_Of_HiddenCst_Ids(Machine(Maze)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Maze)) == (GRID,RESTRICTED,PATHSQUARE,EXIT,START);
  List_Of_VisibleVar_Ids(Machine(Maze)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Maze)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Maze)) == (Type(ANSWERS) == Cst(SetOf(etype(ANSWERS,0,1))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Maze)) == (Type(Yes) == Cst(etype(ANSWERS,0,1));Type(No) == Cst(etype(ANSWERS,0,1));Type(GRID) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(RESTRICTED) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(PATHSQUARE) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(EXIT) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(START) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Maze)) == (Type(visited) == Mvl(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(square) == Mvl(btype(INTEGER,?,?)*btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Maze)) == (Type(robotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(visitedSquare) == Cst(etype(ANSWERS,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(foundExit) == Cst(etype(ANSWERS,?,?),No_type);Type(getPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(changeData) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  Observers(Machine(Maze)) == (Type(robotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(visitedSquare) == Cst(etype(ANSWERS,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(foundExit) == Cst(etype(ANSWERS,?,?),No_type);Type(getPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type))
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
