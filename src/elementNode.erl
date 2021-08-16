%%%-------------------------------------------------------------------
%%% @author Dan Gridish
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 14:35
%%%-------------------------------------------------------------------
-module(elementNode).
-author("Dan Gridish").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_SPEED, 30).    % m/s
-define(MOVEMENT_TIMER, 3). % 3 second
-define(NEIGHBORS_TIMER, 3). % 3 second
-define(RADIUS, 500).
-record(elementNode_state, {elementPid, parentPid, quarter, location, direction, speed, time, neighbors, diGraph}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(List::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([ParentNode, Quarter]) ->
  {ok, ElementPid} = gen_server:start_link(?MODULE, [ParentNode, Quarter, {0, 0, 0, 0}], []),
  spawn(fun()->movementTimer(ElementPid) end),
  spawn(fun()->neighborsTimer(ElementPid) end);

start_link([ParentNode, Quarter, {NewLocation, Speed, Direction, Time}]) ->
  {ok, ElementPid} = gen_server:start_link( ?MODULE, [ParentNode, Quarter, {NewLocation, Speed, Direction, Time}], []),
  spawn(fun()->movementTimer(ElementPid) end),
  spawn(fun()->neighborsTimer(ElementPid) end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #elementNode_state{}} | {ok, State :: #elementNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ParentNode, Quarter, {NewLocation, NewSpeed, NewDirection, NewTime}]) ->
  ElementPid = self(),
  DiGraph = digraph:new(),
  digraph:add_vertex(DiGraph, ElementPid),
  if {NewLocation, NewSpeed, NewDirection, NewTime} == {0, 0, 0, 0} ->
    [Location, Direction, Speed] = setSpeedAndDirection(Quarter),
    gen_server:cast(ParentNode, {signMeUp, ElementPid, Location}),                                                      % Update the parent node on its existence
  {ok, #elementNode_state{elementPid = ElementPid, parentPid = ParentNode, quarter = Quarter,
    location = Location, direction = Direction, speed = Speed, time = erlang:system_time(millisecond),
    neighbors = [], diGraph = DiGraph}};

  true -> gen_server:cast(ParentNode, {signMeUp, ElementPid, NewLocation}),
    {ok, #elementNode_state{elementPid = ElementPid, parentPid = ParentNode, quarter = Quarter,
    location = NewLocation, direction = NewDirection, speed = NewSpeed, time = NewTime, neighbors = [], diGraph = DiGraph}}
  end.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #elementNode_state{}) ->
  {reply, Reply :: term(), NewState :: #elementNode_state{}} |
  {reply, Reply :: term(), NewState :: #elementNode_state{}, timeout() | hibernate} |
  {noreply, NewState :: #elementNode_state{}} |
  {noreply, NewState :: #elementNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #elementNode_state{}} |
  {stop, Reason :: term(), NewState :: #elementNode_state{}}).
handle_call({sendYourNeighborsList}, _From, State = #elementNode_state{}) ->
  {reply, State#elementNode_state.neighbors, State};

handle_call(_Request, _From, State = #elementNode_state{}) ->
  {reply, ok, State}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #elementNode_state{}) ->
  {noreply, NewState :: #elementNode_state{}} |
  {noreply, NewState :: #elementNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #elementNode_state{}}).
handle_cast({makeMovement}, State = #elementNode_state{}) ->
  OldQuarter = State#elementNode_state.quarter,
  OLdDirection = State#elementNode_state.direction,
  [NewX, NewY, NewTime] = calcMovement(State),
  NewLocation = {NewX, NewY},
  NewQuarter = checkNewLocation(NewLocation),
  case NewQuarter of
    OldQuarter ->  gen_server:cast(State#elementNode_state.parentPid, {updateElement, self(), NewLocation}),
      {noreply, State#elementNode_state{location = NewLocation, time = NewTime}};

    offTheMap -> % billiard table movement
      gen_server:cast(State#elementNode_state.parentPid, {updateElement, self(), NewLocation}),
      if
        (NewX < 200) and (NewY < 200) -> NewDirection = (180 + State#elementNode_state.direction) rem 360;
        (NewX < 200) and (NewY > 1800) -> NewDirection = (180 + State#elementNode_state.direction) rem 360;
        (NewX > 1800) and (NewY < 200) -> NewDirection = (180 + State#elementNode_state.direction) rem 360;
        (NewX > 1800) and (NewY > 1800) -> NewDirection = (180 + State#elementNode_state.direction) rem 360;
        (NewX < 200) -> NewDirection = (540 - State#elementNode_state.direction) rem 360;
        (NewX > 1800) -> NewDirection = (540 + State#elementNode_state.direction) rem 360;
        (NewY < 200) -> NewDirection = (360 - State#elementNode_state.direction) rem 360;
        (NewY > 1800) -> NewDirection = (360 - State#elementNode_state.direction) rem 360;
        true -> NewDirection = OLdDirection
      end,
      {noreply, State#elementNode_state{location = NewLocation, direction = NewDirection, time = NewTime}};

    NewQuarter ->
      gen_server:cast(State#elementNode_state.parentPid, {moveToOtherQuarter, State#elementNode_state.elementPid, NewQuarter, NewLocation,
        State#elementNode_state.speed, State#elementNode_state.direction, State#elementNode_state.time}),
      gen_server:cast(self(), {deleteElement}),
      {noreply, State#elementNode_state{quarter = NewQuarter, location = NewLocation, time = NewTime}}
  end;

handle_cast({getNeighborsList}, State = #elementNode_state{}) ->
  try
    MyQPid = State#elementNode_state.parentPid,
    {X, Y} = State#elementNode_state.location,
    Quarter = State#elementNode_state.quarter,
    MyPid = self(),
    case Quarter of
      1 ->   if
               (X > 1000 - ?RADIUS) and (Y > 0) and (Y < 1000 - ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [2]});
               (X > 0) and (X < 1000 - ?RADIUS) and (Y > 1000 - ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [3]});
               (X > 1000 - ?RADIUS) and (Y > 1000 - ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [2, 3, 4]});
               true -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, []})
             end;
      2 ->   if
               (X > 1000 + ?RADIUS) and (X < 2000) and (Y > 1000 - ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [4]});
               (X < 1000 + ?RADIUS) and (Y > 0) and (Y < 1000 - ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [1]});
               (X < 1000 + ?RADIUS) and (Y > 1000 - ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [1, 3, 4]});
               true -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, []})
             end;
      3 ->   if
               (X > 0) and (X < 1000 - ?RADIUS) and (Y < 1000 + ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [1]});
               (X > 1000 - ?RADIUS) and (Y < 2000) and (Y > 1000 + ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [4]});
               (X > 1000 - ?RADIUS) and (Y < 1000 + ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [1, 2, 4]});
               true -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, []})
             end;
      4 ->   if
               (X > 1000 + ?RADIUS) and (X < 2000) and (Y < 1000 + ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [2]});
               (X < 1000 + ?RADIUS) and (Y < 2000) and (Y > 1000 + ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [3]});
               (X < 1000 + ?RADIUS) and (Y < 1000 + ?RADIUS) -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, [1, 2, 3]});
               true -> gen_server:cast(MyQPid, {giveMeElementList, MyPid, []})
             end
    end,
    {noreply, State}
  catch
      _:_  -> {noreply, State}
  end;

handle_cast({takeElementList, [FullList]}, State = #elementNode_state{}) ->
  MyLocation = State#elementNode_state.location,
  AllInRadius = findElementsInRadius(FullList, MyLocation),
{noreply, State#elementNode_state{neighbors = AllInRadius}};

handle_cast({sendMassage, ToElement, Data}, State = #elementNode_state{}) ->
  MyPid = self(),
  try
    AlreadyVisitedList = State#elementNode_state.neighbors,
    NList = [gen_server:call(Neighbor, {sendYourNeighborsList}, 500) || Neighbor <- AlreadyVisitedList],
    NeighborsList = flatten(NList),
    if
      NeighborsList == [] ->
        List = gen_server:call(State#elementNode_state.parentPid, {sendYourElementList}, 500),
        ListWithoutMyPid = [X || {X,_} <- List, X /= MyPid];
      true -> ListWithoutMyPid = [X || X <- NeighborsList, X /= MyPid]
    end,
    ToDiGraph = ListWithoutMyPid ++ [MyPid],
    DiGraph = digraph:new(),
    [digraph:add_vertex(DiGraph, X) || X <- ToDiGraph],
    [digraph:add_edge(DiGraph, MyPid, X) || X <- ListWithoutMyPid],
    Path = lookForTargetElement(DiGraph, MyPid, ToElement, ListWithoutMyPid),
    [H|_T] = Path,
    gen_server:call(H, {msgFromOtherElement, MyPid, ToElement, Path, Data}),
    {noreply, State}
  catch
    _:_ -> {noreply, State}
  end;

handle_cast({msgFromOtherElement, FromElement, ToElement, Path, Data}, State = #elementNode_state{}) ->
  MyPid = self(),
  if
    MyPid == ToElement -> io:format("Thanks ~p ! I got this data: ~p ~n", [FromElement, Data]);
    true -> [H|T] = Path, gen_server:cast(H, {msgFromOtherElement, FromElement, ToElement, T, Data})
  end,
  {noreply, State};

handle_cast({deleteElement}, State = #elementNode_state{}) ->
  {stop, shutdown, State};

handle_cast(_Request, State = #elementNode_state{}) ->
  {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #elementNode_state{}) ->
  {noreply, NewState :: #elementNode_state{}} |
  {noreply, NewState :: #elementNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #elementNode_state{}}).
handle_info(_Info, State = #elementNode_state{}) ->
  {noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #elementNode_state{}) -> term()).
terminate(_Reason, _State = #elementNode_state{}) ->
  ok.


%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #elementNode_state{},
    Extra :: term()) ->
  {ok, NewState :: #elementNode_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #elementNode_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


setSpeedAndDirection(Quarter) ->
  case Quarter of
    1 -> Location = {300 + rand:uniform(700), 300 + rand:uniform(700)};
    2 -> Location = {1000 + rand:uniform(700), 300 + rand:uniform(700)};
    3 -> Location = {300 + rand:uniform(700), 1000 + rand:uniform(700)};
    4 -> Location = {1000 + rand:uniform(700), 1000 + rand:uniform(700)}
    end,
  Direction = rand:uniform(360),    % degrees
  Speed = rand:uniform(?MAX_SPEED), % m/s
  [Location, Direction, Speed].


movementTimer(ElementPid) ->
  WaitTime = 1000 * ?MOVEMENT_TIMER,
  receive
    _ ->  doNothing
  after (WaitTime + rand:uniform(500)) -> % milliseconds
    gen_server:cast(ElementPid,{makeMovement}),
    movementTimer(ElementPid)
  end.


neighborsTimer(ElementPid) ->
  WaitTime = 1000 * ?NEIGHBORS_TIMER,
  receive
    _ ->  doNothing
  after (WaitTime + rand:uniform(500)) -> % milliseconds
    gen_server:cast(ElementPid,{getNeighborsList}),
    neighborsTimer(ElementPid)
  end.


calcMovement(State) ->
  {OldX, OldY} = State#elementNode_state.location,
  Direction = State#elementNode_state.direction,
  Speed = State#elementNode_state.speed,
  NewTime = erlang:system_time(millisecond),
  MovementTime = NewTime - State#elementNode_state.time,
  NewX = round(OldX + math:cos(Direction * math:pi() / 180) * Speed * (MovementTime / 1000)),
  NewY = round(OldY + math:sin(Direction * math:pi() / 180) * Speed * (MovementTime / 1000)),
  [NewX, NewY, NewTime].


checkNewLocation({X,Y}) ->
  if
    ((X =< 0) or (X >= 2000) or (Y =< 0) or (Y >= 2000)) -> offTheMap;
    ((X > 0) and (X =< 1000) and (Y > 0) and (Y =< 1000)) -> 1;
    ((X >= 1000) and (X < 2000) and (Y > 0) and (Y =< 1000)) -> 2;
    ((X > 0) and (X < 1000) and (Y >= 1000) and (Y < 2000)) -> 3;
    ((X >= 1000) and (X < 2000) and (Y >= 1000) and (Y < 2000)) -> 4;
    true -> offTheMap
  end.


findElementsInRadius([], _MyLocation) -> [];
findElementsInRadius([H|T], {MyX, MyY}) ->
  {ElementPid, {X, Y}} = H,
  Distance = math:sqrt(math:pow((X - MyX), 2) + math:pow((Y - MyY), 2)),
  if
    (ElementPid == self()) -> findElementsInRadius(T, {MyX, MyY});
    true -> if
              Distance =< ?RADIUS -> findElementsInRadiusAcc(T, {MyX, MyY}, [ElementPid]);
              true ->  findElementsInRadius(T, {MyX, MyY})
            end
  end;
findElementsInRadius(_,_) -> [].

findElementsInRadiusAcc([], _MyLocation, InRadiusList) -> InRadiusList;
findElementsInRadiusAcc([H|T], {MyX, MyY}, InRadiusList) ->
  {ElementPid, {X, Y}} = H,
  if
    (ElementPid == self()) -> findElementsInRadiusAcc(T, {MyX, MyY}, InRadiusList);
    true -> Distance = math:sqrt(math:pow((X - MyX), 2) + math:pow((Y - MyY), 2)),
            if
              Distance =< ?RADIUS -> findElementsInRadiusAcc(T, {MyX, MyY}, (InRadiusList ++ [ElementPid]));
              true -> findElementsInRadiusAcc(T, {MyX, MyY}, InRadiusList)
            end
  end.


flatten([]) -> [];
flatten(List) when is_list(List) == false -> [List];
flatten([H|T]) -> flatten(H) ++ flatten(T).


lookForTargetElement(DiGraph, MyPid, ToElement, [H|T]) ->
  Temp = digraph:vertex(DiGraph, ToElement),
  if
    Temp -> digraph:get_short_path(DiGraph, MyPid, ToElement);
    true ->
      HNList = gen_server:call(H, {sendYourNeighborsList}, 500),
      HNeighborsList = flatten(HNList),
      [digraph:add_vertex(DiGraph, X) || X <- HNeighborsList],
      [digraph:add_edge(DiGraph, H, X) || X <- HNeighborsList],
      lookForTargetElement(DiGraph, MyPid, ToElement, T)
  end.