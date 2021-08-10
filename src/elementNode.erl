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
-define(MOVEMENT_TIMER, 1). % per second
-define(NEIGHBORS_TIMER, 5). % per second
-define(RADIOS, 500).
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
  if {NewLocation, NewSpeed, NewDirection, NewTime} == {0, 0, 0, 0} ->
    [Location, Direction, Speed] = setSpeedAndDirection(Quarter),
    gen_server:cast(ParentNode, {signMeUp, ElementPid, Location}),                                                      % Update the parent node on its existence
  {ok, #elementNode_state{elementPid = ElementPid, parentPid = ParentNode, quarter = Quarter,
    location = Location, direction = Direction, speed = Speed, time = erlang:system_time(millisecond)}};

  true -> gen_server:cast(ParentNode, {signMeUp, ElementPid, NewLocation}),
    {ok, #elementNode_state{elementPid = ElementPid, parentPid = ParentNode, quarter = Quarter,
    location = NewLocation, direction = NewDirection, speed = NewSpeed, time = NewTime}}
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
  NewLocation = {NewX, NewY},                                       % TODO tuple
  NewQuarter = checkNewLocation(NewLocation),
  case NewQuarter of
    OldQuarter ->  gen_server:cast(State#elementNode_state.parentPid, {updateElement, self(), NewLocation}),
      {noreply, State#elementNode_state{location = NewLocation, time = NewTime}};

    offTheMap -> % billiard table movement
      gen_server:cast(State#elementNode_state.parentPid, {updateElement, self(), NewLocation}),
      if % TODO What happens if you cross both boundaries?
        NewX < 0 -> NewDirection = (2 * 270 - State#elementNode_state.direction) rem 360;
        NewX > 2000 -> NewDirection = (180 + State#elementNode_state.direction) rem 360;
        NewY < 0 -> NewDirection = (360 - State#elementNode_state.direction) rem 360;
        NewY > 2000 -> NewDirection = (180 + State#elementNode_state.direction) rem 360;
        true -> NewDirection = OLdDirection
      end,
      %io:format("PID, OLdDirection, NewDirection, OldQuarter, NewQuarter : ~p ~n", [[self(), OLdDirection, NewDirection, OldQuarter, NewQuarter]]),
      {noreply, State#elementNode_state{location = NewLocation, direction = NewDirection, time = NewTime}};

    NewQuarter -> % io:format("NewQuarter : ~p ~n", [[self(),NewQuarter]]),
      gen_server:cast(State#elementNode_state.parentPid, {moveToOtherQuarter, State#elementNode_state.elementPid, NewQuarter, NewLocation,
        State#elementNode_state.speed, State#elementNode_state.direction, State#elementNode_state.time}),
      gen_server:cast(self(), {deleteElement}),
      {noreply, State#elementNode_state{quarter = NewQuarter, location = NewLocation, time = NewTime}}
  end;

handle_cast({getNeighborsList}, State = #elementNode_state{}) ->
  MyQPid = State#elementNode_state.parentPid,
  {X, Y} = State#elementNode_state.location,
  Quarter = State#elementNode_state.quarter,
  case Quarter of
    1 ->   if
             (X > 1000 - ?RADIOS) and Y > 0 and (Y < 1000 - ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [2]});
             X > 0 and (X < 1000 - ?RADIOS) and (Y > 1000 - ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [3]});
             (X > 1000 - ?RADIOS) and (Y > 1000 - ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [2, 3, 4]});
             true -> gen_server:cast(MyQPid, {giveMeElementList, self(), []})
           end;
    2 ->   if
             (X > 1000 + ?RADIOS) and X < 2000 and (Y > 1000 - ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [4]});
             (X < 1000 + ?RADIOS) and Y > 0 and (Y < 1000 - ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [1]});
             (X < 1000 + ?RADIOS) and (Y > 1000 - ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [1, 3, 4]});
             true -> gen_server:cast(MyQPid, {giveMeElementList, self(), []})
           end;
    3 ->   if
             X > 0 and (X < 1000 - ?RADIOS) and (Y < 1000 + ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [1]});
             (X > 1000 - ?RADIOS) and Y < 2000 and (Y > 1000 + ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [4]});
             (X > 1000 - ?RADIOS) and (Y < 1000 + ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [1, 2, 4]});
             true -> gen_server:cast(MyQPid, {giveMeElementList, self(), []})
           end,io:format("ElementNode getNeighborsList: ~p ~n", [[Quarter,X,Y]]);
    4 ->   if
             (X < 1000 + ?RADIOS) and X < 2000 and (Y < 1000 + ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [2]});
             (X > 1000 - ?RADIOS) and Y < 2000 and (Y > 1000 + ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [3]});
             (X < 1000 + ?RADIOS) and (Y < 1000 + ?RADIOS) -> gen_server:cast(MyQPid, {giveMeElementList, self(), [1, 2, 3]});
             true -> gen_server:cast(MyQPid, {giveMeElementList, self(), []})
           end
  end,
  {noreply, State};

handle_cast({takeElementList, FullList}, State = #elementNode_state{}) ->
  io:format("ElementNode takeElementList: ~p ~n", [[self(), FullList]]),
  %AllInRadius = fineElementsInRadius(FullList),

  %Path = digraph:get_short_path(diGraph, FromPid, ToElement),
  %io:format("findPath Path: ~p ~n", [Path]),


{noreply, State};

%%handle_cast({sendMassage, ToElement, Data}, State = #elementNode_state{}) ->
%%  MyPid = self(),
%%  try
%%    Path = gen_server:call(Tbrpf, {findPath, ToElement}),
%%    io:format("elementNode sendMassage Path: ~p ~n", [Path]),
%%    if
%%      Path == [] -> io:format("There is no path ~n", []);
%%      true -> [H|T] = Path, gen_server:cast(H, {msgFromOtherElement, MyPid, ToElement, T, Data})
%%    end,
%%    {noreply, State}
%%  catch
%%      X:Y  -> io:format("elementNode sendMassage Error: ~p ~n", [[X,Y]]),
%%      {noreply, State}
%%  end;


handle_cast({msgFromOtherElement, FromElement, ToElement, Path, Data}, State = #elementNode_state{}) ->
  MyPid = self(),
  if
    MyPid == ToElement -> io:format("Thanks ~p ! I got this data: ~p ~n", [FromElement, Data]);
    true -> [H|T] = Path, gen_server:cast(H, {msgFromOtherElement, FromElement, ToElement, T, Data})
  end,
  {noreply, State};

handle_cast({deleteElement}, State = #elementNode_state{}) -> %TODO How to completely delete an element
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
    1 -> Location = {rand:uniform(1000), rand:uniform(1000)};   % 1 -> Location = {rand:uniform(1000), rand:uniform(1000)};
    2 -> Location = {1000 + rand:uniform(150), rand:uniform(150)};    %2 -> Location = {1000 + rand:uniform(1000), rand:uniform(1000)};
    3 -> Location = {rand:uniform(1000), 1000 + rand:uniform(1000)};    % 3 -> Location = {rand:uniform(1000), 1000 + rand:uniform(1000)};
    4 -> Location = {1000 + rand:uniform(150), 1000 + rand:uniform(150)} %  4 -> Location = {1000 + rand:uniform(1000), 1000 + rand:uniform(1000)}
    end,
  Direction = 45,    % degrees                     % Direction = rand:uniform(360),
  Speed = rand:uniform(?MAX_SPEED), % m/s
  [Location, Direction, Speed].

movementTimer(ElementPid) ->
  WaitTime = 1000 * ?MOVEMENT_TIMER,
  receive
    _ ->  doNothing
  after WaitTime -> % milliseconds
    gen_server:cast(ElementPid,{makeMovement}),
    movementTimer(ElementPid)
  end.

neighborsTimer(ElementPid) ->
  WaitTime = 1000 * ?NEIGHBORS_TIMER,
  receive
    _ ->  doNothing
  after WaitTime -> % milliseconds
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
    true -> io:format("checkNewLocation Bug ~p ~n", [[X,Y]])
  end.

%%buildDigraph(DiGraph, []) -> connectDiGraph(DiGraph);
%%buildDigraph(DiGraph, [H|T])->
%%  {{QPid, ElementPid},{X, Y}} = H,
%%  ElementVertex = digraph:add_vertex(DiGraph, H, ElementPid),
%%  buildDigraph(DiGraph, T).
%%
%%
%%connectDiGraph(DiGraph) ->
%%  AllVertices = digraph:vertices(DiGraph),
%%  io:format("DiGraph: ~p ~n", [DiGraph]),
%%  io:format("AllVertices: ~p ~n", [AllVertices]),
%%  DiGraph.