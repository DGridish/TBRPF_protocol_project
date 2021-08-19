%%%-------------------------------------------------------------------
%%% @author Dan Gridish
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 14:25
%%%-------------------------------------------------------------------
-module(mainNode).
-author("Dan Gridish").

-behaviour(gen_server).

%% API
-export([start_link/2, test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(NUM_OF_ELEMENTS, 16).
-define(sendMassageTimer, 1). % per 5 seconds
-define(MAX_SPEED, 20).

-record(mainNode_state, {qNodes, qAreas}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(QNodes::list(), QAreas::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(QNodes, QAreas) ->
  gen_server:start_link({global, node()}, ?MODULE, [QNodes, QAreas], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #mainNode_state{}} | {ok, State :: #mainNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([QNodes, QAreas]) ->
  TimeStart = erlang:timestamp(),
  MainNodePid = self(),
  ets:new(etsQs,[set, public, named_table]),                                                                            % Create Q node table
  ets:new(etsElements,[ordered_set, public, named_table]),                                                              % Create elements table
  spawn(fun()->sendMassagesRoutine(MainNodePid) end),                                                                   % Send massages routine
  spawnQNodes(QNodes, QAreas, ?NUM_OF_ELEMENTS, init),                                                                  % Spawn q nodes
  spawn_link(fun() -> manageQNodes(QNodes, MainNodePid) end),                                                           % Monitor q nodes
  spawn_link(fun()-> sendDataToGui(TimeStart) end),                                                                     % update gui routine

  % TODO spawn GUI
  % GuiPid = guiStateM:start_link([QNodes, node()]),
  % spawn_link(fun()-> sendDataToGui(GuiPid) end).
  {ok, #mainNode_state{qNodes = QNodes, qAreas = QAreas}}.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mainNode_state{}) ->
  {reply, Reply :: term(), NewState :: #mainNode_state{}} |
  {reply, Reply :: term(), NewState :: #mainNode_state{}, timeout() | hibernate} |
  {noreply, NewState :: #mainNode_state{}} |
  {noreply, NewState :: #mainNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #mainNode_state{}} |
  {stop, Reason :: term(), NewState :: #mainNode_state{}}).
handle_call({howAreThey, QuarterNumbers}, _From, State = #mainNode_state{}) ->                                          % Make a match between a quarter number and an Q node pid
  Temp = [ets:match(etsQs,{Key,'$1'}) || Key <- QuarterNumbers],
  PidsList = flatten(Temp),
  {reply, PidsList, State};

handle_call(_Request, _From, State = #mainNode_state{}) ->
  {reply, ok, State}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mainNode_state{}) ->
  {noreply, NewState :: #mainNode_state{}} |
  {noreply, NewState :: #mainNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #mainNode_state{}}).

handle_cast({addQ, QNode, QPid, Quarter}, State = #mainNode_state{}) ->                                                 % Add Q node to estQs table
  ets:insert(etsQs, {Quarter, {QNode, QPid}}),
  {noreply, State};

handle_cast({addElement, QPid, ElementPid, Location}, State = #mainNode_state{}) ->                                     % Add element to estElements table
  ets:insert(etsElements, {{QPid, ElementPid}, Location}),
  {noreply, State};

handle_cast({deleteElement, QPid, ElementPid}, State = #mainNode_state{}) ->                                            % Delete element from estElements table
  ets:delete(etsElements, {QPid, ElementPid}),
  {noreply, State};

handle_cast({updateElement, QPid, Element, NewLocation}, State = #mainNode_state{}) ->                                  % Update element in estElements table
  ets:delete(etsElements, {QPid, Element}),
  ets:insert(etsElements, {{QPid, Element}, NewLocation}),
{noreply, State};

handle_cast({moveToOtherQuarter, QPid, ElementPid, NewQuarter, NewLocation, Speed, Direction, Time, STime}, State = #mainNode_state{}) -> % Move element to another quarter
  ets:delete(etsElements, {QPid, ElementPid}),
  QIndex = index_of(NewQuarter, State#mainNode_state.qAreas),
  NewQ = lists:nth(QIndex, State#mainNode_state.qNodes),
  gen_server:cast(NewQ, {createElement, NewLocation, Speed, Direction, Time}),
  TimeEnd = erlang:timestamp(),
  T = timer:now_diff(TimeEnd, STime),
  io:format("Main node - The time it took once the step was calculated to complete the transition to a new quarter: ~n ElementPid: ~p, Time: ~p ~n", [ElementPid, T]),
{noreply, State};

handle_cast({sendMassage}, State = #mainNode_state{}) ->                                                                % Select two elements from etsElements table and request to send a message with Data between them
  Data = rand:uniform(1000),
  From = ets:first(etsElements),
  To = ets:last(etsElements),
  {FromQPid, FromElement} = From,
  {ToQPid, ToElement} = To,
  gen_server:cast(FromElement, {sendMassage, ToElement, Data}),
  io:format("MainNode sendMassage ~p ~n", [[FromQPid, FromElement, ToQPid, ToElement, Data]]),
{noreply, State};

handle_cast({qNodeDown, Node}, State = #mainNode_state{}) ->                                                            % Handling of fallen Q nodes
  EtsQsList = ets:tab2list(etsQs),
  [QPid, QQuarterList] = howIsHe(Node, EtsQsList),
  [ets:delete(etsQs, QQuarter) || QQuarter <- QQuarterList],
  EtsElements = ets:tab2list(etsElements),
  LocationList = giveMeLocationList(QPid, EtsElements),
  Quarter = ets:first(etsQs),
  [{_Quarter,{QNode,BackUpQPid}}] = ets:lookup(etsQs, Quarter),
  [ets:insert(etsQs, {QQuarter, {QNode, BackUpQPid}}) || QQuarter <- QQuarterList],
  Time = erlang:system_time(millisecond),
  Direction = rand:uniform(360),
  Speed = rand:uniform(?MAX_SPEED),
  [gen_server:cast(BackUpQPid, {createElement, Location, Speed, Direction, Time}) || Location <- LocationList],
  io:format("Q node ~p is down ~nQ node ~p will back him up ~n", [Node, QNode]),
{noreply, State};

handle_cast({allParameters, Parameters}, State = #mainNode_state{}) ->                                                  % Transfer parameters from the user
  EtsQsList = ets:tab2list(etsQs),
  [gen_server:cast(QPid, {allParameters, Parameters}) || {_Quarter, [_Node, QPid]} <- EtsQsList],
  {noreply, State};

handle_cast(_Request, State = #mainNode_state{}) ->
  {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mainNode_state{}) ->
  {noreply, NewState :: #mainNode_state{}} |
  {noreply, NewState :: #mainNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #mainNode_state{}}).
handle_info(_Info, State = #mainNode_state{}) ->
  {noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mainNode_state{}) -> term()).
terminate(_Reason, _State = #mainNode_state{}) ->
  ok.


%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mainNode_state{},
    Extra :: term()) ->
  {ok, NewState :: #mainNode_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mainNode_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Start all the processes in the qNodes list, with the appropriate areas
spawnQNodes(QNodes, QAreas, NUM_OF_ELEM, init) -> [spawnQNodes(QNodes, QAreas, NUM_OF_ELEM, Node) || Node <- QNodes];
spawnQNodes(QNodes, QAreas, NUM_OF_ELEM, Node) -> spawn(Node, qNode, start_link, [QNodes, QAreas, NUM_OF_ELEM, self()]).

% Monitor all q nodes
manageQNodes(QNodes, MainNodePid) ->
  [erlang:monitor_node(Node, true) || Node <- QNodes],
  manageQNodesLoop(MainNodePid).

% Wait here for messages from q nodes
manageQNodesLoop(MainNodePid)->
  receive
    {nodedown,Node} -> gen_server:cast(MainNodePid, {qNodeDown, Node});
    _ -> donothing
  end,
  manageQNodesLoop(MainNodePid).


index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


flatten([]) -> [];
flatten(List) when is_list(List) == false -> [List];
flatten([H|T]) -> flatten(H) ++ flatten(T).


howIsHe(Node, [H|T]) ->
  {QQuarter,{QNode,QPid}} = H,
  if
    Node == QNode -> howIsHeAcc(Node, T, [QPid, [QQuarter]]);
    true -> howIsHe(Node, T)
  end.

howIsHeAcc(_Node, [], QuarterList) -> QuarterList;
howIsHeAcc(Node, [H|T], [QPid, QuarterList]) ->
  {QQuarter,{QNode,_QPid}} = H,
  if
    Node == QNode -> howIsHeAcc(Node, T, [QPid, QuarterList ++ [QQuarter]]);
    true -> howIsHeAcc(Node, T, [QPid, QuarterList])
  end.


giveMeLocationList(_DownPid, []) -> [];
giveMeLocationList(DownPid, [H|T]) ->
  {{QPid, ElementPid}, Location} = H,
  if
    DownPid == QPid ->
      ets:delete(etsElements, {DownPid, ElementPid}),
      giveMeLocationListAcc(DownPid, T, [Location]);
    true -> giveMeLocationList(DownPid, T)
  end.

giveMeLocationListAcc(_DownPid, [], LocationList) -> LocationList;
giveMeLocationListAcc(DownPid, [H|T], LocationList) ->
  {{QPid, ElementPid}, Location} = H,
  if
    DownPid == QPid ->
      ets:delete(etsElements, {DownPid, ElementPid}),
      giveMeLocationListAcc(DownPid, T, LocationList ++ [Location]);
    true -> giveMeLocationListAcc(DownPid, T, LocationList)
  end.


sendMassagesRoutine(MainNodePid) ->
  try
    receive after 5000  -> % 5 seconds
      %gen_server:cast(MainNodePid, {sendMassage}),
      sendMassagesRoutine(MainNodePid)
    end
  catch
    _ : _ -> sendMassagesRoutine(MainNodePid)
  end.

sendDataToGui(TimeStart)->
  try
    receive after 3000  -> % 3 seconds

      io:format("Send to GUI - etsQs: ~p ~n", [ets:tab2list(etsQs)]),
      io:format("Send to GUI - etsElements: ~p ~n", [ets:tab2list(etsElements)]),
      TimeEnd = erlang:timestamp(),
      Time = timer:now_diff(TimeEnd, TimeStart),
      io:format("Time : ~p ~n", [Time]),
      sendDataToGui(TimeStart)
    end
  catch
    _ : _ -> sendDataToGui(TimeStart)
  end.



test() -> EtsList = ets:tab2list(etsElements), EtsList.