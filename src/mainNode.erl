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
-define(NUM_OF_ELEMENTS, 4).
%-define(RefreshRate, 100).

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
  ets:new(etsQs,[set, public, named_table]),
  ets:new(etsElements,[set, public, named_table]),                                                                      % Create elements table
  ets:new(etsMessages,[ordered_set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]),         % Create massages table

  % TODO spawn GUI
  % GuiPid = guiStateM:start_link([QNodes, node()]),
  % spawn_link(fun()-> updateETS(GuiPid) end).

  % TODO pass parameters to q node or use default in qNode? What parameters?

  spawnQNodes(QNodes, QAreas, ?NUM_OF_ELEMENTS, init),                                                      % Spawn q nodes
  spawn_link(fun() -> manageQNodes(QNodes, node()) end),                                                        % Monitor q nodes

  % TODO send massages between elements

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
handle_call(_Request, _From, State = #mainNode_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mainNode_state{}) ->
  {noreply, NewState :: #mainNode_state{}} |
  {noreply, NewState :: #mainNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #mainNode_state{}}).

handle_cast({allParameters, Parameters}, State = #mainNode_state{}) ->   % Parameters = {NumberOfElements, MaxSpeed, BroadcastRadius}
  EtsQsList = ets:tab2list(etsQs),
  [gen_server:cast(QPid, {allParameters, Parameters}) || {_Quarter, [_Node, QPid]} <- EtsQsList],
  {noreply, State};

handle_cast({addQ, QNode, QPid, Quarter}, State = #mainNode_state{}) ->
  ets:insert(etsQs, {Quarter, [QNode, QPid]}),
  io:format("Send to GUI - etsQs: ~p ~n", [ets:tab2list(etsQs)]),
  {noreply, State};

handle_cast({addElement, QPid, ElementPid, Location}, State = #mainNode_state{}) ->
  ets:insert(etsElements, {[QPid, ElementPid], Location}),   % etsElements: {[QPid, ElementPid],[X,Y]}  % {[<12526.105.0>,<12526.113.0>],[1916,114]
  {noreply, State};

handle_cast({deleteElement, QPid, ElementPid}, State = #mainNode_state{}) ->
  ets:delete(etsElements, [QPid, ElementPid]),   % etsElements: {[QPid, ElementPid],[X,Y]}  % {[<12526.105.0>,<12526.113.0>],[1916,114]
  {noreply, State};

handle_cast({updateElement, QPid, Element, NewLocation}, State = #mainNode_state{}) ->
  ets:delete(etsElements, [QPid, Element]),
  ets:insert(etsElements, {[QPid, Element], NewLocation}),
  io:format("Send to GUI - etsElements: ~p ~n", [ets:tab2list(etsElements)]),
{noreply, State};

handle_cast({moveToOtherQuarter, QPid, ElementPid, NewQuarter, NewLocation, Speed, Direction, Time}, State = #mainNode_state{}) ->
  ets:delete(etsElements, [QPid, ElementPid]),
  QIndex = index_of(NewQuarter, State#mainNode_state.qAreas),
  NewQ = lists:nth(QIndex, State#mainNode_state.qNodes),
  gen_server:cast(NewQ, {createElement, NewLocation, Speed, Direction, Time}),
{noreply, State};

handle_cast({qNodeDown, Node}, State = #mainNode_state{}) ->
  % TODO Transfer all the elements of the fallen node to another node
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
manageQNodes(QNodes, MainNode) -> io:format("Main MainNode: ~p ~n", [MainNode]),
  [erlang:monitor_node(Node, true) || Node <- QNodes],
  manageQNodesLoop(MainNode).

% Wait here for messages from q nodes
manageQNodesLoop(MainNode)->
  receive
    Test -> io:format("Main manageQNodesLoop: ~p ~n", [Test])
    % {nodedown,Node} -> gen_server:cast(MainNode, {qNodeDown, Node})                                                   %{nodedown,q4@dgridish}
  end,
  manageQNodesLoop(MainNode).

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).



%updateETS(GuiPid)->
%  receive
%  after  1000 div ?RefreshRate -> gen_statem:cast(GuiPid, {refresh, ets:tab2list(etsElements)})
%  end,
%  refreshtimer(GuiPid).

test() -> EtsList = ets:tab2list(etsElements), EtsList.