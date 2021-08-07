%%%-------------------------------------------------------------------
%%% @author dgridish
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 14:25
%%%-------------------------------------------------------------------
-module(masterNode).
-author("dgridish").

-behaviour(gen_server).

%% API
-export([start_link/2, test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(NUM_OF_ELEMENTS, 16).
%-define(RefreshRate, 100).

-record(masterNode_state, {slaveNodes, slaveAreas}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(SlaveNodes::list(), SlaveAreas::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SlaveNodes, SlaveAreas) -> io:format("Master start_link~n", []),
  gen_server:start_link({global, node()}, ?MODULE, [SlaveNodes, SlaveAreas], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #masterNode_state{}} | {ok, State :: #masterNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([SlaveNodes, SlaveAreas]) -> io:format("Master init~n", []),
  ets:new(etsElements,[set, public, named_table]),                                                                      % Create elements table
  ets:new(etsMessages,[ordered_set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]),         % Create massages table

  % TODO spawn GUI
  % GuiPid = guiStateM:start_link([SlaveNodes, node()]),
  % spawn_link(fun()-> updateETS(GuiPid) end).

  % TODO pass parameters to slave node or use default in slaveNode? What parameters?

  spawnSlaveNodes(SlaveNodes, SlaveAreas, ?NUM_OF_ELEMENTS, init),                                                      % Spawn slave nodes
  spawn_link(fun() -> manageSlaveNodes(SlaveNodes, node()) end),                                                        % Monitor slave nodes

  % TODO send massages between elements

  {ok, #masterNode_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #masterNode_state{}) ->
  {reply, Reply :: term(), NewState :: #masterNode_state{}} |
  {reply, Reply :: term(), NewState :: #masterNode_state{}, timeout() | hibernate} |
  {noreply, NewState :: #masterNode_state{}} |
  {noreply, NewState :: #masterNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #masterNode_state{}} |
  {stop, Reason :: term(), NewState :: #masterNode_state{}}).
handle_call(_Request, _From, State = #masterNode_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #masterNode_state{}) ->
  {noreply, NewState :: #masterNode_state{}} |
  {noreply, NewState :: #masterNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #masterNode_state{}}).
handle_cast({addElement, SlavePid, ElementPid, Location}, State = #masterNode_state{}) ->
  io:format("Master signMeUp !~p ~n ", [{[SlavePid, ElementPid], Location}]),
  ets:insert(etsElements, {[SlavePid, ElementPid], Location}),   % etsElements: {[SlavePid, ElementPid],[X,Y]}  % {[<12526.105.0>,<12526.113.0>],[1916,114]
  {noreply, State};

handle_cast({deleteElement, SlavePid, ElementPid}, State = #masterNode_state{}) ->
  ets:delete(etsElements, {[SlavePid, ElementPid]}),   % etsElements: {[SlavePid, ElementPid],[X,Y]}  % {[<12526.105.0>,<12526.113.0>],[1916,114]
  {noreply, State};

handle_cast({updateElement, SlavePid, Element, NewLocation}, State = #masterNode_state{}) ->
  ets:delete(etsElements, [SlavePid, Element]),
  ets:insert(etsElements, {[SlavePid, Element], NewLocation}),
  io:format("Master updateElement !~p ~n ", [{[SlavePid, Element], NewLocation}]),
{noreply, State};

handle_cast(_Request, State = #masterNode_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #masterNode_state{}) ->
  {noreply, NewState :: #masterNode_state{}} |
  {noreply, NewState :: #masterNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #masterNode_state{}}).
handle_info(_Info, State = #masterNode_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #masterNode_state{}) -> term()).
terminate(_Reason, _State = #masterNode_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #masterNode_state{},
    Extra :: term()) ->
  {ok, NewState :: #masterNode_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #masterNode_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Start all the processes in the SlaveNodes list, with the appropriate areas
spawnSlaveNodes(SlaveNodes, SlaveAreas, NUM_OF_ELEM, init) -> [spawnSlaveNodes(SlaveNodes, SlaveAreas, NUM_OF_ELEM, Node) || Node <- SlaveNodes];
spawnSlaveNodes(SlaveNodes, SlaveAreas, NUM_OF_ELEM, Node) -> spawn(Node, slaveNode, start_link, [SlaveNodes, SlaveAreas, NUM_OF_ELEM, self()]).

% Monitor all slave nodes
manageSlaveNodes(SlaveNodes, MasterNode) -> io:format("Master MasterNode: ~p ~n", [MasterNode]),
  [erlang:monitor_node(Node, true) || Node <- SlaveNodes],
  manageSlaveNodesLoop(MasterNode).

% Wait here for messages from slave nodes
manageSlaveNodesLoop(MasterNode)->
  receive
    Test -> io:format("Master manageSlaveNodesLoop: ~p ~n", [Test])
  end,
  manageSlaveNodesLoop(MasterNode).


%updateETS(GuiPid)->
%  receive
%  after  1000 div ?RefreshRate -> gen_statem:cast(GuiPid, {refresh, ets:tab2list(etsElements)})
%  end,
%  refreshtimer(GuiPid).

test() -> EtsList = ets:tab2list(etsElements), EtsList.