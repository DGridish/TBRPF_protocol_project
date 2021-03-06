%%%-------------------------------------------------------------------
%%% @author Dan Gridish
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 14:26
%%%-------------------------------------------------------------------
-module(qNode).
-author("Dan Gridish").

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(qNode_state, {mainNode, quarter}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(QNodes::list(), QAreas::list(), NUM_OF_ELEM::byte(), MainNode::atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(QNodes, QAreas, NUM_OF_ELEM, MainNode) ->
  gen_server:start_link({global, node()}, ?MODULE, [QNodes, QAreas, NUM_OF_ELEM, MainNode], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #qNode_state{}} | {ok, State :: #qNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([QNodes, QAreas, NUM_OF_ELEM, MainNode]) ->
  ets:new(etsLocation, [set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]),                % Create elements location table
  Quarter = findQNodeQuarter(QNodes, QAreas, node()),                                                                   % Find the quarter for which the node is responsible from QAreas list
  gen_server:cast(MainNode, {addQ, node(), self(), Quarter}),                                                           % Add Q node to estQs table
  spawnElementNodes(NUM_OF_ELEM, Quarter),                                                                              % Spawn all elements
  {ok, #qNode_state{mainNode = MainNode, quarter = Quarter}}.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #qNode_state{}) ->
  {reply, Reply :: term(), NewState :: #qNode_state{}} |
  {reply, Reply :: term(), NewState :: #qNode_state{}, timeout() | hibernate} |
  {noreply, NewState :: #qNode_state{}} |
  {noreply, NewState :: #qNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #qNode_state{}} |
  {stop, Reason :: term(), NewState :: #qNode_state{}}).
handle_call({sendYourElementList}, _From, State = #qNode_state{}) ->
  ElementList = ets:tab2list(etsLocation),
{reply, ElementList, State};

handle_call(_Request, _From, State = #qNode_state{}) ->
  {reply, ok, State}.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #qNode_state{}) ->
  {noreply, NewState :: #qNode_state{}} |
  {noreply, NewState :: #qNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #qNode_state{}}).

handle_cast({signMeUp, ElementPid, Location, Time}, State = #qNode_state{}) ->                                          % Write a new element in the relevant places
  QPid = self(),
  erlang:monitor(process, ElementPid),
  gen_server:cast(State#qNode_state.mainNode, {addElement, QPid, ElementPid, Location}),
  ets:insert(etsLocation, {ElementPid, Location}),
  TimeEnd = erlang:timestamp(),
  T = timer:now_diff(TimeEnd, Time),
  io:format("The time it took between the initialization of the element and its registration in the etsLocation table: ~n ElementPid: ~p, Time: ~p ~n", [ElementPid, T]),
  {noreply, State};

handle_cast({updateElement, Element, NewLocation}, State = #qNode_state{}) ->                                           % Update element in etsLocation table
  ets:delete(etsLocation, Element),
  ets:insert(etsLocation, {Element, NewLocation}),
  gen_server:cast(State#qNode_state.mainNode, {updateElement, self(), Element, NewLocation}),
  {noreply, State};

handle_cast({deleteElement, Element}, State = #qNode_state{}) ->                                                        % Delete element from etsLocation table
  ets:delete(etsLocation, Element),
  gen_server:cast(State#qNode_state.mainNode, {deleteElement, self(), Element}),
  {noreply, State};

handle_cast({moveToOtherQuarter, ElementPid, NewQuarter, NewLocation, Speed, Direction, Time, STime}, State = #qNode_state{}) -> % Move element to another quarter
  gen_server:cast(State#qNode_state.mainNode, {moveToOtherQuarter, self(), ElementPid, NewQuarter, NewLocation, Speed, Direction, Time, STime}),
  ets:delete(etsLocation, ElementPid),
  NodeAndPid = gen_server:call(State#qNode_state.mainNode, {howAreThey, [NewQuarter]}),
  [{_QNode, QPid}] = NodeAndPid,
  gen_server:cast(QPid, {createElement, NewLocation, Speed, Direction, Time}),
  TimeEnd = erlang:timestamp(),
  T = timer:now_diff(TimeEnd, STime),
  io:format("Q node - The time it took once the step was calculated to complete the transition to a new quarter: ~n ElementPid: ~p, Time: ~p ~n", [ElementPid, T]),
  {noreply, State};

handle_cast({createElement, NewLocation, Speed, Direction, Time}, State = #qNode_state{}) ->                            % Create a new element
  spawn(elementNode, start_link, [[self(), State#qNode_state.quarter, {NewLocation, Speed, Direction, Time}]]),
  {noreply, State};

handle_cast({giveMeElementList, ElementPid, []}, State = #qNode_state{}) ->                                             % Request a list of elements from the relevant Q nodes
  FullList = [ets:tab2list(etsLocation)],
  gen_server:cast(ElementPid, {takeElementList, FullList}),
  {noreply, State};

handle_cast({giveMeElementList, ElementPid, HowToAskList}, State = #qNode_state{}) ->                                   % Request a list of elements from the relevant Q nodes
  MyQPid = self(),
  try
    NodeAndPidList = gen_server:call(State#qNode_state.mainNode, {howAreThey, HowToAskList}),
    OElement = [gen_server:call(Pid, {sendYourElementList}, 500) || {_Node, Pid} <- NodeAndPidList, Pid /= MyQPid],
    OtherElement = toOneList(OElement),
    MyElements = ets:tab2list(etsLocation),
    AllElement = MyElements ++ OtherElement,
    gen_server:cast(ElementPid, {takeElementList, AllElement}),
    {noreply, State}
  catch
      _A:_B  -> %io:format("qNode giveMeElementList Error: ~p ~n", [[MyQPid,A,B,ElementPid]]),
                gen_server:cast(ElementPid, {takeElementList, ets:tab2list(etsLocation)}),
                {noreply, State}
  end;

handle_cast({allParameters, _Parameters}, State = #qNode_state{}) ->
  % TODO send parameters - Re-creation of elements with new parameters or Updating existing elements and creating additional elements
  {noreply, State};

handle_cast(_Request, State = #qNode_state{}) ->
  {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #qNode_state{}) ->
  {noreply, NewState :: #qNode_state{}} |
  {noreply, NewState :: #qNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #qNode_state{}}).
handle_info(_Info, State = #qNode_state{}) ->
  {noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #qNode_state{}) -> term()).
terminate(_Reason, _State = #qNode_state{}) ->
  ok.


%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #qNode_state{},
    Extra :: term()) ->
  {ok, NewState :: #qNode_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #qNode_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% gets My Area from lists of nodes and areas
findQNodeQuarter([], [], _Node) -> emptyLists;
findQNodeQuarter([Node|_T1], [Quarter|_T2], Node) -> Quarter;
findQNodeQuarter([_H1|T1], [_H2|T2], Node) -> findQNodeQuarter(T1, T2, Node).


spawnElementNodes(NUM_OF_ELEM, Quarter) ->
  QPid = self(),
  ElementsNumbers = lists:seq(1, NUM_OF_ELEM div 4),
  [spawn(elementNode, start_link, [[QPid, Quarter]])|| _OneByOne <- ElementsNumbers].


toOneList([[]]) -> [];
toOneList([[]|T]) -> toOneList(T);
toOneList([H|T]) ->
  OneList = H,
  toOneListAcc(T, OneList).


toOneListAcc([[]], OneList) -> OneList;
toOneListAcc([[]|T], OneList) -> toOneListAcc(T,OneList);
toOneListAcc([], OneList) -> OneList;
toOneListAcc([H|T], List) ->
  OneList = H ++ List,
  toOneListAcc(T, OneList).
