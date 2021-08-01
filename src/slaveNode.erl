%%%-------------------------------------------------------------------
%%% @author dgridish
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 14:26
%%%-------------------------------------------------------------------
-module(slaveNode).
-author("dgridish").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(slaveNode_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(SlaveNodes::list(), SlaveAreas::list(), MasterNode::atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SlaveNodes, SlaveAreas, MasterNode) -> io:format("Slave start_link ~n ", []),
  {ok, Pid} = gen_server:start_link({global, node()}, ?MODULE, [SlaveNodes, SlaveAreas, MasterNode], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #slaveNode_state{}} | {ok, State :: #slaveNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([SlaveNodes, SlaveAreas, MasterNode]) -> io:format("Slave init ~n ", []),
  ets:new(etsLocation, [set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]),                % Create elements location table
  Quarter = findSlaveNodeQuarter(SlaveNodes, SlaveAreas, node()),                                                       % Find the quarter for which the node is responsible from SlaveAreas list
  io:format("Slave Quarter: ~p - ~p ~n ", [node(), Quarter]),
  % TODO spawn elements
  % TODO manageElements
  {ok, #slaveNode_state{}}.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #slaveNode_state{}) ->
  {reply, Reply :: term(), NewState :: #slaveNode_state{}} |
  {reply, Reply :: term(), NewState :: #slaveNode_state{}, timeout() | hibernate} |
  {noreply, NewState :: #slaveNode_state{}} |
  {noreply, NewState :: #slaveNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #slaveNode_state{}} |
  {stop, Reason :: term(), NewState :: #slaveNode_state{}}).
handle_call(_Request, _From, State = #slaveNode_state{}) ->
  % TODO decide on message types
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #slaveNode_state{}) ->
  {noreply, NewState :: #slaveNode_state{}} |
  {noreply, NewState :: #slaveNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #slaveNode_state{}}).
handle_cast(_Request, State = #slaveNode_state{}) ->
  % TODO decide on message types
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #slaveNode_state{}) ->
  {noreply, NewState :: #slaveNode_state{}} |
  {noreply, NewState :: #slaveNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #slaveNode_state{}}).
handle_info(_Info, State = #slaveNode_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #slaveNode_state{}) -> term()).
terminate(_Reason, _State = #slaveNode_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #slaveNode_state{},
    Extra :: term()) ->
  {ok, NewState :: #slaveNode_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #slaveNode_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% gets My Area from lists of nodes and areas
findSlaveNodeQuarter([], [], _) -> noData;
findSlaveNodeQuarter([Node|_], [Quarter|_], Node) -> Quarter;
findSlaveNodeQuarter([_|T1], [_|T2], Node) -> findSlaveNodeQuarter(T1, T2, Node).