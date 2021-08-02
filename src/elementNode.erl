%%%-------------------------------------------------------------------
%%% @author dgridish
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 14:35
%%%-------------------------------------------------------------------
-module(elementNode).
-author("dgridish").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_SPEED, 100).

-record(elementNode_state, {parent, location, direction, speed}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ParentNode::atom(), Quarter::byte()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ParentNode, Quarter) -> io:format("Element start_link ~n ", []),
  gen_server:start_link(?MODULE, [ParentNode, Quarter], []).


  % TODO Update the parent node on its existence

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #elementNode_state{}} | {ok, State :: #elementNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ParentNode, Quarter]) -> io:format("Element init: ~p ~n ", [self()]),
  ElementPid = self(),
  setSpeedAndDirection(Quarter), % [Location, Direction, Speed]
  %io:format("Element test1 ~p ~n ", [[Loc, Dir, Sp]]),
  %gen_server:cast(ParentNode, {signMeUp, ElementPid}),
  io:format("Element test2 ~n ", []),
  {ok, #elementNode_state{}}. % parent = ParentNode, location = Location, direction = Direction, speed = Speed

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
setSpeedAndDirection(Quarter) -> io:format("Element setSpeedAndDirection ~p ~n ", [Quarter]),
  case Quarter of
    1 -> Location = [rand:uniform(1000), rand:uniform(1000)];
    2 -> Location = [1000 + rand:uniform(1000), rand:uniform(1000)];
    3 -> Location = [rand:uniform(1000), 1000 + rand:uniform(1000)];
    4 -> Location = [1000 + rand:uniform(1000), 1000 + rand:uniform(1000)]
    end,
  io:format("Element 1111111111111111111: ~n ", []),
  Direction = rand:uniform(360),
  Speed = rand:uniform(?MAX_SPEED),

  [Location, Direction, Speed].