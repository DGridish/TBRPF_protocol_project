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
-define(MAX_SPEED, 100).    % m/s
-define(updateEtsTimer, 1). % per second
-record(elementNode_state, {elementPid, parentPid, quarter, location, direction, speed, time}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ParentNode::atom(), Quarter::byte()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ParentNode, Quarter) ->
  {ok, ElementPid} = gen_server:start_link(?MODULE, [ParentNode, Quarter], []),
  UpdateEtsTimerPid = spawn(fun()->updateEtsTimer(ElementPid) end).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #elementNode_state{}} | {ok, State :: #elementNode_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ParentNode, Quarter]) ->
  ElementPid = self(),
  [Location, Direction, Speed] = setSpeedAndDirection(Quarter),
  gen_server:cast(ParentNode, {signMeUp, ElementPid, Location}),                                                                  % Update the parent node on its existence
  {ok, #elementNode_state{elementPid = ElementPid, parentPid = ParentNode, quarter = Quarter,
    location = Location, direction = Direction, speed = Speed, time = erlang:system_time(millisecond)}}.

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
  [NewX, NewY, NewTime] = calcMovement(State),
  NewLocation = [NewX, NewY],
  case checkNewLocation(NewLocation) of
    offTheMap ->
      gen_server:cast(State#elementNode_state.parentPid, {deleteElement, self()}),
      gen_server:cast(self(),{deleteElement});
    NewQuarter -> State#elementNode_state{quarter = NewQuarter, location = NewLocation, time = NewTime},
              if
                (NewQuarter == State#elementNode_state.quarter) ->
                  gen_server:cast(State#elementNode_state.parentPid, {updateElement, State#elementNode_state.elementPid, NewLocation});
                true ->
                  gen_server:cast(State#elementNode_state.parentPid, {moveToOtherQuarter, State#elementNode_state.elementPid, NewQuarter, NewLocation})
              end
  end,
  {noreply, State};

handle_cast({deleteElement}, State = #elementNode_state{}) -> %TODO delete Element
  {{stop, shutdown, State}};

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
    1 -> Location = [rand:uniform(1000), rand:uniform(1000)];
    2 -> Location = [1000 + rand:uniform(1000), rand:uniform(1000)];
    3 -> Location = [rand:uniform(1000), 1000 + rand:uniform(1000)];
    4 -> Location = [1000 + rand:uniform(1000), 1000 + rand:uniform(1000)]
    end,
  Direction = rand:uniform(360),    % degrees
  Speed = rand:uniform(?MAX_SPEED), % m/s
  [Location, Direction, Speed].

updateEtsTimer(ElementPid) ->
  WaitTime = 1000 div ?updateEtsTimer,
  receive
    _ ->  doNothing
  after WaitTime -> % milliseconds
    gen_server:cast(ElementPid,{makeMovement}),
    updateEtsTimer(ElementPid)
  end.

calcMovement(State) ->
  [OldX, OldY] = State#elementNode_state.location,
  Direction = State#elementNode_state.direction,
  Speed = State#elementNode_state.speed,
  NewTime = erlang:system_time(millisecond),
  MovementTime = NewTime - State#elementNode_state.time,

  NewX = round(OldX + math:cos(Direction * math:pi() / 180) * Speed * MovementTime),
  NewY = round(OldY + math:sin(Direction * math:pi() / 180) * Speed * MovementTime),
  [NewX, NewY, NewTime].

checkNewLocation([X,Y]) ->
  if
    ((X < 0) or (X > 2000) or (Y < 0) or (Y > 2000)) -> offTheMap;
    ((X > 0) and (X < 1000) and (Y > 0) and (Y < 1000)) -> 1;
    ((X > 1000) and (X < 2000) and (Y > 0) and (Y < 1000)) -> 2;
    ((X > 0) and (X < 1000) and (Y > 1000) and (Y < 2000)) -> 3;
    ((X > 1000) and (X < 2000) and (Y > 1000) and (Y < 2000)) -> 4;
    true -> offTheMap
  end.