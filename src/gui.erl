%%%-------------------------------------------------------------------
%%% @author dgridish
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 14:36
%%%-------------------------------------------------------------------
-module(gui).
-author("dgridish").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).
  
-include_lib("wx/include/wx.hrl").
-define(max_x,(600)).
-define(max_y,(600)).



-define(SERVER, ?MODULE).

-record(gui_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  Wx = wx:new(),
  wx_object:start(?MODULE, [Wx], []),
  Frame = wxFrame:new(Wx, 1, "Welcome To TBRPF Protocol Project", [{size, {?max_x+600, ?max_y+100}}]),%Creates the main frame for the gui
  Panel = wxPanel:new(Frame, [{size, {?max_x,?max_y-100}}]), % a panel we will split
  Board = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE},{size, {?max_x+200,?max_y+200}}]), %the board to print on the elemints
  MainSizer = wxBoxSizer:new(?wxVERTICAL),      %main sizer for alignment within the panel
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Our Project"}]), %inside frame 
  StartText = wxStaticText:new(Panel, 1, "What You Waiting For? Click Start!",[]),
  RunProcess = wxStaticText:new(Panel, 1, "Number Of The Running Processes : ",[]),
  
  %Create Sizers labels
  Edit1 = wxStaticText:new(Panel, 1, "edit1:",[]),
  Edit2 = wxStaticText:new(Panel, 1, "edit2:",[]),
  Edit3 = wxStaticText:new(Panel, 1, "edit3:",[]),
  Edit4 = wxStaticText:new(Panel, 1, "edit4:",[]),
  Edit5 = wxStaticText:new(Panel, 1, "edit5:",[]),
  Edit6 = wxStaticText:new(Panel, 1, "edit6:",[]),
  
  %create a botton to initiate batman protocol and connect it to its event handler
  StartB = wxButton:new(Panel, 0, [{label, "Start"}, {size, {130, 40}}]),
  ApplyChangesB = wxButton:new(Panel, 1, [{label, "Apply Changes"}, {size, {130, 40}}]),
  TerminateB = wxButton:new(Panel, 1, [{label, "Terminate"}, {size, {130, 40}}]),
  wxEvtHandler:connect(StartB, command_button_clicked, [callback]),
  wxEvtHandler:connect(ApplyChangesB, command_button_clicked, [callback]),
  wxEvtHandler:connect(TerminateB, command_button_clicked, [callback]),
  wxEvtHandler:connect(Panel, paint, [callback]),%Connect the Panel to paint event
  
  %SpinCtrls
  SC1 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC1, 2, 1000),
  SC2 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC2, 2, 1000),
  SC3 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC3, 2, 1000),
  SC4 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC4, 2, 1000),
  SC5 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC5, 2, 1000),
  SC6 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC6, 2, 1000),

 
  
  %Arrange the Frame:
  Sizer1 = wxGridSizer:new(0,2,3,3),
  wxSizer:addSpacer(Sizer, 5),                  %    ----------------------
  wxSizer:add(Sizer, StartText, [{border, 5}, {flag, ?wxALL}]),

  Sizer2 = wxGridSizer:new(0,2,3,3),
  Sizer3 = wxGridSizer:new(0,2,3,3),
  Sizer4 = wxGridSizer:new(0,2,5,5),
  Sizer5 = wxGridSizer:new(0,3,3,3),
 

  Edit11 = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
  wxSizer:add(Edit11,Edit1, [{flag, ?wxEXPAND}]),
  wxSizer:add(Edit11,SC1, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, Edit11, [{flag, ?wxEXPAND}]),
  
  Edit22 = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
  wxSizer:add(Edit22,Edit2, [{flag, ?wxEXPAND}]),
  wxSizer:add(Edit22,SC2, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, Edit22, [{flag, ?wxEXPAND}]),
  
  Edit33 = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
  wxSizer:add(Edit33,Edit3, [{flag, ?wxEXPAND}]),
  wxSizer:add(Edit33,SC3, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, Edit33, [{flag, ?wxEXPAND}]),
  
  Edit44 = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
  wxSizer:add(Edit44,Edit4, [{flag, ?wxEXPAND}]),
  wxSizer:add(Edit44,SC4, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, Edit44, [{flag, ?wxEXPAND}]),
  
  Edit55 = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
  wxSizer:add(Edit55,Edit5, [{flag, ?wxEXPAND}]),
  wxSizer:add(Edit55,SC5, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, Edit55, [{flag, ?wxEXPAND}]),
  
  Edit66 = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
  wxSizer:add(Edit66,Edit6, [{flag, ?wxEXPAND}]),
  wxSizer:add(Edit66,SC6, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, Edit66, [{flag, ?wxEXPAND}]),
  

  wxSizer:add(Sizer5, StartB, [{border, 5}, {flag, ?wxALL}]),

  wxSizer:add(Sizer5, ApplyChangesB, [{border, 3}, {flag, ?wxALL}]),
  wxSizer:add(Sizer5, TerminateB, [{border, 3}, {flag, ?wxALL}]),
  
 wxSizer:add(Sizer2, RunProcess, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:addSpacer(Sizer2, 5),
  wxSizer:add(Sizer3, Board, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(Sizer2, Sizer5),
  wxSizer:add(Sizer4, Sizer2),
  wxSizer:add(Sizer4, Sizer3),
  wxSizer:add(Sizer1, Sizer4),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, Sizer4),
  
  %add all sizer to the main sizer connected to the panel
  wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(Panel, MainSizer),
  wxSizer:layout(MainSizer),

  %Frame is ready for display, update the guiState and go to state waiting
  wxFrame:show(Frame),
  
  {ok, state_name, #gui_state{}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #gui_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #gui_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #gui_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #gui_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
