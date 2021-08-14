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
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,boardPaint/3,
  paintTheBoard/1,paintTheElem/2,readData/2,code_change/4, callback_mode/0 , handle_click/2,getSlaveColour1/2,getSlaveColour2/2,getSlaveColour3/2]).
  
-include_lib("wx/include/wx.hrl").
-define(max_x,(600)).
-define(max_y,(600)).



-define(SERVER, ?MODULE).

-record(gui_state, {frame,wx,panel,startText,board,spinCtrls,demi,dc,master,slaveNodes}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link([SlaveNodes, Master]) ->
  {ok,Pid} = gen_statem:start_link({global, ?SERVER}, ?MODULE, [SlaveNodes, Master], []),
  Pid.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([SlaveNodes, Master]) ->
  Wx = wx:new(),
  wx_object:start(?MODULE, [Wx], []),
  Frame = wxFrame:new(Wx, 1, "Welcome To TBRPF Protocol Project", [{size, {?max_x+600, ?max_y+100}}]),%Creates the main frame for the gui
  Panel = wxPanel:new(Frame, [{size, {?max_x,?max_y-100}}]), % a panel we will split
  Board = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE},{size, {?max_x+200,?max_y+200}}]), %the board to print on the elemints
  MainSizer = wxBoxSizer:new(?wxVERTICAL),      %main sizer for alignment within the panel
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Our Project"}]), %inside frame 
  StartText = wxStaticText:new(Panel, 1, "enter the parameters and click start",[]),

  
  %Create Sizers labels
  Edit1 = wxStaticText:new(Panel, 1, "Maximum Velocity:",[]),
  Edit2 = wxStaticText:new(Panel, 1, "Transmission radius:",[]),
  Edit3 = wxStaticText:new(Panel, 1, "Number Of Elements:",[]),
  Edit4 = wxStaticText:new(Panel, 1, "edit4:",[]),
  %Edit5 = wxStaticText:new(Panel, 1, "edit5:",[]),
  %Edit6 = wxStaticText:new(Panel, 1, "edit6:",[]),
  
  %create a botton to initiate batman protocol and connect it to its event handler
  StartB = wxButton:new(Panel, 0, [{label, "Start"}, {size, {130, 40}}]),
  %ApplyChangesB = wxButton:new(Panel, 1, [{label, "Setup"}, {size, {130, 40}}]),
  %TerminateB = wxButton:new(Panel, 1, [{label, "Terminate"}, {size, {130, 40}}]),
  wxEvtHandler:connect(StartB, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{board => Board,text => StartText, wx => wx:get_env()}}]),
  %wxEvtHandler:connect(ApplyChangesB, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{board => Board,text => StartText, wx => wx:get_env()}}]),
  %wxEvtHandler:connect(TerminateB, command_button_clicked,  [{callback, fun handle_click/2}, {userData, #{board => Board,text => StartText, wx => wx:get_env()}}]),
  wxEvtHandler:connect(Panel, paint, [callback]),%Connect the Panel to paint event
  
  %SpinCtrls
  SC1 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC1, 2, 1000),
  SC2 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC2, 2, 1000),
  SC3 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC3, 4, 1000),
  SC4 = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SC4, 2, 1000),
  %SC5 = wxSpinCtrl:new(Panel, []),
  %wxSpinCtrl:setRange(SC5, 2, 1000),
  %SC6 = wxSpinCtrl:new(Panel, []),
  %wxSpinCtrl:setRange(SC6, 2, 1000),

 
  
  
  Sizer1 = wxGridSizer:new(0,2,3,3),
  wxSizer:addSpacer(Sizer, 5),                 
  wxSizer:add(Sizer, StartText, [{border, 5}, {flag, ?wxALL}]),

  Sizer2 = wxGridSizer:new(0,2,3,3),
  Sizer3 = wxGridSizer:new(0,2,3,3),
  Sizer4 = wxGridSizer:new(0,2,5,5),
  Sizer5 = wxGridSizer:new(0,1,3,3),
 

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
  

  

  wxSizer:add(Sizer5, StartB, [{border, 5}, {flag, ?wxALL}]),

  %wxSizer:add(Sizer5, ApplyChangesB, [{border, 3}, {flag, ?wxALL}]),
  %wxSizer:add(Sizer5, TerminateB, [{border, 3}, {flag, ?wxALL}]),
  
 
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
  SpinCtrls = {SC1,SC2,SC3,SC4},
  wxFrame:show(Frame),
  
  {ok, state_name, #gui_state{frame = Frame, wx = Wx,panel = Panel,startText = StartText,
        board = Board, spinCtrls = SpinCtrls, demi = 50, master = Master,slaveNodes = SlaveNodes}}.
  
  
%handle the button click
handle_click(#wx{obj = _, userData = #{text := StartText, wx := Wx}},_Event) ->
  wx:set_env(Wx),
  wxStaticText:setLabel(StartText, "communicating"),
  gen_statem:cast({global, ?SERVER}, {sendParam,Wx}).

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

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

%idle( cast, paintBoard, State = #gui_state{}) -> {next_state, boardPaint, State};%
%idle( cast, _, State = #gui_state{}) -> {next_state, idle, State}.



boardPaint( cast, {sendParam,Wx}, State = #gui_state{}) -> readData(Wx,State = #gui_state{}),{next_state, boardPaint, State};
boardPaint( cast, {paintElement,Ets}, State = #gui_state{}) -> paintTheBoard(State = #gui_state{}),
paintTheElem(Ets,State = #gui_state{}),{next_state, boardPaint, State};
boardPaint( cast, _, State = #gui_state{}) -> {next_state, boardPaint, State}.

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

paintTheBoard(#gui_state{frame = Frame,board = Board}) -> 
DC = wxWindowDC:new(Board),
B=wxBrush:new(),
wxDC:setBrush(DC, B),
Pen=wxPen:new(),
wxPen:setColour(Pen,1,1,1),
wxDC:setPen(DC,Pen ),
wxDC:drawRectangle(DC,{0,0},{?max_x,?max_x}),
wxDC:drawLine(DC,{?max_x div 2,0},{?max_x div 2,?max_x}),
wxDC:drawLine(DC,{0,?max_x div 2},{?max_x,?max_x div 2}),
wxPaintDC:destroy(DC),
wxFrame:show(Frame),
{ok, state_name, #gui_state{dc = DC}}.


paintTheElem(Ets,#gui_state{frame=Frame,dc = DC,slaveNodes= SlaveNodes}) -> 
wxDC:setPen(DC, wxPen:new(?wxRED, [{width, 2}])),
 [paintCirclesColors(DC,SlaveNodes,Slave,X div 4,Y div 4 ) || {{Slave,_Pid},{X,Y}}<- Ets],



wxFrame:show(Frame),
{ok, state_name, #gui_state{dc = DC}}.


readData(Wx,State = #gui_state{spinCtrls = SpinCtrls}) -> wx:set_env(Wx),
  {SC1,SC2,SC3,SC4} = SpinCtrls,
  SC1data = wxSpinCtrl:getValue(SC1),
  SC2data = wxSpinCtrl:getValue(SC2),
  SC3data = wxSpinCtrl:getValue(SC3),
  SC4data = wxSpinCtrl:getValue(SC4),
  MasterNode = State#gui_state.master,
  gen_server:cast({MasterNode,{allParameter, {SC1data,SC2data,SC3data,SC4data}}}).
 
  
paintCirclesColors(DC,SalveNodes,Slave,X,Y) -> Penn= wxPen:new(),%Set a different color for each node.
wxDC:setPen(DC, wxPen:setColour(Penn,getSlaveColour1(Slave,SalveNodes),getSlaveColour2(Slave,SalveNodes),getSlaveColour1(Slave,SalveNodes))) ,
wxDC:drawCircle(DC, {X,Y}, 3).
  
 getSlaveColour1(Slave,[{Num,[_,Slave]}|_]) -> 
 case Num of 
      1 -> 1;
      2 -> 0;
      3 -> 0;
      4 -> 0
      end;
 getSlaveColour1(Slave,[_|L]) -> getSlaveColour1(Slave,L).
 
  getSlaveColour2(Slave,[{Num,[_,Slave]}|_]) -> 
 case Num of 
      1 -> 0;
      2 -> 0;
      3 -> 1;
      4 -> 1
      end;
 getSlaveColour2(Slave,[_|L]) -> getSlaveColour2(Slave,L).
 
  getSlaveColour3(Slave,[{Num,[_,Slave]}|_]) -> 
 case Num of 
      1 -> 0;
      2 -> 1;
      3 -> 1;
      4 -> 0
      end;
 getSlaveColour3(Slave,[_|L]) -> getSlaveColour3(Slave,L).
 
 
