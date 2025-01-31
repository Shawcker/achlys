-module(soldier_assist).

-author("Antoine POPELER & Aqsa GHAFFAR").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([add_temp_task/0]).
-export([add_move_task/0]).
-export([get_average/0]).
-export([get_own_average/0]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         terminate/2 ,
         code_change/3]).

-define(SERVER , ?MODULE).

-record(state , {}).

%% Base

start_link() ->
    gen_server:start_link({local , ?SERVER} , ?MODULE , [] , []).

init([]) ->
    {ok , #state{}}.

handle_call(_Request , _From , State) ->
    {reply , ok , State}.

handle_cast({task, Task} , State) ->
    %% Task propagation to the cluster, including self
    achlys:bite(Task),    
    {noreply , State};
handle_cast(_Request , State) ->
    {noreply , State}.

handle_info({task, Task} , State) ->
    %% Task propagation to the cluster, including self
    achlys:bite(Task),
    {noreply , State};
handle_info(_Info , State) ->
    {noreply , State}.

terminate(_Reason , _State) ->
    ok.

code_change(_OldVsn , State , _Extra) ->
    {ok , State}.

%% API

add_temp_task() ->
    gen_server:cast(?SERVER 
        , {task, temp_task(100)}).

add_move_task() ->
    {ok, {Id,_,_,_}} = lasp:declare({<<"accum">>, state_orset}, state_orset),
    gen_server:cast(?SERVER 
        , {task, movement_task(Id)}).

% Displays the average of the movements of every node
get_average() ->
    {ok, Set} = lasp:query({<<"accum">>, state_orset}),
    io:format("Average Movement of Soldier = ~p~n", [avg_tuple_list(sets:to_list(Set))]).

% Displays the average of the movements of own node
get_own_average() ->
    {ok, Set} = lasp:query({<<"accum">>, state_orset}),
    io:format("Average Movement of Soldier for this node = ~p~n", [avg_tuple_list_own(sets:to_list(Set))]).

%% Tasks

% Task to read temperature and displaying a message depending on
% Whether or not it goes over the limit Critical_temp
temp_task(Critical_temp) ->
    Task = achlys:declare(temp_task
        , all
        , permanent
        , fun() ->
            logger:log(notice, "Reading PmodNAV temperature measurements ~n"),
            Temp = pmod_nav:read(alt, [temp_out]),
            Node = erlang:node(),

            if
                hd(Temp) > Critical_temp -> io:format("It's too hot ! Stop firing. ~n");
                true -> io:format("Everything is ok ! ~n")
            end
    end).

% Task to read values from the accelerometer
% Adds the addition of the absolute values of the 3 measures (x, y, z) to a lasp CRDT
movement_task(Id) ->
    Task = achlys:declare(temp_task
        , all
        , permanent
        , fun() ->
            logger:log(notice, "Reading PmodNAV accelerometer measurements ~n"),
            Acc = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl]),
            Node = erlang:node(),

            lasp:update(Id, {add, {add_list(Acc), Node}}, self())
    end).

%% Auxiliary

% Computes the average of the first element of each tuple in the list TupleList
avg_tuple_list(TupleList) ->
    avg_tuple_list(TupleList, 0, 0).

avg_tuple_list([H|T], Acc, Div) ->
    Elem = element(1, H),
    if
        Elem > 0 -> avg_tuple_list(T, Acc + Elem, Div+1);
        true -> avg_tuple_list(T, Acc - Elem, Div+1)
    end;
avg_tuple_list([], Acc, Div) ->
    if
        Div > 0 -> Acc / Div;
        true -> 0
    end.

% Computes the average of the first element of each tuple in the list TupleList
% As long as they come from this node (second element of each tuple)
avg_tuple_list_own(TupleList) ->
    avg_tuple_list_own(TupleList, 0, 0).

avg_tuple_list_own([H|T], Acc, Div) ->
    Elem = element(1, H),
    Node = element(2, H),
    ThisNode = erlang:node(),
    case Node of
        ThisNode -> 
        if
            Elem > 0 -> avg_tuple_list_own(T, Acc + Elem, Div + 1);
            true -> avg_tuple_list_own(T, Acc - Elem, Div + 1)
        end;
        Otherwise -> avg_tuple_list_own(T, Acc, Div)
    end;
avg_tuple_list_own([], Acc, Div) ->
    if
        Div > 0 -> Acc / Div;
        true -> 0
    end.

% Computes the addition of every number from the list List
add_list(List) ->
    add_list(List, 0).

add_list([H|T], Acc) ->
    if
        H > 0 -> add_list(T, Acc + H);
        true -> add_list(T, Acc - H)
    end;
add_list([], Acc) ->
    Acc.