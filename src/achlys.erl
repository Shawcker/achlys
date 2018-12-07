%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/Laymer/achlys]
%%% @doc
%%% Top level module operating the achlys application
%%% as well as providing the main API to the underlying components.
%%% @end
%%% Created : 06. Nov 2018 20:38
%%%-------------------------------------------------------------------
-module(achlys).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-include("achlys.hrl").
-include_lib("partisan/include/partisan.hrl").

%% Application control
-export([start/0]).
-export([stop/0]).

%% Task model API
-export([get_all_tasks/0]).
-export([bite/1]).

%% API
-export([clusterize/0]).
-export([contagion/0]).
-export([pandemia/0]).
-export([get_preys/0]).
-export([get_bounded_preys/0]).
-export([bidirectional_join/1]).

%% PMOD-related functions API
-export([bane/1]).
-export([venom/0]).
-export([venom/1]).

%% Shortcuts
-export([members/0]).
-export([gc/0]).
-export([flush/1]).

%%====================================================================
%% Type definitions
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

% -define(MANAGER,    lasp_peer_service:manager()).
-define(MANAGER,    partisan_hyparview_peer_service_manager).
-define(LPS,    lasp_peer_service).

%% ===================================================================
%% Entry point functions
%% ===================================================================

%% @doc Start the application.
-spec start() -> ok.
start() ->
    {ok , _} = application:ensure_all_started(achlys) ,
    ok.

%% @doc Stop the Application
-spec stop() -> ok.
stop() ->
    ok = application:stop(achlys) ,
    ok.

%% ===================================================================
%% API
%% ===================================================================

%% @doc Returns current view of all tasks.
-spec get_all_tasks() -> [achlys:task()] | [].
get_all_tasks() ->
    {ok, Set} = lasp:query(?TASKS),
    sets:to_list(Set).

%% @doc Adds the given task in the replicated task set.
%% @see achlys_task_server:add_task/1.
-spec bite(Task :: achlys:task()) -> ok.
bite(Task) ->
    achlys_task_server:add_task(Task).

%% @doc Attempts to discover and join other neighboring nodes.
-spec clusterize() -> [atom()].
clusterize() ->
    logger:log(notice , "Cluster formation attempt ~n") ,
    N = seek_neighbors() ,
    Remotes = binary_remotes_to_atoms(N) ,
    Reachable = [R || R <- Remotes
        ,        R =/= node()
        ,        net_adm:ping(R) =:= pong] ,
    clusterize(Reachable).

%% @doc Form Lasp cluster without attempting to ping neighbors beforehand.
-spec contagion() -> list().
contagion() ->
    logger:log(notice , "Pure Lasp Cluster formation attempt ~n") ,
    L = get_preys(),
    Self = ?MANAGER:myself(),
    [ bidirectional_join(R) || R <- L
        ,        R =/= node()
        ,        net_adm:ping(R) =:= pong].
    % _ = [ lasp_peer_service:join(rpc:call(X,?MANAGER,myself,[]))
    % || X <- L, net_adm:ping(X) =:= pong, X =/= node() ],
    % [ rpc:call(X,lasp_peer_service,join,[Self]) || X <- L ].

%% @doc Close disterl TCP connections with neighboring nodes.
-spec pandemia() -> ok.
pandemia() ->
    logger:log(notice , "Closing native Erlang connections ~n") ,
    _ = [ net_kernel:disconnect(X) || X <- get_preys() ],
    ok.

%% @doc Returns the aggregates for the given variable
%% as seen by the current node.
-spec bane(atom()) -> list().
bane(Data) ->
    logger:log(notice , "Reading ~p CRDT ~n", [Data]) ,
    % Id = {atom_to_binary(Data, utf8), state_awset_ps},
    Id = {atom_to_binary(Data, utf8), state_awset},
    {ok, S} = lasp:query(Id),
    sets:to_list(S).

%% @doc Collect data based on sensors available on Pmod modules and store
%% aggregated values in corresponding Lasp variable.
-spec venom() -> ok.
venom() ->
    achlys_pmod_nav_worker:run().

%% @doc Collect data based on sensors available on Pmod modules and store
%% aggregated values in corresponding Lasp variable.
-spec venom(atom()) -> ok.
venom(Worker) ->
    Worker:run().
    % achlys_pmod_nav_worker:run().

%%====================================================================
%% Clustering helper functions
%%====================================================================

%% @doc Returns a list of known remote hostnames
%% that could be potential neighbors.
get_preys() ->
    binary_remotes_to_atoms(seek_neighbors()).

%% @doc Returns a list of known remote hostnames
%% that could be potential neighbors, limited to maximum active view size.
get_bounded_preys() ->
    L = lists:usort(binary_remotes_to_atoms(seek_neighbors())),
    Len = length(L),
    {ok, MaxActiveSize} = application:get_env(partisan, max_active_size),
    case Len > MaxActiveSize of
        true ->
            {H, _T} = lists:split(MaxActiveSize, L),
            H;
        false ->
            L
    end.


%% @private
binary_remotes_to_atoms([H | T]) ->
    [binary_to_atom(H , utf8) | binary_remotes_to_atoms(T)];
binary_remotes_to_atoms([]) ->
    [].

%% @private
seek_neighbors() ->
    Rc = inet_db:get_rc() ,
    seek_neighbors(Rc).
seek_neighbors([{host , _Addr , N} | T]) ->
    [list_to_bitstring(["achlys@" , N]) | seek_neighbors(T)];
seek_neighbors([{_Arg , _Val} | T]) ->
    seek_neighbors(T);
seek_neighbors([]) ->
    [].

% %% @private
% join(Host) ->
%     Manager = rpc:call(Host , partisan_peer_service , manager , []) ,
%     case Manager of
%         partisan_hyparview_peer_service_manager ->
%             Node = rpc:call(Host , Manager , myself , []) ,
%             ok = partisan_peer_service:join(Node) ,
%             logger:log(info , "Joined ~p~n" , [Host]) ,
%             {ok , Node};
%         {badrpc , Reason} ->
%             logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
%             {error , Reason};
%         {error , Reason} ->
%             logger:log(error , "Unable to retrieve remote : ~p~n" , [Manager]) ,
%             {error , Reason}
%     end.
%% @private
join(Host) ->
    try rpc:call(Host , lasp_peer_service:manager() , myself , []) of
        #{channels := _Channels
        , listen_addrs := _Addresses
        , name := _Name
        , parallelism := _Parallelism } = Node ->
            ok = lasp_peer_service:join(Node),
            {ok, Host}
    catch
        {badrpc, Reason} ->
            logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
            {error , Reason};
        {error, Reason} ->
            logger:log(error , "Unable to retrieve remote : ~p~n" , [Reason]) ,
            {error , Reason}
    end.

%% @private
bidirectional_join(Host) ->
    try rpc:call(Host , lasp_peer_service:manager() , myself , []) of
        #{channels := _Channels
        , listen_addrs := _Addresses
        , name := _Name
        , parallelism := _Parallelism } = Node ->
            ok = ?LPS:join(Node),
            ok = rpc:call(Host,?LPS,join,[?MANAGER:myself()]),
            {ok, Host}
    catch
        {badrpc, Reason} ->
            logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
            {error , Reason};
        {error, Reason} ->
            logger:log(error , "Unable to retrieve remote : ~p~n" , [Reason]) ,
            {error , Reason}
    end.

%% @private
clusterize([H | Remotes]) ->
    case join(H) of
        {ok , N} ->
            [N | clusterize(Remotes)];
        _ ->
            logger:log(warning , "Failed to join : ~p ~n" , [H]) ,
            clusterize(Remotes)
    end;

clusterize([]) ->
    [].

%% @private
members() ->
    ?LPS:members().

%% @private
gc() ->
    achlys_util:do_gc().

%% @private
flush(Name) ->
    BS = atom_to_binary(Name , utf8),
    lasp:update({BS,state_awset}
        , {rmv_all, achlys_util:query({BS,state_awset})}
        , self()).
