%%%-------------------------------------------------------------------
%% @doc whistlee top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(whistlee_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      {discovery_id,
       {discovery, start_link, ["Colby's test Accessory", "_hap._tcp.", 9394]},
       permanent,
       5000,
       worker,
       [discovery]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
