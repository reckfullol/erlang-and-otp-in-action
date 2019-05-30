-module(simple_cache_sup).

-behavior(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([
  start_link/0
]).

-export([
  init/1
]).


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  ElementSup = {
    simple_cache_element_sup,
    {
    simple_cache_element_sup, start_link, []
    },
    permanent,
    2000,
    supervisor,
    [simple_cache_event]
  },

  EventManager = {
    simple_cache_event,
    {
      simple_cache_event,
      start_link,
      []
    },
    permanent,
    2000,
    worker,
    [simple_cache_event]
  },

  Children = [
    ElementSup,
    EventManager
  ],

  RestartStrategy = {
    one_for_one,
    4,
    3600
  },

  %% return
  {
    ok,
    {
      RestartStrategy,
      Children
    }
  }.
