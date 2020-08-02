-module(control_monad_st_internal@foreign).

-export([ map_/1
        , pure_/1
        , bind_/2
        , run/1
        , while/2
        , for/3
        , foreach/2
        , new/1
        , read/1
        , modifyImpl/2
        , write/2
        ]).

map_(F, A) ->
  fun() ->
    F(A())
  end.

pure_ (A) ->
  fun() -> A end.

bind_(A, F) ->
  fun() ->
      F(A())()
  end.

run(F) ->
  F().

while(F, A) ->
  fun Fun() ->
    Result = F(),
    if Result -> A(), Fun();
       true -> ok
    end
  end.

for(Lo, Hi, F) ->
  fun() ->
    fun Fun(C, H) - >
      if
        C < Hi -> (F(C))(),
                  Fun(C+1, H);
        true ->
          ok
    end,
    Fun(Lo, Hi)
  end.

foreach(As, F) ->
  fun() ->
    array:map(fun(I, Item) -> (F(Item))() end)
  end.

new(Val) ->
  Self = self(),
  fun() ->
     spawn_link(fun () ->
                    erlang:monitor(process, Self),
                    value_loop(Self, Val)
                end)
    Val
  end.

read(Ref) ->{
   fun() ->
     get_(Ref)
   end.

get_(Ref) ->
  Ref ! { get, self() },
  receive
    %% TODO: after "x" ? We terminate when parent terminates for the moment
    %% a state monad is only valid as long as its parent exists
    %% as it stands , when the parent is terminated, this function call will result in an infinite hang
    {value, Val } -> Val
  end.

modifyImpl(F, Ref) ->
  fun() ->
      Initial = get_(Ref),
      #{ value := Value
       , state := State
       } = F(Initial),
      Ref ! Value,
      State
  end.

write(A, Ref) ->
  fun() ->
      Ref ! A,
      A
  end.

value_loop(Parent, Val) ->
  receive
    {'DOWN', _MonitorRef, _Type, _Object, _Info} ->
      ok;
    { get, Pid } ->
      Pid ! { value, Val },
      value_loop(Parent, Val);
    NewValue ->
      value_loop(Val);
  end.
