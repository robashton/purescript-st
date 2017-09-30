-module(control_monad_sT@foreign).
-export([modifySTRef/2, newSTRef/1, readSTRef/1, runST/1, writeSTRef/2]).

modifySTRef(Ref, F) ->
    fun () ->
        rpc(Ref, {modify, F})
    end.

newSTRef(Value) ->
    fun () ->
        spawn(fun() -> sTRef(Value) end)
    end.

readSTRef(Ref) ->
    fun () ->
        rpc(Ref, read)
    end.

runST(Eff) -> Eff.

writeSTRef(Ref, Value) ->
    fun () ->
        rpc(Ref, {write, Value})
    end.

%% ST Implementation

sTRef(Value) ->
    receive
        {From, {modify, F}} ->
            NewValue = F(Value),
            respond(From, NewValue),
            sTRef(NewValue);

        {From, read} ->
            respond(From, Value),
            sTRef(Value);

        {From, {write, NewValue}} ->
            respond(From, NewValue),
            sTRef(NewValue)
    end.

respond(Pid, Response) ->
    Pid ! {self(), Response}.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.
