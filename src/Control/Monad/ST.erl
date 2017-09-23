-module(control_monad_sT@foreign).
-export([modifySTRef/2, newSTRef/1, readSTRef/1, runST/1, writeSTRef/2]).

modifySTRef(Ref, F) ->
    fun () ->
        [{value, Value1}] = ets:lookup(Ref, value),
        Value2 = F(Value1),
        ets:insert(Ref, {value, Value2}),
        Value2
    end.

newSTRef(Value) ->
    fun () ->
        Ref = ets:new(st_ref, []),
        ets:insert(Ref, {value, Value}),
        Ref
    end.

readSTRef(Ref) ->
    fun () ->
        [{value, Value}] = ets:lookup(Ref, value),
        Value
    end.

runST(Eff) -> Eff.

writeSTRef(Ref, Value) ->
    fun () ->
        ets:insert(Ref, {value, Value}),
        Value
    end.
