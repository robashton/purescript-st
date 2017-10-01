-module(control_monad_sT@foreign).
-behavior(gen_server).
-export([modifySTRef/2, newSTRef/1, readSTRef/1, runST/1, writeSTRef/2]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

modifySTRef(Ref, F) ->
    fun () ->
        gen_server:call(Ref, {modify, F})
    end.

newSTRef(Value) ->
    fun () ->
        {ok, Pid} = gen_server:start(?MODULE, [Value], []),
        Pid
    end.

readSTRef(Ref) ->
    fun () ->
        gen_server:call(Ref, read)
    end.

runST(Eff) -> Eff.

writeSTRef(Ref, Value) ->
    fun () ->
        gen_server:call(Ref, {write, Value})
    end.

%% gen_server callbacks

code_change(_OldVersion, State, _Extra) -> {ok, State}.

handle_call({modify, F}, _From, State) ->
    NewState = F(State),
    {reply, NewState, NewState};
handle_call(read, _From, State) ->
    {reply, State, State};
handle_call({write, NewState}, _From, _State) ->
    {reply, NewState, NewState}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

init(State) -> {ok, State}.

terminate(_Reason, _State) -> ok.
