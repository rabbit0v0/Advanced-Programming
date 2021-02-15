-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

start(Initial) -> 
	{Sig, M} = bind(Initial, #{}),
	if Sig =:= ok ->
		A = init_alia(Initial, []),
		E = spawn(fun() -> loop(M, A) end),
		{Sig, E};
	Sig =:= error ->
		spawn(fun() -> loop(#{}, []) end),
		{Sig, M}
	end.

bind([], M) -> {ok, M};

bind([{A, B}|T], M) -> 
	Tmp = maps:is_key(A, M),
	if Tmp -> {error, "duplicate shortcodes"};
		true -> bind(T, M#{A => B})
	end.

init_alia([], A) -> A;
init_alia([{N, _}|T], A) -> 
	S = [N],
	A_ = lists:append([S], A),
	init_alia(T, A_).

init_fun([], F) -> F;
init_fun({N, _}|T], F) ->
	

new_shortcode(E, Short, Emo) -> 
	request_reply(E, {newcode, Short, Emo}).

alias(E, Short1, Short2) ->
	request_reply(E, {alias, Short1, Short2}).

delete(E, Short) -> 
	E ! {self(), {delete, Short}}.

lookup(E, Short) -> 
	request_reply(E, {lookup, Short}).

analytics(E, Short, Fun, Label, Init) -> 
	request_reply(E, {analytics, Short, Fun, Label, Init}).

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(_) -> not_implemented.

request_reply(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.

find_alias(_, []) -> [];
find_alias(Name, [H|T]) ->
	Tmp = lists:member(Name, H),
	if Tmp -> H;
	true -> find_alias(Name, T)
	end.

add_alias(Head, _, _, []) -> Head;
add_alias(Head, N1, N2, [H|T]) ->
	Tmp = lists:member(N1, H),
	if Tmp -> 
		H_ = [N2] ++ H,
		Head ++ [H_] ++ T;
	true -> add_alias(Head++[H], N1, N2, T)
	end.

remove_names([], M) -> M;
remove_names([H|T], M) -> 
	M_ = maps:remove(H, M),
	remove_names(T, M_).

loop(M, Alia) -> 
	receive
		{From, {newcode, Short, Emo}} ->
			{Sig, M_} = bind([{Short, Emo}], M),
			if Sig =:= ok ->
				From ! {self(), Sig},
				loop(M_, Alia);
			Sig =:= error ->
				From ! {self(), {Sig, M_}},
				loop(M, Alia)
			end;
		{From, {lookup, Short}} ->
			Tmp = maps:get(Short, M, []),
			if Tmp =:= [] ->
				From ! {self(), {error, "key does not exist"}},
				loop(M, Alia);
			true ->
				From ! {self(), {ok, Tmp}},
				loop(M, Alia)
			end;
		{From, {delete, Short}} ->
			Names = find_alias(Short, Alia),
			M_ = remove_names(Names, M),
			loop(M_, Alia);
		{From, {alias, Short1, Short2}} ->
			Tmp1 = maps:is_key(Short1, M),
			Tmp2 = maps:is_key(Short2, M),
			if Tmp2 ->
				From ! {self(), {error, "the alia name has been used"}},
				loop(M, Alia);
			Tmp1 =:= false ->
				From ! {self(), {error, "trying to give an alia name to an unbound shortcode"}},
				loop(M, Alia);
			true ->
				V = maps:get(Short1, M),
				{_, M_} = bind([{Short2, V}], M),
				A_ = add_alias([], Short1, Short2, Alia),
				From ! {self(), {ok}},
				loop(M_, A_)
			end;
		{From, {analytics, Short, Fun, Label, Init}} ->

	end.