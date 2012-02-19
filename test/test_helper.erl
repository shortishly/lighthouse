-module(test_helper).
-export([all/1]).

is_a_test(is_a_test) ->
    false;
is_a_test(Function) ->
    hd(lists:reverse(string:tokens(atom_to_list(Function), "_"))) =:= "test".

all(Module) ->
    [Function || {Function, Arity} <- Module:module_info(exports),		
		 Arity =:= 1,
		 is_a_test(Function)].
