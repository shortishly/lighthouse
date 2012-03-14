-module(impel_hierarchy_gb_trees_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
    test_helper:all(?MODULE).


suite() ->
    [{timetrap, {minutes, 10}}].


init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    void.

init_per_testcase(_, Config) ->
    {ok, _} = impel_hierarchy_gb_trees:start_link(),
    Config.

end_per_testcase(_, _) ->
    ok = impel_hierarchy:stop().

empty_hierarchy_test(_) ->
    {ok, []} = impel_hierarchy:children().

add_leaf_test(_) ->
    impel_hierarchy:update([a], 1),
    {ok, [A]} = impel_hierarchy:children(),
    a = impel_hierarchy:key(A),
    leaf = impel_hierarchy:type(A),
    {error, {is_a_leaf, a, []}} = impel_hierarchy:children([a]).

add_branch_test(_) ->
    impel_hierarchy:update([a, b], 1),
    {ok, [A]} = impel_hierarchy:children(),
    a = impel_hierarchy:key(A),
    branch = impel_hierarchy:type(A),
    {ok, [B]} = impel_hierarchy:children([a]),
    b = impel_hierarchy:key(B),
    leaf = impel_hierarchy:type(B),
    {error, {is_a_leaf, b, [a]}} = impel_hierarchy:children([a, b]).

add_children_test(_) ->
    impel_hierarchy:update([a, b, c], 1),
    impel_hierarchy:update([a, b, d], 2),
    impel_hierarchy:update([a, b, e, f], 3),
    {ok, [A]} = impel_hierarchy:children(),
    a = impel_hierarchy:key(A),
    branch = impel_hierarchy:type(A),
    {ok, [B]} = impel_hierarchy:children([a]),
    b = impel_hierarchy:key(B),
    branch = impel_hierarchy:type(B),
    {ok, Children} = impel_hierarchy:children([a, b]),
    [C, D, E] = lists:sort(fun(P, Q) -> impel_hierarchy:key(P) < impel_hierarchy:key(Q) end, Children),
    c = impel_hierarchy:key(C),
    leaf = impel_hierarchy:type(C),
    d = impel_hierarchy:key(D),
    leaf = impel_hierarchy:type(D),
    e = impel_hierarchy:key(E),
    branch = impel_hierarchy:type(E),
    {ok, [F]} = impel_hierarchy:children([a, b, e]),
    f = impel_hierarchy:key(F),
    leaf = impel_hierarchy:type(F).

not_found_test(_) ->
    impel_hierarchy:update([a, b], 1),
    {error, {not_found, c, [a]}} = impel_hierarchy:children([a, c]).

event_manager_leaf_test(_) ->
    Leaf = [a, b, c],
    impel_hierarchy:update(Leaf, 1),
    {ok, EventManager} = impel_hierarchy:event_manager(Leaf),
    ?assert(is_process_alive(EventManager)).

event_manager_branch_test(_) ->
    Branch = [a, b],
    impel_hierarchy:update(Branch ++ [c], 1),
    ?assertEqual({error, not_a_leaf}, impel_hierarchy:event_manager(Branch)).

delete_leaf_test(_) ->
    Leaf = [a, b, c],
    impel_hierarchy:update(Leaf, 1),
    {ok, EventManager} = impel_hierarchy:event_manager(Leaf),
    ?assert(is_process_alive(EventManager)),
    impel_hierarchy:delete(Leaf),
    ?assertEqual({ok, []}, impel_hierarchy:children([a, b])),
    ?assertNot(is_process_alive(EventManager)).

delete_branch_test(_) ->
    Branch = [a, b],
    Leaf = Branch ++ [c],
    impel_hierarchy:update(Leaf, 1),
    {ok, C} = impel_hierarchy:event_manager(Leaf),
    ?assert(is_process_alive(C)),
    impel_hierarchy:delete(Branch),
    ?assertEqual({ok, []}, impel_hierarchy:children([a])),
    ?assertNot(is_process_alive(C)).

delete_subtree_test(_) ->
    Branch = [a, b],
    L1 = Branch ++ [c, d, e],
    impel_hierarchy:update(L1, 1),
    L2 = Branch ++ [f, g],
    impel_hierarchy:update(L2, 1),
    {ok, E} = impel_hierarchy:event_manager(L1),
    ?assert(is_process_alive(E)),
    {ok, G} = impel_hierarchy:event_manager(L2),
    ?assert(is_process_alive(G)),
    impel_hierarchy:delete(Branch),
    ?assertEqual({ok, []}, impel_hierarchy:children([a])),
    ?assertNot(is_process_alive(E)),
    ?assertNot(is_process_alive(G)).

delete_root_test(_) ->
    Branch = [a, b],
    L1 = Branch ++ [c, d, e],
    impel_hierarchy:update(L1, 1),
    L2 = Branch ++ [f, g],
    impel_hierarchy:update(L2, 1),
    {ok, E} = impel_hierarchy:event_manager(L1),
    ?assert(is_process_alive(E)),
    {ok, G} = impel_hierarchy:event_manager(L2),
    ?assert(is_process_alive(G)),
    impel_hierarchy:delete([a]),
    ?assertEqual({ok, []}, impel_hierarchy:children()),
    ?assertNot(is_process_alive(E)),
    ?assertNot(is_process_alive(G)).
    
    
    
    
    
    
    
    
    
    
    
    


