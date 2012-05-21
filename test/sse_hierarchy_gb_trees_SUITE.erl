-module(sse_hierarchy_gb_trees_SUITE).
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
    {ok, _} = sse_hierarchy_gb_trees:start_link(),
    Config.

end_per_testcase(_, _) ->
    ok = sse_hierarchy:stop().

empty_hierarchy_test(_) ->
    {ok, []} = sse_hierarchy:children().

add_leaf_test(_) ->
    sse_hierarchy:update([a], 1),
    {ok, [A]} = sse_hierarchy:children(),
    a = sse_hierarchy:key(A),
    leaf = sse_hierarchy:type(A),
    {error, {is_a_leaf, a, []}} = sse_hierarchy:children([a]).

leaf_into_a_branch_test(_) ->
    sse_hierarchy:update([a], 1),
    {ok, [A1]} = sse_hierarchy:children(),
    a = sse_hierarchy:key(A1),
    leaf = sse_hierarchy:type(A1),
    {error, {is_a_leaf, a, []}} = sse_hierarchy:children([a]),
    sse_hierarchy:update([a, b], 1),
    {ok, [A2]} = sse_hierarchy:children(),
    a = sse_hierarchy:key(A2),
    branch = sse_hierarchy:type(A2),
    {ok, [B]} = sse_hierarchy:children([a]),
    b = sse_hierarchy:key(B),
    leaf = sse_hierarchy:type(B).
    

add_branch_test(_) ->
    sse_hierarchy:update([a, b], 1),
    {ok, [A]} = sse_hierarchy:children(),
    a = sse_hierarchy:key(A),
    branch = sse_hierarchy:type(A),
    {ok, [B]} = sse_hierarchy:children([a]),
    b = sse_hierarchy:key(B),
    leaf = sse_hierarchy:type(B),
    {error, {is_a_leaf, b, [a]}} = sse_hierarchy:children([a, b]).

add_children_test(_) ->
    sse_hierarchy:update([a, b, c], 1),
    sse_hierarchy:update([a, b, d], 2),
    sse_hierarchy:update([a, b, e, f], 3),
    {ok, [A]} = sse_hierarchy:children(),
    a = sse_hierarchy:key(A),
    branch = sse_hierarchy:type(A),
    {ok, [B]} = sse_hierarchy:children([a]),
    b = sse_hierarchy:key(B),
    branch = sse_hierarchy:type(B),
    {ok, Children} = sse_hierarchy:children([a, b]),
    [C, D, E] = lists:sort(fun(P, Q) -> sse_hierarchy:key(P) < sse_hierarchy:key(Q) end, Children),
    c = sse_hierarchy:key(C),
    leaf = sse_hierarchy:type(C),
    d = sse_hierarchy:key(D),
    leaf = sse_hierarchy:type(D),
    e = sse_hierarchy:key(E),
    branch = sse_hierarchy:type(E),
    {ok, [F]} = sse_hierarchy:children([a, b, e]),
    f = sse_hierarchy:key(F),
    leaf = sse_hierarchy:type(F).

not_found_test(_) ->
    sse_hierarchy:update([a, b], 1),
    {error, {not_found, c, [a]}} = sse_hierarchy:children([a, c]).

event_manager_leaf_test(_) ->
    Leaf = [a, b, c],
    sse_hierarchy:update(Leaf, 1),
    EventManager = sse_hierarchy:event_manager(Leaf),
    ?assert(is_process_alive(EventManager)).

event_manager_branch_test(_) ->
    Branch = [a, b],
    sse_hierarchy:update(Branch ++ [c], 1),
    EventManager = sse_hierarchy:event_manager(Branch),
    ?assert(is_process_alive(EventManager)).

delete_leaf_test(_) ->
    Leaf = [a, b, c],
    sse_hierarchy:update(Leaf, 1),
    EventManager = sse_hierarchy:event_manager(Leaf),
    ?assert(is_process_alive(EventManager)),
    sse_hierarchy:delete(Leaf),
    ?assertEqual({ok, []}, sse_hierarchy:children([a, b])),
    ?assertNot(is_process_alive(EventManager)).

delete_branch_test(_) ->
    Branch = [a, b],
    Leaf = Branch ++ [c],
    sse_hierarchy:update(Leaf, 1),
    C = sse_hierarchy:event_manager(Leaf),
    ?assert(is_process_alive(C)),
    sse_hierarchy:delete(Branch),
    ?assertEqual({ok, []}, sse_hierarchy:children([a])),
    ?assertNot(is_process_alive(C)).

delete_subtree_test(_) ->
    Branch = [a, b],
    L1 = Branch ++ [c, d, e],
    sse_hierarchy:update(L1, 1),
    L2 = Branch ++ [f, g],
    sse_hierarchy:update(L2, 1),
    E = sse_hierarchy:event_manager(L1),
    ?assert(is_process_alive(E)),
    G = sse_hierarchy:event_manager(L2),
    ?assert(is_process_alive(G)),
    sse_hierarchy:delete(Branch),
    ?assertEqual({ok, []}, sse_hierarchy:children([a])),
    ?assertNot(is_process_alive(E)),
    ?assertNot(is_process_alive(G)).

delete_root_test(_) ->
    Branch = [a, b],
    L1 = Branch ++ [c, d, e],
    sse_hierarchy:update(L1, 1),
    L2 = Branch ++ [f, g],
    sse_hierarchy:update(L2, 1),
    E = sse_hierarchy:event_manager(L1),
    ?assert(is_process_alive(E)),
    G = sse_hierarchy:event_manager(L2),
    ?assert(is_process_alive(G)),
    sse_hierarchy:delete([a]),
    ?assertEqual({ok, []}, sse_hierarchy:children()),
    ?assertNot(is_process_alive(E)),
    ?assertNot(is_process_alive(G)).
    
    
    
    
    
    
    
    
    
    
    
    


