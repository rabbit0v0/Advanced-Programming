-module(warmup).
-export([]).

% direction is one of the atoms north, south, east or west

move(north, {X, Y}) -> {X, Y+1};
move(west,  {X, Y}) -> {X-1, Y}.
% complete the definition


% A binary search tree is either
%      the atom leaf
%   or a tuple {node, Key, Value, Left, Right}
%      where Left and Right are binary search trees, and all the keys
%      in Left are smaller than Key and all the keys in Right are
%      larger than Key


% insert inserts a key and a value into a binary search tree. If the
% key is already there the value is updated.

insert(Key, Value, Tree) -> undefined.
% complete the definition.


% lookup find the value associated to a key in a binary search
% tree. Returns {ok, Value} if the key is in the tree; or none if the
% key is not in the tree.

lookup(Key, Tree) -> undefined.
% complete the definition.
