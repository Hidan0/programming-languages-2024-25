-module(p22).

-export([range/2]).

range(Start, End) when Start < End -> [Start | range(Start + 1, End)];
range(Start, End) when Start == End -> [Start].
