S: [1, 2, 3, 4, ...]
T: [1, 2, 3, 4, ...]


all_pair(S, T) 
<=> % 根据all_pair定义替换all_pair
cons_stream(
    {1, 1},
    interleave(
        [{1, 2}, {1, 3}, {1, 4}, ...],
        all_pair([2, 3, 4, ...], [1, 2, 3, 4, ...])
    )
)
=> stream({1, 1})

% 取出上面的interleave部分继续推导
interleave(
    [{1, 2}, {1, 3}, {1, 4}, ...],
    all_pair([2, 3, 4, ...], [1, 2, 3, 4, ...])
) 
<=> % 根据interleave定义替换interleave
cons_stream(
    {1, 2},
    interleave(
        all_pair([2, 3, 4, ...], [1, 2, 3, 4, ...]),
        [{1, 3}, {1, 4}, ...]
    )
)
=> stream({1, 1}, {1, 2})

% 取出上面的interleave部分继续推导
interleave(
    all_pair([2, 3, 4, ...], [1, 2, 3, 4, ...]),
    [{1, 3}, {1, 4}, ...]
) 
<=> % 根据all_pair定义替换all_pair
interleave(
    cons_stream(
        {2, 1},
        interleave(
            [{2, 2}, {2, 3}, {2, 4}, ...],
            all_pair([3, 4, ...], [1, 2, 3, 4, ...])
        )
    ),
    [{1, 3}, {1, 4}, ...]
)
<=> % 根据interleave定义替换外层interleave
cons_stream(
    {2, 1},
    interleave(
        [{1, 3}, {1, 4}, ...],
        interleave(
            [{2, 2}, {2, 3}, {2, 4}, ...],
            all_pair([3, 4, ...], [1, 2, 3, 4, ...])
        )
    )
)
=> stream({1, 1}, {1, 2}, {2, 1})

% 取出上面的外层interleave部分继续推导
interleave(
    [{1, 3}, {1, 4}, ...],
    interleave(
        [{2, 2}, {2, 3}, {2, 4}, ...],
        all_pair([3, 4, ...], [1, 2, 3, 4, ...])
    )
)
<=> % 根据interleave定义替换外层interleave
cons_stream(
    {1, 3},
    interleave(
        interleave(
            [{2, 2}, {2, 3}, {2, 4}, ...],
            all_pair([3, 4, ...], [1, 2, 3, 4, ...])
        ),
        [{1, 4}, ...]
    )
)
=> stream({1, 1}, {1, 2}, {2, 1}, {1, 3})

% 取出上面的外层interleave部分继续推导
interleave(
    interleave(
        [{2, 2}, {2, 3}, {2, 4}, ...],
        all_pair([3, 4, ...], [1, 2, 3, 4, ...])
    ),
    [{1, 4}, ...]
)
<=> % 根据interleave定义替换内层interleave
interleave(
    cons_stream(
        {2, 2},
        interleave(
            all_pair([3, 4, ...], [1, 2, 3, 4, ...]), 
            [{2, 3}, {2, 4}, ...]
        )
    )
    [{1, 4}, ...]
)
<=> % 根据interleave定义替换外层interleave
cons_stream(
    {2, 2},
    interleave(
        [{1, 4}, ...],
        interleave(
            all_pair([3, 4, ...], [1, 2, 3, 4, ...]), 
            [{2, 3}, {2, 4}, ...]
        )
    )
)
=> stream({1, 1}, {1, 2}, {2, 1}, {1, 3}, {2, 2})

% 取出上面的外层interleave部分继续推导
interleave(
    [{1, 4}, ...],
    interleave(
        all_pair([3, 4, ...], [1, 2, 3, 4, ...]), 
        [{2, 3}, {2, 4}, ...]
    )
)
<=> % 根据interleave定义替换外层interleave
cons_stream(
    {1, 4},
    interleave(
        interleave(
            all_pair([3, 4, ...], [1, 2, 3, 4, ...]), 
            [{2, 3}, {2, 4}, ...]
        ),
        [{1, 5}, ...]
    )
)
=> stream({1, 1}, {1, 2}, {2, 1}, {1, 3}, {2, 2}, {1, 4})

% 取出上面的外层interleave部分继续推导
interleave(
    interleave(
        all_pair([3, 4, ...], [1, 2, 3, 4, ...]), 
        [{2, 3}, {2, 4}, ...]
    ),
    [{1, 5}, ...]
)
<=> % 根据interleave定义替换内层interleave
interleave(
    cons_stream(
        {3, 1},
        interleave(
            [{2, 3}, {2, 4}, ...],
            all_pair([4, ...], [1, 2, 3, 4, ...])
        )
    )
    [{1, 5}, ...]
)
<=> % 根据interleave定义替换外层interleave
cons_stream(
    {3, 1},
    interleave(
        [{1, 5}, ...],
        interleave(
            [{2, 3}, {2, 4}, ...],
            all_pair([4, ...], [1, 2, 3, 4, ...])
        )
    )
)
=> stream({1, 1}, {1, 2}, {2, 1}, {1, 3}, {2, 2}, {1, 4}, {3, 1})

% 取出上面的外层interleave部分继续推导
interleave(
    [{1, 5}, ...],
    interleave(
        [{2, 3}, {2, 4}, ...],
        all_pair([4, ...], [1, 2, 3, 4, ...])
    )
)
...
=> stream({1, 1}, {1, 2}, {2, 1}, {1, 3}, {2, 2}, {1, 4}, {3, 1}, {1, 5})

...