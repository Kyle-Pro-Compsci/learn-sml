fun allChange(coins, coinvals, 0, res) = coins :: res
| allChange(coins, [], amount, res) = res
| allChange(coins, c::coinvals, amount, res) =
    if amount < 0 then res
    else allChange(coins, coinvals, amount, allChange(c::coins, c::coinvals, amount-c, res));

allChange([], [5,2], 16, []);
allChange([], [25,10,2,1], 43, []);