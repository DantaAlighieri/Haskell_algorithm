safe i j i' j' =
    (i /= i' && 
    j /= j' && 
    i - i' /= j -j' &&
    i - i' /= j' - j) || 
    (i == i' && j == j')
    