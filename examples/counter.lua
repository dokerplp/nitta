function counter(a1, b1, c1)
    a2 = a1 + 1
    b2 = b1 + 1
    c2 = c1 + 1
    counter(a2, b2, c2)
end
counter(0, 1, 2)
