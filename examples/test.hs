import SDD.SDD

imp x y m = do
    n <- neg x m
    disjoin n y m

main = do

    m <- managerCreate 10 0

    a <- managerLiteral 1 m
    b <- managerLiteral 2 m

    conj <- conjoin a b m

    impl <- imp conj a m
    isTrue <- nodeIsTrue impl
    print $ isTrue

    impl <- imp b a m
    isTrue <- nodeIsTrue impl
    print $ isTrue

    saveAsDot "sdd.dot" conj
