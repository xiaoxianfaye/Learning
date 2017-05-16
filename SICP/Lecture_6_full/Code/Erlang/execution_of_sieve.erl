
% 2 3 4 5 6 7 8 9 10 
% 11 12 13 14 15 16 17 18 19 20 
% 21 22 23 24 25 26 27 28 29 …
sieve(integers_from(2))

cons_stream(
    2,
    sieve(
        filter_stream(
            N rem 2 =/= 0,
            cons_stream(
                3,
                integers_from(4)))))

cons_stream(
    2,
    sieve(
        cons_stream(
            3,
            filter_stream(
                N rem 2 =/= 0,
                integers_from(4)))))
                
cons_stream(
    2,
    cons_stream(
        3,
        sieve(
            filter_stream(
                N rem 3 =/= 0,
                filter_stream(
                    N rem 2 =/= 0,
                    integers_from(4))))))

cons_stream(
    2,
    cons_stream(
        3,
        sieve(
            filter_stream(
                N rem 3 =/= 0,
                cons_stream(
                    5,
                    filter_stream(
                        N rem 2 =/= 0,
                        integers_from(6)))))))

cons_stream(
    2,
    cons_stream(
        3,
        sieve(
            cons_stream(
                5,
                filter_stream(
                    N rem 3 =/= 0,
                        filter_stream(
                            N rem 2 =/= 0,
                            integers_from(6)))))))

cons_stream(
    2,
    cons_stream(
        3,
        cons_stream(
            5,
            sieve(
                filter_stream(
                    N rem 5 =/= 0,
                    filter_stream(
                        N rem 3 =/= 0,
                            filter_stream(
                                N rem 2 =/= 0,
                                integers_from(6))))))))




