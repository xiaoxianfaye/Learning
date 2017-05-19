
%% Normal-Order
fact(N) -> fact(N, 1).

fact(0, R) -> R;
fact(N, R) -> fact(N -1, R * N).

fact(N, R) when N > 0 ->
    fact(N - 1, R * N);
fact(N, R) when N =:= 0 ->
    R.
   