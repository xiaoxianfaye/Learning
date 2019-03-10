import math

def make_vec(_xcor, _ycor):
    return [_xcor, _ycor]

def xcor(vec):
    return head(vec)

def ycor(vec):
    return tail(vec)

def make_seg(start_vec, end_vec):
    return [start_vec, end_vec]

def seg_start(seg):
    return head(seg)

def seg_end(seg):
    return tail(seg)

def middle_point(seg):
    start_vec = seg_start(seg)
    end_vec = seg_end(seg)

    return make_vec(average(xcor(start_vec), xcor(end_vec)),
                    average(ycor(start_vec), ycor(end_vec)))

def seg_length(seg):
    start_vec = seg_start(seg)
    end_vec = seg_end(seg)

    xlength = xcor(start_vec) - xcor(end_vec)
    ylength = ycor(start_vec) - ycor(end_vec)
    return int(math.sqrt(square(xlength) + square(ylength)))

def head(pair):
    return pair[0]

def tail(pair):
    return pair[1]

def average(x, y):
    return (x + y) / 2

def square(x):
    return x * x