def make_vec(_xcor, _ycor):
    return [_xcor, _ycor]

def xcor(vec):
    return head(vec)

def ycor(vec):
    return tail(vec)

def add(vec1, vec2):
    return make_vec(xcor(vec1) + xcor(vec2), ycor(vec1) + ycor(vec2))

def scale(c, vec):
    return make_vec(c * xcor(vec), c * ycor(vec))

def head(pair):
    return pair[0]

def tail(pair):
    return pair[1]