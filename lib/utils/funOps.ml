let (<<) f g x = f(g(x))

let (<<..) f g x y = f(g x y)