import functools

def helloworld(n):

    def helloworld_temp(fun):

        @functools.wraps(fun)
        def hello(*args, **kwargs):
            for i in range(n):
                print("Hello, world!")
            return(fun(*args, **kwargs))
        
        return hello

    return helloworld_temp

@helloworld
def f(a, b):
    "zwraca sume elementow"
    print(a+b)


def licznik(n):

    def zwieksz():
        nonlocal n
        n += 1

    def zmniejsz():
        nonlocal n
        n -= 1

    def stan():
        return n
    
    return zwieksz, zmniejsz, stan


inc, dec, get = licznik(10)



# 2

def add(x, y):
    return x + y

def add_tuple(tup):
    x, y = tup
    return x + y



def curry(fun):
    
    def curry_fun(x, y):
        return fun((x, y))

    return curry_fun


def uncurry(fun):
    
    def uncurry_fun(tup):
        x, y = tup
        return fun(x, y)
    
    return uncurry_fun


# 3

# def compose(f, g):
    
#     def compose_fun(x):
#         return f(g(x))

#     return compose_fun


def compose(*args):
    
    def composed(n):
        for func in reversed(args):
            x = func(x)
        return x
    
    return composed



# 10
def print_debug(f):
    
    def pd_f(*args):
        result = f(*args)
        print(f"{f.__name__}(", end='')
        print(*map(repr, args), sep=', ', end=' ') 
        print(f") = {result!r}")
        return result

    return pd_f


# @print_debug
# def fib(n):
#     if n < 2:
#         return n
#     return fib(n - 1) + fib(n - 2)


@print_debug
def g(a, b, c):
    return a * b + c

@print_debug
def suma(a , b, c):
    return a + b + c



# 11
def print_debg_p(sep=" = "):

    def print_debug_hlp(f):
        
        def pd_f(*args):
            result = f(*args)
            print(f"{f.__name__}(", end='')
            print(*map(repr, args), sep=', ', end=' ') 
            print(f") {sep} {result!r}")
            return result

        return pd_f
    
    return print_debug_hlp


@print_debug_p(2)
def fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)


# 14

def flip(func):
    
    def flipped_func(*args, **kwargs):
        return func(*reversed(args), **kwargs)

    return func