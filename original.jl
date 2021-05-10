import Base: +, -, *, /, show, lcm
using Base.Test;

Variable = Symbol
Monomial = Dict{Variable, Number}
Term = Pair{Monomial, Number}
Polynomial = Dict{Monomial, Number}

macro vars(vs...)
    return quote
        const $(esc(Expr(:tuple, vs...))) =
        $(map(v -> Polynomial(Monomial(v => 1) => 1), vs))
    end
end

function simplify(d::Dict)
    for (k, v) in d
        if v == 0
            delete!(d, k)
        end
    end
    return d
end

# Monomial
## Addition
function +(m::Monomial, n::Monomial)::Polynomial
    return Polynomial(m => 1) + Polynomial(n => 1)
end

function +(m::Monomial, n::Number)::Polynomial
    return Polynomial(m => 1) + Polynomial(Monomial() => n)
end

function +(n::Number, m::Monomial)::Polynomial
    return Polynomial(Monomial() => n) + Polynomial(m => 1)
end

## Substrction
function -(m::Monomial, n::Monomial)::Polynomial
    return Polynomial(m => 1) - Polynomial(n => 1)
end

function -(m::Monomial, n::Number)::Polynomial
    return Polynomial(m => 1) - Polynomial(Monomial() => n)
end

function -(n::Number, m::Monomial)::Polynomial
    return Polynomial(Monomial() => n) - Polynomial(m => 1)
end

## Multiplication
function *(m::Monomial, n::Monomial)::Monomial
    r = Monomial()
    for (x, a) in m
        r[x] = get(r, x, 0) + a
    end
    for (x, a) in n
        r[x] = get(r, x, 0) + a
    end
    return simplify(r)
end

function *(m::Monomial, n::Number)::Term
    return Term(m, 1) * Term(Monomial(), n)
end

function *(n::Number, m::Monomial)::Term
   return Term(Monomial(), n) *  Term(m, 1)
end

## Division
function /(m::Monomial, n::Monomial)::Monomial
    r = Monomial()
    for (x, a) in m
        r[x] = get(r, x, 0) + a
    end
    for (x, a) in n
        r[x] = get(r, x, 0) - a
    end
    return simplify(r)
end

function /(m::Monomial, n::Number)::Term
    return Term(m, 1) / Term(Monomial(), n)
end

function /(n::Number, m::Monomial)::Term
   return Term(Monomial(), n) /  Term(m, 1)
end

## Power
function ^(m::Monomial, e::Number)::Monomial
    r = Monomial()
    for (v,f) in m
        r[v] = f * e
    end
    return r
end

# Term
## Addition
function +(s::Term, t::Term)::Polynomial
    return Polynomial(s) + Polynomial(t)
end

function +(t::Term, m::Monomial)::Polynomial
    return Polynomial(t) - Polynomial(m => 1)
end

function +(m::Monomial, t::Term)::Polynomial
    return Polynomial(m => 1) - Polynomial(t)
end

function +(t::Term, n::Number)::Polynomial
    return Polynomial(t) + Polynomial(Monomial() => n)
end

function +(n::Number, t::Term)::Polynomial
    return Polynomial(Monomial() => n) + Polynomial(t)
end

## Substruction
function -(s::Term, t::Term)::Polynomial
    return Polynomial(s) - Polynomial(t)
end

function -(t::Term, m::Monomial)::Polynomial
    return Polynomial(t) - Polynomial(m => 1)
end

function -(m::Monomial, t::Term)::Polynomial
    return Polynomial(m => 1) - Polynomial(t)
end

function -(t::Term, n::Number)::Polynomial
    return Polynomial(t) - Polynomial(Monomial() => n)
end

function -(n::Number, t::Term)::Polynomial
    return Polynomial(Monomial() => n) - Polynomial(t)
end

## Multiplication
function *(s::Term, t::Term)::Term
    return Term(s[1]*t[1], s[2]*t[2])
end

function *(t::Term, m::Monomial)::Term
    return t * Term(m, 1)
end

function *(m::Monomial, t::Term)::Term
    return Term(m, 1) * t
end

function *(t::Term, n::Number)::Term
    return t * Term(Monomial(), n)
end

function *(n::Number, t::Term)::Term
    return Term(Monomial(), n) * t
end

## Division
function /(s::Term, t::Term)::Term
    return Term(s[1]/t[1], s[2]//t[2])
end

function /(t::Term, m::Monomial)::Term
    return t / Term(m, 1)
end

function /(m::Monomial, t::Term)::Term
    return Term(m, 1) / t
end

function /(t::Term, n::Number)::Term
    return t / Term(Monomial(), n)
end

function /(n::Number, t::Term)::Term
    return Term(Monomial(), n) / t
end

# Polynomial

## Addition
function +(p::Polynomial, q::Polynomial)::Polynomial
    r = Polynomial()
    for (m, c) in p
        r[m] = get(r, m, 0) + c
    end
    for (m, c) in q
        r[m] = get(r, m, 0) + c
    end
    return simplify(r)
end

function +(n::Number, p::Polynomial)::Polynomial
    return Polynomial(Monomial() => n) + p
end

function +(p::Polynomial, n::Number)::Polynomial
    return p + Polynomial(Monomial() => n)
end

function +(p::Polynomial, m::Monomial)::Polynomial
    return p + Polynomial(m => 1)
end

function +(m::Monomial, p::Polynomial)::Polynomial
    return Polynomial(m => 1) + p
end

function +(p::Polynomial, t::Term)::Polynomial
    return p + Polynomial(t)
end

function +(t::Term, p::Polynomial)::Polynomial
    return Polynomial(t) + p
end

## Substruction
function -(p::Polynomial, q::Polynomial)::Polynomial
    r = Polynomial()
    for (m, c) in p
        r[m] = get(r, m, 0) + c
    end
    for (m, c) in q
        r[m] = get(r, m, 0) - c
    end
    return simplify(r)
end

function -(n::Number, p::Polynomial)::Polynomial
    return Polynomial(Monomial() => n) - p
end

function -(p::Polynomial, n::Number)::Polynomial
    return p - Polynomial(Monomial() => n)
end

function -(p::Polynomial, m::Monomial)::Polynomial
    return p - Polynomial(m => 1)
end

function -(m::Monomial, p::Polynomial)::Polynomial
    return Polynomial(m => 1) - p
end

function -(p::Polynomial, t::Term)::Polynomial
    return p - Polynomial(t)
end

function -(t::Term, p::Polynomial)::Polynomial
    return Polynomial(t) - p
end

## Multiplication
function *(p::Polynomial, q::Polynomial)::Polynomial
    r = Polynomial()
    for (m, c) in p, (n, d) in q
        r[m * n] = get(r, m * n, 0) + c * d
    end
    return simplify(r)
end

function *(n::Number, p::Polynomial)::Polynomial
    return Polynomial(Monomial() => n) * p
end

function *(p::Polynomial, n::Number)::Polynomial
    return p * Polynomial(Monomial() => n)
end

function *(p::Polynomial, m::Monomial)::Polynomial
    return p * Polynomial(m => 1)
end

function *(m::Monomial, p::Polynomial)::Polynomial
    return Polynomial(m => 1) * p
end

function *(p::Polynomial, t::Term)::Polynomial
    return p * Polynomial(t)
end

function *(t::Term, p::Polynomial)::Polynomial
    return Polynomial(t) * p
end

## Power
function ^(p::Polynomial, n::Integer)::Polynomial
    r = Polynomial(Monomial() => 1)
    for i in 1:n
        r = r * p
    end
    return r
end

## Division
function /(p::Polynomial, t::Term)::Polynomial
    r = Polynomial()
    for (m, c) in p
        r[m / t[1]] = c * t[2]
    end
    return simplify(r)
end

## Display
function show(io::IO, m::Monomial)::Void
    xs = collect(m)
    if length(xs) == 0
        print(io, 1)
    end
    if length(xs) >= 1
        x, a = xs[1]
        if a == 1
            print(io, x)
        else
            print(io, "$x^$a")
        end
    end
    for (x, a) = xs[2:length(xs)]
        print(io, "*")
        if a == 1
            print(io, x)
        else
            print(io, "$x^$a")
        end
    end
end

function show(io::IO, p::Polynomial)::Void
    ts = collect(p)
    if length(ts) == 0
        print(io, 0)
    end
    if length(ts) >= 1
        m, c = ts[1]
        if c == -1
            if m == Monomial()
                print(io, "-1")
            else
                print(io, "-$m")
            end
        elseif c == 1
            if m == Monomial()
                print(io, "1")
            else
                print(io, "$m")
            end
        else
            print(io, "$c*$m")
        end
    end
    for (m, c) = ts[2:length(ts)]
        if c == -1
            if m == Monomial()
                print(io, "-1")
            else
                print(io, "-$m")
            end
        elseif c == 1
            if m == Monomial()
                print(io, "+1")
            else
                print(io, "+$m")
            end
        elseif c > 0
            print(io, "+$c*$m")
        else
            print(io, "$c*$m")
        end
    end
end

## Ordering
function lex(m::Monomial, n::Monomial)::Bool
    l = Monomial()
    for  (k, v) in n
        l[k] = get(l, k, 0) + v
    end
    for (k, v) in m
        l[k] = get(l, k, 0) - v
    end
    for k in reverse(sort(collect(keys(l))))
        if l[k] > 0
            return true
        end
        if l[k] < 0
            return false
        end
    end
    return false
end

function grlex(m::Monomial, n::Monomial)::Bool
    gm = sum(v for (k, v) in m)
    gn = sum(v for (k, v) in n)
    return gm == gn ? lex(m, n) : gm < gn
end

function revlex(m::Monomial, n::Monomial)::Bool
    l = Monomial()
    for (k, v) in n
        l[k] = get(l, k, 0) + v
    end
    for (k, v) in m
        l[k] = get(l, k, 0) - v
    end
    for k in sort(collect(keys(l)))
        if l[k] < 0
            return true
        end
        if l[k] > 0
            return false
        end
    end
    return false
end

function grevlex(m::Monomial, n::Monomial)::Bool
    gm = sum(v for (k, v) in m)
    gn = sum(v for (k, v) in n)
    return gm == gn ? revlex(m, n) : gm < gn
end

# Utilities
function lt(p::Polynomial, ord::Function=lex)::Term
    return reduce((s, t) -> ord(s[1], t[1]) ? t : s, p)
end

function lm(p::Polynomial, ord::Function=lex)::Monomial
    return lt(p, ord)[1]
end

function lc(p::Polynomial, ord::Function=lex)::Number
    return lt(p, ord)[2]
end

function remainder(f::Polynomial, fs...)
    divides(s::Term, t::Term) = all(xa -> xa[2] >= 0, (t / s)[1])
    qs = fill(Polynomial(), length(fs))
    r = Polynomial()
    p = f
    while p != Polynomial()
        i = 1
        divisionoccurred = false
        while i <= length(fs) && divisionoccurred == false
            if divides(lt(fs[i]), lt(p))
                qs[i] = qs[i] + lt(p) / lt(fs[i])
                p = p - (lt(p) / lt(fs[i])) * fs[i]
                divisionoccurred = true
            else
                i = i + 1
            end
        end
        if divisionoccurred == false
            r = r + lt(p)
            p = p - lt(p)
        end
    end
    return r
end

function lcm(m::Monomial, n::Monomial)::Monomial
    r = Monomial()
    for (x, a) in m
        r[x] = max(get(r, x, 0), a)
    end
    for (x, a) in n
        r[x] = max(get(r, x, 0), a)
    end
    return simplify(r)
end

function S(f::Polynomial, g::Polynomial)::Polynomial
    xr = lcm(lm(f),lm(g))
    return xr / lt(f) * f - xr / lt(g) * g
end

function groebner(F...)
    G=[F...]
    while true
        Gp = G
        for p in Gp, q in Gp
            if p != q
                r = remainder(S(p, q), Gp...)
                if r != Polynomial()
                    G = [r, G...]
                end
            end
        end
        if G == Gp
            return G
        end
    end
end

@vars x y z
@time println(groebner(x*z-y^2, x^3-z^2))
