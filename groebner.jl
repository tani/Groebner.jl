using Symbolics
import SymbolicUtis:Mul,Add

function lex(m::Mul, n::Mul)::Bool

end

function grlex(m::Mul, n::Mul)::Bool
    
end

function revlex(m::Mul, n::Mul)::Bool
    
end

function grevlex(m::Mul, n::Mul)::Bool
    
end

function leading_term(m::Add, ord::Function=lex)::Mul

end

function leading_monomial(m::Add, ord::Function=lex)::Mul

end

function leading_coefficient(m::Add, ord::Function=lex)::Number

end

function div(f::Add, fs...)

end
