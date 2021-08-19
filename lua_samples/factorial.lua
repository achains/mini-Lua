function fact (n)
    if n == 0 then
        return 1
    else 
        return n * fact (n - 1)
    end
end


data = {1, 2, 3, 4, 5, 6, 7}
s = 0
for i = 1,7 do
    s = s + fact(data[i])
end

-- AST for this code should look like this: 
-- Function(["n"], If(Eq(Var("n"), Num(0)), Ret(Num(1)) ))