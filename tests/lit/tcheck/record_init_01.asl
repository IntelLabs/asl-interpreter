// RUN: not %asli --nobanner %s | %decolor | filecheck %s

record R(M) {
    x : bits(M);
};

func S4(r : R(4)) => bits(4)
begin
    return r.x;
end

func T()
begin
    let t = S4(R{x='111'});
// CHECK: Type error: wrong number of type parameters
end
