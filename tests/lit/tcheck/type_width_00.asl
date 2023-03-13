// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F{A}(A : integer, src : bits(A))
begin
end

func Test()
begin
    let src = Zeros(10);
    F(5, src);
// CHECK: Type error: type width parameter 10 does not match 5
end
