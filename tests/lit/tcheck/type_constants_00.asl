// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F{A}(A : integer, src : bits(A)) => boolean
begin
    var B : integer;
    B = A;
    return Zeros(A) == Zeros(B);
// CHECK: Type error: type width parameter B does not match A
end
