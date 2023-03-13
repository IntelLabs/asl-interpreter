// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func IntFunction(x : integer)
begin
end

func T()
begin
    IntFunction(TRUE);
// CHECK: Type error: function arguments
end
