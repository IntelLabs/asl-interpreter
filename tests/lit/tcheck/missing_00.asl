// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func T()
begin
    UndefinedFunction();
// CHECK: Type error: Unknown procedure UndefinedFunction
end
