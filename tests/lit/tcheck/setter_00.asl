// RUN: not %asli --nobanner %s | %decolor | filecheck %s

setter A[] = value : integer;

func T()
begin
    A[] = TRUE;
// CHECK: Type error: type integer does not match boolean
end
