// Test file to check ASL formatting

// A record
record R {
    integer f1;
    integer f2;
};

// A global variable
bits(32) _PC;

// A variable getter
bits(32) PC
    return _PC;

// A variable setter
PC = bits(32) val
    _PC = val;

// An array-style getter
bits(32) IP[]
    return _PC;

// An array-style setter
IP[] = bits(32) val
    _PC = val;

// A procedure
P1()
    PC = Zeros(); // comment after the procedure

    // comment before a statement (and after a blank line)
    PC = PC + 1;

// A function declaration
Unreachable();

// A case statement
bits(2) T1(bits(2) x)
    case x of // C1
        when '00' PC = PC + 1; // C2
        when '01' && PC == Zeros(32) => // C3
            // C4
            PC = PC - 1;
        when '1x' // C5
            x = T1(NOT x);
            x = T1(x + 1);
        otherwise // C6
            Unreachable();
    // C7
    return x; // C8

T2(bits(2) x);

// An if statement
T2(bits(2) x)
    if x == '00' then // C1
        print(x);
    elsif UInt(x) > 2 then // C2
        print("Big");
    else // C3
        // C4
        print("NonZero");

// A for loop
T3(integer x)
    // C1
    for i = 1 to 10 // C2
        // C3
        print(DecStr(i));
        // C4

    return;

// A while loop
T4(integer x)
    // C1
    while x > 0 do // C2
        // C3
        print(DecStr(x));
        x = x - 1;
        // C4

    return;

// A repeat loop
T4(integer x)
    // C1
    repeat // C2
        // C3
        print(DecStr(x));
        x = x - 1;
    until x <= 0; // C4
    // C5
    return;

// A try-catch block
T5(integer x)
    // C1
    try // C2
        // C3
        print(DecStr(x)); // C4
    catch exn // C5
        when IsUNDEFINED(exn) // C6
            print("UNDEF");
        when IsUNDEFINED(exn) // C7
            print("UNDEF");
        otherwise // C8
            print("Not undef");
    // C9

    return;
