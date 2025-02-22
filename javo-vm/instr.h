typedef enum {
    ILOAD_NULL = 0,
    ILOAD_INT = 1,
    ILOAD_LOCAL = 2,
    IADD = 3,
    INEW_OBJECT = 4,
    IMETHOD_CALL = 5,
    ILOCAL_ASSIGN = 6,
    IFIELD_ASSIGN = 7,
    IFETCH = 8,
    IPOP = 9,
    IBRANCH = 10,
    IRETURN = 11,
    IEXIT = 12,
    IPOP_LET = 13,
    ISUB = 14,
    IMUL = 15,
    IEQ = 16,
    IJUMP = 17,
} instr;
