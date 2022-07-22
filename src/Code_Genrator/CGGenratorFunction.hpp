#include "CGFunction.hpp"
class CGGenratorFunction: public CGFunction{
    public:
    CGGenratorFunction(CGCompileUnit &CGM):CGFunction(CGM){};

    llvm::FunctionType *
        createFunctionType(FunctionDeclaration *Proc);
    void run(FunctionDeclaration *Proc);
    bool emitSpecialStmt(Stmt *S);

    llvm::Value* Return_Promise;
    llvm::BasicBlock *c;
    llvm::BasicBlock *s;
    llvm::BasicBlock *f_s;
};