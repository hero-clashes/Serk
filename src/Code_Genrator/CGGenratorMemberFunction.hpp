#include "CGMemberFunction.hpp"
class CGGenratorMemberFunction: public CGMemberFunction{
    public:
    CGGenratorMemberFunction(CGCompileUnit &CGM,CGClass &CGC):CGMemberFunction(CGM,CGC){};

    llvm::FunctionType *
        createFunctionType(FunctionDeclaration *Proc);
    void run(FunctionDeclaration *Proc);
    bool emitSpecialStmt(Stmt *S);

    llvm::Value* Return_Promise;
    llvm::BasicBlock *c;
    llvm::BasicBlock *s;
    llvm::BasicBlock *f_s;
};