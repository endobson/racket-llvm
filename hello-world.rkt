#lang racket

(require "llvm.rkt")

(define context (LLVMContextCreate))
(define module (LLVMModuleCreateWithNameInContext "gcd-module" context))
(define int-type (LLVMInt32TypeInContext context))
(define fun-type1 (LLVMFunctionType int-type (list int-type int-type) 2 false))
(define fun-type2 (LLVMFunctionType int-type (list int-type int-type int-type) 3 false))
(define gcd-fun (LLVMAddFunction module "gcd" fun-type1))
(define mul-add-fun (LLVMAddFunction module "mul-add" fun-type2))

(let ()
 (define x (LLVMGetParam mul-add-fun 0))
 (define y (LLVMGetParam mul-add-fun 1))
 (define z (LLVMGetParam mul-add-fun 2))
 (define entry  (LLVMAppendBasicBlockInContext context mul-add-fun "entry"))
 (define builder (LLVMCreateBuilderInContext context))

 (LLVMSetValueName x "x")
 (LLVMSetValueName y "y")
 (LLVMSetValueName z "z")
 (LLVMPositionBuilderAtEnd builder entry)
 (let* ((a (LLVMBuildMul builder x y "a"))
        (b (LLVMBuildAdd builder z a "b")))
   (LLVMBuildRet builder b))

 (LLVMDisposeBuilder builder)
 )

#|
unsigned gcd(unsigned x, unsigned y) {
  if(x == y) {
    return x;
  } else if(x < y) {
    return gcd(x, y - x);
  } else {
    return gcd(x - y, y);
  }
} 
 |#
(let ()
 (define x (LLVMGetParam gcd-fun 0))
 (define y (LLVMGetParam gcd-fun 1))
 (define test-zero-block
   (LLVMAppendBasicBlockInContext context gcd-fun "test-zero"))
 (define test-less-block 
   (LLVMAppendBasicBlockInContext context gcd-fun "test-less"))
 (define zero-block
   (LLVMAppendBasicBlockInContext context gcd-fun "zero"))
 (define less-block
   (LLVMAppendBasicBlockInContext context gcd-fun "less"))
 (define greater-block
   (LLVMAppendBasicBlockInContext context gcd-fun "greater"))
 (define builder (LLVMCreateBuilderInContext context))
 (define zero (LLVMConstInt int-type 0 false))

 (LLVMSetValueName x "x")
 (LLVMSetValueName y "y")
 (LLVMPositionBuilderAtEnd builder test-zero-block)
 (let ((cond (LLVMBuildICmp builder 'LLVMIntEQ x zero "cond")))
   (LLVMBuildCondBr builder cond zero-block test-less-block))

 (LLVMPositionBuilderAtEnd builder test-less-block)
 (let ((cond (LLVMBuildICmp builder 'LLVMIntULT x y "cond")))
   (LLVMBuildCondBr builder cond less-block greater-block))

 (LLVMPositionBuilderAtEnd builder zero-block)
 (LLVMBuildRet builder y)
 (LLVMPositionBuilderAtEnd builder less-block)
 (let* ((z (LLVMBuildSub builder y x "z"))
        (ans (LLVMBuildCall builder gcd-fun (list x z) "ans")))
  (LLVMBuildRet builder ans))
 (LLVMPositionBuilderAtEnd builder greater-block)
 (let* ((z (LLVMBuildSub builder x y "z"))
        (ans (LLVMBuildCall builder gcd-fun (list y z) "ans")))
  (LLVMBuildRet builder ans))

 (LLVMDumpModule module)
 (LLVMDisposeBuilder builder)
 )

(let-values (((err) (LLVMVerifyModule module 'LLVMReturnStatusAction)))
 (when err
   (display err) (exit 1)))

(define arg1 (LLVMCreateGenericValueOfInt int-type 4592 #t))
(define arg2 (LLVMCreateGenericValueOfInt int-type 60 #t))
(LLVMLinkInJIT)
(define ee (LLVMCreateExecutionEngineForModule module))

(define output (LLVMRunFunction ee gcd-fun (list arg1 arg2)))
(LLVMGenericValueToInt output #t)

;(LLVMWriteBitcodeToFile module "tmp")
;(define pass-manager (LLVMCreatePassManager))

