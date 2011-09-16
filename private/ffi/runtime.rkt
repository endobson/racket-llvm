#lang racket

(require
  "lib.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;Execution Engine

(define-llvm-multiple (LLVMLinkInJIT LLVMLinkInInterpreter) (_fun -> _void))

(define LLVMGenericValueRef _pointer)
(define LLVMExecutionEngineRef _pointer)

;/*===-- Operations on generic values --------------------------------------===*/

(define-llvm LLVMCreateGenericValueOfInt
 (_fun LLVMTypeRef _ulong LLVMBool -> LLVMGenericValueRef))

(define-llvm LLVMCreateGenericValueOfPointer
 (_fun _pointer -> LLVMGenericValueRef))

(define (LLVMCreateGenericValueOfFunctionType fun-type)
 (get-ffi-obj 'LLVMCreateGenericValueOfPointer llvm-lib 
  (_fun fun-type -> LLVMGenericValueRef)))

(define-llvm LLVMCreateGenericValueOfFloat
 (_fun LLVMTypeRef _double* -> LLVMGenericValueRef))

(define-llvm LLVMGenericValueIntWidth
 (_fun LLVMGenericValueRef -> _uint))

(define-llvm LLVMGenericValueToInt
 (_fun LLVMGenericValueRef LLVMBool -> _long))

(define-llvm LLVMGenericValueToPointer
 (_fun LLVMGenericValueRef -> _pointer))

(define-llvm LLVMGenericValueToFloat
 (_fun LLVMTypeRef LLVMGenericValueRef -> _double*))

(define-llvm LLVMDisposeGenericValue
 (_fun LLVMGenericValueRef -> _void))


;/*===-- Operations on execution engines -----------------------------------===*/

(define-llvm LLVMCreateExecutionEngineForModule
 (_fun (module) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm LLVMCreateInterpreterForModule
 (_fun (module) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm LLVMCreateJITCompilerForModule
 (_fun (module opt) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (opt : _uint)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm-multiple 
 (LLVMDisposeExecutionEngine
  LLVMRunStaticConstructors
  LLVMRunStaticDestructors)
 (_fun LLVMExecutionEngineRef -> _void))


;TODO support env
(define-llvm LLVMRunFunctionAsMain
 (_fun (ee fun args) ::
       (ee : LLVMExecutionEngineRef)
       (fun : LLVMValueRef)
       (_uint = (length args))
       (args : (_list i _string))
       (env : (_list i _string) = (list #f))
       ->
       _sint))


(define-llvm LLVMRunFunction
 (_fun (engine function args) ::
       (engine : LLVMExecutionEngineRef)
       (function : LLVMValueRef)
       (_uint = (length args))
       (args : (_list i LLVMGenericValueRef))
       ->
       LLVMGenericValueRef))


(define-llvm LLVMAddModule (_fun LLVMExecutionEngineRef LLVMModuleRef -> _void))

(define-llvm LLVMFreeMachineCodeForFunction
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _void))

(define-llvm LLVMRemoveModule
 (_fun (ee module) ::
       (ee : LLVMExecutionEngineRef)
       (module : LLVMModuleRef)
       (outmod : (_ptr o LLVMModuleRef))
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message outmod)))

(define-llvm LLVMFindFunction
 (_fun (ee name) ::
       (ee : LLVMExecutionEngineRef)
       (name : _string)
       (outfun : (_ptr o LLVMValueRef))
       -> (err : LLVMBool)
       -> (if err #f outfun)))


(define-llvm LLVMRecompileAndRelinkFunction
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))

(define-llvm LLVMGetExecutionEngineTargetData
 (_fun LLVMExecutionEngineRef -> LLVMTargetDataRef))

(define-llvm LLVMAddGlobalMapping
 (_fun LLVMExecutionEngineRef LLVMValueRef _pointer -> _void))

(llvm-unsafe-context
 (define (LLVMAddGlobalMappingForFunction fun-type)
  (get-ffi-obj 'LLVMAddGlobalMapping llvm-lib 
   (_fun LLVMExecutionEngineRef LLVMValueRef fun-type -> LLVMGenericValueRef))))

(define-llvm LLVMGetPointerToGlobal
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))
