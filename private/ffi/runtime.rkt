#lang racket

(require
  "lib.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;Execution Engine

(define-llvm-multiple-unsafe (LLVMLinkInJIT LLVMLinkInInterpreter) (_fun -> _void))


;/*===-- Operations on generic values --------------------------------------===*/

(define-llvm-unsafe LLVMCreateGenericValueOfInt
 (_fun LLVMTypeRef _ulong LLVMBool -> LLVMGenericValueRef))

(define-llvm-unsafe LLVMCreateGenericValueOfPointer
 (_fun _pointer -> LLVMGenericValueRef))

(define (unsafe:LLVMCreateGenericValueOfFunctionType fun-type)
 (get-ffi-obj 'LLVMCreateGenericValueOfPointer llvm-lib 
  (_fun fun-type -> LLVMGenericValueRef)))

(define-llvm-unsafe LLVMCreateGenericValueOfFloat
 (_fun LLVMTypeRef _double* -> LLVMGenericValueRef))

(define-llvm-unsafe LLVMGenericValueIntWidth
 (_fun LLVMGenericValueRef -> _uint))

(define-llvm-unsafe LLVMGenericValueToInt
 (_fun LLVMGenericValueRef LLVMBool -> _long))

(define-llvm-unsafe LLVMGenericValueToPointer
 (_fun LLVMGenericValueRef -> _pointer))

(define-llvm-unsafe LLVMGenericValueToFloat
 (_fun LLVMTypeRef LLVMGenericValueRef -> _double*))

(define-llvm-unsafe LLVMDisposeGenericValue
 (_fun LLVMGenericValueRef -> _void))


(define-llvm-safe LLVMCreateGenericValueOfInt
 (_fun safe:LLVMTypeRef _ulong LLVMBool -> safe:LLVMGenericValueRef))

(define-llvm-safe LLVMCreateGenericValueOfPointer
 (_fun _pointer -> safe:LLVMGenericValueRef))

(define (safe:LLVMCreateGenericValueOfFunctionType fun-type)
 (get-ffi-obj 'LLVMCreateGenericValueOfPointer llvm-lib 
  (_fun fun-type -> safe:LLVMGenericValueRef)))

(define-llvm-safe LLVMCreateGenericValueOfFloat
 (_fun safe:LLVMTypeRef _double* -> safe:LLVMGenericValueRef))

(define-llvm-safe LLVMGenericValueIntWidth
 (_fun safe:LLVMGenericValueRef -> _uint))

(define-llvm-safe LLVMGenericValueToInt
 (_fun safe:LLVMGenericValueRef LLVMBool -> _long))

(define-llvm-safe LLVMGenericValueToPointer
 (_fun safe:LLVMGenericValueRef -> _pointer))

(define-llvm-safe LLVMGenericValueToFloat
 (_fun safe:LLVMTypeRef safe:LLVMGenericValueRef -> _double*))



;/*===-- Operations on execution engines -----------------------------------===*/

(define-llvm-unsafe LLVMCreateExecutionEngineForModule
 (_fun (module) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm-unsafe LLVMCreateInterpreterForModule
 (_fun (module) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm-unsafe LLVMCreateJITCompilerForModule
 (_fun (module opt) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (opt : _uint)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))


(define-llvm-safe LLVMCreateExecutionEngineForModule
  safe:LLVMExecutionEngineCreator)

(define-llvm-safe LLVMCreateJITCompilerForModule
  safe:LLVMJITCreator)


(define-llvm-multiple-unsafe 
 (LLVMDisposeExecutionEngine
  LLVMRunStaticConstructors
  LLVMRunStaticDestructors)
 (_fun LLVMExecutionEngineRef -> _void))


;TODO support env
(define-llvm-unsafe LLVMRunFunctionAsMain
 (_fun (ee fun args) ::
       (ee : LLVMExecutionEngineRef)
       (fun : LLVMValueRef)
       (_uint = (length args))
       (args : (_list i _string))
       (env : (_list i _string) = (list #f))
       ->
       _sint))


(define-llvm-unsafe LLVMRunFunction
 (_fun (engine function args) ::
       (engine : LLVMExecutionEngineRef)
       (function : LLVMValueRef)
       (_uint = (length args))
       (args : (_list i LLVMGenericValueRef))
       ->
       LLVMGenericValueRef))


(define-llvm-safe LLVMRunFunction
 (_fun (engine function args) ::
       (engine : safe:LLVMExecutionEngineRef)
       (function : safe:LLVMValueRef)
       (_uint = (length args))
       (args : (_list i safe:LLVMGenericValueRef))
       ->
       safe:LLVMGenericValueRef))





(define-llvm-unsafe LLVMAddModule (_fun LLVMExecutionEngineRef LLVMModuleRef -> _void))

(define-llvm-unsafe LLVMFreeMachineCodeForFunction
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _void))

(define-llvm-unsafe LLVMRemoveModule
 (_fun (ee module) ::
       (ee : LLVMExecutionEngineRef)
       (module : LLVMModuleRef)
       (outmod : (_ptr o LLVMModuleRef))
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message outmod)))

(define-llvm-unsafe LLVMFindFunction
 (_fun (ee name) ::
       (ee : LLVMExecutionEngineRef)
       (name : _string)
       (outfun : (_ptr o LLVMValueRef))
       -> (err : LLVMBool)
       -> (if err #f outfun)))


(define-llvm-unsafe LLVMRecompileAndRelinkFunction
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))

(define-llvm-unsafe LLVMGetExecutionEngineTargetData
 (_fun LLVMExecutionEngineRef -> LLVMTargetDataRef))

(define-llvm-unsafe LLVMAddGlobalMapping
 (_fun LLVMExecutionEngineRef LLVMValueRef _pointer -> _void))

;TODO fix this
#;
(define (LLVMAddGlobalMappingForFunction fun-type)
 (get-ffi-obj 'LLVMAddGlobalMapping llvm-lib 
  (_fun LLVMExecutionEngineRef LLVMValueRef fun-type -> LLVMGenericValueRef)))

(define-llvm-unsafe LLVMGetPointerToGlobal
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))
