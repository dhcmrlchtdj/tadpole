module T = Types

let validate_functype (t : T.functype) : bool = false
let validate_func (t : T.func) : bool = false
let validate_table (t : T.table) : bool = false
let validate_mem (t : T.mem) : bool = false
let validate_global (t : T.global) : bool = false
let validate_elem (t : T.elem) : bool = false
let validate_data (t : T.data) : bool = false
let validate_start (t : T.start) : bool = false
let validate_import (t : T.import) : bool = false
let validate_export (t : T.export) : bool = false

let validate_module : T.modules -> bool =
   fun ( functypes,
         funcs,
         tables,
         mems,
         globals,
         elem,
         data,
         start,
         imports,
         exports ) ->
       false
