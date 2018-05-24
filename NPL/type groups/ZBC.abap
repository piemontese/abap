TYPE-POOL zbc .

       " function module parameter type
TYPES: BEGIN OF zbc_ty_s_fm_parameters,
         name  TYPE string,   " parameter name
         value TYPE string,   " parmeter value
       END OF zbc_ty_s_fm_parameters,
       " function module results type
       BEGIN OF zbc_ty_s_fm_results,
         t_params     TYPE abap_func_parmbind_tab,  " function parameters
         t_exceptions TYPE abap_func_excpbind_tab,  " function exceptions
       END OF zbc_ty_s_fm_results.

       " function module parrameters table type
TYPES: zbc_ty_t_fm_parameters TYPE TABLE OF zbc_ty_s_fm_parameters.