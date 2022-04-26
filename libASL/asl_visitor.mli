(****************************************************************
 * ASL visitor class
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 *
 * This code follows the pattern used in the cilVisitor class in
 * George Necula's excellent CIL (https://people.eecs.berkeley.edu/~necula/cil/)
 * and makes use of the generic Visitor module that is copied from CIL.
 ****************************************************************)

(** ASL visitor class *)

open Asl_ast
open Visitor

class type aslVisitor = object

    method vvar      : ident          -> ident          visitAction
    method ve_elsif  : e_elsif        -> e_elsif        visitAction
    method vslice    : slice          -> slice          visitAction
    method vpattern  : pattern        -> pattern        visitAction
    method vexpr     : expr           -> expr           visitAction
    method vconstraint: constraint_range -> constraint_range visitAction
    method vtype     : ty             -> ty             visitAction
    method vlvar     : ident          -> ident          visitAction
    method vlexpr    : lexpr          -> lexpr          visitAction
    method vstmt     : stmt           -> stmt           visitAction
    method vs_elsif  : s_elsif        -> s_elsif        visitAction
    method valt      : alt            -> alt            visitAction
    method vcatcher  : catcher        -> catcher        visitAction
    method vmapfield : mapfield       -> mapfield       visitAction
    method vsformal  : sformal        -> sformal        visitAction
    method vdpattern : decode_pattern -> decode_pattern visitAction
    method vencoding : encoding       -> encoding       visitAction
    method vdcase    : decode_case    -> decode_case    visitAction
    method vdalt     : decode_alt     -> decode_alt     visitAction
    method vdbody    : decode_body    -> decode_body    visitAction
    method vdecl     : declaration    -> declaration    visitAction

    method enter_scope : ident list -> unit
    method leave_scope : ident list -> unit
end

val visit_alt              : aslVisitor -> alt                      -> alt
val visit_arg              : aslVisitor -> (ident * ty)             -> (ident * ty)
val visit_args             : aslVisitor -> (ident * ty) list        -> (ident * ty) list
val visit_catcher          : aslVisitor -> catcher                  -> catcher
val visit_constraint_range : aslVisitor -> constraint_range         -> constraint_range
val visit_constraints      : aslVisitor -> constraint_range list    -> constraint_range list
val visit_decl             : aslVisitor -> declaration              -> declaration
val visit_decode_alt       : aslVisitor -> decode_alt               -> decode_alt
val visit_decode_body      : aslVisitor -> decode_body              -> decode_body
val visit_decode_case      : aslVisitor -> decode_case              -> decode_case
val visit_dpattern         : aslVisitor -> decode_pattern           -> decode_pattern
val visit_e_elsif          : aslVisitor -> e_elsif                  -> e_elsif
val visit_encoding         : aslVisitor -> encoding                 -> encoding
val visit_expr             : aslVisitor -> expr                     -> expr
val visit_exprs            : aslVisitor -> expr list                -> expr list
val visit_lexpr            : aslVisitor -> lexpr                    -> lexpr
val visit_lexprs           : aslVisitor -> lexpr list               -> lexpr list
val visit_lvar             : aslVisitor -> ident                    -> ident
val visit_mapfield         : aslVisitor -> mapfield                 -> mapfield
val visit_parameter        : aslVisitor -> (ident * ty option)      -> (ident * ty option)
val visit_parameters       : aslVisitor -> (ident * ty option) list -> (ident * ty option) list
val visit_pattern          : aslVisitor -> pattern                  -> pattern
val visit_patterns         : aslVisitor -> pattern list             -> pattern list
val visit_s_elsif          : aslVisitor -> s_elsif                  -> s_elsif
val visit_sformal          : aslVisitor -> sformal                  -> sformal
val visit_slice            : aslVisitor -> slice                    -> slice
val visit_stmt             : aslVisitor -> stmt                     -> stmt
val visit_stmts            : aslVisitor -> stmt list                -> stmt list
val visit_type             : aslVisitor -> ty                       -> ty
val visit_types            : aslVisitor -> ty list                  -> ty list
val visit_var              : aslVisitor -> ident                    -> ident

class nopAslVisitor : aslVisitor
