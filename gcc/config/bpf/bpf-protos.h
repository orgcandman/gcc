/* Definitions of target machine for BPF architecture.
   Copyright (C) 2013-2015 PLUMgrid Inc
   Copyright (C) 2017 Aaron Conole

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


#ifndef GCC_BPF_PROTOS_H
#define GCC_BPF_PROTOS_H

#include "input.h"

#ifdef RTX_CODE
void output_call_value (rtx *, int, int);
char *bpf_output_compare (rtx *, rtx_insn *, bpf_cc_opcode_type);
char *bpf_output_jump (rtx, rtx_insn *);
void bpf_print_operand (FILE *, rtx, char);
void print_operand_address (FILE *, rtx);
void bpf_output_symbol_ref (FILE *, rtx);
#ifdef TREE_CODE
rtx bpf_function_value (const_tree, const_tree);
void bpf_output_section_name (FILE *, tree, const char *, int);
#endif
#endif

void bpf_output_vars_start (FILE *);
void bpf_output_vars_end (FILE *);

void bpf_output_source_filename (FILE *, const char *);
void bpf_output_internal_label (FILE *, const char *);

void bpf_asm_file_start (FILE *);
void bpf_asm_file_end (FILE *);
void bpf_output_common (FILE *, const char *, int, int);
void bpf_output_local (FILE *, const char *, int, int);
void bpf_output_skip (FILE *, int size);
void bpf_output_align (FILE *, int align);
void bpf_output_label (FILE *, const char *);
void bpf_output_labelref (FILE *, const char *);
void bpf_output_label_ref (FILE *, const char *);
void bpf_output_ascii (FILE *, const char *, int);

#endif /* GCC_BPF_PROTOS_H */
