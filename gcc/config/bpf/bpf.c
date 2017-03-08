/* Machine descriptions for BPF architecture.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "calls.h"
#include "stor-layout.h"
#include "varasm.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-codes.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "hashtab.h"
#include "hash-set.h"
#include "vec.h"
#include "machmode.h"
#include "input.h"
#include "function.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "predict.h"
#include "dominance.h"
#include "cfg.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "basic-block.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "hash-map.h"
#include "is-a.h"
#include "plugin-api.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "hash-table.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimplify.h"
#include "cfgloop.h"
#include "dwarf2.h"
#include "df.h"
#include "builtins.h"

static bool bpf_output_integer (rtx value, size_t size, int aligned_p);
static void output_function_prologue (FILE * file, HOST_WIDE_INT size);
static void output_function_epilogue (FILE * file, HOST_WIDE_INT size);
static void bpf_output_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED);
static void bpf_output_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED);
static void bpf_pretty_output_ascii (FILE *file, const char *ptr, size_t len);

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP NULL

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP NULL

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE output_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE output_function_epilogue

#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR bpf_output_constructor

#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR bpf_output_destructor

#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER bpf_output_integer

extern tree current_function_decl;


char bpf_label_name [4096]; /* name of the current label (variable) printed */
int bpf_label_isprinted;  /* did we print the header for label (variable) definition */
int bpf_cur_align = 4; /* current alignment for output */
unsigned char *vars_buf; /* array for output initial values of variable 'bpf_label_name' */
int vars_buf_pos; /* current position in array */

static char strtab[2048];
static size_t last_strtab_idx = 1;

static size_t
get_strtab (const char *func)
{
  size_t len = strlen(func) + 1;
  size_t i;

  for (i = 0; i < last_strtab_idx; ++i)
	if (!memcmp(&strtab[i], func, len))
	  return i;

  if (last_strtab_idx + len > sizeof(strtab))
	fatal_error(input_location, "strtab overflow\n");

  memcpy(&strtab[i], func, len);
  last_strtab_idx += len;
  return i;
}

void
bpf_output_common (FILE *file, const char *name, int size, int align)
{
  if (name[0] == '*')
    name ++;

  fprintf (file, "/* aligned (%d) unsigned int %s[%u] */\n",
           (align >> 3), name, (size + 3) >> 2);
}

void
bpf_output_local (FILE *file, const char *name, int size, int align)
{
  if (name[0] == '*')
	name ++;

  fprintf (file, "/* aligned (%d) unsigned int %s[%u] */\n",
		   (align >> 3), name, (size + 3) >> 2);
}

static void
bpf_init (void)
{
  static int once;

  if (once)
	return;
  once = 1;
  vars_buf = (unsigned char *)xmalloc (1024*1024);
}

static void
bpf_function_arg_advance (cumulative_args_t cum_v)
{
  CUMULATIVE_ARGS *c = get_cumulative_args (cum_v);
  (*c)++;
}

static rtx
bpf_function_arg (cumulative_args_t cum_v)
{
  CUMULATIVE_ARGS *c = get_cumulative_args (cum_v);
  return gen_rtx_REG (mode, *c);
}

static rtx
bpf_function_incoming_arg (cumulative_args_t cum_v)
{
  CUMULATIVE_ARGS *c = get_cumulative_args (cum_v);
  return gen_rtx_REG (mode, *c);
}

static void
bpf_asm_file_start ()
{
  bpf_init ();

  fputs ("/* BPF insns */\n"
		 "#include \"linux/filter.h\"\n", asm_out_file);
}

static void
bpf_asm_file_end ()
{
  bpf_output_vars_end (asm_out_file);
  fprintf (asm_out_file, "const char func_strtab[%zu] = \"", last_strtab_idx);
  bpf_pretty_output_ascii (asm_out_file, strtab, last_strtab_idx);
  fprintf (asm_out_file, "\";\n");
}

void
bpf_output_source_filename (FILE *f, const char *filename)
{
  fprintf (f, "/* file '%s' */\n", filename);
}

static void
output_function_prologue (FILE *file, HOST_WIDE_INT lsize)
{
  const char *bpf_current_func_name;
  tree decl = current_function_decl;
  size_t size, i;

  if (DECL_ASSEMBLER_NAME (decl))
	{
	  bpf_current_func_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	}

  fprintf (file, "struct bpf_insn bpf_insns_%s[] = {\n", bpf_current_func_name);

  fprintf (file, "/* Save regs:");
  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
	if (df_regs_ever_live_p(i) && !call_used_regs[i])
	  fprintf (file, " %s", reg_names[i]);
  fprintf (file, " */\n");

  size = (lsize + 7) & ~7;

  if (size)
	{
	  fprintf (file, "/* Allocate %zu bytes stack\n */\n", size);
	}
  else if (df_regs_ever_live_p(STACK_POINTER_REGNUM))
	{
	  fprintf (file, "/* zero sized stack */\n");
	}
}

static void
output_return (FILE *file, size_t size)
{
  const char *ret_type_str;
  tree res;
  fprintf (file, "\tBPF_INSN_RET(),");
  res = TREE_TYPE (TREE_TYPE (current_function_decl));

  switch (TREE_CODE (res))
	{
	case POINTER_TYPE:
	  ret_type_str = "ptr";
	  break;
	case VOID_TYPE:
	  ret_type_str = "void";
	  break;
	case BOOLEAN_TYPE:
	  ret_type_str = "bool";
	  break;
	case INTEGER_TYPE:
	  {
		static char type_buf[15];

		sprintf (type_buf, "u%d", TREE_INT_CST_LOW (TYPE_SIZE (res)));
		ret_type_str = type_buf;
	  }
	  break;
	default:
	  error ("Type '%T' is not supported", res);
	}
  fprintf (file, "%s", ret_type_str);
  fprintf (file, " /* pop %zu words */\n", size);
}

static void
output_function_epilogue (FILE *file, HOST_WIDE_INT size)
{
  output_return (file, size);
  fprintf (file, "\t{0,0,0,0,0};\n");
  bpf_label_name[0] = 0;
}

static void
print_operand_address_1 (FILE *file, rtx addr)
{
  rtx op0 = XEXP (addr, 0);
  rtx op1 = XEXP (addr, 1);
  rtx *op, *in;

  if (GET_CODE (op0) == REG && CONST_INT_P (op1))
	{
	  op = &op0;
	  in = &op1;
	}
  else if (GET_CODE (op1) == REG && CONST_INT_P (op0))
	{
	  op = &op1;
	  in = &op0;
	}
  else
	{
	  fatal_insn ("unrecognizable address", addr);
	}

  fprintf (file, "%s, %ld", reg_names[REGNO (*op)], INTVAL (*in));
}

void
print_operand_address (FILE *file, rtx addr)
{
  switch (GET_CODE (addr))
	{
	case MEM:
	case LABEL_REF:
	  fatal_insn ("unsupported LABEL_REF operand addr", addr);
	  break;

	case REG:
	  fprintf (file, "%s, 0", reg_names[REGNO (addr)]);
	  break;

	case PLUS:
	  print_operand_address_1 (file, addr);
	  break;

	default:
	  output_addr_const (file, addr);
	}
}

void
bpf_output_internal_label (FILE *file, const char *prefix)
{
  bpf_output_vars_end (f);
  fprintf (file, "\n/*%s:*/\n", prefix);
}

void
bpf_output_label (FILE *file, const char *name)
{
  bpf_output_vars_end (file);

  if (name[0] == '*')
	name ++;

  fprintf (file, "/* asm_output_label %s */\n", name);
  if (bpf_label_name[0] != 0)
	{
	  fprintf (file, "/* overriden %s label */\n", bpf_label_name);
	}
  strncpy (bpf_label_name, name, sizeof (bpf_label_name));
  bpf_label_isprinted = 0;
}

void
output_call_value (rtx *op, int is_value, int unique_func_num)
{
  static char fmt[128];
  const char *func_name;

  if (GET_CODE (XEXP (op[is_value], 0)) == SYMBOL_REF)
	{
	  func_name = XSTR (XEXP (op[is_value], 0), 0);
	}
  else
	{
	  fatal_insn ("unsupported call", op[is_value]);
	}

  if (func_name[0] == '*')
	func_name ++;

  snprintf (fmt, sizeof (fmt), "BPF_INSN_CALL(%d), /* %s%%%d(); */\n",
			get_strtab (func_name), is_value ? "%0=" : "(void)", is_value);

  output_asm_insn (fmt, op);
}

static const char * cc_opcode_names[] = {
  "==",
  "!=",
  ">",
  ">/*unsign*/",
  "<",
  "</*unsign*/",
  ">=",
  ">=/*unsign*/",
  "<=",
  "<=/*unsign*/",
};

static bpf_cc_opcode_type
bpf_translate_branch_normal (enum rtx_code code)
{
  switch (code)
	{
	case EQ: return CC_EQ;
	case NE: return CC_NE;
	case LT: return CC_LT;
	case LE: return CC_LE;
	case GT: return CC_GT;
	case GE: return CC_GE;
	case LTU: return CC_LTU;
	case LEU: return CC_LEU;
	case GTU: return CC_GTU;
	case GEU: return CC_GEU;
	default:
	  fatal_error (input_location,
				   "bpf_translate_branch_normal: unknown cmp mode %d\n", code);
	}
}

static bpf_cc_opcode_type
bpf_translate_branch_reverse (enum rtx_code code)
{
  switch (code)
	{
	case EQ: return CC_NE;
	case NE: return CC_EQ;
	case LT: return CC_GE;
	case LE: return CC_GT;
	case GT: return CC_LE;
	case GE: return CC_LT;
	case LTU: return CC_GEU;
	case LEU: return CC_GTU;
	case GTU: return CC_LEU;
	case GEU: return CC_LTU;
	default:
	  fatal_error (input_location,
				   "bpf_translate_branch_reverse: unknown cmp mode %d\n", code);
	}
}

const char *
bpf_translate_branch_rtx (enum rtx_code code)
{
  switch (code)
	{
	case EQ: return "BPF_JEQ";
	case NE: return "BPF_JNE";
	case GT: return "BPF_JSGT";
	case GE: return "BPF_JSGE";
	case GTU: return "BPF_JGT";
	case GEU: return "BPF_JGE";
	default:
	  fatal_error (input_location,
				   "bpf_translate_branch_rtx: unknown cmp mode %d\n", code);
	}
}

#define USE_OR_CLOBBER_P(insn)					\
  (GET_CODE (insn) == INSN						\
   && (GET_CODE (PATTERN (insn)) == USE ||		\
	   GET_CODE (PATTERN (insn)) == CLOBBER))

static int
find_delta (rtx_insn *insn, rtx code_label)
{
  int p_cnt = 0, n_cnt = 0;
  rtx_insn *p = insn;
  rtx_insn *n = p;

  for (;;)
	{
	  if (n == code_label)
		return n_cnt - 1;
	  else if (p == code_label)
		return -p_cnt;

	  if (p)
		{
		  if (!NOTE_P (p) && !DEBUG_INSN_P (p) && !BARRIER_P (p) &&
			  !LABEL_P (p) && !USE_OR_CLOBBER_P (p))
			p_cnt ++;
		  p = PREV_INSN (p);
		}

	  if (n)
		{
		  if (!NOTE_P (n) && !DEBUG_INSN_P (n) && !BARRIER_P (n) &&
			  !LABEL_P (n) && !USE_OR_CLOBBER_P (n))
			n_cnt ++;
		  n = NEXT_INSN (n);
		}

	  if (!p && !n)
		break;
	}

  return 0;
}

char *
bpf_output_jump (rtx code_label, rtx_insn *insn)
{
  static char buf[128];
  int delta = find_delta (insn, code_label);

  snprintf (buf, sizeof (buf), "BPF_INSN_JUMP(BPF_JA, 0, 0, %d), /*GO %%l0*/",
			delta);
  return buf;
}

char *
bpf_output_compare (rtx *operands, rtx_insn *insn, bpf_cc_opcode_type opcode)
{
  static char buf[256];
  enum rtx_code code = GET_CODE (operands[3]);

  opcode = (opcode == CC_REVERSE) ?
	bpf_translate_branch_reverse (GET_CODE (operands[3])) :
	bpf_translate_branch_normal (code);

  if (int_reg_operand (operands[0], DImode) &&
	  int_reg_or_const_operand (operands[1], DImode))
	{
	  int delta = find_delta (insn, operands[2]);
	  if (delta == 0)
		fatal_insn ("invalid jump", insn);

	  snprintf(buf, sizeof (buf), "BPF_INSN_JUMP%s(%s, %%0, %%1, %d), \n",
			   const_int_operand (operands[1], DImode) ? "_IMM" : "",
			   bpf_translate_branch_rtx (code), delta, cc_opcode_names[opcode]);
	}
  else
	{
	  fatal_insn ("output_compare: unknown mode", operands[0]);
	}

  return buf;
}

void
bpf_print_operand (FILE *file, rtx op, char code)
{
  switch (GET_CODE (op))
	{
	case REG:
	  fprintf (file, "%s", reg_names[REGNO (op)]);
	  break;
	case MEM:
	  output_address (XEXP (op, 0));
	  break;
	case CONST_DOUBLE:
	case LABEL_REF:
	  fatal_insn ("unsupported", op);
	  break;
	default:
	  output_addr_const (file, op);
	}
}

static void
bpf_output_constructor (rtx symbol, int priority)
{
  fatal_insn ("unsupported constructor", symbol);
}

static void
bpf_output_destructor (rtx symbol, int priority)
{
  fatal_insn ("unsupported destructor", symbol);
}

static void
bpf_output_vars_buf (FILE *file)
{
  int i;

  for (i = 0; i < vars_buf_pos >> 2; ++i)
	{
	  unsigned val = ((unsigned *)vars_buf)[i];
	  fprintf (file, "0x%x,", val);

	  if (i%50 == 0)
		fprintf (file, "\n");
	}

  vars_buf_pos &= 3;

  ((unsigned *)vars_buf)[0] = ((unsigned *)vars_buf)[i];
}

void
bpf_output_vars_start (FILE *file)
{
  if (bpf_label_isprinted == 0)
	{
	  fprintf (file, "unsigned int bpf_%s[] = {\n", bpf_label_name);
	  bpf_label_isprinted = 1;
	  vars_buf_pos = 0;
	}
  else if (vars_buf_pos)
	{
	  bpf_output_vars_buf (file);
	}
}

void
bpf_output_vars_end (FILE *file)
{
  if (bpf_label_isprinted)
    {
      if (vars_buf_pos)
        {
          while ((vars_buf_pos & 3) != 0)
            vars_buf[vars_buf_pos ++] = 0;
          bpf_output_vars_buf (f);
        }
      fprintf (file, "};\n");
      bpf_label_isprinted = 0;
      bpf_label_name[0] = 0;
    }
}

void
bpf_output_align (FILE *file, int align)
{
  int delta = vars_buf_pos & ((1 << align) - 1);

  fprintf (file, "/*.align %d*/\n", 1 << align);
  bpf_cur_align = 1 << align;

  if (bpf_label_isprinted && delta != 0)
    {
      int i;
      for (i = 0; i < (1 << align) - delta; i++)
        vars_buf[vars_buf_pos ++] = 0;

      bpf_output_vars_buf (file);
    }
}

#define BPF_OUTPUT_INT(VAL)												\
  if (BYTES_BIG_ENDIAN)													\
	{																	\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL>>24);			\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL>>16);			\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL>>8);				\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL);				\
	}																	\
  else																	\
	{																	\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL);				\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL>>8);				\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL>>16);			\
	  vars_buf[vars_buf_pos ++] = (unsigned char) (VAL>>24);			\
	}


static void
bpf_output_int (FILE* f, rtx value)
{
  if (GET_CODE (value) == CONST_INT)
    {
      int val = INTVAL (value);
      fprintf (f, "0x%x, /* int */\n", val);
    }
  else
    {
     fatal_insn ("unsupported int32 const", value);
    }
}

static void
bpf_output_double_int (FILE* file, rtx value)
{
  if (GET_CODE (value) == CONST_INT)
    {
      bpf_output_int (file, value);
    }
 else
    {
     output_addr_const (file, value);
      fprintf (file, "  // unsupported int64 const\n");
    }
}

static void
bpf_output_short (FILE* f, rtx value)
{
  if (GET_CODE (value) == CONST_INT)
    {
     union { unsigned char uc[2]; unsigned short us; } d;
      d.us = INTVAL(value);

      fprintf (f, "/* short ");
      output_addr_const (f, value);
      fprintf (f, "*/\n");

      vars_buf[vars_buf_pos++] = d.uc[0];
      vars_buf[vars_buf_pos++] = d.uc[1];
    }
  else
    {
      fatal_insn ("unsupported int16", value);
    }
}

static void
bpf_output_char (FILE* f, struct rtx_def* value)
{
  if (GET_CODE (value) == CONST_INT)
    {
      fprintf (f, "/*char*/\n");
      vars_buf [vars_buf_pos++] = INTVAL (value);
    }
  else
    {
      fatal_insn ("unsupported int8", value);
    }
}

static bool
bpf_output_integer (rtx value, unsigned int size, int aligned_p)
{
  bpf_output_vars_start (asm_out_file);

  fprintf (asm_out_file, "  /*size=%d aligned=%d*/ ", size, aligned_p);

  if (!aligned_p)
    {
      int val;

      if (size == 8)
        {
          if (GET_CODE (value) == CONST_INT)
            {
              if (INTVAL (value) < 0)
                val = -1;
              else
                val = 0;

              BPF_OUTPUT_INT (val);

              val = INTVAL (value);

              BPF_OUTPUT_INT (val);
            }
          else
            fatal_insn ("unsupported output", value);
          return true;
        }

      if (GET_CODE (value) != CONST_INT)
        fatal_insn ("notaligned relocation", value);

      val = INTVAL (value);

      fprintf (asm_out_file, "/*int=%d*/\n", val);

      if (size == 4)
        {
          BPF_OUTPUT_INT (val);
        }
      else if (size == 2)
        {
          if (BYTES_BIG_ENDIAN)
            {
              vars_buf [vars_buf_pos++] = (unsigned char) (val>>8);
              vars_buf [vars_buf_pos++] = (unsigned char) (val);
            }
          else
            {
              vars_buf [vars_buf_pos++] = (unsigned char) (val);
              vars_buf [vars_buf_pos++] = (unsigned char) (val>>8);
            }
        }
      else if (size == 1)
        {
          vars_buf [vars_buf_pos++] = (unsigned char) (val);
        }
    }
  else
    {
      if (size == 8)
        bpf_output_double_int (asm_out_file, value);

      else if (size == 4)
        bpf_output_int (asm_out_file, value);

      else if (size == 2)
        bpf_output_short (asm_out_file, value);

      else if (size == 1)
        bpf_output_char (asm_out_file, value);
    }

  return true;
}

void
bpf_output_symbol_ref (FILE *file, rtx addr)
{
  const char *s = XSTR (addr, 0);
  assemble_name (file, s);
}

void
bpf_output_labelref (FILE* f, const char* s)
{
  asm_fprintf (f, "%U%s", s);
}

void
bpf_output_label_ref (FILE* f, const char* s)
{
  asm_fprintf (f, "// unsupported %s", s + (s[0] == '*'));
}

void
bpf_output_skip (FILE* f, int size)
{
  int i;
  bpf_output_vars_start (f);

  for (i = 0; i < size; i++)
    vars_buf[vars_buf_pos++] = 0;

  fprintf (f, "/*skip %d*/\n", size);
}

static void
bpf_pretty_output_ascii(FILE *file, const char *ptr, int len)
{
  const char *s;
  int i;
  for (i = 0, s = (const char *)ptr; i < len - 1; s++, i++)
    {
      if ( ISPRINT (*s) && *s != '"' && *s != '\\')
        fprintf (file, "%c", *s);

      else if ( *s == 10)
        fputs ("\\n", file);

      else if ( *s == '\\')
        fputs ("\\\\", file);

      else if (*s > 0)
        fprintf (file, "\\0%o", (unsigned int)(unsigned char)(*s));

      else
        fprintf (file, "\\%o", (unsigned int)(unsigned char)(*s));
    }
}

void
bpf_output_ascii (FILE* file, const char * ptr, int len)
{
  const char *s;
  int i;

  fprintf (file, "\n#if 0\nchar %s[%d] = \"", bpf_label_name, len);

  bpf_pretty_output_ascii(file, ptr, len);
  fprintf (file, "\";\n#endif\n");

  bpf_output_vars_start (file);

  for (i = 0, s = (const char *)ptr; i < len; s++, i++)
    {
      vars_buf[vars_buf_pos++] = *s;
    }
}

static void
bpf_asm_named_section (const char *name, unsigned int flags ATTRIBUTE_UNUSED, tree decl)
{
  FILE *file = asm_out_file;
  bpf_output_vars_end (file);

  if (decl && TREE_CODE (decl) == FUNCTION_DECL)
    fprintf (file, "/* func_section '%s' */\n", name);
  else
    fprintf (file, "/* var_section '%s' */\n", name);
}

static void
bpf_asm_globalize_label (FILE *file, const char *name)
{
  if (name[0] == '*')
    name ++;

  fprintf (file, "/* set global '%s' */\n", name);
}

rtx
bpf_function_value (const_tree type, const_tree fntype_or_decl)
{
  enum machine_mode mode;
  int unsignedp;

  mode = TYPE_MODE (type);
  if (INTEGRAL_TYPE_P (type))
    mode = promote_function_mode (type, mode, &unsignedp, fntype_or_decl, 1);

  return gen_rtx_REG (mode, 0);
}

static bool
bpf_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			 enum machine_mode mode, const_tree type,
			 bool named ATTRIBUTE_UNUSED)
{
  return ((type && AGGREGATE_TYPE_P (type)) || COMPLEX_MODE_P (mode));
}

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
static bool
bpf_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED)
{
  return true;
}

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack. */
static int
bpf_return_pops_args (tree fundecl ATTRIBUTE_UNUSED, tree funtype ATTRIBUTE_UNUSED,
                      size_t size)
{
  return size;
}

static bool
bpf_frame_pointer_required (void)
{
  return true;
}

static bool
bpf_mode_dependent_address_p (const_rtx addr ATTRIBUTE_UNUSED,
                              addr_space_t as ATTRIBUTE_UNUSED)
{
  return true;
}

static bool
bpf_scalar_mode_supported_p (enum machine_mode mode)
{
  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
    case DImode:
    case TImode:
      return true;

    default:
      return false;
    }
}

static bool
bpf_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
			   int opno ATTRIBUTE_UNUSED, int *total,
			   bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case CONST_INT:
      if (INTVAL (x) > 0x7fffFFFF || INTVAL (x) < -1 - 0x7fffFFFF)
        *total = 32;
      else
        *total = 0;
      return true;
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = 4;
      return true;
    case CONST_DOUBLE:
      *total = 8;
      return true;
    case MULT:
      *total = COSTS_N_INSNS (10);
      return true;
    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (40);
      return true;
    case FIX:
    case FLOAT:
      *total = 19;
      return true;
    default:
      return false;
    }
}

static enum machine_mode
bpf_promote_function_mode (const_tree type, enum machine_mode mode,
                           int *punsignedp, const_tree fntype,
                           int for_return)
{
  if (type != NULL_TREE && POINTER_TYPE_P (type))
    {
      *punsignedp = POINTERS_EXTEND_UNSIGNED;
      return Pmode;
    }

  return default_promote_function_mode (type, mode, punsignedp, fntype,
					for_return);
}

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P bpf_legitimate_constant_p

#undef TARGET_RETURN_POPS_ARGS
#define TARGET_RETURN_POPS_ARGS bpf_return_pops_args

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED bpf_frame_pointer_required

#undef TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P bpf_mode_dependent_address_p

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE bpf_pass_by_reference

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL      bpf_globalize_label

#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P bpf_scalar_mode_supported_p

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE bpf_function_arg_advance

#undef TARGET_FUNCTION_INCOMING_ARG
#define TARGET_FUNCTION_INCOMING_ARG bpf_function_incoming_arg

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG bpf_function_arg

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION bpf_asm_named_section

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS bpf_rtx_costs

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE bpf_promote_function_mode

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL bpf_asm_globalize_label

struct gcc_target targetm = TARGET_INITIALIZER;
