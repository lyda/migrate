/*
 * Copyright (C) 2002 Keisuke Nishida
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#ifndef COB_SUPPORT_H
#define COB_SUPPORT_H

#include <libcob/cobconfig.h>

/*
 * Frame stack
 */

#if COB_HAVE_COMPUTED_GOTO

/* GCC's computed-goto version */

struct cob_frame {
  int perform_through;
  void *return_address;
};

#define cob_perform(id,from,until)			\
  frame_index++;					\
  frame_stack[frame_index].perform_through = until;	\
  frame_stack[frame_index].return_address = &&l_##id;	\
  goto from;						\
  l_##id:						\
  frame_index--

#define cob_exit(label)					\
 if (frame_stack[frame_index].perform_through == label)	\
   goto *frame_stack[frame_index].return_address;

#else /* !COB_HAVE_COMPUTED_GOTO */

/* regular switch version */

struct cob_frame {
  int perform_through;
  int perform_id;
};

#define cob_perform(id,from,until)			\
  frame_index++;					\
  frame_stack[frame_index].perform_through = until;	\
  frame_stack[frame_index].perform_id = id;		\
  goto from;						\
  l_##id:						\
  frame_index--

#define cob_exit(label)					\
 if (frame_stack[frame_index].perform_through == label)	\
   goto l_exit;

#endif /* !COB_HAVE_COMPUTED_GOTO */

#define cob_exit_program()				\
  return cob_return_code;

#define cob_standard_error_handle(id1,id2,id3,id4,f)			\
  if (f.open_mode == COB_OPEN_INPUT)					\
    {									\
      cob_perform (id1, lb_input_handler, le_input_handler);		\
    }									\
  else if (f.open_mode == COB_OPEN_OUTPUT)				\
    {									\
      cob_perform (id2, lb_output_handler, le_output_handler);		\
    }									\
  else if (f.open_mode == COB_OPEN_I_O)					\
    {									\
      cob_perform (id3, lb_i_o_handler, le_i_o_handler);		\
    }									\
  else if (f.open_mode == COB_OPEN_EXTEND)				\
    {									\
      cob_perform (id4, lb_extend_handler, le_extend_handler);		\
    }

#define COB_INDEX(i,max) ((i) - 1)
#define COB_INDEX_DEPENDING(i,min,max,dep) ((i) - 1)

#define cob_ref(var,off,len) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (len); var; })

#define cob_ref_rest(var,off,siz) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (siz) - cob_ref_off; var; })

#define cob_cmp(x,y) ((x) - (y))

#endif /* COB_SUPPORT_H_ */
