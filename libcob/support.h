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

/*
 * Frame stack
 */

struct cob_frame {
  int perform_through;
  void *return_address;
};

#define cob_perform(id,from,until)			\
  do {							\
    frame_index++;					\
    frame_stack[frame_index].perform_through = until;	\
    frame_stack[frame_index].return_address = &&l_##id;	\
    goto from;						\
    l_##id:						\
    frame_index--;					\
  } while (0)

#define cob_exit(label)					\
 if (frame_stack[frame_index].perform_through == label)	\
   goto *frame_stack[frame_index].return_address;

#define COB_DEFAULT_ERROR_HANDLE				\
  cob_default_error_handle (cob_last_file);

#define COB_STANDARD_ERROR_HANDLE				\
  switch (cob_last_file->open_mode)				\
    {								\
    case COB_OPEN_INPUT:					\
      cob_perform (1, lb_input_handler, le_input_handler);	\
      break;							\
    case COB_OPEN_OUTPUT:					\
      cob_perform (2, lb_output_handler, le_output_handler);	\
      break;							\
    case COB_OPEN_I_O:						\
      cob_perform (3, lb_i_o_handler, le_i_o_handler);		\
      break;							\
    case COB_OPEN_EXTEND:					\
      cob_perform (4, lb_extend_handler, le_extend_handler);	\
      break;							\
    }

#define COB_INITIAL_PERFORM_ID	5

#define COB_INDEX(i,max,name) ((i) - 1)
#define COB_INDEX_DEPENDING(i,min,max,dep,name,depname) ((i) - 1)

#define cob_cmp(x,y) ((x) - (y))

#define cob_ref(var,off,len) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (len); var; })

#define cob_ref_rest(var,off,siz) \
  ({ int cob_ref_off = (off) - 1, cob_ref_len = (siz) - cob_ref_off; var; })

#define cob_exit_program() return cob_return_code;

#endif /* COB_SUPPORT_H_ */
