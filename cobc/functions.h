/*
 * Copyright (C) 2002 Keisuke Nishida
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 */

COBC_DEFINE_FUNCTION (COB_ACCEPT, "cob_accept", 1)
COBC_DEFINE_FUNCTION (COB_ACCEPT_COMMAND_LINE, "cob_accept_command_line", 1)
COBC_DEFINE_FUNCTION (COB_ACCEPT_DATE, "cob_accept_date", 1)
COBC_DEFINE_FUNCTION (COB_ACCEPT_DAY, "cob_accept_day", 1)
COBC_DEFINE_FUNCTION (COB_ACCEPT_DAY_OF_WEEK, "cob_accept_day_of_week", 1)
COBC_DEFINE_FUNCTION (COB_ACCEPT_ENVIRONMENT, "cob_accept_environment", 2)
COBC_DEFINE_FUNCTION (COB_ACCEPT_TIME, "cob_accept_time", 1)
COBC_DEFINE_FUNCTION (COB_ADD, "cob_add", 3)
COBC_DEFINE_FUNCTION (COB_CALL_ERROR, "cob_call_error", 0)
COBC_DEFINE_FUNCTION (COB_CANCEL, "cob_cancel", 1)
COBC_DEFINE_FUNCTION (COB_CLOSE, "cob_close", 1)
COBC_DEFINE_FUNCTION (COB_DELETE, "cob_delete", 2)
COBC_DEFINE_FUNCTION (COB_DIVIDE, "cob_divide", 3)
COBC_DEFINE_FUNCTION (COB_EXIT_PROGRAM, "cob_exit_program", 0)
COBC_DEFINE_FUNCTION (COB_NEWLINE, "cob_newline", 0)
COBC_DEFINE_FUNCTION (COB_OPEN, "cob_open", 2)
COBC_DEFINE_FUNCTION (COB_PUSH, "cob_push_decimal", 1)
COBC_DEFINE_FUNCTION (COB_READ, "cob_read", 2)
COBC_DEFINE_FUNCTION (COB_READ_NEXT, "cob_read_next", 1)
COBC_DEFINE_FUNCTION (COB_REWRITE, "cob_rewrite", 2)
COBC_DEFINE_FUNCTION (COB_START, "cob_start", 3)
COBC_DEFINE_FUNCTION (COB_STOP_RUN, "cob_stop_run", 0)
COBC_DEFINE_FUNCTION (COB_SUB, "cob_sub", 3)
COBC_DEFINE_FUNCTION (COB_WRITE, "cob_write", 2)
COBC_DEFINE_FUNCTION (COB_WRITE_LINES, "cob_write_lines", 2)
COBC_DEFINE_FUNCTION (COB_WRITE_PAGE, "cob_write_page", 1)
COBC_DEFINE_INLINE (COB_CALL, output_call_statement, 3)
COBC_DEFINE_INLINE (COB_DISPLAY, output_display, 1)
COBC_DEFINE_INLINE (COB_GOTO, output_goto, 1)
COBC_DEFINE_INLINE (COB_GOTO_DEPENDING, output_goto_depending, 2)
COBC_DEFINE_INLINE (COB_INITIALIZE, output_initialize, 1)
COBC_DEFINE_INLINE (COB_INSPECT_CONVERTING, output_inspect_converting, 2)
COBC_DEFINE_INLINE (COB_INSPECT_REPLACING, output_inspect_replacing, 2)
COBC_DEFINE_INLINE (COB_INSPECT_TALLYING, output_inspect_tallying, 2)
COBC_DEFINE_INLINE (COB_MOVE, output_move, 2)
COBC_DEFINE_INLINE (COB_SEARCH, output_search, 3)
COBC_DEFINE_INLINE (COB_SEARCH_AT_END, output_search_at_end, 2)
COBC_DEFINE_INLINE (COB_SET_TRUE, output_set_true, 1)
COBC_DEFINE_INLINE (COB_STRING, output_string, 2)
COBC_DEFINE_INLINE (COB_UNSTRING, output_unstring, 2)
