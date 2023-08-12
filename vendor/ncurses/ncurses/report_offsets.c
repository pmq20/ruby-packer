/****************************************************************************
 * Copyright (c) 2017 Free Software Foundation, Inc.                        *
 *                                                                          *
 * Permission is hereby granted, free of charge, to any person obtaining a  *
 * copy of this software and associated documentation files (the            *
 * "Software"), to deal in the Software without restriction, including      *
 * without limitation the rights to use, copy, modify, merge, publish,      *
 * distribute, distribute with modifications, sublicense, and/or sell       *
 * copies of the Software, and to permit persons to whom the Software is    *
 * furnished to do so, subject to the following conditions:                 *
 *                                                                          *
 * The above copyright notice and this permission notice shall be included  *
 * in all copies or substantial portions of the Software.                   *
 *                                                                          *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
 * IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
 * THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
 *                                                                          *
 * Except as contained in this notice, the name(s) of the above copyright   *
 * holders shall not be used in advertising or otherwise to promote the     *
 * sale, use or other dealings in this Software without prior written       *
 * authorization.                                                           *
 ****************************************************************************/

/****************************************************************************
 *  Author: Thomas E. Dickey                                                *
 ****************************************************************************/

#include <curses.priv.h>

MODULE_ID("$Id: report_offsets.c,v 1.5 2017/06/03 13:52:28 tom Exp $")

#define show_size(type) \
	printf("%6ld\t" #type "\n", (long)sizeof(type))
#define show_offset(type,member) \
	printf("%6ld\t" #type "." #member "\n", (long)offsetof(type,member))

int
main(void)
{
    printf("Size/offsets of data structures:\n");

    show_size(attr_t);
    show_size(chtype);
#if NCURSES_WIDECHAR
    show_size(cchar_t);
#endif
    show_size(mmask_t);
    show_size(MEVENT);
    show_size(NCURSES_BOOL);

    printf("\n");
    show_size(SCREEN);
    show_offset(SCREEN, _panelHook);
#if USE_REENTRANT
    show_offset(SCREEN, _ttytype);
#endif
#ifdef TRACE
    show_offset(SCREEN, tracechr_buf);
#endif
#ifdef USE_SP_WINDOWLIST
    show_offset(SCREEN, _windowlist);
#endif
    show_offset(SCREEN, rsp);
#if NCURSES_EXT_FUNCS
#if USE_NEW_PAIR
    show_offset(SCREEN, _ordered_pairs);
#endif
#if NCURSES_SP_FUNCS
    show_offset(SCREEN, use_tioctl);
#endif
#endif
#if USE_WIDEC_SUPPORT
    show_offset(SCREEN, _screen_acs_fix);
#endif

    printf("\n");
    show_size(TERMINAL);
    show_offset(TERMINAL, type);
    show_offset(TERMINAL, Filedes);
#if defined(TERMIOS)
    show_offset(TERMINAL, Ottyb);
    show_offset(TERMINAL, Nttyb);
#endif
    show_offset(TERMINAL, _baudrate);
    show_offset(TERMINAL, _termname);
#if NCURSES_EXT_COLORS && HAVE_INIT_EXTENDED_COLOR
    show_offset(TERMINAL, type2);
#endif

    printf("\n");
    show_size(TERMTYPE);
#if NCURSES_XNAMES
    show_offset(TERMTYPE, ext_str_table);
    show_offset(TERMTYPE, ext_Strings);
#endif

    printf("\n");
    show_size(WINDOW);
#if NCURSES_WIDECHAR
    show_offset(WINDOW, _bkgrnd);
#if NCURSES_EXT_COLORS
    show_offset(WINDOW, _color);
#endif
#endif
    return EXIT_SUCCESS;
}
