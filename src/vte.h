/*
 * Copyright (C) 2001,2002,2003,2008,2009,2010 Red Hat, Inc.
 *
 * This is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef vte_vte_h_included
#define vte_vte_h_included

#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <cairo-xlib.h>
#include <glib/gi18n-lib.h>
#include <glib/gstdio.h>
#include <termcap.h>

#include <math.h>
#include <locale.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <regex.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <termios.h>
#include <stropts.h>
#include <assert.h>
#include <ctype.h>
#include <wchar.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/termios.h>


#define __VTE_VTE_H_INSIDE__ 1

#include "vtepty.h"
#include "vteversion.h"

#undef __VTE_VTE_H_INSIDE__

G_BEGIN_DECLS


/**
 * vteunistr:
 *
 * vteunistr is a gunichar-compatible way to store strings.  A string
 * consisting of a single unichar c is represented as the same value
 * as c itself.  In that sense, gunichars can be readily used as
 * vteunistrs.  Longer strings can be built by appending a unichar
 * to an already existing string.
 *
 * vteunistr is essentially just a gunicode-compatible quark value.
 * It can be used to store strings (of a base followed by combining
 * characters) where the code was designed to only allow one character.
 *
 * Strings are internalized efficiently and never freed.  No memory
 * management of vteunistr values is needed.
 **/
typedef guint32 vteunistr;

struct _vte_termcap;
struct _vte_matcher;
struct _vte_trie;
struct _vte_table;
struct _vte_iso2022_state;
typedef struct _VteTree VteTree;


#define VTE_ISO2022_ENCODED_WIDTH_BIT_OFFSET	28
#define VTE_ISO2022_ENCODED_WIDTH_MASK		(3 << VTE_ISO2022_ENCODED_WIDTH_BIT_OFFSET)
#define VTE_ISO2022_HAS_ENCODED_WIDTH(__c)	(((__c) & VTE_ISO2022_ENCODED_WIDTH_MASK) != 0)
#define VTE_TREE_ARRAY_SIZE (128)

#ifdef VTE_SEAL_ENABLE
#define _VTE_SEAL(name) _vte_sealed__ ## name
#else
#define _VTE_SEAL(name) name
#endif

#ifdef VTE_DISABLE_DEPRECATED
#define _VTE_DEPRECATED(name) _vte_deprecated__ ## name
#else
#define _VTE_DEPRECATED(name) name
#endif

#define VTE_DRAW_SINGLE_WIDE_CHARACTERS	\
					" !\"#$%&'()*+,-./" \
					"0123456789" \
					":;<=>?@" \
					"ABCDEFGHIJKLMNOPQRSTUVWXYZ" \
					"[\\]^_`" \
					"abcdefghijklmnopqrstuvwxyz" \
					"{|}~" \
					""
#define VTE_DRAW_DOUBLE_WIDE_CHARACTERS 0x4e00, 0x4e8c, 0x4e09, 0x56db, 0x4e94,\
					0xac00, 0xac01, 0xac04, 0xac08, 0xac10
/* For Pango, we have to use CJK Ideographs alone. Otherwise, 'width'
   returned by pango_layout would be screwed up for Chinese and Japanese
   fonts without Hangul */
#define VTE_DRAW_DOUBLE_WIDE_IDEOGRAPHS 0x4e00, 0x4e8c, 0x4e09, 0x56db, 0x4e94
#define VTE_DRAW_OPAQUE 0xff
#define VTE_DRAW_MAX_LENGTH 1024

#define VTE_META_MASK		GDK_META_MASK
#define VTE_NUMLOCK_MASK	GDK_MOD2_MASK
#define VTE_DEF_FG			256
#define VTE_DEF_BG			257
#define VTE_BOLD_FG			258
#define VTE_DIM_FG			259
#define VTE_DEF_HL                      260
#define VTE_CUR_BG			261
#define VTE_PALETTE_SIZE		262

#define VTE_TAB_WIDTH 4
#define VTE_LINE_WIDTH			1
#define VTE_ROWS			24
#define VTE_COLUMNS			80
#define VTE_LEGACY_COLOR_SET_SIZE	8
#define VTE_COLOR_PLAIN_OFFSET		0
#define VTE_COLOR_BRIGHT_OFFSET		8
#define VTE_COLOR_DIM_OFFSET		16
/* More color defines in ring.h */

#define VTE_SCROLLBACK_INIT		100
#define VTE_SATURATION_MAX		10000
#define VTE_DEFAULT_CURSOR		GDK_XTERM
#define VTE_MOUSING_CURSOR		GDK_LEFT_PTR
#define VTE_TAB_MAX			999
#define VTE_ADJUSTMENT_PRIORITY		G_PRIORITY_DEFAULT_IDLE
#define VTE_INPUT_RETRY_PRIORITY	G_PRIORITY_HIGH
#define VTE_INPUT_PRIORITY		G_PRIORITY_DEFAULT_IDLE
#define VTE_CHILD_INPUT_PRIORITY	G_PRIORITY_DEFAULT_IDLE
#define VTE_CHILD_OUTPUT_PRIORITY	G_PRIORITY_HIGH
#define VTE_FX_PRIORITY			G_PRIORITY_DEFAULT_IDLE
#define VTE_REGCOMP_FLAGS		REG_EXTENDED
#define VTE_REGEXEC_FLAGS		0
#define VTE_INPUT_CHUNK_SIZE		0x2000
#define VTE_MAX_INPUT_READ		0x1000
#define VTE_INVALID_BYTE		'?'
#define VTE_DISPLAY_TIMEOUT		10
#define VTE_UPDATE_TIMEOUT		15
#define VTE_UPDATE_REPEAT_TIMEOUT	30
#define VTE_MAX_PROCESS_TIME		100
#define VTE_CELL_BBOX_SLACK		1

#define VTE_UTF8_BPC                    (6) /* Maximum number of bytes used per UTF-8 character */

#define I_(string) (g_intern_static_string(string))

#define VTE_TYPE_TERMINAL            (vte_terminal_get_type())
#define VTE_TERMINAL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), VTE_TYPE_TERMINAL, VteTerminal))
#define VTE_TERMINAL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  VTE_TYPE_TERMINAL, VteTerminalClass))
#define VTE_IS_TERMINAL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), VTE_TYPE_TERMINAL))
#define VTE_IS_TERMINAL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  VTE_TYPE_TERMINAL))
#define VTE_TERMINAL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  VTE_TYPE_TERMINAL, VteTerminalClass))

#define VTE_TYPE_BG            (vte_bg_get_type())
#define VTE_BG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), VTE_TYPE_BG, VteBg))
#define VTE_BG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  VTE_TYPE_BG, VteBgClass))
#define VTE_IS_BG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), VTE_TYPE_BG))
#define VTE_IS_BG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  VTE_TYPE_BG))
#define VTE_BG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  VTE_TYPE_BG, VteBgClass))


typedef enum {
        VTE_REGEX_GREGEX,
        VTE_REGEX_VTE,
        VTE_REGEX_UNDECIDED
} VteRegexMode;

typedef enum {
  VTE_REGEX_CURSOR_GDKCURSOR,
  VTE_REGEX_CURSOR_GDKCURSORTYPE,
  VTE_REGEX_CURSOR_NAME
} VteRegexCursorMode;

/* The order is important */
typedef enum {
	MOUSE_TRACKING_NONE,
	MOUSE_TRACKING_SEND_XY_ON_CLICK,
	MOUSE_TRACKING_SEND_XY_ON_BUTTON,
	MOUSE_TRACKING_HILITE_TRACKING,
	MOUSE_TRACKING_CELL_MOTION_TRACKING,
	MOUSE_TRACKING_ALL_MOTION_TRACKING
} MouseTrackingMode;


typedef struct _vte_matcher_impl *(*_vte_matcher_create_func)(void);
typedef const char *(*_vte_matcher_match_func)(struct _vte_matcher_impl *impl,
		const gunichar *pattern, gssize length,
		const char **res, const gunichar **consumed,
		GQuark *quark, GValueArray **array);
typedef void (*_vte_matcher_add_func)(struct _vte_matcher_impl *impl,
		const char *pattern, gssize length,
		const char *result, GQuark quark);
typedef void (*_vte_matcher_print_func)(struct _vte_matcher_impl *impl);
typedef void (*_vte_matcher_destroy_func)(struct _vte_matcher_impl *impl);
struct _vte_matcher_class{
	_vte_matcher_create_func create;
	_vte_matcher_add_func add;
	_vte_matcher_print_func print;
	_vte_matcher_match_func match;
	_vte_matcher_destroy_func destroy;
};
struct _vte_matcher_impl {
	const struct _vte_matcher_class *klass;
	/* private */
};
struct _VteTree {
  GTree *tree;
  gpointer array[VTE_TREE_ARRAY_SIZE];
};
typedef void (*_vte_iso2022_codeset_changed_cb_fn)(struct _vte_iso2022_state *, gpointer);

typedef struct _VteBg         VteBg;
typedef struct _VteBgPrivate  VteBgPrivate;
typedef struct _VteBgClass    VteBgClass;

struct _VteBg {
	GObject parent;

        /*< private >*/
	VteBgPrivate *pvt;
};

struct _VteBgClass {
	GObjectClass parent_class;
};

typedef enum {
	VTE_BG_SOURCE_NONE,
	VTE_BG_SOURCE_ROOT,
	VTE_BG_SOURCE_PIXBUF,
	VTE_BG_SOURCE_FILE
} VteBgSourceType;

struct _vte_draw {
	GtkWidget *widget;

	gint started;

	struct font_info *font;
	struct font_info *font_bold;
	cairo_pattern_t *bg_pattern;

	cairo_t *cr;
};

/* A request to draw a particular character spanning a given number of columns
   at the given location.  Unlike most APIs, (x,y) specifies the top-left
   corner of the cell into which the character will be drawn instead of the
   left end of the baseline. */
struct _vte_draw_text_request {
	vteunistr c;
	gshort x, y, columns;
};

/* A match regex, with a tag. */
struct vte_match_regex {
	gint tag;
        VteRegexMode mode;
        union { /* switched on |mode| */
              struct {
                    GRegex *regex;
                    GRegexMatchFlags flags;
              } gregex;
              struct _vte_regex *reg;
        } regex;
        VteRegexCursorMode cursor_mode;
        union {
	       GdkCursor *cursor;
               char *cursor_name;
               GdkCursorType cursor_type;
        } cursor;
};

typedef struct _VteTerminal             VteTerminal;
typedef struct _VteTerminalPrivate      VteTerminalPrivate;
typedef struct _VteTerminalClass        VteTerminalClass;
typedef struct _VteTerminalClassPrivate VteTerminalClassPrivate;

/**
 * VteTerminal:
 *
 * All of these fields should be considered read-only and deprecated.
 */
struct _VteTerminal {
	GtkWidget widget;
        /*< private >*/
	GtkAdjustment *_VTE_SEAL(adjustment);	/* Scrolling adjustment. */

	/* Metric and sizing data. */
	glong _VTE_SEAL(char_width), _VTE_SEAL(char_height);	/* dimensions of character cells */
	glong _VTE_SEAL(char_ascent), _VTE_SEAL(char_descent); /* important font metrics */
	glong _VTE_SEAL(row_count), _VTE_SEAL(column_count);	/* dimensions of the window */

	/* Titles. */
	char *_VTE_SEAL(window_title);
	char *_VTE_SEAL(icon_title);

	/*< private >*/
	VteTerminalPrivate *pvt;
};

/**
 * VteTerminalClass:
 *
 * All of these fields should be considered read-only, except for derived classes.
 */
struct _VteTerminalClass {
	/*< public > */
	/* Inherited parent class. */
	GtkWidgetClass parent_class;

	/*< protected > */
	/* Default signal handlers. */
	void (*eof)(VteTerminal* terminal);
	void (*child_exited)(VteTerminal* terminal);
	void (*emulation_changed)(VteTerminal* terminal);
	void (*encoding_changed)(VteTerminal* terminal);
	void (*char_size_changed)(VteTerminal* terminal, guint char_width, guint char_height);
	void (*window_title_changed)(VteTerminal* terminal);
	void (*icon_title_changed)(VteTerminal* terminal);
	void (*selection_changed)(VteTerminal* terminal);
	void (*contents_changed)(VteTerminal* terminal);
	void (*cursor_moved)(VteTerminal* terminal);
	void (*status_line_changed)(VteTerminal* terminal);
	void (*commit)(VteTerminal* terminal, const gchar *text, guint size);

	void (*deiconify_window)(VteTerminal* terminal);
	void (*iconify_window)(VteTerminal* terminal);
	void (*raise_window)(VteTerminal* terminal);
	void (*lower_window)(VteTerminal* terminal);
	void (*refresh_window)(VteTerminal* terminal);
	void (*restore_window)(VteTerminal* terminal);
	void (*maximize_window)(VteTerminal* terminal);
	void (*resize_window)(VteTerminal* terminal, guint width, guint height);
	void (*move_window)(VteTerminal* terminal, guint x, guint y);

	void (*increase_font_size)(VteTerminal* terminal);
	void (*decrease_font_size)(VteTerminal* terminal);

	void (*text_modified)(VteTerminal* terminal);
	void (*text_inserted)(VteTerminal* terminal);
	void (*text_deleted)(VteTerminal* terminal);
	void (*text_scrolled)(VteTerminal* terminal, gint delta);
	void (*copy_clipboard)(VteTerminal* terminal);
	void (*paste_clipboard)(VteTerminal* terminal);

#if !GTK_CHECK_VERSION (2, 91, 2)
	void (* set_scroll_adjustments) (GtkWidget *widget,
					 GtkAdjustment *hadjustment,
					 GtkAdjustment *vadjustment);
#endif

 	void (*beep)(VteTerminal* terminal);

#if GTK_CHECK_VERSION (2, 99, 0)
        /* Padding for future expansion. */
        gpointer padding[16];
#else
	/* Padding for future expansion. */
	void (*vte_reserved3)(void);
	void (*vte_reserved4)(void);

	/*< private > */
	/* Signals we might emit. */
        guint _VTE_DEPRECATED(eof_signal);
        guint _VTE_DEPRECATED(child_exited_signal);
        guint _VTE_DEPRECATED(emulation_changed_signal);
        guint _VTE_DEPRECATED(encoding_changed_signal);
        guint _VTE_DEPRECATED(char_size_changed_signal);
        guint _VTE_DEPRECATED(window_title_changed_signal);
        guint _VTE_DEPRECATED(icon_title_changed_signal);
        guint _VTE_DEPRECATED(selection_changed_signal);
        guint _VTE_DEPRECATED(contents_changed_signal);
        guint _VTE_DEPRECATED(cursor_moved_signal);
        guint _VTE_DEPRECATED(status_line_changed_signal);
        guint _VTE_DEPRECATED(commit_signal);
        guint _VTE_DEPRECATED(deiconify_window_signal);
        guint _VTE_DEPRECATED(iconify_window_signal);
        guint _VTE_DEPRECATED(raise_window_signal);
        guint _VTE_DEPRECATED(lower_window_signal);
        guint _VTE_DEPRECATED(refresh_window_signal);
        guint _VTE_DEPRECATED(restore_window_signal);
        guint _VTE_DEPRECATED(maximize_window_signal);
        guint _VTE_DEPRECATED(resize_window_signal);
        guint _VTE_DEPRECATED(move_window_signal);
        guint _VTE_DEPRECATED(increase_font_size_signal);
        guint _VTE_DEPRECATED(decrease_font_size_signal);
        guint _VTE_DEPRECATED(text_modified_signal);
        guint _VTE_DEPRECATED(text_inserted_signal);
        guint _VTE_DEPRECATED(text_deleted_signal);
        guint _VTE_DEPRECATED(text_scrolled_signal);
        guint _VTE_DEPRECATED(reserved1);
        guint _VTE_DEPRECATED(reserved2);
        guint _VTE_DEPRECATED(reserved3);
        guint _VTE_DEPRECATED(reserved4);
        guint _VTE_DEPRECATED(reserved5);
        guint _VTE_DEPRECATED(reserved6);
#endif

#if GTK_CHECK_VERSION (2, 99, 0)
        VteTerminalClassPrivate *priv;
#endif
};

#ifndef VTE_DISABLE_DEPRECATED
#define __VTE_VTE_H_INSIDE__ 1
#include "vtedeprecated.h"
#undef __VTE_VTE_H_INSIDE__
#endif /* VTE_DISABLE_DEPRECATED */

/**
 * VteTerminalEraseBinding:
 * @VTE_ERASE_AUTO: For backspace, attempt to determine the right value from the terminal's IO settings.  For delete, use the control sequence.
 * @VTE_ERASE_ASCII_BACKSPACE: Send an ASCII backspace character (0x08).
 * @VTE_ERASE_ASCII_DELETE: Send an ASCII delete character (0x7F).
 * @VTE_ERASE_DELETE_SEQUENCE: Send the "@@7" control sequence.
 * @VTE_ERASE_TTY: Send terminal's "erase" setting.
 *
 * An enumerated type which can be used to indicate which string the terminal
 * should send to an application when the user presses the Delete or Backspace
 * keys.
 */
typedef enum {
	VTE_ERASE_AUTO,
	VTE_ERASE_ASCII_BACKSPACE,
	VTE_ERASE_ASCII_DELETE,
	VTE_ERASE_DELETE_SEQUENCE,
	VTE_ERASE_TTY
} VteTerminalEraseBinding;

/**
 * VteTerminalCursorBlinkMode:
 * @VTE_CURSOR_BLINK_SYSTEM: Follow GTK+ settings for cursor blinking.
 * @VTE_CURSOR_BLINK_ON: Cursor blinks.
 * @VTE_CURSOR_BLINK_OFF: Cursor does not blink.
 *
 * An enumerated type which can be used to indicate the cursor blink mode
 * for the terminal.
 */
typedef enum {
        VTE_CURSOR_BLINK_SYSTEM,
        VTE_CURSOR_BLINK_ON,
        VTE_CURSOR_BLINK_OFF
} VteTerminalCursorBlinkMode;

/**
 * VteTerminalCursorShape:
 * @VTE_CURSOR_SHAPE_BLOCK: Draw a block cursor.  This is the default.
 * @VTE_CURSOR_SHAPE_IBEAM: Draw a vertical bar on the left side of character.
 * This is similar to the default cursor for other GTK+ widgets.
 * @VTE_CURSOR_SHAPE_UNDERLINE: Draw a horizontal bar below the character.
 *
 * An enumerated type which can be used to indicate what should the terminal
 * draw at the cursor position.
 */
typedef enum {
        VTE_CURSOR_SHAPE_BLOCK,
        VTE_CURSOR_SHAPE_IBEAM,
        VTE_CURSOR_SHAPE_UNDERLINE
} VteTerminalCursorShape;

/* The structure we return as the supplemental attributes for strings. */
struct _VteCharAttributes {
        /*< private >*/
	long row, column;
	GdkColor fore, back;
	guint underline:1, strikethrough:1;
};
typedef struct _VteCharAttributes VteCharAttributes;

/* The name of the same structure in the 0.10 series, for API compatibility. */
struct vte_char_attributes {
        /*< private >*/
	long row, column;
	GdkColor fore, back;
	guint underline:1, strikethrough:1;
};

typedef gboolean (*VteSelectionFunc)(VteTerminal *terminal,
                                     glong column,
                                     glong row,
                                     gpointer data);

#define VTE_TYPE_TERMINAL_ACCESSIBLE            (vte_terminal_accessible_get_type ())
#define VTE_TERMINAL_ACCESSIBLE(object)         (G_TYPE_CHECK_INSTANCE_CAST ((object), VTE_TYPE_TERMINAL_ACCESSIBLE, VteTerminalAccessible))
#define VTE_TERMINAL_ACCESSIBLE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), VTE_TYPE_TERMINAL_ACCESSIBLE, VteTerminalAccessibleClass))
#define VTE_IS_TERMINAL_ACCESSIBLE(object)      (G_TYPE_CHECK_INSTANCE_TYPE ((object), VTE_TYPE_TERMINAL_ACCESSIBLE))
#define VTE_IS_TERMINAL_ACCESSIBLE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), VTE_TYPE_TERMINAL_ACCESSIBLE))
#define VTE_TERMINAL_ACCESSIBLE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), VTE_TYPE_TERMINAL_ACCESSIBLE, VteTerminalAccessibleClass))

typedef struct _VteTerminalAccessible VteTerminalAccessible;
typedef struct _VteTerminalAccessibleClass VteTerminalAccessibleClass;

/**
 * VteTerminalAccessible:
 *
 * The accessible peer for #VteTerminal.
 */
struct _VteTerminalAccessible {
	GtkAccessible parent;
	/*< private > */
	/* Unknown GailWidget implementation stuffs, exact size of which is
	 * worked out at run-time. */
};

struct _VteTerminalAccessibleClass {
	GtkAccessibleClass parent_class;
	/*< private > */
	/* Unknown GailWidgetClass implementation stuffs, exact size of which
	 * is worked out at run-time. */
};

#define VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY            (vte_terminal_accessible_factory_get_type ())
#define VTE_TERMINAL_ACCESSIBLE_FACTORY(object)         (G_TYPE_CHECK_INSTANCE_CAST ((object), VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY, VteTerminalAccessibleFactory))
#define VTE_TERMINAL_ACCESSIBLE_FACTORY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY, VteTerminalAccessibleFactoryClass))
#define VTE_IS_TERMINAL_ACCESSIBLE_FACTORY(object)      (G_TYPE_CHECK_INSTANCE_TYPE ((object), VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY))
#define VTE_IS_TERMINAL_ACCESSIBLE_FACTORY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY))
#define VTE_TERMINAL_ACCESSIBLE_FACTORY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY, VteTerminalAccessibleFactoryClass))

typedef struct _VteTerminalAccessibleFactory VteTerminalAccessibleFactory;
typedef struct _VteTerminalAccessibleFactoryClass VteTerminalAccessibleFactoryClass;

struct _VteTerminalAccessibleFactory {
	AtkObjectFactory parent;
};

struct _VteTerminalAccessibleFactoryClass {
	AtkObjectFactoryClass parent;
};

struct _vte_regex_match {
	int rm_so, rm_eo;
};
struct _vte_regex;

typedef struct _VteStream VteStream;
typedef struct _VteCellAttr {
	guint32 fragment: 1;	/* A continuation cell. */
	guint32 columns: 4;	/* Number of visible columns
				   (as determined by g_unicode_iswide(c)).
				   Also abused for tabs; bug 353610
				   Keep at least 4 for tabs to work
				   */
	guint32 bold: 1;
	guint32 fore: 9;	/* Index into color palette */
	guint32 back: 9;	/* Index into color palette. */

	guint32 standout: 1;
	guint32 underline: 1;
	guint32 strikethrough: 1;

	guint32 reverse: 1;
	guint32 blink: 1;
	guint32 half: 1;

	guint32 invisible: 1;
	/* unused; bug 499893
	guint32 protect: 1;
	 */

	/* 30 bits */
} VteCellAttr;
G_STATIC_ASSERT (sizeof (VteCellAttr) == 4);

typedef union _VteIntCellAttr {
	VteCellAttr s;
	guint32 i;
} VteIntCellAttr;
G_STATIC_ASSERT (sizeof (VteCellAttr) == sizeof (VteIntCellAttr));

/*
 * VteCell: A single cell's data
 */

typedef struct _VteCell {
	vteunistr c;
	VteCellAttr attr;
} VteCell;
G_STATIC_ASSERT (sizeof (VteCell) == 8);

typedef union _VteIntCell {
	VteCell cell;
	struct {
		guint32 c;
		guint32 attr;
	} i;
} VteIntCell;
G_STATIC_ASSERT (sizeof (VteCell) == sizeof (VteIntCell));

static const VteIntCell basic_cell = {
	{
		0,
		{
			0, /* fragment */
			1, /* columns */
			0, /* bold */
			VTE_DEF_FG, /* fore */
			VTE_DEF_BG, /* back */

			0, /* standout */
			0, /* underline */
			0, /* strikethrough */

			0, /* reverse */
			0, /* blink */
			0, /* half */

			0  /* invisible */
		}
	}
};


/*
 * VteRowAttr: A single row's attributes
 */

typedef struct _VteRowAttr {
	guint8 soft_wrapped: 1;
} VteRowAttr;
G_STATIC_ASSERT (sizeof (VteRowAttr) == 1);

/*
 * VteRowData: A single row's data
 */

typedef struct _VteRowData {
	VteCell *cells;
	guint16 len;
	VteRowAttr attr;
} VteRowData;


typedef struct _VteCellAttrChange {
	gsize text_offset;
	VteIntCellAttr attr;
} VteCellAttrChange;


/*
 * VteRing: A scrollback buffer ring
 */

typedef struct _VteRing VteRing;
struct _VteRing {
	gulong max;

	gulong start, end;

	/* Writable */
	gulong writable, mask;
	VteRowData *array;

	/* Storage */
	gulong last_page;
	VteStream *attr_stream, *text_stream, *row_stream;
	VteCellAttrChange last_attr;
	GString *utf8_buffer;

	VteRowData cached_row;
	gulong cached_row_num;
};


/* Create a new termcap structure. */
struct _vte_termcap *_vte_termcap_new(const char *filename);

/* Free a termcap structure. */
void _vte_termcap_free(struct _vte_termcap *termcap);

/* Read a boolean capability for a given terminal. */
gboolean _vte_termcap_find_boolean(struct _vte_termcap *termcap,
				   const char *tname, const char *cap);

/* Read a numeric capability for a given terminal. */
long _vte_termcap_find_numeric(struct _vte_termcap *termcap, const char *tname,
			       const char *cap);

/* Read a string capability for a given terminal.  The returned string should
 * be freed with g_free(). */
char *_vte_termcap_find_string(struct _vte_termcap *termcap, const char *tname,
			       const char *cap);

/* Read a string capability for a given terminal, and return the length of
 * the result in addition to the result itself.  The returned string should
 * be freed with g_free(). */
char *_vte_termcap_find_string_length(struct _vte_termcap *termcap,
				      const char *tname,
				      const char *cap, gssize *length);
				      
/* Create an empty, one-level table. */
struct _vte_table *_vte_table_new(void);

/* Free a table tree. */
void _vte_table_free(struct _vte_table *table);

/* Add a string to the matching tree. */
void _vte_table_add(struct _vte_table *table,
		    const char *pattern, gssize length,
		    const char *result, GQuark quark);

/* Check if a string matches something in the tree. */
const char *_vte_table_match(struct _vte_table *table,
			     const gunichar *pattern, gssize length,
			     const char **res, const gunichar **consumed,
			     GQuark *quark, GValueArray **array);
/* Dump out the contents of a tree. */
void _vte_table_print(struct _vte_table *table);

extern const struct _vte_matcher_class _vte_matcher_table;

/* Create a new trie structure. */
struct _vte_trie *_vte_trie_new(void);

/* Free a trie structure. */
void _vte_trie_free(struct _vte_trie *trie);

/* Add a string to the trie, along with its associated result and an optional
 * Quark to store with it. */
void _vte_trie_add(struct _vte_trie *trie,
		   const char *pattern, size_t length,
		   const char *result, GQuark quark);

/* See if a given pattern of a given length is in the trie.  The result is
 * returned both as the result of the function, and in the pointer res (if
 * res is not NULL).  The associated quark is also stored in "quark".  If
 * the string could be the initial portion of some sequence in the trie, the
 * empty string is returned for the answer.  If no match is found, and the
 * passed-in string can not be an initial substring of one of the strings in
 * the trie, then NULL is returned. */
const char *_vte_trie_match(struct _vte_trie *trie,
			    const gunichar *pattern, size_t length,
			    const char **res,
			    const gunichar **consumed,
			    GQuark *quark,
			    GValueArray **array);

/* Print the contents of the trie (mainly for diagnostic purposes). */
void _vte_trie_print(struct _vte_trie *trie);

extern const struct _vte_matcher_class _vte_matcher_trie;

/* Create and init matcher. */
struct _vte_matcher *_vte_matcher_new(const char *emulation,
				      struct _vte_termcap *termcap);

/* Free a matcher. */
void _vte_matcher_free(struct _vte_matcher *matcher);

/* Check if a string matches a sequence the matcher knows about. */
const char *_vte_matcher_match(struct _vte_matcher *matcher,
			       const gunichar *pattern, gssize length,
			       const char **res, const gunichar **consumed,
			       GQuark *quark, GValueArray **array);

/* Dump out the contents of a matcher, mainly for debugging. */
void _vte_matcher_print(struct _vte_matcher *matcher);

/* Free a parameter array. */
void _vte_matcher_free_params_array(struct _vte_matcher *matcher, GValueArray *params);

VteTree *_vte_tree_new(GCompareFunc key_compare_func);
void _vte_tree_destroy(VteTree *tree);
void _vte_tree_insert(VteTree *tree, gpointer key, gpointer value);
gpointer _vte_tree_lookup(VteTree *tree, gconstpointer key);
struct _vte_iso2022_state *_vte_iso2022_state_new(const char *native_codeset,
						  _vte_iso2022_codeset_changed_cb_fn,
						  gpointer);
void _vte_iso2022_state_set_codeset(struct _vte_iso2022_state *state,
				    const char *codeset);
const char *_vte_iso2022_state_get_codeset(struct _vte_iso2022_state *state);
gsize _vte_iso2022_process(struct _vte_iso2022_state *state,
			  guchar *input, gsize length,
			  GArray *gunichars);
gunichar _vte_iso2022_process_single(struct _vte_iso2022_state *state,
				     gunichar c, gunichar map);
void _vte_iso2022_state_free(struct _vte_iso2022_state *);
int _vte_iso2022_get_encoded_width(gunichar c);
int _vte_iso2022_unichar_width(struct _vte_iso2022_state *state,
			       gunichar c);
			       
/* Start up the given binary (exact path, not interpreted at all) in a
 * pseudo-terminal of its own, returning the descriptor for the master
 * side of the PTY pair, logging the session to the specified files, and
 * storing the child's PID in the given argument. */
int _vte_pty_open(pid_t *child, char **env_add,
		  const char *command, char **argv, const char *directory,
		  int columns, int rows,
		  gboolean lastlog, gboolean utmp, gboolean wtmp);

/* Set or read the size of a terminal.  Returns 0 on success, -1 on failure,
 * with errno set to defined return codes from ioctl(). */
int _vte_pty_get_size(int master, int *columns, int *rows);
int _vte_pty_set_size(int master, int columns, int rows);

/* Try to let the kernel know that the terminal is or is not UTF-8. */
void _vte_pty_set_utf8(int pty, gboolean utf8);

/* Close a pty. */
void _vte_pty_close(int pty);

/**
 * _vte_unistr_append_unichar:
 * @s: a #vteunistr
 * @c: Unicode character to append to @s
 *
 * Creates a vteunistr value for the string @s followed by the
 * character @c.
 *
 * Returns: the new #vteunistr value
 **/
vteunistr
_vte_unistr_append_unichar (vteunistr s, gunichar c);

gunichar
_vte_unistr_get_base (vteunistr s);

/**
 * _vte_unistr_append_to_string:
 * @s: a #vteunistr
 * @gs: a #GString to append @s to
 *
 * Appends @s to @gs.  This is how one converts a #vteunistr to a
 * traditional string.
 **/
void
_vte_unistr_append_to_string (vteunistr s, GString *gs);

/**
 * _vte_unistr_strlen:
 * @s: a #vteunistr
 *
 * Counts the number of character in @s.
 *
 * Returns: length of @s in characters.
 **/
int
_vte_unistr_strlen (vteunistr s);

#define _vte_ring_contains(__ring, __position) \
	(((gulong) (__position) >= (__ring)->start) && \
	 ((gulong) (__position) < (__ring)->end))
#define _vte_ring_delta(__ring) ((glong) (__ring)->start)
#define _vte_ring_length(__ring) ((glong) ((__ring)->end - (__ring)->start))
#define _vte_ring_next(__ring) ((glong) (__ring)->end)
#define _vte_row_data_length(__row)			((__row)->len + 0)


GType vte_bg_get_type(void);

VteBg *vte_bg_get_for_screen(GdkScreen *screen);

cairo_surface_t *
vte_bg_get_surface(VteBg *bg,
		   VteBgSourceType source_type,
		   GdkPixbuf *source_pixbuf,
		   const char *source_file,
		   const PangoColor *tint,
		   double saturation,
		   cairo_surface_t *other);

/* Create and destroy a draw structure. */
struct _vte_draw *_vte_draw_new(GtkWidget *widget);
void _vte_draw_free(struct _vte_draw *draw);

/* Begin and end a drawing operation.  If anything is buffered locally, it is
   flushed to the window system when _end() is called. */
void _vte_draw_start(struct _vte_draw *draw);
void _vte_draw_end(struct _vte_draw *draw);

void _vte_draw_set_background_solid(struct _vte_draw *draw,
				    double red,
				    double green,
				    double blue,
				    double opacity);
void _vte_draw_set_background_image(struct _vte_draw *draw,
				    VteBgSourceType type,
				    GdkPixbuf *pixbuf,
				    const char *file,
				    const PangoColor *color,
				    double saturation);
void _vte_draw_set_background_scroll(struct _vte_draw *draw,
				     gint x, gint y);

void _vte_draw_clip(struct _vte_draw *draw, GdkRegion *region);
void _vte_draw_clear(struct _vte_draw *draw,
		     gint x, gint y, gint width, gint height);

void _vte_draw_set_text_font(struct _vte_draw *draw,
			     const PangoFontDescription *fontdesc,
			     VteTerminalAntiAlias anti_alias);
void _vte_draw_get_text_metrics(struct _vte_draw *draw,
				gint *width, gint *height, gint *ascent);
int _vte_draw_get_char_width(struct _vte_draw *draw, vteunistr c, int columns,
			     gboolean bold);

void _vte_draw_text(struct _vte_draw *draw,
		    struct _vte_draw_text_request *requests, gsize n_requests,
		    const PangoColor *color, guchar alpha, gboolean);
gboolean _vte_draw_char(struct _vte_draw *draw,
			struct _vte_draw_text_request *request,
			const PangoColor *color, guchar alpha, gboolean bold);
gboolean _vte_draw_has_char(struct _vte_draw *draw, vteunistr c, gboolean bold);


void _vte_draw_fill_rectangle(struct _vte_draw *draw,
			      gint x, gint y, gint width, gint height,
			      const PangoColor *color, guchar alpha);
void _vte_draw_draw_rectangle(struct _vte_draw *draw,
			      gint x, gint y, gint width, gint height,
			      const PangoColor *color, guchar alpha);
			      
VteRowData *_vte_terminal_ensure_row(VteTerminal *terminal);
VteRowData * _vte_new_row_data(VteTerminal *terminal);
VteRowData *_vte_terminal_ring_insert (VteTerminal *terminal, glong position, gboolean fill);
VteRowData *_vte_terminal_ring_append (VteTerminal *terminal, gboolean fill);
void _vte_terminal_set_pointer_visible(VteTerminal *terminal, gboolean visible);
void _vte_invalidate_all(VteTerminal *terminal);
void _vte_invalidate_cells(VteTerminal *terminal, glong column_start, gint column_count, glong row_start, gint row_count);
void _vte_invalidate_cell(VteTerminal *terminal, glong col, glong row);
void _vte_invalidate_cursor_once(VteTerminal *terminal, gboolean periodic);
void _vte_terminal_adjust_adjustments(VteTerminal *terminal);
void _vte_terminal_queue_contents_changed(VteTerminal *terminal);
void _vte_terminal_emit_text_deleted(VteTerminal *terminal);
void _vte_terminal_emit_text_inserted(VteTerminal *terminal);
void _vte_terminal_cursor_down (VteTerminal *terminal);
gboolean _vte_terminal_insert_char(VteTerminal *terminal, gunichar c, gboolean force_insert_mode, gboolean invalidate_cells);
void _vte_terminal_scroll_region(VteTerminal *terminal, long row, glong count, glong delta);
void _vte_terminal_set_default_attributes(VteTerminal *terminal);
void _vte_terminal_clear_tabstop(VteTerminal *terminal, int column);
gboolean _vte_terminal_get_tabstop(VteTerminal *terminal, int column);
void _vte_terminal_set_tabstop(VteTerminal *terminal, int column);
void _vte_terminal_update_insert_delta(VteTerminal *terminal);
void _vte_terminal_cleanup_tab_fragments_at_cursor (VteTerminal *terminal);
void _vte_terminal_audible_beep(VteTerminal *terminal);
void _vte_terminal_visible_beep(VteTerminal *terminal);
void _vte_terminal_beep(VteTerminal *terminal);
void _vte_terminal_inline_error_message(VteTerminal *terminal, const char *format, ...) G_GNUC_PRINTF(2,3);
void _vte_terminal_ring_remove (VteTerminal *terminal, glong position);
void _vte_terminal_handle_sequence(VteTerminal *terminal, const char *match_s, GQuark match, GValueArray *params);

/* enumerations from "vte.h" */
GType vte_terminal_erase_binding_get_type (void);
#define VTE_TYPE_TERMINAL_ERASE_BINDING (vte_terminal_erase_binding_get_type ())
GType vte_terminal_cursor_blink_mode_get_type (void);
#define VTE_TYPE_TERMINAL_CURSOR_BLINK_MODE (vte_terminal_cursor_blink_mode_get_type ())
GType vte_terminal_cursor_shape_get_type (void);
#define VTE_TYPE_TERMINAL_CURSOR_SHAPE (vte_terminal_cursor_shape_get_type ())
GType vte_terminal_write_flags_get_type (void);
#define VTE_TYPE_TERMINAL_WRITE_FLAGS (vte_terminal_write_flags_get_type ())

/* enumerations from "vtepty.h" */
GType vte_pty_flags_get_type (void);
#define VTE_TYPE_PTY_FLAGS (vte_pty_flags_get_type ())
GType vte_pty_error_get_type (void);
#define VTE_TYPE_PTY_ERROR (vte_pty_error_get_type ())

/* enumerations from "vtedeprecated.h" */
GType vte_terminal_anti_alias_get_type (void);
#define VTE_TYPE_TERMINAL_ANTI_ALIAS (vte_terminal_anti_alias_get_type ())

struct _vte_regex * _vte_regex_compile(const char *pattern);
void _vte_regex_free(struct _vte_regex *regex);
int _vte_regex_exec(struct _vte_regex *regex, const char *string, gsize nmatch, struct _vte_regex_match *matches);

void _vte_terminal_accessible_ref(VteTerminal *terminal);
char* _vte_terminal_get_selection(VteTerminal *terminal);
void _vte_terminal_get_start_selection(VteTerminal *terminal, long *x, long *y);
void _vte_terminal_get_end_selection(VteTerminal *terminal, long *x, long *y);
void _vte_terminal_select_text(VteTerminal *terminal, long start_x, long start_y, long end_x, long end_y, int start_offset, int end_offset);
void _vte_terminal_remove_selection(VteTerminal *terminal);

GType vte_terminal_accessible_get_type(void);

AtkObject *vte_terminal_accessible_new(VteTerminal *terminal);

GType vte_terminal_accessible_factory_get_type(void);

AtkObjectFactory *vte_terminal_accessible_factory_new(void);

/* The widget's type. */
GType vte_terminal_get_type(void);

GtkWidget *vte_terminal_new(void);

VtePty *vte_terminal_pty_new (VteTerminal *terminal,
                              VtePtyFlags flags,
                              GError **error);

void vte_terminal_watch_child (VteTerminal *terminal,
                               GPid child_pid);

gboolean vte_terminal_fork_command_full(VteTerminal *terminal,
                                        VtePtyFlags pty_flags,
                                        const char *working_directory,
                                        char **argv,
                                        char **envv,
                                        GSpawnFlags spawn_flags,
                                        GSpawnChildSetupFunc child_setup,
                                        gpointer child_setup_data,
                                        GPid *child_pid /* out */,
                                        GError **error);

/* Send data to the terminal to display, or to the terminal's forked command
 * to handle in some way.  If it's 'cat', they should be the same. */
void vte_terminal_feed(VteTerminal *terminal, const char *data, glong length);
void vte_terminal_feed_child(VteTerminal *terminal, const char *text, glong length);
void vte_terminal_feed_child_binary(VteTerminal *terminal, const char *data, glong length);

/* Copy currently-selected text to the clipboard, or from the clipboard to
 * the terminal. */
void vte_terminal_copy_clipboard(VteTerminal *terminal);
void vte_terminal_paste_clipboard(VteTerminal *terminal);
void vte_terminal_copy_primary(VteTerminal *terminal);
void vte_terminal_paste_primary(VteTerminal *terminal);

/* simple manipulation of selection */
void vte_terminal_select_all(VteTerminal *terminal);
void vte_terminal_select_none(VteTerminal *terminal);

/* Set the terminal's size. */
void vte_terminal_set_size(VteTerminal *terminal, glong columns, glong rows);

/* Set various on-off settings. */
void vte_terminal_set_audible_bell(VteTerminal *terminal, gboolean is_audible);
gboolean vte_terminal_get_audible_bell(VteTerminal *terminal);
void vte_terminal_set_visible_bell(VteTerminal *terminal, gboolean is_visible);
gboolean vte_terminal_get_visible_bell(VteTerminal *terminal);
void vte_terminal_set_scroll_background(VteTerminal *terminal, gboolean scroll);
void vte_terminal_set_scroll_on_output(VteTerminal *terminal, gboolean scroll);
void vte_terminal_set_scroll_on_keystroke(VteTerminal *terminal, gboolean scroll);

/* Background effects. */
void vte_terminal_set_background_image(VteTerminal *terminal, GdkPixbuf *image);
void vte_terminal_set_background_image_file(VteTerminal *terminal, const char *path);
void vte_terminal_set_background_tint_color(VteTerminal *terminal, const GdkColor *color);
void vte_terminal_set_background_saturation(VteTerminal *terminal, double saturation);
void vte_terminal_set_background_transparent(VteTerminal *terminal, gboolean transparent);
void vte_terminal_set_opacity(VteTerminal *terminal, guint16 opacity);

/* Set whether or not the cursor blinks. */
void vte_terminal_set_cursor_blink_mode(VteTerminal *terminal, VteTerminalCursorBlinkMode mode);
VteTerminalCursorBlinkMode vte_terminal_get_cursor_blink_mode(VteTerminal *terminal);

/* Set cursor shape */
void vte_terminal_set_cursor_shape(VteTerminal *terminal, VteTerminalCursorShape shape);
VteTerminalCursorShape vte_terminal_get_cursor_shape(VteTerminal *terminal);

/* Set the number of scrollback lines, above or at an internal minimum. */
void vte_terminal_set_scrollback_lines(VteTerminal *terminal, glong lines);

/* Append the input method menu items to a given shell. */
void vte_terminal_im_append_menuitems(VteTerminal *terminal,
				      GtkMenuShell *menushell);

/* Set or retrieve the current font. */
const PangoFontDescription *vte_terminal_get_font(VteTerminal *terminal);
void vte_terminal_set_allow_bold(VteTerminal *terminal, gboolean allow_bold);
gboolean vte_terminal_get_allow_bold(VteTerminal *terminal);

/* Check if the terminal is the current selection owner. */
gboolean vte_terminal_get_has_selection(VteTerminal *terminal);

/* Set the list of word chars, optionally using hyphens to specify ranges
 * (to get a hyphen, place it first), and check if a character is in the
 * range. */
void vte_terminal_set_word_chars(VteTerminal *terminal, const char *spec);
gboolean vte_terminal_is_word_char(VteTerminal *terminal, gunichar c);

/* Set what happens when the user strikes backspace or delete. */
void vte_terminal_set_backspace_binding(VteTerminal *terminal,
					VteTerminalEraseBinding binding);
void vte_terminal_set_delete_binding(VteTerminal *terminal,
				     VteTerminalEraseBinding binding);

/* Manipulate the autohide setting. */
void vte_terminal_set_mouse_autohide(VteTerminal *terminal, gboolean setting);
gboolean vte_terminal_get_mouse_autohide(VteTerminal *terminal);

/* Reset the terminal, optionally clearing the tab stops and line history. */
void vte_terminal_reset(VteTerminal *terminal,
                        gboolean clear_tabstops,
			gboolean clear_history);

/* Read the contents of the terminal, using a callback function to determine
 * if a particular location on the screen (0-based) is interesting enough to
 * include.  Each byte in the returned string will have a corresponding
 * VteCharAttributes structure in the passed GArray, if the array was not %NULL.
 * Note that it will have one entry per byte, not per character, so indexes
 * should match up exactly. */
char *vte_terminal_get_text(VteTerminal *terminal,
			    VteSelectionFunc is_selected,
			    gpointer user_data,
			    GArray *attributes);
char *vte_terminal_get_text_include_trailing_spaces(VteTerminal *terminal,
						    VteSelectionFunc is_selected,
						    gpointer user_data,
						    GArray *attributes);
char *vte_terminal_get_text_range(VteTerminal *terminal,
				  glong start_row, glong start_col,
				  glong end_row, glong end_col,
				  VteSelectionFunc is_selected,
				  gpointer user_data,
				  GArray *attributes);
void vte_terminal_get_cursor_position(VteTerminal *terminal,
				      glong *column, glong *row);
/* Display string matching:  clear all matching expressions. */
void vte_terminal_match_clear_all(VteTerminal *terminal);

/* Add a matching expression, returning the tag the widget assigns to that
 * expression. */
int vte_terminal_match_add_gregex(VteTerminal *terminal, GRegex *regex, GRegexMatchFlags flags);
/* Set the cursor to be used when the pointer is over a given match. */
void vte_terminal_match_set_cursor(VteTerminal *terminal, int tag,
				   GdkCursor *cursor);
void vte_terminal_match_set_cursor_type(VteTerminal *terminal,
					int tag, GdkCursorType cursor_type);
void vte_terminal_match_set_cursor_name(VteTerminal *terminal,
					int tag, const char *cursor_name);
/* Remove a matching expression by tag. */
void vte_terminal_match_remove(VteTerminal *terminal, int tag);

/* Check if a given cell on the screen contains part of a matched string.  If
 * it does, return the string, and store the match tag in the optional tag
 * argument. */
char *vte_terminal_match_check(VteTerminal *terminal,
			       glong column, glong row,
			       int *tag);

void      vte_terminal_search_set_gregex      (VteTerminal *terminal,
					       GRegex      *regex);
GRegex   *vte_terminal_search_get_gregex      (VteTerminal *terminal);
void      vte_terminal_search_set_wrap_around (VteTerminal *terminal,
					       gboolean     wrap_around);
gboolean  vte_terminal_search_get_wrap_around (VteTerminal *terminal);
gboolean  vte_terminal_search_find_previous   (VteTerminal *terminal);
gboolean  vte_terminal_search_find_next       (VteTerminal *terminal);


/* Set the emulation type.  Most of the time you won't need this. */
void vte_terminal_set_emulation(VteTerminal *terminal, const char *emulation);
const char *vte_terminal_get_emulation(VteTerminal *terminal);
const char *vte_terminal_get_default_emulation(VteTerminal *terminal);

/* Set the character encoding.  Most of the time you won't need this. */
void vte_terminal_set_encoding(VteTerminal *terminal, const char *codeset);
const char *vte_terminal_get_encoding(VteTerminal *terminal);

/* Get the contents of the status line. */
const char *vte_terminal_get_status_line(VteTerminal *terminal);

void vte_terminal_set_pty_object(VteTerminal *terminal, VtePty *pty);
VtePty *vte_terminal_get_pty_object(VteTerminal *terminal);

char *vte_get_user_shell (void);

/* Accessors for bindings. */
#if !GTK_CHECK_VERSION (2, 91, 2)
GtkAdjustment *vte_terminal_get_adjustment(VteTerminal *terminal);
#endif

glong vte_terminal_get_char_width(VteTerminal *terminal);
glong vte_terminal_get_char_height(VteTerminal *terminal);
glong vte_terminal_get_row_count(VteTerminal *terminal);
glong vte_terminal_get_column_count(VteTerminal *terminal);
const char *vte_terminal_get_window_title(VteTerminal *terminal);
const char *vte_terminal_get_icon_title(VteTerminal *terminal);

int vte_terminal_get_child_exit_status(VteTerminal *terminal);


/* Writing contents out */

/**
 * VteTerminalWriteFlags:
 * @VTE_TERMINAL_WRITE_DEFAULT: Write contents as UTF-8 text.  This is the default.
 *
 * A flag type to determine how terminal contents should be written
 * to an output stream.
 */
typedef enum {
  VTE_TERMINAL_WRITE_DEFAULT = 0
} VteTerminalWriteFlags;

gboolean vte_terminal_write_contents (VteTerminal *terminal,
				      GOutputStream *stream,
				      VteTerminalWriteFlags flags,
				      GCancellable *cancellable,
				      GError **error);

/* Return TRUE if a keyval is just a modifier key. */
gboolean _vte_keymap_key_is_modifier(guint keyval);

/* Add modifiers to the sequence if they're needed. */
void _vte_keymap_key_add_key_modifiers(guint keyval,
				       GdkModifierType modifiers,
				       gboolean sun_mode,
				       gboolean hp_mode,
				       gboolean legacy_mode,
				       gboolean vt220_mode,
				       gboolean app_cursor_keys,
				       char **normal,
				       gssize *normal_length);


/* 'font' will be processed by Pango font description guideline. */
void vte_terminal_set_font_V2(VteTerminal *this, char *font);

/*
	For 'which':
		Foreground(Normally normal text) = 256, Background = 257, Bold = 258, Dim = 259, Highlight(Selected area) = 260, Cursor = 261
*/
void vte_terminal_set_color_V2(VteTerminal *this, int which, int red, int green, int blue);
void vte_terminal_set_colors(VteTerminal *terminal, const GdkColor *foreground, const GdkColor *background, const GdkColor *palette, glong palette_size);
void vte_terminal_set_default_colors(VteTerminal *terminal);

#undef _VTE_SEAL
#undef _VTE_DEPRECATED

G_END_DECLS

#endif
