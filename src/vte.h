/*
 * Copyright (C) 2001,2002,2003,2009,2010 Red Hat, Inc.
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
#include <sys/wait.h>
#include <sys/types.h>

#define __VTE_VTE_H_INSIDE__ 1

#include "vtepty.h"
#include "vteversion.h"
#include "vteunistr.h"

#undef __VTE_VTE_H_INSIDE__

G_BEGIN_DECLS

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

#define VTE_TYPE_TERMINAL            (vte_terminal_get_type())
#define VTE_TERMINAL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), VTE_TYPE_TERMINAL, VteTerminal))
#define VTE_TERMINAL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  VTE_TYPE_TERMINAL, VteTerminalClass))
#define VTE_IS_TERMINAL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), VTE_TYPE_TERMINAL))
#define VTE_IS_TERMINAL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  VTE_TYPE_TERMINAL))
#define VTE_TERMINAL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  VTE_TYPE_TERMINAL, VteTerminalClass))

#define VTE_META_MASK		GDK_META_MASK
#define VTE_NUMLOCK_MASK	GDK_MOD2_MASK
#define VTE_DEF_FG			256
#define VTE_DEF_BG			257
#define VTE_BOLD_FG			258
#define VTE_DIM_FG			259
#define VTE_DEF_HL                      260
#define VTE_CUR_BG			261
#define VTE_PALETTE_SIZE		262

#define VTE_TAB_WIDTH			8
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

#define _vte_ring_contains(__ring, __position) \
	(((gulong) (__position) >= (__ring)->start) && \
	 ((gulong) (__position) < (__ring)->end))
#define _vte_ring_delta(__ring) ((glong) (__ring)->start)
#define _vte_ring_length(__ring) ((glong) ((__ring)->end - (__ring)->start))
#define _vte_ring_next(__ring) ((glong) (__ring)->end)
#define _vte_row_data_length(__row)			((__row)->len + 0)


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
void vte_terminal_set_size(VteTerminal *terminal,
			   glong columns, glong rows);

/* Set various on-off settings. */
void vte_terminal_set_audible_bell(VteTerminal *terminal, gboolean is_audible);
gboolean vte_terminal_get_audible_bell(VteTerminal *terminal);
void vte_terminal_set_visible_bell(VteTerminal *terminal, gboolean is_visible);
gboolean vte_terminal_get_visible_bell(VteTerminal *terminal);
void vte_terminal_set_scroll_background(VteTerminal *terminal, gboolean scroll);
void vte_terminal_set_scroll_on_output(VteTerminal *terminal, gboolean scroll);
void vte_terminal_set_scroll_on_keystroke(VteTerminal *terminal,
					  gboolean scroll);

/* Set the color scheme. */
void vte_terminal_set_color_dim(VteTerminal *terminal,
				const GdkColor *dim);
void vte_terminal_set_color_bold(VteTerminal *terminal,
				 const GdkColor *bold);
void vte_terminal_set_color_foreground(VteTerminal *terminal,
				       const GdkColor *foreground);
void vte_terminal_set_color_background(VteTerminal *terminal,
				       const GdkColor *background);
void vte_terminal_set_color_cursor(VteTerminal *terminal,
				   const GdkColor *cursor_background);
void vte_terminal_set_color_highlight(VteTerminal *terminal,
				      const GdkColor *highlight_background);
void vte_terminal_set_colors(VteTerminal *terminal,
			     const GdkColor *foreground,
			     const GdkColor *background,
			     const GdkColor *palette,
			     glong palette_size);

#if GTK_CHECK_VERSION (2, 99, 0)
void vte_terminal_set_color_bold_rgba(VteTerminal *terminal,
                                      const GdkRGBA *bold);
void vte_terminal_set_color_dim_rgba(VteTerminal *terminal,
	                             const GdkRGBA *dim);
void vte_terminal_set_color_foreground_rgba(VteTerminal *terminal,
					    const GdkRGBA *foreground);
void vte_terminal_set_color_background_rgba(VteTerminal *terminal,
					    const GdkRGBA *background);
void vte_terminal_set_color_cursor_rgba(VteTerminal *terminal,
					const GdkRGBA *cursor_background);
void vte_terminal_set_color_highlight_rgba(VteTerminal *terminal,
					   const GdkRGBA *highlight_background);
void vte_terminal_set_colors_rgba(VteTerminal *terminal,
				  const GdkRGBA *foreground,
				  const GdkRGBA *background,
				  const GdkRGBA *palette,
				  gsize palette_size);
#endif

void vte_terminal_set_default_colors(VteTerminal *terminal);

/* Background effects. */
void vte_terminal_set_background_image(VteTerminal *terminal, GdkPixbuf *image);
void vte_terminal_set_background_image_file(VteTerminal *terminal,
					    const char *path);
void vte_terminal_set_background_tint_color(VteTerminal *terminal,
					    const GdkColor *color);
void vte_terminal_set_background_saturation(VteTerminal *terminal,
					    double saturation);
void vte_terminal_set_background_transparent(VteTerminal *terminal,
					     gboolean transparent);
void vte_terminal_set_opacity(VteTerminal *terminal, guint16 opacity);

/* Set whether or not the cursor blinks. */
void vte_terminal_set_cursor_blink_mode(VteTerminal *terminal,
					VteTerminalCursorBlinkMode mode);
VteTerminalCursorBlinkMode vte_terminal_get_cursor_blink_mode(VteTerminal *terminal);

/* Set cursor shape */
void vte_terminal_set_cursor_shape(VteTerminal *terminal,
				   VteTerminalCursorShape shape);
VteTerminalCursorShape vte_terminal_get_cursor_shape(VteTerminal *terminal);

/* Set the number of scrollback lines, above or at an internal minimum. */
void vte_terminal_set_scrollback_lines(VteTerminal *terminal, glong lines);

/* Append the input method menu items to a given shell. */
void vte_terminal_im_append_menuitems(VteTerminal *terminal,
				      GtkMenuShell *menushell);

/* Set or retrieve the current font. */
void vte_terminal_set_font(VteTerminal *terminal,
			   const PangoFontDescription *font_desc);
void vte_terminal_set_font_from_string(VteTerminal *terminal, const char *name);
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

#undef _VTE_SEAL
#undef _VTE_DEPRECATED

G_END_DECLS

#ifndef VTE_DISABLE_DEPRECATED
#define __VTE_VTE_H_INSIDE__ 1
#include "vtedeprecated.h"
#undef __VTE_VTE_H_INSIDE__
#endif /* VTE_DISABLE_DEPRECATED */

#endif
