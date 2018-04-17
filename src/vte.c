/*
 * Copyright (C) 2001-2004,2008,2009,2010 Red Hat, Inc.
 * Copyright © 2008, 2009, 2010 Christian Persch
 * Copyright (C) 2004 Benjamin Otte <otte@gnome.org>
 * Copyright (C) 2006 Ryan Lortie <desrt@desrt.ca>
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


#include <config.h>

#include "vte.h"
#include "vte-private.h"
#include "vte-gtk-compat.h"
#include "iso2022.h"
#include "marshal.h"
#include "caps.h"
#include "debug.h"
#include "buffer.h"
#include "vteconv.h"
#include "reaper.h"
#include "ring.h"
#include "vtestream-base.h"
#include "vtestream-file.h"


enum {
        ACTION_MENU,
        LAST_ACTION
};


/* Part: Macros */
#ifdef G_ENABLE_DEBUG
#define g_marshal_value_peek_boolean(v)  g_value_get_boolean (v)
#define g_marshal_value_peek_char(v)     g_value_get_char (v)
#define g_marshal_value_peek_uchar(v)    g_value_get_uchar (v)
#define g_marshal_value_peek_int(v)      g_value_get_int (v)
#define g_marshal_value_peek_uint(v)     g_value_get_uint (v)
#define g_marshal_value_peek_long(v)     g_value_get_long (v)
#define g_marshal_value_peek_ulong(v)    g_value_get_ulong (v)
#define g_marshal_value_peek_int64(v)    g_value_get_int64 (v)
#define g_marshal_value_peek_uint64(v)   g_value_get_uint64 (v)
#define g_marshal_value_peek_enum(v)     g_value_get_enum (v)
#define g_marshal_value_peek_flags(v)    g_value_get_flags (v)
#define g_marshal_value_peek_float(v)    g_value_get_float (v)
#define g_marshal_value_peek_double(v)   g_value_get_double (v)
#define g_marshal_value_peek_string(v)   (char*) g_value_get_string (v)
#define g_marshal_value_peek_param(v)    g_value_get_param (v)
#define g_marshal_value_peek_boxed(v)    g_value_get_boxed (v)
#define g_marshal_value_peek_pointer(v)  g_value_get_pointer (v)
#define g_marshal_value_peek_object(v)   g_value_get_object (v)
#define g_marshal_value_peek_variant(v)  g_value_get_variant (v)
#else /* !G_ENABLE_DEBUG */
/* WARNING: This code accesses GValues directly, which is UNSUPPORTED API.
 *          Do not access GValues directly in your code. Instead, use the
 *          g_value_get_*() functions
 */
#define g_marshal_value_peek_boolean(v)  (v)->data[0].v_int
#define g_marshal_value_peek_char(v)     (v)->data[0].v_int
#define g_marshal_value_peek_uchar(v)    (v)->data[0].v_uint
#define g_marshal_value_peek_int(v)      (v)->data[0].v_int
#define g_marshal_value_peek_uint(v)     (v)->data[0].v_uint
#define g_marshal_value_peek_long(v)     (v)->data[0].v_long
#define g_marshal_value_peek_ulong(v)    (v)->data[0].v_ulong
#define g_marshal_value_peek_int64(v)    (v)->data[0].v_int64
#define g_marshal_value_peek_uint64(v)   (v)->data[0].v_uint64
#define g_marshal_value_peek_enum(v)     (v)->data[0].v_long
#define g_marshal_value_peek_flags(v)    (v)->data[0].v_ulong
#define g_marshal_value_peek_float(v)    (v)->data[0].v_float
#define g_marshal_value_peek_double(v)   (v)->data[0].v_double
#define g_marshal_value_peek_string(v)   (v)->data[0].v_pointer
#define g_marshal_value_peek_param(v)    (v)->data[0].v_pointer
#define g_marshal_value_peek_boxed(v)    (v)->data[0].v_pointer
#define g_marshal_value_peek_pointer(v)  (v)->data[0].v_pointer
#define g_marshal_value_peek_object(v)   (v)->data[0].v_pointer
#define g_marshal_value_peek_variant(v)  (v)->data[0].v_pointer
#endif /* !G_ENABLE_DEBUG */

#if GTK_CHECK_VERSION (2, 90, 7)
#define GDK_KEY(symbol) GDK_KEY_##symbol
#else
#include <gdk/gdkkeysyms.h>
#define GDK_KEY(symbol) GDK_##symbol
#endif

#define ESC _VTE_CAP_ESC
#define CSI _VTE_CAP_CSI
#define ST  _VTE_CAP_ST
#define OSC _VTE_CAP_OSC
#define PM  _VTE_CAP_PM
#define APC _VTE_CAP_APC

#define ENQ "\005"
#define BEL "\007"
#define BS  "\010"
#define TAB "\011"
#define LF  "\012"
#define VT  "\013"
#define FF  "\014"
#define CR  "\015"
#define SO  "\016"
#define SI  "\017"

/* Maps which jive with XTerm's ESC ()*+ ? sequences, RFC 1468.  Add the
 * PC437 map because despite knowing that XTerm doesn't support it, certain
 * applications try to use it anyway. */
#define NARROW_MAPS	"012AB4C5RQKYE6ZH7=" "J" "U"
/* Maps which jive with RFC 1468's ESC $ ? sequences. */
#define WIDE_MAPS	"@B"
/* Maps which jive with RFC 1557/1922/2237's ESC $ ()*+ ? sequences. */
#define WIDE_GMAPS	"C" "AGHIJKLM" "D"
/* Fudge factor we add to wide map identifiers to keep them distinct. */
#define WIDE_FUDGE	0x100000
/* An invalid codepoint. */
#define INVALID_CODEPOINT 0xFFFD

#ifndef HAVE_WINT_T
typedef gunichar wint_t;
#endif
#ifndef TRIE_MAYBE_STATIC
#define TRIE_MAYBE_STATIC
#endif

/* Table info. */
#define VTE_TABLE_MAX_LITERAL (128 + 32)
#define _vte_table_map_literal(__c) \
	(((__c) < (VTE_TABLE_MAX_LITERAL)) ? (__c) : 0)
#define _vte_table_is_numeric(__c) \
	(((__c) >= '0') && ((__c) <= '9'))
#define _vte_table_is_numeric_list(__c) \
	((((__c) >= '0') && ((__c) <= '9')) || (__c) == ';')

#ifndef howmany
#define howmany(x, y) (((x) + ((y) - 1)) / (y))
#endif

#define STATIC_PARAMS (G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB)
#define VTE_TERMCAP_NAME "termcap"

#define FONT_CACHE_TIMEOUT (30) /* seconds */

/* All shared data structures are implicitly protected by GDK mutex, because
 * that's how vte.c works and we only get called from there. */

/* cairo_show_glyphs accepts runs up to 102 glyphs before it allocates a
 * temporary array.
 *
 * Setting this to a large value can cause dramatic slow-downs for some
 * xservers (notably fglrx), see bug #410534.
 *
 * Moreover, setting it larger than %VTE_DRAW_MAX_LENGTH is nonsensical,
 * as the higher layers will not submit runs longer than that value.
 */
#define MAX_RUN_LENGTH 100

#define VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA "VteTerminalAccessiblePrivateData"
#define VTE_UNISTR_START 0x80000000

#ifdef MSG_NOSIGNAL
#define PTY_RECVMSG_FLAGS MSG_NOSIGNAL
#else
#define PTY_RECVMSG_FLAGS 0
#endif


/* Part: typedefs */
typedef struct _VteCells VteCells;
struct _VteCells {
	guint32 alloc_len;
	VteCell cells[1];
};

static VteReaper *singleton_reaper = NULL;

G_DEFINE_TYPE(VteReaper, vte_reaper, G_TYPE_OBJECT)

typedef size_t (*convert_func)(GIConv converter,
			  const guchar **inbuf,
			  gsize *inbytes_left,
			  guchar **outbuf,
			  gsize *outbytes_left);
struct _VteConv {
	GIConv conv;
	convert_func convert;
	gint (*close)(GIConv converter);
	gboolean in_unichar, out_unichar;
	VteBuffer *in_scratch, *out_scratch;
};

/*
 * --- a termcap file is represented by a simple tree ---
 */
typedef struct _vte_termcap
{
  GMappedFile *file;
  GTree *tree;
  const char *end;
} VteTermcap;

struct _vte_table {
	struct _vte_matcher_impl impl;
	GQuark resultq;
	const char *result;
	unsigned char *original;
	gssize original_length;
	int increment;
	struct _vte_table *table_string;
	struct _vte_table *table_number;
	struct _vte_table *table_number_list;
	struct _vte_table **table;
};

/* Argument info. */
enum _vte_table_argtype {
	_vte_table_arg_number=0,
	_vte_table_arg_string,
	_vte_table_arg_char
};
struct _vte_table_arginfo {
	const gunichar *start;
	struct _vte_table_arginfo *next;
	guint type:2;
	guint length:30;
};
#define MAX_STACK 16
struct _vte_table_arginfo_head {
	guint stack_allocated;
	struct _vte_table_arginfo *list;
	struct _vte_table_arginfo stack[MAX_STACK];
};
struct _vte_matcher {
	_vte_matcher_match_func match; /* shortcut to the most common op */
	struct _vte_matcher_impl *impl;
	GValueArray *free_params;
};
/* Structures and whatnot for tracking character classes. */
struct char_class_data {
	gunichar c;			/* A character. */
	int i;				/* An integer. */
	char *s;			/* A string. */
	int inc;			/* An increment value. */
};

struct char_class {
	enum cclass {
		exact = 0,		/* Not a special class. */
		digit,			/* Multiple-digit special class. */
		multi,			/* Multiple-number special class. */
		any,			/* Any single character. */
		string,			/* Any string of characters. */
		invalid			/* A placeholder. */
	} type;
	gboolean multiple;		/* Whether a sequence of multiple
					   characters in this class should be
					   counted together. */
	gunichar *code;			/* A magic string that indicates this
					   class should be found here. */
	gsize code_length;
	gsize ccount;			/* The maximum number of characters
					   after the format specifier to
					   consume. */
	gboolean (*check)(const gunichar c, struct char_class_data *data);
					/* Function to check if a character
					   is in this class. */
	void (*setup)(const gunichar *s, struct char_class_data *data, int inc);
					/* Setup the data struct for use in the
					 * above check function. */
	gboolean (*extract)(const gunichar *s, gsize length,
			    struct char_class_data *data,
			    GValueArray *array);
					/* Extract a parameter. */
};

/* A trie to hold control sequences. */
struct _vte_trie {
	struct _vte_matcher_impl impl;
	const char *result;		/* If this is a terminal node, then this
					   field contains its "value". */
	GQuark quark;			/* The quark for the value of the
					   result. */
	gsize trie_path_count;		/* Number of children of this node. */
	struct trie_path {
		struct char_class *cclass;
		struct char_class_data data;
		struct _vte_trie *trie;	/* The child node corresponding to this
					   character. */
	} *trie_paths;
};

static GStaticMutex _vte_matcher_mutex = G_STATIC_MUTEX_INIT;
static GCache *_vte_matcher_cache = NULL;
static struct _vte_matcher_impl dummy_vte_matcher_trie = {
	&_vte_matcher_trie
};
static struct _vte_matcher_impl dummy_vte_matcher_table = {
	&_vte_matcher_table
};

struct _vte_iso2022_map16 {
	guint16 from, to;
};

struct _vte_iso2022_map32 {
	guint32 from, to;
};

struct _vte_iso2022_block {
	enum {
		_vte_iso2022_cdata,
		_vte_iso2022_preserve,
		_vte_iso2022_control
	} type;
	gulong start, end;
};

struct _vte_iso2022_state {
	gboolean nrc_enabled;
	int current, override;
	gunichar g[4];
	const gchar *codeset, *native_codeset, *utf8_codeset, *target_codeset;
	gint ambiguous_width;
	VteConv conv;
	_vte_iso2022_codeset_changed_cb_fn codeset_changed;
	gpointer codeset_changed_data;
	VteBuffer *buffer;
};

/* DEC Special Character and Line Drawing Set.  VT100 and higher (per XTerm
 * docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_0[] = {
	{ 96, 0x25c6},	/* diamond */
	{'a', 0x2592},	/* checkerboard */
	{'b', 0x2409},	/* HT symbol */
	{'c', 0x240c},	/* FF symbol */
	{'d', 0x240d},	/* CR symbol */
	{'e', 0x240a},	/* LF symbol */
	{'f', 0x00b0},	/* degree */
	{'g', 0x00b1},	/* plus/minus */
	{'h', 0x2424},  /* NL symbol */
	{'i', 0x240b},  /* VT symbol */
	{'j', 0x2518},	/* downright corner */
	{'k', 0x2510},	/* upright corner */
	{'l', 0x250c},	/* upleft corner */
	{'m', 0x2514},	/* downleft corner */
	{'n', 0x253c},	/* cross */
	{'o', 0x23ba},  /* scan line 1/9 */
	{'p', 0x23bb},  /* scan line 3/9 */
	{'q', 0x2500},	/* horizontal line (also scan line 5/9) */
	{'r', 0x23bc},  /* scan line 7/9 */
	{'s', 0x23bd},  /* scan line 9/9 */
	{'t', 0x251c},	/* left t */
	{'u', 0x2524},	/* right t */
	{'v', 0x2534},	/* bottom t */
	{'w', 0x252c},	/* top t */
	{'x', 0x2502},	/* vertical line */
	{'y', 0x2264},  /* <= */
	{'z', 0x2265},  /* >= */
	{'{', 0x03c0},  /* pi */
	{'|', 0x2260},  /* not equal */
	{'}', 0x00a3},  /* pound currency sign */
	{'~', 0x00b7},	/* bullet */
};
/* United Kingdom.  VT100 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_A[] = {
	{'$', GDK_KEY (sterling)},
};
/* US-ASCII (no conversions).  VT100 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_B[] = {
	{0, 0},
};
/* Dutch. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_4[] = {
	{'#',  GDK_KEY (sterling)},
	{'@',  GDK_KEY (threequarters)},
	{'[',  GDK_KEY (ydiaeresis)},
	{'\\', GDK_KEY (onehalf)},
	{']',  GDK_KEY (bar)}, /* FIXME? not in XTerm 170 */
	{'{',  GDK_KEY (diaeresis)},
	{'|',  0x192}, /* f with hook (florin) */ /* FIXME? not in XTerm 170 */
	{'}',  GDK_KEY (onequarter)},
	{'~',  GDK_KEY (acute)}
};
/* Finnish. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_C[] = {
	{'[',  GDK_KEY (Adiaeresis)},
	{'\\', GDK_KEY (Odiaeresis)},
	{']',  GDK_KEY (Aring)},
	{'^',  GDK_KEY (Udiaeresis)},
	{'`',  GDK_KEY (eacute)},
	{'{',  GDK_KEY (adiaeresis)},
	{'|',  GDK_KEY (odiaeresis)},
	{'}',  GDK_KEY (aring)},
	{'~',  GDK_KEY (udiaeresis)},
};
/* French. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_R[] = {
	{'#',  GDK_KEY (sterling)},
	{'@',  GDK_KEY (agrave)},
	{'[',  GDK_KEY (degree)},
	{'\\', GDK_KEY (ccedilla)},
	{']',  GDK_KEY (section)},
	{'{',  GDK_KEY (eacute)},
	{'|',  GDK_KEY (ugrave)},
	{'}',  GDK_KEY (egrave)},
	{'~',  GDK_KEY (diaeresis)},
};
/* French Canadian. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_Q[] = {
	{'@',  GDK_KEY (agrave)},
	{'[',  GDK_KEY (acircumflex)},
	{'\\', GDK_KEY (ccedilla)},
	{']',  GDK_KEY (ecircumflex)},
	{'^',  GDK_KEY (icircumflex)},
	{'`',  GDK_KEY (ocircumflex)},
	{'{',  GDK_KEY (eacute)},
	{'|',  GDK_KEY (ugrave)},
	{'}',  GDK_KEY (egrave)},
	{'~',  GDK_KEY (ucircumflex)},
};
/* German. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_K[] = {
	{'@',  GDK_KEY (section)},
	{'[',  GDK_KEY (Adiaeresis)},
	{'\\', GDK_KEY (Odiaeresis)},
	{']',  GDK_KEY (Udiaeresis)},
	{'{',  GDK_KEY (adiaeresis)},
	{'|',  GDK_KEY (odiaeresis)},
	{'}',  GDK_KEY (udiaeresis)},
	{'~',  GDK_KEY (ssharp)},
};
/* Italian. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_Y[] = {
	{'#',  GDK_KEY (sterling)},
	{'@',  GDK_KEY (section)},
	{'[',  GDK_KEY (degree)},
	{'\\', GDK_KEY (ccedilla)},
	{']',  GDK_KEY (eacute)},
	{'`',  GDK_KEY (ugrave)},
	{'{',  GDK_KEY (agrave)},
	{'|',  GDK_KEY (ograve)},
	{'}',  GDK_KEY (egrave)},
	{'~',  GDK_KEY (igrave)},
};
/* Norwegian and Danish. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_E[] = {
	{'@',  GDK_KEY (Adiaeresis)},
	{'[',  GDK_KEY (AE)},
	{'\\', GDK_KEY (Ooblique)},
	{']',  GDK_KEY (Aring)},
	{'^',  GDK_KEY (Udiaeresis)},
	{'`',  GDK_KEY (adiaeresis)},
	{'{',  GDK_KEY (ae)},
	{'|',  GDK_KEY (oslash)},
	{'}',  GDK_KEY (aring)},
	{'~',  GDK_KEY (udiaeresis)},
};
/* Spanish. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_Z[] = {
	{'#',  GDK_KEY (sterling)},
	{'@',  GDK_KEY (section)},
	{'[',  GDK_KEY (exclamdown)},
	{'\\', GDK_KEY (Ntilde)},
	{']',  GDK_KEY (questiondown)},
	{'{',  GDK_KEY (degree)},
	{'|',  GDK_KEY (ntilde)},
	{'}',  GDK_KEY (ccedilla)},
};
/* Swedish. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_H[] = {
	{'@',  GDK_KEY (Eacute)},
	{'[',  GDK_KEY (Adiaeresis)},
	{'\\', GDK_KEY (Odiaeresis)},
	{']',  GDK_KEY (Aring)},
	{'^',  GDK_KEY (Udiaeresis)},
	{'`',  GDK_KEY (eacute)},
	{'{',  GDK_KEY (adiaeresis)},
	{'|',  GDK_KEY (odiaeresis)},
	{'}',  GDK_KEY (aring)},
	{'~',  GDK_KEY (udiaeresis)},
};
/* Swiss. VT220 and higher (per XTerm docs). */
static const struct _vte_iso2022_map16 _vte_iso2022_map_equal[] = {
	{'#',  GDK_KEY (ugrave)},
	{'@',  GDK_KEY (agrave)},
	{'[',  GDK_KEY (eacute)},
	{'\\', GDK_KEY (ccedilla)},
	{']',  GDK_KEY (ecircumflex)},
	{'^',  GDK_KEY (icircumflex)},
	{'_',  GDK_KEY (egrave)},
	{'`',  GDK_KEY (ocircumflex)},
	{'{',  GDK_KEY (adiaeresis)},
	{'|',  GDK_KEY (odiaeresis)},
	{'}',  GDK_KEY (udiaeresis)},
	{'~',  GDK_KEY (ucircumflex)},
};
/* Codepage 437. */
static const struct _vte_iso2022_map16 _vte_iso2022_map_U[] = {
#include "unitable.CP437"
};

/* Japanese.  JIS X 0201-1976 ("Roman" set), per RFC 1468/2237. */
static const struct _vte_iso2022_map16 _vte_iso2022_map_J[] = {
#include "unitable.JIS0201"
};
/* Japanese.  JIS X 0208-1978, per RFC 1468/2237. */
static const struct _vte_iso2022_map16 _vte_iso2022_map_wide_at[] = {
#include "unitable.JIS0208"
};
/* Chinese.  GB 2312-80, per RFC 1922. */
static const struct _vte_iso2022_map16 _vte_iso2022_map_wide_A[] = {
#include "unitable.GB2312"
};
/* Japanese.  JIS X 0208-1983, per RFC 1468/2237. */
static const struct _vte_iso2022_map16 _vte_iso2022_map_wide_B[] = {
#include "unitable.JIS0208"
};
/* Korean.  KS X 1001 (formerly KS C 5601), per Ken Lunde's
 * CJKV_Information_Processing. */
static const struct _vte_iso2022_map16 _vte_iso2022_map_wide_C[] = {
#include "unitable.KSX1001"
};
/* Japanese.  JIS X 0212-1990, per RFC 2237. */
static const struct _vte_iso2022_map16 _vte_iso2022_map_wide_D[] = {
#include "unitable.JIS0212"
};
/* Chinese.  CNS 11643-plane-1, per RFC 1922. */
static const struct _vte_iso2022_map32 _vte_iso2022_map_wide_G[] = {
#include "unitable.CNS11643"
};

static vteunistr unistr_next = VTE_UNISTR_START + 1;

struct VteUnistrDecomp {
	vteunistr prefix;
	gunichar  suffix;
};

GArray     *unistr_decomp;
GHashTable *unistr_comp;

#define DECOMP_FROM_INDEX(i)	g_array_index (unistr_decomp, struct VteUnistrDecomp, (i))
#define DECOMP_FROM_UNISTR(s)	DECOMP_FROM_INDEX ((s) - VTE_UNISTR_START)


G_DEFINE_TYPE(VteBg, vte_bg, G_TYPE_OBJECT)

struct _VteBgPrivate {
	GList *cache;
	GdkScreen *screen;
#ifdef GDK_WINDOWING_X11
	cairo_surface_t *root_surface;
        struct {
                GdkDisplay *display;
                GdkWindow *window;
                XID native_window;
                GdkAtom atom;
                Atom native_atom;
        } native;
#endif
};

typedef struct {
	VteBgSourceType source_type;
	GdkPixbuf *source_pixbuf;
	char *source_file;

	PangoColor tint_color;
	double saturation;
	cairo_surface_t *surface;
} VteBgCacheItem;

enum unistr_coverage {
	/* in increasing order of speed */
	COVERAGE_UNKNOWN = 0,		/* we don't know about the character yet */
	COVERAGE_USE_PANGO_LAYOUT_LINE,	/* use a PangoLayoutLine for the character */
	COVERAGE_USE_PANGO_GLYPH_STRING,	/* use a PangoGlyphString for the character */
	COVERAGE_USE_CAIRO_GLYPH	/* use a cairo_glyph_t for the character */
};

union unistr_font_info {
	/* COVERAGE_USE_PANGO_LAYOUT_LINE */
	struct {
		PangoLayoutLine *line;
	} using_pango_layout_line;
	/* COVERAGE_USE_PANGO_GLYPH_STRING */
	struct {
		PangoFont *font;
		PangoGlyphString *glyph_string;
	} using_pango_glyph_string;
	/* COVERAGE_USE_CAIRO_GLYPH */
	struct {
		cairo_scaled_font_t *scaled_font;
		unsigned int glyph_index;
	} using_cairo_glyph;
};

struct unistr_info {
	guchar coverage;
	guchar has_unknown_chars;
	guint16 width;
	union unistr_font_info ufi;
};

typedef struct _VteTerminalAccessiblePrivate {
	gboolean snapshot_contents_invalid;	/* This data is stale. */
	gboolean snapshot_caret_invalid;	/* This data is stale. */
	GString *snapshot_text;		/* Pointer to UTF-8 text. */
	GArray *snapshot_characters;	/* Offsets to character begin points. */
	GArray *snapshot_attributes;	/* Attributes, per byte. */
	GArray *snapshot_linebreaks;	/* Offsets to line breaks. */
	gint snapshot_caret;       /* Location of the cursor (in characters). */

	char *action_descriptions[LAST_ACTION];
} VteTerminalAccessiblePrivate;

enum direction {
	direction_previous = -1,
	direction_current = 0,
	direction_next = 1
};

static gunichar vte_terminal_accessible_get_character_at_offset(AtkText *text,
								gint offset);
static gpointer vte_terminal_accessible_parent_class;

G_DEFINE_TYPE(VteTerminalAccessibleFactory, vte_terminal_accessible_factory, ATK_TYPE_OBJECT_FACTORY)

static const char *vte_terminal_accessible_action_names[] = {
        "menu",
        NULL
};

static const char *vte_terminal_accessible_action_descriptions[] = {
        "Popup context menu",
        NULL
};

VteDebugFlags _vte_debug_flags;


static VteCell *vte_terminal_find_charcell(VteTerminal *terminal, glong col, glong row);
static void vte_bg_cache_item_free(VteBgCacheItem *item);
static void vte_bg_cache_prune_int(VteBg *bg, gboolean root);
static const cairo_user_data_key_t item_surface_key;
void _vte_keymap_map(guint keyval, GdkModifierType modifiers, gboolean sun_mode, gboolean hp_mode, gboolean legacy_mode, gboolean vt220_mode, gboolean app_cursor_keys, gboolean app_keypad_keys, struct _vte_termcap *termcap, const char *terminal, char **normal, gssize *normal_length, const char **special);
static void vte_terminal_set_visibility (VteTerminal *terminal, GdkVisibilityState state);
static void vte_terminal_set_termcap(VteTerminal *terminal, const char *path,
				     gboolean reset);
static void vte_terminal_paste(VteTerminal *terminal, GdkAtom board);
static void vte_terminal_real_copy_clipboard(VteTerminal *terminal);
static void vte_terminal_real_paste_clipboard(VteTerminal *terminal);
static gboolean vte_terminal_io_read(GIOChannel *channel,
				     GIOCondition condition,
				     VteTerminal *terminal);
static gboolean vte_terminal_io_write(GIOChannel *channel,
				      GIOCondition condition,
				      VteTerminal *terminal);
static void vte_terminal_match_hilite_clear(VteTerminal *terminal);
static void vte_terminal_match_hilite_hide(VteTerminal *terminal);
static void vte_terminal_match_hilite_show(VteTerminal *terminal, long x, long y);
static void vte_terminal_match_hilite_update(VteTerminal *terminal, long x, long y);
static void vte_terminal_match_contents_clear(VteTerminal *terminal);
static gboolean vte_terminal_background_update(VteTerminal *data);
static void vte_terminal_queue_background_update(VteTerminal *terminal);
static void vte_terminal_process_incoming(VteTerminal *terminal);
static void vte_terminal_emit_pending_signals(VteTerminal *terminal);
static gboolean vte_cell_is_selected(VteTerminal *terminal,
				     glong col, glong row, gpointer data);
static char *vte_terminal_get_text_range_maybe_wrapped(VteTerminal *terminal,
						       glong start_row,
						       glong start_col,
						       glong end_row,
						       glong end_col,
						       gboolean wrap,
						       VteSelectionFunc is_selected,
						       gpointer data,
						       GArray *attributes,
						       gboolean include_trailing_spaces);
static char *vte_terminal_get_text_maybe_wrapped(VteTerminal *terminal,
						 gboolean wrap,
						 VteSelectionFunc is_selected,
						 gpointer data,
						 GArray *attributes,
						 gboolean include_trailing_spaces);
static void _vte_terminal_disconnect_pty_read(VteTerminal *terminal);
static void _vte_terminal_disconnect_pty_write(VteTerminal *terminal);
static void vte_terminal_stop_processing (VteTerminal *terminal);

static inline gboolean vte_terminal_is_processing (VteTerminal *terminal);
static inline void vte_terminal_start_processing (VteTerminal *terminal);
static void vte_terminal_add_process_timeout (VteTerminal *terminal);
static void add_update_timeout (VteTerminal *terminal);
static void remove_update_timeout (VteTerminal *terminal);
static void reset_update_regions (VteTerminal *terminal);
static void vte_terminal_set_cursor_blinks_internal(VteTerminal *terminal, gboolean blink);
static void vte_terminal_set_font_full_internal(VteTerminal *terminal,
                                                const PangoFontDescription *font_desc,
                                                VteTerminalAntiAlias antialias);
static void _vte_check_cursor_blink(VteTerminal *terminal);

static gboolean process_timeout (gpointer data);
static gboolean update_timeout (gpointer data);

enum {
    COPY_CLIPBOARD,
    PASTE_CLIPBOARD,
    LAST_SIGNAL
};
static guint signals[LAST_SIGNAL];

enum {
        PROP_ORDER_0,
#if GTK_CHECK_VERSION (2, 91, 2)
        PROP_HADJUSTMENT,
        PROP_VADJUSTMENT,
        PROP_HSCROLL_POLICY,
        PROP_VSCROLL_POLICY,
#endif
        PROP_ALLOW_BOLD,
        PROP_AUDIBLE_BELL,
        PROP_BACKGROUND_IMAGE_FILE,
        PROP_BACKGROUND_IMAGE_PIXBUF,
        PROP_BACKGROUND_OPACITY,
        PROP_BACKGROUND_SATURATION,
        PROP_BACKGROUND_TINT_COLOR,
        PROP_BACKGROUND_TRANSPARENT,
        PROP_BACKSPACE_BINDING,
        PROP_CURSOR_BLINK_MODE,
        PROP_CURSOR_SHAPE,
        PROP_DELETE_BINDING,
        PROP_EMULATION,
        PROP_ENCODING,
        PROP_FONT_DESC,
        PROP_ICON_TITLE,
        PROP_MOUSE_POINTER_AUTOHIDE,
        PROP_PTY,
        PROP_PTY_OBJECT,
        PROP_SCROLL_BACKGROUND,
        PROP_SCROLLBACK_LINES,
        PROP_SCROLL_ON_KEYSTROKE,
        PROP_SCROLL_ON_OUTPUT,
        PROP_WINDOW_TITLE,
        PROP_WORD_CHARS,
        PROP_VISIBLE_BELL
};

/* these static variables are guarded by the GDK mutex */
static guint process_timeout_tag = 0;
static gboolean in_process_timeout;
static guint update_timeout_tag = 0;
static gboolean in_update_timeout;
static GList *active_terminals;
static GTimer *process_timer;

static const GtkBorder default_inner_border = { 1, 1, 1, 1 };

/* This list combined from the Linux termcap(5) man page, and
 * termcap_&_terminfo by Strang, Mui, and O'Reilly. */
struct _vte_capability_quark _vte_terminal_capability_strings[] = {
	{"!1", TRUE, 0},
	{"!2", TRUE, 0},
	{"!3", TRUE, 0},

	{"#1", TRUE, 0},
	{"#2", TRUE, 0},
	{"#3", TRUE, 0},
	{"#4", TRUE, 0},

	{"%0", TRUE, 0},
	{"%1", TRUE, 0},
	{"%2", TRUE, 0},
	{"%3", TRUE, 0},
	{"%4", TRUE, 0},
	{"%5", TRUE, 0},
	{"%6", TRUE, 0},
	{"%7", TRUE, 0},
	{"%8", TRUE, 0},
	{"%9", TRUE, 0},
	{"%a", TRUE, 0},
	{"%b", TRUE, 0},
	{"%c", TRUE, 0},
	{"%d", TRUE, 0},
	{"%e", TRUE, 0},
	{"%f", TRUE, 0},
	{"%g", TRUE, 0},
	{"%h", TRUE, 0},
	{"%i", TRUE, 0},
	{"%j", TRUE, 0},

	{"&0", TRUE, 0},
	{"&1", TRUE, 0},
	{"&2", TRUE, 0},
	{"&3", TRUE, 0},
	{"&4", TRUE, 0},
	{"&5", TRUE, 0},
	{"&6", TRUE, 0},
	{"&7", TRUE, 0},
	{"&8", TRUE, 0},
	{"&9", TRUE, 0},

	{"*0", TRUE, 0},
	{"*1", TRUE, 0},
	{"*2", TRUE, 0},
	{"*3", TRUE, 0},
	{"*4", TRUE, 0},
	{"*5", TRUE, 0},
	{"*6", TRUE, 0},
	{"*7", TRUE, 0},
	{"*8", TRUE, 0},
	{"*9", TRUE, 0},

	{"@0", TRUE, 0},
	{"@1", TRUE, 0},
	{"@2", TRUE, 0},
	{"@3", TRUE, 0},
	{"@4", TRUE, 0},
	{"@5", TRUE, 0},
	{"@6", TRUE, 0},
	{"@7", TRUE, 0},
	{"@8", TRUE, 0},
	{"@9", TRUE, 0},

	{"ae", FALSE, 0},
	{"al", FALSE, 0},
	{"AL", FALSE, 0},
	{"as", FALSE, 0},

	{"bc", FALSE, 0},
	{"bl", FALSE, 0},
	{"bt", FALSE, 0},

	{"cb", FALSE, 0},
	{"cc", FALSE, 0},
	{"cd", FALSE, 0},
	{"ce", FALSE, 0},
	{"ch", FALSE, 0},
	{"cl", FALSE, 0},
	{"cm", FALSE, 0},
	{"CM", FALSE, 0},
	{"cr", FALSE, 0},
	{"cs", FALSE, 0},
	{"ct", FALSE, 0},
	{"cv", FALSE, 0},

	{"dc", FALSE, 0},
	{"DC", FALSE, 0},
	{"dl", FALSE, 0},
	{"DL", FALSE, 0},
	{"dm", FALSE, 0},
	{"do", FALSE, 0},
	{"DO", FALSE, 0},
	{"ds", FALSE, 0},

	{"eA", FALSE, 0},
	{"ec", FALSE, 0},
	{"ed", FALSE, 0},
	{"ei", FALSE, 0},

	{"F1", TRUE, 0},
	{"F2", TRUE, 0},
	{"F3", TRUE, 0},
	{"F4", TRUE, 0},
	{"F5", TRUE, 0},
	{"F6", TRUE, 0},
	{"F7", TRUE, 0},
	{"F8", TRUE, 0},
	{"F9", TRUE, 0},
	{"FA", TRUE, 0},
	{"FB", TRUE, 0},
	{"FC", TRUE, 0},
	{"FD", TRUE, 0},
	{"FE", TRUE, 0},
	{"FF", TRUE, 0},
	{"FG", TRUE, 0},
	{"FH", TRUE, 0},
	{"FI", TRUE, 0},
	{"FJ", TRUE, 0},
	{"FK", TRUE, 0},
	{"FL", TRUE, 0},
	{"FM", TRUE, 0},
	{"FN", TRUE, 0},
	{"FO", TRUE, 0},
	{"FP", TRUE, 0},
	{"FQ", TRUE, 0},
	{"FR", TRUE, 0},
	{"FS", TRUE, 0},
	{"FT", TRUE, 0},
	{"FU", TRUE, 0},
	{"FV", TRUE, 0},
	{"FW", TRUE, 0},
	{"FX", TRUE, 0},
	{"FY", TRUE, 0},
	{"FZ", TRUE, 0},

	{"Fa", TRUE, 0},
	{"Fb", TRUE, 0},
	{"Fc", TRUE, 0},
	{"Fd", TRUE, 0},
	{"Fe", TRUE, 0},
	{"Ff", TRUE, 0},
	{"Fg", TRUE, 0},
	{"Fh", TRUE, 0},
	{"Fi", TRUE, 0},
	{"Fj", TRUE, 0},
	{"Fk", TRUE, 0},
	{"Fl", TRUE, 0},
	{"Fm", TRUE, 0},
	{"Fn", TRUE, 0},
	{"Fo", TRUE, 0},
	{"Fp", TRUE, 0},
	{"Fq", TRUE, 0},
	{"Fr", TRUE, 0},

	{"ff", FALSE, 0},
	{"fs", FALSE, 0},

	{"hd", FALSE, 0},
	{"ho", FALSE, 0},
	{"hu", FALSE, 0},

	{"i1", FALSE, 0},
	{"i3", FALSE, 0},

	{"ic", FALSE, 0},
	{"IC", FALSE, 0},
	{"if", FALSE, 0},
	{"im", FALSE, 0},
	{"ip", FALSE, 0},
	{"iP", FALSE, 0},
	{"is", FALSE, 0},

	{"K1", TRUE, 0},
	{"K2", TRUE, 0},
	{"K3", TRUE, 0},
	{"K4", TRUE, 0},
	{"K5", TRUE, 0},

	{"k0", TRUE, 0},
	{"k1", TRUE, 0},
	{"k2", TRUE, 0},
	{"k3", TRUE, 0},
	{"k4", TRUE, 0},
	{"k5", TRUE, 0},
	{"k6", TRUE, 0},
	{"k7", TRUE, 0},
	{"k8", TRUE, 0},
	{"k9", TRUE, 0},
	{"k;", TRUE, 0},
	{"ka", TRUE, 0},
	{"kA", TRUE, 0},
	{"kb", TRUE, 0},
	{"kB", TRUE, 0},
	{"kC", TRUE, 0},
	{"kd", TRUE, 0},
	{"kD", TRUE, 0},
	{"ke", TRUE, 0},
	{"kE", TRUE, 0},
	{"kF", TRUE, 0},
	{"kh", TRUE, 0},
	{"kH", TRUE, 0},
	{"kI", TRUE, 0},
	{"kl", TRUE, 0},
	{"kL", TRUE, 0},
	{"kM", TRUE, 0},
	{"kN", TRUE, 0},
	{"kP", TRUE, 0},
	{"kr", TRUE, 0},
	{"kR", TRUE, 0},
	{"ks", TRUE, 0},
	{"kS", TRUE, 0},
	{"kt", TRUE, 0},
	{"kT", TRUE, 0},
	{"ku", TRUE, 0},

	{"l0", FALSE, 0},
	{"l1", FALSE, 0},
	{"l2", FALSE, 0},
	{"l3", FALSE, 0},
	{"l4", FALSE, 0},
	{"l5", FALSE, 0},
	{"l6", FALSE, 0},
	{"l7", FALSE, 0},
	{"l8", FALSE, 0},
	{"l9", FALSE, 0},

	{"la", FALSE, 0},
	{"le", FALSE, 0},
	{"LE", FALSE, 0},
	{"LF", FALSE, 0},
	{"ll", FALSE, 0},
	{"LO", FALSE, 0},

	{"mb", FALSE, 0},
	{"MC", FALSE, 0},
	{"md", FALSE, 0},
	{"me", FALSE, 0},
	{"mh", FALSE, 0},
	{"mk", FALSE, 0},
	{"ml", FALSE, 0},
	{"ML", FALSE, 0},
	{"mm", FALSE, 0},
	{"mo", FALSE, 0},
	{"mp", FALSE, 0},
	{"mr", FALSE, 0},
	{"MR", FALSE, 0},
	{"mu", FALSE, 0},

	{"nd", FALSE, 0},
	{"nl", FALSE, 0},
	{"nw", FALSE, 0},

	{"pc", FALSE, 0},
	{"pf", FALSE, 0},
	{"pk", FALSE, 0},
	{"pl", FALSE, 0},
	{"pn", FALSE, 0},
	{"po", FALSE, 0},
	{"pO", FALSE, 0},
	{"ps", FALSE, 0},
	{"px", FALSE, 0},

	{"r1", FALSE, 0},
	{"r2", FALSE, 0},
	{"r3", FALSE, 0},

	{"RA", FALSE, 0},
	{"rc", FALSE, 0},
	{"rf", FALSE, 0},
	{"RF", FALSE, 0},
	{"RI", FALSE, 0},
	{"rp", FALSE, 0},
	{"rP", FALSE, 0},
	{"rs", FALSE, 0},
	{"RX", FALSE, 0},

	{"s0", FALSE, 0},
	{"s1", FALSE, 0},
	{"s2", FALSE, 0},
	{"s3", FALSE, 0},

	{"sa", FALSE, 0},
	{"SA", FALSE, 0},
	{"sc", FALSE, 0},
	{"se", FALSE, 0},
	{"sf", FALSE, 0},
	{"SF", FALSE, 0},
	/* {"so", 0}, standout is always the same as another attribute. */
	{"sr", FALSE, 0},
	{"SR", FALSE, 0},
	{"st", FALSE, 0},
	{"SX", FALSE, 0},

	{"ta", FALSE, 0},
	/* {"te", 0}, terminal end-use is "logical". */
	/* {"ti", 0}, terminal init is "logical". */
	{"ts", FALSE, 0},

	{"uc", FALSE, 0},
	{"ue", FALSE, 0},
	{"up", FALSE, 0},
	{"UP", FALSE, 0},
	{"us", FALSE, 0},

	{"vb", FALSE, 0},
	/* {"ve", FALSE, 0}, */
	{"vi", FALSE, 0},
	/* {"vs", FALSE, 0}, */

	{"wi", FALSE, 0},

	{"XF", FALSE, 0},
	{"XN", FALSE, 0},


	{"", FALSE, 0}
};

/* From some really old XTerm docs we had at the office, and an updated
 * version at Moy, Gildea, and Dickey. */
struct _vte_capability_string _vte_xterm_capability_strings[] = {
	{ENQ, "return-terminal-status", 0},
	{VT,  "vertical-tab", 0},
	{FF,  "form-feed", 0},

	{ESC " F", "7-bit-controls", 0},
	{ESC " G", "8-bit-controls", 0},
	{ESC " L", "ansi-conformance-level-1", 0},
	{ESC " M", "ansi-conformance-level-2", 0},
	{ESC " N", "ansi-conformance-level-3", 0},
	{ESC "#3", "double-height-top-half", 0},
	{ESC "#4", "double-height-bottom-half", 0},
	{ESC "#5", "single-width", 0},
	{ESC "#6", "double-width", 0},
	{ESC "#8", "screen-alignment-test", 0},

	/* These are actually designate-other-coding-system from ECMA 35,
	 * but we don't support the full repertoire.  Actually, we don't
	 * know what the full repertoire looks like. */
	{ESC "%%@", "iso8859-1-character-set", 0},
	{ESC "%%G", "utf-8-character-set", 0},

	{ESC "7", "save-cursor", 0},
	{ESC "8", "restore-cursor", 0},
	{ESC "=", "application-keypad", 0},
	{ESC ">", "normal-keypad", 0},
	{ESC "D", "index", 0},
	{ESC "E", "next-line", 0},
	{ESC "F", "cursor-lower-left", 0},
	{ESC "H", "tab-set", 0},
	{ESC "M", "reverse-index", 0},
	/* {ESC "N", "single-shift-g2", 0}, */
	/* {ESC "O", "single-shift-g3", 0}, */
	{ESC "P%s" ESC "\\", "device-control-string", 0},
	{ESC "V", "start-of-guarded-area", 0},
	{ESC "W", "end-of-guarded-area", 0},
	{ESC "X", "start-of-string", 0},
	{ESC "\\", "end-of-string", 0},
	{ESC "Z", "return-terminal-id", 0},
	{ESC "c", "full-reset", 0},
	{ESC "l", "memory-lock", 0},
	{ESC "m", "memory-unlock", 0},
	/* {ESC "n", "invoke-g2-character-set", 0}, */
	/* {ESC "o", "invoke-g3-character-set", 0}, */
	/* {ESC "|", "invoke-g3-character-set-as-gr", 0}, */
	/* {ESC "}", "invoke-g2-character-set-as-gr", 0}, */
	/* {ESC "~", "invoke-g1-character-set-as-gr", 0}, */

	/* APC stuff omitted. */

	/* DCS stuff omitted. */

	{CSI "@", "insert-blank-characters", 0},
	{CSI "%d@", "insert-blank-characters", 0},
	{CSI "A", "cursor-up", 0},
	{CSI "%dA", "cursor-up", 0},
	{CSI "B", "cursor-down", 0},
	{CSI "%dB", "cursor-down", 0},
	{CSI "C", "cursor-forward", 0},
	{CSI "%dC", "cursor-forward", 0},
	{CSI "D", "cursor-backward", 0},
	{CSI "%dD", "cursor-backward", 0},
	{CSI "E", "cursor-next-line", 0},
	{CSI "%dE", "cursor-next-line", 0},
	{CSI "F", "cursor-preceding-line", 0},
	{CSI "%dF", "cursor-preceding-line", 0},
	{CSI "G", "cursor-character-absolute", 0},
	{CSI "%dG", "cursor-character-absolute", 0},
	{CSI ";H", "cursor-position", 0},
	{CSI "%dH", "cursor-position", 0},
	{CSI "%d;H", "cursor-position", 0},
	{CSI ";%dH", "cursor-position", 0},
	{CSI "%d;%dH", "cursor-position", 0},
	{CSI "I", "cursor-forward-tabulation", 0},
	{CSI "J", "erase-in-display", 0},
	{CSI "%dJ", "erase-in-display", 0},
	{CSI "?J", "selective-erase-in-display", 0},
	{CSI "?%dJ", "selective-erase-in-display", 0},
	{CSI "K", "erase-in-line", 0},
	{CSI "%dK", "erase-in-line", 0},
	{CSI "?K", "selective-erase-in-line", 0},
	{CSI "?%dK", "selective-erase-in-line", 0},
	{CSI "L", "insert-lines", 0},
	{CSI "%dL", "insert-lines", 0},
	{CSI "M", "delete-lines", 0},
	{CSI "%dM", "delete-lines", 0},
	{CSI "P", "delete-characters", 0},
	{CSI "%dP", "delete-characters", 0},
	{CSI "S", "scroll-up", 0},
	{CSI "%dS", "scroll-up", 0},
	{CSI "T", "scroll-down", 0},
	{CSI "%dT", "scroll-down", 0},
	{CSI "%d;%d;%d;%d;%dT", "initiate-hilite-mouse-tracking", 0},
	{CSI "X", "erase-characters", 0},
	{CSI "%dX", "erase-characters", 0},
	{CSI "Z", "cursor-back-tab", 0},
	{CSI "%dZ", "cursor-back-tab", 0},

	{CSI "`", "character-position-absolute", 0},
	{CSI "%d`", "character-position-absolute", 0},
	{CSI "b", "repeat", 0},
	{CSI "%db", "repeat", 0},
	{CSI "c", "send-primary-device-attributes", 0},
	{CSI "%dc", "send-primary-device-attributes", 0},
	{CSI ">c", "send-secondary-device-attributes", 0},
	{CSI ">%dc", "send-secondary-device-attributes", 0},
	{CSI "=c", "send-tertiary-device-attributes", 0},
	{CSI "=%dc", "send-tertiary-device-attributes", 0},
	{CSI "?%mc", "linux-console-cursor-attributes", 0},
	{CSI "d", "line-position-absolute", 0},
	{CSI "%dd", "line-position-absolute", 0},
	{CSI ";f", "horizontal-and-vertical-position", 0},
	{CSI "%d;f", "horizontal-and-vertical-position", 0},
	{CSI ";%df", "horizontal-and-vertical-position", 0},
	{CSI "%d;%df", "horizontal-and-vertical-position", 0},
	{CSI "g", "tab-clear", 0},
	{CSI "%dg", "tab-clear", 0},

	{CSI "%mh", "set-mode", 0},
	{CSI "?%mh", "decset", 0},

	{CSI "%mi", "media-copy", 0},
	{CSI "?%mi", "dec-media-copy", 0},

	{CSI "%ml", "reset-mode", 0},
	{CSI "?%ml", "decreset", 0},

	{CSI "%mm", "character-attributes", 0},

	{CSI "%dn", "device-status-report", 0},
	{CSI "?%dn", "dec-device-status-report", 0},
	{CSI "!p", "soft-reset", 0},
	{CSI "%d;%d\"p", "set-conformance-level", 0},
	{CSI "%d\"q", "select-character-protection", 0},

	{CSI "r", "set-scrolling-region", 0},
	{CSI ";r", "set-scrolling-region", 0},
	{CSI ";%dr", "set-scrolling-region-from-start", 0},
	{CSI "%dr", "set-scrolling-region-to-end", 0},
	{CSI "%d;r", "set-scrolling-region-to-end", 0},
	{CSI "%d;%dr", "set-scrolling-region", 0},

	{CSI "?%mr", "restore-mode", 0},
	{CSI "s", "save-cursor", 0},
	{CSI "?%ms", "save-mode", 0},
	{CSI "u", "restore-cursor", 0},

	{CSI "%mt", "window-manipulation", 0},

	{CSI "%d;%d;%d;%dw", "enable-filter-rectangle", 0},
	{CSI "%dx", "request-terminal-parameters", 0},
	{CSI "%d;%d'z", "enable-locator-reporting", 0},
	{CSI "%m'{", "select-locator-events", 0},
	{CSI "%d'|", "request-locator-position", 0},

	/* Set text parameters, BEL-terminated versions. */
	{OSC ";%s" BEL, "set-icon-and-window-title", 0}, /* undocumented default */
	{OSC "0;%s" BEL, "set-icon-and-window-title", 0},
	{OSC "1;%s" BEL, "set-icon-title", 0},
	{OSC "2;%s" BEL, "set-window-title", 0},
	{OSC "3;%s" BEL, "set-xproperty", 0},
	{OSC "4;%s" BEL, "change-color", 0},
	{OSC "10;%s" BEL, "change-foreground-colors", 0},
	{OSC "11;%s" BEL, "change-background-colors", 0},
	{OSC "12;%s" BEL, "change-cursor-colors", 0},
	{OSC "13;%s" BEL, "change-mouse-cursor-foreground-colors", 0},
	{OSC "14;%s" BEL, "change-mouse-cursor-foreground-colors", 0},
	{OSC "15;%s" BEL, "change-tek-background-colors", 0},
	{OSC "16;%s" BEL, "change-tek-background-colors", 0},
	{OSC "17;%s" BEL, "change-highlight-colors", 0},
	{OSC "46;%s" BEL, "change-logfile", 0},
	{OSC "50;#%d" BEL, "change-font-number", 0},
	{OSC "50;%s" BEL, "change-font-name", 0},

	/* Set text parameters, ST-terminated versions. */
	{OSC ";%s" ST, "set-icon-and-window-title", 0}, /* undocumented default */
	{OSC "0;%s" ST, "set-icon-and-window-title", 0},
	{OSC "1;%s" ST, "set-icon-title", 0},
	{OSC "2;%s" ST, "set-window-title", 0},
	{OSC "3;%s" ST, "set-xproperty", 0},
	{OSC "4;%s" ST, "change-color", 0},
	{OSC "10;%s" ST, "change-foreground-colors", 0},
	{OSC "11;%s" ST, "change-background-colors", 0},
	{OSC "12;%s" ST, "change-cursor-colors", 0},
	{OSC "13;%s" ST, "change-mouse-cursor-foreground-colors", 0},
	{OSC "14;%s" ST, "change-mouse-cursor-foreground-colors", 0},
	{OSC "15;%s" ST, "change-tek-background-colors", 0},
	{OSC "16;%s" ST, "change-tek-background-colors", 0},
	{OSC "17;%s" ST, "change-highlight-colors", 0},
	{OSC "46;%s" ST, "change-logfile", 0},
	{OSC "50;#%d" ST, "change-font-number", 0},
	{OSC "50;%s" ST, "change-font-name", 0},

	/* These may be bogus, I can't find docs for them anywhere (#104154). */
	{OSC "21;%s" BEL, "set-text-property-21", 0},
	{OSC "2L;%s" BEL, "set-text-property-2L", 0},
	{OSC "21;%s" ST, "set-text-property-21", 0},
	{OSC "2L;%s" ST, "set-text-property-2L", 0},

	{NULL, NULL, 0},
};


/* Part: functions */
static inline VteCells *
_vte_cells_for_cell_array (VteCell *cells)
{
	if (G_UNLIKELY (!cells))
		return NULL;

	return (VteCells *) (((guchar *) cells) - G_STRUCT_OFFSET (VteCells, cells));
}

static VteCells *
_vte_cells_realloc (VteCells *cells, guint32 len)
{
	guint32 alloc_len = (1 << g_bit_storage (MAX (len, 80))) - 1;

	_vte_debug_print(VTE_DEBUG_RING, "Enlarging cell array of %d cells to %d cells\n", cells ? cells->alloc_len : 0, alloc_len);
	cells = g_realloc (cells, G_STRUCT_OFFSET (VteCells, cells) + alloc_len * sizeof (cells->cells[0]));
	cells->alloc_len = alloc_len;

	return cells;
}

static void
_vte_cells_free (VteCells *cells)
{
	_vte_debug_print(VTE_DEBUG_RING, "Freeing cell array of %d cells\n", cells->alloc_len);
	g_free (cells);
}


/*
 * VteRowData: A row's data
 */

void
_vte_row_data_init (VteRowData *row)
{
	memset (row, 0, sizeof (*row));
}

void
_vte_row_data_clear (VteRowData *row)
{
	VteCell *cells = row->cells;
	_vte_row_data_init (row);
	row->cells = cells;
}

void
_vte_row_data_fini (VteRowData *row)
{
	if (row->cells)
		_vte_cells_free (_vte_cells_for_cell_array (row->cells));
	row->cells = NULL;
}

static inline gboolean
_vte_row_data_ensure (VteRowData *row, gulong len)
{
	VteCells *cells = _vte_cells_for_cell_array (row->cells);
	if (G_LIKELY (cells && len <= cells->alloc_len))
		return TRUE;

	if (G_UNLIKELY (len >= 0xFFFF))
		return FALSE;

	row->cells = _vte_cells_realloc (cells, len)->cells;

	return TRUE;
}

void
_vte_row_data_insert (VteRowData *row, gulong col, const VteCell *cell)
{
	gulong i;

	if (G_UNLIKELY (!_vte_row_data_ensure (row, row->len + 1)))
		return;

	for (i = row->len; i > col; i--)
		row->cells[i] = row->cells[i - 1];

	row->cells[col] = *cell;
	row->len++;
}

void _vte_row_data_append (VteRowData *row, const VteCell *cell)
{
	if (G_UNLIKELY (!_vte_row_data_ensure (row, row->len + 1)))
		return;

	row->cells[row->len] = *cell;
	row->len++;
}

void _vte_row_data_remove (VteRowData *row, gulong col)
{
	gulong i;

	for (i = col + 1; i < row->len; i++)
		row->cells[i - 1] = row->cells[i];

	if (G_LIKELY (row->len))
		row->len--;
}

void _vte_row_data_fill (VteRowData *row, const VteCell *cell, gulong len)
{
	if (row->len < len) {
		gulong i = len - row->len;

		if (G_UNLIKELY (!_vte_row_data_ensure (row, len)))
			return;

		for (i = row->len; i < len; i++)
			row->cells[i] = *cell;

		row->len = len;
	}
}

void _vte_row_data_shrink (VteRowData *row, gulong max_len)
{
	if (max_len < row->len)
		row->len = max_len;
}

/*
 * VteRing: A buffer ring
 */

#ifdef VTE_DEBUG
static void
_vte_ring_validate (VteRing * ring)
{
	g_assert(ring != NULL);
	_vte_debug_print(VTE_DEBUG_RING,
			" Delta = %lu, Length = %lu, Max = %lu, Writable = %lu.\n",
			ring->start, ring->end - ring->start,
			ring->max, ring->end - ring->writable);

	g_assert (ring->start <= ring->writable);
	g_assert (ring->writable <= ring->end);

	g_assert (ring->end - ring->start <= ring->max);
	g_assert (ring->end - ring->writable <= ring->mask);
}
#else
#define _vte_ring_validate(ring) G_STMT_START {} G_STMT_END
#endif


void
_vte_ring_init (VteRing *ring, gulong max_rows)
{
	_vte_debug_print(VTE_DEBUG_RING, "New ring %p.\n", ring);

	memset (ring, 0, sizeof (*ring));

	ring->max = MAX (max_rows, 3);

	ring->mask = 31;
	ring->array = g_malloc0 (sizeof (ring->array[0]) * (ring->mask + 1));

	ring->attr_stream = _vte_file_stream_new ();
	ring->text_stream = _vte_file_stream_new ();
	ring->row_stream = _vte_file_stream_new ();

	ring->last_attr.text_offset = 0;
	ring->last_attr.attr.i = basic_cell.i.attr;
	ring->utf8_buffer = g_string_sized_new (128);

	_vte_row_data_init (&ring->cached_row);
	ring->cached_row_num = (gulong) -1;

	_vte_ring_validate(ring);
}

void
_vte_ring_fini (VteRing *ring)
{
	gulong i;

	for (i = 0; i <= ring->mask; i++)
		_vte_row_data_fini (&ring->array[i]);

	g_free (ring->array);

	g_object_unref (ring->attr_stream);
	g_object_unref (ring->text_stream);
	g_object_unref (ring->row_stream);

	g_string_free (ring->utf8_buffer, TRUE);

	_vte_row_data_fini (&ring->cached_row);
}

typedef struct _VteRowRecord {
	gsize text_offset;
	gsize attr_offset;
} VteRowRecord;

static gboolean
_vte_ring_read_row_record (VteRing *ring, VteRowRecord *record, gulong position)
{
	return _vte_stream_read (ring->row_stream, position * sizeof (*record), (char *) record, sizeof (*record));
}

static void
_vte_ring_append_row_record (VteRing *ring, const VteRowRecord *record, gulong position)
{
	_vte_stream_append (ring->row_stream, (const char *) record, sizeof (*record));
}

static void
_vte_ring_freeze_row (VteRing *ring, gulong position, const VteRowData *row)
{
	VteRowRecord record;
	VteCell *cell;
	GString *buffer = ring->utf8_buffer;
	int i;

	_vte_debug_print (VTE_DEBUG_RING, "Freezing row %lu.\n", position);

	record.text_offset = _vte_stream_head (ring->text_stream);
	record.attr_offset = _vte_stream_head (ring->attr_stream);

	g_string_set_size (buffer, 0);
	for (i = 0, cell = row->cells; i < row->len; i++, cell++) {
		VteIntCellAttr attr;
		int num_chars;

		/* Attr storage:
		 *
		 * 1. We don't store attrs for fragments.  They can be
		 * reconstructed using the columns of their start cell.
		 *
		 * 2. We store one attr per vteunistr character starting
		 * from the second character, with columns=0.
		 *
		 * That's enough to reconstruct the attrs, and to store
		 * the text in real UTF-8.
		 */
		attr.s = cell->attr;
		if (G_LIKELY (!attr.s.fragment)) {

			if (ring->last_attr.attr.i != attr.i) {
				ring->last_attr.text_offset = record.text_offset + buffer->len;
				_vte_stream_append (ring->attr_stream, (const char *) &ring->last_attr, sizeof (ring->last_attr));
				if (!buffer->len)
					/* This row doesn't use last_attr, adjust */
					record.attr_offset += sizeof (ring->last_attr);
				ring->last_attr.attr = attr;
			}

			num_chars = _vte_unistr_strlen (cell->c);
			if (num_chars > 1) {
				attr.s.columns = 0;
				ring->last_attr.text_offset = record.text_offset + buffer->len
							    + g_unichar_to_utf8 (_vte_unistr_get_base (cell->c), NULL);
				_vte_stream_append (ring->attr_stream, (const char *) &ring->last_attr, sizeof (ring->last_attr));
				ring->last_attr.attr = attr;
			}

			_vte_unistr_append_to_string (cell->c, buffer);
		}
	}
	if (!row->attr.soft_wrapped)
		g_string_append_c (buffer, '\n');

	_vte_stream_append (ring->text_stream, buffer->str, buffer->len);
	_vte_ring_append_row_record (ring, &record, position);
}

static void
_vte_ring_thaw_row (VteRing *ring, gulong position, VteRowData *row, gboolean do_truncate)
{
	VteRowRecord records[2], record;
	VteIntCellAttr attr;
	VteCellAttrChange attr_change;
	VteCell cell;
	const char *p, *q, *end;
	GString *buffer = ring->utf8_buffer;

	_vte_debug_print (VTE_DEBUG_RING, "Thawing row %lu.\n", position);

	_vte_row_data_clear (row);

	attr_change.text_offset = 0;

	if (!_vte_ring_read_row_record (ring, &records[0], position))
		return;
	if ((position + 1) * sizeof (records[0]) < _vte_stream_head (ring->row_stream)) {
		if (!_vte_ring_read_row_record (ring, &records[1], position + 1))
			return;
	} else
		records[1].text_offset = _vte_stream_head (ring->text_stream);

	g_string_set_size (buffer, records[1].text_offset - records[0].text_offset);
	if (!_vte_stream_read (ring->text_stream, records[0].text_offset, buffer->str, buffer->len))
		return;

	record = records[0];

	if (G_LIKELY (buffer->len && buffer->str[buffer->len - 1] == '\n'))
		buffer->len--;
	else
		row->attr.soft_wrapped = TRUE;

	p = buffer->str;
	end = p + buffer->len;
	while (p < end) {

		if (record.text_offset >= ring->last_attr.text_offset) {
			attr = ring->last_attr.attr;
		} else {
			if (record.text_offset >= attr_change.text_offset) {
				if (!_vte_stream_read (ring->attr_stream, record.attr_offset, (char *) &attr_change, sizeof (attr_change)))
					return;
				record.attr_offset += sizeof (attr_change);
			}
			attr = attr_change.attr;
		}

		cell.attr = attr.s;
		cell.c = g_utf8_get_char (p);

		q = g_utf8_next_char (p);
		record.text_offset += q - p;
		p = q;

		if (G_UNLIKELY (cell.attr.columns == 0)) {
			if (G_LIKELY (row->len)) {
				/* Combine it */
				row->cells[row->len - 1].c = _vte_unistr_append_unichar (row->cells[row->len - 1].c, cell.c);
			} else {
				cell.attr.columns = 1;
				_vte_row_data_append (row, &cell);
			}
		} else {
			_vte_row_data_append (row, &cell);
			if (cell.attr.columns > 1) {
				/* Add the fragments */
				int i, columns = cell.attr.columns;
				cell.attr.fragment = 1;
				cell.attr.columns = 1;
				for (i = 1; i < columns; i++)
					_vte_row_data_append (row, &cell);
			}
		}
	}

	if (do_truncate) {
		if (records[0].text_offset < ring->last_attr.text_offset)
			if (!_vte_stream_read (ring->attr_stream, records[0].attr_offset, (char *) &ring->last_attr, sizeof (ring->last_attr))) {
				ring->last_attr.text_offset = 0;
				ring->last_attr.attr.i = basic_cell.i.attr;
			}
		_vte_stream_truncate (ring->row_stream, position * sizeof (record));
		_vte_stream_truncate (ring->attr_stream, records[0].attr_offset);
		_vte_stream_truncate (ring->text_stream, records[0].text_offset);
	}
}

static void
_vte_ring_reset_streams (VteRing *ring, gulong position)
{
	_vte_debug_print (VTE_DEBUG_RING, "Reseting streams to %lu.\n", position);

	_vte_stream_reset (ring->row_stream, position * sizeof (VteRowRecord));
	_vte_stream_reset (ring->text_stream, 0);
	_vte_stream_reset (ring->attr_stream, 0);

	ring->last_attr.text_offset = 0;
	ring->last_attr.attr.i = basic_cell.i.attr;

	ring->last_page = position;
}

static void
_vte_ring_new_page (VteRing *ring)
{
	_vte_debug_print (VTE_DEBUG_RING, "Starting new stream page at %lu.\n", ring->writable);

	_vte_stream_new_page (ring->attr_stream);
	_vte_stream_new_page (ring->text_stream);
	_vte_stream_new_page (ring->row_stream);

	ring->last_page = ring->writable;
}



static inline VteRowData *
_vte_ring_writable_index (VteRing *ring, gulong position)
{
	return &ring->array[position & ring->mask];
}

const VteRowData *
_vte_ring_index (VteRing *ring, gulong position)
{
	if (G_LIKELY (position >= ring->writable))
		return _vte_ring_writable_index (ring, position);

	if (ring->cached_row_num != position) {
		_vte_debug_print(VTE_DEBUG_RING, "Caching row %lu.\n", position);
		_vte_ring_thaw_row (ring, position, &ring->cached_row, FALSE);
		ring->cached_row_num = position;
	}

	return &ring->cached_row;
}

static void _vte_ring_ensure_writable (VteRing *ring, gulong position);
static void _vte_ring_ensure_writable_room (VteRing *ring);

VteRowData *
_vte_ring_index_writable (VteRing *ring, gulong position)
{
	_vte_ring_ensure_writable (ring, position);
	return _vte_ring_writable_index (ring, position);
}

static void
_vte_ring_freeze_one_row (VteRing *ring)
{
	VteRowData *row;

	if (G_UNLIKELY (ring->writable == ring->start))
		_vte_ring_reset_streams (ring, ring->writable);

	row = _vte_ring_writable_index (ring, ring->writable);
	_vte_ring_freeze_row (ring, ring->writable, row);

	ring->writable++;

	if (G_UNLIKELY (ring->writable == ring->last_page || ring->writable - ring->last_page >= ring->max))
		_vte_ring_new_page (ring);
}

static void
_vte_ring_thaw_one_row (VteRing *ring)
{
	VteRowData *row;

	g_assert (ring->start < ring->writable);

	_vte_ring_ensure_writable_room (ring);

	ring->writable--;

	if (ring->writable == ring->cached_row_num)
		ring->cached_row_num = (gulong) -1; /* Invalidate cached row */

	row = _vte_ring_writable_index (ring, ring->writable);

	_vte_ring_thaw_row (ring, ring->writable, row, TRUE);
}

static void
_vte_ring_discard_one_row (VteRing *ring)
{
	ring->start++;
	if (G_UNLIKELY (ring->start == ring->writable)) {
		_vte_ring_reset_streams (ring, 0);
	}
	if (ring->start > ring->writable)
		ring->writable = ring->start;
}

static void
_vte_ring_maybe_freeze_one_row (VteRing *ring)
{
	if (G_LIKELY (ring->writable + ring->mask == ring->end))
		_vte_ring_freeze_one_row (ring);
}

static void
_vte_ring_maybe_discard_one_row (VteRing *ring)
{
	if ((gulong) _vte_ring_length (ring) == ring->max)
		_vte_ring_discard_one_row (ring);
}

static void
_vte_ring_ensure_writable_room (VteRing *ring)
{
	gulong new_mask, old_mask, i, end;
	VteRowData *old_array, *new_array;;

	if (G_LIKELY (ring->writable + ring->mask > ring->end))
		return;

	_vte_debug_print(VTE_DEBUG_RING, "Enlarging writable array.\n");

	old_mask = ring->mask;
	old_array = ring->array;

	ring->mask = (ring->mask << 1) + 1;
	ring->array = g_malloc0 (sizeof (ring->array[0]) * (ring->mask + 1));

	new_mask = ring->mask;
	new_array = ring->array;

	end = ring->writable + old_mask + 1;
	for (i = ring->writable; i < end; i++)
		new_array[i & new_mask] = old_array[i & old_mask];

	g_free (old_array);
}

static void
_vte_ring_ensure_writable (VteRing *ring, gulong position)
{
	if (G_LIKELY (position >= ring->writable))
		return;

	_vte_debug_print(VTE_DEBUG_RING, "Ensure writable %lu.\n", position);

	while (position < ring->writable)
		_vte_ring_thaw_one_row (ring);
}

/**
 * _vte_ring_resize:
 * @ring: a #VteRing
 * @max_rows: new maximum numbers of rows in the ring
 *
 * Changes the number of lines the ring can contain.
 */
void
_vte_ring_resize (VteRing *ring, gulong max_rows)
{
	_vte_debug_print(VTE_DEBUG_RING, "Resizing to %lu.\n", max_rows);
	_vte_ring_validate(ring);

	/* Adjust the start of tail chunk now */
	if ((gulong) _vte_ring_length (ring) > max_rows) {
		ring->start = ring->end - max_rows;
		if (ring->start >= ring->writable) {
			_vte_ring_reset_streams (ring, 0);
			ring->writable = ring->start;
		}
	}

	ring->max = max_rows;
}

void
_vte_ring_shrink (VteRing *ring, gulong max_len)
{
	if ((gulong) _vte_ring_length (ring) <= max_len)
		return;

	_vte_debug_print(VTE_DEBUG_RING, "Shrinking to %lu.\n", max_len);
	_vte_ring_validate(ring);

	if (ring->writable - ring->start <= max_len)
		ring->end = ring->start + max_len;
	else {
		while (ring->writable - ring->start > max_len) {
			_vte_ring_ensure_writable (ring, ring->writable - 1);
			ring->end = ring->writable;
		}
	}

	/* TODO May want to shrink down ring->array */

	_vte_ring_validate(ring);
}

/**
 * _vte_ring_insert_internal:
 * @ring: a #VteRing
 * @position: an index
 *
 * Inserts a new, empty, row into @ring at the @position'th offset.
 * The item at that position and any items after that are shifted down.
 *
 * Return: the newly added row.
 */
VteRowData *
_vte_ring_insert (VteRing *ring, gulong position)
{
	gulong i;
	VteRowData *row, tmp;

	_vte_debug_print(VTE_DEBUG_RING, "Inserting at position %lu.\n", position);
	_vte_ring_validate(ring);

	_vte_ring_maybe_discard_one_row (ring);

	_vte_ring_ensure_writable (ring, position);
	_vte_ring_ensure_writable_room (ring);

	g_assert (position >= ring->writable && position <= ring->end);

	tmp = *_vte_ring_writable_index (ring, ring->end);
	for (i = ring->end; i > position; i--)
		*_vte_ring_writable_index (ring, i) = *_vte_ring_writable_index (ring, i - 1);
	*_vte_ring_writable_index (ring, position) = tmp;

	row = _vte_ring_writable_index (ring, position);
	_vte_row_data_clear (row);
	ring->end++;

	_vte_ring_maybe_freeze_one_row (ring);

	_vte_ring_validate(ring);
	return row;
}

/**
 * _vte_ring_remove:
 * @ring: a #VteRing
 * @position: an index
 *
 * Removes the @position'th item from @ring.
 */
void
_vte_ring_remove (VteRing * ring, gulong position)
{
	gulong i;
	VteRowData tmp;

	_vte_debug_print(VTE_DEBUG_RING, "Removing item at position %lu.\n", position);
	_vte_ring_validate(ring);

	if (G_UNLIKELY (!_vte_ring_contains (ring, position)))
		return;

	_vte_ring_ensure_writable (ring, position);

	tmp = *_vte_ring_writable_index (ring, position);
	for (i = position; i < ring->end - 1; i++)
		*_vte_ring_writable_index (ring, i) = *_vte_ring_writable_index (ring, i + 1);
	*_vte_ring_writable_index (ring, ring->end - 1) = tmp;

	if (ring->end > ring->writable)
		ring->end--;

	_vte_ring_validate(ring);
}


/**
 * _vte_ring_append:
 * @ring: a #VteRing
 * @data: the new item
 *
 * Appends a new item to the ring.
 *
 * Return: the newly added row.
 */
VteRowData *
_vte_ring_append (VteRing * ring)
{
	return _vte_ring_insert (ring, _vte_ring_next (ring));
}


static gboolean
_vte_ring_write_row (VteRing *ring,
		     GOutputStream *stream,
		     VteRowData *row,
		     VteTerminalWriteFlags flags,
		     GCancellable *cancellable,
		     GError **error)
{
	VteCell *cell;
	GString *buffer = ring->utf8_buffer;
	int i;
	gsize bytes_written;

	/* Simple version of the loop in _vte_ring_freeze_row().
	 * TODO Should unify one day */
	g_string_set_size (buffer, 0);
	for (i = 0, cell = row->cells; i < row->len; i++, cell++) {
		if (G_LIKELY (!cell->attr.fragment))
			_vte_unistr_append_to_string (cell->c, buffer);
	}
	if (!row->attr.soft_wrapped)
		g_string_append_c (buffer, '\n');

	return g_output_stream_write_all (stream, buffer->str, buffer->len, &bytes_written, cancellable, error);
}

/**
 * _vte_ring_write_contents:
 * @ring: a #VteRing
 * @stream: a #GOutputStream to write to
 * @flags: a set of #VteTerminalWriteFlags
 * @cancellable: optional #GCancellable object, %NULL to ignore
 * @error: a #GError location to store the error occuring, or %NULL to ignore
 *
 * Write entire ring contents to @stream according to @flags.
 *
 * Return: %TRUE on success, %FALSE if there was an error
 */
gboolean
_vte_ring_write_contents (VteRing *ring,
			  GOutputStream *stream,
			  VteTerminalWriteFlags flags,
			  GCancellable *cancellable,
			  GError **error)
{
	gulong i;

	_vte_debug_print(VTE_DEBUG_RING, "Writing contents to GOutputStream.\n");

	if (ring->start < ring->writable) {
		VteRowRecord record;
		/* XXX what to do in case of error? */
		if (_vte_ring_read_row_record (ring, &record, ring->start)) {
			if (!_vte_stream_write_contents (ring->text_stream, stream,
							 record.text_offset,
							 cancellable, error))
				return FALSE;
		}
	}

	for (i = ring->writable; i < ring->end; i++) {
		if (!_vte_ring_write_row (ring, stream,
					  _vte_ring_writable_index (ring, i),
					  flags, cancellable, error))
			return FALSE;
	}

	return TRUE;
}

static void
vte_reaper_child_watch_cb(GPid pid, gint status, gpointer data)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Reaper emitting child-exited signal.\n");
	g_signal_emit_by_name(data, "child-exited", pid, status);
	g_spawn_close_pid (pid);
}

/**
 * vte_reaper_add_child:
 * @pid: the ID of a child process which will be monitored
 *
 * Ensures that child-exited signals will be emitted when @pid exits.  This is
 * necessary for correct operation when running with glib versions >= 2.4.
 *
 * Returns: the new source ID
 *
 * Since 0.11.11
 */
int
vte_reaper_add_child(GPid pid)
{
	return g_child_watch_add_full(G_PRIORITY_LOW,
				      pid,
				      vte_reaper_child_watch_cb,
				      vte_reaper_get(),
				      (GDestroyNotify)g_object_unref);
}

static void
vte_reaper_init(VteReaper *reaper)
{
}

static GObject*
vte_reaper_constructor (GType                  type,
                        guint                  n_construct_properties,
                        GObjectConstructParam *construct_properties)
{
  if (singleton_reaper) {
	  return g_object_ref (singleton_reaper);
  } else {
	  GObject *obj;
	  obj = G_OBJECT_CLASS (vte_reaper_parent_class)->constructor (type, n_construct_properties, construct_properties);
	  singleton_reaper = VTE_REAPER (obj);
	  return obj;
  }
}


static void
vte_reaper_finalize(GObject *reaper)
{
	G_OBJECT_CLASS(vte_reaper_parent_class)->finalize(reaper);
	singleton_reaper = NULL;
}

static void
vte_reaper_class_init(VteReaperClass *klass)
{
	GObjectClass *gobject_class;

        /**
         * VteReaper::child-exited:
         * @vtereaper: the object which received the signal
         * @arg1: the process ID of the exited child
         * @arg2: the status of the exited child, as returned by waitpid()
         * 
         * Emitted when the #VteReaper object detects that a child of the
         * current process has exited.
         *
         * Since: 0.11.11
         */
	klass->child_exited_signal = g_signal_new(g_intern_static_string("child-exited"),
						  G_OBJECT_CLASS_TYPE(klass),
						  G_SIGNAL_RUN_LAST,
						  0,
						  NULL,
						  NULL,
						  _vte_marshal_VOID__INT_INT,
						  G_TYPE_NONE,
						  2, G_TYPE_INT, G_TYPE_INT);

	gobject_class = G_OBJECT_CLASS(klass);
	gobject_class->constructor = vte_reaper_constructor;
	gobject_class->finalize = vte_reaper_finalize;
}

/**
 * vte_reaper_get:
 *
 * Finds the address of the global #VteReaper object, creating the object if
 * necessary.
 *
 * Returns: the global #VteReaper object, which should not be unreffed.
 */
VteReaper *
vte_reaper_get(void)
{
	return g_object_new(VTE_TYPE_REAPER, NULL);
}

#ifdef REAPER_MAIN

#include <unistd.h>

GMainContext *context;
GMainLoop *loop;
pid_t child;

static void
child_exited(GObject *object, int pid, int status, gpointer data)
{
	g_print("[parent] Child with pid %d exited with code %d, "
		"was waiting for %d.\n", pid, status, GPOINTER_TO_INT(data));
	if (child == pid) {
		g_print("[parent] Quitting.\n");
		g_main_loop_quit(loop);
	}
}

int
main(int argc, char **argv)
{
	VteReaper *reaper;
	pid_t p, q;

	_vte_debug_init();

	g_type_init();
	context = g_main_context_default();
	loop = g_main_loop_new(context, FALSE);
	reaper = vte_reaper_get();

	g_print("[parent] Forking.\n");
	p = fork();
	switch (p) {
		case -1:
			g_print("[parent] Fork failed.\n");
			g_assert_not_reached();
			break;
		case 0:
			g_print("[child]  Going to sleep.\n");
			sleep(10);
			g_print("[child]  Quitting.\n");
			_exit(30);
			break;
		default:
			g_print("[parent] Starting to wait for %d.\n", p);
			child = p;
			g_signal_connect(reaper,
					 "child-exited",
					 G_CALLBACK(child_exited),
					 GINT_TO_POINTER(child));
			break;
	}

	g_print("[parent] Forking.\n");
	q = fork();
	switch (q) {
		case -1:
			g_print("[parent] Fork failed.\n");
			g_assert_not_reached();
			break;
		case 0:
			g_print("[child]  Going to sleep.\n");
			sleep(5);
			_exit(5);
			break;
		default:
			g_print("[parent] Not waiting for %d.\n", q);
			break;
	}


	g_main_loop_run(loop);

	g_object_unref(reaper);

	return 0;
}
#endif

/* VOID:INT,INT (marshal.list:1) */
G_GNUC_INTERNAL void _vte_marshal_VOID__INT_INT (GClosure     *closure,
                                                 GValue       *return_value,
                                                 guint         n_param_values,
                                                 const GValue *param_values,
                                                 gpointer      invocation_hint,
                                                 gpointer      marshal_data);
void
_vte_marshal_VOID__INT_INT (GClosure     *closure,
                            GValue       *return_value G_GNUC_UNUSED,
                            guint         n_param_values,
                            const GValue *param_values,
                            gpointer      invocation_hint G_GNUC_UNUSED,
                            gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__INT_INT) (gpointer     data1,
                                              gint         arg_1,
                                              gint         arg_2,
                                              gpointer     data2);
  register GMarshalFunc_VOID__INT_INT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure))
    {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
    }
  else
    {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__INT_INT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_int (param_values + 1),
            g_marshal_value_peek_int (param_values + 2),
            data2);
}

/* VOID:OBJECT,OBJECT (marshal.list:2) */
G_GNUC_INTERNAL void _vte_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                                       GValue       *return_value,
                                                       guint         n_param_values,
                                                       const GValue *param_values,
                                                       gpointer      invocation_hint,
                                                       gpointer      marshal_data);
void
_vte_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                  GValue       *return_value G_GNUC_UNUSED,
                                  guint         n_param_values,
                                  const GValue *param_values,
                                  gpointer      invocation_hint G_GNUC_UNUSED,
                                  gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__OBJECT_OBJECT) (gpointer     data1,
                                                    gpointer     arg_1,
                                                    gpointer     arg_2,
                                                    gpointer     data2);
  register GMarshalFunc_VOID__OBJECT_OBJECT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure))
    {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
    }
  else
    {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__OBJECT_OBJECT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_object (param_values + 1),
            g_marshal_value_peek_object (param_values + 2),
            data2);
}

/* VOID:STRING,UINT (marshal.list:3) */
G_GNUC_INTERNAL void _vte_marshal_VOID__STRING_UINT (GClosure     *closure,
                                                     GValue       *return_value,
                                                     guint         n_param_values,
                                                     const GValue *param_values,
                                                     gpointer      invocation_hint,
                                                     gpointer      marshal_data);
void
_vte_marshal_VOID__STRING_UINT (GClosure     *closure,
                                GValue       *return_value G_GNUC_UNUSED,
                                guint         n_param_values,
                                const GValue *param_values,
                                gpointer      invocation_hint G_GNUC_UNUSED,
                                gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__STRING_UINT) (gpointer     data1,
                                                  gpointer     arg_1,
                                                  guint        arg_2,
                                                  gpointer     data2);
  register GMarshalFunc_VOID__STRING_UINT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure))
    {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
    }
  else
    {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__STRING_UINT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_string (param_values + 1),
            g_marshal_value_peek_uint (param_values + 2),
            data2);
}

/* VOID:UINT,UINT (marshal.list:4) */
G_GNUC_INTERNAL void _vte_marshal_VOID__UINT_UINT (GClosure     *closure,
                                                   GValue       *return_value,
                                                   guint         n_param_values,
                                                   const GValue *param_values,
                                                   gpointer      invocation_hint,
                                                   gpointer      marshal_data);
void
_vte_marshal_VOID__UINT_UINT (GClosure     *closure,
                              GValue       *return_value G_GNUC_UNUSED,
                              guint         n_param_values,
                              const GValue *param_values,
                              gpointer      invocation_hint G_GNUC_UNUSED,
                              gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__UINT_UINT) (gpointer     data1,
                                                guint        arg_1,
                                                guint        arg_2,
                                                gpointer     data2);
  register GMarshalFunc_VOID__UINT_UINT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure))
    {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
    }
  else
    {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__UINT_UINT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_uint (param_values + 1),
            g_marshal_value_peek_uint (param_values + 2),
            data2);
}

void
_vte_debug_init(void)
{
#ifdef VTE_DEBUG
  const GDebugKey keys[] = {
    { "misc",         VTE_DEBUG_MISC         },
    { "io",           VTE_DEBUG_IO           },
    { "adj",          VTE_DEBUG_ADJ          },
    { "updates",      VTE_DEBUG_UPDATES      },
    { "events",       VTE_DEBUG_EVENTS       },
    { "parse",        VTE_DEBUG_PARSE        },
    { "signals",      VTE_DEBUG_SIGNALS      },
    { "selection",    VTE_DEBUG_SELECTION    },
    { "substitution", VTE_DEBUG_SUBSTITUTION },
    { "ring",         VTE_DEBUG_RING         },
    { "pty",          VTE_DEBUG_PTY          },
    { "cursor",       VTE_DEBUG_CURSOR       },
    { "keyboard",     VTE_DEBUG_KEYBOARD     },
    { "lifecycle",    VTE_DEBUG_LIFECYCLE    },
    { "trie",         VTE_DEBUG_TRIE         },
    { "work",         VTE_DEBUG_WORK         },
    { "cells",        VTE_DEBUG_CELLS        },
    { "timeout",      VTE_DEBUG_TIMEOUT      },
    { "draw",         VTE_DEBUG_DRAW         },
    { "ally",         VTE_DEBUG_ALLY         },
    { "pangocairo",   VTE_DEBUG_PANGOCAIRO   },
    { "widget-size",  VTE_DEBUG_WIDGET_SIZE  },
    { "bg",           VTE_DEBUG_BG           }
  };

  _vte_debug_flags = g_parse_debug_string (g_getenv("VTE_DEBUG"),
                                           keys, G_N_ELEMENTS (keys));
  _vte_debug_print(0xFFFFFFFF, "VTE debug flags = %x\n", _vte_debug_flags);
#endif /* VTE_DEBUG */
}

#if 0
/**
 * vte_capability_init:
 *
 * Initializes the vte_terminal_capability_strings and
 * vte_xterm_capability_strings structures used by the terminal.  Can
 * be called multiple times without ill effect.
 *
 * Returns: void
 */

void
_vte_capability_init(void)
{
	unsigned int i;
	for (i = 0; _vte_terminal_capability_strings[i].capability[0]; i++) {
		const char *tmp;
		GQuark quark;
		tmp = _vte_terminal_capability_strings[i].capability;
		quark = g_quark_from_static_string(tmp);
		_vte_terminal_capability_strings[i].quark = quark;
	}
	for (i = 0; i < G_N_ELEMENTS(_vte_xterm_capability_strings); i++) {
		const char *tmp;
		GQuark quark;
		tmp = _vte_xterm_capability_strings[i].value;
		if (tmp != NULL) {
			quark = g_quark_from_static_string(tmp);
			_vte_xterm_capability_strings[i].quark = quark;
		}
	}
}
#endif

/* We can't use g_utf8_strlen as that's not nul-safe :( */
static glong
_vte_conv_utf8_strlen(const gchar *p, gssize max)
{
	const gchar *q = p + max;
	glong length = -1;
	do {
		length++;
		p = g_utf8_next_char(p);
	} while (p < q);
	return length;
}

/* A bogus UTF-8 to UTF-8 conversion function which attempts to provide the
 * same semantics as g_iconv(). */
static size_t
_vte_conv_utf8_utf8(GIConv converter,
		    const gchar **inbuf,
		    gsize *inbytes_left,
		    gchar **outbuf,
		    gsize *outbytes_left)
{
	gboolean validated;
	const gchar *endptr;
	size_t bytes;
	guint skip;

	/* We don't tolerate shenanigans! */
	g_assert(*outbytes_left >= *inbytes_left);

	/* The only error we can throw is EILSEQ, so check for that here. */
	validated = g_utf8_validate(*inbuf, *inbytes_left, &endptr);

	/* Copy whatever data was validated. */
	bytes = endptr - *inbuf;
	memcpy(*outbuf, *inbuf, bytes);
	*inbuf += bytes;
	*outbuf += bytes;
	*outbytes_left -= bytes;
	*inbytes_left -= bytes;

	/* Return 0 (number of non-reversible conversions performed) if everything
	 * looked good, else EILSEQ. */
	if (validated) {
		return 0;
	}

	/* Determine why the end of the string is not valid.
	 * We are pur b@stards for running g_utf8_next_char() on an
	 * invalid sequence. */
	skip = g_utf8_next_char(*inbuf) - *inbuf;
	if (skip > *inbytes_left) {
		/* We didn't have enough bytes to validate the character.
		 * That qualifies for EINVAL, but only if the part of the
		 * character that we have is a valid prefix to a character.
		 * Differentiating those requires verifying that all the
		 * remaining bytes after this one are UTF-8 continuation
		 * bytes.  Actually even that is not quite enough as not
		 * all continuation bytes are valid in the most strict
		 * interpretation of UTF-8, but we don't care about that.
		 */
		size_t i;

		for (i = 1; i < *inbytes_left; i++)
			if (((*inbuf)[i] & 0xC0) != 0x80) {
				/* Not a continuation byte */
				errno = EILSEQ;
				return (size_t) -1;
			}

		errno = EINVAL;
	} else {
		/* We had enough bytes to validate the character, and
		 * it failed.  It just doesn't look right. */
		errno = EILSEQ;
	}
	return (size_t) -1;
}

/* Open a conversion descriptor which, in addition to normal cases, provides
 * UTF-8 to UTF-8 conversions and a gunichar-compatible source and target
 * encoding. */
VteConv
_vte_conv_open(const char *target, const char *source)
{
	VteConv ret;
	GIConv conv;
	gboolean in_unichar, out_unichar, utf8;
	const char *real_target, *real_source;

	/* No shenanigans. */
	g_assert(target != NULL);
	g_assert(source != NULL);
	g_assert(strlen(target) > 0);
	g_assert(strlen(source) > 0);

	/* Assume normal iconv usage. */
	in_unichar = FALSE;
	out_unichar = FALSE;
	real_source = source;
	real_target = target;

	/* Determine if we need to convert gunichars to UTF-8 on input. */
	if (strcmp(target, VTE_CONV_GUNICHAR_TYPE) == 0) {
		real_target = "UTF-8";
		out_unichar = TRUE;
	}

	/* Determine if we need to convert UTF-8 to gunichars on output. */
	if (strcmp(source, VTE_CONV_GUNICHAR_TYPE) == 0) {
		real_source = "UTF-8";
		in_unichar = TRUE;
	}

	/* Determine if this is a UTF-8 to UTF-8 conversion. */
	utf8 = ((g_ascii_strcasecmp(real_target, "UTF-8") == 0) &&
		(g_ascii_strcasecmp(real_source, "UTF-8") == 0));

	/* If we're doing UTF-8 to UTF-8, just use a dummy function which
	 * checks for bad data. */
	conv = NULL;
	if (!utf8) {
		char *translit_target = g_strdup_printf ("%s//translit", real_target);
		conv = g_iconv_open(translit_target, real_source);
		g_free (translit_target);
		if (conv == ((GIConv) -1)) {
			conv = g_iconv_open(real_target, real_source);
		}
		if (conv == ((GIConv) -1)) {
			return VTE_INVALID_CONV;
		}
	}

	/* Set up the descriptor. */
	ret = g_slice_new0(struct _VteConv);
	if (utf8) {
		ret->conv = NULL;
		ret->convert = (convert_func) _vte_conv_utf8_utf8;
		ret->close = NULL;
	} else {
		g_assert((conv != NULL) && (conv != ((GIConv) -1)));
		ret->conv = conv;
		ret->convert = (convert_func) g_iconv;
		ret->close = g_iconv_close;
	}

	/* Initialize other elements. */
	ret->in_unichar = in_unichar;
	ret->out_unichar = out_unichar;

	/* Create scratch buffers. */
	ret->in_scratch = _vte_buffer_new();
	ret->out_scratch = _vte_buffer_new();

	return ret;
}

gint
_vte_conv_close(VteConv converter)
{
	g_assert(converter != NULL);
	g_assert(converter != VTE_INVALID_CONV);

	/* Close the underlying descriptor, if there is one. */
	if (converter->conv != NULL) {
		g_assert(converter->close != NULL);
		converter->close(converter->conv);
	}

	/* Free the scratch buffers. */
	_vte_buffer_free(converter->in_scratch);
	_vte_buffer_free(converter->out_scratch);

	/* Free the structure itself. */
	g_slice_free(struct _VteConv, converter);

	return 0;
}

size_t
_vte_conv(VteConv converter,
	  const guchar **inbuf, gsize *inbytes_left,
	  guchar **outbuf, gsize *outbytes_left)
{
	size_t ret, tmp;
	const guchar *work_inbuf_start, *work_inbuf_working;
	guchar *work_outbuf_start, *work_outbuf_working;
	gsize work_inbytes, work_outbytes;
	gsize in_converted, out_converted;

	g_assert(converter != NULL);
	g_assert(converter != VTE_INVALID_CONV);

	work_inbuf_start = work_inbuf_working = *inbuf;
	work_outbuf_start = work_outbuf_working = *outbuf;
	work_inbytes = *inbytes_left;
	work_outbytes = *outbytes_left;
	in_converted = 0;
	out_converted = 0;

	/* Possibly convert the input data from gunichars to UTF-8. */
	if (converter->in_unichar) {
		int i, char_count;
		guchar *p, *end;
		gunichar *g;
		/* Make sure the scratch buffer has enough space. */
		char_count = *inbytes_left / sizeof(gunichar);
		_vte_buffer_set_minimum_size(converter->in_scratch,
					     (char_count + 1) * VTE_UTF8_BPC);
		/* Convert the incoming text. */
		g = (gunichar*) *inbuf;
		p = converter->in_scratch->data;
		end = p + (char_count + 1) * VTE_UTF8_BPC;
		for (i = 0; i < char_count; i++) {
			p += g_unichar_to_utf8(g[i], (gchar *)p);
			g_assert(p <= end);
		}
		/* Update our working pointers. */
		work_inbuf_start = converter->in_scratch->data;
		work_inbuf_working = work_inbuf_start;
		work_inbytes = p - work_inbuf_start;
	}

	/* Possibly set the output pointers to point at our scratch buffer. */
	if (converter->out_unichar) {
		work_outbytes = *outbytes_left * VTE_UTF8_BPC;
		_vte_buffer_set_minimum_size(converter->out_scratch,
					     work_outbytes);
		work_outbuf_start = converter->out_scratch->data;
		work_outbuf_working = work_outbuf_start;
	}

	/* Call the underlying conversion. */
	ret = 0;
	do {
		tmp = converter->convert(converter->conv,
					 &work_inbuf_working,
					 &work_inbytes,
					 &work_outbuf_working,
					 &work_outbytes);
		if (tmp == (size_t) -1) {
			/* Check for zero bytes, which we pass right through. */
			if (errno == EILSEQ) {
				if ((work_inbytes > 0) &&
				    (work_inbuf_working[0] == '\0') &&
				    (work_outbytes > 0)) {
					work_outbuf_working[0] = '\0';
					work_outbuf_working++;
					work_inbuf_working++;
					work_outbytes--;
					work_inbytes--;
					ret++;
				} else {
					/* No go. */
					ret = -1;
					break;
				}
			} else {
				ret = -1;
				break;
			}
		} else {
			ret += tmp;
			break;
		}
	} while (work_inbytes > 0);

	/* We can't handle this particular failure, and it should
	 * never happen.  (If it does, our caller needs fixing.)  */
	g_assert((ret != (size_t)-1) || (errno != E2BIG));

	/* Possibly convert the output from UTF-8 to gunichars. */
	if (converter->out_unichar) {
		int  left = *outbytes_left;
		gunichar *g;
		gchar *p;

		g = (gunichar*) *outbuf;
		for(p = (gchar *)work_outbuf_start;
				p < (gchar *)work_outbuf_working;
				p = g_utf8_next_char(p)) {
		       g_assert(left>=0);
		       *g++ = g_utf8_get_char(p);
		       left -= sizeof(gunichar);
		}
		*outbytes_left = left;
		*outbuf = (guchar*) g;
	} else {
		/* Pass on the output results. */
		*outbuf = work_outbuf_working;
		*outbytes_left -= (work_outbuf_working - work_outbuf_start);
	}

	/* Advance the input pointer to the right place. */
	if (converter->in_unichar) {
		/* Get an idea of how many characters were converted, and
		 * advance the pointer as required. */
		int chars;
		chars = _vte_conv_utf8_strlen((const gchar *)work_inbuf_start,
					      work_inbuf_working - work_inbuf_start);
		*inbuf += (sizeof(gunichar) * chars);
		*inbytes_left -= (sizeof(gunichar) * chars);
	} else {
		/* Pass on the input results. */
		*inbuf = work_inbuf_working;
		*inbytes_left -= (work_inbuf_working - work_inbuf_start);
	}

	return ret;
}

size_t
_vte_conv_cu(VteConv converter,
	     const guchar **inbuf, gsize *inbytes_left,
	     gunichar **outbuf, gsize *outbytes_left)
{
	return _vte_conv(converter,
			 inbuf, inbytes_left,
			 (guchar**)outbuf, outbytes_left);
}

size_t
_vte_conv_uu(VteConv converter,
	     const gunichar **inbuf, gsize *inbytes_left,
	     gunichar **outbuf, gsize *outbytes_left)
{
	return _vte_conv(converter,
			 (const guchar**)inbuf, inbytes_left,
			 (guchar**)outbuf, outbytes_left);
}

size_t
_vte_conv_uc(VteConv converter,
	     const gunichar **inbuf, gsize *inbytes_left,
	     guchar **outbuf, gsize *outbytes_left)
{
	return _vte_conv(converter,
			 (const guchar**)inbuf, inbytes_left,
			 outbuf, outbytes_left);
}

#ifdef VTECONV_MAIN
static void
clear(gunichar wide[5], gchar narrow[5])
{
	wide[0] = 'T';
	wide[1] = 'E';
	wide[2] = 'S';
	wide[3] = 'T';
	wide[4] = '\0';
	strcpy(narrow, "test");
}

static int
mixed_strcmp(gunichar *wide, gchar *narrow)
{
	while (*wide && *narrow) {
		if (*wide != *narrow) {
			return -1;
		}
		wide++;
		narrow++;
	}
	return 0;
}

int
main(int argc, char **argv)
{
	gunichar wide_test[5];
	gchar narrow_test[5], buf[10];
	VteConv conv;
	gchar *inbuf, *outbuf;
	gsize inbytes, outbytes;
	char mbyte_test[] = {0xe2, 0x94, 0x80};
	char mbyte_test_break[] = {0xe2, 0xe2, 0xe2};
	int i;

	i = _vte_conv_utf8_strlen("\0\0\0\0", 4);
	g_assert(i == 4);
	i = _vte_conv_utf8_strlen("\0A\0\0", 4);
	g_assert(i == 4);
	i = _vte_conv_utf8_strlen("\0A\0B", 4);
	g_assert(i == 4);
	i = _vte_conv_utf8_strlen("A\0B\0", 4);
	g_assert(i == 4);
	i = _vte_conv_utf8_strlen("ABCDE", 4);
	g_assert(i == 4);

	/* Test g_iconv, no gunichar stuff. */
	clear(wide_test, narrow_test);
	memset(buf, 0, sizeof(buf));
	inbuf = narrow_test;
	inbytes = strlen(narrow_test);
	outbuf = buf;
	outbytes = sizeof(buf);
	conv = _vte_conv_open("UTF-8", "ISO-8859-1");
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if (strcmp(narrow_test, buf) != 0) {
		g_error("Conversion 1 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test g_iconv, no gunichar stuff. */
	clear(wide_test, narrow_test);
	memset(buf, 0, sizeof(buf));
	inbuf = narrow_test;
	inbytes = strlen(narrow_test);
	outbuf = buf;
	outbytes = sizeof(buf);
	conv = _vte_conv_open("ISO-8859-1", "UTF-8");
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if (strcmp(narrow_test, buf) != 0) {
		g_error("Conversion 2 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test g_iconv + gunichar stuff. */
	clear(wide_test, narrow_test);
	memset(buf, 0, sizeof(buf));
	inbuf = narrow_test;
	inbytes = strlen(narrow_test);
	outbuf = (gchar*) wide_test;
	outbytes = sizeof(wide_test);
	conv = _vte_conv_open(VTE_CONV_GUNICHAR_TYPE, "ISO-8859-1");
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if (mixed_strcmp(wide_test, narrow_test) != 0) {
		g_error("Conversion 3 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test g_iconv + gunichar stuff. */
	clear(wide_test, narrow_test);
	memset(buf, 0, sizeof(buf));
	inbuf = (gchar*) wide_test;
	inbytes = 4 * sizeof(gunichar);
	outbuf = buf;
	outbytes = sizeof(buf);
	conv = _vte_conv_open("ISO-8859-1", VTE_CONV_GUNICHAR_TYPE);
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if (mixed_strcmp(wide_test, buf) != 0) {
		g_error("Conversion 4 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test UTF-8 to UTF-8 "conversion". */
	clear(wide_test, narrow_test);
	memset(buf, 0, sizeof(buf));
	inbuf = (gchar*) narrow_test;
	inbytes = strlen(narrow_test);
	outbuf = buf;
	outbytes = sizeof(buf);
	conv = _vte_conv_open("UTF-8", "UTF-8");
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if (strcmp(narrow_test, buf) != 0) {
		g_error("Conversion 5 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test zero-byte pass-through. */
	clear(wide_test, narrow_test);
	memset(wide_test, 0, sizeof(wide_test));
	inbuf = (gchar*) wide_test;
	inbytes = 3 * sizeof(gunichar);
	outbuf = narrow_test;
	outbytes = sizeof(narrow_test);
	conv = _vte_conv_open("UTF-8", VTE_CONV_GUNICHAR_TYPE);
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if ((narrow_test[0] != 0) ||
	    (narrow_test[1] != 0) ||
	    (narrow_test[2] != 0)) {
		g_error("Conversion 6 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test zero-byte pass-through. */
	clear(wide_test, narrow_test);
	memset(wide_test, 'A', sizeof(wide_test));
	memset(narrow_test, 0, sizeof(narrow_test));
	inbuf = narrow_test;
	inbytes = 3;
	outbuf = (char*)wide_test;
	outbytes = sizeof(wide_test);
	conv = _vte_conv_open(VTE_CONV_GUNICHAR_TYPE, "UTF-8");
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if ((wide_test[0] != 0) ||
	    (wide_test[1] != 0) ||
	    (wide_test[2] != 0)) {
		g_error("Conversion 7 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test zero-byte pass-through. */
	clear(wide_test, narrow_test);
	memset(wide_test, 'A', sizeof(wide_test));
	memset(narrow_test, 0, sizeof(narrow_test));
	inbuf = narrow_test;
	inbytes = 3;
	outbuf = (char*)wide_test;
	outbytes = sizeof(wide_test);
	conv = _vte_conv_open(VTE_CONV_GUNICHAR_TYPE, "ISO-8859-1");
	i = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
	g_assert(inbytes == 0);
	if ((wide_test[0] != 0) ||
	    (wide_test[1] != 0) ||
	    (wide_test[2] != 0)) {
		g_error("Conversion 8 failed.\n");
	}
	_vte_conv_close(conv);

	/* Test UTF-8 to UTF-8 error reporting, valid multibyte. */
	for (i = 0; i < sizeof(mbyte_test); i++) {
		int ret;
		inbuf = mbyte_test;
		inbytes = i + 1;
		outbuf = buf;
		outbytes = sizeof(buf);
		conv = _vte_conv_open("UTF-8", "UTF-8");
		ret = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
		switch (i) {
		case 0:
			g_assert((ret == -1) && (errno == EINVAL));
			break;
		case 1:
			g_assert((ret == -1) && (errno == EINVAL));
			break;
		case 2:
			g_assert(ret != -1);
			break;
		default:
			g_assert_not_reached();
			break;
		}
		_vte_conv_close(conv);
	}

	/* Test UTF-8 to UTF-8 error reporting, invalid multibyte. */
	for (i = 0; i < sizeof(mbyte_test_break); i++) {
		int ret;
		inbuf = mbyte_test_break;
		inbytes = i + 1;
		outbuf = buf;
		outbytes = sizeof(buf);
		conv = _vte_conv_open("UTF-8", "UTF-8");
		ret = _vte_conv(conv, &inbuf, &inbytes, &outbuf, &outbytes);
		_vte_conv_close(conv);
		switch (i) {
		case 0:
			g_assert((ret == -1) && (errno == EINVAL));
			break;
		case 1:
			g_assert((ret == -1) && (errno == EINVAL));
			break;
		case 2:
			g_assert((ret == -1) && (errno == EILSEQ));
			break;
		default:
			g_assert_not_reached();
			break;
		}
	}

	return 0;
}
#endif

static void
display_control_sequence(const char *name, GValueArray *params)
{
#ifdef VTE_DEBUG
	guint i;
	long l;
	const char *s;
	const gunichar *w;
	GValue *value;
	g_printerr("%s(", name);
	if (params != NULL) {
		for (i = 0; i < params->n_values; i++) {
			value = g_value_array_get_nth(params, i);
			if (i > 0) {
				g_printerr(", ");
			}
			if (G_VALUE_HOLDS_LONG(value)) {
				l = g_value_get_long(value);
				g_printerr("%ld", l);
			} else
			if (G_VALUE_HOLDS_STRING(value)) {
				s = g_value_get_string(value);
				g_printerr("\"%s\"", s);
			} else
			if (G_VALUE_HOLDS_POINTER(value)) {
				w = g_value_get_pointer(value);
				g_printerr("\"%ls\"", (const wchar_t*) w);
			}
		}
	}
	g_printerr(")\n");
#endif
}


/* A couple are duplicated from vte.c, to keep them static... */

/* Find the character an the given position in the backscroll buffer. */
static VteCell *
vte_terminal_find_charcell (VteTerminal *terminal, glong col, glong row)
{
	VteRowData *rowdata;
	VteCell *ret = NULL;
	VteScreen *screen;
	g_assert(VTE_IS_TERMINAL(terminal));
	screen = terminal->pvt->screen;
	if (_vte_ring_contains (screen->row_data, row)) {
		rowdata = _vte_ring_index_writable (screen->row_data, row);
		ret = _vte_row_data_get_writable (rowdata, col);
	}
	return ret;
}
/* Find the row in the given position in the backscroll buffer. */
static inline const VteRowData *
_vte_terminal_find_row_data (VteTerminal *terminal, glong row)
{
	const VteRowData *rowdata = NULL;
	VteScreen *screen = terminal->pvt->screen;
	if (G_LIKELY (_vte_ring_contains (screen->row_data, row))) {
		rowdata = _vte_ring_index (screen->row_data, row);
	}
	return rowdata;
}
/* Find the row in the given position in the backscroll buffer. */
static inline VteRowData *
_vte_terminal_find_row_data_writable (VteTerminal *terminal, glong row)
{
	VteRowData *rowdata = NULL;
	VteScreen *screen = terminal->pvt->screen;
	if (G_LIKELY (_vte_ring_contains (screen->row_data, row))) {
		rowdata = _vte_ring_index_writable (screen->row_data, row);
	}
	return rowdata;
}

static glong
find_start_column (VteTerminal *terminal, glong col, glong row)
{
	const VteRowData *row_data = _vte_terminal_find_row_data (terminal, row);
	if (G_UNLIKELY (col < 0))
		return col;
	if (row_data != NULL) {
		const VteCell *cell = _vte_row_data_get (row_data, col);
		while (col > 0 && cell != NULL && cell->attr.fragment) {
			cell = _vte_row_data_get (row_data, --col);
		}
	}
	return MAX(col, 0);
}
static glong
find_end_column (VteTerminal *terminal, glong col, glong row)
{
	const VteRowData *row_data = _vte_terminal_find_row_data (terminal, row);
	gint columns = 0;
	if (G_UNLIKELY (col < 0))
		return col;
	if (row_data != NULL) {
		const VteCell *cell = _vte_row_data_get (row_data, col);
		while (col > 0 && cell != NULL && cell->attr.fragment) {
			cell = _vte_row_data_get (row_data, --col);
		}
		if (cell) {
			columns = cell->attr.columns - 1;
		}
	}
	return MIN(col + columns, terminal->column_count);
}

/* Determine the width of the portion of the preedit string which lies
 * to the left of the cursor, or the entire string, in columns. */
static gssize
vte_terminal_preedit_width(VteTerminal *terminal, gboolean left_only)
{
	gunichar c;
	int i;
	gssize ret = 0;
	const char *preedit = NULL;

	if (terminal->pvt->im_preedit != NULL) {
		preedit = terminal->pvt->im_preedit;
		for (i = 0;
		     (preedit != NULL) &&
		     (preedit[0] != '\0') &&
		     (!left_only || (i < terminal->pvt->im_preedit_cursor));
		     i++) {
			c = g_utf8_get_char(preedit);
			ret += _vte_iso2022_unichar_width(terminal->pvt->iso2022, c);
			preedit = g_utf8_next_char(preedit);
		}
	}

	return ret;
}

/* Determine the length of the portion of the preedit string which lies
 * to the left of the cursor, or the entire string, in gunichars. */
static gssize
vte_terminal_preedit_length(VteTerminal *terminal, gboolean left_only)
{
	int i = 0;
	const char *preedit = NULL;

	if (terminal->pvt->im_preedit != NULL) {
		preedit = terminal->pvt->im_preedit;
		for (i = 0;
		     (preedit != NULL) &&
		     (preedit[0] != '\0') &&
		     (!left_only || (i < terminal->pvt->im_preedit_cursor));
		     i++) {
			preedit = g_utf8_next_char(preedit);
		}
	}

	return i;
}

/* Cause the cell to be redrawn. */
void
_vte_invalidate_cell(VteTerminal *terminal, glong col, glong row)
{
	const VteRowData *row_data;
	int columns;

	if (G_UNLIKELY (! gtk_widget_is_drawable (&terminal->widget)
				|| terminal->pvt->invalidated_all)) {
		return;
	}

	columns = 1;
	row_data = _vte_terminal_find_row_data(terminal, row);
	if (row_data != NULL) {
		const VteCell *cell;
		cell = _vte_row_data_get (row_data, col);
		if (cell != NULL) {
			while (cell->attr.fragment && col> 0) {
				cell = _vte_row_data_get (row_data, --col);
			}
			columns = cell->attr.columns;
			if (cell->c != 0 &&
					_vte_draw_get_char_width (
						terminal->pvt->draw,
						cell->c,
						columns, cell->attr.bold) >
					terminal->char_width * columns) {
				columns++;
			}
		}
	}

	_vte_debug_print(VTE_DEBUG_UPDATES,
			"Invalidating cell at (%ld,%ld-%ld).\n",
			row, col, col + columns);
	_vte_invalidate_cells(terminal,
			col, columns,
			row, 1);
}

/* Cause the cursor to be redrawn. */
void
_vte_invalidate_cursor_once(VteTerminal *terminal, gboolean periodic)
{
	VteScreen *screen;
	const VteCell *cell;
	gssize preedit_width;
	glong column, row;
	gint columns;

	if (terminal->pvt->invalidated_all) {
		return;
	}

	if (periodic) {
		if (!terminal->pvt->cursor_blinks) {
			return;
		}
	}

	if (terminal->pvt->cursor_visible && gtk_widget_is_drawable (&terminal->widget)) {
		preedit_width = vte_terminal_preedit_width(terminal, FALSE);

		screen = terminal->pvt->screen;
		row = screen->cursor_current.row;
		column = screen->cursor_current.col;
		columns = 1;
		column = find_start_column (terminal, column, row);
		cell = vte_terminal_find_charcell(terminal, column, row);
		if (cell != NULL) {
			columns = cell->attr.columns;
			if (cell->c != 0 &&
					_vte_draw_get_char_width (
						terminal->pvt->draw,
						cell->c,
						columns, cell->attr.bold) >
			    terminal->char_width * columns) {
				columns++;
			}
		}
		if (preedit_width > 0) {
			columns += preedit_width;
			columns++; /* one more for the preedit cursor */
		}

		_vte_debug_print(VTE_DEBUG_UPDATES,
				"Invalidating cursor at (%ld,%ld-%ld).\n",
				row, column, column + columns);
		_vte_invalidate_cells(terminal,
				     column, columns,
				     row, 1);
	}
}

/* Invalidate the cursor repeatedly. */
static gboolean
vte_invalidate_cursor_periodic (VteTerminal *terminal)
{
        VteTerminalPrivate *pvt = terminal->pvt;

	pvt->cursor_blink_state = !pvt->cursor_blink_state;
	pvt->cursor_blink_time += pvt->cursor_blink_cycle;

	_vte_invalidate_cursor_once(terminal, TRUE);

	/* only disable the blink if the cursor is currently shown.
	 * else, wait until next time.
	 */
	if (pvt->cursor_blink_time / 1000 >= pvt->cursor_blink_timeout &&
	    pvt->cursor_blink_state) {
                pvt->cursor_blink_tag = 0;
		return FALSE;
        }

	pvt->cursor_blink_tag = g_timeout_add_full(G_PRIORITY_LOW,
						   terminal->pvt->cursor_blink_cycle,
						   (GSourceFunc)vte_invalidate_cursor_periodic,
						   terminal,
						   NULL);
	return FALSE;
}

/* Check how long a string of unichars is.  Slow version. */
static gssize
vte_unichar_strlen(gunichar *c)
{
	int i;
	for (i = 0; c[i] != 0; i++) ;
	return i;
}

/* Convert a wide character string to a multibyte string */
static gchar *
vte_ucs4_to_utf8 (VteTerminal *terminal, const guchar *in)
{
	gchar *out = NULL;
	guchar *buf = NULL, *bufptr = NULL;
	gsize inlen, outlen;
	VteConv conv;

	conv = _vte_conv_open ("UTF-8", VTE_CONV_GUNICHAR_TYPE);

	if (conv != VTE_INVALID_CONV) {
		inlen = vte_unichar_strlen ((gunichar *) in) * sizeof (gunichar);
		outlen = (inlen * VTE_UTF8_BPC) + 1;

		_vte_buffer_set_minimum_size (terminal->pvt->conv_buffer, outlen);
		buf = bufptr = terminal->pvt->conv_buffer->data;

		if (_vte_conv (conv, &in, &inlen, &buf, &outlen) == (size_t) -1) {
			_vte_debug_print (VTE_DEBUG_IO,
					  "Error converting %ld string bytes (%s), skipping.\n",
					  (long) _vte_buffer_length (terminal->pvt->outgoing),
					  g_strerror (errno));
			bufptr = NULL;
		} else {
			out = g_strndup ((gchar *) bufptr, buf - bufptr);
		}
	}

	_vte_conv_close (conv);

	return out;
}

static gboolean
vte_parse_color (const char *spec, GdkColor *color)
{
	gchar *spec_copy = (gchar *) spec;
	gboolean retval = FALSE;

	/* gdk_color_parse doesnt handle all XParseColor formats.  It only
	 * supports the #RRRGGGBBB format, not the rgb:RRR/GGG/BBB format.
	 * See: man XParseColor */

	if (g_ascii_strncasecmp (spec_copy, "rgb:", 4) == 0) {
		gchar *cur, *ptr;

		spec_copy = g_strdup (spec);
		cur = spec_copy;
		ptr = spec_copy + 3;

		*cur++ = '#';
		while (*ptr++)
			if (*ptr != '/')
				*cur++ = *ptr;
		*cur++ = '\0';
	}

	retval = gdk_color_parse (spec_copy, color);

	if (spec_copy != spec)
		g_free (spec_copy);

	return retval;
}






/* Emit a "deiconify-window" signal. */
static void
vte_terminal_emit_deiconify_window(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `deiconify-window'.\n");
	g_signal_emit_by_name(terminal, "deiconify-window");
}

/* Emit a "iconify-window" signal. */
static void
vte_terminal_emit_iconify_window(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `iconify-window'.\n");
	g_signal_emit_by_name(terminal, "iconify-window");
}

/* Emit a "raise-window" signal. */
static void
vte_terminal_emit_raise_window(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `raise-window'.\n");
	g_signal_emit_by_name(terminal, "raise-window");
}

/* Emit a "lower-window" signal. */
static void
vte_terminal_emit_lower_window(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `lower-window'.\n");
	g_signal_emit_by_name(terminal, "lower-window");
}

/* Emit a "maximize-window" signal. */
static void
vte_terminal_emit_maximize_window(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `maximize-window'.\n");
	g_signal_emit_by_name(terminal, "maximize-window");
}

/* Emit a "refresh-window" signal. */
static void
vte_terminal_emit_refresh_window(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `refresh-window'.\n");
	g_signal_emit_by_name(terminal, "refresh-window");
}

/* Emit a "restore-window" signal. */
static void
vte_terminal_emit_restore_window(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `restore-window'.\n");
	g_signal_emit_by_name(terminal, "restore-window");
}

/* Emit a "move-window" signal.  (Pixels.) */
static void
vte_terminal_emit_move_window(VteTerminal *terminal, guint x, guint y)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `move-window'.\n");
	g_signal_emit_by_name(terminal, "move-window", x, y);
}

/* Emit a "resize-window" signal.  (Pixels.) */
static void
vte_terminal_emit_resize_window(VteTerminal *terminal,
				guint width, guint height)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `resize-window'.\n");
	g_signal_emit_by_name(terminal, "resize-window", width, height);
}


/* Some common functions */

static void
_vte_terminal_home_cursor (VteTerminal *terminal)
{
	VteScreen *screen;
	screen = terminal->pvt->screen;
	screen->cursor_current.row = screen->insert_delta;
	screen->cursor_current.col = 0;
}

/* Clear the entire screen. */
static void
_vte_terminal_clear_screen (VteTerminal *terminal)
{
	long i, initial, row;
	VteScreen *screen;
	screen = terminal->pvt->screen;
	initial = screen->insert_delta;
	row = screen->cursor_current.row - screen->insert_delta;
	initial = _vte_ring_next(screen->row_data);
	/* Add a new screen's worth of rows. */
	for (i = 0; i < terminal->row_count; i++)
		_vte_terminal_ring_append (terminal, TRUE);
	/* Move the cursor and insertion delta to the first line in the
	 * newly-cleared area and scroll if need be. */
	screen->insert_delta = initial;
	screen->cursor_current.row = row + screen->insert_delta;
	_vte_terminal_adjust_adjustments(terminal);
	/* Redraw everything. */
	_vte_invalidate_all(terminal);
	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Clear the current line. */
static void
_vte_terminal_clear_current_line (VteTerminal *terminal)
{
	VteRowData *rowdata;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	/* If the cursor is actually on the screen, clear data in the row
	 * which corresponds to the cursor. */
	if (_vte_ring_next(screen->row_data) > screen->cursor_current.row) {
		/* Get the data for the row which the cursor points to. */
		rowdata = _vte_ring_index_writable (screen->row_data, screen->cursor_current.row);
		g_assert(rowdata != NULL);
		/* Remove it. */
		_vte_row_data_shrink (rowdata, 0);
		/* Add enough cells to the end of the line to fill out the row. */
		_vte_row_data_fill (rowdata, &screen->fill_defaults, terminal->column_count);
		rowdata->attr.soft_wrapped = 0;
		/* Repaint this row. */
		_vte_invalidate_cells(terminal,
				      0, terminal->column_count,
				      screen->cursor_current.row, 1);
	}

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Clear above the current line. */
static void
_vte_terminal_clear_above_current (VteTerminal *terminal)
{
	VteRowData *rowdata;
	long i;
	VteScreen *screen;
	screen = terminal->pvt->screen;
	/* If the cursor is actually on the screen, clear data in the row
	 * which corresponds to the cursor. */
	for (i = screen->insert_delta; i < screen->cursor_current.row; i++) {
		if (_vte_ring_next(screen->row_data) > i) {
			/* Get the data for the row we're erasing. */
			rowdata = _vte_ring_index_writable (screen->row_data, i);
			g_assert(rowdata != NULL);
			/* Remove it. */
			_vte_row_data_shrink (rowdata, 0);
			/* Add new cells until we fill the row. */
			_vte_row_data_fill (rowdata, &screen->fill_defaults, terminal->column_count);
			rowdata->attr.soft_wrapped = 0;
			/* Repaint the row. */
			_vte_invalidate_cells(terminal,
					0, terminal->column_count, i, 1);
		}
	}
	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Scroll the text, but don't move the cursor.  Negative = up, positive = down. */
static void
_vte_terminal_scroll_text (VteTerminal *terminal, int scroll_amount)
{
	long start, end, i;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	if (screen->scrolling_restricted) {
		start = screen->insert_delta + screen->scrolling_region.start;
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		start = screen->insert_delta;
		end = start + terminal->row_count - 1;
	}

	while (_vte_ring_next(screen->row_data) <= end)
		_vte_terminal_ring_append (terminal, FALSE);

	if (scroll_amount > 0) {
		for (i = 0; i < scroll_amount; i++) {
			_vte_terminal_ring_remove (terminal, end);
			_vte_terminal_ring_insert (terminal, start, TRUE);
		}
	} else {
		for (i = 0; i < -scroll_amount; i++) {
			_vte_terminal_ring_remove (terminal, start);
			_vte_terminal_ring_insert (terminal, end, TRUE);
		}
	}

	/* Update the display. */
	_vte_terminal_scroll_region(terminal, start, end - start + 1,
				   scroll_amount);

	/* Adjust the scrollbars if necessary. */
	_vte_terminal_adjust_adjustments(terminal);

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_inserted_flag = TRUE;
	terminal->pvt->text_deleted_flag = TRUE;
}

static gboolean
vte_terminal_termcap_string_same_as_for (VteTerminal *terminal,
					 const char  *cap_str,
					 const char  *cap_other)
{
	char *other_str;
	gboolean ret;

	other_str = _vte_termcap_find_string(terminal->pvt->termcap,
					     terminal->pvt->emulation,
					     cap_other);

	ret = other_str && (g_ascii_strcasecmp(cap_str, other_str) == 0);

	g_free (other_str);

	return ret;
}

/* Set icon/window titles. */
static void
vte_sequence_handler_set_title_internal(VteTerminal *terminal,
					GValueArray *params,
					gboolean icon_title,
					gboolean window_title)
{
	GValue *value;
	char *title = NULL;

	if (icon_title == FALSE && window_title == FALSE)
		return;

	/* Get the string parameter's value. */
	value = g_value_array_get_nth(params, 0);
	if (value) {
		if (G_VALUE_HOLDS_LONG(value)) {
			/* Convert the long to a string. */
			title = g_strdup_printf("%ld", g_value_get_long(value));
		} else
		if (G_VALUE_HOLDS_STRING(value)) {
			/* Copy the string into the buffer. */
			title = g_value_dup_string(value);
		} else
		if (G_VALUE_HOLDS_POINTER(value)) {
			title = vte_ucs4_to_utf8 (terminal, g_value_get_pointer (value));
		}
		if (title != NULL) {
			char *p, *validated;
			const char *end;

			/* Validate the text. */
			g_utf8_validate(title, strlen(title), &end);
			validated = g_strndup(title, end - title);

			/* No control characters allowed. */
			for (p = validated; *p != '\0'; p++) {
				if ((*p & 0x1f) == *p) {
					*p = ' ';
				}
			}

			/* Emit the signal */
			if (window_title) {
				g_free (terminal->pvt->window_title_changed);
				terminal->pvt->window_title_changed = g_strdup (validated);
			}

			if (icon_title) {
				g_free (terminal->pvt->icon_title_changed);
				terminal->pvt->icon_title_changed = g_strdup (validated);
			}

			g_free (validated);
			g_free(title);
		}
	}
}

/* Toggle a terminal mode. */
static void
vte_sequence_handler_set_mode_internal(VteTerminal *terminal,
				       long setting, gboolean value)
{
	switch (setting) {
	case 2:		/* keyboard action mode (?) */
		break;
	case 4:		/* insert/overtype mode */
		terminal->pvt->screen->insert_mode = value;
		break;
	case 12:	/* send/receive mode (local echo) */
		terminal->pvt->screen->sendrecv_mode = value;
		break;
	case 20:	/* automatic newline / normal linefeed mode */
		terminal->pvt->screen->linefeed_mode = value;
		break;
	default:
		break;
	}
}


/*
 * Sequence handling boilerplate
 */

/* Typedef the handle type */
typedef void (*VteTerminalSequenceHandler) (VteTerminal *terminal, GValueArray *params);

/* Prototype all handlers... */
#define VTE_SEQUENCE_HANDLER(name) \
	static void name (VteTerminal *terminal, GValueArray *params);
#include "vteseq-list.h"
#undef VTE_SEQUENCE_HANDLER


/* Call another handler, offsetting any long arguments by the given
 * increment value. */
static void
vte_sequence_handler_offset(VteTerminal *terminal,
			    GValueArray *params,
			    int increment,
			    VteTerminalSequenceHandler handler)
{
	guint i;
	long val;
	GValue *value;
	/* Decrement the parameters and let the _cs handler deal with it. */
	for (i = 0; (params != NULL) && (i < params->n_values); i++) {
		value = g_value_array_get_nth(params, i);
		if (G_VALUE_HOLDS_LONG(value)) {
			val = g_value_get_long(value);
			val += increment;
			g_value_set_long(value, val);
		}
	}
	handler (terminal, params);
}

/* Call another function a given number of times, or once. */
static void
vte_sequence_handler_multiple_limited(VteTerminal *terminal,
                                      GValueArray *params,
                                      VteTerminalSequenceHandler handler,
                                      glong max)
{
	long val = 1;
	int i;
	GValue *value;

	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			val = g_value_get_long(value);
			val = CLAMP(val, 1, max);	/* FIXME: vttest. */
		}
	}
	for (i = 0; i < val; i++)
		handler (terminal, NULL);
}

static void
vte_sequence_handler_multiple(VteTerminal *terminal,
                              GValueArray *params,
                              VteTerminalSequenceHandler handler)
{
        vte_sequence_handler_multiple_limited(terminal, params, handler, G_MAXLONG);
}

static void
vte_sequence_handler_multiple_r(VteTerminal *terminal,
                                GValueArray *params,
                                VteTerminalSequenceHandler handler)
{
        vte_sequence_handler_multiple_limited(terminal, params, handler,
                                              terminal->column_count - terminal->pvt->screen->cursor_current.col);
}

/* Manipulate certain terminal attributes. */
static void
vte_sequence_handler_decset_internal(VteTerminal *terminal,
				     int setting,
				     gboolean restore,
				     gboolean save,
				     gboolean set)
{
	gboolean recognized = FALSE;
	gpointer p;
	guint i;
	struct {
		int setting;
		gboolean *bvalue;
		gint *ivalue;
		gpointer *pvalue;
		gpointer fvalue;
		gpointer tvalue;
		VteTerminalSequenceHandler reset, set;
	} settings[] = {
		/* 1: Application/normal cursor keys. */
		{1, NULL, &terminal->pvt->cursor_mode, NULL,
		 GINT_TO_POINTER(VTE_KEYMODE_NORMAL),
		 GINT_TO_POINTER(VTE_KEYMODE_APPLICATION),
		 NULL, NULL,},
		/* 2: disallowed, we don't do VT52. */
		{2, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 3: disallowed, window size is set by user. */
		{3, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 4: Smooth scroll. */
		{4, &terminal->pvt->smooth_scroll, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 5: Reverse video. */
		{5, &terminal->pvt->screen->reverse_mode, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 6: Origin mode: when enabled, cursor positioning is
		 * relative to the scrolling region. */
		{6, &terminal->pvt->screen->origin_mode, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 7: Wraparound mode. */
		{7, &terminal->pvt->flags.am, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 8: disallowed, keyboard repeat is set by user. */
		{8, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 9: Send-coords-on-click. */
		{9, NULL, &terminal->pvt->mouse_tracking_mode, NULL,
		 GINT_TO_POINTER(0),
		 GINT_TO_POINTER(MOUSE_TRACKING_SEND_XY_ON_CLICK),
		 NULL, NULL,},
		/* 12: disallowed, cursor blinks is set by user. */
		{12, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 18: print form feed. */
		/* 19: set print extent to full screen. */
		/* 25: Cursor visible. */
		{25, &terminal->pvt->cursor_visible, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 30/rxvt: disallowed, scrollbar visibility is set by user. */
		{30, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 35/rxvt: disallowed, fonts set by user. */
		{35, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 38: enter Tektronix mode. */
		/* 40: disallowed, the user sizes dynamically. */
		{40, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 41: more(1) fix. */
		/* 42: Enable NLS replacements. */
		{42, &terminal->pvt->nrc_mode, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 44: Margin bell. */
		{44, &terminal->pvt->margin_bell, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 47: Alternate screen. */
		{47, NULL, NULL, (gpointer) &terminal->pvt->screen,
		 &terminal->pvt->normal_screen,
		 &terminal->pvt->alternate_screen,
		 NULL, NULL,},
		/* 66: Keypad mode. */
		{66, &terminal->pvt->keypad_mode, NULL, NULL,
		 GINT_TO_POINTER(VTE_KEYMODE_NORMAL),
		 GINT_TO_POINTER(VTE_KEYMODE_APPLICATION),
		 NULL, NULL,},
		/* 67: disallowed, backspace key policy is set by user. */
		{67, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 1000: Send-coords-on-button. */
		{1000, NULL, &terminal->pvt->mouse_tracking_mode, NULL,
		 GINT_TO_POINTER(0),
		 GINT_TO_POINTER(MOUSE_TRACKING_SEND_XY_ON_BUTTON),
		 NULL, NULL,},
		/* 1001: Hilite tracking. */
		{1001, NULL, &terminal->pvt->mouse_tracking_mode, NULL,
		 GINT_TO_POINTER(0),
		 GINT_TO_POINTER(MOUSE_TRACKING_HILITE_TRACKING),
		 NULL, NULL,},
		/* 1002: Cell motion tracking. */
		{1002, NULL, &terminal->pvt->mouse_tracking_mode, NULL,
		 GINT_TO_POINTER(0),
		 GINT_TO_POINTER(MOUSE_TRACKING_CELL_MOTION_TRACKING),
		 NULL, NULL,},
		/* 1003: All motion tracking. */
		{1003, NULL, &terminal->pvt->mouse_tracking_mode, NULL,
		 GINT_TO_POINTER(0),
		 GINT_TO_POINTER(MOUSE_TRACKING_ALL_MOTION_TRACKING),
		 NULL, NULL,},
		/* 1010/rxvt: disallowed, scroll-on-output is set by user. */
		{1010, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 1011/rxvt: disallowed, scroll-on-keypress is set by user. */
		{1011, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 1035: disallowed, don't know what to do with it. */
		{1035, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 1036: Meta-sends-escape. */
		{1036, &terminal->pvt->meta_sends_escape, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
		/* 1037: disallowed, delete key policy is set by user. */
		{1037, NULL, NULL, NULL, NULL, NULL, NULL, NULL,},
		/* 1047: Use alternate screen buffer. */
		{1047, NULL, NULL, (gpointer) &terminal->pvt->screen,
		 &terminal->pvt->normal_screen,
		 &terminal->pvt->alternate_screen,
		 NULL, NULL,},
		/* 1048: Save/restore cursor position. */
		{1048, NULL, NULL, NULL,
		 NULL,
		 NULL,
		 vte_sequence_handler_rc,
		 vte_sequence_handler_sc,},
		/* 1049: Use alternate screen buffer, saving the cursor
		 * position. */
		{1049, NULL, NULL, (gpointer) &terminal->pvt->screen,
		 &terminal->pvt->normal_screen,
		 &terminal->pvt->alternate_screen,
		 vte_sequence_handler_rc,
		 vte_sequence_handler_sc,},
		/* 1051: Sun function key mode. */
		{1051, NULL, NULL, (gpointer) &terminal->pvt->sun_fkey_mode,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL},
		/* 1052: HP function key mode. */
		{1052, NULL, NULL, (gpointer) &terminal->pvt->hp_fkey_mode,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL},
		/* 1060: Legacy function key mode. */
		{1060, NULL, NULL, (gpointer) &terminal->pvt->legacy_fkey_mode,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL},
		/* 1061: VT220 function key mode. */
		{1061, NULL, NULL, (gpointer) &terminal->pvt->vt220_fkey_mode,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL},
		/* 2004: Bracketed paste mode. */
		{2004, &terminal->pvt->screen->bracketed_paste_mode, NULL, NULL,
		 GINT_TO_POINTER(FALSE),
		 GINT_TO_POINTER(TRUE),
		 NULL, NULL,},
	};

	/* Handle the setting. */
	for (i = 0; i < G_N_ELEMENTS(settings); i++)
	if (settings[i].setting == setting) {
		recognized = TRUE;
		/* Handle settings we want to ignore. */
		if ((settings[i].fvalue == settings[i].tvalue) &&
		    (settings[i].set == NULL) &&
		    (settings[i].reset == NULL)) {
			continue;
		}

		/* Read the old setting. */
		if (restore) {
			p = g_hash_table_lookup(terminal->pvt->dec_saved,
						GINT_TO_POINTER(setting));
			set = (p != NULL);
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Setting %d was %s.\n",
					setting, set ? "set" : "unset");
		}
		/* Save the current setting. */
		if (save) {
			if (settings[i].bvalue) {
				set = *(settings[i].bvalue) != FALSE;
			} else
			if (settings[i].ivalue) {
				set = *(settings[i].ivalue) ==
				      GPOINTER_TO_INT(settings[i].tvalue);
			} else
			if (settings[i].pvalue) {
				set = *(settings[i].pvalue) ==
				      settings[i].tvalue;
			}
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Setting %d is %s, saving.\n",
					setting, set ? "set" : "unset");
			g_hash_table_insert(terminal->pvt->dec_saved,
					    GINT_TO_POINTER(setting),
					    GINT_TO_POINTER(set));
		}
		/* Change the current setting to match the new/saved value. */
		if (!save) {
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Setting %d to %s.\n",
					setting, set ? "set" : "unset");
			if (settings[i].set && set) {
				settings[i].set (terminal, NULL);
			}
			if (settings[i].bvalue) {
				*(settings[i].bvalue) = set;
			} else
			if (settings[i].ivalue) {
				*(settings[i].ivalue) = set ?
					GPOINTER_TO_INT(settings[i].tvalue) :
					GPOINTER_TO_INT(settings[i].fvalue);
			} else
			if (settings[i].pvalue) {
				*(settings[i].pvalue) = set ?
					settings[i].tvalue :
					settings[i].fvalue;
			}
			if (settings[i].reset && !set) {
				settings[i].reset (terminal, NULL);
			}
		}
	}

	/* Do whatever's necessary when the setting changes. */
	switch (setting) {
	case 1:
		_vte_debug_print(VTE_DEBUG_KEYBOARD, set ?
				"Entering application cursor mode.\n" :
				"Leaving application cursor mode.\n");
		break;
#if 0		/* 3: disallowed, window size is set by user. */
	case 3:
		vte_terminal_emit_resize_window(terminal,
						(set ? 132 : 80) *
						terminal->char_width +
						terminal->pvt->inner_border.left +
                                                terminal->pvt->inner_border.right,
						terminal->row_count *
						terminal->char_height +
						terminal->pvt->inner_border.top +
                                                terminal->pvt->inner_border.bottom);
		/* Request a resize and redraw. */
		_vte_invalidate_all(terminal);
		break;
#endif
	case 5:
		/* Repaint everything in reverse mode. */
		_vte_invalidate_all(terminal);
		break;
	case 6:
		/* Reposition the cursor in its new home position. */
		terminal->pvt->screen->cursor_current.col = 0;
		terminal->pvt->screen->cursor_current.row =
			terminal->pvt->screen->insert_delta;
		break;
	case 47:
	case 1047:
	case 1049:
		/* Clear the alternate screen if we're switching
		 * to it, and home the cursor. */
		if (set) {
			_vte_terminal_clear_screen (terminal);
			_vte_terminal_home_cursor (terminal);
		}
		/* Reset scrollbars and repaint everything. */
		gtk_adjustment_set_value(terminal->adjustment,
					 terminal->pvt->screen->scroll_delta);
		vte_terminal_set_scrollback_lines(terminal,
				terminal->pvt->scrollback_lines);
		_vte_terminal_queue_contents_changed(terminal);
		_vte_invalidate_all (terminal);
		break;
	case 9:
	case 1000:
	case 1001:
	case 1002:
	case 1003:
		/* Make the pointer visible. */
		_vte_terminal_set_pointer_visible(terminal, TRUE);
		break;
	case 66:
		_vte_debug_print(VTE_DEBUG_KEYBOARD, set ?
				"Entering application keypad mode.\n" :
				"Leaving application keypad mode.\n");
		break;
	case 1051:
		_vte_debug_print(VTE_DEBUG_KEYBOARD, set ?
				"Entering Sun fkey mode.\n" :
				"Leaving Sun fkey mode.\n");
		break;
	case 1052:
		_vte_debug_print(VTE_DEBUG_KEYBOARD, set ?
				"Entering HP fkey mode.\n" :
				"Leaving HP fkey mode.\n");
		break;
	case 1060:
		_vte_debug_print(VTE_DEBUG_KEYBOARD, set ?
				"Entering Legacy fkey mode.\n" :
				"Leaving Legacy fkey mode.\n");
		break;
	case 1061:
		_vte_debug_print(VTE_DEBUG_KEYBOARD, set ?
				"Entering VT220 fkey mode.\n" :
				"Leaving VT220 fkey mode.\n");
		break;
	default:
		break;
	}

	if (!recognized) {
		_vte_debug_print (VTE_DEBUG_MISC,
				  "DECSET/DECRESET mode %d not recognized, ignoring.\n",
				  setting);
	}
}




/* THE HANDLERS */


/* End alternate character set. */
static void
vte_sequence_handler_ae (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->alternate_charset = FALSE;
}

/* Add a line at the current cursor position. */
static void
vte_sequence_handler_al (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	long start, end, param, i;
	GValue *value;

	/* Find out which part of the screen we're messing with. */
	screen = terminal->pvt->screen;
	start = screen->cursor_current.row;
	if (screen->scrolling_restricted) {
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		end = screen->insert_delta + terminal->row_count - 1;
	}

	/* Extract any parameters. */
	param = 1;
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			param = g_value_get_long(value);
		}
	}

	/* Insert the right number of lines. */
	for (i = 0; i < param; i++) {
		/* Clear a line off the end of the region and add one to the
		 * top of the region. */
		_vte_terminal_ring_remove (terminal, end);
		_vte_terminal_ring_insert (terminal, start, TRUE);
		/* Adjust the scrollbars if necessary. */
		_vte_terminal_adjust_adjustments(terminal);
	}

	/* Update the display. */
	_vte_terminal_scroll_region(terminal, start, end - start + 1, param);

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Add N lines at the current cursor position. */
static void
vte_sequence_handler_AL (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_al (terminal, params);
}

/* Start using alternate character set. */
static void
vte_sequence_handler_as (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->alternate_charset = TRUE;
}

/* Beep. */
static void
vte_sequence_handler_bl (VteTerminal *terminal, GValueArray *params)
{
	_vte_terminal_beep (terminal);
	g_signal_emit_by_name(terminal, "beep");
}

/* Backtab. */
static void
vte_sequence_handler_bt (VteTerminal *terminal, GValueArray *params)
{
	long newcol;

	/* Calculate which column is the previous tab stop. */
	newcol = terminal->pvt->screen->cursor_current.col;

	if (terminal->pvt->tabstops != NULL) {
		/* Find the next tabstop. */
		while (newcol > 0) {
			newcol--;
			if (_vte_terminal_get_tabstop(terminal,
						     newcol % terminal->column_count)) {
				break;
			}
		}
	}

	/* Warp the cursor. */
	_vte_debug_print(VTE_DEBUG_PARSE,
			"Moving cursor to column %ld.\n", (long)newcol);
	terminal->pvt->screen->cursor_current.col = newcol;
}

/* Clear from the cursor position to the beginning of the line. */
static void
vte_sequence_handler_cb (VteTerminal *terminal, GValueArray *params)
{
	VteRowData *rowdata;
	long i;
	VteScreen *screen;
	VteCell *pcell;
	screen = terminal->pvt->screen;

	/* Get the data for the row which the cursor points to. */
	rowdata = _vte_terminal_ensure_row(terminal);
	/* Clear the data up to the current column with the default
	 * attributes.  If there is no such character cell, we need
	 * to add one. */
	for (i = 0; i <= screen->cursor_current.col; i++) {
		if (i < (glong) _vte_row_data_length (rowdata)) {
			/* Muck with the cell in this location. */
			pcell = _vte_row_data_get_writable (rowdata, i);
			*pcell = screen->color_defaults;
		} else {
			/* Add new cells until we have one here. */
			_vte_row_data_append (rowdata, &screen->color_defaults);
		}
	}
	/* Repaint this row. */
	_vte_invalidate_cells(terminal,
			      0, screen->cursor_current.col+1,
			      screen->cursor_current.row, 1);

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Clear to the right of the cursor and below the current line. */
static void
vte_sequence_handler_cd (VteTerminal *terminal, GValueArray *params)
{
	VteRowData *rowdata;
	glong i;
	VteScreen *screen;

	screen = terminal->pvt->screen;
	/* If the cursor is actually on the screen, clear the rest of the
	 * row the cursor is on and all of the rows below the cursor. */
	i = screen->cursor_current.row;
	if (i < _vte_ring_next(screen->row_data)) {
		/* Get the data for the row we're clipping. */
		rowdata = _vte_ring_index_writable (screen->row_data, i);
		/* Clear everything to the right of the cursor. */
		if (rowdata)
			_vte_row_data_shrink (rowdata, screen->cursor_current.col);
	}
	/* Now for the rest of the lines. */
	for (i = screen->cursor_current.row + 1;
	     i < _vte_ring_next(screen->row_data);
	     i++) {
		/* Get the data for the row we're removing. */
		rowdata = _vte_ring_index_writable (screen->row_data, i);
		/* Remove it. */
		if (rowdata)
			_vte_row_data_shrink (rowdata, 0);
	}
	/* Now fill the cleared areas. */
	for (i = screen->cursor_current.row;
	     i < screen->insert_delta + terminal->row_count;
	     i++) {
		/* Retrieve the row's data, creating it if necessary. */
		if (_vte_ring_contains (screen->row_data, i)) {
			rowdata = _vte_ring_index_writable (screen->row_data, i);
			g_assert(rowdata != NULL);
		} else {
			rowdata = _vte_terminal_ring_append (terminal, FALSE);
		}
		/* Pad out the row. */
		_vte_row_data_fill (rowdata, &screen->fill_defaults, terminal->column_count);
		rowdata->attr.soft_wrapped = 0;
		/* Repaint this row. */
		_vte_invalidate_cells(terminal,
				      0, terminal->column_count,
				      i, 1);
	}

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Clear from the cursor position to the end of the line. */
static void
vte_sequence_handler_ce (VteTerminal *terminal, GValueArray *params)
{
	VteRowData *rowdata;
	VteScreen *screen;

	screen = terminal->pvt->screen;
	/* Get the data for the row which the cursor points to. */
	rowdata = _vte_terminal_ensure_row(terminal);
	g_assert(rowdata != NULL);
	/* Remove the data at the end of the array until the current column
	 * is the end of the array. */
	if ((glong) _vte_row_data_length (rowdata) > screen->cursor_current.col) {
		_vte_row_data_shrink (rowdata, screen->cursor_current.col);
		/* We've modified the display.  Make a note of it. */
		terminal->pvt->text_deleted_flag = TRUE;
	}
	if (screen->fill_defaults.attr.back != VTE_DEF_BG) {
		/* Add enough cells to fill out the row. */
		_vte_row_data_fill (rowdata, &screen->fill_defaults, terminal->column_count);
	}
	rowdata->attr.soft_wrapped = 0;
	/* Repaint this row. */
	_vte_invalidate_cells(terminal,
			      screen->cursor_current.col,
			      terminal->column_count -
			      screen->cursor_current.col,
			      screen->cursor_current.row, 1);
}

/* Move the cursor to the given column (horizontal position). */
static void
vte_sequence_handler_ch (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	GValue *value;
	long val;

	screen = terminal->pvt->screen;
	/* We only care if there's a parameter in there. */
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			val = CLAMP(g_value_get_long(value),
				    0,
				    terminal->column_count - 1);
			/* Move the cursor. */
			screen->cursor_current.col = val;
			_vte_terminal_cleanup_tab_fragments_at_cursor (terminal);
		}
	}
}

/* Clear the screen and home the cursor. */
static void
vte_sequence_handler_cl (VteTerminal *terminal, GValueArray *params)
{
	_vte_terminal_clear_screen (terminal);
	_vte_terminal_home_cursor (terminal);

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Move the cursor to the given position. */
static void
vte_sequence_handler_cm (VteTerminal *terminal, GValueArray *params)
{
	GValue *row, *col;
	long rowval, colval, origin;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	/* We need at least two parameters. */
	rowval = colval = 0;
	if (params != NULL && params->n_values >= 1) {
		/* The first is the row, the second is the column. */
		row = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(row)) {
			if (screen->origin_mode &&
			    screen->scrolling_restricted) {
				origin = screen->scrolling_region.start;
			} else {
				origin = 0;
			}
			rowval = g_value_get_long(row) + origin;
			rowval = CLAMP(rowval, 0, terminal->row_count - 1);
		}
		if (params->n_values >= 2) {
			col = g_value_array_get_nth(params, 1);
			if (G_VALUE_HOLDS_LONG(col)) {
				colval = g_value_get_long(col);
				colval = CLAMP(colval, 0, terminal->column_count - 1);
			}
		}
	}
	screen->cursor_current.row = rowval + screen->insert_delta;
	screen->cursor_current.col = colval;
	_vte_terminal_cleanup_tab_fragments_at_cursor (terminal);
}

/* Carriage return. */
static void
vte_sequence_handler_cr (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->cursor_current.col = 0;
}

/* Restrict scrolling and updates to a subset of the visible lines. */
static void
vte_sequence_handler_cs (VteTerminal *terminal, GValueArray *params)
{
	long start=-1, end=-1, rows;
	GValue *value;
	VteScreen *screen;

	_vte_terminal_home_cursor (terminal);

	/* We require two parameters.  Anything less is a reset. */
	screen = terminal->pvt->screen;
	if ((params == NULL) || (params->n_values < 2)) {
		screen->scrolling_restricted = FALSE;
		return;
	}
	/* Extract the two values. */
	value = g_value_array_get_nth(params, 0);
	if (G_VALUE_HOLDS_LONG(value)) {
		start = g_value_get_long(value);
	}
	value = g_value_array_get_nth(params, 1);
	if (G_VALUE_HOLDS_LONG(value)) {
		end = g_value_get_long(value);
	}
	/* Catch garbage. */
	rows = terminal->row_count;
	if (start <= 0 || start >= rows) {
		start = 0;
	}
	if (end <= 0 || end >= rows) {
		end = rows - 1;
	}
	/* Set the right values. */
	screen->scrolling_region.start = start;
	screen->scrolling_region.end = end;
	screen->scrolling_restricted = TRUE;
	/* Special case -- run wild, run free. */
	if (screen->scrolling_region.start == 0 &&
	    screen->scrolling_region.end == rows - 1) {
		screen->scrolling_restricted = FALSE;
	}
}

/* Restrict scrolling and updates to a subset of the visible lines, because
 * GNU Emacs is special. */
static void
vte_sequence_handler_cS (VteTerminal *terminal, GValueArray *params)
{
	long start=0, end=terminal->row_count-1, rows;
	GValue *value;
	VteScreen *screen;

	/* We require four parameters. */
	screen = terminal->pvt->screen;
	if ((params == NULL) || (params->n_values < 2)) {
		screen->scrolling_restricted = FALSE;
		return;
	}
	/* Extract the two parameters we care about, encoded as the number
	 * of lines above and below the scrolling region, respectively. */
	value = g_value_array_get_nth(params, 1);
	if (G_VALUE_HOLDS_LONG(value)) {
		start = g_value_get_long(value);
	}
	value = g_value_array_get_nth(params, 2);
	if (G_VALUE_HOLDS_LONG(value)) {
		end -= g_value_get_long(value);
	}
	/* Set the right values. */
	screen->scrolling_region.start = start;
	screen->scrolling_region.end = end;
	screen->scrolling_restricted = TRUE;
	/* Special case -- run wild, run free. */
	rows = terminal->row_count;
	if ((screen->scrolling_region.start == 0) &&
	    (screen->scrolling_region.end == rows - 1)) {
		screen->scrolling_restricted = FALSE;
	}
	/* Clamp the cursor to the scrolling region. */
	screen->cursor_current.row = CLAMP(screen->cursor_current.row,
					   screen->insert_delta + start,
					   screen->insert_delta + end);
}

/* Clear all tab stops. */
static void
vte_sequence_handler_ct (VteTerminal *terminal, GValueArray *params)
{
	if (terminal->pvt->tabstops != NULL) {
		g_hash_table_destroy(terminal->pvt->tabstops);
		terminal->pvt->tabstops = NULL;
	}
}

/* Move the cursor to the lower left-hand corner. */
static void
vte_sequence_handler_cursor_lower_left (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	long row;
	screen = terminal->pvt->screen;
	row = MAX(0, terminal->row_count - 1);
	screen->cursor_current.row = screen->insert_delta + row;
	screen->cursor_current.col = 0;
}

/* Move the cursor to the beginning of the next line, scrolling if necessary. */
static void
vte_sequence_handler_cursor_next_line (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->cursor_current.col = 0;
	vte_sequence_handler_DO (terminal, params);
}

/* Move the cursor to the beginning of the next line, scrolling if necessary. */
static void
vte_sequence_handler_cursor_preceding_line (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->cursor_current.col = 0;
	vte_sequence_handler_UP (terminal, params);
}

/* Move the cursor to the given row (vertical position). */
static void
vte_sequence_handler_cv (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	GValue *value;
	long val, origin;
	screen = terminal->pvt->screen;
	/* We only care if there's a parameter in there. */
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			/* Move the cursor. */
			if (screen->origin_mode &&
			    screen->scrolling_restricted) {
				origin = screen->scrolling_region.start;
			} else {
				origin = 0;
			}
			val = g_value_get_long(value) + origin;
			val = CLAMP(val, 0, terminal->row_count - 1);
			screen->cursor_current.row = screen->insert_delta + val;
		}
	}
}

/* Delete a character at the current cursor position. */
static void
vte_sequence_handler_dc (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	VteRowData *rowdata;
	long col;

	screen = terminal->pvt->screen;

	if (_vte_ring_next(screen->row_data) > screen->cursor_current.row) {
		long len;
		/* Get the data for the row which the cursor points to. */
		rowdata = _vte_ring_index_writable (screen->row_data, screen->cursor_current.row);
		g_assert(rowdata != NULL);
		col = screen->cursor_current.col;
		len = _vte_row_data_length (rowdata);
		/* Remove the column. */
		if (col < len) {
			_vte_row_data_remove (rowdata, col);
			if (screen->fill_defaults.attr.back != VTE_DEF_BG) {
				_vte_row_data_fill (rowdata, &screen->fill_defaults, terminal->column_count);
				len = terminal->column_count;
			}
			/* Repaint this row. */
			_vte_invalidate_cells(terminal,
					col, len - col,
					screen->cursor_current.row, 1);
		}
	}

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Delete N characters at the current cursor position. */
static void
vte_sequence_handler_DC (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_multiple(terminal, params, vte_sequence_handler_dc);
}

/* Delete a line at the current cursor position. */
static void
vte_sequence_handler_dl (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	long start, end, param, i;
	GValue *value;

	/* Find out which part of the screen we're messing with. */
	screen = terminal->pvt->screen;
	start = screen->cursor_current.row;
	if (screen->scrolling_restricted) {
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		end = screen->insert_delta + terminal->row_count - 1;
	}

	/* Extract any parameters. */
	param = 1;
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			param = g_value_get_long(value);
		}
	}

	/* Delete the right number of lines. */
	for (i = 0; i < param; i++) {
		/* Clear a line off the end of the region and add one to the
		 * top of the region. */
		_vte_terminal_ring_remove (terminal, start);
		_vte_terminal_ring_insert (terminal, end, TRUE);
		/* Adjust the scrollbars if necessary. */
		_vte_terminal_adjust_adjustments(terminal);
	}

	/* Update the display. */
	_vte_terminal_scroll_region(terminal, start, end - start + 1, -param);

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Delete N lines at the current cursor position. */
static void
vte_sequence_handler_DL (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_dl (terminal, params);
}

/* Cursor down, no scrolling. */
static void
vte_sequence_handler_do (VteTerminal *terminal, GValueArray *params)
{
	long start, end;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	if (screen->scrolling_restricted) {
		start = screen->insert_delta + screen->scrolling_region.start;
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		start = screen->insert_delta;
		end = start + terminal->row_count - 1;
	}

	/* Move the cursor down. */
	screen->cursor_current.row = MIN(screen->cursor_current.row + 1, end);
}

/* Cursor down, no scrolling. */
static void
vte_sequence_handler_DO (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_multiple(terminal, params, vte_sequence_handler_do);
}

/* Start using alternate character set. */
static void
vte_sequence_handler_eA (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_ae (terminal, params);
}

/* Erase characters starting at the cursor position (overwriting N with
 * spaces, but not moving the cursor). */
static void
vte_sequence_handler_ec (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	VteRowData *rowdata;
	GValue *value;
	VteCell *cell;
	long col, i, count;

	screen = terminal->pvt->screen;

	/* If we got a parameter, use it. */
	count = 1;
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			count = g_value_get_long(value);
		}
	}

	/* Clear out the given number of characters. */
	rowdata = _vte_terminal_ensure_row(terminal);
	if (_vte_ring_next(screen->row_data) > screen->cursor_current.row) {
		g_assert(rowdata != NULL);
		/* Write over the characters.  (If there aren't enough, we'll
		 * need to create them.) */
		for (i = 0; i < count; i++) {
			col = screen->cursor_current.col + i;
			if (col >= 0) {
				if (col < (glong) _vte_row_data_length (rowdata)) {
					/* Replace this cell with the current
					 * defaults. */
					cell = _vte_row_data_get_writable (rowdata, col);
					*cell = screen->color_defaults;
				} else {
					/* Add new cells until we have one here. */
					_vte_row_data_fill (rowdata, &screen->color_defaults, col);
				}
			}
		}
		/* Repaint this row. */
		_vte_invalidate_cells(terminal,
				      screen->cursor_current.col, count,
				      screen->cursor_current.row, 1);
	}

	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* End insert mode. */
static void
vte_sequence_handler_ei (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->insert_mode = FALSE;
}

/* Form-feed / next-page. */
static void
vte_sequence_handler_form_feed (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_index (terminal, params);
}

/* Move from status line. */
static void
vte_sequence_handler_fs (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->status_line = FALSE;
}

/* Move the cursor to the home position. */
static void
vte_sequence_handler_ho (VteTerminal *terminal, GValueArray *params)
{
	_vte_terminal_home_cursor (terminal);
}

/* Move the cursor to a specified position. */
static void
vte_sequence_handler_horizontal_and_vertical_position (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_offset(terminal, params, -1, vte_sequence_handler_cm);
}

/* Insert a character. */
static void
vte_sequence_handler_ic (VteTerminal *terminal, GValueArray *params)
{
	VteVisualPosition save;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	save = screen->cursor_current;

	_vte_terminal_insert_char(terminal, ' ', TRUE, TRUE);

	screen->cursor_current = save;
}

/* Insert N characters. */
static void
vte_sequence_handler_IC (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_multiple_r(terminal, params, vte_sequence_handler_ic);
}

/* Begin insert mode. */
static void
vte_sequence_handler_im (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->insert_mode = TRUE;
}

/* Cursor down, with scrolling. */
static void
vte_sequence_handler_index (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_sf (terminal, params);
}

/* Send me a backspace key sym, will you?  Guess that the application meant
 * to send the cursor back one position. */
static void
vte_sequence_handler_kb (VteTerminal *terminal, GValueArray *params)
{
	/* Move the cursor left. */
	vte_sequence_handler_le (terminal, params);
}

/* Keypad mode end. */
static void
vte_sequence_handler_ke (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->keypad_mode = VTE_KEYMODE_NORMAL;
}

/* Keypad mode start. */
static void
vte_sequence_handler_ks (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->keypad_mode = VTE_KEYMODE_APPLICATION;
}

/* Cursor left. */
static void
vte_sequence_handler_le (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;

	screen = terminal->pvt->screen;
	if (screen->cursor_current.col > 0) {
		/* There's room to move left, so do so. */
		screen->cursor_current.col--;
		_vte_terminal_cleanup_tab_fragments_at_cursor (terminal);
	} else {
		if (terminal->pvt->flags.bw) {
			/* Wrap to the previous line. */
			screen->cursor_current.col = terminal->column_count - 1;
			if (screen->scrolling_restricted) {
				vte_sequence_handler_sr (terminal, params);
			} else {
				screen->cursor_current.row = MAX(screen->cursor_current.row - 1,
								 screen->insert_delta);
			}
		} else {
			/* Stick to the first column. */
			screen->cursor_current.col = 0;
		}
	}
}

/* Move the cursor left N columns. */
static void
vte_sequence_handler_LE (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_multiple(terminal, params, vte_sequence_handler_le);
}

/* Move the cursor to the lower left corner of the display. */
static void
vte_sequence_handler_ll (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	screen = terminal->pvt->screen;
	screen->cursor_current.row = MAX(screen->insert_delta,
					 screen->insert_delta +
					 terminal->row_count - 1);
	screen->cursor_current.col = 0;
}

/* Blink on. */
static void
vte_sequence_handler_mb (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->defaults.attr.blink = 1;
}

/* Bold on. */
static void
vte_sequence_handler_md (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->defaults.attr.bold = 1;
	terminal->pvt->screen->defaults.attr.half = 0;
}

/* End modes. */
static void
vte_sequence_handler_me (VteTerminal *terminal, GValueArray *params)
{
	_vte_terminal_set_default_attributes(terminal);
}

/* Half-bright on. */
static void
vte_sequence_handler_mh (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->defaults.attr.half = 1;
	terminal->pvt->screen->defaults.attr.bold = 0;
}

/* Invisible on. */
static void
vte_sequence_handler_mk (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->defaults.attr.invisible = 1;
}

/* Protect on. */
static void
vte_sequence_handler_mp (VteTerminal *terminal, GValueArray *params)
{
	/* unused; bug 499893
	terminal->pvt->screen->defaults.attr.protect = 1;
	 */
}

/* Reverse on. */
static void
vte_sequence_handler_mr (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->defaults.attr.reverse = 1;
}

/* Cursor right. */
static void
vte_sequence_handler_nd (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	screen = terminal->pvt->screen;
	if ((screen->cursor_current.col + 1) < terminal->column_count) {
		/* There's room to move right. */
		screen->cursor_current.col++;
	}
}

/* Move the cursor to the beginning of the next line, scrolling if necessary. */
static void
vte_sequence_handler_next_line (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->cursor_current.col = 0;
	vte_sequence_handler_DO (terminal, params);
}

/* No-op. */
static void
vte_sequence_handler_noop (VteTerminal *terminal, GValueArray *params)
{
}

/* Carriage return command(?). */
static void
vte_sequence_handler_nw (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_cr (terminal, params);
}

/* Restore cursor (position). */
static void
vte_sequence_handler_rc (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	screen = terminal->pvt->screen;
	screen->cursor_current.col = screen->cursor_saved.col;
	screen->cursor_current.row = CLAMP(screen->cursor_saved.row +
					   screen->insert_delta,
					   screen->insert_delta,
					   screen->insert_delta +
					   terminal->row_count - 1);
}

/* Cursor down, with scrolling. */
static void
vte_sequence_handler_reverse_index (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_sr (terminal, params);
}

/* Cursor right N characters. */
static void
vte_sequence_handler_RI (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_multiple(terminal, params, vte_sequence_handler_nd);
}

/* Save cursor (position). */
static void
vte_sequence_handler_sc (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	screen = terminal->pvt->screen;
	screen->cursor_saved.col = screen->cursor_current.col;
	screen->cursor_saved.row = CLAMP(screen->cursor_current.row -
					 screen->insert_delta,
					 0, terminal->row_count - 1);
}

/* Scroll the text down, but don't move the cursor. */
static void
vte_sequence_handler_scroll_down (VteTerminal *terminal, GValueArray *params)
{
	long val = 1;
	GValue *value;

	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			val = g_value_get_long(value);
			val = MAX(val, 1);
		}
	}

	_vte_terminal_scroll_text (terminal, val);
}

/* change color in the palette */
static void
vte_sequence_handler_change_color (VteTerminal *terminal, GValueArray *params)
{
	gchar **pairs, *str = NULL;
	GValue *value;
	GdkColor color;
	guint idx, i;

	if (params != NULL && params->n_values > 0) {
		value = g_value_array_get_nth (params, 0);

		if (G_VALUE_HOLDS_STRING (value))
			str = g_value_dup_string (value);
		else if (G_VALUE_HOLDS_POINTER (value))
			str = vte_ucs4_to_utf8 (terminal, g_value_get_pointer (value));

		if (! str)
			return;

		pairs = g_strsplit (str, ";", 0);
		if (! pairs) {
			g_free (str);
			return;
		}

		for (i = 0; pairs[i] && pairs[i + 1]; i += 2) {
			idx = strtoul (pairs[i], (char **) NULL, 10);

			if (idx >= VTE_DEF_FG)
				continue;

			if (vte_parse_color (pairs[i + 1], &color)) {
				terminal->pvt->palette[idx].red = color.red;
				terminal->pvt->palette[idx].green = color.green;
				terminal->pvt->palette[idx].blue = color.blue;
			} else if (strcmp (pairs[i + 1], "?") == 0) {
				gchar buf[128];
				g_snprintf (buf, sizeof (buf),
					    _VTE_CAP_OSC "4;%u;rgb:%04x/%04x/%04x" BEL, idx,
					    terminal->pvt->palette[idx].red,
					    terminal->pvt->palette[idx].green,
					    terminal->pvt->palette[idx].blue);
				vte_terminal_feed_child (terminal, buf, -1);
			}
		}

		g_free (str);
		g_strfreev (pairs);

		/* emit the refresh as the palette has changed and previous
		 * renders need to be updated. */
		vte_terminal_emit_refresh_window (terminal);
	}
}

/* Scroll the text up, but don't move the cursor. */
static void
vte_sequence_handler_scroll_up (VteTerminal *terminal, GValueArray *params)
{
	long val = 1;
	GValue *value;

	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			val = g_value_get_long(value);
			val = MAX(val, 1);
		}
	}

	_vte_terminal_scroll_text (terminal, -val);
}

/* Standout end. */
static void
vte_sequence_handler_se (VteTerminal *terminal, GValueArray *params)
{
	char *standout;

	/* Standout may be mapped to another attribute, so attempt to do
	 * the Right Thing here.
	 *
	 * If the standout sequence is the same as another sequence, do what
	 * we'd do for that other sequence instead. */

	standout = _vte_termcap_find_string(terminal->pvt->termcap,
					    terminal->pvt->emulation,
					    "so");
	g_assert(standout != NULL);

	if (vte_terminal_termcap_string_same_as_for (terminal, standout, "mb") /* blink */   ||
	    vte_terminal_termcap_string_same_as_for (terminal, standout, "md") /* bold */    ||
	    vte_terminal_termcap_string_same_as_for (terminal, standout, "mh") /* half */    ||
	    vte_terminal_termcap_string_same_as_for (terminal, standout, "mr") /* reverse */ ||
	    vte_terminal_termcap_string_same_as_for (terminal, standout, "us") /* underline */)
	{
		vte_sequence_handler_me (terminal, params);
	} else {
		/* Otherwise just set standout mode. */
		terminal->pvt->screen->defaults.attr.standout = 0;
	}

	g_free(standout);
}

/* Cursor down, with scrolling. */
static void
vte_sequence_handler_sf (VteTerminal *terminal, GValueArray *params)
{
	_vte_terminal_cursor_down (terminal);
}

/* Cursor down, with scrolling. */
static void
vte_sequence_handler_SF (VteTerminal *terminal, GValueArray *params)
{
	/* XXX implement this directly in _vte_terminal_cursor_down */
	vte_sequence_handler_multiple(terminal, params, vte_sequence_handler_sf);
}

/* Standout start. */
static void
vte_sequence_handler_so (VteTerminal *terminal, GValueArray *params)
{
	char *standout;

	/* Standout may be mapped to another attribute, so attempt to do
	 * the Right Thing here.
	 *
	 * If the standout sequence is the same as another sequence, do what
	 * we'd do for that other sequence instead. */

	standout = _vte_termcap_find_string(terminal->pvt->termcap,
					    terminal->pvt->emulation,
					    "so");
	g_assert(standout != NULL);

	if (vte_terminal_termcap_string_same_as_for (terminal, standout, "mb") /* blink */)
		vte_sequence_handler_mb (terminal, params);
	else if (vte_terminal_termcap_string_same_as_for (terminal, standout, "md") /* bold */)
		vte_sequence_handler_md (terminal, params);
	else if (vte_terminal_termcap_string_same_as_for (terminal, standout, "mh") /* half */)
		vte_sequence_handler_mh (terminal, params);
	else if (vte_terminal_termcap_string_same_as_for (terminal, standout, "mr") /* reverse */)
		vte_sequence_handler_mr (terminal, params);
	else if (vte_terminal_termcap_string_same_as_for (terminal, standout, "us") /* underline */)
		vte_sequence_handler_us (terminal, params);
	else {
		/* Otherwise just set standout mode. */
		terminal->pvt->screen->defaults.attr.standout = 1;
	}

	g_free(standout);
}

/* Cursor up, scrolling if need be. */
static void
vte_sequence_handler_sr (VteTerminal *terminal, GValueArray *params)
{
	long start, end;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	if (screen->scrolling_restricted) {
		start = screen->scrolling_region.start + screen->insert_delta;
		end = screen->scrolling_region.end + screen->insert_delta;
	} else {
		start = terminal->pvt->screen->insert_delta;
		end = start + terminal->row_count - 1;
	}

	if (screen->cursor_current.row == start) {
		/* If we're at the top of the scrolling region, add a
		 * line at the top to scroll the bottom off. */
		_vte_terminal_ring_remove (terminal, end);
		_vte_terminal_ring_insert (terminal, start, TRUE);
		/* Update the display. */
		_vte_terminal_scroll_region(terminal, start, end - start + 1, 1);
		_vte_invalidate_cells(terminal,
				      0, terminal->column_count,
				      start, 2);
	} else {
		/* Otherwise, just move the cursor up. */
		screen->cursor_current.row--;
	}
	/* Adjust the scrollbars if necessary. */
	_vte_terminal_adjust_adjustments(terminal);
	/* We modified the display, so make a note of it. */
	terminal->pvt->text_modified_flag = TRUE;
}

/* Cursor up, with scrolling. */
static void
vte_sequence_handler_SR (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_multiple(terminal, params, vte_sequence_handler_sr);
}

/* Set tab stop in the current column. */
static void
vte_sequence_handler_st (VteTerminal *terminal, GValueArray *params)
{
	if (terminal->pvt->tabstops == NULL) {
		terminal->pvt->tabstops = g_hash_table_new(NULL, NULL);
	}
	_vte_terminal_set_tabstop(terminal,
				 terminal->pvt->screen->cursor_current.col);
}

/* Tab. */
static void
vte_sequence_handler_ta (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	long old_len, newcol, col;

	/* Calculate which column is the next tab stop. */
	screen = terminal->pvt->screen;
	newcol = col = screen->cursor_current.col;

	g_assert (col >= 0);

	if (terminal->pvt->tabstops != NULL) {
		/* Find the next tabstop. */
		for (newcol++; newcol < VTE_TAB_MAX; newcol++) {
			if (_vte_terminal_get_tabstop(terminal, newcol)) {
				break;
			}
		}
	}

	/* If we have no tab stops or went past the end of the line, stop
	 * at the right-most column. */
	if (newcol >= terminal->column_count) {
		newcol = terminal->column_count - 1;
	}

	/* but make sure we don't move cursor back (bug #340631) */
	if (col < newcol) {
		VteRowData *rowdata = _vte_terminal_ensure_row (terminal);

		/* Smart tab handling: bug 353610
		 *
		 * If we currently don't have any cells in the space this
		 * tab creates, we try to make the tab character copyable,
		 * by appending a single tab char with lots of fragment
		 * cells following it.
		 *
		 * Otherwise, just append empty cells that will show up
		 * as a space each.
		 */

		old_len = _vte_row_data_length (rowdata);
		_vte_row_data_fill (rowdata, &screen->fill_defaults, newcol);

		/* Insert smart tab if there's nothing in the line after
		 * us.  Though, there may be empty cells (with non-default
		 * background color for example.
		 *
		 * Notable bugs here: 545924 and 597242 */
		{
			glong i;
			gboolean found = FALSE;
			for (i = old_len; i > col; i--) {
				const VteCell *cell = _vte_row_data_get (rowdata, i - 1);
				if (cell->attr.fragment || cell->c != 0) {
					found = TRUE;
					break;
				}
			}
			/* Nothing found on the line after us, turn this into
			 * a smart tab */
			if (!found) {
				VteCell *cell = _vte_row_data_get_writable (rowdata, col);
				VteCell tab = *cell;
				tab.attr.columns = newcol - col;
				tab.c = '\t';
				/* Check if it fits in columns */
				if (tab.attr.columns == newcol - col) {
					/* Save tab char */
					*cell = tab;
					/* And adjust the fragments */
					for (i = col + 1; i < newcol; i++) {
						cell = _vte_row_data_get_writable (rowdata, i);
						cell->c = '\t';
						cell->attr.columns = 1;
						cell->attr.fragment = 1;
					}
				}
			}
		}

		_vte_invalidate_cells (terminal,
				screen->cursor_current.col,
				newcol - screen->cursor_current.col,
				screen->cursor_current.row, 1);
		screen->cursor_current.col = newcol;
	}
}

/* Clear tabs selectively. */
static void
vte_sequence_handler_tab_clear (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	long param = 0;

	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			param = g_value_get_long(value);
		}
	}
	if (param == 0) {
		_vte_terminal_clear_tabstop(terminal,
					   terminal->pvt->screen->cursor_current.col);
	} else
	if (param == 3) {
		if (terminal->pvt->tabstops != NULL) {
			g_hash_table_destroy(terminal->pvt->tabstops);
			terminal->pvt->tabstops = NULL;
		}
	}
}

/* Move to status line. */
static void
vte_sequence_handler_ts (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->status_line = TRUE;
	terminal->pvt->screen->status_line_changed = TRUE;
	g_string_truncate(terminal->pvt->screen->status_line_contents, 0);
}

/* Underline this character and move right. */
static void
vte_sequence_handler_uc (VteTerminal *terminal, GValueArray *params)
{
	VteCell *cell;
	int column;
	VteScreen *screen;

	screen = terminal->pvt->screen;
	column = screen->cursor_current.col;
	cell = vte_terminal_find_charcell(terminal, column, screen->cursor_current.row);
	while ((cell != NULL) && (cell->attr.fragment) && (column > 0)) {
		column--;
		cell = vte_terminal_find_charcell(terminal, column, screen->cursor_current.row);
	}
	if (cell != NULL) {
		/* Set this character to be underlined. */
		cell->attr.underline = 1;
		/* Cause the character to be repainted. */
		_vte_invalidate_cells(terminal,
				      column, cell->attr.columns,
				      screen->cursor_current.row, 1);
		/* Move the cursor right. */
		vte_sequence_handler_nd (terminal, params);
	}

	/* We've modified the display without changing the text.  Make a note
	 * of it. */
	terminal->pvt->text_modified_flag = TRUE;
}

/* Underline end. */
static void
vte_sequence_handler_ue (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->defaults.attr.underline = 0;
}

/* Cursor up, no scrolling. */
static void
vte_sequence_handler_up (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	long start, end;

	screen = terminal->pvt->screen;

	if (screen->scrolling_restricted) {
		start = screen->insert_delta + screen->scrolling_region.start;
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		start = screen->insert_delta;
		end = start + terminal->row_count - 1;
	}

	screen->cursor_current.row = MAX(screen->cursor_current.row - 1, start);
}

/* Cursor up N lines, no scrolling. */
static void
vte_sequence_handler_UP (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_multiple(terminal, params, vte_sequence_handler_up);
}

/* Underline start. */
static void
vte_sequence_handler_us (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->screen->defaults.attr.underline = 1;
}

/* Visible bell. */
static void
vte_sequence_handler_vb (VteTerminal *terminal, GValueArray *params)
{
	_vte_terminal_visible_beep (terminal);
}

/* Cursor visible. */
static void
vte_sequence_handler_ve (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->cursor_visible = TRUE;
}

/* Vertical tab. */
static void
vte_sequence_handler_vertical_tab (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_index (terminal, params);
}

/* Cursor invisible. */
static void
vte_sequence_handler_vi (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->cursor_visible = FALSE;
}

/* Cursor standout. */
static void
vte_sequence_handler_vs (VteTerminal *terminal, GValueArray *params)
{
	terminal->pvt->cursor_visible = TRUE; /* FIXME: should be *more*
						 visible. */
}

/* Handle ANSI color setting and related stuffs (SGR). */
static void
vte_sequence_handler_character_attributes (VteTerminal *terminal, GValueArray *params)
{
	unsigned int i;
	GValue *value;
	long param;
	/* The default parameter is zero. */
	param = 0;
	/* Step through each numeric parameter. */
	for (i = 0; (params != NULL) && (i < params->n_values); i++) {
		/* If this parameter isn't a number, skip it. */
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		param = g_value_get_long(value);
		switch (param) {
		case 0:
			_vte_terminal_set_default_attributes(terminal);
			break;
		case 1:
			terminal->pvt->screen->defaults.attr.bold = 1;
			terminal->pvt->screen->defaults.attr.half = 0;
			break;
		case 2:
			terminal->pvt->screen->defaults.attr.half = 1;
			terminal->pvt->screen->defaults.attr.bold = 0;
			break;
		case 4:
			terminal->pvt->screen->defaults.attr.underline = 1;
			break;
		case 5:
			terminal->pvt->screen->defaults.attr.blink = 1;
			break;
		case 7:
			terminal->pvt->screen->defaults.attr.reverse = 1;
			break;
		case 8:
			terminal->pvt->screen->defaults.attr.invisible = 1;
			break;
		case 9:
			terminal->pvt->screen->defaults.attr.strikethrough = 1;
			break;
		case 21: /* Error in old versions of linux console. */
		case 22: /* ECMA 48. */
			terminal->pvt->screen->defaults.attr.bold = 0;
			terminal->pvt->screen->defaults.attr.half = 0;
			break;
		case 24:
			terminal->pvt->screen->defaults.attr.underline = 0;
			break;
		case 25:
			terminal->pvt->screen->defaults.attr.blink = 0;
			break;
		case 27:
			terminal->pvt->screen->defaults.attr.reverse = 0;
			break;
		case 28:
			terminal->pvt->screen->defaults.attr.invisible = 0;
			break;
		case 29:
			terminal->pvt->screen->defaults.attr.strikethrough = 0;
			break;
		case 30:
		case 31:
		case 32:
		case 33:
		case 34:
		case 35:
		case 36:
		case 37:
			terminal->pvt->screen->defaults.attr.fore = param - 30;
			break;
		case 38:
		{
			/* The format looks like: ^[[38;5;COLORNUMBERm,
			   so look for COLORNUMBER here. */
			if ((i + 2) < params->n_values){
				GValue *value1, *value2;
				long param1, param2;
				value1 = g_value_array_get_nth(params, i + 1);
				value2 = g_value_array_get_nth(params, i + 2);
				if (G_UNLIKELY (!(G_VALUE_HOLDS_LONG(value1) && G_VALUE_HOLDS_LONG(value2))))
					break;
				param1 = g_value_get_long(value1);
				param2 = g_value_get_long(value2);
				if (G_LIKELY (param1 == 5 && param2 >= 0 && param2 < 256))
					terminal->pvt->screen->defaults.attr.fore = param2;
				i += 2;
			}
			break;
		}
		case 39:
			/* default foreground */
			terminal->pvt->screen->defaults.attr.fore = VTE_DEF_FG;
			break;
		case 40:
		case 41:
		case 42:
		case 43:
		case 44:
		case 45:
		case 46:
		case 47:
			terminal->pvt->screen->defaults.attr.back = param - 40;
			break;
		case 48:
		{
			/* The format looks like: ^[[48;5;COLORNUMBERm,
			   so look for COLORNUMBER here. */
			if ((i + 2) < params->n_values){
				GValue *value1, *value2;
				long param1, param2;
				value1 = g_value_array_get_nth(params, i + 1);
				value2 = g_value_array_get_nth(params, i + 2);
				if (G_UNLIKELY (!(G_VALUE_HOLDS_LONG(value1) && G_VALUE_HOLDS_LONG(value2))))
					break;
				param1 = g_value_get_long(value1);
				param2 = g_value_get_long(value2);
				if (G_LIKELY (param1 == 5 && param2 >= 0 && param2 < 256))
					terminal->pvt->screen->defaults.attr.back = param2;
				i += 2;
			}
			break;
		}
		case 49:
			/* default background */
			terminal->pvt->screen->defaults.attr.back = VTE_DEF_BG;
			break;
		case 90:
		case 91:
		case 92:
		case 93:
		case 94:
		case 95:
		case 96:
		case 97:
			terminal->pvt->screen->defaults.attr.fore = param - 90 + VTE_COLOR_BRIGHT_OFFSET;
			break;
		case 100:
		case 101:
		case 102:
		case 103:
		case 104:
		case 105:
		case 106:
		case 107:
			terminal->pvt->screen->defaults.attr.back = param - 100 + VTE_COLOR_BRIGHT_OFFSET;
			break;
		}
	}
	/* If we had no parameters, default to the defaults. */
	if (i == 0) {
		_vte_terminal_set_default_attributes(terminal);
	}
	/* Save the new colors. */
	terminal->pvt->screen->color_defaults.attr.fore =
		terminal->pvt->screen->defaults.attr.fore;
	terminal->pvt->screen->color_defaults.attr.back =
		terminal->pvt->screen->defaults.attr.back;
	terminal->pvt->screen->fill_defaults.attr.fore =
		terminal->pvt->screen->defaults.attr.fore;
	terminal->pvt->screen->fill_defaults.attr.back =
		terminal->pvt->screen->defaults.attr.back;
}

/* Move the cursor to the given column, 1-based. */
static void
vte_sequence_handler_cursor_character_absolute (VteTerminal *terminal, GValueArray *params)
{
	VteScreen *screen;
	GValue *value;
	long val;

	screen = terminal->pvt->screen;

        val = 0;
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			val = CLAMP(g_value_get_long(value),
				    1, terminal->column_count) - 1;
		}
	}

        screen->cursor_current.col = val;
	_vte_terminal_cleanup_tab_fragments_at_cursor (terminal);
}

/* Move the cursor to the given position, 1-based. */
static void
vte_sequence_handler_cursor_position (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_offset(terminal, params, -1, vte_sequence_handler_cm);
}

/* Request terminal attributes. */
static void
vte_sequence_handler_request_terminal_parameters (VteTerminal *terminal, GValueArray *params)
{
	vte_terminal_feed_child(terminal, "\e[?x", -1);
}

/* Request terminal attributes. */
static void
vte_sequence_handler_return_terminal_status (VteTerminal *terminal, GValueArray *params)
{
	vte_terminal_feed_child(terminal, "", 0);
}

/* Send primary device attributes. */
static void
vte_sequence_handler_send_primary_device_attributes (VteTerminal *terminal, GValueArray *params)
{
	/* Claim to be a VT220 with only national character set support. */
	vte_terminal_feed_child(terminal, "\e[?62;9;c", -1);
}

/* Send terminal ID. */
static void
vte_sequence_handler_return_terminal_id (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_send_primary_device_attributes (terminal, params);
}

/* Send secondary device attributes. */
static void
vte_sequence_handler_send_secondary_device_attributes (VteTerminal *terminal, GValueArray *params)
{
	char **version;
	char buf[128];
	long ver = 0, i;
	/* Claim to be a VT220, more or less.  The '>' in the response appears
	 * to be undocumented. */
	version = g_strsplit(VERSION, ".", 0);
	if (version != NULL) {
		for (i = 0; version[i] != NULL; i++) {
			ver = ver * 100;
			ver += atol(version[i]);
		}
		g_strfreev(version);
	}
	g_snprintf(buf, sizeof (buf), _VTE_CAP_ESC "[>1;%ld;0c", ver);
	vte_terminal_feed_child(terminal, buf, -1);
}

/* Set one or the other. */
static void
vte_sequence_handler_set_icon_title (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_set_title_internal(terminal, params, TRUE, FALSE);
}

static void
vte_sequence_handler_set_window_title (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_set_title_internal(terminal, params, FALSE, TRUE);
}

/* Set both the window and icon titles to the same string. */
static void
vte_sequence_handler_set_icon_and_window_title (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_set_title_internal(terminal, params, TRUE, TRUE);
}

/* Restrict the scrolling region. */
static void
vte_sequence_handler_set_scrolling_region (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_offset(terminal, params, -1, vte_sequence_handler_cs);
}

static void
vte_sequence_handler_set_scrolling_region_from_start (VteTerminal *terminal, GValueArray *params)
{
	GValue value = {0};

	g_value_init (&value, G_TYPE_LONG);
	g_value_set_long (&value, 0); /* Out of range means start/end */

	g_value_array_insert (params, 0, &value);

	vte_sequence_handler_offset(terminal, params, -1, vte_sequence_handler_cs);
}

static void
vte_sequence_handler_set_scrolling_region_to_end (VteTerminal *terminal, GValueArray *params)
{
	GValue value = {0};

	g_value_init (&value, G_TYPE_LONG);
	g_value_set_long (&value, 0); /* Out of range means start/end */

	g_value_array_insert (params, 1, &value);

	vte_sequence_handler_offset(terminal, params, -1, vte_sequence_handler_cs);
}

/* Set the application or normal keypad. */
static void
vte_sequence_handler_application_keypad (VteTerminal *terminal, GValueArray *params)
{
	_vte_debug_print(VTE_DEBUG_KEYBOARD,
			"Entering application keypad mode.\n");
	terminal->pvt->keypad_mode = VTE_KEYMODE_APPLICATION;
}

static void
vte_sequence_handler_normal_keypad (VteTerminal *terminal, GValueArray *params)
{
	_vte_debug_print(VTE_DEBUG_KEYBOARD,
			"Leaving application keypad mode.\n");
	terminal->pvt->keypad_mode = VTE_KEYMODE_NORMAL;
}

/* Move the cursor. */
static void
vte_sequence_handler_character_position_absolute (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_offset(terminal, params, -1, vte_sequence_handler_ch);
}
static void
vte_sequence_handler_line_position_absolute (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_offset(terminal, params, -1, vte_sequence_handler_cv);
}

/* Set certain terminal attributes. */
static void
vte_sequence_handler_set_mode (VteTerminal *terminal, GValueArray *params)
{
	guint i;
	long setting;
	GValue *value;
	if ((params == NULL) || (params->n_values == 0)) {
		return;
	}
	for (i = 0; i < params->n_values; i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		setting = g_value_get_long(value);
		vte_sequence_handler_set_mode_internal(terminal, setting, TRUE);
	}
}

/* Unset certain terminal attributes. */
static void
vte_sequence_handler_reset_mode (VteTerminal *terminal, GValueArray *params)
{
	guint i;
	long setting;
	GValue *value;
	if ((params == NULL) || (params->n_values == 0)) {
		return;
	}
	for (i = 0; i < params->n_values; i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		setting = g_value_get_long(value);
		vte_sequence_handler_set_mode_internal(terminal, setting, FALSE);
	}
}

/* Set certain terminal attributes. */
static void
vte_sequence_handler_decset (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	long setting;
	guint i;
	if ((params == NULL) || (params->n_values == 0)) {
		return;
	}
	for (i = 0; i < params->n_values; i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		setting = g_value_get_long(value);
		vte_sequence_handler_decset_internal(terminal, setting, FALSE, FALSE, TRUE);
	}
}

/* Unset certain terminal attributes. */
static void
vte_sequence_handler_decreset (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	long setting;
	guint i;
	if ((params == NULL) || (params->n_values == 0)) {
		return;
	}
	for (i = 0; i < params->n_values; i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		setting = g_value_get_long(value);
		vte_sequence_handler_decset_internal(terminal, setting, FALSE, FALSE, FALSE);
	}
}

/* Erase a specified number of characters. */
static void
vte_sequence_handler_erase_characters (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_ec (terminal, params);
}

/* Erase certain lines in the display. */
static void
vte_sequence_handler_erase_in_display (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	long param;
	guint i;
	/* The default parameter is 0. */
	param = 0;
	/* Pull out a parameter. */
	for (i = 0; (params != NULL) && (i < params->n_values); i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		param = g_value_get_long(value);
	}
	/* Clear the right area. */
	switch (param) {
	case 0:
		/* Clear below the current line. */
		vte_sequence_handler_cd (terminal, NULL);
		break;
	case 1:
		/* Clear above the current line. */
		_vte_terminal_clear_above_current (terminal);
		/* Clear everything to the left of the cursor, too. */
		/* FIXME: vttest. */
		vte_sequence_handler_cb (terminal, NULL);
		break;
	case 2:
		/* Clear the entire screen. */
		_vte_terminal_clear_screen (terminal);
		break;
	default:
		break;
	}
	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Erase certain parts of the current line in the display. */
static void
vte_sequence_handler_erase_in_line (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	long param;
	guint i;
	/* The default parameter is 0. */
	param = 0;
	/* Pull out a parameter. */
	for (i = 0; (params != NULL) && (i < params->n_values); i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		param = g_value_get_long(value);
	}
	/* Clear the right area. */
	switch (param) {
	case 0:
		/* Clear to end of the line. */
		vte_sequence_handler_ce (terminal, NULL);
		break;
	case 1:
		/* Clear to start of the line. */
		vte_sequence_handler_cb (terminal, NULL);
		break;
	case 2:
		/* Clear the entire line. */
		_vte_terminal_clear_current_line (terminal);
		break;
	default:
		break;
	}
	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Perform a full-bore reset. */
static void
vte_sequence_handler_full_reset (VteTerminal *terminal, GValueArray *params)
{
	vte_terminal_reset(terminal, TRUE, TRUE);
}

/* Insert a specified number of blank characters. */
static void
vte_sequence_handler_insert_blank_characters (VteTerminal *terminal, GValueArray *params)
{
	vte_sequence_handler_IC (terminal, params);
}

/* Insert a certain number of lines below the current cursor. */
static void
vte_sequence_handler_insert_lines (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	VteScreen *screen;
	long param, end, row;
	int i;
	screen = terminal->pvt->screen;
	/* The default is one. */
	param = 1;
	/* Extract any parameters. */
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			param = g_value_get_long(value);
		}
	}
	/* Find the region we're messing with. */
	row = screen->cursor_current.row;
	if (screen->scrolling_restricted) {
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		end = screen->insert_delta + terminal->row_count - 1;
	}
	/* Insert the new lines at the cursor. */
	for (i = 0; i < param; i++) {
		/* Clear a line off the end of the region and add one to the
		 * top of the region. */
		_vte_terminal_ring_remove (terminal, end);
		_vte_terminal_ring_insert (terminal, row, TRUE);
	}
	/* Update the display. */
	_vte_terminal_scroll_region(terminal, row, end - row + 1, param);
	/* Adjust the scrollbars if necessary. */
	_vte_terminal_adjust_adjustments(terminal);
	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_inserted_flag = TRUE;
}

/* Delete certain lines from the scrolling region. */
static void
vte_sequence_handler_delete_lines (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	VteScreen *screen;
	long param, end, row;
	int i;

	screen = terminal->pvt->screen;
	/* The default is one. */
	param = 1;
	/* Extract any parameters. */
	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			param = g_value_get_long(value);
		}
	}
	/* Find the region we're messing with. */
	row = screen->cursor_current.row;
	if (screen->scrolling_restricted) {
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		end = screen->insert_delta + terminal->row_count - 1;
	}
	/* Clear them from below the current cursor. */
	for (i = 0; i < param; i++) {
		/* Insert a line at the end of the region and remove one from
		 * the top of the region. */
		_vte_terminal_ring_remove (terminal, row);
		_vte_terminal_ring_insert (terminal, end, TRUE);
	}
	/* Update the display. */
	_vte_terminal_scroll_region(terminal, row, end - row + 1, -param);
	/* Adjust the scrollbars if necessary. */
	_vte_terminal_adjust_adjustments(terminal);
	/* We've modified the display.  Make a note of it. */
	terminal->pvt->text_deleted_flag = TRUE;
}

/* Set the terminal encoding. */
static void
vte_sequence_handler_local_charset (VteTerminal *terminal, GValueArray *params)
{
	G_CONST_RETURN char *locale_encoding;
	g_get_charset(&locale_encoding);
	vte_terminal_set_encoding(terminal, locale_encoding);
}

static void
vte_sequence_handler_utf_8_charset (VteTerminal *terminal, GValueArray *params)
{
	vte_terminal_set_encoding(terminal, "UTF-8");
}

/* Device status reports. The possible reports are the cursor position and
 * whether or not we're okay. */
static void
vte_sequence_handler_device_status_report (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	VteScreen *screen;
	long param;
	char buf[128];

	screen = terminal->pvt->screen;

	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			param = g_value_get_long(value);
			switch (param) {
			case 5:
				/* Send a thumbs-up sequence. */
				vte_terminal_feed_child(terminal, _VTE_CAP_CSI "0n", -1);
				break;
			case 6:
				/* Send the cursor position. */
				g_snprintf(buf, sizeof(buf),
					   _VTE_CAP_CSI "%ld;%ldR",
					   screen->cursor_current.row + 1 -
					   screen->insert_delta,
					   screen->cursor_current.col + 1);
				vte_terminal_feed_child(terminal, buf, -1);
				break;
			default:
				break;
			}
		}
	}
}

/* DEC-style device status reports. */
static void
vte_sequence_handler_dec_device_status_report (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	VteScreen *screen;
	long param;
	char buf[128];

	screen = terminal->pvt->screen;

	if ((params != NULL) && (params->n_values > 0)) {
		value = g_value_array_get_nth(params, 0);
		if (G_VALUE_HOLDS_LONG(value)) {
			param = g_value_get_long(value);
			switch (param) {
			case 6:
				/* Send the cursor position. */
				g_snprintf(buf, sizeof(buf),
					   _VTE_CAP_CSI "?%ld;%ldR",
					   screen->cursor_current.row + 1 -
					   screen->insert_delta,
					   screen->cursor_current.col + 1);
				vte_terminal_feed_child(terminal, buf, -1);
				break;
			case 15:
				/* Send printer status -- 10 = ready,
				 * 11 = not ready.  We don't print. */
				vte_terminal_feed_child(terminal, _VTE_CAP_CSI "?11n", -1);
				break;
			case 25:
				/* Send UDK status -- 20 = locked,
				 * 21 = not locked.  I don't even know what
				 * that means, but punt anyway. */
				vte_terminal_feed_child(terminal, _VTE_CAP_CSI "?20n", -1);
				break;
			case 26:
				/* Send keyboard status.  50 = no locator. */
				vte_terminal_feed_child(terminal, _VTE_CAP_CSI "?50n", -1);
				break;
			default:
				break;
			}
		}
	}
}

/* Restore a certain terminal attribute. */
static void
vte_sequence_handler_restore_mode (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	long setting;
	guint i;
	if ((params == NULL) || (params->n_values == 0)) {
		return;
	}
	for (i = 0; i < params->n_values; i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		setting = g_value_get_long(value);
		vte_sequence_handler_decset_internal(terminal, setting, TRUE, FALSE, FALSE);
	}
}

/* Save a certain terminal attribute. */
static void
vte_sequence_handler_save_mode (VteTerminal *terminal, GValueArray *params)
{
	GValue *value;
	long setting;
	guint i;
	if ((params == NULL) || (params->n_values == 0)) {
		return;
	}
	for (i = 0; i < params->n_values; i++) {
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		setting = g_value_get_long(value);
		vte_sequence_handler_decset_internal(terminal, setting, FALSE, TRUE, FALSE);
	}
}

/* Perform a screen alignment test -- fill all visible cells with the
 * letter "E". */
static void
vte_sequence_handler_screen_alignment_test (VteTerminal *terminal, GValueArray *params)
{
	long row;
	VteRowData *rowdata;
	VteScreen *screen;
	VteCell cell;

	screen = terminal->pvt->screen;

	for (row = terminal->pvt->screen->insert_delta;
	     row < terminal->pvt->screen->insert_delta + terminal->row_count;
	     row++) {
		/* Find this row. */
		while (_vte_ring_next(screen->row_data) <= row)
			_vte_terminal_ring_append (terminal, FALSE);
		_vte_terminal_adjust_adjustments(terminal);
		rowdata = _vte_ring_index_writable (screen->row_data, row);
		g_assert(rowdata != NULL);
		/* Clear this row. */
		_vte_row_data_shrink (rowdata, 0);

		_vte_terminal_emit_text_deleted(terminal);
		/* Fill this row. */
		cell.c = 'E';
		cell.attr = basic_cell.cell.attr;
		cell.attr.columns = 1;
		_vte_row_data_fill (rowdata, &cell, terminal->column_count);
		_vte_terminal_emit_text_inserted(terminal);
	}
	_vte_invalidate_all(terminal);

	/* We modified the display, so make a note of it for completeness. */
	terminal->pvt->text_modified_flag = TRUE;
}

/* Perform a soft reset. */
static void
vte_sequence_handler_soft_reset (VteTerminal *terminal, GValueArray *params)
{
	vte_terminal_reset(terminal, FALSE, FALSE);
}

/* Window manipulation control sequences.  Most of these are considered
 * bad ideas, but they're implemented as signals which the application
 * is free to ignore, so they're harmless. */
static void
vte_sequence_handler_window_manipulation (VteTerminal *terminal, GValueArray *params)
{
	GdkScreen *gscreen;
	VteScreen *screen;
	GValue *value;
	GtkWidget *widget;
	char buf[128];
	long param, arg1, arg2;
	gint width, height;
	guint i;
	GtkAllocation allocation;

	widget = &terminal->widget;
	screen = terminal->pvt->screen;

	for (i = 0; ((params != NULL) && (i < params->n_values)); i++) {
		arg1 = arg2 = -1;
		if (i + 1 < params->n_values) {
			value = g_value_array_get_nth(params, i + 1);
			if (G_VALUE_HOLDS_LONG(value)) {
				arg1 = g_value_get_long(value);
			}
		}
		if (i + 2 < params->n_values) {
			value = g_value_array_get_nth(params, i + 2);
			if (G_VALUE_HOLDS_LONG(value)) {
				arg2 = g_value_get_long(value);
			}
		}
		value = g_value_array_get_nth(params, i);
		if (!G_VALUE_HOLDS_LONG(value)) {
			continue;
		}
		param = g_value_get_long(value);
		switch (param) {
		case 1:
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Deiconifying window.\n");
			vte_terminal_emit_deiconify_window(terminal);
			break;
		case 2:
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Iconifying window.\n");
			vte_terminal_emit_iconify_window(terminal);
			break;
		case 3:
			if ((arg1 != -1) && (arg2 != -2)) {
				_vte_debug_print(VTE_DEBUG_PARSE,
						"Moving window to "
						"%ld,%ld.\n", arg1, arg2);
				vte_terminal_emit_move_window(terminal,
							      arg1, arg2);
				i += 2;
			}
			break;
		case 4:
			if ((arg1 != -1) && (arg2 != -1)) {
				_vte_debug_print(VTE_DEBUG_PARSE,
						"Resizing window "
						"(to %ldx%ld pixels).\n",
						arg2, arg1);
				vte_terminal_emit_resize_window(terminal,
								arg2 +
								terminal->pvt->inner_border.left +
								terminal->pvt->inner_border.right,
								arg1 +
								terminal->pvt->inner_border.top +
								terminal->pvt->inner_border.bottom);
				i += 2;
			}
			break;
		case 5:
			_vte_debug_print(VTE_DEBUG_PARSE, "Raising window.\n");
			vte_terminal_emit_raise_window(terminal);
			break;
		case 6:
			_vte_debug_print(VTE_DEBUG_PARSE, "Lowering window.\n");
			vte_terminal_emit_lower_window(terminal);
			break;
		case 7:
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Refreshing window.\n");
			_vte_invalidate_all(terminal);
			vte_terminal_emit_refresh_window(terminal);
			break;
		case 8:
			if ((arg1 != -1) && (arg2 != -1)) {
				_vte_debug_print(VTE_DEBUG_PARSE,
						"Resizing window "
						"(to %ld columns, %ld rows).\n",
						arg2, arg1);
				vte_terminal_emit_resize_window(terminal,
								arg2 * terminal->char_width +
								terminal->pvt->inner_border.left +
								terminal->pvt->inner_border.right,
								arg1 * terminal->char_height +
								terminal->pvt->inner_border.top +
								terminal->pvt->inner_border.bottom);
				i += 2;
			}
			break;
		case 9:
			switch (arg1) {
			case 0:
				_vte_debug_print(VTE_DEBUG_PARSE,
						"Restoring window.\n");
				vte_terminal_emit_restore_window(terminal);
				break;
			case 1:
				_vte_debug_print(VTE_DEBUG_PARSE,
						"Maximizing window.\n");
				vte_terminal_emit_maximize_window(terminal);
				break;
			default:
				break;
			}
			i++;
			break;
		case 11:
			/* If we're unmapped, then we're iconified. */
			g_snprintf(buf, sizeof(buf),
				   _VTE_CAP_CSI "%dt",
				   1 + !gtk_widget_get_mapped(widget));
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Reporting window state %s.\n",
					gtk_widget_get_mapped(widget) ?
					"non-iconified" : "iconified");
			vte_terminal_feed_child(terminal, buf, -1);
			break;
		case 13:
			/* Send window location, in pixels. */
			gdk_window_get_origin(gtk_widget_get_window(widget),
					      &width, &height);
			g_snprintf(buf, sizeof(buf),
				   _VTE_CAP_CSI "3;%d;%dt",
				   width + terminal->pvt->inner_border.left,
                                   height + terminal->pvt->inner_border.top);
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Reporting window location"
					"(%d++,%d++).\n",
					width, height);
			vte_terminal_feed_child(terminal, buf, -1);
			break;
		case 14:
			/* Send window size, in pixels. */
			gtk_widget_get_allocation(widget, &allocation);
			g_snprintf(buf, sizeof(buf),
				   _VTE_CAP_CSI "4;%d;%dt",
				   allocation.height -
                                       (terminal->pvt->inner_border.top +
                                        terminal->pvt->inner_border.bottom),
				   allocation.width -
                                       (terminal->pvt->inner_border.left +
                                        terminal->pvt->inner_border.right));
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Reporting window size "
					"(%dx%dn",
					width - (terminal->pvt->inner_border.left + terminal->pvt->inner_border.right),
					height - (terminal->pvt->inner_border.top + terminal->pvt->inner_border.bottom));
			vte_terminal_feed_child(terminal, buf, -1);
			break;
		case 18:
			/* Send widget size, in cells. */
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Reporting widget size.\n");
			g_snprintf(buf, sizeof(buf),
				   _VTE_CAP_CSI "8;%ld;%ldt",
				   terminal->row_count,
				   terminal->column_count);
			vte_terminal_feed_child(terminal, buf, -1);
			break;
		case 19:
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Reporting screen size.\n");
			gscreen = gtk_widget_get_screen(widget);
			height = gdk_screen_get_height(gscreen);
			width = gdk_screen_get_width(gscreen);
			g_snprintf(buf, sizeof(buf),
				   _VTE_CAP_CSI "9;%ld;%ldt",
				   height / terminal->char_height,
				   width / terminal->char_width);
			vte_terminal_feed_child(terminal, buf, -1);
			break;
		case 20:
			/* Report a static icon title, since the real
			   icon title should NEVER be reported, as it
			   creates a security vulnerability.  See
			   http://marc.info/?l=bugtraq&m=104612710031920&w=2
			   and CVE-2003-0070. */
			_vte_debug_print(VTE_DEBUG_PARSE,
				"Reporting fake icon title.\n");
			/* never use terminal->icon_title here! */
			g_snprintf (buf, sizeof (buf),
				    _VTE_CAP_OSC "LTerminal" _VTE_CAP_ST);
			vte_terminal_feed_child(terminal, buf, -1);
			break;
		case 21:
			/* Report a static window title, since the real
			   window title should NEVER be reported, as it
			   creates a security vulnerability.  See
			   http://marc.info/?l=bugtraq&m=104612710031920&w=2
			   and CVE-2003-0070. */
			_vte_debug_print(VTE_DEBUG_PARSE,
					"Reporting fake window title.\n");
			/* never use terminal->window_title here! */
			g_snprintf (buf, sizeof (buf),
				    _VTE_CAP_OSC "lTerminal" _VTE_CAP_ST);
			vte_terminal_feed_child(terminal, buf, -1);
			break;
		default:
			if (param >= 24) {
				_vte_debug_print(VTE_DEBUG_PARSE,
						"Resizing to %ld rows.\n",
					       	param);
				/* Resize to the specified number of
				 * rows. */
				vte_terminal_emit_resize_window(terminal,
								terminal->column_count * terminal->char_width +
                                                                terminal->pvt->inner_border.left +
                                                                terminal->pvt->inner_border.right,
								param * terminal->char_height +
								terminal->pvt->inner_border.top +
                                                                terminal->pvt->inner_border.bottom);
			}
			break;
		}
	}
}

/* Change the color of the cursor */
static void
vte_sequence_handler_change_cursor_color (VteTerminal *terminal, GValueArray *params)
{
	gchar *name = NULL;
	GValue *value;
	GdkColor color;

	if (params != NULL && params->n_values > 0) {
		value = g_value_array_get_nth (params, 0);

		if (G_VALUE_HOLDS_STRING (value))
			name = g_value_dup_string (value);
		else if (G_VALUE_HOLDS_POINTER (value))
			name = vte_ucs4_to_utf8 (terminal, g_value_get_pointer (value));

		if (! name)
			return;

		if (vte_parse_color (name, &color))
			vte_terminal_set_color_cursor (terminal, &color);
		else if (strcmp (name, "?") == 0) {
			gchar buf[128];
			g_snprintf (buf, sizeof (buf),
				    _VTE_CAP_OSC "12;rgb:%04x/%04x/%04x" BEL,
				    terminal->pvt->palette[VTE_CUR_BG].red,
				    terminal->pvt->palette[VTE_CUR_BG].green,
				    terminal->pvt->palette[VTE_CUR_BG].blue);
			vte_terminal_feed_child (terminal, buf, -1);
		}

		g_free (name);
	}
}


/* Lookup tables */

#define VTE_SEQUENCE_HANDLER(name) name

static const struct vteseq_2_struct *
vteseq_2_lookup (register const char *str, register unsigned int len);
#include"vteseq-2.c"

static const struct vteseq_n_struct *
vteseq_n_lookup (register const char *str, register unsigned int len);
#include"vteseq-n.c"

#undef VTE_SEQUENCE_HANDLER

static VteTerminalSequenceHandler
_vte_sequence_get_handler (const char *name)
{
	int len = strlen (name);

	if (G_UNLIKELY (len < 2)) {
		return NULL;
	} else if (len == 2) {
		const struct vteseq_2_struct *seqhandler;
		seqhandler = vteseq_2_lookup (name, 2);
		return seqhandler ? seqhandler->handler : NULL;
	} else {
		const struct vteseq_n_struct *seqhandler;
		seqhandler = vteseq_n_lookup (name, len);
		return seqhandler ? seqhandler->handler : NULL;
	}
}


/* Handle a terminal control sequence and its parameters. */
void
_vte_terminal_handle_sequence(VteTerminal *terminal,
			      const char *match_s,
			      GQuark match G_GNUC_UNUSED,
			      GValueArray *params)
{
	VteTerminalSequenceHandler handler;

	_VTE_DEBUG_IF(VTE_DEBUG_PARSE)
		display_control_sequence(match_s, params);

	/* Find the handler for this control sequence. */
	handler = _vte_sequence_get_handler (match_s);

	if (handler != NULL) {
		/* Let the handler handle it. */
		handler (terminal, params);
	} else {
		_vte_debug_print (VTE_DEBUG_MISC,
				  "No handler for control sequence `%s' defined.\n",
				  match_s);
	}
}

/* a special strcmp that treats any character in
 * its third argument (plus '\0') as end of
 * string.
 *
 *  we have to be a little bit careful, however.
 *  note that '=' < 'A' < '|' and consider the
 *  following three strings with "=|" given as
 *  the 3rd argument:
 *
 *  foo=
 *  fooA
 *  foo|
 *
 *  if we just do the normal *a - *b thing when
 *  the strings don't match then we will find
 *
 *      "foo=" < "fooA"
 *
 *  and
 *
 *      "foo|" > "fooA"
 *
 *  but of course,
 *
 *      "foo=" == "foo|"
 *
 *  which means that our ordering isn't proper
 *  which may cause odd things to happen inside of
 *  the tree.  for this reason, all of the
 *  terminating characters are treated as '\0' for
 *  purposes of deciding greater or less than.
 *
 *  note: if anything in this file should be
 *        micro-optimised then it is probably
 *        this function!
 */
static int
_vte_termcap_strcmp (const char *a,
                     const char *b,
                     const char *enders)
{
  /* note: strchr on '\0' returns the
   * end of the string (not NULL)
   */

  while (!strchr (enders, *a))
  {
    if (*a != *b)
    {
      /* we're in the loop so we know that *a is not a terminator.
       * but maybe *b is?
       */
      if (strchr (enders, *b))
        return *a - '\0';
      else
        /* b is not a terminator.  proceed normally. */
        return *a - *b;
    }
    a++;
    b++;
  }

  /* *a is a terminator for sure, but maybe *b is too. */
  if (strchr (enders, *b))
    /* b is too, so we have a match. */
    return 0;

  /* else, since *a is a terminator character and *b is not, *a is
   * less than *b. */
  return -1;
}

/*
 * --- routines for searching the tree ---
 */
static const char *
_vte_termcap_find_start (VteTermcap *termcap,
                         const char *tname,
                         const char *cap)
{
  const char *contents;
  const char *start;
  char *chain;

  /* find the terminal */
  contents = g_tree_lookup (termcap->tree, tname);

  if (contents == NULL)
    return NULL;

  start = contents;
  while (contents != termcap->end)
  {
    if (*contents == '\\' &&
        contents + 1 != termcap->end &&
        contents[1] == '\n')
    {
      /* we've hit \ at the end of a line.  skip. */
      contents++;
    }
    else if (*contents == ':' || *contents == '\n')
    {
      if (!_vte_termcap_strcmp (start, cap, "=#:\n"))
        return start;

      start = contents + 1;
      if (*contents == '\n')
        break;
    }

    contents++;
  }

  /* else, try to find it in the term listed in our 'tc' entry.
   * obviously, don't recurse when we're trying to find "tc"
   * itself else we infinite loop.
   */
  if (!strcmp (cap, "tc"))
    return NULL;

  chain = _vte_termcap_find_string (termcap, tname, "tc");
  if (chain[0])
    start = _vte_termcap_find_start (termcap, chain, cap);
  g_free (chain);

  return start;
}

static int
_vte_termcap_unescape_string(const char *string, char *result)
{
  int value = -1;
  int length = 0;

  while (TRUE)
  {
    /* Each time through the loop puts a value into 'value' if it
     * wants to have it written into the string.  We do the write
     * here because it is complicated (check for NULL result, etc)
     *
     * We finish and return the length whenb value is 0.
     */
    if (value >= 0)
    {
      if (result != NULL)
        result[length] = value;
      length++;

      if (value == 0)
        return length;

      value = -1;
    }

    /* Now, decide what value should be for the next iteration.
     * Here, "continue;" means "I've possibly set 'value' and I want
     * to continue looking at the string starting at the next
     * character pointed to by 'string'.
     */
    switch (*string++)
    {
      case '\n':
      case '\0':
      case ':':
        value = 0;
        continue;

      case '\\':
        switch (*string++)
        {
          case '\n':
            while (*string == ' ' || *string == '\t')
              string++;
            continue;
          case 'E':
          case 'e':
            value = 27;
            continue;
          case 'n':
            value = 10;
            continue;
          case 'r':
            value = 13;
            continue;
          case 't':
            value = 8;
            continue;
          case 'b':
            value = 9;
            continue;
          case 'f':
            value = 12;
            continue;
          case '0':
          case '1':
            value = strtol(string - 1, (void *) &string, 8);
            continue;
          default:
            /* invalid escape sequence.  write the \ and
             * continue as if we never saw it...
             */
            value = '\\';
            string--;
            continue;
        }

      case '^':
        if (*string >= 'A' && *string <= 'Z')
        {
          value = *string++ - '@';
          break;
        }

        /* else, invalid control sequnce.  write the ^
         * and continue as if we never saw it...
         */

      default:
        /* else, the value is this character and the pointer has
         * already been advanced to the next character. */
        value = string[-1];
    }
  }
}

char *
_vte_termcap_find_string_length (VteTermcap *termcap,
                                 const char *tname,
                                 const char *cap,
                                 gssize *length)
{
  const char *result = _vte_termcap_find_start (termcap, tname, cap);
  char *string;

  if (result == NULL || result[2] != '=')
  {
    *length = 0;
    return g_strdup ("");
  }

  result += 3;

  *length = _vte_termcap_unescape_string (result, NULL);
  string = g_malloc (*length);
  _vte_termcap_unescape_string (result, string);

  (*length)--;

  return string;
}

char *
_vte_termcap_find_string (VteTermcap *termcap,
                          const char *tname,
                          const char *cap)
{
  gssize length;

  return _vte_termcap_find_string_length (termcap, tname, cap, &length);
}

long
_vte_termcap_find_numeric (VteTermcap *termcap,
                           const char *tname,
                           const char *cap)
{
  const char *result = _vte_termcap_find_start (termcap, tname, cap);
  long value;
  char *end;

  if (result == NULL || result[2] != '#')
    return 0;

  result += 3;

  value = strtol (result, &end, 0);
  if (*end != ':' && *end != '\0' && *end != '\n')
    return 0;

  return value;
}

gboolean
_vte_termcap_find_boolean (VteTermcap *termcap,
                           const char *tname,
                           const char *cap)
{
  const char *result = _vte_termcap_find_start (termcap, tname, cap);

  if (result == NULL)
    return FALSE;

  result += 2;

  if (*result != ':' && *result != '\0' && *result != '\n')
    return FALSE;

  return TRUE;
}

/*
 * --- routines for building the tree from the file ---
 */
static void
_vte_termcap_parse_entry (GTree *termcap, const char **cnt, const char *end)
{
  gboolean seen_content;
  const char *contents;
  const char *start;
  const char *caps;

  contents = *cnt;

  /* look for the start of the capabilities.
   */
  caps = contents;
  while (caps != end)
    if (*caps == ':')
      break;
    else
      caps++;

  if (*caps != ':')
    return;

  /* parse all of the aliases and insert one item into the termcap
   * tree for each alias, pointing it to our caps.
   */
  seen_content = FALSE;
  start = contents;
  while (contents != end)
  {
    /* 
    if (contents == end)
    {
       * we can't deal with end of file directly following a
       * terminal name without any delimiters or even a newline.
       * but honestly, what did they expect?  end of file without
       * newline in the middle of a terminal alias with no
       * capability definitions?  i'll doubt they notice that
       * anything is missing.
    }
    */

    if (*contents == '\\' && contents + 1 != end && contents[1] == '\n')
    {
      /* we've hit \ at the end of a line.  skip. */
      contents++;
    }
    else if (*contents == '|' || *contents == ':' || *contents == '\n')
    {
      /* we wait to find the terminator before putting anything in
       * the tree to ensure that _vte_termcap_strcmp will always
       * terminate.  we also only add the alias if we've seen
       * actual characters (not just spaces, continuations, etc)
       */
      if (seen_content)
        g_tree_insert (termcap, (gpointer) start, (gpointer) caps);
      start = contents + 1;
      seen_content = FALSE;

      /* we've either hit : and need to move on to capabilities or
       * end of line and then there are no capabilities for this
       * terminal.  any aliases have already been added to the tree
       * so we can just move on.  if it was '\n' then the next while
       * loop will exit immediately.
       */
      if (*contents == ':' || *contents == '\n')
        break;
    }
    else if (*contents != ' ' && *contents != '\t')
      seen_content = TRUE;

    contents++;
  }

  /* we've processed all of the aliases.  now skip past the capabilities
   * so that we're ready to go on the next entry. */
  while (contents != end)
  {
    if (*contents == '\\' && contents + 1 != end && contents[1] == '\n')
    {
      /* we've hit \ at the end of a line.  skip. */
      contents++;
    }
    else if (*contents == '\n')
      break;

    contents++;
  }

  *cnt = contents;
}

static GTree *
_vte_termcap_parse_file (const char *contents, int length)
{
  const char *end = contents + length;
  GTree *termcap;

  /* this tree contains terminal alias names which in a proper
   * termcap file will always be followed by : or |.  we
   * include \n to be extra-permissive. \0 is here to allow
   * us to notice the end of strings passed to us by vte.
   */
  termcap = g_tree_new_full ((GCompareDataFunc) _vte_termcap_strcmp,
                             (gpointer)":|\n", NULL, NULL);

  while (contents != end)
  {
    switch (*contents++)
    {
      /* comments */
      case '#':
        /* eat up to (but not) the \n */
        while (contents != end && *contents != '\n')
          contents++;

      /* whitespace */
      case ' ':
      case '\t':
      case '\n':
        continue;

      default:
        /* bring back the character */
        contents--;

        /* parse one entry (ie: one line) */
        _vte_termcap_parse_entry (termcap, &contents, end);
    }
  }

  return termcap;
}

static VteTermcap *
_vte_termcap_create (const char *filename)
{
  const char *contents;
  VteTermcap *termcap;
  GMappedFile *file;
  int length;

  file = g_mapped_file_new (filename, FALSE, NULL);
  if (file == NULL)
    return NULL;

  contents = g_mapped_file_get_contents (file);
  length = g_mapped_file_get_length (file);

  termcap = g_slice_new (VteTermcap);
  termcap->file = file;
  termcap->tree = _vte_termcap_parse_file (contents, length);
  termcap->end = contents + length;

  return termcap;
}

static void
_vte_termcap_destroy (VteTermcap *termcap)
{
  if (!termcap)
    return;
  g_tree_destroy (termcap->tree);
  g_mapped_file_unref (termcap->file);
  g_slice_free (VteTermcap, termcap);
}

/*
 * --- cached interface to create/destroy termcap trees ---
 */
static GStaticMutex _vte_termcap_mutex = G_STATIC_MUTEX_INIT;
static GCache *_vte_termcap_cache = NULL;

VteTermcap *
_vte_termcap_new(const char *filename)
{
  VteTermcap *result;

  g_static_mutex_lock (&_vte_termcap_mutex);

  if (_vte_termcap_cache == NULL)
    _vte_termcap_cache = g_cache_new((GCacheNewFunc) _vte_termcap_create,
                                     (GCacheDestroyFunc) _vte_termcap_destroy,
                                     (GCacheDupFunc) g_strdup,
                                     (GCacheDestroyFunc) g_free,
                                     g_str_hash, g_direct_hash, g_str_equal);

  result = g_cache_insert (_vte_termcap_cache, (gpointer) filename);

  g_static_mutex_unlock (&_vte_termcap_mutex);

  return result;
}

void
_vte_termcap_free (VteTermcap *termcap)
{
  g_static_mutex_lock (&_vte_termcap_mutex);
  g_cache_remove (_vte_termcap_cache, termcap);
  g_static_mutex_unlock (&_vte_termcap_mutex);
}

#ifdef TERMCAP_MAIN
#include <stdio.h>

int
main (int argc, char **argv)
{
  VteTermcap *tc;
  char *str;
  gssize len;
  int i;

  if (argc < 4)
  {
    g_printerr("vtetc /path/to/termcap termname attrs...\n"
                     "  where attrs are\n"
                     "    :xx for boolean\n"
                     "    =xx for string\n"
                     "    +xx for string displayed in hex\n"
                     "    #xx for numeric\n");
    return 1;
  }

  tc = _vte_termcap_new (argv[1]);

  if (tc == NULL)
  {
    perror ("open");
    return 1;
  }

  for (i = 3; i < argc; i++)
  {
    printf ("%s -> ", argv[i]);

    switch (argv[i][0])
    {
      case ':':
        printf ("%s\n", _vte_termcap_find_boolean (tc, argv[2], argv[i] + 1)?
                        "true" : "false");
        break;

      case '=':
      case '+':
        str = _vte_termcap_find_string_length (tc, argv[2], argv[i] + 1, &len);

        if (argv[i][0] == '=')
          printf ("'%s' (%d)\n", str, (int)len);
        else
        {
          int i;

          for (i = 0; str[i]; i++)
            printf ("%02x", str[i]);
          printf (" (%d) \n", (int)len);
        }
        g_free (str);
        break;

      case '#':
        printf ("%ld\n", _vte_termcap_find_numeric (tc, argv[2], argv[i] + 1));
        break;

      default:
        g_printerr("unrecognised type '%c'\n", argv[i][0]);
    }
  }

  _vte_termcap_free(tc);

  return 0;
}
#endif

static void
_vte_table_arginfo_head_init(struct _vte_table_arginfo_head *head)
{
	head->list = NULL;
	head->stack_allocated = 0;
}
static inline struct _vte_table_arginfo*
_vte_table_arginfo_alloc(struct _vte_table_arginfo_head *head)
{
	struct _vte_table_arginfo *info;
	if (G_LIKELY (head->stack_allocated < G_N_ELEMENTS(head->stack))) {
		info = &head->stack[head->stack_allocated++];
	} else {
		info = g_slice_new (struct _vte_table_arginfo);
	}
	info->next = head->list;
	head->list = info;
	return info;
}
static void
_vte_table_arginfo_head_revert(struct _vte_table_arginfo_head *head, struct _vte_table_arginfo *last)
{
	struct _vte_table_arginfo *info;
	info = head->list;
	head->list = last->next;
	if (last >= &head->stack[0] &&
			last < &head->stack[G_N_ELEMENTS(head->stack)]){
		head->stack_allocated = last - &head->stack[0];
	}
	do {
		struct _vte_table_arginfo *next = info->next;
		if (info >= &head->stack[0] &&
				info < &head->stack[G_N_ELEMENTS(head->stack)]){
			break;
		}
		g_slice_free(struct _vte_table_arginfo, info);
		if (info == last) {
			break;
		}
		info = next;
	}while (TRUE);
}
static struct _vte_table_arginfo *
_vte_table_arginfo_head_reverse(struct _vte_table_arginfo_head *head)
{
	struct _vte_table_arginfo *prev = NULL;
	while (head->list) {
		struct _vte_table_arginfo *next = head->list->next;

		head->list->next = prev;

		prev = head->list;
		head->list = next;
	}
	return prev;
}
static void
_vte_table_arginfo_head_finalize(struct _vte_table_arginfo_head *head)
{
	struct _vte_table_arginfo *info, *next;
	for (info = head->list; info != NULL; info = next) {
		next = info->next;
		if (info >= &head->stack[0] &&
				info < &head->stack[G_N_ELEMENTS(head->stack)]){
			continue;
		}
		g_slice_free(struct _vte_table_arginfo, info);
	}
}

/* Create an empty, one-level table. */
struct _vte_table *
_vte_table_new(void)
{
	struct _vte_table * ret;
	ret = g_slice_new0(struct _vte_table);
	ret->impl.klass = &_vte_matcher_table;
	return ret;
}

static struct _vte_table **
_vte_table_literal_new(void)
{
	return g_new0(struct _vte_table *, VTE_TABLE_MAX_LITERAL);
}

/* Free a table. */
void
_vte_table_free(struct _vte_table *table)
{
	unsigned int i;
	if (table->table != NULL) {
		for (i = 0; i < VTE_TABLE_MAX_LITERAL; i++) {
			if (table->table[i] != NULL) {
				_vte_table_free(table->table[i]);
			}
		}
		g_free(table->table);
	}
	if (table->table_string != NULL) {
		_vte_table_free(table->table_string);
	}
	if (table->table_number != NULL) {
		_vte_table_free(table->table_number);
	}
	if (table->table_number_list != NULL) {
		_vte_table_free(table->table_number_list);
	}
	if (table->original_length == 0) {
		g_assert(table->original == NULL);
	} else {
		g_assert(table->original != NULL);
	}
	if (table->original != NULL) {
		g_free(table->original);
	}
	g_slice_free(struct _vte_table, table);
}

/* Add a string to the tree with the given increment value. */
static void
_vte_table_addi(struct _vte_table *table,
		const unsigned char *original, gssize original_length,
		const char *pattern, gssize length,
		const char *result, GQuark quark, int inc)
{
	int i;
	guint8 check;
	struct _vte_table *subtable;

	if (original_length == -1) {
		original_length = strlen((char *) original);
	}
	if (length == -1) {
		length = strlen(pattern);
	}

	/* If this is the terminal node, set the result. */
	if (length == 0) {
		if (table->result != NULL)
			_vte_debug_print (VTE_DEBUG_PARSE, 
					  "`%s' and `%s' are indistinguishable.\n",
					  table->result, result);

		table->resultq = g_quark_from_string(result);
		table->result = g_quark_to_string(table->resultq);
		if (table->original != NULL) {
			g_free(table->original);
		}
		table->original = g_memdup(original, original_length);
		table->original_length = original_length;
		table->increment = inc;
		return;
	}

	/* All of the interesting arguments begin with '%'. */
	if (pattern[0] == '%') {
		/* Handle an increment. */
		if (pattern[1] == 'i') {
			_vte_table_addi(table, original, original_length,
					pattern + 2, length - 2,
					result, quark, inc + 1);
			return;
		}

		/* Handle numeric parameters. */
		if ((pattern[1] == 'd') ||
		    (pattern[1] == '2') ||
		    (pattern[1] == '3')) {
			/* Create a new subtable. */
			if (table->table_number == NULL) {
				subtable = _vte_table_new();
				table->table_number = subtable;
			} else {
				subtable = table->table_number;
			}
			/* Add the rest of the string to the subtable. */
			_vte_table_addi(subtable, original, original_length,
					pattern + 2, length - 2,
					result, quark, inc);
			return;
		}

		/* Handle variable-length parameters. */
		if ((pattern[1] == 'm') ||
		    (pattern[1] == 'M')) {
			/* Build the "new" original using the initial portion
			 * of the original string and what's left after this
			 * specifier. */
			if (pattern[1] == 'm') {
				int initial;
				GByteArray *b;

				initial = original_length - length;
				/* 0 args; we use 'M' to signal that zero is
				 * not allowed.  */
				b = g_byte_array_new();
				g_byte_array_set_size(b, 0);
				g_byte_array_append(b, original, initial);
				g_byte_array_append(b, (const guint8*)pattern + 2, length - 2);
				_vte_table_addi(table, b->data, b->len,
						(const char *)b->data + initial,
						b->len - initial,
						result, quark, inc);
				g_byte_array_free(b, TRUE);
			}
			/* Create a new subtable. */
			if (table->table_number_list == NULL) {
				subtable = _vte_table_new();
				table->table_number_list = subtable;
			} else {
				subtable = table->table_number_list;
			}
			/* Add the rest of the string to the subtable. */
			_vte_table_addi(subtable, original, original_length,
					pattern + 2, length - 2,
					result, quark, inc);
			return;
		}

		/* Handle string parameters. */
		if (pattern[1] == 's') {
			/* It must have a terminator. */
			g_assert(length >= 3);
			/* Create a new subtable. */
			if (table->table_string == NULL) {
				subtable = _vte_table_new();
				table->table_string = subtable;
			} else {
				subtable = table->table_string;
			}
			/* Add the rest of the string to the subtable. */
			_vte_table_addi(subtable, original, original_length,
					pattern + 2, length - 2,
					result, quark, inc);
			return;
		}

		/* Handle an escaped '%'. */
		if (pattern[1] == '%') {
			/* Create a new subtable. */
			if (table->table == NULL) {
				table->table = _vte_table_literal_new();
				subtable = _vte_table_new();
				table->table['%'] = subtable;
			} else
			if (table->table['%'] == NULL) {
				subtable = _vte_table_new();
				table->table['%'] = subtable;
			} else {
				subtable = table->table['%'];
			}
			/* Add the rest of the string to the subtable. */
			_vte_table_addi(subtable, original, original_length,
					pattern + 2, length - 2,
					result, quark, inc);
			return;
		}

		/* Handle a parameter character. */
		if (pattern[1] == '+') {
			/* It must have an addend. */
			g_assert(length >= 3);
			/* Fill in all of the table entries above the given
			 * character value. */
			for (i = pattern[2]; i < VTE_TABLE_MAX_LITERAL; i++) {
				/* Create a new subtable. */
				if (table->table == NULL) {
					table->table = _vte_table_literal_new();
					subtable = _vte_table_new();
					table->table[i] = subtable;
				} else
				if (table->table[i] == NULL) {
					subtable = _vte_table_new();
					table->table[i] = subtable;
				} else {
					subtable = table->table[i];
				}
				/* Add the rest of the string to the subtable. */
				_vte_table_addi(subtable,
						original, original_length,
						pattern + 3, length - 3,
						result, quark, inc);
			}
			/* Also add a subtable for higher characters. */
			if (table->table == NULL) {
				table->table = _vte_table_literal_new();
				subtable = _vte_table_new();
				table->table[0] = subtable;
			} else
			if (table->table[0] == NULL) {
				subtable = _vte_table_new();
				table->table[0] = subtable;
			} else {
				subtable = table->table[0];
			}
			/* Add the rest of the string to the subtable. */
			_vte_table_addi(subtable, original, original_length,
					pattern + 3, length - 3,
					result, quark, inc);
			return;
		}
	}

	/* A literal (or an unescaped '%', which is also a literal). */
	check = (guint8) pattern[0];
	g_assert(check < VTE_TABLE_MAX_LITERAL);
	if (table->table == NULL) {
		table->table = _vte_table_literal_new();
		subtable = _vte_table_new();
		table->table[check] = subtable;
	} else
	if (table->table[check] == NULL) {
		subtable = _vte_table_new();
		table->table[check] = subtable;
	} else {
		subtable = table->table[check];
	}

	/* Add the rest of the string to the subtable. */
	_vte_table_addi(subtable, original, original_length,
			pattern + 1, length - 1,
			result, quark, inc);
}

/* Add a string to the matching tree. */
void
_vte_table_add(struct _vte_table *table,
	       const char *pattern, gssize length,
	       const char *result, GQuark quark)
{
	_vte_table_addi(table,
			(const unsigned char *) pattern, length,
			pattern, length,
			result, quark, 0);
}

/* Match a string in a subtree. */
static const char *
_vte_table_matchi(struct _vte_table *table,
		  const gunichar *candidate, gssize length,
		  const char **res, const gunichar **consumed, GQuark *quark,
		  unsigned char **original, gssize *original_length,
		  struct _vte_table_arginfo_head *params)
{
	int i = 0;
	struct _vte_table *subtable = NULL;
	struct _vte_table_arginfo *arginfo;

	/* Check if this is a result node. */
	if (table->result != NULL) {
		*consumed = candidate;
		*original = table->original;
		*original_length = table->original_length;
		*res = table->result;
		*quark = table->resultq;
		return table->result;
	}

	/* If we're out of data, but we still have children, return the empty
	 * string. */
	if (G_UNLIKELY (length == 0)) {
		*consumed = candidate;
		return "";
	}

	/* Check if this node has a string disposition. */
	if (table->table_string != NULL) {
		/* Iterate over all non-terminator values. */
		subtable = table->table_string;
		for (i = 0; i < length; i++) {
			if ((subtable->table != NULL) &&
			    (subtable->table[_vte_table_map_literal(candidate[i])] != NULL)) {
				break;
			}
		}
		/* Save the parameter info. */
		arginfo = _vte_table_arginfo_alloc(params);
		arginfo->type = _vte_table_arg_string;
		arginfo->start = candidate;
		arginfo->length = i;
		/* Continue. */
		return _vte_table_matchi(subtable, candidate + i, length - i,
					 res, consumed, quark,
					 original, original_length, params);
	}

	/* Check if this could be a list. */
	if ((_vte_table_is_numeric_list(candidate[0])) &&
	    (table->table_number_list != NULL)) {
		const char *local_result;

		subtable = table->table_number_list;
		/* Iterate over all numeric characters and ';'. */
		for (i = 1; i < length; i++) {
			if (!_vte_table_is_numeric_list(candidate[i])) {
				break;
			}
		}
		/* Save the parameter info. */
		arginfo = _vte_table_arginfo_alloc(params);
		arginfo->type = _vte_table_arg_number;
		arginfo->start = candidate;
		arginfo->length = i;

		/* Try and continue. */
		local_result = _vte_table_matchi(subtable,
					 candidate + i, length - i,
					 res, consumed, quark,
					 original, original_length,
					 params);
		if (local_result != NULL) {
			return local_result;
		}
		_vte_table_arginfo_head_revert (params, arginfo);

		/* try again */
	}

	/* Check if this could be a number. */
	if ((_vte_table_is_numeric(candidate[0])) &&
	    (table->table_number != NULL)) {
		subtable = table->table_number;
		/* Iterate over all numeric characters. */
		for (i = 1; i < length; i++) {
			if (!_vte_table_is_numeric(candidate[i])) {
				break;
			}
		}
		/* Save the parameter info. */
		arginfo = _vte_table_arginfo_alloc(params);
		arginfo->type = _vte_table_arg_number;
		arginfo->start = candidate;
		arginfo->length = i;
		/* Continue. */
		return _vte_table_matchi(subtable, candidate + i, length - i,
					 res, consumed, quark,
					 original, original_length, params);
	}

	/* Check for an exact match. */
	if ((table->table != NULL) &&
	    (table->table[_vte_table_map_literal(candidate[0])] != NULL)) {
		subtable = table->table[_vte_table_map_literal(candidate[0])];
		/* Save the parameter info. */
		arginfo = _vte_table_arginfo_alloc(params);
		arginfo->type = _vte_table_arg_char;
		arginfo->start = candidate;
		arginfo->length = 1;
		/* Continue. */
		return _vte_table_matchi(subtable, candidate + 1, length - 1,
					 res, consumed, quark,
					 original, original_length, params);
	}

	/* If there's nothing else to do, then we can't go on.  Keep track of
	 * where we are. */
	*consumed = candidate;
	return NULL;
}

static void
_vte_table_extract_numbers(GValueArray **array,
			   struct _vte_table_arginfo *arginfo, long increment)
{
	GValue value = {0,};
	gssize i;

	g_value_init(&value, G_TYPE_LONG);
	i = 0;
	do {
		long total = 0;
		for (; i < arginfo->length && arginfo->start[i] != ';'; i++) {
			gint v = g_unichar_digit_value (arginfo->start[i]);
			total *= 10;
			total += v == -1 ?  0 : v;
		}
		if (G_UNLIKELY (*array == NULL)) {
			*array = g_value_array_new(1);
		}
		g_value_set_long(&value, total);
		g_value_array_append(*array, &value);
	} while (i++ < arginfo->length);
	g_value_unset(&value);
}

static void
_vte_table_extract_string(GValueArray **array,
			  struct _vte_table_arginfo *arginfo)
{
	GValue value = {0,};
	gunichar *ptr;
	guint i;

	ptr = g_new(gunichar, arginfo->length + 1);
	for (i = 0; i < arginfo->length; i++) {
		ptr[i] = arginfo->start[i] & ~VTE_ISO2022_ENCODED_WIDTH_MASK;
	}
	ptr[i] = '\0';
	g_value_init(&value, G_TYPE_POINTER);
	g_value_set_pointer(&value, ptr);

	if (G_UNLIKELY (*array == NULL)) {
		*array = g_value_array_new(1);
	}
	g_value_array_append(*array, &value);
	g_value_unset(&value);
}

static void
_vte_table_extract_char(GValueArray **array,
			struct _vte_table_arginfo *arginfo, long increment)
{
	GValue value = {0,};

	g_value_init(&value, G_TYPE_LONG);
	g_value_set_long(&value, *(arginfo->start) - increment);

	if (G_UNLIKELY (*array == NULL)) {
		*array = g_value_array_new(1);
	}
	g_value_array_append(*array, &value);
	g_value_unset(&value);
}

/* Check if a string matches something in the tree. */
const char *
_vte_table_match(struct _vte_table *table,
		 const gunichar *candidate, gssize length,
		 const char **res, const gunichar **consumed,
		 GQuark *quark, GValueArray **array)
{
	struct _vte_table *head;
	const gunichar *dummy_consumed;
	const char *dummy_res;
	GQuark dummy_quark;
	GValueArray *dummy_array;
	const char *ret;
	unsigned char *original, *p;
	gssize original_length;
	long increment = 0;
	int i;
	struct _vte_table_arginfo_head params;
	struct _vte_table_arginfo *arginfo;

	/* Clean up extracted parameters. */
	if (G_UNLIKELY (res == NULL)) {
		res = &dummy_res;
	}
	*res = NULL;
	if (G_UNLIKELY (consumed == NULL)) {
		consumed = &dummy_consumed;
	}
	*consumed = candidate;
	if (G_UNLIKELY (quark == NULL)) {
		quark = &dummy_quark;
	}
	*quark = 0;
	if (G_UNLIKELY (array == NULL)) {
		dummy_array = NULL;
		array = &dummy_array;
	}

	/* Provide a fast path for the usual "not a sequence" cases. */
	if (G_LIKELY (length == 0 || candidate == NULL)) {
		return NULL;
	}

	/* If there's no literal path, and no generic path, and the numeric
	 * path isn't available, then it's not a sequence, either. */
	if (table->table == NULL ||
	    table->table[_vte_table_map_literal(candidate[0])] == NULL) {
		if (table->table_string == NULL) {
			if (table->table_number == NULL ||
					!_vte_table_is_numeric(candidate[0])){
				if (table->table_number_list == NULL ||
					!_vte_table_is_numeric_list(candidate[0])){
					/* No match. */
					return NULL;
				}
			}
		}
	}

	/* Check for a literal match. */
	for (i = 0, head = table; i < length && head != NULL; i++) {
		if (head->table == NULL) {
			head = NULL;
		} else {
			head = head->table[_vte_table_map_literal(candidate[i])];
		}
	}
	if (head != NULL && head->result != NULL) {
		/* Got a literal match. */
		*consumed = candidate + i;
		*res = head->result;
		*quark = head->resultq;
		return *res;
	}

	_vte_table_arginfo_head_init (&params);

	/* Check for a pattern match. */
	ret = _vte_table_matchi(table, candidate, length,
				res, consumed, quark,
				&original, &original_length,
				&params);
	*res = ret;

	/* If we got a match, extract the parameters. */
	if (ret != NULL && ret[0] != '\0' && array != &dummy_array) {
		g_assert(original != NULL);
		p = original;
		arginfo = _vte_table_arginfo_head_reverse (&params);
		do {
			/* All of the interesting arguments begin with '%'. */
			if (p[0] == '%') {
				/* Handle an increment. */
				if (p[1] == 'i') {
					increment++;
					p += 2;
					continue;
				}
				/* Handle an escaped '%'. */
				else if (p[1] == '%') {
					p++;
				}
				/* Handle numeric parameters. */
				else if ((p[1] == 'd') ||
				    (p[1] == '2') ||
				    (p[1] == '3') ||
				    (p[1] == 'm') ||
				    (p[1] == 'M')) {
					_vte_table_extract_numbers(array,
								   arginfo,
								   increment);
					p++;
				}
				/* Handle string parameters. */
				else if (p[1] == 's') {
					_vte_table_extract_string(array,
								  arginfo);
					p++;
				}
				/* Handle a parameter character. */
				else if (p[1] == '+') {
					_vte_table_extract_char(array,
								arginfo,
								p[2]);
					p += 2;
				} else {
					_vte_debug_print (VTE_DEBUG_PARSE,
							  "Invalid termcap sequence %s\n",
							  original);
				}
			} /* else Literal. */
			arginfo = arginfo->next;
		} while (++p < original + original_length && arginfo);
	}

	/* Clean up extracted parameters. */
	_vte_table_arginfo_head_finalize (&params);

	return ret;
}

static void
_vte_table_printi(struct _vte_table *table, const char *lead, int *count)
{
	unsigned int i;
	char *newlead = NULL;

	(*count)++;

	/* Result? */
	if (table->result != NULL) {
		g_printerr("%s = `%s'(%d)\n", lead,
			table->result, table->increment);
	}

	/* Literal? */
	for (i = 1; i < VTE_TABLE_MAX_LITERAL; i++) {
		if ((table->table != NULL) && (table->table[i] != NULL)) {
			if (i < 32) {
				newlead = g_strdup_printf("%s^%c", lead,
							  i + 64);
			} else {
				newlead = g_strdup_printf("%s%c", lead, i);
			}
			_vte_table_printi(table->table[i], newlead, count);
			g_free(newlead);
		}
	}

	/* String? */
	if (table->table_string != NULL) {
		newlead = g_strdup_printf("%s{string}", lead);
		_vte_table_printi(table->table_string,
				  newlead, count);
		g_free(newlead);
	}

	/* Number(+)? */
	if (table->table_number != NULL) {
		newlead = g_strdup_printf("%s{number}", lead);
		_vte_table_printi(table->table_number,
				  newlead, count);
		g_free(newlead);
	}
}

/* Dump out the contents of a tree. */
void
_vte_table_print(struct _vte_table *table)
{
	int count = 0;
	_vte_table_printi(table, "", &count);
	g_printerr("%d nodes = %ld bytes.\n",
		count, (long) count * sizeof(struct _vte_table));
}

#ifdef TABLE_MAIN
/* Return an escaped version of a string suitable for printing. */
static char *
escape(const char *p)
{
	char *tmp;
	GString *ret;
	int i;
	guint8 check;
	ret = g_string_new(NULL);
	for (i = 0; p[i] != '\0'; i++) {
		tmp = NULL;
		check = p[i];
		if (check < 32) {
			tmp = g_strdup_printf("^%c", check + 64);
		} else
		if (check >= 0x80) {
			tmp = g_strdup_printf("{0x%x}", check);
		} else {
			tmp = g_strdup_printf("%c", check);
		}
		g_string_append(ret, tmp);
		g_free(tmp);
	}
	return g_string_free(ret, FALSE);
}

/* Spread out a narrow ASCII string into a wide-character string. */
static gunichar *
make_wide(const char *p)
{
	gunichar *ret;
	guint8 check;
	int i;
	ret = g_malloc((strlen(p) + 1) * sizeof(gunichar));
	for (i = 0; p[i] != 0; i++) {
		check = (guint8) p[i];
		g_assert(check < 0x80);
		ret[i] = check;
	}
	ret[i] = '\0';
	return ret;
}

/* Print the contents of a GValueArray. */
static void
print_array(GValueArray *array)
{
	int i;
	GValue *value;
	if (array != NULL) {
		printf(" (");
		for (i = 0; i < array->n_values; i++) {
			value = g_value_array_get_nth(array, i);
			if (i > 0) {
				printf(", ");
			}
			if (G_VALUE_HOLDS_LONG(value)) {
				printf("%ld", g_value_get_long(value));
			} else
			if (G_VALUE_HOLDS_STRING(value)) {
				printf("\"%s\"", g_value_get_string(value));
			} else
			if (G_VALUE_HOLDS_POINTER(value)) {
				printf("\"%ls\"",
				       (wchar_t*) g_value_get_pointer(value));
			}
		}
		printf(")");
		/* _vte_matcher_free_params_array(array); */
	}
}

int
main(int argc, char **argv)
{
	struct _vte_table *table;
	int i;
	const char *candidates[] = {
		"ABCD",
		"ABCDEF",
		"]2;foo",
		"]3;foo",
		"]3;fook",
		"[3;foo",
		"[3;3m",
		"[3;3mk",
		"[3;3hk",
		"[3;3h",
		"]3;3h",
		"[3;3k",
		"[3;3kj",
		"s",
	};
	const char *result, *p;
	const gunichar *consumed;
	char *tmp;
	gunichar *candidate;
	GQuark quark;
	GValueArray *array;
	g_type_init();
	table = _vte_table_new();
	_vte_table_add(table, "ABCDEFG", 7, "ABCDEFG", 0);
	_vte_table_add(table, "ABCD", 4, "ABCD", 0);
	_vte_table_add(table, "ABCDEFH", 7, "ABCDEFH", 0);
	_vte_table_add(table, "ACDEFH", 6, "ACDEFH", 0);
	_vte_table_add(table, "ACDEF%sJ", 8, "ACDEF%sJ", 0);
	_vte_table_add(table, "ACDEF%i%mJ", 10, "ACDEF%dJ", 0);
	_vte_table_add(table, "[%mh", 5, "move-cursor", 0);
	_vte_table_add(table, "[%d;%d;%dm", 11, "set-graphic-rendition", 0);
	_vte_table_add(table, "[%dm", 5, "set-graphic-rendition", 0);
	_vte_table_add(table, "[m", 3, "set-graphic-rendition", 0);
	_vte_table_add(table, "]3;%s", 7, "set-icon-title", 0);
	_vte_table_add(table, "]4;%s", 7, "set-window-title", 0);
	printf("Table contents:\n");
	_vte_table_print(table);
	printf("\nTable matches:\n");
	for (i = 0; i < G_N_ELEMENTS(candidates); i++) {
		p = candidates[i];
		candidate = make_wide(p);
		array = NULL;
		_vte_table_match(table, candidate, strlen(p),
				 &result, &consumed, &quark, &array);
		tmp = escape(p);
		printf("`%s' => `%s'", tmp, (result ? result : "(NULL)"));
		g_free(tmp);
		print_array(array);
		printf(" (%d chars)\n", (int) (consumed ? consumed - candidate: 0));
		g_free(candidate);
	}
	_vte_table_free(table);
	return 0;
}
#endif

const struct _vte_matcher_class _vte_matcher_table = {
	(_vte_matcher_create_func)_vte_table_new,
	(_vte_matcher_add_func)_vte_table_add,
	(_vte_matcher_print_func)_vte_table_print,
	(_vte_matcher_match_func)_vte_table_match,
	(_vte_matcher_destroy_func)_vte_table_free
};

/* Functions for checking if a particular character is part of a class, and
 * for setting up a structure for use when determining matches. */
static gboolean
char_class_exact_check(gunichar c, struct char_class_data *data)
{
	return (c == data->c) ? TRUE : FALSE;
}
static void
char_class_exact_setup(const gunichar *s, struct char_class_data *data, int inc)
{
	data->c = s[0];
	return;
}
static void
char_class_percent_setup(const gunichar *s, struct char_class_data *data,
			 int inc)
{
	data->c = '%';
	return;
}
static gboolean
char_class_none_extract(const gunichar *s, gsize length,
			struct char_class_data *data, GValueArray *array)
{
	return FALSE;
}

static gboolean
char_class_digit_check(gunichar c, struct char_class_data *data)
{
	switch (c) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			return TRUE;
		default:
			return FALSE;
	}
}
static void
char_class_digit_setup(const gunichar *s, struct char_class_data *data, int inc)
{
	data->inc = inc;
	return;
}
static gboolean
char_class_digit_extract(const gunichar *s, gsize length,
			 struct char_class_data *data, GValueArray *array)
{
	long ret = 0;
	gsize i;
	GValue value;
	for (i = 0; i < length; i++) {
		ret *= 10;
		ret += g_unichar_digit_value(s[i]) == -1 ?
		       0 : g_unichar_digit_value(s[i]);
	}
	memset(&value, 0, sizeof(value));
	g_value_init(&value, G_TYPE_LONG);
	g_value_set_long(&value, ret - data->inc);
	g_value_array_append(array, &value);
	g_value_unset(&value);
	return TRUE;
}

static gboolean
char_class_multi_check(gunichar c, struct char_class_data *data)
{
	switch (c) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		case ';':
			return TRUE;
		default:
			return FALSE;
	}
}
static void
char_class_multi_setup(const gunichar *s, struct char_class_data *data, int inc)
{
	data->inc = inc;
	return;
}
static gboolean
char_class_multi_extract(const gunichar *s, gsize length,
			 struct char_class_data *data, GValueArray *array)
{
	long ret = 0;
	gsize i;
	GValue value;
	memset(&value, 0, sizeof(value));
	g_value_init(&value, G_TYPE_LONG);
	for (i = 0; i < length; i++) {
		if (s[i] == ';') {
			g_value_set_long(&value, ret - data->inc);
			g_value_array_append(array, &value);
			ret = 0;
		} else {
			ret *= 10;
			ret += (s[i] - '0');
		}
	}
	g_value_set_long(&value, ret - data->inc);
	g_value_array_append(array, &value);
	g_value_unset(&value);
	return TRUE;
}

static gboolean
char_class_any_check(gunichar c, struct char_class_data *data)
{
	return (c >= data->c) ? TRUE : FALSE;
}
static void
char_class_any_setup(const gunichar *s, struct char_class_data *data, int inc)
{
	data->c = s[0] + inc;
	return;
}
static gboolean
char_class_any_extract(const gunichar *s, gsize length,
		       struct char_class_data *data, GValueArray *array)
{
	long ret = 0;
	GValue value;
	ret = s[0] - data->c;
	memset(&value, 0, sizeof(value));
	g_value_init(&value, G_TYPE_LONG);
	g_value_set_long(&value, ret - data->inc);
	g_value_array_append(array, &value);
	g_value_unset(&value);
	return TRUE;
}

static gboolean
char_class_string_check(gunichar c, struct char_class_data *data)
{
	return (c != data->c) ? TRUE : FALSE;
}
static void
char_class_string_setup(const gunichar *s, struct char_class_data *data, int inc)
{
	data->c = s[0];
	return;
}
static gsize
unichar_snlen(const gunichar *s, gsize length)
{
	gsize i;
	for (i = 0; i < length; i++) {
		if (s[i] == '\0') {
			return i;
		}
	}
	return length;
}
static void
unichar_sncpy(gunichar *d, const gunichar *s, gsize length)
{
	unsigned int i;
	for (i = 0; i < length; i++) {
		d[i] = s[i];
		if (s[i] == 0) {
			break;
		}
	}
}
static int
unichar_sncmp(const gunichar *a, const gunichar *b, gsize length)
{
	gsize i;
	for (i = 0; i < length; i++) {
		if (a[i] != b[i]) {
			return a[i] - b[i];
		}
		if (a[i] == 0) {
			break;
		}
	}
	return 0;
}
static gboolean
char_class_string_extract(const gunichar *s, gsize length,
			  struct char_class_data *data, GValueArray *array)
{
	gunichar *ret = NULL;
	gsize len;
	gsize i;
	GValue value;

	len = unichar_snlen(s, length);
	ret = g_malloc0((len + 1) * sizeof(gunichar));
	unichar_sncpy(ret, s, len);
	for (i = 0; i < len; i++) {
		ret[i] &= ~(VTE_ISO2022_ENCODED_WIDTH_MASK);
	}
	_vte_debug_print(VTE_DEBUG_PARSE,
			"Extracting string `%ls'.\n", (wchar_t*) ret);
	memset(&value, 0, sizeof(value));

	g_value_init(&value, G_TYPE_POINTER);
	g_value_set_pointer(&value, ret);
	g_value_array_append(array, &value);
	g_value_unset(&value);

	return TRUE;
}

static gunichar empty_wstring[] = {'\0'};
static gunichar digit_wstring1[] = {'%', '2', '\0'};
static gunichar digit_wstring2[] = {'%', 'd', '\0'};
static gunichar any_wstring[] = {'%', '+', '\0'};
static gunichar exact_wstring[] = {'%', '%', '\0'};
static gunichar string_wstring[] = {'%', 's', '\0'};
static gunichar multi_wstring[] = {'%', 'm', '\0'};

static struct char_class char_classes[] = {
	{exact, FALSE, empty_wstring, 0, 1,
	 char_class_exact_check,
	 char_class_exact_setup,
	 char_class_none_extract},
	{digit, TRUE, digit_wstring1, 2, 0,
	 char_class_digit_check,
	 char_class_digit_setup,
	 char_class_digit_extract},
	{digit, TRUE, digit_wstring2, 2, 0,
	 char_class_digit_check,
	 char_class_digit_setup,
	 char_class_digit_extract},
	{multi, TRUE, multi_wstring, 2, 0,
	 char_class_multi_check,
	 char_class_multi_setup,
	 char_class_multi_extract},
	{any, FALSE, any_wstring, 2, 1,
	 char_class_any_check,
	 char_class_any_setup,
	 char_class_any_extract},
	{exact, FALSE, exact_wstring, 2, 0,
	 char_class_exact_check,
	 char_class_percent_setup,
	 char_class_none_extract},
	{string, TRUE, string_wstring, 2, 0,
	 char_class_string_check,
	 char_class_string_setup,
	 char_class_string_extract},
};

/* Create a new trie. */
TRIE_MAYBE_STATIC struct _vte_trie *
_vte_trie_new(void)
{
	struct _vte_trie *ret;
	ret = g_slice_new0(struct _vte_trie);
	ret->impl.klass = &_vte_matcher_trie;
	return ret;
}

TRIE_MAYBE_STATIC void
_vte_trie_free(struct _vte_trie *trie)
{
	unsigned int i;
	for (i = 0; i < trie->trie_path_count; i++) {
		_vte_trie_free(trie->trie_paths[i].trie);
	}
	if (trie->trie_path_count > 0) {
		g_free(trie->trie_paths);
	}
	g_slice_free(struct _vte_trie, trie);
}

/* Add the given pattern, with its own result string, to the trie, with the
 * given initial increment value. */
static void
_vte_trie_addx(struct _vte_trie *trie, gunichar *pattern, gsize length,
	       const char *result, GQuark quark, int inc)
{
	gsize i;
	struct char_class *cclass = NULL;
	struct char_class_data data;
	gunichar *code;
	gsize len = 0, ccount = 0;
	gunichar inc_wstring[] = {'%', 'i', '\0'};

	/* The trivial case -- we'll just set the result at this node. */
	if (length == 0) {
		if (trie->result == NULL) {
			trie->quark = g_quark_from_string(result);
			trie->result = g_quark_to_string(trie->quark);
		} else {
			_VTE_DEBUG_IF(VTE_DEBUG_PARSE)
				g_warning(_("Duplicate (%s/%s)!"),
					  result, trie->result);
		}
		return;
	}

	/* If this part of the control sequence indicates incrementing a
	 * parameter, keep track of the incrementing, skip over the increment
	 * substring, and keep going. */
	if ((length >= 2) && (unichar_sncmp(pattern, inc_wstring, 2) == 0)) {
		_vte_trie_addx(trie, pattern + 2, length - 2,
			       result, quark, inc + 1);
		return;
	}

	/* Now check for examples of character class specifiers, and use that
	 * to put this part of the pattern in a character class. */
	for (i = G_N_ELEMENTS(char_classes); i--; ) {
		len = char_classes[i].code_length;
		code = char_classes[i].code;
		ccount = char_classes[i].ccount;
		if ((len <= length) && (unichar_sncmp(pattern, code, len) == 0)) {
			cclass = &char_classes[i];
			break;
		}
	}

	/* Initialize the data item using the data we have here. */
	memset(&data, 0, sizeof(data));
	cclass->setup(pattern + len, &data, inc);

	/* Hunt for a subtrie which matches this class / data pair. */
	for (i = 0; i < trie->trie_path_count; i++) {
		struct char_class_data *tdata;
		tdata =  &trie->trie_paths[i].data;
		if ((trie->trie_paths[i].cclass == cclass) &&
		    (memcmp(&data, tdata, sizeof(data)) == 0)) {
			/* It matches, so insert the rest of the pattern into
			 * this subtrie. */
			_vte_trie_addx(trie->trie_paths[i].trie,
				       pattern + (len + ccount),
				       length - (len + ccount),
				       result,
				       quark,
				       inc);
			return;
		}
	}

	/* Add a new subtrie to contain the rest of this pattern. */
	trie->trie_path_count++;
	trie->trie_paths = g_realloc(trie->trie_paths,
				     trie->trie_path_count *
				     sizeof(trie->trie_paths[0]));
	i = trie->trie_path_count - 1;
	memset(&trie->trie_paths[i], 0, sizeof(trie->trie_paths[i]));
	trie->trie_paths[i].trie = _vte_trie_new();
	cclass->setup(pattern + len, &trie->trie_paths[i].data, inc);
	trie->trie_paths[i].cclass = cclass;

	/* Now insert the rest of the pattern into the node we just created. */
	_vte_trie_addx(trie->trie_paths[i].trie,
		       pattern + (len + ccount),
		       length - (len + ccount),
		       result,
		       quark,
		       inc);
}

/* Add the given pattern, with its own result string, to the trie. */
TRIE_MAYBE_STATIC void
_vte_trie_add(struct _vte_trie *trie, const char *pattern, gsize length,
	      const char *result, GQuark quark)
{
	const guchar *tpattern;
	guchar *wpattern, *wpattern_end;
	VteConv conv;
	gsize wlength;

	g_return_if_fail(trie != NULL);
	g_return_if_fail(pattern != NULL);
	g_return_if_fail(length > 0);
	g_return_if_fail(result != NULL);
	if (quark == 0) {
		quark = g_quark_from_string(result);
	}

	wlength = sizeof(gunichar) * (length + 1);
	wpattern = wpattern_end = g_malloc0(wlength + 1);

	conv = _vte_conv_open(VTE_CONV_GUNICHAR_TYPE, "UTF-8");
	g_assert(conv != VTE_INVALID_CONV);

	tpattern = (const guchar *)pattern;
	_vte_conv(conv, &tpattern, &length, &wpattern_end, &wlength);
	if (length == 0) {
		wlength = (wpattern_end - wpattern) / sizeof(gunichar);
		_vte_trie_addx(trie, (gunichar*)wpattern, wlength,
			       result, quark, 0);
	}
	_vte_conv_close(conv);

	g_free(wpattern);
}

/* Check if the given pattern matches part of the given trie, returning an
 * empty string on a partial initial match, a %NULL if there's no match in the
 * works, and the result string if we have an exact match. */
static const char *
_vte_trie_matchx(struct _vte_trie *trie, const gunichar *pattern, gsize length,
		 gboolean greedy,
		 const char **res, const gunichar **consumed,
		 GQuark *quark, GValueArray *array)
{
	unsigned int i;
	const char *hres;
	enum cclass cc;
	const char *best = NULL;
	GValueArray *bestarray = NULL;
	GQuark bestquark = 0;
	const gunichar *bestconsumed = pattern;

	/* Make sure that attempting to save output values doesn't kill us. */
	if (res == NULL) {
		res = &hres;
	}

	/* Trivial cases.  We've matched an entire pattern, or we're out of
	 * pattern to match. */
	if (trie->result != NULL) {
		*res = trie->result;
		*quark = trie->quark;
		*consumed = pattern;
		return *res;
	}
	if (length <= 0) {
		if (trie->trie_path_count > 0) {
			*res = "";
			*quark = g_quark_from_static_string("");
			*consumed = pattern;
			return *res;
		} else {
			*res = NULL;
			*quark = 0;
			*consumed = pattern;
			return *res;
		}
	}

	/* Now figure out which (if any) subtrees to search.  First, see
	 * which character class this character matches. */
	for (cc = exact; cc < invalid; cc++)
	for (i = 0; i < trie->trie_path_count; i++) {
		struct _vte_trie *subtrie = trie->trie_paths[i].trie;
		struct char_class *cclass = trie->trie_paths[i].cclass;
		struct char_class_data *data = &trie->trie_paths[i].data;
		if (trie->trie_paths[i].cclass->type == cc) {
			/* If it matches this character class... */
			if (cclass->check(pattern[0], data)) {
				const gunichar *prospect = pattern + 1;
				const char *tmp;
				GQuark tmpquark = 0;
				GValueArray *tmparray;
				gboolean better = FALSE;
				/* Move past characters which might match this
				 * part of the string... */
				while (cclass->multiple &&
				       ((gsize)(prospect - pattern) < length) &&
				       cclass->check(prospect[0], data)) {
					prospect++;
				}
				/* ... see if there's a parameter here, ... */
				tmparray = g_value_array_new(0);
				cclass->extract(pattern,
						prospect - pattern,
						data,
						tmparray);
				/* ... and check if the subtree matches the
				 * rest of the input string.  Any parameters
				 * further on will be appended to the array. */
				_vte_trie_matchx(subtrie,
						 prospect,
						 length - (prospect - pattern),
						 greedy,
						 &tmp,
						 consumed,
						 &tmpquark,
						 tmparray);
				/* If we haven't seen any matches yet, go ahead
				 * and go by this result. */
				if (best == NULL) {
					better = TRUE;
				} else
				/* If we have a match, and we didn't have one
				 * already, go by this result. */
				if ((best != NULL) &&
				    (best[0] == '\0') &&
				    (tmp != NULL) &&
				    (tmp[0] != '\0')) {
					better = TRUE;
				} else
				/* If we already have a match, and this one's
				 * better (longer if we're greedy, shorter if
				 * we're not), then go by this result. */
				if ((tmp != NULL) &&
				    (tmp[0] != '\0') &&
				    (bestconsumed != NULL) &&
				    (consumed != NULL) &&
				    (*consumed != NULL)) {
					if (greedy &&
					    (bestconsumed < *consumed)) {
						better = TRUE;
					} else
					if (!greedy &&
					    (bestconsumed > *consumed)) {
						better = TRUE;
					}
				}
				if (better) {
					best = tmp;
					if (bestarray != NULL) {
						_vte_matcher_free_params_array(
								NULL, bestarray);
					}
					bestarray = tmparray;
					bestquark = tmpquark;
					bestconsumed = *consumed;
				} else {
					_vte_matcher_free_params_array(
							NULL, tmparray);
					tmparray = NULL;
				}
			}
		}
	}

	/* We're done searching.  Copy out any parameters we picked up. */
	if (bestarray != NULL) {
		for (i = 0; i < bestarray->n_values; i++) {
			GValue *value = g_value_array_get_nth(bestarray, i);
			g_value_array_append(array, value);

			if (G_VALUE_HOLDS_POINTER(value)) {
				g_value_set_pointer(value, NULL);
			}
		}
		_vte_matcher_free_params_array(NULL, bestarray);
	}
#if 0
	printf("`%s' ", best);
	dump_array(array);
#endif
	*quark = bestquark;
	*res = best;
	*consumed = bestconsumed;
	return *res;
}

/* Check if the given pattern matches part of the given trie, returning an
 * empty string on a partial initial match, a %NULL if there's no match in the
 * works, and the result string if we have an exact match. */
TRIE_MAYBE_STATIC const char *
_vte_trie_match(struct _vte_trie *trie, const gunichar *pattern, gsize length,
		const char **res, const gunichar **consumed,
		GQuark *quark, GValueArray **array)
{
	const char *ret = NULL;
	GQuark tmpquark;
	GValueArray *valuearray;
	GValue *value;
	const gunichar *dummyconsumed;
	gboolean greedy = FALSE;
	guint i;

	if (array != NULL && *array != NULL) {
		valuearray = *array;
	} else {
		valuearray = g_value_array_new(0);
	}
	if (quark == NULL) {
		quark = &tmpquark;
	}
	*quark = 0;

	if (consumed == NULL) {
		consumed = &dummyconsumed;
	}
	*consumed = pattern;

	ret = _vte_trie_matchx(trie, pattern, length, greedy,
			       res, consumed, quark, valuearray);

	if (((ret == NULL) || (ret[0] == '\0')) || (valuearray->n_values == 0)){
		if (valuearray != NULL) {
			for (i = 0; i < valuearray->n_values; i++) {
				value = g_value_array_get_nth(valuearray, i);
				if (G_VALUE_HOLDS_POINTER(value)) {
					g_free(g_value_get_pointer(value));
					g_value_set_pointer(value, NULL);
				}
			}
			if (array == NULL || valuearray != *array) {
				_vte_matcher_free_params_array(NULL, valuearray);
			}
		}
	} else {
		if (array == NULL) {
			_vte_matcher_free_params_array(NULL, valuearray);
		}
	}

	return ret;
}

/* Print the next layer of the trie, indented by length spaces. */
static void
_vte_trie_printx(struct _vte_trie *trie, const char *previous,
		 gsize *nodecount)
{
	unsigned int i;
	char buf[LINE_MAX];

	if ((nodecount) && (trie->trie_path_count > 0)) {
		(*nodecount)++;
	}

	for (i = 0; i < trie->trie_path_count; i++) {
		memset(buf, '\0', sizeof(buf));
		snprintf(buf, sizeof(buf), "%s", previous);
		switch (trie->trie_paths[i].cclass->type) {
			case exact:
				if (trie->trie_paths[i].data.c < 32) {
					snprintf(buf + strlen(buf),
						 sizeof(buf) - strlen(buf),
						 "^%lc",
						 (wint_t)trie->trie_paths[i].data.c +
						 64);
				} else
				if (trie->trie_paths[i].data.c > 126) {
					snprintf(buf + strlen(buf),
						 sizeof(buf) - strlen(buf),
						 "[:%ld:]",
						 (long)trie->trie_paths[i].data.c);
				} else {
					snprintf(buf + strlen(buf),
						 sizeof(buf) - strlen(buf),
						 "%lc",
						 (wint_t)trie->trie_paths[i].data.c);
				}
				break;
			case digit:
				snprintf(buf + strlen(buf),
					 sizeof(buf) - strlen(buf),
					 "{num+%d}",
					 trie->trie_paths[i].data.inc);
				break;
			case multi:
				snprintf(buf + strlen(buf),
					 sizeof(buf) - strlen(buf),
					 "{multinum+%d}",
					 trie->trie_paths[i].data.inc);
				break;
			case any:
				if (trie->trie_paths[i].data.c < 32) {
					snprintf(buf + strlen(buf),
						 sizeof(buf) - strlen(buf),
						 "{char+0x%02lx}",
						 (long)trie->trie_paths[i].data.c);
				} else {
					snprintf(buf + strlen(buf),
						 sizeof(buf) - strlen(buf),
						 "{char+`%lc'}",
						 (wint_t)trie->trie_paths[i].data.c);
				}
				break;
			case string:
				snprintf(buf + strlen(buf),
					 sizeof(buf) - strlen(buf),
					 "{string}");
				break;
			case invalid:
				break;
		}
		if (trie->trie_paths[i].trie->result != NULL) {
			printf("%s = `%s'\n", buf,
			       trie->trie_paths[i].trie->result);
		}
		_vte_trie_printx(trie->trie_paths[i].trie, buf, nodecount);
	}
}

/* Print the trie. */
TRIE_MAYBE_STATIC void
_vte_trie_print(struct _vte_trie *trie)
{
	gsize nodecount = 0;
	_vte_trie_printx(trie, "", &nodecount);
	printf("Trie has %ld nodes.\n", (long) nodecount);
}

#ifdef TRIE_MAIN
static void
dump_array(GValueArray *array)
{
	unsigned int i;
	if (array != NULL) {
		printf("args = {");
		for (i = 0; i < array->n_values; i++) {
			GValue *value;
			value = g_value_array_get_nth(array, i);
			if (i > 0) {
				printf(", ");
			}
			if (G_VALUE_HOLDS_LONG(value)) {
				printf("%ld", g_value_get_long(value));
			}
			if (G_VALUE_HOLDS_STRING(value)) {
				printf("`%s'", g_value_get_string(value));
			}
			if (G_VALUE_HOLDS_POINTER(value)) {
				printf("`%ls'",
				       (wchar_t*) g_value_get_pointer(value));
			}
		}
		printf("}\n");
	}
}

static void
convert_mbstowcs(const char *i, gsize ilen,
		 gunichar *o, gsize *olen, gsize max_olen)
{
	VteConv conv;
	gsize outlen;
	conv = _vte_conv_open(VTE_CONV_GUNICHAR_TYPE, "UTF-8");
	g_assert(conv != VTE_INVALID_CONV);

	memset(o, 0, max_olen);
	outlen = max_olen;
	_vte_conv_cu(conv, (char**)&i, &ilen, &o, &outlen);
	_vte_conv_close(conv);

	*olen = (max_olen - outlen) / sizeof(gunichar);
}

int
main(int argc, char **argv)
{
	struct _vte_trie *trie;
	GValueArray *array = NULL;
	GQuark quark;
	gunichar buf[LINE_MAX];
	const gunichar *consumed;
	gsize buflen;

	_vte_debug_init();

	g_type_init();
	trie = _vte_trie_new();

	_vte_trie_add(trie, "abcdef", 6, "abcdef",
		      g_quark_from_static_string("abcdef"));
	_vte_trie_add(trie, "abcde", 5, "abcde",
		      g_quark_from_static_string("abcde"));
	_vte_trie_add(trie, "abcdeg", 6, "abcdeg",
		      g_quark_from_static_string("abcdeg"));
	_vte_trie_add(trie, "abc%+Aeg", 8, "abc%+Aeg",
		      g_quark_from_static_string("abc%+Aeg"));
	_vte_trie_add(trie, "abc%deg", 7, "abc%deg",
		      g_quark_from_static_string("abc%deg"));
	_vte_trie_add(trie, "abc%%eg", 7, "abc%%eg",
		      g_quark_from_static_string("abc%%eg"));
	_vte_trie_add(trie, "abc%%%i%deg", 11, "abc%%%i%deg",
		      g_quark_from_static_string("abc%%%i%deg"));
	_vte_trie_add(trie, "<esc>[%i%d;%dH", 14, "vtmatch",
		      g_quark_from_static_string("vtmatch"));
	_vte_trie_add(trie, "<esc>[%i%mL", 11, "multimatch",
		      g_quark_from_static_string("multimatch"));
	_vte_trie_add(trie, "<esc>[%mL<esc>[%mL", 18, "greedy",
		      g_quark_from_static_string("greedy"));
	_vte_trie_add(trie, "<esc>]2;%sh", 11, "decset-title",
		      g_quark_from_static_string("decset-title"));

	printf("Wide encoding is `%s'.\n", VTE_CONV_GUNICHAR_TYPE);

	_vte_trie_print(trie);
	printf("\n");

	quark = 0;
	convert_mbstowcs("abc", 3, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abc",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abcdef", 6, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abcdef",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abcde", 5, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abcde",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abcdeg", 6, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abcdeg",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abc%deg", 7, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abc%deg",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abc10eg", 7, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abc10eg",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abc%eg", 6, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abc%eg",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abc%10eg", 8, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abc%10eg",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("abcBeg", 6, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "abcBeg",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("<esc>[25;26H", 12, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>[25;26H",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("<esc>[25;2", 10, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>[25;2",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
	}

	quark = 0;
	convert_mbstowcs("<esc>[25L", 9, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>[25L",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
	}

	quark = 0;
	convert_mbstowcs("<esc>[25L<esc>[24L", 18, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>[25L<esc>[24L",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
	}

	quark = 0;
	convert_mbstowcs("<esc>[25;26L", 12, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>[25;26L",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
	}

	quark = 0;
	convert_mbstowcs("<esc>]2;WoofWoofh", 17, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>]2;WoofWoofh",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("<esc>]2;WoofWoofh<esc>]2;WoofWoofh", 34,
			 buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>]2;WoofWoofh<esc>]2;WoofWoofh",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	quark = 0;
	convert_mbstowcs("<esc>]2;WoofWoofhfoo", 20, buf, &buflen, sizeof(buf));
	printf("`%s' = `%s'\n", "<esc>]2;WoofWoofhfoo",
	       _vte_trie_match(trie, buf, buflen,
			       NULL, &consumed, &quark, &array));
	printf("=> `%s' (%d)\n", g_quark_to_string(quark), (int)(consumed - buf));
	if (array != NULL) {
		dump_array(array);
		_vte_matcher_free_params_array(NULL, array);
		array = NULL;
	}

	_vte_trie_free(trie);

	return 0;
}
#endif

const struct _vte_matcher_class _vte_matcher_trie = {
	(_vte_matcher_create_func)_vte_trie_new,
	(_vte_matcher_add_func)_vte_trie_add,
	(_vte_matcher_print_func)_vte_trie_print,
	(_vte_matcher_match_func)_vte_trie_match,
	(_vte_matcher_destroy_func)_vte_trie_free
};

/* Add a string to the matcher. */
static void
_vte_matcher_add(const struct _vte_matcher *matcher,
		 const char *pattern, gssize length,
		 const char *result, GQuark quark)
{
	matcher->impl->klass->add(matcher->impl, pattern, length, result, quark);
}

/* Loads all sequences into matcher */
static void
_vte_matcher_init(struct _vte_matcher *matcher, const char *emulation,
		  struct _vte_termcap *termcap)
{
	const char *code, *value;
	gboolean found_cr = FALSE, found_lf = FALSE;
	gssize stripped_length;
	char *stripped;
	int i;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "_vte_matcher_init()\n");

	if (termcap != NULL) {
		/* Load the known capability strings from the termcap
		 * structure into the table for recognition. */
		for (i = 0;
				_vte_terminal_capability_strings[i].capability[0];
				i++) {
			if (_vte_terminal_capability_strings[i].key) {
				continue;
			}
			code = _vte_terminal_capability_strings[i].capability;
			stripped = _vte_termcap_find_string_length(termcap,
					emulation,
					code,
					&stripped_length);
			if (stripped[0] != '\0') {
				_vte_matcher_add(matcher,
						stripped, stripped_length,
						code, 0);
				if (stripped[0] == '\r') {
					found_cr = TRUE;
				} else
					if (stripped[0] == '\n') {
						if (strcmp(code, "sf") == 0 ||
								strcmp(code, "do") == 0) {
							found_lf = TRUE;
						}
					}
			}
			g_free(stripped);
		}
	}

	/* Add emulator-specific sequences. */
	if (strstr(emulation, "xterm") || strstr(emulation, "dtterm")) {
		/* Add all of the xterm-specific stuff. */
		for (i = 0;
		     _vte_xterm_capability_strings[i].value != NULL;
		     i++) {
			code = _vte_xterm_capability_strings[i].code;
			value = _vte_xterm_capability_strings[i].value;
			_vte_matcher_add(matcher, code, strlen (code),
					 value, 0);
		}
	}

	/* Always define cr and lf. */
	if (!found_cr) {
		_vte_matcher_add(matcher, "\r", 1, "cr", 0);
	}
	if (!found_lf) {
		_vte_matcher_add(matcher, "\n", 1, "sf", 0);
	}

	_VTE_DEBUG_IF(VTE_DEBUG_TRIE) {
		g_printerr("Trie contents:\n");
		_vte_matcher_print(matcher);
		g_printerr("\n");
	}
}

/* Allocates new matcher structure. */
static gpointer
_vte_matcher_create(gpointer key)
{
	char *emulation = key;
	struct _vte_matcher *ret = NULL;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "_vte_matcher_create()\n");
	ret = g_slice_new(struct _vte_matcher);
	ret->impl = &dummy_vte_matcher_trie;
	ret->match = NULL;
	ret->free_params = NULL;

	if (strcmp(emulation, "xterm") == 0) {
		ret->impl = &dummy_vte_matcher_table;
	} else
	if (strcmp(emulation, "dtterm") == 0) {
		ret->impl = &dummy_vte_matcher_table;
	}

	return ret;
}

/* Noone uses this matcher, free it. */
static void
_vte_matcher_destroy(gpointer value)
{
	struct _vte_matcher *matcher = value;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "_vte_matcher_destroy()\n");
	if (matcher->free_params != NULL) {
		g_value_array_free (matcher->free_params);
	}
	if (matcher->match != NULL) /* do not call destroy on dummy values */
		matcher->impl->klass->destroy(matcher->impl);
	g_slice_free(struct _vte_matcher, matcher);
}

/* Create and init matcher. */
struct _vte_matcher *
_vte_matcher_new(const char *emulation, struct _vte_termcap *termcap)
{
	struct _vte_matcher *ret = NULL;
	g_static_mutex_lock(&_vte_matcher_mutex);

	if (emulation == NULL) {
		emulation = "";
	}

	if (_vte_matcher_cache == NULL) {
		_vte_matcher_cache = g_cache_new(_vte_matcher_create,
				_vte_matcher_destroy,
				(GCacheDupFunc) g_strdup, g_free,
				g_str_hash, g_direct_hash, g_str_equal);
	}

	ret = g_cache_insert(_vte_matcher_cache, (gpointer) emulation);

	if (ret->match == NULL) {
		ret->impl = ret->impl->klass->create();
		ret->match = ret->impl->klass->match;
		_vte_matcher_init(ret, emulation, termcap);
	}

	g_static_mutex_unlock(&_vte_matcher_mutex);
	return ret;
}

/* Free a matcher. */
void
_vte_matcher_free(struct _vte_matcher *matcher)
{
	g_assert(_vte_matcher_cache != NULL);
	g_static_mutex_lock(&_vte_matcher_mutex);
	g_cache_remove(_vte_matcher_cache, matcher);
	g_static_mutex_unlock(&_vte_matcher_mutex);
}

/* Check if a string matches a sequence the matcher knows about. */
const char *
_vte_matcher_match(struct _vte_matcher *matcher,
		   const gunichar *pattern, gssize length,
		   const char **res, const gunichar **consumed,
		   GQuark *quark, GValueArray **array)
{
	if (G_UNLIKELY (array != NULL && matcher->free_params != NULL)) {
		*array = matcher->free_params;
		matcher->free_params = NULL;
	}
	return matcher->match(matcher->impl, pattern, length,
					res, consumed, quark, array);
}

/* Dump out the contents of a matcher, mainly for debugging. */
void
_vte_matcher_print(struct _vte_matcher *matcher)
{
	matcher->impl->klass->print(matcher->impl);
}

/* Free a parameter array.  Most of the GValue elements can clean up after
 * themselves, but we're using gpointers to hold unicode character strings, and
 * we need to free those ourselves. */
void
_vte_matcher_free_params_array(struct _vte_matcher *matcher,
		               GValueArray *params)
{
	guint i;
	for (i = 0; i < params->n_values; i++) {
		GValue *value = &params->values[i];
		if (G_UNLIKELY (g_type_is_a (value->g_type, G_TYPE_POINTER))) {
			g_free (g_value_get_pointer (value));
		}
	}
	if (G_UNLIKELY (matcher == NULL || matcher->free_params != NULL)) {
		g_value_array_free (params);
	} else {
		matcher->free_params = params;
		params->n_values = 0;
	}
}

VteTree *
_vte_tree_new(GCompareFunc key_compare_func)
{
  VteTree *tree = g_slice_new0 (VteTree);
  tree->tree = g_tree_new (key_compare_func);
  return tree;
}

void 
_vte_tree_destroy(VteTree *tree)
{
  g_tree_destroy (tree->tree);
  g_slice_free (VteTree, tree);
}

void 
_vte_tree_insert(VteTree *tree, gpointer key, gpointer value)
{
  guint index = GPOINTER_TO_UINT (key);
  
  if (index < VTE_TREE_ARRAY_SIZE) {
    tree->array[index] = value;
    return;
  }
  g_tree_insert (tree->tree, key, value);
}

gpointer
_vte_tree_lookup(VteTree *tree, gconstpointer key)
{
  const guint index = GPOINTER_TO_UINT (key);
  
  if (index < VTE_TREE_ARRAY_SIZE)
    return tree->array[index];

  return g_tree_lookup (tree->tree, key);
}

static gint
_vte_direct_compare(gconstpointer a, gconstpointer b)
{
	return GPOINTER_TO_INT(a) - GPOINTER_TO_INT(b);
}

/* If we have the encoding, decide how wide an ambiguously-wide character is
 * based on the encoding.  This is basically what GNU libc does, and it agrees
 * with my reading of Unicode UAX 11, so.... */
static int
_vte_iso2022_ambiguous_width(struct _vte_iso2022_state *state)
{
	const char wide_codelist[][10] = {
		"big5",
		"big5hkscs",
		"euccn",
		"eucjp",
		"euckr",
		"euctw",
		"gb18030",
		"gb2312",
		"gbk",
		"shiftjis",
		"tcvn",
	};
	gsize i, j;
	char codeset[16];

	/* Catch weirdo cases. */
	if ((state->codeset == NULL) || (state->codeset[0] == '\0')) {
		return 1;
	}

	/* Sort-of canonify the encoding name. */
	i = j = 0;
	for (i = 0; state->codeset[i] != '\0'; i++) {
		if (g_ascii_isalnum(state->codeset[i]))
			codeset[j++] = g_ascii_tolower(state->codeset[i]);

		if (j >= sizeof(codeset) - 1)
			break;
	}
	codeset[j] = '\0';

	/* Check for the name in the list. */
	for (i = 0; i < G_N_ELEMENTS(wide_codelist); i++) {
		if (strcmp(codeset, wide_codelist[i]) == 0) {
			return 2;
		}
	}

	/*
	 * Decide the ambiguous width according to user preference if
	 * current locale is UTF-8.
	 */
	if (strcmp (codeset, "utf8") == 0) {
	  const char *env = g_getenv ("VTE_CJK_WIDTH");
	  if (env && (g_ascii_strcasecmp (env, "wide")==0 || g_ascii_strcasecmp (env, "1")==0))
	    return 2;
	}

	/* Not in the list => not wide. */
	return 1;
}

static inline gboolean
_vte_iso2022_is_ambiguous(gunichar c)
{
	if (G_LIKELY (c < 0x80))
		return FALSE;
	if (G_UNLIKELY (g_unichar_iszerowidth (c)))
		return FALSE;
	return G_UNLIKELY (!g_unichar_iswide (c) && g_unichar_iswide_cjk (c));
}

int
_vte_iso2022_unichar_width(struct _vte_iso2022_state *state,
			   gunichar c)
{
	if (G_LIKELY (c < 0x80))
		return 1;
	if (G_UNLIKELY (g_unichar_iszerowidth (c)))
		return 0;
	if (G_UNLIKELY (g_unichar_iswide (c)))
		return 2;
	if (G_LIKELY (state->ambiguous_width == 1))
		return 1;
	if (G_UNLIKELY (g_unichar_iswide_cjk (c)))
		return 2;
	return 1;
}

static GHashTable *
_vte_iso2022_map_init16(const struct _vte_iso2022_map16 *map, gssize length)
{
	GHashTable *ret;
	int i;
	if (length == 0) {
		return NULL;
	}
	ret = g_hash_table_new(NULL, NULL);
	for (i = 0; i < length; i++) {
		g_hash_table_insert(ret,
			      GINT_TO_POINTER((unsigned int) map[i].from),
			      GINT_TO_POINTER((unsigned int) map[i].to));
	}
	return ret;
}

static GHashTable *
_vte_iso2022_map_init32(const struct _vte_iso2022_map32 *map, gssize length)
{
	GHashTable *ret;
	int i;
	if (length == 0) {
		return NULL;
	}
	ret = g_hash_table_new(NULL, NULL);
	for (i = 0; i < length; i++) {
		g_hash_table_insert(ret,
			      GINT_TO_POINTER((unsigned int) map[i].from),
			      GINT_TO_POINTER((unsigned int) map[i].to));
	}
	return ret;
}

static void
_vte_iso2022_map_get(gunichar mapname,
		     GHashTable **_map, guint *bytes_per_char, guint *force_width,
		     gulong *or_mask, gulong *and_mask)
{
	static VteTree *maps = NULL;
	gint bytes = 0, width = 0;
	GHashTable *map = NULL;
	gboolean new_map = FALSE;
	gsize i;

	if (or_mask) {
		*or_mask = 0;
	}
	if (and_mask) {
		*and_mask = (~(0));
	}

	/* Make sure we have a map, erm, map. */
	if (maps == NULL) {
		maps = _vte_tree_new(_vte_direct_compare);
	}

	/* Check for a cached map for this charset. */
	map = _vte_tree_lookup(maps, GINT_TO_POINTER(mapname));
	new_map = map == NULL;

	/* Construct a new one. */
	switch (mapname) {
	case '0':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_0,
					    G_N_ELEMENTS(_vte_iso2022_map_0));
		}
		width = 1;
		bytes = 1;
		break;
	case 'A':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_A,
					    G_N_ELEMENTS(_vte_iso2022_map_A));
		}
		width = 1;
		bytes = 1;
		break;
	case '1': /* treated as an alias in xterm */
	case '2': /* treated as an alias in xterm */
	case 'B':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_B,
					    G_N_ELEMENTS(_vte_iso2022_map_B));
		}
		width = 1;
		bytes = 1;
		break;
	case '4':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_4,
					    G_N_ELEMENTS(_vte_iso2022_map_4));
		}
		width = 1;
		bytes = 1;
		break;
	case 'C':
	case '5':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_C,
					    G_N_ELEMENTS(_vte_iso2022_map_C));
		}
		width = 1;
		bytes = 1;
		break;
	case 'R':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_R,
					    G_N_ELEMENTS(_vte_iso2022_map_R));
		}
		width = 1;
		bytes = 1;
		break;
	case 'Q':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_Q,
					    G_N_ELEMENTS(_vte_iso2022_map_Q));
		}
		width = 1;
		bytes = 1;
		break;
	case 'K':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_K,
					    G_N_ELEMENTS(_vte_iso2022_map_K));
		}
		width = 1;
		bytes = 1;
		break;
	case 'Y':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_Y,
					    G_N_ELEMENTS(_vte_iso2022_map_Y));
		}
		width = 1;
		bytes = 1;
		break;
	case 'E':
	case '6':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_E,
					    G_N_ELEMENTS(_vte_iso2022_map_E));
		}
		width = 1;
		bytes = 1;
		break;
	case 'Z':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_Z,
					    G_N_ELEMENTS(_vte_iso2022_map_Z));
		}
		width = 1;
		bytes = 1;
		break;
	case 'H':
	case '7':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_H,
					    G_N_ELEMENTS(_vte_iso2022_map_H));
		}
		width = 1;
		bytes = 1;
		break;
	case '=':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_equal,
					    G_N_ELEMENTS(_vte_iso2022_map_equal));
		}
		width = 1;
		bytes = 1;
		break;
	case 'U':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_U,
					    G_N_ELEMENTS(_vte_iso2022_map_U));
		}
		width = 1;
		bytes = 1;
		break;
	case 'J':
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_J,
					    G_N_ELEMENTS(_vte_iso2022_map_J));
		}
		width = 1;
		bytes = 1;
		break;
	case '@' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_wide_at,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_at));
		}
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		*and_mask = 0xf7f7f;
		break;
	case 'A' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_wide_A,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_A));
		}
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		*and_mask = 0xf7f7f;
		break;
	case 'B' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_wide_B,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_B));
		}
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		*and_mask = 0xf7f7f;
		break;
	case 'C' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_wide_C,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_C));
		}
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		*and_mask = 0xf7f7f;
		break;
	case 'D' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init16(_vte_iso2022_map_wide_D,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_D));
		}
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		*and_mask = 0xf7f7f;
		break;
	case 'G' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init32(_vte_iso2022_map_wide_G,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_G));
		}
		/* Return the plane number as part of the "or" mask. */
		g_assert(or_mask != NULL);
		*or_mask = 0x10000;
		*and_mask = 0xf7f7f;
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		break;
	case 'H' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init32(_vte_iso2022_map_wide_G,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_G));
		}
		/* Return the plane number as part of the "or" mask. */
		g_assert(or_mask != NULL);
		*or_mask = 0x20000;
		*and_mask = 0xf7f7f;
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		break;
	case 'I' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init32(_vte_iso2022_map_wide_G,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_G));
		}
		/* Return the plane number as part of the "or" mask. */
		g_assert(or_mask != NULL);
		*or_mask = 0x30000;
		*and_mask = 0xf7f7f;
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		break;
	case 'J' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init32(_vte_iso2022_map_wide_G,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_G));
		}
		/* Return the plane number as part of the "or" mask. */
		g_assert(or_mask != NULL);
		*or_mask = 0x40000;
		*and_mask = 0xf7f7f;
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		break;
	case 'K' + WIDE_FUDGE:
		if (map == NULL) {
			map = _vte_iso2022_map_init32(_vte_iso2022_map_wide_G,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_G));
		}
		/* Return the plane number as part of the "or" mask. */
		g_assert(or_mask != NULL);
		*or_mask = 0x50000;
		*and_mask = 0xf7f7f;
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		break;
	case 'L' + WIDE_FUDGE:
		if (G_UNLIKELY (map == NULL)) {
			map = _vte_iso2022_map_init32(_vte_iso2022_map_wide_G,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_G));
		}
		/* Return the plane number as part of the "or" mask. */
		g_assert(or_mask != NULL);
		*or_mask = 0x60000;
		*and_mask = 0xf7f7f;
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		break;
	case 'M' + WIDE_FUDGE:
		if (G_UNLIKELY(map == NULL)) {
			map = _vte_iso2022_map_init32(_vte_iso2022_map_wide_G,
					    G_N_ELEMENTS(_vte_iso2022_map_wide_G));
		}
		/* Return the plane number as part of the "or" mask. */
		g_assert(or_mask != NULL);
		*or_mask = 0x70000;
		*and_mask = 0xf7f7f;
		width = 2; /* CJKV expects 2 bytes -> 2 columns */
		bytes = 2;
		break;
	default:
		/* No such map.  Set up a ISO-8859-1 to UCS-4 map. */
		if (G_UNLIKELY (map == NULL)) {
			struct _vte_iso2022_map16 _vte_iso2022_map_NUL[256];
			for (i = 0; i < G_N_ELEMENTS(_vte_iso2022_map_NUL); i++) {
				_vte_iso2022_map_NUL[i].from = (i & 0xff);
				_vte_iso2022_map_NUL[i].to = (i & 0xff);
			}
			map = _vte_iso2022_map_init16(_vte_iso2022_map_NUL,
					    G_N_ELEMENTS(_vte_iso2022_map_NUL));
		}
		width = 1;
		bytes = 1;
		break;
	}
	/* Save the new map. */
	if (G_UNLIKELY(new_map && map != NULL)) {
		_vte_tree_insert(maps, GINT_TO_POINTER(mapname), map);
	}
	/* Return. */
	if (_map) {
		*_map = map;
	}
	if (bytes_per_char) {
		*bytes_per_char = bytes;
	}
	if (force_width) {
		*force_width = width;
	}
}

int
_vte_iso2022_get_encoded_width(gunichar c)
{
	int width;
	width = (c & VTE_ISO2022_ENCODED_WIDTH_MASK) >> VTE_ISO2022_ENCODED_WIDTH_BIT_OFFSET;
	return CLAMP(width, 0, 2);
}

static gunichar
_vte_iso2022_set_encoded_width(gunichar c, int width)
{
	width = CLAMP(width, 0, 2);
	c &= ~(VTE_ISO2022_ENCODED_WIDTH_MASK);
	c |= (width << VTE_ISO2022_ENCODED_WIDTH_BIT_OFFSET);
	return c;
}

struct _vte_iso2022_state *
_vte_iso2022_state_new(const char *native_codeset,
		       _vte_iso2022_codeset_changed_cb_fn fn,
		       gpointer data)
{
	struct _vte_iso2022_state *state;
	state = g_slice_new0(struct _vte_iso2022_state);
	state->nrc_enabled = TRUE;
	state->current = 0;
	state->override = -1;
	state->g[0] = 'B';
	state->g[1] = 'B';
	state->g[2] = 'B';
	state->g[3] = 'B';
	state->codeset = native_codeset;
	state->native_codeset = state->codeset;
	if (native_codeset == NULL) {
		g_get_charset(&state->codeset);
		state->native_codeset = state->codeset;
	}
	state->utf8_codeset = "UTF-8";
	state->target_codeset = VTE_CONV_GUNICHAR_TYPE;
	_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
			"Native codeset \"%s\", currently %s\n",
			state->native_codeset, state->codeset);
	state->conv = _vte_conv_open(state->target_codeset, state->codeset);
	state->codeset_changed = fn;
	state->codeset_changed_data = data;
	state->buffer = _vte_buffer_new();
	if (state->conv == VTE_INVALID_CONV) {
		g_warning(_("Unable to convert characters from %s to %s."),
			  state->codeset, state->target_codeset);
		_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
				"Using UTF-8 instead.\n");
		state->codeset = state->utf8_codeset;
		state->conv = _vte_conv_open(state->target_codeset,
					     state->codeset);
		if (state->conv == VTE_INVALID_CONV) {
			g_error(_("Unable to convert characters from %s to %s."),
				state->codeset, state->target_codeset);
		}
	}
	state->ambiguous_width = _vte_iso2022_ambiguous_width(state);
	return state;
}

void
_vte_iso2022_state_free(struct _vte_iso2022_state *state)
{
	_vte_buffer_free(state->buffer);
	if (state->conv != VTE_INVALID_CONV) {
		_vte_conv_close(state->conv);
	}
	g_slice_free(struct _vte_iso2022_state, state);
}

void
_vte_iso2022_state_set_codeset(struct _vte_iso2022_state *state,
			       const char *codeset)
{
	VteConv conv;

	g_return_if_fail(state != NULL);
	g_return_if_fail(codeset != NULL);
	g_return_if_fail(strlen(codeset) > 0);

	_vte_debug_print(VTE_DEBUG_SUBSTITUTION, "%s\n", codeset);
	conv = _vte_conv_open(state->target_codeset, codeset);
	if (conv == VTE_INVALID_CONV) {
		g_warning(_("Unable to convert characters from %s to %s."),
			  codeset, state->target_codeset);
		return;
	}
	if (state->conv != VTE_INVALID_CONV) {
		_vte_conv_close(state->conv);
	}
	state->codeset = g_intern_string (codeset);
	state->conv = conv;
	state->ambiguous_width = _vte_iso2022_ambiguous_width (state);
}

const char *
_vte_iso2022_state_get_codeset(struct _vte_iso2022_state *state)
{
	return state->codeset;
}

static const guchar *
_vte_iso2022_find_nextctl(const guchar *p, const guchar * const q)
{
	do {
		switch (*p) {
			case '\033':
			case '\n':
			case '\r':
			case '\016':
			case '\017':
#ifdef VTE_ISO2022_8_BIT_CONTROLS
		    /* This breaks UTF-8 and other encodings which
		     * use the high bits.
		     */
			case '0x8e':
			case '0x8f':
#endif
				return p;
		}
	} while (++p < q);
	return NULL;
}

static long
_vte_iso2022_sequence_length(const unsigned char *nextctl, gsize length)
{
	long sequence_length = -1;
	gsize i;

	switch (nextctl[0]) {
	case '\n':
	case '\r':
	case '\016':
	case '\017':
		/* LF */
		/* CR */
		/* SO */
		/* SI */
		sequence_length = 1;
		break;
	case 0x8e:
	case 0x8f:
		/* SS2 - 8bit */
		/* SS3 - 8bit */
		sequence_length = 1;
		break;
	case '\033':
		if (length < 2) {
			/* Inconclusive. */
			sequence_length = 0;
		} else {
			switch (nextctl[1]) {
			case '[':
				/* ESC [, the CSI.  The first letter
				 * is the end of the sequence, */
				for (i = 2; i < length; i++) {
					if (g_unichar_isalpha(nextctl[i])) {
						break;
					}
					if ((nextctl[i] == '@') ||
					    (nextctl[i] == '`') ||
					    (nextctl[i] == '{') ||
					    (nextctl[i] == '|')) {
						break;
					}
				}
				if (i < length) {
					/* Return the length of this
					 * sequence. */
					sequence_length = i + 1;
				} else {
					/* Inconclusive. */
					sequence_length = 0;
				}
				break;
#if 0
			case ']':
				/* ESC ], the OSC.  Search for a string
				 * terminator or a BEL. */
				for (i = 2; i < q - nextctl - 1; i++) {
					if ((nextctl[i] == '\033') &&
					    (nextctl[i + 1] == '\\')) {
						break;
					}
				}
				if (i < length - 1) {
					/* Return the length of this
					 * sequence. */
					sequence_length = i + 1;
				} else {
					for (i = 2; i < length; i++) {
						if (nextctl[i] == '\007') {
							break;
						}
					}
					if (i < length) {
						/* Return the length of
						 * this sequence. */
						sequence_length = i + 1;
					} else {
						/* Inconclusive. */
						sequence_length = 0;
					}
				}
				break;
#endif
#if 0
			case '^':
				/* ESC ^, the PM.  Search for a string
				 * terminator. */
#endif
			case 'P':
				/* ESC P, the DCS.  Search for a string
				 * terminator. */
				for (i = 2; i < length - 1; i++) {
					if ((nextctl[i] == '\033') &&
					    (nextctl[i + 1] == '\\')) {
						break;
					}
				}
				if (i < length - 1) {
					/* Return the length of this
					 * sequence. */
					sequence_length = i + 1;
				} else {
					/* Inconclusive. */
					sequence_length = 0;
				}
				break;
			case 'N':
			case 'O':
			case 'n':
			case 'o':
				/* ESC N */
				/* ESC O */
				/* ESC n */
				/* ESC o */
				sequence_length = 2;
				break;
			case '(':
			case ')':
			case '*':
			case '+':
				if (length < 3) {
					/* Inconclusive. */
					sequence_length = 0;
				} else {
					/* ESC ) x */
					/* ESC ( x */
					/* ESC * x */
					/* ESC + x */
					/* Just accept whatever. */
					sequence_length = 3;
				}
				break;
			case '%':
				if (length < 3) {
					/* Inconclusive. */
					sequence_length = 0;
				} else {
					/* ESC % @ */
					/* ESC % G */
					switch (nextctl[2]) {
					case '@':
					case 'G':
						sequence_length = 3;
						break;
					default:
						break;
					}
				}
				break;
			case '$':
				if (length < 3) {
					/* Inconclusive. */
					sequence_length = 0;
				} else {
					switch (nextctl[2]) {
					case '@':
					case 'B':
						/* ESC $ @ */
						/* ESC $ B */
						sequence_length = 3;
						break;
					case '(':
					case ')':
					case '*':
					case '+':
						/* ESC $ ( x */
						/* ESC $ ) x */
						/* ESC $ * x */
						/* ESC $ + x */
						if (length < 4) {
							/* Inconclusive. */
							sequence_length = 0;
						} else {
							/* strchr(WIDE_GMAPS, nextctl[3]) */
							switch (nextctl[3]) {
							case 'C':
							case 'A':
							case 'G':
							case 'H':
							case 'I':
							case 'J':
							case 'K':
							case 'L':
							case 'M':
							case 'D':
								sequence_length = 4;
								break;
							default:
								break;
							}
						}
						break;
					}
				}
				break;
			default:
				break;
			}
		}
		break;
	}
	return sequence_length;
}

static int
process_8_bit_sequence(struct _vte_iso2022_state *state,
		       const guchar **inbuf, gsize *inbytes,
		       gunichar **outbuf, gsize *outbytes)
{
	guint i, width;
	gpointer p;
	gunichar c, *outptr;
	const guchar *inptr;
	gulong acc, or_mask, and_mask;
	GHashTable *map;
	guint bytes_per_char, force_width, current;

	/* Check if it's an 8-bit escape.  If it is, take a note of which map
	 * it's for, and if it isn't, fail. */
	current = 0;
	switch (**inbuf) {
	case 0x8e:
		current = 2;
		break;
	case 0x8f:
		current = 3;
		break;
	default:
		/* We processed 0 bytes, and we have no intention of looking
		 * at this byte again. */
		return 0;
	}

	inptr = *inbuf;
	outptr = *outbuf;

	/* Find the map, and ensure that in addition to the escape byte, we
	 * have enough information to construct the character. */
	_vte_iso2022_map_get(state->g[current],
			     &map, &bytes_per_char, &force_width,
			     &or_mask, &and_mask);
	if (*inbytes < (bytes_per_char + 1)) {
		/* We need more information to work with. */
		return -1;
	}

	for (acc = 0, i = 0; i < bytes_per_char; i++) {
		acc = (acc << 8) | ((guint8*)inptr)[i + 1];
	}
	*inbuf += (bytes_per_char + 1);
	*inbytes -= (bytes_per_char + 1);

	acc &= and_mask;
	acc |= or_mask;
	p = GINT_TO_POINTER(acc);
	c = GPOINTER_TO_INT(g_hash_table_lookup(map, p));
	if ((c == 0) && (acc != 0)) {
		_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
				"%04lx -(%c)-> %04lx(?)\n",
				acc, state->g[current] & 0xff, acc);
	} else {
		width = 0;
		if (force_width != 0) {
			width = force_width;
		} else {
			if (G_UNLIKELY (_vte_iso2022_is_ambiguous(c))) {
				width = state->ambiguous_width;
			}
		}
		_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
				"%05lx -> " "%04x\n", acc, c);
		c = _vte_iso2022_set_encoded_width(c, width);
	}
	/* Save the unichar. */
	g_assert(*outbytes >= sizeof(c));
	*outbytes -= sizeof(c);
	*outptr++ = c;
	*outbuf = outptr;
	/* Return the number of input bytes consumed. */
	return bytes_per_char + 1;
}

static glong
process_cdata(struct _vte_iso2022_state *state, const guchar *cdata, gsize length,
	      GArray *gunichars)
{
	int ambiguous_width;
	glong processed = 0;
	GHashTable *map;
	guint bytes_per_char, force_width, current;
	gsize converted;
	const guchar *inbuf;
	gunichar *outbuf, *buf;
	gsize inbytes, outbytes;
	guint i, j, width;
	gulong acc, or_mask, and_mask;
	gunichar c;
	gboolean single, stop;

	ambiguous_width = state->ambiguous_width;

	single = (state->override != -1);
	current = single ? state->override : state->current;
	state->override = -1;
	g_assert(current < G_N_ELEMENTS(state->g));

	_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
			"Current map = %d (%c).\n",
			current, (state->g[current] & 0xff));

	if (!state->nrc_enabled || (state->g[current] == 'B')) {
		inbuf = cdata;
		inbytes = length;
		_vte_buffer_set_minimum_size(state->buffer,
					     sizeof(gunichar) * length * 2);
		buf = (gunichar *)state->buffer->data;
		outbuf = buf;
		outbytes = sizeof(gunichar) * length * 2;
		do {
			converted = _vte_conv_cu(state->conv,
					         &inbuf, &inbytes,
					         &outbuf, &outbytes);
			stop = FALSE;
			switch (converted) {
			case ((gsize)-1):
				switch (errno) {
				case EILSEQ:
					/* Check if it's an 8-bit sequence. */
					i = process_8_bit_sequence(state,
								   &inbuf,
								   &inbytes,
								   &outbuf,
								   &outbytes);
					switch (i) {
					case 0:
						/* iconv() may be buggy,
						 * returning EILSEQ and
						 * no remaining bytes.
						 * Bug 567064 */
						if (inbytes) {
							/* Nope, munge the input. */
							inbuf++;
							inbytes--;
						}
						*outbuf++ = INVALID_CODEPOINT;
						outbytes -= sizeof(gunichar);
						break;
					case -1:
						/* Looks good so far, try again
						 * later. */
						stop = TRUE;
						break;
					default:
						/* Processed n bytes, just keep
						 * going. */
						break;
					}
					break;
				case EINVAL:
					/* Incomplete. Save for later. */
					stop = TRUE;
					break;
				case E2BIG:
					/* Should never happen. */
					g_assert_not_reached();
					break;
				default:
					/* Should never happen. */
					g_assert_not_reached();
					break;
				}
			default:
				break;
			}
		} while ((inbytes > 0) && !stop);

		/* encode any ambiguous widths and skip blanks */
		j = gunichars->len;
		g_array_set_size(gunichars, gunichars->len + outbuf-buf);
		for (i = 0; buf + i < outbuf; i++) {
			c = buf[i];
			if (G_UNLIKELY (c == '\0')) {
				/* Skip the padding character. */
				continue;
			}
			if (G_UNLIKELY (_vte_iso2022_is_ambiguous(c))) {
				width = ambiguous_width;
				c = _vte_iso2022_set_encoded_width(c, width);
			}
			g_array_index(gunichars, gunichar, j++) = c;
		}
		gunichars->len = j;

		/* Done. */
		processed = length - inbytes;
	} else {
		_vte_iso2022_map_get(state->g[current],
				     &map, &bytes_per_char, &force_width,
				     &or_mask, &and_mask);
		i = 0;
		acc = 0;
		j = gunichars->len;
		g_array_set_size(gunichars, gunichars->len + length);
		do {
			if (i < length) {
				acc = (acc << 8) | cdata[i];
			}
			i++;
			if ((i % bytes_per_char) == 0) {
				acc &= and_mask;
				acc |= or_mask;
				c = GPOINTER_TO_INT(g_hash_table_lookup(map,
							GINT_TO_POINTER(acc)));
				if ((c == 0) && (acc != 0)) {
					_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
							"%04lx -(%c)-> "
							"%04lx(?)\n",
							acc,
							state->g[current] & 0xff,
							acc);
					c = acc;
				} else {
					width = 0;
					if (force_width != 0) {
						width = force_width;
					} else {
						if (G_UNLIKELY (_vte_iso2022_is_ambiguous(c))) {
							width = ambiguous_width;
						}
					}
					_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
							"%05lx -> "
							"%04x\n", acc, c);
					c = _vte_iso2022_set_encoded_width(c, width);
				}
				g_array_index(gunichars, gunichar, j++) = c;
				if (single) {
					break;
				}
				acc = 0;
			}
		} while (i < length);
		processed = i;
		gunichars->len = j;
	}
	return processed;
}

gunichar
_vte_iso2022_process_single(struct _vte_iso2022_state *state,
			    gunichar c, gunichar map)
{
	GHashTable *hash;
	gunichar ret = c;
	gpointer p;
	guint bytes_per_char, force_width;
	gulong or_mask, and_mask;

	_vte_iso2022_map_get(map,
			     &hash, &bytes_per_char, &force_width,
			     &or_mask, &and_mask);

	p = GINT_TO_POINTER((c & and_mask) | or_mask);
	if (hash != NULL) {
		p = g_hash_table_lookup(hash, p);
	}
	if (p != NULL) {
		ret = GPOINTER_TO_INT(p);
	}
	if (force_width) {
		ret = _vte_iso2022_set_encoded_width(ret, force_width);
	}
	return ret;
}

static void
process_control(struct _vte_iso2022_state *state, guchar *ctl, gsize length,
		GArray *gunichars)
{
	gunichar c;
	gsize i;
	if (length >= 1) {
		switch (ctl[0]) {
		case '\r':  /* CR */
			c = '\r';
			g_array_append_val(gunichars, c);
			_vte_debug_print(VTE_DEBUG_SUBSTITUTION, "\tCR\n");
			break;
		case '\n':  /* LF */
			c = '\n';
			g_array_append_val(gunichars, c);
			_vte_debug_print(VTE_DEBUG_SUBSTITUTION, "\tLF\n");
			break;
		case '\016': /* SO */
			state->current = 1;
			state->override = -1;
			_vte_debug_print(VTE_DEBUG_SUBSTITUTION, "\tSO (^N)\n");
			break;
		case '\017': /* SI */
			state->current = 0;
			state->override = -1;
			_vte_debug_print(VTE_DEBUG_SUBSTITUTION, "\tSI (^O)\n");
			break;
		case 0x8e:
			/* SS2 - 8bit */
			state->override = 2;
			_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
					"\tSS2 (8-bit)\n");
			break;
		case 0x8f:
			/* SS3 - 8bit */
			state->override = 3;
			_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
				"\tSS3 (8-bit)\n");
			break;
		case '\033':
			if (length >= 2) {
				switch (ctl[1]) {
				case '[': /* CSI */
				case ']': /* OSC */
				case '^': /* PM */
				case 'P': /* DCS */
					/* Pass it through. */
					for (i = 0; i < length; i++) {
						c = (guchar) ctl[i];
						g_array_append_val(gunichars,
								   c);
					}
					_VTE_DEBUG_IF(VTE_DEBUG_SUBSTITUTION) {
						g_printerr("\t");
						for (i = 0; i < length; i++) {
							c = (guchar) ctl[i];
							g_printerr(
								"(%s%c)",
								c < 0x20 ?
								"^" : "",
								c < 0x20 ?
								c : c + 64);
						}
						g_printerr("\n");
					}
					break;
				case 'N': /* SS2 */
					state->override = 2;
					_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
						"\tSS2\n");
					break;
				case 'O': /* SS3 */
					state->override = 3;
					_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
						"\tSS3\n");
					break;
				case 'n': /* LS2 */
					state->current = 2;
					state->override = -1;
					_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
							"\tLS2\n");
					break;
				case 'o': /* LS3 */
					state->current = 3;
					state->override = -1;
					_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
							"\tLS3\n");
					break;
				case '(':
				case ')':
				case '*':
				case '+':
					if (length >= 3) {
						int g = -1;
						switch (ctl[1]) {
						case '(':
							g = 0;
							break;
						case ')':
							g = 1;
							break;
						case '*':
							g = 2;
							break;
						case '+':
							g = 3;
							break;
						default:
							g_assert_not_reached();
							break;
						}
						/* strchr(NARROW_MAPS, c) */
						switch (ctl[2]) {
						case '0':
						case '1':
						case '2':
						case 'A':
						case 'B':
						case '4':
						case 'C':
						case '5':
						case 'R':
						case 'Q':
						case 'K':
						case 'Y':
						case 'E':
						case '6':
						case 'Z':
						case 'H':
						case '7':
						case '=':
						case 'J':
						case 'U':
							state->g[g] = ctl[2];
							break;

						default:
							g_warning(_("Attempt to set invalid NRC map '%c'."), ctl[2]);
							break;
						}
						_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
								"\tG[%d] = %c.\n",
								g, c);
					}
					break;
				case '%':
					if (length >= 3) {
						gboolean notify = FALSE;
						switch (ctl[2]) {
						case '@':
							if (strcmp(state->codeset, state->native_codeset) != 0) {
								notify = TRUE;
							}
							_vte_iso2022_state_set_codeset(state, state->native_codeset);
							_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
									"\tNative encoding.\n");
							break;
						case 'G':
							if (strcmp(state->codeset, state->utf8_codeset) != 0) {
								notify = TRUE;
							}
							_vte_iso2022_state_set_codeset(state, state->utf8_codeset);
							_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
									"\tUTF-8 encoding.\n");
							break;
						default:
							/* Application signalled an "identified coding system" we haven't heard of.  See ECMA-35 for gory details. */
							g_warning(_("Unrecognized identified coding system."));
							break;
						}
						if (notify &&
						    state->codeset_changed) {
							state->codeset_changed(state, state->codeset_changed_data);
						}
					}
					break;
				case '$':
					if (length >= 4) {
						int g = -1;
						c = 0;
						switch (ctl[2]) {
						case '@':
							g = 0;
							c = '@';
							break;
						case 'B':
							g = 0;
							c = 'B';
							break;
						case '(':
							g = 0;
							break;
						case ')':
							g = 1;
							break;
						case '*':
							g = 2;
							break;
						case '+':
							g = 3;
							break;
						default:
							g_assert_not_reached();
							break;
						}
						if (c == 0) {
							c = ctl[3];
						}
						/* strchr(WIDE_MAPS WIDE_GMAPS, c) */
						switch (c) {
						case '@':
						case 'B':
						case 'C':
						case 'A':
						case 'G':
						case 'H':
						case 'I':
						case 'J':
						case 'K':
						case 'L':
						case 'M':
						case 'D':
							state->g[g] = c + WIDE_FUDGE;
							break;

						default:
							g_warning(_("Attempt to set invalid wide NRC map '%c'."), c);
							break;
						}
						_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
								"\tG[%d] = wide %c.\n",
								g, c);
					} else
					if (length >= 3) {
						switch (ctl[2]) {
						case '@':
							c = '@';
							break;
						case 'B':
							c = 'B';
							break;
						default:
							c = ctl[2];
							break;
						}
						/* strchr(WIDE_MAPS, c) */
						switch (c){
						case '@':
						case 'B':
							state->g[0] = c + WIDE_FUDGE;
							break;

						default:
							g_warning(_("Attempt to set invalid wide NRC map '%c'."), c);
						}
						_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
								"\tG[0] = wide %c.\n",
								c);
					}
					break;
				default:
					g_assert_not_reached();
					break;
				}
				break;
			}
			break;
		default:
			g_assert_not_reached();
			break;
		}
	}
}

static guint
process_block (struct _vte_iso2022_state *state,
	       guchar *input,
	       struct _vte_iso2022_block *block,
	       gboolean last,
	       GArray *gunichars)
{
	guint preserve_last = -1;
	guint initial;

	switch (block->type) {
	case _vte_iso2022_cdata:
		_VTE_DEBUG_IF(VTE_DEBUG_SUBSTITUTION) {
			guint j;
			g_printerr("%3ld %3ld CDATA \"%.*s\"",
				block->start, block->end,
				(int) (block->end - block->start),
				input + block->start);
			g_printerr(" (");
			for (j = block->start; j < block->end; j++) {
				if (j > block->start) {
					g_printerr(", ");
				}
				g_printerr("0x%02x",
					input[j]);
			}
			g_printerr(")\n");
		}
		initial = 0;
		while (initial < block->end - block->start) {
			int j;
			j = process_cdata(state,
					  input +
					  block->start +
					  initial,
					  block->end -
					  block->start -
					  initial,
					  gunichars);
			if (j == 0) {
				break;
			}
			initial += j;
		}
		if (initial < block->end - block->start && last) {
			preserve_last = block->start + initial;
		}
		break;
	case _vte_iso2022_control:
		_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
				"%3ld %3ld CONTROL ",
				block->start, block->end);
		process_control(state,
				input + block->start,
				block->end - block->start,
				gunichars);
		break;
	case _vte_iso2022_preserve:
		_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
				"%3ld %3ld PRESERVE\n",
				block->start, block->end);
		preserve_last = block->start;
		break;
	default:
		g_assert_not_reached();
		break;
	}

	return preserve_last;
}

gsize
_vte_iso2022_process(struct _vte_iso2022_state *state,
		     guchar *input, gsize length,
		     GArray *gunichars)
{
	struct _vte_iso2022_block block;
	guint preserve_last = -1;
	const guchar *nextctl, *p, *q;
	glong sequence_length = 0;

	p = input;
	q = input + length;
	do {
		nextctl = _vte_iso2022_find_nextctl(p, q);
		if (nextctl == NULL) {
			/* It's all garden-variety data. */
			block.type = _vte_iso2022_cdata;
			block.start = p - input;
			block.end = q - input;
			preserve_last = process_block (state,
					               input, &block,
						       TRUE,
					               gunichars);
			break;
		}
		/* We got some garden-variety data. */
		if (nextctl != p) {
			block.type = _vte_iso2022_cdata;
			block.start = p - input;
			block.end = nextctl - input;
			process_block (state, input, &block, FALSE, gunichars);
		}
		/* Move on to the control data. */
		p = nextctl;
		sequence_length = _vte_iso2022_sequence_length(nextctl,
							       q - nextctl);
		switch (sequence_length) {
		case -1:
			/* It's just garden-variety data. */
			block.type = _vte_iso2022_cdata;
			block.start = p - input;
			block.end = nextctl + 1 - input;
			/* Continue at the next byte. */
			p = nextctl + 1;
			break;
		case 0:
			/* Inconclusive.  Save this data and try again later. */
			block.type = _vte_iso2022_preserve;
			block.start = nextctl - input;
			block.end = q - input;
			/* Trigger an end-of-loop. */
			p = q;
			break;
		default:
			/* It's a control sequence. */
			block.type = _vte_iso2022_control;
			block.start = nextctl - input;
			block.end = nextctl + sequence_length - input;
			/* Continue after the sequence. */
			p = nextctl + sequence_length;
			break;
		}
		preserve_last = process_block (state,
				               input, &block,
					       FALSE,
					       gunichars);
	} while (p < q);
	if (preserve_last != (guint) -1) {
		length = preserve_last;
	}
	_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
			"Consuming %ld bytes.\n", (long) length);
	return length;
}

#ifdef ISO2022_MAIN
#include <stdio.h>
int
main(int argc, char **argv)
{
	VteBuffer *buffer;
	struct _vte_iso2022_state *state;
	GString *string;
	GArray *gunichars;
	struct {
		const char *s;
		gboolean process;
	} strings[] = {
		{"abcd\033$(Cefgh\ri\nj\033)0k\017lmn\033Nopqrst\033%G", TRUE},
		{"ABCD\033$(C\033)", TRUE},
		{"0", TRUE},
		{"\014\033[33;41m", TRUE},
		{"\015", TRUE},
		{"\014{|}\015\r\n", TRUE},
		{"\033(B\033)0\033*B\033+B", TRUE},
		{"\033$B$+$J4A;z\033(J~", TRUE},
		{"\033(B\033)0\033*B\033+B", TRUE},
		{"\033$)C\0161hD!\017", TRUE},
		{"\033$*C\033N1hD!", TRUE},
		{"\033$(G\043\071", TRUE},
		{"\033(B\033)0\033*B\033+B", TRUE},
		{"\r\n", TRUE},
	};
	guint i;
	FILE *fp;
	guchar b;

	state = _vte_iso2022_state_new(NULL, NULL, NULL);
	buffer = _vte_buffer_new();
	gunichars = g_array_new(FALSE, FALSE, sizeof(gunichar));
	if (argc > 1) {
		string = g_string_new(NULL);
		for (i = 1; i < (guint) argc; i++) {
			if (strcmp(argv[i], "-") == 0) {
				fp = stdin;
			} else {
				fp = fopen(argv[i], "r");
			}
			while (fread(&b, sizeof(guchar), 1, fp) == sizeof(b)) {
				g_string_append_c(string, b);
			}
			if (fp != stdin) {
				fclose(fp);
			}
		}
		_vte_buffer_append(buffer, string->str, string->len);
		_vte_iso2022_process(state, buffer->data, _vte_buffer_length (buffer), gunichars);
		g_string_free(string, TRUE);
	} else {
		for (i = 0; i < G_N_ELEMENTS(strings); i++) {
			string = g_string_new(strings[i].s);
			_vte_buffer_append(buffer, string->str, string->len);
			g_string_free(string, TRUE);
			if (strings[i].process) {
				_vte_iso2022_process(state, buffer->data, _vte_buffer_length (buffer), gunichars);
			}
		}
	}
	_vte_buffer_free(buffer);
	_vte_iso2022_state_free(state);

	string = g_string_new(NULL);
	for (i = 0; i < gunichars->len; i++) {
		g_string_append_unichar(string,
					g_array_index(gunichars, gunichar, i));
	}
	(void) write(STDOUT_FILENO, string->str, string->len);
	g_string_free(string, TRUE);

	return 0;
}
#endif

/* Reset the handlers for all known signals to their defaults.  The parent
 * (or one of the libraries it links to) may have changed one to be ignored. */
static void
_vte_pty_reset_signal_handlers(void)
{
#ifdef SIGHUP
	signal(SIGHUP,  SIG_DFL);
#endif
	signal(SIGINT,  SIG_DFL);
	signal(SIGILL,  SIG_DFL);
	signal(SIGABRT, SIG_DFL);
	signal(SIGFPE,  SIG_DFL);
#ifdef SIGKILL
	signal(SIGKILL, SIG_DFL);
#endif
	signal(SIGSEGV, SIG_DFL);
#ifdef SIGPIPE
	signal(SIGPIPE, SIG_DFL);
#endif
#ifdef SIGALRM
	signal(SIGALRM, SIG_DFL);
#endif
	signal(SIGTERM, SIG_DFL);
#ifdef SIGCHLD
	signal(SIGCHLD, SIG_DFL);
#endif
#ifdef SIGCONT
	signal(SIGCONT, SIG_DFL);
#endif
#ifdef SIGSTOP
	signal(SIGSTOP, SIG_DFL);
#endif
#ifdef SIGTSTP
	signal(SIGTSTP, SIG_DFL);
#endif
#ifdef SIGTTIN
	signal(SIGTTIN, SIG_DFL);
#endif
#ifdef SIGTTOU
	signal(SIGTTOU, SIG_DFL);
#endif
#ifdef SIGBUS
	signal(SIGBUS,  SIG_DFL);
#endif
#ifdef SIGPOLL
	signal(SIGPOLL, SIG_DFL);
#endif
#ifdef SIGPROF
	signal(SIGPROF, SIG_DFL);
#endif
#ifdef SIGSYS
	signal(SIGSYS,  SIG_DFL);
#endif
#ifdef SIGTRAP
	signal(SIGTRAP, SIG_DFL);
#endif
#ifdef SIGURG
	signal(SIGURG,  SIG_DFL);
#endif
#ifdef SIGVTALARM
	signal(SIGVTALARM, SIG_DFL);
#endif
#ifdef SIGXCPU
	signal(SIGXCPU, SIG_DFL);
#endif
#ifdef SIGXFSZ
	signal(SIGXFSZ, SIG_DFL);
#endif
#ifdef SIGIOT
	signal(SIGIOT,  SIG_DFL);
#endif
#ifdef SIGEMT
	signal(SIGEMT,  SIG_DFL);
#endif
#ifdef SIGSTKFLT
	signal(SIGSTKFLT, SIG_DFL);
#endif
#ifdef SIGIO
	signal(SIGIO,   SIG_DFL);
#endif
#ifdef SIGCLD
	signal(SIGCLD,  SIG_DFL);
#endif
#ifdef SIGPWR
	signal(SIGPWR,  SIG_DFL);
#endif
#ifdef SIGINFO
	signal(SIGINFO, SIG_DFL);
#endif
#ifdef SIGLOST
	signal(SIGLOST, SIG_DFL);
#endif
#ifdef SIGWINCH
	signal(SIGWINCH, SIG_DFL);
#endif
#ifdef SIGUNUSED
	signal(SIGUNUSED, SIG_DFL);
#endif
}

typedef struct _VtePtyPrivate VtePtyPrivate;

typedef struct {
	enum {
		TTY_OPEN_BY_NAME,
		TTY_OPEN_BY_FD
	} mode;
	union {
		const char *name;
		int fd;
	} tty;
} VtePtyChildSetupData;

/**
 * VtePty:
 *
 * Since: 0.26
 */
struct _VtePty {
        GObject parent_instance;

        /* <private> */
        VtePtyPrivate *priv;
};

struct _VtePtyPrivate {
        VtePtyFlags flags;
        int pty_fd;

        const char *term;
        VtePtyChildSetupData child_setup_data;

        gpointer helper_tag; /* only use when using_helper is TRUE */

        guint utf8 : 1;
        guint foreign : 1;
        guint using_helper : 1;
};

struct _VtePtyClass {
        GObjectClass parent_class;
};

/**
 * vte_pty_child_setup:
 * @pty: a #VtePty
 *
 * FIXMEchpe
 *
 * Since: 0.26
 */
void
vte_pty_child_setup (VtePty *pty)
{
        VtePtyPrivate *priv = pty->priv;
	VtePtyChildSetupData *data = &priv->child_setup_data;
	int fd = -1;
	const char *tty = NULL;

        if (priv->foreign) {
                fd = priv->pty_fd;
        } else {
                /* Save the name of the pty -- we'll need it later to acquire
                * it as our controlling terminal.
                */
                switch (data->mode) {
                        case TTY_OPEN_BY_NAME:
                                tty = data->tty.name;
                                break;
                        case TTY_OPEN_BY_FD:
                                fd = data->tty.fd;
                                tty = ttyname(fd);
                                break;
                }

                _vte_debug_print (VTE_DEBUG_PTY,
                                "Setting up child pty: name = %s, fd = %d\n",
                                        tty ? tty : "(none)", fd);


                /* Try to reopen the pty to acquire it as our controlling terminal. */
                /* FIXMEchpe: why not just use the passed fd in TTY_OPEN_BY_FD mode? */
                if (tty != NULL) {
                        int i = open(tty, O_RDWR);
                        if (i != -1) {
                                if (fd != -1){
                                        close(fd);
                                }
                                fd = i;
                        }
                }
        }

	if (fd == -1)
		_exit (127);

	/* Start a new session and become process-group leader. */
#if defined(HAVE_SETSID) && defined(HAVE_SETPGID)
	_vte_debug_print (VTE_DEBUG_PTY, "Starting new session\n");
	setsid();
	setpgid(0, 0);
#endif

#ifdef TIOCSCTTY
	/* TIOCSCTTY is defined?  Let's try that, too. */
	ioctl(fd, TIOCSCTTY, fd);
#endif

#ifdef HAVE_STROPTS_H
	if (isastream (fd) == 1) {
		if ((ioctl(fd, I_FIND, "ptem") == 0) &&
				(ioctl(fd, I_PUSH, "ptem") == -1)) {
			_exit (127);
		}
		if ((ioctl(fd, I_FIND, "ldterm") == 0) &&
				(ioctl(fd, I_PUSH, "ldterm") == -1)) {
			_exit (127);
		}
		if ((ioctl(fd, I_FIND, "ttcompat") == 0) &&
				(ioctl(fd, I_PUSH, "ttcompat") == -1)) {
			perror ("ioctl (fd, I_PUSH, \"ttcompat\")");
			_exit (127);
		}
	}
#endif

	/* now setup child I/O through the tty */
	if (fd != STDIN_FILENO) {
		if (dup2(fd, STDIN_FILENO) != STDIN_FILENO){
			_exit (127);
		}
	}
	if (fd != STDOUT_FILENO) {
		if (dup2(fd, STDOUT_FILENO) != STDOUT_FILENO){
			_exit (127);
		}
	}
	if (fd != STDERR_FILENO) {
		if (dup2(fd, STDERR_FILENO) != STDERR_FILENO){
			_exit (127);
		}
	}

	/* Close the original slave descriptor, unless it's one of the stdio
	 * descriptors. */
	if (fd != STDIN_FILENO &&
			fd != STDOUT_FILENO &&
			fd != STDERR_FILENO) {
		close(fd);
	}

	/* Reset our signals -- our parent may have done any number of
	 * weird things to them. */
	_vte_pty_reset_signal_handlers();

        /* Now set the TERM environment variable */
        if (priv->term != NULL) {
                g_setenv("TERM", priv->term, TRUE);
        }
}

/* TODO: clean up the spawning
 * - replace current env rather than adding!
 */

/*
 * __vte_pty_get_argv:
 * @command: the command to run
 * @argv: the argument vector
 * @flags: (inout) flags from #GSpawnFlags
 *
 * Creates an argument vector to pass to g_spawn_async() from @command and
 * @argv, modifying *@flags if necessary.
 *
 * Returns: a newly allocated array of strings. Free with g_strfreev()
 */
char **
__vte_pty_get_argv (const char *command,
                    char **argv,
                    GSpawnFlags *flags /* inout */)
{
	char **argv2;
	int i, argc;

        g_return_val_if_fail(command != NULL, NULL);

	/* push the command into argv[0] */
	argc = argv ? g_strv_length (argv) : 0;
	argv2 = g_new (char *, argc + 2);

	argv2[0] = g_strdup (command);

	for (i = 0; i < argc; i++) {
		argv2[i+1] = g_strdup (argv[i]);
	}
	argv2[i+1] = NULL;

        if (argv) {
                *flags |= G_SPAWN_FILE_AND_ARGV_ZERO;
        }

	return argv2;
}

/*
 * __vte_pty_merge_environ:
 * @envp: environment vector
 *
 * Merges @envp to the parent environment, and returns a new environment vector.
 *
 * Returns: a newly allocated string array. Free using g_strfreev()
 */
static gchar **
__vte_pty_merge_environ (char **envp, const char *term_value)
{
	GHashTable *table;
        GHashTableIter iter;
        char *name, *value;
	gchar **parent_environ;
	GPtrArray *array;
	gint i;

	table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

	parent_environ = g_listenv ();
	for (i = 0; parent_environ[i] != NULL; i++) {
		g_hash_table_replace (table,
			              g_strdup (parent_environ[i]),
				      g_strdup (g_getenv (parent_environ[i])));
	}
	g_strfreev (parent_environ);

	if (envp != NULL) {
		for (i = 0; envp[i] != NULL; i++) {
			name = g_strdup (envp[i]);
			value = strchr (name, '=');
			if (value) {
				*value = '\0';
				value = g_strdup (value + 1);
			}
			g_hash_table_replace (table, name, value);
		}
	}

        if (term_value != NULL)
                g_hash_table_replace (table, g_strdup ("TERM"), g_strdup (term_value));

	array = g_ptr_array_sized_new (g_hash_table_size (table) + 1);
        g_hash_table_iter_init(&iter, table);
        while (g_hash_table_iter_next(&iter, (gpointer) &name, (gpointer) &value)) {
                g_ptr_array_add (array, g_strconcat (name, "=", value, NULL));
        }
        g_assert(g_hash_table_size(table) == array->len);
	g_hash_table_destroy (table);
	g_ptr_array_add (array, NULL);

	return (gchar **) g_ptr_array_free (array, FALSE);
}

/*
 * __vte_pty_get_pty_flags:
 * @lastlog: %TRUE if the session should be logged to the lastlog
 * @utmp: %TRUE if the session should be logged to the utmp/utmpx log
 * @wtmp: %TRUE if the session should be logged to the wtmp/wtmpx log
 *
 * Combines the @lastlog, @utmp, @wtmp arguments into the coresponding
 * #VtePtyFlags flags.
 *
 * Returns: flags from #VtePtyFlags
 */
VtePtyFlags
__vte_pty_get_pty_flags(gboolean lastlog,
                        gboolean utmp,
                        gboolean wtmp)
{
        VtePtyFlags flags = VTE_PTY_DEFAULT;

        if (!lastlog)
                flags |= VTE_PTY_NO_LASTLOG;
        if (!utmp)
                flags |= VTE_PTY_NO_UTMP;
        if (!wtmp)
                flags |= VTE_PTY_NO_WTMP;

        return flags;
}

/*
 * __vte_pty_spawn:
 * @pty: a #VtePty
 * @directory: the name of a directory the command should start in, or %NULL
 *   to use the cwd
 * @argv: child's argument vector
 * @envv: a list of environment variables to be added to the environment before
 *   starting the process, or %NULL
 * @spawn_flags: flags from #GSpawnFlags
 * @child_setup: function to run in the child just before exec()
 * @child_setup_data: user data for @child_setup
 * @child_pid: a location to store the child PID, or %NULL
 * @error: return location for a #GError, or %NULL
 *
 * Uses g_spawn_async() to spawn the command in @argv. The child's environment will
 * be the parent environment with the variables in @envv set afterwards.
 *
 * Enforces the vte_terminal_watch_child() requirements by adding
 * %G_SPAWN_DO_NOT_REAP_CHILD to @spawn_flags.
 *
 * Note that the %G_SPAWN_LEAVE_DESCRIPTORS_OPEN flag is not supported;
 * it will be cleared!
 *
 * If spawning the command in @working_directory fails because the child
 * is unable to chdir() to it, falls back trying to spawn the command
 * in the parent's working directory.
 *
 * Returns: %TRUE on success, or %FALSE on failure with @error filled in
 */
gboolean
__vte_pty_spawn (VtePty *pty,
                 const char *directory,
                 char **argv,
                 char **envv,
                 GSpawnFlags spawn_flags,
                 GSpawnChildSetupFunc child_setup,
                 gpointer child_setup_data,
                 GPid *child_pid /* out */,
                 GError **error)
{
	gboolean ret = TRUE;
        char **envp2;
        gint i;
        GError *err = NULL;

        spawn_flags |= G_SPAWN_DO_NOT_REAP_CHILD;

        /* FIXMEchpe: Enforce this until I've checked our code to make sure
         * it doesn't leak out internal FDs into the child this way.
         */
        spawn_flags &= ~G_SPAWN_LEAVE_DESCRIPTORS_OPEN;

        /* add the given environment to the childs */
        envp2 = __vte_pty_merge_environ (envv, pty->priv->term);

        _VTE_DEBUG_IF (VTE_DEBUG_MISC) {
                g_printerr ("Spawing command:\n");
                for (i = 0; argv[i] != NULL; i++) {
                        g_printerr ("    argv[%d] = %s\n", i, argv[i]);
                }
                for (i = 0; envp2[i] != NULL; i++) {
                        g_printerr ("    env[%d] = %s\n", i, envp2[i]);
                }
                g_printerr ("    directory: %s\n",
                            directory ? directory : "(none)");
        }

        ret = g_spawn_async_with_pipes(directory,
                                       argv, envp2,
                                       spawn_flags,
                                       child_setup ? child_setup
                                                   : (GSpawnChildSetupFunc) vte_pty_child_setup,
                                       child_setup ? child_setup_data : pty,
                                       child_pid,
                                       NULL, NULL, NULL,
                                       &err);
        if (!ret &&
            directory != NULL &&
            g_error_matches(err, G_SPAWN_ERROR, G_SPAWN_ERROR_CHDIR)) {
                /* try spawning in our working directory */
                g_clear_error(&err);
                ret = g_spawn_async_with_pipes(NULL,
                                               argv, envp2,
                                               spawn_flags,
                                               child_setup ? child_setup
                                                           : (GSpawnChildSetupFunc) vte_pty_child_setup,
                                               child_setup ? child_setup_data : pty,
                                               child_pid,
                                               NULL, NULL, NULL,
                                               &err);
        }

        g_strfreev (envp2);

        if (ret)
                return TRUE;

        g_propagate_error (error, err);
        return FALSE;
}

/*
 * __vte_pty_fork:
 * @pty: a #VtePty
 * @pid: (out) a location to store a #GPid, or %NULL
 * @error: a location to store a #GError, or %NULL
 *
 * Forks and calls vte_pty_child_setup() in the child.
 *
 * Returns: %TRUE on success, or %FALSE on failure with @error filled in
 */
gboolean
__vte_pty_fork(VtePty *pty,
               GPid *pid,
               GError **error)
{
#ifdef HAVE_FORK
        gboolean ret = TRUE;

        *pid = fork();
        switch (*pid) {
                case -1:
                        g_set_error(error,
                                    G_SPAWN_ERROR,
                                    G_SPAWN_ERROR_FAILED,
                                    "Unable to fork: %s",
                                    g_strerror(errno));
                        ret = FALSE;
                case 0: /* child */
                        vte_pty_child_setup(pty);
                        break;
                default: /* parent */
                        break;
        }

	return ret;
#else /* !HAVE_FORK */
        g_set_error_literal(error, G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
                            "No fork implementation");
        return FALSE;
#endif /* HAVE_FORK */
}

/**
 * vte_pty_set_size:
 * @pty: a #VtePty
 * @rows: the desired number of rows
 * @columns: the desired number of columns
 * @error: (allow-none); return location to store a #GError, or %NULL
 *
 * Attempts to resize the pseudo terminal's window size.  If successful, the
 * OS kernel will send #SIGWINCH to the child process group.
 *
 * If setting the window size failed, @error will be set to a #GIOError.
 *
 * Returns: %TRUE on success, %FALSE on failure with @error filled in
 *
 * Since: 0.26
 */
gboolean
vte_pty_set_size(VtePty *pty,
                 int rows,
                 int columns,
                 GError **error)
{
        VtePtyPrivate *priv;
	struct winsize size;
        int master;
	int ret;

        g_return_val_if_fail(VTE_IS_PTY(pty), FALSE);

        priv = pty->priv;

        master = vte_pty_get_fd(pty);

	memset(&size, 0, sizeof(size));
	size.ws_row = rows > 0 ? rows : 24;
	size.ws_col = columns > 0 ? columns : 80;
	_vte_debug_print(VTE_DEBUG_PTY,
			"Setting size on fd %d to (%d,%d).\n",
			master, columns, rows);
	ret = ioctl(master, TIOCSWINSZ, &size);
	if (ret != 0) {
                int errsv = errno;

                g_set_error(error, G_IO_ERROR,
                            g_io_error_from_errno(errsv),
                            "Failed to set window size: %s",
                            g_strerror(errsv));

		_vte_debug_print(VTE_DEBUG_PTY,
				"Failed to set size on %d: %s.\n",
				master, g_strerror(errsv));

                errno = errsv;

                return FALSE;
	}

        return TRUE;
}

/**
 * vte_pty_get_size:
 * @pty: a #VtePty
 * @rows: (out) (allow-none): a location to store the number of rows, or %NULL
 * @columns: (out) (allow-none): a location to store the number of columns, or %NULL
 * @error: return location to store a #GError, or %NULL
 *
 * Reads the pseudo terminal's window size.
 *
 * If getting the window size failed, @error will be set to a #GIOError.
 *
 * Returns: %TRUE on success, %FALSE on failure with @error filled in
 *
 * Since: 0.26
 */
gboolean
vte_pty_get_size(VtePty *pty,
                 int *rows,
                 int *columns,
                 GError **error)
{
        VtePtyPrivate *priv;
	struct winsize size;
        int master;
	int ret;

        g_return_val_if_fail(VTE_IS_PTY(pty), FALSE);

        priv = pty->priv;

        master = vte_pty_get_fd(pty);

	memset(&size, 0, sizeof(size));
	ret = ioctl(master, TIOCGWINSZ, &size);
	if (ret == 0) {
		if (columns != NULL) {
			*columns = size.ws_col;
		}
		if (rows != NULL) {
			*rows = size.ws_row;
		}
		_vte_debug_print(VTE_DEBUG_PTY,
				"Size on fd %d is (%d,%d).\n",
				master, size.ws_col, size.ws_row);
                return TRUE;
	} else {
                int errsv = errno;

                g_set_error(error, G_IO_ERROR,
                            g_io_error_from_errno(errsv),
                            "Failed to get window size: %s",
                            g_strerror(errsv));

		_vte_debug_print(VTE_DEBUG_PTY,
				"Failed to read size from fd %d: %s\n",
				master, g_strerror(errsv));

                errno = errsv;

                return FALSE;
	}
}

/*
 * _vte_pty_ptsname:
 * @master: file descriptor to the PTY master
 * @error: a location to store a #GError, or %NULL
 *
 * Returns: a newly allocated string containing the file name of the
 *   PTY slave device, or %NULL on failure with @error filled in
 */
static char *
_vte_pty_ptsname(int master,
                 GError **error)
{
#if defined(HAVE_PTSNAME_R)
	gsize len = 1024;
	char *buf = NULL;
	int i, errsv;
	do {
		buf = g_malloc0(len);
		i = ptsname_r(master, buf, len - 1);
		switch (i) {
		case 0:
			/* Return the allocated buffer with the name in it. */
			_vte_debug_print(VTE_DEBUG_PTY,
					"PTY slave is `%s'.\n", buf);
			return buf;
			break;
		default:
                        errsv = errno;
			g_free(buf);
                        errno = errsv;
			buf = NULL;
			break;
		}
		len *= 2;
	} while ((i != 0) && (errno == ERANGE));

        g_set_error(error, VTE_PTY_ERROR, VTE_PTY_ERROR_PTY98_FAILED,
                    "%s failed: %s", "ptsname_r", g_strerror(errno));
        return NULL;
#elif defined(HAVE_PTSNAME)
	char *p;
	if ((p = ptsname(master)) != NULL) {
		_vte_debug_print(VTE_DEBUG_PTY, "PTY slave is `%s'.\n", p);
		return g_strdup(p);
	}

        g_set_error(error, VTE_PTY_ERROR, VTE_PTY_ERROR_PTY98_FAILED,
                    "%s failed: %s", "ptsname", g_strerror(errno));
        return NULL;
#elif defined(TIOCGPTN)
	int pty = 0;
	if (ioctl(master, TIOCGPTN, &pty) == 0) {
		_vte_debug_print(VTE_DEBUG_PTY,
				"PTY slave is `/dev/pts/%d'.\n", pty);
		return g_strdup_printf("/dev/pts/%d", pty);
	}

        g_set_error(error, VTE_PTY_ERROR, VTE_PTY_ERROR_PTY98_FAILED,
                    "%s failed: %s", "ioctl(TIOCGPTN)", g_strerror(errno));
        return NULL;
#else
#error no ptsname implementation for this platform
#endif
}

/*
 * _vte_pty_getpt:
 * @error: a location to store a #GError, or %NULL
 *
 * Opens a file descriptor for the next available PTY master.
 * Sets the descriptor to blocking mode!
 *
 * Returns: a new file descriptor, or %-1 on failure
 */
static int
_vte_pty_getpt(GError **error)
{
	int fd, flags, rv;
#ifdef HAVE_GETPT
	/* Call the system's function for allocating a pty. */
	fd = getpt();
#else
	/* Try to allocate a pty by accessing the pty master multiplex. */
	fd = open("/dev/ptmx", O_RDWR | O_NOCTTY);
	if ((fd == -1) && (errno == ENOENT)) {
		fd = open("/dev/ptc", O_RDWR | O_NOCTTY); /* AIX */
	}
#endif
        if (fd == -1) {
                g_set_error (error, VTE_PTY_ERROR,
                             VTE_PTY_ERROR_PTY98_FAILED,
                             "%s failed: %s", "getpt", g_strerror(errno));
                return -1;
        }

        rv = fcntl(fd, F_GETFL, 0);
        if (rv < 0) {
                int errsv = errno;
                g_set_error(error, VTE_PTY_ERROR,
                            VTE_PTY_ERROR_PTY98_FAILED,
                            "%s failed: %s", "fcntl(F_GETFL)", g_strerror(errno));
                close(fd);
                errno = errsv;
                return -1;
        }

	/* Set it to blocking. */
        /* FIXMEchpe: why?? vte_terminal_set_pty does the inverse... */
        flags = rv & ~(O_NONBLOCK);
        rv = fcntl(fd, F_SETFL, flags);
        if (rv < 0) {
                int errsv = errno;
                g_set_error(error, VTE_PTY_ERROR,
                            VTE_PTY_ERROR_PTY98_FAILED,
                            "%s failed: %s", "fcntl(F_SETFL)", g_strerror(errno));
                close(fd);
                errno = errsv;
                return -1;
        }

	return fd;
}

static gboolean
_vte_pty_grantpt(int master,
                 GError **error)
{
#ifdef HAVE_GRANTPT
        int rv;

        rv = grantpt(master);
        if (rv != 0) {
                int errsv = errno;
                g_set_error(error, VTE_PTY_ERROR, VTE_PTY_ERROR_PTY98_FAILED,
                            "%s failed: %s", "grantpt", g_strerror(errsv));
                errno = errsv;
                return FALSE;
        }
#endif
        return TRUE;
}

static gboolean
_vte_pty_unlockpt(int fd,
                  GError **error)
{
        int rv;
#ifdef HAVE_UNLOCKPT
	rv = unlockpt(fd);
        if (rv != 0) {
                int errsv = errno;
                g_set_error(error, VTE_PTY_ERROR, VTE_PTY_ERROR_PTY98_FAILED,
                            "%s failed: %s", "unlockpt", g_strerror(errsv));
                errno = errsv;
                return FALSE;
        }
        return TRUE;
#elif defined(TIOCSPTLCK)
	int zero = 0;
	rv = ioctl(fd, TIOCSPTLCK, &zero);
        if (rv != 0) {
                int errsv = errno;
                g_set_error(error, VTE_PTY_ERROR, VTE_PTY_ERROR_PTY98_FAILED,
                            "%s failed: %s", "ioctl(TIOCSPTLCK)", g_strerror(errsv));
                errno = errsv;
                return FALSE;
        }
        return TRUE;
#else
#error no unlockpt implementation for this platform
#endif
}

/*
 * _vte_pty_open_unix98:
 * @pty: a #VtePty
 * @error: a location to store a #GError, or %NULL
 *
 * Opens a new file descriptor to a new PTY master.
 *
 * Returns: %TRUE on success, %FALSE on failure with @error filled in
 */
static gboolean
_vte_pty_open_unix98(VtePty *pty,
                     GError **error)
{
        VtePtyPrivate *priv = pty->priv;
	int fd;
	char *buf;

	/* Attempt to open the master. */
	fd = _vte_pty_getpt(error);
	if (fd == -1)
                return FALSE;

	_vte_debug_print(VTE_DEBUG_PTY, "Allocated pty on fd %d.\n", fd);

        /* Read the slave number and unlock it. */
        if ((buf = _vte_pty_ptsname(fd, error)) == NULL ||
            !_vte_pty_grantpt(fd, error) ||
            !_vte_pty_unlockpt(fd, error)) {
                int errsv = errno;
                _vte_debug_print(VTE_DEBUG_PTY,
                                "PTY setup failed, bailing.\n");
                close(fd);
                errno = errsv;
                return FALSE;
        }

        priv->pty_fd = fd;
        priv->child_setup_data.mode = TTY_OPEN_BY_NAME;
        priv->child_setup_data.tty.name = buf;
        priv->using_helper = FALSE;

        return TRUE;
}

#ifdef VTE_USE_GNOME_PTY_HELPER
#ifdef HAVE_RECVMSG
static void
_vte_pty_read_ptypair(int tunnel, int *parentfd, int *childfd)
{
	int i, ret;
	char control[LINE_MAX], iobuf[LINE_MAX];
	struct cmsghdr *cmsg;
	struct msghdr msg;
	struct iovec vec;

	for (i = 0; i < 2; i++) {
		vec.iov_base = iobuf;
		vec.iov_len = sizeof(iobuf);
		msg.msg_name = NULL;
		msg.msg_namelen = 0;
		msg.msg_iov = &vec;
		msg.msg_iovlen = 1;
		msg.msg_control = control;
		msg.msg_controllen = sizeof(control);
		ret = recvmsg(tunnel, &msg, PTY_RECVMSG_FLAGS);
		if (ret == -1) {
			return;
		}
		for (cmsg = CMSG_FIRSTHDR(&msg);
		     cmsg != NULL;
		     cmsg = CMSG_NXTHDR(&msg, cmsg)) {
			if (cmsg->cmsg_type == SCM_RIGHTS) {
				memcpy(&ret, CMSG_DATA(cmsg), sizeof(ret));
				switch (i) {
					case 0:
						*parentfd = ret;
						break;
					case 1:
						*childfd = ret;
						break;
					default:
						g_assert_not_reached();
						break;
				}
			}
		}
	}
}
#elif defined (I_RECVFD)
static void
_vte_pty_read_ptypair(int tunnel, int *parentfd, int *childfd)
{
	int i;
	if (ioctl(tunnel, I_RECVFD, &i) == -1) {
		return;
	}
	*parentfd = i;
	if (ioctl(tunnel, I_RECVFD, &i) == -1) {
		return;
	}
	*childfd = i;
}
#endif

#ifdef HAVE_SOCKETPAIR
static int
_vte_pty_pipe_open(int *a, int *b)
{
	int p[2], ret = -1;
#ifdef PF_UNIX
#ifdef SOCK_STREAM
	ret = socketpair(PF_UNIX, SOCK_STREAM, 0, p);
#else
#ifdef SOCK_DGRAM
	ret = socketpair(PF_UNIX, SOCK_DGRAM, 0, p);
#endif
#endif
	if (ret == 0) {
		*a = p[0];
		*b = p[1];
		return 0;
	}
#endif
	return ret;
}
#else
static int
_vte_pty_pipe_open(int *a, int *b)
{
	int p[2], ret = -1;

	ret = pipe(p);

	if (ret == 0) {
		*a = p[0];
		*b = p[1];
	}
	return ret;
}
#endif

/* Like read, but hide EINTR and EAGAIN. */
static ssize_t
n_read(int fd, void *buffer, size_t count)
{
	size_t n = 0;
	char *buf = buffer;
	int i;
	while (n < count) {
		i = read(fd, buf + n, count - n);
		switch (i) {
		case 0:
			return n;
		case -1:
			switch (errno) {
			case EINTR:
			case EAGAIN:
#ifdef ERESTART
			case ERESTART:
#endif
				break;
			default:
				return -1;
			}
			break;
		default:
			n += i;
			break;
		}
	}
	return n;
}

/* Like write, but hide EINTR and EAGAIN. */
static ssize_t
n_write(int fd, const void *buffer, size_t count)
{
	size_t n = 0;
	const char *buf = buffer;
	int i;
	while (n < count) {
		i = write(fd, buf + n, count - n);
		switch (i) {
		case 0:
			return n;
		case -1:
			switch (errno) {
			case EINTR:
			case EAGAIN:
#ifdef ERESTART
			case ERESTART:
#endif
				break;
			default:
				return -1;
			}
			break;
		default:
			n += i;
			break;
		}
	}
	return n;
}

/*
 * _vte_pty_stop_helper:
 *
 * Terminates the running GNOME PTY helper.
 */
static void
_vte_pty_stop_helper(void)
{
	if (_vte_pty_helper_started) {
		close(_vte_pty_helper_tunnel);
		_vte_pty_helper_tunnel = -1;
		kill(_vte_pty_helper_pid, SIGTERM);
		_vte_pty_helper_pid = -1;
		_vte_pty_helper_started = FALSE;
	}
}

/*
 * _vte_pty_start_helper:
 * @error: a location to store a #GError, or %NULL
 *
 * Starts the GNOME PTY helper process, if it is not already running.
 *
 * Returns: %TRUE if the helper was already started, or starting it succeeded,
 *   %FALSE on failure with @error filled in
 */
static gboolean
_vte_pty_start_helper(GError **error)
{
	int i, errsv;
        int tunnel = -1;
        int tmp[2] = { -1, -1 };

        if (_vte_pty_helper_started)
                return TRUE;

	/* Create a communication link for use with the helper. */
	tmp[0] = open("/dev/null", O_RDONLY);
	if (tmp[0] == -1) {
		goto failure;
	}
	tmp[1] = open("/dev/null", O_RDONLY);
	if (tmp[1] == -1) {
		goto failure;
	}
	if (_vte_pty_pipe_open(&_vte_pty_helper_tunnel, &tunnel) != 0) {
		goto failure;
	}
	close(tmp[0]);
	close(tmp[1]);
        tmp[0] = tmp[1] = -1;

	/* Now fork and start the helper. */
	_vte_pty_helper_pid = fork();
	if (_vte_pty_helper_pid == -1) {
		goto failure;
	}
	if (_vte_pty_helper_pid == 0) {
		/* Child.  Close descriptors.  No need to close all,
		 * gnome-pty-helper does that anyway. */
		for (i = 0; i < 3; i++) {
			close(i);
		}
		/* Reassign the socket pair to stdio. */
		dup2(tunnel, STDIN_FILENO);
		dup2(tunnel, STDOUT_FILENO);
		close(tunnel);
		close(_vte_pty_helper_tunnel);
		/* Exec our helper. */
		execl(LIBEXECDIR "/gnome-pty-helper",
		      "gnome-pty-helper", NULL);
		/* Bail. */
		_exit(1);
	}
	close(tunnel);
	atexit(_vte_pty_stop_helper);

        _vte_pty_helper_started = TRUE;
	return TRUE;

failure:
        errsv = errno;

        g_set_error(error, VTE_PTY_ERROR,
                    VTE_PTY_ERROR_PTY_HELPER_FAILED,
                    "Failed to start gnome-pty-helper: %s",
                    g_strerror (errsv));

        if (tmp[0] != -1)
                close(tmp[0]);
        if (tmp[1] != -1)
                close(tmp[1]);
        if (tunnel != -1)
                close(tunnel);
        if (_vte_pty_helper_tunnel != -1)
                close(_vte_pty_helper_tunnel);

        _vte_pty_helper_pid = -1;
        _vte_pty_helper_tunnel = -1;

        errno = errsv;
        return FALSE;
}

/*
 * _vte_pty_helper_ops_from_flags:
 * @flags: flags from #VtePtyFlags
 *
 * Translates @flags into the corresponding op code for the
 * GNOME PTY helper.
 *
 * Returns: the #GnomePtyOps corresponding to @flags
 */
static int
_vte_pty_helper_ops_from_flags (VtePtyFlags flags)
{
	int op = 0;
	static const int opmap[8] = {
		GNOME_PTY_OPEN_NO_DB_UPDATE,		/* 0 0 0 */
		GNOME_PTY_OPEN_PTY_LASTLOG,		/* 0 0 1 */
		GNOME_PTY_OPEN_PTY_UTMP,		/* 0 1 0 */
		GNOME_PTY_OPEN_PTY_LASTLOGUTMP,		/* 0 1 1 */
		GNOME_PTY_OPEN_PTY_WTMP,		/* 1 0 0 */
		GNOME_PTY_OPEN_PTY_LASTLOGWTMP,		/* 1 0 1 */
		GNOME_PTY_OPEN_PTY_UWTMP,		/* 1 1 0 */
		GNOME_PTY_OPEN_PTY_LASTLOGUWTMP,	/* 1 1 1 */
	};
	if ((flags & VTE_PTY_NO_LASTLOG) == 0) {
		op += 1;
	}
	if ((flags & VTE_PTY_NO_UTMP) == 0) {
		op += 2;
	}
	if ((flags & VTE_PTY_NO_WTMP) == 0) {
		op += 4;
	}
	g_assert(op >= 0 && op < (int) G_N_ELEMENTS(opmap));

        return opmap[op];
}

/*
 * _vte_pty_open_with_helper:
 * @pty: a #VtePty
 * @error: a location to store a #GError, or %NULL
 *
 * Opens a new file descriptor to a new PTY master using the
 * GNOME PTY helper.
 *
 * Returns: %TRUE on success, %FALSE on failure with @error filled in
 */
static gboolean
_vte_pty_open_with_helper(VtePty *pty,
                          GError **error)
{
        VtePtyPrivate *priv = pty->priv;
	GnomePtyOps ops;
	int ret;
	int parentfd = -1, childfd = -1;
	gpointer tag;

	/* We have to use the pty helper here. */
	if (!_vte_pty_start_helper(error))
                return FALSE;

	/* Try to open a new descriptor. */

        ops = _vte_pty_helper_ops_from_flags(priv->flags);
        /* Send our request. */
        if (n_write(_vte_pty_helper_tunnel,
                    &ops, sizeof(ops)) != sizeof(ops)) {
                g_set_error (error, VTE_PTY_ERROR,
                              VTE_PTY_ERROR_PTY_HELPER_FAILED,
                              "Failed to send request to gnome-pty-helper: %s",
                              g_strerror(errno));
                return FALSE;
        }
        _vte_debug_print(VTE_DEBUG_PTY, "Sent request to helper.\n");
        /* Read back the response. */
        if (n_read(_vte_pty_helper_tunnel,
                    &ret, sizeof(ret)) != sizeof(ret)) {
                g_set_error (error, VTE_PTY_ERROR,
                              VTE_PTY_ERROR_PTY_HELPER_FAILED,
                              "Failed to read response from gnome-pty-helper: %s",
                              g_strerror(errno));
                return FALSE;
        }
        _vte_debug_print(VTE_DEBUG_PTY,
                        "Received response from helper.\n");
        if (ret == 0) {
                g_set_error_literal (error, VTE_PTY_ERROR,
                                      VTE_PTY_ERROR_PTY_HELPER_FAILED,
                                      "gnome-pty-helper failed to open pty");
                return FALSE;
        }
        _vte_debug_print(VTE_DEBUG_PTY, "Helper returns success.\n");
        /* Read back a tag. */
        if (n_read(_vte_pty_helper_tunnel,
                    &tag, sizeof(tag)) != sizeof(tag)) {
                g_set_error (error, VTE_PTY_ERROR,
                              VTE_PTY_ERROR_PTY_HELPER_FAILED,
                              "Failed to read tag from gnome-pty-helper: %s",
                              g_strerror(errno));
                return FALSE;
        }
        _vte_debug_print(VTE_DEBUG_PTY, "Tag = %p.\n", tag);
        /* Receive the master and slave ptys. */
        _vte_pty_read_ptypair(_vte_pty_helper_tunnel,
                              &parentfd, &childfd);

        if ((parentfd == -1) || (childfd == -1)) {
                int errsv = errno;

                close(parentfd);
                close(childfd);

                g_set_error (error, VTE_PTY_ERROR,
                              VTE_PTY_ERROR_PTY_HELPER_FAILED,
                              "Failed to read master or slave pty from gnome-pty-helper: %s",
                              g_strerror(errsv));
                errno = errsv;
                return FALSE;
        }

        _vte_debug_print(VTE_DEBUG_PTY,
                        "Got master pty %d and slave pty %d.\n",
                        parentfd, childfd);

        priv->using_helper = TRUE;
        priv->helper_tag = tag;
        priv->pty_fd = parentfd;

        priv->child_setup_data.mode = TTY_OPEN_BY_FD;
        priv->child_setup_data.tty.fd = childfd;

        return TRUE;
}

#endif /* VTE_USE_GNOME_PTY_HELPER */

/**
 * vte_pty_set_utf8:
 * @pty: a #VtePty
 * @utf8: whether or not the pty is in UTF-8 mode
 * @error: (allow-none): return location to store a #GError, or %NULL
 *
 * Tells the kernel whether the terminal is UTF-8 or not, in case it can make
 * use of the info.  Linux 2.6.5 or so defines IUTF8 to make the line
 * discipline do multibyte backspace correctly.
 *
 * Returns: %TRUE on success, %FALSE on failure with @error filled in
 *
 * Since: 0.26
 */
gboolean
vte_pty_set_utf8(VtePty *pty,
                 gboolean utf8,
                 GError **error)
{
#if defined(HAVE_TCSETATTR) && defined(IUTF8)
        VtePtyPrivate *priv;
	struct termios tio;
	tcflag_t saved_cflag;

        g_return_val_if_fail(VTE_IS_PTY(pty), FALSE);

        priv = pty->priv;
        g_return_val_if_fail (priv->pty_fd > 0, FALSE);

        if (tcgetattr(priv->pty_fd, &tio) == -1) {
                int errsv = errno;
                g_set_error(error, G_IO_ERROR, g_io_error_from_errno(errsv),
                            "%s failed: %s", "tcgetattr", g_strerror(errsv));
                errno = errsv;
                return FALSE;
        }

        saved_cflag = tio.c_iflag;
        if (utf8) {
                tio.c_iflag |= IUTF8;
        } else {
              tio.c_iflag &= ~IUTF8;
        }

        /* Only set the flag if it changes */
        if (saved_cflag != tio.c_iflag &&
            tcsetattr(priv->pty_fd, TCSANOW, &tio) == -1) {
                int errsv = errno;
                g_set_error(error, G_IO_ERROR, g_io_error_from_errno(errsv),
                            "%s failed: %s", "tcgetattr", g_strerror(errsv));
                errno = errsv;
                return FALSE;
	}
#endif

        return TRUE;
}

/**
 * vte_pty_close:
 * @pty: a #VtePty
 *
 * Cleans up the PTY, specifically any logging performed for the session.
 * The file descriptor to the PTY master remains open.
 *
 * Since: 0.26
 */
void
vte_pty_close (VtePty *pty)
{
#ifdef VTE_USE_GNOME_PTY_HELPER
        VtePtyPrivate *priv = pty->priv;
	gpointer tag;
	GnomePtyOps ops;

        if (!priv->using_helper)
                return;

        /* Signal the helper that it needs to close its connection. */
        tag = priv->helper_tag;

        ops = GNOME_PTY_CLOSE_PTY;
        if (n_write(_vte_pty_helper_tunnel,
                    &ops, sizeof(ops)) != sizeof(ops)) {
                return;
        }
        if (n_write(_vte_pty_helper_tunnel,
                    &tag, sizeof(tag)) != sizeof(tag)) {
                return;
        }

        ops = GNOME_PTY_SYNCH;
        if (n_write(_vte_pty_helper_tunnel,
                    &ops, sizeof(ops)) != sizeof(ops)) {
                return;
        }
        n_read(_vte_pty_helper_tunnel, &ops, 1);

        priv->helper_tag = NULL;
        priv->using_helper = FALSE;
#endif
}

/* VTE PTY class */

enum {
        PROP_PTYCLASS_0,
        PROP_FLAGS,
        PROP_FD,
        PROP_TERM
};

/* GInitable impl */

static gboolean
vte_pty_initable_init (GInitable *initable,
                       GCancellable *cancellable,
                       GError **error)
{
        VtePty *pty = VTE_PTY (initable);
        VtePtyPrivate *priv = pty->priv;
        gboolean ret = FALSE;

        if (cancellable != NULL) {
                g_set_error_literal (error, G_IO_ERROR, G_IO_ERROR_NOT_SUPPORTED,
                                    "Cancellable initialisation not supported");
                return FALSE;
        }

        /* If we already have a (foreign) FD, we're done. */
        if (priv->foreign) {
                g_assert(priv->pty_fd != -1);
                return TRUE;
        }

#ifdef VTE_USE_GNOME_PTY_HELPER
	if ((priv->flags & VTE_PTY_NO_HELPER) == 0) {
                GError *err = NULL;

		ret = _vte_pty_open_with_helper(pty, &err);
                g_assert(ret || err != NULL);

                if (ret)
                        goto out;

                _vte_debug_print(VTE_DEBUG_PTY,
                                 "_vte_pty_open_with_helper failed: %s\n",
                                 err->message);

                /* Only do fallback if gnome-pty-helper failed! */
                if ((priv->flags & VTE_PTY_NO_FALLBACK) ||
                    !g_error_matches(err,
                                     VTE_PTY_ERROR,
                                     VTE_PTY_ERROR_PTY_HELPER_FAILED)) {
                        g_propagate_error (error, err);
                        goto out;
                }

                g_error_free(err);
                /* Fall back to unix98 PTY */
        }
#else
        if (priv->flags & VTE_PTY_NO_FALLBACK) {
                g_set_error_literal(error, VTE_PTY_ERROR, VTE_PTY_ERROR_PTY_HELPER_FAILED,
                                    "VTE compiled without GNOME PTY helper");
                goto out;
        }
#endif /* VTE_USE_GNOME_PTY_HELPER */

        ret = _vte_pty_open_unix98(pty, error);

  out:
	_vte_debug_print(VTE_DEBUG_PTY,
			"vte_pty_initable_init returning %s with ptyfd = %d\n",
			ret ? "TRUE" : "FALSE", priv->pty_fd);

	return ret;
}

static void
vte_pty_initable_iface_init (GInitableIface  *iface)
{
        iface->init = vte_pty_initable_init;
}

/* GObjectClass impl */

G_DEFINE_TYPE_WITH_CODE (VtePty, vte_pty, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (G_TYPE_INITABLE, vte_pty_initable_iface_init))

static void
vte_pty_init (VtePty *pty)
{
        VtePtyPrivate *priv;

        priv = pty->priv = G_TYPE_INSTANCE_GET_PRIVATE (pty, VTE_TYPE_PTY, VtePtyPrivate);

        priv->flags = VTE_PTY_DEFAULT;
        priv->pty_fd = -1;
        priv->foreign = FALSE;
        priv->using_helper = FALSE;
        priv->helper_tag = NULL;
        priv->term = vte_terminal_get_default_emulation(NULL /* that's ok, this function is just retarded */); /* already interned */
}

static void
vte_pty_finalize (GObject *object)
{
        VtePty *pty = VTE_PTY (object);
        VtePtyPrivate *priv = pty->priv;

        if (priv->child_setup_data.mode == TTY_OPEN_BY_FD &&
            priv->child_setup_data.tty.fd != -1) {
                /* Close the child FD */
                close(priv->child_setup_data.tty.fd);
        }

        vte_pty_close(pty);

        /* Close the master FD */
        if (priv->pty_fd != -1) {
                close(priv->pty_fd);
        }

        G_OBJECT_CLASS (vte_pty_parent_class)->finalize (object);
}

static void
vte_pty_get_property (GObject    *object,
                       guint       property_id,
                       GValue     *value,
                       GParamSpec *pspec)
{
        VtePty *pty = VTE_PTY (object);
        VtePtyPrivate *priv = pty->priv;

        switch (property_id) {
        case PROP_FLAGS:
                g_value_set_flags(value, priv->flags);
                break;

        case PROP_FD:
                g_value_set_int(value, vte_pty_get_fd(pty));
                break;

        case PROP_TERM:
                g_value_set_string(value, priv->term);
                break;

        default:
                G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
        }
}

static void
vte_pty_set_property (GObject      *object,
                       guint         property_id,
                       const GValue *value,
                       GParamSpec   *pspec)
{
        VtePty *pty = VTE_PTY (object);
        VtePtyPrivate *priv = pty->priv;

        switch (property_id) {
        case PROP_FLAGS:
                priv->flags = g_value_get_flags(value);
                break;

        case PROP_FD:
                priv->pty_fd = g_value_get_int(value);
                priv->foreign = (priv->pty_fd != -1);
                break;

        case PROP_TERM:
                vte_pty_set_term(pty, g_value_get_string(value));
                break;

        default:
                G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        }
}

static void
vte_pty_class_init (VtePtyClass *klass)
{
        GObjectClass *object_class = G_OBJECT_CLASS (klass);

        g_type_class_add_private(object_class, sizeof(VtePtyPrivate));

        object_class->set_property = vte_pty_set_property;
        object_class->get_property = vte_pty_get_property;
        object_class->finalize     = vte_pty_finalize;

        /**
         * VtePty:flags:
         *
         * Controls how the session is recorded in lastlog, utmp, and wtmp,
         * and whether to use the GNOME PTY helper.
         *
         * Since: 0.26
         */
        g_object_class_install_property
                (object_class,
                 PROP_FLAGS,
                 g_param_spec_flags ("flags", NULL, NULL,
                                     VTE_TYPE_PTY_FLAGS,
                                     VTE_PTY_DEFAULT,
                                     G_PARAM_READWRITE |
                                     G_PARAM_CONSTRUCT_ONLY |
                                     G_PARAM_STATIC_STRINGS));

        /**
         * VtePty:fd:
         *
         * The file descriptor of the PTY master.
         *
         * Since: 0.26
         */
        g_object_class_install_property
                (object_class,
                 PROP_FD,
                 g_param_spec_int ("fd", NULL, NULL,
                                   -1, G_MAXINT, -1,
                                   G_PARAM_READWRITE |
                                   G_PARAM_CONSTRUCT_ONLY |
                                   G_PARAM_STATIC_STRINGS));

        /**
         * VtePty:term:
         *
         * The value to set for the TERM environment variable just after
         * forking.
         *
         * Since: 0.26
         */
        g_object_class_install_property
                (object_class,
                 PROP_TERM,
                 g_param_spec_string ("term", NULL, NULL,
                                      "xterm",
                                      G_PARAM_READWRITE |
                                      G_PARAM_STATIC_STRINGS));
}

/* public API */

/**
 * vte_pty_error_quark:
 *
 * Error domain for VTE PTY errors. Errors in this domain will be from the #VtePtyError
 * enumeration. See #GError for more information on error domains.
 *
 * Returns: the error domain for VTE PTY errors
 *
 * Since: 0.26
 */
GQuark
vte_pty_error_quark(void)
{
  static GQuark quark = 0;

  if (G_UNLIKELY (quark == 0))
    quark = g_quark_from_static_string("vte-pty-error");

  return quark;
}

/**
 * vte_pty_new:
 * @flags: flags from #VtePtyFlags
 * @error: (allow-none): return location for a #GError, or %NULL
 *
 * Allocates a new pseudo-terminal.
 *
 * You can later use fork() or the g_spawn_async() family of functions
 * to start a process on the PTY.
 *
 * If using fork(), you MUST call vte_pty_child_setup() in the child.
 *
 * If using g_spawn_async() and friends, you MUST either use
 * vte_pty_child_setup() directly as the child setup function, or call
 * vte_pty_child_setup() from your own child setup function supplied.
 * Also, you MUST pass the %G_SPAWN_DO_NOT_REAP_CHILD flag.
 *
 * If GNOME PTY Helper is available and
 * unless some of the %VTE_PTY_NO_LASTLOG, %VTE_PTY_NO_UTMP or
 * %VTE_PTY_NO_WTMP flags are passed in @flags, the
 * session is logged in the corresponding lastlog, utmp or wtmp
 * system files.  When passing %VTE_PTY_NO_HELPER in @flags, the
 * GNOME PTY Helper is bypassed entirely.
 *
 * When passing %VTE_PTY_NO_FALLBACK in @flags,
 * and opening a PTY using the PTY helper fails, there will
 * be no fallback to allocate a PTY using Unix98 PTY functions.
 *
 * Returns: (transfer full): a new #VtePty, or %NULL on error with @error filled in
 *
 * Since: 0.26
 */
VtePty *
vte_pty_new (VtePtyFlags flags,
             GError **error)
{
        return g_initable_new (VTE_TYPE_PTY,
                               NULL /* cancellable */,
                               error,
                               "flags", flags,
                               NULL);
}

/**
 * vte_pty_new_foreign:
 * @fd: (transfer full): a file descriptor to the PTY
 * @error: (allow-none): return location for a #GError, or %NULL
 *
 * Creates a new #VtePty for the PTY master @fd.
 *
 * No entry will be made in the lastlog, utmp or wtmp system files.
 *
 * Note that the newly created #VtePty will take ownership of @fd
 * and close it on finalize.
 *
 * Returns: (transfer full): a new #VtePty for @fd, or %NULL on error with @error filled in
 *
 * Since: 0.26
 */
VtePty *
vte_pty_new_foreign (int fd,
                     GError **error)
{
        g_return_val_if_fail(fd >= 0, NULL);

        return g_initable_new (VTE_TYPE_PTY,
                               NULL /* cancellable */,
                               error,
                               "fd", fd,
                               NULL);
}

/**
 * vte_pty_get_fd:
 * @pty: a #VtePty
 *
 * Returns: (transfer none): the file descriptor of the PTY master in @pty. The
 *   file descriptor belongs to @pty and must not be closed
 */
int
vte_pty_get_fd (VtePty *pty)
{
        VtePtyPrivate *priv;

        g_return_val_if_fail(VTE_IS_PTY(pty), -1);

        priv = pty->priv;
        g_return_val_if_fail(priv->pty_fd != -1, -1);

        return priv->pty_fd;
}

/**
 * vte_pty_set_term:
 * @pty: a #VtePty
 * @emulation: (allow-none): the name of a terminal description, or %NULL
 *
 * Sets what value of the TERM environment variable to set just after forking.
 *
 * Since: 0.26
 */
void
vte_pty_set_term (VtePty *pty,
                  const char *emulation)
{
        VtePtyPrivate *priv;

        g_return_if_fail(VTE_IS_PTY(pty));
        g_return_if_fail(emulation != NULL);

        priv = pty->priv;
        emulation = g_intern_string(emulation);
        if (emulation == priv->term)
                return;

        priv->term = emulation;
        g_object_notify(G_OBJECT(pty), "term");
}

/* Reimplementation of the ugly deprecated APIs _vte_pty_*() */

#ifndef VTE_DISABLE_DEPRECATED_SOURCE

static GHashTable *fd_to_pty_hash = NULL;

static VtePty *
get_vte_pty_for_fd (int fd)
{
        VtePty *pty;

        if (fd_to_pty_hash != NULL &&
            (pty = g_hash_table_lookup(fd_to_pty_hash, &fd)) != NULL)
                return pty;

        g_warning("No VtePty found for fd %d!\n", fd);
        return NULL;
}

/**
 * _vte_pty_open:
 * @child: location to store the new process's ID
 * @env_add: a list of environment variables to add to the child's environment
 * @command: name of the binary to run
 * @argv: arguments to pass to @command
 * @directory: directory to start the new command in, or %NULL
 * @columns: desired window columns
 * @rows: desired window rows
 * @lastlog: %TRUE if the lastlog should be updated
 * @utmp: %TRUE if the utmp or utmpx log should be updated
 * @wtmp: %TRUE if the wtmp or wtmpx log should be updated
 *
 * Starts a new copy of @command running under a psuedo-terminal, optionally in
 * the supplied @directory, with window size set to @rows x @columns
 * and variables in @env_add added to its environment.  If any combination of
 * @lastlog, @utmp, and @wtmp is set, then the session is logged in the
 * corresponding system files.
 *
 * Returns: an open file descriptor for the pty master, -1 on failure
 *
 * Deprecated: 0.26: Use #VtePty together with fork() or the g_spawn_async() family of functions instead
 */
int
_vte_pty_open(pid_t *child,
              char **env_add,
              const char *command,
              char **argv,
              const char *directory,
              int columns,
              int rows,
              gboolean lastlog,
              gboolean utmp,
              gboolean wtmp)
{
        VtePty *pty;
        GPid pid;
        gboolean ret;

        pty = vte_pty_new(__vte_pty_get_pty_flags (lastlog, utmp, wtmp), NULL);
        if (pty == NULL)
                return -1;

        if (command != NULL) {
                char **real_argv;
                GSpawnFlags spawn_flags;

                spawn_flags = G_SPAWN_CHILD_INHERITS_STDIN |
                              G_SPAWN_SEARCH_PATH;
                real_argv = __vte_pty_get_argv(command, argv, &spawn_flags);
                ret = __vte_pty_spawn(pty,
                                      directory,
                                      real_argv,
                                      env_add,
                                      spawn_flags,
                                      NULL, NULL,
                                      &pid,
                                      NULL);
                g_strfreev(real_argv);
        } else {
                ret = __vte_pty_fork(pty, &pid, NULL);
        }

        if (!ret) {
                g_object_unref(pty);
                return -1;
        }

        vte_pty_set_size(pty, rows, columns, NULL);

        /* Stash the pty in the hash so we can later retrieve it by FD */
        if (fd_to_pty_hash == NULL) {
                fd_to_pty_hash = g_hash_table_new_full(g_int_hash,
                                                       g_int_equal,
                                                       NULL,
                                                       (GDestroyNotify) g_object_unref);
        }

        g_hash_table_insert(fd_to_pty_hash, &pty->priv->pty_fd, pty /* adopt refcount */);

        if (child)
                *child = (pid_t) pid;

        return vte_pty_get_fd(pty);
}

/**
 * _vte_pty_get_size:
 * @master: the file descriptor of the PTY master
 * @columns: a place to store the number of columns
 * @rows: a place to store the number of rows
 *
 * Attempts to read the pseudo terminal's window size.
 *
 * Returns: 0 on success, -1 on failure.
 *
 * Deprecated: 0.26: Use #VtePty and vte_pty_get_size() instead
 */
int
_vte_pty_get_size(int master,
                  int *columns,
                  int *rows)
{
        VtePty *pty;

        if ((pty = get_vte_pty_for_fd(master)) == NULL)
                return -1;

        if (vte_pty_get_size(pty, rows, columns, NULL))
                return 0;

        return -1;
}

/**
 * _vte_pty_set_size:
 * @master: the file descriptor of the PTY master
 * @columns: the desired number of columns
 * @rows: the desired number of rows
 *
 * Attempts to resize the pseudo terminal's window size.  If successful, the
 * OS kernel will send #SIGWINCH to the child process group.
 *
 * Returns: 0 on success, -1 on failure.
 *
 * Deprecated: 0.26: Use #VtePty and vte_pty_set_size() instead
 */
int
_vte_pty_set_size(int master,
                  int columns,
                  int rows)
{
        VtePty *pty;

        if ((pty = get_vte_pty_for_fd(master)) == NULL)
                return -1;

        if (vte_pty_set_size(pty, rows, columns, NULL))
                return 0;

        return -1;
}

/**
 * _vte_pty_set_utf8:
 * @pty: The pty master descriptor.
 * @utf8: Whether or not the pty is in UTF-8 mode.
 *
 * Tells the kernel whether the terminal is UTF-8 or not, in case it can make
 * use of the info.  Linux 2.6.5 or so defines IUTF8 to make the line
 * discipline do multibyte backspace correctly.
 *
 * Deprecated: 0.26: Use #VtePty and vte_pty_set_utf8() instead
 */
void _vte_pty_set_utf8(int master,
                       gboolean utf8)
{
        VtePty *pty;

        if ((pty = get_vte_pty_for_fd(master)) == NULL)
                return;

        vte_pty_set_utf8(pty, utf8, NULL);
}

/**
 * _vte_pty_close:
 * @pty: the pty master descriptor.
 *
 * Cleans up the PTY associated with the descriptor, specifically any logging
 * performed for the session.  The descriptor itself remains open.
 *
 * Deprecated: 0.26: Use #VtePty and vte_pty_close() instead
 */
void _vte_pty_close(int master)
{
        VtePty *pty;

        if ((pty = get_vte_pty_for_fd(master)) == NULL)
                return;

        /* Prevent closing the FD */
        pty->priv->pty_fd = -1;

        g_hash_table_remove(fd_to_pty_hash, &master);

        if (g_hash_table_size(fd_to_pty_hash) == 0) {
                g_hash_table_destroy(fd_to_pty_hash);
                fd_to_pty_hash = NULL;
        }
}

#endif /* !VTE_DISABLE_DEPRECATED_SOURCE */

#ifdef GDK_WINDOWING_X11

static void
_vte_bg_display_sync(VteBg *bg)
{
        VteBgPrivate *pvt = bg->pvt;

	gdk_display_sync(pvt->native.display);
}

static gboolean
_vte_property_get_pixmaps(GdkWindow *window, GdkAtom atom,
			  GdkAtom *type, int *size,
			  XID **pixmaps)
{
	return gdk_property_get(window, atom, GDK_TARGET_PIXMAP,
				0, INT_MAX - 3,
				FALSE,
				type, NULL, size,
				(guchar**) pixmaps);
}

static cairo_surface_t *
vte_bg_root_surface(VteBg *bg)
{
        VteBgPrivate *pvt = bg->pvt;
	GdkAtom prop_type;
	int prop_size;
	Window root;
	XID *pixmaps;
	int x, y;
	unsigned int width, height, border_width, depth;
	cairo_surface_t *surface = NULL;
	Display *display;
	Screen *screen;

	pixmaps = NULL;
	gdk_error_trap_push();
	if (!_vte_property_get_pixmaps(pvt->native.window, pvt->native.atom,
                                       &prop_type, &prop_size,
                                       &pixmaps))
		goto out;

	if ((prop_type != GDK_TARGET_PIXMAP) ||
	    (prop_size < (int)sizeof(XID) ||
	     (pixmaps == NULL)))
		goto out_pixmaps;
		
	if (!XGetGeometry (GDK_DISPLAY_XDISPLAY (pvt->native.display),
			   pixmaps[0], &root,
			   &x, &y, &width, &height, &border_width, &depth))
		goto out_pixmaps;

	display = gdk_x11_display_get_xdisplay (pvt->native.display);
	screen = gdk_x11_screen_get_xscreen (pvt->screen);
	surface = cairo_xlib_surface_create (display,
					     pixmaps[0],
					     DefaultVisualOfScreen(screen),
					     width, height);

        _vte_debug_print(VTE_DEBUG_BG|VTE_DEBUG_EVENTS,
                         "VteBg new background image %dx%d\n", width, height);

 out_pixmaps:
	g_free(pixmaps);
 out:
	_vte_bg_display_sync(bg);
	gdk_error_trap_pop_ignored ();

	return surface;
}

static void
vte_bg_set_root_surface(VteBg *bg, cairo_surface_t *surface)
{
        VteBgPrivate *pvt = bg->pvt;

	if (pvt->root_surface != NULL) {
		cairo_surface_destroy (pvt->root_surface);
	}
	pvt->root_surface = surface;
	vte_bg_cache_prune_int (bg, TRUE);
	g_signal_emit_by_name(bg, "root-pixmap-changed");
}

static GdkFilterReturn
vte_bg_root_filter(GdkXEvent *native, GdkEvent *event, gpointer data)
{
	XEvent *xevent = (XEvent*) native;
	VteBg *bg;
        VteBgPrivate *pvt;
	cairo_surface_t *surface;

	switch (xevent->type) {
	case PropertyNotify:
		bg = VTE_BG(data);
                pvt = bg->pvt;
		if ((xevent->xproperty.window == pvt->native.native_window) &&
		    (xevent->xproperty.atom == pvt->native.native_atom)) {
			surface = vte_bg_root_surface(bg);
			vte_bg_set_root_surface(bg, surface);
		}
		break;
	default:
		break;
	}
	return GDK_FILTER_CONTINUE;
}

#endif /* GDK_WINDOWING_X11 */

static void
vte_bg_finalize (GObject *obj)
{
	VteBg *bg = VTE_BG (obj);
        VteBgPrivate *pvt = bg->pvt;

        g_list_foreach (pvt->cache, (GFunc)vte_bg_cache_item_free, NULL);
        g_list_free (pvt->cache);

	G_OBJECT_CLASS(vte_bg_parent_class)->finalize (obj);
}

static void
vte_bg_class_init(VteBgClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

	gobject_class->finalize = vte_bg_finalize;

	g_signal_new("root-pixmap-changed",
                     G_OBJECT_CLASS_TYPE(klass),
                     G_SIGNAL_RUN_LAST,
                     0,
                     NULL,
                     NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);
	g_type_class_add_private(klass, sizeof (VteBgPrivate));
}

static void
vte_bg_init(VteBg *bg)
{
	bg->pvt = G_TYPE_INSTANCE_GET_PRIVATE (bg, VTE_TYPE_BG, VteBgPrivate);
}

/**
 * vte_bg_get:
 * @screen: a #GdkScreen
 *
 * Returns the global #VteBg object for @screen, creating it if necessary.
 *
 * Returns: (transfer none): a #VteBg
 */
VteBg *
vte_bg_get_for_screen(GdkScreen *screen)
{
	VteBg       *bg;

	bg = g_object_get_data(G_OBJECT(screen), "vte-bg");
	if (G_UNLIKELY(bg == NULL)) {
                VteBgPrivate *pvt;

		bg = g_object_new(VTE_TYPE_BG, NULL);
		g_object_set_data_full(G_OBJECT(screen),
				"vte-bg", bg, (GDestroyNotify)g_object_unref);

		/* connect bg to screen */
                pvt = bg->pvt;
		pvt->screen = screen;
#ifdef GDK_WINDOWING_X11
            {
                GdkEventMask events;
                GdkWindow   *window;

		window = gdk_screen_get_root_window(screen);
                pvt->native.window = window;
#if GTK_CHECK_VERSION (2, 91, 6)
                pvt->native.native_window = GDK_WINDOW_XID (window);
                pvt->native.display = gdk_window_get_display(window);
#else
                pvt->native.native_window = gdk_x11_drawable_get_xid(window);
                pvt->native.display = gdk_drawable_get_display(GDK_DRAWABLE(window));
#endif
                pvt->native.native_atom = gdk_x11_get_xatom_by_name_for_display(pvt->native.display, "_XROOTPMAP_ID");
                pvt->native.atom = gdk_x11_xatom_to_atom_for_display(pvt->native.display, pvt->native.native_atom);
		pvt->root_surface = vte_bg_root_surface(bg);
		events = gdk_window_get_events(window);
		events |= GDK_PROPERTY_CHANGE_MASK;
		gdk_window_set_events(window, events);
		gdk_window_add_filter(window, vte_bg_root_filter, bg);
            }
#endif /* GDK_WINDOWING_X11 */
	}

	return bg;
}

static gboolean
vte_bg_colors_equal(const PangoColor *a, const PangoColor *b)
{
	return  (a->red >> 8) == (b->red >> 8) &&
		(a->green >> 8) == (b->green >> 8) &&
		(a->blue >> 8) == (b->blue >> 8);
}

static void
vte_bg_cache_item_free(VteBgCacheItem *item)
{
        _vte_debug_print(VTE_DEBUG_BG,
                         "VteBgCacheItem %p freed\n", item);

	/* Clean up whatever is left in the structure. */
	if (item->source_pixbuf != NULL) {
		g_object_remove_weak_pointer(G_OBJECT(item->source_pixbuf),
				(gpointer*)(void*)&item->source_pixbuf);
	}
	g_free(item->source_file);

	if (item->surface != NULL)
		cairo_surface_set_user_data (item->surface,
					     &item_surface_key, NULL, NULL);

	g_slice_free(VteBgCacheItem, item);
}

static void
vte_bg_cache_prune_int(VteBg *bg, gboolean root)
{
	GList *i, *next;
	for (i = bg->pvt->cache; i != NULL; i = next) {
		VteBgCacheItem *item = i->data;
		next = g_list_next (i);
		/* Prune the item if either it is a "root pixmap" item and
		 * we want to prune them, or its surface is NULL because
		 * whichever object it created has been destroyed. */
		if ((root && (item->source_type == VTE_BG_SOURCE_ROOT)) ||
		    item->surface == NULL) {
			vte_bg_cache_item_free (item);
			bg->pvt->cache = g_list_delete_link(bg->pvt->cache, i);
		}
	}
}

static void
vte_bg_cache_prune(VteBg *bg)
{
	vte_bg_cache_prune_int(bg, FALSE);
}

static void item_surface_destroy_func(void *data)
{
	VteBgCacheItem *item = data;

        _vte_debug_print(VTE_DEBUG_BG,
                         "VteBgCacheItem %p surface destroyed\n", item);

	item->surface = NULL;
}

/*
 * vte_bg_cache_add:
 * @bg: a #VteBg
 * @item: a #VteBgCacheItem
 *
 * Adds @item to @bg's cache, instructing all of the objects therein to
 * clear the field which holds a pointer to the object upon its destruction.
 */
static void
vte_bg_cache_add(VteBg *bg, VteBgCacheItem *item)
{
	vte_bg_cache_prune(bg);
	bg->pvt->cache = g_list_prepend(bg->pvt->cache, item);
	if (item->source_pixbuf != NULL) {
		g_object_add_weak_pointer(G_OBJECT(item->source_pixbuf),
					  (gpointer*)(void*)&item->source_pixbuf);
	}

        if (item->surface != NULL)
                cairo_surface_set_user_data (item->surface, &item_surface_key, item,
                                            item_surface_destroy_func);
}

/*
 * vte_bg_cache_search:
 * @bg: a #VteBg
 * @source_type: a #VteBgSourceType
 * @source_pixbuf: a #GdkPixbuf, or %NULL
 * @source_file: path of an image file, or %NULL
 * @tint: a #PangoColor to use as tint color
 * @saturation: the saturation as a value between 0.0 and 1.0
 *
 * Returns: a reference to a #cairo_surface_t, or %NULL on if
 *   there is no matching item in the cache
 */
static cairo_surface_t *
vte_bg_cache_search(VteBg *bg,
		    VteBgSourceType source_type,
		    const GdkPixbuf *source_pixbuf,
		    const char *source_file,
		    const PangoColor *tint,
		    double saturation)
{
	GList *i;

	vte_bg_cache_prune(bg);
	for (i = bg->pvt->cache; i != NULL; i = g_list_next(i)) {
		VteBgCacheItem *item = i->data;
		if (vte_bg_colors_equal(&item->tint_color, tint) &&
		    (saturation == item->saturation) &&
		    (source_type == item->source_type)) {
			switch (source_type) {
			case VTE_BG_SOURCE_ROOT:
				break;
			case VTE_BG_SOURCE_PIXBUF:
				if (item->source_pixbuf != source_pixbuf) {
					continue;
				}
				break;
			case VTE_BG_SOURCE_FILE:
				if (strcmp(item->source_file, source_file)) {
					continue;
				}
				break;
			default:
				g_assert_not_reached();
				break;
			}

			return cairo_surface_reference(item->surface);
		}
	}
	return NULL;
}

/*< private >
 * vte_bg_get_surface:
 * @bg: a #VteBg
 * @source_type: a #VteBgSourceType
 * @source_pixbuf: (allow-none): a #GdkPixbuf, or %NULL
 * @source_file: (allow-none): path of an image file, or %NULL
 * @tint: a #PangoColor to use as tint color
 * @saturation: the saturation as a value between 0.0 and 1.0
 * @other: a #cairo_surface_t
 *
 * Returns: a reference to a #cairo_surface_t, or %NULL on failure
 */
cairo_surface_t *
vte_bg_get_surface(VteBg *bg,
		   VteBgSourceType source_type,
		   GdkPixbuf *source_pixbuf,
		   const char *source_file,
		   const PangoColor *tint,
		   double saturation,
		   cairo_surface_t *other)
{
        VteBgPrivate *pvt;
	VteBgCacheItem *item;
	GdkPixbuf *pixbuf;
	cairo_surface_t *cached;
	cairo_t *cr;
	int width, height;

        g_return_val_if_fail(VTE_IS_BG(bg), NULL);
        pvt = bg->pvt;

	if (source_type == VTE_BG_SOURCE_NONE) {
		return NULL;
	}
#ifndef GDK_WINDOWING_X11
        if (source_type == VTE_BG_SOURCE_ROOT) {
                return NULL;
        }
#endif

	cached = vte_bg_cache_search(bg, source_type,
				     source_pixbuf, source_file,
				     tint, saturation);
	if (cached != NULL) {
		return cached;
	}

        /* FIXME: The above only returned a hit when the source *and*
         * tint and saturation matched. This means that for VTE_BG_SOURCE_FILE,
         * we will create below *another* #GdkPixbuf for the same source file,
         * wasting memory. We should instead look up the source pixbuf regardless
         * of tint and saturation, and just create a new #VteBgCacheItem
         * with a new surface for it.
         */

	item = g_slice_new(VteBgCacheItem);
	item->source_type = source_type;
	item->source_pixbuf = NULL;
	item->source_file = NULL;
	item->tint_color = *tint;
	item->saturation = saturation;
        item->surface = NULL;
	pixbuf = NULL;

	switch (source_type) {
	case VTE_BG_SOURCE_ROOT:
		break;
	case VTE_BG_SOURCE_PIXBUF:
		item->source_pixbuf = g_object_ref (source_pixbuf);
		pixbuf = g_object_ref (source_pixbuf);
		break;
	case VTE_BG_SOURCE_FILE:
		if (source_file != NULL && source_file[0] != '\0') {
			item->source_file = g_strdup(source_file);
			pixbuf = gdk_pixbuf_new_from_file(source_file, NULL);
		}
		break;
	default:
		g_assert_not_reached();
		break;
	}

	if (pixbuf) {
		width = gdk_pixbuf_get_width(pixbuf);
		height = gdk_pixbuf_get_height(pixbuf);
	}
#ifdef GDK_WINDOWING_X11
        else if (source_type == VTE_BG_SOURCE_ROOT &&
                 pvt->root_surface != NULL) {
		width = cairo_xlib_surface_get_width(pvt->root_surface);
		height = cairo_xlib_surface_get_height(pvt->root_surface);
	}
#endif
        else
                goto out;

	item->surface =
		cairo_surface_create_similar(other, CAIRO_CONTENT_COLOR_ALPHA,
					     width, height);

	cr = cairo_create (item->surface);
	cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
	if (pixbuf)
		gdk_cairo_set_source_pixbuf (cr, pixbuf, 0, 0);
#ifdef GDK_WINDOWING_X11
	else if (source_type == VTE_BG_SOURCE_ROOT)
		cairo_set_source_surface (cr, pvt->root_surface, 0, 0);
#endif
	cairo_paint (cr);

	if (saturation < 1.0) {
		cairo_set_source_rgba (cr, 
				       tint->red / 65535.,
				       tint->green / 65535.,
				       tint->blue / 65535.,
				       1 - saturation);
		cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
		cairo_paint (cr);
	}
	cairo_destroy (cr);

    out:
	vte_bg_cache_add(bg, item);

	if (pixbuf)
		g_object_unref (pixbuf);

	return item->surface;
}

static struct unistr_info *
unistr_info_create (void)
{
	return g_slice_new0 (struct unistr_info);
}

static void
unistr_info_finish (struct unistr_info *uinfo)
{
	union unistr_font_info *ufi = &uinfo->ufi;

	switch (uinfo->coverage) {
	default:
	case COVERAGE_UNKNOWN:
		break;
	case COVERAGE_USE_PANGO_LAYOUT_LINE:
		/* we hold a manual reference on layout */
		g_object_unref (ufi->using_pango_layout_line.line->layout);
		ufi->using_pango_layout_line.line->layout = NULL;
		pango_layout_line_unref (ufi->using_pango_layout_line.line);
		ufi->using_pango_layout_line.line = NULL;
		break;
	case COVERAGE_USE_PANGO_GLYPH_STRING:
		if (ufi->using_pango_glyph_string.font)
			g_object_unref (ufi->using_pango_glyph_string.font);
		ufi->using_pango_glyph_string.font = NULL;
		pango_glyph_string_free (ufi->using_pango_glyph_string.glyph_string);
		ufi->using_pango_glyph_string.glyph_string = NULL;
		break;
	case COVERAGE_USE_CAIRO_GLYPH:
		cairo_scaled_font_destroy (ufi->using_cairo_glyph.scaled_font);
		ufi->using_cairo_glyph.scaled_font = NULL;
		break;
	}
}

static void
unistr_info_destroy (struct unistr_info *uinfo)
{
	unistr_info_finish (uinfo);
	g_slice_free (struct unistr_info, uinfo);
}

struct font_info {
	/* lifecycle */
	int ref_count;
	guint destroy_timeout; /* only used when ref_count == 0 */

	/* reusable layout set with font and everything set */
	PangoLayout *layout;

	/* cache of character info */
	struct unistr_info ascii_unistr_info[128];
	GHashTable *other_unistr_info;

	/* cell metrics */
	gint width, height, ascent;

	/* reusable string for UTF-8 conversion */
	GString *string;

#ifdef VTE_DEBUG
	/* profiling info */
	int coverage_count[4];
#endif
};


static struct unistr_info *
font_info_find_unistr_info (struct font_info    *info,
			    vteunistr            c)
{
	struct unistr_info *uinfo;

	if (G_LIKELY (c < G_N_ELEMENTS (info->ascii_unistr_info)))
		return &info->ascii_unistr_info[c];

	if (G_UNLIKELY (info->other_unistr_info == NULL))
		info->other_unistr_info = g_hash_table_new_full (NULL, NULL, NULL, (GDestroyNotify) unistr_info_destroy);

	uinfo = g_hash_table_lookup (info->other_unistr_info, GINT_TO_POINTER (c));
	if (G_LIKELY (uinfo))
		return uinfo;

	uinfo = unistr_info_create ();
	g_hash_table_insert (info->other_unistr_info, GINT_TO_POINTER (c), uinfo);
	return uinfo;
}


static void
font_info_cache_ascii (struct font_info *info)
{
	PangoLayoutLine *line;
	PangoGlyphItemIter iter;
	PangoGlyphItem *glyph_item;
	PangoGlyphString *glyph_string;
	PangoFont *pango_font;
	cairo_scaled_font_t *scaled_font;
	const char *text;
	gboolean more;
	PangoLanguage *language;
	gboolean latin_uses_default_language;
	
	/* We have info->layout holding most ASCII characters.  We want to
	 * cache as much info as we can about the ASCII letters so we don't
	 * have to look them up again later */

	/* Don't cache if unknown glyphs found in layout */
	if (pango_layout_get_unknown_glyphs_count (info->layout) != 0)
		return;

	language = pango_context_get_language (pango_layout_get_context (info->layout));
	if (language == NULL)
		language = pango_language_get_default ();
	latin_uses_default_language = pango_language_includes_script (language, PANGO_SCRIPT_LATIN);

	text = pango_layout_get_text (info->layout);

	line = pango_layout_get_line_readonly (info->layout, 0);

	/* Don't cache if more than one font used for the line */
	if (G_UNLIKELY (!line || !line->runs || line->runs->next))
		return;

	glyph_item = line->runs->data;
	glyph_string = glyph_item->glyphs;
	pango_font = glyph_item->item->analysis.font;
	if (!pango_font)
		return;
	scaled_font = pango_cairo_font_get_scaled_font ((PangoCairoFont *) pango_font);
	if (!scaled_font)
		return;

	for (more = pango_glyph_item_iter_init_start (&iter, glyph_item, text);
	     more;
	     more = pango_glyph_item_iter_next_cluster (&iter))
	{
		struct unistr_info *uinfo;
		union unistr_font_info *ufi;
	 	PangoGlyphGeometry *geometry;
		PangoGlyph glyph;
		vteunistr c;

		/* Only cache simple clusters */
		if (iter.start_char +1 != iter.end_char  ||
		    iter.start_index+1 != iter.end_index ||
		    iter.start_glyph+1 != iter.end_glyph)
			continue;

		c = text[iter.start_index];
		glyph = glyph_string->glyphs[iter.start_glyph].glyph;
		geometry = &glyph_string->glyphs[iter.start_glyph].geometry;

		/* If not using the default locale language, only cache non-common
		 * characters as common characters get their font from their neighbors
		 * and we don't want to force Latin on them. */
		if (!latin_uses_default_language &&
		    pango_script_for_unichar (c) <= PANGO_SCRIPT_INHERITED)
			continue;

		/* Only cache simple glyphs */
		if (!(glyph <= 0xFFFF) || (geometry->x_offset | geometry->y_offset) != 0)
			continue;

		uinfo = font_info_find_unistr_info (info, c);
		if (G_UNLIKELY (uinfo->coverage != COVERAGE_UNKNOWN))
			continue;

		ufi = &uinfo->ufi;

		uinfo->width = PANGO_PIXELS_CEIL (geometry->width);
		uinfo->has_unknown_chars = FALSE;

		uinfo->coverage = COVERAGE_USE_CAIRO_GLYPH;

		ufi->using_cairo_glyph.scaled_font = cairo_scaled_font_reference (scaled_font);
		ufi->using_cairo_glyph.glyph_index = glyph;

#ifdef VTE_DEBUG
		info->coverage_count[0]++;
		info->coverage_count[uinfo->coverage]++;
#endif
	}

#ifdef VTE_DEBUG
	_vte_debug_print (VTE_DEBUG_PANGOCAIRO,
			  "vtepangocairo: %p cached %d ASCII letters\n",
			  info, info->coverage_count[0]);
#endif
}

static void
font_info_measure_font (struct font_info *info)
{
	PangoRectangle logical;

	/* Estimate for ASCII characters. */
	pango_layout_set_text (info->layout, VTE_DRAW_SINGLE_WIDE_CHARACTERS, -1);
	pango_layout_get_extents (info->layout, NULL, &logical);
	/* We don't do CEIL for width since we are averaging;
	 * rounding is more accurate */
	info->width  = PANGO_PIXELS (howmany (logical.width, strlen(VTE_DRAW_SINGLE_WIDE_CHARACTERS)));
	info->height = PANGO_PIXELS_CEIL (logical.height);
	info->ascent = PANGO_PIXELS_CEIL (pango_layout_get_baseline (info->layout));

	/* Now that we shaped the entire ASCII character string, cache glyph
	 * info for them */
	font_info_cache_ascii (info);


	if (info->height == 0) {
		info->height = PANGO_PIXELS_CEIL (logical.height);
	}
	if (info->ascent == 0) {
		info->ascent = PANGO_PIXELS_CEIL (pango_layout_get_baseline (info->layout));
	}

	_vte_debug_print (VTE_DEBUG_MISC,
			  "vtepangocairo: %p font metrics = %dx%d (%d)\n",
			  info, info->width, info->height, info->ascent);
}


static struct font_info *
font_info_allocate (PangoContext *context)
{
	struct font_info *info;
	PangoTabArray *tabs;

	info = g_slice_new0 (struct font_info);

	_vte_debug_print (VTE_DEBUG_PANGOCAIRO,
			  "vtepangocairo: %p allocating font_info\n",
			  info);

	info->layout = pango_layout_new (context);
	tabs = pango_tab_array_new_with_positions (1, FALSE, PANGO_TAB_LEFT, 1);
	pango_layout_set_tabs (info->layout, tabs);
	pango_tab_array_free (tabs);

	info->string = g_string_sized_new (VTE_UTF8_BPC+1);

	font_info_measure_font (info);

	return info;
}

static void
font_info_free (struct font_info *info)
{
	vteunistr i;

#ifdef VTE_DEBUG
	_vte_debug_print (VTE_DEBUG_PANGOCAIRO,
			  "vtepangocairo: %p freeing font_info.  coverages %d = %d + %d + %d\n",
			  info,
			  info->coverage_count[0],
			  info->coverage_count[1],
			  info->coverage_count[2],
			  info->coverage_count[3]);
#endif

	g_string_free (info->string, TRUE);
	g_object_unref (info->layout);

	for (i = 0; i < G_N_ELEMENTS (info->ascii_unistr_info); i++)
		unistr_info_finish (&info->ascii_unistr_info[i]);
		
	if (info->other_unistr_info) {
		g_hash_table_destroy (info->other_unistr_info);
	}

	g_slice_free (struct font_info, info);
}


static GHashTable *font_info_for_context;

static struct font_info *
font_info_register (struct font_info *info)
{
	g_hash_table_insert (font_info_for_context,
			     pango_layout_get_context (info->layout),
			     info);

	return info;
}

static void
font_info_unregister (struct font_info *info)
{
	g_hash_table_remove (font_info_for_context,
			     pango_layout_get_context (info->layout));
}


static struct font_info *
font_info_reference (struct font_info *info)
{
	if (!info)
		return info;

	g_return_val_if_fail (info->ref_count >= 0, info);

	if (info->destroy_timeout) {
		g_source_remove (info->destroy_timeout);
		info->destroy_timeout = 0;
	}

	info->ref_count++;

	return info;
}

static gboolean
font_info_destroy_delayed (struct font_info *info)
{
	info->destroy_timeout = 0;

	font_info_unregister (info);
	font_info_free (info);

	return FALSE;
}

static void
font_info_destroy (struct font_info *info)
{
	if (!info)
		return;

	g_return_if_fail (info->ref_count > 0);

	info->ref_count--;
	if (info->ref_count)
		return;

	/* Delay destruction by a few seconds, in case we need it again */
	info->destroy_timeout = gdk_threads_add_timeout_seconds (FONT_CACHE_TIMEOUT,
								 (GSourceFunc) font_info_destroy_delayed,
								 info);
}

static GQuark
fontconfig_timestamp_quark (void)
{
	static GQuark quark;

	if (G_UNLIKELY (!quark))
		quark = g_quark_from_static_string ("vte-fontconfig-timestamp");

	return quark;
}

static void
vte_pango_context_set_fontconfig_timestamp (PangoContext *context,
					    guint         fontconfig_timestamp)
{
	g_object_set_qdata ((GObject *) context,
			    fontconfig_timestamp_quark (),
			    GUINT_TO_POINTER (fontconfig_timestamp));
}

static guint
vte_pango_context_get_fontconfig_timestamp (PangoContext *context)
{
	return GPOINTER_TO_UINT (g_object_get_qdata ((GObject *) context,
						     fontconfig_timestamp_quark ()));
}

static guint
context_hash (PangoContext *context)
{
	return pango_units_from_double (pango_cairo_context_get_resolution (context))
	     ^ pango_font_description_hash (pango_context_get_font_description (context))
	     ^ cairo_font_options_hash (pango_cairo_context_get_font_options (context))
	     ^ GPOINTER_TO_UINT (pango_context_get_language (context))
	     ^ vte_pango_context_get_fontconfig_timestamp (context);
}

static gboolean
context_equal (PangoContext *a,
	       PangoContext *b)
{
	return pango_cairo_context_get_resolution (a) == pango_cairo_context_get_resolution (b)
	    && pango_font_description_equal (pango_context_get_font_description (a), pango_context_get_font_description (b))
	    && cairo_font_options_equal (pango_cairo_context_get_font_options (a), pango_cairo_context_get_font_options (b))
	    && pango_context_get_language (a) == pango_context_get_language (b)
	    && vte_pango_context_get_fontconfig_timestamp (a) == vte_pango_context_get_fontconfig_timestamp (b);
}

static struct font_info *
font_info_find_for_context (PangoContext *context)
{
	struct font_info *info;

	if (G_UNLIKELY (font_info_for_context == NULL))
		font_info_for_context = g_hash_table_new ((GHashFunc) context_hash, (GEqualFunc) context_equal);

	info = g_hash_table_lookup (font_info_for_context, context);
	if (G_LIKELY (info)) {
		_vte_debug_print (VTE_DEBUG_PANGOCAIRO,
				  "vtepangocairo: %p found font_info in cache\n",
				  info);
		return font_info_reference (info);
	}

	info = font_info_allocate (context);
	info->ref_count = 1;
	font_info_register (info);

	g_object_unref (context);

	return info;
}

/* assumes ownership/reference of context */
static struct font_info *
font_info_create_for_context (PangoContext               *context,
			      const PangoFontDescription *desc,
			      VteTerminalAntiAlias        antialias,
			      PangoLanguage              *language,
			      guint                       fontconfig_timestamp)
{
	if (!PANGO_IS_CAIRO_FONT_MAP (pango_context_get_font_map (context))) {
		/* Ouch, Gtk+ switched over to some drawing system?
		 * Lets just create one from the default font map.
		 */
		g_object_unref (context);
		context = pango_font_map_create_context (pango_cairo_font_map_get_default ());
	}

	vte_pango_context_set_fontconfig_timestamp (context, fontconfig_timestamp);

	pango_context_set_base_dir (context, PANGO_DIRECTION_LTR);

	if (desc)
		pango_context_set_font_description (context, desc);

	pango_context_set_language (context, language);

	switch (antialias) {
		cairo_font_options_t *font_options;
		cairo_antialias_t cr_aa;

	case VTE_ANTI_ALIAS_FORCE_ENABLE:
	case VTE_ANTI_ALIAS_FORCE_DISABLE:

		if (antialias == VTE_ANTI_ALIAS_FORCE_ENABLE)
			cr_aa = CAIRO_ANTIALIAS_DEFAULT; /* let surface decide between gray and subpixel */
		else
			cr_aa = CAIRO_ANTIALIAS_NONE;

		font_options = cairo_font_options_copy (pango_cairo_context_get_font_options (context));
		cairo_font_options_set_antialias (font_options, cr_aa);
		pango_cairo_context_set_font_options (context, font_options);
		cairo_font_options_destroy (font_options);

		break;

	default:
	case VTE_ANTI_ALIAS_USE_DEFAULT:
		/* Make sure our contexts have a font_options set.  We use
		 * this invariant in our context hash and equal functions.
		 */
		if (!pango_cairo_context_get_font_options (context)) {
			font_options = cairo_font_options_create ();
			pango_cairo_context_set_font_options (context, font_options);
			cairo_font_options_destroy (font_options);
		}
		break;
	}

	return font_info_find_for_context (context);
}

static struct font_info *
font_info_create_for_screen (GdkScreen                  *screen,
			     const PangoFontDescription *desc,
			     VteTerminalAntiAlias        antialias,
			     PangoLanguage              *language)
{
	GtkSettings *settings = gtk_settings_get_for_screen (screen);
	int fontconfig_timestamp;
	g_object_get (settings, "gtk-fontconfig-timestamp", &fontconfig_timestamp, NULL);
	return font_info_create_for_context (gdk_pango_context_get_for_screen (screen),
					     desc, antialias, language, fontconfig_timestamp);
}

static struct font_info *
font_info_create_for_widget (GtkWidget                  *widget,
			     const PangoFontDescription *desc,
			     VteTerminalAntiAlias        antialias)
{
	GdkScreen *screen = gtk_widget_get_screen (widget);
	PangoLanguage *language = pango_context_get_language (gtk_widget_get_pango_context (widget));

	return font_info_create_for_screen (screen, desc, antialias, language);
}

static struct unistr_info *
font_info_get_unistr_info (struct font_info *info,
			   vteunistr c)
{
	struct unistr_info *uinfo;
	union unistr_font_info *ufi;
	PangoRectangle logical;
	PangoLayoutLine *line;

	uinfo = font_info_find_unistr_info (info, c);
	if (G_LIKELY (uinfo->coverage != COVERAGE_UNKNOWN))
		return uinfo;

	ufi = &uinfo->ufi;

	g_string_set_size (info->string, 0);
	_vte_unistr_append_to_string (c, info->string);
	pango_layout_set_text (info->layout, info->string->str, -1);
	pango_layout_get_extents (info->layout, NULL, &logical);

	uinfo->width = PANGO_PIXELS_CEIL (logical.width);

	line = pango_layout_get_line_readonly (info->layout, 0);

	uinfo->has_unknown_chars = pango_layout_get_unknown_glyphs_count (info->layout) != 0;
	/* we use PangoLayoutRun rendering unless there is exactly one run in the line. */
	if (G_UNLIKELY (!line || !line->runs || line->runs->next))
	{
		uinfo->coverage = COVERAGE_USE_PANGO_LAYOUT_LINE;

		ufi->using_pango_layout_line.line = pango_layout_line_ref (line);
		/* we hold a manual reference on layout.  pango currently
		 * doesn't work if line->layout is NULL.  ugh! */
		pango_layout_set_text (info->layout, "", -1); /* make layout disassociate from the line */
		ufi->using_pango_layout_line.line->layout = g_object_ref (info->layout);

	} else {
		PangoGlyphItem *glyph_item = line->runs->data;
		PangoFont *pango_font = glyph_item->item->analysis.font;
		PangoGlyphString *glyph_string = glyph_item->glyphs;

		/* we use fast cairo path if glyph string has only one real
		 * glyph and at origin */
		if (!uinfo->has_unknown_chars &&
		    glyph_string->num_glyphs == 1 && glyph_string->glyphs[0].glyph <= 0xFFFF &&
		    (glyph_string->glyphs[0].geometry.x_offset |
		     glyph_string->glyphs[0].geometry.y_offset) == 0)
		{
			cairo_scaled_font_t *scaled_font = pango_cairo_font_get_scaled_font ((PangoCairoFont *) pango_font);

			if (scaled_font) {
				uinfo->coverage = COVERAGE_USE_CAIRO_GLYPH;

				ufi->using_cairo_glyph.scaled_font = cairo_scaled_font_reference (scaled_font);
				ufi->using_cairo_glyph.glyph_index = glyph_string->glyphs[0].glyph;
			}
		}

		/* use pango fast path otherwise */
		if (G_UNLIKELY (uinfo->coverage == COVERAGE_UNKNOWN)) {
			uinfo->coverage = COVERAGE_USE_PANGO_GLYPH_STRING;

			ufi->using_pango_glyph_string.font = pango_font ? g_object_ref (pango_font) : NULL;
			ufi->using_pango_glyph_string.glyph_string = pango_glyph_string_copy (glyph_string);
		}
	}

	/* release internal layout resources */
	pango_layout_set_text (info->layout, "", -1);

#ifdef VTE_DEBUG
	info->coverage_count[0]++;
	info->coverage_count[uinfo->coverage]++;
#endif

	return uinfo;
}

struct _vte_draw *
_vte_draw_new (GtkWidget *widget)
{
	struct _vte_draw *draw;

	/* Create the structure. */
	draw = g_slice_new0 (struct _vte_draw);
	draw->widget = g_object_ref (widget);

	_vte_debug_print (VTE_DEBUG_DRAW, "draw_new\n");

	return draw;
}

void
_vte_draw_free (struct _vte_draw *draw)
{
	_vte_debug_print (VTE_DEBUG_DRAW, "draw_free\n");

	if (draw->bg_pattern != NULL) {
		cairo_pattern_destroy (draw->bg_pattern);
		draw->bg_pattern = NULL;
	}

	if (draw->font != NULL) {
		font_info_destroy (draw->font);
		draw->font = NULL;
	}

	if (draw->widget != NULL) {
		g_object_unref (draw->widget);
	}

	g_slice_free (struct _vte_draw, draw);
}

void
_vte_draw_start (struct _vte_draw *draw)
{
	GdkWindow *window;

	g_return_if_fail (gtk_widget_get_realized (draw->widget));

	_vte_debug_print (VTE_DEBUG_DRAW, "draw_start\n");

	if (draw->started == 0) {
		window = gtk_widget_get_window(draw->widget);
		g_object_ref (window);
		draw->cr = gdk_cairo_create (window);
	}

	draw->started++;
}

void
_vte_draw_end (struct _vte_draw *draw)
{
	g_return_if_fail (draw->started);

	draw->started--;
	if (draw->started == 0) {
		cairo_destroy (draw->cr);
		draw->cr = NULL;
		g_object_unref (gtk_widget_get_window(draw->widget));
 	}

	_vte_debug_print (VTE_DEBUG_DRAW, "draw_end\n");
}

void _vte_draw_set_background_solid(struct _vte_draw *draw, double red, double green, double blue, double opacity)
{
	if (draw->bg_pattern)
		cairo_pattern_destroy (draw->bg_pattern);

	draw->bg_pattern = cairo_pattern_create_rgba (red,
						      green,
						      blue,
						      opacity);
}

void _vte_draw_set_background_image (struct _vte_draw *draw, VteBgSourceType type, GdkPixbuf *pixbuf, const char *filename, const PangoColor *color, double saturation)
{
	cairo_surface_t *surface;

	/* Need a valid draw->cr for cairo_get_target () */
	_vte_draw_start (draw);

	surface = vte_bg_get_surface (vte_bg_get_for_screen (gtk_widget_get_screen (draw->widget)),
				     type, pixbuf, filename,
				     color, saturation,
				     cairo_get_target(draw->cr));

	_vte_draw_end (draw);

	if (!surface)
		return;

	if (draw->bg_pattern)
		cairo_pattern_destroy (draw->bg_pattern);

	draw->bg_pattern = cairo_pattern_create_for_surface (surface);
	cairo_surface_destroy (surface);
	cairo_pattern_set_extend (draw->bg_pattern, CAIRO_EXTEND_REPEAT);
}

void
_vte_draw_set_background_scroll (struct _vte_draw *draw,
				 gint x, gint y)
{
	cairo_matrix_t matrix;

	_vte_debug_print (VTE_DEBUG_DRAW,
			"draw_set_scroll (%d, %d)\n",
			x, y);

	g_return_if_fail (draw->bg_pattern != NULL);

	cairo_matrix_init_translate (&matrix, x, y);
	cairo_pattern_set_matrix (draw->bg_pattern, &matrix);
}

void
_vte_draw_clip (struct _vte_draw *draw, GdkRegion *region)
{
	_vte_debug_print (VTE_DEBUG_DRAW, "draw_clip\n");
	gdk_cairo_region(draw->cr, region);
	cairo_clip (draw->cr);
}

void
_vte_draw_clear (struct _vte_draw *draw, gint x, gint y, gint width, gint height)
{
	g_return_if_fail (draw->bg_pattern != NULL);

	_vte_debug_print (VTE_DEBUG_DRAW, "draw_clear (%d, %d, %d, %d)\n",
			  x,y,width, height);

	cairo_rectangle (draw->cr, x, y, width, height);
	cairo_set_operator (draw->cr, CAIRO_OPERATOR_SOURCE);
	cairo_set_source (draw->cr, draw->bg_pattern);
	cairo_fill (draw->cr);
}

void
_vte_draw_set_text_font (struct _vte_draw *draw,
			const PangoFontDescription *fontdesc,
			VteTerminalAntiAlias antialias)
{
	PangoFontDescription *bolddesc = NULL;

	_vte_debug_print (VTE_DEBUG_DRAW, "draw_set_text_font (aa=%d)\n",
			  antialias);

	if (draw->font_bold != draw->font)
		font_info_destroy (draw->font_bold);
	font_info_destroy (draw->font);
	draw->font = font_info_create_for_widget (draw->widget, fontdesc, antialias);

	/* calculate bold font desc */
	bolddesc = pango_font_description_copy (fontdesc);
	pango_font_description_set_weight (bolddesc, PANGO_WEIGHT_BOLD);

	draw->font_bold = font_info_create_for_widget (draw->widget, bolddesc, antialias);
	pango_font_description_free (bolddesc);

	/* Decide if we should keep this bold font face, per bug 54926:
	 *  - reject bold font if it is not within 10% of normal font width
	 */
	if ( abs((draw->font_bold->width * 100 / draw->font->width) - 100) > 10 ) {
		font_info_destroy (draw->font_bold);
		draw->font_bold = draw->font;
	}
}

void
_vte_draw_get_text_metrics(struct _vte_draw *draw,
			   gint *width, gint *height, gint *ascent)
{
	g_return_if_fail (draw->font != NULL);

	if (width)
		*width  = draw->font->width;
	if (height)
		*height = draw->font->height;
	if (ascent)
		*ascent = draw->font->ascent;
}


int
_vte_draw_get_char_width (struct _vte_draw *draw, vteunistr c, int columns,
			  gboolean bold)
{
	struct unistr_info *uinfo;

	g_return_val_if_fail (draw->font != NULL, 0);

	uinfo = font_info_get_unistr_info (bold ? draw->font_bold : draw->font, c);
	return uinfo->width;
}

static gboolean
_vte_draw_has_bold (struct _vte_draw *draw)
{
	return (draw->font != draw->font_bold);
}

static void
set_source_color_alpha (cairo_t        *cr,
			const PangoColor *color,
			guchar alpha)
{
	cairo_set_source_rgba (cr,
			      color->red / 65535.,
			      color->green / 65535.,
			      color->blue / 65535.,
			      alpha / 255.);
}

static void
_vte_draw_text_internal (struct _vte_draw *draw,
			 struct _vte_draw_text_request *requests, gsize n_requests,
			 const PangoColor *color, guchar alpha, gboolean bold)
{
	gsize i;
	cairo_scaled_font_t *last_scaled_font = NULL;
	int n_cr_glyphs = 0;
	cairo_glyph_t cr_glyphs[MAX_RUN_LENGTH];
	struct font_info *font = bold ? draw->font_bold : draw->font;

	g_return_if_fail (font != NULL);

	set_source_color_alpha (draw->cr, color, alpha);
	cairo_set_operator (draw->cr, CAIRO_OPERATOR_OVER);

	for (i = 0; i < n_requests; i++) {
		vteunistr c = requests[i].c;
		int x = requests[i].x;
		int y = requests[i].y + font->ascent;
		struct unistr_info *uinfo = font_info_get_unistr_info (font, c);
		union unistr_font_info *ufi = &uinfo->ufi;

		switch (uinfo->coverage) {
		default:
		case COVERAGE_UNKNOWN:
			g_assert_not_reached ();
			break;
		case COVERAGE_USE_PANGO_LAYOUT_LINE:
			cairo_move_to (draw->cr, x, y);
			pango_cairo_show_layout_line (draw->cr,
						      ufi->using_pango_layout_line.line);
			break;
		case COVERAGE_USE_PANGO_GLYPH_STRING:
			cairo_move_to (draw->cr, x, y);
			pango_cairo_show_glyph_string (draw->cr,
						       ufi->using_pango_glyph_string.font,
						       ufi->using_pango_glyph_string.glyph_string);
			break;
		case COVERAGE_USE_CAIRO_GLYPH:
			if (last_scaled_font != ufi->using_cairo_glyph.scaled_font || n_cr_glyphs == MAX_RUN_LENGTH) {
				if (n_cr_glyphs) {
					cairo_set_scaled_font (draw->cr, last_scaled_font);
					cairo_show_glyphs (draw->cr,
							   cr_glyphs,
							   n_cr_glyphs);
					n_cr_glyphs = 0;
				}
				last_scaled_font = ufi->using_cairo_glyph.scaled_font;
			}
			cr_glyphs[n_cr_glyphs].index = ufi->using_cairo_glyph.glyph_index;
			cr_glyphs[n_cr_glyphs].x = x;
			cr_glyphs[n_cr_glyphs].y = y;
			n_cr_glyphs++;
			break;
		}
	}
	if (n_cr_glyphs) {
		cairo_set_scaled_font (draw->cr, last_scaled_font);
		cairo_show_glyphs (draw->cr,
				   cr_glyphs,
				   n_cr_glyphs);
		n_cr_glyphs = 0;
	}
}

void
_vte_draw_text (struct _vte_draw *draw,
	       struct _vte_draw_text_request *requests, gsize n_requests,
	       const PangoColor *color, guchar alpha, gboolean bold)
{
	g_return_if_fail (draw->started);

	if (_vte_debug_on (VTE_DEBUG_DRAW)) {
		GString *string = g_string_new ("");
		gchar *str;
		gsize n;
		for (n = 0; n < n_requests; n++) {
			g_string_append_unichar (string, requests[n].c);
		}
		str = g_string_free (string, FALSE);
		g_printerr ("draw_text (\"%s\", len=%"G_GSIZE_FORMAT", color=(%d,%d,%d,%d), %s)\n",
				str, n_requests, color->red, color->green, color->blue,
				alpha, bold ? "bold" : "normal");
		g_free (str);
	}

	_vte_draw_text_internal (draw, requests, n_requests, color, alpha, bold);

	/* handle fonts that lack a bold face by double-striking */
	if (bold && !_vte_draw_has_bold (draw)) {
		gsize i;

		/* Take a step to the right. */
		for (i = 0; i < n_requests; i++) {
			requests[i].x++;
		}
		_vte_draw_text_internal (draw, requests,
					   n_requests, color, alpha, FALSE);
		/* Now take a step back. */
		for (i = 0; i < n_requests; i++) {
			requests[i].x--;
		}
	}
}

gboolean
_vte_draw_has_char (struct _vte_draw *draw, vteunistr c, gboolean bold)
{
	struct unistr_info *uinfo;

	_vte_debug_print (VTE_DEBUG_DRAW, "draw_has_char ('0x%04X', %s)\n", c,
			  bold ? "bold" : "normal");

	g_return_val_if_fail (draw->font != NULL, FALSE);

	uinfo = font_info_get_unistr_info (bold ? draw->font_bold : draw->font, c);
	return !uinfo->has_unknown_chars;
}

gboolean
_vte_draw_char (struct _vte_draw *draw,
	       struct _vte_draw_text_request *request,
	       const PangoColor *color, guchar alpha, gboolean bold)
{
	gboolean has_char;

	_vte_debug_print (VTE_DEBUG_DRAW,
			"draw_char ('%c', color=(%d,%d,%d,%d), %s)\n",
			request->c,
			color->red, color->green, color->blue,
			alpha, bold ? "bold" : "normal");

	has_char =_vte_draw_has_char (draw, request->c, bold);
	if (has_char)
		_vte_draw_text (draw, request, 1, color, alpha, bold);

	return has_char;
}

void
_vte_draw_draw_rectangle (struct _vte_draw *draw,
			 gint x, gint y, gint width, gint height,
			 const PangoColor *color, guchar alpha)
{
	g_return_if_fail (draw->started);

	_vte_debug_print (VTE_DEBUG_DRAW,
			"draw_rectangle (%d, %d, %d, %d, color=(%d,%d,%d,%d))\n",
			x,y,width,height,
			color->red, color->green, color->blue,
			alpha);

	cairo_set_operator (draw->cr, CAIRO_OPERATOR_OVER);
	cairo_rectangle (draw->cr, x+VTE_LINE_WIDTH/2., y+VTE_LINE_WIDTH/2., width-VTE_LINE_WIDTH, height-VTE_LINE_WIDTH);
	set_source_color_alpha (draw->cr, color, alpha);
	cairo_set_line_width (draw->cr, VTE_LINE_WIDTH);
	cairo_stroke (draw->cr);
}

void
_vte_draw_fill_rectangle (struct _vte_draw *draw,
			 gint x, gint y, gint width, gint height,
			 const PangoColor *color, guchar alpha)
{
	g_return_if_fail (draw->started);

	_vte_debug_print (VTE_DEBUG_DRAW,
			"draw_fill_rectangle (%d, %d, %d, %d, color=(%d,%d,%d,%d))\n",
			x,y,width,height,
			color->red, color->green, color->blue,
			alpha);

	cairo_set_operator (draw->cr, CAIRO_OPERATOR_OVER);
	cairo_rectangle (draw->cr, x, y, width, height);
	set_source_color_alpha (draw->cr, color, alpha);
	cairo_fill (draw->cr);
}

static guint
unistr_comp_hash (gconstpointer key)
{
	struct VteUnistrDecomp *decomp;
	decomp = &DECOMP_FROM_INDEX (GPOINTER_TO_UINT (key));
	return decomp->prefix ^ decomp->suffix;
}

static gboolean
unistr_comp_equal (gconstpointer a, gconstpointer b)
{
	return 0 == memcmp (&DECOMP_FROM_INDEX (GPOINTER_TO_UINT (a)),
			    &DECOMP_FROM_INDEX (GPOINTER_TO_UINT (b)),
			    sizeof (struct VteUnistrDecomp));
}

vteunistr
_vte_unistr_append_unichar (vteunistr s, gunichar c)
{
	struct VteUnistrDecomp decomp;
	vteunistr ret = 0;

	decomp.prefix = s;
	decomp.suffix = c;

	if (G_UNLIKELY (!unistr_decomp)) {
		unistr_decomp = g_array_new (FALSE, TRUE, sizeof (struct VteUnistrDecomp));
		g_array_set_size (unistr_decomp, 1);
		unistr_comp = g_hash_table_new (unistr_comp_hash, unistr_comp_equal);
	} else {
		DECOMP_FROM_INDEX (0) = decomp;
		ret = GPOINTER_TO_UINT (g_hash_table_lookup (unistr_comp, GUINT_TO_POINTER (0)));
	}

	if (G_UNLIKELY (!ret)) {
		/* sanity check to avoid OOM */
		if (G_UNLIKELY (_vte_unistr_strlen (s) > 10 || unistr_next - VTE_UNISTR_START > 100000))
			return s;

		ret = unistr_next++;
		g_array_append_val (unistr_decomp, decomp);
		g_hash_table_insert (unistr_comp,
				     GUINT_TO_POINTER (ret - VTE_UNISTR_START),
				     GUINT_TO_POINTER (ret));
	}

	return ret;
}

gunichar
_vte_unistr_get_base (vteunistr s)
{
	g_return_val_if_fail (s < unistr_next, s);
	while (G_UNLIKELY (s >= VTE_UNISTR_START))
		s = DECOMP_FROM_UNISTR (s).prefix;
	return (gunichar) s;
}

void
_vte_unistr_append_to_string (vteunistr s, GString *gs)
{
	g_return_if_fail (s < unistr_next);
	if (G_UNLIKELY (s >= VTE_UNISTR_START)) {
		struct VteUnistrDecomp *decomp;
		decomp = &DECOMP_FROM_UNISTR (s);
		_vte_unistr_append_to_string (decomp->prefix, gs);
		s = decomp->suffix;
	}
	g_string_append_unichar (gs, (gunichar) s);
}

int
_vte_unistr_strlen (vteunistr s)
{
	int len = 1;
	g_return_val_if_fail (s < unistr_next, len);
	while (G_UNLIKELY (s >= VTE_UNISTR_START)) {
		s = DECOMP_FROM_UNISTR (s).prefix;
		len++;
	}
	return len;
}

/* enumerations from "vte.h" */
GType
vte_terminal_erase_binding_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
 
  if (g_once_init_enter (&g_define_type_id__volatile)) {
    static const GEnumValue values[] = {
      { VTE_ERASE_AUTO, "VTE_ERASE_AUTO", "auto" },
      { VTE_ERASE_ASCII_BACKSPACE, "VTE_ERASE_ASCII_BACKSPACE", "ascii-backspace" },
      { VTE_ERASE_ASCII_DELETE, "VTE_ERASE_ASCII_DELETE", "ascii-delete" },
      { VTE_ERASE_DELETE_SEQUENCE, "VTE_ERASE_DELETE_SEQUENCE", "delete-sequence" },
      { VTE_ERASE_TTY, "VTE_ERASE_TTY", "tty" },
      { 0, NULL, NULL }
    };
    GType g_define_type_id = \
       g_enum_register_static (g_intern_static_string ("VteTerminalEraseBinding"), values);
      
    g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
  }
    
  return g_define_type_id__volatile;
}

GType
vte_terminal_cursor_blink_mode_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
 
  if (g_once_init_enter (&g_define_type_id__volatile)) {
    static const GEnumValue values[] = {
      { VTE_CURSOR_BLINK_SYSTEM, "VTE_CURSOR_BLINK_SYSTEM", "system" },
      { VTE_CURSOR_BLINK_ON, "VTE_CURSOR_BLINK_ON", "on" },
      { VTE_CURSOR_BLINK_OFF, "VTE_CURSOR_BLINK_OFF", "off" },
      { 0, NULL, NULL }
    };
    GType g_define_type_id = \
       g_enum_register_static (g_intern_static_string ("VteTerminalCursorBlinkMode"), values);
      
    g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
  }
    
  return g_define_type_id__volatile;
}

GType
vte_terminal_cursor_shape_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
 
  if (g_once_init_enter (&g_define_type_id__volatile)) {
    static const GEnumValue values[] = {
      { VTE_CURSOR_SHAPE_BLOCK, "VTE_CURSOR_SHAPE_BLOCK", "block" },
      { VTE_CURSOR_SHAPE_IBEAM, "VTE_CURSOR_SHAPE_IBEAM", "ibeam" },
      { VTE_CURSOR_SHAPE_UNDERLINE, "VTE_CURSOR_SHAPE_UNDERLINE", "underline" },
      { 0, NULL, NULL }
    };
    GType g_define_type_id = \
       g_enum_register_static (g_intern_static_string ("VteTerminalCursorShape"), values);
      
    g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
  }
    
  return g_define_type_id__volatile;
}

GType
vte_terminal_write_flags_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
 
  if (g_once_init_enter (&g_define_type_id__volatile)) {
    static const GEnumValue values[] = {
      { VTE_TERMINAL_WRITE_DEFAULT, "VTE_TERMINAL_WRITE_DEFAULT", "default" },
      { 0, NULL, NULL }
    };
    GType g_define_type_id = \
       g_enum_register_static (g_intern_static_string ("VteTerminalWriteFlags"), values);
      
    g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
  }
    
  return g_define_type_id__volatile;
}

/* enumerations from "vtepty.h" */
GType
vte_pty_flags_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
 
  if (g_once_init_enter (&g_define_type_id__volatile)) {
    static const GFlagsValue values[] = {
      { VTE_PTY_NO_LASTLOG, "VTE_PTY_NO_LASTLOG", "no-lastlog" },
      { VTE_PTY_NO_UTMP, "VTE_PTY_NO_UTMP", "no-utmp" },
      { VTE_PTY_NO_WTMP, "VTE_PTY_NO_WTMP", "no-wtmp" },
      { VTE_PTY_NO_HELPER, "VTE_PTY_NO_HELPER", "no-helper" },
      { VTE_PTY_NO_FALLBACK, "VTE_PTY_NO_FALLBACK", "no-fallback" },
      { VTE_PTY_DEFAULT, "VTE_PTY_DEFAULT", "default" },
      { 0, NULL, NULL }
    };
    GType g_define_type_id = \
       g_flags_register_static (g_intern_static_string ("VtePtyFlags"), values);
      
    g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
  }
    
  return g_define_type_id__volatile;
}

GType
vte_pty_error_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
 
  if (g_once_init_enter (&g_define_type_id__volatile)) {
    static const GEnumValue values[] = {
      { VTE_PTY_ERROR_PTY_HELPER_FAILED, "VTE_PTY_ERROR_PTY_HELPER_FAILED", "pty-helper-failed" },
      { VTE_PTY_ERROR_PTY98_FAILED, "VTE_PTY_ERROR_PTY98_FAILED", "pty98-failed" },
      { 0, NULL, NULL }
    };
    GType g_define_type_id = \
       g_enum_register_static (g_intern_static_string ("VtePtyError"), values);
      
    g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
  }
    
  return g_define_type_id__volatile;
}

/* enumerations from "vtedeprecated.h" */
GType
vte_terminal_anti_alias_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
 
  if (g_once_init_enter (&g_define_type_id__volatile)) {
    static const GEnumValue values[] = {
      { VTE_ANTI_ALIAS_USE_DEFAULT, "VTE_ANTI_ALIAS_USE_DEFAULT", "use-default" },
      { VTE_ANTI_ALIAS_FORCE_ENABLE, "VTE_ANTI_ALIAS_FORCE_ENABLE", "force-enable" },
      { VTE_ANTI_ALIAS_FORCE_DISABLE, "VTE_ANTI_ALIAS_FORCE_DISABLE", "force-disable" },
      { 0, NULL, NULL }
    };
    GType g_define_type_id = \
       g_enum_register_static (g_intern_static_string ("VteTerminalAntiAlias"), values);
      
    g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
  }
    
  return g_define_type_id__volatile;
}

/* process incoming data without copying */
static struct _vte_incoming_chunk *free_chunks;
static struct _vte_incoming_chunk *
get_chunk (void)
{
	struct _vte_incoming_chunk *chunk = NULL;
	if (free_chunks) {
		chunk = free_chunks;
		free_chunks = free_chunks->next;
	}
	if (chunk == NULL) {
		chunk = g_new (struct _vte_incoming_chunk, 1);
	}
	chunk->next = NULL;
	chunk->len = 0;
	return chunk;
}
static void
release_chunk (struct _vte_incoming_chunk *chunk)
{
	chunk->next = free_chunks;
	chunk->len = free_chunks ? free_chunks->len + 1 : 0;
	free_chunks = chunk;
}
static void
prune_chunks (guint len)
{
	struct _vte_incoming_chunk *chunk = NULL;
	if (len && free_chunks != NULL) {
	    if (free_chunks->len > len) {
		struct _vte_incoming_chunk *last;
		chunk = free_chunks;
		while (free_chunks->len > len) {
		    last = free_chunks;
		    free_chunks = free_chunks->next;
		}
		last->next = NULL;
	    }
	} else {
	    chunk = free_chunks;
	    free_chunks = NULL;
	}
	while (chunk != NULL) {
		struct _vte_incoming_chunk *next = chunk->next;
		g_free (chunk);
		chunk = next;
	}
}
static void
_vte_incoming_chunks_release (struct _vte_incoming_chunk *chunk)
{
	while (chunk) {
		struct _vte_incoming_chunk *next = chunk->next;
		release_chunk (chunk);
		chunk = next;
	}
}
static gsize
_vte_incoming_chunks_length (struct _vte_incoming_chunk *chunk)
{
	gsize len = 0;
	while (chunk) {
		len += chunk->len;
		chunk = chunk->next;
	}
	return len;
}
static gsize
_vte_incoming_chunks_count (struct _vte_incoming_chunk *chunk)
{
	gsize cnt = 0;
	while (chunk) {
		cnt ++;
		chunk = chunk->next;
	}
	return cnt;
}
static struct _vte_incoming_chunk *
_vte_incoming_chunks_reverse(struct _vte_incoming_chunk *chunk)
{
	struct _vte_incoming_chunk *prev = NULL;
	while (chunk) {
		struct _vte_incoming_chunk *next = chunk->next;
		chunk->next = prev;
		prev = chunk;
		chunk = next;
	}
	return prev;
}

#if GTK_CHECK_VERSION (2, 99, 0)
#ifdef VTE_DEBUG
G_DEFINE_TYPE_WITH_CODE(VteTerminal, vte_terminal, GTK_TYPE_WIDGET,
                        g_type_add_class_private (g_define_type_id, sizeof (VteTerminalClassPrivate));
                        G_IMPLEMENT_INTERFACE(GTK_TYPE_SCROLLABLE, NULL)
                        if (_vte_debug_on(VTE_DEBUG_LIFECYCLE)) {
                                g_printerr("vte_terminal_get_type()\n");
                        })
#else
G_DEFINE_TYPE_WITH_CODE(VteTerminal, vte_terminal, GTK_TYPE_WIDGET,
                        g_type_add_class_private (g_define_type_id, sizeof (VteTerminalClassPrivate));
                        G_IMPLEMENT_INTERFACE(GTK_TYPE_SCROLLABLE, NULL))
#endif
#else
#ifdef VTE_DEBUG
G_DEFINE_TYPE_WITH_CODE(VteTerminal, vte_terminal, GTK_TYPE_WIDGET,
		if (_vte_debug_on(VTE_DEBUG_LIFECYCLE)) {
			g_printerr("vte_terminal_get_type()\n");
		})
#else
G_DEFINE_TYPE(VteTerminal, vte_terminal, GTK_TYPE_WIDGET)
#endif
#endif /* GTK 3.0 */

/* Indexes in the "palette" color array for the dim colors.
 * Only the first %VTE_LEGACY_COLOR_SET_SIZE colors have dim versions.  */
static const guchar corresponding_dim_index[] = {16,88,28,100,18,90,30,102};

static void
vte_g_array_fill(GArray *array, gconstpointer item, guint final_size)
{
	if (array->len >= final_size)
		return;

	final_size -= array->len;
	do {
		g_array_append_vals(array, item, 1);
	} while (--final_size);
}


VteRowData *
_vte_terminal_ring_insert (VteTerminal *terminal, glong position, gboolean fill)
{
	VteRowData *row;
	VteRing *ring = terminal->pvt->screen->row_data;
	while (G_UNLIKELY (_vte_ring_next (ring) < position)) {
		row = _vte_ring_append (ring);
		_vte_row_data_fill (row, &terminal->pvt->screen->fill_defaults, terminal->column_count);
	}
	row = _vte_ring_insert (ring, position);
	if (fill)
		_vte_row_data_fill (row, &terminal->pvt->screen->fill_defaults, terminal->column_count);
	return row;
}

VteRowData *
_vte_terminal_ring_append (VteTerminal *terminal, gboolean fill)
{
	return _vte_terminal_ring_insert (terminal, _vte_ring_next (terminal->pvt->screen->row_data), fill);
}

void
_vte_terminal_ring_remove (VteTerminal *terminal, glong position)
{
	_vte_ring_remove (terminal->pvt->screen->row_data, position);
}

/* Reset defaults for character insertion. */
void
_vte_terminal_set_default_attributes(VteTerminal *terminal)
{
	VteScreen *screen;

	screen = terminal->pvt->screen;

	screen->defaults = basic_cell.cell;
	screen->color_defaults = screen->defaults;
	screen->fill_defaults = screen->defaults;
}

/* Cause certain cells to be repainted. */
void
_vte_invalidate_cells(VteTerminal *terminal,
		      glong column_start, gint column_count,
		      glong row_start, gint row_count)
{
	VteRegionRectangle rect;
	glong i;

	if (!column_count || !row_count) {
		return;
	}

	if (G_UNLIKELY (! gtk_widget_is_drawable (&terminal->widget)
				|| terminal->pvt->invalidated_all)) {
		return;
	}

	_vte_debug_print (VTE_DEBUG_UPDATES,
			"Invalidating cells at (%ld,%ld+%ld)x(%d,%d).\n",
			column_start, row_start,
			(long)terminal->pvt->screen->scroll_delta,
			column_count, row_count);
	_vte_debug_print (VTE_DEBUG_WORK, "?");

	/* Subtract the scrolling offset from the row start so that the
	 * resulting rectangle is relative to the visible portion of the
	 * buffer. */
	row_start -= terminal->pvt->screen->scroll_delta;

	/* Ensure the start of region is on screen */
	if (column_start > terminal->column_count ||
			row_start > terminal->row_count) {
		return;
	}

	/* Clamp the start values to reasonable numbers. */
	i = row_start + row_count;
	row_start = MAX (0, row_start);
	row_count = CLAMP (i - row_start, 0, terminal->row_count);

	i = column_start + column_count;
	column_start = MAX (0, column_start);
	column_count = CLAMP (i - column_start, 0 , terminal->column_count);

	if (!column_count || !row_count) {
		return;
	}
	if (column_count == terminal->column_count &&
			row_count == terminal->row_count) {
		_vte_invalidate_all (terminal);
		return;
	}

	/* Convert the column and row start and end to pixel values
	 * by multiplying by the size of a character cell.
	 * Always include the extra pixel border and overlap pixel.
	 */
	rect.x = column_start * terminal->char_width - 1;
	if (column_start != 0) {
		rect.x += terminal->pvt->inner_border.left;
	}
	rect.width = (column_start + column_count) * terminal->char_width + 3 + terminal->pvt->inner_border.left;
	if (column_start + column_count == terminal->column_count) {
		rect.width += terminal->pvt->inner_border.right;
	}
	rect.width -= rect.x;

	rect.y = row_start * terminal->char_height - 1;
	if (row_start != 0) {
		rect.y += terminal->pvt->inner_border.top;
	}
	rect.height = (row_start + row_count) * terminal->char_height + 2 + terminal->pvt->inner_border.top;
	if (row_start + row_count == terminal->row_count) {
		rect.height += terminal->pvt->inner_border.bottom;
	}
	rect.height -= rect.y;

	_vte_debug_print (VTE_DEBUG_UPDATES,
			"Invalidating pixels at (%d,%d)x(%d,%d).\n",
			rect.x, rect.y, rect.width, rect.height);

	if (terminal->pvt->active != NULL) {
		terminal->pvt->update_regions = g_slist_prepend (
				terminal->pvt->update_regions,
				gdk_region_rectangle (&rect));
		/* Wait a bit before doing any invalidation, just in
		 * case updates are coming in really soon. */
		add_update_timeout (terminal);
	} else {
		gdk_window_invalidate_rect (gtk_widget_get_window (&terminal->widget), &rect, FALSE);
	}

	_vte_debug_print (VTE_DEBUG_WORK, "!");
}

static void
_vte_invalidate_region (VteTerminal *terminal,
			glong scolumn, glong ecolumn,
			glong srow, glong erow,
			gboolean block)
{
	if (block || srow == erow) {
		_vte_invalidate_cells(terminal,
				scolumn, ecolumn - scolumn + 1,
				srow, erow - srow + 1);
	} else {
		_vte_invalidate_cells(terminal,
				scolumn,
				terminal->column_count - scolumn,
				srow, 1);
		_vte_invalidate_cells(terminal,
				0, terminal->column_count,
				srow + 1, erow - srow - 1);
		_vte_invalidate_cells(terminal,
				0, ecolumn + 1,
				erow, 1);
	}
}


/* Redraw the entire visible portion of the window. */
void
_vte_invalidate_all(VteTerminal *terminal)
{
	VteRegionRectangle rect;
	GtkAllocation allocation;

	g_assert(VTE_IS_TERMINAL(terminal));

	if (! gtk_widget_is_drawable (&terminal->widget)) {
		return;
	}
	if (terminal->pvt->invalidated_all) {
		return;
	}

	_vte_debug_print (VTE_DEBUG_WORK, "*");
	_vte_debug_print (VTE_DEBUG_UPDATES, "Invalidating all.\n");

	gtk_widget_get_allocation (&terminal->widget, &allocation);

	/* replace invalid regions with one covering the whole terminal */
	reset_update_regions (terminal);
	rect.x = rect.y = 0;
	rect.width = allocation.width;
	rect.height = allocation.height;
	terminal->pvt->invalidated_all = TRUE;

	if (terminal->pvt->active != NULL) {
		terminal->pvt->update_regions = g_slist_prepend (NULL,
				gdk_region_rectangle (&rect));
		/* Wait a bit before doing any invalidation, just in
		 * case updates are coming in really soon. */
		add_update_timeout (terminal);
	} else {
		gdk_window_invalidate_rect (gtk_widget_get_window (&terminal->widget), &rect, FALSE);
	}
}

/* Scroll a rectangular region up or down by a fixed number of lines,
 * negative = up, positive = down. */
void
_vte_terminal_scroll_region (VteTerminal *terminal,
			     long row, glong count, glong delta)
{
	if ((delta == 0) || (count == 0)) {
		/* Shenanigans! */
		return;
	}

	if (terminal->pvt->scroll_background || count >= terminal->row_count) {
		/* We have to repaint the entire window. */
		_vte_invalidate_all(terminal);
	} else {
		/* We have to repaint the area which is to be
		 * scrolled. */
		_vte_invalidate_cells(terminal,
				     0, terminal->column_count,
				     row, count);
	}
}

/* Emit a "selection_changed" signal. */
static void
vte_terminal_emit_selection_changed(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `selection-changed'.\n");
	g_signal_emit_by_name(terminal, "selection-changed");
}

/* Emit a "commit" signal. */
static void
vte_terminal_emit_commit(VteTerminal *terminal, const gchar *text, guint length)
{
	const char *result = NULL;
	char *wrapped = NULL;

	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `commit' of %d bytes.\n", length);

	if (length == (guint)-1) {
		length = strlen(text);
		result = text;
	} else {
		result = wrapped = g_slice_alloc(length + 1);
		memcpy(wrapped, text, length);
		wrapped[length] = '\0';
	}

	g_signal_emit_by_name(terminal, "commit", result, length);

	if(wrapped)
		g_slice_free1(length+1, wrapped);
}

/* Emit an "emulation-changed" signal. */
static void
vte_terminal_emit_emulation_changed(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `emulation-changed'.\n");
	g_signal_emit_by_name(terminal, "emulation-changed");
        g_object_notify(G_OBJECT(terminal), "emulation");

}

/* Emit an "encoding-changed" signal. */
static void
vte_terminal_emit_encoding_changed(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `encoding-changed'.\n");
	g_signal_emit_by_name(terminal, "encoding-changed");
        g_object_notify(G_OBJECT(terminal), "encoding");
}

/* Emit a "child-exited" signal. */
static void
vte_terminal_emit_child_exited(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `child-exited'.\n");
	g_signal_emit_by_name(terminal, "child-exited");
}

/* Emit a "contents_changed" signal. */
static void
vte_terminal_emit_contents_changed(VteTerminal *terminal)
{
	if (terminal->pvt->contents_changed_pending) {
		/* Update dingus match set. */
		vte_terminal_match_contents_clear(terminal);
		if (terminal->pvt->mouse_cursor_visible) {
			vte_terminal_match_hilite_update(terminal,
					terminal->pvt->mouse_last_x,
					terminal->pvt->mouse_last_y);
		}

		_vte_debug_print(VTE_DEBUG_SIGNALS,
				"Emitting `contents-changed'.\n");
		g_signal_emit_by_name(terminal, "contents-changed");
		terminal->pvt->contents_changed_pending = FALSE;
	}
}
void
_vte_terminal_queue_contents_changed(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Queueing `contents-changed'.\n");
	terminal->pvt->contents_changed_pending = TRUE;
}

/* Emit a "cursor_moved" signal. */
static void
vte_terminal_emit_cursor_moved(VteTerminal *terminal)
{
	if (terminal->pvt->cursor_moved_pending) {
		_vte_debug_print(VTE_DEBUG_SIGNALS,
				"Emitting `cursor-moved'.\n");
		g_signal_emit_by_name(terminal, "cursor-moved");
		terminal->pvt->cursor_moved_pending = FALSE;
	}
}
static void
vte_terminal_queue_cursor_moved(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Queueing `cursor-moved'.\n");
	terminal->pvt->cursor_moved_pending = TRUE;
}

static gboolean
vte_terminal_emit_eof(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `eof'.\n");
	GDK_THREADS_ENTER ();
	g_signal_emit_by_name(terminal, "eof");
	GDK_THREADS_LEAVE ();

	return FALSE;
}
/* Emit a "eof" signal. */
static void
vte_terminal_queue_eof(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Queueing `eof'.\n");
	g_idle_add_full (G_PRIORITY_HIGH,
		(GSourceFunc) vte_terminal_emit_eof,
		g_object_ref (terminal),
		g_object_unref);
}

/* Emit a "char-size-changed" signal. */
static void
vte_terminal_emit_char_size_changed(VteTerminal *terminal,
				    guint width, guint height)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `char-size-changed'.\n");
	g_signal_emit_by_name(terminal, "char-size-changed",
			      width, height);
/*         g_object_notify(G_OBJECT(terminal), "char-size"); */
}

/* Emit a "status-line-changed" signal. */
static void
_vte_terminal_emit_status_line_changed(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `status-line-changed'.\n");
	g_signal_emit_by_name(terminal, "status-line-changed");
/*         g_object_notify(G_OBJECT(terminal), "status-line"); */
}

/* Emit an "increase-font-size" signal. */
static void
vte_terminal_emit_increase_font_size(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `increase-font-size'.\n");
	g_signal_emit_by_name(terminal, "increase-font-size");
        g_object_notify(G_OBJECT(terminal), "font-scale");
}

/* Emit a "decrease-font-size" signal. */
static void
vte_terminal_emit_decrease_font_size(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `decrease-font-size'.\n");
	g_signal_emit_by_name(terminal, "decrease-font-size");
        g_object_notify(G_OBJECT(terminal), "font-scale");
}

/* Emit a "text-inserted" signal. */
void
_vte_terminal_emit_text_inserted(VteTerminal *terminal)
{
	if (!terminal->pvt->accessible_emit) {
		return;
	}
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `text-inserted'.\n");
	g_signal_emit_by_name(terminal, "text-inserted");
}

/* Emit a "text-deleted" signal. */
void
_vte_terminal_emit_text_deleted(VteTerminal *terminal)
{
	if (!terminal->pvt->accessible_emit) {
		return;
	}
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `text-deleted'.\n");
	g_signal_emit_by_name(terminal, "text-deleted");
}

/* Emit a "text-modified" signal. */
static void
vte_terminal_emit_text_modified(VteTerminal *terminal)
{
	if (!terminal->pvt->accessible_emit) {
		return;
	}
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `text-modified'.\n");
	g_signal_emit_by_name(terminal, "text-modified");
}

/* Emit a "text-scrolled" signal. */
static void
vte_terminal_emit_text_scrolled(VteTerminal *terminal, gint delta)
{
	if (!terminal->pvt->accessible_emit) {
		return;
	}
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `text-scrolled'(%d).\n", delta);
	g_signal_emit_by_name(terminal, "text-scrolled", delta);
}

/* Deselect anything which is selected and refresh the screen if needed. */
static void
vte_terminal_deselect_all(VteTerminal *terminal)
{
	if (terminal->pvt->has_selection) {
		gint sx, sy, ex, ey;

		_vte_debug_print(VTE_DEBUG_SELECTION,
				"Deselecting all text.\n");

		terminal->pvt->has_selection = FALSE;
		/* Don't free the current selection, as we need to keep
		 * hold of it for async copying from the clipboard. */

		vte_terminal_emit_selection_changed(terminal);

		sx = terminal->pvt->selection_start.col;
		sy = terminal->pvt->selection_start.row;
		ex = terminal->pvt->selection_end.col;
		ey = terminal->pvt->selection_end.row;
		_vte_invalidate_region(terminal,
				MIN (sx, ex), MAX (sx, ex),
				MIN (sy, ey),   MAX (sy, ey),
				FALSE);
	}
}

/* Remove a tabstop. */
void
_vte_terminal_clear_tabstop(VteTerminal *terminal, int column)
{
	g_assert(VTE_IS_TERMINAL(terminal));
	if (terminal->pvt->tabstops != NULL) {
		/* Remove a tab stop from the hash table. */
		g_hash_table_remove(terminal->pvt->tabstops,
				    GINT_TO_POINTER(2 * column + 1));
	}
}

/* Check if we have a tabstop at a given position. */
gboolean
_vte_terminal_get_tabstop(VteTerminal *terminal, int column)
{
	gpointer hash;
	g_assert(VTE_IS_TERMINAL(terminal));
	if (terminal->pvt->tabstops != NULL) {
		hash = g_hash_table_lookup(terminal->pvt->tabstops,
					   GINT_TO_POINTER(2 * column + 1));
		return (hash != NULL);
	} else {
		return FALSE;
	}
}

/* Reset the set of tab stops to the default. */
void
_vte_terminal_set_tabstop(VteTerminal *terminal, int column)
{
	g_assert(VTE_IS_TERMINAL(terminal));
	if (terminal->pvt->tabstops != NULL) {
		/* Just set a non-NULL pointer for this column number. */
		g_hash_table_insert(terminal->pvt->tabstops,
				    GINT_TO_POINTER(2 * column + 1),
				    terminal);
	}
}

/* Reset the set of tab stops to the default. */
static void
vte_terminal_set_default_tabstops(VteTerminal *terminal)
{
	int i, width = 0;
	if (terminal->pvt->tabstops != NULL) {
		g_hash_table_destroy(terminal->pvt->tabstops);
	}
	terminal->pvt->tabstops = g_hash_table_new(NULL, NULL);
	if (terminal->pvt->termcap != NULL) {
		width = _vte_termcap_find_numeric(terminal->pvt->termcap,
						  terminal->pvt->emulation,
						  "it");
	}
	if (width == 0) {
		width = VTE_TAB_WIDTH;
	}
	for (i = 0; i <= VTE_TAB_MAX; i += width) {
		_vte_terminal_set_tabstop(terminal, i);
	}
}

/* Clear the cache of the screen contents we keep. */
static void
vte_terminal_match_contents_clear(VteTerminal *terminal)
{
	g_assert(VTE_IS_TERMINAL(terminal));
	if (terminal->pvt->match_contents != NULL) {
		g_free(terminal->pvt->match_contents);
		terminal->pvt->match_contents = NULL;
	}
	if (terminal->pvt->match_attributes != NULL) {
		g_array_free(terminal->pvt->match_attributes, TRUE);
		terminal->pvt->match_attributes = NULL;
	}
	vte_terminal_match_hilite_clear(terminal);
}

/* Refresh the cache of the screen contents we keep. */
static gboolean
always_selected(VteTerminal *terminal, glong column, glong row, gpointer data)
{
	return TRUE;
}

static void
vte_terminal_match_contents_refresh(VteTerminal *terminal)
{
	GArray *array;
	vte_terminal_match_contents_clear(terminal);
	array = g_array_new(FALSE, TRUE, sizeof(struct _VteCharAttributes));
	terminal->pvt->match_contents = vte_terminal_get_text(terminal,
							      always_selected,
							      NULL,
							      array);
	terminal->pvt->match_attributes = array;
}

static void
regex_match_clear_cursor (struct vte_match_regex *regex)
{
        switch (regex->cursor_mode) {
                case VTE_REGEX_CURSOR_GDKCURSOR:
                        if (regex->cursor.cursor != NULL) {
                                gdk_cursor_unref(regex->cursor.cursor);
                                regex->cursor.cursor = NULL;
                        }
                        break;
                case VTE_REGEX_CURSOR_GDKCURSORTYPE:
                        break;
                case VTE_REGEX_CURSOR_NAME:
                        g_free (regex->cursor.cursor_name);
                        regex->cursor.cursor_name = NULL;
                        break;
		default:
			g_assert_not_reached ();
			return;
        }
}

static void
regex_match_clear (struct vte_match_regex *regex)
{
        regex_match_clear_cursor(regex);

        if (regex->mode == VTE_REGEX_GREGEX) {
                g_regex_unref(regex->regex.gregex.regex);
                regex->regex.gregex.regex = NULL;
        } else if (regex->mode == VTE_REGEX_VTE) {
                _vte_regex_free(regex->regex.reg);
                regex->regex.reg = NULL;
        }

        regex->tag = -1;
}

static void
vte_terminal_set_cursor_from_regex_match(VteTerminal *terminal, struct vte_match_regex *regex)
{
        GdkCursor *cursor = NULL;

        if (! gtk_widget_get_realized (&terminal->widget))
                return;

        switch (regex->cursor_mode) {
                case VTE_REGEX_CURSOR_GDKCURSOR:
                        if (regex->cursor.cursor != NULL) {
                                cursor = gdk_cursor_ref(regex->cursor.cursor);
                        }
                        break;
                case VTE_REGEX_CURSOR_GDKCURSORTYPE:
                        cursor = gdk_cursor_new_for_display(gtk_widget_get_display(GTK_WIDGET(terminal)), regex->cursor.cursor_type);
                        break;
                case VTE_REGEX_CURSOR_NAME:
                        cursor = gdk_cursor_new_from_name(gtk_widget_get_display(GTK_WIDGET(terminal)), regex->cursor.cursor_name);
                        break;
		default:
			g_assert_not_reached ();
			return;
        }

	gdk_window_set_cursor (gtk_widget_get_window (&terminal->widget), cursor);

        if (cursor)
                gdk_cursor_unref(cursor);
}

/**
 * vte_terminal_match_clear_all:
 * @terminal: a #VteTerminal
 *
 * Clears the list of regular expressions the terminal uses to highlight text
 * when the user moves the mouse cursor.
 */
void
vte_terminal_match_clear_all(VteTerminal *terminal)
{
	struct vte_match_regex *regex;
	guint i;
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	for (i = 0; i < terminal->pvt->match_regexes->len; i++) {
		regex = &g_array_index(terminal->pvt->match_regexes,
				       struct vte_match_regex,
				       i);
		/* Unless this is a hole, clean it up. */
		if (regex->tag >= 0) {
                        regex_match_clear (regex);
		}
	}
	g_array_set_size(terminal->pvt->match_regexes, 0);
	vte_terminal_match_hilite_clear(terminal);
}

/**
 * vte_terminal_match_remove:
 * @terminal: a #VteTerminal
 * @tag: the tag of the regex to remove
 *
 * Removes the regular expression which is associated with the given @tag from
 * the list of expressions which the terminal will highlight when the user
 * moves the mouse cursor over matching text.
 */
void
vte_terminal_match_remove(VteTerminal *terminal, int tag)
{
	struct vte_match_regex *regex;
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	if (terminal->pvt->match_regexes->len > (guint)tag) {
		/* The tag is an index, so find the corresponding struct. */
		regex = &g_array_index(terminal->pvt->match_regexes,
				       struct vte_match_regex,
				       tag);
		/* If it's already been removed, return. */
		if (regex->tag < 0) {
			return;
		}
		/* Remove this item and leave a hole in its place. */
                regex_match_clear (regex);
	}
	vte_terminal_match_hilite_clear(terminal);
}

static GdkCursor *
vte_terminal_cursor_new(VteTerminal *terminal, GdkCursorType cursor_type)
{
	GdkDisplay *display;
	GdkCursor *cursor;

	display = gtk_widget_get_display(&terminal->widget);
	cursor = gdk_cursor_new_for_display(display, cursor_type);
	return cursor;
}

/**
 * vte_terminal_match_add:
 * @terminal: a #VteTerminal
 * @match: a regular expression
 *
 * Adds a regular expression to the list of matching expressions.  When the
 * user moves the mouse cursor over a section of displayed text which matches
 * this expression, the text will be highlighted.
 *
 * Returns: an integer associated with this expression
 *
 * Deprecated: 0.17.1: Use vte_terminal_match_add_gregex() instead
 */
int
vte_terminal_match_add(VteTerminal *terminal, const char *match)
{
	struct vte_match_regex new_regex, *regex;
	guint ret;
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
        g_return_val_if_fail(terminal->pvt->match_regex_mode != VTE_REGEX_GREGEX, -1);
	g_return_val_if_fail(match != NULL, -1);
	g_return_val_if_fail(strlen(match) > 0, -1);

        terminal->pvt->match_regex_mode = VTE_REGEX_VTE;

	memset(&new_regex, 0, sizeof(new_regex));
        new_regex.mode = VTE_REGEX_VTE;
	new_regex.regex.reg = _vte_regex_compile(match);
	if (new_regex.regex.reg == NULL) {
		g_warning(_("Error compiling regular expression \"%s\"."),
			  match);
		return -1;
	}

	/* Search for a hole. */
	for (ret = 0; ret < terminal->pvt->match_regexes->len; ret++) {
		regex = &g_array_index(terminal->pvt->match_regexes,
				       struct vte_match_regex,
				       ret);
		if (regex->tag == -1) {
			break;
		}
	}
	/* Set the tag to the insertion point. */
	new_regex.tag = ret;
        new_regex.cursor_mode = VTE_REGEX_CURSOR_GDKCURSORTYPE;
        new_regex.cursor.cursor_type = VTE_DEFAULT_CURSOR;
	if (ret < terminal->pvt->match_regexes->len) {
		/* Overwrite. */
		g_array_index(terminal->pvt->match_regexes,
			      struct vte_match_regex,
			      ret) = new_regex;
	} else {
		/* Append. */
		g_array_append_val(terminal->pvt->match_regexes, new_regex);
	}
	return new_regex.tag;
}

/**
 * vte_terminal_match_add_gregex:
 * @terminal: a #VteTerminal
 * @regex: a #GRegex
 * @flags: the #GRegexMatchFlags to use when matching the regex
 *
 * Adds the regular expression @regex to the list of matching expressions.  When the
 * user moves the mouse cursor over a section of displayed text which matches
 * this expression, the text will be highlighted.
 *
 * Returns: an integer associated with this expression
 *
 * Since: 0.17.1
 */
int
vte_terminal_match_add_gregex(VteTerminal *terminal, GRegex *regex, GRegexMatchFlags flags)
{
	VteTerminalPrivate *pvt;
	struct vte_match_regex new_regex_match, *regex_match;
	guint ret, len;

	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
        g_return_val_if_fail(terminal->pvt->match_regex_mode != VTE_REGEX_VTE, -1);
	g_return_val_if_fail(regex != NULL, -1);

        pvt = terminal->pvt;
        pvt->match_regex_mode = VTE_REGEX_GREGEX;

	/* Search for a hole. */
        len = pvt->match_regexes->len;
	for (ret = 0; ret < len; ret++) {
		regex_match = &g_array_index(pvt->match_regexes,
                                             struct vte_match_regex,
                                             ret);
		if (regex_match->tag == -1) {
			break;
		}
	}

	/* Set the tag to the insertion point. */
        new_regex_match.mode = VTE_REGEX_GREGEX;
        new_regex_match.regex.gregex.regex = g_regex_ref(regex);
        new_regex_match.regex.gregex.flags = flags;
	new_regex_match.tag = ret;
        new_regex_match.cursor_mode = VTE_REGEX_CURSOR_GDKCURSORTYPE;
        new_regex_match.cursor.cursor_type = VTE_DEFAULT_CURSOR;
	if (ret < pvt->match_regexes->len) {
		/* Overwrite. */
		g_array_index(pvt->match_regexes,
			      struct vte_match_regex,
			      ret) = new_regex_match;
	} else {
		/* Append. */
		g_array_append_val(pvt->match_regexes, new_regex_match);
	}

	return new_regex_match.tag;
}

/**
 * vte_terminal_match_set_cursor:
 * @terminal: a #VteTerminal
 * @tag: the tag of the regex which should use the specified cursor
 * @cursor: (allow-none): the #GdkCursor which the terminal should use when the pattern is
 *   highlighted, or %NULL to use the standard cursor
 *
 * Sets which cursor the terminal will use if the pointer is over the pattern
 * specified by @tag.  The terminal keeps a reference to @cursor.
 *
 * Since: 0.11
 *
 */
void
vte_terminal_match_set_cursor(VteTerminal *terminal, int tag, GdkCursor *cursor)
{
	struct vte_match_regex *regex;
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail((guint) tag < terminal->pvt->match_regexes->len);
	regex = &g_array_index(terminal->pvt->match_regexes,
			       struct vte_match_regex,
			       tag);
        regex_match_clear_cursor(regex);
        regex->cursor_mode = VTE_REGEX_CURSOR_GDKCURSOR;
	regex->cursor.cursor = cursor ? gdk_cursor_ref(cursor) : NULL;
	vte_terminal_match_hilite_clear(terminal);
}

/**
 * vte_terminal_match_set_cursor_type:
 * @terminal: a #VteTerminal
 * @tag: the tag of the regex which should use the specified cursor
 * @cursor_type: a #GdkCursorType
 *
 * Sets which cursor the terminal will use if the pointer is over the pattern
 * specified by @tag.
 *
 * Since: 0.11.9
 *
 */
void
vte_terminal_match_set_cursor_type(VteTerminal *terminal,
				   int tag, GdkCursorType cursor_type)
{
	struct vte_match_regex *regex;
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail((guint) tag < terminal->pvt->match_regexes->len);
	regex = &g_array_index(terminal->pvt->match_regexes,
			       struct vte_match_regex,
			       tag);
        regex_match_clear_cursor(regex);
        regex->cursor_mode = VTE_REGEX_CURSOR_GDKCURSORTYPE;
	regex->cursor.cursor_type = cursor_type;
	vte_terminal_match_hilite_clear(terminal);
}

/**
 * vte_terminal_match_set_cursor_name:
 * @terminal: a #VteTerminal
 * @tag: the tag of the regex which should use the specified cursor
 * @cursor_name: the name of the cursor
 *
 * Sets which cursor the terminal will use if the pointer is over the pattern
 * specified by @tag.
 *
 * Since: 0.17.1
 *
 */
void
vte_terminal_match_set_cursor_name(VteTerminal *terminal,
				   int tag, const char *cursor_name)
{
	struct vte_match_regex *regex;
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
        g_return_if_fail(cursor_name != NULL);
	g_return_if_fail((guint) tag < terminal->pvt->match_regexes->len);
	regex = &g_array_index(terminal->pvt->match_regexes,
			       struct vte_match_regex,
			       tag);
        regex_match_clear_cursor(regex);
        regex->cursor_mode = VTE_REGEX_CURSOR_NAME;
	regex->cursor.cursor_name = g_strdup (cursor_name);
	vte_terminal_match_hilite_clear(terminal);
}

/* Check if a given cell on the screen contains part of a matched string.  If
 * it does, return the string, and store the match tag in the optional tag
 * argument. */
static char *
vte_terminal_match_check_internal_vte(VteTerminal *terminal,
                                      long column, glong row,
                                      int *tag, int *start, int *end)
{
	struct _vte_regex_match matches[256];
	guint i, j;
	gint k;
	gint start_blank, end_blank;
	int ret, offset;
	struct vte_match_regex *regex = NULL;
	struct _VteCharAttributes *attr = NULL;
	gssize sattr, eattr;
	gchar *line, eol;

	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Checking for match at (%ld,%ld).\n", row, column);
	*tag = -1;
	if (start != NULL) {
		*start = 0;
	}
	if (end != NULL) {
		*end = 0;
	}
	/* Map the pointer position to a portion of the string. */
	eattr = terminal->pvt->match_attributes->len;
	for (offset = eattr; offset--; ) {
		attr = &g_array_index(terminal->pvt->match_attributes,
				      struct _VteCharAttributes,
				      offset);
		if (row < attr->row) {
			eattr = offset;
		}
		if (row == attr->row &&
		    column == attr->column &&
		    terminal->pvt->match_contents[offset] != ' ') {
			break;
		}
	}

	_VTE_DEBUG_IF(VTE_DEBUG_EVENTS) {
		if (offset < 0)
			g_printerr("Cursor is not on a character.\n");
		else
			g_printerr("Cursor is on character '%c' at %d.\n",
					g_utf8_get_char (terminal->pvt->match_contents + offset),
					offset);
	}

	/* If the pointer isn't on a matchable character, bug out. */
	if (offset < 0) {
		return NULL;
	}

	/* If the pointer is on a newline, bug out. */
	if ((g_ascii_isspace(terminal->pvt->match_contents[offset])) ||
	    (terminal->pvt->match_contents[offset] == '\0')) {
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Cursor is on whitespace.\n");
		return NULL;
	}

	/* Snip off any final newlines. */
	while (terminal->pvt->match_contents[eattr] == '\n' ||
			terminal->pvt->match_contents[eattr] == '\0') {
		eattr--;
	}
	/* and scan forwards to find the end of this line */
	while (!(terminal->pvt->match_contents[eattr] == '\n' ||
			terminal->pvt->match_contents[eattr] == '\0')) {
		eattr++;
	}

	/* find the start of row */
	if (row == 0) {
		sattr = 0;
	} else {
		for (sattr = offset; sattr > 0; sattr--) {
			attr = &g_array_index(terminal->pvt->match_attributes,
					      struct _VteCharAttributes,
					      sattr);
			if (row > attr->row) {
				break;
			}
		}
	}
	/* Scan backwards to find the start of this line */
	while (sattr > 0 &&
		! (terminal->pvt->match_contents[sattr] == '\n' ||
		    terminal->pvt->match_contents[sattr] == '\0')) {
		sattr--;
	}
	/* and skip any initial newlines. */
	while (terminal->pvt->match_contents[sattr] == '\n' ||
		terminal->pvt->match_contents[sattr] == '\0') {
		sattr++;
	}
	if (eattr <= sattr) { /* blank line */
		return NULL;
	}
	if (eattr <= offset || sattr > offset) {
		/* nothing to match on this line */
		return NULL;
	}
	offset -= sattr;
	eattr -= sattr;

	/* temporarily shorten the contents to this row */
	line = terminal->pvt->match_contents + sattr;
	eol = line[eattr];
	line[eattr] = '\0';

	start_blank = 0;
	end_blank = eattr;

	/* Now iterate over each regex we need to match against. */
	for (i = 0; i < terminal->pvt->match_regexes->len; i++) {
		regex = &g_array_index(terminal->pvt->match_regexes,
				       struct vte_match_regex,
				       i);
		/* Skip holes. */
		if (regex->tag < 0) {
			continue;
		}
		/* We'll only match the first item in the buffer which
		 * matches, so we'll have to skip each match until we
		 * stop getting matches. */
		k = 0;
		ret = _vte_regex_exec(regex->regex.reg,
				      line + k,
				      G_N_ELEMENTS(matches),
				      matches);
		while (ret == 0) {
			gint ko = offset - k;
			gint sblank=G_MININT, eblank=G_MAXINT;
			for (j = 0;
			     j < G_N_ELEMENTS(matches) &&
			     matches[j].rm_so != -1;
			     j++) {
				/* The offsets should be "sane". */
				g_assert(matches[j].rm_so + k < eattr);
				g_assert(matches[j].rm_eo + k <= eattr);
				_VTE_DEBUG_IF(VTE_DEBUG_MISC) {
					gchar *match;
					struct _VteCharAttributes *_sattr, *_eattr;
					match = g_strndup(line + matches[j].rm_so + k,
							matches[j].rm_eo - matches[j].rm_so);
					_sattr = &g_array_index(terminal->pvt->match_attributes,
							struct _VteCharAttributes,
							matches[j].rm_so + k);
					_eattr = &g_array_index(terminal->pvt->match_attributes,
							struct _VteCharAttributes,
							matches[j].rm_eo + k - 1);
					g_printerr("Match %u `%s' from %d(%ld,%ld) to %d(%ld,%ld) (%d).\n",
							j, match,
							matches[j].rm_so + k,
							_sattr->column,
							_sattr->row,
							matches[j].rm_eo + k - 1,
							_eattr->column,
							_eattr->row,
							offset);
					g_free(match);

				}
				/* If the pointer is in this substring,
				 * then we're done. */
				if (ko >= matches[j].rm_so &&
				    ko < matches[j].rm_eo) {
					gchar *result;
					if (tag != NULL) {
						*tag = regex->tag;
					}
					if (start != NULL) {
						*start = sattr + k + matches[j].rm_so;
					}
					if (end != NULL) {
						*end = sattr + k + matches[j].rm_eo - 1;
					}
                                        vte_terminal_set_cursor_from_regex_match(terminal, regex);
					result = g_strndup(line + k + matches[j].rm_so,
							 matches[j].rm_eo - matches[j].rm_so);
					line[eattr] = eol;
					return result;
				}
				if (ko > matches[j].rm_eo &&
						matches[j].rm_eo > sblank) {
					sblank = matches[j].rm_eo;
				}
				if (ko < matches[j].rm_so &&
						matches[j].rm_so < eblank) {
					eblank = matches[j].rm_so;
				}
			}
			if (k + sblank > start_blank) {
				start_blank = k + sblank;
			}
			if (k + eblank < end_blank) {
				end_blank = k + eblank;
			}
			/* Skip past the beginning of this match to
			 * look for more. */
			k += matches[0].rm_so + 1;
			if (k > offset) {
				break;
			}
			ret = _vte_regex_exec(regex->regex.reg,
					      line + k,
					      G_N_ELEMENTS(matches),
					      matches);
		}
	}
	line[eattr] = eol;
	if (start != NULL) {
		*start = sattr + start_blank;
	}
	if (end != NULL) {
		*end = sattr + end_blank;
	}
	return NULL;
}

/* Check if a given cell on the screen contains part of a matched string.  If
 * it does, return the string, and store the match tag in the optional tag
 * argument. */
static char *
vte_terminal_match_check_internal_gregex(VteTerminal *terminal,
                                         long column, glong row,
                                         int *tag, int *start, int *end)
{
	gint start_blank, end_blank;
        guint i;
	int offset;
	struct vte_match_regex *regex = NULL;
	struct _VteCharAttributes *attr = NULL;
	gssize sattr, eattr;
	gchar *line, eol;
        GMatchInfo *match_info;

	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Checking for gregex match at (%ld,%ld).\n", row, column);
	*tag = -1;
	if (start != NULL) {
		*start = 0;
	}
	if (end != NULL) {
		*end = 0;
	}
	/* Map the pointer position to a portion of the string. */
	eattr = terminal->pvt->match_attributes->len;
	for (offset = eattr; offset--; ) {
		attr = &g_array_index(terminal->pvt->match_attributes,
				      struct _VteCharAttributes,
				      offset);
		if (row < attr->row) {
			eattr = offset;
		}
		if (row == attr->row &&
		    column == attr->column &&
		    terminal->pvt->match_contents[offset] != ' ') {
			break;
		}
	}

	_VTE_DEBUG_IF(VTE_DEBUG_EVENTS) {
		if (offset < 0)
			g_printerr("Cursor is not on a character.\n");
		else
			g_printerr("Cursor is on character '%c' at %d.\n",
					g_utf8_get_char (terminal->pvt->match_contents + offset),
					offset);
	}

	/* If the pointer isn't on a matchable character, bug out. */
	if (offset < 0) {
		return NULL;
	}

	/* If the pointer is on a newline, bug out. */
	if ((g_ascii_isspace(terminal->pvt->match_contents[offset])) ||
	    (terminal->pvt->match_contents[offset] == '\0')) {
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Cursor is on whitespace.\n");
		return NULL;
	}

	/* Snip off any final newlines. */
	while (terminal->pvt->match_contents[eattr] == '\n' ||
			terminal->pvt->match_contents[eattr] == '\0') {
		eattr--;
	}
	/* and scan forwards to find the end of this line */
	while (!(terminal->pvt->match_contents[eattr] == '\n' ||
			terminal->pvt->match_contents[eattr] == '\0')) {
		eattr++;
	}

	/* find the start of row */
	if (row == 0) {
		sattr = 0;
	} else {
		for (sattr = offset; sattr > 0; sattr--) {
			attr = &g_array_index(terminal->pvt->match_attributes,
					      struct _VteCharAttributes,
					      sattr);
			if (row > attr->row) {
				break;
			}
		}
	}
	/* Scan backwards to find the start of this line */
	while (sattr > 0 &&
		! (terminal->pvt->match_contents[sattr] == '\n' ||
		    terminal->pvt->match_contents[sattr] == '\0')) {
		sattr--;
	}
	/* and skip any initial newlines. */
	while (terminal->pvt->match_contents[sattr] == '\n' ||
		terminal->pvt->match_contents[sattr] == '\0') {
		sattr++;
	}
	if (eattr <= sattr) { /* blank line */
		return NULL;
	}
	if (eattr <= offset || sattr > offset) {
		/* nothing to match on this line */
		return NULL;
	}
	offset -= sattr;
	eattr -= sattr;

	/* temporarily shorten the contents to this row */
	line = terminal->pvt->match_contents + sattr;
	eol = line[eattr];
	line[eattr] = '\0';

	start_blank = 0;
	end_blank = eattr;

	/* Now iterate over each regex we need to match against. */
	for (i = 0; i < terminal->pvt->match_regexes->len; i++) {
		regex = &g_array_index(terminal->pvt->match_regexes,
				       struct vte_match_regex,
				       i);
		/* Skip holes. */
		if (regex->tag < 0) {
			continue;
		}
		/* We'll only match the first item in the buffer which
		 * matches, so we'll have to skip each match until we
		 * stop getting matches. */
                if (!g_regex_match_full(regex->regex.gregex.regex,
                                        line, -1, 0,
                                        regex->regex.gregex.flags,
                                        &match_info,
                                        NULL)) {
                        g_match_info_free(match_info);
                        continue;
                }

                while (g_match_info_matches(match_info)) {
			gint ko = offset;
			gint sblank=G_MININT, eblank=G_MAXINT;
                        gint rm_so, rm_eo;

                        if (g_match_info_fetch_pos (match_info, 0, &rm_so, &rm_eo)) {
				/* The offsets should be "sane". */
				g_assert(rm_so < eattr);
				g_assert(rm_eo <= eattr);
				_VTE_DEBUG_IF(VTE_DEBUG_MISC) {
					gchar *match;
					struct _VteCharAttributes *_sattr, *_eattr;
					match = g_strndup(line + rm_so, rm_eo - rm_so);
					_sattr = &g_array_index(terminal->pvt->match_attributes,
							struct _VteCharAttributes,
							rm_so);
					_eattr = &g_array_index(terminal->pvt->match_attributes,
							struct _VteCharAttributes,
							rm_eo - 1);
					g_printerr("Match `%s' from %d(%ld,%ld) to %d(%ld,%ld) (%d).\n",
							match,
							rm_so,
							_sattr->column,
							_sattr->row,
							rm_eo - 1,
							_eattr->column,
							_eattr->row,
							offset);
					g_free(match);

				}
				/* If the pointer is in this substring,
				 * then we're done. */
				if (ko >= rm_so &&
				    ko < rm_eo) {
					gchar *result;
					if (tag != NULL) {
						*tag = regex->tag;
					}
					if (start != NULL) {
						*start = sattr + rm_so;
					}
					if (end != NULL) {
						*end = sattr + rm_eo - 1;
					}
                                        vte_terminal_set_cursor_from_regex_match(terminal, regex);
                                        result = g_match_info_fetch(match_info, 0);
					line[eattr] = eol;

                                        g_match_info_free(match_info);
					return result;
				}
				if (ko > rm_eo &&
						rm_eo > sblank) {
					sblank = rm_eo;
				}
				if (ko < rm_so &&
						rm_so < eblank) {
					eblank = rm_so;
				}
			}
			if (sblank > start_blank) {
				start_blank = sblank;
			}
			if (eblank < end_blank) {
				end_blank = eblank;
			}

                        g_match_info_next(match_info, NULL);
		}

                g_match_info_free(match_info);
	}
	line[eattr] = eol;
	if (start != NULL) {
		*start = sattr + start_blank;
	}
	if (end != NULL) {
		*end = sattr + end_blank;
	}
	return NULL;
}

static char *
vte_terminal_match_check_internal(VteTerminal *terminal,
                                  long column, glong row,
                                  int *tag, int *start, int *end)
{
	if (terminal->pvt->match_contents == NULL) {
		vte_terminal_match_contents_refresh(terminal);
	}

        if (terminal->pvt->match_regex_mode == VTE_REGEX_GREGEX)
                return vte_terminal_match_check_internal_gregex(terminal, column, row, tag, start, end);
        if (terminal->pvt->match_regex_mode == VTE_REGEX_VTE)
                return vte_terminal_match_check_internal_vte(terminal, column, row, tag, start, end);
        return NULL;
}

static gboolean
rowcol_inside_match (VteTerminal *terminal, glong row, glong col)
{
	if (terminal->pvt->match_start.row == terminal->pvt->match_end.row) {
		return row == terminal->pvt->match_start.row &&
			col >= terminal->pvt->match_start.col &&
			col <= terminal->pvt->match_end.col;
	} else {
		if (row < terminal->pvt->match_start.row ||
				row > terminal->pvt->match_end.row) {
			return FALSE;
		}
		if (row == terminal->pvt->match_start.row) {
			return col >= terminal->pvt->match_start.col;
		}
		if (row == terminal->pvt->match_end.row) {
			return col <= terminal->pvt->match_end.col;
		}
		return TRUE;
	}
}

/**
 * vte_terminal_match_check:
 * @terminal: a #VteTerminal
 * @column: the text column
 * @row: the text row
 * @tag: (out) (allow-none): a location to store the tag, or %NULL
 *
 * Checks if the text in and around the specified position matches any of the
 * regular expressions previously set using vte_terminal_match_add().  If a
 * match exists, the text string is returned and if @tag is not %NULL, the number
 * associated with the matched regular expression will be stored in @tag.
 *
 * If more than one regular expression has been set with
 * vte_terminal_match_add(), then expressions are checked in the order in
 * which they were added.
 *
 * Returns: (transfer full): a newly allocated string which matches one of the previously
 *   set regular expressions
 */
char *
vte_terminal_match_check(VteTerminal *terminal, glong column, glong row,
			 int *tag)
{
	long delta;
	char *ret;
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	delta = terminal->pvt->screen->scroll_delta;
	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Checking for match at (%ld,%ld).\n",
			row, column);
	if (rowcol_inside_match (terminal, row + delta, column)) {
		if (tag) {
			*tag = terminal->pvt->match_tag;
		}
		ret = terminal->pvt->match != NULL ?
			g_strdup (terminal->pvt->match) :
			NULL;
	} else {
		ret = vte_terminal_match_check_internal(terminal,
				column, row + delta,
				tag, NULL, NULL);
	}
	_VTE_DEBUG_IF(VTE_DEBUG_EVENTS) {
		if (ret != NULL) g_printerr("Matched `%s'.\n", ret);
	}
	return ret;
}

/* Emit an adjustment changed signal on our adjustment object. */
static void
vte_terminal_emit_adjustment_changed(VteTerminal *terminal)
{
	if (terminal->pvt->adjustment_changed_pending) {
		VteScreen *screen = terminal->pvt->screen;
		gboolean changed = FALSE;
		glong v;
		gdouble current;

		g_object_freeze_notify (G_OBJECT (terminal->adjustment));

		v = _vte_ring_delta (screen->row_data);
		current = gtk_adjustment_get_lower(terminal->adjustment);
		if (current != v) {
			_vte_debug_print(VTE_DEBUG_ADJ,
					"Changing lower bound from %.0f to %ld\n",
					 current, v);
			gtk_adjustment_set_lower(terminal->adjustment, v);
			changed = TRUE;
		}

		/* The upper value is the number of rows which might be visible.  (Add
		 * one to the cursor offset because it's zero-based.) */
		v = MAX(_vte_ring_next(screen->row_data),
				screen->cursor_current.row + 1);
		current = gtk_adjustment_get_upper(terminal->adjustment);
		if (current != v) {
			_vte_debug_print(VTE_DEBUG_ADJ,
					"Changing upper bound from %.0f to %ld\n",
					 current, v);
			gtk_adjustment_set_upper(terminal->adjustment, v);
			changed = TRUE;
		}

		g_object_thaw_notify (G_OBJECT (terminal->adjustment));

		if (changed)
			_vte_debug_print(VTE_DEBUG_SIGNALS,
					"Emitting adjustment_changed.\n");
		terminal->pvt->adjustment_changed_pending = FALSE;
	}
	if (terminal->pvt->adjustment_value_changed_pending) {
		glong v, delta;
		_vte_debug_print(VTE_DEBUG_SIGNALS,
				"Emitting adjustment_value_changed.\n");
		terminal->pvt->adjustment_value_changed_pending = FALSE;
		v = round (gtk_adjustment_get_value(terminal->adjustment));
		if (v != terminal->pvt->screen->scroll_delta) {
			/* this little dance is so that the scroll_delta is
			 * updated immediately, but we still handled scrolling
			 * via the adjustment - e.g. user interaction with the
			 * scrollbar
			 */
			delta = terminal->pvt->screen->scroll_delta;
			terminal->pvt->screen->scroll_delta = v;
			gtk_adjustment_set_value(terminal->adjustment, delta);
		}
	}
}

/* Queue an adjustment-changed signal to be delivered when convenient. */
static inline void
vte_terminal_queue_adjustment_changed(VteTerminal *terminal)
{
	terminal->pvt->adjustment_changed_pending = TRUE;
	add_update_timeout (terminal);
}

static void
vte_terminal_queue_adjustment_value_changed(VteTerminal *terminal, glong v)
{
	if (v != terminal->pvt->screen->scroll_delta) {
		terminal->pvt->screen->scroll_delta = v;
		terminal->pvt->adjustment_value_changed_pending = TRUE;
		add_update_timeout (terminal);
	}
}

static void
vte_terminal_queue_adjustment_value_changed_clamped(VteTerminal *terminal, glong v)
{
	gdouble lower, upper;

	lower = gtk_adjustment_get_lower(terminal->adjustment);
	upper = gtk_adjustment_get_upper(terminal->adjustment);

	v = CLAMP(v, lower, MAX (lower, upper - terminal->row_count));

	vte_terminal_queue_adjustment_value_changed (terminal, v);
}


void
_vte_terminal_adjust_adjustments(VteTerminal *terminal)
{
	VteScreen *screen;
	long delta;

	g_assert(terminal->pvt->screen != NULL);
	g_assert(terminal->pvt->screen->row_data != NULL);

	vte_terminal_queue_adjustment_changed(terminal);

	/* The lower value should be the first row in the buffer. */
	screen = terminal->pvt->screen;
	delta = _vte_ring_delta(screen->row_data);
	/* Snap the insert delta and the cursor position to be in the visible
	 * area.  Leave the scrolling delta alone because it will be updated
	 * when the adjustment changes. */
	screen->insert_delta = MAX(screen->insert_delta, delta);
	screen->cursor_current.row = MAX(screen->cursor_current.row,
					 screen->insert_delta);

	if (screen->scroll_delta > screen->insert_delta) {
		vte_terminal_queue_adjustment_value_changed(terminal,
				screen->insert_delta);
	}
}

/* Update the adjustment field of the widget.  This function should be called
 * whenever we add rows to or remove rows from the history or switch screens. */
static void
_vte_terminal_adjust_adjustments_full (VteTerminal *terminal)
{
	gboolean changed = FALSE;
	gdouble v;

	g_assert(terminal->pvt->screen != NULL);
	g_assert(terminal->pvt->screen->row_data != NULL);

	_vte_terminal_adjust_adjustments(terminal);

        g_object_freeze_notify(G_OBJECT(terminal->adjustment));

	/* The step increment should always be one. */
	v = gtk_adjustment_get_step_increment(terminal->adjustment);
	if (v != 1) {
		_vte_debug_print(VTE_DEBUG_ADJ,
				"Changing step increment from %.0lf to %ld\n",
				v, terminal->row_count);
		gtk_adjustment_set_step_increment(terminal->adjustment, 1);
		changed = TRUE;
	}

	/* Set the number of rows the user sees to the number of rows the
	 * user sees. */
	v = gtk_adjustment_get_page_size(terminal->adjustment);
	if (v != terminal->row_count) {
		_vte_debug_print(VTE_DEBUG_ADJ,
				"Changing page size from %.0f to %ld\n",
				 v, terminal->row_count);
		gtk_adjustment_set_page_size(terminal->adjustment,
					     terminal->row_count);
		changed = TRUE;
	}

	/* Clicking in the empty area should scroll one screen, so set the
	 * page size to the number of visible rows. */
	v = gtk_adjustment_get_page_increment(terminal->adjustment);
	if (v != terminal->row_count) {
		_vte_debug_print(VTE_DEBUG_ADJ,
				"Changing page increment from "
				"%.0f to %ld\n",
				v, terminal->row_count);
		gtk_adjustment_set_page_increment(terminal->adjustment,
						  terminal->row_count);
		changed = TRUE;
	}

	g_object_thaw_notify(G_OBJECT(terminal->adjustment));

	if (changed)
		_vte_debug_print(VTE_DEBUG_SIGNALS,
				"Emitting adjustment_changed.\n");
}

/* Scroll a fixed number of lines up or down in the current screen. */
static void
vte_terminal_scroll_lines(VteTerminal *terminal, gint lines)
{
	glong destination;
	_vte_debug_print(VTE_DEBUG_ADJ, "Scrolling %d lines.\n", lines);
	/* Calculate the ideal position where we want to be before clamping. */
	destination = terminal->pvt->screen->scroll_delta;
	destination += lines;
	/* Tell the scrollbar to adjust itself. */
	vte_terminal_queue_adjustment_value_changed_clamped (terminal, destination);
}

/* Scroll a fixed number of pages up or down, in the current screen. */
static void
vte_terminal_scroll_pages(VteTerminal *terminal, gint pages)
{
	vte_terminal_scroll_lines(terminal, pages * terminal->row_count);
}

/* Scroll so that the scroll delta is the minimum value. */
static void
vte_terminal_maybe_scroll_to_top(VteTerminal *terminal)
{
	vte_terminal_queue_adjustment_value_changed (terminal,
			_vte_ring_delta(terminal->pvt->screen->row_data));
}

static void
vte_terminal_maybe_scroll_to_bottom(VteTerminal *terminal)
{
	glong delta;
	delta = terminal->pvt->screen->insert_delta;
	vte_terminal_queue_adjustment_value_changed (terminal, delta);
	_vte_debug_print(VTE_DEBUG_ADJ,
			"Snapping to bottom of screen\n");
}

static void
_vte_terminal_setup_utf8 (VteTerminal *terminal)
{
        VteTerminalPrivate *pvt = terminal->pvt;
        GError *error = NULL;

        if (!vte_pty_set_utf8(pvt->pty,
                              strcmp(terminal->pvt->encoding, "UTF-8") == 0,
                              &error)) {
                g_warning ("Failed to set UTF8 mode: %s\n", error->message);
                g_error_free (error);
        }
}

/**
 * vte_terminal_set_encoding:
 * @terminal: a #VteTerminal
 * @codeset: (allow-none): a valid #GIConv target, or %NULL to use the default encoding
 *
 * Changes the encoding the terminal will expect data from the child to
 * be encoded with.  For certain terminal types, applications executing in the
 * terminal can change the encoding.  The default encoding is defined by the
 * application's locale settings.
 */
void
vte_terminal_set_encoding(VteTerminal *terminal, const char *codeset)
{
        VteTerminalPrivate *pvt;
        GObject *object;
	const char *old_codeset;
	VteConv conv;
	char *obuf1, *obuf2;
	gsize bytes_written;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        object = G_OBJECT(terminal);
        pvt = terminal->pvt;

	old_codeset = pvt->encoding;
	if (codeset == NULL) {
		g_get_charset(&codeset);
	}
	if ((old_codeset != NULL) && (strcmp(codeset, old_codeset) == 0)) {
		/* Nothing to do! */
		return;
	}

        g_object_freeze_notify(object);

	/* Open new conversions. */
	conv = _vte_conv_open(codeset, "UTF-8");
	if (conv == VTE_INVALID_CONV) {
		g_warning(_("Unable to convert characters from %s to %s."),
			  "UTF-8", codeset);
		/* fallback to no conversion */
		conv = _vte_conv_open(codeset = "UTF-8", "UTF-8");
	}
	if (terminal->pvt->outgoing_conv != VTE_INVALID_CONV) {
		_vte_conv_close(terminal->pvt->outgoing_conv);
	}
	terminal->pvt->outgoing_conv = conv;

	/* Set the terminal's encoding to the new value. */
	terminal->pvt->encoding = g_intern_string(codeset);

	/* Convert any buffered output bytes. */
	if ((_vte_buffer_length(terminal->pvt->outgoing) > 0) &&
	    (old_codeset != NULL)) {
		/* Convert back to UTF-8. */
		obuf1 = g_convert((gchar *)terminal->pvt->outgoing->data,
				  _vte_buffer_length(terminal->pvt->outgoing),
				  "UTF-8",
				  old_codeset,
				  NULL,
				  &bytes_written,
				  NULL);
		if (obuf1 != NULL) {
			/* Convert to the new encoding. */
			obuf2 = g_convert(obuf1,
					  bytes_written,
					  codeset,
					  "UTF-8",
					  NULL,
					  &bytes_written,
					  NULL);
			if (obuf2 != NULL) {
				_vte_buffer_clear(terminal->pvt->outgoing);
				_vte_buffer_append(terminal->pvt->outgoing,
						   obuf2, bytes_written);
				g_free(obuf2);
			}
			g_free(obuf1);
		}
	}

	/* Set the encoding for incoming text. */
	_vte_iso2022_state_set_codeset(terminal->pvt->iso2022,
				       terminal->pvt->encoding);

	_vte_debug_print(VTE_DEBUG_IO,
			"Set terminal encoding to `%s'.\n",
			terminal->pvt->encoding);
	vte_terminal_emit_encoding_changed(terminal);

        g_object_thaw_notify(object);
}

/**
 * vte_terminal_get_encoding:
 * @terminal: a #VteTerminal
 *
 * Determines the name of the encoding in which the terminal expects data to be
 * encoded.
 *
 * Returns: (transfer none): the current encoding for the terminal
 */
const char *
vte_terminal_get_encoding(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return terminal->pvt->encoding;
}

static inline VteRowData *
vte_terminal_insert_rows (VteTerminal *terminal, guint cnt)
{
	VteRowData *row;
	do {
		row = _vte_terminal_ring_append (terminal, FALSE);
	} while(--cnt);
	return row;
}


/* Make sure we have enough rows and columns to hold data at the current
 * cursor position. */
VteRowData *
_vte_terminal_ensure_row (VteTerminal *terminal)
{
	VteRowData *row;
	VteScreen *screen;
	gint delta;
	glong v;

	/* Must make sure we're in a sane area. */
	screen = terminal->pvt->screen;
	v = screen->cursor_current.row;

	/* Figure out how many rows we need to add. */
	delta = v - _vte_ring_next(screen->row_data) + 1;
	if (delta > 0) {
		row = vte_terminal_insert_rows (terminal, delta);
		_vte_terminal_adjust_adjustments(terminal);
	} else {
		/* Find the row the cursor is in. */
		row = _vte_ring_index_writable (screen->row_data, v);
	}
	g_assert(row != NULL);

	return row;
}

static VteRowData *
vte_terminal_ensure_cursor(VteTerminal *terminal)
{
	VteRowData *row;

	row = _vte_terminal_ensure_row (terminal);
	_vte_row_data_fill (row, &basic_cell.cell, terminal->pvt->screen->cursor_current.col);

	return row;
}

/* Update the insert delta so that the screen which includes it also
 * includes the end of the buffer. */
void
_vte_terminal_update_insert_delta(VteTerminal *terminal)
{
	long delta, rows;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	/* The total number of lines.  Add one to the cursor offset
	 * because it's zero-based. */
	rows = _vte_ring_next (screen->row_data);
	delta = screen->cursor_current.row - rows + 1;
	if (G_UNLIKELY (delta > 0)) {
		vte_terminal_insert_rows (terminal, delta);
		rows = _vte_ring_next (screen->row_data);
	}

	/* Make sure that the bottom row is visible, and that it's in
	 * the buffer (even if it's empty).  This usually causes the
	 * top row to become a history-only row. */
	delta = screen->insert_delta;
	delta = MIN(delta, rows - terminal->row_count);
	delta = MAX(delta,
		    screen->cursor_current.row - (terminal->row_count - 1));
	delta = MAX(delta, _vte_ring_delta(screen->row_data));

	/* Adjust the insert delta and scroll if needed. */
	if (delta != screen->insert_delta) {
		screen->insert_delta = delta;
		_vte_terminal_adjust_adjustments(terminal);
	}
}

/* Show or hide the pointer. */
void
_vte_terminal_set_pointer_visible(VteTerminal *terminal, gboolean visible)
{
	GdkWindow *window;
	struct vte_match_regex *regex = NULL;

	terminal->pvt->mouse_cursor_visible = visible;

        if (! gtk_widget_get_realized (&terminal->widget))
                return;

	window = gtk_widget_get_window (&terminal->widget);

	if (visible || !terminal->pvt->mouse_autohide) {
		if (terminal->pvt->mouse_tracking_mode) {
			_vte_debug_print(VTE_DEBUG_CURSOR,
					"Setting mousing cursor.\n");
			gdk_window_set_cursor (window, terminal->pvt->mouse_mousing_cursor);
		} else
		if ( (guint)terminal->pvt->match_tag < terminal->pvt->match_regexes->len) {
			regex = &g_array_index(terminal->pvt->match_regexes,
					       struct vte_match_regex,
					       terminal->pvt->match_tag);
                        vte_terminal_set_cursor_from_regex_match(terminal, regex);
		} else {
			_vte_debug_print(VTE_DEBUG_CURSOR,
					"Setting default mouse cursor.\n");
			gdk_window_set_cursor (window, terminal->pvt->mouse_default_cursor);
		}
	} else {
		_vte_debug_print(VTE_DEBUG_CURSOR,
				"Setting to invisible cursor.\n");
		gdk_window_set_cursor (window, terminal->pvt->mouse_inviso_cursor);
	}
}

/**
 * vte_terminal_new:
 *
 * Creates a new terminal widget.
 *
 * Returns: (transfer full) (type Vte.Terminal): a new #VteTerminal object
 */
GtkWidget *
vte_terminal_new(void)
{
	return g_object_new(VTE_TYPE_TERMINAL, NULL);
}

/* Set up a palette entry with a more-or-less match for the requested color. */
static void
vte_terminal_set_color_internal(VteTerminal *terminal, int entry,
				const GdkColor *proposed)
{
	PangoColor *color;

	color = &terminal->pvt->palette[entry];

	if (color->red == proposed->red &&
	    color->green == proposed->green &&
	    color->blue == proposed->blue) {
		return;
	}

	_vte_debug_print(VTE_DEBUG_MISC,
			"Set color[%d] to (%04x,%04x,%04x).\n", entry,
			proposed->red, proposed->green, proposed->blue);

	/* Save the requested color. */
	color->red = proposed->red;
	color->green = proposed->green;
	color->blue = proposed->blue;

	/* If we're not realized yet, there's nothing else to do. */
	if (! gtk_widget_get_realized (&terminal->widget)) {
		return;
	}

	/* If we're setting the background color, set the background color
	 * on the widget as well. */
	if (entry == VTE_DEF_BG) {
		vte_terminal_queue_background_update(terminal);
	}

	/* and redraw */
	if (entry == VTE_CUR_BG)
		_vte_invalidate_cursor_once(terminal, FALSE);
	else
		_vte_invalidate_all (terminal);
}

static void
vte_terminal_generate_bold(const PangoColor *foreground,
			   const PangoColor *background,
			   double factor,
			   GdkColor *bold)
{
	double fy, fcb, fcr, by, bcb, bcr, r, g, b;
	g_assert(foreground != NULL);
	g_assert(background != NULL);
	g_assert(bold != NULL);
	fy =   0.2990 * foreground->red +
	       0.5870 * foreground->green +
	       0.1140 * foreground->blue;
	fcb = -0.1687 * foreground->red +
	      -0.3313 * foreground->green +
	       0.5000 * foreground->blue;
	fcr =  0.5000 * foreground->red +
	      -0.4187 * foreground->green +
	      -0.0813 * foreground->blue;
	by =   0.2990 * background->red +
	       0.5870 * background->green +
	       0.1140 * background->blue;
	bcb = -0.1687 * background->red +
	      -0.3313 * background->green +
	       0.5000 * background->blue;
	bcr =  0.5000 * background->red +
	      -0.4187 * background->green +
	      -0.0813 * background->blue;
	fy = (factor * fy) + ((1.0 - factor) * by);
	fcb = (factor * fcb) + ((1.0 - factor) * bcb);
	fcr = (factor * fcr) + ((1.0 - factor) * bcr);
	r = fy + 1.402 * fcr;
	g = fy + 0.34414 * fcb - 0.71414 * fcr;
	b = fy + 1.722 * fcb;
	_vte_debug_print(VTE_DEBUG_MISC,
			"Calculated bold (%d, %d, %d) = (%lf,%lf,%lf)",
			foreground->red, foreground->green, foreground->blue,
			r, g, b);
	bold->pixel = 0;
	bold->red = CLAMP(r, 0, 0xffff);
	bold->green = CLAMP(g, 0, 0xffff);
	bold->blue = CLAMP(b, 0, 0xffff);
	_vte_debug_print(VTE_DEBUG_MISC,
			"= (%04x,%04x,%04x).\n",
			bold->red, bold->green, bold->blue);
}

/**
 * vte_terminal_set_color_bold:
 * @terminal: a #VteTerminal
 * @bold: the new bold color
 *
 * Sets the color used to draw bold text in the default foreground color.
 */
void
vte_terminal_set_color_bold(VteTerminal *terminal, const GdkColor *bold)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(bold != NULL);

	_vte_debug_print(VTE_DEBUG_MISC,
			"Set bold color to (%04x,%04x,%04x).\n",
			bold->red, bold->green, bold->blue);
	vte_terminal_set_color_internal(terminal, VTE_BOLD_FG, bold);
}

/**
 * vte_terminal_set_color_dim:
 * @terminal: a #VteTerminal
 * @dim: the new dim color
 *
 * Sets the color used to draw dim text in the default foreground color.
 */
void
vte_terminal_set_color_dim(VteTerminal *terminal, const GdkColor *dim)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(dim != NULL);

	_vte_debug_print(VTE_DEBUG_MISC,
			"Set dim color to (%04x,%04x,%04x).\n",
			dim->red, dim->green, dim->blue);
	vte_terminal_set_color_internal(terminal, VTE_DIM_FG, dim);
}

/**
 * vte_terminal_set_color_foreground:
 * @terminal: a #VteTerminal
 * @foreground: the new foreground color
 *
 * Sets the foreground color used to draw normal text
 */
void
vte_terminal_set_color_foreground(VteTerminal *terminal,
				  const GdkColor *foreground)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(foreground != NULL);

	_vte_debug_print(VTE_DEBUG_MISC,
			"Set foreground color to (%04x,%04x,%04x).\n",
			foreground->red, foreground->green, foreground->blue);
	vte_terminal_set_color_internal(terminal, VTE_DEF_FG, foreground);
}

/**
 * vte_terminal_set_color_background:
 * @terminal: a #VteTerminal
 * @background: the new background color
 *
 * Sets the background color for text which does not have a specific background
 * color assigned.  Only has effect when no background image is set and when
 * the terminal is not transparent.
 */
void
vte_terminal_set_color_background(VteTerminal *terminal,
				  const GdkColor *background)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(background != NULL);

	_vte_debug_print(VTE_DEBUG_MISC,
			"Set background color to (%04x,%04x,%04x).\n",
			background->red, background->green, background->blue);
	vte_terminal_set_color_internal(terminal, VTE_DEF_BG, background);
}

/**
 * vte_terminal_set_color_cursor:
 * @terminal: a #VteTerminal
 * @cursor_background: (allow-none): the new color to use for the text cursor, or %NULL
 *
 * Sets the background color for text which is under the cursor.  If %NULL, text
 * under the cursor will be drawn with foreground and background colors
 * reversed.
 *
 * Since: 0.11.11
 */
void
vte_terminal_set_color_cursor(VteTerminal *terminal,
			      const GdkColor *cursor_background)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	if (cursor_background != NULL) {
		_vte_debug_print(VTE_DEBUG_MISC,
				"Set cursor color to (%04x,%04x,%04x).\n",
				cursor_background->red,
				cursor_background->green,
				cursor_background->blue);
		vte_terminal_set_color_internal(terminal, VTE_CUR_BG,
						cursor_background);
		terminal->pvt->cursor_color_set = TRUE;
	} else {
		_vte_debug_print(VTE_DEBUG_MISC,
				"Cleared cursor color.\n");
		terminal->pvt->cursor_color_set = FALSE;
	}
}

/**
 * vte_terminal_set_color_highlight:
 * @terminal: a #VteTerminal
 * @highlight_background: (allow-none): the new color to use for highlighted text, or %NULL
 *
 * Sets the background color for text which is highlighted.  If %NULL,
 * highlighted text (which is usually highlighted because it is selected) will
 * be drawn with foreground and background colors reversed.
 *
 * Since: 0.11.11
 */
void
vte_terminal_set_color_highlight(VteTerminal *terminal,
				 const GdkColor *highlight_background)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	if (highlight_background != NULL) {
		_vte_debug_print(VTE_DEBUG_MISC,
				"Set highlight color to (%04x,%04x,%04x).\n",
				highlight_background->red,
				highlight_background->green,
				highlight_background->blue);
		vte_terminal_set_color_internal(terminal, VTE_DEF_HL,
						highlight_background);
		terminal->pvt->highlight_color_set = TRUE;
	} else {
		_vte_debug_print(VTE_DEBUG_MISC,
				"Cleared highlight color.\n");
		terminal->pvt->highlight_color_set = FALSE;
	}
}

/**
 * vte_terminal_set_colors:
 * @terminal: a #VteTerminal
 * @foreground: (allow-none): the new foreground color, or %NULL
 * @background: (allow-none): the new background color, or %NULL
 * @palette: (array length=palette_size zero-terminated=0) (element-type Gdk.Color): the color palette
 * @palette_size: the number of entries in @palette
 *
 * The terminal widget uses a 28-color model comprised of the default foreground
 * and background colors, the bold foreground color, the dim foreground
 * color, an eight color palette, bold versions of the eight color palette,
 * and a dim version of the the eight color palette.
 *
 * @palette_size must be either 0, 8, 16, or 24, or between 25 and 255 inclusive.
 * If @foreground is %NULL and
 * @palette_size is greater than 0, the new foreground color is taken from
 * @palette[7].  If @background is %NULL and @palette_size is greater than 0,
 * the new background color is taken from @palette[0].  If
 * @palette_size is 8 or 16, the third (dim) and possibly the second (bold)
 * 8-color palettes are extrapolated from the new background color and the items
 * in @palette.
 */
void
vte_terminal_set_colors(VteTerminal *terminal,
			const GdkColor *foreground,
			const GdkColor *background,
			const GdkColor *palette,
			glong palette_size)
{
	guint i;
	GdkColor color;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	g_return_if_fail(palette_size >= 0);
	g_return_if_fail((palette_size == 0) ||
			 (palette_size == 8) ||
			 (palette_size == 16) ||
			 (palette_size == 24) ||
			 (palette_size > 24 && palette_size < 256));

	_vte_debug_print(VTE_DEBUG_MISC,
			"Set color palette [%ld elements].\n",
			palette_size);

	/* Accept NULL as the default foreground and background colors if we
	 * got a palette. */
	if ((foreground == NULL) && (palette_size >= 8)) {
		foreground = &palette[7];
	}
	if ((background == NULL) && (palette_size >= 8)) {
		background = &palette[0];
	}

	memset(&color, 0, sizeof(color));

	/* Initialize each item in the palette if we got any entries to work
	 * with. */
	for (i=0; i < G_N_ELEMENTS(terminal->pvt->palette); i++) {
		if (i < 16) {
			color.blue = (i & 4) ? 0xc000 : 0;
			color.green = (i & 2) ? 0xc000 : 0;
			color.red = (i & 1) ? 0xc000 : 0;
			if (i > 7) {
				color.blue += 0x3fff;
				color.green += 0x3fff;
				color.red += 0x3fff;
			}
		}
		else if (i < 232) {
			int j = i - 16;
			int r = j / 36, g = (j / 6) % 6, b = j % 6;
			int red =   (r == 0) ? 0 : r * 40 + 55;
			int green = (g == 0) ? 0 : g * 40 + 55;
			int blue =  (b == 0) ? 0 : b * 40 + 55;
			color.red   = red | red << 8  ;
			color.green = green | green << 8;
			color.blue  = blue | blue << 8;
		} else if (i < 256) {
			int shade = 8 + (i - 232) * 10;
			color.red = color.green = color.blue = shade | shade << 8;
		}
		else switch (i) {
			case VTE_DEF_BG:
				if (background != NULL) {
					color = *background;
				} else {
					color.red = 0;
					color.blue = 0;
					color.green = 0;
				}
				break;
			case VTE_DEF_FG:
				if (foreground != NULL) {
					color = *foreground;
				} else {
					color.red = 0xc000;
					color.blue = 0xc000;
					color.green = 0xc000;
				}
				break;
			case VTE_BOLD_FG:
				vte_terminal_generate_bold(&terminal->pvt->palette[VTE_DEF_FG],
							   &terminal->pvt->palette[VTE_DEF_BG],
							   1.8,
							   &color);
				break;
			case VTE_DIM_FG:
				vte_terminal_generate_bold(&terminal->pvt->palette[VTE_DEF_FG],
							   &terminal->pvt->palette[VTE_DEF_BG],
							   0.5,
							   &color);
				break;
			case VTE_DEF_HL:
				color.red = 0xc000;
				color.blue = 0xc000;
				color.green = 0xc000;
				break;
			case VTE_CUR_BG:
				color.red = 0x0000;
				color.blue = 0x0000;
				color.green = 0x0000;
				break;
			}

		/* Override from the supplied palette if there is one. */
		if ((glong) i < palette_size) {
			color = palette[i];
		}

		/* Set up the color entry. */
		vte_terminal_set_color_internal(terminal, i, &color);
	}

	/* Track that we had a color palette set. */
	terminal->pvt->palette_initialized = TRUE;
}

#if GTK_CHECK_VERSION (2, 99, 0)

static GdkColor *
gdk_color_from_rgba (GdkColor *color,
                     const GdkRGBA *rgba)
{
        if (rgba == NULL)
                return NULL;

        color->red = rgba->red * 65535.;
        color->green = rgba->green * 65535.;
        color->blue = rgba->blue * 65535.;
        color->pixel = 0;

	return color;
}

/**
 * vte_terminal_set_color_bold_rgba:
 * @terminal: a #VteTerminal
 * @bold: (allow-none): the new bold color or %NULL
 *
 * Sets the color used to draw bold text in the default foreground color.
 * If @bold is %NULL then the default color is used.
 */
void
vte_terminal_set_color_bold_rgba(VteTerminal *terminal,
                                 const GdkRGBA *bold)
{
	GdkColor color;

	if (bold == NULL)
	{
		vte_terminal_generate_bold(&terminal->pvt->palette[VTE_DEF_FG],
					   &terminal->pvt->palette[VTE_DEF_BG],
					   1.8,
					   &color);
	}
	else
	{
		gdk_color_from_rgba(&color, bold);
	}

	vte_terminal_set_color_bold(terminal, &color);
}

/**
 * vte_terminal_set_color_dim_rgba:
 * @terminal: a #VteTerminal
 * @dim: (allow-none): the new dim color or %NULL
 *
 * Sets the color used to draw dim text in the default foreground color.
 * If @dim is %NULL then the default color is used.
 *
 * Since: 0.28
 */
void
vte_terminal_set_color_dim_rgba(VteTerminal *terminal,
                                const GdkRGBA *dim)
{
	GdkColor color;

	if (dim == NULL)
	{
		vte_terminal_generate_bold(&terminal->pvt->palette[VTE_DEF_FG],
					   &terminal->pvt->palette[VTE_DEF_BG],
					   0.5,
					   &color);
	}
	else
	{
		gdk_color_from_rgba(&color, dim);
	}

	vte_terminal_set_color_dim(terminal, &color);
}

/**
 * vte_terminal_set_color_foreground_rgba:
 * @terminal: a #VteTerminal
 * @foreground: the new foreground color
 *
 * Sets the foreground color used to draw normal text.
 *
 * Since: 0.28
 */
void
vte_terminal_set_color_foreground_rgba(VteTerminal *terminal,
				       const GdkRGBA *foreground)
{
	GdkColor color;

	vte_terminal_set_color_foreground(terminal,
                                          gdk_color_from_rgba(&color, foreground));
}

/**
 * vte_terminal_set_color_background_rgba:
 * @terminal: a #VteTerminal
 * @background: the new background color
 *
 * Sets the background color for text which does not have a specific background
 * color assigned.  Only has effect when no background image is set and when
 * the terminal is not transparent.
 *
 * Since: 0.28
 */
void
vte_terminal_set_color_background_rgba(VteTerminal *terminal,
				       const GdkRGBA *background)
{
	GdkColor color;

	vte_terminal_set_color_background(terminal,
                                          gdk_color_from_rgba (&color, background));
}

/**
 * vte_terminal_set_color_cursor_rgba:
 * @terminal: a #VteTerminal
 * @cursor_background: (allow-none): the new color to use for the text cursor, or %NULL
 *
 * Sets the background color for text which is under the cursor.  If %NULL, text
 * under the cursor will be drawn with foreground and background colors
 * reversed.
 *
 * Since: 0.28
 */
void
vte_terminal_set_color_cursor_rgba(VteTerminal *terminal,
				   const GdkRGBA *cursor_background)
{
        GdkColor color;

	vte_terminal_set_color_cursor(terminal,
                                      gdk_color_from_rgba(&color, cursor_background));
}

/**
 * vte_terminal_set_color_highlight_rgba:
 * @terminal: a #VteTerminal
 * @highlight_background: (allow-none): the new color to use for highlighted text, or %NULL
 *
 * Sets the background color for text which is highlighted.  If %NULL,
 * highlighted text (which is usually highlighted because it is selected) will
 * be drawn with foreground and background colors reversed.
 *
 * Since: 0.28
 */
void
vte_terminal_set_color_highlight_rgba(VteTerminal *terminal,
				      const GdkRGBA *highlight_background)
{
	GdkColor color;

	vte_terminal_set_color_highlight(terminal,
                                         gdk_color_from_rgba(&color, highlight_background));
}

/**
 * vte_terminal_set_colors_rgba:
 * @terminal: a #VteTerminal
 * @foreground: (allow-none): the new foreground color, or %NULL
 * @background: (allow-none): the new background color, or %NULL
 * @palette: (array length=palette_size zero-terminated=0) (element-type Gdk.RGBA): the color palette
 * @palette_size: the number of entries in @palette
 *
 * The terminal widget uses a 28-color model comprised of the default foreground
 * and background colors, the bold foreground color, the dim foreground
 * color, an eight color palette, bold versions of the eight color palette,
 * and a dim version of the the eight color palette.
 *
 * @palette_size must be either 0, 8, 16, or 24, or between 25 and 255 inclusive.
 * If @foreground is %NULL and
 * @palette_size is greater than 0, the new foreground color is taken from
 * @palette[7].  If @background is %NULL and @palette_size is greater than 0,
 * the new background color is taken from @palette[0].  If
 * @palette_size is 8 or 16, the third (dim) and possibly the second (bold)
 * 8-color palettes are extrapolated from the new background color and the items
 * in @palette.
 *
 * Since: 0.28
 */
void
vte_terminal_set_colors_rgba(VteTerminal *terminal,
			     const GdkRGBA *foreground,
			     const GdkRGBA *background,
			     const GdkRGBA *palette,
			     gsize palette_size)
{
	GdkColor fg, bg, *pal;
	gsize i;

	pal = g_new (GdkColor, palette_size);
	for (i = 0; i < palette_size; ++i)
                gdk_color_from_rgba(&pal[i], &palette[i]);

	vte_terminal_set_colors(terminal,
                                gdk_color_from_rgba(&fg, foreground),
                                gdk_color_from_rgba(&bg, background),
	                        pal, palette_size);

	g_free (pal);
}

#endif /* GTK 3.0 */

/**
 * vte_terminal_set_opacity:
 * @terminal: a #VteTerminal
 * @opacity: the new opacity
 *
 * Sets the opacity of the terminal background, were 0 means completely
 * transparent and 65535 means completely opaque.
 */
void
vte_terminal_set_opacity(VteTerminal *terminal, guint16 opacity)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        if (opacity == pvt->bg_opacity)
                return;

	pvt->bg_opacity = opacity;

        g_object_notify(G_OBJECT(terminal), "background-opacity");
}

/**
 * vte_terminal_set_default_colors:
 * @terminal: a #VteTerminal
 *
 * Reset the terminal palette to reasonable compiled-in default color.
 */
void
vte_terminal_set_default_colors(VteTerminal *terminal)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	vte_terminal_set_colors(terminal, NULL, NULL, NULL, 0);
}


/* Cleanup smart-tabs.  See vte_sequence_handler_ta() */
void
_vte_terminal_cleanup_tab_fragments_at_cursor (VteTerminal *terminal)
{
	VteRowData *row = _vte_terminal_ensure_row (terminal);
	VteScreen *screen = terminal->pvt->screen;
	long col = screen->cursor_current.col;
	const VteCell *pcell = _vte_row_data_get (row, col);

	if (G_UNLIKELY (pcell != NULL && pcell->c == '\t')) {
		long i, num_columns;
		VteCell *cell = _vte_row_data_get_writable (row, col);
		
		_vte_debug_print(VTE_DEBUG_MISC,
				 "Cleaning tab fragments at %ld",
				 col);

		/* go back to the beginning of the tab */
		while (cell->attr.fragment && col > 0)
			cell = _vte_row_data_get_writable (row, --col);

		num_columns = cell->attr.columns;
		for (i = 0; i < num_columns; i++) {
			cell = _vte_row_data_get_writable (row, col++);
			if (G_UNLIKELY (!cell))
			  break;
			*cell = screen->fill_defaults;
		}
	}
}

/* Cursor down, with scrolling. */
void
_vte_terminal_cursor_down (VteTerminal *terminal)
{
	long start, end;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	if (screen->scrolling_restricted) {
		start = screen->insert_delta + screen->scrolling_region.start;
		end = screen->insert_delta + screen->scrolling_region.end;
	} else {
		start = screen->insert_delta;
		end = start + terminal->row_count - 1;
	}
	if (screen->cursor_current.row == end) {
		/* Match xterm and fill to the end of row when scrolling. */
		if (screen->fill_defaults.attr.back != VTE_DEF_BG) {
			VteRowData *rowdata;
			rowdata = _vte_terminal_ensure_row (terminal);
			_vte_row_data_fill (rowdata, &screen->fill_defaults, terminal->column_count);
		}

		if (screen->scrolling_restricted) {
			if (start == screen->insert_delta) {
				/* Scroll this line into the scrollback
				 * buffer by inserting a line at the next
				 * line and scrolling the area up. */
				screen->insert_delta++;
				screen->scroll_delta++;
				screen->cursor_current.row++;
				/* update start and end, as they are relative
				 * to insert_delta. */
				start++;
				end++;
				_vte_terminal_ring_insert (terminal, screen->cursor_current.row, FALSE);
				/* Force the areas below the region to be
				 * redrawn -- they've moved. */
				_vte_terminal_scroll_region(terminal, start,
							    end - start + 1, 1);
				/* Force scroll. */
				_vte_terminal_adjust_adjustments(terminal);
			} else {
				/* If we're at the bottom of the scrolling
				 * region, add a line at the top to scroll the
				 * bottom off. */
				_vte_terminal_ring_remove (terminal, start);
				_vte_terminal_ring_insert (terminal, end, TRUE);
				/* Update the display. */
				_vte_terminal_scroll_region(terminal, start,
							   end - start + 1, -1);
				_vte_invalidate_cells(terminal,
						      0, terminal->column_count,
						      end - 2, 2);
			}
		} else {
			/* Scroll up with history. */
			screen->cursor_current.row++;
			_vte_terminal_update_insert_delta(terminal);
		}

		/* Match xterm and fill the new row when scrolling. */
		if (screen->fill_defaults.attr.back != VTE_DEF_BG) {
			VteRowData *rowdata;
			rowdata = _vte_terminal_ensure_row (terminal);
			_vte_row_data_fill (rowdata, &screen->fill_defaults, terminal->column_count);
		}
	} else {
		/* Otherwise, just move the cursor down. */
		screen->cursor_current.row++;
	}
}

/* Insert a single character into the stored data array. */
gboolean
_vte_terminal_insert_char(VteTerminal *terminal, gunichar c,
			 gboolean insert, gboolean invalidate_now)
{
	VteCellAttr attr;
	VteRowData *row;
	long col;
	int columns, i;
	VteScreen *screen;
	gboolean line_wrapped = FALSE; /* cursor moved before char inserted */

	screen = terminal->pvt->screen;
	insert |= screen->insert_mode;
	invalidate_now |= insert;

	/* If we've enabled the special drawing set, map the characters to
	 * Unicode. */
	if (G_UNLIKELY (screen->alternate_charset)) {
		_vte_debug_print(VTE_DEBUG_SUBSTITUTION,
				"Attempting charset substitution"
				"for U+%04X.\n", c);
		/* See if there's a mapping for it. */
		c = _vte_iso2022_process_single(terminal->pvt->iso2022, c, '0');
	}

	/* If this character is destined for the status line, save it. */
	if (G_UNLIKELY (screen->status_line)) {
		g_string_append_unichar(screen->status_line_contents, c);
		screen->status_line_changed = TRUE;
		return FALSE;
	}

	/* Figure out how many columns this character should occupy. */
	if (G_UNLIKELY (VTE_ISO2022_HAS_ENCODED_WIDTH(c))) {
		columns = _vte_iso2022_get_encoded_width(c);
		c &= ~VTE_ISO2022_ENCODED_WIDTH_MASK;
	} else {
		columns = _vte_iso2022_unichar_width(terminal->pvt->iso2022, c);
	}


	/* If we're autowrapping here, do it. */
	col = screen->cursor_current.col;
	if (G_UNLIKELY (columns && col + columns > terminal->column_count)) {
		if (terminal->pvt->flags.am) {
			_vte_debug_print(VTE_DEBUG_ADJ,
					"Autowrapping before character\n");
			/* Wrap. */
			/* XXX clear to the end of line */
			col = screen->cursor_current.col = 0;
			/* Mark this line as soft-wrapped. */
			row = _vte_terminal_ensure_row (terminal);
			row->attr.soft_wrapped = 1;
			_vte_terminal_cursor_down (terminal);
		} else {
			/* Don't wrap, stay at the rightmost column. */
			col = screen->cursor_current.col =
				terminal->column_count - columns;
		}
		line_wrapped = TRUE;
	}

	_vte_debug_print(VTE_DEBUG_PARSE,
			"Inserting %ld '%c' (%d/%d) (%ld+%d, %ld), delta = %ld; ",
			(long)c, c < 256 ? c : ' ',
			screen->defaults.attr.fore,
			screen->defaults.attr.back,
			col, columns, (long)screen->cursor_current.row,
			(long)screen->insert_delta);


	if (G_UNLIKELY (columns == 0)) {

		/* It's a combining mark */

		long row_num;
		VteCell *cell;

		_vte_debug_print(VTE_DEBUG_PARSE, "combining U+%04X", c);

		row_num = screen->cursor_current.row;
		row = NULL;
		if (G_UNLIKELY (col == 0)) {
			/* We are at first column.  See if the previous line softwrapped.
			 * If it did, move there.  Otherwise skip inserting. */

			if (G_LIKELY (row_num > 0)) {
				row_num--;
				row = _vte_terminal_find_row_data_writable (terminal, row_num);

				if (row) {
					if (!row->attr.soft_wrapped)
						row = NULL;
					else
						col = _vte_row_data_length (row);
				}
			}
		} else {
			row = _vte_terminal_find_row_data_writable (terminal, row_num);
		}

		if (G_UNLIKELY (!row || !col))
			goto not_inserted;

		/* Combine it on the previous cell */

		col--;
		cell = _vte_row_data_get_writable (row, col);

		if (G_UNLIKELY (!cell))
			goto not_inserted;

		/* Find the previous cell */
		while (cell && cell->attr.fragment && col > 0)
			cell = _vte_row_data_get_writable (row, --col);
		if (G_UNLIKELY (!cell || cell->c == '\t'))
			goto not_inserted;

		/* Combine the new character on top of the cell string */
		c = _vte_unistr_append_unichar (cell->c, c);

		/* And set it */
		columns = cell->attr.columns;
		for (i = 0; i < columns; i++) {
			cell = _vte_row_data_get_writable (row, col++);
			cell->c = c;
		}

		/* Always invalidate since we put the mark on the *previous* cell
		 * and the higher level code doesn't know this. */
		_vte_invalidate_cells(terminal,
				      col - columns,
				      columns,
				      row_num, 1);

		goto done;
	}

	/* Make sure we have enough rows to hold this data. */
	row = vte_terminal_ensure_cursor (terminal);
	g_assert(row != NULL);

	_vte_terminal_cleanup_tab_fragments_at_cursor (terminal);

	if (insert) {
		for (i = 0; i < columns; i++)
			_vte_row_data_insert (row, col + i, &screen->color_defaults);
	} else {
		_vte_row_data_fill (row, &basic_cell.cell, col + columns);
	}

	/* Convert any wide characters we may have broken into single
	 * cells. (#514632) */
	if (G_LIKELY (col > 0)) {
		glong col2 = col - 1;
		VteCell *cell = _vte_row_data_get_writable (row, col2);
		while (col2 > 0 && cell != NULL && cell->attr.fragment)
			cell = _vte_row_data_get_writable (row, --col2);
		cell->attr.columns = col - col2;
	}
	{
		glong col2 = col + columns;
		VteCell *cell = _vte_row_data_get_writable (row, col2);
		while (cell != NULL && cell->attr.fragment) {
			cell->attr.columns = 1;
			cell->c = 0;
			cell = _vte_row_data_get_writable (row, ++col2);
		}
	}

	attr = screen->defaults.attr;
	attr.columns = columns;

	if (G_UNLIKELY (c == '_' && terminal->pvt->flags.ul)) {
		const VteCell *pcell = _vte_row_data_get (row, col);
		/* Handle overstrike-style underlining. */
		if (pcell->c != 0) {
			/* restore previous contents */
			c = pcell->c;
			attr.columns = pcell->attr.columns;
			attr.fragment = pcell->attr.fragment;

			attr.underline = 1;
		}
	}


	{
		VteCell *pcell = _vte_row_data_get_writable (row, col);
		pcell->c = c;
		pcell->attr = attr;
		col++;
	}

	/* insert wide-char fragments */
	attr.fragment = 1;
	for (i = 1; i < columns; i++) {
		VteCell *pcell = _vte_row_data_get_writable (row, col);
		pcell->c = c;
		pcell->attr = attr;
		col++;
	}
	_vte_row_data_shrink (row, terminal->column_count);

	/* Signal that this part of the window needs drawing. */
	if (G_UNLIKELY (invalidate_now)) {
		_vte_invalidate_cells(terminal,
				col - columns,
				insert ? terminal->column_count : columns,
				screen->cursor_current.row, 1);
	}


	/* If we're autowrapping *here*, do it. */
	screen->cursor_current.col = col;
	if (G_UNLIKELY (col >= terminal->column_count)) {
		if (terminal->pvt->flags.am && !terminal->pvt->flags.xn) {
			/* Wrap. */
			screen->cursor_current.col = 0;
			/* Mark this line as soft-wrapped. */
			row->attr.soft_wrapped = 1;
			_vte_terminal_cursor_down (terminal);
		}
	}

done:
	/* We added text, so make a note of it. */
	terminal->pvt->text_inserted_flag = TRUE;

not_inserted:
	_vte_debug_print(VTE_DEBUG_ADJ|VTE_DEBUG_PARSE,
			"insertion delta => %ld.\n",
			(long)screen->insert_delta);
	return line_wrapped;
}

/* Catch a VteReaper child-exited signal, and if it matches the one we're
 * looking for, emit one of our own. */
static void
vte_terminal_catch_child_exited(VteReaper *reaper, int pid, int status,
				VteTerminal *terminal)
{
	if (pid == terminal->pvt->pty_pid) {
                GObject *object = G_OBJECT(terminal);

                g_object_ref(object);
                g_object_freeze_notify(object);

		_VTE_DEBUG_IF (VTE_DEBUG_LIFECYCLE) {
			g_printerr ("Child[%d] exited with status %d\n",
					pid, status);
#ifdef HAVE_SYS_WAIT_H
			if (WIFEXITED (status)) {
				g_printerr ("Child[%d] exit code %d.\n",
						pid, WEXITSTATUS (status));
			}else if (WIFSIGNALED (status)) {
				g_printerr ("Child[%d] dies with signal %d.\n",
						pid, WTERMSIG (status));
			}
#endif
		}
		/* Disconnect from the reaper. */
		if (terminal->pvt->pty_reaper != NULL) {
			g_signal_handlers_disconnect_by_func(terminal->pvt->pty_reaper,
							     vte_terminal_catch_child_exited,
							     terminal);
			g_object_unref(terminal->pvt->pty_reaper);
			terminal->pvt->pty_reaper = NULL;
		}
		terminal->pvt->pty_pid = -1;

		/* Close out the PTY. */
                vte_terminal_set_pty_object(terminal, NULL);

		/* Tell observers what's happened. */
                terminal->pvt->child_exit_status = status;
		vte_terminal_emit_child_exited(terminal);

                g_object_thaw_notify(object);
                g_object_unref(object);

                /* Note: terminal may be destroyed at this point */
	}
}

static void mark_input_source_invalid(VteTerminal *terminal)
{
	_vte_debug_print (VTE_DEBUG_IO, "removed poll of vte_terminal_io_read\n");
	terminal->pvt->pty_input_source = 0;
}
static void
_vte_terminal_connect_pty_read(VteTerminal *terminal)
{
	if (terminal->pvt->pty_channel == NULL) {
		return;
	}

	if (terminal->pvt->pty_input_source == 0) {
		_vte_debug_print (VTE_DEBUG_IO, "polling vte_terminal_io_read\n");
		terminal->pvt->pty_input_source =
			g_io_add_watch_full(terminal->pvt->pty_channel,
					    VTE_CHILD_INPUT_PRIORITY,
					    G_IO_IN | G_IO_HUP,
					    (GIOFunc) vte_terminal_io_read,
					    terminal,
					    (GDestroyNotify) mark_input_source_invalid);
	}
}

static void mark_output_source_invalid(VteTerminal *terminal)
{
	_vte_debug_print (VTE_DEBUG_IO, "removed poll of vte_terminal_io_write\n");
	terminal->pvt->pty_output_source = 0;
}
static void
_vte_terminal_connect_pty_write(VteTerminal *terminal)
{
        VteTerminalPrivate *pvt = terminal->pvt;

        g_assert(pvt->pty != NULL);
	if (terminal->pvt->pty_channel == NULL) {
		pvt->pty_channel =
			g_io_channel_unix_new(vte_pty_get_fd(pvt->pty));
	}

	if (terminal->pvt->pty_output_source == 0) {
		if (vte_terminal_io_write (terminal->pvt->pty_channel,
					     G_IO_OUT,
					     terminal))
		{
			_vte_debug_print (VTE_DEBUG_IO, "polling vte_terminal_io_write\n");
			terminal->pvt->pty_output_source =
				g_io_add_watch_full(terminal->pvt->pty_channel,
						    VTE_CHILD_OUTPUT_PRIORITY,
						    G_IO_OUT,
						    (GIOFunc) vte_terminal_io_write,
						    terminal,
						    (GDestroyNotify) mark_output_source_invalid);
		}
	}
}

static void
_vte_terminal_disconnect_pty_read(VteTerminal *terminal)
{
	if (terminal->pvt->pty_input_source != 0) {
		_vte_debug_print (VTE_DEBUG_IO, "disconnecting poll of vte_terminal_io_read\n");
		g_source_remove(terminal->pvt->pty_input_source);
		terminal->pvt->pty_input_source = 0;
	}
}

static void
_vte_terminal_disconnect_pty_write(VteTerminal *terminal)
{
	if (terminal->pvt->pty_output_source != 0) {
		_vte_debug_print (VTE_DEBUG_IO, "disconnecting poll of vte_terminal_io_write\n");

		g_source_remove(terminal->pvt->pty_output_source);
		terminal->pvt->pty_output_source = 0;
	}
}

/**
 * vte_terminal_pty_new:
 * @terminal: a #VteTerminal
 * @flags: flags from #VtePtyFlags
 * @error: (allow-none): return location for a #GError, or %NULL
 *
 * Creates a new #VtePty, and sets the emulation property
 * from #VteTerminal:emulation.
 *
 * See vte_pty_new() for more information.
 *
 * Returns: (transfer full): a new #VtePty
 * Since: 0.26
 */
VtePty *
vte_terminal_pty_new(VteTerminal *terminal,
                     VtePtyFlags flags,
                     GError **error)
{
        VteTerminalPrivate *pvt;
        VtePty *pty;

        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);

        pvt = terminal->pvt;

        pty = vte_pty_new(flags, error);
        if (pty == NULL)
                return NULL;

        vte_pty_set_term(pty, vte_terminal_get_emulation(terminal));

        return pty;
}

/**
 * vte_terminal_watch_child:
 * @terminal: a #VteTerminal
 * @child_pid: a #GPid
 *
 * Watches @child_pid. When the process exists, the #VteReaper::child-exited
 * signal will be called. Use vte_terminal_get_child_exit_status() to
 * retrieve the child's exit status.
 *
 * Prior to calling this function, a #VtePty must have been set in @terminal
 * using vte_terminal_set_pty_object().
 * When the child exits, the terminal's #VtePty will be set to %NULL.
 *
 * Note: g_child_watch_add() or g_child_watch_add_full() must not have
 * been called for @child_pid, nor a #GSource for it been created with
 * g_child_watch_source_new().
 *
 * Note: when using the g_spawn_async() family of functions,
 * the %G_SPAWN_DO_NOT_REAP_CHILD flag MUST have been passed.
 *
 * Since: 0.26
 */
void
vte_terminal_watch_child (VteTerminal *terminal,
                          GPid child_pid)
{
        VteTerminalPrivate *pvt;
        GObject *object;
        VteReaper *reaper;

        g_return_if_fail(VTE_IS_TERMINAL(terminal));
        g_return_if_fail(child_pid != -1);

        pvt = terminal->pvt;
        g_return_if_fail(pvt->pty != NULL);

        // FIXMEchpe: support passing child_pid = -1 to remove the wathch

        object = G_OBJECT(terminal);

        g_object_freeze_notify(object);

        /* Set this as the child's pid. */
        pvt->pty_pid = child_pid;
        pvt->child_exit_status = 0;

        /* Catch a child-exited signal from the child pid. */
        reaper = vte_reaper_get();
        vte_reaper_add_child(child_pid);
        if (reaper != pvt->pty_reaper) {
                if (terminal->pvt->pty_reaper != NULL) {
                        g_signal_handlers_disconnect_by_func(pvt->pty_reaper,
                                        vte_terminal_catch_child_exited,
                                        terminal);
                        g_object_unref(pvt->pty_reaper);
                }
                g_signal_connect(reaper, "child-exited",
                                G_CALLBACK(vte_terminal_catch_child_exited),
                                terminal);
                pvt->pty_reaper = reaper;
        } else {
                g_object_unref(reaper);
	}

        /* FIXMEchpe: call vte_terminal_set_size here? */

        g_object_thaw_notify(object);
}

/**
 * vte_get_user_shell:
 *
 * Gets the user's shell, or %NULL. In the latter case, the
 * system default (usually "/bin/sh") should be used.
 *
 * Returns: (tranfer full) (type filename): a newly allocated string with the
 *   user's shell, or %NULL
 *
 * Since: 0.28
 */
char *
vte_get_user_shell (void)
{
	struct passwd *pwd;

	pwd = getpwuid(getuid());
        if (pwd && pwd->pw_shell)
                return g_strdup (pwd->pw_shell);

        return NULL;
}

static char *
_vte_terminal_get_user_shell_with_fallback (void)
{
        char *command;
        const gchar *env;

        if ((command = vte_get_user_shell ()))
                return command;

        if ((env = g_getenv ("SHELL")))
                return g_strdup (env);

        return g_strdup ("/bin/sh");
}

/*
 * _vte_terminal_get_argv:
 * @command: the command to run
 * @argv: the argument vector
 * @flags: (inout) flags from #GSpawnFlags
 *
 * Creates an argument vector to pass to g_spawn_async() from @command and
 * @argv, modifying *@flags if necessary.
 * Like __vte_pty_get_argv(), but returns the argument vector to spawn
 * the user's shell if @command is %NULL.
 *
 * Returns: a newly allocated array of strings. Free with g_strfreev()
 */
static char **
_vte_terminal_get_argv (const char *command,
                        char **argv,
                        GSpawnFlags *flags /* inout */)
{
	char **argv2;
        char *shell = NULL;

        argv2 = __vte_pty_get_argv(command ? command : (shell = _vte_terminal_get_user_shell_with_fallback ()),
                                   argv,
                                   flags);
        g_free(shell);
        return argv2;
}

/**
 * vte_terminal_fork_command:
 * @terminal: a #VteTerminal
 * @command: (allow-none) (type filename): the name of a binary to run, or %NULL to spawn the user's shell
 * @argv: (allow-none) (array zero-terminated=1) (element-type filename): the argument list to be passed to @command, or %NULL
 * @envv: (allow-none) (array zero-terminated=1) (element-type filename): a list of environment variables to be added to the environment before
 *   starting @command, or %NULL
 * @working_directory: (allow-none) (type filename): the name of a directory the command should start in, or %NULL
 * @lastlog: %TRUE if the session should be logged to the lastlog
 * @utmp: %TRUE if the session should be logged to the utmp/utmpx log
 * @wtmp: %TRUE if the session should be logged to the wtmp/wtmpx log
 *
 * Starts the specified command under a newly-allocated controlling
 * pseudo-terminal.  The @argv and @envv lists should be %NULL-terminated, and
 * argv[0] is expected to be the name of the file being run, as it would be if
 * execve() were being called.  TERM is automatically set to reflect the
 * terminal widget's emulation setting.  If @lastlog, @utmp, or @wtmp are %TRUE,
 * logs the session to the specified system log files.
 *
 * Note that all file descriptors except stdin/stdout/stderr will be closed
 * before calling exec() in the child.
 *
 * Returns: the PID of the new process, or <literal>-1</literal> on failure
 *
 * Deprecated: 0.26: Use vte_terminal_fork_command_full()
 */
pid_t
vte_terminal_fork_command(VteTerminal *terminal,
			  const char *command,
                          char **argv,
                          char **envv,
			  const char *working_directory,
			  gboolean lastlog,
                          gboolean utmp,
                          gboolean wtmp)
{
        char **real_argv;
        GSpawnFlags spawn_flags;
        GPid child_pid;
        gboolean ret;
#ifdef VTE_DEBUG
        GError *error = NULL;
        GError **err = &error;
#else
        GError **err = NULL;
#endif

        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);

        spawn_flags = G_SPAWN_CHILD_INHERITS_STDIN |
                      G_SPAWN_SEARCH_PATH;
        real_argv = _vte_terminal_get_argv (command, argv, &spawn_flags);

        ret = vte_terminal_fork_command_full(terminal,
                                             __vte_pty_get_pty_flags(lastlog, utmp, wtmp),
                                             working_directory,
                                             real_argv,
                                             envv,
                                             spawn_flags,
                                             NULL, NULL,
                                             &child_pid,
                                             err);
        g_strfreev (real_argv);

#ifdef VTE_DEBUG
        if (error) {
                _vte_debug_print(VTE_DEBUG_MISC,
                                "vte_terminal_fork_command failed: %s\n", error->message);
                g_error_free(error);
        }
#endif

        if (!ret)
                return -1;

        return (pid_t) child_pid;
}

/**
 * vte_terminal_fork_command_full:
 * @terminal: a #VteTerminal
 * @pty_flags: flags from #VtePtyFlags
 * @working_directory: (allow-none): the name of a directory the command should start
 *   in, or %NULL to use the current working directory
 * @argv: (array zero-terminated=1) (element-type filename): child's argument vector
 * @envv: (allow-none) (array zero-terminated=1) (element-type filename): a list of environment
 *   variables to be added to the environment before starting the process, or %NULL
 * @spawn_flags: flags from #GSpawnFlags
 * @child_setup: (allow-none) (scope call): function to run in the child just before exec(), or %NULL
 * @child_setup_data: user data for @child_setup
 * @child_pid: (out) (allow-none) (transfer full): a location to store the child PID, or %NULL
 * @error: (allow-none): return location for a #GError, or %NULL
 *
 * Starts the specified command under a newly-allocated controlling
 * pseudo-terminal.  The @argv and @envv lists should be %NULL-terminated.
 * The "TERM" environment variable is automatically set to reflect the
 * terminal widget's emulation setting.
 * @pty_flags controls logging the session to the specified system log files.
 *
 * Note that %G_SPAWN_DO_NOT_REAP_CHILD will always be added to @spawn_flags.
 *
 * Note that unless @spawn_flags contains %G_SPAWN_LEAVE_DESCRIPTORS_OPEN, all file
 * descriptors except stdin/stdout/stderr will be closed before calling exec()
 * in the child.
 *
 * See vte_pty_new(), g_spawn_async() and vte_terminal_watch_child() for more information.
 *
 * Returns: %TRUE on success, or %FALSE on error with @error filled in
 *
 * Since: 0.26
 */
gboolean
vte_terminal_fork_command_full(VteTerminal *terminal,
                               VtePtyFlags pty_flags,
                               const char *working_directory,
                               char **argv,
                               char **envv,
                               GSpawnFlags spawn_flags,
                               GSpawnChildSetupFunc child_setup,
                               gpointer child_setup_data,
                               GPid *child_pid /* out */,
                               GError **error)
{
        VtePty *pty;
        GPid pid;

        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);
        g_return_val_if_fail(argv != NULL, FALSE);
        g_return_val_if_fail(child_setup_data == NULL || child_setup, FALSE);
        g_return_val_if_fail(error == NULL || *error == NULL, FALSE);

        pty = vte_pty_new(pty_flags, error);
        if (pty == NULL)
                return FALSE;

        /* FIXMEchpe: is this flag needed */
        spawn_flags |= G_SPAWN_CHILD_INHERITS_STDIN;

        if (!__vte_pty_spawn(pty,
                             working_directory,
                             argv,
                             envv,
                             spawn_flags,
                             child_setup, child_setup_data,
                             &pid,
                             error)) {
                g_object_unref(pty);
                return FALSE;
        }

        vte_terminal_set_pty_object(terminal, pty);
        vte_terminal_watch_child(terminal, pid);
        g_object_unref (pty);

        if (child_pid)
                *child_pid = pid;

        return TRUE;
}

/**
 * vte_terminal_forkpty:
 * @terminal: a #VteTerminal
 * @envv: a list of environment variables to be added to the environment before
 * starting returning in the child process, or %NULL
 * @working_directory: the name of a directory the child process should change to, or
 * %NULL
 * @lastlog: %TRUE if the session should be logged to the lastlog
 * @utmp: %TRUE if the session should be logged to the utmp/utmpx log
 * @wtmp: %TRUE if the session should be logged to the wtmp/wtmpx log
 *
 * Starts a new child process under a newly-allocated controlling
 * pseudo-terminal.  TERM is automatically set to reflect the terminal widget's
 * emulation setting.  If @lastlog, @utmp, or @wtmp are %TRUE, logs the session
 * to the specified system log files.
 *
 * Note that all file descriptors except stdin/stdout/stderr will be closed
 * in the child.
 *
 * Note that @envv and @working_directory are silently ignored.
 *
 * Returns: the ID of the new process in the parent, 0 in the child, and -1 if
 * there was an error
 *
 * Since: 0.11.11
 *
 * Deprecated: 0.26: Use #VtePty and fork() instead
 */
pid_t
vte_terminal_forkpty(VteTerminal *terminal,
		     char **envv, const char *working_directory,
		     gboolean lastlog, gboolean utmp, gboolean wtmp)
{
#ifdef HAVE_FORK
        VtePty *pty;
        GPid pid;

        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);

        pty = vte_pty_new(__vte_pty_get_pty_flags(lastlog, utmp, wtmp), NULL);
        if (pty == NULL)
                return FALSE;

        if (!__vte_pty_fork(pty,
                            &pid,
                            NULL)) {
                g_object_unref(pty);
                return FALSE;
        }

        vte_terminal_set_pty_object(terminal, pty);
        // FIXMEchpe is that really right?
        vte_terminal_watch_child(terminal, pid);

        return pid;
#else
        return -1;
#endif
}

/* Handle an EOF from the client. */
static void
vte_terminal_eof(GIOChannel *channel, VteTerminal *terminal)
{
        GObject *object = G_OBJECT(terminal);

        g_object_freeze_notify(object);

        vte_terminal_set_pty_object(terminal, NULL);

	/* Emit a signal that we read an EOF. */
	vte_terminal_queue_eof(terminal);

        g_object_thaw_notify(object);
}

/* Reset the input method context. */
static void
vte_terminal_im_reset(VteTerminal *terminal)
{
	if (gtk_widget_get_realized (&terminal->widget)) {
		gtk_im_context_reset(terminal->pvt->im_context);
		if (terminal->pvt->im_preedit != NULL) {
			g_free(terminal->pvt->im_preedit);
			terminal->pvt->im_preedit = NULL;
		}
		if (terminal->pvt->im_preedit_attrs != NULL) {
			pango_attr_list_unref(terminal->pvt->im_preedit_attrs);
			terminal->pvt->im_preedit_attrs = NULL;
		}
	}
}

/* Emit whichever signals are called for here. */
static void
vte_terminal_emit_pending_text_signals(VteTerminal *terminal, GQuark quark)
{
	static struct {
		const char *name;
		GQuark quark;
	} non_visual_quarks[] = {
		{"mb", 0},
		{"md", 0},
		{"mr", 0},
		{"mu", 0},
		{"se", 0},
		{"so", 0},
		{"ta", 0},
		{"character-attributes", 0},
	};
	guint i;

	if (quark != 0) {
		for (i = 0; i < G_N_ELEMENTS(non_visual_quarks); i++) {
			if (non_visual_quarks[i].quark == 0) {
				non_visual_quarks[i].quark =
					g_quark_from_static_string(non_visual_quarks[i].name);
			}
			if (quark == non_visual_quarks[i].quark) {
				return;
			}
		}
	}

	if (terminal->pvt->text_modified_flag) {
		_vte_debug_print(VTE_DEBUG_SIGNALS,
				"Emitting buffered `text-modified'.\n");
		vte_terminal_emit_text_modified(terminal);
		terminal->pvt->text_modified_flag = FALSE;
	}
	if (terminal->pvt->text_inserted_flag) {
		_vte_debug_print(VTE_DEBUG_SIGNALS,
				"Emitting buffered `text-inserted'\n");
		_vte_terminal_emit_text_inserted(terminal);
		terminal->pvt->text_inserted_flag = FALSE;
	}
	if (terminal->pvt->text_deleted_flag) {
		_vte_debug_print(VTE_DEBUG_SIGNALS,
				"Emitting buffered `text-deleted'\n");
		_vte_terminal_emit_text_deleted(terminal);
		terminal->pvt->text_deleted_flag = FALSE;
	}
}

/* Process incoming data, first converting it to unicode characters, and then
 * processing control sequences. */
static void
vte_terminal_process_incoming(VteTerminal *terminal)
{
	VteScreen *screen;
	VteVisualPosition cursor;
	gboolean cursor_visible;
	GdkPoint bbox_topleft, bbox_bottomright;
	gunichar *wbuf, c;
	long wcount, start, delta;
	gboolean leftovers, modified, bottom, again;
	gboolean invalidated_text;
	GArray *unichars;
	struct _vte_incoming_chunk *chunk, *next_chunk, *achunk = NULL;

	_vte_debug_print(VTE_DEBUG_IO,
			"Handler processing %"G_GSIZE_FORMAT" bytes over %"G_GSIZE_FORMAT" chunks + %d bytes pending.\n",
			_vte_incoming_chunks_length(terminal->pvt->incoming),
			_vte_incoming_chunks_count(terminal->pvt->incoming),
			terminal->pvt->pending->len);
	_vte_debug_print (VTE_DEBUG_WORK, "(");

	screen = terminal->pvt->screen;

	delta = screen->scroll_delta;
	bottom = screen->insert_delta == delta;

	/* Save the current cursor position. */
	cursor = screen->cursor_current;
	cursor_visible = terminal->pvt->cursor_visible;

	/* We should only be called when there's data to process. */
	g_assert(terminal->pvt->incoming ||
		 (terminal->pvt->pending->len > 0));

	/* Convert the data into unicode characters. */
	unichars = terminal->pvt->pending;
	for (chunk = _vte_incoming_chunks_reverse (terminal->pvt->incoming);
			chunk != NULL;
			chunk = next_chunk) {
		gsize processed;
		next_chunk = chunk->next;
		if (chunk->len == 0) {
			goto skip_chunk;
		}
		processed = _vte_iso2022_process(terminal->pvt->iso2022,
				chunk->data, chunk->len,
				unichars);
		if (G_UNLIKELY (processed != chunk->len)) {
			/* shuffle the data about */
			g_memmove (chunk->data, chunk->data + processed,
					chunk->len - processed);
			chunk->len = chunk->len - processed;
			processed = sizeof (chunk->data) - chunk->len;
			if (processed != 0 && next_chunk !=  NULL) {
				if (next_chunk->len <= processed) {
					/* consume it entirely */
					memcpy (chunk->data + chunk->len,
							next_chunk->data,
							next_chunk->len);
					chunk->len += next_chunk->len;
					chunk->next = next_chunk->next;
					release_chunk (next_chunk);
				} else {
					/* next few bytes */
					memcpy (chunk->data + chunk->len,
							next_chunk->data,
							processed);
					chunk->len += processed;
					g_memmove (next_chunk->data,
							next_chunk->data + processed,
							next_chunk->len - processed);
					next_chunk->len -= processed;
				}
				next_chunk = chunk; /* repeat */
			} else {
				break;
			}
		} else {
skip_chunk:
			/* cache the last chunk */
			if (achunk) {
				release_chunk (achunk);
			}
			achunk = chunk;
		}
	}
	if (achunk) {
		if (chunk != NULL) {
			release_chunk (achunk);
		} else {
			chunk = achunk;
			chunk->next = NULL;
			chunk->len = 0;
		}
	}
	terminal->pvt->incoming = chunk;

	/* Compute the number of unicode characters we got. */
	wbuf = &g_array_index(unichars, gunichar, 0);
	wcount = unichars->len;

	/* Try initial substrings. */
	start = 0;
	modified = leftovers = again = FALSE;
	invalidated_text = FALSE;

	bbox_bottomright.x = bbox_bottomright.y = -G_MAXINT;
	bbox_topleft.x = bbox_topleft.y = G_MAXINT;

	while (start < wcount && !leftovers) {
		const char *match;
		GQuark quark;
		const gunichar *next;
		GValueArray *params = NULL;

		/* Try to match any control sequences. */
		_vte_matcher_match(terminal->pvt->matcher,
				   &wbuf[start],
				   wcount - start,
				   &match,
				   &next,
				   &quark,
				   &params);
		/* We're in one of three possible situations now.
		 * First, the match string is a non-empty string and next
		 * points to the first character which isn't part of this
		 * sequence. */
		if ((match != NULL) && (match[0] != '\0')) {
			/* Call the right sequence handler for the requested
			 * behavior. */
			_vte_terminal_handle_sequence(terminal,
						      match,
						      quark,
						      params);
			/* Skip over the proper number of unicode chars. */
			start = (next - wbuf);
			modified = TRUE;

			/* if we have moved during the sequence handler, restart the bbox */
			if (invalidated_text &&
					(screen->cursor_current.col > bbox_bottomright.x + VTE_CELL_BBOX_SLACK ||
					 screen->cursor_current.col < bbox_topleft.x - VTE_CELL_BBOX_SLACK     ||
					 screen->cursor_current.row > bbox_bottomright.y + VTE_CELL_BBOX_SLACK ||
					 screen->cursor_current.row < bbox_topleft.y - VTE_CELL_BBOX_SLACK)) {
				/* Clip off any part of the box which isn't already on-screen. */
				bbox_topleft.x = MAX(bbox_topleft.x, 0);
				bbox_topleft.y = MAX(bbox_topleft.y, delta);
				bbox_bottomright.x = MIN(bbox_bottomright.x,
						terminal->column_count);
				/* lazily apply the +1 to the cursor_row */
				bbox_bottomright.y = MIN(bbox_bottomright.y + 1,
						delta + terminal->row_count);

				_vte_invalidate_cells(terminal,
						bbox_topleft.x,
						bbox_bottomright.x - bbox_topleft.x,
						bbox_topleft.y,
						bbox_bottomright.y - bbox_topleft.y);

				invalidated_text = FALSE;
				bbox_bottomright.x = bbox_bottomright.y = -G_MAXINT;
				bbox_topleft.x = bbox_topleft.y = G_MAXINT;
			}
		} else
		/* Second, we have a NULL match, and next points to the very
		 * next character in the buffer.  Insert the character which
		 * we're currently examining into the screen. */
		if (match == NULL) {
			c = wbuf[start];
			/* If it's a control character, permute the order, per
			 * vttest. */
			if ((c != *next) &&
			    ((*next & 0x1f) == *next) &&
			    (start + 1 < next - wbuf)) {
				const gunichar *tnext = NULL;
				const char *tmatch = NULL;
				GQuark tquark = 0;
				gunichar ctrl;
				int i;
				/* We don't want to permute it if it's another
				 * control sequence, so check if it is. */
				_vte_matcher_match(terminal->pvt->matcher,
						   next,
						   wcount - (next - wbuf),
						   &tmatch,
						   &tnext,
						   &tquark,
						   NULL);
				/* We only do this for non-control-sequence
				 * characters and random garbage. */
				if (tnext == next + 1) {
					/* Save the control character. */
					ctrl = *next;
					/* Move everything before it up a
					 * slot.  */
					for (i = next - wbuf; i > start; i--) {
						wbuf[i] = wbuf[i - 1];
					}
					/* Move the control character to the
					 * front. */
					wbuf[i] = ctrl;
					goto next_match;
				}
			}
			_VTE_DEBUG_IF(VTE_DEBUG_PARSE) {
				gunichar cc = c & ~VTE_ISO2022_ENCODED_WIDTH_MASK;
				if (cc > 255) {
					g_printerr("U+%04lx\n", (long) cc);
				} else {
					if (cc > 127) {
						g_printerr("%ld = ",
								(long) cc);
					}
					if (cc < 32) {
						g_printerr("^%lc\n",
								(wint_t)cc + 64);
					} else {
						g_printerr("`%lc'\n",
								(wint_t)cc);
					}
				}
			}

			bbox_topleft.x = MIN(bbox_topleft.x,
					screen->cursor_current.col);
			bbox_topleft.y = MIN(bbox_topleft.y,
					screen->cursor_current.row);

			/* Insert the character. */
			if (G_UNLIKELY (_vte_terminal_insert_char(terminal, c,
						 FALSE, FALSE))) {
				/* line wrapped, correct bbox */
				if (invalidated_text &&
						(screen->cursor_current.col > bbox_bottomright.x + VTE_CELL_BBOX_SLACK	||
						 screen->cursor_current.col < bbox_topleft.x - VTE_CELL_BBOX_SLACK	||
						 screen->cursor_current.row > bbox_bottomright.y + VTE_CELL_BBOX_SLACK	||
						 screen->cursor_current.row < bbox_topleft.y - VTE_CELL_BBOX_SLACK)) {
					/* Clip off any part of the box which isn't already on-screen. */
					bbox_topleft.x = MAX(bbox_topleft.x, 0);
					bbox_topleft.y = MAX(bbox_topleft.y, delta);
					bbox_bottomright.x = MIN(bbox_bottomright.x,
							terminal->column_count);
					/* lazily apply the +1 to the cursor_row */
					bbox_bottomright.y = MIN(bbox_bottomright.y + 1,
							delta + terminal->row_count);

					_vte_invalidate_cells(terminal,
							bbox_topleft.x,
							bbox_bottomright.x - bbox_topleft.x,
							bbox_topleft.y,
							bbox_bottomright.y - bbox_topleft.y);
					bbox_bottomright.x = bbox_bottomright.y = -G_MAXINT;
					bbox_topleft.x = bbox_topleft.y = G_MAXINT;

				}
				bbox_topleft.x = MIN(bbox_topleft.x, 0);
				bbox_topleft.y = MIN(bbox_topleft.y,
						screen->cursor_current.row);
			}
			/* Add the cells over which we have moved to the region
			 * which we need to refresh for the user. */
			bbox_bottomright.x = MAX(bbox_bottomright.x,
					screen->cursor_current.col);
			/* cursor_current.row + 1 (defer until inv.) */
			bbox_bottomright.y = MAX(bbox_bottomright.y,
					screen->cursor_current.row);
			invalidated_text = TRUE;

			/* We *don't* emit flush pending signals here. */
			modified = TRUE;
			start++;
		} else {
			/* Case three: the read broke in the middle of a
			 * control sequence, so we're undecided with no more
			 * data to consult. If we have data following the
			 * middle of the sequence, then it's just garbage data,
			 * and for compatibility, we should discard it. */
			if (wbuf + wcount > next) {
				_vte_debug_print(VTE_DEBUG_PARSE,
						"Invalid control "
						"sequence, discarding %ld "
						"characters.\n",
						(long)(next - (wbuf + start)));
				/* Discard. */
				start = next - wbuf + 1;
			} else {
				/* Pause processing here and wait for more
				 * data before continuing. */
				leftovers = TRUE;
			}
		}

#ifdef VTE_DEBUG
		/* Some safety checks: ensure the visible parts of the buffer
		 * are all in the buffer. */
		g_assert(screen->insert_delta >=
			 _vte_ring_delta(screen->row_data));
		/* The cursor shouldn't be above or below the addressable
		 * part of the display buffer. */
		g_assert(screen->cursor_current.row >= screen->insert_delta);
#endif

next_match:
		if (G_LIKELY(params != NULL)) {
			/* Free any parameters we don't care about any more. */
			_vte_matcher_free_params_array(terminal->pvt->matcher,
					params);
		}
	}

	/* Remove most of the processed characters. */
	if (start < wcount) {
		g_array_remove_range(terminal->pvt->pending, 0, start);
	} else {
		g_array_set_size(terminal->pvt->pending, 0);
		/* If we're out of data, we needn't pause to let the
		 * controlling application respond to incoming data, because
		 * the main loop is already going to do that. */
	}

	if (modified) {
		/* Keep the cursor on-screen if we scroll on output, or if
		 * we're currently at the bottom of the buffer. */
		_vte_terminal_update_insert_delta(terminal);
		if (terminal->pvt->scroll_on_output || bottom) {
			vte_terminal_maybe_scroll_to_bottom(terminal);
		}
		/* Deselect the current selection if its contents are changed
		 * by this insertion. */
		if (terminal->pvt->has_selection) {
			char *selection;
			selection =
			vte_terminal_get_text_range(terminal,
						    terminal->pvt->selection_start.row,
						    0,
						    terminal->pvt->selection_end.row,
						    terminal->column_count,
						    vte_cell_is_selected,
						    NULL,
						    NULL);
			if ((selection == NULL) || (terminal->pvt->selection == NULL) ||
			    (strcmp(selection, terminal->pvt->selection) != 0)) {
				vte_terminal_deselect_all(terminal);
			}
			g_free(selection);
		}
	}

	if (modified || (screen != terminal->pvt->screen)) {
		/* Signal that the visible contents changed. */
		_vte_terminal_queue_contents_changed(terminal);
	}

	vte_terminal_emit_pending_signals (terminal);

	if (invalidated_text) {
		/* Clip off any part of the box which isn't already on-screen. */
		bbox_topleft.x = MAX(bbox_topleft.x, 0);
		bbox_topleft.y = MAX(bbox_topleft.y, delta);
		bbox_bottomright.x = MIN(bbox_bottomright.x,
				terminal->column_count);
		/* lazily apply the +1 to the cursor_row */
		bbox_bottomright.y = MIN(bbox_bottomright.y + 1,
				delta + terminal->row_count);

		_vte_invalidate_cells(terminal,
				bbox_topleft.x,
				bbox_bottomright.x - bbox_topleft.x,
				bbox_topleft.y,
				bbox_bottomright.y - bbox_topleft.y);
	}


	if ((cursor.col != terminal->pvt->screen->cursor_current.col) ||
	    (cursor.row != terminal->pvt->screen->cursor_current.row)) {
		/* invalidate the old and new cursor positions */
		if (cursor_visible)
			_vte_invalidate_cell(terminal, cursor.col, cursor.row);
		_vte_invalidate_cursor_once(terminal, FALSE);
		_vte_check_cursor_blink(terminal);
		/* Signal that the cursor moved. */
		vte_terminal_queue_cursor_moved(terminal);
	} else if (cursor_visible != terminal->pvt->cursor_visible) {
		_vte_invalidate_cell(terminal, cursor.col, cursor.row);
		_vte_check_cursor_blink(terminal);
	}

	/* Tell the input method where the cursor is. */
	if (gtk_widget_get_realized (&terminal->widget)) {
		GdkRectangle rect;
		rect.x = terminal->pvt->screen->cursor_current.col *
			 terminal->char_width + terminal->pvt->inner_border.left;
		rect.width = terminal->char_width;
		rect.y = (terminal->pvt->screen->cursor_current.row - delta) *
			 terminal->char_height + terminal->pvt->inner_border.top;
		rect.height = terminal->char_height;
		gtk_im_context_set_cursor_location(terminal->pvt->im_context,
						   &rect);
	}

	_vte_debug_print (VTE_DEBUG_WORK, ")");
	_vte_debug_print (VTE_DEBUG_IO,
			"%ld chars and %ld bytes in %"G_GSIZE_FORMAT" chunks left to process.\n",
			(long) unichars->len,
			(long) _vte_incoming_chunks_length(terminal->pvt->incoming),
			_vte_incoming_chunks_count(terminal->pvt->incoming));
}

static inline void
_vte_terminal_enable_input_source (VteTerminal *terminal)
{
	if (terminal->pvt->pty_channel == NULL) {
		return;
	}

	if (terminal->pvt->pty_input_source == 0) {
		_vte_debug_print (VTE_DEBUG_IO, "polling vte_terminal_io_read\n");
		terminal->pvt->pty_input_source =
			g_io_add_watch_full(terminal->pvt->pty_channel,
					    VTE_CHILD_INPUT_PRIORITY,
					    G_IO_IN | G_IO_HUP,
					    (GIOFunc) vte_terminal_io_read,
					    terminal,
					    (GDestroyNotify) mark_input_source_invalid);
	}
}
static void
_vte_terminal_feed_chunks (VteTerminal *terminal, struct _vte_incoming_chunk *chunks)
{
	struct _vte_incoming_chunk *last;

	_vte_debug_print(VTE_DEBUG_IO, "Feed %"G_GSIZE_FORMAT" bytes, in %"G_GSIZE_FORMAT" chunks.\n",
			_vte_incoming_chunks_length(chunks),
			_vte_incoming_chunks_count(chunks));

	for (last = chunks; last->next != NULL; last = last->next) ;
	last->next = terminal->pvt->incoming;
	terminal->pvt->incoming = chunks;
}
/* Read and handle data from the child. */
static gboolean
vte_terminal_io_read(GIOChannel *channel,
		     GIOCondition condition,
		     VteTerminal *terminal)
{
	int err = 0;
	gboolean eof, again = TRUE;

	_vte_debug_print (VTE_DEBUG_WORK, ".");

	/* Check for end-of-file. */
	eof = condition & G_IO_HUP;

	/* Read some data in from this channel. */
	if (condition & G_IO_IN) {
		struct _vte_incoming_chunk *chunk, *chunks = NULL;
		const int fd = g_io_channel_unix_get_fd (channel);
		guchar *bp;
		int rem, len;
		guint bytes, max_bytes;

		/* Limit the amount read between updates, so as to
		 * 1. maintain fairness between multiple terminals;
		 * 2. prevent reading the entire output of a command in one
		 *    pass, i.e. we always try to refresh the terminal ~40Hz.
		 *    See time_process_incoming() where we estimate the
		 *    maximum number of bytes we can read/process in between
		 *    updates.
		 */
		max_bytes = terminal->pvt->active ?
		            g_list_length (active_terminals) - 1 : 0;
		if (max_bytes) {
			max_bytes = terminal->pvt->max_input_bytes / max_bytes;
		} else {
			max_bytes = VTE_MAX_INPUT_READ;
		}
		bytes = terminal->pvt->input_bytes;

		chunk = terminal->pvt->incoming;
		do {
			if (!chunk || chunk->len >= 3*sizeof (chunk->data)/4) {
				chunk = get_chunk ();
				chunk->next = chunks;
				chunks = chunk;
			}
			rem = sizeof (chunk->data) - chunk->len;
			bp = chunk->data + chunk->len;
			len = 0;
			do {
				int ret = read (fd, bp, rem);
				switch (ret){
					case -1:
						err = errno;
						goto out;
					case 0:
						eof = TRUE;
						goto out;
					default:
						bp += ret;
						rem -= ret;
						len += ret;
						break;
				}
			} while (rem);
out:
			chunk->len += len;
			bytes += len;
		} while (bytes < max_bytes &&
		         chunk->len == sizeof (chunk->data));
		if (chunk->len == 0 && chunk == chunks) {
			chunks = chunks->next;
			release_chunk (chunk);
		}

		if (chunks != NULL) {
			_vte_terminal_feed_chunks (terminal, chunks);
		}
		if (!vte_terminal_is_processing (terminal)) {
			GDK_THREADS_ENTER ();
			vte_terminal_add_process_timeout (terminal);
			GDK_THREADS_LEAVE ();
		}
		terminal->pvt->pty_input_active = len != 0;
		terminal->pvt->input_bytes = bytes;
		again = bytes < max_bytes;

		_vte_debug_print (VTE_DEBUG_IO, "read %d/%d bytes, again? %s, active? %s\n",
				bytes, max_bytes,
				again ? "yes" : "no",
				terminal->pvt->pty_input_active ? "yes" : "no");
	}

	/* Error? */
	switch (err) {
		case 0: /* no error */
			break;
		case EIO: /* Fake an EOF. */
			eof = TRUE;
			break;
		case EAGAIN:
		case EBUSY: /* do nothing */
			break;
		default:
			/* Translators: %s is replaced with error message returned by strerror(). */
			g_warning (_("Error reading from child: " "%s."),
					g_strerror (err));
			break;
	}

	/* If we detected an eof condition, signal one. */
	if (eof) {
		/* potential deadlock ... */
		if (!vte_terminal_is_processing (terminal)) {
			GDK_THREADS_ENTER ();
			vte_terminal_eof (channel, terminal);
			GDK_THREADS_LEAVE ();
		} else {
			vte_terminal_eof (channel, terminal);
		}

		again = FALSE;
	}

	return again;
}

/**
 * vte_terminal_feed:
 * @terminal: a #VteTerminal
 * @data: a string in the terminal's current encoding
 * @length: the length of the string
 *
 * Interprets @data as if it were data received from a child process.  This
 * can either be used to drive the terminal without a child process, or just
 * to mess with your users.
 */
void
vte_terminal_feed(VteTerminal *terminal, const char *data, glong length)
{
	/* If length == -1, use the length of the data string. */
	if (length == ((gssize)-1)) {
		length = strlen(data);
	}

	/* If we have data, modify the incoming buffer. */
	if (length > 0) {
		struct _vte_incoming_chunk *chunk;
		if (terminal->pvt->incoming &&
				(gsize)length < sizeof (terminal->pvt->incoming->data) - terminal->pvt->incoming->len) {
			chunk = terminal->pvt->incoming;
		} else {
			chunk = get_chunk ();
			_vte_terminal_feed_chunks (terminal, chunk);
		}
		do { /* break the incoming data into chunks */
			gsize rem = sizeof (chunk->data) - chunk->len;
			gsize len = (gsize) length < rem ? (gsize) length : rem;
			memcpy (chunk->data + chunk->len, data, len);
			chunk->len += len;
			length -= len;
			if (length == 0) {
				break;
			}
			data += len;

			chunk = get_chunk ();
			_vte_terminal_feed_chunks (terminal, chunk);
		} while (1);
		vte_terminal_start_processing (terminal);
	}
}

/* Send locally-encoded characters to the child. */
static gboolean
vte_terminal_io_write(GIOChannel *channel,
		      GIOCondition condition,
		      VteTerminal *terminal)
{
	gssize count;
	int fd;
	gboolean leave_open;

	fd = g_io_channel_unix_get_fd(channel);

	count = write(fd, terminal->pvt->outgoing->data,
		      _vte_buffer_length(terminal->pvt->outgoing));
	if (count != -1) {
		_VTE_DEBUG_IF (VTE_DEBUG_IO) {
			gssize i;
			for (i = 0; i < count; i++) {
				g_printerr("Wrote %c%c\n",
					((guint8)terminal->pvt->outgoing->data[i]) >= 32 ?
					' ' : '^',
					((guint8)terminal->pvt->outgoing->data[i]) >= 32 ?
					terminal->pvt->outgoing->data[i] :
					((guint8)terminal->pvt->outgoing->data[i])  + 64);
			}
		}
		_vte_buffer_consume(terminal->pvt->outgoing, count);
	}

	if (_vte_buffer_length(terminal->pvt->outgoing) == 0) {
		leave_open = FALSE;
	} else {
		leave_open = TRUE;
	}

	return leave_open;
}

/* Convert some arbitrarily-encoded data to send to the child. */
static void
vte_terminal_send(VteTerminal *terminal, const char *encoding,
		  const void *data, gssize length,
		  gboolean local_echo, gboolean newline_stuff)
{
	gsize icount, ocount;
	const guchar *ibuf;
	guchar *obuf, *obufptr;
	gchar *cooked;
	VteConv conv;
	long crcount, cooked_length, i;

	g_assert(VTE_IS_TERMINAL(terminal));
	g_assert(encoding && strcmp(encoding, "UTF-8") == 0);

	conv = VTE_INVALID_CONV;
	if (strcmp(encoding, "UTF-8") == 0) {
		conv = terminal->pvt->outgoing_conv;
	}
	if (conv == VTE_INVALID_CONV) {
		g_warning (_("Unable to send data to child, invalid charset convertor"));
		return;
	}

	icount = length;
	ibuf =  data;
	ocount = ((length + 1) * VTE_UTF8_BPC) + 1;
	_vte_buffer_set_minimum_size(terminal->pvt->conv_buffer, ocount);
	obuf = obufptr = terminal->pvt->conv_buffer->data;

	if (_vte_conv(conv, &ibuf, &icount, &obuf, &ocount) == (gsize)-1) {
		g_warning(_("Error (%s) converting data for child, dropping."),
			  g_strerror(errno));
	} else {
		crcount = 0;
		if (newline_stuff) {
			for (i = 0; i < obuf - obufptr; i++) {
				switch (obufptr[i]) {
				case '\015':
					crcount++;
					break;
				default:
					break;
				}
			}
		}
		if (crcount > 0) {
			cooked = g_malloc(obuf - obufptr + crcount);
			cooked_length = 0;
			for (i = 0; i < obuf - obufptr; i++) {
				switch (obufptr[i]) {
				case '\015':
					cooked[cooked_length++] = '\015';
					cooked[cooked_length++] = '\012';
					break;
				default:
					cooked[cooked_length++] = obufptr[i];
					break;
				}
			}
		} else {
			cooked = (gchar *)obufptr;
			cooked_length = obuf - obufptr;
		}
		/* Tell observers that we're sending this to the child. */
		if (cooked_length > 0) {
			vte_terminal_emit_commit(terminal,
						 cooked, cooked_length);
		}
		/* Echo the text if we've been asked to do so. */
		if ((cooked_length > 0) && local_echo) {
			gunichar *ucs4;
			ucs4 = g_utf8_to_ucs4(cooked, cooked_length,
					      NULL, NULL, NULL);
			if (ucs4 != NULL) {
				int len;
				len = g_utf8_strlen(cooked, cooked_length);
				for (i = 0; i < len; i++) {
					_vte_terminal_insert_char(terminal,
								 ucs4[i],
								 FALSE,
								 TRUE);
				}
				g_free(ucs4);
			}
		}
		/* If there's a place for it to go, add the data to the
		 * outgoing buffer. */
		if ((cooked_length > 0) && (terminal->pvt->pty != NULL)) {
			_vte_buffer_append(terminal->pvt->outgoing,
					   cooked, cooked_length);
			_VTE_DEBUG_IF(VTE_DEBUG_KEYBOARD) {
				for (i = 0; i < cooked_length; i++) {
					if ((((guint8) cooked[i]) < 32) ||
					    (((guint8) cooked[i]) > 127)) {
						g_printerr(
							"Sending <%02x> "
							"to child.\n",
							cooked[i]);
					} else {
						g_printerr(
							"Sending '%c' "
							"to child.\n",
							cooked[i]);
					}
				}
			}
			/* If we need to start waiting for the child pty to
			 * become available for writing, set that up here. */
			_vte_terminal_connect_pty_write(terminal);
		}
		if (crcount > 0) {
			g_free(cooked);
		}
	}
	return;
}

/**
 * vte_terminal_feed_child:
 * @terminal: a #VteTerminal
 * @text: data to send to the child
 * @length: length of @text in bytes, or -1 if @text is NUL-terminated
 *
 * Sends a block of UTF-8 text to the child as if it were entered by the user
 * at the keyboard.
 */
void
vte_terminal_feed_child(VteTerminal *terminal, const char *text, glong length)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	if (length == ((gssize)-1)) {
		length = strlen(text);
	}
	if (length > 0) {
		vte_terminal_send(terminal, "UTF-8", text, length,
				  FALSE, FALSE);
	}
}

/**
 * vte_terminal_feed_child_binary:
 * @terminal: a #VteTerminal
 * @data: data to send to the child
 * @length: length of @data
 *
 * Sends a block of binary data to the child.
 *
 * Since: 0.12.1
 */
void
vte_terminal_feed_child_binary(VteTerminal *terminal, const char *data, glong length)
{
	g_assert(VTE_IS_TERMINAL(terminal));

	/* Tell observers that we're sending this to the child. */
	if (length > 0) {
		vte_terminal_emit_commit(terminal,
					 data, length);

		/* If there's a place for it to go, add the data to the
		 * outgoing buffer. */
		if (terminal->pvt->pty != NULL) {
			_vte_buffer_append(terminal->pvt->outgoing,
					   data, length);
			/* If we need to start waiting for the child pty to
			 * become available for writing, set that up here. */
			_vte_terminal_connect_pty_write(terminal);
		}
	}
}

static void
vte_terminal_feed_child_using_modes(VteTerminal *terminal,
				    const char *data, glong length)
{
	if (length == ((gssize)-1)) {
		length = strlen(data);
	}
	if (length > 0) {
		vte_terminal_send(terminal, "UTF-8", data, length,
				  !terminal->pvt->screen->sendrecv_mode,
				  terminal->pvt->screen->linefeed_mode);
	}
}

/* Send text from the input method to the child. */
static void
vte_terminal_im_commit(GtkIMContext *im_context, gchar *text, VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Input method committed `%s'.\n", text);
	vte_terminal_feed_child_using_modes(terminal, text, -1);
	/* Committed text was committed because the user pressed a key, so
	 * we need to obey the scroll-on-keystroke setting. */
	if (terminal->pvt->scroll_on_keystroke) {
		vte_terminal_maybe_scroll_to_bottom(terminal);
	}
}

/* We've started pre-editing. */
static void
vte_terminal_im_preedit_start(GtkIMContext *im_context, VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Input method pre-edit started.\n");
	terminal->pvt->im_preedit_active = TRUE;
}

/* We've stopped pre-editing. */
static void
vte_terminal_im_preedit_end(GtkIMContext *im_context, VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Input method pre-edit ended.\n");
	terminal->pvt->im_preedit_active = FALSE;
}

/* The pre-edit string changed. */
static void
vte_terminal_im_preedit_changed(GtkIMContext *im_context, VteTerminal *terminal)
{
	gchar *str;
	PangoAttrList *attrs;
	gint cursor;

	gtk_im_context_get_preedit_string(im_context, &str, &attrs, &cursor);
	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Input method pre-edit changed (%s,%d).\n",
			str, cursor);

	/* Queue the area where the current preedit string is being displayed
	 * for repainting. */
	_vte_invalidate_cursor_once(terminal, FALSE);

	g_free(terminal->pvt->im_preedit);
	terminal->pvt->im_preedit = str;

	if (terminal->pvt->im_preedit_attrs != NULL) {
		pango_attr_list_unref(terminal->pvt->im_preedit_attrs);
	}
	terminal->pvt->im_preedit_attrs = attrs;

	terminal->pvt->im_preedit_cursor = cursor;

	_vte_invalidate_cursor_once(terminal, FALSE);
}

/* Handle the toplevel being reconfigured. */
static gboolean
vte_terminal_configure_toplevel(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_EVENTS, "Top level parent configured.\n");

	if (terminal->pvt->bg_transparent) {
		/* We have to repaint the entire window, because we don't get
		 * an expose event unless some portion of our visible area
		 * moved out from behind another window. */
		_vte_invalidate_all(terminal);
	}

	return FALSE;
}

/* Handle a hierarchy-changed signal. */
static void
vte_terminal_hierarchy_changed(GtkWidget *widget, GtkWidget *old_toplevel,
			       gpointer data)
{
	GtkWidget *toplevel;

	_vte_debug_print(VTE_DEBUG_EVENTS, "Hierarchy changed.\n");
	if (old_toplevel != NULL) {
		g_signal_handlers_disconnect_by_func(old_toplevel,
						     vte_terminal_configure_toplevel,
						     widget);
	}

	toplevel = gtk_widget_get_toplevel(widget);
	if (toplevel != NULL) {
		g_signal_connect_swapped (toplevel, "configure-event",
				 G_CALLBACK (vte_terminal_configure_toplevel),
				 widget);
	}
}

static void
vte_terminal_set_inner_border(VteTerminal *terminal)
{
        VteTerminalPrivate *pvt = terminal->pvt;
        GtkWidget *widget = GTK_WIDGET(terminal);
        GtkBorder *border = NULL;
        GtkBorder inner_border;

        gtk_widget_style_get(widget, "inner-border", &border, NULL);

        if (border != NULL) {
                inner_border = *border;
                gtk_border_free(border);
        } else {
                inner_border = default_inner_border;
        }

        _vte_debug_print(VTE_DEBUG_MISC,
                         "Setting inner-border to { %d, %d, %d, %d }\n",
                         inner_border.left, inner_border.right,
                         inner_border.top, inner_border.bottom);

        if (memcmp(&inner_border, &pvt->inner_border, sizeof(GtkBorder)) == 0)
                return;

        pvt->inner_border = inner_border;

        gtk_widget_queue_resize(widget);
}

static void
vte_terminal_style_set (GtkWidget      *widget,
			GtkStyle       *prev_style)
{
	VteTerminal *terminal = VTE_TERMINAL(widget);
        float aspect;

        GTK_WIDGET_CLASS (vte_terminal_parent_class)->style_set (widget, prev_style);

        if (gtk_widget_get_style(widget) == prev_style)
                return;

        vte_terminal_set_font_full_internal(terminal, terminal->pvt->fontdesc,
                                            terminal->pvt->fontantialias);

        vte_terminal_set_inner_border(terminal);

        gtk_widget_style_get(widget, "cursor-aspect-ratio", &aspect, NULL);
        if (aspect != terminal->pvt->cursor_aspect_ratio) {
                terminal->pvt->cursor_aspect_ratio = aspect;
                _vte_invalidate_cursor_once(terminal, FALSE);
        }
}

static void
add_cursor_timeout (VteTerminal *terminal)
{
	if (terminal->pvt->cursor_blink_tag)
		return; /* already added */

	terminal->pvt->cursor_blink_time = 0;
	terminal->pvt->cursor_blink_tag = g_timeout_add_full(G_PRIORITY_LOW,
							     terminal->pvt->cursor_blink_cycle,
							     (GSourceFunc)vte_invalidate_cursor_periodic,
							     terminal,
							     NULL);
}

static void
remove_cursor_timeout (VteTerminal *terminal)
{
	if (terminal->pvt->cursor_blink_tag == 0)
		return; /* already removed */

	g_source_remove (terminal->pvt->cursor_blink_tag);
	terminal->pvt->cursor_blink_tag = 0;
}

/* Activates / disactivates the cursor blink timer to reduce wakeups */
static void
_vte_check_cursor_blink(VteTerminal *terminal)
{
	if (terminal->pvt->has_focus &&
	    terminal->pvt->cursor_blinks &&
	    terminal->pvt->cursor_visible)
		add_cursor_timeout(terminal);
	else
		remove_cursor_timeout(terminal);
}

void
_vte_terminal_audible_beep(VteTerminal *terminal)
{
	GdkDisplay *display;

	g_assert(VTE_IS_TERMINAL(terminal));
	display = gtk_widget_get_display(&terminal->widget);
	gdk_display_beep(display);
}

void
_vte_terminal_visible_beep(VteTerminal *terminal)
{
	GtkWidget *widget;
	GtkAllocation allocation;
	GtkStyle *style;
	PangoColor color;

	widget = &terminal->widget;

	if (gtk_widget_get_realized (widget)) {

		style = gtk_widget_get_style (widget);
		gtk_widget_get_allocation (widget, &allocation);
		color.red = style->fg[gtk_widget_get_state (widget)].red;
		color.green = style->fg[gtk_widget_get_state (widget)].green;
		color.blue = style->fg[gtk_widget_get_state (widget)].blue;

		_vte_draw_start(terminal->pvt->draw);
		_vte_draw_fill_rectangle(terminal->pvt->draw,
					 0, 0,
					 allocation.width, allocation.height,
					 &color, VTE_DRAW_OPAQUE);
		_vte_draw_end(terminal->pvt->draw);

		/* Force the repaint, max delay of UPDATE_REPEAT_TIMEOUT */
		_vte_invalidate_all (terminal);
	}
}

void
_vte_terminal_beep(VteTerminal *terminal)
{
	if (terminal->pvt->audible_bell) {
		_vte_terminal_audible_beep (terminal);
	}
	if (terminal->pvt->visible_bell) {
		_vte_terminal_visible_beep (terminal);
	}
}


static guint
vte_translate_ctrlkey (GdkEventKey *event)
{
	guint keyval;
	GdkKeymap *keymap;
	unsigned int i;

	if (event->keyval < 128)
		return event->keyval;

#if GTK_CHECK_VERSION (2, 90, 8)
        keymap = gdk_keymap_get_for_display(gdk_window_get_display (event->window));
#else
	keymap = gdk_keymap_get_for_display(gdk_drawable_get_display (event->window));
#endif

	/* Try groups in order to find one mapping the key to ASCII */
	for (i = 0; i < 4; i++) {
		GdkModifierType consumed_modifiers;

		gdk_keymap_translate_keyboard_state (keymap,
				event->hardware_keycode, event->state,
				i,
				&keyval, NULL, NULL, &consumed_modifiers);
		if (keyval < 128) {
			_vte_debug_print (VTE_DEBUG_EVENTS,
					"ctrl+Key, group=%d de-grouped into keyval=0x%x\n",
					event->group, keyval);
			return keyval;
		}
	}

	return event->keyval;
}

static void
vte_terminal_read_modifiers (VteTerminal *terminal,
			     GdkEvent *event)
{
	GdkModifierType modifiers;

	/* Read the modifiers. */
	if (gdk_event_get_state((GdkEvent*)event, &modifiers)) {
		GdkKeymap *keymap;
#if GTK_CHECK_VERSION (2, 90, 8)
                keymap = gdk_keymap_get_for_display(gdk_window_get_display(((GdkEventAny*)event)->window));
#else
                keymap = gdk_keymap_get_for_display(gdk_drawable_get_display(((GdkEventAny*)event)->window));
#endif
                gdk_keymap_add_virtual_modifiers (keymap, &modifiers);
		terminal->pvt->modifiers = modifiers;
	}
}

/* Read and handle a keypress event. */
static gint
vte_terminal_key_press(GtkWidget *widget, GdkEventKey *event)
{
	VteTerminal *terminal;
	GdkModifierType modifiers;
	struct _vte_termcap *termcap;
	const char *tterm;
	char *normal = NULL, *output;
	gssize normal_length = 0;
	int i;
	const char *special = NULL;
	struct termios tio;
	gboolean scrolled = FALSE, steal = FALSE, modifier = FALSE, handled,
		 suppress_meta_esc = FALSE;
	guint keyval = 0;
	gunichar keychar = 0;
	char keybuf[VTE_UTF8_BPC];

	terminal = VTE_TERMINAL(widget);

	/* First, check if GtkWidget's behavior already does something with
	 * this key. */
	if (GTK_WIDGET_CLASS(vte_terminal_parent_class)->key_press_event) {
		if ((GTK_WIDGET_CLASS(vte_terminal_parent_class))->key_press_event(widget,
								      event)) {
			return TRUE;
		}
	}

	/* If it's a keypress, record that we got the event, in case the
	 * input method takes the event from us. */
	if (event->type == GDK_KEY_PRESS) {
		/* Store a copy of the key. */
		keyval = event->keyval;
		vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

		/* If we're in margin bell mode and on the border of the
		 * margin, bell. */
		if (terminal->pvt->margin_bell) {
			if ((terminal->pvt->screen->cursor_current.col +
			     (glong) terminal->pvt->bell_margin) ==
			     terminal->column_count) {
				_vte_terminal_beep (terminal);
			}
		}

		if (terminal->pvt->cursor_blink_tag != 0)
		{
			remove_cursor_timeout (terminal);
			terminal->pvt->cursor_blink_state = TRUE;
			add_cursor_timeout (terminal);
		}

		/* Determine if this is just a modifier key. */
		modifier = _vte_keymap_key_is_modifier(keyval);

		/* Unless it's a modifier key, hide the pointer. */
		if (!modifier) {
			_vte_terminal_set_pointer_visible(terminal, FALSE);
		}

		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Keypress, modifiers=0x%x, "
				"keyval=0x%x, raw string=`%s'.\n",
				terminal->pvt->modifiers,
				keyval, event->string);

		/* We steal many keypad keys here. */
		if (!terminal->pvt->im_preedit_active) {
			switch (keyval) {
			case GDK_KEY (KP_Add):
			case GDK_KEY (KP_Subtract):
			case GDK_KEY (KP_Multiply):
			case GDK_KEY (KP_Divide):
			case GDK_KEY (KP_Enter):
				steal = TRUE;
				break;
			default:
				break;
			}
			if (terminal->pvt->modifiers & VTE_META_MASK) {
				steal = TRUE;
			}
			switch (keyval) {
			case GDK_KEY (Multi_key):
			case GDK_KEY (Codeinput):
			case GDK_KEY (SingleCandidate):
			case GDK_KEY (MultipleCandidate):
			case GDK_KEY (PreviousCandidate):
			case GDK_KEY (Kanji):
			case GDK_KEY (Muhenkan):
			case GDK_KEY (Henkan):
			case GDK_KEY (Romaji):
			case GDK_KEY (Hiragana):
			case GDK_KEY (Katakana):
			case GDK_KEY (Hiragana_Katakana):
			case GDK_KEY (Zenkaku):
			case GDK_KEY (Hankaku):
			case GDK_KEY (Zenkaku_Hankaku):
			case GDK_KEY (Touroku):
			case GDK_KEY (Massyo):
			case GDK_KEY (Kana_Lock):
			case GDK_KEY (Kana_Shift):
			case GDK_KEY (Eisu_Shift):
			case GDK_KEY (Eisu_toggle):
				steal = FALSE;
				break;
			default:
				break;
			}
		}
	}

	modifiers = terminal->pvt->modifiers;

	/* Let the input method at this one first. */
	if (!steal) {
		if (gtk_widget_get_realized (&terminal->widget)
				&& gtk_im_context_filter_keypress (terminal->pvt->im_context, event)) {
			_vte_debug_print(VTE_DEBUG_EVENTS,
					"Keypress taken by IM.\n");
			return TRUE;
		}
	}

	/* Now figure out what to send to the child. */
	if ((event->type == GDK_KEY_PRESS) && !modifier) {
		handled = FALSE;
		/* Map the key to a sequence name if we can. */
		switch (keyval) {
		case GDK_KEY (BackSpace):
			switch (terminal->pvt->backspace_binding) {
			case VTE_ERASE_ASCII_BACKSPACE:
				normal = g_strdup("");
				normal_length = 1;
				suppress_meta_esc = FALSE;
				break;
			case VTE_ERASE_ASCII_DELETE:
				normal = g_strdup("");
				normal_length = 1;
				suppress_meta_esc = FALSE;
				break;
			case VTE_ERASE_DELETE_SEQUENCE:
				special = "kD";
				suppress_meta_esc = TRUE;
				break;
			case VTE_ERASE_TTY:
				if (terminal->pvt->pty != NULL &&
				    tcgetattr(vte_pty_get_fd(terminal->pvt->pty), &tio) != -1)
				{
					normal = g_strdup_printf("%c", tio.c_cc[VERASE]);
					normal_length = 1;
				}
				suppress_meta_esc = FALSE;
				break;
			case VTE_ERASE_AUTO:
			default:
#ifndef _POSIX_VDISABLE
#define _POSIX_VDISABLE '\0'
#endif
				if (terminal->pvt->pty != NULL &&
				    tcgetattr(vte_pty_get_fd(terminal->pvt->pty), &tio) != -1 &&
				    tio.c_cc[VERASE] != _POSIX_VDISABLE)
				{
					normal = g_strdup_printf("%c", tio.c_cc[VERASE]);
					normal_length = 1;
				}
				else
				{
					normal = g_strdup("");
					normal_length = 1;
					suppress_meta_esc = FALSE;
				}
				suppress_meta_esc = FALSE;
				break;
			}
			handled = TRUE;
			break;
		case GDK_KEY (KP_Delete):
		case GDK_KEY (Delete):
			switch (terminal->pvt->delete_binding) {
			case VTE_ERASE_ASCII_BACKSPACE:
				normal = g_strdup("\010");
				normal_length = 1;
				break;
			case VTE_ERASE_ASCII_DELETE:
				normal = g_strdup("\177");
				normal_length = 1;
				break;
			case VTE_ERASE_TTY:
				if (terminal->pvt->pty != NULL &&
				    tcgetattr(vte_pty_get_fd(terminal->pvt->pty), &tio) != -1)
				{
					normal = g_strdup_printf("%c", tio.c_cc[VERASE]);
					normal_length = 1;
				}
				suppress_meta_esc = FALSE;
				break;
			case VTE_ERASE_DELETE_SEQUENCE:
			case VTE_ERASE_AUTO:
			default:
				special = "kD";
				break;
			}
			handled = TRUE;
			suppress_meta_esc = TRUE;
			break;
		case GDK_KEY (KP_Insert):
		case GDK_KEY (Insert):
			if (modifiers & GDK_SHIFT_MASK) {
				if (modifiers & GDK_CONTROL_MASK) {
					vte_terminal_paste_clipboard(terminal);
					handled = TRUE;
					suppress_meta_esc = TRUE;
				} else {
					vte_terminal_paste_primary(terminal);
					handled = TRUE;
					suppress_meta_esc = TRUE;
				}
			} else if (modifiers & GDK_CONTROL_MASK) {
				vte_terminal_copy_clipboard(terminal);
				handled = TRUE;
				suppress_meta_esc = TRUE;
			}
			break;
		/* Keypad/motion keys. */
		case GDK_KEY (KP_Up):
		case GDK_KEY (Up):
			if (modifiers & GDK_CONTROL_MASK 
                            && modifiers & GDK_SHIFT_MASK) {
				vte_terminal_scroll_lines(terminal, -1);
				scrolled = TRUE;
				handled = TRUE;
				suppress_meta_esc = TRUE;
			}
			break;
		case GDK_KEY (KP_Down):
		case GDK_KEY (Down):
			if (modifiers & GDK_CONTROL_MASK
                            && modifiers & GDK_SHIFT_MASK) {
				vte_terminal_scroll_lines(terminal, 1);
				scrolled = TRUE;
				handled = TRUE;
				suppress_meta_esc = TRUE;
			}
			break;
		case GDK_KEY (KP_Page_Up):
		case GDK_KEY (Page_Up):
			if (modifiers & GDK_SHIFT_MASK) {
				vte_terminal_scroll_pages(terminal, -1);
				scrolled = TRUE;
				handled = TRUE;
				suppress_meta_esc = TRUE;
			}
			break;
		case GDK_KEY (KP_Page_Down):
		case GDK_KEY (Page_Down):
			if (modifiers & GDK_SHIFT_MASK) {
				vte_terminal_scroll_pages(terminal, 1);
				scrolled = TRUE;
				handled = TRUE;
				suppress_meta_esc = TRUE;
			}
			break;
		case GDK_KEY (KP_Home):
		case GDK_KEY (Home):
			if (modifiers & GDK_SHIFT_MASK) {
				vte_terminal_maybe_scroll_to_top(terminal);
				scrolled = TRUE;
				handled = TRUE;
			}
			break;
		case GDK_KEY (KP_End):
		case GDK_KEY (End):
			if (modifiers & GDK_SHIFT_MASK) {
				vte_terminal_maybe_scroll_to_bottom(terminal);
				scrolled = TRUE;
				handled = TRUE;
			}
			break;
		/* Let Shift +/- tweak the font, like XTerm does. */
		case GDK_KEY (KP_Add):
		case GDK_KEY (KP_Subtract):
			if (modifiers &
			    (GDK_SHIFT_MASK | GDK_CONTROL_MASK)) {
				switch (keyval) {
				case GDK_KEY (KP_Add):
					vte_terminal_emit_increase_font_size(terminal);
					handled = TRUE;
					suppress_meta_esc = TRUE;
					break;
				case GDK_KEY (KP_Subtract):
					vte_terminal_emit_decrease_font_size(terminal);
					handled = TRUE;
					suppress_meta_esc = TRUE;
					break;
				}
			}
			break;
		default:
			break;
		}
		/* If the above switch statement didn't do the job, try mapping
		 * it to a literal or capability name. */
		if (handled == FALSE && terminal->pvt->termcap != NULL) {
			_vte_keymap_map(keyval, modifiers,
					terminal->pvt->sun_fkey_mode,
					terminal->pvt->hp_fkey_mode,
					terminal->pvt->legacy_fkey_mode,
					terminal->pvt->vt220_fkey_mode,
					terminal->pvt->cursor_mode == VTE_KEYMODE_APPLICATION,
					terminal->pvt->keypad_mode == VTE_KEYMODE_APPLICATION,
					terminal->pvt->termcap,
					terminal->pvt->emulation ?
					terminal->pvt->emulation : vte_terminal_get_default_emulation(terminal),
					&normal,
					&normal_length,
					&special);
			/* If we found something this way, suppress
			 * escape-on-meta. */
			if (((normal != NULL) && (normal_length > 0)) ||
			    (special != NULL)) {
				suppress_meta_esc = TRUE;
			}
		}

		/* Shall we do this here or earlier?  See bug 375112 and bug 589557 */
		if (modifiers & GDK_CONTROL_MASK)
			keyval = vte_translate_ctrlkey(event);

		/* If we didn't manage to do anything, try to salvage a
		 * printable string. */
		if (handled == FALSE && normal == NULL && special == NULL) {

			/* Convert the keyval to a gunichar. */
			keychar = gdk_keyval_to_unicode(keyval);
			normal_length = 0;
			if (keychar != 0) {
				/* Convert the gunichar to a string. */
				normal_length = g_unichar_to_utf8(keychar,
								  keybuf);
				if (normal_length != 0) {
					normal = g_malloc(normal_length + 1);
					memcpy(normal, keybuf, normal_length);
					normal[normal_length] = '\0';
				} else {
					normal = NULL;
				}
			}
			if ((normal != NULL) &&
			    (modifiers & GDK_CONTROL_MASK)) {
				/* Replace characters which have "control"
				 * counterparts with those counterparts. */
				for (i = 0; i < normal_length; i++) {
					if ((((guint8)normal[i]) >= 0x40) &&
					    (((guint8)normal[i]) <  0x80)) {
						normal[i] &= (~(0x60));
					}
				}
			}
			_VTE_DEBUG_IF (VTE_DEBUG_EVENTS) {
				if (normal) g_printerr(
						"Keypress, modifiers=0x%x, "
						"keyval=0x%x, cooked string=`%s'.\n",
						modifiers,
						keyval, normal);
			}
		}
		/* If we got normal characters, send them to the child. */
		if (normal != NULL) {
			if (terminal->pvt->meta_sends_escape &&
			    !suppress_meta_esc &&
			    (normal_length > 0) &&
			    (modifiers & VTE_META_MASK)) {
				vte_terminal_feed_child(terminal,
							_VTE_CAP_ESC,
							1);
			}
			if (normal_length > 0) {
				vte_terminal_feed_child_using_modes(terminal,
								    normal,
								    normal_length);
			}
			g_free(normal);
		} else
		/* If the key maps to characters, send them to the child. */
		if (special != NULL && terminal->pvt->termcap != NULL) {
			termcap = terminal->pvt->termcap;
			tterm = terminal->pvt->emulation;
			normal = _vte_termcap_find_string_length(termcap,
								 tterm,
								 special,
								 &normal_length);
			_vte_keymap_key_add_key_modifiers(keyval,
							  modifiers,
							  terminal->pvt->sun_fkey_mode,
							  terminal->pvt->hp_fkey_mode,
							  terminal->pvt->legacy_fkey_mode,
							  terminal->pvt->vt220_fkey_mode,
							  terminal->pvt->cursor_mode == VTE_KEYMODE_APPLICATION,
							  &normal,
							  &normal_length);
			output = g_strdup_printf(normal, 1);
			vte_terminal_feed_child_using_modes(terminal,
							    output, -1);
			g_free(output);
			g_free(normal);
		}
		/* Keep the cursor on-screen. */
		if (!scrolled && !modifier &&
		    terminal->pvt->scroll_on_keystroke) {
			vte_terminal_maybe_scroll_to_bottom(terminal);
		}
		return TRUE;
	}
	return FALSE;
}

static gboolean
vte_terminal_key_release(GtkWidget *widget, GdkEventKey *event)
{
	VteTerminal *terminal;

	terminal = VTE_TERMINAL(widget);

	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

	return gtk_widget_get_realized (&terminal->widget)
			&& gtk_im_context_filter_keypress (terminal->pvt->im_context, event);
}

/**
 * vte_terminal_is_word_char:
 * @terminal: a #VteTerminal
 * @c: a candidate Unicode code point
 *
 * Checks if a particular character is considered to be part of a word or not,
 * based on the values last passed to vte_terminal_set_word_chars().
 *
 * Returns: %TRUE if the character is considered to be part of a word
 */
gboolean
vte_terminal_is_word_char(VteTerminal *terminal, gunichar c)
{
	guint i;
	VteWordCharRange *range;
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);

	if (terminal->pvt->word_chars != NULL) {
		/* Go through each range and check if c is included. */
		for (i = 0; i < terminal->pvt->word_chars->len; i++) {
			range = &g_array_index(terminal->pvt->word_chars,
					       VteWordCharRange,
					       i);
			if ((c >= range->start) && (c <= range->end))
				return TRUE;
		}
	}

	/* If not ASCII, or ASCII and no array set (or empty array),
	 * fall back on Unicode properties. */
	return (c >= 0x80 ||
	        (terminal->pvt->word_chars == NULL) ||
	        (terminal->pvt->word_chars->len == 0)) &&
		g_unichar_isgraph(c) &&
	       !g_unichar_ispunct(c) &&
	       !g_unichar_isspace(c) &&
	       (c != '\0');
}

/* Check if the characters in the two given locations are in the same class
 * (word vs. non-word characters). */
static gboolean
vte_same_class(VteTerminal *terminal, glong acol, glong arow,
	       glong bcol, glong brow)
{
	const VteCell *pcell = NULL;
	gboolean word_char;
	if ((pcell = vte_terminal_find_charcell(terminal, acol, arow)) != NULL && pcell->c != 0) {
		word_char = vte_terminal_is_word_char(terminal, _vte_unistr_get_base (pcell->c));

		/* Lets not group non-wordchars together (bug #25290) */
		if (!word_char)
			return FALSE;

		pcell = vte_terminal_find_charcell(terminal, bcol, brow);
		if (pcell == NULL || pcell->c == 0) {
			return FALSE;
		}
		if (word_char != vte_terminal_is_word_char(terminal, _vte_unistr_get_base (pcell->c))) {
			return FALSE;
		}
		return TRUE;
	}
	return FALSE;
}

/* Check if we soft-wrapped on the given line. */
static gboolean
vte_line_is_wrappable(VteTerminal *terminal, glong row)
{
	const VteRowData *rowdata;
	rowdata = _vte_terminal_find_row_data(terminal, row);
	return rowdata && rowdata->attr.soft_wrapped;
}

/* Check if the given point is in the region between the two points,
 * optionally treating the second point as included in the region or not. */
static gboolean
vte_cell_is_between(glong col, glong row,
		    glong acol, glong arow, glong bcol, glong brow,
		    gboolean inclusive)
{
	/* Negative between never allowed. */
	if ((arow > brow) || ((arow == brow) && (acol > bcol))) {
		return FALSE;
	}
	/* Zero-length between only allowed if we're being inclusive. */
	if ((row == arow) && (row == brow) && (col == acol) && (col == bcol)) {
		return inclusive;
	}
	/* A cell is between two points if it's on a line after the
	 * specified area starts, or before the line where it ends,
	 * or any of the lines in between. */
	if ((row > arow) && (row < brow)) {
		return TRUE;
	}
	/* It's also between the two points if they're on the same row
	 * the cell lies between the start and end columns. */
	if ((row == arow) && (row == brow)) {
		if (col >= acol) {
			if (col < bcol) {
				return TRUE;
			} else {
				if ((col == bcol) && inclusive) {
					return TRUE;
				} else {
					return FALSE;
				}
			}
		} else {
			return FALSE;
		}
	}
	/* It's also "between" if it's on the line where the area starts and
	 * at or after the start column, or on the line where the area ends and
	 * before the end column. */
	if ((row == arow) && (col >= acol)) {
		return TRUE;
	} else {
		if (row == brow) {
			if (col < bcol) {
				return TRUE;
			} else {
				if ((col == bcol) && inclusive) {
					return TRUE;
				} else {
					return FALSE;
				}
			}
		} else {
			return FALSE;
		}
	}
	return FALSE;
}

/* Check if a cell is selected or not. */
static gboolean
vte_cell_is_selected(VteTerminal *terminal, glong col, glong row, gpointer data)
{
	VteVisualPosition ss, se;

	/* If there's nothing selected, it's an easy question to answer. */
	if (!terminal->pvt->has_selection) {
		return FALSE;
	}

	/* If the selection is obviously bogus, then it's also very easy. */
	ss = terminal->pvt->selection_start;
	se = terminal->pvt->selection_end;
	if ((ss.row < 0) || (se.row < 0)) {
		return FALSE;
	}

	/* Limit selection in block mode. */
	if (terminal->pvt->selection_block_mode) {
		if (col < ss.col || col > se.col) {
			return FALSE;
		}
	}

	/* Now it boils down to whether or not the point is between the
	 * begin and endpoint of the selection. */
	return vte_cell_is_between(col, row, ss.col, ss.row, se.col, se.row, TRUE);
}

/* Once we get text data, actually paste it in. */
static void
vte_terminal_paste_cb(GtkClipboard *clipboard, const gchar *text, gpointer data)
{
	VteTerminal *terminal;
	gchar *paste, *p;
	long length;
	terminal = data;
	if (text != NULL) {
		_vte_debug_print(VTE_DEBUG_SELECTION,
				"Pasting %"G_GSIZE_FORMAT" UTF-8 bytes.\n",
				strlen(text));
		if (!g_utf8_validate(text, -1, NULL)) {
			g_warning(_("Error (%s) converting data for child, dropping."), g_strerror(EINVAL));
			return;
		}

		/* Convert newlines to carriage returns, which more software
		 * is able to cope with (cough, pico, cough). */
		paste = g_strdup(text);
		length = strlen(paste);
		p = paste;
		while ((p != NULL) && (p - paste < length)) {
			p = memchr(p, '\n', length - (p - paste));
			if (p != NULL) {
				*p = '\r';
				p++;
			}
		}
		if (terminal->pvt->screen->bracketed_paste_mode)
			vte_terminal_feed_child(terminal, "\e[200~", -1);
		vte_terminal_feed_child(terminal, paste, length);
		if (terminal->pvt->screen->bracketed_paste_mode)
			vte_terminal_feed_child(terminal, "\e[201~", -1);
		g_free(paste);
	}
}

static void
vte_terminal_get_mouse_tracking_info (VteTerminal   *terminal,
				      int            button,
				      long           col,
				      long           row,
				      unsigned char *pb,
				      unsigned char *px,
				      unsigned char *py)
{
	unsigned char cb = 0, cx = 0, cy = 0;

	/* Encode the button information in cb. */
	switch (button) {
	case 0:			/* Release/no buttons. */
		cb = 3;
		break;
	case 1:			/* Left. */
		cb = 0;
		break;
	case 2:			/* Middle. */
		cb = 1;
		break;
	case 3:			/* Right. */
		cb = 2;
		break;
	case 4:
		cb = 64;	/* Scroll up. */
		break;
	case 5:
		cb = 65;	/* Scroll down. */
		break;
	}
	cb += 32; /* 32 for normal */

	/* Encode the modifiers. */
	if (terminal->pvt->modifiers & GDK_SHIFT_MASK) {
		cb |= 4;
	}
	if (terminal->pvt->modifiers & VTE_META_MASK) {
		cb |= 8;
	}
	if (terminal->pvt->modifiers & GDK_CONTROL_MASK) {
		cb |= 16;
	}

	/* Encode the cursor coordinates. */
	cx = 32 + CLAMP(1 + col,
			1, terminal->column_count);
	cy = 32 + CLAMP(1 + row,
			1, terminal->row_count);;

	*pb = cb;
	*px = cx;
	*py = cy;
}

static void
vte_terminal_send_mouse_button_internal(VteTerminal *terminal,
					int          button,
					long         x,
					long         y)
{
	unsigned char cb, cx, cy;
	char buf[LINE_MAX];
	gint len;
	int width = terminal->char_width;
	int height = terminal->char_height;
	long col = (x - terminal->pvt->inner_border.left) / width;
	long row = (y - terminal->pvt->inner_border.top) / height;

	vte_terminal_get_mouse_tracking_info (terminal,
					      button, col, row,
					      &cb, &cx, &cy);

	/* Send event direct to the child, this is binary not text data */
	len = g_snprintf(buf, sizeof(buf), _VTE_CAP_CSI "M%c%c%c", cb, cx, cy);
	vte_terminal_feed_child_binary(terminal, buf, len);
}

/* Send a mouse button click/release notification. */
static void
vte_terminal_maybe_send_mouse_button(VteTerminal *terminal,
				     GdkEventButton *event)
{
	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

	switch (event->type) {
	case GDK_BUTTON_PRESS:
		if (terminal->pvt->mouse_tracking_mode < MOUSE_TRACKING_SEND_XY_ON_CLICK) {
			return;
		}
		break;
	case GDK_BUTTON_RELEASE: {
		if (terminal->pvt->mouse_tracking_mode < MOUSE_TRACKING_SEND_XY_ON_BUTTON) {
			return;
		}
		break;
	}
	default:
		return;
		break;
	}

	vte_terminal_send_mouse_button_internal(terminal,
					        (event->type == GDK_BUTTON_PRESS) ? event->button : 0,
						event->x, event->y);
}

/* Send a mouse motion notification. */
static void
vte_terminal_maybe_send_mouse_drag(VteTerminal *terminal, GdkEventMotion *event)
{
	unsigned char cb, cx, cy;
	char buf[LINE_MAX];
	gint len;
	int width = terminal->char_width;
	int height = terminal->char_height;
	long col = ((long) event->x - terminal->pvt->inner_border.left) / width;
	long row = ((long) event->y - terminal->pvt->inner_border.top) / height;

	/* First determine if we even want to send notification. */
	switch (event->type) {
	case GDK_MOTION_NOTIFY:
		if (terminal->pvt->mouse_tracking_mode < MOUSE_TRACKING_CELL_MOTION_TRACKING)
			return;

		if (terminal->pvt->mouse_tracking_mode < MOUSE_TRACKING_ALL_MOTION_TRACKING) {

			if (terminal->pvt->mouse_last_button == 0) {
				return;
			}
			/* the xterm doc is not clear as to whether
			 * all-tracking also sends degenerate same-cell events */
			if (col == terminal->pvt->mouse_last_x / width &&
			    row == terminal->pvt->mouse_last_y / height)
				return;
		}
		break;
	default:
		return;
		break;
	}

	vte_terminal_get_mouse_tracking_info (terminal,
					      terminal->pvt->mouse_last_button, col, row,
					      &cb, &cx, &cy);
	cb += 32; /* for movement */

	/* Send event direct to the child, this is binary not text data */
	len = g_snprintf(buf, sizeof(buf), _VTE_CAP_CSI "M%c%c%c", cb, cx, cy);
	vte_terminal_feed_child_binary(terminal, buf, len);
}

/* Clear all match hilites. */
static void
vte_terminal_match_hilite_clear(VteTerminal *terminal)
{
	long srow, scolumn, erow, ecolumn;
	srow = terminal->pvt->match_start.row;
	scolumn = terminal->pvt->match_start.col;
	erow = terminal->pvt->match_end.row;
	ecolumn = terminal->pvt->match_end.col;
	terminal->pvt->match_start.row = -1;
	terminal->pvt->match_start.col = -1;
	terminal->pvt->match_end.row = -2;
	terminal->pvt->match_end.col = -2;
	if (terminal->pvt->match_tag != -1) {
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Clearing hilite (%ld,%ld) to (%ld,%ld).\n",
				srow, scolumn, erow, ecolumn);
		_vte_invalidate_region (terminal,
				scolumn, ecolumn, srow, erow, FALSE);
		terminal->pvt->match_tag = -1;
	}
	terminal->pvt->show_match = FALSE;
	if (terminal->pvt->match) {
		g_free (terminal->pvt->match);
		terminal->pvt->match = NULL;
	}
}

static gboolean
cursor_inside_match (VteTerminal *terminal, long x, long y)
{
	gint width = terminal->char_width;
	gint height = terminal->char_height;
	glong col = x / width;
	glong row = y / height + terminal->pvt->screen->scroll_delta;
	if (terminal->pvt->match_start.row == terminal->pvt->match_end.row) {
		return row == terminal->pvt->match_start.row &&
			col >= terminal->pvt->match_start.col &&
			col <= terminal->pvt->match_end.col;
	} else {
		if (row < terminal->pvt->match_start.row ||
				row > terminal->pvt->match_end.row) {
			return FALSE;
		}
		if (row == terminal->pvt->match_start.row) {
			return col >= terminal->pvt->match_start.col;
		}
		if (row == terminal->pvt->match_end.row) {
			return col <= terminal->pvt->match_end.col;
		}
		return TRUE;
	}
}

static void
vte_terminal_match_hilite_show(VteTerminal *terminal, long x, long y)
{
	if(terminal->pvt->match != NULL && !terminal->pvt->show_match){
		if (cursor_inside_match (terminal, x, y)) {
			_vte_invalidate_region (terminal,
					terminal->pvt->match_start.col,
					terminal->pvt->match_end.col,
					terminal->pvt->match_start.row,
					terminal->pvt->match_end.row,
					FALSE);
			terminal->pvt->show_match = TRUE;
		}
	}
}
static void
vte_terminal_match_hilite_hide(VteTerminal *terminal)
{
	if(terminal->pvt->match != NULL && terminal->pvt->show_match){
		_vte_invalidate_region (terminal,
				terminal->pvt->match_start.col,
				terminal->pvt->match_end.col,
				terminal->pvt->match_start.row,
				terminal->pvt->match_end.row,
				FALSE);
		terminal->pvt->show_match = FALSE;
	}
}


static void
vte_terminal_match_hilite_update(VteTerminal *terminal, long x, long y)
{
	int start, end, width, height;
	char *match;
	struct _VteCharAttributes *attr;
	VteScreen *screen;
	long delta;

	width = terminal->char_width;
	height = terminal->char_height;

	/* Check for matches. */
	screen = terminal->pvt->screen;
	delta = screen->scroll_delta;

	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Match hilite update (%ld, %ld) -> %ld, %ld\n",
			x, y,
			x / width,
			y / height + delta);

	match = vte_terminal_match_check_internal(terminal,
						  x / width,
						  y / height + delta,
						  &terminal->pvt->match_tag,
						  &start,
						  &end);
	if (terminal->pvt->show_match) {
		/* Repaint what used to be hilited, if anything. */
		_vte_invalidate_region(terminal,
				terminal->pvt->match_start.col,
				terminal->pvt->match_end.col,
				terminal->pvt->match_start.row,
				terminal->pvt->match_end.row,
				FALSE);
	}

	/* Read the new locations. */
	attr = NULL;
	if ((guint) start < terminal->pvt->match_attributes->len) {
		attr = &g_array_index(terminal->pvt->match_attributes,
				struct _VteCharAttributes,
				start);
		terminal->pvt->match_start.row = attr->row;
		terminal->pvt->match_start.col = attr->column;

		attr = NULL;
		if ((guint) end < terminal->pvt->match_attributes->len) {
			attr = &g_array_index(terminal->pvt->match_attributes,
					struct _VteCharAttributes,
					end);
			terminal->pvt->match_end.row = attr->row;
			terminal->pvt->match_end.col = attr->column;
		}
	}
	if (attr == NULL) { /* i.e. if either endpoint is not found */
		terminal->pvt->match_start.row = -1;
		terminal->pvt->match_start.col = -1;
		terminal->pvt->match_end.row = -2;
		terminal->pvt->match_end.col = -2;
		g_assert (match == NULL);
	}

	g_free (terminal->pvt->match);
	terminal->pvt->match = match;

	/* If there are no matches, repaint what we had matched before. */
	if (match == NULL) {
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"No matches. [(%ld,%ld) to (%ld,%ld)]\n",
				terminal->pvt->match_start.col,
				terminal->pvt->match_start.row,
				terminal->pvt->match_end.col,
				terminal->pvt->match_end.row);
		terminal->pvt->show_match = FALSE;
	} else {
		terminal->pvt->show_match = TRUE;
		/* Repaint the newly-hilited area. */
		_vte_invalidate_region(terminal,
				terminal->pvt->match_start.col,
				terminal->pvt->match_end.col,
				terminal->pvt->match_start.row,
				terminal->pvt->match_end.row,
				FALSE);
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Matched (%ld,%ld) to (%ld,%ld).\n",
				terminal->pvt->match_start.col,
				terminal->pvt->match_start.row,
				terminal->pvt->match_end.col,
				terminal->pvt->match_end.row);
	}
}
/* Update the hilited text if the pointer has moved to a new character cell. */
static void
vte_terminal_match_hilite(VteTerminal *terminal, long x, long y)
{
	int width, height;
	GtkAllocation allocation;

	width = terminal->char_width;
	height = terminal->char_height;

	gtk_widget_get_allocation (&terminal->widget, &allocation);

	/* if the cursor is not above a cell, skip */
	if (x < 0 || x > allocation.width
			|| y < 0 || y > allocation.height) {
		return;
	}

	/* If the pointer hasn't moved to another character cell, then we
	 * need do nothing. */
	if (x / width  == terminal->pvt->mouse_last_x / width &&
	    y / height == terminal->pvt->mouse_last_y / height) {
		terminal->pvt->show_match = terminal->pvt->match != NULL;
		return;
	}

	if (cursor_inside_match (terminal, x, y)) {
		terminal->pvt->show_match = terminal->pvt->match != NULL;
		return;
	}

	vte_terminal_match_hilite_update(terminal, x, y);
}


/* Note that the clipboard has cleared. */
static void
vte_terminal_clear_cb(GtkClipboard *clipboard, gpointer owner)
{
	VteTerminal *terminal;
	terminal = owner;
	if (terminal->pvt->has_selection) {
		_vte_debug_print(VTE_DEBUG_SELECTION, "Lost selection.\n");
		vte_terminal_deselect_all(terminal);
	}
}

/* Supply the selected text to the clipboard. */
static void
vte_terminal_copy_cb(GtkClipboard *clipboard, GtkSelectionData *data,
		     guint info, gpointer owner)
{
	VteTerminal *terminal;
	terminal = owner;
	if (terminal->pvt->selection != NULL) {
		_VTE_DEBUG_IF(VTE_DEBUG_SELECTION) {
			int i;
			g_printerr("Setting selection (%"G_GSIZE_FORMAT" UTF-8 bytes.)\n",
				strlen(terminal->pvt->selection));
			for (i = 0; terminal->pvt->selection[i] != '\0'; i++) {
				g_printerr("0x%04x\n",
					terminal->pvt->selection[i]);
			}
		}
		gtk_selection_data_set_text(data, terminal->pvt->selection, -1);
	}
}

/**
 * VteSelectionFunc:
 * @terminal: terminal in which the cell is.
 * @column: column in which the cell is.
 * @row: row in which the cell is.
 * @data: user data.
 *
 * Specifies the type of a selection function used to check whether
 * a cell has to be selected or not.
 *
 * Returns: %TRUE if cell has to be selected; %FALSE if otherwise.
 */

/**
 * vte_terminal_get_text_range:
 * @terminal: a #VteTerminal
 * @start_row: first row to search for data
 * @start_col: first column to search for data
 * @end_row: last row to search for data
 * @end_col: last column to search for data
 * @is_selected: a #VteSelectionFunc callback
 * @user_data: (closure): user data to be passed to the callback
 * @attributes: (out) (transfer full) (array) (element-type Vte.CharAttributes): location for storing text attributes
 *
 * Extracts a view of the visible part of the terminal.  If @is_selected is not
 * %NULL, characters will only be read if @is_selected returns %TRUE after being
 * passed the column and row, respectively.  A #VteCharAttributes structure
 * is added to @attributes for each byte added to the returned string detailing
 * the character's position, colors, and other characteristics.  The
 * entire scrollback buffer is scanned, so it is possible to read the entire
 * contents of the buffer using this function.
 *
 * Returns: (transfer full): a newly allocated text string, or %NULL.
 */
char *
vte_terminal_get_text_range(VteTerminal *terminal,
			    glong start_row, glong start_col,
			    glong end_row, glong end_col,
			    VteSelectionFunc is_selected,
			    gpointer user_data,
			    GArray *attributes)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return vte_terminal_get_text_range_maybe_wrapped(terminal,
							 start_row, start_col,
							 end_row, end_col,
							 TRUE,
							 is_selected,
							 user_data,
							 attributes,
							 FALSE);
}

static char *
vte_terminal_get_text_range_maybe_wrapped(VteTerminal *terminal,
					  glong start_row, glong start_col,
					  glong end_row, glong end_col,
					  gboolean wrap,
					  VteSelectionFunc is_selected,
					  gpointer data,
					  GArray *attributes,
					  gboolean include_trailing_spaces)
{
	glong col, row, last_empty, last_emptycol, last_nonempty, last_nonemptycol;
	VteScreen *screen;
	const VteCell *pcell = NULL;
	GString *string;
	struct _VteCharAttributes attr;
	PangoColor fore, back, *palette;

	if (!is_selected)
		is_selected = always_selected;

	screen = terminal->pvt->screen;

	if (attributes)
		g_array_set_size (attributes, 0);

	string = g_string_new(NULL);
	memset(&attr, 0, sizeof(attr));

	palette = terminal->pvt->palette;
	col = start_col;
	for (row = start_row; row < end_row + 1; row++, col = 0) {
		const VteRowData *row_data = _vte_terminal_find_row_data (terminal, row);
		last_empty = last_nonempty = string->len;
		last_emptycol = last_nonemptycol = -1;

		attr.row = row;
		attr.column = col;
		pcell = NULL;
		if (row_data != NULL) {
			while ((pcell = _vte_row_data_get (row_data, col))) {

				attr.column = col;

				/* If it's not part of a multi-column character,
				 * and passes the selection criterion, add it to
				 * the selection. */
				if (!pcell->attr.fragment && is_selected(terminal, col, row, data)) {
					/* Store the attributes of this character. */
					fore = palette[pcell->attr.fore];
					back = palette[pcell->attr.back];
					attr.fore.red = fore.red;
					attr.fore.green = fore.green;
					attr.fore.blue = fore.blue;
					attr.back.red = back.red;
					attr.back.green = back.green;
					attr.back.blue = back.blue;
					attr.underline = pcell->attr.underline;
					attr.strikethrough = pcell->attr.strikethrough;

					/* Store the cell string */
					if (pcell->c == 0) {
						g_string_append_c (string, ' ');
						last_empty = string->len;
						last_emptycol = col;
					} else {
						_vte_unistr_append_to_string (pcell->c, string);
						last_nonempty = string->len;
						last_nonemptycol = col;
					}

					/* If we added text to the string, record its
					 * attributes, one per byte. */
					if (attributes) {
						vte_g_array_fill(attributes,
								&attr, string->len);
					}
				}
				/* If we're on the last line, and have just looked in
				 * the last column, stop. */
				if ((row == end_row) && (col >= end_col)) {
					break;
				}

				col++;
			}
		}

	       /* If the last thing we saw was a empty, and we stopped at the
		* right edge of the selected area, trim the trailing spaces
		* off of the line. */
		if (!include_trailing_spaces && last_empty > last_nonempty) {

			col = last_emptycol + 1;

			if (row_data != NULL) {
				while ((pcell = _vte_row_data_get (row_data, col))) {
					col++;

					if (pcell->attr.fragment)
						continue;

					if (pcell->c != 0)
						break;
				}
			}
			if (pcell == NULL) {
				g_string_truncate(string, last_nonempty);
				if (attributes)
					g_array_set_size(attributes, string->len);
				attr.column = last_nonemptycol;
			}
		}

		/* Adjust column, in case we want to append a newline */
		attr.column = MAX(terminal->column_count, attr.column + 1);

		/* Add a newline in block mode. */
		if (terminal->pvt->selection_block_mode) {
			string = g_string_append_c(string, '\n');
		}
		/* Else, if the last visible column on this line was selected and
		 * not soft-wrapped, append a newline. */
		else if (is_selected(terminal, terminal->column_count, row, data)) {
			/* If we didn't softwrap, add a newline. */
			/* XXX need to clear row->soft_wrap on deletion! */
			if (!vte_line_is_wrappable(terminal, row)) {
				string = g_string_append_c(string, '\n');
			}
		}

		/* Make sure that the attributes array is as long as the string. */
		if (attributes) {
			vte_g_array_fill (attributes, &attr, string->len);
		}
	}
	/* Sanity check. */
	g_assert(attributes == NULL || string->len == attributes->len);
	return g_string_free(string, FALSE);
}

static char *
vte_terminal_get_text_maybe_wrapped(VteTerminal *terminal,
				    gboolean wrap,
				    VteSelectionFunc is_selected,
				    gpointer data,
				    GArray *attributes,
				    gboolean include_trailing_spaces)
{
	long start_row, start_col, end_row, end_col;
	start_row = terminal->pvt->screen->scroll_delta;
	start_col = 0;
	end_row = start_row + terminal->row_count - 1;
	end_col = terminal->column_count - 1;
	return vte_terminal_get_text_range_maybe_wrapped(terminal,
							 start_row, start_col,
							 end_row, end_col,
							 wrap,
							 is_selected,
							 data,
							 attributes,
							 include_trailing_spaces);
}

/**
 * vte_terminal_get_text:
 * @terminal: a #VteTerminal
 * @is_selected: a #VteSelectionFunc callback
 * @user_data: (closure): user data to be passed to the callback
 * @attributes: (out) (transfer full) (array) (element-type Vte.CharAttributes): location for storing text attributes
 *
 * Extracts a view of the visible part of the terminal.  If @is_selected is not
 * %NULL, characters will only be read if @is_selected returns %TRUE after being
 * passed the column and row, respectively.  A #VteCharAttributes structure
 * is added to @attributes for each byte added to the returned string detailing
 * the character's position, colors, and other characteristics.
 *
 * Returns: (transfer full): a newly allocated text string, or %NULL.
 */
char *
vte_terminal_get_text(VteTerminal *terminal,
		      VteSelectionFunc is_selected,
		      gpointer user_data,
		      GArray *attributes)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return vte_terminal_get_text_maybe_wrapped(terminal,
						   TRUE,
						   is_selected,
						   user_data,
						   attributes,
						   FALSE);
}

/**
 * vte_terminal_get_text_include_trailing_spaces:
 * @terminal: a #VteTerminal
 * @is_selected: a #VteSelectionFunc callback
 * @user_data: (closure): user data to be passed to the callback
 * @attributes: (out) (transfer full) (array) (element-type Vte.CharAttributes): location for storing text attributes
 *
 * Extracts a view of the visible part of the terminal.  If @is_selected is not
 * %NULL, characters will only be read if @is_selected returns %TRUE after being
 * passed the column and row, respectively.  A #VteCharAttributes structure
 * is added to @attributes for each byte added to the returned string detailing
 * the character's position, colors, and other characteristics. This function
 * differs from vte_terminal_get_text() in that trailing spaces at the end of
 * lines are included.
 *
 * Returns: (transfer full): a newly allocated text string, or %NULL.
 *
 * Since: 0.11.11
 */
char *
vte_terminal_get_text_include_trailing_spaces(VteTerminal *terminal,
					      VteSelectionFunc is_selected,
					      gpointer user_data,
					      GArray *attributes)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return vte_terminal_get_text_maybe_wrapped(terminal,
						   TRUE,
						   is_selected,
						   user_data,
						   attributes,
						   TRUE);
}

/**
 * vte_terminal_get_cursor_position:
 * @terminal: a #VteTerminal
 * @column: (out) (allow-none): a location to store the column, or %NULL
 * @row : (out) (allow-none): a location to store the row, or %NULL
 *
 * Reads the location of the insertion cursor and returns it.  The row
 * coordinate is absolute.
 */
void
vte_terminal_get_cursor_position(VteTerminal *terminal,
				 glong *column, glong *row)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	if (column) {
		*column = terminal->pvt->screen->cursor_current.col;
	}
	if (row) {
		*row = terminal->pvt->screen->cursor_current.row;
	}
}

static GtkClipboard *
vte_terminal_clipboard_get(VteTerminal *terminal, GdkAtom board)
{
	GdkDisplay *display;
	display = gtk_widget_get_display(&terminal->widget);
	return gtk_clipboard_get_for_display(display, board);
}

/* Place the selected text onto the clipboard.  Do this asynchronously so that
 * we get notified when the selection we placed on the clipboard is replaced. */
static void
vte_terminal_copy(VteTerminal *terminal, GdkAtom board)
{
	GtkClipboard *clipboard;
	static GtkTargetEntry *targets = NULL;
	static gint n_targets = 0;

	clipboard = vte_terminal_clipboard_get(terminal, board);

	/* Chuck old selected text and retrieve the newly-selected text. */
	g_free(terminal->pvt->selection);
	terminal->pvt->selection =
		vte_terminal_get_text_range(terminal,
					    terminal->pvt->selection_start.row,
					    0,
					    terminal->pvt->selection_end.row,
					    terminal->column_count,
					    vte_cell_is_selected,
					    NULL,
					    NULL);
	terminal->pvt->has_selection = TRUE;

	/* Place the text on the clipboard. */
	if (terminal->pvt->selection != NULL) {
		_vte_debug_print(VTE_DEBUG_SELECTION,
				"Assuming ownership of selection.\n");
		if (!targets) {
			GtkTargetList *list;

			list = gtk_target_list_new (NULL, 0);
			gtk_target_list_add_text_targets (list, 0);
                        targets = gtk_target_table_new_from_list (list, &n_targets);
			gtk_target_list_unref (list);
		}

		gtk_clipboard_set_with_owner(clipboard,
					     targets,
					     n_targets,
					     vte_terminal_copy_cb,
					     vte_terminal_clear_cb,
					     G_OBJECT(terminal));
		gtk_clipboard_set_can_store(clipboard, NULL, 0);
	}
}

/* Paste from the given clipboard. */
static void
vte_terminal_paste(VteTerminal *terminal, GdkAtom board)
{
	GtkClipboard *clipboard;
	clipboard = vte_terminal_clipboard_get(terminal, board);
	if (clipboard != NULL) {
		_vte_debug_print(VTE_DEBUG_SELECTION,
				"Requesting clipboard contents.\n");
		gtk_clipboard_request_text(clipboard,
					   vte_terminal_paste_cb,
					   terminal);
	}
}

static void
vte_terminal_invalidate_selection (VteTerminal *terminal)
{
	_vte_invalidate_region (terminal,
				terminal->pvt->selection_start.col,
				terminal->pvt->selection_end.col,
				terminal->pvt->selection_start.row,
				terminal->pvt->selection_end.row,
				terminal->pvt->selection_block_mode);
}


/* Start selection at the location of the event. */
static void
vte_terminal_start_selection(VteTerminal *terminal, GdkEventButton *event,
			     enum vte_selection_type selection_type)
{
	long delta;

	terminal->pvt->selection_block_mode = !!(terminal->pvt->modifiers & GDK_CONTROL_MASK);

	if (terminal->pvt->selection_block_mode)
		selection_type = selection_type_char;

	/* Record that we have the selection, and where it started. */
	delta = terminal->pvt->screen->scroll_delta;
	terminal->pvt->has_selection = TRUE;
	terminal->pvt->selection_last.x = event->x - terminal->pvt->inner_border.left;
	terminal->pvt->selection_last.y = event->y - terminal->pvt->inner_border.top +
					  (terminal->char_height * delta);

	/* Decide whether or not to restart on the next drag. */
	switch (selection_type) {
	case selection_type_char:
		/* Restart selection once we register a drag. */
		terminal->pvt->selecting_restart = TRUE;
		terminal->pvt->has_selection = FALSE;
		terminal->pvt->selecting_had_delta = FALSE;

		terminal->pvt->selection_origin = terminal->pvt->selection_last;
		break;
	case selection_type_word:
	case selection_type_line:
		/* Mark the newly-selected areas now. */
		terminal->pvt->selecting_restart = FALSE;
		terminal->pvt->has_selection = FALSE;
		terminal->pvt->selecting_had_delta = FALSE;
		break;
	}

	/* Record the selection type. */
	terminal->pvt->selection_type = selection_type;
	terminal->pvt->selecting = TRUE;

	_vte_debug_print(VTE_DEBUG_SELECTION,
			"Selection started at (%ld,%ld).\n",
			terminal->pvt->selection_start.col,
			terminal->pvt->selection_start.row);

	/* Temporarily stop caring about input from the child. */
	_vte_terminal_disconnect_pty_read(terminal);
}

static gboolean
_vte_terminal_maybe_end_selection (VteTerminal *terminal)
{
	if (terminal->pvt->selecting) {
		/* Copy only if something was selected. */
		if (terminal->pvt->has_selection &&
		    !terminal->pvt->selecting_restart &&
		    terminal->pvt->selecting_had_delta) {
			vte_terminal_copy_primary(terminal);
			vte_terminal_emit_selection_changed(terminal);
		}
		terminal->pvt->selecting = FALSE;

		/* Reconnect to input from the child if we paused it. */
		_vte_terminal_connect_pty_read(terminal);

		return TRUE;
	}
	return FALSE;
}

static long
math_div (long a, long b)
{
	if (G_LIKELY (a >= 0))
		return a / b;
	else
		return (a / b) - 1;
}

/* Helper */
static void
vte_terminal_extend_selection_expand (VteTerminal *terminal)
{
	long i, j;
	VteScreen *screen;
	const VteRowData *rowdata;
	const VteCell *cell;
	VteVisualPosition *sc, *ec;

	if (terminal->pvt->selection_block_mode)
		return;

	screen = terminal->pvt->screen;
	sc = &terminal->pvt->selection_start;
	ec = &terminal->pvt->selection_end;

	/* Extend the selection to handle end-of-line cases, word, and line
	 * selection.  We do this here because calculating it once is cheaper
	 * than recalculating for each cell as we render it. */

	/* Handle end-of-line at the start-cell. */
	rowdata = _vte_terminal_find_row_data(terminal, sc->row);
	if (rowdata != NULL) {
		/* Find the last non-empty character on the first line. */
		for (i = _vte_row_data_length (rowdata); i > 0; i--) {
			cell = _vte_row_data_get (rowdata, i - 1);
			if (cell->attr.fragment || cell->c != 0)
				break;
		}
		/* If the start point is to its right, then move the
		 * startpoint up to the beginning of the next line
		 * unless that would move the startpoint after the end
		 * point, or we're in select-by-line mode. */
		if ((sc->col >= i) &&
				(terminal->pvt->selection_type != selection_type_line)) {
			if (sc->row < ec->row) {
				sc->col = 0;
				sc->row++;
			} else {
				sc->col = i;
			}
		}
	} else {
		/* Snap to the leftmost column. */
		sc->col = 0;
	}
	sc->col = find_start_column (terminal, sc->col, sc->row);

	/* Handle end-of-line at the end-cell. */
	rowdata = _vte_terminal_find_row_data(terminal, ec->row);
	if (rowdata != NULL) {
		/* Find the last non-empty character on the last line. */
		for (i = _vte_row_data_length (rowdata); i > 0; i--) {
			cell = _vte_row_data_get (rowdata, i - 1);
			if (cell->attr.fragment || cell->c != 0)
				break;
		}
		/* If the end point is to its right, then extend the
		 * endpoint as far right as we can expect. */
		if (ec->col >= i) {
			ec->col = MAX(ec->col,
				    MAX(terminal->column_count,
					(long) _vte_row_data_length (rowdata)));
		}
	} else {
		/* Snap to the rightmost column, only if selecting anything of
		 * this row. */
		if (ec->col >= 0)
			ec->col = MAX(ec->col, terminal->column_count);
	}
	ec->col = find_end_column (terminal, ec->col, ec->row);


	/* Now extend again based on selection type. */
	switch (terminal->pvt->selection_type) {
	case selection_type_char:
		/* Nothing more to do. */
		break;
	case selection_type_word:
		/* Keep selecting to the left as long as the next character we
		 * look at is of the same class as the current start point. */
		i = sc->col;
		j = sc->row;
		while (_vte_ring_contains (screen->row_data, j)) {
			/* Get the data for the row we're looking at. */
			rowdata = _vte_ring_index(screen->row_data, j);
			if (rowdata == NULL) {
				break;
			}
			/* Back up. */
			for (i = (j == sc->row) ?
				 sc->col :
				 terminal->column_count;
			     i > 0;
			     i--) {
				if (vte_same_class(terminal,
						   i - 1,
						   j,
						   i,
						   j)) {
					sc->col = i - 1;
					sc->row = j;
				} else {
					break;
				}
			}
			if (i > 0) {
				/* We hit a stopping point, so stop. */
				break;
			} else {
				if (vte_line_is_wrappable(terminal, j - 1) &&
				    vte_same_class(terminal,
						   terminal->column_count - 1,
						   j - 1,
						   0,
						   j)) {
					/* Move on to the previous line. */
					j--;
					sc->col = terminal->column_count - 1;
					sc->row = j;
				} else {
					break;
				}
			}
		}
		/* Keep selecting to the right as long as the next character we
		 * look at is of the same class as the current end point. */
		i = ec->col;
		j = ec->row;
		while (_vte_ring_contains (screen->row_data, j)) {
			/* Get the data for the row we're looking at. */
			rowdata = _vte_ring_index(screen->row_data, j);
			if (rowdata == NULL) {
				break;
			}
			/* Move forward. */
			for (i = (j == ec->row) ?
				 ec->col :
				 0;
			     i < terminal->column_count - 1;
			     i++) {
				if (vte_same_class(terminal,
						   i,
						   j,
						   i + 1,
						   j)) {
					ec->col = i + 1;
					ec->row = j;
				} else {
					break;
				}
			}
			if (i < terminal->column_count - 1) {
				/* We hit a stopping point, so stop. */
				break;
			} else {
				if (vte_line_is_wrappable(terminal, j) &&
				    vte_same_class(terminal,
						   terminal->column_count - 1,
						   j,
						   0,
						   j + 1)) {
					/* Move on to the next line. */
					j++;
					ec->col = 0;
					ec->row = j;
				} else {
					break;
				}
			}
		}
		break;
	case selection_type_line:
		/* Extend the selection to the beginning of the start line. */
		sc->col = 0;
		/* Now back up as far as we can go. */
		j = sc->row;
		while (_vte_ring_contains (screen->row_data, j - 1) &&
		       vte_line_is_wrappable(terminal, j - 1)) {
			j--;
			sc->row = j;
		}
		/* And move forward as far as we can go. */
		j = ec->row;
		while (_vte_ring_contains (screen->row_data, j) &&
		       vte_line_is_wrappable(terminal, j)) {
			j++;
			ec->row = j;
		}
		/* Make sure we include all of the last line. */
		ec->col = terminal->column_count;
		if (_vte_ring_contains (screen->row_data, ec->row)) {
			rowdata = _vte_ring_index(screen->row_data, ec->row);
			if (rowdata != NULL) {
				ec->col = MAX(ec->col, (long) _vte_row_data_length (rowdata));
			}
		}
		break;
	}
}

/* Extend selection to include the given event coordinates. */
static void
vte_terminal_extend_selection(VteTerminal *terminal, long x, long y,
			      gboolean always_grow, gboolean force)
{
	VteScreen *screen;
	int width, height;
	long delta, residual;
	struct selection_event_coords *origin, *last, *start, *end;
	VteVisualPosition old_start, old_end, *sc, *ec, *so, *eo;
	gboolean invalidate_selected = FALSE;
	gboolean had_selection;

	height = terminal->char_height;
	width = terminal->char_width;

	/* Confine y into the visible area. (#563024) */
	if (y < 0) {
		y = 0;
		if (!terminal->pvt->selection_block_mode)
			x = 0;
	} else if (y >= terminal->row_count * height) {
		if (!terminal->pvt->selection_block_mode) {
			y = terminal->row_count * height;
			x = -1;
		} else {
			y = terminal->row_count * height - 1;
		}
	}

	screen = terminal->pvt->screen;
	old_start = terminal->pvt->selection_start;
	old_end = terminal->pvt->selection_end;
	so = &old_start;
	eo = &old_end;

	/* Convert the event coordinates to cell coordinates. */
	delta = screen->scroll_delta;

	/* If we're restarting on a drag, then mark this as the start of
	 * the selected block. */
	if (terminal->pvt->selecting_restart) {
		vte_terminal_deselect_all(terminal);
		invalidate_selected = TRUE;
		_vte_debug_print(VTE_DEBUG_SELECTION,
				"Selection delayed start at (%ld,%ld).\n",
				terminal->pvt->selection_origin.x / width,
				terminal->pvt->selection_origin.y / height);
	}

	/* Recognize that we've got a selected block. */
	had_selection = terminal->pvt->has_selection;
	terminal->pvt->has_selection = TRUE;
	terminal->pvt->selecting_had_delta = TRUE;
	terminal->pvt->selecting_restart = FALSE;

	/* If we're not in always-grow mode, update the last location of
	 * the selection. */
	last = &terminal->pvt->selection_last;

	/* Map the origin and last selected points to a start and end. */
	origin = &terminal->pvt->selection_origin;
	if (terminal->pvt->selection_block_mode) {
		last->x = x;
		last->y = y + height * delta;

		/* We don't support always_grow in block mode */
		if (always_grow)
			vte_terminal_invalidate_selection (terminal);

		if (origin->y <= last->y) {
			/* The origin point is "before" the last point. */
			start = origin;
			end = last;
		} else {
			/* The last point is "before" the origin point. */
			start = last;
			end = origin;
		}
	} else {
		if (!always_grow) {
			last->x = x;
			last->y = y + height * delta;
		}

		if ((origin->y / height < last->y / height) ||
		    ((origin->y / height == last->y / height) &&
		     (origin->x / width < last->x / width ))) {
			/* The origin point is "before" the last point. */
			start = origin;
			end = last;
		} else {
			/* The last point is "before" the origin point. */
			start = last;
			end = origin;
		}

		/* Extend the selection by moving whichever end of the selection is
		 * closer to the new point. */
		if (always_grow) {
			/* New endpoint is before existing selection. */
			if ((y / height < ((start->y / height) - delta)) ||
			    ((y / height == ((start->y / height) - delta)) &&
			     (x / width < start->x / width))) {
				start->x = x;
				start->y = y + height * delta;
			} else {
				/* New endpoint is after existing selection. */
				end->x = x;
				end->y = y + height * delta;
			}
		}
	}

#if 0
	_vte_debug_print(VTE_DEBUG_SELECTION,
			"Selection is (%ld,%ld) to (%ld,%ld).\n",
			start->x, start->y, end->x, end->y);
#endif

	/* Recalculate the selection area in terms of cell positions. */

	sc = &terminal->pvt->selection_start;
	ec = &terminal->pvt->selection_end;

	sc->row = MAX (0, start->y / height);
	ec->row = MAX (0, end->y   / height);

	/* Sort x using row cell coordinates */
	if ((terminal->pvt->selection_block_mode || sc->row == ec->row) && (start->x > end->x)) {
		struct selection_event_coords *tmp;
		tmp = start;
		start = end;
		end = tmp;
	}

	/* We want to be more lenient on the user with their column selection.
	 * We round to the closest logical position (positions are located between
	 * cells).  But we don't want to fully round.  So we divide the cell
	 * width into three parts.  The side parts round to their nearest
	 * position.  The middle part is always inclusive in the selection.
	 *
	 * math_div and no MAX, to allow selecting no cells in the line,
	 * ie. ec->col = -1, which is essentially equal to copying the
	 * newline from previous line but no chars from current line. */
	residual = (width + 1) / 3;
	sc->col = math_div (start->x + residual, width);
	ec->col = math_div (end->x - residual, width);


	vte_terminal_extend_selection_expand (terminal);

	if (!invalidate_selected && !force &&
	    0 == memcmp (sc, so, sizeof (*sc)) &&
	    0 == memcmp (ec, eo, sizeof (*ec)))
		/* No change */
		return;

	/* Invalidate */

	if (had_selection) {

		if (terminal->pvt->selection_block_mode) {
			/* Update the selection area diff in block mode. */

			/* The top band */
			_vte_invalidate_region (terminal,
						MIN(sc->col, so->col),
						MAX(ec->col, eo->col),
						MIN(sc->row, so->row),
						MAX(sc->row, so->row) - 1,
						TRUE);
			/* The bottom band */
			_vte_invalidate_region (terminal,
						MIN(sc->col, so->col),
						MAX(ec->col, eo->col),
						MIN(ec->row, eo->row) + 1,
						MAX(ec->row, eo->row),
						TRUE);
			/* The left band */
			_vte_invalidate_region (terminal,
						MIN(sc->col, so->col),
						MAX(sc->col, so->col) - 1,
						MIN(sc->row, so->row),
						MAX(ec->row, eo->row),
						TRUE);
			/* The right band */
			_vte_invalidate_region (terminal,
						MIN(ec->col, eo->col) + 1,
						MAX(ec->col, eo->col),
						MIN(sc->row, so->row),
						MAX(ec->row, eo->row),
						TRUE);
		} else {
			/* Update the selection area diff in non-block mode. */

			/* The before band */
			if (sc->row < so->row)
				_vte_invalidate_region (terminal,
							sc->col, so->col - 1,
							sc->row, so->row,
							FALSE);
			else if (sc->row > so->row)
				_vte_invalidate_region (terminal,
							so->col, sc->col - 1,
							so->row, sc->row,
							FALSE);
			else
				_vte_invalidate_region (terminal,
							MIN(sc->col, so->col), MAX(sc->col, so->col) - 1,
							sc->row, sc->row,
							TRUE);

			/* The after band */
			if (ec->row < eo->row)
				_vte_invalidate_region (terminal,
							ec->col + 1, eo->col,
							ec->row, eo->row,
							FALSE);
			else if (ec->row > eo->row)
				_vte_invalidate_region (terminal,
							eo->col + 1, ec->col,
							eo->row, ec->row,
							FALSE);
			else
				_vte_invalidate_region (terminal,
							MIN(ec->col, eo->col) + 1, MAX(ec->col, eo->col),
							ec->row, ec->row,
							TRUE);
		}
	}

	if (invalidate_selected || !had_selection) {
		_vte_debug_print(VTE_DEBUG_SELECTION, "Invalidating selection.");
		vte_terminal_invalidate_selection (terminal);
	}

	_vte_debug_print(VTE_DEBUG_SELECTION,
			"Selection changed to "
			"(%ld,%ld) to (%ld,%ld).\n",
			sc->col, sc->row, ec->col, ec->row);
}

/**
 * vte_terminal_select_all:
 * @terminal: a #VteTerminal
 *
 * Selects all text within the terminal (including the scrollback buffer).
 *
 * Since: 0.16
 */
void
vte_terminal_select_all (VteTerminal *terminal)
{
	g_return_if_fail (VTE_IS_TERMINAL (terminal));

	vte_terminal_deselect_all (terminal);

	terminal->pvt->has_selection = TRUE;
	terminal->pvt->selecting_had_delta = TRUE;
	terminal->pvt->selecting_restart = FALSE;

	terminal->pvt->selection_start.row = _vte_ring_delta (terminal->pvt->screen->row_data);
	terminal->pvt->selection_start.col = 0;
	terminal->pvt->selection_end.row = _vte_ring_next (terminal->pvt->screen->row_data);
	terminal->pvt->selection_end.col = -1;

	_vte_debug_print(VTE_DEBUG_SELECTION, "Selecting *all* text.\n");

	vte_terminal_copy_primary(terminal);
	vte_terminal_emit_selection_changed (terminal);
	_vte_invalidate_all (terminal);
}

/**
 * vte_terminal_select_none:
 * @terminal: a #VteTerminal
 *
 * Clears the current selection.
 *
 * Since: 0.16
 */
void
vte_terminal_select_none (VteTerminal *terminal)
{
	g_return_if_fail (VTE_IS_TERMINAL (terminal));

	_vte_debug_print(VTE_DEBUG_SELECTION, "Clearing selection.\n");

	vte_terminal_deselect_all (terminal);
}



/* Autoscroll a bit. */
static gboolean
vte_terminal_autoscroll(VteTerminal *terminal)
{
	gboolean extend = FALSE;
	long x, y, xmax, ymax;
	glong adj;

	/* Provide an immediate effect for mouse wigglers. */
	if (terminal->pvt->mouse_last_y < 0) {
		if (terminal->adjustment) {
			/* Try to scroll up by one line. */
			adj = terminal->pvt->screen->scroll_delta - 1;
			vte_terminal_queue_adjustment_value_changed_clamped (terminal, adj);
			extend = TRUE;
		}
		_vte_debug_print(VTE_DEBUG_EVENTS, "Autoscrolling down.\n");
	}
	if (terminal->pvt->mouse_last_y >=
	    terminal->row_count * terminal->char_height) {
		if (terminal->adjustment) {
			/* Try to scroll up by one line. */
			adj = terminal->pvt->screen->scroll_delta + 1;
			vte_terminal_queue_adjustment_value_changed_clamped (terminal, adj);
			extend = TRUE;
		}
		_vte_debug_print(VTE_DEBUG_EVENTS, "Autoscrolling up.\n");
	}
	if (extend) {
		/* Don't select off-screen areas.  That just confuses people. */
		xmax = terminal->column_count * terminal->char_width;
		ymax = terminal->row_count * terminal->char_height;

		x = CLAMP(terminal->pvt->mouse_last_x, 0, xmax);
		y = CLAMP(terminal->pvt->mouse_last_y, 0, ymax);
		/* If we clamped the Y, mess with the X to get the entire
		 * lines. */
		if (terminal->pvt->mouse_last_y < 0 && !terminal->pvt->selection_block_mode) {
			x = 0;
		}
		if (terminal->pvt->mouse_last_y >= ymax && !terminal->pvt->selection_block_mode) {
			x = terminal->column_count * terminal->char_width;
		}
		/* Extend selection to cover the newly-scrolled area. */
		vte_terminal_extend_selection(terminal, x, y, FALSE, TRUE);
	} else {
		/* Stop autoscrolling. */
		terminal->pvt->mouse_autoscroll_tag = 0;
	}
	return (terminal->pvt->mouse_autoscroll_tag != 0);
}

/* Start autoscroll. */
static void
vte_terminal_start_autoscroll(VteTerminal *terminal)
{
	if (terminal->pvt->mouse_autoscroll_tag == 0) {
		terminal->pvt->mouse_autoscroll_tag =
			g_timeout_add_full(G_PRIORITY_LOW,
					   666 / terminal->row_count,
					   (GSourceFunc)vte_terminal_autoscroll,
					   terminal,
					   NULL);
	}
}

/* Stop autoscroll. */
static void
vte_terminal_stop_autoscroll(VteTerminal *terminal)
{
	if (terminal->pvt->mouse_autoscroll_tag != 0) {
		g_source_remove(terminal->pvt->mouse_autoscroll_tag);
		terminal->pvt->mouse_autoscroll_tag = 0;
	}
}

/* Read and handle a motion event. */
static gboolean
vte_terminal_motion_notify(GtkWidget *widget, GdkEventMotion *event)
{
	VteTerminal *terminal;
	int width, height;
	long x, y;
	gboolean handled = FALSE;

	/* check to see if it matters */
	if (! gtk_widget_is_drawable (widget)) {
		return handled;
	}

	terminal = VTE_TERMINAL(widget);
	x = event->x - terminal->pvt->inner_border.left;
	y = event->y - terminal->pvt->inner_border.top;
	width = terminal->char_width;
	height = terminal->char_height;

	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Motion notify (%ld,%ld) [%ld, %ld].\n",
			x, y,
			x / width, y / height + terminal->pvt->screen->scroll_delta);

	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

	if (terminal->pvt->mouse_last_button) {
		vte_terminal_match_hilite_hide (terminal);
	} else {
		/* Hilite any matches. */
		vte_terminal_match_hilite(terminal, x, y);
		/* Show the cursor. */
		_vte_terminal_set_pointer_visible(terminal, TRUE);
	}

	switch (event->type) {
	case GDK_MOTION_NOTIFY:
		if (terminal->pvt->selecting &&
		    ((terminal->pvt->modifiers & GDK_SHIFT_MASK) ||
		      !terminal->pvt->mouse_tracking_mode))
		{
			_vte_debug_print(VTE_DEBUG_EVENTS, "Mousing drag 1.\n");
			vte_terminal_extend_selection(terminal,
						      x, y, FALSE, FALSE);

			/* Start scrolling if we need to. */
			if (event->y < terminal->pvt->inner_border.top ||
			    event->y >= terminal->row_count * height +
                                        terminal->pvt->inner_border.top)
			{
				/* Give mouse wigglers something. */
				vte_terminal_autoscroll(terminal);
				/* Start a timed autoscroll if we're not doing it
				 * already. */
				vte_terminal_start_autoscroll(terminal);
			}

			handled = TRUE;
		}

		if (!handled)
			vte_terminal_maybe_send_mouse_drag(terminal, event);
		break;
	default:
		break;
	}

	/* Save the pointer coordinates for later use. */
	terminal->pvt->mouse_last_x = x;
	terminal->pvt->mouse_last_y = y;

	return handled;
}

/* Read and handle a pointing device buttonpress event. */
static gint
vte_terminal_button_press(GtkWidget *widget, GdkEventButton *event)
{
	VteTerminal *terminal;
	long height, width, delta;
	gboolean handled = FALSE;
	gboolean start_selecting = FALSE, extend_selecting = FALSE;
	long cellx, celly;
	long x,y;

	terminal = VTE_TERMINAL(widget);

	x = event->x - terminal->pvt->inner_border.left;
	y = event->y - terminal->pvt->inner_border.top;

	height = terminal->char_height;
	width = terminal->char_width;
	delta = terminal->pvt->screen->scroll_delta;

	vte_terminal_match_hilite(terminal, x, y);

	_vte_terminal_set_pointer_visible(terminal, TRUE);

	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

	/* Convert the event coordinates to cell coordinates. */
	cellx = x / width;
	celly = y / height + delta;

	switch (event->type) {
	case GDK_BUTTON_PRESS:
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Button %d single-click at (%ld,%ld)\n",
				event->button,
				x, y + terminal->char_height * delta);
		/* Handle this event ourselves. */
		switch (event->button) {
		case 1:
			_vte_debug_print(VTE_DEBUG_EVENTS,
					"Handling click ourselves.\n");
			/* Grab focus. */
			if (! gtk_widget_has_focus (widget)) {
				gtk_widget_grab_focus(widget);
			}

			/* If we're in event mode, and the user held down the
			 * shift key, we start selecting. */
			if (terminal->pvt->mouse_tracking_mode) {
				if (terminal->pvt->modifiers & GDK_SHIFT_MASK) {
					start_selecting = TRUE;
				}
			} else {
				/* If the user hit shift, then extend the
				 * selection instead. */
				if ((terminal->pvt->modifiers & GDK_SHIFT_MASK) &&
				    (terminal->pvt->has_selection ||
				     terminal->pvt->selecting_restart) &&
				    !vte_cell_is_selected(terminal,
							  cellx,
							  celly,
							  NULL)) {
					extend_selecting = TRUE;
				} else {
					start_selecting = TRUE;
				}
			}
			if (start_selecting) {
				vte_terminal_deselect_all(terminal);
				vte_terminal_start_selection(terminal,
							     event,
							     selection_type_char);
				handled = TRUE;
			}
			if (extend_selecting) {
				vte_terminal_extend_selection(terminal,
							      x, y,
							      !terminal->pvt->selecting_restart, TRUE);
				/* The whole selection code needs to be
				 * rewritten.  For now, put this here to
				 * fix bug 614658 */
				terminal->pvt->selecting = TRUE;
				handled = TRUE;
			}
			break;
		/* Paste if the user pressed shift or we're not sending events
		 * to the app. */
		case 2:
			if ((terminal->pvt->modifiers & GDK_SHIFT_MASK) ||
			    !terminal->pvt->mouse_tracking_mode) {
				vte_terminal_paste_primary(terminal);
				handled = TRUE;
			}
			break;
		case 3:
		default:
			break;
		}
		/* If we haven't done anything yet, try sending the mouse
		 * event to the app. */
		if (handled == FALSE) {
			vte_terminal_maybe_send_mouse_button(terminal, event);
			handled = TRUE;
		}
		break;
	case GDK_2BUTTON_PRESS:
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Button %d double-click at (%ld,%ld)\n",
				event->button,
				x, y + (terminal->char_height * delta));
		switch (event->button) {
		case 1:
			if ((terminal->pvt->modifiers & GDK_SHIFT_MASK) ||
			    !terminal->pvt->mouse_tracking_mode) {
				vte_terminal_start_selection(terminal,
							     event,
							     selection_type_word);
				vte_terminal_extend_selection(terminal,
							      x, y, FALSE, TRUE);
			}
			break;
		case 2:
		case 3:
		default:
			break;
		}
		break;
	case GDK_3BUTTON_PRESS:
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Button %d triple-click at (%ld,%ld).\n",
				event->button,
				x, y + (terminal->char_height * delta));
		switch (event->button) {
		case 1:
			if ((terminal->pvt->modifiers & GDK_SHIFT_MASK) ||
			    !terminal->pvt->mouse_tracking_mode) {
				vte_terminal_start_selection(terminal,
							     event,
							     selection_type_line);
				vte_terminal_extend_selection(terminal,
							      x, y, FALSE, TRUE);
			}
			break;
		case 2:
		case 3:
		default:
			break;
		}
	default:
		break;
	}

	/* Save the pointer state for later use. */
	terminal->pvt->mouse_last_button = event->button;
	terminal->pvt->mouse_last_x = x;
	terminal->pvt->mouse_last_y = y;

	return TRUE;
}

/* Read and handle a pointing device buttonrelease event. */
static gint
vte_terminal_button_release(GtkWidget *widget, GdkEventButton *event)
{
	VteTerminal *terminal;
	gboolean handled = FALSE;
	int x, y;

	terminal = VTE_TERMINAL(widget);

	x = event->x - terminal->pvt->inner_border.left;
	y = event->y - terminal->pvt->inner_border.top;

	vte_terminal_match_hilite(terminal, x, y);

	_vte_terminal_set_pointer_visible(terminal, TRUE);

	vte_terminal_stop_autoscroll(terminal);

	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

	switch (event->type) {
	case GDK_BUTTON_RELEASE:
		_vte_debug_print(VTE_DEBUG_EVENTS,
				"Button %d released at (%d,%d).\n",
				event->button, x, y);
		switch (event->button) {
		case 1:
			/* If Shift is held down, or we're not in events mode,
			 * copy the selected text. */
			if ((terminal->pvt->modifiers & GDK_SHIFT_MASK) ||
			    !terminal->pvt->mouse_tracking_mode)
				handled = _vte_terminal_maybe_end_selection (terminal);
			break;
		case 2:
			if ((terminal->pvt->modifiers & GDK_SHIFT_MASK) ||
			    !terminal->pvt->mouse_tracking_mode) {
				handled = TRUE;
			}
			break;
		case 3:
		default:
			break;
		}
		if (!handled) {
			vte_terminal_maybe_send_mouse_button(terminal, event);
			handled = TRUE;
		}
		break;
	default:
		break;
	}

	/* Save the pointer state for later use. */
	terminal->pvt->mouse_last_button = 0;
	terminal->pvt->mouse_last_x = x;
	terminal->pvt->mouse_last_y = y;

	return TRUE;
}

/* Handle receiving or losing focus. */
static gboolean
vte_terminal_focus_in(GtkWidget *widget, GdkEventFocus *event)
{
	VteTerminal *terminal;

	_vte_debug_print(VTE_DEBUG_EVENTS, "Focus in.\n");

	terminal = VTE_TERMINAL(widget);
	gtk_widget_grab_focus (widget);

	/* Read the keyboard modifiers, though they're probably garbage. */
	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

	/* We only have an IM context when we're realized, and there's not much
	 * point to painting the cursor if we don't have a window. */
	if (gtk_widget_get_realized (widget)) {
		terminal->pvt->cursor_blink_state = TRUE;
		terminal->pvt->has_focus = TRUE;

		_vte_check_cursor_blink (terminal);

		gtk_im_context_focus_in(terminal->pvt->im_context);
		_vte_invalidate_cursor_once(terminal, FALSE);
		_vte_terminal_set_pointer_visible(terminal, TRUE);
	}

	return FALSE;
}

static gboolean
vte_terminal_focus_out(GtkWidget *widget, GdkEventFocus *event)
{
	VteTerminal *terminal;
	_vte_debug_print(VTE_DEBUG_EVENTS, "Focus out.\n");
	terminal = VTE_TERMINAL(widget);
	/* Read the keyboard modifiers, though they're probably garbage. */
	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);
	/* We only have an IM context when we're realized, and there's not much
	 * point to painting ourselves if we don't have a window. */
	if (gtk_widget_get_realized (widget)) {
		_vte_terminal_maybe_end_selection (terminal);

		gtk_im_context_focus_out(terminal->pvt->im_context);
		_vte_invalidate_cursor_once(terminal, FALSE);

		/* XXX Do we want to hide the match just because the terminal
		 * lost keyboard focus, but the pointer *is* still within our
		 * area top? */
		vte_terminal_match_hilite_hide (terminal);
		/* Mark the cursor as invisible to disable hilite updating */
		terminal->pvt->mouse_cursor_visible = FALSE;
	}

	terminal->pvt->has_focus = FALSE;
	_vte_check_cursor_blink (terminal);

	return FALSE;
}

static gboolean
vte_terminal_enter(GtkWidget *widget, GdkEventCrossing *event)
{
	gboolean ret = FALSE;
	_vte_debug_print(VTE_DEBUG_EVENTS, "Enter.\n");
	if (GTK_WIDGET_CLASS (vte_terminal_parent_class)->enter_notify_event) {
		ret = GTK_WIDGET_CLASS (vte_terminal_parent_class)->enter_notify_event (widget, event);
	}
	if (gtk_widget_get_realized (widget)) {
		VteTerminal *terminal = VTE_TERMINAL (widget);
		/* Hilite any matches. */
		vte_terminal_match_hilite_show(terminal,
					       event->x - terminal->pvt->inner_border.left,
					       event->y - terminal->pvt->inner_border.top);
	}
	return ret;
}
static gboolean
vte_terminal_leave(GtkWidget *widget, GdkEventCrossing *event)
{
	gboolean ret = FALSE;
	_vte_debug_print(VTE_DEBUG_EVENTS, "Leave.\n");
	if (GTK_WIDGET_CLASS (vte_terminal_parent_class)->leave_notify_event) {
		ret = GTK_WIDGET_CLASS (vte_terminal_parent_class)->leave_notify_event (widget, event);
	}
	if (gtk_widget_get_realized (widget)) {
		VteTerminal *terminal = VTE_TERMINAL (widget);
		vte_terminal_match_hilite_hide (terminal);
		/* Mark the cursor as invisible to disable hilite updating,
		 * whilst the cursor is absent (otherwise we copy the entire
		 * buffer after each update for nothing...)
		 */
		terminal->pvt->mouse_cursor_visible = FALSE;
	}
	return ret;
}

static G_GNUC_UNUSED inline const char *
visibility_state_str(GdkVisibilityState state)
{
	switch(state){
		case GDK_VISIBILITY_FULLY_OBSCURED:
			return "fully-obscured";
		case GDK_VISIBILITY_UNOBSCURED:
			return "unobscured";
		default:
			return "partial";
	}
}

static void
vte_terminal_set_visibility (VteTerminal *terminal, GdkVisibilityState state)
{
	_vte_debug_print(VTE_DEBUG_MISC, "change visibility: %s -> %s.\n",
			visibility_state_str(terminal->pvt->visibility_state),
			visibility_state_str(state));

	if (state == terminal->pvt->visibility_state) {
		return;
	}

	/* fully obscured to visible switch, force the fast path */
	if (terminal->pvt->visibility_state == GDK_VISIBILITY_FULLY_OBSCURED) {
		/* set invalidated_all false, since we didn't really mean it
		 * when we set it to TRUE when becoming obscured */
		terminal->pvt->invalidated_all = FALSE;

		/* if all unobscured now, invalidate all, otherwise, wait
		 * for the expose event */
		if (state == GDK_VISIBILITY_UNOBSCURED) {
			_vte_invalidate_all (terminal);
		}
	}

	terminal->pvt->visibility_state = state;

	/* no longer visible, stop processing display updates */
	if (terminal->pvt->visibility_state == GDK_VISIBILITY_FULLY_OBSCURED) {
		remove_update_timeout (terminal);
		/* if fully obscured, just act like we have invalidated all,
		 * so no updates are accumulated. */
		terminal->pvt->invalidated_all = TRUE;
	}
}

static gboolean
vte_terminal_visibility_notify(GtkWidget *widget, GdkEventVisibility *event)
{
	VteTerminal *terminal;
	terminal = VTE_TERMINAL(widget);

	_vte_debug_print(VTE_DEBUG_EVENTS, "Visibility (%s -> %s).\n",
			visibility_state_str(terminal->pvt->visibility_state),
			visibility_state_str(event->state));
	vte_terminal_set_visibility(terminal, event->state);

	return FALSE;
}

/* Apply the changed metrics, and queue a resize if need be. */
static void
vte_terminal_apply_metrics(VteTerminal *terminal,
			   gint width, gint height, gint ascent, gint descent)
{
	gboolean resize = FALSE, cresize = FALSE;
	gint line_thickness;

	/* Sanity check for broken font changes. */
	width = MAX(width, 1);
	height = MAX(height, 2);
	ascent = MAX(ascent, 1);
	descent = MAX(descent, 1);

	/* Change settings, and keep track of when we've changed anything. */
	if (width != terminal->char_width) {
		resize = cresize = TRUE;
		terminal->char_width = width;
	}
	if (height != terminal->char_height) {
		resize = cresize = TRUE;
		terminal->char_height = height;
	}
	if (ascent != terminal->char_ascent) {
		resize = TRUE;
		terminal->char_ascent = ascent;
	}
	if (descent != terminal->char_descent) {
		resize = TRUE;
		terminal->char_descent = descent;
	}
	terminal->pvt->line_thickness = line_thickness = MAX (MIN ((height - ascent) / 2, height / 14), 1);
	terminal->pvt->underline_position = MIN (ascent + line_thickness, height - line_thickness);
	terminal->pvt->strikethrough_position =  ascent - height / 4;

	/* Queue a resize if anything's changed. */
	if (resize) {
		if (gtk_widget_get_realized (&terminal->widget)) {
			gtk_widget_queue_resize_no_redraw(&terminal->widget);
		}
	}
	/* Emit a signal that the font changed. */
	if (cresize) {
		vte_terminal_emit_char_size_changed(terminal,
						    terminal->char_width,
						    terminal->char_height);
	}
	/* Repaint. */
	_vte_invalidate_all(terminal);
}


static void
vte_terminal_ensure_font (VteTerminal *terminal)
{
	if (terminal->pvt->draw != NULL) {
		/* Load default fonts, if no fonts have been loaded. */
		if (!terminal->pvt->has_fonts) {
			vte_terminal_set_font_full_internal(terminal,
                                                            terminal->pvt->fontdesc,
                                                            terminal->pvt->fontantialias);
		}
		if (terminal->pvt->fontdirty) {
			gint width, height, ascent;
			terminal->pvt->fontdirty = FALSE;
			_vte_draw_set_text_font (terminal->pvt->draw,
					terminal->pvt->fontdesc,
					terminal->pvt->fontantialias);
			_vte_draw_get_text_metrics (terminal->pvt->draw,
						    &width, &height, &ascent);
			vte_terminal_apply_metrics(terminal,
						   width, height, ascent, height - ascent);
		}
	}
}


/**
 * vte_terminal_set_font_full:
 * @terminal: a #VteTerminal
 * @font_desc: The #PangoFontDescription of the desired font, or %NULL
 * @antialias: Specify if anti aliasing of the fonts is to be used or not.
 *
 * Sets the font used for rendering all text displayed by the terminal,
 * overriding any fonts set using gtk_widget_modify_font().  The terminal
 * will immediately attempt to load the desired font, retrieve its
 * metrics, and attempt to resize itself to keep the same number of rows
 * and columns.
 *
 * Since: 0.11.11
 * 
 * Deprecated: 0.20: Use vte_terminal_set_font()
 */
void
vte_terminal_set_font_full(VteTerminal *terminal,
			   const PangoFontDescription *font_desc,
			   VteTerminalAntiAlias antialias)
{
        vte_terminal_set_font_full_internal(terminal, font_desc, antialias);
}

static void
vte_terminal_set_font_full_internal(VteTerminal *terminal,
                                    const PangoFontDescription *font_desc,
                                    VteTerminalAntiAlias antialias)
{
        GObject *object;
	GtkStyle *style;
	VteTerminalPrivate *pvt;
	PangoFontDescription *desc;
        gboolean same_desc;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        object = G_OBJECT(terminal);
        pvt = terminal->pvt;

	/* Create an owned font description. */
	gtk_widget_ensure_style (&terminal->widget);
	style = gtk_widget_get_style (&terminal->widget);
	desc = pango_font_description_copy (style->font_desc);
	pango_font_description_set_family_static (desc, "monospace");
	if (font_desc != NULL) {
		pango_font_description_merge (desc, font_desc, TRUE);
		_VTE_DEBUG_IF(VTE_DEBUG_MISC) {
			if (desc) {
				char *tmp;
				tmp = pango_font_description_to_string(desc);
				g_printerr("Using pango font \"%s\".\n", tmp);
				g_free (tmp);
			}
		}
	} else {
		_vte_debug_print(VTE_DEBUG_MISC,
				"Using default monospace font.\n");
	}

        same_desc = pvt->fontdesc && pango_font_description_equal (pvt->fontdesc, desc);
	
	/* Note that we proceed to recreating the font even if the description
	 * and antialias settings are the same.  This is because maybe screen
	 * font options were changed, or new fonts installed.  Those will be
	 * detected at font creation time and respected.
	 */

        g_object_freeze_notify(object);

	/* Free the old font description and save the new one. */
	if (terminal->pvt->fontdesc != NULL) {
		pango_font_description_free(terminal->pvt->fontdesc);
	}
	pvt->fontdesc = desc;
	pvt->fontantialias = antialias;
	pvt->fontdirty = TRUE;
	pvt->has_fonts = TRUE;

        if (!same_desc)
                g_object_notify(object, "font-desc");

	/* Set the drawing font. */
	if (gtk_widget_get_realized (&terminal->widget)) {
		vte_terminal_ensure_font (terminal);
	}

        g_object_thaw_notify(object);
}

/**
 * vte_terminal_set_font:
 * @terminal: a #VteTerminal
 * @font_desc: (allow-none): a #PangoFontDescription for the desired font, or %NULL
 *
 * Sets the font used for rendering all text displayed by the terminal,
 * overriding any fonts set using gtk_widget_modify_font().  The terminal
 * will immediately attempt to load the desired font, retrieve its
 * metrics, and attempt to resize itself to keep the same number of rows
 * and columns.
 */
void
vte_terminal_set_font(VteTerminal *terminal,
		      const PangoFontDescription *font_desc)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	vte_terminal_set_font_full_internal(terminal, font_desc,
                                            VTE_ANTI_ALIAS_USE_DEFAULT);
}

static void
vte_terminal_set_font_from_string_full_internal(VteTerminal *terminal,
                                                const char *name,
                                                VteTerminalAntiAlias antialias)
{
	PangoFontDescription *font_desc = NULL;
	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	if (name)
	  font_desc = pango_font_description_from_string(name);
	vte_terminal_set_font_full_internal(terminal, font_desc, antialias);
	pango_font_description_free(font_desc);
}

/**
 * vte_terminal_set_font_from_string_full:
 * @terminal: a #VteTerminal
 * @name: A string describing the font.
 * @antialias: Whether or not to antialias the font (if possible).
 *
 * A convenience function which converts @name into a #PangoFontDescription and
 * passes it to vte_terminal_set_font_full().
 *
 * Since: 0.11.11
 *
 * Deprecated: 0.20: Use vte_terminal_set_font()
 */
void
vte_terminal_set_font_from_string_full(VteTerminal *terminal, const char *name,
				       VteTerminalAntiAlias antialias)
{
        vte_terminal_set_font_from_string_full_internal(terminal, name, antialias);
}

/**
 * vte_terminal_set_font_from_string:
 * @terminal: a #VteTerminal
 * @name: (type utf8): a pango font description in string form
 *
 * A convenience function which converts @name into a #PangoFontDescription and
 * passes it to vte_terminal_set_font().
 */
void
vte_terminal_set_font_from_string(VteTerminal *terminal, const char *name)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(name != NULL);
	vte_terminal_set_font_from_string_full_internal(terminal, name,
                                                        VTE_ANTI_ALIAS_USE_DEFAULT);
}

/**
 * vte_terminal_get_font:
 * @terminal: a #VteTerminal
 *
 * Queries the terminal for information about the fonts which will be
 * used to draw text in the terminal.
 *
 * Returns: (transfer none): a #PangoFontDescription describing the font the terminal is
 *   currently using to render text
 */
const PangoFontDescription *
vte_terminal_get_font(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return terminal->pvt->fontdesc;
}

/* Read and refresh our perception of the size of the PTY. */
static void
vte_terminal_refresh_size(VteTerminal *terminal)
{
        VteTerminalPrivate *pvt = terminal->pvt;
	int rows, columns;
        GError *error = NULL;

        if (pvt->pty == NULL)
                return;

        if (vte_pty_get_size(pvt->pty, &rows, &columns, &error)) {
                terminal->row_count = rows;
                terminal->column_count = columns;
        } else {
                g_warning(_("Error reading PTY size, using defaults: %s\n"), error->message);
                g_error_free(error);
	}
}

/**
 * vte_terminal_set_size:
 * @terminal: a #VteTerminal
 * @columns: the desired number of columns
 * @rows: the desired number of rows
 *
 * Attempts to change the terminal's size in terms of rows and columns.  If
 * the attempt succeeds, the widget will resize itself to the proper size.
 */
void
vte_terminal_set_size(VteTerminal *terminal, glong columns, glong rows)
{
	glong old_columns, old_rows;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	_vte_debug_print(VTE_DEBUG_MISC,
			"Setting PTY size to %ldx%ld.\n",
			columns, rows);

	old_rows = terminal->row_count;
	old_columns = terminal->column_count;

	if (terminal->pvt->pty != NULL) {
                GError *error = NULL;

		/* Try to set the terminal size, and read it back,
		 * in case something went awry.
                 */
		if (!vte_pty_set_size(terminal->pvt->pty, rows, columns, &error)) {
			g_warning("%s\n", error->message);
                        g_error_free(error);
		}
		vte_terminal_refresh_size(terminal);
	} else {
		terminal->row_count = rows;
		terminal->column_count = columns;
	}
	if (old_rows != terminal->row_count || old_columns != terminal->column_count) {
		VteScreen *screen = terminal->pvt->screen;
		glong visible_rows = MIN (old_rows, _vte_ring_length (screen->row_data));
		if (terminal->row_count < visible_rows) {
			glong delta = visible_rows - terminal->row_count;
			screen->insert_delta += delta;
			vte_terminal_queue_adjustment_value_changed (
					terminal,
					screen->scroll_delta + delta);
		}
		gtk_widget_queue_resize_no_redraw (&terminal->widget);
		/* Our visible text changed. */
		vte_terminal_emit_text_modified(terminal);
	}
}

/* Redraw the widget. */
static void
vte_terminal_handle_scroll(VteTerminal *terminal)
{
	long dy, adj;
	VteScreen *screen;

	screen = terminal->pvt->screen;

	/* Read the new adjustment value and save the difference. */
	adj = round (gtk_adjustment_get_value(terminal->adjustment));
	dy = adj - screen->scroll_delta;
	screen->scroll_delta = adj;

	/* Sanity checks. */
	if (! gtk_widget_is_drawable (&terminal->widget)
			|| terminal->pvt->visibility_state == GDK_VISIBILITY_FULLY_OBSCURED) {
		return;
	}

	if (dy != 0) {
		_vte_debug_print(VTE_DEBUG_ADJ,
			    "Scrolling by %ld\n", dy);
		_vte_terminal_scroll_region(terminal, screen->scroll_delta,
					   terminal->row_count, -dy);
		vte_terminal_emit_text_scrolled(terminal, dy);
		_vte_terminal_queue_contents_changed(terminal);
	} else {
		_vte_debug_print(VTE_DEBUG_ADJ, "Not scrolling\n");
	}
}

#if GTK_CHECK_VERSION (2, 91, 2)
static void
vte_terminal_set_hadjustment(VteTerminal *terminal,
                             GtkAdjustment *adjustment)
{
  VteTerminalPrivate *pvt = terminal->pvt;

  if (adjustment == pvt->hadjustment)
    return;

  if (pvt->hadjustment)
    g_object_unref (pvt->hadjustment);

  pvt->hadjustment = adjustment ? g_object_ref_sink (adjustment) : NULL;
}
#endif

static void
vte_terminal_set_vadjustment(VteTerminal *terminal,
                             GtkAdjustment *adjustment)
{
	if (adjustment != NULL && adjustment == terminal->adjustment)
		return;
	if (adjustment == NULL && terminal->adjustment != NULL)
		return;

	if (adjustment == NULL)
		adjustment = GTK_ADJUSTMENT(gtk_adjustment_new(0, 0, 0, 0, 0, 0));
	else
		g_return_if_fail(GTK_IS_ADJUSTMENT(adjustment));

	/* Add a reference to the new adjustment object. */
	g_object_ref_sink(adjustment);
	/* Get rid of the old adjustment object. */
	if (terminal->adjustment != NULL) {
		/* Disconnect our signal handlers from this object. */
		g_signal_handlers_disconnect_by_func(terminal->adjustment,
						     vte_terminal_handle_scroll,
						     terminal);
		g_object_unref(terminal->adjustment);
	}

	/* Set the new adjustment object. */
	terminal->adjustment = adjustment;

	/* We care about the offset, not the top or bottom. */
	g_signal_connect_swapped(terminal->adjustment,
				 "value-changed",
				 G_CALLBACK(vte_terminal_handle_scroll),
				 terminal);
}

/* Set the adjustment objects used by the terminal widget. */
#if !GTK_CHECK_VERSION (2, 91, 2)
static void
vte_terminal_set_scroll_adjustments(GtkWidget *widget,
                                   GtkAdjustment *hadjustment G_GNUC_UNUSED,
                                   GtkAdjustment *vadjustment)
{
        vte_terminal_set_vadjustment(VTE_TERMINAL(widget), vadjustment);
}
#endif /* GTK 2.x */

/**
 * vte_terminal_set_emulation:
 * @terminal: a #VteTerminal
 * @emulation: (allow-none): the name of a terminal description, or %NULL to use the default
 *
 * Sets what type of terminal the widget attempts to emulate by scanning for
 * control sequences defined in the system's termcap file.  Unless you
 * are interested in this feature, always use "xterm".
 */
void
vte_terminal_set_emulation(VteTerminal *terminal, const char *emulation)
{
        VteTerminalPrivate *pvt;
        GObject *object;
	int columns, rows;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        object = G_OBJECT(terminal);
        pvt = terminal->pvt;

        g_object_freeze_notify(object);

	/* Set the emulation type, for reference. */
	if (emulation == NULL) {
		emulation = vte_terminal_get_default_emulation(terminal);
	}
	terminal->pvt->emulation = g_intern_string(emulation);
	_vte_debug_print(VTE_DEBUG_MISC,
			"Setting emulation to `%s'...\n", emulation);
	/* Find and read the right termcap file. */
	vte_terminal_set_termcap(terminal, NULL, FALSE);

	/* Create a table to hold the control sequences. */
	if (terminal->pvt->matcher != NULL) {
		_vte_matcher_free(terminal->pvt->matcher);
	}
	terminal->pvt->matcher = _vte_matcher_new(emulation, terminal->pvt->termcap);

	if (terminal->pvt->termcap != NULL) {
		/* Read emulation flags. */
		terminal->pvt->flags.am = _vte_termcap_find_boolean(terminal->pvt->termcap,
								    terminal->pvt->emulation,
								    "am");
		terminal->pvt->flags.bw = _vte_termcap_find_boolean(terminal->pvt->termcap,
								    terminal->pvt->emulation,
								    "bw");
		terminal->pvt->flags.LP = _vte_termcap_find_boolean(terminal->pvt->termcap,
								    terminal->pvt->emulation,
								    "LP");
		terminal->pvt->flags.ul = _vte_termcap_find_boolean(terminal->pvt->termcap,
								    terminal->pvt->emulation,
								    "ul");
		terminal->pvt->flags.xn = _vte_termcap_find_boolean(terminal->pvt->termcap,
								    terminal->pvt->emulation,
								    "xn");

		/* Resize to the given default. */
		columns = _vte_termcap_find_numeric(terminal->pvt->termcap,
						    terminal->pvt->emulation,
						    "co");
		if (columns <= 0) {
			columns = VTE_COLUMNS;
		}
		terminal->pvt->default_column_count = columns;

		rows = _vte_termcap_find_numeric(terminal->pvt->termcap,
						 terminal->pvt->emulation,
						 "li");
		if (rows <= 0 ) {
			rows = VTE_ROWS;
		}
		terminal->pvt->default_row_count = rows;
	}

	/* Notify observers that we changed our emulation. */
	vte_terminal_emit_emulation_changed(terminal);

        g_object_thaw_notify(object);
}

/* FIXMEchpe deprecate this function, it's wrong */
/**
 * vte_terminal_get_default_emulation:
 * @terminal: a #VteTerminal
 *
 * Queries the terminal for its default emulation, which is attempted if the
 * terminal type passed to vte_terminal_set_emulation() is %NULL.
 *
 * Returns: (transfer none) (type utf8): an interned string containing the name of the default terminal
 *   type the widget attempts to emulate
 *
 * Since: 0.11.11
 */
const char *
vte_terminal_get_default_emulation(VteTerminal *terminal)
{
	return g_intern_static_string(VTE_DEFAULT_EMULATION);
}

/**
 * vte_terminal_get_emulation:
 * @terminal: a #VteTerminal
 *
 * Queries the terminal for its current emulation, as last set by a call to
 * vte_terminal_set_emulation().
 *
 * Returns: an interned string containing the name of the terminal type the
 *   widget is attempting to emulate
 */
const char *
vte_terminal_get_emulation(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return terminal->pvt->emulation;
}

void
_vte_terminal_inline_error_message(VteTerminal *terminal, const char *format, ...)
{
	va_list ap;
	char *str;

	va_start (ap, format);
	str = g_strdup_vprintf (format, ap);
	va_end (ap);

	vte_terminal_feed (terminal, "*** VTE ***: ", 13);
	vte_terminal_feed (terminal, str, -1);
	vte_terminal_feed (terminal, "\r\n", 2);
	g_free (str);
}

/* Set the path to the termcap file we read, and read it in. */
static void
vte_terminal_set_termcap(VteTerminal *terminal, const char *path,
			 gboolean reset)
{
        GObject *object = G_OBJECT(terminal);
	struct stat st;
	char *wpath;

	if (path == NULL) {
		wpath = g_build_filename(TERMCAPDIR,
					 terminal->pvt->emulation ?
					 terminal->pvt->emulation :
					 vte_terminal_get_default_emulation(terminal),
					 NULL);
		if (g_stat(wpath, &st) != 0) {
			g_free(wpath);
			wpath = g_strdup("/etc/termcap");
		}
		path = g_intern_string (wpath);
		g_free(wpath);
	} else {
		path = g_intern_string (path);
	}
	if (path == terminal->pvt->termcap_path) {
		return;
	}

        g_object_freeze_notify(object);

	terminal->pvt->termcap_path = path;

	_vte_debug_print(VTE_DEBUG_MISC, "Loading termcap `%s'...",
			terminal->pvt->termcap_path);
	if (terminal->pvt->termcap != NULL) {
		_vte_termcap_free(terminal->pvt->termcap);
	}
	terminal->pvt->termcap = _vte_termcap_new(terminal->pvt->termcap_path);
	_vte_debug_print(VTE_DEBUG_MISC, "\n");
	if (terminal->pvt->termcap == NULL) {
		_vte_terminal_inline_error_message(terminal,
				"Failed to load terminal capabilities from '%s'",
				terminal->pvt->termcap_path);
	}
	if (reset) {
		vte_terminal_set_emulation(terminal, terminal->pvt->emulation);
	}

        g_object_thaw_notify(object);
}

static void
_vte_terminal_codeset_changed_cb(struct _vte_iso2022_state *state, gpointer p)
{
	vte_terminal_set_encoding(p, _vte_iso2022_state_get_codeset(state));
}

/* Initialize the terminal widget after the base widget stuff is initialized.
 * We need to create a new psuedo-terminal pair, read in the termcap file, and
 * set ourselves up to do the interpretation of sequences. */
static void
vte_terminal_init(VteTerminal *terminal)
{
	VteTerminalPrivate *pvt;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_init()\n");

	/* Initialize private data. */
	pvt = terminal->pvt = G_TYPE_INSTANCE_GET_PRIVATE (terminal, VTE_TYPE_TERMINAL, VteTerminalPrivate);

	gtk_widget_set_can_focus(&terminal->widget, TRUE);

	gtk_widget_set_app_paintable (&terminal->widget, TRUE);

	/* We do our own redrawing. */
	gtk_widget_set_redraw_on_allocate (&terminal->widget, FALSE);

	/* Set an adjustment for the application to use to control scrolling. */
        terminal->adjustment = NULL;
#if GTK_CHECK_VERSION (2, 91, 2)
        pvt->hadjustment = NULL;
        /* GtkScrollable */
        pvt->hscroll_policy = GTK_SCROLL_NATURAL;
        pvt->vscroll_policy = GTK_SCROLL_NATURAL;

        vte_terminal_set_hadjustment(terminal, NULL);
#endif

	vte_terminal_set_vadjustment(terminal, NULL);


	/* Set up dummy metrics, value != 0 to avoid division by 0 */
	terminal->char_width = 1;
	terminal->char_height = 1;
	terminal->char_ascent = 1;
	terminal->char_descent = 1;
	terminal->pvt->line_thickness = 1;
	terminal->pvt->underline_position = 1;
	terminal->pvt->strikethrough_position = 1;

	/* We allocated zeroed memory, just fill in non-zero stuff. */

	/* Initialize the screens and histories. */
	_vte_ring_init (pvt->alternate_screen.row_data, terminal->row_count);
	pvt->alternate_screen.sendrecv_mode = TRUE;
	pvt->alternate_screen.status_line_contents = g_string_new(NULL);
	pvt->screen = &terminal->pvt->alternate_screen;
	_vte_terminal_set_default_attributes(terminal);

	_vte_ring_init (pvt->normal_screen.row_data,  VTE_SCROLLBACK_INIT);
	pvt->normal_screen.sendrecv_mode = TRUE;
	pvt->normal_screen.status_line_contents = g_string_new(NULL);
	pvt->screen = &terminal->pvt->normal_screen;
	_vte_terminal_set_default_attributes(terminal);

	/* Set up I/O encodings. */
	pvt->iso2022 = _vte_iso2022_state_new(pvt->encoding,
					      &_vte_terminal_codeset_changed_cb,
					      terminal);
	pvt->incoming = NULL;
	pvt->pending = g_array_new(FALSE, TRUE, sizeof(gunichar));
	pvt->max_input_bytes = VTE_MAX_INPUT_READ;
	pvt->cursor_blink_tag = 0;
	pvt->outgoing = _vte_buffer_new();
	pvt->outgoing_conv = VTE_INVALID_CONV;
	pvt->conv_buffer = _vte_buffer_new();
	vte_terminal_set_encoding(terminal, NULL);
	g_assert(terminal->pvt->encoding != NULL);

	/* Load the termcap data and set up the emulation. */
	pvt->keypad_mode = VTE_KEYMODE_NORMAL;
	pvt->cursor_mode = VTE_KEYMODE_NORMAL;
	pvt->dec_saved = g_hash_table_new(NULL, NULL);
	pvt->default_column_count = VTE_COLUMNS;
	pvt->default_row_count = VTE_ROWS;

	/* Setting the terminal type and size requires the PTY master to
	 * be set up properly first. */
        pvt->pty = NULL;
	vte_terminal_set_emulation(terminal, NULL);
	vte_terminal_set_size(terminal,
			      pvt->default_column_count,
			      pvt->default_row_count);
	pvt->pty_input_source = 0;
	pvt->pty_output_source = 0;
	pvt->pty_pid = -1;
        pvt->child_exit_status = 0;

	/* Scrolling options. */
	pvt->scroll_on_keystroke = TRUE;
        pvt->scrollback_lines = -1; /* force update in vte_terminal_set_scrollback_lines */
	vte_terminal_set_scrollback_lines(terminal, VTE_SCROLLBACK_INIT);

	/* Selection info. */
	vte_terminal_set_word_chars(terminal, NULL);

	/* Miscellaneous options. */
	vte_terminal_set_backspace_binding(terminal, VTE_ERASE_AUTO);
	vte_terminal_set_delete_binding(terminal, VTE_ERASE_AUTO);
	pvt->meta_sends_escape = TRUE;
	pvt->audible_bell = TRUE;
	pvt->bell_margin = 10;
	pvt->allow_bold = TRUE;
	pvt->nrc_mode = TRUE;
	vte_terminal_set_default_tabstops(terminal);

	/* Cursor shape. */
	pvt->cursor_shape = VTE_CURSOR_SHAPE_BLOCK;
        pvt->cursor_aspect_ratio = 0.04;

	/* Cursor blinking. */
	pvt->cursor_visible = TRUE;
	pvt->cursor_blink_timeout = 500;
        pvt->cursor_blinks = FALSE;
        pvt->cursor_blink_mode = VTE_CURSOR_BLINK_SYSTEM;

	/* Matching data. */
        pvt->match_regex_mode = VTE_REGEX_UNDECIDED;
	pvt->match_regexes = g_array_new(FALSE, TRUE,
					 sizeof(struct vte_match_regex));
	vte_terminal_match_hilite_clear(terminal);

	/* Rendering data.  Try everything. */
	pvt->draw = _vte_draw_new(&terminal->widget);

	/* The font description. */
	pvt->fontantialias = VTE_ANTI_ALIAS_USE_DEFAULT;
	gtk_widget_ensure_style(&terminal->widget);

	/* Set up background information. */
	pvt->bg_tint_color.red = 0;
	pvt->bg_tint_color.green = 0;
	pvt->bg_tint_color.blue = 0;
	pvt->bg_saturation = 0.4 * VTE_SATURATION_MAX;
	pvt->bg_opacity = 0xffff;
	pvt->selection_block_mode = FALSE;
	pvt->has_fonts = FALSE;
	pvt->root_pixmap_changed_tag = 0;

	/* Not all backends generate GdkVisibilityNotify, so mark the
	 * window as unobscured initially. */
	pvt->visibility_state = GDK_VISIBILITY_UNOBSCURED;

	/* Listen for hierarchy change notifications. */
	g_signal_connect(terminal, "hierarchy-changed",
			 G_CALLBACK(vte_terminal_hierarchy_changed),
			 NULL);

        pvt->inner_border = default_inner_border;

#ifdef VTE_DEBUG
	/* In debuggable mode, we always do this. */
	/* gtk_widget_get_accessible(&terminal->widget); */
#endif

#if GTK_CHECK_VERSION (2, 99, 0)
{
        GtkStyleContext *context;

        context = gtk_widget_get_style_context (&terminal->widget);
        gtk_style_context_add_provider (context,
                                        VTE_TERMINAL_GET_CLASS (terminal)->priv->style_provider,
                                        GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
}
#endif
}

/* Tell GTK+ how much space we need. */
#if GTK_CHECK_VERSION (2, 91, 0)
static void
vte_terminal_get_preferred_width(GtkWidget *widget,
				 int       *minimum_width,
				 int       *natural_width)
{
	VteTerminal *terminal;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_get_preferred_width()\n");

	terminal = VTE_TERMINAL(widget);

	vte_terminal_ensure_font (terminal);

        vte_terminal_refresh_size(terminal);
	*minimum_width = terminal->char_width * 1;
        *natural_width = terminal->char_width * terminal->column_count;

	*minimum_width += terminal->pvt->inner_border.left +
                          terminal->pvt->inner_border.right;
	*natural_width += terminal->pvt->inner_border.left +
                          terminal->pvt->inner_border.right;

	_vte_debug_print(VTE_DEBUG_WIDGET_SIZE,
			"[Terminal %p] minimum_width=%d, natural_width=%d for %ldx%ld cells.\n",
                        terminal,
			*minimum_width, *natural_width,
			terminal->column_count,
			terminal->row_count);
}

static void
vte_terminal_get_preferred_height(GtkWidget *widget,
				  int       *minimum_height,
				  int       *natural_height)
{
	VteTerminal *terminal;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_get_preferred_height()\n");

	terminal = VTE_TERMINAL(widget);

	vte_terminal_ensure_font (terminal);

        vte_terminal_refresh_size(terminal);
	*minimum_height = terminal->char_height * 1;
        *natural_height = terminal->char_height * terminal->row_count;

	*minimum_height += terminal->pvt->inner_border.left +
			   terminal->pvt->inner_border.right;
	*natural_height += terminal->pvt->inner_border.left +
			   terminal->pvt->inner_border.right;

	_vte_debug_print(VTE_DEBUG_WIDGET_SIZE,
			"[Terminal %p] minimum_height=%d, natural_height=%d for %ldx%ld cells.\n",
                        terminal,
			*minimum_height, *natural_height,
			terminal->column_count,
			terminal->row_count);
}
#else /* GTK+ 2.x */
static void
vte_terminal_size_request(GtkWidget *widget, GtkRequisition *requisition)
{
	VteTerminal *terminal;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_size_request()\n");

	terminal = VTE_TERMINAL(widget);

	vte_terminal_ensure_font (terminal);

        vte_terminal_refresh_size(terminal);
        requisition->width = terminal->char_width * terminal->column_count;
        requisition->height = terminal->char_height * terminal->row_count;

	requisition->width += terminal->pvt->inner_border.left +
                              terminal->pvt->inner_border.right;
	requisition->height += terminal->pvt->inner_border.top +
                               terminal->pvt->inner_border.bottom;

	_vte_debug_print(VTE_DEBUG_WIDGET_SIZE,
			"[Terminal %p] Size request is %dx%d for %ldx%ld cells.\n",
                        terminal,
			requisition->width, requisition->height,
			terminal->column_count,
			terminal->row_count);
}
#endif

/* Accept a given size from GTK+. */
static void
vte_terminal_size_allocate(GtkWidget *widget, GtkAllocation *allocation)
{
	VteTerminal *terminal;
	glong width, height;
	GtkAllocation current_allocation;
	gboolean repaint, update_scrollback;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE,
			"vte_terminal_size_allocate()\n");

	terminal = VTE_TERMINAL(widget);

	width = (allocation->width - (terminal->pvt->inner_border.left + terminal->pvt->inner_border.right)) /
		terminal->char_width;
	height = (allocation->height - (terminal->pvt->inner_border.top + terminal->pvt->inner_border.bottom)) /
		 terminal->char_height;
	width = MAX(width, 1);
	height = MAX(height, 1);

	_vte_debug_print(VTE_DEBUG_WIDGET_SIZE,
			"[Terminal %p] Sizing window to %dx%d (%ldx%ld).\n",
                        terminal,
			allocation->width, allocation->height,
			width, height);

	gtk_widget_get_allocation (widget, &current_allocation);

	repaint = current_allocation.width != allocation->width
			|| current_allocation.height != allocation->height;
	update_scrollback = current_allocation.height != allocation->height;

	/* Set our allocation to match the structure. */
	gtk_widget_set_allocation (widget, allocation);

	if (width != terminal->column_count
			|| height != terminal->row_count
			|| update_scrollback)
	{
		VteScreen *screen = terminal->pvt->screen;

		/* Set the size of the pseudo-terminal. */
		vte_terminal_set_size(terminal, width, height);

		/* Adjust scrolling area in case our boundaries have just been
		 * redefined to be invalid. */
		if (screen->scrolling_restricted) {
			screen->scrolling_region.start =
				MIN(screen->scrolling_region.start,
						terminal->row_count - 1);
			screen->scrolling_region.end =
				MIN(screen->scrolling_region.end,
						terminal->row_count - 1);
		}

		/* Ensure scrollback buffers cover the screen. */
		vte_terminal_set_scrollback_lines(terminal,
				terminal->pvt->scrollback_lines);
		/* Ensure the cursor is valid */
		screen->cursor_current.row = CLAMP (screen->cursor_current.row,
				_vte_ring_delta (screen->row_data),
				MAX (_vte_ring_delta (screen->row_data),
					_vte_ring_next (screen->row_data) - 1));
		/* Notify viewers that the contents have changed. */
		_vte_terminal_queue_contents_changed(terminal);
	}

	/* Resize the GDK window. */
	if (gtk_widget_get_realized (widget)) {
		gdk_window_move_resize (gtk_widget_get_window (widget),
					allocation->x,
					allocation->y,
					allocation->width,
					allocation->height);
		/* Force a repaint if we were resized. */
		if (repaint) {
			reset_update_regions (terminal);
			_vte_invalidate_all(terminal);
		}
	}
}

/* Queue a background update. */
static void
root_pixmap_changed_cb(VteBg *bg, VteTerminal *terminal)
{
	_vte_debug_print (VTE_DEBUG_EVENTS, "Root pixmap changed.\n");
	if (terminal->pvt->bg_transparent) {
		vte_terminal_queue_background_update(terminal);
	}
}

/* The window is being destroyed. */
static void
vte_terminal_unrealize(GtkWidget *widget)
{
	GdkWindow *window;
	VteTerminal *terminal;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_unrealize()\n");

	terminal = VTE_TERMINAL (widget);
	window = gtk_widget_get_window (widget);

	/* Disconnect from background-change events. */
	if (terminal->pvt->root_pixmap_changed_tag != 0) {
		VteBg       *bg;
		bg = vte_bg_get_for_screen(gtk_widget_get_screen(widget));
		g_signal_handler_disconnect (bg,
				terminal->pvt->root_pixmap_changed_tag);
		terminal->pvt->root_pixmap_changed_tag = 0;
	}

	/* Deallocate the cursors. */
	terminal->pvt->mouse_cursor_visible = FALSE;
	gdk_cursor_unref(terminal->pvt->mouse_default_cursor);
	terminal->pvt->mouse_default_cursor = NULL;
	gdk_cursor_unref(terminal->pvt->mouse_mousing_cursor);
	terminal->pvt->mouse_mousing_cursor = NULL;
	gdk_cursor_unref(terminal->pvt->mouse_inviso_cursor);
	terminal->pvt->mouse_inviso_cursor = NULL;

	vte_terminal_match_hilite_clear(terminal);

	/* Shut down input methods. */
	if (terminal->pvt->im_context != NULL) {
	        g_signal_handlers_disconnect_by_func (terminal->pvt->im_context,
						      vte_terminal_im_preedit_changed,
						      terminal);
		vte_terminal_im_reset(terminal);
		gtk_im_context_set_client_window(terminal->pvt->im_context,
						 NULL);
		g_object_unref(terminal->pvt->im_context);
		terminal->pvt->im_context = NULL;
	}
	terminal->pvt->im_preedit_active = FALSE;
	if (terminal->pvt->im_preedit != NULL) {
		g_free(terminal->pvt->im_preedit);
		terminal->pvt->im_preedit = NULL;
	}
	if (terminal->pvt->im_preedit_attrs != NULL) {
		pango_attr_list_unref(terminal->pvt->im_preedit_attrs);
		terminal->pvt->im_preedit_attrs = NULL;
	}
	terminal->pvt->im_preedit_cursor = 0;

	/* Clean up our draw structure. */
	if (terminal->pvt->draw != NULL) {
		_vte_draw_free(terminal->pvt->draw);
		terminal->pvt->draw = NULL;
	}
	terminal->pvt->fontdirty = TRUE;

	/* Unmap the widget if it hasn't been already. */
	if (gtk_widget_get_mapped (widget)) {
		gtk_widget_unmap (widget);
	}

	/* Remove the GDK window. */
	if (window != NULL) {
		/* detach style */
		GtkStyle *style;

		style = gtk_widget_get_style (widget);
		gtk_style_detach (style);

		gdk_window_set_user_data (window, NULL);
		gtk_widget_set_window (widget, NULL);

		gdk_window_destroy (window);
	}

	/* Remove the blink timeout function. */
	remove_cursor_timeout(terminal);

	/* Cancel any pending redraws. */
	remove_update_timeout (terminal);

	/* Cancel any pending signals */
	terminal->pvt->contents_changed_pending = FALSE;
	terminal->pvt->cursor_moved_pending = FALSE;
	terminal->pvt->text_modified_flag = FALSE;
	terminal->pvt->text_inserted_flag = FALSE;
	terminal->pvt->text_deleted_flag = FALSE;

	/* Clear modifiers. */
	terminal->pvt->modifiers = 0;

	/* Mark that we no longer have a GDK window. */
	gtk_widget_set_realized (widget, FALSE);
}

static void
vte_terminal_sync_settings (GtkSettings *settings,
                            GParamSpec *pspec,
                            VteTerminal *terminal)
{
        VteTerminalPrivate *pvt = terminal->pvt;
        gboolean blink;
        int blink_time = 1000;
        int blink_timeout = G_MAXINT;

        g_object_get(G_OBJECT (settings),
                     "gtk-cursor-blink", &blink,
                     "gtk-cursor-blink-time", &blink_time,
                     "gtk-cursor-blink-timeout", &blink_timeout,
                     NULL);

        _vte_debug_print(VTE_DEBUG_MISC,
                         "Cursor blinking settings setting: blink=%d time=%d timeout=%d\n",
                         blink, blink_time, blink_timeout);

        pvt->cursor_blink_cycle = blink_time / 2;
        pvt->cursor_blink_timeout = blink_timeout;

        if (pvt->cursor_blink_mode == VTE_CURSOR_BLINK_SYSTEM)
                vte_terminal_set_cursor_blinks_internal(terminal, blink);
}

static void
vte_terminal_screen_changed (GtkWidget *widget,
                             GdkScreen *previous_screen)
{
        VteTerminal *terminal = VTE_TERMINAL (widget);
        GdkScreen *screen;
        GtkSettings *settings;

        screen = gtk_widget_get_screen (widget);
        if (previous_screen != NULL &&
            (screen != previous_screen || screen == NULL)) {
                settings = gtk_settings_get_for_screen (previous_screen);
                g_signal_handlers_disconnect_matched (settings, G_SIGNAL_MATCH_DATA,
                                                      0, 0, NULL, NULL,
                                                      widget);
        }

        if (GTK_WIDGET_CLASS (vte_terminal_parent_class)->screen_changed) {
                GTK_WIDGET_CLASS (vte_terminal_parent_class)->screen_changed (widget, previous_screen);
        }

        if (screen == previous_screen || screen == NULL)
                return;

        settings = gtk_widget_get_settings (widget);
        vte_terminal_sync_settings (settings, NULL, terminal);
        g_signal_connect (settings, "notify::gtk-cursor-blink",
                          G_CALLBACK (vte_terminal_sync_settings), widget);
        g_signal_connect (settings, "notify::gtk-cursor-blink-time",
                          G_CALLBACK (vte_terminal_sync_settings), widget);
        g_signal_connect (settings, "notify::gtk-cursor-blink-timeout",
                          G_CALLBACK (vte_terminal_sync_settings), widget);
}

/* Perform final cleanups for the widget before it's freed. */
static void
vte_terminal_finalize(GObject *object)
{
    	GtkWidget *widget = GTK_WIDGET (object);
    	VteTerminal *terminal = VTE_TERMINAL (object);
	GtkWidget *toplevel;
	GtkClipboard *clipboard;
        GtkSettings *settings;
	struct vte_match_regex *regex;
	guint i;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_finalize()\n");

	/* Free the draw structure. */
	if (terminal->pvt->draw != NULL) {
		_vte_draw_free(terminal->pvt->draw);
	}

	/* The NLS maps. */
	_vte_iso2022_state_free(terminal->pvt->iso2022);

	/* Free background info. */
	g_free(terminal->pvt->bg_file);

	/* Free the font description. */
	if (terminal->pvt->fontdesc != NULL) {
		pango_font_description_free(terminal->pvt->fontdesc);
	}
	terminal->pvt->fontantialias = VTE_ANTI_ALIAS_USE_DEFAULT;

	/* Free matching data. */
	if (terminal->pvt->match_attributes != NULL) {
		g_array_free(terminal->pvt->match_attributes, TRUE);
	}
	g_free(terminal->pvt->match_contents);
	if (terminal->pvt->match_regexes != NULL) {
		for (i = 0; i < terminal->pvt->match_regexes->len; i++) {
			regex = &g_array_index(terminal->pvt->match_regexes,
					       struct vte_match_regex,
					       i);
			/* Skip holes. */
			if (regex->tag < 0) {
				continue;
			}
                        regex_match_clear(regex);
		}
		g_array_free(terminal->pvt->match_regexes, TRUE);
	}

	if (terminal->pvt->search_regex)
		g_regex_unref (terminal->pvt->search_regex);
	if (terminal->pvt->search_attrs)
		g_array_free (terminal->pvt->search_attrs, TRUE);

	/* Disconnect from toplevel window configure events. */
	toplevel = gtk_widget_get_toplevel(&terminal->widget);
	if ((toplevel != NULL) && (G_OBJECT(toplevel) != object)) {
		g_signal_handlers_disconnect_by_func(toplevel,
						     vte_terminal_configure_toplevel,
						     terminal);
	}

	/* Disconnect from autoscroll requests. */
	vte_terminal_stop_autoscroll(terminal);

	/* Cancel pending adjustment change notifications. */
	terminal->pvt->adjustment_changed_pending = FALSE;

	/* Tabstop information. */
	if (terminal->pvt->tabstops != NULL) {
		g_hash_table_destroy(terminal->pvt->tabstops);
	}

	/* Free any selected text, but if we currently own the selection,
	 * throw the text onto the clipboard without an owner so that it
	 * doesn't just disappear. */
	if (terminal->pvt->selection != NULL) {
		clipboard = vte_terminal_clipboard_get(terminal,
						       GDK_SELECTION_PRIMARY);
		if (gtk_clipboard_get_owner(clipboard) == object) {
			gtk_clipboard_set_text(clipboard,
					       terminal->pvt->selection,
					       -1);
		}
		g_free(terminal->pvt->selection);
	}
	if (terminal->pvt->word_chars != NULL) {
		g_array_free(terminal->pvt->word_chars, TRUE);
	}

	/* Clear the output histories. */
	_vte_ring_fini(terminal->pvt->normal_screen.row_data);
	_vte_ring_fini(terminal->pvt->alternate_screen.row_data);

	/* Clear the status lines. */
	g_string_free(terminal->pvt->normal_screen.status_line_contents,
		      TRUE);
	g_string_free(terminal->pvt->alternate_screen.status_line_contents,
		      TRUE);

	/* Free conversion descriptors. */
	if (terminal->pvt->outgoing_conv != VTE_INVALID_CONV) {
		_vte_conv_close(terminal->pvt->outgoing_conv);
		terminal->pvt->outgoing_conv = VTE_INVALID_CONV;
	}

	/* Stop listening for child-exited signals. */
	if (terminal->pvt->pty_reaper != NULL) {
		g_signal_handlers_disconnect_by_func(terminal->pvt->pty_reaper,
						     vte_terminal_catch_child_exited,
						     terminal);
		g_object_unref(terminal->pvt->pty_reaper);
	}

	/* Stop processing input. */
	vte_terminal_stop_processing (terminal);

	/* Discard any pending data. */
	_vte_incoming_chunks_release (terminal->pvt->incoming);
	_vte_buffer_free(terminal->pvt->outgoing);
	g_array_free(terminal->pvt->pending, TRUE);
	_vte_buffer_free(terminal->pvt->conv_buffer);

	/* Stop the child and stop watching for input from the child. */
	if (terminal->pvt->pty_pid != -1) {
#ifdef HAVE_GETPGID
		pid_t pgrp;
		pgrp = getpgid(terminal->pvt->pty_pid);
		if (pgrp != -1) {
			kill(-pgrp, SIGHUP);
		}
#endif
		kill(terminal->pvt->pty_pid, SIGHUP);
	}
	_vte_terminal_disconnect_pty_read(terminal);
	_vte_terminal_disconnect_pty_write(terminal);
	if (terminal->pvt->pty_channel != NULL) {
		g_io_channel_unref (terminal->pvt->pty_channel);
	}
	if (terminal->pvt->pty != NULL) {
                vte_pty_close(terminal->pvt->pty);
                g_object_unref(terminal->pvt->pty);
	}

	/* Remove hash tables. */
	if (terminal->pvt->dec_saved != NULL) {
		g_hash_table_destroy(terminal->pvt->dec_saved);
	}

	/* Clean up emulation structures. */
	if (terminal->pvt->matcher != NULL) {
		_vte_matcher_free(terminal->pvt->matcher);
	}
	if (terminal->pvt->termcap != NULL) {
		_vte_termcap_free(terminal->pvt->termcap);
	}

	remove_update_timeout (terminal);

	/* discard title updates */
	g_free(terminal->pvt->window_title_changed);
	g_free(terminal->pvt->icon_title_changed);

	/* Free public-facing data. */
	g_free(terminal->window_title);
	g_free(terminal->icon_title);
	if (terminal->adjustment != NULL) {
		g_object_unref(terminal->adjustment);
	}

        settings = gtk_widget_get_settings (widget);
        g_signal_handlers_disconnect_matched (settings, G_SIGNAL_MATCH_DATA,
                                              0, 0, NULL, NULL,
                                              terminal);

	/* Call the inherited finalize() method. */
	G_OBJECT_CLASS(vte_terminal_parent_class)->finalize(object);
}

/* Handle realizing the widget.  Most of this is copy-paste from GGAD. */
static void
vte_terminal_realize(GtkWidget *widget)
{
	GdkWindow *window;
	VteTerminal *terminal;
	GdkWindowAttr attributes;
	GtkAllocation allocation;
        GdkColor color;
	guint attributes_mask = 0, i;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_realize()\n");

	terminal = VTE_TERMINAL(widget);
	gtk_widget_get_allocation (widget, &allocation);

	/* Create the draw structure if we don't already have one. */
	if (terminal->pvt->draw == NULL) {
		terminal->pvt->draw = _vte_draw_new(&terminal->widget);
	}

	/* Create the stock cursors. */
	terminal->pvt->mouse_cursor_visible = TRUE;
	terminal->pvt->mouse_default_cursor =
		vte_terminal_cursor_new(terminal, VTE_DEFAULT_CURSOR);
	terminal->pvt->mouse_mousing_cursor =
		vte_terminal_cursor_new(terminal, VTE_MOUSING_CURSOR);

	/* Create a GDK window for the widget. */
	attributes.window_type = GDK_WINDOW_CHILD;
	attributes.x = allocation.x;
	attributes.y = allocation.y;
	attributes.width = allocation.width;
	attributes.height = allocation.height;
	attributes.wclass = GDK_INPUT_OUTPUT;
	attributes.visual = gtk_widget_get_visual (widget);
#if !GTK_CHECK_VERSION (2, 90, 8)
	attributes.colormap = gtk_widget_get_colormap (widget);
#endif
	attributes.event_mask = gtk_widget_get_events(widget) |
				GDK_EXPOSURE_MASK |
				GDK_VISIBILITY_NOTIFY_MASK |
				GDK_FOCUS_CHANGE_MASK |
				GDK_BUTTON_PRESS_MASK |
				GDK_BUTTON_RELEASE_MASK |
				GDK_POINTER_MOTION_MASK |
				GDK_BUTTON1_MOTION_MASK |
				GDK_ENTER_NOTIFY_MASK |
				GDK_LEAVE_NOTIFY_MASK |
				GDK_KEY_PRESS_MASK |
				GDK_KEY_RELEASE_MASK;
	attributes.cursor = terminal->pvt->mouse_default_cursor;
	attributes_mask = GDK_WA_X |
			  GDK_WA_Y |
			  (attributes.visual ? GDK_WA_VISUAL : 0) |
#if !GTK_CHECK_VERSION (2, 90, 8)
			  (attributes.colormap ? GDK_WA_COLORMAP : 0) |
#endif
			  GDK_WA_CURSOR;

	window = gdk_window_new (gtk_widget_get_parent_window (widget),
				 &attributes, attributes_mask);

	gtk_widget_set_window (widget, window);
	gdk_window_set_user_data (window, widget);
	_VTE_DEBUG_IF (VTE_DEBUG_UPDATES) gdk_window_set_debug_updates (TRUE);

	/* Set the realized flag. */
	gtk_widget_set_realized (widget, TRUE);

	/* Set up the desired palette. */
	if (!terminal->pvt->palette_initialized) {
		vte_terminal_set_default_colors(terminal);
	}

	/* Allocate colors. */
	for (i = 0; i < G_N_ELEMENTS(terminal->pvt->palette); i++) {
		color.red = terminal->pvt->palette[i].red;
		color.green = terminal->pvt->palette[i].green;
		color.blue = terminal->pvt->palette[i].blue;
		color.pixel = 0;
		vte_terminal_set_color_internal(terminal, i, &color);
	}

	/* Set up input method support.  FIXME: do we need to handle the
	 * "retrieve-surrounding" and "delete-surrounding" events? */
	if (terminal->pvt->im_context != NULL) {
		vte_terminal_im_reset(terminal);
		g_object_unref(terminal->pvt->im_context);
		terminal->pvt->im_context = NULL;
	}
	terminal->pvt->im_preedit_active = FALSE;
	terminal->pvt->im_context = gtk_im_multicontext_new();
	gtk_im_context_set_client_window (terminal->pvt->im_context, window);
	g_signal_connect(terminal->pvt->im_context, "commit",
			 G_CALLBACK(vte_terminal_im_commit), terminal);
	g_signal_connect(terminal->pvt->im_context, "preedit-start",
			 G_CALLBACK(vte_terminal_im_preedit_start),
			 terminal);
	g_signal_connect(terminal->pvt->im_context, "preedit-changed",
			 G_CALLBACK(vte_terminal_im_preedit_changed),
			 terminal);
	g_signal_connect(terminal->pvt->im_context, "preedit-end",
			 G_CALLBACK(vte_terminal_im_preedit_end),
			 terminal);
	gtk_im_context_set_use_preedit(terminal->pvt->im_context, TRUE);

	/* Clear modifiers. */
	terminal->pvt->modifiers = 0;

	/* Create our invisible cursor. */
#if GTK_CHECK_VERSION (2, 15, 1)
	terminal->pvt->mouse_inviso_cursor = gdk_cursor_new_for_display(gtk_widget_get_display(widget), GDK_BLANK_CURSOR);
#else
    {
	GdkPixmap *bitmap;
	GdkColor black = {0,0,0,0};

	bitmap = gdk_bitmap_create_from_data (window, "\0", 1, 1);
	terminal->pvt->mouse_inviso_cursor = gdk_cursor_new_from_pixmap(bitmap,
									bitmap,
									&black,
									&black,
									0, 0);
	g_object_unref(bitmap);
    }
#endif /* GTK >= 2.15.1 */

#if GTK_CHECK_VERSION (2, 20, 0)
	gtk_widget_style_attach (widget);
#else
	widget->style = gtk_style_attach(widget->style, widget->window);
#endif

	vte_terminal_ensure_font (terminal);

	/* Set up the background, *now*. */
	vte_terminal_background_update(terminal);
}

static inline void
swap (guint *a, guint *b)
{
	guint tmp;
	tmp = *a, *a = *b, *b = tmp;
}

static void
vte_terminal_determine_colors_internal(VteTerminal *terminal,
				       const VteCell *cell,
				       gboolean selected,
				       gboolean cursor,
				       guint *pfore, guint *pback)
{
	guint fore, back;

	if (!cell)
		cell = &basic_cell.cell;

	/* Start with cell colors */
	fore = cell->attr.fore;
	back = cell->attr.back;

	/* Reverse-mode switches default fore and back colors */
	if (G_UNLIKELY (terminal->pvt->screen->reverse_mode)) {
		if (fore == VTE_DEF_FG)
			fore = VTE_DEF_BG;
		if (back == VTE_DEF_BG)
			back = VTE_DEF_FG;
	}

	/* Handle bold by using set bold color or brightening */
	if (cell->attr.bold) {
		if (fore == VTE_DEF_FG)
			fore = VTE_BOLD_FG;
		else if (fore < VTE_LEGACY_COLOR_SET_SIZE) {
			fore += VTE_COLOR_BRIGHT_OFFSET;
		}
	}

	/* Handle half similarly */
	if (cell->attr.half) {
		if (fore == VTE_DEF_FG)
			fore = VTE_DIM_FG;
		else if ((fore < VTE_LEGACY_COLOR_SET_SIZE))
			fore = corresponding_dim_index[fore];
	}

	/* And standout */
	if (cell->attr.standout) {
		if (back < VTE_LEGACY_COLOR_SET_SIZE)
			back += VTE_COLOR_BRIGHT_OFFSET;
	}

	/* Reverse cell? */
	if (cell->attr.reverse) {
		swap (&fore, &back);
	}

	/* Selection: use hightlight back, or inverse */
	if (selected) {
		/* XXX what if hightlight back is same color as current back? */
		if (terminal->pvt->highlight_color_set)
			back = VTE_DEF_HL;
		else
			swap (&fore, &back);
	}

	/* Cursor: use cursor back, or inverse */
	if (cursor) {
		/* XXX what if cursor back is same color as current back? */
		if (terminal->pvt->cursor_color_set)
			back = VTE_CUR_BG;
		else
			swap (&fore, &back);
	}

	/* Invisible? */
	if (cell && cell->attr.invisible) {
		fore = back;
	}

	*pfore = fore;
	*pback = back;
}

static inline void
vte_terminal_determine_colors (VteTerminal *terminal,
			       const VteCell *cell,
			       gboolean highlight,
			       guint *fore, guint *back)
{
	return vte_terminal_determine_colors_internal (terminal, cell,
						       highlight, FALSE,
						       fore, back);
}

static inline void
vte_terminal_determine_cursor_colors (VteTerminal *terminal,
				      const VteCell *cell,
				      gboolean highlight,
				      guint *fore, guint *back)
{
	return vte_terminal_determine_colors_internal (terminal, cell,
						       highlight, TRUE,
						       fore, back);
}

/* Check if a unicode character is actually a graphic character we draw
 * ourselves to handle cases where fonts don't have glyphs for them. */
static gboolean
vte_unichar_is_local_graphic(vteunistr c)
{
	if ((c >= 0x2500) && (c <= 0x257f)) {
		return TRUE;
	}
	switch (c) {
	case 0x00a3: /* british pound */
	case 0x00b0: /* degree */
	case 0x00b1: /* plus/minus */
	case 0x00b7: /* bullet */
	case 0x03c0: /* pi */
	case 0x2190: /* left arrow */
	case 0x2191: /* up arrow */
	case 0x2192: /* right arrow */
	case 0x2193: /* down arrow */
	case 0x2260: /* != */
	case 0x2264: /* <= */
	case 0x2265: /* >= */
	case 0x23ba: /* scanline 1/9 */
	case 0x23bb: /* scanline 3/9 */
	case 0x23bc: /* scanline 7/9 */
	case 0x23bd: /* scanline 9/9 */
	case 0x2409: /* HT symbol */
	case 0x240a: /* LF symbol */
	case 0x240b: /* VT symbol */
	case 0x240c: /* FF symbol */
	case 0x240d: /* CR symbol */
	case 0x2424: /* NL symbol */
	case 0x2592: /* checkerboard */
	case 0x25ae: /* solid rectangle */
	case 0x25c6: /* diamond */
		return TRUE;
		break;
	default:
		break;
	}
	return FALSE;
}
static gboolean
vte_terminal_unichar_is_local_graphic(VteTerminal *terminal, vteunistr c, gboolean bold)
{
	return vte_unichar_is_local_graphic (c) &&
		!_vte_draw_has_char (terminal->pvt->draw, c, bold);
}

static void
vte_terminal_fill_rectangle(VteTerminal *terminal,
			    const PangoColor *color,
			    gint x,
			    gint y,
			    gint width,
			    gint height)
{
	_vte_draw_start(terminal->pvt->draw);
	_vte_draw_fill_rectangle(terminal->pvt->draw,
				 x + terminal->pvt->inner_border.left,
                                 y + terminal->pvt->inner_border.top,
				 width, height,
				 color, VTE_DRAW_OPAQUE);
	_vte_draw_end(terminal->pvt->draw);
}

static void
vte_terminal_draw_line(VteTerminal *terminal,
		       const PangoColor *color,
		       gint x,
		       gint y,
		       gint xp,
		       gint yp)
{
	vte_terminal_fill_rectangle(terminal, color,
				    x, y,
				    MAX(VTE_LINE_WIDTH, xp - x + 1), MAX(VTE_LINE_WIDTH, yp - y + 1));
}

static void
vte_terminal_draw_rectangle(VteTerminal *terminal,
			    const PangoColor *color,
			    gint x,
			    gint y,
			    gint width,
			    gint height)
{
	_vte_draw_start(terminal->pvt->draw);
	_vte_draw_draw_rectangle(terminal->pvt->draw,
				 x + terminal->pvt->inner_border.left,
                                 y + terminal->pvt->inner_border.top,
				 width, height,
				 color, VTE_DRAW_OPAQUE);
	_vte_draw_end(terminal->pvt->draw);
}

static void
vte_terminal_draw_point(VteTerminal *terminal,
			const PangoColor *color,
			gint x,
			gint y)
{
	vte_terminal_fill_rectangle(terminal, color, x, y, 1, 1);
}

/* Draw the graphic representation of a line-drawing or special graphics
 * character. */
static gboolean
vte_terminal_draw_graphic(VteTerminal *terminal, vteunistr c,
			  guint fore, guint back, gboolean draw_default_bg,
			  gint x, gint y,
			  gint column_width, gint columns, gint row_height,
			  gboolean bold)
{
	gboolean ret;
	gint xcenter, xright, ycenter, ybottom, i;
	struct _vte_draw_text_request request;

	request.c = c;
	request.x = x + terminal->pvt->inner_border.left;
	request.y = y + terminal->pvt->inner_border.top;
	request.columns = columns;

	xright = x + column_width * columns;
	ybottom = y + row_height;
	xcenter = (x + xright) / 2;
	ycenter = (y + ybottom) / 2;

	if ((back != VTE_DEF_BG) || draw_default_bg) {
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[back],
					    x, y,
					    column_width * columns, row_height);
	}

	if (_vte_draw_char(terminal->pvt->draw, &request,
			   &terminal->pvt->palette[fore], VTE_DRAW_OPAQUE, bold)) {
		/* We were able to draw with actual fonts. */
		return TRUE;
	}

	ret = TRUE;

	switch (c) {
	case 124:
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* != */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2 - 1, ycenter,
				       (xright + xcenter) / 2 + 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2 - 1,
				       (ybottom + ycenter) / 2,
				       (xright + xcenter) / 2 + 1,
				       (ybottom + ycenter) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, y + 1,
				       x + 1, ybottom - 1);
		break;
	case 127:
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* A "delete" symbol I saw somewhere. */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, ycenter,
				       xcenter, y);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, y,
				       xright - 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, ycenter,
				       xright - 1, ybottom - 1);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, ybottom - 1,
				       x, ybottom - 1);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, ybottom - 1,
				       x, ycenter);
		break;
	case 0x00a3:
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* British pound.  An "L" with a hyphen. */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2,
				       (y + ycenter) / 2,
				       (x + xcenter) / 2,
				       (ycenter + ybottom) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2,
				       (ycenter + ybottom) / 2,
				       (xcenter + xright) / 2,
				       (ycenter + ybottom) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, ycenter,
				       xcenter + 1, ycenter);
		break;
	case 0x00b0: /* f */
		/* litle circle */
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter - 1, ycenter);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter + 1, ycenter);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter, ycenter - 1);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter, ycenter + 1);
		break;
	case 0x00b1: /* g */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* +/- */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter,
				       (y + ycenter) / 2,
				       xcenter,
				       (ycenter + ybottom) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2,
				       ycenter,
				       (xcenter + xright) / 2,
				       ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2,
				       (ycenter + ybottom) / 2,
				       (xcenter + xright) / 2,
				       (ycenter + ybottom) / 2);
		break;
	case 0x00b7:
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* short hyphen? */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter - 1, ycenter,
				       xcenter + 1, ycenter);
		break;
	case 0x3c0: /* pi */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2 - 1,
				       (y + ycenter) / 2,
				       (xright + xcenter) / 2 + 1,
				       (y + ycenter) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2,
				       (y + ycenter) / 2,
				       (x + xcenter) / 2,
				       (ybottom + ycenter) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (xright + xcenter) / 2,
				       (y + ycenter) / 2,
				       (xright + xcenter) / 2,
				       (ybottom + ycenter) / 2);
		break;
	/* case 0x2190: FIXME */
	/* case 0x2191: FIXME */
	/* case 0x2192: FIXME */
	/* case 0x2193: FIXME */
	/* case 0x2260: FIXME */
	case 0x2264: /* y */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* <= */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, y,
				       x, (y + ycenter) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, (y + ycenter) / 2,
				       xright - 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, ycenter,
				       xright - 1, (ycenter + ybottom) / 2);
		break;
	case 0x2265: /* z */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* >= */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       xright - 1, (y + ycenter) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, (y + ycenter) / 2,
				       x, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, ycenter,
				       x, (ycenter + ybottom) / 2);
		break;
	case 0x23ba: /* o */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x, y,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x23bb: /* p */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x, (y + ycenter) / 2,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x23bc: /* r */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    (ycenter + ybottom) / 2,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x23bd: /* s */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ybottom - 1,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x2409:  /* b */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* H */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       x, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, y,
				       xcenter, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, (y + ycenter) / 2,
				       xcenter, (y + ycenter) / 2);
		/* T */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xright - 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (xcenter + xright) / 2, ycenter,
				       (xcenter + xright) / 2, ybottom - 1);
		break;
	case 0x240a: /* e */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* L */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       x, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, ycenter,
				       xcenter, ycenter);
		/* F */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xcenter, ybottom - 1);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xright - 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, (ycenter + ybottom) / 2,
				       xright - 1, (ycenter + ybottom) / 2);
		break;
	case 0x240b: /* i */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* V */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       (x + xcenter) / 2, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (x + xcenter) / 2, ycenter,
				       xcenter, y);
		/* T */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xright - 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       (xcenter + xright) / 2, ycenter,
				       (xcenter + xright) / 2, ybottom - 1);
		break;
	case 0x240c:  /* c */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* F */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       x, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       xcenter, y);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, (y + ycenter) / 2,
				       xcenter, (y + ycenter) / 2);
		/* F */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xcenter, ybottom - 1);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xright - 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, (ycenter + ybottom) / 2,
				       xright - 1, (ycenter + ybottom) / 2);
		break;
	case 0x240d: /* d */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* C */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       x, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       xcenter, y);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, ycenter,
				       xcenter, ycenter);
		/* R */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xcenter, ybottom - 1);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xright - 1, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, ycenter,
				       xright - 1, (ycenter + ybottom) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xright - 1, (ycenter + ybottom) / 2,
				       xcenter, (ycenter + ybottom) / 2);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, (ycenter + ybottom) / 2,
				       xright - 1, ybottom - 1);
		break;
	case 0x2424: /* h */
		xcenter--;
		ycenter--;
		xright--;
		ybottom--;
		/* N */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       x, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       x, y,
				       xcenter, ycenter);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, y,
				       xcenter, ycenter);
		/* L */
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ycenter,
				       xcenter, ybottom - 1);
		vte_terminal_draw_line(terminal,
				       &terminal->pvt->palette[fore],
				       xcenter, ybottom - 1,
				       xright - 1, ybottom - 1);
		break;
	case 0x2500: /* q */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x2501:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    column_width * columns,
					    VTE_LINE_WIDTH * 2);
		break;
	case 0x2502: /* x */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH,
					    row_height);
		break;
	case 0x2503:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH * 2,
					    row_height);
		break;
	case 0x250c: /* l */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    xright - xcenter,
					    VTE_LINE_WIDTH);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    VTE_LINE_WIDTH,
					    ybottom - ycenter);
		break;
	case 0x250f:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    xright - xcenter,
					    VTE_LINE_WIDTH * 2);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    VTE_LINE_WIDTH * 2,
					    ybottom - ycenter);
		break;
	case 0x2510: /* k */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    xcenter - x + VTE_LINE_WIDTH,
					    VTE_LINE_WIDTH);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    VTE_LINE_WIDTH,
					    ybottom - ycenter);
		break;
	case 0x2513:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    xcenter - x + VTE_LINE_WIDTH * 2,
					    VTE_LINE_WIDTH * 2);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    VTE_LINE_WIDTH * 2,
					    ybottom - ycenter);
		break;
	case 0x2514: /* m */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    xright - xcenter,
					    VTE_LINE_WIDTH);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH,
					    ycenter - y + VTE_LINE_WIDTH);
		break;
	case 0x2517:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    xright - xcenter,
					    VTE_LINE_WIDTH * 2);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH * 2,
					    ycenter - y + VTE_LINE_WIDTH * 2);
		break;
	case 0x2518: /* j */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    xcenter - x + VTE_LINE_WIDTH,
					    VTE_LINE_WIDTH);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH,
					    ycenter - y + VTE_LINE_WIDTH);
		break;
	case 0x251b:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    xcenter - x + VTE_LINE_WIDTH * 2,
					    VTE_LINE_WIDTH * 2);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH * 2,
					    ycenter - y + VTE_LINE_WIDTH * 2);
		break;
	case 0x251c: /* t */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH,
					    row_height);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    xright - xcenter,
					    VTE_LINE_WIDTH);
		break;
	case 0x2523:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH * 2,
					    row_height);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    xright - xcenter,
					    VTE_LINE_WIDTH * 2);
		break;
	case 0x2524: /* u */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH,
					    row_height);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    xcenter - x + VTE_LINE_WIDTH,
					    VTE_LINE_WIDTH);
		break;
	case 0x252b:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH * 2,
					    row_height);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    xcenter - x + VTE_LINE_WIDTH * 2,
					    VTE_LINE_WIDTH * 2);
		break;
	case 0x252c: /* w */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    VTE_LINE_WIDTH,
					    ybottom - ycenter);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x2533:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    ycenter,
					    VTE_LINE_WIDTH * 2,
					    ybottom - ycenter);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    column_width * columns,
					    VTE_LINE_WIDTH * 2);
		break;
	case 0x2534: /* v */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH,
					    ycenter - y + VTE_LINE_WIDTH);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x253c: /* n */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH,
					    row_height);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    column_width * columns,
					    VTE_LINE_WIDTH);
		break;
	case 0x254b:
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    xcenter,
					    y,
					    VTE_LINE_WIDTH * 2,
					    row_height);
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x,
					    ycenter,
					    column_width * columns,
					    VTE_LINE_WIDTH * 2);
		break;
	case 0x2592:  /* a */
		for (i = x; i < xright + 1; i++) {
			gint j, draw = ((i - x) & 1) == 0;
			for (j = y; j < ybottom; j++) {
				if (draw) {
					vte_terminal_draw_point(terminal,
								&terminal->pvt->palette[fore],
								i, j);
				}
				draw = !draw;
			}
		}
		break;
	case 0x25ae: /* solid rectangle */
		vte_terminal_fill_rectangle(terminal,
					    &terminal->pvt->palette[fore],
					    x, y,
					    xright - x, ybottom - y);
		break;
	case 0x25c6:
		/* diamond */
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter - 2, ycenter);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter + 2, ycenter);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter, ycenter - 2);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter, ycenter + 2);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter - 1, ycenter - 1);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter - 1, ycenter + 1);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter + 1, ycenter - 1);
		vte_terminal_draw_point(terminal,
					&terminal->pvt->palette[fore],
					xcenter + 1, ycenter + 1);
		break;
	default:
		ret = FALSE;
		break;
	}
	return ret;
}

/* Draw a string of characters with similar attributes. */
static void
vte_terminal_draw_cells(VteTerminal *terminal,
			struct _vte_draw_text_request *items, gssize n,
			guint fore, guint back, gboolean clear,
			gboolean draw_default_bg,
			gboolean bold, gboolean underline,
			gboolean strikethrough, gboolean hilite, gboolean boxed,
			gint column_width, gint row_height)
{
	int i, x, y, ascent;
	gint columns = 0;
	PangoColor *fg, *bg, *defbg;

	g_assert(n > 0);
	_VTE_DEBUG_IF(VTE_DEBUG_CELLS) {
		GString *str = g_string_new (NULL);
		gchar *tmp;
		for (i = 0; i < n; i++) {
			g_string_append_unichar (str, items[i].c);
		}
		tmp = g_string_free (str, FALSE);
		g_printerr ("draw_cells('%s', fore=%d, back=%d, bold=%d,"
				" ul=%d, strike=%d, hilite=%d, boxed=%d)\n",
				tmp, fore, back, bold,
				underline, strikethrough, hilite, boxed);
		g_free (tmp);
	}

	bold = bold && terminal->pvt->allow_bold;
	fg = &terminal->pvt->palette[fore];
	bg = &terminal->pvt->palette[back];
	defbg = &terminal->pvt->palette[VTE_DEF_BG];
	ascent = terminal->char_ascent;

	i = 0;
	do {
		columns = 0;
		x = items[i].x;
		y = items[i].y;
		for (; i < n && items[i].y == y; i++) {
			/* Adjust for the border. */
			items[i].x += terminal->pvt->inner_border.left;
			items[i].y += terminal->pvt->inner_border.top;
			columns += items[i].columns;
		}
		if (clear && (draw_default_bg || bg != defbg)) {
			_vte_draw_fill_rectangle(terminal->pvt->draw,
					x + terminal->pvt->inner_border.left,
                                        y + terminal->pvt->inner_border.top,
					columns * column_width + bold,
					row_height,
					bg, VTE_DRAW_OPAQUE);
		}
	} while (i < n);
	_vte_draw_text(terminal->pvt->draw,
			items, n,
			fg, VTE_DRAW_OPAQUE, bold);
	for (i = 0; i < n; i++) {
		/* Deadjust for the border. */
		items[i].x -= terminal->pvt->inner_border.left;
		items[i].y -= terminal->pvt->inner_border.top;
	}

	/* Draw whatever SFX are required. */
	if (underline | strikethrough | hilite | boxed) {
		i = 0;
		do {
			x = items[i].x;
			y = items[i].y;
			for (columns = 0; i < n && items[i].y == y; i++) {
				columns += items[i].columns;
			}
			if (underline) {
				vte_terminal_draw_line(terminal,
						&terminal->pvt->palette[fore],
						x,
						y + terminal->pvt->underline_position,
						x + (columns * column_width) - 1,
						y + terminal->pvt->underline_position + terminal->pvt->line_thickness - 1);
			}
			if (strikethrough) {
				vte_terminal_draw_line(terminal,
						&terminal->pvt->palette[fore],
						x,
						y + terminal->pvt->strikethrough_position,
						x + (columns * column_width) - 1,
						y + terminal->pvt->strikethrough_position + terminal->pvt->line_thickness - 1);
			}
			if (hilite) {
				vte_terminal_draw_line(terminal,
						&terminal->pvt->palette[fore],
						x,
						y + row_height - 1,
						x + (columns * column_width) - 1,
						y + row_height - 1);
			}
			if (boxed) {
				vte_terminal_draw_rectangle(terminal,
						&terminal->pvt->palette[fore],
						x, y,
						MAX(0, (columns * column_width)),
						MAX(0, row_height));
			}
		}while (i < n);
	}
}

/* Try to map a PangoColor to a palette entry and return its index. */
static guint
_vte_terminal_map_pango_color(VteTerminal *terminal, PangoColor *color)
{
	long distance[G_N_ELEMENTS(terminal->pvt->palette)];
	guint i, ret;

	/* Calculate a "distance" value.  Could stand to be improved a bit. */
	for (i = 0; i < G_N_ELEMENTS(distance); i++) {
		const PangoColor *entry = &terminal->pvt->palette[i];
		distance[i] = 0;
		distance[i] += ((entry->red >> 8) - (color->red >> 8)) *
			       ((entry->red >> 8) - (color->red >> 8));
		distance[i] += ((entry->blue >> 8) - (color->blue >> 8)) *
			       ((entry->blue >> 8) - (color->blue >> 8));
		distance[i] += ((entry->green >> 8) - (color->green >> 8)) *
			       ((entry->green >> 8) - (color->green >> 8));
	}

	/* Find the index of the minimum value. */
	ret = 0;
	for (i = 1; i < G_N_ELEMENTS(distance); i++) {
		if (distance[i] < distance[ret]) {
			ret = i;
		}
	}

	_vte_debug_print(VTE_DEBUG_UPDATES,
			"mapped PangoColor(%04x,%04x,%04x) to "
			"palette entry (%04x,%04x,%04x)\n",
			color->red, color->green, color->blue,
			terminal->pvt->palette[ret].red,
			terminal->pvt->palette[ret].green,
			terminal->pvt->palette[ret].blue);

	return ret;
}

/* FIXME: we don't have a way to tell GTK+ what the default text attributes
 * should be, so for now at least it's assuming white-on-black is the norm and
 * is using "black-on-white" to signify "inverse".  Pick up on that state and
 * fix things.  Do this here, so that if we suddenly get red-on-black, we'll do
 * the right thing. */
static void
_vte_terminal_fudge_pango_colors(VteTerminal *terminal, GSList *attributes,
				 VteCell *cells, gssize n)
{
	int i, sumlen = 0;
	struct _fudge_cell_props{
		gboolean saw_fg, saw_bg;
		PangoColor fg, bg;
		guint index;
	}*props = g_newa (struct _fudge_cell_props, n);

	for (i = 0; i < n; i++) {
		gchar ubuf[7];
		gint len = g_unichar_to_utf8 (cells[i].c, ubuf);
		props[i].index = sumlen;
		props[i].saw_fg = props[i].saw_bg = FALSE;
		sumlen += len;
	}

	while (attributes != NULL) {
		PangoAttribute *attr = attributes->data;
		PangoAttrColor *color;
		switch (attr->klass->type) {
		case PANGO_ATTR_FOREGROUND:
			for (i = 0; i < n; i++) {
				if (props[i].index < attr->start_index) {
					continue;
				}
				if (props[i].index >= attr->end_index) {
					break;
				}
				props[i].saw_fg = TRUE;
				color = (PangoAttrColor*) attr;
				props[i].fg = color->color;
			}
			break;
		case PANGO_ATTR_BACKGROUND:
			for (i = 0; i < n; i++) {
				if (props[i].index < attr->start_index) {
					continue;
				}
				if (props[i].index >= attr->end_index) {
					break;
				}
				props[i].saw_bg = TRUE;
				color = (PangoAttrColor*) attr;
				props[i].bg = color->color;
			}
			break;
		default:
			break;
		}
		attributes = g_slist_next(attributes);
	}

	for (i = 0; i < n; i++) {
		if (props[i].saw_fg && props[i].saw_bg &&
				(props[i].fg.red == 0xffff) &&
				(props[i].fg.green == 0xffff) &&
				(props[i].fg.blue == 0xffff) &&
				(props[i].bg.red == 0) &&
				(props[i].bg.green == 0) &&
				(props[i].bg.blue == 0)) {
			cells[i].attr.fore = terminal->pvt->screen->color_defaults.attr.fore;
			cells[i].attr.back = terminal->pvt->screen->color_defaults.attr.back;
			cells[i].attr.reverse = TRUE;
		}
	}
}

/* Apply the attribute given in the PangoAttribute to the list of cells. */
static void
_vte_terminal_apply_pango_attr(VteTerminal *terminal, PangoAttribute *attr,
			       VteCell *cells, guint n_cells)
{
	guint i, ival;
	PangoAttrInt *attrint;
	PangoAttrColor *attrcolor;

	switch (attr->klass->type) {
	case PANGO_ATTR_FOREGROUND:
	case PANGO_ATTR_BACKGROUND:
		attrcolor = (PangoAttrColor*) attr;
		ival = _vte_terminal_map_pango_color(terminal,
						     &attrcolor->color);
		for (i = attr->start_index;
		     i < attr->end_index && i < n_cells;
		     i++) {
			if (attr->klass->type == PANGO_ATTR_FOREGROUND) {
				cells[i].attr.fore = ival;
			}
			if (attr->klass->type == PANGO_ATTR_BACKGROUND) {
				cells[i].attr.back = ival;
			}
		}
		break;
	case PANGO_ATTR_STRIKETHROUGH:
		attrint = (PangoAttrInt*) attr;
		ival = attrint->value;
		for (i = attr->start_index;
		     (i < attr->end_index) && (i < n_cells);
		     i++) {
			cells[i].attr.strikethrough = (ival != FALSE);
		}
		break;
	case PANGO_ATTR_UNDERLINE:
		attrint = (PangoAttrInt*) attr;
		ival = attrint->value;
		for (i = attr->start_index;
		     (i < attr->end_index) && (i < n_cells);
		     i++) {
			cells[i].attr.underline = (ival != PANGO_UNDERLINE_NONE);
		}
		break;
	case PANGO_ATTR_WEIGHT:
		attrint = (PangoAttrInt*) attr;
		ival = attrint->value;
		for (i = attr->start_index;
		     (i < attr->end_index) && (i < n_cells);
		     i++) {
			cells[i].attr.bold = (ival >= PANGO_WEIGHT_BOLD);
		}
		break;
	default:
		break;
	}
}

/* Convert a PangoAttrList and a location in that list to settings in a
 * charcell structure.  The cells array is assumed to contain enough items
 * so that all ranges in the attribute list can be mapped into the array, which
 * typically means that the cell array should have the same length as the
 * string (byte-wise) which the attributes describe. */
static void
_vte_terminal_pango_attribute_destroy(gpointer attr, gpointer data)
{
	pango_attribute_destroy(attr);
}
static void
_vte_terminal_translate_pango_cells(VteTerminal *terminal, PangoAttrList *attrs,
				    VteCell *cells, guint n_cells)
{
	PangoAttribute *attr;
	PangoAttrIterator *attriter;
	GSList *list, *listiter;
	guint i;

	for (i = 0; i < n_cells; i++) {
		cells[i] = terminal->pvt->screen->fill_defaults;
	}

	attriter = pango_attr_list_get_iterator(attrs);
	if (attriter != NULL) {
		do {
			list = pango_attr_iterator_get_attrs(attriter);
			if (list != NULL) {
				for (listiter = list;
				     listiter != NULL;
				     listiter = g_slist_next(listiter)) {
					attr = listiter->data;
					_vte_terminal_apply_pango_attr(terminal,
								       attr,
								       cells,
								       n_cells);
				}
				attr = list->data;
				_vte_terminal_fudge_pango_colors(terminal,
								 list,
								 cells +
								 attr->start_index,
								 attr->end_index -
								 attr->start_index);
				g_slist_foreach(list,
						_vte_terminal_pango_attribute_destroy,
						NULL);
				g_slist_free(list);
			}
		} while (pango_attr_iterator_next(attriter) == TRUE);
		pango_attr_iterator_destroy(attriter);
	}
}

/* Draw the listed items using the given attributes.  Tricky because the
 * attribute string is indexed by byte in the UTF-8 representation of the string
 * of characters.  Because we draw a character at a time, this is slower. */
static void
vte_terminal_draw_cells_with_attributes(VteTerminal *terminal,
					struct _vte_draw_text_request *items,
					gssize n,
					PangoAttrList *attrs,
					gboolean draw_default_bg,
					gint column_width, gint height)
{
	int i, j, cell_count;
	VteCell *cells;
	char scratch_buf[VTE_UTF8_BPC];
	guint fore, back;

	/* Note: since this function is only called with the pre-edit text,
	 * all the items contain gunichar only, not vteunistr. */

	for (i = 0, cell_count = 0; i < n; i++) {
		cell_count += g_unichar_to_utf8(items[i].c, scratch_buf);
	}
	cells = g_new(VteCell, cell_count);
	_vte_terminal_translate_pango_cells(terminal, attrs, cells, cell_count);
	for (i = 0, j = 0; i < n; i++) {
		vte_terminal_determine_colors(terminal, &cells[j], FALSE, &fore, &back);
		vte_terminal_draw_cells(terminal, items + i, 1,
					fore,
					back,
					TRUE, draw_default_bg,
					cells[j].attr.bold,
					cells[j].attr.underline,
					cells[j].attr.strikethrough,
					FALSE, FALSE, column_width, height);
		j += g_unichar_to_utf8(items[i].c, scratch_buf);
	}
	g_free(cells);
}


/* Paint the contents of a given row at the given location.  Take advantage
 * of multiple-draw APIs by finding runs of characters with identical
 * attributes and bundling them together. */
static void
vte_terminal_draw_rows(VteTerminal *terminal,
		      VteScreen *screen,
		      gint start_row, gint row_count,
		      gint start_column, gint column_count,
		      gint start_x, gint start_y,
		      gint column_width, gint row_height)
{
	struct _vte_draw_text_request items[4*VTE_DRAW_MAX_LENGTH];
	gint i, j, row, rows, x, y, end_column;
	guint fore, nfore, back, nback;
	glong delta;
	gboolean underline, nunderline, bold, nbold, hilite, nhilite,
		 selected, nselected, strikethrough, nstrikethrough;
	guint item_count;
	const VteCell *cell;
	const VteRowData *row_data;

	/* adjust for the absolute start of row */
	start_x -= start_column * column_width;
	end_column = start_column + column_count;

	/* clear the background */
	delta = screen->scroll_delta;
	x = start_x + terminal->pvt->inner_border.left;
	y = start_y + terminal->pvt->inner_border.top;
	row = start_row;
	rows = row_count;
	do {
		row_data = _vte_terminal_find_row_data(terminal, row);
		/* Back up in case this is a multicolumn character,
		 * making the drawing area a little wider. */
		i = start_column;
		if (row_data != NULL) {
			cell = _vte_row_data_get (row_data, i);
			if (cell != NULL) {
				while (cell->attr.fragment && i > 0) {
					cell = _vte_row_data_get (row_data, --i);
				}
			}
			/* Walk the line. */
			do {
				/* Get the character cell's contents. */
				cell = _vte_row_data_get (row_data, i);
				/* Find the colors for this cell. */
				selected = vte_cell_is_selected(terminal, i, row, NULL);
				vte_terminal_determine_colors(terminal, cell, selected, &fore, &back);

				bold = cell && cell->attr.bold;
				j = i + (cell ? cell->attr.columns : 1);

				while (j < end_column){
					/* Retrieve the cell. */
					cell = _vte_row_data_get (row_data, j);
					/* Don't render fragments of multicolumn characters
					 * which have the same attributes as the initial
					 * portions. */
					if (cell != NULL && cell->attr.fragment) {
						j++;
						continue;
					}
					/* Resolve attributes to colors where possible and
					 * compare visual attributes to the first character
					 * in this chunk. */
					selected = vte_cell_is_selected(terminal, j, row, NULL);
					vte_terminal_determine_colors(terminal, cell, selected, &nfore, &nback);
					if (nback != back) {
						break;
					}
					bold = cell && cell->attr.bold;
					j += cell ? cell->attr.columns : 1;
				}
				if (back != VTE_DEF_BG) {
					_vte_draw_fill_rectangle (
							terminal->pvt->draw,
							x + i * column_width,
							y,
							(j - i) * column_width + bold,
							row_height,
							&terminal->pvt->palette[back], VTE_DRAW_OPAQUE);
				}
				/* We'll need to continue at the first cell which didn't
				 * match the first one in this set. */
				i = j;
			} while (i < end_column);
		} else {
			do {
				selected = vte_cell_is_selected(terminal, i, row, NULL);
				j = i + 1;
				while (j < end_column){
					nselected = vte_cell_is_selected(terminal, j, row, NULL);
					if (nselected != selected) {
						break;
					}
					j++;
				}
				vte_terminal_determine_colors(terminal, NULL, selected, &fore, &back);
				if (back != VTE_DEF_BG) {
					_vte_draw_fill_rectangle (terminal->pvt->draw,
								  x + i *column_width,
								  y,
								  (j - i)  * column_width,
								  row_height,
								  &terminal->pvt->palette[back], VTE_DRAW_OPAQUE);
				}
				i = j;
			} while (i < end_column);
		}
		row++;
		y += row_height;
	} while (--rows);


	/* render the text */
	y = start_y;
	row = start_row;
	rows = row_count;
	item_count = 1;
	do {
		row_data = _vte_terminal_find_row_data(terminal, row);
		if (row_data == NULL) {
			goto fg_skip_row;
		}
		/* Back up in case this is a multicolumn character,
		 * making the drawing area a little wider. */
		i = start_column;
		cell = _vte_row_data_get (row_data, i);
		if (cell == NULL) {
			goto fg_skip_row;
		}
		while (cell->attr.fragment && i > 0)
			cell = _vte_row_data_get (row_data, --i);

		/* Walk the line. */
		do {
			/* Get the character cell's contents. */
			cell = _vte_row_data_get (row_data, i);
			if (cell == NULL) {
				goto fg_skip_row;
			}
			while (cell->c == 0 || cell->attr.invisible ||
					(cell->c == ' ' &&
					 !cell->attr.underline &&
					 !cell->attr.strikethrough) ||
					cell->attr.fragment) {
				if (++i >= end_column) {
					goto fg_skip_row;
				}
				cell = _vte_row_data_get (row_data, i);
				if (cell == NULL) {
					goto fg_skip_row;
				}
			}
			/* Find the colors for this cell. */
			selected = vte_cell_is_selected(terminal, i, row, NULL);
			vte_terminal_determine_colors(terminal, cell, selected, &fore, &back);
			underline = cell->attr.underline;
			strikethrough = cell->attr.strikethrough;
			bold = cell->attr.bold;
			if (terminal->pvt->show_match) {
				hilite = vte_cell_is_between(i, row,
						terminal->pvt->match_start.col,
						terminal->pvt->match_start.row,
						terminal->pvt->match_end.col,
						terminal->pvt->match_end.row,
						TRUE);
			} else {
				hilite = FALSE;
			}

			items[0].c = cell->c;
			items[0].columns = cell->attr.columns;
			items[0].x = start_x + i * column_width;
			items[0].y = y;
			j = i + items[0].columns;

			/* If this is a graphics character, draw it locally. */
			if (vte_terminal_unichar_is_local_graphic(terminal, cell->c, cell->attr.bold)) {
				if (vte_terminal_draw_graphic(terminal,
							items[0].c,
							fore, back,
							FALSE,
							items[0].x,
							items[0].y,
							column_width,
							items[0].columns,
							row_height,
							cell->attr.bold)) {
					i = j;
					continue;
				}
			}

			/* Now find out how many cells have the same attributes. */
			do {
				while (j < end_column &&
						item_count < G_N_ELEMENTS(items)) {
					/* Retrieve the cell. */
					cell = _vte_row_data_get (row_data, j);
					if (cell == NULL) {
						goto fg_next_row;
					}
					/* Don't render blank cells or fragments of multicolumn characters
					 * which have the same attributes as the initial
					 * portions.  Don't render invisible cells */
					if (cell->attr.fragment || cell->attr.invisible) {
						j++;
						continue;
					}
					if (cell->c == 0){
						/* only break the run if we
						 * are drawing attributes
						 */
						if (underline || strikethrough || hilite) {
							break;
						} else {
							j++;
							continue;
						}
					}
					/* Resolve attributes to colors where possible and
					 * compare visual attributes to the first character
					 * in this chunk. */
					selected = vte_cell_is_selected(terminal, j, row, NULL);
					vte_terminal_determine_colors(terminal, cell, selected, &nfore, &nback);
					/* Graphic characters must be drawn individually. */
					if (vte_terminal_unichar_is_local_graphic(terminal, cell->c, cell->attr.bold)) {
						if (vte_terminal_draw_graphic(terminal,
									cell->c,
									nfore, nback,
									FALSE,
									start_x + j * column_width,
									y,
									column_width,
									cell->attr.columns,
									row_height,
									cell->attr.bold)) {

							j += cell->attr.columns;
							continue;
						}
					}
					if (nfore != fore) {
						break;
					}
					nbold = cell->attr.bold;
					if (nbold != bold) {
						break;
					}
					/* Break up underlined/not-underlined text. */
					nunderline = cell->attr.underline;
					if (nunderline != underline) {
						break;
					}
					nstrikethrough = cell->attr.strikethrough;
					if (nstrikethrough != strikethrough) {
						break;
					}
					/* Break up matched/not-matched text. */
					nhilite = FALSE;
					if (terminal->pvt->show_match) {
						nhilite = vte_cell_is_between(j, row,
								terminal->pvt->match_start.col,
								terminal->pvt->match_start.row,
								terminal->pvt->match_end.col,
								terminal->pvt->match_end.row,
								TRUE);
					}
					if (nhilite != hilite) {
						break;
					}
					/* Add this cell to the draw list. */
					items[item_count].c = cell->c;
					items[item_count].columns = cell->attr.columns;
					items[item_count].x = start_x + j * column_width;
					items[item_count].y = y;
					j +=  items[item_count].columns;
					item_count++;
				}
				/* have we encountered a state change? */
				if (j < end_column) {
					break;
				}
fg_next_row:
				/* is this the last column, on the last row? */
				do {
					do {
						if (!--rows) {
							goto fg_draw;
						}

						/* restart on the next row */
						row++;
						y += row_height;
						row_data = _vte_terminal_find_row_data(terminal, row);
					} while (row_data == NULL);

					/* Back up in case this is a
					 * multicolumn character, making the drawing
					 * area a little wider. */
					j = start_column;
					cell = _vte_row_data_get (row_data, j);
				} while (cell == NULL);
				while (cell->attr.fragment && j > 0) {
					cell = _vte_row_data_get (row_data, --j);
				}
			} while (TRUE);
fg_draw:
			/* Draw the cells. */
			vte_terminal_draw_cells(terminal,
					items,
					item_count,
					fore, back, FALSE, FALSE,
					bold, underline,
					strikethrough, hilite, FALSE,
					column_width, row_height);
			item_count = 1;
			/* We'll need to continue at the first cell which didn't
			 * match the first one in this set. */
			i = j;
			if (!rows) {
				goto fg_out;
			}
		} while (i < end_column);
fg_skip_row:
		row++;
		y += row_height;
	} while (--rows);
fg_out:
	return;
}

static void
vte_terminal_expand_region (VteTerminal *terminal, GdkRegion *region, const GdkRectangle *area)
{
	VteScreen *screen;
	int width, height;
	int row, col, row_stop, col_stop;
	VteRegionRectangle rect;

	screen = terminal->pvt->screen;

	width = terminal->char_width;
	height = terminal->char_height;

	/* increase the paint by one pixel on all sides to force the
	 * inclusion of neighbouring cells */
	row = MAX(0, (area->y - terminal->pvt->inner_border.top - 1) / height);
	row_stop = MIN(howmany(area->height + area->y - terminal->pvt->inner_border.top + 1, height),
		       terminal->row_count);
	if (row_stop <= row) {
		return;
	}
	col = MAX(0, (area->x - terminal->pvt->inner_border.left - 1) / width);
	col_stop = MIN(howmany(area->width + area->x - terminal->pvt->inner_border.left + 1, width),
		       terminal->column_count);
	if (col_stop <= col) {
		return;
	}

	rect.x = col*width + terminal->pvt->inner_border.left;
	rect.width = (col_stop - col) * width;

	rect.y = row*height + terminal->pvt->inner_border.top;
	rect.height = (row_stop - row)*height;

	/* the rect must be cell aligned to avoid overlapping XY bands */
	gdk_region_union_with_rect(region, &rect);

	_vte_debug_print (VTE_DEBUG_UPDATES,
			"vte_terminal_expand_region"
			"	(%d,%d)x(%d,%d) pixels,"
			" (%d,%d)x(%d,%d) cells"
			" [(%d,%d)x(%d,%d) pixels]\n",
			area->x, area->y, area->width, area->height,
			col, row, col_stop - col, row_stop - row,
			rect.x, rect.y, rect.width, rect.height);
}

static void
vte_terminal_paint_area (VteTerminal *terminal, const GdkRectangle *area)
{
	VteScreen *screen;
	int width, height, delta;
	int row, col, row_stop, col_stop;

	screen = terminal->pvt->screen;

	width = terminal->char_width;
	height = terminal->char_height;

	row = MAX(0, (area->y - terminal->pvt->inner_border.top) / height);
	row_stop = MIN((area->height + area->y - terminal->pvt->inner_border.top) / height,
		       terminal->row_count);
	if (row_stop <= row) {
		return;
	}
	col = MAX(0, (area->x - terminal->pvt->inner_border.left) / width);
	col_stop = MIN((area->width + area->x - terminal->pvt->inner_border.left) / width,
		       terminal->column_count);
	if (col_stop <= col) {
		return;
	}
	_vte_debug_print (VTE_DEBUG_UPDATES,
			"vte_terminal_paint_area"
			"	(%d,%d)x(%d,%d) pixels,"
			" (%d,%d)x(%d,%d) cells"
			" [(%d,%d)x(%d,%d) pixels]\n",
			area->x, area->y, area->width, area->height,
			col, row, col_stop - col, row_stop - row,
			col * width + terminal->pvt->inner_border.left,
			row * height + terminal->pvt->inner_border.top,
			(col_stop - col) * width,
			(row_stop - row) * height);

	/* Now we're ready to draw the text.  Iterate over the rows we
	 * need to draw. */
	delta = screen->scroll_delta;
	vte_terminal_draw_rows(terminal,
			      screen,
			      row + delta, row_stop - row,
			      col, col_stop - col,
			      col * width,
			      row * height,
			      width,
			      height);
}

static void
vte_terminal_paint_cursor(VteTerminal *terminal)
{
	VteScreen *screen;
	const VteCell *cell;
	struct _vte_draw_text_request item;
	int row, drow, col;
	long width, height, delta, cursor_width;
	guint fore, back;
	int x, y;
	gboolean blink, selected, focus;

	if (!terminal->pvt->cursor_visible)
		return;

	screen = terminal->pvt->screen;
	delta = screen->scroll_delta;
	col = screen->cursor_current.col;
	drow = screen->cursor_current.row;
	row = drow - delta;
	width = terminal->char_width;
	height = terminal->char_height;

	if ((CLAMP(col, 0, terminal->column_count - 1) != col) ||
	    (CLAMP(row, 0, terminal->row_count    - 1) != row))
		return;

	focus = terminal->pvt->has_focus;
	blink = terminal->pvt->cursor_blink_state;

	if (focus && !blink)
		return;

	/* Find the character "under" the cursor. */
	cell = vte_terminal_find_charcell(terminal, col, drow);
	while ((cell != NULL) && (cell->attr.fragment) && (col > 0)) {
		col--;
		cell = vte_terminal_find_charcell(terminal, col, drow);
	}

	/* Draw the cursor. */
	item.c = (cell && cell->c) ? cell->c : ' ';
	item.columns = cell ? cell->attr.columns : 1;
	item.x = col * width;
	item.y = row * height;
	cursor_width = item.columns * width;
	if (cell && cell->c != 0) {
		gint cw = _vte_draw_get_char_width (terminal->pvt->draw,
				cell->c, cell->attr.columns, cell->attr.bold);
		cursor_width = MAX(cursor_width, cw);
	}

	selected = vte_cell_is_selected(terminal, col, drow, NULL);

	vte_terminal_determine_cursor_colors(terminal, cell, selected, &fore, &back);

	x = item.x;
	y = item.y;

	switch (terminal->pvt->cursor_shape) {

		case VTE_CURSOR_SHAPE_IBEAM: {
                        int stem_width;

                        stem_width = (int) (((float) height) * terminal->pvt->cursor_aspect_ratio + 0.5);
                        stem_width = CLAMP (stem_width, VTE_LINE_WIDTH, cursor_width);
		 	
			vte_terminal_fill_rectangle(terminal, &terminal->pvt->palette[back],
						     x, y, stem_width, height);
			break;
                }

		case VTE_CURSOR_SHAPE_UNDERLINE: {
                        int line_height;

                        line_height = (int) (((float) width) * terminal->pvt->cursor_aspect_ratio + 0.5);
                        line_height = CLAMP (line_height, VTE_LINE_WIDTH, height);

			vte_terminal_fill_rectangle(terminal, &terminal->pvt->palette[back],
						     x, y + height - line_height,
						     cursor_width, line_height);
			break;
                }

		case VTE_CURSOR_SHAPE_BLOCK:

			if (focus) {
				/* just reverse the character under the cursor */
				vte_terminal_fill_rectangle (terminal,
							     &terminal->pvt->palette[back],
							     x, y,
							     cursor_width, height);

				if (!vte_terminal_unichar_is_local_graphic(terminal, item.c, cell ? cell->attr.bold : FALSE) ||
				    !vte_terminal_draw_graphic(terminal,
							       item.c,
							       fore, back,
							       TRUE,
							       item.x,
							       item.y,
							       width,
							       item.columns,
							       height,
							       cell ? cell->attr.bold : FALSE)) {
					gboolean hilite = FALSE;
					if (cell && terminal->pvt->show_match) {
						hilite = vte_cell_is_between(col, row,
								terminal->pvt->match_start.col,
								terminal->pvt->match_start.row,
								terminal->pvt->match_end.col,
								terminal->pvt->match_end.row,
								TRUE);
					}
					if (cell && cell->c != 0 && cell->c != ' ') {
						vte_terminal_draw_cells(terminal,
								&item, 1,
								fore, back, TRUE, FALSE,
								cell->attr.bold,
								cell->attr.underline,
								cell->attr.strikethrough,
								hilite,
								FALSE,
								width,
								height);
					}
				}

			} else {
				/* draw a box around the character */

				vte_terminal_draw_rectangle (terminal,
							    &terminal->pvt->palette[back],
							     x - VTE_LINE_WIDTH,
							     y - VTE_LINE_WIDTH,
							     cursor_width + 2*VTE_LINE_WIDTH,
							     height + 2*VTE_LINE_WIDTH);
			}

			break;
	}
}

static void
vte_terminal_paint_im_preedit_string(VteTerminal *terminal)
{
	VteScreen *screen;
	int row, drow, col, columns;
	long width, height, ascent, descent, delta;
	int i, len;
	guint fore, back;

	if (!terminal->pvt->im_preedit)
		return;

	/* Get going. */
	screen = terminal->pvt->screen;

	/* Keep local copies of rendering information. */
	width = terminal->char_width;
	height = terminal->char_height;
	ascent = terminal->char_ascent;
	descent = terminal->char_descent;
	delta = screen->scroll_delta;

	drow = screen->cursor_current.row;
	row = screen->cursor_current.row - delta;

	/* Find out how many columns the pre-edit string takes up. */
	columns = vte_terminal_preedit_width(terminal, FALSE);
	len = vte_terminal_preedit_length(terminal, FALSE);

	/* If the pre-edit string won't fit on the screen if we start
	 * drawing it at the cursor's position, move it left. */
	col = screen->cursor_current.col;
	if (col + columns > terminal->column_count) {
		col = MAX(0, terminal->column_count - columns);
	}

	/* Draw the preedit string, boxed. */
	if (len > 0) {
		struct _vte_draw_text_request *items;
		const char *preedit = terminal->pvt->im_preedit;
		int preedit_cursor;

		items = g_new(struct _vte_draw_text_request, len);
		for (i = columns = 0; i < len; i++) {
			items[i].c = g_utf8_get_char(preedit);
			items[i].columns = _vte_iso2022_unichar_width(terminal->pvt->iso2022,
								      items[i].c);
			items[i].x = (col + columns) * width;
			items[i].y = row * height;
			columns += items[i].columns;
			preedit = g_utf8_next_char(preedit);
		}
		_vte_draw_clear(terminal->pvt->draw,
				col * width + terminal->pvt->inner_border.left,
				row * height + terminal->pvt->inner_border.top,
				width * columns,
				height);
		fore = screen->defaults.attr.fore;
		back = screen->defaults.attr.back;
		vte_terminal_draw_cells_with_attributes(terminal,
							items, len,
							terminal->pvt->im_preedit_attrs,
							TRUE,
							width, height);
		preedit_cursor = terminal->pvt->im_preedit_cursor;
		if (preedit_cursor >= 0 && preedit_cursor < len) {
			/* Cursored letter in reverse. */
			vte_terminal_draw_cells(terminal,
						&items[preedit_cursor], 1,
						back, fore, TRUE, TRUE,
						FALSE,
						FALSE,
						FALSE,
						FALSE,
						TRUE,
						width, height);
		}
		g_free(items);
	}
}

/* Draw the widget. */
static void
vte_terminal_paint(GtkWidget *widget, GdkRegion *region)
{
	VteTerminal *terminal;
	GtkAllocation allocation;

	_vte_debug_print(VTE_DEBUG_LIFECYCLE, "vte_terminal_paint()\n");
	_vte_debug_print(VTE_DEBUG_WORK, "=");

	terminal = VTE_TERMINAL(widget);
	gtk_widget_get_allocation (widget, &allocation);

	/* Designate the start of the drawing operation and clear the area. */
	_vte_draw_start(terminal->pvt->draw);
	if (terminal->pvt->bg_transparent) {
		int x, y;
		gdk_window_get_origin (gtk_widget_get_window (widget), &x, &y);
		_vte_draw_set_background_scroll(terminal->pvt->draw, x, y);
	} else {
		if (terminal->pvt->scroll_background) {
			_vte_draw_set_background_scroll(terminal->pvt->draw,
							0,
							terminal->pvt->screen->scroll_delta *
							terminal->char_height);
		} else {
			_vte_draw_set_background_scroll(terminal->pvt->draw, 0, 0);
		}
	}

	_VTE_DEBUG_IF (VTE_DEBUG_UPDATES) {
		VteRegionRectangle clip;
		gdk_region_get_clipbox (region, &clip);
		g_printerr ("vte_terminal_paint"
				"	(%d,%d)x(%d,%d) pixels\n",
				clip.x, clip.y, clip.width, clip.height);
	}

	_vte_draw_clip(terminal->pvt->draw, region);
	gtk_widget_get_allocation(&terminal->widget, &allocation);
	_vte_draw_clear (terminal->pvt->draw, 0, 0,
			 allocation.width, allocation.height);

	/* Calculate the bounding rectangle. */
	{
		VteRegionRectangle *rectangles;
		gint n, n_rectangles;
		gdk_region_get_rectangles (region, &rectangles, &n_rectangles);
		/* don't bother to enlarge an invalidate all */
		if (!(n_rectangles == 1
		      && rectangles[0].width == allocation.width
		      && rectangles[0].height == allocation.height)) {
			GdkRegion *rr = gdk_region_new ();
			/* convert pixels into whole cells */
			for (n = 0; n < n_rectangles; n++) {
				vte_terminal_expand_region (terminal, rr, rectangles + n);
			}
			g_free (rectangles);
			gdk_region_get_rectangles (rr, &rectangles, &n_rectangles);
			gdk_region_destroy (rr);
		}

		/* and now paint them */
		for (n = 0; n < n_rectangles; n++) {
			vte_terminal_paint_area (terminal, rectangles + n);
		}
		g_free (rectangles);
	}

	vte_terminal_paint_cursor(terminal);

	vte_terminal_paint_im_preedit_string(terminal);

	/* Done with various structures. */
	_vte_draw_end(terminal->pvt->draw);
}

/* Handle an expose event by painting the exposed area. */
#if GTK_CHECK_VERSION (2, 90, 8)

static cairo_region_t *
vte_cairo_get_clip_region (cairo_t *cr)
{
        cairo_rectangle_list_t *list;
        cairo_region_t *region;
        int i;

        list = cairo_copy_clip_rectangle_list (cr);
        if (list->status == CAIRO_STATUS_CLIP_NOT_REPRESENTABLE) {
                cairo_rectangle_int_t clip_rect;

                cairo_rectangle_list_destroy (list);

                if (!gdk_cairo_get_clip_rectangle (cr, &clip_rect))
                        return NULL;
                return cairo_region_create_rectangle (&clip_rect);
        }


        region = cairo_region_create ();
        for (i = list->num_rectangles - 1; i >= 0; --i) {
                cairo_rectangle_t *rect = &list->rectangles[i];
                cairo_rectangle_int_t clip_rect;

                clip_rect.x = floor (rect->x);
                clip_rect.y = floor (rect->y);
                clip_rect.width = ceil (rect->x + rect->width) - clip_rect.x;
                clip_rect.height = ceil (rect->y + rect->height) - clip_rect.y;

                if (cairo_region_union_rectangle (region, &clip_rect) != CAIRO_STATUS_SUCCESS) {
                        cairo_region_destroy (region);
                        region = NULL;
                        break;
                }
        }

        cairo_rectangle_list_destroy (list);
        return region;
}

static gboolean
vte_terminal_draw(GtkWidget *widget,
                  cairo_t *cr)
{
        VteTerminal *terminal = VTE_TERMINAL (widget);
        cairo_rectangle_int_t clip_rect;
        cairo_region_t *region;

        if (!gdk_cairo_get_clip_rectangle (cr, &clip_rect))
                return FALSE;

        _vte_debug_print (VTE_DEBUG_WORK, "+");
        _vte_debug_print (VTE_DEBUG_EVENTS, "Draw (%d,%d)x(%d,%d)\n",
                          clip_rect.x, clip_rect.y,
                          clip_rect.width, clip_rect.height);

        region = vte_cairo_get_clip_region (cr);
        if (region == NULL)
                return FALSE;

        vte_terminal_paint(widget, region);
        cairo_region_destroy (region);

        terminal->pvt->invalidated_all = FALSE;

        return FALSE;
}

#else

static gboolean
vte_terminal_expose(GtkWidget *widget,
                    GdkEventExpose *event)
{
	VteTerminal *terminal = VTE_TERMINAL (widget);
	GtkAllocation allocation;

	/* Beware the out of order events -
	 *   do not even think about skipping exposes! */
	_vte_debug_print (VTE_DEBUG_WORK, "+");
	_vte_debug_print (VTE_DEBUG_EVENTS, "Expose (%d,%d)x(%d,%d)\n",
			event->area.x, event->area.y,
			event->area.width, event->area.height);
	if (terminal->pvt->active != NULL &&
			update_timeout_tag != 0 &&
			!in_update_timeout) {
		/* fix up a race condition where we schedule a delayed update
		 * after an 'immediate' invalidate all */
		if (terminal->pvt->invalidated_all &&
				terminal->pvt->update_regions == NULL) {
			terminal->pvt->invalidated_all = FALSE;
		}
		/* if we expect to redraw the widget soon,
		 * just add this event to the list */
		if (!terminal->pvt->invalidated_all) {
			gtk_widget_get_allocation (widget, &allocation);
			if (event->area.width >= allocation.width &&
			    event->area.height >= allocation.height) {
				_vte_invalidate_all (terminal);
			} else {
				terminal->pvt->update_regions =
					g_slist_prepend (terminal->pvt->update_regions,
							gdk_region_copy (event->region));
			}
		}
	} else {
		vte_terminal_paint(widget, event->region);
		terminal->pvt->invalidated_all = FALSE;
	}
	return FALSE;
}

#endif /* GTK 3.0 */

/* Handle a scroll event. */
static gboolean
vte_terminal_scroll(GtkWidget *widget, GdkEventScroll *event)
{
	GtkAdjustment *adj;
	VteTerminal *terminal;
	gdouble v;
	int button;

	terminal = VTE_TERMINAL(widget);

	vte_terminal_read_modifiers (terminal, (GdkEvent*) event);

	_VTE_DEBUG_IF(VTE_DEBUG_EVENTS)
		switch (event->direction) {
		case GDK_SCROLL_UP:
			g_printerr("Scroll up.\n");
			break;
		case GDK_SCROLL_DOWN:
			g_printerr("Scroll down.\n");
			break;
		default:
			break;
		}

	/* If we're running a mouse-aware application, map the scroll event
	 * to a button press on buttons four and five. */
	if (terminal->pvt->mouse_tracking_mode) {
		switch (event->direction) {
		case GDK_SCROLL_UP:
			button = 4;
			break;
		case GDK_SCROLL_DOWN:
			button = 5;
			break;
		default:
			button = 0;
			break;
		}
		if (button != 0) {
			/* Encode the parameters and send them to the app. */
			vte_terminal_send_mouse_button_internal(terminal,
								button,
								event->x,
								event->y);
		}
		return TRUE;
	}

	adj = terminal->adjustment;
	v = MAX (1., ceil (gtk_adjustment_get_page_increment (adj) / 10.));
	switch (event->direction) {
	case GDK_SCROLL_UP:
		v = -v;
		break;
	case GDK_SCROLL_DOWN:
		break;
	default:
		return FALSE;
	}

	if (terminal->pvt->screen == &terminal->pvt->alternate_screen ||
		terminal->pvt->normal_screen.scrolling_restricted) {
		char *normal;
		gssize normal_length;
		const gchar *special;
		gint i, cnt = v;

		/* In the alternate screen there is no scrolling,
		 * so fake a few cursor keystrokes. */

		_vte_keymap_map (
				cnt > 0 ? GDK_KEY (Down) : GDK_KEY (Up),
				terminal->pvt->modifiers,
				terminal->pvt->sun_fkey_mode,
				terminal->pvt->hp_fkey_mode,
				terminal->pvt->legacy_fkey_mode,
				terminal->pvt->vt220_fkey_mode,
				terminal->pvt->cursor_mode == VTE_KEYMODE_APPLICATION,
				terminal->pvt->keypad_mode == VTE_KEYMODE_APPLICATION,
				terminal->pvt->termcap,
				terminal->pvt->emulation ?
				terminal->pvt->emulation : vte_terminal_get_default_emulation(terminal),
				&normal,
				&normal_length,
				&special);
		if (cnt < 0)
			cnt = -cnt;
		for (i = 0; i < cnt; i++) {
			vte_terminal_feed_child_using_modes (terminal,
					normal, normal_length);
		}
		g_free (normal);
	} else {
		/* Perform a history scroll. */
		v += terminal->pvt->screen->scroll_delta;
		vte_terminal_queue_adjustment_value_changed_clamped (terminal, v);
	}

	return TRUE;
}

/* Create a new accessible object associated with ourselves, and return
 * it to the caller. */
static AtkObject *
vte_terminal_get_accessible(GtkWidget *widget)
{
	VteTerminal *terminal;
	static gboolean first_time = TRUE;

	terminal = VTE_TERMINAL(widget);

	if (first_time) {
		AtkObjectFactory *factory;
		AtkRegistry *registry;
		GType derived_type;
		GType derived_atk_type;

		/*
		 * Figure out whether accessibility is enabled by looking at the
		 * type of the accessible object which would be created for
		 * the parent type of VteTerminal.
		 */
		derived_type = g_type_parent (VTE_TYPE_TERMINAL);

		registry = atk_get_default_registry ();
		factory = atk_registry_get_factory (registry,
						    derived_type);

		derived_atk_type = atk_object_factory_get_accessible_type (factory);
		if (g_type_is_a (derived_atk_type, GTK_TYPE_ACCESSIBLE)) {
			atk_registry_set_factory_type (registry,
						       VTE_TYPE_TERMINAL,
						       vte_terminal_accessible_factory_get_type ());
		}
		first_time = FALSE;
	}

	return GTK_WIDGET_CLASS (vte_terminal_parent_class)->get_accessible (widget);
}

static void
vte_terminal_get_property (GObject *object,
                           guint prop_id,
                           GValue *value,
                           GParamSpec *pspec)
{
        VteTerminal *terminal = VTE_TERMINAL (object);
        VteTerminalPrivate *pvt = terminal->pvt;

	switch (prop_id)
	{
#if GTK_CHECK_VERSION (2, 91, 2)
                case PROP_HADJUSTMENT:
                        g_value_set_object (value, pvt->hadjustment);
                        break;
                case PROP_VADJUSTMENT:
                        g_value_set_object (value, terminal->adjustment);
                        break;
                case PROP_HSCROLL_POLICY:
                        g_value_set_enum (value, pvt->hscroll_policy);
                        break;
                case PROP_VSCROLL_POLICY:
                        g_value_set_enum (value, pvt->vscroll_policy);
                        break;
#endif
                case PROP_ALLOW_BOLD:
                        g_value_set_boolean (value, vte_terminal_get_allow_bold (terminal));
                        break;
                case PROP_AUDIBLE_BELL:
                        g_value_set_boolean (value, vte_terminal_get_audible_bell (terminal));
                        break;
                case PROP_BACKGROUND_IMAGE_FILE:
                        g_value_set_string (value, pvt->bg_file);
                        break;
                case PROP_BACKGROUND_IMAGE_PIXBUF:
                        g_value_set_object (value, pvt->bg_pixbuf);
                        break;
                case PROP_BACKGROUND_OPACITY:
                        g_value_set_double (value, (double) pvt->bg_opacity / (double) G_MAXUINT16);
                        break;
                case PROP_BACKGROUND_SATURATION:
                        g_value_set_double (value, (double) pvt->bg_saturation / (double) VTE_SATURATION_MAX);
                        break;
                case PROP_BACKGROUND_TINT_COLOR:
                        g_value_set_boxed (value, &pvt->bg_tint_color);
                        break;
                case PROP_BACKGROUND_TRANSPARENT:
                        g_value_set_boolean (value, pvt->bg_transparent);
                        break;
                case PROP_BACKSPACE_BINDING:
                        g_value_set_enum (value, pvt->backspace_binding);
                        break;
                case PROP_CURSOR_BLINK_MODE:
                        g_value_set_enum (value, vte_terminal_get_cursor_blink_mode (terminal));
                        break;
                case PROP_CURSOR_SHAPE:
                        g_value_set_enum (value, vte_terminal_get_cursor_shape (terminal));
                        break;
                case PROP_DELETE_BINDING:
                        g_value_set_enum (value, pvt->delete_binding);
                        break;
                case PROP_EMULATION:
                        g_value_set_string (value, vte_terminal_get_emulation (terminal));
                        break;
                case PROP_ENCODING:
                        g_value_set_string (value, vte_terminal_get_encoding (terminal));
                        break;
                case PROP_FONT_DESC:
                        g_value_set_boxed (value, vte_terminal_get_font (terminal));
                        break;
                case PROP_ICON_TITLE:
                        g_value_set_string (value, vte_terminal_get_icon_title (terminal));
                        break;
                case PROP_MOUSE_POINTER_AUTOHIDE:
                        g_value_set_boolean (value, vte_terminal_get_mouse_autohide (terminal));
                        break;
                case PROP_PTY:
                        g_value_set_int (value, pvt->pty != NULL ? vte_pty_get_fd(pvt->pty) : -1);
                        break;
                case PROP_PTY_OBJECT:
                        g_value_set_object (value, vte_terminal_get_pty_object(terminal));
                        break;
                case PROP_SCROLL_BACKGROUND:
                        g_value_set_boolean (value, pvt->scroll_background);
                        break;
                case PROP_SCROLLBACK_LINES:
                        g_value_set_uint (value, pvt->scrollback_lines);
                        break;
                case PROP_SCROLL_ON_KEYSTROKE:
                        g_value_set_boolean (value, pvt->scroll_on_keystroke);
                        break;
                case PROP_SCROLL_ON_OUTPUT:
                        g_value_set_boolean (value, pvt->scroll_on_output);
                        break;
                case PROP_WINDOW_TITLE:
                        g_value_set_string (value, vte_terminal_get_window_title (terminal));
                        break;
                case PROP_WORD_CHARS:
                        g_value_set_string (value, NULL /* FIXME */);
                        break;
                case PROP_VISIBLE_BELL:
                        g_value_set_boolean (value, vte_terminal_get_visible_bell (terminal));
                        break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			return;
        }
}

static void
vte_terminal_set_property (GObject *object,
                           guint prop_id,
                           const GValue *value,
                           GParamSpec *pspec)
{
        VteTerminal *terminal = VTE_TERMINAL (object);
        VteTerminalPrivate *pvt = terminal->pvt;

	switch (prop_id)
	{
#if GTK_CHECK_VERSION (2, 91, 2)
                case PROP_HADJUSTMENT:
                        vte_terminal_set_hadjustment (terminal, g_value_get_object (value));
                        break;
                case PROP_VADJUSTMENT:
                        vte_terminal_set_vadjustment (terminal, g_value_get_object (value));
                        break;
                case PROP_HSCROLL_POLICY:
                        pvt->hscroll_policy = g_value_get_enum (value);
                        gtk_widget_queue_resize_no_redraw (GTK_WIDGET (terminal));
                        break;
                case PROP_VSCROLL_POLICY:
                        pvt->vscroll_policy = g_value_get_enum (value);
                        gtk_widget_queue_resize_no_redraw (GTK_WIDGET (terminal));
                        break;
#endif
                case PROP_ALLOW_BOLD:
                        vte_terminal_set_allow_bold (terminal, g_value_get_boolean (value));
                        break;
                case PROP_AUDIBLE_BELL:
                        vte_terminal_set_audible_bell (terminal, g_value_get_boolean (value));
                        break;
                case PROP_BACKGROUND_IMAGE_FILE:
                        vte_terminal_set_background_image_file (terminal, g_value_get_string (value));
                        break;
                case PROP_BACKGROUND_IMAGE_PIXBUF:
                        vte_terminal_set_background_image (terminal, g_value_get_object (value));
                        break;
                case PROP_BACKGROUND_OPACITY:
                        vte_terminal_set_opacity (terminal, g_value_get_double (value) * (double) G_MAXUINT16);
                        break;
                case PROP_BACKGROUND_SATURATION:
                        vte_terminal_set_background_saturation (terminal, g_value_get_double (value));
                        break;
                case PROP_BACKGROUND_TINT_COLOR:
                        vte_terminal_set_background_tint_color (terminal, g_value_get_boxed (value));
                        break;
                case PROP_BACKGROUND_TRANSPARENT:
                        vte_terminal_set_background_transparent (terminal, g_value_get_boolean (value));
                        break;
                case PROP_BACKSPACE_BINDING:
                        vte_terminal_set_backspace_binding (terminal, g_value_get_enum (value));
                        break;
                case PROP_CURSOR_BLINK_MODE:
                        vte_terminal_set_cursor_blink_mode (terminal, g_value_get_enum (value));
                        break;
                case PROP_CURSOR_SHAPE:
                        vte_terminal_set_cursor_shape (terminal, g_value_get_enum (value));
                        break;
                case PROP_DELETE_BINDING:
                        vte_terminal_set_delete_binding (terminal, g_value_get_enum (value));
                        break;
                case PROP_EMULATION:
                        vte_terminal_set_emulation (terminal, g_value_get_string (value));
                        break;
                case PROP_ENCODING:
                        vte_terminal_set_encoding (terminal, g_value_get_string (value));
                        break;
                case PROP_FONT_DESC:
                        vte_terminal_set_font_full_internal (terminal, g_value_get_boxed (value), pvt->fontantialias);
                        break;
                case PROP_MOUSE_POINTER_AUTOHIDE:
                        vte_terminal_set_mouse_autohide (terminal, g_value_get_boolean (value));
                        break;
                case PROP_PTY:
                        vte_terminal_set_pty (terminal, g_value_get_int (value));
                        break;
                case PROP_PTY_OBJECT:
                        vte_terminal_set_pty_object (terminal, g_value_get_object (value));
                        break;
                case PROP_SCROLL_BACKGROUND:
                        vte_terminal_set_scroll_background (terminal, g_value_get_boolean (value));
                        break;
                case PROP_SCROLLBACK_LINES:
                        vte_terminal_set_scrollback_lines (terminal, g_value_get_uint (value));
                        break;
                case PROP_SCROLL_ON_KEYSTROKE:
                        vte_terminal_set_scroll_on_keystroke(terminal, g_value_get_boolean (value));
                        break;
                case PROP_SCROLL_ON_OUTPUT:
                        vte_terminal_set_scroll_on_output (terminal, g_value_get_boolean (value));
                        break;
                case PROP_WORD_CHARS:
                        vte_terminal_set_word_chars (terminal, g_value_get_string (value));
                        break;
                case PROP_VISIBLE_BELL:
                        vte_terminal_set_visible_bell (terminal, g_value_get_boolean (value));
                        break;

                /* Not writable */
                case PROP_ICON_TITLE:
                case PROP_WINDOW_TITLE:
                        g_assert_not_reached ();
                        break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			return;
	}
}

/* Initialize methods. */
static void
vte_terminal_class_init(VteTerminalClass *klass)
{
	GObjectClass *gobject_class;
	GtkWidgetClass *widget_class;
	GtkBindingSet  *binding_set;

#ifdef VTE_DEBUG
	{
                _vte_debug_init();
		_vte_debug_print(VTE_DEBUG_LIFECYCLE,
				"vte_terminal_class_init()\n");
		/* print out the legend */
		_vte_debug_print(VTE_DEBUG_WORK,
			"Debugging work flow (top input to bottom output):\n"
					"  .  _vte_terminal_process_incoming\n"
					"  <  start process_timeout\n"
					"  {[ start update_timeout  [ => rate limited\n"
					"  T  start of terminal in update_timeout\n"
					"  (  start _vte_terminal_process_incoming\n"
					"  ?  _vte_invalidate_cells (call)\n"
					"  !  _vte_invalidate_cells (dirty)\n"
					"  *  _vte_invalidate_all\n"
					"  )  end _vte_terminal_process_incoming\n"
					"  -  gdk_window_process_updates\n"
					"  +  vte_terminal_expose\n"
					"  =  vte_terminal_paint\n"
					"  ]} end update_timeout\n"
					"  >  end process_timeout\n");
	}
#endif

	bindtextdomain(GETTEXT_PACKAGE, LOCALEDIR);
#ifdef HAVE_DECL_BIND_TEXTDOMAIN_CODESET
	bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
#endif

	g_type_class_add_private(klass, sizeof (VteTerminalPrivate));

	gobject_class = G_OBJECT_CLASS(klass);
	widget_class = GTK_WIDGET_CLASS(klass);

	/* Override some of the default handlers. */
	gobject_class->finalize = vte_terminal_finalize;
        gobject_class->get_property = vte_terminal_get_property;
        gobject_class->set_property = vte_terminal_set_property;
	widget_class->realize = vte_terminal_realize;
	widget_class->scroll_event = vte_terminal_scroll;
#if GTK_CHECK_VERSION (2, 90, 8)
        widget_class->draw = vte_terminal_draw;
#else
	widget_class->expose_event = vte_terminal_expose;
#endif
	widget_class->key_press_event = vte_terminal_key_press;
	widget_class->key_release_event = vte_terminal_key_release;
	widget_class->button_press_event = vte_terminal_button_press;
	widget_class->button_release_event = vte_terminal_button_release;
	widget_class->motion_notify_event = vte_terminal_motion_notify;
	widget_class->enter_notify_event = vte_terminal_enter;
	widget_class->leave_notify_event = vte_terminal_leave;
	widget_class->focus_in_event = vte_terminal_focus_in;
	widget_class->focus_out_event = vte_terminal_focus_out;
	widget_class->visibility_notify_event = vte_terminal_visibility_notify;
	widget_class->unrealize = vte_terminal_unrealize;
	widget_class->style_set = vte_terminal_style_set;
#if GTK_CHECK_VERSION (2, 91, 0)
	widget_class->get_preferred_width = vte_terminal_get_preferred_width;
	widget_class->get_preferred_height = vte_terminal_get_preferred_height;
#else
	widget_class->size_request = vte_terminal_size_request;
#endif
	widget_class->size_allocate = vte_terminal_size_allocate;
	widget_class->get_accessible = vte_terminal_get_accessible;
        widget_class->screen_changed = vte_terminal_screen_changed;

	/* Initialize default handlers. */
	klass->eof = NULL;
	klass->child_exited = NULL;
	klass->emulation_changed = NULL;
	klass->encoding_changed = NULL;
	klass->char_size_changed = NULL;
	klass->window_title_changed = NULL;
	klass->icon_title_changed = NULL;
	klass->selection_changed = NULL;
	klass->contents_changed = NULL;
	klass->cursor_moved = NULL;
	klass->status_line_changed = NULL;
	klass->commit = NULL;

	klass->deiconify_window = NULL;
	klass->iconify_window = NULL;
	klass->raise_window = NULL;
	klass->lower_window = NULL;
	klass->refresh_window = NULL;
	klass->restore_window = NULL;
	klass->maximize_window = NULL;
	klass->resize_window = NULL;
	klass->move_window = NULL;

	klass->increase_font_size = NULL;
	klass->decrease_font_size = NULL;

	klass->text_modified = NULL;
	klass->text_inserted = NULL;
	klass->text_deleted = NULL;
	klass->text_scrolled = NULL;

	klass->copy_clipboard = vte_terminal_real_copy_clipboard;
	klass->paste_clipboard = vte_terminal_real_paste_clipboard;

        klass->beep = NULL;

#if GTK_CHECK_VERSION (2, 91, 2)
        /* GtkScrollable interface properties */
        g_object_class_override_property (gobject_class, PROP_HADJUSTMENT, "hadjustment");
        g_object_class_override_property (gobject_class, PROP_VADJUSTMENT, "vadjustment");
        g_object_class_override_property (gobject_class, PROP_HSCROLL_POLICY, "hscroll-policy");
        g_object_class_override_property (gobject_class, PROP_VSCROLL_POLICY, "vscroll-policy");

#else

        klass->set_scroll_adjustments = vte_terminal_set_scroll_adjustments;

        /**
         * VteTerminal::set-scroll-adjustments:
         * @vteterminal: the object which received the signal
         * @horizontal: (allow-none): the horizontal #GtkAdjustment (unused in #VteTerminal)
         * @vertical: (allow-none): the vertical #GtkAdjustment
         *
         * Set the scroll adjustments for the terminal. Usually scrolled containers
         * like #GtkScrolledWindow will emit this signal to connect two instances
         * of #GtkScrollbar to the scroll directions of the #VteTerminal.
         *
         * Since: 0.17.1
         */
	widget_class->set_scroll_adjustments_signal =
		g_signal_new(I_("set-scroll-adjustments"),
			     G_TYPE_FROM_CLASS (klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET (VteTerminalClass, set_scroll_adjustments),
			     NULL, NULL,
			     _vte_marshal_VOID__OBJECT_OBJECT,
			     G_TYPE_NONE, 2,
			     GTK_TYPE_ADJUSTMENT, GTK_TYPE_ADJUSTMENT);

#endif

	/* Register some signals of our own. */

#if GTK_CHECK_VERSION (2, 99, 0)
#define OBSOLETE_SIGNAL(str)
#else
#define OBSOLETE_SIGNAL(str) str
#endif

        /**
         * VteTerminal::eof:
         * @vteterminal: the object which received the signal
         *
         * Emitted when the terminal receives an end-of-file from a child which
         * is running in the terminal.  This signal is frequently (but not
         * always) emitted with a #VteTerminal::child-exited signal.
         */
        OBSOLETE_SIGNAL (klass->eof_signal =)
                g_signal_new(I_("eof"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, eof),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::child-exited:
         * @vteterminal: the object which received the signal
         *
         * This signal is emitted when the terminal detects that a child started
         * using vte_terminal_fork_command() has exited.
         */
        OBSOLETE_SIGNAL (klass->child_exited_signal =)
                g_signal_new(I_("child-exited"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, child_exited),
			     NULL,
			     NULL,
			     g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::window-title-changed:
         * @vteterminal: the object which received the signal
         *
         * Emitted when the terminal's %window_title field is modified.
         */
        OBSOLETE_SIGNAL (klass->window_title_changed_signal =)
                g_signal_new(I_("window-title-changed"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, window_title_changed),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::icon-title-changed:
         * @vteterminal: the object which received the signal
         *
         * Emitted when the terminal's %icon_title field is modified.
         */
        OBSOLETE_SIGNAL (klass->icon_title_changed_signal =)
                g_signal_new(I_("icon-title-changed"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, icon_title_changed),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::encoding-changed:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever the terminal's current encoding has changed, either
         * as a result of receiving a control sequence which toggled between the
         * local and UTF-8 encodings, or at the parent application's request.
         */
        OBSOLETE_SIGNAL (klass->encoding_changed_signal =)
                g_signal_new(I_("encoding-changed"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, encoding_changed),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::commit:
         * @vteterminal: the object which received the signal
         * @text: a string of text
         * @size: the length of that string of text
         *
         * Emitted whenever the terminal receives input from the user and
         * prepares to send it to the child process.  The signal is emitted even
         * when there is no child process.
         */
        OBSOLETE_SIGNAL (klass->commit_signal =)
                g_signal_new(I_("commit"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, commit),
			     NULL,
			     NULL,
			     _vte_marshal_VOID__STRING_UINT,
			     G_TYPE_NONE, 2, G_TYPE_STRING, G_TYPE_UINT);

        /**
         * VteTerminal::emulation-changed:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever the terminal's emulation changes, only possible at
         * the parent application's request.
         */
        OBSOLETE_SIGNAL (klass->emulation_changed_signal =)
                g_signal_new(I_("emulation-changed"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, emulation_changed),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::char-size-changed:
         * @vteterminal: the object which received the signal
         * @width: the new character cell width
         * @height: the new character cell height
         *
         * Emitted whenever selection of a new font causes the values of the
         * %char_width or %char_height fields to change.
         */
        OBSOLETE_SIGNAL (klass->char_size_changed_signal =)
                g_signal_new(I_("char-size-changed"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, char_size_changed),
			     NULL,
			     NULL,
			     _vte_marshal_VOID__UINT_UINT,
			     G_TYPE_NONE, 2, G_TYPE_UINT, G_TYPE_UINT);

        /**
         * VteTerminal::selection-changed:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever the contents of terminal's selection changes.
         */
        OBSOLETE_SIGNAL (klass->selection_changed_signal =)
                g_signal_new (I_("selection-changed"),
			      G_OBJECT_CLASS_TYPE(klass),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET(VteTerminalClass, selection_changed),
			      NULL,
			      NULL,
                              g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);

        /**
         * VteTerminal::contents-changed:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever the visible appearance of the terminal has changed.
         * Used primarily by #VteTerminalAccessible.
         */
        OBSOLETE_SIGNAL (klass->contents_changed_signal =)
                g_signal_new(I_("contents-changed"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, contents_changed),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::cursor-moved:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever the cursor moves to a new character cell.  Used
         * primarily by #VteTerminalAccessible.
         */
        OBSOLETE_SIGNAL (klass->cursor_moved_signal =)
                g_signal_new(I_("cursor-moved"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, cursor_moved),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::deiconify-window:
         * @vteterminal: the object which received the signal
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->deiconify_window_signal =)
                g_signal_new(I_("deiconify-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, deiconify_window),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::iconify-window:
         * @vteterminal: the object which received the signal
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->iconify_window_signal =)
                g_signal_new(I_("iconify-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, iconify_window),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::raise-window:
         * @vteterminal: the object which received the signal
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->raise_window_signal =)
                g_signal_new(I_("raise-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, raise_window),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::lower-window:
         * @vteterminal: the object which received the signal
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->lower_window_signal =)
                g_signal_new(I_("lower-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, lower_window),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::refresh-window:
         * @vteterminal: the object which received the signal
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->refresh_window_signal =)
                g_signal_new(I_("refresh-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, refresh_window),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::restore-window:
         * @vteterminal: the object which received the signal
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->restore_window_signal =)
                g_signal_new(I_("restore-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, restore_window),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::maximize-window:
         * @vteterminal: the object which received the signal
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->maximize_window_signal =)
                g_signal_new(I_("maximize-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, maximize_window),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::resize-window:
         * @vteterminal: the object which received the signal
         * @width: the desired width in pixels, including padding
         * @height: the desired height in pixels, including padding
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->resize_window_signal =)
                g_signal_new(I_("resize-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, resize_window),
			     NULL,
			     NULL,
			     _vte_marshal_VOID__UINT_UINT,
			     G_TYPE_NONE, 2, G_TYPE_UINT, G_TYPE_UINT);

        /**
         * VteTerminal::move-window:
         * @vteterminal: the object which received the signal
         * @x: the terminal's desired location, X coordinate
         * @y: the terminal's desired location, Y coordinate
         *
         * Emitted at the child application's request.
         */
        OBSOLETE_SIGNAL (klass->move_window_signal =)
                g_signal_new(I_("move-window"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, move_window),
			     NULL,
			     NULL,
			     _vte_marshal_VOID__UINT_UINT,
			     G_TYPE_NONE, 2, G_TYPE_UINT, G_TYPE_UINT);

        /**
         * VteTerminal::status-line-changed:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever the contents of the status line are modified or
         * cleared.
         */
        OBSOLETE_SIGNAL (klass->status_line_changed_signal =)
                g_signal_new(I_("status-line-changed"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, status_line_changed),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::increase-font-size:
         * @vteterminal: the object which received the signal
         *
         * Emitted when the user hits the '+' key while holding the Control key.
         */
        OBSOLETE_SIGNAL (klass->increase_font_size_signal =)
                g_signal_new(I_("increase-font-size"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, increase_font_size),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::decrease-font-size:
         * @vteterminal: the object which received the signal
         *
         * Emitted when the user hits the '-' key while holding the Control key.
         */
        OBSOLETE_SIGNAL (klass->decrease_font_size_signal =)
                g_signal_new(I_("decrease-font-size"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, decrease_font_size),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::text-modified:
         * @vteterminal: the object which received the signal
         *
         * An internal signal used for communication between the terminal and
         * its accessibility peer. May not be emitted under certain
         * circumstances.
         */
        OBSOLETE_SIGNAL (klass->text_modified_signal =)
                g_signal_new(I_("text-modified"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, text_modified),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::text-inserted:
         * @vteterminal: the object which received the signal
         *
         * An internal signal used for communication between the terminal and
         * its accessibility peer. May not be emitted under certain
         * circumstances.
         */
        OBSOLETE_SIGNAL (klass->text_inserted_signal =)
                g_signal_new(I_("text-inserted"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, text_inserted),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::text-deleted:
         * @vteterminal: the object which received the signal
         *
         * An internal signal used for communication between the terminal and
         * its accessibility peer. May not be emitted under certain
         * circumstances.
         */
        OBSOLETE_SIGNAL (klass->text_deleted_signal =)
                g_signal_new(I_("text-deleted"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, text_deleted),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::text-scrolled:
         * @vteterminal: the object which received the signal
         * @delta: the number of lines scrolled
         *
         * An internal signal used for communication between the terminal and
         * its accessibility peer. May not be emitted under certain
         * circumstances.
         */
       OBSOLETE_SIGNAL (klass->text_scrolled_signal =)
                g_signal_new(I_("text-scrolled"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, text_scrolled),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__INT,
			     G_TYPE_NONE, 1, G_TYPE_INT);

#undef OBSOLETE_SIGNAL

        /**
         * VteTerminal::copy-clipboard:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever vte_terminal_copy_clipboard() is called.
         */
	signals[COPY_CLIPBOARD] =
                g_signal_new(I_("copy-clipboard"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			     G_STRUCT_OFFSET(VteTerminalClass, copy_clipboard),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::paste-clipboard:
         * @vteterminal: the object which received the signal
         *
         * Emitted whenever vte_terminal_paste_clipboard() is called.
         */
	signals[PASTE_CLIPBOARD] =
                g_signal_new(I_("paste-clipboard"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			     G_STRUCT_OFFSET(VteTerminalClass, paste_clipboard),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal::beep:
         * @vteterminal: the object which received the signal
         *
         * This signal is emitted when the a child sends a beep request to the
         * terminal.
         */
        g_signal_new(I_("beep"),
			     G_OBJECT_CLASS_TYPE(klass),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(VteTerminalClass, beep),
			     NULL,
			     NULL,
                             g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        /**
         * VteTerminal:allow-bold:
         *
         * Controls whether or not the terminal will attempt to draw bold text.
         * This may happen either by using a bold font variant, or by
         * repainting text with a different offset.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_ALLOW_BOLD,
                 g_param_spec_boolean ("allow-bold", NULL, NULL,
                                       TRUE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:audible-bell:
         *
         * Controls whether or not the terminal will beep when the child outputs the
         * "bl" sequence.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_AUDIBLE_BELL,
                 g_param_spec_boolean ("audible-bell", NULL, NULL,
                                       TRUE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:background-image-file: (type filename):
         *
         * Sets a background image file for the widget.  If specified by
         * #VteTerminal:background-saturation:, the terminal will tint its
         * in-memory copy of the image before applying it to the terminal.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_BACKGROUND_IMAGE_FILE,
                 g_param_spec_string ("background-image-file", NULL, NULL,
                                      NULL,
                                      G_PARAM_READWRITE | STATIC_PARAMS));

        /**
         * VteTerminal:background-image-pixbuf:
         *
         * Sets a background image for the widget.  Text which would otherwise be
         * drawn using the default background color will instead be drawn over the
         * specified image.  If necessary, the image will be tiled to cover the
         * widget's entire visible area. If specified by
         * #VteTerminal:background-saturation:, the terminal will tint its
         * in-memory copy of the image before applying it to the terminal.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_BACKGROUND_IMAGE_PIXBUF,
                 g_param_spec_object ("background-image-pixbuf", NULL, NULL,
                                      GDK_TYPE_PIXBUF,
                                      G_PARAM_READWRITE | STATIC_PARAMS));

        /**
         * VteTerminal:background-opacity:
         *
         * Sets the opacity of the terminal background, were 0.0 means completely
         * transparent and 1.0 means completely opaque.
         *
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_BACKGROUND_OPACITY,
                 g_param_spec_double ("background-opacity", NULL, NULL,
                                      0.0, 1.0,
                                      1.0,
                                      G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:background-saturation:
         *
         * If a background image has been set using #VteTerminal:background-image-file: or
         * #VteTerminal:background-image-pixbuf:, or #VteTerminal:background-transparent:,
         * and the saturation value is less
         * than 1.0, the terminal will adjust the colors of the image before drawing
         * the image.  To do so, the terminal will create a copy of the background
         * image (or snapshot of the root window) and modify its pixel values.
         *
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_BACKGROUND_SATURATION,
                 g_param_spec_double ("background-saturation", NULL, NULL,
                                      0.0, 1.0,
                                      0.4,
                                      G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:background-tint-color:
         *
         * If a background image has been set using #VteTerminal:background-image-file: or
         * #VteTerminal:background-image-pixbuf:, or #VteTerminal:background-transparent:, and
         * and the value set by VteTerminal:background-saturation: is less than 1.0,
         * the terminal
         * will adjust the color of the image before drawing the image.  To do so,
         * the terminal will create a copy of the background image (or snapshot of
         * the root window) and modify its pixel values.  The initial tint color
         * is black.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_BACKGROUND_TINT_COLOR,
                 g_param_spec_boxed ("background-tint-color", NULL, NULL,
                                     GDK_TYPE_COLOR,
                                     G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:background-transparent:
         *
         * Sets whther the terminal uses the pixmap stored in the root
         * window as the background, adjusted so that if there are no windows
         * below your application, the widget will appear to be transparent.
         *
         * Note: When using a compositing window manager, you should instead
         * set a RGBA colourmap on the toplevel window, so you get real transparency.
         *
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_BACKGROUND_TRANSPARENT,
                 g_param_spec_boolean ("background-transparent", NULL, NULL,
                                       FALSE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:backspace-binding:
         *
         * *Controls what string or control sequence the terminal sends to its child
         * when the user presses the backspace key.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_BACKSPACE_BINDING,
                 g_param_spec_enum ("backspace-binding", NULL, NULL,
                                    VTE_TYPE_TERMINAL_ERASE_BINDING,
                                    VTE_ERASE_AUTO,
                                    G_PARAM_READWRITE | STATIC_PARAMS));

        /**
         * VteTerminal:cursor-blink-mode:
         *
         * Sets whether or not the cursor will blink. Using %VTE_CURSOR_BLINK_SYSTEM
         * will use the #GtkSettings::gtk-cursor-blink setting.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_CURSOR_BLINK_MODE,
                 g_param_spec_enum ("cursor-blink-mode", NULL, NULL,
                                    VTE_TYPE_TERMINAL_CURSOR_BLINK_MODE,
                                    VTE_CURSOR_BLINK_SYSTEM,
                                    G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:cursor-shape:
         *
         * Controls the shape of the cursor.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_CURSOR_SHAPE,
                 g_param_spec_enum ("cursor-shape", NULL, NULL,
                                    VTE_TYPE_TERMINAL_CURSOR_SHAPE,
                                    VTE_CURSOR_SHAPE_BLOCK,
                                    G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:delete-binding:
         *
         * Controls what string or control sequence the terminal sends to its child
         * when the user presses the delete key.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_DELETE_BINDING,
                 g_param_spec_enum ("delete-binding", NULL, NULL,
                                    VTE_TYPE_TERMINAL_ERASE_BINDING,
                                    VTE_ERASE_AUTO,
                                    G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:emulation:
         *
         * Sets what type of terminal the widget attempts to emulate by scanning for
         * control sequences defined in the system's termcap file.  Unless you
         * are interested in this feature, always use the default which is "xterm".
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_EMULATION,
                 g_param_spec_string ("emulation", NULL, NULL,
                                      VTE_DEFAULT_EMULATION,
                                      G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:encoding:
         *
         * Controls the encoding the terminal will expect data from the child to
         * be encoded with.  For certain terminal types, applications executing in the
         * terminal can change the encoding.  The default is defined by the
         * application's locale settings.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_ENCODING,
                 g_param_spec_string ("encoding", NULL, NULL,
                                      NULL,
                                      G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:font-desc:
         *
         * Specifies the font used for rendering all text displayed by the terminal,
         * overriding any fonts set using gtk_widget_modify_font().  The terminal
         * will immediately attempt to load the desired font, retrieve its
         * metrics, and attempt to resize itself to keep the same number of rows
         * and columns.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_FONT_DESC,
                 g_param_spec_boxed ("font-desc", NULL, NULL,
                                     PANGO_TYPE_FONT_DESCRIPTION,
                                     G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:icon-title:
         *
         * The terminal's so-called icon title, or %NULL if no icon title has been set.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_ICON_TITLE,
                 g_param_spec_string ("icon-title", NULL, NULL,
                                      NULL,
                                      G_PARAM_READABLE | STATIC_PARAMS));
     
        /**
         * VteTerminal:pointer-autohide:
         *
         * Controls the value of the terminal's mouse autohide setting.  When autohiding
         * is enabled, the mouse cursor will be hidden when the user presses a key and
         * shown when the user moves the mouse.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_MOUSE_POINTER_AUTOHIDE,
                 g_param_spec_boolean ("pointer-autohide", NULL, NULL,
                                       FALSE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:pty:
         *
         * The file descriptor of the master end of the terminal's PTY.
         * 
         * Since: 0.20
         *
         * Deprecated: 0.26: Use the #VteTerminal:pty-object property instead
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_PTY,
                 g_param_spec_int ("pty", NULL, NULL,
                                   -1, G_MAXINT,
                                   -1,
                                   G_PARAM_READWRITE | STATIC_PARAMS));

        /**
         * VteTerminal:pty-object:
         *
         * The PTY object for the terminal.
         *
         * Since: 0.26
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_PTY_OBJECT,
                 g_param_spec_object ("pty-object", NULL, NULL,
                                      VTE_TYPE_PTY,
                                      G_PARAM_READWRITE |
                                      G_PARAM_STATIC_STRINGS));

        /**
         * VteTerminal:scroll-background:
         *
         * Controls whether or not the terminal will scroll the background image (if
         * one is set) when the text in the window must be scrolled.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_SCROLL_BACKGROUND,
                 g_param_spec_boolean ("scroll-background", NULL, NULL,
                                       FALSE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:scrollback-lines:
         *
         * The length of the scrollback buffer used by the terminal.  The size of
         * the scrollback buffer will be set to the larger of this value and the number
         * of visible rows the widget can display, so 0 can safely be used to disable
         * scrollback.  Note that this setting only affects the normal screen buffer.
         * For terminal types which have an alternate screen buffer, no scrollback is
         * allowed on the alternate screen buffer.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_SCROLLBACK_LINES,
                 g_param_spec_uint ("scrollback-lines", NULL, NULL,
                                    0, G_MAXUINT,
                                    VTE_SCROLLBACK_INIT,
                                    G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:scroll-on-keystroke:
         *
         * Controls whether or not the terminal will forcibly scroll to the bottom of
         * the viewable history when the user presses a key.  Modifier keys do not
         * trigger this behavior.
         *
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_SCROLL_ON_KEYSTROKE,
                 g_param_spec_boolean ("scroll-on-keystroke", NULL, NULL,
                                       FALSE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:scroll-on-output:
         *
         * Controls whether or not the terminal will forcibly scroll to the bottom of
         * the viewable history when the new data is received from the child.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_SCROLL_ON_OUTPUT,
                 g_param_spec_boolean ("scroll-on-output", NULL, NULL,
                                       TRUE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:window-title:
         *
         * The terminal's title.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_WINDOW_TITLE,
                 g_param_spec_string ("window-title", NULL, NULL,
                                      NULL,
                                      G_PARAM_READABLE | STATIC_PARAMS));
     
        /**
         * VteTerminal:word-chars:
         *
         * When the user double-clicks to start selection, the terminal will extend
         * the selection on word boundaries.  It will treat characters the word-chars
         * characters as parts of words, and all other characters as word separators.
         * Ranges of characters can be specified by separating them with a hyphen.
         *
         * As a special case, when setting this to %NULL or the empty string, the terminal will
         * treat all graphic non-punctuation non-space characters as word characters.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_WORD_CHARS,
                 g_param_spec_string ("word-chars", NULL, NULL,
                                      NULL,
                                      G_PARAM_READWRITE | STATIC_PARAMS));
     
        /**
         * VteTerminal:visible-bell:
         *
         * Controls whether the terminal will present a visible bell to the
         * user when the child outputs the "bl" sequence.  The terminal
         * will clear itself to the default foreground color and then repaint itself.
         * 
         * Since: 0.20
         */
        g_object_class_install_property
                (gobject_class,
                 PROP_VISIBLE_BELL,
                 g_param_spec_boolean ("visible-bell", NULL, NULL,
                                       FALSE,
                                       G_PARAM_READWRITE | STATIC_PARAMS));

        /* Style properties */

        /**
         * VteTerminal:inner-border:
         *
         * Sets the border around the terminal.
         *
         * Since: 0.24
         */
        gtk_widget_class_install_style_property
                (widget_class,
                 g_param_spec_boxed ("inner-border", NULL, NULL,
                                     GTK_TYPE_BORDER,
                                     G_PARAM_READABLE |
                                     G_PARAM_STATIC_STRINGS));

#if !GTK_CHECK_VERSION (2,99, 0)
        /* Now install the default style */
        gtk_rc_parse_string("style \"vte-default-style\" {\n"
                              "VteTerminal::inner-border = { 1, 1, 1, 1 }\n"
                            "}\n"
                            "class \"VteTerminal\" style : gtk \"vte-default-style\"\n");
#endif

        /* Keybindings */
	binding_set = gtk_binding_set_by_class(klass);

	/* Bind Copy, Paste, Cut keys */
	gtk_binding_entry_add_signal(binding_set, GDK_KEY (F16), 0, "copy-clipboard",0);
	gtk_binding_entry_add_signal(binding_set, GDK_KEY (F18), 0, "paste-clipboard", 0);
	gtk_binding_entry_add_signal(binding_set, GDK_KEY (F20), 0, "copy-clipboard",0);

	process_timer = g_timer_new ();

#if GTK_CHECK_VERSION (2, 99, 0)
        klass->priv = G_TYPE_CLASS_GET_PRIVATE (klass, VTE_TYPE_TERMINAL, VteTerminalClassPrivate);

        klass->priv->style_provider = GTK_STYLE_PROVIDER (gtk_css_provider_new ());
        gtk_css_provider_load_from_data (GTK_CSS_PROVIDER (klass->priv->style_provider),
                                         "VteTerminal {\n"
                                           "-VteTerminal-inner-border: 1;\n"
                                         "}\n",
                                         -1, NULL);
#endif /* GTK 3.0 */
}

/**
 * vte_terminal_set_audible_bell:
 * @terminal: a #VteTerminal
 * @is_audible: %TRUE if the terminal should beep
 *
 * Controls whether or not the terminal will beep when the child outputs the
 * "bl" sequence.
 */
void
vte_terminal_set_audible_bell(VteTerminal *terminal, gboolean is_audible)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        is_audible = is_audible != FALSE;
        if (is_audible == pvt->audible_bell)
                return;

	pvt->audible_bell = is_audible;

        g_object_notify (G_OBJECT (terminal), "audible-bell");
}

/**
 * vte_terminal_get_audible_bell:
 * @terminal: a #VteTerminal
 *
 * Checks whether or not the terminal will beep when the child outputs the
 * "bl" sequence.
 *
 * Returns: %TRUE if audible bell is enabled, %FALSE if not
 */
gboolean
vte_terminal_get_audible_bell(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);
	return terminal->pvt->audible_bell;
}

/**
 * vte_terminal_set_visible_bell:
 * @terminal: a #VteTerminal
 * @is_visible: whether the terminal should flash on bell
 *
 * Controls whether or not the terminal will present a visible bell to the
 * user when the child outputs the "bl" sequence.  The terminal
 * will clear itself to the default foreground color and then repaint itself.
 *
 */
void
vte_terminal_set_visible_bell(VteTerminal *terminal, gboolean is_visible)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        is_visible = is_visible != FALSE;
        if (is_visible == pvt->visible_bell)
                return;

	pvt->visible_bell = is_visible;

        g_object_notify (G_OBJECT (terminal), "visible-bell");
}

/**
 * vte_terminal_get_visible_bell:
 * @terminal: a #VteTerminal
 *
 * Checks whether or not the terminal will present a visible bell to the
 * user when the child outputs the "bl" sequence.  The terminal
 * will clear itself to the default foreground color and then repaint itself.
 *
 * Returns: %TRUE if visible bell is enabled, %FALSE if not
 */
gboolean
vte_terminal_get_visible_bell(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);
	return terminal->pvt->visible_bell;
}

/**
 * vte_terminal_set_allow_bold:
 * @terminal: a #VteTerminal
 * @allow_bold: %TRUE if the terminal should attempt to draw bold text
 *
 * Controls whether or not the terminal will attempt to draw bold text,
 * either by using a bold font variant or by repainting text with a different
 * offset.
 *
 */
void
vte_terminal_set_allow_bold(VteTerminal *terminal, gboolean allow_bold)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        allow_bold = allow_bold != FALSE;
        if (allow_bold == pvt->allow_bold)
                return;

	pvt->allow_bold = allow_bold;
        g_object_notify (G_OBJECT (terminal), "allow-bold");

	_vte_invalidate_all (terminal);
}

/**
 * vte_terminal_get_allow_bold:
 * @terminal: a #VteTerminal
 *
 * Checks whether or not the terminal will attempt to draw bold text by
 * repainting text with a one-pixel offset.
 *
 * Returns: %TRUE if bolding is enabled, %FALSE if not
 */
gboolean
vte_terminal_get_allow_bold(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);
	return terminal->pvt->allow_bold;
}

/**
 * vte_terminal_set_scroll_background:
 * @terminal: a #VteTerminal
 * @scroll: whether the terminal should scroll the background image along with
 *   the text
 *
 * Controls whether or not the terminal will scroll the background image (if
 * one is set) when the text in the window must be scrolled.
 *
 * Since: 0.11
 */
void
vte_terminal_set_scroll_background(VteTerminal *terminal, gboolean scroll)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        scroll = scroll != FALSE;
        if (scroll == pvt->scroll_background)
                return;

	pvt->scroll_background = scroll;

        g_object_notify (G_OBJECT (terminal), "scroll-background");

        vte_terminal_queue_background_update(terminal);
}

/**
 * vte_terminal_set_scroll_on_output:
 * @terminal: a #VteTerminal
 * @scroll: whether the terminal should scroll on output
 *
 * Controls whether or not the terminal will forcibly scroll to the bottom of
 * the viewable history when the new data is received from the child.
 */
void
vte_terminal_set_scroll_on_output(VteTerminal *terminal, gboolean scroll)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	terminal->pvt->scroll_on_output = scroll;
}

/**
 * vte_terminal_set_scroll_on_keystroke:
 * @terminal: a #VteTerminal
 * @scroll: whether the terminal should scroll on keystrokes
 *
 * Controls whether or not the terminal will forcibly scroll to the bottom of
 * the viewable history when the user presses a key.  Modifier keys do not
 * trigger this behavior.
 */
void
vte_terminal_set_scroll_on_keystroke(VteTerminal *terminal, gboolean scroll)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        scroll = scroll != FALSE;
        if (scroll == pvt->scroll_on_keystroke)
                return;

	pvt->scroll_on_keystroke = scroll;

        g_object_notify (G_OBJECT (terminal), "scroll-on-keystroke");
}

static void
vte_terminal_real_copy_clipboard(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SELECTION, "Copying to CLIPBOARD.\n");
	if (terminal->pvt->selection != NULL) {
		GtkClipboard *clipboard;
		clipboard = vte_terminal_clipboard_get(terminal,
						       GDK_SELECTION_CLIPBOARD);
		gtk_clipboard_set_text(clipboard, terminal->pvt->selection, -1);
	}
}

/**
 * vte_terminal_copy_clipboard:
 * @terminal: a #VteTerminal
 *
 * Places the selected text in the terminal in the #GDK_SELECTION_CLIPBOARD
 * selection.
 */
void
vte_terminal_copy_clipboard(VteTerminal *terminal)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_signal_emit (terminal, signals[COPY_CLIPBOARD], 0);
}

static void
vte_terminal_real_paste_clipboard(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SELECTION, "Pasting CLIPBOARD.\n");
	vte_terminal_paste(terminal, GDK_SELECTION_CLIPBOARD);
}

/**
 * vte_terminal_paste_clipboard:
 * @terminal: a #VteTerminal
 *
 * Sends the contents of the #GDK_SELECTION_CLIPBOARD selection to the
 * terminal's child.  If necessary, the data is converted from UTF-8 to the
 * terminal's current encoding. It's called on paste menu item, or when
 * user presses Shift+Insert.
 */
void
vte_terminal_paste_clipboard(VteTerminal *terminal)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_signal_emit (terminal, signals[PASTE_CLIPBOARD], 0);
}

/**
 * vte_terminal_copy_primary:
 * @terminal: a #VteTerminal
 *
 * Places the selected text in the terminal in the #GDK_SELECTION_PRIMARY
 * selection.
 */
void
vte_terminal_copy_primary(VteTerminal *terminal)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	_vte_debug_print(VTE_DEBUG_SELECTION, "Copying to PRIMARY.\n");
	vte_terminal_copy(terminal, GDK_SELECTION_PRIMARY);
}

/**
 * vte_terminal_paste_primary:
 * @terminal: a #VteTerminal
 *
 * Sends the contents of the #GDK_SELECTION_PRIMARY selection to the terminal's
 * child.  If necessary, the data is converted from UTF-8 to the terminal's
 * current encoding.  The terminal will call also paste the
 * #GDK_SELECTION_PRIMARY selection when the user clicks with the the second
 * mouse button.
 */
void
vte_terminal_paste_primary(VteTerminal *terminal)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	_vte_debug_print(VTE_DEBUG_SELECTION, "Pasting PRIMARY.\n");
	vte_terminal_paste(terminal, GDK_SELECTION_PRIMARY);
}

/**
 * vte_terminal_im_append_menuitems:
 * @terminal: a #VteTerminal
 * @menushell: a GtkMenuShell
 *
 * Appends menu items for various input methods to the given menu.  The
 * user can select one of these items to modify the input method used by
 * the terminal.
 */
void
vte_terminal_im_append_menuitems(VteTerminal *terminal, GtkMenuShell *menushell)
{
	GtkIMMulticontext *context;
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail (gtk_widget_get_realized (&terminal->widget));
        g_return_if_fail(GTK_IS_MENU_SHELL(menushell));
	context = GTK_IM_MULTICONTEXT(terminal->pvt->im_context);
	gtk_im_multicontext_append_menuitems(context, menushell);
}

/* Set up whatever background we wanted. */
static gboolean
vte_terminal_background_update(VteTerminal *terminal)
{
	double saturation;
	const PangoColor *entry;
	GdkColor color;

	/* If we're not realized yet, don't worry about it, because we get
	 * called when we realize. */
	if (! gtk_widget_get_realized (&terminal->widget)) {
		_vte_debug_print(VTE_DEBUG_MISC,
				"Can not set background image without "
				"window.\n");
		return TRUE;
	}

	_vte_debug_print(VTE_DEBUG_MISC|VTE_DEBUG_EVENTS,
			"Updating background image.\n");

	entry = &terminal->pvt->palette[VTE_DEF_BG];
	_vte_debug_print(VTE_DEBUG_BG,
			 "Setting background color to (%d, %d, %d, %d).\n",
			 entry->red, entry->green, entry->blue,
			 terminal->pvt->bg_opacity);

	/* Set the terminal widget background color since otherwise we
	 * won't draw it for VTE_BG_SOURCE_NONE. */
	color.red = entry->red;
	color.green = entry->green;
	color.blue = entry->blue;
	gtk_widget_modify_bg (&terminal->widget, GTK_STATE_NORMAL, &color);

	_vte_draw_set_background_solid (terminal->pvt->draw, 
					entry->red / 65535.,
					entry->green / 65535.,
					entry->blue / 65535.,
					terminal->pvt->bg_opacity / 65535.);

	/* If we're transparent, and either have no root image or are being
	 * told to update it, get a new copy of the root window. */
	saturation = (double) terminal->pvt->bg_saturation;
	saturation /= VTE_SATURATION_MAX;
	if (terminal->pvt->bg_transparent) {
		if (terminal->pvt->root_pixmap_changed_tag == 0) {
			VteBg *bg;

			/* Connect to background-change events. */
			bg = vte_bg_get_for_screen (gtk_widget_get_screen (&terminal->widget));
			terminal->pvt->root_pixmap_changed_tag =
				g_signal_connect(bg, "root-pixmap-changed",
					G_CALLBACK(root_pixmap_changed_cb),
					terminal);
		}

		_vte_draw_set_background_image(terminal->pvt->draw,
					       VTE_BG_SOURCE_ROOT,
					       NULL,
					       NULL,
					       &terminal->pvt->bg_tint_color,
					       saturation);
	} else
	if (terminal->pvt->bg_file) {
		_vte_draw_set_background_image(terminal->pvt->draw,
					       VTE_BG_SOURCE_FILE,
					       NULL,
					       terminal->pvt->bg_file,
					       &terminal->pvt->bg_tint_color,
					       saturation);
	} else
	if (GDK_IS_PIXBUF(terminal->pvt->bg_pixbuf)) {
		_vte_draw_set_background_image(terminal->pvt->draw,
					       VTE_BG_SOURCE_PIXBUF,
					       terminal->pvt->bg_pixbuf,
					       NULL,
					       &terminal->pvt->bg_tint_color,
					       saturation);
	} else {
		_vte_draw_set_background_image(terminal->pvt->draw,
					       VTE_BG_SOURCE_NONE,
					       NULL,
					       NULL,
					       &terminal->pvt->bg_tint_color,
					       saturation);
	}

	/* Note that the update has finished. */
	terminal->pvt->bg_update_pending = FALSE;

	/* Force a redraw for everything. */
	_vte_invalidate_all (terminal);

	return FALSE;
}

/* Queue an update of the background image, to be done as soon as we can
 * get to it.  Just bail if there's already an update pending, so that if
 * opaque move tables to screw us, we don't end up with an insane backlog
 * of updates after the user finishes moving us. */
static void
vte_terminal_queue_background_update(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_EVENTS,
			"Queued background update.\n");
	terminal->pvt->bg_update_pending = TRUE;
	/* force a redraw when convenient */
	add_update_timeout (terminal);
}

/**
 * vte_terminal_set_background_saturation:
 * @terminal: a #VteTerminal
 * @saturation: a floating point value between 0.0 and 1.0.
 *
 * If a background image has been set using
 * vte_terminal_set_background_image(),
 * vte_terminal_set_background_image_file(), or
 * vte_terminal_set_background_transparent(), and the saturation value is less
 * than 1.0, the terminal will adjust the colors of the image before drawing
 * the image.  To do so, the terminal will create a copy of the background
 * image (or snapshot of the root window) and modify its pixel values.
 */
void
vte_terminal_set_background_saturation(VteTerminal *terminal, double saturation)
{
        VteTerminalPrivate *pvt;
	guint v;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

	v = CLAMP(saturation * VTE_SATURATION_MAX, 0, VTE_SATURATION_MAX);
        if (v == pvt->bg_saturation)
                return;

	_vte_debug_print(VTE_DEBUG_MISC,
			"Setting background saturation to %d/%d.\n",
			v, VTE_SATURATION_MAX);

        pvt->bg_saturation = v;
        g_object_notify(G_OBJECT (terminal), "background-saturation");

        vte_terminal_queue_background_update(terminal);
}

/**
 * vte_terminal_set_background_tint_color:
 * @terminal: a #VteTerminal
 * @color: a color which the terminal background should be tinted to if its
 *   saturation is not 1.0.
 *
 * If a background image has been set using
 * vte_terminal_set_background_image(),
 * vte_terminal_set_background_image_file(), or
 * vte_terminal_set_background_transparent(), and the value set by
 * vte_terminal_set_background_saturation() is less than one, the terminal
 * will adjust the color of the image before drawing the image.  To do so,
 * the terminal will create a copy of the background image (or snapshot of
 * the root window) and modify its pixel values.  The initial tint color
 * is black.
 *
 * Since: 0.11
 */
void
vte_terminal_set_background_tint_color(VteTerminal *terminal,
				       const GdkColor *color)
{
        VteTerminalPrivate *pvt;
	PangoColor *tint;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(color != NULL);

        pvt = terminal->pvt;

	_vte_debug_print(VTE_DEBUG_MISC,
			"Setting background tint to %d,%d,%d.\n",
			terminal->pvt->bg_tint_color.red >> 8,
			terminal->pvt->bg_tint_color.green >> 8,
			terminal->pvt->bg_tint_color.blue >> 8);
        tint = &pvt->bg_tint_color;
	if (color->red == tint->red &&
            color->green == tint->green &&
            color->blue == tint->blue)
                return;

	tint->red = color->red;
	tint->green = color->green;
	tint->blue = color->blue;

        g_object_notify(G_OBJECT (terminal), "background-tint-color");

        vte_terminal_queue_background_update(terminal);
}

/**
 * vte_terminal_set_background_transparent:
 * @terminal: a #VteTerminal
 * @transparent: whether the terminal should fake transparency
 *
 * Sets the terminal's background image to the pixmap stored in the root
 * window, adjusted so that if there are no windows below your application,
 * the widget will appear to be transparent.
 */
void
vte_terminal_set_background_transparent(VteTerminal *terminal,
					gboolean transparent)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        transparent = transparent != FALSE;
        if (transparent == pvt->bg_transparent)
                return;

	_vte_debug_print(VTE_DEBUG_MISC,
		"Turning background transparency %s.\n",
			transparent ? "on" : "off");
		
	pvt->bg_transparent = transparent;
        g_object_notify(G_OBJECT (terminal), "background-transparent");

        /* Update the background. */
        vte_terminal_queue_background_update(terminal);
}

/**
 * vte_terminal_set_background_image:
 * @terminal: a #VteTerminal
 * @image: (allow-none): a #GdkPixbuf to use, or %NULL to unset the background
 *
 * Sets a background image for the widget.  Text which would otherwise be
 * drawn using the default background color will instead be drawn over the
 * specified image.  If necessary, the image will be tiled to cover the
 * widget's entire visible area. If specified by
 * vte_terminal_set_background_saturation(), the terminal will tint its
 * in-memory copy of the image before applying it to the terminal.
 */
void
vte_terminal_set_background_image(VteTerminal *terminal, GdkPixbuf *image)
{
        VteTerminalPrivate *pvt;
        GObject *object;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(image==NULL || GDK_IS_PIXBUF(image));

        object = G_OBJECT(terminal);
        pvt = terminal->pvt;

        if (image && image == pvt->bg_pixbuf)
                return;

	_vte_debug_print(VTE_DEBUG_MISC,
			"%s background image.\n",
			GDK_IS_PIXBUF(image) ? "Setting" : "Clearing");

        g_object_freeze_notify(object);

	/* Get a ref to the new image if there is one.  Do it here just in
	 * case we're actually given the same one we're already using. */
	if (image != NULL) {
		g_object_ref(image);
	}

	/* Unref the previous background image. */
	if (pvt->bg_pixbuf != NULL) {
		g_object_unref(pvt->bg_pixbuf);
	}

	/* Clear a background file name, if one was set. */
        if (pvt->bg_file) {
                g_free(pvt->bg_file);
                pvt->bg_file = NULL;

                g_object_notify(object, "background-image-file");
        }

	/* Set the new background. */
	pvt->bg_pixbuf = image;

        g_object_notify(object, "background-image-pixbuf");

	vte_terminal_queue_background_update(terminal);

        g_object_thaw_notify(object);
}

/**
 * vte_terminal_set_background_image_file:
 * @terminal: a #VteTerminal
 * @path: (type filename): path to an image file
 *
 * Sets a background image for the widget.  If specified by
 * vte_terminal_set_background_saturation(), the terminal will tint its
 * in-memory copy of the image before applying it to the terminal.
 */
void
vte_terminal_set_background_image_file(VteTerminal *terminal, const char *path)
{
        VteTerminalPrivate *pvt;
        GObject *object;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        object = G_OBJECT(terminal);
        pvt = terminal->pvt;

	_vte_debug_print(VTE_DEBUG_MISC,
			"Loading background image from `%s'.\n", path);

        g_object_freeze_notify(G_OBJECT(terminal));

	/* Save this background type. */
	g_free(pvt->bg_file);
	pvt->bg_file = g_strdup(path);

	/* Turn off other background types. */
	if (pvt->bg_pixbuf != NULL) {
		g_object_unref(pvt->bg_pixbuf);
		pvt->bg_pixbuf = NULL;

                g_object_notify(object, "background-image-pixbuf");
	}

        g_object_notify(object, "background-image-file");

	vte_terminal_queue_background_update(terminal);

        g_object_thaw_notify(G_OBJECT(terminal));
}

/**
 * vte_terminal_get_has_selection:
 * @terminal: a #VteTerminal
 *
 * Checks if the terminal currently contains selected text.  Note that this
 * is different from determining if the terminal is the owner of any
 * #GtkClipboard items.
 *
 * Returns: %TRUE if part of the text in the terminal is selected.
 */
gboolean
vte_terminal_get_has_selection(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);
	return terminal->pvt->has_selection;
}

/**
 * vte_terminal_get_using_xft:
 * @terminal: a #VteTerminal
 *
 * A #VteTerminal can use multiple methods to draw text.  This function
 * allows an application to determine whether or not the current method uses
 * fontconfig to find fonts.  This setting cannot be changed by the caller,
 * but in practice usually matches the behavior of GTK+ itself.
 *
 * Returns: %TRUE
 *
 * Deprecated: 0.20
 */
gboolean
vte_terminal_get_using_xft(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), TRUE);
	return TRUE;
}

static void
vte_terminal_set_cursor_blinks_internal(VteTerminal *terminal, gboolean blink)
{
        VteTerminalPrivate *pvt = terminal->pvt;

	blink = !!blink;
	if (pvt->cursor_blinks == blink)
		return;

	pvt->cursor_blinks = blink;
	_vte_check_cursor_blink (terminal);
}

/**
 * vte_terminal_set_cursor_blinks:
 * @terminal: a #VteTerminal
 * @blink: whether the cursor should blink
 *
 *  Sets whether or not the cursor will blink.
 *
 * Deprecated: 0.17.1 Use vte_terminal_set_cursor_blink_mode() instead.
 */
void
vte_terminal_set_cursor_blinks(VteTerminal *terminal, gboolean blink)
{
        vte_terminal_set_cursor_blink_mode(terminal, blink ? VTE_CURSOR_BLINK_ON : VTE_CURSOR_BLINK_OFF);
}

/**
 * vte_terminal_set_cursor_blink_mode:
 * @terminal: a #VteTerminal
 * @mode: the #VteTerminalCursorBlinkMode to use
 *
 * Sets whether or not the cursor will blink. Using %VTE_CURSOR_BLINK_SYSTEM
 * will use the #GtkSettings::gtk-cursor-blink setting.
 *
 * Since: 0.17.1
 */
void
vte_terminal_set_cursor_blink_mode(VteTerminal *terminal, VteTerminalCursorBlinkMode mode)
{
        VteTerminalPrivate *pvt;
        gboolean blinks;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));
        pvt = terminal->pvt;

        if (pvt->cursor_blink_mode == mode)
                return;

        pvt->cursor_blink_mode = mode;

        switch (mode) {
          case VTE_CURSOR_BLINK_SYSTEM:
            g_object_get(gtk_widget_get_settings(GTK_WIDGET(terminal)),
                                                 "gtk-cursor-blink", &blinks,
                                                 NULL);
            break;
          case VTE_CURSOR_BLINK_ON:
            blinks = TRUE;
            break;
          case VTE_CURSOR_BLINK_OFF:
            blinks = FALSE;
            break;
        }

        vte_terminal_set_cursor_blinks_internal(terminal, blinks);

        g_object_notify(G_OBJECT(terminal), "cursor-blink-mode");
}

/**
 * vte_terminal_get_cursor_blink_mode:
 * @terminal: a #VteTerminal
 *
 * Returns the currently set cursor blink mode.
 *
 * Return value: cursor blink mode.
 *
 * Since: 0.17.1
 */
VteTerminalCursorBlinkMode
vte_terminal_get_cursor_blink_mode(VteTerminal *terminal)
{
        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), VTE_CURSOR_BLINK_SYSTEM);

        return terminal->pvt->cursor_blink_mode;
}

/**
 * vte_terminal_set_cursor_shape:
 * @terminal: a #VteTerminal
 * @shape: the #VteTerminalCursorShape to use
 *
 * Sets the shape of the cursor drawn.
 *
 * Since: 0.20
 */
void
vte_terminal_set_cursor_shape(VteTerminal *terminal, VteTerminalCursorShape shape)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));
        pvt = terminal->pvt;

        if (pvt->cursor_shape == shape)
                return;

        pvt->cursor_shape = shape;
	_vte_invalidate_cursor_once(terminal, FALSE);

        g_object_notify(G_OBJECT(terminal), "cursor-shape");
}

/**
 * vte_terminal_get_cursor_shape:
 * @terminal: a #VteTerminal
 *
 * Returns the currently set cursor shape.
 *
 * Return value: cursor shape.
 *
 * Since: 0.17.6
 */
VteTerminalCursorShape
vte_terminal_get_cursor_shape(VteTerminal *terminal)
{
        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), VTE_CURSOR_SHAPE_BLOCK);

        return terminal->pvt->cursor_shape;
}

/**
 * vte_terminal_set_scrollback_lines:
 * @terminal: a #VteTerminal
 * @lines: the length of the history buffer
 *
 * Sets the length of the scrollback buffer used by the terminal.  The size of
 * the scrollback buffer will be set to the larger of this value and the number
 * of visible rows the widget can display, so 0 can safely be used to disable
 * scrollback.
 *
 * A negative value means "infinite scrollback".
 *
 * Note that this setting only affects the normal screen buffer.
 * For terminal types which have an alternate screen buffer, no scrollback is
 * allowed on the alternate screen buffer.
 */
void
vte_terminal_set_scrollback_lines(VteTerminal *terminal, glong lines)
{
        VteTerminalPrivate *pvt;
        GObject *object;
	glong scroll_delta;
	VteScreen *screen;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	if (lines < 0)
		lines = G_MAXLONG;

        object = G_OBJECT(terminal);
        pvt = terminal->pvt;

#if 0
        /* FIXME: this breaks the scrollbar range, bug #562511 */
        if (lines == pvt->scrollback_lines)
                return;
#endif

        g_object_freeze_notify(object);

	_vte_debug_print (VTE_DEBUG_MISC,
			"Setting scrollback lines to %ld\n", lines);

	pvt->scrollback_lines = lines;
	screen = pvt->screen;
	scroll_delta = screen->scroll_delta;

	/* The main screen gets the full scrollback buffer, but the
	 * alternate screen isn't allowed to scroll at all. */
	if (screen == &terminal->pvt->normal_screen) {
		glong low, high, next;
		/* We need at least as many lines as are visible */
		lines = MAX (lines, terminal->row_count);
		next = MAX (screen->cursor_current.row + 1,
				_vte_ring_next (screen->row_data));
		_vte_ring_resize (screen->row_data, lines);
		low = _vte_ring_delta (screen->row_data);
		high = lines + MIN (G_MAXLONG - lines, low - terminal->row_count + 1);
		screen->insert_delta = CLAMP (screen->insert_delta, low, high);
		scroll_delta = CLAMP (scroll_delta, low, screen->insert_delta);
		next = MIN (next, screen->insert_delta + terminal->row_count);
		if (_vte_ring_next (screen->row_data) > next){
			_vte_ring_shrink (screen->row_data, next - low);
		}
	} else {
		_vte_ring_resize (screen->row_data, terminal->row_count);
		scroll_delta = _vte_ring_delta (screen->row_data);
		screen->insert_delta = _vte_ring_delta (screen->row_data);
		if (_vte_ring_next (screen->row_data) > screen->insert_delta + terminal->row_count){
			_vte_ring_shrink (screen->row_data, terminal->row_count);
		}
	}

	/* Adjust the scrollbars to the new locations. */
	vte_terminal_queue_adjustment_value_changed (terminal, scroll_delta);
	_vte_terminal_adjust_adjustments_full (terminal);

        g_object_notify(object, "scrollback-lines");

        g_object_thaw_notify(object);
}

/**
 * vte_terminal_set_word_chars:
 * @terminal: a #VteTerminal
 * @spec: a specification
 *
 * When the user double-clicks to start selection, the terminal will extend
 * the selection on word boundaries.  It will treat characters included in @spec
 * as parts of words, and all other characters as word separators.  Ranges of
 * characters can be specified by separating them with a hyphen.
 *
 * As a special case, if @spec is %NULL or the empty string, the terminal will
 * treat all graphic non-punctuation non-space characters as word characters.
 */
void
vte_terminal_set_word_chars(VteTerminal *terminal, const char *spec)
{
	VteConv conv;
	gunichar *wbuf;
	guchar *ibuf, *ibufptr, *obuf, *obufptr;
	gsize ilen, olen;
	VteWordCharRange range;
	guint i;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	/* Allocate a new range array. */
	if (terminal->pvt->word_chars != NULL) {
		g_array_free(terminal->pvt->word_chars, TRUE);
	}
	terminal->pvt->word_chars = g_array_new(FALSE, TRUE,
						sizeof(VteWordCharRange));
	/* Special case: if spec is NULL, try to do the right thing. */
	if (spec == NULL || spec[0] == '\0') {
                g_object_notify(G_OBJECT(terminal), "word-chars");
		return;
	}
	/* Convert the spec from UTF-8 to a string of gunichars . */
        /* FIXME: why not just directly use g_utf8_to_ucs4 here? It'll never fail */
	conv = _vte_conv_open(VTE_CONV_GUNICHAR_TYPE, "UTF-8");
	if (conv == VTE_INVALID_CONV) {
		/* Aaargh.  We're screwed. */
		g_warning(_("_vte_conv_open() failed setting word characters"));
		return;
	}
	ilen = strlen(spec);
	ibuf = ibufptr = (guchar *)g_strdup(spec);
	olen = (ilen + 1) * sizeof(gunichar);
	_vte_buffer_set_minimum_size(terminal->pvt->conv_buffer, olen);
	obuf = obufptr = terminal->pvt->conv_buffer->data;
	wbuf = (gunichar*) obuf;
	wbuf[ilen] = '\0';
	_vte_conv(conv, (const guchar **)&ibuf, &ilen, &obuf, &olen);
	_vte_conv_close(conv);
	for (i = 0; i < ((obuf - obufptr) / sizeof(gunichar)); i++) {
		/* The hyphen character. */
		if (wbuf[i] == '-') {
			range.start = wbuf[i];
			range.end = wbuf[i];
			g_array_append_val(terminal->pvt->word_chars, range);
			_vte_debug_print(VTE_DEBUG_MISC,
				"Word charset includes hyphen.\n");
			continue;
		}
		/* A single character, not the start of a range. */
		if ((wbuf[i] != '-') && (wbuf[i + 1] != '-')) {
			range.start = wbuf[i];
			range.end = wbuf[i];
			g_array_append_val(terminal->pvt->word_chars, range);
			_vte_debug_print(VTE_DEBUG_MISC,
					"Word charset includes `%lc'.\n",
					(wint_t) wbuf[i]);
			continue;
		}
		/* The start of a range. */
		if ((wbuf[i] != '-') &&
		    (wbuf[i + 1] == '-') &&
		    (wbuf[i + 2] != '-') &&
		    (wbuf[i + 2] != 0)) {
			range.start = wbuf[i];
			range.end = wbuf[i + 2];
			g_array_append_val(terminal->pvt->word_chars, range);
			_vte_debug_print(VTE_DEBUG_MISC,
					"Word charset includes range from "
					"`%lc' to `%lc'.\n", (wint_t) wbuf[i],
					(wint_t) wbuf[i + 2]);
			i += 2;
			continue;
		}
	}
	g_free(ibufptr);

        g_object_notify(G_OBJECT(terminal), "word-chars");
}

/**
 * vte_terminal_set_backspace_binding:
 * @terminal: a #VteTerminal
 * @binding: a #VteTerminalEraseBinding for the backspace key
 *
 * Modifies the terminal's backspace key binding, which controls what
 * string or control sequence the terminal sends to its child when the user
 * presses the backspace key.
 */
void
vte_terminal_set_backspace_binding(VteTerminal *terminal,
				   VteTerminalEraseBinding binding)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        if (binding == pvt->backspace_binding)
                return;

	/* FIXME: should we set the pty mode to match? */
	pvt->backspace_binding = binding;

        g_object_notify(G_OBJECT(terminal), "backspace-binding");
}

/**
 * vte_terminal_set_delete_binding:
 * @terminal: a #VteTerminal
 * @binding: a #VteTerminalEraseBinding for the delete key
 *
 * Modifies the terminal's delete key binding, which controls what
 * string or control sequence the terminal sends to its child when the user
 * presses the delete key.
 */
void
vte_terminal_set_delete_binding(VteTerminal *terminal,
				VteTerminalEraseBinding binding)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        if (binding == pvt->delete_binding)
                return;

	pvt->delete_binding = binding;

        g_object_notify(G_OBJECT(terminal), "delete-binding");
}

/**
 * vte_terminal_set_mouse_autohide:
 * @terminal: a #VteTerminal
 * @setting: whether the mouse pointer should autohide
 *
 * Changes the value of the terminal's mouse autohide setting.  When autohiding
 * is enabled, the mouse cursor will be hidden when the user presses a key and
 * shown when the user moves the mouse.  This setting can be read using
 * vte_terminal_get_mouse_autohide().
 */
void
vte_terminal_set_mouse_autohide(VteTerminal *terminal, gboolean setting)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        setting = setting != FALSE;
        if (setting == pvt->mouse_autohide)
                return;

	pvt->mouse_autohide = setting;

        g_object_notify(G_OBJECT(terminal), "pointer-autohide");
}

/**
 * vte_terminal_get_mouse_autohide:
 * @terminal: a #VteTerminal
 *
 * Determines the value of the terminal's mouse autohide setting.  When
 * autohiding is enabled, the mouse cursor will be hidden when the user presses
 * a key and shown when the user moves the mouse.  This setting can be changed
 * using vte_terminal_set_mouse_autohide().
 *
 * Returns: %TRUE if autohiding is enabled, %FALSE if not
 */
gboolean
vte_terminal_get_mouse_autohide(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);
	return terminal->pvt->mouse_autohide;
}

/**
 * vte_terminal_reset:
 * @terminal: a #VteTerminal
 * @clear_tabstops: whether to reset tabstops
 * @clear_history: whether to empty the terminal's scrollback buffer
 *
 * Resets as much of the terminal's internal state as possible, discarding any
 * unprocessed input data, resetting character attributes, cursor state,
 * national character set state, status line, terminal modes (insert/delete),
 * selection state, and encoding.
 *
 */
void
vte_terminal_reset(VteTerminal *terminal,
                   gboolean clear_tabstops,
                   gboolean clear_history)
{
        VteTerminalPrivate *pvt;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

        pvt = terminal->pvt;

        g_object_freeze_notify(G_OBJECT(terminal));

	/* Stop processing any of the data we've got backed up. */
	vte_terminal_stop_processing (terminal);

	/* Clear the input and output buffers. */
	_vte_incoming_chunks_release (pvt->incoming);
	pvt->incoming = NULL;
	g_array_set_size(pvt->pending, 0);
	_vte_buffer_clear(pvt->outgoing);
	/* Reset charset substitution state. */
	_vte_iso2022_state_free(pvt->iso2022);
	pvt->iso2022 = _vte_iso2022_state_new(NULL,
							&_vte_terminal_codeset_changed_cb,
							terminal);
	_vte_iso2022_state_set_codeset(pvt->iso2022,
				       pvt->encoding);
	/* Reset keypad/cursor/function key modes. */
	pvt->keypad_mode = VTE_KEYMODE_NORMAL;
	pvt->cursor_mode = VTE_KEYMODE_NORMAL;
	pvt->sun_fkey_mode = FALSE;
	pvt->hp_fkey_mode = FALSE;
	pvt->legacy_fkey_mode = FALSE;
	pvt->vt220_fkey_mode = FALSE;
	/* Enable meta-sends-escape. */
	pvt->meta_sends_escape = TRUE;
	/* Disable smooth scroll. */
	pvt->smooth_scroll = FALSE;
	/* Disable margin bell. */
	pvt->margin_bell = FALSE;
	/* Enable iso2022/NRC processing. */
	pvt->nrc_mode = TRUE;
	/* Reset saved settings. */
	if (pvt->dec_saved != NULL) {
		g_hash_table_destroy(pvt->dec_saved);
		pvt->dec_saved = g_hash_table_new(NULL, NULL);
	}
	/* Reset the color palette. */
	/* vte_terminal_set_default_colors(terminal); */
	/* Reset the default attributes.  Reset the alternate attribute because
	 * it's not a real attribute, but we need to treat it as one here. */
	pvt->screen = &pvt->alternate_screen;
	_vte_terminal_set_default_attributes(terminal);
	pvt->screen = &pvt->normal_screen;
	_vte_terminal_set_default_attributes(terminal);
	/* Reset alternate charset mode. */
	pvt->normal_screen.alternate_charset = FALSE;
	pvt->alternate_screen.alternate_charset = FALSE;
	/* Clear the scrollback buffers and reset the cursors. */
	if (clear_history) {
		_vte_ring_fini(pvt->normal_screen.row_data);
		_vte_ring_init(pvt->normal_screen.row_data, pvt->scrollback_lines);
		_vte_ring_fini(pvt->alternate_screen.row_data);
		_vte_ring_init(pvt->alternate_screen.row_data, terminal->row_count);
		pvt->normal_screen.cursor_saved.row = 0;
		pvt->normal_screen.cursor_saved.col = 0;
		pvt->normal_screen.cursor_current.row = 0;
		pvt->normal_screen.cursor_current.col = 0;
		pvt->normal_screen.scroll_delta = 0;
		pvt->normal_screen.insert_delta = 0;
		pvt->alternate_screen.cursor_saved.row = 0;
		pvt->alternate_screen.cursor_saved.col = 0;
		pvt->alternate_screen.cursor_current.row = 0;
		pvt->alternate_screen.cursor_current.col = 0;
		pvt->alternate_screen.scroll_delta = 0;
		pvt->alternate_screen.insert_delta = 0;
		_vte_terminal_adjust_adjustments_full (terminal);
	}
	/* Clear the status lines. */
	pvt->normal_screen.status_line = FALSE;
	pvt->normal_screen.status_line_changed = FALSE;
	if (pvt->normal_screen.status_line_contents != NULL) {
		g_string_free(pvt->normal_screen.status_line_contents,
			      TRUE);
	}
	pvt->normal_screen.status_line_contents = g_string_new(NULL);
	pvt->alternate_screen.status_line = FALSE;
	pvt->alternate_screen.status_line_changed = FALSE;
	if (pvt->alternate_screen.status_line_contents != NULL) {
		g_string_free(pvt->alternate_screen.status_line_contents,
			      TRUE);
	}
	pvt->alternate_screen.status_line_contents = g_string_new(NULL);
	/* Do more stuff we refer to as a "full" reset. */
	if (clear_tabstops) {
		vte_terminal_set_default_tabstops(terminal);
	}
	/* Reset restricted scrolling regions, leave insert mode, make
	 * the cursor visible again. */
	pvt->normal_screen.scrolling_restricted = FALSE;
	pvt->normal_screen.sendrecv_mode = TRUE;
	pvt->normal_screen.insert_mode = FALSE;
	pvt->normal_screen.linefeed_mode = FALSE;
	pvt->normal_screen.origin_mode = FALSE;
	pvt->normal_screen.reverse_mode = FALSE;
	pvt->normal_screen.bracketed_paste_mode = FALSE;
	pvt->alternate_screen.scrolling_restricted = FALSE;
	pvt->alternate_screen.sendrecv_mode = TRUE;
	pvt->alternate_screen.insert_mode = FALSE;
	pvt->alternate_screen.linefeed_mode = FALSE;
	pvt->alternate_screen.origin_mode = FALSE;
	pvt->alternate_screen.reverse_mode = FALSE;
	pvt->alternate_screen.bracketed_paste_mode = FALSE;
	pvt->cursor_visible = TRUE;
	/* Reset the encoding. */
	vte_terminal_set_encoding(terminal, NULL);
	g_assert(pvt->encoding != NULL);
	/* Reset selection. */
	vte_terminal_deselect_all(terminal);
	pvt->has_selection = FALSE;
	pvt->selecting = FALSE;
	pvt->selecting_restart = FALSE;
	pvt->selecting_had_delta = FALSE;
	if (pvt->selection != NULL) {
		g_free(pvt->selection);
		pvt->selection = NULL;
		memset(&pvt->selection_origin, 0,
		       sizeof(&pvt->selection_origin));
		memset(&pvt->selection_last, 0,
		       sizeof(&pvt->selection_last));
		memset(&pvt->selection_start, 0,
		       sizeof(&pvt->selection_start));
		memset(&pvt->selection_end, 0,
		       sizeof(&pvt->selection_end));
	}
	/* Reset mouse motion events. */
	pvt->mouse_tracking_mode = MOUSE_TRACKING_NONE;
	pvt->mouse_last_button = 0;
	pvt->mouse_last_x = 0;
	pvt->mouse_last_y = 0;
	/* Clear modifiers. */
	pvt->modifiers = 0;
	/* Cause everything to be redrawn (or cleared). */
	vte_terminal_maybe_scroll_to_bottom(terminal);
	_vte_invalidate_all(terminal);

        g_object_thaw_notify(G_OBJECT(terminal));
}

/**
 * vte_terminal_get_status_line:
 * @terminal: a #VteTerminal
 *
 * Some terminal emulations specify a status line which is separate from the
 * main display area, and define a means for applications to move the cursor
 * to the status line and back.
 *
 * Returns: (transfer none): the current contents of the terminal's status line.
 *   For terminals like "xterm", this will usually be the empty string.  The string
 *   must not be modified or freed by the caller.
 */
const char *
vte_terminal_get_status_line(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return terminal->pvt->screen->status_line_contents->str;
}

/**
 * vte_terminal_get_padding:
 * @terminal: a #VteTerminal
 * @xpad: address in which to store left/right-edge padding
 * @ypad: address in which to store top/bottom-edge ypadding
 *
 * Determines the amount of additional space the widget is using to pad the
 * edges of its visible area.  This is necessary for cases where characters in
 * the selected font don't themselves include a padding area and the text
 * itself would otherwise be contiguous with the window border.  Applications
 * which use the widget's %row_count, %column_count, %char_height, and
 * %char_width fields to set geometry hints using
 * gtk_window_set_geometry_hints() will need to add this value to the base
 * size.  The values returned in @xpad and @ypad are the total padding used in
 * each direction, and do not need to be doubled.
 *
 * Deprecated: 0.26: Get the #VteTerminal:inner-border style property instead
 */
void
vte_terminal_get_padding(VteTerminal *terminal, int *xpad, int *ypad)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	g_return_if_fail(xpad != NULL);
	g_return_if_fail(ypad != NULL);
	*xpad = terminal->pvt->inner_border.left + terminal->pvt->inner_border.right;
	*ypad = terminal->pvt->inner_border.top + terminal->pvt->inner_border.bottom;
}

/**
 * vte_terminal_get_adjustment:
 * @terminal: a #VteTerminal
 *
 * An accessor function provided for the benefit of language bindings.
 *
 * Returns: (transfer none): the contents of @terminal's adjustment field
 *
 * Deprecated: 0.28: Use gtk_scrollable_get_vadjustment() instead
 */
GtkAdjustment *
vte_terminal_get_adjustment(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);
	return terminal->adjustment;
}

/**
 * vte_terminal_get_char_width:
 * @terminal: a #VteTerminal
 *
 * Returns: the width of a character cell
 */
glong
vte_terminal_get_char_width(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
	vte_terminal_ensure_font (terminal);
	return terminal->char_width;
}

/**
 * vte_terminal_get_char_height:
 * @terminal: a #VteTerminal
 *
 * Returns: the height of a character cell
 */
glong
vte_terminal_get_char_height(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
	vte_terminal_ensure_font (terminal);
	return terminal->char_height;
}

/**
 * vte_terminal_get_char_descent:
 * @terminal: a #VteTerminal
 *
 * An accessor function provided for the benefit of language bindings.
 *
 * Returns: the contents of @terminal's char_descent field
 *
 * Deprecated: 0.20
 */
glong
vte_terminal_get_char_descent(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
	vte_terminal_ensure_font (terminal);
	return terminal->char_descent;
}

/**
 * vte_terminal_get_char_ascent:
 * @terminal: a #VteTerminal
 *
 * An accessor function provided for the benefit of language bindings.
 *
 * Returns: the contents of @terminal's char_ascent field
 *
 * Deprecated: 0.20
 */
glong
vte_terminal_get_char_ascent(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
	vte_terminal_ensure_font (terminal);
	return terminal->char_ascent;
}

/**
 * vte_terminal_get_row_count:
 * @terminal: a #VteTerminal
 *
 *
 * Returns: the number of rows
 */
glong
vte_terminal_get_row_count(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
	return terminal->row_count;
}

/**
 * vte_terminal_get_column_count:
 * @terminal: a #VteTerminal
 *
 * Returns: the number of columns
 */
glong
vte_terminal_get_column_count(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
	return terminal->column_count;
}

/**
 * vte_terminal_get_window_title:
 * @terminal: a #VteTerminal
 *
 * Returns: (transfer none): the window title
 */
const char *
vte_terminal_get_window_title(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), "");
	return terminal->window_title;
}

/**
 * vte_terminal_get_icon_title:
 * @terminal: a #VteTerminal
 *
 * Returns: (transfer none): the icon title
 */
const char *
vte_terminal_get_icon_title(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), "");
	return terminal->icon_title;
}

/**
 * vte_terminal_set_pty:
 * @terminal: a #VteTerminal
 * @pty_master: a file descriptor of the master end of a PTY, or %-1
 *
 * Attach an existing PTY master side to the terminal widget.  Use
 * instead of vte_terminal_fork_command() or vte_terminal_forkpty().
 *
 * Since: 0.12.1
 *
 * Deprecated: 0.26: Use vte_pty_new_foreign() and vte_terminal_set_pty_object()
 */
void
vte_terminal_set_pty(VteTerminal *terminal, int pty_master)
{
        VtePty *pty;

        if (pty_master == -1) {
                vte_terminal_set_pty_object(terminal, NULL);
                return;
        }

        pty = vte_pty_new_foreign(pty_master, NULL);
        if (pty == NULL)
                return;

        vte_terminal_set_pty_object(terminal, pty);
        g_object_unref(pty);
}

/**
 * vte_terminal_set_pty_object:
 * @terminal: a #VteTerminal
 * @pty: (allow-none): a #VtePty, or %NULL
 *
 * Sets @pty as the PTY to use in @terminal.
 * Use %NULL to unset the PTY.
 *
 * Since: 0.26.
 */
void
vte_terminal_set_pty_object(VteTerminal *terminal,
                            VtePty *pty)
{
        VteTerminalPrivate *pvt;
        GObject *object;
        long flags;
        int pty_master;

        g_return_if_fail(VTE_IS_TERMINAL(terminal));
        g_return_if_fail(pty == NULL || VTE_IS_PTY(pty));

        pvt = terminal->pvt;
        if (pvt->pty == pty)
                return;

        object = G_OBJECT(terminal);

        g_object_freeze_notify(object);

        if (pvt->pty != NULL) {
                _vte_terminal_disconnect_pty_read(terminal);
                _vte_terminal_disconnect_pty_write(terminal);

                if (terminal->pvt->pty_channel != NULL) {
                        g_io_channel_unref (terminal->pvt->pty_channel);
                        pvt->pty_channel = NULL;
                }

		/* Take one last shot at processing whatever data is pending,
		 * then flush the buffers in case we're about to run a new
		 * command, disconnecting the timeout. */
		if (terminal->pvt->incoming != NULL) {
			vte_terminal_process_incoming(terminal);
			_vte_incoming_chunks_release (terminal->pvt->incoming);
			terminal->pvt->incoming = NULL;
			terminal->pvt->input_bytes = 0;
		}
		g_array_set_size(terminal->pvt->pending, 0);
		vte_terminal_stop_processing (terminal);

		/* Clear the outgoing buffer as well. */
		_vte_buffer_clear(terminal->pvt->outgoing);

                vte_pty_close(pvt->pty);
                g_object_unref(pvt->pty);
                pvt->pty = NULL;
        }

        if (pty == NULL) {
                pvt->pty = NULL;
                g_object_notify(object, "pty");
                g_object_notify(object, "pty-object");
                g_object_thaw_notify(object);
                return;
        }

        pvt->pty = g_object_ref(pty);
        pty_master = vte_pty_get_fd(pvt->pty);

        pvt->pty_channel = g_io_channel_unix_new (pty_master);
        g_io_channel_set_close_on_unref (pvt->pty_channel, FALSE);

        /* FIXMEchpe: vte_pty_open_unix98 does the inverse ... */
        /* Set the pty to be non-blocking. */
        flags = fcntl(pty_master, F_GETFL);
        if ((flags & O_NONBLOCK) == 0) {
                fcntl(pty_master, F_SETFL, flags | O_NONBLOCK);
        }

        vte_terminal_set_size(terminal,
                              terminal->column_count,
                              terminal->row_count);

        _vte_terminal_setup_utf8 (terminal);

        /* Open channels to listen for input on. */
        _vte_terminal_connect_pty_read (terminal);

        g_object_notify(object, "pty");
        g_object_notify(object, "pty-object");

        g_object_thaw_notify(object);
}

/**
 * vte_terminal_get_pty:
 * @terminal: a #VteTerminal
 *
 * Returns the file descriptor of the master end of @terminal's PTY.
 *
 * Return value: the file descriptor, or -1 if the terminal has no PTY.
 *
 * Since: 0.20
 *
 * Deprecated: 0.26: Use vte_terminal_get_pty_object() and vte_pty_get_fd()
 */
int
vte_terminal_get_pty(VteTerminal *terminal)
{
        VteTerminalPrivate *pvt;

        g_return_val_if_fail (VTE_IS_TERMINAL (terminal), -1);

        pvt = terminal->pvt;
        if (pvt->pty != NULL)
                return vte_pty_get_fd(pvt->pty);

        return -1;
}

/**
 * vte_terminal_get_pty_object:
 * @terminal: a #VteTerminal
 *
 * Returns the #VtePty of @terminal.
 *
 * Returns: (transfer none): a #VtePty, or %NULL
 *
 * Since: 0.26
 */
VtePty *
vte_terminal_get_pty_object(VteTerminal *terminal)
{
        g_return_val_if_fail (VTE_IS_TERMINAL (terminal), NULL);

        return terminal->pvt->pty;
}

/**
 * vte_terminal_get_child_exit_status:
 * @terminal: a #VteTerminal
 *
 * Gets the exit status of the command started by vte_terminal_fork_command().
 * See your C library's documentation for more details on how to interpret the
 * exit status.
 * 
 * Note that this function may only be called from the signal handler of
 * the #VteTerminal::child-exited signal.
 * 
 * Returns: the child's exit status
 * 
 * Since: 0.20
 */
int
vte_terminal_get_child_exit_status(VteTerminal *terminal)
{
        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), -1);
        return terminal->pvt->child_exit_status;
}

/* We need this bit of glue to ensure that accessible objects will always
 * get signals. */
void
_vte_terminal_accessible_ref(VteTerminal *terminal)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));
	terminal->pvt->accessible_emit = TRUE;
}

char *
_vte_terminal_get_selection(VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);

	return g_strdup (terminal->pvt->selection);
}

void
_vte_terminal_get_start_selection(VteTerminal *terminal, long *col, long *row)
{
	VteVisualPosition ss;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	ss = terminal->pvt->selection_start;

	if (col) {
		*col = ss.col;
	}

	if (row) {
		*row = ss.row;
	}
}

void
_vte_terminal_get_end_selection(VteTerminal *terminal, long *col, long *row)
{
	VteVisualPosition se;

	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	se = terminal->pvt->selection_end;

	if (col) {
		*col = se.col;
	}

	if (row) {
		*row = se.row;
	}
}

void
_vte_terminal_select_text(VteTerminal *terminal,
			  long start_col, long start_row,
			  long end_col, long end_row,
			  int start_offset, int end_offset)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	vte_terminal_deselect_all (terminal);

	terminal->pvt->selection_type = selection_type_char;
	terminal->pvt->selecting_had_delta = TRUE;
	terminal->pvt->selection_start.col = start_col;
	terminal->pvt->selection_start.row = start_row;
	terminal->pvt->selection_end.col = end_col;
	terminal->pvt->selection_end.row = end_row;
	vte_terminal_copy_primary(terminal);
	vte_terminal_emit_selection_changed(terminal);

	_vte_invalidate_region (terminal,
			MIN (start_col, end_col), MAX (start_col, end_col),
			MIN (start_row, end_row), MAX (start_row, end_row),
			FALSE);

}

void
_vte_terminal_remove_selection(VteTerminal *terminal)
{
	vte_terminal_deselect_all (terminal);
}

static void
_vte_terminal_select_empty_at(VteTerminal *terminal,
			      long col, long row)
{
	_vte_terminal_select_text(terminal, col, row, col - 1, row, 0, 0);
}

static void
add_update_timeout (VteTerminal *terminal)
{
	if (update_timeout_tag == 0) {
		_vte_debug_print (VTE_DEBUG_TIMEOUT,
				"Starting update timeout\n");
		update_timeout_tag =
			g_timeout_add_full (GDK_PRIORITY_REDRAW,
					VTE_UPDATE_TIMEOUT,
					update_timeout, NULL,
					NULL);
	}
	if (in_process_timeout == FALSE &&
			process_timeout_tag != 0) {
		_vte_debug_print (VTE_DEBUG_TIMEOUT,
				"Removing process timeout\n");
		g_source_remove (process_timeout_tag);
		process_timeout_tag = 0;
	}
	if (terminal->pvt->active == NULL) {
		_vte_debug_print (VTE_DEBUG_TIMEOUT,
				"Adding terminal to active list\n");
		terminal->pvt->active = active_terminals =
			g_list_prepend (active_terminals, terminal);
	}

}
static void
reset_update_regions (VteTerminal *terminal)
{
	if (terminal->pvt->update_regions != NULL) {
		g_slist_foreach (terminal->pvt->update_regions,
				(GFunc)gdk_region_destroy, NULL);
		g_slist_free (terminal->pvt->update_regions);
		terminal->pvt->update_regions = NULL;
	}
	/* the invalidated_all flag also marks whether to skip processing
	 * due to the widget being invisible */
	terminal->pvt->invalidated_all =
		terminal->pvt->visibility_state==GDK_VISIBILITY_FULLY_OBSCURED;
}

static void
remove_from_active_list (VteTerminal *terminal)
{
	if (terminal->pvt->active != NULL
			&& terminal->pvt->update_regions == NULL) {
		_vte_debug_print(VTE_DEBUG_TIMEOUT,
			"Removing terminal from active list\n");
		active_terminals = g_list_delete_link (active_terminals,
				terminal->pvt->active);
		terminal->pvt->active = NULL;

		if (active_terminals == NULL) {
			if (in_process_timeout == FALSE &&
					process_timeout_tag != 0) {
				_vte_debug_print(VTE_DEBUG_TIMEOUT,
						"Removing process timeout\n");
				g_source_remove (process_timeout_tag);
				process_timeout_tag = 0;
			}
			if (in_update_timeout == FALSE &&
					update_timeout_tag != 0) {
				_vte_debug_print(VTE_DEBUG_TIMEOUT,
						"Removing update timeout\n");
				g_source_remove (update_timeout_tag);
				update_timeout_tag = 0;
			}
		}
	}
}
static void
remove_update_timeout (VteTerminal *terminal)
{
	reset_update_regions (terminal);
	remove_from_active_list (terminal);
}

static void
vte_terminal_add_process_timeout (VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_TIMEOUT,
			"Adding terminal to active list\n");
	terminal->pvt->active = active_terminals =
		g_list_prepend (active_terminals, terminal);
	if (update_timeout_tag == 0 &&
			process_timeout_tag == 0) {
		_vte_debug_print(VTE_DEBUG_TIMEOUT,
				"Starting process timeout\n");
		process_timeout_tag =
			g_timeout_add (VTE_DISPLAY_TIMEOUT,
					process_timeout, NULL);
	}
}
static inline gboolean
vte_terminal_is_processing (VteTerminal *terminal)
{
	return terminal->pvt->active != NULL;
}
static inline void
vte_terminal_start_processing (VteTerminal *terminal)
{
	if (!vte_terminal_is_processing (terminal)) {
		vte_terminal_add_process_timeout (terminal);
	}
}

static void
vte_terminal_stop_processing (VteTerminal *terminal)
{
	remove_from_active_list (terminal);
}

static inline gboolean
need_processing (VteTerminal *terminal)
{
	return _vte_incoming_chunks_length (terminal->pvt->incoming) != 0;
}

/* Emit an "icon-title-changed" signal. */
static void
vte_terminal_emit_icon_title_changed(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `icon-title-changed'.\n");
	g_signal_emit_by_name(terminal, "icon-title-changed");
}

/* Emit a "window-title-changed" signal. */
static void
vte_terminal_emit_window_title_changed(VteTerminal *terminal)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS,
			"Emitting `window-title-changed'.\n");
	g_signal_emit_by_name(terminal, "window-title-changed");
}

static void
vte_terminal_emit_pending_signals(VteTerminal *terminal)
{
        GObject *object;
	GdkWindow *window;

	object = G_OBJECT (terminal);
	window = gtk_widget_get_window (&terminal->widget);

        g_object_freeze_notify(object);

	vte_terminal_emit_adjustment_changed (terminal);

	if (terminal->pvt->screen->status_line_changed) {
		_vte_terminal_emit_status_line_changed (terminal);
		terminal->pvt->screen->status_line_changed = FALSE;
	}

	if (terminal->pvt->window_title_changed) {
		g_free (terminal->window_title);
		terminal->window_title = terminal->pvt->window_title_changed;
		terminal->pvt->window_title_changed = NULL;

		if (window)
			gdk_window_set_title (window, terminal->window_title);
		vte_terminal_emit_window_title_changed(terminal);
                g_object_notify(object, "window-title");
	}

	if (terminal->pvt->icon_title_changed) {
		g_free (terminal->icon_title);
		terminal->icon_title = terminal->pvt->icon_title_changed;
		terminal->pvt->icon_title_changed = NULL;

		if (window)
			gdk_window_set_icon_name (window, terminal->icon_title);
		vte_terminal_emit_icon_title_changed(terminal);
                g_object_notify(object, "icon-title");
	}

	/* Flush any pending "inserted" signals. */
	vte_terminal_emit_cursor_moved(terminal);
	vte_terminal_emit_pending_text_signals(terminal, 0);
	vte_terminal_emit_contents_changed (terminal);

        g_object_thaw_notify(object);
}

static void time_process_incoming (VteTerminal *terminal)
{
	gdouble elapsed;
	glong target;
	g_timer_reset (process_timer);
	vte_terminal_process_incoming (terminal);
	elapsed = g_timer_elapsed (process_timer, NULL) * 1000;
	target = VTE_MAX_PROCESS_TIME / elapsed * terminal->pvt->input_bytes;
	terminal->pvt->max_input_bytes =
		(terminal->pvt->max_input_bytes + target) / 2;
}


/* This function is called after DISPLAY_TIMEOUT ms.
 * It makes sure initial output is never delayed by more than DISPLAY_TIMEOUT
 */
static gboolean
process_timeout (gpointer data)
{
	GList *l, *next;
	gboolean again;

	GDK_THREADS_ENTER();

	in_process_timeout = TRUE;

	_vte_debug_print (VTE_DEBUG_WORK, "<");
	_vte_debug_print (VTE_DEBUG_TIMEOUT,
			"Process timeout:  %d active\n",
			g_list_length (active_terminals));

	for (l = active_terminals; l != NULL; l = next) {
		VteTerminal *terminal = l->data;
		gboolean active = FALSE;

		next = g_list_next (l);

		if (l != active_terminals) {
			_vte_debug_print (VTE_DEBUG_WORK, "T");
		}
		if (terminal->pvt->pty_channel != NULL) {
			if (terminal->pvt->pty_input_active ||
					terminal->pvt->pty_input_source == 0) {
				terminal->pvt->pty_input_active = FALSE;
				vte_terminal_io_read (terminal->pvt->pty_channel,
						G_IO_IN, terminal);
			}
			_vte_terminal_enable_input_source (terminal);
		}
		if (need_processing (terminal)) {
			active = TRUE;
			if (VTE_MAX_PROCESS_TIME) {
				time_process_incoming (terminal);
			} else {
				vte_terminal_process_incoming(terminal);
			}
			terminal->pvt->input_bytes = 0;
		} else
			vte_terminal_emit_pending_signals (terminal);
		if (!active && terminal->pvt->update_regions == NULL) {
			if (terminal->pvt->active != NULL) {
				_vte_debug_print(VTE_DEBUG_TIMEOUT,
						"Removing terminal from active list [process]\n");
				active_terminals = g_list_delete_link (
						active_terminals,
						terminal->pvt->active);
				terminal->pvt->active = NULL;
			}
		}
	}

	_vte_debug_print (VTE_DEBUG_WORK, ">");

	if (active_terminals && update_timeout_tag == 0) {
		again = TRUE;
	} else {
		_vte_debug_print(VTE_DEBUG_TIMEOUT,
				"Stoping process timeout\n");
		process_timeout_tag = 0;
		again = FALSE;
	}

	in_process_timeout = FALSE;

	GDK_THREADS_LEAVE();

	if (again) {
		/* Force us to relinquish the CPU as the child is running
		 * at full tilt and making us run to keep up...
		 */
		g_usleep (0);
	} else if (update_timeout_tag == 0) {
		/* otherwise free up memory used to capture incoming data */
		prune_chunks (10);
	}

	return again;
}


static gboolean
update_regions (VteTerminal *terminal)
{
	GSList *l;
	GdkRegion *region;
	GdkWindow *window;

	if (G_UNLIKELY (! gtk_widget_is_drawable (&terminal->widget)
				|| terminal->pvt->visibility_state == GDK_VISIBILITY_FULLY_OBSCURED)) {
		reset_update_regions (terminal);
		return FALSE;
	}

	if (G_UNLIKELY (!terminal->pvt->update_regions))
		return FALSE;


	l = terminal->pvt->update_regions;
	if (g_slist_next (l) != NULL) {
		/* amalgamate into one super-region */
		region = gdk_region_new ();
		do {
			gdk_region_union (region, l->data);
			gdk_region_destroy (l->data);
		} while ((l = g_slist_next (l)) != NULL);
	} else {
		region = l->data;
	}
	g_slist_free (terminal->pvt->update_regions);
	terminal->pvt->update_regions = NULL;
	terminal->pvt->invalidated_all = FALSE;

	/* and perform the merge with the window visible area */
	window = gtk_widget_get_window (&terminal->widget);
	gdk_window_invalidate_region (window, region, FALSE);
	gdk_window_process_updates (window, FALSE);
	gdk_region_destroy (region);

	_vte_debug_print (VTE_DEBUG_WORK, "-");

	return TRUE;
}

static gboolean
update_repeat_timeout (gpointer data)
{
	GList *l, *next;
	gboolean again;

	GDK_THREADS_ENTER();

	in_update_timeout = TRUE;

	_vte_debug_print (VTE_DEBUG_WORK, "[");
	_vte_debug_print (VTE_DEBUG_TIMEOUT,
			"Repeat timeout:  %d active\n",
			g_list_length (active_terminals));

	for (l = active_terminals; l != NULL; l = next) {
		VteTerminal *terminal = l->data;

		next = g_list_next (l);

		if (l != active_terminals) {
			_vte_debug_print (VTE_DEBUG_WORK, "T");
		}
		if (terminal->pvt->pty_channel != NULL) {
			if (terminal->pvt->pty_input_active ||
					terminal->pvt->pty_input_source == 0) {
				terminal->pvt->pty_input_active = FALSE;
				vte_terminal_io_read (terminal->pvt->pty_channel,
						G_IO_IN, terminal);
			}
			_vte_terminal_enable_input_source (terminal);
		}
		if (terminal->pvt->bg_update_pending) {
			vte_terminal_background_update (terminal);
		}
		vte_terminal_emit_adjustment_changed (terminal);
		if (need_processing (terminal)) {
			if (VTE_MAX_PROCESS_TIME) {
				time_process_incoming (terminal);
			} else {
				vte_terminal_process_incoming (terminal);
			}
			terminal->pvt->input_bytes = 0;
		} else
			vte_terminal_emit_pending_signals (terminal);

		again = update_regions (terminal);
		if (!again) {
			if (terminal->pvt->active != NULL) {
				_vte_debug_print(VTE_DEBUG_TIMEOUT,
						"Removing terminal from active list [update]\n");
				active_terminals = g_list_delete_link (
						active_terminals,
						terminal->pvt->active);
				terminal->pvt->active = NULL;
			}
		}
	}


	if (active_terminals != NULL) {
		/* remove the idle source, and draw non-Terminals
		 * (except for gdk/{directfb,quartz}!)
		 */
		gdk_window_process_all_updates ();
	}

	_vte_debug_print (VTE_DEBUG_WORK, "]");

	/* We only stop the timer if no update request was received in this
	 * past cycle.
	 */
	again = TRUE;
	if (active_terminals == NULL) {
		_vte_debug_print(VTE_DEBUG_TIMEOUT,
				"Stoping update timeout\n");
		update_timeout_tag = 0;
		again = FALSE;
	}

	in_update_timeout = FALSE;

	GDK_THREADS_LEAVE();

	if (again) {
		/* Force us to relinquish the CPU as the child is running
		 * at full tilt and making us run to keep up...
		 */
		g_usleep (0);
	} else {
		/* otherwise free up memory used to capture incoming data */
		prune_chunks (10);
	}

	return again;
}

static gboolean
update_timeout (gpointer data)
{
	GList *l, *next;
	gboolean redraw = FALSE;

	GDK_THREADS_ENTER();

	in_update_timeout = TRUE;

	_vte_debug_print (VTE_DEBUG_WORK, "{");
	_vte_debug_print (VTE_DEBUG_TIMEOUT,
			"Update timeout:  %d active\n",
			g_list_length (active_terminals));

	if (process_timeout_tag != 0) {
		_vte_debug_print(VTE_DEBUG_TIMEOUT,
				"Removing process timeout\n");
		g_source_remove (process_timeout_tag);
		process_timeout_tag = 0;
	}

	for (l = active_terminals; l != NULL; l = next) {
		VteTerminal *terminal = l->data;

		next = g_list_next (l);

		if (l != active_terminals) {
			_vte_debug_print (VTE_DEBUG_WORK, "T");
		}
		if (terminal->pvt->pty_channel != NULL) {
			if (terminal->pvt->pty_input_active ||
					terminal->pvt->pty_input_source == 0) {
				terminal->pvt->pty_input_active = FALSE;
				vte_terminal_io_read (terminal->pvt->pty_channel,
						G_IO_IN, terminal);
			}
			_vte_terminal_enable_input_source (terminal);
		}
		if (terminal->pvt->bg_update_pending) {
			vte_terminal_background_update (terminal);
		}
		vte_terminal_emit_adjustment_changed (terminal);
		if (need_processing (terminal)) {
			if (VTE_MAX_PROCESS_TIME) {
				time_process_incoming (terminal);
			} else {
				vte_terminal_process_incoming (terminal);
			}
			terminal->pvt->input_bytes = 0;
		} else
			vte_terminal_emit_pending_signals (terminal);

		redraw |= update_regions (terminal);
	}

	if (redraw) {
		/* remove the idle source, and draw non-Terminals
		 * (except for gdk/{directfb,quartz}!)
		 */
		gdk_window_process_all_updates ();
	}

	_vte_debug_print (VTE_DEBUG_WORK, "}");

	/* Set a timer such that we do not invalidate for a while. */
	/* This limits the number of times we draw to ~40fps. */
	update_timeout_tag =
		g_timeout_add_full (G_PRIORITY_DEFAULT_IDLE,
				    VTE_UPDATE_REPEAT_TIMEOUT,
				    update_repeat_timeout, NULL,
				    NULL);
	in_update_timeout = FALSE;

	GDK_THREADS_LEAVE();

	return FALSE;
}

/**
 * vte_terminal_write_contents:
 * @terminal: a #VteTerminal
 * @stream: a #GOutputStream to write to
 * @flags: a set of #VteTerminalWriteFlags
 * @cancellable: (allow-none): a #GCancellable object, or %NULL
 * @error: (allow-none): a #GError location to store the error occuring, or %NULL
 *
 * Write contents of the current contents of @terminal (including any
 * scrollback history) to @stream according to @flags.
 *
 * If @cancellable is not %NULL, then the operation can be cancelled by triggering
 * the cancellable object from another thread. If the operation was cancelled,
 * the error %G_IO_ERROR_CANCELLED will be returned in @error.
 *
 * This is a synchronous operation and will make the widget (and input
 * processing) during the write operation, which may take a long time
 * depending on scrollback history and @stream availability for writing.
 *
 * Returns: %TRUE on success, %FALSE if there was an error
 *
 * Since: 0.24
 */
gboolean
vte_terminal_write_contents (VteTerminal *terminal,
                             GOutputStream *stream,
                             VteTerminalWriteFlags flags,
                             GCancellable *cancellable,
                             GError **error)
{
        g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);
        g_return_val_if_fail(G_IS_OUTPUT_STREAM(stream), FALSE);
	return _vte_ring_write_contents (terminal->pvt->screen->row_data,
					 stream, flags,
					 cancellable, error);
}


/*
 * Buffer search
 */

/* TODO Add properties & signals */

/**
 * vte_terminal_search_set_gregex:
 * @terminal: a #VteTerminal
 * @regex: (allow-none): a #GRegex, or %NULL
 *
 * Sets the #GRegex regex to search for. Unsets the search regex when passed %NULL.
 *
 * Since: 0.26
 */
void
vte_terminal_search_set_gregex (VteTerminal *terminal,
				GRegex      *regex)
{
        g_return_if_fail(VTE_IS_TERMINAL(terminal));

	if (terminal->pvt->search_regex == regex)
		return;

	if (terminal->pvt->search_regex) {
		g_regex_unref (terminal->pvt->search_regex);
		terminal->pvt->search_regex = NULL;
	}

	if (regex)
		terminal->pvt->search_regex = g_regex_ref (regex);

	_vte_invalidate_all (terminal);
}

/**
 * vte_terminal_search_get_gregex:
 * @terminal: a #VteTerminal
 *
 * Returns: (transfer none): the search #GRegex regex set in @terminal, or %NULL
 *
 * Since: 0.26
 */
GRegex *
vte_terminal_search_get_gregex (VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);

	return terminal->pvt->search_regex;
}

/**
 * vte_terminal_search_set_wrap_around:
 * @terminal: a #VteTerminal
 * @wrap_around: whether search should wrap
 *
 * Sets whether search should wrap around to the beginning of the
 * terminal content when reaching its end.
 * 
 * Since: 0.26
 */
void
vte_terminal_search_set_wrap_around (VteTerminal *terminal,
				     gboolean     wrap_around)
{
	g_return_if_fail(VTE_IS_TERMINAL(terminal));

	terminal->pvt->search_wrap_around = !!wrap_around;
}

/**
 * vte_terminal_search_get_wrap_around:
 * @terminal: a #VteTerminal
 *
 * Returns: whether searching will wrap around
 *
 * Since: 0.26
 */
gboolean
vte_terminal_search_get_wrap_around (VteTerminal *terminal)
{
	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);

	return terminal->pvt->search_wrap_around;
}

static gboolean
vte_terminal_search_rows (VteTerminal *terminal,
			  long start_row,
			  long end_row,
			  gboolean backward)
{
        VteTerminalPrivate *pvt;
	char *row_text;
	GMatchInfo *match_info;
	GError *error = NULL;
	int start, end;
	long start_col, end_col;
	gchar *word;
	VteCharAttributes *ca;
	GArray *attrs;
	gdouble value, page_size;

	pvt = terminal->pvt;

	row_text = vte_terminal_get_text_range (terminal, start_row, 0, end_row, -1, NULL, NULL, NULL);

	g_regex_match_full (pvt->search_regex, row_text, -1, 0, G_REGEX_MATCH_NOTEMPTY, &match_info, &error);
	if (error) {
		g_printerr ("Error while matching: %s\n", error->message);
		g_error_free (error);
		g_match_info_free (match_info);
		g_free (row_text);
		return TRUE;
	}

	if (!g_match_info_matches (match_info)) {
		g_match_info_free (match_info);
		g_free (row_text);
		return FALSE;
	}

	word = g_match_info_fetch (match_info, 0);

	/* Fetch text again, with attributes */
	g_free (row_text);
	if (!pvt->search_attrs)
		pvt->search_attrs = g_array_new (FALSE, TRUE, sizeof (VteCharAttributes));
	attrs = pvt->search_attrs;
	row_text = vte_terminal_get_text_range (terminal, start_row, 0, end_row, -1, NULL, NULL, attrs);

	/* This gives us the offset in the buffer */
	g_match_info_fetch_pos (match_info, 0, &start, &end);

	ca = &g_array_index (attrs, VteCharAttributes, start);
	start_row = ca->row;
	start_col = ca->column;
	ca = &g_array_index (attrs, VteCharAttributes, end - 1);
	end_row = ca->row;
	end_col = ca->column;

	g_free (word);
	g_free (row_text);
	g_match_info_free (match_info);

	_vte_terminal_select_text (terminal, start_col, start_row, end_col, end_row, 0, 0);
	/* Quite possibly the math here should not access adjustment directly... */
	value = gtk_adjustment_get_value(terminal->adjustment);
	page_size = gtk_adjustment_get_page_size(terminal->adjustment);
	if (backward) {
		if (end_row < value || end_row >= value + page_size)
			vte_terminal_queue_adjustment_value_changed_clamped (terminal, end_row - page_size + 1);
	} else {
		if (start_row < value || start_row >= value + page_size)
			vte_terminal_queue_adjustment_value_changed_clamped (terminal, start_row);
	}

	return TRUE;
}

static gboolean
vte_terminal_search_rows_iter (VteTerminal *terminal,
			       long start_row,
			       long end_row,
			       gboolean backward)
{
	const VteRowData *row;
	long iter_start_row, iter_end_row;

	if (backward) {
		iter_start_row = end_row;
		while (iter_start_row > start_row) {
			iter_end_row = iter_start_row;

			do {
				iter_start_row--;
				row = _vte_terminal_find_row_data (terminal, iter_start_row);
			} while (row && row->attr.soft_wrapped);

			if (vte_terminal_search_rows (terminal, iter_start_row, iter_end_row, backward))
				return TRUE;
		}
	} else {
		iter_end_row = start_row;
		while (iter_end_row < end_row) {
			iter_start_row = iter_end_row;

			do {
				row = _vte_terminal_find_row_data (terminal, iter_end_row);
				iter_end_row++;
			} while (row && row->attr.soft_wrapped);

			if (vte_terminal_search_rows (terminal, iter_start_row, iter_end_row, backward))
				return TRUE;
		}
	}

	return FALSE;
}

static gboolean
vte_terminal_search_find (VteTerminal *terminal,
			  gboolean     backward)
{
        VteTerminalPrivate *pvt;
	long buffer_start_row, buffer_end_row;
	long last_start_row, last_end_row;

	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), FALSE);

	pvt = terminal->pvt;
	if (!pvt->search_regex)
		return FALSE;

	/* TODO
	 * Currently We only find one result per extended line, and ignore columns
	 * Moreover, the whole search thing is implemented very inefficiently.
	 */

	buffer_start_row = _vte_ring_delta (terminal->pvt->screen->row_data);
	buffer_end_row = _vte_ring_next (terminal->pvt->screen->row_data);

	if (pvt->has_selection) {
		last_start_row = pvt->selection_start.row;
		last_end_row = pvt->selection_end.row + 1;
	} else {
		last_start_row = pvt->screen->scroll_delta + terminal->row_count;
		last_end_row = pvt->screen->scroll_delta;
	}
	last_start_row = MAX (buffer_start_row, last_start_row);
	last_end_row = MIN (buffer_end_row, last_end_row);

	/* If search fails, we make an empty selection at the last searched
	 * position... */
	if (backward) {
		if (vte_terminal_search_rows_iter (terminal, buffer_start_row, last_start_row, backward))
			return TRUE;
		if (pvt->search_wrap_around &&
		    vte_terminal_search_rows_iter (terminal, last_end_row, buffer_end_row, backward))
			return TRUE;
		if (pvt->has_selection) {
			if (pvt->search_wrap_around)
			    _vte_terminal_select_empty_at (terminal,
							   pvt->selection_start.col,
							   pvt->selection_start.row);
			else
			    _vte_terminal_select_empty_at (terminal,
							   -1,
							   buffer_start_row - 1);
		}
	} else {
		if (vte_terminal_search_rows_iter (terminal, last_end_row, buffer_end_row, backward))
			return TRUE;
		if (pvt->search_wrap_around &&
		    vte_terminal_search_rows_iter (terminal, buffer_start_row, last_start_row, backward))
			return TRUE;
		if (pvt->has_selection) {
			if (pvt->search_wrap_around)
			    _vte_terminal_select_empty_at (terminal,
							   pvt->selection_end.col + 1,
							   pvt->selection_end.row);
			else
			    _vte_terminal_select_empty_at (terminal,
							   -1,
							   buffer_end_row);
		}
	}

	return FALSE;
}

/**
 * vte_terminal_search_find_previous:
 * @terminal: a #VteTerminal
 *
 * Searches the previous string matching the search regex set with
 * vte_terminal_search_set_gregex().
 *
 * Returns: %TRUE if a match was found
 *
 * Since: 0.26
 */
gboolean
vte_terminal_search_find_previous (VteTerminal *terminal)
{
	return vte_terminal_search_find (terminal, TRUE);
}

/**
 * vte_terminal_search_find_next:
 * @terminal: a #VteTerminal
 *
 * Searches the next string matching the search regex set with
 * vte_terminal_search_set_gregex().
 *
 * Returns: %TRUE if a match was found
 *
 * Since: 0.26
 */
gboolean
vte_terminal_search_find_next (VteTerminal *terminal)
{
	return vte_terminal_search_find (terminal, FALSE);
}

#ifdef VTE_DEBUG
static const char *
_vte_keysym_name(guint keyval)
{
	switch (keyval) {
#include "keysyms.c"
		default:
			break;
	}
	return "(unknown)";
}
static void
_vte_keysym_print(guint keyval,
		GdkModifierType modifiers,
		gboolean sun_mode,
		gboolean hp_mode,
		gboolean legacy_mode,
		gboolean vt220_mode)
{
	g_printerr("Mapping ");
	if (modifiers & GDK_CONTROL_MASK) {
		g_printerr("Control+");
	}
	if (modifiers & VTE_META_MASK) {
		g_printerr("Meta+");
	}
	if (modifiers & VTE_NUMLOCK_MASK) {
		g_printerr("NumLock+");
	}
	if (modifiers & GDK_SHIFT_MASK) {
		g_printerr("Shift+");
	}
	g_printerr("%s" , _vte_keysym_name(keyval));
	if (sun_mode|hp_mode|legacy_mode|vt220_mode) {
		gboolean first = TRUE;
		g_printerr("(");
		if (sun_mode) {
			if (!first) {
				g_printerr(",");
			}
			first = FALSE;
			g_printerr("Sun");
		}
		if (hp_mode) {
			if (!first) {
				g_printerr(",");
			}
			first = FALSE;
			g_printerr("HP");
		}
		if (legacy_mode) {
			if (!first) {
				g_printerr(",");
			}
			first = FALSE;
			g_printerr("Legacy");
		}
		if (vt220_mode) {
			if (!first) {
				g_printerr(",");
			}
			first = FALSE;
			g_printerr("VT220");
		}
		g_printerr(")");
	}
}
#else
static void
_vte_keysym_print(guint keyval,
		GdkModifierType modifiers,
		gboolean sun_mode,
		gboolean hp_mode,
		gboolean legacy_mode,
		gboolean vt220_mode)
{
}
#endif

enum _vte_cursor_mode {
	cursor_default =	1 << 0,
	cursor_app =		1 << 1
};

enum _vte_keypad_mode {
	keypad_default =	1 << 0,
	keypad_app =		1 << 1
};

enum _vte_fkey_mode {
	fkey_default =	1 << 0,
	fkey_sun =	1 << 1,
	fkey_hp =	1 << 2,
	fkey_legacy =	1 << 3,
	fkey_vt220 =	1 << 4
};

#define cursor_all	(cursor_default | cursor_app)
#define keypad_all	(keypad_default | keypad_app)
#define fkey_all	(fkey_default | fkey_sun | fkey_hp | fkey_legacy | fkey_vt220)
#define fkey_notvt220	(fkey_default | fkey_sun | fkey_hp | fkey_legacy)
#define fkey_notsun	(fkey_default | fkey_hp | fkey_legacy | fkey_vt220)
#define fkey_nothp	(fkey_default | fkey_sun | fkey_legacy | fkey_vt220)
#define fkey_notsunvt	(fkey_default | fkey_hp | fkey_legacy)
#define fkey_notsunhp	(fkey_default | fkey_legacy | fkey_vt220)
#define fkey_nothpvt	(fkey_default | fkey_sun | fkey_legacy)

struct _vte_keymap_entry {
	enum _vte_cursor_mode cursor_mode;
	enum _vte_keypad_mode keypad_mode;
	enum _vte_fkey_mode fkey_mode;
	GdkModifierType mod_mask;
	const char normal[8];
	gssize normal_length;
	const char special[4];
};

#define X_NULL ""

/* Normal keys unaffected by modes. */
static const struct _vte_keymap_entry _vte_keymap_GDK_space[] = {
	/* Meta+space = ESC+" " */
	{cursor_all, keypad_all, fkey_all,
	 VTE_META_MASK, _VTE_CAP_ESC " ", 2, X_NULL},
	/* Control+space = NUL */
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\0", 1, X_NULL},
	/* Regular space. */
	{cursor_all, keypad_all, fkey_all, 0, " ", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Tab[] = {
	/* Shift+Tab = Back-Tab */
	{cursor_all, keypad_all, fkey_all,
	 GDK_SHIFT_MASK, X_NULL, 0, "kB"},
	{cursor_all, keypad_all, fkey_all,
	 GDK_SHIFT_MASK, _VTE_CAP_CSI "Z", -1, X_NULL},
	/* Alt+Tab = Esc+Tab */
	{cursor_all, keypad_all, fkey_all,
	 VTE_META_MASK, _VTE_CAP_ESC "\t", -1, X_NULL},
	/* Regular tab. */
	{cursor_all, keypad_all, fkey_all,
	 0, X_NULL, 0, "ta"},
	{cursor_all, keypad_all, fkey_all, 0, "\t", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Return[] = {
	{cursor_all, keypad_all, fkey_all,
	 VTE_META_MASK, _VTE_CAP_ESC "\n", 2, X_NULL},
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\n", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, "\r", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Escape[] = {
	{cursor_all, keypad_all, fkey_all,
	 VTE_META_MASK, _VTE_CAP_ESC _VTE_CAP_ESC, 2, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, _VTE_CAP_ESC, 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Insert[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kI"},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "Q", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "2z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_notsunhp, 0, _VTE_CAP_CSI "2~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_ISO_Left_Tab[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kB"},
	{cursor_all, keypad_all, fkey_all, 0, _VTE_CAP_CSI "Z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_slash[] = {
	{cursor_all, keypad_all, fkey_all,
	 VTE_META_MASK, _VTE_CAP_ESC "/", 2, X_NULL},
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\037", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, "/", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_question[] = {
	{cursor_all, keypad_all, fkey_all,
	 VTE_META_MASK, _VTE_CAP_ESC "?", 2, X_NULL},
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\177", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, "?", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

/* Various numeric keys enter control characters. */
static const struct _vte_keymap_entry _vte_keymap_GDK_2[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\0", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};
static const struct _vte_keymap_entry _vte_keymap_GDK_3[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\033", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};
static const struct _vte_keymap_entry _vte_keymap_GDK_4[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\034", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};
static const struct _vte_keymap_entry _vte_keymap_GDK_5[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\035", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};
static const struct _vte_keymap_entry _vte_keymap_GDK_6[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\036", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};
static const struct _vte_keymap_entry _vte_keymap_GDK_7[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\037", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};
static const struct _vte_keymap_entry _vte_keymap_GDK_8[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\177", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};
static const struct _vte_keymap_entry _vte_keymap_GDK_Minus[] = {
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\037", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

/* Home and End are strange cases because their sequences vary wildly from
 * system to system, or mine's just broken.  But anyway. */
static const struct _vte_keymap_entry _vte_keymap_GDK_Home[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kh"},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "1~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "h", -1, X_NULL},
	{cursor_all, keypad_all, fkey_nothpvt, 0, X_NULL, 0, "kh"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_End[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "@7"},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "4~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "@7"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Page_Up[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kP"},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "T", -1, X_NULL},
	{cursor_all, keypad_all, fkey_notsunhp, 0, _VTE_CAP_CSI "5~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "5z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Page_Down[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kN"},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "S", -1, X_NULL},
	{cursor_all, keypad_all, fkey_notsunhp, 0, _VTE_CAP_CSI "6~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "6z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

/* Keys affected by the cursor key mode. */
static const struct _vte_keymap_entry _vte_keymap_GDK_Up[] = {
	{cursor_default, keypad_all, fkey_all, 0, X_NULL, 0, "ku"},
	{cursor_default, keypad_all, fkey_nothp, 0, _VTE_CAP_CSI "A", -1, X_NULL},
	{cursor_default, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "A", -1, X_NULL},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "A", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Down[] = {
	{cursor_default, keypad_all, fkey_all, 0, X_NULL, 0, "kd"},
	{cursor_default, keypad_all, fkey_nothp, 0, _VTE_CAP_CSI "B", -1, X_NULL},
	{cursor_default, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "B", -1, X_NULL},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "B", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Right[] = {
	{cursor_default, keypad_all, fkey_all, 0, X_NULL, 0, "kr"},
	{cursor_default, keypad_all, fkey_nothp, 0, _VTE_CAP_CSI "C", -1, X_NULL},
	{cursor_default, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "C", -1, X_NULL},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "C", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_Left[] = {
	{cursor_default, keypad_all, fkey_all, 0, X_NULL, 0, "kl"},
	{cursor_default, keypad_all, fkey_nothp, 0, _VTE_CAP_CSI "D", -1, X_NULL},
	{cursor_default, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "D", -1, X_NULL},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "D", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

/* Keys (potentially) affected by the keypad key mode. */
static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Space[] = {
	{cursor_all, keypad_default, fkey_all, 0, " ", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, 0, _VTE_CAP_SS3 " ", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Tab[] = {
	{cursor_all, keypad_default, fkey_all, 0, "\t", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, 0, _VTE_CAP_SS3 "I", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Enter[] = {
	{cursor_all, keypad_default, fkey_all, 0, X_NULL, 0, "@8"},
	{cursor_all, keypad_app, fkey_all, VTE_NUMLOCK_MASK | GDK_CONTROL_MASK, "\n", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, VTE_NUMLOCK_MASK, "\r", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, 0, _VTE_CAP_SS3 "M", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, GDK_CONTROL_MASK, "\n", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, "\r", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_F1[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "k1"},
	{cursor_all, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "P", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_F2[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "k2"},
	{cursor_all, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "Q", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_F3[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "k3"},
	{cursor_all, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "R", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_F4[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "k4"},
	{cursor_all, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "S", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Multiply[] = {
	{cursor_all, keypad_default, fkey_all, 0, "*", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, VTE_NUMLOCK_MASK, "*", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, 0, _VTE_CAP_SS3 "j", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Add[] = {
	{cursor_all, keypad_default, fkey_notvt220, 0, "+", 1, X_NULL},
	{cursor_all, keypad_default, fkey_vt220, 0, ",", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, VTE_NUMLOCK_MASK, "+", 1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, VTE_NUMLOCK_MASK, ",", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, 0, _VTE_CAP_SS3 "k", -1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "l", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Separator[] = {
	{cursor_all, keypad_default, fkey_all, 0, ",", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, 0, _VTE_CAP_SS3 "l", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Subtract[] = {
	{cursor_all, keypad_default, fkey_all, 0, "-", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, VTE_NUMLOCK_MASK, "-", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, 0, _VTE_CAP_SS3 "m", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Decimal_Delete[] = {
	{cursor_all, keypad_default, fkey_default, 0, ".", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notsun, 0, _VTE_CAP_SS3 "3~", -1, X_NULL},
	{cursor_all, keypad_app, fkey_sun, 0, _VTE_CAP_SS3 "3~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Divide[] = {
	{cursor_all, keypad_default, fkey_all, 0, "/", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, VTE_NUMLOCK_MASK, "/", 1, X_NULL},
	{cursor_all, keypad_app, fkey_all, 0, _VTE_CAP_SS3 "o", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

/* GDK already separates keypad "0" from keypad "Insert", so the only time
 * we'll see this key is when NumLock is on, and that means that we're in
 * "default" mode. */
static const struct _vte_keymap_entry _vte_keymap_GDK_KP_0[] = {
	{cursor_all, keypad_all, fkey_all, 0, "0", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_1[] = {
	{cursor_all, keypad_all, fkey_all, 0, "1", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_2[] = {
	{cursor_all, keypad_all, fkey_all, 0, "2", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_3[] = {
	{cursor_all, keypad_all, fkey_all, 0, "3", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_4[] = {
	{cursor_all, keypad_all, fkey_all, 0, "4", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_5[] = {
	{cursor_all, keypad_all, fkey_all, 0, "5", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_6[] = {
	{cursor_all, keypad_all, fkey_all, 0, "6", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_7[] = {
	{cursor_all, keypad_all, fkey_all, 0, "7", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_8[] = {
	{cursor_all, keypad_all, fkey_all, 0, "8", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_9[] = {
	{cursor_all, keypad_all, fkey_all, 0, "9", 1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

/* These are the same keys as above, but without numlock.  If there's a
 * capability associated with the key, then we send that, unless we're in
 * application mode. */
static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Insert[] = {
	{cursor_all, keypad_default, fkey_notsunvt,
	 0, _VTE_CAP_CSI "2~", -1, X_NULL},
	{cursor_all, keypad_default, fkey_sun, 0, _VTE_CAP_CSI "2z", -1, X_NULL},
	{cursor_all, keypad_default, fkey_vt220, 0, "0", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, 0, _VTE_CAP_CSI "2~", -1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "p", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_End[] = {
	{cursor_all, keypad_default, fkey_all, 0, X_NULL, 0, "K4"},
	{cursor_all, keypad_default, fkey_notvt220,
	 0, _VTE_CAP_CSI "4~", -1, X_NULL},
	{cursor_all, keypad_default, fkey_vt220, 0, "1", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, 0, _VTE_CAP_CSI "4~", -1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "q", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Down[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kd"},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "B", -1, X_NULL},
	{cursor_default, keypad_all, fkey_notvt220,
	 0, _VTE_CAP_CSI "B", -1, X_NULL},
	{cursor_default, keypad_default, fkey_vt220, 0, "2", 1, X_NULL},
	{cursor_default, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "r", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Page_Down[] = {
	{cursor_all, keypad_default, fkey_all, 0, X_NULL, 0, "K5"},
	{cursor_all, keypad_default, fkey_notsunvt,
	 0, _VTE_CAP_CSI "6~", -1, X_NULL},
	{cursor_all, keypad_default, fkey_sun, 0, _VTE_CAP_CSI "6z", -1, X_NULL},
	{cursor_all, keypad_default, fkey_vt220, 0, "3", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, 0, _VTE_CAP_CSI "6~", -1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "s", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Left[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kl"},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "D", -1, X_NULL},
	{cursor_default, keypad_all, fkey_notvt220,
	 0, _VTE_CAP_CSI "D", -1, X_NULL},
	{cursor_default, keypad_default, fkey_vt220, 0, "4", 1, X_NULL},
	{cursor_default, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "t", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Begin[] = {
	{cursor_all, keypad_default, fkey_all, 0, X_NULL, 0, "K2"},
	{cursor_all, keypad_default, fkey_notvt220,
	 0, _VTE_CAP_CSI "E", -1, X_NULL},
	{cursor_all, keypad_default, fkey_vt220, 0, "5", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, 0, _VTE_CAP_CSI "E", -1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "u", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Right[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "kr"},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "C", -1, X_NULL},
	{cursor_default, keypad_all, fkey_notvt220,
	 0, _VTE_CAP_CSI "C", -1, X_NULL},
	{cursor_default, keypad_default, fkey_vt220, 0, "6", 1, X_NULL},
	{cursor_default, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "v", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Home[] = {
	{cursor_all, keypad_default, fkey_all, 0, X_NULL, 0, "K1"},
	{cursor_all, keypad_default, fkey_notvt220,
	 0, _VTE_CAP_CSI "1~", -1, X_NULL},
	{cursor_all, keypad_default, fkey_vt220, 0, "7", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, 0, _VTE_CAP_CSI "1~", -1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "w", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Up[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "ku"},
	{cursor_app, keypad_all, fkey_all, 0, _VTE_CAP_SS3 "A", -1, X_NULL},
	{cursor_default, keypad_all, fkey_notvt220,
	 0, _VTE_CAP_CSI "A", -1, X_NULL},
	{cursor_default, keypad_default, fkey_vt220, 0, "8", 1, X_NULL},
	{cursor_default, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "x", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_KP_Page_Up[] = {
	{cursor_all, keypad_default, fkey_all, 0, X_NULL, 0, "K3"},
	{cursor_all, keypad_default, fkey_notvt220,
	 0, _VTE_CAP_CSI "5~", -1, X_NULL},
	{cursor_all, keypad_default, fkey_vt220, 0, "9", 1, X_NULL},
	{cursor_all, keypad_app, fkey_notvt220, 0, _VTE_CAP_CSI "5~", -1, X_NULL},
	{cursor_all, keypad_app, fkey_vt220, 0, _VTE_CAP_SS3 "y", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};


/* Keys affected by the function key mode. */
static const struct _vte_keymap_entry _vte_keymap_GDK_F1[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k1"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "F3"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k1"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_SS3 "P", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "224z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "p", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "11~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "23~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_SS3 "P", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F2[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k2"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "F4"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k2"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_SS3 "Q", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "225z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "q", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "12~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "24~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_SS3 "Q", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F3[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k3"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "F5"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k3"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_SS3 "R", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "226z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "r", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "13~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "25~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_SS3 "R", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F4[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k4"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "F6"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k4"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_SS3 "S", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "227z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "s", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "14~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "26~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_SS3 "S", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F5[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k5"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "F7"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k5"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "15~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "228z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "t", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "15~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "28~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "15~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F6[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k6"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "F8"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k6"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "17~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "229z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "u", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "17~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "29~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "17~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F7[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k7"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "F9"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k7"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "18~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "230z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "v", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "18~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "31~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "18~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F8[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k8"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "FA"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k8"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "19~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "231z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_ESC "w", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "19~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "32~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "19~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F9[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k9"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "FB"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k9"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "20~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "232z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_CSI "20~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "20~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "33~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "20~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F10[] = {
	{cursor_all, keypad_all, fkey_notvt220, 0, X_NULL, 0, "k;"},
	{cursor_all, keypad_all, fkey_vt220, GDK_CONTROL_MASK, X_NULL, 0, "FC"},
	{cursor_all, keypad_all, fkey_vt220, 0, X_NULL, 0, "k;"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "21~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "233z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_CSI "21~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "21~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220,
	 GDK_CONTROL_MASK, _VTE_CAP_CSI "34~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "21~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F11[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F1"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "23~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "192z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_CSI "23~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "23~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "23~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F12[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F2"},
	{cursor_all, keypad_all, fkey_default, 0, _VTE_CAP_CSI "24~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "193z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_hp, 0, _VTE_CAP_CSI "24~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_legacy, 0, _VTE_CAP_CSI "24~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_vt220, 0, _VTE_CAP_CSI "24~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F13[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F3"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "25~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "194z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F14[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F4"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "26~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "195z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F15[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F5"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "28~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "196z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F16[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F6"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "29~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "197z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F17[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F7"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "31~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "198z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F18[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F8"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "32~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "199z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F19[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "F9"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "33~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "200z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F20[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FA"},
	{cursor_all, keypad_all, fkey_notsun, 0, _VTE_CAP_CSI "34~", -1, X_NULL},
	{cursor_all, keypad_all, fkey_sun, 0, _VTE_CAP_CSI "201z", -1, X_NULL},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F21[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FB"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F22[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FC"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F23[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FD"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F24[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FE"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F25[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FF"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F26[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FG"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F27[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FH"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F28[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FI"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F29[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FJ"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F30[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FK"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F31[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FL"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F32[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FM"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F33[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FN"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F34[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FO"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_entry _vte_keymap_GDK_F35[] = {
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, "FP"},
	{cursor_all, keypad_all, fkey_all, 0, X_NULL, 0, X_NULL},
};

static const struct _vte_keymap_group {
	guint keyval;
	const struct _vte_keymap_entry *entries;
} _vte_keymap[] = {
	{GDK_KEY (space),		_vte_keymap_GDK_space},
	{GDK_KEY (Return),		_vte_keymap_GDK_Return},
	{GDK_KEY (Escape),		_vte_keymap_GDK_Escape},
	{GDK_KEY (Tab),		        _vte_keymap_GDK_Tab},
	{GDK_KEY (ISO_Left_Tab),	_vte_keymap_GDK_ISO_Left_Tab},
	{GDK_KEY (Home),		_vte_keymap_GDK_Home},
	{GDK_KEY (End),		        _vte_keymap_GDK_End},
	{GDK_KEY (Insert),		_vte_keymap_GDK_Insert},
	{GDK_KEY (slash),		_vte_keymap_GDK_slash},
	{GDK_KEY (question),		_vte_keymap_GDK_question},
	/* GDK_KEY (Delete is all handled in code), due to funkiness. */
	{GDK_KEY (Page_Up),		_vte_keymap_GDK_Page_Up},
	{GDK_KEY (Page_Down),		_vte_keymap_GDK_Page_Down},

	{GDK_KEY (2),			_vte_keymap_GDK_2},
	{GDK_KEY (3),			_vte_keymap_GDK_3},
	{GDK_KEY (4),			_vte_keymap_GDK_4},
	{GDK_KEY (5),			_vte_keymap_GDK_5},
	{GDK_KEY (6),			_vte_keymap_GDK_6},
	{GDK_KEY (7),			_vte_keymap_GDK_7},
	{GDK_KEY (8),			_vte_keymap_GDK_8},
	{GDK_KEY (minus),		_vte_keymap_GDK_Minus},

	{GDK_KEY (Up),		_vte_keymap_GDK_Up},
	{GDK_KEY (Down),		_vte_keymap_GDK_Down},
	{GDK_KEY (Right),		_vte_keymap_GDK_Right},
	{GDK_KEY (Left),		_vte_keymap_GDK_Left},

	{GDK_KEY (KP_Space),		_vte_keymap_GDK_KP_Space},
	{GDK_KEY (KP_Tab),		_vte_keymap_GDK_KP_Tab},
	{GDK_KEY (KP_Enter),		_vte_keymap_GDK_KP_Enter},
	{GDK_KEY (KP_F1),		_vte_keymap_GDK_KP_F1},
	{GDK_KEY (KP_F2),		_vte_keymap_GDK_KP_F2},
	{GDK_KEY (KP_F3),		_vte_keymap_GDK_KP_F3},
	{GDK_KEY (KP_F4),		_vte_keymap_GDK_KP_F4},
	{GDK_KEY (KP_Multiply),	_vte_keymap_GDK_KP_Multiply},
	{GDK_KEY (KP_Add),		_vte_keymap_GDK_KP_Add},
	{GDK_KEY (KP_Separator),	_vte_keymap_GDK_KP_Separator},
	{GDK_KEY (KP_Subtract),	_vte_keymap_GDK_KP_Subtract},
	{GDK_KEY (KP_Decimal),	_vte_keymap_GDK_KP_Decimal_Delete},
	{GDK_KEY (KP_Delete),		_vte_keymap_GDK_KP_Decimal_Delete},
	{GDK_KEY (KP_Divide),		_vte_keymap_GDK_KP_Divide},
	{GDK_KEY (KP_0),		_vte_keymap_GDK_KP_0},
	{GDK_KEY (KP_Insert),		_vte_keymap_GDK_KP_Insert},
	{GDK_KEY (KP_1),		_vte_keymap_GDK_KP_1},
	{GDK_KEY (KP_End),		_vte_keymap_GDK_KP_End},
	{GDK_KEY (KP_2),		_vte_keymap_GDK_KP_2},
	{GDK_KEY (KP_Down),		_vte_keymap_GDK_KP_Down},
	{GDK_KEY (KP_3),		_vte_keymap_GDK_KP_3},
	{GDK_KEY (KP_Page_Down),	_vte_keymap_GDK_KP_Page_Down},
	{GDK_KEY (KP_4),		_vte_keymap_GDK_KP_4},
	{GDK_KEY (KP_Left),		_vte_keymap_GDK_KP_Left},
	{GDK_KEY (KP_5),		_vte_keymap_GDK_KP_5},
	{GDK_KEY (KP_Begin),		_vte_keymap_GDK_KP_Begin},
	{GDK_KEY (KP_6),		_vte_keymap_GDK_KP_6},
	{GDK_KEY (KP_Right),		_vte_keymap_GDK_KP_Right},
	{GDK_KEY (KP_7),		_vte_keymap_GDK_KP_7},
	{GDK_KEY (KP_Home),		_vte_keymap_GDK_KP_Home},
	{GDK_KEY (KP_8),		_vte_keymap_GDK_KP_8},
	{GDK_KEY (KP_Up),		_vte_keymap_GDK_KP_Up},
	{GDK_KEY (KP_9),		_vte_keymap_GDK_KP_9},
	{GDK_KEY (KP_Page_Up),	_vte_keymap_GDK_KP_Page_Up},

	{GDK_KEY (F1),		_vte_keymap_GDK_F1},
	{GDK_KEY (F2),		_vte_keymap_GDK_F2},
	{GDK_KEY (F3),		_vte_keymap_GDK_F3},
	{GDK_KEY (F4),		_vte_keymap_GDK_F4},
	{GDK_KEY (F5),		_vte_keymap_GDK_F5},
	{GDK_KEY (F6),		_vte_keymap_GDK_F6},
	{GDK_KEY (F7),		_vte_keymap_GDK_F7},
	{GDK_KEY (F8),		_vte_keymap_GDK_F8},
	{GDK_KEY (F9),		_vte_keymap_GDK_F9},
	{GDK_KEY (F10),		_vte_keymap_GDK_F10},
	{GDK_KEY (F11),		_vte_keymap_GDK_F11},
	{GDK_KEY (F12),		_vte_keymap_GDK_F12},
	{GDK_KEY (F13),		_vte_keymap_GDK_F13},
	{GDK_KEY (F14),		_vte_keymap_GDK_F14},
	{GDK_KEY (F15),		_vte_keymap_GDK_F15},
	{GDK_KEY (F16),		_vte_keymap_GDK_F16},
	{GDK_KEY (F17),		_vte_keymap_GDK_F17},
	{GDK_KEY (F18),		_vte_keymap_GDK_F18},
	{GDK_KEY (F19),		_vte_keymap_GDK_F19},
	{GDK_KEY (F20),		_vte_keymap_GDK_F20},
	{GDK_KEY (F21),		_vte_keymap_GDK_F21},
	{GDK_KEY (F22),		_vte_keymap_GDK_F22},
	{GDK_KEY (F23),		_vte_keymap_GDK_F23},
	{GDK_KEY (F24),		_vte_keymap_GDK_F24},
	{GDK_KEY (F25),		_vte_keymap_GDK_F25},
	{GDK_KEY (F26),		_vte_keymap_GDK_F26},
	{GDK_KEY (F27),		_vte_keymap_GDK_F27},
	{GDK_KEY (F28),		_vte_keymap_GDK_F28},
	{GDK_KEY (F29),		_vte_keymap_GDK_F29},
	{GDK_KEY (F30),		_vte_keymap_GDK_F30},
	{GDK_KEY (F31),		_vte_keymap_GDK_F31},
	{GDK_KEY (F32),		_vte_keymap_GDK_F32},
	{GDK_KEY (F33),		_vte_keymap_GDK_F33},
	{GDK_KEY (F34),		_vte_keymap_GDK_F34},
	{GDK_KEY (F35),		_vte_keymap_GDK_F35},
};

/* Map the specified keyval/modifier setup, dependent on the mode, to either
 * a literal string or a capability name. */
void _vte_keymap_map(guint keyval, GdkModifierType modifiers, gboolean sun_mode, gboolean hp_mode, gboolean legacy_mode, gboolean vt220_mode, gboolean app_cursor_keys, gboolean app_keypad_keys, struct _vte_termcap *termcap, const char *terminal, char **normal, gssize *normal_length, const char **special) {
	gsize i;
	const struct _vte_keymap_entry *entries;
	enum _vte_cursor_mode cursor_mode;
	enum _vte_keypad_mode keypad_mode;
	enum _vte_fkey_mode fkey_mode;
	char *cap, *tmp;
	const char *termcap_special = NULL;
	char ncurses_buffer[4096];
	char ncurses_area[512];

	g_return_if_fail(normal != NULL);
	g_return_if_fail(normal_length != NULL);
	g_return_if_fail(special != NULL);

	_VTE_DEBUG_IF(VTE_DEBUG_KEYBOARD) 
		_vte_keysym_print(keyval, modifiers,
				sun_mode,
				hp_mode,
				legacy_mode,
				vt220_mode);

	/* Start from scratch. */
	*normal = NULL;
	*special = NULL;
	*normal_length = 0;

	/* Search for the list for this key. */
	entries = NULL;
	for (i = 0; i < G_N_ELEMENTS(_vte_keymap); i++) {
#ifdef VTE_DEBUG
		int j;
		GdkModifierType mods;
		/* Check for NULL strings with non-zero length, and
		 * vice-versa. */
		entries = _vte_keymap[i].entries;
		for (j = 0; entries[j].normal_length || entries[j].special[0]; j++) {
			if (entries[j].normal_length) {
				g_assert(!entries[j].special[0]);
			} else {
				g_assert(!entries[j].normal[0]);
			}
		}
		/* Check for coverage. This check is not exhaustive. */
		fkey_mode = 0;
		mods = GDK_SHIFT_MASK | GDK_CONTROL_MASK | VTE_META_MASK | VTE_NUMLOCK_MASK;
		for (j = 0; entries[j].normal_length || entries[j].special[0]; j++) {
			if (entries[j].fkey_mode != fkey_all) {
				fkey_mode |= entries[j].fkey_mode;
			}
			mods &= entries[j].mod_mask;
		}
		switch (_vte_keymap[i].keyval) {
		case GDK_KEY (2):
		case GDK_KEY (3):
		case GDK_KEY (4):
		case GDK_KEY (5):
		case GDK_KEY (6):
		case GDK_KEY (7):
		case GDK_KEY (8):
			/* Known non-full-coverage cases. */
			break;
		default:
			/* Everything else we double-check. */
			g_assert((fkey_mode == 0) || (fkey_mode == fkey_all));
			break;
		}
		entries = NULL;
#endif
		if (_vte_keymap[i].keyval == keyval) {
			/* Found it! */
			entries = _vte_keymap[i].entries;
			break;
		}
	}
	if (entries == NULL) {
		_vte_debug_print(VTE_DEBUG_KEYBOARD,
				" (ignoring, no map for key).\n");
		return;
	}

	/* Build mode masks. */
	cursor_mode = app_cursor_keys ? cursor_app : cursor_default;
	keypad_mode = app_keypad_keys ? keypad_app : keypad_default;
	if (sun_mode) {
		fkey_mode = fkey_sun;
	} else
	if (hp_mode) {
		fkey_mode = fkey_hp;
	} else
	if (legacy_mode) {
		fkey_mode = fkey_legacy;
	} else
	if (vt220_mode) {
		fkey_mode = fkey_vt220;
	} else {
		fkey_mode = fkey_default;
	}
	modifiers &= (GDK_SHIFT_MASK | GDK_CONTROL_MASK | VTE_META_MASK | VTE_NUMLOCK_MASK);

	/* Search for the conditions. */
	for (i = 0; entries[i].normal_length || entries[i].special[0]; i++)
	if ((entries[i].cursor_mode & cursor_mode) &&
	    (entries[i].keypad_mode & keypad_mode) &&
	    (entries[i].fkey_mode & fkey_mode))
	if ((modifiers & entries[i].mod_mask) == entries[i].mod_mask) {
		if (entries[i].normal_length) {
			if (entries[i].normal_length != -1) {
				*normal_length = entries[i].normal_length;
				*normal = g_memdup(entries[i].normal,
						   entries[i].normal_length);
			} else {
				*normal_length = strlen(entries[i].normal);
				*normal = g_strdup(entries[i].normal);
			}
			_vte_keymap_key_add_key_modifiers(keyval,
							  modifiers,
							  sun_mode,
							  hp_mode,
							  legacy_mode,
							  vt220_mode,
							  cursor_mode & cursor_app,
							  normal,
							  normal_length);
			_VTE_DEBUG_IF(VTE_DEBUG_KEYBOARD) {
				int j;
				g_printerr(" to '");
				for (j = 0; j < *normal_length; j++) {
					if (((*normal)[j] < 32) ||
					    ((*normal)[j] >= 127)) {
						g_printerr("<0x%02x>",
							(*normal)[j]);
					} else {
						g_printerr("%c",
							(*normal)[j]);
					}
				}
				g_printerr("'.\n");
			}
			return;
		} else {
			termcap_special = entries[i].special;
			cap = _vte_termcap_find_string(termcap,
						       terminal,
						       entries[i].special);
			if (cap != NULL) {
				*special = NULL;
				if (strlen(cap) > 0) {
					/* Save the special string. */
					*special = entries[i].special;
					_vte_debug_print(VTE_DEBUG_KEYBOARD,
							" to \"%s\"", *special);
				}
				g_free(cap);
				if (*special != NULL) {
					/* Return the special string. */
					_vte_debug_print(VTE_DEBUG_KEYBOARD,
							", returning.\n");
					return;
				}
			}
		}
	}
	if (termcap_special != NULL) {
		tmp = g_strdup(terminal);
		cap = NULL;
		if (tgetent(ncurses_buffer, tmp) == 1) {
			cap = ncurses_area;
			tmp = g_strdup(termcap_special);
			cap = tgetstr(tmp, &cap);
		}
		if ((cap == NULL) && (strstr(terminal, "xterm") != NULL)) {
			/* try, try again */
			if (tgetent(ncurses_buffer, "xterm-xfree86") == 1) {
				cap = ncurses_area;
				tmp = g_strdup(termcap_special);
				cap = tgetstr(tmp, &cap);
			}
		}
		g_free(tmp);
		if ((cap != NULL) && (*cap != '\0')) {
			*normal_length = strlen(cap);
			*normal = g_strdup(cap);
#ifdef VTE_DEBUG
			if (_vte_debug_on(VTE_DEBUG_KEYBOARD)) {
				int j;
				g_printerr(" via " VTE_TERMCAP_NAME " to '");
				for (j = 0; j < *normal_length; j++) {
					if (((*normal)[j] < 32) ||
					    ((*normal)[j] >= 127)) {
						g_printerr("<0x%02x>",
							(*normal)[j]);
					} else {
						g_printerr("%c",
							(*normal)[j]);
					}
				}
				g_printerr("', returning.\n");
			}
#endif
			return;
		}
	}

	_vte_debug_print(VTE_DEBUG_KEYBOARD,
			" (ignoring, no match for modifier state).\n");
}

gboolean
_vte_keymap_key_is_modifier(guint keyval)
{
	gboolean modifier = FALSE;
	/* Determine if this is just a modifier key. */
	switch (keyval) {
	case GDK_KEY (Alt_L):
	case GDK_KEY (Alt_R):
	case GDK_KEY (Caps_Lock):
	case GDK_KEY (Control_L):
	case GDK_KEY (Control_R):
	case GDK_KEY (Eisu_Shift):
	case GDK_KEY (Hyper_L):
	case GDK_KEY (Hyper_R):
	case GDK_KEY (ISO_First_Group_Lock):
	case GDK_KEY (ISO_Group_Lock):
	case GDK_KEY (ISO_Group_Shift):
	case GDK_KEY (ISO_Last_Group_Lock):
	case GDK_KEY (ISO_Level3_Lock):
	case GDK_KEY (ISO_Level3_Shift):
	case GDK_KEY (ISO_Lock):
	case GDK_KEY (ISO_Next_Group_Lock):
	case GDK_KEY (ISO_Prev_Group_Lock):
	case GDK_KEY (Kana_Lock):
	case GDK_KEY (Kana_Shift):
	case GDK_KEY (Meta_L):
	case GDK_KEY (Meta_R):
	case GDK_KEY (Num_Lock):
	case GDK_KEY (Scroll_Lock):
	case GDK_KEY (Shift_L):
	case GDK_KEY (Shift_Lock):
	case GDK_KEY (Shift_R):
	case GDK_KEY (Super_L):
	case GDK_KEY (Super_R):
		modifier = TRUE;
		break;
	default:
		modifier = FALSE;
		break;
	}
	return modifier;
}

static gboolean
_vte_keymap_key_gets_modifiers(guint keyval)
{
	gboolean fkey = FALSE;
	/* Determine if this key gets modifiers. */
	switch (keyval) {
	case GDK_KEY (Up):
	case GDK_KEY (Down):
	case GDK_KEY (Left):
	case GDK_KEY (Right):
	case GDK_KEY (Insert):
	case GDK_KEY (Delete):
	case GDK_KEY (Page_Up):
	case GDK_KEY (Page_Down):
	case GDK_KEY (KP_Up):
	case GDK_KEY (KP_Down):
	case GDK_KEY (KP_Left):
	case GDK_KEY (KP_Right):
	case GDK_KEY (KP_Insert):
	case GDK_KEY (KP_Delete):
	case GDK_KEY (KP_Page_Up):
	case GDK_KEY (KP_Page_Down):
	case GDK_KEY (F1):
	case GDK_KEY (F2):
	case GDK_KEY (F3):
	case GDK_KEY (F4):
	case GDK_KEY (F5):
	case GDK_KEY (F6):
	case GDK_KEY (F7):
	case GDK_KEY (F8):
	case GDK_KEY (F9):
	case GDK_KEY (F10):
	case GDK_KEY (F11):
	case GDK_KEY (F12):
	case GDK_KEY (F13):
	case GDK_KEY (F14):
	case GDK_KEY (F15):
	case GDK_KEY (F16):
	case GDK_KEY (F17):
	case GDK_KEY (F18):
	case GDK_KEY (F19):
	case GDK_KEY (F20):
	case GDK_KEY (F21):
	case GDK_KEY (F22):
	case GDK_KEY (F23):
	case GDK_KEY (F24):
	case GDK_KEY (F25):
	case GDK_KEY (F26):
	case GDK_KEY (F27):
	case GDK_KEY (F28):
	case GDK_KEY (F29):
	case GDK_KEY (F30):
	case GDK_KEY (F31):
	case GDK_KEY (F32):
	case GDK_KEY (F33):
	case GDK_KEY (F34):
	case GDK_KEY (F35):
		fkey = TRUE;
		break;
	default:
		fkey = FALSE;
		break;
	}
	return fkey;
}

/* Prior and Next are ommitted for the SS3 to CSI switch below */
static gboolean
is_cursor_key(guint keyval)
{
	switch (keyval) {
	case GDK_KEY (Home):
	case GDK_KEY (Left):
	case GDK_KEY (Up):
	case GDK_KEY (Right):
	case GDK_KEY (Down):
	case GDK_KEY (End):

	case GDK_KEY (KP_Home):
	case GDK_KEY (KP_Left):
	case GDK_KEY (KP_Up):
	case GDK_KEY (KP_Right):
	case GDK_KEY (KP_Down):
	case GDK_KEY (KP_End):
		return TRUE;
	default:
		return FALSE;
	}
}


void
_vte_keymap_key_add_key_modifiers(guint keyval,
				  GdkModifierType modifiers,
				  gboolean sun_mode,
				  gboolean hp_mode,
				  gboolean legacy_mode,
				  gboolean vt220_mode,
				  gboolean cursor_app_mode,
				  char **normal,
				  gssize *normal_length)
{
	int modifier, offset;
	char *nnormal;
	GdkModifierType significant_modifiers;

	significant_modifiers = GDK_SHIFT_MASK |
				GDK_CONTROL_MASK |
				VTE_META_MASK;

	if (!_vte_keymap_key_gets_modifiers(keyval)) {
		return;
	}
	if (sun_mode || hp_mode || vt220_mode) {
		/* no modifiers for you! */
		return;
	}

	switch (modifiers & significant_modifiers) {
	case 0:
		modifier = 0;
		break;
	case GDK_SHIFT_MASK:
		modifier = 2;
		break;
	case VTE_META_MASK:
		modifier = 3;
		break;
	case GDK_SHIFT_MASK | VTE_META_MASK:
		modifier = 4;
		break;
	case GDK_CONTROL_MASK:
		modifier = 5;
		break;
	case GDK_SHIFT_MASK | GDK_CONTROL_MASK:
		modifier = 6;
		break;
	case VTE_META_MASK | GDK_CONTROL_MASK:
		modifier = 7;
		break;
	case GDK_SHIFT_MASK | VTE_META_MASK | GDK_CONTROL_MASK:
		modifier = 8;
		break;
	default:
		modifier = 8;
		break;
	}

	if (modifier == 0) {
		return;
	}

	nnormal = g_malloc0(*normal_length + 4);
	memcpy(nnormal, *normal, *normal_length);
	if (strlen(nnormal) > 1) {
		/* SS3 should have no modifiers so make it CSI instead. See
		 * http://cvsweb.xfree86.org/cvsweb/xc/programs/xterm/input.c.diff?r1=3.57&r2=3.58
		 */
		if (cursor_app_mode &&
			g_str_has_prefix(nnormal, _VTE_CAP_SS3)
			&& is_cursor_key(keyval)) {
			nnormal[1] = '[';
		}

		/* Get the offset of the last character. */
		offset = strlen(nnormal) - 1;
		if (g_ascii_isdigit(nnormal[offset - 1])) {
			/* Stuff a semicolon and the modifier in right before
			 * that last character. */
			nnormal[offset + 2] = nnormal[offset];
			nnormal[offset + 1] = modifier + '0';
			nnormal[offset + 0] = ';';
			*normal_length += 2;
		} else {
#if 1
			/* Stuff a "1", a semicolon and the modifier in right
			 * before that last character, matching Xterm. */
			nnormal[offset + 3] = nnormal[offset];
			nnormal[offset + 2] = modifier + '0';
			nnormal[offset + 1] = ';';
			nnormal[offset + 0] = '1';
			*normal_length += 3;
#else
			/* Stuff the modifier in right before that last
			 * character, matching what people expect. */
			nnormal[offset + 1] = nnormal[offset];
			nnormal[offset + 0] = modifier + '0';
			*normal_length += 1;
#endif
		}
		g_free(*normal);
		*normal = nnormal;
	} else {
		g_free(nnormal);
	}
}

/* Create snapshot private data. */
static VteTerminalAccessiblePrivate *
vte_terminal_accessible_new_private_data(void)
{
	VteTerminalAccessiblePrivate *priv;
	priv = g_slice_new0(VteTerminalAccessiblePrivate);
	priv->snapshot_text = NULL;
	priv->snapshot_characters = NULL;
	priv->snapshot_attributes = NULL;
	priv->snapshot_linebreaks = NULL;
	priv->snapshot_caret = -1;
	priv->snapshot_contents_invalid = TRUE;
	priv->snapshot_caret_invalid = TRUE;
	return priv;
}

/* Free snapshot private data. */
static void
vte_terminal_accessible_free_private_data(VteTerminalAccessiblePrivate *priv)
{
	gint i;

	g_assert(priv != NULL);
	if (priv->snapshot_text != NULL) {
		g_string_free(priv->snapshot_text, TRUE);
	}
	if (priv->snapshot_characters != NULL) {
		g_array_free(priv->snapshot_characters, TRUE);
	}
	if (priv->snapshot_attributes != NULL) {
		g_array_free(priv->snapshot_attributes, TRUE);
	}
	if (priv->snapshot_linebreaks != NULL) {
		g_array_free(priv->snapshot_linebreaks, TRUE);
	}
	for (i = 0; i < LAST_ACTION; i++) {
		g_free (priv->action_descriptions[i]);
	}
	g_slice_free(VteTerminalAccessiblePrivate, priv);
}

static gint
offset_from_xy (VteTerminalAccessiblePrivate *priv,
		gint x, gint y)
{
	gint offset;
	gint linebreak;
	gint next_linebreak;

	if (y >= (gint) priv->snapshot_linebreaks->len)
		y = priv->snapshot_linebreaks->len -1;

	linebreak = g_array_index (priv->snapshot_linebreaks, int, y);
	if (y + 1 == (gint) priv->snapshot_linebreaks->len)
		next_linebreak = priv->snapshot_characters->len;
	else
		next_linebreak = g_array_index (priv->snapshot_linebreaks, int, y + 1);

	offset = linebreak + x;
	if (offset >= next_linebreak)
		offset = next_linebreak -1;
	return offset;
}

static void
xy_from_offset (VteTerminalAccessiblePrivate *priv,
		guint offset, gint *x, gint *y)
{
	guint i, linebreak;
	gint cur_x, cur_y;
	gint cur_offset = 0;

	cur_x = -1;
	cur_y = -1;
	for (i = 0; i < priv->snapshot_linebreaks->len; i++) {
		linebreak = g_array_index (priv->snapshot_linebreaks, int, i);
		if (offset < linebreak) {
			cur_x = offset - cur_offset;
			cur_y = i - 1;
			break;

		}  else {
			cur_offset = linebreak;
		}
	}
	if (i == priv->snapshot_linebreaks->len) {
		if (offset <= priv->snapshot_characters->len) {
			cur_x = offset - cur_offset;
			cur_y = i - 1;
		}
	}
	*x = cur_x;
	*y = cur_y;
}

/* "Oh yeah, that's selected.  Sure." callback. */
static gboolean
all_selected(VteTerminal *terminal, glong column, glong row, gpointer data)
{
	return TRUE;
}

static void
emit_text_caret_moved(GObject *object, glong caret)
{
	_vte_debug_print(VTE_DEBUG_SIGNALS|VTE_DEBUG_ALLY,
			"Accessibility peer emitting "
			"`text-caret-moved'.\n");
	g_signal_emit_by_name(object, "text-caret-moved", caret);
}

static void
emit_text_changed_insert(GObject *object,
			 const char *text, glong offset, glong len)
{
	glong start, count;
	if (len == 0) {
		return;
	}
	/* Convert the byte offsets to character offsets. */
	start = g_utf8_pointer_to_offset (text, text + offset);
	count = g_utf8_pointer_to_offset (text + offset, text + offset + len);
	_vte_debug_print(VTE_DEBUG_SIGNALS|VTE_DEBUG_ALLY,
			"Accessibility peer emitting "
			"`text-changed::insert' (%ld, %ld) (%ld, %ld).\n"
			"Inserted text was `%.*s'.\n",
			offset, len, start, count,
			(int) len, text + offset);
	g_signal_emit_by_name(object, "text-changed::insert", start, count);
}

static void
emit_text_changed_delete(GObject *object,
			 const char *text, glong offset, glong len)
{
	glong start, count;
	if (len == 0) {
		return;
	}
	/* Convert the byte offsets to characters. */
	start = g_utf8_pointer_to_offset (text, text + offset);
	count = g_utf8_pointer_to_offset (text + offset, text + offset + len);
	_vte_debug_print(VTE_DEBUG_SIGNALS|VTE_DEBUG_ALLY,
			"Accessibility peer emitting "
			"`text-changed::delete' (%ld, %ld) (%ld, %ld).\n"
			"Deleted text was `%.*s'.\n",
			offset, len, start, count,
			(int) len, text + offset);
	g_signal_emit_by_name(object, "text-changed::delete", start, count);
}

static void
vte_terminal_accessible_update_private_data_if_needed(AtkObject *text,
						      char **old, glong *olen)
{
	VteTerminal *terminal;
	VteTerminalAccessiblePrivate *priv;
	struct _VteCharAttributes attrs;
	char *next, *tmp;
	long row, offset, caret;
	long ccol, crow;
	guint i;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));

	/* Retrieve the private data structure.  It must already exist. */
	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	g_assert(priv != NULL);

	/* If nothing's changed, just return immediately. */
	if ((priv->snapshot_contents_invalid == FALSE) &&
	    (priv->snapshot_caret_invalid == FALSE)) {
		if (old) {
			if (priv->snapshot_text) {
				*old = g_malloc(priv->snapshot_text->len + 1);
				memcpy(*old,
				       priv->snapshot_text->str,
				       priv->snapshot_text->len);
				(*old)[priv->snapshot_text->len] = '\0';
				if (olen) {
					*olen = priv->snapshot_text->len;
				}
			} else {
				*old = g_strdup("");
				if (olen) {
					*olen = 0;
				}
			}
		} else {
			if (olen) {
				g_assert_not_reached();
			}
		}
		return;
	}

	/* Re-read the contents of the widget if the contents have changed. */
	terminal = VTE_TERMINAL(gtk_accessible_get_widget(GTK_ACCESSIBLE(text)));
	if (priv->snapshot_contents_invalid) {
		/* Free the outdated snapshot data, unless the caller
		 * wants it. */
		if (old) {
			if (priv->snapshot_text != NULL) {
				*old = priv->snapshot_text->str;
				if (olen) {
					*olen = priv->snapshot_text->len;
				}
				g_string_free(priv->snapshot_text, FALSE);
			} else {
				*old = g_strdup("");
				if (olen) {
					*olen = 0;
				}
			}
		} else {
			if (olen) {
				g_assert_not_reached();
			}
			if (priv->snapshot_text != NULL) {
				g_string_free(priv->snapshot_text, TRUE);
			}
		}
		priv->snapshot_text = NULL;

		/* Free the character offsets and allocate a new array to hold
		 * them. */
		if (priv->snapshot_characters != NULL) {
			g_array_free(priv->snapshot_characters, TRUE);
		}
		priv->snapshot_characters = g_array_new(FALSE, FALSE, sizeof(int));

		/* Free the attribute lists and allocate a new array to hold
		 * them. */
		if (priv->snapshot_attributes != NULL) {
			g_array_free(priv->snapshot_attributes, TRUE);
		}
		priv->snapshot_attributes = g_array_new(FALSE, FALSE,
							sizeof(struct _VteCharAttributes));

		/* Free the linebreak offsets and allocate a new array to hold
		 * them. */
		if (priv->snapshot_linebreaks != NULL) {
			g_array_free(priv->snapshot_linebreaks, TRUE);
		}
		priv->snapshot_linebreaks = g_array_new(FALSE, FALSE, sizeof(int));

		/* Get a new view of the uber-label. */
		tmp = vte_terminal_get_text_include_trailing_spaces(terminal,
								    all_selected,
								    NULL,
								    priv->snapshot_attributes);
		if (tmp == NULL) {
			/* Aaargh!  We're screwed. */
			return;
		}
		priv->snapshot_text = g_string_new_len(tmp,
						       priv->snapshot_attributes->len);
		g_free(tmp);

		/* Get the offsets to the beginnings of each character. */
		i = 0;
		next = priv->snapshot_text->str;
		while (i < priv->snapshot_attributes->len) {
			g_array_append_val(priv->snapshot_characters, i);
			next = g_utf8_next_char(next);
			if (next == NULL) {
				break;
			} else {
				i = next - priv->snapshot_text->str;
			}
		}
		/* Find offsets for the beginning of lines. */
		for (i = 0, row = 0; i < priv->snapshot_characters->len; i++) {
			/* Get the attributes for the current cell. */
			offset = g_array_index(priv->snapshot_characters,
					       int, i);
			attrs = g_array_index(priv->snapshot_attributes,
					      struct _VteCharAttributes,
					      offset);
			/* If this character is on a row different from the row
			 * the character we looked at previously was on, then
			 * it's a new line and we need to keep track of where
			 * it is. */
			if ((i == 0) || (attrs.row != row)) {
				_vte_debug_print(VTE_DEBUG_ALLY,
						"Row %d/%ld begins at %u.\n",
						priv->snapshot_linebreaks->len,
						attrs.row, i);
				g_array_append_val(priv->snapshot_linebreaks, i);
			}
			row = attrs.row;
		}
		/* Add the final line break. */
		g_array_append_val(priv->snapshot_linebreaks, i);
		/* We're finished updating this. */
		priv->snapshot_contents_invalid = FALSE;
	}

	/* Update the caret position. */
	vte_terminal_get_cursor_position(terminal, &ccol, &crow);
	_vte_debug_print(VTE_DEBUG_ALLY,
			"Cursor at (%ld, " "%ld).\n", ccol, crow);

	/* Get the offsets to the beginnings of each line. */
	caret = -1;
	for (i = 0; i < priv->snapshot_characters->len; i++) {
		/* Get the attributes for the current cell. */
		offset = g_array_index(priv->snapshot_characters,
				       int, i);
		attrs = g_array_index(priv->snapshot_attributes,
				      struct _VteCharAttributes,
				      offset);
		/* If this cell is "before" the cursor, move the
		 * caret to be "here". */
		if ((attrs.row < crow) ||
		    ((attrs.row == crow) && (attrs.column < ccol))) {
			caret = i + 1;
		}
	}

	/* If no cells are before the caret, then the caret must be
	 * at the end of the buffer. */
	if (caret == -1) {
		caret = priv->snapshot_characters->len;
	}

	/* Notify observers if the caret moved. */
	if (caret != priv->snapshot_caret) {
		priv->snapshot_caret = caret;
		emit_text_caret_moved(G_OBJECT(text), caret);
	}

	/* Done updating the caret position, whether we needed to or not. */
	priv->snapshot_caret_invalid = FALSE;

	_vte_debug_print(VTE_DEBUG_ALLY,
			"Refreshed accessibility snapshot, "
			"%ld cells, %ld characters.\n",
			(long)priv->snapshot_attributes->len,
			(long)priv->snapshot_characters->len);
}

/* A signal handler to catch "text-inserted/deleted/modified" signals. */
static void
vte_terminal_accessible_text_modified(VteTerminal *terminal, gpointer data)
{
	VteTerminalAccessiblePrivate *priv;
	char *old, *current;
	glong offset, caret_offset, olen, clen;
	gint old_snapshot_caret;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));

	priv = g_object_get_data(G_OBJECT(data),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	g_assert(priv != NULL);

	old_snapshot_caret = priv->snapshot_caret;
	priv->snapshot_contents_invalid = TRUE;
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(data),
							      &old, &olen);
	g_assert(old != NULL);

	current = priv->snapshot_text->str;
	clen = priv->snapshot_text->len;

	if ((guint) priv->snapshot_caret < priv->snapshot_characters->len) {
		caret_offset = g_array_index(priv->snapshot_characters,
				int, priv->snapshot_caret);
	} else {
		/* caret was not in the line */
		caret_offset = clen;
	}

	/* Find the offset where they don't match. */
	offset = 0;
	while ((offset < olen) && (offset < clen)) {
		if (old[offset] != current[offset]) {
			break;
		}
		offset++;
	}

        /* Check if we just backspaced over a space. */
	if ((olen == offset) &&
		       	(caret_offset < olen && old[caret_offset] == ' ') &&
			(old_snapshot_caret == priv->snapshot_caret + 1)) {
                priv->snapshot_text->str = old;
		priv->snapshot_text->len = caret_offset + 1;
		emit_text_changed_delete(G_OBJECT(data),
					 old, caret_offset, 1);
		priv->snapshot_text->str = current;
		priv->snapshot_text->len = clen;
	}


	/* At least one of them had better have more data, right? */
	if ((offset < olen) || (offset < clen)) {
		/* Back up from both end points until we find the *last* point
		 * where they differed. */
		gchar *op = old + olen;
		gchar *cp = current + clen;
		while (op > old + offset && cp > current + offset) {
			gchar *opp = g_utf8_prev_char (op);
			gchar *cpp = g_utf8_prev_char (cp);
			if (g_utf8_get_char (opp) != g_utf8_get_char (cpp)) {
				break;
			}
			op = opp;
			cp = cpp;
		}
		/* recompute the respective lengths */
		olen = op - old;
		clen = cp - current;
		/* At least one of them has to have text the other
		 * doesn't. */
		g_assert((clen > offset) || (olen > offset));
		g_assert((clen >= 0) && (olen >= 0));
		/* Now emit a deleted signal for text that was in the old
		 * string but isn't in the new one... */
		if (olen > offset) {
			gchar *saved_str = priv->snapshot_text->str;
			gsize saved_len = priv->snapshot_text->len;

			priv->snapshot_text->str = old;
			priv->snapshot_text->len = olen;
			emit_text_changed_delete(G_OBJECT(data),
						 old,
						 offset,
						 olen - offset);
			priv->snapshot_text->str = saved_str;
			priv->snapshot_text->len = saved_len;
		}
		/* .. and an inserted signal for text that wasn't in the old
		 * string but is in the new one. */
		if (clen > offset) {
			emit_text_changed_insert(G_OBJECT(data),
						 current,
						 offset,
						 clen - offset);
		}
	}

	g_free(old);
}

/* A signal handler to catch "text-scrolled" signals. */
static void
vte_terminal_accessible_text_scrolled(VteTerminal *terminal,
				      gint howmuch,
				      gpointer data)
{
	VteTerminalAccessiblePrivate *priv;
	struct _VteCharAttributes attr;
	long delta;
	guint i, len;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));
	g_assert(howmuch != 0);

	priv = g_object_get_data(G_OBJECT(data),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	g_assert(priv != NULL);

	if (((howmuch < 0) && (howmuch <= -terminal->row_count)) ||
	    ((howmuch > 0) && (howmuch >= terminal->row_count))) {
		/* All of the text was removed. */
		if (priv->snapshot_text != NULL) {
			if (priv->snapshot_text->str != NULL) {
				emit_text_changed_delete(G_OBJECT(data),
							 priv->snapshot_text->str,
							 0,
							 priv->snapshot_text->len);
			}
		}
		priv->snapshot_contents_invalid = TRUE;
		vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(data),
								      NULL,
								      NULL);
		/* All of the present text was added. */
		if (priv->snapshot_text != NULL) {
			if (priv->snapshot_text->str != NULL) {
				emit_text_changed_insert(G_OBJECT(data),
							 priv->snapshot_text->str,
							 0,
							 priv->snapshot_text->len);
			}
		}
		return;
	}
	/* Find the start point. */
	delta = 0;
	if (priv->snapshot_attributes != NULL) {
		if (priv->snapshot_attributes->len > 0) {
			attr = g_array_index(priv->snapshot_attributes,
					     struct _VteCharAttributes,
					     0);
			delta = attr.row;
		}
	}
	/* We scrolled up, so text was added at the top and removed
	 * from the bottom. */
	if ((howmuch < 0) && (howmuch > -terminal->row_count)) {
		gboolean inserted = FALSE;
		howmuch = -howmuch;
		if (priv->snapshot_attributes != NULL &&
				priv->snapshot_text != NULL) {
			/* Find the first byte that scrolled off. */
			for (i = 0; i < priv->snapshot_attributes->len; i++) {
				attr = g_array_index(priv->snapshot_attributes,
						struct _VteCharAttributes,
						i);
				if (attr.row >= delta + terminal->row_count - howmuch) {
					break;
				}
			}
			if (i < priv->snapshot_attributes->len) {
				/* The rest of the string was deleted -- make a note. */
				emit_text_changed_delete(G_OBJECT(data),
						priv->snapshot_text->str,
						i,
						priv->snapshot_attributes->len - i);
			}
			inserted = TRUE;
		}
		/* Refresh.  Note that i is now the length of the data which
		 * we expect to have left over. */
		priv->snapshot_contents_invalid = TRUE;
		vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(data),
								      NULL,
								      NULL);
		/* If we now have more text than before, the initial portion
		 * was added. */
		if (inserted) {
			len = priv->snapshot_text->len;
			if (len > i) {
				emit_text_changed_insert(G_OBJECT(data),
							 priv->snapshot_text->str,
							 0,
							 len - i);
			}
		}
		return;
	}
	/* We scrolled down, so text was added at the bottom and removed
	 * from the top. */
	if ((howmuch > 0) && (howmuch < terminal->row_count)) {
		gboolean inserted = FALSE;
		if (priv->snapshot_attributes != NULL &&
				priv->snapshot_text != NULL) {
			/* Find the first byte that wasn't scrolled off the top. */
			for (i = 0; i < priv->snapshot_attributes->len; i++) {
				attr = g_array_index(priv->snapshot_attributes,
						struct _VteCharAttributes,
						i);
				if (attr.row >= delta + howmuch) {
					break;
				}
			}
			/* That many bytes disappeared -- make a note. */
			emit_text_changed_delete(G_OBJECT(data),
					priv->snapshot_text->str,
					0,
					i);
			/* Figure out how much text was left, and refresh. */
			i = strlen(priv->snapshot_text->str + i);
			inserted = TRUE;
		}
		priv->snapshot_contents_invalid = TRUE;
		vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(data),
								      NULL,
								      NULL);
		/* Any newly-added string data is new, so note that it was
		 * inserted. */
		if (inserted) {
			len = priv->snapshot_text->len;
			if (len > i) {
				emit_text_changed_insert(G_OBJECT(data),
							 priv->snapshot_text->str,
							 i,
							 len - i);
			}
		}
		return;
	}
	g_assert_not_reached();
}

/* A signal handler to catch "cursor-moved" signals. */
static void
vte_terminal_accessible_invalidate_cursor(VteTerminal *terminal, gpointer data)
{
	VteTerminalAccessiblePrivate *priv;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));

	priv = g_object_get_data(G_OBJECT(data),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	g_assert(priv != NULL);

	_vte_debug_print(VTE_DEBUG_ALLY,
			"Invalidating accessibility cursor.\n");
	priv->snapshot_caret_invalid = TRUE;
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(data),
							      NULL, NULL);
}

/* Handle title changes by resetting the description. */
static void
vte_terminal_accessible_title_changed(VteTerminal *terminal, gpointer data)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));
	g_assert(VTE_IS_TERMINAL(terminal));
	atk_object_set_description(ATK_OBJECT(data), terminal->window_title);
}

/* Reflect focus-in events. */
static gboolean
vte_terminal_accessible_focus_in(VteTerminal *terminal, GdkEventFocus *event,
				 gpointer data)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));
	g_assert(VTE_IS_TERMINAL(terminal));
	g_signal_emit_by_name(data, "focus-event", TRUE);
	atk_object_notify_state_change(ATK_OBJECT(data),
				       ATK_STATE_FOCUSED, TRUE);

	return FALSE;
}

/* Reflect focus-out events. */
static gboolean
vte_terminal_accessible_focus_out(VteTerminal *terminal, GdkEventFocus *event,
				  gpointer data)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));
	g_assert(VTE_IS_TERMINAL(terminal));
	g_signal_emit_by_name(data, "focus-event", FALSE);
	atk_object_notify_state_change(ATK_OBJECT(data),
				       ATK_STATE_FOCUSED, FALSE);

	return FALSE;
}

/* Reflect visibility-notify events. */
static gboolean
vte_terminal_accessible_visibility_notify(VteTerminal *terminal,
					  GdkEventVisibility *event,
					  gpointer data)
{
	GtkWidget *widget;
	gboolean visible;
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));
	g_assert(VTE_IS_TERMINAL(terminal));
	visible = event->state != GDK_VISIBILITY_FULLY_OBSCURED;
	/* The VISIBLE state indicates that this widget is "visible". */
	atk_object_notify_state_change(ATK_OBJECT(data),
				       ATK_STATE_VISIBLE,
				       visible);
	widget = GTK_WIDGET(terminal);
	while (visible) {
		if (gtk_widget_get_toplevel(widget) == widget) {
			break;
		}
		if (widget == NULL) {
			break;
		}
		visible = visible && (gtk_widget_get_visible(widget));
		widget = gtk_widget_get_parent(widget);
	}
	/* The SHOWING state indicates that this widget, and all of its
	 * parents up to the toplevel, are "visible". */
	atk_object_notify_state_change(ATK_OBJECT(data),
				       ATK_STATE_SHOWING,
				       visible);

	return FALSE;
}

static void
vte_terminal_accessible_selection_changed (VteTerminal *terminal,
					   gpointer data)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(data));
	g_assert(VTE_IS_TERMINAL(terminal));

	g_signal_emit_by_name (data, "text_selection_changed");
}

static void
vte_terminal_initialize (AtkObject *obj, gpointer data)
{
	VteTerminal *terminal;
	AtkObject *parent;

	ATK_OBJECT_CLASS (vte_terminal_accessible_parent_class)->initialize (obj, data);

	terminal = VTE_TERMINAL (data);

	_vte_terminal_accessible_ref(terminal);

	g_object_set_data(G_OBJECT(obj),
			  VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA,
			  vte_terminal_accessible_new_private_data());

	g_signal_connect(terminal, "text-inserted",
			 G_CALLBACK(vte_terminal_accessible_text_modified),
			 obj);
	g_signal_connect(terminal, "text-deleted",
			 G_CALLBACK(vte_terminal_accessible_text_modified),
			 obj);
	g_signal_connect(terminal, "text-modified",
			 G_CALLBACK(vte_terminal_accessible_text_modified),
			 obj);
	g_signal_connect(terminal, "text-scrolled",
			 G_CALLBACK(vte_terminal_accessible_text_scrolled),
			 obj);
	g_signal_connect(terminal, "cursor-moved",
			 G_CALLBACK(vte_terminal_accessible_invalidate_cursor),
			 obj);
	g_signal_connect(terminal, "window-title-changed",
			 G_CALLBACK(vte_terminal_accessible_title_changed),
			 obj);
	g_signal_connect(terminal, "focus-in-event",
			 G_CALLBACK(vte_terminal_accessible_focus_in),
			 obj);
	g_signal_connect(terminal, "focus-out-event",
			 G_CALLBACK(vte_terminal_accessible_focus_out),
			 obj);
	g_signal_connect(terminal, "visibility-notify-event",
			 G_CALLBACK(vte_terminal_accessible_visibility_notify),
			 obj);
	g_signal_connect(terminal, "selection-changed",
			 G_CALLBACK(vte_terminal_accessible_selection_changed),
			 obj);

	if (GTK_IS_WIDGET(gtk_widget_get_parent(GTK_WIDGET(terminal)))) {
		parent = gtk_widget_get_accessible(gtk_widget_get_parent ((GTK_WIDGET(terminal))));
		if (ATK_IS_OBJECT(parent)) {
			atk_object_set_parent(obj, parent);
		}
	}

	atk_object_set_name(obj, "Terminal");
	atk_object_set_description(obj,
				   terminal->window_title ?
				   terminal->window_title :
				   "");

	atk_object_notify_state_change(obj,
				       ATK_STATE_FOCUSABLE, TRUE);
	atk_object_notify_state_change(obj,
				       ATK_STATE_EXPANDABLE, FALSE);
	atk_object_notify_state_change(obj,
				       ATK_STATE_RESIZABLE, TRUE);
	obj->role = ATK_ROLE_TERMINAL;
}

/**
 * vte_terminal_accessible_new:
 * @terminal: a #VteTerminal
 *
 * Creates a new accessibility peer for the terminal widget.
 *
 * Returns: the new #AtkObject
 */
AtkObject *
vte_terminal_accessible_new(VteTerminal *terminal)
{
	AtkObject *accessible;
	GObject *object;

	g_return_val_if_fail(VTE_IS_TERMINAL(terminal), NULL);

	object = g_object_new(VTE_TYPE_TERMINAL_ACCESSIBLE, NULL);
	accessible = ATK_OBJECT (object);
	atk_object_initialize(accessible, terminal);

	return accessible;
}

static void
vte_terminal_accessible_finalize(GObject *object)
{
	VteTerminalAccessiblePrivate *priv;
	GtkAccessible *accessible = NULL;
        GtkWidget *widget;

	_vte_debug_print(VTE_DEBUG_ALLY, "Finalizing accessible peer.\n");

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(object));
	accessible = GTK_ACCESSIBLE(object);
        widget = gtk_accessible_get_widget (accessible);

	if (widget != NULL) {
		g_object_remove_weak_pointer(G_OBJECT(widget),
					     (gpointer*)(gpointer)&widget);
		g_signal_handlers_disconnect_matched(widget,
						     G_SIGNAL_MATCH_FUNC |
						     G_SIGNAL_MATCH_DATA,
						     0, 0, NULL,
						     vte_terminal_accessible_text_modified,
						     object);
		g_signal_handlers_disconnect_matched(widget,
						     G_SIGNAL_MATCH_FUNC |
						     G_SIGNAL_MATCH_DATA,
						     0, 0, NULL,
						     vte_terminal_accessible_text_scrolled,
						     object);
		g_signal_handlers_disconnect_matched(widget,
						     G_SIGNAL_MATCH_FUNC |
						     G_SIGNAL_MATCH_DATA,
						     0, 0, NULL,
						     vte_terminal_accessible_invalidate_cursor,
						     object);
		g_signal_handlers_disconnect_matched(widget,
						     G_SIGNAL_MATCH_FUNC |
						     G_SIGNAL_MATCH_DATA,
						     0, 0, NULL,
						     vte_terminal_accessible_title_changed,
						     object);
		g_signal_handlers_disconnect_matched(widget,
						     G_SIGNAL_MATCH_FUNC |
						     G_SIGNAL_MATCH_DATA,
						     0, 0, NULL,
						     vte_terminal_accessible_focus_in,
						     object);
		g_signal_handlers_disconnect_matched(widget,
						     G_SIGNAL_MATCH_FUNC |
						     G_SIGNAL_MATCH_DATA,
						     0, 0, NULL,
						     vte_terminal_accessible_focus_out,
						     object);
		g_signal_handlers_disconnect_matched(widget,
						     G_SIGNAL_MATCH_FUNC |
						     G_SIGNAL_MATCH_DATA,
						     0, 0, NULL,
						     vte_terminal_accessible_visibility_notify,
						     object);
	}
	priv = g_object_get_data(object,
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	if (priv != NULL) {
		vte_terminal_accessible_free_private_data(priv);
		g_object_set_data(object,
				  VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA,
				  NULL);
	}
	G_OBJECT_CLASS(vte_terminal_accessible_parent_class)->finalize(object);
}

static gchar *
vte_terminal_accessible_get_text(AtkText *text,
				 gint start_offset, gint end_offset)
{
	VteTerminalAccessiblePrivate *priv;
	int start, end;
	gchar *ret;

        /* Swap around if start is greater than end */
        if (start_offset > end_offset && end_offset != -1) {
                gint tmp;

                tmp = start_offset;
                start_offset = end_offset;
                end_offset = tmp;
        }

	g_assert((start_offset >= 0) && (end_offset >= -1));

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);

	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	_vte_debug_print(VTE_DEBUG_ALLY,
			"Getting text from %d to %d of %d.\n",
			start_offset, end_offset,
			priv->snapshot_characters->len);
	g_assert(ATK_IS_TEXT(text));

	/* If the requested area is after all of the text, just return an
	 * empty string. */
	if (start_offset >= (int) priv->snapshot_characters->len) {
		return g_strdup("");
	}

	/* Map the offsets to, er, offsets. */
	start = g_array_index(priv->snapshot_characters, int, start_offset);
	if ((end_offset == -1) || (end_offset >= (int) priv->snapshot_characters->len) ) {
		/* Get everything up to the end of the buffer. */
		end = priv->snapshot_text->len;
	} else {
		/* Map the stopping point. */
		end = g_array_index(priv->snapshot_characters, int, end_offset);
	}
	if (end <= start) {
		ret = g_strdup ("");
	} else {
		ret = g_malloc(end - start + 1);
		memcpy(ret, priv->snapshot_text->str + start, end - start);
		ret[end - start] = '\0';
	}
	return ret;
}

/* Map a subsection of the text with before/at/after char/word/line specs
 * into a run of Unicode characters.  (The interface is specifying characters,
 * not bytes, plus that saves us from having to deal with parts of multibyte
 * characters, which are icky.) */
static gchar *
vte_terminal_accessible_get_text_somewhere(AtkText *text,
					   gint offset,
					   AtkTextBoundary boundary_type,
					   enum direction direction,
					   gint *start_offset,
					   gint *end_offset)
{
	VteTerminalAccessiblePrivate *priv;
	VteTerminal *terminal;
	gunichar current, prev, next;
	guint start, end, line;

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);

	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	terminal = VTE_TERMINAL(gtk_accessible_get_widget (GTK_ACCESSIBLE(text)));

	_vte_debug_print(VTE_DEBUG_ALLY,
			"Getting %s %s at %d of %d.\n",
			(direction == direction_current) ? "this" :
			((direction == direction_next) ? "next" : "previous"),
			(boundary_type == ATK_TEXT_BOUNDARY_CHAR) ? "char" :
			((boundary_type == ATK_TEXT_BOUNDARY_LINE_START) ? "line (start)" :
			((boundary_type == ATK_TEXT_BOUNDARY_LINE_END) ? "line (end)" :
			((boundary_type == ATK_TEXT_BOUNDARY_WORD_START) ? "word (start)" :
			((boundary_type == ATK_TEXT_BOUNDARY_WORD_END) ? "word (end)" :
			((boundary_type == ATK_TEXT_BOUNDARY_SENTENCE_START) ? "sentence (start)" :
			((boundary_type == ATK_TEXT_BOUNDARY_SENTENCE_END) ? "sentence (end)" : "unknown")))))),
			offset, priv->snapshot_attributes->len);
	g_assert(priv->snapshot_text != NULL);
	g_assert(priv->snapshot_characters != NULL);
	if (offset >= (int) priv->snapshot_characters->len) {
		return g_strdup("");
	}
	g_assert(offset < (int) priv->snapshot_characters->len);
	g_assert(offset >= 0);

	switch (boundary_type) {
		case ATK_TEXT_BOUNDARY_CHAR:
			/* We're either looking at the character at this
			 * position, the one before it, or the one after it. */
			offset += direction;
			start = MAX(offset, 0);
			end = MIN(offset + 1, (int) priv->snapshot_attributes->len);
			break;
		case ATK_TEXT_BOUNDARY_WORD_START:
			/* Back up to the previous non-word-word transition. */
			while (offset > 0) {
				prev = vte_terminal_accessible_get_character_at_offset(text, offset - 1);
				if (vte_terminal_is_word_char(terminal, prev)) {
					offset--;
				} else {
					break;
				}
			}
			start = offset;
			/* If we started in a word and we're looking for the
			 * word before this one, keep searching by backing up
			 * to the previous non-word character and then searching
			 * for the word-start before that. */
			if (direction == direction_previous) {
				while (offset > 0) {
					prev = vte_terminal_accessible_get_character_at_offset(text, offset - 1);
					if (!vte_terminal_is_word_char(terminal, prev)) {
						offset--;
					} else {
						break;
					}
				}
				while (offset > 0) {
					prev = vte_terminal_accessible_get_character_at_offset(text, offset - 1);
					if (vte_terminal_is_word_char(terminal, prev)) {
						offset--;
					} else {
						break;
					}
				}
				start = offset;
			}
			/* If we're looking for the word after this one,
			 * search forward by scanning forward for the next
			 * non-word character, then the next word character
			 * after that. */
			if (direction == direction_next) {
				while (offset < (int) priv->snapshot_characters->len) {
					next = vte_terminal_accessible_get_character_at_offset(text, offset);
					if (vte_terminal_is_word_char(terminal, next)) {
						offset++;
					} else {
						break;
					}
				}
				while (offset < (int) priv->snapshot_characters->len) {
					next = vte_terminal_accessible_get_character_at_offset(text, offset);
					if (!vte_terminal_is_word_char(terminal, next)) {
						offset++;
					} else {
						break;
					}
				}
				start = offset;
			}
			/* Now find the end of this word. */
			while (offset < (int) priv->snapshot_characters->len) {
				current = vte_terminal_accessible_get_character_at_offset(text, offset);
				if (vte_terminal_is_word_char(terminal, current)) {
					offset++;
				} else {
					break;
				}

			}
			/* Now find the next non-word-word transition */
			while (offset < (int) priv->snapshot_characters->len) {
				next = vte_terminal_accessible_get_character_at_offset(text, offset);
				if (!vte_terminal_is_word_char(terminal, next)) {
					offset++;
				} else {
					break;
				}
			}
			end = offset;
			break;
		case ATK_TEXT_BOUNDARY_WORD_END:
			/* Back up to the previous word-non-word transition. */
			current = vte_terminal_accessible_get_character_at_offset(text, offset);
			while (offset > 0) {
				prev = vte_terminal_accessible_get_character_at_offset(text, offset - 1);
				if (vte_terminal_is_word_char(terminal, prev) &&
				    !vte_terminal_is_word_char(terminal, current)) {
					break;
				} else {
					offset--;
					current = prev;
				}
			}
			start = offset;
			/* If we're looking for the word end before this one, 
			 * keep searching by backing up to the previous word 
			 * character and then searching for the word-end 
			 * before that. */
			if (direction == direction_previous) {
				while (offset > 0) {
					prev = vte_terminal_accessible_get_character_at_offset(text, offset - 1);
					if (vte_terminal_is_word_char(terminal, prev)) {
						offset--;
					} else {
						break;
					}
				}
				current = vte_terminal_accessible_get_character_at_offset(text, offset);
				while (offset > 0) {
					prev = vte_terminal_accessible_get_character_at_offset(text, offset - 1);
					if (vte_terminal_is_word_char(terminal, prev) &&
					    !vte_terminal_is_word_char(terminal, current)) {
						break;
					} else {
						offset--;
						current = prev;
					}
				}
				start = offset;
			}
			/* If we're looking for the word end after this one,
			 * search forward by scanning forward for the next
			 * word character, then the next non-word character
			 * after that. */
			if (direction == direction_next) {
				while (offset < (int) priv->snapshot_characters->len) {
					current = vte_terminal_accessible_get_character_at_offset(text, offset);
					if (!vte_terminal_is_word_char(terminal, current)) {
						offset++;
					} else {
						break;
					}
				}
				while (offset < (int) priv->snapshot_characters->len) {
					current = vte_terminal_accessible_get_character_at_offset(text, offset);
					if (vte_terminal_is_word_char(terminal, current)) {
						offset++;
					} else {
						break;
					}
				}
				start = offset;
			}
			/* Now find the next word end. */
			while (offset < (int) priv->snapshot_characters->len) {
				current = vte_terminal_accessible_get_character_at_offset(text, offset);
				if (!vte_terminal_is_word_char(terminal, current)) {
					offset++;
				} else {
					break;
				}
			}
			while (offset < (int) priv->snapshot_characters->len) {
				current = vte_terminal_accessible_get_character_at_offset(text, offset);
				if (vte_terminal_is_word_char(terminal, current)) {
					offset++;
				} else {
					break;
				}
			}
			end = offset;
			break;
		case ATK_TEXT_BOUNDARY_LINE_START:
		case ATK_TEXT_BOUNDARY_LINE_END:
			/* Figure out which line we're on.  If the start of the
			 * i'th line is before the offset, then i could be the
			 * line we're looking for. */
			line = 0;
			for (line = 0;
			     line < priv->snapshot_linebreaks->len;
			     line++) {
				if (g_array_index(priv->snapshot_linebreaks,
						  int, line) > offset) {
					line--;
					break;
				}
			}
			_vte_debug_print(VTE_DEBUG_ALLY,
					"Character %d is on line %d.\n",
					offset, line);
			/* Perturb the line number to handle before/at/after. */
			line += direction;
			line = MIN(line, priv->snapshot_linebreaks->len - 1);
			/* Read the offsets for this line. */
			start = g_array_index(priv->snapshot_linebreaks,
						      int, line);
			line++;
			line = MIN(line, priv->snapshot_linebreaks->len - 1);
			end = g_array_index(priv->snapshot_linebreaks,
						    int, line);
			_vte_debug_print(VTE_DEBUG_ALLY,
					"Line runs from %d to %d.\n",
					start, end);
			break;
		case ATK_TEXT_BOUNDARY_SENTENCE_START:
		case ATK_TEXT_BOUNDARY_SENTENCE_END:
			/* This doesn't make sense.  Fall through. */
		default:
			start = end = 0;
			break;
	}
	*start_offset = start = MIN(start, priv->snapshot_characters->len - 1);
	*end_offset = end = CLAMP(end, start, priv->snapshot_characters->len);
	return vte_terminal_accessible_get_text(text, start, end);
}

static gchar *
vte_terminal_accessible_get_text_before_offset(AtkText *text, gint offset,
					       AtkTextBoundary boundary_type,
					       gint *start_offset,
					       gint *end_offset)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	return vte_terminal_accessible_get_text_somewhere(text,
							  offset,
							  boundary_type,
							  -1,
							  start_offset,
							  end_offset);
}

static gchar *
vte_terminal_accessible_get_text_after_offset(AtkText *text, gint offset,
					      AtkTextBoundary boundary_type,
					      gint *start_offset,
					      gint *end_offset)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	return vte_terminal_accessible_get_text_somewhere(text,
							  offset,
							  boundary_type,
							  1,
							  start_offset,
							  end_offset);
}

static gchar *
vte_terminal_accessible_get_text_at_offset(AtkText *text, gint offset,
					   AtkTextBoundary boundary_type,
					   gint *start_offset,
					   gint *end_offset)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	return vte_terminal_accessible_get_text_somewhere(text,
							  offset,
							  boundary_type,
							  0,
							  start_offset,
							  end_offset);
}

static gunichar
vte_terminal_accessible_get_character_at_offset(AtkText *text, gint offset)
{
	VteTerminalAccessiblePrivate *priv;
	int mapped;
	char *unichar;
	gunichar ret;

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);

	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);

	g_assert(offset < (int) priv->snapshot_characters->len);

	mapped = g_array_index(priv->snapshot_characters, int, offset);

	unichar = vte_terminal_accessible_get_text(text, offset, offset + 1);
	ret = g_utf8_get_char(unichar);
	g_free(unichar);

	return ret;
}

static gint
vte_terminal_accessible_get_caret_offset(AtkText *text)
{
	VteTerminalAccessiblePrivate *priv;

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);

	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);

	return priv->snapshot_caret;
}

static AtkAttributeSet *
get_attribute_set (struct _VteCharAttributes attr)
{
	AtkAttributeSet *set = NULL;
	AtkAttribute *at;

	if (attr.underline) {
		at = g_new (AtkAttribute, 1);
		at->name = g_strdup ("underline");
		at->value = g_strdup ("true");
		set = g_slist_append (set, at);
	}
	if (attr.strikethrough) {
		at = g_new (AtkAttribute, 1);
		at->name = g_strdup ("strikethrough");
		at->value = g_strdup ("true");
		set = g_slist_append (set, at);
	}
	at = g_new (AtkAttribute, 1);
	at->name = g_strdup ("fg-color");
	at->value = g_strdup_printf ("%u,%u,%u",
				     attr.fore.red, attr.fore.green, attr.fore.blue);
	set = g_slist_append (set, at);

	at = g_new (AtkAttribute, 1);
	at->name = g_strdup ("bg-color");
	at->value = g_strdup_printf ("%u,%u,%u",
				     attr.back.red, attr.back.green, attr.back.blue);
	set = g_slist_append (set, at);

	return set;
}

static AtkAttributeSet *
vte_terminal_accessible_get_run_attributes(AtkText *text, gint offset,
					   gint *start_offset, gint *end_offset)
{
	VteTerminalAccessiblePrivate *priv;
	guint i;
	struct _VteCharAttributes cur_attr;
	struct _VteCharAttributes attr;

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);

	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);

	attr = g_array_index (priv->snapshot_attributes,
			      struct _VteCharAttributes,
			      offset);
	*start_offset = 0;
	for (i = offset; i--;) {
		cur_attr = g_array_index (priv->snapshot_attributes,
				      struct _VteCharAttributes,
				      i);
		if (!gdk_color_equal (&cur_attr.fore, &attr.fore) ||
		    !gdk_color_equal (&cur_attr.back, &attr.back) ||
		    cur_attr.underline != attr.underline ||
		    cur_attr.strikethrough != attr.strikethrough) {
			*start_offset = i + 1;
			break;
		}
	}
	*end_offset = priv->snapshot_attributes->len - 1;
	for (i = offset + 1; i < priv->snapshot_attributes->len; i++) {
		cur_attr = g_array_index (priv->snapshot_attributes,
				      struct _VteCharAttributes,
				      i);
		if (!gdk_color_equal (&cur_attr.fore, &attr.fore) ||
		    !gdk_color_equal (&cur_attr.back, &attr.back) ||
		    cur_attr.underline != attr.underline ||
		    cur_attr.strikethrough != attr.strikethrough) {
			*end_offset = i - 1;
			break;
		}
	}

	return get_attribute_set (attr);
}

static AtkAttributeSet *
vte_terminal_accessible_get_default_attributes(AtkText *text)
{
	return NULL;
}

static void
vte_terminal_accessible_get_character_extents(AtkText *text, gint offset,
					      gint *x, gint *y,
					      gint *width, gint *height,
					      AtkCoordType coords)
{
	VteTerminalAccessiblePrivate *priv;
	VteTerminal *terminal;
	glong char_width, char_height;
	gint base_x, base_y;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	terminal = VTE_TERMINAL (gtk_accessible_get_widget (GTK_ACCESSIBLE (text)));

	atk_component_get_position (ATK_COMPONENT (text), &base_x, &base_y, coords);
	xy_from_offset (priv, offset, x, y);
	char_width = vte_terminal_get_char_width (terminal);
	char_height = vte_terminal_get_char_height (terminal);
	*x *= char_width;
	*y *= char_height;
	*width = char_width;
	*height = char_height;
	*x += base_x;
	*y += base_y;
}

static gint
vte_terminal_accessible_get_character_count(AtkText *text)
{
	VteTerminalAccessiblePrivate *priv;

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);

	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);

	return priv->snapshot_attributes->len;
}

static gint
vte_terminal_accessible_get_offset_at_point(AtkText *text,
					    gint x, gint y,
					    AtkCoordType coords)
{
	VteTerminalAccessiblePrivate *priv;
	VteTerminal *terminal;
	glong char_width, char_height;
	gint base_x, base_y;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));

	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	terminal = VTE_TERMINAL (gtk_accessible_get_widget (GTK_ACCESSIBLE (text)));

	atk_component_get_position (ATK_COMPONENT (text), &base_x, &base_y, coords);
	char_width = vte_terminal_get_char_width (terminal);
	char_height = vte_terminal_get_char_height (terminal);
	x -= base_x;
	y -= base_y;
	x /= char_width;
	y /= char_height;
	return offset_from_xy (priv, x, y);
}

static gint
vte_terminal_accessible_get_n_selections(AtkText *text)
{
	GtkWidget *widget;
	VteTerminal *terminal;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);

	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(text));
	if (widget == NULL) {
		/* State is defunct */
		return -1;
	}
	g_assert(VTE_IS_TERMINAL (widget));
	terminal = VTE_TERMINAL (widget);
	return (vte_terminal_get_has_selection (terminal)) ? 1 : 0;
}

static gchar *
vte_terminal_accessible_get_selection(AtkText *text, gint selection_number,
				      gint *start_offset, gint *end_offset)
{
	GtkWidget *widget;
	VteTerminal *terminal;
	VteTerminalAccessiblePrivate *priv;
	long start_x, start_y, end_x, end_y;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(text));
	if (widget == NULL) {
		/* State is defunct */
		return NULL;
	}
	g_assert(VTE_IS_TERMINAL (widget));
	terminal = VTE_TERMINAL (widget);
	if (!vte_terminal_get_has_selection (terminal)) {
		return NULL;
	}
	if (selection_number != 0) {
		return NULL;
	}

	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	_vte_terminal_get_start_selection (terminal, &start_x, &start_y);
	*start_offset = offset_from_xy (priv, start_x, start_y);
	_vte_terminal_get_end_selection (terminal, &end_x, &end_y);
	*end_offset = offset_from_xy (priv, end_x, end_y);
	return _vte_terminal_get_selection (terminal);
}

static gboolean
vte_terminal_accessible_add_selection(AtkText *text,
				      gint start_offset, gint end_offset)
{
	GtkWidget *widget;
	VteTerminal *terminal;
	VteTerminalAccessiblePrivate *priv;
	gint start_x, start_y, end_x, end_y;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(text));
	if (widget == NULL) {
		/* State is defunct */
		return FALSE;
	}
	g_assert(VTE_IS_TERMINAL (widget));
	terminal = VTE_TERMINAL (widget);
	g_assert(!vte_terminal_get_has_selection (terminal));
	priv = g_object_get_data(G_OBJECT(text),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	xy_from_offset (priv, start_offset, &start_x, &start_y);
	xy_from_offset (priv, end_offset, &end_x, &end_y);
	_vte_terminal_select_text (terminal, start_x, start_y, end_x, end_y, start_offset, end_offset);
	return TRUE;
}

static gboolean
vte_terminal_accessible_remove_selection(AtkText *text,
					 gint selection_number)
{
	GtkWidget *widget;
	VteTerminal *terminal;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(text));
	if (widget == NULL) {
		/* State is defunct */
		return FALSE;
	}
	g_assert(VTE_IS_TERMINAL (widget));
	terminal = VTE_TERMINAL (widget);
	if (selection_number == 0 && vte_terminal_get_has_selection (terminal)) {
		_vte_terminal_remove_selection (terminal);
		return TRUE;
	} else {
		return FALSE;
	}
}

static gboolean
vte_terminal_accessible_set_selection(AtkText *text, gint selection_number,
				      gint start_offset, gint end_offset)
{
	GtkWidget *widget;
	VteTerminal *terminal;

	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(text));
	if (widget == NULL) {
		/* State is defunct */
		return FALSE;
	}
	g_assert(VTE_IS_TERMINAL (widget));
	terminal = VTE_TERMINAL (widget);
	if (selection_number != 0) {
		return FALSE;
	}
	if (vte_terminal_get_has_selection (terminal)) {
		_vte_terminal_remove_selection (terminal);
	}

	return vte_terminal_accessible_add_selection (text, start_offset, end_offset);
}

static gboolean
vte_terminal_accessible_set_caret_offset(AtkText *text, gint offset)
{
	g_assert(VTE_IS_TERMINAL_ACCESSIBLE(text));
	vte_terminal_accessible_update_private_data_if_needed(ATK_OBJECT(text),
							      NULL, NULL);
	/* Whoa, very not allowed. */
	return FALSE;
}

static void
vte_terminal_accessible_text_init(gpointer iface, gpointer data)
{
	AtkTextIface *text;
	g_assert(G_TYPE_FROM_INTERFACE(iface) == ATK_TYPE_TEXT);
	text = iface;
	_vte_debug_print(VTE_DEBUG_ALLY,
			"Initializing accessible peer's AtkText interface.\n");
	text->get_text = vte_terminal_accessible_get_text;
	text->get_text_after_offset = vte_terminal_accessible_get_text_after_offset;
	text->get_text_at_offset = vte_terminal_accessible_get_text_at_offset;
	text->get_character_at_offset = vte_terminal_accessible_get_character_at_offset;
	text->get_text_before_offset = vte_terminal_accessible_get_text_before_offset;
	text->get_caret_offset = vte_terminal_accessible_get_caret_offset;
	text->get_run_attributes = vte_terminal_accessible_get_run_attributes;
	text->get_default_attributes = vte_terminal_accessible_get_default_attributes;
	text->get_character_extents = vte_terminal_accessible_get_character_extents;
	text->get_character_count = vte_terminal_accessible_get_character_count;
	text->get_offset_at_point = vte_terminal_accessible_get_offset_at_point;
	text->get_n_selections = vte_terminal_accessible_get_n_selections;
	text->get_selection = vte_terminal_accessible_get_selection;
	text->add_selection = vte_terminal_accessible_add_selection;
	text->remove_selection = vte_terminal_accessible_remove_selection;
	text->set_selection = vte_terminal_accessible_set_selection;
	text->set_caret_offset = vte_terminal_accessible_set_caret_offset;
}

static AtkLayer
vte_terminal_accessible_get_layer(AtkComponent *component)
{
	return ATK_LAYER_WIDGET;
}

static gint
vte_terminal_accessible_get_mdi_zorder(AtkComponent *component)
{
	return G_MININT;
}

static gboolean
vte_terminal_accessible_contains(AtkComponent *component,
				 gint x, gint y,
				 AtkCoordType coord_type)
{
	gint ex, ey, ewidth, eheight;
	atk_component_get_extents(component, &ex, &ey, &ewidth, &eheight,
				  coord_type);
	return ((x >= ex) &&
		(x < ex + ewidth) &&
		(y >= ey) &&
		(y < ey + eheight));
}

static void
vte_terminal_accessible_get_extents(AtkComponent *component,
				    gint *x, gint *y,
				    gint *width, gint *height,
				    AtkCoordType coord_type)
{
	atk_component_get_position(component, x, y, coord_type);
	atk_component_get_size(component, width, height);
}

static void
vte_terminal_accessible_get_position(AtkComponent *component,
				     gint *x, gint *y,
				     AtkCoordType coord_type)
{
	GtkWidget *widget;
	*x = 0;
	*y = 0;
	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(component));
	if (widget == NULL) {
		return;
	}
	if (!gtk_widget_get_realized(widget)) {
		return;
	}
	switch (coord_type) {
	case ATK_XY_SCREEN:
		gdk_window_get_origin(gtk_widget_get_window (widget), x, y);
		break;
	case ATK_XY_WINDOW:
		gdk_window_get_position(gtk_widget_get_window (widget), x, y);
		break;
	default:
		g_assert_not_reached();
		break;
	}
}

static void
vte_terminal_accessible_get_size(AtkComponent *component,
				 gint *width, gint *height)
{
	GtkWidget *widget;
	GdkWindow *window;
	*width = 0;
	*height = 0;
	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(component));
	if (widget == NULL) {
		return;
	}
	if (!gtk_widget_get_realized(widget)) {
		return;
	}
	window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION (2, 90, 8)
	if (width)
		*width = gdk_window_get_width (window);
	if (height)
		*height = gdk_window_get_height (window);
#else
	gdk_drawable_get_size(window, width, height);
#endif
}

static gboolean
vte_terminal_accessible_set_extents(AtkComponent *component,
				    gint x, gint y,
				    gint width, gint height,
				    AtkCoordType coord_type)
{
	/* FIXME?  We can change the size, but our position is controlled
	 * by the parent container. */
	return FALSE;
}

static gboolean
vte_terminal_accessible_set_position(AtkComponent *component,
				     gint x, gint y,
				     AtkCoordType coord_type)
{
	/* Controlled by the parent container, if there is one. */
	return FALSE;
}

static gboolean
vte_terminal_accessible_set_size(AtkComponent *component,
				 gint width, gint height)
{
	VteTerminal *terminal;
	gint columns, rows, char_width, char_height;
	GtkWidget *widget;
        GtkBorder *inner_border;

	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE(component));
	if (widget == NULL) {
		return FALSE;
	}
	terminal = VTE_TERMINAL(widget);

        char_width = vte_terminal_get_char_width (terminal);
        char_height = vte_terminal_get_char_height (terminal);
        gtk_widget_style_get (widget, "inner-border", &inner_border, NULL);
	/* If the size is an exact multiple of the cell size, use that,
	 * otherwise round down. */
        columns = (width - (inner_border ? (inner_border->left + inner_border->right) : 0)) / char_width;
        rows = (height - (inner_border ? (inner_border->top + inner_border->bottom) : 0)) / char_height;
        gtk_border_free (inner_border);
	vte_terminal_set_size(terminal, columns, rows);
	return (vte_terminal_get_row_count (terminal) == rows) &&
	       (vte_terminal_get_column_count (terminal) == columns);
}


static AtkObject *
vte_terminal_accessible_ref_accessible_at_point(AtkComponent *component,
						gint x, gint y,
						AtkCoordType coord_type)
{
	/* There are no children. */
	return NULL;
}

static guint
vte_terminal_accessible_add_focus_handler(AtkComponent *component,
					  AtkFocusHandler handler)
{
	guint signal_id;
	signal_id = g_signal_lookup("focus-event",
				    VTE_TYPE_TERMINAL_ACCESSIBLE);
	if (g_signal_handler_find(component,
				  G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_ID,
				  signal_id,
				  0,
				  NULL,
				  (gpointer)handler,
				  NULL) != 0) {
		return 0;
	}
	return g_signal_connect(component, "focus-event",
				G_CALLBACK(handler), NULL);
}

static void
vte_terminal_accessible_remove_focus_handler(AtkComponent *component,
					     guint handler_id)
{
	g_assert(g_signal_handler_is_connected(component, handler_id));
	g_signal_handler_disconnect(component, handler_id);
}

static void
vte_terminal_accessible_component_init(gpointer iface, gpointer data)
{
	AtkComponentIface *component;
	g_assert(G_TYPE_FROM_INTERFACE(iface) == ATK_TYPE_COMPONENT);
	component = iface;

	_vte_debug_print(VTE_DEBUG_ALLY,
			"Initializing accessible peer's "
			"AtkComponent interface.\n");
	/* Set our virtual functions. */
	component->add_focus_handler = vte_terminal_accessible_add_focus_handler;
	component->contains = vte_terminal_accessible_contains;
	component->ref_accessible_at_point = vte_terminal_accessible_ref_accessible_at_point;
	component->get_extents = vte_terminal_accessible_get_extents;
	component->get_position = vte_terminal_accessible_get_position;
	component->get_size = vte_terminal_accessible_get_size;
	component->remove_focus_handler = vte_terminal_accessible_remove_focus_handler;
	component->set_extents = vte_terminal_accessible_set_extents;
	component->set_position = vte_terminal_accessible_set_position;
	component->set_size = vte_terminal_accessible_set_size;
	component->get_layer = vte_terminal_accessible_get_layer;
	component->get_mdi_zorder = vte_terminal_accessible_get_mdi_zorder;
}

/* AtkAction interface */

static gboolean
vte_terminal_accessible_do_action (AtkAction *accessible, int i)
{
	GtkWidget *widget;
	gboolean retval = FALSE;

	g_return_val_if_fail (i < LAST_ACTION, FALSE);

	widget = gtk_accessible_get_widget (GTK_ACCESSIBLE (accessible));
	if (!widget) {
		return FALSE;
	}

        switch (i) {
        case ACTION_MENU :
		g_signal_emit_by_name (widget, "popup_menu", &retval);
                break;
        default :
                g_warning ("Invalid action passed to VteTerminalAccessible::do_action");
                return FALSE;
        }
        return retval;
}

static int
vte_terminal_accessible_get_n_actions (AtkAction *accessible)
{
	return LAST_ACTION;
}

static const char *
vte_terminal_accessible_action_get_description (AtkAction *accessible, int i)
{
        VteTerminalAccessiblePrivate *priv;

        g_return_val_if_fail (i < LAST_ACTION, NULL);

	g_return_val_if_fail(VTE_IS_TERMINAL_ACCESSIBLE(accessible), NULL);

	/* Retrieve the private data structure.  It must already exist. */
	priv = g_object_get_data(G_OBJECT(accessible),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	g_return_val_if_fail(priv != NULL, NULL);

        if (priv->action_descriptions[i]) {
                return priv->action_descriptions[i];
        } else {
                return vte_terminal_accessible_action_descriptions[i];
        }
}

static const char *
vte_terminal_accessible_action_get_name (AtkAction *accessible, int i)
{
        g_return_val_if_fail (i < LAST_ACTION, NULL);

        return vte_terminal_accessible_action_names[i];
}

static const char *
vte_terminal_accessible_action_get_keybinding (AtkAction *accessible, int i)
{
        g_return_val_if_fail (i < LAST_ACTION, NULL);

        return NULL;
}

static gboolean
vte_terminal_accessible_action_set_description (AtkAction *accessible,
                                                int i,
                                                const char *description)
{
        VteTerminalAccessiblePrivate *priv;

        g_return_val_if_fail (i < LAST_ACTION, FALSE);

	g_return_val_if_fail(VTE_IS_TERMINAL_ACCESSIBLE(accessible), FALSE);

	/* Retrieve the private data structure.  It must already exist. */
	priv = g_object_get_data(G_OBJECT(accessible),
				 VTE_TERMINAL_ACCESSIBLE_PRIVATE_DATA);
	g_return_val_if_fail(priv != NULL, FALSE);

        if (priv->action_descriptions[i]) {
                g_free (priv->action_descriptions[i]);
        }
        priv->action_descriptions[i] = g_strdup (description);

        return TRUE;
}

static void
vte_terminal_accessible_action_init(gpointer iface, gpointer data)
{
	AtkActionIface *action;
	g_return_if_fail(G_TYPE_FROM_INTERFACE(iface) == ATK_TYPE_ACTION);
	action = iface;

	_vte_debug_print(VTE_DEBUG_ALLY,
			"Initializing accessible peer's "
			"AtkAction interface.\n");
	/* Set our virtual functions. */
	action->do_action = vte_terminal_accessible_do_action;
	action->get_n_actions = vte_terminal_accessible_get_n_actions;
	action->get_description = vte_terminal_accessible_action_get_description;
	action->get_name = vte_terminal_accessible_action_get_name;
	action->get_keybinding = vte_terminal_accessible_action_get_keybinding;
	action->set_description = vte_terminal_accessible_action_set_description;
}
static void
vte_terminal_accessible_class_init(gpointer *klass)
{
	GObjectClass *gobject_class;
	AtkObjectClass *class = ATK_OBJECT_CLASS (klass);

	vte_terminal_accessible_parent_class = g_type_class_peek_parent (klass);

	gobject_class = G_OBJECT_CLASS(klass);

	class->initialize = vte_terminal_initialize;
	/* Override the finalize method. */
	gobject_class->finalize = vte_terminal_accessible_finalize;
}

GType
vte_terminal_accessible_get_type(void)
{
	static GType terminal_accessible_type = 0;

	if (G_UNLIKELY (terminal_accessible_type == 0)) {
		AtkRegistry *registry;
		AtkObjectFactory *factory;
		GType parent_type, parent_accessible_type;
		GTypeQuery type_info;

		GInterfaceInfo text = {
			vte_terminal_accessible_text_init,
			NULL,
			NULL,
		};
		GInterfaceInfo component = {
			vte_terminal_accessible_component_init,
			NULL,
			NULL,
		};
		GInterfaceInfo action = {
			vte_terminal_accessible_action_init,
			NULL,
			NULL,
		};
		GTypeInfo terminal_accessible_info = {
			0,
			(GBaseInitFunc)NULL,
			(GBaseFinalizeFunc)NULL,

			(GClassInitFunc)vte_terminal_accessible_class_init,
			(GClassFinalizeFunc)NULL,
			(gconstpointer)NULL,

			0,
			0,
			(GInstanceInitFunc) NULL,

			(GTypeValueTable*)NULL,
		};

		/* Find the Atk object used for the parent (GtkWidget) type. */
		parent_type = g_type_parent(VTE_TYPE_TERMINAL);
		factory = atk_registry_get_factory(atk_get_default_registry(),
				parent_type);
		parent_accessible_type = atk_object_factory_get_accessible_type(factory);
		if (!g_type_is_a(parent_accessible_type, GTK_TYPE_ACCESSIBLE)) {
#ifdef VTE_DEBUG
			g_warning("Accessibility (%s) is not derived from "
					"%s (GTK_MODULES=gail not set?), "
					"deriving from %s instead.\n",
					g_type_name(parent_accessible_type),
					g_type_name(GTK_TYPE_ACCESSIBLE),
					g_type_name(GTK_TYPE_ACCESSIBLE));
#endif
			/* Fudge it. */
			parent_accessible_type = GTK_TYPE_ACCESSIBLE;
		}

		/* Find the size of the parent type's objects. */
		g_type_query(parent_accessible_type, &type_info);
		terminal_accessible_info.class_size = type_info.class_size;
		terminal_accessible_info.instance_size = type_info.instance_size;
		/* Register the class with the GObject type system. */
		terminal_accessible_type = g_type_register_static(parent_accessible_type,
				"VteTerminalAccessible",
				&terminal_accessible_info,
				0);

		/* Add a text interface to this object class. */
		g_type_add_interface_static(terminal_accessible_type,
				ATK_TYPE_TEXT,
				&text);
		/* Add a component interface to this object class. */
		g_type_add_interface_static(terminal_accessible_type,
				ATK_TYPE_COMPONENT,
				&component);
		/* Add an action interface to this object class. */
		g_type_add_interface_static(terminal_accessible_type,
				ATK_TYPE_ACTION,
				&action);

		/* Associate the terminal and its peer factory in the
		 * Atk type registry. */
		registry = atk_get_default_registry();
		atk_registry_set_factory_type(registry,
				VTE_TYPE_TERMINAL,
				VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY);
	}

	return terminal_accessible_type;
}

/* Create an accessible peer for the object. */
static AtkObject *
vte_terminal_accessible_factory_create_accessible(GObject *obj)
{
	GtkAccessible *accessible;
	VteTerminal *terminal;

	g_assert(VTE_IS_TERMINAL(obj));

	terminal = VTE_TERMINAL(obj);
	accessible = GTK_ACCESSIBLE(vte_terminal_accessible_new(terminal));
	g_assert(accessible != NULL);

	return ATK_OBJECT(accessible);
}

static void
vte_terminal_accessible_factory_class_init(VteTerminalAccessibleFactoryClass *klass)
{
	AtkObjectFactoryClass *class = ATK_OBJECT_FACTORY_CLASS(klass);
	/* Override the one method we care about. */
	class->create_accessible = vte_terminal_accessible_factory_create_accessible;
}
static void
vte_terminal_accessible_factory_init(VteTerminalAccessibleFactory *self)
{
	/* nothing to initialise */
}

AtkObjectFactory *
vte_terminal_accessible_factory_new(void)
{
	_vte_debug_print(VTE_DEBUG_ALLY,
			"Creating a new VteTerminalAccessibleFactory.\n");
	return g_object_new(VTE_TYPE_TERMINAL_ACCESSIBLE_FACTORY, NULL);
}

static gint
compare_matches(gconstpointer a, gconstpointer b)
{
	const struct _vte_regex_match *A, *B;
	A = a;
	B = b;
	if (B->rm_so != A->rm_so) {
		return B->rm_so - A->rm_so;
	}
	return B->rm_eo - A->rm_eo;
}

/* Sort match structures first by starting position, and then by ending
 * position.  We do this because some expression matching APIs sort their
 * results differently, or just plain don't sort them. */
static void
_vte_regex_sort_matches(struct _vte_regex_match *matches, gsize n_matches)
{
	GArray *array;
	if (n_matches <= 1) {
		return;
	}
	array = g_array_new(FALSE, FALSE, sizeof(struct _vte_regex_match));
	g_array_append_vals(array, matches, n_matches);
	g_array_sort(array, compare_matches);
	memmove(matches, array->data,
		n_matches * sizeof(struct _vte_regex_match));
	g_array_free(array, TRUE);
}

/* Ah, POSIX regex.  Kind of clunky, but I don't have anything better to
 * suggest.  Better still, it works on my machine. */

struct _vte_regex {
	regex_t posix_regex;
};

struct _vte_regex *
_vte_regex_compile(const char *pattern)
{
	struct _vte_regex *ret;
	int i;

	ret = g_slice_new(struct _vte_regex);
	i = regcomp(&ret->posix_regex, pattern, REG_EXTENDED);
	if (i != 0) {
		g_slice_free(struct _vte_regex, ret);
		return NULL;
	}
	return ret;
}

void
_vte_regex_free(struct _vte_regex *regex)
{
	regfree(&regex->posix_regex);
	g_slice_free(struct _vte_regex, regex);
}

int
_vte_regex_exec(struct _vte_regex *regex, const char *string,
		gsize nmatch, struct _vte_regex_match *matches)
{
	regmatch_t *posix_matches;
	guint i, ret;

	posix_matches = g_new(regmatch_t, nmatch);
	ret = regexec(&regex->posix_regex, string, nmatch, posix_matches, 0);
	if (ret == 0) {
		for (i = 0; i < nmatch; i++) {
			matches[i].rm_so = -1;
			matches[i].rm_eo = -1;
		}
		for (i = 0; i < nmatch; i++) {
			matches[i].rm_so = posix_matches[i].rm_so;
			matches[i].rm_eo = posix_matches[i].rm_eo;
			if (matches[i].rm_so == -1) {
				_vte_regex_sort_matches(matches, i);
				break;
			}
		}
	}
	g_free(posix_matches);
	if (ret == 0) {
		return 0;
	}
	return -1;
}
