/*
 * Copyright (C) 2002,2003 Red Hat, Inc.
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

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <sys/types.h>

#include "debug.h"
#include "buffer.h"
#include "iso2022.h"
#include "vteconv.h"
#include "caps.h"

#include <gtk/gtk.h>
#include <glib/gi18n-lib.h>

#if GTK_CHECK_VERSION (2, 90, 7)
#define GDK_KEY(symbol) GDK_KEY_##symbol
#else
#include <gdk/gdkkeysyms.h>
#define GDK_KEY(symbol) GDK_##symbol
#endif

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
