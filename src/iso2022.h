/*
 * Copyright (C) 2002 Red Hat, Inc.
 * Copyright (C) 2004 Benjamin Otte <otte@gnome.org>
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

/* The interfaces in this file are subject to change at any time. */

#ifndef vte_iso2022_h_included
#define vte_iso2022_h_included


#include <glib.h>
#include <glib-object.h>
#include "buffer.h"


#define VTE_TREE_ARRAY_SIZE (128)


G_BEGIN_DECLS

struct _vte_termcap;
struct _vte_matcher;
struct _vte_trie;
struct _vte_table;
struct _vte_iso2022_state;
typedef struct _VteTree VteTree;

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


#define VTE_ISO2022_ENCODED_WIDTH_BIT_OFFSET	28
#define VTE_ISO2022_ENCODED_WIDTH_MASK		(3 << VTE_ISO2022_ENCODED_WIDTH_BIT_OFFSET)
#define VTE_ISO2022_HAS_ENCODED_WIDTH(__c)	(((__c) & VTE_ISO2022_ENCODED_WIDTH_MASK) != 0)
int _vte_iso2022_get_encoded_width(gunichar c);
int _vte_iso2022_unichar_width(struct _vte_iso2022_state *state,
			       gunichar c);

G_END_DECLS

#endif
