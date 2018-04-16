/*
 * Copyright (C) 2002,2009,2010 Red Hat, Inc.
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
 *
 * Red Hat Author(s): Behdad Esfahbod
 */

/* The interfaces in this file are subject to change at any time. */

#ifndef vte_ring_h_included
#define vte_ring_h_included

#include <glib-object.h>
#include <gio/gio.h>

#include "vte.h"


G_BEGIN_DECLS

void _vte_stream_reset (VteStream *stream, gsize offset);
gsize _vte_stream_append (VteStream *stream, const char *data, gsize len);
gboolean _vte_stream_read (VteStream *stream, gsize offset, char *data, gsize len);
void _vte_stream_truncate (VteStream *stream, gsize offset);
void _vte_stream_new_page (VteStream *stream);
gsize _vte_stream_head (VteStream *stream);
gboolean _vte_stream_write_contents (VteStream *stream, GOutputStream *output,
				     gsize start_offset,
				     GCancellable *cancellable, GError **error);

/* Various streams */

VteStream *
_vte_file_stream_new (void);
static inline const VteCell *
_vte_row_data_get (const VteRowData *row, gulong col)
{
	if (G_UNLIKELY (row->len <= col))
		return NULL;

	return &row->cells[col];
}

static inline VteCell *
_vte_row_data_get_writable (VteRowData *row, gulong col)
{
	if (G_UNLIKELY (row->len <= col))
		return NULL;

	return &row->cells[col];
}

void _vte_row_data_init (VteRowData *row);
void _vte_row_data_clear (VteRowData *row);
void _vte_row_data_fini (VteRowData *row);
void _vte_row_data_insert (VteRowData *row, gulong col, const VteCell *cell);
void _vte_row_data_append (VteRowData *row, const VteCell *cell);
void _vte_row_data_remove (VteRowData *row, gulong col);
void _vte_row_data_fill (VteRowData *row, const VteCell *cell, gulong len);
void _vte_row_data_shrink (VteRowData *row, gulong max_len);

const VteRowData *_vte_ring_index (VteRing *ring, gulong position);
VteRowData *_vte_ring_index_writable (VteRing *ring, gulong position);

void _vte_ring_init (VteRing *ring, gulong max_rows);
void _vte_ring_fini (VteRing *ring);
void _vte_ring_resize (VteRing *ring, gulong max_rows);
void _vte_ring_shrink (VteRing *ring, gulong max_len);
VteRowData *_vte_ring_insert (VteRing *ring, gulong position);
VteRowData *_vte_ring_append (VteRing *ring);
void _vte_ring_remove (VteRing *ring, gulong position);
gboolean _vte_ring_write_contents (VteRing *ring,
				   GOutputStream *stream,
				   VteTerminalWriteFlags flags,
				   GCancellable *cancellable,
				   GError **error);

G_END_DECLS

#endif
