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
#include "vteunistr.h"
#include "vtestream.h"


#define VTE_DEF_FG			256
#define VTE_DEF_BG			257
#define VTE_BOLD_FG			258
#define VTE_DIM_FG			259
#define VTE_DEF_HL                      260
#define VTE_CUR_BG			261
#define VTE_PALETTE_SIZE		262


G_BEGIN_DECLS

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
