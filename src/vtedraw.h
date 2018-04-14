/*
 * Copyright (C) 2003 Red Hat, Inc.
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

#ifndef vte_vtedraw_h_included
#define vte_vtedraw_h_included


#include <glib.h>
#include <gtk/gtk.h>
#include "vte.h"
#include "vteunistr.h"
#include "vte-gtk-compat.h"

G_BEGIN_DECLS

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

#define VTE_TYPE_BG            (vte_bg_get_type())
#define VTE_BG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), VTE_TYPE_BG, VteBg))
#define VTE_BG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  VTE_TYPE_BG, VteBgClass))
#define VTE_IS_BG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), VTE_TYPE_BG))
#define VTE_IS_BG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  VTE_TYPE_BG))
#define VTE_BG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  VTE_TYPE_BG, VteBgClass))


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

G_END_DECLS

#endif
