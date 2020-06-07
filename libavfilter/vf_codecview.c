/*
 * Copyright (c) 2002-2004 Michael Niedermayer <michaelni@gmx.at>
 * Copyright (c) 2014 Clément Bœsch <u pkh me>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * Codec debug viewer filter.
 *
 * All the MV drawing code from Michael Niedermayer is extracted from
 * libavcodec/mpegvideo.c.
 *
 * TODO: segmentation
 */

#include "libavutil/imgutils.h"
#include "libavutil/motion_vector.h"
#include "libavutil/opt.h"
#include "avfilter.h"
#include "internal.h"

#define MV_P_FOR  (1<<0)
#define MV_B_FOR  (1<<1)
#define MV_B_BACK (1<<2)
#define MV_TYPE_FOR  (1<<0)
#define MV_TYPE_BACK (1<<1)
#define FRAME_TYPE_I (1<<0)
#define FRAME_TYPE_P (1<<1)
#define FRAME_TYPE_B (1<<2)

typedef struct CodecViewContext {
    const AVClass *class;
    unsigned mv;
    unsigned frame_type;
    unsigned mv_type;
    int hsub, vsub;
    int qp;
    int intra4x4;
    int intra16x16;
    int inter;
    int skip;
} CodecViewContext;

#define OFFSET(x) offsetof(CodecViewContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM
#define CONST(name, help, val, unit) { name, help, 0, AV_OPT_TYPE_CONST, {.i64=val}, 0, 0, FLAGS, unit }

static const AVOption codecview_options[] = {
    { "mv", "set motion vectors to visualize", OFFSET(mv), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "mv" },
        CONST("pf", "forward predicted MVs of P-frames",  MV_P_FOR,  "mv"),
        CONST("bf", "forward predicted MVs of B-frames",  MV_B_FOR,  "mv"),
        CONST("bb", "backward predicted MVs of B-frames", MV_B_BACK, "mv"),
    { "qp", NULL, OFFSET(qp), AV_OPT_TYPE_BOOL, {.i64=0}, 0, 1, .flags = FLAGS },
    { "mv_type", "set motion vectors type", OFFSET(mv_type), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "mv_type" },
    { "mvt",     "set motion vectors type", OFFSET(mv_type), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "mv_type" },
        CONST("fp", "forward predicted MVs",  MV_TYPE_FOR,  "mv_type"),
        CONST("bp", "backward predicted MVs", MV_TYPE_BACK, "mv_type"),
    { "frame_type", "set frame types to visualize motion vectors of", OFFSET(frame_type), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "frame_type" },
    { "ft",         "set frame types to visualize motion vectors of", OFFSET(frame_type), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "frame_type" },
        CONST("if", "I-frames", FRAME_TYPE_I, "frame_type"),
        CONST("pf", "P-frames", FRAME_TYPE_P, "frame_type"),
        CONST("bf", "B-frames", FRAME_TYPE_B, "frame_type"),
    { "intra4x4", "draw intra prediction 4x4", OFFSET(intra4x4), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "rectangle_draw_type" },
    { "intra16x16", "draw intra prediction 16x16", OFFSET(intra16x16), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "rectangle_draw_type" },
    { "inter", "draw inter prediction", OFFSET(inter), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "rectangle_draw_type" },
    { "skip", "draw inter prediction", OFFSET(skip), AV_OPT_TYPE_FLAGS, {.i64=0}, 0, INT_MAX, FLAGS, "rectangle_draw_type" },
        CONST("rect", "rectangle",  1,  "rectangle_draw_type"),
        CONST("fill", "filled rectangle",  2,  "rectangle_draw_type"),
    { NULL }
};

AVFILTER_DEFINE_CLASS(codecview);

static int query_formats(AVFilterContext *ctx)
{
    // TODO: we can probably add way more pixel formats without any other
    // changes; anything with 8-bit luma in first plane should be working
    static const enum AVPixelFormat pix_fmts[] = {AV_PIX_FMT_YUV420P, AV_PIX_FMT_NONE};
    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

static int clip_line(int *sx, int *sy, int *ex, int *ey, int maxx)
{
    if(*sx > *ex)
        return clip_line(ex, ey, sx, sy, maxx);

    if (*sx < 0) {
        if (*ex < 0)
            return 1;
        *sy = *ey + (*sy - *ey) * (int64_t)*ex / (*ex - *sx);
        *sx = 0;
    }

    if (*ex > maxx) {
        if (*sx > maxx)
            return 1;
        *ey = *sy + (*ey - *sy) * (int64_t)(maxx - *sx) / (*ex - *sx);
        *ex = maxx;
    }
    return 0;
}

/**
 * Draw a line from (ex, ey) -> (sx, sy).
 * @param w width of the image
 * @param h height of the image
 * @param stride stride/linesize of the image
 * @param color color of the arrow
 */
static void draw_line(uint8_t *buf, int sx, int sy, int ex, int ey,
                      int w, int h, int stride, int color)
{
    int x, y, fr, f;

    if (clip_line(&sx, &sy, &ex, &ey, w - 1))
        return;
    if (clip_line(&sy, &sx, &ey, &ex, h - 1))
        return;

    sx = av_clip(sx, 0, w - 1);
    sy = av_clip(sy, 0, h - 1);
    ex = av_clip(ex, 0, w - 1);
    ey = av_clip(ey, 0, h - 1);

    buf[sy * stride + sx] += color;

    if (FFABS(ex - sx) > FFABS(ey - sy)) {
        if (sx > ex) {
            FFSWAP(int, sx, ex);
            FFSWAP(int, sy, ey);
        }
        buf += sx + sy * stride;
        ex  -= sx;
        f    = ((ey - sy) << 16) / ex;
        for (x = 0; x <= ex; x++) {
            y  = (x * f) >> 16;
            fr = (x * f) & 0xFFFF;
                   buf[ y      * stride + x] += (color * (0x10000 - fr)) >> 16;
            if(fr) buf[(y + 1) * stride + x] += (color *            fr ) >> 16;
        }
    } else {
        if (sy > ey) {
            FFSWAP(int, sx, ex);
            FFSWAP(int, sy, ey);
        }
        buf += sx + sy * stride;
        ey  -= sy;
        if (ey)
            f = ((ex - sx) << 16) / ey;
        else
            f = 0;
        for(y= 0; y <= ey; y++){
            x  = (y*f) >> 16;
            fr = (y*f) & 0xFFFF;
                   buf[y * stride + x    ] += (color * (0x10000 - fr)) >> 16;
            if(fr) buf[y * stride + x + 1] += (color *            fr ) >> 16;
        }
    }
}

/**
 * Draw an arrow from (ex, ey) -> (sx, sy).
 * @param w width of the image
 * @param h height of the image
 * @param stride stride/linesize of the image
 * @param color color of the arrow
 */
static void draw_arrow(uint8_t *buf, int sx, int sy, int ex,
                       int ey, int w, int h, int stride, int color, int tail, int direction)
{
    int dx,dy;

    if (direction) {
        FFSWAP(int, sx, ex);
        FFSWAP(int, sy, ey);
    }

    sx = av_clip(sx, -100, w + 100);
    sy = av_clip(sy, -100, h + 100);
    ex = av_clip(ex, -100, w + 100);
    ey = av_clip(ey, -100, h + 100);

    dx = ex - sx;
    dy = ey - sy;

    if (dx * dx + dy * dy > 3 * 3) {
        int rx =  dx + dy;
        int ry = -dx + dy;
        int length = sqrt((rx * rx + ry * ry) << 8);

        // FIXME subpixel accuracy
        rx = ROUNDED_DIV(rx * 3 << 4, length);
        ry = ROUNDED_DIV(ry * 3 << 4, length);

        if (tail) {
            rx = -rx;
            ry = -ry;
        }

        draw_line(buf, sx, sy, sx + rx, sy + ry, w, h, stride, color);
        draw_line(buf, sx, sy, sx - ry, sy + rx, w, h, stride, color);
    }
    draw_line(buf, sx, sy, ex, ey, w, h, stride, color);
}

/**
 * Draw an rectangle at (sx, sy) size (width, height)
 * @param w width of the image
 * @param h height of the image
 * @param stride stride/linesize of the image
 * @param color color of the rectangle
 */
static void draw_rectangle(uint8_t **buf, int sx, int sy, int width, int height ,
                        int w, int h, int *stride, uint8_t color_y, uint8_t color_u, uint8_t color_v, int fill)
{
    if (sx + width > w){
        width = w - sx;
    }
    if (sy + height > h){
        height = h - sy;
    }

    if (fill){
        for (size_t y = sy; y < sy + height; y++)
        {
            for (size_t x = sx; x < sx + width; x++)
            {
                buf[0][y * stride[0] + x] = color_y;
                buf[1][y / 2 * stride[1] + x / 2] = color_u;
                buf[2][y / 2 * stride[2] + x / 2] = color_v;
            }
        }
    }else{
        for (size_t y = sy; y < sy + height; y++)
        {
            buf[0][y * stride[0] + sx] = color_y;
            buf[1][y / 2 * stride[1] + sx / 2] = color_u;
            buf[2][y / 2 * stride[2] + sx / 2] = color_v;
            buf[0][y * stride[0] + sx + width - 1] = color_y;
            buf[1][y / 2 * stride[1] + (sx + width - 1) / 2] = color_u;
            buf[2][y / 2 * stride[2] + (sx + width - 1) / 2] = color_v;
        }
        for (size_t x = sx; x < sx + width; x++)
        {
            buf[0][sy * stride[0] + x] = color_y;
            buf[1][sy / 2 * stride[1] + x / 2] = color_u;
            buf[2][sy / 2 * stride[2] + x / 2] = color_v;
            buf[0][(sy + height - 1) * stride[0] + x] = color_y;
            buf[1][(sy + height - 1) / 2 * stride[1] + x / 2] = color_u;
            buf[2][(sy + height - 1) / 2 * stride[2] + x / 2] = color_v;
        }
    }
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    AVFilterContext *ctx = inlink->dst;
    CodecViewContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

    if (s->qp) {
        int qstride, qp_type;
        int8_t *qp_table = av_frame_get_qp_table(frame, &qstride, &qp_type);

        if (qp_table) {
            int x, y;
            const int w = AV_CEIL_RSHIFT(frame->width,  s->hsub);
            const int h = AV_CEIL_RSHIFT(frame->height, s->vsub);
            uint8_t *pu = frame->data[1];
            uint8_t *pv = frame->data[2];
            const int lzu = frame->linesize[1];
            const int lzv = frame->linesize[2];

            for (y = 0; y < h; y++) {
                for (x = 0; x < w; x++) {
                    const int qp = ff_norm_qscale(qp_table[(y >> 3) * qstride + (x >> 3)], qp_type) * 128/31;
                    pu[x] = pv[x] = qp;
                }
                pu += lzu;
                pv += lzv;
            }
        }
    }

    if (s->mv || s->mv_type) {
        AVFrameSideData *sd = av_frame_get_side_data(frame, AV_FRAME_DATA_MOTION_VECTORS);
        if (sd) {
            int i;
            const AVMotionVector *mvs = (const AVMotionVector *)sd->data;
            const int is_iframe = (s->frame_type & FRAME_TYPE_I) && frame->pict_type == AV_PICTURE_TYPE_I;
            const int is_pframe = (s->frame_type & FRAME_TYPE_P) && frame->pict_type == AV_PICTURE_TYPE_P;
            const int is_bframe = (s->frame_type & FRAME_TYPE_B) && frame->pict_type == AV_PICTURE_TYPE_B;

            for (i = 0; i < sd->size / sizeof(*mvs); i++) {
                const AVMotionVector *mv = &mvs[i];
                const int direction = mv->source > 0;

                if (s->mv_type) {
                    const int is_fp = direction == 0 && (s->mv_type & MV_TYPE_FOR);
                    const int is_bp = direction == 1 && (s->mv_type & MV_TYPE_BACK);

                    if ((!s->frame_type && (is_fp || is_bp)) ||
                        is_iframe && is_fp || is_iframe && is_bp ||
                        is_pframe && is_fp ||
                        is_bframe && is_fp || is_bframe && is_bp)
                        draw_arrow(frame->data[0], mv->dst_x, mv->dst_y, mv->src_x, mv->src_y,
                                   frame->width, frame->height, frame->linesize[0],
                                   100, 0, direction);
                } else if (s->mv)
                    if ((direction == 0 && (s->mv & MV_P_FOR)  && frame->pict_type == AV_PICTURE_TYPE_P) ||
                        (direction == 0 && (s->mv & MV_B_FOR)  && frame->pict_type == AV_PICTURE_TYPE_B) ||
                        (direction == 1 && (s->mv & MV_B_BACK) && frame->pict_type == AV_PICTURE_TYPE_B))
                        draw_arrow(frame->data[0], mv->dst_x, mv->dst_y, mv->src_x, mv->src_y,
                                   frame->width, frame->height, frame->linesize[0],
                                   100, 0, direction);
            }
        }
    }

    if (s->inter){
        AVFrameSideData *sd = av_frame_get_side_data(frame, AV_FRAME_DATA_MOTION_VECTORS);
        if (sd) {
            const AVMotionVector *mvs = (const AVMotionVector *)sd->data;
            for (int i = 0; i < sd->size / sizeof(*mvs); i++) {
                const AVMotionVector *mv = &mvs[i];
                draw_rectangle(frame->data, mv->dst_x - mv->w / 2, mv->dst_y - mv->h / 2, mv->w, mv->h, 
                    frame->width, frame->height, frame->linesize, 128, 255, 0, s->inter - 1);
            }
        }
    }

    if (s->intra4x4 || s->intra16x16 || s->skip){
        AVFrameSideData *sd = av_frame_get_side_data(frame, AV_FRAME_DATA_MACRO_BLOCK_TYPES);
        if (sd) {
            const mb_stride = frame->width / 16 + 1;
            for (size_t mb_y = 0; mb_y < frame->height / 16 ; mb_y++)
            {
                for (size_t mb_x = 0; mb_x < frame->width / 16 ; mb_x++)
                {
                    const mb_type = ((uint32_t*)(sd->data))[mb_x + mb_y * mb_stride];
                    if(mb_type & 1){
                        // intra 4x4
                        if (s->intra4x4){
                            for (size_t submb_y = 0; submb_y < 2; submb_y++)
                            {
                                for (size_t submb_x = 0; submb_x < 2; submb_x++)
                                {
                                    draw_rectangle(frame->data, mb_x * 16 + submb_x * 8, mb_y * 16 + submb_y * 8, 8, 8
                                        ,frame->width, frame->height, frame->linesize, 128, 0, 255, s->intra4x4 - 1);
                                }
                            }
                        }
                    }else if(mb_type & (1 << 1)){
                        // intra 16x16
                        if (s->intra16x16){
                            draw_rectangle(frame->data, mb_x * 16, mb_y * 16, 16, 16
                                ,frame->width, frame->height, frame->linesize, 128, 0, 255, s->intra16x16 - 1);
                        }
                    }else if(mb_type & (1 << 11)){
                        // inter skip
                        if (s->skip){
                            draw_rectangle(frame->data, mb_x * 16, mb_y * 16, 16, 16
                                ,frame->width, frame->height, frame->linesize, 200, 128, 128, s->skip - 1);
                        }
                    }else if(mb_type & ((1 <<  3) |(1 <<  4) |(1 <<  5) |(1 <<  6))){

                    }
                }
            }
        }
    }

    return ff_filter_frame(outlink, frame);
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    CodecViewContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    s->hsub = desc->log2_chroma_w;
    s->vsub = desc->log2_chroma_h;
    return 0;
}

static const AVFilterPad codecview_inputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
        .filter_frame   = filter_frame,
        .config_props   = config_input,
        .needs_writable = 1,
    },
    { NULL }
};

static const AVFilterPad codecview_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_codecview = {
    .name          = "codecview",
    .description   = NULL_IF_CONFIG_SMALL("Visualize information about some codecs."),
    .priv_size     = sizeof(CodecViewContext),
    .query_formats = query_formats,
    .inputs        = codecview_inputs,
    .outputs       = codecview_outputs,
    .priv_class    = &codecview_class,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC,
};
