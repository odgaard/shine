package apps

import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.HighLevelConstructs._

// algorithm taken from Halide (halide-lang.org)
object cameraPipe {
  // iN -> iN -> uN
  val abs_diff = foreignFun("abs_diff", i16 ->: i16 ->: u16)
  val clamp = dtFun(dt => foreignFun("clamp", dt ->: dt ->: dt ->: dt))
  val max = foreignFun("max", i16 ->: i16 ->: i16)
  val pow = foreignFun("pow", f32 ->: f32 ->: f32)

  def li16(v: Int) = cast(l(v)) :: i16
  def li32(v: Int) = cast(l(v)) :: i32
  def lu8(v: Int) = cast(l(v)) :: u8

  // average two positive values rounding up
  val avg: Expr = implDT(dt => dtFun(compute_dt => fun(
    dt ->: dt ->: dt
  )((a, b) => {
    val cast_ = fun(a => cast(a) :: compute_dt)
    // FIXME: avoid literal casts?
    cast((cast_(a) + cast_(b) + cast_(l(1))) / cast_(l(2))) :: dt
  })))

  case class Image(xBeg: Nat, xEnd: Nat,
                   yBeg: Nat, yEnd: Nat,
                   expr: Expr)

  def letImage(img: Image, f: Image => Expr): Expr =
    img.expr |> fun(x =>
      f(Image(img.xBeg, img.xEnd, img.yBeg, img.yEnd, x))
    )

  def mapImage(img: Image, f: Expr): Image =
    Image(img.xBeg, img.xEnd, img.yBeg, img.yEnd, img.expr |> map(map(f)))

  def zipImage(a: Image, b: Image): Image =
    // TODO: check a and b regions?
    Image(a.xBeg, a.xEnd, a.yBeg, a.yEnd, zipND(2)(a.expr, b.expr))

  // point-wise avg
  def interpolate(img: Image): Image = mapImage(img, fun(2`.`i16)(x =>
    avg(i32)(x `@` lidx(0, 2), x `@` lidx(1, 2))
  ))
  // point-wise abs_diff
  def point_abs_diff(img: Image): Image = mapImage(img, fun(2`.`i16)(x =>
    abs_diff(x `@` lidx(0, 2), x `@` lidx(1, 2))
  ))

  def slice(beg: Nat, end: Nat, newBeg: Nat, newEnd: Nat): Expr = {
    /* if (newBeg < beg || end < newEnd) {
      throw new Exception("slice out of bounds")
    } */
    // if (newBeg > beg)
    // if (end > newEnd)
    drop(newBeg - beg) >> take(newEnd - newBeg)
  }

  def intersectImages(images: Seq[Image]): Seq[Image] = {
    import arithexpr.arithmetic.ArithExpr.Math.Max
    import arithexpr.arithmetic.ArithExpr.Math.Min

    val xBeg = images.map(_.xBeg).reduce(Max)
    val yBeg = images.map(_.yBeg).reduce(Max)
    val xEnd = images.map(_.xEnd).reduce(Min)
    val yEnd = images.map(_.yEnd).reduce(Min)

    images.map(img => {
      val xSlice = slice(img.xBeg, img.xEnd, xBeg, xEnd)
      val ySlice = slice(img.yBeg, img.yEnd, yBeg, yEnd)
      val sliced = img.expr |> map(xSlice) |> ySlice
      Image(xBeg, xEnd, yBeg, yEnd, sliced)
    })
  }

  def stencilCollect(offsets: Seq[(Int, Int)], img: Image): Image = {
    val n = offsets.length
    val x_min = offsets.map(_._1).min
    val x_max = offsets.map(_._1).max
    val x_range = x_max - x_min + 1
    val y_min = offsets.map(_._2).min
    val y_max = offsets.map(_._2).max
    val y_range = y_max - y_min + 1

    mapImage(
      Image(
        img.xBeg - x_min, img.xEnd - x_max,
        img.yBeg - y_min, img.yEnd - y_max,
        img.expr |> slide2D(y_range, 1, x_range, 1)
      ),
      fun(nbh =>
        offsets.foldLeft(makeArray(n): Expr)({ case (e, offset) =>
          e(nbh `@`
            lidx(offset._2 - y_min, y_range) `@`
            lidx(offset._1 - x_min, x_range))
        })
      )
    )
  }

  val demosaic: Expr = implN(ho => implN(wo => nFun(h => nFun(w => fun(
    (4`.`h`.`w`.`i16) ->: (3`.`ho`.`wo`.`i16)
  )(deinterleaved => {
    // x_y = the value of channel x at a site in the input of channel y
    // gb = green sites in the blue rows
    // gr = green sites in the red rows
    // | gr r |    | r_gr r_r |   | g_gr g_r |   | b_gr b_r |
    // | b gb | -> | r_b r_gb | , | g_b g_gb | , | b_b b_gb |
    val g_gr = Image(0, w, 0, h, deinterleaved `@` lidx(0, 4))
    val r_r = Image(0, w, 0, h, deinterleaved `@` lidx(1, 4))
    val b_b = Image(0, w, 0, h, deinterleaved `@` lidx(2, 4))
    val g_gb = Image(0, w, 0, h, deinterleaved `@` lidx(3, 4))

    // first calculate green at the red and blue sites

    def select_interpolation(a_ : Image, ad_ : Image, b_ : Image, bd_ : Image): Image = {
      val Seq(a, ad, b, bd) = intersectImages(Seq(a_, ad_, b_, bd_))
      mapImage(zipImage(zipImage(a, ad), zipImage(b, bd)), fun(ab =>
        select(snd(fst(ab)) < snd(snd(ab)), fst(fst(ab)), fst(snd(ab)))
      ))
    }

    def correct(a_ : Image, b_ : Image, c_ : Image): Image = {
      val Seq(a, b, c) = intersectImages(Seq(a_, b_, c_))
      mapImage(zipImage(a, zipImage(b, c)), fun(a_bc =>
        fst(a_bc) + (fst(snd(a_bc)) - snd(snd(a_bc)))
      ))
    }

    // try interpolating vertically and horizontally
    // use interpolation in whichever direction had the smallest difference
    letImage(interpolate(stencilCollect(Seq((0, -1), (0, 0)), g_gb)), gv_r =>
    letImage(point_abs_diff(stencilCollect(Seq((0, -1), (0, 0)), g_gb)), gvd_r =>
    letImage(interpolate(stencilCollect(Seq((1, 0), (0, 0)), g_gr)), gh_r =>
    letImage(point_abs_diff(stencilCollect(Seq((1, 0), (0, 0)), g_gr)), ghd_r =>

    letImage(select_interpolation(gv_r, gvd_r, gh_r, ghd_r), g_r => // y: [1; H[, x: [0; W-1[

    letImage(interpolate(stencilCollect(Seq((0, 1), (0, 0)), g_gr)), gv_b =>
    letImage(point_abs_diff(stencilCollect(Seq((0, 1), (0, 0)), g_gr)), gvd_b =>
    letImage(interpolate(stencilCollect(Seq((-1, 0), (0, 0)), g_gb)), gh_b =>
    letImage(point_abs_diff(stencilCollect(Seq((-1, 0), (0, 0)), g_gb)), ghd_b =>

    letImage(select_interpolation(gv_b, gvd_b, gh_b, ghd_b), g_b => // y: [0; H-1[, x: [1; W[

    // next interpolate red at gr by first interpolating,
    // then correcting using the error green would have had if we had
    // interpolated it in the same way (i.e. add the second derivative
    // of the green channel at the same place).
    letImage(correct(
      interpolate(stencilCollect(Seq((-1, 0), (0, 0)), r_r)),
      g_gr,
      interpolate(stencilCollect(Seq((-1, 0), (0, 0)), g_r))
    ), r_gr =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (0, -1)), b_b)),
      g_gr,
      interpolate(stencilCollect(Seq((0, 0), (0, -1)), g_b))
    ), b_gr =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (0, 1)), r_r)),
      g_gb,
      interpolate(stencilCollect(Seq((0, 0), (0, 1)), g_r))
    ), r_gb =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (1, 0)), b_b)),
      g_gb,
      interpolate(stencilCollect(Seq((0, 0), (1, 0)), g_b))
    ), b_gb =>

    // now interpolate diagonally to get red at blue and blue at red.
    // we try both directions (positive and negative diagonals),
    // and use the one with smallest difference. we also correct our interpolations
    // using the second derivative of green at the same sites.
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (1, -1)), r_r)),
      g_b,
      interpolate(stencilCollect(Seq((0, 0), (1, -1)), g_r))
    ), rp_b =>
    letImage(point_abs_diff(stencilCollect(Seq((0, 0), (1, -1)), r_r)), rpd_b =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, -1), (1, 0)), r_r)),
      g_b,
      interpolate(stencilCollect(Seq((0, -1), (1, 0)), g_r))
    ), rn_b =>
    letImage(point_abs_diff(stencilCollect(Seq((0, -1), (1, 0)), r_r)), rnd_b =>

    letImage(select_interpolation(rp_b, rpd_b, rn_b, rnd_b), r_b => // y: [1; H-1[, x: [1; W-1[

    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (-1, 1)), b_b)),
      g_r,
      interpolate(stencilCollect(Seq((0, 0), (-1, 1)), g_b))
    ), bp_r =>
    letImage(point_abs_diff(stencilCollect(Seq((0, 0), (-1, 1)), b_b)), bpd_r =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 1), (-1, 0)), b_b)),
      g_r,
      interpolate(stencilCollect(Seq((0, 1), (-1, 0)), g_b))
    ), bn_r =>
    letImage(point_abs_diff(stencilCollect(Seq((0, 1), (-1, 0)), b_b)), bnd_r =>

    letImage(select_interpolation(bp_r, bpd_r, bn_r, bnd_r), b_r => { // y: [1; H-1[, x: [1; W-1[
      val Seq(
        r_gr_o, r_r_o, r_b_o, r_gb_o,
        g_gr_o, g_r_o, g_b_o, g_gb_o,
        b_gr_o, b_r_o, b_b_o, b_gb_o
      ) = intersectImages(Seq(
        r_gr, r_r, r_b, r_gb,
        g_gr, g_r, g_b, g_gb,
        b_gr, b_r, b_b, b_gb
      ))
      makeArray(3)(
        makeArray(2)( makeArray(2)(r_gr_o.expr, r_r_o.expr), makeArray(2)(r_b_o.expr, r_gb_o.expr) ), // r
        makeArray(2)( makeArray(2)(g_gr_o.expr, g_r_o.expr), makeArray(2)(g_b_o.expr, g_gb_o.expr) ), // g
        makeArray(2)( makeArray(2)(b_gr_o.expr, b_r_o.expr), makeArray(2)(b_b_o.expr, b_gb_o.expr) )  // b
      ) /* 3.2.2.H.W.f */ |> map(
        map( // 2.H.W.f
          transpose >> map(transpose) >> join // H.(W * 2).f
        ) // 2.H.(W * 2).f
          >> transpose >> join // (H * 2).(W * 2).f
      )
    }))))))))))))))))))))))))
  })))))

  val hot_pixel_suppression = nFun(h => nFun(w => fun(
    (h`.`w`.`i16) ->: ((h-4)`.`(w-4)`.`i16)
  )(input =>
    mapImage(
      stencilCollect(Seq((-2, 0), (2, 0), (0, 0), (0, -2), (0, 2)), Image(0, w, 0, h, input)),
      fun(5`.`i16)(nbh => clamp(i16)(nbh `@` lidx(2, 5), li16(0),
        max(max(nbh `@` lidx(0, 5), nbh `@` lidx(1, 5)), max(nbh `@` lidx(3, 5), nbh `@` lidx(4, 5)))
      ))
    ).expr
  )))

  val deinterleave = nFun(h => nFun(w => fun(
    ((2*h)`.`(2*w)`.`i16) ->: (4`.`h`.`w`.`i16)
  )(raw =>
    raw |>
    map(split(2)) >> split(2) >> // h.2.w.2.i16
      map(map(transpose)) >> // h.2.2.w.
      transpose >> map(transpose) >> // 2.2.h.w.
      join // 4.h.w.
  )))

  val color_correct = nFun(h => nFun(w => nFun(hm => nFun(wm => fun(
    (3`.`h`.`w`.`i16) ->:
      (hm`.`wm`.`f32) ->: (hm`.`wm`.`f32) ->: f32 ->:
      (3`.`h`.`w`.`i16)
  )((input, matrix_3200, matrix_7000, color_temp) => {
    // get a color matrix by linearly interpolating between two
    // calibrated matrices using inverse kelvin.
    val kelvin = color_temp
    val alpha = (l(1.0f) / kelvin - l(1.0f / 3200)) / l(1.0f / 7000 - 1.0f / 3200)
    (
      zipND(2)(matrix_3200, matrix_7000) |>
        map(map(fun(p => p._1 * alpha + p._2 * (l(1.0f) - alpha)))) >>
          map(map(fun(v => cast(v * l(256.0f)) :: i16))) // Q8.8 fixed point
      ) |> fun(matrix =>
        input |> transpose >> map(transpose >> map(map(cast :: (i16 ->: i32)))) // H.W.3.i32
        >> map(map(fun(irgb => {
          val ir = irgb `@` lidx(0, 3)
          val ig = irgb `@` lidx(1, 3)
          val ib = irgb `@` lidx(2, 3)
          def m(x: Int, y: Int): Expr = cast(matrix `@` lidx(y, hm) `@` lidx(x, wm)) :: i32
          makeArray(3)(
            m(3, 0) + m(0, 0) * ir + m(1, 0) * ig + m(2, 0) * ib,
            m(3, 1) + m(0, 1) * ir + m(1, 1) * ig + m(2, 1) * ib,
            m(3, 2) + m(0, 2) * ir + m(1, 2) * ig + m(2, 2) * ib
          )
        }))) >> map(map(map(fun(rgb => cast(rgb / li32(256)) :: i16))) >> transpose) >> transpose
    )
  })))))

  val apply_curve = nFun(h => nFun(w => fun(
    (3`.`h`.`w`.`i16) ->: f32 ->: f32 ->: int ->: int ->: (3`.`h`.`w`.`u8)
  )((input, gamma, contrast, blackLevel, whiteLevel) => {
    val lutResample = 1 // how much to resample the LUT by when sampling it
    val minRaw = blackLevel / l(lutResample)
    val maxRaw = whiteLevel / l(lutResample)

    val invRange = l(1.0f) / (cast(maxRaw - minRaw) :: f32)
    val b = l(2.0f) - pow(l(2.0f), contrast / l(100.0f))
    val a = l(2.0f) - l(2.0f) * b

    // make LUT
    generate(fun(IndexType(1024))(x_ => {
      val x = cast(x_) :: int
      // get a linear luminance in the range 0-1
      val xf = clamp(f32)((cast(x - minRaw) :: f32) * invRange, l(0.0f), l(1.0f))
      // gamma correct it
      val g = pow(xf, l(1.0f) / gamma)
      // apply a piecewise quadratic contrast curve
      val gmo = l(1.0f) - g
      val z = select(g > l(0.5f),
        l(1.0f) - (a * gmo) * gmo + b * gmo,
        a * g * g + b * g
      )
      // convert to 8 bit
      val v = cast(clamp(f32)(z * l(255.0f) + l(0.5f), l(0.0f), l(255.0f))) :: u8
      // add guard band outside of (minraw, maxRaw]
      select(x <= minRaw, lu8(0), select(x > maxRaw, lu8(255), v))
    })) |> fun(curve =>
      input |> map(map(map(fun(p => curve `@` natAsIndex(1024)(cast(clamp(i16)(p, li16(0), li16(1023))) :: NatType)))))
    )
  })))

  val blur121: Expr = implDT(dt => dtFun(compute_dt => fun(
    (3`.`dt) ->: dt
  )(vs =>
    avg(compute_dt)(avg(compute_dt)(vs `@` lidx(0, 3), vs `@` lidx(1, 3)), vs `@` lidx(2, 3))
  )))

  val sharpen = nFun(h => nFun(w => fun(
    (3`.`h`.`w`.`u8) ->: f32 ->: (3`.`(h-2)`.`(w-2)`.`u8)
  )((input, strength) => {
    // convert sharpening strength to 2.5 fixed point. this allows sharpening in the range [0, 4].
    val u8_sat = dtFun(dt => fun(x => cast(clamp(dt)(x, cast(l(0)) :: dt, cast(l(255)) :: dt)) :: u8))
    u8_sat(f32)(strength * l(32.0f)) |> fun(strength_x32 =>
      input |> map(fun(plane_ => {
        val plane = Image(0, w, 0, h, plane_)
        letImage(mapImage(stencilCollect(Seq((0, -1), (0, 0), (0, 1)), plane), blur121(u16)), unsharp_y =>
        letImage(mapImage(stencilCollect(Seq((-1, 0), (0, 0), (1, 0)), unsharp_y), blur121(u16)), unsharp => {
          val Seq(pl, un) = intersectImages(Seq(plane, unsharp))
          letImage(mapImage(zipImage(pl, un), fun(p =>
            (cast(p._1) :: i16) - (cast(p._2) :: i16)
          )), mask => {
            val Seq(pl, ma) = intersectImages(Seq(plane, mask))
            mapImage(zipImage(pl, ma), fun(p =>
              u8_sat(i16)((cast(p._1) :: i16) + (p._2 * cast(strength_x32) :: i16) / li16(32))
            )).expr
          })
        }))
      }))
    )
  })))

  /* TODO
  val shift = ??? >> map(map(fun(p => cast(p) :: i16)))

  val camera_pipe = nFun(h => nFun(w => fun(
    (h`.`w`.`u16) ->: (3`.`((h - 24) / 32) * 32)`.`((w - 32) / 32) * 32)`.`u8)
  )(input =>
    shift >>
    hot_pixel_suppression >>
    deinterleave >>
    demosaic >>
    color_correct >>
    apply_curve >>
    sharpen
  )))
  */
}