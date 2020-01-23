package apps

import cameraPipe._
import util._
import rise.core.types._
import rise.core.DSL._
import rise.core.TypeLevelDSL._

import elevate.core._
import elevate.rise.Rise
import elevate.rise.rules._
// import elevate.rise.rules.algorithmic._
// import elevate.rise.rules.movement._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise.strategies.normalForm._
// import elevate.rise.strategies.algorithmic._
import elevate.rise.rules.traversal._

class cameraPipeCheck extends test_util.TestsWithExecutor {
  val N = 8
  val M = 10

  // test values are taken from Halide
  val color_temp = 3700
  val gamma = 2.0f
  val contrast = 50
  val sharpen_strength = 1.0f
  val black_level = 25
  val white_level = 1023

  val matrix_3200 = Array(
    1.6697f, -0.2693f, -0.4004f, -42.4346f,
    -0.3576f, 1.0615f, 1.5949f, -37.1158f,
    -0.2175f, -1.8751f, 6.9640f, -26.6970f
  )
  val matrix_7000 = Array(
    2.2997f, -0.4478f, 0.1706f, -39.0923f,
    -0.3826f, 1.5906f, -0.2080f, -25.4311f,
    -0.0888f, -0.7344f, 2.2832f, -20.0826f
  )

  val goldInput: Array[Int] =
    "69 150 103 184 137 218 171 252 205 30 239 64 17 98 51 132 85 166 119 200 153 234 187 12 174 191 80 97 242 3 148 165 54 71 216 233 122 139 28 45 190 207 96 113 2 19 164 181 215 168 249 202 27 236 61 14 95 48 129 82 163 116 197 150 231 184 9 218 43 252 77 30 192 81 98 243 4 149 166 55 72 217 234 123 140 29 46 191 208 97 114 3 20 165 182 71 105 186 139 220 173 254 207 32 241 66 19 100 53 134 87 168 121 202 155 236 189 14 223 48 210 227 116 133 22 39 184 201 90 107 252 13 158 175 64 81 226 243 132 149 38 55 200 217 251 204 29 238 63 16 97 50 131 84 165 118 199 152 233 186 11 220 45 254 79 32 113 66 228 117 134 23 40 185 202 91 108 253 14 159 176 65 82 227 244 133 150 39 56 201 218 107 141 222 175 0 209 34 243 68 21 102 55 136 89 170 123 204 157 238 191 16 225 50 3 84 246 7 152 169 58 75 220 237 126 143 32 49 194 211 100 117 6 23 168 185 74 91 236 253 31 240 65 18 99 52 133 86 167 120 201 154 235 188 13 222 47 0 81 34 115 68 149 102 8 153 170 59 76 221 238 127 144 33 50 195 212 101 118 7 24 169 186 75 92 237 254 143 177 2 211 36 245 70 23 104 57 138 91 172 125 206 159 240 193 18 227 52 5 86 39 120 26 43 188 205 94 111 0 17 162 179 68 85 230 247 136 153 42 59 204 221 110 127 16 33 67 20 101 54 135 88 169 122 203 156 237 190 15 224 49 2 83 36 117 70 151 104 185 138 44 189 206 95 112 1 18 163 180 69 86 231 248 137 154 43 60 205 222 111 128 17 34 179 213 38 247 72 25 106 59 140 93 174 127 208 161 242 195 20 229 54 7 88 41 122 75 156 62 79 224 241 130 147 36 53 198 215 104 121 10 27 172 189 78 95 240 1 146 163 52 69 103 56 137 90 171 124 205 158 239 192 17 226 51 4 85 38 119 72 153 106 187 140 221 174 80 225 242 131 148 37 54 199 216 105 122 11 28 173 190 79 96 241 2 147 164 53 70 215"
      .split(' ')
      .map(_.toInt)
  val goldDenoised: Array[Int] =
    "215 202 27 236 61 14 95 48 129 82 163 116 197 150 197 184 9 218 43 234 98 149 4 149 166 55 72 123 234 123 140 29 46 97 208 97 114 3 20 71 139 220 173 236 207 32 207 66 19 100 53 134 87 168 121 202 155 236 189 14 116 133 22 39 184 107 90 107 234 13 158 81 64 81 226 149 132 149 38 55 29 220 63 16 97 50 131 84 165 118 199 152 199 186 11 220 45 236 79 32 134 23 40 91 202 91 108 159 14 159 176 65 82 133 226 133 150 39 56 107 175 0 209 34 209 68 21 102 55 136 89 170 123 204 157 220 191 16 191 50 152 75 58 75 220 143 126 143 32 49 194 117 100 117 6 23 168 91 74 91 65 18 99 52 133 86 167 120 201 154 201 188 13 222 47 0 81 34 115 68 170 59 76 127 220 127 144 33 50 101 212 101 118 7 24 75 186 75 92 143 211 36 211 70 23 104 57 138 91 172 125 206 159 222 193 18 193 52 5 86 188 111 94 111 0 17 162 85 68 85 230 153 136 153 42 59 204 127 110 127 101 54 135 88 169 122 203 156 203 190 15 224 49 2 83 36 117 70 151 104 206 95 112 1 18 69 180 69 86 137 230 137 154 43 60 111 222 111 128 17 213 72 25 106 59 140 93 174 127 208 161 224 195 20 195 54 7 88 41 122 224 147 130 147 36 53 198 121 104 121 10 27 172 95 78 95 222 1 146 69"
      .split(' ')
      .map(_.toInt)
  val goldDeinterleaved: Array[Int] =
    "215 27 61 95 129 163 197 197 9 43 139 173 207 207 19 53 87 121 155 189 29 63 97 131 165 199 199 11 45 79 175 209 209 21 55 89 123 157 191 191 65 99 133 167 201 201 13 47 81 115 211 211 23 57 91 125 159 193 193 5 101 135 169 203 203 15 49 83 117 151 213 25 59 93 127 161 195 195 7 41 202 236 14 48 82 116 150 184 218 234 220 236 32 66 100 134 168 202 236 14 220 16 50 84 118 152 186 220 236 32 0 34 68 102 136 170 204 220 16 50 18 52 86 120 154 188 222 0 34 68 36 70 104 138 172 206 222 18 52 86 54 88 122 156 190 224 2 36 70 104 72 106 140 174 208 224 20 54 88 122 98 4 166 72 234 140 46 208 114 20 116 22 184 90 234 158 64 226 132 38 134 40 202 108 14 176 82 226 150 56 152 58 220 126 32 194 100 6 168 74 170 76 220 144 50 212 118 24 186 92 188 94 0 162 68 230 136 42 204 110 206 112 18 180 86 230 154 60 222 128 224 130 36 198 104 10 172 78 222 146 149 149 55 123 123 29 97 97 3 71 133 39 107 107 13 81 81 149 149 55 23 91 91 159 159 65 133 133 39 107 75 75 143 143 49 117 117 23 91 91 59 127 127 33 101 101 7 75 75 143 111 111 17 85 85 153 153 59 127 127 95 1 69 69 137 137 43 111 111 17 147 147 53 121 121 27 95 95 1 69"
      .split(' ')
      .map(_.toInt)
  val goldDemosaic: Array[Int] =
    "40 50 74 84 101 118 143 152 215 186 90 220 -21 42 68 102 102 159 120 73 170 212 212 195 97 68 -28 102 62 136 153 170 194 204 219 220 52 94 94 86 96 57 91 188 196 252 189 14 76 86 110 120 145 154 217 188 148 222 111 0 78 134 80 33 75 117 180 172 182 126 49 -45 30 104 121 138 162 172 196 206 214 222 128 18 64 17 51 93 101 164 190 224 215 144 -14 -61 97 99 131 148 165 182 199 199 199 107 11 141 91 91 125 159 159 159 112 65 99 133 133 133 209 117 21 151 55 72 89 106 123 125 157 174 109 143 143 143 96 49 83 117 117 117 70 23 133 135 167 184 201 201 201 109 13 30 47 64 127 127 80 33 67 101 101 101 54 7 41 75 23 40 57 74 91 93 125 142 159 176 193 193 64 17 51 85 85 85 119 153 153 153 106 59 208 146 114 67 179 301 286 246 182 154 113 228 202 138 108 78 14 118 176 88 82 171 226 211 320 164 4 110 -50 72 176 129 106 84 171 276 220 190 126 102 32 72 194 164 100 76 6 46 235 222 190 167 160 248 312 196 36 13 6 111 220 205 144 56 50 148 212 188 118 30 24 122 37 142 144 97 74 140 236 189 182 176 152 216 0 40 162 132 68 132 230 200 136 112 42 82"
      .split(' ')
      .map(_.toInt)
  val goldCorrected: Array[Int] =
    "12 31 68 82 114 147 193 212 345 319 151 380 -116 17 58 116 114 239 176 92 286 365 366 329 92 63 -109 120 69 224 256 284 328 348 370 368 33 108 107 89 127 64 126 319 335 454 336 -18 75 95 133 147 193 215 352 324 272 423 178 -63 81 201 103 18 95 173 310 292 331 227 49 -164 16 170 200 228 272 294 335 349 359 369 160 -74 71 -10 53 128 143 280 325 383 362 209 -112 -192 147 131 161 171 216 264 276 263 224 94 -20 148 160 121 154 183 168 171 131 59 71 126 139 142 316 155 13 179 16 42 85 93 103 97 159 208 163 190 174 172 83 38 104 110 92 65 4 10 192 189 219 234 248 265 257 106 -57 -65 -1 91 179 155 92 20 53 110 101 99 10 -59 18 126 2 24 43 50 61 76 136 146 166 186 241 297 40 -1 65 92 73 65 128 158 146 167 141 100 591 374 229 48 415 816 744 605 378 378 345 594 584 356 215 73 -147 204 456 206 140 407 596 547 857 416 -26 191 -258 137 476 295 195 116 382 726 619 476 256 174 -21 169 551 402 181 91 -95 111 643 595 450 352 308 610 823 520 71 -34 -62 295 598 539 383 133 73 371 585 503 308 59 13 327 78 414 402 221 121 346 641 461 418 379 290 524 -95 96 478 335 114 327 627 485 266 191 16 208"
      .split(' ')
      .map(_.toInt)
  val goldCurved: Array[Int] =
    "0 13 40 48 63 78 97 104 150 143 80 160 0 0 34 64 63 114 90 53 132 156 156 146 53 37 0 66 41 109 121 131 145 151 158 157 15 61 60 51 69 38 69 143 147 179 148 0 44 54 72 78 97 105 153 144 127 171 91 0 47 100 58 0 54 89 140 134 146 110 28 0 0 88 100 110 127 134 147 152 155 157 84 0 42 0 31 70 76 129 144 161 155 103 0 0 78 71 84 88 106 124 128 123 109 54 0 78 84 67 81 93 87 88 71 35 42 69 75 76 142 81 0 91 0 23 49 53 58 55 83 103 85 96 89 88 48 20 59 62 53 38 0 0 96 95 107 113 118 124 121 60 0 0 0 52 91 81 53 0 31 62 57 56 0 0 0 69 0 0 24 29 36 45 73 78 86 94 115 135 21 0 38 53 43 38 70 83 78 86 75 57 205 159 111 28 169 236 228 208 160 160 150 206 204 154 105 43 0 101 179 102 75 167 206 197 240 170 0 96 0 74 183 135 98 64 161 225 210 183 121 89 0 87 198 166 92 52 0 62 214 206 178 153 139 208 237 192 42 0 0 135 206 196 161 72 43 158 204 189 139 35 0 145 46 169 166 108 67 151 213 180 170 160 133 193 0 55 184 147 63 145 211 185 124 96 0 103"
      .split(' ')
      .map(_.toInt)
  val goldSharpened: Array[Int] =
    "0 36 76 55 136 82 3 134 166 166 43 0 81 16 132 139 143 149 147 169 75 67 41 69 0 33 154 145 203 175 48 81 93 119 115 184 150 110 206 97 130 46 0 28 68 146 125 150 101 0 117 128 141 157 146 153 154 163 197 110 58 90 107 95 98 66 3 21 76 84 81 0 117 0 3 47 53 63 54 104 102 95 97 33 0 55 65 62 44 0 98 126 144 156 164 161 64 0 0 0 97 50 0 7 54 41 55 0 0 0 0 18 26 34 39 81 86 104 118 149 154 116 28 0 79 195 61 29 193 234 184 0 137 0 59 218 133 95 28 189 192 116 89 0 57 224 178 91 52 0 220 197 187 166 255 255 217 0 0 0 212 165 33 0 159 211 199 155 1 0 202 182 104 38 158 236 177 190 209 164"
      .split(' ')
      .map(_.toInt)

  val cHeader =
    s"""
#include <stdio.h>
#include <stdint.h>
#include <math.h>

int16_t min_i16(int16_t a, int16_t b) {
  return (a < b) ? a : b;
}
int16_t max_i16(int16_t a, int16_t b) {
  return (a > b) ? a : b;
}
int16_t clamp_i16(int16_t v, int16_t l, int16_t h) {
  return min_i16(max_i16(v, l), h);
}
float min_f32(float a, float b) {
  return (a < b) ? a : b;
}
float max_f32(float a, float b) {
  return (a > b) ? a : b;
}
float clamp_f32(float v, float l, float h) {
  return min_f32(max_f32(v, l), h);
}
#define pow_f32 powf
"""

  test("hot pixel suppression passes checks") {
    val typed = printTime(infer(hot_pixel_suppression))
    println(s"hot pixel suppression: ${typed.t}")
    val lower: Strategy[Rise] = LCNF `;` CNF `;` repeatNTimes(2, oncetd(lowering.mapSeq)) `;` BENF
    val lowered = printTime(lower(typed).get)
    println(s"lowered: ${lowered}")
    val prog = gen.CProgram(lowered)
    val testCode =
    s"""
${cHeader}

${prog.code}

int main(int argc, char** argv) {
  int16_t input[($N*2 + 4) * ($M*2 + 4)] = { ${goldInput.mkString(", ")} };
  int16_t gold[($N*2) * ($M*2)] = { ${goldDenoised.mkString(", ")} };

  int16_t output[($N*2) * ($M*2)];
  ${prog.function.name}(output, ($N*2 + 4), ($M*2 + 4), input);

  for (int i = 0; i < ($N*2) * ($M*2); i++) {
    if (gold[i] != output[i]) {
      fprintf(stderr, "%d != %d\\n", gold[i], output[i]);
      return 1;
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  test("deinterleave passes checks") {
    val typed = printTime(infer(nFun(h => nFun(w =>
      deinterleave(h)(w) >> mapSeq(mapSeq(mapSeq(fun(x => x))))
    ))))
    println(s"deinterleave: ${typed.t}")
    /* TODO
    val lower: Strategy[Rise] = LCNF `;` CNF `;` repeatNTimes(1, oncetd(lowering.mapSeq)) `;` BENF
    val lowered = printTime(lower(typed).get)
     */
    val lowered = typed
    println(s"lowered: ${lowered}")
    val prog = gen.CProgram(lowered)
    val testCode =
      s"""
${cHeader}

${prog.code}

int main(int argc, char** argv) {
  int16_t input[($N*2) * ($M*2)] = { ${goldDenoised.mkString(", ")} };
  int16_t gold[4 * $N * $M] = { ${goldDeinterleaved.mkString(", ")} };

  int16_t output[4 * $N * $M];
  ${prog.function.name}(output, $N, $M, input);

  for (int i = 0; i < 4 * $N * $M; i++) {
    if (gold[i] != output[i]) {
      fprintf(stderr, "%d != %d\\n", gold[i], output[i]);
      return 1;
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  // TODO
  ignore("demosaic passes checks") {
    val typed = printTime(infer(demosaic))
    println(s"demosaic: ${typed.t}")
    // TODO
    val lower: Strategy[Rise] = strategies.basic.id()
    val lowered = printTime(lower(typed).get)
    println(s"lowered: ${lowered}")
    val prog = gen.CProgram(lowered)
    val testCode =
      s"""
${cHeader}

${prog.code}

int main(int argc, char** argv) {
  int16_t input[4 * $N * $M] = { ${goldDeinterleaved.mkString(", ")} };
  int16_t gold[3 * (2*$N - 8) * (2*$M - 8)] = { ${goldDemosaic.mkString(", ")} };

  int16_t output[3 * (2*$N - 8) * (2*$M - 8)];
  ${prog.function.name}(output, $N, $M, input);

  for (int i = 0; i < 3 * (2*$N - 8) * (2*$M - 8); i++) {
    if (gold[i] != output[i]) {
      fprintf(stderr, "%d != %d\\n", gold[i], output[i]);
      return 1;
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  test("color correction passes checks") {
    val typed = printTime(infer(color_correct))
    println(s"color correction: ${typed.t}")
    val lower: Strategy[Rise] = LCNF `;` CNF `;`
      repeatNTimes(2, oncetd(lowering.mapSeq)) `;`
      oncetd(lowering.mapSeqUnroll)
    val lowered = printTime(lower(typed).get)
    println(s"lowered: ${lowered}")
    val prog = gen.CProgram(lowered)
    // TODO: investigate output difference of 1
    val testCode =
      s"""
${cHeader}

${prog.code}

int main(int argc, char** argv) {
  int16_t input[3 * (2*$N - 8) * (2*$M - 8)] = { ${goldDemosaic.mkString(", ")} };
  int16_t gold[3 * (2*$N - 8) * (2*$M - 8)] = { ${goldCorrected.mkString(", ")} };

  float matrix_3200[3 * 4] = { ${matrix_3200.mkString(", ")} };
  float matrix_7000[3 * 4] = { ${matrix_7000.mkString(", ")} };
  float color_temp = ${color_temp};

  int16_t output[3 * (2*$N - 8) * (2*$M - 8)];
  ${prog.function.name}(output,
    2*$N - 8, 2*$M - 8, 3, 4,
    input, matrix_3200, matrix_7000, color_temp);

  for (int i = 0; i < 3 * (2*$N - 8) * (2*$M - 8); i++) {
    int16_t d = gold[i] - output[i];
    if (d < -1 || d > 1) {
      fprintf(stderr, "%d != %d\\n", gold[i], output[i]);
      return 1;
    }
    if (d != 0) {
      fprintf(stderr, "WARNING: %d != %d\\n", gold[i], output[i]);
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  test("apply curve passes checks") {
    val typed = printTime(infer(apply_curve))
    println(s"apply curve: ${typed.t}")
    val lower: Strategy[Rise] = LCNF `;` CNF `;` repeatNTimes(3, oncetd(lowering.mapSeq))
    val lowered = printTime(lower(typed).get)
    println(s"lowered: ${lowered}")
    val prog = gen.CProgram(lowered)
    val testCode =
      s"""
${cHeader}

${prog.code}

int main(int argc, char** argv) {
  int16_t input[3 * (2*$N - 8) * (2*$M - 8)] = { ${goldCorrected.mkString(", ")} };
  uint8_t gold[3 * (2*$N - 8) * (2*$M - 8)] = { ${goldCurved.mkString(", ")} };

  uint8_t output[3 * (2*$N - 8) * (2*$M - 8)];
  ${prog.function.name}(output, 2*$N - 8, 2*$M - 8,
    input, ${gamma}, ${contrast}, ${black_level}, ${white_level});

  for (int i = 0; i < 3 * (2*$N - 8) * (2*$M - 8); i++) {
    if (gold[i] != output[i]) {
      fprintf(stderr, "%d != %d\\n", gold[i], output[i]);
      return 1;
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  // TODO: rewrite fails
  ignore("sharpen passes checks") {
    val typed = printTime(infer(sharpen))
    println(s"sharpen: ${typed.t}")
    val lower: Strategy[Rise] = LCNF `;` CNF
    val lowered = printTime(lower(typed).get)
    println(s"lowered: ${lowered}")
    val prog = gen.CProgram(lowered)
    val testCode =
      s"""
${cHeader}

${prog.code}

int main(int argc, char** argv) {
  uint8_t input[3 * (2*$N - 8) * (2*$M - 8)] = { ${goldCurved.mkString(", ")} };
  uint8_t gold[3 * (2*$N - 10) * (2*$M - 10)] = { ${goldSharpened.mkString(", ")} };

  uint8_t output[3 * (2*$N - 10) * (2*$M - 10)];
  ${prog.function.name}(output, 2*$N - 8, 2*$M - 8,
    input, ${sharpen_strength});

  for (int i = 0; i < 3 * (2*$N - 10) * (2*$M - 10); i++) {
    int16_t d = (int16_t)gold[i] - (int16_t)output[i];
    if (d < -1 || d > 1) {
      fprintf(stderr, "%d != %d\\n", gold[i], output[i]);
      return 1;
    }
    if (d != 0) {
      fprintf(stderr, "WARNING: %d != %d\\n", gold[i], output[i]);
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  test("type inference") {
    println("avg: " + infer(avg).t)
    println(
      "interpolate: " + infer(
        nFun(h =>
          nFun(w =>
            fun(h `.` w `.` 2 `.` i16)(a =>
              interpolate(Image(0, w, 0, h, a)).expr
            )
          )
        )
      ).t
    )
    println(
      "point_abs_diff: " + infer(
        nFun(h =>
          nFun(w =>
            fun(h `.` w `.` 2 `.` i16)(a =>
              pointAbsDiff(Image(0, w, 0, h, a)).expr
            )
          )
        )
      ).t
    )
  }
}
