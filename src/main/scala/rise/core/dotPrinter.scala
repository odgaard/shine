package rise.core

import rise.core.types.Type

case object dotPrinter {

  def apply(expr: Expr): String = generateDotString(expr)

  def generateDotString(
      expr: Expr,
      printTypes: Boolean = true,
      inlineLambdaIdentifier: Boolean = false
  ): String = {

    def getID(x: Any): String = x match {
      case i: Identifier if !inlineLambdaIdentifier => i.toString
      case _                                        => freshName("node")
    }

    def generateNodesAndEdges(
        expr: Expr,
        parent: String,
        printTypes: Boolean,
        inlineLambdaIdentifier: Boolean
    ): String = {

      def attr(str: String): String = s"[$str]"
      def fill(c: String): String = s"fillcolor=$c "
      def fillWhite: String = fill("white")
      def fillGray: String = fill("\"#e6e2de\"")
      def fillDarkGray: String = fill("\"#9a9a9a\"")
      def fillBlack: String = fill("black")

      def formatType(t: Type): String =
        if(printTypes)
          t.toString.replaceAll(">", "\\\\>").replaceAll("<", "\\\\<")
        else ""

      case class Label(
          s: String,
          decorations: String => String = x => s"$x",
          forEdge: Boolean = false
      ) {
        def bold: Label =
          this.copy(decorations = x => s"<b>${decorations(x)}</b>")
        def italic: Label =
          this.copy(decorations = x => s"<i>${decorations(x)}</i>")
        def gray: Label =
          this.copy(decorations =
            x => s"<font color='gray'>${decorations(x)}</font>"
          )
        // keep for now: used as style in the arXive paper
        // def green: Label =
        //  this.copy(decorations =
        //    x => s"""<font color="#3C8031">${decorations(x)}</font>"""
        //  )
        def orange: Label =
          this.copy(decorations =
            x => s"""<font color="#F26035">${decorations(x)}</font>"""
          )
        def edge: Label = this.copy(forEdge = true)

        override def toString: String = {
          val label =
            if (!forEdge && printTypes) {
              "\"" + s + "\\n" + formatType(expr.t) + "\""
            } else "<" + decorations(s) + ">"

          s"label=$label"
        }
      }

      val edgeLabel = (x: String) =>
        attr(fillBlack + Label(x).gray.edge.toString)

      def recurse(e: Expr, parent: String, ty: Option[String]): String =
        generateNodesAndEdges(e, parent, printTypes, inlineLambdaIdentifier)

      def binaryNode(
          nodeLabel: String,
          a: (Expr, String),
          b: (Expr, String)
      ): String = {
        val aID = getID(a._1)
        val bID = getID(b._1)
        s"""$parent ${attr(fillWhite + Label(nodeLabel).toString)}
           |$parent -> $aID ${edgeLabel(a._2)};
           |$parent -> $bID ${edgeLabel(b._2)};
           |${recurse(a._1, aID, None)}
           |${recurse(b._1, bID, None)}""".stripMargin
      }

      expr match {
        case Lambda(i, e) if !inlineLambdaIdentifier =>
          binaryNode("λ", (i, "id"), (e, "body"))

        case Lambda(i, e) if inlineLambdaIdentifier =>
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label(s"λ.$i").toString)}
             |$parent -> $expr ${edgeLabel("body")};
             |${recurse(e, expr, None)}""".stripMargin

        case App(f, e) => binaryNode("apply", (f, "fun"), (e, "arg"))

        case DepLambda(x, e) if !inlineLambdaIdentifier =>
          val id = getID(x)
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label("Λ").bold.toString)}
             |$parent -> $id ${edgeLabel("id")};
             |$parent -> $expr ${edgeLabel("body")};
             |$id ${attr(fillWhite + Label(x.toString).orange.toString)}
             |${recurse(e, expr, None)}""".stripMargin

        case DepLambda(x, e) if inlineLambdaIdentifier =>
          val expr = getID(e)
          s"""$parent ${attr(fillWhite + Label(s"Λ.$x").toString)}
             |$parent -> $expr ${edgeLabel("body")};
             |${recurse(e, expr, None)}""".stripMargin

        case DepApp(f, e) =>
          val fun = getID(f)
          val arg = getID(e)
          s"""$parent ${attr(fillWhite + Label("depApply").toString)}
             |$parent -> $fun ${edgeLabel("fun")};
             |$parent -> $arg ${edgeLabel("arg")};
             |$arg ${attr(fillWhite + Label(e.toString).toString)}
             |${recurse(f, fun, None)}""".stripMargin

        case l: Literal =>
          s"$parent ${attr(fillWhite +
            Label(l.toString.trim).orange.italic.toString)}"
        case i: Identifier =>
          s"$parent ${attr(fillWhite +
            Label(i.toString.trim).orange.italic.toString)}"
        case p: Primitive => p match {
          case primitives.MapSeq() => s"$parent ${attr(fillDarkGray +
            Label(p.toString.trim).bold.toString)}"
          case primitives.ReduceSeq() => s"$parent ${attr(fillDarkGray +
            Label(p.toString.trim).bold.toString)}"
          case _ => s"$parent ${attr(fillGray +
            Label(p.toString.trim).bold.toString)}"
        }
      }
    }

    val content =
      generateNodesAndEdges(expr, getID(expr),
        printTypes, inlineLambdaIdentifier)

    s"""
       |digraph graphname
       |{
       |graph [fontname = "FiraCode"];
       |node [fontname = "FiraCode"];
       |edge [fontname = "FiraCode"];
       |node [shape="record",style="rounded, filled"]
       |$content
       |}
     """.stripMargin
  }
}
