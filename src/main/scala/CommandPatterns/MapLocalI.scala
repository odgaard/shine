package CommandPatterns

import Core.PhraseType._
import Core._
import DSL._

case class MapLocalI(out: Phrase[AccType],
                     f: Phrase[AccType -> (ExpType -> CommandType)],
                     in: Phrase[ExpType]) extends AbstractMapI(out, f, in) {

  override def makeMapI = MapLocalI

  override def substituteImpl: Phrase[CommandType] = {
    // TODO: replace with for loop iterating over local stuff
    `for`(length(in), i => {
      f(out `@` i)(in `@` i)
    })
  }

}
