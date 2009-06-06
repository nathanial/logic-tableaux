package logic.reduction
import java.util.regex.Pattern

object PredicateUtils {
  val propPattern = Pattern.compile("""~?[A-Z][a-z]""")

  def isProposition(text: String): Boolean = {
    return propPattern.matcher(text).matches
  }
}

class Proposition
case class PredicateProposition(negated: Boolean, predicate: String, element: String) extends Proposition
case class SententialProposition(negated: Boolean, element: String) extends Proposition
