package Objects

object Conditions {

  // AND
  def AndNotIf(chars: String, sign: String) = {
    Symbol.isOr(sign) || Symbol.isImplication(sign) || Symbol.isLeftImplication(sign)
  }

  def AndNotWhile(chars: String, sign: String, flag: Int) = {
    Symbol.isAnd(chars) && Symbol.isNot(sign) && flag == 0
  }


  def AndAndWhile(chars: String, sign: String, flag: Int) = {
    Symbol.isAnd(chars) && Symbol.isAnd(sign) && flag == 0
  }

  // OR
  def OrNotIf(chars: String, sign: String) = {
    Symbol.isImplication(sign) || Symbol.isLeftImplication(sign)
  }

  def OrNotWhile(chars: String, sign: String, flag: Int) = {
    Symbol.isOr(chars) && (Symbol.isAnd(sign) || Symbol.isNot(sign)) && flag == 0
  }


  def OrOrWhile(chars: String, sign: String, flag: Int) = {
    Symbol.isOr(chars) && Symbol.isOr(sign) && flag == 0
  }

  // IMPLICATION
  def ImplicationNotIf(chars: String, sign: String) = {
    Symbol.isLeftImplication(sign)
  }

  def ImplicationNotWhile(chars: String, sign: String, flag: Int) = {
    Symbol.isImplication(chars) && (Symbol.isAnd(sign) || Symbol.isNot(sign) || Symbol.isOr(sign)) && flag == 0
  }


  def ImplicationImplicationWhile(chars: String, sign: String, flag: Int) = {
    Symbol.isImplication(chars) && Symbol.isImplication(sign) && flag == 0
  }

  // LEFT IMPLICATION
  def LeftImplicationWhile(chars: String, sign: String, flag: Int) = {
    Symbol.isImplication(chars) && (Symbol.isAnd(sign) || Symbol.isNot(sign) || Symbol.isOr(sign)) && flag == 0

  }
}
