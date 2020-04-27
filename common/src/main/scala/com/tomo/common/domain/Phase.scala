package main.scala.com.tomo.common.domain

abstract class Phase(val nbCard: Int) {
  def next: Phase
}
case object FirstDraw extends Phase(nbCard = 5) {
  override def next: Phase = SecondDraw
}
case object SecondDraw extends Phase(nbCard = 3) {
  override def next: Phase = ThirdDraw
}
case object ThirdDraw extends Phase(nbCard = 3) {
  override def next: Phase = FourthDraw
}
case object FourthDraw extends Phase(nbCard = 3) {
  override def next: Phase = FifthDraw
}
case object FifthDraw extends Phase(nbCard = 3) {
  override def next: Phase = ScoringPhase
}
case object ScoringPhase extends Phase(nbCard = -1) {
  override def next: Phase = NotPlaying
}
case object NotPlaying extends Phase(nbCard = -1) {
  override def next: Phase = FirstDraw
}