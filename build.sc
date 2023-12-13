import mill._, scalalib._, scalafmt._

trait AdvancedScalaModule extends ScalaModule with ScalafmtModule {
  // val baseDir = build.millSourcePath
  def scalaVersion = "3.3.1"
  def scalatestVersion = "3.2.17"
  def scalacheckVersion = "3.2.17.0"
  def catsVersion = "2.10.0"
  def disciplineVersion = "2.2.0"

  override def scalacOptions: T[Seq[String]] = Seq(
    "-encoding", "UTF-8",
    "-feature",
    "-Werror",
    "-explain",
    "-deprecation",
    "-unchecked",
    "-Wunused:all",
    // Require then and do in control expressions
    // "-new-syntax",
    "-rewrite",
    "-indent",
    "-source", "future",
  )
}

object exercises extends AdvancedScalaModule {
  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-laws:$catsVersion"
  )
  object test extends ScalaTests with TestModule.ScalaTest {
    // // use `::` for scala deps, `:` for java deps
    override def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:$scalatestVersion",
      ivy"org.scalatest::scalatest:$scalatestVersion",
      ivy"org.scalatestplus::scalacheck-1-17:$scalacheckVersion",
      ivy"org.typelevel::discipline-scalatest:$disciplineVersion",
    )
  }
}

object lectures extends AdvancedScalaModule

