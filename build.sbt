lazy val `qdsls-presentation` =
  project.in(file("."))
    .settings(
      name    := "dotty_macro_example",
      version := "0.1.0",
      resolvers += Resolver.mavenLocal,
      scalaVersion := "3.1.0",
      scalacOptions ++= Seq(
        "-language:implicitConversions"
      ),
      libraryDependencies ++= Seq(
        "dev.zio"        %% "zio-json"     % Version.zioJson,
        "com.lihaoyi"    %% "pprint"       % Version.pprint,
        ("org.scalameta" %% "scalafmt-cli" % Version.scalafmt)
          .excludeAll(
            ExclusionRule(organization = "com.lihaoyi", name = "sourcecode_2.13"),
            ExclusionRule(organization = "com.lihaoyi", name = "fansi_2.13"),
            ExclusionRule(organization = "com.lihaoyi", name = "pprint_2.13"),
            ExclusionRule(organization = "org.scala-lang.modules", name = "scala-xml_2.13")
          )
          .cross(CrossVersion.for3Use2_13)
      ),
      excludeDependencies ++= Seq(
        ExclusionRule("org.scala-lang.modules", "scala-collection-compat_2.13")
      )
    )

val Version = new {
  val pprint     = "0.6.6"
  val scalafmt   = "3.1.0"
  val zioJson    = "0.2.0-M2"
  val zioPrelude = "1.0.0-RC6"
}
