lazy val `qdsls-presentation` =
  project.in(file("."))
    .settings(
      name    := "dotty_macro_example",
      version := "0.1.0",
      resolvers += Resolver.mavenLocal,
      scalaVersion := "3.0.0", // "0.21.0-RC1", //"0.22.0-bin-20200114-193f7de-NIGHTLY", //dottyLatestNightlyBuild.get,

      scalacOptions ++= Seq(
        "-language:implicitConversions"
      ),
      libraryDependencies ++= Seq(
        ("dev.zio"       %% "zio-json"     % "0.1.5").withDottyCompat(scalaVersion.value),
        ("com.lihaoyi"   %% "pprint"       % "0.5.6").withDottyCompat(scalaVersion.value),
        ("org.scalameta" %% "scalafmt-cli" % "2.7.5").excludeAll(ExclusionRule(
          organization = "org.scala-lang.modules",
          name = "scala-xml_2.13"
        )).withDottyCompat(scalaVersion.value)
      )
    )
