# com.vpalos.Json

A fully dynamic JSON handling library for Scala.

## Introduction

For some time now I (badly) wanted to be able to work with [JSON][1] structures in [Scala][2] just as easily as in JavaScript (though even in JS you need [some wizardry][3] to be able to to this elegantly).

I don't particularly like [Json4s][4] since I feel that it's overly complex (no, I don't want to extract data into case classes using for-combinators), documentation is difficult to grasp and use (especially for edge cases) and [Jackson][5] and various other libraries are mainly oriented towards the Java-style JSON handling which basically means mapping to/from POJOs or similar.

What I wanted was to be able to navigate and/or alter a JSON structure directly using simple dot/call notations (e.g. `json.server.hosts(2).name = "storage.mysite.com"`) and have a **cheat-sheet-like documentation** which whould (always) be simple and sufficient.

So here is my first swing at this using pure Scala parser combinators and other Scala-specific magic like Dynamics. The only dependencies (beside the Scala library) are the Scala [parser combinators][6] and Apache [commons lang][7] (I need this for string-escaping, especially Unicode stuff). License is Apache 2.0.

> **Disclaimer:** This library is a first serious attempt at this problem and thus **it is not optimized for high parsing speed, or low memory footprint**; it does, however, deliver on ease of working with the JSON structure, which is the main goal. A second release is in the works which tries to optimize for better parsing speed and lower memory consumption.
>
> Furthermore, I had no time yet to write a complete reference (i.e. the mentioned cheat-sheet), however the 120+ unit tests should be enough to get you going for now.

## Compiling

Normally you would just copy the `com/vpalos/Json.scala` file inside your source tree and ensure you bring the dependencies in your project (i.e. see `build.gradle`), but if you want to build a Jar or run the tests just do this (you should be connected to the Internet for all tests to pass):

```
git clone https://github.com/vpalos/com.vpalos.Json.git
cd com.vpalos.Json
./gradlew test
./gradlew build
```

 [1]: http://json.org
 [2]: http://scala-lang.org
 [3]: http://vpalos.com/1439/universal-getter-for-plain-js-objects/
 [4]: http://json4s.org
 [5]: http://wiki.fasterxml.com/JacksonHome
 [6]: http://search.maven.org/#artifactdetails|org.scala-lang.modules|scala-parser-combinators_2.11|1.0.2|bundle
 [7]: http://search.maven.org/#artifactdetails|org.apache.commons|commons-lang3|3.3.2|jar
