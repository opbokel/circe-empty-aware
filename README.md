# circe-empty-aware

This provide a way to make circe (https://github.com/circe/circe) encoders empty aware. 

To use add to your build.sbt:

lazy val emptyAwareProject = RootProject( uri("git://github.com/opbokel/circe-empty-aware#v0.5") )
and add this on your root project definition .dependsOn(emptyAwareProject)


## The problem:

If you have several objects with lots of optional fields and/or empty arrays. Let's see the example below:

```scala
case class Documentation(userInstruction: Option[String] = None, quickTips: Option[String] = None)
```

Documentation is a class with two optional fields


```scala
case class ItemOptDoc(name: String, documentation: Option[Documentation])
```

An ItemOptDoc have a name and possible a Documentation. An ItemOptDoc have a name and possible a Documentation. **This was made that way to allow the encoded json to support null documentation key and reduce payload.**

And the encoders something like this:

```scala
implicit val docEncoder = deriveEncoder[Documentation]

implicit val itemOptEncoder = deriveEncoder[ItemOptDoc]
```

But this means that now I have **one extra complexity and two ways to represent a empty object in scala.** It makes the code more complex because of a json representation.

```scala
/**
* Bouth represents empty Documentation
**/
ItemOpt("item 1", None)

ItemOpt("item 1", Some(Documentation()))
```

So, the simpler (and probably best) way the make it in Scala is to declare item like this, **without an optional field:**

```scala
case class Item(name: String, documentation: Documentation)
```
But the generated (TBA)

**The Encoder should do the work of dropping the documentation key if the Json generated by the Documentation encoder is empty. A Value is considered empty if it is a empty array, null, a empty object or if all the keys on the object are associated with empty values.**




