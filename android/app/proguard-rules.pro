# kotlinx.serialization keeps generated serializers via @Serializable; the
# Kotlin compiler plugin emits the needed keep rules, but guard the models
# explicitly in case R8 strips reflectively-referenced companions.
-keepclassmembers class pl.kinowo.model.** {
    *** Companion;
}
-keepclasseswithmembers class pl.kinowo.model.** {
    kotlinx.serialization.KSerializer serializer(...);
}
# R8 full mode (AGP 8 default) strips annotations and `object INSTANCE` fields
# more aggressively than the legacy optimizer. Keep the annotations the
# serialization runtime reads, and the INSTANCE of any @Serializable object so
# `Foo.serializer()` resolves.
-keepattributes RuntimeVisibleAnnotations,AnnotationDefault,InnerClasses
-if @kotlinx.serialization.Serializable class **
-keepclassmembers class <1> {
    static <1> INSTANCE;
}
