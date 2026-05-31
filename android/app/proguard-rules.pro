# kotlinx.serialization keeps generated serializers via @Serializable; the
# Kotlin compiler plugin emits the needed keep rules, but guard the models
# explicitly in case R8 strips reflectively-referenced companions.
-keepclassmembers class pl.kinowo.model.** {
    *** Companion;
}
-keepclasseswithmembers class pl.kinowo.model.** {
    kotlinx.serialization.KSerializer serializer(...);
}
