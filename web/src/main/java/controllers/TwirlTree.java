package controllers;

import play.twirl.api.Html;

/**
 * The two members of a Twirl {@code Html} that {@link ResponseBody} walks: its child
 * fragments, and a leaf's own rendering into a builder. Both are public on the JVM but
 * hidden from Scala callers ({@code elements} is a private constructor field of
 * {@code Html}, {@code buildString} protected), so they are reached from Java.
 *
 * <p>Coupled to Twirl's class layout on purpose and pinned by {@code ResponseBodySpec},
 * which asserts the walk reproduces {@code Html.body} byte for byte, escaping included:
 * a Twirl upgrade that changes either member fails that spec, not a page.
 */
final class TwirlTree {
  private TwirlTree() {}

  static scala.collection.immutable.Seq<Html> children(Html node) {
    return node.elements();
  }

  static void renderLeaf(Html leaf, scala.collection.mutable.StringBuilder into) {
    leaf.buildString(into);
  }
}
