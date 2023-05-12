/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.render

/** Mapping of XSL-FO tag names to their supported properties.
  *  All properties not supported by Apache FOP are omitted.
  *
  *  @author Jens Halm
  */
trait FOProperties {

  private val border = Set(
    "border",
    "border-top",
    "border-bottom",
    "border-left",
    "border-right",
    "border-color",
    "border-style",
    "border-width",
    "border-before-color",
    "border-before-style",
    "border-before-width",
    "border-after-color",
    "border-after-style",
    "border-after-width",
    "border-start-color",
    "border-start-style",
    "border-start-width",
    "border-end-color",
    "border-end-style",
    "border-end-width",
    "border-top-color",
    "border-top-style",
    "border-top-width",
    "border-bottom-color",
    "border-bottom-style",
    "border-bottom-width",
    "border-left-color",
    "border-left-style",
    "border-left-width",
    "border-right-color",
    "border-right-style",
    "border-right-width",
    "fox:border-radius",
    "fox:border-before-start-radius",
    "fox:border-before-end-radius",
    "fox:border-after-start-radius",
    "fox:border-after-end-radius"
  )

  private val padding = Set(
    "padding",
    "padding-before",
    "padding-after",
    "padding-start",
    "padding-end",
    "padding-top",
    "padding-bottom",
    "padding-left",
    "padding-right"
  )

  private val background = Set(
    "background-color",
    "background-image",
    "background-repeat",
    "background-position",
    "background-position-horizontal",
    "background-position-vertical"
  )

  private val blockMargin = Set(
    "margin",
    "margin-top",
    "margin-bottom",
    "margin-left",
    "margin-right",
    "space-before",
    "space-after"
  )

  private val absolutePosition =
    Set("position", "absolute-position", "top", "bottom", "left", "right")

  private val dimension =
    Set("block-progression-dimension", "inline-progression-dimension", "height", "width")

  private val areaAlign = Set(
    "vertical-align",
    "alignment-adjust",
    "alignment-baseline",
    "baseline-shift",
    "dominant-baseline"
  )

  private val break = Set("break-after", "break-before", "page-break-after", "page-break-before")
  private val keep  = Set("keep-with-next", "keep-with-previous")

  private val region =
    border ++
      padding ++
      background ++
      Set(
        "region-name",
        "extent",
        "precedence",
        "overflow",
        "reference-orientation"
      )

  private val pageNumber =
    border ++
      padding ++
      background ++
      areaAlign ++
      keep ++
      Set(
        "id",
        "text-decoration"
      )

  private val embedded =
    border ++
      padding ++
      background ++
      areaAlign ++
      dimension ++
      keep ++
      Set(
        "id",
        "content-height",
        "content-width",
        "scaling",
        "overflow"
      )

  private val tablePart: Set[String] = border ++ padding ++ background

  private val map = Map[String, Set[String]](
    "page-sequence"                       -> Set(
      "id",
      "flow-map-reference",
      "format",
      "letter-value",
      "grouping-separator",
      "grouping-size",
      "initial-page-number",
      "force-page-count",
      "master-reference",
      "reference-orientation"
    ),
    "layout-master-set"                   -> Set.empty,
    "page-sequence-master"                -> Set("master-name"),
    "single-page-master-reference"        -> Set("master-reference"),
    "repeatable-master-reference"         -> Set("master-reference", "maximum-repeats"),
    "repeatable-page-master-alternatives" -> Set("maximum-repeats"),
    "conditional-page-master-reference"   -> Set(
      "master-reference",
      "page-position",
      "odd-or-even",
      "blank-or-not-blank"
    ),
    "simple-page-master"                  -> (blockMargin ++ Set(
      "master-name",
      "page-height",
      "page-width",
      "reference-orientation"
    )),
    "region-body"                         -> (border ++
      padding ++
      background ++
      blockMargin ++ Set(
        "region-name",
        "column-count",
        "column-gap",
        "overflow",
        "reference-orientation"
      )),
    "region-before"                       -> region,
    "region-after"                        -> region,
    "region-start"                        -> region,
    "region-end"                          -> region,
    "flow"                                -> Set("id", "flow-name"),
    "static-content"                      -> Set("id", "flow-name"),
    "title"                               -> (border ++
      padding ++
      background),
    "block"                               -> (border ++
      padding ++
      background ++
      blockMargin ++
      break ++
      keep ++ Set(
        "id",
        "span"
      )),
    "block-container"                     -> (border ++
      padding ++
      background ++
      blockMargin ++
      absolutePosition ++
      dimension ++
      break ++
      keep ++ Set(
        "id",
        "reference-orientation",
        "overflow",
        "span"
      )),
    "inline"                              -> (border ++
      padding ++
      background ++
      dimension ++
      keep ++
      areaAlign ++ Set("id", "text-decoration")),
    "leader"                              -> (border ++
      padding ++
      background ++
      keep ++
      areaAlign ++ Set("id")),
    "page-number"                         -> pageNumber,
    "page-number-citation"                -> (pageNumber + "ref-id"),
    "page-number-citation-last"           -> (pageNumber + "ref-id"),
    "character"                           -> (border ++
      padding ++
      background ++
      keep ++
      areaAlign ++ Set(
        "id",
        "character",
        "text-decoration"
      )),
    "basic-link"                          -> (border ++
      padding ++
      background ++
      keep ++
      areaAlign ++ Set(
        "id",
        "external-destination",
        "internal-destination",
        "show-destination"
      )),
    "external-graphic"                    -> (embedded + "src"),
    "instream-foreign-object"             -> embedded,
    "bidi-override"                       -> Set.empty,
    "table"                               -> (border ++
      padding ++
      background ++
      blockMargin ++
      dimension ++
      break ++
      keep ++ Set(
        "id",
        "table-omit-footer-at-break",
        "table-omit-header-at-break"
      )),
    "table-column"                        -> (border ++
      padding ++
      background ++ Set(
        "column-number",
        "column-width",
        "number-columns-repeated",
        "number-columns-spanned"
      )),
    "table-header"                        -> tablePart,
    "table-body"                          -> tablePart,
    "table-footer"                        -> tablePart,
    "table-row"                           -> (tablePart ++
      dimension ++
      break ++
      keep),
    "table-cell"                          -> (tablePart ++
      dimension ++
      break ++
      keep ++ Set(
        "id",
        "column-number",
        "starts-row",
        "ends-row",
        "number-columns-spanned",
        "number-rows-spanned"
      )),
    "list-block"                          -> (border ++
      padding ++
      background ++
      blockMargin ++
      break ++
      keep + "id"),
    "list-item"                           -> (border ++
      padding ++
      background ++
      blockMargin ++
      break ++
      keep + "id"),
    "list-item-label"                     -> Set("id"),
    "list-item-body"                      -> Set("id"),
    "footnote"                            -> Set("id"),
    "footnote-body"                       -> Set("id"),
    "marker"                              -> Set("marker-class-name"),
    "retrieve-marker" -> Set("retrieve-class-name", "retrieve-position", "retrieve-boundary"),
    "bookmark-tree"   -> Set.empty,
    "bookmark"        -> Set("external-destination", "internal-destination", "starting-state"),
    "bookmark-title"  -> Set(),
    "wrapper"         -> Set("id"),
    "declarations"    -> Set.empty,
    "color-profile"   -> Set("src")
  ).withDefaultValue(Set())

  /** Filters out all unsupported attributes for the specified tagName
    *  and return a new sequence containing only attributes valid for that tag.
    *
    *  @param tagName the name of tag to filter the attributes for
    *  @param attributes the attributes to filter as a sequence of key-name tuples
    *  @return a new sequence containing only attributes valid for that tag
    */
  def filterAttributes(
      tagName: String,
      attributes: Seq[(String, String)]
  ): Seq[(String, String)] = {
    val supportedProps = map(tagName.drop(3))
    attributes.filter(pair => FOProperties.inherited(pair._1) || supportedProps(pair._1))
  }

}

object FOProperties {

  /* https://www.w3.org/TR/xsl11/#prtab1 */
  private val inherited = Set(
    "allowed-height-scale",
    "allowed-width-scale",
    "auto-restore",
    "azimuth",
    "border-collapse",
    "border-separation",
    "border-spacing",
    "change-bar-color",
    "change-bar-offset",
    "change-bar-placement",
    "change-bar-style",
    "change-bar-width",
    "color",
    "country",
    "direction",
    "display-align",
    "elevation",
    "empty-cells",
    "end-indent",
    "font",
    "font-family",
    "font-selection-strategy",
    "font-size",
    "font-size-adjust",
    "font-stretch",
    "font-style",
    "font-variant",
    "font-weight",
    "glyph-orientation-horizontal",
    "glyph-orientation-vertical",
    "hyphenate",
    "hyphenation-character",
    "hyphenation-keep",
    "hyphenation-ladder-count",
    "hyphenation-push-character-count",
    "hyphenation-remain-character-count",
    "intrinsic-scale-value",
    "intrusion-displace",
    "keep-together",
    "language",
    "last-line-end-indent",
    "leader-alignment",
    "leader-length",
    "leader-pattern",
    "leader-pattern-width",
    "letter-spacing",
    "linefeed-treatment",
    "line-height",
    "line-height-shift-adjustment",
    "line-stacking-strategy",
    "merge-pages-across-index-key-references",
    "merge-ranges-across-index-key-references",
    "merge-sequential-page-numbers",
    "orphans",
    "page-break-inside",
    "page-number-treatment",
    "pitch",
    "pitch-range",
    "provisional-distance-between-starts",
    "provisional-label-separation",
    "relative-align",
    "richness",
    "rule-style",
    "rule-thickness",
    "scale-option",
    "score-spaces",
    "script",
    "speak",
    "speak-header",
    "speak-numeral",
    "speak-punctuation",
    "speech-rate",
    "start-indent",
    "stress",
    "text-align",
    "text-align-last",
    "text-indent",
    "text-transform",
    "visibility",
    "voice-family",
    "volume",
    "white-space",
    "white-space-collapse",
    "white-space-treatment",
    "widows",
    "word-spacing",
    "wrap-option",
    "writing-mode",
    "xml:lang"
  )

}
