/*
 * Copyright 2014-2016 the original author or authors.
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
      "border-spacing",
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
      "border-right-width")
  private val padding = Set(
      "padding",
      "padding-before",
      "padding-after",
      "padding-start",
      "padding-end",
      "padding-top",
      "padding-bottom",
      "padding-left",
      "padding-right")
  private val background = Set(
      "background-color",
      "background-image",
      "background-repeat",
      "background-position",
      "background-position-horizontal",
      "background-position-vertical")
  private val blockMargin = Set(
      "margin",
      "margin-top",
      "margin-bottom",
      "margin-left",
      "margin-right",
      "space-before",
      "space-after",
      "start-indent",
      "end-indent")
  private val absolutePosition = Set(
      "position",
      "absolute-position",
      "top",
      "bottom",
      "left",
      "right")
  private val dimension = Set(
      "block-progression-dimension",
      "inline-progression-dimension",
      "height",
      "width")
  private val areaAlign = Set(
      "vertical-align",
      "alignment-adjust",
      "alignment-baseline",
      "baseline-shift",
      "dominant-baseline")
  private val break = Set(
      "break-after",
      "break-before",
      "page-break-after",
      "page-break-before")
  private val keep = Set(
      "keep-with-next",
      "keep-with-previous")
  private val keepPlus = keep ++ Set(
      "page-break-inside",
      "keep-together")
  private val font = Set(
      "font",
      "font-family",
      "font-size",
      "font-style",
      "font-weight")
  private val hyphenation = Set(
      "country",
      "language",
      "hyphenate",
      "hyphenation-character",
      "hyphenation-push-character-count",
      "hyphenation-remain-character-count")
  
  
  private val region = (
        border ++
        padding ++ 
        background ++ Set(
        "region-name",
        "extent",
        "precedence",
        "display-align",
        "overflow",
        "reference-orientation",
        "writing-mode"))

  private val pageNumber = (
        border ++
        padding ++ 
        background ++ 
        font ++ 
        areaAlign ++ 
        keep ++ Set(
        "id",
        "letter-spacing",
        "line-height",
        "text-decoration",
        "text-transform",
        "word-spacing",
        "wrap-option"))

  private val embedded = (
        border ++
        padding ++ 
        background ++ 
        areaAlign ++ 
        dimension ++
        keep ++ Set(
        "id",
        "content-height",
        "content-width",
        "scaling",
        "display-align",
        "line-height",
        "overflow",
        "text-align"))
        
  private val tablePart:Set[String] = (border ++ padding ++ background)      


  private val map = Map[String,Set[String]](
    "page-sequence" -> Set(
        "id", 
        "country",
        "flow-map-reference",
        "format",
        "language",
        "letter-value",
        "grouping-separator",
        "grouping-size",
        "initial-page-number",
        "force-page-count",
        "master-reference",
        "reference-orientation",
        "writing-mode"),
    "layout-master-set" -> Set.empty,
    "page-sequence-master" -> Set(
        "master-name"),
    "single-page-master-reference" -> Set(
        "master-reference"),
    "repeatable-master-reference" -> Set(
        "master-reference",
        "maximum-repeats"),
    "repeatable-page-master-alternatives" -> Set(
        "maximum-repeats"),
    "conditional-page-master-reference" -> Set(
        "master-reference",
        "page-position",
        "odd-or-even",
        "blank-or-not-blank"),
    "simple-page-master" -> (
        blockMargin ++ Set(
        "master-name",
        "page-height",
        "page-width",
        "reference-orientation",
        "writing-mode")),
    "region-body" -> (
        border ++
        padding ++ 
        background ++ 
        blockMargin ++ Set(
        "region-name",
        "column-count",
        "column-gap",
        "display-align",
        "overflow",
        "reference-orientation",
        "writing-mode")),
    "region-before" -> region,
    "region-after" -> region,
    "region-start" -> region,
    "region-end" -> region,
    "flow" -> Set(
         "id",
         "flow-name"),
    "static-content" -> Set(
         "id",
         "flow-name"),
    "title" -> (
        border ++
        padding ++ 
        background ++ 
        font ++ Set(
        "color",
        "line-height")),   
    "block" -> (
        border ++
        padding ++ 
        background ++ 
        font ++ 
        blockMargin ++ 
        hyphenation ++ 
        break ++ 
        keepPlus ++ Set(
        "id",
        "orphans",
        "widows",
        "color",
        "hyphenation-ladder-count",
        "last-line-end-indent",
        "line-height",
        "line-height-shift-adjustment",
        "line-stacking-strategy",
        "span",
        "text-align",
        "text-align-last",
        "text-indent",
        "white-space",
        "white-space-treatment",
        "white-space-collapse",
        "linefeed-treatment",
        "wrap-option")),
    "block-container" -> (
        border ++
        padding ++ 
        background ++ 
        blockMargin ++ 
        absolutePosition ++ 
        dimension ++ 
        break ++ 
        keepPlus ++ Set(
        "id",
        "display-align",
        "reference-orientation",
        "overflow",
        "span",
        "writing-mode")),
    "inline" -> (
        border ++
        padding ++ 
        background ++ 
        font ++ 
        dimension ++ 
        keepPlus ++ 
        areaAlign ++ Set(
        "id",
        "color",
        "line-height",
        "text-decoration",
        "wrap-option")),
    "leader" -> (
        border ++
        padding ++ 
        background ++ 
        font ++ 
        keep ++ 
        areaAlign ++ Set(
        "id",
        "color",
        "leader-length",
        "leader-pattern",
        "leader-pattern-width",
        "rule-style",
        "rule-thickness",
        "line-height",
        "word-spacing")),
    "page-number" -> pageNumber,
    "page-number-citation" -> (pageNumber + "ref-id"),
    "page-number-citation-last" -> (pageNumber + "ref-id"),
    "character" -> (
        border ++
        padding ++ 
        background ++ 
        font ++ 
        hyphenation ++
        keep ++ 
        areaAlign ++ Set(
        "id",
        "color",
        "character",
        "letter-spacing",
        "line-height",
        "text-decoration",
        "text-transform",
        "word-spacing")),     
    "basic-link" -> (
        border ++
        padding ++ 
        background ++ 
        keepPlus ++ 
        areaAlign ++ Set(
        "id",
        "color",
        "line-height",
        "external-destination",
        "internal-destination",
        "show-destination")),    
    "external-graphic" -> (embedded + "src"),
    "instream-foreign-object" -> embedded,
    "bidi-override" -> (
        font ++ Set(
        "color",
        "direction",
        "letter-spacing",
        "line-height",
        "word-spacing")),
    "table" -> (
        border ++
        padding ++ 
        background ++ 
        blockMargin ++ 
        dimension ++
        break ++
        keepPlus ++ Set(
        "id",
        "border-separation",
        "border-collapse",
        "table-omit-footer-at-break",
        "table-omit-header-at-break",
        "writing-mode")),  
    "table-column" -> (
        border ++
        padding ++ 
        background ++ Set(
        "column-number",
        "column-width",
        "number-columns-repeated",
        "number-columns-spanned")), 
    "table-header" -> tablePart,
    "table-body" -> tablePart,
    "table-footer" -> tablePart,
    "table-row" -> (tablePart ++ 
        dimension ++ 
        break ++ 
        keepPlus),
    "table-cell" -> (tablePart ++ 
        dimension ++ 
        break ++ 
        keepPlus ++ Set(
        "id",
        "display-align",
        "column-number",
        "starts-row",
        "ends-row",
        "number-columns-spanned",
        "number-rows-spanned")),
    "list-block" -> (
        border ++
        padding ++
        background ++
        blockMargin ++
        break ++ 
        keepPlus ++ Set(
        "id",
        "provisional-distance-between-starts",
        "provisional-label-separation")),    
    "list-item" -> (
        border ++
        padding ++
        background ++
        blockMargin ++
        break ++ 
        keepPlus ++ Set(
        "id")), 
    "list-item-label" -> Set("id","keep-together","end-indent"),
    "list-item-body" -> Set("id","keep-together","start-indent"),
    "footnote" -> Set("id"),
    "footnote-body" -> Set("id"),
    "marker" -> Set("marker-class-name"),
    "retrieve-marker" -> Set(
        "retrieve-class-name",
        "retrieve-position",
        "retrieve-boundary"),
    "bookmark-tree" -> Set.empty,
    "bookmark" -> Set(
        "external-destination",
        "internal-destination",
        "starting-state"),
    "bookmark-title" -> Set(
        "color",
        "font-style",
        "font-weight"),
    "wrapper" -> Set("id"),
    "declarations" -> Set.empty,
    "color-profile" -> Set("src")
  ).withDefaultValue(Set())
  
  /** Filters out all unsupported attributes for the specified tagName
   *  and return a new sequence containing only attributes valid for that tag.
   *  
   *  @param tagName the name of tag to filter the attributes for
   *  @param attributes the attributes to filter as a sequence of key-name tuples
   *  @return a new sequence containing only attributes valid for that tag
   */
  def filterAttributes (tagName: String, attributes: Seq[(String, Any)]): Seq[(String, Any)] = {
    val supportedProps = map(tagName.drop(3))
    attributes.filter(pair => supportedProps(pair._1))
  }
    
}
