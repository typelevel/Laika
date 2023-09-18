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

package laika.helium.internal.config

import laika.helium.config.ColorQuintet
import laika.theme.config.Color

private[helium] case class ColorSet(
    theme: ThemeColors,
    messages: MessageColors,
    syntaxHighlighting: SyntaxColors
)

private[helium] case class ThemeColors(
    primary: Color,
    primaryMedium: Color,
    primaryLight: Color,
    secondary: Color,
    text: Color,
    background: Color,
    bgGradient: (Color, Color)
)

private[helium] case class MessageColors(
    info: Color,
    infoLight: Color,
    warning: Color,
    warningLight: Color,
    error: Color,
    errorLight: Color
)

private[helium] case class SyntaxColors(base: ColorQuintet, wheel: ColorQuintet)
