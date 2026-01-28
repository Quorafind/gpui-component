//! Markdown text element with WYSIWYG rendering.
//!
//! This element renders Markdown text with conditional syntax visibility:
//! - When cursor is outside a formatted region, syntax markers are hidden
//! - When cursor enters a formatted region, syntax markers become visible

use gpui::{
    App, Bounds, Element, ElementId, ElementInputHandler, Entity, GlobalElementId, Hitbox,
    HitboxBehavior, IntoElement, LayoutId, MouseButton, MouseMoveEvent, Pixels, SharedString,
    Style, TextAlign, TextRun, Window, fill, point, px, size,
};

use crate::input::blink_cursor::CURSOR_WIDTH;
use crate::theme::ActiveTheme;

use super::parser::MarkdownSpan;
use super::state::MarkdownInputState;

/// Text element for rendering Markdown with WYSIWYG behavior.
pub struct MarkdownTextElement {
    state: Entity<MarkdownInputState>,
    /// Whether to show the cursor. When false, cursor is hidden even if state says to show it.
    /// This is used when multiple MarkdownInputs share the same state (e.g., single block refs)
    /// to only show cursor in the focused location.
    cursor_visible: bool,
}

impl MarkdownTextElement {
    /// Create a new MarkdownTextElement.
    pub fn new(state: Entity<MarkdownInputState>) -> Self {
        Self {
            state,
            cursor_visible: true,
        }
    }

    /// Set whether to show the cursor.
    pub fn cursor_visible(mut self, visible: bool) -> Self {
        self.cursor_visible = visible;
        self
    }
}

impl IntoElement for MarkdownTextElement {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

/// Prepaint state for the markdown text element.
pub struct PrepaintState {
    /// The bounds after layout.
    bounds: Bounds<Pixels>,
    /// Cursor bounds for painting.
    cursor_bounds: Option<Bounds<Pixels>>,
    /// Hitbox for mouse interaction.
    hitbox: Hitbox,
    /// Line height.
    line_height: Pixels,
}

impl Element for MarkdownTextElement {
    type RequestLayoutState = ();
    type PrepaintState = PrepaintState;

    fn id(&self) -> Option<ElementId> {
        None
    }

    fn source_location(&self) -> Option<&'static std::panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _id: Option<&GlobalElementId>,
        _: Option<&gpui::InspectorElementId>,
        window: &mut Window,
        cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        let line_height = window.line_height();

        let mut style = Style::default();
        style.size.width = gpui::relative(1.).into();
        style.size.height = line_height.into();
        style.min_size.height = line_height.into();

        (window.request_layout(style, [], cx), ())
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _: Option<&gpui::InspectorElementId>,
        bounds: Bounds<Pixels>,
        _request_layout: &mut Self::RequestLayoutState,
        window: &mut Window,
        cx: &mut App,
    ) -> Self::PrepaintState {
        let state = self.state.read(cx);
        let line_height = window.line_height();

        // Update last bounds for click handling
        self.state.update(cx, |state, _| {
            state.last_bounds = Some(bounds);
        });

        let state = self.state.read(cx);

        // Calculate cursor position
        let cursor_bounds = if state.focus_handle.is_focused(window) {
            let cursor = state.cursor();
            let visual_cursor = state.offset_map.source_to_visual(cursor);

            // Approximate cursor x position (8px per char average)
            let char_width = px(8.0);
            let cursor_x = bounds.origin.x + char_width * visual_cursor as f32;

            let cursor_height = line_height * 0.85;
            let cursor_y = bounds.origin.y + (line_height - cursor_height) / 2.0;

            Some(Bounds::new(
                point(cursor_x, cursor_y),
                size(CURSOR_WIDTH, cursor_height),
            ))
        } else {
            None
        };

        let hitbox = window.insert_hitbox(bounds, HitboxBehavior::Normal);

        PrepaintState {
            bounds,
            cursor_bounds,
            hitbox,
            line_height,
        }
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _: Option<&gpui::InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _request_layout: &mut Self::RequestLayoutState,
        prepaint: &mut Self::PrepaintState,
        window: &mut Window,
        cx: &mut App,
    ) {
        let focus_handle = self.state.read(cx).focus_handle.clone();
        // Only show cursor if both: 1) state says to show it, and 2) this element allows cursor display
        let show_cursor = self.cursor_visible && self.state.read(cx).show_cursor(window, cx);
        let bounds = prepaint.bounds;
        let line_height = prepaint.line_height;

        // Set up input handler
        window.handle_input(
            &focus_handle,
            ElementInputHandler::new(bounds, self.state.clone()),
            cx,
        );

        // Set up global mouse event listener for drag selection
        // This is essential for selection to work when dragging outside element bounds
        window.on_mouse_event({
            let state = self.state.clone();

            move |event: &MouseMoveEvent, _, window, cx| {
                if event.pressed_button == Some(MouseButton::Left) {
                    state.update(cx, |state, cx| {
                        state.on_mouse_move(event, window, cx);
                    });
                }
            }
        });

        let text_style = window.text_style();
        let font = text_style.font();
        let font_size = text_style.font_size.to_pixels(window.rem_size());

        // Extract needed data before mutable borrow
        let (final_text, final_runs, selected_range, selection_start, selection_end) = {
            let state = self.state.read(cx);
            let cursor = state.cursor();
            let is_focused = state.focus_handle.is_focused(window);
            let has_block_refs = state.tokens.iter().any(|t| t.span.is_block_ref());

            // Debug logging
            if has_block_refs {
                println!(
                    "[DEBUG element.rs] is_focused={}, cursor={}, text={:?}",
                    is_focused,
                    cursor,
                    state.text.to_string()
                );
                for (i, token) in state.tokens.iter().enumerate() {
                    if token.span.is_block_ref() {
                        println!(
                            "[DEBUG element.rs] token[{}]: start={}, end={}, contains_cursor={}, visible_text={:?}",
                            i,
                            token.start,
                            token.end,
                            token.contains_cursor(cursor),
                            token.span.visible_text()
                        );
                    }
                }
            }

            let (display_text, runs) = self.build_display_text_and_runs(
                &state,
                cursor,
                font.clone(),
                cx.theme().foreground,
                cx,
            );

            let (text, text_runs, _) = if state.text.len() == 0 {
                let placeholder = state.placeholder.clone();
                let placeholder_run = TextRun {
                    len: placeholder.len(),
                    font: font.clone(),
                    color: cx.theme().muted_foreground,
                    background_color: None,
                    underline: None,
                    strikethrough: None,
                };
                (
                    placeholder,
                    vec![placeholder_run],
                    cx.theme().muted_foreground,
                )
            } else {
                (display_text, runs, cx.theme().foreground)
            };

            let start = state
                .offset_map
                .source_to_visual(state.selected_range.start);
            let end = state.offset_map.source_to_visual(state.selected_range.end);

            (text, text_runs, state.selected_range.clone(), start, end)
        };

        // Shape and paint the text
        let text_width = if !final_text.is_empty() {
            let shaped_line =
                window
                    .text_system()
                    .shape_line(final_text, font_size, &final_runs, None);

            let text_width = shaped_line.width;

            // Store text width for accurate cursor positioning
            self.state.update(cx, |state, _| {
                state.last_text_width = Some(text_width);
            });

            let text_origin = bounds.origin;
            let _ = shaped_line.paint(text_origin, line_height, TextAlign::Left, None, window, cx);

            text_width
        } else {
            self.state.update(cx, |state, _| {
                state.last_text_width = None;
            });
            px(0.)
        };

        // Get visual length for char width calculation
        let visual_len = self.state.read(cx).offset_map.visual_len();
        let char_width = if visual_len > 0 && text_width > px(0.) {
            text_width / visual_len as f32
        } else {
            px(8.0) // Fallback
        };

        // Paint selection if any
        if focus_handle.is_focused(window) && !selected_range.is_empty() {
            let selection_color = cx.theme().selection;

            let selection_x = bounds.origin.x + char_width * selection_start as f32;
            let selection_width = char_width * (selection_end - selection_start) as f32;

            let selection_bounds = Bounds::new(
                point(selection_x, bounds.origin.y),
                size(selection_width, line_height),
            );

            window.paint_quad(fill(selection_bounds, selection_color));
        }

        // Paint cursor
        if show_cursor {
            // Recalculate cursor position with accurate char width
            let state = self.state.read(cx);
            let cursor = state.cursor();
            let visual_cursor = state.offset_map.source_to_visual(cursor);
            let cursor_x = bounds.origin.x + char_width * visual_cursor as f32;

            let cursor_height = line_height * 0.85;
            let cursor_y = bounds.origin.y + (line_height - cursor_height) / 2.0;

            let cursor_bounds =
                Bounds::new(point(cursor_x, cursor_y), size(CURSOR_WIDTH, cursor_height));

            window.paint_quad(fill(cursor_bounds, cx.theme().caret));
        }
    }
}

impl MarkdownTextElement {
    /// Build display text and text runs based on cursor position.
    ///
    /// When cursor is inside a formatted token, syntax is visible.
    /// When cursor is outside, syntax is hidden and styling is applied.
    fn build_display_text_and_runs(
        &self,
        state: &MarkdownInputState,
        cursor: usize,
        font: gpui::Font,
        default_color: gpui::Hsla,
        cx: &App,
    ) -> (SharedString, Vec<TextRun>) {
        let mut display_text = String::new();
        let mut runs: Vec<TextRun> = Vec::new();

        if state.tokens.is_empty() {
            // No tokens, just return the raw text
            let text = state.text.to_string();
            let len = text.len();
            display_text = text;
            if len > 0 {
                runs.push(TextRun {
                    len,
                    font,
                    color: default_color,
                    background_color: None,
                    underline: None,
                    strikethrough: None,
                });
            }
            return (display_text.into(), runs);
        }

        for token in &state.tokens {
            let cursor_in_token = token.contains_cursor(cursor);
            let has_syntax = token.span.has_syntax();

            if has_syntax && !cursor_in_token {
                // Cursor outside: hide syntax, show styled content
                let content = token.span.visible_text();
                let content_len = content.len();

                if content_len > 0 {
                    display_text.push_str(content);

                    let (style_weight, style_italic, style_color, style_bg, style_strikethrough) =
                        match &token.span {
                            MarkdownSpan::Bold(_) => (
                                Some(gpui::FontWeight::BOLD),
                                false,
                                default_color,
                                None,
                                None,
                            ),
                            MarkdownSpan::Italic(_) => (None, true, default_color, None, None),
                            MarkdownSpan::BoldItalic(_) => (
                                Some(gpui::FontWeight::BOLD),
                                true,
                                default_color,
                                None,
                                None,
                            ),
                            MarkdownSpan::Code(_) => (
                                None,
                                false,
                                cx.theme().accent, // Code uses accent color
                                Some(cx.theme().muted.opacity(0.3)),
                                None,
                            ),
                            MarkdownSpan::Link { .. } => (
                                None,
                                false,
                                cx.theme().link, // Link uses link color
                                None,
                                None,
                            ),
                            MarkdownSpan::Strikethrough(_) => (
                                None,
                                false,
                                default_color,
                                None,
                                Some(gpui::StrikethroughStyle {
                                    thickness: px(1.),
                                    color: Some(default_color),
                                }),
                            ),
                            MarkdownSpan::Highlight(_) => (
                                None,
                                false,
                                default_color,
                                Some(cx.theme().warning.opacity(0.3)), // Yellow highlight background
                                None,
                            ),
                            MarkdownSpan::Image { .. } => (
                                None,
                                true,            // Italic for alt text
                                cx.theme().link, // Link color for image alt
                                None,
                                None,
                            ),
                            MarkdownSpan::BlockRef { .. } => (
                                None,
                                false,
                                cx.theme().link, // Block ref uses link color
                                Some(cx.theme().accent.opacity(0.15)), // Light accent background
                                None,
                            ),
                            _ => (None, false, default_color, None, None),
                        };

                    let styled_font = if style_weight.is_some() || style_italic {
                        gpui::Font {
                            weight: style_weight.unwrap_or(font.weight),
                            style: if style_italic {
                                gpui::FontStyle::Italic
                            } else {
                                font.style
                            },
                            ..font.clone()
                        }
                    } else {
                        font.clone()
                    };

                    runs.push(TextRun {
                        len: content_len,
                        font: styled_font,
                        color: style_color,
                        background_color: style_bg,
                        underline: if matches!(
                            &token.span,
                            MarkdownSpan::Link { .. }
                                | MarkdownSpan::Image { .. }
                                | MarkdownSpan::BlockRef { .. }
                        ) {
                            Some(gpui::UnderlineStyle {
                                thickness: px(1.),
                                color: Some(cx.theme().link),
                                wavy: false,
                            })
                        } else {
                            None
                        },
                        strikethrough: style_strikethrough,
                    });
                }
            } else {
                // Cursor inside or plain text: show full source
                let full_text = if has_syntax {
                    // Reconstruct the full source including syntax
                    state.text.slice(token.start..token.end).to_string()
                } else {
                    token.span.visible_text().to_string()
                };

                let full_len = full_text.len();
                if full_len > 0 {
                    display_text.push_str(&full_text);
                    runs.push(TextRun {
                        len: full_len,
                        font: font.clone(),
                        color: default_color,
                        background_color: None,
                        underline: None,
                        strikethrough: None,
                    });
                }
            }
        }

        (display_text.into(), runs)
    }
}
