//! Markdown WYSIWYG Input Component
//!
//! A text input that renders Markdown with "what you see is what you get" behavior.
//! Markdown syntax symbols are hidden when the cursor is outside the formatted region,
//! and revealed when the cursor moves into or near the formatted text.
//!
//! ## Example
//!
//! ```rust,ignore
//! use gpui_component::input::markdown_input::{MarkdownInput, MarkdownInputState};
//!
//! let state = cx.new(|cx| MarkdownInputState::new(window, cx));
//! MarkdownInput::new(&state)
//! ```

mod element;
mod offset_map;
mod parser;
mod state;

pub use element::MarkdownTextElement;
pub use offset_map::OffsetMap;
pub use parser::{MarkdownSpan, MarkdownToken, parse_inline_markdown};
pub use state::{
    BlockRefChangeCallback, CycleTodoState, CycleTodoStateBack, EmbeddedBlockRefInput,
    EmbeddedFocusState, MarkdownInputEvent, MarkdownInputState, MoveDown, MoveUp, init,
};

// 1. Crate imports
use crate::input::Input;
use crate::{Disableable, Sizable, Size, h_flex, styled::StyleSized as _, theme::ActiveTheme};

// 2. GPUI imports
use gpui::{
    AnyElement, App, Div, ElementId, Entity, InteractiveElement, IntoElement, MouseButton,
    MouseDownEvent, ParentElement, RenderOnce, StatefulInteractiveElement, StyleRefinement, Styled,
    Window, div, px,
};

use state::{
    Backspace, Copy, CreateChild, CreateSibling, Cut, Delete, Enter, InsertNewline, MoveEnd,
    MoveHome, MoveLeft, MoveRight, MoveToNextWord, MoveToPreviousWord, Paste, Redo, SelectAll,
    SelectLeft, SelectRight, SelectToEnd, SelectToHome, Undo,
};

/// A Markdown WYSIWYG input component.
///
/// This component provides a text input that renders Markdown formatting inline,
/// hiding syntax symbols when the cursor is outside formatted regions.
#[derive(IntoElement)]
pub struct MarkdownInput {
    // 1. Identity fields
    id: ElementId,
    base: Div,
    style: StyleRefinement,

    // 2. State
    state: Entity<MarkdownInputState>,

    // 3. Configuration fields
    size: Size,
    bordered: bool,
    appearance: bool,
    disabled: bool,
    cleanable: bool,
    /// Whether to show the cursor. When false, cursor is hidden even if focused.
    /// Used when multiple MarkdownInputs share the same state (e.g., single block refs).
    cursor_visible: bool,
}

impl MarkdownInput {
    /// Create a new MarkdownInput with the given state.
    pub fn new(state: &Entity<MarkdownInputState>) -> Self {
        Self {
            id: ElementId::Name("markdown-input".into()),
            base: div(),
            style: StyleRefinement::default(),
            state: state.clone(),
            size: Size::default(),
            bordered: true,
            appearance: true,
            disabled: false,
            cleanable: false,
            cursor_visible: true,
        }
    }

    /// Set whether the cursor should be visible.
    /// When false, cursor is hidden even if the input is focused.
    /// Used when multiple MarkdownInputs share the same state.
    pub fn cursor_visible(mut self, visible: bool) -> Self {
        self.cursor_visible = visible;
        self
    }

    /// Set a custom element ID.
    pub fn id(mut self, id: impl Into<ElementId>) -> Self {
        self.id = id.into();
        self
    }

    /// Set whether the input has a visible border.
    pub fn bordered(mut self, bordered: bool) -> Self {
        self.bordered = bordered;
        self
    }

    /// Set whether the input has default appearance styling.
    pub fn appearance(mut self, appearance: bool) -> Self {
        self.appearance = appearance;
        self
    }

    /// Set whether the input shows a clear button when not empty.
    pub fn cleanable(mut self, cleanable: bool) -> Self {
        self.cleanable = cleanable;
        self
    }
}

impl InteractiveElement for MarkdownInput {
    fn interactivity(&mut self) -> &mut gpui::Interactivity {
        self.base.interactivity()
    }
}

impl StatefulInteractiveElement for MarkdownInput {}

impl Styled for MarkdownInput {
    fn style(&mut self) -> &mut StyleRefinement {
        &mut self.style
    }
}

impl Sizable for MarkdownInput {
    fn with_size(mut self, size: impl Into<Size>) -> Self {
        self.size = size.into();
        self
    }
}

impl Disableable for MarkdownInput {
    fn disabled(mut self, disabled: bool) -> Self {
        self.disabled = disabled;
        self
    }
}

impl MarkdownInput {
    /// Build content elements for hybrid rendering (text + embedded inputs).
    /// This is used when editable_block_refs is enabled and there are block refs.
    fn build_hybrid_content(
        state: &Entity<MarkdownInputState>,
        _window: &mut Window,
        cx: &mut App,
    ) -> Vec<AnyElement> {
        use parser::MarkdownSpan;

        let mut elements = Vec::new();
        let state_read = state.read(cx);
        let tokens = state_read.tokens.clone();
        let source_text = state_read.text.to_string();
        let cursor = state_read.cursor();
        let embedded_inputs = state_read.embedded_inputs.clone();
        let block_ref_contents = state_read.block_ref_contents.clone();
        drop(state_read);

        let theme = cx.theme();

        for token in &tokens {
            let cursor_in_token = token.contains_cursor(cursor);

            match &token.span {
                MarkdownSpan::BlockRef { uuid, .. } => {
                    // For block refs, use the pre-created embedded MarkdownInput if it exists
                    // The embedded input state should be created by ensure_embedded_inputs
                    // which must be called by the parent before render
                    if let Some(input_state) = embedded_inputs.get(uuid) {
                        // Create the embedded MarkdownInput element with block ref styling
                        // Using MarkdownInput allows the referenced content to have markdown formatting
                        let embedded = div()
                            .id(format!("block-ref-{}", uuid))
                            .px_1()
                            .py(px(1.0))
                            .rounded(px(3.0))
                            .bg(theme.accent.opacity(0.1))
                            .border_b_2()
                            .border_color(theme.link)
                            .child(MarkdownInput::new(input_state))
                            .into_any_element();

                        elements.push(embedded);
                    } else {
                        // Fallback: show as read-only text if embedded input not created
                        let content = block_ref_contents
                            .get(uuid)
                            .cloned()
                            .unwrap_or_else(|| format!("[{}]", uuid));

                        let fallback = div()
                            .px_1()
                            .py(px(1.0))
                            .rounded(px(3.0))
                            .bg(theme.accent.opacity(0.1))
                            .border_b_2()
                            .border_color(theme.link)
                            .text_color(theme.link)
                            .child(content)
                            .into_any_element();

                        elements.push(fallback);
                    }
                }
                _ => {
                    // For non-block-ref tokens, render as styled text
                    let has_syntax = token.span.has_syntax();

                    let text = if has_syntax && !cursor_in_token {
                        // Show only visible content when cursor is outside
                        token.span.visible_text().to_string()
                    } else {
                        // Show full source when cursor is inside or plain text
                        source_text[token.start..token.end].to_string()
                    };

                    if text.is_empty() {
                        continue;
                    }

                    // Apply styling based on span type
                    let text_element = match &token.span {
                        MarkdownSpan::Bold(_) if !cursor_in_token => {
                            div().font_weight(gpui::FontWeight::BOLD).child(text)
                        }
                        MarkdownSpan::Italic(_) if !cursor_in_token => div().italic().child(text),
                        MarkdownSpan::BoldItalic(_) if !cursor_in_token => div()
                            .font_weight(gpui::FontWeight::BOLD)
                            .italic()
                            .child(text),
                        MarkdownSpan::Code(_) if !cursor_in_token => div()
                            .px(px(4.0))
                            .mx(px(2.0))
                            .rounded(px(3.0))
                            .bg(theme.muted.opacity(0.3))
                            .text_color(theme.accent)
                            .child(text),
                        MarkdownSpan::Link { .. } if !cursor_in_token => {
                            div().text_color(theme.link).underline().child(text)
                        }
                        MarkdownSpan::Strikethrough(_) if !cursor_in_token => {
                            div().line_through().child(text)
                        }
                        MarkdownSpan::Highlight(_) if !cursor_in_token => div()
                            .px(px(2.0))
                            .rounded(px(2.0))
                            .bg(theme.warning.opacity(0.3))
                            .child(text),
                        MarkdownSpan::Image { .. } if !cursor_in_token => div()
                            .italic()
                            .text_color(theme.link)
                            .underline()
                            .child(format!("[{}]", text)),
                        _ => {
                            // Plain text or cursor in token
                            div().child(text)
                        }
                    };

                    elements.push(text_element.into_any_element());
                }
            }
        }

        elements
    }
}

impl RenderOnce for MarkdownInput {
    fn render(self, window: &mut Window, cx: &mut App) -> impl IntoElement {
        let state_ref = self.state.read(cx);
        let focused = state_ref.focus_handle.is_focused(window);
        let disabled = self.disabled || state_ref.disabled;
        let use_hybrid = state_ref.editable_block_refs && state_ref.has_block_refs();
        drop(state_ref); // Release borrow before mutable operations

        let theme = cx.theme();
        let input_bg = theme.input;
        let ring_color = theme.ring;
        let radius = theme.radius;

        let base = self
            .base
            .id(self.id)
            .input_py(self.size)
            // .input_px(self.size)
            .input_text_size(self.size);

        let styled = if self.appearance {
            let with_bg = base.bg(input_bg);
            let with_border = if self.bordered {
                let bordered = with_bg.border_1().border_color(input_bg);
                if focused {
                    bordered.border_color(ring_color)
                } else {
                    bordered
                }
            } else {
                with_bg
            };
            with_border.rounded(px(radius.into()))
        } else {
            base
        };

        let with_disabled = if disabled {
            styled.opacity(0.5).cursor_not_allowed()
        } else {
            styled.cursor_text()
        };

        // Build the content - either hybrid (with embedded inputs) or standard
        let cursor_visible = self.cursor_visible;
        let content: AnyElement = if use_hybrid {
            h_flex()
                .flex_wrap()
                .items_baseline()
                .gap(px(2.0))
                .children(Self::build_hybrid_content(&self.state, window, cx))
                .into_any_element()
        } else {
            MarkdownTextElement::new(self.state.clone())
                .cursor_visible(cursor_visible)
                .into_any_element()
        };

        with_disabled
            .key_context("MarkdownInput")
            .track_focus(&self.state.read(cx).focus_handle)
            .child(content)
            .on_action({
                let state = self.state.clone();
                move |action: &MoveLeft, window, cx| {
                    state.update(cx, |state, cx| state.move_left(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &MoveRight, window, cx| {
                    state.update(cx, |state, cx| state.move_right(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &MoveUp, window, cx| {
                    state.update(cx, |state, cx| state.move_up(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &MoveDown, window, cx| {
                    state.update(cx, |state, cx| state.move_down(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &MoveHome, window, cx| {
                    state.update(cx, |state, cx| state.move_home(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &MoveEnd, window, cx| {
                    state.update(cx, |state, cx| state.move_end(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &MoveToPreviousWord, window, cx| {
                    state.update(cx, |state, cx| {
                        state.move_to_previous_word(action, window, cx)
                    });
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &MoveToNextWord, window, cx| {
                    state.update(cx, |state, cx| state.move_to_next_word(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &SelectLeft, window, cx| {
                    state.update(cx, |state, cx| state.select_left(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &SelectRight, window, cx| {
                    state.update(cx, |state, cx| state.select_right(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &SelectToHome, window, cx| {
                    state.update(cx, |state, cx| state.select_to_home(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &SelectToEnd, window, cx| {
                    state.update(cx, |state, cx| state.select_to_end(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &SelectAll, window, cx| {
                    state.update(cx, |state, cx| state.select_all(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Backspace, window, cx| {
                    state.update(cx, |state, cx| state.backspace(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Delete, window, cx| {
                    state.update(cx, |state, cx| state.delete(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Enter, window, cx| {
                    state.update(cx, |state, cx| state.enter(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &InsertNewline, window, cx| {
                    state.update(cx, |state, cx| state.insert_newline(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &CreateSibling, window, cx| {
                    state.update(cx, |state, cx| {
                        state.create_sibling_action(action, window, cx)
                    });
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &CreateChild, window, cx| {
                    state.update(cx, |state, cx| state.create_child(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Copy, window, cx| {
                    state.update(cx, |state, cx| state.copy(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Cut, window, cx| {
                    state.update(cx, |state, cx| state.cut(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Paste, window, cx| {
                    state.update(cx, |state, cx| state.paste(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Undo, window, cx| {
                    state.update(cx, |state, cx| state.undo(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |action: &Redo, window, cx| {
                    state.update(cx, |state, cx| state.redo(action, window, cx));
                }
            })
            .on_action({
                let state = self.state.clone();
                move |_action: &CycleTodoState, _window, cx| {
                    state.update(cx, |_state, cx| {
                        cx.emit(MarkdownInputEvent::CycleTodo);
                    });
                }
            })
            .on_action({
                let state = self.state.clone();
                move |_action: &CycleTodoStateBack, _window, cx| {
                    state.update(cx, |_state, cx| {
                        cx.emit(MarkdownInputEvent::CycleTodoBack);
                    });
                }
            })
            .on_mouse_down(MouseButton::Left, {
                let state = self.state.clone();
                move |event: &MouseDownEvent, window, cx| {
                    if !disabled {
                        state.update(cx, |state, cx| {
                            state.on_mouse_down(event, window, cx);
                        });
                    }
                }
            })
    }
}
