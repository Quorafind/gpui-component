//! State management for MarkdownInput.
//!
//! This module provides the core state for the Markdown WYSIWYG input,
//! managing text content, cursor position, selection, and token parsing.

use gpui::{
    App, AppContext, Bounds, ClipboardItem, Context, Entity, EntityInputHandler, EventEmitter,
    FocusHandle, Focusable, KeyBinding, MouseButton, MouseDownEvent, MouseMoveEvent, MouseUpEvent,
    Pixels, Point, Render, ScrollHandle, SharedString, Subscription, UTF16Selection, Window,
    actions, point, px,
};
use ropey::Rope;
use std::ops::Range;
use std::sync::Arc;

use crate::Size;
use crate::history::History;
use crate::input::InputState;
use crate::input::RopeExt as _;
use crate::input::Selection;
use crate::input::blink_cursor::BlinkCursor;
use crate::input::change::Change;

use super::offset_map::OffsetMap;
use super::parser::MarkdownToken;

/// Callback type for when a block reference's content changes.
/// Parameters: (uuid, new_content)
pub type BlockRefChangeCallback = Arc<dyn Fn(&str, &str) + Send + Sync>;

/// Information about an embedded block reference input.
#[derive(Clone)]
pub struct EmbeddedBlockRefInput {
    /// The UUID of the referenced block.
    pub uuid: String,
    /// The MarkdownInput state for editing this block ref with full markdown support.
    pub input_state: Entity<MarkdownInputState>,
    /// Visual start position (for ordering).
    pub visual_start: usize,
}

/// Tracks focus state for embedded inputs.
#[derive(Clone, Debug, PartialEq)]
pub enum EmbeddedFocusState {
    /// Main input is focused.
    Main,
    /// An embedded block ref input is focused.
    BlockRef(String), // uuid
}

/// Events emitted by MarkdownInputState.
#[derive(Clone)]
pub enum MarkdownInputEvent {
    /// Text content changed.
    Change,
    /// Enter key pressed - split node intelligently.
    /// Contains (text_before_cursor, text_after_cursor, cursor_at_end).
    PressEnter {
        text_before: String,
        text_after: String,
        at_end: bool,
    },
    /// Alt+Enter pressed - always create sibling node.
    /// Contains (text_before_cursor, text_after_cursor).
    CreateSibling {
        text_before: String,
        text_after: String,
    },
    /// Ctrl+Shift+Enter pressed - create child node.
    CreateChild,
    /// Input gained focus.
    Focus,
    /// Input lost focus.
    Blur,
    /// Backspace pressed at the start of input.
    BackspaceAtStart,
    /// Up arrow pressed - contains cursor position (character offset).
    ArrowUp { cursor_pos: usize },
    /// Down arrow pressed - contains cursor position (character offset).
    ArrowDown { cursor_pos: usize },
    /// Up arrow pressed when cursor is at the start (position 0).
    ArrowUpAtStart,
    /// Down arrow pressed when cursor is at the end.
    ArrowDownAtEnd,
    /// Ctrl+Enter pressed - cycle todo state forward.
    CycleTodo,
    /// Ctrl+Shift+Enter pressed - cycle todo state backward (legacy, use CreateChild instead).
    CycleTodoBack,
}

actions!(
    markdown_input,
    [
        Backspace,
        Delete,
        Enter,
        InsertNewline,
        CreateSibling,
        CreateChild,
        MoveLeft,
        MoveRight,
        MoveUp,
        MoveDown,
        MoveHome,
        MoveEnd,
        MoveToPreviousWord,
        MoveToNextWord,
        SelectAll,
        SelectLeft,
        SelectRight,
        SelectToHome,
        SelectToEnd,
        Copy,
        Cut,
        Paste,
        Undo,
        Redo,
        CycleTodoState,
        CycleTodoStateBack,
    ]
);

const CONTEXT: &str = "MarkdownInput";

pub fn init(cx: &mut App) {
    cx.bind_keys([
        KeyBinding::new("backspace", Backspace, Some(CONTEXT)),
        KeyBinding::new("delete", Delete, Some(CONTEXT)),
        KeyBinding::new("enter", Enter, Some(CONTEXT)),
        KeyBinding::new("shift-enter", InsertNewline, Some(CONTEXT)),
        KeyBinding::new("alt-enter", CreateSibling, Some(CONTEXT)),
        KeyBinding::new("ctrl-shift-enter", CreateChild, Some(CONTEXT)),
        KeyBinding::new("left", MoveLeft, Some(CONTEXT)),
        KeyBinding::new("right", MoveRight, Some(CONTEXT)),
        KeyBinding::new("up", MoveUp, Some(CONTEXT)),
        KeyBinding::new("down", MoveDown, Some(CONTEXT)),
        KeyBinding::new("home", MoveHome, Some(CONTEXT)),
        KeyBinding::new("end", MoveEnd, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("alt-left", MoveToPreviousWord, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-left", MoveToPreviousWord, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("alt-right", MoveToNextWord, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-right", MoveToNextWord, Some(CONTEXT)),
        KeyBinding::new("shift-left", SelectLeft, Some(CONTEXT)),
        KeyBinding::new("shift-right", SelectRight, Some(CONTEXT)),
        KeyBinding::new("shift-home", SelectToHome, Some(CONTEXT)),
        KeyBinding::new("shift-end", SelectToEnd, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("cmd-a", SelectAll, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-a", SelectAll, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("cmd-c", Copy, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-c", Copy, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("cmd-x", Cut, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-x", Cut, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("cmd-v", Paste, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-v", Paste, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("cmd-z", Undo, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-z", Undo, Some(CONTEXT)),
        #[cfg(target_os = "macos")]
        KeyBinding::new("cmd-shift-z", Redo, Some(CONTEXT)),
        #[cfg(not(target_os = "macos"))]
        KeyBinding::new("ctrl-y", Redo, Some(CONTEXT)),
        // Todo state cycling
        KeyBinding::new("ctrl-enter", CycleTodoState, Some(CONTEXT)),
        // Note: ctrl-shift-enter is bound to CreateChild above
    ]);
}

/// State for MarkdownInput component.
pub struct MarkdownInputState {
    /// Focus handle for keyboard input.
    pub(crate) focus_handle: FocusHandle,
    /// The raw Markdown text content.
    pub(crate) text: Rope,
    /// Parsed Markdown tokens with position info.
    pub(crate) tokens: Vec<MarkdownToken>,
    /// Offset map for source ↔ visual coordinate conversion.
    pub(crate) offset_map: OffsetMap,
    /// Selected range in source coordinates (UTF-8 byte offsets).
    pub(crate) selected_range: Selection,
    /// Whether selection is reversed (start > end visually).
    pub(crate) selection_reversed: bool,
    /// Whether user is currently selecting with mouse.
    pub(crate) selecting: bool,
    /// Blink cursor state.
    pub(crate) blink_cursor: Entity<BlinkCursor>,
    /// Edit history for undo/redo.
    pub(crate) history: History<Change>,
    /// IME marked range.
    pub(crate) ime_marked_range: Option<Selection>,
    /// Scroll handle.
    pub(crate) scroll_handle: ScrollHandle,
    /// Input bounds.
    pub(crate) input_bounds: Bounds<Pixels>,
    /// Last layout bounds.
    pub(crate) last_bounds: Option<Bounds<Pixels>>,
    /// Last shaped text width for accurate cursor positioning.
    pub(crate) last_text_width: Option<Pixels>,
    /// Whether input is disabled.
    pub(crate) disabled: bool,
    /// Component size.
    pub(crate) size: Size,
    /// Placeholder text.
    pub(crate) placeholder: SharedString,
    /// Block reference contents: uuid -> display content
    /// Used to resolve ((uuid)) to actual content for display
    pub(crate) block_ref_contents: std::collections::HashMap<String, String>,

    // === Embedded Block Ref Support ===
    /// Embedded MarkdownInput states for inline block references.
    /// Key is the block ref UUID, value is the MarkdownInputState for editing referenced content.
    /// Using MarkdownInputState allows the referenced content to have its own markdown formatting.
    pub(crate) embedded_inputs: std::collections::HashMap<String, Entity<MarkdownInputState>>,
    /// Callback invoked when an embedded block ref content changes.
    /// This allows the parent to sync changes back to the original node.
    pub(crate) on_block_ref_change: Option<BlockRefChangeCallback>,
    /// Current focus state - whether main input or an embedded input is focused.
    pub(crate) embedded_focus_state: EmbeddedFocusState,
    /// Whether to use embedded inputs for block refs (vs read-only display).
    pub(crate) editable_block_refs: bool,

    /// Subscriptions.
    _subscriptions: Vec<Subscription>,
}

impl EventEmitter<MarkdownInputEvent> for MarkdownInputState {}

impl MarkdownInputState {
    /// Create a new MarkdownInputState.
    pub fn new(window: &mut Window, cx: &mut Context<Self>) -> Self {
        let focus_handle = cx.focus_handle().tab_stop(true);
        let blink_cursor = cx.new(|_| BlinkCursor::new());
        let history = History::new().group_interval(std::time::Duration::from_secs(1));

        let _subscriptions = vec![
            cx.observe(&blink_cursor, |_, _, cx| cx.notify()),
            cx.observe_window_activation(window, |input, window, cx| {
                if window.is_window_active() {
                    let focus_handle = input.focus_handle.clone();
                    if focus_handle.is_focused(window) {
                        input.blink_cursor.update(cx, |blink_cursor, cx| {
                            blink_cursor.start(cx);
                        });
                    }
                }
            }),
            cx.on_focus(&focus_handle, window, Self::on_focus),
            cx.on_blur(&focus_handle, window, Self::on_blur),
        ];

        Self {
            focus_handle,
            text: Rope::new(),
            tokens: Vec::new(),
            offset_map: OffsetMap::default(),
            selected_range: Selection::default(),
            selection_reversed: false,
            selecting: false,
            blink_cursor,
            history,
            ime_marked_range: None,
            scroll_handle: ScrollHandle::new(),
            input_bounds: Bounds::default(),
            last_bounds: None,
            last_text_width: None,
            disabled: false,
            size: Size::default(),
            placeholder: SharedString::default(),
            block_ref_contents: std::collections::HashMap::new(),
            embedded_inputs: std::collections::HashMap::new(),
            on_block_ref_change: None,
            embedded_focus_state: EmbeddedFocusState::Main,
            editable_block_refs: false,
            _subscriptions,
        }
    }

    /// Set the placeholder text.
    pub fn placeholder(mut self, placeholder: impl Into<SharedString>) -> Self {
        self.placeholder = placeholder.into();
        self
    }

    /// Set disabled state.
    pub fn disabled(mut self, disabled: bool) -> Self {
        self.disabled = disabled;
        self
    }

    /// Set the text value.
    pub fn set_value(
        &mut self,
        value: impl Into<SharedString>,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let text: SharedString = value.into();
        self.text = Rope::from(text.as_str());
        self.selected_range = (self.text.len()..self.text.len()).into();
        self.reparse_tokens();
        cx.notify();
    }

    /// Get the current text value.
    pub fn value(&self) -> SharedString {
        SharedString::new(self.text.to_string())
    }

    /// Get the cursor position in source coordinates.
    pub fn cursor(&self) -> usize {
        if let Some(ime_range) = &self.ime_marked_range {
            return ime_range.end;
        }
        if self.selection_reversed {
            self.selected_range.start
        } else {
            self.selected_range.end
        }
    }

    /// Set the cursor position in source coordinates.
    /// The offset will be clamped to the text length.
    pub fn set_cursor(&mut self, offset: usize, cx: &mut Context<Self>) {
        self.move_to(offset, cx);
    }

    /// Get the cursor's pixel position relative to the window.
    /// Returns the point where a popup menu (like slash menu) should appear.
    /// Returns None if bounds are not yet available.
    pub fn cursor_pixel_position(&self) -> Option<Point<Pixels>> {
        let bounds = self.last_bounds?;

        // Get cursor position in source coordinates
        let cursor = self.cursor();

        // Convert to visual coordinates
        let visual_cursor = self.offset_map.source_to_visual(cursor);

        // Get visual text to calculate char count
        let visual_text = self.offset_map.visual_text(&self.text.to_string());
        let char_count = visual_text.chars().count();

        // Calculate character width
        let char_width = if let Some(text_width) = self.last_text_width {
            if text_width > px(0.) && char_count > 0 {
                text_width / char_count as f32
            } else {
                px(8.0) // Fallback
            }
        } else {
            px(8.0) // Fallback
        };

        // Calculate x position: bounds origin + (visual cursor * char width)
        let cursor_x = bounds.origin.x + char_width * visual_cursor as f32;

        // Y position: below the input line
        let cursor_y = bounds.origin.y + bounds.size.height;

        Some(point(cursor_x, cursor_y))
    }

    /// Focus this input.
    pub fn focus(&self, window: &mut Window, cx: &mut Context<Self>) {
        self.focus_handle.focus(window, cx);
        self.blink_cursor.update(cx, |cursor, cx| {
            cursor.start(cx);
        });
    }

    /// Set block reference contents for resolving ((uuid)) references.
    /// Call this before or after set_value to update the display content.
    pub fn set_block_ref_contents(&mut self, contents: std::collections::HashMap<String, String>) {
        self.block_ref_contents = contents;
        self.reparse_tokens();
    }

    /// Update a single block reference content.
    pub fn update_block_ref_content(&mut self, uuid: String, content: String) {
        self.block_ref_contents
            .insert(uuid.clone(), content.clone());

        // Note: Embedded inputs are updated separately via set_value when needed
        self.reparse_tokens();
    }

    // === Embedded Block Ref Methods ===

    /// Enable or disable editable block refs.
    /// When enabled, inline block refs are rendered as embedded inputs that can be edited.
    pub fn set_editable_block_refs(&mut self, editable: bool) {
        self.editable_block_refs = editable;
    }

    /// Set the callback for when a block ref content changes.
    /// This is called whenever an embedded block ref input is edited.
    pub fn set_on_block_ref_change(&mut self, callback: BlockRefChangeCallback) {
        self.on_block_ref_change = Some(callback);
    }

    /// Check if this input has any block references in its content.
    pub fn has_block_refs(&self) -> bool {
        self.tokens.iter().any(|t| t.span.is_block_ref())
    }

    /// Check if the content is a single block ref with no other content.
    /// Returns Some(uuid) if content is just `((uuid))`, None otherwise.
    /// This is used to render single-ref nodes like child nodes instead of inline embedding.
    pub fn is_single_block_ref(&self) -> Option<String> {
        use super::parser::MarkdownSpan;

        // Must have exactly one token that is a block ref
        if self.tokens.len() != 1 {
            return None;
        }

        if let Some(token) = self.tokens.first() {
            if let MarkdownSpan::BlockRef { uuid, .. } = &token.span {
                return Some(uuid.clone());
            }
        }

        None
    }

    /// Get list of block ref UUIDs in order of appearance.
    pub fn get_block_ref_uuids(&self) -> Vec<String> {
        use super::parser::MarkdownSpan;
        self.tokens
            .iter()
            .filter_map(|t| {
                if let MarkdownSpan::BlockRef { uuid, .. } = &t.span {
                    Some(uuid.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get or create an embedded MarkdownInput state for a block ref.
    /// Returns the MarkdownInputState entity for the given UUID.
    pub fn get_or_create_embedded_input(
        &mut self,
        uuid: &str,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Entity<MarkdownInputState> {
        if let Some(state) = self.embedded_inputs.get(uuid) {
            return state.clone();
        }

        // Get initial content from block_ref_contents
        let content = self
            .block_ref_contents
            .get(uuid)
            .cloned()
            .unwrap_or_default();

        // Create new MarkdownInputState - allows the referenced content to have markdown formatting
        let input_state = cx.new(|inner_cx| {
            let mut state = MarkdownInputState::new(window, inner_cx);
            state.set_value(content, window, inner_cx);
            // Don't enable editable_block_refs for embedded inputs to avoid recursion
            state
        });

        self.embedded_inputs
            .insert(uuid.to_string(), input_state.clone());
        input_state
    }

    /// Handle embedded input change - called when an embedded block ref input's content changes.
    pub fn handle_embedded_input_change(&mut self, uuid: &str, new_content: &str) {
        // Update our local content cache
        self.block_ref_contents
            .insert(uuid.to_string(), new_content.to_string());

        // Call the callback if set
        if let Some(callback) = &self.on_block_ref_change {
            callback(uuid, new_content);
        }

        // Reparse to update visual display
        self.reparse_tokens();
    }

    /// Set focus to an embedded block ref input.
    pub fn focus_embedded(&mut self, uuid: &str, window: &mut Window, cx: &mut Context<Self>) {
        if let Some(input_state) = self.embedded_inputs.get(uuid) {
            self.embedded_focus_state = EmbeddedFocusState::BlockRef(uuid.to_string());
            input_state.update(cx, |state, inner_cx| {
                state.focus(window, inner_cx);
            });
            cx.notify();
        }
    }

    /// Return focus to the main input.
    pub fn focus_main(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        self.embedded_focus_state = EmbeddedFocusState::Main;
        self.focus(window, cx);
    }

    /// Check if any embedded input is currently focused.
    pub fn is_embedded_focused(&self) -> bool {
        matches!(self.embedded_focus_state, EmbeddedFocusState::BlockRef(_))
    }

    /// Get the UUID of the currently focused embedded input, if any.
    pub fn focused_embedded_uuid(&self) -> Option<&str> {
        match &self.embedded_focus_state {
            EmbeddedFocusState::BlockRef(uuid) => Some(uuid),
            EmbeddedFocusState::Main => None,
        }
    }

    /// Cleanup embedded inputs that are no longer referenced.
    pub fn cleanup_embedded_inputs(&mut self) {
        let current_uuids: std::collections::HashSet<_> =
            self.get_block_ref_uuids().into_iter().collect();
        self.embedded_inputs
            .retain(|uuid, _| current_uuids.contains(uuid));
    }

    /// Ensure all block refs have embedded MarkdownInput states created and subscribed.
    /// This should be called after setting block_ref_contents and before rendering.
    pub fn ensure_embedded_inputs(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        let uuids = self.get_block_ref_uuids();

        for uuid in uuids {
            // Skip if already exists
            if self.embedded_inputs.contains_key(&uuid) {
                continue;
            }

            // Get initial content
            let content = self
                .block_ref_contents
                .get(&uuid)
                .cloned()
                .unwrap_or_default();

            // Create the MarkdownInputState - allows referenced content to have markdown formatting
            let input_state = cx.new(|inner_cx| {
                let mut state = MarkdownInputState::new(window, inner_cx);
                state.set_value(&content, window, inner_cx);
                // Don't enable editable_block_refs for embedded inputs to avoid recursion
                state
            });

            // Subscribe to changes
            let uuid_for_sub = uuid.clone();
            let subscription = cx.subscribe_in(
                &input_state,
                window,
                move |this: &mut Self,
                      entity: &Entity<MarkdownInputState>,
                      event: &MarkdownInputEvent,
                      _window: &mut Window,
                      cx: &mut Context<Self>| {
                    if let MarkdownInputEvent::Change = event {
                        let new_content = entity.read(cx).value().to_string();
                        this.handle_embedded_input_change(&uuid_for_sub, &new_content);
                        cx.notify();
                    }
                },
            );

            // Store subscription
            self._subscriptions.push(subscription);

            // Store input state
            self.embedded_inputs.insert(uuid, input_state);
        }

        // Cleanup any that no longer exist
        self.cleanup_embedded_inputs();
    }

    /// Reparse tokens from current text.
    fn reparse_tokens(&mut self) {
        use super::parser::parse_inline_markdown_with_refs;

        let text_str = self.text.to_string();
        let block_refs = &self.block_ref_contents;

        // Create resolver closure that captures block_ref_contents
        let resolver = |uuid: &str| -> Option<String> { block_refs.get(uuid).cloned() };

        self.tokens = parse_inline_markdown_with_refs(&text_str, Some(&resolver));
        self.offset_map = OffsetMap::from_tokens(&self.tokens, self.cursor(), self.text.len());
    }

    /// Update offset map based on current cursor position.
    pub(crate) fn update_offset_map(&mut self) {
        self.offset_map = OffsetMap::from_tokens(&self.tokens, self.cursor(), self.text.len());
    }

    /// Find the token at the given source offset.
    pub fn token_at_offset(&self, offset: usize) -> Option<&MarkdownToken> {
        self.tokens.iter().find(|t| t.contains_cursor(offset))
    }

    /// Check if cursor is inside any formatted token.
    pub fn cursor_in_formatted_token(&self) -> bool {
        let cursor = self.cursor();
        self.tokens
            .iter()
            .any(|t| t.span.has_syntax() && t.contains_cursor(cursor))
    }

    // === Event Handlers ===

    fn on_focus(&mut self, _window: &mut Window, cx: &mut Context<Self>) {
        self.blink_cursor.update(cx, |cursor, cx| {
            cursor.start(cx);
        });
        cx.emit(MarkdownInputEvent::Focus);
    }

    fn on_blur(&mut self, _window: &mut Window, cx: &mut Context<Self>) {
        self.blink_cursor.update(cx, |cursor, cx| {
            cursor.stop(cx);
        });
        cx.emit(MarkdownInputEvent::Blur);
        cx.notify();
    }

    pub(crate) fn on_mouse_down(
        &mut self,
        event: &MouseDownEvent,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        if event.button != MouseButton::Left {
            return;
        }

        self.selecting = true;
        let offset = self.index_for_position(event.position);

        if event.modifiers.shift {
            self.select_to(offset, cx);
        } else {
            self.move_to(offset, cx);
        }

        self.focus(window, cx);
    }

    pub(crate) fn on_mouse_up(
        &mut self,
        _event: &MouseUpEvent,
        _window: &mut Window,
        _cx: &mut Context<Self>,
    ) {
        self.selecting = false;
    }

    pub(crate) fn on_mouse_move(
        &mut self,
        event: &MouseMoveEvent,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        if !self.selecting {
            return;
        }

        let offset = self.index_for_position(event.position);
        self.select_to(offset, cx);
    }

    fn index_for_position(&self, position: Point<Pixels>) -> usize {
        // Convert click position to source byte offset
        let Some(bounds) = self.last_bounds else {
            return 0;
        };

        // Check Y coordinate - if outside vertical bounds, clamp to start/end
        // This prevents incorrect cursor positioning when clicking/dragging from lines above/below
        if position.y < bounds.origin.y {
            return 0; // Above element - return start
        }
        if position.y > bounds.origin.y + bounds.size.height {
            return self.text.len(); // Below element - return end
        }

        let relative_x = position.x - bounds.origin.x;
        if relative_x <= px(0.) {
            return 0;
        }

        // Get the visible text (without hidden markdown syntax)
        let visual_text = self.offset_map.visual_text(&self.text.to_string());
        let char_count = visual_text.chars().count();
        if char_count == 0 {
            return 0;
        }

        // Use actual text width if available, otherwise fall back to approximation
        let char_width = if let Some(text_width) = self.last_text_width {
            if text_width > px(0.) {
                text_width / char_count as f32
            } else {
                px(8.0) // Fallback
            }
        } else {
            px(8.0) // Fallback
        };

        // Calculate character index from click position
        let char_index = (relative_x / char_width).round() as usize;
        let clamped_char_index = char_index.min(char_count);

        // Convert character index to byte offset in visual text
        let visual_byte_offset = visual_text
            .char_indices()
            .nth(clamped_char_index)
            .map(|(i, _)| i)
            .unwrap_or(visual_text.len());

        // Convert visual byte offset to source byte offset
        self.offset_map.visual_to_source(visual_byte_offset)
    }

    // === Cursor Movement ===

    fn move_to(&mut self, offset: usize, cx: &mut Context<Self>) {
        let offset = offset.min(self.text.len());
        self.selected_range = (offset..offset).into();
        self.selection_reversed = false;
        self.update_offset_map();
        self.pause_blink_cursor(cx);
        cx.notify();
    }

    fn select_to(&mut self, offset: usize, cx: &mut Context<Self>) {
        let offset = offset.min(self.text.len());

        if self.selection_reversed {
            self.selected_range.start = offset;
        } else {
            self.selected_range.end = offset;
        }

        if self.selected_range.end < self.selected_range.start {
            self.selection_reversed = !self.selection_reversed;
            self.selected_range = (self.selected_range.end..self.selected_range.start).into();
        }

        self.update_offset_map();
        cx.notify();
    }

    pub(crate) fn move_left(&mut self, _: &MoveLeft, _window: &mut Window, cx: &mut Context<Self>) {
        let cursor = self.cursor();
        if cursor > 0 {
            let new_pos = self.previous_boundary(cursor);
            self.move_to(new_pos, cx);
        }
    }

    pub(crate) fn move_right(
        &mut self,
        _: &MoveRight,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let cursor = self.cursor();
        if cursor < self.text.len() {
            let new_pos = self.next_boundary(cursor);
            self.move_to(new_pos, cx);
        }
    }

    pub(crate) fn move_home(&mut self, _: &MoveHome, _window: &mut Window, cx: &mut Context<Self>) {
        self.move_to(0, cx);
    }

    pub(crate) fn move_end(&mut self, _: &MoveEnd, _window: &mut Window, cx: &mut Context<Self>) {
        self.move_to(self.text.len(), cx);
    }

    pub(crate) fn move_up(&mut self, _: &MoveUp, _window: &mut Window, cx: &mut Context<Self>) {
        let cursor_pos = self.cursor();
        println!(
            "[DEBUG] MarkdownInput::move_up called, cursor={}, text_len={}",
            cursor_pos,
            self.text.len()
        );
        // Always emit ArrowUp with cursor position for cross-node navigation
        cx.emit(MarkdownInputEvent::ArrowUp { cursor_pos });
        // Also emit legacy event for backwards compatibility
        if cursor_pos == 0 {
            println!("[DEBUG] MarkdownInput::move_up -> emitting ArrowUpAtStart");
            cx.emit(MarkdownInputEvent::ArrowUpAtStart);
        }
    }

    pub(crate) fn move_down(&mut self, _: &MoveDown, _window: &mut Window, cx: &mut Context<Self>) {
        let cursor_pos = self.cursor();
        println!(
            "[DEBUG] MarkdownInput::move_down called, cursor={}, text_len={}",
            cursor_pos,
            self.text.len()
        );
        // Always emit ArrowDown with cursor position for cross-node navigation
        cx.emit(MarkdownInputEvent::ArrowDown { cursor_pos });
        // Also emit legacy event for backwards compatibility
        if cursor_pos >= self.text.len() {
            println!("[DEBUG] MarkdownInput::move_down -> emitting ArrowDownAtEnd");
            cx.emit(MarkdownInputEvent::ArrowDownAtEnd);
        }
    }

    pub(crate) fn move_to_previous_word(
        &mut self,
        _: &MoveToPreviousWord,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let new_pos = self.previous_word_boundary(self.cursor());
        self.move_to(new_pos, cx);
    }

    pub(crate) fn move_to_next_word(
        &mut self,
        _: &MoveToNextWord,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let new_pos = self.next_word_boundary(self.cursor());
        self.move_to(new_pos, cx);
    }

    pub(crate) fn select_all(
        &mut self,
        _: &SelectAll,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        self.selected_range = (0..self.text.len()).into();
        self.selection_reversed = false;
        cx.notify();
    }

    pub(crate) fn select_left(
        &mut self,
        _: &SelectLeft,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let cursor = self.cursor();
        if cursor > 0 {
            self.select_to(self.previous_boundary(cursor), cx);
        }
    }

    pub(crate) fn select_right(
        &mut self,
        _: &SelectRight,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let cursor = self.cursor();
        if cursor < self.text.len() {
            self.select_to(self.next_boundary(cursor), cx);
        }
    }

    pub(crate) fn select_to_home(
        &mut self,
        _: &SelectToHome,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        self.select_to(0, cx);
    }

    pub(crate) fn select_to_end(
        &mut self,
        _: &SelectToEnd,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        self.select_to(self.text.len(), cx);
    }

    fn previous_boundary(&self, offset: usize) -> usize {
        self.text
            .clip_offset(offset.saturating_sub(1), sum_tree::Bias::Left)
    }

    fn next_boundary(&self, offset: usize) -> usize {
        self.text.clip_offset(offset + 1, sum_tree::Bias::Right)
    }

    fn previous_word_boundary(&self, offset: usize) -> usize {
        if offset == 0 {
            return 0;
        }
        let text_str = self.text.to_string();
        let left_part = &text_str[..offset];

        use unicode_segmentation::UnicodeSegmentation;
        UnicodeSegmentation::split_word_bound_indices(left_part)
            .filter(|(_, s)| !s.chars().all(|c| c.is_whitespace()))
            .last()
            .map(|(i, _)| i)
            .unwrap_or(0)
    }

    fn next_word_boundary(&self, offset: usize) -> usize {
        let len = self.text.len();
        if offset >= len {
            return len;
        }
        let text_str = self.text.to_string();
        let right_part = &text_str[offset..];

        use unicode_segmentation::UnicodeSegmentation;
        UnicodeSegmentation::split_word_bound_indices(right_part)
            .skip(1)
            .find(|(_, s)| !s.chars().all(|c| c.is_whitespace()))
            .map(|(i, _)| offset + i)
            .unwrap_or(len)
    }

    // === Editing ===

    pub(crate) fn backspace(&mut self, _: &Backspace, window: &mut Window, cx: &mut Context<Self>) {
        if self.selected_range.is_empty() && self.cursor() == 0 {
            cx.emit(MarkdownInputEvent::BackspaceAtStart);
            return;
        }

        if self.selected_range.is_empty() {
            let cursor = self.cursor();
            let new_start = self.previous_boundary(cursor);
            self.selected_range = (new_start..cursor).into();
        }

        self.replace_text_in_range(None, "", window, cx);
    }

    pub(crate) fn delete(&mut self, _: &Delete, window: &mut Window, cx: &mut Context<Self>) {
        if self.selected_range.is_empty() {
            let cursor = self.cursor();
            let new_end = self.next_boundary(cursor);
            self.selected_range = (cursor..new_end).into();
        }

        self.replace_text_in_range(None, "", window, cx);
    }

    pub(crate) fn enter(&mut self, _: &Enter, _window: &mut Window, cx: &mut Context<Self>) {
        let cursor = self.cursor();
        let text = self.text.to_string();
        let text_before = text[..cursor].to_string();
        let text_after = text[cursor..].to_string();
        let at_end = cursor >= text.len();

        cx.emit(MarkdownInputEvent::PressEnter {
            text_before,
            text_after,
            at_end,
        });
    }

    pub(crate) fn create_sibling_action(
        &mut self,
        _: &CreateSibling,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let cursor = self.cursor();
        let text = self.text.to_string();
        let text_before = text[..cursor].to_string();
        let text_after = text[cursor..].to_string();

        cx.emit(MarkdownInputEvent::CreateSibling {
            text_before,
            text_after,
        });
    }

    pub(crate) fn insert_newline(
        &mut self,
        _: &InsertNewline,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        // Insert a newline character at the current cursor position
        self.replace_text_in_range(None, "\n", window, cx);
    }

    pub(crate) fn create_child(
        &mut self,
        _: &CreateChild,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        cx.emit(MarkdownInputEvent::CreateChild);
    }

    pub(crate) fn copy(&mut self, _: &Copy, _window: &mut Window, cx: &mut Context<Self>) {
        if self.selected_range.is_empty() {
            return;
        }

        let selected_text = self.text.slice(self.selected_range).to_string();
        cx.write_to_clipboard(ClipboardItem::new_string(selected_text));
    }

    pub(crate) fn cut(&mut self, _: &Cut, window: &mut Window, cx: &mut Context<Self>) {
        if self.selected_range.is_empty() {
            return;
        }

        let selected_text = self.text.slice(self.selected_range).to_string();
        cx.write_to_clipboard(ClipboardItem::new_string(selected_text));
        self.replace_text_in_range(None, "", window, cx);
    }

    pub(crate) fn paste(&mut self, _: &Paste, window: &mut Window, cx: &mut Context<Self>) {
        if let Some(clipboard) = cx.read_from_clipboard() {
            let text = clipboard.text().unwrap_or_default().replace('\n', " ");
            self.replace_text_in_range(None, &text, window, cx);
        }
    }

    pub(crate) fn undo(&mut self, _: &Undo, window: &mut Window, cx: &mut Context<Self>) {
        self.history.ignore = true;
        if let Some(changes) = self.history.undo() {
            for change in changes {
                let range_utf16 = self.range_to_utf16(&change.new_range.into());
                self.replace_text_in_range(Some(range_utf16), &change.old_text, window, cx);
            }
        }
        self.history.ignore = false;
    }

    pub(crate) fn redo(&mut self, _: &Redo, window: &mut Window, cx: &mut Context<Self>) {
        self.history.ignore = true;
        if let Some(changes) = self.history.redo() {
            for change in changes {
                let range_utf16 = self.range_to_utf16(&change.old_range.into());
                self.replace_text_in_range(Some(range_utf16), &change.new_text, window, cx);
            }
        }
        self.history.ignore = false;
    }

    fn push_history(&mut self, text: &Rope, range: &Range<usize>, new_text: &str) {
        if self.history.ignore {
            return;
        }

        let range = text.clip_offset(range.start, sum_tree::Bias::Left)
            ..text.clip_offset(range.end, sum_tree::Bias::Right);
        let old_text = text.slice(range.clone()).to_string();
        let new_range = range.start..range.start + new_text.len();

        self.history
            .push(Change::new(range, &old_text, new_range, new_text));
    }

    fn pause_blink_cursor(&mut self, cx: &mut Context<Self>) {
        self.blink_cursor.update(cx, |cursor, cx| {
            cursor.pause(cx);
        });
    }

    pub(crate) fn show_cursor(&self, window: &Window, cx: &App) -> bool {
        self.focus_handle.is_focused(window)
            && !self.disabled
            && self.blink_cursor.read(cx).visible()
            && window.is_window_active()
    }

    // === UTF-16 Conversion ===

    fn offset_from_utf16(&self, offset: usize) -> usize {
        self.text.offset_utf16_to_offset(offset)
    }

    fn offset_to_utf16(&self, offset: usize) -> usize {
        self.text.offset_to_offset_utf16(offset)
    }

    fn range_to_utf16(&self, range: &Range<usize>) -> Range<usize> {
        self.offset_to_utf16(range.start)..self.offset_to_utf16(range.end)
    }

    fn range_from_utf16(&self, range: &Range<usize>) -> Range<usize> {
        self.offset_from_utf16(range.start)..self.offset_from_utf16(range.end)
    }
}

impl EntityInputHandler for MarkdownInputState {
    fn text_for_range(
        &mut self,
        range_utf16: Range<usize>,
        adjusted_range: &mut Option<Range<usize>>,
        _window: &mut Window,
        _cx: &mut Context<Self>,
    ) -> Option<String> {
        let range = self.range_from_utf16(&range_utf16);
        adjusted_range.replace(self.range_to_utf16(&range));
        Some(self.text.slice(range).to_string())
    }

    fn selected_text_range(
        &mut self,
        _ignore_disabled: bool,
        _window: &mut Window,
        _cx: &mut Context<Self>,
    ) -> Option<UTF16Selection> {
        Some(UTF16Selection {
            range: self.range_to_utf16(&self.selected_range.into()),
            reversed: self.selection_reversed,
        })
    }

    fn marked_text_range(
        &self,
        _window: &mut Window,
        _cx: &mut Context<Self>,
    ) -> Option<Range<usize>> {
        self.ime_marked_range
            .map(|range| self.range_to_utf16(&range.into()))
    }

    fn unmark_text(&mut self, _window: &mut Window, _cx: &mut Context<Self>) {
        self.ime_marked_range = None;
    }

    fn replace_text_in_range(
        &mut self,
        range_utf16: Option<Range<usize>>,
        new_text: &str,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        if self.disabled {
            return;
        }

        self.pause_blink_cursor(cx);

        let range = range_utf16
            .as_ref()
            .map(|r| self.range_from_utf16(r))
            .or(self.ime_marked_range.map(|r| r.into()))
            .unwrap_or(self.selected_range.into());

        let old_text = self.text.clone();
        self.text.replace(range.clone(), new_text);

        let new_offset = (range.start + new_text.len()).min(self.text.len());
        self.push_history(&old_text, &range, new_text);
        self.history.end_grouping();

        self.selected_range = (new_offset..new_offset).into();
        self.ime_marked_range = None;
        self.reparse_tokens();

        cx.emit(MarkdownInputEvent::Change);
        cx.notify();
    }

    fn replace_and_mark_text_in_range(
        &mut self,
        range_utf16: Option<Range<usize>>,
        new_text: &str,
        new_selected_range_utf16: Option<Range<usize>>,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        if self.disabled {
            return;
        }

        let range = range_utf16
            .as_ref()
            .map(|r| self.range_from_utf16(r))
            .or(self.ime_marked_range.map(|r| r.into()))
            .unwrap_or(self.selected_range.into());

        let old_text = self.text.clone();
        self.text.replace(range.clone(), new_text);

        if new_text.is_empty() {
            self.selected_range = (range.start..range.start).into();
            self.ime_marked_range = None;
        } else {
            self.ime_marked_range = Some((range.start..range.start + new_text.len()).into());
            self.selected_range = new_selected_range_utf16
                .as_ref()
                .map(|r| self.range_from_utf16(r))
                .map(|r| r.start + range.start..r.end + range.end)
                .unwrap_or_else(|| range.start + new_text.len()..range.start + new_text.len())
                .into();
        }

        self.history.start_grouping();
        self.push_history(&old_text, &range, new_text);
        self.reparse_tokens();
        cx.notify();
    }

    fn bounds_for_range(
        &mut self,
        _range_utf16: Range<usize>,
        bounds: Bounds<Pixels>,
        _window: &mut Window,
        _cx: &mut Context<Self>,
    ) -> Option<Bounds<Pixels>> {
        Some(bounds)
    }

    fn character_index_for_point(
        &mut self,
        point: Point<Pixels>,
        _window: &mut Window,
        _cx: &mut Context<Self>,
    ) -> Option<usize> {
        let bounds = self.last_bounds?;
        let relative_x = point.x - bounds.origin.x;
        if relative_x <= px(0.) {
            return Some(0);
        }

        let approx_char_width = px(8.0);
        let visual_offset = (relative_x / approx_char_width).floor() as usize;
        let source_offset = self.offset_map.visual_to_source(visual_offset);
        Some(self.offset_to_utf16(source_offset))
    }
}

impl Focusable for MarkdownInputState {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl Render for MarkdownInputState {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl gpui::IntoElement {
        use gpui::{ParentElement, Styled, div};

        div()
            .flex_1()
            .child(super::MarkdownTextElement::new(cx.entity().clone()))
    }
}
