//! Offset mapping between source text and visual display.
//!
//! When Markdown syntax is hidden, the visual position of characters differs
//! from their position in the source text. This module provides bidirectional
//! mapping between these coordinate spaces.

use std::ops::Range;

use super::parser::MarkdownToken;

/// A segment in the offset map.
#[derive(Debug, Clone)]
struct Segment {
    /// Start offset in source text.
    source_start: usize,
    /// Start offset in visual display.
    visual_start: usize,
    /// Length of this segment.
    len: usize,
    /// Whether this segment contains hidden syntax.
    is_syntax: bool,
}

/// Maps between source text offsets and visual display offsets.
///
/// When Markdown syntax markers (like `**` for bold) are hidden,
/// the visual position of text differs from its source position.
/// This struct provides bidirectional mapping.
#[derive(Debug, Clone, Default)]
pub struct OffsetMap {
    segments: Vec<Segment>,
    /// Total length of source text.
    source_len: usize,
    /// Total length of visual text.
    visual_len: usize,
}

impl OffsetMap {
    /// Create an offset map from parsed tokens and cursor position.
    ///
    /// Tokens whose syntax should be visible (cursor is inside) are treated
    /// as having no hidden content.
    pub fn from_tokens(tokens: &[MarkdownToken], cursor: usize, source_len: usize) -> Self {
        let mut segments = Vec::new();
        let mut visual_offset = 0;
        let mut source_offset = 0;

        for token in tokens {
            // Fill any gap before this token (shouldn't happen with proper parsing)
            if token.start > source_offset {
                let gap_len = token.start - source_offset;
                segments.push(Segment {
                    source_start: source_offset,
                    visual_start: visual_offset,
                    len: gap_len,
                    is_syntax: false,
                });
                visual_offset += gap_len;
                source_offset = token.start;
            }

            let cursor_in_token = token.contains_cursor(cursor);
            let has_syntax = token.span.has_syntax();

            if has_syntax && !cursor_in_token {
                // Syntax is hidden - only content is visible
                let prefix_len = token.span.prefix_len();
                let suffix_len = token.span.suffix_len();
                let is_block_ref = token.span.is_block_ref();

                // Hidden prefix
                if prefix_len > 0 {
                    segments.push(Segment {
                        source_start: source_offset,
                        visual_start: visual_offset,
                        len: prefix_len,
                        is_syntax: true,
                    });
                    source_offset += prefix_len;
                }

                // Visible content
                // For block refs, visual_len may differ from content_len
                if token.visual_len > 0 {
                    segments.push(Segment {
                        source_start: source_offset,
                        visual_start: visual_offset,
                        len: if is_block_ref {
                            token.visual_len
                        } else {
                            token.content_len
                        },
                        is_syntax: false,
                    });
                    visual_offset += token.visual_len;
                    source_offset += token.content_len; // Source still advances by content_len
                }

                // Hidden suffix
                if suffix_len > 0 {
                    segments.push(Segment {
                        source_start: source_offset,
                        visual_start: visual_offset,
                        len: suffix_len,
                        is_syntax: true,
                    });
                    source_offset += suffix_len;
                }
            } else {
                // Cursor is in token or no syntax - everything visible
                let token_len = token.end - token.start;
                segments.push(Segment {
                    source_start: source_offset,
                    visual_start: visual_offset,
                    len: token_len,
                    is_syntax: false,
                });
                visual_offset += token_len;
                source_offset += token_len;
            }
        }

        // Handle any remaining text after last token
        if source_offset < source_len {
            let remaining = source_len - source_offset;
            segments.push(Segment {
                source_start: source_offset,
                visual_start: visual_offset,
                len: remaining,
                is_syntax: false,
            });
            visual_offset += remaining;
        }

        Self {
            segments,
            source_len,
            visual_len: visual_offset,
        }
    }

    /// Convert a source offset to a visual offset.
    ///
    /// If the source offset is within hidden syntax, returns the visual offset
    /// at the boundary of the hidden region.
    pub fn source_to_visual(&self, source_offset: usize) -> usize {
        if self.segments.is_empty() {
            return source_offset.min(self.visual_len);
        }

        for segment in &self.segments {
            let segment_end = segment.source_start + segment.len;

            if source_offset < segment.source_start {
                return segment.visual_start;
            }

            if source_offset <= segment_end {
                if segment.is_syntax {
                    // Within hidden syntax - return start of visual position
                    return segment.visual_start;
                } else {
                    // Within visible segment
                    let offset_in_segment = source_offset - segment.source_start;
                    return segment.visual_start + offset_in_segment;
                }
            }
        }

        self.visual_len
    }

    /// Convert a visual offset to a source offset.
    ///
    /// Returns the source offset corresponding to the visual position.
    pub fn visual_to_source(&self, visual_offset: usize) -> usize {
        if self.segments.is_empty() {
            return visual_offset.min(self.source_len);
        }

        for segment in &self.segments {
            if segment.is_syntax {
                continue; // Skip hidden segments
            }

            let visual_end = segment.visual_start + segment.len;

            if visual_offset <= visual_end {
                if visual_offset < segment.visual_start {
                    return segment.source_start;
                }
                let offset_in_segment = visual_offset - segment.visual_start;
                return segment.source_start + offset_in_segment;
            }
        }

        // Return source_len - cursor can be at end of text (after last char)
        self.source_len
    }

    /// Get the visual length (total visible characters).
    pub fn visual_len(&self) -> usize {
        self.visual_len
    }

    /// Get the source length.
    pub fn source_len(&self) -> usize {
        self.source_len
    }

    /// Convert a source range to a visual range.
    pub fn source_range_to_visual(&self, range: Range<usize>) -> Range<usize> {
        self.source_to_visual(range.start)..self.source_to_visual(range.end)
    }

    /// Convert a visual range to a source range.
    pub fn visual_range_to_source(&self, range: Range<usize>) -> Range<usize> {
        self.visual_to_source(range.start)..self.visual_to_source(range.end)
    }

    /// Get the visible text (with hidden markdown syntax removed).
    /// Note: For block refs, this extracts from source which contains the UUID,
    /// not the display content. Use build_display_text_and_runs for actual display.
    pub fn visual_text(&self, source: &str) -> String {
        if self.segments.is_empty() {
            return source.to_string();
        }

        let mut result = String::new();
        for segment in &self.segments {
            if !segment.is_syntax {
                // This is visible content
                let end = (segment.source_start + segment.len).min(source.len());
                if segment.source_start < source.len() {
                    result.push_str(&source[segment.source_start..end]);
                }
            }
        }
        result
    }

    /// Get visual text using tokens for accurate block ref display.
    /// This properly handles block refs by using their display_content.
    pub fn visual_text_from_tokens(
        tokens: &[super::parser::MarkdownToken],
        cursor: usize,
        source: &str,
    ) -> String {
        let mut result = String::new();

        for token in tokens {
            let cursor_in_token = token.contains_cursor(cursor);
            let has_syntax = token.span.has_syntax();

            if has_syntax && !cursor_in_token {
                // Syntax is hidden - show visible content
                result.push_str(token.span.visible_text());
            } else {
                // Cursor inside or plain text - show full source
                if token.end <= source.len() {
                    result.push_str(&source[token.start..token.end]);
                }
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::markdown_input::parser::parse_inline_markdown;

    #[test]
    fn test_plain_text_mapping() {
        let tokens = parse_inline_markdown("Hello world");
        let map = OffsetMap::from_tokens(&tokens, 0, 11);

        assert_eq!(map.source_to_visual(0), 0);
        assert_eq!(map.source_to_visual(5), 5);
        assert_eq!(map.source_to_visual(11), 11);

        assert_eq!(map.visual_to_source(0), 0);
        assert_eq!(map.visual_to_source(5), 5);
    }

    #[test]
    fn test_bold_hidden() {
        // "**bold**" with cursor outside
        let tokens = parse_inline_markdown("**bold**");
        let map = OffsetMap::from_tokens(&tokens, 100, 8); // cursor far away

        // Visual: "bold" (4 chars)
        // Source: "**bold**" (8 chars)
        assert_eq!(map.visual_len(), 4);

        // Source offset 0 (at first *) -> visual 0
        assert_eq!(map.source_to_visual(0), 0);
        // Source offset 2 (at 'b') -> visual 0
        assert_eq!(map.source_to_visual(2), 0);
        // Source offset 3 (at 'o') -> visual 1
        assert_eq!(map.source_to_visual(3), 1);
    }

    #[test]
    fn test_bold_visible_when_cursor_inside() {
        // "**bold**" with cursor inside
        let tokens = parse_inline_markdown("**bold**");
        let map = OffsetMap::from_tokens(&tokens, 4, 8); // cursor in "bold"

        // Visual: "**bold**" (8 chars) - syntax visible
        assert_eq!(map.visual_len(), 8);
        assert_eq!(map.source_to_visual(0), 0);
        assert_eq!(map.source_to_visual(4), 4);
    }

    #[test]
    fn test_mixed_content() {
        // "Hello **world**" with cursor outside bold
        let tokens = parse_inline_markdown("Hello **world**");
        let map = OffsetMap::from_tokens(&tokens, 3, 15); // cursor in "Hello"

        // Visual: "Hello world" (11 chars)
        // "Hello " = 6, "world" = 5
        assert_eq!(map.visual_len(), 11);

        // "Hello " maps 1:1
        assert_eq!(map.source_to_visual(5), 5);
        // "**world**" starts at source 6, visual 6
        // Source 6 is *, should map to visual 6 (start of content)
        assert_eq!(map.source_to_visual(6), 6);
        // Source 8 is 'w', should map to visual 6
        assert_eq!(map.source_to_visual(8), 6);
    }
}
