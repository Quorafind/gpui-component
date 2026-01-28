//! Markdown inline parser with position tracking.
//!
//! This module provides a parser for inline Markdown syntax that tracks
//! both the full span range (including syntax markers) and the content range.

use std::ops::Range;

/// Represents a type of Markdown formatting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MarkdownSpan {
    /// Plain text without formatting.
    Text(String),
    /// Bold text: **text** or __text__
    Bold(String),
    /// Italic text: *text* or _text_
    Italic(String),
    /// Bold and italic text: ***text*** or ___text___
    BoldItalic(String),
    /// Inline code: `code`
    Code(String),
    /// Link: [text](url)
    Link { text: String, url: String },
    /// Strikethrough text: ~~text~~
    Strikethrough(String),
    /// Highlighted text: ==text== (extended GFM)
    Highlight(String),
    /// Image: ![alt](url)
    Image { alt: String, url: String },
    /// Block reference: ((uuid)) - displays content from another block
    /// The uuid is stored, and display_content is resolved at render time
    BlockRef {
        uuid: String,
        display_content: String,
    },
}

impl MarkdownSpan {
    /// Returns the visible text content (without syntax markers).
    pub fn visible_text(&self) -> &str {
        match self {
            MarkdownSpan::Text(s) => s,
            MarkdownSpan::Bold(s) => s,
            MarkdownSpan::Italic(s) => s,
            MarkdownSpan::BoldItalic(s) => s,
            MarkdownSpan::Code(s) => s,
            MarkdownSpan::Link { text, .. } => text,
            MarkdownSpan::Strikethrough(s) => s,
            MarkdownSpan::Highlight(s) => s,
            MarkdownSpan::Image { alt, .. } => alt,
            MarkdownSpan::BlockRef {
                display_content, ..
            } => display_content,
        }
    }

    /// Returns the number of syntax characters at the start.
    pub fn prefix_len(&self) -> usize {
        match self {
            MarkdownSpan::Text(_) => 0,
            MarkdownSpan::Bold(_) => 2,          // **
            MarkdownSpan::Italic(_) => 1,        // *
            MarkdownSpan::BoldItalic(_) => 3,    // ***
            MarkdownSpan::Code(_) => 1,          // `
            MarkdownSpan::Link { .. } => 1,      // [
            MarkdownSpan::Strikethrough(_) => 2, // ~~
            MarkdownSpan::Highlight(_) => 2,     // ==
            MarkdownSpan::Image { .. } => 2,     // ![
            MarkdownSpan::BlockRef { .. } => 2,  // ((
        }
    }

    /// Returns the number of syntax characters at the end.
    pub fn suffix_len(&self) -> usize {
        match self {
            MarkdownSpan::Text(_) => 0,
            MarkdownSpan::Bold(_) => 2,                           // **
            MarkdownSpan::Italic(_) => 1,                         // *
            MarkdownSpan::BoldItalic(_) => 3,                     // ***
            MarkdownSpan::Code(_) => 1,                           // `
            MarkdownSpan::Link { url, .. } => 2 + url.len() + 1,  // ](url)
            MarkdownSpan::Strikethrough(_) => 2,                  // ~~
            MarkdownSpan::Highlight(_) => 2,                      // ==
            MarkdownSpan::Image { url, .. } => 2 + url.len() + 1, // ](url)
            MarkdownSpan::BlockRef { .. } => 2,                   // ))
        }
    }

    /// Returns true if this span has syntax markers that can be hidden.
    pub fn has_syntax(&self) -> bool {
        !matches!(self, MarkdownSpan::Text(_))
    }

    /// Returns true if this is a block reference span.
    pub fn is_block_ref(&self) -> bool {
        matches!(self, MarkdownSpan::BlockRef { .. })
    }
}

/// A parsed Markdown token with position information.
#[derive(Debug, Clone)]
pub struct MarkdownToken {
    /// The parsed span type and content.
    pub span: MarkdownSpan,
    /// Byte position where the span starts (including syntax markers like `**`).
    pub start: usize,
    /// Byte position where the span ends (after closing syntax markers).
    pub end: usize,
    /// Byte position where the visible content starts (after opening markers).
    pub content_start: usize,
    /// Byte length of the source content (between markers).
    pub content_len: usize,
    /// Byte length of the visual content (may differ from content_len for BlockRef).
    /// For most spans, this equals content_len. For BlockRef, it's display_content.len().
    pub visual_len: usize,
}

impl MarkdownToken {
    /// Returns the full range including syntax markers.
    pub fn full_range(&self) -> Range<usize> {
        self.start..self.end
    }

    /// Returns the range of visible content only.
    pub fn content_range(&self) -> Range<usize> {
        self.content_start..self.content_start + self.content_len
    }

    /// Check if a cursor position is within or at the edge of this token.
    pub fn contains_cursor(&self, cursor: usize) -> bool {
        cursor >= self.start && cursor <= self.end
    }

    /// Check if the cursor is strictly inside the content (not at edges).
    pub fn cursor_in_content(&self, cursor: usize) -> bool {
        cursor > self.content_start && cursor < self.content_start + self.content_len
    }
}

/// Parse inline Markdown text into tokens with position information.
/// The block_ref_resolver is an optional function to resolve block reference UUIDs to display content.
pub fn parse_inline_markdown(input: &str) -> Vec<MarkdownToken> {
    parse_inline_markdown_with_refs(input, None)
}

/// Parse inline Markdown with optional block reference resolution.
pub fn parse_inline_markdown_with_refs(
    input: &str,
    block_ref_resolver: Option<&dyn Fn(&str) -> Option<String>>,
) -> Vec<MarkdownToken> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut pos = 0;

    while pos < chars.len() {
        // Try to parse block reference: ((uuid))
        if chars[pos] == '(' && chars.get(pos + 1) == Some(&'(') {
            if let Some((uuid, end_pos)) = parse_block_ref(&chars, pos) {
                let byte_start = chars[..pos].iter().collect::<String>().len();
                let byte_end = chars[..end_pos].iter().collect::<String>().len();
                let content_start = byte_start + 2; // after '(('
                let content_len = uuid.len();

                // Resolve display content if resolver provided
                let display_content = block_ref_resolver
                    .and_then(|resolver| resolver(&uuid))
                    .unwrap_or_else(|| uuid.clone());

                // Visual length is the display content length, not UUID length
                let visual_len = display_content.len();

                tokens.push(MarkdownToken {
                    span: MarkdownSpan::BlockRef {
                        uuid,
                        display_content,
                    },
                    start: byte_start,
                    end: byte_end,
                    content_start,
                    content_len,
                    visual_len,
                });
                pos = end_pos;
                continue;
            }
        }

        // Try to parse image: ![alt](url) - check before link since '!' + '[' is more specific
        if chars[pos] == '!' && chars.get(pos + 1) == Some(&'[') {
            if let Some((alt, url, end_pos)) = parse_image(&chars, pos) {
                let byte_start = chars[..pos].iter().collect::<String>().len();
                let byte_end = chars[..end_pos].iter().collect::<String>().len();
                let content_start = byte_start + 2; // after '!['
                let content_len = alt.len();
                tokens.push(MarkdownToken {
                    span: MarkdownSpan::Image { alt, url },
                    start: byte_start,
                    end: byte_end,
                    content_start,
                    content_len,
                    visual_len: content_len,
                });
                pos = end_pos;
                continue;
            }
        }

        // Try to parse link: [text](url)
        if chars[pos] == '[' {
            if let Some((text, url, end_pos)) = parse_link(&chars, pos) {
                let byte_start = chars[..pos].iter().collect::<String>().len();
                let byte_end = chars[..end_pos].iter().collect::<String>().len();
                let content_start = byte_start + 1; // after '['
                let content_len = text.len();
                tokens.push(MarkdownToken {
                    span: MarkdownSpan::Link { text, url },
                    start: byte_start,
                    end: byte_end,
                    content_start,
                    content_len,
                    visual_len: content_len,
                });
                pos = end_pos;
                continue;
            }
        }

        // Try to parse code: `code`
        if chars[pos] == '`' {
            if let Some((code, end_pos)) = parse_code(&chars, pos) {
                let byte_start = chars[..pos].iter().collect::<String>().len();
                let byte_end = chars[..end_pos].iter().collect::<String>().len();
                let content_start = byte_start + 1; // after '`'
                let content_len = code.len();
                tokens.push(MarkdownToken {
                    span: MarkdownSpan::Code(code),
                    start: byte_start,
                    end: byte_end,
                    content_start,
                    content_len,
                    visual_len: content_len,
                });
                pos = end_pos;
                continue;
            }
        }

        // Try to parse strikethrough: ~~text~~
        if chars[pos] == '~' {
            if let Some((text, end_pos)) = parse_strikethrough(&chars, pos) {
                let byte_start = chars[..pos].iter().collect::<String>().len();
                let byte_end = chars[..end_pos].iter().collect::<String>().len();
                let content_start = byte_start + 2; // after '~~'
                let content_len = text.len();
                tokens.push(MarkdownToken {
                    span: MarkdownSpan::Strikethrough(text),
                    start: byte_start,
                    end: byte_end,
                    content_start,
                    content_len,
                    visual_len: content_len,
                });
                pos = end_pos;
                continue;
            }
        }

        // Try to parse highlight: ==text==
        if chars[pos] == '=' {
            if let Some((text, end_pos)) = parse_highlight(&chars, pos) {
                let byte_start = chars[..pos].iter().collect::<String>().len();
                let byte_end = chars[..end_pos].iter().collect::<String>().len();
                let content_start = byte_start + 2; // after '=='
                let content_len = text.len();
                tokens.push(MarkdownToken {
                    span: MarkdownSpan::Highlight(text),
                    start: byte_start,
                    end: byte_end,
                    content_start,
                    content_len,
                    visual_len: content_len,
                });
                pos = end_pos;
                continue;
            }
        }

        // Try to parse emphasis: * or _
        if chars[pos] == '*' || chars[pos] == '_' {
            if let Some((text, style, end_pos)) = parse_emphasis(&chars, pos) {
                let byte_start = chars[..pos].iter().collect::<String>().len();
                let byte_end = chars[..end_pos].iter().collect::<String>().len();
                let marker_len = style; // 1, 2, or 3
                let content_start = byte_start + marker_len;
                let content_len = text.len();
                tokens.push(MarkdownToken {
                    span: match style {
                        1 => MarkdownSpan::Italic(text),
                        2 => MarkdownSpan::Bold(text),
                        _ => MarkdownSpan::BoldItalic(text),
                    },
                    start: byte_start,
                    end: byte_end,
                    content_start,
                    content_len,
                    visual_len: content_len,
                });
                pos = end_pos;
                continue;
            }
        }

        // Plain text - break on any potential markdown character
        let byte_start = chars[..pos].iter().collect::<String>().len();
        let mut text = String::new();
        while pos < chars.len() {
            let c = chars[pos];
            // Break on any markdown syntax character that could start a valid pattern
            // For '!', only break if followed by '[' (potential image)
            if c == '[' || c == '`' || c == '*' || c == '_' || c == '~' || c == '=' {
                break;
            }
            // '!' only breaks if followed by '[' (start of image syntax)
            if c == '!' && chars.get(pos + 1) == Some(&'[') {
                break;
            }
            // '(' only breaks if followed by '(' (start of block reference)
            if c == '(' && chars.get(pos + 1) == Some(&'(') {
                break;
            }
            text.push(c);
            pos += 1;
        }
        if !text.is_empty() {
            let content_len = text.len();
            let byte_end = byte_start + content_len;
            tokens.push(MarkdownToken {
                span: MarkdownSpan::Text(text),
                start: byte_start,
                end: byte_end,
                content_start: byte_start,
                content_len,
                visual_len: content_len,
            });
        }
    }

    tokens
}

fn parse_link(chars: &[char], start: usize) -> Option<(String, String, usize)> {
    if chars.get(start)? != &'[' {
        return None;
    }

    let mut pos = start + 1;
    let mut text = String::new();

    while pos < chars.len() && chars[pos] != ']' {
        text.push(chars[pos]);
        pos += 1;
    }

    if pos >= chars.len() || chars[pos] != ']' {
        return None;
    }
    pos += 1;

    if pos >= chars.len() || chars[pos] != '(' {
        return None;
    }
    pos += 1;

    let mut url = String::new();
    while pos < chars.len() && chars[pos] != ')' {
        url.push(chars[pos]);
        pos += 1;
    }

    if pos >= chars.len() || chars[pos] != ')' {
        return None;
    }
    pos += 1;

    Some((text, url, pos))
}

fn parse_code(chars: &[char], start: usize) -> Option<(String, usize)> {
    if chars.get(start)? != &'`' {
        return None;
    }

    let mut pos = start + 1;
    let mut code = String::new();

    while pos < chars.len() && chars[pos] != '`' {
        code.push(chars[pos]);
        pos += 1;
    }

    if pos >= chars.len() || chars[pos] != '`' {
        return None;
    }
    pos += 1;

    Some((code, pos))
}

fn parse_emphasis(chars: &[char], start: usize) -> Option<(String, usize, usize)> {
    let marker = *chars.get(start)?;
    if marker != '*' && marker != '_' {
        return None;
    }

    let mut count = 0;
    let mut pos = start;
    while pos < chars.len() && chars[pos] == marker {
        count += 1;
        pos += 1;
    }

    if count > 3 {
        count = 3;
        pos = start + 3;
    }

    let mut text = String::new();

    while pos < chars.len() {
        if chars[pos] == marker {
            let mut end_count = 0;
            while pos < chars.len() && chars[pos] == marker {
                end_count += 1;
                pos += 1;
            }
            if end_count >= count {
                let text_len = text.chars().count();
                return Some((text, count, start + count + text_len + count));
            }
            for _ in 0..end_count {
                text.push(marker);
            }
        } else {
            text.push(chars[pos]);
            pos += 1;
        }
    }

    None
}

/// Parse image: ![alt](url)
fn parse_image(chars: &[char], start: usize) -> Option<(String, String, usize)> {
    // Must start with '!['
    if chars.get(start)? != &'!' || chars.get(start + 1)? != &'[' {
        return None;
    }

    let mut pos = start + 2; // after '!['
    let mut alt = String::new();

    // Parse alt text
    while pos < chars.len() && chars[pos] != ']' {
        alt.push(chars[pos]);
        pos += 1;
    }

    if pos >= chars.len() || chars[pos] != ']' {
        return None;
    }
    pos += 1;

    if pos >= chars.len() || chars[pos] != '(' {
        return None;
    }
    pos += 1;

    // Parse URL
    let mut url = String::new();
    while pos < chars.len() && chars[pos] != ')' {
        url.push(chars[pos]);
        pos += 1;
    }

    if pos >= chars.len() || chars[pos] != ')' {
        return None;
    }
    pos += 1;

    Some((alt, url, pos))
}

/// Parse strikethrough: ~~text~~
fn parse_strikethrough(chars: &[char], start: usize) -> Option<(String, usize)> {
    // Must start with '~~'
    if chars.get(start)? != &'~' || chars.get(start + 1)? != &'~' {
        return None;
    }

    let mut pos = start + 2; // after '~~'
    let mut text = String::new();

    // Parse content until we find '~~'
    while pos < chars.len() {
        if chars[pos] == '~' && chars.get(pos + 1) == Some(&'~') {
            // Found closing ~~
            let text_len = text.chars().count();
            return Some((text, start + 2 + text_len + 2));
        }
        text.push(chars[pos]);
        pos += 1;
    }

    None // No closing ~~
}

/// Parse highlight: ==text==
fn parse_highlight(chars: &[char], start: usize) -> Option<(String, usize)> {
    // Must start with '=='
    if chars.get(start)? != &'=' || chars.get(start + 1)? != &'=' {
        return None;
    }

    let mut pos = start + 2; // after '=='
    let mut text = String::new();

    // Parse content until we find '=='
    while pos < chars.len() {
        if chars[pos] == '=' && chars.get(pos + 1) == Some(&'=') {
            // Found closing ==
            let text_len = text.chars().count();
            return Some((text, start + 2 + text_len + 2));
        }
        text.push(chars[pos]);
        pos += 1;
    }

    None // No closing ==
}

/// Parse block reference: ((uuid))
/// Returns (uuid_string, end_position)
fn parse_block_ref(chars: &[char], start: usize) -> Option<(String, usize)> {
    // Must start with '(('
    if chars.get(start)? != &'(' || chars.get(start + 1)? != &'(' {
        return None;
    }

    let mut pos = start + 2; // after '(('
    let mut uuid = String::new();

    // Parse content until we find '))'
    while pos < chars.len() {
        if chars[pos] == ')' && chars.get(pos + 1) == Some(&')') {
            // Found closing ))
            if uuid.is_empty() {
                return None; // Empty block ref not valid
            }
            let uuid_len = uuid.chars().count();
            return Some((uuid, start + 2 + uuid_len + 2));
        }
        // Block refs should not contain newlines
        if chars[pos] == '\n' {
            return None;
        }
        uuid.push(chars[pos]);
        pos += 1;
    }

    None // No closing ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_plain_text() {
        let tokens = parse_inline_markdown("Hello world");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Text(s) if s == "Hello world"));
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 11);
    }

    #[test]
    fn test_parse_bold() {
        let tokens = parse_inline_markdown("**bold**");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Bold(s) if s == "bold"));
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 8);
        assert_eq!(tokens[0].content_start, 2);
        assert_eq!(tokens[0].content_len, 4);
    }

    #[test]
    fn test_parse_italic() {
        let tokens = parse_inline_markdown("*italic*");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Italic(s) if s == "italic"));
        assert_eq!(tokens[0].content_start, 1);
    }

    #[test]
    fn test_parse_code() {
        let tokens = parse_inline_markdown("`code`");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Code(s) if s == "code"));
    }

    #[test]
    fn test_parse_link() {
        let tokens = parse_inline_markdown("[link](https://example.com)");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Link { text, url }
            if text == "link" && url == "https://example.com"));
    }

    #[test]
    fn test_parse_mixed() {
        let tokens = parse_inline_markdown("Hello **world** test");
        assert_eq!(tokens.len(), 3);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Text(s) if s == "Hello "));
        assert!(matches!(&tokens[1].span, MarkdownSpan::Bold(s) if s == "world"));
        assert!(matches!(&tokens[2].span, MarkdownSpan::Text(s) if s == " test"));
    }

    #[test]
    fn test_token_contains_cursor() {
        let tokens = parse_inline_markdown("**bold**");
        let token = &tokens[0];

        assert!(token.contains_cursor(0)); // at start
        assert!(token.contains_cursor(4)); // middle
        assert!(token.contains_cursor(8)); // at end
        assert!(!token.contains_cursor(9)); // past end
    }

    #[test]
    fn test_unicode_positions() {
        let tokens = parse_inline_markdown("你好 **世界**");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 7); // "你好 " is 7 bytes
        assert_eq!(tokens[1].start, 7);
        assert_eq!(tokens[1].content_start, 9); // after **
    }

    #[test]
    fn test_parse_strikethrough() {
        let tokens = parse_inline_markdown("~~deleted~~");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Strikethrough(s) if s == "deleted"));
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 11);
        assert_eq!(tokens[0].content_start, 2); // after ~~
        assert_eq!(tokens[0].content_len, 7);
    }

    #[test]
    fn test_parse_highlight() {
        let tokens = parse_inline_markdown("==important==");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Highlight(s) if s == "important"));
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 13);
        assert_eq!(tokens[0].content_start, 2); // after ==
        assert_eq!(tokens[0].content_len, 9);
    }

    #[test]
    fn test_parse_image() {
        let tokens = parse_inline_markdown("![alt text](https://example.com/image.png)");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Image { alt, url }
            if alt == "alt text" && url == "https://example.com/image.png"));
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 42);
        assert_eq!(tokens[0].content_start, 2); // after ![
        assert_eq!(tokens[0].content_len, 8); // "alt text".len()
    }

    #[test]
    fn test_parse_mixed_new_formatting() {
        let tokens = parse_inline_markdown("Normal ~~strike~~ and ==highlight== text");
        assert_eq!(tokens.len(), 5);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Text(s) if s == "Normal "));
        assert!(matches!(&tokens[1].span, MarkdownSpan::Strikethrough(s) if s == "strike"));
        assert!(matches!(&tokens[2].span, MarkdownSpan::Text(s) if s == " and "));
        assert!(matches!(&tokens[3].span, MarkdownSpan::Highlight(s) if s == "highlight"));
        assert!(matches!(&tokens[4].span, MarkdownSpan::Text(s) if s == " text"));
    }

    #[test]
    fn test_unclosed_strikethrough() {
        // Unclosed ~~ should not be parsed as strikethrough
        let tokens = parse_inline_markdown("Hello ~~unclosed");
        // Should have "Hello " as text, then plain text (since ~~ isn't in break chars,
        // but we added it, so it breaks into pieces)
        assert!(tokens.len() >= 1);
        // The unclosed ~~ means no strikethrough token
        assert!(
            !tokens
                .iter()
                .any(|t| matches!(&t.span, MarkdownSpan::Strikethrough(_)))
        );
    }

    #[test]
    fn test_image_before_link() {
        // Image should be parsed, not link
        let tokens = parse_inline_markdown("![img](url) and [link](url2)");
        assert_eq!(tokens.len(), 3);
        assert!(matches!(&tokens[0].span, MarkdownSpan::Image { alt, url }
            if alt == "img" && url == "url"));
        assert!(matches!(&tokens[1].span, MarkdownSpan::Text(s) if s == " and "));
        assert!(matches!(&tokens[2].span, MarkdownSpan::Link { text, url }
            if text == "link" && url == "url2"));
    }
}
