use std::ops::Range;

use gpui::SharedString;
use markdown::mdast::{self, Node};

use crate::{
    highlighter::HighlightTheme,
    text::{
        document::ParsedDocument,
        markdown_ext::MarkdownParseContext,
        node::{
            self, BlockNode, CodeBlock, ImageNode, InlineNode, LinkMark, MathNode, NodeContext,
            Paragraph, Span, Table, TableRow, TextMark,
        },
    },
};

/// Parse Markdown into a tree of nodes.
///
/// TODO: Remove `highlight_theme` option, this should in render stage.
pub(crate) fn parse(
    source: &str,
    cx: &mut NodeContext,
    highlight_theme: &HighlightTheme,
) -> Result<ParsedDocument, SharedString> {
    let options = cx.markdown_extensions.parse_options();
    markdown::to_mdast(&source, &options)
        .map(|n| ast_to_document(source, n, cx, highlight_theme))
        .map_err(|e| e.to_string().into())
}

fn parse_table_row(table: &mut Table, node: &mdast::TableRow, cx: &mut NodeContext) {
    let mut row = TableRow::default();
    node.children.iter().for_each(|c| {
        match c {
            Node::TableCell(cell) => {
                parse_table_cell(&mut row, cell, cx);
            }
            _ => {}
        };
    });
    table.children.push(row);
}

fn parse_table_cell(row: &mut node::TableRow, node: &mdast::TableCell, cx: &mut NodeContext) {
    let mut paragraph = Paragraph::default();
    parse_paragraph_children(&mut paragraph, &node.children, cx);
    let table_cell = node::TableCell {
        children: paragraph,
        ..Default::default()
    };
    row.children.push(table_cell);
}

fn inline_mark_tag(node: &mdast::Node) -> Option<bool> {
    let Node::Html(html) = node else {
        return None;
    };
    match html.value.trim().to_ascii_lowercase().as_str() {
        "<mark>" => Some(true),
        "</mark>" => Some(false),
        _ => None,
    }
}

fn parse_paragraph_children(
    paragraph: &mut Paragraph,
    children: &[mdast::Node],
    cx: &mut NodeContext,
) -> String {
    let mut text = String::new();
    let mut index = 0;

    while index < children.len() {
        if inline_mark_tag(&children[index]) == Some(true) {
            let mut depth = 1;
            let mut closing_index = index + 1;
            while closing_index < children.len() {
                match inline_mark_tag(&children[closing_index]) {
                    Some(true) => depth += 1,
                    Some(false) => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    None => {}
                }
                closing_index += 1;
            }

            if depth == 0 {
                text.push_str(&merge_children_with_mark(
                    paragraph,
                    &children[index + 1..closing_index],
                    TextMark::default().highlight(crate::yellow(200)),
                    cx,
                ));
                index = closing_index + 1;
                continue;
            }
        }

        text.push_str(&parse_paragraph(paragraph, &children[index], cx));
        index += 1;
    }

    text
}

/// Push a text run with its existing `marks` plus `new_mark` across the full
/// run.
///
/// If the last mark already covers the full run, merge into it. Otherwise add a
/// new full-run mark. Empty runs are skipped so callers can flush freely.
fn push_merged(
    paragraph: &mut Paragraph,
    text: String,
    marks: Vec<(Range<usize>, TextMark)>,
    new_mark: TextMark,
) {
    if text.is_empty() {
        return;
    }

    let mut node = InlineNode::new(text).marks(marks);
    let len = node.text.len();
    if let Some(last) = node.marks.last_mut()
        && last.0.start == 0
        && last.0.end == len
    {
        last.1.merge(new_mark);
    } else {
        node.marks.push((0..len, new_mark));
    }
    paragraph.push(node);
}

/// Parse `children` and apply `mark` across each emitted text run.
///
/// Nested child marks are kept and shifted to match the combined text for the
/// current run, which lets nested emphasis like `**_x_**` render as both bold
/// and italic. Inline images split the run and are emitted as sibling image
/// nodes. The return value is the plain text from all children, for callers that
/// need to pass text back to their parent node.
fn merge_children_with_mark(
    paragraph: &mut Paragraph,
    children: &[mdast::Node],
    mark: TextMark,
    cx: &mut NodeContext,
) -> String {
    let mut child_paragraph = Paragraph::default();
    let text = parse_paragraph_children(&mut child_paragraph, children, cx);
    let mut merged_text = String::new();
    let mut merged_marks = Vec::new();

    for mut node in child_paragraph.children {
        if let Some(mut image) = node.image.take() {
            if let Some(link_mark) = mark.link.clone() {
                image.link = Some(link_mark);
            }

            // GPUI InteractiveText does not support inline images, so
            // flush the accumulated text run and emit the image as its
            // own sibling InlineNode.
            push_merged(
                paragraph,
                std::mem::take(&mut merged_text),
                std::mem::take(&mut merged_marks),
                mark.clone(),
            );
            paragraph.push(InlineNode::image(image));
        } else if node.math.is_some() {
            // Math must remain a distinct inline child so a renderer can
            // replace it later without reparsing the Markdown.
            push_merged(
                paragraph,
                std::mem::take(&mut merged_text),
                std::mem::take(&mut merged_marks),
                mark.clone(),
            );
            let len = node.text.len();
            if let Some((range, child_mark)) = node.marks.last_mut()
                && range.start == 0
                && range.end == len
            {
                child_mark.merge(mark.clone());
            } else {
                node.marks.push((0..len, mark.clone()));
            }
            paragraph.push(node);
        } else {
            let merged_offset = merged_text.len();
            merged_text.push_str(&node.text);
            for (range, child_mark) in node.marks {
                merged_marks.push((
                    range.start + merged_offset..range.end + merged_offset,
                    child_mark,
                ));
            }
        }
    }

    push_merged(paragraph, merged_text, merged_marks, mark);
    text
}

fn append_inline_html_blocks(paragraph: &mut Paragraph, blocks: Vec<BlockNode>) -> Option<String> {
    let mut text = String::new();

    for block in blocks {
        match block {
            BlockNode::Root { children, .. } => {
                text.push_str(&append_inline_html_blocks(paragraph, children)?);
            }
            BlockNode::Paragraph(html_paragraph) => {
                text.push_str(&html_paragraph.text());
                for child in html_paragraph.children {
                    paragraph.push(child);
                }
            }
            BlockNode::Break { .. } => {
                text.push('\n');
                paragraph.push(InlineNode::new("\n"));
            }
            _ => return None,
        }
    }

    Some(text)
}

fn parse_paragraph(paragraph: &mut Paragraph, node: &mdast::Node, cx: &mut NodeContext) -> String {
    let span = node.position().map(|pos| Span {
        start: cx.offset + pos.start.offset,
        end: cx.offset + pos.end.offset,
    });
    if let Some(span) = span {
        paragraph.set_span(span);
    }

    let mut text = String::new();

    match node {
        Node::Paragraph(val) => {
            text = parse_paragraph_children(paragraph, &val.children, cx);
        }
        Node::Text(val) => {
            text = val.value.clone();
            paragraph.push_str(&val.value)
        }
        Node::Emphasis(val) => {
            text = merge_children_with_mark(
                paragraph,
                &val.children,
                TextMark::default().italic(),
                cx,
            );
        }
        Node::Strong(val) => {
            text =
                merge_children_with_mark(paragraph, &val.children, TextMark::default().bold(), cx);
        }
        Node::Delete(val) => {
            text = merge_children_with_mark(
                paragraph,
                &val.children,
                TextMark::default().strikethrough(),
                cx,
            );
        }
        Node::InlineCode(val) => {
            text = val.value.clone();
            paragraph.push(
                InlineNode::new(&text).marks(vec![(0..text.len(), TextMark::default().code())]),
            );
        }
        Node::Link(val) => {
            let link_mark = Some(LinkMark {
                url: val.url.clone().into(),
                title: val.title.clone().map(|s| s.into()),
                ..Default::default()
            });

            text = merge_children_with_mark(
                paragraph,
                &val.children,
                TextMark {
                    link: link_mark,
                    ..Default::default()
                },
                cx,
            );
        }
        Node::Image(raw) => {
            paragraph.push_image(ImageNode {
                url: raw.url.clone().into(),
                title: raw.title.clone().map(|t| t.into()),
                alt: Some(raw.alt.clone().into()),
                ..Default::default()
            });
        }
        Node::InlineMath(raw) => {
            text = raw.value.clone();
            paragraph.push(InlineNode::math(text.clone()));
        }
        Node::MdxTextExpression(raw) => {
            text = raw.value.clone();
            paragraph
                .push(InlineNode::new(&text).marks(vec![(0..text.len(), TextMark::default())]));
        }
        Node::Html(val) => match super::html::parse(&val.value, cx) {
            Ok(el) => {
                if let Some(inline_text) = append_inline_html_blocks(paragraph, el.blocks) {
                    text = inline_text;
                } else {
                    if cfg!(debug_assertions) {
                        tracing::warn!("unsupported inline html tag: {:#?}", val.value);
                    }
                }
            }
            Err(err) => {
                if cfg!(debug_assertions) {
                    tracing::warn!("failed parsing html: {:#?}", err);
                }

                text.push_str(&val.value);
            }
        },
        Node::FootnoteReference(foot) => {
            let prefix = format!("[{}]", foot.identifier);
            paragraph.push(InlineNode::new(&prefix).marks(vec![(
                0..prefix.len(),
                TextMark {
                    italic: true,
                    ..Default::default()
                },
            )]));
        }
        Node::LinkReference(link) => {
            let link_mark = LinkMark {
                url: "".into(),
                title: link.label.clone().map(Into::into),
                identifier: Some(link.identifier.clone().into()),
            };

            text = merge_children_with_mark(
                paragraph,
                &link.children,
                TextMark {
                    link: Some(link_mark),
                    ..Default::default()
                },
                cx,
            );
        }
        _ => {
            if cfg!(debug_assertions) {
                tracing::warn!("unsupported inline node: {:#?}", node);
            }
        }
    }

    text
}

fn ast_to_document(
    source: &str,
    root: mdast::Node,
    cx: &mut NodeContext,
    highlight_theme: &HighlightTheme,
) -> ParsedDocument {
    let root = match root {
        Node::Root(r) => r,
        _ => panic!("expected root node"),
    };

    let blocks = root
        .children
        .into_iter()
        .map(|c| ast_to_node(source, c, cx, highlight_theme))
        .collect();
    ParsedDocument {
        source: source.to_string().into(),
        blocks,
    }
}

fn new_span(pos: Option<markdown::unist::Position>, cx: &NodeContext) -> Option<Span> {
    let pos = pos?;

    Some(Span {
        start: cx.offset + pos.start.offset,
        end: cx.offset + pos.end.offset,
    })
}

fn ast_to_node(
    source: &str,
    value: mdast::Node,
    cx: &mut NodeContext,
    highlight_theme: &HighlightTheme,
) -> BlockNode {
    let span = new_span(value.position().cloned(), cx);
    let parse_cx = MarkdownParseContext::new(source, cx.offset);
    if let Some(mut node) = cx.markdown_extensions.parse_block(&value, &parse_cx) {
        node.set_span(span);
        return BlockNode::Custom(node);
    }

    match value {
        Node::Root(_) => unreachable!("node::Root should be handled separately"),
        Node::Paragraph(val) => {
            let mut paragraph = Paragraph::default();
            parse_paragraph_children(&mut paragraph, &val.children, cx);
            paragraph.span = new_span(val.position, cx);
            BlockNode::Paragraph(paragraph)
        }
        Node::Blockquote(val) => {
            let children = val
                .children
                .into_iter()
                .map(|c| ast_to_node(source, c, cx, highlight_theme))
                .collect();
            BlockNode::Blockquote {
                children,
                span: new_span(val.position, cx),
            }
        }
        Node::List(list) => {
            let children = list
                .children
                .into_iter()
                .map(|c| ast_to_node(source, c, cx, highlight_theme))
                .collect();
            BlockNode::List {
                ordered: list.ordered,
                children,
                span: new_span(list.position, cx),
            }
        }
        Node::ListItem(val) => {
            let children = val
                .children
                .into_iter()
                .map(|c| ast_to_node(source, c, cx, highlight_theme))
                .collect();
            BlockNode::ListItem {
                children,
                spread: val.spread,
                checked: val.checked,
                span: new_span(val.position, cx),
            }
        }
        Node::Break(val) => BlockNode::Break {
            html: false,
            span: new_span(val.position, cx),
        },
        Node::Code(raw) => BlockNode::CodeBlock(CodeBlock::new(
            raw.value.into(),
            raw.lang.map(|s| s.into()),
            highlight_theme,
            new_span(raw.position, cx),
        )),
        Node::Heading(val) => {
            let mut paragraph = Paragraph::default();
            parse_paragraph_children(&mut paragraph, &val.children, cx);

            BlockNode::Heading {
                level: val.depth,
                children: paragraph,
                span: new_span(val.position, cx),
            }
        }
        Node::Math(val) => BlockNode::Math(MathNode::new(
            val.value.into(),
            highlight_theme,
            new_span(val.position, cx),
        )),
        Node::Html(val) => match super::html::parse(&val.value, cx) {
            Ok(el) => BlockNode::Root {
                children: el.blocks,
                span: new_span(val.position, cx),
            },
            Err(err) => {
                if cfg!(debug_assertions) {
                    tracing::warn!("error parsing html: {:#?}", err);
                }

                BlockNode::Paragraph(Paragraph::new(val.value))
            }
        },
        Node::MdxFlowExpression(val) => BlockNode::CodeBlock(CodeBlock::new(
            val.value.into(),
            Some("mdx".into()),
            highlight_theme,
            new_span(val.position, cx),
        )),
        Node::Yaml(val) => BlockNode::CodeBlock(CodeBlock::new(
            val.value.into(),
            Some("yml".into()),
            highlight_theme,
            new_span(val.position, cx),
        )),
        Node::Toml(val) => BlockNode::CodeBlock(CodeBlock::new(
            val.value.into(),
            Some("toml".into()),
            highlight_theme,
            new_span(val.position, cx),
        )),
        Node::MdxJsxTextElement(val) => {
            let mut paragraph = Paragraph::default();
            parse_paragraph_children(&mut paragraph, &val.children, cx);
            paragraph.span = new_span(val.position, cx);
            BlockNode::Paragraph(paragraph)
        }
        Node::MdxJsxFlowElement(val) => {
            let mut paragraph = Paragraph::default();
            parse_paragraph_children(&mut paragraph, &val.children, cx);
            paragraph.span = new_span(val.position, cx);
            BlockNode::Paragraph(paragraph)
        }
        Node::ThematicBreak(val) => BlockNode::HorizontalRule {
            span: new_span(val.position, cx),
        },
        Node::Table(val) => {
            let mut table = Table::default();
            table.column_aligns = val
                .align
                .clone()
                .into_iter()
                .map(|align| align.into())
                .collect();
            val.children.iter().for_each(|c| {
                if let Node::TableRow(row) = c {
                    parse_table_row(&mut table, row, cx);
                }
            });
            table.span = new_span(val.position, cx);

            BlockNode::Table(table)
        }
        Node::FootnoteDefinition(def) => {
            let mut paragraph = Paragraph::default();
            let prefix = format!("[{}]: ", def.identifier);
            paragraph.push(InlineNode::new(&prefix).marks(vec![(
                0..prefix.len(),
                TextMark {
                    italic: true,
                    ..Default::default()
                },
            )]));

            parse_paragraph_children(&mut paragraph, &def.children, cx);
            paragraph.span = new_span(def.position, cx);
            BlockNode::Paragraph(paragraph)
        }
        Node::Definition(def) => {
            cx.add_ref(
                def.identifier.clone().into(),
                LinkMark {
                    url: def.url.clone().into(),
                    identifier: Some(def.identifier.clone().into()),
                    title: def.title.clone().map(Into::into),
                },
            );

            BlockNode::Definition {
                identifier: def.identifier.clone().into(),
                url: def.url.clone().into(),
                title: def.title.clone().map(|s| s.into()),
                span: new_span(def.position, cx),
            }
        }
        _ => {
            if cfg!(debug_assertions) {
                tracing::warn!("unsupported node: {:#?}", value);
            }
            BlockNode::Unknown
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use gpui::ParentElement;

    use crate::text::{MarkdownExtensions, MarkdownNode, MarkdownPlugin};

    #[test]
    fn test_nested_emphasis_merges_text_marks() {
        let mut cx = NodeContext::default();
        let document = parse(
            "This has **_bold and italic_** text.",
            &mut cx,
            &HighlightTheme::default_light(),
        )
        .unwrap();

        let BlockNode::Paragraph(paragraph) = &document.blocks[0] else {
            panic!("expected paragraph");
        };

        let bold_italic = paragraph
            .children
            .iter()
            .find(|child| child.text.as_ref() == "bold and italic")
            .expect("expected emphasized text");

        assert!(
            bold_italic
                .marks
                .iter()
                .any(|(_, mark)| mark.bold && mark.italic),
            "nested emphasis should produce a bold and italic mark"
        );
    }

    #[test]
    fn math_nodes_remain_distinct_without_changing_ordinary_markdown() {
        let mut cx = NodeContext::default();
        let document = parse(
            "Normal **bold** and $x + y$.\n\n$$\nz^2\n$$\n\nAfter.",
            &mut cx,
            &HighlightTheme::default_light(),
        )
        .unwrap();

        let BlockNode::Paragraph(paragraph) = &document.blocks[0] else {
            panic!("expected paragraph");
        };
        let inline_math = paragraph
            .children
            .iter()
            .find(|child| child.math.as_deref() == Some("x + y"))
            .expect("expected distinct inline math node");
        assert!(inline_math.marks.iter().any(|(_, mark)| mark.code));
        assert!(
            paragraph
                .children
                .iter()
                .any(|child| child.marks.iter().any(|(_, mark)| mark.bold))
        );

        let BlockNode::Math(display_math) = &document.blocks[1] else {
            panic!("expected distinct display math node");
        };
        assert_eq!(display_math.expression(), "z^2");
        assert!(matches!(document.blocks[2], BlockNode::Paragraph(_)));
        assert_eq!(
            document.to_markdown(),
            "Normal **bold** and $x + y$.\n\n$$\nz^2\n$$\n\nAfter."
        );
    }

    #[test]
    fn inline_html_mark_applies_to_its_text() {
        let mut cx = NodeContext::default();
        let document = parse(
            "Before <mark>@app/</mark> after.",
            &mut cx,
            &HighlightTheme::default_light(),
        )
        .unwrap();

        let BlockNode::Paragraph(paragraph) = &document.blocks[0] else {
            panic!("expected paragraph");
        };
        let marked = paragraph
            .children
            .iter()
            .find(|child| child.text.as_ref() == "@app/")
            .expect("expected marked text");
        assert!(
            marked
                .marks
                .iter()
                .any(|(_, mark)| mark.highlight.is_some()),
            "inline <mark> should highlight its contents"
        );
    }

    #[test]
    fn inline_html_mark_composes_with_markdown_formatting() {
        let mut cx = NodeContext::default();
        let document = parse(
            "**<mark>@app/</mark>** and _<mark>/help</mark>_",
            &mut cx,
            &HighlightTheme::default_light(),
        )
        .unwrap();

        let BlockNode::Paragraph(paragraph) = &document.blocks[0] else {
            panic!("expected paragraph");
        };
        let mention = paragraph
            .children
            .iter()
            .find(|child| child.text.as_ref() == "@app/")
            .expect("expected marked mention");
        assert!(
            mention
                .marks
                .iter()
                .any(|(_, mark)| mark.bold && mark.highlight.is_some())
        );
        let command = paragraph
            .children
            .iter()
            .find(|child| child.text.as_ref() == "/help")
            .expect("expected marked command");
        assert!(
            command
                .marks
                .iter()
                .any(|(_, mark)| mark.italic && mark.highlight.is_some())
        );
    }

    #[test]
    fn test_inline_html_image_stays_in_markdown_paragraph() {
        let mut cx = NodeContext::default();
        let document = parse(
            r#"Before <img src="https://example.com/avatar.png" alt="Avatar" width="32" height="32" /> after."#,
            &mut cx,
            &HighlightTheme::default_light(),
        )
        .unwrap();

        let BlockNode::Paragraph(paragraph) = &document.blocks[0] else {
            panic!("expected paragraph");
        };

        assert_eq!(paragraph.children.len(), 3);
        assert_eq!(paragraph.children[0].text.as_ref(), "Before ");
        assert_eq!(paragraph.children[2].text.as_ref(), " after.");

        let image = paragraph.children[1]
            .image
            .as_ref()
            .expect("expected inline html image");
        assert_eq!(image.url.as_ref(), "https://example.com/avatar.png");
        assert_eq!(image.width, Some(gpui::px(32.).into()));
        assert_eq!(image.height, Some(gpui::px(32.).into()));
    }

    #[test]
    fn test_inline_html_image_without_size_stays_in_markdown_paragraph() {
        let mut cx = NodeContext::default();
        let document = parse(
            r#"Before <img src="https://avatars.githubusercontent.com/u/5518"> after."#,
            &mut cx,
            &HighlightTheme::default_light(),
        )
        .unwrap();

        let BlockNode::Paragraph(paragraph) = &document.blocks[0] else {
            panic!("expected paragraph");
        };

        assert_eq!(paragraph.children.len(), 3);
        assert_eq!(paragraph.children[0].text.as_ref(), "Before ");
        assert_eq!(paragraph.children[2].text.as_ref(), " after.");

        let image = paragraph.children[1]
            .image
            .as_ref()
            .expect("expected inline html image");
        assert_eq!(
            image.url.as_ref(),
            "https://avatars.githubusercontent.com/u/5518"
        );
        assert_eq!(image.width, None);
        assert_eq!(image.height, None);
    }

    #[derive(Debug, Clone, PartialEq)]
    struct Ticker {
        symbol: String,
    }

    fn parse_ticker_block(node: &Node, cx: &MarkdownParseContext<'_>) -> Option<MarkdownNode> {
        let Node::Paragraph(paragraph) = node else {
            return None;
        };
        let [Node::Text(text)] = paragraph.children.as_slice() else {
            return None;
        };
        let symbol = text.value.strip_prefix('$')?.to_string();
        let node_text = format!("${symbol}");

        Some(
            MarkdownNode::new("ticker", Ticker { symbol })
                .text(node_text)
                .markdown(cx.node_source(node).unwrap_or_default()),
        )
    }

    #[test]
    fn custom_block_parser_converts_ticker_syntax_to_custom_node() {
        let extensions = MarkdownExtensions::default().block_parser(parse_ticker_block);

        let mut cx = NodeContext {
            markdown_extensions: extensions.into(),
            ..NodeContext::default()
        };
        let document = parse("$TSLA.US", &mut cx, &HighlightTheme::default_light()).unwrap();

        let BlockNode::Custom(node) = &document.blocks[0] else {
            panic!("expected custom markdown node");
        };
        assert_eq!(node.name(), "ticker");
        assert_eq!(node.as_text(), "$TSLA.US");
        assert_eq!(node.as_markdown(), "$TSLA.US");
        assert_eq!(
            node.data::<Ticker>(),
            Some(&Ticker {
                symbol: "TSLA.US".to_string()
            })
        );
        assert_eq!(document.text(), "$TSLA.US\n");
        assert_eq!(document.to_markdown(), "$TSLA.US");
    }

    struct TickerPlugin {
        name: &'static str,
    }

    impl TickerPlugin {
        fn new(name: &'static str) -> Self {
            Self { name }
        }
    }

    impl MarkdownPlugin for TickerPlugin {
        fn is_block(&self) -> bool {
            true
        }

        fn name(&self) -> &str {
            self.name
        }

        fn parse(&self, node: &Node, cx: &MarkdownParseContext<'_>) -> Option<MarkdownNode> {
            parse_ticker_block(node, cx)
        }

        fn render(
            &self,
            node: &MarkdownNode,
            _window: &mut gpui::Window,
            _cx: &mut gpui::App,
        ) -> impl gpui::IntoElement {
            gpui::div().child(node.as_text().to_string())
        }
    }

    #[test]
    fn custom_block_plugin_registers_parser_and_renderer() {
        let extensions = MarkdownExtensions::default().plugin(TickerPlugin::new("ticker"));

        let mut cx = NodeContext {
            markdown_extensions: extensions.into(),
            ..NodeContext::default()
        };
        let document = parse("$TSLA.US", &mut cx, &HighlightTheme::default_light()).unwrap();

        let BlockNode::Custom(node) = &document.blocks[0] else {
            panic!("expected custom markdown node");
        };
        assert_eq!(node.name(), "ticker");
        assert_eq!(
            node.data::<Ticker>(),
            Some(&Ticker {
                symbol: "TSLA.US".to_string()
            })
        );
    }
}
