#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const { unified } = require("unified");
const remarkParseModule = require("remark-parse");
const remarkParse = remarkParseModule.default || remarkParseModule;

/**
 * Slugify a string for use in filenames
 */
function slugify(text) {
  return text
    .toLowerCase()
    .replace(/[^\w\s-]/g, "")
    .replace(/\s+/g, "-")
    .replace(/-+/g, "-")
    .trim();
}

/**
 * Generate filename from heading number and text
 */
function generateFilename(numbering, headingText, isHeader = false) {
  if (isHeader) {
    return `${numbering}-header.md`;
  }
  const slug = slugify(headingText);
  return `${numbering}-${slug}.md`;
}

/**
 * Extract text content from AST node (handles inline formatting like code, emphasis, etc.)
 */
function extractText(node) {
  if (node.type === "text") {
    return node.value;
  }
  if (node.children && Array.isArray(node.children)) {
    return node.children.map(extractText).join("");
  }
  return "";
}

/**
 * Find all heading positions in the markdown text
 * Uses remark to parse and identify headings, then matches them back to source positions
 */
function findHeadings(markdownText) {
  // Parse markdown using remark to get AST
  const processor = unified().use(remarkParse);
  const ast = processor.parse(markdownText);

  const headings = [];
  const lines = markdownText.split("\n");

  // Extract headings from AST
  function traverse(node) {
    if (node.type === "heading") {
      const headingText = extractText(node);
      headings.push({
        level: node.depth,
        text: headingText,
      });
    }

    if (node.children && Array.isArray(node.children)) {
      node.children.forEach(traverse);
    }
  }

  traverse(ast);

  // Now match AST headings back to source positions using regex
  // This ensures we get exact positions while using the library for parsing
  const result = [];
  let astIndex = 0;
  let currentPosition = 0;

  lines.forEach((line, lineIndex) => {
    const headingMatch = line.match(/^(#{1,6})\s+(.+)$/);
    if (headingMatch && astIndex < headings.length) {
      const level = headingMatch[1].length;
      const text = headingMatch[2].trim();

      // Match with AST heading (they should be in same order)
      if (headings[astIndex] && headings[astIndex].level === level) {
        result.push({
          level: level,
          text: headings[astIndex].text, // Use text from AST (handles formatting correctly)
          position: currentPosition,
          lineNumber: lineIndex + 1,
        });
        astIndex++;
      }
    }

    currentPosition += line.length + 1; // Include newline
  });

  return result;
}

/**
 * Split markdown file by heading hierarchy
 */
function splitMarkdown(inputFile, outputDir, splitDepth) {
  const markdownText = fs.readFileSync(inputFile, "utf8");
  const headings = findHeadings(markdownText);

  if (headings.length === 0) {
    console.error("No headings found in markdown file");
    process.exit(1);
  }

  // Create output directory if it doesn't exist
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  // Find content before first heading
  const firstHeading = headings[0];
  let headerContent = markdownText.slice(0, firstHeading.position);
  headerContent = headerContent.replace(/\s+$/, "");

  if (headerContent) {
    const headerFilename = generateFilename("0.0.0", "", true);
    fs.writeFileSync(
      path.join(outputDir, headerFilename),
      headerContent + "\n"
    );
    console.log(`Created: ${headerFilename}`);
  }

  // Process each heading at or above split depth
  const numbering = [];

  headings.forEach((heading, index) => {
    // Update numbering array based on heading level
    if (heading.level <= splitDepth) {
      // Reset numbering for this level and below
      numbering.length = heading.level;
      numbering[heading.level - 1] = (numbering[heading.level - 1] || 0) + 1;

      // Find content for this section
      const startPos = heading.position;
      let endPos = markdownText.length;

      // If this heading is above split depth, stop before the next heading that will be split
      // If this heading is at split depth, stop at the next heading at same or higher level
      if (heading.level < splitDepth) {
        // Parent heading: stop before the next heading that will be split into its own file
        // This is the first heading that is <= splitDepth, but we need to be careful:
        // - If it's at the same level or higher (<= heading.level), stop there
        // - If it's deeper (> heading.level) but <= splitDepth, it's a child that will be split,
        //   so we should stop before it (at its position)
        // - But we should prioritize stopping at same/higher level headings first
        for (let i = index + 1; i < headings.length; i++) {
          // First priority: stop at next heading at same/higher level that will be split
          if (
            headings[i].level <= heading.level &&
            headings[i].level <= splitDepth
          ) {
            endPos = headings[i].position;
            break;
          }
          // Second priority: stop at first deeper heading that will be split (child heading)
          // This handles the case where H1 has H2 children (H2 is deeper but will be split)
          if (
            headings[i].level > heading.level &&
            headings[i].level <= splitDepth
          ) {
            endPos = headings[i].position;
            break;
          }
        }
      } else {
        // Heading at split depth: stop at next heading at same or higher level
        for (let i = index + 1; i < headings.length; i++) {
          if (headings[i].level <= heading.level) {
            endPos = headings[i].position;
            break;
          }
        }
      }

      let sectionContent = markdownText.slice(startPos, endPos);

      // Trim trailing whitespace but preserve leading content
      sectionContent = sectionContent.replace(/\s+$/, "");

      if (sectionContent) {
        const numberingStr = numbering.join(".");
        const filename = generateFilename(numberingStr, heading.text);
        fs.writeFileSync(path.join(outputDir, filename), sectionContent + "\n");
        console.log(`Created: ${filename}`);
      }
    }
    // Headings deeper than split depth are included in their parent section
  });

  console.log(`\nSplit complete! Files created in ${outputDir}`);
}

/**
 * Merge markdown files back together
 */
function mergeMarkdown(inputDir, outputFile) {
  const files = fs
    .readdirSync(inputDir)
    .filter((f) => f.endsWith(".md"))
    .sort((a, b) => {
      // Custom sort: handle 0.0.0-header.md first, then numbered files
      if (a.startsWith("0.0.0")) return -1;
      if (b.startsWith("0.0.0")) return 1;

      // Extract numbers for comparison
      const numA = a.split("-")[0].split(".").map(Number);
      const numB = b.split("-")[0].split(".").map(Number);

      // Compare number arrays
      for (let i = 0; i < Math.max(numA.length, numB.length); i++) {
        const aVal = numA[i] || 0;
        const bVal = numB[i] || 0;
        if (aVal !== bVal) {
          return aVal - bVal;
        }
      }
      return 0;
    });

  let mergedContent = "";

  files.forEach((file, index) => {
    const filePath = path.join(inputDir, file);
    let content = fs.readFileSync(filePath, "utf8");

    // Remove the single trailing newline that was added during split
    content = content.replace(/\n$/, "");

    // Add content, with newline between files
    mergedContent += content;
    if (index < files.length - 1) {
      mergedContent += "\n\n";
    }
  });

  // Add final newline(s) to match original file ending (typically blank line)
  // Original ends with content + \n + blank line = \n\n
  if (mergedContent.endsWith("\n")) {
    mergedContent += "\n";
  } else {
    mergedContent += "\n\n";
  }

  fs.writeFileSync(outputFile, mergedContent);
  console.log(`Merged ${files.length} files into ${outputFile}`);
}

// CLI interface
const args = process.argv.slice(2);

if (args.length < 2) {
  console.error(`
Usage:
  node split-markdown.js split <input-file> <output-dir> [depth]
  node split-markdown.js merge <input-dir> <output-file>

Examples:
  node split-markdown.js split README.md docs/readme 2
  node split-markdown.js merge docs/readme README.md

Options:
  split: Split a markdown file by heading hierarchy
    - input-file: Path to the markdown file to split
    - output-dir: Directory where split files will be created
    - depth: Heading depth level to split at (default: 1, splits at H1)
  
  merge: Merge split markdown files back into one file
    - input-dir: Directory containing the split markdown files
    - output-file: Path to the output merged markdown file
`);
  process.exit(1);
}

const command = args[0];

if (command === "split") {
  const inputFile = args[1];
  const outputDir = args[2];
  const splitDepth = args[3] ? parseInt(args[3], 10) : 1;

  if (!fs.existsSync(inputFile)) {
    console.error(`Error: Input file not found: ${inputFile}`);
    process.exit(1);
  }

  if (isNaN(splitDepth) || splitDepth < 1) {
    console.error("Error: Depth must be a positive integer");
    process.exit(1);
  }

  splitMarkdown(inputFile, outputDir, splitDepth);
} else if (command === "merge") {
  const inputDir = args[1];
  const outputFile = args[2];

  if (!fs.existsSync(inputDir)) {
    console.error(`Error: Input directory not found: ${inputDir}`);
    process.exit(1);
  }

  mergeMarkdown(inputDir, outputFile);
} else {
  console.error(`Error: Unknown command: ${command}`);
  console.error('Use "split" or "merge"');
  process.exit(1);
}
