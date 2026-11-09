#!/usr/bin/env python3
"""
Convert capital_gains_realization.md -> .tex -> .pdf via xelatex.

Targeted converter for this specific spec's markdown subset:
  headers, paragraphs, bullets, numbered lists, pipe tables, code blocks,
  inline code, bold, italics, hyphen rules, hyperlinks, and math (display
  $$...$$ and inline $...$, both passed through verbatim to LaTeX).
"""

import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
MD   = os.path.join(HERE, 'capital_gains_realization.md')
TEX  = os.path.join(HERE, 'capital_gains_realization.tex')
PDF  = os.path.join(HERE, 'capital_gains_realization.pdf')

# ---- math protection ---------------------------------------------------------

def protect_math(text):
    blocks = []
    def stash(m, display):
        blocks.append((display, m.group(1)))
        return f'@@MATH{len(blocks)-1}@@'
    # Backslash-escaped literal dollar signs (e.g., currency: "\$1") are
    # not math delimiters. Stash them so the math regex won't touch them.
    text = text.replace(r'\$', '@@LITDOLLAR@@')
    text = re.sub(r'\$\$([\s\S]+?)\$\$',  lambda m: stash(m, True),  text)
    # Inline math can wrap across lines in the source. Display math is
    # already extracted, so a non-greedy [^\$]+? is unambiguous.
    text = re.sub(r'\$([^\$]+?)\$',        lambda m: stash(m, False), text)
    # Restore literal dollars so latex_escape later turns them into \$.
    text = text.replace('@@LITDOLLAR@@', '$')
    return text, blocks

def restore_math(text, blocks):
    def repl(m):
        display, body = blocks[int(m.group(1))]
        return f'\\[{body}\\]' if display else f'${body}$'
    return re.sub(r'@@MATH(\d+)@@', repl, text)

# ---- LaTeX-special escaping (outside math) -----------------------------------

LATEX_ESCAPES = {
    '\\': r'\textbackslash{}',
    '{':  r'\{',
    '}':  r'\}',
    '&':  r'\&',
    '%':  r'\%',
    '#':  r'\#',
    '_':  r'\_',
    '$':  r'\$',
    '~':  r'\textasciitilde{}',
    '^':  r'\textasciicircum{}',
}
def latex_escape(s):
    out = []
    for ch in s:
        out.append(LATEX_ESCAPES.get(ch, ch))
    return ''.join(out)

# ---- inline formatting (bold, italic, code, links) ---------------------------

def render_inline(text):
    """Convert markdown inline formatting to LaTeX. Math placeholders pass
    through unchanged; everything else gets LaTeX-escaped except for the
    formatted tokens we recognize."""

    # Tokenize so we can escape literal text but not the markup itself.
    tokens = []  # list of ('text', str) | ('raw', str)
    pos = 0
    pat = re.compile(
        r'(`[^`\n]+`)'                  # 1: inline code
        r'|(\*\*[^\*\n]+?\*\*)'         # 2: bold
        r'|(\*[^\*\n]+?\*)'             # 3: italic
        r'|(\[[^\]\n]+?\]\([^)\n]+?\))' # 4: link
        r'|(@@MATH\d+@@)'               # 5: math placeholder (raw)
    )
    for m in pat.finditer(text):
        if m.start() > pos:
            tokens.append(('text', text[pos:m.start()]))
        chunk = m.group(0)
        if chunk.startswith('`'):
            tokens.append(('raw', r'\texttt{' + latex_escape(chunk[1:-1]) + '}'))
        elif chunk.startswith('**'):
            tokens.append(('raw', r'\textbf{' + render_inline(chunk[2:-2]) + '}'))
        elif chunk.startswith('*'):
            tokens.append(('raw', r'\emph{' + render_inline(chunk[1:-1]) + '}'))
        elif chunk.startswith('['):
            label, _, rest = chunk.partition('](')
            url = rest[:-1]
            tokens.append(('raw',
                r'\href{' + url.replace('%', r'\%').replace('#', r'\#') + '}{'
                + render_inline(label[1:]) + '}'))
        else:  # math placeholder
            tokens.append(('raw', chunk))
        pos = m.end()
    if pos < len(text):
        tokens.append(('text', text[pos:]))

    out = []
    for kind, s in tokens:
        out.append(latex_escape(s) if kind == 'text' else s)
    return ''.join(out)

# ---- block-level conversion --------------------------------------------------

def is_table_sep(line):
    return bool(re.match(r'^\s*\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)+\|?\s*$', line))

def split_table_row(line):
    line = line.strip()
    if line.startswith('|'): line = line[1:]
    if line.endswith('|'):   line = line[:-1]
    return [c.strip() for c in line.split('|')]

def convert(md):
    md, math_blocks = protect_math(md)
    lines = md.split('\n')
    out = []
    i = 0
    n = len(lines)

    list_stack = []  # list of ('itemize'|'enumerate', indent)

    def close_lists_to(target_depth):
        while len(list_stack) > target_depth:
            kind, _ = list_stack.pop()
            out.append(r'\end{' + kind + '}')

    while i < n:
        line = lines[i]

        # Code block
        if line.lstrip().startswith('```'):
            close_lists_to(0)
            out.append(r'\begin{verbatim}')
            i += 1
            while i < n and not lines[i].lstrip().startswith('```'):
                # NOTE: math placeholders inside code blocks would render wrong.
                # The spec doesn't put math in code; if it ever does, restore
                # those before this line.
                out.append(lines[i])
                i += 1
            out.append(r'\end{verbatim}')
            i += 1
            continue

        # Horizontal rule
        if re.match(r'^\s*---+\s*$', line):
            close_lists_to(0)
            out.append(r'\par\medskip\hrule\medskip')
            i += 1
            continue

        # Headers
        m = re.match(r'^(#{1,4})\s+(.*)$', line)
        if m:
            close_lists_to(0)
            depth = len(m.group(1))
            txt   = render_inline(m.group(2).strip())
            cmd   = {1: r'\section*', 2: r'\section*',
                     3: r'\subsection*', 4: r'\subsubsection*'}[depth]
            out.append(f'{cmd}{{{txt}}}')
            i += 1
            continue

        # Pipe table: header row, then separator, then body rows
        if (line.strip().startswith('|') and i + 1 < n
                and is_table_sep(lines[i + 1])):
            close_lists_to(0)
            header = split_table_row(line)
            i += 2  # skip header + separator
            body = []
            while i < n and lines[i].strip().startswith('|'):
                body.append(split_table_row(lines[i]))
                i += 1
            ncols = len(header)
            colspec = '|' + 'l|' * ncols
            out.append(r'\begin{center}')
            out.append(r'\begin{tabular}{' + colspec + '}')
            out.append(r'\hline')
            out.append(' & '.join(render_inline(c) for c in header) + r' \\')
            out.append(r'\hline')
            for row in body:
                # pad/truncate to ncols
                row = (row + [''] * ncols)[:ncols]
                out.append(' & '.join(render_inline(c) for c in row) + r' \\')
                out.append(r'\hline')
            out.append(r'\end{tabular}')
            out.append(r'\end{center}')
            continue

        # Bullet list item
        m = re.match(r'^(\s*)[\*\-]\s+(.*)$', line)
        if m:
            indent_spaces = len(m.group(1))
            depth = indent_spaces // 2 + 1
            content = m.group(2)
            # Manage list nesting: open/close to match depth+kind
            while list_stack and (list_stack[-1][1] >= depth):
                close_lists_to(len(list_stack) - 1)
            while len(list_stack) < depth:
                out.append(r'\begin{itemize}')
                list_stack.append(('itemize', len(list_stack) + 1))
            # If current top isn't itemize, close & reopen
            if list_stack[-1][0] != 'itemize':
                close_lists_to(len(list_stack) - 1)
                out.append(r'\begin{itemize}')
                list_stack.append(('itemize', depth))
            out.append(r'  \item ' + render_inline(content))
            i += 1
            # Capture continuation lines (indented under the bullet)
            while (i < n and lines[i].strip()
                   and not re.match(r'^\s*([\*\-]|\d+\.)\s', lines[i])
                   and lines[i].startswith(' ' * (indent_spaces + 2))):
                out.append('    ' + render_inline(lines[i].strip()))
                i += 1
            continue

        # Numbered list item
        m = re.match(r'^(\s*)(\d+)\.\s+(.*)$', line)
        if m:
            indent_spaces = len(m.group(1))
            depth = indent_spaces // 3 + 1
            content = m.group(3)
            while list_stack and (list_stack[-1][1] >= depth):
                close_lists_to(len(list_stack) - 1)
            while len(list_stack) < depth:
                out.append(r'\begin{enumerate}')
                list_stack.append(('enumerate', len(list_stack) + 1))
            if list_stack[-1][0] != 'enumerate':
                close_lists_to(len(list_stack) - 1)
                out.append(r'\begin{enumerate}')
                list_stack.append(('enumerate', depth))
            out.append(r'  \item ' + render_inline(content))
            i += 1
            while (i < n and lines[i].strip()
                   and not re.match(r'^\s*([\*\-]|\d+\.)\s', lines[i])
                   and lines[i].startswith(' ' * (indent_spaces + 3))):
                out.append('    ' + render_inline(lines[i].strip()))
                i += 1
            continue

        # Blank line ends any active list and emits paragraph break
        if line.strip() == '':
            close_lists_to(0)
            out.append('')
            i += 1
            continue

        # Plain paragraph line (could span multiple consecutive non-empty
        # lines that aren't list items, headers, or tables).
        close_lists_to(0)
        para = [line]
        i += 1
        while (i < n and lines[i].strip()
               and not re.match(r'^(#{1,4}\s|---+\s*$|\s*[\*\-]\s|\s*\d+\.\s|```)', lines[i])
               and not (lines[i].strip().startswith('|')
                        and i + 1 < n and is_table_sep(lines[i + 1]))):
            para.append(lines[i])
            i += 1
        out.append(render_inline(' '.join(p.strip() for p in para)))

    close_lists_to(0)

    body = '\n'.join(out)
    body = restore_math(body, math_blocks)
    return body

# ---- LaTeX preamble ----------------------------------------------------------

PREAMBLE = r"""\documentclass[11pt]{article}
\usepackage[margin=1in]{geometry}
\usepackage{amsmath,amssymb}
% Stick with the classical lmodern T1 fonts. To get Unicode glyphs that
% T1 doesn't have (em-dash, en-dash, Greek letters used in body text and
% pseudocode), map each one to its LaTeX equivalent via newunicodechar.
\usepackage[T1]{fontenc}
\usepackage{lmodern}
\usepackage{newunicodechar}
\newunicodechar{—}{\textemdash}
\newunicodechar{–}{\textendash}
\newunicodechar{−}{\ensuremath{-}}
\newunicodechar{·}{\ensuremath{\cdot}}
\newunicodechar{×}{\ensuremath{\times}}
\newunicodechar{Δ}{\ensuremath{\Delta}}
\newunicodechar{Σ}{\ensuremath{\Sigma}}
\newunicodechar{τ}{\ensuremath{\tau}}
\newunicodechar{λ}{\ensuremath{\lambda}}
\newunicodechar{β}{\ensuremath{\beta}}
\newunicodechar{ω}{\ensuremath{\omega}}
\newunicodechar{α}{\ensuremath{\alpha}}
\newunicodechar{η}{\ensuremath{\eta}}
\newunicodechar{δ}{\ensuremath{\delta}}
\newunicodechar{φ}{\ensuremath{\varphi}}
\newunicodechar{θ}{\ensuremath{\theta}}
\newunicodechar{ε}{\ensuremath{\varepsilon}}
\newunicodechar{ψ}{\ensuremath{\psi}}
\newunicodechar{≥}{\ensuremath{\geq}}
\newunicodechar{≤}{\ensuremath{\leq}}
\newunicodechar{≈}{\ensuremath{\approx}}
\newunicodechar{∞}{\ensuremath{\infty}}
\newunicodechar{∈}{\ensuremath{\in}}
\newunicodechar{→}{\ensuremath{\to}}
\newunicodechar{̄}{}  % combining overline; ignore in body text
\newunicodechar{§}{\S}
\usepackage{microtype}
\usepackage{hyperref}
\usepackage{xcolor}
\hypersetup{colorlinks=true, linkcolor=black, urlcolor=blue!50!black}
\usepackage{enumitem}
\setlist{itemsep=2pt, topsep=4pt}
\usepackage{array}
\usepackage{titlesec}
\titleformat{\section}{\large\bfseries}{\thesection}{0.6em}{}
\titlespacing*{\section}{0pt}{1.4em}{0.6em}
\titleformat{\subsection}{\normalsize\bfseries}{\thesubsection}{0.5em}{}
\titlespacing*{\subsection}{0pt}{1.0em}{0.4em}
\setlength{\parskip}{0.5em}
\setlength{\parindent}{0pt}
\sloppy
\begin{document}
"""

POSTAMBLE = r"""
\end{document}
"""

# ---- main --------------------------------------------------------------------

def main():
    with open(MD, 'r', encoding='utf-8') as f:
        md = f.read()

    body = convert(md)
    tex  = PREAMBLE + body + POSTAMBLE

    with open(TEX, 'w', encoding='utf-8') as f:
        f.write(tex)
    print(f'Wrote {TEX}')

    # Compile twice for hyperref refs to settle
    for _ in range(2):
        r = subprocess.run(
            ['xelatex', '-interaction=nonstopmode', '-halt-on-error',
             '-output-directory', HERE, TEX],
            cwd=HERE, capture_output=True, text=True
        )
        if r.returncode != 0:
            print('xelatex failed:')
            print(r.stdout[-3000:])
            print(r.stderr[-1500:])
            sys.exit(1)
    print(f'Wrote {PDF}')

if __name__ == '__main__':
    main()
