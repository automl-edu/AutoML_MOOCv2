# 04_BO — Bayesian Optimization chapter

Slides and R figure scripts for the BO lecture.


# LaTeX conventions 
- Nearly all slides, if possible, should be "flat itemize" lists; maybe with pic(s) below or on the RHS
- always use \begin{columns}[onlytextwidth,T] because we have a speakermargin in the slide setup
- always use the \liturl macro to cite literature
- in the displayed citation text, only use the first author: "X et al. YYYY" (or "X YYYY" for single-author papers); never list two or three authors


## Language and Text Formatting

- Use American english
- Use the Oxford comma
- **Prefer precision over flourish**: concise, concrete, unambiguous language beats fancy prose.
- Write SHORT, PRECISE sentences in a terse style, remove filler words as much as possible; 
  if the sentence is ROUGHLY grammatically correct, slight "fragments" are fine;  
  especially do this if this saves a nearly empty line
- You can abbreviate words (mainly technical terms) if an intelligent person can guess the abbreviation.
- Do not capitalize normal nouns or method names. "Bayesian" is capitalized, "random forest" is not. 
- Capitalize at the beginning of a bullet point
- Slide titles are written in "title case"
- Use punctuation "sparingly"; not a "." after every text fragment in a bullet list or so
- Use cspell to check against typos, and add needed words to .cspell/project-words.txt if reasonable
- "e.g." and "i.e." are embedded in 2 commas


## Text Formatting
- Avoid font size changes unless really necessary.
- One sentence / thought per line (roughly), consider manual LBs after comma or semicolon; no "hanging" words
- Use "emphasized" markup in text sparingly, only for few words in slide and when really important;
  use `\textbf` then not `\emph`
- Quote text using backticks and apostrophes ``` ``text'' ``` syntax to produce "text", rather than `"text"`. Use of `\enquote{}` from `csquotes` is not required in an English locale.

## Math
- Use existing notation macros from ../macros.tex WHENEVER possible. If a macros is missing, chat about it.
- If an expression is used often but not in macros.tex yet, bring it up iun chat and offer to add
  then also add it to notation.tex

- Have latex formulas in src as readable as possible
- Use `$$ ... $$` to denote display math, not `\[`
- The `equation` environment is equivalent to `\[ ... \]`, which we do not use in favor of the simple `$$ ... $$`.
- Do not use `eqnarray` and remove it where you see it. It has been deprecated for years and `align` or `$$ ... $$` is usually preferred.
- Do not use English orthography (`.`, `,`) in math formulas
- Avoid "manual whitespace control" in formulas as much as possible, keep them simply;
  don't use "," often, maybe some well-placed q(q)uad sometimes
  
- Do not use `{ }` around single-character elements unless required; 
  to display $e^x$, use `e^x` rather than `e^{x}`
- Vector / Matrix transposition is denoted using `^T`
- For delimiters such as parentheses and vertical bars, the "simple" versions are preferred,
  so no `\left` and `\right` stuff, use this only when ABSOLUTELY necessary


# Code conventions and setup

- All R scripts and the `Makefile` assume **WD = `04_BO/`**. 

- Scripts source `rsrc/_setup.R` 

- We write figures to `images/` via `myggsave("name", plot, width, height)`.

- **Naming scheme for `rsrc/*.R` and `images/*`:** every R script's filename is prefixed with the chapter where the figure is used. E.g., a plot used in chapter 02 lives in `rsrc/02_foo.R` and produces `images/02_foo.<ext>`. Each script's image stem must equal the script's stem; postfixing for multi-output scripts is fine: `rsrc/02_foo.R` may write `02_foo_1.pdf`, `02_foo_2.pdf`, `02_foo_x.pdf`, etc. Hand-curated images that do not come from a script (e.g., title backgrounds, photos) keep their original names.

- Use the `rsrc/_setup.R`helpers if reasonable

- use mlr3mbo whenever possible

- each scripts starts like this 
```
# Used in: <filepath(s)>
#
# <short general docs what happens now>
```

- Each script is seeded

- `data.table` for tabular manipulation

- `ggplot2` + `patchwork` for plots

- Prefer `expression(lambda[1])` / `bquote(...)` for axis labels when the
  plotting function supports expressions (ggplot2 does). Only when the function
  does NOT accept expressions (e.g. base R `persp()`) fall back to literal
  Unicode characters (`"λ₁"`, `"λ₂"`) directly in the source -- do NOT use
  `\uXXXX` escape sequences, they are unreadable in the code.

